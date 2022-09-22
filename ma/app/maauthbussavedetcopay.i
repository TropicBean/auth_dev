/* maauthbussavedetcopay.i  MEDSTAR Medical Aid System
                            Validate Auth Provider Co-payment
                            (c) Copyright 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/

/*
  Determine if co-payments are activated on the Authorisation Type
  and only continue if no claims are paid for the Clinical Detail Line.
*/
ASSIGN cTrackingMessage = "CopayDetItem-START-ActivateCopayment?" + STRING(ttAuthTypeConfig.ActivateCopayment)
                        + ";btt_auth_detail.amount_paid="         + STRING(btt_auth_detail.amount_paid)
                        + ";btt_auth_detail.quantity_paid="       + STRING(btt_auth_detail.quantity_paid).

{ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

IF  ttAuthTypeConfig.ActivateCopayment = YES
AND btt_auth_detail.amount_paid        = 0
AND btt_auth_detail.quantity_paid      = 0
AND btt_auth_detail.auth_status        = 1 THEN
DO:
  CASE btt_auth_detail.owning_entity_mnemonic:
    WHEN "hlmac":U THEN ASSIGN cEntityDescr = "ATC Class":U.
    WHEN "hlmcr":U THEN ASSIGN cEntityDescr = "Basket":U.
    WHEN "hlmnl":U THEN ASSIGN cEntityDescr = "Nappi":U.
    WHEN "htmtl":U THEN ASSIGN cEntityDescr = "Tariff":U.
  END CASE.

  IF btt_auth_detail.related_entity_mnemonic <> ""
  THEN
    CASE btt_auth_detail.related_entity_mnemonic:
      WHEN "hlmac":U THEN ASSIGN cRelatedEntityDescr = "ATC Class":U.
      WHEN "hlmcr":U THEN ASSIGN cRelatedEntityDescr = "Basket":U.
      WHEN "hlmnl":U THEN ASSIGN cRelatedEntityDescr = "Nappi":U.
      WHEN "htmtl":U THEN ASSIGN cRelatedEntityDescr = "Tariff":U.
    END CASE.

  /*
    If the co-payment check must apply, we need to get the Emergency Flag details from the main provider.
    Emergency flag will be checked when it is determined if a co-payment must apply.
  */
  /* If detail line provider is the main provider. */
  IF AVAILABLE btt_auth_provider
  AND btt_auth_provider.main_provider = YES
  THEN
    ASSIGN cEmergencyFlag        = btt_auth_provider._emergency_flag
           lEmergencyFlagUpdated = btt_auth_provider._emergency_flag_updated.
  /* If detail line provider is not the main provider. */
  ELSE DO:
    FIND btt_main_provider NO-LOCK
      WHERE btt_main_provider.auth_obj      = btt_auth_detail.auth_obj
      AND   btt_main_provider.main_provider = YES
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = FALSE }

    IF AVAILABLE btt_main_provider
    THEN
      ASSIGN cEmergencyFlag        = btt_main_provider._emergency_flag
             lEmergencyFlagUpdated = btt_main_provider._emergency_flag_updated.
    ELSE DO:
      /*
        If main provider is not available return a Warn Acknowledgement that co-payments
        processing will be skipped because main provider is not available.
        Don't continue co-payment processing.
      */
      ASSIGN cWarning = "Co-payments processing skipped for " + cEntityDescr + " "
                      + btt_auth_detail.owning_alt_value + ", because Main Provider "
                      + "is not available on authoration number '" + btt_auth.auth_num + "'".
      goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,
                          INPUT btt_auth_detail.auth_detail_obj,
                          INPUT "":U,
                          INPUT "":U,
                          INPUT btt_auth_detail.line_number,
                          INPUT cWarning,
                          INPUT "WAR":U,
                          INPUT lAcknowledge).
    END.  // ELSE - IF AVAILABLE btt_main_provider
  END.  // ELSE DO - If detail line provider is not the main provider.

  IF NOT AVAILABLE buf_auth_detail
  AND btt_auth_detail.auth_detail_obj > 0 THEN
  DO:
    FIND buf_auth_detail NO-LOCK
      WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = FALSE }
  END.

  ASSIGN cTrackingMessage = "CopayDetItem-ACTIVATE COPAY WHEN:Auth_detail_obj(" + STRING(btt_auth_detail.auth_detail_obj)
                           + ")<=0=" + " OR EmergencyFlagUpdated(" + STRING(lEmergencyFlagUpdated) + ")=yes".
  IF AVAILABLE buf_auth_detail
  THEN
    ASSIGN cTrackingMessage = cTrackingMessage
                            + " OR StartDate("         + STRING(btt_auth_detail.start_date)    + ")<>" + STRING(buf_auth_detail.start_date)
                            + " OR PMB("               + STRING(btt_auth_detail.pmb_indicator) + ")<>" + STRING(buf_auth_detail.pmb_indicator)
                            + " OR CopayOverrideNote(" + btt_auth_detail.copay_override_note   + ")<>" + buf_auth_detail.copay_override_note
                            + " OR RelatedObj("        + STRING(btt_auth_detail.related_obj)   + ")<>" + STRING(buf_auth_detail.related_obj)
                            + " OR RelatedKey("        + btt_auth_detail.related_key           + ")<>" + buf_auth_detail.related_key.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    If main provider is available, the co-payment processing must only be activated when:
    - A new Clinical Detail Line is added.
    - A Clinical Detail Line is updated and one of the following values are updated:
      * Start Date.
      * PMB Indicator.
      * Emergency flag is updated
      * Co-payment override reason
      * When the related entity on a tariff detail line is updated.
  */
  IF btt_auth_detail.auth_detail_obj     <= 0
  OR btt_auth_detail.start_date          <> buf_auth_detail.start_date
  OR btt_auth_detail.pmb_indicator       <> buf_auth_detail.pmb_indicator
  OR lEmergencyFlagUpdated                = TRUE
  OR btt_auth_detail.copay_override_note <> buf_auth_detail.copay_override_note
  OR btt_auth_detail.related_obj         <> buf_auth_detail.related_obj
  OR btt_auth_detail.related_key         <> buf_auth_detail.related_key
  OR btt_auth_detail.auth_status         <> buf_auth_detail.auth_status THEN
  DO:
    /* Co-payment processing should only apply if the co-payment override reason is blank. */
    /* A co-payment override reason will indicate that a co-payment should not apply.      */
    IF btt_auth_detail.copay_override_note = "" THEN
    DO:

      IF btt_auth_detail.related_entity_mnemonic <> ""
      THEN
        ASSIGN cEntityMnemonic = btt_auth_detail.related_entity_mnemonic
               dEntityObj      = btt_auth_detail.related_obj
               cEntityKey      = btt_auth_detail.related_key
               cEntityValue    = btt_auth_detail.related_value.
      ELSE
        ASSIGN cEntityMnemonic = btt_auth_detail.owning_entity_mnemonic
               dEntityObj      = btt_auth_detail.owning_obj
               cEntityKey      = btt_auth_detail.owning_key
               cEntityValue    = btt_auth_detail.owning_alt_value.

      ASSIGN cTrackingMessage = "CopayDetItem-checkForCopayDetailItem ***INPUT*** "
                              + "InsurerObj="   + STRING(btt_auth.insurer_obj)
                              + ";OptionCode="  + STRING(btt_auth.option_code)
                              + ";StartDate="   + STRING(btt_auth_detail.start_date)
                              + ";Entity="      + cEntityMnemonic
                              + ";EntityObj="   + STRING(dEntityObj)
                              + ";EntityKey="   + cEntityKey
                              + ";EntityValue=" + cEntityValue
                              + ";PrType="      + STRING(btt_auth_provider.pr_type)
                              + ";Emergency="   + cEmergencyFlag
                              + ";PMB="         + STRING(btt_auth_detail.pmb_indicator).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      mipEnv:Health:AuthService:checkForCopayDetailItem (INPUT  btt_auth.insurer_obj,
                                                         INPUT  btt_auth.option_code,
                                                         INPUT  btt_auth_detail.start_date,
                                                         INPUT  cEntityMnemonic,
                                                         INPUT  dEntityObj,
                                                         INPUT  cEntityKey,
                                                         INPUT  cEntityValue,
                                                         INPUT  btt_auth_provider.pr_type,
                                                         INPUT  cEmergencyFlag,
                                                         INPUT  btt_auth_detail.pmb_indicator,
                                                         INPUT  YES,
                                                         OUTPUT cWarning,
                                                         OUTPUT cWarnType,
                                                         OUTPUT dAuthCopayTypeObj,
                                                         OUTPUT lCopayValueType,
                                                         OUTPUT dCopayValue,
                                                         OUTPUT cError).

      ASSIGN cTrackingMessage = "CopayDetItem-checkForCopayDetailItem ***OUTPUT*** "
                              + "cWarning="           + cWarning
                              + ";cWarnType="         + cWarnType
                              + ";dAuthCopayTypeObj=" + STRING(dAuthCopayTypeObj)
                              + ";lCopayValueType="   + IF lCopayValueType <> ? THEN STRING(lCopayValueType) ELSE "?"
                              + ";dCopayValue="       + STRING(dCopayValue)
                              + ";cError="            + cError.

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF cError <> "" THEN
      DO:
        goErrorObject:addError
                      (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                       INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                       INPUT "":U,                                                /* ipcOwningEntityKey      */
                       INPUT "owning_alt_value":U,                                /* ipcFieldName            */
                       INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                       INPUT cError,                                              /* ipcMessageText          */
                       INPUT "ERR":U).                                            /* ipcMessageType          */
      END.  // IF cError <> "" THEN
      ELSE DO:

        /* Populate a warning if a warning is returned and a co-payment is applied. */
        IF  dCopayValue <> 0
        AND cWarning    <> "" THEN
        DO:
          ASSIGN lAcknowledge = (cWarnType = "ma_acAuthCopayWarnMessageTypeWarnAck":U).

          goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,
                          INPUT btt_auth_detail.auth_detail_obj,
                          INPUT "":U,
                          INPUT "":U,
                          INPUT btt_auth_detail.line_number,
                          INPUT cWarning,
                          INPUT "WAR":U,
                          INPUT lAcknowledge).

        END.  /* THEN - IF dCopayValue <> 0. */

        /* We exclude the dAuthCopayTypeObj in the find because the value may differ when an update is done. */
        FIND FIRST btt_auth_copay
          WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
          AND   btt_auth_copay.owning_entity_mnemonic = "hatad":U
          AND   btt_auth_copay.owning_obj             = btt_auth_detail.auth_detail_obj
          AND   btt_auth_copay.owning_key             = ""
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = FALSE }

        ASSIGN cTrackingMessage = "CopayDetItem-FIND btt_auth_copay with:"
                                + "auth_obj="    + STRING(btt_auth_detail.auth_obj)
                                + ";owning_obj=" + STRING(btt_auth_detail.owning_obj)
                                + ";AVAILABLE btt_auth_copay?" + STRING(AVAILABLE btt_auth_copay).

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        /* Continue processing if a co-payment value is returned. */
        /* Create/update hat_auth_copay record and assign co-payment values on the Clinical Detail Line (btt_auth_detail). */
        IF dCopayValue <> 0 THEN
        DO:
          IF NOT AVAILABLE btt_auth_copay THEN
          DO:
            /*
              When the provider emergency flag is updated, it may result in multiple co-payment records
              being created across the multiple detail lines.
              We need to ensure that the btt_auth_copay.auth_copay_obj value is always unique, otherwise
              the co-payment records will not create and no changes will be applied (and there will be
              no messages displayed to the user or errors written to the logfile).
            */
            ASSIGN dAuthCopayObj = 0.

            TTOBJBLOCK:
            REPEAT:
              IF CAN-FIND(FIRST btt_auth_copay
                          WHERE btt_auth_copay.auth_copay_obj = dAuthCopayObj)
              THEN
                ASSIGN dAuthCopayObj = dAuthCopayObj - 1.
              ELSE
                LEAVE TTOBJBLOCK.
            END.  // TTOBJBLOCK: REPEAT:

            ASSIGN cTrackingMessage = "CopayDetItem-btt_auth_copay CREATED with:"
                                    + "auth_obj="          + STRING(btt_auth_detail.auth_obj)
                                    + ";auth_copay_obj="   + STRING(dAuthCopayObj)
                                    + ";owning_obj="       + STRING(btt_auth_detail.owning_obj)
                                    + ";owning_alt_value=" + STRING(btt_auth_detail.owning_alt_value).

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            CREATE btt_auth_copay.
            ASSIGN btt_auth_copay.auth_obj               = btt_auth_detail.auth_obj
                   btt_auth_copay.auth_copay_obj         = dAuthCopayObj
                   btt_auth_copay.owning_entity_mnemonic = "hatad":U
                   btt_auth_copay.owning_obj             = btt_auth_detail.auth_detail_obj
                   btt_auth_copay.owning_key             = ""
                   btt_auth_copay.owning_alt_value       = cEntityDescr + ":" + btt_auth_detail.owning_alt_value.
            IF btt_auth_detail.related_entity_mnemonic <> ""
            THEN
              ASSIGN btt_auth_copay.owning_alt_value = btt_auth_copay.owning_alt_value + " " + cRelatedEntityDescr + ":" + cEntityValue.

          END.  /* THEN - IF NOT AVAILABLE btt_auth_copay_obj. */

          ASSIGN btt_auth_copay.auth_copay_type_obj	= dAuthCopayTypeObj.

          IF lCopayValueType THEN
          DO:
            ASSIGN cTrackingMessage = "CopayDetItem-btt_auth_copay UPDATED:"
                                    + "amount="        + STRING(btt_auth_copay.amount)        + "->0"
                                    + ";amount_%="     + STRING(btt_auth_copay.amount_%)      + "->"  + STRING(dCopayValue)
                                    + ";copay_auth="   + STRING(btt_auth_detail.copay_auth)   + "->0"
                                    + ";copay_auth_%=" + STRING(btt_auth_detail.copay_auth_%) + "->"  + STRING(dCopayValue)
                   btt_auth_copay.amount        = 0
                   btt_auth_copay.amount_%      = dCopayValue
                   btt_auth_detail.copay_auth   = 0
                   btt_auth_detail.copay_auth_% = dCopayValue.
          END.  // IF lCopayValueType THEN
          ELSE DO:
            ASSIGN cTrackingMessage = "CopayDetItem-btt_auth_copay UPDATED:"
                                    + "amount="        + STRING(btt_auth_copay.amount)        + "->"  + STRING(dCopayValue)
                                    + ";amount_%="     + STRING(btt_auth_copay.amount_%)      + "->0"
                                    + ";copay_auth="   + STRING(btt_auth_detail.copay_auth)   + "->"  + STRING(dCopayValue)
                                    + ";copay_auth_%=" + STRING(btt_auth_detail.copay_auth_%) + "->0"
                   btt_auth_copay.amount        = dCopayValue
                   btt_auth_copay.amount_%      = 0
                   btt_auth_detail.copay_auth   = dCopayValue
                   btt_auth_detail.copay_auth_% = 0.
          END.  // ELSE - IF lCopayValueType THEN

          ASSIGN btt_auth_copay.record_action = "MODIFY":U.

          VALIDATE btt_auth_copay.

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END.  /* THEN - IF dCopayValue <> 0. */
      END.  // ELSE - IF cError <> "" THEN
    END.  // IF btt_auth_detail.copay_override_note = "" THEN

    ASSIGN cTrackingMessage = "CopayDetItem-btt_auth_copay OVERRIDE? cError=" + cError
                            + ";btt_auth_detail.copay_override_note=" + STRING(btt_auth_detail.copay_override_note)
                            + ";dCopayValue="  + STRING(dCopayValue).
    IF AVAILABLE buf_auth_detail
    THEN
      ASSIGN cTrackingMessage = cTrackingMessage
                              + ";copay_auth="   + STRING(buf_auth_detail.copay_auth)
                              + ";copay_auth_%=" + STRING(buf_auth_detail.copay_auth_%).
    ELSE
      ASSIGN cTrackingMessage = cTrackingMessage
                              + ";buf_auth_detail NOT AVAILABLE".

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /*
      If a co-payment does exist it must be deleted when:
      a) A co-payment override reason will indicate that a co-payment should not apply
      OR
      b) A co-payment was applied, on update of a value that may affect the co-payment the co-payment may no longer apply.
    */
    IF   cError = ""
    AND (btt_auth_detail.copay_override_note <> "" OR (dCopayValue = 0
                                                       AND  AVAILABLE buf_auth_detail
                                                       AND (buf_auth_detail.copay_auth <> 0 OR buf_auth_detail.copay_auth_% <> 0))) THEN
    DO:
      IF NOT AVAILABLE btt_auth_copay THEN
      DO:
        FIND FIRST btt_auth_copay
            WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
            AND   btt_auth_copay.owning_entity_mnemonic = "hatad":U
            AND   btt_auth_copay.owning_obj             = btt_auth_detail.auth_detail_obj
            AND   btt_auth_copay.owning_key             = ""
            NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = FALSE }
      END.  // IF NOT AVAILABLE btt_auth_copay THEN

      ASSIGN cTrackingMessage = "CopayDetItem-btt_auth_copay OVERRIDE-DELETE - AVAILABLE btt_auth_copay?" + STRING(AVAILABLE btt_auth_copay).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF AVAILABLE btt_auth_copay THEN
      DO:
        ASSIGN btt_auth_copay.record_action = "DELETE":U
               btt_auth_detail.copay_auth   = 0
               btt_auth_detail.copay_auth_% = 0.

        VALIDATE btt_auth_detail.
        VALIDATE btt_auth_copay.
      END.  // IF AVAILABLE btt_auth_copay THEN
    END.  // IF btt_auth_detail.copay_override_note = "" THEN
  END.  // IF btt_auth_detail.auth_detail_obj <= 0 OR...
END.  // IF ttAuthTypeConfig.ActivateCopayment = YES
