/* maauthservcopaydetailitem.i  MEDSTAR Medical Aid System
                                AuthService -> checkForCopayDetailItem
                                (c) Copyright 2020 - 2022
                                MIP Holdings (Pty) Ltd
                                All rights reserved
*/
/*------------------------------------------------------------------------------
  Purpose:     Determine if a co-payment must apply for:
               o	An Authorisation Clinical Detail Line when a detail line is
                  loaded on an Authorisation.
               o	A Claim that links to an Authorisation when no Clinical
                  Detail Line exists on the Authorisation.
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipdInsurerObj       AS DECIMAL                                NO-UNDO.
DEFINE INPUT  PARAMETER ipiOptioncode       AS INTEGER                                NO-UNDO.
DEFINE INPUT  PARAMETER ipdDate             AS DATE                                   NO-UNDO.
DEFINE INPUT  PARAMETER ipcEntity           AS CHARACTER                              NO-UNDO.
DEFINE INPUT  PARAMETER ipdEntityObj        AS DECIMAL                                NO-UNDO.
DEFINE INPUT  PARAMETER ipcEntityKey        AS CHARACTER                              NO-UNDO.
DEFINE INPUT  PARAMETER ipcEntityAltValue   AS CHARACTER                              NO-UNDO.
DEFINE INPUT  PARAMETER ipiPrType           AS INTEGER                                NO-UNDO.
DEFINE INPUT  PARAMETER ipcEmergency        AS CHARACTER                              NO-UNDO.
DEFINE INPUT  PARAMETER iplPMB              AS LOGICAL                                NO-UNDO.
DEFINE INPUT  PARAMETER iplAuthorisation    AS LOGICAL   FORMAT "Authorisation/Claim" NO-UNDO.
DEFINE OUTPUT PARAMETER opcWarning          AS CHARACTER                              NO-UNDO.
DEFINE OUTPUT PARAMETER opcWarnType         AS CHARACTER                              NO-UNDO.
DEFINE OUTPUT PARAMETER opdAuthCopayTypeObj AS DECIMAL                                NO-UNDO.
DEFINE OUTPUT PARAMETER oplCopayValueType   AS LOGICAL                                NO-UNDO.
DEFINE OUTPUT PARAMETER opdCopayValue       AS DECIMAL                                NO-UNDO.
DEFINE OUTPUT PARAMETER opcError            AS CHARACTER                              NO-UNDO.

{ma/inc/manappifieldvalues.i}

ASSIGN opcWarning          = "":U
       opcWarnType         = "":U
       opdAuthCopayTypeObj = 0
       oplCopayValueType   = ?
       opdCopayValue       = 0
       opcError            = "":U.

DEFINE VARIABLE cTrackingMessage            AS CHARACTER                              NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

/*
  Define new variables that will be used in repeat block
  to find the correct hac_auth_copay_detail_item.
*/
DEFINE VARIABLE dInsurerObj     LIKE hac_auth_copay_detail_item.insurer_obj NO-UNDO.
DEFINE VARIABLE iOptionCode     LIKE hac_auth_copay_detail_item.option_code NO-UNDO.
DEFINE VARIABLE iPrType         LIKE hac_auth_copay_detail_item.pr_type     NO-UNDO.
DEFINE VARIABLE cEntity         LIKE hat_auth_detail.owning_entity_mnemonic NO-UNDO.
DEFINE VARIABLE cEntityKey      LIKE hat_auth_detail.owning_key             NO-UNDO.
DEFINE VARIABLE cEntityAltValue LIKE hat_auth_detail.owning_key             NO-UNDO.
DEFINE VARIABLE dEntityObj      LIKE hat_auth_detail.owning_obj             NO-UNDO.
DEFINE VARIABLE lTariffDefault  AS LOGICAL FORMAT "yes/no"                  NO-UNDO.
DEFINE VARIABLE cValidEntities  AS CHARACTER                                NO-UNDO.

ASSIGN cTrackingMessage = "CopayDetItem - Input parameters:"
                        + " ipdInsurerObj="     + STRING(ipdInsurerObj)        + " ipiOptioncode=" + STRING(ipiOptioncode)
                        + " ipdDate="           + STRING(ipdDate,"9999/99/99") + " ipcEntity="     + STRING(ipcEntity)
                        + " ipdEntityObj="      + STRING(ipdEntityObj)         + " ipcEntityKey="  + STRING(ipcEntityKey)
                        + " ipcEntityAltValue=" + STRING(ipcEntityAltValue)    + " ipiPrType="     + STRING(ipiPrType)
                        + " ipcEmergency="      + STRING(ipcEmergency)         + " iplPMB="        + STRING(iplPMB)
                        + " iplAuthorisation="  + STRING(iplAuthorisation).

{ ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

/* Date should also have a value. */
IF ipdDate = ? THEN
DO:
  ASSIGN opcError = "Date is missing, value is required to find the correct Co-payment Detail Item setup.".
  RETURN.
END.  // IF ipdDate = ? THEN

/* Entiy should also have a value. */
IF ipcEntity = "" THEN
DO:
  ASSIGN opcError = "Entity is missing, value is required to find the correct Co-payment Detail Item setup.".
  RETURN.
END.  // IF ipcEntity = "" THEN

IF ipcEntityKey = ? THEN ASSIGN ipcEntityKey = "".

/*
  Only owning entities for which co-payments are activated should be allowed.
  Valid owning entities will be saved on mic_acronym where mic_acronym.category_key = 'ma_acAuthCopayDetailItemEntities'.
*/
IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
  WHERE mic_acronym.category_key  = "ma_acAuthCopayDetailItemEntities":U
  AND   mic_acronym.acronym_value = ipcEntity) THEN
DO:
  ASSIGN cValidEntities = "":U.
  FOR EACH mic_acronym NO-LOCK
    WHERE mic_acronym.category_key  = "ma_acAuthCopayDetailItemEntities":U:
    ASSIGN cValidEntities = cValidEntities + ", ":U + TRIM(mic_acronym.acronym_value).
  END.  // FOR EACH mic_acronym NO-LOCK

  ASSIGN cValidEntities   = TRIM(cValidEntities,",":U)
         opcError         = "Co-payment not allowed for entity '" + ipcEntity + "'.  Valid entities are '" + TRIM(cValidEntities)
                          + "'." //"[HELP=Acronym Key: Authorisation Co-payment Detail Item Entity Tables]"
         cTrackingMessage = "CopayDetItem - Acronym not found - " + opcError.

  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  RETURN.
END.  // IF NOT AVAILABLE mic_acronym THEN

/* Return an error if iplAuthorisation contains an unknown value. */
IF iplAuthorisation = ? THEN
DO:
  ASSIGN opcError = "Invalid input parameter indicating where checkForCopayDetailItem is called from.  Valid values = Authorisation or Claim.".
  RETURN.
END.  // IF iplAuthorisation = ? THEN

/*
  Entity Alt Value is required to find the correct hac_auth_copay_detail_item
  record if method is called from a claim.
  Return an error if iplAuthorisation = No (for claim) and ipcEntityAltValue = "".
*/
IF  iplAuthorisation  = NO
AND ipcEntityAltValue = "":U THEN
DO:
  ASSIGN opcError = "Entity Alt Value is required to find the correct Co-payment Detail Item setup for a Claim.".
  RETURN.
END.  // IF iplAuthorisation = NO AND ipcEntityAltValue = "":U THEN

/*
  Entity Obj or Entity Key is required to find the correct hac_auth_copay_detail_item
  record if method is called from an Authorisation.
  Return an error if iplAuthorisation = Yes and ipdEntityObj = 0 or ipcEntityKey = "".
*/
IF  iplAuthorisation = YES
AND ipdEntityObj = 0
AND ipcEntityKey = "" THEN
DO:
  ASSIGN opcError = "Entity Obj or Entity Key is required to find the correct Co-payment Detail Item setup for an Authorisation.".
  RETURN.
END.  // IF iplAuthorisation = YES AND (ipdEntityObj = 0 OR ipcEntityKey = "") THEN

/*
  Assign variables that will be used in COPAYDETAILITEMBLOCK.
*/
ASSIGN dInsurerObj     = ipdInsurerObj
       iOptionCode     = ipiOptionCode
       iPrType         = ipiPrType
       cEntity         = ipcEntity
       dEntityObj      = ipdEntityObj
       cEntityKey      = ipcEntityKey
       cEntityAltValue = ipcEntityAltValue
       lTariffDefault  = NO.

/*
  Determine if a co-payment value must apply by checking table hac_auth_copay_detail.
  Processing is done in a repeat block because we need to cater for scenarios where
  checks must be done again if no record is found.
*/
COPAYDETAILITEMBLOCK:
REPEAT:
  /*
    We will do the following finds in this COPAYDETAILITEMBLOCK:
    1.                            1st find will check for exact match on insurer_obj, option_code and pr_type.
    2. If nothing was found, then 2nd find will check for exact match on insurer_obj and option_code, but pr_type = 0.
    3. If nothing was found, then 3rd find will check for exact match on insurer_obj and pr_type, but option_code = 0.
    4. If nothing was found, then 4th find will check for exact match on insurer_obj, but option_code = 0 and pr_type = 0.
    5. If nothing was found, then 5th find will check for insurer_obj = 0, exact match on option_code and pr_type.
    6. If nothing was found, then 6th find will check for insurer_obj = 0, exact match on option code, but pr_type = 0.
    7. If nothing was found, then 7th find will check for insurer_obj = 0, option_code = 0, but pr_type = exact match.
    8. If nothing was found, then 8th find will check for insurer_obj = 0, option_code = 0 and pr_type = 0.
  */

  /* Find hac-auth_copay_detail_item, match insurer and option. */
  IF iplAuthorisation = YES THEN
  DO: /* When method is called from an Authorisation. */

    FIND FIRST hac_auth_copay_detail_item NO-LOCK
      WHERE hac_auth_copay_detail_item.insurer_obj            = dInsurerObj
      AND   hac_auth_copay_detail_item.option_code            = iOptionCode
      AND   hac_auth_copay_detail_item.owning_entity_mnemonic = cEntity
      AND   hac_auth_copay_detail_item.owning_obj             = dEntityObj
      AND   hac_auth_copay_detail_item.owning_key             = cEntityKey
      AND   hac_auth_copay_detail_item.pr_type                = iPrType
      AND   hac_auth_copay_detail_item.effective_date        <= ipdDate
      AND  (hac_auth_copay_detail_item.end_date               = ?
      OR    hac_auth_copay_detail_item.end_date              >= ipdDate)
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    ASSIGN cTrackingMessage = "CopayDetItem - FIND hac_auth_copay_detail_item FOR AUTHORISATION:"
                            + " dInsurerObj=" + STRING(dInsurerObj) + " iOptionCode=" + STRING(iOptionCode)
                            + " iPrType="     + STRING(iPrType)     + " cEntity="     + STRING(cEntity)
                            + " dEntityObj="  + STRING(dEntityObj)  + " cEntityKey="  + STRING(cEntityKey)
                            + " ipdDate="     + STRING(ipdDate,"9999/99/99")
                            + " AVAILABLE hac_auth_copay_detail_item=" + STRING(AVAILABLE hac_auth_copay_detail_item).
  END.  // IF iplAuthorisation = YES THEN
  ELSE DO: /* When method is called from a Claim. */
    FIND FIRST hac_auth_copay_detail_item NO-LOCK
      WHERE hac_auth_copay_detail_item.insurer_obj            = dInsurerObj
      AND   hac_auth_copay_detail_item.option_code            = iOptionCode
      AND   hac_auth_copay_detail_item.owning_entity_mnemonic = cEntity
      AND   hac_auth_copay_detail_item.owning_alt_value       = cEntityAltValue
      AND   hac_auth_copay_detail_item.pr_type                = iPrType
      AND   hac_auth_copay_detail_item.effective_date        <= ipdDate
      AND  (hac_auth_copay_detail_item.end_date               = ?
      OR    hac_auth_copay_detail_item.end_date              >= ipdDate)
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    ASSIGN cTrackingMessage = "CopayDetItem - FIND hac_auth_copay_detail_item FOR CLAIM:"
                            + " dInsurerObj=" + STRING(dInsurerObj)         + " iOptionCode=" + STRING(iOptionCode)
                            + " iPrType="     + STRING(iPrType)             + " cEntity="     + STRING(cEntity)
                            + " cEntityAltValue=" + STRING(cEntityAltValue) + " ipdDate="     + STRING(ipdDate,"9999/99/99")
                            + " AVAILABLE hac_auth_copay_detail_item=" + STRING(AVAILABLE hac_auth_copay_detail_item).

  END.  // ELSE -  IF iplAuthorisation = YES THEN

  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  IF NOT AVAILABLE hac_auth_copay_detail_item THEN
  DO:
    IF dInsurerObj <> 0 THEN
    DO:
      IF iOptionCode <> 0 THEN
      DO:
        IF iPrType <> 0
        THEN
          ASSIGN iPrType = 0.
        ELSE
          ASSIGN iOptionCode = 0
                 iPrType     = ipiPrType.
      END.  // IF iOptionCode <> 0 THEN
      ELSE DO:
        IF iPrType <> 0
        THEN
          ASSIGN iPrType = 0.
        ELSE
          ASSIGN iPrType     = ipiPrType
                 iOptionCode = ipiOptionCode
                 dInsurerObj = 0.
      END.  // ELSE - IF iOptionCode <> 0 THEN
      NEXT COPAYDETAILITEMBLOCK.
    END.  // IF dInsurerObj <> 0 THEN
    ELSE DO:
      IF iOptionCode <> 0 THEN
      DO:
        IF iPrType <> 0
        THEN
          ASSIGN iPrType = 0.
        ELSE
          ASSIGN iOptionCode = 0
                 iPrType     = ipiPrType.
        NEXT COPAYDETAILITEMBLOCK.
      END.  // IF iOptionCode <> 0 THEN
      ELSE DO:
        IF iPrType <> 0 THEN
        DO:
          ASSIGN iPrType = 0.
          NEXT COPAYDETAILITEMBLOCK.
        END.
        ELSE
          ASSIGN iPrType     = ipiPrType
                 iOptionCode = ipiOptionCode
                 dInsurerObj = ipdInsurerObj.
      END.  // ELSE - IF iOptionCode <> 0 THEN

      /*
        Do special finds in the same order:
          - If nappi is not set up and we need to find ATC Class
          - If tariff is not set up for specific base/ars/discipline and we need to find on default tariff
      */
      IF cEntity = "hlmnl":U THEN
      DO:
        /*
          Determine if a setup exists for the ATC Class if a detail line is for a Nappi.
          If the nappi contains an ATC Class Key another check must be done for the ATC Class.
        */
        FIND FIRST hlm_nappi NO-LOCK
          WHERE hlm_nappi.nappi_link_obj    = dEntityObj
          AND   hlm_nappi.nappi_status      = "A":U
          AND  (hlm_nappi.discontinue_date  = ?
          OR    hlm_nappi.discontinue_date >= ipdDate)
          AND   hlm_nappi.atc_class_key    <> ""
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        /*
          We've had issues where the hlm_nappi.nappi_link_obj = 0. This should not happen
          but incase it does, we don't want to return a "no copayment" if
          there should be one. So let's double check the hlm_nappi table
          using the nappi_code_prefix.
        */
        IF NOT AVAILABLE hlm_nappi THEN
        DO:
          /*
            Determine if cEntityAltValue is a prefix or full length nappi code
          */
          IF LENGTH(cEntityAltValue) = vi-min-nappi-prefix
          OR LENGTH(cEntityAltValue) = vi-max-nappi-prefix
          THEN
            FIND FIRST hlm_nappi NO-LOCK
              WHERE hlm_nappi.nappi_code_prefix = INTEGER(cEntityAltValue)
              AND   hlm_nappi.nappi_status      = "A":U
              AND  (hlm_nappi.discontinue_date  = ?
              OR    hlm_nappi.discontinue_date >= ipdDate)
              AND   hlm_nappi.atc_class_key    <> ""
              NO-ERROR.
          ELSE
            IF LENGTH(cEntityAltValue) = vi-min-nappi-length
            OR LENGTH(cEntityAltValue) = vi-max-nappi-length
            THEN
              FIND FIRST hlm_nappi NO-LOCK
                WHERE hlm_nappi.nappi_code        = cEntityAltValue
                AND   hlm_nappi.nappi_status      = "A":U
                AND  (hlm_nappi.discontinue_date  = ?
                OR    hlm_nappi.discontinue_date >= ipdDate)
                AND   hlm_nappi.atc_class_key    <> ""
                NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        END.  // IF NOT AVAILABLE hlm_nappi THEN

        ASSIGN cTrackingMessage = "CopayDetItem - Check if Nappi (" + cEntityAltValue + ") is linked to an ATC Class - "
                                + "hlm_nappi available?" + STRING(AVAILABLE hlm_nappi).
        IF AVAILABLE hlm_nappi
        THEN
          ASSIGN cTrackingMessage = cTrackingMessage + " ATC Class:" + hlm_nappi.atc_class_key.

        { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

        IF AVAILABLE hlm_nappi
        AND hlm_nappi.atc_class_key <> "":U
        THEN
          ASSIGN cEntity         = "hlmac":U
                 dEntityObj      = 0
                 cEntityKey      = TRIM(hlm_nappi.atc_class_key)
                 cEntityAltValue = TRIM(hlm_nappi.atc_class_key).
        ELSE
          LEAVE COPAYDETAILITEMBLOCK. // No co-payment must apply.

        ASSIGN cTrackingMessage = "CopayDetItem - FIND Nappi ATC Class ".

        { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

        NEXT COPAYDETAILITEMBLOCK.
      END.  // IF cEntity = "hlmnl":U THEN
      ELSE IF  ipcEntity      = "htmtl":U
           AND lTariffDefault = NO THEN
      DO:
        /*
          A clinical detail line will never link to a default tariff link record.
          If no setup exists for the tariff that links to the Base Rate, ARS Rate
          and Discipline for the provider, a check must be done for the default tariff setup.
        */
        FIND htm_tariff_link NO-LOCK
          WHERE htm_tariff_link.tariff_code         = cEntityAltValue
          AND   htm_tariff_link.tariff_link_default = YES
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF AVAILABLE htm_tariff_link THEN
        DO:
          ASSIGN dEntityObj      = htm_tariff_link.tariff_link_obj
                 cEntityKey      = ""
                 lTariffDefault  = YES
                 iPrType         = 0
                 iOptionCode     = 0
                 dInsurerObj     = 0.

          ASSIGN cTrackingMessage = "CopayDetItem - FIND Default Tariff".

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

          NEXT COPAYDETAILITEMBLOCK.
        END.  // IF AVAILABLE htm_tariff_link THEN
        ELSE
          LEAVE COPAYDETAILITEMBLOCK. // No co-payment must apply.
      END. /* Then - if ipcEntity = "htmtl". */
    END.  // ELSE - IF dInsurerObj <> 0 THEN
  END.  // IF NOT AVAILABLE hac_auth_copay_detail_item THEN
  ELSE DO:
    /*
      Determine if co-payment must apply for an Emergency and a PMB.
    */
    IF (hac_auth_copay_detail_item.apply_to_emergency = NO AND ipcEmergency = "YES")
    OR (hac_auth_copay_detail_item.apply_to_pmb       = NO AND iplPMB       = YES) THEN
    DO:
      ASSIGN cTrackingMessage = "CopayDetItem - Copayment not applicable to Emergency/PMB:"
                              + " hac_auth_copay_detail_item.apply_to_emergency="         + STRING(hac_auth_copay_detail_item.apply_to_emergency,"Yes/No")
                              + " ipcEmergency="                                          + STRING(ipcEmergency)
                              + " hac_auth_copay_detail_item.apply_to_pmb="               + STRING(hac_auth_copay_detail_item.apply_to_pmb,"Yes/No")
                              + " iplPMB="                                                + STRING(iplPMB)
                              + " hac_auth_copay_detail_item.auth_copay_detail_item_obj=" + STRING(hac_auth_copay_detail_item.auth_copay_detail_item_obj).

      { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

      LEAVE COPAYDETAILITEMBLOCK.  // No co-payment must apply.
    END.  // IF (hac_auth_copay_detail_item.apply_to_emergency = NO AND...

    ASSIGN opcWarning          = hac_auth_copay_detail_item.warning_message
           opcWarnType         = hac_auth_copay_detail_item.warning_message_type
           opdAuthCopayTypeObj = hac_auth_copay_detail_item.auth_copay_type_obj
           oplCopayValueType   = hac_auth_copay_detail_item.copayment_value_type
           opdCopayValue       = hac_auth_copay_detail_item.copayment_value.

    // LEAVE COPAYDETAILITEMBLOCK.

  END. /* ELSE - IF NOT AVAILABLE hac_auth_copay_detail_item. */

  LEAVE COPAYDETAILITEMBLOCK.

END.   /* REPEAT - COPAYDETAILITEMBLOCK. */

&ENDIF

{ mip/inc/mipcatcherror.i }
