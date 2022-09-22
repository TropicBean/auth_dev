/* maauthbussaveprovdetcreate.i  MEDSTAR Medical Aid System
                                 Create Clinical detail lines automatically as per Authorisation Type Detail Control setup
                                 (c) Copyright 2022 - 2022
                                 MIP Holdings (Pty) Ltd
                                 All rights reserved
*/
&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cAlertMessage          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cArsRate               AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBaseRate              AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cError                 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProviderTypeIndicator AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cWarning               AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTrackingMessage       AS CHARACTER NO-UNDO.

  DEFINE VARIABLE dTariffLinkObj         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dOwningObj             AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTariffObj             AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTrfCostObj            AS DECIMAL   NO-UNDO.

  DEFINE VARIABLE iCalcTime              AS INTEGER   NO-UNDO INITIAL 0.
  DEFINE VARIABLE iRow                   AS INTEGER   NO-UNDO INITIAL 0.

  DEFINE BUFFER   buf_auth_type_provider FOR hac_auth_type_provider.
  DEFINE BUFFER   buf_auth_type_detail   FOR hac_auth_type_detail.
  DEFINE BUFFER   buf_crosswalk          FOR hlm_crosswalk.

  ASSIGN cTrackingMessage = "Checking for Auth Auto Create setup on Auth Type Detail".

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  IF AVAILABLE btt_auth_provider THEN
  DO:
    /* Determine if any Auth Type Provider (hac_auth_type_provider) default setup exists for the provider type */
    ASSIGN cProviderTypeIndicator = "ma_acAuthProviderTypeIndicatorDef":U.

    { ma/msc/maauthtypeproviderread.i
               &hac_auth_type_provider  = buf_auth_type_provider
               &AuthTypeObj             = goAuthorisation:AuthTypeObj
               &InsurerObj              = goAuthorisation:InsurerObj
               &OptionCode              = goAuthorisation:OptionCode
               &ProviderType            = btt_auth_provider.provider_type
               &AuthGroupObj            = btt_auth_provider.auth_group_obj
               &ProviderTypeIndicator   = cProviderTypeIndicator
               &EffectiveDate           = btt_auth_provider.start_date
               &Lock                    = NO-LOCK }

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    ASSIGN cTrackingMessage = "AuthSaveAutoCreate - "
                            + " AVAILABLE hac_auth_type_provider?" + STRING(AVAILABLE buf_auth_type_provider).
    IF AVAILABLE buf_auth_type_provider
    THEN ASSIGN cTrackingMessage = cTrackingMessage
                            + " CAN-FIND(FIRST hac_auth_type_detail WHERE hac_auth_type_detail.auth_type_obj = "
                            + STRING(goAuthorisation:AuthTypeObj)
                            + " AND hac_auth_type_detail.auth_type_provider_obj = " + STRING(buf_auth_type_provider.auth_type_provider_obj)
                            + " AND hac_auth_type_detail.detail_type_indicator = "  + "'ma_acAuthDetailTypeIndicatorDef'":U
                            + " AND hac_auth_type_detail.effective_date <= "        + STRING(btt_auth_provider.start_date,'9999/99/99')
                            + " AND (hac_auth_type_detail.end_date = ?"
                            + " OR hac_auth_type_detail.end_date >= " + STRING(btt_auth_provider.start_date,'9999/99/99')
                            + ") AND hac_auth_type_detail.auth_auto_create = YES)".

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /* Check if any Auth Type Detail setup exists for Automatic Creation of clinical detail lines */
    IF AVAILABLE buf_auth_type_provider
    AND CAN-FIND(FIRST  buf_auth_type_detail
                 WHERE  buf_auth_type_detail.auth_type_obj           = goAuthorisation:AuthTypeObj
                   AND  buf_auth_type_detail.auth_type_provider_obj  = buf_auth_type_provider.auth_type_provider_obj
                   AND  buf_auth_type_detail.detail_type_indicator   = "ma_acAuthDetailTypeIndicatorDef":U
                   AND  buf_auth_type_detail.effective_date         <= btt_auth_provider.start_date
                   AND (buf_auth_type_detail.end_date                = ?
                    OR  buf_auth_type_detail.end_date               >= btt_auth_provider.start_date)
                   AND  buf_auth_type_detail.auth_auto_create        = YES) THEN
    DO:
      /* Read through the Auth Type Detail setup where Auth Auto Create is Yes */
      ASSIGN iCalcTime        = MTIME
             cTrackingMessage = "AuthSaveAutoCreate - Auth Auto Create detail setup has been found. Continue with automatic creation of detail lines.".

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      CREATEDETAILBLK:
      FOR EACH buf_auth_type_detail NO-LOCK
        WHERE  buf_auth_type_detail.auth_type_obj          = goAuthorisation:AuthTypeObj
          AND  buf_auth_type_detail.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
          AND  buf_auth_type_detail.detail_type_indicator  = "ma_acAuthDetailTypeIndicatorDef":U
          AND  buf_auth_type_detail.effective_date        <= btt_auth_provider.start_date
          AND (buf_auth_type_detail.end_date               = ?
           OR  buf_auth_type_detail.end_date		          >= btt_auth_provider.start_date)
          AND  buf_auth_type_detail.auth_auto_create       = YES :

        /* Only create details if owning entity is a Tariff, Basket or Nappi. */

        IF LOOKUP(buf_auth_type_detail.owning_entity,"htmtl,hlmnl,hlmcr":U) = 0
        OR  (     buf_auth_type_detail.pr_type  > 0
              AND buf_auth_type_detail.pr_type <> btt_auth_provider.pr_type)
        THEN
          NEXT CREATEDETAILBLK.

        /* Reset dOwningObj value as this is an input-output field for getValidTariff */
        ASSIGN dOwningObj = 0.

        /* Only create tariffs where the tariff is valid for the provider base rate, ars rate and discipline. */
        IF buf_auth_type_detail.owning_entity_mnemonic = "htmtl":U THEN
        DO: 
          /* Define variables cBaseRate and cArsRate for provider base rate and ars rate. */
          ASSIGN cBaseRate = IF btt_auth_provider.override_base_rate <> ""
                             THEN btt_auth_provider.override_base_rate
                             ELSE btt_auth_provider.default_base_rate
                 cArsRate  = IF btt_auth_provider.override_base_rate <> ""
                             THEN btt_auth_provider.override_ars_rate
                             ELSE btt_auth_provider.default_ars_rate.

           IF cBaseRate = "" THEN
           DO:
             mipEnv:Health:maDoctor:getProviderBaseRates( INPUT  btt_auth_provider.doc_num,
                                                          INPUT  goAuthorisation:MemNum,
                                                          INPUT  goAuthorisation:MemberOptionCode,
                                                          INPUT  btt_auth_provider.start_date,
                                                          OUTPUT cBaseRate,
                                                          OUTPUT cArsRate).
           END.  /* IF cBaseRate = "" THEN */

          /* Ensure that only valid tariff codes are created */
          ASSIGN lSuccess = mipEnv:Health:maMedical:getValidTariff
                                    (INPUT-OUTPUT dOwningObj,                             /*  iopdTariffLinkObj - zero to find default Trf link */
                                     INPUT        buf_auth_type_detail.owning_alt_value,  /*  ipcTariffCode     */
                                     INPUT        cBaseRate,                              /*  ipcBaseRate       */
                                     INPUT        cArsRate,                               /*  ipcARSRate        */
                                     INPUT        btt_auth_provider.pr_type,              /*  ipiPrType         */
                                     INPUT        btt_auth_provider.sub_pr_type,          /*  ipiSubPrType      */
                                     INPUT        btt_auth_provider.start_date,           /*  ipdDate           */
                                     INPUT        goAuthorisation:OptionCode,             /*  ipiOptionCode     */
                                     INPUT        "":U,                                   /*  ipcAddValidations */
                                     OUTPUT       dTariffObj,                             /*  opdTariffObj      */
                                     OUTPUT       dTrfCostObj,                            /*  opdTrfCostObj     */
                                     OUTPUT       cError,                                 /*  opcError          */
                                     OUTPUT       cWarning,                               /*  opcWarning        */
                                     OUTPUT       cAlertMessage).                         /*  opcAlertMessage   */

          IF cError <> "" THEN
          DO:
            ASSIGN cWarning = "Tariff Code: " + buf_auth_type_detail.owning_alt_value
                            + " is not valid for provider " + STRING(btt_auth_provider.doc_num)
                            + ". Authorisation Type Detail setup will be ignored".

            goErrorObject:addError
                              (INPUT 'hatad:' + buf_auth_type_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */
                               INPUT btt_auth_provider.auth_provider_obj,                    /* ipdOwningEntityObj      */
                               INPUT "":U,                                                   /* ipcOwningEntityKey      */
                               INPUT btt_auth_provider.line_number,                          /* ipiLineNumber           */
                               INPUT cWarning,                                               /* ipcMessageText          */
                               INPUT "WAR":U).                                               /* ipcMessageType          */

            NEXT CREATEDETAILBLK.
          END.  /* IF cError <> "" THEN */
        END. /* IF buf_auth_type_detail.owning_entity_mnemonic = "htmtl":U THEN */

        IF buf_auth_type_detail.owning_entity_mnemonic = "hlmnl":U
        THEN
          ASSIGN lSuccess = mipEnv:Health:maMedical:getValidNappi(INPUT-OUTPUT dOwningObj,                                 /*  iodNappiLinkObj  */
                                                                        INPUT  buf_auth_type_detail.owning_alt_value,      /*  ipcNappiCode     */
                                                                        INPUT  btt_auth_provider.start_date,               /*  ipdDate          */
                                                                        OUTPUT cError).                                    /*  opcError         */

        IF buf_auth_type_detail.owning_entity_mnemonic = "hlmcr":U THEN
        DO: 
          FIND FIRST buf_crosswalk NO-LOCK
               WHERE buf_crosswalk.crosswalk_code = buf_auth_type_detail.owning_alt_value
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

          IF AVAILABLE buf_crosswalk
          THEN
            ASSIGN dOwningObj = buf_crosswalk.crosswalk_obj .

        END. //IF buf_auth_type_detail.owning_entity_mnemonic = "hlmcr":U THEN

        FIND LAST btt_auth_detail
          WHERE btt_auth_detail.auth_obj = btt_auth_provider.auth_obj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF AVAILABLE btt_auth_detail
        THEN
          ASSIGN iRow = btt_auth_detail.line_number + 1.
        ELSE DO:
          IF iRow = 0
          THEN ASSIGN iRow = 1.
          ELSE ASSIGN iRow = iRow + 1.
        END.  /* ELSE - IF AVAILABLE btt_auth_detail */

      FIND FIRST tt_auth_detail NO-LOCK
           WHERE tt_auth_detail.auth_obj                = btt_auth_provider.auth_obj
             AND tt_auth_detail.auth_provider_obj       = btt_auth_provider.auth_provider_obj
             AND tt_auth_detail.owning_entity_mnemonic  = buf_auth_type_detail.owning_entity_mnemonic
             AND tt_auth_detail.owning_obj              = dOwningObj
             AND tt_auth_detail.owning_key              = buf_auth_type_detail.owning_key
             AND tt_auth_detail.related_entity_mnemonic = "":U
             AND tt_auth_detail.related_obj             = 0
             AND tt_auth_detail.related_key             = "":U
             AND tt_auth_detail.start_date              = btt_auth_provider.start_date
             AND tt_auth_detail.start_ampm              = btt_auth_provider.start_ampm
             AND tt_auth_detail.end_date                = btt_auth_provider.end_date
             AND tt_auth_detail.end_ampm                = btt_auth_provider.end_ampm
            NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE tt_auth_detail  THEN
        DO: 

          /* Create btt_auth_detail records for the Provider */
          CREATE tt_auth_detail.
          ASSIGN tt_auth_detail.line_number              = iRow
                 tt_auth_detail.record_action            = "MODIFY":U
                 tt_auth_detail.auth_detail_obj          = iRow * -1
                 tt_auth_detail.auth_obj                 = btt_auth_provider.auth_obj
                 tt_auth_detail.auth_provider_obj        = btt_auth_provider.auth_provider_obj
                 tt_auth_detail.start_date               = btt_auth_provider.start_date
                 tt_auth_detail.start_ampm               = btt_auth_provider.start_ampm
                 tt_auth_detail.end_date                 = btt_auth_provider.end_date
                 tt_auth_detail.end_ampm                 = btt_auth_provider.end_ampm
                 tt_auth_detail.auth_group_obj           = btt_auth_provider.auth_group_obj
                 tt_auth_detail.owning_entity_mnemonic   = buf_auth_type_detail.owning_entity_mnemonic
                 tt_auth_detail.owning_obj               = dOwningObj
                 tt_auth_detail.owning_key               = buf_auth_type_detail.owning_key
                 tt_auth_detail.owning_alt_value         = buf_auth_type_detail.owning_alt_value
                 tt_auth_detail.quantity_auth            = buf_auth_type_detail.quantity_auth
                 tt_auth_detail.line_restriction         = buf_auth_type_detail.default_line_restriction.

          VALIDATE tt_auth_detail.
        END.  // IF NOT AVAILABLE tt_auth_detail  THEN

      END. /* FOR EACH buf_auth_type_detail NO-LOCK */

      ASSIGN cTrackingMessage = "AuthSaveAutoCreate - Creation of Detail lines from Auth type detail setup completed.  (Duration="
                              + STRING(MTIME - iCalcTime) + " milliseconds).".

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    END. /* IF AVAILABLE buf_auth_type_provider AND CAN-FIND(FIRST buf_auth_type_detail... */
  END. //IF AVAILABLE btt_auth_provider
  ELSE DO: 
    ASSIGN cTrackingMessage = "AuthSaveAutoCreate - No Auth type detail setup found for Automatic creation of Detail lines. No validations necessary.".

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
  END.  /* ELSE - IF AVAILABLE btt_auth_provider */

&ENDIF


