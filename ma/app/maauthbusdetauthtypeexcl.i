/* maauthbusdetauthtypeexcl.i    MEDSTAR Medical Aid System
                                 Processing for the Exclusions for the Create Clinical detail lines as per Authorisation Type Detail Control setup
                                 (c) Copyright 2022 - 2022
                                 MIP Holdings (Pty) Ltd
                                 All rights reserved
*/
&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cProviderTypeIndicator AS CHARACTER NO-UNDO.

  DEFINE BUFFER buf_auth_type_provider   FOR hac_auth_type_provider.
  DEFINE BUFFER buf_auth_type_detail     FOR hac_auth_type_detail.
  DEFINE BUFFER buf_tariff_type          FOR htm_tariff_type.

  /*
    Activate Auth Type detail exclusion processing when new Auth detail line is added
  */
  ASSIGN cTrackingMessage = "Checking for Exclusions as per the Auth Type Detail Control setup.".

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  FIND buf_auth_detail NO-LOCK
    WHERE buf_auth_detail.auth_obj          = btt_auth_detail.auth_obj
      AND buf_auth_detail.auth_provider_obj = btt_auth_detail.auth_provider_obj
      AND buf_auth_detail.auth_detail_obj   = btt_auth_detail.auth_detail_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE }

  IF btt_auth_detail.auth_detail_obj <= 0
  OR  (AVAILABLE buf_auth_detail
  AND (buf_auth_detail.start_date  <> btt_auth_detail.start_date
  OR   buf_auth_detail.related_obj <> btt_auth_detail.related_obj))
  THEN DO:
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

    IF AVAILABLE buf_auth_type_provider THEN
    DO:
      /* Check if any Auth Type Detail setup exists for Exclusions */
      IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
      DO:
        IF LOOKUP(btt_auth_detail.related_entity_mnemonic,"hlmnl,hlmcr":U) = 0
        THEN  /* Note: We can't use the hat_auth_detail.owning_obj to find buf_auth_type_detail because the detail line will never link to a default tariff link setup. */
          FIND FIRST buf_auth_type_detail NO-LOCK
            WHERE  buf_auth_type_detail.auth_type_obj	       = goAuthorisation:AuthTypeObj
	      AND  buf_auth_type_detail.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
	      AND  buf_auth_type_detail.detail_type_indicator  = "ma_acAuthDetailTypeIndicatorExcl":U
              AND  buf_auth_type_detail.owning_entity_mnemonic = btt_auth_detail.owning_entity_mnemonic
              AND  buf_auth_type_detail.owning_alt_value       = btt_auth_detail.owning_alt_value
              AND  buf_auth_type_detail.effective_date        <= btt_auth_detail.start_date
	      AND (buf_auth_type_detail.end_date               = ?
               OR  buf_auth_type_detail.end_date              >= btt_auth_detail.start_date)
            NO-ERROR.
        ELSE  /* We need to cater for the scenarios where a basket or nappi can be saved as a related entity on a Tariff. */
          FIND FIRST buf_auth_type_detail NO-LOCK
            WHERE  buf_auth_type_detail.auth_type_obj          = goAuthorisation:AuthTypeObj
	      AND  buf_auth_type_detail.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
	      AND  buf_auth_type_detail.detail_type_indicator  = "ma_acAuthDetailTypeIndicatorExcl":U
              AND  buf_auth_type_detail.owning_entity_mnemonic = btt_auth_detail.related_entity_mnemonic
              AND  buf_auth_type_detail.owning_obj             = btt_auth_detail.related_obj
	      AND  buf_auth_type_detail.owning_key	       = btt_auth_detail.related_key
              AND  buf_auth_type_detail.effective_date        <= btt_auth_detail.start_date
	      AND (buf_auth_type_detail.end_date               = ?
               OR  buf_auth_type_detail.end_date	      >= btt_auth_detail.start_date)
            NO-ERROR.
      END.  /* IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U */
      ELSE
        FIND FIRST buf_auth_type_detail NO-LOCK
          WHERE  buf_auth_type_detail.auth_type_obj          = goAuthorisation:AuthTypeObj
            AND  buf_auth_type_detail.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
	    AND  buf_auth_type_detail.detail_type_indicator  = "ma_acAuthDetailTypeIndicatorExcl":U
            AND  buf_auth_type_detail.owning_entity_mnemonic = btt_auth_detail.owning_entity_mnemonic
            AND  buf_auth_type_detail.owning_obj             = btt_auth_detail.owning_obj
	    AND  buf_auth_type_detail.owning_key             = btt_auth_detail.owning_key
            AND  buf_auth_type_detail.effective_date        <= btt_auth_detail.start_date
	    AND (buf_auth_type_detail.end_date               = ?
             OR  buf_auth_type_detail.end_date              >= btt_auth_detail.start_date)
          NO-ERROR.

      /* If Clinical Detail Line is not declined and an Authorisation Type Exclusion must apply. */
      IF AVAILABLE buf_auth_type_detail
      AND (btt_auth_detail.auth_status      <> integer(buf_auth_type_detail.default_auth_status)
       OR  btt_auth_detail.auth_status_note <> buf_auth_type_detail.default_auth_status_note) THEN
      DO:
         ASSIGN btt_auth_detail.auth_status      = integer(buf_auth_type_detail.default_auth_status)
                btt_auth_detail.auth_status_note = buf_auth_type_detail.default_auth_status_note.

         /* Check Auth Rule "AuthTypeExclWarn" setup */
         mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,    // ipdInsurerObj
                                                        INPUT  goAuthorisation:OptionCode,    // ipiOptionCode
                                                        INPUT  "ma_acAuthRuleTypeAuthReg":U,  // ipcRuleType
                                                        INPUT  "AuthTypeExclWarn":U,          // ipcRuleCode
                                                        INPUT  goAuthorisation:StartDate,     // ipdEffectiveDate
                                                        OUTPUT lValidRule,                    // oplValidRule
                                                        OUTPUT cRuleValue                     // opcRuleValue
                                                        ).
        IF lValidRule THEN
        DO:
          IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
          DO:
            ASSIGN cTrackingMessage = "Auth Type Exclusion applied to detail line: " + btt_auth_detail.owning_alt_value.

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            ASSIGN cError       = "Authorisation Type Detail exclusion applied; detail line is declined."
                                + "[HELP=Auth Rule Code: AuthTypeExclWarn]":U
                   lAcknowledge = (IF cRuleValue = "WARNACK":U THEN TRUE ELSE FALSE).

            ASSIGN lSuccess     = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                                         INPUT btt_auth_detail.auth_detail_obj,                    // ipdOwningEntityObj
                                                         INPUT "":U,                                               // ipcOwningEntityKey
                                                         INPUT "auth_status":U,                                    // ipcFieldName
                                                         INPUT btt_auth_detail.line_number,                        // ipiLineNumber
                                                         INPUT cError,                                             // ipcMessageText
                                                         INPUT "WAR":U,                                            // ipcMessageType
                                                         INPUT lAcknowledge).                                      // ipcAcknowledge
          END.  // IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
          ELSE
            ASSIGN cError   = "Auth Rule Value: ":U + cRuleValue + ",[HELP=Auth Rule Code: AuthTypeExclWarn]":U
                   lSuccess = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,        // ipcOwningEntityMnemonic
                                                     INPUT btt_auth_detail.auth_detail_obj,                          // ipdOwningEntityObj
                                                     INPUT "":U,                                                     // ipcOwningEntityKey
                                                     INPUT "auth_status":U,                                          // ipcFieldName
                                                     INPUT btt_auth_detail.line_number,                              // ipiLineNumber
                                                     INPUT "MA":U,                                                   // ipcMessageGroup
                                                     INPUT 112,  /* The '&1' specified is invalid. &2 */             // ipiMessageNumber
                                                     INPUT cError).                                                  // ipcReplaceTextList
        END. // IF lValidRule THEN
      END. /* IF AVAILABLE buf_auth_type_detail AND (btt_auth_detail.auth_status <> buf_auth_type_detail.defaul_auth_status ... */

      /* Clinical Detail Line is already declined but Clinical Detail Line date or related entity code was changed.                                                      */
      /* Determine if line is declined because of an Auth Type Exclusion - Authorise line if it was declined and
         exclusion no longer apply (not available). */
       IF  AVAILABLE buf_auth_detail
      AND  buf_auth_detail.auth_status  = 6
      AND (btt_auth_detail.start_date  <> buf_auth_detail.start_date
       OR  btt_auth_detail.related_obj <> buf_auth_detail.related_obj)
      AND NOT AVAILABLE buf_auth_type_detail THEN
      DO:
        IF buf_auth_detail.owning_entity_mnemonic = "htmtl":U
        THEN  DO:
        IF LOOKUP(buf_auth_detail.related_entity_mnemonic,"hlmnl,hlmcr":U) = 0
          THEN  /* Note:  We can't use the hat_auth_detail.owning_obj to find hac_auth_type_detail because the detail line will never link to a default tariff link setup. */
            FIND FIRST buf_auth_type_detail NO-LOCK
              WHERE  buf_auth_type_detail.auth_type_obj          = goAuthorisation:AuthTypeObj
                AND  buf_auth_type_detail.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
                AND  buf_auth_type_detail.detail_type_indicator  = "ma_acAuthDetailTypeIndicatorExcl":U
                AND  buf_auth_type_detail.owning_entity_mnemonic = buf_auth_detail.owning_entity_mnemonic
                AND  buf_auth_type_detail.owning_alt_value       = buf_auth_detail.owning_alt_value
                AND  buf_auth_type_detail.effective_date       	<= buf_auth_detail.start_date
	  	AND (buf_auth_type_detail.end_date               = ?
                 OR  buf_auth_type_detail.end_date	        >= buf_auth_detail.start_date)
              NO-ERROR.
          ELSE /* We need to cater for the scenarios where a basket or nappi can be saved as a related entity on a Tariff. */
            FIND FIRST buf_auth_type_detail NO-LOCK
              WHERE  buf_auth_type_detail.auth_type_obj	   	 = goAuthorisation:AuthTypeObj
                AND  buf_auth_type_detail.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
                AND  buf_auth_type_detail.detail_type_indicator  = "ma_acAuthDetailTypeIndicatorExcl":U
                AND  buf_auth_type_detail.owning_entity_mnemonic = buf_auth_detail.related_entity_mnemonic
                AND  buf_auth_type_detail.owning_obj             = buf_auth_detail.related_obj
                AND  buf_auth_type_detail.owning_key             = buf_auth_detail.related_key
                AND  buf_auth_type_detail.effective_date       	<= buf_auth_detail.start_date
                AND (buf_auth_type_detail.end_date               = ?
                 OR  buf_auth_type_detail.end_date              >= buf_auth_detail.start_date)
              NO-ERROR.
        END.  /* THEN - IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U */
        ELSE
          FIND FIRST buf_auth_type_detail NO-LOCK
            WHERE  buf_auth_type_detail.auth_type_obj          = goAuthorisation:AuthTypeObj
              AND  buf_auth_type_detail.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
              AND  buf_auth_type_detail.detail_type_indicator  = "ma_acAuthDetailTypeIndicatorExcl":U
              AND  buf_auth_type_detail.owning_entity_mnemonic = buf_auth_detail.owning_entity_mnemonic
              AND  buf_auth_type_detail.owning_obj	       = buf_auth_detail.owning_obj
              AND  buf_auth_type_detail.owning_key	       = buf_auth_detail.owning_key
              AND  buf_auth_type_detail.effective_date        <= buf_auth_detail.start_date
              AND (buf_auth_type_detail.end_date               = ?
               OR  buf_auth_type_detail.end_date              >= btt_auth_detail.start_date)
            NO-ERROR.

        IF  AVAILABLE buf_auth_type_detail
        AND buf_auth_type_detail.default_auth_status_note = buf_auth_detail.auth_status_note THEN
        DO:
          ASSIGN btt_auth_detail.auth_status      = 1
                 btt_auth_detail.auth_status_note = "":U
                 btt_auth_detail.claim_code       = btt_auth_detail.default_claim_code.

          /* Check Auth Rule "AuthTypeExclWarn" setup */
          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,   // ipdInsurerObj
                                                         INPUT  goAuthorisation:OptionCode,    // ipiOptionCode
                                                         INPUT  "ma_acAuthRuleTypeAuthReg":U,  // ipcRuleType
                                                         INPUT  "AuthTypeExclWarn":U,          // ipcRuleCode
                                                         INPUT  goAuthorisation:StartDate,     // ipdEffectiveDate
                                                         OUTPUT lValidRule,                    // oplValidRule
                                                         OUTPUT cRuleValue                     // opcRuleValue
                                                         ).
          IF lValidRule THEN
          DO:
            IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
            DO:
              ASSIGN cTrackingMessage = "Auth Type Exclusion no longer applies to detail line: " + btt_auth_detail.owning_alt_value.

              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

              ASSIGN cError       = "Authorisation Type Detail exclusion no longer applies."
                                  + "[HELP=Auth Rule Code: AuthTypeExclWarn]":U
                     lAcknowledge = (IF cRuleValue = "WARNACK":U THEN TRUE ELSE FALSE).

              ASSIGN lSuccess     = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                                           INPUT btt_auth_detail.auth_detail_obj,                    // ipdOwningEntityObj
                                                           INPUT "":U,                                               // ipcOwningEntityKey
                                                           INPUT "auth_status":U,                                    // ipcFieldName
                                                           INPUT btt_auth_detail.line_number,                        // ipiLineNumber
                                                           INPUT cError,                                             // ipcMessageText
                                                           INPUT "WAR":U,                                            // ipcMessageType
                                                           INPUT lAcknowledge).                                      // ipcAcknowledge
            END.  // IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
            ELSE
              ASSIGN cError   = "Auth Rule Value: ":U + cRuleValue + ",[HELP=Auth Rule Code: AuthTypeExclWarn]":U
                     lSuccess = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,      // ipcOwningEntityMnemonic
                                                       INPUT btt_auth_detail.auth_detail_obj,                        // ipdOwningEntityObj
                                                       INPUT "":U,                                                   // ipcOwningEntityKey
                                                       INPUT "auth_status":U,                                        // ipcFieldName
                                                       INPUT btt_auth_detail.line_number,                            // ipiLineNumber
                                                       INPUT "MA":U,                                                 // ipcMessageGroup
                                                       INPUT 112,  /* The '&1' specified is invalid. &2 */           // ipiMessageNumber
                                                       INPUT cError).                                                // ipcReplaceTextList
          END. /* IF lValidRule THEN */
        END. /* IF  AVAILABLE buf_auth_type_detail AND buf_auth_detail.auth_status_note = buf_auth_type_detail.default_auth_status_note THEN */
      END.  /* IF  AVAILABLE buf_auth_detail AND  buf_auth_detail.auth_status = 6 */
    END. /* IF AVAILABLE buf_auth_type_provider THEN */
  END. /* IF btt_auth_detail.auth_detail_obj <= 0 ... */

&ENDIF

