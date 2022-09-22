/* maauthbusvalprovcopay.i  MEDSTAR Medical Aid System
                            Validate Auth Provider Co-payment
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/
IF AVAILABLE ttAuthTypeConfig
AND ttAuthTypeConfig.ActivateCopayment
AND btt_auth_provider.main_provider
AND btt_auth_provider.authorised_service
AND btt_auth_provider.amount_paid = 0 AND btt_auth_provider.quantity_paid = 0 THEN
DO:

  ASSIGN iDocNegNum     = INTEGER(TRIM(ENTRY(1, btt_auth_provider._neg_group, ":"), "("))
         lCheckForCopay = FALSE
         dOldCopayControlObj = btt_auth_provider.auth_copay_control_obj.

  /*
    Check for copayments if:
      - it's a new auth provider; or
      - the doc_num / start_date or provider_type has changed
    For this scenario, we run the checkForCopay, but we don't pass
    the ipdAuthCopayControlObj, ipcCopayOverrideNote or iplCopayProvider
    in to the service.
  */
  IF btt_auth_provider.auth_provider_obj <= 0
  OR (AVAILABLE buf_auth_provider
      AND (btt_auth_provider.doc_num       <> buf_auth_provider.doc_num
      OR   btt_auth_provider.start_date    <> buf_auth_provider.start_date
      OR   btt_auth_provider.provider_type <> buf_auth_provider.provider_type)) THEN
  DO:
    ASSIGN cTrackingMessage = "CopayAuthProvider-INPUT1-"     +
                              "ipdInsurerObj="            + (IF goAuthorisation:InsurerObj <> ?        THEN STRING(goAuthorisation:InsurerObj)                ELSE "?") +
                              ";ipiOptionCode="           + (IF goAuthorisation:MemberOptionCode <> ?  THEN STRING(goAuthorisation:MemberOptionCode)          ELSE "?") +
                              ";ipdDate="                 + (IF btt_auth_provider.start_date <> ?      THEN STRING(btt_auth_provider.start_date,"9999/99/99") ELSE "?") +
                              ";ipcEmergency="            + (IF btt_auth_provider._emergency_flag <> ? THEN STRING(btt_auth_provider._emergency_flag)         ELSE "?") +
                              ";iplPMB="                  + (IF btt_auth_provider.pmb_indicator <> ?   THEN STRING(btt_auth_provider.pmb_indicator)           ELSE "?") +
                              ";ipiDocNum="               + (IF btt_auth_provider.doc_num <> ?         THEN STRING(btt_auth_provider.doc_num)                 ELSE "?") +
                              ";ipiPrType="               + (IF btt_auth_provider.pr_type <> ?         THEN STRING(btt_auth_provider.pr_type)                 ELSE "?") +
                              ";ipiDocNegNum="            + (IF iDocNegNum <> ?                        THEN STRING(iDocNegNum)                                ELSE "?") +
                              ";ipdAuthCopayControlObj=0" +
                              ";ipcCopayOverrideNote=''"  +
                              ";iplCopayProvider='?'"     +
                              ";ipiAuthStatus="           + (IF btt_auth_provider.auth_status <> ?     THEN STRING(btt_auth_provider.auth_status)             ELSE "?") +
                              ";ipcAuthStatusNote="       + btt_auth_provider.auth_status_note
           lCheckForCopay = TRUE.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    mipEnv:Health:AuthService:checkForCopay (INPUT  goAuthorisation:InsurerObj,         // ipdInsurerObj           - Client
                                             INPUT  goAuthorisation:MemberOptionCode,   // ipiOptionCode           - Member option code
                                             INPUT  btt_auth_provider.start_date,       // ipdDate                 - Date for which setups must be checked.
                                             INPUT  btt_auth_provider._emergency_flag,  // ipcEmergency            - Indicate if it is an emergency.
                                             INPUT  btt_auth_provider.pmb_indicator,    // iplPMB                  - Indicate if it is a PMB.
                                             INPUT  btt_auth_provider.doc_num,          // ipiDocNum               - Doctor number/Provider number.
                                             INPUT  btt_auth_provider.pr_type,          // ipiPrType               - Doctor/Provider discipline.
                                             INPUT  iDocNegNum,                         // ipiDocNegNum            - Provider DNU group.
                                             INPUT  0,                                  // ipdAuthCopayControlObj  - Co-payment control obj to which the provider currently links.
                                             INPUT  "",                                 // ipcCopayOverrideNote    - Co-payment override note captured when the authorisation is authorised.
                                             INPUT  ?,                                  // iplCopayProvider        - Indicate if provider is a co-payment provider.
                                             INPUT  btt_auth_provider.auth_status,      // ipiAuthStatus           - Authorisation status before co-payment processing.
                                             INPUT  btt_auth_provider.auth_status_note, // ipcAuthStatusNote       - Authorisation status reason before co-payment processing.
                                             INPUT  btt_auth_provider.provider_type,    // ipcProviderType         - Provider type, e.g. Admitting or Main Treating, etc.
                                             OUTPUT dAuthCopayControlObj,               // dAuthCopayControlObj    - hac_auth_copay_control.auth_copay_control_obj.  Will not contain a value if no co-payment should apply.
                                             OUTPUT lCopayProvider,                     // oplCopayProvider        - Output parameter that will indicate if the provider is a co-payment provider.
                                             OUTPUT iAuthStatus,                        // opiAuthStatus           - hat_auth_copay_control.auth_status.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cAuthStatusNote,                    // opcAuthStatusNote       - hat_auth_copay_control.auth_status_note.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cWarning,                           // opcWarning              - hat_auth_copay_control.warning_message.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cWarningType,                       // opcWarningType          - hat_auth_copay_control.warning_message_type.  Will not contain a value if no co-payment should apply.
                                             OUTPUT dAuthCopayTypeObj,                  // opdAuthCopayTypeObj     - hat_auth_copay_control.auth_copay_type_obj.  Will not contain a value if no co-payment should apply.
                                             OUTPUT lCopayValueType,                    // oplCopayValueType       - hat_auth_copay_control.copayment_value_type.  Will not contain a value if no co-payment should apply.
                                             OUTPUT dCopayValue,                        // opdCopayValue           - hat_auth_copay_control.copayment_value.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cErrorMessage).                     // opcError                - Return errors that should be handled in the calling procedure.
  END.  // IF btt_auth_provider.auth_provider_obj <= 0

  /*
    If the emergency_flag, auth status or copay_override_note has changed,
    we need to check if the current copay control still applies.
  */
  ELSE IF (AVAILABLE buf_auth_provider
      AND (btt_auth_provider._emergency_flag_updated = YES
      OR   btt_auth_provider.auth_status         <> buf_auth_provider.auth_status
      OR   btt_auth_provider.copay_override_note <> buf_auth_provider.copay_override_note)) THEN
  DO:
    ASSIGN cTrackingMessage = "CopayAuthProvider-INPUT2-"    +
                              "ipdInsurerObj="           + (IF goAuthorisation:InsurerObj <> ?               THEN STRING(goAuthorisation:InsurerObj)                ELSE "?") +
                              ";ipiOptionCode="          + (IF goAuthorisation:MemberOptionCode <> ?         THEN STRING(goAuthorisation:MemberOptionCode)          ELSE "?") +
                              ";ipdDate="                + (IF btt_auth_provider.start_date <> ?             THEN STRING(btt_auth_provider.start_date,"9999/99/99") ELSE "?") +
                              ";ipcEmergency="           + (IF btt_auth_provider._emergency_flag <> ?        THEN STRING(btt_auth_provider._emergency_flag)         ELSE "?") +
                              ";iplPMB="                 + (IF btt_auth_provider.pmb_indicator <> ?          THEN STRING(btt_auth_provider.pmb_indicator)           ELSE "?") +
                              ";ipiDocNum="              + (IF btt_auth_provider.doc_num <> ?                THEN STRING(btt_auth_provider.doc_num)                 ELSE "?") +
                              ";ipiPrType="              + (IF btt_auth_provider.pr_type <> ?                THEN STRING(btt_auth_provider.pr_type)                 ELSE "?") +
                              ";ipiDocNegNum="           + (IF iDocNegNum <> ?                               THEN STRING(iDocNegNum)                                ELSE "?") +
                              ";ipdAuthCopayControlObj=" + (IF btt_auth_provider.auth_copay_control_obj <> ? THEN STRING(btt_auth_provider.auth_copay_control_obj)  ELSE "?") +
                              ";ipcCopayOverrideNote="   + (IF btt_auth_provider.copay_override_note <> ?    THEN        btt_auth_provider.copay_override_note      ELSE "?") +
                              ";iplCopayProvider="       + (IF btt_auth_provider.copay_provider <> ?         THEN STRING(btt_auth_provider.copay_provider)          ELSE "?") +
                              ";ipiAuthStatus="          + (IF btt_auth_provider.auth_status <> ?            THEN STRING(btt_auth_provider.auth_status)             ELSE "?") +
                              ";ipcAuthStatusNote="      + btt_auth_provider.auth_status_note
           lCheckForCopay = TRUE.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    mipEnv:Health:AuthService:checkForCopay (INPUT  goAuthorisation:InsurerObj,                // ipdInsurerObj           - Client
                                             INPUT  goAuthorisation:MemberOptionCode,          // ipiOptionCode           - Member option code
                                             INPUT  btt_auth_provider.start_date,              // ipdDate                 - Date for which setups must be checked.
                                             INPUT  btt_auth_provider._emergency_flag,         // ipcEmergency            - Indicate if it is an emergency.
                                             INPUT  btt_auth_provider.pmb_indicator,           // iplPMB                  - Indicate if it is a PMB.
                                             INPUT  btt_auth_provider.doc_num,                 // ipiDocNum               - Doctor number/Provider number.
                                             INPUT  btt_auth_provider.pr_type,                 // ipiPrType               - Doctor/Provider discipline.
                                             INPUT  iDocNegNum,                                // ipiDocNegNum            - Provider DNU group.
                                             INPUT  btt_auth_provider.auth_copay_control_obj,  // ipdAuthCopayControlObj  - Co-payment control obj to which the provider currently links.
                                             INPUT  btt_auth_provider.copay_override_note,     // ipcCopayOverrideNote    - Co-payment override note captured when the authorisation is authorised.
                                             INPUT  btt_auth_provider.copay_provider,          // iplCopayProvider        - Indicate if provider is a co-payment provider.
                                             INPUT  btt_auth_provider.auth_status,             // ipiAuthStatus           - Authorisation status before co-payment processing.
                                             INPUT  btt_auth_provider.auth_status_note,        // ipcAuthStatusNote       - Authorisation status reason before co-payment processing.
                                             INPUT  btt_auth_provider.provider_type,           // ipcProviderType         - Provider type, e.g. Admitting or Main Treating, etc.
                                             OUTPUT dAuthCopayControlObj,                      // dAuthCopayControlObj    - hac_auth_copay_control.auth_copay_control_obj.  Will not contain a value if no co-payment should apply.
                                             OUTPUT lCopayProvider,                            // oplCopayProvider        - Output parameter that will indicate if the provider is a co-payment provider.
                                             OUTPUT iAuthStatus,                               // opiAuthStatus           - hat_auth_copay_control.auth_status.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cAuthStatusNote,                           // opcAuthStatusNote       - hat_auth_copay_control.auth_status_note.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cWarning,                                  // opcWarning              - hat_auth_copay_control.warning_message.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cWarningType,                              // opcWarningType          - hat_auth_copay_control.warning_message_type.  Will not contain a value if no co-payment should apply.
                                             OUTPUT dAuthCopayTypeObj,                         // opdAuthCopayTypeObj     - hat_auth_copay_control.auth_copay_type_obj.  Will not contain a value if no co-payment should apply.
                                             OUTPUT lCopayValueType,                           // oplCopayValueType       - hat_auth_copay_control.copayment_value_type.  Will not contain a value if no co-payment should apply.
                                             OUTPUT dCopayValue,                               // opdCopayValue           - hat_auth_copay_control.copayment_value.  Will not contain a value if no co-payment should apply.
                                             OUTPUT cErrorMessage).                            // opcError                - Return errors that should be handled in the calling procedure.
  END.  // ELSE - IF btt_auth_provider.auth_provider_obj <= 0

  /*
    If the PMB indicator on the provider has changed, we need to check if the
    copayment applies to a PMB.
  */
  ELSE IF btt_auth_provider.pmb_indicator <> buf_auth_provider.pmb_indicator THEN
  DO:
    FIND hac_auth_copay_control NO-LOCK
      WHERE hac_auth_copay_control.auth_copay_control_obj = btt_auth_provider.auth_copay_control_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE hac_auth_copay_control
    AND hac_auth_copay_control.apply_to_pmb = NO THEN
    DO:
      ASSIGN cTrackingMessage = "CopayAuthProvider-INPUT3-"    +
                                "ipdInsurerObj="           + (IF goAuthorisation:InsurerObj <> ?               THEN STRING(goAuthorisation:InsurerObj)                ELSE "?") +
                                ";ipiOptionCode="          + (IF goAuthorisation:MemberOptionCode <> ?         THEN STRING(goAuthorisation:MemberOptionCode)          ELSE "?") +
                                ";ipdDate="                + (IF btt_auth_provider.start_date <> ?             THEN STRING(btt_auth_provider.start_date,"9999/99/99") ELSE "?") +
                                ";ipcEmergency="           + (IF btt_auth_provider._emergency_flag <> ?        THEN STRING(btt_auth_provider._emergency_flag)         ELSE "?") +
                                ";iplPMB="                 + (IF btt_auth_provider.pmb_indicator <> ?          THEN STRING(btt_auth_provider.pmb_indicator)           ELSE "?") +
                                ";ipiDocNum="              + (IF btt_auth_provider.doc_num <> ?                THEN STRING(btt_auth_provider.doc_num)                 ELSE "?") +
                                ";ipiPrType="              + (IF btt_auth_provider.pr_type <> ?                THEN STRING(btt_auth_provider.pr_type)                 ELSE "?") +
                                ";ipiDocNegNum="           + (IF iDocNegNum <> ?                               THEN STRING(iDocNegNum)                                ELSE "?") +
                                ";ipdAuthCopayControlObj=" + (IF btt_auth_provider.auth_copay_control_obj <> ? THEN STRING(btt_auth_provider.auth_copay_control_obj)  ELSE "?") +
                                ";ipcCopayOverrideNote="   + (IF btt_auth_provider.copay_override_note <> ?    THEN        btt_auth_provider.copay_override_note      ELSE "?") +
                                ";iplCopayProvider=?"      +
                                ";ipiAuthStatus="          + (IF btt_auth_provider.auth_status <> ?            THEN STRING(btt_auth_provider.auth_status)             ELSE "?") +
                                ";ipcAuthStatusNote="      + btt_auth_provider.auth_status_note
           lCheckForCopay = TRUE.
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      /*
        We need to check the PMB logic again, therefor we send iplCopayProvider in as a questionmark.
      */
      mipEnv:Health:AuthService:checkForCopay (INPUT  goAuthorisation:InsurerObj,                // ipdInsurerObj           - Client
                                               INPUT  goAuthorisation:MemberOptionCode,          // ipiOptionCode           - Member option code
                                               INPUT  btt_auth_provider.start_date,              // ipdDate                 - Date for which setups must be checked.
                                               INPUT  btt_auth_provider._emergency_flag,         // ipcEmergency            - Indicate if it is an emergency.
                                               INPUT  btt_auth_provider.pmb_indicator,           // iplPMB                  - Indicate if it is a PMB.
                                               INPUT  btt_auth_provider.doc_num,                 // ipiDocNum               - Doctor number/Provider number.
                                               INPUT  btt_auth_provider.pr_type,                 // ipiPrType               - Doctor/Provider discipline.
                                               INPUT  iDocNegNum,                                // ipiDocNegNum            - Provider DNU group.
                                               INPUT  btt_auth_provider.auth_copay_control_obj,  // ipdAuthCopayControlObj  - Co-payment control obj to which the provider currently links.
                                               INPUT  btt_auth_provider.copay_override_note,     // ipcCopayOverrideNote    - Co-payment override note captured when the authorisation is authorised.
                                               INPUT  ?,                                         // iplCopayProvider        - Indicate if provider is a co-payment provider.
                                               INPUT  btt_auth_provider.auth_status,             // ipiAuthStatus           - Authorisation status before co-payment processing.
                                               INPUT  btt_auth_provider.auth_status_note,        // ipcAuthStatusNote       - Authorisation status reason before co-payment processing.
                                               INPUT  btt_auth_provider.provider_type,           // ipcProviderType         - Provider type, e.g. Admitting or Main Treating, etc.
                                               OUTPUT dAuthCopayControlObj,                      // dAuthCopayControlObj    - hac_auth_copay_control.auth_copay_control_obj.  Will not contain a value if no co-payment should apply.
                                               OUTPUT lCopayProvider,                            // oplCopayProvider        - Output parameter that will indicate if the provider is a co-payment provider.
                                               OUTPUT iAuthStatus,                               // opiAuthStatus           - hat_auth_copay_control.auth_status.  Will not contain a value if no co-payment should apply.
                                               OUTPUT cAuthStatusNote,                           // opcAuthStatusNote       - hat_auth_copay_control.auth_status_note.  Will not contain a value if no co-payment should apply.
                                               OUTPUT cWarning,                                  // opcWarning              - hat_auth_copay_control.warning_message.  Will not contain a value if no co-payment should apply.
                                               OUTPUT cWarningType,                              // opcWarningType          - hat_auth_copay_control.warning_message_type.  Will not contain a value if no co-payment should apply.
                                               OUTPUT dAuthCopayTypeObj,                         // opdAuthCopayTypeObj     - hat_auth_copay_control.auth_copay_type_obj.  Will not contain a value if no co-payment should apply.
                                               OUTPUT lCopayValueType,                           // oplCopayValueType       - hat_auth_copay_control.copayment_value_type.  Will not contain a value if no co-payment should apply.
                                               OUTPUT dCopayValue,                               // opdCopayValue           - hat_auth_copay_control.copayment_value.  Will not contain a value if no co-payment should apply.
                                               OUTPUT cErrorMessage).                            // opcError                - Return errors that should be handled in the calling procedure.
    END.  // IF AVAILABLE hac_auth_copay_control...
  END.  // ELSE IF btt_auth_provider.pmb_indicator <> buf_auth_provider.pmb_indicator THEN

  ASSIGN cTrackingMessage = "CopayAuthProvider-OUTPUT-" +
                            "dAuthCopayControlObj="  + STRING(dAuthCopayControlObj) +
                            ";lCopayProvider="       + (IF lCopayProvider <> ?    THEN STRING(lCopayProvider)   ELSE "?") +
                            ";iAuthStatus="          + (IF iAuthStatus <> ?       THEN STRING(iAuthStatus)      ELSE "?") +
                            ";cAuthStatusNote="      + cAuthStatusNote +
                            ";cWarning="             + cWarning +
                            ";cWarningType="         + cWarningType +
                            ";dAuthCopayTypeObj="    + (IF dAuthCopayTypeObj <> ? THEN STRING(dAuthCopayTypeObj) ELSE "?") +
                            ";lCopayValueType="      + (IF lCopayValueType <> ?   THEN STRING(lCopayValueType)   ELSE "?") +
                            ";dCopayValue="          + (IF dCopayValue <> ?       THEN STRING(dCopayValue)       ELSE "?") +
                            ";cErrorMessage="        + cErrorMessage +
                            ";lCheckForCopay="       + (IF lCheckForCopay <> ?    THEN STRING(lCheckForCopay)    ELSE "?").
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    If we ran the checkForCopay service, the lCheckForCopay will be true.
  */
  IF lCheckForCopay THEN
  DO:
    /*
      Get the auth rule code for the Copay Flag
    */
    ASSIGN cRuleType    = "ma_acAuthRuleTypeAuthFlag":U
           cRuleCode    = "COPAY":U
           dStartDate   = btt_auth_provider.start_date
           dInsurerObj  = goAuthorisation:InsurerObj
           iOptionCode  = goAuthorisation:MemberOptionCode
           dAuthRuleObj = 0.

    mipEnv:Health:AuthMaintenance:getAuthRuleDetails(INPUT-OUTPUT dAuthRuleObj,
                                                     INPUT-OUTPUT dInsurerObj,
                                                     INPUT-OUTPUT iOptionCode,
                                                     INPUT-OUTPUT cRuleType,
                                                     INPUT-OUTPUT cRuleCode,
                                                     INPUT-OUTPUT dStartDate,
                                                           OUTPUT lValidRule,
                                                           OUTPUT cRuleValue,
                                                           OUTPUT cRuleValidValues,
                                                           OUTPUT dLinkAuthRuleObj,
                                                           OUTPUT cRuleDescription,
                                                           OUTPUT lSystemOwned,
                                                           OUTPUT dEndDate).
    FIND FIRST btt_auth_flag_copay
      WHERE btt_auth_flag_copay.owning_entity_mnemonic = "hatau":U
      AND   btt_auth_flag_copay.owning_obj             = btt_auth_provider.auth_obj
      AND   btt_auth_flag_copay.owning_key             = "":U
      AND   btt_auth_flag_copay.auth_rule_obj          = dAuthRuleObj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_flag_copay THEN
    DO:
      ASSIGN cMessage = "Copay Flag must be captured if co-payment is activated on the Authorisation Type."
                      + "[HELP=Auth Rule Code: Copay]".
      goErrorObject:addError(INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                             INPUT "":U,                                /* ipcOwningEntityKey       */
                             INPUT "doc_num":U,                         /* ipcFieldName             */
                             INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                             INPUT cMessage,                            /* ipcMessageText           */
                             INPUT "ERR":U).                            /* ipcMessageType           */
    END.  // IF NOT AVAILABLE btt_auth_flag_copay THEN
    ELSE DO:
      IF btt_auth_flag_copay.auth_flag_value = ""
      THEN
        ASSIGN cCopayFlagValue = "NO".
    END.  // ELSE - IF NOT AVAILABLE btt_auth_flag_copay THEN

    IF btt_auth_provider.auth_copay_control_obj <> dAuthCopayControlObj
    THEN
      ASSIGN btt_auth_provider.auth_copay_control_obj = dAuthCopayControlObj.

    IF btt_auth_provider.auth_copay_control_obj <> 0
    THEN
      ASSIGN btt_auth_provider.copay_provider = lCopayProvider.

    ASSIGN cTrackingMessage = "CopayAuthProvider-PROVIDER-" +
                              "btt_auth_provider.auth_copay_control_obj=" + STRING(btt_auth_provider.auth_copay_control_obj) +
                              ";btt_auth_provider.copay_provider="        + STRING(btt_auth_provider.copay_provider).
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /*
       If the authorisation status is returned it indicates that we should
       change the authorisation status because a co-payment will apply.
    */
    IF btt_auth_provider.copay_provider = YES
    AND iAuthStatus <> ? THEN
    DO:
      IF iAuthStatus <> btt_auth_provider.auth_status
      THEN
        ASSIGN btt_auth_provider.auth_status = iAuthStatus.

      IF  cAuthStatusNote <> ""
      AND cAuthStatusNote <> btt_auth_provider.auth_status_note
      THEN
        ASSIGN btt_auth_provider.auth_status_note = cAuthStatusNote.

      ASSIGN cTrackingMessage = "CopayAuthProvider-PROVIDER STATUS- "      +
                                "btt_auth_provider.auth_status="        + STRING(btt_auth_provider.auth_status) +
                                "; btt_auth_provider.auth_status_note=" + btt_auth_provider.auth_status_note.
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      /*
         If the authorisation status is changed, the warning message that
         is returned from CheckForCopay must be populated if it is not blank.
      */
      IF cWarning <> "":U THEN
      DO:
        IF cWarningType = "ma_acAuthCopayWarnMessageTypeWarnAck":U
        THEN
          ASSIGN lAcknowledge = TRUE.
        ELSE
          ASSIGN lAcknowledge = FALSE.

        goErrorObject:addError(INPUT "hatap":U,                            // ipcOwningEntityMnemonic
                               INPUT btt_auth_provider.auth_provider_obj,  // ipdOwningEntityObj
                               INPUT "":U,                                 // ipcOwningEntityKey
                               INPUT btt_auth_provider.line_number,        // ipiLineNumber
                               INPUT cWarning,                             // ipcMessageText
                               INPUT "WAR":U,                              // ipiMessageType
                               INPUT lAcknowledge).                        // iplAcknowledge
      END.  // IF cWarning <> "":U THEN

    END.  // IF btt_auth_provider.copay_provider = YES

    /*
      We need to clear the co-payment override note to cater for
      scenarios where the co-payment should no longer apply.
    */
    IF lCopayProvider <> YES
    THEN
      ASSIGN btt_auth_provider.copay_override_note = "".

    /*
       If a co-payment value is returned, we should apply the co-payment
       for the provider and create or update hat_auth_copay.
    */
    FIND FIRST btt_auth_copay
      WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
      AND   btt_auth_copay.auth_copay_type_obj    = dAuthCopayTypeObj
      AND   btt_auth_copay.owning_entity_mnemonic = "hatap":U
      AND   btt_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
      AND   btt_auth_copay.owning_key             = ""
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    ASSIGN cTrackingMessage = "CopayAuthProvider-APPLY COPAY?- "          +
                              "dAuthCopayTypeObj="                     + STRING(dAuthCopayTypeObj) +
                              "; btt_auth_provider.auth_provider_obj=" + STRING(btt_auth_provider.auth_provider_obj) +
                              "; AVAILABLE btt_auth_copay="            + STRING(AVAILABLE btt_auth_copay) +
                              "; btt_auth_provider.copay_provider="    + STRING(btt_auth_provider.copay_provider) +
                              "; dCopayValue="                         + STRING(dCopayValue).
   {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF  NOT AVAILABLE btt_auth_copay
    AND btt_auth_provider.copay_provider = YES
    AND dCopayValue <> 0 THEN
    DO:
      /*
        If more than one co-payment record is created at a time, we need to ensure
        that the bbt_auth_copay.auth_copay_obj is always unique.
      */
      ASSIGN iAuthCopayObj = 0.
      ObjBlock:
      REPEAT:
        IF CAN-FIND(FIRST bbt_auth_copay
                    WHERE bbt_auth_copay.auth_copay_obj = iAuthCopayObj)
        THEN
          ASSIGN iAuthCopayObj = iAuthCopayObj - 1.
        ELSE
          LEAVE ObjBlock.
      END.  // ObjBlock: REPEAT

      CREATE btt_auth_copay.
      ASSIGN btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
             btt_auth_copay.auth_copay_obj         = iAuthCopayObj
             btt_auth_copay.auth_copay_type_obj    = dAuthCopayTypeObj
             btt_auth_copay.owning_entity_mnemonic = "hatap":U
             btt_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
             btt_auth_copay.owning_key             = "":U
             btt_auth_copay.owning_alt_value       = STRING(btt_auth_provider.doc_num,"9999999":U)
             btt_auth_copay.record_action          = "MODIFY":U
             cCopayFlagValue                       = "YES":U.

      /* Update provider record (btt_auth_provider) with the co-payment values.
         **Please note** - co-payment values must be added and not assigned to btt_auth_provider,
         because co-payment values can include a penalty co-payment as well. */
      IF lCopayValueType = NO
      THEN
        ASSIGN btt_auth_copay.amount        = dCopayValue
               btt_auth_copay.amount_%      = 0
               btt_auth_provider.copay_auth = btt_auth_provider.copay_auth + dCopayValue.
      ELSE
        ASSIGN btt_auth_copay.amount          = 0
               btt_auth_copay.amount_%        = dCopayValue
               btt_auth_provider.copay_auth_% = btt_auth_provider.copay_auth_% + dCopayValue.

      VALIDATE btt_auth_copay.

      ASSIGN cTrackingMessage = "DebugAuthCopay-AUTH COPAY CREATED-"   +
                                "btt_auth_copay.auth_obj="             + STRING(btt_auth_copay.auth_obj) +
                                ";btt_auth_copay.auth_copay_type_obj=" + (IF btt_auth_copay.auth_copay_type_obj <> ? THEN STRING(btt_auth_copay.auth_copay_type_obj) ELSE "?") +
                                ";btt_auth_copay.owning_obj="          + (IF btt_auth_copay.owning_obj <> ? THEN STRING(btt_auth_copay.owning_obj) ELSE "?") +
                                ";btt_auth_copay.owning_alt_value="    + btt_auth_copay.owning_alt_value +
                                ";btt_auth_copay.amount="              + STRING(btt_auth_copay.amount) +
                                ";btt_auth_copay.amount_%="            + STRING(btt_auth_copay.amount_%) +
                                ";btt_auth_provider.copay_auth="       + STRING(btt_auth_provider.copay_auth) +
                                ";btt_auth_provider.copay_auth_%="     + STRING(btt_auth_provider.copay_auth_%).
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    END.  // IF NOT AVAILABLE btt_auth_copay THEN
    ELSE IF AVAILABLE btt_auth_copay then
    DO:
      ASSIGN cTrackingMessage = "".

      /*
         If a co-payment exists already.
         Update the btt_auth_copay record if the co-payment values are different.
         Old values must be subtracted, and new values must be added because the btt_auth_provider
         co-payment values are shared between provider co-payments and penalties.
      */
      IF lCopayValueType = NO /* Rand Amount */
      AND (btt_auth_copay.amount <> dCopayValue OR btt_auth_copay.amount_% <> 0)
      THEN
        ASSIGN dOldCopayValue                 = btt_auth_copay.amount
               dOldCopayValue%                = btt_auth_copay.amount_%
               btt_auth_copay.record_action   = "MODIFY":U
               btt_auth_copay.amount          = dCopayValue
               btt_auth_copay.amount_%        = 0
               btt_auth_provider.copay_auth   = (btt_auth_provider.copay_auth   - dOldCopayValue) + dCopayValue
               btt_auth_provider.copay_auth_% =  btt_auth_provider.copay_auth_% - dOldCopayValue%
               cTrackingMessage               = "UPDATED".
      ELSE IF lCopayValueType = YES /* Percentage */
           AND (btt_auth_copay.amount <> 0 OR btt_auth_copay.amount_% <> dOldCopayValue)
      THEN
        ASSIGN dOldCopayValue                 = btt_auth_copay.amount
               dOldCopayValue%                = btt_auth_copay.amount_%
               btt_auth_copay.record_action   = "MODIFY":U
               btt_auth_copay.amount          = 0
               btt_auth_copay.amount_%        = dCopayValue
               btt_auth_provider.copay_auth	  =  btt_auth_provider.copay_auth   - dOldCopayValue
               btt_auth_provider.copay_auth_%	= (btt_auth_provider.copay_auth_% - dOldCopayValue%) + dCopayValue
               cTrackingMessage               = "UPDATED".

      IF cTrackingMessage <> "" THEN
      DO:
        ASSIGN cTrackingMessage = "CopayAuthProvider-AUTH COPAY " + cTrackingMessage + "-" +
                   "btt_auth_copay.auth_obj="               + STRING(btt_auth_copay.auth_obj) +
                   ";btt_auth_copay.auth_copay_type_obj="   + (IF btt_auth_copay.auth_copay_type_obj <> ? THEN STRING(btt_auth_copay.auth_copay_type_obj) ELSE "?") +
                   ";btt_auth_copay.owning_obj="            + (IF btt_auth_copay.owning_obj <> ? THEN STRING(btt_auth_copay.owning_obj) ELSE "?") +
                   ";btt_auth_copay.owning_alt_value="      + btt_auth_copay.owning_alt_value +
                   ";OLD_auth_copay.amount="                + STRING(dOldCopayValue) +
                   ";OLD_auth_copay.amount_%="              + STRING(dOldCopayValue%) +
                   ";btt_auth_copay.amount="                + STRING(btt_auth_copay.amount) +
                   ";btt_auth_copay.amount_%="              + STRING(btt_auth_copay.amount_%) +
                   ";btt_auth_provider.copay_auth="         + STRING(btt_auth_provider.copay_auth) +
                   ";btt_auth_provider.copay_auth_%="       + STRING(btt_auth_provider.copay_auth_%).
        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      END.  // IF cTrackingMessage <> "" THEN
    END.  // ELSE - IF NOT AVAILABLE btt_auth_copay THEN

    /* If no co-payment value is returned and a co-payment did apply before for the provider the co-payment must be deleted. */
    IF  dCopayValue = 0
    AND (dAuthCopayTypeObj <> 0 OR dOldCopayControlObj <> 0) THEN
    DO:
      IF dAuthCopayTypeObj <> 0
      THEN
        FIND btt_auth_copay EXCLUSIVE-LOCK
          WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
          AND   btt_auth_copay.auth_copay_type_obj    = dAuthCopayTypeObj
          AND   btt_auth_copay.owning_entity_mnemonic = "hatap":U
          AND   btt_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
          AND   btt_auth_copay.owning_key             = ""
          NO-ERROR.
      ELSE DO:
        FIND hac_auth_copay_control NO-LOCK
          WHERE hac_auth_copay_control.auth_copay_control_obj = dOldCopayControlObj
          NO-ERROR.
        IF AVAILABLE hac_auth_copay_control
        THEN
          FIND btt_auth_copay EXCLUSIVE-LOCK
            WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
            AND   btt_auth_copay.auth_copay_type_obj    = hac_auth_copay_control.auth_copay_type_obj
            AND   btt_auth_copay.owning_entity_mnemonic = "hatap":U
            AND   btt_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
            AND   btt_auth_copay.owning_key             = ""
            NO-ERROR.
      END.  // ELSE - IF dAuthCopayTypeObj <> 0

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE btt_auth_copay THEN
      DO:
        /*
           Please note co-payment values must be subtracted and not cleared because the total
           co-payment values on hat_auth_provider may include a co-payment for a penalty as well.
        */
        ASSIGN btt_auth_provider.copay_auth   = btt_auth_provider.copay_auth   - btt_auth_copay.amount
               btt_auth_provider.copay_auth_% = btt_auth_provider.copay_auth_% - btt_auth_copay.amount_%
               btt_auth_copay.record_action   = "Delete":U
               cCopayFlagValue                = "NO":U.

        ASSIGN cTrackingMessage = "CopayAuthProvider-AUTH COPAY DELETED-" +
                                " btt_auth_copay.auth_obj="            + STRING(btt_auth_copay.auth_obj) +
                                ";btt_auth_copay.auth_copay_type_obj=" + (IF btt_auth_copay.auth_copay_type_obj <> ? THEN STRING(btt_auth_copay.auth_copay_type_obj) ELSE "?") +
                                ";btt_auth_copay.owning_obj="          + (IF btt_auth_copay.owning_obj <> ? THEN STRING(btt_auth_copay.owning_obj) ELSE "?") +
                                ";btt_auth_copay.owning_alt_value="    + btt_auth_copay.owning_alt_value +
                                ";btt_auth_copay.amount="              + STRING(btt_auth_copay.amount) +
                                ";btt_auth_copay.amount_%="            + STRING(btt_auth_copay.amount_%) +
                                ";btt_auth_provider.copay_auth="       + STRING(btt_auth_provider.copay_auth) +
                                ";btt_auth_provider.copay_auth_%="     + STRING(btt_auth_provider.copay_auth_%).
        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      END.  // IF AVAILABLE btt_auth_copay THEN
    END.  // IF dCopayValue = 0 AND dAuthCopayTypeObj <> 0 THEN

    IF  cCopayFlagValue <> ""
    AND cCopayFlagValue <> btt_auth_flag_copay.auth_flag_value
    THEN
      ASSIGN btt_auth_flag_copay.record_action        = "MODIFY":U
             btt_auth_flag_copay.auth_flag_value      = cCopayFlagValue
             btt_auth_flag_copay.last_change_datetime = NOW.
  END.  // IF lCheckForCopay THEN
END.  // IF AVAILABLE ttAuthTypeConfig AND...
ELSE IF  btt_auth_provider.amount_paid   = 0
     AND btt_auth_provider.quantity_paid = 0
     THEN
       ASSIGN
         btt_auth_provider.auth_copay_control_obj = 0
         btt_auth_provider.copay_provider         = ?
         btt_auth_provider.copay_override_note    = "".

