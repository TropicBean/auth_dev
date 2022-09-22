/* maauthbusvalprovpenalty.i  MEDSTAR Medical Aid System
                              Co-payment penalty processing
                              (c) Copyright 1990 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/

IF ttAuthTypeConfig.ActivatePenalty
AND btt_auth_provider.main_provider
AND btt_auth_provider.authorised_service
AND btt_auth_provider.amount_paid   = 0
AND btt_auth_provider.quantity_paid = 0 THEN
DO:
  /*
    Get the auth rule code for the Penalty Flag
  */
  ASSIGN cRuleType    = "ma_acAuthRuleTypeAuthFlag":U
         cRuleCode    = "PENALTY":U
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
  FIND FIRST btt_auth_flag_penalty
    WHERE btt_auth_flag_penalty.owning_entity_mnemonic = "hatau":U
    AND   btt_auth_flag_penalty.owning_obj             = btt_auth_provider.auth_obj
    AND   btt_auth_flag_penalty.owning_key             = "":U
    AND   btt_auth_flag_penalty.auth_rule_obj          = dAuthRuleObj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE btt_auth_flag_penalty THEN
  DO:
    ASSIGN cMessage = "Penalty Flag must be captured if penalty is activated on the Authorisation Type."
                    + "[HELP=Auth Rule Code: Penalty]".
    goErrorObject:addError(INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                           INPUT "":U,                                /* ipcOwningEntityKey       */
                           INPUT "doc_num":U,                         /* ipcFieldName             */
                           INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                           INPUT cMessage,                            /* ipcMessageText           */
                           INPUT "ERR":U).                            /* ipcMessageType           */
  END.  // IF NOT AVAILABLE btt_auth_flag_penalty THEN

  /*
    Get the auth rule code for the Late Auth Flag
  */
  ASSIGN cRuleType   = "ma_acAuthRuleTypeAuthFlag":U
         cRuleCode   = "LATEAUTH":U
         dStartDate  = btt_auth_provider.start_date
         dInsurerObj = goAuthorisation:InsurerObj
         iOptionCode = goAuthorisation:MemberOptionCode
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
  FIND FIRST btt_auth_flag_lateauth
    WHERE btt_auth_flag_lateauth.owning_entity_mnemonic = "hatau":U
    AND   btt_auth_flag_lateauth.owning_obj             = btt_auth_provider.auth_obj
    AND   btt_auth_flag_lateauth.owning_key             = "":U
    AND   btt_auth_flag_lateauth.auth_rule_obj          = dAuthRuleObj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE btt_auth_flag_lateauth THEN
  DO:
    ASSIGN cMessage = "Lateauth Flag must be captured if penalty is activated on the Authorisation Type."
                    + "[HELP=Auth Rule Code: LateAuth]".
    goErrorObject:addError(INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                           INPUT "":U,                                /* ipcOwningEntityKey       */
                           INPUT "doc_num":U,                         /* ipcFieldName             */
                           INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                           INPUT cMessage,                            /* ipcMessageText           */
                           INPUT "ERR":U).                            /* ipcMessageType           */
  END.  // IF NOT AVAILABLE btt_auth_flag_lateauth THEN

  ASSIGN lRunCheckForPenalty = NO.

  IF btt_auth_provider.auth_provider_obj <= 0
  OR  (AVAILABLE buf_auth_provider 
  AND (buf_auth_provider.start_date <> btt_auth_provider.start_date
  OR   buf_auth_provider.doc_num    <> btt_auth_provider.doc_num
  OR   buf_auth_provider.pr_type    <> btt_auth_provider.pr_type))
  OR   btt_auth_provider._request_date_updated = TRUE
  THEN 
    ASSIGN cPenaltyFlag        = IF btt_auth_provider.auth_status = 1
                                 THEN ""
                                 ELSE "NO":U  // Provider was declined while adding e.g. non-dsp provider
           cLateAuthFlag       = ""
           lRunCheckForPenalty = YES.
  ELSE IF AVAILABLE buf_auth_provider THEN
  DO:
    /*
       If the provider authorisation status is updated and it is not authorised
       after a penalty is applied (PENALTY = "YES"), the penalty should no longer apply.
    */
    IF  buf_auth_provider.auth_status <> btt_auth_provider.auth_status
    AND btt_auth_provider.auth_status <> 1 /* Authorised */
    THEN 
      ASSIGN cPenaltyFlag        = "NO":U
             cLateAuthFlag       = btt_auth_flag_lateauth.auth_flag_value
             lRunCheckForPenalty = YES.
    ELSE IF btt_auth_provider._emergency_flag_updated
         OR btt_auth_provider._penalty_flag_updated
         OR buf_auth_provider.auth_status <> btt_auth_provider.auth_status THEN // Auth was declined but user changed it to authorised
    DO:
      IF btt_auth_provider._penalty_flag_updated
      THEN
        ASSIGN cPenaltyFlag        = btt_auth_provider._penalty_flag
               cLateAuthFlag       = btt_auth_flag_lateauth.auth_flag_value
               lRunCheckForPenalty = YES.
      ELSE 
        ASSIGN cPenaltyFlag        = ""
               cLateAuthFlag       = btt_auth_flag_lateauth.auth_flag_value
               lRunCheckForPenalty = YES.
    END.  // IF btt_auth_provider._emergency_flag_updated OR ...
  END.  // ELSE IF AVAILABLE buf_auth_provider THEN
  
  IF lRunCheckForPenalty = YES THEN
  DO:
    ASSIGN cErrorMessage    = ""
           cTrackingMessage = "DebugAuthPenalty-INPUT-"  +
                                      "ipdInsurerObj="   + (IF goAuthorisation:InsurerObj <> ?        THEN STRING(goAuthorisation:InsurerObj)        ELSE "?") +
                                      ";ipiOptionCode="  + (IF goAuthorisation:MemberOptionCode <> ?  THEN STRING(goAuthorisation:MemberOptionCode)  ELSE "?") +
                                      ";ipdStartDate="   + STRING(btt_auth_provider.start_date,"9999/99/99")                                                   +
                                      ";ipdRequestDate=" + STRING(goAuthorisation:RequestDate,"9999/99/99")                                                    +
                                      ";ipcEmergency="   + (IF btt_auth_provider._emergency_flag <> ? THEN STRING(btt_auth_provider._emergency_flag) ELSE "?") +
                                      ";ipcPenalty="     + (IF cPenaltyFlag <> ?                      THEN cPenaltyFlag                              ELSE "?") +
                                      ";ipcLateAuth="    + (IF cLateAuthFlag <> ?                     THEN cLateAuthFlag                             ELSE "?") +
                                      ";ipiPrType="      + STRING(btt_auth_provider.pr_type,"999").

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    mipEnv:Health:AuthService:checkForPenalty (INPUT  goAuthorisation:InsurerObj,         // ipdInsurerObj           - Client.
                                               INPUT  goAuthorisation:MemberOptionCode,   // ipiOptionCode           - Member option code.
                                               INPUT  btt_auth_provider.start_date,       // ipdStartDate            - Date for which setups must be checked.
                                               INPUT  goAuthorisation:RequestDate,        // ipdRequestDate          - Date member requested the authorisation from the scheme.
                                               INPUT  btt_auth_provider._emergency_flag,  // ipcEmergency            - Indicate if it is an emergency.
                                               INPUT  cPenaltyFlag,                       // ipcPenalty              - Indicate if a penalty must apply.
                                               INPUT  cLateAuthFlag,                      // ipcLateAuth             - Indicate if a penalty must apply.
                                               INPUT  btt_auth_provider.pr_type,          // ipiPrType               - Provider disciplines exceptions,
                                               OUTPUT cPenaltyApplies,                    // opcPenaltyApplies       - Indicate if a penalty must apply.
                                               OUTPUT cLateAuthApplies,                   // opcLateAuthApplies      - Indicate if it is a late auth.
                                               OUTPUT cWarning,                           // opcWarning              - Indicate if a warning must be populated.
                                               OUTPUT cWarningType,                       // opcWarningType          - Indicate warning type.
                                               OUTPUT dPenaltyCopayTypeObj,               // opdPenaltyCopayTypeObj  - Co-payment type that must apply for a penalty.
                                               OUTPUT cPenaltyValueType,                  // opcPenaltyValueType     - Indicate if a rand value or percentage must apply.
                                               OUTPUT dPenaltyValue,                      // opdPenaltyValue         - Penalty value that must apply.
                                               OUTPUT cErrorMessage,                      // opcPenaltyError         - Error that will be returned if any setups are missing.
                                               OUTPUT lClearPenaltyReason).

    ASSIGN cTrackingMessage = "DebugAuthPenalty-OUTPUT-"          +
                                      "opcPenaltyApplies="        + (IF cPenaltyApplies <> ?      THEN cPenaltyApplies              ELSE "?") +
                                      ";opcLateAuthApplies="      + (IF cLateAuthApplies <> ?     THEN cLateAuthApplies             ELSE "?") +
                                      ";opcWarning="              + cWarning                                                                  +
                                      ";opcWarningType="          + cWarningType                                                              +
                                      ";opdPenaltyCopayTypeObj="  + (IF dPenaltyCopayTypeObj <> ? THEN STRING(dPenaltyCopayTypeObj) ELSE "?") +
                                      ";opcPenaltyValueType="     + (IF cPenaltyValueType <> ?    THEN cPenaltyValueType            ELSE "?") +
                                      ";opdPenaltyValue="         + (IF dPenaltyValue <> ?        THEN STRING(dPenaltyValue)        ELSE "?") +
                                      ";opcPenaltyError="         + cErrorMessage +
                                      ";lClearPenaltyReason="     + STRING(lClearPenaltyReason,"yes/no").

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF cErrorMessage <> "" THEN
    DO:
      goErrorObject:addError(INPUT "hatap":U,                           // ipcOwningEntityMnemonic
                             INPUT btt_auth_provider.auth_provider_obj, // ipdOwningEntityObj
                             INPUT "":U,                                // ipcOwningEntityKey
                             INPUT "doc_num":U,                         // ipcFieldName
                             INPUT btt_auth_provider.line_number,       // ipiLineNumber
                             INPUT cErrorMessage,                       // ipcMessageText
                             INPUT "ERR":U).                            // ipcMessageType
    END. // IF cErrorMessage <> "" THEN
    ELSE DO:
      /*
        If a penalty must apply - create hat_auth_copay and update hat_auth_provider with the Penalty values.
        Generate warning if the warning is setup
      */
      IF cPenaltyApplies = "YES":U THEN
      DO:
        FIND FIRST btt_auth_copay
          WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
          AND   btt_auth_copay.auth_copay_type_obj    = dPenaltyCopayTypeObj
          AND   btt_auth_copay.owning_entity_mnemonic = "hatap":U
          AND   btt_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
          AND   btt_auth_copay.owning_key             = ""
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
        IF NOT AVAILABLE btt_auth_copay THEN
        DO:
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
          ASSIGN btt_auth_copay.auth_copay_obj         = iAuthCopayObj
                 btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
                 btt_auth_copay.auth_copay_type_obj    = dPenaltyCopayTypeObj
                 btt_auth_copay.owning_entity_mnemonic = "hatap":U
                 btt_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
                 btt_auth_copay.owning_key             = ""
                 btt_auth_copay.owning_alt_value       = STRING(btt_auth_provider.doc_num,"9999999").
          VALIDATE btt_auth_copay.
        END.  // IF NOT AVAILABLE btt_auth_copay THEN
        ELSE
          ASSIGN btt_auth_provider.copay_auth   = btt_auth_provider.copay_auth   - btt_auth_copay.amount
                 btt_auth_provider.copay_auth_% = btt_auth_provider.copay_auth_% - btt_auth_copay.amount_%.

        IF cPenaltyValueType = "Amount":U
        THEN
          ASSIGN btt_auth_copay.amount          = dPenaltyValue
                 btt_auth_copay.amount_%        = 0
                 btt_auth_copay.record_action   = "MODIFY":U
                 btt_auth_provider.copay_auth   = btt_auth_provider.copay_auth + dPenaltyValue.
        ELSE
          ASSIGN btt_auth_copay.amount          = 0
                 btt_auth_copay.amount_%        = dPenaltyValue
                 btt_auth_copay.record_action   = "MODIFY":U
                 btt_auth_provider.copay_auth_% = btt_auth_provider.copay_auth_% + dPenaltyValue.
        VALIDATE btt_auth_copay.

        IF cWarningType = "WarnAck":U
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

      END.  // IF cPenaltyApplies = "YES" THEN
      ELSE IF (cPenaltyApplies       = "NO":U
          AND (dPenaltyCopayTypeObj <> 0
          AND  dPenaltyValue         = 0)) 
           OR (AVAILABLE buf_auth_provider
           AND (buf_auth_provider.doc_num   <> btt_auth_provider.doc_num
           OR   buf_auth_provider.pr_type   <> btt_auth_provider.pr_type)) THEN
      DO:
        FIND FIRST btt_auth_copay
          WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
          AND   btt_auth_copay.auth_copay_type_obj    = dPenaltyCopayTypeObj
          AND   btt_auth_copay.owning_entity_mnemonic = "hatap":U
          AND   btt_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
          AND   btt_auth_copay.owning_key             = ""
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE btt_auth_copay THEN
        DO:
          /*
            Please note penalty values must be subtracted and not cleared because the total co-payment
            values on hat_auth_provider may include a non-DSP co-payment and a penalty as well.
          */
          ASSIGN btt_auth_provider.copay_auth   = btt_auth_provider.copay_auth   - btt_auth_copay.amount
                 btt_auth_provider.copay_auth_% = btt_auth_provider.copay_auth_% - btt_auth_copay.amount_%
                 btt_auth_copay.record_action   = "DELETE":U.

        END.  // IF AVAILABLE tt_auth_copay THEN
      END.  // ELSE IF cPenaltyApplies = "NO":U AND...

      /* Update the PENALTY flag value if it is different. */
      IF cPenaltyApplies                          <> btt_auth_flag_penalty.auth_flag_value
      OR btt_auth_provider._penalty_override_note <> btt_auth_flag_penalty.override_note
      THEN 
        ASSIGN btt_auth_flag_penalty.record_action        = "MODIFY":U
               btt_auth_flag_penalty.last_change_datetime = NOW
               btt_auth_flag_penalty.auth_flag_value      = cPenaltyApplies
               btt_auth_flag_penalty.override_note        = /* We only want to assign the override note if the user changed the penalty to 'NO'.
                                                               If the penaly is 'YES', we want to clean the note, because the override note
                                                               will no longer apply.
                                                            */
                                                            IF btt_auth_flag_penalty.auth_flag_value <> "NO":U
                                                            THEN ""
                                                            ELSE btt_auth_provider._penalty_override_note.
      ELSE IF  btt_auth_provider._emergency_flag_updated = YES
           AND lClearPenaltyReason                       = YES
           AND btt_auth_provider._penalty_override_note <> ""
           AND cPenaltyApplies                           = "NO":U
           THEN 
             ASSIGN btt_auth_flag_penalty.override_note        = ""
                    btt_auth_flag_penalty.record_action        = "MODIFY":U
                    btt_auth_flag_penalty.last_change_datetime = NOW
                    btt_auth_provider._penalty_override_note   = "".
     
      /* Update the Provider flag value if it is different. */
      IF cPenaltyApplies <> btt_auth_provider._penalty_flag
      THEN 
        ASSIGN btt_auth_provider._penalty_flag = cPenaltyApplies.     
	   
      /* Update the LATEAUTH flag value if it is different. */
      IF cLateAuthApplies <> btt_auth_flag_lateauth.auth_flag_value
      THEN
        ASSIGN btt_auth_flag_lateauth.record_action        = "MODIFY":U
               btt_auth_flag_lateauth.last_change_datetime = NOW
               btt_auth_flag_lateauth.auth_flag_value      = cLateAuthApplies.
	       
    END.   // ELSE - IF cErrorMessage <> "" THEN
  END.  // IF lRunCheckForPenalty = YES THEN
END.  // IF ttAuthTypeConfig.ActivatePenalty...

