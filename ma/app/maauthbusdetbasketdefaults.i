/* maauthbusdetbasketdefaults.i MEDSTAR Medical Aid System
                                Business Rules for the Basket default claim code and claim type
                                (c) Copyright 1990 - 2022
                                MIP Holdings (Pty) Ltd
                                All rights reserved
*/

/*
  The processing should only be activated:
  o	When a claim code and claim type is not setup and pulled in from the Crosswalk
  o	When a new line is added for a Basket.
  o	When a line is updated and the start date on the detail line is updated (Authorisation rule setups may differ between different dates).
*/

IF btt_auth_detail.default_claim_code_source <> "ma_acAuthDefClaimCodeSourceCrosswalk":U AND 
   btt_auth_detail.owning_entity_mnemonic     = "hlmcr":U
THEN DO:
  IF  btt_auth_detail.auth_detail_obj <= 0 OR 
     (AVAILABLE buf_auth_detail           AND
      buf_auth_detail.start_date <> btt_auth_detail.start_date)
  THEN DO:
    /*
      Determine Basket Claim Code Defaults 
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                   INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                   INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                   INPUT "BasketDefaultCC":U,              /* ipcRuleCode      */
                                                   INPUT btt_auth_detail.start_date,       /* ipdEffectiveDate */
                                                   OUTPUT lValidRule,                      /* oplValidRule     */
                                                   OUTPUT cRuleValue                       /* opcRuleValue     */
                                                   ).
  
    IF lValidRule THEN
    DO:
      CASE cRuleValue:
        WHEN "Authorisation Header":U THEN
        DO:
          ASSIGN btt_auth_detail.default_claim_code = goAuthorisation:ClaimCode.
          IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
          THEN
            ASSIGN btt_auth_detail.claim_code = goAuthorisation:ClaimCode.
        END.  /* WHEN "Authorisation Header":U */
        
        WHEN "Authorisation Provider":U THEN
        DO:
          ASSIGN btt_auth_detail.default_claim_code = btt_auth_provider.claim_code.
          IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
          THEN 
            ASSIGN btt_auth_detail.claim_code = btt_auth_provider.claim_code.
        END.  /* WHEN "Authorisation Provider":U */
     
        WHEN "User Decision":U THEN
        DO:
          IF NOT btt_auth_detail._claim_code_updated AND      /* Claim code was not updated by the user. */
             btt_auth_detail.claim_code = 0 
          THEN DO:
            mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                           INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                           INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                           INPUT "BasketDefaultUserDecision":U,    /* ipcRuleCode      */
                                                           INPUT btt_auth_detail.start_date,        /* ipdEffectiveDate */
                                                           OUTPUT lValidRuleUserDecision,          /* oplValidRule     */
                                                           OUTPUT cRuleValueUserDecision           /* opcRuleValue     */
                                                           ).  

            IF lValidRuleUserDecision THEN
            DO:
              IF ENTRY(1,cRuleValueUserDecision,"|") = "Mandatory":U
              THEN DO:
                goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,   // ipcOwningEntityMnemonic
                                       INPUT btt_auth_detail.auth_detail_obj,                       // ipdOwningEntityObj
                                       INPUT "":U,                                                  // ipcOwningEntityKey
                                       INPUT "claim_code":U,                                        // ipcFieldName      
                                       INPUT btt_auth_detail.line_number,                           // ipiLineNumber     
                                       INPUT "The Basket claim code must be completed by the User" +
                                             "[HELP=Auth Rule Code: BasketDefaultCC, BasketDefaultUserDecision]",    // ipcMessageText    
                                       INPUT "ERR":U).     
              END. /* IF ENTRY(1,cRuleValueUserDecision,"|") = "Mandatory" */
                
              IF NUM-ENTRIES(cRuleValueUserDecision,"|") > 1 AND
                 ENTRY(1,cRuleValueUserDecision,"|") = "Optional":U
              THEN DO:
                 ASSIGN cRuleValueWarn = ENTRY(2,cRuleValueUserDecision,"|"). 

                 IF LOOKUP(cRuleValueWarn,"Warn,WarnAck":U) > 0 THEN
                 DO:
                   ASSIGN cError       = "The Basket claim code must be captured by the User - please note it is not completed" +
                                         "[HELP=Auth Rule Code: BasketDefaultCC, BasketDefaultUserDecision]"
                          lAcknowledge = (IF cRuleValueWarn = "WARNACK":U THEN TRUE ELSE FALSE).
                 
                          lSuccess = goErrorObject:addError(INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                                                            INPUT btt_auth_detail.auth_detail_obj,                      /* ipdOwningEntityObj      */
                                                            INPUT "":U,                                                 /* ipcOwningEntityKey      */
                                                            INPUT "claim_code":U,                                       /* ipcFieldName            */
                                                            INPUT btt_auth_detail.line_number,                          /* ipiLineNumber           */
                                                            INPUT cError,                                               /* ipcMessageText          */
                                                            INPUT "WAR":U,                                              /* ipcMessageType          */
                                                            INPUT lAcknowledge).                                        /* ipcAcknowledge          */
                 END.  /* IF LOOKUP(cRuleValueWarn,"Warn,WarnAck":U) > 0 THEN */                                                          
              END. /* IF NUM-ENTRIES(cRuleValueUserDecision,"|") > 2  */
            END. /* IF lValidRuleUserDecision THEN */
          END. /* IF NOT btt_auth_detail._claim_code_updated AND */

        END. /* WHEN "User Decision":U THEN */
      END CASE.
    END.  /* IF lValidRule THEN */

    /*
      Determine Basket Claim Type Defaults
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                   INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                   INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                   INPUT "BasketDefaultCT":U,              /* ipcRuleCode      */
                                                   INPUT btt_auth_detail.start_date,       /* ipdEffectiveDate */
                                                   OUTPUT lValidRule,                      /* oplValidRule     */
                                                   OUTPUT cRuleValue                       /* opcRuleValue     */
                                                   ).
  
    IF lValidRule THEN
    DO:
      CASE cRuleValue:
        WHEN "Authorisation Header":U THEN
        DO:
          ASSIGN btt_auth_detail.default_claim_type = goAuthorisation:ClaimType.
          IF NOT btt_auth_detail._claim_type_updated  /* Claim code was not updated by the user. */
          THEN
            ASSIGN btt_auth_detail.claim_type = goAuthorisation:ClaimType.
        END.  /* WHEN "Authorisation Header":U */
        
        WHEN "Authorisation Provider":U THEN
        DO:
          ASSIGN btt_auth_detail.default_claim_type = btt_auth_provider.claim_type.
          IF NOT btt_auth_detail._claim_type_updated  /* Claim code was not updated by the user. */
          THEN 
            ASSIGN btt_auth_detail.claim_type = btt_auth_provider.claim_type.
        END.  /* WHEN "Authorisation Provider":U */
     
        WHEN "User Decision":U THEN
        DO:
          IF NOT btt_auth_detail._claim_type_updated AND      /* Claim code was not updated by the user. */
             btt_auth_detail.claim_type = "" 
          THEN DO:
            mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                           INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                           INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                           INPUT "BasketDefaultUserDecision":U,    /* ipcRuleCode      */
                                                           INPUT btt_auth_detail.start_date,       /* ipdEffectiveDate */
                                                           OUTPUT lValidRuleUserDecision,          /* oplValidRule     */
                                                           OUTPUT cRuleValueUserDecision           /* opcRuleValue     */
                                                           ).  

            IF lValidRuleUserDecision THEN
            DO:
              IF ENTRY(1,cRuleValueUserDecision,"|") = "Mandatory"
              THEN DO:
                goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,   // ipcOwningEntityMnemonic
                                       INPUT btt_auth_detail.auth_detail_obj,                       // ipdOwningEntityObj
                                       INPUT "":U,                                                  // ipcOwningEntityKey
                                       INPUT "claim_type":U,                                        // ipcFieldName      
                                       INPUT btt_auth_detail.line_number,                           // ipiLineNumber     
                                       INPUT "The Basket claim type must be completed by the User" +
                                             "[HELP=Auth Rule Code: BasketDefaultCT, BasketDefaultUserDecision]",    // ipcMessageText    
                                       INPUT "ERR":U).     
              END. /* IF ENTRY(1,cRuleValueUserDecision,"|") = "Mandatory" */
                
              IF NUM-ENTRIES(cRuleValueUserDecision,"|") > 1 AND
                 ENTRY(1,cRuleValueUserDecision,"|") = "Optional" 
              THEN DO:
                 ASSIGN cRuleValueWarn = ENTRY(2,cRuleValueUserDecision,"|"). 

                 IF LOOKUP(cRuleValueWarn,"Warn,WarnAck":U) > 0 THEN
                 DO:
                   ASSIGN cError       = "The Basket claim type must be captured by the User - please note it is not completed" +
                                         "[HELP=Auth Rule Code: BasketDefaultCT, BasketDefaultUserDecision]"
                          lAcknowledge = (IF cRuleValueWarn = "WARNACK":U THEN TRUE ELSE FALSE).
                 
                          lSuccess = goErrorObject:addError(INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                                                            INPUT btt_auth_detail.auth_detail_obj,                      /* ipdOwningEntityObj      */
                                                            INPUT "":U,                                                 /* ipcOwningEntityKey      */
                                                            INPUT "claim_type":U,                                       /* ipcFieldName            */
                                                            INPUT btt_auth_detail.line_number,                          /* ipiLineNumber           */
                                                            INPUT cError,                                               /* ipcMessageText          */
                                                            INPUT "WAR":U,                                              /* ipcMessageType          */
                                                            INPUT lAcknowledge).                                        /* ipcAcknowledge          */
                 END.  /* IF LOOKUP(cRuleValueWarn,"Warn,WarnAck":U) > 0 THEN */                                                          
              END. /* IF NUM-ENTRIES(cRuleValueUserDecision,"|") > 2  */
            END. /* IF lValidRuleUserDecision THEN */
          END. /* IF NOT btt_auth_detail._claim_type_updated AND */

        END. /* WHEN "User Decision":U THEN */
      END CASE.
    END.  /* IF lValidRule THEN */
  
  END. /* IF btt_auth_detail.auth_detail_obj <= 0 OR */

  IF btt_auth_detail._claim_code_updated AND /* Claim code was updated by the user. */ 
    (AVAILABLE buf_auth_detail           AND
     buf_auth_detail.claim_code <> btt_auth_detail.claim_code)
  THEN ASSIGN btt_auth_detail.default_claim_code = btt_auth_detail.claim_code.
  
  IF btt_auth_detail._claim_type_updated AND /* Claim code was updated by the user. */ 
    (AVAILABLE buf_auth_detail           AND
     buf_auth_detail.claim_type <> btt_auth_detail.claim_type)
  THEN ASSIGN btt_auth_detail.default_claim_type = btt_auth_detail.claim_type.

END. /* IF btt_auth_detail.default_claim_code_source <> "ma_acAuthDefClaimCodeSourceCrosswalk":U */
