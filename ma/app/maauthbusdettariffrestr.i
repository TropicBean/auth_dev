/* maauthbusdettariffrestr.i MEDSTAR Medical Aid System
                             Activate Tariff restriction business logic on auth clinical detail line
                             (c) Copyright  2021 - 2022
                             MIP Holdings (Pty) Ltd
                             All rights reserved
*/

/*
  Only activate the tariff restriction check if:
  - A new auth detail line is added.
  - The detail line is not declined yet / declined with applicable Tariff restriction status reason
  - A detail line is updated for a tariff without related entities
  - A detail line is updated and the btt_auth_detail.start_date is changed.
  - The override base/ars rates are updated or applied 
*/  
                             
mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                               INPUT  btt_auth.option_code,
                                               INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                               INPUT  "TariffRestrictionProvider":U,
                                               INPUT  btt_auth_detail.start_date,
                                               OUTPUT lValidRule,
                                               OUTPUT cRuleValue).

IF lValidRule
AND cRuleValue = "Activate":U 
THEN ASSIGN lProvider = YES.
ELSE ASSIGN lProvider = NO.

mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,
                                               INPUT goAuthorisation:MemberOptionCode,
                                               INPUT "ma_acAuthRuleTypeAuthDetail":U,
                                               INPUT "TariffRestriction":U,
                                               INPUT goAuthorisation:StartDate,
                                               OUTPUT lValidRule,
                                               OUTPUT cRuleValue).    

IF cRuleValue <> "" THEN 
DO:  
  IF NUM-ENTRIES(cRuleValue,"|":U) > 1
  THEN ASSIGN cRuleLoadStatus = ENTRY(2,cRuleValue,"|").
 
  IF cRuleLoadStatus <> "" 
  AND NUM-ENTRIES(cRuleLoadStatus,",":U) > 1
  THEN ASSIGN cStatus = ENTRY(1,cRuleLoadStatus,",")
              cReason = ENTRY(2,cRuleLoadStatus,",").
END.  /* IF cRuleValue <> "" THEN */

IF   btt_auth_detail.owning_entity_mnemonic  = "htmtl":U
AND  btt_auth_detail.related_entity_mnemonic = "":U
AND (btt_auth_detail.auth_status            <> 6 
 OR (btt_auth_detail.auth_status             = 6 
AND  btt_auth_detail.auth_status_note        = cReason))
 OR  lOverrideBaseARSRate THEN
DO:
  IF btt_auth_detail.record_action = "modify":U
  THEN DO:
    IF  btt_auth_detail.auth_detail_obj <= 0 
    OR (AVAILABLE buf_auth_detail
    AND (buf_auth_detail.start_date            <> btt_auth_detail.start_date
    OR buf_auth_detail.related_entity_mnemonic <> "":U))
    OR lOverrideBaseARSRate THEN
    DO:
      IF lValidRule
      AND cRuleValue <> "" THEN
      DO:
        /* Get tariff restrictions */
        mipEnv:Health:AuthService:getAuthTariffRestriction(INPUT tariff.tariff-code, 
                                                           INPUT tariff.effect-date,
                                                           INPUT tariff.base-rate,
                                                           INPUT tariff.ars-rate,
                                                           INPUT btt_auth_detail.auth_provider_obj,
                                                           INPUT-OUTPUT TABLE btt_auth_detail BY-REFERENCE,
                                                           OUTPUT cRestrictionTariffs,
                                                           OUTPUT cRestrictionMessage). 
    
        IF cRestrictionTariffs <> "":U 
        THEN DO iCount = 1 TO NUM-ENTRIES(cRestrictionTariffs):
          IF (lProvider AND CAN-FIND(FIRST buf_auth_detail NO-LOCK 
                                     WHERE buf_auth_detail.auth_obj                = btt_auth_detail.auth_obj
                                       AND buf_auth_detail.auth_provider_obj       = btt_auth_detail.auth_provider_obj
                                       AND buf_auth_detail.owning_entity_mnemonic  = "htmtl":U
                                       AND buf_auth_detail.owning_alt_value        = ENTRY(iCount,cRestrictionTariffs)
                                       AND buf_auth_detail.related_entity_mnemonic = "":U)) 
          OR (NOT lProvider AND CAN-FIND(FIRST buf_auth_detail NO-LOCK 
                                         WHERE buf_auth_detail.auth_obj                = btt_auth_detail.auth_obj
                                           AND buf_auth_detail.owning_entity_mnemonic  = "htmtl":U
                                           AND buf_auth_detail.owning_alt_value        = ENTRY(iCount,cRestrictionTariffs)
                                           AND buf_auth_detail.related_entity_mnemonic = "":U))
          THEN
            ASSIGN btt_auth_detail.reason = cRestrictionMessage + ": ":U + cRestrictionTariffs
                   lFoundTrfRestriction   = TRUE.
        END. /* IF cRestrictionTariffs <> "":U  */
        ELSE DO:
          /* Only approve line if TariffRestriction does not apply anymore */
          IF  AVAILABLE buf_auth_detail
          AND buf_auth_detail.auth_status      = 6 
          AND buf_auth_detail.auth_status_note = cReason 
          THEN
            ASSIGN btt_auth_detail.auth_status      = 1
                   btt_auth_detail.auth_status_note = "":U
                   btt_auth_detail.claim_code       = btt_auth_detail.default_claim_code.
        END.  /* ELSE - IF cRestrictionTariffs <> "":U */
      END.  /* IF lValidRule AND cRuleValue <> "" THEN */
    END.  /* IF btt_auth_detail.auth_detail_obj <= 0 */
  END. /* IF btt_auth_detail.record_action = "modify" */
END.  /* IF  btt_auth_detail.owning_entity_mnemonic = "htmtl":U */

/*
  If Tariff Restrictions were found, handle it according to the setup for rule 'TariffRestriction'
  Variable lFoundTrfRestriction will only be true if the rule was valid above and has a value
*/
IF lFoundTrfRestriction THEN
DO:
  CASE ENTRY(1,cRuleValue,"|":U):
      WHEN "BLOCK":U THEN
      DO:
        ASSIGN cError   = cRestrictionMessage + ": " + cRestrictionTariffs + "[HELP=Auth Rule Code: TariffRestriction]":U
               lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  // ipcOwningEntityMnemonic
                                       ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                       ,INPUT "":U                                                 // ipcOwningEntityKey
                                       ,INPUT "owning_entity_value":U                              // ipcFieldName     
                                       ,INPUT btt_auth_detail.line_number                          // ipiLineNumber     
                                       ,INPUT cError                                               // ipcMessageText    
                                       ,INPUT "ERR":U).                                            // ipcMessageType    
        RETURN.
      END.  /* WHEN "BLOCK":U */

      WHEN "LOAD":U THEN
      DO:
        IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN
        DO:
          /* cRuleLoadStatus assigned at the top */
          ASSIGN cError = "".

          IF cRuleLoadStatus = "" 
          THEN
            ASSIGN iAuthStatus   = 6      // Declined
                   cStatusReason = "" NO-ERROR.
          ELSE
            ASSIGN iAuthStatus   = INTEGER(cStatus)
                   cStatusReason = cReason  NO-ERROR.

          IF ERROR-STATUS:ERROR 
          THEN
            ASSIGN cError = "Invalid status (" + cRuleLoadStatus + ") specified on Auth Rule setup - please check."
                          + "[HELP=Auth Rule Code: TariffRestriction]":U.
          ELSE DO:
            ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  iAuthStatus,
                                                                               INPUT  "System":U).
            IF NOT lValidStatus 
            THEN 
              ASSIGN cError = "Invalid status (" + STRING(iAuthStatus) + ") specified on Auth Rule setup - please check."
                            + "[HELP=Auth Rule Code: TariffRestriction, ValidStatuses]":U.
          END.  /* ELSE - IF ERROR-STATUS:ERROR THEN */

          IF cError <> "" THEN
          DO:
            ASSIGN lSuccess = goErrorObject:addError
                                           (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  // ipcOwningEntityMnemonic
                                           ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                           ,INPUT "":U                                                 // ipcOwningEntityKey
                                           ,INPUT "owning_entity_value":U                              // ipcFieldName  
                                           ,INPUT btt_auth_detail.line_number                          // ipiLineNumber     
                                           ,INPUT cError                                               // ipcMessageText    
                                           ,INPUT "ERR":U).                                            // ipcMessageType 
            RETURN.
          END.  /* IF cError <> "" THEN */
          ELSE 
	          ASSIGN btt_auth_detail.auth_status      = iAuthStatus
                         btt_auth_detail.auth_status_note = cStatusReason.
        END.  /* IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN */
        ELSE 
          ASSIGN btt_auth_detail.auth_status      = 6 // Declined
                 btt_auth_detail.auth_status_note = "":U.

        ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT btt_auth_detail.auth_status,
                                                                            INPUT goAuthorisation:InsurerObj,     
                                                                            INPUT goAuthorisation:MemberOptionCode,
                                                                            INPUT btt_auth_detail.start_date).  
        IF lMandatory AND cStatusReason  = "":U 
        THEN
          ASSIGN cError = "Please specify an auth status reason on the Auth Rule setup, "
                        + "because the status reason is mandatory for auth status '" + STRING(btt_auth_detail.auth_status) 
                        + "'.[HELP=Auth Rule Code: EnforceStatusNote,TariffRestriction]":U.
          
        IF cError <> "" THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  // ipcOwningEntityMnemonic
                                         ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                         ,INPUT "":U                                                 // ipcOwningEntityKey
                                         ,INPUT "owning_entity_value":U                              // ipcFieldName  
                                         ,INPUT btt_auth_detail.line_number                          // ipiLineNumber     
                                         ,INPUT cError                                               // ipcMessageText    
                                         ,INPUT "ERR":U).  
          RETURN.
        END. /* cError <> "" */   
        
        /* Get the TariffRestrictionWarn-rule setup */
        mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                     (INPUT  goAuthorisation:InsurerObj       // ipdInsurerObj   
                                     ,INPUT  0                                // ipiOptionCode   
                                     ,INPUT  "ma_acAuthRuleTypeAuthDetail":U  // ipcRuleType     
                                     ,INPUT  "TariffRestrictionWarn":U        // ipcRuleCode     
                                     ,INPUT  goAuthorisation:StartDate        // ipdEffectiveDate
                                     ,OUTPUT lValidRule                       // oplValidRule    
                                     ,OUTPUT cRuleValue).                     // opcRuleValue    

        IF lValidRule THEN
        DO:
          IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
          DO:
            ASSIGN cStatusDesc = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U, 
                                                                                INPUT btt_auth_detail.auth_status)  
                   cError      = cRestrictionMessage + ": " + cRestrictionTariffs
                               + "'. Status is changed to '" + cStatusDesc 
                               + IF btt_auth_detail.auth_status_note <> ""
                                 THEN
                                   " - " + btt_auth_detail.auth_status_note + "'."
                                 ELSE 
                                   "'."
                   cError       = cError + "[Help=Auth Rule Code: TariffRestrictionWarn]":U
                   lAcknowledge = (IF cRuleValue = "WARNACK":U THEN TRUE ELSE FALSE).

            IF cStatusDesc = "":U 
            THEN
              ASSIGN cError   = "Attempt to change authorisation detail line status to " + STRING(btt_auth_detail.auth_status)
                              + " was unsuccessful. Status description is invalid."
                     lSuccess = goErrorObject:addError
                                             (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic // ipcOwningEntityMnemonic
                                             ,INPUT btt_auth_detail.auth_detail_obj                     // ipdOwningEntityObj
                                             ,INPUT "":U                                                // ipcOwningEntityKey
                                             ,INPUT "owning_entity_value":U                             // ipcFieldName     
                                             ,INPUT btt_auth_detail.line_number                         // ipiLineNumber     
                                             ,INPUT cError                                              // ipcMessageText    
                                             ,INPUT "ERR":U).                                           // ipcReplaceTextList      
            ELSE
              ASSIGN lSuccess = goErrorObject:addError
                                             (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic // ipcOwningEntityMnemonic
                                             ,INPUT btt_auth_detail.auth_detail_obj                     // ipdOwningEntityObj     
                                             ,INPUT "":U                                                // ipcOwningEntityKey     
                                             ,INPUT "owning_entity_value":U                             // ipcFieldName       
                                             ,INPUT btt_auth_detail.line_number                         // ipiLineNumber          
                                             ,INPUT cError                                              // ipcMessageText         
                                             ,INPUT "WAR":U                                             // ipcMessageType         
                                             ,INPUT lAcknowledge).                                      // ipcAcknowledge
          END.  /* IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN */
          ELSE
            ASSIGN lSuccess = goErrorObject:addError
                                           (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic     // ipcOwningEntityMnemonic  
                                           ,INPUT btt_auth_detail.auth_detail_obj                       // ipdOwningEntityObj       
                                           ,INPUT "":U                                                  // ipcOwningEntityKey       
                                           ,INPUT "auth_status":U                                       // ipcFieldName             
                                           ,INPUT btt_auth_detail.line_number                           // ipiLineNumber            
                                           ,INPUT "MA":U                                                // ipcMessageGroup          
                                           ,INPUT 112  /* The '&1' specified is invalid. &2 */          // ipiMessageNumber         
                                           ,INPUT "Auth Rule Value: ":U + cRuleValue + ",[HELP=Auth Rule Code: TariffRestrictionWarn]":U). // ipcReplaceTextList       
        END. /* IF lValidRule THEN */

        IF iAuthStatus = 6 
        THEN
          ASSIGN btt_auth_detail.claim_code = buf_auth_schext.claim-code[1].
	  
        VALIDATE btt_auth_detail.  

      END.  /* WHEN "LOAD":U */

      OTHERWISE DO:
        ASSIGN cError   = "Invalid Auth Rule setup." + "[HELP=Auth Rule Code: TariffRestriction]":U
               lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic       // ipcOwningEntityMnemonic
                                       ,INPUT btt_auth_detail.auth_detail_obj                           // ipdOwningEntityObj
                                       ,INPUT "":U                                                      // ipcOwningEntityKey
                                       ,INPUT "related_value":U                                         // ipcFieldName      
                                       ,INPUT btt_auth_detail.line_number                               // ipiLineNumber     
                                       ,INPUT cError                                                    // ipcMessageText    
                                       ,INPUT "ERR":U).                                                 // ipcMessageType 
      END.  /* OTHERWISE DO */
    END CASE.  /* CASE ENTRY(1,cRuleValue,"|":U) */

END.  /* IF lFoundTrfRestriction THEN */
