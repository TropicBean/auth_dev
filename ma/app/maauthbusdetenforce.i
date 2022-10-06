/* maauthbusdetenforce.i MEDSTAR Medical Aid System
                         Healthcare Auth business logic service - Enforce fields
                         (c) Copyright 2021 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
------------------------------------------------------------------------------
  Purpose:  Include to activate validations where the capture of certain field values must be 
            enforced on the Clinical Detail Lines according to rule setups.

  Notes  :  Return an error message if applicable
            
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN

DEFINE VARIABLE cDetailEntity  AS CHARACTER NO-UNDO.
 
IF btt_auth_detail.record_action <> "DELETE":U 
THEN DO:

  ASSIGN cError = "":U.
 // ADD comment - hey this is cool  
  /* 
    Get owning entity mnemonic value 
  */
  IF btt_auth_detail.claim_code = 0
  OR btt_auth_detail.claim_type = "":U
  THEN DO:
    FIND FIRST mic_acronym NO-LOCK 
      WHERE mic_acronym.category_key  = "ma_acAuthDetailEntities":U
        AND mic_acronym.acronym_value = btt_auth_detail.owning_entity_mnemonic NO-ERROR.

    ASSIGN cDetailEntity = IF AVAILABLE mic_acronym 
                           THEN mic_acronym.acronym_label
                           ELSE "":U.

    /*
      Determine if Claim code must be enforced
    */
    IF btt_auth_detail.claim_code = 0 
    THEN DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                     INPUT  goAuthorisation:MemberOptionCode,
                                                     INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                     INPUT  "EnforceClaimCode":U,
                                                     INPUT  btt_auth_detail.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
      IF lValidRule
      AND LOOKUP(cDetailEntity,cRuleValue,",") <> 0
      THEN DO:
      	ASSIGN cError = "The Claim Code is mandatory and cannot be left zero for " + cDetailEntity + " " + btt_auth_detail.owning_alt_value +
                        ".[HELP=Auth Rule Code: EnforceClaimCode]".
      	
      	goErrorObject:addError
	                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
	                      INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
	                      INPUT "":U,                                               /* ipcOwningEntityKey      */
	                      INPUT "claim_code":U,                                     /* FieldName               */ 
	                      INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
	                      INPUT cError,                                             /* ipcMessageText          */ 
	                      INPUT "Err":U).                                           /* ErrorType               */
  
      END.  /* IF lValidRule */
    END.  /* IF btt_auth_detail.claim_code = 0 */

    /*
      Determine if Claim type must be enforced
    */
    IF btt_auth_detail.claim_type = "":U OR btt_auth_detail.claim_type = ?

    THEN DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                     INPUT  goAuthorisation:MemberOptionCode,
                                                     INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                     INPUT  "EnforceClaimType":U,
                                                     INPUT  btt_auth_detail.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
                                                     
      IF lValidRule
      AND LOOKUP(cDetailEntity,cRuleValue,",") <> 0 
      THEN DO:
      	ASSIGN cError = "The Claim Type is mandatory and cannot be left blank for " + cDetailEntity + " " + btt_auth_detail.owning_alt_value +
                        ".[HELP=Auth Rule Code: EnforceClaimType]".
      	
      	goErrorObject:addError
	                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
	                      INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
	                      INPUT "":U,                                               /* ipcOwningEntityKey      */
	                      INPUT "claim_type":U,                                     /* FieldName               */ 
	                      INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
	                      INPUT cError,                                             /* ipcMessageText          */ 
                    	      INPUT "Err":U).                                           /* ErrorType               */
       END.  /* IF lValidRule */
    END.  /* IF btt_auth_detail.claim_type = "":U */

  END.  /* IF btt_auth_detail.claim_code = 0 OR btt_auth_detail.claim_type = "":U */

END.  /* IF btt_auth_detail.record_action <> "DELETE":U */

&ENDIF   

