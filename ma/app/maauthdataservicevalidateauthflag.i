/* maauthdataservicevalidateauthflag.i MEDSTAR Medical Aid System
                                       Healthcare Auth data access service: Validate auth flag values 
                                       (c) Copyright 2017
                                       MIP Holdings (Pty) Ltd
                                       All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Flag Value Buffer        
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack            
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 010195 &THEN              
  
  DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.
  
  DEFINE BUFFER btt_auth_flag_value FOR tt_auth_flag_value.
  
  /*
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error_flag_value:HANDLE).     /* use tt_auth_error ? MH */
  
  
  FOR EACH btt_auth_flag_value NO-LOCK:
  
    IF CAN-DO("{&ModifyList}":U, btt_auth_flag_value.record_action) THEN
    DO:
      /*
        Rule Code Validation - can't be blank
        Table hat_auth_flag_value links to hac_auth_rule - the flag is set up as a rule
        so....the owning_obj       on hat_auth_flag_value is the auth_rule_obj 
        and...the owning_alt_value on hat_auth_flag_value is the rule_value
      */
      FIND FIRST memdep NO-LOCK
           WHERE memdep.mem-num   = btt_auth_flag_value.mem_num
             AND memdep.dependant = btt_auth_flag_value.dependant
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF NOT AVAILABLE memdep
      THEN  
        oErrorObject:addError(INPUT "hataf":U, 
                              INPUT btt_auth_flag_value.auth_flag_value_obj, 
                              INPUT "":U, 
                              INPUT "mem_num":U,
                              INPUT btt_auth_flag_value.line_number, 
                              INPUT "MA":U, 
                              INPUT 100, 
                              INPUT "Authorisation Flag value:":U + btt_auth_flag_value.mem_num + "|":U + STRING(btt_auth_flag_value.dependant)).                  
                                
      /*
        Auth flag_value validation
      */
      IF btt_auth_flag_value.flag_value_num <> "":U THEN
      DO:
        oErrorObject:addError(INPUT "hataf":U, 
                              INPUT btt_auth_flag_value.auth_flag_value_obj, 
                              INPUT "":U, 
                              INPUT "flag_value_num":U,
                              INPUT btt_auth_flag_value.line_number, 
                              INPUT "MA":U, 
                              INPUT 100, 
                              INPUT "Authorisation Flag Value number:":U + btt_auth_flag_value.flag_value_num).                  
      END. /*IF btt_auth_flag_value.flag_value_num <> "":U THEN*/               
 
    END. /*IF btt_auth_flag_value.record_action = 'Modify' THEN*/
  END. /*FOR EACH btt_auth_flag_value NO-LOCK:*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}
  */
&ENDIF  

