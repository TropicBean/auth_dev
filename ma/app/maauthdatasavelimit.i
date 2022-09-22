/* maauthdatasavelimit.i  MEDSTAR Medical Aid System
                          Healthcare Auth data access service: Save Auth limits
                          (c) Copyright 2017 - 2022
                          MIP Holdings (Pty) Ltd
                          All rights reserved

------------------------------------------------------------------------------
  Purpose   : Save Authorisation Limit Record    
  Parameters:
  Notes     : Data Access Only !!!      
-------------------------------------------------------------------------------- */
  DEFINE PARAMETER BUFFER btt_auth_limit FOR tt_auth_limit.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.
  
  
&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth            FOR hat_auth.
  DEFINE BUFFER buf_auth_limit      FOR hat_auth_limit.
  
  DEFINE VARIABLE lBufferCompare AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject   AS cls.maerrorobject NO-UNDO.
  
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  IF AVAILABLE btt_auth_limit THEN
  DO:
    {&FindResultRecord}
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_limit.record_action) THEN 
    DO
    TRANSACTION:
      /*
        Ensure that we have a valid parent
      */
      IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
                      WHERE buf_auth.auth_obj = btt_auth_limit.auth_obj) THEN 
      DO:
        oErrorObject:addError(INPUT "hatal":U,
                              INPUT btt_auth_limit.auth_limit_obj,
                              INPUT "":U,
                              INPUT btt_auth_limit.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* 	The "&1" specified is invalid */
                              INPUT "Authorisation Limit Parent record (Obj = ":U + STRING(btt_auth_limit.auth_obj) + ")").
      
      END. /*IF NOT CAN-FIND(FIRST buf_auth NO-LOCK*/
      
      /*
        Duplicate check
      */ 
      IF CAN-FIND(FIRST buf_auth_limit NO-LOCK
                  WHERE buf_auth_limit.auth_obj               = btt_auth_limit.auth_obj
                    AND buf_auth_limit.effective_date         = btt_auth_limit.effective_date
                    AND buf_auth_limit.claim_code             = btt_auth_limit.claim_code
                    AND buf_auth_limit.claim_type             = btt_auth_limit.claim_type
                    AND buf_auth_limit.limit_#                = btt_auth_limit.limit_#
                    AND buf_auth_limit.option_code            = btt_auth_limit.option_code
                    AND buf_auth_limit.dependant              = btt_auth_limit.dependant
                    AND buf_auth_limit.auth_limit_obj        <> btt_auth_limit.auth_limit_obj) THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "auth_limit_obj":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 125, /* &1 already exists with &2 */
                              INPUT "Authorisation Limit,":U + "Effective Date: " + STRING(btt_auth_limit.effective_date) 
                                                             + " Claim Code: " + STRING(btt_auth_limit.claim_code)
                                                             + " Claim Type: " + btt_auth_limit.claim_type
                                                             + " Limit #: " + STRING(btt_auth_limit.limit_#)
                                                             + " Option Code:" + STRING(btt_auth_limit.option_code)
                                                             + " Dependant:" + STRING(btt_auth_limit.dependant)).
      END. /*IF CAN-FIND(FIRST buf_auth_limit NO-LOCK*/

      IF oErrorObject:CanFind("hatal":U, btt_auth_limit.auth_limit_obj, "":U) THEN
        RETURN.
        
      FIND FIRST buf_auth_limit EXCLUSIVE-LOCK
           WHERE buf_auth_limit.auth_limit_obj = btt_auth_limit.auth_limit_obj
        NO-ERROR NO-WAIT.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.
                     
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again */
                              INPUT "Authorisation Limit":U).
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
      
      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}
        
        IF NOT oErrorObject:CanFind("hatal":U, btt_auth_limit.auth_limit_obj,"":U) THEN
          CREATE buf_auth_limit.
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/ 
      
      IF AVAILABLE buf_auth_limit 
      AND NOT oErrorObject:CanFind("hatal":U, btt_auth_limit.auth_limit_obj,"":U) THEN
      DO:
        /*
          Update the tally depending on whether we are creating or updating 
        */
        IF NEW buf_auth_limit 
        THEN ASSIGN tt_auth_result.records_created  = tt_auth_result.records_created  + 1.
        ELSE ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1.
        
        BUFFER-COPY btt_auth_limit 
             EXCEPT btt_auth_limit.auth_limit_obj
                 TO buf_auth_limit.        
        
        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_limit.auth_limit_obj = buf_auth_limit.auth_limit_obj.
        
        VALIDATE buf_auth_limit.          
        
        FIND CURRENT buf_auth_limit NO-LOCK.                      

      END. /*IF AVAILABLE buf_auth_limit AND NOT oErrorObject:CanFind("hatal":U, btt_auth_limit.auth_limit_obj,"":U) THEN*/    
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_limit.record_action) THEN*/
    
    IF btt_auth_limit.record_action = "Delete":U THEN 
      RUN _deleteAuthLimit IN TARGET-PROCEDURE ( INPUT btt_auth_limit.auth_limit_obj, INPUT btt_auth_limit.line_number, INPUT-OUTPUT oErrorObject ).

  END. /*IF AVAILABLE btt_auth_limit THEN*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF



