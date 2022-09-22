/* maauthdataservicesaveauthflag.i MEDSTAR Medical Aid System
                                   Healthcare Auth data access service: Save auth flag value records
                                   (c) Copyright 2017
                                   MIP Holdings (Pty) Ltd
                                   All rights reserved

------------------------------------------------------------------------------
  Purpose   : Save Authorisation Flag Value Record    
  Parameters:
  Notes     : Data Access Only !!!      
-------------------------------------------------------------------------------- */
  DEFINE PARAMETER BUFFER btt_auth_flag_value FOR tt_auth_flag_value.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE         FOR tt_auth_flag_value_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE         FOR tt_auth_flag_value_error.
                                              
  
&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth            FOR hat_auth.
  DEFINE BUFFER buf_auth_flag_value FOR hat_auth_flag_value.
  
  DEFINE VARIABLE lBufferCompare AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject   AS cls.maerrorobject NO-UNDO.
  
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_flag_value_error:HANDLE).
  
  
  IF AVAILABLE btt_auth_flag_value THEN 
  DO:
    {&FindResultRecord}
  
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_flag_value.record_action) THEN 
    DO
    TRANSACTION:
    
      FIND FIRST buf_auth_flag_value EXCLUSIVE-LOCK
           WHERE buf_auth_flag_value.auth_flag_value_obj = btt_auth_flag_value.auth_flag_value_obj
        NO-ERROR NO-WAIT.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_flag_value_result.records_locked = tt_auth_flag_value_result.records_locked + 1.
                     
        oErrorObject:addError("hataf":U, btt_auth_flag_value.auth_flag_value_obj, "":U, btt_auth_flag_value.line_number, "MA":U, 200, "Authorisation Flag Value:":U + STRING(btt_auth_flag_value.auth_flag_value_obj)).
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
      
      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}
        
        /*
          Duplicate check
        */ 
        IF CAN-FIND(FIRST buf_auth_flag_value NO-LOCK
                    WHERE buf_auth_flag_value.auth_flag_value = btt_auth_flag_value.auth_flag_value)   
        THEN
        DO:
          oErrorObject:addError(INPUT "hataf":U, 
                                INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                INPUT "":U, 
                                INPUT "auth_flag_value":U,
                                INPUT btt_auth_flag_value.line_number, 
                                INPUT "MA":U, 
                                INPUT 125, 
                                INPUT "Authorisation Flag Value,":U + STRING(btt_auth_flag_value.auth_flag_value)).
                         
        END. /*IF CAN-FIND(FIRST buf_auth_flag_value NO-LOCK*/                        
                    
        IF NOT oErrorObject:CanFind("hataf":U, btt_auth_flag_value.auth_flag_value_obj,"":U)
        THEN
          CREATE buf_auth_flag_value.
          
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/ 
      
       
      IF AVAILABLE buf_auth_flag_value AND NOT oErrorObject:CanFind("hataf":U, btt_auth_flag_value.auth_flag_value_obj,"":U) THEN
      DO:
        /*
          Update the tally depending on whether we are creating or updating 
        */
        IF NEW buf_auth_flag_value 
        THEN ASSIGN tt_auth_flag_value_result.records_created  = tt_auth_flag_value_result.records_created  + 1.
        ELSE ASSIGN tt_auth_flag_value_result.records_modified = tt_auth_flag_value_result.records_modified + 1.
        
        /*
          Check if anything has been modified or this is a create in which case we will create a history record
        */
        BUFFER-COMPARE btt_auth_flag_value
                EXCEPT btt_auth_flag_value.auth_flag_value_obj 
                    TO buf_auth_flag_value SAVE RESULT IN lBufferCompare.                 
                    
        BUFFER-COPY btt_auth_flag_value 
             EXCEPT btt_auth_flag_value.auth_flag_value_obj
                 TO buf_auth_flag_value.        
        
        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_flag_value.auth_flag_value_obj = buf_auth_flag_value.auth_flag_value_obj.
        
        VALIDATE buf_auth_flag_value.          
        
        FIND CURRENT buf_auth_flag_value NO-LOCK.
              
        /*
          Create History
        */
        IF NOT lBufferCompare THEN
        DO:
          RUN _createAuthFlagHistory IN TARGET-PROCEDURE(BUFFER buf_auth_flag_value).
        END. /*IF NOT lBufferCompare THEN*/                             
      END. /*IF NOT oErrorObject:CanFind("hataf":U, btt_auth_flag_value.auth_flag_value_obj,"":U) THEN*/    
    END. /*IF btt_auth_flag_value.record_action = "Modify":U THEN */
    
    
    IF btt_auth_flag_value.record_action = "Delete":U THEN 
    DO:
      RUN _deleteAuthFlag IN TARGET-PROCEDURE ( INPUT btt_auth_flag_value.auth_flag_value_obj, INPUT btt_auth_flag_value.line_number, INPUT-OUTPUT oErrorObject ).
    END. /*IF btt_auth_flag_value.record_action = "Delete":U THEN */
  END. /*IF AVAILABLE btt_auth_flag_value THEN */
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

