/* maauthdatasavecopay.i MEDSTAR Medical Aid System
                         Healthcare Auth data access service: Save Authorisation Co-payment
                         (c) Copyright 2017 - 2018
                         MIP Holdings (Pty) Ltd
                         All rights reserved

------------------------------------------------------------------------------
  Purpose   : Save Authorisation Co-payment Record    
  Parameters:
  Notes     : Data Access Only !!!      
-------------------------------------------------------------------------------- */
  DEFINE PARAMETER BUFFER btt_auth_copay FOR tt_auth_copay.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.
  
&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth         FOR hat_auth.
  DEFINE BUFFER buf_auth_copay   FOR hat_auth_copay.
  
  DEFINE VARIABLE lBufferCompare AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject   AS cls.maerrorobject NO-UNDO.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  IF AVAILABLE btt_auth_copay THEN
  DO:
    {&FindResultRecord}
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_copay.record_action) THEN 
    DO
    TRANSACTION:
      /*
        Ensure that we have a valid parent
      */
      IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
        WHERE buf_auth.auth_obj = btt_auth_copay.auth_obj) THEN 
      DO:
        oErrorObject:addError(INPUT "hatcp":U,
                              INPUT btt_auth_copay.auth_copay_obj,
                              INPUT "":U,
                              INPUT btt_auth_copay.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* 	The "&1" specified is invalid */
                              INPUT "Authorisation Co-payment Parent record (Obj = ":U + STRING(btt_auth_copay.auth_obj) + ")").
      
      END. /*IF NOT CAN-FIND(FIRST buf_auth NO-LOCK*/
      
      /*
        Duplicate check
      */ 
      IF CAN-FIND(FIRST buf_auth_copay NO-LOCK
                  WHERE buf_auth_copay.auth_obj               = btt_auth_copay.auth_obj
                    AND buf_auth_copay.auth_copay_type_obj    = btt_auth_copay.auth_copay_type_obj
                    AND buf_auth_copay.owning_entity_mnemonic = btt_auth_copay.owning_entity_mnemonic
                    AND buf_auth_copay.owning_obj             = btt_auth_copay.owning_obj
                    AND buf_auth_copay.owning_key             = btt_auth_copay.owning_key
                    AND buf_auth_copay.auth_copay_obj        <> btt_auth_copay.auth_copay_obj) THEN
      DO:
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj, 
                              INPUT "":U, 
                              INPUT "auth_copay_obj":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U, 
                              INPUT 125, /* &1 already exists with &2 */
                              INPUT "Authorisation Co-payment,":U).
      END. /*IF CAN-FIND(FIRST buf_auth_copay NO-LOCK*/
      
      IF oErrorObject:CanFind("hatcp":U, btt_auth_copay.auth_copay_obj, "":U) 
      THEN
        RETURN.
        
      FIND FIRST buf_auth_copay EXCLUSIVE-LOCK
           WHERE buf_auth_copay.auth_copay_obj = btt_auth_copay.auth_copay_obj
        NO-ERROR NO-WAIT.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.
                     
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj, 
                              INPUT "":U, 
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U, 
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again */
                              INPUT "Authorisation Co-payment":U).
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
      
      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}
        
        IF NOT oErrorObject:CanFind("hatcp":U, btt_auth_copay.auth_copay_obj,"":U) 
        THEN
          CREATE buf_auth_copay.
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/ 
      
      IF AVAILABLE buf_auth_copay 
      AND NOT oErrorObject:CanFind("hatcp":U, btt_auth_copay.auth_copay_obj,"":U) THEN
      DO:
        /*
          Update the tally depending on whether we are creating or updating 
        */
        IF NEW buf_auth_copay 
        THEN 
          ASSIGN tt_auth_result.records_created  = tt_auth_result.records_created  + 1.
        ELSE 
          ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1.
        
        BUFFER-COPY btt_auth_copay 
             EXCEPT btt_auth_copay.auth_copay_obj
                 TO buf_auth_copay.        
        
        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_copay.auth_copay_obj = buf_auth_copay.auth_copay_obj.
        
        VALIDATE buf_auth_copay.          
        
        FIND CURRENT buf_auth_copay NO-LOCK.                      

      END. /*IF AVAILABLE buf_auth_copay AND NOT oErrorObject:CanFind("hatcp":U, btt_auth_copay.auth_copay_obj,"":U) THEN*/    
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_copay.record_action) THEN*/
    
    IF btt_auth_copay.record_action = "Delete":U 
    THEN 
      RUN _deleteAuthCopay IN TARGET-PROCEDURE ( INPUT btt_auth_copay.auth_copay_obj, INPUT btt_auth_copay.line_number, INPUT-OUTPUT oErrorObject ).

  END. /*IF AVAILABLE btt_auth_copay THEN*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject NO-ERROR."}

&ENDIF



