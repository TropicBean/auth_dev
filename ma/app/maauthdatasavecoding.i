/* maauthdatasavecoding.i MEDSTAR Medical Aid System
                          Healthcare Auth data access service: Save authorisation coding
                          (c) Copyright 2017 - 2018
                          MIP Holdings (Pty) Ltd
                          All rights reserved
------------------------------------------------------------------------------
  Purpose   : Save Authorisation Coding Record    
  Parameters:
  Notes     : Data Access Only !!!      
-----------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth_coding FOR tt_auth_coding.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.

&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth        FOR hat_auth.
  DEFINE BUFFER buf_auth_coding FOR hat_auth_coding.
  
  DEFINE VARIABLE cMessage                AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE lBufferCompare          AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lNewRecordErrorMessage  AS LOGICAL           NO-UNDO INITIAL FALSE.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  IF AVAILABLE btt_auth_coding THEN
  DO:
    {&FindResultRecord}
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_coding.record_action) THEN 
    DO
    TRANSACTION:
      /*
        Ensure that we have a valid parent
      */
      IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
                      WHERE buf_auth.auth_obj = btt_auth_coding.auth_obj) THEN
      DO:
        oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                              INPUT btt_auth_coding.auth_coding_obj,
                              INPUT "":U,
                              INPUT btt_auth_coding.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Coding Parent Authorisation (Obj = ":U + STRING(btt_auth_coding.auth_obj) + ")").
      END. /*IF NOT AVAILABLE buf_auth THEN*/
      
      /*
        Duplicate check
      */ 
      IF CAN-FIND(FIRST buf_auth_coding NO-LOCK
                  WHERE buf_auth_coding.auth_obj               = btt_auth_coding.auth_obj
                    AND buf_auth_coding.owning_entity_mnemonic = btt_auth_coding.owning_entity_mnemonic
                    AND buf_auth_coding.owning_obj             = btt_auth_coding.owning_obj
                    AND buf_auth_coding.owning_key             = btt_auth_coding.owning_key
                    AND buf_auth_coding.auth_coding_obj       <> btt_auth_coding.auth_coding_obj) THEN
      DO:
        IF btt_auth_coding.owning_entity_mnemonic = "diagnos":U THEN
          ASSIGN cMessage = "ICD":U.
        ELSE
          ASSIGN cMessage = "CPT":U.

        oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, 
                              INPUT btt_auth_coding.auth_coding_obj, 
                              INPUT "":U, 
                              INPUT "":U,   /* Leave owning fields blank to enable line level validation */
                              INPUT btt_auth_coding.line_number, 
                              INPUT "MA":U, 
                              INPUT 125,  /* &1 already exists with &2 */
                              INPUT cMessage + " Coding,":U + 
                                    cMessage + " Code: " + btt_auth_coding.owning_alt_value + 
                                    " and Start Date: ":U + STRING(btt_auth_coding.start_date,"9999/99/99")).
        
      END. /*IF CAN-FIND(FIRST buf_auth_coding NO-LOCK*/                        

      IF oErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj, "":U) 
      THEN
        RETURN.
      
      FIND FIRST buf_auth_coding EXCLUSIVE-LOCK
           WHERE buf_auth_coding.auth_coding_obj = btt_auth_coding.auth_coding_obj
        NO-ERROR NO-WAIT.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.
                     
        oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, 
                              INPUT btt_auth_coding.auth_coding_obj, 
                              INPUT "":U, 
                              INPUT btt_auth_coding.line_number, 
                              INPUT "MA":U, 
                              INPUT 200, /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Coding":U).
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
      
      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}
        
        IF NOT oErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj,"":U)
        THEN DO:
          CREATE buf_auth_coding.
          ASSIGN lNewRecordErrorMessage = TRUE.
        END. /* IF NOT oErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj,"":U) */
          
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/ 
      
      IF AVAILABLE buf_auth_coding AND NOT oErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj,"":U) THEN
      DO:
        /*
          Update the tally depending on whether we are creating or updating 
        */
        IF NEW buf_auth_coding 
        THEN ASSIGN tt_auth_result.records_created  = tt_auth_result.records_created  + 1.
        ELSE ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1.
        
        
        BUFFER-COPY btt_auth_coding 
             EXCEPT btt_auth_coding.auth_coding_obj
                 TO buf_auth_coding.        
        
        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_coding.auth_coding_obj = buf_auth_coding.auth_coding_obj.
        
        VALIDATE buf_auth_coding.          
        
        FIND CURRENT buf_auth_coding NO-LOCK.  

        IF lNewRecordErrorMessage THEN
          FOR EACH tt_auth_error EXCLUSIVE-LOCK
             WHERE tt_auth_error.owning_entity_mnemonic BEGINS "hatac"
               AND tt_auth_error.owning_obj <= 0.00:

            ASSIGN tt_auth_error.owning_obj = btt_auth_coding.auth_coding_obj.
          END. /* FOR EACH tt_auth_error EXCLUSIVE-LOCK */

      END. /*IF NOT oErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj,"":U) THEN*/    
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_coding.record_action) THEN */
    
    
    IF btt_auth_coding.record_action = "Delete":U THEN 
    DO:
      RUN _deleteAuthCoding IN TARGET-PROCEDURE ( INPUT btt_auth_coding.auth_coding_obj, INPUT btt_auth_coding.line_number, INPUT-OUTPUT oErrorObject ).
    END. /*IF btt_auth_coding.record_action = "Delete":U THEN */
  END. /*IF AVAILABLE btt_auth_coding THEN*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}
  
&ENDIF  
  


