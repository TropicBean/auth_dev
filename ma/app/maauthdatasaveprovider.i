/* maauthdatasaveprovider.i MEDSTAR Medical Aid System
                            Healthcare Auth data access service: Save auth provider records
                            (c) Copyright 2017 - 2021
                            MIP Holdings (Pty) Ltd
                            All rights reserved

------------------------------------------------------------------------------
  Purpose   : Save Authorisation Provider Record    
  Parameters:
  Notes     : Data Access Only !!!      
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth_provider FOR tt_auth_provider.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.  
  
&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth          FOR hat_auth.
  DEFINE BUFFER buf_auth_provider FOR hat_auth_provider.
  DEFINE BUFFER hat_auth_provider FOR hat_auth_provider.
  
  DEFINE VARIABLE lBufferCompare          AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE iSequence               AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lNewRecordErrorMessage  AS LOGICAL           NO-UNDO INITIAL FALSE.
                                                      
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
                
  IF AVAILABLE btt_auth_provider THEN 
  DO:
    {&FindResultRecord}
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_provider.record_action) THEN 
    DO
    TRANSACTION:
      /*
        Ensure that we have a valid parent
      */
      IF btt_auth_provider.auth_obj = 0 THEN
      DO:
        oErrorObject:addError(INPUT "hatap":U,
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT btt_auth_provider.line_number,
                              INPUT "MA":U,
                              INPUT 111,  /* The &1 must be specified. &2 */
                              INPUT "Authorisation for Provider " + STRING(btt_auth_provider.doc_num) + ",":U).
      END.  /*IF btt_auth_provider.auth_obj = 0 THEN*/
      ELSE DO:
        FIND FIRST buf_auth NO-LOCK
             WHERE buf_auth.auth_obj = btt_auth_provider.auth_obj
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
        IF NOT AVAILABLE buf_auth THEN
        DO:                  
          oErrorObject:addError(INPUT "hatap":U,
                                INPUT btt_auth_provider.auth_provider_obj,
                                INPUT "":U,
                                INPUT btt_auth_provider.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation (Obj = " + STRING(btt_auth_provider.auth_obj) + ") for Provider " + STRING(btt_auth_provider.doc_num)).
        
        END. /*IF NOT AVAILABLE buf_auth THEN*/                                           
      END.  /*ELSE - IF btt_auth_provider.auth_obj = 0 THEN*/

      /*
        Duplicate check
      */ 
      IF CAN-FIND(FIRST buf_auth_provider NO-LOCK
                  WHERE buf_auth_provider.auth_obj           = btt_auth_provider.auth_obj
                    AND buf_auth_provider.group_doc_num      = btt_auth_provider.group_doc_num
                    AND buf_auth_provider.doc_num            = btt_auth_provider.doc_num
                    AND buf_auth_provider.pr_type            = btt_auth_provider.pr_type
                    AND buf_auth_provider.sub_pr_type        = btt_auth_provider.sub_pr_type
                    AND buf_auth_provider.auth_provider_obj <> btt_auth_provider.auth_provider_obj) THEN
      DO:
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj, 
                              INPUT "":U, 
                              INPUT "":U, /* Leave owning fields blank to enable line level validation */
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U, 
                              INPUT 125,  /* "&1 already exists with &2" */
                              INPUT "Authorisation Provider, ":U + CHR(10) + "Group Practice = "      + STRING(btt_auth_provider.group_doc_num) + CHR(10) + 
                                                                             "Service Provider = "    + STRING(btt_auth_provider.doc_num)       + CHR(10) +
                                                                             "Discipline code = "     + STRING(btt_auth_provider.pr_type)       + CHR(10) +
                                                                             "Sub discipline code = " + STRING(btt_auth_provider.sub_pr_type)).
      END. /*IF CAN-FIND(FIRST buf_auth_provider NO-LOCK*/
      
      IF oErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj, "":U) THEN
        RETURN.
        
      FIND FIRST buf_auth_provider EXCLUSIVE-LOCK
           WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj
        NO-ERROR NO-WAIT.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.
                     
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj, 
                              INPUT "":U, 
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U, 
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again */ 
                              INPUT "Authorisation Provider (":U + STRING(btt_auth_provider.doc_num) + ")").
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
      
      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}
        
        IF NOT oErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj,"":U)
        THEN DO:
          CREATE buf_auth_provider.
          ASSIGN lNewRecordErrorMessage = TRUE.
        END. /* IF NOT oErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj,"":U) */
          
          
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/ 
      
      IF AVAILABLE buf_auth_provider AND NOT oErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj,"":U) THEN
      DO:
        /*
          Update the tally depending on whether we are creating or updating 
        */
        IF NEW buf_auth_provider AND btt_auth_provider.provider_sequence <= 0 THEN 
        DO:
          ASSIGN tt_auth_result.records_created = tt_auth_result.records_created  + 1.
          
          FOR EACH hat_auth_provider NO-LOCK
             WHERE hat_auth_provider.auth_obj = buf_auth.auth_obj:
  
            ASSIGN iSequence = MAXIMUM(iSequence, hat_auth_provider.provider_sequence).              
          END. /*FOR EACH hat_auth_provider NO-LOCK*/
          
          ASSIGN
            iSequence                           = iSequence + 1
            btt_auth_provider.provider_sequence = iSequence
            btt_auth_provider.discount_type     = ?.
        END. /*IF NEW buf_auth_provider AND btt_auth_provider.provider_sequence <= 0 THEN*/
        ELSE 
          ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1 WHEN NOT NEW buf_auth_provider.
        
        BUFFER-COPY btt_auth_provider 
             EXCEPT btt_auth_provider.auth_provider_obj
	                  btt_auth_provider.amount_claimed
	                  btt_auth_provider.quantity_claimed
	                  btt_auth_provider.copay_paid
	                  btt_auth_provider.discount_paid
	                  btt_auth_provider.adjustment_paid
	                  btt_auth_provider.adjustment_private_paid
                 TO buf_auth_provider.          
        
        IF NEW buf_auth_provider THEN
          ASSIGN buf_auth_provider.discount_type = ?.

        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_provider.auth_provider_obj = buf_auth_provider.auth_provider_obj.
        
        VALIDATE buf_auth_provider.          
        
        FIND CURRENT buf_auth_provider NO-LOCK.
        
        IF lNewRecordErrorMessage THEN
          FOR EACH tt_auth_error EXCLUSIVE-LOCK
             WHERE tt_auth_error.owning_entity_mnemonic BEGINS "hatap"
               AND tt_auth_error.owning_obj <= 0.00:

            ASSIGN tt_auth_error.owning_obj = btt_auth_provider.auth_provider_obj.
          END. /* FOR EACH tt_auth_error EXCLUSIVE-LOCK */
                 
      END. /*IF AVAILABLE buf_auth_provider AND NOT oErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj,"":U) THEN*/    
    END. /*IF btt_auth_provider.record_action = "Modify":U THEN */
    
    IF btt_auth_provider.record_action = "Delete":U 
    THEN 
      RUN _deleteAuthProvider IN TARGET-PROCEDURE ( INPUT btt_auth_provider.auth_provider_obj, INPUT btt_auth_provider.line_number, INPUT-OUTPUT oErrorObject ).

  END. /*IF AVAILABLE btt_auth_provider THEN */
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

