/* maauthdatasaveauth.i MEDSTAR Medical Aid System
                        Healthcare Auth data access service: Save authorisation records
                        (c) Copyright 2017 - 2019
                        MIP Holdings (Pty) Ltd
                        All rights reserved
*/

  DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.

&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth  FOR hat_auth.
  
  DEFINE VARIABLE lBufferCompare  AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject    AS cls.maerrorobject NO-UNDO.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  IF AVAILABLE btt_auth THEN 
  DO:
    {&FindResultRecord}
  
    IF CAN-DO("{&ModifyList}":U, btt_auth.record_action) THEN 
    DO
    TRANSACTION:
      IF  btt_auth.auth_num  = ""
      AND btt_auth.auth_obj <= 0.00 THEN
      DO:
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj, 
                              INPUT "":U,
                              INPUT "auth_num":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */
                              INPUT "Authorisation Number,The auth number for auth obj ":U + STRING(btt_auth.auth_obj) +
                                    " is blank. Please contact your CSM.").
      END.  /* IF btt_auth.auth_num = "" THEN */

      FIND FIRST buf_auth EXCLUSIVE-LOCK
           WHERE buf_auth.auth_obj = btt_auth.auth_obj
        NO-ERROR NO-WAIT.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.
                     
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj, 
                              INPUT "":U, btt_auth.line_number, 
                              INPUT "MA":U, 
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation: ":U + STRING(btt_auth.auth_num)).
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
      
      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}
        
        IF NOT oErrorObject:CanFind("hatau":U, btt_auth.auth_obj,"":U) THEN
          CREATE buf_auth.
          
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/ 
      
      IF AVAILABLE buf_auth AND NOT oErrorObject:CanFind("hatau":U, btt_auth.auth_obj,"":U) THEN
      DO:
        
        /*
          Update the tally depending on whether we are creating or updating 
        */
        IF NEW buf_auth THEN 
        DO:
          ASSIGN tt_auth_result.records_created  = tt_auth_result.records_created  + 1.

          /*  
            Assign the discount type on the temp table to a default value
          */
          ASSIGN btt_auth.discount_type = ?.
        END.
        ELSE DO:
          ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1.
        END.
        

        IF NEW buf_auth THEN
        DO:
          BUFFER-COPY btt_auth 
               EXCEPT btt_auth.auth_obj btt_auth.option_code
                   TO buf_auth. 

        /*  
          Assign the discount type on the db table to a default value
        */
          ASSIGN buf_auth.discount_type = ?.
        END.
        ELSE DO:
          BUFFER-COPY btt_auth 
               EXCEPT btt_auth.auth_obj         btt_auth.auth_type_obj 
                      btt_auth.auth_episode_obj btt_auth.auth_date 
                      btt_auth.user_id          btt_auth.auth_num 
                      btt_auth.mem_num          
                      btt_auth.option_code
                   TO buf_auth.
        END.

        VALIDATE buf_auth.          
        
        FIND CURRENT buf_auth NO-LOCK.
        
        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        BUFFER-COPY buf_auth 
             EXCEPT buf_auth.option_code
                 TO btt_auth.
        
        VALIDATE btt_auth.
                
      END. /* IF AVAILABLE buf_auth AND NOT oErrorObject:CanFind("hatau":U, btt_auth.auth_obj,"":U) THEN */    
    END. /* IF CAN-DO("{&ModifyList}":U, btt_auth.record_action) THEN */
    
    IF btt_auth.record_action = "Delete":U THEN
      RUN _deleteAuth IN TARGET-PROCEDURE ( INPUT btt_auth.auth_obj, INPUT btt_auth.line_number, INPUT-OUTPUT oErrorObject ).

  END. /*IF AVAILABLE btt_auth THEN */
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

