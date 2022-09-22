/* maauthdatasaveepisode.i MEDSTAR Medical Aid System
                           Healthcare Auth data access service: Save auth episode records
                           (c) Copyright 2017 - 2018
                           MIP Holdings (Pty) Ltd
                           All rights reserved

------------------------------------------------------------------------------
  Purpose   : Save Authorisation Episode Record    
  Parameters:
  Notes     : Data Access Only !!!      
-------------------------------------------------------------------------------- */
  DEFINE PARAMETER BUFFER btt_auth_episode FOR tt_auth_episode.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.
  
&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth         FOR hat_auth.
  DEFINE BUFFER buf_auth_episode FOR hat_auth_episode.
  
  DEFINE VARIABLE lBufferCompare AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject   AS cls.maerrorobject NO-UNDO.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  IF AVAILABLE btt_auth_episode THEN 
  DO:
    {&FindResultRecord}
  
    IF CAN-DO("{&ModifyList}":U, btt_auth_episode.record_action) THEN 
    DO
    TRANSACTION:
    
      /*
        Duplicate check
      */ 
      IF CAN-FIND(FIRST buf_auth_episode NO-LOCK
                  WHERE buf_auth_episode.episode_num       = btt_auth_episode.episode_num
                  AND   buf_auth_episode.auth_episode_obj <> btt_auth_episode.auth_episode_obj)   
      THEN
      DO:
        oErrorObject:addError(INPUT "hatae":U, 
                              INPUT btt_auth_episode.auth_episode_obj, 
                              INPUT "":U, 
                              INPUT "episode_num":U,
                              INPUT btt_auth_episode.line_number, 
                              INPUT "MA":U, 
                              INPUT 125,  /* &1 already exists with &2 */
                              INPUT "Authorisation Episode,Episode Nr. ":U + STRING(btt_auth_episode.episode_num)).
                       
      END. /*IF CAN-FIND(FIRST buf_auth_episode NO-LOCK*/  

      IF oErrorObject:CanFind("hatae":U, btt_auth_episode.auth_episode_obj,"":U) THEN
        NEXT.
                    
      FIND FIRST buf_auth_episode EXCLUSIVE-LOCK
           WHERE buf_auth_episode.auth_episode_obj = btt_auth_episode.auth_episode_obj
        NO-ERROR NO-WAIT.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.
                     
        oErrorObject:addError(INPUT "hatae":U, 
                              INPUT btt_auth_episode.auth_episode_obj, 
                              INPUT "":U, 
                              INPUT btt_auth_episode.line_number, 
                              INPUT "MA":U, 
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Episode (Obj=":U + STRING(btt_auth_episode.auth_episode_obj) + ")").
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/

      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}
        
        IF NOT oErrorObject:CanFind("hatae":U, btt_auth_episode.auth_episode_obj,"":U)
        THEN
          CREATE buf_auth_episode.
          
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/ 
       
      IF AVAILABLE buf_auth_episode AND NOT oErrorObject:CanFind("hatae":U, btt_auth_episode.auth_episode_obj,"":U) THEN
      DO:
        /*
          Update the tally depending on whether we are creating or updating 
        */
        IF NEW buf_auth_episode 
        THEN ASSIGN tt_auth_result.records_created  = tt_auth_result.records_created  + 1.
        ELSE ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1.
        
        BUFFER-COPY btt_auth_episode 
             EXCEPT btt_auth_episode.auth_episode_obj
                 TO buf_auth_episode.        
        
        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_episode.auth_episode_obj = buf_auth_episode.auth_episode_obj.
        
        VALIDATE buf_auth_episode.          
        
        FIND CURRENT buf_auth_episode NO-LOCK.                      

      END. /*IF AVAILABLE buf_auth_episode AND NOT oErrorObject:CanFind("hatae":U, btt_auth_episode.auth_episode_obj,"":U) THEN*/    
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_episode.record_action) THEN */
    
    IF btt_auth_episode.record_action = "Delete":U 
    THEN 
      RUN _deleteAuthEpisode IN TARGET-PROCEDURE ( INPUT btt_auth_episode.auth_episode_obj, INPUT btt_auth_episode.line_number, INPUT-OUTPUT oErrorObject ).

  END. /*IF AVAILABLE btt_auth_episode THEN */
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

