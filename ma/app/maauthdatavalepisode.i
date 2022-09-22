/* maauthdatavalepisode.i MEDSTAR Medical Aid System
                          Healthcare Auth data access service: Validate auth episode 
                          (c) Copyright 2017 - 2018
                          MIP Holdings (Pty) Ltd
                          All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Episode Buffer        
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack            
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 010195 &THEN              
  
  DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.
  
  DEFINE BUFFER btt_auth_episode FOR tt_auth_episode.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  FOR EACH btt_auth_episode NO-LOCK:
  
    IF CAN-DO("{&ModifyList}":U, btt_auth_episode.record_action) THEN
    DO:
      /*
        Member Validation
      */
      IF NOT CAN-FIND(FIRST memdep NO-LOCK
           WHERE memdep.mem-num = btt_auth_episode.mem_num
             AND memdep.dependant = btt_auth_episode.dependant) THEN  
        oErrorObject:addError(INPUT "hatae":U, 
                              INPUT btt_auth_episode.auth_episode_obj, 
                              INPUT "":U, 
                              INPUT "mem_num":U,
                              INPUT btt_auth_episode.line_number, 
                              INPUT "MA":U, 
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Episode member: ":U + btt_auth_episode.mem_num + "|":U + STRING(btt_auth_episode.dependant)).                  
                                
      /*
        Auth episode number validation
      */
      IF btt_auth_episode.episode_num = "":U THEN
        oErrorObject:addError(INPUT "hatae":U, 
                              INPUT btt_auth_episode.auth_episode_obj, 
                              INPUT "":U, 
                              INPUT "episode_num":U,
                              INPUT btt_auth_episode.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */
                              INPUT "Episode number,":U).

    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_episode.record_action) THEN*/
  END. /*FOR EACH btt_auth_episode NO-LOCK:*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF  

