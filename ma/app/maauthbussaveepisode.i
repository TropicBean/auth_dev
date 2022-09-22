/* maauthbussaveepisode.i  MEDSTAR Medical Aid System
                           Save Authorisation Episode Record
                           (c) Copyright 1990 - 2018
                           MIP Holdings (Pty) Ltd
                           All rights reserved
*/                      
DEFINE VARIABLE cDefaultSequence AS CHARACTER         NO-UNDO.
DEFINE VARIABLE lFailureOccurred AS LOGICAL           NO-UNDO.
DEFINE VARIABLE lSequenceActive  AS LOGICAL           NO-UNDO.
DEFINE VARIABLE oErrorObject     AS cls.maerrorobject NO-UNDO.

DEFINE BUFFER btt_auth_episode FOR tt_auth_episode.

/*
  Default auth episode sequence
*/
ASSIGN cDefaultSequence = "ma_AuthEpisodeNumber":U
       oErrorObject     = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).  

/*
  Save Authorisation Episode Records
*/
EpisodeBlock:
FOR EACH btt_auth_episode NO-LOCK BY btt_auth_episode.auth_episode_obj DESCENDING:

  IF oErrorObject:CanFind("hatae":U, btt_auth_episode.auth_episode_obj, "":U)
  THEN NEXT EpisodeBlock.

  ASSIGN lFailureOccurred = FALSE.
  
  IF CAN-DO("{&ActionList}":U, btt_auth_episode.record_action) THEN  
  DO:
    IF btt_auth_episode.auth_episode_obj <= 0.00 THEN
    DO:
      ASSIGN lSequenceActive  = mipEnv:miUtility:isSequenceActive(INPUT cDefaultSequence)
             lFailureOccurred = (IF cDefaultSequence = "":U OR lSequenceActive = ? 
                                 THEN TRUE ELSE NOT lSequenceActive).
      IF lFailureOccurred 
      THEN 
        oErrorObject:addError
          (INPUT "hatae":U, 
           INPUT btt_auth_episode.auth_episode_obj, 
           INPUT "":U, 
           INPUT btt_auth_episode.line_number, 
           INPUT "MA":U, 
           INPUT 100,  /* The "&1" specified is invalid */
           INPUT "Sequence Key: ":U + cDefaultSequence).                          
      ELSE
        ASSIGN btt_auth_episode.episode_num = mipEnv:miUtility:getNextSequence(cDefaultSequence).                                                   
      
    END. /*IF btt_auth_episode.auth_episode_obj <= 0.00 THEN*/    
    
    /*
      No point in continuing if an error occurred 
    */
    IF lFailureOccurred
    THEN NEXT EpisodeBlock.
          
    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthEpisode(BUFFER btt_auth_episode, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).
    
    /*
      Clear record action
    */
    ASSIGN btt_auth_episode.record_action = "":U.
    
    VALIDATE btt_auth_episode.
  END. /*IF CAN-DO("{&ActionList}":U, tt_auth_episode.record_action) THEN  */
END. /*FOR EACH btt_auth_episode NO-LOCK BY btt_auth_episode.auth_episode_obj DESCENDING:*/

{ mip/inc/mipcatcherror.i 
  &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

