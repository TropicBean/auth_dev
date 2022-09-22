
/* maauthbusvalepisode.i  MEDSTAR Medical Aid System
                          Validate Auth Episode Buffer
                          (c) Copyright 1990 - 2017
                          MIP Holdings (Pty) Ltd
                          All rights reserved
*/                      
DEFINE PARAMETER BUFFER btt_auth_episode FOR tt_auth_episode.

DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.


ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).


IF AVAILABLE btt_auth_episode THEN
DO:
  
END. /*IF AVAILABLE btt_auth_episode THEN*/

{ mip/inc/mipcatcherror.i 
  &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}
                


