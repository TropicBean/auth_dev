/* maauthbussavelimit.i  MEDSTAR Medical Aid System
                         Save Authorisation Limit Record
                         (c) Copyright 1990 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
------------------------------------------------------------------------------*/
  
DEFINE PARAMETER BUFFER btt_auth_limit FOR tt_auth_limit.
   
&IF {&DBDFMA} >= 010195 &THEN    
    
  IF AVAILABLE btt_auth_limit AND 
    CAN-DO("{&ActionList}":U, btt_auth_limit.record_action) AND NOT goErrorObject:CanFind("hatal":U, btt_auth_limit.auth_limit_obj, "":U, "ERR":U) 
  THEN
  DO:
    
    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthLimit(BUFFER btt_auth_limit, 
                                               INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, 
                                               INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).
    
    /*
      Clear record action
    */
    ASSIGN btt_auth_limit.record_action = "":U.
    
    VALIDATE btt_auth_limit.
  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_limit.record_action) THEN*/

  { mip/inc/mipcatcherror.i }

&ENDIF

  




