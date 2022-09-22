/* maauthbussavecopay.i  MEDSTAR Medical Aid System
                         Save Authorisation Co-payment Record
                         (c) Copyright 1990 - 2017
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/                      

  DEFINE PARAMETER BUFFER btt_auth_copay FOR tt_auth_copay.
   

&IF {&DBDFMA} >= 010195 &THEN    
    
  IF AVAILABLE btt_auth_copay AND 
    CAN-DO("{&ActionList}":U, btt_auth_copay.record_action) AND NOT goErrorObject:CanFind("hatcp":U, btt_auth_copay.auth_copay_obj, "":U, "ERR":U) 
  THEN
  DO:
    
    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthCopay(BUFFER btt_auth_copay, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).
    
    /*
      Clear record action
    */
    ASSIGN btt_auth_copay.record_action = "":U.
    
    VALIDATE btt_auth_copay.
  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_copay.record_action) THEN*/

  { mip/inc/mipcatcherror.i }

&ENDIF

  



