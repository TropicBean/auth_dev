/* maauthbussavemcsavings.i  MEDSTAR Medical Aid System
                             Save Authorisation Savings Record
                             (c) Copyright 1990 - 2017
                             MIP Holdings (Pty) Ltd
                             All rights reserved
*/                      

  DEFINE PARAMETER BUFFER btt_auth_mc_savings FOR tt_auth_mc_savings.
   

&IF {&DBDFMA} >= 010195 &THEN    
    
  IF AVAILABLE btt_auth_mc_savings AND 
    CAN-DO("{&ActionList}":U, btt_auth_mc_savings.record_action) AND NOT goErrorObject:CanFind("hatms":U, btt_auth_mc_savings.auth_mc_savings_obj, "":U, "ERR":U) 
  THEN
  DO:
    /*
      Set update user
    */       
    ASSIGN btt_auth_mc_savings.user_id = mipEnv:miUser:UserCode.
    
    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthMCSavings(BUFFER btt_auth_mc_savings, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).
    
    /*
      Clear record action
    */
    ASSIGN btt_auth_mc_savings.record_action = "":U.
    
    VALIDATE btt_auth_mc_savings.
  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_mc_savings.record_action) THEN*/

&ENDIF

  { mip/inc/mipcatcherror.i }


