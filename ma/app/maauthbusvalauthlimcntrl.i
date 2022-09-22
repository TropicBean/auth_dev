/* maauthbusvalauthlimcntrl.i  MEDSTAR Medical Aid System
                       Validate Auth Buffer
                       (c) Copyright 2019 - 2022
                       MIP Holdings (Pty) Ltd
                       All rights reserved
*/     
    
  DEFINE VARIABLE lApplyAuthLimitControls   AS LOGICAL      NO-UNDO INITIAL FALSE.

  IF(AVAILABLE buf_auth 
     AND (buf_auth.amount_auth   <> btt_auth.amount_auth   AND btt_auth.amount_auth   <> 0
      OR  buf_auth.quantity_auth <> btt_auth.quantity_auth AND btt_auth.quantity_auth <> 0 ))
  OR (NOT AVAILABLE buf_auth 
      AND btt_auth.record_action = "MODIFY":U )
  THEN 
    ASSIGN lApplyAuthLimitControls = TRUE .
        
        
  IF NOT lApplyAuthLimitControls 
  AND AVAILABLE buf_auth 
  AND buf_auth.auth_status <> 1 
  AND btt_auth.auth_status  = 1 
  THEN
    ASSIGN 
      lApplyAuthLimitControls = TRUE
      cTrackingMessage        = "auth header, auth staus is approved".

  IF lApplyAuthLimitControls 
  THEN
  DO:
    ASSIGN cTrackingMessage = "applyAuthLimitControls is about to run for auth header validation. Reason for run is: " + cTrackingMessage.
            
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    mipEnv:Health:AuthService:applyAuthLimitControls (INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                      INPUT btt_auth.auth_obj,
                                                      INPUT "hatau",
                                                      INPUT btt_auth.auth_obj,
                                                      INPUT btt_auth.amount_auth,
                                                      INPUT btt_auth.quantity_auth).
                                                      
  END. // IF lApplyAuthLimitControls 

