/* maauthbusdetlimcntrl.i  MEDSTAR Medical Aid System
                       Validate Auth Buffer
                       (c) Copyright 2019 - 2022
                       MIP Holdings (Pty) Ltd
                       All rights reserved
*/     

  DEFINE VARIABLE lApplyAuthLimitControls   AS LOGICAL    NO-UNDO INITIAL FALSE.

  IF btt_auth_detail.auth_detail_obj > 0.00 THEN
  DO:
    
    IF  (AVAILABLE buf_auth_detail AND buf_auth_detail.quantity_auth <>  btt_auth_detail.quantity_auth AND btt_auth_detail.quantity_auth <> 0 )
    OR  (AVAILABLE buf_auth_detail AND buf_auth_detail.amount_auth   <>  btt_auth_detail.amount_auth   AND btt_auth_detail.quantity_auth <> 0 )
    THEN 
      ASSIGN lApplyAuthLimitControls = TRUE 
             cTrackingMessage        = "auth detail update, auth amount/quantity update." .
  
    IF AVAILABLE buf_auth_detail 
    AND (buf_auth_detail.claim_code = 99 
    AND btt_auth_detail.claim_code <> 99) 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage        = "auth detail update, claim code has been changed from a default code of 99".

    IF lApplyAuthLimitControls 
    AND AVAILABLE buf_auth_detail
    AND buf_auth_detail.start_date <> btt_auth_detail.start_date 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage        = "auth detail update, start date has been updated".

    IF NOT lApplyAuthLimitControls 
    AND AVAILABLE buf_auth_detail 
    AND btt_auth_detail.auth_status  = 1 
    AND buf_auth_detail.auth_status <> 1 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage        = "auth detail update, auth detail status has been updated to approved".

    IF NOT lApplyAuthLimitControls
    AND AVAILABLE buf_auth_detail 
        AND (buf_auth_detail.line_restriction <> btt_auth_detail.line_restriction)
        AND (btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U          OR 
             btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount&Quantity":U OR 
             btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U) 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage = "auth detail update, line restriction has been updated and set to either ma_acAuthLineRestrictionAmount, ma_acAuthLineRestrictionAmount&Quantity or ma_acAuthLineRestrictionQuantity".

  END. // IF btt_auth_detail.auth_detail_obj > 0.00 THEN
  ELSE
  DO:
    IF NOT lApplyAuthLimitControls AND (btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U          OR 
                                        btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount&Quantity":U OR 
                                        btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U) 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage = "auth detail, line restriction has been updated and set to either ma_acAuthLineRestrictionAmount, ma_acAuthLineRestrictionAmount&Quantity or ma_acAuthLineRestrictionQuantity".


    IF lApplyAuthLimitControls AND (btt_auth_detail.amount_auth <> 0 OR btt_auth_detail.quantity_auth <> 0) THEN
    ASSIGN 
      lApplyAuthLimitControls = TRUE
      cTrackingMessage = "auth detail, either amount auth or quantity auth is not 0".
  END.

  IF lApplyAuthLimitControls THEN
  DO:
    ASSIGN cTrackingMessage = "applyAuthLimitControls is about to run for detail validation. Reason for run is: " + cTrackingMessage.
            
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    mipEnv:Health:AuthService:applyAuthLimitControls (INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                      INPUT btt_auth_detail.auth_obj,
                                                      INPUT "hatad":U,
                                                      INPUT btt_auth_detail.auth_detail_obj,
                                                      INPUT btt_auth_detail.amount_auth,
                                                      INPUT btt_auth_detail.quantity_auth).

  END. //IF lApplyAuthLimitControls THEN
 
