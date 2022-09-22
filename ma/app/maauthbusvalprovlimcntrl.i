/* maauthbusvalprovlimcntrl.i  MEDSTAR Medical Aid System
                       Validate Auth Buffer
                       (c) Copyright 2019 - 2022
                       MIP Holdings (Pty) Ltd
                       All rights reserved
*/     

  DEFINE VARIABLE lApplyAuthLimitControls   AS LOGICAL      NO-UNDO INITIAL FALSE.
  DEFINE BUFFER bbuf_auth_schext FOR schext.

  IF btt_auth_provider.auth_provider_obj > 0.00 
  AND AVAILABLE buf_auth_provider
  THEN
  DO: 
    IF  buf_auth_provider.authorised_service <> btt_auth_provider.authorised_service
    AND btt_auth_provider.authorised_service 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage = "Auth provider update, Authorised service has changed to true".

    IF NOT lApplyAuthLimitControls 
    AND (   buf_auth_provider.amount_auth   <> btt_auth_provider.amount_auth     
         OR buf_auth_provider.quantity_auth <> btt_auth_provider.quantity_auth) 
    OR (   btt_auth_provider.quantity_auth <> 0.00
         OR btt_auth_provider.amount_auth   <> 0.00) 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage        = "Auth provider update, Amount or Quantity suth is not 0 or has been changed" .

    IF NOT lApplyAuthLimitControls 
    AND buf_auth_provider.claim_code <> btt_auth_provider.claim_code 
    THEN
    DO: 
      FIND FIRST bbuf_auth_schext NO-LOCK
           WHERE bbuf_auth_schext.scheme-code = iOptionCode
          NO-ERROR.
            
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'Progress:565' &ResetIgnoredErrors = TRUE}

      IF AVAILABLE bbuf_auth_schext 
      AND btt_auth_provider.claim_code <> bbuf_auth_schext.claim-code[1] 
      THEN
        ASSIGN 
          lApplyAuthLimitControls = TRUE
          cTrackingMessage = "Auth provider update, calim code does not match scheme claim code".
          
    END. //IF btt_auth_provider.auth_provider_obj > 0.00  AND AVAILABLE buf_auth_provider

    IF NOT lApplyAuthLimitControls 
    AND btt_auth_provider.auth_status  = 1
    AND buf_auth_provider.auth_status <> 1 
    THEN
      ASSIGN lApplyAuthLimitControls = TRUE
            cTrackingMessage         = "Auth provider update, Auth status changed".

    IF NOT lApplyAuthLimitControls
    AND buf_auth_provider.start_date <> btt_auth_provider.start_date 
    THEN
      ASSIGN lApplyAuthLimitControls = TRUE
             cTrackingMessage        = "Auth provider update, Start date has been changed".

    IF NOT lApplyAuthLimitControls 
    AND AVAILABLE buf_auth_provider 
    AND (buf_auth_provider.claim_code = 99 AND btt_auth_provider.claim_code <> 99)
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage = "Auth provider update, default claim code changed from 99".

  END. //IF btt_auth_provider.auth_provider_obj > 0.00 AND AVAILABLE buf_auth_provider
  ELSE
  DO: 
    IF btt_auth_provider.authorised_service 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage        = "Auth provider autorised service is true".

    IF NOT lApplyAuthLimitControls 
    AND (btt_auth_provider.amount_auth <> 0 OR btt_auth_provider.quantity_auth <> 0) THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage        = "Auth provider amount or quantity suth is not 0".

    IF NOT lApplyAuthLimitControls THEN
    DO: 
      FIND FIRST bbuf_auth_schext NO-LOCK
           WHERE bbuf_auth_schext.scheme-code = iOptionCode
           NO-ERROR.
          
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'Progress:565' &ResetIgnoredErrors = TRUE}

      IF AVAILABLE bbuf_auth_schext 
      AND btt_auth_provider.claim_code <> bbuf_auth_schext.claim-code[1]
      THEN
        ASSIGN 
          lApplyAuthLimitControls = TRUE
          cTrackingMessage        = "Auth provider Claim code does not match scheme claim code".
    END. // IF NOT lApplyAuthLimitControls THEN

    IF NOT lApplyAuthLimitControls
    AND btt_auth_provider.auth_status = 1 
    THEN
      ASSIGN 
        lApplyAuthLimitControls = TRUE
        cTrackingMessage        = "auth provider status is approved".
  END. // ELSE

  IF lApplyAuthLimitControls THEN
  DO:
    ASSIGN cTrackingMessage = "applyAuthLimitControls is about to run for provider validation. Reason for run is: " + cTrackingMessage.
            
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    mipEnv:Health:AuthService:applyAuthLimitControls (INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                      INPUT btt_auth_provider.auth_obj,
                                                      INPUT "hatap",
                                                      INPUT btt_auth_provider.auth_provider_obj,
                                                      INPUT btt_auth_provider.amount_auth,
                                                      INPUT btt_auth_provider.quantity_auth).
  END. //IF lApplyAuthLimitControls THEN
  
