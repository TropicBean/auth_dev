/* maauthbusdettariffexcl.i MEDSTAR Medical Aid System
                            Tariff exclusions business logic on auth clinical detail line
                            (c) Copyright 2021
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/

/*
  Only activate the tariff exclusion check if:
  -- A new auth detail line is added.
  -- trfcost.claim-code = buf_auth_schext.claim-code[1] and trfcost.default-note <> ""
  -- A detail line is updated and the btt_auth_detail.start_date is changed.
  -- A detail line is not already declined for another reason 
    (btt_auth_detail.auth_status_note <> trfcost.default-note).
  -- A detail line is captured for a tariff code without any related nappi or basket:
  -- A override base/ars rate has been updated or applied by the user
*/  
  ASSIGN lGeTtariffExcl = FALSE.

  /*
   A detail line is captured for a tariff code without any related nappi or basket
   Do not do the checks again on Final Save of Authorisaton - Declined line may 
   have been changed to Authorised. Running the checks again, will cause the line
   to be declined again 
  */
  IF  (btt_auth_detail.owning_entity_mnemonic   = "htmtl":U 
  AND  btt_auth_detail.related_entity_mnemonic  = "":U)  THEN
  DO:                    
    /*
      Check if a tariff item is an exclusion
      The user should be able to override an exclusion item and authorise it to allow payment.
      We need to cater for it, and we should not decline the line again if the user overrides the 
      claim code, authorisation status and status reason.
    */ 
    IF  btt_auth_detail.auth_status <> 6
    AND btt_auth_detail.claim_code  <> buf_auth_schext.claim-code[1] THEN
    DO:
      IF  (trfcost.claim-code               = buf_auth_schext.claim-code[1] 
      AND  trfcost.default-note            <> "") 
      OR   btt_auth_detail.auth_detail_obj <= 0                              // A new auth detail line is added
      OR  (AVAILABLE buf_auth_detail 
      AND  buf_auth_detail.start_date       <> btt_auth_detail.start_date)   // A detail line is updated and the btt_auth_detail.start_date is changed     
      OR  (btt_auth_detail.auth_status_note <> ""
      AND  btt_auth_detail.auth_status_note <> trfcost.default-note)          // detail line is not already declined for another reason 
      OR   lOverrideBaseARSRate
      THEN
        ASSIGN lGetTariffExcl = TRUE.

    END.  // IF  btt_auth_detail.auth_status <> 6
  END. //  IF (btt_auth_detail.owning_entity_mnemonic  = "htmtl":U ...

  IF lGetTariffExcl 
  THEN DO:
      
    ASSIGN 
      cExclusion  = "":U
      lAccept     = FALSE
      lAutoReject = FALSE.
    
    mipEnv:Health:maMedical:getTariffExclusion
                           (INPUT  goAuthorisation:InsurerObj,       // ipdInsurerObj
                            INPUT  goAuthorisation:MemberOptionCode, // ipiOptionCode
                            INPUT  trfcost.default-note,             // ipcDefaultNote
                            INPUT  btt_auth_detail.start_date,       // ipdDate
                            INPUT  no,                               // iplAssessing
                            INPUT  "":U,                             // ipcUserId
                            OUTPUT cExclusion,                       // opcExclusion
                            OUTPUT lAccept,                          // oplAccept
                            OUTPUT lAutoReject).                     // oplAutoReject
        
    /*
      If cExclusion = "", then the tariff is not an exclusion item.
      Otherwise we need to check the 'tariffExclusion' rule and handle it as 
      per the rule setup.
    */
    IF cExclusion  <> "":U THEN
    DO:  
      RUN _checktariffExclRule(BUFFER btt_auth_detail
                              ,BUFFER buf_auth_schext   
                              ,INPUT cExclusion
                              ,INPUT trfcost.default-note).
  
    END. // IF cExclusion <> "" 
  END. // IF lGeTtariffExcl <> ""




