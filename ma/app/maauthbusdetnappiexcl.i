/* maauthbusdetnappiexcl.i MEDSTAR Medical Aid System
                           Nappi exclusions business logic on auth clinical detail line
                           (c) Copyright 1990 - 2020
                           MIP Holdings (Pty) Ltd
                           All rights reserved
*/

/*
  Only activate the nappi exclusion check if:
  - A new auth detail line is added.
  - A detail line is updated and the related entity is changed.
  - A detail line is updated and the btt_auth_detail.start_date is changed.
*/  

IF (btt_auth_detail.owning_entity_mnemonic  = "htmtl":U 
AND btt_auth_detail.related_entity_mnemonic = "hlmnl":U) 
 OR btt_auth_detail.owning_entity_mnemonic  = "hlmnl":U THEN
DO:
  /*
    Check if a nappi item is an exclusion
    The user should be able to override an exclusion item and authorise it to allow payment.
    We need to cater for it, and we should not decline the line again if the user overrides the 
    claim code, authorisation status and status reason.
  */
  ASSIGN lGetNappiExcl = FALSE.
  
  IF  btt_auth_detail.auth_status <> 6
  AND btt_auth_detail.claim_code  <> buf_auth_schext.claim-code[1] THEN
  DO:
    IF btt_auth_detail.auth_detail_obj <= 0 
    OR (AVAILABLE buf_auth_detail 
    AND (buf_auth_detail.related_obj <> btt_auth_detail.related_obj
    OR   buf_auth_detail.start_date  <> btt_auth_detail.start_date)) 
    THEN
      ASSIGN lGetNappiExcl = TRUE.
  END.  // IF  btt_auth_detail.auth_status <> 6
  
  IF lGetNappiExcl THEN
  DO:
    /* Nappi is captured as related entity */
    IF (btt_auth_detail.owning_entity_mnemonic  = "htmtl":U 
    AND btt_auth_detail.related_entity_mnemonic = "hlmnl":U) 
    THEN
      ASSIGN dNappiLinkObj = btt_auth_detail.related_obj.
    
    /* Nappi is captured as owning entity without a tariff code */
    IF  btt_auth_detail.owning_entity_mnemonic  = "hlmnl":U 
    THEN 
      ASSIGN dNappiLinkObj = btt_auth_detail.owning_obj.
      
    ASSIGN 
      cExclusion = "":U
      cError     = "":U
      cNote      = "":U.
    
    mipEnv:Health:maMedical:getNappiExclusion
                              (INPUT  goAuthorisation:InsurerObj,       // ipdInsurerObj
                               INPUT  goAuthorisation:MemberOptionCode, // ipiOptionCode
                               INPUT  dNappiLinkObj,                    // ipdNappiLinkObj
                               INPUT  "":U,                             // ipcNappiCodePrefix
                               INPUT  btt_auth_detail.start_date,       // ipdDate
                               OUTPUT cExclusion,                       // opcExclusion
                               OUTPUT cError,                           // opcError
                               OUTPUT cNote).                           // opcNote
        
    IF cError <> "":U THEN
    DO:
      goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                             INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                             INPUT "":U,                                                 // ipcOwningEntityKey
                             INPUT "related_value":U,                                    // ipcFieldName      
                             INPUT btt_auth_detail.line_number,                          // ipiLineNumber     
                             INPUT cError,                                               // ipcMessageText    
                             INPUT "ERR":U).                                             // ipcMessageType 
    END.  // IF cError <> "" THEN
    
    /*
      If cExclusion = "", then the nappi is not an exclusion item.
      Otherwise we need to check the 'NappiExclusion' rule and handle it as 
      per the rule setup.
    */
    ELSE IF cExclusion <> "":U THEN
      RUN _checkNappiExclRule (BUFFER btt_auth_detail, BUFFER buf_auth_schext, INPUT cExclusion, INPUT cNote).

  END.  // IF btt_auth_detail.auth_detail_obj <= 0 THEN
END.  //IF (btt_auth_detail.owning_entity_mnemonic  = "htmtl":U ...

