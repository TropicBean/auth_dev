/* maauthbusdetnappinoncharge.i MEDSTAR Medical Aid System
                           Nappi non-charge business logic on auth clinical detail line
                           (c) Copyright 1990 - 2020
                           MIP Holdings (Pty) Ltd
                           All rights reserved
*/

/*
  Do the non-chargeable check only when the nappi is a related item,
  because the tariff (owning entity) will indicate if this nappi is
  used in the theatre or ward.
*/
IF (btt_auth_detail.owning_entity_mnemonic  = "htmtl":U 
AND btt_auth_detail.related_entity_mnemonic = "hlmnl":U) THEN
DO:
  /*
    Check if a nappi item is a non-chargeable 
    The user should be able to override a non-chargeable item and authorise it to allow payment.
    We need to cater for it, and we should not decline the line again if the user overrides the 
    claim code, authorisation status and status reason.
  */
  ASSIGN lGetNappiNonCharge = FALSE.
  
  IF  btt_auth_detail.auth_status <> 6
  AND btt_auth_detail.claim_code  <> buf_auth_schext.claim-code[1] THEN
  DO:
    IF btt_auth_detail.auth_detail_obj <= 0 THEN
      ASSIGN lGetNappiNonCharge = TRUE.
    ELSE DO:
      IF AVAILABLE buf_auth_detail 
      AND (buf_auth_detail.related_obj <> btt_auth_detail.related_obj
        OR buf_auth_detail.start_date  <> btt_auth_detail.start_date) THEN
        ASSIGN lGetNappiNonCharge = TRUE.
    END.  // ELSE - IF btt_auth_detail.auth_detail_obj <= 0 THEN
  END.  // IF  btt_auth_detail.auth_status <> 6
  
  IF lGetNappiNonCharge THEN
  DO:
    ASSIGN 
      /*
        Get the neg num from btt_auth_detail.negotiation_group and only pass in the negotiation 
        group number to getNappiNonCharge.
        The value in btt_auth_detail.negotiation_group is saved as "(" + neg_num + ":" + neggroup.name + ")". 
        Eg. "(3:MEDCLINIC)"
      */
      iNegNum        = INTEGER(LEFT-TRIM(ENTRY(1,btt_auth_detail._negotiation_group,":":U),"(":U))
      cError         = "":U
      cNonChargeType = "":U.
    
    mipEnv:Health:maMedical:getNappiNonCharge
                                   (INPUT  goAuthorisation:InsurerObj,       // Insurer Obj
                                    INPUT  btt_auth_detail.related_obj,      // ipdNappiLinkObj
                                    INPUT  "":U,                             // ipcNappiCodePrefix
                                    INPUT  btt_auth_detail.owning_alt_value, // Tariff code used with nappi
                                    INPUT  btt_auth_detail.start_date,       // ipdDate
                                    INPUT  iNegNum,                          // Provider negotiation group
                                    OUTPUT cNonChargeType,                   // Indicate where it is non chargeable - Theatre or Ward
                                    OUTPUT cError).                          // opcError
    
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
      If cNonChargeType = "", then the nappi is not a non-chargeable item.
      Otherwise we need to check either the 'NonChargeTheatre' or the
      'NonChargeWard' rule and handle the non-chargeable according to the
      applicable rule setup.
    */
    ELSE IF cNonChargeType <> "":U THEN
      RUN _checkNonChargeRule (BUFFER btt_auth_detail, BUFFER buf_auth_schext, INPUT cNonChargeType).

  END.  // IF btt_auth_detail.auth_detail_obj <= 0 THEN
END.  //IF (btt_auth_detail.owning_entity_mnemonic  = "htmtl":U ...
