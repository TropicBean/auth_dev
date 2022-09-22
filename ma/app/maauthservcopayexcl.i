/* maauthservcopayexcl.i MEDSTAR Medical Aid System
              Check for Co-payment exclusions
              (c) Copyright 2021
              MIP Holdings (Pty) Ltd
              All rights reserved                
*/

/* 
  Note that multiple hac_auth_copay_detail records may exist 
  for the same owning entity with different exclusions. 
*/
IF CAN-FIND(FIRST hac_auth_copay_detail 
            WHERE hac_auth_copay_detail.auth_copay_control_obj = hac_auth_copay_control.auth_copay_control_obj
            AND   hac_auth_copay_detail.owning_entity_mnemonic = {&oem}
            AND   hac_auth_copay_detail.owning_obj             = {&oobj}
            AND   hac_auth_copay_detail.owning_key             = {&okey}
            AND   hac_auth_copay_detail.effective_date        <= ipdDate
            AND  (hac_auth_copay_detail.end_date              >= ipdDate
            OR    hac_auth_copay_detail.end_date               = ?)) THEN
DO:
  ASSIGN oplCopayProvider = YES.

  {&BLKNAME}:
  FOR EACH hac_auth_copay_detail NO-LOCK 
    WHERE  hac_auth_copay_detail.auth_copay_control_obj = hac_auth_copay_control.auth_copay_control_obj
    AND    hac_auth_copay_detail.owning_entity_mnemonic = {&oem}
    AND    hac_auth_copay_detail.owning_obj             = {&oobj}
    AND    hac_auth_copay_detail.owning_key             = {&okey}
    AND    hac_auth_copay_detail.effective_date        <= ipdDate
    AND   (hac_auth_copay_detail.end_date              >= ipdDate
    OR     hac_auth_copay_detail.end_date               = ?):

    /* If no exclusion is setup, the co-payment will apply. */
    IF hac_auth_copay_detail.exclusion_entity_mnemonic = "" THEN 
    DO:
      LEAVE {&BLKNAME}.
    END.  // IF hac_auth_copay_detail.exclusion_entity_mnemonic = ""
  
&IF DEFINED(DocNum) &THEN
    /* If exclustion applies for the provider number. */
    IF  hac_auth_copay_detail.exclusion_entity_mnemonic = "doctor":U
    AND hac_auth_copay_detail.exclusion_alt_value       = STRING({&DocNum}) THEN 
    DO:
      {&exclapply}
    END.  // IF  hac_auth_copay_detail.exclusion_entity_mnemonic = "doctor":U
&ENDIF

&IF DEFINED(PrType) &THEN
    /* If exclusion applies for the provider discipline. */
    IF  hac_auth_copay_detail.exclusion_entity_mnemonic = "prtype":U
    AND hac_auth_copay_detail.exclusion_alt_value       = STRING({&PrType}) THEN
    DO:
      ASSIGN cTrackingMessage = "Exclusion applies for the provider discipline (" + STRING({&PrType}) + ")".
      {&exclapply}
    END.  // IF  hac_auth_copay_detail.exclusion_entity_mnemonic =  "prtype":U
&ENDIF

&IF DEFINED(DocNegNum) &THEN
    /* If exclusion apply for the provider negotiation group.  */
    IF  hac_auth_copay_detail.exclusion_entity_mnemonic = "neggroup":U
    AND hac_auth_copay_detail.exclusion_alt_value       = STRING({&DocNegNum}) THEN
    DO:
      ASSIGN cTrackingMessage = "Exclusion applies for the provider negotiation group (" + STRING({&DocNegNum}) + ")".
      {&exclapply}
    END.  // IF hac_auth_copay_detail.exclusion_entity_mnemonic = "neggroup":U
&ENDIF
      
&IF DEFINED(WorkGroupObjList) &THEN
    /* If exclusion applies for the provider workgroup. */
    IF  hac_auth_copay_detail.exclusion_entity_mnemonic = "hdmwg":U
    AND {&WorkgroupObjList} <> ""
    AND LOOKUP(STRING(hac_auth_copay_detail.exclusion_obj),{&WorkgroupObjList}) > 0 THEN
    DO:
      ASSIGN cTrackingMessage = "Exclusion applies for the provider workgroup (" + STRING(hac_auth_copay_detail.exclusion_obj) + ")".
      {&exclapply}
    END.  // IF  hac_auth_copay_detail.exclusion_entity_mnemonic = "hdmwg":U
&ENDIF

  END.  // {&BLKNAME}: FOR EACH hac_auth_copay_detail NO-LOCK

END.  // IF CAN-FIND(FIRST hac_auth_copay_detail 

