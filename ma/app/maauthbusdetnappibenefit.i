/* maauthbusdetnappibenefit.i MEDSTAR Medical Aid System
                              Nappi benefit business logic on auth clinical detail line
                              (c) Copyright 2020 - 2021
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/

ASSIGN lNappiBenefits = FALSE.
/*
  The processing should only be activated:
  - When a new line is added.
  - If a line is updated and the related nappi is changed.
  - If the btt_auth_detail.start_date is changed.
  - Only run the validation when default_claim_code_source is not  ma_acAuthDefClaimCodeSourceCrosswalk
*/  
IF  btt_auth_detail.auth_status               <> 6 
AND btt_auth_detail.default_claim_code_source <> "ma_acAuthDefClaimCodeSourceCrosswalk":U THEN
DO:
  IF btt_auth_detail.auth_detail_obj <= 0 
  OR  (AVAILABLE buf_auth_detail 
  AND (buf_auth_detail.related_obj <> btt_auth_detail.related_obj
  OR   buf_auth_detail.start_date  <> btt_auth_detail.start_date)) THEN
  DO:
    /*
      Nappi is captured as related entity
    */
    IF (btt_auth_detail.owning_entity_mnemonic  = "htmtl":U 
    AND btt_auth_detail.related_entity_mnemonic = "hlmnl":U) 
    THEN
      ASSIGN cNappiCode = btt_auth_detail.related_value.
    
    /* 
      Nappi is captured as owning entity without a tariff code. 
    */
    IF  btt_auth_detail.owning_entity_mnemonic  = "hlmnl":U 
    THEN 
      ASSIGN cNappiCode = btt_auth_detail.owning_alt_value.
    
    /*
      Determine the prefix
    */
    IF LENGTH(cNappiCode) = vi-max-nappi-length
    OR LENGTH(cNappiCode) = vi-max-nappi-prefix
    THEN
      ASSIGN iNappiCodePrefix = INTEGER(SUBSTRING(cNappiCode,1, vi-max-nappi-prefix)).
    ELSE
      ASSIGN iNappiCodePrefix = INTEGER(SUBSTRING(cNappiCode,1, vi-min-nappi-prefix)).

    {ma/msc/manappibenefitread.i 
            &nappibenefit    = hlm_nappi_benefit
            &nappicodeprefix = iNappiCodePrefix
            &insurer         = goAuthorisation:InsurerObj
            &scheme          = goAuthorisation:OptionCode
            &date            = btt_auth_detail.start_date
            &lock1           = NO-LOCK}
              
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE hlm_nappi_benefit 
    THEN 
      ASSIGN 
        btt_auth_detail.claim_code         = IF NOT btt_auth_detail._claim_code_updated
                                             THEN hlm_nappi_benefit.claim_code
                                             ELSE btt_auth_detail.claim_code
        btt_auth_detail.claim_type         = IF NOT btt_auth_detail._claim_type_updated
                                             THEN hlm_nappi_benefit.claim_type
                                             ELSE btt_auth_detail.claim_type
        btt_auth_detail.default_claim_code = hlm_nappi_benefit.claim_code
        btt_auth_detail.default_claim_type = hlm_nappi_benefit.claim_type
        lNappiBenefits                     = TRUE.

  END.  // ELSE - IF btt_auth_detail.auth_detail_obj <= 0 THEN
END.  // IF  btt_auth_detail.auth_status <> 6
  

