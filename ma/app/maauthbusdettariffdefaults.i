/* maauthbusdettariffdefaults.i MEDSTAR Medical Aid System
                                Tariff Claim Code and Claim Type defaults
                                (c) Copyright 1990 - 2021
                                MIP Holdings (Pty) Ltd
                                All rights reserved
*/

/*
  The processing should only be activated:
  - When a new line is added for a tariff where no related entity is captured.
  - When a line is updated and btt_auth_detail.start_date is changed.
  - When a line is updated and the related entity is removed 
  - When the override rates have been updated/applied on the detail line
  - Only run the validation when default_claim_code_source is not  ma_acAuthDefClaimCodeSourceCrosswalk
*/
IF btt_auth_detail.default_claim_code_source <> "ma_acAuthDefClaimCodeSourceCrosswalk":U THEN 
DO:
  IF    btt_auth_detail.owning_entity_mnemonic  = "htmtl":U
  AND ((btt_auth_detail.auth_detail_obj        <= 0 
  AND   btt_auth_detail.related_entity_mnemonic <> "hlmnl":U)
  OR   (AVAILABLE buf_auth_detail 
  AND  (btt_auth_detail.related_entity_mnemonic <> "hlmnl":U
  AND   buf_auth_detail.start_date              <> btt_auth_detail.start_date
   OR  (buf_auth_detail.related_entity_mnemonic <> btt_auth_detail.related_entity_mnemonic
  AND   btt_auth_detail.related_entity_mnemonic =  "")))) 
  OR    lOverrideBaseARSRate
  THEN DO:
    IF btt_auth_provider.main_provider 
    THEN DO:
      /*
        Determine Main Provider tariff code defaults 
      */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                     INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                     INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                     INPUT "MainProvTariffDefaultCC":U,      /* ipcRuleCode      */
                                                     INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                     OUTPUT lValidRule,                      /* oplValidRule     */
                                                     OUTPUT cRuleValue                       /* opcRuleValue     */
                                                     ).
  
      IF lValidRule THEN
      DO:
        CASE cRuleValue:
          WHEN "Authorisation Header":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_code = goAuthorisation:ClaimCode.
            IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
            THEN
              ASSIGN btt_auth_detail.claim_code = goAuthorisation:ClaimCode.
          END.  /* WHEN "Authorisation Header":U */
          
          WHEN "Authorisation Provider":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_code = btt_auth_provider.claim_code.
            IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
            THEN 
              ASSIGN btt_auth_detail.claim_code = btt_auth_provider.claim_code.
          END.  /* WHEN "Authorisation Provider":U */
       
          WHEN "Tariff Cost":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_code = trfcost.claim-code.
            IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
            THEN 
              ASSIGN btt_auth_detail.claim_code = trfcost.claim-code.
          END.  /* WHEN "Tariff Cost":U */
        END CASE.
      END.  /* IF lValidRule (NappiWithTariffDefaultCC) THEN */
  
      /*
        Determine Main Provider tariff type defaults 
      */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                     INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                     INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                     INPUT "MainProvTariffDefaultCT":U,      /* ipcRuleCode      */
                                                     INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                     OUTPUT lValidRule,                      /* oplValidRule     */
                                                     OUTPUT cRuleValue                       /* opcRuleValue     */
                                                     ).
  
      IF lValidRule THEN
      DO:
        CASE cRuleValue:
          WHEN "Authorisation Header":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_type = goAuthorisation:ClaimType.
            IF NOT btt_auth_detail._claim_type_updated  /* Claim type was not updated by the user. */
            THEN
              ASSIGN btt_auth_detail.claim_type = goAuthorisation:ClaimType.
          END.  /* WHEN "Authorisation Header":U */
          
          WHEN "Authorisation Provider":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_type = btt_auth_provider.claim_type.
            IF NOT btt_auth_detail._claim_type_updated  /* Claim type was not updated by the user. */
            THEN 
              ASSIGN btt_auth_detail.claim_type = btt_auth_provider.claim_type.
          END.  /* WHEN "Authorisation Provider":U */
       
          WHEN "Tariff Cost":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_type = trfcost.claim-type.
            IF NOT btt_auth_detail._claim_type_updated  /* Claim type was not updated by the user. */
            THEN 
              ASSIGN btt_auth_detail.claim_type = trfcost.claim-type.
          END.  /* WHEN "Tariff Cost":U */
        END CASE.
      END.  /* IF lValidRule (NappiWithTariffDefaultCC) THEN */
    END. /* IF btt_auth_provider.main_provider */
  
    IF btt_auth_provider.main_provider = FALSE AND btt_auth_provider.authorised_service = TRUE
    THEN DO:
      /*
        Check Associated Provider tariff claim code exceptions
      */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                     INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                     INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                     INPUT "AssProvTariffExceptCC":U,        /* ipcRuleCode      */
                                                     INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                     OUTPUT lValidRule,                      /* oplValidRule     */
                                                     OUTPUT cRuleValue                       /* opcRuleValue     */
                                                     ).
  
      IF lValidRule AND LOOKUP(STRING(goAuthorisation:ClaimCode,"999"),cRuleValue,",") > 0 
      THEN ASSIGN btt_auth_detail.claim_code         = IF NOT btt_auth_detail._claim_code_updated /* Claim code was not updated by the user. */
                                                       THEN goAuthorisation:ClaimCode
                                                       ELSE btt_auth_detail.claim_code
                  btt_auth_detail.default_claim_code = goAuthorisation:ClaimCode.
      ELSE DO:
        /*
          Determine Associated Provider tariff code defaults 
        */
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                       INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                       INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                       INPUT "AssProvTariffDefaultCC":U,       /* ipcRuleCode      */
                                                       INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                       OUTPUT lValidRule,                      /* oplValidRule     */
                                                       OUTPUT cRuleValue                       /* opcRuleValue     */
                                                       ).
      
        IF lValidRule THEN
        DO:
          CASE cRuleValue:
            WHEN "Authorisation Header":U THEN
            DO:
              ASSIGN btt_auth_detail.default_claim_code = goAuthorisation:ClaimCode.
              IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
              THEN
                ASSIGN btt_auth_detail.claim_code = goAuthorisation:ClaimCode.
            END.  /* WHEN "Authorisation Header":U */
            
            WHEN "Authorisation Provider":U THEN
            DO:
              ASSIGN btt_auth_detail.default_claim_code = btt_auth_provider.claim_code.
              IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
              THEN 
                ASSIGN btt_auth_detail.claim_code = btt_auth_provider.claim_code.
            END.  /* WHEN "Authorisation Provider":U */
         
            WHEN "Tariff Cost":U THEN
            DO:
              ASSIGN btt_auth_detail.default_claim_code = trfcost.claim-code.
              IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
              THEN 
                ASSIGN btt_auth_detail.claim_code = trfcost.claim-code.
            END.  /* WHEN "Tariff Cost":U */
          END CASE.
        END.  /* IF lValidRule (NappiWithTariffDefaultCC) THEN */
      END. /* ELSE - IF lValidRule AND LOOKUP(goAuthorisation:ClaimCode,cRuleValue,",") > 0 */
  
      /*
        Determine Associated Provider tariff type defaults 
      */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                     INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                     INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                     INPUT "AssProvTariffDefaultCT":U,       /* ipcRuleCode      */
                                                     INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                     OUTPUT lValidRule,                      /* oplValidRule     */
                                                     OUTPUT cRuleValue                       /* opcRuleValue     */
                                                     ).
  
      IF lValidRule THEN
      DO:
        CASE cRuleValue:
          WHEN "Authorisation Header":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_type = goAuthorisation:ClaimType.
            IF NOT btt_auth_detail._claim_type_updated  /* Claim type was not updated by the user. */
            THEN
              ASSIGN btt_auth_detail.claim_type = goAuthorisation:ClaimType.
          END.  /* WHEN "Authorisation Header":U */
          
          WHEN "Authorisation Provider":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_type = btt_auth_provider.claim_type.
            IF NOT btt_auth_detail._claim_type_updated  /* Claim type was not updated by the user. */
            THEN 
              ASSIGN btt_auth_detail.claim_type = btt_auth_provider.claim_type.
          END.  /* WHEN "Authorisation Provider":U */
       
          WHEN "Tariff Cost":U THEN
          DO:
            ASSIGN btt_auth_detail.default_claim_type = trfcost.claim-type.
            IF NOT btt_auth_detail._claim_type_updated  /* Claim type was not updated by the user. */
            THEN 
              ASSIGN btt_auth_detail.claim_type = trfcost.claim-type.
          END.  /* WHEN "Tariff Cost":U */
        END CASE.
      END.  /* IF lValidRule (NappiWithTariffDefaultCC) THEN */
    END. /* IF btt_auth_provider.main_provider = FALSE AND btt_auth_provider.authorised_service = TRUE */
  END. /* IF btt_auth_detail.auth_detail_obj <= 0 */ 
END. /*IF btt_auth_detail.default_claim_code_source <> "ma_acAuthDefClaimCodeSourceCrosswalk" THEN */ 
