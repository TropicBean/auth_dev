/* maauthbusdetnappidefaults.i MEDSTAR Medical Aid System
                               Business Rules for the Nappi default claim code and claim type
                               (c) Copyright 1990 - 2021
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/

/*
  The processing should only be activated:
  - When a new line is added.
  - If a line is updated and the related nappi is changed.
  - If the btt_auth_detail.start_date is changed.
  - Only run the validation when default_claim_code_source is not ma_acAuthDefClaimCodeSourceCrosswalk
*/

IF (btt_auth_detail.auth_detail_obj <= 0 
    OR (AVAILABLE buf_auth_detail 
        AND (   buf_auth_detail.related_obj <> btt_auth_detail.related_obj 
             OR buf_auth_detail.start_date  <> btt_auth_detail.start_date
             )
        )
    )
AND  btt_auth_detail.default_claim_code_source <> "ma_acAuthDefClaimCodeSourceCrosswalk":U
THEN
DO:
  /*
    If nappi is captured as the related entity on a tariff code
  */
  IF  btt_auth_detail.owning_entity_mnemonic  = "htmtl":U
  AND btt_auth_detail.related_entity_mnemonic = "hlmnl":U THEN
  DO:
    /*
      Determine if tariff code defaults must be assigned from table trfcost
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                   INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                   INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                   INPUT "NappiDefaultTariffCost":U,       /* ipcRuleCode      */
                                                   INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                   OUTPUT lValidRule,                      /* oplValidRule     */
                                                   OUTPUT cRuleValue                       /* opcRuleValue     */
                                                   ).
    IF lValidRule 
    AND LOOKUP(btt_auth_detail.owning_alt_value,cRuleValue) > 0 THEN
      ASSIGN 
        btt_auth_detail.claim_code         = IF NOT btt_auth_detail._claim_code_updated  /* Claim code was not updated by the user. */
                                             THEN trfcost.claim-code
                                             ELSE btt_auth_detail.claim_code
        btt_auth_detail.claim_type         = IF NOT btt_auth_detail._claim_type_updated  /* Claim type was not updated by the user. */
                                             THEN trfcost.claim-type
                                             ELSE btt_auth_detail.claim_type
        btt_auth_detail.default_claim_code = trfcost.claim-code
        btt_auth_detail.default_claim_type = trfcost.claim-type.
    ELSE DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                     INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                     INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                     INPUT "NappiWithTariffDefaultCC":U,     /* ipcRuleCode      */
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

      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                     INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                     INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                     INPUT "NappiWithTariffDefaultCT":U,     /* ipcRuleCode      */
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
    END.  /* ELSE - IF lValidRule (NappiDefaultTariffCost) THEN */
  END.  /* IF  btt_auth_detail.owning_entity_mnemonic  = "htmtl":U AND... */
  ELSE IF btt_auth_detail.owning_entity_mnemonic = "hlmnl":U THEN
  DO:
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                   INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                   INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                   INPUT "NappiNoTariffDefaultCC":U,       /* ipcRuleCode      */
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
      END CASE.
    END.  /* IF lValidRule THEN */

    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                   INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */
                                                   INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                   INPUT "NappiNoTariffDefaultCT":U,       /* ipcRuleCode      */
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
      END CASE.
    END.  /* IF lValidRule THEN */
  END.  /* ELSE IF btt_auth_detail.owning_entity_mnemonic = "hlmnl":U THEN */
END.  /* IF btt_auth_detail.auth_detail_obj <= 0 OR... */


