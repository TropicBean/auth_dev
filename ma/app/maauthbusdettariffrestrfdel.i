/* maauthbusdettariffrestrfdel.i MEDSTAR Medical Aid System
                                 Save Authorisation Detail Record
                                 (c) Copyright 1990 - 2021
                                 MIP Holdings (Pty) Ltd
                                  All rights reserved
*/   

/*
 Authorise the declined detail lines when the detai line causing the restriction is deleted.
*/
IF btt_auth_detail.record_action = "DELETE":U 
AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
THEN DO:
                               
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                 INPUT  btt_auth.option_code,
                                                 INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                 INPUT  "TariffRestrictionProvider":U,
                                                 INPUT  btt_auth_detail.start_date,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  IF lValidRule
  AND cRuleValue = "Activate":U 
  THEN ASSIGN lProvider = YES.
  ELSE ASSIGN lProvider = NO.

  /* build the tariff restriction disallow list*/
  FOR EACH trfdisallow NO-LOCK
     WHERE trfdisallow.tariff-code    = btt_auth_detail.owning_alt_value
       AND trfdisallow.base-rate      = cBaseRate
       AND trfdisallow.ars-rate       = cArsRate
       AND trfdisallow.effect-date   <= btt_auth_detail.start_date :

    ASSIGN cDisallowTrfList = IF cDisallowTrfList = "":U THEN trfdisallow.disallow-code
                              ELSE cDisallowTrfList + ",":U + trfdisallow.disallow-code no-error.

  END. /* FOR EACH trfdisallow NO-LOCK */

  FOR EACH trfdisallow NO-LOCK
     WHERE trfdisallow.disallow-code  = btt_auth_detail.owning_alt_value
       AND trfdisallow.base-rate      = cBaseRate
       AND trfdisallow.ars-rate       = cArsRate
       AND trfdisallow.effect-date   <= btt_auth_detail.start_date :

    ASSIGN cDisallowTrfList = IF cDisallowTrfList = "":U THEN trfdisallow.tariff-code
                              ELSE cDisallowTrfList + ",":U + trfdisallow.tariff-code no-error.

  END. /* FOR EACH trfdisallow NO-LOCK */

  /* run through the rest of the detail lines */
  IF cDisallowTrfList <> "":U THEN
  DO iCount = 1 TO NUM-ENTRIES(cDisallowTrfList):
    IF lProvider THEN 
    DO:
      FOR EACH bttt_auth_detail EXCLUSIVE-LOCK
         WHERE bttt_auth_detail.auth_provider_obj      = btt_auth_detail.auth_provider_obj
           AND bttt_auth_detail.owning_entity_mnemonic = "htmtl":U
           AND bttt_auth_detail.owning_alt_value       = ENTRY(iCount, cDisallowTrfList)
           AND bttt_auth_detail.auth_status            = 6 :
      
        ASSIGN
          bttt_auth_detail.auth_status      = 1
          bttt_auth_detail.auth_status_note = "":U
          bttt_auth_detail.reason           = "":U
          bttt_auth_detail.claim_code       = bttt_auth_detail.default_claim_code  .
      
        mipEnv:Health:AuthDataAccess:saveAuthDetail(BUFFER bttt_auth_detail, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).
      END. /* FOR EACH bttt_auth_detail EXCLUSIVE-LOCK */
    END. /* IF lProvider THEN */
    ELSE DO:
      FOR EACH bttt_auth_detail EXCLUSIVE-LOCK
         WHERE bttt_auth_detail.owning_entity_mnemonic = "htmtl":U
           AND bttt_auth_detail.owning_alt_value       = ENTRY(iCount, cDisallowTrfList)
           AND bttt_auth_detail.auth_status            = 6 :
      
        ASSIGN
          bttt_auth_detail.record_action    = "modify":U
          bttt_auth_detail.auth_status      = 1
          bttt_auth_detail.auth_status_note = "":U
          bttt_auth_detail.reason           = "":U
          bttt_auth_detail.claim_code       = bttt_auth_detail.default_claim_code  .
      
        mipEnv:Health:AuthDataAccess:saveAuthDetail(BUFFER bttt_auth_detail, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).
      END. /* FOR EACH bttt_auth_detail EXCLUSIVE-LOCK */
    END. /* *ELSE DO: */
  END. /* IF cDisallowTrfList <> "":U THEN */
END. /* IF btt_auth_detail.record_action = "DELETE":U  */
    
