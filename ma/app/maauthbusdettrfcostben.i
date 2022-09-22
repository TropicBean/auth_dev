/* maauthbusdettrfcostben.i MEDSTAR Medical Aid System
                            Activate Tariff Cost Benefit business logic on auth clinical detail line
                            (c) Copyright  2021 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/

/*
  Only activate the tariff restriction check if:
  - A new auth detail line is added for a Tariff.
  - A detail line is updated and the btt_auth_detail.start_date, claim_code or claim_type is changed.
  - The ovderride base/ars rate has been applied/updated by the user
*/  

IF btt_auth_detail.auth_detail_obj <= 0 
OR  (AVAILABLE buf_auth_detail 
AND (buf_auth_detail.start_date  <> btt_auth_detail.start_date
 OR  buf_auth_detail.claim_code  <> btt_auth_detail.claim_code
 OR  buf_auth_detail.claim_type  <> btt_auth_detail.claim_type))
OR lOverrideBaseARSRate THEN
DO:
  /*
    If detail line is a tariff code
  */
  IF  btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
  DO:
    /*
      Get the TrfcostBenCCWarn rule setup
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                   INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */ 
                                                   INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                   INPUT "TrfcostBenCCWarn":U,             /* ipcRuleCode      */
                                                   INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                   OUTPUT lValidRule,                      /* oplValidRule     */
                                                   OUTPUT cRuleValue                       /* opcRuleValue     */
                                                   ).
    ASSIGN lValidRuleWarn = lValidRule
           cRuleValueWarn = cRuleValue.

    /*
      Get the TrfcostBenCCInvalid rule setup
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,       /* ipdInsurerObj    */
                                                   INPUT goAuthorisation:MemberOptionCode, /* ipiOptionCode    */ 
                                                   INPUT "ma_acAuthRuleTypeAuthDetail":U,  /* ipcRuleType      */
                                                   INPUT "TrfcostBenCCInvalid":U,          /* ipcRuleCode      */
                                                   INPUT goAuthorisation:StartDate,        /* ipdEffectiveDate */
                                                   OUTPUT lValidRule,                      /* oplValidRule     */
                                                   OUTPUT cRuleValue                       /* opcRuleValue     */
                                                   ).
    ASSIGN lValidRuleInvalid = lValidRule
           cRuleValueInvalid = cRuleValue.

    IF lValidRuleWarn
    OR lValidRuleInvalid THEN
    DO:
      ASSIGN cClaimCodeList = "".

      FOR EACH htm_trfcost_benefit NO-LOCK
         WHERE  htm_trfcost_benefit.trfcost_obj  = dTrfcostObj
           AND (htm_trfcost_benefit.option_code	 = goAuthorisation:MemberOptionCode  
            OR  htm_trfcost_benefit.option_code  = 0)                                               
           AND (htm_trfcost_benefit.claim_code  <> 0)                                                                      
           AND (htm_trfcost_benefit.claim_type 	 = ""			
            OR  htm_trfcost_benefit.claim_type	 = btt_auth_detail.claim_type)  
           AND (btt_auth_detail.pmb_indicator 	 = NO                            
            OR (btt_auth_detail.pmb_indicator    = YES                           
           AND  htm_trfcost_benefit.apply_to_pmb = YES)):

        IF LOOKUP(STRING(htm_trfcost_benefit.claim_code),cClaimCodeList) = 0 
        THEN
          ASSIGN cClaimCodeList = IF cClaimCodeList = ""
                                  THEN STRING(htm_trfcost_benefit.claim_code)
                                  ELSE cClaimCodeList + "," + STRING(htm_trfcost_benefit.claim_code).
      END.  /* FOR EACH htm_trfcost_benefit NO-LOCK */

      IF cClaimCodeList <> "" THEN 
      DO:
        IF LOOKUP(STRING(btt_auth_detail.default_claim_code),cClaimCodeList) = 0 
        THEN
          ASSIGN cClaimCodeList = cClaimCodeList + "," + STRING(btt_auth_detail.default_claim_code).

        /* TrfCostBenCCWarn rule is activated */
        IF lValidRuleWarn THEN 
        DO:
          IF LOOKUP(cRuleValueWarn,"Warn,WarnAck":U) > 0 THEN
          DO:
            IF NUM-ENTRIES(cClaimCodeList,",":U) > 1 THEN 
            DO:
              ASSIGN cError       = "Ensure the correct claim code is captured because multiple claim codes apply: '" + cClaimCodeList 
                                  + "'.[HELP=Auth Rule Code: TrfCostBenCCWarn]"
                     lAcknowledge = (IF cRuleValueWarn = "WARNACK":U THEN TRUE ELSE FALSE).
         
                     lSuccess = goErrorObject:addError(INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic   /* ipcOwningEntityMnemonic */
                                                      ,INPUT btt_auth_detail.auth_detail_obj                       /* ipdOwningEntityObj      */
                                                      ,INPUT "":U                                                  /* ipcOwningEntityKey      */
                                                      ,INPUT "owning_entity_value":U                               /* ipcFieldName            */
                                                      ,INPUT btt_auth_detail.line_number                           /* ipiLineNumber           */
                                                      ,INPUT cError                                                /* ipcMessageText          */
                                                      ,INPUT "WAR":U                                               /* ipcMessageType          */
                                                      ,INPUT lAcknowledge).                                        /* ipcAcknowledge          */
            END.  /* IF NUM-ENTRIES(cClaimCodeList,",":U) > 1 */
          END.  /* IF LOOKUP(cRuleValueWarn,"Warn,WarnAck":U) > 0 THEN */                                                          
          ELSE                                                                                                                     
            ASSIGN lSuccess = goErrorObject:addError                                                                               
                                           (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic        /* ipcOwningEntityMnemonic */ 
                                           ,INPUT btt_auth_detail.auth_detail_obj                          /* ipdOwningEntityObj      */ 
                                           ,INPUT "":U                                                     /* ipcOwningEntityKey      */ 
                                           ,INPUT "owning_entity_value":U                                  /* ipcFieldName            */
                                           ,INPUT btt_auth_detail.line_number                              /* ipiLineNumber           */ 
                                           ,INPUT "MA":U                                                   /* ipcMessageGroup         */ 
                                           ,INPUT 112  /* The '&1' specified is invalid. '&2' */           /* ipiMessageNumber        */ 
                                           ,INPUT "Auth Rule Value: ":U + cRuleValueWarn + 
                                                  ",[HELP=Auth Rule Code: TrfCostBenCCWarn]").             /* ipcReplaceTextList      */ 
        END.  /* IF lValidRuleWarn */

        /*  TrfcostBenCCInvalid rule is activated */
        IF lValidRuleInvalid THEN 
        DO:
          IF LOOKUP(STRING(btt_auth_detail.claim_code), cClaimCodeList) = 0
          AND btt_auth_detail.claim_code <> buf_auth_schext.claim-code[1] THEN  /* Claim code <> 99 */
          DO:
            CASE ENTRY(1,cRuleValueInvalid,"|":U):
              WHEN "BLOCK":U THEN
              DO:
                ASSIGN cError   = "Invalid claim code - only the following claim codes may be used: " + cClaimCodeList 
                                + "'.[HELP=Auth Rule Code: TrfcostBenCCInvalid]"
                       lSuccess = goErrorObject:addError
                                               (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  /* ipcOwningEntityMnemonic */
                                               ,INPUT btt_auth_detail.auth_detail_obj                      /* ipdOwningEntityObj      */
                                               ,INPUT "":U                                                 /* ipcOwningEntityKey      */
                                               ,INPUT "owning_entity_value":U                              /* ipcFieldName            */
                                               ,INPUT btt_auth_detail.line_number                          /* ipiLineNumber           */
                                               ,INPUT cError                                               /* ipcMessageText          */
                                               ,INPUT "ERR":U).                                            /* ipcMessageType          */
                RETURN.
              END.  /* WHEN "BLOCK":U */

              WHEN "LOAD":U THEN
              DO:
                IF NUM-ENTRIES(cRuleValueInvalid,"|":U) > 1 THEN
                DO:
                  ASSIGN cRuleLoadStatus = ENTRY(2,cRuleValueInvalid,"|":U)
                         cError          = "".

                  IF cRuleLoadStatus     = "" 
                  THEN
                    ASSIGN iAuthStatus   = 6      /* Declined */
                           cStatusReason = "" NO-ERROR.
                  ELSE
                    ASSIGN iAuthStatus   = INTEGER(ENTRY(1,cRuleLoadStatus))  
                           cStatusReason = ENTRY(2,cRuleLoadStatus)  NO-ERROR.

                  IF ERROR-STATUS:ERROR 
                  THEN
                    ASSIGN cError = "Invalid status (" + ENTRY(1,cRuleLoadStatus) + ") specified on Auth Rule setup - please check."
                                  + "[HELP=Auth Rule Code: TrfCostBenCCInvalid]".
                  ELSE DO:
                    ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  iAuthStatus,
                                                                                       INPUT  "System":U).
                    IF NOT lValidStatus 
                    THEN 
                      ASSIGN cError = "Invalid status (" + STRING(iAuthStatus) + ") specified on Auth Rule setup - please check."
                                    + "[HELP=Auth Rule Code: TrfCostBenCCInvalid, ValidStatuses]".
                  END.  /* ELSE - IF ERROR-STATUS:ERROR THEN */

                  IF cError <> "" THEN
                  DO:
                    ASSIGN lSuccess = goErrorObject:addError
                                                   (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  /* ipcOwningEntityMnemonic */
                                                   ,INPUT btt_auth_detail.auth_detail_obj                      /* ipdOwningEntityObj      */
                                                   ,INPUT "":U                                                 /* ipcOwningEntityKey      */
                                                   ,INPUT "related_value":U                                    /* ipcFieldName            */
                                                   ,INPUT btt_auth_detail.line_number                          /* ipiLineNumber           */
                                                   ,INPUT cError                                               /* ipcMessageText          */
                                                   ,INPUT "ERR":U).                                            /* ipcMessageType          */
                    RETURN.
                  END.  /* IF cError <> "" THEN */
                  ELSE 
                    ASSIGN btt_auth_detail.auth_status      = iAuthStatus
                           btt_auth_detail.auth_status_note = cStatusReason.

                END.  /* IF NUM-ENTRIES(cRuleValueInvalid,"|":U) > 1 THEN */
                ELSE 
                  ASSIGN btt_auth_detail.auth_status      = 6 /* Declined */
                         btt_auth_detail.auth_status_note = "":U.

                IF iAuthStatus = 6 
                THEN
                  ASSIGN btt_auth_detail.claim_code = buf_auth_schext.claim-code[1].
              END.  /* WHEN "LOAD":U */

              OTHERWISE DO:
                ASSIGN cError   = "Invalid Auth Rule setup." + "[HELP=Auth Rule Code: TrfCostBenCCInvalid]"
                       lSuccess = goErrorObject:addError
                                               (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  /* ipcOwningEntityMnemonic */
                                               ,INPUT btt_auth_detail.auth_detail_obj                      /* ipdOwningEntityObj      */
                                               ,INPUT "":U                                                 /* ipcOwningEntityKey      */
                                               ,INPUT "related_value":U                                    /* ipcFieldName            */
                                               ,INPUT btt_auth_detail.line_number                          /* ipiLineNumber           */
                                               ,INPUT cError                                               /* ipcMessageText          */
                                               ,INPUT "ERR":U).                                            /* ipcMessageType          */
              END.  /* OTHERWISE DO */

            END CASE.  /* CASE ENTRY(1,cRuleValueInvalid,"|":U): */
          END.  /* IF LOOKUP(btt_auth_detail.claim_code, cClaimCodeList) = 0... */

          IF  AVAILABLE buf_auth_detail
          AND LOOKUP(STRING(buf_auth_detail.claim_code), cClaimCodeList) =  0
          AND LOOKUP(STRING(btt_auth_detail.claim_code), cClaimCodeList) <> 0
          AND ENTRY(1,cRuleValueInvalid,"|":U) = "LOAD":U
          THEN
            ASSIGN btt_auth_detail.auth_status      = 1     /* Authorised */
                   btt_auth_detail.auth_status_note = "".

        END.  /* IF lValidRuleInvalid */
      END.  /* IF cClaimCodeList <> "" */
    END.  /* IF lValidRuleWarn OR lValidRuleInvalid THEN */
  END.  /* IF  btt_auth_detail.owning_entity_mnemonic  = "htmtl":U THEN */
END.  /* IF btt_auth_detail.auth_detail_obj <= 0... */

/*
  Processing to get the tariff cost benefit setups.
  - We only need to get the benefit setups 
    If the detail line is for a Tariff.
    If the detail line is not a Tariff with a related value.
    If the detail line claim code is not zero.              
    If the detail line claim code is not 99.                
*/

IF  btt_auth_detail.owning_entity_mnemonic   = "htmtl":U                        
AND btt_auth_detail.related_entity_mnemonic  = ""                             
AND btt_auth_detail.claim_code              <> 0				     
AND btt_auth_detail.claim_code              <>  buf_auth_schext.claim-code[1] 
AND AVAILABLE trfcost
THEN DO:

  /* 
    Calculate the Tariff Cost amount 
  */
  mipEnv:health:mamedical:getTariffCostBenefit(INPUT  trfcost.trfcost-obj,               /* ipdTrfCostObj     */
                                               INPUT  goAuthorisation:MemberOptionCode,  /* ipiOptionCode     */
                                               INPUT  btt_auth_detail.claim_code,        /* ipiClaimCode      */
                                               INPUT  btt_auth_detail.claim_type,        /* ipcClaimType      */
                                               INPUT  btt_auth_detail.pmb_indicator,     /* iplPMB            */
                                               OUTPUT lTrfCostBenefit,                   /* oplTrfCostBenefit */
                                               OUTPUT iClaimCode,                        /* opiClaimCode      */
                                               OUTPUT dTrfCostAmount,                    /* opdAmount         */
                                               OUTPUT dTrfCostFixedAmount,               /* opdFixedAmount    */
                                               OUTPUT dTrfCostBenefit%).                 /* opdBenefit%       */

  IF lTrfCostBenefit = YES THEN 
  DO:
    IF dTrfCostBenefit% <> 0
    THEN
      ASSIGN btt_auth_detail.benefit_% = dTrfCostBenefit%.

    /*
      For LOC tariff codes the item cost is saved in the LOS calculation 
      (maauthservicestack.p - calcAuthLOSDetails,applAuthLOSDetailsUser).
      If an amount must apply as per Tariff Cost Benefit setups, we need to adjust the item cost and 
      the amount authorised must be recalculated for the LOC line.   
      Note: The item cost and amount authorised for a NON-LOC tariff code is saved and calculated in maauthbussavedetail.i.
            If a tariff cost benefit amount must apply for a detail line that is NOT a LOC the item cost and 
            amount authorised must also be adjusted, but this will be done in maauthbussavedetail.i.
    */
    IF  dTrfCostAmount               <> 0
    AND btt_auth_detail.quantity_los <> 0 
    THEN
      ASSIGN btt_auth_detail.fixed_item_cost = dTrfCostFixedAmount
             btt_auth_detail.item_cost       = IF btt_auth_detail.line_restriction  <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                               THEN dTrfCostAmount 
                                               ELSE ROUND((btt_auth_detail.amount_auth - btt_auth_detail.fixed_item_cost) / 
                                                           btt_auth_detail.quantity_auth , 2 )
             btt_auth_detail.amount_auth     = IF btt_auth_detail.line_restriction  <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                               THEN btt_auth_detail.fixed_item_cost + 
                                                   (btt_auth_detail.item_cost * btt_auth_detail.quantity_auth)
                                               ELSE btt_auth_detail.amount_auth .


  END.  /* IF lTrfCostBenefit = YES */
  ELSE DO:
    IF  AVAILABLE buf_auth_detail
    AND buf_auth_detail.quantity_los <> 0 
    THEN 
      ASSIGN btt_auth_detail.fixed_item_cost = trfcost.fixed-amount
             btt_auth_detail.item_cost       = IF btt_auth_detail.line_restriction  <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                               THEN trfcost.amount
                                               ELSE ROUND((btt_auth_detail.amount_auth - btt_auth_detail.fixed_item_cost) / 
                                                           btt_auth_detail.quantity_auth , 2 )
             btt_auth_detail.amount_auth     = IF btt_auth_detail.line_restriction  <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                               THEN btt_auth_detail.fixed_item_cost + 
                                                   (btt_auth_detail.item_cost * btt_auth_detail.quantity_auth)
                                               ELSE btt_auth_detail.amount_auth .

  END.  /* ELSE DO: IF lTrfCostBenefit = YES */
END.  /* Processing to get the tariff cost benefit setups */

