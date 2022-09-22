/* maauthbusdetlinerestrictval.i MEDSTAR Medical Aid System
                         Authorisation Line Restriction Validations
                         (c) Copyright 1990 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/

/*
  Line Restriction Validations
  The captured amount is saved in the dAmountAuth variable (in maauthbusdetlinerestrict.i)
  before the item cost and amount authorised is calculated for a tariff or a nappi.
*/
IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U 
OR btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U THEN
DO:

  IF  btt_auth_detail.owning_entity_mnemonic = "hlmcr":U 
  AND btt_auth_detail.amount_auth = 0 
  THEN
    ASSIGN btt_auth_detail.item_cost       = 0
           btt_auth_detail.fixed_item_cost = 0.

  IF  dAmountAuth <> 0 
  AND dAmountAuth <> ? THEN
  DO:
    /* Check if an Authorised Amount may be bigger than the System Calculated Amount */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                   INPUT  btt_auth.option_code,
                                                   INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                   INPUT  "LineRestrictionExceptions":U,
                                                   INPUT  btt_auth.start_date,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).
    
    IF lValidRule
    AND AVAILABLE trfcost
    AND trfcost.cost-calculation <> "":U
    AND LOOKUP(cRuleValue, trfcost.cost-calculation) > 0 THEN
    DO:
      IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U
      AND dAmountAuth > btt_auth_detail.item_cost THEN
      DO:
        ASSIGN cError = SUBSTITUTE("The captured amount authorised (&1) cannot exceed Tariff cost amount (&2).",
                                   dAmountAuth,btt_auth_detail.item_cost)
                      + "[HELP=Auth Rule Code: LineRestrictionExceptions]".
        
        goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */  
                          INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */  
                          INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                          INPUT "item_cost":U,                                      /* FieldName               */  
                          INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */  
                          INPUT cError,                                             /* ipcMessageText          */  
                          INPUT "Err").                                             /* ErrorType               */  
        LEAVE. 
      END.  /* IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U AND dAmountAuth > btt_auth_detail.item_cost THEN */
      
      IF dAMountAuth <= btt_auth_detail.item_cost
      THEN
        ASSIGN btt_auth_detail.amount_auth = dAmountAuth.

    END.  /* IF lValidRule AND AND AVAILABLE trfcost trfcost.cost-calculation <> "":U AND LOOKUP(trfcost.cost-calculation, cRuleValue) > 0 THEN */
    ELSE DO:
      /* Validate the captured amount against the calculated amount for non loc lines */
      IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U
      AND dAmountAuth                     > btt_auth_detail.amount_auth
      AND btt_auth_detail.quantity_los    = 0 
      AND btt_auth_detail.amount_auth    <> 0 
      AND (NOT AVAILABLE buf_auth_detail
      OR   buf_auth_detail.amount_auth   <> dAmountAuth
      OR   buf_auth_detail.quantity_auth <> btt_auth_detail.quantity_auth) THEN
      DO:
        ASSIGN cError = SUBSTITUTE("The captured amount authorised (&1) cannot exceed calculated authorised amount (&2).",
                                   dAmountAuth,btt_auth_detail.amount_auth).
        goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */  
                          INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */  
                          INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                          INPUT "amount_auth":U,                                    /* FieldName               */  
                          INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */  
                          INPUT cError,                                             /* ipcMessageText          */  
                          INPUT "Err").                                             /* ErrorType               */  
        LEAVE. 
      END.  /* IF dAmountAuth > btt_auth_detail.amount_auth... */
      
      /* Validate the captured amount against the calculated amount on LOC lines*/
      IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U
      AND (AVAILABLE buf_auth_detail 
      AND dAmountAuth                   > buf_auth_detail.amount_auth)
      AND btt_auth_detail.quantity_los <> 0 
      AND btt_auth_detail.amount_auth  <> 0 
      THEN
      DO:
        ASSIGN cError = SUBSTITUTE("The captured amount authorised (&1) cannot exceed calculated authorised amount (&2).",
                                   dAmountAuth,buf_auth_detail.amount_auth).
        goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */  
                          INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */  
                          INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                          INPUT "amount_auth":U,                                    /* FieldName               */  
                          INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */  
                          INPUT cError,                                             /* ipcMessageText          */  
                          INPUT "Err").                                             /* ErrorType               */  
        LEAVE. 
      END.  /* IF dAmountAuth > btt_auth_detail.amount_auth... */
      
      IF NOT AVAILABLE buf_auth_detail
      OR (btt_auth_detail.related_entity_mnemonic = buf_auth_detail.related_entity_mnemonic
      AND btt_auth_detail.related_value           = buf_auth_detail.related_value) THEN 
      DO:
        IF dAmountAuth <= btt_auth_detail.amount_auth
        OR btt_auth_detail.amount_auth = 0 THEN
        DO:
          ASSIGN btt_auth_detail.amount_auth = dAmountAuth.
          
          /* Item cost should not be reduced if amount was reduced by the limit checking.  
             Variable lLimitAmtApplied was assigned in maauthbusdetlinerestict.i. */
          IF lLimitAmtApplied 
          THEN ASSIGN btt_auth_detail.item_cost = IF AVAILABLE buf_auth_detail
                                                  THEN buf_auth_detail.item_cost
                                                  ELSE btt_auth_detail.item_cost.
          ELSE ASSIGN btt_auth_detail.item_cost = IF btt_auth_detail.quantity_auth <> 0
                                                  THEN ROUND((btt_auth_detail.amount_auth - btt_auth_detail.fixed_item_cost) /
                                                              btt_auth_detail.quantity_auth,2)
                                                  ELSE btt_auth_detail.amount_auth - btt_auth_detail.fixed_item_cost.
        END.  /* IF dAmountAuth <= btt_auth_detail.amount_auth OR btt_auth_detail.amount_auth = 0 THEN */
      END.  /* IF   NOT AVAILABLE buf_auth_detail */
    END.  /* ELSE - IF lValidRule AND AVAILABLE trfcost AND trfcost.cost-calculation <> "":U ... */
  END.  /* IF dAmountAuth <> 0 AND dAmountAuth <> ? THEN */
END.  /* IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U OR... */
ELSE IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U THEN
DO:
  IF btt_auth_detail.owning_entity_mnemonic = "hlmcr":U THEN
  DO:
    IF btt_auth_detail.amount_auth <> 0 THEN
    DO:
      ASSIGN btt_auth_detail.amount_auth     = 0
             btt_auth_detail.item_cost       = 0
             btt_auth_detail.fixed_item_cost = 0.

      goErrorObject:addError
                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
                      INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                      INPUT "":U,                                               /* ipcOwningEntityKey      */
                      INPUT "amount_auth":U,                                    /* FieldName               */
                      INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                      INPUT "Amount for Basket not allowed",                    /* ipcMessageText          */
                      INPUT "WAR":U,                                            /* ErrorType               */
                      INPUT FALSE).                                             /* Warnack/Warn            */
    END.  /* IF btt_auth_detail.amount_auth <> 0 THEN */
    ELSE
      ASSIGN btt_auth_detail.item_cost       = 0
             btt_auth_detail.fixed_item_cost = 0.

  END.  /* IF btt_auth_detail.owning_entity_mnemonic = "hlmcr":U THEN */

  /*
    lUseCalculatedAmount is set to true when the LineRestriction rule specifies that the
    btt_auth_detail.line_restriction must always be set to "ma_acAuthLineRestrictionQuantity":U.
    If this is the case, then the auth amount must always be set to the calculated amount.
    (Calculated amount = fixed_item_cost + (quantity_auth * item_cost))
  */
  ELSE IF lUseCalculatedAmount THEN
  DO:
    ASSIGN btt_auth_detail.amount_auth = IF dAmountAuth = btt_auth_detail.amount_auth 
                                         THEN btt_auth_detail.fixed_item_cost + (btt_auth_detail.quantity_auth * btt_auth_detail.item_cost)
                                         ELSE btt_auth_detail.amount_auth.

    IF AVAILABLE buf_auth_detail THEN
    DO:
      IF buf_auth_detail.quantity_auth = btt_auth_detail.quantity_auth
      AND dAmountAuth <> btt_auth_detail.amount_auth 
      THEN
        ASSIGN lDisplayWarning = TRUE.
      ELSE IF dAmountAuth <> buf_auth_detail.amount_auth 
           AND buf_auth_detail.quantity_auth <> btt_auth_detail.quantity_auth
           THEN
             ASSIGN lDisplayWarning = TRUE.
           ELSE
             ASSIGN lDisplayWarning = FALSE.
    END.  /* IF AVAILABLE buf_auth_detail THEN */

    IF dAmountAuth = btt_auth_detail.amount_auth THEN
      ASSIGN lDisplayWarning = FALSE.

    IF lDisplayWarning THEN
    DO:
      ASSIGN cError = "The captured amount authorised (&1) may not differ from the calculated authorised amount (&2)." +
                      " Calculated amount will be used."
             cError = SUBSTITUTE(cError,dAmountAuth,btt_auth_detail.amount_auth).
      goErrorObject:addError
                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */  
                        INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */  
                        INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                        INPUT "amount_auth":U,                                    /* FieldName               */  
                        INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */  
                        INPUT cError,                                             /* ipcMessageText          */  
                        INPUT "WAR",                                              /* ErrorType               */
                        INPUT FALSE).                                             /* Warnack/Warn            */ 
    END.  /* IF dAmountAuth > btt_auth_detail.amount_auth... */
  END.  /* ELSE IF lUseCalculatedAmount THEN */
END.  /* ELSE IF btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U THEN */
ELSE IF btt_auth_detail.owning_entity_mnemonic = "hlmcr":U 
     AND btt_auth_detail.amount_auth = 0 
     THEN
       ASSIGN btt_auth_detail.item_cost       = 0
              btt_auth_detail.fixed_item_cost = 0.

