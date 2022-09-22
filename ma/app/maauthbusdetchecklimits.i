/* maauthbusdetchecklimits.i MEDSTAR Medical Aid System
                             Healthcare Auth Businesslogic Service - Limit checking on Authorisation Detail lines
                             (c) Copyright 2021 - 2022
                             MIP Holdings (Pty) Ltd
                             All rights reserved
------------------------------------------------------------------------------
  Purpose:  Include to activate limit checking on Auth Clinical Detail Lines.

  Notes  :  Return an error message if applicable

------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 010195 &THEN

DEFINE VARIABLE cAuthStatus         AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cAuthStatusNote     AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cClaimType          AS CHARACTER                NO-UNDO.

DEFINE VARIABLE dAllowAmount        AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dLimitAmount        AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dLimitQuantity      AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dPMBValue           AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dAllowQuantity      AS DECIMAL                  NO-UNDO.

DEFINE VARIABLE lLimitsChecked      AS LOGICAL   INITIAL FALSE  NO-UNDO.

/*
  New Clinical detail line is added OR existing detail line is updated.
*/
IF btt_auth_detail.record_action <> "DELETE":U THEN
DO:
  IF  btt_auth_detail.auth_status      = 1
  AND btt_auth.dependant              <> 99
  AND (btt_auth_detail.amount_auth    <> 0
   OR  btt_auth_detail.quantity_auth  <> 0) THEN
  DO:
    ASSIGN cError = "":U.

    IF LOOKUP(btt_auth_detail.line_restriction, "ma_acAuthLineRestrictionAmount,ma_acAuthLineRestrictionAmt&Qty,ma_acAuthLineRestrictionQuantity":U,",") = 0
    THEN NEXT.

    ASSIGN cTrackingMessage = "Auth Limits - Check if Limits must be applied: AuthObj=" + STRING(btt_auth.auth_obj) + " AuthDetailObj=" + STRING(btt_auth_detail.auth_detail_obj)
                            + " InsurerObj=" + STRING(btt_auth.insurer_obj) + " Option=" + STRING(btt_auth.option_code) + " StartDate=" + STRING(btt_auth_detail.start_date).

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /*
      When updating Existing Clinical detail lines, check if any values affecting limits, were changed.
    */
    IF btt_auth_detail.claim_code <> 99
    THEN
      ASSIGN iClaimCode = btt_auth_detail.claim_code.
    ELSE
      ASSIGN iClaimCode = btt_auth_detail.default_claim_code.

    IF NOT (AVAILABLE buf_auth_detail
    AND btt_auth_detail.auth_status      = buf_auth_detail.auth_status
    AND btt_auth_detail.line_restriction = buf_auth_detail.line_restriction
    AND btt_auth_detail.amount_auth      = buf_auth_detail.amount_auth
    AND btt_auth_detail.quantity_auth    = buf_auth_detail.quantity_auth
    AND btt_auth_detail.start_date       = buf_auth_detail.start_date
    AND iClaimCode                       = buf_auth_detail.claim_code
    AND btt_auth_detail.claim_type       = buf_auth_detail.claim_type
    AND btt_auth_detail.pmb_indicator    = buf_auth_detail.pmb_indicator)
    THEN DO:
      /*
        All exclusion values passed - Criteria met whereby Limits must be checked.
        Set values that need to be checked for the limits.
      */
      IF btt_auth_detail.amount_auth <> 0
      THEN
        ASSIGN dLimitAmount = btt_auth_detail.amount_auth.
      ELSE
        IF btt_auth_detail.item_cost <> 0
        THEN ASSIGN dLimitAmount = btt_auth_detail.fixed_item_cost
                                 + (btt_auth_detail.item_cost * btt_auth_detail.quantity_auth). /* Pass in an amount for limit */
        ELSE ASSIGN dLimitAmount = 9999999.99.                                                  /* To cater for an unlimited value */
      
      IF btt_auth_detail.quantity_auth <> 0
      THEN
        ASSIGN dLimitQuantity = btt_auth_detail.quantity_auth.
      ELSE
        IF btt_auth_detail.item_cost <> 0
        THEN ASSIGN dLimitQuantity = (btt_auth_detail.amount_auth - btt_auth_detail.fixed_item_cost) / btt_auth_detail.item_cost.
        ELSE ASSIGN dLimitQuantity = 1.
      
      /*
        Call Check Limits method.
      */
      ASSIGN cTrackingMessage = "Auth Limits - RUN checkLimits with: Insurer=" + STRING(btt_auth.insurer_obj) + " Option=" + STRING(btt_auth.option_code) 
                              + " Member=" + STRING(btt_auth.mem_num) + " Dep=" + STRING(btt_auth.dependant) + " StartDate=" + STRING(btt_auth_detail.start_date,"9999/99/99")
                              + " ClaimCode=" + STRING(iClaimCode) + " ClaimType=" + btt_auth_detail.claim_type
                              + " dLimitAmount=" + STRING(dLimitAmount) + " dLimitQuantity=" + STRING(dLimitQuantity)
                              + " PBMind=" + STRING(btt_auth_detail.pmb_indicator) + " AuthLevel=Detail".
      
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      
      mipEnv:Health:AuthService:checkLimits(INPUT  btt_auth.insurer_obj,
                                            INPUT  btt_auth.option_code,                /* ipiOptionCode                                       */
                                            INPUT  btt_auth.mem_num,                    /* ipcMemNum                                           */
                                            INPUT  btt_auth.dependant,                  /* ipiDependant                                        */
                                            INPUT  btt_auth_detail.start_date,          /* ipdDate                                             */
                                            INPUT  iClaimCode,                          /* ipiClaimCode                                        */
                                            INPUT  btt_auth_detail.claim_type,          /* ipcClaimType                                        */
                                            INPUT  dLimitAmount,                        /* ipdAmount                                           */
                                            INPUT  dLimitQuantity,                      /* ipdQuantity                                         */
                                            INPUT  btt_auth_detail.pmb_indicator,       /* iplPMBIndicator                                     */
                                            INPUT  "Detail":U,                          /* ipcAuthLevel                                        */
                                            INPUT  btt_auth.auth_obj,                   /* Auth header obj                                     */
                                            INPUT  btt_auth_provider.auth_provider_obj, /* Auth provider obj                                   */
                                            OUTPUT dAllowAmount,                        /* Authorised amount allowed after Limit checking      */
                                            OUTPUT dAllowQuantity,                      /* Authorised quantity allowed after Limit checking    */
                                            OUTPUT dPMBValue,                           /* PMB Value after Limit checking                      */
                                            OUTPUT iAuthStatus,                         /* Authorised status allowed after Limit checking      */
                                            OUTPUT cAuthStatusNote,                     /* Authorised status note after Limit checking         */
                                            OUTPUT cClaimType,                          /* Claim type after Limit checking                     */
                                            OUTPUT cError,                              /* Any errors returned from Limit checking             */
                                            OUTPUT lLimitsChecked,                      /* Indicates if limit checking did apply               */
                                            OUTPUT TABLE tt_limitwarn).                 /* Temp table with limit warnings after Limit checking */
      
      IF iAuthStatus = ?
      THEN ASSIGN cAuthStatus = "Unknown".
      ELSE ASSIGN cAuthStatus = STRING(iAuthStatus).
      
      ASSIGN cTrackingMessage = "Auth Limits - RETURN from checkLimits: dAllowAmount=" + STRING(dAllowAmount) + " dAllowQuantity=" + STRING(dAllowQuantity) 
                              + " dPMBValue=" + STRING(dPMBValue) + " iAuthStatus=" + cAuthStatus + " cAuthStatusNote=" + cAuthStatusNote
                              + " cClaimType=" + cClaimType + " lLimitsChecked=" + STRING(lLimitsChecked) + " cError=" + cError.
      
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      
      /*
        If any errors are returned, display it and don't save the Detail line.
      */
      IF cError <> "":U THEN
      DO:
        ASSIGN cError = cError + " (Tariff: " + btt_auth_detail.owning_alt_value + ")".
      
        goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                               INPUT btt_auth_detail.auth_detail_obj,                      /* ipdOwningEntityObj      */
                               INPUT "":U,                                                 /* ipcOwningEntityKey      */
                               INPUT "claim_code":U,                                       /* ipcFieldName            */
                               INPUT btt_auth_detail.line_number,                          /* ipiLineNumber           */
                               INPUT cError,                                             /* ipcMessageText          */
                               INPUT "ERR":U).
      END.  /*  IF cError <> "":U THEN */
      ELSE DO:
        /*
          No errors returned, method checkLimits indicates Limits were applied. Now apply further validations and amount/quantity reductions.
        */
        IF lLimitsChecked THEN
        DO:
          /*
            Display any limit warnings returned as Warning Acknowledgements.
          */
          FOR EACH tt_limitwarn
            BY limit_sequence:
      
            goErrorObject:addError
                             (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */
                              INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                              INPUT "":U,                                                /* ipcOwningEntityKey      */
                              INPUT "claim_code":U,                                      /* FieldName               */
                              INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                              INPUT tt_limitwarn.limit_warning,                          /* ipcMessageText          */
                              INPUT "WAR":U,                                             /* ErrorType               */
                              INPUT TRUE).                                               /* Warnack/Warn            */
          END.  /* FOR EACH tt_limitwarn */
      
          /*
            Decline detail line if authorisation status is declined with limit checking and assign claim code to 99.
          */
          IF  iAuthStatus  = 6
          AND iAuthStatus <> btt_auth_detail.auth_status
          THEN
            ASSIGN btt_auth_detail.auth_status      = iAuthStatus
                   btt_auth_detail.auth_status_note = cAuthStatusNote
                   btt_auth_detail.claim_code       = buf_auth_schext.claim-code[1].
          ELSE DO:
            /*
              Reduce the authorised quantity and amount if line restriction is quantity or amount&quantity
              AND quantity passed in was reduced with limit checking.
            */
            IF  dAllowQuantity  <  dLimitQuantity                               /* Output quantity < detail value passed in */
            AND cAuthStatusNote <> "":U                   		            /* If authorised value is reduced, a reason must be assigned */
            AND LOOKUP(btt_auth_detail.line_restriction,"ma_acAuthLineRestrictionQuantity,ma_acAuthLineRestrictionAmt&Qty",",") > 0
            THEN
              ASSIGN btt_auth_detail.quantity_auth = dAllowQuantity
                     btt_auth_detail.amount_auth   = btt_auth_detail.fixed_item_cost
                                                   + (btt_auth_detail.quantity_auth * btt_auth_detail.item_cost).
      
            /*
              Reduce the authorised amount if line restriction is amount, quantity or amount&quantity
              AND amount passed in was reduced with limit checking.
            */
            IF  dAllowAmount    <  dLimitAmount                                 /* Output amount < detail value passed in */
            AND cAuthStatusNote <> "":U                  		              /* If authorised value is reduced a reason must be assigned */
            AND LOOKUP(btt_auth_detail.line_restriction,"ma_acAuthLineRestrictionAmount,ma_acAuthLineRestrictionQuantity,ma_acAuthLineRestrictionAmt&Qty",",") > 0
            THEN
              ASSIGN btt_auth_detail.amount_auth = dAllowAmount.
      
            /*
              Reduce authorised amount if line restriction is amount and quantity passed in, was reduced with the limit checking.
            */
            IF  dAllowQuantity                  <  dLimitQuantity              /* Output quantity < detail value passed in */
            AND btt_auth_detail.item_cost       <> 0
            AND cAuthStatusNote                 <> "":U                  	  /* If authorised value is reduced a reason must be assigned */
            AND btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U
            THEN
              ASSIGN btt_auth_detail.amount_auth = ((btt_auth_detail.amount_auth - btt_auth_detail.fixed_item_cost) / btt_auth_detail.item_cost)
                                                 * dAllowQuantity.
      
            /*
              Assign default status note if a value is passed back from the limit checking.
            */
            IF  cAuthStatusNote <> "":U
            AND cAuthStatusNote <> btt_auth_detail.auth_status_note
            THEN
              ASSIGN btt_auth_detail.auth_status_note = cAuthStatusNote.
      
            /*
              Assign values that will apply for PMB.
            */
            IF dPMBValue <> btt_auth_detail.pmb_value
            THEN
              ASSIGN btt_auth_detail.pmb_value = dPMBValue.
      
            IF  cClaimType <> btt_auth_detail.claim_type
            AND cClaimType <> "":U
            THEN
              ASSIGN btt_auth_detail.claim_type = cClaimType.
      
          END. /* ELSE - IF iAuthStatus = 6 AND iAuthStatus <> btt_auth_detail.auth_status THEN */
      
          ASSIGN cTrackingMessage = "Auth Limits - After checkLimits applied: AuthStatus=" + STRING(btt_auth_detail.auth_status) + " StatusNote=" + STRING(btt_auth_detail.auth_status_note)
                                  + " AmountAuth=" + STRING(btt_auth_detail.amount_auth) + " QtyAuth=" + STRING(btt_auth_detail.quantity_auth)
                                  + " ClaimCode=" + STRING(btt_auth_detail.claim_code) + " ClaimType=" + STRING(btt_auth_detail.claim_type) + " PMBvalue=" + STRING(btt_auth_detail.pmb_value).
      
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      
        END.  /* IF lLimitsChecked THEN */
      END.  /* ELSE - IF cError <> "":U THEN */
    END.  /* IF NOT (AVAILABLE buf_auth_detail... */
  END.  /*IF btt_auth_detail.auth_status = 1... */

  IF NOT lLimitsChecked THEN
  DO:
    ASSIGN cTrackingMessage = "Auth Limits - NO Limit checking done: Insurer=" + STRING(btt_auth.insurer_obj) + " Option=" + STRING(btt_auth.option_code) 
                            + " Member=" + STRING(btt_auth.mem_num) + " Dep=" + STRING(btt_auth.dependant) + " StartDate=" + STRING(btt_auth_detail.start_date,"9999/99/99")
                            + " ClaimCode=" + STRING(btt_auth_detail.claim_code) + " ClaimType=" + btt_auth_detail.claim_type.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
  END.  /* IF NOT lLimitsChecked THEN */

END.  /* IF btt_auth_detail.record_action <> "DELETE":U */

&ENDIF


