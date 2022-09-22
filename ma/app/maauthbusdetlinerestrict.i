/* maauthbusdetlinerestrict.i MEDSTAR Medical Aid System
                         Authorisation Line Restriction Business Logic
                         (c) Copyright 1990 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/
mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT goAuthorisation:InsurerObj,
                                               INPUT goAuthorisation:MemberOptionCode,
                                               INPUT "ma_acAuthRuleTypeAuthDetail":U,
                                               INPUT "LineRestriction":U,
                                               INPUT goAuthorisation:StartDate,
                                               OUTPUT lValidRule,
                                               OUTPUT cRuleValue).

IF NOT lValidRule
THEN
  ASSIGN cError   = "Auth Rule setup is missing.[HELP=Auth Rule Code: LineRestriction]":U
         lSuccess = goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                           INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                           INPUT "":U,                                                 // ipcOwningEntityKey
                                           INPUT "line_restriction":U,                                 // ipcFieldName
                                           INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                           INPUT cError,                                               // ipcMessageText
                                           INPUT "ERR":U).                                             // ipcMessageType
ELSE DO:
  ASSIGN cTrackingMessage = "LineRestriction START - cRuleValue=" + cRuleValue
                          + " auth_detail_obj="           + STRING(btt_auth_detail.auth_detail_obj)
                          + " auth_provider_obj="         + STRING(btt_auth_detail.auth_provider_obj)
                          + " owning_alt_value="          + btt_auth_detail.owning_alt_value
                          + " quantity_auth="             + STRING(btt_auth_detail.quantity_auth)
                          + " amount_auth="               + STRING(btt_auth_detail.amount_auth)
                          + " line_restriction="          + btt_auth_detail.line_restriction
                          + " AVAILABLE buf_auth_detail=" + STRING(AVAILABLE buf_auth_detail)
                          + " _auth_group_update="        + STRING(btt_auth_detail._auth_group_update).
  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

    CASE ENTRY(1,cRuleValue,"|":U):
      WHEN "Rule":U THEN
      DO:
        IF btt_auth_detail.auth_detail_obj <= 0 THEN
        DO:
          IF NUM-ENTRIES(cRuleValue,"|":U) < 2
          OR ENTRY(2,cRuleValue,"|":U) = "" THEN
          DO:
            ASSIGN cError   = "LineRestriction rule setup is incorrect - second entry is required in the rule value.":U
                   lSuccess = goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                                     INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                                     INPUT "":U,                                                 // ipcOwningEntityKey
                                                     INPUT "line_restriction":U,                                 // ipcFieldName
                                                     INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                                     INPUT cError,                                               // ipcMessageText
                                                     INPUT "ERR":U).                                             // ipcMessageType
          END.  // IF NUM-ENTRIES(cRuleValue,"|":U) < 2 OR...
          ELSE DO:
            FIND FIRST mic_acronym NO-LOCK
               WHERE mic_acronym.acronym_key = ENTRY(2,cRuleValue,"|":U)
               NO-ERROR.

            IF NOT AVAILABLE mic_acronym
            THEN
              FIND FIRST mic_acronym NO-LOCK
                WHERE mic_acronym.acronym_label = ENTRY(2,cRuleValue,"|":U)
                NO-ERROR.

            {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE mic_acronym
            THEN
              ASSIGN
                btt_auth_detail.line_restriction = mic_acronym.acronym_key.
            ELSE
              ASSIGN
                cError   = "LineRestriction rule setup is incorrect - invalid line restriction default value.":U
                lSuccess = goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                                  INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                                  INPUT "":U,                                                 // ipcOwningEntityKey
                                                  INPUT "line_restriction":U,                                 // ipcFieldName
                                                  INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                                  INPUT cError,                                               // ipcMessageText
                                                  INPUT "ERR":U).                                             // ipcMessageType
          END.  // ELSE - IF NUM-ENTRIES(cRuleValue,"|":U) < 2 THEN
        END.  // IF btt_auth_detail.auth_detail_obj <= 0 THEN
      END.  // WHEN "Rule":U THEN

      WHEN "System":U THEN
      DO:
        /*
          Line restriction defaults when adding a new auth detail line
        */
        IF btt_auth_detail.auth_detail_obj <= 0
        OR NOT AVAILABLE buf_auth_detail THEN
        DO:
          IF  btt_auth_detail.quantity_auth <> 0
          AND btt_auth_detail.amount_auth    = 0
          THEN
            ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U.
          ELSE IF btt_auth_detail.amount_auth <> 0 THEN
          DO:
            IF btt_auth_detail.quantity_auth = 0
            THEN
              ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U.
            ELSE
              ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U.
          END.  // ELSE IF btt_auth_detail.amount_auth <> 0 THEN
          ELSE IF btt_auth_detail.quantity_auth = 0
              AND btt_auth_detail.amount_auth   = 0
              THEN
                ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionUnlimited":U.
        END.  // IF btt_auth_detail.auth_detail_obj <= 0 THEN

        /*
          Line restriction defaults when updating an existing auth detail line
        */
        ELSE DO:
          /* If amount authorised is cleared and line contains a quantity authorised. */
          IF (buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U
          OR  buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U)
          AND btt_auth_detail.amount_auth      = 0
          AND btt_auth_detail.quantity_auth   <> 0
          THEN
            ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U.

          /* If quantity authorised is cleared and line contains an amount authorised. */
          IF  buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U
          AND btt_auth_detail.amount_auth     <> 0
          AND btt_auth_detail.quantity_auth    = 0
          THEN
            ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U.

          /*  If line restriction = Amount: The quantity authorised is updated and amount authorised contains a value.  */
          IF  buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U
          AND buf_auth_detail.quantity_auth   <> btt_auth_detail.quantity_auth
          AND btt_auth_detail.amount_auth     <> 0
          AND btt_auth_detail.quantity_auth   <> 0
          THEN
            ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U.

          /* If line restriction = Quantity: The amount authorised is updated with a value and quantity authorised is updated to zero. */
          IF  buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U
          AND buf_auth_detail.amount_auth     <> btt_auth_detail.amount_auth
          AND btt_auth_detail.amount_auth     <> 0
          AND btt_auth_detail.quantity_auth    = 0
          THEN
            ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U.

          /*  If line restriction = Quantity: The amount authorised is updated and quantity authorised contains a value.  */
          IF  buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U
          AND buf_auth_detail.amount_auth     <> btt_auth_detail.amount_auth
          AND btt_auth_detail.amount_auth     <> 0
          AND btt_auth_detail.quantity_auth   <> 0
          THEN
            ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U.

          /* If line was loaded with no quantity authorised and amount authorised, and one/both fields are updated.  */
          IF  buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionUnlimited":U THEN
          DO:
            IF  btt_auth_detail.quantity_auth <> 0
            AND btt_auth_detail.amount_auth   =  0
            THEN
              ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U.

            /* If line was loaded with no quantity and amount and amount is updated. */
            ELSE IF  btt_auth_detail.amount_auth  <> 0
                 AND btt_auth_detail.quantity_auth = 0
                 THEN
                   ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount":U.

            ELSE IF  btt_auth_detail.amount_auth   <> 0
                 AND btt_auth_detail.quantity_auth <> 0
                 THEN
                   ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmt&Qty":U.

          END.  // ELSE IF  buf_auth_detail.line_restriction = "ma_acAuthLineRestrictionUnlimited":U THEN

          /* If both quantity and amount authorised is updated to zero. */
          IF (btt_auth_detail.quantity_auth = 0 AND btt_auth_detail.amount_auth = 0)
          OR (btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionQuantity":U
          AND btt_auth_detail.quantity_auth = 0)
          THEN
            ASSIGN btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionUnlimited":U
                   btt_auth_detail.amount_auth      = 0.

        END.  // ELSE - IF btt_auth_detail.auth_detail_obj <= 0 THEN
      END.  // WHEN "System":U THEN

      OTHERWISE
      DO:
        ASSIGN cError   = "The Auth Rule setup is incorrect - invalid rule value.[HELP=Auth Rule Code: LineRestriction]":U
               lSuccess = goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                                 INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                                 INPUT "":U,                                                 // ipcOwningEntityKey
                                                 INPUT "line_restriction":U,                                 // ipcFieldName
                                                 INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                                 INPUT cError,                                               // ipcMessageText
                                                 INPUT "ERR":U).                                             // ipcMessageType
      END.  /* OTHERWISE */
    END CASE.

    /*
       Variable dAmountAuth is saved to determine if value was adjusted from original system calculated values.
       We assume that if dAmountAuth is different from the calculated system value, that the user updated the amount.
    */
    ASSIGN dAmountAuth = btt_auth_detail.amount_auth.

    /*
       Determine if amount authorised was reduced by limit checking. If amount was reduced by the limit checking
       (not enough benefits were available), the line will be authorised and a default status note would have been assigned.
    */
    ASSIGN lLimitAmtApplied = NO.

    IF btt_auth_detail.amount_auth       <> 0
    AND btt_auth_detail.auth_status       = 1
    AND btt_auth_detail.auth_status_note <> "":U THEN
    DO:
      /* Get rule code “ExceedLimitStatusReason” */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                     INPUT  goAuthorisation:MemberOptionCode,
                                                     INPUT  "ma_acAuthRuleTypeLimits":U,
                                                     INPUT  "ExceedLimitStatusReason":U,
                                                     INPUT  goAuthorisation:StartDate,
                                                     OUTPUT lValidRuleExceedLim,
                                                     OUTPUT cRuleValueExceedLim).

      ASSIGN cTrackingMessage = "LineRestriction - ExceedLimitStatusReason - "
                            + " lValidRuleExceedLim="              + STRING(lValidRuleExceedLim)
                            + " cRuleValueExceedLim="              + cRuleValueExceedLim
                            + " btt_auth_detail.auth_status_note=" + btt_auth_detail.auth_status_note.
    { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

      IF  lValidRuleExceedLim
      AND cRuleValueExceedLim = btt_auth_detail.auth_status_note THEN
      DO:
        /* We will only set lLimitApplied if the USER didn't change the amount. */
        IF btt_auth_detail.amount_auth = buf_auth_detail.amount_auth
        THEN
          ASSIGN lLimitAmtApplied = YES.
        /* We clear the auth_status_note, because the Limit checking will run again if the Auth amount is changed. */
        ELSE
          ASSIGN btt_auth_detail.auth_status_note = "":U.
      END.  /* IF  lValidRuleExceedLim AND cRuleValueExceedLim = btt_auth_detail.auth_status_note */

    END.  /* IF btt_auth_detai.amount_auth <> 0 AND... */

  ASSIGN cTrackingMessage = "LineRestriction End - "
                          + " btt_auth_detail.line_restriction=" + btt_auth_detail.line_restriction
                          + " lLimitAmtApplied=" + STRING(lLimitAmtApplied).
  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

END.  //ELSE - IF NOT lValidRule THEN
