/* maauthbusvalauthchecklimits.i  MEDSTAR Medical Aid System
                                  Authorisation Header Limit Checking
                                  (c) Copyright 2022
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/
DEFINE VARIABLE cAuthStatus     AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cAuthStatusNote AS CHARACTER                NO-UNDO.
DEFINE VARIABLE dAllowAmount    AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dLimitAmount    LIKE hat_auth.amount_auth   NO-UNDO.
DEFINE VARIABLE dLimitQuantity  LIKE hat_auth.quantity_auth NO-UNDO.
DEFINE VARIABLE dPMBValue       AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dQuantity       AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE lLimitsChecked  AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE iAuthStatus     AS INTEGER                  NO-UNDO.
DEFINE VARIABLE iClaimCode      AS INTEGER                  NO-UNDO.

ASSIGN cTrackingMessage = "Checklimits - Before Header Limits checking:"
                        + " Status="   + STRING(btt_auth.auth_status)   + " Auth Amount=" + STRING(btt_auth.amount_auth)
                        + " Auth Qty=" + STRING(btt_auth.quantity_auth) + " Dependant="   + STRING(btt_auth.dependant).

{ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

IF   btt_auth.auth_status  = 1 /* Authorised */
AND (btt_auth.amount_auth <> 0 OR btt_auth.quantity_auth <> 0)
AND  btt_auth.dependant   <> 99 THEN
DO:
  ASSIGN lLimitsChecked = FALSE.

  IF AVAILABLE buf_auth_schext
  AND btt_auth.claim_code <> buf_auth_schext.claim-code[1]
  THEN
    ASSIGN iClaimCode = btt_auth.claim_code.
  ELSE
    ASSIGN iClaimCode = btt_auth.default_claim_code.

  /*
    Only do limit checking if the auth is new or any of the values that
    may influence the limit checking, has changed.
  */

  ASSIGN cTrackingMessage = "Checklimits - Only do limit checking if:"
                          + " btt_auth.auth_obj:" + STRING(btt_auth.auth_obj) + "<=0".
  IF AVAILABLE buf_auth
  THEN
    ASSIGN cTrackingMessage = cTrackingMessage
                            + " OR Status:"    + STRING(btt_auth.auth_status)   + "<>" + STRING(buf_auth.auth_status)
                            + " OR Amount:"    + STRING(btt_auth.amount_auth)   + "<>" + STRING(buf_auth.amount_auth)
                            + " OR Qty:"       + STRING(btt_auth.quantity_auth) + "<>" + STRING(buf_auth.quantity_auth)
                            + " OR StartDate:" + STRING(btt_auth.start_date)    + "<>" + STRING(buf_auth.start_date)
                            + " OR CC:"        + STRING(btt_auth.claim_code)    + "<>" + STRING(buf_auth.claim_code)
                            + " OR CT:"        + STRING(btt_auth.claim_type)    + "<>" + STRING(buf_auth.claim_type)
                            + " OR PMB:"       + STRING(btt_auth.pmb_indicator) + "<>" + STRING(buf_auth.pmb_indicator).

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  IF btt_auth.auth_obj <= 0
  OR AVAILABLE buf_auth
  AND (btt_auth.auth_status   <> buf_auth.auth_status
  OR   btt_auth.amount_auth   <> buf_auth.amount_auth
  OR   btt_auth.quantity_auth <> buf_auth.quantity_auth
  OR   btt_auth.start_date    <> buf_auth.start_date
  OR   btt_auth.claim_code    <> buf_auth.claim_code
  OR   btt_auth.claim_type    <> buf_auth.claim_type
  OR   btt_auth.pmb_indicator <> buf_auth.pmb_indicator) THEN
  DO:
    IF btt_auth.amount_auth <> 0
    THEN
      ASSIGN dLimitAmount = btt_auth.amount_auth.
    ELSE
      ASSIGN dLimitAmount = 9999999.99.

    IF btt_auth.quantity_auth <> 0
    THEN
      ASSIGN dLimitQuantity = btt_auth.quantity_auth.
    ELSE
      ASSIGN dLimitQuantity = 1.

    ASSIGN cTrackingMessage = "Checklimits - RUN checkLimits with:"
                            + " Option="       + STRING(btt_auth.option_code)   + " Member="         + STRING(btt_auth.mem_num)
                            + " Dep="          + STRING(btt_auth.dependant)     + " StartDate="      + STRING(btt_auth.start_date,"9999/99/99")
                            + " ClaimCode="    + STRING(iClaimCode)             + " ClaimType="      + btt_auth.claim_type
                            + " dLimitAmount=" + STRING(dLimitAmount)           + " dLimitQuantity=" + STRING(dLimitQuantity)
                            + " PBMind="       + STRING(btt_auth.pmb_indicator) + " AuthLevel=Header".

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    mipEnv:Health:AuthService:checkLimits(INPUT  btt_auth.insurer_obj,     // ipdInsurerObj
                                          INPUT  btt_auth.option_code,     // ipiOptionCode
                                          INPUT  btt_auth.mem_num,         // ipcMemNum
                                          INPUT  btt_auth.dependant,       // ipiDependant
                                          INPUT  btt_auth.start_date,      // ipdDate
                                          INPUT  iClaimCode,               // ipiClaimCode
                                          INPUT  btt_auth.claim_type,      // ipcClaimType
                                          INPUT  dLimitAmount,             // ipdAllowAmount
                                          INPUT  dLimitQuantity,           // ipdQuantity
                                          INPUT  btt_auth.pmb_indicator,   // iplPMBIndicator
                                          INPUT  "Header":U,               // ipcAuthLevel
                                          INPUT  0,                        // Auth header obj
                                          INPUT  0,                        // Auth providerder obj
                                          OUTPUT dAllowAmount,             // Authorised amount allowed after Limit checking
                                          OUTPUT dQuantity,                // Authorised quantity allowed after Limit checking
                                          OUTPUT dPMBValue,                // PMB Value after Limit checking
                                          OUTPUT iAuthStatus,              // Authorised status allowed after Limit checking
                                          OUTPUT cAuthStatusNote,          // Authorised status note after Limit checking
                                          OUTPUT cClaimType,               // Claim type after Limit checking
                                          OUTPUT cErrorMessage,            // Any errors returned from Limit checking
                                          OUTPUT lLimitsChecked,           // Indicates if limit checking did apply
                                          OUTPUT TABLE tt_limitwarn).      // Temp table with limit warnings after Limit checking
    IF iAuthStatus = ?
    THEN ASSIGN cAuthStatus = "Unknown".
    ELSE ASSIGN cAuthStatus = STRING(iAuthStatus).

    ASSIGN cTrackingMessage = "Checklimits - RETURN from checkLimits:"
                            + " dAllowAmount="    + STRING(dAllowAmount)   + " dQuantity="     + STRING(dQuantity)
                            + " dPMBValue="       + STRING(dPMBValue)      + " iAuthStatus="   + cAuthStatus
                            + " cAuthStatusNote=" + cAuthStatusNote        + " cClaimType="    + cClaimType
                            + " lLimitsChecked="  + STRING(lLimitsChecked) + " cErrorMessage=" + cErrorMessage.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /*
      If any errors are returned, display it and don't save the Authorisation.
    */
    IF cErrorMessage <> "":U THEN
    DO:
      goErrorObject:addError(INPUT "hatau:":U,            // ipcOwningEntityMnemonic
                             INPUT btt_auth.auth_obj,     // ipdOwningEntityObj
                             INPUT "":U,                  // ipcOwningEntityKey
                             INPUT "amount_auth":U,       // ipcFieldName
                             INPUT btt_auth.line_number,  // ipiLineNumber
                             INPUT cErrorMessage,         // ipcMessageText
                             INPUT "ERR":U).
    END.  /*  IF cErrorMessage <> "":U THEN */
    ELSE DO:
      /*
        No errors returned, method checkLimits indicates Limits were applied.
        Now apply further validations and amount/quantity reductions.
      */
      IF lLimitsChecked THEN
      DO:
        /*
          Display any limit warnings returned as Warning Acknowledgements.
        */
        FOR EACH tt_limitwarn
          BY limit_sequence:

          goErrorObject:addError
                           (INPUT 'hatau':U,                   // ipcOwningEntityMnemonic
                            INPUT btt_auth.auth_obj,           // ipdOwningEntityObj
                            INPUT "":U,                        // ipcOwningEntityKey
                            INPUT "amount_auth":U,             // FieldName
                            INPUT btt_auth.line_number,        // ipiLineNumber
                            INPUT tt_limitwarn.limit_warning,  // ipcMessageText
                            INPUT "WAR":U,                     // ErrorType
                            INPUT TRUE).                       // Warnack/Warn
        END.  /* FOR EACH tt_limitwarn */

        /*
          Decline header if authorisation status is declined with limit checking and assign claim code to 99.
        */
        IF  iAuthStatus  = 6 // Declined
        AND iAuthStatus <> btt_auth.auth_status
        THEN
          ASSIGN btt_auth.auth_status      = iAuthStatus
                 btt_auth.auth_status_note = cAuthStatusNote
                 btt_auth.claim_code       = buf_auth_schext.claim-code[1].
        ELSE DO:
          /*
            Reduce the authorised quantity and amount if the quantity passed in was reduced with limit checking.
          */
          IF  dQuantity       <  dLimitQuantity // Output quantity < header value passed in
          AND cAuthStatusNote <> "":U           // If authorised value is reduced, a reason must be assigned
          THEN
            ASSIGN btt_auth.amount_auth   = IF btt_auth.amount_auth <> 0
                                            THEN dQuantity * (btt_auth.amount_auth / btt_auth.quantity_auth)
                                            ELSE btt_auth.amount_auth
                   btt_auth.quantity_auth = dQuantity.

          /*
            Reduce authorised amount if amount was reduced with the limit checking.
          */
          IF  dAllowAmount    <  dLimitAmount   // Output amount is less than authorised value passed in.
          AND cAuthStatusNote <> ""             // If authorised value is reduced a reason must be assigned.
          THEN
            ASSIGN btt_auth.amount_auth = dAllowAmount.

          /*
            Assign default status note if a value is passed back from the limit checking.
          */
          IF cAuthStatusNote <> btt_auth.auth_status_note
          THEN
            ASSIGN btt_auth.auth_status_note = cAuthStatusNote.

          /*
            Assign values that will apply for PMB.
          */
          IF dPMBValue <> btt_auth.pmb_value
          THEN
            ASSIGN btt_auth.pmb_value = dPMBValue.

          IF  cClaimType <> btt_auth.claim_type
          AND cClaimType <> "":U
          THEN
            ASSIGN btt_auth.claim_type = cClaimType.

        END. /* ELSE - IF iAuthStatus = 6 AND iAuthStatus <> btt_auth.auth_status THEN */

        ASSIGN cTrackingMessage = "Checklimits - After checkLimits applied:"
                                + " AuthStatus=" + STRING(btt_auth.auth_status) + " StatusNote=" + STRING(btt_auth.auth_status_note)
                                + " AmountAuth=" + STRING(btt_auth.amount_auth) + " QtyAuth="    + STRING(btt_auth.quantity_auth)
                                + " ClaimCode="  + STRING(iClaimcode)           + " ClaimType="  + STRING(btt_auth.claim_type)
                                + " PMBvalue="   + STRING(btt_auth.pmb_value).

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      END.  // IF oplLimitsChecked THEN
    END.  // ELSE - IF cErrorMessage <> "":U THEN
  END.  // IF btt_auth.auth_obj <= 0 OR OR AVAILABLE buf_auth AND...

  IF NOT lLimitsChecked THEN
  DO:
    ASSIGN cTrackingMessage = "Checklimits - NO limit checking done:"
                            + " AuthObj=" + STRING(btt_auth.auth_obj) + " Option="    + STRING(btt_auth.option_code)
                            + " Member="  + STRING(btt_auth.mem_num)  + " Dependant=" + STRING(btt_auth.dependant).
    IF AVAILABLE buf_auth
    THEN
      ASSIGN cTrackingMessage = cTrackingMessage
                              + " PrevStatus="    + STRING(buf_auth.auth_status)   + " NewStatus="    + STRING(btt_auth.auth_status)
                              + " PrevAmount="    + STRING(buf_auth.amount_auth)   + " NewAmount="    + STRING(btt_auth.amount_auth)
                              + " PrevQuantity="  + STRING(buf_auth.quantity_auth) + " NewQuantity="  + STRING(btt_auth.quantity_auth)
                              + " PrevStartDate=" + STRING(buf_auth.start_date)    + " NewStartDate=" + STRING(btt_auth.start_date)
                              + " PrevClaimCode=" + STRING(buf_auth.claim_code)    + " NewClaimCode=" + STRING(iClaimCode)
                              + " PrevClaimType=" + STRING(buf_auth.claim_type)    + " NewClaimType=" + STRING(btt_auth.claim_type)
                              + " PrevPMBInd="    + STRING(buf_auth.pmb_indicator) + " NewPMBInd="    + STRING(btt_auth.pmb_indicator).

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  END.  // IF NOT lLimitsChecked THEN
END.  // IF btt_auth.auth_status = 1 AND...
ELSE DO:
  ASSIGN cTrackingMessage = "Checklimits - Header Limits NOT checked because "
                          + " Status:"      + STRING(btt_auth.auth_status)   + "<>1 OR ("
                          +  "Auth Amount:" + STRING(btt_auth.amount_auth)   + "=0 AND"
                          + " Auth Qty:"    + STRING(btt_auth.quantity_auth) + "=0) OR"
                          + " Dependant:"   + STRING(btt_auth.dependant)     + "=99".

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
END.  // ELSE - IF btt_auth.auth_status = 1 AND...
