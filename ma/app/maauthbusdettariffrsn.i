/* maauthbusdettariffrsn.i MEDSTAR Medical Aid System
                           Tariff reason business logic on auth clinical detail line
                           (c) Copyright 2021
                           MIP Holdings (Pty) Ltd
                           All rights reserved
*/

IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U
AND btt_auth_detail.auth_status <> 6 THEN
DO:
  IF btt_auth_detail.auth_detail_obj <= 0
  OR (AVAILABLE buf_auth_detail 
  AND  (buf_auth_detail.start_date <> btt_auth_detail.start_date
  OR    buf_auth_detail.reason     <> btt_auth_detail.reason))
  OR lOverrideBaseARSRate THEN 
  DO:
    FIND FIRST trfcostrsn NO-LOCK
      WHERE trfcostrsn.base-rate   = tariff.base-rate
      AND   trfcostrsn.ars-rate    = tariff.ars-rate
      AND   trfcostrsn.effect-date = tariff.effect-date
      AND   trfcostrsn.tariff-code = tariff.tariff-code
      AND   trfcostrsn.pr-type     = tariff.pr-type
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE trfcostrsn THEN
    DO:
      IF btt_auth_detail.reason = "" THEN
      DO:
        ASSIGN cError = "Reason field is mandatory".

        /*
          Include the tariff cost reason question in the error message
          as long as the question does not include the words "reason" 
          or "question".
        */
        IF trfcostrsn.question <> "":U
        AND INDEX(trfcostrsn.question,"reason":U)   = 0 
        AND INDEX(trfcostrsn.question,"question":U) = 0 THEN
          ASSIGN cError = cError + " - " + trfcostrsn.question.

        goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
                          INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                          INPUT "":U,                                               /* ipcOwningEntityKey      */
                          INPUT "reason":U,                                         /* FieldName               */ 
                          INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                          INPUT cError,                                             /* ipcMessageText          */ 
                          INPUT "Err").                                             /* ErrorType               */
      END.  /* IF btt_auth_detail.reason = "" THEN */
      ELSE DO:
        IF trfcostrsn.min-length <> 0
        AND LENGTH(btt_auth_detail.reason) < trfcostrsn.min-length THEN
        DO:
          ASSIGN cError = "Reason field is mandatory with a minimum length of " + STRING(trfcostrsn.min-length).
          goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
                          INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                          INPUT "":U,                                               /* ipcOwningEntityKey      */
                          INPUT "reason":U,                                         /* FieldName               */ 
                          INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                          INPUT cError,                                             /* ipcMessageText          */ 
                          INPUT "Err").                                             /* ErrorType               */
        END.  /* IF trfcostrsn.min-length <> 0 AND... */
      END.  /* ELSE - IF btt_auth_detail.reason = "" THEN */
    END.  /* IF AVAILABLE trfcostrsn THEN */
  END.  /* IF btt_auth_detail.auth_detail_obj <= 0 OR... */
END.  /* IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U AND... */
