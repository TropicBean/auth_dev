/* maauthcopayservalcontrol.i MEDSTAR Medical Aid System
                              Validate Auth Copay Control
                              (c) Copyright 2021 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
   This include is for the _validateAuthCopayControl procedure in maauthcopayservicestack.p
*/      
                
  DEFINE PARAMETER BUFFER btt_auth_copay_control   FOR tt_auth_copay_control.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL.

&IF {&DBDFMA} >= 10195 &THEN
  

  DEFINE VARIABLE cStatusDescr        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOverrideReason     AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValidMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE iEntry              AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lMandatory          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidStatus        AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.  
  
  DEFINE BUFFER buf_auth_copay_control FOR hac_auth_copay_control.
  DEFINE BUFFER buf_auth_copay_detail  FOR hac_auth_copay_detail.
  DEFINE BUFFER erm_insurer            FOR erm_insurer.
  
  /* 
    Make sure we have a valid buffer before we go any further 
  */
  IF NOT AVAILABLE btt_auth_copay_control
  THEN 
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Copay as no buffer is available.'" }
    
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).

  IF btt_auth_copay_control.auth_copay_control_obj > 0 THEN
  DO:
    FIND buf_auth_copay_control NO-LOCK
      WHERE buf_auth_copay_control.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj
    NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
  END.  // IF btt_auth_copay_control.auth_copay_control_obj > 0 THEN
  
  /* 
    Ensure that a valid insurer has been specified 
  */
  IF  btt_auth_copay_control.insurer_obj <> 0.00
  AND btt_auth_copay_control.insurer_obj <> ? THEN
  DO:

    FIND FIRST erm_insurer NO-LOCK
         WHERE erm_insurer.insurer_obj = btt_auth_copay_control.insurer_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "insurer_obj":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Copay Client: ":U + STRING(btt_auth_copay_control.insurer_obj)).
    END. // IF NOT AVAILABLE erm_insurer THEN
  END. // IF btt_auth_copay_control.insurer_obj <> 0.00 AND btt_auth_copay_control.insurer_obj <> ?

  IF btt_auth_copay_control.provider_type <> "":U THEN
  DO:
    IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key = "ma_acAuthProviderType":U
         AND mic_acronym.acronym_code = btt_auth_copay_control.provider_type) 
    THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "provider_type":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Copay Control Provider Type: ":U + STRING(btt_auth_copay_control.option_code)).
    END.  // IF NOT AVAILABLE scheme THEN
  END. /* IF btt_auth_copay_control.provider_type <> "":U THEN */

  /* If a scheme has been specified, make sure it exists */
  IF btt_auth_copay_control.option_code = ?
  THEN
    ASSIGN btt_auth_copay_control.option_code = 0.

  IF btt_auth_copay_control.option_code <> 0 THEN
  DO:
    IF NOT CAN-FIND(FIRST scheme NO-LOCK
         WHERE scheme.scheme-code = btt_auth_copay_control.option_code
         AND   scheme.active      = TRUE) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "option_code":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Copay Control Scheme Option: ":U + STRING(btt_auth_copay_control.option_code)).
    END.  // IF NOT AVAILABLE scheme THEN
  END. // IF btt_auth_copay_control.option_code <> 0 THEN
  
  /* 
    Ensure that a valid Auth Copay Type has been specified 
  */
  IF btt_auth_copay_control.auth_copay_type_obj = 0
  OR btt_auth_copay_control.auth_copay_type_obj = ? THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "auth_copay_type_obj":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Type,":U ).
  END. // btt_auth_copay_control.auth_copay_type_obj = "":U THEN
  ELSE DO:
    IF NOT CAN-FIND(FIRST ham_auth_copay_type
                    WHERE ham_auth_copay_type.auth_copay_type_obj = btt_auth_copay_control.auth_copay_type_obj) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "auth_copay_type_obj":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Copay Type":U ).
    END.  // IF NOT CAN-FIND(FIRST ham_auth_copay_type
  END.  // ELSE - IF btt_auth_copay_control.auth_copay_type_obj = 0

  /*
    Ensure that a valid Auth Copay control effective date has been specified
  */
  IF btt_auth_copay_control.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Effective Date,":U) .
  END. // btt_auth_copay_control.effective_date = ? THEN
  ELSE DO:
    IF AVAILABLE buf_auth_copay_control
    AND CAN-FIND(FIRST buf_auth_copay_detail NO-LOCK
                WHERE buf_auth_copay_detail.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj
                AND   buf_auth_copay_detail.effective_date         < btt_auth_copay_control.effective_date
                AND   buf_auth_copay_detail.effective_date        <> buf_auth_copay_control.effective_date) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "effective_date":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT "MA":U,
                            INPUT 112,  /* The &1 specified is invalid. &2 */
                            INPUT "Auth Copay Effective Date,Some Detail lines will fall outside the specified period.":U).
    END.  // IF CAN-FIND(FIRST buf_auth_copay_detail
  END.  // ELSE - IF btt_auth_copay_control.effective_date = ? THEN

  /*
    If an end date is supplied, ensure that it is not before the effective date
  */
  IF btt_auth_copay_control.end_date <> ? THEN
  DO:
    IF btt_auth_copay_control.end_date < btt_auth_copay_control.effective_date THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage      = SUBSTITUTE("The End Date (&1) cannot be before the Effective/Start Date (&2)",
                                             STRING(btt_auth_copay_control.end_date,"9999/99/99"),
                                             STRING(btt_auth_copay_control.effective_date,"9999/99/99")).

      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "end_date":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END.  // IF btt_auth_copay_control.end_date < btt_auth_copay_control.effective_date THEN
    ELSE DO:
      /*
        Check if there are any detail lines with end dates that will fall outside the control's period.
        Note that details lines with end dates that are the same as the control's old end date, will
        automatically be updated with the new end date in the _setAuthCopayControlDates procedure.
      */
      IF AVAILABLE buf_auth_copay_control 
      AND btt_auth_copay_control.end_date <> ? THEN
      DO:
        IF CAN-FIND(FIRST buf_auth_copay_detail NO-LOCK
          WHERE buf_auth_copay_detail.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj
          AND   buf_auth_copay_detail.end_date              <> buf_auth_copay_control.end_date
          AND  (buf_auth_copay_detail.end_date               > btt_auth_copay_control.end_date
          OR    buf_auth_copay_detail.end_date               = ?)) THEN
        DO:
          ASSIGN oplFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "haccc":U,
                                INPUT btt_auth_copay_control.auth_copay_control_obj,
                                INPUT "":U,
                                INPUT "end_date":U,
                                INPUT btt_auth_copay_control.line_number,
                                INPUT "MA":U,
                                INPUT 112,  /* The &1 specified is invalid. &2 */
                                INPUT "Auth Copay End Date,Some Detail lines will fall outside the specified period.":U).
        END.  // IF CAN-FIND(FIRST buf_auth_copay_detail NO-LOCK
      END.  // IF AVAILABLE buf_auth_copay_control THEN
    END.  // ELSE - IF btt_auth_copay_control.end_date < btt_auth_copay_control.effective_date THEN
  END.  // IF btt_auth_copay_control.end_date <> ? THEN

  /*First check for overlapping Effective Dates and End Dates */     
  IF CAN-FIND(FIRST buf_auth_copay_control NO-LOCK
              WHERE buf_auth_copay_control.auth_copay_control_obj <> btt_auth_copay_control.auth_copay_control_obj
                AND buf_auth_copay_control.insurer_obj            = btt_auth_copay_control.insurer_obj
                AND buf_auth_copay_control.option_code            = btt_auth_copay_control.option_code
                AND buf_auth_copay_control.provider_type          = btt_auth_copay_control.provider_type
                AND buf_auth_copay_control.effective_date         < btt_auth_copay_control.effective_date
                AND (   buf_auth_copay_control.end_date            = ?
                     OR buf_auth_copay_control.end_date            > btt_auth_copay_control.effective_date)) THEN 
   DO: 
    ASSIGN 
      oplFailureOccurred = TRUE
      cValidMessage = "Overlapping effective dates and end dates found".
    
    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "btt_auth_coapy_control.effective_date",
                          INPUT btt_auth_copay_control.line_number,
                          INPUT cValidMessage,
                          INPUT "ERR":U).
  END. // IF AVAILABLE buf_auth_copay_control
  
  /*Final check for overlapping Effective Dates and End Dates */    
  IF CAN-FIND(FIRST buf_auth_copay_control NO-LOCK
              WHERE buf_auth_copay_control.auth_copay_control_obj <> btt_auth_copay_control.auth_copay_control_obj
                AND buf_auth_copay_control.insurer_obj            = btt_auth_copay_control.insurer_obj
                AND buf_auth_copay_control.option_code            = btt_auth_copay_control.option_code
                AND buf_auth_copay_control.provider_type          = btt_auth_copay_control.provider_type
                AND buf_auth_copay_control.effective_date         > btt_auth_copay_control.effective_date
                AND buf_auth_copay_control.effective_date         < btt_auth_copay_control.end_date
              )
  AND btt_auth_copay_control.end_date <> ?
  OR (btt_auth_copay_control.end_date = ? 
      AND 
      CAN-FIND(FIRST buf_auth_copay_control NO-LOCK
              WHERE buf_auth_copay_control.auth_copay_control_obj <> btt_auth_copay_control.auth_copay_control_obj
                AND buf_auth_copay_control.insurer_obj            = btt_auth_copay_control.insurer_obj
                AND buf_auth_copay_control.option_code            = btt_auth_copay_control.option_code
                AND buf_auth_copay_control.provider_type          = btt_auth_copay_control.provider_type
                AND buf_auth_copay_control.effective_date         > btt_auth_copay_control.effective_date)) THEN 
  DO: 
    ASSIGN 
      oplFailureOccurred = TRUE 
      cValidMessage = "A record already exists with overlapping effective and end dates".
      MESSAGE "33333333333333333333333333333333333333".
    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "btt_auth_coapy_control.end_date",
                          INPUT btt_auth_copay_control.line_number,
                          INPUT cValidMessage,
                          INPUT "ERR":U).
  END. // IF AVAILABLE buf_auth_copay_control
  
  /* Check that the End Dates are not overlapping */
  IF btt_auth_copay_control.end_date <> ?
  AND CAN-FIND(FIRST buf_auth_copay_control NO-LOCK
           WHERE buf_auth_copay_control.auth_copay_control_obj <> btt_auth_copay_control.auth_copay_control_obj
             AND buf_auth_copay_control.insurer_obj            = btt_auth_copay_control.insurer_obj
             AND buf_auth_copay_control.option_code            = btt_auth_copay_control.option_code
             AND buf_auth_copay_control.provider_type          = btt_auth_copay_control.provider_type
             AND buf_auth_copay_control.effective_date         > btt_auth_copay_control.end_date
             AND buf_auth_copay_control.end_date               < btt_auth_copay_control.end_date) THEN 
  DO: 
    ASSIGN 
      oplFailureOccurred = TRUE
      cValidMessage = "Overlapping end dates found".
    
    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "btt_auth_coapy_control.end_date",
                          INPUT btt_auth_copay_control.line_number,
                          INPUT cValidMessage,
                          INPUT "ERR":U).
  END. // IF AVAILABLE buf_auth_copay_control

  /*
    Ensure that a valid Auth Copay value type has been specified
  */
  IF btt_auth_copay_control.copayment_value_type = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "copayment_value_type":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Value Type,":U ).
  END. // btt_auth_copay_control.copayment_value_type = "":U THEN

  /* If a copay value type has been specified, make sure the copayment value is entered */
  IF btt_auth_copay_control.copayment_value = 0
  OR btt_auth_copay_control.copayment_value = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "copayment_value":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Copayment Value,":U).
  END. // ELSE IF btt_auth_copay_control.copayment_value = 0

  IF btt_auth_copay_control.copayment_value < 0  THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE
           cValidMessage      = "The Auth Copay Value specified is invalid. Copayment value can not be less than 0.".
         
    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "copayment_value":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT cValidMessage,
                          INPUT "ERR":U).
  END. /*IF btt_auth_copay_control.copayment_value < 0  THEN*/

  IF btt_auth_copay_control.copayment_value_type THEN
  DO:
    IF btt_auth_copay_control.copayment_value > 100 
    OR btt_auth_copay_control.copayment_value < 0  THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage      = "The Auth Copay Value specified is invalid. Copayment value can not be less than 0 or greater than 100.00 when a value type of ~"Percentage~" is selected.".
           
      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "copayment_value":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    
    END. /*IF tt_auth_copay_control.copayment_value > 100 OR tt_auth_copay_control.copayment_value < 0  THEN*/
  END. /*IF tt_auth_copay_control.copayment_value_type THEN*/

  /*
    Ensure that a valid apply to PMB has been specified
  */
  IF btt_auth_copay_control.apply_to_pmb = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "apply_to_pmb":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Apply to PMB,":U ).
  END. // IF btt_auth_copay_control.apply_to_pmb = ? THEN

  /*
    Ensure that a valid apply to Emergency has been specified
  */
  IF btt_auth_copay_control.apply_to_emergency = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "apply_to_emergency":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Apply to Emergency,":U ).
  END. // IF btt_auth_copay_control.apply_to_emergency = ? THEN

  /* The authorisation status that will be assigned by default to the provider if a co-payment should apply.
     Valid status values will be setup on auth rule 'CopayValidStatuses'. */
  /* Ensure that a valid auth status is entered */
  IF btt_auth_copay_control.auth_status = ? THEN
  DO:
     ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,
                          INPUT btt_auth_copay_control.auth_copay_control_obj,
                          INPUT "":U,
                          INPUT "auth_status":U,
                          INPUT btt_auth_copay_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Status,":U ).
  END. // IF btt_auth_copay_control.auth_status = ?
  ELSE DO:
    RUN validateCopayAuthStatus(INPUT btt_auth_copay_control.auth_status,
                                OUTPUT lValidStatus).
  
    IF NOT lValidStatus THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage = SUBSTITUTE("The Status Code (&1) specified is invalid. See valid statuses set up in auth rule 'CopayValidStatuses'.",
                                        STRING(btt_auth_copay_control.auth_status)).
  
      oErrorObject:addError(INPUT "haccc":U,
                            INPUT btt_auth_copay_control.auth_copay_control_obj,
                            INPUT "":U,
                            INPUT "auth_status":U,
                            INPUT btt_auth_copay_control.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END.  // IF NOT lValidStatus THEN

    /*
       Check if the status reason/note on the auth copay is mandatory
    */
    ELSE IF btt_auth_copay_control.auth_status_note = ""
         OR btt_auth_copay_control.auth_status_note = ? THEN
    DO:
      ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  btt_auth_copay_control.auth_status,
                                                                          INPUT  btt_auth_copay_control.insurer_obj,
                                                                          INPUT  btt_auth_copay_control.option_code,
                                                                          INPUT  btt_auth_copay_control.effective_date).
  
      IF lMandatory THEN
      DO:
        ASSIGN 
          cStatusDescr       = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                              INPUT btt_auth_copay_control.auth_status)
          cValidMessage      = "Status Reason,The reason is mandatory for status '":U 
                             + cStatusDescr 
                             + "'.[HELP=Auth Rule Code: EnforceStatusNote]":U
          oplFailureOccurred = TRUE.
  
        oErrorObject:addError
                     (INPUT "haccc":U,                                     /* ipcOwningEntityMnemonic  */
                      INPUT btt_auth_copay_control.auth_copay_control_obj, /* ipdOwningEntityObj       */
                      INPUT "":U,                                          /* ipcOwningEntityKey       */
                      INPUT "auth_status_note":U,                          /* ipcFieldName             */
                      INPUT btt_auth_copay_control.line_number,            /* ipiLineNumber            */
                      INPUT "MA":U,                                        /* ipcMessageGroup          */
                      INPUT 111,  /* The &1 must be specified. &2 */       /* ipiMessageNumber         */
                      INPUT cValidMessage).                                /* ipcReplaceTextList       */
      END.  // IF lMandatory THEN
    END.  // ELSE IF btt_auth_copay_control.auth_status_note = "" THEN
    /*
      Validate status reason if specified
    */
    ELSE DO:
      mipEnv:Health:AuthService:ValidateStatusReason(INPUT  btt_auth_copay_control.insurer_obj,
                                                     INPUT  btt_auth_copay_control.option_code,
                                                     INPUT  btt_auth_copay_control.auth_status_note,
                                                     INPUT  INTEGER(btt_auth_copay_control.auth_status),
                                                     INPUT  btt_auth_copay_control.effective_date,
                                                     OUTPUT cValidMessage).
  
      IF cValidMessage <> "":U THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
  
        oErrorObject:addError(INPUT "haccc":U,
                              INPUT btt_auth_copay_control.auth_copay_control_obj,
                              INPUT "":U,
                              INPUT "auth_status_note":U,
                              INPUT btt_auth_copay_control.line_number,
                              INPUT cValidMessage,
                              INPUT "ERR":U).
      END. // IF cValidMessage <> "":U THEN
    END. // ELSE - IF btt_auth_copay_control.auth_status_note = "":U THEN
  END. // ELSE - IF btt_auth_copay_control.auth_status = ?

  IF btt_auth_copay_control.copay_apply_override_reasons = "":U 
  OR btt_auth_copay_control.copay_apply_override_reasons = ? THEN
  DO:
     ASSIGN oplFailureOccurred = TRUE.

     oErrorObject:addError(INPUT "haccc":U,                                          /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_copay_control.auth_copay_control_obj,      /* ipdOwningEntityObj      */
                           INPUT "":U,                                               /* ipcOwningEntityKey      */
                           INPUT "copay_apply_override_reasons":U,                   /* ipcFieldName            */
                           INPUT btt_auth_copay_control.line_number,                 /* ipiLineNumber           */
                           INPUT "MA":U,                                             /* ipcMessageGroup         */
                           INPUT 111,  /* The &1 must be specified. &2 */            /* ipiMessageNumber        */
                           INPUT "Co-payment Override Reasons,").                    /* ipcReplaceTextList      */
  END.  // IF btt_auth_copay_control.copay_apply_override_reasons = "":U THEN
  ELSE DO:
    DO iEntry = 1 TO NUM-ENTRIES(btt_auth_copay_control.copay_apply_override_reasons):
      ASSIGN cOverrideReason = ENTRY(iEntry,btt_auth_copay_control.copay_apply_override_reasons).

      IF NOT CAN-FIND(FIRST note NO-LOCK
          WHERE note.scheme-code = 0
          AND   note.type        = "AO":U
          AND   note.key         = cOverrideReason) THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.

        oErrorObject:addError(INPUT "haccc":U,                                          /* ipcOwningEntityMnemonic */
                              INPUT btt_auth_copay_control.auth_copay_control_obj,      /* ipdOwningEntityObj      */
                              INPUT "":U,                                               /* ipcOwningEntityKey      */
                              INPUT "copay_apply_override_reasons":U,                   /* ipcFieldName            */
                              INPUT btt_auth_copay_control.line_number,                 /* ipiLineNumber           */
                              INPUT "MA":U,                                             /* ipcMessageGroup         */
                              INPUT 100,  /* The "&1" specified is invalid */           /* ipiMessageNumber        */
                              INPUT "Co-payment Override Reason (" +
                                    cOverrideReason + ")").                             /* ipcReplaceTextList      */
      END.  // IF NOT CAN-FIND(FIRST note
    END.  // DO iEntry = 1 TO NUM-ENTRIES(btt_auth_copay_control.copay_apply_override_reasons)
  END.  // ELSE - IF btt_auth_copay_control.copay_apply_override_reasons = "":U THEN

  IF btt_auth_copay_control.warning_message = ?
  THEN
    ASSIGN btt_auth_copay_control.warning_message = "".

  IF btt_auth_copay_control.warning_message_type = ?
  THEN
    ASSIGN btt_auth_copay_control.warning_message_type = "".

  IF  btt_auth_copay_control.warning_message     <> ""
  AND btt_auth_copay_control.warning_message_type = "" THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,                                          /* ipcOwningEntityMnemonic */
                          INPUT btt_auth_copay_control.auth_copay_control_obj,      /* ipdOwningEntityObj      */
                          INPUT "":U,                                               /* ipcOwningEntityKey      */
                          INPUT "warning_message_type":U,                           /* ipcFieldName            */
                          INPUT btt_auth_copay_control.line_number,                 /* ipiLineNumber           */
                          INPUT "MA":U,                                             /* ipcMessageGroup         */
                          INPUT 111,  /* The &1 must be specified. &2 */            /* ipiMessageNumber        */
                          INPUT "Warning Message Type,Warning Message").            /* ipcReplaceTextList      */
  END.  // IF btt_auth_copay_control.warning_message <> "" AND (btt_auth_copay_control.warning_message_type = ""

  IF  btt_auth_copay_control.warning_message       = ""
  AND btt_auth_copay_control.warning_message_type <> "" THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccc":U,                                          /* ipcOwningEntityMnemonic */
                          INPUT btt_auth_copay_control.auth_copay_control_obj,      /* ipdOwningEntityObj      */
                          INPUT "":U,                                               /* ipcOwningEntityKey      */
                          INPUT "warning_message_type":U,                           /* ipcFieldName            */
                          INPUT btt_auth_copay_control.line_number,                 /* ipiLineNumber           */
                          INPUT "MA":U,                                             /* ipcMessageGroup         */
                          INPUT 111,  /* The &1 must be specified. &2 */            /* ipiMessageNumber        */
                          INPUT "Warning Message,Warning Message Type").            /* ipcReplaceTextList      */
  END.  // IF btt_auth_copay_control.warning_message <> "" AND (btt_auth_copay_control.warning_message_type = ""

  IF btt_auth_copay_control.warning_message_type <> "" THEN
  DO:
    IF NOT btt_auth_copay_control.warning_message_type BEGINS "ma_acAuthCopayWarnMessageType":U 
    THEN
      ASSIGN btt_auth_copay_control.warning_message_type = "ma_acAuthCopayWarnMessageType":U + btt_auth_copay_control.warning_message_type.

    IF NOT CAN-FIND(FIRST mic_acronym
                    WHERE mic_acronym.category_key = "ma_acAuthCopayWarnMessageType":U
                    AND   mic_acronym.acronym_key  = btt_auth_copay_control.warning_message_type) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "haccc":U,                                          /* ipcOwningEntityMnemonic */
                            INPUT btt_auth_copay_control.auth_copay_control_obj,      /* ipdOwningEntityObj      */
                            INPUT "":U,                                               /* ipcOwningEntityKey      */
                            INPUT "warning_message_type":U,                           /* ipcFieldName            */
                            INPUT btt_auth_copay_control.line_number,                 /* ipiLineNumber           */
                            INPUT "MA":U,                                             /* ipcMessageGroup         */
                            INPUT 100,  /* The "&1" specified is invalid */           /* ipiMessageNumber        */
                            INPUT "Warning Message Type (" +
                                  btt_auth_copay_control.warning_message_type + ")"). /* ipcReplaceTextList      */
    END.  // IF NOT CAN-FIND(FIRST mic_acronym
  END.  // IF  btt_auth_copay_control.warning_message_type <> 0

  { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
        
&ENDIF

