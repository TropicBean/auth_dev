/* maauthtypeservalauthtype.i  MEDSTAR Medical Aid System
                               Validate Auth type Buffer
                               (c) Copyright 2018 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/      
                
  DEFINE PARAMETER BUFFER btt_auth_type FOR tt_auth_type.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL.
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cKeylist            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cCodelist           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cDescriptionList    AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cLabellist          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cSequenceList       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatusDescr        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRuleValue          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValueList          AS CHARACTER           NO-UNDO.

  DEFINE VARIABLE iCnt                AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lMandatory          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidRule          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.
  
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider.
  DEFINE BUFFER buf_condition          FOR condition.
  DEFINE BUFFER buf_condtype           FOR condtype.
  DEFINE BUFFER buf_note               FOR note.
  DEFINE BUFFER buf_auth_rule          FOR hac_auth_rule.
  DEFINE BUFFER buf_sequence           FOR mic_sequence.
  DEFINE BUFFER buf_auth_type_control  FOR hac_auth_type_control.
  
  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_type THEN
  DO:
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Type as no buffer is available.'" }
  END.  // IF NOT AVAILABLE btt_auth_type THEN
    
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE) .
  
  /* Ensure that a valid auth type has been specified */
  IF btt_auth_type.auth_type = "":U THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "auth_type":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 13,  /* The &1 must be specified. */
                          INPUT "Auth Type":U ).
  END. // btt_auth_type.auth_type = "":U THEN
  
  /* Ensure that a valid insurer has been specified */
  IF  btt_auth_type.insurer_obj <> 0.00 
  AND btt_auth_type.insurer_obj <> ? THEN 
  DO:
  
    FIND FIRST erm_insurer NO-LOCK
         WHERE erm_insurer.insurer_obj = btt_auth_type.insurer_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "":U,
                            INPUT "insurer_obj":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Type Client:":U + STRING(btt_auth_type.insurer_obj)).
    END. // IF NOT AVAILABLE erm_insurer THEN
  END. // IF btt_auth_type.insurer_obj <> 0.00 AND btt_auth_type.insurer_obj <> ?
  
  /* Ensure that a valid auth type description has been specified */
  IF btt_auth_type.description = "":U THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "description":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 13,  /* The &1 must be specified. */
                          INPUT "Auth Type Description":U ).
  END. /* btt_auth_type.description = "":U THEN */
  
  /* Validate comma delimited list of users */
  IF fnValidateUsers (INPUT btt_auth_type.claim_code_upd_user,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "claim_code_upd_user":U, INPUT btt_auth_type.line_number, INPUT "Claim Code Update User":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  IF fnValidateUsers (INPUT btt_auth_type.claim_type_upd_user,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "claim_type_upd_user":U, INPUT btt_auth_type.line_number, INPUT "Claim Type Update User":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  IF fnValidateUsers (INPUT btt_auth_type.default_auth_status_upd_user,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "default_auth_status_upd_user":U, INPUT btt_auth_type.line_number, INPUT "Default Status Update User":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  IF fnValidateUsers (INPUT btt_auth_type.end_date_upd_user,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "end_date_upd_user":U, INPUT btt_auth_type.line_number, INPUT "End Date Update User":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
    
  /* Validate comma delimited list of roles */
  IF fnValidateRoles (INPUT btt_auth_type.claim_code_upd_role,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "claim_code_upd_role":U, INPUT btt_auth_type.line_number, INPUT "Claim Code Update Role":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  IF fnValidateRoles (INPUT btt_auth_type.claim_type_upd_role,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "claim_type_upd_role":U, INPUT btt_auth_type.line_number, INPUT "Claim Type Update Role":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  IF fnValidateRoles (INPUT btt_auth_type.default_auth_status_upd_role,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "default_auth_status_upd_role":U, INPUT btt_auth_type.line_number, INPUT "Default Status Update Role":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  IF fnValidateRoles (INPUT btt_auth_type.end_date_upd_role,INPUT "hacat":U, INPUT btt_auth_type.auth_type_obj, INPUT "end_date_upd_role":U, INPUT btt_auth_type.line_number, INPUT "End Date Update Role":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  /* Ensure that a valid auth type effective date has been specified */
  IF btt_auth_type.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Type Effective Date,":U) .
  END. // btt_auth_type.effective_date = ? THEN

  /* If an end date is supplied, ensure that is not before the effective date */
  IF  btt_auth_type.end_date <> ? 
  AND btt_auth_type.end_date < btt_auth_type.effective_date THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 11,  // The End Date &1 cannot be before the Effective/Start Date &2
                          INPUT STRING(btt_auth_type.end_date,"9999/99/99") + "," + STRING(btt_auth_type.effective_date,"9999/99/99")).
  END. // IF btt_auth_type.end_date <> ? AND btt_auth_type.end_date < btt_auth_type.effective_date THEN
  
  /* If sequencekey is entered , ensure that the sequence_key is valid */
  IF btt_auth_type.sequence_key <> "":U THEN 
  DO:
    FIND FIRST buf_sequence NO-LOCK 
         WHERE buf_sequence.sequence_key = btt_auth_type.sequence_key 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

    IF NOT AVAILABLE buf_sequence THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
    
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "":U,
                            INPUT "sequence_key":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "MA":U,
                            INPUT 100,  // The "&1" specified is invalid
                            INPUT "Sequence Key: ":U + btt_auth_type.sequence_key).   
    END. // IF NOT CAN-FIND(FIRST buf_sequence...
    ELSE DO:
      IF buf_sequence.category_key <> "ma_authorisation_sequence":U THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.

        oErrorObject:addError(INPUT "hacat":U,                                                                   /* ipcOwningEntityMnemonic  */ 
                              INPUT btt_auth_type.auth_type_obj,                                                 /* ipdOwningEntityObj       */ 
                              INPUT "":U,                                                                        /* ipcOwningEntityKey       */ 
                              INPUT "sequence_key":U,                                                            /* ipcFieldName             */
                              INPUT btt_auth_type.line_number,                                                   /* ipiLineNumber            */ 
                              INPUT "MA":U,                                                                      /* ipcMessageGroup          */ 
                              INPUT 112, /* The "&1" specified is invalid. &2 */                                 /* ipiMessageNumber         */ 
                              INPUT "Auth Type Sequence Key: ":U + btt_auth_type.sequence_key +
                                    ",The sequence must be in the authorisation category.").                    /* ipcReplaceTextList       */ 
      END.  // IF buf_sequence.category_key <> "ma_sequence":U THEN

      IF NOT buf_sequence.sequence_active THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.

        oErrorObject:addError(INPUT "hacat":U,                                                                   /* ipcOwningEntityMnemonic  */ 
                              INPUT btt_auth_type.auth_type_obj,                                                 /* ipdOwningEntityObj       */ 
                              INPUT "":U,                                                                        /* ipcOwningEntityKey       */ 
                              INPUT "sequence_key":U,                                                            /* ipcFieldName             */
                              INPUT btt_auth_type.line_number,                                                   /* ipiLineNumber            */ 
                              INPUT "MA":U,                                                                      /* ipcMessageGroup          */ 
                              INPUT 112, /* The "&1" specified is invalid. &2 */                                 /* ipiMessageNumber         */ 
                              INPUT "Auth Type Sequence Key: ":U + btt_auth_type.sequence_key +
                                    ",The sequence must be active.").                                            /* ipcReplaceTextList       */ 
      END.  // IF NOT buf_sequence.sequence_active THEN
    END.  // ELSE - IF NOT AVAILABLE buf_sequence THEN

    IF btt_auth_type.auth_type_prefix = "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacat":U,                                                                   /* ipcOwningEntityMnemonic  */ 
                            INPUT btt_auth_type.auth_type_obj,                                                 /* ipdOwningEntityObj       */ 
                            INPUT "":U,                                                                        /* ipcOwningEntityKey       */ 
                            INPUT "sequence_key":U,                                                            /* ipcFieldName             */
                            INPUT btt_auth_type.line_number,                                                   /* ipiLineNumber            */ 
                            INPUT "MA":U,                                                                      /* ipcMessageGroup          */ 
                            INPUT 111, /* The &1 must be specified. &2 */                                      /* ipiMessageNumber         */ 
                            INPUT "Auth Type Prefix,":U + 
                                  ",The Prefix must be specified when a sequence is specified.").              /* ipcReplaceTextList       */
    END.  // IF btt_auth_type.auth_type_prefix = "" THEN
  END.  // IF btt_auth_type.sequence_key <> "":U THEN

  IF  btt_auth_type.auth_type_prefix <> "" 
  AND btt_auth_type.sequence_key     <> "" THEN
  DO:
    IF CAN-FIND(FIRST hac_auth_type NO-LOCK
                WHERE hac_auth_type.auth_type_obj   <> btt_auth_type.auth_type_obj
                AND   hac_auth_type.auth_type_prefix = btt_auth_type.auth_type_prefix
                AND   hac_auth_type.sequence_key     = btt_auth_type.sequence_key) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacat":U,                                                                   /* ipcOwningEntityMnemonic  */
                            INPUT btt_auth_type.auth_type_obj,                                                 /* ipdOwningEntityObj       */
                            INPUT "":U,                                                                        /* ipcOwningEntityKey       */
                            INPUT "sequence_key":U,                                                            /* ipcFieldName             */
                            INPUT btt_auth_type.line_number,                                                   /* ipiLineNumber            */
                            INPUT "MA":U,                                                                      /* ipcMessageGroup          */
                            INPUT 112, /* The &1 specified is invalid. &2 */                                   /* ipiMessageNumber         */
                            INPUT "Auth Type Prefix: ":U + btt_auth_type.auth_type_prefix +
                                  ",This Prefix already exists on another authorisation type.").              /* ipcReplaceTextList       */
    END.  // IF CAN-FIND(FIRST hdc_auth_type NO-LOCK
  END.  // IF btt_auth_type.auth_type_prefix <> ""... THEN

  /* Validate condition code if specified */
  IF btt_auth_type.icd_cond_code <> "":U 
  AND NOT CAN-FIND(FIRST buf_condition NO-LOCK 
                   WHERE buf_condition.cond-code = btt_auth_type.icd_cond_code) THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "icd_cond_code":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 100,  // The "&1" specified is invalid
                          INPUT "Auth Type ICD condition code: ":U + STRING(btt_auth_type.icd_cond_code)).
  END. // IF btt_auth_type.icd_cond_code <> "":U AND NOT CAN-FIND(FIRST buf_condition...
  
  IF btt_auth_type.icd_cond_type <> "":U 
  AND NOT CAN-FIND(FIRST buf_condtype NO-LOCK 
                   WHERE buf_condtype.cond-type = btt_auth_type.icd_cond_type) THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "icd_cond_type":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 100,  // The "&1" specified is invalid
                          INPUT "Auth type ICD condition type: ":U + STRING(btt_auth_type.icd_cond_type)).
  END. // IF btt_auth_type.icd_cond_type <> "":U AND NOT CAN-FIND(FIRST buf_condtype...

  /* Validate auth_type_group */
  IF btt_auth_type.auth_type_group <> "":U THEN
  DO:
    FIND FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key = "ma_acAuthTypeGroup":U
           AND mic_acronym.acronym_key  = btt_auth_type.auth_type_group
     NO-ERROR.
     
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
     
    IF NOT AVAILABLE mic_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "":U,
                            INPUT "auth_type_group":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "MA":U,
                            INPUT 100,  // The "&1" specified is invalid
                            INPUT "Auth type group: ":U + STRING(btt_auth_type.auth_type_group)).
    END.  // IF NOT AVAILABLE mic_acronym THEN
  END. // IF btt_auth_type.auth_type_group <> "":U THEN

  /* Validate detail line restrictions */
  IF btt_auth_type.default_line_restriction <> "":U THEN
  DO:
    lSuccess = mipEnv:Health:AuthService:validateLineRestriction(INPUT btt_auth_type.default_line_restriction).
     
    IF NOT lSuccess THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "":U,
                            INPUT "default_line_restriction":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "MA":U,
                            INPUT 100,  // The "&1" specified is invalid
                            INPUT "Default Line Restriction: ":U + STRING(btt_auth_type.default_line_restriction)).
    END. // IF NOT lSuccess THEN
  END. // IF btt_auth_type.default_line_restriction <> "":U

  /* 
    If status rule is not set up or it is not active, no default statuses or status reason may be entered.
  */
  IF btt_auth_type.effective_date <> ? THEN 
  DO:
    ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0,
                                                                     INPUT  0,
                                                                     INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                                     INPUT  "ValidStatuses":U,
                                                                     INPUT  btt_auth_type.effective_date,
                                                                     OUTPUT lValidRule,
                                                                     OUTPUT cRuleValue).

    IF NOT lValidRule OR NOT lSuccess THEN
    DO:
      ASSIGN btt_auth_type.default_auth_status          = ""
             btt_auth_type.default_auth_status_note     = ""
             btt_auth_type.default_auth_status_upd_user = ""
             oplFailureOccurred                         = TRUE.
    
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "",
                            INPUT "default_auth_status":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "ma_MsgAuth":U,
                            INPUT 12,  // The Auth Rule '&1' must be set up for rule type '&2'.
                            INPUT "ValidStatuses,Auth Status":U + "[Help=Auth Rule Code: ValidStatuses]").
    
    END. // IF NOT lValidRule OR NOT lSuccess THEN
    
    /*
      Validate the default_auth_status if it has been specified 
    */
    IF btt_auth_type.default_auth_status <> "":U
    THEN DO:
      ASSIGN lSuccess = mipEnv:Health:AuthService:validateAuthStatus(INPUT  INTEGER(btt_auth_type.default_auth_status),
                                                                     INPUT  "Auth":U).
      IF NOT lSuccess THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cErrorMessage      = "Auth Type Default Status ":U + STRING(btt_auth_type.default_auth_status) +
                                    ",[HELP=Auth Rule Code: ValidStatuses]".
        
        oErrorObject:addError(INPUT "hacat":U,
                              INPUT btt_auth_type.auth_type_obj,
                              INPUT "":U,
                              INPUT "default_auth_status":U,
                              INPUT btt_auth_type.line_number,
                              INPUT "MA":U,
                              INPUT 112,  // The "&1" specified is invalid. &2
                              INPUT cErrorMessage).
      END. // IF NOT lSuccess THEN
    
      /* Validate if the auth_status_note is mandatory */
      IF btt_auth_type.default_auth_status_note = "" THEN 
      DO:
        ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  INTEGER(btt_auth_type.default_auth_status),
                                                                            INPUT  btt_auth_type.insurer_obj,
                                                                            INPUT  00,
                                                                            INPUT  btt_auth_type.effective_date).
        IF lMandatory THEN 
        DO:
          ASSIGN oplFailureOccurred = TRUE
                 cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT  "Auth":U,
                                                                               INPUT  INTEGER(btt_auth_type.default_auth_status))
                 cErrorMessage = "The Default Status Reason is mandatory for auth status '" + cStatusDescr 
                               + "'.[HELP=Auth Rule Code: EnforceStatusNote]":U.
          oErrorObject:addError(INPUT "hacat":U,
                                INPUT btt_auth_type.auth_type_obj,
                                INPUT "",
                                INPUT "default_auth_status_note":U,
                                INPUT btt_auth_type.line_number,
                                INPUT cErrorMessage,
                                INPUT "ERR":U).                                                                               
        END. // IF lMandatory THEN
      END. // IF btt_auth_type.default_auth_status_note = "":U THEN
    END. // IF btt_auth_type.default_auth_status <> "" THEN
    
    /*
      Validate default status reason if specified
    */
    IF btt_auth_type.default_auth_status_note <> "":U THEN
    DO:
      mipEnv:Health:AuthService:ValidateStatusReason(INPUT  btt_auth_type.insurer_obj,
                                                     INPUT  00,
                                                     INPUT  btt_auth_type.default_auth_status_note,
                                                     INPUT  INTEGER(btt_auth_type.default_auth_status),
                                                     INPUT  btt_auth_type.effective_date,
                                                     OUTPUT cErrorMessage).
  
      IF cErrorMessage <> "":U THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
  
        oErrorObject:addError(INPUT "hacat":U,
                              INPUT btt_auth_type.auth_type_obj,
                              INPUT "",
                              INPUT "default_auth_status_note":U,
                              INPUT btt_auth_type.line_number,
                              INPUT cErrorMessage,
                              INPUT "ERR":U).
      END. // IF cErrorMessage <> "":U THEN
    END. // IF btt_auth_type.default_auth_status_note <> "":U THEN
  END. // IF btt_auth_type.effective_date <> ? THEN
  
  /* Validate comma delimited list of icds */
  IF btt_auth_type.valid_icds <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type.valid_icds):
     IF NOT CAN-FIND(FIRST diagnos
                     WHERE diagnos.diagnosis = TRIM(ENTRY(iCnt,btt_auth_type.valid_icds)))
     THEN DO:
       ASSIGN oplFailureOccurred = TRUE.

       oErrorObject:addError(INPUT "hacat":U,
                             INPUT btt_auth_type.auth_type_obj,
                             INPUT "":U,
                             INPUT "valid_icds":U,
                             INPUT btt_auth_type.line_number,
                             INPUT "MA":U,
                             INPUT 100,  /* The "&1" specified is invalid */
                             INPUT "Auth Type Valid ICDs: ":U + STRING(btt_auth_type.valid_icds)).

     END. // IF NOT CAN-FIND(FIRST diagnos WHERE diagnos.diagnosis = TRIM(ENTRY(iCnt,btt_auth_type.valid_icds)))
  END. // IF btt_auth_type.valid_icds <> ""
  
  /*
    Only one set up can be captured "ICD Condition Code & ICD Condition Type" or "Valid ICD code(s)"
  */
  IF  btt_auth_type.valid_icds    <> ""
  AND btt_auth_type.icd_cond_code <> "" THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "",
                          INPUT "icd_cond_code":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "ma_MsgAuth":U,
                          INPUT 20,  /* You may select either &1, OR &2. */
                          INPUT "ICD Condition Code & ICD Condition Type,Valid ICD code(s)":U).

  END. // IF  btt_auth_type.valid_icds <> "" AND btt_auth_type.icd_cond_code <> "" THEN
  ELSE DO:
    IF  btt_auth_type.icd_cond_code <> "" 
    AND btt_auth_type.icd_cond_type  = "" THEN
    DO:                                       
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "",
                            INPUT "icd_cond_type":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "ma_MsgAuth":U,
                            INPUT 21,  // Enter a valid &1 for &2.
                            INPUT "ICD Condition Type,ICD Condition Code":U).
                            
    END. // IF  btt_auth_type.icd_cond_code <> "" AND...
    ELSE
      IF  btt_auth_type.icd_cond_code  = "" 
      AND btt_auth_type.icd_cond_type <> "" THEN
      DO:
    
        ASSIGN oplFailureOccurred = TRUE.
    
        oErrorObject:addError(INPUT "hacat":U,
                              INPUT btt_auth_type.auth_type_obj,
                              INPUT "",
                              INPUT "icd_cond_type":U,
                              INPUT btt_auth_type.line_number,
                              INPUT "ma_MsgAuth":U,
                              INPUT 21,  // Enter a valid &1 for &2.
                              INPUT "ICD Condition Code,ICD Condition Type":U).
                              
      END. // IF  btt_auth_type.icd_cond_code = "" AND...
  END. // ELSE DO - IF btt_auth_type.valid_icds <> "" AND...
  
  /*
    Validate header_values_allowed if specified
  */
  IF btt_auth_type.header_values_allowed <> "":U THEN 
  DO:
    IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key = "ma_acAuthHeadValAllowed":U
           AND mic_acronym.acronym_key  = btt_auth_type.header_values_allowed) THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "":U,
                            INPUT "header_values_allowed":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "MA":U,
                            INPUT 100,  // The "&1" specified is invalid
                            INPUT "Auth Type Header Values Allowed: ":U + STRING(btt_auth_type.header_values_allowed)).
    END. // IF NOT CAN-FIND(FIRST mic_acronym
  END. // IF btt_auth_type.header_values_allowed <> "":U
  
  IF btt_auth_type.activate_copayment = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "activate_copayment":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 100,  // The "&1" specified is invalid
                          INPUT "Activate Copayment":U) .
  END. // IF btt_auth_type.activate_copayment = ? THEN
  
  IF btt_auth_type.activate_penalty = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "":U,
                          INPUT "activate_penalty":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "MA":U,
                          INPUT 100,  // The "&1" specified is invalid
                          INPUT "Activate Penalty":U) .
  END. // IF btt_auth_type.activate_penalty = ? THEN
  
  IF btt_auth_type.activate_am_pm = FALSE THEN
  DO:
    IF btt_auth_type.activate_penalty = TRUE THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
    
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "",
                            INPUT "activate_am_pm":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "Penalty cannot be activated if AM&PM is not activated on the authorisation type." ,
                            INPUT "ERR":U).
    END. // IF btt_auth_type.activate_penalty = TRUE THEN
    
    /* 
      Validate am_pm must be activated if LOS is activated 
    */                                                               
    IF btt_auth_type.activate_los = TRUE THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
    
      oErrorObject:addError(INPUT "hacat":U,
                            INPUT btt_auth_type.auth_type_obj,
                            INPUT "",
                            INPUT "activate_am_pm":U,
                            INPUT btt_auth_type.line_number,
                            INPUT "AM&PM can not be deactivated if LOS is activated on the authorisation type." ,
                            INPUT "ERR":U).
    
    END. // IF btt_auth_type.activate_los = TRUE THEN
  END. // IF btt_auth_type.activate_am_pm = FALSE THEN
  
  IF  btt_auth_type.activate_los = FALSE 
  AND btt_auth_type.activate_los_weekend_pass = TRUE THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacat":U,
                          INPUT btt_auth_type.auth_type_obj,
                          INPUT "",
                          INPUT "activate_los_weekend_pass":U,
                          INPUT btt_auth_type.line_number,
                          INPUT "LOS Weekend Pass can not be activated if LOS is deactivated on the authorisation type." ,
                          INPUT "ERR":U).
  END. /* IF btt_auth_type.activate_los = FALSE AND btt_auth_type.activate_los_weekend_pass = TRUE THEN  */
  
  IF fnValidateActiveHealth (INPUT "ma_acDepHealthCat":U , 
                             INPUT btt_auth_type.activate_health ,
                             INPUT "hacat":U , 
                             INPUT btt_auth_type.auth_type_obj , 
                             INPUT "active_health":U ) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
                                                                                          
  /*
    If claim codes are set up on control level , a message type must be specified on the header_values_allowed
  */
  IF (CAN-FIND(FIRST buf_auth_type_control NO-LOCK
               WHERE buf_auth_type_control.auth_type_obj = btt_auth_type.auth_type_obj 
                 AND (   NUM-ENTRIES(buf_auth_type_control.claim_codes_header)  > 1 
                      OR NUM-ENTRIES(buf_auth_type_control.claim_types_header)  > 1 ))
   OR CAN-FIND(FIRST buf_auth_type_provider  NO-LOCK 
               WHERE buf_auth_type_provider.auth_type_obj = btt_auth_type.auth_type_obj 
                 AND (   NUM-ENTRIES(buf_auth_type_provider.claim_codes_provider)  > 1 
                      OR NUM-ENTRIES(buf_auth_type_provider.claim_types_provider)  > 1 )))
  AND btt_auth_type.multiple_cc_message_type  = "":U 
  THEN 
    ASSIGN oplFailureOccurred = TRUE 
           lSuccess           = oErrorObject:addError(INPUT "hacat":U,
                                                      INPUT btt_auth_type.auth_type_obj,
                                                      INPUT "",
                                                      INPUT "multiple_cc_message_type":U,
                                                      INPUT btt_auth_type.line_number,
                                                      INPUT "Message Type must be specified when auth type controls are set up with multiple Claim Codes/Types" ,
                                                      INPUT "ERR":U).
  
  IF btt_auth_type.multiple_cc_message_type <> "":U 
  AND NOT CAN-FIND( FIRST mic_acronym NO-LOCK 
                    WHERE mic_acronym.category_key  = "ma_acAuthTypeMultipleCCMessageType":U 
                      AND mic_acronym.acronym_key   = btt_auth_type.multiple_cc_message_type )
  THEN 
    ASSIGN oplFailureOccurred = TRUE    
           lSuccess           = oErrorObject:addError(INPUT "hacat":U,
                                                      INPUT btt_auth_type.auth_type_obj,
                                                      INPUT "",
                                                      INPUT "multiple_cc_message_type":U,
                                                      INPUT btt_auth_type.line_number,
                                                      INPUT "MA":U,
                                                      INPUT 112,  // The "&1" specified is invalid. &2
                                                      INPUT "Message Type,":U + btt_auth_type.multiple_cc_message_type + " is not a valid acronym":U ).


{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
        
&ENDIF

