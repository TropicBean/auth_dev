/* maauthcopayservaldetail.i  MEDSTAR Medical Aid System
                              Validate Auth Copay Detail
                              (c) Copyright 2021 - 2021
                              MIP Holdings (Pty) Ltd
                              All rights reserved
   This include is for the _validateAuthCopayDetail procedure in maauthcopayservicestack.p
*/      
                
  DEFINE PARAMETER BUFFER btt_auth_copay_detail    FOR tt_auth_copay_detail.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL.
  
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE cAuthCopayDetailEntities   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cEndDate                   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValidMessage              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidProvider             AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE oErrorObject               AS cls.maerrorobject   NO-UNDO. 
  DEFINE VARIABLE iProviderNr                AS INTEGER             NO-UNDO.
  DEFINE VARIABLE dEffectiveDate             AS DATE                NO-UNDO.
  
  DEFINE BUFFER buf_auth_copay_detail  FOR hac_auth_copay_detail.
  DEFINE BUFFER buf_auth_copay_control FOR hac_auth_copay_control.
  DEFINE BUFFER buf_owning_acronym     FOR mic_acronym.
  DEFINE BUFFER buf_exclusion_acronym  FOR mic_acronym.

  /* 
    Make sure we have a valid buffer before we go any further 
  */
  IF NOT AVAILABLE btt_auth_copay_detail
  THEN 
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Copay Detail as no buffer is available.'" }
    
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE) .

  IF btt_auth_copay_detail.auth_copay_control_obj = 0 
  OR btt_auth_copay_detail.auth_copay_control_obj = ? THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "haccd":U,
                          INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                          INPUT "":U,
                          INPUT "auth_copay_control_obj":U,
                          INPUT btt_auth_copay_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Control Obj,":U).
  END.  // IF btt_auth_copay_detail.auth_copay_control_obj = 0
  ELSE IF NOT CAN-FIND(FIRST hac_auth_copay_control 
                  WHERE hac_auth_copay_control.auth_copay_control_obj = btt_auth_copay_detail.auth_copay_control_obj)
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccd":U, 
                          INPUT btt_auth_copay_detail.auth_copay_detail_obj, 
                          INPUT "":U, 
                          INPUT "auth_copay_control_obj":U,
                          INPUT btt_auth_copay_detail.line_number, 
                          INPUT "MA":U, 
                          INPUT 107,  /* Could not find a "&1" record using "&2" */
                          INPUT "Auth Copay Control,":U + STRING(btt_auth_copay_detail.auth_copay_control_obj)).       
  END.  // IF NOT CAN-FIND(FIRST hac_auth_copay_control

  /*
    Owning entity validation
  */
  IF btt_auth_copay_detail.owning_entity_mnemonic = "":U 
  OR btt_auth_copay_detail.owning_entity_mnemonic = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccd":U,
                          INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                          INPUT "":U,
                          INPUT "owning_entity_mnemonic":U,
                          INPUT btt_auth_copay_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Owning Entity Mnemonic,":U ).
  END.  // IF btt_auth_copay_detail.owning_entity_mnemonic = "":U
  ELSE DO:
    FIND FIRST buf_owning_acronym NO-LOCK
         WHERE buf_owning_acronym.category_key  = "ma_acAuthCopayDetailEntities":U
         AND   buf_owning_acronym.acronym_value = btt_auth_copay_detail.owning_entity_mnemonic
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_owning_acronym THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "owning_entity_mnemonic":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid*/
                            INPUT "Owning entity: ":U + 
                                  btt_auth_copay_detail.owning_entity_mnemonic).    
    END.  // IF NOT AVAILABLE buf_owning_acronym THEN

    /*
      Validate the owning obj and owning key
    */
    IF btt_auth_copay_detail.owning_obj = ? 
    THEN
      ASSIGN btt_auth_copay_detail.owning_obj = 0.
    IF btt_auth_copay_detail.owning_key = ? 
    THEN
      ASSIGN btt_auth_copay_detail.owning_key = "".

    IF  btt_auth_copay_detail.owning_obj = 0
    AND btt_auth_copay_detail.owning_key = "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "owning_obj":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT "Either the Owning Obj or the Owning Key must be specified.",
                            INPUT "ERR":U).
    END.  // IF btt_auth_copay_detail.owning_obj = 0

    /* 
      Ensure that a valid owning alt value has been specified 
    */
    ASSIGN btt_auth_copay_detail.owning_alt_value = IF btt_auth_copay_detail.owning_alt_value = ?
                                                    THEN "":U
                                                    ELSE TRIM(btt_auth_copay_detail.owning_alt_value).

    IF btt_auth_copay_detail.owning_alt_value = "":U THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage      = IF AVAILABLE buf_owning_acronym
                                  THEN SUBSTITUTE("The &1 must be specified.","Owning Value for the ":U + buf_owning_acronym.acronym_label)
                                  ELSE SUBSTITUTE("The &1 must be specified.","Owning Value":U).
  
      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "owning_alt_value":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END. // btt_auth_copay_detail.owning_alt_value = "":U THEN
    ELSE IF btt_auth_copay_detail.owning_entity_mnemonic = "doctor":U THEN    
    DO:
      IF btt_auth_copay_detail.owning_key = ""
      THEN
        ASSIGN iProviderNr = INTEGER(btt_auth_copay_detail.owning_alt_value).
      ELSE
        ASSIGN iProviderNr = INTEGER(btt_auth_copay_detail.owning_key).

      IF btt_auth_copay_detail.effective_date = ?
      THEN
        ASSIGN dEffectiveDate = TODAY.
      ELSE
        ASSIGN dEffectiveDate = btt_auth_copay_detail.effective_date.

      ASSIGN
        lValidProvider = mipEnv:Health:maDoctor:isProviderValid( INPUT iProviderNr, 
                                                                 INPUT btt_auth_copay_detail.effective_date, 
                                                                 INPUT btt_auth_copay_detail.end_date).
      IF NOT lValidProvider THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cValidMessage      = "Provider,":U
               cValidMessage      = IF btt_auth_copay_detail.owning_key = ""
                                    THEN cValidMessage
                                    ELSE cValidMessage + "(":U + STRING(iProviderNr) + ")":U.
  
        oErrorObject:addError(INPUT "haccd":U,                                   /* ipcOwningEntityMnemonic  */
                              INPUT btt_auth_copay_detail.auth_copay_detail_obj, /* ipdOwningEntityObj       */
                              INPUT "":U,                                        /* ipcOwningEntityKey       */
                              INPUT "owning_key",                                /* ipcFieldName             */
                              INPUT btt_auth_copay_detail.line_number,           /* ipiLineNumber            */
                              INPUT "ma_MsgProvider":U,                          /* ipcMessageGroup          */
                              INPUT 2,  /* The &1 &2 is inactive. */             /* ipiMessageNumber         */
                              INPUT cValidMessage).                              /* ipcReplaceTextList       */
        
      END.  // IF NOT lValidProvider THEN  
    END.  // IF btt_auth_copay_detail.owning_entity_mnemonic = "doctor":U THEN
    ELSE IF AVAILABLE buf_owning_acronym THEN
    DO:
      mipEnv:miDBEntity:focusTable(buf_owning_acronym.acronym_value).
    
      IF NOT mipEnv:miDBEntity:InFocus THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cValidMessage      = "Invalid owning table '" + buf_owning_acronym.acronym_value + "' specified.".

        oErrorObject:addError(INPUT "haccd":U,
                              INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                              INPUT "":U,
                              INPUT "exclusion_entity_mnemonic":U,
                              INPUT btt_auth_copay_detail.line_number,
                              INPUT cValidMessage,
                              INPUT "ERR":U).                  
      END.  // IF NOT mipEnv:miDBEntity:InFocus THEN
      ELSE DO:
        ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                           THEN 
                              mipEnv:miDBEntity:findRecord(btt_auth_copay_detail.owning_obj)
                           ELSE 
                              mipEnv:miDBEntity:findRecord(btt_auth_copay_detail.owning_key)).
                           
        IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
        DO:
          ASSIGN oplFailureOccurred = TRUE
                 cValidMessage      = "Could not find an owning " + buf_owning_acronym.acronym_label + " record using '" +
                                      (IF btt_auth_copay_detail.owning_key <> "" 
                                       THEN btt_auth_copay_detail.owning_key 
                                       ELSE btt_auth_copay_detail.owning_alt_value) + "'":U.

          oErrorObject:addError(INPUT "haccd":U,
                                INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                                INPUT "":U,
                                INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                       THEN "btt_auth_copay_detail.owning_obj":U 
                                       ELSE "btt_auth_copay_detail.owning_key":U),
                                INPUT btt_auth_copay_detail.line_number,
                                INPUT cValidMessage,
                                INPUT "ERR":U).
        END.  // IF NOT mipEnv:miDBEntity:RecordAvailable
      END.  // ELSE - IF NOT mipEnv:miDBEntity:InFocus THEN
    END.  // ELSE IF AVAILABLE buf_owning_acronym THEN
  END.  // ELSE - IF btt_auth_copay_detail.owning_entity_mnemonic = "":U

  /*
    Exclusion entity validation
  */
  IF  btt_auth_copay_detail.exclusion_entity_mnemonic <> "":U 
  AND btt_auth_copay_detail.exclusion_entity_mnemonic <> ? THEN
  DO:
    FIND FIRST buf_exclusion_acronym NO-LOCK
         WHERE buf_exclusion_acronym.category_key  = "ma_acAuthCopayDetailEntities":U
         AND   buf_exclusion_acronym.acronym_value = btt_auth_copay_detail.exclusion_entity_mnemonic 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_exclusion_acronym THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "exclusion_entity_mnemonic":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid*/
                            INPUT "Exclusion Entity: ":U + btt_auth_copay_detail.exclusion_entity_mnemonic).    
    END.  // IF NOT AVAILABLE mic_acronym THEN

    IF btt_auth_copay_detail.exclusion_obj = ? 
    THEN
      ASSIGN btt_auth_copay_detail.exclusion_obj = 0.

    IF btt_auth_copay_detail.exclusion_key = ? 
    THEN
      ASSIGN btt_auth_copay_detail.exclusion_key = "".

    IF  btt_auth_copay_detail.exclusion_obj = 0
    AND btt_auth_copay_detail.exclusion_key = "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "exclusion_obj":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT "Either the Exclusion Obj or the Exclusion Key must be specified.",
                            INPUT "ERR":U).
    END.  // IF btt_auth_copay_detail.exclusion_obj = 0

    ELSE IF btt_auth_copay_detail.owning_entity_mnemonic = "doctor":U THEN  
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage      = "An exclusion entity is not allowed.".

      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "exclusion_entity_mnemonic":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END.  // IF btt_auth_copay_detail.owning_entity_mnemonic = "doctor":U THEN
    ELSE IF  btt_auth_copay_detail.owning_entity_mnemonic    = "neggroup":U 
         AND btt_auth_copay_detail.exclusion_entity_mnemonic = "neggroup":U THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage      = "A Negotiation Group exclusion entity is not allowed for a Negotiation Group co-payment entity.".

      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "exclusion_entity_mnemonic":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END.  // ELSE IF btt_auth_copay_detail.owning_entity_mnemonic = "neggroup":U
    ELSE IF  btt_auth_copay_detail.owning_entity_mnemonic    = "prtype":U  
         AND btt_auth_copay_detail.exclusion_entity_mnemonic = "prtype":U THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage      = "A Doctor Discipline exclusion entity is not allowed for a Doctor Discipline co-payment entity.".

      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "exclusion_entity_mnemonic":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END.  // ELSE IF btt_auth_copay_detail.owning_entity_mnemonic = "prtype":U
    ELSE IF AVAILABLE buf_exclusion_acronym THEN
    DO:
      mipEnv:miDBEntity:focusTable(buf_exclusion_acronym.acronym_value).
      
      IF NOT mipEnv:miDBEntity:InFocus THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cValidMessage      = "Invalid exclusion table '" + buf_exclusion_acronym.acronym_value + "' specified.".

        oErrorObject:addError(INPUT "haccd":U,
                              INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                              INPUT "":U,
                              INPUT "exclusion_entity_mnemonic":U,
                              INPUT btt_auth_copay_detail.line_number,
                              INPUT cValidMessage,
                              INPUT "ERR":U).
      END. // IF NOT mipEnv:miDBEntity:InFocus THEN
      ELSE DO:
        ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                           THEN 
                             mipEnv:miDBEntity:findRecord(btt_auth_copay_detail.exclusion_obj)
                           ELSE 
                             mipEnv:miDBEntity:findRecord(btt_auth_copay_detail.exclusion_key)).
                           
        IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
        DO:
          ASSIGN oplFailureOccurred = TRUE
                 cValidMessage      = "Could not find an exclusion " + buf_exclusion_acronym.acronym_label + " record using '" +
                                      (IF btt_auth_copay_detail.exclusion_key <> "" 
                                       THEN btt_auth_copay_detail.exclusion_key 
                                       ELSE btt_auth_copay_detail.exclusion_alt_value) + "'":U.

          oErrorObject:addError(INPUT "haccd":U,
                                INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                                INPUT "":U,
                                INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                       THEN "btt_auth_copay_detail.exclusion_obj":U 
                                       ELSE "btt_auth_copay_detail.exclusion_key":U),
                                INPUT btt_auth_copay_detail.line_number,
                                INPUT cValidMessage,
                                INPUT "ERR":U).                  
        END. // IF NOT mipEnv:miDBEntity:RecordAvailable
      END.  // ELSE - IF NOT mipEnv:miDBEntity:InFocus THEN

      IF btt_auth_copay_detail.exclusion_alt_value = "" THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cValidMessage      = IF AVAILABLE buf_exclusion_acronym
                                    THEN SUBSTITUTE("The &1 must be specified.",buf_exclusion_acronym.acronym_label + " Exclusion Value":U)
                                    ELSE SUBSTITUTE("The &1 must be specified.","Exclusion Value":U).
  
        oErrorObject:addError(INPUT "haccd":U,
                              INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                              INPUT "":U,
                              INPUT "exclusion_alt_value":U,
                              INPUT btt_auth_copay_detail.line_number,
                              INPUT cValidMessage,
                              INPUT "ERR":U).
      END.  // IF btt_auth_copay_detail.exclusion_alt_value = "" THEN
    END.  // IF AVAILABLE buf_exclusion_acronym <> "":U THEN
  END.  // IF btt_auth_copay_detail.exclusion_entity_mnemonic <> "":U THEN

  /*
    Ensure that a valid auth type effective date has been specified
  */
  IF btt_auth_copay_detail.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "haccd":U,
                          INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_copay_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Detail Effective Date,":U) .
  END. // IF btt_auth_copay_detail.effective_date = ? THEN

  /* 
    If an end date is supplied, ensure that is not before the effective date 
  */
  IF  btt_auth_copay_detail.end_date <> ? 
  AND btt_auth_copay_detail.end_date < btt_auth_copay_detail.effective_date THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE
           cValidMessage      = SUBSTITUTE("The End Date (&1) cannot be before the Effective Date (&2).",
                                           STRING(btt_auth_copay_detail.end_date,'9999/99/99'),
                                           STRING(btt_auth_copay_detail.effective_date,'9999/99/99')).
    oErrorObject:addError(INPUT "haccd":U,
                          INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_copay_detail.line_number,
                          INPUT cValidMessage,
                          INPUT "ERR":U).
  END. // IF btt_auth_copay_detail.end_date <> ? AND btt_auth_copay_detail.end_date < btt_auth_copay_detail.effective_date THEN
  
  FIND FIRST buf_auth_copay_control NO-LOCK
       WHERE buf_auth_copay_control.auth_copay_control_obj = btt_auth_copay_detail.auth_copay_control_obj 
    NO-ERROR.

  { mip/inc/mipthrowerror.i &ResetIgnoredErrors = TRUE &IgnoreErrors = 'PROGRESS:565' } 

  IF AVAILABLE buf_auth_copay_control THEN
  DO:
    IF (btt_auth_copay_detail.effective_date < buf_auth_copay_control.effective_date
    OR (buf_auth_copay_control.end_date <> ? AND btt_auth_copay_detail.effective_date > buf_auth_copay_control.end_date)) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cEndDate           = IF buf_auth_copay_control.end_date <> ?
                                  THEN STRING(buf_auth_copay_control.end_date,'9999/99/99')
                                  ELSE "?"
             cValidMessage      = SUBSTITUTE("The Effective Date (&1) specified is invalid. " +
                                             "The Effective date must be within the auth copay control period (&2).",
                                             STRING(btt_auth_copay_detail.effective_date,'9999/99/99'),
                                             STRING(buf_auth_copay_control.effective_date,'9999/99/99') + '-' +
                                             cEndDate).
      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "effective_date":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END.  // IF (btt_auth_copay_detail.effective_date < buf_auth_copay_control.effective_date

    IF btt_auth_copay_detail.end_date <> ? 
    AND ((btt_auth_copay_detail.end_date  < buf_auth_copay_control.effective_date
     OR  (buf_auth_copay_control.end_date <> ? AND btt_auth_copay_detail.end_date > buf_auth_copay_control.end_date))) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cValidMessage      = SUBSTITUTE("The End Date (&1) specified is invalid. " +
                                             "The End date must be within the auth copay control period (&2).",
                                             STRING(btt_auth_copay_detail.end_date,'9999/99/99'),
                                             STRING(buf_auth_copay_control.effective_date,'9999/99/99') + '-' +
                                             STRING(buf_auth_copay_control.end_date,'9999/99/99')).
      oErrorObject:addError(INPUT "haccd":U,
                            INPUT btt_auth_copay_detail.auth_copay_detail_obj,
                            INPUT "":U,
                            INPUT "end_date":U,
                            INPUT btt_auth_copay_detail.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
    END.  // IF btt_auth_copay_detail.end_date <> ?
  END.  // IF AVAILABLE buf_auth_copay_control THEN

{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
        
&ENDIF


