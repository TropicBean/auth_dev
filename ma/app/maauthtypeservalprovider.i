/* maauthtypeservalprovider.i  MEDSTAR Medical Aid System
                               Validate Auth type provider Buffer
                               (c) Copyright 2018 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/
  DEFINE PARAMETER BUFFER btt_auth_type_provider   FOR tt_auth_type_provider.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL     NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cEntry               AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cExclFieldsAllowed   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cFieldName           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessage             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRuleValue           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatusDescr         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE iCnt                 AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iExtent              AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iField               AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lMandatory           AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL             NO-UNDO. 
  DEFINE VARIABLE lValidRule           AS LOGICAL             NO-UNDO. 
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE oProviderTypeInd     AS cls.mipacronym      NO-UNDO.

  DEFINE BUFFER buf_auth_type           FOR hac_auth_type.
  DEFINE BUFFER buf_hac_auth_rule       FOR hac_auth_rule.
  DEFINE BUFFER buf_auth_type_provider  FOR hac_auth_type_provider. 
  DEFINE BUFFER btt1_auth_type_provider FOR tt_auth_type_provider.
  DEFINE BUFFER buf_auth_group          FOR ham_auth_group.

  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_type_provider THEN
  DO:
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Type Provider as no buffer is available.'" }
  END.  // IF NOT AVAILABLE btt_auth_type_provider THEN
  
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

  /* Validate provider type indicator */
  IF btt_auth_type_provider.provider_type_indicator = "":U THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactp":U,
                          INPUT btt_auth_type_provider.auth_type_provider_obj,
                          INPUT "":U,
                          INPUT "provider_type_indicator":U,
                          INPUT btt_auth_type_provider.line_number,
                          INPUT "MA":U,
                          INPUT 112,  // The &1 specified is invalid. &2
                          INPUT "Auth Type Provider Type Indicator" + ",Please enter a valid value.").
  END.  // IF btt_auth_type_provider.provider_type_indicator = "":U THEN
  ELSE DO:
    ASSIGN oProviderTypeInd = NEW cls.mipacronym(?,FALSE, "ma_acAuthProviderTypeIndicator":U, ?).
      
    oProviderTypeInd:focusAcronym("KEY":U, btt_auth_type_provider.provider_type_indicator) NO-ERROR.

    IF NOT oProviderTypeInd:AcronymInFocus THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cMessage           = SUBSTITUTE("The Provider Type Indicator (&1) specified could not be found.":U, 
                                             btt_auth_type_provider.provider_type_indicator).

      oErrorObject:addError(INPUT "hactp":U,                     
                            INPUT btt_auth_type_provider.auth_type_provider_obj,    
                            INPUT "":U,                                           
                            INPUT "provider_type_indicator":U,                                
                            INPUT btt_auth_type_provider.line_number,                    
                            INPUT cMessage,                                                                           
                            INPUT "ERR":U).
    END. // IF NOT oProviderTypeInd:AcronymInFocus THEN 
    ELSE IF btt_auth_type_provider.provider_type_indicator = "ma_acAuthProviderTypeIndicatorExcl":U THEN
    DO:
      /*
        The fields listed below are the only fields that MAY contain  
        values when the provider_type_indicator is an exclusion.
        If any other field in the table contains a value, it will be cleared
        in the function and a warning message will be returned.
      */
      ASSIGN cMessage           = ""
             cExclFieldsAllowed = "auth_type_obj,auth_type_provider_obj,insurer_obj,option_code,provider_type_indicator,":U   +
                                  "effective_date,end_date,default_auth_status,default_auth_status_note,provider_sequence,":U +
                                  "pr_type_list,pr_type_valid_list,mandatory,number_providers_allowed,default_claim_code_detail,default_claim_type_detail":U.
    
      ASSIGN cMessage = fnValidateExclusionValues(INPUT TEMP-TABLE btt_auth_type_provider:HANDLE,      // The name of the buffer that is validated
                                                  INPUT btt_auth_type_provider.auth_type_provider_obj, // The unique obj of the buffer that is validated
                                                  INPUT cExclFieldsAllowed).                           // The list of fields that MAY contain a value
      IF cMessage <> ""
      THEN
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_provider_obj,
                              INPUT "":U,
                              INPUT "provider_type_indicator":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT cMessage,
                              INPUT "WAR":U).

      /*
        Auth Type Detail lines are not allowed for providers with Exclusion-status
      */
      IF CAN-FIND(FIRST hac_auth_type_detail
                  WHERE hac_auth_type_detail.auth_type_obj          = btt_auth_type_provider.auth_type_obj
                  AND   hac_auth_type_detail.auth_type_provider_obj = btt_auth_type_provider.auth_type_provider_obj)
      THEN
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_provider_obj,
                              INPUT "":U,
                              INPUT "provider_type_indicator":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT "No Auth Type Detail setups are allowed for a Provider Type Indicator that is an Exclusion.",
                              INPUT "ERR":U).
    
    END.  // IF btt_auth_type_provider.provider_type_indicator = "ma_acAuthProviderTypeIndicatorExcl":U THEN
  END.  // ELSE - IF btt_auth_type_provider.provider_type_indicator = "":U THEN
  
  IF btt_auth_type_provider.auth_group_obj <> 0.00 THEN
  DO:
    FIND FIRST buf_auth_group NO-LOCK
         WHERE buf_auth_group.auth_group_obj = btt_auth_type_provider.auth_group_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE buf_auth_group THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "auth_group_obj":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "AUth group selected isinvalid":U,
                            INPUT "ERR").
    END.  // IF NOT AVAILABLE buf_auth_type THEN
  END.

  /* Ensure that a valid auth type has been specified */
  IF btt_auth_type_provider.auth_type_obj <> 0.00 THEN
  DO:
    FIND FIRST buf_auth_type NO-LOCK
         WHERE buf_auth_type.auth_type_obj = btt_auth_type_provider.auth_type_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE buf_auth_type THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "auth_type_obj":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 101,  // The "&1" is not available
                            INPUT "Auth Type Header ").
    END.  // IF NOT AVAILABLE buf_auth_type THEN
  END. // IF btt_auth_type_provider.auth_type_obj <> 0.00 THEN
  
  /* Ensure that a valid auth type effective date has been specified */
  IF btt_auth_type_provider.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactp":U,
                          INPUT btt_auth_type_provider.auth_type_provider_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_type_provider.line_number,
                          INPUT "MA":U,
                          INPUT 111, // The &1 must be specified. &2
                          INPUT "Auth Type Provider Effective Date,").
  END. // btt_auth_type_provider.effective_date = ? THEN
  ELSE DO:
    FIND FIRST buf_auth_type NO-LOCK
         WHERE buf_auth_type.auth_type_obj = btt_auth_type_provider.auth_type_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = FALSE }

    IF NOT AVAILABLE buf_auth_type THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "effective_date":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 107,  // Could not find a "&1" record using "&2"
                            INPUT "Auth Type Header,Obj: " + STRING(btt_auth_type_provider.auth_type_obj)).

    END. // IF NOT AVAILABLE buf_auth_type
    ELSE IF btt_auth_type_provider.effective_date < buf_auth_type.effective_date THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "effective_date":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 112,  // The &1 specified is invalid. &2
                            INPUT "Authorisation type provider effective date: " + STRING(btt_auth_type_provider.effective_date,"9999/99/99")
                                  + ",The date cannot be before the authorisation type effective date: " + STRING(buf_auth_type.effective_date,"9999/99/99")).
    END. // ELSE IF btt_auth_type_provider.effective_date < buf_auth_type.effective_date THEN
    ELSE IF buf_auth_type.end_date <> ?
    AND btt_auth_type_provider.effective_date > buf_auth_type.end_date THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "effective_date":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 112,  /* The &1 specified is invalid. &2 */
                            INPUT "Authorisation type provider effective date: " + STRING(btt_auth_type_provider.effective_date,"9999/99/99")
                                  + ",The date cannot be after the authorisation type end date: " + STRING(buf_auth_type.end_date,"9999/99/99")).
    END. // ELSE IF buf_auth_type.end_date <> ? AND...

    /* If an end date is supplied, ensure that it is not before the effective date */
    IF btt_auth_type_provider.end_date <> ? THEN
    DO:
      IF btt_auth_type_provider.end_date < btt_auth_type_provider.effective_date THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
    
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_provider_obj,
                              INPUT "":U,
                              INPUT "end_date":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT "MA":U,
                              INPUT 112,  // The &1 specified is invalid. &2
                              INPUT "Authorisation Type Provider End Date: " + STRING(btt_auth_type_provider.end_date,"9999/99/99") +
                                    ",The End Date cannot be before the Authorisation Type Provider Effective Date: " + 
                                    STRING(btt_auth_type_provider.effective_date,"9999/99/99")).
      END. // IF btt_auth_type_provider.end_date < btt_auth_type_provider.effective_date THEN
      ELSE IF buf_auth_type.end_date <> ?
          AND btt_auth_type_provider.end_date > buf_auth_type.end_date THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
    
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_provider_obj,
                              INPUT "":U,
                              INPUT "end_date":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT "MA":U,
                              INPUT 112,  // The &1 specified is invalid. &2
                              INPUT "Authorisation Type Provider End Date: " + STRING(btt_auth_type_provider.end_date,"9999/99/99") +
                                    ",The End Date cannot be after the Authorisation Type End Date: " + 
                                    STRING(buf_auth_type.end_date,"9999/99/99")).
      END. // ELSE IF buf_auth_type.end_date <> ? AND...
    END. // IF btt_auth_type_provider.end_date <> ? THEN
  END. // ELSE IF btt_auth_type_provider.effective_date = ? THEN
  
  /* Ensure that a valid Option Code has been specified */
  IF btt_auth_type_provider.option_code <> 0 THEN 
  DO:
    FIND FIRST scheme NO-LOCK
         WHERE scheme.scheme-code = btt_auth_type_provider.option_code
         AND   scheme.active      = TRUE
     NO-ERROR.
     
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE scheme THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "option_code":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Option Code: ":U + STRING(btt_auth_type_provider.option_code)).
    END. // IF NOT AVAILABLE scheme THEN
  END. // btt_auth_type_provider.option_code = "":U
  
  /* Ensure that a valid insurer has been specified */
  IF btt_auth_type_provider.insurer_obj <> 0.00 THEN
  DO:
    FIND FIRST erm_insurer NO-LOCK
         WHERE erm_insurer.insurer_obj = btt_auth_type_provider.insurer_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "insurer_obj":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  // The "&1" specified is invalid
                            INPUT "Auth Type Provider Client: ":U + STRING(btt_auth_type_provider.insurer_obj)).
    END.  // IF NOT AVAILABLE erm_insurer THEN
    ELSE
      ASSIGN cInsurerName = erm_insurer.insurer_name.
  END. // IF btt_auth_type_provider.insurer_obj <> 0.00 THEN

  /* Validate provider_type */
  IF btt_auth_type_provider.provider_type <> "":U THEN
  DO:
    lSuccess = mipEnv:Health:AuthService:validateProviderType(INPUT btt_auth_type_provider.provider_type).

    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "provider_type":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  // The "&1" specified is invalid
                            INPUT "Provider Type: ":U + btt_auth_type_provider.provider_type).
    END. // IF NOT lSuccess THEN
  END. // IF btt_auth_type_provider.provider_type <> "":U THEN
  
  /* Validate provider_status */
  IF btt_auth_type_provider.default_auth_status <> "":U THEN
  DO:
    ASSIGN lSuccess = mipEnv:Health:AuthService:validateAuthStatus(INPUT  INTEGER(btt_auth_type_provider.default_auth_status),
                                                                   INPUT  "Auth":U).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cErrorMessage      = "Provider Default Authorisation Status: ":U + btt_auth_type_provider.default_auth_status +
                                  ",[HELP=Auth Rule Code: ValidStatuses]".
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "default_auth_status":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 112,  // The "&1" specified is invalid. &2
                            INPUT cErrorMessage).
    END. // IF NOT lSuccess THEN

    /* Validate if the auth_status_note is mandatory */
    IF btt_auth_type_provider.default_auth_status_note = "" THEN 
    DO:
      ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  INTEGER(btt_auth_type_provider.default_auth_status),
                                                                          INPUT  btt_auth_type_provider.insurer_obj,
                                                                          INPUT  00,
                                                                          INPUT  btt_auth_type_provider.effective_date).
      IF lMandatory THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT  "Auth":U,
                                                                             INPUT  INTEGER(btt_auth_type_provider.default_auth_status))
               cErrorMessage = "The Provider Default Status Reason is mandatory for auth status '" + cStatusDescr 
                             + "'.[HELP=Auth Rule Code: EnforceStatusNote]":U.
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_obj,
                              INPUT "",
                              INPUT "default_auth_status_note":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT cErrorMessage,
                              INPUT "ERR":U).                                                                               
      END. // IF lMandatory THEN
    END. // IF btt_auth_type_provider.default_auth_status_note = "":U
  END. // IF btt_auth_type_provider.default_auth_status <> "":U
  
  IF btt_auth_type_provider.auth_auto_create = ? THEN
  DO:
    ASSIGN
      oplFailureOccurred = TRUE
      cErrorMessage = "authorisation auto create can not be unknowen. Value must be either yes or no".

    oErrorObject:addError(INPUT "hactp":U,
                          INPUT btt_auth_type_provider.auth_type_obj,
                          INPUT "",
                          INPUT "auth_auto_create":U,
                          INPUT btt_auth_type_provider.line_number,
                          INPUT cErrorMessage,
                          INPUT "ERR":U).  
  END. /* IF btt_auth_type_detail.auth_auto_create = ? THEN */

  /* Validate default status reason if specified */
  IF btt_auth_type_provider.default_auth_status_note <> "":U THEN
  DO:
    mipEnv:Health:AuthService:ValidateStatusReason(INPUT  btt_auth_type_provider.insurer_obj,
                                                   INPUT  00,
                                                   INPUT  btt_auth_type_provider.default_auth_status_note,
                                                   INPUT  INTEGER(btt_auth_type_provider.default_auth_status),
                                                   INPUT  btt_auth_type_provider.effective_date,
                                                   OUTPUT cErrorMessage).
  
    IF cErrorMessage <> "":U THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_obj,
                            INPUT "":U,
                            INPUT "default_auth_status_note":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT cErrorMessage,
                            INPUT "ERR":U).
    END. // IF cErrorMessage <> "":U THEN
  END. // IF btt_auth_type_provider.default_auth_status_note <> "":U
  
  /* 
    Validate pr_type_list 
  */
  /* The pr_type is made of the pr_type(first 3 digits) and the sub_pr_type(last 3 digits) */
  IF btt_auth_type_provider.pr_type_list <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.pr_type_list):
  
    ASSIGN
      cEntry = TRIM(ENTRY(iCnt, btt_auth_type_provider.pr_type_list)).
      
    IF SUBSTRING(cEntry,4,3) = "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "pr_type_list":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 111, // The &1 must be specified. &2
                            INPUT "Sub Discipline for Practice Type ":U + cEntry + ",").
    END.  // IF SUBSTRING(cEntry,4,3) = "" THEN
    ELSE IF LENGTH(cEntry) <> 6
    OR NOT CAN-FIND(FIRST prtype NO-LOCK
                    WHERE prtype.pr-type = INTEGER(SUBSTRING(cEntry,1,3)))
    OR NOT CAN-FIND(FIRST subdisc NO-LOCK
                    WHERE subdisc.pr-type      = INTEGER(SUBSTRING(cEntry,1,3))
                      AND subdisc.subdisp-code = INTEGER(SUBSTRING(cEntry,4,3))) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "pr_type_list":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  // The "&1" specified is invalid
                            INPUT "Provider Practice Types Entry: ":U + cEntry).
    END. // IF NOT CAN-FIND(FIRST prtype NO-LOCK
  END. // btt_auth_type_provider.pr_type_list <> "":U THEN
  
  /* Validate number_providers_allowed  */
  IF  btt_auth_type_provider.provider_type <> "":U 
  AND btt_auth_type_provider.number_providers_allowed = 0 THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    oErrorObject:addError(INPUT "hactp":U,
                          INPUT btt_auth_type_provider.auth_type_provider_obj,
                          INPUT "":U,
                          INPUT "number_providers_allowed":U,
                          INPUT btt_auth_type_provider.line_number,
                          INPUT "Enter the number of Providers allowed":U,
                          INPUT "ERR":U).
  END. // IF btt_auth_type_provider.provider_type <> "":U AND...
  
  /* Validate provider_sequence */
  IF btt_auth_type_provider.provider_sequence = 0 THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactp":U,
                          INPUT btt_auth_type_provider.auth_type_provider_obj,
                          INPUT "":U,
                          INPUT "provider_sequence":U,
                          INPUT btt_auth_type_provider.line_number,
                          INPUT "MA":U,
                          INPUT 111, // The &1 must be specified. &2 
                          INPUT "Provider Sequence,").
  END. // IF btt_auth_type_provider.provider_sequence = 0 THEN
  ELSE DO:  
    IF CAN-FIND(FIRST hac_auth_type_provider NO-LOCK
                WHERE hac_auth_type_provider.auth_type_obj           = btt_auth_type_provider.auth_type_obj 
                AND   hac_auth_type_provider.auth_type_provider_obj <> btt_auth_type_provider.auth_type_provider_obj 
                AND   hac_auth_type_provider.provider_sequence       = btt_auth_type_provider.provider_sequence) THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "provider_sequence":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100, // The "&1" specified is invalid
                            INPUT "Provider Sequence ":U + STRING(btt_auth_type_provider.provider_sequence) + " is already used. Provider Sequence ":U).
    END. // IF CAN-FIND(FIRST hac_auth_type_provider
  END. // IF btt_auth_type_provider.provider_sequence = 0

  /* Validate pr_type_valid_list */
  IF btt_auth_type_provider.pr_type_valid_list <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.pr_type_valid_list):
  
    ASSIGN
      cEntry = TRIM(ENTRY(iCnt, btt_auth_type_provider.pr_type_valid_list)).

    IF SUBSTRING(cEntry,4,3) = "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "pr_type_valid_list":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 111, // The &1 must be specified. &2
                            INPUT "Sub Discipline for Valid Practice Type ":U + cEntry + ",").
    END.  // IF SUBSTRING(cEntry,4,3) = "" THEN
    ELSE IF LENGTH(cEntry) <> 6
    OR NOT CAN-FIND(FIRST prtype NO-LOCK
                    WHERE prtype.pr-type = INTEGER(SUBSTRING(cEntry,1,3)))
    OR NOT CAN-FIND(FIRST subdisc NO-LOCK
                    WHERE subdisc.pr-type      = INTEGER(SUBSTRING(cEntry,1,3))
                      AND subdisc.subdisp-code = INTEGER(SUBSTRING(cEntry,4,3))) THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "pr_type_valid_list":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100, // The "&1" specified is invalid
                            INPUT "Provider Valid Practice Type Entry ":U + cEntry).
    END. // ELSE IF LENGTH(cEntry) <> 6 OR...
  END. // IF btt_auth_type_provider.pr_type_valid_list <> "":U THEN

  /* Validate header_values_allowed if specified */
  IF btt_auth_type_provider.header_values_allowed <> "":U
  AND NOT CAN-FIND(mic_acronym NO-LOCK
             WHERE mic_acronym.category_key = "ma_acAuthHeadValAllowed":U
               AND mic_acronym.acronym_key  = btt_auth_type_provider.header_values_allowed) THEN 
  DO:                                                                     
    ASSIGN oplFailureOccurred = TRUE.
    oErrorObject:addError(INPUT "hactp":U,
                          INPUT btt_auth_type_provider.auth_type_obj,
                          INPUT "":U,
                          INPUT "header_values_allowed":U,
                          INPUT btt_auth_type_provider.line_number,
                          INPUT "MA":U,
                          INPUT 100, // The "&1" specified is invalid
                          INPUT "Header Values Allowed: ":U + STRING(btt_auth_type_provider.header_values_allowed) 
                                                            + " for Option: ":U + STRING(btt_auth_type_provider.option_code) + ", Client: ":U + cInsurerName).
  END. // IF btt_auth_type_provider.header_values_allowed <> "":U AND...
  
  /* Ensure that the first "Default" Authorisation Type Provider Control record has the Authorised Service selected */
  IF  btt_auth_type_provider.authorised_service       = NO
  AND btt_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorDef":U THEN
  DO:
    IF NOT CAN-FIND(FIRST buf_auth_type_provider NO-LOCK
                    WHERE buf_auth_type_provider.auth_type_obj            = btt_auth_type_provider.auth_type_obj
                      AND buf_auth_type_provider.authorised_service       = YES
                      AND buf_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorDef":U) THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "authorised_service":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "ma_MsgAuth":U,
                            INPUT 32,  // Select the Authorised Service for the the first Default Auth Type Provider.
                            INPUT "":U).

    END. // IF NOT CAN-FIND(FIRST buf_auth_type_provider...
  END. // IF btt_auth_type_provider.authorised_service = NO AND...


  IF  btt_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorExcl":U
  AND btt_auth_type_provider.pr_type_valid_list      <> "":U 
  AND btt_auth_type_provider.pr_type_list            <> "":U THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    oErrorObject:addError(INPUT "hactp":U,                           
                           INPUT btt_auth_type_provider.auth_type_provider_obj, 
                           INPUT "":U,                                
                           INPUT "pr_type_list":U,                    
                           INPUT btt_auth_type_provider.line_number,  
                           INPUT "Please specify either the Practice Types or the Valid Practice Types. You may not specify both for an Exclusion record.":U,                       
                           INPUT "ERR":U).                            
  END. /* IF btt_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorExcl":U */

  /* Allow only one Main Provider */
  IF  btt_auth_type_provider.main_provider            = YES
  AND btt_auth_type_provider.authorised_service       = YES
  AND btt_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorDef":U THEN 
  DO:
    FIND FIRST buf_auth_type_provider NO-LOCK
         WHERE buf_auth_type_provider.auth_type_obj            = btt_auth_type_provider.auth_type_obj
           AND buf_auth_type_provider.auth_type_provider_obj  <> btt_auth_type_provider.auth_type_provider_obj
           AND buf_auth_type_provider.main_provider            = YES
           AND buf_auth_type_provider.authorised_service       = YES
           AND buf_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorDef":U
           AND buf_auth_type_provider.insurer_obj              = btt_auth_type_provider.insurer_obj    
           AND buf_auth_type_provider.option_code              = btt_auth_type_provider.option_code    
           AND buf_auth_type_provider.effective_date           = btt_auth_type_provider.effective_date 
     NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
     
    IF AVAILABLE buf_auth_type_provider THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "main_provider":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "ma_MsgAuth":U,
                            INPUT 17,  // You can only have one Main Authorisation Type Provider.
                            INPUT "":U).
    END. // IF AVAILABLE buf_auth_type_provider THEN
  END. // IF btt_auth_type_provider.main_provider = YES AND...

  /* Validate comma delimited list of users */
  IF fnValidateUsers (INPUT btt_auth_type_provider.base_rate_upd_user ,
                      INPUT "hactp":U, 
                      INPUT btt_auth_type_provider.auth_type_provider_obj, 
                      INPUT "base_rate_upd_user":U, 
                      INPUT btt_auth_type_provider.line_number, 
                      INPUT "Base Rate Updates Allowed User":U ) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
  
  /* Validate comma delimited list of users */
  IF fnValidateUsers (INPUT btt_auth_type_provider.default_auth_status_upd_user,
                      INPUT "hactp":U, 
                      INPUT btt_auth_type_provider.auth_type_provider_obj, 
                      INPUT "default_auth_status_upd_user":U, 
                      INPUT btt_auth_type_provider.line_number, 
                      INPUT "Provider Status Update User":U ) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
  
  /* Validate comma delimited list of roles */
  IF fnValidateRoles (INPUT btt_auth_type_provider.default_auth_status_upd_role,
                      INPUT "hactp":U, 
                      INPUT btt_auth_type_provider.auth_type_provider_obj, 
                      INPUT "default_auth_status_upd_role":U, 
                      INPUT btt_auth_type_provider.line_number, 
                      INPUT "Provider Default Status Update Role":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
  
  /* Validate comma delimited list of roles */
  IF fnValidateRoles (INPUT btt_auth_type_provider.base_rate_upd_role,
                      INPUT "hactp":U, 
                      INPUT btt_auth_type_provider.auth_type_provider_obj, 
                      INPUT "base_rate_upd_role":U, 
                      INPUT btt_auth_type_provider.line_number, 
                      INPUT "Base Rate Updates Allowed Role":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
  
  /* Validate comma delimited list of users */
  IF fnValidateUsers (INPUT btt_auth_type_provider.ars_rate_upd_user ,
                      INPUT "hactp":U, 
                      INPUT btt_auth_type_provider.auth_type_provider_obj, 
                      INPUT "ars_rate_upd_user":U, 
                      INPUT btt_auth_type_provider.line_number, 
                      INPUT "Ars Rate Updates Allowed User":U ) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
  
  /* Validate comma delimited list of roles */
  IF fnValidateRoles (INPUT btt_auth_type_provider.ars_rate_upd_role,
                      INPUT "hactp":U, 
                      INPUT btt_auth_type_provider.auth_type_provider_obj, 
                      INPUT "ars_rate_upd_role":U, 
                      INPUT btt_auth_type_provider.line_number, 
                      INPUT "Ars Rate Updates Allowed Role":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.
  
  IF btt_auth_type_provider.neg_num <> 0 
  AND NOT(CAN-FIND( FIRST neggroup NO-LOCK 
                    WHERE neggroup.neg-num = btt_auth_type_provider.neg_num))
  THEN 
    ASSIGN oplFailureOccurred = TRUE
           lSuccess           = oErrorObject:addError(INPUT "hactp":U,
                                                      INPUT btt_auth_type_provider.auth_type_provider_obj,
                                                      INPUT "":U,
                                                      INPUT "neg_num":U,
                                                      INPUT btt_auth_type_provider.line_number,
                                                      INPUT "MA":U,
                                                      INPUT 112,  // The &1 specified is invalid. &2
                                                      INPUT "Negotiation Number," + STRING(btt_auth_type_provider.neg_num) + " is not a valid value." ).
                                                      
  IF btt_auth_type_provider.enforce_header_claim_code_match = ? 
  AND btt_auth_type_provider.provider_type_indicator        = "ma_acAuthProviderTypeIndicatorDef"
  THEN 
    ASSIGN oplFailureOccurred = TRUE 
           lSuccess           = oErrorObject:addError(INPUT "hactp":U,
                                                      INPUT btt_auth_type_provider.auth_type_provider_obj,
                                                      INPUT "":U,
                                                      INPUT "enforce_header_claim_code_match":U,
                                                      INPUT btt_auth_type_provider.line_number,
                                                      INPUT "MA":U,
                                                      INPUT 112, // The "&1" specified is invalid.&2
                                                      INPUT "Enforce Header Claim Code Match Indicator, It must have a value of either Yes/No":U).
                                                    
  IF btt_auth_type_provider.enforce_header_claim_type_match = ? 
  AND btt_auth_type_provider.provider_type_indicator        = "ma_acAuthProviderTypeIndicatorDef"
  THEN 
    ASSIGN oplFailureOccurred = TRUE 
           lSuccess           = oErrorObject:addError(INPUT "hactp":U,
                                                      INPUT btt_auth_type_provider.auth_type_provider_obj,
                                                      INPUT "":U,
                                                      INPUT "enforce_header_claim_type_match":U,
                                                      INPUT btt_auth_type_provider.line_number,
                                                      INPUT "MA":U,
                                                      INPUT 112, // The "&1" specified is invalid.&2
                                                      INPUT "Enforce Header Claim Type Match Indicator, It must have a value of either Yes/No":U).

  /* Validate Pr Types, Claim Codes and Claim Types */
  RUN _validateProviderClaimCodes        IN TARGET-PROCEDURE(BUFFER btt_auth_type_provider, INPUT-OUTPUT oplFailureOccurred). 
  RUN _validateProviderAuthorisedService IN TARGET-PROCEDURE(BUFFER btt_auth_type_provider, INPUT-OUTPUT oplFailureOccurred). 
  
  { mip/inc/mipcatcherror.i
      &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF      

