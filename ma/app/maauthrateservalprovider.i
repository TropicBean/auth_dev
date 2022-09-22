/* maauthrateservalprovider.i MEDSTAR Medical Aid System
                              Validate Auth Rate Provider
                              (c) Copyright 2020 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/      
                
  DEFINE PARAMETER BUFFER btt_auth_rate_provider   FOR tt_auth_rate_provider.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS  LOGICAL.
  
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.  

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
  
  DEFINE BUFFER buf_auth_rate_provider  FOR hac_auth_rate_provider.
  DEFINE BUFFER buf_auth_rate_control   FOR hac_auth_rate_control.
  
  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_rate_provider THEN 
  DO:
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Rate Provider as no buffer is available.'" }
  END.  // IF NOT AVAILABLE btt_auth_rate_provider THEN
    
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE) .

  IF btt_auth_rate_provider.auth_rate_control_obj = 0 
  OR btt_auth_rate_provider.auth_rate_control_obj = ? THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "hacrp":U,
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                          INPUT "":U,
                          INPUT "auth_rate_control_obj":U,
                          INPUT btt_auth_rate_provider.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Control Obj,":U).
  END. /* IF btt_auth_rate_provider.auth_rate_control_obj = 0 OR btt_auth_rate_provider.auth_rate_control_obj = ? */

  FIND FIRST buf_auth_rate_control NO-LOCK
    WHERE buf_auth_rate_control.auth_rate_control_obj = btt_auth_rate_provider.auth_rate_control_obj 
    NO-ERROR.

  { mip/inc/mipthrowerror.i &ResetIgnoredErrors = TRUE &IgnoreErrors = 'PROGRESS:565' } 
  
  IF NOT AVAILABLE buf_auth_rate_control THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrp":U, 
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj, 
                          INPUT "":U, 
                          INPUT "auth_rate_control_obj":U,
                          INPUT btt_auth_rate_provider.line_number, 
                          INPUT "MA":U, 
                          INPUT 107,  /* Could not find a "&1" record using "&2" */
                          INPUT "Auth Rate Control,":U + STRING(btt_auth_rate_provider.auth_rate_control_obj)).       
  END. /* IF NOT CAN-FIND(FIRST hac_auth_rate_control */

  IF btt_auth_rate_provider.auth_group_obj <> 0 
  AND AVAILABLE buf_auth_rate_control THEN
  DO:
    FIND ham_auth_group NO-LOCK
      WHERE ham_auth_group.auth_group_obj = btt_auth_rate_provider.auth_group_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &ResetIgnoredErrors = TRUE &IgnoreErrors = 'PROGRESS:138' } 

    IF AVAILABLE ham_auth_group THEN
    DO:
      IF  ham_auth_group.effective_date > buf_auth_rate_control.effective_date
      OR (buf_auth_rate_control.end_date <> ? AND ham_auth_group.effective_date > buf_auth_rate_control.end_date)
      THEN
        ASSIGN oplFailureOccurred = TRUE 
               cErrorMessage      = "The Auth Group must be active for the Auth Rate Control Period (" + 
                                     STRING(buf_auth_rate_control.effective_date,"9999/99/99") + " - " + 
                                     STRING(buf_auth_rate_control.end_date,"9999/99/99")       + ")"
               lSuccess           = oErrorObject:addError(INPUT "hacrp":U, 
                                                          INPUT btt_auth_rate_provider.auth_rate_control_obj, 
                                                          INPUT "":U, 
                                                          INPUT "auth_group_obj":U,
                                                          INPUT btt_auth_rate_provider.line_number, 
                                                          INPUT cErrorMessage,
                                                          INPUT "ERR":U ).

      ELSE IF ham_auth_group.end_date <> ? THEN
      DO:
        IF ham_auth_group.end_date < buf_auth_rate_control.effective_date
        OR (buf_auth_rate_control.end_date <> ? AND ham_auth_group.end_date < buf_auth_rate_control.end_date)
        THEN
          ASSIGN oplFailureOccurred = TRUE
                 cErrorMessage      = "The Auth Group must be active for the Auth Rate Control Period (" + 
                                       STRING(buf_auth_rate_control.effective_date,"9999/99/99") + " - " + 
                                       STRING(buf_auth_rate_control.end_date,"9999/99/99")       + ")"
                 lSuccess           = oErrorObject:addError(INPUT "hacrp":U, 
                                                            INPUT btt_auth_rate_provider.auth_rate_control_obj, 
                                                            INPUT "":U, 
                                                            INPUT "auth_group_obj":U,
                                                            INPUT btt_auth_rate_provider.line_number, 
                                                            INPUT cErrorMessage,
                                                            INPUT "ERR":U ).
      END.  /* IF ham_auth_group.end_date <> ? THEN */
      ELSE IF CAN-FIND(FIRST ham_auth_group_detail
                    WHERE ham_auth_group_detail.auth_group_obj         = ham_auth_group.auth_group_obj
                    AND   ham_auth_group_detail.owning_entity_mnemonic = "prtype":U)
        AND NOT CAN-FIND(FIRST ham_auth_group_detail
                         WHERE ham_auth_group_detail.auth_group_obj         = ham_auth_group.auth_group_obj
                         AND   ham_auth_group_detail.effective_date        <= buf_auth_rate_control.effective_date
                         AND  (ham_auth_group_detail.end_date               = ?                                                      OR ham_auth_group_detail.end_date   >= buf_auth_rate_control.effective_date)
                         AND   ham_auth_group_detail.owning_entity_mnemonic = "prtype":U
                         AND  (ham_auth_group_detail.pr_type                = STRING(btt_auth_rate_provider.auth_rate_pr_type,"999") OR ham_auth_group_detail.pr_type     = "000")
                         AND  (ham_auth_group_detail.sub_pr_type            = btt_auth_rate_provider.auth_rate_sub_pr_type           OR ham_auth_group_detail.sub_pr_type = 0)) THEN 
        DO:
          ASSIGN oplFailureOccurred = TRUE
                 cErrorMessage = IF AVAILABLE ham_auth_group
                                 THEN "Auth group '" + ham_auth_group.auth_group_code + 
                                      "' not allowed for discipline " + STRING(btt_auth_rate_provider.auth_rate_pr_type,"999":U) + 
                                      " and sub-discipline " + STRING(btt_auth_rate_provider.auth_rate_sub_pr_type,"999":U)
                                 ELSE "Invalid auth group selected"
                 lSuccess = oErrorObject:addError
                                   (INPUT "hacrp":U,                                    /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth_rate_provider.auth_rate_control_obj, /* ipdOwningEntityObj       */
                                    INPUT "":U,                                         /* ipcOwningEntityKey       */
                                    INPUT "auth_group_obj":U,                           /* ipcFieldName             */
                                    INPUT btt_auth_rate_provider.line_number,           /* ipiLineNumber            */
                                    INPUT cErrorMessage,                                /* ipcMessageText           */
                                    INPUT "ERR":U).                                     /* ipiMessageType           */
        END.  /* IF NOT CAN-FIND(FIRST ham_auth_group_detail... */
    END.  /* IF AVAILABLE ham_auth_group THEN */
  END.  /* IF btt_auth_rate_provider.auth_group_obj <> 0 THEN */

  /* Ensure that a valid auth type effective date has been specified */
  IF btt_auth_rate_provider.effective_date = ? THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrp":U,
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_rate_provider.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Provider Effective Date,":U) .
  END. /* btt_auth_rate_provider.effective_date = ? THEN */

  /* If an end date is supplied, ensure that is not before the effective date */
  IF  btt_auth_rate_provider.end_date <> ? 
  AND btt_auth_rate_provider.end_date <  btt_auth_rate_provider.effective_date THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrp":U,
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_rate_provider.line_number,
                          INPUT "MA":U,
                          INPUT 11,  /* The End Date &1 cannot be before the Effective/Start Date &2 */
                          INPUT STRING(btt_auth_rate_provider.end_date,"9999/99/99") + "," + STRING(btt_auth_rate_provider.effective_date,"9999/99/99")).
  END. /* IF btt_auth_rate_provider.end_date <> ? AND btt_auth_rate_provider.end_date < btt_auth_rate_provider.effective_date THEN */
  
  IF btt_auth_rate_provider.auth_rate_indicator = "":U THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "hacrp":U,
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                          INPUT "":U,
                          INPUT "auth_rate_indicator":U,
                          INPUT btt_auth_rate_provider.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Provider Indicator,":U) .
    
  END. /* IF btt_auth_rate_provider.auth_rate_indicator = "":U */
  ELSE DO:
    IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
                    WHERE mic_acronym.category_key = "ma_acAuthRateProviderIndicator":U
                      AND mic_acronym.acronym_key  = btt_auth_rate_provider.auth_rate_indicator) THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "auth_rate_indicator":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Rate provider Indicator: ":U + btt_auth_rate_provider.auth_rate_indicator). 
    END. /* IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK */
    ELSE DO:
      IF btt_auth_rate_provider.auth_rate_indicator = "ma_acAuthRateProvIndicatorDefault":U THEN
      DO:
        // Both auth_rate_provider_type & auth_rate_provider_pr_type must be completed because 
        // the details will be used to create a provider record on the authorisation.
        IF btt_auth_rate_provider.auth_rate_provider_type = "":U 
        OR btt_auth_rate_provider.auth_rate_pr_type       = 0 THEN 
        DO:
          ASSIGN oplFailureOccurred = TRUE.
  
          IF btt_auth_rate_provider.auth_rate_provider_type = "":U 
          THEN
            oErrorObject:addError(INPUT "hacrp":U,
                                  INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                                  INPUT "":U,
                                  INPUT "auth_rate_provider_type":U,
                                  INPUT btt_auth_rate_provider.line_number,
                                  INPUT "MA":U,
                                  INPUT 111,  /* The &1 must be specified. &2 */
                                  INPUT "Auth Rate Provider Type and Discipline,":U).
          
          IF btt_auth_rate_provider.auth_rate_pr_type = 0
          THEN
            oErrorObject:addError(INPUT "hacrp":U,
                                  INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                                  INPUT "":U,
                                  INPUT "auth_rate_pr_type":U,
                                  INPUT btt_auth_rate_provider.line_number,
                                  INPUT "MA":U,
                                  INPUT 111,  /* The &1 must be specified. &2 */
                                  INPUT "Auth Rate Provider Type and Discipline,":U).                      
        END. /* IF (btt_auth_rate_provider.auth_rate_provider_type = "":U AND btt_auth_rate_provider.auth_rate_pr_type = "":U) */
      END. /* IF btt_auth_rate_provider.auth_rate_indicator = "ma_acAuthRateProvIndicatorDefault":U THEN */
      
      /* Check if the auth_rate_provider_type is a valid acronym */
      IF btt_auth_rate_provider.auth_rate_provider_type <> "":U THEN 
      DO:
        IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
                        WHERE mic_acronym.category_key = "ma_acAuthProviderType":U
                          AND mic_acronym.acronym_key  = btt_auth_rate_provider.auth_rate_provider_type) THEN 
        DO:
          ASSIGN oplFailureOccurred = TRUE.
      
          oErrorObject:addError(INPUT "hacrp":U,
                                INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                                INPUT "":U,
                                INPUT "auth_rate_provider_type":U,
                                INPUT btt_auth_rate_provider.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Auth Rate Provider Type: ":U + btt_auth_rate_provider.auth_rate_provider_type).
        END. /* IF NOT CAN-FIND( FIRST mic_acronym NO-LOCK */
      END. /* IF btt_auth_rate_provider.auth_rate_provider_type <> "":U */
      
      /* Check if btt_auth_rate_provider.auth_rate_pr_type is valid */
      IF btt_auth_rate_provider.auth_rate_pr_type <> 0 THEN 
      DO:
        FIND FIRST prtype NO-LOCK
          WHERE prtype.pr-type = btt_auth_rate_provider.auth_rate_pr_type 
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
        IF NOT AVAILABLE prtype THEN 
        DO:
          ASSIGN oplFailureOccurred = TRUE.
      
          oErrorObject:addError(INPUT "hacrp":U,
                                INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                                INPUT "":U,
                                INPUT "auth_rate_pr_type":U,
                                INPUT btt_auth_rate_provider.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Auth Rate Discipline Type: ":U + STRING(btt_auth_rate_provider.auth_rate_pr_type)).
        END. /* IF NOT AVAILABLE prtype THEN */
      END. /* IF btt_auth_rate_provider.auth_rate_pr_type <> "":U */
    END. /* ELSE DO: */
  END. /* ELSE DO: */
  
  /* Validate the override_auth_status */
  IF btt_auth_rate_provider.override_auth_status <> ? THEN 
  DO:
    ASSIGN lSuccess = mipEnv:Health:AuthService:validateAuthStatus(INPUT INTEGER(btt_auth_rate_provider.override_auth_status),
                                                                   INPUT "Auth":U).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cErrorMessage      = "Override Auth Status: ":U + STRING(btt_auth_rate_provider.override_auth_status) +
                                  ",[HELP=Auth Rule Code: ValidStatuses]".
        
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "override_auth_status":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "MA":U,
                            INPUT 112,  /* The "&1" specified is invalid. &2 */
                            INPUT cErrorMessage).
    END. /* IF NOT lSuccess THEN */
  END. /* IF btt_auth_rate_provider.override_auth_status <> ? THEN */
  ELSE DO:
    ASSIGN oplFailureOccurred = TRUE.
                                                                                 
    oErrorObject:addError(INPUT "hacrp":U,
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                          INPUT "":U,
                          INPUT "override_auth_status":U,
                          INPUT btt_auth_rate_provider.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Override Auth Status,":U) .

  END.  /* ELSE - IF btt_auth_rate_provider.override_auth_status <> ? THEN */
  
  /* Validate if the override_auth_status_note is mandatory */
  IF btt_auth_rate_provider.override_auth_status_note = "":U THEN 
  DO:
    ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(btt_auth_rate_provider.override_auth_status),
                                                                        INPUT 0.00,
                                                                        INPUT 00,
                                                                        INPUT btt_auth_rate_provider.effective_date).
    IF lMandatory THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cStatusDescr       = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                                 INPUT INTEGER(btt_auth_rate_provider.override_auth_status))
             cErrorMessage      = "The Override Auth Status reason is mandatory for override Auth Status '":U + cStatusDescr
                                + "'.[HELP=Auth Rule Code: EnforceStatusNote]":U.
                                                                                 
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "override_auth_status_note":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT cErrorMessage,
                            INPUT "ERR":U).
    END. /* IF lMandatory THEN */
  END. /* IF btt_auth_rate_provider.override_auth_status_note = "":U THEN */
  
  /* Validate override auth statuse reason if specified */
  IF btt_auth_rate_provider.override_auth_status_note <> "":U THEN 
  DO:
    mipEnv:Health:AuthService:ValidateStatusReason(INPUT  0.00,
                                                   INPUT  00,
                                                   INPUT  btt_auth_rate_provider.override_auth_status_note,
                                                   INPUT  INTEGER(btt_auth_rate_provider.override_auth_status),
                                                   INPUT  btt_auth_rate_provider.effective_date,
                                                   OUTPUT cErrorMessage).
    IF cErrorMessage <> "":U THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "override_auth_status_note":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "Override Auth Status Note: " + cErrorMessage,
                            INPUT "ERR":U).
    END. /* IF cErrorMessage <> "":U THEN  */
  END. /* IF btt_auth_rate_provider.override_auth_status_note <> "":U THEN */
  
  /* Validate the revert_auth_status if it has been specified */
  IF  btt_auth_rate_provider.revert_auth_status <> ? THEN 
  DO:
    ASSIGN lSuccess = mipEnv:Health:AuthService:validateAuthStatus(INPUT INTEGER(btt_auth_rate_provider.revert_auth_status),
                                                                   INPUT "Auth":U).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cErrorMessage      = "Revert Auth Status: ":U + STRING(btt_auth_rate_provider.revert_auth_status) +
                                  ",[HELP=Auth Rule Code: ValidStatuses]".   
        
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "revert_auth_status":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "MA":U,
                            INPUT 112, /* The "&1" specified is invalid. &2 */
                            INPUT cErrorMessage).
    END. /* IF NOT lSuccess THEN */
  END. /* IF btt_auth_rate_provider.revert_auth_status <> ? THEN */
  ELSE IF btt_auth_rate_provider.revert_auth_status = ?
    AND btt_auth_rate_provider.auth_rate_indicator = "ma_acAuthRateProvIndicatorOverride":U THEN 
    DO: 
      ASSIGN oplFailureOccurred = TRUE.
    
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "revert_auth_status":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "The Revert Auth Status must be specified for an Override Indicator.":U,
                            INPUT "ERR":U) .
    END.  /* ELSE IF btt_auth_rate_provider.revert_auth_status <> ? AND btt_auth_rate_provider.auth_rate_indicator = "ma_acAuthRateProvIndicatorOverride":U */

  /* Validate if the revert_auth_status_note is mandatory */
  IF  btt_auth_rate_provider.revert_auth_status_note = "":U 
  AND btt_auth_rate_provider.revert_auth_status     <> ? THEN 
  DO:
    ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(btt_auth_rate_provider.revert_auth_status),
                                                                        INPUT 0.00,
                                                                        INPUT 00,
                                                                        INPUT btt_auth_rate_provider.effective_date).
    IF lMandatory THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cStatusDescr       = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                                 INPUT INTEGER(btt_auth_rate_provider.revert_auth_status)).
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "revert_auth_status_note":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "The Revert Auth Status reason is mandatory for Revert Auth Status '":U + cStatusDescr + "'",
                            INPUT "ERR":U).
    END. /* IF lMandatory THEN */
  END. /* IF btt_auth_rate_provider.revert_auth_status_note = "":U THEN */
  
  /* Validate revert auth statuse reason if specified */
  IF btt_auth_rate_provider.revert_auth_status_note <> "":U THEN 
  DO:
    mipEnv:Health:AuthService:ValidateStatusReason(INPUT  0.00,
                                                   INPUT  00,
                                                   INPUT  btt_auth_rate_provider.revert_auth_status_note,
                                                   INPUT  INTEGER(btt_auth_rate_provider.revert_auth_status),
                                                   INPUT  btt_auth_rate_provider.effective_date,
                                                   OUTPUT cErrorMessage).
    IF cErrorMessage <> "":U THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "revert_auth_status_note":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "Revert Auth Status Note: " + cErrorMessage,
                            INPUT "ERR":U).
    END. /* IF cErrorMessage <> "":U THEN  */
  END. /* IF btt_auth_rate_provider.revert_auth_status_note <> "":U THEN */

  IF  btt_auth_rate_provider.revert_auth_status       = ? 
  AND btt_auth_rate_provider.revert_auth_status_note <> "":U THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "revert_auth_status_note":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "Auth Status Reason can't be completed if no Revert Auth Status is captured":U,
                            INPUT "ERR":U).
  END. /* IF btt_auth_rate_provider.revert_auth_status = ? AND btt_auth_rate_provider.revert_auth_status_note <> "":U */

  IF btt_auth_rate_provider.related_entity_mnemonic <> "":U THEN 
  DO:
    FIND FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key  = "ma_acAuthRateProviderEntities":U
           AND mic_acronym.acronym_value = btt_auth_rate_provider.related_entity_mnemonic 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "related_entity_mnemonic":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Related entity: ":U + btt_auth_rate_provider.related_entity_mnemonic ).    
    END. /* IF NOT AVAILABLE mic_acronym */
  END. /* IF btt_auth_rate_provider.related_entity_mnemonic <> "":U THEN */

  IF btt_auth_rate_provider.related_entity_mnemonic <> "" THEN 
  DO:
    /* Validate entity mnemonics */
    mipEnv:miDBEntity:focusTable(btt_auth_rate_provider.related_entity_mnemonic) NO-ERROR.

    IF mipEnv:miDBentity:Infocus THEN 
    DO:
      IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
      THEN
        mipEnv:miDBEntity:findRecord(btt_auth_rate_provider.related_obj).
      ELSE
        mipEnv:miDBEntity:findRecord(btt_auth_rate_provider.related_key).
      
      IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
    
        oErrorObject:addError(INPUT "hacrp":U, 
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj, 
                              INPUT "":U, 
                              INPUT "related_alt_value":U,
                              INPUT btt_auth_rate_provider.line_number, 
                              INPUT "MA":U, 
                              INPUT 107,  /* Could not find a "&1" record using "&2" */
                              INPUT mipEnv:miDBEntity:TableLabel + ",":U + (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                                                            THEN 
                                                                              STRING(btt_auth_rate_provider.related_obj)
                                                                            ELSE 
                                                                              btt_auth_rate_provider.related_key)
                              ).                  
        
      END. /* IF NOT mipEnv:miDBEntity:RecordAvailable */
      ELSE DO:
        IF btt_auth_rate_provider.related_entity_mnemonic = "tariff":U
        OR btt_auth_rate_provider.related_entity_mnemonic = "htmtl":U THEN 
        DO:
          FIND FIRST htm_tariff_link NO-LOCK
            WHERE htm_tariff_link.tariff_link_obj = mipEnv:miDBEntity:RecordObj
            NO-ERROR.
        
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
          IF AVAILABLE htm_tariff_link 
          AND htm_tariff_link.tariff_link_default = FALSE THEN 
          DO:
            ASSIGN oplFailureOccurred = TRUE.
        
            oErrorObject:addError(INPUT "hacrp":U,
                                  INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                                  INPUT "":U,
                                  INPUT "related_alt_value":U,
                                  INPUT btt_auth_rate_provider.line_number,
                                  INPUT "Tariff link ~"" + related_alt_value + "~" selected is not a default a default tariff link.",
                                  INPUT "ERR":U).
          END. /* IF AVAILABLE htm_tariff_link AND htm_tariff_link.tariff_link_default = FALSE THEN */
        END. /* IF btt_auth_rate_provider.related_entity_mnemonic = "tariff":U OR btt_auth_rate_provider.related_entity_mnemonic = "htmtl":U THEN */
      END. /* ELSE */
    END. /* IF mipEnv:miDBentity:Infocus THEN */
    ELSE DO:
      ASSIGN oplFailureOccurred = TRUE.
          
      oErrorObject:addError(INPUT "hacrp":U, 
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj, 
                            INPUT "":U, 
                            INPUT "related_entity_mnemonic":U,
                            INPUT btt_auth_rate_provider.line_number, 
                            INPUT "MA":U, 
                            INPUT 107,  /* Could not find a "&1" record using "&2" */
                            INPUT "valid Related Entity,":U + btt_auth_rate_provider.related_entity_mnemonic).                  
                           
    END. /* IF NOT mipEnv:miDBentity:Infocus */
  END. /* IF btt_auth_rate_provider.related_entity_mnemonic <> "" THEN */
  ELSE DO:
    IF btt_auth_rate_provider.related_alt_value <> "":U 
    OR btt_auth_rate_provider.related_obj       <> 0
    OR btt_auth_rate_provider.related_key       <> "":U THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
              
      oErrorObject:addError(INPUT "hacrp":U, 
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj, 
                            INPUT "":U, 
                            INPUT "related_entity_mnemonic":U,
                            INPUT btt_auth_rate_provider.line_number, 
                            INPUT "Related Type cannot be blank." ,
                            INPUT "ERR":U). 
    END. /* IF btt_auth_rate_provider.related_alt_value <> "":U OR btt_auth_rate_provider.related_obj <> 0 OR btt_auth_rate_provider.related_key <> "":U THEN */
  END. /* ELSE */

  IF btt_auth_rate_provider.auth_rate_sub_pr_type > 0 THEN
  DO:
    IF NOT CAN-FIND(FIRST subdisc NO-LOCK
                    WHERE subdisc.pr-type      = btt_auth_rate_provider.auth_rate_pr_type
                      AND subdisc.subdisp-code = btt_auth_rate_provider.auth_rate_sub_pr_type) THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
            
      oErrorObject:addError(INPUT "hacrp":U,
                            INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                            INPUT "":U,
                            INPUT "auth_rate_sub_pr_type":U,
                            INPUT btt_auth_rate_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Sub-Discipline: ":U + STRING(btt_auth_rate_provider.auth_rate_sub_pr_type,"999")).
    END. /* IF NOT CAN-FIND(FIRST prtype NO-LOCK */
  END. /* IF btt_auth_rate_provider.auth_rate_sub_pr_type > 0 THEN */

  IF  btt_auth_rate_provider.related_entity_mnemonic <> "":U
  AND btt_auth_rate_provider.provider_amount          > 0.0 THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "hacrp":U,
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                          INPUT "":U,
                          INPUT "related_entity_mnemonic":U,
                          INPUT btt_auth_rate_provider.line_number,
                          INPUT "~"Related entity~" and ~"Provider Amount~" was entered only one of the two can be entered",
                          INPUT "ERR":U).
  END. /* IF btt_auth_rate_provider.related_entity_mnemonic <> "":U AND btt_auth_rate_provider.provider_amount > 0.0 THEN */

  IF btt_auth_rate_provider.authorise_all_services = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "hacrp":U,
                          INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                          INPUT "":U,
                          INPUT "authorise_all_services":U,
                          INPUT btt_auth_rate_provider.line_number,
                          INPUT "Authorise All Services field value is Unknown.",
                          INPUT "ERR":U).

  END. /* IF btt_auth_rate_provider.authorise_all_services = ? THEN */
   
  FOR EACH hac_auth_rate_provider NO-LOCK 
    WHERE hac_auth_rate_provider.auth_rate_control_obj = btt_auth_rate_provider.auth_rate_control_obj :
   
    IF  hac_auth_rate_provider.auth_group_obj          = btt_auth_rate_provider.auth_group_obj 
    AND hac_auth_rate_provider.auth_rate_provider_obj <> btt_auth_rate_provider.auth_rate_provider_obj 
    AND btt_auth_rate_provider.auth_group_obj          > 0.00 THEN 
    DO:
      IF btt_auth_rate_provider.effective_date <> hac_auth_rate_provider.effective_date THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "effective_date":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Effective dates are not the same for Provider records that belong to the same group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.effective_date <> hac_auth_rate_provider.effective_date THEN */

      IF btt_auth_rate_provider.override_auth_status <> hac_auth_rate_provider.override_auth_status THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "override_status":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Override statuses are not the same for Provider records that belong to the same authorisation group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.override_status <> hac_auth_rate_provider.override_status THEN */
      
      IF btt_auth_rate_provider.override_auth_status_note <> hac_auth_rate_provider.override_auth_status_note THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "override_auth_status_note":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Override status Notes are not the same for Provider records that belong to the same authorisation group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.override_auth_status_note <> hac_auth_rate_provider.override_auth_status_note THEN */
      
      IF btt_auth_rate_provider.revert_auth_status <> hac_auth_rate_provider.revert_auth_status THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "revert_auth_status":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Revert Authorisation Statuses are not the same for Provider records that belong to the same authorisation group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.revert_auth_status <> hac_auth_rate_provider.revert_auth_status THEN */
      
      IF btt_auth_rate_provider.revert_auth_status_note <> hac_auth_rate_provider.revert_auth_status_note THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "revert_auth_status_note":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Revert Authorisation Status Notes are not the same for Provider records that belong to the same authorisation group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.revert_auth_status_note <> hac_auth_rate_provider.revert_auth_status_note THEN */
      
      IF btt_auth_rate_provider.provider_amount <> hac_auth_rate_provider.provider_amount THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "provider_amount":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Provider Amounts are not the same for Provider records that belong to the same authorisation group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.provider_amount <> hac_auth_rate_provider.provider_amount THEN */
      
      IF btt_auth_rate_provider.related_entity_mnemonic <> hac_auth_rate_provider.related_entity_mnemonic THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "related_entity_mnemonic":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Related Entity Mnemonics are not the same for Provider records that belong to the same authorisation group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.related_entity_mnemonic <> hac_auth_rate_provider.related_entity_mnemonic THEN */
      
      IF btt_auth_rate_provider.authorise_all_services <> hac_auth_rate_provider.authorise_all_services THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
	
        oErrorObject:addError(INPUT "hacrp":U,
                              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                              INPUT "":U,
                              INPUT "":U,
                              INPUT btt_auth_rate_provider.line_number,
                              INPUT "Authorise all Services Values are not the same for Provider records that belong to the same authorisation group.",
                              INPUT "ERR":U).
      END. /* IF btt_auth_rate_provider.authorise_all_services <> hac_auth_rate_provider.authorise_all_services THEN */
    END. /* IF hac_auth_rate_provider.auth_group_obj = btt_auth_rate_provider.auth_group_obj THEN */
  END. /* FOR EACH hac_auth_rate_provider */

  IF AVAILABLE buf_auth_rate_control THEN 
  DO:
    IF btt_auth_rate_provider.effective_date < buf_auth_rate_control.effective_date
    OR btt_auth_rate_provider.effective_date > buf_auth_rate_control.end_date
    THEN
      ASSIGN oplFailureOccurred = TRUE 
             lSuccess           = oErrorObject:addError(INPUT "hacrc":U, 
                                                        INPUT btt_auth_rate_provider.auth_rate_control_obj, 
                                                        INPUT "":U, 
                                                        INPUT "override_ars_rate":U,
                                                        INPUT btt_auth_rate_provider.line_number, 
                                                        INPUT "MA":U, 
                                                        INPUT 112,  /* The "&1" specified is invalid . &2*/
                                                        INPUT "Effective Date , The effective date must be within the auth rate control period.":U ).
  
    IF btt_auth_rate_provider.end_date < buf_auth_rate_control.effective_date
    OR btt_auth_rate_provider.end_date > buf_auth_rate_control.end_date
    THEN
      ASSIGN oplFailureOccurred = TRUE
             lSuccess           = oErrorObject:addError(INPUT "hacrc":U, 
                                                        INPUT btt_auth_rate_provider.auth_rate_control_obj, 
                                                        INPUT "":U, 
                                                        INPUT "override_ars_rate":U,
                                                        INPUT btt_auth_rate_provider.line_number, 
                                                        INPUT "MA":U, 
                                                        INPUT 112,  /* The "&1" specified is invalid . &2 */
                                                        INPUT "End Date , The end date must be within the auth rate control period.":U ).
  END.  /* IF AVAILABLE buf_auth_rate_control THEN */

{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
        
&ENDIF



