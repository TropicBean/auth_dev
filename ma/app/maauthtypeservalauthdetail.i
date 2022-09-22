/* maauthtypeservalauthdetail.i   MEDSTAR Medical Aid System
                                  Validate Auth Type Detail Buffer
                                  (c) Copyright 2021 - 2022
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/                      

  DEFINE PARAMETER BUFFER btt_auth_type_detail     FOR tt_auth_type_detail.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE cExclFieldsAllowed   AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cMessage             AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cStatusDescr         AS CHARACTER             NO-UNDO. 
  DEFINE VARIABLE cWhere               AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE dInsurerObj          AS DECIMAL  INITIAL 0.00 NO-UNDO.
  DEFINE VARIABLE lMandatory           AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL               NO-UNDO.
                                       
  DEFINE VARIABLE oDetailTypeInd       AS cls.mipacronym        NO-UNDO.
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject     NO-UNDO.
                                       
  DEFINE BUFFER buf_auth_type          FOR hac_auth_type.
  DEFINE BUFFER buf_auth_type_detail   FOR hac_auth_type_detail.
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider.
  DEFINE BUFFER buf_owning_acronym     FOR mic_acronym.

  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_type_detail
  THEN
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Type Detail as no buffer is available.'" }

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

  /* Ensure that a valid obj-number has been specified */
  IF btt_auth_type_detail.auth_type_detail_obj > 0 
  AND NOT CAN-FIND(FIRST buf_auth_type_detail
                   WHERE buf_auth_type_detail.auth_type_detail_obj = btt_auth_type_detail.auth_type_detail_obj) THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "auth_type_detail_obj":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "MA":U,
                          INPUT 107,  /* Could not find a "&1" record using "&2" */
                          INPUT "Auth Type Detail,Obj=" + STRING(btt_auth_type_detail.auth_type_detail_obj)).
  END.  // IF btt_auth_type_detail.auth_type_detail_obj > 0

  /* Ensure that a valid authorisation type has been specified */
  IF btt_auth_type_detail.auth_type_obj = 0.00 THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "auth_type_obj":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Type Obj,":U).
  END.  // IF btt_auth_type_detail.auth_type_obj = 0.00 THEN
  ELSE DO:
    FIND buf_auth_type NO-LOCK
         WHERE buf_auth_type.auth_type_obj = btt_auth_type_detail.auth_type_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_auth_type THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactd":U,
                            INPUT btt_auth_type_detail.auth_type_detail_obj,
                            INPUT "":U,
                            INPUT "auth_type_obj":U,
                            INPUT btt_auth_type_detail.line_number,
                            INPUT "MA":U,
                            INPUT 107,  /* Could not find a "&1" record using "&2" */
                            INPUT "Auth Type Header,Obj=" + STRING(btt_auth_type_detail.auth_type_obj)).
    END.  // IF NOT AVAILABLE buf_auth_type THEN

    /* Ensure that a valid authorisation type provider has been specified */
    IF btt_auth_type_detail.auth_type_provider_obj = 0.00 THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hactd":U,
                            INPUT btt_auth_type_detail.auth_type_detail_obj,
                            INPUT "":U,
                            INPUT "auth_type_provider_obj":U,
                            INPUT btt_auth_type_detail.line_number,
                            INPUT "MA":U,
                            INPUT 111,  /* The &1 must be specified. &2 */
                            INPUT "Auth Type Provider,":U).
    END.  // IF btt_auth_type_detail.auth_type_provider_obj = 0.00 THEN
    ELSE DO:
      FIND buf_auth_type_provider NO-LOCK
           WHERE buf_auth_type_provider.auth_type_provider_obj = btt_auth_type_detail.auth_type_provider_obj
        NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
  
      IF NOT AVAILABLE buf_auth_type_provider THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
  
        oErrorObject:addError(INPUT "hactd":U,
                              INPUT btt_auth_type_detail.auth_type_detail_obj,
                              INPUT "":U,
                              INPUT "auth_type_provider_obj":U,
                              INPUT btt_auth_type_detail.line_number,
                              INPUT "MA":U,
                              INPUT 107,  /* Could not find a "&1" record using "&2" */
                              INPUT "Auth Type Provider,Obj: " + STRING(btt_auth_type_detail.auth_type_provider_obj)).
      END.  /* IF NOT AVAILABLE buf_auth_type_provider  */
      ELSE DO:
        ASSIGN dInsurerObj = buf_auth_type_provider.insurer_obj.
        
        /*
	  Validate that no Auth Type Detail lines are allowed for Providers with status 'Exclusion'
        */
        IF buf_auth_type_provider.provider_type_indicator = "ma_acAuthProviderTypeIndicatorExcl":U THEN
	DO:
	  ASSIGN oplFailureOccurred = TRUE
	  	 cMessage 	    = "No Auth Type Detail setups are allowed for a Provider with an exclusion status".
	          
	  oErrorObject:addError(INPUT "hactd":U,
	                        INPUT btt_auth_type_detail.auth_type_detail_obj,
	                        INPUT "":U,
	                        INPUT "auth_type_provider_obj":U,
	                        INPUT btt_auth_type_detail.line_number,
	                        INPUT cMessage,
	                        INPUT "ERR":U).
  	END.  /* IF btt_auth_type_provider.provider_type_indicator = "ma_acAuthProviderTypeIndicatorExcl":U */
  
      END.  /* ELSE - IF NOT AVAILABLE buf_auth_type_provider THEN */  
    END. /* ELSE - IF btt_auth_type_detail.auth_type_provider_obj = 0.00 THEN */
  END. /* ELSE - IF btt_auth_type_detail.auth_type_obj = 0.00 THEN */

  /* Ensure that a valid auth type detail effective date has been specified */
  IF btt_auth_type_detail.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Type Detail Effective Date,":U).
  END. /* IF btt_auth_type_detail.effective_date = ? THEN */
  ELSE DO:
    IF AVAILABLE buf_auth_type_provider THEN
    DO:
      IF btt_auth_type_detail.effective_date < buf_auth_type_provider.effective_date THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
  
        oErrorObject:addError(INPUT "hactd":U,
                              INPUT btt_auth_type_detail.auth_type_detail_obj,
                              INPUT "":U,
                              INPUT "effective_date":U,
                              INPUT btt_auth_type_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Auth Type Detail effective date: " + STRING(btt_auth_type_detail.effective_date,"9999/99/99")
                                    + ",The date cannot be before the Auth Type Provider effective date: " + STRING(buf_auth_type_provider.effective_date,"9999/99/99")).
      END. /* IF btt_auth_type_detail.effective_date < buf_auth_type_provider.effective_date  */
      ELSE IF  buf_auth_type_provider.end_date <> ?
           AND btt_auth_type_detail.effective_date > buf_auth_type_provider.end_date THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
  
        oErrorObject:addError(INPUT "hactd":U,
                              INPUT btt_auth_type_detail.auth_type_detail_obj,
                              INPUT "":U,
                              INPUT "effective_date":U,
                              INPUT btt_auth_type_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Auth Type Detail effective date: " + STRING(btt_auth_type_detail.effective_date,"9999/99/99")
                                    + ",The date cannot be after the Auth Type Provider end date: " + STRING(buf_auth_type_provider.end_date,"9999/99/99")).
      END. /* IF  buf_auth_type_provider.end_date <> ? */
    END.  // IF AVAILABLE buf_auth_type_provider THEN
     
    IF AVAILABLE buf_auth_type THEN
    DO:
      IF btt_auth_type_detail.effective_date < buf_auth_type.effective_date THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
  
        oErrorObject:addError(INPUT "hactd":U,
                              INPUT btt_auth_type_detail.auth_type_detail_obj,
                              INPUT "":U,
                              INPUT "effective_date":U,
                              INPUT btt_auth_type_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Auth Type Detail effective date: " + STRING(btt_auth_type_detail.effective_date,"9999/99/99")
                                    + ",The date cannot be before the Auth Type effective date: " + STRING(buf_auth_type.effective_date,"9999/99/99")).
      END. /* IF btt_auth_type_detail.effective_date < buf_auth_type.effective_date  */
    END.  // IF AVAILABLE buf_auth_type THEN
    
    /* If an end date is supplied, ensure that is not before the effective date */
    IF  btt_auth_type_detail.end_date <> ? THEN
    DO:
      IF btt_auth_type_detail.end_date < btt_auth_type_detail.effective_date THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
      
        oErrorObject:addError(INPUT "hactd":U,
                              INPUT btt_auth_type_detail.auth_type_detail_obj,
                              INPUT "":U,
                              INPUT "end_date":U,
                              INPUT btt_auth_type_detail.line_number,
                              INPUT "MA":U,
                              INPUT 11,  /* The End Date &1 cannot be before the Effective/Start Date &2 */
                              INPUT "(" + STRING(btt_auth_type_detail.end_date,"9999/99/99") + "),(" + STRING(btt_auth_type_detail.effective_date,"9999/99/99") + ")").
      END.  /* IF btt_auth_type_detail.end_date <> ? */
      ELSE IF  AVAILABLE buf_auth_type_provider
           AND buf_auth_type_provider.end_date <> ?
           AND btt_auth_type_detail.end_date    > buf_auth_type_provider.end_date THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.
      
        oErrorObject:addError(INPUT "hactd":U,
                              INPUT btt_auth_type_detail.auth_type_detail_obj,
                              INPUT "":U,
                              INPUT "end_date":U,
                              INPUT btt_auth_type_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Auth Type Detail end date: " + STRING(btt_auth_type_detail.end_date,"9999/99/99")
                                    + ",The date cannot be after the Auth Type Provider end date: " + STRING(buf_auth_type_provider.end_date,"9999/99/99")).
      END. /* IF  buf_auth_type_provider.end_date <> ? */
    END.  /* IF  btt_auth_type_detail.end_date <> ? THEN */
  END.  /* ELSE IF btt_auth_type_detail.effective_date = ? THEN */

  /* Validate detail type indicator */
  IF btt_auth_type_detail.detail_type_indicator = "":U THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "detail_type_indicator":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Detail Type Indicator,":U).
  END.  /* IF btt_auth_type_detail.detail_type_indicator = "":U THEN */
  ELSE DO:
    ASSIGN oDetailTypeInd = NEW cls.mipacronym(?,FALSE, "ma_acAuthDetailTypeIndicator":U, ?) NO-ERROR.

    IF NOT VALID-OBJECT(oDetailTypeInd) THEN
    DO:
      { mip/inc/mipreturnerror.i &ifCondition = "NOT VALID-OBJECT(oDetailTypeInd)"
                                 &errorText   = "~{ mip/inc/miperrortext.i 'wf_MsgAMErr' 100 ~}" }.
    END.  // IF NOT VALID-OBJECT(oDetailTypeInd) THEN
    ELSE DO:
      oDetailTypeInd:focusAcronym("KEY":U, btt_auth_type_detail.detail_type_indicator) NO-ERROR.
      
      IF NOT oDetailTypeInd:AcronymInFocus THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cMessage           = SUBSTITUTE("The Auth Detail Type Indicator (&1) specified could not be found.":U, btt_auth_type_detail.detail_type_indicator).
      
        oErrorObject:addError(INPUT "hactd":U,                     
                              INPUT btt_auth_type_detail.auth_type_detail_obj,    
                              INPUT "":U,                                           
                              INPUT "detail_type_indicator":U,                                
                              INPUT btt_auth_type_detail.line_number,                    
                              INPUT cMessage,                                                                           
                              INPUT "ERR":U).
      END.  /* IF NOT oDetailTypeInd:AcronymInFocus THEN  */     
      ELSE DO:
        /* Detail Type Indicator is an exclusion */
        IF btt_auth_type_detail.detail_type_indicator = "ma_acAuthDetailTypeIndicatorExcl":U THEN
        DO:
          /*
            The fields listed below are the only fields that MAY contain  
            values when the detail_type_indicator is an exclusion.
            If any other field in the table contains a value, it will be cleared
            in the function and a warning message will be returned.
          */
          ASSIGN cMessage           = ""
                 cExclFieldsAllowed = "auth_type_obj,auth_type_provider_obj,auth_type_detail_obj,detail_type_indicator,":U   +
                                      "effective_date,end_date,default_auth_status,default_auth_status_note,":U +
                                      "owning_entity_mnemonic,owning_key,owning_obj,owning_alt_value":U.
        
          ASSIGN cMessage = fnValidateExclusionValues(INPUT TEMP-TABLE btt_auth_type_detail:HANDLE,        // The name of the buffer that is validated
                                                      INPUT btt_auth_type_detail.auth_type_detail_obj,     // The unique obj of the buffer that is validated
                                                      INPUT cExclFieldsAllowed).                           // The list of fields that MAY contain a value
          IF cMessage <> ""
          THEN
            oErrorObject:addError(INPUT "hactd":U,
                                  INPUT btt_auth_type_detail.auth_type_detail_obj,
                                  INPUT "":U,
                                  INPUT "detail_type_indicator":U,
                                  INPUT btt_auth_type_detail.line_number,
                                  INPUT cMessage,
                                  INPUT "WAR":U).
          
          /* Validate default status */
          IF btt_auth_type_detail.default_auth_status <> "6":U THEN
          DO:
            ASSIGN oplFailureOccurred = TRUE.
              
            oErrorObject:addError(INPUT "hactd":U,
                                  INPUT btt_auth_type_detail.auth_type_detail_obj,
                                  INPUT "":U,
                                  INPUT "default_auth_status":U,
                                  INPUT btt_auth_type_detail.line_number,
                                  INPUT "MA":U,
                                  INPUT 111,  /* The &1 must be specified. &2 */
                                  INPUT "Auth Default Status, Change the Default Status to 'Declined'.").
          END.  /* IF  btt_auth_type_detail.default_auth_status   <> "6" */                                                                                      
          
          /* Validate default status note */
          IF btt_auth_type_detail.default_auth_status <> "":U THEN 
          DO:
            IF btt_auth_type_detail.default_auth_status_note = "":U THEN 
            DO:
              ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(btt_auth_type_detail.default_auth_status),
                                                                                  INPUT dInsurerObj,
                                                                                  INPUT 0,
                                                                                  INPUT btt_auth_type_detail.effective_date).
              IF lMandatory THEN 
              DO:
                ASSIGN oplFailureOccurred = TRUE
                       cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                                     INPUT INTEGER(btt_auth_type_detail.default_auth_status))
                       cMessage     = "The Detail Default Status Reason is mandatory for auth status '" + cStatusDescr
                                    + "'.[HELP=Auth Rule Code: EnforceStatusNote]":U.
                oErrorObject:addError(INPUT "hactd":U,
                                      INPUT btt_auth_type_detail.auth_type_detail_obj,
                                      INPUT "",
                                      INPUT "default_auth_status_note":U,
                                      INPUT btt_auth_type_detail.line_number,
                                      INPUT cMessage,
                                      INPUT "ERR":U).                                                                               
              END. /* IF lMandatory */
            END.  /* IF btt_auth_type_detail.default_auth_status_note = "":U THEN */
            ELSE IF btt_auth_type_detail.default_auth_status_note <> "" THEN
            DO:
              mipEnv:Health:AuthService:ValidateStatusReason(INPUT  dInsurerObj, 
                                                             INPUT  0,
                                                             INPUT  btt_auth_type_detail.default_auth_status_note,
                                                             INPUT  INTEGER(btt_auth_type_detail.default_auth_status),
                                                             INPUT  btt_auth_type_detail.effective_date,
                                                             OUTPUT cMessage).
          
              IF cMessage <> "":U THEN
              DO:
                ASSIGN oplFailureOccurred = TRUE.
              
                oErrorObject:addError(INPUT "hactd":U,
                                      INPUT btt_auth_type_detail.auth_type_detail_obj,
                                      INPUT "":U,
                                      INPUT "default_auth_status_note":U,
                                      INPUT btt_auth_type_detail.line_number,
                                      INPUT cMessage,
                                      INPUT "ERR":U).
              END.  // IF cErrorMessage <> "":U THEN
            END.  // ELSE IF btt_auth_type_detail.default_auth_status_note <> "" THEN
          END.  // IF  btt_auth_type_detail.default_auth_status <> "":U THEN

          IF btt_auth_type_detail.pr_type > 0  THEN
          DO:
            ASSIGN oplFailureOccurred = TRUE.
             
            oErrorObject:addError(INPUT "hactd":U,
                                  INPUT btt_auth_type_detail.auth_type_detail_obj,
                                  INPUT "":U,
                                  INPUT "pr_type":U,
                                  INPUT  btt_auth_type_detail.line_number,
                                  INPUT "MA":U,
                                  INPUT 111,  /* The "&1" specified is invalid.&2 */
                                  INPUT SUBSTITUTE("Discipline,Discipline can not be specified for an exclusion.")).
          END. //IF btt_auth_type_detail.pr_type > 0 

          IF btt_auth_type_detail.quantity_auth > 0 THEN
          DO:
            ASSIGN oplFailureOccurred = TRUE.
            
            oErrorObject:addError(INPUT "hactd":U,
                                  INPUT btt_auth_type_detail.auth_type_detail_obj,
                                  INPUT "":U,
                                  INPUT "quantity_auth":U,
                                  INPUT  btt_auth_type_detail.line_number,
                                  INPUT "MA":U,
                                  INPUT 111,  /* The "&1" specified is invalid.&2 */
                                  INPUT SUBSTITUTE("Quantity Auth,Quantity Auth can not be specified for an exclusion.")). 
          END.  //IF btt_auth_type_detail.quantity_auth > 0 

        END.  // IF btt_auth_type_detail.detail_type_indicator = "ma_acAuthDetailTypeIndicatorExcl":U THEN
        ELSE DO:
          IF btt_auth_type_detail.default_auth_status <> "":U THEN
          DO:
            ASSIGN cMessage = "Default Auth Status " +
                              IF btt_auth_type_detail.default_auth_status_note <> "":U THEN "and Default Auth Status Note " ELSE "" +
                              "only allowed for exclusions. Status fields cleared."
                   btt_auth_type_detail.default_auth_status      = "":U
                   btt_auth_type_detail.default_auth_status_note = "":U.

            oErrorObject:addError(INPUT "hactd":U,
                                  INPUT btt_auth_type_detail.auth_type_detail_obj,
                                  INPUT "":U,
                                  INPUT "detail_type_indicator":U,
                                  INPUT btt_auth_type_detail.line_number,
                                  INPUT cMessage,
                                  INPUT "WAR":U).
          
          END.  // IF btt_auth_type_detail.default_auth_status <> "":U THEN
        END.  // ELSE - IF btt_auth_type_detail.detail_type_indicator = "ma_acAuthDetailTypeIndicatorExcl":U THEN
      END.  //  ELSE - IF NOT oDetailTypeInd:AcronymInFocus THEN
    END.  // ELSE - IF NOT VALID-OBJECT(oDetailTypeInd) THEN
  END.  // ELSE - IF btt_auth_type_detail.detail_type_indicator = "":U THEN

  /* Validate owning entity mnemonic */
  IF btt_auth_type_detail.owning_entity_mnemonic = "":U 
  OR btt_auth_type_detail.owning_entity_mnemonic = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "owning_entity_mnemonic":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Type Detail Entity,":U).
  END.
  ELSE DO:
    FIND FIRST buf_owning_acronym NO-LOCK
         WHERE buf_owning_acronym.category_key  = "ma_acAuthTypeDetailEntities":U
         AND   buf_owning_acronym.acronym_value = btt_auth_type_detail.owning_entity_mnemonic 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_owning_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactd":U,
                            INPUT btt_auth_type_detail.auth_type_detail_obj,
                            INPUT "":U,
                            INPUT "owning_entity_mnemonic":U,
                            INPUT btt_auth_type_detail.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Type Detail Entity (":U + btt_auth_type_detail.owning_entity_mnemonic + ")":U).
    END. /* IF NOT AVAILABLE buf_owning_acronym THEN */
  END. /* ELSE - IF btt_auth_type_detail.owning_entity_mnemonic = "":U */

  /* Validate owning key and owning obj */
  IF  btt_auth_type_detail.owning_key = "":U
  AND btt_auth_type_detail.owning_obj = 0 THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
      
    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT IF btt_auth_type_detail.owning_key = "":U
                                THEN "owning_key":U
                                ELSE "owning_obj":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "Either the Owning Obj or the Owning Key must be specified",                                                                           
                          INPUT "ERR":U).
  END.  // IF btt_auth_type_detail.owning_key = "":U AND btt_auth_type_detail.owning_obj = 0 THEN
  ELSE DO:
    /* Validate owning alt value */
    IF btt_auth_type_detail.owning_alt_value = "" THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE
             cMessage           = IF AVAILABLE buf_owning_acronym
                                  THEN SUBSTITUTE("The &1 must be specified.","Owning Value for the ":U + buf_owning_acronym.acronym_label)
                                  ELSE SUBSTITUTE("The &1 must be specified.","Owning Value":U).

      oErrorObject:addError(INPUT "hactd":U,
                            INPUT btt_auth_type_detail.auth_type_detail_obj,
                            INPUT "":U,
                            INPUT "owning_alt_value":U,
                            INPUT btt_auth_type_detail.line_number,
                            INPUT cMessage,
                            INPUT "ERR":U).

    END.  /* IF btt_auth_type_detail.owning_alt_value = "" THEN */
    
    /* Ensure that the entity specified, is valid */
    ELSE IF AVAILABLE buf_owning_acronym THEN
    DO:
      /* Validate that a Crosswalk entity is a valid Basket */
      DO:
        mipEnv:miDBEntity:focusTable(buf_owning_acronym.acronym_value).
        
        IF NOT mipEnv:miDBEntity:InFocus THEN
        DO:
          ASSIGN oplFailureOccurred = TRUE
                 cMessage           = "Invalid owning table '" + buf_owning_acronym.acronym_value + "' specified.".
        
          oErrorObject:addError(INPUT "hactd":U,
                                INPUT btt_auth_type_detail.auth_type_detail_obj,
                                INPUT "":U,
                                INPUT "owning_entity_mnemonic":U,
                                INPUT btt_auth_type_detail.line_number,
                                INPUT cMessage,
                                INPUT "ERR":U).                  
        END.  // IF NOT mipEnv:miDBEntity:InFocus THEN
        ELSE DO:
          CASE btt_auth_type_detail.owning_entity_mnemonic: 
            WHEN "hlmcr":U THEN  // hlm_crosswalk (basket)
            DO:
              ASSIGN cWhere   =           mipEnv:miDBEntity:TableName + ".crosswalk_obj  = ":U + STRING(btt_auth_type_detail.owning_obj) +
                                " AND " + mipEnv:miDBEntity:TableName + ".crosswalk_type = 'ma_acCrossTypeBasket'":U
                     lSuccess = mipEnv:miDBEntity:findRecordWhere(cWhere).
            END.  // WHEN "hlmcr":U THEN

            WHEN "htmtl":U THEN // htm_tariff_link
            DO:
              ASSIGN cWhere   =           mipEnv:miDBEntity:TableName + ".tariff_link_obj     = ":U + STRING(btt_auth_type_detail.owning_obj) +
                                " AND " + mipEnv:miDBEntity:TableName + ".tariff_link_default = yes":U
                     lSuccess = mipEnv:miDBEntity:findRecordWhere(cWhere).
            END.  // WHEN "htmtl":U THEN

            WHEN "htmtt":U THEN  // htm_tariff_type
            DO:
              ASSIGN cWhere   =           mipEnv:miDBEntity:TableName + ".tariff_type_obj = ":U + STRING(btt_auth_type_detail.owning_obj) +
                                " AND " + mipEnv:miDBEntity:TableName + ".acronym_key     = 'ma_acTariffTypeUsageControl'":U
                     lSuccess = mipEnv:miDBEntity:findRecordWhere(cWhere).
            END.  // WHEN "htmtl":U THEN
            WHEN "hlmnl":U THEN  // hlm_nappi_link
            DO:
              ASSIGN cWhere   =           mipEnv:miDBEntity:TableName + ".nappi_code_prefix = ":U + btt_auth_type_detail.owning_alt_value +
	                        " OR " +  mipEnv:miDBEntity:TableName + ".nappi_link_obj    = ":U + STRING(btt_auth_type_detail.owning_obj) 
                     lSuccess = mipEnv:miDBEntity:findRecordWhere(cWhere).
            END.  // WHEN "hlmnl":U THEN 
            OTHERWISE
              DO:
                ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                   THEN mipEnv:miDBEntity:findRecord(btt_auth_type_detail.owning_obj)
                                   ELSE mipEnv:miDBEntity:findRecord(btt_auth_type_detail.owning_key)).
              END.  // OTHERWISE DO:
          END CASE.
          
          IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
          DO:
            ASSIGN oplFailureOccurred = TRUE
                   cMessage           = "Could not find an owning " + buf_owning_acronym.acronym_label + " record using '" +
                                        (IF btt_auth_type_detail.owning_key <> "" 
                                         THEN btt_auth_type_detail.owning_key 
                                         ELSE btt_auth_type_detail.owning_alt_value) + "'":U.
        
            oErrorObject:addError(INPUT "hactd":U,
                                  INPUT btt_auth_type_detail.auth_type_detail_obj,
                                  INPUT "":U,
                                  INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                         THEN "owning_obj":U 
                                         ELSE "owning_key":U),
                                  INPUT btt_auth_type_detail.line_number,
                                  INPUT cMessage,
                                  INPUT "ERR":U).
          END.  // IF NOT mipEnv:miDBEntity:RecordAvailable
        END.  // ELSE - IF NOT mipEnv:miDBEntity:InFocus THEN
      END.  // ELSE - IF btt_auth_type_detail.owning_entity_mnemonic = "hlmcr":U THEN
    END.  // ELSE IF AVAILABLE buf_owning_acronym THEN
  END.  /* ELSE - IF btt_auth_type_detail.owning_key = "":U AND btt_auth_type_detail.owning_obj = 0 THEN */

  /* Validate the auth usage limit */
  IF btt_auth_type_detail.auth_usage_limit = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "auth_usage_limit":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "Authorisation usage limit cannot contain an unknown value.", 
                          INPUT "ERR":U).
  END.  /* IF btt_auth_type_detail.auth_usage_limit <> ? THEN */
  ELSE IF btt_auth_type_detail.auth_usage_limit < 0 THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "auth_usage_limit":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "Authorisation usage limit cannot contain a negative value.", 
                          INPUT "ERR":U).
  END.  /* ELSE IF btt_auth_type_detail.auth_usage_limit < 0 THEN */

  /* Validate the creation of an automatic auth detail line */
  IF btt_auth_type_detail.auth_auto_create = ? 
  AND btt_auth_type_detail.detail_type_indicator <> "ma_acAuthDetailTypeIndicatorExcl":U THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "auth_auto_create":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "Indicator for the automatic creation of the Authorisation detail line cannot contain an unknown value.",
                          INPUT "ERR":U).
  END.  /* IF btt_auth_type_detail.auth_auto_create = ? THEN */
  ELSE IF LOOKUP(btt_auth_type_detail.owning_entity_mnemonic,"hlmcr,htmtl,hlmnl":U) = 0  
       AND btt_auth_type_detail.auth_auto_create = YES THEN 
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactd":U,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "auth_auto_create":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "Indicator for the automatic creation of the Authorisation detail line not allowed for this Entity.",
                          INPUT "ERR":U).
  END.  /* ELSE IF LOOKUP(btt_auth_type_detail.owning_entity_mnemonic,"hlmcr,htmtl,hlmnl":U) = 0 */

  /* Validate default line restriction */
  IF btt_auth_type_detail.default_line_restriction <> "":U THEN
  DO:
    ASSIGN lSuccess = mipEnv:Health:AuthService:validateLineRestriction(INPUT btt_auth_type_detail.default_line_restriction).

    IF NOT lSuccess THEN   
    DO: 
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactd:":U + btt_auth_type_detail.owning_entity_mnemonic,
                            INPUT btt_auth_type_detail.auth_type_detail_obj,
                            INPUT "":U,
                            INPUT "default_line_restriction":U,
                            INPUT btt_auth_type_detail.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Default line restriction ":U + btt_auth_type_detail.default_line_restriction).
    END.  /* IF NOT lSuccess THEN */
  END. /* IF btt_auth_type_detail.default_line_restriction <> "":U THEN */

  IF btt_auth_type_detail.pr_type = ?
  OR btt_auth_type_detail.pr_type < 0  THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "hactd":U ,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "pr_type":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "MA":U,
                          INPUT 100,  /* The "&1" specified is invalid */
                          INPUT "Discipline":U ).
                          
  END. /* IF btt_auth_type_detail.pr_type = ? THEN */
  ELSE DO:
    IF btt_auth_type_detail.pr_type > 0  THEN
    DO:
      mipEnv:Health:maAdministration:validateDiscipline(INPUT  STRING(btt_auth_type_detail.pr_type),
                                                        INPUT  0,
                                                        OUTPUT cMessage).
                                                        
      IF cMessage <> "":U THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
         
        oErrorObject:addError(INPUT "hactd":U,
                              INPUT btt_auth_type_detail.auth_type_detail_obj,
                              INPUT "":U,
                              INPUT "pr_type":U,
                              INPUT  btt_auth_type_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The "&1" specified is invalid.&2 */
                              INPUT SUBSTITUTE("Discipline," + cMessage)).     
      END. /* IF cMessage <> "":U THEN */                              
    END.                                                    
  END. /*  ELSE DO (IF btt_auth_type_detail.pr_type = ? OR btt_auth_type_detail.pr_type < 0) */ 

  IF btt_auth_type_detail.quantity_auth = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
  
    oErrorObject:addError(INPUT "hactd":U ,
                          INPUT btt_auth_type_detail.auth_type_detail_obj,
                          INPUT "":U,
                          INPUT "quantity_auth":U,
                          INPUT btt_auth_type_detail.line_number,
                          INPUT "MA":U,
                          INPUT 100,  /* The "&1" specified is invalid */
                          INPUT "Quantity Auth":U ).
  END. /* IF btt_auth_type_detail.quantity_auth = ? THEN */
  

  { mip/inc/mipcatcherror.i
      &FINALLY = "IF VALID-OBJECT(oErrorObject)    THEN DELETE OBJECT oErrorObject.
                  IF VALID-OBJECT(oDetailTypeInd)  THEN DELETE OBJECT oDetailTypeInd."}

&ENDIF      


