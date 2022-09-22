/* maauthtypeservalautpcontrol.i  MEDSTAR Medical Aid System
                                  Validate Auth Type Control Buffer
                                  (c) Copyright 2018 - 2022
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/                      

  DEFINE PARAMETER BUFFER btt_auth_type_control FOR tt_auth_type_control.
  
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cAgeRangeObj        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cAuthType           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cClaimTypeExclusion AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cExclFieldsAllowed  AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cField              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cField1             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cFieldname          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cGender             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessage            AS CHARACTER           NO-UNDO. 
  DEFINE VARIABLE cStatusDescr        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE dEffDate            AS DATE                NO-UNDO.
  DEFINE VARIABLE iCCDisallow         AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iClaimCodeExclusion AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iCnt                AS INTEGER             NO-UNDO.
  DEFINE VARIABLE dAgeRangeObj        AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE lMandatory          AS LOGICAL             NO-UNDO.  
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO. 
  DEFINE VARIABLE lValidStatus        AS LOGICAL             NO-UNDO.
  
  DEFINE VARIABLE iClaimCode          AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iCount              AS INTEGER             NO-UNDO.
  DEFINE VARIABLE cClaimType          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cInvalidClaimCodes  AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cInvalidClaimTypes  AS CHARACTER           NO-UNDO.
  
  DEFINE VARIABLE oControlTypeInd     AS cls.mipacronym      NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE oUsageType          AS cls.mipacronym      NO-UNDO.

  DEFINE BUFFER buf_auth_type         FOR hac_auth_type.
  DEFINE BUFFER buf_auth_type_control FOR hac_auth_type_control.

  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_type_control
  THEN
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Type Control as no buffer is available.'" }

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

  /* Ensure that a valid authorisation type has been specified */
  IF btt_auth_type_control.auth_type_obj <> 0.00 THEN
  DO:
    FIND buf_auth_type NO-LOCK
      WHERE buf_auth_type.auth_type_obj = btt_auth_type_control.auth_type_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_auth_type THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "auth_type_obj":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 101,  /* The "&1" is not available */
                            INPUT "Auth Type Header ":U).
    END.  /* IF NOT AVAILABLE buf_auth_type  */
  END. /* IF btt_auth_type_control.auth_type_obj <> 0.00  */

  /* Ensure that a valid auth type effective date has been specified */
  IF btt_auth_type_control.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactc":U,
                          INPUT btt_auth_type_control.auth_type_control_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_type_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Type Control Effective Date,":U).
  END. /* IF btt_auth_type_control.effective_date = ? THEN */
  ELSE DO:
    IF btt_auth_type_control.effective_date < buf_auth_type.effective_date THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "effective_date":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 112,  /* The &1 specified is invalid. &2 */
                            INPUT "Auth Type Control effective date: " + STRING(btt_auth_type_control.effective_date,"9999/99/99")
                                  + ",The date cannot be before the Auth Type effective date: " + STRING(buf_auth_type.effective_date,"9999/99/99")).
    END. /* IF btt_auth_type_control.effective_date < buf_auth_type.effective_date  */
  END. /* ELSE - IF btt_auth_type_control.effective_date = ? THEN */

  /* If an end date is supplied, ensure that is not before the effective date */
  IF  btt_auth_type_control.end_date <> ? 
  AND btt_auth_type_control.end_date < btt_auth_type_control.effective_date THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactc":U,
                          INPUT btt_auth_type_control.auth_type_control_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_type_control.line_number,
                          INPUT "MA":U,
                          INPUT 11,  /* The End Date &1 cannot be before the Effective/Start Date &2 */
                          INPUT "(" + STRING(btt_auth_type_control.end_date,"9999/99/99") + 
                                "),(" + STRING(btt_auth_type_control.effective_date,"9999/99/99") + ")").
  END. /* IF btt_auth_type_control.end_date <> ? */

  /* Ensure that the period_override indicator is yes/no */
  IF btt_auth_type_control.period_override = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactc":U,
                          INPUT btt_auth_type_control.auth_type_control_obj,
                          INPUT "":U,
                          INPUT "period_override":U,
                          INPUT btt_auth_type_control.line_number,
                          INPUT "The Period override indicator cannot contain an unknown value.",
                          INPUT "ERR":U).
  END. /* IF btt_auth_type_control.period_override = ? THEN */

  /* Ensure that a valid insurer has been specified */
  IF btt_auth_type_control.insurer_obj <> 0.00 THEN
  DO:
    FIND erm_insurer NO-LOCK
      WHERE erm_insurer.insurer_obj = btt_auth_type_control.insurer_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "insurer_obj":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Type Control Client ":U + STRING(btt_auth_type_control.insurer_obj)).
    END.  /* IF NOT AVAILABLE erm_insurer THEN */
  END. /* IF btt_auth_type_control.insurer_obj <> 0.00 THEN */

  /* Validate control type indicator */
  IF btt_auth_type_control.control_type_indicator = "":U THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactc":U,
                          INPUT btt_auth_type_control.auth_type_control_obj,
                          INPUT "":U,
                          INPUT "control_type_indicator":U,
                          INPUT btt_auth_type_control.line_number,
                          INPUT "MA":U,
                          INPUT 112,  /* The &1 specified is invalid. &2 */
                          INPUT "Auth Type Control Type Indicator" + ",Please enter a valid value.").
  END. /* IF btt_auth_type_control.control_type_indicator = "":U THEN */
  ELSE DO:
    ASSIGN oControlTypeInd = NEW cls.mipacronym(?,FALSE, "ma_acAuthControlTypeIndicator":U, ?) NO-ERROR.

    IF NOT VALID-OBJECT(oControlTypeInd) THEN
    DO:
      { mip/inc/mipreturnerror.i &ifCondition = "NOT VALID-OBJECT(oControlTypeInd)"
                                 &errorText   = "~{ mip/inc/miperrortext.i 'wf_MsgAMErr' 100 ~}" }.
    END.  /* IF NOT VALID-OBJECT(oControlTypeInd) THEN */
    ELSE DO:

      oControlTypeInd:focusAcronym("KEY":U, btt_auth_type_control.control_type_indicator) NO-ERROR.
      
      IF NOT oControlTypeInd:AcronymInFocus THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cMessage           = SUBSTITUTE("The Control Type Indicator (&1) specified could not be found.":U, 
                                               btt_auth_type_control.control_type_indicator).
      
        oErrorObject:addError(INPUT "hactc":U,                     
                              INPUT btt_auth_type_control.auth_type_control_obj,    
                              INPUT "":U,                                           
                              INPUT "control_type_indicator":U,                                
                              INPUT btt_auth_type_control.line_number,                    
                              INPUT cMessage,                                                                           
                              INPUT "ERR":U).
      END. /* IF NOT oControlTypeInd:AcronymInFocus  */     
      ELSE 
      /* Control Type Indicator is an Exclusion */
      IF btt_auth_type_control.control_type_indicator = "ma_acAuthControlTypeIndicatorExcl":U THEN
      DO:
            /* Validate claim code */
            FIND FIRST schext NO-LOCK
              WHERE schext.scheme-code = btt_auth_type_control.option_code
              NO-ERROR.

            IF NOT AVAILABLE schext
            THEN
              FIND FIRST schext NO-LOCK NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE schext
            THEN ASSIGN iClaimCodeExclusion = schext.claim-code[1]
                        cClaimTypeExclusion = schext.claim-type[1].
            ELSE ASSIGN iClaimCodeExclusion = 99
                        cClaimTypeExclusion = "":U.
                             
        IF btt_auth_type_control.claim_codes_header <> STRING(iClaimCodeExclusion) THEN
        DO: 
          ASSIGN cMessage = SUBSTITUTE("Invalid claim code (&1) for an exclusion. Claim Code changed to '&2'.",
                                       STRING(btt_auth_type_control.claim_codes_header), STRING(iClaimCodeExclusion)).

          oErrorObject:addError(INPUT "hactc":U,
                                INPUT btt_auth_type_control.auth_type_control_obj,
                                INPUT "":U,
                                INPUT "claim_codes_header":U,
                                INPUT btt_auth_type_control.line_number,
                                INPUT cMessage,
                                INPUT "WAR").
  
          ASSIGN btt_auth_type_control.claim_codes_header = STRING(iClaimCodeExclusion).
                
        END.  /* IF btt_auth_type_control.claim_codes_header <> iClaimCodeExclusion THEN */
      
        IF btt_auth_type_control.claim_types_header <> cClaimTypeExclusion THEN
        DO:
          ASSIGN cMessage = SUBSTITUTE("Invalid claim type (&1) for an exclusion. Claim Type changed to '&2'.",
                                       btt_auth_type_control.claim_types_header, cClaimTypeExclusion).

          oErrorObject:addError(INPUT "hactc":U,
                                INPUT btt_auth_type_control.auth_type_control_obj,
                                INPUT "":U,
                                INPUT "claim_types_header":U,
                                INPUT btt_auth_type_control.line_number,
                                INPUT cMessage,
                                INPUT "WAR").
      
          ASSIGN btt_auth_type_control.claim_types_header = cClaimTypeExclusion.
      
        END.  /* IF btt_auth_type_control.claim_types_header <> iClaimTypeExclusion THEN */
      
      
        /* Validate default status for exclusions */
        IF btt_auth_type_control.default_auth_status <> "6":U THEN
        DO:
          ASSIGN btt_auth_type_control.default_auth_status = "6":U.
      
          oErrorObject:addError(INPUT "hactc":U,
                                INPUT btt_auth_type_control.auth_type_control_obj,
                                INPUT "":U,
                                INPUT "default_auth_status":U,
                                INPUT btt_auth_type_control.line_number,
                                INPUT "Invalid Default Auth Status for an exclusion. Auth Status changed to 'Declined'.",
                                INPUT "WAR").
        END.  /* IF  btt_auth_type_control.default_auth_status   <> "6" */
        /* Validate Default Auth Status Note */
        ASSIGN dEffDate = IF btt_auth_type_control.effective_date <> ?
                          THEN btt_auth_type_control.effective_date
                          ELSE TODAY.
        
        IF btt_auth_type_control.default_auth_status_note = "":U THEN
        DO:
          ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(btt_auth_type_control.default_auth_status),
                                                                              INPUT btt_auth_type_control.insurer_obj,
                                                                              INPUT 00,
                                                                              INPUT dEffDate).
          IF lMandatory THEN 
          DO:
            ASSIGN oplFailureOccurred = TRUE
                   cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                                 INPUT INTEGER(btt_auth_type_control.default_auth_status))
                   cMessage     = "The Control Default Status Reason is mandatory for auth status '" + cStatusDescr 
                                + "'.[HELP=Auth Rule Code: EnforceStatusNote]":U.
            oErrorObject:addError(INPUT "hactc":U,
                                  INPUT btt_auth_type_control.auth_type_control_obj,
                                  INPUT "",
                                  INPUT "default_auth_status_note":U,
                                  INPUT btt_auth_type_control.line_number,
                                  INPUT cMessage,
                                  INPUT "ERR":U).
          END. /* IF lMandatory */
        END.  /* IF btt_auth_type_control.default_auth_status_note = "":U THEN */
        ELSE DO:
          mipEnv:Health:AuthService:ValidateStatusReason(INPUT  btt_auth_type_control.insurer_obj,
                                                         INPUT  00,
                                                         INPUT  btt_auth_type_control.default_auth_status_note,
                                                         INPUT  INTEGER(btt_auth_type_control.default_auth_status),
                                                         INPUT  dEffDate,
                                                         OUTPUT cMessage).
        
          IF cMessage <> "":U THEN
          DO:
            ASSIGN oplFailureOccurred = TRUE.
        
            oErrorObject:addError(INPUT "hactc":U,
                                  INPUT btt_auth_type_control.auth_type_control_obj,
                                  INPUT "":U,
                                  INPUT "default_auth_status_note":U,
                                  INPUT btt_auth_type_control.line_number,
                                  INPUT cMessage,
                                  INPUT "ERR":U).
          END. /* IF cMessage <> "":U THEN  */
        END.  /* ELSE IF btt_auth_type_control.default_auth_status_note <> "" THEN */
        
        /*
          The fields listed below are the only fields that MAY contain  
          values when the control_type_indicator is an exclusion.
          If any other field in the table contains a value, it will be cleared
          in the function and a warning message will be returned.
        */
        ASSIGN cMessage           = "":U
               cExclFieldsAllowed = "auth_type_obj,auth_type_control_obj,insurer_obj,option_code,control_type_indicator,":U        +
                                    "effective_date,end_date,gender,age_range_female_obj,age_range_male_obj,age_range_both_obj,":U +
                                    "default_auth_status,default_auth_status_note,default_claim_code_detail,default_claim_type_detail":U +
                                    ",claim_codes_header,claim_types_header":U  .

        ASSIGN cMessage = fnValidateExclusionValues(INPUT TEMP-TABLE btt_auth_type_control:HANDLE,     // The name of the buffer that is validated
                                                    INPUT btt_auth_type_control.auth_type_control_obj, // The unique obj of the buffer that is validated
                                                    INPUT cExclFieldsAllowed).                         // The list of fields that MAY contain a value
      
        IF cMessage <> ""
        THEN
          oErrorObject:addError(INPUT "hactc":U,
                                INPUT btt_auth_type_control.auth_type_control_obj,
                                INPUT "":U,
                                INPUT "control_type_indicator":U,
                                INPUT btt_auth_type_control.line_number,
                                INPUT cMessage,
                                INPUT "WAR":U).
        
        IF  btt_auth_type_control.option_code          = 0 
        AND btt_auth_type_control.age_range_both_obj   = 0.00 
        AND btt_auth_type_control.age_range_male_obj   = 0.00  
        AND btt_auth_type_control.age_range_female_obj = 0.00        
        AND btt_auth_type_control.gender               = "":U 
        THEN DO: 
          cMessage = "When an exclusion record is added one of the following fields must be captured:"
                   + "Option, Gender,age range male,age range female or age range both.".

          oErrorObject:addError(INPUT "hactc":U,
                                INPUT btt_auth_type_control.auth_type_control_obj,
                                INPUT "":U,
                                INPUT "control_type_indicator":U,
                                INPUT btt_auth_type_control.line_number,
                                INPUT cMessage,
                                INPUT "Err").

        END. /*IF btt_auth_type_control.option_code = 0 AND btt_auth_type_control.age_range_both_obj   = 0.00 ...... */
      END.  /* ELSE IF btt_auth_type_control.control_type_indicator = "ma_acAuthControlTypeIndicatorExcl":U THEN */
    END.  /* ELSE - IF NOT VALID-OBJECT(oControlTypeInd) THEN */
  END.  /* ELSE - IF btt_auth_type_control.control_type_indicator = "":U THEN */
  
  /* Validate default status for non-exclusions */
  IF  btt_auth_type_control.control_type_indicator <> "ma_acAuthControlTypeIndicatorExcl":U 
  AND btt_auth_type_control.default_auth_status    <> "":U THEN
  DO:
    ASSIGN btt_auth_type_control.default_auth_status = "":U.
      
          oErrorObject:addError(INPUT "hactc":U,
                                INPUT btt_auth_type_control.auth_type_control_obj,
                                INPUT "":U,
                                INPUT "default_auth_status":U,
                                INPUT btt_auth_type_control.line_number,
                                INPUT "Default Auth Status may only be entered for an exclusion. Default Auth Status cleared.",
                                INPUT "WAR").
  END.  /* IF btt_auth_type_control.control_type_indicator <> "ma_acAuthControlTypeIndicatorExcl":U THEN */

  /* Ensure that a valid Option Code has been specified */
  IF btt_auth_type_control.option_code <> 0 THEN
  DO:
    FIND FIRST scheme NO-LOCK
      WHERE scheme.scheme-code = btt_auth_type_control.option_code
        AND scheme.active      = TRUE
     NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE scheme THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT "option_code":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Option Code ":U + STRING(btt_auth_type_control.option_code)).
    END. /* IF NOT AVAILABLE scheme THEN */
  END. /* IF btt_auth_type_control.option_code <> 0 */
  
  /* Validate comma delimited list of users */
  IF fnValidateUsers (INPUT btt_auth_type_control.usage_override_user,
                      INPUT "hactc":U, 
                      INPUT btt_auth_type_control.auth_type_control_obj, 
                      INPUT "usage_override_user":U, 
                      INPUT btt_auth_type_control.line_number, 
                      INPUT "Control Usage Override User":U) 
  THEN
    ASSIGN oplFailureOccurred = TRUE.

  /* Validate period type */
  IF btt_auth_type_control.period_type <> "":U THEN
  DO:
    FIND FIRST mic_acronym NO-LOCK
      WHERE mic_acronym.category_key = "ma_acAuthPeriod":U
        AND mic_acronym.acronym_key  = btt_auth_type_control.period_type
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "period_type":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Control Default Period Type: ":U + STRING(btt_auth_type_control.period_type)).
    END. /* IF NOT AVAILABLE mic_acronym THEN */
  END. /* IF btt_auth_type_control.period_type <> "":U THEN */

  /* Validate usage type */
  IF btt_auth_type_control.usage_type <> "" THEN
  DO:
    FIND FIRST mic_acronym NO-LOCK
      WHERE mic_acronym.category_key = "ma_acAuthUsageType":U
        AND mic_acronym.acronym_key  = btt_auth_type_control.usage_type
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc",
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT "usage_type":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Control Default Usage Type ":U + STRING(btt_auth_type_control.usage_type)).
    END. /* IF NOT AVAILABLE mic_acronym THEN */
  END. /* IF btt_auth_type_control.usage_type <> "" THEN */

  /*
    If usage_override is captured:  usage_quantity, usage_period and usage_period_type must be captured
  */
  IF btt_auth_type_control.usage_override_user <> "" THEN
  DO:
    ASSIGN cMessage = ""
           cField   = ""
           cField1  = "Usage Override":U.

    IF btt_auth_type_control.usage_quantity = 0 
    THEN
      ASSIGN cMessage = "Usage Quantity"
             cField   = "usage_quantity":U.

    IF btt_auth_type_control.usage_period = 0 
    THEN
      ASSIGN cMessage = IF cMessage = "" 
                        THEN "Usage Period"
                        ELSE cMessage + " and Usage Period"
             cfield   = IF cField = "" 
                        THEN "usage_period":U
                        ELSE cField.

    IF btt_auth_type_control.usage_period_type = "" 
    THEN
      ASSIGN cMessage = IF cMessage = "" 
                        THEN "Usage Period Type"
                        ELSE cMessage + " and Usage Period Type":U
             cfield   = IF cField = "" 
                        THEN "usage_period_type":U
                        ELSE cField.

    IF cMessage <> "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc",
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT cField,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "ma_MsgAuth":U,
                            INPUT 23,       /* &1 is mandatory when &2 is captured. */
                            INPUT cMessage + "," + cField1).
    END. /* IF cMessage <> "" THEN */
  END. /* IF btt_auth_type_control.usage_override_user <> "" THEN */
  ELSE   
  /*
    If usage_quantity is captured:usage_period and usage_period_type must be captured
  */
  IF btt_auth_type_control.usage_quantity <> 0 THEN
  DO:
    ASSIGN cMessage = ""
           cField   = ""
           cField1  = "Usage Quantity".

    IF btt_auth_type_control.usage_period = 0 
    THEN
      ASSIGN cMessage = "Usage Period "
             cfield   = "usage_period":U.

    IF btt_auth_type_control.usage_period_type = "" 
    THEN
      ASSIGN cMessage = IF cMessage = "" 
                        THEN "Usage Period Type ":U 
                        ELSE cMessage + "and Usage Period Type ":U
             cfield   = IF cField = "" 
                        THEN "usage_period_type":U
                        ELSE cField.

    IF cMessage <> "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactc",
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT cField,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "ma_MsgAuth":U,
                            INPUT 23,       /* &1 is mandatory when &2 is captured. */
                            INPUT cMessage + "," + cField1).
    END. /* IF cMessage <> "" THEN */
  END. /* IF btt_auth_type_control.usage_quantity  <> 0 THEN */
  ELSE
  /*
    If usage_period is captured: usage_period_type must be captured
  */
  IF (btt_auth_type_control.usage_period      <> 0 
  AND btt_auth_type_control.usage_period_type  = "")
  OR (btt_auth_type_control.usage_period_type <> ""
  AND btt_auth_type_control.usage_period       = 0) THEN
  DO:
    ASSIGN cMessage = ""
           cField   = ""
           cField1  = "".

    IF btt_auth_type_control.usage_period = 0
    THEN 
      ASSIGN cMessage = "Usage Period"
             cfield   = "usage_period":U
             cfield1  = "Usage Period Type".
    ELSE
    IF btt_auth_type_control.usage_period_type = "" 
    THEN
      ASSIGN cMessage = "Usage Period Type"
             cfield   = "usage_period_type":U
             cfield1  = "Usage Period".
   
    IF cMessage <> "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactc",
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT cField,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "ma_MsgAuth":U,
                            INPUT 23,       /* &1 is mandatory when &2 is captured. */
                            INPUT cMessage + "," + cField1).
    END. /* IF cMessage <> "" THEN */
  END. /* IF btt_auth_type_control.usage_period <> 0 THEN */
  
  /* Validate usage period type */
  IF btt_auth_type_control.usage_period_type <> "":U THEN
  DO:
    FIND FIRST mic_acronym NO-LOCK
      WHERE mic_acronym.category_key = "ma_acAuthPeriod":U
        AND mic_acronym.acronym_key  = btt_auth_type_control.usage_period_type
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "usage_period_type":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Type Control Usage Period Type: ":U + STRING(btt_auth_type_control.usage_period_type)).
    END. /* IF NOT AVAILABLE mic_acronym THEN  */
  END. /* IF btt_auth_type_control.usage_period_type <> "":U THEN */

  /* Validate the restrictons */
  IF btt_auth_type_control.auth_type_restrictions <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_control.auth_type_restrictions):

    ASSIGN cAuthType = ENTRY(iCnt, btt_auth_type_control.auth_type_restrictions).

    IF NOT CAN-FIND(FIRST buf_auth_type NO-LOCK
                    WHERE buf_auth_type.auth_type = cAuthType) THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "auth_type_restrictions":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Type Control Restrictions ":U + cAuthType).
    END. /* IF NOT CAN-FIND(FIRST buf_auth_type NO-LOCK */
  END. /* IF btt_auth_type_control.auth_type_restrictions <> "":U THEN */

  /* Validate claim_codes_disallow */
  IF btt_auth_type_control.claim_codes_disallow <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_control.claim_codes_disallow):

    ASSIGN iCCDisallow = INTEGER(ENTRY(iCnt,btt_auth_type_control.claim_codes_disallow)) NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:76' &ResetIgnoredErrors = TRUE }

    IF ERROR-STATUS:ERROR THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "claim_codes_disallow":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 201,  /* Invalid characters found in &1 */
                            INPUT "Disallow Claim Code: ":U + ENTRY(iCnt,btt_auth_type_control.claim_codes_disallow) +
                                  ". Only numeric characters are accepted.").
    END.  /* IF ERROR-STATUS:ERROR THEN */
    ELSE DO:
      mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_control.option_code,
                                                         INPUT  iCCDisallow,
                                                         INPUT  btt_auth_type_control.effective_date,
                                                         OUTPUT lSuccess,
                                                         OUTPUT cMessage).
      IF NOT lSuccess THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
  
        oErrorObject:addError(INPUT "hactc":U,
                              INPUT btt_auth_type_control.auth_type_control_obj,
                              INPUT "":U,
                              INPUT "claim_codes_disallow":U,
                              INPUT btt_auth_type_control.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Disallow Claim Code: ":U + ENTRY(iCnt,btt_auth_type_control.claim_codes_disallow)).
      END. /* IF NOT lSuccess */
    END.  /* ELSE - IF ERROR-STATUS:ERROR THEN */
  END. /* IF btt_auth_type_control.claim_codes_disallow <> "":U THEN */

  /* Validate claim_types_disallow */
  IF btt_auth_type_control.claim_types_disallow <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_control.claim_types_disallow):
    mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  ENTRY(iCnt,btt_auth_type_control.claim_types_disallow),
                                                       OUTPUT lSuccess,
                                                       OUTPUT cMessage).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "claim_types_disallow":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Disallow Claim Type: ":U + ENTRY(iCnt,btt_auth_type_control.claim_types_disallow)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_control.claim_types_disallow <> "":U THEN */

  /*
    Ensure that claim code and type lists have the same amount of entries
  */
  IF NUM-ENTRIES(btt_auth_type_control.claim_codes_header) <> NUM-ENTRIES(btt_auth_type_control.claim_types_header) THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE .
    
    IF NUM-ENTRIES(btt_auth_type_control.claim_codes_header) > NUM-ENTRIES(btt_auth_type_control.claim_types_header)
    THEN 
       ASSIGN cMessage = "Each claim code must have a corresponding claim type. Please ensure that the claim type list has the same amount of entries as the claim code list":U 
              cField   = "claim_types_header":U .
    ELSE 
       ASSIGN cMessage = "Each claim type must have a corresponding claim code. Please ensure that the claim code list has the same amount of entries as the claim type list":U. 
              cField   = "claim_code_header":U .
              
    oErrorObject:addError(INPUT "hactc":U,                     
                          INPUT btt_auth_type_control.auth_type_control_obj,    
                          INPUT "":U,                                           
                          INPUT cField ,                                 
                          INPUT btt_auth_type_control.line_number,                    
                          INPUT cMessage,                                                                           
                          INPUT "ERR":U).
  END. //IF NUM-ENTRIES(btt_auth_type_control.claim_codes_header) <> NUM-ENTRIES(btt_auth_type_control.claim_types_header) THEN
    
  /* 
    Validate claim_code list
  */
  IF   btt_auth_type_control.claim_codes_header <> "":U THEN
  DO:
    /*
      Validate each claim code in the list 
    */
    VALIDATE-CLAIM-CODE-BLK:
    DO iCount = 1 TO NUM-ENTRIES(btt_auth_type_control.claim_codes_header):

      ASSIGN iClaimCode = INTEGER(ENTRY(iCount, btt_auth_type_control.claim_codes_header)) 
             lSuccess   = FALSE NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:76' &ResetIgnoredErrors = TRUE }

      IF iClaimCode <> ? THEN
      DO:
        
        mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_control.option_code,
                                                           INPUT  iClaimCode,
                                                           INPUT  btt_auth_type_control.effective_date,
                                                           OUTPUT lSuccess,
                                                           OUTPUT cMessage).
                                                           
       IF LOOKUP(STRING(iClaimCode) , btt_auth_type_control.claim_codes_disallow ) > 0 
       THEN 
         ASSIGN lSuccess = FALSE .
         
      END. //IF iClaimCode <> ? THEN

      IF NOT lSuccess 
      THEN 
        ASSIGN cInvalidClaimCodes = cInvalidClaimCodes 
                                  + (IF cInvalidClaimCodes <> "":U THEN ",":U ELSE "":U ) 
                                  + (IF ENTRY(iCount,btt_auth_type_control.claim_codes_header) = "":U THEN " ":U ELSE ENTRY(iCount,btt_auth_type_control.claim_codes_header)) 
                                  + (IF LOOKUP(STRING(iClaimCode), btt_auth_type_control.claim_codes_disallow ) > 0 THEN "(Overlapped with disallow) "  ELSE "":U) .
    END. // VALIDATE-CLAIM-CODE-BLK

    IF cInvalidClaimCodes <> "":U 
    THEN
      ASSIGN oplFailureOccurred = TRUE 
             cMessage           = "The following claim codes that were specified in the list are invalid: ":U + cInvalidClaimCodes 
             lSuccess           = oErrorObject:addError(INPUT "hactc":U,                     
                                                        INPUT btt_auth_type_control.auth_type_control_obj,    
                                                        INPUT "":U,                                           
                                                        INPUT "claim_codes_header":U ,                                 
                                                        INPUT btt_auth_type_control.line_number,                    
                                                        INPUT cMessage,                                                                           
                                                        INPUT "ERR":U).
  END. /* IF   btt_auth_type_control.claim_codes_header <> "":U THEN */
  
  /* 
    Validate claim type list
  */
  IF btt_auth_type_control.claim_types_header <> "":U THEN
  DO:
    /*
      Validate each claim type in the list 
    */
    VALIDATE-CLAIM-TYPE-BLK:
    DO iCount = 1 TO NUM-ENTRIES(btt_auth_type_control.claim_types_header) :

      ASSIGN cClaimType = ENTRY(iCount, btt_auth_type_control.claim_types_header)
             lSuccess   = FALSE .

      IF cClaimType <> "":U THEN
      DO:
        mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  cClaimType,
                                                           OUTPUT lSuccess,
                                                           OUTPUT cMessage).
                                                           
       IF LOOKUP(cClaimType, btt_auth_type_control.claim_types_disallow ) > 0 
       THEN 
         ASSIGN lSuccess = FALSE .
                                                           
      END. //IF cClaimType <> ? THEN
      
      IF NOT lSuccess
      THEN
        ASSIGN cInvalidClaimTypes = cInvalidClaimTypes 
                                  + (IF cInvalidClaimTypes <> "":U THEN ",":U ELSE "":U ) 
                                  + (IF ENTRY(iCount,btt_auth_type_control.claim_types_header) = "":U THEN " ":U ELSE ENTRY(iCount,btt_auth_type_control.claim_types_header))
                                  + (IF LOOKUP(cClaimType, btt_auth_type_control.claim_types_disallow ) > 0 THEN "(Overlapped with disallow) "  ELSE "":U ) .
    END. // VALIDATE-CLAIM-TYPE-BLK

    IF cInvalidClaimTypes <> "":U 
    THEN
      ASSIGN oplFailureOccurred = TRUE 
             cMessage           = "The following claim codes that were specified in the list are invalid: ":U + cInvalidClaimTypes
             lSuccess           = oErrorObject:addError(INPUT "hactc":U,                     
                                                        INPUT btt_auth_type_control.auth_type_control_obj,    
                                                        INPUT "":U,                                           
                                                        INPUT "claim_types_header":U ,                                 
                                                        INPUT btt_auth_type_control.line_number,                    
                                                        INPUT cMessage,                                                                           
                                                        INPUT "ERR":U).
  END. /* IF   btt_auth_type_control.claim_codes_header <> "":U THEN */

  /* Validate the gender and age ranges */
  IF btt_auth_type_control.gender               <> ""
  OR btt_auth_type_control.age_range_male_obj   <> 0
  OR btt_auth_type_control.age_range_female_obj <> 0
  OR btt_auth_type_control.age_range_both_obj   <> 0 THEN
  DO:
    mipEnv:Health:mautility:validateGenderAgeRangeObj(INPUT  btt_auth_type_control.gender,
                                                      INPUT  btt_auth_type_control.age_range_male_obj,
                                                      INPUT  btt_auth_type_control.age_range_female_obj,
                                                      INPUT  btt_auth_type_control.age_range_both_obj,
                                                      OUTPUT cFieldname,
                                                      OUTPUT cMessage).
    IF cMessage <> "" THEN
    DO iCnt = 1 TO NUM-ENTRIES(cMessage,"^":U):
      ASSIGN oplFailureOccurred = TRUE.
          
      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT ENTRY(iCnt,cFieldname,"^":U),
                            INPUT btt_auth_type_control.line_number,
                            INPUT ENTRY(iCnt,cMessage,"^":U),
                            INPUT "ERR":U).
    END.  /* IF cMessage <> "" THEN */
  END.  // IF btt_auth_type_control.gender <> ""

{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oControlTypeInd) THEN DELETE OBJECT oControlTypeInd.
      		      IF VALID-OBJECT(oErrorObject)    THEN DELETE OBJECT oErrorObject.
                IF VALID-OBJECT(oUsageType)      THEN DELETE OBJECT oUsageType."}
&ENDIF      
