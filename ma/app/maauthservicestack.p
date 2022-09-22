&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*----------------------------------------------------------------------------------
    Purpose: Healthcare Authorisation Service Stack

    Author : Andrewd

    Note   : Copied from original program maauthservicestack.p in Sep 2017 (MMP-104)
  ----------------------------------------------------------------------------------*/

/* ----------------------------------  Definitions  ------------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i }

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthtypeconfigtt.i                           }
{ ma/inc/maextreftt.i &TEMP-TABLE-NAME = tt_auth_extref }

/* temporary tt for use in applyRateConversions procedure */
{ ma/inc/maauthdetailtt.i &TEMP-TABLE-NAME = "tt_temp_auth_detail" }
{ ma/inc/maauthtt.i &TEMP-TABLE-NAME = tt_temp_auth }
{ ma/inc/maauthflagvaluett.i &TEMP-TABLE-NAME = tt_temp_auth_flag_value}


{ ma/inc/maauthds.i                 }
{ ma/inc/mamemberds.i               }
{ ma/inc/maauthtypeds.i             }
{ ma/inc/matariffds.i               }
{ ma/inc/madecisionauthhighriskds.i }
{ ma/inc/maexternalreferenceds.i    }
{ ma/inc/maauthflagvalueds.i        }
{ ma/inc/maauthupdatescheckds.i     }
{ ma/inc/maauthratecontrolds.i      }
{ ma/inc/maauthruleds.i             }
{ ma/inc/madephealthds.i            }
{ ma/inc/maauthcopytt.i             }
{ ma/inc/masuccesstt.i &TEMP-TABLE-NAME = tt_auth_copy_success }
{ ma/inc/madeplimittt.i             }
{ ma/inc/madeplimitwarntt.i         }

DEFINE TEMP-TABLE tt_auth_copy_error LIKE tt_auth_error
  FIELD orig_auth_num AS CHARACTER
  FIELD new_auth_num  AS CHARACTER .

DEFINE TEMP-TABLE ttConfigCache LIKE ttAuthTypeConfig.

DEFINE TEMP-TABLE tt-additionalicd
   FIELD diagnosis  LIKE dclaim.diagnosis
   FIELD ass-code   LIKE dclaim.ass-code
   FIELD morph-code LIKE dclaim.morph-code.

DEFINE TEMP-TABLE ttRule NO-UNDO
   FIELD RuleCode      AS CHARACTER
   FIELD RuleProcedure AS CHARACTER
   FIELD RuleValue     AS CHARACTER
   FIELD Sequence      AS INTEGER
   INDEX idx1 RuleCode.

DEFINE TEMP-TABLE ttAuthRequestValues NO-UNDO
   FIELD auth_obj                   AS DECIMAL
   FIELD total_amount_requested     AS DECIMAL
   FIELD total_amount_authorised    AS DECIMAL
   FIELD initial_fee_requested      AS DECIMAL
   FIELD initial_fee_authorised     AS DECIMAL
   FIELD instalment_requested       AS DECIMAL
   FIELD instalment_num_requested   AS DECIMAL
   FIELD instalment_authorised      AS DECIMAL
   FIELD instalment_num_authorised  AS DECIMAL.

/* Temp tables used in getRateControl */
DEFINE TEMP-TABLE tt_trigger NO-UNDO
       FIELD entity      AS CHARACTER
       FIELD entity_code AS CHARACTER .

DEFINE TEMP-TABLE tt_exclusion            LIKE tt_trigger.
DEFINE TEMP-TABLE tt_day_clinic_exclusion LIKE tt_trigger.

&SCOPED-DEFINE ActionList delete,modify,partialsave

&SCOPED-DEFINE ModifyList modify,partialsave

{ mip/inc/mipgetttords.i &Dataset-01=dsRequest
                         &Dataset-02=dsResponse
                         &Dataset-03=dsAuthorisation}


&SCOPED-DEFINE EndDate           opdEndDate
&SCOPED-DEFINE EndTime           oplEndTime
&SCOPED-DEFINE CalculatedEndDate ipdCalculatedEndDate
&SCOPED-DEFINE CalculatedEndTime iplCalculatedEndTime
&SCOPED-DEFINE TotalLOS          opdTotalLOS
&SCOPED-DEFINE NextStartDate     dNextStartDate
&SCOPED-DEFINE NextStartTime     lNextStartTime
&SCOPED-DEFINE DetailBuffer      tt_auth_detail

&SCOPED-DEFINE CalcProcedureSignature DEFINE        PARAMETER BUFFER {&DetailBuffer} FOR {&DetailBuffer}.   ~
                                      DEFINE INPUT  PARAMETER ipdInsurerObj          AS DECIMAL   NO-UNDO.  ~
                                      DEFINE INPUT  PARAMETER ipiOptionCode          AS INTEGER   NO-UNDO.  ~
                                      DEFINE INPUT  PARAMETER ipdStartDate           AS DATE      NO-UNDO.  ~
                                      DEFINE INPUT  PARAMETER iplStartTime           AS LOGICAL   NO-UNDO.  ~
                                      DEFINE INPUT  PARAMETER ~{&CalculatedEndDate~} AS DATE      NO-UNDO.  ~
                                      DEFINE INPUT  PARAMETER ~{&CalculatedEndTime~} AS LOGICAL   NO-UNDO.  ~
                                      DEFINE OUTPUT PARAMETER ~{&EndDate~}           AS DATE      NO-UNDO.  ~
                                      DEFINE OUTPUT PARAMETER ~{&EndTime~}           AS LOGICAL   NO-UNDO.  ~
                                      DEFINE OUTPUT PARAMETER ~{&TotalLos~}          AS DECIMAL   NO-UNDO.


&SCOPED-DEFINE CalcStandardDefinitions DEFINE VARIABLE ~{&NextStartDate~} AS DATE        NO-UNDO. ~
                                       DEFINE VARIABLE ~{&NextStartTime~} AS LOGICAL     NO-UNDO. ~
                                       DEFINE VARIABLE cTrackingMessage   AS CHARACTER   NO-UNDO. ~
                                       DEFINE VARIABLE iCalcTime          AS INTEGER     NO-UNDO. ~
                                       DEFINE BUFFER b{&DetailBuffer} FOR {&DetailBuffer}.

&SCOPED-DEFINE CalcNextStartDateTime ASSIGN iCalcTime = ETIME.                               ~
                                     RUN _nextStartDateTime ( INPUT  ipdStartDate,           ~
                                                              INPUT  iplStartTime,           ~
                                                              INPUT  ~{&CalculatedEndDate~}, ~
                                                              INPUT  ~{&CalculatedEndTime~}, ~
                                                              OUTPUT ~{&NextStartDate~},     ~
                                                              OUTPUT ~{&NextStartTime~}).    ~
                                     ASSIGN cTrackingMessage = "_nextStartDateTime (Duration=" + STRING(ETIME - iCalcTime)    + ")"  + ~
                                                               " Input Start="    + (IF ipdStartDate = ?           THEN "" ELSE STRING(ipdStartDate,'99/99/99'))           + (IF iplStartTime = ? THEN "" ELSE STRING(iplStartTime,'AM/PM')) + ~
                                                               " Calculated End=" + (IF ~{&CalculatedEndDate~} = ? THEN "" ELSE STRING(~{&CalculatedEndDate~},'99/99/99')) + (IF ~{&CalculatedEndTime~} = ? THEN "" ELSE STRING(~{&CalculatedEndTime~},'AM/PM')) + ~
                                                               " Next Start="     + (IF ~{&NextStartDate~} = ?     THEN "" ELSE STRING(~{&NextStartDate~},'99/99/99'))     + (IF ~{&NextStartTime~} = ? THEN "" ELSE STRING(~{&NextStartTime~},'AM/PM')) + ~
                                                               " AVAILABLE ~{&DetailBuffer~}=" + STRING(AVAILABLE ~{&DetailBuffer~}).  ~
                                     ~{ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage~}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnGetErrorMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnGetErrorMessage Procedure
FUNCTION fnGetErrorMessage RETURNS CHARACTER
  ( INPUT ipcMessageGroup  AS CHARACTER,
    INPUT ipiMessageNumber AS INTEGER,
    INPUT ipcReplaceText   AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnResetAuthDetailValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnResetAuthDetailValues Procedure
FUNCTION fnResetAuthDetailValues RETURNS LOGICAL
  ( BUFFER btt_auth_detail   FOR tt_auth_detail,
    BUFFER bttAuthTypeConfig FOR ttAuthTypeConfig)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnResetAuthProviderValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnResetAuthProviderValues Procedure
FUNCTION fnResetAuthProviderValues RETURNS LOGICAL
  ( BUFFER btt_auth_provider FOR tt_auth_provider,
    BUFFER bttAuthTypeConfig FOR ttAuthTypeConfig)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnResetAuthValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnResetAuthValues Procedure
FUNCTION fnResetAuthValues RETURNS LOGICAL
  ( BUFFER btt_auth          FOR tt_auth,
    BUFFER bttAuthTypeConfig FOR ttAuthTypeConfig)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnUserHasRole) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnUserHasRole Procedure
FUNCTION fnUserHasRole RETURNS LOGICAL
  ( ipcRoleCodeList AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow:
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB)
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 33.57
         WIDTH              = 273.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-applyAuthLimitControls) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyAuthLimitControls Procedure
PROCEDURE applyAuthLimitControls :
/*------------------------------------------------------------------------------
  Purpose   : This procedure will ensure that the provider and detail quantity/amounts fall within the global
              limit of the auth header amount/quantity.

              It will also ensure that the detail amount/quantity falls within the limit of the related provider amount/quantity

              It can be called from Header, provider and detail level.

              AuthGlobalLimit and AuthProviderLimit will determine if the user will be notified via error, warning or warn acknowledgment
              when an amount/quantity exceeds a limit

  Parameters: DATASET dsAuthorisation {ma/inc/maauthds.i}
              ipdAuthObj
              ipcAuthLevel    - Auth level to be validated. Valid values:
                                "hatau" - Header
                                "hatap" - Provider
                                "hatad" - Detail
              ipdAuthLevelObj - Obj related to the level specified above
                                 when "hatap" - auth provider obj
                                 when "hatad" - auth detail obj

              ipdAmount       - Amount of the auth level to be validated
              ipdQuantity     - Quantity of the auth level to be validated

  Notes     : Errors will be returned in the tt_auth_error temp-table , which
              is included in the dsAuthorisation
------------------------------------------------------------------------------*/

    {ma/app/maauthservauthlimcntrl.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyAuthLOSDetailsUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyAuthLOSDetailsUser Procedure
PROCEDURE applyAuthLOSDetailsUser :
/*------------------------------------------------------------------------------
  Purpose   : Apply LOS start/end date/time changes as captured by user
  Notes     :
------------------------------------------------------------------------------*/
  {ma/app/maauthservlosdetailsuser.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyAuthRateConversion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyAuthRateConversion Procedure
PROCEDURE applyAuthRateConversion :
/*------------------------------------------------------------------------------
  Purpose: Apply rate conversion to Authorisation detail lines
  Parameters:  Authorisation dataset
               Auth obj
               Code Link Category
               Override base rate
               Override ars rate
  Notes:  This method converts auth detail lines in the dataset and returns the dataset
          with converted values.
          New detail line records will be created for converted tariffs and record
          action will be set to "MODIFY". Detail lines that need to be deleted will
          have record action set to "DELETE".
------------------------------------------------------------------------------*/

  { ma/app/maauthservapplrateconv.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcAmountAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcAmountAuth Procedure
PROCEDURE calcAmountAuth :
/*------------------------------------------------------------------------------
  Purpose   : Auth Tariff Cost Amount Calculation
  Parameters:
  Notes     : This routine will accept as input parameters, the Authorisation
              dataset, the Tariff Cost calculation acronym value and the
              Tariff Cost amount.
              The Cost Calculation with be done an returned.
              An error message will also be returned for any errors that might
              have occurred.
------------------------------------------------------------------------------*/
  {ma/app/maauthservcalcamountauth.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcAuthLOSDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcAuthLOSDetails Procedure
PROCEDURE calcAuthLOSDetails :
/*------------------------------------------------------------------------------
  Purpose   : Calculate Authorisation LOS details
  Notes     : This routine will accept as input parameters, the main provider
              ars rate,the auth start date as well as an authorisation dataset.
              The LOS information will be updated on the detail temp table
              in the authorisation dataset accordingly, the final discharge
              date and the total LOS will be returned.

              * MMP-399 ( RTB49002 )
                Developed

              * MMP-423 ( RTB49582 )
                Bug fix for multiple calculations being applicable
                across all detail lines. Changes made to cater for
                additional input dates which will be the end date
                of the previous line in the sequence if applicable.
                eg. Dependant moves between wards from general Ward
                    to maternity ward.

  Author    : Andrewd
------------------------------------------------------------------------------*/

  { ma/app/maauthservcalcauthlosdetails.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcAuthQtyFromMinutes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcAuthQtyFromMinutes Procedure
PROCEDURE calcAuthQtyFromMinutes :
/*------------------------------------------------------------------------------
  Purpose:    Calculation of Quantity Authorised from Minutes Authorised
  Parameters: This routine will accept as input parameters:
                - Client
                - Member option code
                - Authorisation detail line start date
                - Tariff Type Obj for minutes
                - Minutes Authorised on detail line

              And will produce as output parameters:
                - Quantity Authorised calculated
                - Rule used in calculation of Quantity Authorised
                - If any error is picked up in calculation
  Notes:      This method will be used in the Authorisation processing
              if minutes are captured on a Clinical Detail Line
------------------------------------------------------------------------------*/

  { ma/app/maauthservcalcqtyfrommin.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForCopay Procedure
PROCEDURE checkForCopay :
/*------------------------------------------------------------------------------
  Purpose   : To check if Co-payment should apply on an Auth Provider
  Parameters: Input parameters: ipdInsurerObj            - Client
                                 ipiOptionCode           - Member option code
                                 ipdDate                 - Date for which setups must be checked.
                                 ipcEmergency            - Indicate if it is an emergency.
                                 iplPMB                  - Indicate if it is a PMB.
                                 ipiDocNum               - Doctor number/Provider number.
                                 ipiPrType               - Doctor/Provider discipline.
                                 ipiDocNegNum            - Provider DNU group.
                                 ipdAuthCopayControlObj  - Co-payment control obj to which the provider currently links.
                                 ipcCopayOverrideNote    - Co-payment override note captured when the authorisation is authorised.
                                 iplCopayProvider        - Indicate if provider is a co-payment provider.
                                 ipiAuthStatus           - Authorisation status before co-payment processing.
                                 ipcAuthStatusNote       - Authorisation status note before co-payment processing.
                                 ipcProviderType         - Provider type, e.g. Admission or Main Treating
              Output parameters: opdAuthCopayControlObj  - hac_auth_copay_control.auth_copay_control_obj.  Will not contain a value if no co-payment should apply.
                                 oplCopayProvider        - Output parameter that will indicate if the provider is a co-payment provider.
                                 opiAuthStatus           - hat_auth_copay_control.auth_status.  Will not contain a value if no co-payment should apply.
                                 opcAuthStatusNote       - hat_auth_copay_control.auth_status_note.  Will not contain a value if no co-payment should apply.
                                 opcWarning              - hat_auth_copay_control.warning_message.  Will not contain a value if no co-payment should apply.
                                 opcWarningType          - hat_auth_copay_control.warning_message_type.  Will not contain a value if no co-payment should apply.
                                 opdAuthCopayTypeObj     - hat_auth_copay_control.auth_copay_type_obj.  Will not contain a value if no co-payment should apply.
                                 oplCopayValueType       - hat_auth_copay_control.copayment_value_type.  Will not contain a value if no co-payment should apply.
                                 opdCopayValue           - hat_auth_copay_control.copayment_value.  Will not contain a value if no co-payment should apply.
                                 opcError                - Return errors that should be handled in the calling procedure.
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj          AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode          AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate                AS DATE      NO-UNDO.
  DEFINE INPUT  PARAMETER ipcEmergency           AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iplPMB                 AS LOGICAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiDocNum              AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiPrType              AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiDocNegNum           AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthCopayControlObj AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcCopayOverrideNote   AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iplCopayProvider       AS LOGICAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiAuthStatus          AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcAuthStatusNote      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcProviderType        AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opdAuthCopayControlObj AS DECIMAL   NO-UNDO INITIAL 0.
  DEFINE OUTPUT PARAMETER oplCopayProvider       AS LOGICAL   NO-UNDO INITIAL ?.  // Important that we know if no check was done to determine if a provider is a co-payment provider.
  DEFINE OUTPUT PARAMETER opiAuthStatus          AS INTEGER   NO-UNDO INITIAL ?.  // The unknown value is important for the processing after method is called if the authorisation status is not
                                                                                  // changed.  We can't use zero because zero is a valid authorisation status.
  DEFINE OUTPUT PARAMETER opcAuthStatusNote      AS CHARACTER NO-UNDO INITIAL "".
  DEFINE OUTPUT PARAMETER opcWarning             AS CHARACTER NO-UNDO INITIAL "".
  DEFINE OUTPUT PARAMETER opcWarningType         AS CHARACTER NO-UNDO INITIAL "".
  DEFINE OUTPUT PARAMETER opdAuthCopayTypeObj    AS DECIMAL   NO-UNDO INITIAL 0.
  DEFINE OUTPUT PARAMETER oplCopayValueType      AS LOGICAL   NO-UNDO INITIAL ?.
  DEFINE OUTPUT PARAMETER opdCopayValue          AS DECIMAL   NO-UNDO INITIAL 0.
  DEFINE OUTPUT PARAMETER opcError               AS CHARACTER NO-UNDO INITIAL "".

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cInsurerObjList   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOptionCodeList   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cProviderType     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cProviderTypeList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTrackingMessage  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWorkGroupObjList AS CHARACTER   NO-UNDO. // Will be used to save the list of obj's for the workgroups to which a provider belongs
  DEFINE VARIABLE dInsurer          AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dWorkGroupObj     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iCnt              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iInsurerCnt       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iOption           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iOptionCnt        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iProviderTypeCnt  AS INTEGER     NO-UNDO.

  &SCOPED-DEFINE exclapply  ASSIGN oplCopayProvider = NO. ~
                            LEAVE {&BLKNAME}.

  DEFINE BUFFER hac_auth_copay_control FOR hac_auth_copay_control.

  /*
    Find the hac_auth_copay_control record if provider links to a co-payment set up already
  */
  IF ipdAuthCopayControlObj <> 0 THEN
  DO:
    FIND hac_auth_copay_control NO-LOCK
      WHERE hac_auth_copay_control.auth_copay_control_obj = ipdAuthCopayControlObj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE hac_auth_copay_control THEN
    DO:
      ASSIGN opcError = SUBSTITUTE("Invalid Auth Copay Control Obj (&1) linked to auth provider.",STRING(ipdAuthCopayControlObj)).
      RETURN.
    END.  // IF NOT AVAILABLE hac_auth_copay_control THEN
    ELSE DO:
      /*
        Authorisation is not authorised after a co-payment was applied.  We should remove the co-payment.
        We save the values here in case we need to authorise the co-payment again next time, so then
        we don't need to repeat all the processing again.
      */
      IF  ipiAuthStatus    <> 1
      AND iplCopayProvider <> ? THEN
      DO:

        IF (hac_auth_copay_control.apply_to_emergency = NO AND ipcEmergency = "YES")
        OR (hac_auth_copay_control.apply_to_pmb       = NO AND iplPMB       = TRUE )
        THEN
          ASSIGN opdAuthCopayControlObj = hac_auth_copay_control.auth_copay_control_obj
                 opdAuthCopayTypeObj    = hac_auth_copay_control.auth_copay_type_obj. // Assign value here to cater for if we should delete a co-payment.
        ELSE
          ASSIGN opdAuthCopayControlObj = ipdAuthCopayControlObj
                 oplCopayProvider       = IF  hac_auth_copay_control.auth_status      = ipiAuthStatus
                                          AND hac_auth_copay_control.auth_status_note = ipcAuthStatusNote
                                          THEN iplCopayProvider
                                          ELSE ?
                 opdAuthCopayTypeObj    = hac_auth_copay_control.auth_copay_type_obj.

        RETURN.
      END.  // IF ipiAuthStatus <> 1
    END.  // ELSE - IF NOT AVAILABLE hac_auth_copay_control THEN
  END.  // IF ipdAuthCopayControlObj <> 0 THEN

  /*
     If provider doesn't link to a hat_auth_copay_control record already we need to determine if a co-payment
     setup does exist for the insurer, option and provider type.
  */
  ELSE DO:

    /*
      We will do the following finds in this CopayControlBlock:
      1. First find will check for an exact match on insurer_obj, option_code and provider_type.
      2. If nothing was found, then 2nd find will check for exact match on insurer_obj and option_code, but provider_type = "".
      3. If nothing was found, then 3rd find will check for exact match on insurer_obj and provider_type, but option_code = 0.
      4. If nothing was found, then 4th find will check for exact match on insurer_obj, but option_code = 0 and provider_type = "".
      5. If nothing was found, then 5th find will check for insurer_obj = 0, exact match on option_code and provider_type.
      6. If nothing was found, then 6th find will check for insurer_obj = 0, exact match on option code, but provider_type = "".
      7. If nothing was found, then 7th find will check for insurer_obj = 0, option_code = 0, but provider_type = exact match.
      8. If nothing was found, then 8th find will check for insurer_obj = 0, option_code = 0 and provider_type = "".
    */
    ASSIGN cInsurerObjList   = STRING(ipdInsurerObj) + ",0"
           cOptionCodeList   = STRING(ipiOptionCode) + ",0"
           cProviderTypeList = ipcProviderType       + ",".

    CopayControlBlock:
    DO iInsurerCnt = 1 TO 2:
      ASSIGN dInsurer = DECIMAL(ENTRY(iInsurerCnt,cInsurerObjList)).
      DO iOptionCnt = 1 TO 2:
        ASSIGN iOption = INTEGER(ENTRY(iOptionCnt,cOptionCodeList)).
        DO iProviderTypeCnt = 1 TO 2:
          ASSIGN cProviderType = ENTRY(iProviderTypeCnt,cProviderTypeList).
          FIND FIRST hac_auth_copay_control NO-LOCK
            WHERE hac_auth_copay_control.insurer_obj     = dInsurer
            AND   hac_auth_copay_control.option_code     = iOption
            AND   hac_auth_copay_control.provider_type   = cProviderType
            AND   hac_auth_copay_control.effective_date <= ipdDate
            AND  (hac_auth_copay_control.end_date       >= ipdDate
            OR    hac_auth_copay_control.end_date        = ?)
            NO-ERROR.
          IF AVAILABLE hac_auth_copay_control
          THEN
            LEAVE CopayControlBlock.
        END.  // DO iProviderType = 1 TO 2:
      END.  // DO iOption = 1 TO 2:
    END.  // DO iInsurer = 1 TO 2:

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE hac_auth_copay_control
    THEN
      RETURN.

  END.  // ELSE - IF ipdAuthCopayControlObj <> 0 THEN

  /*
    Check if auth is an Emergency or PMB and leave the method if the co-payment should not apply.
    The obj must still be assigned to link the provider to the co-payments setups
    for if the emergency flag or PMB status is updated later and a co-payment must apply,
    to save processing time on finding the hac_auth_copay_control record again.
  */
  IF (hac_auth_copay_control.apply_to_emergency = NO AND ipcEmergency = "YES":U)
  OR (hac_auth_copay_control.apply_to_pmb       = NO AND iplPMB       = TRUE ) THEN
  DO:
    ASSIGN opdAuthCopayControlObj = hac_auth_copay_control.auth_copay_control_obj
           opdAuthCopayTypeObj    = hac_auth_copay_control.auth_copay_type_obj. // Assign value here to cater for if we should delete a co-payment.

    RETURN.
  END.  // IF ipcEmergency <> "" OR iplPMB = TRUE THEN

  /*
    If provider is a co-payment provider as per input parameter passed in.
    We assign the value here so that we don't check if the provider
    requires a co-payment if we know it already.
  */
  IF iplCopayProvider <> ?
  THEN
    ASSIGN oplCopayProvider = iplCopayProvider.

  /*
    Check if the co-payment will apply for the provider.
    First check if any setups exist for the provider number.
    Note that no exclusions can be set up for the provider number.
  */
  IF oplCopayProvider = ? THEN
  DO:
    IF CAN-FIND(FIRST hac_auth_copay_detail NO-LOCK
      WHERE hac_auth_copay_detail.auth_copay_control_obj = hac_auth_copay_control.auth_copay_control_obj
      AND   hac_auth_copay_detail.owning_entity_mnemonic = "doctor":U
      AND   hac_auth_copay_detail.owning_obj             = 0
      AND   hac_auth_copay_detail.owning_key             = STRING(ipiDocNum)
      AND   hac_auth_copay_detail.effective_date        <= ipdDate
      AND  (hac_auth_copay_detail.end_date              >= ipdDate
      OR    hac_auth_copay_detail.end_date               =  ?))
    THEN
      ASSIGN oplCopayProvider = YES.
  END.  // IF oplCopayProvider = ? THEN

  /*
    If no co-payment must apply for the provider get all the workgroups to which
    the provider belong.
  */
  IF oplCopayProvider = ? THEN
  DO:
    mipEnv:Health:maDoctor:getProviderWorkGroup(INPUT ipiOptionCode,
                                                INPUT ipiDocNum,
                                                INPUT ipdDate,
                                                INPUT "",
                                                OUTPUT cWorkgroupObjList).
  END.  // IF oplCopayProvider = ? THEN

  /*
    If no co-payment must apply for the provider check if any setup exists for the provider discipline.
    Please note multiple hac_auth_copay_detail records may exist for the discipline with different exclusions.
  */
  IF oplCopayProvider = ? THEN
  DO:

    { ma/app/maauthservcopayexcl.i
                &oem              = "'prtype'":U
                &oobj             = 0
                &okey             = STRING(ipiPrType)
                &blkname          = PRTYPEBLK
                &DocNum           = ipiDocNum
                &DocNegNum        = ipiDocNegNum
                &WorkgroupObjList = cWorkgroupObjList }

  END.  // IF oplCopayProvider = ? THEN

  /*
    If no co-payment must apply for the provider or the discipline setups we need to
    check for the provider negotiation group (DNU).
  */
  IF  oplCopayProvider = ?
  AND ipiDocNegNum <> 0 THEN
  DO:

    { ma/app/maauthservcopayexcl.i
                &oem              = "'neggroup'":U
                &oobj             = 0
                &okey             = STRING(ipiDocNegNum)
                &blkname          = NEGGROUPBLK
                &DocNum           = ipiDocNum
                &PrType           = ipiPrType
                &WorkgroupObjList = cWorkgroupObjList }

  END.  // IF  oplCopayProvider = ? AND ipiDocNegNum <> 0 THEN

  /*
    If no co-payment should apply to the provider number, discipline and negotiation group
    we need to check if any setups apply for the provider workgroup.
    Loop through the list of workgroups to which the provider belong.
  */
  IF  oplCopayProvider = ?
  AND cWorkGroupObjList <> "" THEN
  DO:

    WORKGROUPBLK1:
    DO iCnt = 1 TO NUM-ENTRIES(cWorkGroupObjList):
      ASSIGN dWorkGroupObj = DECIMAL(ENTRY(iCnt,cWorkGroupObjList)).

      IF dWorkGroupObj = 0
      THEN
        LEAVE WORKGROUPBLK1.

      { ma/app/maauthservcopayexcl.i
                &oem              = "'hdmwg'":U
                &oobj             = dWorkGroupObj
                &okey             = "''"
                &blkname          = "WORKGROUPBLK2"
                &DocNum           = ipiDocNum
                &PrType           = ipiPrType
                &DocNegNum        = ipiDocNegNum
                &WorkgroupObjList = cWorkgroupObjList }

    END.  // WORKGROUPBLK1: DO iCnt = 1 TO NUM-ENTRIES(cWorkGroupObjList):
  END.  // IF oplCopayProvider = ? AND cWorkGroupObjList <> "" THEN

  /*
    If no hac_auth_copay_detail records exists, no co-payment should apply.
    We need to cater for two scenarios - oplCopayProvider = ? or oplCopayProvider = No.
  */
  IF oplCopayProvider <> YES THEN
  DO:
    ASSIGN opdAuthCopayControlObj = hac_auth_copay_control.auth_copay_control_obj
           opdAuthCopayTypeObj    = hac_auth_copay_control.auth_copay_type_obj // Assign value here to cater for if we should delete a co-payment.
           oplCopayProvider       = NO.
    RETURN.
  END.  // IF NOT oplCopayProvider THEN

  /*
    Co-payment should apply and output parameters should be assigned if the input AuthCopayControlObj = 0 or input iplCopayProvider = ?
    It will indicate that we apply the co-payment check the first time and the provider authorisation status should be adjusted.
  */
  IF (ipdAuthCopayControlObj = 0    // If no copay control obj was passed into method when method was called.
  OR  iplCopayProvider       = ? )  // If copay indicator was unknown when method was called.
  AND oplCopayProvider = YES THEN
  DO:
    ASSIGN opdAuthCopayControlObj = hac_auth_copay_control.auth_copay_control_obj
           opiAuthStatus          = hac_auth_copay_control.auth_status
           opcAuthStatusNote      = hac_auth_copay_control.auth_status_note
           opcWarning             = hac_auth_copay_control.warning_message
           opcWarningType         = hac_auth_copay_control.warning_message_type.
    RETURN.
  END.  // IF (ipdAuthCopayControlObj = 0 OR iplCopayProvider = ? ) AND oplCopayProvider = YES THEN

  /*
    If the Co-payment should still apply - assign co-payment values if the copay override note indicates that co-payment must still apply.
  */
  IF  ipdAuthCopayControlObj <> 0
  AND oplCopayProvider = YES THEN
  DO:
    IF LOOKUP(ipcCopayOverrideNote,hac_auth_copay_control.copay_apply_override_reasons) > 0
    THEN
      ASSIGN opdAuthCopayControlObj = hac_auth_copay_control.auth_copay_control_obj
             opdAuthCopayTypeObj    = hac_auth_copay_control.auth_copay_type_obj
             oplCopayValueType      = hac_auth_copay_control.copayment_value_type
             opdCopayValue          = hac_auth_copay_control.copayment_value.
    ELSE
      ASSIGN opdAuthCopayControlObj = hac_auth_copay_control.auth_copay_control_obj
             opdAuthCopayTypeObj    = hac_auth_copay_control.auth_copay_type_obj.
  END.  // IF ipdAuthCopayControlObj <> 0 AND oplCopayProvider = YES THEN

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForCopayDetailItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForCopayDetailItem Procedure
PROCEDURE checkForCopayDetailItem :
/*------------------------------------------------------------------------------
  Purpose:     Determine if a co-payment must apply for:
               o An Authorisation Clinical Detail Line when a detail line is
                 loaded on an Authorisation.
               o A Claim that links to an Authorisation when no Clinical
                 Detail Line exists on the Authorisation.

  Parameters:  This routine will accept as Input parameters:
               - ipdInsurerObj     /* Insurer obj.*/
               - ipiOptionCode     /* Option code. */
               - ipdDate           /* Detail line start date or claim date. */
               - ipcEntity         /* Detail line entity mnemonic. Always required. */
               - ipdEntityObj      /* Detail line entity obj. */
               - ipcEntityKey      /* Detail line key. */
               - ipcEntityAltValue /* Detail line entity alt value - Not required if called from Auths. Required if called from claims. */
               - ipiPrType         /* Discipline. */
               - ipcEmergency      /* Indicate if it is an Emergency. */
               - iplPMB            /* Indicate if it is a PMB. */
               - iplAuthorisations /* Indicate where method is called from. Format Authorisation/Claim. */
               This routine will return the following   Output parameters:
               - opcWarning          /* Details if a warning is returned. */
               - opcWarnType         /* Indicate the waring type - Warn or Warnack. */
               - opdAuthCopayTypeObj /* hac_auth_copay_detail_item.copay_type_obj. No value = no co-payment must apply. */
               - oplCopayValueType   /* hac_auth_copay_detail_item.copayment_value_type. No value = no co-payment must apply. */
               - opdCopayValue       /* hac_auth_copay_detail_item.copayment_value. No value = no co-payment must apply. */
               - opcError            /* Details if an error is returned. */

  Notes:
------------------------------------------------------------------------------*/

  { ma/app/maauthservcopaydetailitem.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForDiscounts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForDiscounts Procedure
PROCEDURE checkForDiscounts :
/*------------------------------------------------------------------------------
  Purpose:     Method to check for Discounts on other Auth Levels
  Notes:       o        Either an auth_obj or an auth_provider_obj or an auth_detail_obj must be input.
                  This will be the record that the user is currently trying to update with a Discount value.
               o        Return an error if more than ONE input parameter has been passed in
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthObj         AS DECIMAL NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthProviderObj AS DECIMAL NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthDetailObj   AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opcErrorMessage    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cEntity AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  /*
    At least one parameter must be passed in
  */
  IF  (ipdAuthObj         = ? OR ipdAuthObj         = 0)
  AND (ipdAuthProviderObj = ? OR ipdAuthProviderObj = 0)
  AND (ipdAuthDetailObj   = ? OR ipdAuthDetailObj   = 0) THEN
  DO:
    ASSIGN opcErrorMessage = "Invalid parameters passed in to method checkForDiscount().".
    RETURN.
  END.  // IF (ipdAuthObj = ? OR ipdAuthObj = 0)

  /*
    Ensure that only one parameter was passed in
  */
  IF (ipdAuthObj <> 0 AND (ipdAuthProviderObj <> 0 OR ipdAuthDetailObj <> 0))
  OR (ipdAuthProviderObj <> 0 AND ipdAuthDetailObj <> 0) THEN
  DO:
    ASSIGN opcErrorMessage = "More than one parameter passed in to method checkForDiscount().".
    RETURN.
  END.  // IF (ipdAuthObj <> 0 AND (ipdAuthProviderObj <> 0 OR ipdAuthDetailObj <> 0))

  /*
    Validation for Authorisation Header Discount
  */
  IF ipdAuthObj <> 0 THEN
  DO:
    /*
      Check that there are no existing auth providers with discount on them.
      Discount is NOT ALLOWED on authorisation header and authorisation provider level.
      Using a CAN-FIND is the most efficient option.
      If you use the CAN-FIND here and we need to return an error with the Provider number,
      then you will need to use a FIND before the Error message to return the Provider record.
      This should still be more efficient than using FIND initially.
    */
    IF CAN-FIND(FIRST hat_auth_provider NO-LOCK
                WHERE hat_auth_provider.auth_obj = ipdAuthObj
                AND  (hat_auth_provider.auth_status <> 6
                AND   hat_auth_provider.auth_status <> 5)
                AND   hat_auth_provider.discount_auth <> 0
                AND   hat_auth_provider.discount_auth <> ?) THEN
    DO:
      FIND FIRST hat_auth_provider NO-LOCK
           WHERE hat_auth_provider.auth_obj = ipdAuthObj
           AND  (hat_auth_provider.auth_status <> 6
           AND   hat_auth_provider.auth_status <> 5)
           AND   hat_auth_provider.discount_auth <> 0
           AND   hat_auth_provider.discount_auth <> ?
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE hat_auth_provider THEN
        ASSIGN opcErrorMessage = SUBSTITUTE("A Discount may not be specified here as a Discount has already been added to Provider: &1",
                                            STRING(hat_auth_provider.doc_num)).
      ELSE
        ASSIGN opcErrorMessage = "A Discount may not be specified here as a Discount has already been added to an auth Provider.".

      RETURN.
    END.  // IF CAN-FIND(FIRST hat_auth_provider NO-LOCK

    /*
      Check that there are no existing auth detail lines with discount on them.
      Discount is NOT ALLOWED on authorisation header and authorisation detail line level.
      Using a CAN-FIND is the most efficient option.
      If you use the CAN-FIND here and we need to return an error with the Detail line entity,
      then you will need to use a FIND before the Error message to return the detail line record.
      This should still be more efficient than using FIND initially.
    */
    IF CAN-FIND(FIRST hat_auth_detail NO-LOCK
                WHERE hat_auth_detail.auth_obj = ipdAuthObj
                AND  (hat_auth_detail.auth_status <> 6
                AND   hat_auth_detail.auth_status <> 5)
                AND  (hat_auth_detail.discount_auth <> 0
                AND   hat_auth_detail.discount_auth <> ?)) THEN
    DO:
      FIND FIRST hat_auth_detail NO-LOCK
           WHERE hat_auth_detail.auth_obj = ipdAuthObj
           AND  (hat_auth_detail.auth_status <> 6
           AND   hat_auth_detail.auth_status <> 5)
           AND  (hat_auth_detail.discount_auth <> 0
           AND   hat_auth_detail.discount_auth <> ?)
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE hat_auth_detail THEN
      DO:
        CASE hat_auth_detail.owning_entity_mnemonic:
          WHEN "hlmnl":U  THEN ASSIGN cEntity = "Nappi".
          WHEN "htmtl":U  THEN ASSIGN cEntity = "Tariff".
          WHEN "hlmcr":U  THEN ASSIGN cEntity = "Basket".
          WHEN "hlmac":U  THEN ASSIGN cEntity = "ACT":U.
          OTHERWISE            ASSIGN cEntity = "Entity".
        END CASE.

        ASSIGN opcErrorMessage = SUBSTITUTE("A Discount may not be specified here as a Discount has already been added to Clinical Details: &1 &2",
                                            cEntity, hat_auth_detail.owning_alt_value).
      END.  // IF AVAILABLE hat_auth_detail THEN
      ELSE
        ASSIGN opcErrorMessage = "A Discount may not be specified here as a Discount has already been added to the Clinical Details.".

      RETURN.
    END.  // IF CAN-FIND(FIRST hat_auth_detail NO-LOCK
  END.  // IF ipdAuthObj <> 0 THEN

  /*
    Validation for Authorisation Provider Discount
  */
  IF ipdAuthProviderObj <> 0 THEN
  DO:
    FIND hat_auth_provider NO-LOCK
         WHERE hat_auth_provider.auth_provider_obj = ipdAuthProviderObj
       NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE hat_auth_provider THEN
    DO:
      /*
        Check that there no discount specified on the authorisation Header.
        Discount is NOT ALLOWED on authorisation header and authorisation provider level.
      */
      IF CAN-FIND(FIRST hat_auth NO-LOCK
                  WHERE hat_auth.auth_obj = hat_auth_provider.auth_obj
                  AND  (hat_auth.auth_status <> 6
                  AND   hat_auth.auth_status <> 5)
                  AND   hat_auth.discount_auth <> 0
                  AND   hat_auth.discount_auth <> ?) THEN
      DO:
        ASSIGN opcErrorMessage = "A Discount may not be specified here as a Discount has already been added to the Authorisation Financial level.".

        RETURN.
      END.  // IF CAN-FIND(FIRST hat_auth NO-LOCK

      /*
        Check that there are no existing auth detail lines with discount on them.
        Discount is NOT ALLOWED on authorisation provider and authorisation detail line level.
        Using a CAN-FIND is the most efficient option.
        If you use the CAN-FIND here and we need to return an error with the Detail line entity,
        then you will need to use a FIND before the Error message to return the detail line record.
        This should still be more efficient than using FIND initially.
      */
      IF CAN-FIND(FIRST hat_auth_detail NO-LOCK
                  WHERE hat_auth_detail.auth_provider_obj = ipdAuthProviderObj
                  AND  (hat_auth_detail.auth_status <> 6
                  AND   hat_auth_detail.auth_status <> 5)
                  AND   hat_auth_detail.discount_auth <> 0
                  AND   hat_auth_detail.discount_auth <> ?) THEN
      DO:
        FIND FIRST hat_auth_detail NO-LOCK
             WHERE hat_auth_detail.auth_provider_obj = ipdAuthProviderObj
             AND  (hat_auth_detail.auth_status <> 6
             AND   hat_auth_detail.auth_status <> 5)
             AND   hat_auth_detail.discount_auth <> 0
             AND   hat_auth_detail.discount_auth <> ?
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE hat_auth_detail THEN
        DO:
          CASE hat_auth_detail.owning_entity_mnemonic:
            WHEN "hlmnl":U  THEN ASSIGN cEntity = "Nappi".
            WHEN "htmtl":U  THEN ASSIGN cEntity = "Tariff".
            WHEN "hlmcr":U  THEN ASSIGN cEntity = "Basket".
            WHEN "hlmac":U  THEN ASSIGN cEntity = "ACT":U.
            OTHERWISE            ASSIGN cEntity = "Entity".
          END CASE.

          ASSIGN opcErrorMessage = SUBSTITUTE("A Discount may not be specified here as a Discount has already been added to Clinical Details: &1 &2",
                                              cEntity, hat_auth_detail.owning_alt_value).
        END.  // IF AVAILABLE hat_auth_detail THEN
        ELSE
          ASSIGN opcErrorMessage = "A Discount may not be specified here as a Discount has already been added to the Clinical Details.".

        RETURN.
      END.  // IF CAN-FIND(FIRST hat_auth_detail NO-LOCK
    END.  // IF AVAILABLE hat_auth_provider THEN
  END.  // IF ipdAuthProviderObj <> 0 THEN

  /*
    Validation for Authorisation Provider Discount
  */
  IF ipdAuthDetailObj <> 0 THEN
  DO:
    FIND hat_auth_detail NO-LOCK
         WHERE hat_auth_detail.auth_detail_obj = ipdAuthDetailObj
       NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE hat_auth_detail THEN
    DO:
      /*
        Check that there no discount specified on the authorisation Header.
        Discount is NOT ALLOWED on authorisation header and authorisation provider level.
      */
      IF CAN-FIND(FIRST hat_auth NO-LOCK
                  WHERE hat_auth.auth_obj = hat_auth_detail.auth_obj
                  AND  (hat_auth.auth_status <> 6
                  AND   hat_auth.auth_status <> 5)
                  AND   hat_auth.discount_auth <> 0
                  AND   hat_auth.discount_auth <> ?) THEN
      DO:
        ASSIGN opcErrorMessage = "A Discount may not be specified here as a Discount has already been added to the Authorisation Financial level.".

        RETURN.
      END.  // IF CAN-FIND(FIRST hat_auth NO-LOCK

      /*
        Check that there are no existing auth detail lines with discount on them.
        Discount is NOT ALLOWED on authorisation provider and authorisation detail line level.
        Using a CAN-FIND is the most efficient option.
        If you use the CAN-FIND here and we need to return an error with the Provider number,
        then you will need to use a FIND before the Error message to return the Provider record.
        This should still be more efficient than using FIND initially.
      */
      IF CAN-FIND(FIRST hat_auth_provider NO-LOCK
                  WHERE hat_auth_provider.auth_provider_obj = hat_auth_detail.auth_provider_obj
                  AND  (hat_auth_provider.auth_status <> 6
                  AND   hat_auth_provider.auth_status <> 5)
                  AND   hat_auth_provider.discount_auth <> 0
                  AND   hat_auth_provider.discount_auth <> ?) THEN
      DO:
        FIND FIRST hat_auth_provider NO-LOCK
           WHERE hat_auth_provider.auth_provider_obj = hat_auth_detail.auth_provider_obj
           AND  (hat_auth_provider.auth_status <> 6
           AND   hat_auth_provider.auth_status <> 5)
           AND   hat_auth_provider.discount_auth <> 0
           AND   hat_auth_provider.discount_auth <> ?
        NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE hat_auth_provider THEN
          ASSIGN opcErrorMessage = SUBSTITUTE("A Discount may not be specified here as a Discount has already been added to Provider: &1",
                                              STRING(hat_auth_provider.doc_num)).
        ELSE
          ASSIGN opcErrorMessage = "A Discount may not be specified here as a Discount has already been added to an auth Provider.".

        RETURN.
      END.  // IF CAN-FIND(FIRST hat_auth_detail NO-LOCK
    END.  // IF AVAILABLE hat_auth_provider THEN
  END.  // IF ipdAuthProviderObj <> 0 THEN

&ENDIF

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForDuplicateAuths) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForDuplicateAuths Procedure
PROCEDURE checkForDuplicateAuths :
/*------------------------------------------------------------------------------
  Purpose:    Check for Auth duplicates and cater for overlapping dates when checking for duplicates.
  Parameters: <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.
  DEFINE OUTPUT       PARAMETER TABLE   FOR tt_auth_type.
  DEFINE OUTPUT       PARAMETER             opcMessage  AS CHARACTER  NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSuccess                   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE iLogTime                   AS INTEGER              NO-UNDO.
  DEFINE VARIABLE cTrackingMessage           AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthNumList               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE lDuplCheckStatusValidRule  AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cDuplCheckStatusRuleValue  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusDescr               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMemNum                    AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iOptionCode                AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iDependant                 AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj               AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dAuthObj                   AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dStartDate                 AS DATE                 NO-UNDO.
  DEFINE VARIABLE dEndDate                   AS DATE                 NO-UNDO.

  DEFINE VARIABLE oAuthSearch                AS cls.maauthsearch     NO-UNDO.
  DEFINE VARIABLE oAuthTypeSearch            AS cls.maauthtypesearch NO-UNDO.

  DEFINE BUFFER buf_auth FOR hat_auth.
  DEFINE BUFFER tt_auth FOR tt_auth.

  FIND FIRST tt_auth NO-LOCK.

  IF AVAILABLE tt_auth THEN
  DO:

    ASSIGN
      dAuthTypeObj = tt_auth.auth_type_obj
      dAuthObj     = tt_auth.auth_obj
      cMemNum      = tt_auth.mem_num
      iOptionCode  = tt_auth.option_code
      iDependant   = tt_auth.dependant
      dStartDate   = tt_auth.start_date
      dEndDate     = tt_auth.end_date.

    /*
     Check rule to get the authorisation statuses to be included in the Duplicate Status Check,
     when the Duplicate Body region is validated
   */
    ASSIGN iLogTime = ETIME.
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  tt_auth.insurer_obj,
                                                   INPUT  iOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                   INPUT  "DuplicateCheckStatuses":U,
                                                   INPUT  dStartDate,
                                                   OUTPUT lDuplCheckStatusValidRule,
                                                   OUTPUT cDuplCheckStatusRuleValue).

    ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(DuplicateCheckStatuses) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN iLogTime = ETIME

           cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                         INPUT tt_auth.auth_status)
           cStatusDescr = STRING(tt_auth.auth_status) + "-":U + cStatusDescr.

    ASSIGN cTrackingMessage = "AuthService:getStatusDescription completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF lDuplCheckStatusValidRule AND LOOKUP(cStatusDescr, cDuplCheckStatusRuleValue) > 0 THEN
    DO:
      FIND FIRST buf_auth NO-LOCK
           WHERE buf_auth.mem_num       = cMemNum
             AND buf_auth.dependant     = iDependant
             AND (buf_auth.option_code   = iOptionCode
               OR buf_auth.option_code   = 0)
             AND buf_auth.auth_type_obj = dAuthTypeObj
             AND buf_auth.auth_status   = tt_auth.auth_status
             AND buf_auth.start_date   <= tt_auth.start_date
             AND (buf_auth.end_date     = ?
              OR  buf_auth.end_date    >= tt_auth.start_date)
          NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE buf_auth
      THEN
        FIND FIRST buf_auth NO-LOCK
             WHERE buf_auth.mem_num       = cMemNum
               AND buf_auth.dependant     = iDependant
               AND (buf_auth.option_code   = iOptionCode
                 OR buf_auth.option_code   = 0)
               AND buf_auth.auth_type_obj = dAuthTypeObj
               AND buf_auth.auth_status   = tt_auth.auth_status
               AND buf_auth.start_date   >= tt_auth.start_date
               AND (buf_auth.end_date     = ?
                OR  buf_auth.end_date    <= tt_auth.start_date)
            NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE buf_auth AND tt_auth.end_date <> ?
      THEN
        FIND FIRST buf_auth NO-LOCK
             WHERE buf_auth.mem_num       = cMemNum
               AND buf_auth.dependant     = iDependant
               AND (buf_auth.option_code   = iOptionCode
                 OR buf_auth.option_code   = 0)
               AND buf_auth.auth_type_obj = dAuthTypeObj
               AND buf_auth.auth_status   = tt_auth.auth_status
               AND buf_auth.start_date   <= tt_auth.end_date
               AND (buf_auth.end_date     = ?
                OR  buf_auth.end_date    >= tt_auth.end_date)
            NO-ERROR.
         { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

      IF AVAILABLE buf_auth THEN
      DO:
        IF tt_auth._data_load THEN
        DO:
          ASSIGN opcMessage = SUBSTITUTE("A possible duplicate Authorisation was found: &1", buf_auth.auth_num ).
        END. /* IF tt_auth._data_load THEN */
        ELSE DO:
          DATASET dsAuthorisation:EMPTY-DATASET().

          ASSIGN
            oAuthSearch  = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE)
            lSuccess     = oAuthSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_provider,tt_auth_coding":U)
            lSuccess     = oAuthSearch:SetFilterCriteria("tt_auth.auth_obj":U     , "<>":U, dAuthObj)
            lSuccess     = oAuthSearch:SetFilterCriteria("tt_auth.mem_num":U      , "=":U , cMemNum)
            lSuccess     = oAuthSearch:SetFilterCriteria("tt_auth.dependant":U    , "=":U , iDependant)
            lSuccess     = oAuthSearch:SetFilterCriteria("tt_auth.option_code":U  , "=":U , iOptionCode)
            lSuccess     = oAuthSearch:SetFilterCriteria("tt_auth.option_code":U  , "=":U , 0)
            lSuccess     = oAuthSearch:SetFilterCriteria("tt_auth.auth_type_obj":U, "=":U , dAuthTypeObj).

            oAuthSearch:fetchData().

          /*
            o     Remove the Auth records which are not set up in the DuplicateCheckStatuses rule.
            o     Remove the Auth records which are not duplicates according to the: Start_date range and End_date range
          */
          FOR EACH tt_auth NO-LOCK:

            ASSIGN cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                                 INPUT tt_auth.auth_status)
                   cStatusDescr = STRING(tt_auth.auth_status) + "-":U + cStatusDescr.

            IF LOOKUP(cStatusDescr, cDuplCheckStatusRuleValue) = 0 THEN
              DELETE tt_auth.

            IF (tt_auth.start_date < dStartDate AND tt_auth.end_date < dStartDate) OR
               (tt_auth.start_date > dEndDate)
            THEN
              DELETE tt_auth.

          END. /* FOR EACH tt_auth NO-LOCK */

          /* If there are any records available  */
          ASSIGN oAuthTypeSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).

          FOR EACH tt_auth NO-LOCK:
            ASSIGN
              lSuccess   = oAuthTypeSearch:SetCriteria("BufferList":U, "tt_auth_type":U)
              lSuccess   = oAuthTypeSearch:SetFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, tt_auth.auth_type_obj).

            oAuthTypeSearch:fetchData().

            /* Build up a list of Duplicate Auths */
            ASSIGN
              cAuthNumList = cAuthNumList
                           + (IF cAuthNumList = "":U THEN "":U ELSE ",")
                           + tt_auth.auth_num.
          END. /* FOR EACH tt_auth NO-LOCK: */

          ASSIGN opcMessage = SUBSTITUTE("Potential duplicate Auth records were found: &1", cAuthNumList ).

        END. /* ELSE DO: */
      END. /* IF AVAILABLE tt_auth THEN */
    END. /* IF lDuplCheckStatusValidRule AND */
  END. /* FOR EACH tt_auth: */

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oAuthSearch) THEN DELETE OBJECT oAuthSearch.
                IF VALID-OBJECT(oAuthTypeSearch) THEN DELETE OBJECT oAuthTypeSearch."}

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForPenalty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForPenalty Procedure
PROCEDURE checkForPenalty :
/*------------------------------------------------------------------------------
  Purpose   : To check if a Penalty should apply on an Auth Provider
  Parameters: Input parameters: ipdInsurerObj            - Client
                                 ipiOptionCode           - Member option code
                                 ipdStartDate            - Authorisation start date.
                                 ipdRequestDate          - Date authorisation is captured/requested.
                                 ipcEmergency            - Indicate if it is an emergency.
                                 ipcPenalty              - Indicate if a penalty must apply.
                                 ipcLateAuth             - Indicate if it is a late auth.
                                 ipiPrType               - Provider disciplines exceptions.
              Output parameters: opcPenalty              - Indicate if a penalty must apply.
                                 opcLateAuth             - Indicate if it is a late auth.
                                 opcPenaltyWarning       - Indicate if a warning must be populated.
                                 opcPenaltyWarningType   - Indicate warning type.
                                 opdPenaltyCopayTypeObj  - Co-payment type that must apply for a penalty.
                                 opcPenaltyValueType     - Indicate if a rand value or percentage must apply.
                                 opdPenaltyValue         - Penalty value that must apply.
                                 opcPenaltyError         - Error that will be returned if any setups are missing.
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj            AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode            AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate             AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER ipdRequestDate           AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER ipcEmergency             AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ipcPenalty               AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ipcLateAuth              AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ipiPrType                AS INTEGER    NO-UNDO.

  DEFINE OUTPUT PARAMETER opcPenalty               AS CHARACTER  NO-UNDO  INITIAL "".
  DEFINE OUTPUT PARAMETER opcLateAuth              AS CHARACTER  NO-UNDO  INITIAL "".
  DEFINE OUTPUT PARAMETER opcPenaltyWarning        AS CHARACTER  NO-UNDO  INITIAL "".
  DEFINE OUTPUT PARAMETER opcPenaltyWarningType    AS CHARACTER  NO-UNDO  INITIAL "".
  DEFINE OUTPUT PARAMETER opdPenaltyCopayTypeObj   AS DECIMAL    NO-UNDO  INITIAL 0.
  DEFINE OUTPUT PARAMETER opcPenaltyValueType      AS CHARACTER  NO-UNDO  INITIAL "".
  DEFINE OUTPUT PARAMETER opdPenaltyValue          AS DECIMAL    NO-UNDO  INITIAL 0.
  DEFINE OUTPUT PARAMETER opcPenaltyError          AS CHARACTER  NO-UNDO  INITIAL "".
  DEFINE OUTPUT PARAMETER oplClearReason           AS LOGICAL    NO-UNDO  INITIAL NO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cCopayType         AS CHARACTER  NO-UNDO  INITIAL "".      /* Save the co-payment type as setup in the rule value */
  DEFINE VARIABLE cRuleValue         AS CHARACTER  NO-UNDO  INITIAL "".      /* Save the Late Auth Penalty rule value */
  DEFINE VARIABLE cExceptRuleValue   AS CHARACTER  NO-UNDO  INITIAL "".      /* Save the Late Auth Exceptions rule value */
  DEFINE VARIABLE cWarnRuleValue     AS CHARACTER  NO-UNDO  INITIAL "".      /* Save the Late Auth Warning rule value */

  DEFINE VARIABLE iPenaltySetup      AS INTEGER    NO-UNDO  INITIAL ?.       /* Save the penalty period as setup in the rule value */
  DEFINE VARIABLE iWorkDays          AS INTEGER    NO-UNDO  INITIAL ?.       /* Save the workdays passed back from method checkWorkDays */

  DEFINE VARIABLE lEmergency         AS LOGICAL    NO-UNDO  INITIAL ?.       /* Save the Emergency indicator as saved in the rule value */
  DEFINE VARIABLE lErrorMessage      AS LOGICAL    NO-UNDO  INITIAL NO.
  DEFINE VARIABLE lValidRule         AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lExceptValidRule   AS LOGICAL    NO-UNDO.

  /* Get rule LateAuthPenalty */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                 INPUT  "LateAuthPenalty":U,
                                                 INPUT  ipdStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  /* Leave method if rule is invalid or has no values */
  IF NOT lValidRule
  OR cRuleValue = ""
  THEN RETURN.

  /* Get co-payment type that must apply for a Penalty from rule LateAuthPenalty */
  ASSIGN cCopayType = ENTRY(1,cRuleValue,"|").

  IF cCopayType = ""
  THEN DO:
    ASSIGN opcPenaltyError = "Co-payment Type setup is missing in Auth Rule value."
                           + "[HELP=Auth Rule Code: LateAuthPenalty]".
    RETURN.
  END.   /* IF NOT AVAILABLE ham_auth_copay_type */
  ELSE IF NUM-ENTRIES(cRuleValue,"|":U) <> 5 THEN
  DO:
    ASSIGN opcPenaltyError = "Invalid rule value setup for Auth Rule."
                           + "[HELP=Auth Rule Code: LateAuthPenalty]".
    RETURN.
  END.

  FIND ham_auth_copay_type NO-LOCK
    WHERE ham_auth_copay_type.auth_copay_type = cCopayType
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE ham_auth_copay_type
  THEN DO:
    ASSIGN opcPenaltyError = SUBSTITUTE("Invalid Co-payment Type '&1' setup in Auth Rule value.",cCopayType)
           opcPenaltyError = opcPenaltyError + "[HELP=Auth Rule Code: LateAuthPenalty]".
    RETURN.
  END.   /* IF NOT AVAILABLE ham_auth_copay_type */

  /* Get rule LateAuthExceptions */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                 INPUT  "LateAuthExceptions":U,
                                                 INPUT  ipdStartDate,
                                                 OUTPUT lExceptValidRule,
                                                 OUTPUT cExceptRuleValue).


  /* Leave method if rule exists and Provider practice type matches discipline code setup in rule value */
  IF  lExceptValidRule
  AND LOOKUP(STRING(ipiPrType,"999"),cExceptRuleValue) > 0
  THEN DO:
    ASSIGN opcPenalty             = "NO":U
           opcLateAuth            = "NO":U
           opdPenaltyCopayTypeObj = ham_auth_copay_type.auth_copay_type_obj.
    RETURN.
  END.  /* IF lExceptValidRule */

  /* Determine in the Rule setup, if the penalty must apply for an emergency */
  ASSIGN lEmergency = IF ENTRY(5,cRuleValue,"|") = "YES":U
                      THEN YES
                      ELSE IF ENTRY(5,cRuleValue,"|") = "NO":U
                           THEN NO
                           ELSE ?.
  IF lEmergency = ?
  THEN DO:
    ASSIGN opcPenaltyError = SUBSTITUTE("The Emergency indicator setup (&1) in Auth Rule value is invalid.",ENTRY(5,cRuleValue,"|"))
           opcPenaltyError = opcPenaltyError + "[HELP=Auth Rule Code: LateAuthPenalty]".
    RETURN.
  END.  /* IF lEmergency = ? */

  /* Check if the penalty flag was updated by the user and if the penalty should still apply or not */
  IF  ipcPenalty  = "NO":U
  AND ipcLateAuth = "YES":U
  THEN DO:
    ASSIGN opcPenalty             = "NO":U
           opcLateAuth            = "YES":U
           oplClearReason         = IF ipcEmergency = "YES":U AND lEmergency = NO
                                    THEN YES
                                    ELSE NO
           opdPenaltyCopayTypeObj = ham_auth_copay_type.auth_copay_type_obj.
    RETURN.
  END.  /* IF  ipcPenalty  = "NO" */

  /* Get Penalty Setup value */
  ASSIGN opcPenaltyError = ""
         iPenaltySetup   = INTEGER(ENTRY(2,cRuleValue,"|")) NO-ERROR.

  IF ERROR-STATUS:ERROR
  THEN ASSIGN opcPenaltyError = SUBSTITUTE("Invalid Penalty Period '&1' setup in Auth Rule value.",iPenaltySetup)
              opcPenaltyError = opcPenaltyError + "[HELP=Auth Rule Code: LateAuthPenalty]".

  IF iPenaltySetup = 0
  THEN ASSIGN opcPenaltyError = "No Penalty Period is setup in Auth Rule value."
                              + "[HELP=Auth Rule Code: LateAuthPenalty]".

  IF opcPenaltyError <> ""
  THEN RETURN.

  /* Check if the authorisation is late, if ipcLateAuth = "" */
  IF ipcLateAuth = ""
  THEN DO:
    /* Call method checkWorkDays to determine the number of workdays to be checked, to determine if a penalty should apply */
    mipEnv:Health:maUtility:checkWorkDays(INPUT  ipdStartDate,
                                          INPUT  ipdRequestDate,
                                          INPUT  NO,             //iplInclStartDate,
                                          INPUT  YES,            //iplInclEndDate,
                                          OUTPUT iWorkDays).

    /* If Authorisation is not late according to workdays calculated, no penalty will be applied */
    /* For schemes e.g. Profmed with a Positive Penalty Rule value setup, if the Workdays >= Penalty Period, then it is a Penalty */
    /* For other schemes e.g. Genesis with a Negative Penalty Rule value setup, if the Workdays > Penalty Period, then it is a Penalty */
    IF iWorkDays <> ?
    AND ((iPenaltySetup >  0 AND iWorkDays <  iPenaltySetup)    /* For Profmed eg */
    OR   (iPenaltySetup <= 0 AND iWorkDays <= iPenaltySetup))   /* For Genesis eg */
    THEN DO:
      ASSIGN opcPenalty             = "NO":U
             opcLateAuth            = "NO":U
             opdPenaltyCopayTypeObj = ham_auth_copay_type.auth_copay_type_obj.
      RETURN.
    END.  /* IF (iWorkDays <> ? AND iWorkDays > iPenaltySetup) AND ipcLateAuth = "" */
  END.  /* IF ipcLateAuth = "" */

  /* Determine if the penalty must apply:
     If the workdays > the period setup in rule LateAuthPenalty OR it is a late Auth
  */
  IF ipcLateAuth = "YES":U
  OR (iWorkDays <> ?
  AND ((iPenaltySetup >  0 AND iWorkDays >= iPenaltySetup)      /* For Profmed eg */
  OR   (iPenaltySetup <= 0 AND iWorkDays >  iPenaltySetup)))    /* For Genesis eg */
  THEN DO:

    /* Check if the penalty must apply for an Emergency. */
    IF  ipcEmergency = "YES":U
    AND lEmergency   = NO
    THEN DO:
      ASSIGN opcPenalty             = "NO":U            /* We don't want to apply the penalty */
             oplClearReason         = YES
             opcLateAuth            = "YES":U           /* We still want to indicate that the auth is late */
             opdPenaltyCopayTypeObj = ham_auth_copay_type.auth_copay_type_obj.

      RETURN.
    END.  /* IF ipcEmergency = "YES":U AND lEmergency = NO */

    /* Ensure that the Penalty Value and Penalty Value Type contain valid values before they are returned */
    ASSIGN opcPenaltyValueType = ENTRY(3,cRuleValue,"|").
    IF opcPenaltyValueType = ""
    OR LOOKUP(opcPenaltyValueType,"Amount,Percentage") = 0
    THEN DO:
      ASSIGN opcPenaltyError = SUBSTITUTE("Invalid Penalty Value Type '&1' setup in Auth Rule value." +
                                          "[HELP=Auth Rule Code: LateAuthPenalty]",opcPenaltyValueType).
      RETURN.
    END.  /* IF ENTRY(3,cRuleValue,"|") = "" */

    ASSIGN opdPenaltyValue = DECIMAL(ENTRY(4,cRuleValue,"|")) NO-ERROR.
    IF ERROR-STATUS:ERROR
    OR opdPenaltyValue = 0 THEN
    DO:
      ASSIGN opcPenaltyError = SUBSTITUTE("Invalid Penalty Value '&1' setup in Auth Rule value." +
                                          "[HELP=Auth Rule Code: LateAuthPenalty]",ENTRY(4,cRuleValue,"|")).
      RETURN.
    END.  /* IF ERROR-STATUS:ERROR */

    /* Assign penalty values needed because a penalty applies */
    ASSIGN opcPenalty             = IF ipcPenalty = ""
                                    THEN "YES":U        /* Apply the penalty */
                                    ELSE ipcPenalty
           opcLateAuth            = "YES":U             /* Indicate that the auth is late */
           opdPenaltyCopayTypeObj = ham_auth_copay_type.auth_copay_type_obj.

    /* Get rule LateAuthWarn to check if a warning message must be populated when a penalty is applied */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                   INPUT  ipiOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                   INPUT  "LateAuthWarn":U,
                                                   INPUT  ipdStartDate,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cWarnRuleValue).

    /* Leave method if rule is invalid or has no values */
    IF  lValidRule THEN
    DO:
      IF cWarnRuleValue = ""
      OR NUM-ENTRIES(cWarnRuleValue,"|":U) < 2
      OR TRIM(ENTRY(1,cWarnRuleValue,"|":U)) = ""
      OR LOOKUP(ENTRY(2,cWarnRuleValue,"|":U),"Warn,WarnAck":U) = 0
      THEN
        ASSIGN lErrorMessage = YES.
      ELSE
        ASSIGN opcPenaltyWarning     = ENTRY(1,cWarnRuleValue,"|":U) + "[HELP=Auth Rule Code: LateAuthPenalty, " +
                                       IF lExceptValidRule THEN "LateAuthExceptions, LateAuthWarn]" ELSE "LateAuthWarn]":U
               opcPenaltyWarningType = ENTRY(2,cWarnRuleValue,"|":U).
    END.  // IF lValidRule THEN
    ELSE
      ASSIGN lErrorMessage = YES.

    IF lErrorMessage THEN
    DO:
      ASSIGN opcPenaltyError = "Invalid value setup in Auth Rule."
                             + "[HELP=Auth Rule Code: LateAuthWarn]".
      RETURN.
    END.
  END.  /* IF (iWorkDays <> ? AND iWorkDays <= iPenaltySetup) OR ipcLateAuth = "YES":U*/
  ELSE
    ASSIGN opcPenalty             = "NO":U  /* We don't want to apply the penalty */
           oplClearReason         = YES
           opcLateAuth            = "NO":U  /* This is not a late auth */
           opdPenaltyCopayTypeObj = 0.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForUpdates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForUpdates Procedure
PROCEDURE checkForUpdates :
/*------------------------------------------------------------------------------
  Purpose   : Automatic rebuild detection
              This routine/service will be used by a rest request to check if
              there are any updates to the specified authorisation from the
              last know update and if so, the relevant container html will be
              returned for a rebuild.
  Parameters:
  Notes     :
  Author    : Andrewd
------------------------------------------------------------------------------*/

  {ma/app/maauthservcheckforupd.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkLimits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkLimits Procedure
PROCEDURE checkLimits :
/*------------------------------------------------------------------------------
  Purpose:     This procedure will do Limit checking on all the levels of the
               Authorisation where authorised values can be specified
  Parameters:  ipcAuthLevel - Specify 'Header', 'Provider' or 'Detail'
  Notes:
------------------------------------------------------------------------------*/

{ma/app/maauthservchecklimits.i}

END PROCEDURE.  /* checkLimits */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkWeekendPass) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkWeekendPass Procedure
PROCEDURE checkWeekendPass :
/*------------------------------------------------------------------------------
  Purpose   : To check if Weekend Pass should apply on a LOC line
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthTypeObj         AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj          AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode          AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthStartDate       AS DATE      NO-UNDO.
  DEFINE INPUT  PARAMETER ipcTariffCode          AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipiAuthStatus          AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcAuthStatusReason    AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplWeekendPass         AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER oplDeclineLOC          AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDeclineStatusReason AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcError               AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE lValidRule                     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cRuleValue                     AS CHARACTER NO-UNDO.

  ASSIGN oplWeekendPass         = FALSE
         oplDeclineLOC          = FALSE
         opcDeclineStatusReason = ""
         opcError               = "".

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                 INPUT  "WeekendPass":U,
                                                 INPUT  ipdAuthStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  IF  lValidRule
  AND cRuleValue <> ""
  AND NUM-ENTRIES(cRuleValue,"|") > 1 THEN
  DO:
    EMPTY TEMP-TABLE ttAuthTypeConfig.

    mipEnv:Health:AuthService:getAuthTypeConfig (INPUT ipdAuthTypeObj,                  /* ipdAuthTypeObj   */
                                                 INPUT ipdInsurerObj,                   /* ipdInsurerObj    */
                                                 INPUT ipiOptionCode,                   /* ipiOptionCode    */
                                                 INPUT ipdAuthStartDate,                /* ipdEffectiveDate */
                                                 INPUT-OUTPUT TABLE ttAuthTypeConfig).

    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE ttAuthTypeConfig THEN
    DO:
      ASSIGN oplWeekendPass = FALSE
             opcError       = SUBSTITUTE("No auth type record available for auth type obj = &1",ipdAuthTypeObj).
      RETURN.
    END.  /* IF NOT AVAILABLE ttAuthTypeConfig THEN */

    /*
      Weekend pass setup with Tariff Code
    */
    IF  ENTRY(1,cRuleValue,"|") <> ""
    AND ipcTariffCode <> ""
    AND LOOKUP(ipcTariffCode,ENTRY(1,cRuleValue,"|")) <> 0 THEN
    DO:
      IF ttAuthTypeConfig.ActivateLOSWeekendPass
      THEN
        ASSIGN oplWeekendPass         = TRUE
               oplDeclineLOC          = TRUE
               opcDeclineStatusReason = ENTRY(2,cRuleValue,"|").
      ELSE
        ASSIGN opcError = "Weekend Pass is not allowed for the Authorisation Type."
                        + "[HELP=Auth Rule Code: WeekendPass]".
      RETURN.
    END. /* IF ENTRY(1,cRuleValue,",") <> "" AND ipcTariffCode <> "" AND */

    /*
      Weekend pass setup with Status Reason
    */
    IF  ENTRY(1,cRuleValue,"|") = ""
    AND ENTRY(2,cRuleValue,"|") = ipcAuthStatusReason
    AND ipiAuthStatus = 6 THEN
    DO:
      IF ttAuthTypeConfig.ActivateLOSWeekendPass
      THEN
        ASSIGN oplWeekendPass = TRUE.
      ELSE
        ASSIGN opcError = "Weekend Pass is not allowed for the Authorisation Type."
                        + "[HELP=Auth Rule Code: WeekendPass]".

      RETURN.
    END. /* IF ENTRY(1,cRuleValue,",") = "" AND */
  END. /* IF lValidRule */
  ELSE
    RETURN.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearAuthTypeConfigCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearAuthTypeConfigCache Procedure
PROCEDURE clearAuthTypeConfigCache :
/*------------------------------------------------------------------------------
  Purpose   : Clear cached auth type config data
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  FOR EACH ttConfigCache EXCLUSIVE-LOCK:

    DELETE ttConfigCache.
  END. /*FOR EACH ttConfigCache EXCLUSIVE-LOCK:*/

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyAuth Procedure
PROCEDURE copyAuth :
/*------------------------------------------------------------------------------
  Purpose:     Copy an auth/batch of auths according to the criteria in
               the tt_authcopy temp table . Return all errors related
               to the copy in tt_copy_error
  Parameters:  tt_authcopy
               tt_auth_copy_error
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_authcopy.
  DEFINE       OUTPUT PARAMETER TABLE FOR tt_auth_copy_error.
 // DEFINE       OUTPUT PARAMETER TABLE FOR tt_auth_copy_success .

  DEFINE VARIABLE lSuccess       AS  LOGICAL           NO-UNDO.
  DEFINE VARIABLE cMessage       AS  CHARACTER         NO-UNDO.
  DEFINE VARIABLE cNewAuthNum    AS  CHARACTER         NO-UNDO.
  DEFINE VARIABLE oErrorObject   AS  cls.maerrorobject NO-UNDO.

  DEFINE BUFFER btt_auth_copy_error   FOR tt_auth_copy_error.
  DEFINE BUFFER btt_authcopy          FOR tt_authcopy.
  DEFINE BUFFER btt_auth              FOR tt_auth.
  DEFINE BUFFER btt_temp_auth         FOR tt_temp_auth.

  EMPTY TEMP-TABLE tt_auth_copy_error.
  EMPTY TEMP-TABLE tt_auth_copy_success.

  ASSIGN oErrorObject    = NEW cls.maerrorobject(TEMP-TABLE btt_auth_copy_error:HANDLE ).

  FIND FIRST btt_authcopy NO-ERROR .

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE btt_authcopy THEN
  DO:
    ASSIGN cMessage  =  "No auth copy temp table has been supplied."
           lSuccess  =  oErrorObject:addError(INPUT 'hataucopy',        // ipcOwningEntityMnemonic
                                              INPUT 0,                  // ipdOwningEntityObj
                                              INPUT "":U,               // ipcOwningEntityKey
                                              INPUT 0,                  // ipiLineNumber
                                              INPUT cMessage,           // ipcMessageText
                                              INPUT "ERR":U) NO-ERROR.  // ipcMessageType

    RETURN.
  END. //IF NOT AVAILABLE btt_authcopy THEN

  // Validate copy criteria
  RUN _validateAuthCopyCriteria IN TARGET-PROCEDURE(BUFFER btt_authcopy,
                                                    INPUT-OUTPUT TABLE  tt_auth_copy_error ).


  // If we have any missing/incorrect copy criteria , return to the calling procedure
  IF oErrorObject:CanFind("hataucopy":U , 0 , "":U, "ERR":U)
  THEN
    RETURN.

  //Get the batch of auths to be copied
  RUN _retrieveAuthsToCopy IN TARGET-PROCEDURE(BUFFER btt_authcopy,
                                               OUTPUT DATASET dsAuthorisation).

  // Copy the batch of auths into a seperate handle than the temp table in the dataset . We do this ,because the auth dataset is a globally defined entity
  // and there might be other procedures that modify it in the midst of processing. This way we have the security that the temp table will remain unchanged
  TEMP-TABLE tt_temp_auth:COPY-TEMP-TABLE(TEMP-TABLE tt_auth:HANDLE).

  //Now lets call the procedure that will make the service call and save the auths to the database
  COPY-AUTH-RECORDS-BLK:
  FOR EACH btt_temp_auth:

    RUN _copyAuthRecord  IN TARGET-PROCEDURE(INPUT btt_temp_auth.auth_obj,
                                             INPUT TABLE tt_authcopy,
                                             OUTPUT cNewAuthNum,
                                             OUTPUT TABLE tt_auth_error ) .

    IF NOT CAN-FIND(FIRST tt_auth_error)
    AND cNewAuthNum <> ?
    AND cNewAuthNum <> "":U THEN
    DO:
      CREATE tt_auth_copy_success.

      ASSIGN tt_auth_copy_success.owning_entity_mnemonic = "hatau":U
             tt_auth_copy_success.owning_key             = cNewAuthNum
             tt_auth_copy_success.success_message        = SUBSTITUTE("New authorisation succesfully created . Copied from Auth Num: &1":U , btt_temp_auth.auth_num) .

    END. //IF NOT CAN-FIND(FIRST tt_auth_error)
    ELSE
      //Append errors returned in to copy error temp table
      TEMP-TABLE tt_auth_copy_error:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_error:HANDLE , TRUE, FALSE , TRUE ).

  END. //COPY-AUTH-RECORDS-BLK:


  {mip/inc/mipcatcherror.i
    &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject.
              DATASET dsAuthorisation:EMPTY-DATASET().
              EMPTY TEMP-TABLE tt_temp_auth. " }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-determineTariffModifierType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE determineTariffModifierType Procedure
PROCEDURE determineTariffModifierType :
/*------------------------------------------------------------------------------
  Purpose:  Determine if a the tariff link obj passed in is set up as a parent of
            another tariff and/or if it is set up as a child modifier of another tariff
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdTariffLinkObj     AS DECIMAL NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate         AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER oplParentOfModifier  AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER oplChildModifier     AS LOGICAL NO-UNDO.

  DEFINE VARIABLE cAcronymKeyList AS CHARACTER NO-UNDO INITIAL "ma_acCodeLinkCatCreateTariffAGE,ma_acCodeLinkCatCreateTariffBMI,ma_acCodeLinkCatCreateTariffCDL" .
  DEFINE VARIABLE cAcronymKey     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.

  DEFINE BUFFER buf_code_link FOR hlm_code_link .

  ACRONYM-KEY-BLK:
  DO iCount = 1 TO NUM-ENTRIES(cAcronymKeyList) :

    ASSIGN cAcronymKey = ENTRY(iCount, cAcronymKeyList) .

    IF NOT oplParentOfModifier
    THEN
      FIND FIRST buf_code_link NO-LOCK
           WHERE buf_code_link.acronym_key       = cAcronymKey
             AND buf_code_link.parent_entity     = "htmtl":U
             AND buf_code_link.parent_entity_obj = ipdTariffLinkObj
             AND buf_code_link.parent_entity_key = "":U
             AND buf_code_link.child_entity      = "htmtl":U
             AND buf_code_link.effective_date    < ipdStartDate NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    IF AVAILABLE(buf_code_link)
    THEN
      ASSIGN oplParentOfModifier = TRUE .

    IF NOT oplChildModifier
    THEN
      FIND FIRST buf_code_link
           WHERE buf_code_link.acronym_key       = cAcronymKey
             AND buf_code_link.child_entity      = "htmtl":U
             AND buf_code_link.child_entity_obj  = ipdTariffLinkObj
             AND buf_code_link.child_entity_key  = "":U
             AND buf_code_link.parent_entity     = "htmtl":U
             AND buf_code_link.effective_date    < ipdStartDate NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = FALSE }

    IF AVAILABLE(buf_code_link)
    THEN
      ASSIGN oplChildModifier = TRUE .

  END. //ACRONYM-KEY-BLK

  {mip/inc/mipcatcherror.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAuthBMI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAuthBMI Procedure
PROCEDURE getAuthBMI :
/*------------------------------------------------------------------------------
  Purpose: get the Auth BMI value
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER TABLE FOR tt_dephealth.
  DEFINE        PARAMETER BUFFER btt_auth FOR tt_auth.
  DEFINE OUTPUT PARAMETER opdBMIValue  AS DECIMAL               NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER btt_dephealth FOR tt_dephealth.

  DEFINE VARIABLE lValidRule           AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE cRuleValue           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE dInsurerObj          AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dAuthObj             AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE iOptionCode          AS INTEGER               NO-UNDO.
  DEFINE VARIABLE dEffectiveDate       AS DATE                  NO-UNDO.

  IF AVAILABLE btt_auth
  THEN
    ASSIGN
      dAuthObj       = btt_auth.auth_obj
      dInsurerObj    = btt_auth.insurer_obj
      iOptionCode    = btt_auth.option_code
      dEffectiveDate = btt_auth.start_date.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                 INPUT  iOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                 INPUT  "DependantBMI":U,
                                                 INPUT  dEffectiveDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  IF lValidRule THEN
  DO:
    FIND FIRST tt_dephealth NO-LOCK
         WHERE tt_dephealth.related_entity_mnemonic = "hatau":U
           AND tt_dephealth.related_obj             = dAuthObj
           AND tt_dephealth.detail_identifier       = cRuleValue
           AND tt_dephealth.end_date_time           = ?
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE tt_dephealth
    THEN
      ASSIGN opdBMIValue = DECIMAL(tt_dephealth.detail_value).
    ELSE
      ASSIGN opdBMIValue = 0.00.

  END. /* IF lValidRule THEN */
  ELSE
      ASSIGN opdBMIValue = 0.00.

{ mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAuthExternalReference) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAuthExternalReference Procedure
PROCEDURE getAuthExternalReference :
/*------------------------------------------------------------------------------
  Purpose   : Get resolved auth external reference records
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

 { ma/app/maauthservgetauthextref.i }

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAuthRequestValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAuthRequestValues Procedure
PROCEDURE getAuthRequestValues :
/*------------------------------------------------------------------------------
  Purpose   : To display the authorised and requested summary values
  Parameters: Input parameters: ipdAuthObj            - Obj linking to the authorisation

              Output parameters: ttAuthRequestValues  - Temp Table with all contents and values.

  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthObj AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER TABLE FOR ttAuthRequestValues.

  DEFINE VARIABLE cRuleValue            AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cTariffTypeList       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTariffTypeObjList    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dAuthDetailObjInFocus AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cError                AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER buf_auth_detail FOR hat_auth_detail.

  EMPTY TEMP-TABLE ttAuthRequestValues.

  CREATE ttAuthRequestValues.

  ASSIGN
    ttAuthRequestValues.auth_obj                    = ipdAuthObj
    ttAuthRequestValues.total_amount_requested      = 0.00
    ttAuthRequestValues.total_amount_authorised     = 0.00
    ttAuthRequestValues.initial_fee_requested       = 0.00
    ttAuthRequestValues.initial_fee_authorised      = 0.00
    ttAuthRequestValues.instalment_requested        = 0.00
    ttAuthRequestValues.instalment_authorised       = 0.00
    ttAuthRequestValues.instalment_num_requested    = 0.0
    ttAuthRequestValues.instalment_num_authorised   = 0.0.

  FIND FIRST htm_tariff_type NO-LOCK
       WHERE tariff_type_code = "InitialFee" NO-ERROR.

  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

  IF NOT AVAILABLE htm_tariff_type
  THEN
    RETURN.
  ELSE
  DO:

      CHECK-INITIALFEE:
      FOR EACH hat_auth_detail NO-LOCK
         WHERE hat_auth_detail.auth_obj = ipdAuthObj:

        ASSIGN
          cError = "":U.

        mipEnv:health:maMedical:getTariffTypes(INPUT  hat_auth_detail.owning_obj ,
                                               INPUT  hat_auth_detail.start_date,
                                               INPUT  "":U ,
                                               OUTPUT cTariffTypeList,
                                               OUTPUT cTariffTypeObjList,
                                               OUTPUT cError  ) .

        IF LOOKUP("InitialFee":U, cTariffTypeList) > 0
        THEN
          ASSIGN dAuthDetailObjInFocus                      = hat_auth_detail.auth_detail_obj
                 ttAuthRequestValues.initial_fee_requested  = hat_auth_detail.amount_requested
                 ttAuthRequestValues.initial_fee_authorised = hat_auth_detail.amount_auth.

        IF dAuthDetailObjInFocus <> 0.00 AND cError = "":U THEN
          LEAVE CHECK-INITIALFEE.
      END. /* CHECK-INITIALFEE*/

      IF dAuthDetailObjInFocus <> 0.00 THEN
      DO:
        FIND FIRST buf_auth_detail NO-LOCK
           WHERE buf_auth_detail.auth_obj                =  ipdAuthObj
             AND buf_auth_detail.auth_provider_obj       =  hat_auth_detail.auth_provider_obj
             AND buf_auth_detail.auth_detail_obj         <> dAuthDetailObjInFocus
             AND buf_auth_detail.related_entity_mnemonic =  "hatad":U
             AND buf_auth_detail.related_obj             =  dAuthDetailObjInFocus NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

        IF AVAILABLE buf_auth_detail
        THEN
          ASSIGN
            ttAuthRequestValues.instalment_requested      = buf_auth_detail.amount_requested
            ttAuthRequestValues.instalment_authorised     = buf_auth_detail.amount_auth
            ttAuthRequestValues.instalment_num_requested  = buf_auth_detail.quantity_requested
            ttAuthRequestValues.instalment_num_authorised = buf_auth_detail.quantity_auth.

        FIND FIRST hat_auth_provider NO-LOCK
             WHERE hat_auth_provider.auth_provider_obj  = hat_auth_detail.auth_provider_obj NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

        IF AVAILABLE hat_auth_provider
        THEN
          ASSIGN
            ttAuthRequestValues.total_amount_requested  = hat_auth_provider.amount_requested
            ttAuthRequestValues.total_amount_authorised = hat_auth_provider.amount_auth.
      END. /* IF dAuthDetailObjInFocus <> 0.00 */

  END. /* Else Do: */
  { mip/inc/mipcatcherror.i }
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAuthTariffRestriction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAuthTariffRestriction Procedure
PROCEDURE getAuthTariffRestriction :
/*------------------------------------------------------------------------------
  Purpose:  Check whether a Tariff Code has any Tariff Restrictions

  Notes  : Please view maauthservgetauthtrfrestr.i for further notes

------------------------------------------------------------------------------*/

{ ma/app/maauthservgetauthtrfrestr.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAuthTypeConfig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAuthTypeConfig Procedure
PROCEDURE getAuthTypeConfig :
/*------------------------------------------------------------------------------
  Purpose   : Get resolved auth type configuration for a specified authorisation type
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

 { ma/app/maauthservauthtypeconfig.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCopayFlags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCopayFlags Procedure
PROCEDURE getCopayFlags :
/*------------------------------------------------------------------------------
  Purpose: Determine the flags which will be applicable for the Copayment/penalty processing.
           The following flags, depending on auth type set up, could be linked to the main provider:
                 -PENALTY
                 -LATEAUTH
                 -EMERGENCY
                 -COPAY
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
  DEFINE        PARAMETER BUFFER btt_auth FOR tt_auth.
  DEFINE OUTPUT PARAMETER opcCopayFlagList AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lActivateCopayment AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lActivatePenalty   AS LOGICAL NO-UNDO.

  IF AVAILABLE btt_auth THEN
  DO:

    EMPTY TEMP-TABLE ttAuthTypeConfig.

    mipEnv:Health:AuthService:getAuthTypeConfig(BUFFER btt_auth ,
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig).

    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
 
    IF AVAILABLE ttAuthTypeConfig THEN
    DO:

      ASSIGN lActivateCopayment = ttAuthTypeConfig.ActivateCopayment
             lActivatePenalty   = ttAuthTypeConfig.ActivatePenalty .

             opcCopayFlagList   =  IF      lActivateCopayment AND lActivatePenalty     THEN "EMERGENCY,COPAY,LATEAUTH,PENALTY":U
                                   ELSE IF lActivateCopayment AND NOT lActivatePenalty THEN "EMERGENCY,COPAY":U
                                   ELSE IF NOT lActivateCopayment AND lActivatePenalty THEN "EMERGENCY,LATEAUTH,PENALTY":U
                                   ELSE "":U.

    END. // IF AVAILABLE ttAuthTypeConfig THEN

  END. //IF AVAILABLE btt_auth THEN

   {mip/inc/mipcatcherror.i
     &FINALLY="EMPTY TEMP-TABLE ttAuthTypeConfig." }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRateControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRateControl Procedure
PROCEDURE getRateControl :
/*------------------------------------------------------------------------------
  Purpose:  Check whether any Rate Control setups apply for the auth according to the
            setups on the Rate Control tables and the values captured on the auth.

  Notes  : Please view maauthservgetratecontrol.i for further notes

------------------------------------------------------------------------------*/

  { ma/app/maauthservgetratecontrol.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStatusDescription) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getStatusDescription Procedure
PROCEDURE getStatusDescription :
/*------------------------------------------------------------------------------
  Purpose:  Return the correct authorisation status description
  Notes  :  Valid parameter values:
            ipcStatusType -> "System" or "Auth"
            ipiStatusCode -> 0,1,2,4,5,6,7
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcStatusType  AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipiStatusCode  AS INTEGER   NO-UNDO.

  DEFINE OUTPUT PARAMETER opcDescription AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  EMPTY TEMP-TABLE ttAuthStatus.

  mipEnv:Health:authService:getStatusTable(INPUT ipcStatusType, OUTPUT TABLE ttAuthStatus).

  FIND FIRST ttAuthStatus NO-LOCK
       WHERE ttAuthStatus.status_code = ipiStatusCode
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE ttAuthStatus
  THEN
    ASSIGN opcDescription = TRIM(ttAuthStatus.status_description).
  ELSE
    ASSIGN opcDescription = "":U.

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStatusReasonDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getStatusReasonDesc Procedure
PROCEDURE getStatusReasonDesc :
/*----------------------------------------------------------------------------------------------------------------------------------------------
  Purpose: Get the status reason description
  Parameters:  ipdInsurerObj                   - Insurer Obj (optional - default 0 )
               ipiOption                       - Option code (optional - default 0 )
               ipdStartDate                    - Start date ( optional - will use TODAY if ?)
               ipiStatus                       - Status (required)
               ipcStatusReasonKey              - Status reason key (required)
               ipocReasonType                  - Reason type (optional) - if you don't pass this in, the NotePerAuthStatus rule value
                                                                          will be evaluated to determine the type and return it to the caller.
               ipolNotePerAuthStatusValidRule  - Rule value for NotePerAuthStatus rule( optional)- Pass ? if you want the service
                                                                                                   method to read the rule value .
               opcReasonDescription            - Reason description
               opcErrorMessage                 - Error message

  Notes: Check the NotePerAuthStatus rule first to determine if we are using only "AS" or "AS" + status
         as the reason type.
         Error will be returned in opcErrorMessage if a description couldn't be found
------------------------------------------------------------------------------------------------------------------------------------------------*/

  DEFINE INPUT        PARAMETER ipdInsurerObj                  AS DECIMAL   NO-UNDO.
  DEFINE INPUT        PARAMETER ipiOption                      AS INTEGER   NO-UNDO.
  DEFINE INPUT        PARAMETER ipdStartDate                   AS DATE      NO-UNDO.
  DEFINE INPUT        PARAMETER ipiStatus                      AS INTEGER   NO-UNDO.
  DEFINE INPUT        PARAMETER ipcStatusReasonKey             AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipocStatusReasonType           AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipolNotePerAuthStatusValidRule AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT       PARAMETER opcReasonDescription           AS CHARACTER NO-UNDO.
  DEFINE OUTPUT       PARAMETER opcErrorMessage                AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lNotePerAuthStatusRule       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lCheckNotePerAuthStatusRule  AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE cNotePerAuthStatusRuleValue  AS CHARACTER NO-UNDO.

  /*
    Santize input parameters
  */
  ASSIGN ipdInsurerObj                  = IF ipdInsurerObj        = ?  THEN 0.00  ELSE ipdInsurerObj
         ipiOption                      = IF ipiOption            = ?  THEN 0     ELSE ipiOption
         ipdStartDate                   = IF ipdStartDate         = ?  THEN TODAY ELSE ipdStartDate
         ipiStatus                      = IF ipiStatus            = ?  THEN 0     ELSE ipiStatus
         ipcStatusReasonKey             = IF ipcStatusReasonKey   = ?  THEN "":U  ELSE ipcStatusReasonKey
         ipocStatusReasonType           = IF ipocStatusReasonType = ?  THEN "":U  ELSE ipocStatusReasonType
         lCheckNotePerAuthStatusRule    = ipolNotePerAuthStatusValidRule = ?
         ipolNotePerAuthStatusValidRule = IF ipolNotePerAuthStatusValidRule = ? THEN FALSE ELSE ipolNotePerAuthStatusValidRule .

  IF ipocStatusReasonType = "":U THEN
  DO:

    IF lCheckNotePerAuthStatusRule
    THEN
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                     INPUT  ipiOption,
                                                     INPUT  "ma_acAuthRuleTypeAUTHSETUPS":U,
                                                     INPUT  "NotePerAuthStatus":U,
                                                     INPUT  ipdStartDate,
                                                     OUTPUT lNotePerAuthStatusRule,
                                                     OUTPUT cNotePerAuthStatusRuleValue).
    ELSE
      ASSIGN lNotePerAuthStatusRule      = ipolNotePerAuthStatusValidRule
             cNotePerAuthStatusRuleValue = IF lNotePerAuthStatusRule THEN "TRUE":U ELSE "FALSE":U.

    ASSIGN ipocStatusReasonType  =  IF lNotePerAuthStatusRule AND CAN-DO("Y,YES,TRUE,T":U ,cNotePerAuthStatusRuleValue )
                                    THEN "AS":U + STRING(ipiStatus)
                                    ELSE "AS":U .

  END. /* IF ipcStatusReasonType = "" */

  IF ipcStatusReasonKey <> "" THEN
  DO:

    FIND FIRST note NO-LOCK
         WHERE note.type = ipocStatusReasonType
           AND note.key  = ipcStatusReasonKey NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  END. /* IF ipcStatusReasonKey <> "" */

  IF AVAILABLE note
  THEN
    ASSIGN opcReasonDescription = note.narration[1].
  ELSE
    ASSIGN opcErrorMessage = SUBSTITUTE("Status Reason Description not available for type: &1, key: &2":U,ipocStatusReasonType, ipcStatusReasonKey)
           opcErrorMessage = opcErrorMessage + "[HELP=Auth Rule Code: NotePerAuthStatus]":U.

  {mip/inc/mipcatcherror.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStatusTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getStatusTable Procedure
PROCEDURE getStatusTable :
/*------------------------------------------------------------------------------
  Purpose:     Create a temp-table with all the status codes and descriptions
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcStatusType  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER TABLE FOR ttAuthStatus.

  DEFINE VARIABLE cRuleValue AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lSuccess   AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule AS LOGICAL     NO-UNDO.

  EMPTY TEMP-TABLE ttAuthStatus.

  ASSIGN lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                    (INPUT  0,
                     INPUT  0,
                     INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                     INPUT  "ValidStatuses":U,
                     INPUT  TODAY,
                     OUTPUT lValidRule,
                     OUTPUT cRuleValue).
  /*
     If no rule exists, return an empty temp-table ttAuthStatus
  */
  IF NOT lValidRule
  THEN
    RETURN.

  DO TRANSACTION ON ERROR UNDO, THROW:

    IF LOOKUP("0-Pending",cRuleValue) > 0 THEN
    DO:
      CREATE ttAuthStatus.
      ASSIGN ttAuthStatus.status_code        = 0
             ttAuthStatus.status_description = "Pending".
    END. /* IF LOOKUP("0-Pending",cRuleValue) > 0 THEN */

    IF LOOKUP("1-Authorised":U,cRuleValue) > 0 THEN
    DO:
      CREATE ttAuthStatus.
      ASSIGN ttAuthStatus.status_code        = 1
             ttAuthStatus.status_description = "Authorised".
    END. /* IF LOOKUP("1-Authorised":U,cRuleValue) > 0 THEN */

    IF LOOKUP("2-Assessed":U,cRuleValue) > 0 THEN
    DO:
      CREATE ttAuthStatus.
      ASSIGN ttAuthStatus.status_code        = 2
             ttAuthStatus.status_description = IF ipcStatusType = "System"
                                             THEN "Assess"
                                             ELSE "Authorised".
    END. /* IF LOOKUP("2-Assessed":U,cRuleValue) > 0 THEN */

    IF LOOKUP("4-Complete":U,cRuleValue) > 0 THEN
    DO:
      CREATE ttAuthStatus.
      ASSIGN ttAuthStatus.status_code        = 4
             ttAuthStatus.status_description = IF ipcStatusType = "System"
                                             THEN "Complete"
                                             ELSE "Authorised".
    END. /* IF LOOKUP("4-Complete":U,cRuleValue) > 0 THEN */

    IF LOOKUP("5-Cancelled":U,cRuleValue) > 0 THEN
    DO:
      CREATE ttAuthStatus.
      ASSIGN ttAuthStatus.status_code        = 5
             ttAuthStatus.status_description = "Cancelled".
    END. /* IF LOOKUP("5-Cancelled":U,cRuleValue) > 0 THEN */

    IF LOOKUP("6-Declined":U,cRuleValue) > 0 THEN
    DO:
      CREATE ttAuthStatus.
      ASSIGN ttAuthStatus.status_code        = 6
             ttAuthStatus.status_description = "Declined".
    END. /* IF LOOKUP("6-Declined":U,cRuleValue) > 0 THEN */

    IF LOOKUP("7-Requested":U,cRuleValue) > 0 THEN
    DO:
      CREATE ttAuthStatus.
      ASSIGN ttAuthStatus.status_code        = 7
             ttAuthStatus.status_description = "Requested".
    END. /* IF LOOKUP("7-Requested":U,cRuleValue) > 0 THEN */

  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-revertAuthRateConversion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE revertAuthRateConversion Procedure
PROCEDURE revertAuthRateConversion :
/*------------------------------------------------------------------------------
  Purpose: Revert auth rate conversions on detail lines
  Notes  : This routine will revert rate conversions on all auth detail
           line records in the dataset passed in. It will not save the converted
           detail lines to the db. It will only pass back the auth dataset in the state
           where all detail lines have been converted and will flag all updated detail
           lines with record action "MODIFY" so that further processing can
           be handled by the saveAuthorisation service.
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.
  DEFINE INPUT  PARAMETER ipdAuthObj                   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthRateControlObj        AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcCodeLinkCategory          AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcOverrideBaseRate          AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcOverrideArsRate           AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcRevertBaseRate            AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcRevertArsRate             AS CHARACTER NO-UNDO.

  DEFINE BUFFER btt_auth               FOR tt_auth.
  DEFINE BUFFER btt_auth_provider      FOR tt_auth_provider.
  DEFINE BUFFER bbt_auth_provider      FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail        FOR tt_auth_detail.
  DEFINE BUFFER buf_code_link          FOR hlm_code_link.
  DEFINE BUFFER btt_temp_auth_detail   FOR tt_temp_auth_detail.

  DEFINE VARIABLE lCodeLinkFound          AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lSuccess                AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lErrors                 AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE dCodeLinkChildObj       AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dDummyObj               AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dNewTariffLinkObj       AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj          AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffObj              AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj             AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE cAddValidations         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cAlertMessage           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cError                  AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cMessage                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarnDetailLineDeletion AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cPrType                 AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTLArsRate              AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTLBaseRate             AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTLPrType               AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTrackingMessage        AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject     NO-UNDO.

  /*
    Revert auth rate providers
  */
  RUN _revertAuthRateProviders IN TARGET-PROCEDURE (INPUT-OUTPUT DATASET dsAuthorisation ,
                                                    INPUT                ipdAuthObj,
                                                    INPUT                ipdAuthRateControlObj).
  EMPTY TEMP-TABLE tt_temp_auth_detail.

  FIND FIRST btt_auth
       WHERE btt_auth.auth_obj = ipdAuthObj NO-ERROR.


  FIND FIRST btt_auth_provider NO-LOCK
       WHERE btt_auth_provider.auth_obj      = ipdAuthObj
         AND btt_auth_provider.main_provider = TRUE NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  /*
    If there is no main provider , leave now
  */
  IF NOT AVAILABLE btt_auth_provider
  OR NOT AVAILABLE btt_auth
  THEN
    RETURN .

  FOR EACH btt_auth_detail
     WHERE btt_auth_detail.auth_obj       = ipdAuthObj
       AND btt_auth_detail.record_action <> "DELETE":U :

    ASSIGN lCodeLinkFound = FALSE .

    /*
      Try to find a code link record where the child entity matches the tariff link on our detail record
      and if the code link's parent applies , revert it to the parent tariff link
    */
    CODE-LINK-BLK:
    FOR EACH buf_code_link NO-LOCK
       WHERE buf_code_link.acronym_key      = ipcCodeLinkCategory
         AND buf_code_link.child_entity     = btt_auth_detail.owning_entity_mnemonic
         AND buf_code_link.child_entity_obj = btt_auth_detail.owning_obj
         AND buf_code_link.parent_entity    = btt_auth_detail.owning_entity_mnemonic
         AND buf_code_link.effective_date  <= btt_auth_detail.start_date :

      ASSIGN dTariffLinkObj = buf_code_link.parent_entity_obj .

      mipEnv:health:mamedical:getValidTariff( INPUT-OUTPUT dTariffLinkObj,                 // iopdTariffLinkObj
                                              INPUT        buf_code_link.parent_alt_value, // ipcTariffCode
                                              INPUT        ipcRevertBaseRate,              // ipcBaseRate
                                              INPUT        ipcRevertArsRate  ,             // ipcARSRate
                                              INPUT        btt_auth_provider.pr_type ,     // ipiPrType
                                              INPUT        btt_auth_provider.sub_pr_type,  // ipiSubPrType
                                              INPUT        btt_auth_detail.start_date,     // ipdDate
                                              INPUT        btt_auth.option_code,           // ipiOptionCode
                                              INPUT        cAddValidations,                // ipcAddValidations
                                              OUTPUT       dTariffObj,                     // opdTariffObj
                                              OUTPUT       dTrfCostObj,                    // opdTrfCostObj
                                              OUTPUT       cError,                         // opcError
                                              OUTPUT       cWarning,                       // opcWarning
                                              OUTPUT       cAlertMessage) NO-ERROR.

         IF cError = "" THEN
         DO:

            ASSIGN lCodeLinkFound = TRUE .

            FIND FIRST btt_temp_auth_detail
                 WHERE btt_temp_auth_detail.auth_provider_obj      = btt_auth_detail.auth_provider_obj
                   AND btt_temp_auth_detail.owning_entity_mnemonic = buf_code_link.child_entity
                   AND btt_temp_auth_detail.owning_obj             = dTariffLinkObj
                   AND btt_temp_auth_detail.owning_key             = buf_code_link.parent_alt_value
                   AND btt_temp_auth_detail.start_date             = btt_auth_detail.start_date NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF NOT AVAILABLE btt_temp_auth_detail
            THEN
              CREATE btt_temp_auth_detail.

            BUFFER-COPY btt_auth_detail EXCEPT btt_auth_detail.auth_detail_obj
                                               btt_auth_detail.owning_obj
                                               btt_auth_detail.owning_alt_value
                                               btt_auth_detail.loc_sequence
                   TO btt_temp_auth_detail.

          ASSIGN dDummyObj                              = dDummyObj - 1000
                 btt_temp_auth_detail.auth_detail_obj   = dDummyObj
                 btt_temp_auth_detail.owning_obj        = dTariffLinkObj
                 btt_temp_auth_detail.owning_alt_value  = buf_code_link.parent_alt_value
                 btt_temp_auth_detail.owning_key        = buf_code_link.parent_alt_value
                 btt_temp_auth_detail.record_action     = "MODIFY":U
                 btt_temp_auth_detail.loc_sequence      = btt_auth_detail.loc_sequence * 100

                 btt_auth_detail.record_action          = "DELETE":U.

         END. /* IF NOT cError  */


    END. // CODE-LINK-BLK

    /*
        If no code link was found , try to match the tariff code to the base and ars rates we are reverting to
    */

    IF NOT lCodeLinkFound THEN
    DO:

      ASSIGN dNewTariffLinkObj = 0 .

      mipEnv:health:mamedical:getValidTariff( INPUT-OUTPUT dNewTariffLinkObj,                // iopdTariffLinkObj
                                              INPUT        btt_auth_detail.owning_alt_value, // ipcTariffCode
                                              INPUT        ipcRevertBaseRate,                // ipcBaseRate
                                              INPUT        ipcRevertArsRate  ,                // ipcARSRate
                                              INPUT        btt_auth_provider.pr_type ,       // ipiPrType
                                              INPUT        btt_auth_provider.sub_pr_type,    // ipiSubPrType
                                              INPUT        btt_auth_detail.start_date,       // ipdDate
                                              INPUT        btt_auth.option_code,             // ipiOptionCode
                                              INPUT        cAddValidations,                  // ipcAddValidations
                                              OUTPUT       dTariffObj,                       // opdTariffObj
                                              OUTPUT       dTrfCostObj,                      // opdTrfCostObj
                                              OUTPUT       cError,                           // opcError
                                              OUTPUT       cWarning,                         // opcWarning
                                              OUTPUT       cAlertMessage).
      IF cError = "" THEN
      DO:

        ASSIGN btt_auth_detail.owning_obj    = dNewTariffLinkObj
               btt_auth_detail.record_action = "MODIFY":U
               btt_auth_detail.loc_sequence  = btt_auth_detail.loc_sequence * 100 .

      END. // IF cError = ""
      ELSE
        ASSIGN btt_auth_detail.record_action = "DELETE":U
               btt_auth_detail.rate_change   = TRUE
               cWarnDetailLineDeletion       = cWarnDetailLineDeletion + "," + btt_auth_detail.owning_alt_value.

    END. // IF NOT lCodeLinkFound
  END. //FOR EACH btt_auth_detail

  FOR EACH btt_temp_auth_detail :

    FIND FIRST btt_auth_detail
         WHERE btt_auth_detail.auth_provider_obj      = btt_temp_auth_detail.auth_provider_obj
           AND btt_auth_detail.owning_entity_mnemonic = btt_temp_auth_detail.owning_entity_mnemonic
           AND btt_auth_detail.owning_obj             = btt_temp_auth_detail.owning_obj
           AND btt_auth_detail.owning_key             = btt_temp_auth_detail.owning_key
           AND btt_auth_detail.start_date             = btt_temp_auth_detail.start_date
           NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_detail
    THEN
      CREATE btt_auth_detail.

    BUFFER-COPY btt_temp_auth_detail
             TO btt_auth_detail   .

  END.  //FOR EACH btt_temp_auth_detail

  /* Generate neccesary warnings and errors */
  IF cWarnDetailLineDeletion <> "" THEN
    ASSIGN
      oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE)
      cMessage      = "Detail lines for tariffs " + TRIM(cWarnDetailLineDeletion,",") + " could not be reverted, and consequently they were deleted."
      lSuccess      = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                            INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                            INPUT "":U,                 // ipcOwningEntityKey
                                            INPUT btt_auth.line_number, // ipiLineNumber
                                            INPUT cMessage,             // ipcMessageText
                                            INPUT "WAR":U) NO-ERROR.    // ipcMessageType

  {mip/inc/mipcatcherror.i
    &FINALLY="EMPTY TEMP-TABLE tt_temp_auth_detail .
              IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startAuthCopyJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startAuthCopyJob Procedure
PROCEDURE startAuthCopyJob :
/*------------------------------------------------------------------------------
  Purpose:   To start an Authorisation Copy Job in Job Manager
  Parameters:  input-output table tt_authcopy
  Notes:
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_authcopy.

&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE cUserID       AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cEmailAddress AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cTextMessage  AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE oNewJob       AS cls.mipjmjob  NO-UNDO.
  DEFINE VARIABLE cSchemeName   AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cInsurer      AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE lSuccess      AS LOGICAL       NO-UNDO.

  FIND FIRST jmm_template NO-LOCK
       WHERE jmm_template.template_code = "ma_AuthCopy"
    NO-ERROR.

  IF NOT AVAILABLE jmm_template
  THEN RETURN ERROR "Please set up job template ma_AuthCopy".
  ELSE DO:

    IF NOT VALID-OBJECT(mipEnv:JobManager) THEN
      RETURN ERROR  "Job Manager Service not started".

    FIND FIRST tt_authcopy EXCLUSIVE-LOCK
      NO-ERROR.
    {mip/inc/mipreturnerror.i}

    /* Create a clone of mipEnv:JobManager:jmJob */
    /* We can't use mipEnv:JobManager:jmJob directly because the current job will be in this object */
    ASSIGN
      oNewJob = CAST(mipEnv:JobManager:jmJob:cloneService(), "cls.mipjmjob":U) NO-ERROR.
    {mip/inc/mipreturnerror.i}

    /*Create the job object*/
    ASSIGN lSuccess = oNewJob:newJob(jmm_template.template_code) NO-ERROR.

    {mip/inc/mipreturnerror.i}

    /*Set all the required values*/
    ASSIGN
      oNewJob:JobRunAtDatetime   = NOW
      oNewJob:UserMnemonic       = mipEnv:miUser:UserMnemonic
      oNewJob:UserObj            = mipEnv:miUser:UserObj
      oNewJob:UserKey            = mipEnv:miUser:UserKey
      lSuccess                   = oNewJob:JobStatus:focusStatus(INPUT "Key":U, INPUT "jm_StJmtjoSched":U) /*i dont know if the second param is correct*/

      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "TypeOfCopy",        INPUT        tt_authcopy.copy_type)
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "AuthNumber",        INPUT STRING(tt_authcopy.auth_num))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyFromStartDate", INPUT STRING(tt_authcopy.from_start_date))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyFromEndDate",   INPUT STRING(tt_authcopy.from_end_date))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "OptionCode",        INPUT STRING(tt_authcopy.option_code))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "InsurerObj",        INPUT STRING(tt_authcopy.insurer_obj))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "AuthType",          INPUT        tt_authcopy.auth_type_list)
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyToStartDate",   INPUT STRING(tt_authcopy.to_start_date))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyToEndDate",     INPUT STRING(tt_authcopy.to_end_date))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyProviders",     INPUT STRING(tt_authcopy.provider_info))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyICDs",          INPUT STRING(tt_authcopy.coding_icd))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyCPTs",          INPUT STRING(tt_authcopy.coding_cpt))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyDetails",       INPUT STRING(tt_authcopy.clinical_details))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyFlags",         INPUT STRING(tt_authcopy.flag_info))
      lSuccess                   = oNewJob:JobRefs:setRefValue(INPUT "CopyReferences",    INPUT STRING(tt_authcopy.external_ref))
      lSuccess                   = oNewJob:saveJob()
      tt_authcopy.job_reference  = oNewJob:JobReference
      .

    ASSIGN
      cEmailAddress = mipEnv:miUtility:getPreference("cm_PrefServInfoSMTPFrom":U)
      cTextMessage  = "Good Day " + mipEnv:miUser:UserName + "."
                    + CHR(13)
                    + CHR(13)
                    + "An Authorisation copy job with the Job Reference:":U
                    + tt_authcopy.job_reference
                    + " was started."
                    + CHR(13)
                    + CHR(13)
                    + "Copy Type: ":U + tt_authcopy.copy_type + " Authorisation Copy"
                    .


    CASE tt_authcopy.copy_type:

      WHEN "Batch":U THEN
      DO:
        IF tt_authcopy.option_code > 0 THEN
        DO:
          FIND FIRST scheme  NO-LOCK
            WHERE scheme.scheme-code = tt_authcopy.option_code NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE scheme THEN
            RETURN ERROR "scheme code ~"" + STRING(tt_authcopy.option_code) + "~"not found.".
          ELSE
            ASSIGN cSchemeName   = scheme.short-name.
        END. /*IF tt_authcopy.option_code > 0 THEN*/
        ELSE
          ASSIGN cSchemeName = "None":U.

        IF tt_authcopy.insurer_obj > 0 THEN
          FIND erm_insurer NO-LOCK
            WHERE erm_insurer.insurer_obj = tt_authcopy.insurer_obj NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE erm_insurer THEN
          ASSIGN cInsurer = erm_insurer.insurer_label.
        ELSE
          ASSIGN cInsurer = "None".

        ASSIGN
          cTextMessage = cTextMessage
                       + CHR(13)
                       + "From Start Date: ":U + (IF tt_authcopy.from_start_date = ? THEN "" ELSE STRING(tt_authcopy.from_start_date))
                       + CHR(13)
                       + "From End Date: ":U + (IF tt_authcopy.from_end_date = ? THEN "" ELSE STRING(tt_authcopy.from_end_date))
                       + CHR(13)
                       + "Option: ":U + cSchemeName
                       + CHR(13)
                       + "Client: ":U + cInsurer
                       + CHR(13)
                       + "Authorisation Type(s): ":U + (IF tt_authcopy.auth_type_list <> "" THEN tt_authcopy.auth_type_list ELSE "None":U)
                       + CHR(13)
                       + "To Start Date: ":U + (IF tt_authcopy.to_start_date = ? THEN "" ELSE  STRING(tt_authcopy.to_start_date))
                       + CHR(13)
                       + "To End Date: ":U  + (IF tt_authcopy.to_end_date = ? THEN "" ELSE STRING(tt_authcopy.to_end_date))
                       + CHR(13)
                       + CHR(13)
                       + "Authorisation Information to copy:":U
                       + CHR(13)
                       + "Provider Information: ":U + STRING(tt_authcopy.provider_info, "Yes/No":U)
                       + CHR(13)
                       + "Coding Details-Diagnosis(ICD): ":U + STRING(tt_authcopy.coding_icd,"Yes/No":U)
                       + CHR(13)
                       + "Coding Details-Procedure(CPT): ":U + STRING(tt_authcopy.coding_cpt,"Yes/No":U)
                       + CHR(13)
                       + "Clinical Details: ":U + STRING(tt_authcopy.clinical_details,"Yes/No":U)
                       + CHR(13)
                       + "Flag Information: ":U + STRING(tt_authcopy.flag_info,"Yes/No":U)
                       + CHR(13)
                       + "External References List: ":U + STRING(tt_authcopy.external_ref,"Yes/No":U).
      END. /* WHEN "Batch":U THEN*/
      WHEN "Single":U THEN
      DO:
        ASSIGN
          cTextMessage = cTextMessage
                       + CHR(13)
                       + "From Authorisation Number: ":U + tt_authcopy.auth_num
                       + CHR(13)
                       + CHR(13)
                       + "Authorisatiion Information to copy:":U
                       + CHR(13)
                       + "Provider Information: ":U + STRING(tt_authcopy.provider_info, "Yes/No":U)
                       + CHR(13)
                       + "Coding Details-Diagnosis(ICD): ":U + STRING(tt_authcopy.coding_icd,"Yes/No":U)
                       + CHR(13)
                       + "Coding Details-Procedure(CPT): ":U + STRING(tt_authcopy.coding_cpt,"Yes/No":U)
                       + CHR(13)
                       + "Clinical Details: ":U + STRING(tt_authcopy.clinical_details,"Yes/No":U)
                       + CHR(13)
                       + "Flag Information: ":U + STRING(tt_authcopy.flag_info,"Yes/No":U)
                       + CHR(13)
                       + "External References List: ":U + STRING(tt_authcopy.external_ref,"Yes/No":U).
      END. /*WHEN "Single":U THEN*/

    END CASE. /* CASE tt_authcopy.copy_type */

    RUN maCommunication IN (mipEnv:Health:maUtility:maUtility)
      (INPUT "EMAIL":U,
       INPUT cEmailAddress,
       INPUT mipEnv:miUser:UserEmail,
       INPUT "":U,
       INPUT "":U,
       INPUT tt_authcopy.copy_type + " Authorisaton Copy Job Created: ":U + tt_authcopy.job_reference,
       INPUT cTextMessage,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U,
       INPUT "":U)
      NO-ERROR.

  END. /*IF NOT AVAILABLE jmm_template*/

  {mip/inc/mipcatcherror.i
      &FINALLY = "IF VALID-OBJECT(oNewJob)  THEN DELETE OBJECT oNewJob  NO-ERROR."
  }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-statusReasonMandatory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE statusReasonMandatory Procedure
PROCEDURE statusReasonMandatory :
/*------------------------------------------------------------------------------
  Purpose:     Returns TRUE if a status reason note is mandatory for the status
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipiStatusCode AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthDate   AS DATE        NO-UNDO.
  DEFINE OUTPUT PARAMETER oplMandatory  AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE dAuthDate             AS DATE        NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER   NO-UNDO.

  IF ipdAuthDate = ?
  THEN dAuthDate = TODAY.
  ELSE dAuthDate = ipdAuthDate.

&IF {&DBDFMA} >= 010195 &THEN

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAUTHSTATUS":U,
                                                 INPUT  "EnforceStatusNote":U,
                                                 INPUT  dAuthDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  IF lValidRule AND LOOKUP(STRING(ipiStatusCode),cRuleValue) > 0
  THEN ASSIGN oplMandatory = TRUE.
  ELSE ASSIGN oplMandatory = FALSE.

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateAuthorisedValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateAuthorisedValues Procedure
PROCEDURE validateAuthorisedValues :
/*------------------------------------------------------------------------------
  Purpose:    Validate that Amount_Auth and Amount_Quantity values have only
              been entered where they can be. If not, then return an error.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  {ma/app/maauthservvalauthvalues.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateAuthStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateAuthStatus Procedure
PROCEDURE validateAuthStatus :
/*------------------------------------------------------------------------------
  Purpose:     Validate the auth status and return an error if the auth status
               is not valid
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipiStatusCode AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcStatusType AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplValid      AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE cRuleValue  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cValidList  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cStatusCode AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iEntry      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lSuccess    AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule  AS LOGICAL     NO-UNDO.

  ASSIGN oplValid = FALSE.

  ASSIGN lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                        (INPUT  0,
                         INPUT  0,
                         INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                         INPUT  "ValidStatuses":U,
                         INPUT  TODAY,
                         OUTPUT lValidRule,
                         OUTPUT cRuleValue).
  IF NOT lValidRule
  THEN
    RETURN.

  /*
     The rule value will contain a list of valid new
     status codes with their descriptions.
     We need to build a list without the descriptions.
  */
  DO iEntry = 1 TO NUM-ENTRIES(cRuleValue):
    ASSIGN cStatusCode = TRIM(ENTRY(iEntry,cRuleValue))
           cStatusCode = SUBSTRING(cStatusCode,1,1)
           cValidList  = cValidList + ",":U + cStatusCode.
  END.  /* DO iEntry = 1 TO NUM-ENTRIES(cRuleValue): */

  ASSIGN cValidList = SUBSTRING(cValidList,2).

  IF LOOKUP(STRING(ipiStatusCode),cValidList) <> 0
  THEN
    ASSIGN oplValid = TRUE.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateAuthStatusUpdate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateAuthStatusUpdate Procedure
PROCEDURE validateAuthStatusUpdate :
/*------------------------------------------------------------------------------
  Purpose:     Validate the auth status when the record is updated (NOT a new
               record) and return an error if the auth status is not valid
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  {ma/app/maauthservvalauthstatupd.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateAuthTypeDisciplineExcl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateAuthTypeDisciplineExcl Procedure
PROCEDURE validateAuthTypeDisciplineExcl :
/*------------------------------------------------------------------------------
  Purpose   : Checks whether the specified discipline is valid for the
              specified auth type
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthTypeObj   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj    AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcAuthType      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcProviderType  AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcDiscipline    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthGroupObj  AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcSubDiscipline AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipdEffectiveDate AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER opcStatus        AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcStatusReason  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplValid         AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cDisciplineList          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProviderTypeIndicator   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dEffectiveDate           AS DATE      NO-UNDO.
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider.

  IF ipdAuthTypeObj <> 0.00 THEN
  DO:
    { ma/msc/madispad.i &discipline = ipcDiscipline }
    { ma/msc/madispad.i &discipline = ipcSubDiscipline &comment = "/* "}

    ASSIGN cProviderTypeIndicator = 'ma_acAuthProviderTypeIndicatorExcl'
           dEffectiveDate         = ipdEffectiveDate.

    {ma/msc/maauthtypeproviderread.i   &hac_auth_type_provider = buf_auth_type_provider
                                       &AuthTypeObj            = ipdAuthTypeObj
                                       &InsurerObj             = ipdInsurerObj
                                       &OptionCode             = ipiOptionCode
                                       &ProviderType           = ipcProviderType
                                       &AuthGroupObj           = ipdAuthGroupObj
                                       &ProviderTypeIndicator  = cProviderTypeIndicator
                                       &EffectiveDate          = dEffectiveDate
                                       &Lock                   = NO-LOCK}

    IF AVAILABLE buf_auth_type_provider AND buf_auth_type_provider.pr_type_valid_list <> "":U
    AND LOOKUP(ipcDiscipline + ipcSubDiscipline, buf_auth_type_provider.pr_type_valid_list) <= 0
    THEN DO:
      ASSIGN
        opcStatus        = buf_auth_type_provider.default_auth_status
        opcStatusReason  = buf_auth_type_provider.default_auth_status_note
        oplValid         = FALSE
        cDisciplineList  = buf_auth_type_provider.pr_type_valid_list
        opcMessage       = SUBSTITUTE("Discipline '&1' may not be used for Auth Type '&2'. Only the following disciplines may be used '&3'. The Provider
                            record has been change to cStatusDescription as per the Auth Type Provider setups.", ipcDiscipline, ipcAuthType, cDisciplineList).

    END. /* IF AVAILABLE buf_auth_type_provider THEN */

  END. /*IF ipdAuthTypeObj <> 0.00 THEN*/

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateBodyRegion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateBodyRegion Procedure
PROCEDURE validateBodyRegion :
/*------------------------------------------------------------------------------
  Purpose:    1. Validate the body region in the container
              2. Validate that the container body region matches the header body region.
                 The header body region is set up as a rule and the matching body regions
                 are set up in the rule value.
------------------------------------------------------------------------------*/
  DEFINE INPUT    PARAMETER ipdInsurerObj               AS DECIMAL    NO-UNDO.
  DEFINE INPUT    PARAMETER ipiOptionCode               AS INTEGER    NO-UNDO.
  DEFINE INPUT    PARAMETER ipcAuthHeaderBodyRegion     AS CHARACTER  NO-UNDO.
  DEFINE INPUT    PARAMETER ipdtAuthHeaderDate          AS DATE       NO-UNDO.
  DEFINE INPUT    PARAMETER ipcAuthContainerBodyRegion  AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT   PARAMETER oplValidBodyRegion          AS LOGICAL    NO-UNDO INITIAL TRUE.
  DEFINE OUTPUT   PARAMETER opcValidationErrorMessage   AS CHARACTER  NO-UNDO INITIAL "".

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cBodyRegionList  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lValidRule       AS LOGICAL    NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE cRuleValue       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValidRuleValues AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iList            AS INTEGER    NO-UNDO.

  DEFINE BUFFER buff_auth_rule FOR hac_auth_rule.
  DEFINE BUFFER buff_acronym   FOR mic_acronym.

  /*
    Validate the body region in the container
  */
  IF NOT CAN-FIND(FIRST buff_acronym
                  WHERE buff_acronym.acronym_code = ipcAuthContainerBodyRegion) THEN
  DO:
    ASSIGN oplValidBodyRegion        = FALSE
           opcValidationErrorMessage = SUBSTITUTE("The body region (&1) specified could not be found.[&HELP]",
                                                  ipcAuthContainerBodyRegion).
    RETURN.
  END.  // IF NOT CAN-FIND(FIRST buff_acronym

  /*
    Validate that the container body region matches the header body region.
    The header body region is set up as a rule and the matching body regions
    are set up in the rule value.
  */
  IF ipcAuthHeaderBodyRegion <> "":U THEN
  DO:
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                   INPUT  ipiOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeBodyRegion":U,
                                                   INPUT  ipcAuthHeaderBodyRegion,
                                                   INPUT  ipdtAuthHeaderDate,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).

    IF NOT lValidRule
    THEN
      RETURN.
    ELSE DO:
      ASSIGN cBodyRegionList = REPLACE(cRuleValue, "|", ",").

      IF NOT CAN-DO(cBodyRegionList, ipcAuthContainerBodyRegion) THEN
      DO:
        DO iList = 1 TO NUM-ENTRIES(cBodyRegionList):
          FIND FIRST buff_acronym NO-LOCK
            WHERE buff_acronym.acronym_code = ENTRY(iList, cBodyRegionList)
            NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

          IF AVAILABLE buff_acronym
          THEN
            ASSIGN cValidRuleValues = cValidRuleValues + " " + buff_acronym.acronym_label.
        END.  // DO iList = 1 TO NUM-ENTRIES(cBodyRegionList):

        ASSIGN oplValidBodyRegion = FALSE
               opcValidationErrorMessage = "Please ensure the line body region either matches the Authorisation Header "
                                         + "or is one of the following: " + cValidRuleValues
                                         + "[HELP=Auth Rule Code: [&RULE]":U + ipcAuthHeaderBodyRegion + "]":U.
      END.  // IF NOT CAN-DO(cBodyRegionList, ipcAuthContainerBodyRegion) THEN
      ELSE
        RETURN.

    END. // ELSE - IF NOT lValidRule
  END.  // IF ipcAuthHeaderBodyRegion <> "":U THEN

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateLineRestriction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateLineRestriction Procedure
PROCEDURE validateLineRestriction :
/*------------------------------------------------------------------------------
  Purpose:     Validate the auth line restriction and return FALSE
               if auth line restriction is not valid
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcLineRestriction AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplValid           AS LOGICAL   NO-UNDO.

  ASSIGN oplValid = TRUE.

  FIND FIRST mic_acronym NO-LOCK
       WHERE mic_acronym.category_key = "ma_acAuthLineRestriction":U
         AND mic_acronym.acronym_key  = ipcLineRestriction
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE mic_acronym THEN
    ASSIGN oplValid = FALSE.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateProviderDiscipline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateProviderDiscipline Procedure
PROCEDURE validateProviderDiscipline :
/*------------------------------------------------------------------------------
  Purpose   : Checks whether the specified provider is valid for the
              specified auth type if the auth type has a list of
              valid disciplines linked to it.
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcProvider      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcDiscipline    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcSubDiscipline AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipdEffectiveDate AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER oplValid         AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cDiscipline              AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProviderPrType          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProviderSubPrType       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lValidationError         AS LOGICAL   NO-UNDO INITIAL FALSE.

  ASSIGN cDiscipline        = mipEnv:Health:maDoctor:getProviderDiscipline( INPUT  INTEGER(ipcProvider),
                                                                            INPUT  ipdEffectiveDate,
                                                                            INPUT  "BOTH":U )
         cProviderPrType     = ENTRY(1,cDiscipline,"|":U)
         cProviderSubPrType  = ENTRY(2,cDiscipline,"|":U) NO-ERROR.

  {&ResetError}

  { ma/msc/madispad.i &discipline = ipcDiscipline }
  { ma/msc/madispad.i &discipline = ipcSubDiscipline &comment = "/* "}

  IF (ipcDiscipline <> cProviderPrType)
  THEN
    ASSIGN oplValid         = FALSE
           opcMessage       = "Invalid Discipline.The Discipline does not match the Provider Discipline":U
           lValidationError = TRUE.

  IF (ipcSubDiscipline <> cProviderSubPrType )
  THEN
    ASSIGN oplValid         = FALSE
           opcMessage       = "Invalid Sub Discipline.The Sub Discipline does not match the Provider Sub Discipline":U
           lValidationError = TRUE.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateProviderType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateProviderType Procedure
PROCEDURE validateProviderType :
/*------------------------------------------------------------------------------
  Purpose:     Validate the provider type
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcProviderType AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER lValid          AS LOGICAL   NO-UNDO.

  ASSIGN lValid = TRUE.

  FIND FIRST mic_acronym NO-LOCK
       WHERE mic_acronym.category_key = "ma_acAuthProviderType":U
         AND mic_acronym.acronym_key  = ipcProviderType
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE mic_acronym THEN
    ASSIGN lValid = FALSE.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateStatusReason) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateStatusReason Procedure
PROCEDURE validateStatusReason :
/*------------------------------------------------------------------------------
  Purpose:     Validate the authorisation status reason against the note table
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcStatusNote   AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipiStatusCode   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate         AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cNoteType    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cStatusDescr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lSuccess     AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule   AS LOGICAL     NO-UNDO.

  /*
    Check if status reasons are set up per auth status
  */
  ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                      (INPUT  ipdInsurerObj,
                       INPUT  ipiOptionCode,
                       INPUT  "ma_acAuthRuleTypeAuthSetups":U,
                       INPUT  "NotePerAuthStatus":U,
                       INPUT  ipdDate,
                       OUTPUT lValidRule,
                       OUTPUT cRuleValue).

  IF lValidRule AND LOOKUP(cRuleValue,"Y,YES,T,TRUE") <> 0
  THEN
    ASSIGN cNoteType = "AS":U + STRING(ipiStatusCode).
  ELSE
    ASSIGN cNoteType = "AS":U.

  FIND FIRST note NO-LOCK
       WHERE note.scheme-code = 00
       AND   note.type        = cNoteType
       AND   note.key         = ipcStatusNote
       NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE note
  THEN
    ASSIGN cStatusDescr    = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                            INPUT ipiStatusCode)
           opcErrorMessage = "Status reason (" + ipcStatusNote + ") for status '" + cStatusDescr + "' is invalid."
                           + "[HELP=Auth Rule Code: NotePerAuthStatus]":U.
  ELSE IF  note.expiry-date <> ?
       AND note.expiry-date < ipdDate
    THEN
      ASSIGN opcErrorMessage = "Status reason is no longer active."
                             + "[HELP=Auth Rule Code: NotePerAuthStatus]":U.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_applyAuthRateCoding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _applyAuthRateCoding Procedure
PROCEDURE _applyAuthRateCoding :
/*------------------------------------------------------------------------------
  Purpose: For day clinicscases the main provider needs to be determined
           according to the rvu setup
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation .
  DEFINE INPUT PARAMETER ipdAuthObj            AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipdAuthRateControlObj AS DECIMAL NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cCptList       AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cDriverCptList AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cMessage       AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cRuleValue     AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lMainCode      AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lPrimaryCode   AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE oErrorObject   AS cls.maerrorobject  NO-UNDO.

  DEFINE BUFFER buf_auth_rate_control FOR hac_auth_rate_control.
  DEFINE BUFFER buf_auth_rate_detail  FOR hac_auth_rate_detail.
  DEFINE BUFFER btt_auth_coding       FOR tt_auth_coding.
  DEFINE BUFFER btt_auth              FOR tt_auth.

  FIND FIRST btt_auth
       WHERE btt_auth.auth_obj = ipdAuthObj
    NO-ERROR.

  FIND FIRST buf_auth_rate_control NO-LOCK
       WHERE buf_auth_rate_control.auth_rate_control_obj = ipdAuthRateControlObj
    NO-ERROR.

  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE buf_auth_rate_control
  AND buf_auth_rate_control.cpt_relative_value_type <> "":U THEN
  DO:

    ASSIGN oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

    FOR EACH btt_auth_coding
       WHERE btt_auth_coding.owning_entity_mnemonic = "hlmck":U :

       FIND FIRST  buf_auth_rate_detail  NO-LOCK
            WHERE  buf_auth_rate_detail.auth_rate_control_obj  =  ipdAuthRateControlObj
              AND  buf_auth_rate_detail.owning_entity_mnemonic =  "hlmck":U
              AND  buf_auth_rate_detail.owning_alt_value       =  tt_auth_coding.owning_alt_value
              AND  buf_auth_rate_detail.effective_date         <= tt_auth_coding.start_date
              AND (buf_auth_rate_detail.end_date               >= tt_auth_coding.end_date
               OR  buf_auth_rate_detail.end_date                = ? )
         NO-ERROR.

       { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

       IF AVAILABLE buf_auth_rate_detail
       THEN
         ASSIGN cCptList = cCptList
                         + (IF cCptList <> "":U THEN "|":U ELSE "":U)
                         + btt_auth_coding.owning_alt_value + ",":U + buf_auth_rate_detail.override_ars_rate .

    END. // FOR EACH btt_auth_coding

    mipEnv:health:mamedical:getDriverCPT(INPUT cCptList,
                                         INPUT btt_auth.start_date,
                                         INPUT buf_auth_rate_control.cpt_relative_value_type ,
                                         OUTPUT cDriverCptList).

    IF NUM-ENTRIES(cDriverCptList, "|":U) > 1 THEN
    DO:
      ASSIGN cMessage  = "Could not determine a driver CPT, please review RVU setup.":U
             lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT btt_auth.line_number, // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "ERR":U).             // ipcMessageType

      RETURN.
    END. // IF NUM-ENTRIES(cDriverCptList, "|":U) > 1 THEN

    IF NUM-ENTRIES(cDriverCptList, "|":U) = 1 THEN
    DO:
      /*
        Check main/primary rules to determine whether we need to set main/primary or both
      */
      ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                                       INPUT  btt_auth.option_code,
                                                                       INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                                       INPUT  "CodingMainCode":U,
                                                                       INPUT  btt_auth.start_date,
                                                                       OUTPUT lMainCode,
                                                                       OUTPUT cRuleValue)

             lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                                       INPUT  btt_auth.option_code,
                                                                       INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                                       INPUT  "CPTPrimaryCode":U,
                                                                       INPUT  btt_auth.start_date,
                                                                       OUTPUT lPrimaryCode,
                                                                       OUTPUT cRuleValue).

      FIND FIRST btt_auth_coding
           WHERE btt_auth_coding.owning_alt_value = ENTRY(1, cDriverCptList)
        NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE btt_auth_coding THEN
      DO:
        /*
          Assign main/primary coding
        */
        ASSIGN btt_auth_coding.main_code    = IF lMainCode    THEN TRUE ELSE btt_auth_coding.main_code
               btt_auth_coding.primary_code = IF lPrimaryCode THEN TRUE ELSE btt_auth_coding.primary_code .

        /*
          Now we need to set any other cpts that are currently main/primary back to false since the driver cpt will now be main/primary
        */
        FOR EACH btt_auth_coding
           WHERE btt_auth_coding.auth_obj               =  ipdAuthObj
             AND btt_auth_coding.owning_entity_mnemonic =  "hlmck":U
             AND btt_auth_coding.owning_alt_value       <> ENTRY(1, cDriverCptList):

          IF  btt_auth_coding.main_code
          AND lMainCode
          THEN
            ASSIGN btt_auth_coding.main_code = FALSE
                   cMessage                  = SUBSTITUTE("CPT Code &1 is no longer the main CPT. &2 is now the main.":U +
                                                          "[HELP=Auth Rule Code = CodingMainCode]",
                                                          btt_auth_coding.owning_alt_value , ENTRY(1,cDriverCptList)) .
                   lSuccess                  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                                                     INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                                                     INPUT "":U,                 // ipcOwningEntityKey
                                                                     INPUT btt_auth.line_number, // ipiLineNumber
                                                                     INPUT cMessage,             // ipcMessageText
                                                                     INPUT "WAR":U,              // ipcMessageType
                                                                     INPUT TRUE).                // iplAcknowledge

          IF  btt_auth_coding.primary_code
          AND lPrimaryCode
          THEN
            ASSIGN btt_auth_coding.main_code = FALSE
                   cMessage                  = SUBSTITUTE("CPT Code &1 is no longer the Primary CPT. &2 is now the Primary.":U +
                                                          "[HELP=Auth Rule Code = CPTPrimaryCode]",
                                                          btt_auth_coding.owning_alt_value , ENTRY(1, cDriverCptList)) .
                   lSuccess                  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                                                     INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                                                     INPUT "":U,                 // ipcOwningEntityKey
                                                                     INPUT btt_auth.line_number, // ipiLineNumber
                                                                     INPUT cMessage,             // ipcMessageText
                                                                     INPUT "WAR":U,              // ipcMessageType
                                                                     INPUT TRUE).                // iplAcknowledge
        END. // FOR EACH btt_auth_coding
      END. // IF AVAILABLE btt_auth_coding THEN
    END. // IF NUM-ENTRIES(cDriverCptList, "|":U) = 1 THEN
  END. // IF AVAILABLE buf_auth_rate_control AND...

  {mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_applyAuthRateProviders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _applyAuthRateProviders Procedure
PROCEDURE _applyAuthRateProviders :
/*------------------------------------------------------------------------------
  Purpose:  There are provider defaults/overrides that need to be loaded on the auth.
            To apply these, we'll need to retrieve the auth rate control dataset
            so that we can access tt_auth_rate_provider
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation .
  DEFINE INPUT        PARAMETER ipdAuthObj            AS DECIMAL  NO-UNDO.
  DEFINE INPUT        PARAMETER ipdAuthRateControlObj AS DECIMAL  NO-UNDO.

  DEFINE BUFFER btt_auth               FOR tt_auth.
  DEFINE BUFFER btt_auth_provider      FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail        FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_rate_control  FOR tt_auth_rate_control.
  DEFINE BUFFER btt_auth_rate_provider FOR tt_auth_rate_provider.
  DEFINE BUFFER buf_tariff_type_link   FOR htm_tariff_type_link.
  DEFINE BUFFER buf_tariff_type        FOR htm_tariff_type.

  DEFINE VARIABLE lSuccess                AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE dDummyObj               AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj          AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffObj              AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj             AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE cAddValidations         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cArsRate                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cBaseRate               AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cAlertMessage           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cDefaultProviderList    AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cError                  AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cMessage                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cOverrideProviderList   AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cCreatedEntityList      AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cProviderType           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject     NO-UNDO.
  DEFINE VARIABLE oAuthRateControlSearch  AS cls.maauthratesearch  NO-UNDO.
  DEFINE VARIABLE oAcronymHelper          AS cls.mipacronym        NO-UNDO.

  DATASET dsAuthRateControl:EMPTY-DATASET().

  FIND FIRST btt_auth WHERE btt_auth.auth_obj = ipdAuthObj .

  ASSIGN oErrorObject           = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE)
         oAuthRateControlSearch = NEW cls.maauthratesearch(DATASET dsAuthRateControl BY-REFERENCE)
         lSuccess               =  oAuthRateControlSearch:SetCriteria("BufferList":U, "tt_auth_rate_control,tt_auth_rate_provider":U)
         lSuccess               =  oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.auth_rate_control_obj":U  , "=":U , ipdAuthRateControlObj).

  oAuthRateControlSearch:fetchData().

  FIND FIRST btt_auth_rate_control NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE btt_auth_rate_control THEN
  DO:
    ASSIGN dDummyObj             = -9999.99
           oAcronymHelper        = NEW cls.mipacronym(?, FALSE, "ma_acAuthProviderType":U, ?).

    RATE-PROVIDER-BLK:
    FOR EACH btt_auth_rate_provider
       WHERE btt_auth_rate_provider.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj
         AND btt_auth_rate_provider.effective_date       <= btt_auth.start_date
          BY btt_auth_rate_provider.auth_rate_indicator :

      /*******
        Handle Provider Overrides
      *******/
      IF btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorOverride":U THEN
      DO:
        FOR EACH btt_auth_provider
           WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
             AND btt_auth_provider.pr_type       = btt_auth_rate_provider.auth_rate_pr_type
             AND btt_auth_provider.sub_pr_type   = btt_auth_rate_provider.auth_rate_sub_pr_type
             AND btt_auth_provider.provider_type = btt_auth_rate_provider.auth_rate_provider_type :

            ASSIGN btt_auth_provider.auth_status             = INTEGER(btt_auth_rate_provider.override_auth_status)
                   btt_auth_provider.auth_status_note        = btt_auth_rate_provider.override_auth_status_note
                   btt_auth_provider.record_action           = IF btt_auth_provider.record_action <> "DELETE":U THEN "MODIFY":U ELSE "DELETE":U
                   btt_auth_provider.authorise_all_services  = btt_auth_rate_provider.authorise_all_services
                   btt_auth_provider.auth_group_obj          = btt_auth_rate_provider.auth_group_obj
                   btt_auth_provider.amount_auth             = btt_auth_rate_provider.provider_amount
                   btt_auth_provider.rate_change_type        = "ma_acAuthRateChangeTypeProviderOverride":U.

            VALIDATE btt_auth_rate_provider.

            oAcronymHelper:focusAcronym("KEY":U, btt_auth_provider.provider_type) NO-ERROR.

            ASSIGN cProviderType          = IF   oAcronymHelper:AcronymInFocus
                                            THEN oAcronymHelper:AcronymLabel
                                            ELSE btt_auth_provider.provider_type
                   cOverrideProviderList  = cOverrideProviderList
                                          + (IF cOverrideProviderList = "":U THEN "":U ELSE "^":U)
                                          + cProviderType   + ", ":U
                                          + "Disc : ":U     + STRING(btt_auth_rate_provider.auth_rate_pr_type )     + " & ":U
                                          + "Sub Disc : ":U + STRING(btt_auth_rate_provider.auth_rate_sub_pr_type ) .

        END. /* FOR EACH btt_auth_provider  */
      END. /* IF OVERRIDE  btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorOverride":U*/

      /*******
        Handle Provider Defaults
      ********/
      IF  btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorDefault":U THEN
      DO:

        FIND FIRST btt_auth_provider
             WHERE btt_auth_provider.auth_obj          = btt_auth.auth_obj
               AND btt_auth_provider.main_provider     = FALSE
               AND btt_auth_provider.pr_type           = btt_auth_rate_provider.auth_rate_pr_type
               AND btt_auth_provider.sub_pr_type       = btt_auth_rate_provider.auth_rate_sub_pr_type
               AND btt_auth_provider.provider_type     = btt_auth_rate_provider.auth_rate_provider_type
            NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE btt_auth_provider  THEN
        DO:
          CREATE btt_auth_provider.

          ASSIGN dDummyObj                           = dDummyObj - 1
                 btt_auth_provider.auth_obj          = btt_auth.auth_obj
                 btt_auth_provider.auth_provider_obj = dDummyObj.

          /*
            Only add the warning if the provider was created
          */
          oAcronymHelper:focusAcronym("KEY":U, btt_auth_rate_provider.auth_rate_provider_type) NO-ERROR .

          ASSIGN cProviderType                  = IF oAcronymHelper:AcronymInFocus THEN oAcronymHelper:AcronymLabel ELSE btt_auth_rate_provider.auth_rate_provider_type
                 cDefaultProviderList           = cDefaultProviderList
                                                + (IF cDefaultProviderList = "":U THEN "":U ELSE "^":U)
                                                +  cProviderType        + ", ":U
                                                + "Disc : ":U           + STRING(btt_auth_rate_provider.auth_rate_pr_type)      + " & ":U
                                                + "Sub Disc : ":U       + STRING(btt_auth_rate_provider.auth_rate_sub_pr_type ) .
        END. /* IF NOT AVAILABLE btt_auth_provider  THEN */

        ASSIGN btt_auth_provider.record_action          = IF btt_auth_provider.record_action <> "DELETE":U THEN "MODIFY":U ELSE "DELETE":U
               btt_auth_provider.provider_type          = btt_auth_rate_provider.auth_rate_provider_type
               btt_auth_provider.pr_type                = btt_auth_rate_provider.auth_rate_pr_type
               btt_auth_provider.sub_pr_type            = btt_auth_rate_provider.auth_rate_sub_pr_type
               btt_auth_provider.authorise_all_services = btt_auth_rate_provider.authorise_all_services
               btt_auth_provider.auth_group_obj         = btt_auth_rate_provider.auth_group_obj
               btt_auth_provider.amount_auth            = btt_auth_rate_provider.provider_amount
               btt_auth_provider.auth_status            = INTEGER(btt_auth_rate_provider.override_auth_status)
               btt_auth_provider.auth_status_note       = btt_auth_rate_provider.override_auth_status_note
               btt_auth_provider.start_date             = btt_auth.start_date
               btt_auth_provider.end_date               = btt_auth.end_date
               btt_auth_provider.start_ampm             = btt_auth.start_ampm
               btt_auth_provider.end_ampm               = btt_auth.end_ampm
               btt_auth_provider.claim_code             = btt_auth.claim_code
               btt_auth_provider.claim_type             = btt_auth.claim_type
               btt_auth_provider.rate_change_type       = "ma_acAuthRateChangeTypeProviderDefault":U
               btt_auth_provider.main_provider          = FALSE
               btt_auth_provider.los_calculation        = TRUE
               btt_auth_provider.authorised_service     = TRUE.

        VALIDATE btt_auth_rate_provider.

      END. /* IF DEFAULT (btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorDefault":U) */

      /*******
        Create Related Entities
      *******/
      IF  btt_auth_rate_provider.related_entity_mnemonic <> "":U
      AND btt_auth_rate_provider.related_alt_value       <> "":U
      AND btt_auth_rate_provider.related_obj              > 0 THEN
      DO:
        PROVIDER-ENTITY-CREATE-BLK:
        FOR EACH btt_auth_provider
           WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
             AND btt_auth_provider.pr_type       = btt_auth_rate_provider.auth_rate_pr_type
             AND btt_auth_provider.sub_pr_type   = btt_auth_rate_provider.auth_rate_sub_pr_type
             AND btt_auth_provider.provider_type = btt_auth_rate_provider.auth_rate_provider_type:

          ASSIGN cError = "":U.

          IF btt_auth_rate_provider.related_entity_mnemonic = "htmtl":U THEN
          DO:
            ASSIGN cBaseRate        = IF   btt_auth_provider.override_base_rate <> "":U
                                      THEN btt_auth_provider.override_base_rate
                                      ELSE btt_auth_provider.default_base_rate
                   cArsRate         = IF   btt_auth_provider.override_ars_rate  <> "":U
                                      THEN btt_auth_provider.override_ars_rate
                                      ELSE btt_auth_provider.default_ars_rate
                   cAddValidations  = "age,":U    + STRING(btt_auth._dependant_age_years) +
                                      "|gender,":U + btt_auth._dependant_gender           +
                                      "|bmi,":U    + STRING(btt_auth._dependant_bmi)
                   dTariffLinkObj   = 0.

            IF  cBaseRate = "":U
            AND cArsRate  = "":U
            THEN
              mipEnv:Health:maDoctor:getProviderBaseRates(INPUT  btt_auth_provider.doc_num,
                                                          INPUT  btt_auth.mem_num,
                                                          INPUT  btt_auth.option_code,
                                                          INPUT  btt_auth_provider.start_date,
                                                          OUTPUT cBaseRate ,
                                                          OUTPUT cArsRate ).

            mipEnv:health:mamedical:getValidTariff( INPUT-OUTPUT dTariffLinkObj ,                          // iopdTariffLinkObj
                                                    INPUT        btt_auth_rate_provider.related_alt_value, // ipcTariffCode
                                                    INPUT        cBaseRate,                                // ipcBaseRate
                                                    INPUT        cArsRate ,                                // ipcARSRate
                                                    INPUT        btt_auth_provider.pr_type ,               // ipiPrType
                                                    INPUT        btt_auth_provider.sub_pr_type,            // ipiSubPrType
                                                    INPUT        btt_auth_provider.start_date,             // ipdDate
                                                    INPUT        btt_auth.option_code,                     // ipiOptionCode
                                                    INPUT        cAddValidations,                          // ipcAddValidations
                                                    OUTPUT       dTariffObj,                               // opdTariffObj
                                                    OUTPUT       dTrfCostObj,                              // opdTrfCostObj
                                                    OUTPUT       cError,                                   // opcError
                                                    OUTPUT       cWarning,                                 // opcWarning
                                                    OUTPUT       cAlertMessage).                           // cAlertMessage

            /*
              Check if this is a LOC tariff, in which case we will not be creating the detail record
            */
            FOR EACH buf_tariff_type_link NO-LOCK
               WHERE buf_tariff_type_link.tariff_link_obj  = dTariffLinkObj
                 AND buf_tariff_type_link.effective_date  <= btt_auth_provider.start_date :

              FIND FIRST buf_tariff_type NO-LOCK
                   WHERE buf_tariff_type.tariff_type_obj =  buf_tariff_type_link.tariff_type_obj
                     AND buf_tariff_type.acronym_key     = "ma_acTariffTypeCatLOC":U NO-ERROR.

               {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

               IF AVAILABLE buf_tariff_type
               THEN
                 ASSIGN cError = SUBSTITUTE("Tariff Code &1 a LOC tariff.Auth Rate Provider setup does not cater for LOC tariffs!",btt_auth_rate_provider.related_alt_value).

            END. /* IF AVAILABLE buf_tariff_type_link THEN */
          END. /* FOR EACH buf_tariff_type_link NO-LOCK*/

          IF cError <> "":U THEN
          DO:
            ASSIGN cMessage  = SUBSTITUTE("Auth Rate Provider -  Detail Code : &1 Could not be Created. ":U , btt_auth_rate_provider.related_alt_value ) + cError
                   lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                                     INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                                     INPUT "":U,                 // ipcOwningEntityKey
                                                     INPUT btt_auth.line_number, // ipiLineNumber
                                                     INPUT cMessage,             // ipcMessageText
                                                     INPUT "WAR":U).             // ipcMessageType

          END.  // IF cError <> "" THEN
          ELSE DO:
            FIND FIRST btt_auth_detail
                 WHERE btt_auth_detail.auth_provider_obj      = btt_auth_provider.auth_provider_obj
                   AND btt_auth_detail.owning_entity_mnemonic = btt_auth_rate_provider.related_entity_mnemonic
                   AND btt_auth_detail.owning_obj             = btt_auth_rate_provider.related_obj
                   AND btt_auth_detail.owning_key             = btt_auth_rate_provider.related_key
                   AND (   (btt_auth_detail.start_date  = btt_auth_provider.start_date AND btt_auth_detail.start_ampm = btt_auth_provider.start_ampm)
                        OR (btt_auth_detail.end_date    = btt_auth_provider.end_date   AND btt_auth_detail.end_ampm   = btt_auth_provider.end_ampm) ) NO-ERROR.

            {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF NOT AVAILABLE btt_auth_detail THEN
            DO:
              CREATE btt_auth_detail.

              ASSIGN dDummyObj                              = dDummyObj - 1
                     btt_auth_detail.auth_obj               = btt_auth.auth_obj
                     btt_auth_detail.auth_detail_obj        = dDummyObj
                     btt_auth_detail.record_action          = "MODIFY":U
                     btt_auth_detail.auth_provider_obj      = btt_auth_provider.auth_provider_obj
                     btt_auth_detail.auth_status            = btt_auth.auth_status
                     btt_auth_detail.auth_status_note       = btt_auth.auth_status_note
                     btt_auth_detail.start_date             = btt_auth_provider.start_date
                     btt_auth_detail.start_ampm             = btt_auth_provider.start_ampm
                     btt_auth_detail.end_date               = btt_auth_provider.end_date
                     btt_auth_detail.end_ampm               = btt_auth_provider.end_ampm
                     btt_auth_detail.added_by_user          = NO
                     btt_auth_detail.line_restriction       = "ma_acAuthLineRestrictionNone":U
                     btt_auth_detail.claim_code             = btt_auth_provider.claim_code
                     btt_auth_detail.claim_type             = btt_auth_provider.claim_type
                     btt_auth_detail.owning_entity_mnemonic = btt_auth_rate_provider.related_entity_mnemonic
                     btt_auth_detail.owning_obj             = dTariffLinkObj
                     btt_auth_detail.owning_key             = btt_auth_rate_provider.related_key
                     btt_auth_detail.owning_alt_value       = btt_auth_rate_provider.related_alt_value
                     btt_auth_detail.auth_group_obj         = btt_auth_rate_provider.auth_group_obj
                     btt_auth_detail.discount_auth          = ?
                     btt_auth_detail.quantity_auth          = IF (((btt_auth_detail.quantity_auth   = 0 OR btt_auth_detail.quantity_auth = ?)
                                                              AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
                                                              AND (btt_auth_detail.related_entity_mnemonic = "hlmnl":U OR btt_auth_detail.related_entity_mnemonic = ""))
                                                              OR btt_auth_detail.owning_entity_mnemonic = "hlmnl":U)
                                                              THEN 1.00
                                                              ELSE IF ((btt_auth_detail.owning_entity_mnemonic = "htmtl" AND btt_auth_detail.related_entity_mnemonic = "hlmcr":U)
                                                              OR btt_auth_detail.owning_entity_mnemonic = "hlmcr":U)
                                                              THEN 0.00
                                                              ELSE btt_auth_detail.quantity_auth

                     cCreatedEntityList                     = cCreatedEntityList + (IF cCreatedEntityList = "":U THEN "":U ELSE "^":U)
                                                            + btt_auth_rate_provider.related_alt_value    + ",":U
                                                            + cBaseRate
                                                            + IF cArsRate <> "":U THEN ",":U + cArsRate ELSE "":U
                                                            + ",":U + STRING(btt_auth_provider.pr_type    )
                                                            + IF btt_auth_provider.sub_pr_type <> 0 THEN  ",":U + STRING(btt_auth_provider.sub_pr_type) ELSE "".

            END. /*  IF NOT AVAILABLE btt_auth_detail THEN */
          END. /* ELSE DO - IF cError <> "" */
        END. /* PROVIDER-ENTITY-CREATE-BLK */
      END. /* IF btt_auth_rate_provider.related_entity_mnemonic <> "":U AND btt_auth_rate_provider.related_alt_value <> "":U  AND btt_auth_rate_provider.related_entity_obj > 0 */
    END. /* RATE-PROVIDER-BLK: */

    /*
      Now generate warnings for defaulted/overrided providers
    */
    IF cOverrideProviderList <> "":U
    THEN
      ASSIGN cMessage  = "Existing provider record(s) were overrided by auth rate control setup: ":U
                       + SUBSTITUTE("[notificationlist=&1]", cOverrideProviderList)
             lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT btt_auth.line_number, // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "WAR":U).             // ipcMessageType
    IF cDefaultProviderList <> "":U
    THEN
      ASSIGN cMessage  = "The following default provider records were created from rate control setup : "
                       + SUBSTITUTE("[notificationlist=&1]", cDefaultProviderList)
             lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT btt_auth.line_number, // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "WAR":U).             // ipcMessageType
    IF cCreatedEntityList <> "":U
    THEN
      ASSIGN cMessage  = "The following auth detail records were created from rate control setup : "
                       + SUBSTITUTE("[notificationlist=&1]", cCreatedEntityList)
             lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT btt_auth.line_number, // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "WAR":U).             // ipcMessageType
  END. /* IF AVAILABLE tt_auth_rate_control */

  {mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oAuthRateControlSearch) THEN DELETE OBJECT oAuthRateControlSearch.
                IF VALID-OBJECT(oAcronymHelper)         THEN DELETE OBJECT oAcronymHelper.
                IF VALID-OBJECT(oErrorObject)           THEN DELETE OBJECT oErrorObject.
                DATASET dsAuthRateControl:EMPTY-DATASET()." }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_calcLOSDayCase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _calcLOSDayCase Procedure
PROCEDURE _calcLOSDayCase PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Day Case LOS Calculation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  {ma/app/maauthservcalclosdaycase.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_calcLOSFeeForService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _calcLOSFeeForService Procedure
PROCEDURE _calcLOSFeeForService PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Fee For Service LOS Calculation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  {ma/app/maauthservcalcfeeforservice.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_calcLOSPerDiem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _calcLOSPerDiem Procedure
PROCEDURE _calcLOSPerDiem PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Per Diem LOS Calculation
  Parameters:
  Notes     :

------------------------------------------------------------------------------*/
  {ma/app/maauthservcalclosperdiem.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_calcLOSPerDiemFixedFee) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _calcLOSPerDiemFixedFee Procedure
PROCEDURE _calcLOSPerDiemFixedFee PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Fixed Fee Per Diem LOS Calculation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  {ma/app/maauthservcalcperdiemfixed.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_calcLOSPerDiemMat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _calcLOSPerDiemMat Procedure
PROCEDURE _calcLOSPerDiemMat PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Maternity Per Diem LOS Calculation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  {ma/app/maauthservcalcperdiemmat.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_calcLOSPerDiemPVT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _calcLOSPerDiemPVT Procedure
PROCEDURE _calcLOSPerDiemPVT PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Private Ward Per Diem LOS Calculation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  {ma/app/maauthservcalcperdiempvt.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_copyAuthRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _copyAuthRecord Procedure
PROCEDURE _copyAuthRecord :
/*------------------------------------------------------------------------------
  Purpose:  1. Retrieve the auth dataset for the single copy-from auth with the auth obj provided
            2. Modify the auth dataset to the copy-to criteria using tt_authcopy
            3. Save auth dataset with service(btt_auth,btt_auth_provider,btt_auth_coding,btt_auth_detail,btt_auth_crosswalk)
            4. Save auth flags(If specified in copy)
            5. Save external references(If specified in copy)
            6. Return new auth number/Return validation errors

  Parameters:  INPUT        ipdAuthObj
               INPUT  TABLE tt_authcopy
               OUTPUT       opcNewAuthNum
               OUTPUT TABLE tt_auth_error

  Notes:  For now this method will only process a single auth at a time .
          TODO: An enhnancement can be made to build up the entire auth
          dataset and only make the service call once , rather than calling
          the service auth by auth .
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthObj              AS DECIMAL        NO-UNDO.
  DEFINE INPUT  PARAMETER TABLE FOR tt_authcopy.
  DEFINE OUTPUT PARAMETER opcNewAuthNum           AS CHARACTER      NO-UNDO.
  DEFINE OUTPUT PARAMETER TABLE FOR tt_auth_error.

  DEFINE VARIABLE lSuccess                 AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidRule               AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cArsRate                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBaseRate                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE opcCopayFlagList         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDefaultCopyStatusReason AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cMessage                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cStatusRuleValue         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBufferList              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dCopyFromAuthObj         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCopyToAuthObj           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dDummyObj                AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dNewAuthObj              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE iDefaultCopyStatus       AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iCount                   AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iDetailCount             AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iProviderCount           AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE oExternalRefSearch       AS cls.maexternalrefsearch   NO-UNDO.
  DEFINE VARIABLE oFVSearch                AS cls.maauthflagvaluesearch NO-UNDO.
  DEFINE VARIABLE oAuthRuleSearch          AS cls.maauthrulesearch      NO-UNDO.
  DEFINE VARIABLE oAuthSearch              AS cls.maauthsearch          NO-UNDO.
  DEFINE VARIABLE oErrorObject             AS cls.maerrorobject         NO-UNDO.

  DEFINE BUFFER btt_authcopy              FOR tt_authcopy.
  DEFINE BUFFER btt_auth                  FOR tt_auth.
  DEFINE BUFFER btt_auth_provider         FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_coding           FOR tt_auth_coding.
  DEFINE BUFFER btt_auth_detail           FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_rule             FOR tt_auth_rule.
  DEFINE BUFFER btt_auth_flag_value       FOR tt_auth_flag_value.
  DEFINE BUFFER btt_temp_auth_flag_value  FOR tt_temp_auth_flag_value.
  DEFINE BUFFER btt_extref                FOR tt_extref.
  DEFINE BUFFER btt_auth_crosswalk        FOR tt_auth_crosswalk.

  DATASET dsAuthorisation:EMPTY-DATASET() .

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  FIND FIRST btt_authcopy NO-ERROR.

  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  // This should never happen
  IF NOT AVAILABLE btt_authcopy THEN
  DO:
    ASSIGN cMessage =   SUBSTITUTE("The Auth copy criteria could not be found for Auth(&1)" , STRING(ipdAuthObj))
           lSuccess = oErrorObject:addError(INPUT 'hatau',     // ipcOwningEntityMnemonic
                                            INPUT ipdAuthObj,  // ipdOwningEntityObj
                                            INPUT "":U,        // ipcOwningEntityKey
                                            INPUT 0,           // ipiLineNumber
                                            INPUT cMessage,    // ipcMessageText
                                            INPUT "ERR":U,     // ipcMessageType
                                            INPUT TRUE).       // iplAcknowledge
    RETURN.
  END. // IF NOT AVAILABLE btt_auth

  //Retrieve the copy-from auth record
  ASSIGN oAuthSearch  = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE)
         cBufferList  = "tt_auth,tt_auth_crosswalk":U
                      + (IF btt_authcopy.provider_info                         THEN ",tt_auth_provider":U ELSE "":U)
                      + (IF btt_authcopy.coding_icd OR btt_authcopy.coding_cpt THEN ",tt_auth_coding":U   ELSE "":U)
                      + (IF btt_authcopy.clinical_details                      THEN ",tt_auth_detail":U   ELSE "":U)
         lSuccess     = oAuthSearch:setCriteria("BufferList":U, cBufferList)
         lSuccess     = oAuthSearch:setFilterCriteria("tt_auth.auth_obj" , "=":U , ipdAuthObj).

  oAuthSearch:fetchData().

  FIND FIRST btt_auth
       WHERE btt_auth.auth_obj = ipdAuthObj NO-ERROR.

  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE btt_auth THEN
  DO:
    ASSIGN cMessage = SUBSTITUTE("The Auth with obj &1 could not be found and was skipped", STRING(ipdAuthObj))

           lSuccess = oErrorObject:addError(INPUT 'hatau',     // ipcOwningEntityMnemonic
                                            INPUT ipdAuthObj,  // ipdOwningEntityObj
                                            INPUT "":U,        // ipcOwningEntityKey
                                            INPUT 0,           // ipiLineNumber
                                            INPUT cMessage,    // ipcMessageText
                                            INPUT "ERR":U,     // ipcMessageType
                                            INPUT TRUE).       // iplAcknowledge
    RETURN.
  END. // IF NOT AVAILABLE btt_auth

  //Find the auth copy default status rule
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_authcopy.insurer_obj,
                                                 INPUT  btt_authcopy.option_code,
                                                 INPUT  "ma_acAuthRuleTypeAuthCopy":U,
                                                 INPUT  "AuthCopyStatusDefault":U,
                                                 INPUT  btt_authcopy.to_end_date,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cStatusRuleValue).


  ASSIGN iDefaultCopyStatus       = INTEGER(ENTRY(1,cStatusRuleValue, "|":U) )
         cDefaultCopyStatusReason =         ENTRY(2,cStatusRuleValue, "|":U) NO-ERROR.

IF {&ErrorStatus}
  OR NOT lValidRule THEN
  DO:
    ASSIGN cMessage = "The AuthCopyStatusDefault rule setup is invalid.":U
           lSuccess = oErrorObject:addError(INPUT 'hatau',     // ipcOwningEntityMnemonic
                                            INPUT ipdAuthObj,  // ipdOwningEntityObj
                                            INPUT "":U,        // ipcOwningEntityKey
                                            INPUT 0,           // ipiLineNumber
                                            INPUT cMessage,    // ipcMessageText
                                            INPUT "ERR":U,     // ipcMessageType
                                            INPUT TRUE).       // iplAcknowledge

    RETURN.

  END. //IF {&ErrorStatus} OR NOT lValidRule

  mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).

  FIND FIRST ttAuthTypeConfig NO-ERROR.

  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE ttAuthTypeConfig THEN
  DO:
    ASSIGN cMessage = SUBSTITUTE("The Auth Type with obj &1 could not be found and was skipped", STRING(btt_auth.auth_type_obj))
           lSuccess = oErrorObject:addError(INPUT 'hatau',     // ipcOwningEntityMnemonic
                                            INPUT ipdAuthObj,  // ipdOwningEntityObj
                                            INPUT "":U,        // ipcOwningEntityKey
                                            INPUT 0,           // ipiLineNumber
                                            INPUT cMessage,    // ipcMessageText
                                            INPUT "ERR":U,     // ipcMessageType
                                            INPUT TRUE).       // iplAcknowledge
    RETURN.
  END. // IF NOT AVAILABLE ttAuthTypeConfig

  //Now let's make the copy-to version of the copy-from auth tt record
  ASSIGN dCopyFromAuthObj          = btt_auth.auth_obj
        dCopyToAuthObj            = -1
         btt_auth.auth_obj         = dCopyToAuthObj
         btt_auth.auth_num         = "":U
         btt_auth.record_action    = "MODIFY":U
         btt_auth.request_date     = TODAY
         btt_auth.start_date       = IF btt_authcopy.to_start_date <> ? THEN btt_authcopy.to_start_date
                                                                         ELSE btt_authcopy.from_end_date + 1
         btt_auth.start_ampm       = TRUE
         btt_auth.end_date         = btt_authcopy.to_end_date
         btt_auth.end_ampm         = TRUE
         btt_auth.auth_status      = iDefaultCopyStatus
         btt_auth.auth_status_note = cDefaultCopyStatusReason
         btt_auth.user_id          = STRING(mipEnv:miUser:UserObj)
         btt_auth.auth_incomplete  = TRUE           .
         //btt_auth._data_load       = YES .

  // clear auth values we don't intend on copying over
  fnResetAuthValues(BUFFER btt_auth ,
                    BUFFER ttAuthTypeConfig).

  VALIDATE btt_auth.

  IF btt_authcopy.provider_info THEN
  DO:

    //NB assign counters to high numbers - we do this because there are also auth detail and provider tt records that could be created in the service and
    //the obj counters in the service will start at 1 . So we do this to prevent potential duplicates
    ASSIGN iProviderCount = 9999
           iDetailCount   = 9999 .

    PROVIDER-BLK:
    FOR EACH btt_auth_provider
       WHERE btt_auth_provider.auth_obj = dCopyFromAuthObj :

      ASSIGN iProviderCount                      = iProviderCount + 1
             btt_auth_provider.record_action     = "MODIFY":U
             btt_auth_provider.auth_obj          = dCopyToAuthObj
             btt_auth_provider.auth_status       = iDefaultCopyStatus
             btt_auth_provider.auth_status_note  = cDefaultCopyStatusReason
             btt_auth_provider.end_ampm          = btt_auth.end_ampm
             btt_auth_provider.end_date          = btt_auth.end_date
             btt_auth_provider.start_ampm        = btt_auth.start_ampm
             btt_auth_provider.start_date        = btt_auth.start_date .

      //Clear the values on the provider that we don't intend on copying over
      fnResetAuthProviderValues(BUFFER btt_auth_provider,
                                BUFFER ttAuthTypeConfig).

      IF btt_authcopy.clinical_details THEN
      DO:
        //Create all auth detail copy-to records. Do this before changing the auth_provider_obj
        AUTH-DETAIL-BLK:
        FOR EACH btt_auth_detail
           WHERE btt_auth_detail.auth_obj          = dCopyFromAuthObj
             AND btt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj :

           mipEnv:Health:maDoctor:getProviderBaseRates(INPUT btt_auth_provider.doc_num,
                                                       INPUT btt_auth.mem_num,
                                                       INPUT btt_auth.option_code,
                                                       INPUT btt_auth_provider.start_date,
                                                       OUTPUT cBaseRate,
                                                       OUTPUT cArsRate).

            ASSIGN iDetailCount                      = iDetailCount + 1
                   btt_auth_detail.auth_obj          = dCopyToAuthObj
                   btt_auth_detail.auth_provider_obj = iProviderCount * -1
                   btt_auth_detail.auth_detail_obj   = iDetailCount   * -1
                   btt_auth_detail.record_action     = "MODIFY":U
                   btt_auth_detail.start_date        = btt_auth_provider.start_date
                   btt_auth_detail.end_date          = btt_auth_provider.end_date
                   btt_auth_detail.start_ampm        = btt_auth_provider.start_ampm
                   btt_auth_detail.end_ampm          = btt_auth_provider.end_ampm
                   btt_auth_detail.auth_status       = btt_auth_provider.auth_status
                   btt_auth_detail.auth_status_note  = btt_auth_provider.auth_status_note
                   btt_auth_detail.default_base_rate = cBaseRate
                   btt_auth_detail.default_ars_rate  = cArsRate .

            //Clear the detail values we don't intend on copying over
            fnResetAuthDetailValues(BUFFER btt_auth_detail,
                                    BUFFER ttAuthTypeConfig) .

            VALIDATE btt_auth_detail.

        END. //AUTH-DETAIL-BLK
      END. // btt_authcopy.clinical_details

      // Now that we've handled all the children we can re-assign the obj to a dummy
      ASSIGN btt_auth_provider.auth_provider_obj = iProviderCount * -1 .

      VALIDATE btt_auth_provider.

    END. //PROVIDER-BLK

  END. // IF btt_authcopy.provider_info
  //Create copy-to providers from the copy-from providers


  // Create copy-to auth coding
  ASSIGN iCount = 0.

  AUTH-CODING-BLK:
  FOR EACH btt_auth_coding
     WHERE btt_auth_coding.auth_obj = dCopyFromAuthObj :

      IF (btt_auth_coding.owning_entity_mnemonic = "diagnos":U AND btt_authcopy.coding_icd )
      OR (btt_auth_coding.owning_entity_mnemonic = "hlmck":U   AND btt_authcopy.coding_cpt ) THEN
      DO:
        ASSIGN iCount                             = iCount + 1
               btt_auth_coding.auth_obj           = dCopyToAuthObj
               btt_auth_coding.auth_coding_obj    = iCount * -1
               btt_auth_coding.record_action      = "MODIFY":U
               btt_auth_coding.coding_status      = iDefaultCopyStatus
               btt_auth_coding.coding_status_note = cDefaultCopyStatusReason
               btt_auth_coding.start_date         = btt_auth.start_date
               btt_auth_coding.end_date           = btt_auth.end_date.

         VALIDATE btt_auth_coding.

      END. //IF (btt_auth_coding.owning_entity_mnemonic = "diagnos":U AND btt_authcopy.coding_icd ) OR (btt_auth_coding.owning_entity_mnemonic = "hlmck":U
      ELSE
        DELETE btt_auth_coding.

  END. //AUTH-CODING-BLK

  //Create copy-to auth crosswalks
  ASSIGN iCount = 0 .

  AUTH-CROSSWALK-BLK:
  FOR EACH btt_auth_crosswalk:

    ASSIGN iCount                                = iCount + 1
           btt_auth_crosswalk.auth_obj           = dCopyToAuthObj
           btt_auth_crosswalk.auth_crosswalk_obj = iCount * -1
           btt_auth_crosswalk.record_action      = "MODIFY":U.

    VALIDATE btt_auth_crosswalk.

  END. // AUTH-CROSSWALK-BLK

  // Now save the "copy to" auth dataset
  mipEnv:Health:AuthBusinessLogic:SaveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE) NO-ERROR.

  //Re-focus auth buffer
  FIND FIRST btt_auth NO-ERROR .


  //Check if we don't have any errors from the auth service before we continue with saving the flags and external references
  IF CAN-FIND(FIRST tt_auth_error
              WHERE tt_auth_error.error_type = "ERR")
  OR NOT AVAILABLE btt_auth
  THEN
    RETURN.

  // The save was succesful , now that we have the new obj and num assign them to variables for further use
  ASSIGN dNewAuthObj   = btt_auth.auth_obj
         opcNewAuthNum = btt_auth.auth_num .

  //Copy auth flags
  IF btt_authcopy.flag_info = TRUE THEN
  DO:

    //Determine the flags will be applicable for the "Copy To" period
    DATASET dsAuthRule:EMPTY-DATASET().
    DATASET dsAuthFlagValue:EMPTY-DATASET().

    ASSIGN oAuthRuleSearch = NEW cls.maauthrulesearch(DATASET dsAuthRule BY-REFERENCE)

           //Set criteria for default rules
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, btt_auth.insurer_obj)
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, 0.00)
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, btt_auth.option_code)
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, 0)
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.rule_type":U     ,  "=":U, "ma_acAuthRuleTypeAuthFlagDef":U)
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.effective_date":U, "<=":U, btt_auth.start_date)
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.rule_code":U     ,  "=":U, "ALL":U)
           lSuccess   = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.rule_code":U     ,  "=":U, ttAuthTypeConfig.AuthTypeGroupLabel).

    // Fetch default rules
    oAuthRuleSearch:fetchData().

    // Remove all filter criteria from step 1 so we can re-search using the records
    // we found above to apply filter criteria.
    oAuthRuleSearch:removeFilterCriteria().

    ASSIGN lSuccess = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, btt_auth.insurer_obj)
           lSuccess = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, 0.00)
           lSuccess = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, btt_auth.option_code)
           lSuccess = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, 0)
           lSuccess = oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.effective_date":U, "<=":U, btt_auth.start_date).

    SET-RULE-CRITERIA-BLK:
    FOR EACH btt_auth_rule NO-LOCK:

      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.link_auth_rule_obj":U, "=":U, btt_auth_rule.auth_rule_obj).

    END. //SET-RULE-CRITERIA-BLK:

    //Now that we are done with results from step 1 which were used to apply filter criteria,
    //we can empty the dataset and refetch with the new filter criteria
    DATASET dsAuthRule:EMPTY-DATASET().

    //Fetch the auth rules again
    oAuthRuleSearch:fetchData().

    // Get the copay flags , because we want to know which ones to exclude when we are copying
    mipEnv:Health:AuthService:getCopayFlags(BUFFER btt_auth,
                                            OUTPUT opcCopayFlagList) .

    // Fetch the flags on the original copy from auth
    ASSIGN oFVSearch  = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE)
           lSuccess   = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
           lSuccess   = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, dCopyFromAuthObj)
           lSuccess   = oFVSearch:fetchData().

    // Copy the from flags in to a temporary handle so we can access it after we've fetched to "copy to" flags
    COPY-FROM-AUTH-FLAG-BLK:
    FOR EACH btt_auth_flag_value
       WHERE btt_auth_flag_value.owning_obj = dCopyFromAuthObj:

      FIND FIRST btt_auth_rule
           WHERE btt_auth_rule.auth_rule_obj = btt_auth_flag_value.auth_rule_obj NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE btt_auth_rule
      THEN
        NEXT COPY-FROM-AUTH-FLAG-BLK.

      // If the auth rule is a copay rule , put it in to a temporary buffer because once the auth has saved we will
      // need to reference the old flag dataset to be able to copy the values to the new auth
      IF LOOKUP(btt_auth_rule.rule_code , opcCopayFlagList ) = 0  THEN
      DO:
        CREATE btt_temp_auth_flag_value.

        BUFFER-COPY btt_auth_flag_value
                 TO btt_temp_auth_flag_value.

        VALIDATE btt_temp_auth_flag_value.
      END. //IF LOOKUP(btt_auth_rule.rule_code , opcCopayFlagList ) = 0  THEN
    END. //COPY-FROM-AUTH-FLAG-BLK:

    // Re-fetch the new flags that would have been saved in the saveAuthorisation service call
    DATASET dsAuthFlagValue:EMPTY-DATASET().

    ASSIGN oFVSearch  = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE)
           lSuccess   = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
           lSuccess   = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, dNewAuthObj)
           lSuccess   = oFVSearch:fetchData().

    //Now we need to copy the values of the old flags in to the new ones and do the save again
    COPY-TO-AUTH-FLAG-BLK:
    FOR EACH btt_auth_flag_value
       WHERE btt_auth_flag_value.owning_obj = dNewAuthObj :

      FIND FIRST btt_temp_auth_flag_value
           WHERE btt_temp_auth_flag_value.owning_obj    = dCopyFromAuthObj
             AND btt_temp_auth_flag_value.auth_rule_obj = btt_auth_flag_value.auth_rule_obj NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE btt_temp_auth_flag_value THEN
      DO:
        ASSIGN btt_auth_flag_value.auth_flag_value = btt_temp_auth_flag_value.auth_flag_value
               btt_auth_flag_value.record_action   = "MODIFY":U .

         VALIDATE btt_auth_flag_value.
      END. //IF AVAILABLE btt_temp_auth_flag_value

    END. // COPY-TO-AUTH-FLAG-BLK

    mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).

    FOR EACH tt_auth_flag_value_error :

      ASSIGN tt_auth_flag_value_error.owning_entity_mnemonic = "hatau":U
             tt_auth_flag_value_error.owning_obj             = btt_auth.auth_obj .

    END. //FOR EACH tt_auth_flag_value_error

    TEMP-TABLE tt_auth_error:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_flag_value_error:HANDLE ) .

  END. //IF btt_authcopy.flag_info = TRUE THEN

  IF btt_authcopy.external_ref = TRUE THEN
  DO:
    ASSIGN oExternalRefSearch = NEW cls.maexternalrefsearch(DATASET dsExternalReference BY-REFERENCE)
           lSuccess           = oExternalRefSearch:setCriteria("BufferList":U , "tt_extref,tt_schintref":U)
           iCount             = 0. //counter for new dummy objs

    oExternalRefSearch:fetchExternalReferences(INPUT "hatau":U , INPUT dCopyFromAuthObj , INPUT "":U).

    EXT-REF-BLK:
    FOR EACH btt_extref
       WHERE btt_extref.owning-entity-mnemonic = "hatau":U
         AND btt_extref.owning-key             = "":U
         AND btt_extref.owning-obj             = dCopyFromAuthObj :

      ASSIGN iCount = iCount - 1
             btt_extref.owning-obj       = dNewAuthObj
             btt_extref.extref-obj       = iCount
             btt_extref.owning-alt-value = opcNewAuthNum
             btt_extref.transaction-date = TODAY
             btt_extref.record_action    = "MODIFY":U .

      VALIDATE btt_extref.

    END. //EXT-REF-BLK:

     mipEnv:Health:maUtility:saveExternalReference(INPUT-OUTPUT DATASET dsExternalReference BY-REFERENCE).

  END. //IF btt_authcopy.external_ref = TRUE THEN

  {mip/inc/mipcatcherror.i
    &FINALLY="IF VALID-OBJECT(oAuthSearch)        THEN DELETE OBJECT oAuthSearch.
              IF VALID-OBJECT(oAuthRuleSearch)    THEN DELETE OBJECT oAuthRuleSearch.
              IF VALID-OBJECT(oFVSearch)          THEN DELETE OBJECT oFVSearch .
              IF VALID-OBJECT(oExternalRefSearch) THEN DELETE OBJECT oExternalRefSearch.
              IF VALID-OBJECT(oErrorObject)       THEN DELETE OBJECT oErrorObject.
              EMPTY TEMP-TABLE tt_temp_auth_flag_value.
              DATASET dsAuthFlagValue:EMPTY-DATASET().
              DATASET dsAuthRule:EMPTY-DATASET().
              DATASET dsExternalReference:EMPTY-DATASET().
              EMPTY TEMP-TABLE ttAuthTypeConfig."}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_findTariffLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _findTariffLink Procedure
PROCEDURE _findTariffLink PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipdTariffLinkObj AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTariffCode    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcBaseRate      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcArsRate       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrType        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdTariffLinkObj AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcBaseRate      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcArsRate       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcPrType        AS CHARACTER NO-UNDO.

DEFINE BUFFER buf_tariff_link FOR htm_tariff_link.

IF ipdTariffLinkObj <> 0 THEN
  FIND FIRST buf_tariff_link NO-LOCK
       WHERE buf_tariff_link.tariff_link_obj = ipdTariffLinkObj
       NO-ERROR.
ELSE
  FIND FIRST buf_tariff_link NO-LOCK
       WHERE buf_tariff_link.tariff_code = ipcTariffCode
       AND   buf_tariff_link.base_rate   = ipcBaseRate
       AND   buf_tariff_link.ars_rate    = ipcArsRate
       AND   buf_tariff_link.pr_type     = ipcPrType
       NO-ERROR.

{mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

IF AVAILABLE buf_tariff_link THEN
  ASSIGN opdTariffLinkObj = buf_tariff_link.tariff_link_obj
         opcBaseRate      = buf_tariff_link.base_rate
         opcArsRate       = buf_tariff_link.ars_rate
         opcPrType        = buf_tariff_link.pr_type.

{mip/inc/mipcatcherror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_nextStartDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _nextStartDateTime Procedure
PROCEDURE _nextStartDateTime PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdStartDate         AS DATE    NO-UNDO.
  DEFINE INPUT  PARAMETER iplStartTime         AS LOGICAL NO-UNDO.
  DEFINE INPUT  PARAMETER ipdCalculatedEndDate AS DATE    NO-UNDO.
  DEFINE INPUT  PARAMETER iplCalculatedEndTime AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opdNextStartDate     AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER oplNextStartTime     AS LOGICAL NO-UNDO.

  DEFINE VARIABLE dStartDate AS DATE        NO-UNDO.
  DEFINE VARIABLE lStartTime AS LOGICAL     NO-UNDO.

  ASSIGN
     dStartDate       = (IF ipdCalculatedEndDate = ?
                         THEN ipdStartDate
                         ELSE ipdCalculatedEndDate)

     lStartTime       = (IF ipdCalculatedEndDate =  ? /* Calculated start time is a logical so we cant be sure if it has been       */
                         THEN iplStartTime            /* set or if the value is defaulted so check calculated date variable instead */
                         ELSE iplCalculatedEndTime)

     opdNextStartDate = (IF NOT lStartTime AND ipdCalculatedEndDate <> ? /* PM and not first record then add a day */
                         THEN ADD-INTERVAL(dStartDate, 1, "DAYS":U)
                         ELSE dStartDate)

     oplNextStartTime = (IF ipdCalculatedEndDate <> ?
                         THEN NOT lStartTime
                         ELSE lStartTime).


  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_realignAuthDetailLOCSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _realignAuthDetailLOCSequence Procedure
PROCEDURE _realignAuthDetailLOCSequence PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Update Auth LOC Sequence
  Parameters:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER        ipdAuthobj      AS DECIMAL NO-UNDO.
  DEFINE        PARAMETER BUFFER btt_auth_detail FOR tt_auth_detail.

  DEFINE VARIABLE iSequence               AS INTEGER     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  ASSIGN iSequence = 0.

  FOR EACH btt_auth_detail EXCLUSIVE-LOCK
     WHERE btt_auth_detail.auth_obj               = ipdAuthobj
       AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
       AND btt_auth_detail.quantity_los          <> 0.00
        BY btt_auth_detail.loc_sequence:

    ASSIGN btt_auth_detail.record_action = "MODIFY":U WHEN btt_auth_detail.record_action = "":U.

    IF btt_auth_detail.record_action = "MODIFY":U THEN
      ASSIGN
         iSequence                     = iSequence + 1
         btt_auth_detail.loc_sequence  = iSequence.

    VALIDATE btt_auth_detail.

  END. /*FOR EACH btt_auth_detail EXCLUSIVE-LOCK*/
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_retrieveAuthsToCopy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _retrieveAuthsToCopy Procedure
PROCEDURE _retrieveAuthsToCopy :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE         PARAMETER BUFFER btt_authcopy FOR tt_authcopy.
  DEFINE OUTPUT  PARAMETER DATASET             FOR dsAuthorisation.

  DEFINE VARIABLE iCount             AS INTEGER           NO-UNDO.
  DEFINE VARIABLE iOptionCode        AS INTEGER           NO-UNDO.
  DEFINE VARIABLE dCopyToStartDate   AS DATE              NO-UNDO.
  DEFINE VARIABLE dInactStart        AS DATE              NO-UNDO.
  DEFINE VARIABLE dInactEnd          AS DATE              NO-UNDO.
  DEFINE VARIABLE dResignDate        AS DATE              NO-UNDO.
  DEFINE VARIABLE lAuthValid         AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lBatchCopy         AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lCheckMemberOption AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lValidRule         AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lSuccess           AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE cAuthType          AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cMessage           AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cRuleValue         AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE oErrorObject       AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE oAuthSearch        AS cls.maauthsearch  NO-UNDO.

  DEFINE BUFFER btt_auth      FOR tt_auth .
  DEFINE BUFFER buf_auth_type FOR hac_auth_type.

  /**
    Retrieve relevant auth batch according to copy criteria
  **/
  DATASET dsAuthorisation:EMPTY-DATASET().

  ASSIGN oAuthSearch  = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE)
         oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copy_error:HANDLE)
         lBatchCopy   = btt_authcopy.copy_type = "BATCH"

         lSuccess    = oAuthSearch:SetCriteria("BufferList":U, "tt_auth":U)
         lSuccess    = oAuthSearch:SetFilterCriteria("tt_auth.auth_status":U , "=":U, 1 ) //Only authorised auths

         //single criteria
         lSuccess    = IF  btt_authcopy.auth_num <> "":U
                       AND btt_authcopy.auth_num <> ?          THEN  oAuthSearch:SetFilterCriteria("tt_auth.auth_num":U ,    "=":U, btt_authcopy.auth_num)       ELSE TRUE

         //batch criteria
         lSuccess    = IF lBatchCopy AND btt_authcopy.from_end_date   <> ?    THEN oAuthSearch:SetFilterCriteria("tt_auth.end_date":U ,    "=":U, btt_authcopy.from_end_date)   ELSE TRUE
         lSuccess    = IF lBatchCopy AND btt_authcopy.from_start_date <> ?    THEN oAuthSearch:SetFilterCriteria("tt_auth.start_date":U ,  "=":U, btt_authcopy.from_start_date) ELSE TRUE
         lSuccess    = IF lBatchCopy AND btt_authcopy.insurer_obj     <> ?    THEN oAuthSearch:SetFilterCriteria("tt_auth.insurer_obj":U , "=":U, btt_authcopy.insurer_obj)     ELSE TRUE .



  /*
    Set criteria for all auth types listed - will only be applicable for batch copies
  */
  IF  btt_authcopy.auth_type_list <> "":U
  AND btt_authcopy.auth_type_list <> ? THEN
  DO:
    AUTH-TYPE-FILTER-BLK:
      DO iCount = 1 TO NUM-ENTRIES(btt_authcopy.auth_type_list):

        ASSIGN cAuthType = ENTRY(iCount, btt_authcopy.auth_type_list).

        FIND FIRST buf_auth_type NO-LOCK
             WHERE buf_auth_type.auth_type = cAuthType NO-ERROR .

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        ASSIGN  lSuccess  = IF AVAILABLE buf_auth_type THEN oAuthSearch:SetFilterCriteria("tt_auth.auth_type_obj":U, "=":U, buf_auth_type.auth_type_obj ) ELSE TRUE .

    END. //AUTH-TYPE-FILTER-BLK:
  END. //IF  btt_authcopy.auth_type_list <> "":U


  IF btt_authcopy.option_code <> ? AND btt_authcopy.option_code <> 0 THEN
  DO:
    /*
      Before we set criteria on the option code , we need to check the OptionCodeDefaultZero rule. If it is set up we can safely set criteria on the tt_auth.option_code field
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ?,
                                                   INPUT  0,
                                                   INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                   INPUT  "OptionCodeDefaultZero":U,
                                                   INPUT  btt_authcopy.to_end_date,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).

    IF lValidRule AND LOOKUP(STRING(btt_authcopy.option_code) , cRuleValue) > 0
    THEN
      ASSIGN lSuccess = oAuthSearch:SetCriteria("tt_auth.option_code" , "=", iOptionCode).
    ELSE
      ASSIGN lCheckMemberOption = TRUE .

  END.  // IF btt_authcopy.option_code <> ?

  // Fetch the data
  oAuthSearch:fetchData().

  /*
    Now we have the batch we recieved back from the data retrieval service. Do final validations and determine whether the record suited to be copied ,according to:
      - Option code
      - Member is active for the "copy to" period
  */
  DETERMINE-AUTHS-TO-COPY-BLK:
  FOR EACH btt_auth :

    ASSIGN lAuthValid = TRUE .
    /*
      If the option code was not in the OptionCodeDefaultZero rule , we need to retrieve the member option from the actual member, because it would not have been saved in
      the tt_auth.option_code field
    */
    IF lCheckMemberOption THEN
    DO:
      mipEnv:Health:maMember:getMemberOption(INPUT  btt_auth.mem_num,
                                             INPUT  btt_auth.start_date,
                                             OUTPUT iOptionCode).

      IF iOptionCode  <> btt_authcopy.option_code
      AND btt_authcopy.option_code <> ?
      THEN
        ASSIGN lAuthValid = FALSE.

    END. //IF lCheckMemberOption THEN

    /*
      Check if the member is active for "copy to" period
    */
    ASSIGN dCopyToStartDate = IF btt_authcopy.to_start_date = ? THEN btt_auth.end_date + 1
                                                                 ELSE btt_authcopy.to_start_date

           lSuccess         = mipEnv:Health:maMember:validateDependantInactivity(INPUT  btt_auth.mem_num,
                                                                                 INPUT  btt_auth.dependant,
                                                                                 INPUT  dCopyToStartDate,
                                                                                 INPUT  btt_authcopy.to_end_date,
                                                                                 OUTPUT dInactStart,
                                                                                 OUTPUT dInactEnd) .
    /*
      Check if the dependant is resigned for the "copy to" period
    */
    ASSIGN lSuccess = mipEnv:Health:maMember:validateResignation(INPUT "DEPENDANT":U,
                                                                 INPUT  btt_auth.mem_num,
                                                                 INPUT  btt_auth.dependant,
                                                                 INPUT  dCopyToStartDate,
                                                                 INPUT  btt_authcopy.to_end_date,
                                                                 OUTPUT dResignDate).

    IF (dInactStart <> ? OR dInactEnd <> ?)
    OR  dResignDate <> ? THEN
    DO:
      ASSIGN lAuthValid = FALSE
             cMessage   =  SUBSTITUTE("Member(&1) , dependant(&2) is inactive/resigned for the copy to period and will not be processed in the auth copy", btt_auth.mem_num, STRING(btt_auth.dependant))
             lSuccess   =  oErrorObject:addError(INPUT 'hataucopy',          // ipcOwningEntityMnemonic
                                                 INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                                 INPUT "":U,                 // ipcOwningEntityKey
                                                 INPUT btt_auth.line_number, // ipiLineNumber
                                                 INPUT cMessage,             // ipcMessageText
                                                 INPUT "WAR":U) NO-ERROR.    // ipcMessageType


    END. // IF (dInactStart <> ? OR dInactEnd <> ? OR  dResignDate <> ?

    IF NOT lAuthValid THEN
    DO:
      DELETE btt_auth.

      NEXT DETERMINE-AUTHS-TO-COPY-BLK.
    END. // IF NOT lAuthValid

  END. //DETERMINE-AUTHS-TO-COPY-BLK

  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oAuthSearch)  THEN DELETE OBJECT oAuthSearch.
                                      IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_revertAuthRateProviders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _revertAuthRateProviders Procedure
PROCEDURE _revertAuthRateProviders PRIVATE :
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation .
  DEFINE INPUT        PARAMETER ipdAuthObj            AS DECIMAL  NO-UNDO.
  DEFINE INPUT        PARAMETER ipdAuthRateControlObj AS DECIMAL  NO-UNDO.

  DEFINE BUFFER btt_auth               FOR tt_auth.
  DEFINE BUFFER btt_auth_provider      FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail        FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_rate_control  FOR tt_auth_rate_control.
  DEFINE BUFFER btt_auth_rate_provider FOR tt_auth_rate_provider.
  DEFINE BUFFER buf_tariff_type_link   FOR htm_tariff_type_link.
  DEFINE BUFFER buf_tariff_type        FOR htm_tariff_type.

  DEFINE VARIABLE lSuccess                AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE dDummyObj               AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj          AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffObj              AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj             AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE cAddValidations         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cArsRate                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cBaseRate               AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cAlertMessage           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cDefaultProviderList    AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cError                  AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cMessage                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cDeletedEntityList      AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cOverrideProviderList   AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cCreatedEntityList      AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cProviderType           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject     NO-UNDO.
  DEFINE VARIABLE oAuthRateControlSearch  AS cls.maauthratesearch  NO-UNDO.
  DEFINE VARIABLE oAcronymHelper          AS cls.mipacronym        NO-UNDO.

  DATASET dsAuthRateControl:EMPTY-DATASET().

  FIND FIRST btt_auth WHERE btt_auth.auth_obj = ipdAuthObj .

  ASSIGN oErrorObject           = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE)
         oAuthRateControlSearch = NEW cls.maauthratesearch(DATASET dsAuthRateControl BY-REFERENCE)
         lSuccess               =  oAuthRateControlSearch:SetCriteria("BufferList":U, "tt_auth_rate_control,tt_auth_rate_provider":U)
         lSuccess               =  oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.auth_rate_control_obj":U  , "=":U , ipdAuthRateControlObj).

  oAuthRateControlSearch:fetchData().

  FIND FIRST btt_auth_rate_control NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE btt_auth_rate_control THEN
  DO:
    ASSIGN dDummyObj             = -9999.99
           oAcronymHelper        = NEW cls.mipacronym(?, FALSE, "ma_acAuthProviderType":U, ?).

    RATE-PROVIDER-BLK:
    FOR EACH btt_auth_rate_provider
       WHERE btt_auth_rate_provider.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj
         AND btt_auth_rate_provider.effective_date       <= btt_auth.start_date
          BY btt_auth_rate_provider.auth_rate_indicator :

      /*******
        Handle Provider Overrides
      *******/
      IF btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorOverride":U THEN
      DO:
        FOR EACH btt_auth_provider
           WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
             AND btt_auth_provider.pr_type       = btt_auth_rate_provider.auth_rate_pr_type
             AND btt_auth_provider.sub_pr_type   = btt_auth_rate_provider.auth_rate_sub_pr_type
             AND btt_auth_provider.provider_type = btt_auth_rate_provider.auth_rate_provider_type :

            IF btt_auth_rate_provider.revert_auth_status <> ? THEN
            DO:
              ASSIGN btt_auth_provider.auth_status        = INTEGER(btt_auth_rate_provider.revert_auth_status)
                     btt_auth_provider.auth_status_note   = btt_auth_rate_provider.revert_auth_status_note
                     btt_auth_provider.rate_change_type   = "ma_acAuthRateChangeTypeProviderRevert":U
                     btt_auth_provider.record_action      = IF btt_auth_provider.record_action <> "DELETE":U THEN "MODIFY":U ELSE "DELETE":U .

              VALIDATE btt_auth_rate_provider.

              oAcronymHelper:focusAcronym("KEY":U, btt_auth_provider.provider_type) NO-ERROR.

              ASSIGN cProviderType          = IF   oAcronymHelper:AcronymInFocus
                                              THEN oAcronymHelper:AcronymLabel
                                              ELSE btt_auth_provider.provider_type
                     cOverrideProviderList  = cOverrideProviderList
                                            + (IF cOverrideProviderList = "":U THEN "":U ELSE "^":U)
                                            + cProviderType   + ", ":U
                                            + "Disc: ":U      + STRING(btt_auth_rate_provider.auth_rate_pr_type )     + " & ":U
                                            + "Sub Disc : ":U + STRING(btt_auth_rate_provider.auth_rate_sub_pr_type )
                                            + " has been updated with the revert auth status and reason".

            END. //IF btt_auth_rate_provider.revert_auth_status <> ?

        END. /* FOR EACH btt_auth_provider  */
      END. /* IF OVERRIDE  btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorOverride":U*/

      /*******
        Handle Provider Defaults
      ********/
      IF  btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorDefault":U THEN
      DO:

        FIND FIRST btt_auth_provider
             WHERE btt_auth_provider.auth_obj          = btt_auth.auth_obj
               AND btt_auth_provider.main_provider     = FALSE
               AND btt_auth_provider.pr_type           = btt_auth_rate_provider.auth_rate_pr_type
               AND btt_auth_provider.sub_pr_type       = btt_auth_rate_provider.auth_rate_sub_pr_type
               AND btt_auth_provider.provider_type     = btt_auth_rate_provider.auth_rate_provider_type
            NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE btt_auth_provider THEN
        DO:

          /*
            Revert auth provider status and status notes if a revert status and note are setup . If not we will mark the record to be deleted
          */
          ASSIGN btt_auth_provider.auth_status             = INTEGER(btt_auth_rate_provider.revert_auth_status)  WHEN btt_auth_rate_provider.revert_auth_status      <> ?
                 btt_auth_provider.auth_status_note        = btt_auth_rate_provider.revert_auth_status_note
                 btt_auth_provider.rate_change_type        = "ma_acAuthRateChangeTypeProviderRevert":U
                 btt_auth_provider.record_action           = IF btt_auth_rate_provider.revert_auth_status      <> ? THEN "MODIFY":U
                                                                                                                    ELSE "DELETE":U  .

          VALIDATE btt_auth_rate_provider.

          oAcronymHelper:focusAcronym("KEY":U, btt_auth_provider.provider_type) NO-ERROR.

          ASSIGN cProviderType                  = IF oAcronymHelper:AcronymInFocus THEN oAcronymHelper:AcronymLabel ELSE btt_auth_provider.provider_type
                 cDefaultProviderList           = cDefaultProviderList
                                                + (IF cDefaultProviderList = "":U THEN "":U ELSE "^":U)
                                                + cProviderType   + ", ":U
                                                + "Disc : ":U     + STRING(btt_auth_rate_provider.auth_rate_pr_type)      + " & ":U
                                                + "Sub Disc : ":U + STRING(btt_auth_rate_provider.auth_rate_sub_pr_type )
                                                + IF   btt_auth_provider.record_action = "DELETE":U
                                                  THEN " has been deleted"
                                                  ELSE " has been updated with the revert auth status and reason".

        END. //IF AVAILABLE btt_auth_provider
      END. /* IF DEFAULT (btt_auth_rate_provider.auth_rate_indicator   = "ma_acAuthRateProvIndicatorDefault":U) */

      /*******
        Delete any Related Entities that were created from auth rate provider setup
      *******/
      IF  btt_auth_rate_provider.related_entity_mnemonic <> "":U
      AND btt_auth_rate_provider.related_alt_value       <> "":U
      AND btt_auth_rate_provider.related_obj              > 0 THEN
      DO:
        PROVIDER-ENTITY-DELETE-BLK:
        FOR EACH btt_auth_provider
           WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
             AND btt_auth_provider.pr_type       = btt_auth_rate_provider.auth_rate_pr_type
             AND btt_auth_provider.sub_pr_type   = btt_auth_rate_provider.auth_rate_sub_pr_type
             AND btt_auth_provider.provider_type = btt_auth_rate_provider.auth_rate_provider_type:


          ASSIGN cBaseRate        = IF   btt_auth_provider.override_base_rate <> "":U
                                    THEN btt_auth_provider.override_base_rate
                                    ELSE btt_auth_provider.default_base_rate
                 cArsRate         = IF   btt_auth_provider.override_ars_rate  <> "":U
                                    THEN btt_auth_provider.override_ars_rate
                                    ELSE btt_auth_provider.default_ars_rate
                 cAddValidations  = "":U
                 dTariffLinkObj   = 0.

          IF  cBaseRate = "":U
          AND cArsRate  = "":U
          THEN
            mipEnv:Health:maDoctor:getProviderBaseRates(INPUT  btt_auth_provider.doc_num,
                                                        INPUT  btt_auth.mem_num,
                                                        INPUT  btt_auth.option_code,
                                                        INPUT  btt_auth_provider.start_date,
                                                        OUTPUT cBaseRate ,
                                                        OUTPUT cArsRate ).

          /*
            Get the tariff link obj
          */
          mipEnv:health:mamedical:getValidTariff( INPUT-OUTPUT dTariffLinkObj ,                          // iopdTariffLinkObj
                                                  INPUT        btt_auth_rate_provider.related_alt_value, // ipcTariffCode
                                                  INPUT        cBaseRate,                                // ipcBaseRate
                                                  INPUT        cArsRate ,                                // ipcARSRate
                                                  INPUT        btt_auth_provider.pr_type ,               // ipiPrType
                                                  INPUT        btt_auth_provider.sub_pr_type,            // ipiSubPrType
                                                  INPUT        btt_auth_provider.start_date,             // ipdDate
                                                  INPUT        btt_auth.option_code,                     // ipiOptionCode
                                                  INPUT        cAddValidations,                          // ipcAddValidations
                                                  OUTPUT       dTariffObj,                               // opdTariffObj
                                                  OUTPUT       dTrfCostObj,                              // opdTrfCostObj
                                                  OUTPUT       cError,                                   // opcError
                                                  OUTPUT       cWarning,                                 // opcWarning
                                                  OUTPUT       cAlertMessage).                           // cAlertMessage

          FIND FIRST btt_auth_detail
               WHERE btt_auth_detail.auth_provider_obj      = btt_auth_provider.auth_provider_obj
                 AND btt_auth_detail.owning_entity_mnemonic = btt_auth_rate_provider.related_entity_mnemonic
                 AND btt_auth_detail.owning_obj             = dTariffLinkObj
                 AND btt_auth_detail.owning_key             = btt_auth_rate_provider.related_key
                 AND (   (btt_auth_detail.start_date        = btt_auth_provider.start_date AND btt_auth_detail.start_ampm = btt_auth_provider.start_ampm)
                      OR (btt_auth_detail.end_date          = btt_auth_provider.end_date   AND btt_auth_detail.end_ampm   = btt_auth_provider.end_ampm) ) NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE btt_auth_detail
          THEN
            ASSIGN btt_auth_detail.record_action = "DELETE":U
                   cDeletedEntityList            = cDeletedEntityList + (IF cDeletedEntityList = "":U THEN "":U ELSE "^":U)
                                                 + btt_auth_rate_provider.related_alt_value
                                                 + cBaseRate
                                                 + IF cArsRate <> "":U THEN ",":U + cArsRate ELSE "":U
                                                 + ",":U + STRING(btt_auth_provider.pr_type    )
                                                 + IF btt_auth_provider.sub_pr_type <> 0 THEN  ",":U + STRING(btt_auth_provider.sub_pr_type) ELSE "" .

        END. /* PROVIDER-ENTITY-DELETE-BLK */
      END. /* IF btt_auth_rate_provider.related_entity_mnemonic <> "":U AND btt_auth_rate_provider.related_alt_value <> "":U  AND btt_auth_rate_provider.related_entity_obj > 0 */
    END. /* RATE-PROVIDER-BLK: */

    /*
      Now generate warnings for defaulted/overrided providers after the revert
    */
    IF cOverrideProviderList <> "":U
    THEN
      ASSIGN cMessage  = "Existing provider record(s) were overrided by auth rate control setup:":U
                       + SUBSTITUTE("[notificationlist=&1]", cOverrideProviderList)
             lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT btt_auth.line_number, // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "WAR":U).             // ipcMessageType

    IF cDefaultProviderList <> "":U
    THEN
      ASSIGN cMessage  = "The following default provider records were adjusted from rate control setup after the revert : "
                       + SUBSTITUTE("[notificationlist=&1]", cDefaultProviderList)
             lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT btt_auth.line_number, // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "WAR":U).             // ipcMessageType
    IF cDeletedEntityList <> "":U
    THEN
      ASSIGN cMessage  = "The following auth detail records were deleted from rate control setup : "
                       + SUBSTITUTE("[notificationlist=&1]", cDeletedEntityList)
             lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT btt_auth.line_number, // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "WAR":U).             // ipcMessageType
  END. /* IF AVAILABLE tt_auth_rate_control */

  {mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oAuthRateControlSearch) THEN DELETE OBJECT oAuthRateControlSearch.
                IF VALID-OBJECT(oAcronymHelper)         THEN DELETE OBJECT oAcronymHelper.
                IF VALID-OBJECT(oErrorObject)           THEN DELETE OBJECT oErrorObject.
                DATASET dsAuthRateControl:EMPTY-DATASET()." }
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_updateAuthDetailLOCSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _updateAuthDetailLOCSequence Procedure
PROCEDURE _updateAuthDetailLOCSequence PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Update Auth LOC Sequence
  Parameters:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthobj      AS DECIMAL NO-UNDO.
  DEFINE PARAMETER BUFFER btt_auth_detail FOR tt_auth_detail.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER seq_auth_detail FOR tt_auth_detail.
  DEFINE BUFFER buf_auth_detail FOR hat_auth_detail.

  FIND FIRST buf_auth_detail NO-LOCK
       WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  /*
    Find existing records where the line sequence is the same
  */
  IF AVAILABLE buf_auth_detail AND ABSOLUTE(buf_auth_detail.loc_sequence - btt_auth_detail.loc_sequence) = 1 THEN
  DO:
    /*
      If the change in sequence was 1 and we are able to find a record with the sequence
      we are changing to then we will simply swop sequences
    */
    FIND FIRST seq_auth_detail EXCLUSIVE-LOCK
         WHERE seq_auth_detail.auth_obj               = ipdAuthobj
           AND seq_auth_detail.owning_entity_mnemonic = "htmtl":U
           AND seq_auth_detail.quantity_los          <> 0.00
           AND seq_auth_detail.loc_sequence           = btt_auth_detail.loc_sequence
           AND seq_auth_detail.auth_detail_obj       <> btt_auth_detail.auth_detail_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE seq_auth_detail THEN
    DO:
      ASSIGN seq_auth_detail.loc_sequence = buf_auth_detail.loc_sequence.

      VALIDATE seq_auth_detail.
    END. /*IF AVAILABLE seq_auth_detail THEN*/
  END. /*IF AVAILABLE buf_auth_detail AND ABSOLUTE(buf_auth_detail.loc_sequence - btt_auth_detail.loc_sequence) = 1 THEN*/
  ELSE
  DO:
    /*
      Find all existing lines where the sequence is the same or greater than and shift the sequence down
    */
    FOR EACH seq_auth_detail EXCLUSIVE-LOCK
       WHERE seq_auth_detail.auth_obj               = ipdAuthobj
         AND seq_auth_detail.owning_entity_mnemonic = "htmtl":U
         AND seq_auth_detail.quantity_los          <> 0.00
         AND seq_auth_detail.record_action          = "":U
         AND seq_auth_detail.loc_sequence          >= btt_auth_detail.loc_sequence:

      ASSIGN seq_auth_detail.loc_sequence = seq_auth_detail.loc_sequence + 1.

      VALIDATE seq_auth_detail.
    END. /*FOR EACH seq_auth_detail EXCLUSIVE-LOCK*/
  END. /*DO:*/
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCopyCriteria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCopyCriteria Procedure
PROCEDURE _validateAuthCopyCriteria :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE               PARAMETER BUFFER btt_authcopy FOR tt_authcopy.
  DEFINE INPUT-OUTPUT  PARAMETER TABLE               FOR tt_auth_copy_error.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE dAuthTypeObj             AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE dInsurerObj              AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE iCount                   AS INTEGER           NO-UNDO.
  DEFINE VARIABLE iOptionCode              AS INTEGER           NO-UNDO.
  DEFINE VARIABLE iDefaultCopyStatus       AS INTEGER           NO-UNDO.
  DEFINE VARIABLE dEffectiveDate           AS DATE              NO-UNDO.
  DEFINE VARIABLE lValidRule               AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lSuccess                 AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE cAuthType                AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cMessage                 AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cNewAuthNum              AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cDefaultCopyStatusReason AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cRuleValue               AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE oErrorObject             AS cls.maerrorobject NO-UNDO.

  DEFINE BUFFER scheme                FOR scheme.
  DEFINE BUFFER erm_insurer           FOR erm_insurer.
  DEFINE BUFFER btt_auth_copy_error   FOR tt_auth_copy_error.
  DEFINE BUFFER ttAuthTypeConfig      FOR ttAuthTypeConfig.
  DEFINE BUFFER buf_auth_type         FOR hac_auth_type.
  DEFINE BUFFER buf_auth              FOR hat_auth.

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE btt_auth_copy_error:HANDLE ).

  //Assign variables for use when retrieving auth rules
  ASSIGN  dInsurerObj     = IF btt_authcopy.insurer_obj = ? THEN 0 ELSE btt_authcopy.insurer_obj
          iOptionCode     = IF btt_authcopy.option_code = ? THEN 0 ELSE btt_authcopy.option_code
          dEffectiveDate  =    btt_authcopy.from_start_date.

  //Basic validations to see that the necessary are fields populated
  IF  btt_authcopy.to_start_date <> ? THEN
  DO:
    ASSIGN cMessage = IF btt_authcopy.to_start_date > btt_authcopy.to_end_date
                      THEN "The To Start Date can not be after the To End Date.":U
                      ELSE IF btt_authcopy.to_start_date < btt_authcopy.from_end_date
                           THEN  "The To Start Date can not be before the From End Date.":U
                           ELSE  "":U.
    IF cMessage <> "":U
    THEN
      ASSIGN lSuccess = oErrorObject:addError
                             (INPUT 'hataucopy',        // ipcOwningEntityMnemonic
                              INPUT 0,                  // ipdOwningEntityObj
                              INPUT "":U,               // ipcOwningEntityKey
                              INPUT 0,                  // ipiLineNumber
                              INPUT cMessage,           // ipcMessageText
                              INPUT "ERR":U) NO-ERROR.  // ipcMessageType
  END. //IF  btt_authcopy.to_start_date <> ? THEN

  IF btt_authcopy.to_end_date = ?
  THEN
    ASSIGN lSuccess = oErrorObject:addError
                           (INPUT "hataucopy":U,          // ipcOwningEntityMnemonic
                            INPUT 0.00,                   // ipdOwningEntityObj
                            INPUT "":U,                   // ipcOwningEntityKey
                            INPUT 0,                      // ipiLineNumber
                            INPUT "MA":U,                 // ipcMessageGroup
                            INPUT 355,                    // &1 cannot be blank or unknown, please input a correct value.
                            INPUT "To End Date":U).       // ipcReplaceTextList
  ELSE DO:
    ASSIGN cMessage = IF btt_authcopy.to_end_date < btt_authcopy.from_end_date
                      THEN "To End Date can not be before From End Date":U
                      ELSE IF  btt_authcopy.to_start_date <> ?
                           AND btt_authcopy.to_end_date   <  btt_authcopy.to_start_date
                           THEN "To End Date can not be before To Start Date":U
                           ELSE "":U .
    IF cMessage <> "":U
    THEN
      ASSIGN lSuccess  =  oErrorObject:addError
                               (INPUT 'hataucopy',        // ipcOwningEntityMnemonic
                                INPUT 0,                  // ipdOwningEntityObj
                                INPUT "":U,               // ipcOwningEntityKey
                                INPUT 0,                  // ipiLineNumber
                                INPUT cMessage,           // ipcMessageText
                                INPUT "ERR":U) NO-ERROR.  // ipcMessageType
  END. // ELSE DO (IF btt_authcopy.to_end_date)

  IF  btt_authcopy.option_code <> ?
  AND btt_authcopy.option_code <> 0 THEN
  DO:
    FIND FIRST scheme NO-LOCK
         WHERE scheme.scheme-code = btt_authcopy.option_code
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE scheme
    THEN
      ASSIGN lSuccess = oErrorObject:addError
                             (INPUT "hataucopy":U,                                                 // ipcOwningEntityMnemonic
                              INPUT 0.00,                                                          // ipdOwningEntityObj
                              INPUT "":U,                                                          // ipcOwningEntityKey
                              INPUT 0,                                                             // ipiLineNumber
                              INPUT "MA":U,                                                        // ipcMessageGroup
                              INPUT 100,                                                           // The "&1" specified is invalid.
                              INPUT "Option Code (":U + STRING(btt_authcopy.option_code) + ")":U).  // ipcReplaceTextList
  END. // IF  btt_authcopy.option_code <> ? AND btt_authcopy.option_code <> 0 THEN

  IF  btt_authcopy.insurer_obj  <> 0
  AND btt_authcopy.insurer_obj <> ? THEN
  DO:
    FIND FIRST erm_insurer NO-LOCK
         WHERE erm_insurer.insurer_obj  = btt_authcopy.insurer_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer
    THEN
      ASSIGN lSuccess = oErrorObject:addError
                             (INPUT "hataucopy":U,                                                 // ipcOwningEntityMnemonic
                              INPUT 0.00,                                                          // ipdOwningEntityObj
                              INPUT "":U,                                                          // ipcOwningEntityKey
                              INPUT 0,                                                             // ipiLineNumber
                              INPUT "MA":U,                                                        // ipcMessageGroup
                              INPUT 100,                                                           // The "&1" specified is invalid.
                              INPUT "Insurer Obj (":U + STRING(btt_authcopy.insurer_obj) + ")":U).  // ipcReplaceTextList
  END. //IF btt_authcopy.insurer_obj  <> 0 AND btt_authcopy.insurer_obj <> ? THEN

  //Try to find the auth copy default status rule
  mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                (INPUT  btt_authcopy.insurer_obj,
                                 INPUT  btt_authcopy.option_code,
                                 INPUT  "ma_acAuthRuleTypeAuthCopy":U,
                                 INPUT  "AuthCopyStatusDefault":U,
                                 INPUT  btt_authcopy.to_end_date,
                                 OUTPUT lValidRule,
                                 OUTPUT cRuleValue).
  IF lValidRule THEN
  DO:
    ASSIGN iDefaultCopyStatus       = INTEGER(ENTRY(1,cRuleValue,"|":U) )
           cDefaultCopyStatusReason =         ENTRY(2,cRuleValue,"|":U)
      NO-ERROR.

    IF {&ErrorStatus} THEN
    DO:
      ASSIGN cMessage = "The AuthCopyStatusDefault rule is not correctly set up.Correct format is 'Status|StatusReason'":U
             lSuccess = oErrorObject:addError
                             (INPUT 'hatau',   // ipcOwningEntityMnemonic
                              INPUT 0,         // ipdOwningEntityObj
                              INPUT "":U,      // ipcOwningEntityKey
                              INPUT 0,         // ipiLineNumber
                              INPUT cMessage,  // ipcMessageText
                              INPUT "ERR":U,   // ipcMessageType
                              INPUT TRUE).     // iplAcknowledge
    END. //IF {&ErrorStatus}
  END. //IF lValidRule THEN
  ELSE DO:
    ASSIGN cMessage = "The AuthCopyStatusDefault rule is not set up.Set up the rule before attempting the copy":U
           lSuccess = oErrorObject:addError
                           (INPUT 'hatau',     // ipcOwningEntityMnemonic
                            INPUT 0,           // ipdOwningEntityObj
                            INPUT "":U,        // ipcOwningEntityKey
                            INPUT 0,           // ipiLineNumber
                            INPUT cMessage,    // ipcMessageText
                            INPUT "ERR":U,     // ipcMessageType
                            INPUT TRUE).       // iplAcknowledge
  END. //ELSE DO(IF lValidRule THEN )

  // Specific validations for batch copy
  IF btt_authcopy.copy_type = "BATCH":U THEN
  DO:
    IF btt_authcopy.from_start_date = ?
    THEN
      ASSIGN lSuccess = oErrorObject:addError
                             (INPUT "hataucopy":U,         // ipcOwningEntityMnemonic
                              INPUT 0.00,                  // ipdOwningEntityObj
                              INPUT "":U,                  // ipcOwningEntityKey
                              INPUT 0,                     // ipiLineNumber
                              INPUT "MA":U,                // ipcMessageGroup
                              INPUT 355,                   // &1 cannot be blank or unknown, please input a correct value.
                              INPUT "From Start Date":U).  // ipcReplaceTextList

    IF btt_authcopy.from_end_date = ?
    THEN
      ASSIGN lSuccess = oErrorObject:addError
                             (INPUT "hataucopy":U,       // ipcOwningEntityMnemonic
                              INPUT 0.00,                // ipdOwningEntityObj
                              INPUT "":U,                // ipcOwningEntityKey
                              INPUT 0,                   // ipiLineNumber
                              INPUT "MA":U,              // ipcMessageGroup
                              INPUT 355,                 // &1 cannot be blank or unknown, please input a correct value.
                              INPUT "From End Date":U).  // ipcReplaceTextList


    IF btt_authcopy.from_end_date <  btt_authcopy.from_start_date
    THEN
      ASSIGN cMessage = "The From End Date can not be before the From Start Date."
             lSuccess = oErrorObject:addError
                             (INPUT 'hataucopy',          // ipcOwningEntityMnemonic
                              INPUT 0,                    // ipdOwningEntityObj
                              INPUT "":U,                 // ipcOwningEntityKey
                              INPUT 0,                    // ipiLineNumber
                              INPUT cMessage,             // ipcMessageText
                              INPUT "ERR":U) NO-ERROR.    // ipcMessageType



    //Auth types specified must all exist and be valid on the AuthCopyValidTypes rule
    IF btt_authcopy.auth_type_list = "":U
    THEN
      ASSIGN lSuccess = oErrorObject:addError
                              (INPUT "hataucopy":U,        // ipcOwningEntityMnemonic
                               INPUT 0.00,                 // ipdOwningEntityObj
                               INPUT "":U,                 // ipcOwningEntityKey
                               INPUT 0,                    // ipiLineNumber
                               INPUT "MA":U,               // ipcMessageGroup
                               INPUT 355,                  // &1 cannot be blank or unknown, please input a correct value.
                               INPUT "Auth type list":U).  // ipcReplaceTextList
    ELSE DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                    (INPUT  dInsurerObj,
                                     INPUT  iOptionCode,
                                     INPUT  "ma_acAuthRuleTypeAuthCopy":U,
                                     INPUT  "AuthCopyValidTypes":U,
                                     INPUT  dEffectiveDate,
                                     OUTPUT lValidRule,
                                     OUTPUT cRuleValue).

      IF NOT lValidRule OR cRuleValue = "":U
      THEN
        ASSIGN cMessage = "AuthCopyValidTypes rule is not set up. Please set up the rule before attempting a copy."
               lSuccess = oErrorObject:addError
                                (INPUT 'hataucopy',          // ipcOwningEntityMnemonic
                                 INPUT 0 ,                   // ipdOwningEntityObj
                                 INPUT "":U,                 // ipcOwningEntityKey
                                 INPUT 0,                    // ipiLineNumber
                                 INPUT cMessage,             // ipcMessageText
                                 INPUT "ERR":U) NO-ERROR.    // ipcMessageType
      ELSE DO:
        AUTH-TYPE-BLK:
        DO iCount = 1 TO NUM-ENTRIES(btt_authcopy.auth_type_list):

          ASSIGN cAuthType =  ENTRY(iCount, btt_authcopy.auth_type_list).

          IF LOOKUP(cAuthType,cRuleValue) = 0
          THEN
            ASSIGN lSuccess = oErrorObject:addError
                                   (INPUT "hataucopy":U,                        // ipcOwningEntityMnemonic
                                    INPUT 0.00,                                 // ipdOwningEntityObj
                                    INPUT "":U,                                 // ipcOwningEntityKey
                                    INPUT 0,                                    // ipiLineNumber
                                    INPUT "MA":U,                               // ipcMessageGroup
                                    INPUT 112,                                  // The "&1" specified is invalid.&2
                                    INPUT "Auth type(":U + cAuthType + ")":U    // ipcReplaceTextList
                                        + ", Please esnure the auth type is set up in the AuthCopyValidTypes rule ":U).

          FIND FIRST buf_auth_type NO-LOCK
               WHERE buf_auth_type.auth_type = cAuthType
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE buf_auth_type
          THEN
            ASSIGN lSuccess = oErrorObject:addError
                                    (INPUT "hataucopy":U,                        // ipcOwningEntityMnemonic
                                     INPUT 0.00,                                 // ipdOwningEntityObj
                                     INPUT "":U,                                 // ipcOwningEntityKey
                                     INPUT 0,                                    // ipiLineNumber
                                     INPUT "MA":U,                               // ipcMessageGroup
                                     INPUT 112,                                  // The "&1" specified is invalid.&2
                                     INPUT "Auth type(":U + cAuthType + ")":U
                                         + "The Auth type does not exist."  ).   // ipcReplaceTextList
          ELSE DO:
            ASSIGN dAuthTypeObj = buf_auth_type.auth_type_obj .

            mipEnv:Health:AuthService:getAuthTypeConfig(INPUT dAuthTypeObj,
                                                        INPUT dInsurerObj,
                                                        INPUT iOptionCode,
                                                        INPUT dEffectiveDate,
                                                        INPUT-OUTPUT TABLE ttAuthTypeConfig).

            FIND FIRST ttAuthTypeConfig NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

            IF AVAILABLE ttAuthTypeConfig
            AND ttAuthTypeConfig.activateLOS
            THEN
              ASSIGN lSuccess = oErrorObject:addError
                                      (INPUT "hataucopy":U,                        // ipcOwningEntityMnemonic
                                       INPUT 0.00,                                 // ipdOwningEntityObj
                                       INPUT "":U,                                 // ipcOwningEntityKey
                                       INPUT 0,                                    // ipiLineNumber
                                       INPUT "MA":U,                               // ipcMessageGroup
                                       INPUT 112,                                  // The "&1" specified is invalid.&2
                                       INPUT "Auth type(":U + cAuthType + ")":U
                                           + "LOS Auth types can not be copied."). // ipcReplaceTextList
            IF AVAILABLE ttAuthTypeConfig
            AND   ttAuthTypeConfig.EffectiveDate > btt_authcopy.to_start_date
             OR ( ttAuthTypeConfig.EndDate <> ? AND ttAuthTypeConfig.EndDate  < btt_authcopy.to_end_date)
            THEN
              ASSIGN lSuccess = oErrorObject:addError
                                     (INPUT "hataucopy":U,                        // ipcOwningEntityMnemonic
                                      INPUT 0.00,                                 // ipdOwningEntityObj
                                      INPUT "":U,                                 // ipcOwningEntityKey
                                      INPUT 0,                                    // ipiLineNumber
                                      INPUT "MA":U,                               // ipcMessageGroup
                                      INPUT 112,                                  // The "&1" specified is invalid.&2
                                      INPUT "Auth type(":U + cAuthType + ")":U    // ipcReplaceTextList
                                          + "Copy to start and end dates don~'t fall within the Authorisation type effective date range. "   ).
          END. // ELSE DO (IF NOT AVAILABLE buf_auth_type)
        END. // AUTH-TYPE-BLK
      END. //ELSE DO: (IF NOT lValidRule OR cRuleValue = "" )
    END. // ELSE DO: ( IF btt_authcopy.auth_type_list = "" )
  END. //IF iplBatchCopy
  ELSE IF btt_authcopy.copy_type = "SINGLE":U THEN
  DO:
    //Specific validations for a single auth copy
    //Auth number specified must be valid
    IF btt_authcopy.auth_num = "":U OR btt_authcopy.auth_num = ?
    THEN
      ASSIGN lSuccess = oErrorObject:addError
                             (INPUT "hataucopy":U,       // ipcOwningEntityMnemonic
                              INPUT 0.00,                // ipdOwningEntityObj
                              INPUT "":U,                // ipcOwningEntityKey
                              INPUT 0,                   // ipiLineNumber
                              INPUT "MA":U,              // ipcMessageGroup
                              INPUT 355,                 // &1 cannot be blank or unknown, please input a correct value.
                              INPUT "Auth Number":U).    // ipcReplaceTextList
    ELSE DO:
      IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
                      WHERE buf_auth.auth_num = btt_authcopy.auth_num)
      THEN
        ASSIGN lSuccess = oErrorObject:addError
                               (INPUT "hataucopy":U,                                              // ipcOwningEntityMnemonic
                                INPUT 0.00,                                                       // ipdOwningEntityObj
                                INPUT "":U,                                                       // ipcOwningEntityKey
                                INPUT 0,                                                          // ipiLineNumber
                                INPUT "MA":U,                                                     // ipcMessageGroup
                                INPUT 100,                                                        // The "&1" specified is invalid.
                                INPUT "Auth Number(":U + STRING(btt_authcopy.auth_num) + ")":U).  // ipcReplaceTextList
    END. //IF btt_authcopy.auth_num = "":U OR btt_authcopy.auth_num = ?
  END. // ELSE btt_authcopy.copy_type = "SINGLE":U DO
  ELSE
    ASSIGN lSuccess = oErrorObject:addError
                           (INPUT "hataucopy":U,                                              // ipcOwningEntityMnemonic
                            INPUT 0.00,                                                       // ipdOwningEntityObj
                            INPUT "":U,                                                       // ipcOwningEntityKey
                            INPUT 0,                                                          // ipiLineNumber
                            INPUT "MA":U,                                                     // ipcMessageGroup
                            INPUT 100,                                                        // The "&1" specified is invalid.
                            INPUT "Copy Type(":U + STRING(btt_authcopy.copy_type) + ")":U).   // ipcReplaceTextList

  { mip/inc/mipcatcherror.i
            &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnGetErrorMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnGetErrorMessage Procedure
FUNCTION fnGetErrorMessage RETURNS CHARACTER
  ( INPUT ipcMessageGroup  AS CHARACTER,
    INPUT ipiMessageNumber AS INTEGER,
    INPUT ipcReplaceText   AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieve the message and replace code were applicable.
            Returns the complete message after parameters were replaced.
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cMessageText        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMessageDescription AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMessageType        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE iEntry              AS INTEGER     NO-UNDO.

  ASSIGN lSuccess = mipEnv:miUtility:getMessageText(INPUT ipcMessageGroup,
                                                    INPUT STRING(ipiMessageNumber),
                                                    INPUT "ENG":U,
                                                    OUTPUT cMessageText,
                                                    OUTPUT cMessageDescription,
                                                    OUTPUT cMessageType).
  IF cMessageText <> "":U THEN
  DO:
    DO iEntry = 1 TO NUM-ENTRIES(ipcReplaceText):
      ASSIGN cMessageText = REPLACE(cMessageText,"&" + STRING(iEntry),ENTRY(iEntry,ipcReplaceText)).
    END.  /* DO iEntry = 1 TO NUM-ENTRIES(ipcReplaceText): */

    ASSIGN cMessageText = SUBSTITUTE(cMessageText,"").
  END.  /* IF cMessageText <> "":U THEN */

  RETURN cMessageText.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnResetAuthDetailValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnResetAuthDetailValues Procedure
FUNCTION fnResetAuthDetailValues RETURNS LOGICAL
  ( BUFFER btt_auth_detail   FOR tt_auth_detail,
    BUFFER bttAuthTypeConfig FOR ttAuthTypeConfig) :
/*------------------------------------------------------------------------------
  Purpose: Clear auth detail buffer fields that we do not intend to copy over
           for the auth copy
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iDefaultClaimCode AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cDefaultClaimType AS CHARACTER NO-UNDO.

  ASSIGN iDefaultClaimCode = bttAuthTypeConfig.DefaultClaimCodeDetail
         cDefaultClaimType = bttAuthTypeConfig.DefaultClaimTypeDetail NO-ERROR .

  IF {&ErrorStatus} THEN
  DO:
    ASSIGN iDefaultClaimCode = 0
           cDefaultClaimType = "":U .
  END. // IF {&ErrorStatus}

  ASSIGN btt_auth_detail.adjustment_auth             =  0
         btt_auth_detail.amount_auth                 =  0
         btt_auth_detail.amount_requested            =  0
         btt_auth_detail.amount_paid                 =  0
         btt_auth_detail.discount_auth               =  0
         btt_auth_detail.discount_paid               =  0
         btt_auth_detail.adjustment_private_auth     =  0
         btt_auth_detail.adjustment_private_paid     =  0
         btt_auth_detail.copay_auth                  =  0
         btt_auth_detail.copay_paid                  =  0
         btt_auth_detail.adjustment_paid             =  0
         btt_auth_detail.pmb_value                   =  0
         btt_auth_detail.quantity_paid               =  0
         btt_auth_detail.quantity_requested          =  0
         btt_auth_detail.pmb_benefit_%               =  0
         btt_auth_detail.pmb_pay_cost                =  FALSE
         btt_auth_detail.pmb_indicator               =  FALSE
         btt_auth_detail.claim_code                  =  0
         btt_auth_detail.claim_type                  =  "":U
         btt_auth_detail.benefit_%                   =  0
         btt_auth_detail.quantity_claimed            =  0
         btt_auth_detail.override_base_rate          =  "":U
         btt_auth_detail.override_ars_rate           =  "":U
         btt_auth_detail.default_claim_code          =  iDefaultClaimCode
         btt_auth_detail.default_claim_type          =  cDefaultClaimType
         btt_auth_detail.item_cost                   =  0
         btt_auth_detail.reason                      =  "":U
         btt_auth_detail.fixed_item_cost             =  0
         btt_auth_detail.default_claim_code_source   =  "":U
         btt_auth_detail.copay_auth_%                =  0   .

     VALIDATE btt_auth_detail.

  RETURN TRUE.

  { mip/inc/mipcatcherror.i }

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnResetAuthProviderValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnResetAuthProviderValues Procedure
FUNCTION fnResetAuthProviderValues RETURNS LOGICAL
  ( BUFFER btt_auth_provider FOR tt_auth_provider,
    BUFFER bttAuthTypeConfig FOR ttAuthTypeConfig) :
/*------------------------------------------------------------------------------
  Purpose: Clear auth provider buffer fields that we do not intend to copy over
           for the auth copy
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iDefaultClaimCode AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cDefaultClaimType AS CHARACTER NO-UNDO.

  ASSIGN iDefaultClaimCode = INTEGER(ENTRY(1,bttAuthTypeConfig.ClaimCodes))
         cDefaultClaimType = ENTRY(1,bttAuthTypeConfig.ClaimTypes) .

  ASSIGN btt_auth_provider.adjustment_auth         = 0
         btt_auth_provider.adjustment_paid         = 0
         btt_auth_provider.adjustment_private_auth = 0
         btt_auth_provider.adjustment_private_paid = 0
         btt_auth_provider.amount_auth             = 0
         btt_auth_provider.amount_claimed          = 0
         btt_auth_provider.amount_interim          = 0
         btt_auth_provider.amount_paid             = 0
         btt_auth_provider.amount_requested        = 0
         btt_auth_provider.amount_total            = 0
         btt_auth_provider.auth_copay_control_obj  = 0
         btt_auth_provider.benefit_%               = 0
         btt_auth_provider.copay_auth              = 0
         btt_auth_provider.copay_auth_%            = 0
         btt_auth_provider.copay_paid              = 0
         btt_auth_provider.copay_provider          = FALSE
         btt_auth_provider.default_ars_rate        = "":U
         btt_auth_provider.default_base_rate       = "":U
         btt_auth_provider.default_claim_code      = 580 //iDefaultClaimCode
         btt_auth_provider.default_claim_type      = "C" //cDefaultClaimType
         btt_auth_provider.discount_auth           = ?
         btt_auth_provider.discount_paid           = ?
         btt_auth_provider.discount_type           = FALSE
         btt_auth_provider.override_ars_rate       = "":U
         btt_auth_provider.override_base_rate      = "":U
         btt_auth_provider.quantity_auth           = 0
         btt_auth_provider.quantity_claimed        = 0
         btt_auth_provider.quantity_paid           = 0
         btt_auth_provider.quantity_requested      = 0
         btt_auth_provider.rate_change_type        = "":U.

  VALIDATE btt_auth_provider.

  RETURN TRUE.

  { mip/inc/mipcatcherror.i }

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnResetAuthValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnResetAuthValues Procedure
FUNCTION fnResetAuthValues RETURNS LOGICAL
  ( BUFFER btt_auth          FOR tt_auth,
    BUFFER bttAuthTypeConfig FOR ttAuthTypeConfig) :
/*------------------------------------------------------------------------------
  Purpose: Clear auth buffer fields that we do not intend to copy over
           for the auth copy
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iDefaultClaimCode AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cDefaultClaimType AS CHARACTER NO-UNDO.

  ASSIGN iDefaultClaimCode = INTEGER(ENTRY(1,bttAuthTypeConfig.ClaimCodes))
         cDefaultClaimType =         ENTRY(1,bttAuthTypeConfig.ClaimTypes) NO-ERROR .

  IF {&ErrorStatus} THEN
  DO:
    ASSIGN iDefaultClaimCode = 0
           cDefaultClaimType = "":U .
  END. // IF {&ErrorStatus}

  ASSIGN btt_auth.adjustment_auth         = 0
         btt_auth.adjustment_paid         = 0
         btt_auth.adjustment_private_auth = 0
         btt_auth.adjustment_private_paid = 0
         btt_auth.amount_auth             = 0
         btt_auth.amount_claimed          = 0
         btt_auth.amount_interim          = 0
         btt_auth.amount_paid             = 0
         btt_auth.benefit_%               = 0
         btt_auth.claim_code              = iDefaultClaimCode
         btt_auth.claim_type              = cDefaultClaimType
         btt_auth.default_claim_code      = 580 // iDefaultClaimCode
         btt_auth.default_claim_type      = "C" // cDefaultClaimType
         btt_auth.discount_paid           = 0
         btt_auth.quantity_paid           = 0.

  VALIDATE btt_auth.

  RETURN TRUE.

  { mip/inc/mipcatcherror.i }

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnUserHasRole) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnUserHasRole Procedure
FUNCTION fnUserHasRole RETURNS LOGICAL
  ( ipcRoleCodeList AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRole     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lHasRole  AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRoleCode AS CHARACTER   NO-UNDO.


  RoleBlk:
  DO iRole = 1 TO NUM-ENTRIES(ipcRoleCodeList):

    ASSIGN cRoleCode = ENTRY(iRole, ipcRoleCodeList)

           lHasRole  = mipEnv:miUser:hasRole(TRIM(cRoleCode)).

    IF lHasRole
    THEN
      LEAVE RoleBlk.

  END. /*DO iRole = 1 TO NUM-ENTRIES(ipcUserRoleCodeList):*/

  RETURN lHasRole.   /* Function return value. */

  { mip/inc/mipcatcherror.i }

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

