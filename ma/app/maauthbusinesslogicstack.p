&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*----------------------------------------------------------------------------------
    Purpose: Healthcare Authorisation Business Logic Stack

    Author : MMP


  ----------------------------------------------------------------------------------*/

/* ----------------------------------  Definitions  ------------------------------- */
USING Progress.Json.ObjectModel.*   FROM PROPATH .

CREATE WIDGET-POOL.

{ sysadmma.i }

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthds.i                 }
{ ma/inc/mamemberds.i               }
{ ma/inc/maauthtypeds.i             }
{ ma/inc/madecisionauthhighriskds.i }
{ ma/inc/maauthruleds.i             }
{ ma/inc/maauthflagvalueds.i        }
{ ma/inc/macptds.i                  }
{ ma/inc/maexternalreferenceds.i    }
{ ma/inc/matariffds.i               }
{ ma/inc/maauthtypeconfigtt.i       }
{ ma/inc/madephealthds.i            }
{ ma/inc/maclinicaldocds.i          }
{ ma/inc/manappifieldvalues.i       }
{ ma/inc/madeplimittt.i             }
{ ma/inc/madeplimitwarntt.i         }

{ ma/inc/matariffconditiontt.i  &TEMP-TABLE-NAME = tt_tariff_condition }

{ ma/inc/maextreftt.i &TEMP-TABLE-NAME = tt_auth_extref }

{ mip/inc/mipgetttords.i &DataSet-01 = dsAuthorisation }

DEFINE VARIABLE goAuthorisation               AS cls.maauthorisation NO-UNDO.
DEFINE VARIABLE goErrorObject                 AS cls.maerrorobject   NO-UNDO.
DEFINE VARIABLE glSuccess                     AS LOGICAL             NO-UNDO.
DEFINE VARIABLE glSkipDetailLineValuesProcess AS LOGICAL             NO-UNDO.

DEFINE BUFFER buf_auth_schext FOR schext.

DEFINE TEMP-TABLE tt_usage_processing
         FIELD owning_obj             AS DECIMAL
         FIELD total_quantity_auth    AS DECIMAL
         FIELD auth_detail_code_list  AS CHARACTER
         FIELD owning_entity_mnemonic AS CHARACTER.

DEFINE TEMP-TABLE ttFilter NO-UNDO
  FIELD AuthObj AS DECIMAL
  INDEX idx1 AuthObj.

DEFINE TEMP-TABLE tt_parent_error NO-UNDO
  FIELD owning_obj AS DECIMAL.

DEFINE TEMP-TABLE tt-additionalicd
   FIELD diagnosis  LIKE dclaim.diagnosis
   FIELD ass-code   LIKE dclaim.ass-code
   FIELD morph-code LIKE dclaim.morph-code.

/* A Temp table to store the ICD/CPT coding type pairs. */
DEFINE TEMP-TABLE tt_icd_cpt_pairs NO-UNDO
   FIELD icd_coding_type AS CHARACTER FORMAT "X(30)"
   FIELD cpt_coding_type AS CHARACTER FORMAT "X(30)"

   INDEX ttIcdCptPairs1
     icd_coding_type

  INDEX ttIcdCptPairs2
     cpt_coding_type.

DEFINE TEMP-TABLE tt_flagdet
  FIELD flag_num          AS INTEGER
  FIELD flag_description  AS CHARACTER
  FIELD flag_value        AS CHARACTER
  FIELD effective_date    AS DATE
  FIELD end_date          AS DATE.

DEFINE TEMP-TABLE tt_provider_count
  FIELD auth_obj          AS DECIMAL
  FIELD provider_type     AS CHARACTER
  FIELD provider_count    AS INTEGER
  INDEX ttProvCnt
    auth_obj
    provider_type.

DEFINE TEMP-TABLE tt_auth_ccct
  FIELD claim_code             AS INTEGER   FORMAT "999"
  FIELD claim_type             AS CHARACTER FORMAT "x(10)"
  FIELD owning_entity_mnemonic AS CHARACTER FORMAT "x(5)"
  FIELD owning_obj             AS DECIMAL   FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_alt_value       AS CHARACTER FORMAT "x(20)"
  FIELD amount_auth            AS DECIMAL   FORMAT "->,>>>,>>9.99"
  FIELD quantity_auth          AS DECIMAL   FORMAT "->,>>>,>>9.99"
INDEX idx_ccct AS PRIMARY UNIQUE
   claim_code
   claim_type.

&SCOPED-DEFINE ActionList delete,modify,partialsave

&SCOPED-DEFINE ModifyList modify,partialsave

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnAddError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnAddError Procedure
FUNCTION fnAddError RETURNS LOGICAL
  ( ipdObj AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnAllowLineStatusChange) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnAllowLineStatusChange Procedure
FUNCTION fnAllowLineStatusChange RETURNS LOGICAL
  ( ipiOldAuthHeaderStatus   AS INTEGER,
    ipcOldAuthHeaderNote     AS CHARACTER,
    ipiNewAuthHeaderStatus   AS INTEGER,
    ipcNewAuthHeaderNote     AS CHARACTER,
    ipiAuthLineStatus        AS INTEGER,
    ipcAuthLineNote          AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnClearErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnClearErrors Procedure
FUNCTION fnClearErrors RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnErrorOccurred) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnErrorOccurred Procedure
FUNCTION fnErrorOccurred RETURNS LOGICAL
  ( ipdObj AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnInvalidLineStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnInvalidLineStatus Procedure
FUNCTION fnInvalidLineStatus RETURNS CHARACTER
  ( INPUT ipdAuthObj AS DECIMAL,
    INPUT ipiStatus  AS INTEGER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnBuildCC-CT-List) = 0 &THEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnBuildCC-CT-List Procedure
FUNCTION fnBuildCC-CT-List RETURNS CHARACTER
  ( BUFFER ttAuthTypeConfig FOR ttAuthTypeConfig  )  FORWARD.
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
         HEIGHT             = 25.71
         WIDTH              = 62.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-activateEmergencyFlag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateEmergencyFlag Procedure
PROCEDURE activateEmergencyFlag :
/*------------------------------------------------------------------------------
  Purpose:  Return logical whether emergency flag should be enabled on the auth type
  Parameters:  ipdAuthTypeObj
               ipdInsurerObj
               ipiOptionCode
               ipdStartDate
               oplActivateEmergencyFlag

  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthTypeObj             AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj              AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode              AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate               AS DATE        NO-UNDO.
  DEFINE OUTPUT PARAMETER oplActivateEmergencyFlag   AS LOGICAL     NO-UNDO.

  EMPTY TEMP-TABLE ttAuthTypeConfig.

  mipEnv:Health:AuthService:getAuthTypeConfig (INPUT ipdAuthTypeObj,
                                               INPUT ipdInsurerObj,
                                               INPUT ipiOptionCode,
                                               INPUT ipdStartDate,
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).

  FIND FIRST ttAuthTypeConfig NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

  IF AVAILABLE ttAuthTypeConfig
  AND (    ttAuthTypeConfig.ActivateCopayment
        OR ttAuthTypeConfig.ActivatePenalty )
  THEN
    ASSIGN oplActivateEmergencyFlag = TRUE .
  ELSE
    ASSIGN oplActivateEmergencyFlag = FALSE.

  { mip/inc/mipcatcherror.i }
END PROCEDURE.  //activateEmergencyFlag

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-activateMainCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateMainCode Procedure
PROCEDURE activateMainCode :
/*-----------------------------------------------------------------------------
  Purpose   : Returns a logical indicating whether the Main Code indicator
              on table hat_auth_coding must be enabled in the UI
  Parameters: Insurer Object
              Option Code
              Auth Date

  Notes     :
 ------------------------------------------------------------------------------ */
  DEFINE INPUT  PARAMETER ipdInsurerObj       AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode       AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate             AS DATE        NO-UNDO.
  DEFINE OUTPUT PARAMETER oplEnableMainCode   AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE lValidRule                  AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRuleValue                  AS CHARACTER   NO-UNDO.

  /*
     Get the 'CodingMainCode' rule
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                 INPUT  "CodingMainCode":U,
                                                 INPUT  ipdDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  /*
     If the rule is not set up, then the CodingMainCode indicator is disabled
  */
  IF lValidRule THEN
  DO:
    IF ENTRY(1,cRuleValue,"|":U) = "Activate":U
    THEN ASSIGN oplEnableMainCode = TRUE.
    ELSE ASSIGN oplEnableMainCode = FALSE.
  END.  /* IF lValidRule THEN */
  ELSE
    ASSIGN oplEnableMainCode = FALSE.

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-activateMainProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateMainProvider Procedure
PROCEDURE activateMainProvider :
/*-----------------------------------------------------------------------------
  Purpose   : Returns a logical indicating whether the Main Provider indicator
              on table hat_auth_provider must be enabled in the UI by checking
              whether the selected auth type has a default provider flagged as
              main provider or not.
  Parameters: Auth Type Obj
              Insurer Obj
              Option Code
              Auth Date

  Notes     :
 ------------------------------------------------------------------------------ */
  DEFINE INPUT  PARAMETER ipdAuthTypeObj    AS  DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj     AS  DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode     AS  INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdEffectiveDate  AS  DATE       NO-UNDO.
  DEFINE OUTPUT PARAMETER oplEnabled        AS  LOGICAL    NO-UNDO.

  DEFINE VARIABLE oSearch  AS cls.maauthtypesearch NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL              NO-UNDO.


  DATASET dsAuthType:EMPTY-DATASET().


  IF ipdAuthTypeObj   <> 0.00 AND
     ipdEffectiveDate <> ?
  THEN
  DO:
    oSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).

    ASSIGN lSuccess   = oSearch:fetchAuthType(INPUT ipdAuthTypeObj,
                                              INPUT ipdInsurerObj,
                                              INPUT ipiOptionCode,
                                              INPUT ipdEffectiveDate,
                                              INPUT "":U,
                                              INPUT "":U  )

           oplEnabled = NOT CAN-FIND(FIRST tt_auth_type_provider NO-LOCK
                                     WHERE tt_auth_type_provider.auth_type_obj = ipdAuthTypeObj
                                       AND tt_auth_type_provider.main_provider = TRUE).

  END. /*IF ipdAuthTypeObj <> 0.00 THEN*/

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch." }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-activateMinutes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateMinutes Procedure
PROCEDURE activateMinutes :
/*-----------------------------------------------------------------------------
  Purpose   : Returns a logical indicating whether the minutes must be activated
              when a specific tariff code id captured.
  Parameters:
      Input Parameters: ipdInsurerObj
                        ipiOptionCode
                        ipdStartDate
                        ipdTariffLinkObj
      Ouput Parameters: opcError
                        opdMinutesTariffTypeObj
                        oplActivateMinutes

  Notes     :
 ------------------------------------------------------------------------------ */
  DEFINE INPUT  PARAMETER ipdInsurerObj            AS  DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode            AS  INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate             AS  DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER ipdTariffLinkObj         AS  DECIMAL    NO-UNDO.

  DEFINE OUTPUT PARAMETER opcError                 AS  CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER opdMinutesTariffTypeObj  AS  DECIMAL    NO-UNDO.
  DEFINE OUTPUT PARAMETER oplActivateMinutes       AS  LOGICAL    NO-UNDO.

  DEFINE VARIABLE iCount                           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lSuccess                         AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cError                           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTariffTypeList                  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTariffTypeObjList               AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lValidRule                       AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRuleValue                       AS CHARACTER   NO-UNDO.


  /* Get all tariff types to which the tariff may link. */
  mipEnv:health:maMedical:getTariffTypes(INPUT  ipdTariffLinkObj ,   /* ipdTariffLinkObj     */
                                         INPUT  ipdStartDate,        /* ipdDate              */
                                         INPUT  "":U ,               /* ipcCategory          */
                                         OUTPUT cTariffTypeList,     /* opcTariffTypeList    */
                                         OUTPUT cTariffTypeObjList,  /* opcTariffTypeObjList */
                                         OUTPUT cError ).            /* opcError             */

  IF cError <> "":U THEN
  DO:
    ASSIGN opcError = cError.
    LEAVE.
  END. /* IF cError <> "":U */

  /* Determine if minutes must be activated for the tariff types received back from getTariffTypes. */
  ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                                   INPUT  ipiOptionCode,
                                                                   INPUT  "ma_acAuthRuleTypeMinCalc":U,
                                                                   INPUT  "EnforceMinutes":U,
                                                                   INPUT  ipdStartDate,
                                                                   OUTPUT lValidRule,
                                                                   OUTPUT cRuleValue).

  IF NOT lValidRule THEN
  DO:
    ASSIGN opdMinutesTariffTypeObj = 0.00
           oplActivateMinutes      = NO.
    LEAVE.
  END. /* IF NOT lValidRule THEN  */

  IF lValidRule AND cRuleValue = "":U THEN
  DO:
    ASSIGN opdMinutesTariffTypeObj = 0.00
           oplActivateMinutes      = NO
           opcError                = "The rule value for Rule: 'EnforceMinutes' is missing, rule setup invalid.".
    LEAVE.
  END. /* IF lValidRule AND cRuleValue = "":U THEN*/

  /* Loop through the list of tariff types (cTariffTypeList) to determine if minutes must be activated. */
  IF cTariffTypeList <> "":U THEN
  DO iCount = 1 TO NUM-ENTRIES(cTariffTypeList):
    FIND FIRST htm_tariff_type NO-LOCK
         WHERE htm_tariff_type.tariff_type_code  = ENTRY(iCount,cTariffTypeList)
      NO-ERROR.

    IF AVAILABLE htm_tariff_type AND LOOKUP(htm_tariff_type.acronym_key, cRuleValue) > 0
    THEN DO:
      ASSIGN oplActivateMinutes      = YES
             opdMinutesTariffTypeObj = DECIMAL(ENTRY(iCount,cTariffTypeObjList)).
    END.

  END. /* Do iCount = 1 to NUM-ENTRIES(cTariffTypeList) - CHECKTARIFFTYPECAT. */


{ mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-activatePenaltyFlag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activatePenaltyFlag Procedure
PROCEDURE activatePenaltyFlag :
/*------------------------------------------------------------------------------
  Purpose:  Return logical whether penalty flag should be enabled on the auth type
  Parameters:  ipdAuthTypeObj
               ipdInsurerObj
               ipiOptionCode
               ipdStartDate
               oplActivatePenaltyFlag

  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthTypeObj             AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj              AS DECIMAL     NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode              AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate               AS DATE        NO-UNDO.
  DEFINE OUTPUT PARAMETER oplActivatePenaltyFlag     AS LOGICAL     NO-UNDO.

  EMPTY TEMP-TABLE ttAuthTypeConfig.

  mipEnv:Health:AuthService:getAuthTypeConfig (INPUT ipdAuthTypeObj,
                                               INPUT ipdInsurerObj,
                                               INPUT ipiOptionCode,
                                               INPUT ipdStartDate,
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).

  FIND FIRST ttAuthTypeConfig NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

  IF AVAILABLE ttAuthTypeConfig
  AND ttAuthTypeConfig.ActivatePenalty
  THEN
    ASSIGN oplActivatePenaltyFlag = TRUE .
  ELSE
    ASSIGN oplActivatePenaltyFlag = FALSE.


  { mip/inc/mipcatcherror.i }
END PROCEDURE.  //activatePenaltyFlag

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-activateProcedureDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateProcedureDate Procedure
PROCEDURE activateProcedureDate :
/*------------------------------------------------------------------------------
  Purpose: Method to determine if the procedure date must be captured.
  Parameters: Insurer Obj
              Option Code
              Auth Date
              Cpt code
              Cpt service type
              oplEnableProcedureDate
              opcProcedureDateAction
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj           AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode           AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate                 AS DATE      NO-UNDO.
  DEFINE INPUT  PARAMETER ipcCptCode              AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcServiceType          AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplEnableProcedureDate  AS LOGICAL   NO-UNDO INITIAL FALSE.
  DEFINE OUTPUT PARAMETER opcProcedureDateAction  AS CHARACTER NO-UNDO INITIAL "":U.

  DEFINE VARIABLE lValidRule              AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lSuccess                AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE cRuleValue              AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cServiceType            AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cServiceTypeAcronym     AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE oCPSearch               AS ma.cls.macptsearch NO-UNDO.
  DEFINE VARIABLE oServiceType            AS cls.mipacronym     NO-UNDO.

  DEFINE BUFFER btt_cpt FOR tt_cpt.

  /*
     Get the 'Procedure Date' rule
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                 INPUT  "CPTProcedureDate":U,
                                                 INPUT  ipdDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  IF lValidRule THEN
  DO:

    IF ipcCptCode = "":U
    THEN
      ASSIGN
        oplEnableProcedureDate = TRUE
        opcProcedureDateAction = cRuleValue.

    IF (ENTRY(1,cRuleValue,"|") = "BOTH":U)
    THEN
      ASSIGN
        oplEnableProcedureDate = TRUE
        opcProcedureDateAction = cRuleValue.

    IF (ENTRY(1,cRuleValue,"|") <> "BOTH":U) AND ipcCptCode <> "":U THEN
    DO:
      IF ipcServiceType = "" THEN
      DO:

        /* We might already have a buffer populated for tt_cpt, in case this is being
           called from a procedure within the same stack, only fetch cpt if we don't */
        IF NOT CAN-FIND(FIRST btt_cpt WHERE btt_cpt.cpt_code = ipcCptCode) THEN
        DO:
          /* Get the CPT code details if CPT service type is not passed in */
          ASSIGN
            oCPSearch = NEW ma.cls.macptsearch(DATASET dsCPT:HANDLE)
            lSuccess  = oCPSearch:setCriteria("BufferList":U, "tt_cpt,tt_cpt_link":U)
            lSuccess  = oCPSearch:setFilterCriteria("tt_cpt_link.cpt_code":U, "=":U, ipcCptCode).

          oCPSearch:fetchCptData().
        END. /* IF NOT CAN-FIND(FIRST btt_cpt WHERE btt_cpt.cpt_code = ipcCptCode) */

        /* Get cpt with most recent effective date */

        CPTBlk:
        FOR EACH btt_cpt NO-LOCK
           WHERE btt_cpt.cpt_code        = ipcCptCode
             AND btt_cpt.effective_date <= ipdDate
             AND (btt_cpt.end_date       >= ipdDate
              OR btt_cpt.end_date        = ?)
              BY btt_cpt.effective_date DESCENDING:

          ASSIGN cServiceTypeAcronym = btt_cpt.service_type.

          LEAVE CPTBlk.
        END. /* CPTBlk */
      END. /* IF ipcServiceType = ""  */
      ELSE
        ASSIGN cServiceTypeAcronym = ipcServiceType .

      ASSIGN oServiceType = NEW cls.mipacronym(?, FALSE, "ma_acServiceType":U, ?).

      /* try to focus the acronym */
      oServiceType:focusAcronym("KEY":U, cServiceTypeAcronym) NO-ERROR.

      IF oServiceType:AcronymInFocus
      THEN
        ASSIGN cServiceType = oServiceType:AcronymLabel.
      ELSE
        ASSIGN cServiceType = ipcServiceType.

      IF cServiceType = ENTRY(1,cRuleValue,"|")
      THEN
        ASSIGN
          oplEnableProcedureDate = TRUE
          opcProcedureDateAction = cRuleValue.
      ELSE
        ASSIGN
          oplEnableProcedureDate = FALSE
          opcProcedureDateAction = "".

    END. /* IF (ENTRY(1,cRuleValue) <> "BOTH":U) AND ipcCptCode <> "":U */
  END. /* IF lValidRule  */
  ELSE
    ASSIGN
      oplEnableProcedureDate = FALSE
      opcProcedureDateAction = "".

  { mip/inc/mipcatcherror.i
    &FINALLY="IF VALID-OBJECT(oCPSearch)    THEN DELETE OBJECT oCPSearch.    ~{mip/inc/mipmessageerror.i~}
              IF VALID-OBJECT(oServiceType) THEN DELETE OBJECT oServiceType. ~{mip/inc/mipmessageerror.i~}"}

END PROCEDURE. /* activateProcedureDate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-authDependantUpdatable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authDependantUpdatable Procedure
PROCEDURE authDependantUpdatable :
/*------------------------------------------------------------------------------
  Purpose:     Returns a logical indicating whether the authorisation dependant
               is updatable or not
  Parameters:  ipdInsurerObj
               ipdAuthTypeObj
               ipiOptionCode
               ipdDate
               ipdAmountPaid   -> The hat_auth.amount_paid value or similar field on the UI
               ipdQuantityPaid -> The hat_auth.quantity_paid value or similar field on the UI
               oplAllowUpdate
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthTypeObj  AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate         AS DATE      NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAmountPaid   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdQuantityPaid AS DECIMAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER oplAllowUpdate  AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE oAuthType            AS cls.maauthtype NO-UNDO.
  DEFINE VARIABLE lValidRule              AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRuleValue              AS CHARACTER   NO-UNDO.


  ASSIGN oAuthType      = NEW cls.maauthtype(ipdAuthTypeObj).

  IF oAuthType:AuthTypeInFocus THEN
  DO:
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                   INPUT  ipiOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeAuthCopy":U,
                                                   INPUT  "AuthMoveValidTypes":U,
                                                   INPUT  ipdDate,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).

    IF LOOKUP(oAuthType:AuthType, cRuleValue, "|") > 0 THEN
        ASSIGN oplAllowUpdate = TRUE.
    ELSE
        ASSIGN oplAllowUpdate = FALSE.
  END. /* IF oAuthType:AuthTypeInFocus THEN */


  /*
     If the rule is not set up or disabled, then the dependant is only
     updatable as long as no claims were processed against the authorisation.
  */
  IF oplAllowUpdate
  AND (ipdAmountPaid <> 0 OR ipdQuantityPaid <> 0)
  THEN
    ASSIGN oplAllowUpdate = FALSE.

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-authNextOfKinActivated) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authNextOfKinActivated Procedure
PROCEDURE authNextOfKinActivated :
/*-----------------------------------------------------------------------------
  Purpose   : Returns a logical indicating whether the next of kin is activated
  Parameters:
  Notes     :
 ------------------------------------------------------------------------------ */
  DEFINE INPUT  PARAMETER ipdInsurerObj          AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode          AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate                AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER oplNOKActivated        AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE lValidRule              AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRuleValue              AS CHARACTER   NO-UNDO.

  /*
     Get the 'Episode' rule
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                 INPUT  "NextOfKin":U,
                                                 INPUT  ipdDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  /*
     If the rule is not set up, then the Episode is not updatable
  */
  IF lValidRule THEN
  DO:
    IF cRuleValue = "Activate" THEN
      ASSIGN oplNOKActivated = TRUE.
    ELSE
      ASSIGN oplNOKActivated = FALSE.
  END.  /* IF lValidRule THEN */
  ELSE
    ASSIGN oplNOKActivated = FALSE.

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-authNumViewable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authNumViewable Procedure
PROCEDURE authNumViewable :
/*-----------------------------------------------------------------------------
  Purpose   : Returns a logical indicating whether the authorisation can be
              displayed or not.
  Parameters: Authorisation Status
              Insurer Object
              Option Code
              Auth Date

  Notes     :
 ------------------------------------------------------------------------------ */
  DEFINE INPUT  PARAMETER ipiStatusCode       AS INTEGER          NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj       AS DECIMAL          NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode       AS INTEGER          NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthDate         AS DATE             NO-UNDO.
  DEFINE OUTPUT PARAMETER oplAuthNumViewable  AS LOGICAL          NO-UNDO.

  DEFINE VARIABLE dAuthDate                   AS DATE             NO-UNDO.
  DEFINE VARIABLE lValidRule                  AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE cRuleValue                  AS CHARACTER        NO-UNDO.

  IF ipdAuthDate = ?
  THEN dAuthDate = TODAY.
  ELSE dAuthDate = ipdAuthDate.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                 INPUT  "AuthNumViewable":U,
                                                 INPUT  dAuthDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).


  IF lValidRule AND LOOKUP(STRING(ipiStatusCode), cRuleValue) > 0
  THEN ASSIGN oplAuthNumViewable = FALSE.
  ELSE ASSIGN oplAUthNumViewable = TRUE.

 { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-authOptionMandatory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authOptionMandatory Procedure
PROCEDURE authOptionMandatory :
/*------------------------------------------------------------------------------
  Purpose   : Returns TRUE if the option code on the auth is mandatory
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipiOptionCode AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER oplMandatory  AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE lValidRule            AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER NO-UNDO.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                 INPUT  "RESERVELIMITS":U,
                                                 INPUT  TODAY,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  ASSIGN oplMandatory = lValidRule AND LOOKUP(cRuleValue,"Yes,Y,True,T") > 0.

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-authRefNumUpdatable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authRefNumUpdatable Procedure
PROCEDURE authRefNumUpdatable :
/*-----------------------------------------------------------------------------
  Purpose   : Returns a logical indicating whether the authorisation
              reference number is updatable or not
  Parameters: Auth Type Obj
              Insurer Obj
              Option Code
              Effective Date

  Notes     :
 ------------------------------------------------------------------------------ */
  DEFINE INPUT  PARAMETER ipdAuthTypeObj   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj    AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdEffectiveDate AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER oplAllowUpdate   AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE oAuthType    AS cls.maauthtype NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE lAllowUpdate AS LOGICAL        NO-UNDO.
  DEFINE VARIABLE lValidRule   AS LOGICAL        NO-UNDO.
  DEFINE VARIABLE cRuleValue   AS CHARACTER      NO-UNDO.

  ASSIGN oAuthType      = NEW cls.maauthtype(ipdAuthTypeObj)
         oplAllowUpdate = FALSE.

  IF oAuthType:AuthTypeInFocus THEN
  DO:
    FIND FIRST uusr NO-LOCK
         WHERE uusr.id = mipEnv:miUser:UserCode
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    IF AVAILABLE uusr AND uusr.assessor-type = "H":U THEN
    DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                     INPUT  ipiOptionCode,
                                                     INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                     INPUT  "AUTHREFNUM":U,
                                                     INPUT  ipdEffectiveDate,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
      ASSIGN oplAllowUpdate = lValidRule AND LOOKUP(oAuthType:AuthType, cRuleValue) > 0.

    END. /*IF AVAILABLE uusr AND uusr.assessor-type = "H":U THEN */
  END. /*IF oAuthType:AuthTypeInFocus THEN */

  IF oplAllowUpdate THEN
  DO:
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                   INPUT  ipiOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                   INPUT  "AUTHREFNUMGENERATE":U,
                                                   INPUT  ipdEffectiveDate,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).
    /*
       If the "AuthRefNumGenerate"-rule exists the reference_auth_num should not be updateable,
       even if it is set up on AuthRefNum, because the Episode + Authorisation number will be
       saved in the reference_auth_num and if the user capture or update the field the value
       will be overridden.
    */
    IF lValidRule
    THEN
      ASSIGN oplAllowUpdate = FALSE.
  END.  /* IF oplAllowUpdate THEN */

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oAuthType) THEN DELETE OBJECT oAuthType."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-authServiceTypeUpdatable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authServiceTypeUpdatable Procedure
PROCEDURE authServiceTypeUpdatable :
/*------------------------------------------------------------------------------
  Purpose: Method must indicate if the service type can be updated or if it must be
           displayed only
  Parameters:  InsurerObj
               OptionCode
               Date
               Service Type Updatable (Y/N)
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj             AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode             AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate                   AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER oplServiceTypeAllowUpdate AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE lCPTMandatoryValidRule            AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cCPTMandatoryRuleValue            AS CHARACTER NO-UNDO.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                 INPUT  "CPTMandatory":U,
                                                 INPUT  ipdDate,
                                                 OUTPUT lCPTMandatoryValidRule,
                                                 OUTPUT cCPTMandatoryRuleValue).

  ASSIGN oplServiceTypeAllowUpdate = NOT lCPTMandatoryValidRule .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-authUpdatesAllowed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authUpdatesAllowed Procedure
PROCEDURE authUpdatesAllowed :
/*-----------------------------------------------------------------------------
  Purpose   : Returns a logical indicating whether updates are allowed on the
              Authorisation or not.
  Parameters: AuthObj
              AuthTypeObj
              Insurer Object
              Option Code
              Auth Date

  Notes     :
 ------------------------------------------------------------------------------ */
  DEFINE INPUT  PARAMETER ipdAuthObj         AS DECIMAL                  NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthTypeObj     AS DECIMAL                  NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj      AS DECIMAL                  NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode      AS INTEGER                  NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthDate        AS DATE                     NO-UNDO.
  DEFINE OUTPUT PARAMETER oplUpdatesAllowed  AS LOGICAL                  NO-UNDO.
  DEFINE OUTPUT PARAMETER opcValidMessage    AS CHARACTER                NO-UNDO.

  DEFINE VARIABLE oAuthTypeSearch            AS cls.maauthtypesearch     NO-UNDO.
  DEFINE VARIABLE oExternalRef               AS cls.maexternalrefsearch  NO-UNDO.
  DEFINE VARIABLE dAuthDate                  AS DATE                     NO-UNDO.
  DEFINE VARIABLE lValidRule                 AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE cRuleValue                 AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL                  NO-UNDO.

  DEFINE VARIABLE cMessageText               AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cMessageDescription        AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cMessageHelp               AS CHARACTER                NO-UNDO.

  IF ipdAuthDate <> ?
  THEN ASSIGN dAuthDate = ipdAuthDate.
  ELSE ASSIGN dAuthDate = TODAY.

  DATASET dsAuthType:EMPTY-DATASET().
  DATASET dsExternalReference:EMPTY-DATASET().

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeExtRef":U,
                                                 INPUT  "UpdatesNotAllowed":U,
                                                 INPUT  dAuthDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  mipEnv:miUtility:getMessageText(INPUT "ma_MsgAuth":U,INPUT "30":U, INPUT "ENG":U, OUTPUT cMessageText, OUTPUT cMessageDescription, OUTPUT cMessageHelp).

  ASSIGN
    oAuthTypeSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE)
    lSuccess        = oAuthTypeSearch:setCriteria("Bufferlist":U, "tt_auth_type":U)
    lSuccess        = oAuthTypeSearch:setFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, ipdAuthTypeObj).

  oAuthTypeSearch:fetchData().

  FIND FIRST tt_auth_type NO-LOCK
       WHERE tt_auth_type.auth_type_obj = ipdAuthTypeObj
   NO-ERROR.
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT lValidRule OR (lValidRule AND cRuleValue = "":U)
  THEN DO:

    IF AVAILABLE tt_auth_type THEN
    DO:
      IF tt_auth_type.updates_allowed = TRUE
      THEN ASSIGN oplUpdatesAllowed   = TRUE.
      ELSE ASSIGN oplUpdatesAllowed   = FALSE
                  opcValidMessage     = cMessageText.
    END. /* IF AVAILABLE tt_auth_type THEN */

  END. /* IF NOT lValidRule OR (lValidRule AND cRuleValue = "":U) */
  ELSE DO:

    IF ipdAuthObj <> 0.00 THEN
    DO:
      ASSIGN
        oExternalRef = NEW cls.maexternalrefsearch(DATASET dsExternalReference BY-REFERENCE)
        lSuccess     = oExternalRef:SetCriteria("BufferList":U, "tt_extref,tt_schintref":U).


      oExternalRef:fetchExternalReferences(INPUT "hatau":U, INPUT ipdAuthObj, INPUT "":U).

      FIND FIRST tt_extref NO-LOCK NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE tt_extref AND LOOKUP(tt_extref.interface-type, cRuleValue) > 0
      THEN DO:

        ASSIGN
         oplUpdatesAllowed = FALSE
         opcValidMessage   = cMessageText.
      END. /* IF AVAILABLE tt_extref */
      ELSE DO:

        IF AVAILABLE tt_auth_type THEN
        DO:
          IF tt_auth_type.updates_allowed = TRUE
          THEN ASSIGN oplUpdatesAllowed   = TRUE.
          ELSE
            ASSIGN oplUpdatesAllowed   = FALSE
                   opcValidMessage     = cMessageText.
        END. /* IF AVAILABLE tt_auth_type THEN  */

      END. /* ELSE DO: */

    END. /* IF ipdAuthObj <> 0.00 THEN */
    ELSE DO:

      IF AVAILABLE tt_auth_type THEN
      DO:
        IF tt_auth_type.updates_allowed = TRUE
        THEN ASSIGN oplUpdatesAllowed   = TRUE.
        ELSE
          ASSIGN oplUpdatesAllowed   = FALSE
                 opcValidMessage     = cMessageText.
      END. /*IF AVAILABLE tt_auth_type THEN  */

    END. /*  ELSE DO:*/
  END. /* ELSE DO: */

  { mip/inc/mipcatcherror.i
    &FINALLY="IF VALID-OBJECT(oAuthTypeSearch) THEN DELETE OBJECT oAuthTypeSearch.
              IF VALID-OBJECT(oExternalRef)    THEN DELETE OBJECT oExternalRef.

              DATASET dsAuthType:EMPTY-DATASET().
              DATASET dsExternalReference:EMPTY-DATASET()."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-buildAuthLimitTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildAuthLimitTT Procedure
PROCEDURE buildAuthLimitTT :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER dAuthObj  AS DECIMAL   NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

  DEFINE VARIABLE cMessage     AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE dDummyObj    AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE iDependant   AS INTEGER           NO-UNDO.

  DEFINE BUFFER btt_auth          FOR tt_auth.
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail   FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_ccct     FOR tt_auth_ccct.
  DEFINE BUFFER btt_limit         FOR tt_limit.
  DEFINE BUFFER btt_auth_limit    FOR tt_auth_limit.

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE) .

  // Build a temp table of the claim codes and types we have stored on this auth at the moment
  AUTH-BLK:
  FOR EACH btt_auth
    WHERE btt_auth.auth_obj = dAuthObj:

    IF NOT AVAILABLE buf_auth_schext
    OR buf_auth_schext.scheme-code <> btt_auth.option_code THEN
    DO:
      FIND FIRST buf_auth_schext NO-LOCK
        WHERE buf_auth_schext.scheme-code = btt_auth.option_code
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
    END.  //IF NOT AVAILABLE buf_auth_schext THEN

    IF NOT AVAILABLE buf_auth_schext
    OR btt_auth.claim_code = buf_auth_schext.claim-code[1]
    THEN
      NEXT AUTH-BLK.

    FIND FIRST btt_auth_ccct
         WHERE btt_auth_ccct.claim_code = btt_auth.claim_code
           AND btt_auth_ccct.claim_type = btt_auth.claim_type NO-ERROR .

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_ccct
    AND (btt_auth.amount_auth   <> 0
     OR  btt_auth.quantity_auth <> 0) THEN
    DO:

      CREATE btt_auth_ccct.

      ASSIGN btt_auth_ccct.claim_code             = btt_auth.claim_code
             btt_auth_ccct.claim_type             = btt_auth.claim_type
             btt_auth_ccct.owning_entity_mnemonic = "hatau":U
             btt_auth_ccct.owning_obj             = btt_auth.auth_obj
             btt_auth_ccct.owning_alt_value       = btt_auth.auth_num
             btt_auth_ccct.amount_auth            = btt_auth.amount_auth
             btt_auth_ccct.quantity_auth          = btt_auth.quantity_auth.

    END. //IF NOT AVAILABLE tt_auth_ccct THEN

    PROVIDER-BLK:
    FOR EACH btt_auth_provider
       WHERE btt_auth_provider.auth_obj        = btt_auth.auth_obj
         AND (btt_auth_provider.claim_code    <> btt_auth.claim_code
           OR btt_auth_provider.claim_type    <> btt_auth.claim_type)
         AND (btt_auth_provider.amount_auth   <> 0
           OR btt_auth_provider.quantity_auth <> 0 ):

      IF btt_auth_provider.claim_code <> buf_auth_schext.claim-code[1]
      THEN
        NEXT PROVIDER-BLK.

      IF NOT CAN-FIND(FIRST btt_auth_ccct
                      WHERE btt_auth_ccct.claim_code = btt_auth_provider.claim_code
                        AND btt_auth_ccct.claim_type = btt_auth_provider.claim_type ) THEN
      DO:

        CREATE btt_auth_ccct.

        ASSIGN btt_auth_ccct.claim_code             = btt_auth_provider.claim_code
               btt_auth_ccct.claim_type             = btt_auth_provider.claim_type
               btt_auth_ccct.owning_entity_mnemonic = "hatap":U
               btt_auth_ccct.owning_obj             = btt_auth_provider.auth_provider_obj
               btt_auth_ccct.owning_alt_value       = STRING(btt_auth_provider.doc_num )
               btt_auth_ccct.amount_auth            = btt_auth_provider.amount_auth
               btt_auth_ccct.quantity_auth          = btt_auth_provider.quantity_auth.

      END. //IF NOT CAN-FIND(FIRST btt_auth_ccct
    END. //PROVIDER-BLK

    DETAIL-BLK:
    FOR EACH btt_auth_detail
       WHERE btt_auth_detail.auth_obj        = btt_auth.auth_obj
         AND (btt_auth_detail.claim_code    <> btt_auth.claim_code
           OR btt_auth_detail.claim_type    <> btt_auth.claim_type)
         AND (btt_auth_detail.amount_auth   <> 0
           OR btt_auth_detail.quantity_auth <> 0 ):

      IF btt_auth_detail.claim_code <> buf_auth_schext.claim-code[1]
      THEN
        NEXT DETAIL-BLK.

      IF NOT CAN-FIND(FIRST btt_auth_ccct
                      WHERE btt_auth_ccct.claim_code = btt_auth_detail.claim_code
                        AND btt_auth_ccct.claim_type = btt_auth_detail.claim_type ) THEN
      DO:

        CREATE btt_auth_ccct.

        ASSIGN btt_auth_ccct.claim_code             = btt_auth_detail.claim_code
               btt_auth_ccct.claim_type             = btt_auth_detail.claim_type
               btt_auth_ccct.owning_entity_mnemonic = "hatad":U
               btt_auth_ccct.owning_obj             = btt_auth_detail.auth_detail_obj
               btt_auth_ccct.owning_alt_value       = btt_auth_detail.owning_alt_value
               btt_auth_ccct.amount_auth            = btt_auth_detail.amount_auth
               btt_auth_ccct.quantity_auth          = btt_auth_detail.quantity_auth.

      END. //IF NOT CAN-FIND(FIRST btt_auth_ccct
    END. //DETAIL-BLK

  END. // AUTH-BLK

  FIND FIRST btt_auth
       WHERE btt_auth.auth_obj = dAuthObj NO-ERROR.

  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  // Build the limit dataset according to the claim codes and types on this auth
  AUTH-CCCT-BLK:
  FOR EACH btt_auth_ccct:
&IF {&DBDFMA} >= 010195 &THEN
    mipEnv:Health:limitBusinessLogic:fetchAvailLimits(INPUT btt_auth.option_code,         // ipiSchemeCode
                                                      INPUT btt_auth.mem_num,             // ipcMemNum
                                                      INPUT btt_auth.dependant,           // ipiDependant
                                                      INPUT btt_auth.start_date,          // ipdDate
                                                      INPUT btt_auth_ccct.claim_code,     // ipiClaimCode
                                                      INPUT btt_auth_ccct.claim_type,     // ipcClaimType
                                                      INPUT "SERVICE",                    // ipcMenuOption
                                                      INPUT 0,                            // ipdBenefit
                                                      INPUT 0,                            // ipiQuantity
                                                      INPUT YES,                          // iplAuthorisation
                                                      INPUT YES,                          // iplCalcMaxBenefit
                                                      INPUT YES,                          // iplCalcLimitLeft
                                                      OUTPUT cMessage,                    // opcError
                                                      OUTPUT TABLE tt_limit          ).   // Output temp table that will contain limit details.
&ENDIF
    IF cMessage <> "":U THEN
    DO:
      ASSIGN glSuccess = oErrorObject:addError(INPUT "hatau":U,             // ipcOwningEntityMnemonic
                                               INPUT btt_auth.auth_obj,     // ipdOwningEntityObj
                                               INPUT "":U,                  // ipcOwningEntityKey
                                               INPUT "limit_#",             // ipcFieldName
                                               INPUT btt_auth.line_number,  // ipiLineNumber
                                               INPUT cMessage ,             // ipcMessageText
                                               INPUT "ERR":U).              // ipcMessageType


    END.  // IF cMessage <> "" THEN

    BUILD-AUTH-LIMITS-BLK:
    FOR EACH btt_limit:

      ASSIGN iDependant = IF btt_limit.rate_per = "F":U
                          THEN 0
                          ELSE btt_auth.dependant.
      // Now that we have the limits , create the tt_auth_limit records that will be saved against the auth
      FIND FIRST btt_auth_limit
           WHERE btt_auth_limit.auth_obj       = btt_auth.auth_obj
           AND   btt_auth_limit.option_code    = btt_auth.option_code
           AND   btt_auth_limit.dependant      = iDependant
           AND   btt_auth_limit.limit_#        = btt_limit.limit_#
           AND   btt_auth_limit.effective_date = btt_limit.effective_date
           AND   btt_auth_limit.claim_code     = btt_limit.claim_code
           AND   btt_auth_limit.claim_type     = btt_limit.claim_type
        NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE btt_auth_limit
                    OR btt_auth_limit.limit_avail    <> btt_limit.limit_avail
                    OR btt_auth_limit.limit_used     <> btt_limit.limit_used
                    OR btt_auth_limit.limit_reserved <> btt_limit.limit_reserved THEN
      DO:

        IF NOT AVAILABLE btt_auth_limit THEN
        DO:
          CREATE btt_auth_limit.

          ASSIGN dDummyObj                             = dDummyObj - 1
                 btt_auth_limit.auth_limit_obj         = dDummyObj
                 btt_auth_limit.auth_obj               = btt_auth.auth_obj
                 btt_auth_limit.option_code            = btt_auth.option_code
                 btt_auth_limit.dependant              = iDependant
                 btt_auth_limit.limit_#                = btt_limit.limit_#
                 btt_auth_limit.effective_date         = btt_limit.effective_date
                 btt_auth_limit.claim_code             = btt_limit.claim_code
                 btt_auth_limit.claim_type             = btt_limit.claim_type
                 btt_auth_limit.rate_per               = btt_limit.rate_per
                 btt_auth_limit.owning_entity_mnemonic = btt_auth_ccct.owning_entity_mnemonic
                 btt_auth_limit.owning_obj             = btt_auth_ccct.owning_obj
                 btt_auth_limit.owning_alt_value       = btt_auth_ccct.owning_alt_value.
        END. // IF NOT AVAILABLE btt_auth_limit

        ASSIGN btt_auth_limit.limit_avail            = btt_limit.limit_avail
               btt_auth_limit.limit_used             = btt_limit.limit_used
               btt_auth_limit.limit_reserved         = btt_limit.limit_reserved
               btt_auth_limit.fetch_date_time        = NOW
               btt_auth_limit.record_action          = "MODIFY":U NO-ERROR.

        VALIDATE btt_auth_limit.

      END.  // IF NOT AVAILABLE btt_auth_limit…
    END.  // BUILD-AUTH-LIMITS-BLK
  END. //AUTH-CCCT-BLK

  {mip/inc/mipcatcherror.i
  &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
END PROCEDURE. /* buildAuthLimitDataset */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkForRateChange) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForRateChange Procedure
PROCEDURE checkForRateChange :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  {ma/app/maauthbuscheckforratechange.i}

END PROCEDURE. /* _checkForRateChange */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-populateAdditionalData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateAdditionalData Procedure
PROCEDURE populateAdditionalData :
/*------------------------------------------------------------------------------
  Purpose   : Procedure to populate the additional auth data into the Auth dataset,
              used when rendering containers, to decrease service calls when
              rendering, and by doing so, improve performance
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

  DEFINE BUFFER btt_auth                    FOR tt_auth.
  DEFINE BUFFER btt_auth_provider           FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_provider_history   FOR tt_auth_provider_history.
  DEFINE BUFFER btt_auth_detail             FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_detail_history     FOR tt_auth_detail_history.
  DEFINE BUFFER btt_auth_flag_value         FOR tt_auth_flag_value.
  DEFINE BUFFER btt_auth_copay              FOR tt_auth_copay.
  DEFINE BUFFER btt_auth_copay_history      FOR tt_auth_copay_history.
  DEFINE BUFFER group_discipline            FOR docdisp.
  DEFINE BUFFER group_doctor                FOR doctor.
  DEFINE BUFFER attending_doctor            FOR doctor.

  DEFINE VARIABLE dAmountPaid               AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthObj                  AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj          AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthRuleObj              AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj              AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dBMIValue                 AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dInsurerObj               AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dLinkAuthRuleObj          AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dOwningObj                AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dQuantityPaid             AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dWorkshopObj              AS DECIMAL          NO-UNDO.

  DEFINE VARIABLE dEffectiveDate            AS DATE             NO-UNDO.
  DEFINE VARIABLE dEndDate                  AS DATE             NO-UNDO.
  DEFINE VARIABLE dProvStartDate            AS DATE             NO-UNDO.
  DEFINE VARIABLE dStartDate                AS DATE             NO-UNDO.
  DEFINE VARIABLE dTermDate                 AS DATE             NO-UNDO.

  DEFINE VARIABLE cArsRate                  AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAttDocDesc               AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAuthGroupList            AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cBaseRate                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cBaseRateList             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cDependantGender          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cDiscipline               AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cDocDesc                  AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cLineRestriction          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cMemNum                   AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cNegGroup                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cOEM                      AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cOptionDescription        AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cOwningAltValue           AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cProviderList             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cProviderType             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleCode                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleDescription          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleType                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleValidValues          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleValue                AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cStatus                   AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cStatusNote               AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cSubDiscipline            AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cWorkgroupObjList         AS CHARACTER        NO-UNDO.

  DEFINE VARIABLE iDependantAgeDays         AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iDependantAgeYears        AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iDocNum                   AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iGroupDocNum              AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iOption                   AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iSearchDoctor             AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iStatusCode               AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iNegNum                   AS INTEGER          NO-UNDO.
  DEFINE VARIABLE lActivateCopayment        AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lActivatePenalty          AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lAllowUpdate              AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lGroupProvider            AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lMandatory                AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSuccess                  AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSystemOwned              AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lValidRule                AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lcConfiguration           AS LONGCHAR         NO-UNDO.

  DEFINE VARIABLE oDepHealthSearch AS cls.madephealthsearch     NO-UNDO.

  DEFINE BUFFER scheme FOR scheme.

&IF {&DBDFMA} >= 010195 &THEN

  FOR EACH btt_auth:

    mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                                 INPUT-OUTPUT TABLE ttAuthTypeConfig).

    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    ASSIGN cMemNum            = btt_auth.mem_num
           dAuthObj           = DECIMAL(btt_auth.auth_obj)
           dInsurerObj        = DECIMAL(btt_auth.insurer_obj)
           dAuthTypeObj       = DECIMAL(btt_auth.auth_type_obj)
           iOption            = INTEGER(btt_auth.option_code)
           dStartDate         = IF DATE(btt_auth.start_date) = ?
                                THEN TODAY
                                ELSE DATE(btt_auth.start_date)
           iStatusCode        = INTEGER(btt_auth.auth_status)
           dAmountPaid        = DECIMAL(btt_auth.amount_paid)
           dQuantityPaid      = DECIMAL(btt_auth.quantity_paid)
           lActivatePenalty   = ttAuthTypeConfig.ActivatePenalty
           lActivateCopayment = ttAuthTypeConfig.ActivateCopayment.

    /*
      Populate additional data for Provider Container
    */
    { ma/app/maauthbuspopaddprovider.i }

    /* We only need to populate the blank entry after the provider list has been populated */
    ASSIGN cProviderList                      = IF INDEX(cProviderList, "^":U) > 0
                                                THEN REPLACE(cProviderList, "^":U, "|":U) ELSE cProviderList
           btt_auth._provider_list            = cProviderList
           btt_auth._refnum_updatable         = mipEnv:Health:AuthBusinessLogic:authRefNumUpdatable   (INPUT dAuthObj,    INPUT dInsurerObj,  INPUT iOption, INPUT dStartDate)
           btt_auth._reason_mandatory         = mipEnv:Health:AuthService:statusReasonMandatory       (INPUT iStatusCode, INPUT dInsurerObj,  INPUT iOption, INPUT dStartDate)
           btt_auth._auth_dependant_updatable = mipEnv:Health:AuthBusinessLogic:authDependantUpdatable(INPUT dInsurerObj, INPUT dAuthTypeObj, INPUT iOption, INPUT dStartDate, INPUT dAmountPaid, INPUT dQuantityPaid)
           btt_auth._authnum_viewable         = mipEnv:Health:AuthBusinessLogic:AuthNumViewable       (INPUT iStatusCode, INPUT dInsurerObj,  INPUT iOption, INPUT dStartDate)
           btt_auth._auth_group_list          = cAuthGroupList.

    IF iOption = 0 OR iOption = ?
    THEN
      ASSIGN lSuccess = mipEnv:Health:maMember:getMemberOption(INPUT cMemNum,
                                                               INPUT dStartDate,
                                                               OUTPUT iOption,
                                                               OUTPUT cOptionDescription).
    ELSE DO:
      FIND scheme NO-LOCK
        WHERE scheme.scheme-code = iOption
        NO-ERROR.
      IF AVAILABLE scheme THEN
        ASSIGN cOptionDescription = scheme.short-name.
    END.  // ELSE - IF iOption = 0 OR iOption = ?

    ASSIGN btt_auth._option_code = STRING(iOption) + (IF cOptionDescription <> "" THEN " - ":U + cOptionDescription ELSE "":U).

    /*
      Populate additional data for Coding Containers
    */
    { ma/app/maauthbuspopaddcoding.i }

    /*
      Populate additional data for Detail Containers
    */
    { ma/app/maauthbuspopadddetail.i }

    /*
      Populate additional data for Copay Containers
    */
    { ma/app/maauthbuspopaddcopay.i }

    ASSIGN
      btt_auth._base_rate_list    = cBaseRateList
      btt_auth._line_restriction  = cLineRestriction.

    /*
      Populate fields required for tariff validation
    */
    IF NOT VALID-OBJECT(goAuthorisation) AND dAuthObj <> 0
    THEN
      mipEnv:Health:AuthService:getAuthObject
        ( INPUT  dAuthObj,
          INPUT  "":U,
          OUTPUT goAuthorisation ).

    IF VALID-OBJECT(goAuthorisation) AND goAuthorisation:InFocus
    THEN
      ASSIGN btt_auth._dependant_age_days  = goAuthorisation:DependantAgeDays
             btt_auth._dependant_age_years = goAuthorisation:DependantAgeYears
             btt_auth._dependant_gender    = goAuthorisation:DependantGender
             btt_auth._dependant_bmi       = goAuthorisation:DependantBMI.
    ELSE DO:
      mipEnv:Health:maMember:getDependantAgeGender (INPUT  cMemNum,
                                                    INPUT  btt_auth.dependant,
                                                    INPUT  dStartDate,
                                                    OUTPUT iDependantAgeYears,
                                                    OUTPUT iDependantAgeDays,
                                                    OUTPUT cDependantGender).

      DATASET dsDepHealth:EMPTY-DATASET().
      ASSIGN
        oDepHealthSearch = NEW cls.madephealthsearch(DATASET dsDepHealth BY-REFERENCE)
        lSuccess         = oDepHealthSearch:SetCriteria("BufferList":U , "tt_dephealth":U)
        lSuccess         = oDepHealthSearch:SetCriteria("DataSecured":U, "FALSE":U)
        lSuccess         = oDepHealthSearch:setFilterCriteria("tt_dephealth.mem_num":U,   "=":U, btt_auth.mem_num)
        lSuccess         = oDepHealthSearch:setFilterCriteria("tt_dephealth.dependant":U, "=":U, btt_auth.dependant)
        lSuccess         = oDepHealthSearch:fetchData().

      mipEnv:Health:AuthService:getAuthBMI(INPUT TABLE tt_dephealth,
                                           BUFFER btt_auth,
                                           OUTPUT      dBMIValue).

      ASSIGN btt_auth._dependant_age_days  = iDependantAgeDays
             btt_auth._dependant_age_years = iDependantAgeYears
             btt_auth._dependant_gender    = cDependantGender
             btt_auth._dependant_bmi       = dBMIValue NO-ERROR.

    END.  /* ELSE - IF goAuthorisation:InFocus THEN */
  END. /* FOR EACH btt_auth */

  RUN _populateAuthStatusReasonDescriptions IN TARGET-PROCEDURE.

  { mip/inc/mipcatcherror.i
      &FINALLY = "IF VALID-OBJECT(oDepHealthSearch) THEN DELETE OBJECT oDepHealthSearch."}

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reselectCrosswalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reselectCrosswalk Procedure
PROCEDURE reselectCrosswalk :
/*------------------------------------------------------------------------------
  Purpose   : reselectCrosswalk
              Business Logic and validation before a Reselect Crosswalk
              (or Authorisation Reguide).
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE oFVSearch AS cls.maauthflagvaluesearch NO-UNDO.

  DEFINE VARIABLE iCalcTime        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cTrackingMessage AS CHARACTER   NO-UNDO.

  FIND FIRST tt_auth NO-LOCK NO-ERROR.

  /* Data Access */
  ASSIGN
    iCalcTime        = MTIME
    glSuccess        = mipEnv:Health:AuthDataAccess:reselectCrosswalk
                        ( INPUT tt_auth.auth_obj,
                          INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE )
    cTrackingMessage = "Data Access reselectCrosswalk completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /* Handle Flags differently */
  ASSIGN
    iCalcTime  = MTIME
    oFVSearch  = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE)
    glSuccess  = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
    glSuccess  = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, tt_auth.auth_obj)
    glSuccess  = oFVSearch:fetchData().

  FIND FIRST tt_auth NO-LOCK NO-ERROR.

  IF AVAILABLE tt_auth THEN
  DO:
    RUN _createDefaultAuthFlags IN TARGET-PROCEDURE (BUFFER tt_auth,                             // Auth buffer
                                                     INPUT TABLE tt_auth_provider BY-REFERENCE,  // Provider temp table
                                                     INPUT TRUE ).                               // iplResetToDefaults

  END. /*  IF AVAILABLE tt_auth */

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  { mip/inc/mipcatcherror.i
    &FINALLY="DATASET dsAuthFlagValue:EMPTY-DATASET().
              IF VALID-OBJECT(oFVSearch) THEN DELETE OBJECT oFVSearch."}
&ENDIF
END PROCEDURE.  /* reselectCrosswalk */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthorisation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthorisation Procedure
PROCEDURE saveAuthorisation :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation
  Parameters:
  Notes     :
  Author    :
------------------------------------------------------------------------------*/

  {ma/app/maauthbussaveauthorisation.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateAuthTypeUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateAuthTypeUsage Procedure
PROCEDURE validateAuthTypeUsage :
/*------------------------------------------------------------------------------
  Purpose:     Validate's if an auth type may be used again within a certain
               period from another auth. Also, the use of certain auth types
               is restricted and may not be used in the same benefit year as
               another auth.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthTypeObj        AS DECIMAL              NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj         AS DECIMAL              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode         AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipcMemberNumber       AS CHARACTER            NO-UNDO.
  DEFINE INPUT  PARAMETER ipiDependant          AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthDate           AS DATE                 NO-UNDO.
  DEFINE INPUT  PARAMETER ipcValidationType     AS CHARACTER            NO-UNDO.  /* takes in: Both/Usage/Restrictions */
  DEFINE INPUT  PARAMETER ipcCode               AS CHARACTER            NO-UNDO.  /* Blank, ICD code or CPT Code */
  DEFINE OUTPUT PARAMETER oplValidAuthTypeUsage AS LOGICAL              NO-UNDO.
  DEFINE OUTPUT PARAMETER opcValidMessage       AS CHARACTER            NO-UNDO.

  DEFINE VARIABLE oATGroup                      AS cls.mipacronym       NO-UNDO.

  DEFINE VARIABLE cUsageType                    AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE dAuthDate                     AS DATE                 NO-UNDO.
  DEFINE VARIABLE dFromDate                     AS DATE                 NO-UNDO.
  DEFINE VARIABLE dToDate                       AS DATE                 NO-UNDO.
  DEFINE VARIABLE iAuthTypeCnt                  AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iAuth                         AS INTEGER  INITIAL 0   NO-UNDO.
  DEFINE VARIABLE iLosTotal                     AS INTEGER  INITIAL 0   NO-UNDO.
  DEFINE VARIABLE iEntry                        AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iDay                          AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iMonth                        AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iYear                         AS INTEGER              NO-UNDO.
  DEFINE VARIABLE lSuccess                      AS LOGICAL              NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  IF ipdAuthDate <> ?
  THEN ASSIGN dAuthDate = ipdAuthDate.
  ELSE ASSIGN dAuthDate = TODAY.

  ASSIGN oplValidAuthTypeUsage = TRUE.

  EMPTY TEMP-TABLE ttAuthTypeConfig.

  mipEnv:Health:AuthService:getAuthTypeConfig (INPUT ipdAuthTypeObj,                   /* ipdAuthTypeObj   */
                                               INPUT 0.00,                             /* ipdInsurerObj    */
                                               INPUT ipiOptionCode,                    /* ipiOptionCode    */
                                               INPUT ipdAuthDate,                      /* ipdEffectiveDate */
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).

  FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE ttAuthTypeConfig THEN
  DO:
    ASSIGN oplValidAuthTypeUsage = FALSE
           opcValidMessage       = SUBSTITUTE("No auth type record available for auth type obj = &1",ipdAuthTypeObj).
    RETURN.
  END.  /* IF NOT AVAILABLE ttAuthTypeConfig THEN */

  IF (ipcValidationType = "Both" OR ipcValidationType = "Usage":U)
  AND ttAuthTypeConfig.UsageQuantity <> 0 THEN
  DO:
    ASSIGN dToDate = dAuthDate.

    CASE ttAuthTypeConfig.UsagePeriodType:

      WHEN "ma_acAuthPeriodDays":U
      THEN
        ASSIGN dFromDate = dToDate - ttAuthTypeConfig.UsagePeriod.

      WHEN "ma_acAuthPeriodWeeks":U
      THEN
        ASSIGN dFromDate = dToDate - (ttAuthTypeConfig.UsagePeriod * 7).

      WHEN "ma_acAuthPeriodMonths":U THEN
      DO:
        ASSIGN
           iMonth = MONTH(dToDate) - ttAuthTypeConfig.UsagePeriod
           iYear  = YEAR(dToDate).

        REPEAT:
          IF iMonth < 1
          THEN
            ASSIGN iYear  = iYear  - 1
                   iMonth = iMonth + 12.

          IF iMonth > 0
          THEN LEAVE.
        END.  /* REPEAT: */

        ASSIGN dFromDate = DATE(iMonth,DAY(dToDate),iYear).
      END. /* WHEN "ma_acAuthPeriodMonths" */

      WHEN "ma_acAuthPeriodYears":U
      THEN
        ASSIGN dFromDate = DATE(MONTH(dToDate),DAY(dToDate),YEAR(dToDate) - ttAuthTypeConfig.UsagePeriod).

      WHEN "ma_acAuthPeriodBenefitYears":U
      THEN     /* Benefit years will run from 1 January - 31 December for a year */
        ASSIGN iMonth    = 01
               iDay      = 01
               iYear     = YEAR(dToDate) - ttAuthTypeConfig.UsagePeriod
               dFromDate = DATE(iMonth,iDay,iYear)

               iMonth    = 12
               iDay      = 31
               iYear     = YEAR(dToDate)
               dToDate   = DATE(iMonth,iDay,iYear).

    END CASE.

    ASSIGN iAuthTypeCnt = 0.

    IF ttAuthTypeConfig.UsageType = "":U THEN
    DO:
      FOR EACH hat_auth NO-LOCK
         WHERE hat_auth.mem_num       = ipcMemberNumber
           AND hat_auth.dependant     = ipiDependant
           AND hat_auth.auth_type_obj = ipdAuthTypeObj
           AND hat_auth.start_date   >= dFromDate
           AND hat_auth.start_date   <= dToDate:

        IF LOOKUP(STRING(hat_auth.auth_status),"0,1,2,4,7") <> 0
        THEN ASSIGN iAuthTypeCnt = iAuthTypeCnt + 1
                    iLosTotal    = iLosTotal    + hat_auth.total_los.

      END. /*FOR EACH hat_auth NO-LOCK*/
    END. /* IF ttAuthTypeConfig.UsageType = "":U THEN */


    /* Count the number of authorisations per ICD */
    IF ttAuthTypeConfig.UsageType = "ma_acAuthUsageTypeICD":U THEN
    DO:
      ASSIGN cUsageType                                = "ICD code".
      FOR EACH  hat_auth NO-LOCK
         WHERE  hat_auth.mem_num                       = ipcMemberNumber
           AND  hat_auth.dependant                     = ipiDependant
           AND  hat_auth.auth_type_obj                 = ipdAuthTypeObj
           AND  hat_auth.start_date                   >= dFromDate
           AND  hat_auth.start_date                   <= dToDate,
         FIRST  hat_auth_coding NO-LOCK
         WHERE  hat_auth_coding.auth_obj               = hat_auth.auth_obj
           AND  hat_auth_coding.owning_entity_mnemonic = "diagnos":U
           AND  hat_auth_coding.owning_alt_value       = ipcCode
           AND (hat_auth_coding.primary_code           = YES
            OR  hat_auth_coding.main_code              = YES)
         :

        IF LOOKUP(STRING(hat_auth.auth_status),"0,1,2,4,7") <> 0
        THEN ASSIGN iAuthTypeCnt = iAuthTypeCnt + 1
                    iLosTotal    = iLosTotal    + hat_auth.total_los.

      END. /*FOR EACH hat_auth NO-LOCK*/
    END. /* IF ttAuthTypeConfig.UsageType = "ma_acAuthUsageTypeICD" */

    /* Count the number of authorisations per CPT */
    IF ttAuthTypeConfig.UsageType = "ma_acAuthUsageTypeCPT":U THEN
    DO:
      ASSIGN cUsageType                                = "CPT code".
      FOR EACH  hat_auth NO-LOCK
         WHERE  hat_auth.mem_num                       = ipcMemberNumber
           AND  hat_auth.dependant                     = ipiDependant
           AND  hat_auth.auth_type_obj                 = ipdAuthTypeObj
           AND  hat_auth.start_date                   >= dFromDate
           AND  hat_auth.start_date                   <= dToDate,
         FIRST  hat_auth_coding NO-LOCK
         WHERE  hat_auth_coding.auth_obj               = hat_auth.auth_obj
           AND  hat_auth_coding.owning_entity_mnemonic = "hlmck":U
           AND  hat_auth_coding.owning_alt_value       = ipcCode
           AND (hat_auth_coding.primary_code           = YES
            OR  hat_auth_coding.main_code              = YES)
         :

        IF LOOKUP(STRING(hat_auth.auth_status),"0,1,2,4,7") <> 0
        THEN ASSIGN iAuthTypeCnt = iAuthTypeCnt + 1
                    iLosTotal    = iLosTotal    + hat_auth.total_los.

      END. /*FOR EACH hat_auth NO-LOCK*/
    END. /* IF ttAuthTypeConfig.UsageType = "ma_acAuthUsageTypeCPT" */

    IF iAuthTypeCnt > ttAuthTypeConfig.UsageQuantity THEN
    DO:

      ASSIGN oATGroup = NEW cls.mipacronym(INPUT ?, INPUT FALSE, INPUT "ma_acAuthPeriod":U).

      oATGroup:focusAcronym("KEY":U, ttAuthTypeConfig.UsagePeriodType).

      FIND FIRST hac_auth_type NO-LOCK
           WHERE hac_auth_type.auth_type_obj = ipdAuthTypeObj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE hac_auth_type
      THEN ASSIGN cUsageType = IF cUsageType = ""
                               THEN hac_auth_type.auth_type
                               ELSE hac_auth_type.auth_type + " and " + cUsageType + " " + ipcCode.


      IF  ttAuthTypeConfig.UsageOverrideUser <> ""
      AND LOOKUP(mipEnv:miUser:UserCode, ttAuthTypeConfig.UsageOverrideUser, ",":U) = 0
      THEN ASSIGN oplValidAuthTypeUsage = FALSE.


      ASSIGN opcValidMessage = SUBSTITUTE('Only &1 authorisation/s for authorisation type &2 allowed every &3.',
                                          STRING(ttAuthTypeConfig.UsageQuantity),
                                          cUsageType,
                                          STRING(ttAuthTypeConfig.UsagePeriod) + " " + oATGroup:AcronymLabel)
             opcValidMessage = opcValidMessage + CHR(10) + " Number of authorisations loaded: " + STRING(iAuthTypeCnt)
                                               + CHR(10) + " Total LOS: " + STRING(iLosTotal).

      RETURN.
    END.  /* IF iAuthTypeCnt >= ttAuthTypeConfig.UsageQuantity */
  END. /* IF (ipcValidationType = "Both" OR ipcValidationType = "Usage":U) */

  IF (ipcValidationType = "Both":U OR ipcValidationType = "Restrictions":U)
  AND ttAuthTypeConfig.AuthTypeRestrictions <> "" THEN
  DO:
    DO iEntry = 1 TO NUM-ENTRIES(ttAuthTypeConfig.AuthTypeRestrictions):

      FIND FIRST hac_auth_type NO-LOCK
           WHERE hac_auth_type.auth_type = ENTRY(iEntry,ttAuthTypeConfig.AuthTypeRestrictions)
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE hac_auth_type THEN
      DO:
        IF CAN-FIND(FIRST hat_auth NO-LOCK
               WHERE hat_auth.mem_num          = ipcMemberNumber
               AND   hat_auth.dependant        = ipiDependant
               AND   hat_auth.auth_type_obj    = hac_auth_type.auth_type_obj
               AND   hat_auth.start_date      <> dAuthDate
               AND   YEAR(hat_auth.start_date) = YEAR(dAuthDate)) THEN
          ASSIGN oplValidAuthTypeUsage = FALSE
                 opcValidMessage       = SUBSTITUTE("Another authorisation with auth type '&1' was already registered for this dependant in the &2-benefit year.",
                                              hac_auth_type.auth_type,
                                              STRING(YEAR(dAuthDate))).
      END.  /* IF AVAILABLE hac_auth_type THEN */
    END.  /* DO iEntry = 1 TO NUM-ENTRIES(ttAuthTypeConfig.AuthTypeRestrictions): */
  END.  /* IF (ipcValidationType = "Both":U OR ipcValidationType = "Restrictions":U) */

&ENDIF

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oATGroup) THEN DELETE OBJECT oATGroup."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthClaimCodesAndTypes) = 0 &THEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthClaimCodesAndTypes Procedure
PROCEDURE _validateAuthClaimCodesAndTypes :
/*------------------------------------------------------------------------------
  Purpose:     Validate that all the auth claim codes and types comply 
               with the auth type and auth type provider setups .
  Parameters:  Auth dataset 
               Auth Obj of auth to be validated 
  Notes: If EnforceHeaderClaimCodeMatch or EnforceHeaderClaimTypeMatch is TRUE 
         this procedure will autmatically update the provider claim code and type
         on change of the auth header claim code/type , but not vice versa. If the provider claim
         code/type is updated and it does not match the header , an error will be returned.
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation. 
  DEFINE INPUT        PARAMETER ipdAuthObj    AS DECIMAL NO-UNDO .

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER btt_auth          FOR tt_auth .
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail   FOR tt_auth_detail .
  DEFINE BUFFER buf_auth          FOR hat_auth .
  DEFINE BUFFER buf_auth_provider FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_detail   FOR hat_auth_detail.
  DEFINE VARIABLE cCC-CTList                  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cClaimCodeTypeCombo         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMessage                    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMessageType                AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lAcknowledge                AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lSuccess                    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lAuthClaimCodeUpdated       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lAuthClaimTypeUpdated       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lProviderClaimCodeUpdated   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lProviderClaimTypeUpdated   AS LOGICAL   NO-UNDO.
  FIND FIRST btt_auth NO-LOCK 
       WHERE btt_auth.auth_obj = ipdAuthObj  NO-ERROR.
  {mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors= TRUE } 
  IF NOT AVAILABLE btt_auth 
  THEN 
    RETURN .
  FIND FIRST buf_auth NO-LOCK
       WHERE buf_auth.auth_obj = ipdAuthObj NO-ERROR . 
  mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).
  FIND FIRST ttAuthTypeConfig NO-ERROR.
  {mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors= TRUE } 
  IF AVAILABLE ttAuthTypeConfig THEN
  DO:
    ASSIGN cCC-CTList              = fnBuildCC-CT-List(BUFFER ttAuthTypeConfig)
           lAuthClaimCodeUpdated   = AVAILABLE buf_auth AND buf_auth.claim_code <> btt_auth.claim_code
                                     OR NOT AVAILABLE buf_auth
           lAuthClaimTypeUpdated   = AVAILABLE buf_auth AND buf_auth.claim_type <> btt_auth.claim_type 
                                     OR NOT AVAILABLE buf_auth
           cClaimCodeTypeCombo     = STRING(btt_auth.claim_code) + "-":U + btt_auth.claim_type.
    /*
      Since claim codes and types are set up as a combination , ensure the correct combinations is captured
      Generate an error if the user does not have permission to update the claim code 
      Generate a warning if the user does have permission - depending on the auth type setup
      Only display the warning when the claim code or type was updated
    */
    IF LOOKUP( cClaimCodeTypeCombo , cCC-CTList) = 0 
    AND (    (NOT ttAuthTypeConfig.ClaimCodeUpdAllowed AND lAuthClaimCodeUpdated)
          OR (NOT ttAuthTypeConfig.ClaimTypeUpdAllowed AND lAuthClaimTypeUpdated))
    THEN 
      ASSIGN cMessageType = "ERR":U 
             cMessage     = "The claim code and type combination specified is invalid. Valid combinations are: ":U
                          +  cCC-CTList .
    ELSE IF  ttAuthTypeConfig.MultipleCCMessageType   <> "":U 
         AND NUM-ENTRIES(ttAuthTypeConfig.ClaimCodes ) > 1
         AND (   lAuthClaimCodeUpdated
              OR lAuthClaimTypeUpdated)
    THEN
      ASSIGN cMessageType = "WAR":U
             lAcknowledge = INDEX(ttAuthTypeConfig.MultipleCCMessageType , "warnack":U) > 0
             cMessage     = "Ensure that the correct claim code is captured because multiple claim codes and types apply: " 
                          +  cCC-CTList       
             lSuccess     = goErrorObject:addError(INPUT "hatau":U,            /* ipcOwningEntityMnemonic  */
                                                   INPUT btt_auth.auth_obj,    /* ipdOwningEntityObj       */
                                                   INPUT "":U,                 /* ipcOwningEntityKey       */
                                                   INPUT "claim_code":U,       /* ipcFieldName             */
                                                   INPUT btt_auth.line_number, /* ipiLineNumber            */
                                                   INPUT cMessage,             /* ipcMessageText           */
                                                   INPUT cMessageType,         /* ipcMessageType           */
                                                   INPUT lAcknowledge ) .      /* iplAcknowledge           */    
    PROVIDER-BLK:
    FOR EACH btt_auth_provider
       WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj :
      FIND FIRST buf_auth_provider 
           WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj NO-ERROR .
      { mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
      mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                                   BUFFER btt_auth_provider, 
                                                   INPUT-OUTPUT TABLE ttAuthTypeConfig).
      FIND FIRST ttAuthTypeConfig NO-ERROR.
      {mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors= TRUE }   
      ASSIGN cCC-CTList                = fnBuildCC-CT-List(BUFFER ttAuthTypeConfig)
             lProviderClaimCodeUpdated = AVAILABLE buf_auth_provider AND buf_auth_provider.claim_code <> btt_auth_provider.claim_code
                                         OR NOT AVAILABLE buf_auth_provider
             lProviderClaimTypeUpdated = AVAILABLE buf_auth_provider AND buf_auth_provider.claim_type <> btt_auth_provider.claim_type 
                                         OR NOT AVAILABLE buf_auth_provider .
      IF ttAuthTypeConfig.EnforceHeaderClaimCodeMatch 
      AND btt_auth_provider.claim_code <> btt_auth.claim_code 
      AND NOT lAuthClaimCodeUpdated  // Note we put this part here because we only want this validation to run on change of a provider and not the header
      AND lProviderClaimCodeUpdated
      THEN 
        ASSIGN cMessageType = "ERR":U
               cMessage     = SUBSTITUTE("The provider(&1) claim code(&2) must match the auth header claim code(&3)":U , btt_auth_provider.doc_num , STRING(btt_auth_provider.claim_code) , STRING(btt_auth.claim_code) )       
               lSuccess     = goErrorObject:addError(INPUT "hatap":U,                              /* ipcOwningEntityMnemonic  */
                                                     INPUT btt_auth_provider.auth_provider_obj,    /* ipdOwningEntityObj       */
                                                     INPUT "":U,                                   /* ipcOwningEntityKey       */
                                                     INPUT "claim_code":U,                         /* ipcFieldName             */
                                                     INPUT btt_auth.line_number,                   /* ipiLineNumber            */
                                                     INPUT cMessage,                               /* ipcMessageText           */
                                                     INPUT cMessageType,                           /* ipcMessageType           */
                                                     INPUT FALSE ) .                               /* iplAcknowledge           */   

      IF  ttAuthTypeConfig.EnforceHeaderClaimTypeMatch
      AND btt_auth_provider.claim_type <> btt_auth.claim_type 
      AND NOT lAuthClaimTypeUpdated  // Note we put this part here because we only want this validation to run on change of a provider and not the header
      AND lProviderClaimTypeUpdated 
      THEN 
        ASSIGN cMessageType = "ERR":U
               cMessage     = SUBSTITUTE("The provider(&1) claim type(&2) must match the auth header claim type(&3)":U , btt_auth_provider.doc_num , btt_auth_provider.claim_type , btt_auth.claim_type )         
               lSuccess     = goErrorObject:addError(INPUT "hatap":U,                            /* ipcOwningEntityMnemonic  */
                                                     INPUT btt_auth_provider.auth_provider_obj,  /* ipdOwningEntityObj       */
                                                     INPUT "":U,                                 /* ipcOwningEntityKey       */
                                                     INPUT "claim_type":U,                       /* ipcFieldName             */
                                                     INPUT btt_auth.line_number,                 /* ipiLineNumber            */
                                                     INPUT cMessage,                             /* ipcMessageText           */
                                                     INPUT cMessageType,                         /* ipcMessageType           */
                                                     INPUT FALSE ) .                             /* iplAcknowledge           */      
      IF (lProviderClaimCodeUpdated 
       OR lProviderClaimTypeUpdated)
      AND NUM-ENTRIES(ttAuthTypeConfig.ClaimCodes) > 1
      AND ttAuthTypeConfig.MultipleCCMessageType <> "":U
      AND NOT goErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj, "":U, "ERR":U) // Only generate the warning if there aren't any errors
      THEN
        ASSIGN cMessageType = "WAR":U
               lAcknowledge = INDEX(ttAuthTypeConfig.MultipleCCMessageType , "warnack":U) > 0
               cMessage     = "Ensure that the correct claim code is captured because multiple claim codes and types apply: " 
                            +  cCC-CTList       
               lSuccess     = goErrorObject:addError(INPUT "hatau":U,            /* ipcOwningEntityMnemonic  */
                                                     INPUT btt_auth.auth_obj,    /* ipdOwningEntityObj       */
                                                     INPUT "":U,                 /* ipcOwningEntityKey       */
                                                     INPUT "claim_code":U,       /* ipcFieldName             */
                                                     INPUT btt_auth.line_number, /* ipiLineNumber            */
                                                     INPUT cMessage,             /* ipcMessageText           */
                                                     INPUT cMessageType,         /* ipcMessageType           */
                                                     INPUT lAcknowledge ) .      /* iplAcknowledge           */  
      /*
        If the claim code/type match is enforced , we will make sure the claim code/type is updated accordingly
      */
      IF  ttAuthTypeConfig.EnforceHeaderClaimCodeMatch 
      AND lAuthClaimCodeUpdated 
      THEN 
        ASSIGN btt_auth_provider.claim_code    = btt_auth.claim_code 
               btt_auth_provider.record_action = "MODIFY":U .

      IF ttAuthTypeConfig.EnforceHeaderClaimTypeMatch
      AND lAuthClaimTypeUpdated 
      THEN
        ASSIGN btt_auth_provider.claim_type    = btt_auth.claim_type 
               btt_auth_provider.record_action = "MODIFY":U .
    END. // PROVIDER-BLK
  END. // IF AVAILABLE ttAuthTypeConfig
  {mip/inc/mipcatcherror.i} 
&ENDIF
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ENDIF
&IF DEFINED(EXCLUDE-validateClaimCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateClaimCode Procedure
PROCEDURE validateClaimCode :
/*------------------------------------------------------------------------------
  Purpose:     Check if the claim code is valid for the authorisation
  Parameters:  If oplValidClaimCode returns TRUE and opcErrorMessage <> "",
               then it's a WARNING only.
               If oplValidClaimCode returns FALSE and opcErrorMessage <> "",
               then it's an ERROR and user must not be able to continue.
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipiInsurerObj      AS DECIMAL              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode      AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipcMemberNumber    AS CHARACTER            NO-UNDO.
  DEFINE INPUT  PARAMETER ipiDependant       AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiClaimCode       AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthTypeObj     AS DECIMAL              NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthDate        AS DATE                 NO-UNDO.
  DEFINE INPUT  PARAMETER ipcProviderType    AS CHARACTER            NO-UNDO.
  DEFINE INPUT  PARAMETER ipiPrType          AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiSubPrType       AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiNegNumber       AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipcAuthLevel       AS CHARACTER            NO-UNDO.
  DEFINE OUTPUT PARAMETER oplValidClaimCode  AS LOGICAL              NO-UNDO.
  DEFINE OUTPUT PARAMETER opcErrorMessage    AS CHARACTER            NO-UNDO.

  DEFINE VARIABLE dAuthDate                  AS DATE                 NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE iOptionCode                AS INTEGER              NO-UNDO.
  DEFINE VARIABLE cMessageText               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMessageDescription        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMessageHelp               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE lValidRule                 AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cRuleValue                 AS CHARACTER            NO-UNDO.

  ASSIGN oplValidClaimCode = TRUE.

  IF ipdAuthDate <> ?
  THEN ASSIGN dAuthDate = ipdAuthDate.
  ELSE ASSIGN dAuthDate = TODAY.

  /*
     Get the correct scheme option for the member if nothing is passed through
  */
  IF ipiOptionCode = 0 THEN
  DO:
    ASSIGN lSuccess = mipEnv:Health:maMember:getMemberOption(INPUT  ipcMemberNumber,
                                                             INPUT  dAuthDate,
                                                             OUTPUT iOptionCode).

  END. /*IF ipiOptionCode = 0 THEN */
  ELSE
    ASSIGN iOptionCode = ipiOptionCode.

  IF iOptionCode = 0 THEN
  DO:
    ASSIGN oplValidClaimCode = FALSE
           opcErrorMessage   = SUBSTITUTE('Unable to validate claim code for option code &1.',STRING(iOptionCode)).
    RETURN.
  END. /*IF iOptionCode = 0 THEN*/

  /*
     Validate the claim code
  */
  lSuccess = mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  iOptionCode,
                                                                INPUT  ipiClaimCode,
                                                                INPUT  dAuthDate,
                                                                OUTPUT oplValidClaimCode,
                                                                OUTPUT opcErrorMessage).
  IF NOT oplValidClaimCode THEN RETURN.

  /*
     Validate the claim code against the auth type setups
  */
  EMPTY TEMP-TABLE ttAuthTypeConfig.

  mipEnv:Health:AuthService:getAuthTypeConfig (INPUT ipdAuthTypeObj,                   /* ipdAuthTypeObj   */
                                               INPUT ipiInsurerObj,                    /* ipdInsurerObj    */
                                               INPUT iOptionCode,                      /* ipiOptionCode    */
                                               INPUT ipdAuthDate,                      /* ipdEffectiveDate */
                                               INPUT ipcProviderType,                  /* ipcProviderType  */
                                               INPUT ipiNegNumber,                     /* ipiNegNumber     */
                                               INPUT STRING(ipiPrType,'999'),          /* ipcDiscipline    */
                                               INPUT STRING(ipiSubPrType),             /* ipcSubDiscipline */
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).

  FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE ttAuthTypeConfig THEN
  DO:
    ASSIGN oplValidClaimCode = FALSE
           opcErrorMessage   = SUBSTITUTE("No auth type record available for auth type obj = &1",ipdAuthTypeObj).
    RETURN.
  END.  /* IF NOT AVAILABLE ttAuthTypeConfig THEN */

  /*
     Do not allow claim codes that are specified on the disallow list for the auth type
  */
  IF LOOKUP(STRING(ipiClaimCode,"999"),ttAuthTypeConfig.ClaimCodesDisallow,",":U) > 0 THEN
  DO:
    mipEnv:miUtility:getMessageText(INPUT "ma_MsgAuth":U,
                                    INPUT "4":U,  /* The &1 '&2' is not allowed on auth type '&3'. */
                                    INPUT "ENG":U,
                                    OUTPUT cMessageText,
                                    OUTPUT cMessageDescription,
                                    OUTPUT cMessageHelp).

    ASSIGN oplValidClaimCode = FALSE
           opcErrorMessage   = SUBSTITUTE(cMessageText, "Claim Code", STRING(ipiClaimCode,"999"), ttAuthTypeConfig.AuthType).

    RETURN.
  END. /* IF LOOKUP(STRING(ipiClaimCode,"999"),tt_auth_type_control.claim_codes_disallow,",":U) > 0 */

  /*
    Validate the claim code against the default claim code setup for the auth type
  */
  IF ttAuthTypeConfig.ClaimCodeUpdAllowed = NO THEN
  DO:
    IF  ttAuthTypeConfig.ClaimCodes <> "":U
    AND ttAuthTypeConfig.ClaimCodeUpdAllowed = no
    AND LOOKUP(STRING(ipiClaimCode),ttAuthTypeConfig.ClaimCodes) = 0 THEN
    DO:
      ASSIGN oplValidClaimCode = FALSE
             opcErrorMessage   = SUBSTITUTE("The claim code (&1) is not listed in the claim codes (&2) for the auth type control.",
                                            STRING(ipiClaimCode),
                                            ttAuthTypeConfig.ClaimCodes).
      RETURN.
    END.  /* IF ttAuthTypeConfig.ClaimCodes <> "":U AND LOOKUP(... */
  END.  /* IF ttAuthTypeConfig.ClaimCodeUpdAllowed = NO THEN */

  /*
    Validate the claim code against the default claim code setup for the auth type detail
  */
  IF  ttAuthTypeConfig.DefaultClaimCodeDetail <> 0
  AND ttAuthTypeConfig.ClaimCodeUpdAllowed = no
  AND LOOKUP(STRING(ipiClaimCode),STRING(ttAuthTypeConfig.DefaultClaimCodeDetail)) = 0
  THEN
    ASSIGN oplValidClaimCode = FALSE.  /* Error */

  /*
    Validate that the claim code is set up on the auth type
  */
  IF  ipcAuthLevel = "hat_auth_detail":U       // only run for auth detail level
  AND ttAuthTypeConfig.ValidClaimCodesDetail <> "":U
  AND LOOKUP(STRING(ipiClaimCode),ttAuthTypeConfig.ValidClaimCodesDetail) = 0
  THEN
    ASSIGN oplValidClaimCode = FALSE      /* Error */
           opcErrorMessage   = SUBSTITUTE("The claim code (&1) is not listed in the valid claim codes (&2) for the auth type provider control.",
                                           STRING(ipiClaimCode),
                                           ttAuthTypeConfig.ValidClaimCodesDetail).

  /*
     Check for any warning messages in the auth type rules
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipiInsurerObj,
                                                 INPUT  iOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeAUTHWARNCC":U,
                                                 INPUT  STRING(ipiClaimCode),
                                                 INPUT  dAuthDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  IF lSuccess AND lValidRule
  THEN ASSIGN opcErrorMessage = cRuleValue +
                                "[HELP=Auth Rule Code: ":U + STRING(ipiClaimCode) + "]":U.

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateClaimType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateClaimType Procedure
PROCEDURE validateClaimType :
/*------------------------------------------------------------------------------
  Purpose:     Check if the claim type is valid for the authorisation
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj      AS DECIMAL              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode      AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipcClaimType       AS CHARACTER            NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthTypeObj     AS DECIMAL              NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthDate        AS DATE                 NO-UNDO.
  DEFINE INPUT  PARAMETER ipcProviderType    AS CHARACTER            NO-UNDO.
  DEFINE INPUT  PARAMETER ipiPrType          AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiSubPrType       AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipiNegNumber       AS INTEGER              NO-UNDO.
  DEFINE INPUT  PARAMETER ipcAuthLevel       AS CHARACTER            NO-UNDO.
  DEFINE OUTPUT PARAMETER oplValidClaimType  AS LOGICAL              NO-UNDO.
  DEFINE OUTPUT PARAMETER opcValidMessage    AS CHARACTER            NO-UNDO.

  DEFINE VARIABLE dAuthDate                  AS DATE                 NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cMessageText               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMessageDescription        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMessageHelp               AS CHARACTER            NO-UNDO.

  ASSIGN oplValidClaimType = TRUE.

  IF ipdAuthDate <> ?
  THEN ASSIGN dAuthDate = ipdAuthDate.
  ELSE ASSIGN dAuthDate = TODAY.

  ASSIGN lSuccess = mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  ipcClaimType,
                                                                       OUTPUT oplValidClaimType,
                                                                       OUTPUT opcValidMessage).
  IF NOT oplValidClaimType THEN RETURN.

  /*
     Validate the claim type against the auth type setups
  */
  EMPTY TEMP-TABLE ttAuthTypeConfig.

  mipEnv:Health:AuthService:getAuthTypeConfig (INPUT ipdAuthTypeObj,                   /* ipdAuthTypeObj   */
                                               INPUT ipdInsurerObj,                    /* ipdInsurerObj    */
                                               INPUT ipiOptionCode,                    /* ipiOptionCode    */
                                               INPUT ipdAuthDate,                      /* ipdEffectiveDate */
                                               INPUT ipcProviderType,                  /* ipcProviderType  */
                                               INPUT ipiNegNumber,                     /* ipiNegNumber     */
                                               INPUT STRING(ipiPrType,'999'),          /* ipcDiscipline    */
                                               INPUT STRING(ipiSubPrType,'999'),       /* ipcSubDiscipline */
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).

  FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE ttAuthTypeConfig THEN
  DO:
    ASSIGN oplValidClaimType = FALSE
           opcValidMessage   = SUBSTITUTE("No auth type record available for auth type obj = &1",ipdAuthTypeObj).
    RETURN.
  END.  /* IF NOT AVAILABLE ttAuthTypeConfig THEN */

  /*
     Do not allow claim types that are specified on the restricted list for the auth type
  */
  IF ttAuthTypeConfig.ClaimTypesDisallow <> "" AND LOOKUP(ipcClaimType,ttAuthTypeConfig.ClaimTypesDisallow,",":U) > 0 THEN
  DO:
    mipEnv:miUtility:getMessageText(INPUT "ma_MsgAuth":U,INPUT "5":U, INPUT "ENG":U, OUTPUT cMessageText, OUTPUT cMessageDescription, OUTPUT cMessageHelp).

    ASSIGN oplValidClaimType = FALSE
           opcValidMessage   = SUBSTITUTE(cMessageText, ipcClaimType, STRING(ipiOptionCode)).

    RETURN.
  END. /* IF LOOKUP(STRING(ipiClaimCode,"999"),tt_auth_type_control.claim_codes_disallow,",":U) > 0 */

  /*
    Validate the claim type against the default claim type setup for the auth type
  */
  IF  ttAuthTypeConfig.ClaimTypes <> ""
  AND ttAuthTypeConfig.ClaimTypeUpdAllowed = no
  AND LOOKUP(ipcClaimType,ttAuthTypeConfig.ClaimTypes) = 0 THEN
    ASSIGN opcValidMessage   = SUBSTITUTE("The claim type (&1) is not listed in the valid claim types (&2) for the auth type control.",
                                          ipcClaimType,ttAuthTypeConfig.ClaimTypes).
           oplValidClaimType = FALSE.

  IF  ttAuthTypeConfig.DefaultClaimTypeDetail <> ""
  AND ttAuthTypeConfig.ClaimTypeUpdAllowed = no
  AND LOOKUP(ipcClaimType,ttAuthTypeConfig.DefaultClaimTypeDetail) = 0 THEN
    ASSIGN opcValidMessage   = SUBSTITUTE("The claim type (&1) is not listed in the valid claim types (&2) for the auth type provider details.",
                                          ipcClaimType,ttAuthTypeConfig.DefaultClaimTypeDetail)
           oplValidClaimType = FALSE.

  /*
    Validate that the claim type is set up on the auth type
  */
  IF  ipcAuthLevel = "hat_auth_detail":U       // only run for auth detail level
  AND ttAuthTypeConfig.ValidClaimTypesDetail <> "":U
  AND LOOKUP( ipcClaimType ,ttAuthTypeConfig.ValidClaimTypesDetail) = 0
  THEN
    ASSIGN oplValidClaimType = FALSE      /* Error */
           opcValidMessage   = SUBSTITUTE("The claim type (&1) is not listed in the valid claim types (&2) for the auth type provider control." ,
                                           ipcClaimType,
                                           ttAuthTypeConfig.ValidClaimTypesDetail).

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_changeDetailLineValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _changeDetailLineValues Procedure
PROCEDURE _changeDetailLineValues :
/*------------------------------------------------------------------------------
  Purpose   : Procedure for changing detail lines, qty, line restrictions
              and amount auth to be the same for detail lines in the same auth group
  Parameters:
  Notes     : MMP-704
  Author    :
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 010195 &THEN
  {ma/app/maauthbuschangedetaillinevalues.i}
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkDependantWaitRules) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkDependantWaitRules Procedure
PROCEDURE _checkDependantWaitRules :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER    BUFFER    btt_auth       FOR tt_auth.
  DEFINE INPUT        PARAMETER ipdBenefitDate AS DATE              NO-UNDO.
  DEFINE INPUT        PARAMETER iplWarnOnly    AS LOGICAL           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER oErrorObject   AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cBlockMessage         AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE cRuleCode             AS CHARACTER              NO-UNDO.

  IF btt_auth.end_date = ? OR ipdBenefitDate < btt_auth.end_date THEN
    ASSIGN cRuleCode = "DependantWaitPart":U.
  ELSE
    ASSIGN cRuleCode = "DependantWaitEntire":U.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                 INPUT  btt_auth.option_code,
                                                 INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                 INPUT  cRuleCode,
                                                 INPUT  btt_auth.start_date,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  IF NOT lValidRule THEN
  DO:
    ASSIGN cRuleCode = "DependantWait":U.

    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                   INPUT  btt_auth.option_code,
                                                   INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                   INPUT  cRuleCode,
                                                   INPUT  btt_auth.start_date,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).
  END.  /* IF NOT lValidRule THEN */

  IF lValidRule THEN
  DO:
    IF cRuleCode = "DependantWaitPart":U
    THEN
       ASSIGN cBlockMessage = "Authorisation dates overlapping with dependant waiting period.".
    ELSE
       ASSIGN cBlockMessage = "Authorisation in dependant waiting period.".

    RUN _validateRuleSetup IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                 INPUT cRuleCode,
                                                 INPUT cRuleValue,
                                                 INPUT iplWarnOnly,
                                                 INPUT cBlockMessage,
                                                 INPUT-OUTPUT oErrorObject).
  END.  /* IF (DependantWait) lValidRule THEN */
  ELSE
    ASSIGN cBlockMessage = "Authorisation in dependant waiting period." +
                           "[HELP=Auth Rule Code: " + cRuleCode + "]"
           lSuccess      = oErrorObject:addError
                                         (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                          INPUT "":U,                  /* ipcOwningEntityKey       */
                                          INPUT "start_date",          /* ipcFieldName             */
                                          INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                          INPUT cBlockMessage,         /* ipcMessageText           */
                                          INPUT "ERR":U).              /* ipcMessageType           */

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkForDateChanges) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkForDateChanges Procedure
PROCEDURE _checkForDateChanges :
/*-------------------------------------------------------------------------------------
  Purpose   : Apply date changes from auth header to coding, provider and detail lines
              Apply date changes from provider to detail lines
  Parameters:
  Notes     :
---------------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cValidMessage     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE lEnableProcedureDate       AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE cProcedureDateAction       AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cDefaultDate               AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE oCPSearch                  AS ma.cls.macptsearch          NO-UNDO.

  DEFINE BUFFER buf_auth             FOR hat_auth.
  DEFINE BUFFER btt_auth             FOR tt_auth.
  DEFINE BUFFER btt_auth_provider    FOR tt_auth_provider.
  DEFINE BUFFER buf_auth_provider    FOR hat_auth_provider.
  DEFINE BUFFER btt_auth_coding      FOR tt_auth_coding.
  DEFINE BUFFER btt_auth_detail      FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_mc_savings  FOR tt_auth_mc_savings.

  DATASET dsCpt:EMPTY-DATASET().

  /*
    Fetch CPT data required for CPT validations
  */
  oCPSearch = NEW ma.cls.macptsearch(DATASET dsCpt:HANDLE).

  /*
    We need to find all the modifiers for all the CPT's on the auth, because modifier 51 validation
    further down checks the CPT modifiers on other CPT's for the auth as well.
  */
  ASSIGN
     lSuccess = oCPSearch:SetCriteria("BufferList":U, "tt_cpt_link,tt_cpt,tt_cpt_modifier":U). /* Restrict the buffers to be filled by the data service */


  FOR EACH btt_auth
     WHERE btt_auth.auth_obj > 0.00:

    FIND buf_auth NO-LOCK
      WHERE buf_auth.auth_obj = btt_auth.auth_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE buf_auth THEN
    DO:
      IF btt_auth.start_date <> buf_auth.start_date
      OR btt_auth.start_ampm <> buf_auth.start_ampm
      OR btt_auth.end_date   <> buf_auth.end_date
      OR btt_auth.end_ampm   <> buf_auth.end_ampm THEN
      DO:
        /*
          Prevent the change of the start date if the quantity/amount paid on the detail line is not zero
        */
        IF CAN-FIND(FIRST  btt_auth_detail
                    WHERE  btt_auth_detail.auth_obj   = buf_auth.auth_obj
                      AND (btt_auth_detail.start_date < btt_auth.start_date
                       OR  btt_auth_detail.end_date   > btt_auth.end_date)
                      AND (btt_auth_detail.quantity_paid <> 0
                       OR  btt_auth_detail.amount_paid   <> 0))
        THEN DO:
          ASSIGN cValidMessage = "Claims paid against authorisation. Cannot shorten authorisation period.":U.

          goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                 INPUT "":U,                  /* ipcOwningEntityKey       */
                                 INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                 INPUT cValidMessage,         /* ipcMessageText           */
                                 INPUT "ERR":U).              /* ipcMessageType           */
        END. /* IF btt_auth.start_date <> buf_auth.start_date AND  */

        FOR EACH btt_auth_provider
           WHERE btt_auth_provider.auth_obj = buf_auth.auth_obj:
          IF btt_auth_provider.record_action = "DELETE":U
          THEN NEXT.

          IF btt_auth.start_date         <> buf_auth.start_date AND
             btt_auth_provider.start_date = buf_auth.start_date AND
             btt_auth_provider.start_ampm = buf_auth.start_ampm
          THEN ASSIGN btt_auth_provider.record_action = "MODIFY":U
                      btt_auth_provider.start_date    = btt_auth.start_date
                      btt_auth_provider.start_ampm    = btt_auth.start_ampm.

          IF btt_auth.start_date           = buf_auth.start_date AND
             btt_auth.start_ampm          <> buf_auth.start_ampm AND
             btt_auth_provider.start_date  = buf_auth.start_date AND
             btt_auth_provider.start_ampm  = buf_auth.start_ampm
          THEN ASSIGN btt_auth_provider.record_action = "MODIFY":U
                      btt_auth_provider.start_ampm     = btt_auth.start_ampm.

          IF btt_auth.end_date          <> buf_auth.end_date AND
             btt_auth_provider.end_date =  buf_auth.end_date AND
            (btt_auth_provider.end_ampm =  buf_auth.end_ampm OR
             buf_auth.end_ampm          = ?)
          THEN ASSIGN btt_auth_provider.record_action = "MODIFY":U
                      btt_auth_provider.end_date      = btt_auth.end_date
                      btt_auth_provider.end_ampm      = btt_auth.end_ampm.

          IF btt_auth.end_date           = buf_auth.end_date AND
             btt_auth.end_ampm          <> buf_auth.end_ampm AND
             btt_auth_provider.end_date  = buf_auth.end_date AND
             btt_auth_provider.end_ampm  = buf_auth.end_ampm
          THEN ASSIGN btt_auth_provider.record_action = "MODIFY":U
                      btt_auth_provider.end_ampm      = btt_auth.end_ampm.
        END. /* FOR EACH btt_auth_provider */

        FOR EACH btt_auth_coding
           WHERE btt_auth_coding.auth_obj = buf_auth.auth_obj:
          IF btt_auth_coding.record_action = "DELETE":U
          THEN NEXT.

          IF btt_auth.start_date       <> buf_auth.start_date AND
             btt_auth_coding.start_date = buf_auth.start_date
          THEN ASSIGN btt_auth_coding.record_action = "MODIFY":U
                      btt_auth_coding.start_date    = btt_auth.start_date.

          ASSIGN lSuccess = oCPSearch:SetFilterCriteria("tt_cpt_link.cpt_link_obj":U, "=":U, btt_auth_coding.owning_obj).
          oCPSearch:fetchCptData().

          FOR EACH  tt_cpt NO-LOCK
             WHERE  tt_cpt.cpt_code        = btt_auth_coding.owning_alt_value
               AND  tt_cpt.effective_date <= btt_auth_coding.start_date
               AND (tt_cpt.end_date       >= btt_auth_coding.start_date
                OR  tt_cpt.end_date        = ?)
                BY  tt_cpt.effective_date DESCENDING:

            mipEnv:Health:Authbusinesslogic:activateProcedureDate(INPUT  btt_auth.insurer_obj,
                                                                  INPUT  btt_auth.option_code,
                                                                  INPUT  btt_auth_coding.start_date,
                                                                  INPUT  btt_auth_coding.owning_alt_value,
                                                                  INPUT  tt_cpt.service_type,
                                                                  OUTPUT lEnableProcedureDate,
                                                                  OUTPUT cProcedureDateAction).

            IF lEnableProcedureDate THEN
            DO:
              ASSIGN cDefaultDate = IF NUM-ENTRIES(cProcedureDateAction,"|":U) > 3 THEN ENTRY(4,cProcedureDateAction,"|")  ELSE "".

              IF (btt_auth_coding.procedure_date = ? AND cDefaultDate = "DefaultDate":U)
              OR (btt_auth_coding.procedure_date <> ? AND cDefaultDate = "DefaultDate":U
              AND ((btt_auth_coding.procedure_date <= btt_auth.start_date AND btt_auth_coding.procedure_date <> ? AND btt_auth.start_date <> ?)
              OR  (btt_auth_coding.procedure_date >= btt_auth.end_date   AND btt_auth_coding.procedure_date <> ? AND btt_auth.end_date   <> ?)))
              THEN
                ASSIGN btt_auth_coding.record_action  = "MODIFY":U
                       btt_auth_coding.procedure_date = btt_auth.start_date.
              ELSE IF cDefaultDate <> "DefaultDate":U THEN
                ASSIGN btt_auth_coding.record_action  = "MODIFY":U
                       btt_auth_coding.procedure_date = ?.
            END. /* IF  lEnableProcedureDate THEN */
            ELSE
              ASSIGN btt_auth_coding.record_action  = "MODIFY":U
                     btt_auth_coding.procedure_date = ?.
          END. /* FOR EACH  tt_cpt NO-LOCK */


          IF btt_auth.end_date       <> buf_auth.end_date AND
             btt_auth_coding.end_date = buf_auth.end_date
          THEN ASSIGN btt_auth_coding.record_action = "MODIFY":U
                      btt_auth_coding.end_date      = btt_auth.end_date.
        END. /* FOR EACH btt_auth_coding */

        FOR EACH btt_auth_detail
           WHERE btt_auth_detail.auth_obj = buf_auth.auth_obj:
          IF btt_auth_detail.record_action = "DELETE":U
          THEN NEXT.

          IF btt_auth_detail.quantity_los <> 0
          THEN DO:
            IF btt_auth_detail.record_action <> "MODIFY":U THEN
              ASSIGN btt_auth_detail.record_action = "MODIFY":U
                     btt_auth_detail.loc_sequence  = btt_auth_detail.loc_sequence + 100. /* Doing this to force a recalc when auth dates are updated */

            NEXT.
          END. /* IF btt_auth_detail.quantity_los <> 0 */

          IF btt_auth.start_date        <> buf_auth.start_date AND
             btt_auth_detail.start_date =  buf_auth.start_date AND
             btt_auth_detail.start_ampm =  buf_auth.start_ampm
          THEN ASSIGN btt_auth_detail.record_action = "MODIFY":U
                      btt_auth_detail.start_date    = btt_auth.start_date
                      btt_auth_detail.start_ampm    = btt_auth.start_ampm.

          IF btt_auth.start_date         = buf_auth.start_date AND
             btt_auth.start_ampm        <> buf_auth.start_ampm AND
             btt_auth_detail.start_date  = buf_auth.start_date AND
             btt_auth_detail.start_ampm  = buf_auth.start_ampm
          THEN ASSIGN btt_auth_detail.record_action = "MODIFY":U
                      btt_auth_detail.start_ampm    = btt_auth.start_ampm.

          IF btt_auth.end_date          <> buf_auth.end_date AND
             btt_auth_detail.end_date   =  buf_auth.end_date AND
            (btt_auth_detail.end_ampm   =  buf_auth.end_ampm OR
             buf_auth.end_ampm          = ?)
          THEN ASSIGN btt_auth_detail.record_action = "MODIFY":U
                      btt_auth_detail.end_date      = btt_auth.end_date
                      btt_auth_detail.end_ampm      = btt_auth.end_ampm.

          IF btt_auth.end_date           = buf_auth.end_date AND
             btt_auth.end_ampm          <> buf_auth.end_ampm AND
             btt_auth_detail.end_date    = buf_auth.end_date AND
             btt_auth_detail.end_ampm    = buf_auth.end_ampm
          THEN ASSIGN btt_auth_detail.record_action = "MODIFY":U
                      btt_auth_detail.end_ampm      = btt_auth.end_ampm.
        END. /* FOR EACH btt_auth_detail */

        /*
          When updating auth header start date, update savings lines if dates are the same
        */
        FOR EACH btt_auth_mc_savings
           WHERE btt_auth_mc_savings.auth_obj   =  buf_auth.auth_obj
             AND btt_auth_mc_savings.saving_date =  buf_auth.start_date:
          IF btt_auth_mc_savings.record_action = "DELETE":U
          THEN NEXT.

          IF btt_auth.start_date            <> buf_auth.start_date AND
             btt_auth_mc_savings.saving_date = buf_auth.start_date
          THEN ASSIGN btt_auth_mc_savings.record_action = "MODIFY":U
                      btt_auth_mc_savings.saving_date   = btt_auth.start_date.
        END. /* FOR EACH btt_auth_mc_savings */
      END.  /* IF btt_auth.start_date <> buf_auth.start_date  OR... */

      /*
        Check if provider dates changed
      */
      FOR EACH btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj:
        FIND FIRST buf_auth_provider NO-LOCK
             WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE buf_auth_provider AND
          (btt_auth_provider.start_date <> buf_auth_provider.start_date OR
           btt_auth_provider.start_ampm <> buf_auth_provider.start_ampm OR
           btt_auth_provider.end_date   <> buf_auth_provider.end_date   OR
           btt_auth_provider.end_ampm   <> buf_auth_provider.end_ampm)
        THEN DO:
          /*
            Prevent the change of the start date if the quantity/amount paid on the detail line is not zero
          */
          IF CAN-FIND(FIRST  btt_auth_detail
                      WHERE  btt_auth_detail.auth_obj          = buf_auth.auth_obj
                        AND  btt_auth_detail.auth_provider_obj = buf_auth_provider.auth_provider_obj
                        AND (btt_auth_detail.start_date        < btt_auth_provider.start_date
                         OR  btt_auth_detail.end_date          > btt_auth_provider.end_date)
                        AND (btt_auth_detail.quantity_paid <> 0
                         OR  btt_auth_detail.amount_paid   <> 0))
          THEN DO:
            ASSIGN cValidMessage = "Claims paid against authorisation. Cannot shorten authorisation provider period.":U.

            goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                   INPUT "":U,                  /* ipcOwningEntityKey       */
                                   INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                   INPUT cValidMessage,         /* ipcMessageText           */
                                   INPUT "ERR":U).              /* ipcMessageType           */
          END. /* IF btt_auth.start_date <> buf_auth.start_date AND  */

          FOR EACH btt_auth_detail
             WHERE btt_auth_detail.auth_obj          = buf_auth_provider.auth_obj
               AND btt_auth_detail.auth_provider_obj = buf_auth_provider.auth_provider_obj
               AND btt_auth_detail.start_date        = buf_auth_provider.start_date
               AND (btt_auth_detail.start_date      <> btt_auth_provider.start_date
                OR (btt_auth_detail.start_date       = btt_auth_provider.start_date
               AND  btt_auth_detail.start_ampm      <> btt_auth_provider.start_ampm)):
            IF btt_auth_detail.record_action = "DELETE":U
            THEN NEXT.

            IF btt_auth_provider.los_calculation AND btt_auth_detail.quantity_los <> 0
            THEN DO:
              IF btt_auth_detail.record_action <> "MODIFY":U
              THEN
                ASSIGN btt_auth_detail.record_action = "MODIFY":U
                       btt_auth_detail.loc_sequence  = btt_auth_detail.loc_sequence + 100. /* Doing this to force a recalc when auth dates are updated */

              NEXT.
            END. /* IF btt_auth_detail.quantity_los <> 0 */

            ASSIGN btt_auth_detail.record_action = "MODIFY":U
                   btt_auth_detail.start_date    = btt_auth_provider.start_date
                   btt_auth_detail.start_ampm    = btt_auth_provider.start_ampm.
          END. /* FOR EACH btt_auth_detail */

          FOR EACH btt_auth_detail
             WHERE btt_auth_detail.auth_obj          = buf_auth_provider.auth_obj
               AND btt_auth_detail.auth_provider_obj = buf_auth_provider.auth_provider_obj
               AND btt_auth_detail.end_date          = buf_auth_provider.end_date
               AND (btt_auth_detail.end_date        <> btt_auth_provider.end_date
                OR (btt_auth_detail.end_date         = btt_auth_provider.end_date
               AND  btt_auth_detail.end_ampm        <> btt_auth_provider.end_ampm)):
            IF btt_auth_detail.record_action = "DELETE":U
            THEN NEXT.

            IF btt_auth_detail.quantity_los <> 0
            THEN DO:
              IF btt_auth_detail.record_action <> "MODIFY":U THEN
                ASSIGN btt_auth_detail.record_action = "MODIFY":U
                       btt_auth_detail.loc_sequence  = btt_auth_detail.loc_sequence + 100. /* Doing this to force a recalc when auth dates are updated */

              NEXT.
            END.  /* IF btt_auth_detail.quantity_los <> 0 */

            ASSIGN btt_auth_detail.record_action = "MODIFY":U
                   btt_auth_detail.end_date      = btt_auth_provider.end_date
                   btt_auth_detail.end_ampm      = btt_auth_provider.end_ampm.
          END. /* FOR EACH btt_auth_detail */
        END. /* IF btt_auth_provider.start_date <> buf_auth_provider.start_date OR  */
      END. /* FOR EACH btt_auth_provider NO-LOCK */
    END.  /* IF AVAILABLE buf_auth THEN */
  END. /*FOR EACH btt_auth*/

  { mip/inc/mipcatcherror.i
      &FINALLY = "IF VALID-OBJECT(oCPSearch) THEN DELETE OBJECT oCPSearch."}

&ENDIF


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkLineStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkLineStatus Procedure
PROCEDURE _checkLineStatus :
/*------------------------------------------------------------------------------
  Purpose:     Validate if there are any pending, cancelled, declined or
               requested primary/main lines for the auth.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthObj            AS DECIMAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcStatus             AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcLine               AS CHARACTER NO-UNDO.

  /*
     Check if there are any Pending lines
  */
  ASSIGN opcLine = fnInvalidLineStatus(ipdAuthObj,0).
  IF opcLine <> "" THEN
  DO:
    ASSIGN opcStatus = "pending".
    RETURN.
  END.  /* IF opcLine <> "" THEN */

  /*
     Check if there are any Cancelled lines
  */
  ASSIGN opcLine = fnInvalidLineStatus(ipdAuthObj,5).
  IF opcLine <> "" THEN
  DO:
    ASSIGN opcStatus = "cancelled".
    RETURN.
  END.  /* IF opcLine <> "" THEN */

  /*
     Check if there are any Declined lines
  */
/*   ASSIGN opcLine = fnInvalidLineStatus(iplDefaultAuthHeadInfo , 6). */
  ASSIGN opcLine = fnInvalidLineStatus(ipdAuthObj,6).
  IF opcLine <> "" THEN
  DO:
    ASSIGN opcStatus = "declined".
    RETURN.
  END.  /* IF opcLine <> "" THEN */

  /*
     Check if there are any Requested lines
  */
/*   ASSIGN opcLine = fnInvalidLineStatus(iplDefaultAuthHeadInfo , 7). */
  ASSIGN opcLine = fnInvalidLineStatus(ipdAuthObj,7).
  IF opcLine <> "" THEN
  DO:
    ASSIGN opcStatus = "requested".
    RETURN.
  END.  /* IF opcLine <> "" THEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkNappiExclRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkNappiExclRule Procedure
PROCEDURE _checkNappiExclRule :
/*------------------------------------------------------------------------------
  Purpose:     For nappi's that are exclusions, we need to handle the error
               according to the Nappi Exclusion rule setup.
  Parameters:  Auth clinical detail record
               Exclusion Details
               Exclusion Option Note
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER    btt_auth_detail FOR tt_auth_detail.
  DEFINE PARAMETER BUFFER    buf_auth_schext FOR schext.
  DEFINE INPUT     PARAMETER ipcExclusion    AS  CHARACTER NO-UNDO.
  DEFINE INPUT     PARAMETER ipcNote         AS  CHARACTER NO-UNDO.

  DEFINE VARIABLE cStatusDesc     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cError          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleLoadStatus AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cStatusReason   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAuthStatus     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lAcknowledge    AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidStatus    AS LOGICAL     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,       // ipdInsurerObj
                                                 INPUT  goAuthorisation:OptionCode,       // ipiOptionCode
                                                 INPUT  "ma_acAuthRuleTypeAuthDetail":U,  // ipcRuleType
                                                 INPUT  "NappiExclusion":U,               // ipcRuleCode
                                                 INPUT  goAuthorisation:StartDate,        // ipdEffectiveDate
                                                 OUTPUT lValidRule,                       // oplValidRule
                                                 OUTPUT cRuleValue).                      // opcRuleValue

  IF NOT lValidRule THEN
  DO:
    ASSIGN cError   = "Nappi " + btt_auth_detail.related_value + " is an exclusion item but the Auth Rule setup is invalid." +
                      "[HELP=Auth Rule Code: NappiExclusion]":U
           lSuccess = goErrorObject:addError
                                 (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                  INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                  INPUT "":U,                                                 // ipcOwningEntityKey
                                  INPUT "related_value":U,                                    // ipcFieldName
                                  INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                  INPUT cError,                                               // ipcMessageText
                                  INPUT "ERR":U).                                             // ipcMessageType

    RETURN.
  END.  // IF NOT lValidRule THEN

  CASE ENTRY(1,cRuleValue,"|":U):
    WHEN "BLOCK":U THEN
    DO:
      ASSIGN cError   = "Nappi " + btt_auth_detail.related_value + " is an exclusion item, with value '"
                      + TRIM(ipcExclusion,".") + "', but is not allowed."
                      + "[HELP=Auth Rule Code: NappiExclusion]":U
             lSuccess = goErrorObject:addError
                                   (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                    INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                    INPUT "":U,                                                 // ipcOwningEntityKey
                                    INPUT "related_value":U,                                    // ipcFieldName
                                    INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                    INPUT cError,                                               // ipcMessageText
                                    INPUT "ERR":U).                                             // ipcMessageType
      RETURN.
    END.  // WHEN "BLOCK":U

    WHEN "LOAD":U THEN
    DO:
      IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN
      DO:
        ASSIGN cRuleLoadStatus = ENTRY(2,cRuleValue,"|":U)
               cError          = "".

        IF cRuleLoadStatus = ""
        THEN
          ASSIGN iAuthStatus = 6      // Declined
                 cStatusReason = "" no-error.
        ELSE
          ASSIGN iAuthStatus   = INTEGER(ENTRY(1,cRuleLoadStatus))
                 cStatusReason = ipcNote NO-ERROR.

        IF ERROR-STATUS:ERROR
        THEN
          ASSIGN cError   = "Invalid status (" + ENTRY(1,cRuleLoadStatus) + ") specified on Auth Rule setup. Please check."
                          + "[HELP=Auth Rule Code: NappiExclusion]":U.
        ELSE DO:
          ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  iAuthStatus,
                                                                             INPUT  "System":U).
          IF NOT lValidStatus
          THEN
            ASSIGN cError = "Invalid status (" + STRING(iAuthStatus) + ") specified on Auth Rule setup. Please check."
                          + "[HELP=Auth Rule Code: NappiExclusion, ValidStatuses]":U.
        END.  // ELSE - IF ERROR-STATUS:ERROR THEN

        IF cError <> "" THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                              (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                               INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                               INPUT "":U,                                                 // ipcOwningEntityKey
                               INPUT "related_value":U,                                    // ipcFieldName
                               INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                               INPUT cError,                                               // ipcMessageText
                               INPUT "ERR":U).                                             // ipcMessageType
          RETURN.
        END.  // IF cError <> "" THEN
        ELSE
          ASSIGN btt_auth_detail.auth_status      = iAuthStatus
                 btt_auth_detail.auth_status_note = cStatusReason.

      END.  // IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN
      ELSE
        ASSIGN btt_auth_detail.auth_status      = 6 // Declined
               btt_auth_detail.auth_status_note = "":U.

      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,       // ipdInsurerObj
                                                     INPUT  goAuthorisation:OptionCode,       // ipiOptionCode
                                                     INPUT  "ma_acAuthRuleTypeAuthDetail":U,  // ipcRuleType
                                                     INPUT  "NappiExclusionWarn":U,           // ipcRuleCode
                                                     INPUT  goAuthorisation:StartDate,        // ipdEffectiveDate
                                                     OUTPUT lValidRule,                       // oplValidRule
                                                     OUTPUT cRuleValue                        // opcRuleValue
                                                     ).
      IF lValidRule THEN
      DO:
        IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
        DO:
          ASSIGN cStatusDesc = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                              INPUT btt_auth_detail.auth_status)
                 cError      = "Nappi " + btt_auth_detail.related_value + " is an exclusion item with value '"
                             + trim(ipcExclusion,".") + "'. Status is changed to '" + cStatusDesc
                             + IF btt_auth_detail.auth_status_note <> ""
                               THEN
                                 " - " + btt_auth_detail.auth_status_note + "'."
                               ELSE
                                 "'."
                 lAcknowledge = (IF cRuleValue = "WARNACK":U THEN TRUE ELSE FALSE).

          IF cStatusDesc = "":U
          THEN
            ASSIGN cError   = "Attempt to change authorisation detail line status to " + STRING(btt_auth_detail.auth_status)
                            + " was uncesuccessful. Status description is invalid."
                            + "[HELP=Auth Rule Code: NappiExclusionWarn]":U
                   lSuccess = goErrorObject:addError(INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                                     INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                                     INPUT "":U,                                                 // ipcOwningEntityKey
                                                     INPUT "related_value":U,                                    // ipcFieldName
                                                     INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                                     INPUT cError,                                               // ipcMessageText
                                                     INPUT "ERR":U).                                             // ipcReplaceTextList
          ELSE
            ASSIGN cError   = cError + "[HELP=Auth Rule Code: NappiExclusion, NappiExclusionWarn]":U
                   lSuccess = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,    // ipcOwningEntityMnemonic
                                                     INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                                     INPUT "":U,                                                 // ipcOwningEntityKey
                                                     INPUT "related_value":U,                                    // ipcFieldName
                                                     INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                                     INPUT cError,                                               // ipcMessageText
                                                     INPUT "WAR":U,                                              // ipcMessageType
                                                     INPUT lAcknowledge).                                        // ipcAcknowledge
        END.  // IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
        ELSE
          ASSIGN cError   = "Auth Rule Value: ":U + cRuleValue + ",[HELP=Auth Rule Code: NappiExclusionWarn]":U
                 lSuccess = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,      // ipcOwningEntityMnemonic
                                                 INPUT btt_auth_detail.auth_detail_obj,                          // ipdOwningEntityObj
                                                 INPUT "":U,                                                     // ipcOwningEntityKey
                                                 INPUT "auth_status":U,                                          // ipcFieldName
                                                 INPUT btt_auth_detail.line_number,                              // ipiLineNumber
                                                 INPUT "MA":U,                                                   // ipcMessageGroup
                                                 INPUT 112,  /* The '&1' specified is invalid. &2 */             // ipiMessageNumber
                                                 INPUT cError).                                                  // ipcReplaceTextList
      END. // IF lValidRule THEN

      IF iAuthStatus = 6
      THEN
        ASSIGN btt_auth_detail.claim_code = buf_auth_schext.claim-code[1].

    END.  // WHEN "LOAD":U

    OTHERWISE DO:
      ASSIGN cError   = "Invalid Auth Rule setup.[HELP=Auth Rule Code: NappiExclusion]":U
             lSuccess = goErrorObject:addError
                              (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                               INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                               INPUT "":U,                                                 // ipcOwningEntityKey
                               INPUT "related_value":U,                                    // ipcFieldName
                               INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                               INPUT cError,                                               // ipcMessageText
                               INPUT "ERR":U).                                             // ipcMessageType
    END.  // OTHERWISE DO
  END CASE.

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkNonChargeRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkNonChargeRule Procedure
PROCEDURE _checkNonChargeRule :
/*------------------------------------------------------------------------------
  Purpose:     For nappi's that are non-chargeable, we need to handle the error
               according to the rule setups.
               Depending on the non-chargeable type, one of the following rules
               will be used:
               - NonChargeTheatre
               - NonChargeWard
  Parameters:  Either 'Theatre' or 'Ward' will be passed in.
  Notes:
------------------------------------------------------------------------------*/
  DEFINE       PARAMETER BUFFER btt_auth_detail  FOR tt_auth_detail.
  DEFINE       PARAMETER BUFFER buf_auth_schext  FOR schext.
  DEFINE INPUT PARAMETER        ipcNonChargeType AS CHARACTER NO-UNDO.  /* Theatre or Ward */

  DEFINE VARIABLE cStatusDesc     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cError          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleCode       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleLoadStatus AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cStatusReason   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAuthStatus     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lAcknowledge    AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidStatus    AS LOGICAL     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  ASSIGN cRuleCode = "NonCharge":U + TRIM(ipcNonChargeType).

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,       // ipdInsurerObj
                                                 INPUT  goAuthorisation:OptionCode,       // ipiOptionCode
                                                 INPUT  "ma_acAuthRuleTypeAuthDetail":U,  // ipcRuleType
                                                 INPUT  cRuleCode,                        // ipcRuleCode
                                                 INPUT  goAuthorisation:StartDate,        // ipdEffectiveDate
                                                 OUTPUT lValidRule,                       // oplValidRule
                                                 OUTPUT cRuleValue                        // opcRuleValue
                                                 ).

  IF NOT lValidRule THEN
  DO:
    ASSIGN cError   = "Nappi " + btt_auth_detail.related_value + " is a non-chargeable item in "
                    + ipcNonChargeType + " and is not allowed."
                    + "[HELP=Auth Rule Code: ":U + cRuleCode + "]":U
           lSuccess = goErrorObject:addError
                                 (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                  INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                  INPUT "":U,                                                 // ipcOwningEntityKey
                                  INPUT "related_value":U,                                    // ipcFieldName
                                  INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                  INPUT cError,                                               // ipcMessageText
                                  INPUT "ERR":U).                                             // ipcMessageType

    RETURN.
  END.  // IF NOT lValidRule THEN

  CASE ENTRY(1,cRuleValue,"|":U):
    WHEN "BLOCK":U THEN
    DO:
      ASSIGN cError   = "Nappi " + btt_auth_detail.related_value + " is a non-chargeable item in "
                      + ipcNonChargeType + " and is not allowed."
                      + "[HELP=Auth Rule Code: ":U + cRuleCode + "]":U
             lSuccess = goErrorObject:addError
                                   (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                    INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                    INPUT "":U,                                                 // ipcOwningEntityKey
                                    INPUT "related_value":U,                                    // ipcFieldName
                                    INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                    INPUT cError,                                               // ipcMessageText
                                    INPUT "ERR":U).                                             // ipcMessageType
      RETURN.
    END.  // WHEN "BLOCK":U

    WHEN "LOAD":U THEN
    DO:
      IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN
      DO:
        ASSIGN cRuleLoadStatus = ENTRY(2,cRuleValue,"|":U)
               cError          = "".
        IF cRuleLoadStatus = ""
        THEN
          ASSIGN
            iAuthStatus   = 6  // Declined
            cStatusReason = "".
        ELSE
          ASSIGN
            iAuthStatus   = INTEGER(ENTRY(1,cRuleLoadStatus))
            cStatusReason = IF NUM-ENTRIES(cRuleLoadStatus) > 1
                            THEN ENTRY(2,cRuleLoadStatus)
                            ELSE ""
            NO-ERROR.

        IF ERROR-STATUS:ERROR
        THEN
          ASSIGN cError   = "Invalid status (" + ENTRY(1,cRuleLoadStatus) + ") specified on Auth Rule setup. Please check."
                          + "[HELP=Auth Rule Code: ":U + cRuleCode + "]":U.
        ELSE DO:
          ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  iAuthStatus,
                                                                             INPUT  "System":U).
          IF NOT lValidStatus
          THEN
            ASSIGN cError = "Invalid status (" + STRING(iAuthStatus) + ") specified on Auth Rule setup. Please check."
                          + "[HELP=Auth Rule Code: ":U + cRuleCode + ", ValidStatuses]":U.
        END.  // ELSE - IF ERROR-STATUS:ERROR THEN

        IF cError <> "" THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                              (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                               INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                               INPUT "":U,                                                 // ipcOwningEntityKey
                               INPUT "related_value":U,                                    // ipcFieldName
                               INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                               INPUT cError,                                               // ipcMessageText
                               INPUT "ERR":U).                                             // ipcMessageType
          RETURN.
        END.  // IF cError <> "" THEN
        ELSE
          ASSIGN btt_auth_detail.auth_status      = iAuthStatus
                 btt_auth_detail.auth_status_note = cStatusReason.

      END.  // IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN
      ELSE
        ASSIGN btt_auth_detail.auth_status      = 6 // Declined
               btt_auth_detail.auth_status_note = "":U.

      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,       // ipdInsurerObj
                                                     INPUT  goAuthorisation:OptionCode,       // ipiOptionCode
                                                     INPUT  "ma_acAuthRuleTypeAuthDetail":U,  // ipcRuleType
                                                     INPUT  "NonChargeWarn":U,                // ipcRuleCode
                                                     INPUT  goAuthorisation:StartDate,        // ipdEffectiveDate
                                                     OUTPUT lValidRule,                       // oplValidRule
                                                     OUTPUT cRuleValue                        // opcRuleValue
                                                     ).
      IF lValidRule THEN
      DO:

        IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
        DO:
          ASSIGN cStatusDesc = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                              INPUT btt_auth_detail.auth_status)
                 cError      = "Nappi " + btt_auth_detail.related_value + " is a non-chargeable item in "
                             + ipcNonChargeType + ". Status is changed to '" + cStatusDesc
                             + IF btt_auth_detail.auth_status_note <> ""
                               THEN
                                 " - " + btt_auth_detail.auth_status_note + "'."
                               ELSE
                                 "'."
                 cError      = cError + "[HELP=Auth Rule Code: " + cRuleCode + ", NonChargeWarn]":U
                 lAcknowledge = (IF cRuleValue = "WARNACK":U THEN TRUE ELSE FALSE).

          IF cStatusDesc = "":U
          THEN
            ASSIGN cError   = "Attempt to change authorisation detail line status to " + STRING(btt_auth_detail.auth_status)
                            + " was unsuccessful. Status description is invalid."
                            + "[HELP=Auth Rule Code: " + cRuleCode + ", NonChargeWarn]":U
                   lSuccess = goErrorObject:addError(INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                                                     INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                                                     INPUT "":U,                                                 // ipcOwningEntityKey
                                                     INPUT "related_value":U,                                    // ipcFieldName
                                                     INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                                                     INPUT cError,                                               // ipcMessageText
                                                     INPUT "ERR":U).                                             // ipcReplaceTextList
          ELSE
            ASSIGN lSuccess = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic, // ipcOwningEntityMnemonic
                                                     INPUT btt_auth_detail.auth_detail_obj,                   // ipdOwningEntityObj
                                                     INPUT "":U,                                              // ipcOwningEntityKey
                                                     INPUT "related_value":U,                                 // ipcFieldName
                                                     INPUT btt_auth_detail.line_number,                       // ipiLineNumber
                                                     INPUT cError,                                            // ipcMessageText
                                                     INPUT "WAR":U,                                           // ipcMessageType
                                                     INPUT lAcknowledge).                                     // ipcAcknowledge
        END.  // IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
        ELSE
          ASSIGN cError   = "Auth Rule Value: ":U + cRuleValue + ",[HELP=Auth Rule Code: NonChargeWarn]":U
                 lSuccess = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                                                   INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                                                   INPUT "":U,                                               /* ipcOwningEntityKey      */
                                                   INPUT "auth_status":U,                                    /* ipcFieldName            */
                                                   INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                                                   INPUT "MA":U,                                             /* ipcMessageGroup         */
                                                   INPUT 112,  /* The '&1' specified is invalid. &2 */       /* ipiMessageNumber        */
                                                   INPUT cError).                                            /* ipcReplaceTextList      */
      END. // IF lValidRule THEN

      IF iAuthStatus = 6
      THEN
        ASSIGN btt_auth_detail.claim_code = buf_auth_schext.claim-code[1].

    END.  // WHEN "LOAD":U

    OTHERWISE DO:
      ASSIGN cError   = "Invalid Auth Rule setup.[HELP=Auth Rule Code: ":U + cRuleCode + "]":U
             lSuccess = goErrorObject:addError
                              (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                               INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                               INPUT "":U,                                                 // ipcOwningEntityKey
                               INPUT "related_value":U,                                    // ipcFieldName
                               INPUT btt_auth_detail.line_number,                          // ipiLineNumber
                               INPUT cError,                                               // ipcMessageText
                               INPUT "ERR":U).                                             // ipcMessageType
    END.  // OTHERWISE DO
  END CASE.

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkRateControlWarningMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkRateControlWarningMessage Procedure
PROCEDURE _checkRateControlWarningMessage :
/*------------------------------------------------------------------------------
  Purpose:     To find any valid auth rate control records not picked up by
               _checkRateControlWorkgroup
  Parameters:  <none>
  Notes:       Please refer to JIRA task MMP-812 for any questions
------------------------------------------------------------------------------*/
  DEFINE        PARAMETER BUFFER btt_auth_provider    FOR tt_auth_provider.
  DEFINE OUTPUT PARAMETER opcWarningMessage           AS  CHARACTER         NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE iNegotiationGroupNumber AS INTEGER NO-UNDO.

  DEFINE BUFFER buf_auth              FOR hat_auth.
  DEFINE BUFFER buf_auth_rate_control FOR hac_auth_rate_control.

  ASSIGN opcWarningMessage = "":U.

  /* We are only interested in the main provider */
  FIND FIRST btt_auth_provider NO-LOCK
       WHERE btt_auth_provider.main_provider =  TRUE NO-ERROR.
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

  IF AVAILABLE btt_auth_provider THEN
    FIND FIRST buf_auth NO-LOCK
         WHERE buf_auth.auth_obj = btt_auth_provider.auth_obj NO-ERROR.
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

  IF AVAILABLE btt_auth_provider AND AVAILABLE buf_auth AND btt_auth_provider.record_action <> "DELETE":U THEN
  DO:
    /* We convert the btt_auth_provider._neg_group to an integer value for use by buf_auth_rate_control.main_provider_neg_num */
    ASSIGN iNegotiationGroupNumber = INTEGER(TRIM(ENTRY(1, btt_auth_provider._neg_group, ":"), "(")).

    FIND FIRST  buf_auth_rate_control NO-LOCK
         WHERE  buf_auth_rate_control.insurer_obj              = buf_auth.insurer_obj
           AND  buf_auth_rate_control.option_code              = buf_auth.option_code
           AND  buf_auth_rate_control.effective_date          <= buf_auth.start_date
           AND (buf_auth_rate_control.end_date                 = ?
             OR buf_auth_rate_control.end_date                 > buf_auth.start_date)
            AND buf_auth_rate_control.main_provider_neg_num    = iNegotiationGroupNumber
            AND buf_auth_rate_control.main_provider_base_rate  = btt_auth_provider._base_rate
            AND buf_auth_rate_control.main_provider_ars_rate   = btt_auth_provider._ars_rate
            AND buf_auth_rate_control.workgroup_obj            = 0.00
    NO-ERROR.
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE buf_auth_rate_control THEN
      FIND FIRST  buf_auth_rate_control NO-LOCK
         WHERE  buf_auth_rate_control.insurer_obj                = buf_auth.insurer_obj
           AND  buf_auth_rate_control.option_code                = buf_auth.option_code
           AND  buf_auth_rate_control.effective_date            <= buf_auth.start_date
           AND (buf_auth_rate_control.end_date                   = ?
             OR buf_auth_rate_control.end_date                   > buf_auth.start_date)
            AND buf_auth_rate_control.main_provider_neg_num      = iNegotiationGroupNumber
            AND buf_auth_rate_control.main_provider_base_rate    = "":U
            AND buf_auth_rate_control.main_provider_ars_rate     = "":U
            AND buf_auth_rate_control.workgroup_obj              = 0.00
      NO-ERROR.
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE buf_auth_rate_control THEN
      FIND FIRST  buf_auth_rate_control NO-LOCK
           WHERE  buf_auth_rate_control.insurer_obj             = buf_auth.insurer_obj
             AND  buf_auth_rate_control.option_code             = 0
             AND  buf_auth_rate_control.effective_date         <= buf_auth.start_date
             AND (buf_auth_rate_control.end_date                = ?
              OR  buf_auth_rate_control.end_date                > buf_auth.start_date)
             AND buf_auth_rate_control.main_provider_neg_num    = 0
             AND buf_auth_rate_control.main_provider_base_rate  = btt_auth_provider._base_rate
             AND buf_auth_rate_control.main_provider_ars_rate   = btt_auth_provider._ars_rate
             AND buf_auth_rate_control.workgroup_obj            = 0.00
    NO-ERROR.
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

    /* If we have managed to find a valid buf_auth_rate_control then we want to output the error message if there is one */
    IF AVAILABLE buf_auth_rate_control THEN
      ASSIGN opcWarningMessage = buf_auth_rate_control.warning_message.
  END. /* IF AVAILABLE btt_auth_provider AND AVAILABLE buf_auth THEN */

&ENDIF
  {mip/inc/mipcatcherror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkRateControlWorkgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkRateControlWorkgroup Procedure
PROCEDURE _checkRateControlWorkgroup :
/*------------------------------------------------------------------------------
  Purpose: Check if an auth has one or more providers within the same workgroup.
           If it does, we need to check if these additional providers are valid
           within the associated providers in the rate control setup for the workgroup.

           If both of the above conditions are met , we know that the workgroup applies
           to this provider
  Parameters:  Buffer - main provider buffer
               OUTPUT
  Notes:
------------------------------------------------------------------------------*/
  DEFINE        PARAMETER BUFFER btt_auth_provider    FOR tt_auth_provider .
  DEFINE OUTPUT PARAMETER opcWarningMessage           AS  CHARACTER            NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cAuthRateControlAssocPrTypeList       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAssocPrTypeEntry                     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lWorkGroupMatch                       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE iPrType                               AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iSubPrType                            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iAssocPrTypeEntry                     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iCount                                AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dStartDate                            AS DATE      NO-UNDO.
  DEFINE VARIABLE dEndDate                              AS DATE      NO-UNDO.
  DEFINE VARIABLE cRateControlWarningMessage            AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cWorkgroupMatchList                   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cWorkGroupObj                         AS CHARACTER NO-UNDO.

  DEFINE BUFFER btt_auth              FOR tt_auth.
  DEFINE BUFFER b2tt_auth_provider    FOR tt_auth_provider.
  DEFINE BUFFER buf_auth_rate_control FOR hac_auth_rate_control.

  ASSIGN opcWarningMessage = "":U.

  FIND FIRST btt_auth
       WHERE btt_auth.auth_obj = btt_auth_provider.auth_obj NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE btt_auth THEN RETURN.

  DO iCount = 1 TO NUM-ENTRIES(btt_auth_provider._workgroup_obj):

    ASSIGN cWorkGroupObj = ENTRY(iCount,btt_auth_provider._workgroup_obj).

    FIND FIRST b2tt_auth_provider
         WHERE b2tt_auth_provider.auth_obj        =  btt_auth.auth_obj
           AND b2tt_auth_provider.main_provider   =  FALSE
           AND CAN-DO(b2tt_auth_provider._workgroup_obj,cWorkGroupObj)
           AND b2tt_auth_provider.record_action   <> "DELETE":U NO-ERROR .

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE b2tt_auth_provider
    AND cWorkGroupObj <> "":U
    THEN
      ASSIGN lWorkGroupMatch     = TRUE
             cWorkGroupMatchList = IF NOT CAN-DO(cWorkGroupMatchList,cWorkGroupObj)
                                   THEN (cWorkGroupMatchList + (IF cWorkGroupMatchList = "":U THEN "":U ELSE ",":U) + cWorkGroupObj)
                                   ELSE cWorkGroupMatchList .


  END. /* DO iCount = 1 TO NUM-ENTRIES(btt_auth_provider._workgroup_obj) */

  IF NUM-ENTRIES(cWorkGroupMatchList) > 1 THEN
  DO:
    goErrorObject:addError (INPUT 'hatap' ,                                                                                        /* ipcOwningEntityMnemonic */
                            INPUT b2tt_auth_provider.auth_provider_obj,                                                            /* ipdOwningEntityObj      */
                            INPUT "":U,                                                                                            /* ipcOwningEntityKey      */
                            INPUT b2tt_auth_provider.line_number,                                                                  /* ipiLineNumber           */
                            INPUT "Please ensure that  main and associated providers belong to a single applicable workgroup.":U,  /* ipcMessageText          */
                            INPUT "ERR":U).                                                                                        /* ipcMessageType          */
    RETURN.
  END. /* IF NUM-ENTRIES(cWorkGroupMatchList)  */

  ASSIGN btt_auth_provider._workgroup_obj = ENTRY(1,cWorkGroupMatchList).

  IF lWorkGroupMatch THEN
  DO:
    /*
      Try our best to find a rate control record for this workgroup
    */

    FIND FIRST  buf_auth_rate_control NO-LOCK
         WHERE (buf_auth_rate_control.insurer_obj              = btt_auth.insurer_obj
           OR   buf_auth_rate_control.insurer_obj              = 0)
           AND  buf_auth_rate_control.option_code              = btt_auth.option_code
           AND  buf_auth_rate_control.effective_date          <= btt_auth.start_date
           AND (buf_auth_rate_control.end_date                 = ?
             OR buf_auth_rate_control.end_date                 > btt_auth.start_date)
            AND buf_auth_rate_control.workgroup_obj            = DECIMAL(btt_auth_provider._workgroup_obj)
            AND buf_auth_rate_control.main_provider_base_rate  = btt_auth_provider._base_rate
            AND buf_auth_rate_control.main_provider_ars_rate   = btt_auth_provider._ars_rate
     NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE buf_auth_rate_control
    THEN
      FIND FIRST  buf_auth_rate_control NO-LOCK
           WHERE (buf_auth_rate_control.insurer_obj             = btt_auth.insurer_obj
             OR   buf_auth_rate_control.insurer_obj             = 0)
             AND  buf_auth_rate_control.option_code             = 0
             AND  buf_auth_rate_control.effective_date         <= btt_auth.start_date
             AND (buf_auth_rate_control.end_date                = ?
              OR  buf_auth_rate_control.end_date                > btt_auth.start_date)
             AND  buf_auth_rate_control.workgroup_obj           = DECIMAL(btt_auth_provider._workgroup_obj)
             AND  buf_auth_rate_control.main_provider_base_rate = btt_auth_provider._base_rate
             AND  buf_auth_rate_control.main_provider_ars_rate  = btt_auth_provider._ars_rate
      NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

    IF AVAILABLE buf_auth_rate_control THEN
    DO:
      /*
        We have found a rate control record for this workgroup, now check the if the additional providers fall witin the associated_pr_type_list
      */

      ASSIGN cAuthRateControlAssocPrTypeList = buf_auth_rate_control.associated_pr_type_list.

      ASSOC-PR-TYPE-BLK:
      DO iAssocPrTypeEntry = 1 TO NUM-ENTRIES(cAuthRateControlAssocPrTypeList) :

        ASSIGN cAssocPrTypeEntry = ENTRY(iAssocPrTypeEntry,cAuthRateControlAssocPrTypeList)
               iPrType           = INTEGER(SUBSTRING(cAssocPrTypeEntry,1,3))
               iSubPrType        = INTEGER(SUBSTRING(cAssocPrTypeEntry,4,3)) NO-ERROR.

        {&ResetError}

        IF  iPrType    <> ?
        AND iSubPrType <> ?  THEN
        DO:
          FIND FIRST b2tt_auth_provider
               WHERE b2tt_auth_provider.auth_obj        = btt_auth.auth_obj
                 AND b2tt_auth_provider.main_provider   = FALSE
                 AND b2tt_auth_provider.pr_type         = iPrType
                 AND b2tt_auth_provider.sub_pr_type     = iSubPrType
                 AND CAN-DO(b2tt_auth_provider._workgroup_obj,btt_auth_provider._workgroup_obj) NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

          IF AVAILABLE b2tt_auth_provider
          THEN
            ASSIGN opcWarningMessage       = buf_auth_rate_control.warning_message.

        END. /*  IF cPrType <> ? AND cSubPrType <> ?   */
      END. /* ASSOC-PR-TYPE-BLK */
    END. /* IF AVAILABLE buf_auth_rate_control */
  END.  /* IF lWorkGroupMatch  */

&ENDIF
  {mip/inc/mipcatcherror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_checkTariffExclRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _checkTariffExclRule Procedure
PROCEDURE _checkTariffExclRule :
/*------------------------------------------------------------------------------
  Purpose:     For tariffs that are exclusions, we need to handle the error
               according to the Tariff Exclusion rule setup.
  Parameters:  Auth clinical detail record
               Exclusion Details
               Exclusion Option Note
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER    btt_auth_detail FOR tt_auth_detail.
  DEFINE PARAMETER BUFFER    buf_auth_schext FOR schext.
  DEFINE INPUT     PARAMETER ipcExclusion    AS  CHARACTER NO-UNDO.
  DEFINE INPUT     PARAMETER ipcNote         AS  CHARACTER NO-UNDO.

  DEFINE VARIABLE cStatusDesc     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cError          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleLoadStatus AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cStatusReason   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAuthStatus     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lAcknowledge    AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidStatus    AS LOGICAL     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  mipEnv:Health:AuthMaintenance:getAuthRuleValue
                               (INPUT  goAuthorisation:InsurerObj       // ipdInsurerObj
                               ,INPUT  0                                // ipiOptionCode
                               ,INPUT  "ma_acAuthRuleTypeAuthDetail":U  // ipcRuleType
                               ,INPUT  "TariffExclusion":U              // ipcRuleCode
                               ,INPUT  goAuthorisation:StartDate        // ipdEffectiveDate
                               ,OUTPUT lValidRule                       // oplValidRule
                               ,OUTPUT cRuleValue).                     // opcRuleValue

  IF NOT lValidRule THEN
  DO:
    ASSIGN cError   = "Tariff " + btt_auth_detail.related_value + " is an exclusion but the Auth Rule setup is invalid." +
                      "[HELP=Auth Rule Code: TariffExclusion]":U
           lSuccess = goErrorObject:addError
                                   (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  // ipcOwningEntityMnemonic
                                   ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                   ,INPUT "":U                                                 // ipcOwningEntityKey
                                   ,INPUT "related_value":U                                    // ipcFieldName
                                   ,INPUT btt_auth_detail.line_number                          // ipiLineNumber
                                   ,INPUT cError                                               // ipcMessageText
                                   ,INPUT "ERR":U).                                            // ipcMessageType

    RETURN.
  END.  // IF NOT lValidRule THEN

  CASE ENTRY(1,cRuleValue,"|":U):
    WHEN "BLOCK":U THEN
    DO:
      ASSIGN cError   = "Tariff " + btt_auth_detail.related_value + " is an exclusion, with value '"
                      + TRIM(ipcExclusion,".") + "', but is not allowed."
                      + "[HELP=Auth Rule Code: TariffExclusion]":U
             lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  // ipcOwningEntityMnemonic
                                     ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                     ,INPUT "":U                                                 // ipcOwningEntityKey
                                     ,INPUT "related_value":U                                    // ipcFieldName
                                     ,INPUT btt_auth_detail.line_number                          // ipiLineNumber
                                     ,INPUT cError                                               // ipcMessageText
                                     ,INPUT "ERR":U).                                            // ipcMessageType
      RETURN.
    END.  // WHEN "BLOCK":U

    WHEN "LOAD":U THEN
    DO:
      IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN
      DO:
        ASSIGN cRuleLoadStatus = ENTRY(2,cRuleValue,"|":U)
               cError          = "".

        IF cRuleLoadStatus     = ""
        THEN
          ASSIGN iAuthStatus   = 6      // Declined
                 cStatusReason = ipcNote NO-ERROR.
        ELSE
          ASSIGN iAuthStatus   = INTEGER(cRuleLoadStatus)
                 cStatusReason = ipcNote  NO-ERROR.

        IF ERROR-STATUS:ERROR
        THEN
          ASSIGN cError = "Invalid status (" + cRuleLoadStatus + ") specified on Auth Rule setup. Please check."
                        + "[HELP=Auth Rule Code: TariffExclusion]":U.
        ELSE DO:
          ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  iAuthStatus,
                                                                             INPUT  "System":U).
          IF NOT lValidStatus
          THEN
            ASSIGN cError = "Invalid status (" + STRING(iAuthStatus) + ") specified on Auth Rule setup. Please check."
                          + "[HELP=Auth Rule Code: TariffExclusion, ValidStatuses]":U.
        END.  // ELSE - IF ERROR-STATUS:ERROR THEN

        IF cError <> "" THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic  // ipcOwningEntityMnemonic
                                         ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                         ,INPUT "":U                                                 // ipcOwningEntityKey
                                         ,INPUT "related_value":U                                    // ipcFieldName
                                         ,INPUT btt_auth_detail.line_number                          // ipiLineNumber
                                         ,INPUT cError                                               // ipcMessageText
                                         ,INPUT "ERR":U).                                            // ipcMessageType
          RETURN.
        END.  // IF cError <> "" THEN
        ELSE
          ASSIGN btt_auth_detail.auth_status      = iAuthStatus
                 btt_auth_detail.auth_status_note = cStatusReason.

      END.  // IF NUM-ENTRIES(cRuleValue,"|":U) > 1 THEN
      ELSE
        ASSIGN btt_auth_detail.auth_status      = 6 // Declined
               btt_auth_detail.auth_status_note = "":U.

      mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                   (INPUT  goAuthorisation:InsurerObj       // ipdInsurerObj
                                   ,INPUT  0                                // ipiOptionCode
                                   ,INPUT  "ma_acAuthRuleTypeAuthDetail":U  // ipcRuleType
                                   ,INPUT  "TariffExclusionWarn":U          // ipcRuleCode
                                   ,INPUT  goAuthorisation:StartDate        // ipdEffectiveDate
                                   ,OUTPUT lValidRule                       // oplValidRule
                                   ,OUTPUT cRuleValue).                     // opcRuleValue

      IF lValidRule THEN
      DO:
        IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
        DO:
          ASSIGN cStatusDesc = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                              INPUT btt_auth_detail.auth_status)
                 cError      = "Tariff " + btt_auth_detail.related_value + " is an exclusion with value '"
                             + TRIM(ipcExclusion,".") + "'. Status is changed to '" + cStatusDesc
                             + IF btt_auth_detail.auth_status_note <> ""
                               THEN
                                 " - " + btt_auth_detail.auth_status_note + "'."
                               ELSE
                                 "'."
                 lAcknowledge = (IF cRuleValue = "WARNACK":U THEN TRUE ELSE FALSE).

          IF cStatusDesc = "":U
          THEN
            ASSIGN cError   = "Attempt to change authorisation detail line status to " + STRING(btt_auth_detail.auth_status)
                            + " was uncesuccessful. Status description is invalid."
                   cError   = cError + "[HELP=Auth Rule Code: TariffExclusion, TariffExclusionWarn]":U
                   lSuccess = goErrorObject:addError
                                           (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic // ipcOwningEntityMnemonic
                                           ,INPUT btt_auth_detail.auth_detail_obj                     // ipdOwningEntityObj
                                           ,INPUT "":U                                                // ipcOwningEntityKey
                                           ,INPUT "related_value":U                                   // ipcFieldName
                                           ,INPUT btt_auth_detail.line_number                         // ipiLineNumber
                                           ,INPUT cError                                              // ipcMessageText
                                           ,INPUT "ERR":U).                                           // ipcReplaceTextList
          ELSE
            ASSIGN cError   = cError + "[HELP=Auth Rule Code: TariffExclusion, TariffExclusionWarn]":U
                   lSuccess = goErrorObject:addError
                                           (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic // ipcOwningEntityMnemonic
                                           ,INPUT btt_auth_detail.auth_detail_obj                     // ipdOwningEntityObj
                                           ,INPUT "":U                                                // ipcOwningEntityKey
                                           ,INPUT "related_value":U                                   // ipcFieldName
                                           ,INPUT btt_auth_detail.line_number                         // ipiLineNumber
                                           ,INPUT cError                                              // ipcMessageText
                                           ,INPUT "WAR":U                                             // ipcMessageType
                                           ,INPUT lAcknowledge).                                      // ipcAcknowledge
        END.  // IF LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
        ELSE
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic    // ipcOwningEntityMnemonic
                                         ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                         ,INPUT "":U                                                 // ipcOwningEntityKey
                                         ,INPUT "auth_status":U                                      // ipcFieldName
                                         ,INPUT btt_auth_detail.line_number                          // ipiLineNumber
                                         ,INPUT "MA":U                                               // ipcMessageGroup
                                         ,INPUT 112  /* The '&1' specified is invalid. &2 */         // ipiMessageNumber
                                         ,INPUT "Auth Rule Value: ":U + cRuleValue +
                                                ",[HELP=Auth Rule Code: TariffExclusionWarn]").      // ipcReplaceTextList
      END. // IF lValidRule THEN

      IF iAuthStatus = 6
      THEN
        ASSIGN btt_auth_detail.claim_code = buf_auth_schext.claim-code[1].

    END.  // WHEN "LOAD":U

    OTHERWISE DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic    // ipcOwningEntityMnemonic
                                     ,INPUT btt_auth_detail.auth_detail_obj                      // ipdOwningEntityObj
                                     ,INPUT "":U                                                 // ipcOwningEntityKey
                                     ,INPUT "auth_status":U                                      // ipcFieldName
                                     ,INPUT btt_auth_detail.line_number                          // ipiLineNumber
                                     ,INPUT "MA":U                                               // ipcMessageGroup
                                     ,INPUT 112  /* The '&1' specified is invalid. &2 */         // ipiMessageNumber
                                     ,INPUT "Auth Rule Setup: ":U + ENTRY(1,cRuleValue,"|":U) +
                                            ",[HELP=Auth Rule Code: TariffExclusion]").          // ipcReplaceTextList
    END.  // OTHERWISE DO
  END CASE.

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_createAutoAuthDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _createAutoAuthDetail Procedure
PROCEDURE _createAutoAuthDetail PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Check for modifier tariffs that need to be auto-created on the auth.
  Parameters:  <none>
  Notes:       If a modifier tariff is no longer valid for the auth, but it was
               created on the auth detail line previously, we delete it from the auth.
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER btt_auth_detail   FOR tt_auth_detail.

DEFINE VARIABLE cAddValidations  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAlertMessage    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArsRate         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBaseRate        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCondType        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cError           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleValue       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTrackingMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWarning         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iPrType          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSubPrType       AS INTEGER     NO-UNDO.
DEFINE VARIABLE lValidRule       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dTrfCostObj      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTariffObj       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTariffLinkObj   AS DECIMAL     NO-UNDO.

DEFINE BUFFER hlm_code_link     FOR hlm_code_link.
DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
DEFINE BUFFER tt_auth_detail    FOR tt_auth_detail.

&IF {&DBDFMA} >= 010195 &THEN

DEFINE BUFFER buf_auth_detail   FOR hat_auth_detail.

mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,                            // ipdInsurerObj
                                               INPUT  0,                               // ipiOptionCode
                                               INPUT  "ma_acAuthRuleTypeAuthDetail":U, // ipcRuleType
                                               INPUT  "TariffModifierCDL":U,           // ipcRuleCode
                                               INPUT  btt_auth_detail.start_date,      // ipdEffectiveDate
                                               OUTPUT lValidRule,                      // oplValidRule
                                               OUTPUT cRuleValue).                     // opcRuleValue

ASSIGN cTrackingMessage = "AutoDet - Rule TariffModifierCDL applicable? " + STRING(lValidRule) + " - " + cRuleValue.

{ ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

IF lValidRule
THEN
  ASSIGN cCondType = cRuleValue.
ELSE
  RETURN.

FIND btt_auth_provider NO-LOCK
  WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
  NO-ERROR.

{ mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

IF AVAILABLE btt_auth_provider
THEN
  ASSIGN iPrType    = btt_auth_provider.pr_type
         iSubPrType = btt_auth_provider.sub_pr_type
         cBaseRate  = btt_auth_provider.default_base_rate
         cArsRate   = btt_auth_provider.default_ars_rate.

IF btt_auth_detail.override_base_rate <> ""
THEN
  ASSIGN cBaseRate = btt_auth_detail.override_base_rate
         cArsRate  = btt_auth_detail.override_ars_rate.
ELSE IF btt_auth_detail.default_base_rate <> ""
     THEN
       ASSIGN cBaseRate = btt_auth_detail.default_base_rate
              cArsRate  = btt_auth_detail.default_ars_rate.

IF cBaseRate = "" THEN
DO:
  mipEnv:Health:maDoctor:getProviderBaseRates( INPUT  btt_auth_provider.doc_num,
                                               INPUT  goAuthorisation:MemNum,
                                               INPUT  goAuthorisation:MemberOptionCode,
                                               INPUT  btt_auth_provider.start_date,
                                               OUTPUT cBaseRate,
                                               OUTPUT cArsRate).
END.  /* IF cBaseRate = "" THEN */

ASSIGN cTrackingMessage = "AutoDet - BaseRate=" + cBaseRate + " ArsRate=" + cArsRate.

{ ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

ASSIGN cTrackingMessage = "AutoDet - Parent=" + STRING(btt_auth_detail.owning_obj) + "(" + btt_auth_detail.owning_entity_mnemonic + ")".

{ ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

ASSIGN cAddValidations = "age,":U     + STRING(goAuthorisation:DependantAgeYears) +
                         "|gender,":U + goAuthorisation:DependantGender           +
                         "|BMI,":U    + STRING(goAuthorisation:DependantBMI).

BLKCODELINK:
FOR EACH hlm_code_link NO-LOCK
   WHERE hlm_code_link.parent_entity     = btt_auth_detail.owning_entity_mnemonic
   AND   hlm_code_link.parent_entity_obj = btt_auth_detail.owning_obj
   AND   hlm_code_link.effective_date   <= btt_auth_detail.start_date
   AND  (hlm_code_link.end_date          = ?
    OR   hlm_code_link.end_date         >= btt_auth_detail.start_date)
   AND   hlm_code_link.acronym_key  BEGINS "ma_acCodeLinkCatCreateTariff":U:

  ASSIGN dTariffLinkObj   = 0
         cTrackingMessage = "AutoDet - Check Valid Tariff: trflinkobj=":U + STRING(dTariffLinkObj) +
                            " TrfCode=":U + hlm_code_link.child_alt_value + " BaseRate=":U + cBaseRate + " ArsRate=":U + cArsRate +
                            " PrType=":U + STRING(iPrType) + " SubPrType=":U + STRING(iSubPrType) + " AddValidations=":U + cAddValidations.

  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  mipEnv:health:mamedical:getValidTariff( INPUT-OUTPUT dTariffLinkObj,                // iopdTariffLinkObj
                                          INPUT        hlm_code_link.child_alt_value, // ipcTariffCode
                                          INPUT        cBaseRate,                     // ipcBaseRate
                                          INPUT        cArsRate ,                     // ipcARSRate
                                          INPUT        iPrType,                       // ipiPrType
                                          INPUT        iSubPrType,                    // ipiSubPrType
                                          INPUT        btt_auth_detail.start_date,    // ipdDate
                                          INPUT        goAuthorisation:OptionCode,    // ipiOptionCode
                                          INPUT        cAddValidations,               // ipcAddValidations
                                          OUTPUT       dTariffObj,                    // opdTariffObj
                                          OUTPUT       dTrfCostObj,                   // opdTrfCostObj
                                          OUTPUT       cError,                        // opcError
                                          OUTPUT       cWarning,                      // opcWarning
                                          OUTPUT       cAlertMessage).

  ASSIGN cTrackingMessage = "AutoDet - Valid Tariff? trflinkobj=":U + STRING(dTariffLinkObj) +
                            " TariffObj=" + STRING(dTariffObj) + " TrfCostObj=" + STRING(dTrfCostObj) + " Error? " + cError.

  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  IF cError <> "" THEN
  DO:
    /*
      This child entity is no longer valid, now let's delete all the detail lines that were created for the child entity previously
    */
    FOR EACH tt_auth_detail
       WHERE tt_auth_detail.auth_provider_obj      = btt_auth_detail.auth_provider_obj
         AND tt_auth_detail.owning_entity_mnemonic = hlm_code_link.child_entity
         AND tt_auth_detail.owning_obj             = hlm_code_link.child_entity_obj :

      ASSIGN tt_auth_detail.record_action = "DELETE":U .

      IF CAN-FIND(FIRST buf_auth_detail NO-LOCK
                  WHERE buf_auth_detail.auth_detail_obj = tt_auth_detail.auth_detail_obj )
      THEN
        mipEnv:Health:AuthDataAccess:saveAuthDetail(BUFFER tt_auth_detail, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).

    END. /* FOR EACH tt_auth_detail */

    NEXT BLKCODELINK.
  END. /* IF cError <> ""  */

  IF cWarning <> ""
  THEN
    goErrorObject:addError
                      (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                       INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                       INPUT "":U,                                               /* ipcOwningEntityKey      */
                       INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                       INPUT cWarning,                                           /* ipcMessageText          */
                       INPUT "WAR":U).                                           /* ipcMessageType          */

  IF cAlertMessage <> ""
  THEN
    goErrorObject:addError
                      (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                       INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                       INPUT "":U,                                               /* ipcOwningEntityKey      */
                       INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                       INPUT cAlertMessage,                                      /* ipcMessageText          */
                       INPUT "WAR":U).                                           /* ipcMessageType          */

  ASSIGN cTrackingMessage = "AutoDet - Code Link Acronym=":U + hlm_code_link.acronym_key.

  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  CASE hlm_code_link.acronym_key:

    WHEN "ma_acCodeLinkCatCreateTariffAGE":U OR
    WHEN "ma_acCodeLinkCatCreateTariffBMI":U THEN
    DO:
      /* Create new hat_auth_detail record for the hlm_code_link.child_entity */
      RUN _createAutoAuthDetail2 (BUFFER btt_auth_detail, BUFFER hlm_code_link, dTrfCostObj).

    END. /* WHEN "AGE" or WHEN "BMI" THEN */

    WHEN "ma_acCodeLinkCatCreateTariffCDL":U THEN
    DO:
      EMPTY TEMP-TABLE tt_tariff_condition.

      mipEnv:Health:maMedical:getTariffConditions(INPUT hlm_code_link.child_alt_value,
                                                  INPUT btt_auth_detail.start_date,
                                                  OUTPUT TABLE tt_tariff_condition).
      FOR EACH tt_tariff_condition NO-LOCK
        WHERE tt_tariff_condition.cond-type = cCondType:

        IF NOT CAN-FIND(FIRST depcond NO-LOCK
                        WHERE depcond.mem-num     = goAuthorisation:MemNum
                        AND   depcond.dependant   = goAuthorisation:Dependant
                        AND   depcond.cond-code   = tt_tariff_condition.cond-code
                        AND   depcond.start-date <= btt_auth_detail.start_date
                        AND  (depcond.end-date    = ?
                         OR   depcond.end-date   >= btt_auth_detail.start_date))
        THEN
          NEXT.

        ASSIGN cTrackingMessage = "AutoDet - Dep Cond=":U + tt_tariff_condition.cond-code + " (":U + cCondType + ")":U.

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        /* Create new hat_auth_detail record for the hlm_code_link.child_entity */
        RUN _createAutoAuthDetail2 (BUFFER btt_auth_detail, BUFFER hlm_code_link, dTrfCostObj).

      END.  /* FOR EACH tt_tariff_condition NO-LOCK */
    END. /* WHEN "CDL" THEN */

  END CASE.

END.  /* BLKCODELINK: FOR EACH hlm_code_link NO-LOCK */

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_createAutoAuthDetail2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _createAutoAuthDetail2 Procedure
PROCEDURE _createAutoAuthDetail2 PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Create a tariff modifier on the auth detail line and update the
               amount_auth with the correct calculated value.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE       PARAMETER BUFFER btt_auth_detail   FOR tt_auth_detail.
DEFINE       PARAMETER BUFFER bhlm_code_link    FOR hlm_code_link.
DEFINE INPUT PARAMETER        ipdTrfCostObj     AS DECIMAL NO-UNDO.

DEFINE VARIABLE cError                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLineRestriction       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRelatedEntityMnemonic AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRelatedValue          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTrackingMessage       AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dCostCalculated        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dItemCost              AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dRelatedObj            AS DECIMAL     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

DEFINE BUFFER ntt_auth_detail FOR tt_auth_detail.
DEFINE BUFFER hat_auth_detail FOR hat_auth_detail.
DEFINE BUFFER trfcost         FOR trfcost.

  ASSIGN dCostCalculated = 0.

  FIND trfcost NO-LOCK
    WHERE trfcost.trfcost-obj = ipdTrfCostObj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE trfcost THEN
  DO:

    goErrorObject:addError(INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,
                           INPUT btt_auth_detail.auth_detail_obj,
                           INPUT "":U,
                           INPUT btt_auth_detail.line_number,
                           INPUT "MA":U,
                           INPUT 101,  /* The "&1" is not available */
                           INPUT "Tariff Cost (":U + STRING(ipdTrfCostObj) + ")":U ).

  END.  /* IF NOT AVAILABLE trfcost THEN */
  ELSE IF trfcost.cost-calculation <> "" THEN
       DO:
         /* Calculate the Tariff Cost amount */
         mipEnv:Health:AuthService:calcAmountAuth(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE, /* dsAuthorisation          */
                                                  INPUT  btt_auth_detail.auth_detail_obj,            /* ipdAuthDetailObj         */
                                                  INPUT  trfcost.cost-calculation,                   /* ipcCostCalculation       */
                                                  INPUT  trfcost.amount,                             /* ipdTrfcostAmount         */
                                                  OUTPUT dItemCost,                                  /* opdItemCost              */
                                                  OUTPUT cLineRestriction,                           /* opcLineRestriction       */
                                                  OUTPUT cRelatedEntityMnemonic,                     /* opcRelatedEntityMnemonic */
                                                  OUTPUT dRelatedObj,                                /* opdRelatedObj            */
                                                  OUTPUT cRelatedValue,                              /* opdRelatedValue          */
                                                  OUTPUT dCostCalculated,                            /* opdCostCalculated        */
                                                  OUTPUT cError).                                    /* opcCostError             */

         IF cError <> "" THEN
         DO:
           goErrorObject:addError
                        (INPUT 'hatad:':U + ntt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */
                         INPUT ntt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                         INPUT "":U,                                                /* ipcOwningEntityKey      */
                         INPUT ntt_auth_detail.line_number,                         /* ipiLineNumber           */
                         INPUT cError).                                             /* ipcMessageText          */
         END.  /* IF cError <> "" THEN */
       END.  /* IF trfcost.cost-calculation <> "" */

  FIND FIRST ntt_auth_detail
       WHERE ntt_auth_detail.auth_provider_obj      = btt_auth_detail.auth_provider_obj
       AND   ntt_auth_detail.owning_entity_mnemonic = bhlm_code_link.child_entity
       AND   ntt_auth_detail.owning_obj             = bhlm_code_link.child_entity_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = FALSE }

  /*
    Record not found so we are creating the modifier
  */
  IF NOT AVAILABLE ntt_auth_detail THEN
  DO:
    CREATE ntt_auth_detail.

    BUFFER-COPY btt_auth_detail EXCEPT btt_auth_detail.auth_detail_obj
                                       btt_auth_detail.owning_obj
                                       btt_auth_detail.owning_alt_value
                                       btt_auth_detail.loc_sequence
             TO ntt_auth_detail.

    ASSIGN
      ntt_auth_detail.auth_detail_obj         = btt_auth_detail.auth_obj * -1

      ntt_auth_detail.owning_entity_mnemonic  = bhlm_code_link.child_entity
      ntt_auth_detail.owning_obj              = bhlm_code_link.child_entity_obj
      ntt_auth_detail.owning_key              = bhlm_code_link.child_entity_key

      ntt_auth_detail.owning_alt_value        = bhlm_code_link.child_alt_value
      ntt_auth_detail.end_date                = ntt_auth_detail.start_date
      ntt_auth_detail.record_action           = "MODIFY":U

      ntt_auth_detail.default_base_rate       = trfcost.base-rate
      ntt_auth_detail.default_ars_rate        = trfcost.ars-rate

      ntt_auth_detail.end_ampm                = ntt_auth_detail.start_ampm
      ntt_auth_detail.amount_requested        = 0
      ntt_auth_detail.item_cost               = IF AVAILABLE trfcost
                                                THEN trfcost.amount
                                                ELSE 0
      ntt_auth_detail.quantity_requested      = 0
      ntt_auth_detail.quantity_auth           = 1
      ntt_auth_detail.fixed_item_cost         = IF AVAILABLE trfcost
                                                THEN trfcost.fixed-amount
                                                ELSE 0
      ntt_auth_detail.amount_auth             = IF dCostCalculated <> 0
                                                THEN dCostCalculated
                                                ELSE ntt_auth_detail.fixed_item_cost + (ntt_auth_detail.quantity_auth * ntt_auth_detail.item_cost)
      ntt_auth_detail.quantity_los            = 0
      ntt_auth_detail.loc_sequence            = 0
      ntt_auth_detail.loc_value               = ""
      ntt_auth_detail.los_calculation_rule    = ""
      ntt_auth_detail.add_to_total_los        = FALSE
      ntt_auth_detail.loc_tariff_type_obj     = 0
      ntt_auth_detail.related_entity_mnemonic = "hatad":U
      ntt_auth_detail.related_obj             = btt_auth_detail.auth_detail_obj
      ntt_auth_detail.related_key             = btt_auth_detail.owning_key
      ntt_auth_detail.related_value           = btt_auth_detail.owning_alt_value
      ntt_auth_detail.added_by_user           = NO.

    VALIDATE ntt_auth_detail.

    ASSIGN cTrackingMessage = "AutoDet - Creating Auth Detail Modifier -":U +
                              " OEM(":U       + ntt_auth_detail.owning_entity_mnemonic    + ")":U +
                              " Own-Obj(":U   + STRING(ntt_auth_detail.owning_obj)        + ")":U +
                              " Own-Val(":U   + STRING(ntt_auth_detail.owning_alt_value)  + ")":U +
                              " Dtl-Cost(":U  + STRING(ntt_auth_detail.item_cost)         + ")":U +
                              " Fixd-Cost(":U + STRING(ntt_auth_detail.fixed_item_cost)   + ")":U +
                              " Amount(":U    + STRING(ntt_auth_detail.amount_auth)       + ")":U.

    { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  END.  /* IF NOT AVAILABLE ntt_auth_detail THEN */
  ELSE DO:
    /*
      The modifier exists already, so we're only updating the modifier with the new calculated amount.
    */
    ASSIGN ntt_auth_detail.record_action   = "MODIFY":U
           ntt_auth_detail.item_cost       = IF AVAILABLE trfcost
                                               THEN trfcost.amount
                                               ELSE ntt_auth_detail.item_cost
           ntt_auth_detail.fixed_item_cost = IF AVAILABLE trfcost
                                               THEN trfcost.fixed-amount
                                               ELSE 0
           ntt_auth_detail.amount_auth     = IF dCostCalculated <> 0
                                               THEN dCostCalculated
                                               ELSE ntt_auth_detail.fixed_item_cost +
                                                   (ntt_auth_detail.quantity_auth * ntt_auth_detail.item_cost).
    VALIDATE ntt_auth_detail.

    ASSIGN cTrackingMessage = "AutoDet - Updating Auth Detail Modifier -":U +
                              " OBJ(":U       + STRING(ntt_auth_detail.auth_detail_obj)  + ")":U +
                              " OEM(":U       + ntt_auth_detail.owning_entity_mnemonic   + ")":U +
                              " Own-Obj(":U   + STRING(ntt_auth_detail.owning_obj)       + ")":U +
                              " Own-Val(":U   + STRING(ntt_auth_detail.owning_alt_value) + ")":U +
                              " Dtl-Cost(":U  + STRING(ntt_auth_detail.item_cost)        + ")":U +
                              " Qty(":U       + STRING(ntt_auth_detail.quantity_auth)    + ")":U +
                              " Fixd-Cost(":U + STRING(ntt_auth_detail.fixed_item_cost)  + ")":U +
                              " Amount(":U    + STRING(ntt_auth_detail.amount_auth)      + ")":U.

    { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }
  END.  /* ELSE - IF NOT AVAILABLE ntt_auth_detail THEN */

  mipEnv:Health:AuthDataAccess:saveAuthDetail(BUFFER ntt_auth_detail, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).

  /*
    When a modifier is created, we assign the modifier tariff's REM = LOC tariff. Similarly,
    we assign the LOC-tariff's related fields to point to the modifier tariff.
    After the modifier was committed to db, we assign the modifier tariff's details to
    the LOC-tariff's related values.
    btt_auth_detail was already committed to the db, that's why we update hat_auth_detail here.
  */

  FIND hat_auth_detail EXCLUSIVE-LOCK
    WHERE hat_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138,PROGRESS:445':U &ResetIgnoredErrors = FALSE }

  IF AVAILABLE hat_auth_detail THEN
    ASSIGN hat_auth_detail.related_entity_mnemonic = "hatad":U
           hat_auth_detail.related_obj             = ntt_auth_detail.auth_detail_obj
           hat_auth_detail.related_key             = ntt_auth_detail.owning_key
           hat_auth_detail.related_value           = ntt_auth_detail.owning_alt_value.

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_createDefaultAuthFlags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _createDefaultAuthFlags Procedure
PROCEDURE _createDefaultAuthFlags :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.
  DEFINE INPUT     PARAMETER TABLE FOR tt_auth_provider .
  DEFINE INPUT     PARAMETER iplResetToDefaults  AS LOGICAL NO-UNDO.

  DEFINE VARIABLE lActivateCopayment          AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lActivatePenalty            AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lLinkedToMainProvider       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lValidRule                  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lSuccess                    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cRuleValue                  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMainProviderRuleCodeList   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dLinkAuthRuleValue          AS DECIMAL   NO-UNDO.

  DEFINE VARIABLE oAuthRuleSearch    AS cls.maauthrulesearch      NO-UNDO.
  DEFINE VARIABLE oAuthFlagSearch    AS cls.maauthflagvaluesearch NO-UNDO.

  DEFINE BUFFER btt_auth_provider   FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_flag_value FOR tt_auth_flag_value.

&IF {&DBDFMA} >= 010195 &THEN

  IF AVAILABLE btt_auth THEN
  DO:

    EMPTY TEMP-TABLE ttAuthTypeConfig.

    mipEnv:Health:AuthService:getAuthTypeConfig(BUFFER btt_auth,
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig).

    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }


    IF AVAILABLE ttAuthTypeConfig THEN
    DO:

        /*
          Find the main provider
        */
        FIND FIRST btt_auth_provider
             WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
               AND btt_auth_provider.main_provider = TRUE
               AND btt_auth_provider.record_action <> "DELETE":U
        NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      /*
        Determine the flags which will be applicable for the Copayment/penalty processing .
        The following flags , depending on auth type set up , could be linked to the main provider:
              -PENALTY
              -LATEAUTH
              -EMERGENCY
              -COPAY
      */

      ASSIGN lActivateCopayment = ttAuthTypeConfig.ActivateCopayment
             lActivatePenalty   = ttAuthTypeConfig.ActivatePenalty .

      IF lActivateCopayment AND lActivatePenalty
      THEN
        ASSIGN cMainProviderRuleCodeList = "EMERGENCY,COPAY,LATEAUTH,PENALTY":U .

      ELSE IF lActivateCopayment AND NOT lActivatePenalty
      THEN
        ASSIGN cMainProviderRuleCodeList = "EMERGENCY,COPAY":U .

      ELSE IF NOT lActivateCopayment AND lActivatePenalty
      THEN
        ASSIGN cMainProviderRuleCodeList = "EMERGENCY,LATEAUTH,PENALTY":U .
      ELSE
        ASSIGN cMainProviderRuleCodeList = "":U .

      DATASET dsAuthRule:EMPTY-DATASET().

      oAuthRuleSearch = NEW cls.maauthrulesearch(DATASET dsAuthRule BY-REFERENCE).

      /*
        Fetch all defaults first
      */
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, btt_auth.insurer_obj).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, 0.00).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, btt_auth.option_code).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, 0).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.rule_type":U     ,  "=":U, "ma_acAuthRuleTypeAuthFlagDef":U).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.effective_date":U, "<=":U, btt_auth.start_date).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.rule_code":U     ,  "=":U, "ALL":U).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.rule_code":U     ,  "=":U, ttAuthTypeConfig.AuthTypeGroupLabel).

      /*
        And fetch.......
      */
      oAuthRuleSearch:fetchData().

      /*
        Remove all filter criteria from step 1 so we can re-search using the records
        we found above to apply filter criteria.
      */
      oAuthRuleSearch:removeFilterCriteria().

      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, btt_auth.insurer_obj).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U   ,  "=":U, 0.00).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, btt_auth.option_code).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.option_code":U   ,  "=":U, 0).
      oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.effective_date":U, "<=":U, btt_auth.start_date).

      FOR EACH tt_auth_rule NO-LOCK:

        oAuthRuleSearch:SetFilterCriteria("tt_auth_rule.link_auth_rule_obj":U, "=":U, tt_auth_rule.auth_rule_obj).

      END. /*FOR EACH tt_auth_rule NO-LOCK:*/

      /*
        Now that we are done with results from step 1 which were used to apply filter criteria,
        we can empty the dataset and refetch with the new filter criteria
      */
      DATASET dsAuthRule:EMPTY-DATASET().

      /*
        And fetch.......
      */

      oAuthRuleSearch:fetchData().

      DATASET dsAuthFlagValue:EMPTY-DATASET().

      oAuthFlagSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE).

      /*
        Fetch all flag value data for the authorisation
      */
      ASSIGN lSuccess = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
             lSuccess = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, btt_auth.auth_obj)
             lSuccess = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_key":U            , "=":U, "":U).

      /*
        Applying filter criteria this way so we do one bulk data retrieval of all
        flags for the relevant rules
      */
      FOR EACH tt_auth_rule NO-LOCK WHERE tt_auth_rule.rule_type = "ma_acAuthRuleTypeAuthFlag":

        ASSIGN lSuccess = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.auth_rule_obj":U , "=":U, tt_auth_rule.auth_rule_obj).

      END. /* FOR EACH tt_auth_rule */

      oAuthFlagSearch:fetchData().

      /*
        Create flag value records where flag values dont exist for the relevant rules
      */
      FOR EACH tt_auth_rule NO-LOCK WHERE tt_auth_rule.rule_type = "ma_acAuthRuleTypeAuthFlag":

        FIND FIRST btt_auth_flag_value EXCLUSIVE-LOCK
             WHERE btt_auth_flag_value.owning_entity_mnemonic = 'hatau'
               AND btt_auth_flag_value.owning_obj             = btt_auth.auth_obj
               AND btt_auth_flag_value.owning_key             = ''
               AND btt_auth_flag_value.auth_rule_obj          = tt_auth_rule.auth_rule_obj
        NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }


        IF NOT AVAILABLE btt_auth_flag_value THEN
        DO:

          CREATE btt_auth_flag_value.
          ASSIGN btt_auth_flag_value.auth_flag_value_obj    = tt_auth_rule.auth_rule_obj * -1
                 btt_auth_flag_value.record_action          = 'MODIFY':U
                 btt_auth_flag_value.owning_entity_mnemonic = 'hatau':U
                 btt_auth_flag_value.owning_obj             = btt_auth.auth_obj
                 btt_auth_flag_value.owning_key             = "":U
                 btt_auth_flag_value.owning_alt_value       = btt_auth.auth_num
                 btt_auth_flag_value.auth_rule_obj          = tt_auth_rule.auth_rule_obj
                 btt_auth_flag_value.last_change_datetime   = NOW
                 btt_auth_flag_value.auth_flag_value        = tt_auth_rule.rule_value.

          /*
            MMP-1007
            Assign the tt_auth_flag_value.auth_flag_value = "" when:
            Penalty processing is activated for the Authorisation Type (ttauthtypeconfig.ActivatePenalty) for Flags PENALTY, LATEAUTH and EMERGENCY.
            Co-payment processing is activated for the Authorisation Type (ttauthtypeconfig.Copayment) for Flag EMERGENCY.
          */
          IF  LOOKUP(tt_auth_rule.rule_code,cMainProviderRuleCodeList) > 0
          THEN
            ASSIGN btt_auth_flag_value.auth_flag_value = "".

          VALIDATE btt_auth_flag_value.


        END. /* IF NOT AVAILABLE tt_auth_flag_value */
        ELSE DO:

          ASSIGN lLinkedToMainProvider = LOOKUP(tt_auth_rule.rule_code,cMainProviderRuleCodeList) > 0 .


          /*
            The main provider is not available , this means we will need to reset the values on the flags linked to the main provider
          */
          IF  NOT AVAILABLE  btt_auth_provider
          AND lLinkedToMainProvider
          THEN
            ASSIGN btt_auth_flag_value.record_action           = "MODIFY":U
                   btt_auth_flag_value.auth_flag_value         = "":U
                   btt_auth_flag_value.override_note           = "":U.

          /*
            This procedure has been called with the intention of reseting all the flag values back to the default
              - This could be because a reguide is being done
              - Only reset values for flags not linked to the main provider
          */
          IF iplResetToDefaults
          AND NOT lLinkedToMainProvider
          THEN
            ASSIGN btt_auth_flag_value.record_action           = "MODIFY":U
                   btt_auth_flag_value.auth_flag_value         = tt_auth_rule.rule_value
                   btt_auth_flag_value.override_note           = "":U.

          VALIDATE btt_auth_flag_value .



        END. /* ELSE (IF NOT AVAIL tt_auth_flag_value..)  */
      END. /*FOR EACH tt_auth_rule NO-LOCK WHERE tt_auth_rule.rule_type = "ma_acAuthRuleTypeAuthFlag":*/

      mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).

      FOR EACH tt_auth_flag_value_error :

        ASSIGN tt_auth_flag_value_error.owning_entity_mnemonic = "hatau":U
               tt_auth_flag_value_error.owning_obj             = btt_auth.auth_obj
               tt_auth_flag_value_error.error_help             = "Please view auth flag setup under Healthcare Setup -> Authorsation Rules -> Auth Flag "  .

      END. //FOR EACH tt_auth_flag_value_error

      TEMP-TABLE  tt_auth_error:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_flag_value_error:HANDLE, TRUE ) .

    END. //IF AVAILABLE ttAuthTypeConfig
  END. /*IF AVAILABLE btt_auth THEN*/

  /*
    Cleanup
  */
  { mip/inc/mipcatcherror.i &FINALLY = "DATASET dsAuthRule:EMPTY-DATASET().

                                        IF VALID-OBJECT(oAuthRuleSearch) THEN DELETE OBJECT oAuthRuleSearch NO-ERROR.
                                        IF VALID-OBJECT(oAuthFlagSearch) THEN DELETE OBJECT oAuthFlagSearch NO-ERROR."}

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_determineAuthAutoDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _determineAuthAutoDetails Procedure
PROCEDURE _determineAuthAutoDetails :
/*------------------------------------------------------------------------------
  Purpose:  Create/delete/modify all auto created details here
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER btt_auth_detail FOR tt_auth_detail.

  AUTH-DETAIL-BLK:
  FOR EACH btt_auth_detail
     WHERE btt_auth_detail.record_action <> "DELETE":U
     /* The record action was cleared in maauthbussaveautorisation.i, so we
         use the can-find to check if the btt_auth_detail's db-record was deleted. */
     AND   CAN-FIND(hat_auth_detail WHERE hat_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj) :

    /*
      Check if any tariff modifiers must be created automatically
    */
    IF  btt_auth_detail.owning_entity_mnemonic = "htmtl":U
    AND btt_auth_detail.auth_status <> 6 /* Declined */
    AND btt_auth_detail.auth_status <> 5 /* Cancelled */ THEN
    DO:
      IF CAN-FIND(FIRST hlm_code_link
                  WHERE hlm_code_link.acronym_key  BEGINS "ma_acCodeLinkCatCreateTariff":U
                  AND   hlm_code_link.parent_entity     = btt_auth_detail.owning_entity_mnemonic
                  AND   hlm_code_link.parent_entity_obj = btt_auth_detail.owning_obj
                  AND   hlm_code_link.effective_date   <= btt_auth_detail.start_date
                  AND  (hlm_code_link.end_date          = ?
                   OR   hlm_code_link.end_date         >= btt_auth_detail.end_date)) THEN
      RUN _createAutoAuthDetail (BUFFER btt_auth_detail).

    END.  /* IF btt_auth_detail.owning_entity_mnemonic = "hlmtl":U AND... */

  END. /* AUTH-DETAIL-BLK */

  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_displayFlags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _displayFlags Procedure
PROCEDURE _displayFlags PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Activate member or provider display flags.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipdInsurerObj           AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER ipiOptionCode           AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipcFlagType             AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcFlagKey              AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipdDate                 AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER ipcOwningEntityMnemonic AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipdOwningEntityObj      AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER ipiLineNumber           AS INTEGER     NO-UNDO.

DEFINE VARIABLE cMessage                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleCode                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleType                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleValue                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lValidRule                      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lAcknowledge                    AS LOGICAL     NO-UNDO.

  CASE ipcFlagType:
    WHEN "M":U THEN ASSIGN cRuleType = "ma_acAuthRuleTypeAuthReg":U
                           cRuleCode = "MemberDisplayFlags":U.
    WHEN "D":U THEN ASSIGN cRuleType = "ma_acAuthRuleTypeAuthProvider":U
                           cRuleCode = "ProviderDisplayFlags":U.
    OTHERWISE RETURN.
  END CASE.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  cRuleType,
                                                 INPUT  cRuleCode,
                                                 INPUT  ipdDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  IF lValidRule
  AND LOOKUP(cRuleValue,"Warn,WarnAck":U) <> 0 THEN
  DO:
    ASSIGN cMessage = "":U.
    mipEnv:Health:maUtility:getDisplayFlags(INPUT ipiOptionCode,
                                            INPUT ipcFlagType,
                                            INPUT ipcFlagKey,
                                            INPUT ipdDate,
                                            INPUT TRUE,
                                            OUTPUT cMessage,
                                            OUTPUT TABLE tt_flagdet).
    IF cMessage <> "":U THEN
    DO:
      ASSIGN cMessage = cMessage + "[HELP=Auth Rule Code: " + cRuleCode + "]".
      goErrorObject:addError(INPUT ipcOwningEntityMnemonic, // ipcOwningEntityMnemonic
                             INPUT ipdOwningEntityObj,      // ipdOwningEntityObj
                             INPUT "":U,                    // ipcOwningEntityKey
                             INPUT ipiLineNumber,           // ipiLineNumber
                             INPUT cMessage,                // ipcMessageText
                             INPUT "ERR":U).                // ipiMessageType
    END.  // IF cMessage <> "":U THEN
    ELSE FOR EACH tt_flagdet:
      ASSIGN cMessage = tt_flagdet.flag_description + ": ":U +
                        tt_flagdet.flag_value +
                        " From: " + STRING(tt_flagdet.effective_date,"9999/99/99":U) + " - ":U +
                        IF tt_flagdet.end_date = ?
                        THEN "Current"
                        ELSE STRING(tt_flagdet.end_date,"9999/99/99":U)
             cMessage = cMessage + "[HELP=Auth Rule Code: " + cRuleCode + "]"
             lAcknowledge = IF cRuleValue = "WarnAck":U
                            THEN TRUE
                            ELSE FALSE.
      goErrorObject:addError(INPUT ipcOwningEntityMnemonic, // ipcOwningEntityMnemonic
                             INPUT ipdOwningEntityObj,      // ipdOwningEntityObj
                             INPUT "":U,                    // ipcOwningEntityKey
                             INPUT ipiLineNumber,           // ipiLineNumber
                             INPUT cMessage,                // ipcMessageText
                             INPUT "WAR":U,                 // ipiMessageType
                             INPUT lAcknowledge).           // iplAcknowledge
    END.  // FOR EACH tt_flagdet:
  END.  // IF lValidRule

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_getConfiguration) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _getConfiguration Procedure
PROCEDURE _getConfiguration PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Returns common authorisation provider configuration object
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth          FOR tt_auth .
  DEFINE PARAMETER BUFFER btt_auth_provider FOR tt_auth_provider .
  DEFINE OUTPUT PARAMETER oplcConfiguration AS LONGCHAR   NO-UNDO.

  DEFINE VARIABLE dAuthTypeObj         AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dInsurerObj          AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE iOptionCode          AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dStartDate           AS DATE                 NO-UNDO.
  DEFINE VARIABLE oJsonArray           AS JsonArray            NO-UNDO.
  DEFINE VARIABLE oJsonObject          AS JsonObject           NO-UNDO.
  DEFINE VARIABLE oJsonParser          AS ObjectModelParser    NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValid               AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lNotePerAuthStatus   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cRuleValue           AS CHARACTER            NO-UNDO.

  EMPTY TEMP-TABLE ttAuthTypeConfig.

  ASSIGN dAuthTypeObj  = btt_auth.auth_type_obj
         dInsurerObj   = btt_auth.insurer_obj
         iOptionCode   = btt_auth.option_code
         dStartDate    = btt_auth.start_date .

  IF dAuthTypeObj <> 0.00 THEN
  DO:

    mipEnv:Health:AuthService:getAuthTypeConfig(BUFFER btt_auth ,
                                                BUFFER btt_auth_provider,
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).

    /* UTF encoding for the parser */
    FIX-CODEPAGE(oplcConfiguration) = "UTF-8":U.

    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE ttAuthTypeConfig THEN
    DO:
      TEMP-TABLE ttAuthTypeConfig:WRITE-JSON("LONGCHAR":U, oplcConfiguration, FALSE, "UTF-8":U, FALSE, TRUE).


      ASSIGN lSuccess           = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                    (INPUT  dInsurerObj,
                                     INPUT  iOptionCode,
                                     INPUT  "ma_acAuthRuleTypeAUTHSETUPS":U,
                                     INPUT  "NotePerAuthStatus":U,
                                     INPUT  dStartDate,
                                     OUTPUT lValid,
                                     OUTPUT cRuleValue)

             lNotePerAuthStatus = lValid AND CAN-DO("Y,YES,T,TRUE":U, TRIM(cRuleValue))

             oJsonParser        = NEW ObjectModelParser().


      /*
        Create a combined configuration json object
      */
      oJsonArray  = CAST(oJsonParser:Parse(oplcConfiguration), JsonArray).

      oJsonObject = oJsonArray:GetJsonObject(1).

      oJsonObject:ADD("NotePerAuthStatus":U, lNotePerAuthStatus).

      oJsonObject:WRITE(oplcConfiguration).
    END. /* IF AVAILABLE ttAuthTypeConfig THEN */
  END. /*IF dAuthTypeobj <> 0.00 THEN*/

  { mip/inc/mipcatcherror.i
    &FINALLY = "EMPTY TEMP-TABLE ttAuthTypeConfig."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_populateAuthFlags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _populateAuthFlags Procedure
PROCEDURE _populateAuthFlags :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE oAuthFlagSearch   AS cls.maauthflagvaluesearch NO-UNDO.
  DEFINE VARIABLE cTrackingMessage  AS CHARACTER                 NO-UNDO.

  DEFINE BUFFER btt_auth     FOR tt_auth.

  FOR EACH btt_auth NO-LOCK:

    /*
      Populate the tt_auth_flag_value table with the flags for the auth
    */

    oAuthFlagSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE).
    oAuthFlagSearch:SetCriteria("BufferList":U, "tt_auth_flag_value":U).
    oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U).
    oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, btt_auth.auth_obj).
    oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_key":U            , "=":U, "":U).
    oAuthFlagSearch:fetchData().

  END.  /* FOR EACH btt_auth NO-LOCK: */

  ASSIGN cTrackingMessage = "Auth Flags populated? " + STRING(CAN-FIND(FIRST tt_auth_flag_value)).
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  { mip/inc/mipcatcherror.i
            &FINALLY = "IF VALID-OBJECT(oAuthFlagSearch) THEN DELETE OBJECT oAuthFlagSearch."}

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_populateAuthObj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _populateAuthObj Procedure
PROCEDURE _populateAuthObj :
/*------------------------------------------------------------------------------
  Purpose   : Populates auth obj from new record on all child records before saving
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdFromObj AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipdToObj   AS DECIMAL NO-UNDO.


  /*
    Populate provider records auth obj
  */
  FOR EACH tt_auth_provider EXCLUSIVE-LOCK
     WHERE tt_auth_provider.auth_obj = ipdFromObj:

    ASSIGN tt_auth_provider.auth_obj = ipdToObj.
    VALIDATE tt_auth_provider.
  END. /*FOR EACH tt_auth_provider EXCLUSIVE-LOCK */

  /*
    Populate coding records auth obj
  */
  FOR EACH tt_auth_coding EXCLUSIVE-LOCK
     WHERE tt_auth_coding.auth_obj = ipdFromObj:

    ASSIGN tt_auth_coding.auth_obj = ipdToObj.
    VALIDATE tt_auth_coding.
  END. /*FOR EACH tt_coding_provider EXCLUSIVE-LOCK */

  /*
    Populate detail records auth obj
  */
  FOR EACH tt_auth_detail EXCLUSIVE-LOCK
     WHERE tt_auth_detail.auth_obj = ipdFromObj:

    ASSIGN tt_auth_detail.auth_obj = ipdToObj.
    VALIDATE tt_auth_detail.
  END. /*FOR EACH tt_auth_detail EXCLUSIVE-LOCK */

  /*
    Populate savings records auth obj
  */
  FOR EACH tt_auth_mc_savings EXCLUSIVE-LOCK
     WHERE tt_auth_mc_savings.auth_obj = ipdFromObj:

    ASSIGN tt_auth_mc_savings.auth_obj = ipdToObj.
    VALIDATE tt_auth_mc_savings.
  END. /*FOR EACH tt_auth_mc_savings EXCLUSIVE-LOCK */
   /*
    Populate savings records auth obj
  */
  FOR EACH tt_auth_mc_savings EXCLUSIVE-LOCK
     WHERE tt_auth_mc_savings.auth_obj = ipdFromObj:

    ASSIGN tt_auth_mc_savings.auth_obj = ipdToObj.
    VALIDATE tt_auth_mc_savings.
  END. /*FOR EACH tt_auth_mc_savings EXCLUSIVE-LOCK */
 /*
    Populate crosswalk records auth obj
  */
  FOR EACH tt_auth_crosswalk EXCLUSIVE-LOCK
     WHERE tt_auth_crosswalk.auth_obj = ipdFromObj:

    ASSIGN tt_auth_crosswalk.auth_obj = ipdToObj.
    VALIDATE tt_auth_crosswalk.
  END. /*FOR EACH tt_auth_crosswalk EXCLUSIVE-LOCK */

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_populateAuthStatusReasonDescriptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _populateAuthStatusReasonDescriptions Procedure
PROCEDURE _populateAuthStatusReasonDescriptions :
/*------------------------------------------------------------------------------
  Purpose:  Populate the  auth,provider,coding and detail status reason descriptions
            and reason types for use in the UI
  Parameters:  <none>
  Notes: Even though a direct db read to the note table would have been much quicker
         than a service call to getStatusReasonDesc() for each iteration, the
         decision has been made to rather make use of the service since it is
         central. So if there ever needs to be a change in the way status descriptions
         are retrieved, it only needs to be done in one place(the service).

         When there are any records with for which a status description was not found
         it will be written out to the log under the message group ma_DebugAuth
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER btt_auth FOR tt_auth.
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_coding FOR tt_auth_coding.
  DEFINE BUFFER btt_auth_detail FOR tt_auth_detail.

  DEFINE VARIABLE cErrorMessage      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStatusNote        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStatusReasonType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStatusDescription AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dStartDate         AS DATE      NO-UNDO.
  DEFINE VARIABLE dAuthObj           AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dInsurerObj        AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iIteration         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iOption            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iStatus            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.

  /*
    We'll use this throughout to determine status reason types
  */
  DEFINE VARIABLE lNotePerAuthStatusValidRule AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cNotePerAuthStatusRuleValue AS CHARACTER   NO-UNDO.

  FOR EACH btt_auth :

    ASSIGN dAuthObj     = DECIMAL(btt_auth.auth_obj)
           dInsurerObj  = DECIMAL(btt_auth.insurer_obj)
           iOption      = INTEGER(btt_auth.option_code)
           iStatus      = btt_auth.auth_status
           cStatusNote  = btt_auth.auth_status_note
           dStartDate   = btt_auth.start_date.

    ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                               (INPUT  dInsurerObj,
                                INPUT  iOption,
                                INPUT  "ma_acAuthRuleTypeAUTHSETUPS":U,
                                INPUT  "NOTEPERAUTHSTATUS":U,
                                INPUT  dStartDate,
                                OUTPUT lNotePerAuthStatusValidRule,
                                OUTPUT cNotePerAuthStatusRuleValue).
    /*
      The auth header status
    */
    IF cStatusNote <> "":U THEN
    DO:
      mipEnv:health:AuthService:getStatusReasonDesc( INPUT        dInsurerObj,
                                                     INPUT        iOption,
                                                     INPUT        dStartDate,
                                                     INPUT        iStatus,
                                                     INPUT        cStatusNote,
                                                     INPUT-OUTPUT cStatusReasonType,
                                                     INPUT-OUTPUT lNotePerAuthStatusValidRule,
                                                     OUTPUT       cStatusDescription,
                                                     OUTPUT       cErrorMessage).
      IF cErrorMessage = "":U
      THEN
        ASSIGN btt_auth._note_narration  = cStatusDescription
               btt_auth._reason_type     = cStatusReasonType.
      ELSE
      DO:
        ASSIGN cMessage = SUBSTITUTE('Warning: Status Reason Description for Auth &1. &2':U ,btt_auth.auth_num ,cErrorMessage).

        { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cMessage }

      END.  /* ELSE - IF cErrorMessage = "":U */
    END. /* IF cStatusNote <> "":U THEN */
    ELSE
      ASSIGN btt_auth._note_narration  = "Please Enter a valid Status Reason.":U
             btt_auth._reason_type     = IF NOT lNotePerAuthStatusValidRule
                                         THEN "AS":U
                                         ELSE "AS":U + STRING(iStatus).

    /*
      Provider
    */
    FOR EACH btt_auth_provider
       WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj:

      ASSIGN iStatus            = btt_auth_provider.auth_status
             cStatusNote        = btt_auth_provider.auth_status_note
             dStartDate         = btt_auth_provider.start_date
             cStatusDescription = "":U
             cStatusReasonType  = "":U .

      IF cStatusNote <> "":U THEN
      DO:
        mipEnv:health:AuthService:getStatusReasonDesc( INPUT        dInsurerObj,
                                                       INPUT        iOption,
                                                       INPUT        dStartDate,
                                                       INPUT        iStatus,
                                                       INPUT        cStatusNote,
                                                       INPUT-OUTPUT cStatusReasonType,
                                                       INPUT-OUTPUT lNotePerAuthStatusValidRule,
                                                       OUTPUT       cStatusDescription,
                                                       OUTPUT       cErrorMessage).
        IF cErrorMessage = "":U
        THEN
          ASSIGN btt_auth_provider._note_narration  = cStatusDescription
                 btt_auth_provider._reason_type     = cStatusReasonType.
        ELSE
        DO:
          ASSIGN cMessage = SUBSTITUTE('Warning: Status Description for Provider &1. &2':U ,btt_auth_provider.doc_num ,cErrorMessage).

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cMessage }

        END.  /* ELSE - IF cErrorMessage = "":U */
      END. /*IF cStatusNote <> "":U THEN*/
      ELSE
        ASSIGN btt_auth_provider._note_narration  = "Please Enter a valid Status Note.":U
               btt_auth_provider._reason_type     = IF NOT lNotePerAuthStatusValidRule
                                                    THEN "AS":U
                                                    ELSE "AS":U + STRING(iStatus).

    END. /* FOR EACH btt_auth_provider WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj */

    /*
      Coding
    */
    FOR EACH btt_auth_coding
       WHERE btt_auth_coding.auth_obj = btt_auth.auth_obj :

      ASSIGN iStatus            = btt_auth_coding.coding_status
             cStatusNote        = btt_auth_coding.coding_status_note
             dStartDate         = btt_auth_coding.start_date
             cStatusDescription = "":U
             cStatusReasonType  = "":U .

      IF cStatusNote <> "":U THEN
      DO:
        mipEnv:health:AuthService:getStatusReasonDesc( INPUT        dInsurerObj,
                                                       INPUT        iOption,
                                                       INPUT        dStartDate,
                                                       INPUT        iStatus,
                                                       INPUT        cStatusNote,
                                                       INPUT-OUTPUT cStatusReasonType,
                                                       INPUT-OUTPUT lNotePerAuthStatusValidRule,
                                                       OUTPUT       cStatusDescription,
                                                       OUTPUT       cErrorMessage).
        IF cErrorMessage = "":U
        THEN
          ASSIGN btt_auth_coding._note_narration  = cStatusDescription
                 btt_auth_coding._reason_type     = cStatusReasonType.
        ELSE
        DO:
          ASSIGN cMessage = SUBSTITUTE('Warning: Status Description for coding &1. &2':U ,btt_auth_coding.owning_alt_value,cErrorMessage).

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cMessage }

        END.  /* ELSE - IF cErrorMessage = "":U */
      END. /* IF cStatusNote <> "":U THEN */
      ELSE
        ASSIGN btt_auth_coding._note_narration  = "Please Enter a valid Status Note.":U
               btt_auth_coding._reason_type     = IF NOT lNotePerAuthStatusValidRule
                                                  THEN "AS":U
                                                  ELSE "AS":U + STRING(iStatus).

    END. /* FOR EACH btt_auth_coding WHERE btt_auth_coding.auth_obj = btt_auth.auth_obj  */

    /*
      Detail
    */
    FOR EACH btt_auth_detail
       WHERE btt_auth_detail.auth_obj = btt_auth.auth_obj :

      ASSIGN iStatus            = btt_auth_detail.auth_status
             cStatusNote        = btt_auth_detail.auth_status_note
             dStartDate         = btt_auth_detail.start_date
             cStatusDescription = "":U
             cStatusReasonType  = "":U .

      IF cStatusNote <> "":U THEN
      DO:
        mipEnv:health:AuthService:getStatusReasonDesc( INPUT        dInsurerObj,
                                                       INPUT        iOption,
                                                       INPUT        dStartDate,
                                                       INPUT        iStatus,
                                                       INPUT        cStatusNote,
                                                       INPUT-OUTPUT cStatusReasonType,
                                                       INPUT-OUTPUT lNotePerAuthStatusValidRule,
                                                       OUTPUT       cStatusDescription,
                                                       OUTPUT       cErrorMessage).
        IF cErrorMessage = "":U
        THEN
          ASSIGN btt_auth_detail._auth_status_narration = cStatusDescription
                 btt_auth_detail._reason_type           = cStatusReasonType.
        ELSE
        DO:
          ASSIGN cMessage = SUBSTITUTE('Warning: Status Description for detail &1. &2':U ,btt_auth_detail.owning_alt_value,cErrorMessage).

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cMessage }

        END.  /* ELSE - IF cErrorMessage = "":U */
      END. /* IF cStatusNote <> "":U THEN */
      ELSE
        ASSIGN btt_auth_detail._auth_status_narration  = "Please Enter a valid Status Note."
               btt_auth_detail._reason_type            = IF NOT lNotePerAuthStatusValidRule
                                                         THEN "AS":U
                                                         ELSE "AS":U + STRING(iStatus).

    END. /* FOR EACH btt_auth_detail WHERE btt_auth_detail.auth_obj = btt_auth.auth_obj  */
  END. /* FOR EACH btt_auth */

&ENDIF

  {mip/inc/mipcatcherror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_populateDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _populateDataset Procedure
PROCEDURE _populateDataset :
/*------------------------------------------------------------------------------
  Purpose   : Procedure to populate the Auth data into the Auth dataset, merged
              with the recent updates
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iBuffer     AS INTEGER          NO-UNDO.
  DEFINE VARIABLE hBuffer     AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hDataset    AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hSource     AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hTarget     AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hQuery      AS HANDLE           NO-UNDO.
  DEFINE VARIABLE cBufferList AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE oSearch     AS cls.maauthsearch NO-UNDO.
  DEFINE VARIABLE lSuccess    AS LOGICAL          NO-UNDO.

  ASSIGN cBufferList = "tt_auth,tt_auth_provider,tt_auth_detail,tt_auth_coding,tt_auth_mc_savings,tt_auth_crosswalk,tt_auth_copay,tt_auth_limit":U.

  /*
    Store original data which came is with the saveAuthorisation request
  */
  CREATE DATASET hDataset .
  hDataset:CREATE-LIKE(DATASET dsAuthorisation:HANDLE).
  hDataset:EMPTY-DATASET().

  hDataset:COPY-DATASET(DATASET dsAuthorisation:HANDLE).

  /*
    Now that we have stored the data that came in, we can now empty the dataset before retrieving fresh from the DB
  */
  DATASET dsAuthorisation:EMPTY-DATASET().

  oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).

  ASSIGN lSuccess = oSearch:setCriteria("BufferList":U, cBufferList)
         lSuccess = oSearch:setCriteria("DataSecured":U, "FALSE":U).
  /*
    Set filters to retrieve all the relevant auth date we required
  */
  BufferBlk:
  DO iBuffer = 1 TO NUM-ENTRIES(cBufferList):

    ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE(ENTRY(iBuffer, cBufferList)).

    CREATE QUERY hQuery.

    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK":U, hBuffer:NAME)).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().

    DO WHILE NOT hQuery:QUERY-OFF-END:

      IF NOT CAN-FIND(FIRST ttFilter NO-LOCK WHERE ttFilter.AuthObj = hBuffer::auth_obj) THEN
      DO:
        CREATE ttFilter.
        ASSIGN ttFilter.AuthObj = hBuffer::auth_obj.

        ASSIGN lSuccess = oSearch:setFilterCriteria("tt_auth.auth_obj":U, "=":U, hBuffer::auth_obj).
      END. /*IF NOT CAN-FIND(FIRST ttFilter NO-LOCK WHERE ttFilter.AuthObj = hBuffer::auth_obj) THEN*/

      hQuery:GET-NEXT().

    END. /*DO WHILE NOT hQuery:QUERY-OFF-END:*/

    IF VALID-HANDLE(hQuery) THEN
    DO:
      IF hQuery:IS-OPEN
      THEN hQuery:QUERY-CLOSE().

      DELETE OBJECT hQuery.
    END. /*IF VALID-HANDLE(hQuery) THEN*/

  END. /*BufferBlk*/

  IF CAN-FIND(FIRST ttFilter) THEN
    oSearch:fetchData().

  /*
    Copy the error records that we had in our dataset - we need to do this seperately since the COPY-DATASET call below requires a table to have a
    unique index , which the tt_auth_error table does not have.
  */
  DATASET dsAuthorisation:HANDLE:GET-BUFFER-HANDLE("tt_auth_error"):COPY-TEMP-TABLE( hDataset:GET-BUFFER-HANDLE("tt_auth_error")) .

  /*
    Overlay the data we populated from the database with the data passed in with the service call

    With the replace mode of copy-dataset, all tables copied need a primary index. As we do not
    have primary indexes on the error and result temp-tables in our dataset, we ran into an error.
    To resolve this, we specify a comma-delimited list of the target and source temp-table pairs
    to be copied.
  */
  DATASET dsAuthorisation:COPY-DATASET(hDataset, ?, TRUE, FALSE, "tt_auth,tt_auth,tt_auth_provider,tt_auth_provider,tt_auth_detail,tt_auth_detail,tt_auth_coding,tt_auth_coding,tt_auth_mc_savings,tt_auth_mc_savings,tt_auth_crosswalk,tt_auth_crosswalk,tt_auth_copay,tt_auth_copay,tt_auth_limit,tt_auth_limit":U) NO-ERROR.

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-HANDLE(hDataset) THEN DELETE OBJECT hDataset.
                IF VALID-OBJECT(oSearch)  THEN DELETE OBJECT oSearch.

                EMPTY TEMP-TABLE ttFilter.

                IF VALID-HANDLE(hQuery) THEN
                DO:
                  IF hQuery:IS-OPEN
                  THEN hQuery:QUERY-CLOSE().

                  DELETE OBJECT hQuery.
                END."}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_runLOSCalculations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _runLOSCalculations Procedure
PROCEDURE _runLOSCalculations :
/*------------------------------------------------------------------------------
  Purpose   : Recalculate LOS
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER btt_auth          FOR tt_auth.
  DEFINE BUFFER btt_auth_detail   FOR tt_auth_detail.
  DEFINE BUFFER buf_auth_detail   FOR hat_auth_detail.
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.

  DEFINE VARIABLE cArsRate              AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cErrorMessage         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cTrackingMessage      AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj        AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE iCalcTime             AS INTEGER              NO-UNDO.
  DEFINE VARIABLE lAcknowledge          AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSysLOSCalc           AS LOGICAL INITIAL TRUE NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL              NO-UNDO.

  FOR EACH btt_auth NO-LOCK
     WHERE btt_auth.auth_obj > 0.00:

    FIND FIRST btt_auth_detail NO-LOCK
         WHERE btt_auth_detail.auth_obj      = btt_auth.auth_obj
           AND btt_auth_detail.record_action = "MODIFY":U
           AND btt_auth_detail.quantity_los <> 0
           AND (btt_auth_detail._start_date_ampm_updated
            OR  btt_auth_detail._end_date_ampm_updated)
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE btt_auth_detail THEN
    DO:
      ASSIGN lSysLOSCalc    = FALSE
             dAuthDetailObj = btt_auth_detail.auth_detail_obj.

      FIND FIRST btt_auth_provider NO-LOCK
        WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE btt_auth_provider THEN
      DO:
        ASSIGN cArsRate = IF btt_auth_provider.override_ars_rate <> "":U
                          THEN btt_auth_provider.override_ars_rate
                          ELSE btt_auth_provider.default_ars_rate.

        /*
          Start date/time changed - Check if Private Ward
        */
        IF btt_auth_detail._start_date_ampm_updated THEN
        DO:
          ASSIGN cTrackingMessage = "Start date/time updated - Provider: " + STRING(btt_auth_provider.doc_num,"9999999":U) +
                                    " ARS Rate " + cArsRate.
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                         INPUT  btt_auth.option_code,
                                                         INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                         INPUT  "CalcPVTPerDiem":U,
                                                         INPUT  btt_auth.start_date,
                                                         OUTPUT lValidRule,
                                                         OUTPUT cRuleValue).

          /*
            - Check rule to see if it is a Private Ward
            - If so, start date/time updates allowed - Normal LOS calculation to apply
          */
          IF lValidRule
          AND LOOKUP(cArsRate + ",":U + btt_auth_detail.loc_value,TRIM(cRuleValue),"|") <> 0 THEN
          DO:
            ASSIGN lSysLOSCalc      = TRUE
                   cTrackingMessage = "Start date/time updated on Private Ward - Updates allowed - System LOS calculation applied".

            { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }
          END. /* IF lValidRule AND */
        END. /* IF tt_auth_detail._start_date_ampm_updated */

        IF NOT lSysLOSCalc THEN
        DO:
          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                         INPUT  btt_auth.option_code,
                                                         INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                         INPUT  "UndoLOCDateUpdates":U,
                                                         INPUT  btt_auth.start_date,
                                                         OUTPUT lValidRule,
                                                         OUTPUT cRuleValue).

          /* - Valid Rule
             - Provider ARS Setup in rule
             - Not allowed to update start/end date/time
             - Generate Warning
          */
          IF lValidRule
          AND LOOKUP(cArsRate,TRIM(ENTRY(1,cRuleValue,"|"))) <> 0 THEN
          DO:
            /*
              - Provider ARS setup in rule UndoLOCDateUpdates
              - Update of start/end date/time not allowed
              - Generate warning and undo changes
            */
            FIND buf_auth_detail NO-LOCK
              WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

            /*
              Undo changes - revert back to original db values
            */
            IF AVAILABLE buf_auth_detail
            THEN
              ASSIGN btt_auth_detail.start_date               = buf_auth_detail.start_date
                     btt_auth_detail.start_ampm               = buf_auth_detail.start_ampm
                     btt_auth_detail.end_date                 = buf_auth_detail.end_date
                     btt_auth_detail.end_ampm                 = buf_auth_detail.end_ampm
                     btt_auth_detail._start_date_ampm_updated = FALSE
                     btt_auth_detail._end_date_ampm_updated   = FALSE.

            /*
              Check rule setup to see if a warning should be generated
              If Warn or WarnAck not setup - no warning will be generated
            */
            IF NUM-ENTRIES(cRuleValue,"|") > 1
            AND CAN-DO("Warn,WarnAck",ENTRY(2,cRuleValue,"|"))
            THEN
              ASSIGN lAcknowledge  = IF ENTRY(2,cRuleValue,"|") = "WarnAck":U
                                     THEN TRUE
                                     ELSE FALSE
                     cErrorMessage = "Date not updateable due to Alternative Reimbursement rates."
                                   + "[HELP=Auth Rule Code: CalcPVTPerDiem, UndoLOCDateUpdates]"
                     lSuccess      = goErrorObject:addError
                                             (INPUT "hatad:" + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                              INPUT btt_auth_detail.auth_detail_obj,                   /* ipdOwningEntityObj       */
                                              INPUT "":U,                                              /* ipcOwningEntityKey       */
                                              INPUT btt_auth_detail.line_number,                       /* ipiLineNumber            */
                                              INPUT cErrorMessage,                                     /* ipcMessageText           */
                                              INPUT "WAR":U,                                           /* ipcMessageType           */
                                              INPUT lAcknowledge)                                      /* iplAcknowledge           */
                     lSysLOSCalc   = TRUE.


            ASSIGN cTrackingMessage = "UndoLOCDateUpdates - Provider ARS setup in rule. Update of start/end date/time not allowed. Changes undone. " +
                                      "Provider: " + STRING(btt_auth_provider.doc_num,"9999999") + " ARS Rate: " + cArsRate + " Rule Value: " + cRuleValue.

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END. /* IF lValidRule AND LOOKUP(cArsRate,TRIM(ENTRY(1,cRuleValue,"|"))) <> 0 THEN */
          ELSE DO:
            /*
              - UndoLOCDateUpdates rule not setup or UndoLOCDateUpdates setup but ARS not in rule value
              - Update to start/end date/time allowed
              - Do not run system LOS calc
              - Run applyAuthLOSDetailsUser
            */
            ASSIGN lSysLOSCalc      = FALSE
                   cTrackingMessage = "UndoLOCDateUpdates rule not setup or UndoLOCDateUpdates setup but ARS not in rule value. Update to start/end date/time allowed. " +
                                      "Switch to User LOS calculation".

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          END.  /* ELSE - IF lValidRule AND LOOKUP(cArsRate,TRIM(ENTRY(1,cRuleValue,"|"))) <> 0 THEN */
        END. /* IF NOT lSysLOSCalc */
      END. /* IF AVAILABLE btt_auth_provider */
    END. /* IF AVAILABLE btt_auth_detail */

    /*
      This is to enforce, that once we've switch to User Calc,
      we continue running updates with applyAuthLOCDetailsUser.
      The user might update a field other that the start/end
      date, then we will not hit the above checks. But we
      still want to make sure we stick to User Calc.
    */
    IF lSysLOSCalc = TRUE
    AND CAN-FIND(FIRST btt_auth_provider
                 WHERE btt_auth_provider.auth_obj        = btt_auth.auth_obj
                   AND btt_auth_provider.los_calculation = FALSE)
    THEN
      ASSIGN lSysLOSCalc = FALSE.

    ASSIGN iCalcTime = MTIME.

    IF lSysLOSCalc THEN
    DO:
      mipEnv:Health:AuthService:calcAuthLOSDetails(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).

      ASSIGN cTrackingMessage = "calcAuthLOSDetails completed in " + STRING(MTIME - iCalcTime) + " milliseconds.".

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
    END. /* IF vlSysLOSCalc */
    ELSE DO:
      mipEnv:Health:AuthService:applyAuthLOSDetailsUser(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).

      ASSIGN cTrackingMessage = "applyAuthLOSDetailsUser completed in " + STRING(MTIME - iCalcTime) + " milliseconds.".

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
    END. /* ELSE - IF vlSysLOSCalc */
  END. /* FOR EACH btt_auth EXCLUSIVE-LOCK */

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuth Procedure
PROCEDURE _saveAuth :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

 { ma/app/maauthbussaveauth.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthCoding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthCoding Procedure
PROCEDURE _saveAuthCoding :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Coding Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

{ ma/app/maauthbussavecoding.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthCopay Procedure
PROCEDURE _saveAuthCopay :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Co-payment Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

{ ma/app/maauthbussavecopay.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthCrosswalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthCrosswalk Procedure
PROCEDURE _saveAuthCrosswalk :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Crosswalk Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

  DEFINE PARAMETER BUFFER btt_auth_crosswalk FOR tt_auth_crosswalk.


&IF {&DBDFMA} >= 010195 &THEN

  IF AVAILABLE btt_auth_crosswalk AND
  CAN-DO("{&ActionList}":U, btt_auth_crosswalk.record_action) AND NOT goErrorObject:CanFind("hataw":U, btt_auth_crosswalk.auth_crosswalk_obj, "":U, "ERR":U)
  THEN
  DO:

    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthCrosswalk(BUFFER btt_auth_crosswalk, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).

    /*
      Clear record action
    */
    ASSIGN btt_auth_crosswalk.record_action = "":U.

    VALIDATE btt_auth_crosswalk.
  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_crosswalk.record_action) THEN*/

&ENDIF

  { mip/inc/mipcatcherror.i }




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthDetail Procedure
PROCEDURE _saveAuthDetail :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Detail Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

{ ma/app/maauthbussavedetail.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthEpisode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthEpisode Procedure
PROCEDURE _saveAuthEpisode :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Episode Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

{ ma/app/maauthbussaveepisode.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthLimit Procedure
PROCEDURE _saveAuthLimit :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Limit Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

{ ma/app/maauthbussavelimit.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthMCSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthMCSavings Procedure
PROCEDURE _saveAuthMCSavings :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Savings Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

{ ma/app/maauthbussavemcsavings.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthProvider Procedure
PROCEDURE _saveAuthProvider :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Provider Record
  Parameters:
  Notes     : Business logic only, call data access to save data
------------------------------------------------------------------------------*/

{ ma/app/maauthbussaveprovider.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_setEpisodeObj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _setEpisodeObj Procedure
PROCEDURE _setEpisodeObj :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.


&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_episode FOR hat_auth_episode.


  IF AVAILABLE btt_auth THEN
  DO:

    FIND FIRST buf_auth_episode NO-LOCK
         WHERE buf_auth_episode.mem_num   = btt_auth.mem_num
           AND buf_auth_episode.dependant = btt_auth.dependant
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_auth_episode THEN
    DO:
      IF NOT CAN-FIND(FIRST tt_auth_episode NO-LOCK
                      WHERE tt_auth_episode.mem_num       = btt_auth.mem_num
                        AND tt_auth_episode.dependant     = btt_auth.dependant
                        AND tt_auth_episode.record_action = "MODIFY":U)
      THEN
      DO:
        CREATE tt_auth_episode.
        ASSIGN tt_auth_episode.record_action = "MODIFY":U
               tt_auth_episode.mem_num       = btt_auth.mem_num
               tt_auth_episode.dependant     = btt_auth.dependant.

        VALIDATE tt_auth_episode.
      END. /*IF NOT CAN-FIND(FIRST tt_auth_episode NO-LOCK*/


      /*
        Save Auth Episode
      */
      RUN _saveAuthEpisode IN TARGET-PROCEDURE.


      FIND FIRST tt_auth_episode NO-LOCK
           WHERE tt_auth_episode.mem_num   = btt_auth.mem_num
             AND tt_auth_episode.dependant = btt_auth.dependant
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE tt_auth_episode
      THEN ASSIGN btt_auth.auth_episode_obj = tt_auth_episode.auth_episode_obj.

    END. /*IF NOT AVAILABLE buf_auth_episode THEN */
    ELSE
      ASSIGN btt_auth.auth_episode_obj = buf_auth_episode.auth_episode_obj.

    VALIDATE btt_auth.

  END. /*IF AVAILABLE btt_auth THEN*/

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_setInsurerObj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _setInsurerObj Procedure
PROCEDURE _setInsurerObj :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:  <none>
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.

  DEFINE VARIABLE iClient     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dInsurerObj AS DECIMAL     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  ASSIGN iClient = 0.

  FOR EACH erm_insurer NO-LOCK:

    ASSIGN iClient     = iClient + 1
           dInsurerObj = erm_insurer.insurer_obj.
  END. /*FOR EACH erm_insurer NO-LOCK:*/

  IF iClient = 1 AND AVAILABLE btt_auth THEN
  DO:
    ASSIGN btt_auth.insurer_obj = dInsurerObj.

    VALIDATE btt_auth.
  END. /*IF iClient = 1 THEN */

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_setMainAuthProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _setMainAuthProvider Procedure
PROCEDURE _setMainAuthProvider :
/*------------------------------------------------------------------------------
  Purpose:     Procedure will determine if the main provider checkbox needs to be applied
               to the first auth provider entered on an auth
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

  DEFINE BUFFER btt_auth                    FOR tt_auth .
  DEFINE BUFFER btt_auth_provider           FOR tt_auth_provider.
&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER buf_auth_provider           FOR hat_auth_provider.

  FIND FIRST btt_auth NO-LOCK NO-ERROR.
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE btt_auth
  THEN
    FOR EACH btt_auth_provider no-lock
      WHERE (CAN-DO("{&ActionList}":U, btt_auth_provider.record_action)):

        EMPTY TEMP-TABLE ttAuthTypeConfig .

        mipEnv:Health:AuthService:getAuthTypeConfig( BUFFER btt_auth ,
                                                     BUFFER btt_auth_provider,
                                                     INPUT-OUTPUT TABLE ttAuthTypeConfig).

        FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.MainProvider = TRUE AND NOT btt_auth_provider.main_provider AND
        NOT CAN-FIND(FIRST buf_auth_provider NO-LOCK
                     WHERE buf_auth_provider.auth_obj = btt_auth_provider.auth_obj
                       AND buf_auth_provider.main_provider = TRUE
                       AND buf_auth_provider.auth_provider_obj <> btt_auth_provider.auth_provider_obj)
        THEN
          ASSIGN btt_auth_provider.main_provider = TRUE.

    END. /* FOR EACH btt_auth_provider NO-LOCK */


&ENDIF
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_updateAuthLineStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _updateAuthLineStatus Procedure
PROCEDURE _updateAuthLineStatus :
/*------------------------------------------------------------------------------
  Purpose:     Update the auth lines' statuses with the same status as the
               auth header.
  Parameters:  <none>
  Notes:       The auth lines should only be updated automatically if the
               'DefaultAuthHeadInfo' rule is activated.
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.

  DEFINE INPUT PARAMETER ipiOldAuthHeaderStatus AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipcOldAuthHeaderNote   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cStatusNote      AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cRuleValue       AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cTrackingMessage AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE lUpdateReason    AS LOGICAL      NO-UNDO.
  DEFINE VARIABLE lValidRule       AS LOGICAL      NO-UNDO.

  DEFINE BUFFER bbt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER bbt_auth_detail   FOR tt_auth_detail.
  DEFINE BUFFER bbt_auth_coding   FOR tt_auth_coding.

  ASSIGN cTrackingMessage = "Auth Status - ipiOldAuthHeaderStatus=" + STRING(ipiOldAuthHeaderStatus)
                          + " ipcOldAuthHeaderNote="      + ipcOldAuthHeaderNote
                          + " btt_auth.auth_status="      + STRING(btt_auth.auth_status)
                          + " btt_auth.auth_status_note=" + btt_auth.auth_status_note.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    With the limit checking a default authorisation status reason will be assigned when
    the limit is exceeded according to the status reason as setup in Authorisation Rule
    "ExceedLimitStatusReason" if the user doesn't have access to override the limit.
    The authorisation status will remain authorised.
    In this case we still want to update the provider/detail/coding status and status
    reason, even if the status reasons differ.
  */
  RUN _valUpdateStatusReason(INPUT btt_auth.insurer_obj,
                             INPUT btt_auth.option_code,
                             INPUT btt_auth.start_date,
                             INPUT btt_auth.auth_status,
                             INPUT btt_auth.auth_status_note,
                             OUTPUT lUpdateReason,
                             OUTPUT cStatusNote).

  ASSIGN cTrackingMessage = "Auth Status - lUpdateReason=" + STRING(lUpdateReason)
                          + " cStatusNote=" + cStatusNote.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
     Change the status on the provider records where applicable
  */
  FOR EACH bbt_auth_provider EXCLUSIVE-LOCK
      WHERE bbt_auth_provider.auth_obj = btt_auth.auth_obj:
    IF fnAllowLineStatusChange (INPUT ipiOldAuthHeaderStatus,
                                INPUT ipcOldAuthHeaderNote,
                                INPUT btt_auth.auth_status,
                                INPUT cStatusNote,
                                INPUT bbt_auth_provider.auth_status,
                                INPUT bbt_auth_provider.auth_status_note) THEN
    DO:
      ASSIGN cTrackingMessage = "Auth Status - fnAllowLineStatusChange=TRUE"
                              + " Provider="           + STRING(bbt_auth_provider.doc_num)
                              + " ProviderStatus="     + STRING(bbt_auth_provider.auth_status)
                              + " ProviderStatusNote=" + STRING(bbt_auth_provider.auth_status_note)
                              + " lUpdateReason="      + STRING(lUpdateReason).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      ASSIGN bbt_auth_provider.auth_status      = btt_auth.auth_status
             bbt_auth_provider.auth_status_note = IF lUpdateReason
                                                  THEN cStatusNote
                                                  ELSE bbt_auth_provider.auth_status_note
             bbt_auth_provider.record_action    = "MODIFY":U.

    END.  /* IF fnAllowLineStatusChange (INPUT ipiOldAuthHeaderStatus, */
  END.  /* FOR EACH bbt_auth_provider EXCLUSIVE-LOCK */

  /*
     Change the status on the provider detail records where applicable
  */
  FOR EACH bbt_auth_detail EXCLUSIVE-LOCK
      WHERE bbt_auth_detail.auth_obj = btt_auth.auth_obj:
    IF fnAllowLineStatusChange (INPUT ipiOldAuthHeaderStatus,
                                INPUT ipcOldAuthHeaderNote,
                                INPUT btt_auth.auth_status,
                                INPUT cStatusNote,
                                INPUT bbt_auth_detail.auth_status,
                                INPUT bbt_auth_detail.auth_status_note) THEN
    DO:
      ASSIGN cTrackingMessage = "Auth Status - fnAllowLineStatusChange=TRUE"
                              + " Detail="           + bbt_auth_detail.owning_alt_value
                              + " DetailStatus="     + STRING(bbt_auth_detail.auth_status)
                              + " DetailStatusNote=" + STRING(bbt_auth_detail.auth_status_note)
                              + " lUpdateReason="    + STRING(lUpdateReason).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      ASSIGN bbt_auth_detail.auth_status      = btt_auth.auth_status
             bbt_auth_detail.auth_status_note = IF lUpdateReason
                                                THEN cStatusNote
                                                ELSE bbt_auth_detail.auth_status_note
             bbt_auth_detail.record_action    = "MODIFY":U.
    END.  /* IF fnAllowLineStatusChange (INPUT ipiOldAuthHeaderStatus, */
  END.  /* FOR EACH bbt_auth_detail EXCLUSIVE-LOCK */

  /*
     Change the status on the coding records where applicable
  */
  FOR EACH bbt_auth_coding EXCLUSIVE-LOCK
      WHERE bbt_auth_coding.auth_obj = btt_auth.auth_obj:
    IF fnAllowLineStatusChange (INPUT ipiOldAuthHeaderStatus,
                                INPUT ipcOldAuthHeaderNote,
                                INPUT btt_auth.auth_status,
                                INPUT cStatusNote,
                                INPUT bbt_auth_coding.coding_status,
                                INPUT bbt_auth_coding.coding_status_note) THEN
    DO:
      ASSIGN cTrackingMessage = "Auth Status - fnAllowLineStatusChange=TRUE"
                              + " Coding="           + bbt_auth_coding.owning_alt_value
                              + " CodingStatus="     + STRING(bbt_auth_coding.coding_status)
                              + " CodingStatusNote=" + STRING(bbt_auth_coding.coding_status_note)
                              + " lUpdateReason="    + STRING(lUpdateReason).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      ASSIGN bbt_auth_coding.coding_status      = btt_auth.auth_status
             bbt_auth_coding.coding_status_note = IF lUpdateReason
                                                  THEN cStatusNote
                                                  ELSE bbt_auth_coding.coding_status_note
             bbt_auth_coding.record_action      = "MODIFY":U.
    END.  /* IF fnAllowLineStatusChange (INPUT ipiOldAuthHeaderStatus, */
  END.  /* FOR EACH bbt_auth_coding EXCLUSIVE-LOCK */

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_updateDetailLineDates) = 0 &THEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _updateDetailLineDates Procedure
PROCEDURE _updateDetailLineDates :
/*------------------------------------------------------------------------------
  Purpose:     Ensure we have the correct Authorisation dates before any
               additional fields are assigned, in case the date may impact
               what is assigned.
               Only run for new detail lines which are not LOC lines.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cMessage      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSuccess      AS LOGICAL     NO-UNDO.
 
&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER btt_auth          FOR  tt_auth.
  DEFINE BUFFER btt_auth_provider FOR  tt_auth_provider.
  DEFINE BUFFER btt_auth_detail   FOR  tt_auth_detail.

  FOR EACH btt_auth NO-LOCK:

    FOR EACH btt_auth_provider NO-LOCK
      WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj:

      FOR EACH btt_auth_detail EXCLUSIVE-LOCK
        WHERE btt_auth_detail.auth_obj          = btt_auth.auth_obj
          AND btt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj
          AND btt_auth_detail.auth_detail_obj   < 0 
          AND btt_auth_detail.record_action     = "MODIFY":U
          AND btt_auth_detail.quantity_los      = 0:
         
        IF  btt_auth_detail.start_date <> btt_auth_provider.start_date
        AND btt_auth_detail.start_date  = btt_auth.start_date
        THEN
          ASSIGN 
            btt_auth_detail.start_date = btt_auth_provider.start_date
            cMessage                   = "The detail line start date has been modified to the provider start date."
        
            lSuccess                   = goErrorObject:addError
                                               (INPUT "hatad":U,                           /* ipcOwningEntityMnemonic  */
                                                INPUT btt_auth_detail.auth_detail_obj,     /* ipdOwningEntityObj       */
                                                INPUT "":U,                                /* ipcOwningEntityKey       */
                                                INPUT "start_date":U,                      /* ipcFieldName             */
                                                INPUT btt_auth_detail.line_number,         /* ipiLineNumber            */
                                                INPUT cMessage,                            /* ipcMessageText           */
                                                INPUT "WAR":U).                            /* ipiMessageType           */
        
        IF  btt_auth_detail.start_ampm <> btt_auth_provider.start_ampm
        AND btt_auth_detail.start_ampm  = btt_auth.start_ampm
        THEN
          ASSIGN 
            btt_auth_detail.start_ampm = btt_auth_provider.start_ampm
            cMessage                   = "The detail line start time has been modified to the provider start time."
        
            lSuccess                   = goErrorObject:addError
                                               (INPUT "hatad":U,                           /* ipcOwningEntityMnemonic  */
                                                INPUT btt_auth_detail.auth_detail_obj,     /* ipdOwningEntityObj       */
                                                INPUT "":U,                                /* ipcOwningEntityKey       */
                                                INPUT "start_ampm":U,                      /* ipcFieldName             */
                                                INPUT btt_auth_detail.line_number,         /* ipiLineNumber            */
                                                INPUT cMessage,                            /* ipcMessageText           */
                                                INPUT "WAR":U).                            /* ipiMessageType           */
        
        IF  btt_auth_detail.end_date <> btt_auth_provider.end_date
        AND btt_auth_detail.end_date  = btt_auth.end_date
        THEN
          ASSIGN 
            btt_auth_detail.end_date = btt_auth_provider.end_date
            cMessage                 = "The detail line end date has been modified to the provider end date."
        
            lSuccess                 = goErrorObject:addError
                                             (INPUT "hatad":U,                           /* ipcOwningEntityMnemonic  */
                                              INPUT btt_auth_detail.auth_detail_obj,     /* ipdOwningEntityObj       */
                                              INPUT "":U,                                /* ipcOwningEntityKey       */
                                              INPUT "end_date":U,                        /* ipcFieldName             */
                                              INPUT btt_auth_detail.line_number,         /* ipiLineNumber            */
                                              INPUT cMessage,                            /* ipcMessageText           */
                                              INPUT "WAR":U).                            /* ipiMessageType           */
        
        IF  btt_auth_detail.end_ampm <> btt_auth_provider.end_ampm
        AND btt_auth_detail.end_ampm  = btt_auth.end_ampm
        THEN
          ASSIGN 
            btt_auth_detail.end_ampm = btt_auth_provider.end_ampm
            cMessage                 = "The detail line end time has been modified to the provider end time."
        
            lSuccess                 = goErrorObject:addError
                                             (INPUT "hatad":U,                           /* ipcOwningEntityMnemonic  */
                                              INPUT btt_auth_detail.auth_detail_obj,     /* ipdOwningEntityObj       */
                                              INPUT "":U,                                /* ipcOwningEntityKey       */
                                              INPUT "end_ampm":U,                        /* ipcFieldName             */
                                              INPUT btt_auth_detail.line_number,         /* ipiLineNumber            */
                                              INPUT cMessage,                            /* ipcMessageText           */
                                              INPUT "WAR":U).                            /* ipiMessageType           */
      
        VALIDATE btt_auth_detail.

      END.  /* FOR EACH btt_auth_detail EXCLUSIVE-LOCK */
    END.  /* FOR EACH btt_auth_provider NO-LOCK */
  END.  /* FOR EACH btt_auth NO-LOCK: */
&ENDIF
  
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_updateEpisodeObj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _updateEpisodeObj Procedure
PROCEDURE _updateEpisodeObj :
/*------------------------------------------------------------------------------
  Purpose:     When a 99 Dependant is changed to a valid Dependant,
               update hat_auth_episode.dependant value to new valid Dependant number.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 010195 &THEN
  DEFINE PARAMETER BUFFER buf_auth     FOR hat_auth.         /* This buffer will contain the db-version of btt_auth */
  DEFINE INPUT PARAMETER  ipiDependant AS INTEGER    NO-UNDO.

  DEFINE BUFFER buf_auth_episode FOR hat_auth_episode.
  DEFINE BUFFER btt_auth_episode FOR tt_auth_episode.

  FIND  buf_auth_episode EXCLUSIVE-LOCK
  WHERE buf_auth_episode.auth_episode_obj = buf_auth.auth_episode_obj
  NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE buf_auth_episode THEN
  DO:
    /*
      Create the temp-table as the temp-table is not available
      update record_action to "MODIFY"
    */
    CREATE btt_auth_episode.
    ASSIGN
      btt_auth_episode.auth_episode_obj = buf_auth_episode.auth_episode_obj
      btt_auth_episode.mem_num          = buf_auth_episode.mem_num
      btt_auth_episode.dependant        = ipiDependant
      btt_auth_episode.episode_num      = buf_auth_episode.episode_num
      btt_auth_episode.record_action    = "MODIFY".

    VALIDATE btt_auth_episode.

    RUN _saveAuthEpisode IN TARGET-PROCEDURE.

  END. /* IF AVAILABLE tt_auth_episode */
&ENDIF
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuth Procedure
PROCEDURE _validateAuth PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Buffer
  Parameters:
  Notes     : Business logic validation
------------------------------------------------------------------------------*/

 { ma/app/maauthbusvalauth.i }


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCoding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCoding Procedure
PROCEDURE _validateAuthCoding :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Coding Buffer
  Parameters:
  Notes     : Business logic validation
------------------------------------------------------------------------------*/

{ ma/app/maauthbusvalcoding.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCopay Procedure
PROCEDURE _validateAuthCopay :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Co-pay Buffer
  Parameters:
  Notes     : Business logic validation
------------------------------------------------------------------------------*/

{ ma/app/maauthbusvalcopay.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthDepBenefitDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthDepBenefitDate Procedure
PROCEDURE _validateAuthDepBenefitDate :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER    BUFFER    btt_auth     FOR tt_auth.
  DEFINE INPUT-OUTPUT PARAMETER oErrorObject AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cRuleLoadStatus       AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE cBlockMessage         AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE lOverlap              AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE lWarnOnly             AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE cRuleCode             AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE lJoinValidRule        AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cJoinRuleValue        AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE lPMBWaitOvrValidRule  AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cPMBWaitOvrRuleValue  AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE lPMBWaitCatValidRule  AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cPMBWaitCatRuleValue  AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE lPMBWaitFlagValidRule AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cPMBWaitFlagRuleValue AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE iPMBWaitFlagNum       AS INTEGER                NO-UNDO.
  DEFINE VARIABLE cPMBWaitFlagValue     AS CHARACTER              NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER memdep            FOR memdep.
  DEFINE BUFFER flagdet           FOR flagdet.
  DEFINE BUFFER hat_auth_coding   FOR hat_auth_coding.
  DEFINE BUFFER btt_auth_coding   FOR tt_auth_coding.

  /*
     Only run this procedure if the auth header is not cancelled or declined,
     and if a primary or main diagnosis (ICD) has been captured.
  */
  IF btt_auth.auth_status = 5 /* Cancelled */
  OR btt_auth.auth_status = 6 /* Declined */ THEN
    RETURN.

  IF NOT CAN-FIND(FIRST hat_auth_coding NO-LOCK
                  WHERE hat_auth_coding.auth_obj               = btt_auth.auth_obj
                  AND   hat_auth_coding.owning_entity_mnemonic = "diagnos":U
                  AND  (hat_auth_coding.main_code              = YES
                  OR    hat_auth_coding.primary_code           = YES)
                  AND   hat_auth_coding.coding_status         <> 5
                  AND   hat_auth_coding.coding_status         <> 6)
  AND NOT CAN-FIND(FIRST btt_auth_coding NO-LOCK
                  WHERE btt_auth_coding.auth_obj               = btt_auth.auth_obj
                  AND   btt_auth_coding.owning_entity_mnemonic = "diagnos":U
                  AND  (btt_auth_coding.main_code              = YES
                  OR    btt_auth_coding.primary_code           = YES)
                  AND   btt_auth_coding.coding_status         <> 5
                  AND   btt_auth_coding.coding_status         <> 6)
  THEN
    ASSIGN lWarnOnly = TRUE.
  ELSE
    ASSIGN lWarnOnly = FALSE.

  FIND FIRST memdep NO-LOCK
       WHERE memdep.mem-num   = btt_auth.mem_num
       AND   memdep.dependant = btt_auth.dependant
       NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE memdep
  AND memdep.joined-date <> memdep.benefit-date THEN
  DO:
    mipEnv:Health:mautility:checkOverlappingPeriods(memdep.joined-date,
                                                    memdep.benefit-date,
                                                    btt_auth.start_date,
                                                    btt_auth.end_date,
                                                    lOverlap).
    IF lOverlap THEN
    DO:
      IF btt_auth.pmb_indicator THEN
      DO:
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                       INPUT  "PMBWaitOvr":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lPMBWaitOvrValidRule,
                                                       OUTPUT cPMBWaitOvrRuleValue).
        IF lPMBWaitOvrValidRule AND cPMBWaitOvrRuleValue = "yes":U THEN
        DO:
          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                         INPUT  btt_auth.option_code,
                                                         INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                         INPUT  "PMBWaitCategory":U,
                                                         INPUT  btt_auth.start_date,
                                                         OUTPUT lPMBWaitCatValidRule,
                                                         OUTPUT cPMBWaitCatRuleValue).
          IF lPMBWaitCatValidRule THEN
          DO:
            IF memdep.wait-per-cat = cPMBWaitCatRuleValue
            THEN
              ASSIGN cBlockMessage = "This is a PMB Authorisation in dependant waiting period. Authorisation will continue."
                                   + "[HELP=Auth Rule Code: PMBWaitOvr, PMBWaitCategory]"
                     lSuccess      = oErrorObject:addError
                                         (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                          INPUT "":U,                  /* ipcOwningEntityKey       */
                                          INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                          INPUT cBlockMessage,         /* ipcMessageText           */
                                          INPUT "WAR":U).              /* ipcMessageType           */
            ELSE
              RUN _checkDependantWaitRules IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT memdep.benefit-date, INPUT lWarnOnly, INPUT-OUTPUT oErrorObject ).
          END.  /* IF lPMBWaitCatValidRule THEN */
          ELSE DO:
            mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                           INPUT  btt_auth.option_code,
                                                           INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                           INPUT  "PMBWaitFlag":U,
                                                           INPUT  btt_auth.start_date,
                                                           OUTPUT lPMBWaitFlagValidRule,
                                                           OUTPUT cPMBWaitFlagRuleValue).
            IF lPMBWaitFlagValidRule THEN
            DO:
              ASSIGN iPMBWaitFlagNum   = INTEGER(ENTRY(1,cPMBWaitFlagRuleValue,"|":U))
                     cPMBWaitFlagValue = ENTRY(2,cPMBWaitFlagRuleValue,"|":U).
              FIND FIRST flagdet NO-LOCK
                WHERE flagdet.scheme-code  = 00
                AND   flagdet.type         = "M":U
                AND   flagdet.flag-num     = iPMBWaitFlagNum
                AND   flagdet.key          = btt_auth.mem_num
                AND   flagdet.flag-value   = cPMBWaitFlagValue
                AND   flagdet.effect-date <= btt_auth.start_date
                NO-ERROR.
              IF AVAILABLE flagdet
              AND flagdet.flag-value = cPMBWaitFlagValue
              THEN
                ASSIGN cBlockMessage = "This is a PMB Authorisation in dependant waiting period. Authorisation will continue."
                                     + "[HELP=Auth Rule Code: PMBWaitOvr, PMBWaitCategory, PMBWaitFlag]"
                       lSuccess      = oErrorObject:addError
                                         (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                          INPUT "":U,                  /* ipcOwningEntityKey       */
                                          INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                          INPUT cBlockMessage,         /* ipcMessageText           */
                                          INPUT "WAR":U).              /* ipcMessageType           */
              ELSE
                RUN _checkDependantWaitRules IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT memdep.benefit-date, INPUT lWarnOnly, INPUT-OUTPUT oErrorObject ).
            END.  /* IF lPMBWaitFlagValidRule THEN */
            ELSE
              ASSIGN cBlockMessage = "This is a PMB Authorisation in dependant waiting period. Authorisation will continue."
                                   + "[HELP=Auth Rule Code: PMBWaitOvr, PMBWaitCategory, PMBWaitFlag]"
                     lSuccess      = oErrorObject:addError
                                         (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                          INPUT "":U,                  /* ipcOwningEntityKey       */
                                          INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                          INPUT cBlockMessage,         /* ipcMessageText           */
                                          INPUT "WAR":U).              /* ipcMessageType           */
          END.  /* ELSE - IF lPMBWaitCatValidRule THEN */
        END.  /* IF lPMBWaitOvrValidRule AND cPMBWaitOvrRuleValue = "yes":U THEN */
        ELSE
          RUN _checkDependantWaitRules IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT memdep.benefit-date, INPUT lWarnOnly, INPUT-OUTPUT oErrorObject ).
      END.  /* IF btt_auth.pmb_indicator THEN */
      ELSE
        RUN _checkDependantWaitRules IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT memdep.benefit-date, INPUT lWarnOnly, INPUT-OUTPUT oErrorObject ).
    END.  /* IF lOverlap THEN */
    ELSE DO:
      IF  btt_auth.start_date < memdep.joined-date
      AND btt_auth.end_date   < memdep.joined-date THEN
      DO:
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                       INPUT  "DependantInactive":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lJoinValidRule,
                                                       OUTPUT cJoinRuleValue).
        IF lJoinValidRule THEN
        DO:
          ASSIGN cBlockMessage = "Authorisation can't be loaded. Dependant joined date " +
                                 STRING(memdep.joined-date) + " is after the authorisation date ":U + STRING(btt_auth.start_date) + ".":U.
          RUN _validateRuleSetup IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                       INPUT "DependantInactive":U,
                                                       INPUT cJoinRuleValue,
                                                       INPUT FALSE,
                                                       INPUT cBlockMessage,
                                                       INPUT-OUTPUT oErrorObject).

        END.  /* IF lValidRule THEN */
      END. /* IF  btt_auth.start_date < memdep.joined-date */
    END. /* ELSE DO: IF lOverlap THEN */
  END. /* IF AVAILABLE memdep THEN */

&ENDIF

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthDepInactivity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthDepInactivity Procedure
PROCEDURE _validateAuthDepInactivity :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER    BUFFER    btt_auth     FOR tt_auth.
  DEFINE INPUT-OUTPUT PARAMETER oErrorObject AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cRuleLoadStatus       AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cBlockMessage         AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE dInactStart           AS DATE              NO-UNDO.
  DEFINE VARIABLE dInactEnd             AS DATE              NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER         NO-UNDO.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                 INPUT  btt_auth.option_code,
                                                 INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                 INPUT  "DependantInactive":U,
                                                 INPUT  btt_auth.start_date,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  IF lValidRule THEN
  DO:
    ASSIGN lSuccess = mipEnv:Health:maMember:validateDependantInactivity(INPUT  btt_auth.mem_num,
                                                                         INPUT  btt_auth.dependant,
                                                                         INPUT  btt_auth.start_date,
                                                                         INPUT  btt_auth.end_date,
                                                                         OUTPUT dInactStart,
                                                                         OUTPUT dInactEnd) .
    IF dInactStart <> ? THEN
    DO:
      ASSIGN cBlockMessage = "Authorisation can't be loaded. Period overlaps with dependant inactivity period " +
                             STRING(dInactStart) + " - ":U + STRING(dInactEnd) + ".":U.
      RUN _validateRuleSetup IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                   INPUT "DependantInactive":U,
                                                   INPUT cRuleValue,
                                                   INPUT FALSE,
                                                   INPUT cBlockMessage,
                                                   INPUT-OUTPUT oErrorObject).
    END.  /* IF (btt_auth.start_date >= dInactStart AND... */

  END.  /* IF lValidRule THEN */

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthDetail Procedure
PROCEDURE _validateAuthDetail :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Detail Buffer
  Parameters:
  Notes     : Business logic validation
------------------------------------------------------------------------------*/

{ ma/app/maauthbusvaldetail.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthEpisode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthEpisode Procedure
PROCEDURE _validateAuthEpisode :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Episode Buffer
  Parameters:
  Notes     : Business logic validation
------------------------------------------------------------------------------*/

{ ma/app/maauthbusvalepisode.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthLimit Procedure
PROCEDURE _validateAuthLimit :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER btt_auth_limit FOR tt_auth_limit.

DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.

ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

IF AVAILABLE btt_auth_limit THEN
DO:

END. /*IF AVAILABLE btt_auth_limit THEN*/

{ mip/inc/mipcatcherror.i
  &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject NO-ERROR."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthMCSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthMCSavings Procedure
PROCEDURE _validateAuthMCSavings :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Savings Buffer
  Parameters:
  Notes     : Business logic validation
------------------------------------------------------------------------------*/

{ ma/app/maauthbusvalmcsavings.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthProvider Procedure
PROCEDURE _validateAuthProvider :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Provider Buffer
  Parameters:
  Notes     : Business logic validation
------------------------------------------------------------------------------*/

{ ma/app/maauthbusvalprovider.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthResignation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthResignation Procedure
PROCEDURE _validateAuthResignation :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER    BUFFER    btt_auth     FOR tt_auth.
  DEFINE INPUT-OUTPUT PARAMETER oErrorObject AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cBlockMessage         AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cRuleCode             AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cRuleLoadStatus       AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE dResignDate           AS DATE              NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL           NO-UNDO.

  /*
     Validate Dependant Resign Rule
  */
  ASSIGN lSuccess = mipEnv:Health:maMember:validateResignation(INPUT "DEPENDANT":U,
                                                               INPUT  btt_auth.mem_num,
                                                               INPUT  btt_auth.dependant,
                                                               INPUT  btt_auth.start_date,
                                                               INPUT  btt_auth.end_date,
                                                               OUTPUT dResignDate).
  IF dResignDate <> ? THEN
  DO:
    IF dResignDate <= btt_auth.start_date THEN
      ASSIGN cRuleCode = "ResignDependantEntire":U.
    ELSE
      ASSIGN cRuleCode = "ResignDependantPart":U.

    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                   INPUT  btt_auth.option_code,
                                                   INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                   INPUT  cRuleCode,
                                                   INPUT  btt_auth.start_date,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).
    IF NOT lValidRule THEN
    DO:
      ASSIGN cRuleCode = "ResignDependant":U.

      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                     INPUT  cRuleCode,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
    END.  /* IF NOT lValidRule THEN */

    IF lValidRule THEN
    DO:
      IF cRuleCode = "ResignDependantPart" THEN
         ASSIGN cBlockMessage = "Authorisation can't be loaded. Auth dates overlap with dependant's resigned date - " + STRING(dResignDate).
      ELSE
         ASSIGN cBlockMessage = "Authorisation can't be loaded. Dependant is resigned from " + STRING(dResignDate).

      RUN _validateRuleSetup IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                   INPUT cRuleCode,
                                                   INPUT cRuleValue,
                                                   INPUT FALSE,
                                                   INPUT cBlockMessage,
                                                   INPUT-OUTPUT oErrorObject).
    END.  /* IF lValidRule THEN */
  END. /* IF dResignDate <> ? THEN */

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthSuspendRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthSuspendRule Procedure
PROCEDURE _validateAuthSuspendRule :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER    BUFFER    btt_auth     FOR tt_auth.
  DEFINE INPUT        PARAMETER ipcRuleType  AS CHARACTER         NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER oErrorObject AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cRuleLoadStatus       AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cBlockMessage         AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE dSuspendDate          AS DATE               NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE oMemOptSearch         AS cls.mamembersearch NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cRuleCode             AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE iLogTime              AS INTEGER            NO-UNDO.
  DEFINE VARIABLE cTrackingMessage      AS CHARACTER          NO-UNDO.

  CASE ipcRuleType:

    WHEN "MEMBER" THEN
    DO:
      /*
         Validate Member Suspension Rule
      */
      ASSIGN cRuleCode = "SuspendMember":U.
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                     INPUT  cRuleCode,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
      IF lValidRule
      THEN
        ASSIGN lSuccess = mipEnv:Health:maMember:validateSuspension(INPUT "MEMBER":U,
                                                                    INPUT  btt_auth.mem_num,
                                                                    INPUT  00,
                                                                    INPUT  btt_auth.start_date,
                                                                    INPUT  btt_auth.end_date,
                                                                    OUTPUT dSuspendDate).
    END. /* WHEN "MEMBER" THEN */

    WHEN "DEPENDANT" THEN
    DO:
      /*
         Validate Dependant Suspension Rule
      */
      ASSIGN cRuleCode = "SuspendDependant":U.
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                     INPUT  cRuleCode,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
      IF lValidRule
      THEN
        ASSIGN lSuccess = mipEnv:Health:maMember:validateSuspension(INPUT "DEPENDANT":U,
                                                                    INPUT  btt_auth.mem_num,
                                                                    INPUT  btt_auth.dependant,
                                                                    INPUT  btt_auth.start_date,
                                                                    INPUT  btt_auth.end_date,
                                                                    OUTPUT dSuspendDate).
    END. /* WHEN "DEPENDANT" THEN */

    WHEN "COMPANY" THEN
    DO:
      /*
         Validate Company Suspension Rule
      */
      ASSIGN cRuleCode = "SuspendCompany":U
             iLogTime  = MTIME.
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                     INPUT  cRuleCode,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).

      ASSIGN cTrackingMessage = "AuthMaintenance, getAuthRuleValue(SuspendCompany) completed in - " +
                                STRING(MTIME - iLogTime) + " milliseconds".
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF lValidRule THEN
      DO:
        ASSIGN iLogTime = MTIME.

        FIND LAST memsch NO-LOCK
             WHERE memsch.scheme-code = btt_auth.option_code
             AND   memsch.mem-num     = btt_auth.mem_num
             AND   memsch.start-date <= btt_auth.start_date
             NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE memsch
        THEN
          FIND LAST memsch NO-LOCK
               WHERE memsch.mem-num     = btt_auth.mem_num
               AND   memsch.start-date <= btt_auth.start_date
               NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        ASSIGN cTrackingMessage = "FIND MEMSCH completed in - " + STRING(MTIME - iLogTime) + " milliseconds":U.
        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        IF AVAILABLE memsch THEN
        DO:
          ASSIGN
            iLogTime = MTIME
            lSuccess = mipEnv:Health:maMember:validateSuspension
                         (INPUT "COMPANY":U,
                          INPUT  memsch.comp-code,
                          INPUT  btt_auth.dependant,
                          INPUT  btt_auth.start_date,
                          INPUT  btt_auth.end_date,
                          OUTPUT dSuspendDate)
            cTrackingMessage = "maMember, validateSuspension completed in - " + STRING(MTIME - iLogTime) + " milliseconds":U.
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END.  /* IF AVAILABLE memsch THEN  */
      END.  /* IF lValidRule THEN */
    END. /* WHEN "COMPANY" THEN */
  END CASE.  /* ipcRuleType */

  IF dSuspendDate <> ? AND lValidRule THEN
  DO:
    ASSIGN cBlockMessage = "Authorisation can't be loaded. " + ipcRuleType + " is suspended from " + STRING(dSuspendDate).
    RUN _validateRuleSetup IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                 INPUT cRuleCode,
                                                 INPUT cRuleValue,
                                                 INPUT FALSE,
                                                 INPUT cBlockMessage,
                                                 INPUT-OUTPUT oErrorObject ).
  END. /* IF dSuspendDate <> ? AND... */

  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oMemOptSearch) THEN DELETE OBJECT oMemOptSearch."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthTypeProviderMandatory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthTypeProviderMandatory Procedure
PROCEDURE _validateAuthTypeProviderMandatory :
/*------------------------------------------------------------------------------
  Purpose:     Step through all the auth type provider records and make sure all
               the provider types that are mandatory, are available on the auth.
  Parameters:  btt_auth buffer for temp-table tt_auth
  Notes:       This validation is only done when the auth is complete.
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.

DEFINE VARIABLE oAuthTypeSearch  AS cls.maauthtypesearch NO-UNDO.
DEFINE VARIABLE oProviderType    AS cls.mipacronym       NO-UNDO.

DEFINE VARIABLE cMessage         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iCountProviders  AS INTEGER     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

DEFINE BUFFER btt_auth_provider      FOR tt_auth_provider.
DEFINE BUFFER ctt_auth_provider      FOR tt_auth_provider.
DEFINE BUFFER tt_auth_type           FOR tt_auth_type.
DEFINE BUFFER tt_auth_type_provider  FOR tt_auth_type_provider.

/*
   Do not check these validations if the auth is not yet completed.
*/
IF btt_auth.auth_incomplete THEN
  RETURN.

ASSIGN oAuthTypeSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE)
       lSuccess        = oAuthTypeSearch:SetCriteria("BufferList":U, "tt_auth_type,tt_auth_type_provider":U)
       lSuccess        = oAuthTypeSearch:SetFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, btt_auth.auth_type_obj).

oAuthTypeSearch:fetchData().

FIND FIRST tt_auth_type NO-LOCK NO-ERROR.

{ mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

FOR EACH tt_auth_type_provider NO-LOCK
    WHERE tt_auth_type_provider.auth_type_obj   = btt_auth.auth_type_obj
    AND   tt_auth_type_provider.effective_date <= btt_auth.start_date
    AND  (tt_auth_type_provider.end_date       >= btt_auth.start_date OR tt_auth_type_provider.end_date = ?):

  IF (tt_auth_type_provider.insurer_obj <> 0 AND tt_auth_type_provider.insurer_obj <> btt_auth.insurer_obj)
  OR (tt_auth_type_provider.option_code <> 0 AND tt_auth_type_provider.option_code <> btt_auth.option_code)
  THEN
    NEXT.

  IF tt_auth_type_provider.mandatory AND tt_auth_type_provider.provider_type <> "":U THEN
  DO:
    FIND FIRST btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
         AND   btt_auth_provider.provider_type = tt_auth_type_provider.provider_type
         NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    /*
       If the provider is in the temp-table and marked to be deleted, then prevent the
       record from being deleted.
    */
    IF AVAILABLE btt_auth_provider AND btt_auth_provider.record_action = "DELETE":U
    THEN DO:
      IF NOT CAN-FIND(FIRST ctt_auth_provider
                      WHERE ctt_auth_provider.auth_obj       = btt_auth.auth_obj
                      AND   ctt_auth_provider.provider_type  = tt_auth_type_provider.provider_type
                      AND   ROWID(ctt_auth_provider)        <> ROWID(btt_auth_provider)) THEN
      DO:
        ASSIGN oProviderType = NEW cls.mipacronym(?, FALSE, "ma_acAuthProviderType":U, ?).

        oProviderType:focusAcronym("KEY":U, btt_auth_provider.provider_type) NO-ERROR.

        IF oProviderType:AcronymInFocus THEN
          ASSIGN cMessage = SUBSTITUTE("The &1 provider is mandatory for auth type '&2' and may not be deleted.",
                                       oProviderType:AcronymLabel, tt_auth_type.auth_type).
        ELSE
          ASSIGN cMessage = SUBSTITUTE("The provider is mandatory for auth type '&1' and may not be deleted.",
                                       tt_auth_type.auth_type).

        ASSIGN lSuccess = goErrorObject:addError
                          (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                           INPUT "":U,                                /* ipcOwningEntityKey       */
                           INPUT "provider_type":U,                   /* ipcFieldName             */
                           INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                           INPUT cMessage,                            /* ipcMessageText           */
                           INPUT "ERR":U).                            /* ipiMessageType           */
        RETURN.
      END.  /* IF NOT CAN-FIND(FIRST ctt_auth_provider */
    END. /* IF AVAILABLE btt_auth_provider AND btt_auth_provider.record_action = "DELETE" */

    ELSE IF NOT AVAILABLE btt_auth_provider THEN
    DO:
      ASSIGN oProviderType = NEW cls.mipacronym(?, FALSE, "ma_acAuthProviderType":U, ?).

      oProviderType:focusAcronym("KEY":U, tt_auth_type_provider.provider_type) NO-ERROR.

      IF oProviderType:AcronymInFocus THEN
        ASSIGN cMessage = SUBSTITUTE("The &1 provider is mandatory for auth type '&2'.",
                                     oProviderType:AcronymLabel, tt_auth_type.auth_type).
      ELSE
        ASSIGN cMessage = SUBSTITUTE("At least 1 provider is required for auth type '&1'.",
                                     tt_auth_type.auth_type).

      goErrorObject:addError
        ( INPUT "hatau":U,         /* ipcOwningEntityMnemonic  */
          INPUT btt_auth.auth_obj, /* ipdOwningEntityObj       */
          INPUT "":U,              /* ipcOwningEntityKey       */
          INPUT 1,                 /* ipiLineNumber            */
          INPUT cMessage,          /* ipcMessageText           */
          INPUT "ERR":U ).         /* ipiMessageType           */
      RETURN.
    END. /* ELSE IF NOT AVAILABLE btt_auth_provider THEN */
  END.  /* IF tt_auth_type_provider.mandatory AND ... */
END. /* FOR EACH tt_auth_type_provider NO-LOCK */

&ENDIF

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oAuthTypeSearch) THEN DELETE OBJECT oAuthTypeSearch.
                IF VALID-OBJECT(oProviderType)   THEN DELETE OBJECT oProviderType."}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateDateChanges) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateDateChanges Procedure
PROCEDURE _validateDateChanges :
/*-------------------------------------------------------------------------------------
  Purpose   : Validate date changes updated in _checkForDateChanges
  Parameters:
  Notes     :
---------------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cValidMessage     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFieldChanged     AS CHARACTER NO-UNDO.

  DEFINE BUFFER buf_auth             FOR hat_auth.
  DEFINE BUFFER btt_auth             FOR tt_auth.
  DEFINE BUFFER btt_auth_provider    FOR tt_auth_provider.
  DEFINE BUFFER buf_auth_provider    FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_detail      FOR hat_auth_detail .
  DEFINE BUFFER buf_auth_coding      FOR hat_auth_coding.
  DEFINE BUFFER btt_auth_coding      FOR tt_auth_coding.
  DEFINE BUFFER btt_auth_detail      FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_mc_savings  FOR tt_auth_mc_savings.

  AUTHBLOCK:
  FOR EACH btt_auth EXCLUSIVE-LOCK
     WHERE btt_auth.auth_obj > 0.00:

    IF CAN-FIND(FIRST btt_auth_provider
                WHERE btt_auth_provider.auth_obj        = btt_auth.auth_obj
                  AND btt_auth_provider.los_calculation = FALSE) AND
       btt_auth._final_save = FALSE
    THEN
      NEXT AUTHBLOCK.

    FIND buf_auth EXCLUSIVE-LOCK
      WHERE buf_auth.auth_obj = btt_auth.auth_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE buf_auth THEN
    DO:
      /*
        Check if provider dates changed
      */
      FOR EACH btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj:

        FIND FIRST buf_auth_provider NO-LOCK
             WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE buf_auth_provider
        THEN DO:
          /*
            Check for details with start and end dates which dont fall within the auth start and end dates
          */
          FIND FIRST  btt_auth_detail NO-LOCK
               WHERE  btt_auth_detail.auth_obj          = buf_auth_provider.auth_obj
                 AND  btt_auth_detail.auth_provider_obj = buf_auth_provider.auth_provider_obj
                 AND (btt_auth_detail.start_date        < btt_auth_provider.start_date
                  OR (btt_auth_detail.end_date          > btt_auth_provider.end_date AND btt_auth_provider.end_date <> ?))
                 AND  btt_auth_detail.record_action    <> "DELETE":U NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE btt_auth_detail THEN
          DO:
            FIND FIRST buf_auth_detail NO-LOCK
                 WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj  NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            ASSIGN cValidMessage = "Auth Detail lines linked to provider outside the provider period.":U

                   cFieldChanged = IF      buf_auth_provider.start_date <> btt_auth_provider.start_date
                                   OR     (AVAILABLE buf_auth_detail
                                       AND buf_auth_detail.start_date   <> btt_auth_detail.start_date )  THEN "start_date":U
                                   ELSE
                                   IF      buf_auth_provider.end_date   <> btt_auth_provider.end_date
                                   OR     (AVAILABLE buf_auth_detail
                                       AND buf_auth_detail.end_date     <> btt_auth_detail.end_date)     THEN "end_date":U

                                   ELSE "":U.

            goErrorObject:addError(INPUT 'hatap:' + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_detail.auth_detail_obj,  /* ipdOwningEntityObj       */
                                   INPUT "":U,                             /* ipcOwningEntityKey       */
                                   INPUT cFieldChanged,                    /* ipcFieldName             */
                                   INPUT btt_auth_detail.line_number,      /* ipiLineNumber            */
                                   INPUT cValidMessage,                    /* ipcMessageText           */
                                   INPUT "ERR":U).                         /* ipcMessageType           */
          END. /* IF AVAILABLE btt_auth_detail THEN */
        END. /* IF AVAILABLE buf_auth_provider */
      END. /* FOR EACH btt_auth_provider NO-LOCK */

      ASSIGN cValidMessage = "":U.
      /*
        Check for providers with start and end dates which dont fall within the auth start and end dates
      */

      FIND FIRST  btt_auth_provider
           WHERE  btt_auth_provider.auth_obj       = buf_auth.auth_obj
             AND (btt_auth_provider.start_date     < btt_auth.start_date
              OR (btt_auth_provider.start_date     > btt_auth.end_date AND btt_auth.end_date <> ?)
              OR (btt_auth_provider.end_date       > btt_auth.end_date AND btt_auth.end_date <> ?))
             AND  btt_auth_provider.record_action <> "DELETE":U NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE btt_auth_provider THEN
      DO:

        FIND FIRST buf_auth_provider NO-LOCK
             WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj NO-ERROR .

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        ASSIGN cValidMessage = "Auth Provider/s linked to authorisation outside the auth period.":U

               cFieldChanged = IF      buf_auth.start_date          <> btt_auth.start_date
                               OR     (AVAILABLE buf_auth_provider
                                   AND buf_auth_provider.start_date <> btt_auth_provider.start_date)  THEN "start_date":U
                               ELSE
                               IF      buf_auth.end_date            <> btt_auth.end_date
                               OR     (AVAILABLE buf_auth_provider
                               AND     buf_auth_provider.end_date   <> btt_auth_provider.end_date)      THEN "end_date":U

                               ELSE "":U .


        goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "":U,                  /* ipcOwningEntityKey       */
                               INPUT cFieldChanged,         /* ipcFieldName             */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT cValidMessage,         /* ipcMessageText           */
                               INPUT "ERR":U).              /* ipcMessageType           */
      END. /* IF CAN-FIND(FIRST  btt_auth_provider */

      /*
        Check for coding with start and end dates which dont fall within the auth start and end dates
      */
      FIND FIRST   btt_auth_coding NO-LOCK
           WHERE   btt_auth_coding.auth_obj       = buf_auth.auth_obj
             AND  (btt_auth_coding.start_date     < btt_auth.start_date
              OR  (btt_auth_coding.start_date     > btt_auth.end_date   AND btt_auth.end_date   <> ?)
              OR  (btt_auth_coding.end_date       > btt_auth.end_date   AND btt_auth.end_date <> ?))
             AND   btt_auth_coding.record_action <> "DELETE":U NO-ERROR.

      IF AVAILABLE  btt_auth_coding  THEN
      DO:
        FIND FIRST buf_auth_coding
             WHERE buf_auth_coding.auth_coding_obj = btt_auth_coding.auth_coding_obj NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        ASSIGN cValidMessage = "Auth coding (":U + btt_auth_coding.owning_alt_value + ") linked to authorisation outside the auth period.":U
               cFieldChanged = IF      buf_auth.start_date          <> btt_auth.start_date
                               OR     (AVAILABLE buf_auth_coding
                                   AND buf_auth_coding.start_date   <> btt_auth_coding.start_date)  THEN "start_date":U
                               ELSE
                               IF      buf_auth.end_date            <> btt_auth.end_date
                               OR     (AVAILABLE buf_auth_coding
                               AND     buf_auth_coding.end_date     <> btt_auth_coding.end_date)    THEN "end_date":U

                               ELSE "":U .

        goErrorObject:addError(INPUT "hatau":U,               /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,       /* ipdOwningEntityObj       */
                               INPUT "":U,                    /* ipcOwningEntityKey       */
                               INPUT cFieldChanged ,          /* ipcFieldName             */
                               INPUT btt_auth.line_number,    /* ipiLineNumber            */
                               INPUT cValidMessage,           /* ipcMessageText           */
                               INPUT "ERR":U).                /* ipcMessageType           */
      END. /*  IF AVAILABLE  btt_auth_coding*/

      /*
        Check for coding with procedure dates which dont fall within the auth start and end dates
      */
      FIND FIRST   btt_auth_coding NO-LOCK
           WHERE   btt_auth_coding.auth_obj       = buf_auth.auth_obj
             AND  ((btt_auth_coding.procedure_date < btt_auth.start_date AND btt_auth_coding.procedure_date <> ? AND btt_auth.start_date <> ?)
              OR   (btt_auth_coding.procedure_date > btt_auth.end_date   AND btt_auth_coding.procedure_date <> ? AND btt_auth.end_date   <> ?))
             AND   btt_auth_coding.record_action <> "DELETE":U NO-ERROR.

      IF AVAILABLE  btt_auth_coding  THEN
      DO:
        ASSIGN cValidMessage = "Auth coding (":U + btt_auth_coding.owning_alt_value + ") procedure date linked to authorisation outside the auth period.":U .

        goErrorObject:addError(INPUT "hatau":U,               /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,       /* ipdOwningEntityObj       */
                               INPUT "":U,                    /* ipcOwningEntityKey       */
                               INPUT btt_auth.line_number,    /* ipiLineNumber            */
                               INPUT cValidMessage,           /* ipcMessageText           */
                               INPUT "ERR":U).                /* ipcMessageType           */
      END. /*  IF AVAILABLE  btt_auth_coding*/


      FIND FIRST  btt_auth_detail
           WHERE  btt_auth_detail.auth_obj       = buf_auth.auth_obj
             AND (btt_auth_detail.start_date     < btt_auth.start_date
              OR (btt_auth_detail.start_date     > btt_auth.end_date AND btt_auth.end_date <> ?)
              OR (btt_auth_detail.end_date       > btt_auth.end_date AND btt_auth.end_date <> ?))
             AND  btt_auth_detail.record_action <> "DELETE":U NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      /*
        Check for details with start and end dates which dont fall within the auth start and end dates
      */
      IF AVAILABLE btt_auth_detail THEN
      DO:

        FIND FIRST buf_auth_detail NO-LOCK
             WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj  NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        ASSIGN cValidMessage = "Auth Detail lines linked to authorisation outside the auth period.":U
               cFieldChanged = IF      buf_auth.start_date          <> btt_auth.start_date
                               OR     (AVAILABLE buf_auth_detail
                                   AND buf_auth_detail.start_date   <> btt_auth_detail.start_date )  THEN "start_date":U
                               ELSE
                               IF      buf_auth.end_date            <> btt_auth.end_date
                               OR     (AVAILABLE buf_auth_detail
                                   AND buf_auth_detail.end_date     <> btt_auth_detail.end_date)     THEN "end_date":U
                               ELSE "":U .

        goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "":U,                  /* ipcOwningEntityKey       */
                               INPUT cFieldChanged ,        /* ipcFieldName             */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT cValidMessage,         /* ipcMessageText           */
                               INPUT "ERR":U).              /* ipcMessageType           */

      END. /* IF CAN-FIND(FIRST  btt_auth_detail  */

      /*
        Check for details with start and end dates which dont fall within the auth start and end dates
      */
      IF CAN-FIND(FIRST  btt_auth_mc_savings
                  WHERE  btt_auth_mc_savings.auth_obj       = buf_auth.auth_obj
                    AND (btt_auth_mc_savings.saving_date    < btt_auth.start_date
                     OR (btt_auth_mc_savings.saving_date    > btt_auth.end_date AND btt_auth.end_date <> ?)
                     OR (btt_auth_mc_savings.saving_date    > btt_auth.end_date AND btt_auth.end_date <> ?))
                    AND  btt_auth_mc_savings.record_action <> "DELETE":U) THEN
      DO:
        ASSIGN cValidMessage = "Auth savings linked to authorisation outside the auth period.":U.

        goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "":U,                  /* ipcOwningEntityKey       */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT cValidMessage,         /* ipcMessageText           */
                               INPUT "ERR":U).              /* ipcMessageType           */

      END. /* IF CAN-FIND(FIRST  btt_auth_mc_savings */
    END. /* IF AVAILABLE buf_auth THEN */
  END. /* FOR EACH btt_auth EXCLUSIVE-LOCK */
&ENDIF
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateInvalidSchemeOption) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateInvalidSchemeOption Procedure
PROCEDURE _validateInvalidSchemeOption :
/*------------------------------------------------------------------------------
  Purpose:     If the user doesnt' belong to a valid scheme option, this service
               will determine if the auth can continue or if the auth should be
               blocked according to the 'InvalidSchemeOption'-rule.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE PARAMETER    BUFFER    btt_auth      FOR tt_auth.
  DEFINE INPUT-OUTPUT PARAMETER oErrorObject  AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cRuleLoadStatus       AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE cBlockMessage         AS CHARACTER              NO-UNDO
         INITIAL "Authorisation can't be loaded. No valid scheme option for authorisation period.".
  DEFINE VARIABLE lSuccess              AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER              NO-UNDO.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                 INPUT  btt_auth.option_code,
                                                 INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                 INPUT  "InvalidSchemeOption":U,
                                                 INPUT  btt_auth.start_date,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  IF lValidRule
  THEN
    RUN _validateRuleSetup IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                 INPUT "InvalidSchemeOption":U,
                                                 INPUT cRuleValue,
                                                 INPUT FALSE,
                                                 INPUT cBlockMessage,
                                                 INPUT-OUTPUT oErrorObject).
  ELSE
    ASSIGN cBlockMessage = cBlockMessage + "[HELP=Auth Rule Code: InvalidSchemeOption]"
           lSuccess = oErrorObject:addError
                                 (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                  INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                  INPUT "":U,                  /* ipcOwningEntityKey       */
                                  INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                  INPUT cBlockMessage,         /* ipcMessageText           */
                                  INPUT "ERR":U).              /* ipcMessageType           */

  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateLOSUserCalcs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateLOSUserCalcs Procedure
PROCEDURE _validateLOSUserCalcs :
/*-------------------------------------------------------------------------------------
  Purpose   : Validate LOS User Calc changes
  Parameters:
  Notes     :
---------------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cArsRate          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cArsTrfType       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRuleValue        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValidMessage     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dNextDate         AS DATE      NO-UNDO.
  DEFINE VARIABLE dPrevDate         AS DATE      NO-UNDO.
  DEFINE VARIABLE iSeqNum           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lFoundLOSExcl     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lNextTime         AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lPrevTime         AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lValidRule        AS LOGICAL   NO-UNDO.

  DEFINE BUFFER btt_auth             FOR tt_auth.
  DEFINE BUFFER btt_auth_provider    FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail      FOR tt_auth_detail.

  AUTHBLOCK:
  FOR EACH btt_auth EXCLUSIVE-LOCK
     WHERE btt_auth.auth_obj > 0.00:

    /* Get Auth Rule to check for LOS values to be excluded from the Date Sequence validation */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                   INPUT  0,
                                                   INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                   INPUT  "UserCalcLOSDateSeqExcl":U,
                                                   INPUT  btt_auth.start_date,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).

    FOR EACH btt_auth_provider
       WHERE btt_auth_provider.auth_obj        = btt_auth.auth_obj
         AND btt_auth_provider.los_calculation = FALSE:
      ASSIGN iSeqNum = 1.

      /*  Check all detail records. If ARS&LOS combination does NOT match rule setup, do date sequence validation and determine sequence */
      FOR EACH btt_auth_detail
         WHERE btt_auth_detail.auth_obj          = btt_auth.auth_obj
           AND btt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj
           AND btt_auth_detail.quantity_los     <> 0
            BY btt_auth_detail.start_date
            BY btt_auth_detail.start_ampm DESCENDING:

        ASSIGN cArsRate    = IF btt_auth_provider.override_ars_rate <> "":U
                             THEN btt_auth_provider.override_ars_rate
                             ELSE btt_auth_provider.default_ars_rate
               cArsTrfType = cArsRate + ",":U + btt_auth_detail.loc_value.

        IF lValidRule
        AND LOOKUP(cArsTrfType, TRIM(cRuleValue), "|") > 0
        THEN ASSIGN lFoundLOSExcl = TRUE.
        ELSE ASSIGN lFoundLOSExcl = FALSE.

        IF NOT lFoundLOSExcl THEN
        DO:
          IF iSeqNum > 1 THEN
          DO:
            ASSIGN lNextTime = NOT lPrevTime
                   dNextDate = IF NOT lPrevTime
                               THEN dPrevDate + 1
                               ELSE dPrevDate.

            IF   btt_auth_detail.start_date <> dNextDate
            OR  (btt_auth_detail.start_date =  dNextDate
            AND  btt_auth_detail.start_ampm <> lNextTime) THEN
            DO:
              ASSIGN cValidMessage = "User Calculated LOC line Start Date or AM/PM does not follow on previous line for Tariff " +
                                      btt_auth_detail.owning_alt_value + " LOC Seq " + STRING(btt_auth_detail.loc_sequence,">>9") +
                                     "[HELP=Auth Rule Code: UserCalcLOSDateSeqExcl]":U.

              goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic  */
                                     INPUT btt_auth_detail.auth_detail_obj,                      /* ipdOwningEntityObj       */
                                     INPUT "":U,                                                 /* ipcOwningEntityKey       */
                                     INPUT btt_auth_detail.line_number,                          /* ipiLineNumber            */
                                     INPUT cValidMessage,                                        /* ipcMessageText           */
                                     INPUT "ERR":U).                                             /* ipcMessageType           */
            END. /* IF btt_auth_detail.start_date <> dNextDate AND */
          END. /* IF iSeqNum > 1 */

          IF iSeqNum <> btt_auth_detail.loc_sequence
          THEN
            ASSIGN btt_auth_detail.loc_sequence  = iSeqNum
                   btt_auth_detail.record_action = "MODIFY":U.

          ASSIGN dPrevDate  = btt_auth_detail.end_date
                 lPrevTime  = btt_auth_detail.end_ampm
                 iSeqNum    = iSeqNum + 1.

        END.  /* IF NOT lFoundLOSExcl */
      END. /* FOR EACH btt_auth_detail */

      /*
        Check all detail records again. If ARS&LOS combination matches the rule setup,
        exclude from the date sequence validation and calculate sequence to be last.
        This is done after all the other records' sequences have been determined,
        so that we know what the last seq no will be.
      */
      FOR EACH btt_auth_detail
         WHERE btt_auth_detail.auth_obj          = btt_auth.auth_obj
           AND btt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj
           AND btt_auth_detail.quantity_los     <> 0
            BY btt_auth_detail.start_date
            BY btt_auth_detail.start_ampm DESCENDING:

        ASSIGN cArsRate    = IF btt_auth_provider.override_ars_rate <> "":U
                             THEN btt_auth_provider.override_ars_rate
                             ELSE btt_auth_provider.default_ars_rate
               cArsTrfType = cArsRate + ",":U + btt_auth_detail.loc_value.

        IF lValidRule
        AND LOOKUP(cArsTrfType, TRIM(cRuleValue), "|") > 0
        THEN ASSIGN lFoundLOSExcl = TRUE.
        ELSE ASSIGN lFoundLOSExcl = FALSE.

        IF lFoundLOSExcl
        AND btt_auth_detail.record_action <> "DELETE":U
        THEN
          ASSIGN btt_auth_detail.loc_sequence  = iSeqNum
                 btt_auth_detail.record_action = "MODIFY":U
                 iSeqNum                       = iSeqNum + 1.

      END.  /* FOR EACH btt_auth_detail */
    END. /* FOR EACH btt_auth_provider */
  END. /* FOR EACH btt_auth EXCLUSIVE-LOCK */
&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateRuleSetup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateRuleSetup Procedure
PROCEDURE _validateRuleSetup :
/*------------------------------------------------------------------------------
  Purpose:     If the rule value can be set up as "Block", "Load" or "Load|9,XXX"
               then this procedure will process the rule value.
  Parameters:  iplWarnOnly -> Send "true" through in this parameter if we want to give
                  a warning to the user that this auth may potentially be rejected because
                  it falls in the dependant's waiting period. The block/load will only be
                  done on final save.
  Notes:      If the rule value is:
              "Block" -> An error is generated with error message = ipcBlockMessage.
              "Load" -> The auth status will be assigned as 6 (Declined) and no
                        auth status note.
              "Load|9,XXX", where '9' is the new auth status and 'XXX'is then new
                            auth status note. The status is validated and if valid,
                            will be assigned to the auth status and the auth status
                            note will be updated to the new status note.
------------------------------------------------------------------------------*/
  DEFINE              PARAMETER BUFFER btt_auth          FOR tt_auth.
  DEFINE INPUT        PARAMETER        ipcRuleCode       AS CHARACTER         NO-UNDO.
  DEFINE INPUT        PARAMETER        ipcRuleValue      AS CHARACTER         NO-UNDO.
  DEFINE INPUT        PARAMETER        iplWarnOnly       AS LOGICAL           NO-UNDO.
  DEFINE INPUT        PARAMETER        ipcErrorMessage   AS CHARACTER         NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER        oErrorObject      AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cRuleLoadStatus AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cInvalidMessage AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAuthStatus     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lAcknowledge    AS LOGICAL     NO-UNDO.

  IF iplWarnOnly
  THEN
    ASSIGN cRuleValue      = "WARN":U
           ipcErrorMessage = "Authorisation may be in a waiting period.".
  ELSE
    ASSIGN cRuleValue = ENTRY(1,ipcRuleValue,"|":U).

  CASE cRuleValue:
    WHEN "BLOCK":U THEN
    DO:
      ASSIGN ipcErrorMessage = ipcErrorMessage + "[HELP=Auth Rule Code: ":U + ipcRuleCode + "]":U
             lSuccess = oErrorObject:addError
                                   (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                    INPUT "":U,                  /* ipcOwningEntityKey       */
                                    INPUT "start_date":U,        /* ipcFieldName             */
                                    INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                    INPUT ipcErrorMessage,       /* ipcMessageText           */
                                    INPUT "ERR":U).              /* ipcMessageType           */
      RETURN.
    END.  /* WHEN "BLOCK" */

    WHEN "LOAD":U THEN
    DO:

      IF NUM-ENTRIES(ipcRuleValue,"|":U) > 1 THEN
      DO:
        EMPTY TEMP-TABLE ttAuthStatus.

        ASSIGN cRuleLoadStatus = ENTRY(2,ipcRuleValue,"|":U)
               iAuthStatus     = IF cRuleLoadStatus = ""
                                 THEN 6
                                 ELSE INTEGER(ENTRY(1,cRuleLoadStatus)) NO-ERROR.
        IF ERROR-STATUS:ERROR
        THEN
          ASSIGN cInvalidMessage = "Invalid status specified on Auth Rule setup. Please check." +
                                   "[HELP=Auth Rule Code: ":U + ipcRuleCode + "]":U
                 lSuccess = oErrorObject:addError
                              (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "":U,                  /* ipcOwningEntityKey       */
                               INPUT "start_date":U,        /* ipcFieldName             */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT cInvalidMessage,       /* ipcMessageText           */
                               INPUT "ERR":U).              /* ipcMessageType           */

        ASSIGN btt_auth.auth_status      = iAuthStatus
               btt_auth.auth_status_note = IF NUM-ENTRIES(cRuleLoadStatus) > 1
                                           THEN ENTRY(2,cRuleLoadStatus)
                                           ELSE "".
      END.  /* IF NUM-ENTRIES(ipcRuleValue,"|":U) > 1 THEN */
      ELSE ASSIGN btt_auth.auth_status      = 6 /* Declined */
                  btt_auth.auth_status_note = "":U.
    END.  /* WHEN "LOAD" */

    WHEN "WARN":U OR
    WHEN "WARNACK":U THEN
      ASSIGN lAcknowledge = IF cRuleValue = "WarnAck":U
                            THEN TRUE
                            ELSE FALSE
             ipcErrorMessage = ipcErrorMessage + "[HELP=Auth Rule Code: ":U + ipcRuleCode + "]":U
             lSuccess = oErrorObject:addError
                              (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "":U,                  /* ipcOwningEntityKey       */
                               INPUT "start_date":U,        /* ipcFieldName             */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT ipcErrorMessage,       /* ipcMessageText           */
                               INPUT "WAR":U,               /* ipcMessageType           */
                               INPUT lAcknowledge).         /* iplAcknowledge           */

    OTHERWISE DO:
      ASSIGN cInvalidMessage = "Invalid '" + ipcRuleCode + "' rule setup."
             lSuccess = oErrorObject:addError
                              (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "":U,                  /* ipcOwningEntityKey       */
                               INPUT "start_date":U,        /* ipcFieldName             */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT cInvalidMessage,       /* ipcMessageText           */
                               INPUT "ERR":U).              /* ipcMessageType           */
    END.  /* OTHERWISE DO */
  END CASE.

 { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_valUpdateStatusReason) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _valUpdateStatusReason Procedure
PROCEDURE _valUpdateStatusReason :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipdInsurerObj      AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipiOptionCode      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipdStartDate       AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER ipiAuthStatus      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcAuthStatusNote  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplUpdateReason    AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcStatusNote      AS CHARACTER NO-UNDO.

DEFINE VARIABLE lValidRule    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cRuleValue    AS CHARACTER   NO-UNDO.

IF ipiAuthStatus = 1 /* Authorised */ THEN
DO:
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeLimits":U,
                                                 INPUT  "ExceedLimitStatusReason":U,
                                                 INPUT  ipdStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  IF lValidRule
  AND ipcAuthStatusNote = cRulevalue
  THEN
    ASSIGN oplUpdateReason = TRUE
           opcStatusNote   = "".
  ELSE
    ASSIGN oplUpdateReason = TRUE
           opcStatusNote   = ipcAuthStatusNote.
END. // IF ipiAuthStatus = 1
ELSE
  ASSIGN oplUpdateReason = TRUE
         opcStatusNote   = ipcAuthStatusNote.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnAddError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnAddError Procedure
FUNCTION fnAddError RETURNS LOGICAL
  ( ipdObj AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
  ------------------------------------------------------------------------------ */

  FIND FIRST tt_parent_error NO-LOCK
       WHERE tt_parent_error.owning_obj = ipdObj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE tt_parent_error THEN
  DO:
    CREATE tt_parent_error.
    ASSIGN tt_parent_error.owning_obj = ipdObj.

    VALIDATE tt_parent_error.
  END. /*IF NOT AVAILABLE tt_parent_error THEN*/

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnAllowLineStatusChange) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnAllowLineStatusChange Procedure
FUNCTION fnAllowLineStatusChange RETURNS LOGICAL
  ( ipiOldAuthHeaderStatus   AS INTEGER,
    ipcOldAuthHeaderNote     AS CHARACTER,
    ipiNewAuthHeaderStatus   AS INTEGER,
    ipcNewAuthHeaderNote     AS CHARACTER,
    ipiAuthLineStatus        AS INTEGER,
    ipcAuthLineNote          AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  Check if the status on an auth line may be updated.
    Notes:
------------------------------------------------------------------------------*/

  /*
     If the line's status is currently 5-Cancelled,
     we don't want to change the line's status
  */
  IF LOOKUP(STRING(ipiAuthLineStatus),"5") > 0
  THEN
    RETURN FALSE.

  /*
     If the line's status is already whatever the new status will be,
     there is no need to change the line's status.
  */
  IF ipiNewAuthHeaderStatus = ipiAuthLineStatus
  THEN
    RETURN FALSE.

  /*
     If the auth header status is changed to 6-Declined, we want to
     decline all the lines no matter what their current status is.
  */
  IF ipiNewAuthHeaderStatus = 6 THEN
  DO:
    IF  ipiAuthLineStatus = 6
    AND ipcAuthLineNote <> ipcNewAuthHeaderNote
    THEN
      RETURN FALSE.
    ELSE
      RETURN TRUE.
  END. /* IF ipiNewAuthHeaderStatus = 6  */

  /*
     If the line's current status is the same as the header's old status,
     but the status reason's are different, we don't want to change the
     line's status.
  */
  IF   LOOKUP(STRING(ipiAuthLineStatus),"0,6") > 0
  AND (ipiAuthLineStatus <> ipiOldAuthHeaderStatus
  OR   ipcAuthLineNote   <> ipcOldAuthHeaderNote)
  THEN
    RETURN FALSE.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnClearErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnClearErrors Procedure
FUNCTION fnClearErrors RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
-------------------------------------------------------------------------------- */

  EMPTY TEMP-TABLE tt_parent_error.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnErrorOccurred) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnErrorOccurred Procedure
FUNCTION fnErrorOccurred RETURNS LOGICAL
  ( ipdObj AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
  ------------------------------------------------------------------------------*/

  RETURN CAN-FIND(FIRST tt_parent_error NO-LOCK
                  WHERE tt_parent_error.owning_obj = ipdObj).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnInvalidLineStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnInvalidLineStatus Procedure
FUNCTION fnInvalidLineStatus RETURNS CHARACTER
  ( INPUT ipdAuthObj AS DECIMAL,
    INPUT ipiStatus  AS INTEGER  ) :
/*------------------------------------------------------------------------------
  Purpose:  Check if there are any pending, cancelled, declined or
            requested primary/main lines
    Notes:  When we check status cancelle/declined, we're only concerned
            about the main/primary lines. If any of the other lines are
            cancelled/declined, it's ok. However, if any of the lines are pending
            or requested, we don't care if it the primary line or not, then we
            want to return an error.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cPendingLine AS CHARACTER NO-UNDO.

  DEFINE BUFFER bbt_auth_coding   FOR tt_auth_coding.
  DEFINE BUFFER bbt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER btt_auth          FOR tt_auth.
  DEFINE BUFFER bbt_auth_detail   FOR tt_auth_detail.

  /*
    Check if there are any provider lines for the status.
  */
  FOR EACH bbt_auth_provider NO-LOCK
    WHERE bbt_auth_provider.auth_obj = ipdAuthObj:

    IF bbt_auth_provider.auth_status = ipiStatus THEN
    DO:
      IF LOOKUP(STRING(ipiStatus),"0,7") <> 0 THEN
        ASSIGN cPendingLine = "Provider":U.
      ELSE IF bbt_auth_provider.main_provider THEN
        ASSIGN cPendingLine = "Provider":U.
    END.  /* IF bbt_auth_provider.auth_status = ipiStatus THEN */
  END. /* FOR EACH bbt_auth_provider NO-LOCK */

  /*
    Check if there are any provider detail lines for the status.
    These lines are only checked for status pending and requested.
  */
  IF LOOKUP(STRING(ipiStatus),"0,7") <> 0 THEN
    FOR EACH bbt_auth_detail NO-LOCK
      WHERE bbt_auth_detail.auth_obj = ipdAuthObj:

      IF bbt_auth_detail.auth_status = ipiStatus THEN
          ASSIGN cPendingLine = "Provider Detail":U.
    END.  /* FOR EACH bbt_auth_detail NO-LOCK */

  /*
    Check if there are any coding lines for the status.
  */
  FOR EACH bbt_auth_coding NO-LOCK
    WHERE bbt_auth_coding.auth_obj = ipdAuthObj:
    IF bbt_auth_coding.coding_status = ipiStatus THEN
    DO:
      IF LOOKUP(STRING(ipiStatus),"0,7") <> 0 THEN
        ASSIGN cPendingLine = "Coding":U.
      ELSE IF bbt_auth_coding.primary_code
           OR bbt_auth_coding.main_code THEN
        ASSIGN cPendingLine = "Coding":U.
    END.  /* IF bbt_auth_coding.coding_status = ipiStatus THEN */
  END.  /* FOR EACH bbt_auth_coding NO-LOCK */

  RETURN cPendingLine.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnBuildCC-CT-List) = 0 &THEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnBuildCC-CT-List Procedure
FUNCTION fnBuildCC-CT-List RETURNS CHARACTER
  ( BUFFER ttAuthTypeConfig FOR ttAuthTypeConfig ) :
/*------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cCC-CTList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cClaimCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cClaimType AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.
  BUILD-CC-CT-LIST:
  DO iCount = 1 TO NUM-ENTRIES(ttAuthTypeConfig.ClaimCodes) :
    ASSIGN cClaimCode = STRING(INTEGER(ENTRY(iCount, ttAuthTypeConfig.ClaimCodes))) 
           cClaimType = IF NUM-ENTRIES(ttAuthTypeConfig.ClaimTypes) >= iCount
                        THEN ENTRY(iCount, ttAuthTypeConfig.ClaimTypes) 
                        ELSE "Unknown":U
           cCC-CTList = cCC-CTList  + (IF cCC-CTList <> "":U THEN ",":U ELSE "":U)
                      + cClaimCode  + "-" + cClaimType .
  END. //BUILD-CC-CT-LIST:
  RETURN cCC-CTList.   /* Function return value. */
END FUNCTION.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ENDIF
