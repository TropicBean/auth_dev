&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Auth UI Service stack

    Author : MMP

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation }

{ ma/inc/maauthds.i }

{ ma/inc/maauthflagvalueds.i }
{ ma/inc/maauthtypeconfigtt.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow:
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB)
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 16.24
         WIDTH              = 74.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveFlagContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveFlagContainer Procedure
PROCEDURE ajaxSaveFlagContainer :
/*------------------------------------------------------------------------------
  Purpose   : Flag Value Container Ajax Validation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE oRequestHelper      AS cls.maajaxrequesthelper     NO-UNDO.
  DEFINE VARIABLE oResponseHelper     AS cls.maajaxresponsehelper    NO-UNDO.
  DEFINE VARIABLE oAuthFlagValue      AS cls.maauthflagvalue         NO-UNDO.
  DEFINE VARIABLE oAuthSearch         AS cls.maauthsearch            NO-UNDO.

  DEFINE VARIABLE dAuthObj            AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE dAuthRuleObj        AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE dFlagValueObj       AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE cAuthNum            AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cContainerCode      AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cFlagValue          AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cOEM                AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cOwningKey          AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cRecordAction       AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cRuleCode           AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cOverrideNote       AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE hErrorHandle        AS HANDLE                      NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL                     NO-UNDO.

  ASSIGN
     oRequestHelper     = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
     oResponseHelper    = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)

     cContainerCode     = ipcValidationArgument

     cRecordAction      =         oRequestHelper:getFieldValue("fcAction":U             + cContainerCode)
     dFlagValueObj      = DECIMAL(oRequestHelper:getFieldValue("fdFlagValueObj":U       + cContainerCode))
     cFlagValue         =         oRequestHelper:getFieldValue("cbFlagValue":U          + cContainerCode)
     dAuthRuleObj       = DECIMAL(oRequestHelper:getFieldValue("fdAuthRuleObj":U        + cContainerCode))
     cOEM               =         oRequestHelper:getFieldValue("fcOEMArgument":U        + cContainerCode)
     cOwningKey         =         oRequestHelper:getFieldValue("fcOwningKeyArgument":U  + cContainerCode)
     cOverrideNote      =        oRequestHelper:getFieldValue("fcOverrideNote":U       + cContainerCode)
     dAuthObj           = DECIMAL(oRequestHelper:getFieldValue("_authObjArgument":U     + cContainerCode))

     cRuleCode          =         oRequestHelper:getFieldValue("fcRuleCode":U           + cContainerCode)
     cAuthNum           =         oRequestHelper:getFieldValue("fcAuthNumArgument":U    + cContainerCode)

     oAuthFlagValue     = NEW cls.maauthflagvalue()
    NO-ERROR.

  IF NOT {&ErrorStatus} THEN
  DO:
    CASE cRecordAction:

      WHEN "modify":U THEN
      DO:
        oAuthFlagValue:focusAuthFlagValue(dFlagValueObj) NO-ERROR.

        ASSIGN
           oAuthFlagValue:OwningEntityMnemonic = "hatau":U
           oAuthFlagValue:OwningObj            = dAuthObj
           oAuthFlagValue:OwningKey            = "":U
           oAuthFlagValue:OwningAltValue       = cAuthNum
           oAuthFlagValue:AuthRuleObj          = dAuthRuleObj
           oAuthFlagValue:AuthFlagValue        = cFlagValue
           oAuthFlagValue:OverrideNote         = cOverrideNote
           oAuthFlagValue:LastChangeDateTime   = NOW
         NO-ERROR.

        ASSIGN lSuccess = oAuthFlagValue:saveAuthFlagValue() NO-ERROR.

        IF NOT {&ErrorStatus} AND NOT oAuthFlagValue:ErrorObject:ErrorsExist THEN
        DO:
          ASSIGN
            dFlagValueObj                   = oAuthFlagValue:AuthFlagValueObj
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U
            lSuccess                        = oResponseHelper:addFieldValue("fdFlagValueObj":U + cContainerCode, STRING(oAuthFlagValue:AuthFlagValueObj))
           NO-ERROR.

        END. /*IF NOT {&ErrorStatus} AND NOT oAuthorisation:ErrorObject:ErrorsExist THEN*/
      END. /* WHEN "modify":U THEN */

      WHEN "delete":U THEN
      DO:
        oAuthFlagValue:focusAuthFlagValue(dFlagValueObj) NO-ERROR.

        IF NOT {&ErrorStatus} AND NOT oAuthFlagValue:AuthFlagValueInFocus
        THEN
          ASSIGN
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U
          NO-ERROR.
        ELSE
          ASSIGN lSuccess = oAuthFlagValue:removeAuthFlagValue() NO-ERROR.

        IF NOT {&ErrorStatus} AND NOT oAuthFlagValue:ErrorObject:ErrorsExist THEN
        DO:
          ASSIGN
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully removed":U
            oResponseHelper:ReturnValue     = "Record successfully removed":U
           NO-ERROR.
        END. /*IF NOT {&ErrorStatus} AND NOT oAuthFlagValue:ErrorObject:ErrorsExist THEN*/
      END. /* WHEN "delete":U THEN */

      OTHERWISE
      DO:
        ASSIGN
          oResponseHelper:RequestValid    = FALSE
          oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported":U, cRecordAction)
          oResponseHelper:ResponseMessage = "Unable to perform action":U
         NO-ERROR.
      END. /* OTHERWISE */
    END CASE.
  END. /*IF NOT {&ErrorStatus} THEN*/

  IF NOT {&ErrorStatus} AND NOT oAuthFlagValue:ErrorObject:ErrorsExist AND CAN-DO("modify,delete", cRecordAction ) THEN
  DO:

    /*
      By this point the auth flag value record would have saved succesfully without any validation errors. So now we need to call
      the saveAuthorisation service , because a flag record could possibly trigger a rate change . So now we can rely on the saveAuthorisation
      service to handle the conversions of auth detail lines
    */

    DATASET dsAuthorisation:EMPTY-DATASET().

    ASSIGN oAuthSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE)
           lSuccess    = oAuthSearch:setCriteria("bufferlist", "tt_auth")
           lSuccess    = oAuthSearch:setFilterCriteria("tt_auth.auth_obj", "=":U , dAuthObj ).

    oAuthSearch:fetchData().

     FIND FIRST tt_auth_flag_value
          WHERE tt_auth_flag_value.auth_flag_value_obj = dFlagValueObj NO-ERROR.

     {&ResetError}

     IF NOT AVAILABLE tt_auth_flag_value
     THEN
       CREATE tt_auth_flag_value.

     ASSIGN
       tt_auth_flag_value.record_action          = cRecordAction
       tt_auth_flag_value.auth_flag_value_obj    = oAuthFlagValue:AuthFlagValueObj
       tt_auth_flag_value.auth_flag_value        = oAuthFlagValue:AuthFlagValue
       tt_auth_flag_value.last_change_datetime   = oAuthFlagValue:LastChangeDateTime
       tt_auth_flag_value.owning_alt_value       = oAuthFlagValue:OwningAltValue
       tt_auth_flag_value.owning_entity_mnemonic = oAuthFlagValue:OwningEntityMnemonic
       tt_auth_flag_value.owning_key             = oAuthFlagValue:OwningKey
       tt_auth_flag_value.owning_obj             = oAuthFlagValue:OwningObj
       tt_auth_flag_value.auth_rule_obj          = oAuthFlagValue:AuthRuleObj .

    mipEnv:health:AuthBusinesslogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                      INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).
  END.

  IF {&ErrorStatus}
  OR oAuthFlagValue:ErrorObject:ErrorsExist
  OR oAuthFlagValue:ErrorObject:WarningsExist
  OR CAN-FIND(FIRST tt_auth_error ) THEN
  DO:

    /*
      If either an auth flag value error or saveAuthoristation error has been created. Make this an invalid response
    */
    IF oAuthFlagValue:ErrorObject:ErrorsExist OR CAN-FIND(FIRST tt_auth_error WHERE tt_auth_error.error_type = "ERR":U)
    THEN
      ASSIGN oResponseHelper:RequestValid  = FALSE .

    /*
      Errors will be mapped with the error temp table linked to oAuthFlagValue error object. So here we need to append all the errors
      we have in our tt_auth_error handle to the ones in the auth flag value error tt handle .
    */

    ASSIGN hErrorHandle = oAuthFlagValue:ErrorObject:getErrorTableHandle().

    hErrorHandle:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_error:HANDLE ).

    ASSIGN
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)

      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U)
      lSuccess                        = oResponseHelper:setError(hErrorHandle).

    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }
  END.   /*IF oAuthFlagValue:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/

  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthSearch)     THEN DELETE OBJECT oAuthSearch     NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthFlagValue)  THEN DELETE OBJECT oAuthFlagValue  NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidationFlagValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationFlagValue Procedure
PROCEDURE ajaxValidationFlagValue :
/*------------------------------------------------------------------------------
  Purpose   : Auth Flag Value Container Ajax Validation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE lSuccess             AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE cFilterFieldList     AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterField         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterValueList     AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterValue         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFlagValue           AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFlagValueList       AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnFieldList     AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnField         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnValues        AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cRuleCode            AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cRuleType            AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE dEffectiveDate       AS DATE                     NO-UNDO.
  DEFINE VARIABLE dAuthObj             AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE iCnt                 AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iFilterField         AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iReturnField         AS INTEGER                  NO-UNDO.

  DEFINE VARIABLE oAuthRule            AS cls.maauthrule           NO-UNDO.
  DEFINE VARIABLE oAuthSearch          AS cls.maauthsearch         NO-UNDO.

  &SCOPED-DEFINE AppendReturnValues ASSIGN cReturnValues = cReturnValues + (IF cReturnValues = "":U THEN "":U ELSE "|":U) +

  &SCOPED-DEFINE ValidationSuccess  ASSIGN ttValidation.cReturnValues = cReturnValues ~
                                           ttValidation.lValid        = TRUE          ~
                                           ttValidation.cMessage      = "Success":U.  ~
                                                                                      ~
                                    VALIDATE ttValidation.

  &SCOPED-DEFINE BlankResponse      ASSIGN ttValidation.cReturnValues = FILL("|":U, NUM-ENTRIES(cReturnFieldList) - 1) ~
                                           ttValidation.lValid        = TRUE.                                          ~
                                                                                                                       ~
                                    VALIDATE ttValidation.

  ASSIGN
    cFilterFieldList = get-value('FldLst')
    cFilterValueList = get-value('ValList')
    cReturnFieldList = get-value('RetFldList')
    oAuthSearch      = NEW cls.maauthsearch().

  CREATE ttValidation.

  DO iFilterField = 1 TO NUM-ENTRIES(cFilterFieldList):

    ASSIGN cFilterField = ENTRY(iFilterField, cFilterFieldList)
           cFilterValue = ENTRY(iFilterField, cFilterValueList).

    CASE cFilterField:

      WHEN "[AuthObj]":U
      THEN ASSIGN dAuthObj = DECIMAL(TRIM(cFilterValue)).

      WHEN "[RuleCode]":U
      THEN ASSIGN cRuleCode = TRIM(cFilterValue).

      WHEN "[RuleType]":U
      THEN ASSIGN cRuleType = TRIM(cFilterValue).

      WHEN "[FlagValue]":U
      THEN ASSIGN cFlagValue = TRIM(cFilterValue).

      WHEN "[EffectiveDate]":U
      THEN ASSIGN dEffectiveDate = DATE(TRIM(cFilterValue)).

    END CASE.

  END. /*DO iFilter = 1 TO NUM-ENTRIES(cFilterFields):*/

  CASE ipcValidationArgument:

    WHEN "[AuthObj]":U THEN
    DO:
      oAuthSearch:SetCriteria("BufferList":U , "tt_auth":U).
      oAuthSearch:SetCriteria("DataSecured":U, "FALSE":U).
      oAuthSearch:SetFilterCriteria("tt_auth.auth_obj":U, "=":U, dAuthObj).
      oAuthSearch:fetchData().

      FIND FIRST tt_auth NO-LOCK NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE tt_auth
      THEN CASE cReturnField:

        WHEN "[AuthNum]":U
        THEN {&AppendReturnValues} tt_auth.auth_num.

      END CASE.

    END.  /* WHEN "[AuthObj]":U THEN */

    WHEN "RuleCode":U THEN
    DO:
      IF cRuleCode <> "":U THEN
      DO:
        ASSIGN oAuthRule = NEW cls.maauthrule().

        oAuthRule:focusAuthRule(INPUT 0.00,
                                INPUT 0,
                                INPUT cRuleType,
                                INPUT cRuleCode,
                                INPUT dEffectiveDate).

        IF oAuthRule:AuthRuleInFocus THEN
        DO:
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):

            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

            CASE cReturnField:
              WHEN "[RuleCode]":U
              THEN {&AppendReturnValues} oAuthRule:RuleCode.

              WHEN "[RuleDescription]":U
              THEN {&AppendReturnValues} oAuthRule:RuleDescription.

              WHEN "[RuleValidValues]":U
              THEN DO:
                 ASSIGN cFlagValueList = REPLACE(oAuthRule:RuleValidValues,"|":U,",":U).

                 {&AppendReturnValues} cFlagValueList.
              END.

              WHEN "[RuleObj]":U
              THEN {&AppendReturnValues} STRING(oAuthRule:AuthRuleObj).

            END CASE.
          END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */

          {&ValidationSuccess}
        END. /* IF oAuthRule:AuthRuleInFocus THEN */
        ELSE DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("Rule '&1' not found":U, cRuleCode).

          VALIDATE ttValidation.
        END. /*ELSE: IF oAuthRule:AuthRuleInFocus*/
      END. /*IF cRuleCode <> "":U THEN */
      ELSE
      DO:
        {&BlankResponse}
      END. /*ELSE*/
    END. /*WHEN "RuleCode":U THEN*/

    WHEN "FlagValue":U THEN
    DO:
      IF cFlagValue <> "":U THEN
      DO:
        ASSIGN oAuthRule = NEW cls.maauthrule().

        oAuthRule:focusAuthRule(INPUT 0.00,
                                INPUT 0,
                                INPUT cRuleType,
                                INPUT cRuleCode,
                                INPUT dEffectiveDate).

        IF oAuthRule:AuthRuleInFocus
        AND LOOKUP(cFlagValue,oAuthRule:ruleValidValues,"|":U) = 0
        THEN
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("Flag Value '&1' not listed in rule valid values '&2'":U, cFlagValue, oAuthRule:ruleValidValues).

          VALIDATE ttValidation.
        END. /*IF oAuthRule:AuthRuleInFocus...*/
      END. /*IF cFlagValue <> "":U THEN */
      ELSE
      DO:
        {&BlankResponse}
      END. /*ELSE: IF cFlagValue <> "":U*/
    END. /*WHEN "FlagValue":U THEN*/

  END CASE.

  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oAuthRule)  THEN DELETE OBJECT oAuthRule." }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthFlag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthFlag Procedure
PROCEDURE getCntUpdAuthFlag :
/*------------------------------------------------------------------------------
  Purpose   : Update flag container definition
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oWob                 AS cls.mipwswob              NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  ASSIGN
    oWob                                  = Warpspeed:CurrentObject

    opoContainer                          = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle           = "Flag Information":U
    opoContainer:ViewOnly                 = FALSE
    opoContainer:NoDataMessage            = "Please specify the flag information in the empty line provided above":U
    opoContainer:ShowContainerSettings    = FALSE
    opoContainer:Collapsable              = TRUE
    opoContainer:RowRenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle
    opoContainer:RowRenderProcedure       = "RowRenderProcedure":U
    opoContainer:RowRenderArgument        = "AuthFlagContainer":U

    opoContainer:ContainerMode            = Warpspeed:SubmitValue
    opoContainer:QueryString              = "FOR EACH tt_auth_flag_value NO-LOCK, ":U +
                                                "FIRST tt_auth_flag_value_rule NO-LOCK ":U +
                                                "WHERE tt_auth_flag_value_rule.auth_rule_obj = tt_auth_flag_value.auth_rule_obj OUTER-JOIN , ":U +
                                                "FIRST tt_auth NO-LOCK ":U +
                                                "WHERE tt_auth.auth_obj = tt_auth_flag_value.owning_obj ":U +
                                                "BY tt_auth_flag_value.auth_flag_value_obj DESCENDING":U

    oControl                           = opoContainer:addControl("fiLineNumber":U       + ipcContainername, "wsInput":U, "25":U, "tt_auth_flag_value.line_number":U, "INTEGER":U  ,  1, "#":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "LineNumber":U
    oControl:ControlToken              = "Hidden":U

    oControl                           = opoContainer:addControl("fdFlagValueObj":U      + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value.auth_flag_value_obj":U,    "CHARACTER":U  ,  2, "":U)
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("fdAuthRuleObj":U       + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value.auth_rule_obj":U,          "DECIMAL":U  ,    3, "":U)
    oControl:ControlValue              = ?
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("fcOEMArgument":U       + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value.owning_entity_mnemonic":U, "CHARACTER":U,    4, "":U)
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("_authObjArgument":U    + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value.owning_obj":U, "DECIMAL":U  ,                5, "":U)
    oControl:ControlToken              = "Hidden":U
    oControl:AjaxValidation            =  "SERVICE:maUIService:ajaxValidationFlagValue:AuthObj":U
    oControl:FilterFields              = "[AuthObj]"
    oControl:FilterControls            = "_authObjArgument":U + ipcContainername
    oControl:ReturnFields              = "[AuthNum]":U
    oControl:ReturnControls            = "fcAuthNumArgument":U  + ipcContainername
    oControl                           = opoContainer:addControl("fcOwningKeyArgument":U + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value.owning_key":U,       "CHARACTER":U,  6, "":U)
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("fcAuthNumArgument":U   + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value.owning_alt_value":U, "CHARACTER":U,  7, "":U)
    oControl:ControlToken              = "Hidden":U

    oControl                           = opoContainer:addControl("fcRuleCode":U          + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value_rule.rule_code":U  , "CHARACTER":U,  8, "Flag":U)
    oControl:JavaScriptOnFocus         = " if (this.value==~"~")~{var cv = this.name.replace(~"fcRuleCode" + ipcContainerName + "~",~"~"); rebuildSelect(fnGetControls(~"cbFlagValue" + ipcContainername + "~"+cv)[0],~"~");~}"
    oControl:ControlClass              = "+clMan":U
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationFlagValue:RuleCode":U
    oControl:FilterFields              = "[RuleCode],[RuleType],[EffectiveDate]"
    oControl:FilterControls            = "fcRuleCode":U + ipcContainername + ",fcRuleTypeArgument":U + ipcContainername + ",fdLastChangeArgument":U + ipcContainername
    oControl:ReturnFields              = "[RuleObj],[RuleCode],[RuleDescription],[RuleValidValues]":U
    oControl:ReturnControls            = "fdAuthRuleObj":U + ipcContainername + ",fcRuleCode":U + ipcContainername + ",fcRuleDescription":U + ipcContainername + ",fcRuleValidValues":U + ipcContainername

    oControl                           = opoContainer:addControl("fcRuleTypeArgument":U  + ipcContainername, "wsInput":U        , "":U  , "":U  , "CHARACTER":U,  8, "Flag":U)
    oControl:ControlValue              = "ma_acAuthRuleTypeAuthFlag":U
    oControl:ControlToken              = "Hidden":U

    oControl                           = opoContainer:addControl("buRuleBtn":U           + ipcContainerName, "wsLookupButton":U , "":U  , "":U  , "":U         ,  8, "Flag":U)
    oControl:CellLayoutMask            = "&1&2&3":U
    oControl:LookupWobFLA              = "hacarf":U
    oControl:LookupFields              = "hac_auth_rule.auth_rule_obj":U
    oControl:LookupControls            = "fdAuthRuleObj":U + ipcContainerName
    oControl:FilterFields              = "hac_auth_rule.rule_code":U
    oControl:FilterControls            = "fcRuleCode":U + ipcContainerName
    oControl:ReturnFields              = "hac_auth_rule.auth_rule_obj,hac_auth_rule.rule_code,hac_auth_rule.rule_description,hac_auth_rule.rule_valid_values":U
    oControl:ReturnControls            = "fdAuthRuleObj":U + ipcContainername + ",fcRuleCode":U + ipcContainername + ",fcRuleDescription":U + ipcContainername + ",fcRuleValidValues":U + ipcContainername

    oControl                           = opoContainer:addControl("fcRuleDescription":U  + ipcContainername, "wsTextArea":U, "30":U  , "tt_auth_flag_value_rule.rule_description":U , "CHARACTER":U,  9, "Description":U)
    oControl:ControlToken              = "Disabled":U

    oControl                           = opoContainer:addControl("fcRuleValidValues":U  + ipcContainername, "wsInput":U   , "":U    , "tt_auth_flag_value_rule.rule_valid_values":U, "CHARACTER":U, 10, "Value":U)
    oControl:ControlToken              = "Hidden":U
    oControl:JavascriptOnChange        = "this.value=this.value.replace(/,/g,~"|~");var cv=parseInt(this.name.replace(~"fcRuleValidValues" + ipcContainername + "~",~"~")); rebuildSelect(fnGetControls(~"cbFlagValue" + ipcContainername + "~"+cv)[0],this.value);":U
    oControl                           = opoContainer:addControl("cbFlagValue":U        + ipcContainername, "wsCombo":U, "":U       , "tt_auth_flag_value.auth_flag_value":U ,       "CHARACTER":U, 10, "Value":U)
    oControl:CellLayoutMask            = "&1&2":U
    oControl:ControlClass              = "+clMan":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "FlagValue":U
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationFlagValue:FlagValue":U
    oControl:ControlJavascript         = "style='width:128px'":U
    oControl:ControlTooltip            = "Please select a value for the flag":U

    oControl                           = opoContainer:addControl("fcOverrideNote":U        + ipcContainerName, "wsInput":U       , "10":U  , "tt_auth_flag_value.override_note":U , "INTEGER":U  , 11, "":U)
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationProvider:OverrideNote":U
    oControl:FilterFields              = "[ReasonCode],[ReasonType]":U
    oControl:FilterControls            = "fcCopayOverrideNote":U + ipcContainerName + ",":U
                                       + "fcOverrideNoteTypeArgument":U + ipcContainerName
    oControl:ReturnFields              = "[ReasonDesc]":U
    oControl:ReturnControls            = "fcOverrideNoteDesc":U + ipcContainerName
    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "FlagOverrideNote":U
    oControl:ControlToken              = "Disabled":U

    oControl                           = opoContainer:addControl("fcOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U  , "5":U  , "":U                                  , "CHARACTER":U, 11, "":U)
    oControl:ControlToken              = "Hidden":U
    oControl:ControlValue              = "AP":U
    oControl:ControlClass              = "+clPreserveValue":U

    oControl                           = opoContainer:addControl("fcOverrideNoteDesc":U         + ipcContainerName, "wsInput":U  , "5":U  , "":U                                  , "CHARACTER":U, 11, "":U)
    oControl:ControlToken              = "Hidden":U

    /* Set copay override note description as tooltip when note is selected client side */
    oControl:JavascriptOnChange        = "fnSetReasonDescription(this, ~"fcOverrideNote":U + ipcContainerName + "~", ~"buOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U

    oControl                           = opoContainer:addControl("buOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                             , "":U         , 11, "Override<br> Reason":U)
    oControl:ControlTooltip            = "Lookup on Override Reasons.":U
    oControl:CellLayoutMask            = "&1&2&3&4":U
    oControl:LookupWobFLA              = "note":U
    oControl:LookupFields              = "note.key":U
    oControl:LookupControls            = "fcOverrideNote":U + ipcContainerName
    oControl:FilterFields              = "note.key,note.type":U
    oControl:FilterControls            = "fcOverrideNote":U + ipcContainerName + ",":U
                                       + "fcOverrideNoteTypeArgument":U + ipcContainerName
    oControl:ReturnFields              = "note.key":U
    oControl:ReturnControls            = "fcOverrideNote":U + ipcContainerName
    oControl:ControlToken              = "Hidden":U


    oControl                           = opoContainer:addControl("fdLastChangeArgument":U       + ipcContainername, "wsInput":U, "":U  , "tt_auth_flag_value.last_change_datetime":U, "DATETIME":U , 12, "Last Changed":U)
    oControl:ControlToken              = "ReadOnly":U
    .

  ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(opoContainer)

         oContainerProperties:AutoSaveOperation      = "SERVICE:maUIService:ajaxSave":U + opoContainer:ContainerCode + ":":U + opoContainer:ContainerCode
         oContainerProperties:NumberVisibleControls  = 99.


  mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties).


&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareAuthFlagValueCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareAuthFlagValueCombo Procedure
PROCEDURE prepareAuthFlagValueCombo :
/*------------------------------------------------------------------------------
  Purpose   : Builds auth flag value combo options
  Parameters: Control object
              Insurer Obj
              Option Code
              Rule Code
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER iopoControl    AS cls.mipwscontrol NO-UNDO.
  DEFINE INPUT        PARAMETER ipdInsurerObj  AS DECIMAL          NO-UNDO.
  DEFINE INPUT        PARAMETER ipiOptionCode  AS INTEGER          NO-UNDO.
  DEFINE INPUT        PARAMETER ipcRuleCode    AS CHARACTER        NO-UNDO.
  DEFINE INPUT        PARAMETER ipdStartDate   AS DATE             NO-UNDO.

  DEFINE VARIABLE cFlagValues      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValidValues AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iCnt             AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lValidRule       AS LOGICAL     NO-UNDO.

  mipEnv:Health:AuthMaintenance:getAuthRuleValidValues
                              ( INPUT ipdInsurerObj,
                                INPUT ipiOptionCode,
                                INPUT  "ma_acAuthRuleTypeAuthFlag":U,
                                INPUT  ipcRuleCode,
                                INPUT  ipdStartDate,
                                OUTPUT lValidRule,
                                OUTPUT cRuleValue,
                                OUTPUT cRuleValidValues ).
  IF lValidRule THEN
  DO iCnt = 1 TO NUM-ENTRIES(cRuleValidValues,"|"):
    ASSIGN cFlagValues = cFlagValues
                       + (IF cFlagValues = "":U THEN  "":U ELSE "|":U)
                       + ENTRY(iCnt,cRuleValidValues,"|") + "=":U + ENTRY(iCnt,cRuleValidValues,"|").
  END.  /* DO iCnt = 1 TO NUM-ENTRIES(cRuleValidValues,"|"): */

  ASSIGN iopoControl:AdditionalItems = cFlagValues.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-renderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderProcedure Procedure
PROCEDURE renderProcedure :
/*------------------------------------------------------------------------------
  Purpose   : Auth ui service specific render procedure
  Parameters: Control to be rendered
  Notes     :
  Author    : MMP
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl    AS cls.mipwscontrol NO-UNDO.

  DEFINE VARIABLE cFlagRuleCode        AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cFlagRuleDesc        AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cFlagRuleValueValues AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE dFlagRuleObj         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE hBuffer              AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hQuery               AS HANDLE           NO-UNDO.
  DEFINE VARIABLE cOverrideNote        AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cNoteDescription     AS CHARACTER        NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).

  CASE ipoControl:RenderArgument:

    WHEN "FlagRuleDesc":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer       = hQuery:GET-BUFFER-HANDLE("tt_auth_flag_value_rule":U)
          cFlagRuleCode = hBuffer::rule_code
          cFlagRuleDesc = hBuffer::rule_description.

      ASSIGN ipoControl:ControlValue = IF cFlagRuleDesc = "":U THEN cFlagRuleCode ELSE cFlagRuleDesc.
             ipoControl:renderAsInput().
    END.  /* WHEN "FlagRuleDesc":U THEN */

    WHEN "FlagValue":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          dFlagRuleObj  = hQuery:GET-BUFFER-HANDLE("tt_auth_flag_value":U)::auth_rule_obj
          cFlagRuleCode = hQuery:GET-BUFFER-HANDLE("tt_auth_flag_value_rule":U)::rule_code.

      IF cFlagRuleCode <> "":U
      THEN DO:
        RUN prepareAuthFlagValueCombo IN TARGET-PROCEDURE (INPUT-OUTPUT ipoControl, INPUT 0.0, INPUT 0, INPUT cFlagRuleCode, INPUT TODAY).
      END.  /* IF dFlagRuleObj <> 0  */
      ELSE DO:
        ASSIGN
          ipoControl:AdditionalItems = "None=":U .
      END. /* ELSE DO: */

      ipoControl:renderAsComboOrSelect() .
    END. /* WHEN "FlagValue":U THEN */
    WHEN "FlagOverrideNote":U THEN
    DO:
      ASSIGN cOverrideNote = hQuery:GET-BUFFER-HANDLE("tt_auth_flag_value":U)::override_note.

      mipEnv:health:maUtility:getNoteDescription(INPUT cOverrideNote,
                                                 INPUT "AP":U ,
                                                 OUTPUT cNoteDescription) .

      ASSIGN ipoControl:ControlToolTip = IF cNoteDescription <> "":U THEN cNoteDescription ELSE "Please enter a Override Reason.":U.

      ipoControl:renderAsInput().

    END. //WHEN "FlagOverrideNote":U

    OTHERWISE RUN SUPER(ipoControl).
  END. /*CASE ipoControl:RenderArgument:      */

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rowRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowRenderProcedure Procedure
PROCEDURE rowRenderProcedure :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE dAuthTypeObj     AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dInsurerObj      AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE iOptionCode      AS INTEGER          NO-UNDO.
  DEFINE VARIABLE dStartDate       AS DATE             NO-UNDO.
  DEFINE VARIABLE hWob             AS HANDLE           NO-UNDO.
  DEFINE VARIABLE lValidRule       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE cRuleValue       AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cDisableFlags    AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE oRuleCode        AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oFlagValue       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oLkpBtn          AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oOverrideNote    AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oOverrideNoteBtn AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAction          AS cls.mipwscontrol NO-UNDO.

  /*
    RowRenderProcedure will be run in the current wob to attach
    any business logic validation errors generated during a submit
    to this container. RowRenderProcedure container code which should
    be applied to this container ragardless of which wob this container
    is used in should be placed in the case block below.
  */
  IF LOOKUP(ipoContainer:RowRenderArgument, "AuthFlagContainer":U) = 0 THEN
  DO:
    RUN SUPER(INPUT ipoContainer) NO-ERROR.

    /*
      The super may not have a rowRenderProcedure
    */
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6439' &ResetIgnoredErrors = TRUE }

  END. /*IF LOOKUP(ipoContainer:RenderArgument, "AuthFlagContainer":U) = 0 THEN*/
  ELSE
  DO:
    ASSIGN hWob = mipEnv:miProcedure:getProcedureHandle(INPUT "Wob_":U + WarpSpeed:CurrentWob).

    IF VALID-HANDLE(hWob)
    THEN RUN rowRenderProcedure IN hWob(INPUT ipoContainer) NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6456,PROGRESS:3234' &ResetIgnoredErrors = FALSE }

  END. /*ELSE*/

&IF {&DBDFMA} >= 010195 &THEN

  CASE ipoContainer:RowRenderArgument:

    WHEN "AuthFlagContainer":U THEN
    DO:

      ASSIGN dAuthTypeObj  = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_type_obj":U, "BUFFER-VALUE":U))
             dInsurerObj   = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.insurer_obj":U,   "BUFFER-VALUE":U))
             iOptionCode   = INTEGER(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.option_code":U,   "BUFFER-VALUE":U))
             dStartDate    =    DATE(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.start_date":U,    "BUFFER-VALUE":U)) .

      EMPTY TEMP-TABLE ttAuthTypeConfig NO-ERROR.

      mipEnv:Health:AuthService:GetAuthTypeConfig(INPUT dAuthTypeObj,
                                                  INPUT dInsurerObj,
                                                  INPUT iOptionCode,
                                                  INPUT dStartDate,
                                                  INPUT-OUTPUT TABLE ttAuthTypeConfig).

      ASSIGN oRuleCode        = ipoContainer:getControl("fcRuleCode"         + ipoContainer:ContainerCode)
             oFlagValue       = ipoContainer:getControl("cbFlagValue"        + ipoContainer:ContainerCode)
             oOverrideNote    = ipoContainer:getControl("fcOverrideNote"     + ipoContainer:ContainerCode)
             oOverrideNoteBtn = ipoContainer:getControl("buOverrideNoteBtn"  + ipoContainer:ContainerCode)
             oLkpBtn          = ipoContainer:getControl("buRuleBtn"          + ipoContainer:ContainerCode)
             oAction          = ipoContainer:getControl("buAction"           + ipoContainer:ContainerCode)
             .

      ASSIGN oRuleCode :ControlToken  = "Updatable":U
             oRuleCode :ControlClass  = "+clMan -clDisabled":U
             oFlagValue:ControlToken  = "Updatable":U
             oFlagValue:ControlClass  = "+clMan -clDisabled":U
             oLkpBtn   :ControlToken  = "Updatable":U
             oAction   :ControlToken  = "Updatable":U
             cDisableFlags            = "".

      FIND FIRST ttAuthTypeConfig EXCLUSIVE-LOCK NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'Progress:565' &ResetIgnoredErrors = TRUE }

      /*
        Enable/disable auth flag value controls depending
        on what is activated on the auth type.
      */
      IF AVAILABLE ttAuthTypeConfig THEN
      DO:
        IF ttAuthTypeConfig.ActivateCopayment AND ttAuthTypeConfig.ActivatePenalty
        THEN
          ASSIGN cDisableFlags = "EMERGENCY,COPAY,LATEAUTH,PENALTY":U .

        ELSE IF ttAuthTypeConfig.ActivateCopayment AND NOT ttAuthTypeConfig.ActivatePenalty
        THEN
          ASSIGN cDisableFlags = "EMERGENCY,COPAY":U .

        ELSE IF NOT ttAuthTypeConfig.ActivateCopayment AND ttAuthTypeConfig.ActivatePenalty
        THEN
          ASSIGN cDisableFlags = "EMERGENCY,LATEAUTH,PENALTY":U .
        ELSE
          ASSIGN cDisableFlags = "":U .

        IF LOOKUP(oRuleCode:ControlValue,cDisableFlags) <> 0
        THEN
          ASSIGN oRuleCode:ControlToken   = "Disabled":U
                 oFlagValue:ControlToken  = "Disabled":U
                 oLkpBtn:ControlToken     = "Disabled":U
                 oAction:ControlToken     = "Hidden":U
                 oRuleCode:ControlClass   = "-clMan +clDisabled":U
                 oFlagValue:ControlClass  = "-clMan +clDisabled":U.
      END. /*IF AVAILABLE ttAuthTypeConfig THEN*/
    END. /*WHEN "AuthFlagContainer":U THEN*/

  END CASE.

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

