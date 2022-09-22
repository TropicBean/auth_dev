&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Auth UI Service stack
    
    Author : MMP

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
USING Progress.Json.ObjectModel.*   FROM PROPATH .

CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation }

{ ma/inc/maauthds.i }
{ ma/inc/maauthflagvalueds.i}
{ ma/inc/maauthtypeds.i }

{ ma/inc/maauthtypeconfigtt.i }

DEFINE VARIABLE giOption                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE gcProviderList               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcSequenceList               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcNotePerAuthStatusRuleValue AS CHARACTER   NO-UNDO.
DEFINE VARIABLE glNotePerAuthStatusRule      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE glMandatory                  AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnPrepareStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnPrepareStatus Procedure 
FUNCTION fnPrepareStatus RETURNS CHARACTER
  ( ipcType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnSequenceList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnSequenceList Procedure 
FUNCTION fnSequenceList RETURNS CHARACTER
  (  ipdAuthObj AS DECIMAL  )  FORWARD.

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
         WIDTH              = 55.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveProviderContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveProviderContainer Procedure 
PROCEDURE ajaxSaveProviderContainer :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
  
  
  DEFINE VARIABLE oRequestHelper      AS cls.maajaxrequesthelper     NO-UNDO.
  DEFINE VARIABLE oResponseHelper     AS cls.maajaxresponsehelper    NO-UNDO.
  DEFINE VARIABLE oProvider           AS cls.maauthorisationprovider NO-UNDO.
  
  DEFINE VARIABLE cRecordAction              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDefaultArsRate            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDefaultBaseRate           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOverrideArsRate           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOverrideBaseRate          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cContainerCode             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cProviderType              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cClaimType                 AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cErrorMessage              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPrintDM                   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPayeeDM                   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReason                    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cAccountReference          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cProviderList              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCopayOverrideNote         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cEmergencyFlagValue        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPenaltyFlagValue          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPenaltyOverrideNote       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFlagValue                 AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iDiscipline                AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iSubDiscipline             AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAttDiscipline             AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAttSubDiscipline          AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStatus                    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iProviderNum               AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAttProviderNum            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iClaimCode                 AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dAuthObj                   AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj              AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dProviderObj               AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dQuantityAuth              AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dAmountAuth                AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dAmountRequest             AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dQuantityRequest           AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dStartDate                 AS DATE        NO-UNDO.
  DEFINE VARIABLE dEndDate                   AS DATE        NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lAuthorisedService         AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lAuthoriseAllServices      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lStartAmPm                 AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lEndAmPm                   AS LOGICAL     NO-UNDO. 
  DEFINE VARIABLE lMainProvider              AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lLosCalculation            AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lEmergencyFlagValueUpdated AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lPenaltyFlagValueUpdated   AS LOGICAL     NO-UNDO.

  ASSIGN
     oRequestHelper     = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
     oResponseHelper    = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
     
     cContainerCode     = ipcValidationArgument
     
     cRecordAction               =         oRequestHelper:getFieldValue("fcAction":U                     + cContainerCode)
     cProviderType               =         oRequestHelper:getFieldValue("cbProviderType":U               + cContainerCode)
     cClaimType                  =         oRequestHelper:getFieldValue("cbClaimType":U                  + cContainerCode)
     cPrintDM                    =         oRequestHelper:getFieldValue("cbPrintDM":U                    + cContainerCode)
     cPayeeDM                    =         oRequestHelper:getFieldValue("cbPayeeDM":U                    + cContainerCode)
     cOverrideBaseRate           =         oRequestHelper:getFieldValue("cbOverrideBaseRate":U           + cContainerCode)
     cOverrideArsRate            =         oRequestHelper:getFieldValue("cbOverrideArsRate":U            + cContainerCode)
     cDefaultArsRate             =         oRequestHelper:getFieldValue("fcProviderArsRate":U            + cContainerCode)
     cDefaultBaseRate            =         oRequestHelper:getFieldValue("fcProviderBaseRate":U           + cContainerCode)
     cReason                     =         oRequestHelper:getFieldValue("fcReason":U                     + cContainerCode)
     cAccountReference           =         oRequestHelper:getFieldValue("fcAccountReference":U           + cContainerCode)
     cCopayOverrideNote          =         oRequestHelper:getFieldValue("fcCopayOverrideNote":U          + cContainerCode)
     cEmergencyFlagValue         =         oRequestHelper:getFieldValue("cbEmergencyFlagValue":U         + cContainerCode)
     lEmergencyFlagValueUpdated  = LOGICAL(oRequestHelper:getFieldValue("flEmergencyFlagValueUpdated":U  + cContainerCode))
     cPenaltyFlagValue           =         oRequestHelper:getFieldValue("cbPenaltyValue":U               + cContainerCode)
     lPenaltyFlagValueUpdated    = LOGICAL(oRequestHelper:getFieldValue("flPenaltyValueUpdated":U        + cContainerCode))
     cPenaltyOverrideNote        =         oRequestHelper:getFieldValue("fcPenaltyOverrideNote":U        + cContainerCode)
     iStatus                     = INTEGER(oRequestHelper:getFieldValue("cbStatus":U                     + cContainerCode))
     iProviderNum                = INTEGER(oRequestHelper:getFieldValue("fiProviderNum":U                + cContainerCode))
     iDiscipline                 = INTEGER(oRequestHelper:getFieldValue("fiDiscipline":U                 + cContainerCode))
     iSubDiscipline              = INTEGER(oRequestHelper:getFieldValue("fiSubDiscipline":U              + cContainerCode))
     iAttProviderNum             = INTEGER(oRequestHelper:getFieldValue("fiAttProviderNum":U             + cContainerCode))
     iAttDiscipline              = INTEGER(oRequestHelper:getFieldValue("fiAttDiscipline":U              + cContainerCode))
     iAttSubDiscipline           = INTEGER(oRequestHelper:getFieldValue("fiAttSubDiscipline":U           + cContainerCode))
     iClaimCode                  = INTEGER(oRequestHelper:getFieldValue("fiClaimCode":U                  + cContainerCode))
     dAuthObj                    = DECIMAL(oRequestHelper:getFieldValue("_authObjArgument":U             + cContainerCode))
     dAuthGroupObj               = DECIMAL(oRequestHelper:getFieldValue("fdAuthGroupObj":U               + cContainerCode))
     dProviderObj                = DECIMAL(oRequestHelper:getFieldValue("fdAuthProviderObj":U            + cContainerCode))
     dQuantityAuth               = DECIMAL(oRequestHelper:getFieldValue("fdQuantityAuth":U               + cContainerCode))
     dAmountAuth                 = DECIMAL(oRequestHelper:getFieldValue("fdAmountAuth":U                 + cContainerCode))
     dAmountRequest              = DECIMAL(oRequestHelper:getFieldValue("fdAmountReq":U                  + cContainerCode))
     dQuantityRequest            = DECIMAL(oRequestHelper:getFieldValue("fdQtyReq":U                     + cContainerCode))
     dStartDate                  =    DATE(oRequestHelper:getFieldValue("fdStartDate":U                  + cContainerCode))
     dEndDate                    =    DATE(oRequestHelper:getFieldValue("fdEndDate":U                    + cContainerCode))
     
     /*iAttProviderNum    = IF iAttProviderNum = iProviderNum THEN 0 ELSE iAttProviderNum*/
     
     lStartAmPm         = CAN-DO("AM":U,oRequestHelper:getFieldValue("cbStartAmPm":U                        + cContainerCode))
     lEndAmPm           = CAN-DO("AM":U,oRequestHelper:getFieldValue("cbEndAmPm":U                          + cContainerCode))
     lAuthorisedService = CAN-DO("Y,YES,TRUE":U,oRequestHelper:getFieldValue("flAuthorised":U               + cContainerCode))
     lAuthoriseAllServices = CAN-DO("Y,YES,TRUE":U,oRequestHelper:getFieldValue("flAuthoriseAllServices":U  + cContainerCode))
     lMainProvider      = CAN-DO("Y,YES,TRUE":U,oRequestHelper:getFieldValue("flMainProvider":U             + cContainerCode))
     lLosCalculation    = CAN-DO("System":U,oRequestHelper:getFieldValue("flLosCalculation":U               + cContainerCode))
     
     oProvider          = NEW cls.maauthorisationprovider()
    NO-ERROR.


  IF NOT {&ErrorStatus} THEN
  DO:
    CASE cRecordAction:
      
      WHEN "modify":U THEN
      DO:
        oProvider:focusRecord(dProviderObj) NO-ERROR.
        
        ASSIGN
           oProvider:AccountReference           = cAccountReference
           oProvider:AmountAuth                 = dAmountAuth
           oProvider:AuthObj                    = dAuthObj
           oProvider:AuthGroupObj               = dAuthGroupObj
           oProvider:AuthStatus                 = iStatus    
           oProvider:AuthStatusNote             = cReason
           oProvider:ClaimCode                  = iClaimCode    
           oProvider:ClaimType                  = cClaimType
           oProvider:PrintDM                    = cPrintDM 
           oProvider:PayeeDM                    = cPayeeDM 
           oProvider:EndAmPm                    = lEndAmPm    
           oProvider:EndDate                    = dEndDate    
           oProvider:ProviderType               = cProviderType    
           oProvider:QuantityAuth               = dQuantityAuth
           oProvider:AuthorisedService          = lAuthorisedService
           oProvider:AuthoriseAllServices       = lAuthoriseAllServices
           oProvider:MainProvider               = lMainProvider
           oProvider:StartAmPm                  = lStartAmPm    
           oProvider:StartDate                  = dStartDate  
           oProvider:OverrideBaseRate           = cOverrideBaseRate
           oProvider:OverrideArsRate            = cOverrideArsRate
           oProvider:DefaultArsRate             = cDefaultArsRate
           oProvider:DefaultBaseRate            = cDefaultBaseRate
           oProvider:CopayOverrideNote          = cCopayOverrideNote
           oProvider:EmergencyFlagValue         = cEmergencyFlagValue       
           oProvider:EmergencyFlagValueUpdated  = lEmergencyFlagValueUpdated
           oProvider:PenaltyOverrideNote        = cPenaltyOverrideNote
           oProvider:PenaltyFlag                = cPenaltyFlagValue       
           oProvider:PenaltyFlagUpdated         = lPenaltyFlagValueUpdated
           oProvider:AmountRequested            = dAmountRequest  
           oProvider:QuantityRequested          = dQuantityRequest

           oProvider:ProviderSequence           = INTEGER(oRequestHelper:getFieldValue("cbSequence":U + cContainerCode))            
         NO-ERROR.              
        
        IF mipEnv:Health:maDoctor:isProviderAValidGroup(INPUT iProviderNum, INPUT dStartDate) OR ( iProviderNum <> 0.00 AND iAttProviderNum <> 0.00 )
        THEN
          ASSIGN  
             oProvider:GroupDocNum  = iProviderNum    
             oProvider:DocNum       = iAttProviderNum    
             oProvider:PrType       = iAttDiscipline    
             oProvider:SubPrType    = iAttSubDiscipline
            NO-ERROR.
        ELSE
          ASSIGN  
             oProvider:GroupDocNum  = 0    
             oProvider:DocNum       = iProviderNum    
             oProvider:PrType       = iDiscipline    
             oProvider:SubPrType    = iSubDiscipline
            NO-ERROR. 
        
        ASSIGN lSuccess = oProvider:saveRecord() NO-ERROR.
              
        IF NOT {&ErrorStatus} AND NOT oProvider:ErrorObject:ErrorsExist THEN
        DO:
          RUN _getProviderList IN TARGET-PROCEDURE ( INPUT oProvider:AuthObj, OUTPUT cProviderList  ).
          RUN _getAuthFlags    IN TARGET-PROCEDURE (INPUT  oProvider:AuthObj, INPUT  oProvider:StartDate, OUTPUT cFlagValue, OUTPUT cPenaltyOverrideNote).  

          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U 
            lSuccess                        = oResponseHelper:addFieldValue("fdAuthProviderObj":U           + cContainerCode, STRING(oProvider:AuthProviderObj) , TRUE)
            lSuccess                        = oResponseHelper:addFieldValue("fcSequenceList":U              + cContainerCode, fnSequenceList(oProvider:AuthObj) , TRUE)
            lSuccess                        = oResponseHelper:addFieldValue("_authProviderList":U           + cContainerCode, cProviderList                     , TRUE)
            lSuccess                        = oResponseHelper:addFieldValue("cbSequence":U                  + cContainerCode, STRING(oProvider:ProviderSequence), FALSE)
                                                                                                            
            lSuccess                        = oResponseHelper:addFieldValue("fdEndDate":U                   + cContainerCode, STRING(oProvider:EndDate,"99/99/9999":U))       
            lSuccess                        = oResponseHelper:addFieldValue("flPMBIndicator":U              + cContainerCode, STRING(oProvider:PMBIndicator))
            lSuccess                        = oResponseHelper:addFieldValue("flMainProvider":U              + cContainerCode, STRING(oProvider:MainProvider))
            lSuccess                        = oResponseHelper:addFieldValue("flLosCalculation":U            + cContainerCode, STRING(oProvider:LosCalculation))
            lSuccess                        = oResponseHelper:addFieldValue("cbOverrideBaseRate":U          + cContainerCode, STRING(oProvider:OverrideBaseRate))
            lSuccess                        = oResponseHelper:addFieldValue("cbOverrideArsRate":U           + cContainerCode, STRING(oProvider:OverrideArsRate))
            lSuccess                        = oResponseHelper:addFieldValue("fcDefaultBaseRate":U           + cContainerCode, STRING(oProvider:DefaultBaseRate))
            lSuccess                        = oResponseHelper:addFieldValue("fcDefaultArsRate":U            + cContainerCode, STRING(oProvider:DefaultArsRate))
            lSuccess                        = oResponseHelper:addFieldValue("flDefault":U                   + cContainerCode, "no":U )
            lSuccess                        = oResponseHelper:addFieldValue("flEmergencyFlagValueUpdated":U + cContainerCode, "no":U )
            lSuccess                        = oResponseHelper:addFieldValue("cbStatus":U                    + cContainerCode, STRING(oProvider:AuthStatus))
            lSuccess                        = oResponseHelper:addFieldValue("fcReason":U                    + cContainerCode, oProvider:AuthStatusNote)
            lSuccess                        = oResponseHelper:addFieldValue("fiClaimCode":U                 + cContainerCode, STRING(oProvider:ClaimCode))
            lSuccess                        = oResponseHelper:addFieldValue("cbPenaltyValue":U              + cContainerCode, cFlagValue)
            lSuccess                        = oResponseHelper:addFieldValue("fcPenaltyOverrideNote":U       + cContainerCode, cPenaltyOverrideNote)
            lSuccess                        = oResponseHelper:addFieldValue("fcCopayOverrideNote":U         + cContainerCode, oProvider:CopayOverrideNote)
            lSuccess                        = oResponseHelper:addFieldValue("_CopaymentProviderAction":U    + cContainerCode, IF oProvider:CopayProvider THEN "TRUE":U ELSE "FALSE":U , TRUE)
            lSuccess                        = oResponseHelper:addFieldValue("fdAmountAuth":U                + cContainerCode, STRING(oProvider:AmountAuth,"->,>>>,>>9.99"))
            lSuccess                        = oResponseHelper:addFieldValue("fdQuantityAuth":U              + cContainerCode, STRING(oProvider:QuantityAuth))
           NO-ERROR.            

        END. /*IF NOT {&ErrorStatus} AND NOT oAuthorisation:ErrorObject:ErrorsExist THEN*/             
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN
      DO:  
        oProvider:focusRecord(dProviderObj) NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oProvider:InFocus 
        THEN  
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U 
          NO-ERROR.
        ELSE 
          ASSIGN lSuccess = oProvider:deleteRecord() NO-ERROR.

        IF NOT {&ErrorStatus} AND NOT oProvider:ErrorObject:ErrorsExist THEN
        DO:
          RUN _getProviderList IN TARGET-PROCEDURE ( INPUT oProvider:AuthObj, OUTPUT cProviderList  ).

          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully removed":U
            oResponseHelper:ReturnValue     = "Record successfully removed":U 

            lSuccess                        = oResponseHelper:addFieldValue("_authProviderList":U + cContainerCode, cProviderList, TRUE)
           NO-ERROR.    
        END. /*IF NOT {&ErrorStatus} AND NOT oProvider:ErrorObject:ErrorsExist THEN*/
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
  
  ASSIGN cErrorMessage = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, "DESCRIPTION,STACK":U) NO-ERROR.   
  {&ResetError}

  IF cErrorMessage <> "":U OR oProvider:ErrorObject:ErrorsExist THEN                       
  DO:
   
    ASSIGN 
      oResponseHelper:RequestValid    = FALSE
      
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(oProvider:ErrorObject)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action'
      oResponseHelper:ReturnValue     = cErrorMessage
     NO-ERROR.
      
  END.   /*IF oDetail:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
  
  ASSIGN lSuccess = oResponseHelper:setError(oProvider:ErrorObject) NO-ERROR.
  
  { mip/inc/mipmessageerror.i &ResetTheError = TRUE }
  
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oProvider)       THEN DELETE OBJECT oProvider       NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidationProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationProvider Procedure 
PROCEDURE ajaxValidationProvider :
/*------------------------------------------------------------------------------
  Purpose   : Auth Provider Container Ajax Validation    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  
  { ma/app/maauthprovuiajaxval.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createAuthTypeProviderDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createAuthTypeProviderDefaults Procedure 
PROCEDURE createAuthTypeProviderDefaults :
/*------------------------------------------------------------------------------
  Purpose   : This routine is responsible for business logic involved
              with the creation of default provider records which is configured
              in the the Auth Type Maintenance.     
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER goAuthorisation AS cls.maauthorisation NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

  DEFINE VARIABLE cControlTypeIndicator AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cEntry                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cExceedLimRuleValue   AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cExceedLimStatusNote  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cPrType               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cPrTypeList           AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusList           AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cSubPrType            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cSubPrTypeList        AS CHARACTER            NO-UNDO.
  
  DEFINE VARIABLE iEntry                AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iProvider             AS INTEGER              NO-UNDO.
  
  DEFINE VARIABLE lExceedLimValidRule   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL              NO-UNDO.

  DEFINE VARIABLE oATSearch             AS cls.maauthtypesearch NO-UNDO.
  
  /* 
    Rule to check whether to default to header
  */  
  IF VALID-OBJECT(goAuthorisation) AND goAuthorisation:InFocus THEN
  DO:

    /* 
      Populate auth type dataset 
    */     
    ASSIGN oATSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE)

           lSuccess  = oATSearch:SetFilterCriteria("tt_auth_type.auth_type_obj":U, "=", goAuthorisation:AuthTypeObj) 

           lSuccess  = oATSearch:fetchData().

    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                   INPUT  goAuthorisation:MemberOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                   INPUT  "DefaultAuthHeadInfo":U,
                                                   INPUT  goAuthorisation:StartDate,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).

    /* 
      Separate statuses from descriptions 
    */
    DO iEntry = 1 TO NUM-ENTRIES(cRuleValue):

      ASSIGN cStatusList = TRIM(cStatusList) + SUBSTRING(ENTRY(iEntry, cRuleValue),1,1) + ",":U.
    END. /*DO iEntry = 1 TO NUM-ENTRIES(cRuleValue):*/

    ASSIGN 
       iEntry      = 0
       cStatusList = TRIM(cStatusList, ",":U).

    /* 
      Get ExceedLimitStatusReason Authrule setup to determine further down if 
      Header Authorisation status note was set by Limit Checking
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                   INPUT  goAuthorisation:MemberOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeLimits":U,
                                                   INPUT  "ExceedLimitStatusReason":U,
                                                   INPUT  goAuthorisation:StartDate,
                                                   OUTPUT lExceedLimValidRule,
                                                   OUTPUT cExceedLimRuleValue).
    IF  lExceedLimValidRule
    AND cExceedLimRuleValue <> "":U 
    THEN ASSIGN cExceedLimStatusNote = TRIM(cExceedLimRuleValue).

    FOR FIRST tt_auth_type NO-LOCK
        WHERE tt_auth_type.auth_type_obj = goAuthorisation:AuthTypeObj:
      
      ASSIGN cControlTypeIndicator = "ma_acAuthControlTypeIndicatorDef".

      {ma/msc/maauthtypecontrolread.i &hac_auth_type_control = tt_auth_type_control
                                      &AuthTypeObj           = goAuthorisation:AuthTypeObj
                                      &InsurerObj            = goAuthorisation:InsurerObj
                                      &OptionCode            = goAuthorisation:MemberOptionCode
                                      &ControlTypeIndicator  = cControlTypeIndicator
                                      &Date                  = goAuthorisation:StartDate
                                      &Lock                  = no-lock}

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

      ASSIGN iProvider = 2.

      FOR EACH  tt_auth_type_provider NO-LOCK 
         WHERE  tt_auth_type_provider.auth_type_obj            = goAuthorisation:AuthTypeObj
           AND  tt_auth_type_provider.auth_auto_create         = TRUE
           AND  tt_auth_type_provider.effective_date          <= goAuthorisation:StartDate  
           AND (tt_auth_type_provider.end_date                >= goAuthorisation:StartDate OR tt_auth_type_provider.end_date = ?)
            OR (tt_auth_type_provider.provider_type_indicator <> "ma_acAuthProviderTypeIndicatorExcl":U 
           AND  tt_auth_type_provider.pr_type_list            <> "":U) 
            BY  tt_auth_type_provider.provider_sequence:
/* ----------------- In to include --------------------*/
        ASSIGN cPrTypeList    = "":U
               cSubPrTypeList = "":U.

        /*
          Practice type list - could be populated for both defaults and exclusions

          If practice type list is not specified and the indicator is default then add blank record. 
        */
        IF tt_auth_type_provider.pr_type_list <> "":U 
        THEN 
        DO iEntry = 1 TO NUM-ENTRIES(tt_auth_type_provider.pr_type_list):

          ASSIGN cEntry = ENTRY(iEntry, tt_auth_type_provider.pr_type_list).

          /* 
            The discipline/sub-discipline are stored together eg. 001001,002002
          */
          IF LENGTH(cEntry) = 6   
          THEN 
            ASSIGN cPrTypeList    = cPrTypeList    + (IF cPrTypeList    = "":U THEN "" ELSE ",":U) + SUBSTRING(cEntry, 1, 3)
                   cSubPrTypeList = cSubPrTypeList + (IF cSubPrTypeList = "":U THEN "":U ELSE ",":U) + SUBSTRING(cEntry, 4, 3).

        END. /*IF tt_auth_type_provider.pr_type_list <> "":U  */
        ELSE  
          ASSIGN cPrTypeList    = cPrTypeList    + (IF cPrTypeList    = "":U THEN "":U ELSE ",":U) + "000":U
                 cSubPrTypeList = cSubPrTypeList + (IF cSubPrTypeList = "":U THEN "":U ELSE ",":U) + "000":U.


        /*
          Create Auth Provider records according to the Default and Exclusion Auth Type setups, as well as looping 
          through practice type/sub-practice type list to create Auth Provider records where specified.
        */
        DO iEntry = 1 TO NUM-ENTRIES(cPRTypeList):

          ASSIGN cPrType    = ENTRY(iEntry, cPrTypeList)
                 cSubPrType = ENTRY(iEntry, cSubPrTypeList).                          

          IF NOT CAN-FIND(FIRST tt_auth_provider NO-LOCK
                          WHERE tt_auth_provider.auth_obj      = goAuthorisation:AuthObj                              
                            AND tt_auth_provider.pr_type       = INTEGER(cPrType)
                            AND tt_auth_provider.sub_pr_type   = INTEGER(cSubPrType)
                            AND tt_auth_provider.provider_type = tt_auth_type_provider.provider_type)   
          THEN DO:
            
            CREATE tt_auth_provider.

            ASSIGN 
              iProvider                            = iProvider + 1
              tt_auth_provider._default            = TRUE                                     
              tt_auth_provider.record_action       = "MODIFY":U         
              tt_auth_provider.auth_obj            = goAuthorisation:AuthObj
              tt_auth_provider.auth_provider_obj   = -1 * iProvider 
              tt_auth_provider.doc_num             = 0
              tt_auth_provider.provider_type       = tt_auth_type_provider.provider_type
              tt_auth_provider.provider_sequence   = iProvider
              tt_auth_provider.pr_type             = INTEGER(cPrType)
              tt_auth_provider.sub_pr_type         = INTEGER(cSubPrType)
              tt_auth_provider.main_provider       = tt_auth_type_provider.main_provider
              tt_auth_provider.authorised_service  = tt_auth_type_provider.authorised_service
              tt_auth_provider.auth_group_obj      = tt_auth_type_provider.auth_group_obj

              tt_auth_provider.claim_code          = goAuthorisation:ClaimCode

              tt_auth_provider.claim_type          = goAuthorisation:ClaimType

              tt_auth_provider.amount_auth         = tt_auth_type_provider.amount_auth
              tt_auth_provider.quantity_auth       = tt_auth_type_provider.quantity_auth

              tt_auth_provider.start_date          = goAuthorisation:StartDate
              tt_auth_provider.end_date            = goAuthorisation:EndDate

              tt_auth_provider.start_ampm          = goAuthorisation:StartAmPm
              tt_auth_provider.end_ampm            = goAuthorisation:EndAmPm

              tt_auth_provider.auth_status         = (IF tt_auth_type_provider.default_auth_status <> "":U 
                                                      THEN INTEGER(tt_auth_type_provider.default_auth_status)
                                                      ELSE 
                                                        (IF tt_auth_type.default_auth_status <> "":U 
                                                         THEN INTEGER(tt_auth_type.default_auth_status)
                                                         ELSE goAuthorisation:AuthStatus))

              tt_auth_provider.auth_status_note    = (IF tt_auth_type_provider.default_auth_status_note <> "":U 
                                                      THEN tt_auth_type_provider.default_auth_status_note
                                                      ELSE                                     
                                                        (IF tt_auth_type.default_auth_status <> "":U  /*Deliberately checking status and not status note as note may be blank but status may not*/
                                                         THEN tt_auth_type.default_auth_status_note
                                                         ELSE 
                                                           (IF  goAuthorisation:AuthStatus     = 1
                                                            AND cExceedLimStatusNote          <> "":U
                                                            AND goAuthorisation:AuthStatusNote = cExceedLimStatusNote  
                                                            THEN tt_auth_provider.auth_status_note       /*Do not copy Auth Hdr reason if Limit was exeeded*/
                                                            ELSE goAuthorisation:AuthStatusNote))).      /*Otherwise copy Auth Hdr status reason as default*/        

            VALIDATE tt_auth_provider. 
            FIND FIRST tt_auth_provider EXCLUSIVE-LOCK
              WHERE tt_auth_provider.main_provider = TRUE NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE tt_auth_provider THEN
              ASSIGN tt_auth_provider.provider_sequence = 1.
          END. /*IF NOT CAN-FIND(FIRST tt_auth_provider NO-LOCK*/  
        END. /*DO iEntry = 1 TO NUM-ENTRIES(cPRTypeList):*/
        /*---------------------------- Into Include --------------------*/
      END. /*FOR EACH tt_auth_type_provider NO-LOCK*/

      IF CAN-FIND(FIRST tt_auth_provider NO-LOCK
                    WHERE tt_auth_provider.auth_obj = goAuthorisation:AuthObj                              
                      AND tt_auth_provider.pr_type <> 0)

      THEN DO: 
        mipEnv:Health:AuthBusinessLogic:SaveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).    
      END.

      FOR EACH  tt_auth_type_provider NO-LOCK 
         WHERE  tt_auth_type_provider.auth_type_obj            = goAuthorisation:AuthTypeObj
	   AND  tt_auth_type_provider.auth_auto_create        = NO
           AND  tt_auth_type_provider.effective_date          <= goAuthorisation:StartDate  
           AND (tt_auth_type_provider.end_date                >= goAuthorisation:StartDate OR tt_auth_type_provider.end_date = ?)
            OR (tt_auth_type_provider.provider_type_indicator <> "ma_acAuthProviderTypeIndicatorExcl":U 
           AND  tt_auth_type_provider.pr_type_list            <> "":U) 
           
            BY  tt_auth_type_provider.provider_sequence:
/*---------------------------- Into Include --------------------*/
        ASSIGN cPrTypeList    = "":U
               cSubPrTypeList = "":U.

        /*
          Practice type list - could be populated for both defaults and exclusions

          If practice type list is not specified and the indicator is default then add blank record. 
        */
        IF tt_auth_type_provider.pr_type_list <> "":U 
        THEN 
        DO iEntry = 1 TO NUM-ENTRIES(tt_auth_type_provider.pr_type_list):

          ASSIGN cEntry = ENTRY(iEntry, tt_auth_type_provider.pr_type_list).

          /* 
            The discipline/sub-discipline are stored together eg. 001001,002002
          */
          IF LENGTH(cEntry) = 6   
          THEN 
            ASSIGN cPrTypeList    = cPrTypeList    + (IF cPrTypeList    = "":U THEN "":U ELSE ",":U) + SUBSTRING(cEntry, 1, 3)
                   cSubPrTypeList = cSubPrTypeList + (IF cSubPrTypeList = "":U THEN "":U ELSE ",":U) + SUBSTRING(cEntry, 4, 3).

        END. /*IF tt_auth_type_provider.pr_type_list <> "":U  */
        ELSE  
          ASSIGN cPrTypeList    = cPrTypeList    + (IF cPrTypeList    = "":U THEN "":U ELSE ",":U) + "000":U
                 cSubPrTypeList = cSubPrTypeList + (IF cSubPrTypeList = "":U THEN "":U ELSE ",":U) + "000":U.


        /*
          Create Auth Provider records according to the Default and Exclusion Auth Type setups, as well as looping 
          through practice type/sub-practice type list to create Auth Provider records where specified.
        */
        DO iEntry = 1 TO NUM-ENTRIES(cPRTypeList):

          ASSIGN cPrType    = ENTRY(iEntry, cPrTypeList)
                 cSubPrType = ENTRY(iEntry, cSubPrTypeList).                          

          IF NOT CAN-FIND(FIRST tt_auth_provider NO-LOCK
                          WHERE tt_auth_provider.auth_obj      = goAuthorisation:AuthObj                              
                            AND tt_auth_provider.pr_type       = INTEGER(cPrType)
                            AND tt_auth_provider.sub_pr_type   = INTEGER(cSubPrType)
                            AND tt_auth_provider.provider_type = tt_auth_type_provider.provider_type)   
          THEN DO:
            
            CREATE tt_auth_provider.

            ASSIGN 
              iProvider                            = iProvider + 1
              tt_auth_provider._default            = TRUE                                     
              tt_auth_provider.record_action       = "MODIFY":U         
              tt_auth_provider.auth_obj            = goAuthorisation:AuthObj
              tt_auth_provider.auth_provider_obj   = -1 * iProvider 
              tt_auth_provider.doc_num             = 0
              tt_auth_provider.provider_type       = tt_auth_type_provider.provider_type
              tt_auth_provider.provider_sequence   = iProvider
              tt_auth_provider.pr_type             = INTEGER(cPrType)
              tt_auth_provider.sub_pr_type         = INTEGER(cSubPrType)
              tt_auth_provider.main_provider       = tt_auth_type_provider.main_provider
              tt_auth_provider.authorised_service  = tt_auth_type_provider.authorised_service
              tt_auth_provider.auth_group_obj      = tt_auth_type_provider.auth_group_obj

              tt_auth_provider.claim_code          = goAuthorisation:ClaimCode

              tt_auth_provider.claim_type          = goAuthorisation:ClaimType

              tt_auth_provider.amount_auth         = tt_auth_type_provider.amount_auth
              tt_auth_provider.quantity_auth       = tt_auth_type_provider.quantity_auth

              tt_auth_provider.start_date          = goAuthorisation:StartDate
              tt_auth_provider.end_date            = goAuthorisation:EndDate

              tt_auth_provider.start_ampm          = goAuthorisation:StartAmPm
              tt_auth_provider.end_ampm            = goAuthorisation:EndAmPm

              tt_auth_provider.auth_status         = (IF tt_auth_type_provider.default_auth_status <> "":U 
                                                      THEN INTEGER(tt_auth_type_provider.default_auth_status)
                                                      ELSE 
                                                        (IF tt_auth_type.default_auth_status <> "":U 
                                                         THEN INTEGER(tt_auth_type.default_auth_status)
                                                         ELSE goAuthorisation:AuthStatus))

              tt_auth_provider.auth_status_note    = (IF tt_auth_type_provider.default_auth_status_note <> "":U 
                                                      THEN tt_auth_type_provider.default_auth_status_note
                                                      ELSE                                     
                                                        (IF tt_auth_type.default_auth_status <> "":U  /*Deliberately checking status and not status note as note may be blank but status may not*/
                                                         THEN tt_auth_type.default_auth_status_note
                                                         ELSE 
                                                           (IF  goAuthorisation:AuthStatus     = 1
                                                            AND cExceedLimStatusNote          <> "":U
                                                            AND goAuthorisation:AuthStatusNote = cExceedLimStatusNote  
                                                            THEN tt_auth_provider.auth_status_note       /*Do not copy Auth Hdr reason if Limit was exeeded*/
                                                            ELSE goAuthorisation:AuthStatusNote))).      /*Otherwise copy Auth Hdr status reason as default*/        

            VALIDATE tt_auth_provider. 
            FIND FIRST tt_auth_provider EXCLUSIVE-LOCK
              WHERE tt_auth_provider.main_provider = TRUE NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE tt_auth_provider THEN
              ASSIGN tt_auth_provider.provider_sequence = 1.
          END. /*IF NOT CAN-FIND(FIRST tt_auth_provider NO-LOCK*/  
        END. /*DO iEntry = 1 TO NUM-ENTRIES(cPRTypeList):*/
/*---------------------------- Into Include --------------------*/
      END. /*FOR EACH tt_auth_type_provider NO-LOCK*/
      
    END. /* FOR FIRST tt_auth_type NO-LOCK: */

    IF lValidRule AND LOOKUP(STRING(goAuthorisation:AuthStatus), cStatusList) > 0 THEN
    DO:
      FOR EACH tt_auth_provider EXCLUSIVE-LOCK:

        IF LOOKUP(STRING(tt_auth_provider.auth_status),"0,5,6") = 0 THEN
        DO:
          ASSIGN
            tt_auth_provider.auth_status      = goAuthorisation:AuthStatus
            tt_auth_provider.auth_status_note = goAuthorisation:AuthStatusNote.

          VALIDATE tt_auth_provider.
        END.  /* IF LOOKUP(STRING(tt_auth_provider.auth_status),"0,5,6") = 0 THEN */
      END. /* FOR EACH tt_auth_provider EXCLUSIVE-LOCK: */
    END. /* IF lValidRule AND LOOKUP(STRING(goAuthorisation:AuthStatus), cStatusList) > 0 THEN*/        
  END. /*IF VALID-OBJECT(goAuthorisation) AND goAuthorisation:InFocus THEN*/

  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oATSearch) THEN DELETE OBJECT oATSearch.

                DATASET dsAuthType:EMPTY-DATASET()."}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdProvider Procedure 
PROCEDURE getCntUpdProvider :
/*------------------------------------------------------------------------------
  Purpose   : Update provider container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
        
  { ma/app/maauthprovgetcntupdprovider.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdProviderForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdProviderForm Procedure 
PROCEDURE getCntUpdProviderForm :
/*------------------------------------------------------------------------------
  Purpose   : Update authorisation provider container definition form 
  Parameters: 
  Notes     :     
------------------------------------------------------------------------------*/

  { ma/app/maauthprovgetcntupdform.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-renderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderProcedure Procedure 
PROCEDURE renderProcedure :
/*------------------------------------------------------------------------------
  Purpose   : Auth Provider Container Render Procedure    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  
  { ma/app/maauthprovuirenderprocedure.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rowRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowRenderProcedure Procedure 
PROCEDURE rowRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oQuery                  AS cls.mipquery     NO-UNDO.
                                          
  DEFINE VARIABLE oStatusNote             AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oStatusNoteBtn          AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oClaimCode              AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oClaimCodeBtn           AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oClaimType              AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oReasonType             AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAmntAuth               AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oQtyAuth                AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oSequenceList           AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oProviderList           AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oProvider               AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oPrType                 AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oSubPrType              AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAttProvider            AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAttProviderLkp         AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAttPrType              AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAttSubPrType           AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oPrTypeLkp              AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oSubPrTypeLkp           AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAttPrTypeLkp           AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAttSubPrTypeLkp        AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oAuthGroup              AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oStartDateAmPm          AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oEndDateAmPm            AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oOverrideNoteBtn        AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oPenaltyOverrideNoteBtn AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oPenaltyFlagValue       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oPenaltyOverrideNote    AS cls.mipwscontrol NO-UNDO.
  
  DEFINE VARIABLE dAuthObj         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dInsurerObj      AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj     AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthRuleObj     AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dQuantityPaid    AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAmountPaid      AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dLinkAuthRuleObj AS DECIMAL          NO-UNDO. 
  DEFINE VARIABLE dStartDate       AS DATE             NO-UNDO.
  DEFINE VARIABLE iOption          AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cMemNum          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cProviderType    AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cDiscipline      AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cSubDiscipline   AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cStatus          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cStatusNote      AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAuthGroupList   AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleValue       AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleCode        AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleValidValues AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleDescription AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleType        AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cNoteDescription AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE lMandatory       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSuccess         AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lValidRule       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lStartDateAmPm   AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lEndDateAmPm     AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lCopayProvider   AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lMainProvider    AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSystemOwned     AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lActivatePenaltyFlag    AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE cPenaltyFlag     AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE hWob             AS HANDLE           NO-UNDO.
  DEFINE VARIABLE dEndDate                  AS DATE             NO-UNDO.  
  
  DEFINE VARIABLE oAuthFlagSearch  AS cls.maauthflagvaluesearch NO-UNDO.

  /*
    RowRenderProcedure will be run in the current wob to attach
    any business logic validation errors generated during a submit
    to this container. RowRenderProcedure container code which should
    be applied to this container ragardless of which wob this container
    is used in should be placed in the case block below.
  */
  IF LOOKUP(ipoContainer:RowRenderArgument, "AuthProviderContainer":U) = 0 THEN
  DO:
    RUN SUPER(INPUT ipoContainer) NO-ERROR.
  
    /* 
      The super may not have a rowRenderProcedure 
    */
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6439' &ResetIgnoredErrors = TRUE }      
       
  END. /*IF LOOKUP(ipoContainer:RenderArgument, "AuthProviderContainer":U) = 0 THEN*/
  ELSE
  DO:
    ASSIGN hWob = mipEnv:miProcedure:getProcedureHandle(INPUT "Wob_":U + WarpSpeed:CurrentWob).
      
     
    IF VALID-HANDLE(hWob) 
    THEN RUN rowRenderProcedure IN hWob(INPUT ipoContainer) NO-ERROR.
    
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6456,PROGRESS:3234' &ResetIgnoredErrors = FALSE }

  END. /*ELSE*/
  
&IF {&DBDFMA} >= 010195 &THEN  
  
  CASE ipoContainer:RowRenderArgument:
  
    WHEN "AuthProviderContainer":U THEN
    DO:
      ASSIGN oQuery              = ipoContainer:ContainerQuery
                                 
             dAuthObj            = DECIMAL(oQuery:getFieldAttribute("tt_auth.auth_obj":U                  , "BUFFER-VALUE":U)) 
             dInsurerObj         = DECIMAL(oQuery:getFieldAttribute("tt_auth.insurer_obj":U               , "BUFFER-VALUE":U)) 
             dAuthTypeObj        = DECIMAL(oQuery:getFieldAttribute("tt_auth.auth_type_obj":U             , "BUFFER-VALUE":U)) 
             dAuthProviderObj    = DECIMAL(oQuery:getFieldAttribute("tt_auth_provider.auth_provider_obj":U, "BUFFER-VALUE":U)) 
             dQuantityPaid       = DECIMAL(oQuery:getFieldAttribute("tt_auth_provider.quantity_paid":U    , "BUFFER-VALUE":U))
             dAmountPaid         = DECIMAL(oQuery:getFieldAttribute("tt_auth_provider.amount_paid":U      , "BUFFER-VALUE":U))
                             
             dStartDate          =    DATE(oQuery:getFieldAttribute("tt_auth_provider.start_date":U       , "BUFFER-VALUE":U)) 
                                 
             iOption             = INTEGER(oQuery:getFieldAttribute("tt_auth.option_code":U               , "BUFFER-VALUE":U)) 
                                 
             cMemNum             =         oQuery:getFieldAttribute("tt_auth.mem_num":U                   , "BUFFER-VALUE":U)  
             cProviderType       =         oQuery:getFieldAttribute("tt_auth_provider.provider_type":U    , "BUFFER-VALUE":U)  
             cDiscipline         =         oQuery:getFieldAttribute("tt_auth_provider.pr_type":U          , "BUFFER-VALUE":U)  
             cSubDiscipline      =         oQuery:getFieldAttribute("tt_auth_provider.sub_pr_type":U      , "BUFFER-VALUE":U)
             cStatus             =         oQuery:getFieldAttribute("tt_auth_provider.auth_status":U      , "BUFFER-VALUE":U)
             cStatusNote         =         oQuery:getFieldAttribute("tt_auth_provider.auth_status_note":U , "BUFFER-VALUE":U)
             cAuthGroupList      =         oQuery:getFieldAttribute("tt_auth._auth_group_list":U          , "BUFFER-VALUE":U)
             
             dInsurerObj         = (IF dInsurerObj = ? 
                                    THEN 0.00
                                    ELSE dInsurerObj)
                                 
             iOption             = (IF iOption = ? 
                                    THEN 0 
                                    ELSE iOption)
                                                                     
             dStartDate          = (IF dStartDate = ?
                                    THEN TODAY 
                                    ELSE dStartDate)

             lStartDateAmPm      = LOGICAL(oQuery:getFieldAttribute("tt_auth_provider.start_ampm":U ,     "BUFFER-VALUE":U))
             lEndDateAmPm        = LOGICAL(oQuery:getFieldAttribute("tt_auth_provider.end_ampm":U ,       "BUFFER-VALUE":U))
             lCopayProvider      = LOGICAL(oQuery:getFieldAttribute("tt_auth_provider.copay_provider":U , "BUFFER-VALUE":U))
             lMainProvider       = LOGICAL(oQuery:getFieldAttribute("tt_auth_provider.main_provider":U  , "BUFFER-VALUE":U))
                                    
             gcProviderList      =         oQuery:getFieldAttribute("tt_auth._provider_list":U                 , "BUFFER-VALUE":U)
             glMandatory         = LOGICAL(oQuery:getFieldAttribute("tt_auth_provider._status_note_mandatory":U, "BUFFER-VALUE":U))
             . 
      
      /*
        Lets get all the required controls
      */
      ASSIGN oStatusNote              = ipoContainer:getControl("fcReason":U                  + ipoContainer:ContainerCode)
             oStatusNoteBtn           = ipoContainer:getControl("buReasonBtn":U               + ipoContainer:ContainerCode)
             oReasonType              = ipoContainer:getControl("fcReasonTypeArgument":U      + ipoContainer:ContainerCode)
             oClaimCode               = ipoContainer:getControl("fiClaimCode":U               + ipoContainer:ContainerCode)
             oClaimCodeBtn            = ipoContainer:getControl("buClaimBtn":U                + ipoContainer:ContainerCode)
             oClaimType               = ipoContainer:getControl("cbClaimType":U               + ipoContainer:ContainerCode)
             oAmntAuth                = ipoContainer:getControl("fdAmountAuth":U              + ipoContainer:ContainerCode)
             oQtyAuth                 = ipoContainer:getControl("fdQuantityAuth":U            + ipoContainer:ContainerCode)
             oProviderList            = ipoContainer:getControl("_authProviderList":U         + ipoContainer:ContainerCode)
             oProvider                = ipoContainer:getControl("fiProviderNum":U             + ipoContainer:ContainerCode)
             oPrType                  = ipoContainer:getControl("fiDiscipline":U              + ipoContainer:ContainerCode)
             oSubPrType               = ipoContainer:getControl("fiSubDiscipline":U           + ipoContainer:ContainerCode)
             oAttProvider             = ipoContainer:getControl("fiAttProviderNum":U          + ipoContainer:ContainerCode)
             oAttProviderLkp          = ipoContainer:getControl("buAttProviderBtn":U          + ipoContainer:ContainerCode)
             oAttPrType               = ipoContainer:getControl("fiAttDiscipline":U           + ipoContainer:ContainerCode)
             oAttSubPrType            = ipoContainer:getControl("fiAttSubDiscipline":U        + ipoContainer:ContainerCode) 
             oPrTypeLkp               = ipoContainer:getControl("buDiscipline":U              + ipoContainer:ContainerCode)
             oSubPrTypeLkp            = ipoContainer:getControl("buSubDiscipline":U           + ipoContainer:ContainerCode)
             oAttPrTypeLkp            = ipoContainer:getControl("buAttDiscipline":U           + ipoContainer:ContainerCode)
             oAttSubPrTypeLkp         = ipoContainer:getControl("buAttSubDiscipline":U        + ipoContainer:ContainerCode)
             oAuthGroup               = ipoContainer:getControl("fdAuthGroupObj":U            + ipoContainer:ContainerCode)
             oStartDateAmPm           = ipoContainer:getControl("cbStartAmPm":U               + ipoContainer:ContainerCode)
             oEndDateAmPm             = ipoContainer:getControl("cbEndAmPm":U                 + ipoContainer:ContainerCode)
             oOverrideNoteBtn         = ipoContainer:getControl("buOverrideNoteBtn":U         + ipoContainer:ContainerCode)
             oPenaltyOverrideNoteBtn  = ipoContainer:getControl("buPenaltyOverrideNoteBtn":U  + ipoContainer:ContainerCode)
             oPenaltyOverrideNote     = ipoContainer:getControl("fcPenaltyOverrideNote":U     + ipoContainer:ContainerCode)
             oPenaltyFlagValue        = ipoContainer:getControl("cbPenaltyValue":U            + ipoContainer:ContainerCode).
      /*
        If a provider or Attending provider exists, the user should not be able to update the pr type or sub pr type and we should hide the lookup buttons 
      */
      ASSIGN 
        oPrType:ControlToken          = IF oProvider:ControlValue    <>  "0":U THEN "Disabled":U ELSE "Updatable":U
        oSubPrType:ControlToken       = IF oProvider:ControlValue    <>  "0":U THEN "Disabled":U ELSE "Updatable":U
        oAttPrType:ControlToken       = IF oAttProvider:ControlValue <>  "0":U THEN "Disabled":U ELSE "Updatable":U
        oAttSubPrType:ControlToken    = IF oAttProvider:ControlValue <>  "0":U THEN "Disabled":U ELSE "Updatable":U

        oPrType:ControlClass          = IF oProvider:ControlValue    <>  "0":U THEN "-clMan":U   ELSE "+clMan":U
        oAttPrType:ControlClass       = IF oProvider:ControlValue    <>  "0":U THEN "-clMan":U   ELSE "+clMan":U

        oPrTypeLkp:ControlToken       = IF oProvider:ControlValue    <>  "0":U THEN "Disabled":U ELSE "Updatable":U
        oSubPrTypeLkp:ControlToken    = IF oProvider:ControlValue    <>  "0":U THEN "Disabled":U ELSE "Updatable":U
        oAttPrTypeLkp:ControlToken    = IF oAttProvider:ControlValue <>  "0":U THEN "Disabled":U ELSE "Updatable":U
        oAttSubPrTypeLkp:ControlToken = IF oAttProvider:ControlValue <>  "0":U THEN "Disabled":U ELSE "Updatable":U

        oStartDateAmPm:ControlValue   = IF lStartDateAmPm THEN "AM":U ELSE IF not lStartDateAmPm THEN "PM":U ELSE ?
        oEndDateAmPm:ControlValue     = IF lEndDateAmPm   THEN "AM":U ELSE IF not lEndDateAmPm   THEN "PM":U ELSE ?

        .

      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                     INPUT  iOption,
                                                     INPUT  "ma_acAuthRuleTypeAuthProvider":U,
                                                     INPUT  "ProviderGroupValidate":U,
                                                     INPUT  dStartDate,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
  
      IF lValidRule AND cRuleValue = "[Discipline]":U 
      THEN
        ASSIGN oAttProviderLkp:LookupWobFLA  = "prassoc2":U.
      ELSE
        ASSIGN oAttProviderLkp:LookupWobFLA  = "prassoc":U.

      /*
        - Set provider list
        - Set base rate list
      */
      ASSIGN oProviderList:ControlValue = gcProviderList
             oAuthGroup:AdditionalItems = cAuthGroupList . 
      
      /*
        Set note description as tooltip on note controls and set note control token according to auth type config
      */
      ASSIGN oStatusNote:ControlClass    = oStatusNote:ControlClass 
                                         + (IF glMandatory THEN " +":U ELSE " -":U) + "clMan":U
                                         
             oStatusNote:ControlToken    = (IF glMandatory THEN "Updatable":U ELSE "Disabled":U)
             oStatusNoteBtn:ControlToken = (IF glMandatory THEN "Updatable":U ELSE "Disabled":U).
      
      /*
        - Status update allowed
        - Claim code update allowed
        - Claim type update allowed
        - Auth value update allowed
      */
      IF dAuthProviderObj <> 0.00 AND dAuthProviderObj <> ? 
      THEN
        ASSIGN oStatusNote:ControlToken    = oQuery:getFieldAttribute("tt_auth_provider._status_note_upd_allow":U,"BUFFER-VALUE":U) 

               oStatusNoteBtn:ControlToken = oStatusNote:ControlToken
        
               oClaimCode:ControlToken     = oQuery:getFieldAttribute("tt_auth_provider._claim_code_updateable":U,"BUFFER-VALUE":U) 
                                              
               oClaimCodeBtn:ControlToken  = oClaimCode:ControlToken
               
               oClaimType:ControlToken     = oQuery:getFieldAttribute("tt_auth_provider._claim_type_updateable":U,"BUFFER-VALUE":U) 
               
               oAmntAuth:ControlToken      = oQuery:getFieldAttribute("tt_auth_provider._authorised_value_updateable":U,"BUFFER-VALUE":U) 
               oQtyAuth:ControlToken       = oQuery:getFieldAttribute("tt_auth_provider._authorised_value_updateable":U,"BUFFER-VALUE":U). 
                                                      
      IF dAuthProviderObj <= 0 
      OR dAuthProviderObj = ?
      OR NOT lCopayProvider 
      THEN 
        ASSIGN oOverrideNoteBtn:ControlToken = "Updatable":U .
      ELSE 
        ASSIGN oOverrideNoteBtn:ControlToken = "Disabled":U .
        
      /* Penalty co-Payment */
      ASSIGN cRuleType = "ma_acAuthRuleTypeAuthFlag":U
             cRuleCode = "PENALTY":U.

      mipEnv:Health:AuthMaintenance:getAuthRuleDetails( INPUT-OUTPUT dAuthRuleObj   ,
                                                        INPUT-OUTPUT dInsurerObj    ,
                                                        INPUT-OUTPUT iOption    ,
                                                        INPUT-OUTPUT cRuleType  ,
                                                        INPUT-OUTPUT cRuleCode  ,
                                                        INPUT-OUTPUT dStartDate ,
                                                              OUTPUT lValidRule      ,
                                                              OUTPUT cRuleValue      ,
                                                              OUTPUT cRuleValidValues,
                                                              OUTPUT dLinkAuthRuleObj,
                                                              OUTPUT cRuleDescription,
                                                              OUTPUT lSystemOwned    ,
                                                              OUTPUT dEndDate).

     
      mipEnv:health:AuthBusinessLogic:activatePenaltyFlag( INPUT  dAuthTypeObj , 
                                                           INPUT  dInsurerObj, 
                                                           INPUT  iOption, 
                                                           INPUT  dStartDate ,
                                                           OUTPUT lActivatePenaltyFlag).
     
      IF lActivatePenaltyFlag THEN 
      DO:
        IF lMainProvider THEN 
        DO:
          IF lValidRule AND cRuleValidValues <> "":U
          THEN 
            ASSIGN oPenaltyFlagValue:AdditionalItems = cRuleValidValues .
          ELSE
            ASSIGN oPenaltyFlagValue:AdditionalItems = "<None>":U.
          
          EMPTY TEMP-TABLE tt_auth_flag_value.
          
          ASSIGN oAuthFlagSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE)
                 lSuccess        = mipEnv:RegisterForCleanup(oAuthFlagSearch)
                 lSuccess        = oAuthFlagSearch:SetCriteria("BufferList":U, "tt_auth_flag_value":U)
                 lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
                 lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, dAuthObj)
                 lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.auth_rule_obj":U         , "=":U, dAuthRuleObj)
                 lSuccess        = oAuthFlagSearch:fetchData().
          
          FIND FIRST tt_auth_flag_value 
               WHERE tt_auth_flag_value.owning_entity_mnemonic = "hatau":U
                 AND tt_auth_flag_value.owning_obj             = dAuthObj
                 AND tt_auth_flag_value.auth_rule_obj          = dAuthRuleObj NO-ERROR.
          
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF AVAILABLE(tt_auth_flag_value) 
          AND dAuthProviderObj > 0 
          THEN DO:
            mipEnv:health:maUtility:getNoteDescription(INPUT tt_auth_flag_value.override_note,
                                                       INPUT "AP":U ,
                                                       OUTPUT cNoteDescription) .
       
            ASSIGN
              oPenaltyFlagValue:ControlValue      = tt_auth_flag_value.auth_flag_value
              oPenaltyOverrideNote:ControlValue   = tt_auth_flag_value.override_note
              oPenaltyOverrideNote:ControlToolTip = IF cNoteDescription <> "":U THEN cNoteDescription ELSE "Please enter a Penalty Override Reason.":U.
          END.
          ELSE 
            ASSIGN 
              oPenaltyFlagValue:ControlValue    = "":U 
              oPenaltyOverrideNote:ControlValue = "":U.
        END. /* IF lMainProvider THEN  */
        ELSE DO:
          ASSIGN
            oPenaltyFlagValue:ControlClass       = "+clHid":U
            oPenaltyOverrideNote:ControlClass    = "+clHid":U
            oPenaltyOverrideNoteBtn:ControlToken = "Hidden":U.

        END.
        
      END. /* IF lActivatePenaltyFlag THEN */
      

    END. /*WHEN "AuthProviderContainer":U THEN*/
    
  END CASE.

&ENDIF
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "EMPTY TEMP-TABLE ttAuthTypeConfig.
                DATASET dsAuthorisation:EMPTY-DATASET().
                "}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_getAuthFlags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _getAuthFlags Procedure 
PROCEDURE _getAuthFlags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthObj        AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate      AS DATE       NO-UNDO.
  DEFINE OUTPUT PARAMETER opcFlagValue      AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER opcPenaltNote     AS CHARACTER  NO-UNDO.

  
  DEFINE VARIABLE dAuthRuleObj          AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj          AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dInsurerObj           AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dLinkAuthRuleObj      AS DECIMAL          NO-UNDO. 
  DEFINE VARIABLE cRuleValue            AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleCode             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleValidValues      AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleDescription      AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleType             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iOptionCode           AS INTEGER          NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSystemOwned          AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lActivatePenaltyFlag  AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE dEndDate              AS DATE             NO-UNDO.
  DEFINE VARIABLE dStartDate            AS DATE             NO-UNDO.

  DEFINE VARIABLE oAuthFlagSearch       AS cls.maauthflagvaluesearch NO-UNDO.
  DEFINE VARIABLE oSearch               AS cls.maauthsearch          NO-UNDO.
    
  
  oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).

  DATASET dsAuthorisation:EMPTY-DATASET().

  ASSIGN    
    lSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth":U)
    lSuccess = oSearch:SetFilterCriteria("tt_auth.auth_obj":U, "=":U, ipdAuthObj).
             
    oSearch:fetchData().


   /* Penalty co-Payment */
   ASSIGN cRuleType = "ma_acAuthRuleTypeAuthFlag":U
          cRuleCode = "PENALTY":U.

   FIND FIRST tt_auth NO-LOCK.

   IF AVAILABLE tt_auth 
   THEN ASSIGN dInsurerObj  = tt_auth.insurer_obj
               iOptionCode  = tt_auth.option_code
               dAuthTypeObj = tt_auth.auth_type_obj.


   mipEnv:Health:AuthMaintenance:getAuthRuleDetails( INPUT-OUTPUT dAuthRuleObj   ,
                                                     INPUT-OUTPUT dInsurerObj    ,
                                                     INPUT-OUTPUT iOptionCode    ,
                                                     INPUT-OUTPUT cRuleType  ,
                                                     INPUT-OUTPUT cRuleCode  ,
                                                     INPUT-OUTPUT ipdStartDate ,
                                                           OUTPUT lValidRule      ,
                                                           OUTPUT cRuleValue      ,
                                                           OUTPUT cRuleValidValues,
                                                           OUTPUT dLinkAuthRuleObj,
                                                           OUTPUT cRuleDescription,
                                                           OUTPUT lSystemOwned    ,
                                                           OUTPUT dEndDate).


   mipEnv:health:AuthBusinessLogic:activatePenaltyFlag( INPUT  dAuthTypeObj , 
                                                        INPUT  dInsurerObj, 
                                                        INPUT  iOptionCode, 
                                                        INPUT  dStartDate ,
                                                        OUTPUT lActivatePenaltyFlag).
   
   IF lActivatePenaltyFlag THEN 
   DO:
   
     EMPTY TEMP-TABLE tt_auth_flag_value.
    
     ASSIGN oAuthFlagSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE)
            lSuccess        = mipEnv:RegisterForCleanup(oAuthFlagSearch)
            lSuccess        = oAuthFlagSearch:SetCriteria("BufferList":U, "tt_auth_flag_value":U)
            lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
            lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, ipdAuthObj)
            lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.auth_rule_obj":U         , "=":U, dAuthRuleObj)
            lSuccess        = oAuthFlagSearch:fetchData().
   
     FIND FIRST tt_auth_flag_value 
          WHERE tt_auth_flag_value.owning_entity_mnemonic = "hatau":U
            AND tt_auth_flag_value.owning_obj             = ipdAuthObj
            AND tt_auth_flag_value.auth_rule_obj          = dAuthRuleObj NO-ERROR.
     
     { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
     
     IF AVAILABLE(tt_auth_flag_value)
     THEN ASSIGN opcFlagValue  = tt_auth_flag_value.auth_flag_value
                 opcPenaltNote = tt_auth_flag_value.override_note.
     ELSE ASSIGN opcFlagValue  = "":U
                 opcPenaltNote = "":U.
   END.

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
  DEFINE INPUT  PARAMETER ipdAuthTypeObj    AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj     AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate      AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER ipcProviderType   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ipiNegNum         AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipcPrType         AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ipcSubPrType      AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER oplcConfiguration AS LONGCHAR   NO-UNDO.
  
  DEFINE VARIABLE oJsonArray           AS JsonArray            NO-UNDO.
  DEFINE VARIABLE oJsonObject          AS JsonObject           NO-UNDO.
  DEFINE VARIABLE oJsonParser          AS ObjectModelParser    NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValid               AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lNotePerAuthStatus   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cRuleValue           AS CHARACTER            NO-UNDO.
  
  
  EMPTY TEMP-TABLE ttAuthTypeConfig.
  
  
  IF ipdAuthTypeObj <> 0.00 THEN
  DO:
  
    mipEnv:Health:AuthService:getAuthTypeConfig(INPUT ipdAuthTypeObj, 
                                                INPUT ipdInsurerObj,
                                                INPUT ipiOptionCode,
                                                INPUT ipdStartDate,
                                                INPUT ipcProviderType,
                                                INPUT ipiNegNum,
                                                INPUT ipcPrType,
                                                INPUT ipcSubPrType,
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).
                                                        
    /* UTF encoding for the parser */
    FIX-CODEPAGE(oplcConfiguration) = "UTF-8":U.
    
    
    TEMP-TABLE ttAuthTypeConfig:WRITE-JSON("LONGCHAR":U, oplcConfiguration, FALSE, "UTF-8":U, FALSE, TRUE).
    
    
    ASSIGN lSuccess           = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                  (INPUT  ipdInsurerObj,
                                   INPUT  ipiOptionCode,
                                   INPUT  "ma_acAuthRuleTypeAUTHSETUPS":U,
                                   INPUT  "NotePerAuthStatus":U,
                                   INPUT  ipdStartDate,
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
  END. /*IF ipdAuthTypeObj <> 0.00 THEN*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "EMPTY TEMP-TABLE ttAuthTypeConfig."}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_getProviderList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _getProviderList Procedure 
PROCEDURE _getProviderList PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters:  
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthObj      AS DECIMAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcProviderList AS CHARACTER NO-UNDO.
 
  DEFINE VARIABLE lSuccess AS LOGICAL  NO-UNDO.
  &IF {&DBDFMA} >= 010195 &THEN
    
    DEFINE BUFFER hat_auth          FOR hat_auth.
    DEFINE BUFFER hat_auth_provider FOR hat_auth_provider.
    DEFINE BUFFER buf_doctor        FOR doctor.
    
    IF NOT CAN-FIND(FIRST hat_auth_provider
                    WHERE hat_auth_provider.auth_obj = ipdAuthObj
                      AND hat_auth_provider.main_provider)
    THEN DO:
      ASSIGN
        opcProviderList = "":U.
    
      RETURN.
    END. /* IF NOT CAN-FIND(FIRST hat_auth_provider) */
                       
    FOR EACH hat_auth_provider NO-LOCK
       WHERE hat_auth_provider.auth_obj = ipdAuthObj
          BY hat_auth_provider.doc_num:
    
      RELEASE buf_doctor.
    
      FIND FIRST buf_doctor NO-LOCK
           WHERE buf_doctor.doc-num = hat_auth_provider.doc_num
             AND buf_doctor.doc-num <> 0
        NO-ERROR.
    
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      FIND FIRST prtype NO-LOCK
           WHERE prtype.pr-type = hat_auth_provider.pr_type
        NO-ERROR.    
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE buf_doctor OR AVAILABLE prtype
      THEN   
        ASSIGN
          opcProviderList = opcProviderList
                          + (IF opcProviderList = "":U THEN "":U ELSE "^":U)
                          + (IF AVAILABLE buf_doctor    
                             THEN
                               UPPER(buf_doctor.name) + " (":U + STRING(buf_doctor.doc-num) + ")":U 
                             ELSE 
                               (IF AVAILABLE prtype 
                                THEN UPPER(prtype.description) + " (":U + STRING(prtype.pr-type) + ")":U
                                ELSE "":U))                          
                          + "=":U                        
                          + STRING(hat_auth_provider.auth_provider_obj).
    END. /* FOR EACH hat_auth_provider NO-LOCK */

  &ENDIF /*&IF {&DBDFMA} >= 010195 &THEN*/
  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnPrepareStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnPrepareStatus Procedure 
FUNCTION fnPrepareStatus RETURNS CHARACTER
  ( ipcType AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStatusList AS CHARACTER   NO-UNDO.
  
   
  EMPTY TEMP-TABLE ttAuthStatus.
  
   
  mipEnv:Health:AuthService:getStatusTable(INPUT ipcType, OUTPUT TABLE ttAuthStatus).
  
  FOR EACH ttAuthStatus NO-LOCK BY ttAuthStatus.status_description:
  
    ASSIGN cStatusList = cStatusList
                       + (IF cStatusList = "":U THEN  "":U ELSE "|":U)
                       + ttAuthStatus.status_description + "=":U + STRING(ttAuthStatus.status_code).
  END. /*FOR EACH ttAuthStatus NO-LOCK BY ttAuthStatus.status_description:*/
           
           
  RETURN cStatusList.   /* Function return value. */

  { mip/inc/mipcatcherror.i
    &FINALLY = "EMPTY TEMP-TABLE ttAuthStatus." }
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnSequenceList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnSequenceList Procedure 
FUNCTION fnSequenceList RETURNS CHARACTER
  (  ipdAuthObj AS DECIMAL  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess      AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cSequencelist AS CHARACTER  NO-UNDO.
 
  &IF {&DBDFMA} >= 010195 &THEN 
    DEFINE BUFFER hat_auth_provider FOR hat_auth_provider.
    
    IF ipdAuthObj <> 0 AND ipdAuthObj <> ?
    THEN DO:
      FOR EACH hat_auth_provider NO-LOCK
         WHERE hat_auth_provider.auth_obj = ipdAuthObj
            BY hat_auth_provider.provider_sequence:
         
        ASSIGN cSequencelist = cSequencelist + STRING(hat_auth_provider.provider_sequence) + "=":U + STRING(hat_auth_provider.provider_sequence) + "|":U.
      END. /* FOR EACH hat_auth_provider NO-LOCK */
    END. /* IF ipdAuthObj <> 0 AND ipdAuthObj <> ? */

  &ENDIF /*&IF {&DBDFMA} >= 010195 &THEN */
  
  RETURN TRIM(cSequencelist, "|"). /* Function return value. */

  { mip/inc/mipcatcherror.i }
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

