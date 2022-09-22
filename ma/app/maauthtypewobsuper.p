&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
" This code is based on the cgi-wrapper template as designed by Progress.

  MIP Holdings (Pty) Ltd.

  Use this template to create a new Custom CGI Wrapper Procedure and write WebSpeed code that dynamically generates HTML. No associated static HTML file is needed."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Filename    : ma/app/maauthtypewobsuper.p
  Purpose     : Maintain Authorisation Types
  Description : Maintain Authorisation Types
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

/* This helps to ensure proper clean-up */
CREATE WIDGET-POOL.

/* WarpSpeed's Shared Definitions */
{ mip/inc/mipdefshared.i }

{ sysadmma.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation}

{ ma/inc/maauthtypeds.i }
{ ma/inc/maauthruleds.i }
                         
{ ma/inc/maaudittt.i }
{ ma/inc/maagerangett.i }

&IF {&DBDFMA} >= 10195 &THEN
  {ma/inc/maclinicaldocds.i}
&ENDIF              
                         
{ ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_error"}

/* Variables commonly used by WarpSpeed */
DEFINE VARIABLE goWob                  AS cls.mipwswob            NO-UNDO.

/* Variables for this specific WOB */
DEFINE VARIABLE gcFormat               AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcSearchMethod         AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcSelected             AS CHARACTER               NO-UNDO.
DEFINE VARIABLE glEnquiryWob           AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE glSuccess              AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE glReasonPerStatus      AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE gcAuthTypeGroups       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcStatusKeys           AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcDetails              AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcHeaderValues         AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcClaimType            AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcErrorMessage         AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gdAuthTypeProviderObj  AS DECIMAL                 NO-UNDO.
                                                                  
/* Containers */
DEFINE VARIABLE goCntSearchFilter      AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntSearchResults     AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntMaint             AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntAudit             AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntTypeControl       AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntAuthTypeProvider  AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntAuthTypeDetail    AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntQuestionnaire     AS cls.mipwscontainer      NO-UNDO.

DEFINE VARIABLE goPrTypes              AS cls.mipwscontrol        NO-UNDO.
DEFINE VARIABLE goExclusionPrTypes     AS cls.mipwscontrol        NO-UNDO.

DEFINE TEMP-TABLE tt_sequence
  FIELD sequence_link_obj       AS DECIMAL
  FIELD sequence_value          AS INTEGER.

DEFINE TEMP-TABLE tt_deleted
  FIELD owning_obj           AS DECIMAL
  FIELD owning_key           AS CHARACTER.

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnGetUserCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnGetUserCodes Procedure 
FUNCTION fnGetUserCodes RETURNS CHARACTER
  ( INPUT  ipcObjList      AS CHARACTER,
    INPUT  ipcMnemonic     AS CHARACTER,
    INPUT  ipcCodeList     AS CHARACTER)  FORWARD.

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
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 20.14
         WIDTH              = 52.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/*------------------------------------------------------------------------------
  This is a WarpSpeed Warpspeed - include ws/inc/wsstructure.i, and nothing else.
------------------------------------------------------------------------------*/
  
  ASSIGN goWob = Warpspeed:CurrentObject.
  
  { mip/inc/mipcatcherror.i }

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveAuthTypeControls) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveAuthTypeControls Procedure 
PROCEDURE ajaxSaveAuthTypeControls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
  
  DEFINE VARIABLE oAuthTypeControl       AS cls.maauthtypecontrol      NO-UNDO.
                                         
  DEFINE VARIABLE oRestriction           AS cls.maauthtype             NO-UNDO.
  DEFINE VARIABLE oRequestHelper         AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper        AS cls.maajaxresponsehelper   NO-UNDO.
                                         
  DEFINE VARIABLE hErrorHandle           AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cContainerCode         AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAction                AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction          AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE iLineNumber            AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE dAuthTypeControlObj    AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dInsurerObj            AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dEffectiveDate         AS DATE                       NO-UNDO.
  DEFINE VARIABLE dEndDate               AS DATE                       NO-UNDO.
  DEFINE VARIABLE lSuccess               AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE cAuthTypes             AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE iCount                 AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE cUserObjs              AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cObjlist               AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cUsers                 AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cControleIndicatorType AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cDefaultAuthStatus     AS CHARACTER                  NO-UNDO INITIAL "".
  DEFINE VARIABLE cClaimCode             AS CHARACTER                  NO-UNDO.
    
  ASSIGN
    cContainerCode         = ipcValidationArgument 
    
    oRequestHelper         = NEW cls.maajaxrequesthelper (INPUT GET-VALUE('FldLst'), INPUT GET-VALUE('ValList'))
    oResponseHelper        = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
    oAuthTypeControl       = NEW cls.maauthtypecontrol()
    oRestriction           = NEW cls.maauthtype()
    
    cRecordAction          = oRequestHelper:getFieldValue("fcAction":U                   + cContainerCode)
    dAuthTypeControlObj    = DECIMAL(oRequestHelper:getFieldValue("fcDetailObj":U        + cContainerCode))
    lSuccess               = oAuthTypeControl:focusAuthTypeControl(dAuthTypeControlObj)
    cObjlist               = REPLACE( oRequestHelper:getFieldValue("fcRestrictionObjs":U + cContainerCode), "[-Cma-]", ",")
    cUserObjs              = REPLACE( oRequestHelper:getFieldValue("fcUserObjs":U        + cContainerCode), "[-Cma-]", ",")
    cControleIndicatorType = oRequestHelper:getFieldValue("fcControlIndicatorType":U     + cContainerCode)
    NO-ERROR.

  IF cControleIndicatorType = "ma_acAuthControlTypeIndicatorExcl":U 
  THEN
    ASSIGN cDefaultAuthStatus = "6":U.

  IF cUserObjs <> ""
  THEN DO iCount = 1 TO NUM-ENTRIES(cUserObjs) :
    mipEnv:miDBEntity:focusTable("mimus"). 
    mipEnv:miDBEntity:findRecord(DECIMAL(ENTRY(iCount, cUserObjs))).
   
    IF mipEnv:miDBEntity:RecordAvailable 
    THEN ASSIGN cUsers = trim(cUsers) + trim(mipEnv:miDBEntity:RecordCode) + ",":U. 
  END.  // IF cUserObjs <> ""
  ELSE cUsers = REPLACE( oRequestHelper:getFieldValue("fcUsagePeriodOver":U + cContainerCode), "[-Cma-]", ",").
  
  ASSIGN cUsers = TRIM(cUsers, ",").
  
  IF cObjlist <> ""
  THEN DO iCount = 1 TO NUM-ENTRIES(cObjlist):
    oRestriction:focusAuthType(DECIMAL(ENTRY(iCount, cObjlist))).
    IF oRestriction:AuthTypeInFocus 
    THEN ASSIGN cAuthTypes = TRIM(cAuthTypes) + TRIM(oRestriction:AuthType) + ",":U.
    
  END.  // IF cObjlist <> ""
  ELSE 
    ASSIGN cAuthTypes = REPLACE( oRequestHelper:getFieldValue("fcRestrictions":U + cContainerCode), "[-Cma-]", ",").
          
  ASSIGN 
    cAuthTypes = TRIM(cAuthTypes, ",").
  
  IF cRecordAction <> "delete":U 
  THEN
    ASSIGN
      oAuthTypeControl:LineNumber               = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U           + cContainerCode))
      oAuthTypeControl:AuthTypeObj              = DECIMAL(oRequestHelper:getFieldValue("fcAuthTypeObjArgument":U  + cContainerCode))     
      oAuthTypeControl:ClaimCodesHeader         =         oRequestHelper:getFieldValue("fcClaimCode":U            + cContainerCode)
      oAuthTypeControl:ClaimTypesHeader         =         oRequestHelper:getFieldValue("fcClaimType":U            + cContainerCode)
      oAuthTypeControl:Period                   = INTEGER(oRequestHelper:getFieldValue("fcPeriod":U               + cContainerCode))
      oAuthTypeControl:PeriodType               =         oRequestHelper:getFieldValue("fcPeriodType":U           + cContainerCode)
      oAuthTypeControl:AmountAuth               = DECIMAL(oRequestHelper:getFieldValue("fcAmount":U               + cContainerCode))
      oAuthTypeControl:QuantityAuth             = INTEGER(oRequestHelper:getFieldValue("fcQuantity":U             + cContainerCode))
      oAuthTypeControl:OptionCode               = INTEGER(oRequestHelper:getFieldValue("fcOption":U               + cContainerCode))
      oAuthTypeControl:InsurerObj               = DECIMAL(oRequestHelper:getFieldValue("fcInsurer":U              + cContainerCode))
      oAuthTypeControl:ClaimCodesDisallow       = REPLACE(oRequestHelper:getFieldValue("fcClaimCodeDis":U         + cContainerCode), "[-Cma-]", ",")
      oAuthTypeControl:ClaimTypesDisallow       = REPLACE(oRequestHelper:getFieldValue("fcClaimTypeDis":U         + cContainerCode), "[-Cma-]", ",")
      oAuthTypeControl:UsageType                =         oRequestHelper:getFieldValue("fcUsageType":U            + cContainerCode)
      oAuthTypeControl:UsageQuantity            = INTEGER(oRequestHelper:getFieldValue("fcUsageQuantity":U        + cContainerCode))
      oAuthTypeControl:UsagePeriod              = INTEGER(oRequestHelper:getFieldValue("fcUsagePeriod":U          + cContainerCode))
      oAuthTypeControl:UsagePeriodType          =         oRequestHelper:getFieldValue("fcUsagePeriodType":U      + cContainerCode)
      oAuthTypeControl:UsageOverrideUser        =         cUsers
      oAuthTypeControl:AuthTypeRestrictions     =         cAuthTypes 
      oAuthTypeControl:ControlTypeIndicator     =         oRequestHelper:getFieldValue("fcControlIndicatorType":U + cContainerCode)

      oAuthTypeControl:gender                   =         oRequestHelper:getFieldValue("fcGender":U               + cContainerCode)
      oAuthTypeControl:AgeRangeMaleObj          = DECIMAL(oRequestHelper:getFieldValue("fdAgeRangeMaleObj":U      + cContainerCode))
      oAuthTypeControl:AgeRangeFemaleObj        = DECIMAL(oRequestHelper:getFieldValue("fdAgeRangeFemaleObj":U    + cContainerCode))
      oAuthTypeControl:AgeRangeBothObj          = DECIMAL(oRequestHelper:getFieldValue("fdAgeRangeBothObj":U      + cContainerCode))

      
   
      oAuthTypeControl:DefaultAuthStatus        = IF cDefaultAuthStatus <> "" THEN cDefaultAuthStatus ELSE oRequestHelper:getFieldValue("fcDefaultAuthStatus":U    + cContainerCode)
      oAuthTypeControl:DefaultAuthStatusNote    =         oRequestHelper:getFieldValue("fcReasonKey":U    + cContainerCode)

      oAuthTypeControl:PeriodOverride           = (IF oRequestHelper:getFieldValue("fcPeriodOverride":U            + cContainerCode) <> "":U THEN 
                                                     LOGICAL(oRequestHelper:getFieldValue("fcPeriodOverride":U + cContainerCode))
                                                   ELSE FALSE)
                                              
      oAuthTypeControl:ActivateAuthorisedValues = (IF oRequestHelper:getFieldValue("fcActivateAuthorised":U        + cContainerCode) <> "":U THEN
                                                     LOGICAL (oRequestHelper:getFieldValue("fcActivateAuthorised":U        + cContainerCode))
                                                   ELSE ?)
                                               
      oAuthTypeControl:EnforceAuthorisedValues  = (IF oRequestHelper:getFieldValue("fcEnforceAthorised":U          + cContainerCode) <> "":U THEN 
                                                    LOGICAL(oRequestHelper:getFieldValue("fcEnforceAthorised":U          + cContainerCode))
                                                   ELSE ?)
    
      oAuthTypeControl:EffectiveDate            = (IF     oRequestHelper:getFieldValue("fcEffectDate":U           + cContainerCode) <> "yyyy/mm/dd":U
                                                  THEN DATE(oRequestHelper:getFieldValue("fcEffectDate":U         + cContainerCode))
                                                  ELSE ?)
                                                
      oAuthTypeControl:EndDate                  = (IF oRequestHelper:getFieldValue("fcEndDate":U + cContainerCode) <> "yyyy/mm/dd":U
                                                  THEN DATE(oRequestHelper:getFieldValue("fcEndDate":U + cContainerCode))
                                                  ELSE ?)
                                              
    NO-ERROR.
  
  IF NOT {&ErrorStatus} THEN
  DO:
  
    CASE cRecordAction:
      WHEN "modify":U THEN
      DO: 
        ASSIGN cClaimCode = STRING(oAuthTypeControl:ClaimCodesHeader)
               lSuccess   = oAuthTypeControl:saveAuthType() NO-ERROR.               
        
        IF NOT {&ErrorStatus} AND NOT oAuthTypeControl:ErrorObject:ErrorsExist 
        THEN
          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U 
            lSuccess                        = oResponseHelper:addFieldValue("fcDetailObj":U + cContainerCode, STRING(oAuthTypeControl:AuthTypeControlObj))
            lSuccess                        = oResponseHelper:addFieldValue("fcClaimCode":U + cContainerCode, cClaimCode)
            lSuccess                        = oResponseHelper:addFieldValue("fcClaimType":U + cContainerCode, oAuthTypeControl:ClaimTypesHeader)
           NO-ERROR.            
            
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN
      DO:               
        ASSIGN lSuccess = oAuthTypeControl:focusAuthTypeControl(dAuthTypeControlObj) NO-ERROR.
         
        IF NOT {&ErrorStatus} AND NOT oAuthTypeControl:AuthTypeControlInFocus 
        THEN  
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U 
            NO-ERROR.
        ELSE 
          ASSIGN lSuccess = oAuthTypeControl:removeAuthTypeControl() NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthTypeControl:ErrorObject:ErrorsExist
        THEN
          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully removed":U
            oResponseHelper:ReturnValue     = "Record successfully removed":U 
            NO-ERROR.        
      END. /* WHEN "delete":U THEN */
      
      OTHERWISE
      DO:
        ASSIGN 
          oResponseHelper:RequestValid    = FALSE
          oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported":U, cAction)
          oResponseHelper:ResponseMessage = "Unable to perform action":U 
         NO-ERROR.
      END. /* OTHERWISE */       
    END CASE.
  END. /*IF NOT {&ErrorStatus} THEN*/
  
  IF {&ErrorStatus} OR oAuthTypeControl:ErrorObject:ErrorsExist THEN
  DO:
    ASSIGN 
      oResponseHelper:RequestValid    = FALSE
      
      hErrorHandle                    = oAuthTypeControl:ErrorObject:getErrorTableHandle()
      
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
      lSuccess                        = oResponseHelper:setError(hErrorHandle)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
  
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
  END.   /*IF oUserFlag:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/ 
  
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)   THEN DELETE OBJECT oRequestHelper   NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper)  THEN DELETE OBJECT oResponseHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthTypeControl) THEN DELETE OBJECT oAuthTypeControl NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oRestriction)     THEN DELETE OBJECT oRestriction     NO-ERROR. ~{mip/inc/mipmessageerror.i~}" } 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxSaveAuthTypeDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveAuthTypeDetail Procedure 
PROCEDURE ajaxSaveAuthTypeDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE oAuthTypeDetail      AS cls.maauthtypedetail       NO-UNDO.
  
  DEFINE VARIABLE oRequestHelper       AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper      AS cls.maajaxresponsehelper   NO-UNDO.
  
  DEFINE VARIABLE hErrorHandle         AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cContainerCode       AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAction              AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction        AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE dAuthTypeDetailObj   AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                    NO-UNDO.
  
  ASSIGN
    cContainerCode         = ipcValidationArgument
    oRequestHelper         = NEW cls.maajaxrequesthelper (INPUT GET-VALUE('FldLst'), INPUT GET-VALUE('ValList'))
    oResponseHelper        = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
    oAuthTypeDetail        = NEW cls.maauthtypedetail()
    
    cRecordAction          =         oRequestHelper:getFieldValue("fcAction":U            + cContainerCode)
    dAuthTypeDetailObj     = DECIMAL(oRequestHelper:getFieldValue("fdAuthTypeDetailObj":U + cContainerCode))
   NO-ERROR.

  ASSIGN
    dAuthTypeDetailObj     = IF dAuthTypeDetailObj = 0.00 THEN -99  ELSE dAuthTypeDetailObj
    lSuccess               = oAuthTypeDetail:focusAuthTypeDetail(dAuthTypeDetailObj)
  NO-ERROR.
  
   IF cRecordAction <> "delete":U THEN
   DO:
     oAuthTypeDetail:focusAuthTypeDetail(dAuthTypeDetailObj) NO-ERROR.
     ASSIGN  
       oAuthTypeDetail:LineNumber               = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U                                 + cContainerCode))
       oAuthTypeDetail:DefaultAuthStatus        =         oRequestHelper:getFieldValue("fcAuthDefaultStatus":U                          + cContainerCode)                                             
       oAuthTypeDetail:DefaultAuthStatusNote    =         oRequestHelper:getFieldValue("fcAuthStatusNote":U                             + cContainerCode)                                             
       oAuthTypeDetail:DetailTypeIndicator      =         oRequestHelper:getFieldValue("fcControlTypeIndicator":U                       + cContainerCode) 
                                                 
       oAuthTypeDetail:AuthTypeDetailObj        = dAuthTypeDetailObj 
       oAuthTypeDetail:AuthTypeObj              = DECIMAL(oRequestHelper:getFieldValue("fcAuthTypeObjArgument":U                        + cContainerCode))
       oAuthTypeDetail:AuthTypeProviderObj      = DECIMAL(oRequestHelper:getFieldValue("fdAuthTypeProviderObjArgument":U                + cContainerCode))
       oAuthTypeDetail:OwningEntityMnemonic     =         oRequestHelper:getFieldValue("fcOEM":U                                        + cContainerCode)
       oAuthTypeDetail:OwningObj                = DECIMAL(oRequestHelper:getFieldValue("fdOwningObj":U                                  + cContainerCode))
       oAuthTypeDetail:OwningKey                =         oRequestHelper:getFieldValue("fcOwningKey":U                                  + cContainerCode)
       oAuthTypeDetail:OwningAltValue           =         oRequestHelper:getFieldValue("fcAltValueKey":U                                + cContainerCode)
       oAuthTypeDetail:DetailTypeIndicator      =         oRequestHelper:getFieldValue("fcControlTypeIndicator":U                       + cContainerCode)
       oAuthTypeDetail:PrType                   = INTEGER(oRequestHelper:getFieldValue("fcPrTypeArgument":U                             + cContainerCode))
       oAuthTypeDetail:QuantityAuth             = DECIMAL(oRequestHelper:getFieldValue("fdQuantityAuth":U                               + cContainerCode))
       
       oAuthTypeDetail:AuthUsageLimit           = INTEGER(oRequestHelper:getFieldValue("fcAuthUsageLimit":U                             + cContainerCode)) 
       
       oAuthTypeDetail:AuthAutoCreate           =     NOT oRequestHelper:getFieldValue("fcAutoCreate":U                                 + cContainerCode) = "":U
                                                      
                                                  
       oAuthTypeDetail:DefaultLineRestriction   =         oRequestHelper:getFieldValue("fcDefaultLineRestriction":U                     + cContainerCode)  
                                                                                                                        
       oAuthTypeDetail:EffectiveDate            = (IF     oRequestHelper:getFieldValue("fcEffectiveDate":U                              + cContainerCode) <> "yyyy/mm/dd":U
                                                     THEN DATE(oRequestHelper:getFieldValue("fcEffectiveDate":U                         + cContainerCode))
                                                     ELSE ?)                                                               
                                                                                                                           
       oAuthTypeDetail:EndDate                  = (IF     oRequestHelper:getFieldValue("fcEndDate":U                                    + cContainerCode) <> "yyyy/mm/dd":U
                                                     THEN DATE(oRequestHelper:getFieldValue("fcEndDate":U                               + cContainerCode))
                                                     ELSE ?)                                              
                                                   
      NO-ERROR.
      
   END. /* IF cRecordAction <> "delete":U THEN */

   IF NOT {&ErrorStatus} THEN
   DO:
     CASE cRecordAction:
       WHEN "MODIFY":U THEN
       DO:
         ASSIGN lSuccess = oAuthTypeDetail:saveAuthType() NO-ERROR.  
         
         IF NOT {&ErrorStatus} AND NOT oAuthTypeDetail:ErrorObject:ErrorsExist 
         THEN 
           ASSIGN 
             oResponseHelper:RequestValid    = TRUE
             oResponseHelper:ResponseMessage = "Record successfully saved"  
             lSuccess                        = oResponseHelper:addFieldValue("fdAuthTypeDetailObj":U           + cContainerCode, STRING(oAuthTypeDetail:AuthTypeDetailObj), TRUE)
             lSuccess                        = oResponseHelper:addFieldValue("fdAuthTypeProviderObjArgument":U + cContainerCode, STRING(oAuthTypeDetail:AuthTypeProviderObj), TRUE)
            NO-ERROR.            
         
         IF oAuthTypeDetail:ErrorObject:WarningsExist THEN
         DO:
           ASSIGN 
             hErrorHandle                    = oAuthTypeDetail:ErrorObject:getErrorTableHandle()
             lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
             lSuccess                        = oResponseHelper:setError(hErrorHandle).
         END. /* IF oAuthTypeProvider:ErrorObject:WarningsExist */
       END. /* WHEN "modify":U THEN */
       
       WHEN "DELETE":U THEN
       DO:               
         ASSIGN lSuccess = oAuthTypeDetail:focusAuthTypeDetail(dAuthTypeDetailObj) NO-ERROR.
          
         IF NOT {&ErrorStatus} AND NOT oAuthTypeDetail:AuthTypeDetailInFocus 
         THEN  
           ASSIGN 
             oResponseHelper:RequestValid    = FALSE
             oResponseHelper:ResponseMessage = "Record could not be deleted"
             NO-ERROR.
         ELSE 
           ASSIGN lSuccess = oAuthTypeDetail:removeAuthTypeDetail() NO-ERROR.
         
         IF NOT {&ErrorStatus} AND NOT oAuthTypeDetail:ErrorObject:ErrorsExist
         THEN
           ASSIGN 
             oResponseHelper:RequestValid    = TRUE
             oResponseHelper:ResponseMessage = "Record successfully removed"
             oResponseHelper:ReturnValue     = "Record successfully removed" 
             NO-ERROR.        
       END. /* WHEN "delete":U THEN */
       
       OTHERWISE
       DO:
         ASSIGN 
           oResponseHelper:RequestValid    = FALSE
           oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported", cAction)
           oResponseHelper:ResponseMessage = "Unable to perform action" 
          NO-ERROR.
       END. /* OTHERWISE */       
     END CASE.
   END. /* IF NOT {&ErrorStatus} THEN */
   
   IF {&ErrorStatus} OR oAuthTypeDetail:ErrorObject:ErrorsExist THEN
   DO:
     ASSIGN 
       oResponseHelper:RequestValid    = FALSE
       
       hErrorHandle                    = oAuthTypeDetail:ErrorObject:getErrorTableHandle()
       
       lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
       lSuccess                        = oResponseHelper:setError(hErrorHandle)
       
       oResponseHelper:ResponseMessage = 'Unable to perform action'
       oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
   
     { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
   END.   /*IF oUserFlag:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/ 
   
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthTypeDetail) THEN DELETE OBJECT oAuthTypeDetail NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxSaveAuthTypeProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveAuthTypeProvider Procedure 
PROCEDURE ajaxSaveAuthTypeProvider :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE oAuthTypeProvider    AS cls.maauthtypeprovider    NO-UNDO.
  
  DEFINE VARIABLE oRequestHelper       AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper      AS cls.maajaxresponsehelper   NO-UNDO.
  
  DEFINE VARIABLE hErrorHandle         AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cContainerCode       AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAction              AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction        AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE dAuthTypeProviderObj AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                    NO-UNDO.
  
  ASSIGN
    cContainerCode         = ipcValidationArgument
    oRequestHelper         = NEW cls.maajaxrequesthelper (INPUT GET-VALUE('FldLst'), INPUT GET-VALUE('ValList'))
    oResponseHelper        = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
    oAuthTypeProvider      = NEW cls.maauthtypeprovider()
    
    cRecordAction          = oRequestHelper:getFieldValue("fcAction":U                   + cContainerCode)
    dAuthTypeProviderObj   = DECIMAL(oRequestHelper:getFieldValue("fcDetailObj":U        + cContainerCode)).

  ASSIGN
    dAuthTypeProviderObj   = IF dAuthTypeProviderObj = 0.00 THEN -99  ELSE dAuthTypeProviderObj
    lSuccess               = oAuthTypeProvider:focusAuthTypeProvider(dAuthTypeProviderObj)
   NO-ERROR.
  
   IF cRecordAction <> "delete":U THEN
   DO:
     
     ASSIGN    
       oAuthTypeProvider:LineNumber               = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U                      + cContainerCode))                       
       oAuthTypeProvider:DocNumMandatory          = IF oRequestHelper:getFieldValue("fcDocNumMandatory":U                      + cContainerCode) <> "":U
                                                    THEN LOGICAL(oRequestHelper:getFieldValue("fcDocNumMandatory":U            + cContainerCode))                                                              
                                                    ELSE ?                                                             
       oAuthTypeProvider:DefaultAuthStatus        =         oRequestHelper:getFieldValue("fcDefaultStatus":U                   + cContainerCode)                                             
       oAuthTypeProvider:DefaultAuthStatusNote    =         oRequestHelper:getFieldValue("fcReasonKey":U                       + cContainerCode)                                             
       oAuthTypeProvider:DefaultAuthStatusUpdUser = fnGetUserCodes(INPUT oRequestHelper:getFieldValue("fcStatusUserObjs":U     + cContainerCode), 
                                                                   INPUT "mimus":U, 
                                                                   INPUT oRequestHelper:getFieldValue("fcStatusUsers":U        + cContainerCode))                                                   
       oAuthTypeProvider:DefaultAuthStatusUpdRole = fnGetUserCodes(INPUT oRequestHelper:getFieldValue("fcStatusRoleObjs":U     + cContainerCode), 
                                                                   INPUT "mimro":U,
                                                                   INPUT oRequestHelper:getFieldValue("fcStatusRoles":U        + cContainerCode))                                                   
                                                    
       oAuthTypeProvider:AuthTypeProviderObj      = DECIMAL(oRequestHelper:getFieldValue("fcDetailObj":U                       + cContainerCode))    
       oAuthTypeProvider:AuthTypeObj              = DECIMAL(oRequestHelper:getFieldValue("fcAuthTypeObjArgument":U             + cContainerCode))
       oAuthTypeProvider:InsurerObj               = DECIMAL(oRequestHelper:getFieldValue("fcInsurer":U                         + cContainerCode))
       oAuthTypeProvider:OptionCode               = INTEGER(oRequestHelper:getFieldValue("fcOption":U                          + cContainerCode))
       oAuthTypeProvider:ProviderType             =         oRequestHelper:getFieldValue("fcProviderType":U                    + cContainerCode)
                                                                                                                           
       oAuthTypeProvider:EffectiveDate            = (IF     oRequestHelper:getFieldValue("ftEffectiveDate":U                   + cContainerCode) <> "yyyy/mm/dd":U
                                                     THEN DATE(oRequestHelper:getFieldValue("ftEffectiveDate":U                + cContainerCode))
                                                     ELSE ?)                                                               
                                                                                                                           
       oAuthTypeProvider:EndDate                  = (IF     oRequestHelper:getFieldValue("ftEndDate":U                         + cContainerCode) <> "yyyy/mm/dd":U
                                                     THEN DATE(oRequestHelper:getFieldValue("ftEndDate":U                      + cContainerCode))
                                                     ELSE ?)                                                               
                                                                                                                           
       oAuthTypeProvider:Mandatory                = (IF      oRequestHelper:getFieldValue("fcMandatory":U                       + cContainerCode) <> "":U
                                                     THEN LOGICAL (oRequestHelper:getFieldValue("fcMandatory":U                 + cContainerCode))                                                              
                                                     ELSE ? )                                                           
                                                                                                                           
       oAuthTypeProvider:NumberProvidersAllowed   = INTEGER(oRequestHelper:getFieldValue("fcNumProviderAllowed":U              + cContainerCode))
                                                                                                                           
       oAuthTypeProvider:MainProvider             = (IF      oRequestHelper:getFieldValue("fcMainProvider":U                    + cContainerCode) <> "":U
                                                     THEN LOGICAL(oRequestHelper:getFieldValue("fcMainProvider":U                    + cContainerCode))                                                             
                                                     ELSE ? )                                                            
                                                                                                                           
       oAuthTypeProvider:AuthorisedService        = (IF      oRequestHelper:getFieldValue("fcAuthorisedService":U               + cContainerCode) <> "":U
                                                     THEN LOGICAL(oRequestHelper:getFieldValue("fcAuthorisedService":U               + cContainerCode))                                                              
                                                     ELSE ? )                                                            
                                                                                                                           
       oAuthTypeProvider:ClaimCodesProvider       = REPLACE(oRequestHelper:getFieldValue("fcClaimCodesProvider":U              + cContainerCode), "[-Cma-]", ",")
       oAuthTypeProvider:ClaimTypesProvider       = REPLACE(oRequestHelper:getFieldValue("fcClaimTypesCombo":U                 + cContainerCode), "[-Cma-]", ",")
       oAuthTypeProvider:DefaultClaimCodeDetail   = INTEGER(oRequestHelper:getFieldValue("fcDefaultClaimCodeDetail":U          + cContainerCode))
       oAuthTypeProvider:DefaultClaimTypeDetail   =         oRequestHelper:getFieldValue("fcDefaultClaimTypeDetail":U          + cContainerCode)
       
       oAuthTypeProvider:ProviderSequence         = INTEGER(oRequestHelper:getFieldValue("fiProviderSeq":U                     + cContainerCode))
       oAuthTypeProvider:AmountAuth               = DECIMAL(oRequestHelper:getFieldValue("fdAuthorisedAmount":U                + cContainerCode))
       oAuthTypeProvider:QuantityAuth             = INTEGER(oRequestHelper:getFieldValue("fiQuantityAuth":U                    + cContainerCode))
       oAuthTypeProvider:HeaderValuesAllowed      =         oRequestHelper:getFieldValue("fcHeaderValuesAllowed":U             + cContainerCode)
       oAuthTypeProvider:HeaderValuesUnlimited    = (IF      oRequestHelper:getFieldValue("fcHeaderValuesUnlimited":U          + cContainerCode) <> "":U
                                                     THEN LOGICAL(oRequestHelper:getFieldValue("fcHeaderValuesUnlimited":U     + cContainerCode))                                                              
                                                     ELSE ? )                                                        
   
       oAuthTypeProvider:NegotiationGroup         = INTEGER(oRequestHelper:getFieldValue("fcNegGroup":U                        + cContainerCode))

       oAuthTypeProvider:EnforceClaimCodeMatch    = (IF      oRequestHelper:getFieldValue("fcEnforceCCMatch":U                 + cContainerCode) <> "":U
                                                     THEN LOGICAL(oRequestHelper:getFieldValue("fcEnforceCCMatch":U            + cContainerCode))                                                              
                                                     ELSE FALSE ) 
       oAuthTypeProvider:EnforceClaimTypeMatch    = (IF      oRequestHelper:getFieldValue("fcEnforceCTMatch":U                 + cContainerCode) <> "":U
                                                     THEN LOGICAL(oRequestHelper:getFieldValue("fcEnforceCTMatch":U            + cContainerCode))                                                              
                                                     ELSE FALSE ) 

       oAuthTypeProvider:AuthoriseDetailLines     =         oRequestHelper:getFieldValue("fcAuthDetaiLines":U                  + cContainerCode)
       oAuthTypeProvider:ClaimCodesDisallow       = REPLACE(oRequestHelper:getFieldValue("fcClaimCodesDisallow":U              + cContainerCode), "[-Cma-]", ",")
       oAuthTypeProvider:ClaimTypesDisallow       = REPLACE(oRequestHelper:getFieldValue("fcClaimTypesDisallowCombo":U         + cContainerCode), "[-Cma-]", ",")
       oAuthTypeProvider:AuthGroupObj              = DECIMAL(oRequestHelper:getFieldValue("fcAuthGroup":U                      + cContainerCode))
       oAuthTypeProvider:AuthAutoCreate           = (IF      oRequestHelper:getFieldValue("fcAuthAutoCreate":U                 + cContainerCode) <> "":U
                                                     THEN LOGICAL(oRequestHelper:getFieldValue("fcAuthAutoCreate":U            + cContainerCode))                                                              
                                                     ELSE FALSE ) 
       oAuthTypeProvider:PrTypeList               = REPLACE(oRequestHelper:getFieldValue("fcPrTypes":U                         + cContainerCode), "[-Cma-]", ",")
       oAuthTypeProvider:PrTypeValidList          = REPLACE(oRequestHelper:getFieldValue("fcPrTypeValidList":U                 + cContainerCode), "[-Cma-]", ",")
       oAuthTypeProvider:ProviderTypeIndicator    =         oRequestHelper:getFieldValue("fcomProviderTypeIndicator":U           + cContainerCode)      

       oAuthTypeProvider:ValidClaimCodesDetail    = REPLACE(oRequestHelper:getFieldValue("fcValidClaimCodesDetail":U           + cContainerCode), "[-Cma-]", ",")
       oAuthTypeProvider:ValidClaimTypesDetail    = REPLACE(oRequestHelper:getFieldValue("fcValidClaimTypesDetailCombo":U      + cContainerCode), "[-Cma-]", ",")

       oAuthTypeProvider:BaseRateUpdUser          = IF   oRequestHelper:getFieldValue("fcBaseRateUpdUser":U  + cContainerCode) = "":U THEN "":U
                                                    ELSE fnGetUserCodes(INPUT oRequestHelper:getFieldValue("fcBaseRateUpdUserObj":U  + cContainerCode), 
                                                                        INPUT "mimus":U, 
                                                                        INPUT oRequestHelper:getFieldValue("fcBaseRateUpdUser":U     + cContainerCode))                                                   
       oAuthTypeProvider:BaseRateUpdRole          = IF   oRequestHelper:getFieldValue("fcBaseRateUpdRole":U  + cContainerCode) = "":U THEN "":U
                                                    ELSE fnGetUserCodes(INPUT oRequestHelper:getFieldValue("fcBaseRateUpdRoleObjs":U + cContainerCode), 
                                                                        INPUT "mimro":U,
                                                                        INPUT oRequestHelper:getFieldValue("fcBaseRateUpdRole":U     + cContainerCode))

       oAuthTypeProvider:ArsRateUpdUser           = IF oRequestHelper:getFieldValue("fcArsRateUpdUser":U  + cContainerCode) = "":U THEN "":U
                                                    ELSE fnGetUserCodes(INPUT oRequestHelper:getFieldValue("fcArsRateUpdUserObj":U  + cContainerCode), 
                                                                        INPUT "mimus":U,
                                                                        INPUT oRequestHelper:getFieldValue("fcArsRateUpdUser":U      + cContainerCode))                                                   
       oAuthTypeProvider:ArsRateUpdRole           = IF   oRequestHelper:getFieldValue("fcArsRateUpdRole":U  + cContainerCode) = "":U THEN "":U
                                                    ELSE fnGetUserCodes(INPUT oRequestHelper:getFieldValue("fcArsRateUpdRoleObjs":U  + cContainerCode), 
                                                                        INPUT "mimro":U,
                                                                        INPUT oRequestHelper:getFieldValue("fcArsRateUpdRole":U      + cContainerCode))                                               
                                                    
      NO-ERROR.
      
   END. /* IF cRecordAction <> "delete":U THEN */
   
   IF NOT {&ErrorStatus} THEN
   DO:
     CASE cRecordAction:
       WHEN "MODIFY":U THEN
       DO: 
         ASSIGN lSuccess = oAuthTypeProvider:saveAuthType() NO-ERROR.  
         
         IF NOT {&ErrorStatus} AND NOT oAuthTypeProvider:ErrorObject:ErrorsExist 
         THEN
           ASSIGN 
             oResponseHelper:RequestValid    = TRUE
             oResponseHelper:ResponseMessage = "Record successfully saved":U   
             lSuccess                        = oResponseHelper:addFieldValue("fcDetailObj":U         + cContainerCode, STRING(oAuthTypeProvider:AuthTypeProviderObj))
             lSuccess                        = oResponseHelper:addFieldValue("fcMainProvider":U      + cContainerCode, STRING(oAuthTypeProvider:MainProvider))
            NO-ERROR.            
         
         IF oAuthTypeProvider:ErrorObject:WarningsExist THEN
         DO:
           ASSIGN 
             hErrorHandle                    = oAuthTypeProvider:ErrorObject:getErrorTableHandle()
             lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
             lSuccess                        = oResponseHelper:setError(hErrorHandle).
         END. /* IF oAuthTypeProvider:ErrorObject:WarningsExist */
       END. /* WHEN "modify":U THEN */
       
       WHEN "DELETE":U THEN
       DO:              
         ASSIGN lSuccess = oAuthTypeProvider:focusAuthTypeProvider(dAuthTypeProviderObj) NO-ERROR.
          
         IF NOT {&ErrorStatus} AND NOT oAuthTypeProvider:AuthTypeProviderInFocus 
         THEN  
           ASSIGN 
             oResponseHelper:RequestValid    = FALSE
             oResponseHelper:ResponseMessage = "Record could not be deleted":U 
             NO-ERROR.
         ELSE 
           ASSIGN lSuccess = oAuthTypeProvider:removeAuthTypeProvider() NO-ERROR.
         
         IF NOT {&ErrorStatus} AND NOT oAuthTypeProvider:ErrorObject:ErrorsExist
         THEN
           ASSIGN 
             oResponseHelper:RequestValid    = TRUE
             oResponseHelper:ResponseMessage = "Record successfully removed":U
             oResponseHelper:ReturnValue     = "Record successfully removed":U 
             NO-ERROR.        
       END. /* WHEN "delete":U THEN */
       
       OTHERWISE
       DO:
         ASSIGN 
           oResponseHelper:RequestValid    = FALSE
           oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported":U, cAction)
           oResponseHelper:ResponseMessage = "Unable to perform action":U 
          NO-ERROR.
       END. /* OTHERWISE */       
     END CASE.
   END. /* IF NOT {&ErrorStatus} THEN */
   
   IF {&ErrorStatus} OR oAuthTypeProvider:ErrorObject:ErrorsExist THEN
   DO:
     ASSIGN 
       oResponseHelper:RequestValid    = FALSE
       
       hErrorHandle                    = oAuthTypeProvider:ErrorObject:getErrorTableHandle()
       
       lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
       lSuccess                        = oResponseHelper:setError(hErrorHandle)
       
       oResponseHelper:ResponseMessage = 'Unable to perform action':U
       oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
   
     { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
   END.   /*IF oUserFlag:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/ 
   
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)    THEN DELETE OBJECT oRequestHelper    NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper)   THEN DELETE OBJECT oResponseHelper   NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthTypeProvider) THEN DELETE OBJECT oAuthTypeProvider NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidationAuthType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationAuthType Procedure 
PROCEDURE ajaxValidationAuthType :
/*------------------------------------------------------------------------------
  Purpose   : Auth Type Container Ajax Validation    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

&IF {&DBDFMA} >= 10195 &THEN
                                        
  DEFINE VARIABLE lSuccess              AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cFilterFieldList      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFilterField          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFilterValueList      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFilterValue          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReturnFieldList      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReturnField          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReturnValues         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWhereClause          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iField                AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iFilterField          AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iReturnField          AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cStatus               AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReasonCode           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReasonType           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dStartDate            AS DATE        NO-UNDO.
  DEFINE VARIABLE dInsurerObj           AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dRecordObj            AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE lMandatory            AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cAgeRangeCode         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCodeField            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNoteKey              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cClaimCode            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFillChar             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTableField           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRecordField          AS CHARACTER   NO-UNDO.
  
  &SCOPED-DEFINE AppendReturnValues ASSIGN cReturnValues = cReturnValues + (IF cReturnValues = "":U THEN "":U ELSE "|":U) +
  
  &SCOPED-DEFINE ValidationSuccess  ASSIGN ttValidation.cReturnValues = cReturnValues ~
                                           ttValidation.lValid        = TRUE          ~
                                           ttValidation.cMessage      = "Success":U.  ~
                                                                                      ~
                                    VALIDATE ttValidation.  
  
  &SCOPED-DEFINE BlankResponse     ASSIGN ttValidation.cReturnValues = FILL("|":U, NUM-ENTRIES(cReturnFieldList) - 1) ~
                                          ttValidation.lValid        = TRUE.                                          ~
                                                                                                                      ~
                                   VALIDATE ttValidation.
                                                                   
  ASSIGN
    cFilterFieldList = GET-VALUE('FldLst')
    cFilterValueList = GET-VALUE('ValList')
    cReturnFieldList = GET-VALUE('RetFldList').
  
  
  CREATE ttValidation.
    
  
  DO iFilterField = 1 TO NUM-ENTRIES(cFilterFieldList):
  
    ASSIGN cFilterField = ENTRY(iFilterField, cFilterFieldList)
           cFilterValue = ENTRY(iFilterField, cFilterValueList).
    
    CASE cFilterField:

      WHEN "[ClaimCode]":U 
      THEN ASSIGN cClaimCode = TRIM(cFilterValue)
                  cFillChar  =  FILL("0" , 3 - LENGTH(cClaimCode))
                  cClaimCode = cFillChar + cClaimCode .

      WHEN "[NoteKey]":U 
      THEN ASSIGN cNoteKey = TRIM(cFilterValue).

      WHEN "[TableMnemonic]" 
      THEN ASSIGN cTableField = TRIM(cFilterValue).

      WHEN "[AgeRange]":U 
      THEN ASSIGN cAgeRangeCode = TRIM(cFilterValue).

      WHEN "[CodeField]":U 
      THEN ASSIGN cCodeField = TRIM(cFilterValue).

      WHEN "[RecordField]":U 
      THEN ASSIGN cRecordField = TRIM(cFilterValue).

      WHEN "[RecordObj]":U 
      THEN ASSIGN dRecordObj = DECIMAL(cFilterValue).

      WHEN "[Status]":U
      THEN ASSIGN cStatus = cFilterValue.
      
      WHEN "[ReasonCode]":U
      THEN ASSIGN cReasonCode = TRIM(cFilterValue).

      WHEN "[ReasonType]":U
      THEN ASSIGN cReasonType = TRIM(cFilterValue).
      
      WHEN "[StartDate]":U 
      THEN ASSIGN dStartDate = DATE(cFilterValue).

      WHEN "[InsurerObj]":U 
      THEN ASSIGN dInsurerObj = DECIMAL(cFilterValue).

      WHEN "[StatusReasonMandatory]":U
      THEN ASSIGN lMandatory = IF cFilterValue = "YES":U 
                               THEN TRUE ELSE FALSE.
      
    END CASE.
  
  END. /*DO iFilter = 1 TO NUM-ENTRIES(cFilterFields):*/
  
  
  CASE ipcValidationArgument:
      
    WHEN "SlentAjax":U THEN
    DO:
      IF cTableField <> "" AND cCodeField <> "" THEN
      DO:
        CASE cTableField:
          WHEN "hlmcr":U THEN
          DO:
            FIND FIRST hlm_crosswalk NO-LOCK 
              WHERE hlm_crosswalk.crosswalk_type = 'ma_acCrossTypeBasket'
              AND   hlm_crosswalk.crosswalk_code = cCodeField
            NO-ERROR.
        
            IF AVAILABLE hlm_crosswalk THEN
            DO: 
              DO iField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
          
                IF ENTRY(iField,cReturnFieldList) = "[RecordObj]":U THEN
                DO: 
                  ASSIGN cFilterValue = STRING(hlm_crosswalk.crosswalk_obj).
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                
                END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
                IF ENTRY(iField,cReturnFieldList) = "[RecordField]":U THEN
                DO: 
                  ASSIGN cFilterValue = hlm_crosswalk.crosswalk_code.
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                END.
              END.
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = TRUE
                ttValidation.cMessage      = "":U.
            END.
            ELSE
            DO: 
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = FALSE
                ttValidation.cMessage      = "Basket code entered is not valid":U.
            END.
          END.
          WHEN "hlmnl":U THEN
          DO:                   
            FIND FIRST hlm_nappi_link NO-LOCK 
              WHERE hlm_nappi_link.nappi_code_prefix = INTEGER(cCodeField)
            NO-ERROR.
        
            IF AVAILABLE hlm_nappi_link THEN
            DO:
              DO iField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
          
                IF ENTRY(iField,cReturnFieldList) = "[RecordObj]":U THEN
                DO: 
                  ASSIGN cFilterValue = STRING(hlm_nappi_link.nappi_link_obj).
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                
                END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
                IF ENTRY(iField,cReturnFieldList) = "[RecordField]":U THEN
                DO: 
                  ASSIGN cFilterValue = STRING(hlm_nappi_link.nappi_code_prefix).
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                END.
              END.
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = TRUE
                ttValidation.cMessage      = "":U.
            END.
            ELSE
            DO: 
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = FALSE
                ttValidation.cMessage      = "Nappi code entered is not valid":U.
            END.
          END.
          WHEN "htmtl":U THEN
          DO:
            FIND FIRST htm_tariff_link NO-LOCK 
              WHERE htm_tariff_link.tariff_code = cCodeField
            NO-ERROR.
        
            IF AVAILABLE htm_tariff_link THEN
            DO:
              DO iField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
          
                IF ENTRY(iField,cReturnFieldList) = "[RecordObj]":U THEN
                DO: 
                  ASSIGN cFilterValue = STRING(htm_tariff_link.tariff_link_obj).
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                
                END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
                IF ENTRY(iField,cReturnFieldList) = "[RecordField]":U THEN
                DO: 
                  ASSIGN cFilterValue = htm_tariff_link.tariff_code.
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                END.
              END.
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = TRUE
                ttValidation.cMessage      = "":U.
            END.
            ELSE
            DO: 
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = FALSE
                ttValidation.cMessage      = "Tariff code entered is not valid":U.
            END.
          END.
          WHEN "htmtt":U THEN
          DO: 
            FIND FIRST htm_tariff_type NO-LOCK 
              WHERE htm_tariff_type.tariff_type_code = cCodeField
            NO-ERROR.
        
            IF AVAILABLE htm_tariff_type THEN
            DO:
              DO iField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
          
                IF ENTRY(iField,cReturnFieldList) = "[RecordObj]":U THEN
                DO: 
                  ASSIGN cFilterValue = STRING(htm_tariff_type.tariff_type_obj).
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                
                END. 
                IF ENTRY(iField,cReturnFieldList) = "[RecordField]":U THEN
                DO: 
                  ASSIGN cFilterValue = htm_tariff_type.tariff_type_code.
                
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cFilterValue.
                END.
              END.
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = TRUE
                ttValidation.cMessage      = "":U.
            END.
            ELSE
            DO: 
              ASSIGN 
                ttValidation.cReturnValues = cReturnValues
                ttValidation.lValid        = FALSE
                ttValidation.cMessage      = "Tariff type code entered is not valid":U.
            END.
          END.
          OTHERWISE DO:
            ASSIGN 
              ttValidation.cReturnValues = cReturnValues
              ttValidation.lValid        = FALSE
              ttValidation.cMessage      = "OEM not supported":U.
          END.
         END CASE.
      END.
      ELSE DO:
        {&ValidationSuccess} 
      END.    
    END. /*WHEN "Status":U THEN*/
    WHEN "Status":U THEN
    DO:
      
      DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
      
        ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
      
        CASE cReturnField:
        
          WHEN "[StatusReasonMandatory]":U THEN
          DO:
            ASSIGN lMandatory = (IF TRIM(cStatus) = "":U
                                 THEN FALSE
                                 ELSE mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), INPUT dInsurerObj, INPUT 0, INPUT dStartDate)).
        
            {&AppendReturnValues} STRING(lMandatory). 
          END. /*WHEN "[StatusReasonMandatory]":U THEN*/
        END CASE.
      END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   
      
      {&ValidationSuccess}       
    END. /*WHEN "Status":U THEN*/
    WHEN "note":U THEN
    DO:
      IF cNoteKey <> "" THEN
      DO:
        FIND FIRST note NO-LOCK 
             WHERE note.key = cNoteKey
          NO-ERROR.
        
        IF AVAILABLE note THEN
        DO: 
    
          DO iField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
          
            IF ENTRY(iField,cReturnFieldList) = "[SchemeCode]":U THEN
            DO: 
              ASSIGN cFilterValue = STRING(note.scheme-code).
           
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + cFilterValue.
            
            END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
            IF ENTRY(iField,cReturnFieldList) = "[RecordKey]":U THEN
            DO: 
              ASSIGN cFilterValue = note.KEY.
            
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + cFilterValue.
            
            END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
            IF ENTRY(iField,cReturnFieldList) = "[RecordLabel]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(note.narration[1]). 
                               
            IF ENTRY(iField,cReturnFieldList) = "[RecordDescription]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(note.narration[1]). 
            
          END.
          
          ASSIGN 
           ttValidation.cReturnValues = cReturnValues
           ttValidation.lValid        = TRUE
           ttValidation.cMessage      = "":U. 
        END.
        ELSE
        DO: 
          ASSIGN 
            ttValidation.cReturnValues = cReturnValues
            ttValidation.lValid        = FALSE
            ttValidation.cMessage      = "Error":U.
        END.
      END.
      ELSE
      DO:
        ASSIGN 
           ttValidation.cReturnValues = FILL("|",(NUM-ENTRIES(cReturnFieldList) - 1))
           ttValidation.lValid        = TRUE
           ttValidation.cMessage      = "":U.
      END.
    END. /* WHEN "note":U THEN*/
    WHEN "ClaimCode":U THEN
    DO:
     
      IF cClaimCode <> "" THEN
      DO:
        mipEnv:miDBEntity:focusTable("ccdesc").

        ASSIGN cWhereClause = SUBSTITUTE("claim-code = &1":U, cClaimCode).
      
        mipEnv:miDBEntity:findRecordWhere(cWhereClause).
        
        IF mipEnv:miDBEntity:RecordAvailable THEN
        DO: 
          DO iField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
            IF ENTRY(iField,cReturnFieldList) = "[RecordObj]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(mipEnv:miDBEntity:RecordObj). 
            
            IF ENTRY(iField,cReturnFieldList) = "[RecordCode]":U THEN
            DO: 
              ASSIGN cFilterValue = ENTRY(2 ,mipEnv:miDBEntity:RecordCode,"|")
                     cFilterValue = FILL("0", 3 - LENGTH(cFilterValue)) + cFilterValue.
            
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + cFilterValue.
            
            END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
            IF ENTRY(iField,cReturnFieldList) = "[RecordKey]":U THEN
            DO: 
              ASSIGN cFilterValue = mipEnv:miDBEntity:RecordKey.
            
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + cFilterValue.
            
            END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
            IF ENTRY(iField,cReturnFieldList) = "[RecordLabel]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(mipEnv:miDBEntity:RecordLabel). 
                               
            IF ENTRY(iField,cReturnFieldList) = "[RecordDescription]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(mipEnv:miDBEntity:RecordDescription). 
            
          END.
          
          ASSIGN 
           ttValidation.cReturnValues = cReturnValues
           ttValidation.lValid        = TRUE
           ttValidation.cMessage      = "":U. 
        END.
        ELSE
        DO: 
          ASSIGN 
            ttValidation.cReturnValues = cReturnValues
            ttValidation.lValid        = FALSE
            ttValidation.cMessage      = "Error":U.
        END.
      END.
      ELSE
      DO:
        ASSIGN 
           ttValidation.cReturnValues = FILL("|",(NUM-ENTRIES(cReturnFieldList) - 1))
           ttValidation.lValid        = TRUE
           ttValidation.cMessage      = "":U.
      END.
    END. /* WHEN "ClaimCode":U THEN*/
    WHEN "hlmar":U THEN
    DO:
      IF cAgeRangeCode <> "" THEN
      DO:
        mipEnv:miDBEntity:focusTable(ipcValidationArgument).

        ASSIGN cWhereClause = SUBSTITUTE("age_range_code = '&1'":U, cAgeRangeCode).
        
        mipEnv:miDBEntity:findRecordWhere(cWhereClause).
        
        IF mipEnv:miDBEntity:RecordAvailable THEN
        DO: 
          DO iField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
            IF ENTRY(iField,cReturnFieldList) = "[RecordObj]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(mipEnv:miDBEntity:RecordObj). 
            
            IF ENTRY(iField,cReturnFieldList) = "[RecordCode]":U THEN
            DO: 
              ASSIGN cFilterValue = mipEnv:miDBEntity:RecordCode.
            
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + cFilterValue.
            
            END. /*IF ENTRY(iField,cReturnFields) = "[RecordCode]":U THEN*/
            IF ENTRY(iField,cReturnFieldList) = "[RecordLabel]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(mipEnv:miDBEntity:RecordLabel). 
                               
            IF ENTRY(iField,cReturnFieldList) = "[RecordDescription]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(mipEnv:miDBEntity:RecordDescription). 
            
          END.
          
          ASSIGN 
           ttValidation.cReturnValues = cReturnValues
           ttValidation.lValid        = TRUE
           ttValidation.cMessage      = "":U. 
        END.
        ELSE
        DO: 
          ASSIGN 
            ttValidation.cReturnValues = cReturnValues
            ttValidation.lValid        = FALSE
            ttValidation.cMessage      = "Error":U.
        END.
      END.
      ELSE
      DO:
        ASSIGN 
           ttValidation.cReturnValues = FILL("|",(NUM-ENTRIES(cReturnFieldList) - 1))
           ttValidation.lValid        = TRUE
           ttValidation.cMessage      = "":U.
      END.
    END.

  END CASE. /*  CASE ipcValidationArgument:*/   
      
  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-businessLogic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE businessLogic Procedure 
PROCEDURE businessLogic :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcWhatToDo AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE iRow             AS INTEGER              NO-UNDO.
  DEFINE VARIABLE cRecordAction    AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE lSuccess         AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cUsers           AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cClaimTypeUsers  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cClaimCodeUsers  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cEndDateUpdUsers AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusRoles     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cClaimTypeRoles  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cClaimCodeRoles  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cEndDateUpdRoles AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iCount           AS INTEGER              NO-UNDO.
  DEFINE VARIABLE cIcds            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE oSearch          AS cls.maauthtypesearch NO-UNDO.
  DEFINE VARIABLE cHealthOption    AS CHARACTER            NO-UNDO.
  
  DATASET dsAuthType:EMPTY-DATASET().
  
  oSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).
  
  CASE goWob:Mode:
    WHEN "Maint":U  
    THEN DO:
      
      CASE ipcWhatToDo:
      
        WHEN "Submit->Add":U    OR 
        WHEN "Submit->Change":U OR 
        WHEN "Confirm->Delete":U
        THEN DO:   
          
          IF goWob:CurrentObj <> "":U AND ipcWhatToDo <> "Submit->Add":U THEN
          DO:
            oSearch:SetFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, DECIMAL(goWob:CurrentObj)).
            
            oSearch:fetchData().
          END. /*IF goWob:CurrentObj <> "":U THEN*/
          
          FIND FIRST tt_auth_type NO-LOCK NO-ERROR.
            
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
          
          IF (ipcWhatToDo = "Submit->Add":U OR ipcWhatToDo = "Submit->Change":U) AND 
            GET-VALUE("fcAuthType":U  + goCntMaint:ContainerCode) <> "":U 
          THEN
          DO:
            IF NOT AVAILABLE tt_auth_type THEN
            DO:
              CREATE tt_auth_type.
              ASSIGN tt_auth_type.auth_type_obj = (IF ipcWhatToDo = "Submit->Add":U 
                                                   THEN 0 ELSE DECIMAL(goWob:CurrentObj)).
                                                                 
            END. /*IF NOT AVAILABLE tt_auth_type THEN*/
            ASSIGN
              cClaimCodeRoles  = fnGetUserCodes(INPUT GET-VALUE("fcClaimCodeRoleObjs":U   + goCntMaint:ContainerCode), INPUT "mimro":U, INPUT GET-VALUE("fcClaimCodeRoles":U   + goCntMaint:ContainerCode))
              cUsers           = fnGetUserCodes(INPUT GET-VALUE("fcStatusUserObjs":U      + goCntMaint:ContainerCode), INPUT "mimus":U, INPUT GET-VALUE("fcStatusUsers":U      + goCntMaint:ContainerCode))
              cClaimCodeUsers  = fnGetUserCodes(INPUT GET-VALUE("fcClaimCodeUserObjs":U   + goCntMaint:ContainerCode), INPUT "mimus":U, INPUT GET-VALUE("fcClaimCodeUsers":U   + goCntMaint:ContainerCode))
              cClaimTypeRoles  = fnGetUserCodes(INPUT GET-VALUE("fcClaimTypeRoleObjs":U   + goCntMaint:ContainerCode), INPUT "mimro":U, INPUT GET-VALUE("fcClaimTypeRoles":U   + goCntMaint:ContainerCode))
              cClaimTypeUsers  = fnGetUserCodes(INPUT GET-VALUE("fcClaimTypeUserObjs":U   + goCntMaint:ContainerCode), INPUT "mimus":U, INPUT GET-VALUE("fcClaimTypeUsers":U   + goCntMaint:ContainerCode))
              cEndDateUpdRoles = fnGetUserCodes(INPUT GET-VALUE("fcEndDateUpdRoleObjs":U  + goCntMaint:ContainerCode), INPUT "mimro":U, INPUT GET-VALUE("fcEndDateUpdRoles":U  + goCntMaint:ContainerCode))
              cEndDateUpdUsers = fnGetUserCodes(INPUT GET-VALUE("fcEndDateUpdUserObjs":U  + goCntMaint:ContainerCode), INPUT "mimus":U, INPUT GET-VALUE("fcEndDateUpdUsers":U  + goCntMaint:ContainerCode))
              cStatusRoles     = fnGetUserCodes(INPUT GET-VALUE("fcStatusRoleObjs":U      + goCntMaint:ContainerCode), INPUT "mimro":U, INPUT GET-VALUE("fcStatusRoles":U      + goCntMaint:ContainerCode)).
            IF GET-VALUE("fcIcdObjs":U  + goCntMaint:ContainerCode) <> ""
            THEN 
            DO iCount = 1 TO NUM-ENTRIES(GET-VALUE("fcIcdObjs":U  + goCntMaint:ContainerCode)) :
              
              FIND FIRST diagnos NO-LOCK
                   WHERE diagnos.diagnos-obj = DECIMAL(ENTRY(iCount, GET-VALUE("fcIcdObjs":U  + goCntMaint:ContainerCode)))
                NO-ERROR.  
                
              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
             
              IF AVAILABLE diagnos
              THEN ASSIGN cIcds = TRIM(cIcds) + TRIM(diagnos.diagnosis) + ",":U. 
            END.
            ELSE 
              ASSIGN cIcds = GET-VALUE("fcIcds":U  + goCntMaint:ContainerCode).
            
            ASSIGN cIcds = TRIM(cIcds, ",").

            IF AVAILABLE tt_auth_type
            THEN
              ASSIGN
               tt_auth_type.record_action                 = "MODIFY":U
               tt_auth_type.auth_type                     = GET-VALUE("fcAuthType":U                  + goCntMaint:ContainerCode)                                     
               tt_auth_type.auth_type_prefix              = GET-VALUE("fcPrefix":U                    + goCntMaint:ContainerCode)                        
               tt_auth_type.description                   = GET-VALUE("fcDescription":U               + goCntMaint:ContainerCode)                            
               tt_auth_type.auth_type_group               = GET-VALUE("fcTypeAcronym":U               + goCntMaint:ContainerCode)                  
               tt_auth_type.default_auth_status           = GET-VALUE("fcStatusDefault":U             + goCntMaint:ContainerCode)                      
               tt_auth_type.default_auth_status_note      = GET-VALUE("fcReasonKey":U                 + goCntMaint:ContainerCode)                            
               tt_auth_type.insurer_obj                   = DECIMAL(GET-VALUE("fdInsurerObj":U        + goCntMaint:ContainerCode))                      
               tt_auth_type.sequence_key                  = GET-VALUE("fcAuthTSeqKey":U               + goCntMaint:ContainerCode)                              
               tt_auth_type.effective_date                = DATE(GET-VALUE("fcEffectiveDate":U        + goCntMaint:ContainerCode))                                 
               tt_auth_type.end_date                      = DATE(GET-VALUE("fcEndDate":U              + goCntMaint:ContainerCode))                        
               tt_auth_type.external_auth_type_group      = GET-VALUE("fcExtType":U                   + goCntMaint:ContainerCode)            
               tt_auth_type.icd_cond_code                 = GET-VALUE("fcIcdCondition":U              + goCntMaint:ContainerCode)                     
               tt_auth_type.icd_cond_type                 = GET-VALUE("fcIcdCondType":U               + goCntMaint:ContainerCode)                              
               tt_auth_type.valid_icds                    = TRIM(cIcds, ",":U)                                 
               tt_auth_type.default_auth_status_upd_user  = cUsers                                    
               tt_auth_type.default_line_restriction      = GET-VALUE("fcDetailBase":U                + goCntMaint:ContainerCode)        
               tt_auth_type.updates_allowed               = IF GET-VALUE("chAllowUpdates":U           + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE                 
               tt_auth_type.header_values_allowed         = GET-VALUE("fcHeaderValue":U               + goCntMaint:ContainerCode)                        
               tt_auth_type.header_values_unlimited       = IF GET-VALUE("chUnlimitedHeader":U        + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE
               tt_auth_type.activate_am_pm                = IF GET-VALUE("fcActivateAmPm":U           + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE
               tt_auth_type.activate_body_region          = IF GET-VALUE("fcActivateBodyRegion":U     + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE
               tt_auth_type.activate_service_type         = IF GET-VALUE("fcActivateServiceType":U    + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE  
               tt_auth_type.activate_los                  = IF GET-VALUE("fcActivateLos":U            + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE 
               tt_auth_type.activate_due_date             = IF GET-VALUE("fcActivateDueDate":U        + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE                                                                            
               tt_auth_type.activate_episode_number       = IF GET-VALUE("fcActivateEpisodeNum":U     + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE                                                                            
               tt_auth_type.activate_mouth_part_id        = IF GET-VALUE("fcActMoouthpartId":U        + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE    
               tt_auth_type.authorise_all_services        = IF GET-VALUE("fcAuthAllService":U         + goCntMaint:ContainerCode) = "YES" THEN TRUE ELSE FALSE    
               tt_auth_type.multiple_cc_message_type      = GET-VALUE("fcMultipleCCMessageType":U     + goCntMaint:ContainerCode) 
               tt_auth_type.claim_code_upd_role           = cClaimCodeRoles 
               tt_auth_type.claim_code_upd_user           = cClaimCodeUsers
               tt_auth_type.claim_type_upd_role           = cClaimTypeRoles    
               tt_auth_type.claim_type_upd_user           = cClaimTypeUsers
               tt_auth_type.end_date_upd_role             = cEndDateUpdRoles    
               tt_auth_type.end_date_upd_user             = cEndDateUpdUsers
               tt_auth_type.default_auth_status_upd_role  = cStatusRoles

               /* Handle hard-coded ALL, NONE, RULE and selection (CUSTOM) */
               cHealthOption                              = TRIM(goCntMaint:GetControl("fcHealthOption":U  + goCntMaint:ContainerCode ):ControlValue)
               tt_auth_type.activate_health               = IF cHealthOption = "CUSTOM":U 
                                                            THEN goCntMaint:GetControl("fcActivateHealth":U + goCntMaint:ContainerCode):ControlValue 
                                                            ELSE IF cHealthOption = "RULE":U
                                                                 THEN "":U
                                                                 ELSE "[":U + cHealthOption + "]":U
               tt_auth_type.activate_crosswalk            = IF GET-VALUE("flActivateCrosswalk":U + goCntMaint:ContainerCode) = ? OR 
                                                               GET-VALUE("flActivateCrosswalk":U + goCntMaint:ContainerCode) = "":U THEN NO
                                                            ELSE LOGICAL(GET-VALUE("flActivateCrosswalk":U + goCntMaint:ContainerCode),"YES/NO" )

                tt_auth_type.activate_code_link           = IF GET-VALUE("flActivateCodeLink":U + goCntMaint:ContainerCode) = ? OR 
                                                               GET-VALUE("flActivateCodeLink":U + goCntMaint:ContainerCode) = "":U THEN NO
                                                            ELSE LOGICAL(GET-VALUE("flActivateCodeLink":U + goCntMaint:ContainerCode),"YES/NO" )

               tt_auth_type.activate_copayment            = IF GET-VALUE("flActivateCopayment":U + goCntMaint:ContainerCode) = ? OR 
                                                               GET-VALUE("flActivateCopayment":U + goCntMaint:ContainerCode) = "":U THEN NO
                                                            ELSE LOGICAL(GET-VALUE("flActivateCopayment":U + goCntMaint:ContainerCode),"YES/NO" )                                             
               
               /* 
                 When the checkbox is disabled, the control name has 'chk' added to the end
                 We need to also check the value of disabled checkboxes when assigning the 
                 values to the temp-table
               */               
               tt_auth_type.activate_penalty              = IF GET-VALUE("flActivatePenalty":U + goCntMaint:ContainerCode) = "YES"
                                                            THEN TRUE
                                                            ELSE IF GET-VALUE("flActivatePenalty":U + goCntMaint:ContainerCode + "chk") = "YES"
                                                            THEN TRUE 
                                                            ELSE FALSE
               tt_auth_type.activate_los_weekend_pass     = IF GET-VALUE("fcActivateLosWeekendPass":U + goCntMaint:ContainerCode) = "YES"
                                                            THEN TRUE
                                                            ELSE IF GET-VALUE("fcActivateLosWeekendPass":U + goCntMaint:ContainerCode + "chk") = "YES"
                                                            THEN TRUE 
                                                            ELSE FALSE
                                                            .

            VALIDATE tt_auth_type.

          END.  /* IF (ipcWhatToDo = "Submit->Add":U OR ipcWhatToDo = "Submit->Change":U) AND GET-VALUE("fcAuthType":U  + goCntMaint:ContainerCode) <> "":U THEN */
          
          IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_type THEN
          DO:
            ASSIGN tt_auth_type.record_action = "DELETE":U.
            
            VALIDATE tt_auth_type.
          END. /*IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_type THEN*/
          
&IF {&DBDFMA} >= 010195 &THEN           
          mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE). 
&ENDIF          
          
          ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_auth_type_error).
          
          IF Warpspeed:ValidationError
          THEN 
            mipEnv:Health:maUiService:setContainerErrors(TEMP-TABLE tt_auth_type_error:HANDLE, goCntMaint, "hacat":U, DECIMAL(goWob:CurrentObj), "":U).
            
          IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN
          DO:
            FIND FIRST tt_auth_type NO-LOCK NO-ERROR.
            
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
            
            ASSIGN 
               goWob:CurrentObj     = STRING(tt_auth_type.auth_type_obj)
               goWob:CurrentObj     = goWob:CurrentObj
               lSuccess             = wsUIService:setObj(goWob:ObjectCode, goWob:CurrentObj).
            
          END. /*IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN*/      
        END. /*WHEN "Submit->Add":U*/
        
        WHEN "Confirm->Delete":U THEN 
        DO:
          ASSIGN 
            goWob:CurrentObj = "[clearObj]":U
            lSuccess         = wsUiService:setObj(goWob:ObjectCode, goWob:CurrentObj)
            goWob:Mode       = "Search":U.
               
        END. /*"Confirm->Delete":U */                
      END CASE. /* CASE ipcWhatToDo: */
      
      IF goWob:SubmitValue = "MaintSubmit":U THEN
      DO:
        RUN submitControl       IN TARGET-PROCEDURE.
        RUN submitProviders     IN TARGET-PROCEDURE.
        RUN submitDetails       IN TARGET-PROCEDURE.
        RUN submitQuestionnaire IN TARGET-PROCEDURE.
        
        IF NOT WarpSpeed:ValidationError THEN 
        DO:
&IF {&DBDFMA} >= 010195 &THEN           
          mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE).
&ENDIF
        
          ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_auth_type_error NO-LOCK
                                                      WHERE tt_auth_type_error.error_type = "ERR":U).

          FOR EACH tt_auth_type_error NO-LOCK:
      
            FIND FIRST tt_deleted NO-LOCK
                 WHERE tt_deleted.owning_obj = tt_auth_type_error.owning_obj
              NO-ERROR.
              
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
            
            IF AVAILABLE tt_deleted 
            THEN DELETE tt_deleted. 
      
          END. /*FOR EACH tt_auth_type_error NO-LOCK:*/

        END.  // IF NOT WarpSpeed:ValidationError THEN
       
      END. /*IF goWob:SubmitValue = 'MaintSubmit' THEN*/
    END. /* WHEN "Maint":U */
  END CASE. /* CASE goWob:Mode: */

  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-customRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customRenderProcedure Procedure 
PROCEDURE customRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.

  DEFINE VARIABLE oAcronym   AS cls.mipacronym   NO-UNDO.
  
  DEFINE VARIABLE cObjList                          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cEntityMnemonic                   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCodes                            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lSuccess                          AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE iCount                            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOnClick                          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRow                              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cControls                         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPrType                           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj                      AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dInsurerObj                       AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dAuthTypeProviderObj              AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dStartDate                        AS DATE        NO-UNDO.
  DEFINE VARIABLE iOptionCode                       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lValidRule                        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRuleValue                        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValidValues                  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cHeaderValues                     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cAcronymCode                      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLabel                            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOEMSelection                     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lDisable                          AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cStatus                           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lMandatory                        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lExistingRecord                   AS LOGICAL     NO-UNDO INITIAL NO .
  DEFINE VARIABLE lActivateAMPM                     AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lActivateWeekend                  AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cFormattedDate                    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dtAuthTypeHeaderEffectiveDate     AS DATE        NO-UNDO.
  DEFINE VARIABLE dtAuthTypeControlRowEffectiveDate AS DATE        NO-UNDO.

  CASE ENTRY(1, ipoControl:RenderArgument, "|"):
    WHEN "PRTYPE":U THEN
    DO:
      ASSIGN 
        cRow             = REPLACE(ipoControl:InstanceName, ipoControl:ControlName, "":U)
        lExistingRecord  = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.auth_type_provider_obj":U, "BUFFER-VALUE":U)) > 0 
                           AND DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.auth_type_provider_obj":U, "BUFFER-VALUE":U)) <> ?
        .
      
      CASE ipoControl:ControlName:
        WHEN "fcPrTypeLkp" + goCntAuthTypeProvider:ContainerCode     
        THEN DO:
           ASSIGN 
             cControls               =  " ~"fcPrTypes":U     + goCntAuthTypeProvider:ContainerCode + cRow + "~",":U.
        END.
        WHEN "fcExclPrTypeLkp" + goCntAuthTypeProvider:ContainerCode 
        THEN DO: 
          ASSIGN 
           cControls               =  " ~"fcPrTypeValidList":U + goCntAuthTypeProvider:ContainerCode + cRow + "~",":U.
        END.
      END CASE. /* CASE ipoControl:ControlName: */
      
      ASSIGN
        ipoControl:ControlToken = IF lExistingRecord THEN  "hidden":U ELSE "Updatable":U.

      IF NOT lExistingRecord THEN
      ASSIGN cOnClick = "fnOpenCntDialog(this,":U
                      + "                ~"~",":U
                      + "                ~"maprtypemsc~",":U
                      +                  cControls
                      + "                ~"~",":U
                      + "                ~"~",":U
                      + "                ~"mahacat~",":U
                      + "                ~"~",":U
                      + "                ~"~");":U
                      
             ipoControl:JavascriptOnClick = cOnClick
             ipoControl:ControlValue      = WarpSpeed:ThemePath + "img/lookup.gif":U
             ipoControl:ControlClass      = "clCurPntr":U
             .
      
      ASSIGN lSuccess = ipoControl:renderAsImage().   
    END. /* WHEN "PRTYPE":U THEN */
    
    /*  Communications */  
    WHEN "Communications" THEN                                    
    DO:
      IF DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type.auth_type_obj":U, "BUFFER-VALUE":U)) > 0
      OR DECIMAL(ipoControl:ControlValue) > 0 
      THEN
        ASSIGN
           ipoControl:AssignList    = "OEM=hacat&OwningObj=" + STRING(ipoControl:ControlValue) + "&OwningKey=":U
           ipoControl:Wob           = "mahomrl":U
           ipoControl:Obj           = goWob:CurrentObj
           ipoControl:ControlValue  = goWob:CurrentObj
           ipoControl:LinkText      = "Update":U
           lSuccess                 = IF Warpspeed:CurrentWob <> "mahacat_ro":U THEN ipoControl:RenderAsHref() ELSE FALSE.  
    END. /* WHEN "Communications" THEN */
    
    WHEN "ActivateEpisode" THEN 
    DO:
      ASSIGN
        dInsurerObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.insurer_obj":U, "BUFFER-VALUE":U))
        dStartDate  = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.effective_date":U, "BUFFER-VALUE":U))
        .
    
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                     INPUT  0,
                                                     INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                     INPUT  "Episode":U,
                                                     INPUT  dStartDate,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).  
        
      IF lValidRule AND cRuleValue = "Activate|Update" THEN 
      DO:
        ASSIGN ipoControl:ControlToken = "Updatable":U.
      END. /* IF lValid THEN DO: */
      ELSE 
      DO:
        ASSIGN ipoControl:ControlToken = "ReadOnly":U.
      END.
        
      lSuccess = ipoControl:renderAsCheckBox().  
    END. /* WHEN "ActivateEpisode" THEN */

    WHEN "CustomSlent" THEN
    DO:                    
      ASSIGN cOEMSelection           = ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U).
      
      IF cOEMSelection = "htmtl" THEN
        ASSIGN ipoControl:ControlJavascript      = "onload=~"fnCustomSlentLoad(this, 'buOwningTrfBtn', 'buOwningBtn');~"".
      ELSE
        ASSIGN ipoControl:ControlJavascript      = "onload=~"fnCustomSlentLoad(this, 'buOwningBtn', 'buOwningTrfBtn');~"".
      
      ASSIGN lSuccess = ipoControl:render().
    END.  /* WHEN "CustomSlent" THEN */

    WHEN "ActivatePenalty" THEN 
    DO:
      ASSIGN
        lActivateAMPM = LOGICAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.activate_am_pm":U,"BUFFER-VALUE":U)).
      
      IF lActivateAMPM
      THEN 
        ASSIGN ipoControl:ControlToken = "Updatable":U.
      ELSE 
        ASSIGN ipoControl:ControlToken = "Disabled":U.
      
      ASSIGN lSuccess = ipoControl:renderAsCheckBox().  
    END. /* WHEN "ActivatePenalty" THEN */
    
    WHEN "ActivateWeekendPass" THEN 
    DO:
      ASSIGN
        lActivateWeekend = LOGICAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.activate_los":U,"BUFFER-VALUE":U)).

      IF lActivateWeekend
      THEN 
        ASSIGN ipoControl:ControlToken = "Updatable":U.
      ELSE 
        ASSIGN ipoControl:ControlToken = "Disabled":U.

      ASSIGN lSuccess = ipoControl:renderAsCheckBox().  
    END. /* WHEN "ActivateWeekendPass" THEN */
    
    WHEN "AuthorisedService":U  THEN 
    DO:
      ASSIGN 
        lDisable                =    LOGICAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.authorised_service":U, "BUFFER-VALUE":U)) = NO 
                                  OR LOGICAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.authorised_service":U, "BUFFER-VALUE":U)) = ?
                                  
        ipoControl:ControlToken = (IF lDisable 
                                   THEN "Disabled":U 
                                   ELSE "Updatable":U).
        
      ipoControl:render().
    END. /* WHEN "KeyField":U  THEN */

    WHEN "Status" THEN                                                                                                            
    DO:      
      ASSIGN                                                                                                                               
        dInsurerObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.insurer_obj":U, "BUFFER-VALUE":U)) 
        dStartDate  = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.effective_date":U, "BUFFER-VALUE":U)).             

      IF dStartDate = ? THEN ASSIGN dStartDate = TODAY. 

      ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,                                       
                                                                       INPUT  0,                                                 
                                                                       INPUT  "ma_acAuthRuleTypeAuthStatus":U,                   
                                                                       INPUT  "ValidStatuses":U,                                 
                                                                       INPUT  dStartDate,                                        
                                                                       OUTPUT lValidRule,                                        
                                                                       OUTPUT cRuleValue).          

      IF lValidRule THEN
      DO: 

        DO iCount = 1 TO NUM-ENTRIES(cRuleValue,","):

          IF  ENTRY(iCount,cRuleValue) <> "2-Assessed"
          AND ENTRY(iCount,cRuleValue) <> "4-Complete"  
          THEN ASSIGN ipoControl:AdditionalItems = ipoControl:AdditionalItems
                                                 + (IF ipoControl:AdditionalItems <> ""
                                                    THEN "|"
                                                    ELSE "")
                                                 + ENTRY(2,ENTRY(iCount,cRuleValue,","),"-")
                                                 + "=":U
                                                 + ENTRY(1,ENTRY(iCount,cRuleValue,","),"-").
        END. /* DO iCount = 1 TO NUM-ENTRIES(cRuleValue,",") */
         
        ASSIGN ipoControl:AdditionalItems = "=|":U + ipoControl:AdditionalItems
               ipoControl:ControlToken    = "Updatable":U

               /* fnOnChangeStatusReason = enables/disables the status notes based on the auth status */
               /* fnOnChangeStatusDefaults = Sets the note type according to the auth status.*/

               ipoControl:JavaScriptOnChange = 'fnOnChangeStatusReason(this);fnOnChangeStatusDefault(this);'.
      END. /* IF lValid THEN DO: */                                                                                              
      ELSE                                                                                                                       
        ASSIGN ipoControl:ControlToken = "Disabled":U.                                                                           

      lSuccess = ipoControl:renderAsComboOrSelect().

    END. /* WHEN "Status" THEN */                                                                                              
    
    WHEN "DefaultHiddenDateSet" THEN
    DO:
      ASSIGN
        dAuthTypeObj = DECIMAL(goWob:CurrentObj).

      FIND FIRST tt_auth_type NO-LOCK
           WHERE tt_auth_type.auth_type_obj = dAuthTypeObj
        NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
      
      IF AVAILABLE tt_auth_type 
      THEN 
        ASSIGN dtAuthTypeHeaderEffectiveDate = tt_auth_type.effective_date.

      ASSIGN
        dtAuthTypeControlRowEffectiveDate = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type_control.effective_date":U, "BUFFER-VALUE":U)).

      IF dtAuthTypeControlRowEffectiveDate <> ? THEN
        ASSIGN ipoControl:ControlValue = SUBSTRING(STRING(DATE(dtAuthTypeControlRowEffectiveDate), mipEnv:DateFormat),1,4) + "/01/01".
      ELSE
        ASSIGN ipoControl:ControlValue =  SUBSTRING(STRING(DATE(dtAuthTypeHeaderEffectiveDate), mipEnv:DateFormat),1,4) + "/01/01".

      lSuccess = ipoControl:render().
    END.  /* WHEN "DefaultHiddenDateSet" THEN */

    WHEN "StatusReason" THEN 
    DO:
      ASSIGN                                                                                                                               
        dInsurerObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.insurer_obj":U, "BUFFER-VALUE":U)) 
        dStartDate  = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.effective_date":U, "BUFFER-VALUE":U)) 
        .                                                                                                                        
                                                                                                                                 
      IF dStartDate = ? THEN ASSIGN dStartDate = TODAY. 
      
      ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,                                       
                                                                       INPUT  0,                                                 
                                                                       INPUT  "ma_acAuthRuleTypeAuthStatus":U,                   
                                                                       INPUT  "ValidStatuses":U,                                 
                                                                       INPUT  dStartDate,                                        
                                                                       OUTPUT lValidRule,                                        
                                                                       OUTPUT cRuleValue).          

      IF lValidRule 
      THEN ASSIGN ipoControl:ControlToken = "Updatable":U.
      ELSE ASSIGN ipoControl:ControlToken = "Disabled":U.

      lSuccess = ipoControl:render().
    END.  /* WHEN "StatusReason" THEN */

    WHEN "SeqKey" THEN
    DO:
      ASSIGN                                                                                                                               
        dInsurerObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.insurer_obj":U, "BUFFER-VALUE":U)) 
        dStartDate  = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.effective_date":U, "BUFFER-VALUE":U)).             

      IF dStartDate = ? THEN ASSIGN dStartDate = TODAY. 

      ASSIGN ipoControl:ControlToken = "ReadOnly":U.

      IF goWob:SubmitValue = "ADD":U
      THEN DO:
        ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,                                       
                                                                         INPUT  0,                                                 
                                                                         INPUT  "ma_acAuthRuleTypeAuthSetups":U, 
                                                                         INPUT  "SequenceEnabled":U,                                 
                                                                         INPUT  dStartDate,                                        
                                                                         OUTPUT lValidRule,                                        
                                                                         OUTPUT cRuleValue). 
        IF lValidRule AND NUM-ENTRIES(cRuleValue,"|") >= 1 AND ENTRY(1,cRuleValue,"|") = "Seq=Yes" 
        THEN 
          ASSIGN ipoControl:ControlToken = "Updatable":U.
      END. /* Then - IF goWob:SubmitValue = "ADD":U */ 
      lSuccess = ipoControl:render().
    END. /* WHEN "SeqKey" THEN */

    WHEN "SeqBtn":U  THEN
    DO:
      ASSIGN                                                                                                                               
        dInsurerObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.insurer_obj":U, "BUFFER-VALUE":U)) 
        dStartDate  = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.effective_date":U, "BUFFER-VALUE":U)).             

      IF dStartDate = ? THEN ASSIGN dStartDate = TODAY. 

      ASSIGN ipoControl:ControlToken = "hidden":U.
      IF goWob:SubmitValue = "ADD":U
      THEN DO: 
        ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,                                       
                                                                         INPUT  0,                                                 
                                                                         INPUT  "ma_acAuthRuleTypeAuthSetups":U,                   
                                                                         INPUT  "SequenceEnabled":U,                                 
                                                                         INPUT  dStartDate,                                        
                                                                         OUTPUT lValidRule,                                        
                                                                         OUTPUT cRuleValue).   
        IF lValidRule AND NUM-ENTRIES(cRuleValue,"|") >= 1 AND ENTRY(1,cRuleValue,"|") = "Seq=Yes" 
        THEN 
          ASSIGN ipoControl:ControlToken = "Updatable":U.
      END. /* Then - IF goWob:SubmitValue = "ADD":U */ 
      lSuccess = ipoControl:render().
    END. /* WHEN "SeqBtn":U  THEN */
   
    WHEN "Prefix" THEN
    DO:
      ASSIGN                                                                                                                               
        dInsurerObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.insurer_obj":U, "BUFFER-VALUE":U)) 
        dStartDate  = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.effective_date":U, "BUFFER-VALUE":U)).             

      IF dStartDate = ? THEN ASSIGN dStartDate = TODAY. 

      ASSIGN ipoControl:ControlToken = "ReadOnly":U.

      IF goWob:SubmitValue = "ADD":U
      THEN DO:
        ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,                                       
                                                                         INPUT  0,                                                 
                                                                         INPUT  "ma_acAuthRuleTypeAuthSetups":U,                   
                                                                         INPUT  "SequenceEnabled":U,                                 
                                                                         INPUT  dStartDate,                                        
                                                                         OUTPUT lValidRule,                                        
                                                                         OUTPUT cRuleValue).   
        IF lValidRule AND NUM-ENTRIES(cRuleValue,"|") >= 2 AND ENTRY(2,cRuleValue,"|") = "Prefix=Yes" 
        THEN 
          ASSIGN ipoControl:ControlToken = "Updatable":U.
      END. /* IF goWob:SubmitValue = "ADD":U */
      lSuccess = ipoControl:render().
    END. /* WHEN "Prefix" THEN */

    WHEN "FormatEffectiveDates" THEN
    DO:
      ASSIGN 
        cFormattedDate = STRING(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type_provider.effective_date":U,"BUFFER-VALUE":U))
        cFormattedDate = STRING(DATE(cFormattedDate), mipEnv:DateFormat)
        ipoControl:ControlValue = cFormattedDate.
   
      ASSIGN lSuccess = ipoControl:renderAsInput(). 
    END.  /* WHEN "FormatEffectiveDates" THEN */
	
	  WHEN "FormatEndDates" THEN
    DO:
      ASSIGN 
        cFormattedDate = STRING(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type_provider.end_date":U,"BUFFER-VALUE":U))
        cFormattedDate = STRING(DATE(cFormattedDate), mipEnv:DateFormat)
        ipoControl:ControlValue = cFormattedDate.
   
      ASSIGN lSuccess = ipoControl:renderAsInput(). 
    END.  /* WHEN "FormatEndDates" THEN */

    WHEN "GetAcronym":U THEN 
    DO:
      ASSIGN
        cAcronymCode = STRING(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type_provider.provider_type_indicator":U,"BUFFER-VALUE":U))
        oAcronym = NEW cls.mipacronym(?, FALSE, "ma_acAuthProviderTypeIndicator", ?).

      oAcronym:focusAcronym("KEY":U, cAcronymCode).

      IF oAcronym:AcronymInFocus
      THEN 
         ASSIGN cLabel = oAcronym:AcronymLabel.
      ELSE
         ASSIGN cLabel = "":U.
      
      ASSIGN ipoControl:ControlValue = cLabel.
   
      ASSIGN lSuccess = ipoControl:renderAsInput().  
    END. /* WHEN "GetAcronym":U THEN */

    WHEN "NoteType":U THEN 
    DO:
      ASSIGN 
        dInsurerObj  = DECIMAL(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.insurer_obj":U, "BUFFER-VALUE":U)) 
        dStartDate   = DATE(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type.effective_date":U, "BUFFER-VALUE":U)) 
        cStatus      = ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type.default_auth_status":U , "BUFFER-VALUE":U)
        lMandatory   = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), INPUT dInsurerObj, 00, INPUT dStartDate).
             
      ASSIGN ipoControl:ControlClass = ipoControl:ControlClass 
                                     + (IF lMandatory THEN " +":U ELSE " -":U)
                                     + "clMan":U.
      IF cStatus <> "":U 
      THEN 
        ASSIGN ipoControl:ControlValue = "AS":U + cStatus.
      ELSE
        ASSIGN ipoControl:ControlValue = "AS*":U.
      
      ipoControl:renderAsInput().
    END. /* WHEN "NoteType":U THEN */

    WHEN "ClaimCode":U THEN 
    DO:
      IF ipoControl:ControlValue = "000" 
      THEN 
        ASSIGN ipoControl:ControlValue = "":U.
      IF ipoControl:ControlValue <> "":U THEN
      DO:
        FIND FIRST ccdesc NO-LOCK 
             WHERE ccdesc.claim-code = INTEGER(ipoControl:ControlValue)
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
        
        IF AVAILABLE ccdesc  
        THEN
          ASSIGN ipoControl:ControlToolTip = ccdesc.description. 
      END. /* IF ipoControl:ControlValue <> "":U THEN */
      ipoControl:renderAsInput().
    END. /* WHEN "ClaimCode":U THEN */

    WHEN "DefEffectiveDate":U THEN 
    DO:
      ASSIGN dAuthTypeObj = DECIMAL(goWob:CurrentObj).

      FIND FIRST tt_auth_type NO-LOCK
           WHERE tt_auth_type.auth_type_obj = dAuthTypeObj
        NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
      
      IF AVAILABLE tt_auth_type 
      THEN 
        ASSIGN ipoControl:ControlValue = IF ipoControl:ControlValue <> "":U
                                         THEN ipoControl:ControlValue ELSE
                                         STRING(tt_auth_type.effective_date, "9999/99/99":U).
      
      ipoControl:renderAsInput().
    END. /* WHEN "DefEffectiveDate":U THEN  */

    WHEN "TrimInt" THEN
    DO:
      IF  ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type_provider.auth_type_provider_obj":U, "BUFFER-VALUE":U) = "0":U
      AND ipoControl:ControlValue = "0" 
      THEN
        ASSIGN
          ipoControl:ControlValue = "" .

      glSuccess = ipoControl:renderAsInput().
    END. /* WHEN "TrimInt":U */

    WHEN "PrTypeDetail":U THEN
    DO: 
      ASSIGN cPrType          =  STRING(ipoControl:ParentContainer:ContainerQuery:GetFieldAttribute("tt_auth_type_detail.pr_type":U,              "BUFFER-VALUE":U) )
             lExistingRecord  = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_detail.auth_type_detail_obj":U, "BUFFER-VALUE":U)) > 0 .
      
      { ma/msc/madispad.i &discipline = cPrType }
        
      ASSIGN ipoControl:ControlValue = IF   lExistingRecord  
                                       THEN cPrType
                                       ELSE "":U     .
     
      ipoControl:renderAsInput().
    END. /* WHEN "PrTypeDetail" */
 
  END CASE.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineAuthTypeControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineAuthTypeControl Procedure 
PROCEDURE defineAuthTypeControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   { ma/app/maauthtypedefinecontrol.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineAuthTypeDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineAuthTypeDetail Procedure 
PROCEDURE defineAuthTypeDetail :
/*----------------------------------------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  If any of the control headings are changed, please change the label in maauthtypedetailtt.i accordingly    
------------------------------------------------------------------------------------------------------------------*/

  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.

  DEFINE VARIABLE cControlIndicatorTypeFieldChange  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cControlMakeFieldsMandatory       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cClearItemCode                    AS CHARACTER NO-UNDO INITIAL 'fnClearFieldValue(this, "anyvalue", "fdOwningObj|fOwningKey|fcAltValueKey");'.
  DEFINE VARIABLE cExecuteCustomSlent               AS CHARACTER NO-UNDO INITIAL 'fnCustomSlent(this, "*=buOwningBtn|htmtl=buOwningTrfBtn");'.

   ASSIGN cControlIndicatorTypeFieldChange = "ma_acAuthDetailTypeIndicatorDef=fcAuthDefaultStatus,fcAuthStatusNote,buAuthStatusNoteBtn|ma_acAuthDetailTypeIndicatorExcl=fcAuthUsageLimit,fcDefaultLineRestriction,fcAutoCreate,fcPrTypeArgument,buPrType,fdQuantityAuth":U
          cControlMakeFieldsMandatory      = "ma_acAuthDetailTypeIndicatorDef=|ma_acAuthDetailTypeIndicatorExcl=fcAuthStatusNote":U.
          
   
   ASSIGN                                                                                                                                                                             
    goCntAuthTypeDetail                           = NEW cls.mipwscontainer("AuthTypeDetail":U, "100%":U, "Authorisation Type Detail Controls":U, WarpSpeed:BaseClass, TRUE)  
    goCntAuthTypeDetail:ContainerMode             = goWob:SubmitValue                                                                                                                
    goCntAuthTypeDetail:ViewOnly                  = FALSE                                                                                                                                
    goCntAuthTypeDetail:ShowGenericReportPrint    = TRUE                                                                                                                                 
    goCntAuthTypeDetail:Collapsed                 = FALSE                                                                                                                              
    goCntAuthTypeDetail:RowsToRender              = ? 
    goCntAuthTypeDetail:RowRenderProcedure        = "rowRenderProcedure":U
    goCntAuthTypeDetail:RowRenderArgument         = goCntAuthTypeDetail:ContainerCode
    goCntAuthTypeDetail:ContainerHidden           = FALSE

    goCntAuthTypeDetail:QueryString               = "FOR EACH tt_auth_type_detail NO-LOCK ":U
                                                  + "  WHERE tt_auth_type_detail.auth_type_provider_obj = ":U + string(gdAuthTypeProviderObj)
                                                  + ",  FIRST tt_sequence no-lock":U
                                                  + "   where tt_sequence.sequence_link_obj = tt_auth_type_detail.auth_type_detail_obj":U
                                                  + "   BY tt_sequence.sequence_value DESCENDING ":U
                                                                   
    goCntAuthTypeDetail:QueryBufferList           = STRING(TEMP-TABLE tt_auth_type_detail:DEFAULT-BUFFER-HANDLE) + ",":U
                                                  + STRING(TEMP-TABLE tt_sequence:DEFAULT-BUFFER-HANDLE)  
    
    oControl                        = goCntAuthTypeDetail:addControl("fcAuthTypeObjArgument":U         + goCntAuthTypeDetail:ContainerCode, "wsInput":U,         "8":U,  "tt_auth_type_detail.auth_type_obj":U,          "decimal":U,   2)                              
    oControl:ControlToken           = "HIDDEN":U  
    
    oControl                        = goCntAuthTypeDetail:addControl("fdAuthTypeProviderObjArgument":U + goCntAuthTypeDetail:ContainerCode, "wsInput":U,         "8":U,  "tt_auth_type_detail.auth_type_provider_obj":U, "decimal":U,   3)                              
    oControl:ControlToken           = "HIDDEN":U  
    oControl:ControlValue           = STRING(gdAuthTypeProviderObj)

    oControl                        = goCntAuthTypeDetail:addControl("fdAuthTypeDetailObj":U           + goCntAuthTypeDetail:ContainerCode, "wsInput":U,         "8":U,  "tt_auth_type_detail.auth_type_detail_obj":U,   "decimal":U,   4) 
    oControl:ControlToken           = "HIDDEN":U
                                      
    oControl                        = goCntAuthTypeDetail:addControl("fcControlTypeIndicator":U        + goCntAuthTypeDetail:ContainerCode, "wsCombo":U,         "3":U, "tt_auth_type_detail.detail_type_indicator":U,   "character":U, 5, "Detail<br>Type<br>Indicator":U)  //tt_auth_type_detail.control_type_indicator            
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthDetailTypeIndicator'":U
    oControl:KeyField               = "mic_acronym.acronym_key":U
    oControl:DisplayFields          = "mic_acronym.acronym_label":U                                                                                                                                                                
    oControl:ControlClass           = "+clMan":U
    oControl:ControlTooltip         = "Please enter a valid Indicator Type"
    oControl:JavascriptOnChange     = "fnEnableDisableFieldControl(this, ~"" + cControlIndicatorTypeFieldChange + "~", true, true); fnMandatoryFieldToggleControl(this, ~"" + cControlMakeFieldsMandatory + "~"); fnPreventUserSet(this, ~"fcAuthDefaultStatus~"); fnSetDefaultStatus(this);":U

    oControl                        = goCntAuthTypeDetail:addControl("fcPrTypeArgument":U                      + goCntAuthTypeDetail:ContainerCode,  "wsInput":U ,       "5":U, "tt_auth_type_detail.pr_type":U,                   "integer":U, 6, "":U)
    oControl:JavascriptOnChange     = "fnDisciplinePad(this,~"|~");"
    oControl:RenderProcedure        = "customRenderProcedure":U
    oControl:RenderArgument         = "PrTypeDetail":U
    oControl:ControlTooltip         = "Discipline for which this record will be created on the Authorisation."
    oControl                        = goCntAuthTypeDetail:addControl("buPrType":U                      + goCntAuthTypeDetail:ContainerCode, "wsLookupButton":U , "1":U, "":U,                                            "character":U, 6, "Discipline":U)
    oControl:LookupWobFLA           = "maprtype":U
    oControl:LookupControls         = "fcPrTypeArgument":U + goCntAuthTypeDetail:ContainerCode
    oControl:LookupFields           = "prtype.pr-type":U
    oControl:FilterControls         = "fcPrTypeArgument":U + goCntAuthTypeDetail:ContainerCode
    oControl:FilterFields           = "prtype.pr-type":U
    oControl:CellLayOutMask         = "&1&2":U 

    oControl                        = goCntAuthTypeDetail:addControl("fcOEM":U                         + goCntAuthTypeDetail:ContainerCode, "wsCombo":U,         "8":U, "tt_auth_type_detail.owning_entity_mnemonic":U,   "character":U, 10, "Detail<br>Entity":U)     // tt_auth_type_detail.owning_entity_mnemonic                        
    oControl:AdditionalItems        = "="                                                                                                                                                                                                            
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthTypeDetailEntities' ":U
    oControl:KeyField               = "mic_acronym.acronym_value":U
    oControl:DisplayFields          = "mic_acronym.acronym_label":U
    oControl:ControlClass           = "+clMan":U
    oControl:ControlTooltip         = "Please select a valid detail entity"
    oControl:JavascriptOnChange     = cClearItemCode + cExecuteCustomSlent + " fnOnChangeOEM(this);" 
                                  
    oControl                        = goCntAuthTypeDetail:addControl("fdOwningObj":U                   + goCntAuthTypeDetail:ContainerCode, "wsInput":U,        "10":U  , "tt_auth_type_detail.owning_obj":U,             "decimal":U,   11, "":U) // tt_auth_type_detail.owning_obj 
    oControl:ControlToken           = "Hidden":U                                  
    oControl                        = goCntAuthTypeDetail:addControl("fOwningKey":U                    + goCntAuthTypeDetail:ContainerCode, "wsInput":U,        "10":U  , "tt_auth_type_detail.owning_key":U,             "character":U, 11, "":U) // tt_auth_type_detail.owning_key 
    oControl:ControlToken           = "Hidden":U  
    oControl                        = goCntAuthTypeDetail:addControl("fcAltValueKey":U                 + goCntAuthTypeDetail:ContainerCode, "wsInput":U,        "10":U  , "tt_auth_type_detail.owning_alt_value":U,       "character":U, 11, "Item Code":U) //tt_auth_type_detail.owning_alt_value
    oControl:ControlTooltip         = "Please select a valid detail item - Tariff or Nappi or Basket"
    oControl:ControlClass           = "+clMan":U
    oControl:AjaxValidation         = "START:" + Warpspeed:CurrentWob + ":ajaxValidationAuthType:SlentAjax":U
    oControl:FilterFields           = "[TableMnemonic],[CodeField]":U                                                                               
    oControl:FilterControls         = "fcOEM":U + goCntAuthTypeDetail:ContainerCode + ",":U
                                    + "fcAltValueKey":U   + goCntAuthTypeDetail:ContainerCode                                                                                                                 
    oControl:ReturnFields           = "[RecordObj],[RecordField]":U                                                                    
    oControl:ReturnControls         = "fdOwningObj":U   + goCntAuthTypeDetail:ContainerCode + ",":U
                                    + "fcAltValueKey":U    + goCntAuthTypeDetail:ContainerCode   

    oControl                        = goCntAuthTypeDetail:addControl("buOwningBtn":U                   + goCntAuthTypeDetail:ContainerCode, "wsLookupButton":U, "20":U, "":U, "":U, 11, "Item Code":U)
    oControl:LookupWobFLA           = "slent":U
    oControl:LookupFields           = "CODE_FIELD":U
    oControl:LookupControls         = "fcAltValueKey":U   + goCntAuthTypeDetail:ContainerCode
    oControl:FilterFields           = "QUERY_OEM,CODE_FIELD":U
    oControl:FilterControls         = "fcOEM":U + goCntAuthTypeDetail:ContainerCode + ",":U
                                    + "fcAltValueKey":U + goCntAuthTypeDetail:ContainerCode
    oControl:ReturnFields           = "CODE_FIELD,KEY_FIELD":U
    oControl:ReturnControls         = "fcAltValueKey":U + goCntAuthTypeDetail:ContainerCode + ",fdOwningObj":U + goCntAuthTypeDetail:ContainerCode
    
    oControl                        = goCntAuthTypeDetail:addControl("fcTariffLinkDefault":U              + goCntAuthTypeDetail:ContainerCode, "wsCheckBox":U,        "8":U, "":U,          "character":U,   12, "":U) 
    oControl:ControlToken           = "Hidden":U
    oControl:ControlValue           = STRING(TRUE)

    
    oControl                        = goCntAuthTypeDetail:addControl("buOwningTrfBtn":U                + goCntAuthTypeDetail:ContainerCode, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 11, "Item Code":U)
    oControl:CellLayoutMask         = "&1&2&3<div id='OwningLkpDiv' style='display : inline'>&4&5</div>":U 
    oControl:LookupWobFLA           = "htmtl":U
    oControl:LookupFields           = "htm_tariff_link.tariff_code":U
    oControl:LookupControls         = "fcAltValueKey":U + goCntAuthTypeDetail:ContainerCode  
    oControl:FilterFields           = "htm_tariff_link.tariff_link_default":U
    oControl:FilterControls         = "fcTariffLinkDefault" + goCntAuthTypeDetail:ContainerCode
    oControl:ReturnFields           = "htm_tariff_link.tariff_code,htm_tariff_link.tariff_link_default":U
    oControl:ReturnControls         = "fcAltValueKey":U + goCntAuthTypeDetail:ContainerCode + "," + "fcTariffLinkDefault" + goCntAuthTypeDetail:ContainerCode
    oControl:RenderProcedure        = "customRenderProcedure":U
    oControl:RenderArgument         = "CustomSlent":U

    oControl                        = goCntAuthTypeDetail:addControl("fdQuantityAuth":U                + goCntAuthTypeDetail:ContainerCode, "wsInput":U,        "8":U, "tt_auth_type_detail.quantity_auth":U,             "decimal":U,   13, "Quantity Authorised":U)                            
    oControl:ControlClass           = "+clNumericOnly":U
    oControl:ControlTooltip         = "The default Quantity Authorised value for this Item when it is created on the Authorisation"

    oControl                        = goCntAuthTypeDetail:addControl("fcAuthUsageLimit":U              + goCntAuthTypeDetail:ContainerCode, "wsInput":U,        "8":U, "tt_auth_type_detail.auth_usage_limit":U,          "integer":U,   14, "Authorisation<br>Usage<br>Limit":U)  // tt_auth_type_detail.auth_usage_limit                            
    oControl:ControlClass           = "+clNumericOnly":U
    oControl:ControlTooltip         = "Please specify a usage limit if required"
    

    oControl                        = goCntAuthTypeDetail:addControl("fcAuthDefaultStatus":U           + goCntAuthTypeDetail:ContainerCode, "wsCombo":U,        "8":U, "tt_auth_type_detail.default_auth_status":U,       "character":U, 15, "Exclusion<br>Authorisation<br>Status":U)  // tt_auth_type_detail.default_auth_status        
    oControl:AdditionalItems        = "|Declined=6":U 
    oControl:ControlToken           = "Disabled":U
    oControl:ControlTooltip         = "Please specify a valid authorisation status"
    oControl:JavascriptOnChange     = "fnPreventUserSet(this, ~"val=6~", ~"fcControlTypeIndicator~", ~"Excl~", ~"Can not change the status from declined when indicator type is set to exclusion~");":U

    oControl                        = goCntAuthTypeDetail:addControl("fcAuthStatusNote":U              + goCntAuthTypeDetail:ContainerCode, "wsInput":U,         "8":U, "tt_auth_type_detail.default_auth_status_note":U, "character":U, 16, "Exclusion<br>Status<br>Reason":U)  // tt_auth_type_control.default_auth_status_note                          
    oControl:ControlClass           = "+clAuthService":U
    oControl:ControlToken           = "Disabled":U
    oControl:AjaxValidation         = "START:" + Warpspeed:CurrentWob + ":ajaxValidationAuthType:note":U
    oControl:FilterFields           = "[NoteKey]":U                                                                                                              
    oControl:FilterControls         = "fcAuthStatusNote":U + goCntAuthTypeDetail:ContainerCode
    oControl:ReturnFields           = "[RecordKey],[RecordLabel]":U                                                                                   
    oControl:ReturnControls         = "fcAuthStatusNote":U  + goCntAuthTypeDetail:ContainerCode + ",":U
                                    + "fcNarration":U  + goCntAuthTypeDetail:ContainerCode 
    oControl:JavascriptOnBlur       = "fnMandatoryReleaseCheck(this);":U

    oControl                        = goCntAuthTypeDetail:addControl("fcNarration":U          + goCntAuthTypeDetail:ContainerCode, "wsInput":U,           "10":U, "":U,             "character":U,      16, "Exclusion<br>Status<br>Reason":U)             
    oControl:ControlToken           = "Hidden":U
    oControl:JavascriptOnChange     = "fnChangeToolTip(this, ~"fcAuthStatusNote~", ~"Please enter a valid status reason~");"

    oControl                        = goCntAuthTypeDetail:addControl("buAuthStatusNoteBtn":U           + goCntAuthTypeDetail:ContainerCode, "wsLookupButton":U, "20":U, "":U,                                             "":U         , 16, "Exclusion<br>Status<br>Reason":U)
    oControl:CellLayoutMask         = "&1&2&3":U
    oControl:LookupWobFLA           = "note":U
    oControl:LookupFields           = "note.key":U
    oControl:LookupControls         = "fcAuthStatusNote":U  + goCntAuthTypeDetail:ContainerCode  
    oControl:FilterFields           = "note.key,note.type":U
    oControl:FilterControls         = "fcAuthStatusNote":U + goCntAuthTypeDetail:ContainerCode + ",AS*":U
    oControl:ReturnFields           = "note.key,note.narration[1]":U
    oControl:ReturnControls         = "fcAuthStatusNote":U   + goCntAuthTypeDetail:ContainerCode + ",fcNarration":U + goCntAuthTypeDetail:ContainerCode

    oControl                        = goCntAuthTypeDetail:addControl("fcDefaultLineRestriction":U      + goCntAuthTypeDetail:ContainerCode, "wsCombo":U,         "8":U, "tt_auth_type_detail.default_line_restriction":U, "character":U, 17, "Default<br>Line<br>Restriction":U)    //  tt_auth_type_control.default_line_restriction                         
    oControl:AdditionalItems        = "="                                                                                                                                                                                                            
    oControl:QueryString            = "FOR EACH mic_acronym WHERE mic_acronym.category_key = 'ma_acAuthLineRestriction'":U
    oControl:KeyField               = "mic_acronym.acronym_key":U
    oControl:DisplayFields          = "mic_acronym.acronym_label":U

    oControl                        = goCntAuthTypeDetail:addControl("fcAutoCreate":U                  + goCntAuthTypeDetail:ContainerCode, "wsCheckbox":U,      "8":U, "tt_auth_type_detail.auth_auto_create":U,         "character":U, 18, "Auto<br>Create":U)    //  tt_auth_type_control.auth_auto_create                         
    oControl:ControlValue           = "FALSE":U
    oControl:JavascriptOnClick      = " $(this).prop(~"value~" , $(this).is(~":checked~").toString() );"
    oControl:ControlTooltip         = "Specify whether a Clinical Detail record should be created on the Authorisation"

    oControl                        = goCntAuthTypeDetail:addControl("fcEffectiveDate":U               + goCntAuthTypeDetail:ContainerCode  , "wsInput":U,      "10":U, "tt_auth_type_detail.effective_date":U,           "date":U,      19, "Effective Date":U) // tt_auth_type.effective_date
    oControl:ControlClass           = "+clMan":U
    oControl:ControlTooltip         = "Please enter a valid effective date"

    oControl                        = goCntAuthTypeDetail:addControl("fcEndDate":U                     + goCntAuthTypeDetail:ContainerCode  , "wsInput":U,      "10":U, "tt_auth_type_detail.end_date":U,                 "date":U,      20, "End Date":U) // tt_auth_type.end_date
    oControl:ControlTooltip         = "Please enter a valid end date"

    oControl                        = goCntAuthTypeDetail:addControl("fcAuthTypeCounter":U             + goCntAuthTypeDetail:ContainerCode, "wsInput":U,         "8":U,  "tt_sequence.sequence_value":U,                  "integer":U,   46, "":U)  
    oControl:ControlToken           = "Hidden":U 
    .
    
  ASSIGN oContainerProperties                           = NEW cls.wscontainerproperties(goCntAuthTypeDetail)
         oContainerProperties:DefaultLess               = TRUE
         oContainerProperties:CollapsableControlList    = "fcDefaultLineRestriction":U + goCntAuthTypeDetail:ContainerCode
         oContainerProperties:DisplayEditButton         = FALSE.
         

  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE  &Container = goCntAuthTypeDetail &ContainerProperties = oContainerProperties } 
  
  { mip/inc/mipcatcherror.i } 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineAuthTypeProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineAuthTypeProvider Procedure 
PROCEDURE defineAuthTypeProvider :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  { ma/app/maauthtypedefineprovider.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainer Procedure 
PROCEDURE defineContainer :
/*------------------------------------------------------------------------------
  Purpose   : Define authorisation type specific containers
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  RUN populateComboLists IN TARGET-PROCEDURE.

  RUN defineSearch IN TARGET-PROCEDURE.   
  
  RUN defineMaint IN TARGET-PROCEDURE.   
  
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineMaint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineMaint Procedure 
PROCEDURE defineMaint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  { ma/app/maauthtypedefinemaint.i } 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineSearch Procedure 
PROCEDURE defineSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
  IF goWob:Mode = "Search":U 
  THEN DO:
    ASSIGN
      goCntSearchFilter                         = goWob:getContainer("SearchFilter":U)
      oControl                                  = goCntSearchFilter:addControl("fcAuthType":U                + goCntSearchFilter:ContainerCode  , "wsInput":U       , "20":U , "":U, "character":U,  1, 1, "Authorisation Type:":U)
      oControl                                  = goCntSearchFilter:addControl("fcPrefix":U                  + goCntSearchFilter:ContainerCode  , "wsInput":U       , "20":U , "":U, "character":U,  3, 2, "Prefix:":U)

      oControl                                  = goCntSearchFilter:addControl("fcInsurerArgumentMnemonic":U + goCntSearchFilter:ContainerCode  , "wsInput":U       , "5":U  , "":U, "character":U, 2, 1, "":U)
      oControl:ControlToken                     = "Hidden":U
      oControl:ControlValue                     = "ermin":U

      oControl                                  = goCntSearchFilter:addControl("fcInsurerArgumentField":U    + goCntSearchFilter:ContainerCode  , "wsInput":U       , "5":U  , "":U, "character":U, 2, 1, "":U)
      oControl:ControlToken                     = "Hidden":U
      oControl:ControlValue                     = "[CodeField]":U

      oControl                                  = goCntSearchFilter:addControl("fdInsurerObj":U              + goCntSearchFilter:ContainerCode  , "wsInput":U        , "1":U , "":U, "character":U, 2, 1, "":U)
      oControl:ControlToken                     = "Hidden":U
      oControl                                  = goCntSearchFilter:addControl("fcInsurer":U                 + goCntSearchFilter:ContainerCode  , "wsInput":U        , "20":U, "":U, "character":U, 2, 1, "":U)
      oControl:ControlTooltip                   = "Please enter a valid client":U
      oControl:AjaxValidation                   = "SERVICE:wsUIService:ajaxValidation:":U
      oControl:FilterFields                     = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
      oControl:FilterControls                   = "fcInsurer":U + goCntSearchFilter:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntSearchFilter:ContainerCode + ",fcInsurerArgumentField":U + goCntSearchFilter:ContainerCode
      oControl:ReturnFields                     = "[RecordObj]":U
      oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchFilter:ContainerCode

      oControl                                  = goCntSearchFilter:addControl("buInsurerBtn":U              + goCntSearchFilter:ContainerCode  , "wsLookupButton":U , "":U  , "":U, "":U         , 2, 1, "Client:":U)
      oControl:LookupWobFLA                     = "ermin":U
      oControl:LookupFields                     = "erm_insurer.insurer_code":U
      oControl:LookupControls                   = "fcInsurer":U + goCntSearchFilter:ContainerCode
      oControl:ReturnFields                     = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U
      oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchFilter:ContainerCode + ",fcInsurer":U + goCntSearchFilter:ContainerCode
      oControl:CellLayoutMask                   = "&1&2&3&4&5":U

      oControl                                  = goCntSearchFilter:addControl("fcDescription":U    + goCntSearchFilter:ContainerCode  , "wsTextArea":U, "20":U, "":U, "character":U,  3, 1, "Description:":U)
      oControl                                  = goCntSearchFilter:addControl("fcAuthTSeqKey":U    + goCntSearchFilter:ContainerCode  , "wsInput":U,    "20":U, "":U, "character":U,  4, 2, "Sequence Key:":U)
      oControl                                  = goCntSearchFilter:addControl("fcEffectiveDate":U  + goCntSearchFilter:ContainerCode  , "wsInput":U,    "20":U, "":U, "date":U,       4, 1, "Effective Date:":U)

      oControl                                  = goCntSearchFilter:addControl("fcEndDate":U        + goCntSearchFilter:ContainerCode  , "wsInput":U,    "20":U, "":U, "date":U,       5, 1, "End Date:":U)

      oControl                                  = goCntSearchFilter:addControl("fcTypeAcronym":U    + goCntSearchFilter:ContainerCode  , "wsCombo":U,    "20":U, "":U, "character":U,  6, 1, "Authorisation Type Group:":U)
      oControl:RenderProcedure                  = "RenderProcedure":U
      oControl:RenderArgument                   = "AcronymSelect:ma_acAuthTypeGroup:=":U
      
      oControl                                  = goCntSearchFilter:addControl("fcExtTypeAcronym":U + goCntSearchFilter:ContainerCode  , "wsCombo":U,    "20":U, "":U, "character":U,  6, 2, "External Authorisation Type Group:":U)
      oControl:RenderProcedure                  = "RenderProcedure":U
      oControl:RenderArgument                   = "AcronymSelect:ma_acAuthTypeGroup:=":U
      
      oControl                                  = goCntSearchFilter:addControl("fcHeaderValue":U    + goCntSearchFilter:ContainerCode  , "wsCombo":U,    "20":U, "":U, "character":U,  7, 1, "Header Values Allowed:":U)
      oControl:RenderProcedure                  = "RenderProcedure":U
      oControl:RenderArgument                   = "AcronymSelect:ma_acAuthHeadValAllowed:=":U

      goCntSearchResults                        = NEW cls.mipwscontainer("SearchResults":U + goWob:ObjectCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE) 
      goCntSearchResults:ContainerMode          = goWob:SubmitValue
      goCntSearchResults:ShowGenericReportPrint = TRUE
      goCntSearchResults:Collapsed              = FALSE
      goCntSearchResults:ViewOnly               = TRUE
      goCntSearchResults:RowsToRender           = ?
      goCntSearchResults:RowRenderProcedure     = "rowRenderProcedure":U
      goCntSearchResults:RowRenderArgument      = goCntSearchResults:ContainerCode
      goCntSearchResults:DefaultContainerType   = "TABLE":U
      goCntSearchResults:ContainerTitle         = "Results":U
      goCntSearchResults:QueryString            = "FOR EACH tt_auth_type NO-LOCK BY tt_auth_type.line_number BY tt_auth_type.auth_type":U
      lSuccess                                  = goWob:setContainer("SearchResults":U,  goCntSearchResults)                                                                                                          
      
      oControl                                  = goCntSearchResults:addControl("fcColAuthType":U          + goCntSearchResults:ContainerCode  ,   "wsHRef":U,     "20":U, "tt_auth_type.auth_type_obj":U,   "character":U,  1, "Authorisation Type":U)
      oControl:KeyField                         = "tt_auth_type.auth_type_obj":U
      oControl:DisplayFields                    = "tt_auth_type.auth_type":U
      oControl:AssignList                       = "&wobMode=":U + goWob:Mode
      oControl                                  = goCntSearchResults:addControl("fcColDescription":U       + goCntSearchResults:ContainerCode  ,   "wsInput":U,    "20":U, "tt_auth_type.description":U,     "character":U,  2, "Description":U)
      oControl                                  = goCntSearchResults:addControl("fcColAcronym":U           + goCntSearchResults:ContainerCode  ,   "wsCombo":U,    "17":U, "tt_auth_type.auth_type_group":U, "character":U,  3, "Authorisation Type Group":U)
      oControl:RenderProcedure                  = "RenderProcedure":U
      oControl:RenderArgument                   = "AcronymSelect:ma_acAuthTypeGroup:=":U
      oControl                                  = goCntSearchResults:addControl("fcColPrefix":U            + goCntSearchResults:ContainerCode  ,   "wsInput":U,    "20":U, "tt_auth_type.auth_type_prefix":U,"character":U,  4, "Prefix":U)
      oControl                                  = goCntSearchResults:addControl("fcColStart":U             + goCntSearchResults:ContainerCode  ,   "wsInput":U,    "17":U, "tt_auth_type.effective_date":U,  "character":U,  5, "Effective Date":U)
      oControl                                  = goCntSearchResults:addControl("fcColEndDate":U           + goCntSearchResults:ContainerCode  ,   "wsInput":U,    "17":U, "tt_auth_type.end_date":U,        "character":U,  6, "End Date":U)

      /* Add the corresponce Col */
      oControl                                  = goCntSearchResults:addControl("fcColAuthCorres":U        + goCntSearchResults:ContainerCode  ,   "wsHRef":U,     "20":U, "tt_auth_type.auth_type_obj":U,   "character":U,  7, "Correspondence":U)
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "Communications":U
      . 
     
     DO iControl = 1 TO NUM-ENTRIES(goCntSearchFilter:getControlNameList()):

       ASSIGN oControl = goCntSearchFilter:getControl(ENTRY(iControl,goCntSearchFilter:getControlNameList())).

       IF CAN-DO("wsCombo":U,oControl:ControlType) THEN
         ASSIGN oControl:ControlClass                 = "'style='width:140px;".

     END. /*DO iControl = 1 TO NUM-ENTRIES(goCntSpecified())*/
    { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = FALSE  &Container = goCntSearchResults  } 
  
  END.
  
  {mip/inc/mipcatcherror.i}    
               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dependencyCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dependencyCheck Procedure 
PROCEDURE dependencyCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipoContainer         AS cls.mipwscontainer NO-UNDO.
  DEFINE OUTPUT PARAMETER oplDependencyExists  AS LOGICAL            NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDependencyMessage AS CHARACTER          NO-UNDO.
  
  DEFINE VARIABLE dObj AS DECIMAL   NO-UNDO.
  
  IF VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) AND VALID-OBJECT(goCntAuthTypeProvider) THEN
  DO:
    
    CASE ipoContainer:ContainerCode:
    
      WHEN goCntAuthTypeProvider:ContainerCode THEN
      DO:
        ASSIGN
          dObj                 = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.auth_type_provider_obj":U, "BUFFER-VALUE":U))
            
          oplDependencyExists  = CAN-FIND(FIRST tt_auth_type_detail NO-LOCK WHERE tt_auth_type_detail.auth_type_provider_obj = dObj).

        IF oplDependencyExists
        THEN
          ASSIGN opcDependencyMessage = mipEnv:formatMessage({mip/inc/miperrortext.i 'MA' 370 ? ? "'Crosswalk record'" "'parameter'"}, "TEXT":U).

      END. /* goCntListType:ContainerCode */
    
    END CASE. /* ipoContainer:ContainerCode */
  
  END.   /* VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) */

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputCustomHeaderJS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputCustomHeaderJS Procedure 
PROCEDURE OutputCustomHeaderJS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  
  {&OUT}
     {ws/inc/wsjavascriptopentag.i}
      /*  Setups defaults for authorisation type provider indicator and disables some fields. */ 
      "~n  function fnIndicatorChanged(pElement)~{ ":U
      "~n    var oRow = wsGetParent(pElement,~"TR~"), iRow = pElement.name.replace(~"fcomProviderTypeIndicatorAuthTypeProvider~", ~"~") ;  ":U
      "~n    var oUserNoUpdate = oRow.getElementsByClassName(~"clUserAccess~");                   ":U
      "~n    if (pElement.value == ~"ma_acAuthProviderTypeIndicatorExcl~" ) ~{                    ":U
      "~n        fnSetControlValue(~"fcMainProviderAuthTypeProvider~"           + iRow ,~"~");  ":U 
      "~n        fnSetControlValue(~"fcDocNumMandatoryAuthTypeProvider~"        + iRow ,~"~");  ":U
      "~n        fnSetControlValue(~"fcAuthorisedServiceAuthTypeProvider~"      + iRow ,~"~");":U
      "~n        fnSetControlValue(~"fcDefaultClaimTypeDetailAuthTypeProvider~" + iRow ,~"C~");  ":U
      "~n        fnSetControlValue(~"fcDefaultClaimCodeDetailAuthTypeProvider~" + iRow ,~"99~");  ":U
      "~n        fnSetControlValue(~"fiQuantityAuthAuthTypeProvider~"           + iRow ,~"~");    ":U
      "~n        fnSetControlValue(~"fdAuthorisedAmountAuthTypeProvider~"       + iRow ,~"~");    ":U
      "~n        fnSetControlValue(~"fcHeaderValuesAllowedAuthTypeProvider~"    + iRow ,~"~");  ":U
      "~n        fnSetControlValue(~"fcHeaderValuesUnlimitedAuthTypeProvider~"  + iRow ,~"~");  ":U
      "~n        fnSetControlValue(~"fcAuthDetaiLinesAuthTypeProvider~"         + iRow ,~"~");    ":U
      "~n        fnSetControlValue(~"fcClaimCodesDisallowAuthTypeProvider~"     + iRow ,~"~");    ":U
      "~n        fnSetControlValue(~"fcClaimTypesDisallowAuthTypeProvider~"     + iRow ,~"~");    ":U
      "~n        fnSetControlValue(~"fcDefaultStatusAuthTypeProvider~"          + iRow ,~"6~");   ":U
      "~n        for( var x = 0; x < oUserNoUpdate.length; x++) ~{                                ":U
      "~n          oUserNoUpdate[x].disabled = ~"disabled~";                                      ":U
      "~n        ~}":U
      "~n    ~}":U
      "~n    else~{ ":U
      "~n        for( var x = 0; x < oUserNoUpdate.length; x++) ~{ ":U
      "~n          oUserNoUpdate[x].disabled = ~"~";":U
      "~n        ~}":U
      "~n    ~}":U
      "~n  ~} ":U
      
      /* Enables fields when authorised service is true.*/
      "~n  function fnAuthorisedService(pElement)~{                                             ":U 
      "~n    var oRow = wsGetParent(pElement,~"TR~"),                                           ":U
      "~n        iRow = pElement.name.replace(~"fcAuthorisedServiceAuthTypeProvider~", ~"~") ;  ":U
      "~n    var oUserNoUpdate = oRow.getElementsByClassName(~"clAuthService~");                ":U
      "~n                                                                                       ":U
      "~n    if (pElement.checked != true ) ~{                                                  ":U
      "~n        fnSetControlValue(~"fcMainProviderAuthTypeProvider~"          + iRow ,~"No~"); ":U
      "~n        fnSetControlValue(~"fcClaimCodesProviderAuthTypeProvider~"    + iRow ,~"~");   ":U 
      "~n        fnSetControlValue(~"fcClaimTypesAuthTypeProvider~"            + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcDefaultClaimCodeDetailAuthTypeProvider~"       + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcClaimTypeDetailAuthTypeProvider~"       + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fiQuantityAuthAuthTypeProvider~"          + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fdAuthorisedAmountAuthTypeProvider~"      + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcHeaderValuesAllowedAuthTypeProvider~"   + iRow ,~"No~"); ":U
      "~n        fnSetControlValue(~"fcHeaderValuesUnlimitedAuthTypeProvider~" + iRow ,~"No~"); ":U 
      "~n        fnSetControlValue(~"fcAuthDetaiLinesAuthTypeProvider~"        + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcClaimCodesDisallowAuthTypeProvider~"    + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcClaimTypesDisallowAuthTypeProvider~"    + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcDefaultStatusAuthTypeProvider~"         + iRow ,~"~");   ":U 
      "~n        fnSetControlValue(~"fcReasonKeyAuthTypeProvider~"             + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcStatusRoleObjsAuthTypeProvider~"        + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcStatusRoleAuthTypeProvider~"            + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcStatusUserObjsAuthTypeProvider~"        + iRow ,~"~");   ":U
      "~n        fnSetControlValue(~"fcStatusUserAuthTypeProvider~"            + iRow ,~"~");   ":U
      "~n        for( var x = 0; x < oUserNoUpdate.length; x++) ~{ ":U
      "~n          oUserNoUpdate[x].disabled = ~"disabled~";":U
      "~n        ~}":U
      "~n    ~}":U
      "~n    else~{ ":U
      "~n        for( var x = 0; x < oUserNoUpdate.length; x++) ~{                              ":U
      "~n          oUserNoUpdate[x].disabled = ~"~";                                            ":U
      "~n        ~}":U
      "~n    ~}":U
      "~n  ~} ":U

      "~n $(document).ready(function()~{                                                                   ":U
      "~n    var oTriggers = document.getElementsByClassName(~"clAuthServe~");                             ":U
      "~n        for( var x = 0; x < oTriggers.length; x++) ~{                                             ":U
      "~n            var oRow = wsGetParent(oTriggers[x],~"TR~"),                                          ":U
      "~n                iRow = oTriggers[x].name.replace(~"fcAuthorisedServiceAuthTypeProvider~", ~"~") ; ":U
      "~n            var oUserNoUpdate = oRow.getElementsByClassName(~"clAuthService~");                   ":U    
      "~n            if (oTriggers[x].checked != true ) ~{                                                 ":U
      "~n               for( var y = 0; y < oUserNoUpdate.length; y++) ~{                                  ":U
      "~n                 oUserNoUpdate[y].disabled = ~"disabled~";                                        ":U
      "~n               ~}                                                                                 ":U
      "~n            ~}                                                                                    ":U
      "~n            else~{                                                                                ":U
      "~n                for( var y = 0; y < oUserNoUpdate.length; y++) ~{                                 ":U
      "~n                  oUserNoUpdate[y].disabled = ~"~";                                               ":U
      "~n                ~}                                                                                ":U
      "~n            ~}                                                                                    ":U
      "~n        ~}                                                                                        ":U
      /*
      "~n  setTimeout(function()~{                                                                                     ":U
      "~n      let oAuthTypeDetailContainer ;                                                                          ":U
      "~n      oAuthTypeDetailContainer = eval(AuthTypeDetail) ;  //  try ~{~} catch(err)~{ ~}                         ":U
      "~n                                                                                                              ":U
      "~n      if( typeof(oAuthTypeDetailContainer) !== ~"undefined~" )~{                                              ":U
      "~n                                                                                                              ":U
      "~n          $(oAuthTypeDetailContainer.containertable).find(~".clmRw1,.clmRw2 ~").each( function(iIdx,oRow)~{   ":U
      "~n                                                                                                              ":U
      "~n                fnSetRowState(oRow) ;                                                                         ":U
      "~n          ~});                                                                                                ":U
      "~n      ~}                                                                                                      ":U
      "~n   ~},250);                                                                                                   ":U      
      */
      "~n ~});                                                                                             ":U


      
     /* - Makes status reason mandatory based on the the Status.*/
      "~n function fnOnChangeStatusReason(pControl)~{                                                                       ":U
      "~n   var oStatusReasonMandatory = $(~"tr~").find(~"[name^=~'flStatusReasonMandatorymahacatMaint~']~").first().get(0),":U
      "~n       oReason                = $(~"tr~").find(~"[name^=~'fcReasonKeymahacatMaint~']~").first().get(0),            ":U
      "~n       oNoteType              = $(~"tr~").find(~"[name^=~'fcNoteTypemahacatMaint~']~").first().get(0),             ":U
      "~n       oStatus                = $(~"tr~").find(~"[name^=~'fcStatusDefaultmahacatMaint~']~").first().get(0);        ":U
      "~n                                                                                                                   ":U  
      "~n   if(~"true,yes,y~".split(~",~").indexOf(trim(oStatusReasonMandatory.value)) > -1) ~{                             ":U
      "~n      oReason.disabled         = false;                                                                            ":U
      "~n      oReason.value            = ~'~';                                                                             ":U
      "~n      wsSetClassName(oReason, ~"+clMan~");                                                                         ":U
      "~n   ~} else ~{                                                                                                      ":U
      "~n      oReason.disabled         = true;                                                                             ":U
      "~n      oReason.value            = ~'~';                                                                             ":U
      "~n      wsSetClassName(oReason, ~"-clMan~");                                                                         ":U
      "~n   ~}                                                                                                              ":U
      "~n ~}                                                                                                                ":U
      
      /* - Sets the note type for the status reason based on the auth status. */
      "~n function fnOnChangeStatusDefault(pControl)~{                                                                      ":U
      "~n   if(pControl.value != ~"~")~{                                                                                    ":U
      "~n      fnSetControlValue(~"fcNoteTypemahacatMaint~" ,~"AS~" + pControl.value);                                      ":U
      "~n   ~}else~{                                                                                                        ":U
      "~n      fnSetControlValue(~"fcNoteTypemahacatMaint~" ,~"AS*~");                                                      ":U
      "~n   ~}                                                                                                              ":U
      "~n ~}                                                                                                                ":U

      "~n function fnEnableActivateWeekend( pValue , pControlName) ~{":U
      "~n   let oControl = $(~"[name^='~" + pControlName + ~"']~").first();":U   
      "~n      $(oControl).prop('disabled',!pValue);":U 
      "~n      $(oControl).prop('checked',false);":U
      "~n ~}":U
      
   
      "~n function fnEnableActivatePenalty( pValue , pControlName) ~{":U
      "~n   let oControl = $(~"[name^='~" + pControlName + ~"']~").first();":U                           
      "~n      $(oControl).prop('disabled',!pValue);":U 
      "~n      $(oControl).prop('checked',false);":U
      "~n ~}":U

      /* Enable/Disable Dependant Health Category selection list, according to the combo.           */
      /* Only enable the selection list if the combo box is "Custom Selection".  Otherwise disable. */

      "~n function fnEnableActivateHealth(pControlName, pValue) ~{":U
      "~n   let oControl = fnGetControls(pControlName)[0];":U
      "~n   if (pValue!=='CUSTOM')":U
      "~n  ~{":U
      "~n      $(oControl).prop('disabled',true);":U 
      "~n   ~} ":U
      "~n    else ~{":U
      "~n      $(oControl).prop('disabled',false);":U
      "~n   ~}":U 
      "~n ~}":U
       {ws/inc/wsjavascriptclosetag.i}
      .
    RUN OutputCustomHeaderJS2 IN TARGET-PROCEDURE .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputCustomHeaderJS2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputCustomHeaderJS2 Procedure 
PROCEDURE OutputCustomHeaderJS2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 

     {&OUT}
     {ws/inc/wsjavascriptopentag.i}
      
      "~n function fnSetExclusionValues(oControl) ~{                                                    ":U
      "~n   let oRow       = $(oControl).closest('tr'),                                                 ":U
      "~n       oClaimCode = oRow.find(~"[name^='fcClaimCode']~").first().get(0),                       ":U
      "~n       oColntrolIndicator = oRow.find(~"[name^='fcControlIndicatorType']~").first().get(0),    ":U
      "~n       oClaimType = oRow.find(~"[name^='fcClaimType']~").first().get(0),                       ":U
      "~n       oAuthStatusCombo = oRow.find(~"[name^='fcDefaultAuthStatus']~").first().get(0);         ":U
      "~n                                                                                               ":U
      "~n   if (oColntrolIndicator.value == 'ma_acAuthControlTypeIndicatorExcl') ~{                     ":U
      "~n     fnSetControlValue(oClaimCode.name, '099');                                                ":U
      "~n     fnSetControlValue(oClaimType.name, 'C');                                                  ":U
      "~n     fnSetControlValue(oAuthStatusCombo.name, '6');                                            ":U
      "~n   ~}                                                                                          ":U
      "~n   else ~{                                                                                     ":U
      "~n     fnSetControlValue(oClaimCode.name, '');                                                   ":U
      "~n     fnSetControlValue(oClaimType.name, '');                                                   ":U
      "~n     fnSetControlValue(oAuthStatusCombo.name, '');                                             ":U
      "~n   ~}                                                                                          ":U
      "~n ~}                                                                                            ":U
      "~n                                                                                               ":U
      "~n function fnSetDefaultStatus(oControl) ~{                                                      ":U
      "~n   let oRow       = $(oControl).closest('tr'),                                                 ":U
      "~n       oDetailIndicator = oRow.find(~"[name^='fcControlTypeIndicator']~").first().get(0),      ":U
      "~n       oDetailStatusCombo = oRow.find(~"[name^='fcAuthDefaultStatus']~").first().get(0);       ":U
      "~n                                                                                               ":U
      "~n   if (oDetailIndicator.value == 'ma_acAuthDetailTypeIndicatorExcl')                          ":U
      "~n     fnSetControlValue(oDetailStatusCombo.name, '6');                                          ":U
      "~n   else                                                                                        ":U
      "~n     fnSetControlValue(oDetailStatusCombo.name, '');                                           ":U
      "~n                                                                                               ":U
      "~n ~}                                                                                            ":U
      {ws/inc/wsjavascriptclosetag.i}
    .

    {&OUT}
     {ws/inc/wsjavascriptopentag.i}
      "~n function fnMandatoryReleaseCheck(oControl) ~{                                                 ":U
      "~n   let validValue = oControl.value;                                                            ":U
      "~n                                                                                               ":U
      "~n   if (oControl.value != '')                                                                   ":U
      "~n     oControl.classList.remove('clMan');                                                       ":U
      "~n   else                                                                                        ":U
      "~n     oControl.classList.add('clMan');                                                          ":U
      "~n                                                                                               ":U
      "~n ~}                                                                                            ":U
      "~n                                                                                               ":U
      "~n function fnSetHiddenDate(oControl) ~{                                                         ":U
      "~n   let oRow = $(oControl).closest('tr'),                                                       ":U
      "~n       oHiddenDateField = oRow.find(~"[name^='fcEffectDateHid']~").first().get(0);             ":U
      "~n                                                                                               ":U
      "~n   if(oControl.value != '' || oControl.value != '?')                                           ":U
      "~n     oHiddenDateField.value = oControl.value                                                   ":U
      "~n                                                                                               ":U
      "~n ~}                                                                                            ":U
      "~n                                                                                               ":U
      "~n function fnAddClearMandatoryRecordEvent(oControl, cFieldToClear) ~{                           ":U
      "~n   let oRow = $(oControl).closest('tr'),                                                       ":U
      "~n       oRollBack = oRow.find(~"[name^='buRefresh']~").first().get(0);                          ":U
      "~n                                                                                               ":U
      "~n   oRollBack.addEventListener('click', function() ~{                                           ":U
      "~n     fnMandatoryField(this, cFieldToClear)                                                     ":U
      "~n   ~});                                                                                        ":U
      "~n ~}                                                                                            ":U
      "~n                                                                                               ":U
      "~n function fnMandatoryField(oControl, cFieldToClear) ~{                                         ":U
      "~n   let oRow = $(oControl).closest('tr'),                                                       ":U
      "~n       oField = oRow.find(~"[name^=~" + cFieldToClear + ~"]~").first().get(0);                 ":U
      "~n                                                                                               ":U
      "~n   fnSetControlValue(oField.name ,~"~");                                                       ":U
      "~n                                                                                               ":U
      "~n ~}                                                                                            ":U
      "~n                                                                                               ":U
      "~n function fnDeactivateTextFields (oControl) ~{                                                 ":U
      "~n   let oRow = $(oControl).closest('tr'),                                                       ":U
      "~n       oComboField = oRow.find(~"[name^='fcGender']~").first().get(0),                         ":U
      "~n       oMaleField =  oRow.find(~"[name^='fcAgeRangeMale']~").first().get(0),                   ":U
      "~n       oMaleFieldBut =  oRow.find(~"[name^='buAgeRangeMaleBtn']~").first().get(0),             ":U
      "~n       oMaleFieldObj =  oRow.find(~"[name^='fdAgeRangeMaleObj']~").first().get(0),             ":U
      "~n       oFemaleField =  oRow.find(~"[name^='fcAgeRangeFemale']~").first().get(0),               ":U
      "~n       oFemaleFieldBut =  oRow.find(~"[name^='buAgeRangeFemaleBtn']~").first().get(0),         ":U
      "~n       oFemaleFieldObj =  oRow.find(~"[name^='fdAgeRangeFemaleObj']~").first().get(0),         ":U
      "~n       oBothField =  oRow.find(~"[name^='fcAgeRangeBoth']~").first().get(0),                   ":U
      "~n       oBothFieldBut = oRow.find(~"[name^='buAgeRangeBothBtn']~").first().get(0);              ":U
      "~n       oBothFieldObj = oRow.find(~"[name^='fdAgeRangeBothObj']~").first().get(0);              ":U
      "~n                                                                                               ":U
      "~n   if (oComboField.value == 'B') ~{                                                            ":U
      "~n       oMaleField.disabled = false;                                                            ":U
      "~n       oMaleFieldBut.disabled = false;                                                         ":U
      "~n       oMaleFieldObj.disabled = false;                                                         ":U
      "~n       oFemaleField.disabled = false;                                                          ":U
      "~n       oFemaleFieldBut.disabled = false;                                                       ":U
      "~n       oFemaleFieldObj.disabled = false;                                                       ":U
      "~n       oBothField.disabled = false;                                                            ":U
      "~n       oBothFieldBut.disabled = false;                                                         ":U
      "~n       oBothFieldObj.disabled = false;                                                         ":U
      "~n                                                                                               ":U
      "~n     if (oBothField.value != '') ~{                                                            ":U
      "~n       oMaleField.disabled = true;                                                             ":U
      "~n       oMaleFieldBut.disabled = true;                                                          ":U
      "~n       oMaleFieldObj.disabled = true;                                                          ":U
      "~n       oFemaleField.disabled = true;                                                           ":U
      "~n       oFemaleFieldBut.disabled = true;                                                        ":U
      "~n       oFemaleFieldObj.disabled = true;                                                        ":U
      "~n     ~}                                                                                        ":U
      "~n     else if (oMaleField.value != '' || oFemaleField.value != '') ~{                           ":U
      "~n       oBothField.disabled = true;                                                             ":U
      "~n       oBothFieldBut.disabled = true;                                                          ":U
      "~n       oBothFieldObj.disabled = true;                                                          ":U  
      "~n                                                                                               ":U
      "~n     ~}                                                                                        ":U
      "~n   ~}                                                                                          ":U
      "~n ~}                                                                                            ":U
     {ws/inc/wsjavascriptclosetag.i}
    .
    {&OUT}
     {ws/inc/wsjavascriptopentag.i}
      "~n function fnReasonTypeArgUpdate(oControl) ~{":U
      " let oReasonTypeArgument = $(oControl).closest(~"tr~").find(~"[name^='fcReasonTypeArgument']~").get(0);":U
      " oReasonTypeArgument.value = ~"AS~" + oControl.value;":U
      " ~} ~n":U

      "function fnOnChangeOEM(pObject)~{                                             ~n":U
      "                                                                              ~n":U
      "     let oRow = $(pObject).closest(~"tr~").first(),                           ~n":U
      "         oQuantityAuth =  $(oRow).find(~"[name^='fdQuantityAuth']~").first(), ~n":U
      "         cValue = $(pObject).val() === ~"hlmcr~" ? ~"0~" : ~"1~" ;            ~n":U
      "     if($(oQuantityAuth).prop(~"disabled~") == false) ~{                      ~n":U
      "       $(oQuantityAuth).val(cValue);     ~}                                   ~n":U
      "~}                                                                            ~n":U
      
      "$(document).on(~"wsRowAppended~", ~"table~", function (event, pPreviousRow, pRow)~{       ~n":U
      "     fnSetRowState(pRow);                         ~n":U
      "~} );  ~n":U
      
      
      
      " function fnSetRowState(pRow ) ~{ ~n":U
      "     let oControlTypeIndicator   = $(pRow).find(~"[name^=~'fcControlTypeIndicator~']~").first() ,           ~n":U
      "         lDefault                = $(oControlTypeIndicator).val() === ~"ma_acAuthDetailTypeIndicatorDef~" , ~n":U
      "         oQuantityAuth           = $(pRow).find(~"[name^=~'fdQuantityAuth~']~").first() ,                   ~n":U
      "         oPrType                 = $(pRow).find(~"[name^=~'fcPrTypeArgument~']~").first() ,                 ~n":U
      "         oAuthUsageLimit         = $(pRow).find(~"[name^=~'fcAuthUsageLimit~']~").first() ,                 ~n":U 
      "         oAutoCreate             = $(pRow).find(~"[name^=~'fcAutoCreate~']~").first() ,                     ~n":U 
      "         oAuthStatusNote         = $(pRow).find(~"[name^=~'fcAuthStatusNote~']~").first() ,                 ~n":U 
      "         oAuthDefaultStatus      = $(pRow).find(~"[name^=~'fcAuthDefaultStatus~']~").first() ;              ~n":U 
      "                                                                                                            ~n":U
      "          $(oQuantityAuth).prop(~"disabled~",!lDefault);                                                    ~n":U
      "          $(oPrType).prop(~"disabled~",!lDefault);                                                          ~n":U
      "          $(oAuthUsageLimit).prop(~"disabled~",!lDefault);                                                  ~n":U
      "          $(oAutoCreate).prop(~"disabled~",!lDefault);                                                      ~n":U
      "          $(oAuthStatusNote).prop(~"disabled~",lDefault);                                                   ~n":U
      "          $(oAuthDefaultStatus).prop(~"disabled~",lDefault);                                                ~n":U
      " ~} ":U
      
      

     {ws/inc/wsjavascriptclosetag.i}  .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-populateComboLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateComboLists Procedure 
PROCEDURE populateComboLists :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTempList        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cKeylist         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cCodelist        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cLabellist       AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cDescriptionList AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cSequenceList    AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cValueList       AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iType            AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iCount           AS INTEGER              NO-UNDO. 
  DEFINE VARIABLE lSuccess         AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE oSearch          AS cls.maauthrulesearch NO-UNDO.
  
   
  
 /*---------------------------- Auth Types -----------------------------*/
  ASSIGN 
    gcAuthTypeGroups     = "":U 
    gcHeaderValues       = "":U
    gcStatusKeys         = "":U
    gcDetails            = "":U
    gcClaimType          = "":U
    .
    
  mipEnv:miUtility:getStatusOrAcronymDetails(INPUT  "Acronym":U, 
                                             INPUT  "ma_acAuthHeadValAllowed":U, 
                                             OUTPUT cKeylist,
                                             OUTPUT cCodelist,
                                             OUTPUT cLabellist,
                                             OUTPUT cDescriptionList,
                                             OUTPUT cSequenceList,
                                             OUTPUT cValueList).
  
  DO iCount = 1 TO NUM-ENTRIES(cValueList,{&Delim-ValueList}):
  
    ASSIGN gcHeaderValues = gcHeaderValues + (IF gcHeaderValues <> "":U THEN "|":U ELSE "":U)
                                           + ENTRY(iCount,cLabelList,{&Delim-ValueList}) + "=":U 
                                           + ENTRY(iCount,cValueList,{&Delim-ValueList}) 
                                     .
  
  END. /*DO iTable = 1 TO NUM-ENTRIES(cTempList):*/
  ASSIGN gcHeaderValues = mipEnv:miExpression:sortList(gcHeaderValues,"|":U).

  mipEnv:miUtility:getStatusOrAcronymDetails(INPUT  "Acronym":U, 
                                             INPUT  "ma_acAuthTypeGroup":U, 
                                             OUTPUT cKeylist,
                                             OUTPUT cCodelist,
                                             OUTPUT cLabellist,
                                             OUTPUT cDescriptionList,
                                             OUTPUT cSequenceList,
                                             OUTPUT cValueList).
  
  DO iCount = 1 TO NUM-ENTRIES(cValueList,{&Delim-ValueList}):
  
    ASSIGN gcAuthTypeGroups = gcAuthTypeGroups + (IF gcAuthTypeGroups <> "":U THEN "|":U ELSE "":U)
                                               + ENTRY(iCount,cLabelList,{&Delim-ValueList}) + "=":U 
                                               + ENTRY(iCount,cValueList,{&Delim-ValueList}) 
                                     .
  
  END. /*DO iTable = 1 TO NUM-ENTRIES(cTempList):*/
  ASSIGN gcAuthTypeGroups = mipEnv:miExpression:sortList(gcAuthTypeGroups,"|":U).

  mipEnv:miUtility:getStatusOrAcronymDetails(INPUT  "Acronym":U, 
                                             INPUT  "ma_acAuthTypeDefaultStatus":U,
                                             OUTPUT cKeylist,
                                             OUTPUT cCodelist,
                                             OUTPUT cLabellist,
                                             OUTPUT cDescriptionList,
                                             OUTPUT cSequenceList,
                                             OUTPUT cValueList).
  
  DO iCount = 1 TO NUM-ENTRIES(cValueList,{&Delim-ValueList}):
  
    ASSIGN gcStatusKeys = gcStatusKeys + (IF gcStatusKeys <> "":U THEN "|":U ELSE "":U)
                                       + ENTRY(iCount,cLabelList,{&Delim-ValueList}) + "=":U 
                                       + ENTRY(iCount,cValueList,{&Delim-ValueList}) 
                                       .
  
  END. /*DO iTable = 1 TO NUM-ENTRIES(cTempList):*/
  ASSIGN gcStatusKeys = mipEnv:miExpression:sortList(gcStatusKeys,"|":U).
  
  ASSIGN gcClaimType   = ' = |A=A|C=C|K=K|N=N|O=O|P=P'.
       
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainer Procedure 
PROCEDURE prepareContainer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcContainer   AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER ipcPrepareWhat AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE cBufferList                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cContainerCode             AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRecords                   AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMode                      AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cButtonList                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iRecords                   AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iRowsRendered              AS INTEGER              NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lAuthTypeDetailValErr      AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lAuthTypeContOrProvValErr  AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE dEndDate                   AS DATE                 NO-UNDO.
  DEFINE VARIABLE dEffectiveDate             AS DATE                 NO-UNDO.
  DEFINE VARIABLE hDataset                   AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hATDataset                 AS HANDLE               NO-UNDO.
  DEFINE VARIABLE oControl                   AS cls.mipwscontrol     NO-UNDO.
  DEFINE VARIABLE oCategories                AS cls.mipwscontrol     NO-UNDO.
  DEFINE VARIABLE oContainer                 AS cls.mipwscontainer   NO-UNDO.
  DEFINE VARIABLE oSearch                    AS cls.maauthtypesearch NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN                                       
  DEFINE VARIABLE oQuestionnaireSearch   AS cls.maclinicaldocsearch    NO-UNDO.
&ENDIF 
                  
  ASSIGN oSearch  = NEW cls.maauthtypesearch(INPUT DATASET dsAuthType BY-REFERENCE).

  IF goWob:SubmitValue = "Add":U THEN
     ASSIGN oCategories              = goCntMaint:getControl("fcActivateHealth":U + goCntMaint:ContainerCode)
            oCategories:ControlToken = "Disabled":U
            oCategories:ControlValue = "":U NO-ERROR.
  {&ResetError}

  /*
    Get age range data
  */
  mipEnv:Health:maUtility:getAgeRangeDetails
    (INPUT 0.00, INPUT "":U, INPUT "":U, INPUT ?,INPUT ?,OUTPUT TABLE tt_age_range).

  CREATE DATASET hATDataset .
  hATDataset:CREATE-LIKE(DATASET dsAuthType:HANDLE).
  hATDataset:COPY-DATASET(DATASET dsAuthType:HANDLE). 

  IF Warpspeed:ValidationError THEN
  DO:
    
    ASSIGN hDataset = DATASET dsAuthType:HANDLE.

    
    RUN restoreBuffersInError IN TARGET-PROCEDURE(INPUT hATDataset,                          /*Source dataset             */ 
                                                  INPUT hDataset,                            /*Target dataset             */ 
                                                  INPUT "tt_auth_type_error":U,              /*Error buffer               */ 
                                                  INPUT "hacat=tt_auth_type,":U +            /*Mnemonic to buffer mapping */ 
                                                        "hactc=tt_auth_type_control,":U  +  
                                                        "hactp=tt_auth_type_provider,":U + 
                                                        "hactd=tt_auth_type_detail":U).

  END. /*IF Warpspeed:ValidationError THEN*/  

  CASE goWob:Mode:
  
    WHEN "Search":U THEN
    DO: 
      ASSIGN
        cButtonList                = IF Warpspeed:CurrentWob = "mahacat_ro":U THEN "Search:Reset":U ELSE "Search:Add:Clear:Reset":U
        oControl                   = goCntSearchFilter:getControl("frmButtonBar":U)
        oContainer                 = oControl:SubContainer
        oContainer:ContainerHidden = TRUE 
        oControl:SubContainer      = wsUiService:getButtonContainer(goCntSearchFilter:ContainerCode + "CustomBtnBar":U, cButtonList)                                         
        cRecords                   = goCntSearchFilter:getControl("fiRecords":U            + goCntSearchFilter:ContainerCode):ControlValue   
        cMode                      = goCntSearchFilter:getControl("fcSearchMode":U         + goCntSearchFilter:ContainerCode):ControlValue
        dEffectiveDate             = DATE(goCntSearchFilter:getControl("fcEffectiveDate":U + goCntSearchFilter:ContainerCode):ControlValue)
        dEndDate                   = DATE(goCntSearchFilter:getControl("fcEndDate":U       + goCntSearchFilter:ContainerCode):ControlValue)
        . 
      
      IF NOT Warpspeed:ValidationError THEN
      DO:
        DATASET dsAuthType:EMPTY-DATASET.
        
        ASSIGN                   
          lSuccess  = oSearch:setCriteria("Query-Sort":U, "tt_auth_type.auth_type":U)
          lSuccess  = oSearch:SetFilterCriteria("tt_auth_type.auth_type":U,                cMode, goCntSearchFilter:getControl("fcAuthType":U        + goCntSearchFilter:ContainerCode):ControlValue)
          lSuccess  = oSearch:SetFilterCriteria("tt_auth_type.description":U,              cMode, goCntSearchFilter:getControl("fcDescription":U     + goCntSearchFilter:ContainerCode):ControlValue)
          lSuccess  = oSearch:SetFilterCriteria("tt_auth_type.auth_type_prefix":U,         cMode, goCntSearchFilter:getControl("fcPrefix":U          + goCntSearchFilter:ContainerCode):ControlValue)
          lSuccess  = oSearch:SetFilterCriteria("tt_auth_type.sequence_key":U,             cMode, goCntSearchFilter:getControl("fcAuthTSeqKey":U     + goCntSearchFilter:ContainerCode):ControlValue)
          lSuccess  = oSearch:SetFilterCriteria("tt_auth_type.auth_type_group":U,          cMode, goCntSearchFilter:getControl("fcTypeAcronym":U     + goCntSearchFilter:ContainerCode):ControlValue)
          lSuccess  = oSearch:SetFilterCriteria("tt_auth_type.header_values_allowed":U,    cMode, goCntSearchFilter:getControl("fcHeaderValue":U     + goCntSearchFilter:ContainerCode):ControlValue)

          lSuccess  = (IF dEffectiveDate <> ?
                       THEN oSearch:SetFilterCriteria("tt_auth_type.effective_date":U, ">=":U, dEffectiveDate) ELSE TRUE)
                    
          lSuccess  = (IF dEndDate <> ?
                       THEN oSearch:SetFilterCriteria("tt_auth_type.end_date":U, "<=":U, dEndDate) ELSE TRUE)
          lSuccess  = oSearch:fetchData()
          .
          
      END. /*IF NOT Warpspeed:ValidationError THEN*/
      
      /* Get the number of records received from the fetch data */
      FOR EACH tt_auth_type NO-LOCK: 
        ASSIGN iRowsRendered = iRowsRendered + 1.
      END. /*FOR EACH tt_auth_type NO-LOCK: */
      
      /* Set the container title with the number of records */
      ASSIGN
       goCntSearchResults:ViewOnly        = TRUE
       goCntSearchResults:RowsToRender    = INTEGER(cRecords)
       goCntSearchResults:QueryBufferList = STRING(TEMP-TABLE tt_auth_type:DEFAULT-BUFFER-HANDLE) 
       lSuccess                           = goCntSearchResults:PopulateFromQuery()  
            
       goCntSearchResults:ContainerTitle  = goCntSearchResults:ContainerTitle + ": ":U
                                           + ( IF iRowsRendered > INTEGER(cRecords)
                                               THEN "Please refine your search criteria as it resulted in more than ":U + cRecords + " records...":U
                                               ELSE (STRING(iRowsRendered) + " record/s found":U)).        
    END. /*WHEN "Search":U THEN*/
    
    WHEN "Maint":U THEN
    DO:
      IF Warpspeed:CurrentWob = "mahacat_ro":U THEN
      DO:
      
        ASSIGN
          cButtonList                = "ReturnToSearch":U
          oControl                   = goCntMaint:getControl("frmButtonBar":U)
          oContainer                 = oControl:SubContainer
          oContainer:ContainerHidden = TRUE 
          oControl:SubContainer      = wsUiService:getButtonContainer(goCntMaint:ContainerCode + "CustomBtnBar":U, cButtonList).

        IF LOOKUP("btnReturnToSearch":U, oControl:SubContainer:getControlNameList()) > 0
        THEN
          ASSIGN oControl              = oControl:SubContainer:getControl("btnReturnToSearch":U)
                 oControl:Obj          = "[ClearObj]":U
                 oControl:ControlValue = "":U
                 oControl:ButtonLabel  = "Return To Search":U.
      END. /* IF Warpspeed:CurrentWob = "mahacat_ro":U THEN */

      IF Warpspeed:CurrentWob <> "mahacat_ro":U AND gdAuthTypeProviderObj <> 0.00 THEN
      DO:
        ASSIGN
          cButtonList                = "Return To Search=ReturnToSearch:Return To Maintenance=ReturnToParameters":U

          oControl                   = goCntMaint:getControl("frmButtonBar":U).

        IF VALID-OBJECT(oControl:SubContainer)
        THEN 
          DELETE OBJECT oControl:SubContainer.

        ASSIGN oControl:SubContainer = wsUiService:getButtonContainer(goCntMaint:ContainerCode + "BtnBar":U, cButtonList).

        IF LOOKUP("btnReturnToSearch":U, oControl:SubContainer:getControlNameList()) > 0
        THEN
          ASSIGN oControl              = oControl:SubContainer:getControl("btnReturnToSearch":U)
                 oControl:Obj          = "[ClearObj]":U
                 oControl:ControlValue = "":U
                 oControl:ButtonLabel  = "Return To Search":U.

      END. /* IF Warpspeed:CurrentWob <> "mahacat_ro":U AND gdAuthTypeProviderObj <> 0.00 THEN */
 
      /*
        Re-fetch auth type data if no errors occurred relating to the auth type table
      */
      IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK 
                      WHERE tt_auth_type_error.owning_entity_mnemonic = "hacat":U
                        AND tt_auth_type_error.error_type = "ERR":U) 
      THEN
      DO:
        
        EMPTY TEMP-TABLE tt_auth_type.
        
        ASSIGN cBufferList = "tt_auth_type":U.

        /*
          Empty control temp table and fill from the data retrieval service only 
          if no errors occurred against the control table
        */
        IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK                                                                                   
                        WHERE tt_auth_type_error.owning_entity_mnemonic = "hactc":U                                                        
                          AND tt_auth_type_error.error_type = "ERR":U)                                                                     
        THEN                                                                                                                               
        DO:                                                                                                                                
          EMPTY TEMP-TABLE tt_auth_type_control.                                                                                           
                                                                                                                                           
          ASSIGN cBufferList = cBufferList + ",tt_auth_type_control":U
            
                               /* Add empty row */
                 lSuccess    = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_type_control:DEFAULT-BUFFER-HANDLE). 
        END. /*IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK WHERE tt_auth_type_error.owning_entity_mnemonic = "hactc":U) THEN*/        
                                                                                                                                           
        /*
          Empty provider temp table and fill from the data retrieval service only 
          if no errors occurred against the provider table
        */
        IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK                                                                                   
                        WHERE tt_auth_type_error.owning_entity_mnemonic = "hactp":U                                                        
                          AND tt_auth_type_error.error_type = "ERR":U)                                                                     
        THEN                                                                                                                               
        DO:                                                                                                                                
          EMPTY TEMP-TABLE tt_auth_type_provider.                                                                                          
                                                                                                                                           
          ASSIGN cBufferList = cBufferList + ",tt_auth_type_provider":U 
            
                               /* Add empty row */
                 lSuccess    = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_type_provider:DEFAULT-BUFFER-HANDLE).
        END. /*IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK WHERE tt_auth_type_error.owning_entity_mnemonic = "hactp":U) THEN*/

        /*
          Empty provider temp table and fill from the data retrieval service only 
          if no errors occurred against the provider table
        */
        IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK                                                                                   
                        WHERE tt_auth_type_error.owning_entity_mnemonic = "hactd":U                                                        
                          AND tt_auth_type_error.error_type = "ERR":U)                                                                     
        THEN                                                                                                                               
        DO:                                                                                                                                
          EMPTY TEMP-TABLE tt_auth_type_detail.                                                                                          
                                                                                                                                           
          ASSIGN cBufferList = cBufferList + ",tt_auth_type_detail":U 
            
                               /* Add empty row */
                 lSuccess    = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_type_detail:DEFAULT-BUFFER-HANDLE).
        END. /*IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK WHERE tt_auth_type_error.owning_entity_mnemonic = "hactp":U) THEN*/  

        IF goWob:CurrentObj <> "":U
        THEN
          ASSIGN lSuccess = oSearch:SetCriteria("BufferList":U, cBufferList)
          
                 lSuccess = oSearch:SetFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, DECIMAL(goWob:CurrentObj))                             
                 lSuccess = oSearch:fetchData().

        FIND FIRST  tt_auth_type NO-LOCK 
             WHERE  tt_auth_type.auth_type_obj = DECIMAL(goWob:CurrentObj)
          NO-ERROR.
        {&ResetError}

        IF AVAILABLE tt_auth_type
        THEN DO:
          ASSIGN
            oControl    = goCntMaint:getControl("fcHealthOption":U   + goCntMaint:ContainerCode)
            oCategories = goCntMaint:getControl("fcActivateHealth":U + goCntMaint:ContainerCode).

          /* Handle hard-coded ALL, NONE, RULE and selection (CUSTOM) */
          CASE tt_auth_type.activate_health:

            WHEN "":U  // Cater for Rule
            THEN
              ASSIGN
                oControl   :ControlValue = "RULE":U
                oCategories:ControlToken = "Disabled":U
                oCategories:ControlValue = "":U.

            WHEN "[ALL]":U OR
            WHEN "[NONE]":U
            THEN
              ASSIGN
                oControl   :ControlValue = REPLACE(REPLACE(tt_auth_type.activate_health , "[":U, "":U), "]":U ,"":U)
                oCategories:ControlToken = "Disabled":U
                oCategories:ControlValue = "":U.

            OTHERWISE
              ASSIGN oControl:ControlValue = "CUSTOM":U.

          END CASE.  /* tt_auth_type.activate_health */

        END.

      END. /*IF NOT CAN-FIND(FIRST tt_auth_type_error NO-LOCK WHERE tt_auth_type_error.owning_entity_mnemonic = "hacat":U) THEN*/

      IF gdAuthTypeProviderObj <> 0.00 THEN
      DO:
        RUN rebuildMainMaintContainer IN TARGET-PROCEDURE.
      END.

      /*
        Make sure the auth type obj has been populated on new dummy records
      */
      FOR EACH tt_auth_type_control
         WHERE tt_auth_type_control.auth_type_control_obj <= 0:

        ASSIGN tt_auth_type_control.auth_type_obj = DECIMAL(goWob:CurrentObj).

        VALIDATE tt_auth_type_control.

        CREATE tt_sequence.

        IF NOT WarpSpeed:ValidationError 
        THEN
          ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_control.auth_type_control_obj
                 tt_sequence.sequence_value = 1.
        ELSE
          ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_control.auth_type_control_obj
                 tt_sequence.sequence_value = tt_auth_type_control.auth_type_control_obj.
        
      END. /*FOR EACH tt_auth_type_control*/
      
      FOR EACH tt_auth_type_control NO-LOCK
        WHERE tt_auth_type_control.auth_type_obj = DECIMAL(goWob:CurrentObj)
        AND tt_auth_type_control.auth_type_control_obj > 0.00:
        
        CREATE tt_sequence.
        ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_control.auth_type_control_obj
               tt_sequence.sequence_value = IF tt_auth_type_control.control_type_indicator = "ma_acAuthControlTypeIndicatorDef":U THEN 3 ELSE 2.
      END. /* FOR EACH tt_auth_type_control NO-LOCK */
      /*
        Make sure the auth type obj has been populated on new dummy records
      */
      FOR EACH tt_auth_type_provider
         WHERE tt_auth_type_provider.auth_type_provider_obj <= 0:

        ASSIGN tt_auth_type_provider.auth_type_obj = DECIMAL(goWob:CurrentObj).

        VALIDATE tt_auth_type_provider.

        CREATE tt_sequence.

        IF NOT WarpSpeed:ValidationError 
        THEN
          ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_provider.auth_type_provider_obj
                 tt_sequence.sequence_value = 1.
        ELSE
          ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_provider.auth_type_provider_obj
                 tt_sequence.sequence_value = tt_auth_type_provider.auth_type_provider_obj.
      END. /* FOR EACH tt_auth_type_provider WHERE tt_auth_type_provider.auth_type_provider_obj <= 0: */

      FOR EACH tt_auth_type_provider NO-LOCK
        WHERE tt_auth_type_provider.auth_type_obj = DECIMAL(goWob:CurrentObj)
        AND tt_auth_type_provider.auth_type_provider_obj > 0.00:
        
        CREATE tt_sequence.
        ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_provider.auth_type_provider_obj
               tt_sequence.sequence_value = IF tt_auth_type_provider.provider_type_indicator = "ma_acAuthProviderTypeIndicatorDef":U THEN 3 ELSE 2.
      END. /* FOR EACH tt_auth_type_provider NO-LOCK */
      
      /*
        Make sure the auth type obj has been populated on new dummy records
      */
      FOR EACH tt_auth_type_detail
        WHERE tt_auth_type_detail.auth_type_detail_obj <= 0:

        ASSIGN 
            tt_auth_type_detail.auth_type_obj          = DECIMAL(goWob:CurrentObj)
            tt_auth_type_detail.auth_type_provider_obj = gdAuthTypeProviderObj.

        VALIDATE tt_auth_type_detail.

        CREATE tt_sequence.

        IF NOT WarpSpeed:ValidationError 
        THEN
          ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_detail.auth_type_detail_obj
                 tt_sequence.sequence_value    = 1.
        ELSE
          ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_detail.auth_type_detail_obj
                 tt_sequence.sequence_value    = tt_auth_type_detail.auth_type_detail_obj.
      END. /* FOR EACH tt_auth_type_detail */

      FOR EACH tt_auth_type_detail NO-LOCK
        WHERE tt_auth_type_detail.auth_type_obj = DECIMAL(goWob:CurrentObj)
        AND tt_auth_type_detail.auth_type_detail_obj > 0.00:
        
        CREATE tt_sequence.
        ASSIGN tt_sequence.sequence_link_obj = tt_auth_type_detail.auth_type_detail_obj
               tt_sequence.sequence_value    = IF tt_auth_type_detail.detail_type_indicator = "ma_acAuthDetailTypeIndicatorDef":U THEN 3 ELSE 2.
      END. /* FOR EACH tt_auth_type_provider NO-LOCK */

      IF VALID-OBJECT(goCntTypeControl) AND VALID-OBJECT(goCntAuthTypeProvider) AND VALID-OBJECT(goCntAuthTypeDetail) THEN
      DO:
        IF CAN-FIND(FIRST tt_auth_type_error NO-LOCK WHERE tt_auth_type_error.owning_entity_mnemonic = "hactd") THEN
        ASSIGN lAuthTypeDetailValErr = TRUE.

        IF CAN-FIND(FIRST tt_auth_type_error NO-LOCK 
                    WHERE tt_auth_type_error.owning_entity_mnemonic = "hactc" OR tt_auth_type_error.owning_entity_mnemonic = "hactp") THEN
          ASSIGN lAuthTypeContOrProvValErr = TRUE.
        
        IF lAuthTypeContOrProvValErr THEN
          ASSIGN lAuthTypeDetailValErr = FALSE.
        
        IF gdAuthTypeProviderObj <> 0.00 OR (goWob:SubmitValue = "MaintSubmit" AND lAuthTypeDetailValErr) THEN
          ASSIGN
            goCntTypeControl:ContainerHidden      = TRUE
            goCntAuthTypeProvider:ContainerHidden = TRUE
            goCntAuthTypeDetail:ContainerHidden   = FALSE.
        ELSE
          ASSIGN
            goCntTypeControl:ContainerHidden      = FALSE
            goCntAuthTypeProvider:ContainerHidden = FALSE
            goCntAuthTypeDetail:ContainerHidden   = TRUE.
      END. /* IF VALID-OBJECT(goCntTypeControl) AND VALID-OBJECT(goCntAuthTypeProvider) AND VALID-OBJECT(goCntAuthTypeDetail) THEN */
      
&IF {&DBDFMA} >= 10195 &THEN                                         
      IF goWob:CurrentObj <> "":U THEN
      DO:
        DATASET dsClinicalDocs:EMPTY-DATASET().
        
        ASSIGN oQuestionnaireSearch = NEW cls.maclinicaldocsearch(DATASET dsClinicalDocs BY-REFERENCE).
        
        ASSIGN 
          lSuccess = oQuestionnaireSearch:SetFilterCriteria("tt_clinical_docs.owning_entity_mnemonic":U, "=":U, "hacat":U)
          lSuccess = oQuestionnaireSearch:SetFilterCriteria("tt_clinical_docs.owning_obj":U            , "=":U, DECIMAL(goWob:CurrentObj))
          lSuccess = oQuestionnaireSearch:fetchData().
        
        CREATE tt_clinical_docs.
        ASSIGN tt_clinical_docs.clinical_docs_obj      = -9998
               tt_clinical_docs.line_number            = 9999
               tt_clinical_docs.document_type          = 'ma_acDocTypeQuestionnaires'
               tt_clinical_docs.owning_entity_mnemonic = "hacat":U
               tt_clinical_docs.owning_obj             = DECIMAL(goWob:CurrentObj).
               
        VALIDATE tt_clinical_docs.

      END. /*IF goWob:CurrentObj <> "":U THEN*/
     
      IF VALID-OBJECT(goCntQuestionnaire)
      THEN
        ASSIGN
          goCntQuestionnaire:QueryString     = "FOR EACH tt_clinical_docs NO-LOCK WHERE tt_clinical_docs.document_type = 'ma_acDocTypeQuestionnaires' BY tt_clinical_docs.line_number":U
          goCntQuestionnaire:QueryBufferList = STRING(TEMP-TABLE tt_clinical_docs:DEFAULT-BUFFER-HANDLE)       
          
          lSuccess                           = goCntQuestionnaire:populateFromQuery()
          
          goCntQuestionnaire:ContainerHidden = NOT goCntQuestionnaire:DataAvailable.
            
&ENDIF 
              
      mipEnv:Health:maUtility:getAuditRecordTT(
         INPUT "hac_auth_type":U,             
         INPUT goWob:CurrentObj,                                                                              
         INPUT "":U,                                                                      
         INPUT "":U,                                                                                              
         OUTPUT TABLE ttAuditRecord).
     
      ASSIGN  
        goCntMaint:QueryString             = SUBSTITUTE(goCntMaint:QueryString, goWob:CurrentObj)
        goCntMaint:QueryBufferList         = IF gdAuthTypeProviderObj <> 0.00 OR goWob:SubmitValue = "MaintSubmit"
                                                   THEN (STRING(TEMP-TABLE tt_auth_type:DEFAULT-BUFFER-HANDLE) + ",,":U + STRING(TEMP-TABLE tt_auth_type_provider:DEFAULT-BUFFER-HANDLE))
                                             ELSE (STRING(TEMP-TABLE tt_auth_type:DEFAULT-BUFFER-HANDLE))
        lSuccess                           = goCntMaint:PopulateFromQuery() WHEN goWob:SubmitValue <> "Add":U //AND NOT WarpSpeed:ValidationError
        goCntMaint:ContainerTitle          = "Authorisation Type Maintenance":U
        goCntAudit:QueryBufferList         = STRING(TEMP-TABLE ttAuditRecord:DEFAULT-BUFFER-HANDLE)
        lSuccess                           = goCntAudit:PopulateFromQuery() 
        .   

      IF goWob:SubmitValue <> "ADD" 
      THEN
      DO:
        IF gdAuthTypeProviderObj <= 0.00 THEN
            ASSIGN
              goCntTypeControl:ViewOnly          = FALSE
              cContainerCode                     = goCntTypeControl:ContainerCode 
              
              oControl                           = goCntTypeControl:getControl("fcRecordAction":U + cContainerCode)
              oControl:ControlQueryField         = "tt_auth_type_control.record_action":U
              
              oControl                           = goCntTypeControl:getControl("buEdit":U + cContainerCode)
              oControl:ControlToken              = "Hidden":U /* we are working with detail lines no need for maint mode */
              lSuccess                           = goCntTypeControl:PopulateFromQuery() 
              
              oControl                           = goCntAuthTypeProvider:getControl("fcRecordAction":U + goCntAuthTypeProvider:ContainerCode)
              oControl:ControlQueryField         = " tt_auth_type_provider.record_action":U
              
              lSuccess                           = goCntAuthTypeProvider:PopulateFromQuery()
            .
        IF gdAuthTypeProviderObj <> 0.00 OR goWob:SubmitValue = "MaintSubmit" THEN
          ASSIGN
              oControl                           = goCntAuthTypeDetail:getControl("fcRecordAction":U + goCntAuthTypeDetail:ContainerCode)
              oControl:ControlQueryField         = " tt_auth_type_detail.record_action":U
              
              lSuccess                           = goCntAuthTypeDetail:PopulateFromQuery()
              .
      END.
        

    END. /*WHEN "Maint":U THEN*/
      
  END CASE. /*CASE goWob:Mode:*/
  
  
&IF {&DBDFMA} >= 10195 &THEN
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch.
                IF VALID-OBJECT(oQuestionnaireSearch) THEN DELETE OBJECT oQuestionnaireSearch."}
&ELSE
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rebuildMainMaintContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rebuildMainMaintContainer Procedure 
PROCEDURE rebuildMainMaintContainer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  { ma/app/maauthtyperebuildmaint.i } 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-restoreBuffersInError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreBuffersInError Procedure 
PROCEDURE restoreBuffersInError :
/*------------------------------------------------------------------------------
  Purpose   : Restore buffer data from a temporary dataset if there
              are any errors related to the buffer.    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iphSourceDataset AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER iphTargetDataset AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER ipcErrorBuffer   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipcMappingList   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iPair        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cBuffer      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMnemonic    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPair        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hSource      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hSourceError AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hTarget      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hTargetError AS HANDLE      NO-UNDO.

  /*
    Copy any errors back to target error table
  */
  ASSIGN hSourceError = iphSourceDataset:GET-BUFFER-HANDLE(ipcErrorBuffer)
         hTargetError = iphTargetDataset:GET-BUFFER-HANDLE(ipcErrorBuffer).
  
  hTargetError:TABLE-HANDLE:COPY-TEMP-TABLE(hSourceError:TABLE-HANDLE).
  
  /*
    Check which buffers have any errors and replace the contents of the buffers that have
    errors with the data that was submitted by the user for that buffer
  */
  DO iPair = 1 TO NUM-ENTRIES(ipcMappingList):
  
    ASSIGN cPair     = ENTRY(iPair, ipcMappingList)
      
           cMnemonic = ENTRY(1, cPair, "=":U)
           cBuffer   = ENTRY(2, cPair, "=":U)
      
           hSource   = iphSourceDataset:GET-BUFFER-HANDLE(cBuffer)
           hTarget   = iphTargetDataset:GET-BUFFER-HANDLE(cBuffer).
  
    /*
      Check if the buffer has an error
    */ 
    hSourceError:FIND-FIRST(SUBSTITUTE("WHERE &1.owning_entity_mnemonic = '&2' AND &1.error_type = 'ERR'":U, hSourceError:NAME, cMnemonic)) NO-ERROR.
  
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
         
    IF hSourceError:AVAILABLE THEN
    DO:               
      /*
        Restore the contents of the temp table we captured from the users submit
      */
      hTarget:TABLE-HANDLE:COPY-TEMP-TABLE(hSource:TABLE-HANDLE).
    
    END. /*IF hSource:AVAILABLE THEN*/
  END. /*DO iPair = 1 TO ipcMappingList:*/


  { mip/inc/mipcatcherror.i }              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rowRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowRenderProcedure Procedure 
PROCEDURE rowRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.

  { ma/app/maauthtyperowrender.i } 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shutdown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shutdown Procedure 
PROCEDURE shutdown :
/*------------------------------------------------------------------------------
  Purpose: Do proper clean-up in this procedure, as the wob shuts down.
------------------------------------------------------------------------------*/    
  
  /*All registered containers are deleted in the base wob*/
  IF VALID-OBJECT(goCntQuestionnaire)    THEN DELETE OBJECT goCntQuestionnaire    NO-ERROR.
  IF VALID-OBJECT(goCntAuthTypeProvider) THEN DELETE OBJECT goCntAuthTypeProvider NO-ERROR.
  IF VALID-OBJECT(goCntTypeControl)      THEN DELETE OBJECT goCntTypeControl      NO-ERROR.
  IF VALID-OBJECT(goCntAuthTypeDetail)   THEN DELETE OBJECT goCntAuthTypeDetail   NO-ERROR.
  
  { mip/inc/mipcatcherror.i } 
  
END PROCEDURE.  /* shutdown */

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-submitControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE submitControl Procedure 
PROCEDURE submitControl :
/*----------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow                  AS INTEGER            NO-UNDO.
  DEFINE VARIABLE cContainerCode        AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE iCount                AS INTEGER            NO-UNDO.
  DEFINE VARIABLE cAuthTypes            AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cUsers                AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cControlTypeIndicator AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cAction               AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cAuthTypeInfo         AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE dObj                  AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj          AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE dInsurerObj           AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE iOptionCode           AS INTEGER            NO-UNDO.
  DEFINE VARIABLE tEffectiveDate        AS DATE               NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL            NO-UNDO.

  DEFINE VARIABLE oAuthType             AS cls.maauthtype     NO-UNDO.
  DEFINE VARIABLE oErrorObject          AS cls.maerrorobject  NO-UNDO.

  
  ASSIGN cContainerCode = goCntTypeControl:ContainerCode
         oErrorObject   = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
  
  DO iRow = 1 TO INTEGER(GET-VALUE(goCntTypeControl:ContainerCode + "_rowsrendered":U)):

    ASSIGN cAuthTypes = "":U.
    
    IF GET-VALUE("fcRestrictionObjs":U  + goCntTypeControl:ContainerCode + STRING(iRow)) <> ""
    THEN 
    DO iCount = 1 TO NUM-ENTRIES(GET-VALUE("fcRestrictionObjs":U  + goCntTypeControl:ContainerCode + STRING(iRow))) :
    
      mipEnv:miDBEntity:focusTable("hacat"). 
      mipEnv:miDBEntity:findRecord(DECIMAL(ENTRY(iCount, GET-VALUE("fcRestrictionObjs":U  + goCntTypeControl:ContainerCode + STRING(iRow))))).
     
      IF mipEnv:miDBEntity:RecordAvailable 
      THEN ASSIGN cAuthTypes = trim(cAuthTypes) + trim(mipEnv:miDBEntity:RecordCode) + ",":U. 
    END.
    ELSE ASSIGN cAuthTypes = GET-VALUE("fcRestrictions":U  + goCntTypeControl:ContainerCode + STRING(iRow)).
            
    ASSIGN cAuthTypes = TRIM(cAuthTypes, ",").
    
    ASSIGN
      cAction               =         GET-VALUE("fcAction":U                + cContainerCode + STRING(iRow))  
      dObj                  = DECIMAL(GET-VALUE("fcDetailObj":U             + cContainerCode + STRING(iRow))) 
      dObj                  = (IF dObj <= 0 
                               THEN iRow * -1 ELSE dObj) 
      cControlTypeIndicator =         GET-VALUE("fcControlIndicatorType":U + cContainerCode + STRING(iRow))
      dInsurerObj           = DECIMAL(GET-VALUE("fcInsurer":U              + cContainerCode + STRING(iRow))) 
      dAuthTypeObj          = DECIMAL(GET-VALUE("fcAuthTypeObjArgument":U  + cContainerCode + STRING(iRow)))              
      iOptionCode           = INTEGER(GET-VALUE("fcOption":U               + cContainerCode + STRING(iRow)))              
      tEffectiveDate        = (IF     GET-VALUE("fcEffectDate":U           + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U
                               THEN DATE(GET-VALUE("fcEffectDate":U        + cContainerCode + STRING(iRow)))                 
                               ELSE ?).

    /*Duplicate check*/
    FIND FIRST tt_auth_type_control NO-LOCK
         WHERE tt_auth_type_control.auth_type_obj          = dAuthTypeObj
           AND tt_auth_type_control.insurer_obj            = dInsurerObj
           AND tt_auth_type_control.option_code            = iOptionCode
           AND tt_auth_type_control.control_type_indicator = cControlTypeIndicator
           AND tt_auth_type_control.effective_date         = tEffectiveDate
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF AVAILABLE tt_auth_type_control
    THEN
    DO: 
      ASSIGN WarpSpeed:ValidationError = TRUE.
      
      oAuthType            = NEW cls.maauthtype().

      ASSIGN lSuccess      = oAuthType:focusAuthType(dAuthTypeObj)
             cAuthTypeInfo = oAuthType:AuthType.


      oErrorObject:addError(INPUT "hactc":U, 
                            INPUT dObj, 
                            INPUT "":U, 
                            INPUT "effective_date":U,
                            INPUT iRow, 
                            INPUT "MA":U, 
                            INPUT 125, 
                            INPUT "Auth type control,":U + STRING(cAuthTypeInfo) + "-":U 
                                                         + STRING(tt_auth_type_control.option_code)   + "-":U 
                                                         + STRING(tt_auth_type_control.effective_date)).
      ASSIGN tEffectiveDate = ?                     /* Ensure we dont get an index failure because we will still create the TT record so that it can be returned to the user to fix */
             cAction        = "ValidationError":U.  /* Dummy action so that we dont try and save */
    END. /* IF AVAILABLE tt_auth_type_control */
    
    IF NOT CAN-FIND(FIRST tt_auth_type_control NO-LOCK
         WHERE tt_auth_type_control.auth_type_obj          = dAuthTypeObj
           AND tt_auth_type_control.insurer_obj            = dInsurerObj
           AND tt_auth_type_control.option_code            = iOptionCode
           AND tt_auth_type_control.control_type_indicator = cControlTypeIndicator
           AND tt_auth_type_control.effective_date         = tEffectiveDate)
    THEN
    DO:
      CREATE tt_auth_type_control.
      
      ASSIGN
        tt_auth_type_control.record_action              = cAction
        tt_auth_type_control.auth_type_obj              = dAuthTypeObj                                  
        tt_auth_type_control.claim_codes_header         =         GET-VALUE("fcClaimCode":U             + cContainerCode + STRING(iRow))             
        tt_auth_type_control.claim_types_header         =         GET-VALUE("fcClaimType":U             + cContainerCode + STRING(iRow))               
        tt_auth_type_control.period                     = INTEGER(GET-VALUE("fcPeriod":U                + cContainerCode + STRING(iRow)))              
        tt_auth_type_control.period_type                =         GET-VALUE("fcPeriodType":U            + cContainerCode + STRING(iRow))               
        tt_auth_type_control.amount                     = DECIMAL(GET-VALUE("fcAmount":U                + cContainerCode + STRING(iRow)))              
        tt_auth_type_control.quantity                   = INTEGER(GET-VALUE("fcQuantity":U              + cContainerCode + STRING(iRow)))              
        tt_auth_type_control.option_code                = iOptionCode                                   
        tt_auth_type_control.insurer_obj                = dInsurerObj              
        tt_auth_type_control.claim_codes_disallow       =         GET-VALUE("fcClaimCodeDis":U          + cContainerCode + STRING(iRow))               
        tt_auth_type_control.claim_types_disallow       =         GET-VALUE("fcClaimTypes":U            + cContainerCode + STRING(iRow))               
        tt_auth_type_control.usage_type                 =         GET-VALUE("fcUsageType":U             + cContainerCode + STRING(iRow))              
        tt_auth_type_control.usage_quantity             = INTEGER(GET-VALUE("fcUsageQuantity":U         + cContainerCode + STRING(iRow)))              
        tt_auth_type_control.usage_period               = INTEGER(GET-VALUE("fcUsagePeriod":U           + cContainerCode + STRING(iRow)))              
        tt_auth_type_control.usage_period_type          =         GET-VALUE("fcUsagePeriodType":U       + cContainerCode + STRING(iRow)) 

        tt_auth_type_control.gender                     =         GET-VALUE("fcGender":U                + cContainerCode + STRING(iRow)) 
        tt_auth_type_control.age_range_male_obj         = DECIMAL(GET-VALUE("fdAgeRangeMaleObj":U       + cContainerCode + STRING(iRow))) 
        tt_auth_type_control.age_range_female_obj       = DECIMAL(GET-VALUE("fdAgeRangeFemaleObj":U     + cContainerCode + STRING(iRow))) 
        tt_auth_type_control.age_range_both_obj         = DECIMAL(GET-VALUE("fdAgeRangeBothObj":U       + cContainerCode + STRING(iRow))) 

        tt_auth_type_control.default_auth_status        =         GET-VALUE("fcDefaultAuthStatus":U     + cContainerCode + STRING(iRow))
        tt_auth_type_control.default_auth_status_note   =         GET-VALUE("fcReasonKey":U             + cContainerCode + STRING(iRow))

        tt_auth_type_control.usage_override_user        = fnGetUserCodes(INPUT GET-VALUE("fcUserObjs":U + cContainerCode + STRING(iRow)) , 
                                                                         INPUT "mimus":U, 
                                                                         INPUT GET-VALUE("fcUsagePeriodOver":U + cContainerCode + STRING(iRow)))
        
        tt_auth_type_control.auth_type_restrictions     = fnGetUserCodes(INPUT GET-VALUE("fcRestrictionObjs":U + cContainerCode + STRING(iRow)), 
                                                                         INPUT "mimro":U, 
                                                                         INPUT GET-VALUE("fcRestrictions":U + cContainerCode + STRING(iRow)))                                                                                                                                         

        tt_auth_type_control.auth_type_control_obj      = IF DECIMAL(GET-VALUE("fcDetailObj":U         + cContainerCode + STRING(iRow))) > 0 
                                                          THEN DECIMAL(GET-VALUE("fcDetailObj":U       + cContainerCode + STRING(iRow))) 
                                                          ELSE iRow * -1
                                                          
        tt_auth_type_control.period_override            = IF GET-VALUE("fcPeriodOverride":U            + cContainerCode + STRING(iRow)) = "YES":U          
                                                          THEN TRUE                                                                                       
                                                          ELSE FALSE                                                                                      
                                                                                                                                                         
        tt_auth_type_control.effective_date             = tEffectiveDate                                                                                        
                                                                                                                                                        
        tt_auth_type_control.end_date                   = (IF GET-VALUE("fcEndDate":U + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U              
                                                           THEN DATE(GET-VALUE("fcEndDate":U           + cContainerCode + STRING(iRow)))                        
                                                           ELSE ?)    
                                                           
        tt_auth_type_control.activate_authorised_values = IF GET-VALUE("fcActivateAuthorised":U        + cContainerCode + STRING(iRow)) = "YES":U
                                                          THEN TRUE 
                                                          ELSE FALSE
                                                   
        tt_auth_type_control.enforce_authorised_values  = IF GET-VALUE("fcEnforceAthorised":U          + cContainerCode + STRING(iRow)) = "YES":U
                                                          THEN TRUE 
                                                          ELSE FALSE

        tt_auth_type_control.control_type_indicator     =         GET-VALUE("fcControlIndicatorType":U            + cContainerCode + STRING(iRow))

        
      .                                           
      VALIDATE tt_auth_type_control.

    END. /* IF NOT CAN-FIND(FIRST tt_auth_type_control NO-LOCK */
  END. 
   
  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-submitDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE submitDetails Procedure 
PROCEDURE submitDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow                       AS INTEGER           NO-UNDO.
  DEFINE VARIABLE cContainerCode             AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER           NO-UNDO.
  DEFINE VARIABLE oErrorObject               AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE dAuthTypeObj               AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE dAuthTypeProviderObj       AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE dOwningObj                 AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE dObj                       AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE cOEM                       AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cOwningKey                 AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cAction                    AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cAuthTypeInfo              AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cOEMLabel                  AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE tEffectiveDate             AS DATE              NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL           NO-UNDO.

  DEFINE VARIABLE oAuthType   AS cls.maauthtype    NO-UNDO.
  
  ASSIGN cContainerCode = goCntAuthTypeDetail:ContainerCode
         oErrorObject   = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
  
  DO iRow = 1 TO INTEGER(GET-VALUE(goCntAuthTypeDetail:ContainerCode + "_rowsrendered":U)):
    
    ASSIGN
      cAction               =            GET-VALUE("fcAction":U                + cContainerCode + STRING(iRow))
      dObj                  = DECIMAL(   GET-VALUE("fdAuthTypeDetailObj":U     + cContainerCode + STRING(iRow))) 
      dObj                  = (IF dObj <= 0 
                               THEN iRow * -1 ELSE dObj) 
      dAuthTypeObj          = DECIMAL(   GET-VALUE("fcAuthTypeObjArgument":U    + cContainerCode + STRING(iRow)))
      dAuthTypeProviderObj  = DECIMAL(   GET-VALUE("fdAuthTypeProviderObjArgument":U    + cContainerCode + STRING(iRow)))
      cOEM                  =            GET-VALUE("fcOEM":U                    + cContainerCode + STRING(iRow))
      dOwningObj            = DECIMAL(   GET-VALUE("fdOwningObj":U              + cContainerCode + STRING(iRow)))
      cOwningKey            =            GET-VALUE("fcOwningKey":U              + cContainerCode + STRING(iRow))
      tEffectiveDate        = (IF        GET-VALUE("fcEffectiveDate":U          + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U
                               THEN DATE(GET-VALUE("fcEffectiveDate":U          + cContainerCode + STRING(iRow)))                 
                               ELSE ?).
    
    /*Duplicate check*/
    FIND FIRST tt_auth_type_detail NO-LOCK
         WHERE tt_auth_type_detail.auth_type_obj            = dAuthTypeObj
           AND tt_auth_type_detail.auth_type_provider_obj   = dAuthTypeProviderObj
           AND tt_auth_type_detail.owning_entity_mnemonic   = cOEM
           AND tt_auth_type_detail.owning_obj               = dOwningObj
           AND tt_auth_type_detail.owning_key               = cOwningKey
           AND tt_auth_type_detail.effective_date           = tEffectiveDate 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF AVAILABLE tt_auth_type_detail 
    THEN
    DO: 
      ASSIGN WarpSpeed:ValidationError = TRUE.
      
      oAuthType           = NEW cls.maauthtype().

      ASSIGN lSuccess      = oAuthType:focusAuthType(dAuthTypeObj).
      ASSIGN cAuthTypeInfo = oAuthType:AuthType.

      FIND FIRST mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthTypeDetailEntities'
        AND mic_acronym.acronym_value = string(tt_auth_type_detail.owning_entity_mnemonic) NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

      IF AVAILABLE mic_acronym THEN
        ASSIGN cOEMLabel = mic_acronym.acronym_label.

      oErrorObject:addError(INPUT "hactd":U, 
                              INPUT dObj, 
                              INPUT "":U, 
                              INPUT "effective_date":U,
                              INPUT iRow, 
                              INPUT "MA":U, 
                              INPUT 125, // &1 already exists with &2
                              INPUT "Auth type detail,":U + STRING(cAuthTypeInfo) + "-":U 
                                                          + STRING(cOEMLabel)   + "-":U 
                                                          + STRING(tt_auth_type_detail.effective_date)).
      ASSIGN tEffectiveDate = ?                     /* Ensure we dont get an index failure because we will still create the TT record so that it can be returned to the user to fix */
             cAction        = "ValidationError":U.  /* Dummy action so that we dont try and save */
    END. /* IF AVAILABLE tt_auth_type_provider */

    IF NOT CAN-FIND(FIRST tt_auth_type_detail NO-LOCK
         WHERE tt_auth_type_detail.auth_type_obj          = dAuthTypeObj
           AND tt_auth_type_detail.auth_type_provider_obj = dAuthTypeProviderObj
           AND tt_auth_type_detail.owning_entity_mnemonic = cOEM
           AND tt_auth_type_detail.owning_obj             = dOwningObj
           AND tt_auth_type_detail.owning_key             = cOwningKey
           AND tt_auth_type_detail.effective_date         = tEffectiveDate)
    THEN
    DO:
      CREATE tt_auth_type_detail.
      
      ASSIGN
        tt_auth_type_detail.auth_type_obj                = dAuthTypeObj
        tt_auth_type_detail.auth_type_detail_obj         = dObj                                             
        tt_auth_type_detail.default_auth_status          =           GET-VALUE("fcDefaultStatus":U                      + cContainerCode + STRING(iRow))                                             
        tt_auth_type_detail.default_auth_status_note     =           GET-VALUE("fcReasonKey":U                          + cContainerCode + STRING(iRow))                                             
        tt_auth_type_detail.detail_type_indicator        =           GET-VALUE("fcControlTypeIndicator":U               + cContainerCode + STRING(iRow))                                      
                                                                                                                        
        tt_auth_type_detail.record_action                = cAction
        tt_auth_type_detail.owning_entity_mnemonic       =           GET-VALUE("fcOEM":U                                + cContainerCode + STRING(iRow))
                                                                                                                        
        tt_auth_type_detail.owning_obj                   = dOwningObj                                            
                                                                                                                        
        tt_auth_type_detail.owning_key                   =           GET-VALUE("fcOwningKey":U                          + cContainerCode + STRING(iRow))
        tt_auth_type_detail.owning_alt_value             =           GET-VALUE("fcAltValueKey":U                        + cContainerCode + STRING(iRow))
        
        tt_auth_type_detail.auth_type_provider_obj       = IF     DECIMAL(GET-VALUE("fdAuthTypeProviderObjArgument":U   + cContainerCode + STRING(iRow))) > 0 
                                                             THEN DECIMAL(GET-VALUE("fdAuthTypeProviderObjArgument":U   + cContainerCode + STRING(iRow))) 
                                                             ELSE iRow * -1
                                                          
        tt_auth_type_detail.auth_usage_limit             =  INTEGER(GET-VALUE("fcAuthUsageLimit":U                      + cContainerCode + STRING(iRow)))
                                                                                                                        
        tt_auth_type_detail.auth_auto_create             =  IF       GET-VALUE("fcAutoCreate":U                         + cContainerCode + STRING(iRow)) = "YES":U          
                                                             THEN TRUE                                                                                                
                                                             ELSE FALSE                                                 
                                                                                                                        
        tt_auth_type_detail.default_line_restriction     =           GET-VALUE("fcDfltLineRestrictions":U               + cContainerCode + STRING(iRow))
                                                                                                                        
        tt_auth_type_detail.effective_date               = tEffectiveDate                                               
                                                                                                                        
        tt_auth_type_detail.end_date                     = (IF         GET-VALUE("ftEndDate":U                          + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U
                                                             THEN DATE(GET-VALUE("ftEndDate":U                          + cContainerCode + STRING(iRow)))                 
                                                             ELSE ?)
        
        .

      VALIDATE tt_auth_type_detail.

    END. /* ELSE DO: */
  END. /* DO iRow = 1 TO INTEGER(GET-VALUE(goCntAuthTypeProvider:ContainerCode + "_rowsrendered""U)) */
  
  { mip/inc/mipcatcherror.i 
     &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject. IF VALID-OBJECT(oAuthType)  THEN DELETE OBJECT oAuthType." }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-submitProviders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE submitProviders Procedure 
PROCEDURE submitProviders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow                      AS INTEGER           NO-UNDO.
  DEFINE VARIABLE cContainerCode            AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE iCount                    AS INTEGER           NO-UNDO.
  DEFINE VARIABLE oErrorObject              AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE oAuthType                 AS cls.maauthtype    NO-UNDO.
  DEFINE VARIABLE dObj                      AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj              AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE dAuthTypeProviderObj      AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE dInsurerObj               AS DECIMAL           NO-UNDO.
  DEFINE VARIABLE iNegNum                   AS INTEGER           NO-UNDO.
  DEFINE VARIABLE iOptionCode               AS INTEGER           NO-UNDO.
  DEFINE VARIABLE cProviderType             AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cProviderTypeIndicator    AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cAction                   AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cPracticeTypeList         AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cAuthTypeInfo             AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE tEffectiveDate            AS DATE              NO-UNDO.
  DEFINE VARIABLE lSuccess                  AS LOGICAL           NO-UNDO.

  
  
  ASSIGN cContainerCode = goCntAuthTypeProvider:ContainerCode
         oErrorObject   = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
  
  DO iRow = 1 TO INTEGER(GET-VALUE(goCntAuthTypeProvider:ContainerCode + "_rowsrendered":U)):
    
    ASSIGN
      cAction               =            GET-VALUE("fcAction":U                     + cContainerCode + STRING(iRow))
      dObj                  = DECIMAL(GET-VALUE("fcDetailObj":U                     + cContainerCode + STRING(iRow))) 
      dObj                  = (IF dObj <= 0 
                               THEN iRow * -1 ELSE dObj) 
      dAuthTypeObj            = DECIMAL(   GET-VALUE("fcAuthTypeObjArgument":U      + cContainerCode + STRING(iRow)))
      dInsurerObj             = DECIMAL(   GET-VALUE("fcInsurer":U                  + cContainerCode + STRING(iRow)))
      iOptionCode             = INTEGER(   GET-VALUE("fcOption":U                   + cContainerCode + STRING(iRow)))
      iNegNum                 = INTEGER(   GET-VALUE("fcNegGroup":U                 + cContainerCode + STRING(iRow)))
      cProviderType           =            GET-VALUE("fcProviderType":U             + cContainerCode + STRING(iRow))
      cProviderTypeIndicator  =            GET-VALUE("fcomProviderTypeIndicator":U  + cContainerCode + STRING(iRow))
      cPracticeTypeList       =            GET-VALUE("fcPrTypes":U                  + cContainerCode + STRING(iRow))
      tEffectiveDate          = (IF        GET-VALUE("ftEffectiveDate":U            + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U
                                 THEN DATE(GET-VALUE("ftEffectiveDate":U            + cContainerCode + STRING(iRow)))                 
                                 ELSE ?).
    
    /*Duplicate check*/
    FIND FIRST tt_auth_type_provider NO-LOCK
         WHERE tt_auth_type_provider.auth_type_obj              = dAuthTypeObj
           AND tt_auth_type_provider.insurer_obj                = dInsurerObj
           AND tt_auth_type_provider.option_code                = iOptionCode
           AND tt_auth_type_provider.provider_type_indicator    = cProviderTypeIndicator
           AND tt_auth_type_provider.provider_type              = cProviderType
           AND tt_auth_type_provider.neg_num                    = iNegNum
           AND tt_auth_type_provider.pr_type_list               = cPracticeTypeList
           AND tt_auth_type_provider.effective_date             = tEffectiveDate
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF AVAILABLE tt_auth_type_provider 
    THEN
    DO:
      ASSIGN WarpSpeed:ValidationError = TRUE.
      
      oAuthType            = NEW cls.maauthtype().

      ASSIGN lSuccess      = oAuthType:focusAuthType(dAuthTypeObj)
             cAuthTypeInfo = oAuthType:AuthType.

      oErrorObject:addError(INPUT "hactp":U, 
                              INPUT dObj, 
                              INPUT "":U, 
                              INPUT "effective_date":U,
                              INPUT iRow, 
                              INPUT "MA":U, 
                              INPUT 125, 
                              INPUT "Auth type provider,":U + STRING(cAuthTypeInfo) + "-":U 
                                                            + STRING(tt_auth_type_provider.option_code)   + "-":U 
                                                            + STRING(tt_auth_type_provider.provider_type) + "-":U 
                                                            + STRING(tt_auth_type_provider.effective_date)).
      ASSIGN tEffectiveDate = ?                     /* Ensure we dont get an index failure because we will still create the TT record so that it can be returned to the user to fix */
             cAction        = "ValidationError":U.  /* Dummy action so that we dont try and save */

    END. /* IF AVAILABLE tt_auth_type_provider */

    IF NOT CAN-FIND(FIRST tt_auth_type_provider NO-LOCK
         WHERE tt_auth_type_provider.auth_type_obj              = dAuthTypeObj
           AND tt_auth_type_provider.insurer_obj                = dInsurerObj
           AND tt_auth_type_provider.option_code                = iOptionCode
           AND tt_auth_type_provider.provider_type_indicator    = cProviderTypeIndicator
           AND tt_auth_type_provider.provider_type              = cProviderType
           AND tt_auth_type_provider.neg_num                    = iNegNum
           AND tt_auth_type_provider.pr_type_list               = cPracticeTypeList
           AND tt_auth_type_provider.effective_date             = tEffectiveDate)
    THEN
    DO:

      CREATE tt_auth_type_provider.
      
      ASSIGN
        tt_auth_type_provider.doc_num_mandatory                   = IF GET-VALUE("fcDocNumMandatory":U                      + cContainerCode + STRING(iRow)) <> "":U
                                                                    THEN LOGICAL(GET-VALUE("fcDocNumMandatory":U                      + cContainerCode + STRING(iRow))) 
                                                                    ELSE ?
        tt_auth_type_provider.default_auth_status                 =         GET-VALUE("fcDefaultStatus":U                   + cContainerCode + STRING(iRow))                                             
        tt_auth_type_provider.default_auth_status_note            =         GET-VALUE("fcReasonKey":U                       + cContainerCode + STRING(iRow))                                             
        tt_auth_type_provider.default_auth_status_upd_user        = fnGetUserCodes(INPUT GET-VALUE("fcStatusUserObjs":U     + cContainerCode + STRING(iRow)) , 
                                                                                   INPUT "mimus":U, 
                                                                                   INPUT GET-VALUE("fcStatusUsers":U        + cContainerCode + STRING(iRow)))
                                                                  
        tt_auth_type_provider.default_auth_status_upd_role        = fnGetUserCodes(INPUT GET-VALUE("fcStatusRoleObjs":U     + cContainerCode + STRING(iRow)), 
                                                                                   INPUT "mimro":U, 
                                                                                   INPUT GET-VALUE("fcStatusRoles":U        + cContainerCode + STRING(iRow)))                                                   
                                                                  
        tt_auth_type_provider.record_action                       = cAction
        tt_auth_type_provider.amount_auth                         = DECIMAL(  GET-VALUE("fdAuthorisedAmount":U              + cContainerCode + STRING(iRow)))
                                                                                                                      
        tt_auth_type_provider.authorised_service                  = IF        GET-VALUE("fcAuthorisedService":U             + cContainerCode + STRING(iRow)) = "YES":U          
                                                                    THEN TRUE                                                                                            
                                                                    ELSE FALSE                                        
                                                                                                                      
        tt_auth_type_provider.authorise_detail_lines              =           GET-VALUE("fcAuthDetaiLines":U                + cContainerCode + STRING(iRow))
        tt_auth_type_provider.auth_type_obj                       = dAuthTypeObj                                      
        tt_auth_type_provider.auth_type_provider_obj              = IF DECIMAL(GET-VALUE("fcDetailObj":U                    + cContainerCode + STRING(iRow))) > 0 
                                                                    THEN DECIMAL(GET-VALUE("fcDetailObj":U                  + cContainerCode + STRING(iRow))) 
                                                                    ELSE iRow * -1
                                                                  
        tt_auth_type_provider.claim_codes_provider                =           GET-VALUE("fcClaimCodesProvider":U            + cContainerCode + STRING(iRow))
        tt_auth_type_provider.claim_types_provider                =           GET-VALUE("fcClaimTypesCombo":U               + cContainerCode + STRING(iRow))
        tt_auth_type_provider.claim_codes_disallow                =           GET-VALUE("fcClaimCodesDisallow":U            + cContainerCode + STRING(iRow))
        tt_auth_type_provider.claim_types_disallow                =           GET-VALUE("fcClaimTypesDisallowCombo":U       + cContainerCode + STRING(iRow)) 
        tt_auth_type_provider.default_claim_code_detail           = INTEGER(  GET-VALUE("fcDefaultClaimCodeDetail":U        + cContainerCode + STRING(iRow)))
        tt_auth_type_provider.default_claim_type_detail           =           GET-VALUE("fcClaimTypeDetail":U               + cContainerCode + STRING(iRow))
                                                                  
        tt_auth_type_provider.effective_date                      = tEffectiveDate                                         
                                                                  
        tt_auth_type_provider.end_date                            = (IF       GET-VALUE("ftEndDate":U                       + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U
                                                                    THEN DATE(GET-VALUE("ftEndDate":U                       + cContainerCode + STRING(iRow)))                 
                                                                    ELSE ?)
        tt_auth_type_provider.header_values_allowed               =           GET-VALUE("fcHeaderValuesAllowed":U           + cContainerCode + STRING(iRow))
                                                                  
        tt_auth_type_provider.header_values_unlimited             = IF        GET-VALUE("fcHeaderValuesUnlimited":U         + cContainerCode + STRING(iRow)) = "YES":U
                                                                    THEN TRUE
                                                                    ELSE FALSE
                                                                  
        tt_auth_type_provider.enforce_header_claim_code_match     = IF        GET-VALUE("fcEnforceCCMatch":U                  + cContainerCode + STRING(iRow)) = "YES":U
                                                                    THEN TRUE
                                                                    ELSE FALSE

        tt_auth_type_provider.enforce_header_claim_type_match     = IF        GET-VALUE("fcEnforceCTMatch":U                  + cContainerCode + STRING(iRow)) = "YES":U
                                                                    THEN TRUE
                                                                    ELSE FALSE
                                                                  
        tt_auth_type_provider.insurer_obj                         = dInsurerObj
                                                                  
        tt_auth_type_provider.main_provider                       = IF        GET-VALUE("fcMainProvider":U                  + cContainerCode + STRING(iRow)) = "YES":U
                                                                    THEN TRUE
                                                                    ELSE FALSE
                                                                    
        tt_auth_type_provider.mandatory                           = IF        GET-VALUE("fcMandatory":U                     + cContainerCode + STRING(iRow)) = "YES":U
                                                                    THEN TRUE
                                                                    ELSE FALSE
        tt_auth_type_provider.number_providers_allowed            = INTEGER(  GET-VALUE("fcNumProviderAllowed":U            + cContainerCode + STRING(iRow)))
        tt_auth_type_provider.neg_num                             = iNegNum
        tt_auth_type_provider.option_code                         = iOptionCode
        tt_auth_type_provider.provider_sequence                   = INTEGER(  GET-VALUE("fiProviderSeq":U                   + cContainerCode + STRING(iRow)))
        tt_auth_type_provider.provider_type                       = cProviderType
        tt_auth_type_provider.pr_type_list                        =           GET-VALUE("fcPrTypes":U                       + cContainerCode + STRING(iRow))
        tt_auth_type_provider.quantity_auth                       = INTEGER(  GET-VALUE("fiQuantityAuth":U                  + cContainerCode + STRING(iRow)))
                                                                  
        tt_auth_type_provider.pr_type_valid_list                  =           GET-VALUE("fcPrTypeValidList":U               + cContainerCode + STRING(iRow))
        tt_auth_type_provider.provider_type_indicator             =           GET-VALUE("fcomProviderTypeIndicator":U         + cContainerCode + STRING(iRow))  
                                                                  
        tt_auth_type_provider.base_rate_upd_user                  = IF GET-VALUE("fcBaseRateUpdUser":U + cContainerCode + STRING(iRow)) = "":U THEN "":U
                                                                    ELSE fnGetUserCodes(INPUT GET-VALUE("fcBaseRateUpdUserObj":U + cContainerCode + STRING(iRow)) , 
                                                                                        INPUT "mimus":U, 
                                                                                        INPUT GET-VALUE("fcBaseRateUpdUser":U    + cContainerCode + STRING(iRow)))
                                                                  
        tt_auth_type_provider.base_rate_upd_role                  = IF GET-VALUE("fcBaseRateUpdRole":U + cContainerCode + STRING(iRow)) = "":U THEN "":U
                                                                    ELSE fnGetUserCodes(INPUT GET-VALUE("fcBaseRateUpdRoleObj":U + cContainerCode + STRING(iRow)), 
                                                                                        INPUT "mimro":U, 
                                                                                        INPUT GET-VALUE("fcBaseRateUpdRole":U    + cContainerCode + STRING(iRow)))         
                                                                  
        tt_auth_type_provider.ars_rate_upd_user                   =  IF GET-VALUE("fcArsRateUpdUser":U  + cContainerCode + STRING(iRow)) = "":U THEN "":U
                                                                     ELSE fnGetUserCodes(INPUT GET-VALUE("fcArsRateUpdUserObj":U  + cContainerCode + STRING(iRow)) , 
                                                                                         INPUT "mimus":U,
                                                                                         INPUT GET-VALUE("fcArsRateUpdUser":U     + cContainerCode + STRING(iRow)))
                                                                  
        tt_auth_type_provider.ars_rate_upd_role                   =  IF GET-VALUE("fcArsRateUpdRole":U  + cContainerCode + STRING(iRow)) = "":U THEN "":U
                                                                     ELSE fnGetUserCodes(INPUT GET-VALUE("fcArsRateUpdRoleObj":U  + cContainerCode + STRING(iRow)), 
                                                                                         INPUT "mimro":U, 
                                                                                         INPUT GET-VALUE("fcArsRateUpdRole":U     + cContainerCode + STRING(iRow)))  .
                                                                  
      VALIDATE tt_auth_type_provider.

    END. /* ELSE DO: */
   
  END. /* DO iRow = 1 TO INTEGER(GET-VALUE(goCntAuthTypeProvider:ContainerCode + "_rowsrendered""U)) */
  
  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject." }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-submitQuestionnaire) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE submitQuestionnaire Procedure 
PROCEDURE submitQuestionnaire :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cContainerCode    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iCount            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cAuthTypes        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cUsers            AS CHARACTER   NO-UNDO.
  
  ASSIGN cContainerCode = goCntQuestionnaire:ContainerCode.
  
&IF {&DBDFMA} >= 10195 &THEN
    DATASET dsClinicalDocs:EMPTY-DATASET().
    
  DO iRow = 1 TO INTEGER(GET-VALUE(goCntQuestionnaire:ContainerCode + "_rowsrendered":U)):
    CREATE tt_clinical_docs.
    ASSIGN
    tt_clinical_docs.line_number               = iRow                                                                                            
    tt_clinical_docs.record_action             =         GET-VALUE("fcAction":U               + cContainerCode + STRING(iRow))                   
    tt_clinical_docs.document_type             =         GET-VALUE("cbDocumentTypeArgument":U + cContainerCode + STRING(iRow))                   
    tt_clinical_docs.owning_entity_mnemonic    = "hacat":U                                                                                       
    tt_clinical_docs.owning_obj                = DECIMAL(goWob:CurrentObj)
    tt_clinical_docs.related_entity_mnemonic   = "qumqr":U                                                                                       
    tt_clinical_docs.related_obj               = DECIMAL(GET-VALUE("fdRelatedObj":U           + cContainerCode + STRING(iRow)))                  
    tt_clinical_docs.related_value             =         GET-VALUE("fcRelatedValue":U         + cContainerCode + STRING(iRow))                   
    tt_clinical_docs.document_source           =         GET-VALUE("cbSource":U               + cContainerCode + STRING(iRow))                   
    tt_clinical_docs.insurer_obj               = DECIMAL(GET-VALUE("fdClientObj":U            + cContainerCode + STRING(iRow)))                  
    tt_clinical_docs.option_code               = INTEGER(GET-VALUE("fcOptionCode":U           + cContainerCode + STRING(iRow)))                  
    tt_clinical_docs.document_summary          =         GET-VALUE("fcSummary":U              + cContainerCode + STRING(iRow))                   
    tt_clinical_docs.insurer_code              =         GET-VALUE("fcClientCode":U           + cContainerCode + STRING(iRow))                   
    
    tt_clinical_docs.clinical_docs_obj         = IF   DECIMAL(GET-VALUE("fdClinicalDocObj":U  + cContainerCode + STRING(iRow))) > 0 
                                                 THEN DECIMAL(GET-VALUE("fdClinicalDocObj":U  + cContainerCode + STRING(iRow))) 
                                                 ELSE iRow * -1                                                                                  
    
    tt_clinical_docs.effective_date            = IF        GET-VALUE("fdEffectiveDate":U      + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U 
                                                 THEN DATE(GET-VALUE("fdEffectiveDate":U      + cContainerCode + STRING(iRow)))                 
                                                 ELSE ?                                                                                          
    
    tt_clinical_docs.end_date                  = IF        GET-VALUE("fdEndDate":U            + cContainerCode + STRING(iRow)) <> "yyyy/mm/dd":U              
                                                 THEN DATE(GET-VALUE("fdEndDate":U            + cContainerCode + STRING(iRow)))                        
                                                 ELSE ?
    .
    VALIDATE tt_clinical_docs.    
    
  END. /* DO iRow = 1 TO ... */   
  
   mipEnv:Health:maClinicalDoc:saveClinicalDocuments(INPUT-OUTPUT DATASET dsClinicalDocs BY-REFERENCE) NO-ERROR.
  
  ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_clinical_docs_error).
  
  FOR EACH tt_clinical_docs_error NO-LOCK:
    ASSIGN gcErrorMessage = gcErrorMessage + "<br>" + tt_clinical_docs_error.error_message .
  END. 
&ENDIF
  
  {mip/inc/mipcatcherror.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebAcceptRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebAcceptRequest Procedure 
PROCEDURE WebAcceptRequest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
    goWob:CurrentObj = "" WHEN GET-VALUE("WobOrigin":U) = "menu":U
    goWob:CurrentObj = "" WHEN goWob:CurrentObj = "0":U
    gdAuthTypeProviderObj = DECIMAL(get-value("fcDetailObj":U)).
  
  RUN SUPER.  
 
  goWob:setItem(goWob:ObjectCode + ":AuthTypeProviderObj":U         , STRING(gdAuthTypeProviderObj)).

  FOR EACH tt_sequence EXCLUSIVE-LOCK:
    DELETE tt_sequence.
  END.

 {mip/inc/mipcatcherror.i} 

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebRenderProcedure Procedure 
PROCEDURE WebRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ipoControl  AS cls.mipwscontrol NO-UNDO.
 DEFINE VARIABLE lExistingRecord    AS LOGICAL          NO-UNDO.
 DEFINE VARIABLE lSuccess           AS LOGICAL          NO-UNDO.
 DEFINE VARIABLE dAuthTypeProviderObj   AS DECIMAL          NO-UNDO.
 DEFINE VARIABLE cImageSrc          AS CHARACTER        NO-UNDO.
 DEFINE VARIABLE cClassName         AS CHARACTER        NO-UNDO.
 DEFINE VARIABLE cAuthTypeProviderObj   AS CHARACTER        NO-UNDO.
 
 RUN SUPER(ipoControl).
 
 CASE ipoControl:RenderArgument:
    
    WHEN "KeyField":U  THEN 
    DO:
      ASSIGN 
        lExistingRecord         =     DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.auth_type_provider_obj":U, "BUFFER-VALUE":U)) > 0 
                                  AND DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.auth_type_provider_obj":U, "BUFFER-VALUE":U)) <> ?
                                  
        ipoControl:ControlToken = (IF lExistingRecord 
                                   THEN 
                                     (IF ipoControl:ControlType = "wsLookupButton" 
                                      THEN "hidden":U 
                                      ELSE "Disabled":U) 
                                   ELSE "Updatable":U)
                                   
        ipoControl:ControlClass = ipoControl:ControlClass + (IF lExistingRecord THEN " -clMan":U ELSE "":U)
        .
        
      ipoControl:render().
                           
    END. /* WHEN "KeyField":U  THEN */
   
    WHEN "WebRenderProcedure":U THEN 
    DO:
    
      CASE goWob:Mode:
        WHEN "Maint":U THEN
        DO:
          
          {&OUT} "<center>".
          
&IF {&DBDFMA} >= 10195 &THEN
          
          ASSIGN lSuccess = (IF goWob:SubmitValue <> "ADD":U THEN goCntQuestionnaire:renderContainer("":U, "TABLE":U, FALSE , 1, ?) ELSE TRUE).
&ENDIF
          ASSIGN lSuccess = (IF goWob:SubmitValue <> "ADD":U THEN goCntAudit:renderContainer("":U, "TABLE":U, TRUE, 1, ?) ELSE TRUE).
          
          {&OUT} "</center>".
        END. /* WHEN "Maint":U THEN */
        
      END CASE.  /* CASE goWob:Mode: */
    END. /* WHEN "WebRenderProcedure":U */

    WHEN "ModifyButtonRender" THEN
        DO:
          ASSIGN 
            dAuthTypeProviderObj            = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.auth_type_provider_obj":U, "BUFFER-VALUE":U))
            
            cImageSrc                    = WarpSpeed:ThemePath + "img/modify.png":U
            
            cClassName                   = (IF dAuthTypeProviderObj <= 0.00 THEN "clHid":U ELSE "":U)
            
            cAuthTypeProviderObj             = (IF dAuthTypeProviderObj <= 0.00 THEN "[obj]":U ELSE STRING(dAuthTypeProviderObj))
            
            ipoControl:ControlJavascript = " onmouseover=~"$(this).fadeTo('fast', 0.7);~" onmouseout=~"$(this).fadeTo('fast', 1);~"":U

            ipoControl:JavascriptOnClick = "$(this).closest(~"tr~").find('[name^=~"buEditAuthTypeProvider~"]').trigger('click');".
                  

          {&OUT}

            "<a href='#' name='LeftButtonUpdate' class = '" + cClassName + "'$(this).find(~"img~").click();'>"
            "<img name = '":U + ipoControl:InstanceName + "' src='":U + cImageSrc + "' onclick = " + ipoControl:JavascriptOnClick + " class='' title='":U + mipEnv:miUtility:encode-url(ipoControl:ControlTooltip, "html":U) + "'" + (IF ipoControl:ControlTabOrder = 0 THEN "":U ELSE " tabindex='" + STRING(ipoControl:ControlTabOrder) + "' ") + " ></img>"
            "</a>":U
            .
         
        END.
  END CASE.  /* CASE ipoControl:RenderArgument: */

  
  { mip/inc/mipcatcherror.i }  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnGetUserCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnGetUserCodes Procedure 
FUNCTION fnGetUserCodes RETURNS CHARACTER
  ( INPUT  ipcObjList      AS CHARACTER,
    INPUT  ipcMnemonic     AS CHARACTER,
    INPUT  ipcCodeList     AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iCount  AS INTEGER     NO-UNDO.
  
  IF ipcObjList <> "":U
  THEN 
  DO iCount = 1 TO NUM-ENTRIES(ipcObjList) :
  
    mipEnv:miDBEntity:focusTable(ipcMnemonic). 
    mipEnv:miDBEntity:findRecord(DECIMAL(ENTRY(iCount, ipcObjList))).
   
    IF mipEnv:miDBEntity:RecordAvailable 
    THEN ASSIGN cReturn = trim(cReturn) + trim(mipEnv:miDBEntity:RecordCode) + ",":U. 
  END.
  ELSE 
    ASSIGN cReturn = ipcCodeList.
  
  ASSIGN cReturn = TRIM(cReturn, ",":U).
            
  RETURN cReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

