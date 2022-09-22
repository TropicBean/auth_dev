&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*-----------------------------------------------------------------------
    Purpose: Healthcare Auth Coding UI Service stack
    
    Author : MMP

  ----------------------------------------------------------------------- */

/*---------------------------  Definitions  ----------------------------- */
USING Progress.Json.ObjectModel.*   FROM PROPATH .

CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation }

{ ma/inc/maauthds.i }

{ ma/inc/maauthtypeconfigtt.i }

DEFINE TEMP-TABLE tt-additionalicd
   FIELD diagnosis  LIKE dclaim.diagnosis
   FIELD ass-code   LIKE dclaim.ass-code
   FIELD morph-code LIKE dclaim.morph-code.

DEFINE VARIABLE giOption                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE glNotePerAuthStatusRule      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcNotePerAuthStatusRuleValue AS CHARACTER   NO-UNDO.
DEFINE VARIABLE glBodyRegionRule             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcBodyRegionRuleValue        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE glAuthTypeEnableBodyRegion   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE glIcdCptTypePairRule         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcIcdCptTypePairRuleValue    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE glCptPmbIndicatorRule        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcCptPmbIndicatorRuleValue   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE glIcdPrimaryCodeRule         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcIcdPrimaryCodeRuleValue    AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnGetQueryFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnGetQueryFieldList Procedure 
FUNCTION fnGetQueryFieldList RETURNS CHARACTER
  ( INPUT ipoContainer AS cls.mipwscontainer )  FORWARD.

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
         HEIGHT             = 12.14
         WIDTH              = 69.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveCodingContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveCodingContainer Procedure 
PROCEDURE ajaxSaveCodingContainer :
/*------------------------------------------------------------------------------
  Purpose   : Authorisation Coding Container Auto Save Procedure
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
   
   { ma/app/maauthcoduiajaxsave.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidateECC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidateECC Procedure 
PROCEDURE ajaxValidateECC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE oResponseHelper  AS cls.maajaxresponsehelper  NO-UNDO.
  DEFINE VARIABLE oSearch          AS cls.maauthsearch          NO-UNDO.

  DEFINE VARIABLE dAuthObj         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE cDiagnosis       AS CHARACTER                 NO-UNDO.              
  DEFINE VARIABLE cAssCode         AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE cFieldList       AS CHARACTER                 NO-UNDO.              
  DEFINE VARIABLE cValueList       AS CHARACTER                 NO-UNDO.              
  DEFINE VARIABLE cMorphCode       AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE tClaimDate       AS DATE                      NO-UNDO.        
  DEFINE VARIABLE iSchemeCode      AS INTEGER                   NO-UNDO.        
  DEFINE VARIABLE cMemNum          AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE cDependant       AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE cPrType          AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE iDocNum          AS INTEGER                   NO-UNDO.        
  DEFINE VARIABLE lPrimary         AS LOGICAL   FORMAT "Y/N"    NO-UNDO.        
  DEFINE VARIABLE lCheckEcc        AS LOGICAL   FORMAT "Y/N"    NO-UNDO.        
  DEFINE VARIABLE lValidateMIT     AS LOGICAL   FORMAT "Y/N"    NO-UNDO.        
  DEFINE VARIABLE lSuccess         AS LOGICAL   FORMAT "Y/N"    NO-UNDO.        
  DEFINE VARIABLE cNote            AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE cPrimaryCode     AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE cDiagnosisType   AS CHARACTER                 NO-UNDO.        
  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject         NO-UNDO.        

  EMPTY TEMP-TABLE tt-additionalicd.   
  DATASET dsAuthorisation:EMPTY-DATASET().
        
  ASSIGN oSearch         = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE) 
         oResponseHelper = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
         
         cFieldList      = get-value('FldLst':U)
         cValueList      = get-value('ValList':U)
         dAuthObj        = DECIMAL(mipEnv:miExpression:getNVPElement(cFieldList, cValueList, "[AuthObj]":U, ",":U , ",":U))
         
         cNote          = "":U
         cPrimaryCode   = "":U
         cDiagnosisType = "":U . 


  IF  dAuthObj <> ? 
  AND dAuthObj  > 0  THEN
  DO:
  
    ASSIGN lSuccess  = oSearch:SetFilterCriteria("tt_auth.auth_obj":U, "=":U, dAuthObj) 
           lSuccess  = oSearch:SetCriteria("BufferList":U , "tt_auth,tt_auth_coding":U)
           lSuccess  = oSearch:SetCriteria("DataSecured":U, "FALSE":U)
                     
           lSuccess  = oSearch:SetFilterCriteria("tt_auth_coding.owning_entity_mnemonic":U, "=":U, "diagnos":U)
                     
           lSuccess  = oSearch:fetchData() NO-ERROR.

    FOR EACH tt_auth_coding
       WHERE tt_auth_coding.primary_code <> TRUE
         AND tt_auth_coding.owning_entity_mnemonic = "diagnos":U:
        
        CREATE tt-additionalicd.
        ASSIGN tt-additionalicd.diagnosis  = tt_auth_coding.owning_alt_value.

        VALIDATE tt-additionalicd.

    END. /* FOR EACH tt_auth_coding */
    
    FIND FIRST tt_auth 
         WHERE tt_auth.auth_obj = dAuthObj NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
    
    FIND FIRST tt_auth_coding 
         WHERE tt_auth_coding.auth_obj     = dAuthObj 
           AND tt_auth_coding.primary_code = TRUE NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

    IF  AVAILABLE tt_auth_coding
    AND AVAILABLE tt_auth    THEN
    DO:
      ASSIGN cDiagnosis = tt_auth_coding.owning_alt_value 
             tClaimDate = tt_auth_coding.start_date
             lCheckEcc  = TRUE .
    
      mipEnv:Health:maMedical:validateICD10( INPUT TABLE tt-additionalicd BY-REFERENCE, 
                                             INPUT    cDiagnosis,          /* ipcDiagnosis      */              
                                             INPUT    "":U,                /* ipcAssCode        */          
                                             INPUT    "":U,                /* ipcMorphCode      */          
                                             INPUT    tClaimDate,          /* iptClaimDate      */          
                                             INPUT    tt_auth.option_code, /* ipiSchemeCode     */          
                                             INPUT    "":U,                /* ipcMemNum         */          
                                             INPUT    "":U,                /* ipcDependant      */          
                                             INPUT    "":U,                /* ipcPrType         */          
                                             INPUT    ?,                   /* ipiDocNum         */          
                                             INPUT    TRUE,                   /* iplPrimary        */         
                                             INPUT    lCheckEcc,           /* iplCheckEcc       */   
                                             INPUT    TRUE,                /* iplValidateMIT    */           
                                             OUTPUT   cNote,               /* opcNote           */      
                                             OUTPUT   cPrimaryCode,        /* opcPrimaryCode    */           
                                             OUTPUT   cDiagnosisType ,     /* opcDiagnosisType  */       
                                             OUTPUT   oErrorObject ) .     /* opoErrorObject    */   

      ASSIGN lSuccess = oResponseHelper:setError(oErrorObject) NO-ERROR.

    END. /* IF AVAILABLE tt_auth_coding  */       
  END. /* IF  dAuthObj <> ? AND dAuthObj  > 0 */

  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oSearch)         THEN DELETE OBJECT oSearch         NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oErrorObject)    THEN DELETE OBJECT oErrorObject    NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                                                                
                                        DATASET dsAuthorisation:EMPTY-DATASET().
                                        EMPTY TEMP-TABLE tt-additionalicd.
                                        "}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidationCoding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationCoding Procedure 
PROCEDURE ajaxValidationCoding :
/*------------------------------------------------------------------------------
  Purpose   : Auth Coding Container Ajax Validation    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/

  {ma/app/maauthcoduiajaxval.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntConddiag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntConddiag Procedure 
PROCEDURE getCntConddiag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oControl   AS cls.mipwscontrol NO-UNDO.
                           
  ASSIGN 
    opoContainer                   = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
  
    opoContainer:ContainerTitle    = "Condition ICD Information":U
    opoContainer:RowsToRender      = ?
    opoContainer:Collapsable       = TRUE
    opoContainer:Borders           = "TRUE":U
    opoContainer:ViewOnly          = TRUE
    opoContainer:QueryString       = "FOR EACH tt_conddiag NO-LOCK WHERE tt_conddiag.cond-code = '&1' ":U

    oControl                       = opoContainer:addControl("fcConddiagICD"     ,"wsInput":U    , "10":U,   "tt_conddiag.icd-code":U        ,"character":U, 1, "ICD Code":U)
    oControl                       = opoContainer:addControl("fcConddiagICDDesc" ,"wsTextarea":U , "20,5":U, "tt_conddiag.icd-description":U ,"character":U, 2, "ICD Description":U)

    oControl                       = opoContainer:addControl("fcConddiagComment" ,"wsTextarea":U,  "20,5":U, "tt_conddiag.comment":U  ,       "character":U, 3, "Comment":U)
    oControl                       = opoContainer:addControl("fcConddiagAsterisk","wsInput":U  ,   "10":U,   "tt_conddiag.ass-code":U  ,      "character":U, 4, "Astersik Code":U) .
                     
                                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntDepCond) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntDepCond Procedure 
PROCEDURE getCntDepCond :
/*------------------------------------------------------------------------------
  Purpose   : WebFormDefinition for Conditions  
  Parameters: 
  Notes     :   
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oControl   AS cls.mipwscontrol NO-UNDO.
                           

  /*Create Conditions Table container which becomes an object on goCntMaint*/
  /*Thi is to get the buttons to attach to the container and not look like they're seperate */
  ASSIGN 
    opoContainer                   = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
  
    opoContainer:ContainerTitle    = "Dependant Registered Conditions(Comorbidities)":U
    opoContainer:Collapsable       = TRUE
    opoContainer:Borders           = "TRUE":U
    opoContainer:ViewOnly          = TRUE
    opoContainer:QueryString       = "FOR EACH tt_depcond NO-LOCK WHERE tt_depcond.mem-num = &1 " 
                                   + " , FIRST tt_memdep NO-LOCK WHERE tt_memdep.mem-num  = tt_depcond.mem-num":U
                                   + "     AND tt_memdep.dependant = tt_depcond.dependant OUTER-JOIN ":U
                                   + " , FIRST condition NO-LOCK WHERE condition.cond-code = tt_depcond.cond-code OUTER-JOIN ":U

    opoContainer:RowsToRender      = ?

    oControl                       = opoContainer:addControl("fcDependant":U,           "wsInput":U, "20":U, "tt_depcond.dependant":U,                   "integer":U,   1,  "Dependant")
    oControl:RenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle 
    oControl:RenderProcedure       = "RenderProcedure"                                                                                            
    oControl:RenderArgument        = "DepFormat"                                                                                                     
                                                                                                                                                       
    oControl                       = opoContainer:addControl("fcMemName":U,             "wsInput":U, "20":U, "tt_memdep.first-Name":U,                   "character":U, 2,  "Name")
    oControl                       = opoContainer:addControl("fcCondDesc":U,            "wsInput":U, "20":U, "condition.description[1]":U,               "character":U, 3,  "Condition")
                                                                                                                                                            
    oControl                       = opoContainer:addControl("fcCondCode":U,            "wsInput":U, "20":U, "tt_depcond.cond-code":U,                   "character":U, 4,  "Condition Code")
                                                                                                                                                            
    oControl                       = opoContainer:addControl("fcDocNum":U,              "wsInput":U, "20":U, "tt_depcond.doc-num":U,                     "integer":U,   5,  "Serv Prov")
                                                                                                                                                              
    oControl                       = opoContainer:addControl("fcAuthNum":U,             "wsInput":U, "20":U, "tt_depcond.auth-num":U,                    "integer":U,   6,  "Auth Num")
    oControl:RenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure       = "RenderProcedure"                                                                                            
    oControl:RenderArgument        = "AuthFormat"                                                                                                    
                                                                                                                                                         
    oControl                       = opoContainer:addControl("fcStartDate":U,           "wsInput":U, "20":U, "tt_depcond.start-date":U,                  "date":U,      7,  "Start Date")
    oControl                       = opoContainer:addControl("fcEndDate":U,             "wsInput":U, "20":U, "tt_depcond.end-date":U,                    "date":U,      8,  "End Date")
    oControl                       = opoContainer:addControl("fcReference":U,           "wsInput":U, "20":U, "tt_depcond.reference-auth-num":U,          "character":U, 9,  "Reference")
    
    oControl                       = opoContainer:addControl("fcDisplayExtraDetails":U, "wsInput":U, "20":U, "tt_depcond._display-extra-details":U,      "character":U, 10, "Extra Details":U)
    oControl:ControlToken          = "Hidden":U

    oControl                       = opoContainer:addControl("fcExtraDetails":U,        "wsInput":U, "20":U, "tt_depcond.extra-details":U,               "character":U, 10, "Extra Details":U)
    oControl:RenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure       = "RenderProcedure":U
    oControl:RenderArgument        = "DisplayExtraDetails":U
    oControl:CellLayoutMask        = "&1&2":U

    oControl                       = opoContainer:addControl("fcRiskRatingCond":U,      "wsInput":U, "20":U, "tt_depcond.clinical-risk-rating":U,        "character":U, 12, "Clinical<br>Risk Rating")
    oControl:RenderProcedure       = "RenderProcedure":U                                                                                                                  
    oControl:RenderArgument        = "AcronymSelect:ma_acRiskRating":U 
               
    oControl                       = opoContainer:addControl("fcViewDiag":U,            "wsHref":U,  "10":U, "":U,                                       "character":U, 13, "":U)
    oControl:LinkText              = "View Diagnosis"    
    oControl:RenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle 
    oControl:RenderProcedure       = "RenderProcedure" 
    oControl:RenderArgument        = "DiagnosisLink" .
  
  { mip/inc/mipcatcherror.i } 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdCodingCpt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdCodingCpt Procedure 
PROCEDURE getCntUpdCodingCpt :
/*------------------------------------------------------------------------------
  Purpose   : Clinical update container definition   
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/

  { ma/app/maauthcodgetcntupdcodcpt.i }
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdCodingHolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdCodingHolder Procedure 
PROCEDURE getCntUpdCodingHolder :
/*------------------------------------------------------------------------------
  Purpose   : Clinical update container definition   
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName + "Parent":U , "99%":U, "":U, WarpSpeed:BaseClass, TRUE)                                       
    opoContainer:ContainerTitle        = "Coding Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = TRUE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue 
        
    oControl                           = opoContainer:addControl("ICDContainer":U    + ipcContainerName   , "":U , "":U  , "":U, "":U , 3, 1, "":U)                                                                                                                               
    oControl:SubContainer              = mipEnv:Health:maUiService:getCntUpdCodingICD("ICD":U + ipcContainerName)
    oControl:SubContainerType          = "TABLE":U
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2
    oControl:CellSnippet               = "align='center'":U
    oControl:ControlToken              = "hidden":U
    
    oControl                           = opoContainer:addControl("ContainerSpacer":U + ipcContainerName   , "":U , "":U  , "":U, "":U , 4, 1, "":U)
    oControl:CellSnippet               = "height='20px'":U 
    oControl                           = opoContainer:addControl("CPTContainer":U    + ipcContainerName   , "":U , "":U  , "":U, "":U , 5, 1, "":U)                                                                                                                               
    oControl:SubContainer              = mipEnv:Health:maUiService:getCntUpdCodingCPT("CPT":U + ipcContainerName)
    oControl:SubContainerType          = "TABLE":U
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2
    oControl:CellSnippet               = "align='center'":U
    oControl:ControlToken              = "hidden":U
    .


&ENDIF
  
  { mip/inc/mipcatcherror.i } 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdCodingICD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdCodingICD Procedure 
PROCEDURE getCntUpdCodingICD :
/*------------------------------------------------------------------------------
  Purpose   : Clinical update container definition   
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  
  { ma/app/maauthcodgetcntupdcodicd.i }

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
  
  { ma/app/maauthcodrenderprocedure.i }          
    

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
  DEFINE INPUT PARAMETER ipoContainer       AS cls.mipwscontainer NO-UNDO.
                                            
  DEFINE VARIABLE oBodyRegion               AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oCptCode                  AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oProcedureDate            AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oStatus                   AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oStatusNote               AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oStatusNoteBtn            AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oPMBView                  AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oPMBIndicator             AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oCodingType               AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oCodingTypeRule           AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oQuery                    AS cls.mipquery       NO-UNDO.
  DEFINE VARIABLE hWob                      AS HANDLE             NO-UNDO.
  DEFINE VARIABLE dAuthObj                  AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE dInsurerObj               AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj              AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE dStartDate                AS DATE               NO-UNDO.
  DEFINE VARIABLE dAuthStartDate            AS DATE               NO-UNDO.
  DEFINE VARIABLE cMemNum                   AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cProcedureDateAction      AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cStatus                   AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cStatusNote               AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cBodyRegionRuleValue      AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cIcdCptTypePairRuleValue  AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE lEnableProcedureDate      AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lMandatory                AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lBodyRegionRule           AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lIcdCptTypePairRule       AS LOGICAL            NO-UNDO.
  
  /*
    RowRenderProcedure will be run in the current wob to attach any business logic validation errors 
    generated during a submit to this container. RowRenderProcedure container code which should be 
    applied to this container ragardless of which wob this container is used in should be placed in 
    the case block below.
  */
  IF LOOKUP(ipoContainer:RowRenderArgument, "AuthCPTCodingContainer,AuthICDCodingContainer":U) = 0 THEN
  DO:
    RUN SUPER(INPUT ipoContainer) NO-ERROR.
  
    /* 
      The super may not have a rowRenderProcedure 
    */
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6439' &ResetIgnoredErrors = TRUE }      
       
  END. /*IF LOOKUP(ipoContainer:RenderArgument, "AuthCPTCodingContainer,AuthICDCodingContainer":U) = 0 THEN*/
  ELSE
  DO:

    ASSIGN hWob = mipEnv:miProcedure:getProcedureHandle(INPUT "Wob_":U + WarpSpeed:CurrentWob).
    
    /*
      If the wob is not run persistently it will be added as a super to the ui service render procedure handle
    */
    IF NOT VALID-HANDLE(hWob) 
    THEN 
      ASSIGN hWob = wsUIService:RenderProcedureHandle.
    

    IF VALID-HANDLE(hWob) 
    THEN 
      RUN rowRenderProcedure IN hWob(INPUT ipoContainer) NO-ERROR.
    

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6456,PROGRESS:3234' &ResetIgnoredErrors = FALSE }

  END. /*ELSE*/
  
&IF {&DBDFMA} >= 010195 &THEN  
  
  CASE ipoContainer:RowRenderArgument:
  
    WHEN "AuthICDCodingContainer":U OR WHEN "AuthCPTCodingContainer":U THEN
    DO:
      ASSIGN oQuery          = ipoContainer:ContainerQuery
      
             dAuthObj        = DECIMAL(oQuery:getFieldAttribute("tt_auth.auth_obj":U                  , "BUFFER-VALUE":U)) 
             dAuthTypeObj    = DECIMAL(oQuery:getFieldAttribute("tt_auth.auth_type_obj":U             , "BUFFER-VALUE":U)) 
             dInsurerObj     = DECIMAL(oQuery:getFieldAttribute("tt_auth.insurer_obj":U               , "BUFFER-VALUE":U)) 
                                                                                                  
             dStartDate      =    DATE(oQuery:getFieldAttribute("tt_auth_coding.start_date":U         , "BUFFER-VALUE":U))     
                      
             dAuthStartDate  =    DATE(oQuery:getFieldAttribute("tt_auth.start_date":U                , "BUFFER-VALUE":U))                           
             
             cMemNum         =         oQuery:getFieldAttribute("tt_auth.mem_num":U                   , "BUFFER-VALUE":U)  
             cStatus         =         oQuery:getFieldAttribute("tt_auth_coding.coding_status":U      , "BUFFER-VALUE":U)
             cStatusNote     =         oQuery:getFieldAttribute("tt_auth_coding.coding_status_note":U , "BUFFER-VALUE":U)
             
             dAuthStartDate  =    DATE(oQuery:getFieldAttribute("tt_auth.start_date":U                , "BUFFER-VALUE":U))

             dInsurerObj     = (IF dInsurerObj = ? 
                                THEN 0.00
                                ELSE dInsurerObj)
             
             dStartDate      = (IF dStartDate = ?
                                THEN TODAY 
                                ELSE dStartDate)

             dAuthStartDate  = (IF dAuthStartDate = ?
                                THEN TODAY 
                                ELSE dAuthStartDate).              
                              
      /*
        For performance reasons, there are some things we need to only do once, so common variables
        will be set/determined once when rendering the first row
      */
      IF ipoContainer:CurrentRow = 1 THEN 
      DO:
        /*
          Get member option code required for services below
        */
        mipEnv:Health:maMember:getMemberOption(INPUT cMemNum, INPUT dStartDate, OUTPUT giOption).
                                             

        /*
          Get the ICD and CPT Coding type Pairing type list
        */
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                       INPUT  giOption,
                                                       INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                       INPUT  "IcdCptTypePairing":U,
                                                       INPUT  dStartDate,
                                                       OUTPUT glIcdCptTypePairRule,
                                                       OUTPUT gcIcdCptTypePairRuleValue).


        /*
          Get the PMB Indicator Rule
        */
         mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                        INPUT  giOption,
                                                        INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                        INPUT  "CptPmbIndicator":U,
                                                        INPUT  dStartDate,
                                                        OUTPUT glCptPmbIndicatorRule,
                                                        OUTPUT gcCptPmbIndicatorRuleValue).

         mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                        INPUT  giOption,
                                                        INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                        INPUT  "ICDPrimaryCode":U,
                                                        INPUT  dStartDate,
                                                        OUTPUT glIcdPrimaryCodeRule,
                                                        OUTPUT gcIcdPrimaryCodeRuleValue).



      END. /*IF ipoContainer:CurrentRow = 1 THEN */
      
      /*
        Check if the current record requires a reason depending on the status
      */
      ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), INPUT dInsurerObj, INPUT giOption, INPUT dStartDate).
            
      /*
        Lets get all the required controls
      */
      ASSIGN oStatusNote     = ipoContainer:getControl("fcReason":U             + ipoContainer:ContainerCode)
             oStatusNoteBtn  = ipoContainer:getControl("buReasonBtn":U          + ipoContainer:ContainerCode)
             oCodingType     = ipoContainer:getControl("cbCodingType":U         + ipoContainer:ContainerCode)
              .      

      IF ipoContainer:RowRenderArgument = "AuthICDCodingContainer":U
      THEN 
        ASSIGN oCodingTypeRule              = ipoContainer:getControl("fcCodingTypeRule":U + ipoContainer:ContainerCode)
               oCodingTypeRule:ControlValue = IF glIcdPrimaryCodeRule THEN gcIcdPrimaryCodeRuleValue ELSE "":U.
      
      /*
        Set note description as tooltip on note controls and set note control token according to auth type config
      */
      ASSIGN oStatusNote:ControlClass    = oStatusNote:ControlClass 
                                         + (IF lMandatory THEN " +":U ELSE " -":U) + "clMan":U

             oStatusNote:ControlToken    = (IF lMandatory THEN "Updatable":U ELSE "Disabled":U)
             oStatusNoteBtn:ControlToken = (IF lMandatory THEN "Updatable":U ELSE "Disabled":U).
                                      

      IF ipoContainer:RowRenderArgument = "AuthCPTCodingContainer":U THEN
      DO:

        /* 
          Determine if the procedure date is included in the container 
        */
        ASSIGN
          oCptCode                         = ipoContainer:getControl("fcOwningAltValue":U  + ipoContainer:ContainerCode)
          oProcedureDate                   = ipoContainer:getControl("fdProcedureDate":U   + ipoContainer:ContainerCode).
          
        /*
          Determine if the procedure date must be enabled for the procedure(CPT) code 
        */
        mipEnv:Health:Authbusinesslogic:activateProcedureDate(INPUT  dInsurerObj,
                                                              INPUT  giOption,
                                                              INPUT  dStartDate,
                                                              INPUT  oCptCode:ControlValue ,
                                                              INPUT  "":U ,
                                                              OUTPUT lEnableProcedureDate,
                                                              OUTPUT cProcedureDateAction).
        
        /* Contain date field within a div tag so we can easily hide and show both the input and date picker icon */

        ASSIGN oProcedureDate:CellLayoutMask = (IF NOT lEnableProcedureDate OR oCptCode:ControlValue = ""
                                                THEN "<div class='clHid'>&1</div>":U
                                                ELSE "<div>&1</div>":U).

        /*
          For performance reasons, there are some things we need to only do once, so common variables
          will be set/determined once when rendering the first row
        */
        IF ipoContainer:CurrentRow = 1 THEN 
        DO:
          
          /* Check if body region is enabled by rule and auth type */       
          mipEnv:Health:AuthMaintenance:getAuthRuleValue
            (INPUT  dInsurerObj,
             INPUT  giOption,
             INPUT  "ma_acAuthRuleTypeAuthCoding":U,
             INPUT  "CPTBodyRegion":U,
             INPUT  dAuthStartDate,
             OUTPUT glBodyRegionRule,
             OUTPUT gcBodyRegionRuleValue).
          
          mipEnv:Health:AuthService:getAuthTypeConfig 
            (INPUT dAuthTypeObj,                      
             INPUT dInsurerObj,                       
             INPUT giOption,                       
             INPUT dAuthStartDate,                   
             INPUT-OUTPUT TABLE ttAuthTypeConfig). 
          
          FIND FIRST ttAuthTypeConfig EXCLUSIVE-LOCK NO-ERROR.
          
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
                
          IF AVAILABLE ttAuthTypeConfig 
          THEN
            ASSIGN glAuthTypeEnableBodyRegion = ttAuthTypeConfig.ActivateBodyRegion .
          
          ASSIGN oBodyRegion = ipoContainer:getControl("fcBodyRegion":U  + ipoContainer:ContainerCode) .
          
          /*
            Check if body region is enabled , first for rule and then for auth type
          */       
          IF  gcBodyRegionRuleValue = "Activate":U THEN
          DO:
            /* rule exists, now check if body region is enabled for auth type */
            IF glAuthTypeEnableBodyRegion 
            THEN
              ASSIGN 
                oBodyRegion:ControlToken = "updatable":U 
                oBodyRegion:ControlClass = "+clMan":U.
            ELSE
              ASSIGN 
                oBodyRegion:ControlToken = "disabled":U 
                oBodyRegion:ControlClass = "-clMan +clDisabled":U.
          
          END. /* IF  gcBodyRegionRuleValue = "Activate"  */
          ELSE  
            ASSIGN
              oBodyRegion:ControlToken  =  "disabled":U
              oBodyRegion:ControlClass  =  "-clMan +clDisabled":U.  
        END. /* IF ipoContainer:CurrentRow = 1 */
      END. /* IF ipoContainer:RowRenderArgument = "AuthCPTCodingContainer":U */                                                            
    END. /*WHEN "AuthICDCodingContainer":U OR WHEN "AuthCPTCodingContainer":U THEN*/
  END CASE.

&ENDIF

  { mip/inc/mipcatcherror.i 
    &FINALLY=" EMPTY TEMP-TABLE ttAuthTypeConfig."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_defineSharedControls) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _defineSharedControls Procedure 
PROCEDURE _defineSharedControls PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Clinical update container definition   
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  

&IF {&DBDFMA} >= 10195 &THEN

  ASSIGN 
    opoContainer                         = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, FALSE)                                       
    opoContainer:ViewOnly                = FALSE
    opoContainer:NoDataMessage           = "Please specify coding information in the empty line provided above":U
    opoContainer:ShowContainerSettings   = FALSE
    opoContainer:ContainerMode           = Warpspeed:SubmitValue
    
    oControl                             = opoContainer:addControl("fiLineNumber":U               + ipcContainername, "wsInput":U       , "25":U, "":U                                     , "INTEGER":U  ,  1, "#":U)
    oControl:RenderProcedureHandle       = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure             = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument              = "LineNumber":U                                                                                                                                  
    oControl:ControlToken                = "Hidden":U                                                                                   
                                                                                                                                        
    oControl                             = opoContainer:addControl("fdAuthCodingObj":U            + ipcContainerName, "wsInput":U       , "":U  , "tt_auth_coding.auth_coding_obj":U       , "CHARACTER":U,  2, "":U)
    oControl:JavascriptOnChange          = "fnOnChangeAuthCodingObj(this);":U                     
    oControl:ControlClass                = "+clObjControl +clHid":U
                                                                                                                                   
    oControl                             = opoContainer:addControl("flPrimaryCode":U              + ipcContainerName, "wsCheckBox":U    , "":U  , "tt_auth_coding.primary_code":U          , "LOGICAL":U  , 13, "Primary<br>Code":U)
    oControl:ControlToken                = "Hidden":U                                                                                                                                                                                                                       
                                                                                                                                                                                                                           
    oControl                             = opoContainer:addControl("cbStatus":U                   + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_coding.coding_status":U         , "INTEGER":U  , 14, "":U)
    oControl:ControlTooltip              = "Select a status from the drop-down list.":U
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle                                              
    oControl:RenderProcedure             = "RenderProcedure":U                                                                          
    oControl:RenderArgument              = "AuthStatusCombo":U                                                                          
    oControl:ControlClass                = "+clMan +clPreserveValue":U                                                                  
    oControl:JavaScriptOnChange          = "fnOnChangeAuthCodingStatus(this);":U                                                        
    oControl:AjaxValidation              = "SERVICE:maUIService:ajaxValidationCoding:Status":U                                          
    oControl:FilterFields                = "[Status],[AuthObj]":U                                                                                               
    oControl:FilterControls              = "cbStatus":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName                                    
    oControl:ReturnFields                = "[StatusReasonMandatory]":U                                                                  
    oControl:ReturnControls              = "flStatusReasonMandatory":U + ipcContainerName                                               
                                                                                                                                        
    oControl                             = opoContainer:addControl("flStatusReasonMandatory":U    + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 14, "Status":U)
    oControl:ControlToken                = "Hidden":U                                                                                   
    oControl:CellLayoutMask              = "&1&2":U         

    /* On status change, check whether reason is mandatory or not */ 
    oControl:JavaScriptOnChange          = "fnOnChangeAuthCodingStatus(this);":U
                                         + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"TBL~");":U 
                                                                                                                                        
    /* See RowRenderProcedure - Checks were moved to RowRenderProcedure for performance reasons */
    /* Check whether each reason control is mandatory or not depending on status                */
    oControl                             = opoContainer:addControl("fcReason":U                   + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_coding.coding_status_note":U    , "CHARACTER":U, 15, "":U)            
    oControl:ControlTooltip              = "Status Reason":U
    oControl:AjaxValidation              = "SERVICE:maUIService:ajaxValidationCoding:Reason":U                                                                                        
    oControl:FilterFields                = "[Status],[ReasonCode],[ReasonType],[StartDate]":U                                                                                                            
    oControl:FilterControls              =  "cbStatus":U + ipcContainerName + ",":U + "fcReason":U + ipcContainerName + ",fcReasonTypeArgument":U + ipcContainerName   + ",fdStartDate":U + ipcContainerName                                                                    
    oControl:ReturnFields                = "[ReasonDesc],[ReasonType]":U                                                                
    oControl:ReturnControls              = "fcReasonDesc":U + ipcContainerName  + ",fcReasonTypeArgument":U + ipcContainerName                                                                                                  
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle                                              
    oControl:RenderProcedure             = "RenderProcedure":U                                                                          
    oControl:RenderArgument              = "AuthStatusReason":U   

    oControl                             = opoContainer:addControl("fcReasonTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 15, "":U)                  
    oControl:ControlToken                = "Hidden":U                                                                                   
    oControl:ControlValue                = "AS":U                                                                                       
    oControl:ControlClass                = "+clPreserveValue":U                                                                                                                                      
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle                                              
    oControl:RenderProcedure             = "RenderProcedure":U                                                                          
    oControl:RenderArgument              = "AuthReasonType":U                                                                     
                                                                                                                                        
    oControl                             = opoContainer:addControl("fcReasonDesc":U               + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 15, "":U)            
    oControl:ControlToken                = "Hidden":U                   

    /* Set reason description as tooltip when reason is selected client side */ 
    oControl:JavascriptOnChange          = "fnSetReasonDescription(this, ~"fcReason" + ipcContainerName + "~", ~"buReasonBtn" + ipcContainerName + "~", ~"TBL~");":U              
                                                                                                                                        
    /*See RowRenderProcedure - Check whether 'AS*' or 'AS' + <Status Code> depending on rule            */
    /*The rendering was moved from a render procedure to a row render procedure for performance reasons */
  /*  oControl                             = opoContainer:addControl("fcReasonTypeArgument":U       + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 15, "":U)                  
    oControl:ControlToken                = "Hidden":U */        

    /* See RowRenderProcedure - Check whether each reason lookup is available or not depending on status */
    /* The rendering was moved from a render procedure to a row render procedure for performance reasons */
    oControl                             = opoContainer:addControl("buReasonBtn":U                + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 15, "Status<br>Reason":U)                                                                                                                                                                                                   
    oControl:CellLayoutMask              = "&1&2&3&4":U                                                                                                                                             
    oControl:LookupWobFLA                = "note":U                                                                                                                                               
    oControl:LookupFields                = "note.key":U                                                                                                                                           
    oControl:LookupControls              = "fcReason":U + ipcContainerName                                                                                                                        
    oControl:FilterFields                = "note.key,note.type":U                                                                                                                                     
    oControl:FilterControls              = "fcReason":U + ipcContainerName + ",":U 
                                         + "fcReasonTypeArgument":U + ipcContainerName                                                                                     
    oControl:ReturnFields                = "note.key":U                                                                                                                                           
    oControl:ReturnControls              = "fcReason":U + ipcContainerName 

    oControl                             = opoContainer:addControl("fcViewPMB":U                  + ipcContainerName, "wsHref":U        , "":U  , "":U                                     , "":U         , 16, "":U)
    oControl:LinkText                    = "PMB Details":U                                                                              
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle                                              
    oControl:RenderProcedure             = "RenderProcedure":U                                                                          
    oControl:RenderArgument              = "AuthCodingViewPMB":U /*Check if the PMB hyperlink should be enabled*/                       
                                                                                                                                        
    /* This field will be used dynamically enable     */                                                                                
    /* hyperlink when a record is created             */                                                                                
    oControl                             = opoContainer:addControl("flPmbLinkEnabled":U           + ipcContainerName, "wsInput":U       , "":U  , "":U                                     , "":U         , 17, "":U)    
    oControl:ControlToken                = "Hidden":U                                                                                   
    
    /* This function will hide/show the PMB hyperlink */                                                                                
    /* if PMB's are applicable for ICD captured       */                                                                                
    oControl:JavascriptOnChange          = "fnOnChangeAuthCodingPMBLinkEnabled(~"fcViewPMB~",this);":U                                  
                                                                                                                                        
    /* Used to check if the PMB indicator should be visible,enabled etc */                                                              
    oControl                             = opoContainer:addControl("fcPMBIndicatorState":U        + ipcContainerName, "wsInput":U       , "":U  , "":U                                     , "CHARACTER":U, 19, "":U)
    oControl:ControlToken                = "Hidden":U                                                                                   
                                                                                                                                        
    /* This function will hide/show/enable/disable the PMB indicator    */                                                              
    /* depending on rule configuration rule code "PMBDecision"          */                                                              
    oControl:JavascriptOnChange          = "fnOnChangeAuthCodingPMBIndicatorState(~"flPMBIndicator~",this);":U                          
                                                                                                                                                                                       
    oControl                             = opoContainer:addControl("fdStartDate":U                + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_coding.start_date":U            , "DATE":U     , 20, "Start Date":U)
    oControl:ControlClass                = "+clMan +clPreserveValue":U                                                                                                                                   
                                                                                                                                                                                        
    oControl                             = opoContainer:addControl("fdEndDate":U                  + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_coding.end_date":U              , "DATE":U     , 21, "End Date":U)                                                                                                                                                                                                  
                                                                                                 
    oControl                             = opoContainer:addControl("_authObjArgument":U           + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_coding.auth_obj":U              , "CHARACTER":U, 29, "":U)
    oControl:ControlToken                = "Hidden":U                                                                                   
                                                                                                                                        
    oControl                             = opoContainer:addControl("_oemArgument":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_coding.owning_entity_mnemonic":U, "CHARACTER":U, 30, "":U)
    oControl:ControlToken                = "Hidden":U
    .

&ENDIF
  
  { mip/inc/mipcatcherror.i } 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnGetQueryFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnGetQueryFieldList Procedure 
FUNCTION fnGetQueryFieldList RETURNS CHARACTER
  ( INPUT ipoContainer AS cls.mipwscontainer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iControl        AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cQueryFieldList AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE oControl        AS cls.mipwscontrol NO-UNDO.
  
  /*
    Run through all the controls and build a field name list and query field list from 
    all the controls that are of an input type eg. wsInput, wsTextArea, wsCombo, wsCheckBox
  */                                 
  DO iControl = 1 TO NUM-ENTRIES(ipoContainer:getControlNameList()):
  
    ASSIGN oControl = ipoContainer:getControl(ENTRY(iControl, ipoContainer:getControlNameList())).
    
    IF CAN-DO("wsInput,wsTextArea,wsCombo,wsCheckBox":U, oControl:ControlType)
    THEN
      ASSIGN cQueryFieldList = cQueryFieldList
      
                             + (IF cQueryFieldList = "":U
                                THEN "":U
                                ELSE ",":U)
                             
                             + (IF NUM-ENTRIES(oControl:ControlQueryField, ".":U) > 1 
                                THEN TRIM(ENTRY(2, oControl:ControlQueryField, ".":U)) 
                                ELSE "":U)   
                             
                             + "=":U 
                             
                             + oControl:ControlName 
                             .   
                                 
  END. /*DO iControl = 1 TO NUM-ENTRIES(ipoContainer:getControlNameList()):*/
  
  RETURN cQueryFieldList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

