&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Auth UI Service stack
    
    Author : Andrewd

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation }

{ ma/inc/maMemberHeadVar.i } 
{ ma/inc/maauthds.i }

{ ma/inc/maauthtypeds.i }

{ ma/inc/maauthtypeconfigtt.i }

DEFINE TEMP-TABLE tt_acronym LIKE mic_acronym.

DEFINE TEMP-TABLE tt_auth_provider_str
   FIELD main_provider         AS CHARACTER   FORMAT "x(5)"
   FIELD provider_type         AS CHARACTER   FORMAT "x(20)"
   FIELD authorised_service    AS CHARACTER   FORMAT "x(5)"
   FIELD group_doc_num         AS CHARACTER   FORMAT "x(6)"
   FIELD group_pr_type         AS CHARACTER   FORMAT "x(3)"
   FIELD group_sub_pr_type     AS CHARACTER   FORMAT "x(3)"
   FIELD group_name            AS CHARACTER   FORMAT "x(30)"
   FIELD doc_num               AS CHARACTER   FORMAT "x(6)"
   FIELD pr_type               AS CHARACTER   FORMAT "x(3)"
   FIELD sub_pr_type           AS CHARACTER   FORMAT "x(3)"
   FIELD doc_name              AS CHARACTER   FORMAT "x(30)"  
   FIELD account_reference     AS CHARACTER   FORMAT "x(20)"
   FIELD start_date            AS CHARACTER   FORMAT "x(10)"
   FIELD end_date              AS CHARACTER   FORMAT "x(10)".

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
         HEIGHT             = 12.14
         WIDTH              = 53.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-authRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE authRenderProcedure Procedure 
PROCEDURE authRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  { ma/app/maauthrenderprocedure.i }

END PROCEDURE. /* authRenderProcedure */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntFilterAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntFilterAuth Procedure 
PROCEDURE getCntFilterAuth :
/*------------------------------------------------------------------------------
  Purpose   : Filter authorisation container definition   
  Parameters: 
  Notes     :     
------------------------------------------------------------------------------*/
{ma/app/maauthgetcntfilterauth.i}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntSearchAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntSearchAuth Procedure 
PROCEDURE getCntSearchAuth :
/*------------------------------------------------------------------------------
  Purpose   : Search authorisation container definition   
  Parameters: 
  Notes     :     
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO. 
  
&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE VARIABLE oControl                 AS cls.mipwscontrol NO-UNDO.
                                           
  DEFINE VARIABLE cTrackingMessage         AS CHARACTER        NO-UNDO.
  
  ASSIGN 
    opoContainer                               = NEW cls.mipwscontainer(ipcContainerName, "98%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle                = "Authorisation Results":U
    opoContainer:ViewOnly                      = FALSE
    opoContainer:ShowContainerSettings         = FALSE
    opoContainer:Collapsable                   = TRUE
    opoContainer:ContainerMode                 = Warpspeed:SubmitValue
    opoContainer:RowRenderProcedureHandle      = mipEnv:Health:maUIService:RenderProcedureHandle
    opoContainer:RowRenderProcedure            = "RowRenderProcedure":U
    opoContainer:RowRenderArgument             = "AuthSearchContainer":U
    
    /* mmp234 removing the find for the coding because we will deal with coding records in the primarycode and primaryprocedure renderprocedures */  
    opoContainer:QueryString                   = "FOR EACH tt_auth NO-LOCK":U
                                               + "   WHERE tt_auth.mem_num = '&1'":U  
                                               + "  ,FIRST tt_auth_provider NO-LOCK":U
                                               + "   WHERE tt_auth_provider.auth_obj = tt_auth.auth_obj ":U
                                               + "     AND tt_auth_provider.main_provider = TRUE  OUTER-JOIN"
                                               + "  ,FIRST tt_auth_coding NO-LOCK ":U  
                                               + "   WHERE tt_auth_coding.auth_obj = tt_auth.auth_obj":U
                                               + "     AND tt_auth_coding.primary_code = TRUE OUTER-JOIN ":U
                                               + "  ,FIRST tt_auth_type NO-LOCK":U 
                                               + "   WHERE tt_auth_type.auth_type_obj = tt_auth.auth_type_obj OUTER-JOIN":U 
                                               + "   ,LAST mit_relationship NO-LOCK ":U  
                                               + "   WHERE mit_relationship.related_entity_mnemonic = 'hatau'":U    
                                               + "     AND mit_relationship.related_obj             = tt_auth.auth_obj":U   
                                               + "     AND mit_relationship.relation_key            = 'mm_RelMmtrs2Hatau' OUTER-JOIN ":U
                                               + "      BY tt_auth.start_date DESCENDING":U
    opoContainer:TitleContainer                = NEW cls.mipwscontainer(ipcContainerName + "Title":U)
    opoContainer:TitleContainer:Borders        = "FALSE":U
    opoContainer:TitleContainer:Collapsable    = FALSE
    opoContainer:TitleContainer:ViewOnly       = FALSE 
    opoContainer:RowsToRender                  = ?
    opoContainer:DefaultContainerType          = "Table":U
    
    oControl                                   = opoContainer:TitleContainer:addControl("cbTypeSelect":U + ipcContainerName, "wsCombo":U , "20":U, "":U , "character":U,  1, 1, "":U)
    oControl:AdditionalItems                   = "=":U
    oControl:SpanOverLabel                     = TRUE
    oControl:RenderProcedureHandle             = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure                   = "authRenderProcedure":U
    oControl:RenderArgument                    = "AuthGroupSelect":U                        
    oControl:ControlPrompt                     = "New Authorisation":U
    oControl:JavascriptOnChange                = "var oBtn=buNewAuth" + ipcContainerName + ";":U                                               
                                               + "if(oBtn)~{":U                                               
                                               + "  if(this.value==~"~")~{":U
                                               + "    oBtn.disable();":U
                                               + "  ~}else~{":U
                                               + "    oBtn.enable();":U
                                               + "  ~}"
                                               + "~}":U


    oControl                                   = opoContainer:TitleContainer:addControl("buNewAuth":U + ipcContainerName   , "wsButton":U, "":U  , "1":U, "character":U,  1, 2, "":U)
    oControl:SpanOverLabel                     = TRUE
    oControl:ControlSubType                    = "BUTTON":U
    oControl:ControlToolTip                    = "New Authorisation":U
    oControl:ButtonLabel                       = "New Authorisation":U
    oControl:SubmitValue                       = "NewAuthorisation":U
    oControl:ButtonName                        = "frmSubmit":U
    oControl:ControlClass                      = WarpSpeed:ButtonClass

     oControl:ControlToken                     = "ReadOnly":U

     oControl                          = opoContainer:addControl("fcResObj":U         + ipcContainerName, "wsHref":U    , "20":U, "mit_relationship.owning_obj":U          , "character":U,  1,  "":U)
     oControl:RenderProcedureHandle    = mipEnv:Health:maUiService:RenderProcedureHandle
     oControl:RenderProcedure          = "authRenderProcedure":U
     oControl:RenderArgument           = "AuthResource":U

     oControl                          = opoContainer:addControl("fcAuthNum":U        + ipcContainerName, "wsHref":U    , "20":U, "tt_auth.auth_obj":U                     , "character":U,  2,  "Auth Number":U)

     oControl                          = opoContainer:addControl("fcAuthDep":U        + ipcContainerName, "wsInput":U   , "20":U, "tt_auth.dependant_name":U               , "character":U,  3,  "Dependant":U)

     oControl                          = opoContainer:addControl("fdAuthDate":U       + ipcContainerName, "wsInput":U   , "20":U, "tt_auth.auth_date":U                    , "date":U     ,  4,  "Create Date":U)

     oControl                          = opoContainer:addControl("fiServProv":U       + ipcContainerName, "wsInput":U   , "20":U, "tt_auth_provider.doc_num":U             , "character":U,  5,  "Service<br>Provider":U)
     oControl:RenderProcedureHandle    = mipEnv:Health:maUiService:RenderProcedureHandle                                
     oControl:RenderProcedure          = "authRenderProcedure":U                                                            
     oControl:RenderArgument           = "ProviderName":U                                                                                                            

     oControl                          = opoContainer:addControl("fcAuthPrimCode":U   + ipcContainerName, "wsInput":U   , "20":U, "tt_auth.primary_diagnosis":U            , "character":U,  7,  "Primary<br>Diagnosis":U)
                                                                                                                                                                         
     oControl                          = opoContainer:addControl("fcAuthPrimProc":U   + ipcContainerName, "wsInput":U   , "20":U, "tt_auth.primary_cpt":U                  , "character":U,  8,  "Primary<br>Procedure":U)

     oControl                          = opoContainer:addControl("fcAuthType":U       + ipcContainerName, "wsInput":U   , "20":U, "tt_auth_type.auth_type":U               , "character":U,  9,  "Auth Type":U)
                                                                                                                                                                           
     oControl                          = opoContainer:addControl("fcAuthReference":U  + ipcContainerName, "wsHref":U    , "20":U, "tt_auth.auth_obj":U                     , "character":U,  10,  "Reference<br>Authorisation<br>Number":U)
     oControl:KeyField                 = "tt_auth.auth_obj":U                                                           
     oControl:DisplayFields            = "tt_auth.reference_auth_num":U                                                 
     /*
     oControl:RenderProcedureHandle    = mipEnv:Health:maUiService:RenderProcedureHandle                                
     oControl:RenderProcedure          = "RenderProcedure":U                                                            
     oControl:RenderArgument           = "AuthLink":U                                                                   
     */                                                                                                                                                               
     oControl                          = opoContainer:addControl("fdAuthAmt":U        + ipcContainerName, "wsInput":U   , "20":U, "tt_auth.amount_auth":U                  , "Decimal":U  , 11,  "Auth Amount":U)
     oControl:ControlToken             = IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN "ReadOnly" ELSE "Hidden"                                                
     oControl:ControlFormat            = "->>,>>9.99":U                                                                                                                
                                                                                                                                                                  
     oControl                          = opoContainer:addControl("fcAuthStatus":U     + ipcContainerName, "wsCombo":U   , "20":U, "tt_auth.auth_status":U                  , "character":U, 12,  "Status":U)
     oControl:RenderProcedureHandle    = mipEnv:Health:maUIService:RenderProcedureHandle
     oControl:RenderProcedure          = "authRenderProcedure":U
     oControl:RenderArgument           = "AuthStatusCombo":U
     oControl:AdditionalItems          = "All=7|":U  
     oControl:ControlToken             = IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN "ReadOnly" ELSE "Hidden"

     
     oControl                          = opoContainer:addControl("fcAuthSystemStatus":U   + ipcContainerName, "wsCombo":U   , "20":U, "tt_auth.auth_status":U              , "character":U, 13,  " System Status":U)
     oControl:RenderProcedureHandle    = mipEnv:Health:maUIService:RenderProcedureHandle
     oControl:RenderProcedure          = "authRenderProcedure":U
     oControl:RenderArgument           = "FullSystemStatusCombo":U
     oControl:AdditionalItems          = "All=7|":U  
     oControl:ControlToken             = IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN "ReadOnly" ELSE "Hidden"

     oControl                          = opoContainer:addControl("ftStartDate":U      + ipcContainerName, "wsInput":U   , "20":U, "tt_auth.start_date":U                   , "date":U     , 14,  "Auth<br>Start Date":U)
     oControl                          = opoContainer:addControl("ftEndDate":U        + ipcContainerName, "wsInput":U   , "20":U, "tt_auth.end_date":U                     , "date":U     , 15,  "Auth<br>End Date":U)
     oControl                          = opoContainer:addControl("fcAuthIncomplete":U + ipcContainerName, "wsCheckBox":U, "20":U, "tt_auth.auth_incomplete":U              , "logical":U  , 16,  "Incomplete":U)
     
     oControl                          = opoContainer:addControl("buAuthAdd":U        + ipcContainerName, "wsInput":U   , "":U  , "":U                                     , "":U         , 17,  "":U).
 
    
  { mip/inc/mipcatcherror.i }                                                                                                                                                 
  
&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdDate Procedure 
PROCEDURE getCntUpdDate :
/*------------------------------------------------------------------------------
  Purpose   : Date update container definition   
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  
  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "100%":U, "":U, WarpSpeed:BaseClass, FALSE)
                                       
    opoContainer:ContainerTitle        = "Date Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:ContainerHidden       = TRUE
    
    oControl                           = opoContainer:addControl("fdAdmitDate":U     + ipcContainerName, "wsInput":U   , "20":U, "":U, "DATE":U     , 1, 1, "Admit Date:":U)
    oControl:ColumnSpan                = 2
                                      
    oControl                           = opoContainer:addControl("fdProcedureDate":U + ipcContainerName, "wsInput":U   , "20":U, "":U, "DATE":U     , 1, 3, "Procedure Date:":U)
    oControl:ColumnSpan                = 2
                                                                                                                                                    
    oControl                           = opoContainer:addControl("fdEndDate":U       + ipcContainerName, "wsInput":U   , "20":U, "":U, "DATE":U     , 1, 5, "Discharge Date:":U)
    oControl:ColumnSpan                = 2
                                                                                                                       
    oControl                           = opoContainer:addControl("fcAdmitTime":U     + ipcContainerName, "wsInput":U   , "20":U, "":U, "CHARACTER":U, 2, 1, "Admit Time:":U)
    oControl:ColumnSpan                = 2
                                                                                                                       
    oControl                           = opoContainer:addControl("fcProcedureTime":U + ipcContainerName, "wsInput":U   , "20":U, "":U, "CHARACTER":U, 2, 3, "Procedure Time:":U)
    oControl:ColumnSpan                = 2
                                                                                                                       
    oControl                           = opoContainer:addControl("fcEndTime":U       + ipcContainerName, "wsInput":U   , "20":U, "":U, "CHARACTER":U, 2, 5, "Discharge Time:":U)
    oControl:ColumnSpan                = 2
    
    oControl                           = opoContainer:addControl("flEmergency":U     + ipcContainerName, "wsCheckBox":U, "20":U, "":U, "LOGICAL":U  , 3, 1, "Emergency?":U)
                                      
    oControl                           = opoContainer:addControl("flLateAuth":U      + ipcContainerName, "wsCheckBox":U, "20":U, "":U, "LOGICAL":U  , 3, 2, "Late Auth?":U)
                                      
    oControl                           = opoContainer:addControl("flPenalty":U       + ipcContainerName, "wsCheckBox":U, "20":U, "":U, "LOGICAL":U  , 3, 3, "Penalty?":U)
                                      
    oControl                           = opoContainer:addControl("flMVA":U           + ipcContainerName, "wsCheckBox":U, "20":U, "":U, "LOGICAL":U  , 3, 4, "MVA?":U)
                                      
    oControl                           = opoContainer:addControl("flWCAIOD":U        + ipcContainerName, "wsCheckBox":U, "20":U, "":U, "LOGICAL":U  , 3, 5, "WCA/IOD?":U)
                                      
    oControl                           = opoContainer:addControl("flCoPay":U         + ipcContainerName, "wsCheckBox":U, "20":U, "":U, "LOGICAL":U  , 3, 6, "Copay?":U)
    oControl:CellSnippet               = "height='25px'":U
    .
  
  { mip/inc/mipcatcherror.i }   
&ENDIF 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdNote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdNote Procedure 
PROCEDURE getCntUpdNote :
/*------------------------------------------------------------------------------
  Purpose   : Note update container definition   
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN  
  
  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
                                      
    opoContainer:ContainerTitle        = "Notes":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:NoDataMessage         = "Please specify notes in the empty line provided above":U
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    
    oControl                           = opoContainer:addControl("fiLineNumber":U   + ipcContainername, "wsInput":U   , "25":U, "":U, "INTEGER":U  , 1, "#":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                              
    oControl:RenderArgument            = "LineNumber":U                                                      
    oControl:ControlToken              = "Hidden":U

    oControl                           = opoContainer:addControl("cbNoteType":U     + ipcContainerName, "wsCombo":U   , "5":U , "":U, "CHARACTER":U, 2, "Note Type":U)
                                                                                                                                                     
    oControl                           = opoContainer:addControl("fcNoteContent":U  + ipcContainerName, "wsTextArea":U, "50":U, "":U, "CHARACTER":U, 3, "Note Content":U)
                                                                                                                                                     
    oControl                           = opoContainer:addControl("flMember":U       + ipcContainerName, "wsCheckBox":U, "":U  , "":U, "LOGICAL":U  , 4, "Member":U)
    oControl                           = opoContainer:addControl("flAdmitting":U    + ipcContainerName, "wsCheckBox":U, "":U  , "":U, "LOGICAL":U  , 5, "Admitting Dr./Hospital":U)
    oControl                           = opoContainer:addControl("flRequesting":U   + ipcContainerName, "wsCheckBox":U, "":U  , "":U, "LOGICAL":U  , 6, "Requesting Dr./Hospital":U)
    oControl                           = opoContainer:addControl("flPrint":U        + ipcContainerName, "wsCheckBox":U, "":U  , "":U, "LOGICAL":U  , 7, "Print?":U).
    
    
  ASSIGN oContainerProperties = NEW cls.wscontainerproperties(opoContainer).
  
  
  mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties).
  
  
  { mip/inc/mipcatcherror.i }  

&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareAuthStatusCombo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareAuthStatusCombo Procedure 
PROCEDURE prepareAuthStatusCombo :
/*------------------------------------------------------------------------------
  Purpose   : Builds auth status combo options    
  Parameters: Control object
              Status Type ( "System" or "" )
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER iopoControl    AS cls.mipwscontrol NO-UNDO.
  DEFINE INPUT        PARAMETER ipcControlType AS CHARACTER        NO-UNDO.
  
  DEFINE VARIABLE cStatusList AS CHARACTER   NO-UNDO.
  
   
  EMPTY TEMP-TABLE ttAuthStatus.
  
  mipEnv:Health:AuthService:getStatusTable(INPUT ipcControlType, OUTPUT TABLE ttAuthStatus).
  
  FOR EACH ttAuthStatus NO-LOCK BY ttAuthStatus.status_description:
  
    IF NOT CAN-DO("1,2,4":U, STRING(ttAuthStatus.status_code))                             OR 
      (NOT CAN-DO("1,2,4":U, iopoControl:ControlValue) AND ttAuthStatus.status_code = 1  ) OR 
          (CAN-DO("1,2,4":U, iopoControl:ControlValue) AND TRIM(iopoControl:ControlValue) = STRING(ttAuthStatus.status_code))
    THEN      
    ASSIGN cStatusList = cStatusList
                       + (IF cStatusList = "":U THEN  "":U ELSE "|":U)
                       + ttAuthStatus.status_description + "=":U + STRING(ttAuthStatus.status_code).
  END. /*FOR EACH ttAuthStatus NO-LOCK BY ttAuthStatus.status_description:*/
  
  ASSIGN iopoControl:AdditionalItems = "=|":U + cStatusList.
  
  { mip/inc/mipcatcherror.i
    &FINALLY = "EMPTY TEMP-TABLE ttAuthStatus." }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-renderAuthorisationMarquee) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderAuthorisationMarquee Procedure 
PROCEDURE renderAuthorisationMarquee :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcMemberNumber AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipcProviderList AS CHARACTER NO-UNDO.

  { ma/inc/maMemberMarquee.i            &MemberNumber = "ipcMemberNumber"}
  { ma/inc/maauthproviderflagsmarquee.i &ProviderList = "ipcProviderList"}
  
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-renderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderProcedure Procedure 
PROCEDURE renderProcedure :
/*------------------------------------------------------------------------------
  Purpose   : In an attempt to reduce the number of times we run through
              render procedures, auth specific renders should go through the new
              authRenderProcedure.
  Parameters: Control to be rendered
  Notes     :  
  Author    : Andrewd     
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol  NO-UNDO.

  RUN authRenderProcedure IN TARGET-PROCEDURE( INPUT ipoControl ).
  /* This will ensure backward compatability, allowing the render procedures
     called from other parts of the system to function as they have. */
  RUN SUPER( INPUT ipoControl ).
    
  { mip/inc/mipcatcherror.i }
END PROCEDURE. /* renderProcedure */

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
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer  NO-UNDO.
  
  DEFINE VARIABLE oAuthorisation      AS cls.maauthorisation NO-UNDO.
  DEFINE VARIABLE oQuery              AS cls.mipquery        NO-UNDO.
  DEFINE VARIABLE oRFControl          AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oANControl          AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oAddControl         AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oAuthType           AS cls.maauthtype      NO-UNDO.
  DEFINE VARIABLE dAuthObj            AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dInsurerObj         AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj        AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj    AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dQuantityPaid       AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAmountPaid         AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dStartDate          AS DATE                NO-UNDO.
  DEFINE VARIABLE iAuthStatus         AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iDependant          AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iOption             AS INTEGER             NO-UNDO.
  DEFINE VARIABLE cOnClick            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMemNum             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cProviderType       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cDiscipline         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cSubDiscipline      AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatus             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatusNote         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValidMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lAuthNumViewable    AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lAuthUpdatesAllowed AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lMandatory          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  
  RUN SUPER(INPUT ipoContainer) NO-ERROR.

  /* 
    The super may not have a rowRenderProcedure 
  */
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6439' &ResetIgnoredErrors = TRUE }         


&IF {&DBDFMA} >= 010195 &THEN  
  
  CASE ipoContainer:RowRenderArgument:
  
    WHEN "AuthSearchContainer":U OR WHEN "DuplicateContainer":U THEN
    DO:
      ASSIGN oQuery           = ipoContainer:ContainerQuery
      
             oANControl       = ipoContainer:getControl("fcAuthNum":U       + ipoContainer:ContainerCode)
             oRFControl       = ipoContainer:getControl("fcAuthReference":U + ipoContainer:ContainerCode)
             oAddControl      = ipoContainer:getControl("buAuthAdd":U       + ipoContainer:ContainerCode)

             dAuthObj         = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_obj":U ,      "BUFFER-VALUE":U))
             dAuthTypeObj     = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_type_obj":U , "BUFFER-VALUE":U))
             iDependant       = INTEGER(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.dependant":U,      "BUFFER-VALUE":U))
             dInsurerObj      = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.insurer_obj":U,    "BUFFER-VALUE":U))
             iOption          = INTEGER(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.option_code":U,    "BUFFER-VALUE":U))
             dStartDate       =    DATE(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_date":U,      "BUFFER-VALUE":U))
             iAuthStatus      = INTEGER(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_status":U,    "BUFFER-VALUE":U)).                                            
             

    IF NOT VALID-OBJECT(oAuthorisation)
    OR oAuthorisation:AuthObj <> dAuthObj
    THEN
      lSuccess = mipEnv:Health:AuthService:getAuthObject
                   ( INPUT  dAuthObj,
                     INPUT  "":U,
                     OUTPUT oAuthorisation ).

      IF oAuthorisation:InFocus THEN
      DO:

        ASSIGN 
          oAuthType                = oAuthorisation:getAuthType()

          lAuthNumViewable         = mipEnv:Health:AuthBusinessLogic:AuthNumViewable(INPUT iAuthStatus,INPUT dInsurerObj,INPUT oAuthorisation:MemberOptionCode, INPUT dStartDate)
          lSuccess                 = mipEnv:Health:AuthBusinessLogic:AuthUpdatesAllowed(INPUT dAuthObj,   
                                                                                        INPUT dAuthTypeObj,
                                                                                        INPUT dInsurerObj,
                                                                                        INPUT oAuthorisation:MemberOptionCode,
                                                                                        INPUT dStartDate, 
                                                                                        OUTPUT lAuthUpdatesAllowed,
                                                                                        OUTPUT cValidMessage) .

        ASSIGN
          oANControl:Wob           = "mahatau"
          oANControl:Obj           = oANControl:ControlValue
          oANControl:ControlTarget = "_self":U
          oANControl:AssignList    = "&fiDependant=":U + STRING(iDependant) + "&fcAuthTypeGroup=":U + oAuthType:AuthTypeGroup 
                                   + IF lAuthUpdatesAllowed = FALSE OR get-value("wobmode":U) = "Enquiry":U THEN ("&wobMode=Enquiry&enquiryMessage=":U + cValidMessage) ELSE "":U
          oRFControl:Wob           = oANControl:Wob
          oRFControl:Obj           = oANControl:Obj
          oRFControl:ControlTarget = oANControl:ControlTarget
          oRFControl:AssignList    = oANControl:AssignList.

        IF lAuthNumViewable
        THEN ASSIGN oANControl:LinkText = oAuthorisation:AuthNum.
        ELSE ASSIGN oANControl:LinkText = "********":U.  

        ASSIGN
          oAddControl:CellClass      = IF (lAuthUpdatesAllowed = FALSE AND cValidMessage <> "":U) OR get-value("wobmode":U) = "Enquiry":U
                                       THEN "+clHid":U ELSE "-clHid":U.

        mipEnv:Health:AuthService:getAuthTypeConfig(INPUT dAuthTypeObj,                       
                                                    INPUT dInsurerObj,                        
                                                    INPUT iOption,                        
                                                    INPUT dStartDate,                         
                                                    INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).     

        FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.EpisodeUpdAllowed THEN 
        DO:        
          ASSIGN cOnClick = {ws/inc/wshref.i &wob      = "oAuthType:AuthTypeGroupValue"
                                             &obj      = "'0'"
                                             &linkText = "''"
                                             &linkargs = "'&frmSubmit=NewAuthorisation&flNewAuth=true&AuthEpisodeObj=' + STRING(oAuthorisation:AuthEpisodeObj) + '&fcAuthTypeGroup=' + oAuthType:AuthTypeGroup"
                                             &hreftype = 'javascript' }.

          IF oAddControl:ResolvedToken = "Updatable":U
          THEN
            ASSIGN oAddControl:ControlToolTip = "Add authorisation to episode ":U + oAuthorisation:AuthEpisodeNum

                   oAddControl:CellLayoutMask = { ma/inc/maiconbutton.i &ButtonSource  = "'img/add.png'" 
                                                                        &ButtonOnClick = cOnClick
                                                                        &ButtonControl = oAddControl }.
          
        END. /*IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.EpisodeUpdAllowed THEN */
      END. /*IF oAuthorisation:InFocus THEN*/      
    END. /*WHEN "AuthSearchContainer":U THEN*/   
  END CASE.

&ENDIF
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "EMPTY TEMP-TABLE ttAuthTypeConfig.

                DATASET dsAuthorisation:EMPTY-DATASET().

                IF VALID-OBJECT(oAuthType)      THEN DELETE OBJECT oAuthType."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



