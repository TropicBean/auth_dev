&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
" This code is based on the cgi-wrapper template as designed by Progress.

  MIP Holdings (Pty) Ltd.

  Use this template to create a new Custom CGI Wrapper Procedure and write WebSpeed code that dynamically generates HTML. No associated static HTML file is needed."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Filename    : ma/app/maauthcopaytypewobsuper.p
  Purpose     : Maintain Authorisation Copay Type
  Description : Maintain Authorisation Copay Type
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

/* This helps to ensure proper clean-up */
CREATE WIDGET-POOL.

/* WarpSpeed's Shared Definitions */
{ mip/inc/mipdefshared.i }

{ sysadmma.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation}

{ ma/inc/maauthcopaytypeds.i }
                         
{ ma/inc/maaudittt.i    }   

{ ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_error"}

DEFINE TEMP-TABLE tt_deleted
  FIELD owning_obj AS DECIMAL
  FIELD owning_key AS CHARACTER.

/* Variables commonly used by WarpSpeed */
DEFINE VARIABLE goWob              AS cls.mipwswob            NO-UNDO.

/* Variables for this specific WOB */
DEFINE VARIABLE gcFormat           AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcSearchMethod     AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcSelected         AS CHARACTER               NO-UNDO.
DEFINE VARIABLE glEnquiryWob       AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE glSuccess          AS LOGICAL                 NO-UNDO.

/* Containers */
DEFINE VARIABLE goCntSearchFilter  AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntSearchResults AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntMaint         AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntAudit         AS cls.mipwscontainer      NO-UNDO.

/* That's all Folks! */

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
         HEIGHT             = 25
         WIDTH              = 83.4.
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
  /*
  { ws/inc/wsstructure.i }
  */
  { ma/inc/mastructure.i }
  
  { mip/inc/mipcatcherror.i }    

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveSearchResults Procedure 
PROCEDURE ajaxSaveSearchResults :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE          FOR ttValidation.
  
  DEFINE VARIABLE oAuthCopayType       AS cls.maauthcopaytype        NO-UNDO.
  DEFINE VARIABLE oRequestHelper       AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper      AS cls.maajaxresponsehelper   NO-UNDO.
  
  DEFINE VARIABLE hErrorHandle            AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cContainerCode          AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAction                 AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction           AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthCopayType          AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthCopayDescription   AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE iLineNumber             AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE dAuthCopayTypeObj       AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE lSuccess                AS LOGICAL                    NO-UNDO.
  
    
  ASSIGN
    cContainerCode         = ipcValidationArgument
                        
    oRequestHelper         = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
    oResponseHelper        = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
                        
    oAuthCopayType         = NEW cls.maauthcopaytype()
                                  
    cRecordAction          =         oRequestHelper:getFieldValue("fcAction":U                 + cContainerCode)
    cAuthCopayType         =         oRequestHelper:getFieldValue("fcAuthCopayType":U          + cContainerCode)
    cAuthCopayDescription  =         oRequestHelper:getFieldValue("fcAuthCopayDescription":U   + cContainerCode)
    iLineNumber            = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U             + cContainerCode))
    dAuthCopayTypeObj      = DECIMAL(oRequestHelper:getFieldValue("fdAuthCopayTypeObj":U       + cContainerCode))
    NO-ERROR.
  
  IF NOT {&ErrorStatus} THEN
  DO:
  
    CASE cRecordAction:
      WHEN "modify":U THEN
      DO:
        oAuthCopayType:focusAuthCopayType(dAuthCopayTypeObj) NO-ERROR.
        
        ASSIGN           
           oAuthCopayType:LineNumber     = iLineNumber
           oAuthCopayType:AuthCopayType  = cAuthCopayType
           oAuthCopayType:Description    = cAuthCopayDescription
           
           lSuccess                      = oAuthCopayType:SaveAuthCopayType()            
         NO-ERROR.               
       
        IF NOT {&ErrorStatus} AND NOT oAuthCopayType:ErrorObject:ErrorsExist 
        THEN
          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U 
            lSuccess                        = oResponseHelper:addFieldValue("fdAuthCopayTypeObj":U + cContainerCode, STRING(oAuthCopayType:AuthCopayTypeObj))
           NO-ERROR.            
            
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN
      DO:               
        ASSIGN lSuccess = oAuthCopayType:focusAuthCopayType (dAuthCopayTypeObj) NO-ERROR.
         
        IF NOT {&ErrorStatus} AND NOT oAuthCopayType:AuthCopayTypeInFocus 
        THEN  
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U 
            NO-ERROR.
        ELSE 
          ASSIGN lSuccess = oAuthCopayType:removeAuthCopayType() NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthCopayType:ErrorObject:ErrorsExist
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
  
  IF {&ErrorStatus} OR oAuthCopayType:ErrorObject:ErrorsExist THEN
  DO:
    ASSIGN 
      oResponseHelper:RequestValid    = FALSE
      
      hErrorHandle                    = oAuthCopayType:ErrorObject:getErrorTableHandle()
      
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
      lSuccess                        = oResponseHelper:setError(hErrorHandle)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
  
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
  END.   /*IF oUserFlag:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
  
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthCopayType)  THEN DELETE OBJECT oAuthCopayType  NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }

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
  
  DEFINE VARIABLE lSuccess               AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cRecordAction          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthCopayType         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthCopayDescription  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE iRow                   AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iLineNumber            AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE dAuthCopayTypeObj      AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE oSearch                AS cls.maauthcopaytypesearch NO-UNDO.
   
  DATASET dsAuthCopayType:EMPTY-DATASET().
  
  oSearch = NEW cls.maauthcopaytypesearch(DATASET dsAuthCopayType BY-REFERENCE).
  
  IF goWob:Mode = "Search":U THEN
  DO:
    IF CAN-DO("SearchSubmit":U, goWob:SubmitValue) THEN
    DO:        
      DO-BLK:
      DO iRow = 1 TO INTEGER(get-value(goCntSearchResults:ContainerCode + "_rowsrendered":U)):
      
        ASSIGN
          cRecordAction          = get-value("fcAction":U                    + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthCopayType         = get-value("fcAuthCopayType":U             + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthCopayDescription  = get-value("fcAuthCopayDescription":U      + goCntSearchResults:ContainerCode + STRING(iRow))
          iLineNumber            = INTEGER(get-value("fiLineNumber":U        + goCntSearchResults:ContainerCode + STRING(iRow)))
          dAuthCopayTypeObj      = DECIMAL(get-value("fdAuthCopayTypeObj":U  + goCntSearchResults:ContainerCode + STRING(iRow)))
           
          dAuthCopayTypeObj      = (IF dAuthCopayTypeObj <= 0 
                                 THEN iRow * -1 ELSE dAuthCopayTypeObj).
          
        /* We dont want to update records that did not change but create a temp table record for all rows
           so that if there are any errors, the screen will be built for the user as it was when they submitted
           from this temp table*/
        IF cAuthCopayType <> "":U THEN
        DO:
          CREATE tt_auth_copay_type.
        
          ASSIGN
             tt_auth_copay_type.record_action       = cRecordAction
             tt_auth_copay_type.line_number         = iRow
             tt_auth_copay_type.auth_copay_type_obj = dAuthCopayTypeObj
             tt_auth_copay_type.auth_copay_type     = cAuthCopayType
             tt_auth_copay_type.description         = cAuthCopayDescription.
          
          VALIDATE tt_auth_copay_type.
          
          IF cRecordAction = "Delete":U THEN
          DO:
            CREATE tt_deleted.
            ASSIGN tt_deleted.owning_obj = dAuthCopayTypeObj
                   tt_deleted.owning_key = "":U.
                   
            VALIDATE tt_deleted.       
          END. /*IF cRecordAction = "Delete":U THEN*/
        END. /*IF cAuthCopayType <> "":U*/      
      END. /* DO iRow = 1 */                                                                     
    END. /*IF CAN-DO("SearchSubmit":U, goWob:SubmitValue) THEN*/
  END. /*IF goWob:Mode = "Search":U THEN*/
  
  IF goWob:Mode = "Maint":U THEN
  DO:
    IF CAN-DO(  "Submit->Add,":U
              + "Submit->Change,":U
              + "Confirm->Delete":U,
              TRIM(ipcWhatToDo) ) THEN
    DO TRANSACTION:
    
      ASSIGN     
        cAuthCopayType         = get-value("fcAuthCopayType":U         + goCntMaint:ContainerCode)
        cAuthCopayDescription  = get-value("fcAuthCopayDescription":U  + goCntMaint:ContainerCode).
      
      
      IF goWob:CurrentObj <> "":U AND ipcWhatToDo <> "Submit->Add":U THEN
      DO:
        oSearch:SetFilterCriteria("tt_auth_copay_type.auth_copay_type_obj":U, "=":U, DECIMAL(goWob:CurrentObj)).
        
        oSearch:fetchData().
      END. /*IF goWob:CurrentObj <> "":U THEN*/
      
      
      FIND FIRST tt_auth_copay_type NO-LOCK NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
      
      
      IF ipcWhatToDo = "Submit->Add":U OR ipcWhatToDo = "Submit->Change":U AND cAuthCopayType <> "":U THEN
      DO:
        IF NOT AVAILABLE tt_auth_copay_type THEN
        DO:
          CREATE tt_auth_copay_type.
          
          ASSIGN tt_auth_copay_type.auth_copay_type_obj = (IF ipcWhatToDo = "Submit->Add":U 
                                                       THEN 0 ELSE DECIMAL(goWob:CurrentObj)).
        END. /*IF NOT AVAILABLE tt_auth_copay_type THEN*/
          
      
        IF AVAILABLE tt_auth_copay_type
        THEN
          ASSIGN
             tt_auth_copay_type.record_action   = "MODIFY":U
             tt_auth_copay_type.line_number     = 1
             tt_auth_copay_type.auth_copay_type = cAuthCopayType
             tt_auth_copay_type.description     = cAuthCopayDescription.
        
        VALIDATE tt_auth_copay_type.
      END. /*IF cAuthCopayType <> "":U AND cRecordAction = "MODIFY":U THEN*/                                                                                 
      
      IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_copay_type THEN
      DO:
        ASSIGN tt_auth_copay_type.record_action = "DELETE":U.
        
        VALIDATE tt_auth_copay_type.
      END. /*IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_copay_type THEN*/
    END. /*IF CAN-DO("Submit":U, goWob:SubmitValue) THEN*/
  END. /*IF goWob:Mode = "Submit":U THEN*/
  
  
  IF CAN-DO("SearchSubmit,Submit,Confirm":U, goWob:SubmitValue) THEN
  DO:
  
&IF {&DBDFMA} >= 010195 &THEN  
    mipEnv:Health:AuthMaintenance:saveAuthCopayType(INPUT-OUTPUT DATASET dsAuthCopayType BY-REFERENCE).
&ENDIF    

    ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_auth_copay_type_error).                   
    
    IF goWob:SubmitValue = "SearchSubmit":U THEN
    DO:
      FOR EACH tt_auth_copay_type_error NO-LOCK:
      
        FIND FIRST tt_deleted NO-LOCK
             WHERE tt_deleted.owning_obj = tt_auth_copay_type_error.owning_obj
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
        
        IF AVAILABLE tt_deleted 
        THEN DELETE tt_deleted. 
      
      END. /*FOR EACH tt_auth_copay_type_error NO-LOCK:*/
    END. /*IF goWob:SubmitValue = "SearchSubmit":U THEN*/
    
    
    IF goWob:Mode = "Maint":U THEN
    DO:
      IF Warpspeed:ValidationError
      THEN 
        mipEnv:Health:maUiService:setContainerErrors(TEMP-TABLE tt_auth_copay_type_error:HANDLE, goCntMaint, "hamct":U, DECIMAL(Warpspeed:CurrentObj), "":U).
        
      IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN
      DO:
        FIND FIRST tt_auth_copay_type NO-LOCK NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
        
        ASSIGN 
           goWob:CurrentObj     = STRING(tt_auth_copay_type.auth_copay_type_obj)
           Warpspeed:CurrentObj = goWob:CurrentObj
           lSuccess             = wsUIService:setObj(goWob:ObjectCode, goWob:CurrentObj).
      
      END. /*IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN*/      
    END. /*IF goWob:Mode = "Maint":U THEN*/            
  END. /*IF CAN-DO("SearchSubmit,Search":U goWob:SubmitValue) THEN*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}
  
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

  DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.

  
  CASE ipoControl:RenderArgument:
  
    

  END CASE.

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainer Procedure 
PROCEDURE defineContainer :
/*------------------------------------------------------------------------------
  Purpose   : Define Auth Copay Type specific containers
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  
  IF goWob:Mode = "Search":U THEN 
  DO:
    RUN defineContainerSearch IN TARGET-PROCEDURE.    
    
  END. /*IF goWob:Mode = "Search":U THEN */
  
  
  IF goWob:Mode = "Maint":U THEN 
  DO:
    RUN defineContainerMaint IN TARGET-PROCEDURE.
                
  END. /*IF goWob:Mode = "Maint":U THEN */
      
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerMaint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerMaint Procedure 
PROCEDURE defineContainerMaint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
  /*Maintenance*/                                                                    
  ASSIGN       
    goCntMaint                   = goWob:getContainer("Maint":U)
    goCntMaint:ContainerMode     = goWob:SubmitValue
    goCntMaint:ViewOnly          = TRUE
    goCntMaint:QueryString       = "FOR EACH tt_auth_copay_type NO-LOCK":U
                                 + "   WHERE tt_auth_copay_type.auth_copay_type_obj = &1":U
                                
    oControl                     = goCntMaint:addControl("fcAuthCopayType":U     + goCntMaint:ContainerCode , "wsInput":U    , "20":U  , "tt_auth_copay_type.auth_copay_type":U , "character":U, 1, 1, "Co-payment Type:":U)
    
    oControl                     = goCntMaint:addControl("fcAuthCopayDescription":U + goCntMaint:ContainerCode , "wsTextArea":U , "60,2":U, "tt_auth_copay_type.description":U , "character":U, 3, 1, "Description:":U).
  
  
  ASSIGN goCntAudit              = mipEnv:Health:maUtility:getAuditContainer()
         goCntAudit:RowsToRender = ?.
  
  
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = FALSE &Container = goCntAudit }
  
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerSearch Procedure 
PROCEDURE defineContainerSearch :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
  /* SearchFilter */
  ASSIGN
    goCntSearchFilter                         = goWob:getContainer("SearchFilter":U) 
                                        
    oControl                                  = goCntSearchFilter:addControl("fcAuthCopayType":U          + goCntSearchFilter:ContainerCode  , "wsInput":U        , "20":U, "":U, "character":U, 1, 1, "Co-payment Type:":U)
    oControl                                  = goCntSearchFilter:addControl("fcAuthCopayDescription":U   + goCntSearchFilter:ContainerCode  , "wsInput":U        , "60":U, "":U, "character":U, 2, 1, "Description:":U)
    .      
  
  /*Search Results*/
  ASSIGN              
    goCntSearchResults                        = NEW cls.mipwscontainer("SearchResults":U + goWob:ObjectCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntSearchResults:ContainerMode          = goWob:SubmitValue
    goCntSearchResults:ShowGenericReportPrint = TRUE
    goCntSearchResults:Collapsed              = FALSE
    goCntSearchResults:ViewOnly               = NOT glEnquiryWob
    goCntSearchResults:RowsToRender           = ?
    goCntSearchResults:RowRenderProcedure     = "rowRenderProcedure":U
    goCntSearchResults:RowRenderArgument      = goCntSearchResults:ContainerCode
    goCntSearchResults:DefaultContainerType   = "TABLE":U                                                                                                                                                           
    goCntSearchResults:ContainerTitle         = "Results":U                                                                                                                                                         
    goCntSearchResults:QueryString            = "FOR EACH tt_auth_copay_type NO-LOCK":U
                                              + "   BY tt_auth_copay_type.line_number":U
                                                                                                                                                                                                                    
    lSuccess                                  = goWob:setContainer("SearchResults":U,  goCntSearchResults)                                                                                                          
                                                                                                                                                                                                                    
    oControl                                  = goCntSearchResults:addControl("fdAuthCopayTypeObj":U       + goCntSearchResults:ContainerCode  , "wsInput":U        , "20":U   , "tt_auth_copay_type.auth_copay_type_obj":U , "character":U, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                                                                                                                                            
                                                                                                                                                                                                                      
    oControl                                  = goCntSearchResults:addControl("fcAuthCopayType":U          + goCntSearchResults:ContainerCode  , "wsInput":U        , "10":U   , "tt_auth_copay_type.auth_copay_type":U     , "character":U, 3, "Co-payment Type":U)
    oControl:ControlClass                     = "+clMan":U                                                                                                                                                            
    oControl:ErrorMessage                     = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Copay Type'"}, "TEXT":U)                                                                                  
    oControl:JavascriptOnBlur                 = "fnValidateField(this, true, ":U + QUOTER(oControl:ErrorMessage) + ", (this.value.length == 0));":U                                                                   
    
    oControl                                  = goCntSearchResults:addControl("fcAuthCopayDescription":U   + goCntSearchResults:ContainerCode  , "wsTextArea":U     , "8,2":U  , "tt_auth_copay_type.description":U     , "character":U, 4, "Description":U) 
    oControl:ErrorMessage                     = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Description'"}, "TEXT":U)
    oControl:ControlClass                     = "+clMan":U
    oControl:JavascriptOnBlur                 = "fnValidateField(this, true, ~"A valid Copay Type description must be entered~", (this.value.length==0));":U
    .      
    
      
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE &Container = goCntSearchResults }

  
  { mip/inc/mipcatcherror.i } 
   
    
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
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.
  

  { mip/inc/mipcatcherror.i }
  
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
  
  DEFINE VARIABLE cRecords              AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cMode                 AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cContainerCode        AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE iRecords              AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iRowsRendered         AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iDeleted              AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE lEnquiryMode          AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE cAuthCopayType        AS CHARACTER                  NO-UNDO. 
  DEFINE VARIABLE cAuthCopayDescription AS CHARACTER                  NO-UNDO. 
  DEFINE VARIABLE oControl              AS cls.mipwscontrol           NO-UNDO.
  DEFINE VARIABLE oSearch               AS cls.maauthcopaytypesearch NO-UNDO.
  
  ASSIGN oSearch      = NEW cls.maauthcopaytypesearch(INPUT DATASET dsAuthCopayType BY-REFERENCE)
  
         lEnquiryMode = get-value("WobMode":U) = "enquiry":U.
  
  CASE goWob:Mode:
  
    WHEN "Search":U THEN
    DO: 
      ASSIGN 
         cRecords               = goCntSearchFilter:getControl("fiRecords":U              + goCntSearchFilter:ContainerCode):ControlValue
         cMode                  = goCntSearchFilter:getControl("fcSearchMode":U           + goCntSearchFilter:ContainerCode):ControlValue
         cAuthCopayType         = goCntSearchFilter:getControl("fcAuthCopayType":U        + goCntSearchFilter:ContainerCode):ControlValue
         cAuthCopayDescription  = goCntSearchFilter:getControl("fcAuthCopayDescription":U + goCntSearchFilter:ContainerCode):ControlValue.
         
      IF NOT Warpspeed:ValidationError THEN
      DO:
        DATASET dsAuthCopayType:EMPTY-DATASET.
        
        ASSIGN     
          lSuccess  = (IF cAuthCopayType <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_copay_type.auth_copay_type":U, cMode, cAuthCopayType) ELSE TRUE)
                       
          lSuccess  = (IF cAuthCopayDescription <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_copay_type.description":U, cMode, cAuthCopayDescription) ELSE TRUE)             
                       
          lSuccess  = oSearch:setCriteria("BatchSize":U,  cRecords)
          lSuccess  = oSearch:setCriteria("Query-Sort":U, "tt_auth_copay_type.auth_copay_type":U)
          
          lSuccess  = oSearch:fetchData().     
        
      END. /*IF NOT Warpspeed:ValidationError THEN*/
      
      IF NOT lEnquiryMode
      THEN
        ASSIGN lSuccess = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_copay_type:DEFAULT-BUFFER-HANDLE). 
      
      
      /* Get the number of records received from the fetch data */
      FOR EACH tt_auth_copay_type NO-LOCK: 
        ASSIGN iRowsRendered = iRowsRendered + 1. 
      END. /*FOR EACH tt_auth_copay_type NO-LOCK: */
      
      FOR EACH tt_deleted NO-LOCK:
        ASSIGN iDeleted = iDeleted + 1.
      END. /*FOR EACH tt_deleted NO-LOCK:*/
      
      
      /* Set the container title with the number of records */
      ASSIGN
        goCntSearchResults:ViewOnly        = FALSE
        
        cContainerCode                     = goCntSearchResults:ContainerCode                                
  
        oControl                           = goCntSearchResults:getControl("fcRecordAction":U + cContainerCode)
        oControl:ControlQueryField         = "tt_auth_copay_type.record_action":U
        
        oControl                           = goCntSearchResults:getControl("buEdit":U + cContainerCode)
        oControl:ControlToken              = "Updatable":U
        
        goCntSearchResults:QueryBufferList = STRING(TEMP-TABLE tt_auth_copay_type:DEFAULT-BUFFER-HANDLE)
                
        lSuccess                           = goCntSearchResults:PopulateFromQuery()
        
        goCntSearchResults:ContainerTitle  = goCntSearchResults:ContainerTitle + ": ":U 
                                           + (IF Warpspeed:ValidationError 
                                              THEN "There has been an error submitting your page":U
                                              ELSE
                                               ( IF iRowsRendered > INTEGER(cRecords)
                                                 THEN "Please refine your search criteria as it resulted in more than ":U + cRecords + " records...":U
                                                 ELSE STRING(iRowsRendered - (IF glEnquiryWob THEN 0 ELSE 1)) + " record/s found":U 
                                                        + (IF Warpspeed:ValidationError AND iDeleted > 0 THEN " ( " + STRING(iDeleted) + " record/s deleted )":U ELSE "":U))). 
    END. /*WHEN "Search":U THEN*/
    
    WHEN "Maint":U THEN
    DO:      
      IF NOT Warpspeed:ValidationError THEN
      DO:
        DATASET dsAuthCopayType:EMPTY-DATASET.
        
        IF goWob:CurrentObj <> "":U
        THEN
          ASSIGN 
            lSuccess = oSearch:SetFilterCriteria("tt_auth_copay_type.auth_copay_type_obj":U, "=":U, DECIMAL(goWob:CurrentObj))                              
                      
            lSuccess = oSearch:fetchData().              
      END. /*IF NOT Warpspeed:ValidationError THEN*/
      
      
      mipEnv:Health:maUtility:getAuditRecordTT(
         INPUT "ham_auth_copay_type":U,             
         INPUT goWob:CurrentObj,                                                                              
         INPUT "":U,                                                                      
         INPUT "":U,                                                                                              
         OUTPUT TABLE ttAuditRecord).
     
      
      ASSIGN      
        goCntMaint:ViewOnly         = FALSE WHEN LOOKUP(Warpspeed:SubmitValue, "ADD,CHANGE") > 0
        goCntMaint:QueryString      = SUBSTITUTE(goCntMaint:QueryString, goWob:CurrentObj)
                                    
        goCntMaint:QueryBufferList  = STRING(TEMP-TABLE tt_auth_copay_type:DEFAULT-BUFFER-HANDLE)
                                    
        lSuccess                    = goCntMaint:PopulateFromQuery()
                                    
                                    
        goCntAudit:QueryBufferList  = STRING(TEMP-TABLE ttAuditRecord:DEFAULT-BUFFER-HANDLE)
                                    
        lSuccess                    = goCntAudit:PopulateFromQuery().                
        
    END. /*WHEN "Maint":U THEN*/
   
  END CASE. /*CASE goWob:Mode:*/

  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}

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
  
  DEFINE VARIABLE dAuthCopayTypeObj AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lSuccess          AS LOGICAL NO-UNDO.
   
 
  IF WarpSpeed:ValidationError THEN
  DO:
    ASSIGN dAuthCopayTypeObj = DECIMAL(ipoContainer:getControl("fdAuthCopayTypeObj":U       + goCntSearchResults:ContainerCode):ControlValue).
        
    mipEnv:Health:maUiService:setContainerErrors(TEMP-TABLE tt_auth_copay_type_error:HANDLE, ipoContainer, "hamct":U, dAuthCopayTypeObj, "":U ).
      
  END. /* IF WarpSpeed:ValidationError  */
 
  RUN SUPER (INPUT ipoContainer).
  
  
  CASE ipoContainer:RowRenderArgument:

  END CASE.
  
  { mip/inc/mipcatcherror.i }

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
  
  { mip/inc/mipcatcherror.i } 
  
END PROCEDURE.  /* shutdown */

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
