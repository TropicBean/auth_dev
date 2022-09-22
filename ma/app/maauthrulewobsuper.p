&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
" This code is based on the cgi-wrapper template as designed by Progress.

  MIP Holdings (Pty) Ltd.

  Use this template to create a new Custom CGI Wrapper Procedure and write WebSpeed code that dynamically generates HTML. No associated static HTML file is needed."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Filename    : ma/app/maauthrulewobsuper.p
  Purpose     : Maintain Authorisation Rules
  Description : Maintain Authorisation Rules
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

/* This helps to ensure proper clean-up */
CREATE WIDGET-POOL.

/* WarpSpeed's Shared Definitions */
{ mip/inc/mipdefshared.i }

{ sysadmma.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation}

{ ma/inc/maauthruleds.i }
                         
{ ma/inc/maaudittt.i    }   

{ ma/inc/mainsurertt.i  }                      
                         
{ ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_error"}

DEFINE TEMP-TABLE tt_deleted
  FIELD owning_obj AS DECIMAL
  FIELD owning_key AS CHARACTER.
  
DEFINE TEMP-TABLE tt_record
  FIELD record_key AS CHARACTER.  

DEFINE TEMP-TABLE tt_control
  FIELD control_name  AS CHARACTER
  FIELD control_token AS CHARACTER.

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
         HEIGHT             = 19.62
         WIDTH              = 53.8.
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

&IF DEFINED(EXCLUDE-ajaxSaveSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveSearchResults Procedure 
PROCEDURE ajaxSaveSearchResults :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
  
  DEFINE VARIABLE oAuthRule            AS cls.maauthrule             NO-UNDO.
  DEFINE VARIABLE oRequestHelper       AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper      AS cls.maajaxresponsehelper   NO-UNDO.
  
  DEFINE VARIABLE hErrorHandle         AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cContainerCode       AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAction              AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction        AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthRuleCode        AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthRuleDescription AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthRuleLinkCode    AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthRuleType        AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthRuleValue       AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthRuleValidValues AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE iOptionCode          AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iLineNumber          AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE dAuthRuleObj         AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAuthRuleLinkObj     AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dInsurerObj          AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dEffectiveDate       AS DATE                       NO-UNDO.
  DEFINE VARIABLE dEndDate             AS DATE                       NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE lSystemOwned         AS LOGICAL                    NO-UNDO.
      
  ASSIGN
    cContainerCode         = ipcValidationArgument
                          
    oRequestHelper         = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
    oResponseHelper        = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
                          
    oAuthRule              = NEW cls.maauthrule()
                                     
    cRecordAction          =         oRequestHelper:getFieldValue("fcAction":U        + cContainerCode)
    cAuthRuleCode          =         oRequestHelper:getFieldValue("fcRuleCode":U      + cContainerCode)
    cAuthRuleDescription   =         oRequestHelper:getFieldValue("fcDescription":U   + cContainerCode)
    cAuthRuleType          =         oRequestHelper:getFieldValue("cbRuleType":U      + cContainerCode)
    cAuthRuleValidValues   =         oRequestHelper:getFieldValue("fcValidValues":U   + cContainerCode)    
    cAuthRuleValue         =         oRequestHelper:getFieldValue("fcRuleValue":U     + cContainerCode)    
    iLineNumber            = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U    + cContainerCode))
    iOptionCode            = INTEGER(oRequestHelper:getFieldValue("fiOptionCode":U    + cContainerCode))
    dAuthRuleObj           = DECIMAL(oRequestHelper:getFieldValue("fdRuleObj":U       + cContainerCode))
    dInsurerObj            = DECIMAL(oRequestHelper:getFieldValue("fdInsurerObj":U    + cContainerCode))
    
    dEffectiveDate         = (IF oRequestHelper:getFieldValue("fdEffectiveDate":U + cContainerCode) <> "yyyy/mm/dd":U
                              THEN DATE(oRequestHelper:getFieldValue("fdEffectiveDate":U + cContainerCode))
                              ELSE ?)
                              
    dEndDate               = (IF oRequestHelper:getFieldValue("fdEndDate":U + cContainerCode) <> "yyyy/mm/dd":U 
                              THEN DATE(oRequestHelper:getFieldValue("fdEndDate":U + cContainerCode))
                              ELSE ?)
                              
    lSystemOwned           = (IF oRequestHelper:getFieldValue("flSystemOwned":U       + cContainerCode) = "yes":U
                              THEN TRUE ELSE FALSE) 

    dAuthRuleLinkObj       = DECIMAL(oRequestHelper:getFieldValue("fdLinkRuleObj":U   + cContainerCode))                             
    NO-ERROR.
  
  IF NOT {&ErrorStatus} THEN
  DO:
    CASE cRecordAction:
      WHEN "modify":U THEN
      DO:
        oAuthRule:focusAuthRule(dAuthRuleObj) NO-ERROR.
        
        ASSIGN           
           oAuthRule:LineNumber       = iLineNumber
           oAuthRule:EffectiveDate    = dEffectiveDate
           oAuthRule:EndDate          = dEndDate
           oAuthRule:InsurerObj       = dInsurerObj           
           oAuthRule:OptionCode       = iOptionCode
           oAuthRule:RuleCode         = cAuthRuleCode
           oAuthRule:RuleDescription  = cAuthRuleDescription
           oAuthRule:RuleType         = cAuthRuleType           
           oAuthRule:RuleValue        = cAuthRuleValue
           oAuthRule:RuleValidValues  = cAuthRuleValidValues
           oAuthRule:SystemOwned      = lSystemOwned
           oAuthRule:LinkAuthRuleObj  = dAuthRuleLinkObj
           
           lSuccess                   = oAuthRule:SaveAuthRule()            
        NO-ERROR.               
               
        IF NOT {&ErrorStatus} AND NOT oAuthRule:ErrorObject:ErrorsExist 
        THEN
          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U 
            lSuccess                        = oResponseHelper:addFieldValue("fdRuleObj":U + cContainerCode, STRING(oAuthRule:AuthRuleObj))
           NO-ERROR.            
     
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN
      DO:  
        IF NOT lSystemOwned THEN DO:
           ASSIGN lSuccess = oAuthRule:focusAuthRule (dAuthRuleObj) NO-ERROR.
         
           IF NOT {&ErrorStatus} AND NOT oAuthRule:AuthRuleInFocus 
           THEN  
             ASSIGN 
               oResponseHelper:RequestValid    = FALSE
               oResponseHelper:ResponseMessage = "Record could not be deleted":U 
             NO-ERROR.
           ELSE 
             ASSIGN lSuccess = oAuthRule:removeAuthRule() NO-ERROR.
        
           IF NOT {&ErrorStatus} AND NOT oAuthRule:ErrorObject:ErrorsExist
           THEN
             ASSIGN 
               oResponseHelper:RequestValid    = TRUE
               oResponseHelper:ResponseMessage = "Record successfully removed":U
               oResponseHelper:ReturnValue     = "Record successfully removed":U 
               NO-ERROR.    
        END. /* IF NOT lSystemOwned */
        ELSE DO:
           IF NOT mipEnv:miUser:DevelopmentUser THEN
              ASSIGN 
                 oResponseHelper:RequestValid    = FALSE
                 oResponseHelper:ReturnValue     = "Cannot delete System Owned Rule":U
                 oResponseHelper:ResponseMessage = "Cannot delete System Owned Rule":U 
              NO-ERROR.
        END. /* ELSE DO: */
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
  
  IF {&ErrorStatus} OR oAuthRule:ErrorObject:ErrorsExist THEN
  DO:
    ASSIGN 
      oResponseHelper:RequestValid    = FALSE
      
      hErrorHandle                    = oAuthRule:ErrorObject:getErrorTableHandle()
      
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
      lSuccess                        = oResponseHelper:setError(hErrorHandle)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
  
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
  END.   /*IF oUserFlag:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
  
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthRule)       THEN DELETE OBJECT oAuthRule       NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }


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
  DEFINE INPUT PARAMETER ipcWhatToDo   AS CHARACTER            NO-UNDO.
  
  DEFINE VARIABLE lSuccess             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSystemOwned         AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cRecordAction        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthRuleCode        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthRuleDescription AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthRuleType        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthRuleValue       AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthRuleValidValues AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iRow                 AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iLineNumber          AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iOptionCode          AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dAuthRuleObj         AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dInsurerObj          AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dEffectiveDate       AS DATE                 NO-UNDO.
  DEFINE VARIABLE dEndDate             AS DATE                 NO-UNDO.
  DEFINE VARIABLE oSearch              AS cls.maauthrulesearch NO-UNDO.
   
  
  DATASET dsAuthRule:EMPTY-DATASET().
    
  oSearch = NEW cls.maauthrulesearch(DATASET dsAuthRule BY-REFERENCE).
  
  IF goWob:Mode = "Search":U THEN
  DO:
    IF CAN-DO("SearchSubmit":U, goWob:SubmitValue) THEN
    DO:        
      DO-BLK:
      DO iRow = 1 TO INTEGER(get-value(goCntSearchResults:ContainerCode + "_rowsrendered":U)):
      
        ASSIGN
          cRecordAction          =         get-value("fcAction":U        + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthRuleCode          =         get-value("fcRuleCode":U      + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthRuleDescription   =         get-value("fcDescription":U   + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthRuleType          =         get-value("cbRuleType":U      + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthRuleValue         =         get-value("fcRuleValue":U     + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthRuleValidValues   =         get-value("fcValidValues":U   + goCntSearchResults:ContainerCode + STRING(iRow))  
          iLineNumber            = INTEGER(get-value("fiLineNumber":U    + goCntSearchResults:ContainerCode + STRING(iRow)))
          iOptionCode            = INTEGER(get-value("fiOptionCode":U    + goCntSearchResults:ContainerCode + STRING(iRow)))
          dAuthRuleObj           = DECIMAL(get-value("fdRuleObj":U       + goCntSearchResults:ContainerCode + STRING(iRow)))
          
          dAuthRuleObj           = (IF dAuthRuleObj <= 0 
                                    THEN iRow * -1 ELSE dAuthRuleObj) 
                                   
          dInsurerObj            = DECIMAL(get-value("fdInsurerObj":U    + goCntSearchResults:ContainerCode + STRING(iRow)))
          dEffectiveDate         =    DATE(get-value("fdEffectiveDate":U + goCntSearchResults:ContainerCode + STRING(iRow)))
          dEndDate               =    DATE(get-value("fdEndDate":U       + goCntSearchResults:ContainerCode + STRING(iRow)))
          lSystemOwned           =     (IF get-value("flSystemOwned":U   + goCntSearchResults:ContainerCode + STRING(iRow)) = "yes":U
                                        THEN TRUE ELSE FALSE).
          
        /* We dont want to update records that did not change but create a temp table record for all rows
           so that if there are any errors, the screen will be built for the user as it was when they submitted
           from this temp table*/
        IF cAuthRuleCode <> "":U THEN
        DO:
          CREATE tt_auth_rule.
        
          ASSIGN
             tt_auth_rule.record_action      = cRecordAction
             tt_auth_rule.line_number        = iRow
             tt_auth_rule.auth_rule_obj      = dAuthRuleObj
             tt_auth_rule.effective_date     = dEffectiveDate
             tt_auth_rule.end_date           = dEndDate
             tt_auth_rule.insurer_obj        = dInsurerObj
             tt_auth_rule.option_code        = iOptionCode
             tt_auth_rule.rule_code          = cAuthRuleCode
             tt_auth_rule.rule_description   = cAuthRuleDescription
             tt_auth_rule.rule_type          = cAuthRuleType
             tt_auth_rule.rule_value         = cAuthRuleValue
             tt_auth_rule.rule_valid_values  = cAuthRuleValidValues
             tt_auth_rule.system_owned       = lSystemOwned.
             
          VALIDATE tt_auth_rule.
          
          IF cRecordAction = "Delete":U THEN DO:
             CREATE tt_deleted.
             ASSIGN tt_deleted.owning_obj = dAuthRuleObj
                    tt_deleted.owning_key = "":U.
                
             VALIDATE tt_deleted.
          END. /*IF cRecordAction = "Delete":U THEN*/
        END. /*IF cAuthRuleCode <> "":U*/      
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
        cAuthRuleCode          =         get-value("fcRuleCode":U      + goCntMaint:ContainerCode)
        cAuthRuleDescription   =         get-value("fcDescription":U   + goCntMaint:ContainerCode)
        cAuthRuleType          =         get-value("cbRuleType":U      + goCntMaint:ContainerCode)
        cAuthRuleValue         =         get-value("fcRuleValue":U     + goCntMaint:ContainerCode)      
        cAuthRuleValidValues   =         get-value("fcValidValues":U   + goCntMaint:ContainerCode)  
        iOptionCode            = INTEGER(get-value("fiOptionCode":U    + goCntMaint:ContainerCode))
        dInsurerObj            = DECIMAL(get-value("fdInsurerObj":U    + goCntMaint:ContainerCode))
        dEffectiveDate         =    DATE(get-value("fdEffectiveDate":U + goCntMaint:ContainerCode))
        dEndDate               =    DATE(get-value("fdEndDate":U       + goCntMaint:ContainerCode))
        lSystemOwned           =     (IF get-value("flSystemOwned":U   + goCntMaint:ContainerCode) = "yes":U
                                      THEN TRUE ELSE FALSE).
      
      IF goWob:CurrentObj <> "":U AND ipcWhatToDo <> "Submit->Add":U THEN
      DO:
        oSearch:SetFilterCriteria("tt_auth_rule.auth_rule_obj":U, "=":U, DECIMAL(goWob:CurrentObj)).
        
        oSearch:fetchData().
      END. /*IF goWob:CurrentObj <> "":U THEN*/
      
      
      FIND FIRST tt_auth_rule NO-LOCK NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
      
      
      IF ipcWhatToDo = "Submit->Add":U OR ipcWhatToDo = "Submit->Change":U AND cAuthRuleCode <> "":U THEN
      DO:
        IF NOT AVAILABLE tt_auth_rule THEN
        DO:
          CREATE tt_auth_rule.
          
          ASSIGN tt_auth_rule.auth_rule_obj = (IF ipcWhatToDo = "Submit->Add":U 
                                               THEN 0 ELSE DECIMAL(goWob:CurrentObj)).
        END. /*IF NOT AVAILABLE tt_auth_rule THEN*/
          
      
        IF AVAILABLE tt_auth_rule
        THEN
          ASSIGN
             tt_auth_rule.record_action      = "MODIFY":U
                                                
             tt_auth_rule.line_number        = 1
             
             tt_auth_rule.effective_date     = dEffectiveDate
             tt_auth_rule.end_date           = dEndDate
             tt_auth_rule.insurer_obj        = dInsurerObj
             tt_auth_rule.option_code        = iOptionCode
             tt_auth_rule.rule_code          = cAuthRuleCode
             tt_auth_rule.rule_description   = cAuthRuleDescription
             tt_auth_rule.rule_type          = cAuthRuleType
             tt_auth_rule.rule_value         = cAuthRuleValue
             tt_auth_rule.rule_valid_values  = cAuthRuleValidValues
             tt_auth_rule.system_owned       = lSystemOwned.
        
        VALIDATE tt_auth_rule.
      END. /*IF cAuthRuleCode <> "":U AND cRecordAction = "MODIFY":U THEN*/                                                                                 
      
      IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_rule THEN
      DO:
        ASSIGN tt_auth_rule.record_action = "DELETE":U.
        
        VALIDATE tt_auth_rule.
      END. /*IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_rule THEN*/
    END. /*IF CAN-DO("Submit":U, goWob:SubmitValue) THEN*/
  END. /*IF goWob:Mode = "Submit":U THEN*/
  
  
  IF CAN-DO("SearchSubmit,Submit,Confirm":U, goWob:SubmitValue) THEN
  DO:
&IF {&DBDFMA} >= 010195 &THEN
    mipEnv:Health:AuthMaintenance:saveAuthRule(INPUT-OUTPUT DATASET dsAuthRule BY-REFERENCE).
&ENDIF    
    
    ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_auth_rule_error).
        
    IF goWob:SubmitValue = "SearchSubmit":U THEN
    DO:
      FOR EACH tt_auth_rule_error NO-LOCK:
      
        FIND FIRST tt_deleted NO-LOCK
             WHERE tt_deleted.owning_obj = tt_auth_rule_error.owning_obj
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
        
        IF AVAILABLE tt_deleted 
        THEN DELETE tt_deleted. 
      
      END. /*FOR EACH tt_auth_rule_error NO-LOCK:*/
    END. /*IF goWob:SubmitValue = "SearchSubmit":U THEN*/
    
    
    IF goWob:Mode = "Maint":U THEN
    DO:
      IF Warpspeed:ValidationError
      THEN 
        mipEnv:Health:maUiService:setContainerErrors(TEMP-TABLE tt_auth_rule_error:HANDLE, goCntMaint, "hacar":U, DECIMAL(Warpspeed:CurrentObj), "":U).
                
      IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN
      DO:
        FIND FIRST tt_auth_rule NO-LOCK NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
        
        ASSIGN 
           goWob:CurrentObj     = STRING(tt_auth_rule.auth_rule_obj)
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
  
  DEFINE VARIABLE lSuccess  AS LOGICAL NO-UNDO.
  
  DEFINE VARIABLE lReadOnly AS LOGICAL NO-UNDO.
  
  
  CASE ipoControl:RenderArgument:    
 
    WHEN "PrimaryField":U THEN 
    DO:
    
      /* 
        Check if record already has an Obj - if so, make the unique-index fields Disabled 
      */
      ASSIGN lReadOnly               = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_rule.auth_rule_obj":U, "BUFFER-VALUE":U)) > 0
      
             ipoControl:ControlToken = (IF NOT lReadOnly OR ipoControl:ValidationError
                                        THEN "Updatable":U 
                                        ELSE 
                                          (IF ipoControl:ControlType = "wsLookupButton":U
                                           THEN "Hidden":U
                                           ELSE "Disabled":U)).   
                                           
      ipoControl:Render().
      
    END.  /* WHEN "PrimaryField" THEN DO */
  END CASE. /* CASE ipoControl:RenderArgument */

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainer Procedure 
PROCEDURE defineContainer :
/*------------------------------------------------------------------------------
  Purpose   : Define authorisation rule specific containers
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
    goCntMaint                  = goWob:getContainer("Maint":U)
    
    goCntMaint:ContainerMode    = goWob:SubmitValue
    goCntMaint:ViewOnly         = TRUE
    goCntMaint:QueryString      = "FOR EACH tt_auth_rule NO-LOCK":U
                                + "   WHERE tt_auth_rule.auth_rule_obj = &1":U
                                
    oControl                    = goCntMaint:addControl("fcRuleCode":U                + goCntMaint:ContainerCode , "wsInput":U        , "64":U  , "tt_auth_rule.rule_code":U        , "character":U, 1, 1, "Rule Code:":U)
    oControl:ControlToken       = "ReadOnly":U WHEN NOT CAN-DO("0,":U,goWob:CurrentObj) AND goWob:SubmitValue <> "Copy":U  
    
    oControl                    = goCntMaint:addControl("fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode , "wsInput":U        , "5":U   , "":U                              , "character":U, 1, 2, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                        
    oControl:ControlValue       = "ermin":U                                                                                                                                         
                                                                                                                                                                                    
    oControl                    = goCntMaint:addControl("fcInsurerArgumentField":U    + goCntMaint:ContainerCode , "wsInput":U        , "5":U   , "":U                              , "character":U, 1, 2, "":U)
    oControl:ControlToken       = "Hidden":U                                        
    oControl:ControlValue       = "[CodeField]":U                                   
                                                                                                                                                    
    oControl                    = goCntMaint:addControl("fdInsurerObj":U              + goCntMaint:ContainerCode , "wsInput":U        , "1":U   , "tt_auth_rule.insurer_obj":U      , "character":U, 1, 2, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                         
    
    oControl                    = goCntMaint:addControl("fcInsurer":U                 + goCntMaint:ContainerCode , "wsInput":U        , "10":U  , "":U                              , "character":U, 1, 2, "":U)
    oControl:ControlTooltip     = "Please enter a valid client":U
    oControl:AjaxValidation     = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields       = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls     = "fcInsurer":U + goCntMaint:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode + ",fcInsurerArgumentField":U + goCntMaint:ContainerCode
    oControl:ReturnFields       = "[RecordObj]":U
    oControl:ReturnControls     = "fdInsurerObj":U + goCntMaint:ContainerCode
    oControl:ControlToken       = "Readonly":U WHEN goWob:SubmitValue = "Change":U 
    
    oControl                    = goCntMaint:addControl("buInsurerBtn":U              + goCntMaint:ContainerCode , "wsLookupButton":U , "":U    , "":U                              , "":U         , 1, 2, "Client:":U)
    oControl:LookupWobFLA       = "ermin":U                                                                                                                                         
    oControl:LookupFields       = "erm_insurer.insurer_code":U                                                                                                                      
    oControl:LookupControls     = "fcInsurer":U + goCntMaint:ContainerCode                                                                                                                                                              
    oControl:ReturnFields       = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U                                                                                              
    oControl:ReturnControls     = "fcInsurerObj":U + goCntMaint:ContainerCode + ",fcInsurer":U + goCntMaint:ContainerCode                                                                                                
    oControl:CellLayoutMask     = "&1&2&3&4&5":U        
    oControl:ControlToken       = "Readonly":U WHEN goWob:SubmitValue = "Change":U 
                                                                                                                                                                                    
    oControl                    = goCntMaint:addControl("cbRuleType":U                + goCntMaint:ContainerCode , "wsCombo":U        , "17":U  , "tt_auth_rule.rule_type":U        , "character":U, 2, 1, "Rule Type:":U)
    oControl:RenderProcedure    = "RenderProcedure":U                                                                                                                               
    oControl:RenderArgument     = "AcronymSelect:ma_acAuthRuleType:<All>=":U                                                                                                                                                
    oControl:ControlToken       = "Readonly":U WHEN goWob:SubmitValue = "Change":U 
                                                                                                                                                                                    
    oControl                    = goCntMaint:addControl("fcOptionArgumentMnemonic":U  + goCntMaint:ContainerCode, "wsInput":U         , "5":U   , "":U                              , "character":U, 2, 2, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                        
    oControl:ControlValue       = "scheme":U                                                                                                                                        
                                                                                                                                                                                    
    oControl                    = goCntMaint:addControl("fcOptionArgumentField":U     + goCntMaint:ContainerCode, "wsInput":U         , "5":U   , "":U                              , "character":U, 2, 2, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                        
    oControl:ControlValue       = "[CodeField]":U                                                                                                                                   
                                                                                                                                                                                    
    oControl                    = goCntMaint:addControl("fiOptionCode":U              + goCntMaint:ContainerCode , "wsInput":U        , "10":U  , "tt_auth_rule.option_code":U      , "integer":U  , 2, 2, "":U)
    oControl:ControlFormat      = ">>9":U          
    oControl:ControlTooltip     = "Please enter a valid option":U
    oControl:AjaxValidation     = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields       = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls     = "fiOptionCode":U + goCntMaint:ContainerCode + ",fcOptionArgumentMnemonic":U + goCntMaint:ContainerCode + ",fcOptionArgumentField":U + goCntMaint:ContainerCode
    oControl:ControlToken       = "Readonly":U WHEN goWob:SubmitValue = "Change":U 
    
    oControl                    = goCntMaint:addControl("buOptionBtn":U               + goCntMaint:ContainerCode , "wsLookupButton":U , "":U    , "":U                              , "":U         , 2, 2, "Option:":U)
    oControl:LookupWobFLA       = "scheme":U                                                                                                                                        
    oControl:LookupFields       = "scheme.scheme-code":U   
    oControl:LookupControls     = "fiOptionCode":U + goCntMaint:ContainerCode
    oControl:FilterFields       = "scheme.scheme-code":U                          
    oControl:FilterControls     = "fiOptionCode":U + goCntMaint:ContainerCode
    oControl:ReturnFields       = "scheme.scheme-code":U     
    oControl:ReturnControls     = "fiOptionCode":U + goCntMaint:ContainerCode
    oControl:CellLayoutMask     = "&1&2&3&4":U
    oControl:ControlToken       = "Readonly":U WHEN goWob:SubmitValue = "Change":U 
                                
    oControl                    = goCntMaint:addControl("fcDescription":U             + goCntMaint:ContainerCode , "wsTextArea":U     , "60,2":U, "tt_auth_rule.rule_description":U , "character":U, 3, 1, "Description:":U) 
    oControl                    = goCntMaint:addControl("fcValidValues":U             + goCntMaint:ContainerCode , "wsTextArea":U     , "60,1":U, "tt_auth_rule.rule_valid_values":U, "character":U, 4, 1, "Valid Values:":U) 
    oControl                    = goCntMaint:addControl("fcRuleValue":U               + goCntMaint:ContainerCode , "wsTextArea":U     , "60,3":U, "tt_auth_rule.rule_value":U       , "character":U, 5, 1, "Rule Value:":U) 
    
    oControl                    = goCntMaint:addControl("fdEffectiveDate":U           + goCntMaint:ContainerCode , "wsInput":U        , "17":U  , "tt_auth_rule.effective_date":U   , "date":U     , 6, 1, "Effective Date:":U)
    oControl:ControlToken       = "Readonly":U WHEN goWob:SubmitValue = "Change":U 
    
    oControl                    = goCntMaint:addControl("fdEndDate":U                 + goCntMaint:ContainerCode , "wsInput":U        , "17":U  , "tt_auth_rule.end_date":U         , "date":U     , 7, 1, "End Date:":U)
    
    oControl                    = goCntMaint:addControl("flSystemOwned":U             + goCntMaint:ContainerCode , "wsCheckbox":U     , "":U    , "tt_auth_rule.system_owned":U     , "logical":U  , 8, 1, "System Owned:":U) 
    oControl:ControlToken       = "Disabled":U WHEN NOT mipEnv:miUser:DevelopmentUser.                                                                                                                                        

  ASSIGN goCntAudit              = mipEnv:Health:maUtility:getAuditContainer()
         goCntAudit:RowsToRender = ?.
  
  
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = FALSE  &Container = goCntAudit  }
  
    
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
  
  
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO. 
  
  
  /* SearchFilter */
  ASSIGN
    goCntSearchFilter                         = goWob:getContainer("SearchFilter":U) 
    
    goCntSearchFilter:ContainerTitle          = "Authorisation Rules"
                                        
    oControl                                  = goCntSearchFilter:addControl("fcRuleCode":U                + goCntSearchFilter:ContainerCode  , "wsInput":U        , "30":U, "":U, "character":U, 1, 1, "Rule Code:":U)
    oControl                                  = goCntSearchFilter:addControl("fcDescription":U             + goCntSearchFilter:ContainerCode  , "wsInput":U        , "70":U, "":U, "character":U, 2, 1, "Description:":U)
                                                                                                           
    oControl                                  = goCntSearchFilter:addControl("fcRuleType":U                + goCntSearchFilter:ContainerCode  , "wsCombo":U        , "25":U, "":U, "character":U, 3, 1, "Rule Type:":U)
    oControl:RenderProcedure                  = "RenderProcedure":U                                                     
    oControl:RenderArgument                   = "AcronymSelect:ma_acAuthRuleType:<All>=":U                                
                                             
    oControl                                  = goCntSearchFilter:addControl("fcInsurerArgumentMnemonic":U + goCntSearchFilter:ContainerCode  , "wsInput":U       , "5":U  , "":U, "character":U, 3, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                                                                    
    oControl:ControlValue                     = "ermin":U                                                                                     
                                                                                                                                              
    oControl                                  = goCntSearchFilter:addControl("fcInsurerArgumentField":U    + goCntSearchFilter:ContainerCode  , "wsInput":U       , "5":U  , "":U, "character":U, 3, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                           
    oControl:ControlValue                     = "[CodeField]":U                                      
                                           
    oControl                                  = goCntSearchFilter:addControl("fdInsurerObj":U              + goCntSearchFilter:ContainerCode  , "wsInput":U        , "1":U , "":U, "character":U, 3, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                                                                
    oControl                                  = goCntSearchFilter:addControl("fcInsurer":U                 + goCntSearchFilter:ContainerCode  , "wsInput":U        , "10":U, "":U, "character":U, 3, 2, "":U)
    oControl:ControlTooltip                   = "Please enter a valid client":U
    oControl:AjaxValidation                   = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields                     = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls                   = "fcInsurer":U + goCntSearchFilter:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntSearchFilter:ContainerCode + ",fcInsurerArgumentField":U + goCntSearchFilter:ContainerCode
    oControl:ReturnFields                     = "[RecordObj]":U
    oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchFilter:ContainerCode
  
    oControl                                  = goCntSearchFilter:addControl("buInsurerBtn":U              + goCntSearchFilter:ContainerCode  , "wsLookupButton":U , "":U  , "":U, "":U         , 3, 2, "Client:":U)
    oControl:LookupWobFLA                     = "ermin":U                                                                    
    oControl:LookupFields                     = "erm_insurer.insurer_code":U                                                 
    oControl:LookupControls                   = "fcInsurer":U + goCntSearchFilter:ContainerCode                                                               
    oControl:ReturnFields                     = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U                         
    oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchFilter:ContainerCode + ",fcInsurer":U + goCntSearchFilter:ContainerCode                                                   
    oControl:CellLayoutMask                   = "&1&2&3&4&5":U                                                             
                                                                                                                
    oControl                                  = goCntSearchFilter:addControl("fdEffectiveDate":U           + goCntSearchFilter:ContainerCode  , "wsInput":U        , "10":U, "":U, "date":U     , 4, 1, "Effective Date:":U)
        
    oControl                                  = goCntSearchFilter:addControl("fcOptionArgumentMnemonic":U  + goCntSearchFilter:ContainerCode  , "wsInput":U        , "5":U , "":U, "character":U, 4, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                         
    oControl:ControlValue                     = "scheme":U                                         
                                                                                                   
    oControl                                  = goCntSearchFilter:addControl("fcOptionArgumentField":U     + goCntSearchFilter:ContainerCode  , "wsInput":U        , "5":U , "":U, "character":U, 4, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                           
    oControl:ControlValue                     = "[CodeField]":U                                      
                                                                                                           
    oControl                                  = goCntSearchFilter:addControl("fiOptionCode":U              + goCntSearchFilter:ContainerCode  , "wsInput":U        , "10":U, "":U, "integer":U  , 4, 2, "":U)
    oControl:ControlFormat                    = ">>9":U          
    oControl:AjaxValidation                   = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields                     = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode + ",fcOptionArgumentMnemonic":U + goCntSearchFilter:ContainerCode + ",fcOptionArgumentField":U + goCntSearchFilter:ContainerCode
  
    oControl                                  = goCntSearchFilter:addControl("buOptionBtn":U               + goCntSearchFilter:ContainerCode  , "wsLookupButton":U , "":U  , "":U, "":U         , 4, 2, "Option:":U)
    oControl:LookupWobFLA                     = "scheme":U
    oControl:LookupFields                     = "scheme.scheme-code":U   
    oControl:LookupControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode                                                  
    oControl:FilterFields                     = "scheme.scheme-code":U                          
    oControl:FilterControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode                                                  
    oControl:ReturnFields                     = "scheme.scheme-code":U     
    oControl:ReturnControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode                                                  
    oControl:CellLayoutMask                   = "&1&2&3&4":U   
                                             
    oControl                                  = goCntSearchFilter:addControl("fdEndDate":U                 + goCntSearchFilter:ContainerCode  , "wsInput":U        , "10":U, "":U, "date":U     , 5, 1, "End Date:":U)      
    
    oControl                                  = goCntSearchFilter:addControl("fcSystemOwned":U             + goCntSearchFilter:ContainerCode  , "wsCombo":U        , "10":U, "":U, "character":U, 5, 2, "System Owned:":U)      
    oControl:AdditionalItems                  = "=|Yes=Yes|No=No":U  
    .      
  
         
  RUN defineContainerSearchResults IN THIS-PROCEDURE.

  ASSIGN

    oContainerProperties                        = NEW cls.wscontainerproperties(goCntSearchResults)
    oContainerProperties:DefaultLess            = TRUE
    oContainerProperties:CollapsableControlList = "flSystemOwned" + goCntSearchResults:ContainerCode.
                          
  
   { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE &Container = goCntSearchResults &ContainerProperties = oContainerProperties}
  
  
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerSearchResults Procedure 
PROCEDURE defineContainerSearchResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE iControl             AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
  
    /*Search Results*/
  ASSIGN              
    goCntSearchResults                          = NEW cls.mipwscontainer("SearchResults":U + goWob:ObjectCode, "99%":U, "Authorisation Rule Maintenance":U, WarpSpeed:BaseClass, TRUE)
    goCntSearchResults:ContainerMode            = goWob:SubmitValue
    goCntSearchResults:ShowGenericReportPrint   = TRUE
    goCntSearchResults:Collapsed                = FALSE
    goCntSearchResults:ViewOnly                 = NOT glEnquiryWob  
    goCntSearchResults:RowsToRender             = ?
    goCntSearchResults:RowRenderProcedure       = "rowRenderProcedure":U
    goCntSearchResults:RowRenderArgument        = goCntSearchResults:ContainerCode
    goCntSearchResults:DefaultContainerType     = "TABLE":U                                                                                                                                                           
    goCntSearchResults:ContainerTitle           = "Authorisation Rule Results":U                                                                                                                                                         
    goCntSearchResults:QueryString              = "FOR EACH tt_auth_rule NO-LOCK,":U
                                                + "   FIRST tt_insurer NO-LOCK ":U
                                                + "   WHERE tt_insurer.insurer_obj = tt_auth_rule.insurer_obj OUTER-JOIN,":U  
                                                + "   FIRST hac_auth_rule NO-LOCK ":U
                                                + "   WHERE hac_auth_rule.auth_rule_obj = tt_auth_rule.link_auth_rule_obj OUTER-JOIN":U 
                                                + "      BY tt_auth_rule.line_number BY tt_auth_rule.rule_type   BY tt_auth_rule.rule_code"
                                                + "      BY tt_auth_rule.insurer_obj BY tt_auth_rule.option_code BY tt_auth_rule.effective_date":U

    lSuccess                                    = goWob:setContainer("SearchResults":U,  goCntSearchResults)
                                          
    oControl                                    = goCntSearchResults:addControl("fdRuleObj":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "1":U   , "tt_auth_rule.auth_rule_obj":U    , "character":U, 2, "":U)
    oControl:ControlToken                       = "Hidden":U
                                          
    oControl                                    = goCntSearchResults:addControl("fcRuleCode":U                + goCntSearchResults:ContainerCode  , "wsInput":U        , "27":U   , "tt_auth_rule.rule_code":U         , "character":U, 3, "Rule Code":U)
    oControl:ControlClass                       = "+clMan":U
    oControl:ErrorMessage                       = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Rule code'"}, "TEXT":U)
    oControl:JavascriptOnBlur                   = "fnValidateField(this, true, ":U + QUOTER(oControl:ErrorMessage) + ", (this.value.length == 0));":U
    oControl:RenderProcedure                    = "CustomRenderProcedure":U
    oControl:RenderArgument                     = "PrimaryField":U
                                          
    oControl                                    = goCntSearchResults:addControl("cbRuleType":U                + goCntSearchResults:ContainerCode  , "wsCombo":U        , "10":U   , "tt_auth_rule.rule_type":U        , "character":U, 4, "Rule Type":U)
    oControl:AdditionalItems                    = "=":U
    oControl:KeyField                           = "mic_acronym.acronym_key":U
    oControl:DisplayFields                      = "mic_acronym.acronym_label":U
    oControl:QueryString                        = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthRuleType' BY mic_acronym.acronym_label":U
    oControl:ControlClass                       = "+clMan":U
    oControl:ErrorMessage                       = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Rule type'"}, "TEXT":U)
    oControl:JavascriptOnBlur                   = "var lSuccess=fnValidateField(this, true, ":U + QUOTER(oControl:ErrorMessage) + ", (this.value.length == 0));if(!lSuccess)return false;":U
    oControl:RenderProcedure                    = "CustomRenderProcedure":U
    oControl:RenderArgument                     = "PrimaryField":U

    oControl                                    = goCntSearchResults:addControl("fcDescription":U             + goCntSearchResults:ContainerCode  , "wsTextArea":U     , "15,2":U  , "tt_auth_rule.rule_description":U , "character":U, 5, "Description":U)
    oControl:ErrorMessage                       = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Rule description'"}, "TEXT":U)
    oControl:ControlClass                       = "+clMan":U
    oControl:JavascriptOnBlur                   = "fnValidateField(this, true, ~"A valid auth rule description must be entered~", (this.value.length==0));":U
                                                + "fnValidateField(this, true, ~"The length must be greater than 5 characters~", (this.value.length>=1&&this.value.length<6));":U
                                          
    oControl                                    = goCntSearchResults:addControl("fcValidValues":U             + goCntSearchResults:ContainerCode  , "wsTextArea":U     , "10,2":U  , "tt_auth_rule.rule_valid_values":U, "character":U, 6, "Valid Values":U)
                                          
    oControl                                    = goCntSearchResults:addControl("fcRuleValue":U               + goCntSearchResults:ContainerCode  , "wsTextArea":U     , "15,2":U  , "tt_auth_rule.rule_value":U       , "character":U, 7, "Rule Value":U)
    oControl:ErrorMessage                       = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Rule value'"}, "TEXT":U)
    oControl:JavascriptOnBlur                   = "fnValidateField(this, true, ":U + QUOTER(oControl:ErrorMessage) + ", (this.value.length == 0));":U
                                          
    oControl                                    = goCntSearchResults:addControl("fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode  , "wsInput":U       , "1":U     , "":U                              , "character":U, 8, "":U)
    oControl:ControlToken                       = "Hidden":U
    oControl:ControlValue                       = "ermin":U
                                          
    oControl                                    = goCntSearchResults:addControl("fcInsurerArgumentField":U    + goCntSearchResults:ContainerCode  , "wsInput":U       , "1":U     , "":U                              , "character":U, 8, "":U)
    oControl:ControlToken                       = "Hidden":U
    oControl:ControlValue                       = "[CodeField]":U
                                          
    oControl                                    = goCntSearchResults:addControl("fdInsurerObj":U              + goCntSearchResults:ContainerCode  , "wsInput":U        , "1":U    , "tt_auth_rule.insurer_obj":U      , "character":U, 8, "":U)
    oControl:ControlToken                       = "Hidden":U
                                          
    oControl                                    = goCntSearchResults:addControl("fcInsurer":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "5":U    , "tt_insurer.insurer_code":U       , "character":U, 8, "":U)
    oControl:JavascriptOnChange                 = "if(this.value==~"~")~{fnSetControlValue(~"fdInsurerObj" + goCntSearchResults:ContainerCode + "~" + fnGetControlSuffix(this),~"~");~}"
    oControl:ControlTooltip                     = "Please enter a valid client":U
    oControl:AjaxValidation                     = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields                       = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls                     = "fcInsurer":U + goCntSearchResults:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode + ",fcInsurerArgumentField":U + goCntSearchResults:ContainerCode
    oControl:ReturnFields                       = "[RecordObj]":U
    oControl:ReturnControls                     = "fdInsurerObj":U + goCntSearchResults:ContainerCode
    oControl:RenderProcedure                    = "CustomRenderProcedure":U
    oControl:RenderArgument                     = "PrimaryField":U

    oControl                                    = goCntSearchResults:addControl("buInsurerBtn":U              + goCntSearchResults:ContainerCode  , "wsLookupButton":U , "":U     , "":U                              , "":U         , 8, "Client":U)
    oControl:LookupWobFLA                       = "ermin":U
    oControl:LookupFields                       = "erm_insurer.insurer_code":U
    oControl:LookupControls                     = "fcInsurer":U + goCntSearchResults:ContainerCode
    oControl:ReturnFields                       = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U
    oControl:ReturnControls                     = "fdInsurerObj":U + goCntSearchResults:ContainerCode + ",fcInsurer":U + goCntSearchResults:ContainerCode
    oControl:CellLayoutMask                     = "&1&2&3&4&5":U
    oControl:RenderProcedure                    = "CustomRenderProcedure":U
    oControl:RenderArgument                     = "PrimaryField":U
                                          
    oControl                                    = goCntSearchResults:addControl("fcOptionArgumentMnemonic":U  + goCntSearchResults:ContainerCode  , "wsInput":U        , "1":U    , "":U                              , "character":U, 9, "":U)
    oControl:ControlToken                       = "Hidden":U
    oControl:ControlValue                       = "scheme":U
                                          
    oControl                                    = goCntSearchResults:addControl("fcOptionArgumentField":U     + goCntSearchResults:ContainerCode  , "wsInput":U        , "1":U    , "":U                              , "character":U, 9, "":U)
    oControl:ControlToken                       = "Hidden":U
    oControl:ControlValue                       = "[CodeField]":U

    oControl                                    = goCntSearchResults:addControl("fiOptionCode":U              + goCntSearchResults:ContainerCode  , "wsInput":U        , "2":U    , "tt_auth_rule.option_code":U      , "integer":U  , 9, "":U)
    oControl:ControlFormat                      = ">>9":U
    oControl:ControlTooltip                     = "Please enter a valid option":U
    oControl:AjaxValidation                     = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields                       = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls                     = "fiOptionCode":U + goCntSearchResults:ContainerCode + ",fcOptionArgumentMnemonic":U + goCntSearchResults:ContainerCode + ",fcOptionArgumentField":U + goCntSearchResults:ContainerCode
    oControl:RenderProcedure                    = "CustomRenderProcedure":U
    oControl:RenderArgument                     = "PrimaryField":U
                                           
    oControl                                    = goCntSearchResults:addControl("buOptionBtn":U               + goCntSearchResults:ContainerCode  , "wsLookupButton":U , "":U     , "":U                              , "":U         , 9, "Option":U)
    oControl:LookupWobFLA                       = "scheme":U
    oControl:LookupFields                       = "scheme.scheme-code":U
    oControl:LookupControls                     = "fiOptionCode":U + goCntSearchResults:ContainerCode
    oControl:FilterFields                       = "scheme.scheme-code":U
    oControl:FilterControls                     = "fiOptionCode":U + goCntSearchResults:ContainerCode
    oControl:ReturnFields                       = "scheme.scheme-code":U
    oControl:ReturnControls                     = "fiOptionCode":U + goCntSearchResults:ContainerCode
    oControl:CellLayoutMask                     = "&1&2&3&4":U
    oControl:RenderProcedure                    = "CustomRenderProcedure":U
    oControl:RenderArgument                     = "PrimaryField":U

    oControl                                    = goCntSearchResults:addControl("fcLinkRule":U                + goCntSearchResults:ContainerCode  , "wsInput":U        ,"8":U    , "hac_auth_rule.rule_code":U       , "character":U, 10, "":U)
    oControl:JavascriptOnChange                 = "fnSetControlValue(~"fdLinkRuleObj":U + goCntSearchResults:ContainerCode + "~" + fnGetControlSuffix(this),~"~");":U
    oControl:AjaxValidation                     = "SERVICE:wsUIService:ajaxValidation:hacar|rule_code":U
    oControl:FilterFields                       = "[ArgumentFieldValue]":U
    oControl:ReturnFields                       = "[RecordObj]":U
    oControl:ReturnControls                     = "fdLinkRuleObj":U + goCntSearchResults:ContainerCode

    oControl                                    = goCntSearchResults:addControl("fdLinkRuleObj":U             + goCntSearchResults:ContainerCode  , "wsInput":U        ,"1":U     , "hac_auth_rule.auth_rule_obj":U   , "decimal":U  ,   10, "":U)
    oControl:ControlToken                       = "Hidden":U

    oControl                                    = goCntSearchResults:addControl("fcLinkRuleLkp":U             + goCntSearchResults:ContainerCode  , "wsLookupButton":U , "":U     , "":U                              , "":U         , 10, "Linked Rule":U)
    oControl:LookupWobFLA                       = "hacar":U
    oControl:LookupFields                       = "hac_auth_rule.auth_rule_obj":U
    oControl:LookupControls                     = "fdLinkRuleObj":U + goCntSearchResults:ContainerCode
    oControl:FilterFields                       = "hac_auth_rule.rule_code":U
    oControl:FilterControls                     = "fcLinkRule":U + goCntSearchResults:ContainerCode
    oControl:ReturnFields                       = "hac_auth_rule.auth_rule_obj":U
    oControl:ReturnControls                     = "fdLinkRuleObj":U + goCntSearchResults:ContainerCode
    oControl:CellLayoutMask                     = "&1&2&3":U  
    
    oControl                                    = goCntSearchResults:addControl("fdEffectiveDate":U           + goCntSearchResults:ContainerCode , "wsInput":U        , "8":U   , "tt_auth_rule.effective_date":U    , "date":U     ,11, "Effective Date":U)
    oControl:ControlClass                       = "+clMan":U
    oControl:ErrorMessage                       = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Effective date'"}, "TEXT":U)
    oControl:RenderProcedure                    = "CustomRenderProcedure":U
    oControl:RenderArgument                     = "PrimaryField":U
                                             
    oControl                                    = goCntSearchResults:addControl("fdEndDate":U                 + goCntSearchResults:ContainerCode , "wsInput":U        , "8":U   , "tt_auth_rule.end_date":U          , "date":U     ,12, "End Date":U)
                                             
    oControl                                    = goCntSearchResults:addControl("flSystemOwned":U             + goCntSearchResults:ContainerCode , "wsCheckbox":U     , "":U     , "tt_auth_rule.system_owned":U      , "logical":U  ,13, "System Owned":U)
    oControl:CellCLass                          = "+clCtr"                
    .
  
  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dependencyCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dependencyCheck Procedure 
PROCEDURE dependencyCheck :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters:
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipoContainer         AS cls.mipwscontainer NO-UNDO.
  DEFINE OUTPUT PARAMETER oplDependencyExists  AS LOGICAL            NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDependencyMessage AS CHARACTER          NO-UNDO.

  DEFINE VARIABLE dAuthRuleObj AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cRuleList    AS CHARACTER   NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_rule FOR hac_auth_rule.


  IF VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) THEN
  DO:

    ASSIGN dAuthRuleObj = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_rule.auth_rule_obj":U, "BUFFER-VALUE":U)).


    FOR EACH buf_auth_rule NO-LOCK
       WHERE buf_auth_rule.link_auth_rule_obj = dAuthRuleObj:

      ASSIGN cRuleList = cRuleList 
                       + (IF cRuleList = "":U THEN "":U ELSE ",":U)
                       + buf_auth_rule.rule_code.

    END. /*FOR EACH buf_auth_rule NO-LOCK*/


    ASSIGN oplDependencyExists = cRuleList <> "":U.


    IF oplDependencyExists
    THEN
      ASSIGN opcDependencyMessage = SUBSTITUTE("Rule '&1' may not be removed as the following linked rules exist: '&2'":U, ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_rule.rule_code":U, "BUFFER-VALUE":U),
                                                                                                                           cRuleList).


  END. /*IF VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) THEN*/

&ENDIF


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
  
  DEFINE VARIABLE cRecords         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMode            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cContainerCode   AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cSystemOwned     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleCode        AS CHARACTER            NO-UNDO. 
  DEFINE VARIABLE cRuleType        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleDescription AS CHARACTER            NO-UNDO. 
  DEFINE VARIABLE iRecords         AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iRowsRendered    AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iDeleted         AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iOptionCode      AS INTEGER              NO-UNDO.   
  DEFINE VARIABLE lSuccess         AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lEnquiryMode     AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSystemOwned     AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE dInsurerObj      AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dEffectiveDate   AS DATE                 NO-UNDO.
  DEFINE VARIABLE dEndDate         AS DATE                 NO-UNDO. 
  DEFINE VARIABLE oControl         AS cls.mipwscontrol     NO-UNDO.
  DEFINE VARIABLE oSearch          AS cls.maauthrulesearch NO-UNDO.
  
  mipEnv:Health:maUtility:getInsurerDetails(
    INPUT 0.00,
    INPUT "":U,
    INPUT "":U,
    INPUT "":U,
    INPUT "":U,
    INPUT ?,
    INPUT ?,
    INPUT 0.00,
    OUTPUT TABLE tt_insurer).
                    
  ASSIGN oSearch      = NEW cls.maauthrulesearch(INPUT DATASET dsAuthRule BY-REFERENCE)
  
         lEnquiryMode = get-value("WobMode":U) = "enquiry":U.
  
  CASE goWob:Mode:
  
    WHEN "Search":U THEN
    DO: 
      ASSIGN 
         cRecords         =         goCntSearchFilter:getControl("fiRecords":U       + goCntSearchFilter:ContainerCode):ControlValue
         cMode            =         goCntSearchFilter:getControl("fcSearchMode":U    + goCntSearchFilter:ContainerCode):ControlValue
                                    
         cRuleCode        =         goCntSearchFilter:getControl("fcRuleCode":U      + goCntSearchFilter:ContainerCode):ControlValue
         cRuleType        =         goCntSearchFilter:getControl("fcRuleType":U      + goCntSearchFilter:ContainerCode):ControlValue
         cRuleDescription =         goCntSearchFilter:getControl("fcDescription":U   + goCntSearchFilter:ContainerCode):ControlValue
         dInsurerObj      = DECIMAL(goCntSearchFilter:getControl("fdInsurerObj":U    + goCntSearchFilter:ContainerCode):ControlValue)
         iOptionCode      = INTEGER(goCntSearchFilter:getControl("fiOptionCode":U    + goCntSearchFilter:ContainerCode):ControlValue)
         dEffectiveDate   =    DATE(goCntSearchFilter:getControl("fdEffectiveDate":U + goCntSearchFilter:ContainerCode):ControlValue)
         dEndDate         =    DATE(goCntSearchFilter:getControl("fdEndDate":U       + goCntSearchFilter:ContainerCode):ControlValue)
         cSystemOwned     =         goCntSearchFilter:getControl("fcSystemOwned":U   + goCntSearchFilter:ContainerCode):ControlValue 
         lSystemOwned     = (cSystemOwned = "yes":U).
                  
      IF NOT Warpspeed:ValidationError THEN
      DO:
        DATASET dsAuthRule:EMPTY-DATASET.
        
        ASSIGN     
          lSuccess  = (IF cRuleCode <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.rule_code":U, cMode, cRuleCode) ELSE TRUE)
                       
          lSuccess  = (IF cRuleType <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.rule_type":U, "=":U, cRuleType) ELSE TRUE)             
                       
          lSuccess  = (IF cRuleDescription <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.rule_description":U, cMode, cRuleDescription) ELSE TRUE)             
                       
          lSuccess  = (IF dInsurerObj <> 0.00 
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.insurer_obj":U, "=":U, dInsurerObj) ELSE TRUE)                          
                       
          lSuccess  = (IF iOptionCode <> 0
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.option_code":U, "=":U, iOptionCode) ELSE TRUE)                                       
                       
          lSuccess  = (IF dEffectiveDate <> ?
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.effective_date":U, ">=":U, dEffectiveDate) ELSE TRUE)                                       
                       
          lSuccess  = (IF dEndDate <> ?
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.end_date":U, "<=":U, dEndDate) ELSE TRUE)                                                    
                    
          lSuccess  = (IF cSystemOwned <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_rule.system_owned":U, "=":U, lSystemOwned) AND 
                            oSearch:SetFilterCriteria("tt_auth_rule.system_owned":U, "=":U, (IF NOT lSystemOwned THEN ? ELSE lSystemOwned))
                       ELSE TRUE)                          
                          
          lSuccess  = oSearch:setCriteria("BatchSize":U,  cRecords)
          lSuccess  = oSearch:setCriteria("Query-Sort":U, "tt_auth_rule.rule_type,tt_auth_rule.rule_code,tt_auth_rule.insurer_obj,tt_auth_rule.option_code,tt_auth_rule.effective_date":U)
          lSuccess  = oSearch:fetchData().     
        
      END. /*IF NOT Warpspeed:ValidationError THEN*/

      IF NOT lEnquiryMode
      THEN
        ASSIGN lSuccess = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_rule:DEFAULT-BUFFER-HANDLE). 
      
      
      /* Get the number of records received from the fetch data */
      FOR EACH tt_auth_rule NO-LOCK: 
        ASSIGN iRowsRendered = iRowsRendered + 1. 
      END. /*FOR EACH tt_auth_rule NO-LOCK: */
      
      FOR EACH tt_deleted NO-LOCK:
        ASSIGN iDeleted = iDeleted + 1.
      END. /*FOR EACH tt_deleted NO-LOCK:*/
      
      /* Set the container title with the number of records */
      ASSIGN
        goCntSearchResults:ViewOnly        = FALSE
        
        cContainerCode                     = goCntSearchResults:ContainerCode                                
  
        oControl                           = goCntSearchResults:getControl("fcRecordAction":U + cContainerCode)
        oControl:ControlQueryField         = "tt_auth_rule.record_action":U
        
        oControl                           = goCntSearchResults:getControl("buEdit":U + cContainerCode)
        oControl:ControlToken              = "Updatable":U
        
        oControl                           = goCntSearchFilter:getControl("fcInsurerArgumentMnemonic":U  + goCntSearchFilter:ContainerCode)
        oControl:ControlValue              = "ermin":U                                                                                     
                                                                                                                                         
        oControl                           = goCntSearchFilter:getControl("fcInsurerArgumentField":U     + goCntSearchFilter:ContainerCode)
        oControl:ControlValue              = "[CodeField]":U                                      
        
        oControl                           = goCntSearchResults:getControl("fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode)
        oControl:ControlValue              = "ermin":U                                                                                     
                                                                                                                                         
        oControl                           = goCntSearchResults:getControl("fcInsurerArgumentField":U    + goCntSearchResults:ContainerCode)
        oControl:ControlValue              = "[CodeField]":U                                      
      
        
        goCntSearchResults:QueryBufferList = STRING(TEMP-TABLE tt_auth_rule:DEFAULT-BUFFER-HANDLE) + ",":U
                                           + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE)
        
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
        DATASET dsAuthRule:EMPTY-DATASET.
        
        IF goWob:CurrentObj <> "":U
        THEN
          ASSIGN 
            lSuccess = oSearch:SetFilterCriteria("tt_auth_rule.auth_rule_obj":U, "=":U, DECIMAL(goWob:CurrentObj))                              
                      
            lSuccess = oSearch:fetchData().              
      END. /*IF NOT Warpspeed:ValidationError THEN*/
      
      
      mipEnv:Health:maUtility:getAuditRecordTT(
         INPUT "hac_auth_rule":U,             
         INPUT goWob:CurrentObj,                                                                              
         INPUT "":U,                                                                      
         INPUT "":U,                                                                                              
         OUTPUT TABLE ttAuditRecord).
     
      
      ASSIGN                                            
        oControl                   = goCntMaint:getControl("fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode)
        oControl:ControlValue      = "ermin":U                                                                                     
                                                                                                                                 
        oControl                   = goCntMaint:getControl("fcInsurerArgumentField":U    + goCntMaint:ContainerCode)
        oControl:ControlValue      = "[CodeField]":U                                      
        
        goCntMaint:QueryString     = SUBSTITUTE(goCntMaint:QueryString, goWob:CurrentObj)
                     
        goCntMaint:QueryBufferList = STRING(TEMP-TABLE tt_auth_rule:DEFAULT-BUFFER-HANDLE)
        
        lSuccess                   = goCntMaint:PopulateFromQuery()
        
        
        goCntAudit:QueryBufferList = STRING(TEMP-TABLE ttAuditRecord:DEFAULT-BUFFER-HANDLE)
        
        lSuccess                   = goCntAudit:PopulateFromQuery().                
        
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
  
  
  DEFINE VARIABLE iControl AS INTEGER            NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE oControl AS cls.mipwscontrol   NO-UNDO.

  RUN SUPER (INPUT ipoContainer).  
  
  CASE ipoContainer:RowRenderArgument:
  
    WHEN ipoContainer:ContainerCode THEN
    DO:
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_rule_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hacar":U,
         INPUT DECIMAL(ipoContainer:getControl("fdRuleObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).  
      
      DO iControl = 1 TO NUM-ENTRIES(ipoContainer:getControlNameList()):
      
        ASSIGN oControl = ipoContainer:getControl(ENTRY(iControl,ipoContainer:getControlNameList())).         
        
        FIND FIRST tt_control NO-LOCK
             WHERE tt_control.control_name = oControl:ControlName
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        IF NOT AVAILABLE tt_control THEN 
        DO:
          CREATE tt_control.
          ASSIGN tt_control.control_name  = oControl:ControlName
                 tt_control.control_token = oControl:ControlToken.
                 
          VALIDATE tt_control.
        END. /*IF NOT AVAILABLE tt_control THEN */
                
        IF LOGICAL(oControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_rule.system_owned":U, "BUFFER-VALUE":U)) AND NOT mipEnv:miUser:DevelopmentUser THEN  
        DO:
          ASSIGN oControl:ControlToken = (IF oControl:ControlToken <> "hidden":U  
                                          THEN "Disabled":U 
                                          ELSE "hidden":U).
        
        END. /* IF NOT mipEnv:miUser:DevelopmentUser */                                        
        ELSE
          ASSIGN oControl:ControlToken = (IF oControl:ControlName BEGINS "flSystemOwned":U AND NOT mipEnv:miUser:DevelopmentUser
                                          THEN "Disabled":U
                                          ELSE tt_control.control_token).
         
      END. /*DO iControl = 1 TO NUM-ENTRIES(ipoContainer())*/                    
    END. /* WHEN ipoContainer:ContainerCode */
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

&IF DEFINED(EXCLUDE-validateTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateTable Procedure 
PROCEDURE validateTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.
  
  CASE goWob:SubmitValue:
  
    WHEN "SearchSubmit":U THEN
    DO:                                                                            
    
    END. /*WHEN "SearchSubmit":U THEN*/
    
  END CASE.
  
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

