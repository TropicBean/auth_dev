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

{ ma/inc/maauthds.i }

DEFINE VARIABLE goWob     AS cls.mipwswob     NO-UNDO.

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
         HEIGHT             = 8.91
         WIDTH              = 59.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveAuthMCSavingsContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveAuthMCSavingsContainer Procedure 
PROCEDURE ajaxSaveAuthMCSavingsContainer :
/*------------------------------------------------------------------------------
  Purpose   : Savings Container Ajax Validation    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE oRequestHelper      AS cls.maajaxrequesthelper       NO-UNDO.
  DEFINE VARIABLE oResponseHelper     AS cls.maajaxresponsehelper      NO-UNDO.
  DEFINE VARIABLE oAuthMCSavings      AS cls.maauthorisationmcsavings  NO-UNDO.
  DEFINE VARIABLE oAuthorisation      AS cls.maauthorisation           NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL                       NO-UNDO.
  DEFINE VARIABLE cContainerCode      AS CHARACTER                     NO-UNDO.
  DEFINE VARIABLE cRecordAction       AS CHARACTER                     NO-UNDO.
  DEFINE VARIABLE dAuthObj            AS DECIMAL                       NO-UNDO.
  DEFINE VARIABLE dAuthMCSavingsObj   AS DECIMAL                       NO-UNDO.
  DEFINE VARIABLE dAmountBase         AS DECIMAL                       NO-UNDO.
  DEFINE VARIABLE dAmountActual       AS DECIMAL                       NO-UNDO.
  DEFINE VARIABLE tSavingsDate        AS DATE                          NO-UNDO.
  DEFINE VARIABLE cSavingStatus       AS CHARACTER                     NO-UNDO.
  DEFINE VARIABLE cSavingStatusNote   AS CHARACTER                     NO-UNDO.
  DEFINE VARIABLE cSavingType         AS CHARACTER                     NO-UNDO.
  DEFINE VARIABLE tSavingDate         AS DATE                          NO-UNDO.
  DEFINE VARIABLE cUserID             AS CHARACTER                     NO-UNDO.
  
  ASSIGN
    oRequestHelper     = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
    oResponseHelper    = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
    
    oAuthMCSavings     = NEW cls.maauthorisationmcsavings()
     
    cContainerCode     = ipcValidationArgument
  
    cRecordAction      =         oRequestHelper:getFieldValue("fcAction":U            + cContainerCode)
    dAuthMCSavingsObj  = DECIMAL(oRequestHelper:getFieldValue("fdAuthMCSavingsObj":U  + cContainerCode))
    dAmountBase        = DECIMAL(oRequestHelper:getFieldValue("fdAmountBase":U        + cContainerCode))
    dAmountActual      = DECIMAL(oRequestHelper:getFieldValue("fdAmountActual":U      + cContainerCode))
    cSavingStatus      =         oRequestHelper:getFieldValue("fcSavingsStatus":U     + cContainerCode)
    cSavingType        =         oRequestHelper:getFieldValue("fcSavingsType":U       + cContainerCode)
    cSavingStatusNote  =         oRequestHelper:getFieldValue("fcSavingsReason":U     + cContainerCode)

    dAuthObj = DECIMAL(oRequestHelper:getFieldValue("_authObjArgument":U + cContainerCode))
   
    lSuccess = mipEnv:Health:AuthService:getAuthObject
                 ( INPUT  dAuthObj,
                   INPUT  "":U,
                   OUTPUT oAuthorisation )
  
    tSavingDate        = (IF oAuthorisation:InFocus AND oRequestHelper:getFieldValue("ftSavingDate":U + cContainerCode) = "":U
                          THEN oAuthorisation:StartDate 
                          ELSE DATE(oRequestHelper:getFieldValue("ftSavingDate":U     + cContainerCode)))
   NO-ERROR.
   
  IF NOT {&ErrorStatus} THEN
  DO:
  
    CASE cRecordAction:
    
      WHEN "MODIFY":U THEN 
      DO:
        oAuthMCSavings:focusRecord(dAuthMCSavingsObj) NO-ERROR.
      
        ASSIGN
          oAuthMCSavings:AuthObj            = dAuthObj
          oAuthMCSavings:AmountActual       = dAmountActual
          oAuthMCSavings:AmountBase         = dAmountBase
          oAuthMCSavings:SavingStatus       = cSavingStatus 
          oAuthMCSavings:SavingType         = cSavingType
          oAuthMCSavings:SavingStatusNote   = cSavingStatusNote
          oAuthMCSavings:SavingDate         = tSavingDate
          oAuthMCSavings:SavingsUserId      = mipEnv:miUser:UserCode
         NO-ERROR.
         
        ASSIGN lSuccess = oAuthMCSavings:saveRecord() NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthMCSavings:ErrorObject:ErrorsExist THEN 
        DO:
          ASSIGN
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U  
            lSuccess                        = oResponseHelper:addFieldValue("fdAuthMCSavingsObj":U + cContainerCode, STRING(oAuthMCSavings:AuthMCSavingsObj))
           NO-ERROR. 
        END. /* IF NOT {&ErrorStatus} AND NOT oAuthMCSavings:ErrorObject:ErrorExist THEN  */
      END. /* WHEN "modify":U THEN  */
      
      WHEN "delete":U THEN 
      DO:
        oAuthMCSavings:focusRecord(dAuthMCSavingsObj) NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthMCSavings:InFocus 
        THEN 
          ASSIGN
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U
           NO-ERROR. 
        ELSE
          ASSIGN lSuccess = oAuthMCSavings:deleteRecord() NO-ERROR.
       
        IF NOT {&ErrorStatus} AND NOT oAuthMCSavings:ErrorObject:ErrorsExist THEN
        DO:
          ASSIGN
            oResponseHelper:RequestValid     = TRUE
            oResponseHelper:ResponseMessage  = "Record successfully removed":U
            oResponseHelper:ReturnValue      = "Record successfully removed":U
           NO-ERROR. 
        END. /* IF NOT {&ErrorStatus} AND NOT oAuthMCSavings:ErrorObject:ErrorExists THEN */
        
      END. /* WHEN "delete":U THEN  */
    
      OTHERWISE
      DO:
        ASSIGN 
          oResponseHelper:RequestValid     = FALSE
          
          oResponseHelper:ReturnValue      = SUBSTITUTE("Request '&1' not supported":U, cRecordAction)
          oResponseHelper:ResponseMessage  = "Unable to perform action":U 
         NO-ERROR.
      END. /* OTHERWISE */
    END CASE.
  END. /* IF NOT {&ErrorStatus} THEN */
  
  IF {&ErrorStatus}  OR oAuthMCSavings:ErrorObject:ErrorsExist THEN 
  DO:
    ASSIGN
      oResponseHelper:RequestValid     = FALSE
      lSuccess                         = oRequestHelper:mapElementsFromQueryFields(oAuthMCSavings:ErrorObject)
      
      oResponseHelper:ResponseMessage  = "Unable to perfom action":U
      oResponseHelper:ReturnValue      = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
  
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }
  END. /* IF {&ErrorStatus}  OR oAuthMCSavings:ErrorObject:ErrorExist THEN  */
  
  ASSIGN lSuccess = oResponseHelper:setError(oAuthMCSavings:ErrorObject).  
    
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthMCSavings)  THEN DELETE OBJECT oAuthMCSavings  NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthDetSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthDetSavings Procedure 
PROCEDURE getCntUpdAuthDetSavings :
/*------------------------------------------------------------------------------
  Purpose   : Update savings container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
     
  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, FALSE)
    opoContainer:ContainerTitle        = "Savings":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth_detail NO-LOCK WHERE tt_auth_detail.auth_detail_obj = '&1'":U                                                                                                                                                                             
    
    oControl                           = opoContainer:addControl("fdCurrentAuth":U  + ipcContainername, "wsInput":U   , "15":U, "tt_auth_detail.adjustment_auth":U        , "DECIMAL":U, 1, 1, "Current Authorised:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl                           = opoContainer:addControl("fdCurrentPaid":U  + ipcContainername, "wsInput":U   , "15":U, "tt_auth_detail.adjustment_paid":U        , "DECIMAL":U, 1, 2, "Current Paid:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl                           = opoContainer:addControl("fdPreviousAuth":U + ipcContainername, "wsInput":U   , "15":U, "tt_auth_detail.adjustment_private_auth":U, "DECIMAL":U, 2, 1, "Previous Authorised:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl                           = opoContainer:addControl("fdPreviousPaid":U + ipcContainername, "wsInput":U   , "15":U, "tt_auth_detail.adjustment_private_paid":U, "DECIMAL":U, 2, 2, "Previous Paid:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    .                              
    
&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthMCSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthMCSavings Procedure 
PROCEDURE getCntUpdAuthMCSavings :
/*------------------------------------------------------------------------------
  Purpose: Update the Managed Care Savings  
  Notes:   'MC' part  = Managed Care. It is not for normal savings 
           but Managed Care Savings      
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oWob                 AS cls.mipwswob              NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  ASSIGN 
    oWob                               = WarpSpeed:CurrentObject
  
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle        = "Managed Care Savings":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:NoDataMessage         = "Please specify the provider information in the empty line provided above":U
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = TRUE
    opoContainer:RowRenderProcedure    = "RowRenderProcedure":U
    opoContainer:RowRenderArgument     = "AuthMCSavingsContainer":U 
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth_mc_savings NO-LOCK":U
                                       + " , FIRST note NO-LOCK OUTER-JOIN":U
                                       + "   WHERE note.type = 'AE' ":U
                                       + "     AND note.key  = tt_auth_mc_savings.saving_status_note":U
                                       + "    BY tt_auth_mc_savings.line_number":U                                                                                                                                                                             
    
    oControl                           = opoContainer:addControl("fiLineNumber":U        + ipcContainername, "wsInput":U       , "25":U , "":U                                         , "INTEGER":U  , 1, "#":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "LineNumber":U                                                                                                                                
    oControl:ControlToken              = "Hidden":U                                                                                                                                  
                                                                                                                                                                                       
    oControl                           = opoContainer:addControl("fdAmountBase":U        + ipcContainername, "wsInput":U        , "15":U, "tt_auth_mc_savings.amount_base":U           , "DECIMAL":U  , 2, "Base Amount":U)
    oControl:ControlClass              = "+clNumericOnly":U                                                                                                                                           
                                                                                                                                                                                                      
    oControl                           = opoContainer:addControl("fdAmountActual":U      + ipcContainername, "wsInput":U        , "15":U, "tt_auth_mc_savings.amount_actual":U         , "DECIMAL":U  , 3, "Actual Amount:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:JavaScriptOnBlur          = "fnCalculateMCSavingsAmount(this,~"fdAmountBase":U    + ipcContainerName + "~", ":U
                                       + "                                ~"fdAmountActual":U  + ipcContainerName + "~", ":U
                                       + "                                ~"fdSavingsAmount":U + ipcContainerName + "~");":U                                                                                                                                      
                                                                                                                                                                                                      
    oControl                           = opoContainer:addControl("fdSavingsAmount":U     + ipcContainername, "wsInput":U        , "15":U, "":U                                         , "DECIMAL":U  , 4, "Savings Amount":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle 
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthMCSavingsAmount":U
                                                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcSavingsType":U       + ipcContainername, "wsCombo":U        , "15":U, "tt_auth_mc_savings.saving_type":U           , "CHARACTER":U  , 5, "Savings Type":U)                                                                                                                           
    oControl:ControlClass              = "+clMan":U
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthSavingsType:=":U
    
    oControl                           = opoContainer:addControl("fcSavingsStatus":U     + ipcContainername, "wsCombo":U        , "15":U, "tt_auth_mc_savings.saving_status":U          , "CHARACTER":U  , 6, "Status":U)
    oControl:ControlClass              = "+clMan":U
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                                                                                               
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthSavingStatus:=":U                                                                                                                                                                                            
                                                                                                                                
    oControl                           = opoContainer:addControl("fcSavingsReason":U     + ipcContainername, "wsInput":U        , "5":U,  "tt_auth_mc_savings.saving_status_note":U    , "CHARACTER":U, 7, "Status Reason":U)
    oControl                           = opoContainer:addControl("fcNoteTypeArgument":U  + ipcContainername, "wsInput":U        , "5":U,  "":U                                         , "CHARACTER":U, 7, "Status Reason":U)
    oControl:ControlToken              = "Hidden":U
    oControl:ControlValue              = "AE":U
    
    oControl                           = opoContainer:addControl("buSavingsReasonBtn":U  + ipcContainername, "wsLookupButton":U , "":U  , "":U                                         , "":U         , 7, "Status Reason":U)
    oControl:LookupWobFLA              = "note":U
    oControl:LookupFields              = "note.key":U
    oControl:LookupControls            = "fcSavingsReason":U + ipcContainername
    oControl:FilterFields              = "note.key,note.type":U 
    oControl:FilterControls            = "fcSavingsReason":U + ipcContainername + ",fcNoteTypeArgument":U + ipcContainername
    oControl:ReturnFields              = "note.key":U
    oControl:ReturnControls            = "fcSavingsReason":U + ipcContainername
    oControl:CellLayoutMask            = "&1&2 &3&4":U
    
    oControl                           = opoContainer:addControl("fcSavingsReasonDesc":U + ipcContainername, "wsSpan":U            , "10":U, "note.narration[1]":U                      , "CHARACTER":U, 7, "Status Reason":U)
    oControl:CellClass                 = "+tooltip":U                                                                                                                         
    oControl:ControlClass              = "+tooltiptext":U  
    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                                   
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "StatusReasonDescToolTip":U 
        
    oControl                           = opoContainer:addControl("ftSavingDate":U        + ipcContainername, "wsInput":U           , "15":U, "tt_auth_mc_savings.saving_date":U         , "DATE":U     , 8, "Saving Date":U)
    oControl:ControlClass              = "+clMan":U
    
    oControl                           = opoContainer:addControl("fcUserIdArgument":U    + ipcContainername, "wsInput":U           , "15":U, "tt_auth_mc_savings.user_id":U             , "CHARACTER":U, 9, "User ID":U)
    oControl:ControlToken              = "Disabled":U
                                                                                                                                                               
    oControl                           = opoContainer:addControl("fdAuthMCSavingsObj":U  + ipcContainername, "wsInput":U           , "15":U, "tt_auth_mc_savings.auth_mc_savings_obj":U , "CHARACTER":U, 10, "Auth Savings Obj":U)
    oControl:ControlToken              = "Hidden":U                                                                             
    oControl                           = opoContainer:addControl("_authObjArgument":U    + ipcContainername, "wsInput":U           , "15":U, "tt_auth_mc_savings.auth_obj":U            , "CHARACTER":U, 11, "Savings Status":U)
    oControl:ControlClass              = "+clObjControl +clHid":U.
    
    
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

&IF DEFINED(EXCLUDE-getCntUpdAuthProvSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthProvSavings Procedure 
PROCEDURE getCntUpdAuthProvSavings :
/*------------------------------------------------------------------------------
  Purpose   : Update savings container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
     
  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, FALSE)
    opoContainer:ContainerTitle        = "Savings":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth_provider NO-LOCK WHERE tt_auth_provider.auth_provider_obj = '&1'":U                                                                                                                                                                             
    
    oControl                           = opoContainer:addControl("fdCurrentAuth":U  + ipcContainername, "wsInput":U   , "15":U, "tt_auth_provider.adjustment_auth":U        , "DECIMAL":U, 1, 1, "Current Authorised:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl                           = opoContainer:addControl("fdCurrentPaid":U  + ipcContainername, "wsInput":U   , "15":U, "tt_auth_provider.adjustment_paid":U        , "DECIMAL":U, 1, 2, "Current Paid:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl                           = opoContainer:addControl("fdPreviousAuth":U + ipcContainername, "wsInput":U   , "15":U, "tt_auth_provider.adjustment_private_auth":U, "DECIMAL":U, 2, 1, "Previous Authorised:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl                           = opoContainer:addControl("fdPreviousPaid":U + ipcContainername, "wsInput":U   , "15":U, "tt_auth_provider.adjustment_private_paid":U, "DECIMAL":U, 2, 2, "Previous Paid:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    .                              
    
&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthSavings Procedure 
PROCEDURE getCntUpdAuthSavings :
/*------------------------------------------------------------------------------
  Purpose   : Update savings container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
     
  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, FALSE)
    opoContainer:ContainerTitle        = "Authorised Savings Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth NO-LOCK":U                                                                                                                                                                             
    
    oControl                           = opoContainer:addControl("fdCurrentAuth":U  + ipcContainername, "wsInput":U   , "15":U, "tt_auth.adjustment_auth":U        , "DECIMAL":U, 1, 1, "Current Authorised:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlTooltip            = "Please enter a valid Current Authorised Amount.":U
    
    oControl                           = opoContainer:addControl("fdCurrentPaid":U  + ipcContainername, "wsInput":U   , "15":U, "tt_auth.adjustment_paid":U        , "DECIMAL":U, 1, 2, "Current Paid:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlTooltip            = "Please enter a valid Current Paid Amount.":U
    
    oControl                           = opoContainer:addControl("fdPreviousAuth":U + ipcContainername, "wsInput":U   , "15":U, "tt_auth.adjustment_private_auth":U, "DECIMAL":U, 2, 1, "Previous Authorised:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlTooltip            = "Please enter a valid Previous Authorised Amount.":U
    
    oControl                           = opoContainer:addControl("fdPreviousPaid":U + ipcContainername, "wsInput":U   , "15":U, "tt_auth.adjustment_private_paid":U, "DECIMAL":U, 2, 2, "Previous Paid:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlTooltip            = "Please enter a valid Previous Paid Amount.":U
    .                              
    
&ENDIF
    
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
  Author    : Andrewd     
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  
  DEFINE VARIABLE dAmountActual        AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dAmountBase          AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cAuthSavingStatus    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hBuffer              AS HANDLE    NO-UNDO.
  DEFINE VARIABLE hQuery               AS HANDLE    NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN  
  
  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).

  CASE ipoControl:RenderArgument:      
  
    WHEN "AuthMCSavingsAmount":U THEN 
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer       = hQuery:GET-BUFFER-HANDLE("tt_auth_mc_savings":U)
          dAmountBase   = hBuffer::amount_base
          dAmountActual = hBuffer::amount_actual.
        
      ASSIGN
        ipoControl:ControlValue = STRING(dAmountBase - dAmountActual)
        ipoControl:ControlToken = "Disabled":U 
        lSuccess                = ipoControl:renderAsInput().
        
    END. /* WHEN "AuthMCSavingsAmount":U THEN  */
    
    WHEN "StatusReasonDescToolTip":U THEN
    DO:    
      IF VALID-HANDLE(hQuery)
      AND hQuery:GET-BUFFER-HANDLE("tt_auth_mc_savings":U)::auth_mc_savings_obj > 0
      THEN 
        ipoControl:RenderAsSpan().                       
    END. /* WHEN "StatusReasonDescToolTip" */
    
    OTHERWISE RUN SUPER(ipoControl).
  END. /*CASE ipoControl:RenderArgument:      */
    
  
  { mip/inc/mipcatcherror.i }                                
                
&ENDIF          
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

