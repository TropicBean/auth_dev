&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Purpose     : Auth History Wob
  Description : Auth History Wob
  Author      : Karlb
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.
/* This helps ensure proper cleanup */

CREATE WIDGET-POOL.

{ mip/inc/mipdefshared.i }
{ ma/inc/maauthds.i }

/*Variables*/
DEFINE VARIABLE gcWOBState              AS CHARACTER          NO-UNDO.
DEFINE VARIABLE gcWOBToken              AS CHARACTER          NO-UNDO.
DEFINE VARIABLE gcRowGroupCode          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE glSuccess               AS LOGICAL            NO-UNDO.
DEFINE VARIABLE glPopupMode             AS LOGICAL            NO-UNDO.
DEFINE VARIABLE gdAuthObj               AS DECIMAL            NO-UNDO.

DEFINE VARIABLE gclsWob                 AS cls.mipwswob       NO-UNDO.
                                        
/* Containers */                        
DEFINE VARIABLE goCntLayout             AS cls.mipwscontainer NO-UNDO.
DEFINE VARIABLE goCntFilter             AS cls.mipwscontainer NO-UNDO.
DEFINE VARIABLE goCntAuthHist           AS cls.mipwscontainer NO-UNDO.
DEFINE VARIABLE goCntAuthProvHist       AS cls.mipwscontainer NO-UNDO.
DEFINE VARIABLE goCntAuthCodingHist     AS cls.mipwscontainer NO-UNDO.
DEFINE VARIABLE goCntAuthDetailHist     AS cls.mipwscontainer NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnLinesInGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnLinesInGroup Procedure 
FUNCTION fnLinesInGroup RETURNS INTEGER
  ( INPUT ipcGroupCode AS CHARACTER )  FORWARD.

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
         HEIGHT             = 18.76
         WIDTH              = 55.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/*------------------------------------------------------------------------------
  This is a WarpSpeed Warpspeed - include ws/inc/wsstructure.i, and nothing else.
------------------------------------------------------------------------------*/
  ASSIGN gclsWob = WarpSpeed:CurrentObject.

  {ws/inc/wsstructure.i &WsShutdownProcedure = "'WobShutdown':U"}

  { mip/inc/mipcatcherror.i }

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OutputCustomHeaderJS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputCustomHeaderJS Procedure 
PROCEDURE OutputCustomHeaderJS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lFirst AS LOGICAL     NO-UNDO.
  
  
  {&OUT}
    { ws/inc/wsjavascriptopentag.i }
    
    /*
      A 'contains' function added to the string object in case the browser being used does not support this function
    */
    "~n  if(!(~"contains~" in String.prototype)) ~{                             ":U
    "~n    String.prototype.contains = function(str, startIndex) ~{             ":U
    "~n      return -1 !== String.prototype.indexOf.call(this, str, startIndex);":U
    "~n    ~};                                                                  ":U
    "~n  ~}                                                                     ":U  
  
    /*
      To expand/collapse the rows in a specific group on the TP detail lines 
    */
    "~n  function fnExpandCollapseGroups(pControl,pGroupNum)~{                                                                      ":U      
    "~n    var vGroup          = ~"clGrp~" + pGroupNum,                                                                             ":U  
    "~n        vGrpSelectArray = document.getElementsByClassName(vGroup);                                                           ":U  
    "~n                                                                                                                             ":U
    "~n    if (pControl.checked) ~{                                                                                                 ":U 
    "~n      wsSetClassName(wsGetParent(pControl,~"TR~"), ~"+clred~");                                                              ":U  
    "~n      for( var i=0; i<vGrpSelectArray.length; i++ ) ~{                                                                       ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"-clHid~");                                                  ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"+clred~");                                                  ":U 
    "~n      ~}                                                                                                                     ":U 
    "~n    ~}                                                                                                                       ":U 
    "~n    else ~{                                                                                                                  ":U 
    "~n      wsSetClassName(wsGetParent(pControl,~"TR~"), ~"-clred~");                                                              ":U 
    "~n      for( var i=0; i<vGrpSelectArray.length; i++ ) ~{                                                                       ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"+clHid~");                                                  ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"-clred~");                                                  ":U 
    "~n      ~}                                                                                                                     ":U 
    "~n    ~}                                                                                                                       ":U
    "~n  ~}                                                                                                                         ":U
    "~n    function fnWarning()~{           "
    "~n    new wsNotification(~{                                          "
		"~n	                          notification: ~"Warning ~" , "
		"~n	                          timeout: ~"?~", "
		"~n	                          type: ~"warning~", "
		"~n	                          position: ~"bottom~" "
    "~n                           "
	  "~n              ~	});       "
    "~n    ~}  "
  
    "~n    function fnWarningAck()~{           "
    "~n    new wsNotification(~{                                          "
		"~n	                          notification: ~"Warning acknowledge~" , "
		"~n	                          timeout: ~"?~", "
		"~n	                          type: ~"warning~", "
		"~n	                          position: ~"bottom~", "
    "~n                           Acknowledge : true  "
	  "~n              ~	});       "
    "~n    ~}  "

    "~n    function fnSuccess()~{           "
    "~n    new wsNotification(~{                                          "
		"~n	                          notification: ~"Success!~" , "
		"~n	                          timeout: ~"?~", "
		"~n	                          type: ~"success~", "
		"~n	                          position: ~"bottom~""
    "~n                            "
	  "~n              ~	});       "
    "~n    ~}  "

    "~n    function fnError()~{           "
    "~n    new wsNotification(~{                                          "
		"~n	                          notification: ~"An error has occured!~" , "
		"~n	                          timeout: ~"?~", "
		"~n	                          type: ~"error~", "
		"~n	                          position: ~"bottom~" "
    "~n                            "
	  "~n              ~	});       "
    "~n    ~}  "
    "~n    function fnErrorHelp()~{           "
    "~n    new wsNotification(~{                                          "
		"~n	                          notification: ~"An error has occured!~" , "
		"~n	                          timeout: ~"?~", "
		"~n	                          type: ~"error~", "
		"~n	                          position: ~"bottom~", "
    "~n                           notificationHelp : ~"[Please refer to 123 for more information]~" "
	  "~n              ~	});       "
    "~n    ~}  "
    "~n    function fnInfo()~{           "
    "~n    new wsNotification(~{                                          "
		"~n	                          notification: ~"Here is some extra information~" , "
		"~n	                          timeout: ~"?~", "
		"~n	                          type: ~"info~", "
		"~n	                          position: ~"bottom~""
    "~n                             "
	  "~n              ~	});       "
    "~n    ~}  "

    
    
    "~n               "
    "~n               "
    "~n               "
    "~n               "
    "~n               "
    "~n               "
    "~n               "
    "~n               "
    "~n               "
    "~n               "
    "~n               "
              
    /*
      This function will handle filtering rows by the filter criteria specified in a container title search input.
    */
    "~n  function fnFilterOnKeyup(pFilterControl,pCompControl)~{                                                      ":U
    "~n    var vFilterControl = fnGetControls(pFilterControl)[0];                                                     ":U
    "~n                                                                                                               ":U
    "~n    if(vFilterControl!=null)~{                                                                                 ":U
    "~n      for(i=1;i<=parseInt(wsGetParent(fnGetControls(pCompControl + ~"1~")[0],~"TABLE~").rows.length,10);i++)~{ ":U
    "~n        vControl=fnGetControls(pCompControl+i)[0];                                                             ":U
    "~n        if(vControl!=null&&vControl!=undefined)~{                                                              ":U
    "~n          if(fnGetControlValue(vControl).toLowerCase().contains(vFilterControl.value.toLowerCase()))~{         ":U                                                     
    "~n            wsGetParent(vControl,~"TR~").style.display=~"table-row~";                                          ":U
    "~n          ~}else~{                                                                                             ":U
    "~n            wsGetParent(vControl,~"TR~").style.display=~"none~";                                               ":U
    "~n          ~}                                                                                                   ":U 
    "~n        ~}                                                                                                     ":U 
    "~n      ~}                                                                                                       ":U 
    "~n    ~}                                                                                                         ":U 
    "~n  ~}                                                                                                           ":U
    
    { ws/inc/wsjavascriptclosetag.i }.
  
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RetrieveData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RetrieveData Procedure 
PROCEDURE RetrieveData :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters:  <none>
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess       AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE cSearchMethod  AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cGroupCode     AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE hBuffer        AS HANDLE                      NO-UNDO.  
  DEFINE VARIABLE oAuthSearch  AS cls.maauthsearch   NO-UNDO.

  ASSIGN 
    oAuthSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE) 
    lSuccess    = oAuthSearch:setfilterCriteria("tt_auth.auth_obj" , "=" , gdAuthObj)
    lSuccess    = oAuthSearch:fetchData().

  {mip/inc/mipcatcherror.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebAcceptRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebAcceptRequest Procedure 
PROCEDURE WebAcceptRequest :
/*------------------------------------------------------------------------------
  Purpose   : This procedure must capture all information from the Web Request.
  Parameters: <none>
  Notes     :
------------------------------------------------------------------------------*/
  
  
  IF Warpspeed:CurrentObj = ?
  THEN ASSIGN Warpspeed:CurrentObj = '':U .
  
  ASSIGN 
    gclsWob:SubmitValue = get-value("frmSubmit":U)
    
    gdAuthObj           = DECIMAL(Warpspeed:CurrentObj)
    
    glPopupMode         = get-value("popupMode":U) = "true":U
    
    gclsWob:Mode        = "Enquiry":U.  
  
    
  IF gclsWob:SubmitValue = "Close":U
  THEN 
    RETURN "[Redirect]-":U + Warpspeed:CallingWob + "-":U + STRING(Warpspeed:CurrentObj).
    
  
  RUN WebFormDefinition IN TARGET-PROCEDURE.
  
  
  IF CAN-DO("Search":U, WarpSpeed:SubmitValue) 
  THEN goCntFilter:populateFromRequest("":U, 1).
  
  { mip/inc/mipcatcherror.i }  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebBusinessLogic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebBusinessLogic Procedure 
PROCEDURE WebBusinessLogic :
/*------------------------------------------------------------------------------
  Purpose: This procedure should contain all business logic and DB updates, but
           in the ideal implementation they would be in the App's Libraries.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess   AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cWhatToDo  AS CHARACTER  NO-UNDO.
  
  
  CASE gclsWob:Mode:
 /* -------------------------------------------------------------------------
       Business Logic for "Search" State
    ------------------------------------------------------------------------- */
    WHEN "Search":U THEN
    DO:
      
    END. /* WHEN "Search" */
    /* -- END Business Logic for "Search" State ----------------------------- */

    /* -------------------------------------------------------------------------
       Business Logic for "Maint" State
    ------------------------------------------------------------------------- */
    WHEN "Maint":U THEN
    DO:
      
    END. /* WHEN "Maint" */
    /* -- END Business Logic for "Maint" State ------------------------------ */

  END CASE. /* CASE gclsWob:Mode */                       
  
  { mip/inc/mipcatcherror.i }    
  
END PROCEDURE.
/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebCreateResponse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebCreateResponse Procedure 
PROCEDURE WebCreateResponse :
/*------------------------------------------------------------------------------
  Purpose: Get the Containers ready for rendering.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess      AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE iControl      AS INTEGER            NO-UNDO.
  DEFINE VARIABLE oControl      AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oSubContainer AS cls.mipwscontainer NO-UNDO.
  DEFINE VARIABLE oAppObject    AS cls.mipappobject   NO-UNDO.
                                              
  RUN RetrieveData IN TARGET-PROCEDURE.
  
  CASE gclsWob:Mode:
    /* -------------------------------------------------------------------------
       Preparations for "Search" State
    ------------------------------------------------------------------------- */
    WHEN "Search":U THEN
    DO:            
      
    END. /* WHEN "Search" */
    /* -- END Preparations for "Search" State ------------------------------- */

    /* -------------------------------------------------------------------------
       Preparations for "Maint" State
    ------------------------------------------------------------------------- */
    WHEN "Maint":U THEN
    DO:
    
    END. /*WHEN "Maint":U THEN*/        
    
    WHEN "Enquiry":U THEN
    DO:
      ASSIGN oControl = goCntFilter:getControl("frmButtonBar":U):SubContainer:getControl("btnBack":U).
      
      /*
        If this wob was not run as popup then we will need to five the option to return
        to the calling wob.
      */
      IF Warpspeed:CallingWob <> "":U AND Warpspeed:CallingWob <> gclsWob:ObjectCode THEN 
      DO:
        ASSIGN 
           oAppObject = NEW cls.mipappobject()
           lSuccess   = oAppObject:miFocusAppObject(Warpspeed:CallingWob).
           
        IF glPopupMode THEN
        DO:
          ASSIGN 
             oControl:ControlSubType    = "Button":U
             oControl:ButtonLabel       = "Close":U
             oControl:JavascriptOnClick = "window.opener.document.getElementById(~"divdlg~").style.display = ~"none~";window.close();":U.
        END. /*IF glPopupMode THEN*/
        ELSE
        DO:
          ASSIGN 
             oControl:ButtonLabel = "Return ":U 
                                  + (IF oAppObject:ObjectInFocus THEN " To ":U + oAppObject:ObjectDescription ELSE "":U)
                                  
             oControl:Wob         = Warpspeed:CallingWob.   
        END. /*ELSE*/                   
      END. /*IF Warpspeed:CallingWob <> "":U AND Warpspeed:CallingWob <> gclsWob:ObjectCode THEN */
      ELSE
        ASSIGN oControl:ControlToken = "Hidden":U.                               
      
      oSubContainer = goCntFilter:getControl("frmButtonBar":U):SubContainer.
      
      
      DO iControl = 1 TO NUM-ENTRIES(oSubContainer:getControlNameList()):
      
        ASSIGN 
           oControl            = oSubContainer:getControl(ENTRY(iControl, oSubContainer:getControlNameList()))
           oControl:AssignList = oControl:AssignList 
                               + "&popupMode=":U + get-value("popupMode":U). 
        
      END. /*DO iControl = 1 TO NUM-ENTRIES()*/

      ASSIGN goCntAuthHist:QueryBufferList       = STRING(TEMP-TABLE tt_auth_history:DEFAULT-BUFFER-HANDLE) 
             lSuccess                            = goCntAuthHist:PopulateFromQuery() 
             
             goCntAuthProvHist:QueryBufferList   = STRING(TEMP-TABLE tt_auth_provider_history:DEFAULT-BUFFER-HANDLE) 
             lSuccess                            = goCntAuthProvHist:PopulateFromQuery() 
             
             goCntAuthCodingHist:QueryBufferList = STRING(TEMP-TABLE tt_auth_coding_history:DEFAULT-BUFFER-HANDLE) 
             lSuccess                            = goCntAuthCodingHist:PopulateFromQuery() 
             
             goCntAuthDetailHist:QueryBufferList = STRING(TEMP-TABLE tt_auth_detail_history:DEFAULT-BUFFER-HANDLE) 
             lSuccess                            = goCntAuthDetailHist:PopulateFromQuery() 

             .
      

    END. /*WHEN "Enquiry":U THEN*/
  END CASE. /* gclsWob:Mode */

  { mip/inc/mipcatcherror.i 
    &FINALLY="IF VALID-OBJECT(oAppObject) THEN DELETE OBJECT oAppObject."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebDeliverResponse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebDeliverResponse Procedure 
PROCEDURE WebDeliverResponse :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:  <none>
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess   AS LOGICAL  NO-UNDO.
  DEFINE VARIABLE iTotalRows AS INTEGER  NO-UNDO.
  
  
  output-content-type("text/html":U).

  
  IF glPopupMode AND Warpspeed:SubmitValue = "":U THEN
  DO:
    {&OUT}
      { ws/inc/wsjavascriptopentag.i }
      
      " window.opener.document.getElementById(~"divdlg~").style.display = ~"block~"; ":U
      " window.moveTo(50,50);":U
      " window.resizeTo(screen.width - 100, screen.height - 100);":U
      
      { ws/inc/wsjavascriptclosetag.i }
      .
  END. /*IF glPopupMode THEN*/
  
  ASSIGN
      lSuccess = wsUiService:HTMLHead("", "":U, "":U)
      lSuccess = wsUiService:HTMLForm("OPEN":U, "frmMain").
  
  ASSIGN
     /* We MUST Render the Main Layout. It will run the WebRenderProcedure Procedure to Render our other Containers */
     lSuccess = goCntLayout:renderContainer("":U, "FORM":U, FALSE).    
  
  ASSIGN
      lSuccess = wsUiService:HTMLForm("CLOSE":U, ?)
      lSuccess = wsUiService:HTMLTail().
  
  {&OUT}
    { ws/inc/wsjavascriptopentag.i }
    
    "$(window).unload(function ()~{":U
    "  window.opener.document.getElementById(~"divdlg~").style.display = ~"none~";":U
    "~});":U
    
    { ws/inc/wsjavascriptclosetag.i }.
    
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition Procedure 
PROCEDURE WebFormDefinition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
       
  wsUiService:getLayoutInstance(INPUT "WobLayout":U, INPUT "wsMain":U, OUTPUT goCntLayout). //wsMainNoMenu
                 
  RUN WebFormDefinition_Filter    IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Auth      IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Provider  IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Coding    IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Detail    IN TARGET-PROCEDURE.
  RUN WebFormDefinition_MCSavings IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Crosswalk IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Copay     IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Limit     IN TARGET-PROCEDURE.
  RUN WebFormDefinition_Episode   IN TARGET-PROCEDURE.


                              
  
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Auth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Auth Procedure 
PROCEDURE WebFormDefinition_Auth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl        AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iControl        AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cContainerCode  AS CHARACTER        NO-UNDO.
  
  ASSIGN 
    cContainerCode                      = "AuthHist":U
    goCntAuthHist                       = NEW cls.mipwscontainer(cContainerCode, "100%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntAuthHist:ContainerTitle        = "Authorisation History":U
    goCntAuthHist:ViewOnly              = FALSE
    goCntAuthHist:ShowContainerSettings = FALSE
    goCntAuthHist:Collapsable           = FALSE
    goCntAuthHist:RowsToRender          = ?
    goCntAuthHist:ContainerMode         = Warpspeed:SubmitValue
    goCntAuthHist:QueryString           = "FOR EACH tt_auth_history NO-LOCK":U
                                   //  + "   WHERE tt_auth_history.auth_obj = '&1',":U
  
    oControl                            = goCntAuthHist:addControl("fc_action":U           + cContainerCode , "wsInput":U    , "10":U, "tt_auth_history.action":U                   , "CHARACTER":U, 1, "Action":U                  )   
    oControl                            = goCntAuthHist:addControl("fc_change_date_time":U + cContainerCode , "wsInput":U    , "10":U, "tt_auth_history.change_date_time":U         , "DATETIME":U , 2, "Change Date Time":U        )        
    oControl                            = goCntAuthHist:addControl("fc_change_usr_id":U    + cContainerCode , "wsInput":U    , "10":U, "tt_auth_history.change_usr_id":U            , "CHARACTER":U, 3, "Change User Id":U          )                                                                  
    oControl                            = goCntAuthHist:addControl("fcAuthTypeCode":U      + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.auth_type_obj":U            , "CHARACTER":U, 6, "Auth Type":U               )    
    oControl                            = goCntAuthHist:addControl("fdStartDate":U         + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.start_date":U               , "DATE":U     , 7, "Authorisation Start Date":U)
    oControl                            = goCntAuthHist:addControl("fdEndDate":U           + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.end_date":U                 , "DATE":U     , 8, "Authorisation End Date":U  )     
    oControl                            = goCntAuthHist:addControl("fiClaimCode":U         + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.claim_code":U               , "INTEGER":U  , 9, "Claim Code":U              )
    oControl                            = goCntAuthHist:addControl("cbClaimType":U         + cContainerCode, "wsInput":U     , "3":U , "tt_auth_history.claim_type":U               , "CHARACTER":U, 10, "Claim Type":U             )      
    oControl:AdditionalItems            = "=|C=C|N=N|A=A|K=K|P=P|O=O":U  
    oControl                            = goCntAuthHist:addControl("cbStatusDisp":U        + cContainerCode, "wsCombo":U     , "15":U, "tt_auth_history.auth_status":U              , "INTEGER":U  , 11, "Authorisation Status":U   )
    oControl                            = goCntAuthHist:addControl("fcReason":U            + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.auth_status_note":U         , "CHARACTER":U, 12, "Status Reason":U          ) 
    oControl                            = goCntAuthHist:addControl("fcAction":U            + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.record_action":U            , "CHARACTER":U, 13, "Action":U                 ) .            
    
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Coding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Coding Procedure 
PROCEDURE WebFormDefinition_Provider :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iControl       AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.
 
  ASSIGN 
    cContainerCode                             = "AuthProvHist":U
    goCntAuthProvHist                          = NEW cls.mipwscontainer(cContainerCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntAuthProvHist:ContainerTitle           = "Provider History":U
    goCntAuthProvHist:RowsToRender             = ?
    goCntAuthProvHist:ViewOnly                 = FALSE
    goCntAuthProvHist:ShowContainerSettings    = FALSE
    goCntAuthProvHist:Collapsable              = TRUE
    goCntAuthProvHist:ContainerMode            = Warpspeed:SubmitValue
    goCntAuthProvHist:QueryString              = "FOR EACH tt_auth_provider_history NO-LOCK ":U
                                               + "BY tt_auth_provider_history.auth_provider_obj"
  
    oControl                              = goCntAuthProvHist:addControl("fc_action":U                         + cContainerCode , "wsInput":U      , "10":U , "tt_auth_provider_history.action":U                 , "CHARACTER":U, 1 , "Action":U                )   
    oControl                              = goCntAuthProvHist:addControl("fc_change_date_time":U               + cContainerCode , "wsInput":U      , "10":U , "tt_auth_provider_history.change_date_time":U       , "DATETIME":U , 2 , "Change Date Time":U      )        
    oControl                              = goCntAuthProvHist:addControl("fc_change_usr_id":U                  + cContainerCode , "wsInput":U      , "10":U , "tt_auth_provider_history.change_usr_id":U          , "CHARACTER":U, 3 , "Change User Id":U        )  
    oControl                              = goCntAuthProvHist:addControl("cbSequence":U                        + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.provider_sequence":U      , "CHARACTER":U, 4,  "Seq":U)                                                                                                                    
    oControl                              = goCntAuthProvHist:addControl("flAuthorised":U                      + cContainerCode, "wsCheckBox":U    , "20":U , "tt_auth_provider_history.authorised_service":U     , "LOGICAL":U ,  6,  "A/S":U)                                                                                                                                        
    oControl                              = goCntAuthProvHist:addControl("cbProviderType":U                    + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.provider_type":U          , "CHARACTER":U, 7,  "Provider Type":U)                                                                                                                        
    oControl                              = goCntAuthProvHist:addControl("fiProviderNum":U                     + cContainerCode, "wsInput":U       , "8":U  , "tt_auth_provider_history.doc_num":U                , "CHARACTER":U, 8,  "Provider Number":U)
    oControl                              = goCntAuthProvHist:addControl("fdAuthGroupObj":U                    + cContainerCode, "wsCombo":U       , "10":U , "tt_auth_provider_history.auth_group_obj":U         , "CHARACTER":U, 10, "Auth Group":U)                                                                                                                                                                                                   
    oControl                              = goCntAuthProvHist:addControl("fiDiscipline":U                      + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.pr_type":U                , "CHARACTER":U, 11, "Disc":U)
    oControl                              = goCntAuthProvHist:addControl("fiSubDiscipline":U                   + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.sub_pr_type":U            , "CHARACTER":U, 12, "Sub-Dis":U)                                     
    oControl                              = goCntAuthProvHist:addControl("fcProviderBaseRate":U                + cContainerCode, "wsInput":U       , "18":U , "tt_auth_provider_history.default_base_rate":U      , "CHARACTER":U, 13, "Default<br>Base<br>Rate":U)
    oControl                              = goCntAuthProvHist:addControl("fcProviderArsRate":U                 + cContainerCode, "wsInput":U       , "18":U , "tt_auth_provider_history.default_ars_rate":U       , "CHARACTER":U, 14, "Default<br>Ars<br>Rate":U)
    oControl                              = goCntAuthProvHist:addControl("cbOverrideBaseRate":U                + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.override_base_rate":U     , "CHARACTER":U, 15, "Override<br>Base<br>Rate":U)
    oControl                              = goCntAuthProvHist:addControl("cbOverrideArsRate":U                 + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.override_ars_rate":U      , "CHARACTER":U, 16, "Override<br>Ars<br>Rate":U)                                                                                               
    oControl                              = goCntAuthProvHist:addControl("flMainProvider":U                    + cContainerCode, "wsCheckBox":U    , "18":U , "tt_auth_provider_history.main_provider":U          , "LOGICAL":U  , 17, "Main":U)
    oControl                              = goCntAuthProvHist:addControl("flAuthoriseAllServices":U            + cContainerCode, "wsCheckBox":U    , "18":U , "tt_auth_provider_history.authorise_all_services":U , "LOGICAL":U  , 19, "Authorise<br>All<br>Services":U)
    oControl                              = goCntAuthProvHist:addControl("fiAttProviderNum":U                  + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.group_doc_num":U          , "CHARACTER":U, 20, "Associated Provider":U)
    oControl                              = goCntAuthProvHist:addControl("fdStartDate":U                       + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.start_date":U             , "DATE":U     , 24, "":U)
    oControl                              = goCntAuthProvHist:addControl("cbStartAmPm":U                       + cContainerCode, "wsCombo":U       , "3":U  , "tt_auth_provider_history.start_ampm":U             , "LOGICAL":U  , 24, "Start Date":U)
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
    oControl:CellLayoutMask               = "&1 &2":U    
    oControl                              = goCntAuthProvHist:addControl("fdEndDate":U                         + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.end_date":U               , "DATE":U     , 25, "End Date":U)
    oControl                              = goCntAuthProvHist:addControl("cbEndAmPm":U                         + cContainerCode, "wsCombo":U       , "3":U  , "tt_auth_provider_history.end_ampm":U               , "LOGICAL":U  , 25, "End Date":U)
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
    oControl:CellLayoutMask               = "&1 &2":U    
    oControl                              = goCntAuthProvHist:addControl("fiClaimCode":U                       + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.claim_code":U             , "INTEGER":U  , 26, "Claim<br>Code":U)
    oControl                              = goCntAuthProvHist:addControl("cbClaimType":U                       + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.claim_type":U             , "CHARACTER":U, 27, "Claim<br>Type":U)                                                                       
    oControl                              = goCntAuthProvHist:addControl("cbStatus":U                          + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.auth_status":U            , "INTEGER":U  , 28, "Auth Status":U)
    oControl                              = goCntAuthProvHist:addControl("fcReason":U                          + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.auth_status_note":U       , "CHARACTER":U, 29, "Status<br>Reason":U)
    oControl                              = goCntAuthProvHist:addControl("fcCopayOverrideNote":U               + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.copay_override_note":U    , "INTEGER":U  , 30, "Co-payment<br>Override<br> Reason":U)   
    oControl                              = goCntAuthProvHist:addControl("fcAccountReference":U                + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.account_reference":U      , "CHARACTER":U, 33, "Account<br>Reference":U)
    oControl                              = goCntAuthProvHist:addControl("fdQuantityAuth":U                    + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.quantity_auth":U          , "DECIMAL":U  , 34, "Quantity<br>Auth":U)
    oControl                              = goCntAuthProvHist:addControl("fdAmountAuth":U                      + cContainerCode, "wsInput":U       , "12":U , "tt_auth_provider_history.amount_auth":U            , "DECIMAL":U  , 35, "Amount<br>Auth":U)
    oControl                              = goCntAuthProvHist:addControl("fdAmountReq":U                       + cContainerCode, "wsInput":U       , "12":U , "tt_auth_provider_history.amount_requested":U       , "DECIMAL":U  , 36, "Amount<br>Request":U)
    oControl                              = goCntAuthProvHist:addControl("fdQtyReq":U                          + cContainerCode, "wsInput":U       , "12":U , "tt_auth_provider_history.quantity_requested":U     , "DECIMAL":U  , 37, "Qty<br>Request":U)
    oControl                              = goCntAuthProvHist:addControl("flPmbIndicator":U                    + cContainerCode, "wsCheckBox":U    , "12":U , "tt_auth_provider_history.pmb_indicator":U          , "LOGICAL":U  , 40, "PMB<br>Indicator":U)
    oControl                              = goCntAuthProvHist:addControl("flLosCalculation":U                  + cContainerCode, "wsCheckBox":U    , "12":U , "tt_auth_provider_history.los_calculation":U        , "LOGICAL":U  , 41, "System<br>LOS<br>Calculation":U)
                                                                                                                                   
    .          

  { mip/inc/mipcatcherror.i }  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-WebFormDefinition_Coding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Coding Procedure 
PROCEDURE WebFormDefinition_Coding :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
DEFINE VARIABLE iControl       AS INTEGER          NO-UNDO.
DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.

ASSIGN 
  cContainerCode                               = "AuthCodingHist":U
  goCntAuthCodingHist                          = NEW cls.mipwscontainer(cContainerCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
  goCntAuthCodingHist:ContainerTitle           = "Coding History":U
  goCntAuthCodingHist:RowsToRender             = ?
  goCntAuthCodingHist:ViewOnly                 = FALSE
  goCntAuthCodingHist:ShowContainerSettings    = FALSE
  goCntAuthCodingHist:Collapsable              = TRUE
  goCntAuthCodingHist:ContainerMode            = Warpspeed:SubmitValue
  goCntAuthCodingHist:QueryString              = "FOR EACH tt_auth_coding_history NO-LOCK ":U
                                               + "BY tt_auth_coding_history.auth_coding_obj"

  // Controls 
  oControl = goCntAuthCodingHist:addControl("fc_action":U                    + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.action":U                   , "CHARACTER":U ,  2 , "Record Action":U           )  
  oControl = goCntAuthCodingHist:addControl("fc_change_date_time":U          + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.change_date_time":U         , "DATETIME":U  ,  3 , "Change Datetime":U         )   
  oControl = goCntAuthCodingHist:addControl("fc_change_usr_id":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.change_usr_id":U            , "CHARACTER":U ,  4 , "Change User":U             )        
  oControl = goCntAuthCodingHist:addControl("fc_line_number":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.line_number":U              , "INTEGER":U   ,  5 , "Line Number":U             )  
  oControl = goCntAuthCodingHist:addControl("fc_ass_diag_obj":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.ass_diag_obj":U             , "DECIMAL":U   ,  6 , "Associated diagnosis Obj":U)  
  oControl = goCntAuthCodingHist:addControl("fc_coding_status":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.coding_status":U            , "INTEGER":U   , 12 , "Status":U                  )    
  oControl = goCntAuthCodingHist:addControl("fc_coding_status_note":U        + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.coding_status_note":U       , "CHARACTER":U , 13 , "Reason":U                  )    
  oControl = goCntAuthCodingHist:addControl("fc_coding_type":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.coding_type":U              , "CHARACTER":U , 14 , "Coding Type":U             )  
  oControl = goCntAuthCodingHist:addControl("fc_end_date":U                  + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.end_date":U                 , "DATE":U      , 15 , "End Date":U                )  
  oControl = goCntAuthCodingHist:addControl("fc_morph_diag_obj":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.morph_diag_obj":U           , "DECIMAL":U   , 16 , "Morphology":U              )    
  oControl = goCntAuthCodingHist:addControl("fc_owning_alt_value":U          + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_alt_value":U         , "CHARACTER":U , 17 , "Code":U                    )  
  oControl = goCntAuthCodingHist:addControl("fc_owning_entity_mnemonic":U    + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_entity_mnemonic":U   , "CHARACTER":U , 18 , "Owning Entity":U           )    
  oControl = goCntAuthCodingHist:addControl("fc_owning_key":U                + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_key":U               , "CHARACTER":U , 19 , "Owning Key":U              )  
  oControl = goCntAuthCodingHist:addControl("fc_owning_obj":U                + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_obj":U               , "DECIMAL":U   , 20 , "Owning Obj":U              )  
  oControl = goCntAuthCodingHist:addControl("fc_pmb_indicator":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.pmb_indicator":U            , "LOGICAL":U   , 21 , "PMB":U                     )   
  oControl = goCntAuthCodingHist:addControl("fc_primary_code":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.primary_code":U             , "LOGICAL":U   , 22 , "Primary":U                 )    
  oControl = goCntAuthCodingHist:addControl("fc_related_alt_value":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_alt_value":U        , "CHARACTER":U , 23 , "Related Code":U            )    
  oControl = goCntAuthCodingHist:addControl("fc_related_entity_mnemonic":U   + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_entity_mnemonic":U  , "CHARACTER":U , 24 , "Related Entity":U          )      
  oControl = goCntAuthCodingHist:addControl("fc_related_key":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_key":U              , "CHARACTER":U , 25 , "Related Key":U             )  
  oControl = goCntAuthCodingHist:addControl("fc_related_obj":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_obj":U              , "DECIMAL":U   , 26 , "Related Obj":U             )  
  oControl = goCntAuthCodingHist:addControl("fc_sequence":U                  + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.sequence":U                 , "INTEGER":U   , 27 , "Seq":U                     )    
  oControl = goCntAuthCodingHist:addControl("fc_start_date ":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.start_date":U               , "CHARACTER":U , 28 , "Start Date ":U             )    
  oControl = goCntAuthCodingHist:addControl("fc_body_region":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.body_region":U              , "CHARACTER":U , 30 , "Body Region":U             )  
  .  

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Crosswalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Crosswalk Procedure 
PROCEDURE WebFormDefinition_Crosswalk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Detail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Detail Procedure 
PROCEDURE WebFormDefinition_Detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
DEFINE VARIABLE iControl       AS INTEGER          NO-UNDO.
DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.

ASSIGN 
  cContainerCode                               = "AuthDetailHist":U
  goCntAuthDetailHist                          = NEW cls.mipwscontainer(cContainerCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
  goCntAuthDetailHist:ContainerTitle           = "Detail History":U
  goCntAuthDetailHist:RowsToRender             = ?
  goCntAuthDetailHist:ViewOnly                 = FALSE
  goCntAuthDetailHist:ShowContainerSettings    = FALSE         
  goCntAuthDetailHist:Collapsable              = TRUE
  goCntAuthDetailHist:ContainerMode            = Warpspeed:SubmitValue
  goCntAuthDetailHist:QueryString              = "FOR EACH tt_auth_detail_history NO-LOCK ":U
                                               + "BY tt_auth_detail_history.auth_detail_obj"
 
  oControl                  = goCntAuthDetailHist:addControl("fc_record_action":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.record_action":U          , "CHARACTER":U,  2 , "Record Action":U         )  
  oControl                  = goCntAuthDetailHist:addControl("fc_action":U                   + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.action":U                 , "CHARACTER":U,  3 , "Action":U                )   
  oControl                  = goCntAuthDetailHist:addControl("fc_change_date_time":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.change_date_time":U       , "DATETIME":U ,  4 , "Change Date Time":U      )        
  oControl                  = goCntAuthDetailHist:addControl("fc_change_usr_id":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.change_usr_id":U          , "CHARACTER":U,  5 , "Change User Id":U        )  
  oControl                  = goCntAuthDetailHist:addControl("fc_auth_provider_obj":U        + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.auth_provider_obj":U      , "DECIMAL":U  ,  6 , "Auth Provider Obj":U     )  
  oControl                  = goCntAuthDetailHist:addControl("fc_owning_alt_value":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_alt_value":U       , "CHARACTER":U, 12 , "Owning Alt Value":U      )    
  oControl                  = goCntAuthDetailHist:addControl("fc_owning_entity_mnemonic":U   + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_entity_mnemonic":U , "CHARACTER":U, 13 , "Owning Entity Mnemonic":U)    
  oControl                  = goCntAuthDetailHist:addControl("fc_owning_key":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_key":U             , "CHARACTER":U, 14 , "Owning Key":U            )  
  oControl                  = goCntAuthDetailHist:addControl("fc_owning_obj":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_obj":U             , "DECIMAL":U  , 15 , "Owning Obj":U            )  
  oControl                  = goCntAuthDetailHist:addControl("fc_loc_sequence":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.loc_sequence":U           , "INTEGER":U  , 16 , "LOC Sequence":U          )    
  oControl                  = goCntAuthDetailHist:addControl("fc_quantity_los":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.quantity_los":U           , "DECIMAL":U  , 17 , "Quantity LOS":U          )  
  oControl                  = goCntAuthDetailHist:addControl("fc_minutes_auth":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.minutes_auth":U           , "INTEGER":U  , 18 , "Minutes Auth":U          )    
  oControl                  = goCntAuthDetailHist:addControl("fc_start_date":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.start_date":U             , "DATE":U     , 20 , "Start Date":U            )  
  oControl                  = goCntAuthDetailHist:addControl("fc_start_ampm":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.start_ampm":U             , "LOGICAL":U  , 20 , "Start Date":U            ) 
  oControl:AdditionalItems  = "AM=AM|PM=PM":U                                                                                                               
  oControl:CellLayoutMask   = "&1 &2":U    
  oControl                  = goCntAuthDetailHist:addControl("fc_end_date":U                 + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.end_date":U               , "DATE":U     , 21 , "End Date":U              )   
  oControl                  = goCntAuthDetailHist:addControl("fc_end_ampm":U                 + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.end_ampm":U               , "LOGICAL":U  , 21 , "End Date":U              )    
  oControl:AdditionalItems  = "AM=AM|PM=PM":U                                                                                                               
  oControl:CellLayoutMask   = "&1 &2":U   
  oControl                  = goCntAuthDetailHist:addControl("fc_fixed_item_cost":U          + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.fixed_item_cost":U        , "DECIMAL":U  , 23 , "Fixed Item Cost":U       )    
  oControl                  = goCntAuthDetailHist:addControl("fc_quantity_auth":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.quantity_auth":U          , "DECIMAL":U  , 24 , "Qauntity Auth":U         )      
  oControl                  = goCntAuthDetailHist:addControl("fc_amount_paid":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.amount_paid":U            , "DECIMAL":U  , 25 , "Amount Paid":U           )  
  oControl                  = goCntAuthDetailHist:addControl("fc_claim_code":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.claim_code":U             , "INTEGER":U  , 26 , "Claim Code":U            )  
  oControl                  = goCntAuthDetailHist:addControl("fc_claim_type":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.claim_type":U             , "CHARACTER":U, 27 , "Claim Type":U            )    
  oControl                  = goCntAuthDetailHist:addControl("fc_line_restriction":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.line_restriction":U       , "CHARACTER":U, 28 , "Line Restriction":U      )    
  oControl                  = goCntAuthDetailHist:addControl("fc_auth_status":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.auth_status":U            , "INTEGER":U  , 29 , "Auth Status":U           )    
  oControl                  = goCntAuthDetailHist:addControl("fc_auth_status_note":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.auth_status_note":U       , "CHARACTER":U, 30 , "Auth Status Note":U      )    

  .                                        

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Episode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Episode Procedure 
PROCEDURE WebFormDefinition_Episode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-WebFormDefinition_MCSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_MCSavings Procedure 
PROCEDURE WebFormDefinition_MCSavings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-WebFormDefinition_Copay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Copay Procedure 
PROCEDURE WebFormDefinition_Copay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE iControl AS INTEGER          NO-UNDO.

       
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Limit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Limit Procedure 
PROCEDURE WebFormDefinition_Limit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE iControl AS INTEGER          NO-UNDO.

       
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Filter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Filter Procedure 
PROCEDURE WebFormDefinition_Filter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oButton  AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
  ASSIGN
    goCntFilter                = NEW cls.mipwscontainer(gclsWob:ObjectCode + "Filter":U, "65%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntFilter:ContainerMode  = gclsWob:SubmitValue
    goCntFilter:ViewOnly       = FALSE
    goCntFilter:Collapsable    = FALSE
                               
    lSuccess                   = gclsWob:setItem("Container:Filter":U, STRING(goCntFilter))
    
    oControl                   = goCntFilter:addControl("fcFilterUserCode":U  , "wsInput":U       , "20":U, "":U, "CHARACTER":U, 1, 1, "User:":U)
    oControl:AjaxValidation    = "SERVICE:wsUIService:ajaxValidation:mimus|user_code":U
    oControl:FilterFields      = "[ArgumentFieldValue]":U

    oControl                   = goCntFilter:addControl("buFilterUserButton":U, "wsLookupButton":U, "":U  , "":U, "":U         , 1, 1)
    oControl:CellLayoutMask    = "&1 &2":U
    oControl:LookupWobFLA      = "mimus":U                                                                                     
    oControl:LookupFields      = "mim_user.user_code":U                                                     
    oControl:LookupControls    = "fcFilterUserCode"                                                                       
                                                                                                                               
    oControl                   = goCntFilter:addControl("fdFilterFromDate":U  , "wsInput":U       , "15":U, "":U, "DATE":U     , 2, 1, "Changed From Date:":U) 
    oControl                   = goCntFilter:addControl("fdFilterToDate":U    , "wsInput":U       , "15":U, "":U, "DATE":U     , 3, 1, "Changed To Date:":U) 
    oControl                   = goCntFilter:addControl("frmButtonBar":U      , "":U              , "":U  , "":U, "":U         , 4, 1)
    oControl:SpanOverLabel     = TRUE
    oControl:CellClass         = WarpSpeed:BaseClass + "ButtonBar":U
    oControl:CellSnippet       = "align='right'":U
    oControl:ColumnSpan        = 2
    oControl:SubContainer      = wsUiService:getButtonContainer(gclsWob:ObjectCode + gclsWob:Mode + "BtnBar":U, "Back:Search":U)
    .
       
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_TP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_TP Procedure 
PROCEDURE WebFormDefinition_TP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
       
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormValidation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormValidation Procedure 
PROCEDURE WebFormValidation :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters:  <none>
  Notes     :       
------------------------------------------------------------------------------*/

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebRenderProcedure Procedure 
PROCEDURE WebRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.

  DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cGroupCode  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBufferName AS CHARACTER NO-UNDO.
                                                             
  CASE ipoControl:RenderArgument:

    /* ---------------------------------------------------------------------- */
    /* The WebRenderProcedure is the main Rendering Procedure for this WOB    */
    WHEN "WebRenderProcedure":U THEN 
    DO:
    
      {&OUT}
      "<center>":U.
      
      CASE gclsWob:Mode:
        /* -- Output for Search State ------------------------------------------- */
        WHEN "Search":U THEN
        DO:
          
        END. /*WHEN "Search":U THEN*/

        /* -- Output for Maint State -------------------------------------------- */
        
        WHEN "Maint":U THEN
        DO:
          
        END. /*WHEN "Maint":U THEN*/
        
        WHEN "Enquiry":U THEN
        DO:
          goCntFilter:renderContainer("":U, "FORM":U, FALSE). 
          
          goCntAuthHist:renderContainer      ("":U , "TABLE":U , TRUE) .
          goCntAuthProvHist:renderContainer  ("":U , "TABLE":U , TRUE) .
          goCntAuthCodingHist:renderContainer("":U , "TABLE":U , TRUE) .
          goCntAuthDetailHist:renderContainer("":U , "TABLE":U , TRUE) .

          
        END. /*WHEN "Enquiry":U THEN*/
      END CASE. /* gclsWob:Mode */                                              
      
      {&OUT}
      "</center>":U.
    END. /* WHEN "WebRenderProcedure":U */            
  END CASE. /* CASE ipoControl:RenderArgument: */

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebRowRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebRowRenderProcedure Procedure 
PROCEDURE WebRowRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:      Assign each group a different class when rendering so that we can 
                hide and display the remaining group records according to their class. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE cGroupCode AS CHARACTER NO-UNDO.

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WobShutdown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WobShutdown Procedure 
PROCEDURE WobShutdown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
  IF VALID-OBJECT(goCntLayout) 
  THEN DELETE OBJECT goCntLayout.
  
  IF VALID-OBJECT(goCntFilter) 
  THEN DELETE OBJECT goCntFilter.

  IF VALID-OBJECT(goCntAuthHist) 
  THEN DELETE OBJECT goCntAuthHist.
  
  DATASET dsAuthorisation:EMPTY-DATASET() .

  { mip/inc/mipcatcherror.i } 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnLinesInGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnLinesInGroup Procedure 
FUNCTION fnLinesInGroup RETURNS INTEGER
  ( INPUT ipcGroupCode AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

