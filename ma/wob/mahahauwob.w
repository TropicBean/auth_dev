&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Purpose     : Auth History Wob
  Description : Auth History Wob
  Author      : Karlb

  Notes       :  The default view mode of this wob will be a table view 
                 To get a form view , pass in a wobMode=FORM to view the history in a form view

                 This wob will also take a parameter "EntityList" . You can 
                 use this parameter if you only want certain entity history to display.
                 For example , you only want the auth header history and provider history 
                 you can pass:
                    &EntityList=hahau,hahap
                If the parameter is not specified , the list will default to all entities

                Supported entities are :
                  - hahau : hah_auth_history
                  - hahap : hah_auth_provider_history
                  - hahac : hah_auth_coding_hisotry
                  - hahad : hah_auth_detail_history
                  - hahcw : hah_auth_crosswalk_history
                  - hahcp : hah_auth_copay_history 
                  - hahal : hah_auth_limit_history
                  - hahep : hah_auth_episode_history
                  - hahaf : hah_auth_flag_value_history
                  - hahmc : hah_auth_mc_savings

------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.
/* This helps ensure proper cleanup */

CREATE WIDGET-POOL.

{ mip/inc/mipdefshared.i }
{ ma/inc/maauthds.i }
{ ma/inc/maauthtypeds.i } 

/*Variables*/
DEFINE VARIABLE gcWOBState              AS CHARACTER          NO-UNDO.
DEFINE VARIABLE gcWOBToken              AS CHARACTER          NO-UNDO.
DEFINE VARIABLE gcRowGroupCode          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE glSuccess               AS LOGICAL            NO-UNDO.
DEFINE VARIABLE glPopupMode             AS LOGICAL            NO-UNDO.
DEFINE VARIABLE gdAuthObj               AS DECIMAL            NO-UNDO.
DEFINE VARIABLE gcEntityList            AS CHARACTER          NO-UNDO.

DEFINE VARIABLE gclsWob                 AS cls.mipwswob       NO-UNDO.
                                        
/* Containers */                        
DEFINE VARIABLE goCntLayout             AS cls.mipwscontainer NO-UNDO.
DEFINE VARIABLE goCntFilter             AS cls.mipwscontainer NO-UNDO.

// Container configuration 
// !Important 
// If a additional container needs to be added to this wob, make sure to add the configuration to the JSON object below
DEFINE VARIABLE glcEntityConfiguration AS LONGCHAR NO-UNDO . 

ASSIGN glcEntityConfiguration = '~{ "ttConfiguration": 
                                    [~{"Entity"             : "hahau",
                                       "BufferList"         : "tt_auth_history",
                                       "DefinitionProcedure": "defineContainerAuth",
                                       "Sequence"           : 1 ~},
                                    
                                     ~{"Entity"             : "hahap",
                                       "BufferList"         : "tt_auth_provider_history",
                                       "DefinitionProcedure": "defineContainerProvider",
                                       "Sequence"           : 2 ~},
                                    
                                     ~{"Entity"             : "hahac",
                                       "BufferList"         : "tt_auth_coding_history",
                                       "DefinitionProcedure": "defineContainerCoding",
                                       "Sequence"           : 3 ~},
                                    
                                     ~{"Entity"             : "hahad",
                                       "BufferList"         : "tt_auth_detail_history",
                                       "DefinitionProcedure": "defineContainerDetail",
                                       "Sequence"           : 4 ~},
                                    
                                     ~{"Entity"             : "hahaw",
                                       "BufferList"         : "tt_auth_crosswalk_history",
                                       "DefinitionProcedure": "defineContainerCrosswalk",
                                       "Sequence"           : 5 ~},
                                    
                                     ~{"Entity"             : "hahms",
                                       "BufferList"         : "tt_auth_mc_savings_history",
                                       "DefinitionProcedure": "defineContainerMcSavings",
                                       "Sequence"           : 6 ~},
                                    
                                     ~{"Entity"             : "hahal",
                                       "BufferList"         : "tt_auth_limit_history",
                                       "DefinitionProcedure": "defineContainerLimit",
                                       "Sequence"           : 7 ~},
                                    
                                     ~{"Entity"             : "hahae",
                                       "BufferList"         : "tt_auth_episode_history",
                                       "DefinitionProcedure": "defineContainerEpisode",
                                       "Sequence"           : 8 ~},
                                    
                                     ~{"Entity"             : "hahaf",
                                       "BufferList"         : "tt_auth_flag_value_history",
                                       "DefinitionProcedure": "defineContainerFlag",
                                       "Sequence"           : 9 ~},
                                    
                                     ~{"Entity"             : "hahcp",
                                       "BufferList"         : "tt_auth_copay_history",
                                       "DefinitionProcedure": "defineContainerCopay",
                                       "Sequence"           : 10 ~}]
                                    ~}' .
                              
DEFINE TEMP-TABLE ttConfiguration NO-UNDO 
  FIELD Entity              AS CHARACTER
  FIELD DefinitionProcedure AS CHARACTER
  FIELD BufferList          AS CHARACTER 
  FIELD Sequence            AS INTEGER
  INDEX idx IS UNIQUE Entity
   
  INDEX idxSeq Sequence. 

// Read the JSON configuration in to a temp-table
TEMP-TABLE ttConfiguration:READ-JSON("longchar", glcEntityConfiguration) .

DEFINE TEMP-TABLE ttRegisteredContainer NO-UNDO
  FIELD Entity              AS CHARACTER
  FIELD ContainerCode       AS CHARACTER 
  FIELD ContainerType       AS CHARACTER 
  FIELD ContainerObject     AS Progress.Lang.Object
  FIELD ContainerProperties AS Progress.Lang.Object
  FIELD RenderSequence      AS INTEGER
  INDEX idx RenderSequence.   

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
    "~n  function fnExpandCollapseGroups(pControl,pGroupNum)~{                        ":U      
    "~n    var vGroup          = ~"clGrp~" + pGroupNum,                               ":U  
    "~n        vGrpSelectArray = document.getElementsByClassName(vGroup);             ":U  
    "~n                                                                               ":U
    "~n    if (pControl.checked) ~{                                                   ":U 
    "~n      wsSetClassName(wsGetParent(pControl,~"TR~"), ~"+clred~");                ":U  
    "~n      for( var i=0; i<vGrpSelectArray.length; i++ ) ~{                         ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"-clHid~");    ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"+clred~");    ":U 
    "~n      ~}                                                                       ":U 
    "~n    ~}                                                                         ":U 
    "~n    else ~{                                                                    ":U 
    "~n      wsSetClassName(wsGetParent(pControl,~"TR~"), ~"-clred~");                ":U 
    "~n      for( var i=0; i<vGrpSelectArray.length; i++ ) ~{                         ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"+clHid~");    ":U 
    "~n        wsSetClassName(wsGetParent(vGrpSelectArray[i],~"TR~"), ~"-clred~");    ":U 
    "~n      ~}                                                                       ":U 
    "~n    ~}                                                                         ":U
    "~n  ~}                                                                           ":U
              
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
  DEFINE VARIABLE oAuthSearch    AS cls.maauthsearch            NO-UNDO.
  DEFINE VARIABLE oControl       AS cls.mipwscontrol            NO-UNDO.
  
  DEFINE VARIABLE hQuery       AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cBuffer      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cField       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cQueryString AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iValue       AS INTEGER     NO-UNDO.
  
  DEFINE VARIABLE lMultiRange         AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE iCount              AS INTEGER   NO-UNDO.  
  DEFINE VARIABLE cUser               AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dDateFrom           AS DATETIME  NO-UNDO.
  DEFINE VARIABLE dDateTo             AS DATETIME  NO-UNDO.
  DEFINE VARIABLE cHistBufferList     AS CHARACTER NO-UNDO INITIAL "tt_auth_history,tt_auth_provider_history,tt_auth_coding_history,tt_auth_detail_history":U.
  
  ASSIGN 
    cUser       =          goCntFilter:getControl("fcFilterUserCode":U):ControlValue  
    dDateFrom   = DATETIME(goCntFilter:getControl("fdFilterFromDate":U):ControlValue)
    dDateTo     = DATETIME(goCntFilter:getControl("fdFilterToDate":U):ControlValue)
                                                      
    oAuthSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE) 
    lSuccess    = oAuthSearch:setfilterCriteria("tt_auth.auth_obj" , "=" , gdAuthObj).

  ASSIGN lSuccess    = oAuthSearch:fetchData().
  
  IF cUser     <> "":U
  OR dDateFrom <> ?
  OR dDateTo   <> ? THEN 
  DO:
    BUFFER-BLK:
    DO iCount = 1 TO NUM-ENTRIES(cHistBufferList):
      ASSIGN cBuffer      = ENTRY(iCount , cHistBufferList)
             lMultiRange  = dDateFrom <> ? AND dDateTo   <> ?  
             // This query string is... juicy ... but it works
             cQueryString = "FOR EACH " + cBuffer 
                          + "   WHERE TRUE "  
                          + (IF lMultiRange THEN " AND ":U 
                                            ELSE "":U)
                          + (IF cUser      <> "":U THEN "AND " + cBuffer + ".change_usr_id     <> " + QUOTER(cUser)     ELSE "":U ) + " ":U
                          + (IF lMultiRange THEN "(":U 
                                            ELSE "":U)
                          + (IF dDateFrom  <> ?    THEN (IF lMultiRange THEN "" ELSE "AND " ) + cBuffer + ".change_date_time   < " + QUOTER(STRING(dDateFrom)) ELSE "") + " ":U
                          + (IF lMultiRange THEN " OR ":U 
                                            ELSE "":U ) 
                          + (IF dDateTo    <> ?    THEN (IF lMultiRange THEN "" ELSE "AND " ) +  cBuffer + ".change_date_time   > " + QUOTER(STRING(dDateTo)  ) ELSE "") + " ":U
                          + (IF lMultiRange THEN ")":U 
                                            ELSE "":U) .
            

      CREATE BUFFER hBuffer FOR TABLE cBuffer.
      CREATE QUERY hQuery.
    
      hQuery:ADD-BUFFER(hBuffer).
      hQuery:QUERY-PREPARE(cQueryString).
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST().
      QUERY-BLK:
      REPEAT:
          
        IF hQuery:QUERY-OFF-END THEN LEAVE QUERY-BLK.

        DO TRANSACTION:
            hQuery:GET-CURRENT(EXCLUSIVE-LOCK).
            hBuffer:BUFFER-DELETE().
        END. //DO TRANSACTION
        
        hQuery:QUERY-OPEN().
          
        hQuery:GET-NEXT().
      END. //QUERY-BLK
      
      hQuery:QUERY-CLOSE().
      
      DELETE OBJECT hQuery.
      DELETE OBJECT hBuffer.
    END. //BUFFER-BLK
  END. //IF cUser <> "":U ...

  DEFINE VARIABLE oAuthTypeSearch  AS cls.maauthtypesearch NO-UNDO.

  ASSIGN oAuthTypeSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE)
         lSuccess        = oAuthTypeSearch:setCriteria("Bufferlist":U, "tt_auth_type":U).
  
  FOR EACH tt_auth_history:
    ASSIGN  lSuccess  = oAuthTypeSearch:setFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, tt_auth_history.auth_type_obj) .
  END. // FOR EACH tt_auth_history.
 
  oAuthTypeSearch:fetchData().
  
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
  DEFINE VARIABLE iEntity AS INTEGER NO-UNDO.
  
  IF Warpspeed:CurrentObj = ?
  THEN ASSIGN Warpspeed:CurrentObj = '':U .
  
  ASSIGN 
    gclsWob:SubmitValue = get-value("frmSubmit":U)
    
    gdAuthObj           = DECIMAL(Warpspeed:CurrentObj)
    
    glPopupMode         = get-value("popupMode":U) = "true":U
    
    gclsWob:Mode        = "Enquiry":U

   //The default view mode of this wob will be a table view 
    //To get a form view , pass in a wobMode=FORM to view the history in a form view
    gclsWob:Mode        = IF get-value("wobMode") = "":U 
                          THEN "TABLE":U
                          ELSE  get-value("wobMode") 
    gcEntityList        = IF get-value("EntityList") = "":U 
                          THEN "hahau" // "hahau,hahap,hahac,hahad,hahcw,hahcp,hahal,hahep":U 
                          ELSE get-value("EntityList").

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
  Purpose: Nothing to do here since this is a view-only wob
------------------------------------------------------------------------------*/
                      
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
  
  DEFINE VARIABLE hBuffer       AS HANDLE             NO-UNDO.  
  DEFINE VARIABLE cBufferName   AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE iBuffer       AS INTEGER            NO-UNDO.  
  DEFINE VARIABLE oContainer    AS cls.mipwscontainer NO-UNDO.
  
  RUN RetrieveData IN TARGET-PROCEDURE.

  ATTACH-BUFFER-LIST-BLK:
  FOR EACH ttRegisteredContainer : 

    FIND FIRST ttConfiguration 
         WHERE ttConfiguration.Entity = ttRegisteredContainer.Entity .
    
    ASSIGN oContainer = ttRegisteredContainer.ContainerObject .

    ATTACH-BUFFER-BLK:
    DO iBuffer = 1 TO NUM-ENTRIES(ttConfiguration.BufferList) :
      
      ASSIGN cBufferName = ENTRY(iBuffer, ttConfiguration.BufferList).

      CREATE BUFFER hBuffer FOR TABLE cBufferName . 

      ASSIGN oContainer:QueryBufferList = oContainer:QueryBufferList 
                                        + IF(oContainer:QueryBufferList <> "":U) THEN ",":U ELSE "":U
                                        + STRING(hBuffer:DEFAULT-BUFFER-HANDLE).



    END. //ATTACH-BUFFER-BLK:

    //Once all the buffers are attached, we can populate
    oContainer:PopulateFromQuery().

  END. // ATTACH-BUFFER-BLK

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
  Purpose: This procedure will only define the containers that are defined in the
           gcEntityList global variable. 
           
  Parameters:  <none>
  Notes:  The definition procedure for the entity code will be routed via the 
          configuration stored in gcEntityProcedureList

          Container types will be determined by the wob mode. Default will be TABLE
          for Search mode 
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iEntity               AS INTEGER                   NO-UNDO .
  DEFINE VARIABLE cContainerType        AS CHARACTER                 NO-UNDO .
  DEFINE VARIABLE cEntity               AS INTEGER                   NO-UNDO .
  DEFINE VARIABLE oContainer            AS cls.mipwscontainer        NO-UNDO .
  DEFINE VARIABLE oContainerProperties  AS cls.wscontainerproperties NO-UNDO .

  wsUiService:getLayoutInstance(INPUT "WobLayout":U, INPUT "wsMainNoMenu":U, OUTPUT goCntLayout). //wsMainNoMenu
  
  // Filter is for search mode only
  IF gclsWob:Mode = "Search":U 
  THEN 
    RUN WebFormDefinition_Filter  IN TARGET-PROCEDURE.
    
  ASSIGN cContainerType = IF gclsWob:Mode = "Search":U THEN "TABLE":U ELSE "FORM":U .

  ENTITY-DEFINE-BLK:
  DO iEntity = 1 TO NUM-ENTRIES(gcEntityList):

    ASSIGN cEntity        = ENTRY(iEntity , gcEntityList) .

    FIND FIRST ttConfiguration 
         WHERE ttConfiguration.Entity = cEntity NO-ERROR. 

    IF NOT AVAILABLE ttConfiguration
    THEN 
      {mip/inc/mipthrowerror.i 'AuthHistory_EntityWebFormDefinition':U 1 "'Unable to determine the configuration for entity &1'" ? QUOTER(cEntity)}


    RUN VALUE(ttConfiguration.DefinitionProcedure ) IN TARGET-PROCEDURE( INPUT  cContainerType ,
                                                                         OUTPUT oContainer,
                                                                         OUTPUT oContainerProperties) NO-ERROR .

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:6456":U 
    THEN 
      {mip/inc/mipthrowerror.i 'AuthHistory_EntityWebFormDefinition':U 1 "'Unable to locate procedure &1 in the program mahahauwob.w'" ? QUOTER(ttConfiguration.DefinitionProcedure )}
     
    IF VALID-OBJECT(oContainer) THEN
    DO: 
      CREATE ttRegisteredContainer.
  
      ASSIGN ttRegisteredContainer.Entity              = cEntity 
             ttRegisteredContainer.ContainerCode       = oContainer:ContainerCode
             ttRegisteredContainer.ContainerType       = oContainerProperties
             ttRegisteredContainer.ContainerObject     = oContainer
             ttRegisteredContainer.ContainerProperties = oContainerProperties
             ttRegisteredContainer.RenderSequence      = ttConfiguration.Sequence .

    END. // IF VALID-OBJECT(oContainer)
  END. // ENTITY-BLK                            
  
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUMEdaaa

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Auth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Auth Procedure 
PROCEDURE WebFormDefinition_Auth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.

  DEFINE VARIABLE oControl        AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iControl        AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cContainerCode  AS CHARACTER        NO-UNDO.
  
  CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:
      ASSIGN 
      cContainerCode                     = "AuthHist":U
      opoContainer                       = NEW cls.mipwscontainer(cContainerCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
      opoContainer:ContainerTitle        = "Authorisation History":U
      opoContainer:ViewOnly              = FALSE
      opoContainer:ShowContainerSettings = FALSE
      opoContainer:Collapsable           = FALSE
      opoContainer:RowsToRender          = ?
      opoContainer:ContainerMode         = Warpspeed:SubmitValue
      opoContainer:QueryString           = "FOR EACH tt_auth_history NO-LOCK":U
                                     //  + "   WHERE tt_auth_history.auth_obj = '&1',":U
    
      oControl                            = opoContainer:addControl("fc_action":U           + cContainerCode , "wsInput":U    , "10":U, "tt_auth_history.action":U                   , "CHARACTER":U, 1, "Action":U                  )   
      oControl                            = opoContainer:addControl("fc_change_date_time":U + cContainerCode , "wsInput":U    , "10":U, "tt_auth_history.change_date_time":U         , "DATETIME":U , 2, "Change Date Time":U        )        
      oControl                            = opoContainer:addControl("fc_change_usr_id":U    + cContainerCode , "wsInput":U    , "10":U, "tt_auth_history.change_usr_id":U            , "CHARACTER":U, 3, "Change User Id":U          )                                                                  
      oControl                            = opoContainer:addControl("fcAuthTypeCode":U      + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.auth_type_obj":U            , "CHARACTER":U, 6, "Auth Type":U               )    
      oControl:RenderProcedure            = "WebRenderProcedure":U
      oControl:RenderArgument             = "AuthType":U
      oControl                            = opoContainer:addControl("fdStartDate":U         + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.start_date":U               , "DATE":U     , 7, "Authorisation Start Date":U)
      oControl                            = opoContainer:addControl("fdEndDate":U           + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.end_date":U                 , "DATE":U     , 8, "Authorisation End Date":U  )     
      oControl                            = opoContainer:addControl("fiClaimCode":U         + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.claim_code":U               , "INTEGER":U  , 9, "Claim Code":U              )
      oControl                            = opoContainer:addControl("cbClaimType":U         + cContainerCode, "wsInput":U     , "3":U , "tt_auth_history.claim_type":U               , "CHARACTER":U, 10, "Claim Type":U             )      
      oControl:AdditionalItems            = "=|C=C|N=N|A=A|K=K|P=P|O=O":U  
      oControl                            = opoContainer:addControl("cbStatusDisp":U        + cContainerCode, "wsCombo":U     , "15":U, "tt_auth_history.auth_status":U              , "INTEGER":U  , 11, "Authorisation Status":U   )
      oControl:RenderProcedureHandle      = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                
      oControl:RenderProcedure            = "RenderProcedure":U                                                                                                                            
      oControl:RenderArgument             = "FullSystemStatusCombo":U  
      oControl                            = opoContainer:addControl("fcReason":U            + cContainerCode, "wsInput":U     , "15":U, "tt_auth_history.auth_status_note":U         , "CHARACTER":U, 12, "Status Reason":U          ) .            
    
    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType

  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition_Provider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition_Provider Procedure 
PROCEDURE WebFormDefinition_Provider :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.

  DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iControl       AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
 
  CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:
      ASSIGN 
        cContainerCode                        = "AuthProvHist":U
        opoContainer                          = NEW cls.mipwscontainer(cContainerCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
        opoContainer:ContainerTitle           = "Provider History":U
        opoContainer:RowsToRender             = ?
        opoContainer:ViewOnly                 = FALSE
        opoContainer:ShowContainerSettings    = FALSE
        opoContainer:Collapsable              = TRUE
        opoContainer:ContainerMode            = Warpspeed:SubmitValue
        opoContainer:QueryString              = "FOR EACH tt_auth_provider_history NO-LOCK ":U
                                                   + "BY tt_auth_provider_history.auth_provider_obj"
      
        oControl                              = opoContainer:addControl("fcAction":U                          + cContainerCode , "wsInput":U      , "10":U , "tt_auth_provider_history.action":U                 , "CHARACTER":U, 1 , "Action":U                )   
        oControl                              = opoContainer:addControl("fcChangeDateTime":U                  + cContainerCode , "wsInput":U      , "10":U , "tt_auth_provider_history.change_date_time":U       , "DATETIME":U , 2 , "Change Date Time":U      )        
        oControl                              = opoContainer:addControl("fcChangeUsrId":U                     + cContainerCode , "wsInput":U      , "10":U , "tt_auth_provider_history.change_usr_id":U          , "CHARACTER":U, 3 , "Change User Id":U        )  
        oControl                              = opoContainer:addControl("cbSequence":U                        + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.provider_sequence":U      , "CHARACTER":U, 4,  "Seq":U)                                                                                                                    
        oControl                              = opoContainer:addControl("flAuthorised":U                      + cContainerCode, "wsCheckBox":U    , "20":U , "tt_auth_provider_history.authorised_service":U     , "LOGICAL":U ,  6,  "A/S":U)                                                                                                                                        
        oControl                              = opoContainer:addControl("cbProviderType":U                    + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.provider_type":U          , "CHARACTER":U, 7,  "Provider Type":U)                                                                                                                        
        oControl:RenderProcedure              = "RenderProcedure":U                                                                             
        oControl:RenderArgument               = "AcronymSelect:ma_acAuthProviderType:=":U 
        oControl                              = opoContainer:addControl("fiProviderNum":U                     + cContainerCode, "wsInput":U       , "8":U  , "tt_auth_provider_history.doc_num":U                , "CHARACTER":U, 8,  "Provider Number":U)
        oControl                              = opoContainer:addControl("fdAuthGroupObj":U                    + cContainerCode, "wsCombo":U       , "10":U , "tt_auth_provider_history.auth_group_obj":U         , "CHARACTER":U, 10, "Auth Group":U)                                                                                                                                                                                                   
        oControl                              = opoContainer:addControl("fiDiscipline":U                      + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.pr_type":U                , "CHARACTER":U, 11, "Disc":U)
        oControl                              = opoContainer:addControl("fiSubDiscipline":U                   + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.sub_pr_type":U            , "CHARACTER":U, 12, "Sub-Dis":U)                                     
        oControl                              = opoContainer:addControl("fcProviderBaseRate":U                + cContainerCode, "wsInput":U       , "18":U , "tt_auth_provider_history.default_base_rate":U      , "CHARACTER":U, 13, "Default<br>Base<br>Rate":U)
        oControl                              = opoContainer:addControl("fcProviderArsRate":U                 + cContainerCode, "wsInput":U       , "18":U , "tt_auth_provider_history.default_ars_rate":U       , "CHARACTER":U, 14, "Default<br>Ars<br>Rate":U)
        oControl                              = opoContainer:addControl("cbOverrideBaseRate":U                + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.override_base_rate":U     , "CHARACTER":U, 15, "Override<br>Base<br>Rate":U)
        oControl                              = opoContainer:addControl("cbOverrideArsRate":U                 + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.override_ars_rate":U      , "CHARACTER":U, 16, "Override<br>Ars<br>Rate":U)                                                                                               
        oControl                              = opoContainer:addControl("flMainProvider":U                    + cContainerCode, "wsCheckBox":U    , "18":U , "tt_auth_provider_history.main_provider":U          , "LOGICAL":U  , 17, "Main":U)
        oControl                              = opoContainer:addControl("flAuthoriseAllServices":U            + cContainerCode, "wsCheckBox":U    , "18":U , "tt_auth_provider_history.authorise_all_services":U , "LOGICAL":U  , 19, "Authorise<br>All<br>Services":U)
        oControl                              = opoContainer:addControl("fiAttProviderNum":U                  + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.group_doc_num":U          , "CHARACTER":U, 20, "Associated Provider":U)
        oControl                              = opoContainer:addControl("fdStartDate":U                       + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.start_date":U             , "DATE":U     , 24, "":U)
        oControl                              = opoContainer:addControl("cbStartAmPm":U                       + cContainerCode, "wsCombo":U       , "3":U  , "tt_auth_provider_history.start_ampm":U             , "LOGICAL":U  , 24, "Start Date":U)
        oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
        oControl:CellLayoutMask               = "&1 &2":U    
        oControl                              = opoContainer:addControl("fdEndDate":U                         + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.end_date":U               , "DATE":U     , 25, "End Date":U)
        oControl                              = opoContainer:addControl("cbEndAmPm":U                         + cContainerCode, "wsCombo":U       , "3":U  , "tt_auth_provider_history.end_ampm":U               , "LOGICAL":U  , 25, "End Date":U)
        oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
        oControl:CellLayoutMask               = "&1 &2":U    
        oControl                              = opoContainer:addControl("fiClaimCode":U                       + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.claim_code":U             , "INTEGER":U  , 26, "Claim<br>Code":U)
        oControl                              = opoContainer:addControl("cbClaimType":U                       + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.claim_type":U             , "CHARACTER":U, 27, "Claim<br>Type":U)                                                                       
        oControl                              = opoContainer:addControl("cbStatus":U                          + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.auth_status":U            , "INTEGER":U  , 28, "Auth Status":U)
        oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                
        oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                            
        oControl:RenderArgument               = "FullSystemStatusCombo":U    

        oControl                              = opoContainer:addControl("fcReason":U                          + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.auth_status_note":U       , "CHARACTER":U, 29, "Status<br>Reason":U)
        oControl                              = opoContainer:addControl("fcCopayOverrideNote":U               + cContainerCode, "wsInput":U       , "3":U  , "tt_auth_provider_history.copay_override_note":U    , "INTEGER":U  , 30, "Co-payment<br>Override<br> Reason":U)   
        oControl                              = opoContainer:addControl("fdQuantityAuth":U                    + cContainerCode, "wsInput":U       , "5":U  , "tt_auth_provider_history.quantity_auth":U          , "DECIMAL":U  , 34, "Quantity<br>Auth":U)
        oControl                              = opoContainer:addControl("fdAmountAuth":U                      + cContainerCode, "wsInput":U       , "12":U , "tt_auth_provider_history.amount_auth":U            , "DECIMAL":U  , 35, "Amount<br>Auth":U)
        oControl                              = opoContainer:addControl("fdAmountReq":U                       + cContainerCode, "wsInput":U       , "12":U , "tt_auth_provider_history.amount_requested":U       , "DECIMAL":U  , 36, "Amount<br>Request":U)
        oControl                              = opoContainer:addControl("fdQtyReq":U                          + cContainerCode, "wsInput":U       , "12":U , "tt_auth_provider_history.quantity_requested":U     , "DECIMAL":U  , 37, "Qty<br>Request":U)
        oControl                              = opoContainer:addControl("flPmbIndicator":U                    + cContainerCode, "wsCheckBox":U    , "12":U , "tt_auth_provider_history.pmb_indicator":U          , "LOGICAL":U  , 40, "PMB<br>Indicator":U)
        oControl                              = opoContainer:addControl("flLosCalculation":U                  + cContainerCode, "wsCheckBox":U    , "12":U , "tt_auth_provider_history.los_calculation":U        , "LOGICAL":U  , 41, "System<br>LOS<br>Calculation":U)
     // oControl                              = opoContainer:addControl("fcAccountReference":U                + cContainerCode, "wsInput":U       , "10":U , "tt_auth_provider_history.account_reference":U      , "CHARACTER":U, 33, "Account<br>Reference":U)
         .
      ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(opoContainer)
                oContainerProperties:DefaultLess            = TRUE
                oContainerProperties:NumberVisibleControls  = 1
                oContainerProperties:CollapsableControlList = "fiAttProviderNum":U     + cContainerCode + ",":U // + "buAttProviderBtn":U       + cContainerCode + ",":U   
                                                            + "fiAttDiscipline":U      + cContainerCode + ",":U 
                                                          //  + "buAttDiscipline":U      + cContainerCode + ",":U + "fiAttSubDiscipline":U     + cContainerCode + ",":U 
                                                          //  + "buAttSubDiscipline":U   + cContainerCode + ",":U + "cbOverrideBaseRate":U     + cContainerCode + ",":U 
                                                          //  + "cbOverrideArsRate":U    + cContainerCode + ",":U + "fcAccountReference":U     + cContainerCode + ",":U
                                                            + "fdAuthGroupObj":U       + cContainerCode + ",":U + "flAuthoriseAllServices":U + cContainerCode + ",":U
                                                            + "fcCopayOverrideNote":U  + cContainerCode + ",":U //+ "fcRateChangeType":U       + cContainerCode + ",":U
                                                            + "fdAmountReq":U          + cContainerCode + ",":U + "fdQtyReq":U               + cContainerCode.                                                     

      mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties). 
    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType

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
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.

  DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iControl       AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.

  CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:
      ASSIGN 
        cContainerCode                        = "AuthCodingHist":U
        opoContainer                          = NEW cls.mipwscontainer(cContainerCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
        opoContainer:ContainerTitle           = "Coding History":U
        opoContainer:RowsToRender             = ?
        opoContainer:ViewOnly                 = FALSE
        opoContainer:ShowContainerSettings    = FALSE
        opoContainer:Collapsable              = TRUE
        opoContainer:ContainerMode            = Warpspeed:SubmitValue
        opoContainer:QueryString              = "FOR EACH tt_auth_coding_history NO-LOCK ":U
                                                     + "BY tt_auth_coding_history.auth_coding_obj"
      
        // Controls 
        oControl                       = opoContainer:addControl("fcAction":U                    + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.action":U                   , "CHARACTER":U ,  2 , "Action":U           )  
        oControl                       = opoContainer:addControl("fcChangeDateTime":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.change_date_time":U         , "DATETIME":U  ,  3 , "Change Datetime":U         )   
        oControl                       = opoContainer:addControl("fcChangeUsrId":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.change_usr_id":U            , "CHARACTER":U ,  4 , "Change User":U             )        
        oControl                       = opoContainer:addControl("fcCodingStatus":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.coding_status":U            , "INTEGER":U   , 12 , "Status":U                  )    
        oControl:RenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                
        oControl:RenderProcedure       = "RenderProcedure":U                                                                                                                            
        oControl:RenderArgument        = "FullSystemStatusCombo":U  
        oControl                       = opoContainer:addControl("fcCodingStatusNote":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.coding_status_note":U       , "CHARACTER":U , 13 , "Reason":U                  )    
        oControl                       = opoContainer:addControl("fcCodingType":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.coding_type":U              , "CHARACTER":U , 14 , "Coding Type":U             )  
        oControl:RenderProcedure       = "RenderProcedure":U
        oControl:RenderArgument        = "AcronymSelect:ma_acICDCodingType:=":U
        oControl                       = opoContainer:addControl("fcEndDate":U                  + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.end_date":U                 , "DATE":U      , 15 , "End Date":U                )  
        oControl                       = opoContainer:addControl("fcOwningAltValue":U           + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_alt_value":U         , "CHARACTER":U , 17 , "Code":U                    )  
        oControl                       = opoContainer:addControl("fcOwningEntityMnemonic":U     + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_entity_mnemonic":U   , "CHARACTER":U , 18 , "Owning Entity":U           )    
        oControl                       = opoContainer:addControl("fcPmbIndicator":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.pmb_indicator":U            , "LOGICAL":U   , 21 , "PMB":U                     )   
        oControl                       = opoContainer:addControl("fcPrimaryCode":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.primary_code":U             , "LOGICAL":U   , 22 , "Primary":U                 )    
        oControl                       = opoContainer:addControl("fcRelatedAltValue":U          + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_alt_value":U        , "CHARACTER":U , 23 , "Related Code":U            )    
        oControl                       = opoContainer:addControl("fcRelatedEntityMnemonic":U    + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_entity_mnemonic":U  , "CHARACTER":U , 24 , "Related Entity":U          )      
        oControl                       = opoContainer:addControl("fcSequence":U                 + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.sequence":U                 , "INTEGER":U   , 27 , "Seq":U                     )    
        oControl                       = opoContainer:addControl("fcStartDate ":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.start_date":U               , "CHARACTER":U , 28 , "Start Date ":U             )    
        oControl                       = opoContainer:addControl("fcBodyRegion":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.body_region":U              , "CHARACTER":U , 30 , "Body Region":U             )  
        
        //oControl = goCntAuthCodingHist:addControl("fc_owning_key":U                + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_key":U               , "CHARACTER":U , 19 , "Owning Key":U              )  
        //oControl = goCntAuthCodingHist:addControl("fc_owning_obj":U                + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.owning_obj":U               , "DECIMAL":U   , 20 , "Owning Obj":U              )  
        //oControl = goCntAuthCodingHist:addControl("fc_related_key":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_key":U              , "CHARACTER":U , 25 , "Related Key":U             )  
        //oControl = goCntAuthCodingHist:addControl("fc_morph_diag_obj":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.morph_diag_obj":U           , "DECIMAL":U   , 16 , "Morphology":U              )    
        //oControl = goCntAuthCodingHist:addControl("fc_related_obj":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.related_obj":U              , "DECIMAL":U   , 26 , "Related Obj":U             )  
        //oControl = goCntAuthCodingHist:addControl("fc_ass_diag_obj":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.ass_diag_obj":U             , "DECIMAL":U   ,  6 , "Associated diagnosis Obj":U)  
        //oControl = goCntAuthCodingHist:addControl("fc_line_number":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_coding_history.line_number":U              , "INTEGER":U   ,  5 , "Line Number":U             )  
      .  
    
    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType



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
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.

  CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:

    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType

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
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.
  
  
DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
DEFINE VARIABLE iControl       AS INTEGER          NO-UNDO.
DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.

CASE ipcContainerType :
  WHEN "TABLE":U THEN 
  DO:

    ASSIGN 
      cContainerCode                        = "AuthDetailHist":U
      opoContainer                          = NEW cls.mipwscontainer(cContainerCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
      opoContainer:ContainerTitle           = "Detail History":U
      opoContainer:RowsToRender             = ?
      opoContainer:ViewOnly                 = FALSE
      opoContainer:ShowContainerSettings    = FALSE         
      opoContainer:Collapsable              = TRUE
      opoContainer:ContainerMode            = Warpspeed:SubmitValue
      opoContainer:QueryString              = "FOR EACH tt_auth_detail_history NO-LOCK ":U
                                                   + "BY tt_auth_detail_history.auth_detail_obj"
     
      oControl                              = opoContainer:addControl("fc_action":U                   + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.action":U                 , "CHARACTER":U,  3 , "Action":U                )   
      oControl                              = opoContainer:addControl("fc_change_date_time":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.change_date_time":U       , "DATETIME":U ,  4 , "Change Date Time":U      )        
      oControl                              = opoContainer:addControl("fc_change_usr_id":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.change_usr_id":U          , "CHARACTER":U,  5 , "Change User Id":U        )  
      oControl                              = opoContainer:addControl("fc_auth_provider_obj":U        + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.auth_provider_obj":U      , "DECIMAL":U  ,  6 , "Auth Provider":U     )  
      oControl:RenderProcedure              = "WebRenderProcedure":U
      oControl:RenderArgument               = "AuthProviderDetail":U
      oControl                              = opoContainer:addControl("fc_owning_alt_value":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_alt_value":U       , "CHARACTER":U, 12 , "Owning Alt Value":U      )    
      oControl                              = opoContainer:addControl("fc_owning_entity_mnemonic":U   + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_entity_mnemonic":U , "CHARACTER":U, 13 , "Owning Entity Mnemonic":U)    
      oControl                              = opoContainer:addControl("fc_owning_key":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_key":U             , "CHARACTER":U, 14 , "Owning Key":U            )  
      oControl                              = opoContainer:addControl("fc_loc_sequence":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.loc_sequence":U           , "INTEGER":U  , 16 , "LOC Sequence":U          )    
      oControl                              = opoContainer:addControl("fc_quantity_los":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.quantity_los":U           , "DECIMAL":U  , 17 , "Quantity LOS":U          )  
      oControl                              = opoContainer:addControl("fc_minutes_auth":U             + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.minutes_auth":U           , "INTEGER":U  , 18 , "Minutes Auth":U          )    
      oControl                              = opoContainer:addControl("fc_start_date":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.start_date":U             , "DATE":U     , 20 , "Start Date":U            )  
      oControl                              = opoContainer:addControl("fc_start_ampm":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.start_ampm":U             , "LOGICAL":U  , 20 , "Start Date":U            ) 
      oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
      oControl:CellLayoutMask               = "&1 &2":U    
      oControl                              = opoContainer:addControl("fc_end_date":U                 + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.end_date":U               , "DATE":U     , 21 , "End Date":U              )   
      oControl                              = opoContainer:addControl("fc_end_ampm":U                 + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.end_ampm":U               , "LOGICAL":U  , 21 , "End Date":U              )    
      oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
      oControl:CellLayoutMask               = "&1 &2":U   
      oControl                              = opoContainer:addControl("fc_fixed_item_cost":U          + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.fixed_item_cost":U        , "DECIMAL":U  , 23 , "Fixed Item Cost":U       )    
      oControl                              = opoContainer:addControl("fc_quantity_auth":U            + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.quantity_auth":U          , "DECIMAL":U  , 24 , "Qauntity Auth":U         )      
      oControl                              = opoContainer:addControl("fc_amount_paid":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.amount_paid":U            , "DECIMAL":U  , 25 , "Amount Paid":U           )  
      oControl                              = opoContainer:addControl("fc_claim_code":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.claim_code":U             , "INTEGER":U  , 26 , "Claim Code":U            )  
      oControl                              = opoContainer:addControl("fc_claim_type":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.claim_type":U             , "CHARACTER":U, 27 , "Claim Type":U            )    
      oControl                              = opoContainer:addControl("fc_line_restriction":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.line_restriction":U       , "CHARACTER":U, 28 , "Line Restriction":U      )    
      oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle
      oControl:RenderArgument               = "LineRestriction":U
      oControl:RenderProcedure              = "RenderProcedure":U
      
      oControl                              = opoContainer:addControl("fc_auth_status":U              + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.auth_status":U            , "INTEGER":U  , 29 , "Auth Status":U           )    
      oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                
      oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                            
      oControl:RenderArgument               = "FullSystemStatusCombo":U  
      oControl                              = opoContainer:addControl("fc_auth_status_note":U         + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.auth_status_note":U       , "CHARACTER":U, 30 , "Auth Status Note":U      )    
   // oControl                              = opoContainer:addControl("fc_owning_obj":U               + cContainerCode , "wsInput":U , "10":U ,  "tt_auth_detail_history.owning_obj":U             , "DECIMAL":U  , 15 , "Owning Obj":U            )  
  
    .       
  END. // WHEN "TABLE"
  WHEN "FORM":U THEN
  DO:
    
  END. // WHEN "FORM"
END CASE. // CASE ipcContainerType
                                 

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
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.

  CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:

    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType

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
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.

  CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:

    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType

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
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.

  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE iControl AS INTEGER          NO-UNDO.

  CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:

    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType

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
  Parameters:  Container type : TABLE/FORM
               Container Object
               Container Object Properties
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerType       AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer           AS cls.mipwscontainer        NO-UNDO. 
  DEFINE OUTPUT PARAMETER opoContainerProperties AS cls.wscontainerproperties NO-UNDO.


  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE iControl AS INTEGER          NO-UNDO.

   CASE ipcContainerType :
    WHEN "TABLE":U THEN 
    DO:

    END. // WHEN "TABLE"
    WHEN "FORM":U THEN
    DO:
      
    END. // WHEN "FORM"
  END CASE. // CASE ipcContainerType

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
    oButton                    = oControl:SubContainer:getControl("btnSearch":U) 
    oButton:ControlSubType     = "SUBMIT":U
    .
       
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

  DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cGroupCode   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBufferName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj AS DECIMAL   NO-UNDO .
                                                             
  CASE ipoControl:RenderArgument:

    /* ---------------------------------------------------------------------- */
    /* The WebRenderProcedure is the main Rendering Procedure for this WOB    */
    WHEN "WebRenderProcedure":U THEN 
    DO:
    
      {&OUT}
      "<center>":U.
      
      IF gclsWob:Mode = "Search":U
      THEN
        goCntFilter:renderContainer("":U, "FORM":U, FALSE). 

      RENDER-CONTAINER-BLK:
      FOR EACH ttRegisteredContainer :

        ASSIGN oContainer = ttRegisteredContainer.ContainerObject.

        oContainer:renderContainer("":U , ttRegisteredContainer.ContainerType , TRUE) .

      END. // RENDER-CONTAINER-BLK
                                                
      {&OUT}
      "</center>":U.
    END. /* WHEN "WebRenderProcedure":U */     
    WHEN "AuthType":U THEN
    DO:
      
      ASSIGN dAuthTypeObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_history.auth_type_obj":U, "BUFFER-VALUE":U)) .
      
      FIND FIRST tt_auth_type 
           WHERE tt_auth_type.auth_type_obj = dAuthTypeObj NO-ERROR. 
           
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE tt_auth_type 
      THEN 
        ASSIGN ipoControl:ControlValue = tt_auth_type.auth_type .
        
      ipoControl:renderAsInput() .
      
    END. // WHEN "AuthType":U 
    WHEN "AuthProviderDetail":U THEN
    DO:
      DEFINE VARIABLE dAuthProvObj AS DECIMAL NO-UNDO.
      
      ASSIGN dAuthProvObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_detail_history.auth_provider_obj":U, "BUFFER-VALUE":U)).
    
      FIND FIRST tt_auth_provider
           WHERE tt_auth_provider.auth_provider_obj = dAuthProvObj NO-ERROR .
           
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF NOT AVAILABLE tt_auth_provider 
      THEN 
        ASSIGN ipoControl:ControlValue = "Unavailable":U .
      ELSE 
        ASSIGN ipoControl:ControlValue = STRING(tt_auth_provider.doc_num) .
        
      ipoControl:renderAsInput() .
    END.
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
  DATASET dsAuthType:EMPTY-DATASET(). 
  
  EMPTY TEMP-TABLE ttConfiguration .
  EMPTY TEMP-TABLE ttRegisteredContainer.

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

