&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
" This code is based on the cgi-wrapper template as designed by Progress.

  MIP Holdings (Pty) Ltd.

  Use this template to create a new Custom CGI Wrapper Procedure and write WebSpeed code that dynamically generates HTML. No associated static HTML file is needed."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  $Id: wsadvertwob.w,v 1.1 2013/09/27 12:51:11 cvs Exp $
  Purpose     : WarpSpeed Environment Setup WOB
  Description : Setup WOB for the table mic_environment
------------------------------------------------------------------------------*/

/* This helps to ensure proper cleanup */
CREATE WIDGET-POOL.

/* WarpSpeed's Shared Definitions */
{mip/inc/mipdefshared.i}
{ma/inc/maauthds.i}

DEFINE STREAM sIn.

/* Variables commonly used by WarpSpeed */
DEFINE VARIABLE wob                           AS cls.mipwswob       NO-UNDO.

/* Variables for this specific WOB */
DEFINE VARIABLE gcProduct                     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE gcJSOnLoad                    AS CHARACTER          NO-UNDO.

/* Containers */
DEFINE VARIABLE goCntLayout                   AS cls.mipwscontainer NO-UNDO.
DEFINE VARIABLE goCntSearch                   AS cls.mipwscontainer NO-UNDO.

/* Controls */
DEFINE VARIABLE goAdvertControl               AS cls.mipwscontrol   NO-UNDO.

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
         HEIGHT             = 17.76
         WIDTH              = 53.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/*------------------------------------------------------------------------------
  This is a WarpSpeed WOB - include ws/inc/wsstructure.i, and nothing else.
------------------------------------------------------------------------------*/

ASSIGN 
  Wob = WarpSpeed:CurrentObject
  NO-ERROR.
{mip/inc/mipreturnerror.i}

{ws/inc/wsstructure.i &WsShutdownProcedure = "'WobShutdown':U"}

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OutHTML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutHTML Procedure 
PROCEDURE OutHTML :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cFile   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHTML   AS CHARACTER   NO-UNDO.

  ASSIGN 
    cFile = SEARCH("sc/advert/" + gcProduct + "/" + gcProduct + ".txt":U)
    NO-ERROR.
  {mip/inc/mipreturnerror.i}
  
  INPUT STREAM sIn FROM VALUE(cFile).
  {mip/inc/mipreturnerror.i}
  
  REPEAT:
    IMPORT STREAM sIn UNFORMATTED cHTML NO-ERROR.
    {mip/inc/mipreturnerror.i}

    /* We strip the OnLoad function from the text file and use it in webDelieverResponse */
    IF cHTML BEGINS "'MM_preloadImages":U
    THEN
      gcJSOnLoad = cHTML NO-ERROR.
    ELSE
      {&OUT} cHTML SKIP.

    {mip/inc/mipreturnerror.i}

  END.

FINALLY:
  INPUT STREAM sIn CLOSE.
  {mip/inc/mipreturnerror.i}
END FINALLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebAcceptRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebAcceptRequest Procedure 
PROCEDURE WebAcceptRequest :
/*------------------------------------------------------------------------------
  Purpose: This procedure must capture all information from the Web Request.
           It will also run procedures to:
             1) Define WarpSpeed Containers
             2) Populate the defined Containers from the Request
             3) Perform Data validation
------------------------------------------------------------------------------*/

  /* Here we'll decide which Service Level we are going to be working with. */
  /* In cases where you need to provide for integration to Workflow or      */
  /* Call Center, this would be the place to deal with it, before going on. */
  
  ASSIGN
    wob:SubmitValue   = get-value("frmSubmit":U)
    gcProduct         = mipEnv:miExpression:getNVPElement(WarpSpeed:CurrentObject:ObjectAttributes, "Product":U)
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  /* Decide on the WOB's State - Search or Maint */
   ASSIGN wob:Mode = "Search":U  NO-ERROR.
  { mip/inc/mipreturnerror.i }

  /* Define the Containers */
  RUN WebFormDefinition IN TARGET-PROCEDURE NO-ERROR.
  { mip/inc/mipreturnerror.i }

  /* Perform Application-specific validations */
  RUN WebFormValidation IN TARGET-PROCEDURE NO-ERROR.
  { mip/inc/mipreturnerror.i }

  { mip/inc/mipreturnerror.i }

END PROCEDURE.
/* That's all Folks! */

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

  DEFINE BUFFER buf_environment FOR mic_environment.

  DEFINE VARIABLE lSuccess        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cWhatToDo       AS CHARACTER   NO-UNDO.

  CASE wob:Mode:
    /* -------------------------------------------------------------------------
       Business Logic for "Search" State
    ------------------------------------------------------------------------- */
    WHEN "Search":U THEN
    DO:
      /* nothing */
    END. /* WHEN "Search" */
    /* -- END Business Logic for "Search" State ----------------------------- */

    /* -------------------------------------------------------------------------
       Business Logic for "Maint" State
    ------------------------------------------------------------------------- */
    WHEN "Maint":U THEN
    DO:

    END. /* WHEN "Maint" */
    /* -- END Business Logic for "Maint" State ------------------------------ */

  END CASE. /* CASE wob:Mode */

  {mip/inc/mipreturnerror.i}

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
  DEFINE VARIABLE cSearchField  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cButtonList   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lSuccess      AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cWhatToDo     AS CHARACTER  NO-UNDO.


  CASE wob:Mode:
    /* -------------------------------------------------------------------------
       Preparations for "Search" State
    ------------------------------------------------------------------------- */
    WHEN "Search":U THEN
    DO:
      /* The Search Form really doesn't care about its ContainerMode, so we
         will set cWhatToDo diectly from wob:SubmitValue. */
      ASSIGN
        cWhatToDo = wob:SubmitValue
        NO-ERROR.
      { mip/inc/mipreturnerror.i }

      /* -- The purpose of this section is to get the Search QUERY STRING prepared --*/
      /* -- Only enter here if we CAN-DO(cWhatToDo) -- */
      IF CAN-DO( "Search":U, cWhatToDo ) THEN
      DO:
      END.

    END. /* WHEN "Search" */
    /* -- END Preparations for "Search" State ------------------------------- */

    /* -------------------------------------------------------------------------
       Preparations for "Maint" State
    ------------------------------------------------------------------------- */
    WHEN "Maint":U THEN
    DO:

    END.

  END CASE. /* wob:Mode */

  { mip/inc/mipreturnerror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebDeliverResponse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebDeliverResponse Procedure 
PROCEDURE WebDeliverResponse :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE lSuccess      AS LOGICAL     NO-UNDO.

  output-content-type("text/html":U).

  ASSIGN
      lSuccess = wsUiService:HTMLHead("":U, gcJSOnLoad, "":U)
      lSuccess = wsUiService:HTMLForm("OPEN":U, "frmMain")

      /* We MUST Render the Main Layout. It will run the WebRenderProcedure Procedure to Render our other Containers */
      lSuccess = goCntLayout:renderContainer("":U, "FORM":U, FALSE)

      lSuccess = wsUiService:HTMLForm("CLOSE":U, ?)
      lSuccess = wsUiService:HTMLTail()
    NO-ERROR.

    { mip/inc/mipreturnerror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormDefinition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormDefinition Procedure 
PROCEDURE WebFormDefinition :
/*------------------------------------------------------------------------------
  Purpose:     Use the WarpSpeed APIs to define the form and the form fields.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE lSuccess      AS LOGICAL     NO-UNDO.

  /* We make use of the "wsMain" Wob Layout */
  wsUiService:getLayoutInstance(INPUT "WobLayout":U,
                                INPUT "wsMain":U,
                                OUTPUT goCntLayout) NO-ERROR.
  {mip/inc/mipreturnerror.i}

  /* The Search Form */
  /* After Confirm of Delete we will come back to the Search State */
  IF Wob:Mode = "Search":U
  THEN DO:
    goCntSearch = NEW cls.mipwscontainer(Wob:ObjectCode + "Advert":U, "90%", "":U, WarpSpeed:BaseClass, TRUE) NO-ERROR.
    { mip/inc/mipreturnerror.i }

    ASSIGN 
      goCntSearch:Collapsable = FALSE

      goAdvertControl                  = goCntSearch:addControl("fcImage ":U, "wsInput":U, "":U, "":U, "":U, 1,1, "":U)
      goAdvertControl:RenderProcedure  = "WebRenderProcedure":U
      goAdvertControl:RenderArgument   = "AdvertHTML":U
      goAdvertControl:SpanOverLabel    = TRUE
      NO-ERROR.

      /* Fields for the FORM View */
  END.

  /* The Maintenance Form */
  IF wob:Mode = "Maint":U
  THEN DO:

  END. /* END of the Maintenance Form */

END PROCEDURE.

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebFormValidation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebFormValidation Procedure 
PROCEDURE WebFormValidation :
/*------------------------------------------------------------------------------
  Purpose: Perform all field-level validations for the Appliction here.
  Note:    Use goControl:setError() to raise the error condition correctly.
------------------------------------------------------------------------------*/

  /* Validation to be performed depends on wob:SubmitValue */
  CASE wob:SubmitValue:
    WHEN "Submit":U THEN
    DO:

    END.
    WHEN "Search":U THEN
    DO:

    END.
  END CASE. /* wob:SubmitValue */

END PROCEDURE.

/* That's all Folks! */

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
  DEFINE INPUT PARAMETER poControl  AS cls.mipwscontrol NO-UNDO.

  DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

  CASE poControl:RenderArgument:

    /* ---------------------------------------------------------------------- */
    /* The WebRenderProcedure is the main Rendering Procedure for this WOB    */
    WHEN "WebRenderProcedure":U
    THEN DO:
      {&OUT}
        "<center>":U.

      CASE wob:Mode:
        /* -- Output for Search State ------------------------------------------- */
        WHEN "Search":U THEN
        DO:
          ASSIGN
            goCntSearch:ContainerTitle = "Advert":U
            lSuccess = goCntSearch:renderContainer("":U, "FORM":U, FALSE)
            NO-ERROR.
          { mip/inc/mipreturnerror.i }

        END.

        /* -- Output for Search State ------------------------------------------- */
        WHEN "Maint":U THEN
        DO:
        END.

      END CASE. /* wob:Mode */

      {mip/inc/mipreturnerror.i}

      {&OUT}
        "</center>":U.
    END. /* WHEN "WebRenderProcedure":U */

    WHEN "AdvertHTML" THEN
      RUN OutHTML IN TARGET-PROCEDURE NO-ERROR.

  END CASE. /* CASE ipoControl:RenderArgument: */

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
  THEN DELETE OBJECT goCntLayout NO-ERROR.

  IF VALID-OBJECT(goCntSearch)
  THEN DELETE OBJECT goCntSearch NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

