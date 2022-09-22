&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    Purpose:
  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
{ mip/inc/mipdefshared.i}

DEFINE VARIABLE ghLayoutManager             AS HANDLE     NO-UNDO.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ghLayoutManager = wsUiService:renderProcedureHandle NO-ERROR.

{ mip/inc/mipreturnerror.i }

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getAuthClaims) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAuthClaims Procedure 
PROCEDURE getAuthClaims :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcContainerCode    AS CHARACTER          NO-UNDO.
  DEFINE INPUT  PARAMETER pcExtractCode      AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER oCntSubmit         AS cls.mipwscontainer NO-UNDO.
  DEFINE OUTPUT PARAMETER pcCustomFunctions  AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER pcOnUnloadSnippet  AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER pcScript           AS CHARACTER          NO-UNDO.
  
  DEFINE VARIABLE lSuccess                   AS LOGICAL            NO-UNDO. 
  DEFINE VARIABLE cAuthTypes                 AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER            NO-UNDO.
  DEFINE VARIABLE cTypes                     AS CHARACTER          NO-UNDO.
  
  DEFINE VARIABLE goFromAuthDate             AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goToAuthDate               AS cls.mipwscontrol   NO-UNDO. 
  DEFINE VARIABLE goAuthTypes                AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goAuthStatus               AS cls.mipwscontrol   NO-UNDO.
  
  ASSIGN oCntSubmit = NEW cls.mipwscontainer(pcContainerCode + "_submit":U, "100%", "":U, WarpSpeed:BaseClass, TRUE).
  { mip/inc/mipreturnerror.i }

  FIND FIRST datalist NO-LOCK
       WHERE datalist.list-type = "MCACUST":U      
         AND datalist.list-code = "HOSPAUTH":U     
         AND datalist.link-from = "" NO-ERROR.
  IF AVAILABLE datalist THEN 
    ASSIGN cAuthTypes = REPLACE(datalist.DESCRIPTION,",":U,"|":U) NO-ERROR.

  DO iCount = 1 TO NUM-ENTRIES(cAuthTypes,"|":U):
    FIND FIRST datalist NO-LOCK
         WHERE datalist.list-type = "MCATYPES":U
           AND datalist.list-code = ENTRY(iCount,cAuthTypes,"|":U) NO-ERROR.
    IF AVAILABLE datalist THEN 
      ASSIGN cTypes = cTypes + ENTRY(iCount,cAuthTypes,"|":U) + " - " + datalist.DESCRIPTION + "=" + ENTRY(iCount,cAuthTypes,"|":U) + "|" NO-ERROR.
    ELSE
      ASSIGN cTypes = cTypes + ENTRY(iCount,cAuthTypes,"|":U) + "=" + ENTRY(iCount,cAuthTypes,"|":U) + "|" NO-ERROR.
  END.
  
  IF NOT ERROR-STATUS:ERROR
  THEN DO:
    ASSIGN
      oCntSubmit:ContainerMode       = WarpSpeed:SubmitValue
      oCntSubmit:ViewOnly            = FALSE
      oCntSubmit:Collapsable         = FALSE     
      cTypes                         = TRIM(cTypes,"|")
  
      /* Controls  ------------------------- */
      goAuthTypes                    = oCntSubmit:addControl("fcAuthTypes":U,    "wsSelect":U,     "01":U,  "":U,     "character":U, 3, 1, "Authorization Types:":U)
      goAuthTypes:ControlMandatory   = TRUE
      goAuthTypes:AdditionalItems    = "ALL=ALL|":U + cTypes
      goAuthTypes:ControlTabOrder    = 1
  
      goFromAuthDate                 = oCntSubmit:addControl("fcAuthDateFrom":U, "wsInput":U,     "10":U,  "":U,     "date":U,      4, 1, "From Authorization Date:":U)
      goFromAuthDate:ControlValue    = STRING((TODAY - 1), "99/99/9999")
      goFromAuthDate:ControlTabOrder = 2
  
      goToAuthDate                   = oCntSubmit:addControl("fcAuthDateTo":U,   "wsInput":U,     "10":U,  "[2]":U,  "date":U,      5, 1, "To Authorization Date:":U)
      goToAuthDate:ControlValue      = STRING(TODAY,"99/99/9999")
      goToAuthDate:ControlTabOrder   = 3
  
      goAuthStatus                   = oCntSubmit:addControl("fcAuthStatus":U,   "wsSelect":U,    "01":U,  "":U,     "character":U, 6, 1, "Authorization Status:":U)
      goAuthStatus:SelectMultiple    = TRUE
      goAuthStatus:ControlMandatory  = TRUE
      goAuthStatus:AdditionalItems   = "0 - Pending=0|1 - Authorized=1|2 - Assessed=2|3 - Pulled=3|4 - Complete=4|5 - Cancelled=5|6 - Declined=6":U      
      goAuthStatus:ControlTabOrder   = 4
  
      /* Control tooltips ------------------------ */             
      goAuthStatus      :ControlTooltip  = "Enter auth status to extract. Press ctrl for Multiple"
      goFromAuthDate    :ControlTooltip  = "Enter the date from"
      goToAuthDate      :ControlTooltip  = "Enter the date to"
      goAuthTypes       :ControlTooltip  = "Enter auth types to extract"
      NO-ERROR.
      { mip/inc/mipreturnerror.i }
      
  END. /* IF NOT ERROR-STATUS:ERROR */

  ASSIGN 
    pcScript = "":U
    pcCustomFunctions  = "":U
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
