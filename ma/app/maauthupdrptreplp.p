&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    $Id: procedur.p,v 1.2 2008/09/30 12:26:17 chrisk Exp $ 

    Purpose:
  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
{mip/inc/mipdefshared.i}

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
         HEIGHT             = 6.38
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ghLayoutManager = wsUiService:renderProcedureHandle NO-ERROR.

RUN setCurrentProperties IN TARGET-PROCEDURE NO-ERROR.
{ mip/inc/mipreturnerror.i }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-maAuthUpdateReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maAuthUpdateReport Procedure 
PROCEDURE maAuthUpdateReport :
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
  DEFINE OUTPUT PARAMETER pcCustomJavaScript AS CHARACTER          NO-UNDO.
  
  DEFINE VARIABLE cLayoutAttribute      AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cUserRestrict         AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL            NO-UNDO.
  
  DEFINE VARIABLE goAuthNum                       AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goAuthNumLkp                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goAuthNumButton                 AS cls.mipwscontrol   NO-UNDO. 
  DEFINE VARIABLE goAuthYear                      AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goFromAuthDate                  AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goToAuthDate                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goMemNum                        AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goMemNumLkp                     AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goMemNumButton                  AS cls.mipwscontrol   NO-UNDO. 
  DEFINE VARIABLE goUserUpdate                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goUserUpdateLkp                 AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goUserUpdateButton              AS cls.mipwscontrol   NO-UNDO.
  
  oCntSubmit = NEW cls.mipwscontainer(pcContainerCode + "_submit":U, "100%", "":U, WarpSpeed:BaseClass, FALSE) NO-ERROR.
  { mip/inc/mipreturnerror.i }
  
  IF NOT {&errorstatus}
  THEN DO:
  
    ASSIGN
    
      oCntSubmit:ContainerMode            = WarpSpeed:SubmitValue
      oCntSubmit:ViewOnly                 = FALSE
      oCntSubmit:Collapsable              = FALSE 
    
      goMemNumLkp                         = oCntSubmit:addControl("fcMbrNumLkp":U,          "wsInput":U,  "10":U, "":U, "character":U,  1,  1, "Member Number:")
      goMemNumLkp:ControlToken            = "Hidden":U
      goMemNumLkp:CellLayoutMask          = "&1&2&nbsp;&3":U
      
      goMemNum                            = oCntSubmit:addControl("fcMemNum":U,             "wsInput":U, "10":U, "":U, "character":U,  1,  1, "Member Number:")
      goAuthNum                           = oCntSubmit:addControl("fiAuthNum":U,            "wsInput":U, "10":U, "":U, "integer":U,    2,  1, "Authorization No:":U)
      
      goAuthNumLkp                        = oCntSubmit:addControl("fcAuthNumlkp":U,         "wsInput":U, "10":U, "":U, "integer":U,    2,  1, "Authorization No:")
      goAuthNumLkp:ControlToken           = "Hidden":U
      goAuthNumLkp:CellLayoutMask         = "&1&2&nbsp;&3":U
      
      goUserUpdate                        = oCntSubmit:addControl("fcUserCode":U,           "wsInput":U, "10":U, "":U, "character":U,  3,  1, "User Updated:")
      
      goUserUpdateLkp                     = oCntSubmit:addControl("fcUserCodelkp":U,        "wsInput":U, "10":U, "":U, "integer":U,    3,  1, "User Updated:")
      goUserUpdateLkp:ControlToken        = "Hidden":U
      goUserUpdateLkp:CellLayoutMask      = "&1&2&nbsp;&3":U
      
      goFromAuthDate                      = oCntSubmit:addControl("fdtFromDate":U,          "wsInput":U, "10":U, "":U, "date":U,       1,  2, "From Authorization Update Date:":U)
      goFromAuthDate:ControlValue         = STRING((TODAY - 1), "99/99/9999")
      
      goToAuthDate                        = oCntSubmit:addControl("fdtToDate":U,            "wsInput":U, "10":U, "":U, "date":U,       2,  2, "To Authorization Update Date:":U)
      goToAuthDate:ControlValue           = STRING(TODAY, "99/99/9999")
     
      goAuthYear                          = oCntSubmit:addControl("fcAuthYear":U,           "wsCombo":U, "15":U, "":U, "character":U,  3,  2, "Authorization Update Year:":U)
      goAuthYear:DisplayMask              = "&1":U
      goAuthYear:AdditionalItems          = "All=|":U                                   
                                          + STRING(YEAR(TODAY),"9999") + "=Cur|":U         
                                          + STRING(YEAR(TODAY) - 1,"9999") + "=Prev|< ":U  
                                          + STRING(YEAR(TODAY) - 1,"9999") + "=Hist":U
                                                      
      /* Auth Lookup */
      goAuthNumButton                     = oCntSubmit:addControl("buAuthNumButton":U, "wsLookupButton":U, "":U, "":U, "":U, 2, 1)
      goAuthNumButton:LookupWobFLA        = "mauth":U
      goAuthNumButton:LookupControls      = "fcAuthNumlkp,fiAuthNum":U
      goAuthNumButton:LookupFields        = "memauth.auth-num,memauth.auth-num":U
      goAuthNumButton:FilterControls      = "fcMemNum":U
      goAuthNumButton:FilterFields        = "memauth.mem-num":U
      goAuthNumButton:ReturnFields        = "doctor.doc-num,doctor.name":U
      goAuthNumButton:ReturnControls      = "fiDocNum,fcDocName":U
      
      /* Member Lookup */
      goMemNumButton                      = oCntSubmit:addControl("buMemNumButton":U, "wsLookupButton":U, "":U, "":U, "":U, 1, 1)
      goMemNumButton:LookupWobFLA         = "mamem":U
      goMemNumButton:LookupFields         = "memdet.mem-num,memdet.mem-num":U
      goMemNumButton:LookupControls       = "fcMbrNumLkp,fcMemNum":U
      goMemNumButton:FilterFields         = "memdet.mem-num":U
      goMemNumButton:FilterControls       = "fcMemNum":U
      
      /* User Lookup */
      goUserUpdateButton                  = oCntSubmit:addControl("buUserCodeButton":U, "wsLookupButton":U, "":U, "":U, "":U, 3, 1)
      goUserUpdateButton:LookupWobFLA     = "mimus":U
      goUserUpdateButton:LookupFields     = "mim_user.user_obj,mim_user.user_code":U
      goUserUpdateButton:LookupControls   = "fcUserCodelkp,fcUserCode"
      goUserUpdateButton:FilterFields     = "mim_user.user_code"
      goUserUpdateButton:FilterControls   = "fcUserCode"
      
    NO-ERROR.
    {mip/inc/mipreturnerror.i}
  END. /* NOT errorstatus */
  
  ASSIGN 
    pcCustomJavaScript = "":U
    pcCustomFunctions  = "":U
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  IF WarpSpeed:SubmitValue = "[DRILLDOWN]":U
  THEN DO:
    oCntSubmit:populateFromRequest("":U, 1) NO-ERROR.
    { mip/inc/mipreturnerror.i }
    /* If you want your default to override the drilldown request this is where you'll assign it */
  END. /* IF gscSubmit <> "":U */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrentProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCurrentProperties Procedure 
PROCEDURE setCurrentProperties :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

