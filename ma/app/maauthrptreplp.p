&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Source      : ma/app/maexusreplp.p:Definitions
  Description : WebFormDefinition for External User registration
                This Manager should be used to hold all the Layout definitions
                regularly used in the application.
------------------------------------------------------------------------------*/
{ mip/inc/mipdefshared.i}

DEFINE VARIABLE ghLayoutManager             AS HANDLE     NO-UNDO.

/* end of Definitions */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-customJavaScript) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD customJavaScript Procedure 
FUNCTION customJavaScript RETURNS CHARACTER
  ( INPUT pcSubmitViewer AS CHARACTER )  FORWARD.

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
         HEIGHT             = 13.38
         WIDTH              = 50.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/*------------------------------------------------------------------------------
  Source      : ws/app/wslayoutmngrp.p:Main Block
  Description : The WarpSpeed Layout Manager
                This Manager should be used to hold all the Layout definitions
                regularly used in the application.
------------------------------------------------------------------------------*/
  
ghLayoutManager = wsUiService:renderProcedureHandle NO-ERROR.

RUN setCurrentProperties IN TARGET-PROCEDURE NO-ERROR.
{ mip/inc/mipreturnerror.i }

/* end of ws/app/wslayoutmngrp.p:Main Block */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-maAuthReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maAuthReport Procedure 
PROCEDURE maAuthReport :
/*------------------------------------------------------------------------------
  Purpose:     External User Registration Report
  Parameters:  INPUT  pcContainerCode   
               INPUT  pcExtractCode     
               OUTPUT oCntSubmit        
               OUTPUT pcCustomFunctions 
               OUTPUT pcCustomJavaScript
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
  DEFINE VARIABLE goDocNum                        AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goDocNumLkp                     AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goDocName                       AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goAuthYear                      AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goDependant                     AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goFromAuthDate                  AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goToAuthDate                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goStatus                        AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goPrimDiag                      AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goNAPPICode                     AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goFromExpDate                   AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goToExpDate                     AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goRefAuthNum                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goExternalRef                   AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goAuthType                      AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goMemNum                        AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goAuthNumButton                 AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goDocNumButton                  AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goDiagButton                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goMemNumButton                  AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goPrimDiagLkp                   AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goMemNumLkp                     AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goStatReason                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goMoreDetail                    AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE goAuthDetail                    AS cls.mipwscontrol   NO-UNDO.

  oCntSubmit = NEW cls.mipwscontainer(pcContainerCode + "_submit":U, "100%", "":U, WarpSpeed:BaseClass, FALSE) NO-ERROR.
  { mip/inc/mipreturnerror.i }
  
  IF NOT ERROR-STATUS:ERROR
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

      goFromAuthDate                      = oCntSubmit:addControl("fdtFromDate":U,          "wsInput":U, "10":U, "":U, "date":U,       3,  1, "From Authorization Date:":U)
      goFromAuthDate:ControlValue         = STRING((TODAY - 1), "99/99/9999")

      goToAuthDate                        = oCntSubmit:addControl("fdtToDate":U,            "wsInput":U, "10":U, "":U, "date":U,       4,  1, "To Authorization Date:":U)
      goToAuthDate:ControlValue           = STRING(TODAY, "99/99/9999")                                                                                                                          

      goDocNum                            = oCntSubmit:addControl("fiDocNum":U,             "wsInput":U, "7":U,  "":U, "integer":U,    5,  1, "Service Provider Number:":U)
      goDocNumLkp                         = oCntSubmit:addControl("fcDocNumlkp":U,          "wsInput":U, "10":U, "":U, "character":U,  5,  1, "Service Provider Number:":U)
      goDocNumLkp:ControlToken            = "Hidden":U                                                                                     
      goDocNumLkp:CellLayoutMask          = "&1&2&nbsp;&3":U                                                                               
                                                                                                                                           
      goDocName                           = oCntSubmit:addControl("fcDocName":U,            "wsInput":U, "30":U, "":U, "character":U,  6,  1, "Service Provider Name:":U)
                                                                                                                                           
      goAuthYear                          = oCntSubmit:addControl("fcAuthYear":U,           "wsCombo":U, "15":U, "":U, "character":U,  7,  1, "Authorization Year:":U)
      goAuthYear:DisplayMask              = "&1":U
      goAuthYear:AdditionalItems          = "All=|":U                                   
                                          + STRING(YEAR(TODAY),"9999") + "=Cur|":U         
                                          + STRING(YEAR(TODAY) - 1,"9999") + "=Prev|< ":U  
                                          + STRING(YEAR(TODAY) - 1,"9999") + "=Hist":U    

      goExternalRef                       = oCntSubmit:addControl("fcExternalRef":U,        "wsInput":U, "10":U                     , "":U, "character":U, 8, 1, "External Reference:":U)
      goNappiCode                         = oCntSubmit:addControl("fcNappiCode":U,          "wsInput":U, {ma/inc/manappiwebformat.i}, "":U, "character":U, 9, 1, "Nappi Code:":U)

      goDependant                         = oCntSubmit:addControl("fcDependant":U,          "wsCombo":U, "30":U, "":U, "character":U,   1, 2, "Dependant:":U)
      goDependant:AdditionalItems         = "All=|Main Member=00|First=01|Second=02|Third=03|Four=04|Five=05|Six=06|Seven=07|Eight=08|Nine=09|Ten=10|Eleven=11|Twelve=12|Thirteen=13|Fourteen=14|Fifteen=15":U

      goRefAuthNum                        = oCntSubmit:addControl("fcRefAuthNum":U,         "wsInput":U, "10":U, "":U, "character":U,   2, 2, "Reference Auth Number:":U)
      goFromExpDate                       = oCntSubmit:addControl("fcFromExpDate":U,        "wsInput":U, "10":U, "":U, "date":U,        3, 2, "From Expected Date:":U)
      goToExpDate                         = oCntSubmit:addControl("fcToExpDate":U,          "wsInput":U, "10":U, "":U, "date":U,        4, 2, "From Expected Date:":U)
                                                                                                                                        
      goStatus                            = oCntSubmit:addControl("fcStatus":U,             "wsCombo":U, "20":U, "":U, "character":U,   5, 2, "Status:":U)
      goStatus:DisplayMask                = "&1":U                                                                                      
      goStatus:AdditionalItems            = "All=7|Pending=0|Authorized=1|Assessed=2|Pulled=3|Complete=4|Cancelled=5|Declined=6":U      
                                                                                                                                        
      goPrimDiag                          = oCntSubmit:addControl("fcPrimDiag":U,           "wsInput":U, "10":U, "":U, "character":U,   6, 2, "Primary Diagnosis:":U)
      goPrimDiagLkp                       = oCntSubmit:addControl("fcPrDiaglkp":U,          "wsInput":U, "10":U, "":U, "character":U,   6, 2, "Primary Diagnosis:":U)
      goPrimDiagLkp:ControlToken          = "Hidden":U                                                                                  
      goPrimDiagLkp:CellLayoutMask        = "&1&2&nbsp;&3":U                                                                            
                                                                                                                                        
      goAuthType                          = oCntSubmit:addControl("fcAuthType":U,           "wsCombo":U, "10":U, "":U, "character":U,   7, 2, "Auth. Type:":U)
      goAuthType:KeyField                 = "datalist.list-code":U                                                                      
      goAuthType:QueryString              = "FOR EACH datalist NO-LOCK WHERE datalist.list-type = 'MCATYPES'"                           
      goAuthType:DisplayFields            = "datalist.list-code,datalist.description":U                                                 
      goAuthType:DisplayMask              = "&1 - &2":U                                                                                 
      goAuthType:AdditionalItems          = "All=":U                                                                                  
                                                                                                                                        
      goStatReason                        = oCntSubmit:addControl("fcStatReason":U,         "wsCombo":U, "10":U, "":U, "character":U,   8, 2, "Status Reason:":U)
      goStatReason:KeyField               = "note.key":U                                                                      
      goStatReason:QueryString            = "FOR EACH note NO-LOCK WHERE note.scheme-code = 00 and note.type = 'AS'":U
      goStatReason:DisplayFields          = "note.key,note.narration[1]":U                                                 
      goStatReason:DisplayMask            = "&1 - &2":U                                                                                 
      goStatReason:AdditionalItems        = "All=":U       
      
      goMoreDetail                        = oCntSubmit:addControl("fcMoreDetails":U,     "wsCheckBox":U, "5":U,  "":U, "character":U,   9, 2,  "More Details:":U)
      
      goAuthDetail                        = oCntSubmit:addControl("fcAuthDetails":U,     "wsCheckBox":U, "5":U,  "":U, "character":U,   10, 2, "Display Auth Details:":U)

      /*Lookups*/
      /*Auth Lookup*/
      goAuthNumButton                     = oCntSubmit:addControl("buAuthNumButton":U, "wsLookupButton":U, "":U, "":U, "":U, 2, 1)
      goAuthNumButton:LookupWobFLA        = "mauth":U
      goAuthNumButton:LookupControls      = "fcAuthNumlkp,fiAuthNum":U
      goAuthNumButton:LookupFields        = "memauth.auth-num,memauth.auth-num":U
      goAuthNumButton:FilterControls      = "fcMemNum":U
      goAuthNumButton:FilterFields        = "memauth.mem-num":U
      goAuthNumButton:ReturnFields        = "doctor.doc-num,doctor.name":U
      goAuthNumButton:ReturnControls      = "fiDocNum,fcDocName":U

      /*Doctor Lookup*/
      goDocNumButton                      = oCntSubmit:addControl("buDocNumButton":U, "wsLookupButton":U, "":U, "":U, "":U, 5, 1)
      goDocNumButton:LookupWobFLA         = "madoc":U
      goDocNumButton:LookupControls       = "fcDocNumlkp,fiDocNum":U
      goDocNumButton:LookupFields         = "doctor.doc-num,doctor.doc-num":U

      /*Diagnosis Lookup*/
      goDiagButton                        = oCntSubmit:addControl("buDiagButton":U, "wsLookupButton":U, "":U, "":U, "":U, 6, 2)
      goDiagButton:LookupWobFLA           = "madia":U
      goDiagButton:LookupControls         = "fcPrDiaglkp,fcPrimDiag":U
      goDiagButton:LookupFields           = "diagnos.diagnosis,diagnos.diagnosis":U
      
      /*Member Lookup*/
      goMemNumButton                      = oCntSubmit:addControl("buMemNumButton":U, "wsLookupButton":U, "":U, "":U, "":U, 1, 1)
      goMemNumButton:LookupWobFLA         = "mamem":U
      goMemNumButton:LookupFields         = "memdet.mem-num,memdet.mem-num":U
      goMemNumButton:LookupControls       = "fcMbrNumLkp,fcMemNum":U
      goMemNumButton:FilterFields         = "memdet.mem-num":U
      goMemNumButton:FilterControls       = "fcMemNum":U
      NO-ERROR.
      { mip/inc/mipreturnerror.i }
  END.

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

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-customJavaScript) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION customJavaScript Procedure 
FUNCTION customJavaScript RETURNS CHARACTER
  ( INPUT pcSubmitViewer AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cJavaScript   AS CHARACTER    NO-UNDO.

  /* First do the Inner Section - Case Statements which will be used in the JS */
  CASE pcSubmitViewer:
    WHEN "quAnswerSheetReport":U
    THEN DO:
    END.
  END CASE. /* CASE pcSubmitViewer */

  RETURN cJavaScript.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

