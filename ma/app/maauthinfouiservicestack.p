&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Auth UI Service stack
    
    Author : MMP

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
USING Progress.Json.ObjectModel.*   FROM PROPATH .

CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation }

{ ma/inc/maauthds.i }

{ ma/inc/maauthtypeds.i }

{ ma/inc/mamemberds.i }

{ ma/inc/maauthtypeconfigtt.i }

{ma/inc/mamemdeptt.i &TEMP-TABLE-NAME = tt_ActiveDep}

DEFINE TEMP-TABLE tt_acronym LIKE mic_acronym.

DEFINE VARIABLE gcOption     AS CHARACTER  NO-UNDO.

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
         HEIGHT             = 9.91
         WIDTH              = 56.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveAuth Procedure 
PROCEDURE ajaxSaveAuth :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  
    { ma/app/maauthinfoajxsaveinfo.i }
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidationAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationAuth Procedure 
PROCEDURE ajaxValidationAuth :
/*------------------------------------------------------------------------------
    Purpose   : Auth Container Ajax Validation    
    Parameters: 
    Notes     :       
------------------------------------------------------------------------------*/
  
    { ma/app/maauthinfoajxsvalinfo.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuth Procedure 
PROCEDURE getCntUpdAuth :
/*------------------------------------------------------------------------------
  Purpose   : Authorisation update container definition   
  Parameters: 
  Notes     :       
  Author    : MMP
------------------------------------------------------------------------------*/
  
  { ma/app/maauthgetcntupdauth.i }
  
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
  Author    : MMP     
------------------------------------------------------------------------------*/
  
  { ma/app/maauthinforenderprocedure.i }
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_getConfiguration) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _getConfiguration Procedure 
PROCEDURE _getConfiguration :
/*------------------------------------------------------------------------------
  Purpose   : Returns common authorisation configuration object    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthTypeObj    AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj     AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdStartDate      AS DATE       NO-UNDO.
  DEFINE OUTPUT PARAMETER oplcConfiguration AS LONGCHAR   NO-UNDO.
  
  DEFINE VARIABLE oJsonArray                AS JsonArray            NO-UNDO.
  DEFINE VARIABLE oJsonObject               AS JsonObject           NO-UNDO.
  DEFINE VARIABLE oJsonParser               AS ObjectModelParser    NO-UNDO.
  DEFINE VARIABLE lSuccess                  AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValid                    AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lRefNumUpdatable          AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lNOKUpdatable             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lNotePerAuthStatus        AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lServiceTypeUpdatable     AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cRuleValue                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE lDependantUpdatable       AS LOGICAL              NO-UNDO.
  

  EMPTY TEMP-TABLE ttAuthTypeConfig.
  
  IF ipdAuthTypeObj <> 0.00 THEN
  DO:
  
    mipEnv:Health:AuthService:getAuthTypeConfig(INPUT ipdAuthTypeObj, 
                                                INPUT ipdInsurerObj,
                                                INPUT ipiOptionCode,
                                                INPUT ipdStartDate,
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).
  
    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
   
    {&ResetError}
    
    IF AVAILABLE ttAuthTypeConfig THEN
    DO:

      IF ttAuthTypeConfig.ActivateServiceType 
      THEN
        ASSIGN lSuccess  = mipEnv:Health:AuthBusinessLogic:authServiceTypeUpdatable
                                        (INPUT  ipdInsurerObj, 
                                         INPUT  ipiOptionCode, 
                                         INPUT  ipdStartDate,
                                         OUTPUT lServiceTypeUpdatable).
      ELSE
        ASSIGN lServiceTypeUpdatable = FALSE.

    END. /* IF AVAILABLE ttAuthTypeConfig */

    /* UTF encoding for the parser */
    FIX-CODEPAGE(oplcConfiguration) = "UTF-8":U.

    TEMP-TABLE ttAuthTypeConfig:WRITE-JSON("LONGCHAR":U, oplcConfiguration, FALSE, "UTF-8":U, FALSE, TRUE).

    ASSIGN lSuccess           = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                  (INPUT  ipdInsurerObj,
                                   INPUT  ipiOptionCode,
                                   INPUT  "ma_acAuthRuleTypeAUTHSETUPS":U,
                                   INPUT  "NotePerAuthStatus":U,
                                   INPUT  ipdStartDate,
                                   OUTPUT lValid,
                                   OUTPUT cRuleValue)

           lNotePerAuthStatus = lValid AND CAN-DO("Y,YES,T,TRUE":U, TRIM(cRuleValue))

           lRefNumUpdatable   = mipEnv:Health:AuthBusinessLogic:authRefNumUpdatable
                                  (INPUT ipdAuthTypeObj, 
                                   INPUT ipdInsurerObj, 
                                   INPUT ipiOptionCode, 
                                   INPUT ipdStartDate)

           lNOKUpdatable      = mipEnv:Health:AuthBusinessLogic:authNextOfKinActivated
                                 (INPUT ipdInsurerObj, 
                                  INPUT ipiOptionCode, 
                                  INPUT ipdStartDate)

           oJsonParser        = NEW ObjectModelParser()
        
          lDependantUpdatable = mipEnv:Health:AuthBusinessLogic:authDependantUpdatable
                                 (INPUT ipdInsurerObj, 
                                  INPUT ipdAuthTypeObj, 
                                  INPUT ipiOptionCode, 
                                  INPUT ipdStartDate, 
                                  INPUT 0, 
                                  INPUT 0).

    /*
      Create a combined configuration json object
    */
    oJsonArray  = CAST(oJsonParser:Parse(oplcConfiguration), JsonArray).

    oJsonObject = oJsonArray:GetJsonObject(1).
    
    oJsonObject:ADD("RefNumUpdatable":U  ,    lRefNumUpdatable).
    oJsonObject:ADD("ActivateNextOfKin":U,    lNOKUpdatable).
    oJsonObject:ADD("NotePerAuthStatus":U,    lNotePerAuthStatus).
    oJsonObject:ADD("ServiceTypeUpdatable":U, lServiceTypeUpdatable).
    oJsonObject:ADD("DependantUpdatable":U,   lDependantUpdatable).
    .
    
    oJsonObject:WRITE(oplcConfiguration).
  END. /*IF ipdAuthTypeObj <> 0.00 THEN*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "EMPTY TEMP-TABLE ttAuthTypeConfig."}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


