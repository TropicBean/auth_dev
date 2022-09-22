&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    $Id: maauthcopayservicestack.p       Exp $

    Purpose: Stack procedure for Auth Copay Class

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthcopayds.i}

&SCOPED-DEFINE AddAction         Add
&SCOPED-DEFINE AddErrorAction    AddError
&SCOPED-DEFINE UpdateAction      Update
&SCOPED-DEFINE UpdateErrorAction UpdateError
&SCOPED-DEFINE DeleteAction      Delete

{mip/inc/miptemptables.i &TempTableName = ttDsCriteria }

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
         HEIGHT             = 19.76
         WIDTH              = 57.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-copyAuthCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyAuthCopay Procedure 
PROCEDURE copyAuthCopay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters: INPUT iplCopyDetail,INPUT-OUTPUT DATASET dsAuthCopayControl 
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER  iplCopyDetails AS LOGICAL.
 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthCopayControl.

&IF {&DBDFMA} >= 10195 &THEN
 
  RUN _saveAuthCopayControl  IN TARGET-PROCEDURE.

  IF iplCopyDetails THEN
  DO:

    /*
      We need to change the copay_control_obj field to link it to the  newly created copay_control record,
      Change the  auth_copay_detail_obj to create a new detail record
    */
    FIND FIRST tt_auth_copay_control NO-LOCK
      NO-ERROR.
      
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
    
    FOR EACH tt_auth_copay_detail EXCLUSIVE-LOCK:
  
      ASSIGN tt_auth_copay_detail.auth_copay_control_obj = tt_auth_copay_control.auth_copay_control_obj
             tt_auth_copay_detail.auth_copay_detail_obj  = tt_auth_copay_detail.auth_copay_detail_obj * -1
             tt_auth_copay_detail.effective_date         = tt_auth_copay_control.effective_date    
             tt_auth_copay_detail.end_date               = tt_auth_copay_control.end_date     
             tt_auth_copay_detail.record_action          = "Modify" NO-ERROR.
    
    END.  // FOR EACH tt_auth_copay_detail EXCLUSIVE-LOCK:
    
    RUN _saveAuthCopayDetail   IN TARGET-PROCEDURE.
  END.  // IF iplCopyDetails THEN

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    FIND FIRST tt_auth_copay_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE tt_auth_copay_result
    THEN 
      CREATE tt_auth_copay_result.

    FOR EACH tt_auth_copay_error NO-LOCK:
      ASSIGN 
        tt_auth_copay_result.number_of_errors = tt_auth_copay_result.number_of_errors + 1.
    END. // FOR EACH tt_auth_copay_error NO-LOCK:
  
  END. // DO TRANSACTION ON ERROR UNDO, THROW:

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fetchAuthCopayDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthCopayDataSet Procedure 
PROCEDURE fetchAuthCopayDataSet :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Auth Copay Data
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
  DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
  DEFINE INPUT        PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lSuccess                   AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER               NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle AS HANDLE                NO-UNDO.
  DEFINE VARIABLE hDatasetHandle             AS HANDLE                NO-UNDO.
  DEFINE VARIABLE hBuffer                    AS HANDLE                NO-UNDO.
  DEFINE VARIABLE hAuthCopayControl          AS HANDLE                NO-UNDO.
  DEFINE VARIABLE hAuthCopayDetail           AS HANDLE                NO-UNDO.
  DEFINE VARIABLE hAuthCopayError            AS HANDLE                NO-UNDO.
  DEFINE VARIABLE hAuthCopayResult           AS HANDLE                NO-UNDO.
  DEFINE VARIABLE oSearchObject              AS cls.maauthcopaysearch NO-UNDO.

  DEFINE DATA-SOURCE srcAuthCopayControl FOR hac_auth_copay_control.
  DEFINE DATA-SOURCE srcAuthCopayDetail  FOR hac_auth_copay_detail.

  /*copy the Criteria Table from the input parameter table handle*/
  TEMP-TABLE ttDsCriteria:HANDLE:COPY-TEMP-TABLE ( iophFilterCriteriaTableHandle,false , ? , ?  , ?).

  ASSIGN
    oSearchObject              = NEW cls.maAuthCopaySearch(INPUT DATASET-HANDLE iophDatasetHandle)
    hFilterCriteriaTableHandle = iophFilterCriteriaTableHandle
    hDatasetHandle             = iophDatasetHandle.

  ASSIGN                           
    hAuthCopayControl = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_control":U)
    hAuthCopayDetail  = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_detail":U)
    hAuthCopayError   = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_error":U)
    hAuthCopayResult  = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_result":U).

  ASSIGN                                 
    lSuccess = hAuthCopayControl:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCopayControl:HANDLE).

  /*find criteria set for child table tt_auth_copay_detail*/
  FIND FIRST ttDsCriteria NO-LOCK 
       WHERE ttDsCriteria.cCriteriaType = "BatchSize":U
       AND   ttDsCriteria.cBufferName   = "tt_auth_copay_detail":U
    NO-ERROR.
  
  IF AVAILABLE ttDsCriteria 
  THEN
    ASSIGN 
      oSearchObject:AuthCopayDetailsBatchSize = INTEGER(ttDsCriteria.vCharacter)
      hAuthCopayDetail:BATCH-SIZE             = oSearchObject:AuthCopayDetailsBatchSize 
     NO-ERROR.
  
  ASSIGN                                 
    lSuccess = hAuthCopayDetail:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCopayDetail:HANDLE).

  ASSIGN
    hAuthCopayError:FILL-MODE  = "NO-FILL":U
    hAuthCopayResult:FILL-MODE = "NO-FILL":U.
 
  ASSIGN
    lSuccess                       = oSearchObject:populateDataset(INPUT iophFilterCriteriaTableHandle)
    iophDatasetHandle              = oSearchObject:DatasetHandle
    iophFilterCriteriaTableHandle  = oSearchObject:CriteriaTableHandle.  
  
  {mip/inc/mipcatcherror.i
     &FORMAT  = TRUE
     &FINALLY = "IF VALID-HANDLE(hFilterCriteriaTableHandle)     THEN DELETE OBJECT hFilterCriteriaTableHandle.
                 IF VALID-HANDLE(hDatasetHandle)                 THEN DELETE OBJECT hDatasetHandle.
                 IF VALID-OBJECT(oSearchObject)                  THEN DELETE OBJECT oSearchObject.
                 IF VALID-HANDLE(iophFilterCriteriaTableHandle)  THEN DELETE OBJECT iophFilterCriteriaTableHandle.
                 IF VALID-HANDLE(iophDatasetHandle)              THEN DELETE OBJECT iophDatasetHandle.
                "}
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthCopay Procedure 
PROCEDURE saveAuthCopay :
/*------------------------------------------------------------------------------
  Purpose   : Save Auth Copay from dataset
  Parameters: dsAuthCopayControl as defined in maAuthCopaycontrolds.i
  Notes     : - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthCopayControl.

&IF {&DBDFMA} >= 10195 &THEN

  RUN _saveAuthCopayControl  IN TARGET-PROCEDURE.

  RUN _saveAuthCopayDetail   IN TARGET-PROCEDURE.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    FIND FIRST tt_auth_copay_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE tt_auth_copay_result
    THEN 
      CREATE tt_auth_copay_result.

    FOR EACH tt_auth_copay_error NO-LOCK:
      ASSIGN 
        tt_auth_copay_result.number_of_errors = tt_auth_copay_result.number_of_errors + 1.
    END. /* FOR EACH tt_auth_copay_error NO-LOCK:  */
  
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateCopayAuthStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateCopayAuthStatus Procedure 
PROCEDURE validateCopayAuthStatus :
/*------------------------------------------------------------------------------
  Purpose:     Validate auth status on co-payment
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE INPUT  PARAMETER ipiCopayStatusCode      AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER oplValid                AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cRuleValue  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cValidList  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cStatusCode AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iEntry      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lSuccess    AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule  AS LOGICAL     NO-UNDO.

  ASSIGN oplValid = FALSE.

  ASSIGN lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                        (INPUT  0,
                         INPUT  0,
                         INPUT  "ma_acAuthRuleTypeAuthSetups":U,
                         INPUT  "CopayValidStatuses":U,
                         INPUT  TODAY,
                         OUTPUT lValidRule,
                         OUTPUT cRuleValue).
  IF NOT lValidRule THEN
    RETURN.

  /*
     The rule value will contain a comma delimited list of the status codes 
     and descriptions that are valid and allowed.
     We need to build a list without the descriptions.
  */
  DO iEntry = 1 TO NUM-ENTRIES(cRuleValue):
    ASSIGN cStatusCode = TRIM(ENTRY(iEntry,cRuleValue))
           cStatusCode = SUBSTRING(cStatusCode,1,1)
           cValidList  = cValidList + ",":U + cStatusCode.
  END.  /* DO iEntry = 1 TO NUM-ENTRIES(cRuleValue): */
  ASSIGN cValidList = SUBSTRING(cValidList,2).

  IF LOOKUP(STRING(ipiCopayStatusCode),cValidList) <> 0 
  THEN
    ASSIGN oplValid = TRUE.

&ENDIF

{mip/inc/mipcatcherror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCopayControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCopayControl Procedure 
PROCEDURE _deleteAuthCopayControl PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Remove Auth copay control record
  Parameters: Auth copay control obj
  Notes     : - This will run the procedures to remove any
                dependency records as well.
              - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthCopayControlObj   AS DECIMAL            NO-UNDO.

  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject  NO-UNDO.
  DEFINE VARIABLE lFailureOccurred        AS LOGICAL            NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_copay_result   FOR tt_auth_copay_result.
  DEFINE BUFFER btt_auth_copay_error    FOR tt_auth_copay_error.
  DEFINE BUFFER buf_auth_copay_control  FOR hac_auth_copay_control.
  DEFINE BUFFER buf_auth_copay_detail   FOR hac_auth_copay_detail.
  
  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).

    FIND FIRST btt_auth_copay_result EXCLUSIVE-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_copay_result
    THEN 
      CREATE btt_auth_copay_result.

    /* 
       Make sure no dependencies remain 
    */
    IF CAN-FIND(FIRST hat_auth_provider NO-LOCK
            WHERE hat_auth_provider.auth_copay_control_obj = ipdAuthCopayControlObj) THEN
    DO:
      ASSIGN lFailureOccurred = TRUE.
       oErrorObject:addError(INPUT "haccc":U, 
                              INPUT ipdAuthCopayControlObj,
                              INPUT "":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                              INPUT "Auth Copay Control,Authorisation":U).

    END. /* IF CAN-FIND(FIRST hat_auth_provider */
    ELSE DO:
      IF CAN-FIND(FIRST buf_auth_copay_detail NO-LOCK
                  WHERE buf_auth_copay_detail.auth_copay_control_obj = ipdAuthCopayControlObj)
      THEN DO:            
        ASSIGN lFailureOccurred = TRUE.
        
       oErrorObject:addError(INPUT "haccc":U, 
                              INPUT ipdAuthCopayControlObj,
                              INPUT "":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                              INPUT "Auth Copay Control,Auth Copay Detail":U).
      END. /* IF CAN-FIND(FIRST buf_auth_copay_detail NO-LOCK */
    END. /* ELSE DO: IF CAN-FIND(FIRST hat_auth_provider */

    IF NOT lFailureOccurred THEN
    DO:
      FIND FIRST buf_auth_copay_control EXCLUSIVE-LOCK
           WHERE buf_auth_copay_control.auth_copay_control_obj = ipdAuthCopayControlObj
        NO-ERROR NO-WAIT.
     
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
     
      /* Unable to remove - record is already locked */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        cls.miperror:resetError().
     
        ASSIGN lFailureOccurred = TRUE.
     
        oErrorObject:addError(INPUT "haccc":U,
                              INPUT ipdAuthCopayControlObj,
                              INPUT "":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Auth Copay Control":U).
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
     
      IF AVAILABLE buf_auth_copay_control AND NOT lFailureOccurred THEN
      DO:
        DELETE buf_auth_copay_control.
     
        ASSIGN btt_auth_copay_result.records_removed = btt_auth_copay_result.records_removed + 1.
     
      END. /* IF AVAILABLE buf_auth_copay_control AND NOT lFailureOccurred THEN  */
    END. /* IF NOT lFailureOccurred THEN */
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

&ENDIF

  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCopayDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCopayDetail Procedure 
PROCEDURE _deleteAuthCopayDetail PRIVATE :
/*------------------------------------------------------------------------------
  Purpose    : Remove Auth Rate Detail record     
  Parameters : Auth Rate Detail Obj
  Notes      : - This will run the procedures to remove any
                 dependency records as well.
               - Delta 010195
               - MIP catch error handling used { mip/inc/mipcatcherror.i }               
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthCopayDetailObj AS DECIMAL           NO-UNDO.

  DEFINE VARIABLE oErrorObject                 AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred             AS LOGICAL           NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_copay_result     FOR tt_auth_copay_result.
  DEFINE BUFFER btt_auth_copay_error      FOR tt_auth_copay_error.
  DEFINE BUFFER buf_auth_copay_detail     FOR hac_auth_copay_detail.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).
    
    FIND FIRST btt_auth_copay_result NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_copay_result 
    THEN
      CREATE btt_auth_copay_result.
      
    FIND FIRST buf_auth_copay_detail EXCLUSIVE-LOCK
         WHERE buf_auth_copay_detail.auth_copay_detail_obj = ipdAuthCopayDetailObj
      NO-ERROR NO-WAIT.   
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
    /* Unable to remove - record is already locked */  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().
    
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "haccd":U, 
                            INPUT ipdAuthCopayDetailObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Copay Detail":U ). 
    END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN */
    
    IF AVAILABLE buf_auth_copay_detail AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_copay_detail.
      ASSIGN btt_auth_copay_result.records_removed = btt_auth_copay_result.records_removed + 1.
    END. /* IF AVAILABLE buf_auth_copay_detail AND NOT lFailureOccurred THEN */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

  { mip/inc/mipcatcherror.i 
    &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthCopayControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthCopayControl Procedure 
PROCEDURE _saveAuthCopayControl PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete Auth Copay control information
  Parameters:
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN
  
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE cErrorMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.
  
  DEFINE BUFFER buf_auth_copay_control FOR hac_auth_copay_control.
  DEFINE BUFFER btt_auth_copay_control FOR tt_auth_copay_control.
  DEFINE BUFFER btt_auth_copay_result  FOR tt_auth_copay_result.
  DEFINE BUFFER btt_auth_copay_error   FOR tt_auth_copay_error.
  DEFINE BUFFER btt_auth_copay_detail  FOR tt_auth_copay_detail.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
    ASSIGN oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).

    FIND FIRST btt_auth_copay_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE btt_auth_copay_result
    THEN 
      CREATE btt_auth_copay_result.

    RECORD-BLK:
    FOR EACH btt_auth_copay_control EXCLUSIVE-LOCK
          BY btt_auth_copay_control.auth_copay_control_obj DESCENDING:

      ASSIGN
         lFailureOccurred = FALSE
         btt_auth_copay_result.records_processed = btt_auth_copay_result.records_processed + 1.

      IF btt_auth_copay_control.record_action = "MODIFY":U THEN
      DO:
        /* 
          Auth Copay Control Validations 
        */
        RUN _validateAuthCopayControl IN TARGET-PROCEDURE (BUFFER btt_auth_copay_control, INPUT-OUTPUT lFailureOccurred ).

        /* 
          Duplicate check 
        */
        ASSIGN cErrorMessage = "".
        FIND FIRST buf_auth_copay_control NO-LOCK
             WHERE buf_auth_copay_control.insurer_obj             = btt_auth_copay_control.insurer_obj
               AND buf_auth_copay_control.option_code             = btt_auth_copay_control.option_code
               AND buf_auth_copay_control.provider_type           = btt_auth_copay_control.provider_type 
               AND buf_auth_copay_control.effective_date          = btt_auth_copay_control.effective_date
               AND buf_auth_copay_control.auth_copay_control_obj <> btt_auth_copay_control.auth_copay_control_obj
          NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE} 

        IF AVAILABLE buf_auth_copay_control THEN
        DO:
          FIND ham_auth_copay_type NO-LOCK
            WHERE ham_auth_copay_type.auth_copay_type_obj = btt_auth_copay_control.auth_copay_type_obj
            NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE}

          IF AVAILABLE ham_auth_copay_type
          THEN
            ASSIGN cErrorMessage = "Auth copay control,":U 
                             + "Option: ":U          + STRING(btt_auth_copay_control.option_code)  + CHR(10)            
                             + "Auth Copay Type: ":U + STRING(ham_auth_copay_type.auth_copay_type) + CHR(10) 
                             + "Effective Date: ":U  + STRING(btt_auth_copay_control.effective_date,"9999/99/99").
          ELSE
            ASSIGN cErrorMessage = "Auth copay control,":U 
                             + "Option: ":U          + STRING(btt_auth_copay_control.option_code)         + CHR(10)            
                             + "Auth Copay Type: ":U + STRING(btt_auth_copay_control.auth_copay_type_obj) + CHR(10) 
                             + "Effective Date: ":U  + STRING(btt_auth_copay_control.effective_date,"9999/99/99").
        END.  // IF AVAILABLE buf_auth_copay_control THEN

        IF NOT AVAILABLE buf_auth_copay_control THEN
        DO:
          FIND FIRST buf_auth_copay_control NO-LOCK
             WHERE buf_auth_copay_control.insurer_obj             = btt_auth_copay_control.insurer_obj
             AND   buf_auth_copay_control.option_code             = btt_auth_copay_control.option_code
             AND   buf_auth_copay_control.provider_type           = btt_auth_copay_control.provider_type
             AND   buf_auth_copay_control.end_date                = btt_auth_copay_control.end_date
             AND   buf_auth_copay_control.auth_copay_control_obj <> btt_auth_copay_control.auth_copay_control_obj
          NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE} 

          IF AVAILABLE buf_auth_copay_control THEN
          DO:
            FIND ham_auth_copay_type NO-LOCK
              WHERE ham_auth_copay_type.auth_copay_type_obj = btt_auth_copay_control.auth_copay_type_obj
              NO-ERROR.
  
            {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE}
  
            IF AVAILABLE ham_auth_copay_type
            THEN
              ASSIGN cErrorMessage = "Auth copay control,":U 
                               + "Option: ":U          + STRING(btt_auth_copay_control.option_code)  + CHR(10)            
                               + "Co-payment Type: ":U + STRING(ham_auth_copay_type.auth_copay_type) + CHR(10)
                               + "End Date: ":U.
            ELSE
              ASSIGN cErrorMessage = "Auth copay control,":U 
                               + "Option: ":U          + STRING(btt_auth_copay_control.option_code)         + CHR(10)            
                               + "Co-payment Type: ":U + STRING(btt_auth_copay_control.auth_copay_type_obj) + CHR(10)
                               + "End Date: ":U.
            IF btt_auth_copay_control.end_date <> ?
            THEN
              ASSIGN cErrorMessage = cErrorMessage + STRING(btt_auth_copay_control.end_date,"9999/99/99").
          END.  // IF AVAILABLE buf_auth_copay_control THEN
        END. //  IF NOT AVAILABLE buf_auth_copay_control

        IF AVAILABLE buf_auth_copay_control 
        AND cErrorMessage <> "" THEN
        DO: 
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "haccc":U,
                                INPUT btt_auth_copay_control.auth_copay_control_obj,
                                INPUT "":U,
                                INPUT btt_auth_copay_control.line_number,
                                INPUT "MA":U,
                                INPUT 125,  /* &1 already exists with &2 */
                                INPUT cErrorMessage).
        END. // IF AVAILABLE buf_auth_copay_control
        ELSE DO:
          FIND buf_auth_copay_control NO-LOCK
             WHERE buf_auth_copay_control.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj
          NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE} 

          IF AVAILABLE buf_auth_copay_control
          THEN
            RUN _setAuthCopayControlDates IN TARGET-PROCEDURE(BUFFER btt_auth_copay_control,
                                                              BUFFER buf_auth_copay_control,
                                                              INPUT-OUTPUT oErrorObject,
                                                              INPUT-OUTPUT lFailureOccurred).
        END.  // ELSE - IF AVAILABLE buf_auth_copay_control

        IF lFailureOccurred
        THEN
          NEXT RECORD-BLK.

        FIND FIRST buf_auth_copay_control EXCLUSIVE-LOCK
             WHERE buf_auth_copay_control.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj
          NO-ERROR NO-WAIT.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}

        /* 
          The auth rate control record is locked by another user or process 
        */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().

          ASSIGN
             lFailureOccurred = TRUE
             btt_auth_copay_result.records_locked = btt_auth_copay_result.records_locked + 1.

          oErrorObject:addError(INPUT "haccc":U, 
                                INPUT btt_auth_copay_control.auth_copay_control_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_copay_control.line_number, 
                                INPUT "MA":U, 
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth Copay Control: ":U + STRING(btt_auth_copay_control.auth_copay_type_obj)).

        END. // IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
        
        /* 
          Record not found so we are creating 
        */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().

          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_copay_control.
        END. // IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        
        IF AVAILABLE buf_auth_copay_control AND NOT lFailureOccurred THEN
        DO:
          /* 
            An existing record is being updated
          */
          IF btt_auth_copay_control.auth_copay_control_obj <= 0
          THEN 
            ASSIGN btt_auth_copay_result.records_created  = btt_auth_copay_result.records_created  + 1.
          ELSE 
            ASSIGN btt_auth_copay_result.records_modified = btt_auth_copay_result.records_modified + 1.
        
          BUFFER-COPY btt_auth_copay_control
               EXCEPT btt_auth_copay_control.auth_copay_control_obj
                   TO buf_auth_copay_control.

          /* 
            If we are creating a new record we need to make sure that we run through all the dependants
            and assign the correct _obj 
          */
          IF btt_auth_copay_control.auth_copay_control_obj < 1 THEN
          DO:
            FOR EACH btt_auth_copay_detail EXCLUSIVE-LOCK
               WHERE btt_auth_copay_detail.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj:
              ASSIGN btt_auth_copay_detail.auth_copay_control_obj = buf_auth_copay_control.auth_copay_control_obj.
            END. // FOR EACH btt_auth_copay_detail
          END. // IF btt_auth_copay_control.auth_copay_control_obj < 1 THEN

          ASSIGN
             btt_auth_copay_control.auth_copay_control_obj = buf_auth_copay_control.auth_copay_control_obj
             btt_auth_copay_control.record_action          = "":U.

          VALIDATE buf_auth_copay_control.

          FIND CURRENT buf_auth_copay_control NO-LOCK.
        END. // IF AVAILABLE buf_auth_copay_control AND NOT lFailureOccurred THEN
      END. // IF btt_auth_copay_control.record_action = "MODIFY":U THEN
          
      IF btt_auth_copay_control.record_action = "DELETE":U THEN
      DO:
        /* 
          This routine will ensure that all dependencies will also be removed 
        */
        RUN _deleteAuthCopayControl IN TARGET-PROCEDURE (INPUT btt_auth_copay_control.auth_copay_control_obj).

        IF NOT CAN-FIND(FIRST btt_auth_copay_error NO-LOCK
                        WHERE btt_auth_copay_error.owning_entity_mnemonic = "haccc":U
                          AND btt_auth_copay_error.owning_obj = btt_auth_copay_control.auth_copay_control_obj)
        THEN 
           DELETE btt_auth_copay_control.
        ELSE DO: /*if we ran into an error while deleting copy the record values from DB to the temp-table record*/

          FIND FIRST buf_auth_copay_control NO-LOCK
               WHERE buf_auth_copay_control.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj 
            NO-ERROR.
            
          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE} 

          IF AVAILABLE buf_auth_copay_control 
          THEN
            BUFFER-COPY buf_auth_copay_control
                     TO btt_auth_copay_control.

        END. /*ELSE DO*/
      END. // END - IF btt_auth_copay_control.record_action = "DELETE":U THEN    
    END. // FOR EACH btt_auth_copay_control EXCLUSIVE-LOCK:
  END. // DO TRANSACTION ON ERROR UNDO, THROW:

 { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject." }
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthCopayDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthCopayDetail Procedure 
PROCEDURE _saveAuthCopayDetail PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Create/Update/Delete Auth Copay detail Procedure    
  Parameters:  <none>
  Notes     : MIP catch error handling used { mip/inc/mipatcherror.i }
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE cEEM                 AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOEM                 AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lSucess              AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred     AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE iCount               AS INTEGER             NO-UNDO INITIAL 1. 
  
  DEFINE BUFFER buf_auth_copay_detail   FOR hac_auth_copay_detail.
  DEFINE BUFFER btt_auth_copay_detail   FOR tt_auth_copay_detail.
  DEFINE BUFFER btt_auth_copay_result   FOR tt_auth_copay_result.
  DEFINE BUFFER btt_auth_copay_error    FOR tt_auth_copay_error.
  DEFINE BUFFER tt_auth_copay_detail    FOR tt_auth_copay_detail.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).
    
    FIND FIRST btt_auth_copay_result EXCLUSIVE-LOCK NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_copay_result 
    THEN
      CREATE btt_auth_copay_result.

    Record-Blk:
    FOR EACH btt_auth_copay_detail EXCLUSIVE-LOCK
          BY btt_auth_copay_detail.auth_copay_detail_obj DESCENDING:

      /* No use to carry on if there's an error on the auth co-payment control header */
      IF oErrorObject:canFind("haccc":U, btt_auth_copay_detail.auth_copay_control_obj, "":U) THEN
        NEXT Record-Blk.

      ASSIGN
        lFailureOccurred                        = FALSE
        btt_auth_copay_result.records_processed = btt_auth_copay_result.records_processed + 1.
        
      IF btt_auth_copay_detail.record_action = "MODIFY":U THEN
      DO:
        /* 
          Validate Current buffer before we save 
        */
        RUN _validateAuthCopayDetail IN TARGET-PROCEDURE(BUFFER btt_auth_copay_detail, INPUT-OUTPUT lFailureOccurred).

        /* 
          Duplicate check 
        */
        FIND FIRST buf_auth_copay_detail NO-LOCK
             WHERE buf_auth_copay_detail.auth_copay_control_obj    = btt_auth_copay_detail.auth_copay_control_obj
               AND buf_auth_copay_detail.owning_entity_mnemonic    = btt_auth_copay_detail.owning_entity_mnemonic
               AND buf_auth_copay_detail.owning_obj                = btt_auth_copay_detail.owning_obj
               AND buf_auth_copay_detail.owning_key                = btt_auth_copay_detail.owning_key
               AND buf_auth_copay_detail.exclusion_entity_mnemonic = btt_auth_copay_detail.exclusion_entity_mnemonic
               AND buf_auth_copay_detail.exclusion_obj             = btt_auth_copay_detail.exclusion_obj
               AND buf_auth_copay_detail.exclusion_key             = btt_auth_copay_detail.exclusion_key
               AND buf_auth_copay_detail.effective_date           <= btt_auth_copay_detail.effective_date
               AND (buf_auth_copay_detail.end_date                 = ?
                 OR buf_auth_copay_detail.end_date                >= btt_auth_copay_detail.effective_date)
               AND buf_auth_copay_detail.auth_copay_detail_obj    <> btt_auth_copay_detail.auth_copay_detail_obj
          NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE buf_auth_copay_detail 
        THEN
          FIND FIRST buf_auth_copay_detail NO-LOCK
             WHERE buf_auth_copay_detail.auth_copay_control_obj    = btt_auth_copay_detail.auth_copay_control_obj
               AND buf_auth_copay_detail.owning_entity_mnemonic    = btt_auth_copay_detail.owning_entity_mnemonic
               AND buf_auth_copay_detail.owning_obj                = btt_auth_copay_detail.owning_obj
               AND buf_auth_copay_detail.owning_key                = btt_auth_copay_detail.owning_key
               AND buf_auth_copay_detail.exclusion_entity_mnemonic = btt_auth_copay_detail.exclusion_entity_mnemonic
               AND buf_auth_copay_detail.exclusion_obj             = btt_auth_copay_detail.exclusion_obj
               AND buf_auth_copay_detail.exclusion_key             = btt_auth_copay_detail.exclusion_key
               AND buf_auth_copay_detail.effective_date           >= btt_auth_copay_detail.effective_date
               AND (btt_auth_copay_detail.end_date                 = ?
                 OR buf_auth_copay_detail.end_date                <= btt_auth_copay_detail.effective_date)
               AND buf_auth_copay_detail.auth_copay_detail_obj    <> btt_auth_copay_detail.auth_copay_detail_obj
          NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }


        IF NOT AVAILABLE buf_auth_copay_detail
        AND btt_auth_copay_detail.end_date <> ?
        THEN
          FIND FIRST buf_auth_copay_detail NO-LOCK
               WHERE buf_auth_copay_detail.auth_copay_control_obj    = btt_auth_copay_detail.auth_copay_control_obj
                 AND buf_auth_copay_detail.owning_entity_mnemonic    = btt_auth_copay_detail.owning_entity_mnemonic
                 AND buf_auth_copay_detail.owning_obj                = btt_auth_copay_detail.owning_obj
                 AND buf_auth_copay_detail.owning_key                = btt_auth_copay_detail.owning_key
                 AND buf_auth_copay_detail.exclusion_entity_mnemonic = btt_auth_copay_detail.exclusion_entity_mnemonic
                 AND buf_auth_copay_detail.exclusion_obj             = btt_auth_copay_detail.exclusion_obj
                 AND buf_auth_copay_detail.exclusion_key             = btt_auth_copay_detail.exclusion_key
                 AND buf_auth_copay_detail.effective_date           <= btt_auth_copay_detail.end_date
                 AND (buf_auth_copay_detail.end_date                 = ?
                   OR buf_auth_copay_detail.end_date                >= btt_auth_copay_detail.end_date)
                 AND buf_auth_copay_detail.auth_copay_detail_obj    <> btt_auth_copay_detail.auth_copay_detail_obj
            NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF AVAILABLE buf_auth_copay_detail THEN /*check the detail obj */
        DO: 
          FIND FIRST mic_acronym NO-LOCK
            WHERE mic_acronym.category_key  = "ma_acAuthCopayDetailEntities":U
              AND mic_acronym.acronym_value = btt_auth_copay_detail.owning_entity_mnemonic
            NO-ERROR.
          IF AVAILABLE mic_acronym 
          THEN 
            ASSIGN cOEM = mic_acronym.acronym_label.
          ELSE
            ASSIGN cOEM = btt_auth_copay_detail.owning_entity_mnemonic.
            
          FIND FIRST mic_acronym NO-LOCK
            WHERE mic_acronym.category_key  = "ma_acAuthCopayDetailEntities":U
              AND mic_acronym.acronym_value = btt_auth_copay_detail.exclusion_entity_mnemonic
            NO-ERROR.
          IF AVAILABLE mic_acronym 
          THEN
            ASSIGN cEEM = mic_acronym.acronym_label.
          ELSE 
            ASSIGN cEEM = btt_auth_copay_detail.exclusion_entity_mnemonic.
                
          ASSIGN lFailureOccurred = TRUE
                 cErrorMessage    = CHR(10)
                                    + "Owning Entity: ":U       + cOEM                                              + CHR(10)
                                    + "Owning Alt Value: ":U    + STRING(btt_auth_copay_detail.owning_alt_value)    + CHR(10)
                                    + "Exclusion Entity: ":U    + cEEM                                              + CHR(10) 
                                    + (IF btt_auth_copay_detail.exclusion_alt_value <> ""
                                       AND btt_auth_copay_detail.exclusion_alt_value <> ?
                                       THEN "Exclusion Alt Value: ":U + btt_auth_copay_detail.exclusion_alt_value   + CHR(10)
                                       ELSE "":U)
                                    + "Effective Date: ":U + STRING(btt_auth_copay_detail.effective_date,"9999/99/99")
                                    + (IF btt_auth_copay_detail.end_date <> ? 
                                       THEN CHR(10) + "End Date: ":U + STRING(btt_auth_copay_detail.end_date,"9999/99/99")
                                       ELSE "":U).

          oErrorObject:addError(INPUT "haccd":U, 
                                INPUT btt_auth_copay_detail.auth_copay_detail_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_copay_detail.line_number, 
                                INPUT "MA":U, 
                                INPUT 125,  /* &1 already exists with &2 */
                                INPUT "Auth copay detail,":U + cErrorMessage).

          BUFFER buf_auth_copay_detail:HANDLE:BUFFER-RELEASE(). 
        END. /* IF CAN-FIND(FIRST buf_auth_copay_detail NO-LOCK */

        IF lFailureOccurred
        THEN
          NEXT Record-Blk.

        FIND FIRST buf_auth_copay_detail EXCLUSIVE-LOCK
             WHERE buf_auth_copay_detail.auth_copay_detail_obj = btt_auth_copay_detail.auth_copay_detail_obj
          NO-ERROR NO-WAIT.
          
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /* 
          The auth copay control is locked by another user or process 
        */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN
            lFailureOccurred = TRUE.
            
            btt_auth_copay_result.records_locked = btt_auth_copay_result.records_locked + 1.
            
            oErrorObject:addError(INPUT "haccd":U, 
                                  INPUT btt_auth_copay_detail.auth_copay_detail_obj, 
                                  INPUT "":U, 
                                  INPUT btt_auth_copay_detail.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                  INPUT "Auth Copay Detail :":U + STRING(btt_auth_copay_detail.auth_copay_detail_obj)).
        
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */

        /* 
          Record not found so we are creating 
        */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN
        DO:
          cls.miperror:resetError().
          
          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_copay_detail.
          
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN */
      
        IF AVAILABLE buf_auth_copay_detail AND NOT lFailureOccurred THEN
        DO:
          /* An Existing Record is being updated */
          IF btt_auth_copay_detail.auth_copay_detail_obj <= 0 
          THEN ASSIGN btt_auth_copay_result.records_created  = btt_auth_copay_result.records_created  + 1.
          ELSE ASSIGN btt_auth_copay_result.records_modified = btt_auth_copay_result.records_modified + 1.

          BUFFER-COPY btt_auth_copay_detail
               EXCEPT btt_auth_copay_detail.auth_copay_detail_obj
                   TO buf_auth_copay_detail.
                   
          ASSIGN
            btt_auth_copay_detail.auth_copay_detail_obj = buf_auth_copay_detail.auth_copay_detail_obj
            btt_auth_copay_detail.record_action         = "":U.
            
          VALIDATE buf_auth_copay_detail.
          
          FIND CURRENT buf_auth_copay_detail NO-LOCK.           
        END. /* IF AVAILABLE buf_auth_copay_detail AND NOT lFailureOccurred THEN */
      END. /* IF btt_auth_copay_detail.record_action = "MODIFY":U  */

      IF btt_auth_copay_detail.record_action = "DELETE":U THEN
      DO:
        
        /* This routine will ensure that all dependencies will also be removed */
        RUN _deleteAuthCopayDetail IN TARGET-PROCEDURE (INPUT btt_auth_copay_detail.auth_copay_detail_obj).

        IF NOT CAN-FIND(FIRST btt_auth_copay_error NO-LOCK
                        WHERE btt_auth_copay_error.owning_entity_mnemonic = "haccd":U
                          AND btt_auth_copay_error.owning_obj = btt_auth_copay_detail.auth_copay_detail_obj) 
        THEN 
          DELETE btt_auth_copay_detail.
      END. /* IF btt_auth_copay_detail.record_action = "DELETE":U THEN */
    END. /* FOR EACH btt_auth_copay_detail NO-LOCK */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
  
  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_setAuthCopayControlDates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _setAuthCopayControlDates Procedure 
PROCEDURE _setAuthCopayControlDates PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     When the effective/end date is changed, the effective/end dates
               on the detail lines must be changed as well where the effectve/end
               date on the detail line is the same as the old effectve/end date. 
  Parameters: 
  Notes:       
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE PARAMETER    BUFFER btt_auth_copay_control    FOR tt_auth_copay_control.
  DEFINE PARAMETER    BUFFER buf_auth_copay_control    FOR hac_auth_copay_control.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject         AS cls.maerrorobject         NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ioplFailureOccurred    AS LOGICAL                   NO-UNDO.

  DEFINE VARIABLE lDateChangeApplied   AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL NO-UNDO.
  DEFINE VARIABLE dOldEffectiveDate    AS DATE    NO-UNDO.
  DEFINE VARIABLE dOldEndDate          AS DATE    NO-UNDO.

  DEFINE BUFFER buf_auth_copay_detail   FOR hac_auth_copay_detail.

  IF ioplFailureOccurred 
  THEN
    RETURN.
    
  ASSIGN dOldEffectiveDate = buf_auth_copay_control.effective_date
         dOldEndDate       = buf_auth_copay_control.end_date.

  FOR EACH buf_auth_copay_detail EXCLUSIVE-LOCK 
     WHERE buf_auth_copay_detail.auth_copay_control_obj = btt_auth_copay_control.auth_copay_control_obj:

    IF (buf_auth_copay_detail.effective_date = dOldEffectiveDate AND dOldEffectiveDate <> btt_auth_copay_control.effective_date) 
    THEN
      ASSIGN lDateChangeApplied                   = TRUE
             buf_auth_copay_detail.effective_date = btt_auth_copay_control.effective_date.

    IF (buf_auth_copay_detail.end_date = dOldEndDate AND dOldEndDate <> btt_auth_copay_control.end_date) 
    THEN
      ASSIGN lDateChangeApplied             = TRUE
             buf_auth_copay_detail.end_date = btt_auth_copay_control.end_date. 

  END. /* FOR EACH buf_auth_copay_detail EXCLUSIVE-LOCK */

  IF lDateChangeApplied 
  THEN
      ipoErrorObject:addError(INPUT "haccc":U,                                                                         /* ipcOwningEntityMnemonic  */
                              INPUT btt_auth_copay_control.auth_copay_control_obj,                                     /* ipdOwningEntityObj       */
                              INPUT "":U,                                                                              /* ipcOwningEntityKey       */
                              INPUT btt_auth_copay_control.line_number,                                                /* ipiLineNumber            */
                              INPUT "Auth Copay Detail effective/end dates were updated according to header dates.":U, /* ipcMessageText           */
                              INPUT "WAR":U,                                                                           /* ipcMessageType           */
                              INPUT FALSE).                                                                            /* iplAcknowledge           */

&ENDIF
  {mip/inc/mipcatcherror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCopayControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCopayControl Procedure 
PROCEDURE _validateAuthCopayControl PRIVATE :
/*------------------------------------------------------------------------------
  Purpose    : Auth Copay Control Validation     
  Parameters : Current Buffer to be validated
  Notes      : 
------------------------------------------------------------------------------*/

  { ma/app/maauthcopayservalcontrol.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCopayDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCopayDetail Procedure 
PROCEDURE _validateAuthCopayDetail PRIVATE :
/*----------------------------------------------------------------------------
  Purpose    : Auth Copay Detail validation    
  Parameters : Current Buffer to be validated
  Notes      :
  ----------------------------------------------------------------------------*/
  
  { ma/app/maauthcopayservaldetail.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

