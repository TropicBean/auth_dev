&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    Purpose : Auth Flag Value Service Stack
    
    Author  : Andrewd

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */

BLOCK-LEVEL ON ERROR UNDO, THROW. 

CREATE WIDGET-POOL.

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthflagvalueds.i }
{ ma/inc/maauthds.i          }

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 22.43
         WIDTH              = 87.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-deleteAuthFlagHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteAuthFlagHistory Procedure 
PROCEDURE deleteAuthFlagHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth flag value history records
  Parameters: 
  Notes     : Data Access Only !!!      
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcOwningEntityMnemonic AS CHARACTER           NO-UNDO.  
  DEFINE INPUT        PARAMETER ipdOwningObj            AS DECIMAL             NO-UNDO.
  DEFINE INPUT        PARAMETER ipcOwningKey            AS CHARACTER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject          AS cls.maerrorobject   NO-UNDO.
  
  DEFINE VARIABLE lFailureOccurred            AS LOGICAL           NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN                 
  
  DEFINE BUFFER buf_auth_flag_value_history FOR hah_auth_flag_value_history.

  
  HistoryBlock:
  FOR EACH hah_auth_flag_value_history NO-LOCK
     WHERE hah_auth_flag_value_history.owning_entity_mnemonic = ipcOwningEntityMnemonic
       AND hah_auth_flag_value_history.owning_obj             = ipdOwningObj
       AND hah_auth_flag_value_history.owning_key             = ipcOwningKey:
  
    FIND FIRST buf_auth_flag_value_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_flag_value_history) = ROWID(hah_auth_flag_value_history)
      NO-ERROR NO-WAIT.
      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }
  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}
      
      ipoErrorObject:addError(INPUT ipcOwningEntityMnemonic, 
                              INPUT ipdOwningObj,                                    
                              INPUT ipcOwningKey,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Flag Value History":U).   
                              
      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/       
    
    IF AVAILABLE buf_auth_flag_value_history
    THEN 
      DELETE buf_auth_flag_value_history.
  END. /*FOR EACH hah_auth_flag_value_history NO-LOCK*/
    
  { mip/inc/mipcatcherror.i }

&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fetchAuthFlagValueDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthFlagValueDataset Procedure 
PROCEDURE fetchAuthFlagValueDataset :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Auth Flag Value Data    
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all 
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }            
    Author    : Mandlam
  ------------------------------------------------------------------------------*/   
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
  DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
  DEFINE INPUT        PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO. 
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSuccess                      AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE iCount                        AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle    AS HANDLE                    NO-UNDO. 
  DEFINE VARIABLE hDatasetHandle                AS HANDLE                    NO-UNDO. 
  DEFINE VARIABLE hBuffer                       AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE oSearchObject                 AS cls.maauthflagvaluesearch NO-UNDO.
                                                
  DEFINE DATA-SOURCE dsAuthFlagValue            FOR hat_auth_flag_value.
  DEFINE DATA-SOURCE dsAuthFlagValueAuthRule    FOR hac_auth_rule.
  DEFINE DATA-SOURCE dsAuthFlagValueHistory     FOR hah_auth_flag_value_history.
  
  ASSIGN 
     oSearchObject              = NEW cls.maauthflagvaluesearch(INPUT DATASET-HANDLE iophDatasetHandle)
     hFilterCriteriaTableHandle = iophFilterCriteriaTableHandle
     hDatasetHandle             = iophDatasetHandle.
  
  BufferBlock:
  DO iCount = 1 TO oSearchObject:DatasetHandle:NUM-BUFFERS:
  
    hBuffer = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE(iCount).
    
    /*If a buffer list has been specified then we will exclude all other buffers from being filled*/
    IF NOT CAN-DO(ipcWhatToGet, hBuffer:NAME) THEN
    DO:
      hBuffer:FILL-MODE = "NO-FILL":U.
      NEXT BufferBlock.
    END. /*IF NOT CAN-DO(ipcWhatToGet, hBuffer:NAME) THEN*/
    
    CASE hBuffer:NAME:
    
      WHEN "tt_auth_flag_value":U         THEN lSuccess = hBuffer:ATTACH-DATA-SOURCE(DATA-SOURCE dsAuthFlagValue:HANDLE).
      WHEN "tt_auth_flag_value_rule":U    THEN lSuccess = hBuffer:ATTACH-DATA-SOURCE(DATA-SOURCE dsAuthFlagValueAuthRule:HANDLE).
      WHEN "tt_auth_flag_value_history":U THEN lSuccess = hBuffer:ATTACH-DATA-SOURCE(DATA-SOURCE dsAuthFlagValueHistory:HANDLE).
      
      OTHERWISE
         hBuffer:FILL-MODE = "NO-FILL":U.
    END CASE. /* CASE hBuffer:NAME: */
  
  END. /* BufferBlock: */
  
  ASSIGN
     lSuccess = oSearchObject:populateDataset(INPUT iophFilterCriteriaTableHandle).
  
  ASSIGN
     iophDatasetHandle             = oSearchObject:DatasetHandle
     iophFilterCriteriaTableHandle = oSearchObject:CriteriaTableHandle.
     
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

&IF DEFINED(EXCLUDE-saveAuthFlagValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthFlagValue Procedure 
PROCEDURE saveAuthFlagValue :
/*------------------------------------------------------------------------------
  Purpose   : Save Auth Flag Value from dataset    
  Parameters: dsAuthFlagValue as defined in maauthflagvalueds.i
  Notes     : - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : Mandlam
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthFlagValue.
  
&IF {&DBDFMA} >= 10195 &THEN                             
  
  DEFINE VARIABLE hBuffer       AS HANDLE  NO-UNDO. 
  DEFINE VARIABLE iBuffer       AS INTEGER NO-UNDO.
  DEFINE VARIABLE iTotalBuffers AS INTEGER NO-UNDO.
  
  ASSIGN iTotalBuffers = DATASET dsAuthFlagValue:NUM-BUFFERS.

  DO iBuffer = 1 TO iTotalBuffers:
    
    hBuffer = DATASET dsAuthFlagValue:GET-BUFFER-HANDLE(iBuffer).

    CASE hBuffer:NAME:
    
      WHEN "tt_auth_flag_value":U 
      THEN 
        RUN _saveAuthFlagValue IN TARGET-PROCEDURE.
      
    END CASE.
    
  END. /*DO iBuffer = 1 TO iTotalBuffers*/
  
  FIND FIRST tt_auth_flag_value_result EXCLUSIVE-LOCK NO-ERROR.
    
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
  
  IF NOT AVAILABLE tt_auth_flag_value_result 
  THEN
    CREATE tt_auth_flag_value_result.
      
  FOR EACH tt_auth_flag_value_error NO-LOCK:    
    ASSIGN tt_auth_flag_value_result.number_of_errors = tt_auth_flag_value_result.number_of_errors + 1.
  END. /* FOR EACH tt_auth_flag_value_error NO-LOCK: */
  
  { mip/inc/mipcatcherror.i }
    
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthFlagValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthFlagValue Procedure 
PROCEDURE _deleteAuthFlagValue :
/*------------------------------------------------------------------------------
  Purpose   : Remove Authorisation Flag Value record    
  Parameters: Authorisation Flag Value obj
  Notes     : - This will run the procedures to remove any
                dependency records as well.
              - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author    : Mandlam
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthFlagValueObj AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject NO-UNDO. 
  DEFINE VARIABLE lFailureOccurred AS LOGICAL           NO-UNDO.
    
&IF {&DBDFMA} >= 10195 &THEN                             
  
  DEFINE BUFFER btt_auth_flag_value_result FOR tt_auth_flag_value_result.
  DEFINE BUFFER btt_auth_flag_value_error  FOR tt_auth_flag_value_error.
  DEFINE BUFFER buf_auth_flag_value        FOR hat_auth_flag_value.
  DEFINE BUFFER buf_auth                   FOR hat_auth.
  
  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_flag_value_error:HANDLE).
    
    FIND FIRST btt_auth_flag_value_result EXCLUSIVE-LOCK NO-ERROR.
      
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE}
    
    IF NOT AVAILABLE btt_auth_flag_value_result 
    THEN
      CREATE btt_auth_flag_value_result.
    
    IF NOT lFailureOccurred THEN
    DO:
      FIND FIRST buf_auth_flag_value EXCLUSIVE-LOCK
           WHERE buf_auth_flag_value.auth_flag_value_obj = ipdAuthFlagValueObj
        NO-ERROR NO-WAIT.
    
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
      /*Unable to remove - record is already locked*/  
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        cls.miperror:resetError().
        
        ASSIGN lFailureOccurred = TRUE.
        
        oErrorObject:addError(INPUT "hataf":U, 
                              INPUT ipdAuthFlagValueObj,
                              INPUT "":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Flag Value":U).            
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/  
      
      /*
        MMP-368 - We will only delete the history records if the auth is deleted
      */
      
      IF AVAILABLE buf_auth_flag_value AND NOT lFailureOccurred THEN    
      DO:  
          DELETE buf_auth_flag_value.
          
          ASSIGN btt_auth_flag_value_result.records_removed = btt_auth_flag_value_result.records_removed + 1.
          
      END. /*IF AVAILABLE buf_auth_flag_value AND NOT lFailureOccurred THEN*/
    END. /*IF NOT lFailureOccurred THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
     
&ENDIF                            
  
  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthFlagValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthFlagValue Procedure 
PROCEDURE _saveAuthFlagValue :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete Auth flag value information
  Parameters: 
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : Mandlam
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN                        

  DEFINE VARIABLE cCodeLinkCategory         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOverrideArsRate          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOverrideBaseRate         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDefaultBaseRate          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCurrentOverrideBaseRate  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCurrentOverrideArsRate   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDefaultArsRate           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMessage                  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iSequence                 AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dtDateTime                AS DATETIME  NO-UNDO.
  DEFINE VARIABLE lHistory                  AS LOGICAL   NO-UNDO. 
  DEFINE VARIABLE lSuccess                  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lFailureOccurred          AS LOGICAL   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lOverlap                  AS LOGICAL   NO-UNDO INITIAL FALSE.
  
  DEFINE VARIABLE oErrorObject      AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE oAuthRule         AS cls.maauthrule    NO-UNDO. 
  DEFINE VARIABLE oSearch           AS cls.maauthsearch  NO-UNDO.

  DEFINE BUFFER buf_auth_flag_value_history FOR hah_auth_flag_value_history.                                                         
  DEFINE BUFFER buf_auth_flag_value         FOR hat_auth_flag_value.
  DEFINE BUFFER buf_auth                    FOR hat_auth.
  DEFINE BUFFER buf_note                    FOR note.
  DEFINE BUFFER btt_auth_flag_value_result  FOR tt_auth_flag_value_result.
  DEFINE BUFFER btt_auth_flag_value_error   FOR tt_auth_flag_value_error.
  DEFINE BUFFER btt_auth_flag_value         FOR tt_auth_flag_value.
  DEFINE BUFFER btt_auth                    FOR tt_auth.
  DEFINE BUFFER btt_auth_provider           FOR tt_auth_provider.
  DEFINE BUFFER buf_auth_rule               FOR hac_auth_rule.

  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_flag_value_error:HANDLE).

    FIND FIRST btt_auth_flag_value_result EXCLUSIVE-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_flag_value_result 
    THEN
      CREATE btt_auth_flag_value_result.

    RECORD-BLK:                             
    FOR EACH btt_auth_flag_value EXCLUSIVE-LOCK
          BY btt_auth_flag_value.auth_flag_value_obj DESCENDING:

      ASSIGN 
         lFailureOccurred = FALSE       
         btt_auth_flag_value_result.records_processed = btt_auth_flag_value_result.records_processed + 1.

      IF btt_auth_flag_value.record_action = "MODIFY":U THEN
      DO:
        /*Validate the owning entity mnemonic*/
        mipEnv:miDBEntity:focusTable(btt_auth_flag_value.owning_entity_mnemonic) NO-ERROR.

        cls.miperror:resetError().

        IF NOT mipEnv:miDBEntity:InFocus THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hataf":U,
                                INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                INPUT "":U,
                                INPUT btt_auth_flag_value.line_number, 
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Flag Value entity: " + btt_auth_flag_value.owning_entity_mnemonic).                                                      
        END. /*IF NOT mipEnv:miDBEntity:InFocus THEN*/

        /*Ensure that the entity specified is valid for the table we have focused on*/ 
        ASSIGN lSuccess = IF mipEnv:miDBEntity:MainIndexType = "OBJ":U
                          THEN mipEnv:miDBEntity:findRecord(btt_auth_flag_value.owning_obj)
                          ELSE mipEnv:miDBEntity:findRecord(btt_auth_flag_value.owning_key).

        IF NOT mipEnv:miDBEntity:RecordAvailable THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.

          IF btt_auth_flag_value.owning_obj <> 0.00 AND btt_auth_flag_value.owning_key = "":U
          THEN DO:
            oErrorObject:addError(INPUT "hataf":U,
                                  INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                  INPUT "":U,
                                  INPUT btt_auth_flag_value.line_number, 
                                  INPUT "MA":U,
                                  INPUT 112,  /* The &1 specified is invalid. &2 */
                                  INPUT "Authorisation (Obj: " + STRING(btt_auth_flag_value.owning_obj) + ")":U +
                                        ",Unable to link Flag to Authorisation.").

          END. /* IF btt_auth_flag_value.owning_obj <> 0.00 AND btt_auth_flag_value.owning_key = "":U */
          ELSE IF btt_auth_flag_value.owning_obj = 0.00 AND btt_auth_flag_value.owning_key <> "":U THEN 
          DO:
            oErrorObject:addError(INPUT "hataf":U,
                                  INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                  INPUT "":U,
                                  INPUT btt_auth_flag_value.line_number, 
                                  INPUT "MA":U,
                                  INPUT 100,  /* The "&1" specified is invalid */
                                  INPUT "Authorisation Flag Key (Key:" + btt_auth_flag_value.owning_key + ")":U).            
          END. /* ELSE IF btt_auth_flag_value.owning_obj = 0.00 AND btt_auth_flag_value.owning_key <> "":U THEN */
        END. /*IF NOT mipEnv:miDBEntity:RecordAvailable THEN*/

        /*Get Auth Rule with the AuthFlagValue obj*/
        ASSIGN oAuthrule = NEW cls.maauthrule().

        oAuthrule:focusAuthRule(INPUT btt_auth_flag_value.auth_rule_obj).

        IF NOT oAuthrule:AuthRuleInFocus 
        THEN DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hataf":U,
                                INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                INPUT "":U,
                                INPUT btt_auth_flag_value.line_number, 
                                INPUT "MA":U,
                                INPUT 112,  /* The "&1" specified is invalid. &2 */
                                INPUT "Authorisation Flag Rule (Obj: " + STRING(btt_auth_flag_value.auth_rule_obj) + ")":U).                  
        END. /* IF NOT oAuthrule:AuthRuleInFocus */
        ELSE DO:
          /* Check if Auth Flag value is within Auth rule's valid values */
          IF LOOKUP(btt_auth_flag_value.auth_flag_value,oAuthRule:RuleValidValues,"|") = 0
          THEN DO:
             ASSIGN lFailureOccurred = TRUE
                    cMessage = "Authorisation Flag Value,The Flag Value should be within Authorisation Rule's valid values: " + 
                               oAuthRule:RuleValidValues + "[HELP=Auth Rule Code: " + oAuthRule:RuleCode + "]":U.

             oErrorObject:addError(INPUT "hataf":U,
                                   INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                   INPUT "":U,
                                   INPUT "auth_flag_value", 
                                   INPUT btt_auth_flag_value.line_number,
                                   INPUT "MA":U,
                                   INPUT 112,  /* The &1 specified is invalid. &2 */
                                   INPUT cMessage).                  
          END.  /* IF LOOKUP for valid rule value */

          /* If Auth flag is for AUTH table - Check if Auth dates falls within the Auth rule dates */
          IF btt_auth_flag_value.owning_entity_mnemonic = "hatau":U
          THEN DO:
            FIND FIRST buf_auth NO-LOCK
                 WHERE buf_auth.auth_obj = btt_auth_flag_value.owning_obj 
              NO-ERROR.

            {mip/inc/mipthrowerror.i &IgnoreErrors = 'PRGROESS:565':U &ResetIgnoredErrors = TRUE}

            IF AVAILABLE buf_auth
            AND buf_auth.start_date <> ?
            THEN DO:
                
              mipEnv:Health:mautility:checkOverlappingPeriods
                (buf_auth.start_date,
                 buf_auth.end_date,
                 oAuthrule:EffectiveDate,
                 oAuthrule:EndDate,
                 lOverlap).

              IF NOT lOverlap THEN 
              DO:
                ASSIGN lFailureOccurred = TRUE
                       cMessage = "Authorisation Flag, The Auth Flag Rule is not effective for Authorisation Period."
                                + "[HELP=Auth Rule Code: " + oAuthRule:RuleCode + "]":U.
                
                oErrorObject:addError(INPUT "hataf:":U + btt_auth_flag_value.owning_entity_mnemonic,  
                                      INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                      INPUT "":U, 
                                      INPUT btt_auth_flag_value.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 112,  /* The &1 specified is invalid. &2 */    
                                      INPUT cMessage).

              END. /* IF NOT lOverlap THEN */

              ASSIGN btt_auth_flag_value.owning_alt_value = buf_auth.auth_num.
             
            END.  /* IF AVAILABLE buf_auth */
          END.  /* IF btt_auth_flag_value.owning_entity_mnemonic = "hatau" */

          /*
            MMP-1007
            When an override reason is captured for the PENALTY flag, 
            we should only allow a valid override note for note type AP
          */
          IF  oAuthRule:RuleCode                 = "PENALTY":U
          AND btt_auth_flag_value.override_note <> "" 
          THEN DO:
            FIND FIRST buf_note NO-LOCK
                 WHERE buf_note.scheme-code = 0
                   AND buf_note.type        = "AP":U
                   AND buf_note.key         = btt_auth_flag_value.override_note 
                 NO-ERROR. 
           {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = FALSE}
           
            IF NOT AVAILABLE buf_note  
            THEN
              ASSIGN cMessage = "Penalty Override Note , Note with Key ~"" + btt_auth_flag_value.override_note + "~" was not found."
                              + "[HELP=Auth Rule Code: " + oAuthRule:RuleCode + "]":U
                     lSuccess = oErrorObject:addError(INPUT "hataf":U, 
                                     INPUT btt_auth_flag_value.auth_flag_value_obj,
                                     INPUT "":U,
                                     INPUT btt_auth_flag_value.line_number, 
                                     INPUT "MA":U,
                                     INPUT 112,  /* The &1 specified is invalid. &2 */
                                     INPUT cMessage ).
            
          END. /* IF  oAuthRule:RuleCode = "PENALTY":U... */
        END.  /*  ELSE NOT oAuthrule:AuthRuleInFocus */

        IF lFailureOccurred
        THEN 
          NEXT RECORD-BLK.

        FIND FIRST buf_auth_flag_value EXCLUSIVE-LOCK
             WHERE buf_auth_flag_value.auth_flag_value_obj = btt_auth_flag_value.auth_flag_value_obj
          NO-ERROR NO-WAIT.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}

        /*The flag value record is locked by another user or process*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().

          ASSIGN 
             lFailureOccurred = TRUE

             btt_auth_flag_value_result.records_locked = btt_auth_flag_value_result.records_locked + 1.

          oErrorObject:addError(INPUT "hataf":U, 
                                INPUT  btt_auth_flag_value.auth_flag_value_obj, 
                                INPUT  "":U, 
                                INPUT  btt_auth_flag_value.line_number, 
                                INPUT  "MA":U, 
                                INPUT  200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT  "Authorisation Flag").

        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/

        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().

          /*Duplicate check*/ 
          IF CAN-FIND(FIRST buf_auth_flag_value NO-LOCK
                      WHERE buf_auth_flag_value.owning_entity_mnemonic = btt_auth_flag_value.owning_entity_mnemonic
                        AND buf_auth_flag_value.owning_obj             = btt_auth_flag_value.owning_obj
                        AND buf_auth_flag_value.owning_key             = btt_auth_flag_value.owning_key
                        AND buf_auth_flag_value.owning_alt_value       = btt_auth_flag_value.owning_alt_value
                        AND buf_auth_flag_value.auth_rule_obj          = btt_auth_flag_value.auth_rule_obj) 
          THEN
          DO:
            ASSIGN lFailureOccurred = TRUE.

            oErrorObject:addError(INPUT "hataf":U, 
                                  INPUT btt_auth_flag_value.auth_flag_value_obj, 
                                  INPUT "":U, 
                                  INPUT btt_auth_flag_value.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 125,  /* &1 already exists with &2 */
                                  INPUT "Authorisation Flag, Auth Rule Code.[HELP=Auth Rule Code: " + oAuthRule:RuleCode + "]":U).

          END. /*IF CAN-FIND(FIRST buf_auth_flag_value NO-LOCK*/                        

          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_flag_value.

        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/

        ASSIGN lHistory = ?. 

        IF AVAILABLE buf_auth_flag_value AND NOT lFailureOccurred THEN
        DO:        
          /*An existing record is being updated*/
          IF btt_auth_flag_value.auth_flag_value_obj <= 0 
          THEN ASSIGN btt_auth_flag_value_result.records_created  = btt_auth_flag_value_result.records_created  + 1.
          ELSE ASSIGN btt_auth_flag_value_result.records_modified = btt_auth_flag_value_result.records_modified + 1.

          BUFFER-COPY btt_auth_flag_value 
               EXCEPT btt_auth_flag_value.auth_flag_value_obj 
                   TO buf_auth_flag_value.        

          ASSIGN 
             btt_auth_flag_value.auth_flag_value_obj = buf_auth_flag_value.auth_flag_value_obj
             btt_auth_flag_value.record_action       = "":U.

          VALIDATE buf_auth_flag_value.   

          FIND CURRENT buf_auth_flag_value NO-LOCK.       
        END. /*IF opcErrorMessage = "":U THEN*/                
      END. /*IF btt_auth_flag_value.record_action = "MODIFY":U THEN*/

      IF btt_auth_flag_value.record_action = "DELETE":U THEN
      DO:    
        RUN _deleteAuthFlagValue IN TARGET-PROCEDURE (INPUT btt_auth_flag_value.auth_flag_value_obj).

        IF NOT CAN-FIND(FIRST btt_auth_flag_value_error NO-LOCK
                        WHERE btt_auth_flag_value_error.owning_entity_mnemonic = 'hataf':U
                          AND btt_auth_flag_value_error.owning_obj             = btt_auth_flag_value.auth_flag_value_obj)
        THEN
          DELETE btt_auth_flag_value.
      END. /*END. /*IF btt_auth_flag_value.record_action = "DELETE":U THEN*/*/           

    END. /*FOR EACH btt_auth_flag_value EXCLUSIVE-LOCK:*/      
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject.
                IF VALID-OBJECT(oAuthRule)    THEN DELETE OBJECT oAuthRule.
                IF VALID-OBJECT(oSearch)      THEN DELETE OBJECT oSearch.
                DATASET dsAuthorisation:EMPTY-DATASET()." }    
  
&ENDIF  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

