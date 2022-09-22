&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    $Id: maauthcopaydetitemservstack.p       Exp $

    Purpose: Stack procedure for Auth Copay Detail Items Class

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthcopaydetitemds.i}

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
         HEIGHT             = 23.81
         WIDTH              = 57.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-fetchAuthCopayDetailItemDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthCopayDetailItemDataSet Procedure
PROCEDURE fetchAuthCopayDetailItemDataSet :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Auth Copay Detail Item Data
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
  DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
  DEFINE INPUT        PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lSuccess                   AS LOGICAL                      NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER                      NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle AS HANDLE                       NO-UNDO.
  DEFINE VARIABLE hDatasetHandle             AS HANDLE                       NO-UNDO.
  DEFINE VARIABLE hBuffer                    AS HANDLE                       NO-UNDO.
  DEFINE VARIABLE hAuthCopayDetailItem       AS HANDLE                       NO-UNDO.
  DEFINE VARIABLE hAuthCopayError            AS HANDLE                       NO-UNDO.
  DEFINE VARIABLE hAuthCopayResult           AS HANDLE                       NO-UNDO.
  DEFINE VARIABLE oSearchObject              AS cls.maauthcopaydetitemsearch NO-UNDO.

  DEFINE DATA-SOURCE srcAuthCopayDetailItem  FOR hac_auth_copay_detail_item.

  /* Copy the Criteria Table from the input parameter table handle */
  TEMP-TABLE ttDsCriteria:HANDLE:COPY-TEMP-TABLE ( iophFilterCriteriaTableHandle,false , ? , ?  , ?).

  ASSIGN
    oSearchObject              = NEW cls.maAuthCopayDetItemSearch(INPUT DATASET-HANDLE iophDatasetHandle)
    hFilterCriteriaTableHandle = iophFilterCriteriaTableHandle
    hDatasetHandle             = iophDatasetHandle.

  ASSIGN
    hAuthCopayDetailItem = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_detail_item":U)
    hAuthCopayError      = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_detail_item_error":U)
    hAuthCopayResult     = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_detail_item_result":U).

  ASSIGN
    lSuccess = hAuthCopayDetailItem:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCopayDetailItem:HANDLE).

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

&IF DEFINED(EXCLUDE-saveAuthCopayDetailItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthCopayDetailItem Procedure
PROCEDURE saveAuthCopayDetailItem :
/*------------------------------------------------------------------------------
  Purpose   : Save Auth Copay Detail Item from dataset
  Parameters: dsAuthCopayDetailItem as defined in maauthcopaydetitemds.i
  Notes     : - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthCopayDetailItem.

&IF {&DBDFMA} >= 10195 &THEN

  RUN _saveAuthCopayDetailItem  IN TARGET-PROCEDURE.

  DO TRANSACTION ON ERROR UNDO, THROW:

    FIND FIRST tt_auth_copay_detail_item_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE tt_auth_copay_detail_item_result
    THEN
      CREATE tt_auth_copay_detail_item_result.

    FOR EACH tt_auth_copay_detail_item_error NO-LOCK:
      ASSIGN
        tt_auth_copay_detail_item_result.number_of_errors = tt_auth_copay_detail_item_result.number_of_errors + 1.
    END. /* FOR EACH tt_auth_copay_detail_item_error NO-LOCK:  */

  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCopayDetailItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCopayDetailItem Procedure
PROCEDURE _deleteAuthCopayDetailItem PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Remove Auth Copay Detail Item record
  Parameters: Auth copay Detail Item obj
  Notes     : - This will run the procedures to remove any
                dependency records as well.
              - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthCopayDetailItemObj AS DECIMAL            NO-UNDO.

  DEFINE VARIABLE oErrorObject                     AS cls.maerrorobject  NO-UNDO.
  DEFINE VARIABLE lFailureOccurred                 AS LOGICAL            NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_copay_detail_item_rslt        FOR tt_auth_copay_detail_item_result.
  DEFINE BUFFER btt_auth_copay_detail_item_error       FOR tt_auth_copay_detail_item_error.
  DEFINE BUFFER buf_auth_copay_detail_item             FOR hac_auth_copay_detail_item.

  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_detail_item_error:HANDLE).

    FIND FIRST btt_auth_copay_detail_item_rslt EXCLUSIVE-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_copay_detail_item_rslt
    THEN
      CREATE btt_auth_copay_detail_item_rslt.

    FIND FIRST buf_auth_copay_detail_item EXCLUSIVE-LOCK
      WHERE buf_auth_copay_detail_item.auth_copay_detail_item_obj = ipdAuthCopayDetailItemObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }

    /* Unable to remove - record is already locked */
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().

      ASSIGN lFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT ipdAuthCopayDetailItemObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Copay Detail Item":U).
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/

    IF AVAILABLE buf_auth_copay_detail_item AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_copay_detail_item.

      ASSIGN btt_auth_copay_detail_item_rslt.records_removed = btt_auth_copay_detail_item_rslt.records_removed + 1.

    END. /* IF AVAILABLE buf_auth_copay_detail_item AND NOT lFailureOccurred THEN  */
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

&ENDIF

  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthCopayDetailItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthCopayDetailItem Procedure
PROCEDURE _saveAuthCopayDetailItem PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete Auth Copay Detail Item information
  Parameters:
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE cErrorMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cEntity             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.

  DEFINE BUFFER buf_auth_copay_detail_item FOR hac_auth_copay_detail_item.
  DEFINE BUFFER btt_auth_copay_detail_item FOR tt_auth_copay_detail_item.
  DEFINE BUFFER btt_auth_copay_detail_item_rslt  FOR tt_auth_copay_detail_item_result.
  DEFINE BUFFER btt_auth_copay_detail_item_error       FOR tt_auth_copay_detail_item_error.

  DO TRANSACTION ON ERROR UNDO, THROW:
    ASSIGN oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_detail_item_error:HANDLE).

    FIND FIRST btt_auth_copay_detail_item_rslt EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE btt_auth_copay_detail_item_rslt
    THEN
      CREATE btt_auth_copay_detail_item_rslt.

    RECORD-BLK:
    FOR EACH btt_auth_copay_detail_item EXCLUSIVE-LOCK
          BY btt_auth_copay_detail_item.auth_copay_detail_item_obj DESCENDING:

      ASSIGN
         lFailureOccurred = FALSE
         btt_auth_copay_detail_item_rslt.records_processed = btt_auth_copay_detail_item_rslt.records_processed + 1.

      IF btt_auth_copay_detail_item.record_action = "MODIFY":U THEN
      DO:
        /*
          Auth Copay Detail Item Validations
        */
        RUN _validateAuthCopayDetailItem IN TARGET-PROCEDURE (BUFFER btt_auth_copay_detail_item, INPUT-OUTPUT lFailureOccurred ).

        CASE btt_auth_copay_detail_item.owning_entity_mnemonic:
          WHEN "htmtl":U THEN ASSIGN cEntity = "Tariff":U.
          WHEN "hlmnl":U THEN ASSIGN cEntity = "Nappi":U.
          WHEN "hlmcr":U THEN ASSIGN cEntity = "Basket":U.
          WHEN "hlmac":U THEN ASSIGN cEntity = "ATC Class":U.
        END CASE. 

        IF NOT lFailureOccurred THEN
        DO:
          /*
            Duplicate check
          */
          FIND FIRST buf_auth_copay_detail_item NO-LOCK
            WHERE  buf_auth_copay_detail_item.insurer_obj                 = btt_auth_copay_detail_item.insurer_obj
              AND  buf_auth_copay_detail_item.option_code                 = btt_auth_copay_detail_item.option_code
              AND  buf_auth_copay_detail_item.owning_entity_mnemonic      = btt_auth_copay_detail_item.owning_entity_mnemonic
              AND  buf_auth_copay_detail_item.owning_obj                  = btt_auth_copay_detail_item.owning_obj
              AND  buf_auth_copay_detail_item.owning_key                  = btt_auth_copay_detail_item.owning_key
              AND  buf_auth_copay_detail_item.pr_type                     = btt_auth_copay_detail_item.pr_type
              AND (buf_auth_copay_detail_item.effective_date              = btt_auth_copay_detail_item.effective_date 
               OR (buf_auth_copay_detail_item.effective_date             <= btt_auth_copay_detail_item.effective_date
              AND (buf_auth_copay_detail_item.end_date                    > btt_auth_copay_detail_item.effective_date OR buf_auth_copay_detail_item.end_date = ?))
               OR  buf_auth_copay_detail_item.end_date                    = btt_auth_copay_detail_item.effective_date
               OR  buf_auth_copay_detail_item.effective_date 	            = btt_auth_copay_detail_item.end_date
               OR (btt_auth_copay_detail_item.end_date                   <> ? 
              AND  buf_auth_copay_detail_item.end_date                    = btt_auth_copay_detail_item.end_date) 
               OR (buf_auth_copay_detail_item.effective_date             >= btt_auth_copay_detail_item.effective_date
              AND (btt_auth_copay_detail_item.end_date                    > buf_auth_copay_detail_item.effective_date OR btt_auth_copay_detail_item.end_date = ?)))
              AND  buf_auth_copay_detail_item.auth_copay_detail_item_obj <> btt_auth_copay_detail_item.auth_copay_detail_item_obj
            NO-ERROR.
          
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF AVAILABLE buf_auth_copay_detail_item THEN
          DO:
            ASSIGN lFailureOccurred = TRUE.
          
            oErrorObject:addError(INPUT "hacci":U,
                                  INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                                  INPUT "":U,
                                  INPUT "owning_entity_mnemonic":U,
                                  INPUT btt_auth_copay_detail_item.line_number,
                                  INPUT "MA":U,
                                  INPUT 125,  /* &1 already exists with &2 */
                                  INPUT "Auth Copay Detail Item,:":U +
                                        CHR(10) + "  Owning Entity: " + cEntity +
                                        CHR(10) + "  Owning Value: "  + btt_auth_copay_detail_item.owning_alt_value +
                                        CHR(10) + "  Eff.Date: "      + STRING(btt_auth_copay_detail_item.effective_date,"9999/99/99")).
          END. /* IF NOT AVAILABLE buf_auth_copay_detail_item THEN */
        END. /* IF NOT lFailureOccurred THEN */

        IF lFailureOccurred
        THEN
          NEXT RECORD-BLK.

        FIND FIRST buf_auth_copay_detail_item EXCLUSIVE-LOCK
          WHERE buf_auth_copay_detail_item.auth_copay_detail_item_obj = btt_auth_copay_detail_item.auth_copay_detail_item_obj
          NO-ERROR NO-WAIT.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}

        /*
          The auth copay detail item record is locked by another user or process
        */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().

          ASSIGN
             lFailureOccurred = TRUE
             btt_auth_copay_detail_item_rslt.records_locked = btt_auth_copay_detail_item_rslt.records_locked + 1.

          oErrorObject:addError(INPUT "hacci":U,
                                INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                                INPUT "":U,
                                INPUT btt_auth_copay_detail_item.line_number,
                                INPUT "MA":U,
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth Copay Detail Item: ":U + cEntity).

        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */

        /*
          Record not found so we are creating
        */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().

          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_copay_detail_item.
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN */

        IF AVAILABLE buf_auth_copay_detail_item AND NOT lFailureOccurred THEN
        DO:
          /*
            An existing record is being updated
          */
          IF btt_auth_copay_detail_item.auth_copay_detail_item_obj <= 0
          THEN
            ASSIGN btt_auth_copay_detail_item_rslt.records_created  = btt_auth_copay_detail_item_rslt.records_created  + 1.
          ELSE
            ASSIGN btt_auth_copay_detail_item_rslt.records_modified = btt_auth_copay_detail_item_rslt.records_modified + 1.

          BUFFER-COPY btt_auth_copay_detail_item
               EXCEPT btt_auth_copay_detail_item.auth_copay_detail_item_obj
                   TO buf_auth_copay_detail_item.

          ASSIGN
             btt_auth_copay_detail_item.auth_copay_detail_item_obj = buf_auth_copay_detail_item.auth_copay_detail_item_obj
             btt_auth_copay_detail_item.record_action              = "":U.

          VALIDATE buf_auth_copay_detail_item.

          FIND CURRENT buf_auth_copay_detail_item NO-LOCK.
        END. /* IF AVAILABLE buf_auth_copay_detail_item AND NOT lFailureOccurred THEN */
      END. /* IF btt_auth_copay_detail_item.record_action = "MODIFY":U THEN */

      IF btt_auth_copay_detail_item.record_action = "DELETE":U THEN
      DO:
        /*
          This routine will ensure that all dependencies will also be removed
        */
        RUN _deleteAuthCopayDetailItem IN TARGET-PROCEDURE (INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj).

        IF NOT CAN-FIND(FIRST btt_auth_copay_detail_item_error NO-LOCK
                        WHERE btt_auth_copay_detail_item_error.owning_entity_mnemonic = "hacci":U
                          AND btt_auth_copay_detail_item_error.owning_obj = btt_auth_copay_detail_item.auth_copay_detail_item_obj)
        THEN
           DELETE btt_auth_copay_detail_item.
      END. /* END - IF btt_auth_copay_detail_item.record_action = "DELETE":U THEN */
    END. /* FOR EACH btt_auth_copay_detail_item EXCLUSIVE-LOCK: */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

 { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject." }
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCopayDetailItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCopayDetailItem Procedure
PROCEDURE _validateAuthCopayDetailItem PRIVATE :
/*------------------------------------------------------------------------------
  Purpose    : Auth Copay Detail Item Validation
  Parameters : Current Buffer to be validated
  Notes      :
------------------------------------------------------------------------------*/

  { ma/app/maauthcopayservaldetitem.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


