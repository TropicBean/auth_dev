&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Authorisations Data Access Layer
             All authorisation busines logic will be
             placed in ma/app/maauthservicestack.p

    Author : Andrewd

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i }

{ mip/inc/mipdefshared.i      }

{ ma/inc/maauthds.i           }
{ ma/inc/maauthtypeds.i       }
{ ma/inc/matariffds.i         }
{ ma/inc/mainsurertt.i        }
{ ma/inc/maauthtypeconfigtt.i }
{ ma/inc/maexternalreferenceds.i    }

{mip/inc/mipgetttords.i &DataSet-01 = dsAuthorisation }

&SCOPED-DEFINE FindResultRecord FIND FIRST tt_auth_result EXCLUSIVE-LOCK NO-ERROR. {&ResetError} IF NOT AVAILABLE tt_auth_result THEN CREATE tt_auth_result.

&SCOPED-DEFINE ActionList delete,modify,partialsave

&SCOPED-DEFINE ModifyList modify,partialsave

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnUserHasRole) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnUserHasRole Procedure
FUNCTION fnUserHasRole RETURNS LOGICAL
  ( ipcRoleCodeList AS CHARACTER )  FORWARD.

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB)
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 27.71
         WIDTH              = 61.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-fetchAuthorisationDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthorisationDataset Procedure
PROCEDURE fetchAuthorisationDataset :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Authorisation Data
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
    Author    : Andrewd

      NB- DEVELOPERS ******  ALL CHANGES BEFORE DELTA 185 SHOULD GO IN THE SUPER PROCEDURE
          AND ALL CHANGES AFTER DELTA 185 SHOULD BE IN THE STANDARD STACK ************
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
  DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
  DEFINE INPUT        PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE iBuffer                     AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iBatchSize                  AS INTEGER          NO-UNDO.
  DEFINE VARIABLE lDataSecured                AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSuccess                    AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle  AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hDatasetHandle              AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hBufferHandle               AS HANDLE           NO-UNDO.
  DEFINE VARIABLE cDataSecured                AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cBatchSize                  AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE oSearchObject               AS cls.maauthsearch NO-UNDO.

  DEFINE DATA-SOURCE srcAuth                  FOR hat_auth.
  DEFINE DATA-SOURCE srcAuthCoding            FOR hat_auth_coding.
  DEFINE DATA-SOURCE srcAuthDetail            FOR hat_auth_detail.
  DEFINE DATA-SOURCE srcAuthProvider          FOR hat_auth_provider.
  DEFINE DATA-SOURCE srcAuthMCSaving          FOR hat_auth_mc_savings.
  DEFINE DATA-SOURCE srcAuthEpisode           FOR hat_auth_episode.
  DEFINE DATA-SOURCE srcAuthCrosswalk         FOR hat_auth_crosswalk.
  DEFINE DATA-SOURCE srcAuthCopay             FOR hat_auth_copay.
  DEFINE DATA-SOURCE srcAuthLimit             FOR hat_auth_limit.

  DEFINE DATA-SOURCE srcAuthHistory           FOR hah_auth_history.
  DEFINE DATA-SOURCE srcAuthCodingHistory     FOR hah_auth_coding_history.
  DEFINE DATA-SOURCE srcAuthDetailHistory     FOR hah_auth_detail_history.
  DEFINE DATA-SOURCE srcAuthProviderHistory   FOR hah_auth_provider_history.
  DEFINE DATA-SOURCE srcAuthMCSavingHistory   FOR hah_auth_mc_savings_history.
  DEFINE DATA-SOURCE srcAuthEpisodeHistory    FOR hah_auth_episode_history.
  DEFINE DATA-SOURCE srcAuthCrosswalkHistory  FOR hah_auth_crosswalk_history.
  DEFINE DATA-SOURCE srcAuthCopayHistory      FOR hah_auth_copay_history.
  DEFINE DATA-SOURCE srcAuthLimitHistory      FOR hah_auth_limit_history.


  ASSIGN
     oSearchObject              = NEW cls.maauthsearch(INPUT DATASET-HANDLE iophDatasetHandle)
     hFilterCriteriaTableHandle = iophFilterCriteriaTableHandle
     hDatasetHandle             = iophDatasetHandle

     lSuccess                   = oSearchObject:setCriteria(TABLE-HANDLE iophFilterCriteriaTableHandle)


     lSuccess                   = oSearchObject:getCriteria("BatchSize":U      , cBatchSize)
     iBatchSize                 = INTEGER(cBatchSize)
     lSuccess                   = oSearchObject:getCriteria("DataSecured":U, cDataSecured)
     lDataSecured               = (IF cDataSecured = ? OR cDataSecured = "":U
                                   THEN FALSE
                                   ELSE LOGICAL(cDataSecured)) NO-ERROR.

  IF lDataSecured THEN
  DO:
    { ma/mem/mahatauthacc.i }
  END. /*IF lDataSecured THEN*/


  BufferBlock:
  DO iBuffer = 1 TO oSearchObject:DatasetHandle:NUM-BUFFERS:

    hBufferHandle = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE(iBuffer).

    /*If a buffer list has been specified then we will exclude all other buffers from being filled*/
    IF NOT CAN-DO(ipcWhatToGet, hBufferHandle:NAME) THEN
    DO:
      hBufferHandle:FILL-MODE = "NO-FILL":U.
      NEXT BufferBlock.
    END. /*IF NOT CAN-DO(ipcWhatToGet, hBuffer:NAME) THEN*/

    CASE hBufferHandle:NAME:

      WHEN "tt_auth":U THEN
      DO:
        lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuth:HANDLE).

        IF iBatchSize <> ?
        THEN
          ASSIGN hBufferHandle:BATCH-SIZE = iBatchSize.
      END.
      WHEN "tt_auth_provider":U           THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthProvider:HANDLE           ).
      WHEN "tt_auth_coding":U             THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCoding:HANDLE             ).

      WHEN "tt_auth_detail":U             THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthDetail:HANDLE             ).
      WHEN "tt_auth_mc_savings":U         THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthMCSaving:HANDLE           ).
      WHEN "tt_auth_episode":U            THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthEpisode:HANDLE            ).
      WHEN "tt_auth_crosswalk":U          THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCrosswalk:HANDLE          ).
      WHEN "tt_auth_copay":U              THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCopay:HANDLE              ).
      WHEN "tt_auth_limit":U              THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthLimit:HANDLE              ).

      WHEN "tt_auth_history":U            THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthHistory:HANDLE            ).
      WHEN "tt_auth_coding_history":U     THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCodingHistory:HANDLE      ).
      WHEN "tt_auth_detail_history":U     THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthDetailHistory:HANDLE      ).
      WHEN "tt_auth_provider_history":U   THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthProviderHistory:HANDLE    ).
      WHEN "tt_auth_mc_savings_history":U THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthMCSavingHistory:HANDLE    ).
      WHEN "tt_auth_episode_history":U    THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthEpisodeHistory:HANDLE     ).
      WHEN "tt_auth_crosswalk_history":U  THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCrosswalkHistory:HANDLE   ).
      WHEN "tt_auth_copay_history":U      THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCopayHistory:HANDLE       ).
      WHEN "tt_auth_limit_history":U      THEN lSuccess = hBufferHandle:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthLimitHistory:HANDLE       ).

      OTHERWISE hBufferHandle:FILL-MODE = "NO-FILL":U.

    END CASE. /* CASE hBufferHandle:NAME: */

  END. /* BufferBlock: */


  oSearchObject:DatasetHandle:SET-CALLBACK-PROCEDURE("AFTER-FILL","_afterFillAuth", TARGET-PROCEDURE).


  ASSIGN
     lSuccess                      = oSearchObject:populateDataset(INPUT iophFilterCriteriaTableHandle)
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

&IF DEFINED(EXCLUDE-fetchAuthorisations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthorisations Procedure
PROCEDURE fetchAuthorisations :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthObj   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcMemNum    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER DATASET-HANDLE ophAuthDataset.

  DEFINE VARIABLE oSearch  AS cls.maauthsearch NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.


  ASSIGN
     ipdAuthObj   = (IF ipdAuthObj   = ? THEN 0.00        ELSE ipdAuthObj)
     ipcMemNum    = (IF ipcMemNum    = ? THEN "":U        ELSE ipcMemNum)
     ipcWhatToGet = (IF ipcWhatToGet = ? THEN "tt_auth":U ELSE ipcWhatToGet).

  IF ipdAuthObj <> 0.00 OR ipcMemNum <> "":U THEN
  DO:
    oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).

    ASSIGN
       lSuccess = oSearch:setCriteria("BufferList":U, ipcWhatToGet)

       lSuccess = (IF ipdAuthObj <> 0.00
                   THEN oSearch:setFilterCriteria("tt_auth.auth_obj":U, "=":U, ipdAuthObj)
                   ELSE TRUE)

       lSuccess = (IF ipcMemNum <> "":U
                   THEN oSearch:setFilterCriteria("tt_auth.mem_num":U, "=":U, ipcMemNum)
                   ELSE TRUE).

    oSearch:fetchData().
  END. /*IF ipdAuthObj <> 0.00 OR ipcMemNum <> "":U THEN*/

  /* For some reason, using OUTPUT DATASET breaks getTTorDS. Using a DATASET-HANDLE works though */
  ASSIGN ophAuthDataset = oSearch:DatasetHandle.

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reselectCrosswalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reselectCrosswalk Procedure
PROCEDURE reselectCrosswalk :
/*------------------------------------------------------------------------------
  Purpose   : reselectCrosswalk
              Based on _deleteAuth to delete only related records, but not the
              actual Auth or the main provider.
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred AS LOGICAL           NO-UNDO.

  DEFINE BUFFER buf_auth             FOR hat_auth.
  DEFINE BUFFER hat_auth             FOR hat_auth.
  DEFINE BUFFER buf_auth_coding      FOR hat_auth_coding.
  DEFINE BUFFER buf_auth_detail      FOR hat_auth_detail.
  DEFINE BUFFER buf_auth_provider    FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_savings     FOR hat_auth_mc_savings.
  DEFINE BUFFER buf_auth_crosswalk   FOR hat_auth_crosswalk.

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth EXCLUSIVE-LOCK
         WHERE buf_auth.auth_obj = ipdAuthObj
      NO-ERROR NO-WAIT.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U
    THEN DO:
      {&ResetError}

      ASSIGN lFailureOccurred = TRUE.

      oErrorObject:addError
        ( INPUT "hatau":U,
          INPUT ipdAuthObj,
          INPUT "":U,
          INPUT 0,
          INPUT "MA":U,
          INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
          INPUT "Authorisation Header (Obj = ":U + STRING(ipdAuthObj) + ")" ).
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/


    IF AVAILABLE buf_auth AND NOT lFailureOccurred
    THEN DO:
      /* Clear the End Date and Total LOS on the Auth header */
      ASSIGN
        buf_auth.end_date   = ?
        buf_auth.total_los  = 0.00.

      /* Remove Coding Records */
      IF NOT lFailureOccurred
      THEN DO:
        CodingBlock:
        FOR EACH  buf_auth_coding NO-LOCK
            WHERE buf_auth_coding.auth_obj = ipdAuthObj:

          RUN _deleteAuthCoding IN TARGET-PROCEDURE
            ( INPUT buf_auth_coding.auth_coding_obj,
              INPUT 0,
              INPUT-OUTPUT oErrorObject ).

          IF AVAILABLE buf_auth_coding
          THEN
            ASSIGN
              lFailureOccurred = oErrorObject:CanFind("hatac:":U + buf_auth_coding.owning_entity_mnemonic, buf_auth_coding.auth_coding_obj, "":U).

          IF lFailureOccurred THEN LEAVE CodingBlock.
        END.  /* FOR EACH buf_auth_coding NO-LOCK */
      END. /*IF NOT lFailureOccurred THEN*/


      /* Remove Detail Records */
      IF NOT lFailureOccurred
      THEN DO:
        DetailBlock:
        FOR EACH  buf_auth_detail NO-LOCK
            WHERE buf_auth_detail.auth_obj = ipdAuthObj:

          RUN _deleteAuthDetail IN TARGET-PROCEDURE
            ( INPUT buf_auth_detail.auth_detail_obj,
              INPUT 0,
              INPUT FALSE,
              INPUT-OUTPUT oErrorObject ).

          IF AVAILABLE buf_auth_detail
          THEN
            ASSIGN lFailureOccurred = oErrorObject:CanFind("hatad:":U + buf_auth_detail.owning_entity_mnemonic, buf_auth_detail.auth_detail_obj, "":U).

          IF lFailureOccurred THEN LEAVE DetailBlock.
        END. /*FOR EACH buf_auth_detail NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/


      /* Remove Provider Records */
      IF NOT lFailureOccurred
      THEN DO:
        ProviderBlock:
        FOR EACH  buf_auth_provider NO-LOCK
            WHERE buf_auth_provider.auth_obj = ipdAuthObj
            AND   buf_auth_provider.main_provider = FALSE:

          RUN _deleteAuthProvider IN TARGET-PROCEDURE
            ( INPUT buf_auth_provider.auth_provider_obj,
              INPUT 0,
              INPUT-OUTPUT oErrorObject ).

          IF AVAILABLE buf_auth_provider
          THEN
            ASSIGN lFailureOccurred = oErrorObject:CanFind("hatap":U, buf_auth_provider.auth_provider_obj, "":U).

          IF lFailureOccurred THEN LEAVE ProviderBlock.
        END. /*FOR EACH buf_auth_provider NO-LOCK*/

        FIND FIRST buf_auth_provider EXCLUSIVE-LOCK
             WHERE buf_auth_provider.auth_obj = ipdAuthObj
               AND buf_auth_provider.main_provider = TRUE
          NO-ERROR.

        IF AVAILABLE buf_auth_provider
        THEN DO:
          ASSIGN buf_auth_provider.end_date           = ?
                 buf_auth_provider.override_base_rate = "":U
                 buf_auth_provider.override_ars_rate  = "":U
                 buf_auth_provider.rate_change_type   = "ma_acAuthRateChangeTypeNone":U .
        END. /* IF AVAILABLE buf_auth_provider */
      END. /*IF NOT lFailureOccurred THEN*/


      /* Remove Savings Records */
      IF NOT lFailureOccurred
      THEN DO:
        SavingsBlock:
        FOR EACH  buf_auth_savings NO-LOCK
            WHERE buf_auth_savings.auth_obj = ipdAuthObj:

          RUN _deleteAuthMCSavings IN TARGET-PROCEDURE
            ( INPUT buf_auth_savings.auth_mc_savings_obj,
              INPUT 0,
              INPUT-OUTPUT oErrorObject ).

          IF AVAILABLE buf_auth_savings
          THEN
            ASSIGN lFailureOccurred = oErrorObject:CanFind("hatas":U, buf_auth_savings.auth_mc_savings_obj, "":U).

          IF lFailureOccurred THEN LEAVE SavingsBlock.
        END. /*FOR EACH buf_auth_savings NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/


      /* Remove Auth Crosswalks */
      IF NOT lFailureOccurred
      THEN DO:
        CodingBlock:
        FOR EACH  buf_auth_crosswalk  NO-LOCK
            WHERE buf_auth_crosswalk.auth_obj = ipdAuthObj:

          RUN _deleteAuthCrosswalk IN TARGET-PROCEDURE
            ( INPUT buf_auth_crosswalk.auth_crosswalk_obj,
              INPUT 0,
              INPUT-OUTPUT oErrorObject ).

          IF AVAILABLE buf_auth_crosswalk
          THEN
            ASSIGN
              lFailureOccurred = oErrorObject:CanFind("hataw":U, buf_auth_crosswalk.auth_crosswalk_obj, "":U).

          IF lFailureOccurred THEN LEAVE CodingBlock.
        END.  /* FOR EACH buf_auth_coding NO-LOCK */
      END. /*IF NOT lFailureOccurred THEN*/

      /* Clear all values of existing External References */
      IF NOT lFailureOccurred
      THEN DO:
        DATASET dsExternalReference:EMPTY-DATASET().

        /* Get external reference data */
        mipEnv:Health:AuthService:getAuthExternalReference(INPUT buf_auth.auth_obj, INPUT-OUTPUT DATASET dsExternalReference BY-REFERENCE).

        FOR EACH tt_extref :
          ASSIGN
            tt_extref.record_action       = "MODIFY":U
            tt_extref.reference-value     = "":U
            tt_extref.transaction-date    = buf_auth.start_date
            tt_extref.reference-mandatory = false  .
        END. /*FOR EACH tt_extref */

        IF CAN-FIND(FIRST tt_extref)
        THEN
          mipEnv:Health:maUtility:saveExternalReference(INPUT-OUTPUT DATASET dsExternalReference BY-REFERENCE).
      END. /* IF NOT lFailureOccured */

      /* Check if we had any failures after deleting the history records */
      ASSIGN lFailureOccurred = lFailureOccurred OR oErrorObject:CanFind("hatau":U, ipdAuthObj, "":U).

    END.  /* IF AVAILABLE buf_auth AND NOT lFailureOccurred THEN */
  END.  /* DO TRANSACTION ON ERROR UNDO, THROW: */

  { mip/inc/mipcatcherror.i &FINALLY="
    IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT(oErrorObject).
    DATASET dsExternalReference:EMPTY-DATASET().
    "}
&ENDIF
END PROCEDURE.  /* reselectCrosswalk */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuth Procedure
PROCEDURE saveAuth :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/

  { ma/app/maauthdatasaveauth.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthCoding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthCoding Procedure
PROCEDURE saveAuthCoding :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Coding Record
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/

  { ma/app/maauthdatasavecoding.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthCopay Procedure
PROCEDURE saveAuthCopay :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Co-payment Record
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/

  { ma/app/maauthdatasavecopay.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthCrosswalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthCrosswalk Procedure
PROCEDURE saveAuthCrosswalk :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Crosswalk Record
              Healthcare Auth data access service: Save authorisation crosswalk record so we
              can keep track of crosswalks that have been selected on an auth.
  Parameters:
  Notes     : Data Access Only !!!
-------------------------------------------------------------------------------- */
  DEFINE PARAMETER BUFFER btt_auth_crosswalk FOR tt_auth_crosswalk .

  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.


&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth            FOR hat_auth.
  DEFINE BUFFER buf_auth_crosswalk  FOR hat_auth_crosswalk.

  DEFINE VARIABLE lBufferCompare AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject   AS cls.maerrorobject NO-UNDO.


  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  IF AVAILABLE btt_auth_crosswalk THEN
  DO:
    {&FindResultRecord}

    IF CAN-DO("{&ModifyList}":U, btt_auth_crosswalk.record_action) THEN
    DO
    TRANSACTION:
      /*
        Ensure that we have a valid parent
      */
      IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
                      WHERE buf_auth.auth_obj = btt_auth_crosswalk.auth_obj) THEN
      DO:
        oErrorObject:addError(INPUT "hataw":U,
                              INPUT btt_auth_crosswalk.auth_crosswalk_obj,
                              INPUT "":U,
                              INPUT btt_auth_crosswalk.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /*    The "&1" specified is invalid */
                              INPUT "Auth Crosswalk Parent record (Obj = ":U + STRING(btt_auth_crosswalk.auth_obj) + ")").

      END. /*IF NOT CAN-FIND(FIRST buf_auth NO-LOCK*/

      IF oErrorObject:CanFind("hataw":U, btt_auth_crosswalk.auth_crosswalk_obj, "":U) THEN
        RETURN.

      FIND FIRST buf_auth_crosswalk EXCLUSIVE-LOCK
           WHERE buf_auth_crosswalk.auth_crosswalk_obj = btt_auth_crosswalk.auth_crosswalk_obj
        NO-ERROR NO-WAIT.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }

      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:

        FIND FIRST buf_auth_crosswalk EXCLUSIVE-LOCK
             WHERE buf_auth_crosswalk.auth_obj      = btt_auth_crosswalk.auth_obj
               AND buf_auth_crosswalk.crosswalk_obj = btt_auth_crosswalk.crosswalk_obj
             NO-ERROR NO-WAIT.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      END. /* IF NOT AVAILABLE buf_auth_crosswalk */

      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}

        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.

        oErrorObject:addError(INPUT "hataw":U,
                              INPUT btt_auth_crosswalk.auth_crosswalk_obj,
                              INPUT "":U,
                              INPUT btt_auth_crosswalk.line_number,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again */
                              INPUT "Authorisation Crosswalk":U).

      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/

      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
      DO:
        {&ResetError}

        IF NOT oErrorObject:CanFind("hataw":U, btt_auth_crosswalk.auth_crosswalk_obj,"":U)
        THEN
          CREATE buf_auth_crosswalk.

      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/

      IF AVAILABLE buf_auth_crosswalk
      AND NOT oErrorObject:CanFind("hataw":U,btt_auth_crosswalk.auth_crosswalk_obj,"":U) THEN
      DO:
        /*
          Update the tally depending on whether we are creating or updating
        */
        IF NEW buf_auth_crosswalk
        THEN ASSIGN tt_auth_result.records_created  = tt_auth_result.records_created  + 1.
        ELSE ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1.


        BUFFER-COPY btt_auth_crosswalk
             EXCEPT btt_auth_crosswalk.auth_crosswalk_obj
                 TO buf_auth_crosswalk.

        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_crosswalk.auth_crosswalk_obj = buf_auth_crosswalk.auth_crosswalk_obj.

        VALIDATE buf_auth_crosswalk.

        FIND CURRENT buf_auth_crosswalk NO-LOCK.

      END. /*IF AVAILABLE buf_auth_crosswalk AND NOT oErrorObject:CanFind("hataw":U, buf_auth_crosswalk.auth_crosswalk_obj,"":U) THEN*/
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_mc_savings.record_action) THEN*/

    IF btt_auth_crosswalk.record_action = "Delete":U THEN
      RUN _deleteAuthCrosswalk IN TARGET-PROCEDURE ( INPUT btt_auth_crosswalk.auth_crosswalk_obj, INPUT btt_auth_crosswalk.line_number, INPUT-OUTPUT oErrorObject ).

  END. /*IF AVAILABLE btt_auth_crosswalk THEN*/

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthDetail Procedure
PROCEDURE saveAuthDetail :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Detail Record
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/

  { ma/app/maauthdatasavedetail.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthEpisode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthEpisode Procedure
PROCEDURE saveAuthEpisode :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Episode Record
  Parameters:
  Notes     : Data Access Only !!!
-------------------------------------------------------------------------------- */

  { ma/app/maauthdatasaveepisode.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthLimit Procedure
PROCEDURE saveAuthLimit :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Limit Record
  Parameters:
  Notes     : Data Access Only !!!
-------------------------------------------------------------------------------- */

  { ma/app/maauthdatasavelimit.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthMCSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthMCSavings Procedure
PROCEDURE saveAuthMCSavings :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Savings Record
  Parameters:
  Notes     : Data Access Only !!!
-------------------------------------------------------------------------------- */

  { ma/app/maauthdatasavemcsavings.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthProvider Procedure
PROCEDURE saveAuthProvider :
/*------------------------------------------------------------------------------
  Purpose   : Save Authorisation Provider Record
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/

  { ma/app/maauthdatasaveprovider.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateAuthorisation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateAuthorisation Procedure
PROCEDURE validateAuthorisation :
/*------------------------------------------------------------------------------
  Purpose   : Wrapper validation procedure
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.


  RUN _validateAuth          IN TARGET-PROCEDURE.

  RUN _validateAuthCoding    IN TARGET-PROCEDURE.

  RUN _validateAuthDetail    IN TARGET-PROCEDURE.

  RUN _validateAuthEpisode   IN TARGET-PROCEDURE.

  RUN _validateAuthProvider  IN TARGET-PROCEDURE.

  RUN _validateAuthMCSavings IN TARGET-PROCEDURE.

  RUN _validateAuthCopay     IN TARGET-PROCEDURE.

  RUN _validateAuthLimit     IN TARGET-PROCEDURE.

  RUN _validateAuthCrosswalk IN TARGET-PROCEDURE.


  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_afterFillAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _afterFillAuth Procedure
PROCEDURE _afterFillAuth :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE iphDataset.

  DEFINE VARIABLE httAuth        AS HANDLE                NO-UNDO.
  DEFINE VARIABLE httDetail      AS HANDLE                NO-UNDO.
  DEFINE VARIABLE httCoding      AS HANDLE                NO-UNDO.
  DEFINE VARIABLE hQuery         AS HANDLE                NO-UNDO.
  DEFINE VARIABLE iOptionCode    AS INTEGER               NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE cTariffCode    AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE dTariffObj     AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj    AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE cError         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning       AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cAlertMessage  AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE lPrimaryCode   AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lMainCode      AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE iicdIcnt       AS INTEGER               NO-UNDO INITIAL 1.
  DEFINE VARIABLE icptIcnt       AS INTEGER               NO-UNDO INITIAL 1.
  DEFINE VARIABLE cICDRecordType AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cCPTRecordType AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTheCodeVal    AS CHARACTER             NO-UNDO.

  DEFINE VARIABLE oTariffType    AS cls.matarifftype      NO-UNDO.
  DEFINE VARIABLE oTFSearch      AS ma.cls.matariffsearch NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  ASSIGN
     httAuth     = iphDataset:GET-BUFFER-HANDLE("tt_auth":U)
     httDetail   = iphDataset:GET-BUFFER-HANDLE("tt_auth_detail":U)
     httCoding   = iphDataset:GET-BUFFER-HANDLE("tt_auth_coding":U)
     oTariffType = NEW cls.matarifftype().

  /*
    Auth buffer updates
  */
  CREATE QUERY hQuery.

  hQuery:SET-BUFFERS(httAuth).
  hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1":U, httAuth:NAME)).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  DO WHILE NOT hQuery:QUERY-OFF-END:

    IF httAuth:AVAILABLE THEN
    DO:
      ASSIGN iOptionCode = INTEGER(httAuth::option_code).

      IF iOptionCode = 0
      THEN
        mipEnv:Health:maMember:getMemberOption(INPUT httAuth::mem_num, INPUT DATE(httAuth::start_date), OUTPUT iOptionCode).

      ASSIGN httAuth::option_code = iOptionCode.

    END. /*IF httAuth:AVAILABLE THEN */

    hQuery:GET-NEXT().

  END. /*DO WHILE NOT hQuery:QUERY-OFF-END:*/

  /*
    Cleanup after query created for auth buffer
  */
  IF VALID-HANDLE(hQuery) THEN
  DO:
    IF hQuery:IS-OPEN
    THEN hQuery:QUERY-CLOSE().

    DELETE OBJECT hQuery.
  END. /*IF VALID-HANDLE(hQuery) THEN*/

  /*
    Auth coding buffer updates

    Added for task MMP-465 to allow for correct ordering of ICD and CPT lines.
    Primary or Main line is to always be at the top
  */
  CREATE QUERY hQuery.

  hQuery:SET-BUFFERS(httCoding).
  hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1":U, httCoding:NAME)).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  DO WHILE NOT hQuery:QUERY-OFF-END:

    IF httCoding:AVAILABLE THEN
    DO:

      ASSIGN
        lPrimaryCode = LOGICAL(httCoding::primary_code)
        lMainCode    = LOGICAL(httCoding::main_code)
        cTheCodeVal  = STRING(httCoding::owning_alt_value) .

      /*
        We want the primary or main to always be at the top, thus they get a line number of one
      */
      IF lPrimaryCode = TRUE OR lMainCode = TRUE
      THEN
        ASSIGN
            httCoding::line_number = 1.

      /*
        All other lines are sequenced with a line number according to their mic_acronym sequence
      */
      IF lPrimaryCode = FALSE OR lMainCode = FALSE THEN
      DO:

        IF httCoding::owning_entity_mnemonic = "diagnos":U THEN
        DO:
            FIND mic_acronym NO-LOCK
            WHERE mic_acronym.category_key = "ma_acICDcodingType":U
            AND mic_acronym.acronym_code = httCoding::coding_type
            NO-ERROR.
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE mic_acronym
            THEN
            DO:
                /* Below we check if this is the first record being evaluated or if the record type has changed, if so we reset the icdIcnt to 1 */
                IF cICDRecordType = "" OR httCoding::coding_type <> cICDRecordType
                THEN
                  ASSIGN iicdIcnt = 1.

                ASSIGN
                    httCoding::LINE_number = (INTEGER(mic_acronym.acronym_sequence) * 100) + iicdIcnt
                    cICDRecordType = httCoding::coding_type /* This sets the cICDRecordType so that it can be evaluated in the nex pass to see if icdICnt should be reset or not */
                    iicdIcnt = iicdIcnt + 1. /* This increments the icdICnt by one for the next record */
            END. /* End IF AVAILABLE mic_acronym */

        END. /* IF httCoding::owning_entity_mnemonic = "diagnos" THEN */

        IF httCoding::owning_entity_mnemonic = "hlmck":U THEN
        DO:
            FIND mic_acronym NO-LOCK
              WHERE mic_acronym.category_key = "ma_acCPTcodingType":U
              AND   mic_acronym.acronym_code = httCoding::coding_type
              NO-ERROR.
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE mic_acronym
            THEN
            DO:
                IF cCPTRecordType = "" OR httCoding::coding_type <> cCPTRecordType
                THEN
                    ASSIGN icptIcnt = 1.

                ASSIGN
                    httCoding::LINE_number = (INTEGER(mic_acronym.acronym_sequence) * 100) + icptIcnt
                    cCPTRecordType = httCoding::coding_type /* This sets the cCPTRecordType so that it can be evaluated in the nex pass to see if cptICnt should be reset or not */
                    icptIcnt = icptIcnt + 1. /* This increments the cptICnt by one for the next record */
            END. /* END IF AVAILABLE mic_acronym */

        END. /* IF httCoding::owning_entity_mnemonic = "hlmck" THEN */

      END. /* IF lPrimaryCode = FALSE OR lMainCode = FALSE THEN */

    END. /*IF httAuth:AVAILABLE THEN */

    hQuery:GET-NEXT().

  END. /*DO WHILE NOT hQuery:QUERY-OFF-END: for Auth Coding Buffer Updates */

  /*
    Cleanup after query created for auth coding buffer
  */
  IF VALID-HANDLE(hQuery) THEN
  DO:
    IF hQuery:IS-OPEN
    THEN hQuery:QUERY-CLOSE().

    DELETE OBJECT hQuery.
  END. /*IF VALID-HANDLE(hQuery) THEN*/


  /*
    Auth detail buffer updates
  */
  CREATE QUERY hQuery.

  hQuery:SET-BUFFERS(httDetail).
  hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1":U, httDetail:NAME)).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  DO WHILE NOT hQuery:QUERY-OFF-END:

    IF httDetail:AVAILABLE THEN
    DO:
      /*
        Assign tariff type code where a tariff type is populated
      */
      IF DECIMAL(httDetail::tariff_type_obj) <> 0.00 THEN
      DO:
        oTariffType:focusTariffType(DECIMAL(httDetail::tariff_type_obj)).

        IF oTariffType:TariffTypeInFocus AND oTariffType:TariffTypeKey = "ma_acTariffTypeCatLOC":U
        THEN
          ASSIGN httDetail::loc_value = oTariffType:TariffTypeCode.

      END. /*IF DECIMAL(httDetail::tariff_type_obj) <> 0.00 THEN*/
    END. /*IF httDetail:AVAILABLE THEN */

    hQuery:GET-NEXT().

  END. /*DO WHILE NOT hQuery:QUERY-OFF-END:*/

&ENDIF

  { mip/inc/mipcatcherror.i
    &FINALLY = "DATASET dsTariff:EMPTY-DATASET().

                IF VALID-OBJECT(oTariffType)
                THEN DELETE OBJECT oTariffType.

                IF VALID-OBJECT(oTFSearch)
                THEN DELETE OBJECT oTFSearch.

                IF VALID-HANDLE(hQuery) THEN
                DO:
                  IF hQuery:IS-OPEN
                  THEN hQuery:QUERY-CLOSE().

                  DELETE OBJECT hQuery.
                END.
                 "}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuth Procedure
PROCEDURE _deleteAuth :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lFailureOccurred AS LOGICAL NO-UNDO.

  DEFINE BUFFER hat_auth             FOR hat_auth.
  DEFINE BUFFER buf_auth             FOR hat_auth.
  DEFINE BUFFER buf_auth_coding      FOR hat_auth_coding.
  DEFINE BUFFER buf_auth_detail      FOR hat_auth_detail.
  DEFINE BUFFER buf_auth_provider    FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_savings     FOR hat_auth_mc_savings.
  DEFINE BUFFER buf_auth_copay       FOR hat_auth_copay.
  DEFINE BUFFER buf_auth_limit       FOR hat_auth_limit.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth EXCLUSIVE-LOCK
         WHERE buf_auth.auth_obj = ipdAuthObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445,PROGRESS:565' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN
    DO:
      {&ResetError}

      ASSIGN lFailureOccurred = TRUE.

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 20,  /*The record has been deleted by another user. */
                              INPUT "" ).
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ASSIGN lFailureOccurred = TRUE.

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Header (Obj = ":U + STRING(ipdAuthObj) + ")" ).
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/


    IF AVAILABLE buf_auth AND NOT lFailureOccurred THEN
    DO:
      /*
        Remove episode record if there are no other auths linked to the episode record
      */
      IF buf_auth.auth_episode_obj <> 0.00 AND NOT lFailureOccurred THEN
      DO:
        IF NOT CAN-FIND(FIRST hat_auth NO-LOCK
                        WHERE hat_auth.auth_episode_obj = buf_auth.auth_episode_obj
                          AND ROWID(hat_auth) <> ROWID(buf_auth))
        THEN
          RUN _deleteAuthEpisode IN TARGET-PROCEDURE (INPUT buf_auth.auth_episode_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

      END. /*IF buf_auth.auth_episode_obj <> 0.00 AND NOT lFailureOccurred THEN*/

      /*
        Remove Coding Records
      */
      IF NOT lFailureOccurred THEN
      DO:
        CodingBlock:
        FOR EACH buf_auth_coding NO-LOCK
           WHERE buf_auth_coding.auth_obj = ipdAuthObj:

          RUN _deleteAuthCoding IN TARGET-PROCEDURE (INPUT buf_auth_coding.auth_coding_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

          ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_coding
                                     THEN
                                       ipoErrorObject:CanFind("hatac:":U + buf_auth_coding.owning_entity_mnemonic, buf_auth_coding.auth_coding_obj, "":U)
                                     ELSE
                                       lFailureOccurred).

          IF lFailureOccurred
          THEN
            LEAVE CodingBlock.

        END. /*FOR EACH buf_auth_coding NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/

      /*
        Remove Detail Records
      */
      IF NOT lFailureOccurred THEN
      DO:
        DetailBlock:
        FOR EACH buf_auth_detail NO-LOCK
           WHERE buf_auth_detail.auth_obj = ipdAuthObj:

          RUN _deleteAuthDetail IN TARGET-PROCEDURE (INPUT buf_auth_detail.auth_detail_obj, INPUT 0, INPUT FALSE, INPUT-OUTPUT ipoErrorObject).

          ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_detail
                                     THEN
                                       ipoErrorObject:CanFind("hatad:":U + buf_auth_detail.owning_entity_mnemonic, buf_auth_detail.auth_detail_obj, "":U)
                                     ELSE
                                       lFailureOccurred).

          IF lFailureOccurred
          THEN
            LEAVE DetailBlock.

        END. /*FOR EACH buf_auth_detail NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/

      /*
        Remove Provider Records
      */
      IF NOT lFailureOccurred THEN
      DO:
        ProviderBlock:
        FOR EACH buf_auth_provider NO-LOCK
           WHERE buf_auth_provider.auth_obj = ipdAuthObj:

          RUN _deleteAuthProvider IN TARGET-PROCEDURE (INPUT buf_auth_provider.auth_provider_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

          ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_provider
                                     THEN
                                       ipoErrorObject:CanFind("hatap":U, buf_auth_provider.auth_provider_obj, "":U)
                                     ELSE
                                       lFailureOccurred).

          IF lFailureOccurred
          THEN
            LEAVE ProviderBlock.

        END. /*FOR EACH buf_auth_provider NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/

      /*
        Remove Savings Records
      */
      IF NOT lFailureOccurred THEN
      DO:
        SavingsBlock:
        FOR EACH buf_auth_savings NO-LOCK
           WHERE buf_auth_savings.auth_obj = ipdAuthObj:

          RUN _deleteAuthMCSavings IN TARGET-PROCEDURE (INPUT buf_auth_savings.auth_mc_savings_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

          ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_savings
                                     THEN
                                       ipoErrorObject:CanFind("hatas":U, buf_auth_savings.auth_mc_savings_obj, "":U)
                                     ELSE
                                       lFailureOccurred).

          IF lFailureOccurred
          THEN
            LEAVE SavingsBlock.

        END. /*FOR EACH buf_auth_savings NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/

      /*
        Remove Auth Copay Records
      */
      IF NOT lFailureOccurred THEN
      DO:
        AuthCopayBlock:
        FOR EACH buf_auth_copay NO-LOCK
           WHERE buf_auth_copay.auth_obj = ipdAuthObj:

          RUN _deleteAuthCopay IN TARGET-PROCEDURE (INPUT buf_auth_copay.auth_copay_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

          ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_copay
                                     THEN
                                       ipoErrorObject:CanFind("hatcp":U, buf_auth_copay.auth_copay_obj, "":U)
                                     ELSE
                                       lFailureOccurred).

          IF lFailureOccurred
          THEN
            LEAVE AuthCopayBlock.

        END. /*FOR EACH buf_auth_copay NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/

      /*
        Remove Auth Limit Records
      */
      IF NOT lFailureOccurred THEN
      DO:
        AuthLimitBlock:
        FOR EACH buf_auth_limit NO-LOCK
           WHERE buf_auth_limit.auth_obj = ipdAuthObj:

          RUN _deleteAuthLimit IN TARGET-PROCEDURE (INPUT buf_auth_limit.auth_limit_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

          ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_limit
                                     THEN
                                       ipoErrorObject:CanFind("hatal":U, buf_auth_limit.auth_limit_obj, "":U)
                                     ELSE
                                       lFailureOccurred).

          IF lFailureOccurred
          THEN
            LEAVE AuthLimitBlock.

        END. /*FOR EACH buf_auth_limit NO-LOCK*/
      END. /*IF NOT lFailureOccurred THEN*/

      /*
        Delete all Auth History records
        Note: The auth will not be deleted if it has been flagged as completed
      */
      RUN _deleteAuthHistory          IN TARGET-PROCEDURE (INPUT ipdAuthObj, INPUT ipiLineNumber, INPUT-OUTPUT ipoErrorObject).
      RUN _deleteAuthCodingHistory    IN TARGET-PROCEDURE (INPUT ipdAuthObj, INPUT ipiLineNumber, INPUT-OUTPUT ipoErrorObject).
      RUN _deleteAuthDetailHistory    IN TARGET-PROCEDURE (INPUT ipdAuthObj, INPUT ipiLineNumber, INPUT-OUTPUT ipoErrorObject).
      RUN _deleteAuthMCSavingsHistory IN TARGET-PROCEDURE (INPUT ipdAuthObj, INPUT ipiLineNumber, INPUT-OUTPUT ipoErrorObject).
      RUN _deleteAuthProviderHistory  IN TARGET-PROCEDURE (INPUT ipdAuthObj, INPUT ipiLineNumber, INPUT-OUTPUT ipoErrorObject).
      RUN _deleteAuthCopayHistory     IN TARGET-PROCEDURE (INPUT ipdAuthObj, INPUT ipiLineNumber, INPUT-OUTPUT ipoErrorObject).
      RUN _deleteAuthLimitHistory     IN TARGET-PROCEDURE (INPUT ipdAuthObj, INPUT ipiLineNumber, INPUT-OUTPUT ipoErrorObject).

      mipEnv:Health:AuthBusinessLogic:deleteAuthFlagHistory (INPUT "hatau":U, INPUT ipdAuthObj, INPUT "":U, INPUT-OUTPUT ipoErrorObject).


      /*
        Check if we had any failures after deleting the history records
      */
      ASSIGN lFailureOccurred = (IF ipoErrorObject:CanFind("hatau":U, ipdAuthObj, "":U) THEN TRUE ELSE lFailureOccurred).

      IF NOT lFailureOccurred THEN
      DO:
        DELETE buf_auth.

        ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
      END. /*IF lFailureOccurred THEN*/
    END. /*IF AVAILABLE buf_auth AND NOT lFailureOccurred THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCoding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCoding Procedure
PROCEDURE _deleteAuthCoding :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth coding record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthCodingObj  AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber     AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject    AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_coding FOR hat_auth_coding.
  DEFINE BUFFER buf_auth        FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth_coding EXCLUSIVE-LOCK
         WHERE buf_auth_coding.auth_coding_obj = ipdAuthCodingObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatac":U,
                              INPUT ipdAuthCodingObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation coding (Obj = ":U + STRING(ipdAuthCodingObj) + ")" ).
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/


    IF AVAILABLE buf_auth_coding AND NOT ipoErrorObject:CanFind("hatac":U, ipdAuthCodingObj, "":U) THEN
    DO:
      DELETE buf_auth_coding.

      ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_coding AND NOT ipoErrorObject:CanFind("hatac":U,ipdAuthCodingObj,"":U) THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCodingHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCodingHistory Procedure
PROCEDURE _deleteAuthCodingHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth coding history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.


&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_coding_history FOR hah_auth_coding_history.

  /*
    Remove Auth Coding History Records
  */
  HistoryBlock:
  FOR EACH hah_auth_coding_history NO-LOCK
     WHERE hah_auth_coding_history.auth_obj = ipdAuthObj:

    FIND FIRST buf_auth_coding_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_coding_history) = ROWID(hah_auth_coding_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Coding History (Obj = ":U  + STRING(hah_auth_coding_history.auth_coding_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_coding_history
    THEN
      DELETE buf_auth_coding_history.

  END. /*FOR EACH hah_auth_coding_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCopay Procedure
PROCEDURE _deleteAuthCopay :
/*------------------------------------------------------------------------------
  Purpose   : Remove Auth Co-payment savings record and all dependecies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthCopayObj   AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber     AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject    AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cTrackingMessage AS CHARACTER   NO-UNDO.

  DEFINE BUFFER buf_auth_copay FOR hat_auth_copay.
  DEFINE BUFFER buf_auth       FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth_copay EXCLUSIVE-LOCK
         WHERE buf_auth_copay.auth_copay_obj = ipdAuthCopayObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatcp":U,
                              INPUT ipdAuthCopayObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Co-payment (Obj = ":U + STRING(ipdAuthCopayObj) + ")").
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    ELSE IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN
    DO:
      {&ResetError}

      ASSIGN cTrackingMessage = "CopayAuth DELETE - buf_auth_copay not available with buf_auth_copay.auth_copay_obj=" + STRING(ipdAuthCopayObj).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
    END.  // ELSE IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN

    IF AVAILABLE buf_auth_copay AND NOT ipoErrorObject:CanFind("hatcp":U, ipdAuthCopayObj,"":U) THEN
    DO:
      DELETE buf_auth_copay.

      ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_copay AND NOT ipoErrorObject:CanFind("hatms":U,ipdAuthCopayObj,"":U) THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCopayHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCopayHistory Procedure
PROCEDURE _deleteAuthCopayHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove Auth Co-payment history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.

 &IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_copay_history FOR hah_auth_copay_history.

  /*
    Remove Auth Co-payment History Records
  */
  HistoryBlock:
  FOR EACH hah_auth_copay_history NO-LOCK
     WHERE hah_auth_copay_history.auth_obj = ipdAuthObj:

    FIND FIRST buf_auth_copay_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_copay_history) = ROWID(hah_auth_copay_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatcp":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Co-payment History (Obj = ":U + STRING(hah_auth_copay_history.auth_copay_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_copay_history
    THEN
      DELETE buf_auth_copay_history.
  END. /*FOR EACH hah_auth_copay_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthCrosswalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthCrosswalk Procedure
PROCEDURE _deleteAuthCrosswalk :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth crosswalk record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthCrosswalkObj  AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber        AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject       AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_crosswalk  FOR hat_auth_crosswalk.
  DEFINE BUFFER buf_auth            FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth_crosswalk  EXCLUSIVE-LOCK
         WHERE buf_auth_crosswalk.auth_crosswalk_obj = ipdAuthCrosswalkObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hataw":U,
                              INPUT ipdAuthCrosswalkObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Crosswalk (Obj = ":U + STRING(ipdAuthCrosswalkObj) + ")").
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/


    IF AVAILABLE buf_auth_crosswalk AND NOT ipoErrorObject:CanFind("hataw":U, ipdAuthCrosswalkObj,"":U) THEN
    DO:
      DELETE buf_auth_crosswalk.

      ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_crosswalk AND NOT ipoErrorObject:CanFind("hataw":U,ipdAuthCrosswalkObj,"":U) THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthDetail Procedure
PROCEDURE _deleteAuthDetail :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth detail record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthDetailObj  AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber     AS INTEGER           NO-UNDO.
  DEFINE INPUT        PARAMETER iplRateChange     AS LOGICAL           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject    AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lFailureOccurred   AS LOGICAL     NO-UNDO.

  DEFINE BUFFER buf_auth_detail   FOR hat_auth_detail.
  DEFINE BUFFER los_auth_detail   FOR hat_auth_detail.
  DEFINE BUFFER buf_auth_provider FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_copay    FOR hat_auth_copay.
  DEFINE BUFFER buf_auth_limit    FOR hat_auth_limit.
  DEFINE BUFFER buf_auth          FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth_detail EXCLUSIVE-LOCK
         WHERE buf_auth_detail.auth_detail_obj = ipdAuthDetailObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ASSIGN lFailureOccurred = TRUE.

      ipoErrorObject:addError(INPUT "hatad":U,
                              INPUT ipdAuthDetailObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Detail (Obj = ":U + STRING(ipdAuthDetailObj) + ")").
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_detail AND NOT lFailureOccurred THEN
    DO:
      FIND buf_auth_provider EXCLUSIVE-LOCK
        WHERE buf_auth_provider.auth_provider_obj = buf_auth_detail.auth_provider_obj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138,PROGRESS:445' &ResetIgnoredErrors = FALSE }

      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
      DO:
        {&ResetError}

        ASSIGN lFailureOccurred = TRUE.

        ipoErrorObject:addError(INPUT "hatad":U,
                                INPUT ipdAuthDetailObj,
                                INPUT "":U,
                                INPUT ipiLineNumber,
                                INPUT "MA":U,
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Authorisation Provider (Obj = ":U + STRING(buf_auth_detail.auth_provider_obj) + ")").
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

      IF AVAILABLE buf_auth_provider AND NOT lFailureOccurred THEN
      DO:

        IF NOT buf_auth_provider.los_calculation
        AND NOT CAN-FIND(FIRST los_auth_detail
                         WHERE los_auth_detail.auth_obj          = buf_auth_detail.auth_obj
                           AND los_auth_detail.auth_provider_obj = buf_auth_detail.auth_provider_obj
                           AND los_auth_detail.quantity_los     <> 0
                           AND los_auth_detail.auth_detail_obj  <> buf_auth_detail.auth_detail_obj) THEN
          ASSIGN buf_auth_provider.los_calculation = TRUE.

        FIND CURRENT buf_auth_provider NO-LOCK NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = FALSE }

      END.  /* IF AVAILABLE buf_auth_provider THEN */

      /*
        Remove Co-payment Records
      */
      CopayBlock:
      FOR EACH buf_auth_copay NO-LOCK
         WHERE buf_auth_copay.auth_obj                = buf_auth_detail.auth_obj
           AND buf_auth_copay.owning_entity_mnemonic  = "hatad":U
           AND buf_auth_copay.owning_obj              = ipdAuthDetailObj:

        RUN _deleteAuthCopay ( INPUT buf_auth_copay.auth_copay_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

        ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_copay
                                   THEN ipoErrorObject:CanFind("hatcp":U, buf_auth_copay.auth_copay_obj, "":U)
                                   ELSE lFailureOccurred).

        IF lFailureOccurred THEN
          LEAVE CopayBlock.
      END. /* FOR EACH buf_auth_copay NO-LOCK */

      /*
        Remove Limit Records
      */
      LimitBlock:
      FOR EACH buf_auth_limit NO-LOCK
         WHERE buf_auth_limit.auth_obj                = buf_auth_detail.auth_obj
           AND buf_auth_limit.owning_entity_mnemonic  = "hatad":U
           AND buf_auth_limit.owning_obj              = ipdAuthDetailObj:

        RUN _deleteAuthLimit ( INPUT buf_auth_limit.auth_limit_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

        ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_limit
                                   THEN ipoErrorObject:CanFind("hatal":U, buf_auth_limit.auth_limit_obj, "":U)
                                   ELSE lFailureOccurred).

        IF lFailureOccurred THEN
          LEAVE LimitBlock.
      END. /* FOR EACH buf_auth_limit NO-LOCK */

      /*
        Check if we had any failures
      */
      ASSIGN lFailureOccurred = (IF ipoErrorObject:CanFind("hatad":U, ipdAuthDetailObj, "":U)
                                 THEN
                                   TRUE
                                 ELSE
                                   lFailureOccurred).

      IF NOT lFailureOccurred THEN
      DO:
        /*
          We need to assign rate_change on the history records
          for records that are deleted because of a rate conversion.
          rate_change is not updated on the Record is not updated on
          Assign the Rate Change from the Input parameter to make
          sure we know on the history records which come from
          a Rate Change
        */

        ASSIGN buf_auth_detail.rate_change = iplRateChange.

        DELETE buf_auth_detail.

        ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.

      END. /*IF NOT lFailureOccurred THEN*/
    END. /*IF AVAILABLE buf_auth_detail AND NOT lFailureOccurred THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthDetailHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthDetailHistory Procedure
PROCEDURE _deleteAuthDetailHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth detail history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_detail_history FOR hah_auth_detail_history.

  /*
    Remove Auth Detail History Records
  */
  HistoryBlock:
  FOR EACH hah_auth_detail_history NO-LOCK
     WHERE hah_auth_detail_history.auth_obj = ipdAuthObj:

    FIND FIRST buf_auth_detail_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_detail_history) = ROWID(hah_auth_detail_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Detail History (Obj = ":U  + STRING(hah_auth_detail_history.auth_detail_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_detail_history
    THEN
      DELETE buf_auth_detail_history.
  END. /*FOR EACH hah_auth_detail_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthEpisode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthEpisode Procedure
PROCEDURE _deleteAuthEpisode :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth episode record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthEpisodeObj AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber     AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject    AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_episode FOR hat_auth_episode.
  DEFINE BUFFER buf_auth         FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth_episode EXCLUSIVE-LOCK
         WHERE buf_auth_episode.auth_episode_obj = ipdAuthEpisodeObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatae":U,
                              INPUT ipdAuthEpisodeObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Episode (Obj = ":U + STRING(ipdAuthEpisodeObj) + ")").
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    /*
      MMP-368
      We can link an episode record to multiple auths so to find the first auth linked to the episode
      and check if it is complete or not is not accurate in this scenario.
    */

    IF AVAILABLE buf_auth_episode AND NOT ipoErrorObject:CanFind("hatae":U, ipdAuthEpisodeObj,"":U) THEN
    DO:
      DELETE buf_auth_episode.

      ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_episode AND NOT ipoErrorObject:CanFind("hatae":U,ipdAuthEpisodeObj,"":U) THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthEpisodeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthEpisodeHistory Procedure
PROCEDURE _deleteAuthEpisodeHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth episode history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthEpisodeObj AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber     AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject    AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_episode_history FOR hah_auth_episode_history.

  /*
    Remove Auth Episode History Records
  */
  HistoryBlock:
  FOR EACH hah_auth_episode_history NO-LOCK
     WHERE hah_auth_episode_history.auth_episode_obj = ipdAuthEpisodeObj:

    FIND FIRST buf_auth_episode_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_episode_history) = ROWID(hah_auth_episode_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatae":U,
                              INPUT ipdAuthEpisodeObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Episode History (Obj = ":U  + STRING(hah_auth_episode_history.auth_episode_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_episode_history
    THEN
      DELETE buf_auth_episode_history.

  END. /*FOR EACH hah_auth_episode_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthHistory Procedure
PROCEDURE _deleteAuthHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_history FOR hah_auth_history.

  HistoryBlock:
  FOR EACH hah_auth_history NO-LOCK
     WHERE hah_auth_history.auth_obj = ipdAuthObj:

    FIND FIRST buf_auth_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_history) = ROWID(hah_auth_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation History (Obj = ":U  + STRING(hah_auth_history.auth_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_history
    THEN
      DELETE buf_auth_history.
  END. /*FOR EACH hah_auth_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthLimit Procedure
PROCEDURE _deleteAuthLimit :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth limit record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthLimitObj      AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber        AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject       AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_limit  FOR hat_auth_limit.
  DEFINE BUFFER buf_auth        FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth_limit EXCLUSIVE-LOCK
         WHERE buf_auth_limit.auth_limit_obj = ipdAuthLimitObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatal":U,
                              INPUT ipdAuthLimitObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Limit (Obj = ":U + STRING(ipdAuthLimitObj) + ")").
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/


    IF AVAILABLE buf_auth_limit AND NOT ipoErrorObject:CanFind("hatal":U, ipdAuthLimitObj,"":U) THEN
    DO:
      DELETE buf_auth_limit.

      ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_limit AND NOT ipoErrorObject:CanFind("hatal":U,ipdAuthLimitObj,"":U) THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthLimitHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthLimitHistory Procedure
PROCEDURE _deleteAuthLimitHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth managed care savings history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.


&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_limit_history FOR hah_auth_limit_history.

  /*
    Remove Auth Limit History Records
  */
  HistoryBlock:
  FOR EACH hah_auth_limit_history NO-LOCK
     WHERE hah_auth_limit_history.auth_obj = ipdAuthObj:

    FIND FIRST buf_auth_limit_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_limit_history) = ROWID(hah_auth_limit_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Limit History (Obj = ":U + STRING(hah_auth_limit_history.auth_limit_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_limit_history
    THEN
      DELETE buf_auth_limit_history.
  END. /*FOR EACH hah_auth_limit_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthMCSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthMCSavings Procedure
PROCEDURE _deleteAuthMCSavings :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth savings record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthMCSavingsObj  AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber        AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject       AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_mc_savings FOR hat_auth_mc_savings.
  DEFINE BUFFER buf_auth            FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    {&FindResultRecord}

    FIND FIRST buf_auth_mc_savings EXCLUSIVE-LOCK
         WHERE buf_auth_mc_savings.auth_mc_savings_obj = ipdAuthMCSavingsObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatms":U,
                              INPUT ipdAuthMCSavingsObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Managed Care Savings (Obj = ":U + STRING(ipdAuthMCSavingsObj) + ")").
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/


    IF AVAILABLE buf_auth_mc_savings AND NOT ipoErrorObject:CanFind("hatms":U, ipdAuthMCSavingsObj,"":U) THEN
    DO:
      DELETE buf_auth_mc_savings.

      ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_mc_savings AND NOT ipoErrorObject:CanFind("hatms":U,ipdAuthMCSavingsObj,"":U) THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthMCSavingsHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthMCSavingsHistory Procedure
PROCEDURE _deleteAuthMCSavingsHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth managed care savings history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.


&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_mc_savings_history FOR hah_auth_mc_savings_history.

  /*
    Remove Auth Managed Care Savings History Records
  */
  HistoryBlock:
  FOR EACH hah_auth_mc_savings_history NO-LOCK
     WHERE hah_auth_mc_savings_history.auth_obj = ipdAuthObj:

    FIND FIRST buf_auth_mc_savings_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_mc_savings_history) = ROWID(hah_auth_mc_savings_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Managed Care Savings History (Obj = ":U + STRING(hah_auth_mc_savings_history.auth_mc_savings_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_mc_savings_history
    THEN
      DELETE buf_auth_mc_savings_history.
  END. /*FOR EACH hah_auth_mc_savings_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthProvider Procedure
PROCEDURE _deleteAuthProvider :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth provider record and all dependencies if any
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthProviderObj AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber      AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject     AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lFailureOccurred   AS LOGICAL     NO-UNDO.

  DEFINE BUFFER buf_auth_provider FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_copay    FOR hat_auth_copay.
  DEFINE BUFFER buf_auth_limit    FOR hat_auth_limit.
  DEFINE BUFFER buf_auth          FOR hat_auth.

  DO TRANSACTION ON ERROR UNDO, THROW:

    FIND FIRST buf_auth_provider EXCLUSIVE-LOCK
         WHERE buf_auth_provider.auth_provider_obj = ipdAuthProviderObj
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatap":U,
                              INPUT ipdAuthProviderObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation provider (Obj = ":U + STRING(ipdAuthProviderObj) + ")").
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    /*
      Remove Co-payment Records
    */
    CopayBlock:
    FOR EACH buf_auth_copay NO-LOCK
       WHERE buf_auth_copay.auth_obj                = buf_auth_provider.auth_obj
         AND buf_auth_copay.owning_entity_mnemonic  = "hatap":U
         AND buf_auth_copay.owning_obj              = ipdAuthProviderObj:

      RUN _deleteAuthCopay ( INPUT buf_auth_copay.auth_copay_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

      ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_copay
                                 THEN ipoErrorObject:CanFind("hatcp":U, buf_auth_copay.auth_copay_obj, "":U)
                                 ELSE lFailureOccurred).

      IF lFailureOccurred THEN
        LEAVE CopayBlock.
    END. /* FOR EACH buf_auth_copay NO-LOCK */

    /*
      Remove Limit Records
    */
    LimitBlock:
    FOR EACH buf_auth_limit NO-LOCK
       WHERE buf_auth_limit.auth_obj                = buf_auth_provider.auth_obj
         AND buf_auth_limit.owning_entity_mnemonic  = "hatap":U
         AND buf_auth_limit.owning_obj              = ipdAuthProviderObj:

      RUN _deleteAuthLimit ( INPUT buf_auth_limit.auth_limit_obj, INPUT 0, INPUT-OUTPUT ipoErrorObject).

      ASSIGN lFailureOccurred = (IF AVAILABLE buf_auth_limit
                                 THEN ipoErrorObject:CanFind("hatal":U, buf_auth_limit.auth_limit_obj, "":U)
                                 ELSE lFailureOccurred).

      IF lFailureOccurred THEN
        LEAVE LimitBlock.
    END. /* FOR EACH buf_auth_limit NO-LOCK */

    IF AVAILABLE buf_auth_provider AND NOT ipoErrorObject:CanFind("hatap":U, ipdAuthProviderObj,"":U) AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_provider.

      ASSIGN tt_auth_result.records_removed = tt_auth_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_provider AND NOT ipoErrorObject:CanFind("hatap":U,ipdAuthProviderObj,"":U) THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthProviderHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthProviderHistory Procedure
PROCEDURE _deleteAuthProviderHistory :
/*------------------------------------------------------------------------------
  Purpose   : Remove auth provider history records
  Parameters:
  Notes     : Data Access Only !!!
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj     AS DECIMAL           NO-UNDO.
  DEFINE INPUT        PARAMETER ipiLineNumber  AS INTEGER           NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_provider_history FOR hah_auth_provider_history.

  /*
    Remove Auth Providers History Records
  */
  HistoryBlock:
  FOR EACH hah_auth_provider_history NO-LOCK
     WHERE hah_auth_provider_history.auth_obj = ipdAuthObj:

    FIND FIRST buf_auth_provider_history EXCLUSIVE-LOCK
         WHERE ROWID(buf_auth_provider_history) = ROWID(hah_auth_provider_history)
      NO-ERROR NO-WAIT.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      {&ResetError}

      ipoErrorObject:addError(INPUT "hatau":U,
                              INPUT ipdAuthObj,
                              INPUT "":U,
                              INPUT ipiLineNumber,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Authorisation Providers History (Obj = ":U  + STRING(hah_auth_provider_history.auth_provider_history_obj) + ")").

      LEAVE HistoryBlock.
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN*/

    IF AVAILABLE buf_auth_provider_history
    THEN
      DELETE buf_auth_provider_history.
  END. /*FOR EACH hah_auth_provider_history NO-LOCK*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuth Procedure
PROCEDURE _validateAuth :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

  { ma/app/maauthdatavalauth.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCoding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCoding Procedure
PROCEDURE _validateAuthCoding :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Coding Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

 { ma/app/maauthdatavalcoding.i }


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCopay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCopay Procedure
PROCEDURE _validateAuthCopay :
/*------------------------------------------------------------------------------
  Purpose: Validate Authorisation Copay Buffer
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  { ma/app/maauthdatavalcopay.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthCrosswalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthCrosswalk Procedure
PROCEDURE _validateAuthCrosswalk :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

 { ma/app/maauthdatavalauthcrosswalk.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthDetail Procedure
PROCEDURE _validateAuthDetail :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Detail Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

  { ma/app/maauthdatavaldetail.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthEpisode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthEpisode Procedure
PROCEDURE _validateAuthEpisode :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Episode Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

 { ma/app/maauthdatavalepisode.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthLimit Procedure
PROCEDURE _validateAuthLimit :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Limit Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

 { ma/app/maauthdatavallimit.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthMCSavings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthMCSavings Procedure
PROCEDURE _validateAuthMCSavings :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth MC Savings Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

 { ma/app/maauthdatavalmcsavings.i }



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthProvider Procedure
PROCEDURE _validateAuthProvider :
/*------------------------------------------------------------------------------
  Purpose   : Validate Auth Provider Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

  { ma/app/maauthdatavalprovider.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnUserHasRole) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnUserHasRole Procedure
FUNCTION fnUserHasRole RETURNS LOGICAL
  ( ipcRoleCodeList AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRole     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lHasRole  AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRoleCode AS CHARACTER   NO-UNDO.


  RoleBlk:
  DO iRole = 1 TO NUM-ENTRIES(ipcRoleCodeList):

    ASSIGN cRoleCode = ENTRY(iRole, ipcRoleCodeList)

           lHasRole  = mipEnv:miUser:hasRole(TRIM(cRoleCode)).

    IF lHasRole
    THEN
      LEAVE RoleBlk.

  END. /*DO iRole = 1 TO NUM-ENTRIES(ipcUserRoleCodeList):*/

  RETURN lHasRole.   /* Function return value. */

  { mip/inc/mipcatcherror.i }

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

