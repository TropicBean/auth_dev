&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    $Id: maauthtypeservicestack.p       Exp $

    Purpose: Stack procedure for Auth Type Class

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthtypeds.i}

&IF {&DBDFMA} >= 10195 &THEN
  {ma/inc/maclinicaldocds.i}
&ENDIF

&SCOPED-DEFINE AddAction         Add
&SCOPED-DEFINE AddErrorAction    AddError
&SCOPED-DEFINE UpdateAction      Update
&SCOPED-DEFINE UpdateErrorAction UpdateError
&SCOPED-DEFINE DeleteAction      Delete

DEFINE VARIABLE cInsurerName AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnValidateActiveHealth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnValidateActiveHealth Procedure 
FUNCTION fnValidateActiveHealth RETURNS LOGICAL
  ( INPUT ipcParentCategory           AS CHARACTER, 
    INPUT ipcActivateHealthCategories AS CHARACTER,                                         
    INPUT ipcMnemonic                 AS CHARACTER,
    INPUT ipdObj                      AS DECIMAL  , 
    INPUT ipcField                    AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValidateExclusionValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnValidateExclusionValues Procedure 
FUNCTION fnValidateExclusionValues RETURNS CHARACTER
  ( INPUT iphTableHandle       AS HANDLE,
    INPUT ipdBufferObj         AS DECIMAL,
    INPUT ipcExclFieldsAllowed AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValidateRoles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnValidateRoles Procedure 
FUNCTION fnValidateRoles RETURNS LOGICAL
  ( INPUT  ipcRole         AS CHARACTER,
    INPUT  ipcMnemonic     AS CHARACTER,
    INPUT  ipdObj          AS DECIMAL,
    INPUT  ipcField        AS CHARACTER,
    INPUT  ipiLineNumber   AS INTEGER,
    INPUT  ipcErrorMessage AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValidateUsers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnValidateUsers Procedure 
FUNCTION fnValidateUsers RETURNS LOGICAL
  ( INPUT  ipcUserList     AS CHARACTER,
    INPUT  ipcMnemonic     AS CHARACTER,
    INPUT  ipdObj          AS DECIMAL,
    INPUT  ipcField        AS CHARACTER,
    INPUT  ipiLineNumber   AS INTEGER,
    INPUT  ipcErrorMessage AS CHARACTER)  FORWARD.

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

&IF DEFINED(EXCLUDE-fetchAuthTypeDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthTypeDataset Procedure 
PROCEDURE fetchAuthTypeDataset :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Auth Type Data
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
    Author    : Kati

  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
  DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
  DEFINE INPUT        PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lSuccess                   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER              NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hDatasetHandle             AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hBuffer                    AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthType                  AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthTypeControl           AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthTypeProvider          AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthTypeDetail            AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthTypeError             AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthTypeResult            AS HANDLE               NO-UNDO.
  DEFINE VARIABLE oSearchObject              AS cls.maauthtypesearch NO-UNDO.
  DEFINE VARIABLE cInsurerObj                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cOptionCode                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cEffectiveDate             AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cProviderType              AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cIndicatorType             AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE dInsurerObj                AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE iOptionCode                AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dEffectiveDate             AS DATE                 NO-UNDO.
  
  DEFINE DATA-SOURCE srcAuthType         FOR hac_auth_type.
  DEFINE DATA-SOURCE srcAuthTypeControl  FOR hac_auth_type_control.
  DEFINE DATA-SOURCE srcAuthTypeProvider FOR hac_auth_type_provider.
  DEFINE DATA-SOURCE srcAuthTypeDetail   FOR hac_auth_type_detail.
  
  DEFINE VARIABLE hControlQuery  AS HANDLE NO-UNDO.    
  DEFINE VARIABLE hProviderQuery AS HANDLE NO-UNDO.    
  DEFINE VARIABLE hDetailQuery   AS HANDLE NO-UNDO.  

  CREATE QUERY hProviderQuery.
  CREATE QUERY hControlQuery.
  CREATE QUERY hDetailQuery.

  ASSIGN
    oSearchObject                 = NEW cls.maauthtypesearch(INPUT DATASET-HANDLE iophDatasetHandle)
    hFilterCriteriaTableHandle    = iophFilterCriteriaTableHandle
    hDatasetHandle                = iophDatasetHandle.
  
  ASSIGN                           
    hAuthType                      = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_type":U)
    hAuthTypeControl               = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_type_control":U)
    hAuthTypeProvider              = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_type_provider":U)
    hAuthTypeDetail                = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_type_detail":U)
    hAuthTypeError                 = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_type_error":U)
    hAuthTypeResult                = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_type_result":U)
                                   
    lSuccess                       = hAuthType:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthType:HANDLE)
    lSuccess                       = hAuthTypeControl:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthTypeControl:HANDLE)
    lSuccess                       = hAuthTypeProvider:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthTypeProvider:HANDLE)
    lSuccess                       = hAuthTypeDetail:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthTypeDetail:HANDLE)
    hAuthTypeError:FILL-MODE       = "NO-FILL":U
    hAuthTypeResult:FILL-MODE      = "NO-FILL":U.
  
  ASSIGN
    lSuccess                       = oSearchObject:populateDataset(INPUT iophFilterCriteriaTableHandle)
    iophDatasetHandle              = oSearchObject:DatasetHandle
    iophFilterCriteriaTableHandle  = oSearchObject:CriteriaTableHandle.

  ASSIGN
    lSuccess                      = oSearchObject:getCriteria("InsurerObj":U   ,OUTPUT cInsurerObj)
    lSuccess                      = oSearchObject:getCriteria("OptionCode":U   ,OUTPUT cOptionCode )  
    lSuccess                      = oSearchObject:getCriteria("EffectiveDate":U,OUTPUT cEffectiveDate)  
    lSuccess                      = oSearchObject:getCriteria("ProviderType":U ,OUTPUT cProviderType)  
    lSuccess                      = oSearchObject:getCriteria("IndicatorType":U,OUTPUT cIndicatorType)
    dInsurerObj                   = DECIMAL(cInsurerObj) 
    iOptionCode                   = INTEGER(cOptionCode) 
    dEffectiveDate                = DATE(cEffectiveDate) .  

  /* Provider Delete */
  hProviderQuery:SET-BUFFERS(hAuthTypeProvider).

  hProviderQuery:QUERY-PREPARE('FOR EACH tt_auth_type_provider NO-LOCK').
  
  hProviderQuery:QUERY-OPEN().
  
  hProviderQuery:GET-FIRST(). 
  
  DO WHILE NOT hProviderQuery:QUERY-OFF-END:
    /* Filter records on insurerObj */
    IF dInsurerObj > 0 
    AND hAuthTypeProvider:AVAILABLE 
    THEN
      IF hAuthTypeProvider:BUFFER-FIELD('insurer_obj'):BUFFER-VALUE() <> dInsurerObj 
        AND hAuthTypeProvider:BUFFER-FIELD('insurer_obj'):BUFFER-VALUE() <> 0 THEN
        DO TRANSACTION: 
        hProviderQuery:GET-CURRENT(EXCLUSIVE-LOCK).  
        hAuthTypeProvider:BUFFER-DELETE().  

      END. /* IF hBuffer:BUFFER-FIELD('insurer_obj'):BUFFER-VALUE() <> dInsurerObj THEN */
     
    /* Filter records on cOptionCode */
    IF iOptionCode > 0 
    AND hAuthTypeProvider:AVAILABLE 
    THEN 
        IF  hAuthTypeProvider:BUFFER-FIELD('option_code'):BUFFER-VALUE() <> iOptionCode 
        AND hAuthTypeProvider:BUFFER-FIELD('option_code'):BUFFER-VALUE() <> 0 THEN
      DO TRANSACTION:        
        hProviderQuery:GET-CURRENT(EXCLUSIVE-LOCK). 
        hAuthTypeProvider:BUFFER-DELETE().
         
      END. /* IF hBuffer:BUFFER-FIELD('option_code'):BUFFER-VALUE() <> iOptionCode THEN */

    /* Filter records on dEffectiveDate */
    IF dEffectiveDate <> ? 
    AND hAuthTypeProvider:AVAILABLE 
    THEN 
      IF hAuthTypeProvider:BUFFER-FIELD('effective_date'):BUFFER-VALUE() > dEffectiveDate 
      OR (    hAuthTypeProvider:BUFFER-FIELD('end_date'):BUFFER-VALUE() <> ?
          AND hAuthTypeProvider:BUFFER-FIELD('end_date'):BUFFER-VALUE() < dEffectiveDate )
      THEN DO TRANSACTION:        
        hProviderQuery:GET-CURRENT(EXCLUSIVE-LOCK).  
        hAuthTypeProvider:BUFFER-DELETE().
  
      END. /* IF hBuffer:BUFFER-FIELD('effective_date'):BUFFER-VALUE() < dEffectiveDate */

     /* Filter records on cIndicatorType */
    IF cIndicatorType = "Default":U 
    AND hAuthTypeProvider:AVAILABLE 
    THEN 
      IF hAuthTypeProvider:BUFFER-FIELD('provider_type_indicator'):BUFFER-VALUE() <> "ma_acAuthProviderTypeIndicatorDef":U 
      THEN DO TRANSACTION:        
        hProviderQuery:GET-CURRENT(EXCLUSIVE-LOCK). 
        hAuthTypeProvider:BUFFER-DELETE(). 
        
      END. /* IF hBuffer:BUFFER-FIELD('provider_type_indicator'):BUFFER-VALUE() <> "ma_acAuthProviderTypeIndicatorDef":U THE */

    /* Filter records on cIndicatorType */
    IF cIndicatorType = "Exclusion":U 
    AND hAuthTypeProvider:AVAILABLE 
    THEN 
      IF hAuthTypeProvider:BUFFER-FIELD('provider_type_indicator'):BUFFER-VALUE() <> "ma_acAuthProviderTypeIndicatorExcl":U 
      THEN DO TRANSACTION:        
        hProviderQuery:GET-CURRENT(EXCLUSIVE-LOCK).       
        hAuthTypeProvider:BUFFER-DELETE(). 
        
      END. /* IF hBuffer:BUFFER-FIELD('provider_type_indicator'):BUFFER-VALUE() <> "ma_acAuthProviderTypeIndicatorExcl":U THEN */

    /* Filter records on Provider Type */
  
    IF (cProviderType <> "":U 
    AND cProviderType <> "?"
    AND cProviderType <> ? ) AND hAuthTypeProvider:AVAILABLE 
    THEN 
      IF hAuthTypeProvider:BUFFER-FIELD('provider_type'):BUFFER-VALUE() <> cProviderType 
      THEN DO TRANSACTION:        
        hProviderQuery:GET-CURRENT(EXCLUSIVE-LOCK).
        hAuthTypeProvider:BUFFER-DELETE().   
         
      END. /* IF hBuffer:BUFFER-FIELD('provider_type'):BUFFER-VALUE() <> cProviderType THEN */

    hProviderQuery:GET-NEXT().
  END. /*DO WHILE NOT hProviderQuery:QUERY-OFF-END:*/

  /* Details Delete */

  hDetailQuery:SET-BUFFERS(hAuthTypeDetail).
  
  hDetailQuery:QUERY-PREPARE('FOR EACH tt_auth_type_detail NO-LOCK').
  
  hDetailQuery:QUERY-OPEN().
  
  hDetailQuery:GET-FIRST().
  
  DO WHILE NOT hDetailQuery:QUERY-OFF-END:

    /* Filter records on dEffectiveDate */
    IF dEffectiveDate <> ? 
    AND hAuthTypeDetail:AVAILABLE 
    THEN 
      IF hAuthTypeDetail:BUFFER-FIELD('effective_date'):BUFFER-VALUE() > dEffectiveDate 
      OR (    hAuthTypeDetail:BUFFER-FIELD('end_date'):BUFFER-VALUE() <> ?
          AND hAuthTypeDetail:BUFFER-FIELD('end_date'):BUFFER-VALUE() < dEffectiveDate )
      THEN DO TRANSACTION:        
        hDetailQuery:GET-CURRENT(EXCLUSIVE-LOCK). 
        hAuthTypeDetail:BUFFER-DELETE().  
      END. /* IF hBuffer:BUFFER-FIELD('effective_date'):BUFFER-VALUE() < dEffectiveDate */

    /* Filter records on cIndicatorType */
    IF cIndicatorType = "Default":U 
    AND hAuthTypeDetail:AVAILABLE 
    THEN 
      IF hAuthTypeDetail:BUFFER-FIELD('detail_type_indicator'):BUFFER-VALUE() <> "ma_acAuthDetailTypeIndicatorDef":U 
      THEN DO TRANSACTION:        
        hDetailQuery:GET-CURRENT(EXCLUSIVE-LOCK).
        hAuthTypeDetail:BUFFER-DELETE().    
      END. /* IF hBuffer:BUFFER-FIELD('detail_type_indicator'):BUFFER-VALUE() <> "ma_acAuthDetailTypeIndicatorDef":U THEN */

     /* Filter records on cIndicatorType */
    IF cIndicatorType = "Exclusion":U 
    AND hAuthTypeDetail:AVAILABLE 
    THEN 
      IF hAuthTypeDetail:BUFFER-FIELD('detail_type_indicator'):BUFFER-VALUE() <> "ma_acAuthDetailTypeIndicatorExcl":U 
      THEN DO TRANSACTION:        
        hDetailQuery:GET-CURRENT(EXCLUSIVE-LOCK). 
        hAuthTypeDetail:BUFFER-DELETE().    
        
      END. /* IF hBuffer:BUFFER-FIELD('detail_type_indicator'):BUFFER-VALUE() <> "ma_acAuthDetailTypeIndicatorExcl":U THEN */

    hDetailQuery:GET-NEXT().
  END. /* DO WHILE NOT hDetailQuery:QUERY-OFF-END: */

  /* Control Delete */
  hControlQuery:SET-BUFFERS(hAuthTypeControl).
  
  hControlQuery:QUERY-PREPARE('FOR EACH tt_auth_type_control NO-LOCK').
  
  hControlQuery:QUERY-OPEN().
  
  hControlQuery:GET-FIRST().
  
  DO WHILE NOT hControlQuery:QUERY-OFF-END:
   
    /* Filter records on insurerObj */
    IF dInsurerObj > 0 
    AND hAuthTypeControl:AVAILABLE 
    THEN
      IF hAuthTypeControl:BUFFER-FIELD('insurer_obj'):BUFFER-VALUE() <> dInsurerObj 
        AND hAuthTypeControl:BUFFER-FIELD('insurer_obj'):BUFFER-VALUE() <> 0
      THEN DO TRANSACTION:        
        hControlQuery:GET-CURRENT(EXCLUSIVE-LOCK).
        hAuthTypeControl:BUFFER-DELETE().
      END. /* IF hBuffer:BUFFER-FIELD('insurer_obj'):BUFFER-VALUE() <> dInsurerObj THEN */

    /* Filter records on cOptionCode */
    IF iOptionCode > 0 
    AND hAuthTypeControl:AVAILABLE 
    THEN 
      IF hAuthTypeControl:BUFFER-FIELD('option_code'):BUFFER-VALUE() <> iOptionCode 
        AND hAuthTypeControl:BUFFER-FIELD('option_code'):BUFFER-VALUE() <> 0
      THEN DO TRANSACTION:        
        hControlQuery:GET-CURRENT(EXCLUSIVE-LOCK).
        hAuthTypeControl:BUFFER-DELETE().  

      END. /* IF hBuffer:BUFFER-FIELD('option_code'):BUFFER-VALUE() <> iOptionCode THEN */

    /* Filter records on dEffectiveDate */
    IF dEffectiveDate <> ? 
    AND hAuthTypeControl:AVAILABLE 
    THEN 
      IF hAuthTypeControl:BUFFER-FIELD('effective_date'):BUFFER-VALUE() > dEffectiveDate 
      OR (    hAuthTypeControl:BUFFER-FIELD('end_date'):BUFFER-VALUE() <> ?
          AND hAuthTypeControl:BUFFER-FIELD('end_date'):BUFFER-VALUE() < dEffectiveDate )
      THEN DO TRANSACTION:        
        hControlQuery:GET-CURRENT(EXCLUSIVE-LOCK).     
        hAuthTypeControl:BUFFER-DELETE().

      END. /* IF hBuffer:BUFFER-FIELD('effective_date'):BUFFER-VALUE() < dEffectiveDate */

    /* Filter records on cIndicatorType */
    IF cIndicatorType = "Default":U 
    AND hAuthTypeControl:AVAILABLE 
    THEN 
      IF hAuthTypeControl:BUFFER-FIELD('control_type_indicator'):BUFFER-VALUE() <> "ma_acAuthControlTypeIndicatorDef":U 
      THEN DO TRANSACTION:        
        hControlQuery:GET-CURRENT(EXCLUSIVE-LOCK). 
        hAuthTypeControl:BUFFER-DELETE().

      END. /* IF hBuffer:BUFFER-FIELD('control_type_indicator'):BUFFER-VALUE() <> "ma_acAuthControlTypeIndicatorDef":U THEN */

    /* Filter records on cIndicatorType */
    IF cIndicatorType = "Exclusion":U 
    AND hAuthTypeControl:AVAILABLE 
    THEN 
      IF hAuthTypeControl:BUFFER-FIELD('control_type_indicator'):BUFFER-VALUE() <> "ma_acAuthControlTypeIndicatorExcl":U 
      THEN DO TRANSACTION:        
        hControlQuery:GET-CURRENT(EXCLUSIVE-LOCK).
        hAuthTypeControl:BUFFER-DELETE().  

      END. /* IF hBuffer:BUFFER-FIELD('control_type_indicator'):BUFFER-VALUE() <> "ma_acAuthControlTypeIndicatorExcl":U THEN */

    hControlQuery:GET-NEXT().
  END. /*DO WHILE NOT hControlQuery:QUERY-OFF-END:*/

  {mip/inc/mipcatcherror.i
     &FORMAT  = TRUE
     &FINALLY = "IF VALID-HANDLE(hFilterCriteriaTableHandle)     THEN DELETE OBJECT hFilterCriteriaTableHandle.
                 IF VALID-HANDLE(hDatasetHandle)                 THEN DELETE OBJECT hDatasetHandle.
                 IF VALID-OBJECT(oSearchObject)                  THEN DELETE OBJECT oSearchObject.
                 IF VALID-HANDLE(iophFilterCriteriaTableHandle)  THEN DELETE OBJECT iophFilterCriteriaTableHandle.
                 IF VALID-HANDLE(iophDatasetHandle)              THEN DELETE OBJECT iophDatasetHandle.
                 IF VALID-HANDLE(hControlQuery) THEN
                 DO:
                   IF hControlQuery:IS-OPEN THEN
                   DO:
                     hControlQuery:QUERY-CLOSE().
                     ~{mip/inc/mipmessageerror.i~}
                   END. // IF hControlQuery:IS-OPEN THEN

                   DELETE OBJECT hControlQuery.
                   ~{mip/inc/mipmessageerror.i~}
                 END.  // IF VALID-HANDLE(hControlQuery) THEN
                 IF VALID-HANDLE(hProviderQuery) THEN
                 DO:
                   IF hProviderQuery:IS-OPEN THEN
                   DO:
                     hProviderQuery:QUERY-CLOSE().
                     ~{mip/inc/mipmessageerror.i~}
                   END. // IF hProviderQuery:IS-OPEN THEN

                   DELETE OBJECT hProviderQuery.
                   ~{mip/inc/mipmessageerror.i~}
                 END.  // IF VALID-HANDLE(hProviderQuery) THEN
                 IF VALID-HANDLE(hDetailQuery) THEN
                 DO:
                   IF hDetailQuery:IS-OPEN THEN
                   DO:
                     hDetailQuery:QUERY-CLOSE().
                     ~{mip/inc/mipmessageerror.i~}
                   END. // IF hDetailQuery:IS-OPEN THEN

                   DELETE OBJECT hDetailQuery.
                   ~{mip/inc/mipmessageerror.i~}
                 END.  // IF VALID-HANDLE(hDetailQuery) THEN
                "}
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthType Procedure 
PROCEDURE saveAuthType :
/*------------------------------------------------------------------------------
  Purpose   : Save Auth types from dataset
  Parameters: dsAuthType as defined in maauthtypeds.i
  Notes     : - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
  Author    : Kati
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthType.

&IF {&DBDFMA} >= 10195 &THEN

  RUN _saveAuthType         IN TARGET-PROCEDURE.

  RUN _saveAuthTypeControl  IN TARGET-PROCEDURE. 
  
  RUN _saveAuthTypeProvider IN TARGET-PROCEDURE. 

  RUN _saveAuthTypeDetail IN TARGET-PROCEDURE. 
  
  mipEnv:Health:maClinicalDoc:saveClinicalDocuments(INPUT-OUTPUT DATASET dsClinicalDocs BY-REFERENCE) NO-ERROR.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    FIND FIRST tt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE tt_auth_type_result
    THEN 
      CREATE tt_auth_type_result.

    FOR EACH tt_auth_type_error NO-LOCK:
      ASSIGN 
        tt_auth_type_result.number_of_errors = tt_auth_type_result.number_of_errors + 1.
    END. /* FOR EACH tt_auth_type_error NO-LOCK: */

  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthType Procedure 
PROCEDURE _deleteAuthType :
/*------------------------------------------------------------------------------
  Purpose   : Remove Auth type record
  Parameters: Auth Type obj
  Notes     : - This will run the procedures to remove any
                dependency records as well.
              - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
  Author    : Kati

------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthTypeObj   AS DECIMAL            NO-UNDO.

  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject  NO-UNDO.
  DEFINE VARIABLE lFailureOccurred        AS LOGICAL            NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_type_result   FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error    FOR tt_auth_type_error.
  DEFINE BUFFER buf_auth_type          FOR hac_auth_type.
  DEFINE BUFFER buf_auth_type_control  FOR hac_auth_type_control.
  DEFINE BUFFER buf_clinical_docs      FOR hlm_clinical_docs.
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider.

  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_type_result
    THEN 
      CREATE btt_auth_type_result.

    /* Remove all dependencies to ensure we dont leave any orphan records.*/
    detail-blk:
    FOR EACH buf_auth_type_control NO-LOCK
      WHERE buf_auth_type_control.auth_type_obj = ipdAuthTypeObj:
       
      RUN _deleteAuthTypeControl IN TARGET-PROCEDURE(INPUT buf_auth_type_control.auth_type_control_obj).
      
      ASSIGN lFailureOccurred = IF lFailureOccurred
                                THEN lFailureOccurred
                                ELSE CAN-FIND(FIRST btt_auth_type_error NO-LOCK
                                              WHERE btt_auth_type_error.owning_entity_mnemonic = "hactc":U
                                                AND btt_auth_type_error.owning_obj = buf_auth_type_control.auth_type_control_obj).
    END. /* FOR EACH buf_auth_type_control NO-LOCK */
    
    questionnaire-blk:
    FOR EACH buf_clinical_docs NO-LOCK
      WHERE buf_clinical_docs.owning_entity_mnemonic = "hacat":U
        AND buf_clinical_docs.owning_obj             = ipdAuthTypeObj:
       
      RUN _deleteAuthTypeQuestionnaire IN TARGET-PROCEDURE(INPUT buf_clinical_docs.clinical_docs_obj).
      
      ASSIGN lFailureOccurred = IF lFailureOccurred
                                THEN lFailureOccurred
                                ELSE CAN-FIND(FIRST btt_auth_type_error NO-LOCK
                                              WHERE btt_auth_type_error.owning_entity_mnemonic = "hlmdo":U
                                                AND btt_auth_type_error.owning_obj = buf_clinical_docs.clinical_docs_obj).
    END. /* FOR EACH buf_clinical_docs NO-LOCK */
    
    /* 
       Make sure no dependencies remain 
    */
    IF CAN-FIND(FIRST buf_auth_type_control NO-LOCK
      WHERE buf_auth_type_control.auth_type_obj = ipdAuthTypeObj)
    THEN DO:            
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacat":U, 
                            INPUT ipdAuthTypeObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                            INPUT "Auth Type,Auth Type Detail":U).
    END. /* IF CAN-FIND(FIRST buf_auth_type_control NO-LOCK */
    ELSE IF CAN-FIND(FIRST buf_clinical_docs NO-LOCK
           WHERE buf_clinical_docs.owning_entity_mnemonic = "hacat":U
           AND   buf_clinical_docs.owning_obj             = ipdAuthTypeObj)
    THEN DO:
      ASSIGN lFailureOccurred = TRUE.
       
      oErrorObject:addError(INPUT "hacat":U, 
                            INPUT ipdAuthTypeObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                            INPUT "Auth Type,Questionnaire":U).
    END. /* IF AVAILABLE buf_clinical_docs */
    /* Make sure no Auth Type Provider dependencies exist for the Auth Type */
    ELSE IF CAN-FIND(FIRST buf_auth_type_provider NO-LOCK
           WHERE buf_auth_type_provider.auth_type_obj = ipdAuthTypeObj) 
    THEN DO:            
      ASSIGN lFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacat":U, 
                            INPUT ipdAuthTypeObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                            INPUT "Auth Type,Auth Type Provider":U).
    END. /* IF CAN-FIND(FIRST buf_auth_type_provider NO-LOCK */
    
    IF NOT lFailureOccurred THEN
    DO:
      FIND FIRST buf_auth_type EXCLUSIVE-LOCK
        WHERE buf_auth_type.auth_type_obj = ipdAuthtypeObj
        NO-ERROR NO-WAIT.
     
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
     
      /*Unable to remove - record is already locked*/
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        cls.miperror:resetError().
     
        ASSIGN lFailureOccurred = TRUE.
     
        oErrorObject:addError(INPUT "hacat":U,
                              INPUT ipdAuthTypeObj,
                              INPUT "":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Auth type":U).
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
     
      IF AVAILABLE buf_auth_type AND NOT lFailureOccurred THEN
      DO:
        DELETE buf_auth_type.
     
        ASSIGN btt_auth_type_result.records_removed = btt_auth_type_result.records_removed + 1.
     
      END. /*IF AVAILABLE buf_auth_type AND NOT lFailureOccurred THEN    */
    END. /* IF NOT lFailureOccurred THEN */
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

&ENDIF

  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthTypeControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthTypeControl Procedure 
PROCEDURE _deleteAuthTypeControl :
/*------------------------------------------------------------------------------
  Purpose    : Remove Auth Type Control record     
  Parameters : Auth Type Detail Obj
  Notes      : - This will run the procedures to remove any
                 dependency records as well.
               - Delta 010195
               - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author     : Mandlam      
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthTypeControlObj AS DECIMAL           NO-UNDO.

  DEFINE VARIABLE oErrorObject                 AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred             AS LOGICAL           NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_type_result     FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error      FOR tt_auth_type_error.
  DEFINE BUFFER buf_auth_type_control    FOR hac_auth_type_control.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_type_result 
    THEN
      CREATE btt_auth_type_result.
      
    FIND FIRST buf_auth_type_control EXCLUSIVE-LOCK
      WHERE buf_auth_type_control.auth_type_control_obj = ipdAuthTypeControlObj
      NO-ERROR NO-WAIT.   
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
    /*Unable to remove - record is already locked*/  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().
    
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactc":U, 
                            INPUT ipdAuthTypeControlObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Type Control":U ). 
    END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN */
    
    IF AVAILABLE buf_auth_type_control AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_type_control.
      ASSIGN btt_auth_type_result.records_removed = btt_auth_type_result.records_removed + 1.
    END. /* IF AVAILABLE buf_auth_type_control AND NOT lFailureOccurred THEN */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
 

  { mip/inc/mipcatcherror.i 
    &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
&ENDIF


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthTypeDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthTypeDetail Procedure 
PROCEDURE _deleteAuthTypeDetail :
/*------------------------------------------------------------------------------
  Purpose    : Remove Auth Type Detail record     
  Parameters : Auth Type Detail Obj
  Notes      : - This will run the procedures to remove any 
                 Detail dependency records as well.
               - Delta 010195
               - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author     : Mandlam      
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthTypeDetailObj   AS DECIMAL           NO-UNDO.

  DEFINE VARIABLE oErrorObject                  AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred              AS LOGICAL           NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_type_result     FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error      FOR tt_auth_type_error.
  DEFINE BUFFER buf_auth_type_detail     FOR hac_auth_type_detail.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_type_result 
    THEN
      CREATE btt_auth_type_result.
      
    FIND FIRST buf_auth_type_detail EXCLUSIVE-LOCK
      WHERE buf_auth_type_detail.auth_type_detail_obj = ipdAuthTypeDetailObj
      NO-ERROR NO-WAIT.   
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
    /* Unable to remove - record is already locked */  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().
    
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactd":U, 
                            INPUT ipdAuthTypeDetailObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Type Detail":U). 
    END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN */
    
    IF AVAILABLE buf_auth_type_detail AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_type_detail.
      ASSIGN btt_auth_type_result.records_removed = btt_auth_type_result.records_removed + 1.
    END. /* IF AVAILABLE buf_auth_type_detail AND NOT lFailureOccurred THEN */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
 
  { mip/inc/mipcatcherror.i 
    &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthTypeProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthTypeProvider Procedure 
PROCEDURE _deleteAuthTypeProvider :
/*------------------------------------------------------------------------------
  Purpose    : Remove Auth Type Provider record     
  Parameters : Auth Type Provider Obj
  Notes      : - This will run the procedures to remove any
                 Provider dependency records as well.
               - Delta 010195
               - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author     : Mandlam      
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthTypeProviderObj AS DECIMAL           NO-UNDO.

  DEFINE VARIABLE oErrorObject                  AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred              AS LOGICAL           NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_type_result     FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error      FOR tt_auth_type_error.
  DEFINE BUFFER buf_auth_type_provider   FOR hac_auth_type_provider.
  DEFINE BUFFER buf_auth_type_detail     FOR hac_auth_type_detail.
        
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_type_result 
    THEN
      CREATE btt_auth_type_result.
      
    FIND FIRST buf_auth_type_provider EXCLUSIVE-LOCK
      WHERE buf_auth_type_provider.auth_type_provider_obj = ipdAuthTypeProviderObj
      NO-ERROR NO-WAIT.   
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
    /* Unable to remove - record is already locked */  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
    DO:
      cls.miperror:resetError().
    
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U, 
                            INPUT ipdAuthTypeProviderObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Type Provider":U). 
    END.  /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */
    
    IF AVAILABLE buf_auth_type_provider AND NOT lFailureOccurred THEN
    DO:
      /* 
         Make sure no Auth Type Detail dependencies exist for the Provider
      */
      IF CAN-FIND(FIRST buf_auth_type_detail NO-LOCK
        WHERE buf_auth_type_detail.auth_type_obj          = buf_auth_type_provider.auth_type_obj
          AND buf_auth_type_detail.auth_type_provider_obj = ipdAuthTypeProviderObj) THEN 
      DO:            
        ASSIGN lFailureOccurred = TRUE.

        oErrorObject:addError(INPUT "hactp":U, 
                              INPUT ipdAuthTypeProviderObj,
                              INPUT "":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                              INPUT "Auth Type Provider,Auth Type Detail":U).
      END. /* IF CAN-FIND(FIRST buf_auth_type_detail NO-LOCK */
      ELSE DO:
        DELETE buf_auth_type_provider.
        ASSIGN btt_auth_type_result.records_removed = btt_auth_type_result.records_removed + 1.
      END.  /* ELSE - IF CAN-FIND(FIRST buf_auth_type_detail NO-LOCK */
    END.  /* IF AVAILABLE buf_auth_type_provider AND NOT lFailureOccurred THEN */
  END.  /* DO TRANSACTION ON ERROR UNDO, THROW: */
 
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthTypeQuestionnaire) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthTypeQuestionnaire Procedure 
PROCEDURE _deleteAuthTypeQuestionnaire :
/*------------------------------------------------------------------------------
  Purpose    : Remove Auth Type Questionnaire record     
  Parameters : Clinical Docs Obj
  Notes      : - This will run the procedures to remove any
                 dependency records as well.
               - Delta 010195
               - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author     : Mandlam      
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdClinicalDocsObj   AS DECIMAL           NO-UNDO.

  DEFINE VARIABLE oErrorObject                AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred            AS LOGICAL           NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_type_result    FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error     FOR tt_auth_type_error.
  DEFINE BUFFER buf_clinical_docs       FOR hlm_clinical_docs.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_type_result 
    THEN
      CREATE btt_auth_type_result.
      
    FIND FIRST buf_clinical_docs EXCLUSIVE-LOCK
      WHERE buf_clinical_docs.clinical_docs_obj = ipdClinicalDocsObj
      NO-ERROR NO-WAIT.   
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
    /* Unable to remove - record is already locked */  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().
    
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hlmdo":U, 
                            INPUT ipdClinicalDocsObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Type Questionnaire":U). 
    END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN */
    
    IF AVAILABLE buf_clinical_docs AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_clinical_docs.
      ASSIGN btt_auth_type_result.records_removed = btt_auth_type_result.records_removed + 1.
    END. /* IF AVAILABLE buf_auth_type_control AND NOT lFailureOccurred THEN */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
 

  { mip/inc/mipcatcherror.i 
    &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
&ENDIF


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthType Procedure 
PROCEDURE _saveAuthType :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete Auth type information
  Parameters:
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
  Author    : Kati
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN
  
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE oAuthType           AS cls.mipacronym      NO-UNDO.
                                      
  DEFINE BUFFER buf_auth_type         FOR hac_auth_type.
  DEFINE BUFFER btt_auth_type_control FOR tt_auth_type_control.
  DEFINE BUFFER btt_auth_type_result  FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error   FOR tt_auth_type_error.
  DEFINE BUFFER btt_auth_type         FOR tt_auth_type.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
    ASSIGN oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE btt_auth_type_result
    THEN 
      CREATE btt_auth_type_result.

    RECORD-BLK:
    FOR EACH btt_auth_type EXCLUSIVE-LOCK
      BY btt_auth_type.auth_type_obj DESCENDING:

      ASSIGN
        lFailureOccurred = FALSE
        btt_auth_type_result.records_processed = btt_auth_type_result.records_processed + 1.

      IF btt_auth_type.record_action = "MODIFY":U THEN
      DO:
        /* Auth Type Validations */
        RUN _validateAuthType IN TARGET-PROCEDURE (BUFFER btt_auth_type, INPUT-OUTPUT lFailureOccurred ).

        /*Duplicate check*/
        IF CAN-FIND(FIRST buf_auth_type NO-LOCK
                    WHERE buf_auth_type.auth_type      = btt_auth_type.auth_type
                      AND buf_auth_type.insurer_obj    = btt_auth_type.insurer_obj
                      AND buf_auth_type.auth_type_obj <> btt_auth_type.auth_type_obj)
        THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hacat":U,
                                INPUT btt_auth_type.auth_type_obj,
                                INPUT "":U,
                                INPUT btt_auth_type.line_number,
                                INPUT "MA":U,
                                INPUT 125,  /* &1 already exists with &2 */
                                INPUT "Auth type,":U +
                                      STRING(btt_auth_type.auth_type)).

        END. /*IF CAN-FIND(FIRST buf_auth_type NO-LOCK*/
          
        IF lFailureOccurred
        THEN
          NEXT RECORD-BLK.

        FIND FIRST buf_auth_type EXCLUSIVE-LOCK
          WHERE buf_auth_type.auth_type_obj = btt_auth_type.auth_type_obj
          NO-ERROR NO-WAIT.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}

        /*The auth type record is locked by another user or process*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().

          ASSIGN
            lFailureOccurred = TRUE
            btt_auth_type_result.records_locked = btt_auth_type_result.records_locked + 1.

          oErrorObject:addError(INPUT "hacat":U, 
                                INPUT btt_auth_type.auth_type_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_type.line_number, 
                                INPUT "MA":U, 
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth Type: ":U + STRING(btt_auth_type.auth_type)).

        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
        
        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().

          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_type.

        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
        
        IF AVAILABLE buf_auth_type AND NOT lFailureOccurred THEN
        DO:
          /*An existing record is being updated*/
          IF btt_auth_type.auth_type_obj <= 0
          THEN ASSIGN btt_auth_type_result.records_created  = btt_auth_type_result.records_created  + 1.
          ELSE ASSIGN btt_auth_type_result.records_modified = btt_auth_type_result.records_modified + 1.
        
          BUFFER-COPY btt_auth_type
               EXCEPT btt_auth_type.auth_type_obj
                   TO buf_auth_type.

          /*If we are creating a new record we need to make sure that we run through all the dependants
            and assign the correct _obj*/
          IF btt_auth_type.auth_type_obj < 1 THEN
          DO:
            FOR EACH tt_auth_type_control EXCLUSIVE-LOCK
              WHERE tt_auth_type_control.auth_type_obj = btt_auth_type.auth_type_obj:
              ASSIGN tt_auth_type_control.auth_type_obj = buf_auth_type.auth_type_obj.
            END. /*FOR EACH tt_auth_type_control*/
          END. /*IF btt_auth_type.auth_type_obj < 1 THEN*/

          ASSIGN
            btt_auth_type.auth_type_obj = buf_auth_type.auth_type_obj
            btt_auth_type.record_action = "":U.

          VALIDATE buf_auth_type.

          FIND CURRENT buf_auth_type NO-LOCK.
        END. /*IF AVAILABLE buf_auth_type AND NOT lFailureOccurred THEN*/
      END. /*IF btt_auth_type.record_action = "MODIFY":U THEN*/
          
      IF btt_auth_type.record_action = "DELETE":U THEN
      DO:
        /*This routine will ensure that all dependencies will also be removed*/
        RUN _deleteAuthType IN TARGET-PROCEDURE (INPUT btt_auth_type.auth_type_obj).

        IF NOT CAN-FIND(FIRST btt_auth_type_error NO-LOCK
                        WHERE btt_auth_type_error.owning_entity_mnemonic = "hacat":U
                          AND btt_auth_type_error.owning_obj = btt_auth_type.auth_type_obj)
        THEN DELETE btt_auth_type.
      END. /*END. /*IF btt_auth_type.record_action = "DELETE":U THEN*/*/
    
    END. /*FOR EACH btt_auth_type EXCLUSIVE-LOCK:*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

 { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject.
                IF VALID-OBJECT(oAuthType)     THEN DELETE OBJECT oAuthType.
               " }
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthTypeControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthTypeControl Procedure 
PROCEDURE _saveAuthTypeControl :
/*------------------------------------------------------------------------------
  Purpose   : Create/Update/Delete Auth Type Control Procedure    
  Parameters:  <none>
  Notes     : MIP catch error handling used { mip/inc/mipatcherror.i }
  Author    : Mandlam
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cControlTypeIndicator AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cInsurer              AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cMessage              AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE lFailureOccurred      AS LOGICAL            NO-UNDO INITIAL FALSE.
  
  DEFINE VARIABLE oErrorObject          AS cls.maerrorobject  NO-UNDO.
  
  DEFINE BUFFER buf_auth_type_control  FOR hac_auth_type_control.
  DEFINE BUFFER btt_auth_type_control  FOR tt_auth_type_control.
  DEFINE BUFFER btt_auth_type_result   FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error    FOR tt_auth_type_error.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_type_result 
    THEN
      CREATE btt_auth_type_result.
    
    Record-Blk:
    FOR EACH btt_auth_type_control EXCLUSIVE-LOCK
      BY btt_auth_type_control.auth_type_control_obj DESCENDING:

      /* No use to carry on if there's an error on the auth type header */
      IF oErrorObject:canFind("hacat":U, btt_auth_type_control.auth_type_obj, "":U) THEN
        NEXT Record-Blk.

      ASSIGN
        lFailureOccurred                       = FALSE
        btt_auth_type_result.records_processed = btt_auth_type_result.records_processed + 1.
        
      IF btt_auth_type_control.record_action = "MODIFY":U THEN
      DO:
        /* First check for Duplicate records before validating the Control record further.
           Duplicate check with open btt-record period */
        FIND FIRST buf_auth_type_control NO-LOCK
          WHERE  buf_auth_type_control.auth_type_obj          = btt_auth_type_control.auth_type_obj
            AND  buf_auth_type_control.insurer_obj            = btt_auth_type_control.insurer_obj
            AND  buf_auth_type_control.option_code            = btt_auth_type_control.option_code
            AND  buf_auth_type_control.control_type_indicator = btt_auth_type_control.control_type_indicator
            AND  buf_auth_type_control.effective_date        <= btt_auth_type_control.effective_date
            AND (buf_auth_type_control.end_date               = ?
             OR  buf_auth_type_control.end_date              >= btt_auth_type_control.effective_date)
            AND  buf_auth_type_control.auth_type_control_obj <> btt_auth_type_control.auth_type_control_obj
          NO-ERROR.
  
        /* Duplicate check with closed btt-record period */
        IF NOT AVAILABLE buf_auth_type_control
        THEN 
          FIND FIRST buf_auth_type_control NO-LOCK
           WHERE  buf_auth_type_control.auth_type_obj          = btt_auth_type_control.auth_type_obj
             AND  buf_auth_type_control.insurer_obj            = btt_auth_type_control.insurer_obj
             AND  buf_auth_type_control.option_code            = btt_auth_type_control.option_code
             AND  buf_auth_type_control.control_type_indicator = btt_auth_type_control.control_type_indicator
             AND  buf_auth_type_control.effective_date         > btt_auth_type_control.effective_date
             AND (btt_auth_type_control.end_date               = ?
              OR  buf_auth_type_control.effective_date        <= btt_auth_type_control.end_date)
             AND  buf_auth_type_control.auth_type_control_obj <> btt_auth_type_control.auth_type_control_obj
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }  
  
        IF AVAILABLE buf_auth_type_control THEN
        DO:
          IF buf_auth_type_control.insurer_obj <> 0 THEN 
          DO:
            FIND erm_insurer NO-LOCK
              WHERE erm_insurer.insurer_obj = buf_auth_type_control.insurer_obj
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

            IF AVAILABLE erm_insurer 
            THEN 
              ASSIGN cInsurer = ", Insurer " + erm_insurer.insurer_code.

          END.  /* IF buf_auth_type_control.insurer_obj <> 0 THEN */

          FIND FIRST mic_acronym NO-LOCK
            WHERE mic_acronym.category_key = "ma_acAuthControlTypeIndicator":U
            AND   mic_acronym.acronym_key  = btt_auth_type_control.control_type_indicator
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

          IF AVAILABLE mic_acronym
          THEN
            ASSIGN cControlTypeIndicator = " and Control Type Indicator " + mic_acronym.acronym_label.

          ASSIGN lFailureOccurred = TRUE
  
                 cMessage = "Auth Type Control, Option ":U + STRING(buf_auth_type_control.option_code) + 
                            cInsurer + cControlTypeIndicator + 
                            ". Overlapping period with Effective Date: " + STRING(buf_auth_type_control.effective_date,"9999/99/99":U) 
                 cMessage = cMessage + IF buf_auth_type_control.end_date <> ?
                                       THEN " and End Date: " + STRING(buf_auth_type_control.end_date,"9999/99/99":U)
                                       ELSE "".
          
          oErrorObject:addError(INPUT "hactc":U, 
                                INPUT btt_auth_type_control.auth_type_control_obj, 
                                INPUT "":U, 
                                INPUT "option_code":U,
                                INPUT btt_auth_type_control.line_number, 
                                INPUT "MA":U, 
                                INPUT 125,  /* &1 already exists with &2  */
                                INPUT cMessage).
        END. /* IF AVAILABLE buf_auth_type_control THEN */

        IF lFailureOccurred
        THEN
          NEXT Record-Blk.

        /* Validate Current buffer before we save */
        RUN _validateAuthTypeControl IN TARGET-PROCEDURE(BUFFER btt_auth_type_control, INPUT-OUTPUT lFailureOccurred).


        FIND FIRST buf_auth_type_control EXCLUSIVE-LOCK
          WHERE buf_auth_type_control.auth_type_control_obj = btt_auth_type_control.auth_type_control_obj
          NO-ERROR NO-WAIT.
          
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /* The auth type control is locked by another user or process */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN
            lFailureOccurred = TRUE.
            
            btt_auth_type_result.records_locked = btt_auth_type_result.records_locked + 1.
            
            oErrorObject:addError(INPUT "hactc":U, 
                                  INPUT btt_auth_type_control.auth_type_control_obj, 
                                  INPUT "":U, 
                                  INPUT btt_auth_type_control.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                  INPUT "Auth Type Control :":U + STRING(btt_auth_type_control.auth_type_control_obj)).
        
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */
        
        /* Record not found so we are creating */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN
        DO:
          cls.miperror:resetError().
          
          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_type_control.
          
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN */
      
        IF AVAILABLE buf_auth_type_control AND NOT lFailureOccurred THEN
        DO:
          /* An Existing Record is being updated */
          IF btt_auth_type_control.auth_type_control_obj <= 0 
          THEN ASSIGN btt_auth_type_result.records_created  = btt_auth_type_result.records_created  + 1.
          ELSE ASSIGN btt_auth_type_result.records_modified = btt_auth_type_result.records_modified + 1.

          BUFFER-COPY btt_auth_type_control
               EXCEPT btt_auth_type_control.auth_type_control_obj
                   TO buf_auth_type_control.
                   
          ASSIGN
            btt_auth_type_control.auth_type_control_obj = buf_auth_type_control.auth_type_control_obj
            btt_auth_type_control.record_action        = "":U.
            
          VALIDATE buf_auth_type_control.
          
          FIND CURRENT buf_auth_type_control NO-LOCK.           
        END. /* IF AVAILABLE buf_auth_type_control AND NOT lFailureOccurred THEN */
      END. /* IF btt_auth_type_control.record_action = "MODIFY":U  */
        
      IF btt_auth_type_control.record_action = "DELETE":U THEN
      DO:
        /* This routine will ensure that all dependencies will also be removed */
        RUN _deleteAuthTypeControl IN TARGET-PROCEDURE (INPUT btt_auth_type_control.auth_type_control_obj).
        
        IF NOT CAN-FIND(FIRST btt_auth_type_error NO-LOCK
                        WHERE btt_auth_type_error.owning_entity_mnemonic = "hactc":U
                          AND btt_auth_type_error.owning_obj = btt_auth_type_control.auth_type_control_obj) 
        THEN DELETE btt_auth_type_control.
      END. /* IF btt_auth_type_control.record_action = "DELETE":U THEN */
    END. /* FOR EACH btt_auth_type_control EXCLUSIVE-LOCK */
    
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
  
  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject." }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthTypeDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthTypeDetail Procedure 
PROCEDURE _saveAuthTypeDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cEntity              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOwningInfo          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessage             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lFailureOccurred     AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lOverlap             AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lSucess              AS LOGICAL             NO-UNDO.
  
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  
  DEFINE BUFFER buf_auth_type_detail   FOR hac_auth_type_detail.
  DEFINE BUFFER btt_auth_type_detail   FOR tt_auth_type_detail.
  DEFINE BUFFER btt_auth_type_result   FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error    FOR tt_auth_type_error.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_type_result 
    THEN
      CREATE btt_auth_type_result.

    Record-Blk:
    FOR EACH btt_auth_type_detail EXCLUSIVE-LOCK
      BY btt_auth_type_detail.auth_type_detail_obj DESCENDING:

      /* No use to carry on if there's an error on the auth type header */
      IF oErrorObject:canFind("hactc":U, btt_auth_type_detail.auth_type_obj, "":U) 
      OR oErrorObject:canFind("hactp":U, btt_auth_type_detail.auth_type_provider_obj, "":U) 
      THEN
        NEXT Record-Blk.

      ASSIGN
        lFailureOccurred                       = FALSE
        btt_auth_type_result.records_processed = btt_auth_type_result.records_processed + 1.
        
      IF btt_auth_type_detail.record_action = "MODIFY":U THEN
      DO:
        /* Validate Current buffer before we save */
        RUN _validateAuthTypeDetail IN TARGET-PROCEDURE(BUFFER btt_auth_type_detail, INPUT-OUTPUT lFailureOccurred).

        IF NOT lFailureOccurred THEN
        DO:
          /* Duplicate check with open btt-record period */
          FIND FIRST buf_auth_type_detail NO-LOCK
            WHERE  buf_auth_type_detail.auth_type_obj          = btt_auth_type_detail.auth_type_obj
              AND  buf_auth_type_detail.auth_type_provider_obj = btt_auth_type_detail.auth_type_provider_obj
              AND  buf_auth_type_detail.pr_type                = btt_auth_type_detail.pr_type
              AND  buf_auth_type_detail.owning_entity_mnemonic = btt_auth_type_detail.owning_entity_mnemonic
              AND (buf_auth_type_detail.owning_obj             = btt_auth_type_detail.owning_obj
              AND  buf_auth_type_detail.owning_key             = btt_auth_type_detail.owning_key)   
              AND  buf_auth_type_detail.effective_date        <= btt_auth_type_detail.effective_date
              AND (buf_auth_type_detail.end_date               = ?
               OR  buf_auth_type_detail.end_date              >= btt_auth_type_detail.effective_date)
              AND  buf_auth_type_detail.auth_type_detail_obj <> btt_auth_type_detail.auth_type_detail_obj
            NO-ERROR.
          
          /* Duplicate check with closed btt-record period */
          IF NOT AVAILABLE buf_auth_type_detail 
          THEN
            FIND FIRST buf_auth_type_detail NO-LOCK
              WHERE  buf_auth_type_detail.auth_type_obj          = btt_auth_type_detail.auth_type_obj
                AND  buf_auth_type_detail.auth_type_provider_obj = btt_auth_type_detail.auth_type_provider_obj
                AND  buf_auth_type_detail.auth_type_detail_obj  <> btt_auth_type_detail.auth_type_detail_obj
                AND  buf_auth_type_detail.pr_type                = btt_auth_type_detail.pr_type
                AND  buf_auth_type_detail.owning_entity_mnemonic = btt_auth_type_detail.owning_entity_mnemonic
                AND (buf_auth_type_detail.owning_obj             = btt_auth_type_detail.owning_obj
                AND  buf_auth_type_detail.owning_key             = btt_auth_type_detail.owning_key)   
                AND  buf_auth_type_detail.effective_date         > btt_auth_type_detail.effective_date
                AND (btt_auth_type_detail.end_date               = ?
                 OR  buf_auth_type_detail.effective_date        <= btt_auth_type_detail.end_date)
            NO-ERROR.
            
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
          
          IF AVAILABLE buf_auth_type_detail THEN 
          DO:
            ASSIGN lFailureOccurred = TRUE.
  
            CASE btt_auth_type_detail.owning_entity_mnemonic:
              WHEN "hlmnl":U                   THEN ASSIGN cEntity = "Nappi".
              WHEN "htmtl":U OR WHEN "htmtt":U THEN ASSIGN cEntity = "Tariff".  
              WHEN "hlmcr":U                   THEN ASSIGN cEntity = "Basket".
              OTHERWISE                             ASSIGN cEntity = "Entity".
            END CASE.
 
            ASSIGN cMessage = "Auth Type Detail,":U + cEntity + " ":U + buf_auth_type_detail.owning_alt_value + 
                              ". Overlapping period with Effective Date: " + STRING(buf_auth_type_detail.effective_date,"9999/99/99":U) 
                   cMessage = cMessage + IF buf_auth_type_detail.end_date <> ?
                                         THEN " and End Date: " + STRING(buf_auth_type_detail.end_date,"9999/99/99":U)
                                         ELSE "".
            
            oErrorObject:addError(INPUT "hactd":U, 
                                  INPUT btt_auth_type_detail.auth_type_detail_obj, 
                                  INPUT "":U, 
                                  INPUT "owning_entity_mnemonic":U,
                                  INPUT btt_auth_type_detail.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 125,  /* &1 already exists with &2 */
                                  INPUT cMessage).
          END. /* IF AVAILABLE buf_auth_type_detail THEN */
        END.  // IF NOT lFailureOccurred THEN

        IF lFailureOccurred
        THEN
          NEXT Record-Blk.
          
        FIND FIRST buf_auth_type_detail EXCLUSIVE-LOCK
          WHERE buf_auth_type_detail.auth_type_detail_obj = btt_auth_type_detail.auth_type_detail_obj
          NO-ERROR NO-WAIT.
          
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /* The auth type detail is locked by another user or process */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN
            lFailureOccurred                    = TRUE
            btt_auth_type_result.records_locked = btt_auth_type_result.records_locked + 1.
            
            oErrorObject:addError(INPUT "hactd":U,
                                  INPUT btt_auth_type_detail.auth_type_detail_obj, 
                                  INPUT "":U, 
                                  INPUT btt_auth_type_detail.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 200, /* Record "&1" is locked and cannot be read for updating, please try again. */
                                  INPUT "Auth type detail: ":U + STRING(btt_auth_type_detail.auth_type_detail_obj)).
        
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */
        
        /* Record not found so we are creating */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN
        DO:
          cls.miperror:resetError().

          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_type_detail.
          
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN */
      
        IF AVAILABLE buf_auth_type_detail AND NOT lFailureOccurred THEN
        DO:
          /* An Existing Record is being updated */
          IF btt_auth_type_detail.auth_type_detail_obj <= 0 
          THEN ASSIGN btt_auth_type_result.records_created  = btt_auth_type_result.records_created  + 1.
          ELSE ASSIGN btt_auth_type_result.records_modified = btt_auth_type_result.records_modified + 1.
          
          BUFFER-COPY btt_auth_type_detail
               EXCEPT btt_auth_type_detail.auth_type_detail_obj
                   TO buf_auth_type_detail.
                   
          ASSIGN
            btt_auth_type_detail.auth_type_detail_obj = buf_auth_type_detail.auth_type_detail_obj
            btt_auth_type_detail.record_action        = "":U.
            
          VALIDATE buf_auth_type_detail.
          
          FIND CURRENT buf_auth_type_detail NO-LOCK.
          
        END. /* IF AVAILABLE buf_auth_type_detail AND NOT lFailureOccurred THEN */
      END. /* IF btt_auth_type_detail.record_action = "MODIFY":U  */
        
      IF btt_auth_type_detail.record_action = "DELETE":U THEN
      DO:
        /* This routine will ensure that all dependencies will also be removed */
        RUN _deleteAuthTypeDetail IN TARGET-PROCEDURE (INPUT btt_auth_type_detail.auth_type_detail_obj).
          
        IF NOT CAN-FIND(FIRST btt_auth_type_error NO-LOCK
                          WHERE btt_auth_type_error.owning_entity_mnemonic = "hactd":U
                            AND btt_auth_type_error.owning_obj             = btt_auth_type_detail.auth_type_detail_obj) 
        THEN DELETE btt_auth_type_detail.
      END. /* IF btt_auth_type_detail.record_action = "DELETE":U THEN */
    END. /* FOR EACH btt_auth_type_detail NO-LOCK */
    
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
  
  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject." }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthTypeProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthTypeProvider Procedure 
PROCEDURE _saveAuthTypeProvider :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSucess              AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred     AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider.
  DEFINE BUFFER btt_auth_type_provider FOR tt_auth_type_provider.
  DEFINE BUFFER btt_auth_type_result   FOR tt_auth_type_result.
  DEFINE BUFFER btt_auth_type_error    FOR tt_auth_type_error.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    FIND FIRST btt_auth_type_result EXCLUSIVE-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_type_result 
    THEN
      CREATE btt_auth_type_result.
    
    Record-Blk:
    FOR EACH btt_auth_type_provider EXCLUSIVE-LOCK
      BY btt_auth_type_provider.auth_type_provider_obj DESCENDING:
      
      /* No use to carry on if there's an error on the auth type header */
      IF oErrorObject:canFind("hacat":U, btt_auth_type_provider.auth_type_obj, "":U) 
      THEN
        NEXT Record-Blk.
        
      ASSIGN
        lFailureOccurred                       = FALSE
        btt_auth_type_result.records_processed = btt_auth_type_result.records_processed + 1.
        
      IF btt_auth_type_provider.record_action = "MODIFY":U THEN
      DO:
        /* Validate Current buffer before we save */
        RUN _validateAuthTypeProvider IN TARGET-PROCEDURE(BUFFER btt_auth_type_provider, INPUT-OUTPUT lFailureOccurred).

        IF lFailureOccurred
        THEN
          NEXT Record-Blk.
          
        FIND FIRST buf_auth_type_provider EXCLUSIVE-LOCK
          WHERE buf_auth_type_provider.auth_type_provider_obj = btt_auth_type_provider.auth_type_provider_obj
          NO-ERROR NO-WAIT.
          
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /* The auth type provider is locked by another user or process */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN
            lFailureOccurred                    = TRUE
            btt_auth_type_result.records_locked = btt_auth_type_result.records_locked + 1.
            
            oErrorObject:addError(INPUT "hactp":U,
                                  INPUT btt_auth_type_provider.auth_type_provider_obj, 
                                  INPUT "":U, 
                                  INPUT btt_auth_type_provider.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 200, /* Record "&1" is locked and cannot be read for updating, please try again. */
                                  INPUT "Auth type provider: ":U + STRING(btt_auth_type_provider.auth_type_provider_obj)).
        
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */
        
        /* Record not found so we are creating */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN
        DO:
          cls.miperror:resetError().
          
          /* Duplicate check */
          IF CAN-FIND(FIRST buf_auth_type_provider NO-LOCK
            WHERE  buf_auth_type_provider.auth_type_obj           = btt_auth_type_provider.auth_type_obj
              AND  buf_auth_type_provider.insurer_obj             = btt_auth_type_provider.insurer_obj
              AND  buf_auth_type_provider.option_code             = btt_auth_type_provider.option_code
              AND  buf_auth_type_provider.provider_type           = btt_auth_type_provider.provider_type
              AND  buf_auth_type_provider.provider_type_indicator = btt_auth_type_provider.provider_type_indicator
              AND  buf_auth_type_provider.pr_type_list            = btt_auth_type_provider.pr_type_list
              AND  buf_auth_type_provider.effective_date         <= btt_auth_type_provider.effective_date
              AND (buf_auth_type_provider.end_date                = ?
               OR  buf_auth_type_provider.end_date               >= btt_auth_type_provider.effective_date)
              AND  buf_auth_type_provider.auth_type_provider_obj <> btt_auth_type_provider.auth_type_provider_obj) 
          OR CAN-FIND(FIRST buf_auth_type_provider NO-LOCK
            WHERE  buf_auth_type_provider.auth_type_obj           = btt_auth_type_provider.auth_type_obj
              AND  buf_auth_type_provider.insurer_obj             = btt_auth_type_provider.insurer_obj
              AND  buf_auth_type_provider.option_code             = btt_auth_type_provider.option_code
              AND  buf_auth_type_provider.provider_type           = btt_auth_type_provider.provider_type
              AND  buf_auth_type_provider.provider_type_indicator = btt_auth_type_provider.provider_type_indicator
              AND  buf_auth_type_provider.pr_type_list            = btt_auth_type_provider.pr_type_list
              AND  buf_auth_type_provider.effective_date          > btt_auth_type_provider.effective_date
              AND (btt_auth_type_provider.end_date                = ?
               OR  buf_auth_type_provider.effective_date         <= btt_auth_type_provider.end_date)
              AND  buf_auth_type_provider.auth_type_provider_obj <> btt_auth_type_provider.auth_type_provider_obj) THEN 
          DO:
            ASSIGN lFailureOccurred = TRUE.
            
            oErrorObject:addError(INPUT "hactp":U, 
                                    INPUT btt_auth_type_provider.auth_type_provider_obj, 
                                    INPUT "":U, 
                                    INPUT "option_code":U,
                                    INPUT btt_auth_type_provider.line_number, 
                                    INPUT "MA":U, 
                                    INPUT 125,  // &1 already exists with &2
                                    INPUT "Auth type provider,":U + STRING(btt_auth_type_provider.auth_type_obj)               + "-":U 
                                                                  + STRING(btt_auth_type_provider.option_code)                 + "-":U 
                                                                  + STRING(btt_auth_type_provider.provider_type)               + "-":U 
                                                                  + STRING(btt_auth_type_provider.provider_type_indicator)     + "-":U
                                                                  + btt_auth_type_provider.pr_type_list                        + "-":U
                                                                  + STRING(btt_auth_type_provider.effective_date,"9999/99/99") + "-":U
                                                                  + STRING(btt_auth_type_provider.end_date,"9999/99/99")).
          END. /* IF CAN-FIND(FIRST buf_auth_type_provider NO-LOCK */
          
          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_type_provider.
          
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN */
      
        IF AVAILABLE buf_auth_type_provider AND NOT lFailureOccurred THEN
        DO:
          /* An Existing Record is being updated */
          IF btt_auth_type_provider.auth_type_provider_obj <= 0 
          THEN ASSIGN btt_auth_type_result.records_created  = btt_auth_type_result.records_created  + 1.
          ELSE ASSIGN btt_auth_type_result.records_modified = btt_auth_type_result.records_modified + 1.
          
          BUFFER-COPY btt_auth_type_provider
               EXCEPT btt_auth_type_provider.auth_type_provider_obj
                   TO buf_auth_type_provider.
                   
          ASSIGN
            btt_auth_type_provider.auth_type_provider_obj = buf_auth_type_provider.auth_type_provider_obj
            btt_auth_type_provider.record_action        = "":U.
            
          VALIDATE buf_auth_type_provider.
          
          FIND CURRENT buf_auth_type_provider NO-LOCK.
          
        END. /* IF AVAILABLE buf_auth_type_provider AND NOT lFailureOccurred THEN */
      END. /* IF btt_auth_type_provider.record_action = "MODIFY":U  */
        
      IF btt_auth_type_provider.record_action = "DELETE":U THEN
      DO:
        /* This routine will ensure that all dependencies will also be removed */
        RUN _deleteAuthTypeProvider IN TARGET-PROCEDURE (INPUT btt_auth_type_provider.auth_type_provider_obj).
        
        IF NOT CAN-FIND(FIRST btt_auth_type_error NO-LOCK
                        WHERE btt_auth_type_error.owning_entity_mnemonic = "hactp":U
                          AND btt_auth_type_error.owning_obj             = btt_auth_type_provider.auth_type_provider_obj) 
        THEN DELETE btt_auth_type_provider.
      END. /* IF btt_auth_type_provider.record_action = "DELETE":U THEN */
    END. /* FOR EACH btt_auth_type_provider NO-LOCK */
    
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
  
  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject." }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthType Procedure 
PROCEDURE _validateAuthType :
/*------------------------------------------------------------------------------
  Purpose    : Auth Type Validation     
  Parameters : Current Buffer to be validated
  Notes      : 
  Author     : Mandlam     
------------------------------------------------------------------------------*/

{ ma/app/maauthtypeservalauthtype.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthTypeControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthTypeControl Procedure 
PROCEDURE _validateAuthTypeControl :
/*----------------------------------------------------------------------------
  Purpose    : Auth Type Control validation    
  Parameters : Current Buffer to be validated
  Notes      :
  ----------------------------------------------------------------------------*/
  
  { ma/app/maauthtypeservalautpcontrol.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthTypeDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthTypeDetail Procedure 
PROCEDURE _validateAuthTypeDetail :
/*----------------------------------------------------------------------------
  Purpose    : Auth Type Detail validation    
  Parameters : Current Buffer to be validated
  Notes      :
  ----------------------------------------------------------------------------*/
  
  { ma/app/maauthtypeservalauthdetail.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthTypeProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthTypeProvider Procedure 
PROCEDURE _validateAuthTypeProvider :
/*------------------------------------------------------------------------------
  Purpose    : Auth Type Provider validation    
  Parameters : Current Buffer to be validated
  Notes      :
  Author     :    
------------------------------------------------------------------------------*/
  
  { ma/app/maauthtypeservalprovider.i }      
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateProviderAuthorisedService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateProviderAuthorisedService Procedure 
PROCEDURE _validateProviderAuthorisedService :
/*------------------------------------------------------------------------------
  Purpose    : Auth Type Provider claim codes and claim types validation    
  Parameters : Current Buffer to be validated
  Notes      :
  Author     :      
------------------------------------------------------------------------------*/
  DEFINE              PARAMETER BUFFER btt_auth_type_provider FOR tt_auth_type_provider.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE iCnt                 AS INTEGER             NO-UNDO.
  DEFINE VARIABLE cEntry               AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL             NO-UNDO.
  
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider.
  DEFINE BUFFER ctt_auth_type_provider FOR tt_auth_type_provider.

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
                                                                 
  /*
    If an auth type provider is not an authorised service, 
    save a default auth type provider.
  */
  IF btt_auth_type_provider.authorised_service = NO
  THEN DO: 
    /* 
      Setting the default values for a default auth type provider.
    */
    ASSIGN 
      btt_auth_type_provider.main_provider                = FALSE
      btt_auth_type_provider.claim_codes_provider         = "":U
      btt_auth_type_provider.claim_types_provider         = "":U
      btt_auth_type_provider.valid_claim_codes_detail     = "":U
      btt_auth_type_provider.valid_claim_types_detail     = "":U
      btt_auth_type_provider.amount_auth                  = 0
      btt_auth_type_provider.quantity_auth                = 0
      btt_auth_type_provider.header_values_allowed        = "":U
      btt_auth_type_provider.header_values_unlimited      = FALSE
      btt_auth_type_provider.claim_codes_disallow         = "":U
      btt_auth_type_provider.claim_types_disallow         = "":U
      btt_auth_type_provider.authorise_detail_lines       = "":U
      btt_auth_type_provider.default_auth_status          = "":U
      btt_auth_type_provider.default_auth_status_note     = "":U
      btt_auth_type_provider.default_auth_status_upd_role = "":U
      btt_auth_type_provider.default_auth_status_upd_user = "":U
      btt_auth_type_provider.default_claim_code_detail    = 0
      btt_auth_type_provider.default_claim_type_detail    = "":U
      .
    
    VALIDATE btt_auth_type_provider.   
    
    /* 
      Raising a warning. 
    */
    oErrorObject:addError(INPUT "hactp":U,
                          INPUT btt_auth_type_provider.auth_type_provider_obj,
                          INPUT "":U,
                          INPUT "authorised_services":U,
                          INPUT btt_auth_type_provider.line_number,
                          INPUT "Default values applied to auth type provider, because it is not an authorised service. ":U,
                          INPUT "WAR").
  
  END. /* btt_auth_type_provider.authorised_service = NO */
  ELSE
  DO:
    /* If authorised_service = true and auth_type_indicator is not an exclusion, 
       and it's the first record, then default main_provider to true. */
    IF NOT CAN-FIND(FIRST ctt_auth_type_provider NO-LOCK
                    WHERE ctt_auth_type_provider.auth_type_obj            = btt_auth_type_provider.auth_type_obj 
                      AND ctt_auth_type_provider.main_provider            = TRUE
                      AND ctt_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorDef":U
                      AND ctt_auth_type_provider.auth_type_provider_obj  <> btt_auth_type_provider.auth_type_provider_obj) AND 
       NOT CAN-FIND(FIRST buf_auth_type_provider NO-LOCK
                    WHERE buf_auth_type_provider.auth_type_obj            = btt_auth_type_provider.auth_type_obj 
                      AND buf_auth_type_provider.main_provider            = TRUE 
                      AND buf_auth_type_provider.provider_type_indicator  = "ma_acAuthProviderTypeIndicatorDef":U
                      AND buf_auth_type_provider.auth_type_provider_obj  <> btt_auth_type_provider.auth_type_provider_obj) AND 
      btt_auth_type_provider.line_number <= 0 THEN 
    DO:
      ASSIGN btt_auth_type_provider.main_provider = TRUE.

      VALIDATE btt_auth_type_provider.
    END. /*IF NOT CAN-FIND(FIRST ctt_auth_type_provider NO-LOCK*/
  END. /*DO:*/   

  { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."} 

&ENDIF      
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateProviderClaimCodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateProviderClaimCodes Procedure 
PROCEDURE _validateProviderClaimCodes :
/*------------------------------------------------------------------------------
  Purpose    : Auth Type Provider claim codes and claim types validation    
  Parameters : Current Buffer to be validated
  Notes      :
  Author     :      
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth_type_provider   FOR tt_auth_type_provider.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL    NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE iClaimCode           AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iCnt                 AS INTEGER             NO-UNDO.
  DEFINE VARIABLE cEntry               AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE cErrorMessage        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL             NO-UNDO. 
 
  DEFINE BUFFER buf_auth_type          FOR hac_auth_type.
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider.
  
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE  tt_auth_type_error:HANDLE).
  
  /* Validate claim_codes_provider */
  IF btt_auth_type_provider.claim_codes_provider <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.claim_codes_provider):

    ASSIGN iClaimCode = INTEGER(ENTRY(iCnt,btt_auth_type_provider.claim_codes_provider)) NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:76':U &ResetIgnoredErrors = FALSE}
        
    /* Invalid character in numeric input */
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
    DO:
      cls.miperror:resetError().

      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "claim_codes_provider":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Claim Code: ":U + STRING(ENTRY(icnt,btt_auth_type_provider.claim_codes_provider))).

    END.  // IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
    ELSE DO:
      mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_provider.option_code,
                                                         INPUT  iClaimCode,
                                                         INPUT  btt_auth_type_provider.effective_date,
                                                         OUTPUT lSuccess,
                                                         OUTPUT cErrorMessage).

      IF NOT lSuccess THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_provider_obj,
                              INPUT "":U,
                              INPUT "claim_codes_provider":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Claim Code: ":U + STRING(ENTRY(iCnt,btt_auth_type_provider.claim_codes_provider))).
      END. /* IF NOT lSuccess */
    END.  // ELSE - IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
  END. /* IF btt_auth_type_provider.claim_codes_provider <> "":U THEN */
  
  /* Validate claim_types_provider. */
  IF btt_auth_type_provider.claim_types_provider <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.claim_types_provider):
  
    mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  ENTRY(iCnt,btt_auth_type_provider.claim_types_provider),
                                                       OUTPUT lSuccess,
                                                       OUTPUT cErrorMessage).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "claim_types_provider":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Claim Type: ":U + ENTRY(iCnt,btt_auth_type_provider.claim_types_provider)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_provider.claim_types_provider <> "":U THEN */

  /* Validate default claim code detail */
  IF btt_auth_type_provider.default_claim_code_detail <> 0 THEN
  DO:
    mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_provider.option_code,
                                                       INPUT  btt_auth_type_provider.default_claim_code_detail,
                                                       INPUT  btt_auth_type_provider.effective_date,
                                                       OUTPUT lSuccess,
                                                       OUTPUT cErrorMessage).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "default_claim_code_detail":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Default Claim Code Detail: ":U + STRING(btt_auth_type_provider.default_claim_code_detail)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_provider.default_claim_code_detail <> 0 THEN */

  /* Validate default claim type detail */
  IF btt_auth_type_provider.default_claim_type_detail <> "":U THEN
  DO:
    mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  btt_auth_type_provider.default_claim_type_detail,
                                                       OUTPUT lSuccess,
                                                       OUTPUT cErrorMessage).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "default_claim_type_detail":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Default Claim Type Detail: ":U + btt_auth_type_provider.default_claim_type_detail).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_provider.default_claim_type_detail <> 0 THEN */
  
  /* Validate valid_claim_codes_detail */
  IF btt_auth_type_provider.valid_claim_codes_detail <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.valid_claim_codes_detail):
    ASSIGN iClaimCode = INTEGER(ENTRY(iCnt,btt_auth_type_provider.valid_claim_codes_detail)) NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:76':U &ResetIgnoredErrors = FALSE}
        
    /* Invalid character in numeric input */
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
    DO:
      cls.miperror:resetError().

      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "valid_claim_codes_detail":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Valid Claim Code Detail: ":U + STRING(ENTRY(icnt,btt_auth_type_provider.valid_claim_codes_detail))).

    END.  // IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
    ELSE DO:
      mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_provider.option_code,
                                                         INPUT  iClaimCode,
                                                         INPUT  btt_auth_type_provider.effective_date,
                                                         OUTPUT lSuccess,
                                                         OUTPUT cErrorMessage).
      IF NOT lSuccess THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
        
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_provider_obj,
                              INPUT "":U,
                              INPUT "valid_claim_codes_detail":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Valid Claim Code Detail: ":U + ENTRY(iCnt,btt_auth_type_provider.valid_claim_codes_detail)).
      END. /* IF NOT lSuccess */
    END.  // ELSE - IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
  END. /* IF btt_auth_type_provider.valid_claim_codes_detail <> "":U THEN */
  
  /* Validate valid_claim_types_detail */  
  IF btt_auth_type_provider.valid_claim_types_detail <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.valid_claim_types_detail):

    mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  ENTRY(icnt,btt_auth_type_provider.valid_claim_types_detail),
                                                       OUTPUT lSuccess,
                                                       OUTPUT cErrorMessage).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "valid_claim_types_detail":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Valid Claim Type Detail: ":U + ENTRY(icnt,btt_auth_type_provider.valid_claim_types_detail)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_provider.valid_claim_types_detail <> "":U THEN */

  /* Validate claim_codes_disallow */  
  IF btt_auth_type_provider.claim_codes_disallow <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.claim_codes_disallow):

    ASSIGN iClaimCode = INTEGER(ENTRY(icnt,btt_auth_type_provider.claim_codes_disallow)) NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:76':U &ResetIgnoredErrors = FALSE}
        
    /* Invalid character in numeric input */
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
    DO:
      cls.miperror:resetError().

      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "claim_codes_disallow":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Claim Code Disallow: ":U + STRING(ENTRY(icnt,btt_auth_type_provider.claim_codes_disallow))).

    END.  // IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
    ELSE DO:
      mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_provider.option_code,
                                                         INPUT  iClaimCode,
                                                         INPUT  btt_auth_type_provider.effective_date,
                                                         OUTPUT lSuccess,
                                                         OUTPUT cErrorMessage).
  
      IF NOT lSuccess THEN 
      DO:
        ASSIGN oplFailureOccurred = TRUE.
        
        oErrorObject:addError(INPUT "hactp":U,
                              INPUT btt_auth_type_provider.auth_type_provider_obj,
                              INPUT "":U,
                              INPUT "claim_codes_disallow":U,
                              INPUT btt_auth_type_provider.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Claim Code Disallow: ":U + STRING(iClaimCode)).
      END. /* IF NOT lSuccess */
    END.  // ELSE - IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U THEN
  END. /* IF btt_auth_type_provider.claim_codes_disallow <> "":U THEN */
  
  /* Validate claim_types_disallow */  
  IF btt_auth_type_provider.claim_types_disallow <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_provider.claim_types_disallow):
  
    mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  ENTRY(icnt,btt_auth_type_provider.claim_types_disallow),
                                                       OUTPUT lSuccess,
                                                       OUTPUT cErrorMessage).
    IF NOT lSuccess THEN 
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hactp":U,
                            INPUT btt_auth_type_provider.auth_type_provider_obj,
                            INPUT "":U,
                            INPUT "claim_types_disallow":U,
                            INPUT btt_auth_type_provider.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Claim Type Disallow: ":U + ENTRY(icnt,btt_auth_type_provider.claim_types_disallow)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_provider.claim_types_disallow <> "":U THEN */
       
  { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF      
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnValidateActiveHealth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnValidateActiveHealth Procedure 
FUNCTION fnValidateActiveHealth RETURNS LOGICAL
  ( INPUT ipcParentCategory           AS CHARACTER, 
    INPUT ipcActivateHealthCategories AS CHARACTER,                                         
    INPUT ipcMnemonic                 AS CHARACTER,
    INPUT ipdObj                      AS DECIMAL  , 
    INPUT ipcField                    AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose: to validate the comma delimited string if it a valide list  
    Notes:  
   INPUT ipcParentCategory  /*Indicate if the Health functionality must 
                                                    be activated for the authorisation type 
                                                    and allows the user to specify which 
                                                    Health Categories are required. 
                                                    If more than one category is specified,
                                                    the list will be comma delimited */
   INPUT ipcActivateHealthCategories parent category  
   INPUT ipcMnemonic 
   INPUT ipdObj                      
   INPUT ipcField
   
   RETURN VALUE:  Return a FALSE if the input is valid.
                  Returning a TRUE means there is a problem/error.
   
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess               AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lInvalideActiveHealth  AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE iNumberOfEntries       AS INTEGER           NO-UNDO.
  DEFINE VARIABLE cCategory              AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE iEntry                 AS INTEGER           NO-UNDO.
  DEFINE VARIABLE oCategoryHelper        AS cls.mipcategory   NO-UNDO.
  DEFINE VARIABLE oErrorObject           AS cls.maerrorobject NO-UNDO.
  
  ASSIGN oErrorObject          = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE)
         lInvalideActiveHealth = FALSE.

  IF ipcActivateHealthCategories = "":U  // Using the RULE default
  OR ipcActivateHealthCategories = "[NONE]":U 
  OR ipcActivateHealthCategories = "[ALL]":U
  THEN
    RETURN FALSE.

  ASSIGN 
    oCategoryHelper = NEW cls.mipcategory( INPUT ? ,INPUT ?)
    iNumberOfEntries = NUM-ENTRIES(ipcActivateHealthCategories, ",":U).

  DO iEntry = 1 TO iNumberOfEntries:
    ASSIGN
      cCategory = ENTRY(iEntry,ipcActivateHealthCategories)
      lSuccess  = oCategoryHelper:focusCategory(cCategory).
    
    IF NOT oCategoryHelper:CategoryInFocus THEN 
      ASSIGN 
        lSuccess = oErrorObject:addError(INPUT ipcMnemonic,
                                         INPUT ipdObj,
                                         INPUT "":U,
                                         INPUT ipcField,
                                         INPUT 0,
                                         INPUT "MA":U,
                                         INPUT 112,    /* The "&1" specified is invalid.&2 */
                                         INPUT "Category key '" + cCategory + "',A valid Category Key must be specified.")
        lInvalideActiveHealth = TRUE.
    ELSE
      IF STRING(oCategoryHelper:ParentCategoryKey) <> STRING(ipcParentCategory) THEN 
        ASSIGN 
          lSuccess             = oErrorObject:addError(INPUT ipcMnemonic,
                                                       INPUT ipdObj,
                                                       INPUT "":U,
                                                       INPUT ipcField,
                                                       INPUT 0,
                                                       INPUT "MA":U,
                                                       INPUT 112,    /* The "&1" specified is invalid.&2 */
                                                       INPUT "Category '" + oCategoryHelper:CategoryLabel + "',Only Category Keys with parent category key '" + ipcParentCategory + "' must be specified.")
          lInvalideActiveHealth = TRUE.

  END. /*DO iEntry = 1 TO iNumberOfEntries: */

  RETURN lInvalideActiveHealth.   /* Function return value. */
   
  {mip/inc/mipcatcherror.i &FORMAT  = TRUE &FINALLY = "IF VALID-OBJECT(oCategoryHelper)  THEN DELETE OBJECT oCategoryHelper.
                                                       IF VALID-OBJECT(oErrorObject)     THEN DELETE OBJECT oErrorObject."}
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValidateExclusionValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnValidateExclusionValues Procedure 
FUNCTION fnValidateExclusionValues RETURNS CHARACTER
  ( INPUT iphTableHandle       AS HANDLE,
    INPUT ipdBufferObj         AS DECIMAL,
    INPUT ipcExclFieldsAllowed AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  Only certain fields are allowed to contain values. This function
            will step through all the fields in the table and clear the fields
            that are not listed in the ipcExclFieldsAllowed parameter.
    Notes:  iphTableHandle -> The buffer name of the record that is validated. 
                             E.g. btt_auth_type_control 
                               or btt_auth_type_provider 
                               or btt_auth_type_detail
            ipdBufferObj -> The unique obj number of the record that is validated.
                            We will use this value to find the correct record.
            ipcExclFieldsAllowed -> A list of all the fields that may contain 
                                    a value when the record is an exclusion.
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cBufferName   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cField        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFieldName    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFieldLabel   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cObjName      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cQuery        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWarning      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWhereClause  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE httBuffer     AS HANDLE      NO-UNDO.
  DEFINE VARIABLE httQuery      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE iExtent       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iField        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartPos     AS INTEGER     NO-UNDO.

  /*
    Get a buffer handle for the temp-table record (iphTableHandle and ipdBufferObj) 
    that is being validated.
  */
  IF NOT VALID-HANDLE(iphTableHandle) THEN
  DO:
    {mip/inc/mipthrowerror.i 'MA' 100 ? ? "'Table Handle'"}
  END.  // IF NOT VALID-HANDLE(iphTableHandle) THEN

  CREATE QUERY httQuery.
  
  httBuffer = iphTableHandle:DEFAULT-BUFFER-HANDLE.

  httQuery:SET-BUFFERS(httBuffer) NO-ERROR.

  ASSIGN cBufferName  = httBuffer:NAME
         iStartPos    = INDEX(cBufferName,"_") + 1
         cObjName     = SUBSTRING(cBufferName,iStartPos) + "_obj":U
         cWhereClause = SUBSTITUTE("WHERE &1.&2 = &3", cBufferName, cObjName, ipdBufferObj)
         cQuery       = SUBSTITUTE("FOR EACH &1 &2":U, cBufferName, cWhereClause)
         /* The line_number and record_action are standard fields in all the temp-tables. 
            These two fields will also have values in them that should not be cleared. */
         ipcExclFieldsAllowed = "line_number,record_action," + ipcExclFieldsAllowed.

  httQuery:QUERY-PREPARE(cQuery).
  httQuery:QUERY-OPEN().
  httQuery:GET-FIRST().

  /*
    If the buffer handle is available, step through all the fields in the table and 
    check if the fieldname is in the list (ipcExclFieldsAllowed) that was passed through.
    If the fieldname is in the list, the field may contain a value.
    If the fieldname is not in the list, the field may not contain a value and should 
    be cleared. A warning should be returned if a field was cleared.
  */
  IF httBuffer:AVAILABLE THEN FIELDBLOCK:
  DO iField = 1 TO httBuffer:NUM-FIELDS:
    ASSIGN cFieldName  = httBuffer:BUFFER-FIELD(iField):NAME
           cFieldLabel = httBuffer:BUFFER-FIELD(cFieldName):LABEL.
    /* Is the fieldname in the list? */
    IF LOOKUP(cFieldName,ipcExclFieldsAllowed) = 0
    THEN
      /*
        Check the data type of the field so that we know what value to compare
        the field against, and also how to clear the field's value.
        If a field is being cleared, save it in a comma-delimited list (cWarning)
        to be used in the warning message at the end that will be returned to 
        the calling procedure.
      */
      CASE httBuffer:BUFFER-FIELD(cFieldName):DATA-TYPE:
        WHEN "character":U THEN
        DO:
          /* If the field is an array, we need to step through each
             extent in the array and check the value of each extent.
             Most fields are not an array, so we will handle these first. */
          IF httBuffer:BUFFER-FIELD(cFieldName):EXTENT = 0 THEN
          DO:
            IF httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE <> "" 
            THEN
              ASSIGN httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE = ""
                     cWarning = cWarning + ", " + cFieldLabel.
          END.  // IF httBuffer:BUFFER-FIELD(cFieldName):EXTENT = 0 THEN
          ELSE DO iExtent = 1 TO httBuffer:BUFFER-FIELD(cFieldName):EXTENT:
            IF httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE(iExtent) <> "" 
            THEN
              ASSIGN httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE(iExtent) = ""
                     cWarning = IF LOOKUP(cFieldLabel,cWarning) = 0
                                THEN cWarning + ", " + cFieldLabel
                                ELSE cWarning.
          END.  // ELSE DO iExtent = 1 TO httBuffer:BUFFER-FIELD(cFieldName):EXTENT:
        END.  // WHEN "character":U THEN
        
        WHEN "decimal":U OR
        WHEN "integer":U THEN
        DO:
          /* If the field is an array, we need to step through each
             extent in the array and check the value of each extent.
             Most fields are not an array, so we will handle these first. */
          IF httBuffer:BUFFER-FIELD(cFieldName):EXTENT = 0 THEN
          DO:
            IF httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE <> 0 
            THEN
              ASSIGN httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE = 0
                     cWarning = cWarning + ", " + cFieldLabel.
          END.  // IF httBuffer:BUFFER-FIELD(cFieldName):EXTENT = 0 THEN
          ELSE DO iExtent = 1 TO httBuffer:BUFFER-FIELD(cFieldName):EXTENT:
            IF httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE(iExtent) <> 0 
            THEN
              ASSIGN httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE(iExtent) = 0
                     cWarning = IF LOOKUP(cFieldLabel,cWarning) = 0
                                THEN cWarning + ", " + cFieldLabel
                                ELSE cWarning.
          END.  // ELSE DO iExtent = 1 TO httBuffer:BUFFER-FIELD(cFieldName):EXTENT:
        END.  /* WHEN "decimal":U OR WHEN "integer":U THEN */
        
        WHEN "date":U    OR
        WHEN "logical":U THEN
        DO:
          /* If the field is an array, we need to step through each
             extent in the array and check the value of each extent.
             Most fields are not an array, so we will handle these first. */
          IF httBuffer:BUFFER-FIELD(cFieldName):EXTENT = 0 THEN
          DO:
            IF httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE <> ? 
            THEN
              ASSIGN httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE = ?
                     cWarning = cWarning + ", " + cFieldLabel.
          END.  // IF httBuffer:BUFFER-FIELD(cFieldName):EXTENT = 0 THEN
          ELSE DO iExtent = 1 TO httBuffer:BUFFER-FIELD(cFieldName):EXTENT:
            IF httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE(iExtent) <> ? 
            THEN
              ASSIGN httBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE(iExtent) = ?
                     cWarning = IF LOOKUP(cFieldLabel,cWarning) = 0
                                THEN cWarning + ", " + cFieldLabel
                                ELSE cWarning.
          END.  // ELSE DO iExtent = 1 TO httBuffer:BUFFER-FIELD(cFieldName):EXTENT:
        END.  /* WHEN "date":U OR WHEN "logical":U THEN */
      END CASE.  /* CASE httBuffer:BUFFER-FIELD(cFieldName):DATA-TYPE: */
  END.  /* IF httBuffer:AVAILABLE THEN DO iField = 1 TO httBuffer:NUM-FIELDS:*/

  /*
    If any fields were cleared, then assign a warning message to variable cWarning.
    This variable with the warning message will be returned to the calling procedure.
  */
  IF cWarning <> ""
  THEN
    ASSIGN cField   = IF NUM-ENTRIES(cWarning) = 2 // We check for 2, because the string will always begin with a comma
                      THEN "field was"
                      ELSE "fields were"
           cWarning = SUBSTITUTE("The following &1 cleared as it can not contain a value for an exclusion: &2",
                                 cField,SUBSTRING(cWarning,3)).

  RETURN cWarning.

  { mip/inc/mipcatcherror.i
     &FINALLY = "IF VALID-HANDLE(httQuery) THEN
                 DO:
                   IF httQuery:IS-OPEN THEN
                   DO:
                     httQuery:QUERY-CLOSE().
                     ~{mip/inc/mipmessageerror.i~}
                   END. // IF httQuery:IS-OPEN THEN

                   DELETE OBJECT httQuery.
                   ~{mip/inc/mipmessageerror.i~}
                 END.  // IF VALID-HANDLE(httQuery) THEN
                 "
                 }
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValidateRoles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnValidateRoles Procedure 
FUNCTION fnValidateRoles RETURNS LOGICAL
  ( INPUT  ipcRole         AS CHARACTER,
    INPUT  ipcMnemonic     AS CHARACTER,
    INPUT  ipdObj          AS DECIMAL,
    INPUT  ipcField        AS CHARACTER,
    INPUT  ipiLineNumber   AS INTEGER,
    INPUT  ipcErrorMessage AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCnt                    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE ipcRoleMnemonic         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ipdRoleObj              AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE ipcRoleKey              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcRoleCode             AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcRoleMnemonic         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opdRoleObj              AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE opcRoleKey              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcRoleLabel            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcRoleDescription      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcRoleApplication      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcRoleStatus           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFailure                AS LOGICAL    NO-UNDO INITIAL FALSE.
  
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject NO-UNDO.
  
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).
 
  /* Validate comma delimited list of roles */
  IF  ipcRole <> "":U 
  AND ipcRole <> "None Allowed":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(ipcRole):
    
    FIND FIRST mim_role NO-LOCK
      WHERE mim_role.role_code  = ENTRY(iCnt, ipcRole)
        AND mim_role.status_key = "mip_StMimroAct":U
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mim_role
    THEN DO:
     ASSIGN lFailure = TRUE.

     oErrorObject:addError(INPUT ipcMnemonic,
                           INPUT ipdObj,
                           INPUT "":U,
                           INPUT ipcField,
                           INPUT ipiLineNumber,
                           INPUT "MA":U,
                           INPUT 100,    /* The "&1" specified is invalid */
                           INPUT TRIM(ipcErrorMessage) + " " + STRING(ENTRY(iCnt, ipcRole))).
    END. /* IF NOT AVAILABLE mim_role */
  END. /* IF ipcRole <> "":U THEN */
  
  RETURN lFailure.   /* Function return value. */
  
  {mip/inc/mipcatcherror.i
     &FORMAT  = TRUE
     &FINALLY = "IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject."}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValidateUsers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnValidateUsers Procedure 
FUNCTION fnValidateUsers RETURNS LOGICAL
  ( INPUT  ipcUserList     AS CHARACTER,
    INPUT  ipcMnemonic     AS CHARACTER,
    INPUT  ipdObj          AS DECIMAL,
    INPUT  ipcField        AS CHARACTER,
    INPUT  ipiLineNumber   AS INTEGER,
    INPUT  ipcErrorMessage AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  Validate a comma-delimited list of user id's
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCnt                    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cUserMnemonic           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dUserObj                AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cUserKey                AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcUserCode             AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcUserMnemonic         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opdUserObj              AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE opcUserKey              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcUserLabel            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcUserDescription      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcUserApplication      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE opcUserStatus           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFailure                AS LOGICAL    NO-UNDO INITIAL FALSE.
  
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject NO-UNDO.
  
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

  /* Validate comma delimited list of users */
  IF  ipcUserList <> "":U
  AND ipcUSerList <> "None Allowed":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(ipcUserList):
    mipEnv:miUser:focusUser(ENTRY(iCnt, ipcUserList)) NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'mip_MsgUsrErr:4' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "mip_MsgUsrErr:4":U 
    OR NOT mipEnv:miUser:UserInFocus 
    OR mipEnv:miUser:UserStatus <> "mip_StMimusAct":U THEN
    DO:
      cls.miperror:resetError().
      
      ASSIGN lFailure = TRUE.
      
      oErrorObject:addError(INPUT ipcMnemonic,
                            INPUT ipdObj,
                            INPUT "":U,
                            INPUT ipcField,
                            INPUT ipiLineNumber,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT TRIM(ipcErrorMessage) + " " + STRING(ENTRY(iCnt, ipcUserList))).
    END. /* IF cls.miperror:getMessageGroupNumber() = "mip_MsgUsrErr:4":U */
  END. /* IF ipcUserList <> "":U THEN */
  
  RETURN lFailure.   /* Function return value. */
  
  {mip/inc/mipcatcherror.i
     &FORMAT  = TRUE
     &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

