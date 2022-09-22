&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    $Id: maauthcopaytypeservicestack.p       Exp $ 

    Purpose: Stack procedure for Auth Copay Type  
    Author:  GrahamW

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */

ROUTINE-LEVEL ON ERROR UNDO, THROW. /*This is used to return error messages from an include file                  
                                     that was written for character where no no-errors have been used.            
                                     The error is returned immediately all the way back to the calling procedure      
                                     when encountered*/      
CREATE WIDGET-POOL.
{ sysadmma.i}
{ mip/inc/mipdefshared.i}

{ ma/inc/maauthcopaytypeds.i }

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
         HEIGHT             = 28.14
         WIDTH              = 78.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-fetchAuthCopayTypeDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthCopayTypeDataset Procedure 
PROCEDURE fetchAuthCopayTypeDataset :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Auth Copay Type Dataset    
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all 
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }            
    Author    : GrahamW
    
      NB- DEVELOPERS ******  ALL CHANGES BEFORE DELTA 185 SHOULD GO IN THE SUPER PROCEDURE
          AND ALL CHANGES AFTER DELTA 185 SHOULD BE IN THE STANDARD STACK ************
  ------------------------------------------------------------------------------*/   
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
  DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
  DEFINE INPUT        PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO. 
  
&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lSuccess                   AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER                     NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle AS HANDLE                      NO-UNDO. 
  DEFINE VARIABLE hDatasetHandle             AS HANDLE                      NO-UNDO. 
  DEFINE VARIABLE hBuffer                    AS HANDLE                      NO-UNDO.
  DEFINE VARIABLE hAuthCopayType             AS HANDLE                      NO-UNDO.
  DEFINE VARIABLE hAuthCopayTypeError        AS HANDLE                      NO-UNDO.
  DEFINE VARIABLE hAuthCopayTypeResult       AS HANDLE                      NO-UNDO.
  DEFINE VARIABLE oSearchObject              AS cls.maauthcopaytypesearch  NO-UNDO.
  
  DEFINE DATA-SOURCE srcAuthCopayType FOR ham_auth_copay_type.
  
  ASSIGN      
     hFilterCriteriaTableHandle = iophFilterCriteriaTableHandle
     hDatasetHandle             = iophDatasetHandle
     oSearchObject              = NEW cls.maauthcopaytypesearch(INPUT DATASET-HANDLE iophDatasetHandle).  
  
  
  ASSIGN
     hAuthCopayType                 = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_type":U)
     hAuthCopayTypeError            = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_type_error":U)
     hAuthCopayTypeResult           = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_copay_type_result":U)
                                     
     lSuccess                       = hAuthCopayType:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthCopayType:HANDLE, oSearchObject:getMappedFieldList(BUFFER ham_auth_copay_type:HANDLE, hAuthCopayType))
     
     hAuthCopayTypeError:FILL-MODE  = "NO-FILL":U 
     hAuthCopayTypeResult:FILL-MODE = "NO-FILL":U.
  
  
  ASSIGN
     lSuccess = oSearchObject:populateDataset(INPUT iophFilterCriteriaTableHandle)
    
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

&IF DEFINED(EXCLUDE-saveAuthCopayType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthCopayType Procedure 
PROCEDURE saveAuthCopayType :
/*------------------------------------------------------------------------------
  Purpose   : Save Auth Copay Type from dataset    
  Parameters: dsAuthCopayType as defined in maAuthCopayTypeds.i
  Notes     : - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : GrahamW
  
    NB- DEVELOPERS ******  ALL CHANGES BEFORE DELTA 185 SHOULD GO IN THE SUPER PROCEDURE
          AND ALL CHANGES AFTER DELTA 185 SHOULD BE IN THE STANDARD STACK ************
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthCopayType. 
    
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSuccess                  AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lFailureOccurred          AS LOGICAL              NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE oErrorObject              AS cls.maerrorobject    NO-UNDO.  
  
  DEFINE BUFFER buf_auth_copay_type         FOR ham_auth_copay_type.
  DEFINE BUFFER buf_auth_copay_detail_item  FOR hac_auth_copay_detail_item.
  DEFINE BUFFER btt_auth_copay_type         FOR tt_auth_copay_type.
  DEFINE BUFFER btt_auth_copay_type_result  FOR tt_auth_copay_type_result.
  DEFINE BUFFER btt_auth_copay_type_error   FOR tt_auth_copay_type_error.
  
                                      
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_type_error:HANDLE).
     
    FIND FIRST btt_auth_copay_type_result EXCLUSIVE-LOCK NO-ERROR.
      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_copay_type_result 
    THEN
      CREATE btt_auth_copay_type_result.                             
    
    RECORD-BLK:    
    FOR EACH btt_auth_copay_type EXCLUSIVE-LOCK
          BY btt_auth_copay_type.auth_copay_type_obj: 
             
      ASSIGN 
          lFailureOccurred                             = FALSE
          btt_auth_copay_type_result.records_processed = btt_auth_copay_type_result.records_processed + 1.
          
      IF btt_auth_copay_type.record_action = "MODIFY":U THEN
      DO:
        IF btt_auth_copay_type.auth_copay_type = "":U THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
          
          /*
            The "&1" specified is invalid
          */
          oErrorObject:addError(INPUT "hamct":U, 
                                INPUT btt_auth_copay_type.auth_copay_type_obj, 
                                INPUT "":U, 
                                INPUT "auth_copay_type":U,
                                INPUT btt_auth_copay_type.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Auth copay type:":U + btt_auth_copay_type.auth_copay_type).                  
                                
        END. /*IF btt_auth_copay_type.auth_copay_type = "":U THEN*/

        /*Duplicate check*/ 
        IF CAN-FIND(FIRST buf_auth_copay_type NO-LOCK
                    WHERE buf_auth_copay_type.auth_copay_type      = btt_auth_copay_type.auth_copay_type
                    AND   buf_auth_copay_type.auth_copay_type_obj <> btt_auth_copay_type.auth_copay_type_obj) 
        THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
          
          oErrorObject:addError(INPUT "hamct":U, 
                                INPUT btt_auth_copay_type.auth_copay_type_obj, 
                                INPUT "":U, 
                                INPUT "auth_copay_type":U,
                                INPUT btt_auth_copay_type.line_number, 
                                INPUT "MA":U, 
                                INPUT 125,  /* &1 already exists with &2 */
                                INPUT "Auth Copay Type record, Auth Copay Type '":U + STRING(btt_auth_copay_type.auth_copay_type) + "'").
                         
        END. /*IF CAN-FIND(FIRST buf_auth_copay_type NO-LOCK*/                        
        
        FIND FIRST buf_auth_copay_type EXCLUSIVE-LOCK
             WHERE buf_auth_copay_type.auth_copay_type_obj = btt_auth_copay_type.auth_copay_type_obj
          NO-ERROR NO-WAIT.
        
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /*The auth_copay_type record is locked by another user or process*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN 
             lFailureOccurred = TRUE
             btt_auth_copay_type_result.records_locked = btt_auth_copay_type_result.records_locked + 1.
          
          /*
            Record "&1" is locked and cannot be read for updating, please try again.
          */             
          oErrorObject:addError(INPUT "hamct":U, 
                                INPUT btt_auth_copay_type.auth_copay_type_obj, 
                                INPUT "":U, btt_auth_copay_type.line_number, 
                                INPUT "MA":U, 
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth Copay Type: ":U + STRING(btt_auth_copay_type.auth_copay_type)).
                           
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
        
        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().
                                
          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_copay_type.
            
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
        
        IF AVAILABLE buf_auth_copay_type AND NOT lFailureOccurred THEN
        DO:        
          /*An existing record is being updated*/
          IF btt_auth_copay_type.auth_copay_type_obj > 0 
          THEN ASSIGN btt_auth_copay_type_result.records_modified = btt_auth_copay_type_result.records_modified + 1.
          ELSE ASSIGN btt_auth_copay_type_result.records_created  = btt_auth_copay_type_result.records_created  + 1.
                             
          BUFFER-COPY btt_auth_copay_type 
               EXCEPT btt_auth_copay_type.auth_copay_type_obj 
                   TO buf_auth_copay_type.        
          
          ASSIGN 
             btt_auth_copay_type.auth_copay_type_obj = buf_auth_copay_type.auth_copay_type_obj
             btt_auth_copay_type.record_action = "":U.
          
          VALIDATE buf_auth_copay_type.          
          
          FIND CURRENT buf_auth_copay_type NO-LOCK.

          cls.miperror:resetError().
          
        END. /*IF AVAILABLE buf_auth_copay_type AND NOT lFailureOccurred THEN*/
      END.  /*IF btt_auth_copay_type.record_action = "MODIFY":U THEN */
        
      IF btt_auth_copay_type.record_action = "DELETE":U THEN
      DO:  
        IF CAN-FIND(FIRST buf_auth_copay_detail_item NO-LOCK
          WHERE buf_auth_copay_detail_item.auth_copay_type_obj = btt_auth_copay_type.auth_copay_type_obj) THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
          
          /*
            Child Record exists for Auth Copay Type record, cannot delete it.
          */
          oErrorObject:addError(INPUT "hamct":U, 
                                INPUT btt_auth_copay_type.auth_copay_type_obj,
                                INPUT "":U,
                                INPUT 1,
                                INPUT "MA":U,
                                INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                                INPUT "Auth Copay Type: ":U + STRING(btt_auth_copay_type.auth_copay_type) +
                                      ", Auth Copay Detail Item":U).            
        END.  /*IF CAN-FIND(FIRST buf_auth_copay_detail_item NO-LOCK*/

        FIND FIRST buf_auth_copay_type EXCLUSIVE-LOCK
          WHERE buf_auth_copay_type.auth_copay_type_obj = btt_auth_copay_type.auth_copay_type_obj
          NO-ERROR NO-WAIT.
    
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /*Unable to remove - record is already locked*/  
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN lFailureOccurred = TRUE.
          
          /*
            Record "&1" is locked and cannot be read for updating, please try again.
          */
          oErrorObject:addError(INPUT "hamct":U, 
                                INPUT btt_auth_copay_type.auth_copay_type_obj,
                                INPUT "":U,
                                INPUT 1,
                                INPUT "MA":U,
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth Copay Type: ":U + STRING(btt_auth_copay_type.auth_copay_type)).            
                           
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/  
            
        IF AVAILABLE buf_auth_copay_type AND NOT lFailureOccurred THEN    
        DO:  
          DELETE buf_auth_copay_type.
          
          ASSIGN btt_auth_copay_type_result.records_removed = btt_auth_copay_type_result.records_removed + 1.
        END. /*IF AVAILABLE buf_auth_copay_type AND NOT lFailureOccurred THEN    */
        
        IF NOT CAN-FIND(FIRST btt_auth_copay_type_error NO-LOCK
                        WHERE btt_auth_copay_type_error.owning_entity_mnemonic = 'hamat':U
                          AND btt_auth_copay_type_error.owning_obj = btt_auth_copay_type.auth_copay_type_obj)
        THEN
          DELETE btt_auth_copay_type.                                    
      END. /*END. /*IF btt_auth_copay_type.record_action = "DELETE":U THEN*/*/               
    END. /*FOR EACH tt_auth_copay_type_error EXCLUSIVE-LOCK:    */
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
      
  {mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}    
    
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

