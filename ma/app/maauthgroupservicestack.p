&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Authorisation Service stack
    
    Author : Andrewd

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthorisationdefs.i }

{ ma/inc/maauthgroupds.i }

{ ma/inc/maauthtypeds.i }

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
         HEIGHT             = 35.29
         WIDTH              = 60.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-fetchAuthGroupDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthGroupDataset Procedure 
PROCEDURE fetchAuthGroupDataset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
DEFINE INPUT        PARAMETER ipcWhatToGet   AS CHARACTER NO-UNDO.
   
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSuccess                   AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle AS HANDLE                    NO-UNDO. 
  DEFINE VARIABLE hDatasetHandle             AS HANDLE                    NO-UNDO. 
  DEFINE VARIABLE hBuffer                    AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE oSearchObject              AS cls.maauthgroupsearch NO-UNDO. 
  
  DEFINE DATA-SOURCE dsAuthGroup             FOR ham_auth_group.
  DEFINE DATA-SOURCE dsAuthGroupDetail       FOR ham_auth_group_detail.
  
  ASSIGN 
     hFilterCriteriaTableHandle = iophFilterCriteriaTableHandle
     hDatasetHandle             = iophDatasetHandle 
     oSearchObject              = NEW cls.maauthgroupsearch(INPUT DATASET-HANDLE iophDatasetHandle BY-REFERENCE). 
  
  BufferBlock:
  DO iCount = 1 TO iophDatasetHandle:NUM-BUFFERS:
  
    hBuffer = iophDatasetHandle:GET-BUFFER-HANDLE(iCount).
    
    /*If a buffer list has been specified then we will exclude all other buffers from being filled*/
    IF NOT CAN-DO(ipcWhatToGet, hBuffer:NAME) THEN
    DO:
      hBuffer:FILL-MODE = "NO-FILL":U.
      NEXT BufferBlock.
    END. /*IF NOT CAN-DO(ipcWhatToGet, hBuffer:NAME) THEN*/
    
    CASE hBuffer:NAME:
    
      WHEN "tt_auth_group":U        THEN lSuccess = hBuffer:ATTACH-DATA-SOURCE(DATA-SOURCE dsAuthGroup:HANDLE ).  
      WHEN "tt_auth_group_detail":U THEN lSuccess = hBuffer:ATTACH-DATA-SOURCE(DATA-SOURCE dsAuthGroupDetail:HANDLE).     
      OTHERWISE
         hBuffer:FILL-MODE = "NO-FILL":U.
    END CASE. /* CASE hBuffer:NAME: */
  
  END. /* BufferBlock: */
  
  oSearchObject:DatasetHandle:SET-CALLBACK-PROCEDURE("AFTER-FILL":U,"_afterFill",TARGET-PROCEDURE).
  
  ASSIGN 
     lSuccess                      = oSearchObject:populateDataset(INPUT iophFilterCriteriaTableHandle)
     iophDatasetHandle             = oSearchObject:DatasetHandle 
     iophFilterCriteriaTableHandle = oSearchObject:CriteriaTableHandle.

  {mip/inc/mipcatcherror.i     
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

&IF DEFINED(EXCLUDE-saveAuthGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthGroup Procedure 
PROCEDURE saveAuthGroup :
/*------------------------------------------------------------------------------
  Purpose   : Save auth  group from dataset    
  Parameters: dsAuthGroup as defined in maauthgroupds.i
  Notes     : - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : Christob
  
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthGroup.
  
&IF {&DBDFMA} >= 10195 &THEN                             
  
  RUN _saveAuthGroup IN TARGET-PROCEDURE.
    
  RUN _saveAuthGroupDetail IN TARGET-PROCEDURE.
    
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    FIND FIRST tt_auth_group_result EXCLUSIVE-LOCK NO-ERROR.
      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE tt_auth_group_result
    THEN
      CREATE tt_auth_group_result.
        
    FOR EACH tt_auth_group_error NO-LOCK:    
      ASSIGN tt_auth_group_result.number_of_errors = tt_auth_group_result.number_of_errors + 1.
    END. /*FOR EACH tt_auth_group_error NO-LOCK:*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
      
  { mip/inc/mipcatcherror.i }
    
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_afterFill) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _afterFill Procedure 
PROCEDURE _afterFill :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters:  <none>
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE iphDatasetHandle.
  
  DEFINE VARIABLE hQuery  AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO.
  
  
  ASSIGN hBuffer = iphDatasetHandle:GET-BUFFER-HANDLE("tt_auth_group_detail":U).
  
  CREATE QUERY hQuery.
  
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK":U, hBuffer:NAME)).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  
  DO WHILE NOT hQuery:QUERY-OFF-END:   
  
    IF hBuffer::owning_entity_mnemonic = "htmtl":U THEN
    DO:
      FIND FIRST htm_tariff_link NO-LOCK
           WHERE htm_tariff_link.tariff_link_obj = hBuffer::owning_obj
        NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE htm_tariff_link 
      THEN
        ASSIGN
           hBuffer::ars_rate    = htm_tariff_link.ars_rate
           hBuffer::base_rate   = htm_tariff_link.base_rate
           hBuffer::pr_type     = htm_tariff_link.pr_type
&IF {&DBDFHMA} < 010009 &THEN
           hBuffer::sub_pr_type = htm_tariff_link.sub_pr_type
&ENDIF
           .
     
    END. /*IF hBuffer::owning_entity_mnemonic = "htmtl":U THEN*/
    
    hQuery:GET-NEXT().
    
  END. /*DO WHILE NOT hQuery:QUERY-OFF-END:   */
  
  
  {mip/inc/mipcatcherror.i
     &FINALLY = "IF VALID-HANDLE(hQuery) THEN 
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

&IF DEFINED(EXCLUDE-_deleteAuthGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthGroup Procedure 
PROCEDURE _deleteAuthGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipdAuthGroupObj AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject NO-UNDO. 
  DEFINE VARIABLE lFailureOccurred AS LOGICAL           NO-UNDO.
    
&IF {&DBDFMA} >= 10195 &THEN                             
  
  DEFINE BUFFER btt_auth_group_result FOR tt_auth_group_result.
  DEFINE BUFFER btt_auth_group_error  FOR tt_auth_group_error.
  DEFINE BUFFER buf_auth_group        FOR ham_auth_group.
  DEFINE BUFFER buf_auth_group_detail FOR ham_auth_group_detail.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_group_error:HANDLE).
    
    FIND FIRST tt_auth_group_result EXCLUSIVE-LOCK NO-ERROR.
      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE tt_auth_group_result 
    THEN
      CREATE tt_auth_group_result.
      
    /*
      Remove all dependencies to ensure we dont leave any orphan records.
    */           
    IF NOT lFailureOccurred THEN
    DO:
      for-blk:
      FOR EACH buf_auth_group_detail NO-LOCK
         WHERE buf_auth_group_detail.auth_group_obj = ipdAuthGroupObj:
    
        RUN _deleteAuthGroupDetail IN TARGET-PROCEDURE(INPUT buf_auth_group_detail.auth_group_detail_obj).
        
        ASSIGN lFailureOccurred = IF lFailureOccurred 
                                  THEN lFailureOccurred 
                                  ELSE CAN-FIND(FIRST btt_auth_group_error NO-LOCK 
                                                WHERE btt_auth_group_error.owning_entity_mnemonic = "hamag":U
                                                  AND btt_auth_group_error.owning_obj = buf_auth_group_detail.auth_group_obj).
      END. /*FOR EACH buf_auth_group_detail NO-LOCK*/
    END.
    
    /*
      Dependencies remain
    */
    IF CAN-FIND(FIRST buf_auth_group_detail NO-LOCK WHERE buf_auth_group_detail.auth_group_obj = ipdAuthGroupObj)
    THEN 
    DO:
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hamag":U, 
                            INPUT ipdAuthGroupObj,
                            INPUT "":U,
                            INPUT "auth_group_code":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                            INPUT "Auth group,Auth Group Detail":U).
      
    END. /* IF CAN-FIND(FIRST buf_auth_group_detail  */    
    
    IF NOT lFailureOccurred THEN
    DO:
      FIND FIRST buf_auth_group EXCLUSIVE-LOCK
           WHERE buf_auth_group.auth_group_obj = ipdAuthGroupObj
        NO-ERROR NO-WAIT.
    
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
        
      /* 
        Unable to remove - record is already locked
      */  
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        cls.miperror:resetError().
        
        ASSIGN lFailureOccurred = TRUE.

        oErrorObject:addError(INPUT "hamag":U, 
                              INPUT ipdAuthGroupObj,
                              INPUT "":U,
                              INPUT "auth_group_code":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Auth Group":U).            
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/  
          
      IF AVAILABLE buf_auth_group AND NOT lFailureOccurred THEN    
      DO:  
        DELETE buf_auth_group.                        
        
        ASSIGN tt_auth_group_result.records_removed = tt_auth_group_result.records_removed + 1.
        
      END. /*IF AVAILABLE buf_auth_group AND NOT lFailureOccurred THEN    */
    END. /*IF NOT lFailureOccurred THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
     
&ENDIF                            
  
  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthGroupDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthGroupDetail Procedure 
PROCEDURE _deleteAuthGroupDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthGroupDetailObj AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN                             
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL   NO-UNDO.
  
  DEFINE BUFFER buf_auth_group_detail  FOR ham_auth_group_detail.
  DEFINE BUFFER btt_auth_group_error   FOR tt_auth_group_error.
  DEFINE BUFFER btt_auth_group_result  FOR tt_auth_group_result.
                       
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_group_error:HANDLE).
                                           
    FIND FIRST btt_auth_group_result EXCLUSIVE-LOCK NO-ERROR.
      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_group_result 
    THEN
      CREATE btt_auth_group_result.
       
    FIND FIRST buf_auth_group_detail EXCLUSIVE-LOCK
         WHERE buf_auth_group_detail.auth_group_detail_obj = ipdAuthGroupDetailObj
      NO-ERROR NO-WAIT.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
        
    /*
      Unable to remove - record is already locked
    */      
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().
      
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hamad":U,
                            INPUT ipdAuthGroupDetailObj,
                            INPUT "":U,
                            INPUT "owning_alt_value":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Group Detail":U).                              
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/  
    
    IF AVAILABLE buf_auth_group_detail AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_group_detail.
      
      ASSIGN btt_auth_group_result.records_removed = btt_auth_group_result.records_removed + 1.
    END. /*IF AVAILABLE buf_auth_group_detail AND NOT lFailureOccurred THEN*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
  
&ENDIF

{ mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthGroup Procedure 
PROCEDURE _saveAuthGroup :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete auth  group information
  Parameters: 
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : gifts
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSuccess            AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL           NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE iError              AS INTEGER           NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject NO-UNDO.
  
  
  DEFINE BUFFER buf_auth_group        FOR ham_auth_group.
  DEFINE BUFFER btt_auth_group_result FOR tt_auth_group_result.
  DEFINE BUFFER btt_auth_group_error  FOR tt_auth_group_error.
  DEFINE BUFFER btt_auth_group        FOR tt_auth_group.
  DEFINE BUFFER btt_auth_group_detail FOR tt_auth_group_detail.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_group_error:HANDLE).
    
    
    FIND FIRST btt_auth_group_result EXCLUSIVE-LOCK NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_group_result 
    THEN
      CREATE btt_auth_group_result.     
    
    RECORD-BLK:                             
    FOR EACH btt_auth_group EXCLUSIVE-LOCK
          BY btt_auth_group.auth_group_obj DESCENDING:
    
      ASSIGN 
         lFailureOccurred = FALSE       
         btt_auth_group_result.records_processed = btt_auth_group_result.records_processed + 1.
                
      IF btt_auth_group.record_action = "MODIFY":U THEN
      DO:
        /*Ensure that an effective date has been specified*/
        IF btt_auth_group.effective_date = ? THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
          
          oErrorObject:addError(INPUT "hamag":U, 
                                INPUT btt_auth_group.auth_group_obj, 
                                INPUT "":U, 
                                INPUT "effective_date":U,
                                INPUT btt_auth_group.line_number, 
                                INPUT "MA":U, 
                                INPUT 111,  /* The &1 must be specified. &2 */
                                INPUT "Auth group effective date").                  
        END. /*IF btt_auth_group.effective_date = ? THEN*/
        
        /*Ensure that a valid insurer has been specified*/
        IF btt_auth_group.insurer_obj <> 0.00 THEN
        DO:
          FIND FIRST erm_insurer NO-LOCK
               WHERE erm_insurer.insurer_obj = btt_auth_group.insurer_obj 
            NO-ERROR.
            
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE erm_insurer THEN 
          DO:
            ASSIGN lFailureOccurred = TRUE.

            oErrorObject:addError(INPUT "hamag":U, 
                                  INPUT btt_auth_group.auth_group_obj, 
                                  INPUT "":U, 
                                  INPUT "insurer_obj":U,
                                  INPUT btt_auth_group.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 100,   /* The "&1" specified is invalid */
                                  INPUT " Auth Group Insurer (Obj=":U + STRING(btt_auth_group.insurer_obj) + ")":U).                  
          END. /*IF NOT AVAILABLE erm_insurer THEN */
        END. /*IF btt_auth_group.insurer_obj <> 0.00 THEN*/
        
        IF lFailureOccurred
        THEN 
          NEXT RECORD-BLK.
          
        
        FIND FIRST buf_auth_group EXCLUSIVE-LOCK
             WHERE buf_auth_group.auth_group_obj = btt_auth_group.auth_group_obj
          NO-ERROR NO-WAIT.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
        
        /*The auth  group record is locked by another user or process*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN 
             lFailureOccurred = TRUE
             
             btt_auth_group_result.records_locked = btt_auth_group_result.records_locked + 1.
                       
          oErrorObject:addError(INPUT "hamag":U, 
                                INPUT btt_auth_group.auth_group_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_group.line_number, 
                                INPUT "MA":U, 
                                INPUT 200,   /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth  group (Obj=":U + STRING(btt_auth_group.auth_group_obj) + ")":U).
                           
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
        
        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().
          
          /*Duplicate check*/ 
          IF CAN-FIND(FIRST buf_auth_group NO-LOCK
                      WHERE buf_auth_group.insurer_obj      = btt_auth_group.insurer_obj
                        AND buf_auth_group.option_code      = btt_auth_group.option_code
                        AND buf_auth_group.auth_group_code  = btt_auth_group.auth_group_code
                        AND buf_auth_group.effective_date   = btt_auth_group.effective_date)
          OR CAN-FIND(FIRST buf_auth_group NO-LOCK
                      WHERE buf_auth_group.insurer_obj      = btt_auth_group.insurer_obj
                        AND buf_auth_group.option_code      = btt_auth_group.option_code
                        AND buf_auth_group.auth_group_code  = btt_auth_group.auth_group_code
                        AND buf_auth_group.end_date         = btt_auth_group.end_date) 
          THEN
          DO:
            ASSIGN lFailureOccurred = TRUE.
            
            oErrorObject:addError(INPUT "hamag":U, 
                                  INPUT btt_auth_group.auth_group_obj, 
                                  INPUT "":U, 
                                  INPUT "auth_group_code":U,
                                  INPUT btt_auth_group.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 125,   /* &1 already exists with &2 */  
                                  INPUT "Auth  Group,Opt=":U +
                                        STRING(btt_auth_group.option_code)          + " - Grp.Code=":U + 
                                        STRING(btt_auth_group.auth_group_code) + " - Ins=":U + 
                                        STRING(btt_auth_group.insurer_obj)          + " - Eff.Date=":U + 
                                        STRING(btt_auth_group.effective_date,"9999/99/99") + " - End.Date=":U +
                                        STRING(btt_auth_group.end_date,"9999/99/99")).
                           
          END. /*IF CAN-FIND(FIRST buf_auth_group NO-LOCK*/                        
                      
          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_group.
            
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
        
        IF AVAILABLE buf_auth_group AND NOT lFailureOccurred THEN
        DO:        
          /*An existing record is being updated*/
          IF btt_auth_group.auth_group_obj > 0 
          THEN ASSIGN btt_auth_group_result.records_modified = btt_auth_group_result.records_modified + 1.
          ELSE ASSIGN btt_auth_group_result.records_created  = btt_auth_group_result.records_created  + 1.
                             
          BUFFER-COPY btt_auth_group 
               EXCEPT btt_auth_group.auth_group_obj 
                   TO buf_auth_group.        
          
          /*If we are creating a new record we need to make sure that we run through all the dependants
            and assign the correct _obj*/
          IF btt_auth_group.auth_group_obj < 1 THEN
          DO:
            FOR EACH tt_auth_group_detail EXCLUSIVE-LOCK
               WHERE tt_auth_group_detail.auth_group_obj = btt_auth_group.auth_group_obj:
              ASSIGN tt_auth_group_detail.auth_group_obj = buf_auth_group.auth_group_obj.  
            END. /*FOR EACH tt_auth_group_detail*/
          END. /*IF btt_auth_group.auth_group_obj < 1 THEN*/
                   
          ASSIGN 
             btt_auth_group.auth_group_obj = buf_auth_group.auth_group_obj
             btt_auth_group.record_action  = "":U.
          
          VALIDATE buf_auth_group.          
          
          FIND CURRENT buf_auth_group NO-LOCK.
        END. /*IF AVAILABLE buf_auth_group AND NOT lFailureOccurred THEN*/
      END. /*IF btt_auth_group.record_action = "MODIFY":U THEN*/
      
      IF btt_auth_group.record_action = "DELETE":U THEN
      DO:    
        /*This routine will ensure that all dependencies will also be removed*/
        RUN _deleteAuthGroup IN TARGET-PROCEDURE (INPUT btt_auth_group.auth_group_obj).
        
        IF NOT CAN-FIND(FIRST btt_auth_group_error NO-LOCK
                        WHERE btt_auth_group_error.owning_entity_mnemonic = "hamag":U
                          AND btt_auth_group_error.owning_obj             = btt_auth_group.auth_group_obj)
        THEN
          DELETE btt_auth_group.
                                              
      END. /*IF btt_auth_group.record_action = "DELETE":U THEN*/           
    END. /*FOR EACH btt_auth_group EXCLUSIVE-LOCK*/      
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
  
  { mip/inc/mipcatcherror.i
       &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}
           
&ENDIF  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthGroupDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthGroupDetail Procedure 
PROCEDURE _saveAuthGroupDetail :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete auth group detail information
  Parameters: 
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : Andrewd
------------------------------------------------------------------------------*/

  { ma/app/maauthgrpsrvstacksaveagdetail.i }
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

