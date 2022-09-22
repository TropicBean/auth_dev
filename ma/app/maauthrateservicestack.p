&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    $Id: maauthrateservicestack.p       Exp $

    Purpose: Stack procedure for Auth Rate Class

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthratecontrolds.i}

DEFINE TEMP-TABLE tt_copy_record
            FIELD old_auth_rate_control_obj   AS DECIMAL
            FIELD rate_provider_information   AS LOGICAL
            FIELD all_rate_detail_information AS LOGICAL
            FIELD cpt_information             AS LOGICAL
            FIELD cpt_exclusions              AS LOGICAL
            FIELD icd_information             AS LOGICAL
            FIELD icd_exclusions              AS LOGICAL
            FIELD flag_information            AS LOGICAL
            FIELD flag_exclusion              AS LOGICAL
            FIELD CW_information              AS LOGICAL
            FIELD CW_exclusion                AS LOGICAL
            FIELD override_ars_rate           AS CHARACTER.

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

&IF DEFINED(EXCLUDE-copyAuthRate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyAuthRate Procedure 
PROCEDURE copyAuthRate :
/*------------------------------------------------------------------------------
  Purpose: This service will allow for the auth rate control to be copied along
           with any providers and details the user wishes    
  Parameters:  temp-table input and auth rate control dataset
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER TABLE          FOR tt_copy_record.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthRateControl.

&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE cOEMInfoList        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOEMExclList        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessage            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessageOverrideARS AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lInformation        AS LOGICAL             NO-UNDO INITIAL FALSE.    
  DEFINE VARIABLE lExclusion          AS LOGICAL             NO-UNDO INITIAL FALSE.   
  DEFINE VARIABLE iCnt                AS INTEGER             NO-UNDO.

  DEFINE VARIABLE iRateProviders      AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailCPT      AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailICD      AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailFLG      AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailCRW      AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailCPTExcl  AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailICDExcl  AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailFLGExcl  AS INTEGER             NO-UNDO INITIAL 0. 
  DEFINE VARIABLE iRateDetailCRWExcl  AS INTEGER             NO-UNDO INITIAL 0. 

  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.
  
  DEFINE BUFFER buf_auth_rate_control       FOR hac_auth_rate_control.
  DEFINE BUFFER buf_old_auth_rate_provider  FOR hac_auth_rate_provider.
  DEFINE BUFFER buf_old_auth_rate_detail    FOR hac_auth_rate_detail.
  DEFINE BUFFER buf_auth_rate_detail        FOR hac_auth_rate_detail.
  DEFINE BUFFER buf_auth_rate_provider      FOR hac_auth_rate_provider.
  DEFINE BUFFER btt_auth_rate_control       FOR tt_auth_rate_control.
  DEFINE BUFFER btt_auth_rate_result        FOR tt_auth_rate_result.
  DEFINE BUFFER btt_auth_rate_error         FOR tt_auth_rate_error.
  DEFINE BUFFER btt_auth_rate_detail        FOR tt_auth_rate_detail.
  DEFINE BUFFER btt_auth_rate_provider      for tt_auth_rate_provider.

  ASSIGN oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

  /*
    In copying a record we will only ever have one btt_auth_rate_control record, thus we only get the first one
  */
  FIND FIRST btt_auth_rate_control NO-LOCK.
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

  IF AVAILABLE btt_auth_rate_control 
  AND btt_auth_rate_control.record_action = "MODIFY":U THEN
  DO:
    /* We commit the new rate control record to the database, pending it passing validation within the service */
    RUN _saveAuthRateControl IN TARGET-PROCEDURE.

    /* Only one record will be present with the values from the UI */
    FIND FIRST tt_copy_record NO-LOCK.
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    /* Below we systematically go through the copy record setup to see what needs to be saves with the new rate control record */
    IF AVAILABLE btt_auth_rate_control AND AVAILABLE tt_copy_record THEN
    DO: 
      /* Below code will copy all providers over to new rate control setup */
      IF tt_copy_record.rate_provider_information THEN
        FOR EACH buf_old_auth_rate_provider NO-LOCK
           WHERE buf_old_auth_rate_provider.auth_rate_control_obj = tt_copy_record.old_auth_rate_control_obj:

          ASSIGN iRateProviders = iRateProviders + 1.

          CREATE tt_auth_rate_provider.

          BUFFER-COPY buf_old_auth_rate_provider
               EXCEPT buf_old_auth_rate_provider.auth_rate_control_obj buf_old_auth_rate_provider.auth_rate_provider_obj
                   TO tt_auth_rate_provider.

          /* Assign the values that we dont want buffer copied */
          ASSIGN iCnt = 1 + iCnt
                 tt_auth_rate_provider.auth_rate_provider_obj = iCnt * -2
                 tt_auth_rate_provider.auth_rate_control_obj  = btt_auth_rate_control.auth_rate_control_obj
                 tt_auth_rate_provider.effective_date         = btt_auth_rate_control.effective_date
                 tt_auth_rate_provider.end_date               = btt_auth_rate_control.end_date
                 tt_auth_rate_provider.record_action          = "MODIFY":U.

          /* Focus the auth rate provider we have just assigned data to */
          FIND FIRST tt_auth_rate_provider NO-LOCK 
            NO-ERROR.
          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

          /* We use the detail service to comit the record to the database */
          RUN _saveAuthRateProvider IN TARGET-PROCEDURE.
        END. /* FOR EACH buf_old_auth_rate_provider NO-LOCK */

      /* Below code will copy all details over to new rate control setup this will include exclusions as well */ 
      IF tt_copy_record.all_rate_detail_information THEN
      DO: 

        FOR EACH buf_old_auth_rate_detail NO-LOCK
           WHERE buf_old_auth_rate_detail.auth_rate_control_obj = tt_copy_record.old_auth_rate_control_obj:

          CREATE tt_auth_rate_detail.
          BUFFER-COPY buf_old_auth_rate_detail
               EXCEPT buf_old_auth_rate_detail.auth_rate_control_obj buf_old_auth_rate_detail.auth_rate_detail_obj
                   TO tt_auth_rate_detail.

          /* Assign the values that we dont want buffer copied */
          ASSIGN iCnt = 1 + iCnt
                 tt_auth_rate_detail.auth_rate_detail_obj  = iCnt * -2
                 tt_auth_rate_detail.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj
                 tt_auth_rate_detail.effective_date        = btt_auth_rate_control.effective_date
                 tt_auth_rate_detail.end_date              = btt_auth_rate_control.end_date
                 tt_auth_rate_detail.override_ars_rate     = IF btt_auth_rate_control.cpt_relative_value_type <> "":U 
                                                             AND tt_copy_record.override_ars_rate <> "":U AND buf_old_auth_rate_detail.override_ars_rate = "":U 
                                                             THEN tt_copy_record.override_ars_rate
                                                             ELSE buf_old_auth_rate_detail.override_ars_rate
                 tt_auth_rate_detail.record_action         = "MODIFY":U.

          /* Focus the auth rate provider we have just assigned data to */
          FIND FIRST tt_auth_rate_detail NO-LOCK 
            NO-ERROR.
          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

          IF buf_old_auth_rate_detail.exclusion = TRUE THEN
          DO:
            CASE buf_old_auth_rate_detail.owning_entity_mnemonic:
              WHEN "hlmck":U   THEN ASSIGN iRateDetailCPTExcl = iRateDetailCPTExcl + 1.
              WHEN "diagnos":U THEN ASSIGN iRateDetailICDExcl = iRateDetailICDExcl + 1.
              WHEN "hacar":U   THEN ASSIGN iRateDetailFLGExcl = iRateDetailFLGExcl + 1.
              WHEN "hlmcr":U   THEN ASSIGN iRateDetailCRWExcl = iRateDetailCRWExcl + 1.
            END CASE. /* CASE buf_old_auth_rate_detail.owning_entity_mnemonic: */
          END. /* IF buf_old_auth_rate_detail.exclusion = TRUE THEN */
          ELSE DO:
            CASE buf_old_auth_rate_detail.owning_entity_mnemonic:
              WHEN "hlmck":U   THEN ASSIGN iRateDetailCPT = iRateDetailCPT + 1.
              WHEN "diagnos":U THEN ASSIGN iRateDetailICD = iRateDetailICD + 1.
              WHEN "hacar":U   THEN ASSIGN iRateDetailFLG = iRateDetailFLG + 1.
              WHEN "hlmcr":U   THEN ASSIGN iRateDetailCRW = iRateDetailCRW + 1.
            END CASE. /* CASE buf_old_auth_rate_detail.owning_entity_mnemonic: */
          END. /* IF buf_old_auth_rate_detail.exclusion = TRUE THEN */

          /* We use the detail service to comit the record to the database */
          RUN _saveAuthRateDetail IN TARGET-PROCEDURE.
        END. /*FOR EACH btt_auth_rate_detail*/
      END. /* IF tt_copy_record.all_rate_detail_information THEN */
      ELSE
      DO:
        ASSIGN cOEMInfoList = "":U
               cOEMExclList = "":U.

        /* Below we will be building a comma seperated list of specifica details that the user wants to add to the new rate control records */
        IF tt_copy_record.cpt_information THEN
          ASSIGN cOEMInfoList = IF cOEMInfoList = "" THEN "hlmck" ELSE cOEMInfoList + ",hlmck"
                 lInformation = TRUE.
        
        IF tt_copy_record.cpt_exclusion THEN
          ASSIGN cOEMExclList = IF cOEMExclList = "" THEN "hlmck" ELSE cOEMExclList + ",hlmck"
                 lExclusion   = TRUE.

        IF tt_copy_record.icd_information THEN
          ASSIGN cOEMInfoList = IF cOEMInfoList = "" THEN "diagnos" ELSE cOEMInfoList + ",diagnos"
                 lInformation = TRUE.

        IF tt_copy_record.icd_exclusion THEN
          ASSIGN cOEMExclList = IF cOEMExclList = "" THEN "diagnos" ELSE cOEMExclList + ",diagnos"
                 lExclusion   = TRUE.

        IF tt_copy_record.flag_information THEN
          ASSIGN cOEMInfoList = IF cOEMInfoList = "" THEN "hacar"  ELSE cOEMInfoList + ",hacar"
                 lInformation = TRUE.

        IF tt_copy_record.flag_exclusion THEN
          ASSIGN cOEMExclList = IF cOEMExclList = "" THEN "hacar"  ELSE cOEMExclList + ",hacar"
                 lExclusion   = TRUE.

        IF tt_copy_record.CW_information THEN
          ASSIGN cOEMInfoList = IF cOEMInfoList = "" THEN "hlmcr"  ELSE cOEMInfoList + ",hlmcr"
                 lInformation = TRUE.

        IF tt_copy_record.CW_exclusion THEN
          ASSIGN cOEMExclList = IF cOEMExclList = "" THEN "hlmcr"  ELSE cOEMExclList + ",hlmcr"
                 lExclusion   = TRUE.

        /* We will now cycle though all information records that have been selected */
        IF lInformation THEN
        DO iCnt = 1 TO NUM-ENTRIES(cOEMInfoList, ","):
          FOR EACH buf_old_auth_rate_detail NO-LOCK
             WHERE buf_old_auth_rate_detail.auth_rate_control_obj  = tt_copy_record.old_auth_rate_control_obj
               AND buf_old_auth_rate_detail.owning_entity_mnemonic = ENTRY(iCnt,cOEMInfoList)
               AND buf_old_auth_rate_detail.exclusion              = FALSE:

            CREATE tt_auth_rate_detail.
            BUFFER-COPY buf_old_auth_rate_detail
                 EXCEPT buf_old_auth_rate_detail.auth_rate_control_obj buf_old_auth_rate_detail.auth_rate_detail_obj
                     TO tt_auth_rate_detail.

            ASSIGN iCnt = 1 + iCnt
                   tt_auth_rate_detail.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj
                   tt_auth_rate_detail.auth_rate_detail_obj  = iCnt * -2
                   tt_auth_rate_detail.effective_date        = btt_auth_rate_control.effective_date
                   tt_auth_rate_detail.end_date              = btt_auth_rate_control.end_date
                   tt_auth_rate_detail.override_ars_rate     = IF btt_auth_rate_control.cpt_relative_value_type <> "":U 
                                                               AND tt_copy_record.override_ars_rate <> "":U AND buf_old_auth_rate_detail.override_ars_rate = "":U 
                                                               THEN tt_copy_record.override_ars_rate
                                                               ELSE buf_old_auth_rate_detail.override_ars_rate

                   tt_auth_rate_detail.record_action         = "MODIFY":U.
            
            /* Focus the auth rate provider we have just assigned data to */
            FIND FIRST tt_auth_rate_detail NO-LOCK 
              NO-ERROR.
            {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

            CASE buf_old_auth_rate_detail.owning_entity_mnemonic:
              WHEN "hlmck":U   THEN ASSIGN iRateDetailCPT = iRateDetailCPT + 1.
              WHEN "diagnos":U THEN ASSIGN iRateDetailICD = iRateDetailICD + 1.
              WHEN "hacar":U   THEN ASSIGN iRateDetailFLG = iRateDetailFLG + 1.
              WHEN "hlmcr":U   THEN ASSIGN iRateDetailCRW = iRateDetailCRW + 1.
            END CASE. /* CASE buf_old_auth_rate_detail.owning_entity_mnemonic: */

            /* We use the detail service to comit the record to the database */
            RUN _saveAuthRateDetail IN TARGET-PROCEDURE.
          END. /* FOR EACH buf_old_auth_rate_detail NO-LOCK */ 
        END. /* lInformation */

        /* We will now cycle through all exclusion records that have been selected */
        IF lExclusion THEN
        DO iCnt = 1 TO NUM-ENTRIES(cOEMExclList, ","):
          FOR EACH buf_old_auth_rate_detail NO-LOCK
             WHERE buf_old_auth_rate_detail.auth_rate_control_obj  = tt_copy_record.old_auth_rate_control_obj
               AND buf_old_auth_rate_detail.owning_entity_mnemonic = ENTRY(iCnt,cOEMExclList)
               AND buf_old_auth_rate_detail.exclusion              = TRUE:

            CREATE tt_auth_rate_detail.
            BUFFER-COPY buf_old_auth_rate_detail
                 EXCEPT buf_old_auth_rate_detail.auth_rate_control_obj buf_old_auth_rate_detail.auth_rate_detail_obj
                     TO tt_auth_rate_detail.

            ASSIGN iCnt = 1 + iCnt
                   tt_auth_rate_detail.auth_rate_detail_obj  = iCnt * -2
                   tt_auth_rate_detail.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj
                   tt_auth_rate_detail.effective_date        = btt_auth_rate_control.effective_date
                   tt_auth_rate_detail.end_date              = btt_auth_rate_control.end_date
                   tt_auth_rate_detail.override_ars_rate     = IF btt_auth_rate_control.cpt_relative_value_type <> "":U 
                                                               AND tt_copy_record.override_ars_rate <> "":U AND buf_old_auth_rate_detail.override_ars_rate = "":U
                                                               THEN tt_copy_record.override_ars_rate
                                                               ELSE buf_old_auth_rate_detail.override_ars_rate

                   tt_auth_rate_detail.record_action         = "MODIFY":U.
            
            /* Focus the auth rate provider we have just assigned data to */
            FIND FIRST tt_auth_rate_detail NO-LOCK 
              NO-ERROR.
            {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

            CASE buf_old_auth_rate_detail.owning_entity_mnemonic:
              WHEN "hlmck":U   THEN ASSIGN iRateDetailCPTExcl = iRateDetailCPTExcl + 1.
              WHEN "diagnos":U THEN ASSIGN iRateDetailICDExcl = iRateDetailICDExcl + 1.
              WHEN "hacar":U   THEN ASSIGN iRateDetailFLGExcl = iRateDetailFLGExcl + 1.
              WHEN "hlmcr":U   THEN ASSIGN iRateDetailCRWExcl = iRateDetailCRWExcl + 1.
            END CASE. /* CASE buf_old_auth_rate_detail.owning_entity_mnemonic: */
            
            /* We use the detail service to comit the record to the database */
            RUN _saveAuthRateDetail IN TARGET-PROCEDURE.
          END. /* FOR EACH buf_old_auth_rate_provider NO-LOCK */
        END. /* lExclusion */  
      END. /* ELSE DO: */

      ASSIGN cMessage = SUBSTITUTE("Provider Details copied - &1, ":U, iRateProviders)
                      + SUBSTITUTE("Details copied: &1 ICD's, &2 CPT's, &3 Flags and &4 Crosswalks. ":U, iRateDetailICD,iRateDetailCPT,iRateDetailFLG,iRateDetailCRW)
                      + SUBSTITUTE("Exclusions copied: &1 ICD's, &2 CPT's, &3 Flags and &4 Crosswalks.":U, iRateDetailICDExcl,iRateDetailCPTExcl,iRateDetailFLGExcl,iRateDetailCRWExcl).
    
      oErrorObject:addError(INPUT "hacrc":U,                                       
                            INPUT btt_auth_rate_control.auth_rate_control_obj,   
                            INPUT "":U,                                          
                            INPUT "rate_control_code":U,
                            INPUT 1,             
                            INPUT cMessage,                                          
                            INPUT "WARN":U).

      IF btt_auth_rate_control.cpt_relative_value_type <> "":U AND tt_copy_record.override_ars_rate <> "":U
      THEN DO:
        ASSIGN cMessageOverrideARS = "Override ARS Rate: " + tt_copy_record.override_ars_rate + " has been copied to the Auth Rate Details.":U
                                   + " Please refine the Override ARS rates on the Auth Details lines.":U.

        oErrorObject:addError(INPUT "hacrc":U,                                       
                              INPUT btt_auth_rate_control.auth_rate_control_obj,   
                              INPUT "":U,                                          
                              INPUT "override_ars_rate":U,
                              INPUT 1,             
                              INPUT cMessageOverrideARS,                                          
                              INPUT "WARN":U).
      END. /* IF btt_auth_rate_control.cpt_relative_value_type <> "":U AND tt_copy_record.override_ars_rate <> "":U */

    END. /* IF AVAILABLE buf_auth_rate_control AND AVAILABLE tt_copy_record THEN */
    ELSE
    DO:
      /* We have this error handling in place in the event we dont have a valid rate control record back from the UI */
      oErrorObject:addError(INPUT 'hacrc',                                       
                            INPUT btt_auth_rate_control.auth_rate_control_obj,   
                            INPUT "":U,                                          
                            INPUT btt_auth_rate_control.line_number,             
                            INPUT "No previous rate control record is available":U,                                          
                            INPUT "ERR":U).                                      
    END. /* ELSE DO: */

    
  END. /*IF btt_auth_rate_control.record_action = "MODIFY":U THEN*/

  { mip/inc/mipcatcherror.i }
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fetchAuthRateDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthRateDataSet Procedure 
PROCEDURE fetchAuthRateDataSet :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Auth Rate Data
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
    Author    : Laetitia

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
  DEFINE VARIABLE hAuthRateControl           AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthRateDetail            AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthRateProvider          AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthRateError             AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthRateResult            AS HANDLE               NO-UNDO.
  DEFINE VARIABLE oSearchObject              AS cls.maauthratesearch NO-UNDO.

  DEFINE DATA-SOURCE srcAuthRateControl  FOR hac_auth_rate_control.
  DEFINE DATA-SOURCE srcAuthRateDetail   FOR hac_auth_rate_detail.
  DEFINE DATA-SOURCE srcAuthRateProvider FOR hac_auth_rate_provider.

  /*copy the Criteria Table from the input parameter table handle*/
  TEMP-TABLE ttDsCriteria:handle:COPY-TEMP-TABLE ( iophFilterCriteriaTableHandle,false , ? , ?  , ?).

  ASSIGN
    oSearchObject                 = NEW cls.maauthratesearch(INPUT DATASET-HANDLE iophDatasetHandle)
    hFilterCriteriaTableHandle    = iophFilterCriteriaTableHandle
    hDatasetHandle                = iophDatasetHandle.

  ASSIGN                           
    hAuthRateControl               = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rate_control":U)
    hAuthRateDetail                = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rate_detail":U)
    hAuthRateProvider              = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rate_provider":U)
    hAuthRateError                 = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rate_error":U)
    hAuthRateResult                = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rate_result":U).

  /*find criteria set for child table tt_auth_rate_detail*/
  FIND FIRST ttDsCriteria NO-LOCK 
       WHERE  ttDsCriteria.cCriteriaType = "BatchSize":U
         AND (ttDsCriteria.cBufferName   = "tt_auth_rate_detail":U
           OR ttDsCriteria.cBufferName   = "tt_auth_rate_provider":U)
    NO-ERROR.

  /**/
  IF AVAILABLE ttDsCriteria THEN
  DO: 
    IF ttDsCriteria.cBufferName = "tt_auth_rate_detail":U
    THEN DO: 
      ASSIGN 
        oSearchObject:AuthRateDetailsBatchSize = INTEGER(ttDsCriteria.vCharacter)
        hAuthRateDetail:BATCH-SIZE             = oSearchObject:AuthRateDetailsBatchSize 
       NO-ERROR.
    END. /* IF ttDsCriteria.cBufferName = "tt_auth_rate_detail":U */
    ELSE DO:
      ASSIGN 
        oSearchObject:AuthRateProviderBatchSize = INTEGER(ttDsCriteria.vCharacter)
        hAuthRateProvider:BATCH-SIZE            = oSearchObject:AuthRateProviderBatchSize 
       NO-ERROR.
    END. /* ELSE DO: */
  END. /* IF AVAILABLE ttDsCriteria THEN */
  
  ASSIGN                                 
    lSuccess                       = hAuthRateControl :ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthRateControl :HANDLE)
    lSuccess                       = hAuthRateDetail  :ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthRateDetail  :HANDLE)
    lSuccess                       = hAuthRateProvider:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthRateProvider:HANDLE)
    hAuthRateError:FILL-MODE       = "NO-FILL":U
    hAuthRateResult:FILL-MODE      = "NO-FILL":U.
 
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

&IF DEFINED(EXCLUDE-saveAuthRate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthRate Procedure 
PROCEDURE saveAuthRate :
/*------------------------------------------------------------------------------
  Purpose   : Save Auth Rate from dataset
  Parameters: dsAuthRateControl as defined in maauthratecontrolds.i
  Notes     : - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
  Author    : Laetitia
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthRateControl.

&IF {&DBDFMA} >= 10195 &THEN

  RUN _saveAuthRateControl  IN TARGET-PROCEDURE.

  RUN _saveAuthRateDetail   IN TARGET-PROCEDURE.
  
  RUN _saveAuthRateProvider IN TARGET-PROCEDURE.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    FIND FIRST tt_auth_rate_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE tt_auth_rate_result
    THEN 
      CREATE tt_auth_rate_result.

    FOR EACH tt_auth_rate_error NO-LOCK:
      ASSIGN 
        tt_auth_rate_result.number_of_errors = tt_auth_rate_result.number_of_errors + 1.
    END. /* FOR EACH tt_auth_rate_error NO-LOCK:  */
  
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthRateControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthRateControl Procedure 
PROCEDURE _deleteAuthRateControl :
/*------------------------------------------------------------------------------
  Purpose   : Remove Auth rate control record
  Parameters: Auth rate control obj
  Notes     : - This will run the procedures to remove any
                dependency records as well.
              - Delta 010195
              - MIP catch error handling used { mip/inc/mipcatcherror.i }
  Author    : Laetitia

------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthRateControlObj   AS DECIMAL            NO-UNDO.

  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject  NO-UNDO.
  DEFINE VARIABLE lFailureOccurred        AS LOGICAL            NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_rate_result   FOR tt_auth_rate_result.
  DEFINE BUFFER btt_auth_rate_error    FOR tt_auth_rate_error.
  DEFINE BUFFER buf_auth_rate_control  FOR hac_auth_rate_control.
  DEFINE BUFFER buf_auth_rate_detail   FOR hac_auth_rate_detail.
  DEFINE BUFFER buf_auth_rate_provider FOR hac_auth_rate_provider.
  
  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    FIND FIRST btt_auth_rate_result EXCLUSIVE-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_rate_result
    THEN 
      CREATE btt_auth_rate_result.

    /* 
       Make sure no dependencies remain 
    */
    
    IF CAN-FIND(FIRST buf_auth_rate_detail NO-LOCK
                WHERE buf_auth_rate_detail.auth_rate_control_obj = ipdAuthRateControlObj)
    THEN DO:            
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT ipdAuthRateControlObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                            INPUT "Auth Rate Control,Auth Rate Detail":U).
    END. /* IF CAN-FIND(FIRST buf_auth_rate_detail NO-LOCK */
    
    IF CAN-FIND(FIRST buf_auth_rate_provider NO-LOCK
                WHERE buf_auth_rate_provider.auth_rate_control_obj = ipdAuthRateControlObj)
    THEN DO:            
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError
        ( INPUT "hacrc":U, 
          INPUT ipdAuthRateControlObj,
          INPUT "":U,
          INPUT 1,
          INPUT "MA":U,
          INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
          INPUT "Auth Rate Control,Auth Rate Provider":U ).
    END. /* IF CAN-FIND(FIRST buf_auth_rate_provider NO-LOCK */

    IF NOT lFailureOccurred THEN
    DO:
      FIND FIRST buf_auth_rate_control EXCLUSIVE-LOCK
           WHERE buf_auth_rate_control.auth_rate_control_obj = ipdAuthRateControlObj
        NO-ERROR NO-WAIT.
     
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
     
      /*Unable to remove - record is already locked*/
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        cls.miperror:resetError().
     
        ASSIGN lFailureOccurred = TRUE.
     
        oErrorObject:addError(INPUT "hacrc":U,
                              INPUT ipdAuthRateControlObj,
                              INPUT "":U,
                              INPUT 1,
                              INPUT "MA":U,
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                              INPUT "Auth Rate Control":U).
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
     
      IF AVAILABLE buf_auth_rate_control AND NOT lFailureOccurred THEN
      DO:
        DELETE buf_auth_rate_control.
     
        ASSIGN btt_auth_rate_result.records_removed = btt_auth_rate_result.records_removed + 1.
     
      END. /* IF AVAILABLE buf_auth_rate_control AND NOT lFailureOccurred THEN  */
    END. /* IF NOT lFailureOccurred THEN */
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

&ENDIF

  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthRateDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthRateDetail Procedure 
PROCEDURE _deleteAuthRateDetail :
/*------------------------------------------------------------------------------
  Purpose    : Remove Auth Rate Detail record     
  Parameters : Auth Rate Detail Obj
  Notes      : - This will run the procedures to remove any
                 dependency records as well.
               - Delta 010195
               - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author     : Mandlam      
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthRateDetailObj AS DECIMAL           NO-UNDO.

  DEFINE VARIABLE oErrorObject                 AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred             AS LOGICAL           NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_rate_result     FOR tt_auth_rate_result.
  DEFINE BUFFER btt_auth_rate_error      FOR tt_auth_rate_error.
  DEFINE BUFFER buf_auth_rate_detail     FOR hac_auth_rate_detail.

  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).
    
    FIND FIRST btt_auth_rate_result EXCLUSIVE-LOCK NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_rate_result 
    THEN
      CREATE btt_auth_rate_result.
      
    FIND FIRST buf_auth_rate_detail EXCLUSIVE-LOCK
         WHERE buf_auth_rate_detail.auth_rate_detail_obj = ipdAuthRateDetailObj
      NO-ERROR NO-WAIT.   
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
    /*Unable to remove - record is already locked*/  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().
    
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacrd":U, 
                            INPUT ipdAuthRateDetailObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth Rate Detail":U ). 
    END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN */
    
    IF AVAILABLE buf_auth_rate_detail AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_rate_detail.
      ASSIGN btt_auth_rate_result.records_removed = btt_auth_rate_result.records_removed + 1.
    END. /* IF AVAILABLE buf_auth_rate_detail AND NOT lFailureOccurred THEN */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
 

  { mip/inc/mipcatcherror.i 
    &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
&ENDIF


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthRateProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthRateProvider Procedure 
PROCEDURE _deleteAuthRateProvider :
/*------------------------------------------------------------------------------
  Purpose    : Remove Auth Rate Provider record     
  Parameters : Auth Rate Provider Obj
  Notes      : - This will run the procedures to remove any
                 dependency records as well.
               - Delta 010195
               - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author     : MMP      
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthRateProviderObj AS DECIMAL           NO-UNDO.

  DEFINE VARIABLE oErrorObject                  AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lFailureOccurred              AS LOGICAL           NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE BUFFER btt_auth_rate_result     FOR tt_auth_rate_result.
  DEFINE BUFFER btt_auth_rate_error      FOR tt_auth_rate_error.
  DEFINE BUFFER buf_auth_rate_provider   FOR hac_auth_rate_provider.

  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    FIND FIRST btt_auth_rate_result EXCLUSIVE-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_rate_result
    THEN
      CREATE btt_auth_rate_result.

    FIND FIRST buf_auth_rate_provider EXCLUSIVE-LOCK
         WHERE buf_auth_rate_provider.auth_rate_provider_obj = ipdAuthRateProviderObj
      NO-ERROR NO-WAIT.
     { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }

    /*Unable to remove - record is already locked*/  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().

      ASSIGN lFailureOccurred = TRUE.

      oErrorObject:addError
        ( INPUT "hacrp":U, 
          INPUT ipdAuthRateProviderObj,
          INPUT "":U,
          INPUT 1,
          INPUT "MA":U,
          INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
          INPUT "Auth Rate Provider":U ).
    END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN */

    IF AVAILABLE buf_auth_rate_provider AND NOT lFailureOccurred THEN
    DO:
      DELETE buf_auth_rate_provider.
      ASSIGN btt_auth_rate_result.records_removed = btt_auth_rate_result.records_removed + 1.
    END. /* IF AVAILABLE buf_auth_rate_provider AND NOT lFailureOccurred THEN */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */


  { mip/inc/mipcatcherror.i 
    &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}
&ENDIF


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthRateControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthRateControl Procedure 
PROCEDURE _saveAuthRateControl :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete Auth rate control information
  Parameters:
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }
  Author    : Laetitia
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN
  
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE cErrorMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.
  
  DEFINE BUFFER buf_auth_rate_control FOR hac_auth_rate_control.
  DEFINE BUFFER btt_auth_rate_control FOR tt_auth_rate_control.
  DEFINE BUFFER btt_auth_rate_result  FOR tt_auth_rate_result.
  DEFINE BUFFER btt_auth_rate_error   FOR tt_auth_rate_error.
  DEFINE BUFFER btt_auth_rate_detail  FOR tt_auth_rate_detail.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
    ASSIGN oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    FIND FIRST btt_auth_rate_result EXCLUSIVE-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE btt_auth_rate_result
    THEN 
      CREATE btt_auth_rate_result.

    RECORD-BLK:
    FOR EACH btt_auth_rate_control EXCLUSIVE-LOCK
          BY btt_auth_rate_control.auth_rate_control_obj DESCENDING:

      ASSIGN
         lFailureOccurred = FALSE
         btt_auth_rate_result.records_processed = btt_auth_rate_result.records_processed + 1.

      IF btt_auth_rate_control.record_action = "MODIFY":U THEN
      DO:
        /* Auth Rate Control Validations */
        RUN _validateAuthRateControl IN TARGET-PROCEDURE (BUFFER btt_auth_rate_control, INPUT-OUTPUT lFailureOccurred ).

        /*Duplicate check*/
        FIND FIRST buf_auth_rate_control NO-LOCK
             WHERE buf_auth_rate_control.insurer_obj             = btt_auth_rate_control.insurer_obj
               AND buf_auth_rate_control.option_code             = btt_auth_rate_control.option_code
               AND buf_auth_rate_control.main_provider_neg_num   = btt_auth_rate_control.main_provider_neg_num
               AND buf_auth_rate_control.main_provider_base_rate = btt_auth_rate_control.main_provider_base_rate
               AND buf_auth_rate_control.main_provider_ars_rate  = btt_auth_rate_control.main_provider_ars_rate
               AND buf_auth_rate_control.workgroup_obj           = btt_auth_rate_control.workgroup_obj
               AND buf_auth_rate_control.associated_pr_type_list = btt_auth_rate_control.associated_pr_type_list
               AND buf_auth_rate_control.effective_date          = btt_auth_rate_control.effective_date
          NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE} 

        ASSIGN cErrorMessage = "Auth rate control,":U 
                                    + "Option: ":U                 + STRING(btt_auth_rate_control.option_code)            + CHR(10)            
                                    + "Main Prov Neg: ":U          + STRING(btt_auth_rate_control.main_provider_neg_num)  + CHR(10) 
                                    + "Main Prov Base: ":U         + btt_auth_rate_control.main_provider_base_rate        + CHR(10) 
                                    + "Main Prov ARS: ":U          + btt_auth_rate_control.main_provider_ars_rate         + CHR(10) 
                                    + "Effective Date: ":U         + STRING(btt_auth_rate_control.effective_date)         + CHR(10) 
                                    + "Workgroup Obj:  ":U         + STRING(btt_auth_rate_control.workgroup_obj)          + CHR(10) 
                                    + "Associated Disciplines: ":U + btt_auth_rate_control.associated_pr_type_list.

        IF NOT AVAILABLE buf_auth_rate_control THEN
        DO:
          IF btt_auth_rate_control.end_date <> ? THEN
          DO:
            FIND FIRST buf_auth_rate_control NO-LOCK
               WHERE buf_auth_rate_control.insurer_obj             = btt_auth_rate_control.insurer_obj
                 AND buf_auth_rate_control.option_code             = btt_auth_rate_control.option_code
                 AND buf_auth_rate_control.main_provider_neg_num   = btt_auth_rate_control.main_provider_neg_num
                 AND buf_auth_rate_control.main_provider_base_rate = btt_auth_rate_control.main_provider_base_rate
                 AND buf_auth_rate_control.main_provider_ars_rate  = btt_auth_rate_control.main_provider_ars_rate
                 AND buf_auth_rate_control.workgroup_obj           = btt_auth_rate_control.workgroup_obj
                 AND buf_auth_rate_control.associated_pr_type_list = btt_auth_rate_control.associated_pr_type_list
                 AND buf_auth_rate_control.end_date                = btt_auth_rate_control.end_date
            NO-ERROR.

           {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE} 

           ASSIGN cErrorMessage = "Auth rate control,":U 
                                    + "Option: ":U                 + STRING(btt_auth_rate_control.option_code)            + CHR(10) 
                                    + "Main Prov Neg: ":U          + STRING(btt_auth_rate_control.main_provider_neg_num)  + CHR(10) 
                                    + "Main Prov Base: ":U         + btt_auth_rate_control.main_provider_base_rate        + CHR(10) 
                                    + "Main Prov ARS: ":U          + btt_auth_rate_control.main_provider_ars_rate         + CHR(10) 
                                    + "End Date: ":U               + STRING(btt_auth_rate_control.end_date)               + CHR(10) 
                                    + "Workgroup Obj:  ":U         + STRING(btt_auth_rate_control.workgroup_obj)          + CHR(10) 
                                    + "Associated Disciplines: ":U + btt_auth_rate_control.associated_pr_type_list.
          END. /* IF btt_auth_rate_control.end_date <> ? THEN */
          
          IF NOT AVAILABLE buf_auth_rate_control THEN
          DO: 
            FIND FIRST buf_auth_rate_control NO-LOCK
                 WHERE buf_auth_rate_control.rate_control_code     = btt_auth_rate_control.rate_control_code 
              NO-ERROR.
             {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE} 
          
            ASSIGN cErrorMessage = "Auth rate control,":U 
                                    + "Rate Control Code: ":U  + STRING(btt_auth_rate_control.rate_control_code)  + CHR(10).

          END. /*  IF NOT AVAILABLE buf_auth_rate_control */

        END. /*  IF NOT AVAILABLE buf_auth_rate_control */

        IF AVAILABLE buf_auth_rate_control 
        AND buf_auth_rate_control.auth_rate_control_obj  <> btt_auth_rate_control.auth_rate_control_obj 
        THEN
        DO: 
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hacrc":U,
                                INPUT btt_auth_rate_control.auth_rate_control_obj,
                                INPUT "":U,
                                INPUT btt_auth_rate_control.line_number,
                                INPUT "MA":U,
                                INPUT 125,  /* &1 already exists with &2 */
                                INPUT cErrorMessage).
        END. /* IF CAN-FIND(FIRST buf_auth_rate_control NO-LOCK */
        ELSE IF AVAILABLE buf_auth_rate_control 
        THEN 
          RUN _setAuthRateControlDates IN TARGET-PROCEDURE( BUFFER btt_auth_rate_control,
                                                            BUFFER buf_auth_rate_control,
                                                            INPUT-OUTPUT oErrorObject,
                                                            INPUT-OUTPUT lFailureOccurred).      
        IF lFailureOccurred
        THEN
          NEXT RECORD-BLK.

        FIND FIRST buf_auth_rate_control EXCLUSIVE-LOCK
             WHERE buf_auth_rate_control.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj
          NO-ERROR NO-WAIT.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}

        /*The auth rate control record is locked by another user or process*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().

          ASSIGN
             lFailureOccurred = TRUE
             btt_auth_rate_result.records_locked = btt_auth_rate_result.records_locked + 1.

          oErrorObject:addError(INPUT "hacrc":U, 
                                INPUT btt_auth_rate_control.auth_rate_control_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_rate_control.line_number, 
                                INPUT "MA":U, 
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth Rate Control: ":U + STRING(btt_auth_rate_control.rate_control_code)).

        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
        
        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().

          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_rate_control.
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
        
        IF AVAILABLE buf_auth_rate_control AND NOT lFailureOccurred THEN
        DO:
          /*An existing record is being updated*/
          IF btt_auth_rate_control.auth_rate_control_obj <= 0
          THEN ASSIGN btt_auth_rate_result.records_created  = btt_auth_rate_result.records_created  + 1.
          ELSE ASSIGN btt_auth_rate_result.records_modified = btt_auth_rate_result.records_modified + 1.
        
          BUFFER-COPY btt_auth_rate_control
               EXCEPT btt_auth_rate_control.auth_rate_control_obj
                   TO buf_auth_rate_control.

          /*If we are creating a new record we need to make sure that we run through all the dependants
            and assign the correct _obj*/
          IF btt_auth_rate_control.auth_rate_control_obj < 1 THEN
          DO:
            FOR EACH btt_auth_rate_detail EXCLUSIVE-LOCK
               WHERE btt_auth_rate_detail.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj:
              ASSIGN btt_auth_rate_detail.auth_rate_control_obj = buf_auth_rate_control.auth_rate_control_obj.
            END. /*FOR EACH btt_auth_rate_detail*/
          END. /*IF btt_auth_rate_control.auth_rate_control_obj < 1 THEN*/

          ASSIGN
             btt_auth_rate_control.auth_rate_control_obj = buf_auth_rate_control.auth_rate_control_obj
             btt_auth_rate_control.record_action = "":U.

          VALIDATE buf_auth_rate_control.

          FIND CURRENT buf_auth_rate_control NO-LOCK.
        END. /*IF AVAILABLE buf_auth_rate_control AND NOT lFailureOccurred THEN*/
      END. /*IF btt_auth_rate_control.record_action = "MODIFY":U THEN*/
          
      IF btt_auth_rate_control.record_action = "DELETE":U THEN
      DO:
        /*This routine will ensure that all dependencies will also be removed*/
        RUN _deleteAuthRateControl IN TARGET-PROCEDURE (INPUT btt_auth_rate_control.auth_rate_control_obj).

        IF NOT CAN-FIND(FIRST btt_auth_rate_error NO-LOCK
                        WHERE btt_auth_rate_error.owning_entity_mnemonic = 'hacrc'
                          AND btt_auth_rate_error.owning_obj = btt_auth_rate_control.auth_rate_control_obj)
        THEN 
          DELETE btt_auth_rate_control.

      END. /*END. /*IF btt_auth_rate_control.record_action = "DELETE":U THEN*/*/    
    END. /*FOR EACH btt_auth_rate_control EXCLUSIVE-LOCK:*/
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

 { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject." }
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthRateDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthRateDetail Procedure 
PROCEDURE _saveAuthRateDetail :
/*------------------------------------------------------------------------------
  Purpose   : Create/Update/Delete Auth Rate detail Procedure    
  Parameters:  <none>
  Notes     : MIP catch error handling used { mip/inc/mipatcherror.i }
  Author    : Laetitia
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSucess              AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred     AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE iCount               AS INTEGER             NO-UNDO INITIAL 1. 
  
  DEFINE BUFFER buf_auth_rate_detail   FOR hac_auth_rate_detail.
  DEFINE BUFFER btt_auth_rate_detail   FOR tt_auth_rate_detail.
  DEFINE BUFFER btt_auth_rate_result   FOR tt_auth_rate_result.
  DEFINE BUFFER btt_auth_rate_error    FOR tt_auth_rate_error.
  DEFINE BUFFER tt_auth_rate_detail    FOR tt_auth_rate_detail.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    BLK-rate-detail:
    FOR EACH tt_auth_rate_detail NO-LOCK 
       WHERE tt_auth_rate_detail.record_action          = "Modify":U
         AND tt_auth_rate_detail.owning_entity_mnemonic = "diagnos":U
         AND tt_auth_rate_detail.owning_key            <> "":U
         AND tt_auth_rate_detail.apply_to_all           = YES :
      /*check if the tt_auth_rate_detail is duplicate in the database , if it is skip adding the range codes */
      IF CAN-FIND(FIRST buf_auth_rate_detail NO-LOCK
                  WHERE buf_auth_rate_detail.auth_rate_control_obj  = tt_auth_rate_detail.auth_rate_control_obj  
                  AND   buf_auth_rate_detail.owning_entity_mnemonic = tt_auth_rate_detail.owning_entity_mnemonic 
                  AND   buf_auth_rate_detail.owning_obj             =  0                                         
                  AND   buf_auth_rate_detail.owning_key             = tt_auth_rate_detail.owning_key                          
                  AND   buf_auth_rate_detail.effective_date         = tt_auth_rate_detail.effective_date) 
      THEN 
        NEXT BLK-rate-detail.
      ELSE
        BLK-Diagnos:
        FOR EACH diagnos NO-LOCK
           WHERE diagnos.diagnosis BEGINS SUBSTRING(tt_auth_rate_detail.owning_alt_value, 1, 3) :
         
          FIND FIRST btt_auth_rate_detail NO-LOCK
               WHERE btt_auth_rate_detail.auth_rate_control_obj  = tt_auth_rate_detail.auth_rate_control_obj
                 AND btt_auth_rate_detail.owning_entity_mnemonic = tt_auth_rate_detail.owning_entity_mnemonic
                 AND btt_auth_rate_detail.owning_obj             =  0 
                 AND btt_auth_rate_detail.owning_key             = diagnos.diagnosis
                 AND btt_auth_rate_detail.owning_alt_value       = diagnos.diagnosis
                 AND btt_auth_rate_detail.effective_date         = tt_auth_rate_detail.effective_date
            NO-ERROR. 

          /*check the icd being auto added if is a duplicate, if is then skip add*/
          IF CAN-FIND(FIRST buf_auth_rate_detail NO-LOCK
                      WHERE buf_auth_rate_detail.auth_rate_control_obj  = tt_auth_rate_detail.auth_rate_control_obj  
                      AND   buf_auth_rate_detail.owning_entity_mnemonic = tt_auth_rate_detail.owning_entity_mnemonic 
                      AND   buf_auth_rate_detail.owning_obj             =  0                                         
                      AND   buf_auth_rate_detail.owning_key             = diagnos.diagnosis                          
                      AND   buf_auth_rate_detail.effective_date         = tt_auth_rate_detail.effective_date) 
          THEN
            NEXT BLK-Diagnos.
            
          IF NOT AVAILABLE btt_auth_rate_detail THEN
          DO: 
            CREATE btt_auth_rate_detail.
            ASSIGN btt_auth_rate_detail.record_action           = "Modify":U          
                   btt_auth_rate_detail.line_number             = tt_auth_rate_detail.line_number  + iCount   
                   btt_auth_rate_detail.auth_rate_control_obj   = tt_auth_rate_detail.auth_rate_control_obj  
                   btt_auth_rate_detail.auth_rate_detail_obj    = iCount * -8888   
                   btt_auth_rate_detail.owning_entity_mnemonic  = tt_auth_rate_detail.owning_entity_mnemonic 
                   btt_auth_rate_detail.owning_obj              = 0             
                   btt_auth_rate_detail.owning_key              = diagnos.diagnosis            
                   btt_auth_rate_detail.owning_alt_value        = diagnos.diagnosis       
                   btt_auth_rate_detail.flag_value              = tt_auth_rate_detail.flag_value             
                   btt_auth_rate_detail.effective_date          = tt_auth_rate_detail.effective_date         
                   btt_auth_rate_detail.end_date                = tt_auth_rate_detail.end_date               
                   btt_auth_rate_detail.apply_to_all            = NO 
                   btt_auth_rate_detail.exclusion               = tt_auth_rate_detail.exclusion
                   iCount = iCount + 1.  
               
              RUN _validateAuthRateDetail IN TARGET-PROCEDURE(BUFFER btt_auth_rate_detail, INPUT-OUTPUT lFailureOccurred).
             
          END. /*IF NOT AVAILABLE btt_auth_rate_detail THEN DO:*/
          ELSE
            NEXT BLK-Diagnos.
        
          END. /*FOR EACH diagnos NO-LOCK*/
    END. /* FOR EACH tt_auth_rate_detail NO-LOCK */
    
    FIND FIRST btt_auth_rate_result EXCLUSIVE-LOCK NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_rate_result 
    THEN
      CREATE btt_auth_rate_result.
    
    Record-Blk:
    FOR EACH btt_auth_rate_detail EXCLUSIVE-LOCK
          BY btt_auth_rate_detail.auth_rate_detail_obj DESCENDING:
          
      /* No use to carry on if there's an error on the auth rate control header */
      IF oErrorObject:canFind("hacrc":U, btt_auth_rate_detail.auth_rate_control_obj, "":U) THEN
        NEXT Record-Blk.
        
      ASSIGN
        lFailureOccurred   = FALSE
        btt_auth_rate_result.records_processed = btt_auth_rate_result.records_processed + 1.
        
      IF btt_auth_rate_detail.record_action = "MODIFY":U THEN
      DO:
      
        /* Validate Current buffer before we save */
        RUN _validateAuthRateDetail IN TARGET-PROCEDURE(BUFFER btt_auth_rate_detail, INPUT-OUTPUT lFailureOccurred).
        
        /*Duplicate check*/
        FIND FIRST buf_auth_rate_detail NO-LOCK
             WHERE buf_auth_rate_detail.auth_rate_control_obj  = btt_auth_rate_detail.auth_rate_control_obj
               AND buf_auth_rate_detail.owning_entity_mnemonic = btt_auth_rate_detail.owning_entity_mnemonic
               AND buf_auth_rate_detail.owning_obj             = btt_auth_rate_detail.owning_obj
               AND buf_auth_rate_detail.owning_key             = btt_auth_rate_detail.owning_key
               AND buf_auth_rate_detail.effective_date         = btt_auth_rate_detail.effective_date
          NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE buf_auth_rate_detail THEN
          FIND FIRST buf_auth_rate_detail NO-LOCK
               WHERE buf_auth_rate_detail.auth_rate_control_obj  = btt_auth_rate_detail.auth_rate_control_obj
                 AND buf_auth_rate_detail.owning_entity_mnemonic = btt_auth_rate_detail.owning_entity_mnemonic
                 AND buf_auth_rate_detail.owning_obj             = btt_auth_rate_detail.owning_obj
                 AND buf_auth_rate_detail.owning_key             = btt_auth_rate_detail.owning_key
                 AND buf_auth_rate_detail.end_date               = btt_auth_rate_detail.end_date
            NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
        

        IF AVAILABLE buf_auth_rate_detail 
        AND buf_auth_rate_detail.auth_rate_detail_obj  <> btt_auth_rate_detail.auth_rate_detail_obj THEN /*check the detail obj */
        DO: 

          mipEnv:miDBEntity:focusTable(btt_auth_rate_detail.owning_entity_mnemonic).

          IF mipEnv:miDBEntity:Infocus THEN
          DO:
           /* IF mipEnv:miDBEntity:MainIndexType = "OBJ":U
            THEN mipEnv:miDBEntity:findRecord(btt_auth_rate_detail.owning_obj) .
            ELSE mipEnv:miDBentity:findRecord(btt_auth_rate_detail.owning_key) . */
          END. /* IF mipEnv:miDBEntity:Infocus */
         

          ASSIGN lFailureOccurred = TRUE.
          
          oErrorObject:addError(INPUT "hacrd":U, 
                                INPUT btt_auth_rate_detail.auth_rate_detail_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_rate_detail.line_number, 
                                INPUT "MA":U, 
                                INPUT 125,  /* &1 already exists with &2 */
                                INPUT "Auth rate detail,":U 
                                    + "Owning Entity: ":U   + (IF mipEnv:miDBEntity:InFocus 
                                                               THEN mipEnv:miDBEntity:TableName 
                                                               ELSE btt_auth_rate_detail.owning_entity_mnemonic)     + CHR(10)
                                    + (IF NOT mipEnv:miDBEntity:RecordAvailable 
                                       THEN "Owning Obj: ":U +  STRING(btt_auth_rate_detail.owning_obj)              + CHR(10)
                                       ELSE "":U ) 
                                    + "Owning Code: ":U     +  (IF mipEnv:miDBEntity:RecordAvailable 
                                                                THEN mipEnv:miDBEntity:RecordCode
                                                                ELSE btt_auth_rate_detail.owning_key )                + CHR(10)
                                    + "Effective Date: ":U  +  STRING(btt_auth_rate_detail.effective_date,"9999/99/99")
                                    + (IF btt_auth_rate_detail.end_date <> ? 
                                       THEN "End Date: ":U + STRING(btt_auth_rate_detail.end_date)
                                       ELSE "":U ) ).

          BUFFER buf_auth_rate_detail:HANDLE:BUFFER-RELEASE(). 
        END. /* IF CAN-FIND(FIRST buf_auth_rate_detail NO-LOCK */
        
        IF lFailureOccurred
        THEN
          NEXT Record-Blk.

        FIND FIRST buf_auth_rate_detail EXCLUSIVE-LOCK
             WHERE buf_auth_rate_detail.auth_rate_detail_obj = btt_auth_rate_detail.auth_rate_detail_obj
          NO-ERROR NO-WAIT.
          
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /* The auth rate control is locked by another user or process */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN
            lFailureOccurred = TRUE.
            
            btt_auth_rate_result.records_locked = btt_auth_rate_result.records_locked + 1.
            
            oErrorObject:addError(INPUT "hacrd":U, 
                                  INPUT btt_auth_rate_detail.auth_rate_detail_obj, 
                                  INPUT "":U, 
                                  INPUT btt_auth_rate_detail.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                  INPUT "Auth Rate Detail :":U + STRING(btt_auth_rate_detail.auth_rate_detail_obj)).
        
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */
        
        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN
        DO:
          cls.miperror:resetError().
          
          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_rate_detail.
          
        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN */
      
        IF AVAILABLE buf_auth_rate_detail AND NOT lFailureOccurred THEN
        DO:
          /* An Existing Record is being updated */
          IF btt_auth_rate_detail.auth_rate_detail_obj <= 0 
          THEN ASSIGN btt_auth_rate_result.records_created  = btt_auth_rate_result.records_created  + 1.
          ELSE ASSIGN btt_auth_rate_result.records_modified = btt_auth_rate_result.records_modified + 1.
          
          BUFFER-COPY btt_auth_rate_detail
               EXCEPT btt_auth_rate_detail.auth_rate_detail_obj
                   TO buf_auth_rate_detail.
                   
          ASSIGN
            btt_auth_rate_detail.auth_rate_detail_obj = buf_auth_rate_detail.auth_rate_detail_obj
            btt_auth_rate_detail.record_action        = "":U.
            
          VALIDATE buf_auth_rate_detail.
          
          FIND CURRENT buf_auth_rate_detail NO-LOCK.           
        END. /* IF AVAILABLE buf_auth_rate_detail AND NOT lFailureOccurred THEN */
      END. /* IF btt_auth_rate_detail.record_action = "MODIFY":U  */
        
      IF btt_auth_rate_detail.record_action = "DELETE":U THEN
      DO:
        /*This routine will ensure that all dependencies will also be removed*/
        RUN _deleteAuthRateDetail IN TARGET-PROCEDURE (INPUT btt_auth_rate_detail.auth_rate_detail_obj).

        IF NOT CAN-FIND(FIRST btt_auth_rate_error NO-LOCK
                        WHERE btt_auth_rate_error.owning_entity_mnemonic = "hacrd":U
                          AND btt_auth_rate_error.owning_obj = btt_auth_rate_detail.auth_rate_detail_obj) 
        THEN 
          DELETE btt_auth_rate_detail.
      END. /* IF btt_auth_rate_detail.record_action = "DELETE":U THEN */
    END. /* FOR EACH btt_auth_rate_detail NO-LOCK */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */
  
  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthRateProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthRateProvider Procedure 
PROCEDURE _saveAuthRateProvider :
/*------------------------------------------------------------------------------
  Purpose   : Create/Update/Delete Auth Rate Provider Procedure    
  Parameters:  <none>
  Notes     : MIP catch error handling used { mip/inc/mipatcherror.i }
  Author    : MMP
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE lSucess              AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFailureOccurred     AS LOGICAL             NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE oErrorObject         AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE iCount               AS INTEGER             NO-UNDO INITIAL 1. 

  DEFINE BUFFER buf_auth_rate_provider   FOR hac_auth_rate_provider.
  DEFINE BUFFER btt_auth_rate_provider   FOR tt_auth_rate_provider.
  DEFINE BUFFER btt_auth_rate_result     FOR tt_auth_rate_result.
  DEFINE BUFFER btt_auth_rate_error      FOR tt_auth_rate_error.
  DEFINE BUFFER tt_auth_rate_provider    FOR tt_auth_rate_provider.

  DO TRANSACTION ON ERROR UNDO, THROW:

    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    FIND FIRST btt_auth_rate_result EXCLUSIVE-LOCK NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth_rate_result 
    THEN
      CREATE btt_auth_rate_result.

    Record-Blk:
    FOR EACH btt_auth_rate_provider EXCLUSIVE-LOCK
          BY btt_auth_rate_provider.auth_rate_provider_obj DESCENDING:

      /* No use to carry on if there's an error on the auth rate control header */
      IF oErrorObject:canFind("hacrc":U, btt_auth_rate_provider.auth_rate_control_obj, "":U) 
      THEN
        NEXT Record-Blk.

      ASSIGN
        lFailureOccurred   = FALSE
        btt_auth_rate_result.records_processed = btt_auth_rate_result.records_processed + 1.

      IF btt_auth_rate_provider.record_action = "MODIFY":U 
      THEN
      DO:
        IF btt_auth_rate_provider.auth_group_obj <> 0 THEN
        DO:
          IF NOT CAN-FIND(FIRST ham_auth_group
                          WHERE ham_auth_group.auth_group_obj = btt_auth_rate_provider.auth_group_obj) THEN
          DO:
            oErrorObject:addError(INPUT "hacrp":U,
                                  INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                                  INPUT "":U,
                                  INPUT "auth_group_obj":U,
                                  INPUT btt_auth_rate_provider.line_number,
                                  INPUT "MA":U,
                                  INPUT 100,  /* The "&1" specified is invalid */
                                  INPUT "Auth Group":U).
          END.  /* IF NOT CAN-FIND(FIRST ham_auth_group */
        END.  /* IF btt_auth_rate_provider.auth_group_obj <> 0 THEN */

        /* Validate Current buffer before we save */
        RUN _validateAuthRateProvider IN TARGET-PROCEDURE(BUFFER btt_auth_rate_provider, INPUT-OUTPUT lFailureOccurred).

        /*Duplicate check*/
        FIND FIRST buf_auth_rate_provider NO-LOCK
             WHERE buf_auth_rate_provider.auth_rate_control_obj    = btt_auth_rate_provider.auth_rate_control_obj
               AND buf_auth_rate_provider.auth_rate_indicator      = btt_auth_rate_provider.auth_rate_indicator
               AND buf_auth_rate_provider.auth_rate_provider_type  = btt_auth_rate_provider.auth_rate_provider_type
               AND buf_auth_rate_provider.auth_rate_pr_type        = btt_auth_rate_provider.auth_rate_pr_type
               AND buf_auth_rate_provider.auth_rate_sub_pr_type    = btt_auth_rate_provider.auth_rate_sub_pr_type
               AND buf_auth_rate_provider.effective_date           = btt_auth_rate_provider.effective_date
          NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE buf_auth_rate_provider THEN
          FIND FIRST buf_auth_rate_provider NO-LOCK
               WHERE buf_auth_rate_provider.auth_rate_control_obj    = btt_auth_rate_provider.auth_rate_control_obj
                 AND buf_auth_rate_provider.auth_rate_indicator      = btt_auth_rate_provider.auth_rate_indicator
                 AND buf_auth_rate_provider.auth_rate_provider_type  = btt_auth_rate_provider.auth_rate_provider_type
                 AND buf_auth_rate_provider.auth_rate_pr_type        = btt_auth_rate_provider.auth_rate_pr_type
                 AND buf_auth_rate_provider.auth_rate_sub_pr_type    = btt_auth_rate_provider.auth_rate_sub_pr_type
                 AND buf_auth_rate_provider.end_date                 = btt_auth_rate_provider.end_date
            NO-ERROR. 
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
       
        IF AVAILABLE buf_auth_rate_provider 
        AND buf_auth_rate_provider.auth_rate_provider_obj  <> btt_auth_rate_provider.auth_rate_provider_obj
        THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError
            ( INPUT "hacrp":U,
              INPUT btt_auth_rate_provider.auth_rate_provider_obj,
              INPUT "":U,
              INPUT btt_auth_rate_provider.line_number,
              INPUT "MA":U,
              INPUT 125,  /* &1 already exists with &2 */
              INPUT "Auth rate provider,":U
                  + "Auth Rate Obj: ":U             + STRING(btt_auth_rate_provider.auth_rate_control_obj)          + CHR(10)
                  + "Auth Rate Indicator: ":U       +        btt_auth_rate_provider.auth_rate_indicator             + CHR(10)
                  + "Auth Rate Provider Type: ":U   + STRING(btt_auth_rate_provider.auth_rate_provider_type)        + CHR(10)
                  + "Auth Rate Discipline Type: ":U + STRING(btt_auth_rate_provider.auth_rate_pr_type)              + CHR(10)
                  + "Effective Date: ":U            + STRING(btt_auth_rate_provider.effective_date, "9999/99/99":U) + CHR(10)
                  + IF btt_auth_rate_provider.end_date <> ? THEN
                      "End Date: ":U + STRING(btt_auth_rate_provider.end_date,"9999/99/99":U)
                    ELSE "":U).
        END. /* IF CAN-FIND(FIRST buf_auth_rate_provider NO-LOCK */

        IF lFailureOccurred
        THEN
          NEXT Record-Blk.

        FIND FIRST buf_auth_rate_provider EXCLUSIVE-LOCK
             WHERE buf_auth_rate_provider.auth_rate_provider_obj = btt_auth_rate_provider.auth_rate_provider_obj
          NO-ERROR NO-WAIT.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}

        /* The auth rate control is locked by another user or process */
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U 
        THEN
        DO:
          cls.miperror:resetError().

          ASSIGN
            lFailureOccurred                    = TRUE
            btt_auth_rate_result.records_locked = btt_auth_rate_result.records_locked + 1.

            oErrorObject:addError
              ( INPUT "hacrp":U,
                INPUT btt_auth_rate_provider.auth_rate_provider_obj,
                INPUT "":U,
                INPUT btt_auth_rate_provider.line_number,
                INPUT "MA":U,
                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                INPUT "Auth Rate provider :":U + STRING(btt_auth_rate_provider.auth_rate_provider_obj) ).

        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U THEN */
        
        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U 
        THEN
        DO:
          cls.miperror:resetError().

          IF NOT lFailureOccurred
          THEN
            CREATE buf_auth_rate_provider.

        END. /* IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U THEN */

        IF AVAILABLE buf_auth_rate_provider AND NOT lFailureOccurred 
        THEN
        DO:
          /* An Existing Record is being updated */
          IF btt_auth_rate_provider.auth_rate_provider_obj <= 0 
          THEN ASSIGN btt_auth_rate_result.records_created  = btt_auth_rate_result.records_created  + 1.
          ELSE ASSIGN btt_auth_rate_result.records_modified = btt_auth_rate_result.records_modified + 1.

          BUFFER-COPY btt_auth_rate_provider
               EXCEPT btt_auth_rate_provider.auth_rate_provider_obj
                   TO buf_auth_rate_provider.

          ASSIGN
            btt_auth_rate_provider.auth_rate_provider_obj = buf_auth_rate_provider.auth_rate_provider_obj
            btt_auth_rate_provider.record_action          = "":U.

          VALIDATE buf_auth_rate_provider.

          FIND CURRENT buf_auth_rate_provider NO-LOCK.
        END. /* IF AVAILABLE buf_auth_rate_provider AND NOT lFailureOccurred THEN */
      END. /* IF btt_auth_rate_provider.record_action = "MODIFY":U  */

      IF btt_auth_rate_provider.record_action = "DELETE":U 
      THEN
      DO:
        /*This routine will ensure that all dependencies will also be removed*/
        RUN _deleteAuthRateProvider IN TARGET-PROCEDURE (INPUT btt_auth_rate_provider.auth_rate_provider_obj).

        IF NOT CAN-FIND(FIRST btt_auth_rate_error NO-LOCK
                        WHERE btt_auth_rate_error.owning_entity_mnemonic = "hacrp":U
                          AND btt_auth_rate_error.owning_obj             = btt_auth_rate_provider.auth_rate_provider_obj) 
        THEN
          DELETE btt_auth_rate_provider.
      END. /* IF btt_auth_rate_provider.record_action = "DELETE":U THEN */
    END. /* FOR EACH btt_auth_rate_provider NO-LOCK */
  END. /* DO TRANSACTION ON ERROR UNDO, THROW: */

  { mip/inc/mipcatcherror.i
     &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_setAuthRateControlDates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _setAuthRateControlDates Procedure 
PROCEDURE _setAuthRateControlDates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters: 
  Notes:       
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE PARAMETER BUFFER btt_auth_rate_control FOR tt_auth_rate_control.
  DEFINE PARAMETER BUFFER buf_auth_rate_control FOR hac_auth_rate_control.
  DEFINE INPUT-OUTPUT PARAMETER ipoErrorObject      AS cls.maerrorobject NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ioplFailureOccurred AS LOGICAL           NO-UNDO.

  DEFINE VARIABLE lDateChangeApplied   AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL NO-UNDO.
  DEFINE VARIABLE dOldEffectiveDate    AS DATE    NO-UNDO.
  DEFINE VARIABLE dOldEndDate          AS DATE    NO-UNDO.

  DEFINE BUFFER buf_auth_rate_detail   FOR hac_auth_rate_detail.
  DEFINE BUFFER buf_auth_rate_provider FOR hac_auth_rate_provider.

  IF ioplFailureOccurred 
  THEN
    RETURN.
    
  ASSIGN dOldEffectiveDate = buf_auth_rate_control.effective_date
         dOldEndDate       = buf_auth_rate_control.end_date.

  FOR EACH buf_auth_rate_provider EXCLUSIVE-LOCK 
     WHERE buf_auth_rate_provider.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj:
             
    IF (buf_auth_rate_provider.effective_date = dOldEffectiveDate AND buf_auth_rate_provider.effective_date <> btt_auth_rate_control.effective_date ) 
    OR (buf_auth_rate_provider.end_date       = dOldEndDate       AND buf_auth_rate_provider.end_date       <> btt_auth_rate_control.end_date ) 
    AND NOT ioplFailureOccurred
    THEN
      ASSIGN lDateChangeApplied                    = TRUE 
             buf_auth_rate_provider.effective_date = btt_auth_rate_control.effective_date
             buf_auth_rate_provider.end_date       = btt_auth_rate_control.end_date.
             
    IF buf_auth_rate_provider.effective_date < btt_auth_rate_control.effective_date 
    THEN 
      ASSIGN ioplFailureOccurred = TRUE 
             lSuccess            = ipoErrorObject:addError(INPUT "hacrc",                                                                                                                                                   /* ipcOwningEntityMnemonic  */
                                                           INPUT btt_auth_rate_control.auth_rate_control_obj,                                                                                                               /* ipdOwningEntityObj       */
                                                           INPUT "":U,                                                                                                                                                      /* ipcOwningEntityKey       */
                                                           INPUT btt_auth_rate_control.line_number,                                                                                                                         /* ipiLineNumber            */
                                                           INPUT SUBSTITUTE("Cannot Change effective date to &1 , Auth rate provider setup exits outisde of date period":U, STRING(btt_auth_rate_control.effective_date)) , /* ipcMessageText           */
                                                           INPUT "ERR":U,                                                                                                                                                   /* ipcMessageType           */
                                                           INPUT FALSE).
                              
                              
    IF buf_auth_rate_provider.end_date < btt_auth_rate_control.end_date 
    THEN 
      ASSIGN ioplFailureOccurred = TRUE 
             lSuccess            = ipoErrorObject:addError(INPUT "hacrc",                                                                                                                                       /* ipcOwningEntityMnemonic  */
                                                           INPUT btt_auth_rate_control.auth_rate_control_obj,                                                                                                   /* ipdOwningEntityObj       */
                                                           INPUT "":U,                                                                                                                                          /* ipcOwningEntityKey       */
                                                           INPUT btt_auth_rate_control.line_number,                                                                                                             /* ipiLineNumber            */
                                                           INPUT SUBSTITUTE("Cannot Change end date to &1 , Auth rate provider setup exits outisde of date period":U, STRING(btt_auth_rate_control.end_date)) , /* ipcMessageText           */
                                                           INPUT "ERR":U,                                                                                                                                       /* ipcMessageType           */
                                                           INPUT FALSE). 
      

  END. /* FOR EACH buf_auth_rate_provider EXCLUSIVE-LOCK  */

  FOR EACH buf_auth_rate_detail EXCLUSIVE-LOCK 
     WHERE buf_auth_rate_detail.auth_rate_control_obj = btt_auth_rate_control.auth_rate_control_obj:

    IF (buf_auth_rate_detail.effective_date = dOldEffectiveDate AND buf_auth_rate_detail.effective_date <> btt_auth_rate_control.effective_date ) 
    OR (buf_auth_rate_detail.end_date       = dOldEndDate       AND buf_auth_rate_detail.end_date       <> btt_auth_rate_control.end_date ) 
    THEN
    ASSIGN lDateChangeApplied                  = TRUE
           buf_auth_rate_detail.effective_date = btt_auth_rate_control.effective_date
           buf_auth_rate_detail.end_date       = btt_auth_rate_control.end_date . 
           
    IF buf_auth_rate_detail.effective_date < btt_auth_rate_control.effective_date 
    THEN 
      ASSIGN ioplFailureOccurred = TRUE 
             lSuccess            = ipoErrorObject:addError(INPUT "hacrc",                                                                                                                                                   /* ipcOwningEntityMnemonic  */
                                                           INPUT btt_auth_rate_control.auth_rate_control_obj,                                                                                                               /* ipdOwningEntityObj       */
                                                           INPUT "":U,                                                                                                                                                      /* ipcOwningEntityKey       */
                                                           INPUT btt_auth_rate_control.line_number,                                                                                                                         /* ipiLineNumber            */
                                                           INPUT SUBSTITUTE("Cannot Change effective date to &1 , Auth rate detail setup exits outisde of date period":U, STRING(btt_auth_rate_control.effective_date) ) ,  /* ipcMessageText           */
                                                           INPUT "ERR":U,                                                                                                                                                   /* ipcMessageType           */
                                                           INPUT FALSE).
                              
                              
    IF buf_auth_rate_detail.end_date < btt_auth_rate_control.end_date 
    THEN 
      ASSIGN ioplFailureOccurred = TRUE 
             lSuccess            = ipoErrorObject:addError(INPUT "hacrc",                                                                                                                                       /* ipcOwningEntityMnemonic  */
                                                           INPUT btt_auth_rate_control.auth_rate_control_obj,                                                                                                   /* ipdOwningEntityObj       */
                                                           INPUT "":U,                                                                                                                                          /* ipcOwningEntityKey       */
                                                           INPUT btt_auth_rate_control.line_number,                                                                                                             /* ipiLineNumber            */
                                                           INPUT SUBSTITUTE("Cannot Change end date to &1 , Auth rate detail setup exits outisde of date period":U, STRING(btt_auth_rate_control.end_date))  ,  /* ipcMessageText           */
                                                           INPUT "ERR":U,                                                                                                                                       /* ipcMessageType           */
                                                           INPUT FALSE). 
                                                          
           
  END. /* FOR EACH buf_auth_rate_detail EXCLUSIVE-LOCK */

  IF lDateChangeApplied 
  THEN
      ipoErrorObject:addError(INPUT "hacrc",                                                                                   /* ipcOwningEntityMnemonic  */
                              INPUT btt_auth_rate_control.auth_rate_control_obj,                                               /* ipdOwningEntityObj       */
                              INPUT "":U,                                                                                      /* ipcOwningEntityKey       */
                              INPUT btt_auth_rate_control.line_number,                                                         /* ipiLineNumber            */
                              INPUT "Auth Rate Provider/Detail effective/end dates were updated according to header dates.":U, /* ipcMessageText           */
                              INPUT "WAR":U,                                                                                   /* ipcMessageType           */
                              INPUT FALSE).                                                                                    /* iplAcknowledge           */

&ENDIF
  {mip/inc/mipcatcherror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthRateControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthRateControl Procedure 
PROCEDURE _validateAuthRateControl :
/*------------------------------------------------------------------------------
  Purpose    : Auth Rate Control Validation     
  Parameters : Current Buffer to be validated
  Notes      : 
  Author     : Laetitia     
------------------------------------------------------------------------------*/

  { ma/app/maauthrateservalcontrol.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthRateDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthRateDetail Procedure 
PROCEDURE _validateAuthRateDetail :
/*----------------------------------------------------------------------------
  Purpose    : Auth Rate Detail validation    
  Parameters : Current Buffer to be validated
  Notes      :
  ----------------------------------------------------------------------------*/
  
  { ma/app/maauthrateservaldetail.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_validateAuthRateProvider) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _validateAuthRateProvider Procedure 
PROCEDURE _validateAuthRateProvider :
/*----------------------------------------------------------------------------
  Purpose    : Auth Rate Detail validation
  Parameters : Current Buffer to be validated
  Notes      : 
  ----------------------------------------------------------------------------*/

  { ma/app/maauthrateservalprovider.i }

END PROCEDURE.  /* _validateAuthRateProvider */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

