CLASS ma.cls.basemaauthratesearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include for Auth Rate */
  { ma/inc/maauthratecontrolds.i }
  
  DEFINE PUBLIC PROPERTY AuthRateDetailsBatchSize   AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthRateProviderBatchSize  AS INTEGER NO-UNDO GET. SET.
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthratesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(DATASET dsAuthRateControl:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratesearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthratesearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(iphDataset).    

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratesearch */
  
  
  /* -- METHOD: fetchData ----------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchData
    ():
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN    

    IF NOT VALID-OBJECT(mipEnv:Health)                 THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
    IF NOT VALID-OBJECT(mipEnv:Health:AuthMaintenance) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Auth type data service not started'"}.
    
    IF NOT datasetComplete()
    THEN
    DO:
      /*create a ttDsCriteria record if the AuthRateDetailsBatchSize was assigned a value*/
      IF AuthRateDetailsBatchSize <> 0 THEN DO:
        CREATE ttDsCriteria NO-ERROR.

        IF NOT {&ErrorStatus}
        THEN
          ASSIGN
              ttDsCriteria.cCriteriaType = "BatchSize":U
              ttDsCriteria.cBufferName   = "tt_auth_rate_detail":U
              ttDsCriteria.cFieldName    = "":U
              ttDsCriteria.cOperator     = "":U
              ttDsCriteria.cDataType     = "character":U
              ttDsCriteria.vCharacter    = STRING(AuthRateDetailsBatchSize)
          NO-ERROR.
      END. /* IF AuthRateDetailsBatchSize <> 0 THEN DO: */

      /* create a ttDsCriteria record if the AuthRateProviderBatchSize was assigned a value */
      IF AuthRateProviderBatchSize <> 0 THEN DO:
        CREATE ttDsCriteria NO-ERROR.

        IF NOT {&ErrorStatus}
        THEN
          ASSIGN
              ttDsCriteria.cCriteriaType = "BatchSize":U
              ttDsCriteria.cBufferName   = "tt_auth_rate_provider":U
              ttDsCriteria.cFieldName    = "":U
              ttDsCriteria.cOperator     = "":U
              ttDsCriteria.cDataType     = "character":U
              ttDsCriteria.vCharacter    = STRING(AuthRateProviderBatchSize)
          NO-ERROR.
      END. /* IF AuthRateDetailsBatchSize <> 0 THEN DO: */

      mipEnv:Health:AuthMaintenance:fetchAuthRateDataSet(INPUT CAST(THIS-OBJECT,"cls.maauthratesearch":U)).
    END. /* IF NOT datasetComplete()*/

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */
  
END CLASS. /* CLASS ma.cls.basemaauthratesearch INHERITS cls.mipdataretrieval */

