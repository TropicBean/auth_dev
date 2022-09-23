CLASS ma.cls.basemaauthcopaysearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include for Auth Copay */
  { ma/inc/maauthcopayds.i }
  
  DEFINE PUBLIC PROPERTY AuthCopayDetailsBatchSize   AS INTEGER NO-UNDO GET. SET.
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaysearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(DATASET dsAuthCopayControl:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaysearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthcopaysearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(iphDataset).    

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaysearch */
  
  
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
      /* Create a ttDsCriteria record if the AuthCopayDetailsBatchSize was assigned a value */
      IF AuthCopayDetailsBatchSize <> 0 THEN DO:
        CREATE ttDsCriteria NO-ERROR.

        IF NOT {&ErrorStatus}
        THEN
          ASSIGN
              ttDsCriteria.cCriteriaType = "BatchSize":U
              ttDsCriteria.cBufferName   = "tt_auth_copay_detail":U
              ttDsCriteria.cFieldName    = "":U
              ttDsCriteria.cOperator     = "":U
              ttDsCriteria.cDataType     = "character":U
              ttDsCriteria.vCharacter    = STRING(AuthCopayDetailsBatchSize)
          NO-ERROR.
      END. /* IF AuthCopayDetailsBatchSize <> 0 THEN DO: */

      mipEnv:Health:AuthMaintenance:fetchAuthCopayDataSet(INPUT CAST(THIS-OBJECT,"cls.maauthcopaysearch":U)).
    END. /* IF NOT datasetComplete()*/

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */
  
END CLASS. /* CLASS ma.cls.basemaauthcopaysearch INHERITS cls.mipdataretrieval */


