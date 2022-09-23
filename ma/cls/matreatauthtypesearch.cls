CLASS ma.cls.matreatauthtypesearch INHERITS cls.mipdataretrieval:
  
  {mip/inc/mipdefsharedclass.i}
  
  { ma/inc/matreatauthtypeds.i }
  
  DEFINE PUBLIC PROPERTY UserDefinedFilter AS mip.userdefinedfilter NO-UNDO GET. PROTECTED SET.

  DEFINE PRIVATE VARIABLE glInternallyCreatedUserDefinedFilter AS LOGICAL NO-UNDO INITIAL FALSE.

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC matreatauthtypesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/

    THIS-OBJECT(DATASET dsTreatAuthType:HANDLE, ?).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC matreatauthtypesearch */
  
  CONSTRUCTOR PUBLIC matreatauthtypesearch
    (INPUT iphTreatAuthTypeDataset AS HANDLE):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/

    THIS-OBJECT(iphTreatAuthTypeDataset, ?).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC matreatauthtypesearch */
  
  CONSTRUCTOR PUBLIC matreatauthtypesearch
    (INPUT iphTreatAuthTypeDataset AS HANDLE,
     INPUT ipoUserDefinedFilter    AS mip.userdefinedfilter):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
  
    SUPER(iphTreatAuthTypeDataset).
    
    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC matreatauthtypesearch */
  
  /* -- DESTRUCTOR -------------------------------------------------- */
  DESTRUCTOR PUBLIC matreatauthtypesearch
    ():
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  --------------------------------------------------------------------*/

    /* We will only cleanup objects that we create */
    IF glInternallyCreatedUserDefinedFilter AND VALID-OBJECT(UserDefinedFilter) THEN DELETE OBJECT UserDefinedFilter.
  
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC matreatauthtypesearch */


  /* -- METHOD: fetchTreatAuthTypeData -------------------------------*/
  METHOD PUBLIC LOGICAL fetchTreatAuthTypeData
    ():
  /*------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------*/

    IF NOT VALID-OBJECT(mipEnv:Health)             THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
    IF NOT VALID-OBJECT(mipEnv:Health:maCondition) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Condition data service not started'"}.

    IF NOT datasetComplete() THEN
    DO:
      IF glInternallyCreatedUserDefinedFilter THEN
        UserDefinedFilter:applyUserDefinedFilters(THIS-OBJECT).

      mipEnv:Health:maCondition:fetchTreatAuthTypeDataset(INPUT THIS-OBJECT).
    END. /*IF NOT datasetComplete() THEN*/

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchTreatAuthTypeData */
  

END CLASS. /* CLASS ma.cls.matreatauthtypesearch INHERITS cls.mipdataretrieval */
