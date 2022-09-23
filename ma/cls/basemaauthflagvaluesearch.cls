
CLASS ma.cls.basemaauthflagvaluesearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include*/
  { ma/inc/maauthflagvalueds.i }
  
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthflagvaluesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    
    SUPER(DATASET dsAuthFlagValue:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthflagvaluesearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthflagvaluesearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
  
    SUPER(iphDataset).    

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthflagvaluesearch */
  
  
  /* -- METHOD: fetchData ----------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchData
    ():
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN    

    IF NOT VALID-OBJECT(mipEnv:Health)                   THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
    IF NOT VALID-OBJECT(mipEnv:Health:AuthBusinessLogic) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Authorisation service not started'"}.
    
    IF NOT datasetComplete()
    THEN
    DO:     
      mipEnv:Health:AuthBusinessLogic:fetchAuthFlagValueDataset(INPUT CAST(THIS-OBJECT,"cls.maauthflagvaluesearch":U)).
    END.

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */
  
END CLASS. /* CLASS ma.cls.basemaauthflagvaluesearch INHERITS cls.mipdataretrieval */



