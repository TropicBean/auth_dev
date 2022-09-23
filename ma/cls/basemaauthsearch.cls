CLASS ma.cls.basemaauthsearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include*/
  { ma/inc/maauthds.i }
  
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthsearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    
    SUPER(DATASET dsAuthorisation:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthsearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthsearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
  
    SUPER(iphDataset).    

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthsearch */
  
  
  /* -- METHOD: fetchData ----------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchData
    ():
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN    

    IF NOT VALID-OBJECT(mipEnv:Health)                THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
    IF NOT VALID-OBJECT(mipEnv:Health:AuthDataAccess) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Authorisation data service not started'"}.
    
    IF NOT datasetComplete()
    THEN
    DO:     
      mipEnv:Health:AuthDataAccess:fetchAuthorisationDataset(INPUT CAST(THIS-OBJECT,"cls.maauthsearch":U)).             
    END.

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */
  
  /* -- METHOD: fetchData ----------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchData
    (ipcBufferList AS CHARACTER):
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */

    SetCriteria("BufferList":U, ipcBufferList).
    
    fetchData().

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */

END CLASS. /* CLASS ma.cls.basemaauthsearch INHERITS cls.mipdataretrieval */


