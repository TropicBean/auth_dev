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

   /*  
       !Do not uncomment until we have managed to set up healthcare services
       IF NOT VALID-OBJECT(mipEnv:Health)                THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
       IF NOT VALID-OBJECT(mipEnv:Health:AuthDataAccess) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Authorisation data service not started'"}.
    */ 

    IF NOT datasetComplete()
    THEN
    DO:      
      /*
         ! Do not uncomment until we have done the following: 
         TODO: Get healthcare data service up and running so that we can use this method 
         - We'll need to set up auth tables before we can do this 
         Rather than calling the service here , we're just going to populate the auth dataset from a hard set json file
         mipEnv:Health:AuthDataAccess:fetchAuthorisationDataset(INPUT CAST(THIS-OBJECT,"cls.maauthsearch":U)).*/ 
      
      DatasetHandle:READ-JSON("FILE", "C:\localenv\000mip\trn\hotfix\db\json\authds.json") .

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


