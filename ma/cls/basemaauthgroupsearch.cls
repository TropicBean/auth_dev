
CLASS ma.cls.basemaauthgroupsearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include for tt_auth_group */
  { ma/inc/maauthgroupds.i }
  
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthgroupsearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    
    SUPER(DATASET dsAuthGroup:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthgroupsearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthgroupsearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
  
    SUPER(iphDataset).    

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthgroupsearch */
  
  
  /* -- METHOD: fetchData ----------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchData
    ():
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN    

    IF NOT VALID-OBJECT(mipEnv:Health)                 THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
    IF NOT VALID-OBJECT(mipEnv:Health:AuthMaintenance) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Auth  group data service not started'"}.
    
    IF NOT datasetComplete()
    THEN
    DO:      
      mipEnv:Health:AuthMaintenance:fetchAuthGroupDataset(INPUT CAST(THIS-OBJECT,"cls.maauthgroupsearch":U)).
    END.

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */

END CLASS. /* CLASS ma.cls.basemaauthgroupsearch INHERITS cls.mipdataretrieval */


