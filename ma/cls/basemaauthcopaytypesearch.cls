/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: GrahamW

      Purpose: Base Auth Copay Type Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthcopaytypesearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include for tt_auth_copay_type */
  { ma/inc/maauthcopaytypeds.i }
  
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaytypesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    
    SUPER(DATASET dsAuthCopayType:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaytypesearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthcopaytypesearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
  
    SUPER(iphDataset).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaytypesearch */
  
  
  /* -- METHOD: fetchData ----------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchData
    ():
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN    

    IF NOT VALID-OBJECT(mipEnv:Health)                 THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
    IF NOT VALID-OBJECT(mipEnv:Health:AuthMaintenance) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Adjustment Type data service not started'"}.
    
    IF NOT datasetComplete()
    THEN
    DO:      
      mipEnv:Health:AuthMaintenance:fetchAuthCopayTypeDataset(INPUT CAST(THIS-OBJECT,"cls.maauthcopaytypesearch":U)).
    END.

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */

END CLASS. /* CLASS ma.cls.basemaauthcopaytypesearch INHERITS cls.mipdataretrieval */


