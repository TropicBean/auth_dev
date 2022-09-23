CLASS ma.cls.basemaauthrulesearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include for tt_auth_rule */
  { ma/inc/maauthruleds.i }
  
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthrulesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    
    SUPER(DATASET dsAuthRule:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemipmmsearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthrulesearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
  
    SUPER(iphDataset).    

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthrulesearch */
  
  
  /* -- METHOD: fetchData ----------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchData
    ():
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN

    DEFINE VARIABLE cWhatToGet AS CHARACTER NO-UNDO.

    IF NOT VALID-OBJECT(mipEnv:Health)                 THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Healthcare application not started.'"}.  
    IF NOT VALID-OBJECT(mipEnv:Health:AuthMaintenance) THEN RETURN ERROR {mip/inc/miperrortext.i "'MA'" 0 "'Auth Rule data service not started'" }.
    
    IF NOT datasetComplete()
    THEN DO:
      getCriteria( INPUT "BufferList":U, OUTPUT cWhatToGet ).

      IF cWhatToGet = "":U OR cWhatToGet = ?
      THEN
        ASSIGN cWhatToGet = "*":U.

      mipEnv:Health:AuthMaintenance:fetchAuthRuleDataset
        ( INPUT-OUTPUT   TABLE-HANDLE CriteriaTableHandle,
          INPUT-OUTPUT DATASET-HANDLE DatasetHandle,
          INPUT        cWhatToGet ).
    END. /* IF NOT datasetComplete() */

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */

END CLASS. /* CLASS ma.cls.basemaauthrulesearch INHERITS cls.mipdataretrieval */


