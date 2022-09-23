CLASS ma.cls.basemaauthtypesearch INHERITS cls.mipdataretrieval:
  
  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  /* Dataset include for tt_auth_type */
  { ma/inc/maauthtypeds.i }
  
  
   /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthtypesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    
    SUPER(DATASET dsAuthType:HANDLE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypesearch */ 
  
  CONSTRUCTOR PUBLIC basemaauthtypesearch
    (INPUT DATASET-HANDLE iphDataset):
    /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
  
    SUPER(iphDataset).    

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypesearch */
  
  
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
      mipEnv:Health:AuthMaintenance:fetchAuthTypeDataset(INPUT CAST(THIS-OBJECT,"cls.maauthtypesearch":U)).
    END.

&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchData */
  
  /* -- METHOD: fetchAuthType ------------------------------------------------- */
  METHOD PUBLIC LOGICAL fetchAuthType
    (INPUT ipdAuthTypeObj   AS DECIMAL,
     INPUT ipdInsurerObj    AS DECIMAL,
     INPUT ipiOptionCode    AS INTEGER,
     INPUT ipdEffectiveDate AS DATE,
     INPUT ipcProviderType  AS CHARACTER,
     INPUT ipcIndicatorType AS CHARACTER
     ):
  /*---------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------- */

    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    
&IF {&DBDFMA} >= 010195 &THEN    

    ASSIGN lSuccess = setFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, ipdAuthTypeObj).
    
    IF ipdInsurerObj > 0 THEN
      ASSIGN lSuccess = setCriteria("InsurerObj":U, STRING(ipdInsurerObj)).

    IF ipiOptionCode <> 0 THEN
      ASSIGN lSuccess = setCriteria("OptionCode":U, STRING(ipiOptionCode)).

    IF ipdEffectiveDate <> ? THEN
      ASSIGN lSuccess = setCriteria("EffectiveDate":U, STRING(ipdEffectiveDate)).

    IF ipcProviderType <> "":U 
    AND ipcProviderType <> ? THEN
      ASSIGN lSuccess = setCriteria("ProviderType":U, STRING(ipcProviderType)).
    
    IF  ipcIndicatorType <> "":U
    AND ipcIndicatorType <> ?  THEN 
      ASSIGN lSuccess = setCriteria("IndicatorType":U, STRING(ipcIndicatorType)).

    fetchData().
   
&ENDIF

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchAuthType */

END CLASS. /* CLASS ma.cls.basemaauthtypesearch INHERITS cls.mipdataretrieval */

