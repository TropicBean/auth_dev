/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Matthews

      Purpose: Base TrfcostAuth Class
------------------------------------------------------------------------*/

CLASS ma.cls.basematrfcostauth INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/matariffds.i }

  DEFINE PUBLIC   PROPERTY TrfcostAuthInFocus   AS LOGICAL            NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC   PROPERTY TariffCode           AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY EffectDate           AS DATE               NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY BaseRate             AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY ARSRate              AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY PrType               AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY AuthOnUsageNum       AS INTEGER            NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY LevelOfAlert         AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY LevelDefaultNote     AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY LevelMessage         AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY RecordAction         AS CHARACTER          NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY LineNumber           AS INTEGER            NO-UNDO GET. SET.
  DEFINE PUBLIC   PROPERTY ErrorObject          AS cls.maerrorobject  NO-UNDO GET. PRIVATE SET.

  DEFINE PRIVATE  VARIABLE glSuccess            AS LOGICAL            NO-UNDO.
  DEFINE PRIVATE  VARIABLE ghTariffDataset      AS HANDLE             NO-UNDO.

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basematrfcostauth() :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basematrfcostauth */

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basematrfcostauth
    ( INPUT ipcTariffCode AS CHARACTER,
      INPUT ipdEffectDate AS DATE,
      INPUT ipcPrType     AS CHARACTER,
      INPUT ipcBaseRate   AS CHARACTER,
      INPUT ipcARSRate    AS CHARACTER) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusTrfcostAuth(ipcTariffCode,
                                 ipdEffectDate,
                                 ipcPrType,
                                 ipcBaseRate,
                                 ipcARSRate).

    { mip/inc/mipcatcherror.i }
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basematrfcostauth */
  
                                                           
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basematrfcostauth() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
    
    IF VALID-HANDLE( ghTariffDataset  ) THEN DELETE OBJECT ghTariffDataset.
    IF VALID-OBJECT( ErrorObject )      THEN DELETE OBJECT ErrorObject.
   
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basematrfcostauth */ 

  /* -- METHOD focusTrfcostAuth ------------------------------------------ */
  METHOD PUBLIC LOGICAL focusTrfcostAuth
    ( INPUT ipcTariffCode AS CHARACTER,
      INPUT ipdEffectDate AS DATE,
      INPUT ipcPrType     AS CHARACTER,
      INPUT ipcBaseRate   AS CHARACTER,
      INPUT ipcARSRate    AS CHARACTER) :
  /*------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */

    ASSIGN
      glSuccess           = unfocusTrfcostAuth().

    ASSIGN
      TariffCode          = ipcTariffCode
      EffectDate          = ipdEffectDate
      PrType              = ipcPrType
      BaseRate            = ipcBaseRate
      ArsRate             = ipcARSRate

      glSuccess           = getTrfcostAuthDetails()
        
      TrfcostAuthInFocus  = glSuccess.
        
    RETURN TrfcostAuthInFocus.

    { mip/inc/mipcatcherror.i }
  END METHOD.  /* focusTrfcostAuth  */

/* -- METHOD getTrfcostAuthDetails ------------------------------------- */
  METHOD PRIVATE LOGICAL getTrfcostAuthDetails
    (  ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject   AS ma.cls.matariffsearch NO-UNDO.

    DATASET dsTariff:EMPTY-DATASET().

    /* when linked tables are added to the dataset, here is where you can switch Data Relations if necessary */
    ASSIGN
      oSearchObject   = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE).

    IF TariffCode <> "" AND EffectDate <> ? AND PrType <> "":U AND BaseRate <> "":U THEN
    DO:

      ASSIGN 
        glSuccess = oSearchObject:SetFilterCriteria("tt_trfcostauth.tariff-code":U, "=":U, TariffCode)
        glSuccess = oSearchObject:SetFilterCriteria("tt_trfcostauth.effect-date":U, "=":U, EffectDate)
        glSuccess = oSearchObject:SetFilterCriteria("tt_trfcostauth.pr-type":U,     "=":U, PrType)
        glSuccess = oSearchObject:SetFilterCriteria("tt_trfcostauth.base-rate":U,   "=":U, BaseRate)
        glSuccess = oSearchObject:SetFilterCriteria("tt_trfcostauth.ars-rate":U,    "=":U, ARSRate).

      oSearchObject:fetchTariffData().
    END.

    FOR FIRST tt_trfcostauth NO-LOCK:
    
      ASSIGN
        TariffCode          = tt_trfcostauth.tariff-code
        PrType              = tt_trfcostauth.pr-type
        EffectDate          = tt_trfcostauth.effect-date
        BaseRate            = tt_trfcostauth.base-rate
        ARSRate             = tt_trfcostauth.ars-rate
        AuthOnUsageNum      = tt_trfcostauth.auth-on-usage-num
        LevelOfAlert        = tt_trfcostauth.level-of-alert
        LevelDefaultNote    = tt_trfcostauth.level-default-note
        LevelMessage        = tt_trfcostauth.level-message
        .

    END. /* FOR FIRST tt_trfcostauth NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_trfcostauth NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}
  END METHOD. /* getTrfcostAuthDetails */

  /* We never want a method to return void because then we cannot run it from within an assign statement */
  METHOD PRIVATE LOGICAL initializeObject
  (   ):
  /*--------------------------------------------------------------------
        Purpose: initializes out trfcostauth object
          Notes:
  ---------------------------------------------------------------------- */

    ASSIGN 
      ghTariffDataset = DATASET dsTariff:HANDLE

      ErrorObject     = NEW cls.maerrorobject(TEMP-TABLE tt_tariff_error:HANDLE).

    { mip/inc/mipcatcherror.i }
  END METHOD.  /* initializeObject  */

  /* -- METHOD deleteTrfcostAuth ------------------------------------- */
  METHOD PUBLIC LOGICAL deleteTrfcostAuth () :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused trfcost record.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE cErrorMessage     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iError            AS INTEGER    NO-UNDO.

    DEFINE BUFFER btt_trfcostauth FOR tt_trfcostauth.
    
    IF NOT TrfcostAuthInFocus THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'trfcostauth not in focus'"}

    FIND FIRST btt_trfcostauth EXCLUSIVE-LOCK
         WHERE btt_trfcostauth.tariff-code  = TariffCode
           AND btt_trfcostauth.effect-date  = EffectDate
           AND btt_trfcostauth.pr-type      = PrType
           AND btt_trfcostauth.base-rate    = BaseRate
           AND btt_trfcostauth.ars-rate     = ARSRate.

    ASSIGN
      btt_trfcostauth.record_action = "DELETE":U.
      
    VALIDATE btt_trfcostauth.

    mipEnv:Health:maMedical:saveTrfcostAuth(INPUT-OUTPUT DATASET dsTariff BY-REFERENCE).

    IF VALID-OBJECT(ErrorObject) AND ErrorObject:ErrorsExist THEN
    DO:
      ErrorObject:MoveFirst().

      DO iError = 1 TO ErrorObject:numberOfErrors:
        ASSIGN 
          cErrorMessage = cErrorMessage + CHR(10)
                        + "Line ":U + STRING(ErrorObject:LineNumber) 
                        + " - ":U + ErrorObject:ErrorText
                        + " ( ":U + ErrorObject:ErrorGroup + " - ":U + STRING(ErrorObject:ErrorNumber) + " )":U.

        ErrorObject:MoveNext().
      END. /*DO iError = 1 */
      UNDO, THROW NEW mip.apperror( cErrorMessage ).
    END. /* ErrorObject:ErrorsExist */ 

    RETURN FALSE.

    { mip/inc/mipcatcherror.i }
  END METHOD.  /* deleteTrfcostAuth  */

  /* -- METHOD saveTrfcostAuth ------------------------------------------------- */
  METHOD PUBLIC LOGICAL saveTrfcostAuth() :
  /*--------------------------------------------------------------------
        Purpose:  
          Notes:
  ---------------------------------------------------------------------- */

    DATASET dsTariff:EMPTY-DATASET().

    CREATE tt_trfcostauth.

    ASSIGN
      tt_trfcostauth.record_action      = IF RecordAction = "" THEN "MODIFY" ELSE RecordAction
      tt_trfcostauth.line_number        = LineNumber
      tt_trfcostauth.tariff-code        = TariffCode
      tt_trfcostauth.pr-type            = PrType
      tt_trfcostauth.effect-date        = EffectDate
      tt_trfcostauth.base-rate          = BaseRate
      tt_trfcostauth.ars-rate           = ARSRate
      tt_trfcostauth.auth-on-usage-num  = AuthOnUsageNum    
      tt_trfcostauth.level-of-alert     = LevelOfAlert      
      tt_trfcostauth.level-default-note = LevelDefaultNote
      tt_trfcostauth.level-message      = LevelMessage      
       .

    VALIDATE tt_trfcostauth.

    mipEnv:Health:maMedical:saveTrfcostAuth(INPUT-OUTPUT DATASET dsTariff BY-REFERENCE).

    /* refocus fields */
    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_trfcostauth NO-LOCK:
      
        ASSIGN
          TariffCode  = tt_trfcostauth.tariff-code
          EffectDate  = tt_trfcostauth.effect-date
          PrType      = tt_trfcostauth.pr-type
          BaseRate    = tt_trfcostauth.base-rate
          ARSRate     = tt_trfcostauth.ars-rate

          glSuccess = THIS-OBJECT:focusTrfcostAuth(TariffCode,
                                                   EffectDate,
                                                   PrType,
                                                   BaseRate,
                                                   ARSRate).
      END.
    END. /* IF NOT ErrorObject:ErrorsExist THEN */

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
  END METHOD. /* saveTrfcostAuth */

  /* -- METHOD unfocusTrfcostAuth ---------------------------------------- */
  METHOD PUBLIC LOGICAL unfocusTrfcostAuth() :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */

    DATASET dsTariff:EMPTY-DATASET().
    
    ErrorObject:unfocusError().
    
    ASSIGN 
      TrfcostAuthInFocus  = FALSE
      TariffCode          = "":U
      PrType              = "":U
      BaseRate            = "":U
      ARSRate             = "":U
      EffectDate          = ?
      RecordAction        = "":U
      LineNumber          = 0
      AuthOnUsageNum      = 0
      LevelOfAlert        = "":U
      LevelDefaultNote    = "":U
      LevelMessage        = "":U
      .

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}
  END METHOD. /* unfocusTrfcostAuth */

END CLASS. /* mip.cls.basematrfcostauth */

