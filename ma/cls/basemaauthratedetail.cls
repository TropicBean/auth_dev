/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Laetitia

      Purpose: Auth Rate Detail Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthratedetail INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthratecontrolds.i }
                                                  
                                                  
  DEFINE PRIVATE VARIABLE glSuccess                          AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthRateDetailDataset            AS HANDLE  NO-UNDO.
                                                             
  DEFINE PUBLIC PROPERTY AuthRateDetailInFocus               AS LOGICAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthRateDetailObj                   AS DECIMAL           GET. SET.                                                            
  DEFINE PUBLIC PROPERTY LineNumber                          AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY OverrideArsRate                     AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OwningKey                           AS CHARACTER         GET. SET.    
  DEFINE PUBLIC PROPERTY OwningAltValue                      AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY OwningObj                           AS DECIMAL           GET. SET.       
  DEFINE PUBLIC PROPERTY EffectiveDate                       AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                             AS DATE              GET. SET. 
  DEFINE PUBLIC PROPERTY FlagValue                           AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY AuthRateControlObj                  AS DECIMAL           GET. SET.   
  DEFINE PUBLIC PROPERTY ApplyToAll                          AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY Exclusion                           AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ErrorObject                         AS cls.maerrorobject GET. PRIVATE SET.
    

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthratedetail() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratedetail */


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthratedetail
    ( INPUT ipdAuthRateDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    THIS-OBJECT:focusAuthRateDetail(ipdAuthRateDetailObj).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratedetail */


  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthratedetail() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthRateDetailDataset  )  THEN DELETE OBJECT ghAuthRateDetailDataset.    
    IF VALID-OBJECT( ErrorObject )               THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthratedetail */


  /* -- METHOD focusAuthRateDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthRateDetail
    ( INPUT ipdAuthRateDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth rate detail with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth rate detail obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess             = unfocusAuthRateDetail()    /*Clear any properties*/

        AuthRateDetailObj     = ipdAuthRateDetailObj

        glSuccess             = THIS-OBJECT:getAuthRateDetail()  /* Retrieve all data for the specified obj
                                                                      and populate instance properties*/
        AuthRateDetailInFocus = glSuccess.

    RETURN AuthRateDetailInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthRateDetail  */
  
  /* -- METHOD focusAuthRateDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthRateDetail
   (INPUT ipdAuthRateControlObj AS DECIMAL, 
    INPUT ipdEffectiveDate      AS DATE):
  /*------------------------------------------------------------------
        Purpose   : Find an auth rate detail with the specified auth rate 
                    control obj and effective date and populate the instance 
                    properties with the relevant values.
        Parameters: Rate Control Obj and Effective Date
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess          = unfocusAuthRateDetail()    /*Clear any properties*/

        AuthRateControlObj = ipdAuthRateControlObj
        
        EffectiveDate      = ipdEffectiveDate

        glSuccess          = THIS-OBJECT:getAuthRateDetail()  /* Retrieve all data for the specified auth rate detail and populate instance properties*/
        
        AuthRateDetailInFocus = glSuccess.

    RETURN AuthRateDetailInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthRateDetail  */


  /* -- METHOD getAuthRateDetail --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthRateDetail( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieving auth rate detail data
                    using the auth rate detail data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthratesearch NO-UNDO.

    DATASET dsAuthRateControl:EMPTY-DATASET().

    IF AuthRateDetailObj <> 0.00 OR AuthRateControlObj <> 0 
    THEN DO:
      oSearchObject = NEW cls.maauthratesearch(DATASET dsAuthRateControl BY-REFERENCE).

      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_rate_detail":U).

      IF AuthRateDetailObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_detail.auth_rate_detail_obj":U, "=":U, AuthRateDetailObj).

      IF AuthRateControlObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_detail.auth_rate_control_obj":U, "=":U, AuthRateControlObj).  

      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_detail.effective_date":U, "<=":U, EffectiveDate).
      
      oSearchObject:fetchData().

    END. /*IF AuthRateDetailObj <> 0.00 */

    FOR FIRST tt_auth_rate_detail EXCLUSIVE-LOCK:

      ASSIGN
        AuthRateDetailObj     = tt_auth_rate_detail.auth_rate_detail_obj                            
        OverrideArsRate       = tt_auth_rate_detail.override_ars_rate
        OwningEntityMnemonic  = tt_auth_rate_detail.owning_entity_mnemonic
        OwningKey             = tt_auth_rate_detail.owning_key
        OwningAltValue        = tt_auth_rate_detail.owning_alt_value
        OwningObj             = tt_auth_rate_detail.owning_obj
        EffectiveDate         = tt_auth_rate_detail.effective_date
        EndDate               = tt_auth_rate_detail.end_date
        FlagValue             = tt_auth_rate_detail.flag_value
        AuthRateControlObj    = tt_auth_rate_detail.auth_rate_control_obj
        Exclusion             = tt_auth_rate_detail.exclusion
        . 
               
    END. /*FOR FIRST tt_auth_rate_detail NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_auth_rate_detail NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthTypeDetails  */


  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None
  ---------------------------------------------------------------------- */

    ASSIGN
       ghAuthRateDetailDataset  = DATASET dsAuthRateControl:HANDLE

       ErrorObject              = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */


  /* -- METHOD removeAuthRateDetail ------------------------------------------ */
  METHOD PUBLIC LOGICAL removeAuthRateDetail (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth rate detail record.
        Parameters: None
  ---------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN

    DEFINE BUFFER btt_auth_rate_detail FOR tt_auth_rate_detail.

    IF NOT AuthRateDetailInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth rate detail not in focus'"}

    FIND FIRST btt_auth_rate_detail EXCLUSIVE-LOCK
         WHERE btt_auth_rate_detail.auth_rate_detail_obj = AuthRateDetailObj.

    ASSIGN btt_auth_rate_detail.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthRate(INPUT-OUTPUT DATASET dsAuthRateControl BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* removeAuthRateDetail  */


  /* -- METHOD saveAuthType -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthRateDetail() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth rate detail record using the 
                    instance properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthRateControl:EMPTY-DATASET().

    CREATE tt_auth_rate_detail.

    ASSIGN
      tt_auth_rate_detail.line_number              = LineNumber
      tt_auth_rate_detail.record_action            = "MODIFY":U
      tt_auth_rate_detail.auth_rate_detail_obj     = AuthRateDetailObj
      tt_auth_rate_detail.override_ars_rate        = OverrideArsRate 
      tt_auth_rate_detail.owning_entity_mnemonic   = OwningEntityMnemonic 
      tt_auth_rate_detail.owning_key               = OwningKey
      tt_auth_rate_detail.owning_alt_value         = OwningAltValue
      tt_auth_rate_detail.owning_obj               = OwningObj
      tt_auth_rate_detail.effective_date           = EffectiveDate
      tt_auth_rate_detail.end_date                 = EndDate
      tt_auth_rate_detail.flag_value               = FlagValue
      tt_auth_rate_detail.auth_rate_control_obj    = AuthRateControlObj 
      tt_auth_rate_detail.apply_to_all             = ApplyToAll
      tt_auth_rate_detail.exclusion                = Exclusion.

    VALIDATE tt_auth_rate_detail.
    
    mipEnv:Health:AuthMaintenance:saveAuthRate(INPUT-OUTPUT DATASET dsAuthRateControl BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_rate_detail NO-LOCK:
        ASSIGN
           AuthRateDetailObj = tt_auth_rate_detail.auth_rate_detail_obj
           glSuccess          = THIS-OBJECT:focusAuthRateDetail(AuthRateDetailObj).
      END. /*FOR FIRST tt_auth_rate_detail NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* saveAuthType  */


  /* -- METHOD unfocusAuthRateDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthRateDetail() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

    DATASET dsAuthRateControl:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       AuthRateDetailInFocus       = FALSE
       LineNumber                  = 0
       AuthRateDetailObj           = 0.00
       OverrideArsRate             = "":U
       OwningEntityMnemonic        = "":U
       OwningKey                   = "":U
       OwningAltValue              = "":U
       OwningObj                   = 0
       EffectiveDate               = ?
       EndDate                     = ?
       FlagValue                   = "":U
       AuthRateControlObj          = 0
       Exclusion                   = ?
       .
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* unfocusAuthRateDetail  */

END CLASS.  /* mip.cls.basemaauthratedetail */
