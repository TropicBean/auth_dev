/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: MMP

      Purpose: Auth Copay Detail Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthcopaydetail INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthcopayds.i }
                                                  
                                                  
  DEFINE PRIVATE VARIABLE glSuccess                          AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthCopayDetailDataset           AS HANDLE  NO-UNDO.
                                                             
  DEFINE PUBLIC PROPERTY AuthCopayDetailInFocus              AS LOGICAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthCopayDetailObj                  AS DECIMAL           GET. SET.                                                            
  DEFINE PUBLIC PROPERTY LineNumber                          AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY AuthCopayControlObj                 AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj                           AS DECIMAL           GET. SET.       
  DEFINE PUBLIC PROPERTY OwningKey                           AS CHARACTER         GET. SET.    
  DEFINE PUBLIC PROPERTY OwningAltValue                      AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY ExclusionEntityMnemonic             AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ExclusionObj                        AS DECIMAL           GET. SET.       
  DEFINE PUBLIC PROPERTY ExclusionKey                        AS CHARACTER         GET. SET.    
  DEFINE PUBLIC PROPERTY ExclusionAltValue                   AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate                       AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                             AS DATE              GET. SET. 
  
  DEFINE PUBLIC PROPERTY ErrorObject                         AS cls.maerrorobject GET. PRIVATE SET.

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaydetail() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaydetail */

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaydetail
    ( INPUT ipdAuthCopayDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    THIS-OBJECT:focusAuthCopayDetail(ipdAuthCopayDetailObj).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaydetail */

  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthcopaydetail() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthCopayDetailDataset  ) THEN DELETE OBJECT ghAuthCopayDetailDataset.    
    IF VALID-OBJECT( ErrorObject )               THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthcopaydetail */

  /* -- METHOD focusAuthCopayDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthCopayDetail
    ( INPUT ipdAuthCopayDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth copay detail with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth copay detail obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess              = unfocusAuthCopayDetail()           /* Clear any properties */

        AuthCopayDetailObj     = ipdAuthCopayDetailObj

        AuthCopayDetailInFocus = THIS-OBJECT:getAuthCopayDetail()  /* Retrieve all data for the specified obj
                                                                      and populate instance properties*/
        .

    RETURN AuthCopayDetailInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthCopayDetail  */
  
  /* -- METHOD focusAuthCopayDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthCopayDetail
   (INPUT ipdAuthCopayControlObj AS DECIMAL, 
    INPUT ipdEffectiveDate       AS DATE):
  /*------------------------------------------------------------------
        Purpose   : Find an auth copay detail with the specified auth rate 
                    control obj and effective date and populate the instance 
                    properties with the relevant values.
        Parameters: Auth Copay Control Obj and Effective Date
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess           = unfocusAuthCopayDetail()             /* Clear any properties */

        AuthCopayControlObj = ipdAuthCopayControlObj
        
        EffectiveDate       = ipdEffectiveDate

        AuthCopayDetailInFocus = THIS-OBJECT:getAuthCopayDetail()  /* Retrieve all data for the specified auth rate detail and populate instance properties*/
        
        .

    RETURN AuthCopayDetailInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthCopayDetail  */

  /* -- METHOD getAuthCopayDetail --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthCopayDetail( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieving auth copay detail data
                    using the auth copay detail data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthcopaysearch NO-UNDO.

    DATASET dsAuthCopayControl:EMPTY-DATASET().

    IF AuthCopayDetailObj <> 0.00 OR AuthCopayControlObj <> 0 
    THEN DO:
      oSearchObject = NEW cls.maauthcopaysearch(DATASET dsAuthCopayControl BY-REFERENCE).

      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_copay_detail":U).

      IF AuthCopayDetailObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail.auth_copay_detail_obj":U, "=":U, AuthCopayDetailObj).

      IF AuthCopayControlObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail.auth_copay_control_obj":U, "=":U, AuthCopayControlObj).  

      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail.effective_date":U, "<=":U, EffectiveDate).
      
      oSearchObject:fetchData().

    END. /* IF AuthCopayDetailObj <> 0.00 */

    FOR FIRST tt_auth_copay_detail EXCLUSIVE-LOCK:

      ASSIGN
        AuthCopayDetailObj      = tt_auth_copay_detail.auth_copay_detail_obj                            
        AuthCopayControlObj     = tt_auth_copay_detail.auth_copay_control_obj
        OwningEntityMnemonic    = tt_auth_copay_detail.owning_entity_mnemonic
        OwningObj               = tt_auth_copay_detail.owning_obj
        OwningKey               = tt_auth_copay_detail.owning_key
        OwningAltValue          = tt_auth_copay_detail.owning_alt_value
        ExclusionEntityMnemonic = tt_auth_copay_detail.exclusion_entity_mnemonic
        ExclusionObj            = tt_auth_copay_detail.exclusion_obj
        ExclusionKey            = tt_auth_copay_detail.exclusion_key
        ExclusionAltValue       = tt_auth_copay_detail.exclusion_alt_value
        EffectiveDate           = tt_auth_copay_detail.effective_date
        EndDate                 = tt_auth_copay_detail.end_date
        . 
               
    END. /* FOR FIRST tt_auth_copay_detail NO-LOCK: */

    RETURN CAN-FIND(FIRST tt_auth_copay_detail NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthCopayDetail  */

  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None
  ---------------------------------------------------------------------- */

    ASSIGN
       ghAuthCopayDetailDataset  = DATASET dsAuthCopayControl:HANDLE

       ErrorObject              = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */


  /* -- METHOD removeAuthCopayDetail ------------------------------------------ */
  METHOD PUBLIC LOGICAL removeAuthCopayDetail (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth copay detail record.
        Parameters: None
  ---------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN

    DEFINE BUFFER btt_auth_copay_detail FOR tt_auth_copay_detail.

    IF NOT AuthCopayDetailInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth copay detail not in focus'"}

    FIND FIRST btt_auth_copay_detail EXCLUSIVE-LOCK
         WHERE btt_auth_copay_detail.auth_copay_detail_obj = AuthCopayDetailObj.

    ASSIGN btt_auth_copay_detail.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthCopay(INPUT-OUTPUT DATASET dsAuthCopayControl BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* removeAuthCopayDetail  */


  /* -- METHOD saveAuthCopayDetail -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthCopayDetail() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth copay detail record using the 
                    instance properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthCopayControl:EMPTY-DATASET().

    CREATE tt_auth_copay_detail.

    ASSIGN
      tt_auth_copay_detail.line_number               = LineNumber
      tt_auth_copay_detail.record_action             = "MODIFY":U
      tt_auth_copay_detail.auth_copay_control_obj    = AuthCopayControlObj 
      tt_auth_copay_detail.auth_copay_detail_obj     = AuthCopayDetailObj
      tt_auth_copay_detail.owning_entity_mnemonic    = OwningEntityMnemonic 
      tt_auth_copay_detail.owning_obj                = OwningObj
      tt_auth_copay_detail.owning_key                = OwningKey
      tt_auth_copay_detail.owning_alt_value          = OwningAltValue
      tt_auth_copay_detail.exclusion_entity_mnemonic = ExclusionEntityMnemonic 
      tt_auth_copay_detail.exclusion_obj             = ExclusionObj
      tt_auth_copay_detail.exclusion_key             = ExclusionKey
      tt_auth_copay_detail.exclusion_alt_value       = ExclusionAltValue
      tt_auth_copay_detail.effective_date            = EffectiveDate
      tt_auth_copay_detail.end_date                  = EndDate
      .

    VALIDATE tt_auth_copay_detail.
    
    mipEnv:Health:AuthMaintenance:saveAuthCopay(INPUT-OUTPUT DATASET dsAuthCopayControl BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_copay_detail NO-LOCK:
        ASSIGN
           AuthCopayDetailObj = tt_auth_copay_detail.auth_copay_detail_obj
           glSuccess          = THIS-OBJECT:focusAuthCopayDetail(AuthCopayDetailObj).
      END. /* FOR FIRST tt_auth_copay_detail NO-LOCK: */
    END. /* IF NOT ErrorObject:ErrorsExist THEN */

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* saveAuthCopayDetail  */

  /* -- METHOD unfocusAuthCopayDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthCopayDetail() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

    DATASET dsAuthCopayControl:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
      AuthCopayDetailInFocus  = FALSE
      LineNumber              = 0
      AuthCopayControlObj     = 0
      AuthCopayDetailObj      = 0.00
      OwningEntityMnemonic    = "":U
      OwningKey               = "":U
      OwningAltValue          = "":U
      OwningObj               = 0
      ExclusionEntityMnemonic = "":U
      ExclusionKey            = "":U
      ExclusionAltValue       = "":U
      ExclusionObj            = 0
      EffectiveDate           = ?
      EndDate                 = ?
      .
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* unfocusAuthCopayDetail  */

END CLASS.  /* mip.cls.basemaauthcopaydetail */
