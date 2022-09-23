/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Kati

      Purpose: Base Authorisation Type Provider Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthtypeprovider INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthtypeds.i }
  
  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }
  
  DEFINE PRIVATE VARIABLE glSuccess                   AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthTypeDataset           AS HANDLE  NO-UNDO.
                                                      
  DEFINE PUBLIC PROPERTY AuthTypeProviderInFocus      AS LOGICAL           GET. PRIVATE SET.                        
                                                                           
  DEFINE PUBLIC PROPERTY LineNumber                   AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeObj                  AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeProviderObj          AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AmountAuth                   AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY ArsRateUpdRole               AS CHARACTER         GET. SET.  
  DEFINE PUBLIC PROPERTY ArsRateUpdUser               AS CHARACTER         GET. SET.  
  DEFINE PUBLIC PROPERTY AuthoriseDetailLines         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY AuthorisedService            AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY BaseRateUpdRole              AS CHARACTER         GET. SET.  
  DEFINE PUBLIC PROPERTY BaseRateUpdUser              AS CHARACTER         GET. SET.  
  DEFINE PUBLIC PROPERTY ClaimCodesDisallow           AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimTypesDisallow           AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCodesProvider           AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimTypesProvider           AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatus            AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatusNote        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatusUpdUser     AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatusUpdRole     AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimCodeDetail       AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimTypeDetail       AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DocNumMandatory              AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate                AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                      AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY HeaderValuesAllowed          AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY HeaderValuesUnlimited        AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj                   AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY MainProvider                 AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY Mandatory                    AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY NumberProvidersAllowed       AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY ProviderSequence             AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY ProviderType                 AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ProviderTypeIndicator        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY PrTypeList                   AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY PrTypeValidList              AS CHARACTER         GET. SET.  
  DEFINE PUBLIC PROPERTY OptionCode                   AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY QuantityAuth                 AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY ValidClaimCodesDetail        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ValidClaimTypesDetail        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY NegotiationGroup             AS INTEGER           GET. SET.                             
  DEFINE PUBLIC PROPERTY EnforceClaimCodeMatch        AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY EnforceClaimTypeMatch        AS LOGICAL           GET. SET.  
  DEFINE PUBLIC PROPERTY AuthGroupObj                 AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthAutoCreate               AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ErrorObject                  AS cls.maerrorobject GET. PRIVATE SET.         
  
  /* -- CONSTRUCTOR -------------------------------------------------- */                               
  CONSTRUCTOR PUBLIC basemaauthtypeprovider() :                                                          
  /*------------------------------------------------------------------                                  
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypeprovider */

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthtypeprovider
    ( INPUT ipdAuthTypeProviderObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthTypeProvider(ipdAuthTypeProviderObj).

    { mip/inc/mipcatcherror.i }
                              
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypeprovider */  
  
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthtypeprovider() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthTypeDataset  ) THEN DELETE OBJECT ghAuthTypeDataset.
    IF VALID-OBJECT( ErrorObject )        THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthtypeprovider */
  
  /* -- METHOD focusAuthTypeProvider -------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthTypeProvider
    ( INPUT ipdAuthTypeProviderObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type provider with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth type Provider obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess               = unfocusAuthTypeProvider()    /*Clear any properties*/
                               
        AuthTypeProviderObj     = ipdAuthTypeProviderObj
                               
        glSuccess               = getAuthTypeProvider()      /*Retrieve all data for the specified obj
                                                               and populate instance properties*/
        AuthTypeProviderInFocus = glSuccess.

    RETURN AuthTypeProviderInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthTypeProvider  */
  
  /* -- METHOD focusAuthTypeProvider -------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthTypeProvider
    ( INPUT ipdAuthTypeObj           AS DECIMAL,
      INPUT ipdInsurerObj            AS DECIMAL,
      INPUT ipiOptionCode            AS INTEGER,
      INPUT ipcProviderType          AS CHARACTER,
      INPUT ipdDate                  AS DATE,
      INPUT ipcPrTypeList            AS CHARACTER,
      INPUT ipcPrTypeValidList       AS CHARACTER,
      INPUT ipcProviderTypeIndicator AS CHARACTER) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type provider with the specified parameter
                    values and populate the instance properties with
                    the relevant values.
        Parameters: Auth type obj
                    Insurer Obj
                    Option Code
                    Provider Type
                    Date
                    Practice Type List
                    Practice Type Valid List
                    Provider Type Indicator
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess               = unfocusAuthTypeProvider()    /*Clear any properties*/
                               
        AuthTypeObj             = ipdAuthTypeObj
        InsurerObj              = ipdInsurerObj  
        OptionCode              = ipiOptionCode  
        ProviderType            = ipcProviderType
        EffectiveDate           = ipdDate        
        PrTypeList              = ipcPrTypeList    
        ProviderTypeIndicator   = ipcProviderTypeIndicator
        glSuccess               = getAuthTypeProvider()      /*Retrieve all data for the specified obj
                                                               and populate instance properties*/
        AuthTypeProviderInFocus = glSuccess.

    RETURN AuthTypeProviderInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthTypeProvider  */
  
  
  /* -- METHOD focusAuthTypeProvider -------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthTypeProvider
    ( INPUT ipdAuthTypeObj           AS DECIMAL,
      INPUT ipdInsurerObj            AS DECIMAL,
      INPUT ipiOptionCode            AS INTEGER,
      INPUT ipcProviderType          AS CHARACTER,
      INPUT ipdDate                  AS DATE,
      INPUT ipcPrTypeList            AS INTEGER,
      INPUT ipcPrTypeValidList       AS INTEGER,
      INPUT ipcProviderTypeIndicator AS CHARACTER) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type provider with the specified parameter
                    values and populate the instance properties with
                    the relevant values.
        Parameters: Auth type obj
                    Insurer Obj
                    Option Code
                    Provider Type
                    Date
                    Practice Type List
                    Practice Type Valid List
                    Provider Type Indicator
  -------------------------------------------------------------------- */

    ASSIGN AuthTypeProviderInFocus = focusAuthTypeProvider(ipdAuthTypeObj,
                                                           ipdInsurerObj,
                                                           ipiOptioncode,
                                                           ipcProviderType,
                                                           ipdDate,
                                                           STRING(ipcPrTypeList),
                                                           STRING(ipcPrTypeValidList),
                                                           ipcProviderTypeIndicator).

    RETURN AuthTypeProviderInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthTypeProvider  */
  
  /* -- METHOD getAuthTypeProvider --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthTypeProvider( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing auth type provider 
                    data using the auth type data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthtypesearch NO-UNDO.

    DATASET dsAuthType:EMPTY-DATASET().

    IF AuthTypeProviderObj <> 0.00 
    OR AuthTypeObj         <> 0.00 THEN
    DO:
      oSearchObject = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).

      /*We arent interested in all of the buffers that have been defined in the auth type dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_type_provider":U).

      IF AuthTypeProviderObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_provider.auth_type_provider_obj":U, "=":U, AuthTypeProviderObj).
        
      IF AuthTypeObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_provider.auth_type_obj":U, "=":U, AuthTypeObj).
      
      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_provider.effective_date":U, "<=":U, EffectiveDate).

      oSearchObject:fetchData().

    END. /*IF AuthTypeProviderObj <> 0.00 */

    FOR LAST tt_auth_type_provider NO-LOCK
        WHERE (tt_auth_type_provider.insurer_obj             = InsurerObj            OR tt_auth_type_provider.insurer_obj             = 0  OR InsurerObj = 0   )
        AND   (tt_auth_type_provider.option_code             = OptionCode            OR tt_auth_type_provider.option_code             = 0  OR OptionCode = 0   )
        AND   (tt_auth_type_provider.provider_type           = ProviderType          OR tt_auth_type_provider.provider_type           = "" OR ProviderType = "")
        AND   (tt_auth_type_provider.pr_type_list            = PrTypeList            OR tt_auth_type_provider.pr_type_list            = "" OR PrTypeList = ""       )
        AND   (tt_auth_type_provider.pr_type_valid_list      = PrTypeValidList       OR tt_auth_type_provider.pr_type_valid_list      = "" OR PrTypeValidList = ""    )
        AND   (tt_auth_type_provider.end_date               >= EffectiveDate         OR tt_auth_type_provider.end_date                = ?)
        AND   (tt_auth_type_provider.provider_type_indicator = ProviderTypeIndicator OR tt_auth_type_provider.provider_type_indicator = "" OR ProviderTypeIndicator = ""): 
          
      ASSIGN
        AmountAuth               = tt_auth_type_provider.amount_auth             
        ArsRateUpdRole           = tt_auth_type_provider.ars_rate_upd_role 
        ArsRateUpdUser           = tt_auth_type_provider.ars_rate_upd_user 
        AuthoriseDetailLines     = tt_auth_type_provider.authorise_detail_lines  
        AuthorisedService        = tt_auth_type_provider.authorised_service     
        AuthTypeObj              = tt_auth_type_provider.auth_type_obj       
        AuthTypeProviderObj      = tt_auth_type_provider.auth_type_provider_obj
        BaseRateUpdRole          = tt_auth_type_provider.base_rate_upd_role
        BaseRateUpdUser          = tt_auth_type_provider.base_rate_upd_user
        ClaimCodesDisallow       = tt_auth_type_provider.claim_codes_disallow    
        ClaimCodesProvider       = tt_auth_type_provider.claim_codes_provider    
        ClaimTypesDisallow       = tt_auth_type_provider.claim_types_disallow    
        ClaimTypesProvider       = tt_auth_type_provider.claim_types_provider    
        DefaultAuthStatus        = tt_auth_type_provider.default_auth_status         
        DefaultAuthStatusNote    = tt_auth_type_provider.default_auth_status_note    
        DefaultAuthStatusUpdRole = tt_auth_type_provider.default_auth_status_upd_role
        DefaultAuthStatusUpdUser = tt_auth_type_provider.default_auth_status_upd_user
        DefaultClaimCodeDetail   = tt_auth_type_provider.default_claim_code_detail
        DefaultClaimTypeDetail   = tt_auth_type_provider.default_claim_type_detail
        DocNumMandatory          = tt_auth_type_provider.doc_num_mandatory 
        EffectiveDate            = tt_auth_type_provider.effective_date      
        EndDate                  = tt_auth_type_provider.end_date           
        HeaderValuesAllowed      = tt_auth_type_provider.header_values_allowed   
        HeaderValuesUnlimited    = tt_auth_type_provider.header_values_unlimited 
        InsurerObj               = tt_auth_type_provider.insurer_obj             
        MainProvider             = tt_auth_type_provider.main_provider     
        Mandatory                = tt_auth_type_provider.mandatory
        NumberProvidersAllowed   = tt_auth_type_provider.number_providers_allowed
        OptionCode               = tt_auth_type_provider.option_code    
        ProviderSequence         = tt_auth_type_provider.provider_sequence       
        ProviderType             = tt_auth_type_provider.provider_type  
        ProviderTypeIndicator    = tt_auth_type_provider.provider_type_indicator
        PrTypeList               = tt_auth_type_provider.pr_type_list                 
        PrTypeValidList          = tt_auth_type_provider.pr_type_valid_list
        QuantityAuth             = tt_auth_type_provider.quantity_auth           
        ValidClaimCodesDetail    = tt_auth_type_provider.valid_claim_codes_detail
        ValidClaimTypesDetail    = tt_auth_type_provider.valid_claim_types_detail
        NegotiationGroup         = tt_auth_type_provider.neg_num
        EnforceClaimCodeMatch    = tt_auth_type_provider.enforce_header_claim_code_match 
        EnforceClaimTypeMatch    = tt_auth_type_provider.enforce_header_claim_type_match 
        AuthGroupObj             = tt_auth_type_provider.auth_group_obj 
        AuthAutoCreate           = tt_auth_type_provider.auth_auto_create 
        .  
         
    END. /*FOR LAST tt_auth_type_provider NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_auth_type_provider NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthTypeProvider  */
  
  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None
  ---------------------------------------------------------------------- */

    ASSIGN
       ghAuthTypeDataset = DATASET dsAuthType:HANDLE
 
       ErrorObject       = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */

    /* -- METHOD removeAuthTypeProvider --------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthTypeProvider (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth type provider record.
        Parameters: None
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN
    DEFINE BUFFER btt_auth_type_provider FOR tt_auth_type_provider.

    IF NOT AuthTypeProviderInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authoristion type Provider not in focus'"}

    FIND FIRST btt_auth_type_provider EXCLUSIVE-LOCK
         WHERE btt_auth_type_provider.auth_type_provider_obj = AuthTypeProviderObj.

    ASSIGN btt_auth_type_provider.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF
  END METHOD.  /* removeAuthTypeProvider  */
  
  /* -- METHOD saveAuthType -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthType() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth type provider record using the instance
                    properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthType:EMPTY-DATASET().

    DO TRANSACTION ON ERROR UNDO, THROW:

      CREATE tt_auth_type_provider.
  
      ASSIGN
        tt_auth_type_provider.line_number                     = LineNumber
        tt_auth_type_provider.record_action                   = "MODIFY":U
        tt_auth_type_provider.auth_type_provider_obj          = AuthTypeProviderObj    
        tt_auth_type_provider.auth_type_obj                   = AuthTypeObj           
        tt_auth_type_provider.amount_auth                     = AmountAuth            
        tt_auth_type_provider.ars_rate_upd_role               = ArsRateUpdRole            
        tt_auth_type_provider.ars_rate_upd_user               = ArsRateUpdUser 
        tt_auth_type_provider.authorise_detail_lines          = AuthoriseDetailLines  
        tt_auth_type_provider.authorised_service              = AuthorisedService     
        tt_auth_type_provider.base_rate_upd_role              = BaseRateUpdRole           
        tt_auth_type_provider.base_rate_upd_user              = BaseRateUpdUser
        tt_auth_type_provider.claim_codes_disallow            = ClaimCodesDisallow       
        tt_auth_type_provider.claim_codes_provider            = ClaimCodesProvider    
        tt_auth_type_provider.claim_types_disallow            = ClaimTypesDisallow    
        tt_auth_type_provider.claim_types_provider            = ClaimTypesProvider    
        tt_auth_type_provider.default_auth_status             = DefaultAuthStatus       
        tt_auth_type_provider.default_auth_status_note        = DefaultAuthStatusNote   
        tt_auth_type_provider.default_auth_status_upd_role    = DefaultAuthStatusUpdRole
        tt_auth_type_provider.default_auth_status_upd_user    = DefaultAuthStatusUpdUser
        tt_auth_type_provider.default_claim_code_detail       = DefaultClaimCodeDetail
        tt_auth_type_provider.default_claim_type_detail       = DefaultClaimTypeDetail
        tt_auth_type_provider.doc_num_mandatory               = DocNumMandatory         
        tt_auth_type_provider.effective_date                  = EffectiveDate         
        tt_auth_type_provider.end_date                        = EndDate               
        tt_auth_type_provider.header_values_allowed           = HeaderValuesAllowed   
        tt_auth_type_provider.header_values_unlimited         = HeaderValuesUnlimited 
        tt_auth_type_provider.insurer_obj                     = InsurerObj            
        tt_auth_type_provider.main_provider                   = MainProvider          
        tt_auth_type_provider.mandatory                       = Mandatory             
        tt_auth_type_provider.number_providers_allowed        = NumberProvidersAllowed
        tt_auth_type_provider.option_code                     = OptionCode            
        tt_auth_type_provider.provider_sequence               = ProviderSequence       
        tt_auth_type_provider.provider_type                   = ProviderType          
        tt_auth_type_provider.provider_type_indicator         = ProviderTypeIndicator     
        tt_auth_type_provider.pr_type_list                    = PrTypeList                 
        tt_auth_type_provider.pr_type_valid_list              = PrTypeValidList
        tt_auth_type_provider.quantity_auth                   = QuantityAuth          
        tt_auth_type_provider.valid_claim_codes_detail        = ValidClaimCodesDetail
        tt_auth_type_provider.valid_claim_types_detail        = ValidClaimTypesDetail
        tt_auth_type_provider.neg_num                         = NegotiationGroup      
        tt_auth_type_provider.enforce_header_claim_code_match = EnforceClaimCodeMatch 
        tt_auth_type_provider.enforce_header_claim_type_match = EnforceClaimTypeMatch 
        tt_auth_type_provider.auth_group_obj                  = AuthGroupObj
        tt_auth_type_provider.auth_auto_create                = AuthAutoCreate
        .             
         
      VALIDATE tt_auth_type_provider.

    END.  /* DO TRANSACTION ON ERROR UNDO, THROW: */
    
    mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE) NO-ERROR.
    
    EMPTY TEMP-TABLE tt_temp_error.
    /*
      When the record is re-focused the entire dataset is emptied, meaning all errors and warnings are cleared as well,
      but we would like to keep track of any warnings even though the record created successfully so that these warnings
      can be returned to the user, so we will temporarily keep track of these warnings and copy them back to the dataset.
    */
    IF ErrorObject:WarningsExist 
    THEN 
      TEMP-TABLE tt_temp_error:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_type_error:HANDLE).
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_type_provider NO-LOCK:
        ASSIGN
           AuthTypeProviderObj = tt_auth_type_provider.auth_type_provider_obj
           glSuccess           = THIS-OBJECT:focusAuthTypeProvider(AuthTypeProviderObj).
      END. /*FOR FIRST tt_auth_type_provider NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    /*
      Copy warnings back to the dataset error temp table
    */
    IF TEMP-TABLE tt_temp_error:HAS-RECORDS
    THEN 
      TEMP-TABLE tt_auth_type_error:COPY-TEMP-TABLE(TEMP-TABLE tt_temp_error:HANDLE).
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF
  END METHOD.  /* AuthTypeProvider  */
  
  /* -- METHOD unfocusAuthTypeProvider --------------------------------- */
  METHOD PUBLIC LOGICAL unfocusAuthTypeProvider() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

    DATASET dsAuthType:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       AuthTypeProviderInFocus   = FALSE
       LineNumber                = 0
       AuthTypeProviderObj       = 0.00
       AuthTypeObj               = 0.00
       AmountAuth                = 0.00
       ArsRateUpdRole            = "":U
       ArsRateUpdUser            = "":U
       AuthoriseDetailLines      = "":U
       AuthorisedService         = FALSE
       BaseRateUpdRole           = "":U
       BaseRateUpdUser           = "":U
       ClaimCodesDisallow        = "":U
       ClaimCodesProvider        = "":U
       ClaimTypesDisallow        = "":U
       ClaimTypesProvider        = "":U
       DefaultAuthStatus         = "":U         
       DefaultAuthStatusNote     = "":U    
       DefaultAuthStatusUpdRole  = "":U
       DefaultAuthStatusUpdUser  = "":U
       DefaultClaimCodeDetail    = 0
       DefaultClaimTypeDetail    = "":U
       DocNumMandatory           = FALSE
       EffectiveDate             = ?
       EndDate                   = ?
       HeaderValuesAllowed       = "":U
       HeaderValuesUnlimited     = FALSE
       InsurerObj                = 0.00
       MainProvider              = FALSE
       Mandatory                 = FALSE
       NumberProvidersAllowed    = 0
       OptionCode                = 0
       ProviderSequence          = 0
       ProviderType              = "":U
       ProviderTypeIndicator     = "":U
       PrTypeList                = "":U
       PrTypeValidList           = "":U
       QuantityAuth              = 0
       ValidClaimCodesDetail     = "":U
       ValidClaimTypesDetail     = "":U
       NegotiationGroup          = 0
       EnforceClaimCodeMatch     = FALSE
       EnforceClaimTypeMatch     = FALSE
       AuthGroupObj              = 0.00
       AuthAutoCreate            = FALSE
       .  
       
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* unfocusAuthTypeProvider  */
  
END CLASS.    /* ma.cls.basemaauthtypeprovider */       

