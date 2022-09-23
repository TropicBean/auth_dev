/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandla M

      Purpose: Auth Rate Provider Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthrateprovider INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthratecontrolds.i }
                                                  
                                                  
  DEFINE PRIVATE VARIABLE glSuccess                          AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthRateProviderDataset          AS HANDLE  NO-UNDO.
                                                             
  DEFINE PUBLIC PROPERTY LineNumber                          AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY AuthRateProviderInFocus             AS LOGICAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthRateControlObj                  AS DECIMAL           GET. SET. 
  DEFINE PUBLIC PROPERTY AuthRateProviderObj                 AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthRateIndicator                   AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY AuthRateProviderType                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY AuthRatePrType                      AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY AuthoriseAllServices                AS LOGICAL           GET. SET. 
  DEFINE PUBLIC PROPERTY AuthGroupObj                        AS DECIMAL           GET. SET. 
  DEFINE PUBLIC PROPERTY RelatedEntityMnemonic               AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY RelatedKey                          AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY RelatedObj                          AS DECIMAL           GET. SET. 
  DEFINE PUBLIC PROPERTY RelatedAltValue                     AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY ProviderAmount                      AS DECIMAL           GET. SET. 
  DEFINE PUBLIC PROPERTY AuthRateSubPrType                   AS INTEGER           GET. SET. 
  DEFINE PUBLIC PROPERTY OverrideAuthStatus                  AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY OverrideAuthStatusNote              AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY RevertAuthStatus                    AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY RevertAuthStatusNote                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate                       AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                             AS DATE              GET. SET.
  
  DEFINE PUBLIC PROPERTY ErrorObject                         AS cls.maerrorobject GET. PRIVATE SET.
    

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthrateprovider() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthrateprovider */


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthrateprovider
    ( INPUT ipdAuthRateProviderObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    THIS-OBJECT:focusAuthRateProvider(ipdAuthRateProviderObj).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthrateprovider */


  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthrateprovider() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthRateProviderDataset ) THEN DELETE OBJECT ghAuthRateProviderDataset.    
    IF VALID-OBJECT( ErrorObject )               THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthrateprovider */


  /* -- METHOD focusAuthRateprovider ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthRateprovider
    ( INPUT ipdAuthRateProviderObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth rate provider with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth rate provider obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess             = unfocusAuthRateProvider()          /*Clear any properties*/

        AuthRateProviderObj   = ipdAuthRateProviderObj

        glSuccess             = THIS-OBJECT:getAuthRateProvider()  /* Retrieve all data for the specified obj
                                                                      and populate instance properties*/
        AuthRateProviderInFocus = glSuccess.

    RETURN AuthRateProviderInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthRateProvider  */
  
  /* -- METHOD focusAuthRateprovider ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthRateProvider
   (INPUT ipdAuthRateControlObj AS DECIMAL, 
    INPUT ipdEffectiveDate      AS DATE):
  /*------------------------------------------------------------------
        Purpose   : Find an auth rate provider with the specified auth rate 
                    control obj and effective date and populate the instance 
                    properties with the relevant values.
        Parameters: Rate Control Obj and Effective Date
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess          = unfocusAuthRateProvider()    /*Clear any properties*/

        AuthRateControlObj = ipdAuthRateControlObj
        
        EffectiveDate      = ipdEffectiveDate

        glSuccess          = THIS-OBJECT:getAuthRateProvider()  /* Retrieve all data for the specified auth rate provider and populate instance properties*/
        
        AuthRateProviderInFocus = glSuccess.

    RETURN AuthRateProviderInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthRateProvider  */


  /* -- METHOD getAuthRateProvider --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthRateProvider( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieving auth rate provider data
                    using the auth rate provider data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthratesearch NO-UNDO.

    DATASET dsAuthRateControl:EMPTY-DATASET().

    IF AuthRateProviderObj <> 0.00 OR AuthRateControlObj <> 0 
    THEN DO:
      oSearchObject = NEW cls.maauthratesearch(DATASET dsAuthRateControl BY-REFERENCE).

      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_rate_provider":U).

      IF AuthRateProviderObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_provider.auth_rate_provider_obj":U, "=":U, AuthRateProviderObj).

      IF AuthRateControlObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_provider.auth_rate_control_obj":U, "=":U, AuthRateControlObj).  

      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_provider.effective_date":U, "<=":U, EffectiveDate).
      
      oSearchObject:fetchData().

    END. /*IF AuthRateProviderObj <> 0.00 */

    FOR FIRST tt_auth_rate_provider EXCLUSIVE-LOCK:

      ASSIGN
        AuthRateProviderObj       = tt_auth_rate_provider.auth_rate_provider_obj                            
        AuthRateControlObj        = tt_auth_rate_provider.auth_rate_control_obj
        AuthRateIndicator         = tt_auth_rate_provider.auth_rate_indicator
        AuthRateProviderType      = tt_auth_rate_provider.auth_rate_provider_type
        AuthRatePrType            = tt_auth_rate_provider.auth_rate_pr_type
        AuthoriseAllServices      = tt_auth_rate_provider.authorise_all_services
        AuthGroupObj              = tt_auth_rate_provider.auth_group_obj
        RelatedEntityMnemonic     = tt_auth_rate_provider.related_entity_mnemonic
        RelatedKey                = tt_auth_rate_provider.related_key
        RelatedObj                = tt_auth_rate_provider.related_obj
        RelatedAltValue           = tt_auth_rate_provider.related_alt_value
        ProviderAmount            = tt_auth_rate_provider.provider_amount
        AuthRateSubPrType         = tt_auth_rate_provider.auth_rate_sub_pr_type
        OverrideAuthStatus        = tt_auth_rate_provider.override_auth_status
        OverrideAuthStatusNote    = tt_auth_rate_provider.override_auth_status_note
        RevertAuthStatus          = tt_auth_rate_provider.revert_auth_status
        RevertAuthStatusNote      = tt_auth_rate_provider.revert_auth_status_note
        EffectiveDate             = tt_auth_rate_provider.effective_date
        EndDate                   = tt_auth_rate_provider.end_date
        . 
    END. /*FOR FIRST tt_auth_rate_provider NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_auth_rate_provider NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthRateProvider  */


  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None
  ---------------------------------------------------------------------- */

    ASSIGN
       ghAuthRateProviderDataset  = DATASET dsAuthRateControl:HANDLE

       ErrorObject                = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */


  /* -- METHOD removeAuthRateProvider ------------------------------------------ */
  METHOD PUBLIC LOGICAL removeAuthRateProvider (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth rate provider record.
        Parameters: None
  ---------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN

    DEFINE BUFFER btt_auth_rate_provider FOR tt_auth_rate_provider.

    IF NOT AuthRateProviderInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth rate provider not in focus'"}

    FIND FIRST btt_auth_rate_provider EXCLUSIVE-LOCK
         WHERE btt_auth_rate_provider.auth_rate_provider_obj = AuthRateProviderObj.

    ASSIGN btt_auth_rate_provider.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthRate(INPUT-OUTPUT DATASET dsAuthRateControl BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* removeAuthRateProvider  */


  /* -- METHOD saveAuthType -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthRateProvider() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth rate provider record using the 
                    instance properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthRateControl:EMPTY-DATASET().

    CREATE tt_auth_rate_provider.

    ASSIGN
      tt_auth_rate_provider.line_number                  = LineNumber
      tt_auth_rate_provider.record_action                = "MODIFY":U
      tt_auth_rate_provider.auth_rate_control_obj        = AuthRateControlObj
      tt_auth_rate_provider.auth_rate_provider_obj       = AuthRateProviderObj
      tt_auth_rate_provider.auth_rate_indicator          = AuthRateIndicator
      tt_auth_rate_provider.auth_rate_provider_type      = AuthRateProviderType
      tt_auth_rate_provider.auth_rate_pr_type            = AuthRatePrType
      tt_auth_rate_provider.authorise_all_services       = AuthoriseAllServices               
      tt_auth_rate_provider.auth_group_obj               = AuthGroupObj          
      tt_auth_rate_provider.related_entity_mnemonic      = RelatedEntityMnemonic 
      tt_auth_rate_provider.related_key                  = RelatedKey            
      tt_auth_rate_provider.related_obj                  = RelatedObj            
      tt_auth_rate_provider.related_alt_value            = RelatedAltValue       
      tt_auth_rate_provider.provider_amount              = ProviderAmount        
      tt_auth_rate_provider.auth_rate_sub_pr_type        = AuthRateSubPrType     
      tt_auth_rate_provider.override_auth_status         = OverrideAuthStatus
      tt_auth_rate_provider.override_auth_status_note    = OverrideAuthStatusNote
      tt_auth_rate_provider.revert_auth_status           = RevertAuthStatus
      tt_auth_rate_provider.revert_auth_status_note      = RevertAuthStatusNote
      tt_auth_rate_provider.effective_date               = EffectiveDate
      tt_auth_rate_provider.end_date                     = EndDate
      .  

    VALIDATE tt_auth_rate_provider.
    
    mipEnv:Health:AuthMaintenance:saveAuthRate(INPUT-OUTPUT DATASET dsAuthRateControl BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_rate_provider NO-LOCK:
        ASSIGN
           AuthRateProviderObj = tt_auth_rate_provider.auth_rate_provider_obj
           glSuccess           = THIS-OBJECT:focusAuthRateProvider(AuthRateProviderObj).
      END. /*FOR FIRST tt_auth_rate_provider NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* saveAuthType  */


  /* -- METHOD unfocusAuthRateprovider ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthRateProvider() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

    DATASET dsAuthRateControl:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       AuthRateproviderInFocus     = FALSE
       LineNumber                  = 0
       AuthRateproviderObj         = 0.00
       AuthRateControlObj          = 0.00
       AuthRateIndicator           = "":U
       AuthRateProviderType        = "":U
       AuthRatePrType              = ?
       AuthoriseAllServices        = ?
       AuthGroupObj                = 0.00
       RelatedEntityMnemonic       = "":U
       RelatedKey                  = "":U
       RelatedObj                  = 0.00
       RelatedAltValue             = "":U
       ProviderAmount              = 0.00
       AuthRateSubPrType           = 0
       OverrideAuthStatus          = 0
       OverrideAuthStatusNote      = "":U
       RevertAuthStatus            = ?
       RevertAuthStatusNote        = "":U
       EffectiveDate               = ?
       EndDate                     = ?
       .
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* unfocusAuthRateprovider  */

END CLASS.  /* mip.cls.basemaauthrateprovider */

