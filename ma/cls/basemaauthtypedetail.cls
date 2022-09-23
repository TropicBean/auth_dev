/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Miranda

      Purpose: Base Authorisation Type Detail Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthtypedetail INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthtypeds.i }
  
  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }
  
  DEFINE PRIVATE VARIABLE glSuccess                   AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthTypeDataset           AS HANDLE  NO-UNDO.
                                                      
  DEFINE PUBLIC PROPERTY AuthTypeDetailInFocus        AS LOGICAL           GET. PRIVATE SET.                        
                                                                           
  DEFINE PUBLIC PROPERTY AuthAutoCreate               AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeDetailObj            AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeObj                  AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeProviderObj          AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthUsageLimit               AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY DefaultLineRestriction       AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatus            AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatusNote        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DetailTypeIndicator          AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate                AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                      AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY LineNumber                   AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic         AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY OwningObj                    AS DECIMAL           GET. SET. 
  DEFINE PUBLIC PROPERTY OwningKey                    AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OwningAltValue               AS CHARACTER         GET. SET.  
  DEFINE PUBLIC PROPERTY PrType                       AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY QuantityAuth                 AS INTEGER           GET. SET.

  DEFINE PUBLIC PROPERTY ErrorObject                  AS cls.maerrorobject GET. PRIVATE SET.         
  /* -- CONSTRUCTOR -------------------------------------------------- */                               
  CONSTRUCTOR PUBLIC basemaauthtypedetail() :                                                          
  /*------------------------------------------------------------------                                  
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypedetail */

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthtypedetail
    ( INPUT ipdAuthTypeDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthTypeDetail(ipdAuthTypeDetailObj).

    { mip/inc/mipcatcherror.i }
                              
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypedetail */  
  
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthtypedetail() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthTypeDataset  ) THEN DELETE OBJECT ghAuthTypeDataset.
    IF VALID-OBJECT( ErrorObject )        THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthtypedetail */
  
  /* -- METHOD focusAuthTypeDetail ----------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthTypeDetail
    ( INPUT ipdAuthTypeDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type detail record with the specified 
                    obj value and populate the instance properties with
                    the relevant values.
        Parameters: Auth type Detail obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess             = unfocusAuthTypeDetail()      /*Clear any properties*/
                              
        AuthTypeDetailObj     = ipdAuthTypeDetailObj
                              
        glSuccess             = getAuthTypeDetail()          /*Retrieve all data for the specified obj
                                                               and populate instance properties*/
        AuthTypeDetailInFocus = glSuccess.

    RETURN AuthTypeDetailInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthTypeDetail  */
  
  /* -- METHOD focusAuthTypeDetail ----------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthTypeDetail
    ( INPUT ipdAuthTypeObj           AS DECIMAL,
      INPUT ipdAuthTypeProviderObj   AS DECIMAL,
      INPUT ipiPrType                AS INTEGER,
      INPUT ipcOwningEntityMnemonic  AS CHARACTER,
      INPUT ipdOwningObj             AS DECIMAL,
      INPUT ipcOwningKey             AS CHARACTER,
      INPUT ipdDate                  AS DATE) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type detail record with the specified 
                    parameter values and populate the instance properties 
                    with the relevant values.
        Parameters: Auth Type Obj
                    Auth Type Provider Obj
                    Owning Entity Mnemonic
                    Owning Obj
                    Owning Key
                    Date 
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess               = unfocusAuthTypeDetail()    /*Clear any properties*/
                               
        AuthTypeObj             = ipdAuthTypeObj
        AuthTypeProviderObj     = ipdAuthTypeProviderObj
        PrType                  = ipiPrType
        OwningEntityMnemonic    = ipcOwningEntityMnemonic
        OwningObj               = ipdOwningObj  
        OwningKey               = ipcOwningKey  
        EffectiveDate           = ipdDate        
        glSuccess               = getAuthTypeDetail()        /*Retrieve all data for the specified obj
                                                               and populate instance properties*/
        AuthTypeDetailInFocus = glSuccess.

    RETURN AuthTypeDetailInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthTypeDetail  */

  
  /* -- METHOD getAuthTypeDetail --------------------------------------- */
  METHOD PRIVATE LOGICAL getAuthTypeDetail( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing auth type detail 
                    data using the auth type data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthtypesearch NO-UNDO.

    DATASET dsAuthType:EMPTY-DATASET().

    IF AuthTypeDetailObj <> 0.00            /* MH - what about AuthTypeProviderObj as well ? */
    OR AuthTypeObj       <> 0.00 THEN
    DO:
      oSearchObject = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).

      /*We arent interested in all of the buffers that have been defined in the auth type dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_type_detail":U).

      IF AuthTypeDetailObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.auth_type_detail_obj":U, "=":U, AuthTypeDetailObj).
        
      IF AuthTypeProviderObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.auth_type_provider_obj":U, "=":U, AuthTypeProviderObj).  /* MH correct ? */
        
      IF AuthTypeObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.auth_type_obj":U, "=":U, AuthTypeObj).

      IF PrType <> 0
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.pr_type":U, "=":U, PrType).

      IF  OwningEntityMnemonic <> "":U
      AND OwningEntityMnemonic <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.owning_entity_mnemonic":U, "<=":U, OwningEntityMnemonic).
      
      IF OwningObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.owning_obj":U, "=":U, OwningObj).
      
      IF OwningKey <> "":U
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.owning_key":U, "=":U, OwningKey).

      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_detail.effective_date":U, "<=":U, EffectiveDate).
      
      oSearchObject:fetchData().

    END. /*IF AuthTypeDetailObj <> 0.00 */

    FOR LAST tt_auth_type_detail NO-LOCK      
        WHERE tt_auth_type_detail.end_date >= EffectiveDate         
           OR tt_auth_type_detail.end_date  = ? : 

      ASSIGN
        AuthTypeDetailObj      = tt_auth_type_detail.auth_type_detail_obj
        AuthTypeObj            = tt_auth_type_detail.auth_type_obj       
        AuthTypeProviderObj    = tt_auth_type_detail.auth_type_provider_obj
        AuthAutoCreate         = tt_auth_type_detail.auth_auto_create
        AuthUsageLimit         = tt_auth_type_detail.auth_usage_limit 
        DefaultLineRestriction = tt_auth_type_detail.default_line_restriction
        DefaultAuthStatus      = tt_auth_type_detail.default_auth_status         
        DefaultAuthStatusNote  = tt_auth_type_detail.default_auth_status_note    
        DetailTypeIndicator    = tt_auth_type_detail.detail_type_indicator
        EffectiveDate          = tt_auth_type_detail.effective_date      
        EndDate                = tt_auth_type_detail.end_date           
        OwningEntityMnemonic   = tt_auth_type_detail.owning_entity_mnemonic
        OwningObj              = tt_auth_type_detail.owning_obj             
        OwningKey              = tt_auth_type_detail.owning_key     
        OwningAltValue         = tt_auth_type_detail.owning_alt_value
        PrType                 = tt_auth_type_detail.pr_type
        QuantityAuth           = tt_auth_type_detail.quantity_auth.

    END. /*FOR LAST tt_auth_type_detail NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_auth_type_detail NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthTypeDetail  */
  
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

    /* -- METHOD removeAuthTypeDetail ---------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthTypeDetail (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth type detail record.
        Parameters: None
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN
    DEFINE BUFFER btt_auth_type_detail FOR tt_auth_type_detail.

    IF NOT AuthTypeDetailInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authoristion Type Detail not in focus'"}

    FIND FIRST btt_auth_type_detail EXCLUSIVE-LOCK
         WHERE btt_auth_type_detail.auth_type_detail_obj = AuthTypeDetailObj.

    ASSIGN btt_auth_type_detail.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF
  END METHOD.  /* removeAuthTypeDetail  */
  
  /* -- METHOD saveAuthType -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthType() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth type detail record using the instance
                    properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthType:EMPTY-DATASET().

    DO TRANSACTION ON ERROR UNDO, THROW:

      CREATE tt_auth_type_detail.
  
      ASSIGN
        tt_auth_type_detail.line_number               = LineNumber
        tt_auth_type_detail.record_action             = "MODIFY":U
        tt_auth_type_detail.auth_type_detail_obj      = AuthTypeDetailObj       
        tt_auth_type_detail.auth_type_obj             = AuthTypeObj            
        tt_auth_type_detail.auth_type_provider_obj    = AuthTypeProviderObj    
        tt_auth_type_detail.auth_auto_create          = AuthAutoCreate             
        tt_auth_type_detail.auth_usage_limit          = AuthUsageLimit        
        tt_auth_type_detail.default_line_restriction  = DefaultLineRestriction 
        tt_auth_type_detail.default_auth_status       = DefaultAuthStatus      
        tt_auth_type_detail.default_auth_status_note  = DefaultAuthStatusNote      
        tt_auth_type_detail.detail_type_indicator     = DetailTypeIndicator   
        tt_auth_type_detail.effective_date            = EffectiveDate             
        tt_auth_type_detail.end_date                  = EndDate                
        tt_auth_type_detail.owning_entity_mnemonic    = OwningEntityMnemonic   
        tt_auth_type_detail.owning_obj                = OwningObj              
        tt_auth_type_detail.owning_key                = OwningKey                
        tt_auth_type_detail.owning_alt_value          = OwningAltValue
        tt_auth_type_detail.pr_type                   = PrType                 
        tt_auth_type_detail.quantity_auth             = QuantityAuth .

      VALIDATE tt_auth_type_detail.

    END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

    EMPTY TEMP-TABLE tt_temp_error.
    
    mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE) NO-ERROR.

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
      FOR FIRST tt_auth_type_detail NO-LOCK:
        ASSIGN
           AuthTypeDetailObj = tt_auth_type_detail.auth_type_detail_obj
           glSuccess         = THIS-OBJECT:focusAuthTypeDetail(AuthTypeDetailObj).
      END. /*FOR FIRST tt_auth_type_detail NO-LOCK:*/
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
  END METHOD.  /* saveAuthType  */
  
  /* -- METHOD unfocusAuthTypeDetail ----------------------------------- */
  METHOD PUBLIC LOGICAL unfocusAuthTypeDetail() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

    DATASET dsAuthType:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       AuthTypeDetailInFocus  = FALSE
       LineNumber             = 0
       AuthTypeDetailObj      = 0.00
       AuthTypeObj            = 0.00
       AuthTypeProviderObj    = 0.00
       AuthAutoCreate         = FALSE
       AuthUsageLimit         = 0
       DefaultLineRestriction = "":U
       DefaultAuthStatus      = "":U
       DefaultAuthStatusNote  = "":U
       DetailTypeIndicator    = "":U
       EffectiveDate          = ?
       EndDate                = ?
       OwningEntityMnemonic   = "":U         
       OwningObj              = 0.00
       OwningKey              = "":U
       OwningAltValue         = "":U
       PrType                 = 0
       QuantityAuth           = 0   .  
       
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* unfocusAuthTypeDetail  */
  
END CLASS.    /* ma.cls.basemaauthtypedetail */       


