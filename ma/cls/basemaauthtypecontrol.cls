/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandlam

      Purpose: Base Authorisation Type Control Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthtypecontrol INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthtypeds.i }
  
  DEFINE PRIVATE VARIABLE glSuccess               AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthTypeDataset       AS HANDLE  NO-UNDO.
                                                  
  DEFINE PUBLIC PROPERTY AuthTypeControlInFocus   AS LOGICAL           GET. PRIVATE SET.                        
                                                  
  DEFINE PUBLIC PROPERTY LineNumber               AS INTEGER           GET. SET.
  
  DEFINE PUBLIC PROPERTY ActivateAuthorisedValues AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY AgeRangeBothObj          AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AgeRangeFemaleObj        AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AgeRangeMaleObj          AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AmountAuth               AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeControlObj       AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeObj              AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeRestrictions     AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCodesHeader         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCodesDisallow       AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimTypesHeader         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimTypesDisallow       AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ControlTypeIndicator     AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatus        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatusNote    AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate            AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                  AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EnforceAuthorisedValues  AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY Gender                   AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj               AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY OptionCode               AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY Period                   AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY PeriodType               AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY PeriodOverride           AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY QuantityAuth             AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY UsageOverrideUser        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY UsagePeriod              AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY UsagePeriodType          AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY UsageQuantity            AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY UsageType                AS CHARACTER         GET. SET.
                                                  
  DEFINE PUBLIC PROPERTY ErrorObject              AS cls.maerrorobject GET. PRIVATE SET.

  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }
  
  /* -- CONSTRUCTOR -------------------------------------------------- */                               
  CONSTRUCTOR PUBLIC basemaauthtypecontrol() :                                                          
  /*------------------------------------------------------------------                                  
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypecontrol */

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthtypecontrol
    ( INPUT ipdAuthTypeControlObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthTypeControl(ipdAuthTypeControlObj).

    { mip/inc/mipcatcherror.i }
                              
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtypecontrol */  
  
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthtypecontrol() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthTypeDataset ) THEN DELETE OBJECT ghAuthTypeDataset.
    IF VALID-OBJECT( ErrorObject )       THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthtype */
  
  /* -- METHOD focusAuthTypeControl ---------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthTypeControl
    ( INPUT ipdAuthTypeControlObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type Control with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth type control obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess              = unfocusAuthTypeControl()    /*Clear any properties*/
                               
        AuthTypeControlObj     = ipdAuthTypeControlObj
                               
        glSuccess              = getAuthTypeControl()        /*Retrieve all data for the specified obj
                                                               and populate instance properties*/
        AuthTypeControlInFocus = glSuccess.

    RETURN AuthTypeControlInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthTypeControl  */
  
  /* -- METHOD focusAuthTypeControl ---------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthTypeControl
    ( INPUT ipdAuthTypeObj    AS DECIMAL,
      INPUT ipdInsurerObj     AS DECIMAL,
      INPUT ipiOptionCode     AS INTEGER,
      INPUT ipcControlTypeInd AS CHARACTER,
      INPUT ipdEffectiveDate  AS DATE) :
  /*------------------------------------------------------------------
        Purpose   : Find an Auth Type Control with the specified auth
                    type obj, insurer obj, option code and effective 
                    date and populate the instance properties with
                    the relevant values.
        Parameters: Auth type control obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess              = unfocusAuthTypeControl()    /*Clear any properties*/
                               
        AuthTypeObj            = ipdAuthTypeObj
        
        InsurerObj             = ipdInsurerObj
        
        OptionCode             = ipiOptionCode

        ControlTypeIndicator   = ipcControlTypeInd
        
        EffectiveDate          = ipdEffectiveDate
                               
        glSuccess              = getAuthTypeControl()      /*Retrieve all data for the specified obj
                                                             and populate instance properties*/
        AuthTypeControlInFocus = glSuccess.

    RETURN AuthTypeControlInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthTypeControl  */
  
  /* -- METHOD getAuthTypeControl --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthTypeControl( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing auth type data
                    using the auth type data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthtypesearch NO-UNDO.

    DATASET dsAuthType:EMPTY-DATASET().

    IF AuthTypeControlObj <> 0.00 OR 
       AuthTypeObj        <> 0.000 
    THEN DO:
      oSearchObject = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).

      /*We arent interested in all of the buffers that have been defined in the auth type dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_type,tt_auth_type_control":U).

      IF AuthTypeControlObj <> 0.00
      THEN ASSIGN glSuccess  = oSearchObject:SetFilterCriteria("tt_auth_type_control.auth_type_control_obj":U, "=":U, AuthTypeControlObj).
        
      IF AuthTypeObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_control.auth_type_obj":U, "=":U, AuthTypeObj).
      
      IF InsurerObj <> 0.00
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_control.insurer_obj":U, "=":U, InsurerObj).
      
      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_control.effective_date":U, "=":U, EffectiveDate).
      
      IF OptionCode <> 0
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_control.option_code":U, "=":U, OptionCode).

      IF ControlTypeIndicator <> "":U
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type_control.control_type_ind":U, "=":U, ControlTypeIndicator).

      oSearchObject:fetchData().

    END. /*IF AuthTypeControlObj <> 0.00 */

    FOR FIRST tt_auth_type_control NO-LOCK:

      ASSIGN
        ActivateAuthorisedValues = tt_auth_type_control.activate_authorised_values
        AgeRangeBothObj          = tt_auth_type_control.age_range_both_obj
        AgeRangeFemaleObj        = tt_auth_type_control.age_range_female_obj
        AgeRangeMaleObj          = tt_auth_type_control.age_range_male_obj
        AmountAuth               = tt_auth_type_control.amount_auth         
        AuthTypeControlObj       = tt_auth_type_control.auth_type_control_obj
        AuthTypeObj              = tt_auth_type_control.auth_type_obj       
        AuthTypeRestrictions     = tt_auth_type_control.auth_type_restrictions        
        ClaimCodesHeader         = tt_auth_type_control.claim_codes_header   
        ClaimCodesDisallow       = tt_auth_type_control.claim_codes_disallow
        ClaimTypesHeader         = tt_auth_type_control.claim_types_header
        ClaimTypesDisallow       = tt_auth_type_control.claim_types_disallow
        ControlTypeIndicator     = tt_auth_type_control.control_type_ind        
        DefaultAuthStatus        = tt_auth_type_control.default_auth_status
        DefaultAuthStatusNote    = tt_auth_type_control.default_auth_status_note
        EffectiveDate            = tt_auth_type_control.effective_date      
        EndDate                  = tt_auth_type_control.end_date  
        EnforceAuthorisedValues  = tt_auth_type_control.enforce_authorised_values 
        Gender                   = tt_auth_type_control.gender  
        InsurerObj               = tt_auth_type_control.insurer_obj         
        OptionCode               = tt_auth_type_control.option_code         
        Period                   = tt_auth_type_control.period              
        PeriodType               = tt_auth_type_control.period_type         
        PeriodOverride           = tt_auth_type_control.period_override     
        QuantityAuth             = tt_auth_type_control.quantity_auth            
        UsageOverrideUser        = tt_auth_type_control.usage_override_user      
        UsagePeriod              = tt_auth_type_control.usage_period        
        UsagePeriodType          = tt_auth_type_control.usage_period_type   
        UsageQuantity            = tt_auth_type_control.usage_quantity      
        UsageType                = tt_auth_type_control.usage_type.        
         
    END. /*FOR FIRST tt_auth_type NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_auth_type_control NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthTypeControl  */
  
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

    /* -- METHOD removeAuthTypeControl --------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthTypeControl (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth type control record.
        Parameters: None
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN
    DEFINE BUFFER btt_auth_type_control FOR tt_auth_type_control.


    IF NOT AuthTypeControlInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authoristion Type Control not in focus'"}

    FIND FIRST btt_auth_type_control EXCLUSIVE-LOCK
         WHERE btt_auth_type_control.auth_type_control_obj = AuthTypeControlObj.

    ASSIGN btt_auth_type_control.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF
  END METHOD.  /* removeAuthTypeControl  */
  
  /* -- METHOD saveAuthTypeControl --------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthType() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth type record using the instance
                    properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthType:EMPTY-DATASET().

    DO TRANSACTION ON ERROR UNDO, THROW:

      CREATE tt_auth_type_control.
      
      ASSIGN
         tt_auth_type_control.line_number                = LineNumber
         tt_auth_type_control.record_action              = "MODIFY":U
         tt_auth_type_control.activate_authorised_values = ActivateAuthorisedValues  
         tt_auth_type_control.age_range_both_obj         = AgeRangeBothObj
         tt_auth_type_control.age_range_female_obj       = AgeRangeFemaleObj
         tt_auth_type_control.age_range_male_obj         = AgeRangeMaleObj
         tt_auth_type_control.auth_type_control_obj      = AuthTypeControlObj    
         tt_auth_type_control.auth_type_obj              = AuthTypeObj          
         tt_auth_type_control.claim_codes_header         = ClaimCodesHeader     
         tt_auth_type_control.claim_types_header         = ClaimTypesHeader        
         tt_auth_type_control.control_type_indicator     = ControlTypeIndicator
         tt_auth_type_control.default_auth_status        = DefaultAuthStatus
         tt_auth_type_control.default_auth_status_note   = DefaultAuthStatusNote
         tt_auth_type_control.effective_date             = EffectiveDate         
         tt_auth_type_control.end_date                   = EndDate 
         tt_auth_type_control.enforce_authorised_values  = EnforceAuthorisedValues             
         tt_auth_type_control.gender                     = Gender
         tt_auth_type_control.period                     = Period               
         tt_auth_type_control.period_type                = PeriodType           
         tt_auth_type_control.period_override            = PeriodOverride       
         tt_auth_type_control.amount_auth                = AmountAuth           
         tt_auth_type_control.quantity_auth              = QuantityAuth               
         tt_auth_type_control.option_code                = OptionCode           
         tt_auth_type_control.insurer_obj                = InsurerObj           
         tt_auth_type_control.claim_codes_disallow       = ClaimCodesDisallow   
         tt_auth_type_control.claim_types_disallow       = ClaimTypesDisallow   
         tt_auth_type_control.usage_quantity             = UsageQuantity        
         tt_auth_type_control.usage_period               = UsagePeriod          
         tt_auth_type_control.usage_period_type          = UsagePeriodType      
         tt_auth_type_control.usage_override_user        = UsageOverrideUser        
         tt_auth_type_control.auth_type_restrictions     = AuthTypeRestrictions          
         tt_auth_type_control.usage_type                 = UsageType.         
      
      VALIDATE tt_auth_type_control.

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
      FOR FIRST tt_auth_type_control NO-LOCK:
        ASSIGN
           AuthTypeControlObj = tt_auth_type_control.auth_type_control_obj
           glSuccess          = THIS-OBJECT:focusAuthTypeControl(AuthTypeControlObj).
      END. /*FOR FIRST tt_auth_type_control NO-LOCK:*/
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
  END METHOD.  /* saveAuthTypeControl */
  
  /* -- METHOD unfocusAuthTypeControl ---------------------------------- */
  METHOD PUBLIC LOGICAL unfocusAuthTypeControl() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

    DATASET dsAuthType:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       LineNumber               = 0
       ActivateAuthorisedValues = FALSE
       AgeRangeBothObj          = 0.00
       AgeRangeMaleObj          = 0.00
       AgeRangeFemaleObj        = 0.00
       AmountAuth               = 0.00
       AuthTypeControlInFocus   = FALSE
       AuthTypeControlObj       = 0.00
       AuthTypeObj              = 0.00
       AuthTypeRestrictions     = "":U
       ClaimCodesHeader         = "":U
       ClaimCodesDisallow       = "":U
       ClaimTypesHeader         = "":U
       ClaimTypesDisallow       = "":U
       ControlTypeIndicator     = "":U
       EffectiveDate            = ?
       EndDate                  = ?
       EnforceAuthorisedValues  = FALSE
       DefaultAuthStatus        = "":U
       DefaultAuthStatusNote    = "":U
       Gender                   = "":U
       InsurerObj               = 0.00
       OptionCode               = 0
       Period                   = 0
       PeriodType               = "":U
       PeriodOverride           = FALSE       
       QuantityAuth             = 0
       UsageOverrideUser        = "":U
       UsagePeriod              = 0
       UsagePeriodType          = "":U
       UsageQuantity            = 0
       UsageType                = "":U.
       
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* unfocusAuthTypeControl  */
  
END CLASS.    /* ma.cls.basemaauthtypecontrol */ 
