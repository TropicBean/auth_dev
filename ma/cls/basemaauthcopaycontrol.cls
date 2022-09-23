/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: MMP

      Purpose: Auth Copay Control Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthcopaycontrol INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

&IF {&DBDFMA} >= 010195 &THEN

  { ma/inc/maauthcopayds.i }
                                                  
                                                  
  DEFINE PRIVATE VARIABLE glSuccess                          AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthCopayControlDataset          AS HANDLE  NO-UNDO.
                                                             
  DEFINE PUBLIC PROPERTY AuthCopayControlInFocus             AS LOGICAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthCopayControlObj                 AS DECIMAL           GET. SET.                                                            
  DEFINE PUBLIC PROPERTY LineNumber                          AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj                          AS DECIMAL           GET. SET.         
  DEFINE PUBLIC PROPERTY OptionCode                          AS INTEGER           GET. SET. 
  DEFINE PUBLIC PROPERTY AuthCopayTypeObj                    AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate                       AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                             AS DATE              GET. SET. 
  DEFINE PUBLIC PROPERTY CopaymentValueType                  AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY CopaymentValue                      AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY ApplyToPmb                          AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ApplyToEmergency                    AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatus                          AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatusNote                      AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY CopayApplyOverrideReasons           AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY WarningMessage                      AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY WarningMessageType                  AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ProviderType                        AS CHARACTER         GET. SET.

&ENDIF

  DEFINE PUBLIC PROPERTY ErrorObject                         AS cls.maerrorobject GET. PRIVATE SET.
    
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaycontrol() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaycontrol */

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaycontrol
    ( INPUT ipdAuthCopayControlObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    THIS-OBJECT:focusAuthCopayControl(ipdAuthCopayControlObj).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaycontrol */

  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthcopaycontrol() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN
    IF VALID-HANDLE( ghauthcopayControlDataset  ) THEN DELETE OBJECT ghauthcopayControlDataset.    
&ENDIF

    IF VALID-OBJECT( ErrorObject )               THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthcopaycontrol */

  /* -- METHOD focusAuthCopayControl ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthCopayControl
    ( INPUT ipdAuthCopayControlObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an Auth Copay control with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth Copay control obj
  -------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN

    ASSIGN
        glSuccess              = unfocusAuthCopayControl()    /* Clear any properties */

        AuthCopayControlObj    = ipdAuthCopayControlObj

        glSuccess              = THIS-OBJECT:getAuthCopayControl()  /* Retrieve all data for the specified obj
                                                                       and populate instance properties */
        AuthCopayControlInFocus = glSuccess.

    RETURN AuthCopayControlInFocus.

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* focusAuthCopayControl  */
  
  /* -- METHOD focusAuthCopayControl ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthCopayControl
   (INPUT ipdInsurerObj        AS DECIMAL, 
    INPUT ipiOptionCode        AS INTEGER, 
    INPUT ipcProviderType      AS CHARACTER, 
    INPUT ipdEffectiveDate     AS DATE):
  /*------------------------------------------------------------------
        Purpose   : Find an Auth Copay control with the specified insurer obj, 
                    option code, Auth Copay Type obj and effective date and 
                    populate the instance properties with the relevant values.
        Parameters: Insurer Obj, Option Code, Auth Copay Type Obj and Effective Date
  -------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    ASSIGN
        glSuccess        = unfocusAuthCopayControl()          /* Clear any properties */

        InsurerObj       = ipdInsurerObj

        OptionCode       = ipiOptionCode
        
        ProviderType     = ipcProviderType
    
        EffectiveDate    = ipdEffectiveDate

        glSuccess        = THIS-OBJECT:getAuthCopayControl()  /* Retrieve all data for the specified Auth Copay control and populate instance properties */
        
        AuthCopayControlInFocus = glSuccess.

    RETURN AuthCopayControlInFocus.

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* focusAuthCopayControl */

  /* -- METHOD getAuthCopayControl --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthCopayControl( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieving Auth Copay control data
                    using the Auth Copay control data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DEFINE VARIABLE oSearchObject AS cls.maauthcopaysearch NO-UNDO.

    DATASET dsAuthCopayControl:EMPTY-DATASET().

    IF AuthCopayControlObj <> 0.00 THEN 
    DO:
      oSearchObject = NEW cls.maauthcopaysearch(DATASET dsAuthCopayControl BY-REFERENCE).

      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_copay_control":U).

      IF AuthCopayControlObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_control.auth_copay_control_obj":U, "=":U, AuthCopayControlObj).

      IF InsurerObj <> 0 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_control.insurer_obj":U, "=":U, InsurerObj).

      IF OptionCode <> 0
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_control.option_code":U, "=":U, OptionCode).

      IF AuthCopayTypeObj <> 0
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_control.auth_copay_type_obj":U, "=":U, AuthCopayTypeObj).

      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_control.effective_date":U, "<=":U, EffectiveDate).
      
      oSearchObject:fetchData().

    END. /* IF AuthCopayControlObj <> 0.00 */

    FOR FIRST tt_auth_copay_control EXCLUSIVE-LOCK:

      ASSIGN
        AuthCopayControlObj       = tt_auth_copay_control.auth_copay_control_obj                            
        InsurerObj                = tt_auth_copay_control.insurer_obj
        OptionCode                = tt_auth_copay_control.option_code
        ProviderType              = tt_auth_copay_control.provider_type
        EffectiveDate             = tt_auth_copay_control.effective_date
        EndDate                   = tt_auth_copay_control.end_date
        CopaymentValueType        = tt_auth_copay_control.copayment_value_type
        CopaymentValue            = tt_auth_copay_control.copayment_value
        ApplyToPmb                = tt_auth_copay_control.apply_to_pmb
        ApplyToEmergency          = tt_auth_copay_control.apply_to_emergency
        AuthStatus                = tt_auth_copay_control.auth_status
        AuthStatusNote            = tt_auth_copay_control.auth_status_note
        CopayApplyOverrideReasons = tt_auth_copay_control.copay_apply_override_reasons
        WarningMessage            = tt_auth_copay_control.warning_message
        WarningMessageType        = tt_auth_copay_control.warning_message_type
        . 
               
    END. /* FOR FIRST tt_auth_copay_control NO-LOCK: */

    RETURN CAN-FIND(FIRST tt_auth_copay_control NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

&ENDIF

  END METHOD.  /* getAuthCopayControl  */

  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    ASSIGN
       ghAuthCopayControlDataset = DATASET dsAuthCopayControl:HANDLE

       ErrorObject               = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* initializeObject  */

  /* -- METHOD removeAuthCopayControl ------------------------------------------ */
  METHOD PUBLIC LOGICAL removeAuthCopayControl (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused Auth Copay Control record.
        Parameters: None
  ---------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN

    DEFINE BUFFER btt_auth_copay_control FOR tt_auth_copay_control.

    IF NOT AuthCopayControlInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth Copay Control not in focus'"}

    FIND FIRST btt_auth_copay_control EXCLUSIVE-LOCK
         WHERE btt_auth_copay_control.auth_copay_control_obj = AuthCopayControlObj.

    ASSIGN btt_auth_copay_control.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveauthcopay(INPUT-OUTPUT DATASET dsAuthCopayControl BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* removeAuthCopayControl  */

  /* -- METHOD saveAuthCopayControl -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthCopayControl() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update Auth Copay control record using the 
                    instance properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthCopayControl:EMPTY-DATASET().

    CREATE tt_auth_copay_control.

    ASSIGN
      tt_auth_copay_control.auth_copay_control_obj       = AuthCopayControlObj
      tt_auth_copay_control.line_number                  = LineNumber
      tt_auth_copay_control.record_action                = "MODIFY":U
      tt_auth_copay_control.provider_type                = ProviderType
      tt_auth_copay_control.insurer_obj                  = InsurerObj
      tt_auth_copay_control.option_code                  = OptionCode
      tt_auth_copay_control.auth_copay_type_obj          = AuthCopayTypeObj
      tt_auth_copay_control.effective_date               = EffectiveDate
      tt_auth_copay_control.end_date                     = EndDate
      tt_auth_copay_control.copayment_value_Type         = CopaymentValueType       
      tt_auth_copay_control.copayment_value              = CopaymentValue           
      tt_auth_copay_control.apply_to_pmb                 = ApplyToPmb               
      tt_auth_copay_control.apply_to_emergency           = ApplyToEmergency         
      tt_auth_copay_control.auth_Status                  = AuthStatus               
      tt_auth_copay_control.auth_status_note             = AuthStatusNote           
      tt_auth_copay_control.copay_apply_override_reasons = CopayApplyOverrideReasons
      tt_auth_copay_control.warning_message              = WarningMessage           
      tt_auth_copay_control.warning_message_type         = WarningMessageType
      .  

    VALIDATE tt_auth_copay_control.
    
    mipEnv:Health:AuthMaintenance:saveAuthCopay(INPUT-OUTPUT DATASET dsAuthCopayControl BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_copay_control NO-LOCK:
        ASSIGN
           AuthCopayControlObj = tt_auth_copay_control.auth_copay_control_obj
           glSuccess           = THIS-OBJECT:focusAuthCopayControl(AuthCopayControlObj).
      END. /*FOR FIRST tt_auth_copay_control NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* saveAuthCopayControl  */

  /* -- METHOD unfocusAuthCopayControl ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthCopayControl() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthCopayControl:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       AuthCopayControlInFocus   = FALSE
       LineNumber                = 0
       AuthCopayControlObj       = 0
       InsurerObj                = 0
       OptionCode                = 0
       AuthCopayTypeObj          = 0
       EffectiveDate             = ?
       EndDate                   = ?
       CopaymentValueType        = ?
       CopaymentValue            = 0
       ApplyToPmb                = ?
       ApplyToEmergency          = ?
       AuthStatus                = 0
       AuthStatusNote            = "":U
       CopayApplyOverrideReasons = "":U
       WarningMessage            = "":U
       WarningMessageType        = "":U
       ProviderType              = "":U
      .
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* unfocusAuthCopayControl  */

END CLASS.  /* mip.cls.basemaauthcopaycontrol */

