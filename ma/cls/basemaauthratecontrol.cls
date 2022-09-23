/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Laetitia

      Purpose: Auth Rate Control Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthratecontrol INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

&IF {&DBDFMA} >= 010195 &THEN

  { ma/inc/maauthratecontrolds.i }
                                                  
                                                  
  DEFINE PRIVATE VARIABLE glSuccess                          AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthRateControlDataset           AS HANDLE  NO-UNDO.
                                                             
  DEFINE PUBLIC PROPERTY AuthRateControlInFocus              AS LOGICAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthRateControlObj                  AS DECIMAL           GET. SET.                                                            
  DEFINE PUBLIC PROPERTY LineNumber                          AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj                          AS DECIMAL           GET. SET.         
  DEFINE PUBLIC PROPERTY OptionCode                          AS INTEGER           GET. SET. 
  DEFINE PUBLIC PROPERTY RateControlCode                     AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY RateControlDescription              AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY MainProvNegNum                      AS INTEGER           GET. SET.       
  DEFINE PUBLIC PROPERTY MainProvBaseRate                    AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY MainProvArsRate                     AS CHARACTER         GET. SET.     
  DEFINE PUBLIC PROPERTY EffectiveDate                       AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                             AS DATE              GET. SET. 
  DEFINE PUBLIC PROPERTY OverrideArsRate                     AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY OverrideBaseRate                    AS CHARACTER         GET. SET. 
  DEFINE PUBLIC PROPERTY PrimaryCode                         AS LOGICAL           GET. SET. 
  DEFINE PUBLIC PROPERTY MainCode                            AS LOGICAL           GET. SET. 
  DEFINE PUBLIC PROPERTY CodeLinkCategory                    AS CHARACTER         GET. SET.   
  DEFINE PUBLIC PROPERTY RateChangeMessage                   AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY WorkGroupObj                        AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AssociatedPrTypeList                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY WarningMessage                      AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY RevertRateChange                    AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY BodyRegionExclusion                 AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OverrideAuthTypeObj                 AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY CptRelativeValueType                AS CHARACTER         GET. SET.

&ENDIF

  DEFINE PUBLIC PROPERTY ErrorObject                         AS cls.maerrorobject GET. PRIVATE SET.
    

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthratecontrol() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratecontrol */


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthratecontrol
    ( INPUT ipdAuthRateControlObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    THIS-OBJECT:focusAuthRateControl(ipdAuthRateControlObj).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratecontrol */


  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthratecontrol() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN
    IF VALID-HANDLE( ghAuthRateControlDataset  ) THEN DELETE OBJECT ghAuthRateControlDataset.    
&ENDIF

    IF VALID-OBJECT( ErrorObject )               THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthratecontrol */


  /* -- METHOD focusAuthRateControl ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthRateControl
    ( INPUT ipdAuthRateControlObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth rate control with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth rate control obj
  -------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN

    ASSIGN
        glSuccess              = unfocusAuthRateControl()    /*Clear any properties*/

        AuthRateControlObj     = ipdAuthRateControlObj

        glSuccess              = THIS-OBJECT:getAuthRateControl()  /* Retrieve all data for the specified obj
                                                                      and populate instance properties*/
        AuthRateControlInFocus = glSuccess.

    RETURN AuthRateControlInFocus.

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* focusAuthRateControl  */
  
  /* -- METHOD focusAuthRateControl ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthRateControl
   (INPUT ipdInsurerObj       AS DECIMAL, 
    INPUT ipiOptionCode       AS INTEGER, 
    INPUT ipcRateControlCode  AS CHARACTER, 
    INPUT ipiMainProvNegNum   AS INTEGER, 
    INPUT ipcMainProvBaseRate AS CHARACTER, 
    INPUT ipcMainProvArsRate  AS CHARACTER,  
    INPUT ipdEffectiveDate    AS DATE):
  /*------------------------------------------------------------------
        Purpose   : Find an auth rate control with the specified insurer obj, 
                    option code, rate control code, main provider negotiator
                    number, main provider base rate, main provider ars rate
                    and effective date and populate the instance properties 
                    with the relevant values.
        Parameters: Insurer Obj, Option Code, Rate Control Code, Main Provider
                    Negotiator Number, Main Provider Base Rate, Main Provider
                    Ars Rate and Effective Date
  -------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    ASSIGN
        glSuccess        = unfocusAuthRateControl()    /*Clear any properties*/

        InsurerObj       = ipdInsurerObj

        OptionCode       = ipiOptionCode
        
        RateControlCode  = ipcRateControlCode
    
        MainProvNegNum   = ipiMainProvNegNum

        MainProvBaseRate = ipcMainProvBaseRate

        MainProvArsRate  = ipcMainProvArsRate
        
        EffectiveDate    = ipdEffectiveDate

        glSuccess        = THIS-OBJECT:getAuthRateControl()  /* Retrieve all data for the specified auth rate control and populate instance properties*/
        
        AuthRateControlInFocus = glSuccess.

    RETURN AuthRateControlInFocus.

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* focusAuthRateControl  */


  /* -- METHOD getAuthRateControl --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthRateControl( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieving auth rate control data
                    using the auth rate control data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DEFINE VARIABLE oSearchObject AS cls.maauthratesearch NO-UNDO.

    DATASET dsAuthRateControl:EMPTY-DATASET().

    IF AuthRateControlObj <> 0.00 OR 
       RateControlCode    <> "" 
    THEN DO:
      oSearchObject = NEW cls.maauthratesearch(DATASET dsAuthRateControl BY-REFERENCE).

      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_rate_control":U).

      IF AuthRateControlObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.auth_rate_control_obj":U, "=":U, AuthRateControlObj).

      IF InsurerObj <> 0 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.insurer_obj":U, "=":U, InsurerObj).

      IF OptionCode <> 0
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.option_code":U, "=":U, OptionCode).

      IF RateControlCode <> ""
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.rate_control_code":U, "=":U, RateControlCode).

      IF MainProvNegNum <> 0
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.main_provider_neg_num":U, "=":U,  MainProvNegNum).

      IF MainProvBaseRate <> ""
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.main_provider_base_rate":U, "=":U, MainProvBaseRate).

      IF MainProvArsRate <> ""
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.main_provider_ars_rate":U, "=":U, MainProvArsRate).    
      
      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_rate_control.effective_date":U, "<=":U, EffectiveDate).
      
      oSearchObject:fetchData().

    END. /*IF AuthRateControlObj <> 0.00 */

    FOR FIRST tt_auth_rate_control EXCLUSIVE-LOCK:

      ASSIGN
        AuthRateControlObj     = tt_auth_rate_control.auth_rate_control_obj                            
        InsurerObj             = tt_auth_rate_control.insurer_obj
        OptionCode             = tt_auth_rate_control.option_code
        RateControlCode        = tt_auth_rate_control.rate_control_code
        RateControlDescription = tt_auth_rate_control.rate_control_description
        MainProvNegNum         = tt_auth_rate_control.main_provider_neg_num
        MainProvBaseRate       = tt_auth_rate_control.main_provider_base_rate
        MainProvArsRate        = tt_auth_rate_control.main_provider_ars_rate
        EffectiveDate          = tt_auth_rate_control.effective_date
        EndDate                = tt_auth_rate_control.end_date
        OverrideArsRate        = tt_auth_rate_control.override_ars_rate
        OverrideBaseRate       = tt_auth_rate_control.override_base_rate
        PrimaryCode            = tt_auth_rate_control.primary_code
        MainCode               = tt_auth_rate_control.main_code
        CodeLinkCategory       = tt_auth_rate_control.code_link_category
        RateChangeMessage      = tt_auth_rate_control.rate_change_message
        WorkGroupObj           = tt_auth_rate_control.workgroup_obj
        AssociatedPrTypeList   = tt_auth_rate_control.associated_pr_type_list
        WarningMessage         = tt_auth_rate_control.warning_message
        RevertRateChange       = tt_auth_rate_control.revert_rate_change
        BodyRegionExclusion    = tt_auth_rate_control.body_region_exclusion
        OverrideAuthTypeObj    = tt_auth_rate_control.override_auth_type_obj
        CptRelativeValueType        = tt_auth_rate_control.cpt_relative_value_type
        . 
               
    END. /*FOR FIRST tt_auth_rate_control NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_auth_rate_control NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

&ENDIF

  END METHOD.  /* getAuthTypeDetails  */


  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    ASSIGN
       ghAuthRateControlDataset  = DATASET dsAuthRateControl:HANDLE

       ErrorObject               = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE).

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* initializeObject  */


  /* -- METHOD removeAuthRateControl ------------------------------------------ */
  METHOD PUBLIC LOGICAL removeAuthRateControl (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth rate control record.
        Parameters: None
  ---------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN

    DEFINE BUFFER btt_auth_rate_control FOR tt_auth_rate_control.

    IF NOT AuthRateControlInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth rate control not in focus'"}

    FIND FIRST btt_auth_rate_control EXCLUSIVE-LOCK
         WHERE btt_auth_rate_control.auth_rate_control_obj = AuthRateControlObj.

    ASSIGN btt_auth_rate_control.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthRate(INPUT-OUTPUT DATASET dsAuthRateControl BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* removeAuthRateControl  */


  /* -- METHOD saveAuthType -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthRateControl() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth rate control record using the 
                    instance properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthRateControl:EMPTY-DATASET().

    CREATE tt_auth_rate_control.

    ASSIGN
      tt_auth_rate_control.line_number              = LineNumber
      tt_auth_rate_control.record_action            = "MODIFY":U
      tt_auth_rate_control.auth_rate_control_obj    = AuthRateControlObj
      tt_auth_rate_control.insurer_obj              = InsurerObj
      tt_auth_rate_control.option_code              = OptionCode
      tt_auth_rate_control.rate_control_code        = RateControlCode
      tt_auth_rate_control.rate_control_description = RateControlDescription
      tt_auth_rate_control.main_provider_neg_num    = MainProvNegNum
      tt_auth_rate_control.main_provider_base_rate  = MainProvBaseRate
      tt_auth_rate_control.main_provider_ars_rate   = MainProvArsRate
      tt_auth_rate_control.effective_date           = EffectiveDate
      tt_auth_rate_control.end_date                 = EndDate
      tt_auth_rate_control.override_ars_rate        = OverrideArsRate
      tt_auth_rate_control.override_base_rate       = OverrideBaseRate
      tt_auth_rate_control.primary_code             = PrimaryCode
      tt_auth_rate_control.main_code                = MainCode
      tt_auth_rate_control.code_link_category       = CodeLinkCategory
      tt_auth_rate_control.rate_change_message      = RateChangeMessage
      tt_auth_rate_control.workgroup_obj            = WorkGroupObj
      tt_auth_rate_control.associated_pr_type_list  = AssociatedPrTypeList
      tt_auth_rate_control.warning_message          = WarningMessage
      tt_auth_rate_control.revert_rate_change       = RevertRateChange
      tt_auth_rate_control.body_region_exclusion    = BodyRegionExclusion
      tt_auth_rate_control.override_auth_type_obj   = OverrideAuthTypeObj
      tt_auth_rate_control.cpt_relative_value_type  = CptRelativeValueType
      .  

    VALIDATE tt_auth_rate_control.
    
    mipEnv:Health:AuthMaintenance:saveAuthRate(INPUT-OUTPUT DATASET dsAuthRateControl BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_rate_control NO-LOCK:
        ASSIGN
           AuthRateControlObj = tt_auth_rate_control.auth_rate_control_obj
           glSuccess          = THIS-OBJECT:focusAuthRateControl(AuthRateControlObj).
      END. /*FOR FIRST tt_auth_rate_control NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* saveAuthType  */


  /* -- METHOD unfocusAuthRateControl ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthRateControl() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthRateControl:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       AuthRateControlInFocus       = FALSE
       LineNumber                   = 0
       AuthRateControlObj           = 0.00
       InsurerObj                   = 0.00
       OptionCode                   = 0
       MainProvNegNum               = 0
       EffectiveDate                = ?
       EndDate                      = ?
       PrimaryCode                  = ?
       MainCode                     = ?
       WorkGroupObj                 = 0.00
       RevertRateChange             = ?
       OverrideAuthTypeObj          = 0.00
       AssociatedPrTypeList         = "":U
       CodeLinkCategory             = "":U
       BodyRegionExclusion          = "":U
       CptRelativeValueType              = "":U
       MainProvBaseRate             = "":U
       MainProvArsRate              = "":U       
       OverrideArsRate              = "":U
       OverrideBaseRate             = "":U
       RateChangeMessage            = "":U
       RateControlCode              = "":U   
       RateControlDescription       = "":U   
       WarningMessage               = "":U.
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* unfocusAuthRateControl  */

END CLASS.  /* mip.cls.basemaauthratecontrol */
