
/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandlam

      Purpose: Base Auth Flag Value Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthflagvalue INHERITS cls.mip:

  { sysadmma.i } 
  
  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthflagvalueds.i }
  
  
  DEFINE PRIVATE VARIABLE glSuccess                 AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthFlagValueDataset    AS HANDLE  NO-UNDO.
  
  DEFINE PUBLIC PROPERTY AuthFlagValueInFocus       AS LOGICAL                 GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthFlagValueObj           AS DECIMAL                 GET. PRIVATE SET.
  
  DEFINE PUBLIC PROPERTY AuthFlagValue              AS CHARACTER       NO-UNDO GET. SET. 
  DEFINE PUBLIC PROPERTY LastChangeDateTime         AS DATETIME        NO-UNDO GET. SET. 
  DEFINE PUBLIC PROPERTY OverrideNote               AS CHARACTER       NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningAltValue             AS CHARACTER       NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic       AS CHARACTER       NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningKey                  AS CHARACTER       NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj                  AS DECIMAL         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthRuleObj                AS DECIMAL         NO-UNDO GET. SET.
  
  DEFINE PUBLIC PROPERTY ErrorObject                AS cls.maerrorobject       GET. PRIVATE SET.

  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthflagvalue() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthflagvalue */

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthflagvalue
    ( INPUT ipdAuthFlagValueObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthFlagValue(ipdAuthFlagValueObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthflagvalue */
  
                                                           
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthflagvalue() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
    
    IF VALID-HANDLE( ghAuthFlagValueDataset )  THEN DELETE OBJECT ghAuthFlagValueDataset.
    IF VALID-OBJECT( ErrorObject )             THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaAuthflagvalue */

  
  /* -- METHOD focusAuthFlagValue ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthFlagValue
    ( INPUT ipdAuthFlagValueObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth flag value with the specified obj 
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth flag Value Obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess              = unfocusAuthFlagValue()    /*Clear any properties*/
        
        AuthFlagValueObj       = ipdAuthFlagValueObj
        
        glSuccess              = getAuthFlagValueDetails()
        
        AuthFlagValueInFocus   = glSuccess.
        
    RETURN AuthFlagValueInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthFlagValue  */
  
  /* -- METHOD focusAuthFlagValue ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthFlagValue
    ( INPUT ipcOwningEntityMnemonic AS CHARACTER,
      INPUT ipdOwningObj            AS DECIMAL,
      INPUT ipcOwningKey            AS CHARACTER,
      INPUT ipcOwningAltValue       AS CHARACTER,
      INPUT ipdAuthRuleObj          AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth flag value with the specified owning 
                    values and auth rule obj and populate the instance 
                    properties with the relevant values.
        Parameters: 
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess              = unfocusAuthFlagValue()    /*Clear any properties*/

        OwningEntityMnemonic   = ipcOwningEntityMnemonic
        OwningObj              = ipdOwningObj
        OwningKey              = ipcOwningKey
        OwningAltValue         = ipcOwningAltValue
        AuthRuleObj            = ipdAuthRuleObj

        glSuccess              = getAuthFlagValueDetails()

        AuthFlagValueInFocus   = glSuccess.

    RETURN AuthFlagValueInFocus.    

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthFlagValue  */
  
  /* -- METHOD getAuthFlagValueDetails --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthFlagValueDetails(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing Auth Flag Value data
                    using the Auth Flag Value data retrieval services.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthflagvaluesearch NO-UNDO.
    
    DATASET dsAuthFlagValue:EMPTY-DATASET().
    
    IF AuthFlagValueObj <> 0 THEN
    DO:
      oSearchObject = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE).
     
      /*We arent interested in all of the buffers that have been defined in the Auth Flag Value dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_flag_value":U).
      
      IF AuthFlagValueObj <> 0 THEN 
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_flag_value.auth_flag_value_obj":U, "=":U, AuthFlagValueObj).
      
      IF OwningEntityMnemonic <> "" THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, OwningEntityMnemonic).

      IF OwningObj <> 0 THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_flag_value.owning_obj":U, "=":U, OwningObj).

      IF OwningKey <> "":U THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_flag_value.owning_key":U, "=":U, OwningKey).

      IF OwningAltValue <> "":U THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_flag_value.owning_alt_value":U, "=":U, OwningAltValue).

      IF AuthRuleObj <> 0 THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_flag_value.auth_rule_obj":U, "=":U, AuthRuleObj).
      
      oSearchObject:fetchdata().
      
    END. /*IF AuthFlagValueObj <> "" */
    
    FOR FIRST tt_auth_flag_value NO-LOCK:
    
      ASSIGN 
         AuthFlagValueObj      = tt_auth_flag_value.auth_flag_value_obj
         AuthFlagValue         = tt_auth_flag_value.auth_flag_value
         LastChangeDateTime    = tt_auth_flag_value.last_change_datetime
         OverrideNote          = tt_auth_flag_value.override_note               
         OwningAltValue        = tt_auth_flag_value.owning_alt_value
         OwningEntityMnemonic  = tt_auth_flag_value.owning_entity_mnemonic
         OwningKey             = tt_auth_flag_value.owning_key
         OwningObj             = tt_auth_flag_value.owning_obj
         AuthRuleObj           = tt_auth_flag_value.auth_rule_obj
        NO-ERROR .      
    END. /*FOR FIRST tt_auth_flag_value NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth_flag_value NO-LOCK).
    
    { mip/inc/mipcatcherror.i  &FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthFlagValueDetails  */
  
   /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None                   
  ---------------------------------------------------------------------- */

    ASSIGN 
       ghAuthFlagValueDataset   = DATASET dsAuthFlagValue:HANDLE
       ErrorObject              = NEW cls.maerrorobject(TEMP-TABLE tt_auth_flag_value_error:HANDLE).
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */
  
  
  /* -- METHOD removeAuthFlagValue ------------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthFlagValue (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused Auth Flag Value record.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE BUFFER btt_auth_flag_value FOR tt_auth_flag_value.
    
    IF NOT AuthFlagValueInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth Flag Value not in focus'"}
    
    FIND FIRST btt_auth_flag_value EXCLUSIVE-LOCK
         WHERE btt_auth_flag_value.auth_flag_value_obj = AuthFlagValueObj.
      
    ASSIGN btt_auth_flag_value.record_action = "DELETE":U.
    
    mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).
    
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* removeAuthFlagValue  */
                                    

  /* -- METHOD saveAuthFlagValue --------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthFlagValue() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update Auth Flag Value record using the instance 
                    properties specified.  
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE BUFFER tt_auth_flag_value FOR tt_auth_flag_value.

    DATASET dsAuthFlagValue:EMPTY-DATASET().
    
    DO TRANSACTION ON ERROR UNDO, THROW:

      CREATE tt_auth_flag_value.
      
      ASSIGN 
         tt_auth_flag_value.record_action          = "MODIFY":U
         tt_auth_flag_value.auth_flag_value_obj    = AuthFlagValueObj
         tt_auth_flag_value.auth_flag_value        = AuthFlagValue
         tt_auth_flag_value.last_change_datetime   = LastChangeDateTime
         tt_auth_flag_value.override_note          = OverrideNote                           
         tt_auth_flag_value.owning_alt_value       = OwningAltValue
         tt_auth_flag_value.owning_entity_mnemonic = OwningEntityMnemonic
         tt_auth_flag_value.owning_key             = OwningKey
         tt_auth_flag_value.owning_obj             = OwningObj
         tt_auth_flag_value.auth_rule_obj          = AuthRuleObj
         .
      
      VALIDATE tt_auth_flag_value.
    
    END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/

    mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE) NO-ERROR.
    
    /*
      When the record is re-focused the entire dataset is emptied, meaning all errors and warnings are cleared as well,
      but we would like to keep track of any warnings even though the record created successfully so that these warnings
      can be returned to the user, so we will temporarily keep track of these warnings and copy them back to the dataset.
    */
    IF ErrorObject:WarningsExist 
    THEN TEMP-TABLE tt_temp_error:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_flag_value_error:HANDLE).
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_flag_value NO-LOCK:
        ASSIGN 
           AuthFlagValueObj  = tt_auth_flag_value.auth_flag_value_obj
           glSuccess         = THIS-OBJECT:focusAuthFlagValue(AuthFlagValueObj).
      END. /*FOR FIRST tt_auth_flag_value NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    /*
      Copy warnings back to the dataset error temp table
    */
    IF TEMP-TABLE tt_temp_error:HAS-RECORDS
    THEN TEMP-TABLE tt_auth_flag_value_error:COPY-TEMP-TABLE(TEMP-TABLE tt_temp_error:HANDLE).
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* saveAuthFlagValue  */
  
  
  /* -- METHOD unfocusDMFlagValue ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthFlagValue() :
  /*--------------------------------------------------------------------
    Purpose: Clears all instance properties and empties the 
             Auth Flag Value dataset.           
  ---------------------------------------------------------------------- */

    DATASET dsAuthFlagValue:EMPTY-DATASET().
    
    ErrorObject:clearErrors().
    
    ASSIGN 
       AuthFlagValueInFocus  = FALSE  
       AuthFlagValueObj      = 0
       AuthFlagValue         = ""
       LastChangeDateTime    = ? 
       OverrideNote          = ""     
       OwningAltValue        = ""
       OwningEntityMnemonic  = ""
       OwningKey             = ""
       OwningObj             = 0 
       AuthRuleObj           = 0
       .
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusAuthFlagValue  */
   
END CLASS.    /* ma.cls.basemaauthflagvalue */  


