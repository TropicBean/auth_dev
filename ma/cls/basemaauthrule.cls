/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Kati

      Purpose: Auth Rule Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthrule INHERITS cls.mip:

  { sysadmma.i } 
  
  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthruleds.i }
  
 
  DEFINE PRIVATE VARIABLE glSuccess              AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthRuleDataset      AS HANDLE  NO-UNDO.
  
  DEFINE PUBLIC PROPERTY AuthRuleInFocus         AS LOGICAL           GET. PRIVATE SET.
                                                                      
  DEFINE PUBLIC PROPERTY LineNumber              AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY AuthRuleObj             AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj              AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate           AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                 AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY LinkAuthRuleObj         AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY OptionCode              AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY RuleCode                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY RuleDescription         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY RuleType                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY RuleValidValues         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY RuleValue               AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY SystemOwned             AS LOGICAL           GET. SET.
  
  DEFINE PUBLIC PROPERTY ErrorObject             AS cls.maerrorobject GET. PRIVATE SET.
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthrule() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthrule */

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthrule
    ( INPUT ipdAuthRuleObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthRule(ipdAuthRuleObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthrule */
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthrule
    ( INPUT ipdInsurerObj    AS DECIMAL,
      INPUT ipiOptionCode    AS INTEGER,
      INPUT ipcRuleType      AS CHARACTER,
      INPUT ipcRuleCode      AS CHARACTER,
      INPUT ipdEffectiveDate AS DATE ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthRule(ipdInsurerObj,ipiOptionCode,ipcRuleType,ipcRuleCode,ipdEffectiveDate).

    { mip/inc/mipcatcherror.i }
                              
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthrule */
  
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthrule() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
    
    IF VALID-HANDLE( ghAuthRuleDataset  ) THEN DELETE OBJECT ghAuthRuleDataset.
    IF VALID-OBJECT( ErrorObject )        THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthrule */

  
  /* -- METHOD focusAuthRule ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthRule
    ( INPUT ipdAuthRuleObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth rule with the specified obj 
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth Rule obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess       = unfocusAuthRule()    /*Clear any properties*/
        
        AuthRuleObj     = ipdAuthRuleObj
        
        glSuccess       = getAuthRuleDetails() /*Retrieve all data for the specified obj 
                                                           and populate instance properties*/                       
        AuthRuleInFocus = glSuccess.
        
    RETURN AuthRuleInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthRule  */
  
  
  /* -- METHOD focusAuthRule ----------------------------------------- */
  METHOD PUBLIC LOGICAL focusAuthRule
    ( INPUT ipdInsurerObj    AS DECIMAL,
      INPUT ipiOptionCode    AS INTEGER,
      INPUT ipcRuleType      AS CHARACTER,
      INPUT ipcRuleCode      AS CHARACTER,
      INPUT ipdEffectiveDate AS DATE ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth rule with the specified parameters 
                    and populate the instance properties with
                    the relevant values.
        Parameters: 1. Insurer Obj 
                    2. Option Code
                    3. Rule Type
                    4. Rule Code 
                    5. Link Auth Rule Obj 
                    6. Effective Date        
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess            = unfocusAuthRule()    /*Clear any properties*/
        
        InsurerObj           = ipdInsurerObj 
        OptionCode           = ipiOptionCode 
        RuleType             = ipcRuleType 
        RuleCode             = ipcRuleCode 
        EffectiveDate        = ipdEffectiveDate
        
        glSuccess            = getAuthRuleDetails()
        
        AuthRuleInFocus = glSuccess.
        
    RETURN AuthRuleInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthRule  */

  
  /* -- METHOD getAuthRuleDetails --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthRuleDetails(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing auth rule data
                    using the auth rule data retrieval services.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE lValidRule AS LOGICAL  NO-UNDO.

    mipEnv:Health:AuthMaintenance:getAuthRuleDetails
      ( INPUT-OUTPUT AuthRuleObj,
        INPUT-OUTPUT InsurerObj,
        INPUT-OUTPUT OptionCode,
        INPUT-OUTPUT RuleType,
        INPUT-OUTPUT RuleCode,
        INPUT-OUTPUT EffectiveDate,
              OUTPUT lValidRule,
              OUTPUT RuleValue,
              OUTPUT RuleValidValues,
              OUTPUT LinkAuthRuleObj,
              OUTPUT RuleDescription,
              OUTPUT SystemOwned,
              OUTPUT EndDate ).

    RETURN lValidRule.
    { mip/inc/mipcatcherror.i }
  END METHOD.  /* getAuthRuleDetails  */
  
                        
  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None                   
  ---------------------------------------------------------------------- */

    ASSIGN 
       ghAuthRuleDataset = DATASET dsAuthRule:HANDLE
       
       ErrorObject       = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rule_error:HANDLE).
       
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */
  
  
  /* -- METHOD removeAuthRule ------------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthRule (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth rule record.
        Parameters: None                   
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN        
    DEFINE BUFFER btt_auth_rule FOR tt_auth_rule.
    
    IF NOT AuthRuleInFocus THEN
    DO:
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth rule not in focus'"}
    END.  // IF NOT AuthRuleInFocus THEN
      
    DATASET dsAuthRule:EMPTY-DATASET().
	 
    CREATE btt_auth_rule.
    
    ASSIGN 
      btt_auth_rule.auth_rule_obj      = AuthRuleObj
      btt_auth_rule.insurer_obj        = InsurerObj
      btt_auth_rule.effective_date     = EffectiveDate
      btt_auth_rule.end_date           = EndDate
      btt_auth_rule.link_auth_rule_obj = LinkAuthRuleObj
      btt_auth_rule.option_code        = OptionCode
      btt_auth_rule.rule_code          = RuleCode
      btt_auth_rule.rule_description   = RuleDescription
      btt_auth_rule.rule_type          = RuleType
      btt_auth_rule.rule_valid_values  = RuleValidValues
      btt_auth_rule.rule_value         = RuleValue
      btt_auth_rule.system_owned       = SystemOwned
      btt_auth_rule.record_action      = "DELETE":U.

    VALIDATE btt_auth_rule.
    
    mipEnv:Health:AuthMaintenance:saveAuthRule(INPUT-OUTPUT DATASET dsAuthRule BY-REFERENCE).
    
    RETURN NOT ErrorObject:ErrorsExist.
    
&ENDIF
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */

  /* -- METHOD saveAuthRule --------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthRule() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth rule record using the instance 
                    properties specified.  
        Parameters: None                   
  ---------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN      
  
    DATASET dsAuthRule:EMPTY-DATASET().
    
    CREATE tt_auth_rule.
    
    ASSIGN 
       tt_auth_rule.line_number        = LineNumber
       tt_auth_rule.record_action      = "MODIFY":U
       tt_auth_rule.auth_rule_obj      = AuthRuleObj
       tt_auth_rule.insurer_obj        = InsurerObj
       tt_auth_rule.effective_date     = EffectiveDate
       tt_auth_rule.end_date           = EndDate
       tt_auth_rule.link_auth_rule_obj = LinkAuthRuleObj
       tt_auth_rule.option_code        = OptionCode
       tt_auth_rule.rule_code          = RuleCode
       tt_auth_rule.rule_description   = RuleDescription
       tt_auth_rule.rule_type          = RuleType
       tt_auth_rule.rule_valid_values  = RuleValidValues
       tt_auth_rule.rule_value         = RuleValue
       tt_auth_rule.system_owned       = SystemOwned.

    VALIDATE tt_auth_rule.

    mipEnv:Health:AuthMaintenance:saveAuthRule(INPUT-OUTPUT DATASET dsAuthRule BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_rule NO-LOCK:
        ASSIGN 
           AuthRuleObj = tt_auth_rule.auth_rule_obj
           glSuccess   = THIS-OBJECT:focusAuthRule(AuthRuleObj).
      END. /*FOR FIRST tt_auth_rule NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF    
  END METHOD.  /* saveAuthRule  */
  
  
  /* -- METHOD unfocusAuthRule ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthRule() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.           
  ---------------------------------------------------------------------- */

    DATASET dsAuthRule:EMPTY-DATASET().
    
    ErrorObject:clearErrors().
    
    ASSIGN 
       AuthRuleInFocus  = FALSE
       LineNumber       = 0
       AuthRuleObj      = 0.00
       InsurerObj       = 0.00
       EffectiveDate    = ?
       EndDate          = ?
       LinkAuthRuleObj  = 0.00
       OptionCode       = 0
       RuleCode         = "":U
       RuleDescription  = "":U
       RuleType         = "":U
       RuleValidValues  = "":U
       RuleValue        = "":U
       SystemOwned      = ?.
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusAuthRule  */    
    
END CLASS.  /* mip.cls.basemaauthrule */





