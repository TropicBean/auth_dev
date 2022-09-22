&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    Purpose : Healthcare authorisation decision stack
    Author  : Andrewd

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */

ROUTINE-LEVEL ON ERROR UNDO, THROW.  

CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i}

{ ma/inc/madecisionauthhighriskds.i }
{ ma/inc/maauthflagvalueds.i        }
{ ma/inc/maauthruleds.i             }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 28.14
         WIDTH              = 78.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-highRiskIndicator) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE highRiskIndicator Procedure 
PROCEDURE highRiskIndicator :
/*------------------------------------------------------------------------------
  Purpose   : Prepare dataset and make high risk indicator business rule request    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipoParameter           AS cls.maparameterobject NO-UNDO.
  DEFINE OUTPUT PARAMETER oplcDecisionResult     AS LONGCHAR              NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDecisionErrorDetail AS CHARACTER             NO-UNDO.
  
  DEFINE VARIABLE oDecision      AS cls.mipdecision           NO-UNDO.
  DEFINE VARIABLE oFVSearch      AS cls.maauthflagvaluesearch NO-UNDO.
  DEFINE VARIABLE oAuthorisation AS cls.maauthorisation       NO-UNDO.
  DEFINE VARIABLE oAuthRule      AS cls.maauthrule            NO-UNDO.
  DEFINE VARIABLE lcDataSource   AS LONGCHAR                  NO-UNDO.
  DEFINE VARIABLE hDatasetHandle AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE dAuthObj       AS DECIMAL                   NO-UNDO.
  
  DEFINE BUFFER tt_auth_flag_value FOR tt_auth_flag_value.
                                                       
  DATASET dsAuthHighRiskIndicator:EMPTY-DATASET().
  
  DATASET dsAuthFlagValue:EMPTY-DATASET().
  
  
  IF VALID-OBJECT(ipoParameter) THEN 
  DO:
  
    ipoParameter:messageParameters("ma_DebugAuth":U).
    
    
    ASSIGN dAuthObj = DECIMAL(ipoParameter:getParameter("auth_obj":U))
           
           dAuthObj = (IF dAuthObj = ? THEN 0.00 ELSE dAuthObj).
    
                                                       
    mipEnv:Health:AuthService:getAuthObject
      ( INPUT  dAuthObj,
        INPUT  "":U,
        OUTPUT oAuthorisation ).
    
    IF oAuthorisation:InFocus
    THEN
    DO TRANSACTION:
    
      /*
        Authorisation header
      */
      CREATE ttAuthHighRiskIndicator.
      ASSIGN ttAuthHighRiskIndicator.AmountInterim = oAuthorisation:AmountInterim
             ttAuthHighRiskIndicator.AuthDate      = oAuthorisation:AuthDate
             ttAuthHighRiskIndicator.AuthNum       = oAuthorisation:AuthNum.
             
      VALIDATE ttAuthHighRiskIndicator.       
                                 
      
      ASSIGN hDatasetHandle = DATASET dsAuthHighRiskIndicator:HANDLE.
      
      
      oDecision = NEW cls.mipdecision("ma_deAuthHighRiskIndicator":U).
      
      
      /*
        Prepare data source for decision request
      */
      ASSIGN lcDataSource = mipEnv:Health:maDecisionService:prepareDataSource(INPUT oDecision:DecisionTemplate:TemplateCode, INPUT hDatasetHandle).       
      
      
      IF lcDataSource <> "":U THEN
      DO:
        
        ASSIGN lSuccess = oDecision:setDataSource(lcDataSource)
        
               lSuccess = oDecision:saveDecision().
        
        
        oDecision:executeDecision(TRUE).
        
        
        ASSIGN oplcDecisionResult     = oDecision:DecisionResult
               opcDecisionErrorDetail = oDecision:DecisionErrorDetail.
               
      END. /*IF lcDataSource <> "":U THEN*/
      
      
      DATASET dsAuthHighRiskIndicator:READ-XML("LONGCHAR":U, oplcDecisionResult, "EMPTY":U, ?, ?, ?, ?).
      
      
      oAuthRule = NEW cls.maauthrule().
      
      
      oAuthRule:focusAuthRule(INPUT oAuthorisation:InsurerObj,
                              INPUT oAuthorisation:OptionCode,
                              INPUT "ma_acAuthRuleTypeAuthFlag":U,
                              INPUT "HIGHRISKIND":U,
                              INPUT oAuthorisation:StartDate).
                                   
                                       
      IF oAuthRule:AuthRuleInFocus THEN 
      DO:
        oFVSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE).
          
        ASSIGN lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
               lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, oAuthorisation:AuthObj)
               lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_key":U            , "=":U, "":U)
               lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.auth_rule_obj":U         , "=":U, oAuthRule:AuthRuleObj).
        
        oFVSearch:fetchData().
        
        
        FIND FIRST ttFlagHighRiskIndicator NO-LOCK NO-ERROR.
      
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
        
        FIND FIRST tt_auth_flag_value EXCLUSIVE-LOCK NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        
        
        IF AVAILABLE ttFlagHighRiskIndicator AND ttFlagHighRiskIndicator.FlagValue THEN
        DO:        
          IF NOT AVAILABLE tt_auth_flag_value THEN
          DO:
            CREATE tt_auth_flag_value.
            ASSIGN tt_auth_flag_value.record_action          = 'MODIFY'
                   tt_auth_flag_value.owning_entity_mnemonic = 'hatau'
                   tt_auth_flag_value.owning_obj             = oAuthorisation:AuthObj
                   tt_auth_flag_value.owning_key             = "":U
                   tt_auth_flag_value.owning_alt_value       = oAuthorisation:AuthNum
                   tt_auth_flag_value.auth_rule_obj          = oAuthRule:AuthRuleObj
                   tt_auth_flag_value.last_change_datetime   = NOW
                   tt_auth_flag_value.auth_flag_value        = "TRUE":U.
                   
            VALIDATE tt_auth_flag_value.       
            
            mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).
            
          END. /*IF NOT AVAILABLE tt_auth_flag_value THEN*/        
        END. /*IF AVAILABLE ttFlagHighRiskIndicator AND CAN-DO("Y,YES,TRUE":U, ttFlagHighRiskIndicator.FlagValue) THEN*/
        ELSE
        DO:
          IF AVAILABLE tt_auth_flag_value THEN
          DO:
            ASSIGN tt_auth_flag_value.record_action = 'DELETE'.
            
            mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).
          END. /*IF AVAILABLE tt_auth_flag_value THEN*/          
        END. /*ELSE*/        
      END. /*IF oAuthRule:RuleInFocus THEN */      
    END. /*DO TRANSACTION:*/
  END. /*IF VALID-OBJECT(ipoParameter) THEN */
   
  { mip/inc/mipdefshared.i 
    &FINALLY = "DATASET dsAuthFlagValue:EMPTY-DATASET().
    
                DATASET dsAuthHighRiskIndicator:EMPTY-DATASET().
    
                IF VALID-OBJECT(ipoParameterObject) THEN DELETE OBJECT ipoParameterObject.
                IF VALID-OBJECT(oDecision)          THEN DELETE OBJECT oDecision.
                IF VALID-OBJECT(oAuthRule)          THEN DELETE OBJECT oAuthRule.
                IF VALID-OBJECT(oFVSearch)          THEN DELETE OBJECT oFVSearch.                                
                "}  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


