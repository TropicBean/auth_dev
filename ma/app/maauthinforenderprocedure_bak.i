
/*------------------------------------------------------------------------------
  maauthinforenderprocedure.i  MEDSTAR Medical Aid System
                               Auth UI Service Render Procedure include
                               (c) Copyright 2018
                               MIP Holdings (Pty) Ltd
                               All rights reserved   

  Author    : MMP     
------------------------------------------------------------------------------*/  
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  
  
  RUN SUPER(ipoControl) NO-ERROR. 
  
   /* The super may not have a rowRenderProcedure */
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6439' &ResetIgnoredErrors = TRUE } 
  
  
&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE VARIABLE dAmountPaid            AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dQtyPaid               AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj           AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dInsurerObj            AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dAuthObj               AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE cAdditionalItems       AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMemNum                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleValue             AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatus                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusNote            AS CHARACTER            NO-UNDO.  
  DEFINE VARIABLE iDependant             AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iOptionCode            AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iStatusCode            AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dAuthDate              AS DATE                 NO-UNDO.                           
  DEFINE VARIABLE dStartDate             AS DATE                 NO-UNDO.
  DEFINE VARIABLE lAuthNumViewable       AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lIncomplete            AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lMandatory             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lRefNumUpdatable       AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lServiceTypeUpdatable  AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSuccess               AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lUpdatable             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValidRule             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lcConfiguration        AS LONGCHAR             NO-UNDO.
  DEFINE VARIABLE oSearch                AS cls.maauthsearch     NO-UNDO.
  DEFINE VARIABLE oATSearch              AS cls.maauthtypesearch NO-UNDO.
  DEFINE VARIABLE oAuthorisation         AS cls.maauthorisation  NO-UNDO.
  DEFINE VARIABLE oAuthType              AS cls.maauthtype       NO-UNDO.
  DEFINE VARIABLE oAuthRule              AS cls.maauthrule       NO-UNDO.
  
                                       

  &SCOPED-DEFINE StandardValues ASSIGN cMemNum      =         ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.mem_num":U      , "BUFFER-VALUE":U)  ~
                                       dAuthObj     = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_obj":U     , "BUFFER-VALUE":U)) ~
                                       dInsurerObj  = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.insurer_obj":U  , "BUFFER-VALUE":U)) ~
                                       dAuthTypeObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_type_obj":U, "BUFFER-VALUE":U)) ~
                                       iOptionCode  = INTEGER(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.option_code":U  , "BUFFER-VALUE":U)) ~
                                       dStartDate   =    DATE(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.start_date":U   , "BUFFER-VALUE":U)) ~
                                       dStartDate   = (IF dStartDate = ? THEN TODAY ELSE dStartDate).
  
  &SCOPED-DEFINE FindAuthTypeConfig   mipEnv:Health:AuthService:getAuthTypeConfig  ~
                                        (INPUT dAuthTypeObj,                       ~
                                         INPUT dInsurerObj,                        ~
                                         INPUT iOptionCode,                        ~
                                         INPUT dStartDate,                         ~
                                         INPUT-OUTPUT TABLE ttAuthTypeConfig).     ~
                                                                                   ~
                                      FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.~
                                                                                   ~
                                      cls.miperror:resetError().
  
  
  EMPTY TEMP-TABLE ttAuthTypeConfig.
  
  CASE ipoControl:RenderArgument:      
  
    WHEN "AuthEndDate":U THEN
    DO:
      {&StandardValues}
      
      IF dAuthObj <> 0 AND dAuthObj <> ? THEN
      DO:
        {&FindAuthTypeConfig}
        
        IF AVAILABLE ttAuthTypeConfig 
        THEN 
          ASSIGN ipoControl:ControlToken = (IF ttAuthTypeConfig.EndDateUpdAllowed 
                                            THEN "Updatable":U 
                                            ELSE "Disabled":U).
        
      END. /*IF dAuthObj > 0 THEN*/                                               
      
      IF ipoControl:ControlType = "wsCombo":U 
      THEN ipoControl:RenderAsComboOrSelect().
      ELSE ipoControl:RenderASInput().
    END. /*WHEN "AuthEndDate":U THEN*/
    
    WHEN "RefNumUpdatable":U THEN
    DO:
      {&StandardValues}
      
      IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN
      DO:
        ASSIGN lRefNumUpdatable        = mipEnv:Health:AuthBusinessLogic:authRefNumUpdatable (INPUT dAuthTypeObj, INPUT dInsurerObj, INPUT iOptionCode, INPUT dStartDate) 
        
               ipoControl:ControlToken = (IF lRefNumUpdatable THEN "Updatable":U ELSE "Disabled":U).
      
      END. /*IF dAuthObj > 0.00 THEN */
      
      ipoControl:RenderASInput().                                  
      
    END. /*WHEN "RefNumUpdatable":U THEN*/
    
    WHEN "ClaimCodeUpdatable":U OR WHEN "ClaimTypeUpdatable":U THEN
    DO:
      {&StandardValues}
      
      IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN
      DO:
        {&FindAuthTypeConfig}
        
        IF AVAILABLE ttAuthTypeConfig THEN 
        DO:
          ASSIGN ipoControl:ControlToken = (IF (ipoControl:RenderArgument = "ClaimCodeUpdatable":U AND ttAuthTypeConfig.ClaimCodeUpdAllowed) OR 
                                               (ipoControl:RenderArgument = "ClaimTypeUpdatable":U AND ttAuthTypeConfig.ClaimTypeUpdAllowed)
                                            THEN 
                                              "Updatable":U
                                            ELSE
                                              (IF ipoControl:ControlType = "wsCombo":U 
                                               THEN "ReadOnly":U 
                                               ELSE "Disabled":U)).
        END. /*IF AVAILABLE ttAuthTypeConfig THEN */
      END. /*IF dAuthObj > 0.00 THEN*/
      
      IF ipoControl:ControlType = 'wsLookupButton' 
      THEN ipoControl:RenderASLookupButton().
      ELSE 
        (IF ipoControl:ControlType = 'wsCombo'
         THEN ipoControl:RenderASComboOrSelect()
         ELSE ipoControl:RenderASInput()).
      
    END. /*WHEN "ClaimCodeUpdatable":U OR WHEN "ClaimTypeUpdatable":U THEN*/
    
    WHEN "ReasonMandatory":U THEN
    DO:
      {&StandardValues}
      
      ASSIGN iStatusCode = INTEGER(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_status":U, "BUFFER-VALUE":U)).
             
      IF dAuthObj <> 0.00 AND dAuthObj <> ? 
      THEN
        ASSIGN ipoControl:ControlValue = STRING(mipEnv:Health:AuthService:statusReasonMandatory(INPUT iStatusCode, INPUT dInsurerObj, INPUT iOptionCode, INPUT dStartDate)).
      
      ipoControl:RenderASInput().
    END. /*WHEN "ReasonMandatory":U THEN*/
    
    WHEN "AuthTypeRender":U THEN
    DO:
      ASSIGN dAuthObj = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_obj":U, "BUFFER-VALUE":U)).
      
      IF ipoControl:ControlType = "wsInput":U THEN 
      DO:
        IF dAuthObj > 0.00 AND dAuthObj <> ?
        THEN ASSIGN ipoControl:ControlToken = "ReadOnly":U
                    ipoControl:ControlClass = ipoControl:ControlClass + " -clMan":U.
        
        ipoControl:RenderASInput().
      END. /*IF ipoControl:ControlType = 'wsInput' AND dAuthObj <= 0.00 THEN */
      
      IF ipoControl:ControlType = "wsLookupButton":U AND (dAuthObj <= 0.00 OR dAuthObj = ?)
      THEN ipoControl:RenderASLookupButton().
      
    END. /*WHEN "AuthTypeRender":U THEN*/
  
    WHEN "AuthIncomplete":U THEN 
    DO:

      IF LOGICAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_incomplete":U, "BUFFER-VALUE":U)) = TRUE THEN
      DO:
        ASSIGN
          ipoControl:ControlToken   = "ReadOnly":U 
          ipoControl:ControlValue   = "AUTHORISATION INCOMPLETE":U.
      END. /*IF LOGICAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_incomplete":U, "BUFFER-VALUE":U)) = TRUE THEN*/
      ELSE 
        ASSIGN ipoControl:ControlToken = "Hidden":U.
      
      ipoControl:RenderAsInput().
    
    END. /* WHEN "AuthIncomplete":U THEN  */ 

    WHEN "DependantList":U THEN 
    DO:
      {&StandardValues}
      
      ASSIGN dAmountPaid = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.amount_paid":U  , "BUFFER-VALUE":U))
             dQtyPaid    = DECIMAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.quantity_paid":U, "BUFFER-VALUE":U)).
            
      /*No need to do this check if we are adding*/
      IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN       
      DO:
        ASSIGN lUpdatable = mipEnv:Health:AuthBusinessLogic:authDependantUpdatable
                              (INPUT dInsurerObj,
                               INPUT iOptionCode,
                               INPUT dStartDate,
                               INPUT dAmountPaid,
                               INPUT dQtyPaid)
           
               ipoControl:ControlToken = (IF lUpdatable THEN "Updatable":U ELSE "ReadOnly":U).
      END. /*IF dAuthObj > 0 THEN       */
      
      ipoControl:renderAsComboOrSelect(). 
    
    END. /* WHEN "DependantList":U THEN  */
    
    WHEN "OptionCodeRender":U THEN 
    DO:
      {&StandardValues}
      
      
      IF dAuthObj <> 0.00 AND dAuthObj <> ? AND INTEGER(ipoControl:ControlValue) = 0
      THEN 
        ASSIGN lSuccess                = (IF iOptionCode = 0 
                                          THEN mipEnv:Health:maMember:getMemberOption(INPUT cMemNum, INPUT dStartDate, OUTPUT iOptionCode) 
                                          ELSE TRUE)
                                          
               ipoControl:ControlValue = STRING(iOptionCode).            
        
      
      FIND FIRST scheme NO-LOCK                                                                                                                           
           WHERE scheme.scheme-code = INTEGER(ipoControl:ControlValue) 
        NO-ERROR. 
           
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
      
      
      ASSIGN ipoControl:ControlValue = ipoControl:ControlValue + (IF AVAILABLE scheme THEN " - ":U + scheme.short-name ELSE "":U).                                                                                                                             
      
      CASE ipoControl:ControlType:
        WHEN "wsLookupButton":U THEN lSuccess = ipoControl:renderAsLookupButton().
        WHEN "wsInput":U        THEN lSuccess = ipoControl:renderAsInput().  
      END CASE.
    
    END. /* WHEN "OptionCodeRender":U THEN  */
   
    WHEN "AuthNumViewableMaint":U THEN
    DO:
      {&StandardValues}
      
      ASSIGN iStatusCode      = INTEGER(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_status":U, "BUFFER-VALUE":U)) 
        
             lAuthNumViewable = mipEnv:Health:AuthBusinessLogic:AuthNumViewable(INPUT iStatusCode, INPUT dInsurerObj, INPUT iOptionCode, INPUT dAuthDate).
      
      
      IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN
      DO:
        oAuthorisation = NEW cls.maauthorisation(dAuthObj).
        
        IF lAuthNumViewable AND oAuthorisation:InFocus
        THEN ASSIGN ipoControl:ControlValue = oAuthorisation:AuthNum.
        ELSE ASSIGN ipoControl:ControlValue = "********":U.  
      END. /*IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN*/
      ELSE
        ASSIGN ipoControl:ControlValue = "[Auto-Generate]":U.
      
      
      ASSIGN
        ipoControl:ControlToken = "ReadOnly":U
        lSuccess                = ipoControl:renderAsInput().
        
    END. /*WHEN "AuthNumViewableMaint" THEN*/
    
    WHEN "AuthMode":U THEN 
    DO:
      ASSIGN lIncomplete = LOGICAL(ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_incomplete":U, "BUFFER-VALUE":U)).
    
      ASSIGN ipoControl:ControlValue = (IF lIncomplete = ? OR lIncomplete = TRUE THEN "NEW":U ELSE "UPDATE":U).
                      
      ipoControl:RenderASInput().
    END. /*WHEN "AuthMode":U THEN */
    
    WHEN "AuthStatusNote":U THEN 
    DO:
      {&StandardValues}

      /*
        Check if this field should be mandatory or not
      */
      ASSIGN cStatus      = ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_status":U     , "BUFFER-VALUE":U)
             cStatusNote  = ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_status_note":U, "BUFFER-VALUE":U)

             lMandatory   = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), INPUT dInsurerObj, INPUT iOptionCode, INPUT dStartDate).
             
      
      ASSIGN ipoControl:ControlClass = ipoControl:ControlClass 
                                     + (IF lMandatory THEN " +":U ELSE " -":U)
                                     + "clMan":U.

      IF cStatusNote <> "":U THEN
      DO:
        FIND FIRST note NO-LOCK
             WHERE note.scheme-code = iOptionCode
               AND note.type        = "AS":U
               AND note.key         = cStatusNote
          NO-ERROR.
          
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
        
        IF AVAILABLE note THEN
        DO:
          ASSIGN ipoControl:ControlToolTip = note.narration[1].
        END. /*IF AVAILABLE note THEN*/
      END. /*IF cStatusNote <> "":U THEN*/
      
      /*
        Render control
      */
      IF ipoControl:ControlType = 'wsLookupButton'
      THEN 
        ipoControl:RenderASLookupButton().
      ELSE
        ipoControl:RenderASInput().
    END. /*WHEN "AuthStatusNote":U THEN */
    
    WHEN "AuthReasonTypePrefix":U THEN 
    DO:
      ASSIGN ipoControl:ControlValue = "AS":U.
      
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthReasonType":U */
    
    WHEN "AuthReasonType":U THEN 
    DO:
      {&StandardValues}
      
      ASSIGN lSuccess                = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                         (INPUT  dInsurerObj,
                                          INPUT  iOptionCode,
                                          INPUT  "ma_acAuthRuleTypeAUTHSETUPS":U,
                                          INPUT  "NotePerAuthStatus":U,
                                          INPUT  dStartDate,
                                          OUTPUT lValidRule,
                                          OUTPUT cRuleValue)
            
             cStatus                 = ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth.auth_status":U, "BUFFER-VALUE":U)
             
             ipoControl:ControlValue = "AS":U 
                                     + (IF cStatus = "":U OR NOT lValidRule OR NOT CAN-DO("YES,Y,TRUE,T":U, cRuleValue) 
                                        THEN "*":U ELSE cStatus).
      
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthReasonType":U */
    
    WHEN "AuthConfiguration":U THEN 
    DO:
      {&StandardValues}
      
      IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN
      DO:
        RUN _getConfiguration IN TARGET-PROCEDURE ( INPUT  dAuthTypeObj, 
                                                    INPUT  dInsurerObj,
                                                    INPUT  iOptionCode,
                                                    INPUT  dStartDate,
                                                    OUTPUT lcConfiguration).
                                                    
        ASSIGN ipoControl:ControlValue = STRING(lcConfiguration).                                            
      END. /*IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN*/
                                                            
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthConfiguration":U */
    
    WHEN "ServiceType":U THEN
    DO:
      {&StandardValues}

      IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN
      DO:
        {&FindAuthTypeConfig}

        IF AVAILABLE ttAuthTypeConfig THEN
        DO:
          IF ttAuthTypeConfig.ActivateServiceType 
          THEN
          DO:
            ASSIGN 
              lSuccess  = mipEnv:Health:AuthBusinessLogic:authServiceTypeUpdatable(INPUT  dInsurerObj,
                                                                                   INPUT  iOptionCode,
                                                                                   INPUT  dStartDate,
                                                                                   OUTPUT lServiceTypeUpdatable)

              ipoControl:ControlToken = IF lServiceTypeUpdatable THEN "Updatable":U           ELSE "Disabled":U 
              ipoControl:ControlClass = IF lServiceTypeUpdatable THEN ipoControl:ControlClass ELSE "+clDisabled" . /* apply the grey colour */ 

          END. /* IF ttAuthTypeConfig.ActivateServiceType  */
          ELSE
            ASSIGN ipoControl:ControlToken = "Hidden":U .

        END. /* IF AVAILABLE ttAuthTypeConfig */
      END. /* IF dAuthObj <> 0.00 AND dAuthObj <> ?  */

      ipoControl:renderAsComboOrSelect(). 
    END. /* WHEN "ServiceType" */
  END. /*CASE ipoControl:RenderArgument:      */
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch)        THEN DELETE OBJECT oSearch.
                IF VALID-OBJECT(oAuthorisation) THEN DELETE OBJECT oAuthorisation.
                IF VALID-OBJECT(oAuthType)      THEN DELETE OBJECT oAuthType.
                IF VALID-OBJECT(oAuthRule)      THEN DELETE OBJECT oAuthRule.
                IF VALID-OBJECT(oATSearch)      THEN DELETE OBJECT oATSearch.
                
                EMPTY TEMP-TABLE tt_acronym.
                
                DATASET dsAuthType:EMPTY-DATASET().
                
                EMPTY TEMP-TABLE ttAuthTypeConfig.
                "}
                
                
                
&ENDIF          



