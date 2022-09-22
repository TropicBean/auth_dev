/* maauthservauthtypeconfig.i MEDSTAR Medical Aid System
                              Healthcare Auth data access service: Get auth type configuration                              
                              (c) Copyright 2018-2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved

------------------------------------------------------------------------------
  Purpose   : Get resolved auth type configuration for a specified authorisation type
  Parameters:
  Notes     : 
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdAuthTypeObj     AS  DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdInsurerObj      AS  DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode      AS  INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipdEffectiveDate   AS  DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER ipcProviderType    AS  CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ipiNegNum          AS  INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER ipcDiscipline      AS  CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ipcSubDiscipline   AS  CHARACTER  NO-UNDO.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttAuthTypeConfig.
  
  DEFINE VARIABLE cActiveHealthRuleValue     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cCrosswalkEnableRuleValue  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cDisciplineString          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cKey                       AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleValue                 AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE dEndYear		     AS INTEGER              NO-UNDO.
  DEFINE VARIABLE lActiveHealthValidRule     AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lcData                     AS LONGCHAR             NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValidCrosswalkRule        AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValidRule                 AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE oSearch                    AS cls.maauthtypesearch NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN      


  { ma/msc/madispad.i &discipline = ipcDiscipline                     } 
  { ma/msc/madispad.i &discipline = ipcSubDiscipline &comment = "/* " } 
  
  ASSIGN cDisciplineString = ipcDiscipline + ipcSubDiscipline
  
         /*
           We will store a "Key" on the config record so we re-use the config record
           where the request is the same for obvious performance reasons
         */
         cKey              = SUBSTITUTE("&1^&2^&3^&4^&5^&6^&7^&8":U, STRING(ipdAuthTypeObj), 
                                                                  STRING(ipdInsurerObj),
                                                                  STRING(ipiOptionCode),
                                                                     STRING(ipiNegNum),
                                                                  REPLACE(STRING(ipdEffectiveDate), "/":U, "_":U),
                                                                  ipcProviderType,
                                                                  ipcDiscipline,
                                                                  ipcSubDiscipline).
 
  /*
    Make sure we dont retrieve the record if we already have it
  */
  IF NOT CAN-FIND(FIRST ttAuthTypeConfig NO-LOCK WHERE ttAuthTypeConfig.AuthTypeConfigKey = TRIM(cKey)) THEN
  DO: 
    EMPTY TEMP-TABLE ttAuthTypeConfig.
    
    /*
      If we dont have the record in the config temp table, check if we havent 
      cached the record in a previous request
    */
    FIND FIRST ttConfigCache NO-LOCK 
         WHERE ttConfigCache.AuthTypeConfigKey = TRIM(cKey)
      NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors= TRUE }
    
    IF NOT AVAILABLE ttConfigCache THEN
    DO:   
      
      DATASET dsAuthType:EMPTY-DATASET().                           
      
      
      oSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).
      
      ASSIGN lSuccess = oSearch:fetchAuthType(INPUT ipdAuthTypeObj,
                                              INPUT ipdInsurerObj,
                                              INPUT ipiOptionCode,
                                              INPUT ipdEffectiveDate,
                                              INPUT ipcProviderType,
                                              INPUT "Default":U).

      /* Check if crosswalk have been enabled by checking the relevant auth rule */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                     INPUT  0,
                                                     INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                     INPUT  "CrosswalkEnabled":U,
                                                     INPUT  TODAY,
                                                     OUTPUT lValidCrosswalkRule,
                                                     OUTPUT cCrosswalkEnableRuleValue).
      
      /*
         Get the 'Episode' rule 
      */  
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                     INPUT  ipiOptionCode,
                                                     INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                     INPUT  "Episode":U,
                                                     INPUT  ipdEffectiveDate,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
     /*Get the DefaultHealthContainer*/
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                     INPUT  ipiOptionCode,
                                                     INPUT  "ma_acAuthRuleTypeAuthSetups":U,
                                                     INPUT  "DefaultHealthContainer":U,
                                                     INPUT  ipdEffectiveDate,
                                                     OUTPUT lActiveHealthValidRule,
                                                     OUTPUT cActiveHealthRuleValue).

      IF lActiveHealthValidRule       = FALSE
      OR TRIM(cActiveHealthRuleValue) = "":U
      OR cActiveHealthRuleValue       = ?
      THEN 
        ASSIGN cActiveHealthRuleValue = "None".

      IF cActiveHealthRuleValue = "[None]" THEN
        ASSIGN cActiveHealthRuleValue = "None".
      ELSE IF cActiveHealthRuleValue = "[All]" THEN
        ASSIGN cActiveHealthRuleValue = "All".
      ELSE
        ASSIGN cActiveHealthRuleValue = REPLACE(cActiveHealthRuleValue, "|":U, ",":U).
      
      FOR EACH tt_auth_type NO-LOCK:
      
        IF ipiOptionCode <> 0 AND CAN-FIND(FIRST tt_auth_type_control NO-LOCK WHERE tt_auth_type_control.option_code = ipiOptionCode) THEN 
        DO:
          FOR EACH tt_auth_type_control EXCLUSIVE-LOCK
             WHERE tt_auth_type_control.option_code <> ipiOptionCode:
            DELETE tt_auth_type_control.
          END. /*FOR EACH tt_auth_type_control EXCLUSIVE-LOCK*/
        END. /*IF ipiOptionCode <> 0 AND CAN-FIND(FIRST tt_auth_type_control NO-LOCK WHERE tt_auth_type_control.option_code = ipiOptionCode) THEN */
        
        IF ipdInsurerObj <> 0.00 AND CAN-FIND(FIRST tt_auth_type_control NO-LOCK WHERE tt_auth_type_control.insurer_obj = ipdInsurerObj) THEN 
        DO:
          FOR EACH tt_auth_type_control EXCLUSIVE-LOCK
             WHERE tt_auth_type_control.insurer_obj <> ipdInsurerObj:
            DELETE tt_auth_type_control.
          END. /*FOR EACH tt_auth_type_control EXCLUSIVE-LOCK*/
        END. /*IF dInsurerObj <> 0.00 AND CAN-FIND(FIRST tt_auth_type_control NO-LOCK WHERE tt_auth_type_control.insurer_obj = ipdInsurerObj) THEN */
        
        CREATE ttAuthTypeConfig.
        ASSIGN ttAuthTypeConfig.AuthTypeConfigKey           = TRIM(cKey)
               ttAuthTypeConfig.AuthTypeObj                 = ipdAuthTypeObj
               ttAuthTypeConfig.InsurerObj                  = ipdInsurerObj
               ttAuthTypeConfig.ActivateAmPm                = tt_auth_type.activate_am_pm       
               ttAuthTypeConfig.ActivateBodyRegion          = tt_auth_type.activate_body_region       
               ttAuthTypeConfig.ActivateDueDate             = tt_auth_type.activate_due_date
               ttAuthTypeConfig.ActivateEpisodeNumber       = tt_auth_type.activate_episode_number
               ttAuthTypeConfig.ActivateLosWeekendPass      = tt_auth_type.activate_los_weekend_pass
               ttAuthTypeConfig.ActivateMouthPartID         = tt_auth_type.activate_mouth_part_id
               ttAuthTypeConfig.ActivateServiceType         = tt_auth_type.activate_service_type
               ttAuthTypeConfig.AuthoriseAllServices        = tt_auth_type.authorise_all_services
               ttAuthTypeConfig.AuthType                    = tt_auth_type.auth_type
               ttAuthTypeConfig.SequenceKey                 = tt_auth_type.sequence_key
               ttAuthTypeConfig.AuthTypePrefix              = tt_auth_type.auth_type_prefix

               ttAuthTypeConfig.ClaimCodeUpdUser            = tt_auth_type.claim_code_upd_user
               ttAuthTypeConfig.ClaimCodeUpdRole            = tt_auth_type.claim_code_upd_role
               ttAuthTypeConfig.ClaimCodeUpdAllowed         = (tt_auth_type.claim_code_upd_user  = "":U AND tt_auth_type.claim_code_upd_role = "":U)                          OR 
                                                              (tt_auth_type.claim_code_upd_user <> "":U AND CAN-DO(tt_auth_type.claim_code_upd_user, mipEnv:miUser:UserCode)) OR 
                                                              (tt_auth_type.claim_code_upd_role <> "":U AND fnUserHasRole(tt_auth_type.claim_code_upd_role)) 
                                                              
               ttAuthTypeConfig.ClaimTypeUpdUser            = tt_auth_type.claim_type_upd_user
               ttAuthTypeConfig.ClaimTypeUpdRole            = tt_auth_type.claim_type_upd_role                                               
               ttAuthTypeConfig.ClaimTypeUpdAllowed         = (tt_auth_type.claim_type_upd_user  = "":U AND tt_auth_type.claim_type_upd_role = "":U)                          OR 
                                                              (tt_auth_type.claim_type_upd_user <> "":U AND CAN-DO(tt_auth_type.claim_type_upd_user, mipEnv:miUser:UserCode)) OR 
                                                              (tt_auth_type.claim_type_upd_role <> "":U AND fnUserHasRole(tt_auth_type.claim_type_upd_role))
      
               ttAuthTypeConfig.EndDateUpdUsr               = tt_auth_type.end_date_upd_user
               ttAuthTypeConfig.EndDateUpdRole              = tt_auth_type.end_date_upd_role                                               
               ttAuthTypeConfig.EndDateUpdAllowed           = (tt_auth_type.end_date_upd_user    = "":U AND tt_auth_type.end_date_upd_role = "":U)                            OR 
                                                              (tt_auth_type.end_date_upd_user   <> "":U AND CAN-DO(tt_auth_type.end_date_upd_user, mipEnv:miUser:UserCode))   OR 
                                                              (tt_auth_type.end_date_upd_role   <> "":U AND fnUserHasRole(tt_auth_type.end_date_upd_role))

               ttAuthTypeConfig.DefaultAuthStatus           = tt_auth_type.default_auth_status
               ttAuthTypeConfig.DefaultAuthStatusNote       = tt_auth_type.default_auth_status_note
               ttAuthTypeConfig.DefaultAuthStatusUpdRole    = tt_auth_type.default_auth_status_upd_role
               ttAuthTypeConfig.DefaultAuthStatusUpdUser    = tt_auth_type.default_auth_status_upd_user
               ttAuthTypeConfig.DefaultAuthStatusUpdAllowed = (tt_auth_type.default_auth_status_upd_user  = "":U AND tt_auth_type.default_auth_status_upd_role = "":U)                           
               ttAuthTypeConfig.DefaultLineRestriction      = tt_auth_type.default_line_restriction                                               
               ttAuthTypeConfig.EffectiveDate               = tt_auth_type.effective_date
               ttAuthTypeConfig.EndDate                     = tt_auth_type.end_date
               ttAuthTypeConfig.HeaderValuesAllowed         = tt_auth_type.header_values_allowed
               ttAuthTypeConfig.HeaderValuesUnlimited       = tt_auth_type.header_values_unlimited
               ttAuthTypeConfig.IcdCondCode                 = tt_auth_type.icd_cond_code
               ttAuthTypeConfig.IcdCondType                 = tt_auth_type.icd_cond_type
               ttAuthTypeConfig.UpdatesAllowed              = tt_auth_type.updates_allowed
               ttAuthTypeConfig.ValidIcds                   = tt_auth_type.valid_icds
               ttAuthTypeConfig.AuthTypeGroup               = tt_auth_type.auth_type_group
               ttAuthTypeConfig.ActivateLos                 = tt_auth_type.activate_los
               ttAuthTypeConfig.ActivateCopayment           = tt_auth_type.activate_copayment
               ttAuthTypeConfig.ActivatePenalty             = tt_auth_type.activate_penalty
               ttAuthTypeConfig.ActivateHealth              = IF TRIM (tt_auth_type.activate_health) <> "":U THEN tt_auth_type.activate_health ELSE cActiveHealthRuleValue                                                             
               ttAuthTypeConfig.ActivateCrosswalk           = tt_auth_type.activate_crosswalk AND lValidCrosswalkRule AND CAN-DO("Y,YES,TRUE":U, TRIM(cCrosswalkEnableRuleValue))
               ttAuthTypeConfig.ActivateCodelink            = tt_auth_type.activate_code_link AND lValidCrosswalkRule AND CAN-DO("Y,YES,TRUE":U, TRIM(cCrosswalkEnableRuleValue)) 
               ttAuthTypeConfig.MultipleCCMessageType       = tt_auth_type.multiple_cc_message_type
               .
               
        /* 
          Populate the Auth Type Group Label from the acronym (EA)
        */
        IF tt_auth_type.auth_type_group <> "":U THEN
        DO:
          FIND FIRST mic_acronym NO-LOCK
               WHERE mic_acronym.category_key = "ma_acAuthTypeGroup":U
                 AND mic_acronym.acronym_key  = tt_auth_type.auth_type_group
            NO-ERROR.
           
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
          
          IF AVAILABLE mic_acronym 
          THEN 
            ASSIGN ttAuthTypeConfig.AuthTypeGroupLabel = mic_acronym.acronym_label.
          ELSE 
            ASSIGN ttAuthTypeConfig.AuthTypeGroupLabel = "".
            
        END. /* tt_auth_type.auth_type_group <> "":U THEN */        
        
        /* Looking up or can do when the default_auth_status_upd_user is blank will result in a false hence we should only do it when we have a value in the field */
        IF tt_auth_type.default_auth_status_upd_user <> "":U    
        THEN ASSIGN ttAuthTypeConfig.DefaultAuthStatusUpdAllowed = LOOKUP(tt_auth_type.default_auth_status_upd_user, mipEnv:miUser:UserCode) > 0.

        IF tt_auth_type.default_auth_status_upd_role <> "":U    
        THEN ASSIGN ttAuthTypeConfig.DefaultAuthStatusUpdAllowed = fnUserHasRole(tt_auth_type.default_auth_status_upd_role).
                        
        /*                                                         
           If the rule is set up, check Activate and Updatable values 
        */
        IF lValidRule THEN
        DO:
          IF ENTRY(1,cRuleValue,"|") = "Activate" THEN 
          DO:
            ASSIGN ttAuthTypeConfig.ActivateEpisodeNumber = TRUE.   /* Episode is activated */
          
            IF ENTRY(2,cRuleValue,"|") = "Update" 
            THEN ASSIGN ttAuthTypeConfig.EpisodeUpdAllowed = TRUE.  /* Episode is updatable */
            ELSE ASSIGN ttAuthTypeConfig.EpisodeUpdAllowed = FALSE.
          END.
          ELSE 
            ASSIGN ttAuthTypeConfig.ActivateEpisodeNumber = FALSE
                   ttAuthTypeConfig.EpisodeUpdAllowed     = FALSE.
        END.  /* IF lValidRule THEN */
        ELSE 
          ASSIGN ttAuthTypeConfig.ActivateEpisodeNumber = FALSE
                 ttAuthTypeConfig.EpisodeUpdAllowed     = FALSE. 
        
        /* 
           Override above if the specific Auth Type Episode Number is not activated
        */
        IF NOT tt_auth_type.activate_episode_number 
        THEN 
          ASSIGN ttAuthTypeConfig.ActivateEpisodeNumber = FALSE.
        
        
        FIND FIRST tt_auth_type_control NO-LOCK NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
       
        IF AVAILABLE tt_auth_type_control THEN 
        DO:                                                       
          ASSIGN ttAuthTypeConfig.AgeRangeBothObj          = tt_auth_type_control.age_range_both_obj  
                 ttAuthTypeConfig.AgeRangeFemaleObj        = tt_auth_type_control.age_range_female_obj
                 ttAuthTypeConfig.AgeRangeMaleObj          = tt_auth_type_control.age_range_male_obj
                 ttAuthTypeConfig.ControlTypeIndicator     = tt_auth_type_control.control_type_indicator
                 ttAuthTypeConfig.ClaimCodes               = tt_auth_type_control.claim_codes_header
                 ttAuthTypeConfig.ClaimTypes               = tt_auth_type_control.claim_types_header
                 ttAuthTypeConfig.Gender                   = tt_auth_type_control.gender
                 ttAuthTypeConfig.Period                   = tt_auth_type_control.period
                 ttAuthTypeConfig.PeriodType               = tt_auth_type_control.period_type
                 ttAuthTypeConfig.PeriodOverride           = tt_auth_type_control.period_override
                 ttAuthTypeConfig.AmountAuth               = tt_auth_type_control.amount_auth
                 ttAuthTypeConfig.QuantityAuth             = tt_auth_type_control.quantity_auth
                 ttAuthTypeConfig.ClaimCodesDisallow       = tt_auth_type_control.claim_codes_disallow
                 ttAuthTypeConfig.ClaimTypesDisallow       = tt_auth_type_control.claim_types_disallow
                 ttAuthTypeConfig.UsageQuantity            = tt_auth_type_control.usage_quantity
                 ttAuthTypeConfig.UsagePeriod              = tt_auth_type_control.usage_period
                 ttAuthTypeConfig.UsagePeriodType          = tt_auth_type_control.usage_period_type
                 ttAuthTypeConfig.UsageOverrideUser        = tt_auth_type_control.usage_override_user
                 ttAuthTypeConfig.AuthTypeRestrictions     = tt_auth_type_control.auth_type_restrictions
                 ttAuthTypeConfig.ActivateAuthorisedValues = tt_auth_type_control.activate_authorised_values
                 ttAuthTypeConfig.EnforceAuthorisedValues  = tt_auth_type_control.enforce_authorised_values
                 ttAuthTypeConfig.UsageType                = tt_auth_type_control.usage_type.

          /* 
            Calculate the Auth End date for Fixed period types
          */
          IF tt_auth_type_control.period_type <> "":U THEN
          DO:
            FIND FIRST mic_acronym NO-LOCK
                 WHERE mic_acronym.category_key = "ma_acAuthPeriod":U
                   AND mic_acronym.acronym_key  = tt_auth_type_control.period_type
              NO-ERROR.
             
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

            IF AVAILABLE mic_acronym 
            THEN DO:
              /*
                Benefit years for a scheme runs from January - December for each year.
                If Authorisation Date = 2021/01/01 or e.g. 2021/05/01 and Benefit year is 1, Authorisation End Date must be 2021/12/31.
                If Authorisation Date = 2021/01/01 or e.g. 2021/05/01 and Benefit year is 2, Authorisation End Date must be 2022/12/31.
              */
              IF mic_acronym.acronym_label = "Benefit Years":U 
              THEN DO:
                /* Subtract 1 as date must be at end of period added's year */
                ASSIGN dEndYear                     = YEAR(ipdEffectiveDate) + tt_auth_type_control.period - 1 
                       ttAuthTypeConfig.AuthEndDate = DATE(12,31,dEndYear).
              END.  /* IF mic_acronym.acronym_label = "Benefit Years":U */
              ELSE 
                ASSIGN ttAuthTypeConfig.AuthEndDate = ADD-INTERVAL(ipdEffectiveDate,tt_auth_type_control.period,mic_acronym.acronym_label).
            END.  /* IF AVAILABLE mic_acronym */
            ELSE 
              ASSIGN ttAuthTypeConfig.AuthEndDate = ?.

          END. /* IF tt_auth_type_control.period_type <> "":U THEN */   

        END.  /* IF AVAILABLE tt_auth_type_control  */
        ELSE 
          ASSIGN ttAuthTypeConfig.AuthEndDate = ?.

        IF CAN-FIND(FIRST tt_auth_type_provider NO-LOCK) THEN
        DO:
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = ipdInsurerObj
                 AND tt_auth_type_provider.option_code   = ipiOptionCode
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = ipcProviderType 
                 AND tt_auth_type_provider.pr_type_list  MATCHES "*":U + cDisciplineString + "*":U
            NO-ERROR. 
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = ipdInsurerObj
                 AND tt_auth_type_provider.option_code   = ipiOptionCode
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = ipcProviderType 
                 AND tt_auth_type_provider.pr_type_list  = "":U
            NO-ERROR.
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = ipdInsurerObj
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = ipcProviderType 
                 AND tt_auth_type_provider.pr_type_list  = "":U
            NO-ERROR.
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = ipiOptionCode
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = ipcProviderType 
                 AND tt_auth_type_provider.pr_type_list  = "":U
            NO-ERROR.
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = ipiOptionCode
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = ipcProviderType 
                 AND tt_auth_type_provider.pr_type_list  MATCHES "*":U + cDisciplineString + "*":U
            NO-ERROR.
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = ipcProviderType 
                 AND tt_auth_type_provider.pr_type_list  MATCHES "*":U + cDisciplineString + "*":U
            NO-ERROR.          
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = ipcProviderType              
                 AND tt_auth_type_provider.pr_type_list  = "":U
            NO-ERROR.          
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = "":U
                 AND tt_auth_type_provider.pr_type_list MATCHES "*":U + cDisciplineString + "*":U
            NO-ERROR.              
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = ipiNegNum
                 AND tt_auth_type_provider.provider_type = "":U
                 AND tt_auth_type_provider.pr_type_list  = "":U
            NO-ERROR.              
        
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = 0
                 AND tt_auth_type_provider.provider_type = ipcProviderType
                 AND tt_auth_type_provider.pr_type_list  = "":U
            NO-ERROR.              
        
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = 0
                 AND tt_auth_type_provider.provider_type = ipcProviderType
                 AND tt_auth_type_provider.pr_type_list  MATCHES "*":U + cDisciplineString + "*":U
            NO-ERROR.              
        
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE tt_auth_type_provider
          THEN   
          FIND FIRST tt_auth_type_provider NO-LOCK
               WHERE tt_auth_type_provider.insurer_obj   = 0.00
                 AND tt_auth_type_provider.option_code   = 0
                 AND tt_auth_type_provider.neg_num       = 0
                 AND tt_auth_type_provider.provider_type = "":U
                 AND tt_auth_type_provider.pr_type_list  = "":U
            NO-ERROR.              
        
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF AVAILABLE tt_auth_type_provider THEN 
          DO:

            ASSIGN ttAuthTypeConfig.OptionCode                  =  tt_auth_type_provider.option_code
                   ttAuthTypeConfig.Mandatory                   =  tt_auth_type_provider.mandatory
                   ttAuthTypeConfig.NumberProvidersAllowed      =  tt_auth_type_provider.number_providers_allowed
                   ttAuthTypeConfig.MainProvider                =  tt_auth_type_provider.main_provider
                   ttAuthTypeConfig.AuthorisedService           =  tt_auth_type_provider.authorised_service
                   
                   ttAuthTypeConfig.ClaimCodes                  =  (IF tt_auth_type_provider.claim_codes_provider <> "":U
                                                                    THEN tt_auth_type_provider.claim_codes_provider
                                                                    ELSE 
                                                                      (IF AVAILABLE tt_auth_type_control
                                                                       THEN tt_auth_type_control.claim_codes_header
                                                                       ELSE "" ))
                                                             
                   ttAuthTypeConfig.ClaimTypes                  =  (IF tt_auth_type_provider.claim_types_provider <> "":U
                                                                   THEN tt_auth_type_provider.claim_types_provider
                                                                   ELSE
                                                                     (IF AVAILABLE tt_auth_type_control
                                                                       THEN tt_auth_type_control.claim_types_header
                                                                       ELSE "" )) 
                                                                                                                
                   ttAuthTypeConfig.DefaultClaimCodeDetail      =  tt_auth_type_provider.default_claim_code_detail 
                   ttAuthTypeConfig.DefaultClaimTypeDetail      =  tt_auth_type_provider.default_claim_type_detail
                   ttAuthTypeConfig.AmountAuth                  =  tt_auth_type_provider.amount_auth
                   ttAuthTypeConfig.QuantityAuth                =  tt_auth_type_provider.quantity_auth
                   ttAuthTypeConfig.HeaderValuesAllowed         =  tt_auth_type_provider.header_values_allowed
                   ttAuthTypeConfig.HeaderValuesUnlimited       =  tt_auth_type_provider.header_values_unlimited
                   ttAuthTypeConfig.AuthoriseDetailLines        =  tt_auth_type_provider.authorise_detail_lines
                   ttAuthTypeConfig.ClaimCodesDisallow          =  tt_auth_type_provider.claim_codes_disallow
                   ttAuthTypeConfig.ClaimTypesDisallow          =  tt_auth_type_provider.claim_types_disallow
                   ttAuthTypeConfig.DocNumMandatory             =  tt_auth_type_provider.doc_num_mandatory
                   ttAuthTypeConfig.ProviderType                =  tt_auth_type_provider.provider_type
                   ttAuthTypeConfig.PrTypeList                  =  tt_auth_type_provider.pr_type_list
                   ttAuthTypeConfig.PrTypeValidList             =  tt_auth_type_provider.pr_type_valid_list
                   ttAuthTypeConfig.ProviderTypeIndicator       =  tt_auth_type_provider.provider_type_indicator

                   ttAuthTypeConfig.ValidClaimCodesDetail       =  tt_auth_type_provider.valid_claim_codes_detail
                   ttAuthTypeConfig.ValidClaimTypesDetail       =  tt_auth_type_provider.valid_claim_types_detail

                   ttAuthTypeConfig.DefaultAuthStatus           =  tt_auth_type_provider.default_auth_status          WHEN tt_auth_type_provider.default_auth_status          <> "":U
                   ttAuthTypeConfig.DefaultAuthStatusNote       =  tt_auth_type_provider.default_auth_status_note     WHEN tt_auth_type_provider.default_auth_status          <> "":U
                   ttAuthTypeConfig.DefaultAuthStatusUpdUser    =  tt_auth_type_provider.default_auth_status_upd_user WHEN tt_auth_type_provider.default_auth_status_upd_user <> "":U
                   ttAuthTypeConfig.DefaultAuthStatusUpdRole    =  tt_auth_type_provider.default_auth_status_upd_role WHEN tt_auth_type_provider.default_auth_status_upd_role <> "":U

                   ttAuthTypeConfig.ArsRateUpdAllowed           = IF (tt_auth_type_provider.ars_rate_upd_role    = "":U AND tt_auth_type_provider.ars_rate_upd_user = "":U)                        OR 
                                                                  (tt_auth_type_provider.ars_rate_upd_user   <> "":U AND CAN-DO(tt_auth_type_provider.ars_rate_upd_user, mipEnv:miUser:UserCode))  OR 
                                                                  (tt_auth_type_provider.ars_rate_upd_role   <> "":U AND fnUserHasRole(tt_auth_type_provider.ars_rate_upd_role)) THEN TRUE ELSE FALSE
                   ttAuthTypeConfig.ArsRateUpdAllowedRole       =  tt_auth_type_provider.ars_rate_upd_role
                   ttAuthTypeConfig.ArsRateUpdAllowedUser       =  tt_auth_type_provider.ars_rate_upd_user
                   ttAuthTypeConfig.BaseRateUpdAllowed          = IF (tt_auth_type_provider.base_rate_upd_role   = "":U AND tt_auth_type_provider.base_rate_upd_user = "":U)                       OR
                                                                  (tt_auth_type_provider.base_rate_upd_user  <> "":U AND CAN-DO(tt_auth_type_provider.base_rate_upd_user, mipEnv:miUser:UserCode)) OR
                                                                  (tt_auth_type_provider.base_rate_upd_role  <> "":U AND fnUserHasRole(tt_auth_type_provider.base_rate_upd_role)) THEN TRUE ELSE FALSE
                   ttAuthTypeConfig.BaseRateUpdAllowedRole      =  tt_auth_type_provider.base_rate_upd_role
                   ttAuthTypeConfig.BaseRateUpdAllowedUser      =  tt_auth_type_provider.base_rate_upd_user 
                   ttAuthTypeConfig.EnforceHeaderClaimCodeMatch =  tt_auth_type_provider.enforce_header_claim_code_match                                                   
                   ttAuthTypeConfig.EnforceHeaderClaimTypeMatch =  tt_auth_type_provider.enforce_header_claim_type_match
                   ttAuthTypeConfig.NegNum                      =  tt_auth_type_provider.neg_num
                   .
             
                IF tt_auth_type_provider.default_auth_status_upd_user <> "":U    
                THEN ASSIGN ttAuthTypeConfig.DefaultAuthStatusUpdAllowed = LOOKUP(tt_auth_type_provider.default_auth_status_upd_user, mipEnv:miUser:UserCode) > 0.

                IF tt_auth_type_provider.default_auth_status_upd_role <> "":U    
                THEN ASSIGN ttAuthTypeConfig.DefaultAuthStatusUpdAllowed = fnUserHasRole(tt_auth_type_provider.default_auth_status_upd_role).

          END. /*IF AVAILABLE tt_auth_type_provider THEN */         

        END. /* IF CAN-FIND(FIRST tt_auth_type_provider NO-LOCK) THEN */         
        
        VALIDATE ttAuthTypeConfig.
        
        /*
          Cache the config record
        */
        IF NOT CAN-FIND(FIRST ttConfigCache NO-LOCK WHERE ttConfigCache.AuthTypeConfigKey = cKey) THEN 
        DO:
          CREATE ttConfigCache.
          BUFFER-COPY ttAuthTypeConfig TO ttConfigCache.
        END. /*IF NOT CAN-FIND(FIRST ttConfigCache NO-LOCK*/
        
      END. /*FOR EACH tt_auth_type NO-LOCK:*/                
    END. /*IF NOT AVAILABLE ttConfigCache*/
    ELSE
    DO:
      CREATE ttAuthTypeConfig.
      BUFFER-COPY ttConfigCache 
               TO ttAuthTypeConfig.
    END. /*DO:*/
    
  END. /*IF NOT CAN-FIND(FIRST ttAuthTypeConfig NO-LOCK */
  
&ENDIF  
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "DATASET dsAuthType:EMPTY-DATASET().
    
                IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}
