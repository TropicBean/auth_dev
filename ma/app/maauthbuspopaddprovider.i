/* maauthbuspopaddprovider.i  MEDSTAR Medical Aid System
                              Populate additional data for Provider Container
                              Used for performance improvement when 
                              rendering the container
                              (c) Copyright 1990 - 2021
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/       

                                      
FOR EACH btt_auth_provider:
  ASSIGN dProvStartDate    = DATE(btt_auth_provider.start_date) 
         cProviderType     = btt_auth_provider.provider_type
         iGroupDocNum      = INTEGER(btt_auth_provider.group_doc_num)
         iDocNum           = INTEGER(btt_auth_provider.doc_num) 
         dAuthProviderObj  = DECIMAL(btt_auth_provider.auth_provider_obj)
         cDiscipline       = STRING(btt_auth_provider.pr_type)  
         cSubDiscipline    = STRING(btt_auth_provider.sub_pr_type)
         dTermDate         = DATE(btt_auth_provider.end_date).

  mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                               BUFFER btt_auth_provider,
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE). 

  FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.  

  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
  FIND FIRST attending_doctor NO-LOCK
       WHERE attending_doctor.doc-num = btt_auth_provider.doc_num NO-ERROR.
       
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
       
  FIND FIRST group_doctor NO-LOCK
       WHERE group_doctor.doc-num = btt_auth_provider.group_doc_num NO-ERROR. 
       
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
       
  FIND FIRST group_discipline NO-LOCK
       WHERE group_discipline.doc-num = btt_auth_provider.group_doc_num NO-ERROR. 
       
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
  
  IF AVAILABLE group_discipline
  THEN ASSIGN              
         cDiscipline       = STRING(group_discipline.disp-code)
         cSubDiscipline    = STRING(group_discipline.subdisp-code)
         .  
         
  IF AVAILABLE group_doctor 
  THEN ASSIGN                
         cDocDesc          = group_doctor.name
         . 
         
  IF AVAILABLE attending_doctor
  THEN ASSIGN              
         cAttDocDesc       = attending_doctor.name
         . 
         
  ASSIGN lGroupProvider    = (iGroupDocNum <> 0 AND mipEnv:Health:maDoctor:isProviderAValidGroup(INPUT iGroupDocNum, INPUT dProvStartDate) OR ( (iGroupDocNum <> 0.00 AND iDocNum <> 0.00)))
         . 
  /* MMP-677 */       
  ASSIGN dQuantityPaid     = btt_auth_provider.quantity_paid
         dAmountPaid       = btt_auth_provider.amount_paid
         cStatus           = STRING(btt_auth_provider.auth_status)
         cStatusNote       = btt_auth_provider.auth_status_note

         lMandatory   = mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), 
                                                                        INPUT dInsurerObj, 
                                                                        INPUT iOption, 
                                                                        INPUT dProvStartDate)      
         /* Assign the workgroup obj for the record in the dataset */
         lSuccess = mipEnv:Health:maDoctor:getProviderWorkGroup(INPUT iOption, 
                                                                INPUT iDocNum, 
                                                                INPUT dProvStartDate, 
                                                                INPUT btt_auth_provider.provider_type,
                                                                OUTPUT cWorkgroupObjList).
                                      
  ASSIGN btt_auth_provider._group_provider              = lGroupProvider
         btt_auth_provider._status_note_mandatory       = lMandatory
         btt_auth_provider._workgroup_obj               = cWorkgroupObjList. 


  IF AVAILABLE ttAuthTypeConfig
  THEN ASSIGN 
         btt_auth_provider._status_note_upd_allow       = IF ttAuthTypeConfig.DefaultAuthStatusUpdAllowed = FALSE
                                                          THEN "Disabled":U 
                                                          ELSE "Updatable":U
         btt_auth_provider._claim_code_updateable       = IF NOT ttAuthTypeConfig.ClaimCodeUpdAllowed OR 
                                                             dQuantityPaid <> 0 OR dAmountPaid <> 0.00 
                                                          THEN "Disabled":U ELSE "Updatable":U       
         btt_auth_provider._claim_type_updateable       = IF NOT ttAuthTypeConfig.ClaimTypeUpdAllowed OR 
                                                             dQuantityPaid <> 0 OR dAmountPaid <> 0.00 
                                                          THEN "Disabled":U ELSE "Updatable":U
         btt_auth_provider._authorised_value_updateable = IF ttAuthTypeConfig.ActivateAuthorisedValues 
                                                          THEN "Updatable":U
                                                          ELSE "Disabled":U.
  ELSE ASSIGN 
         btt_auth_provider._status_note_upd_allow       = "Updatable":U
         btt_auth_provider._claim_code_updateable       = "Updatable":U       
         btt_auth_provider._claim_type_updateable       = "Updatable":U
         btt_auth_provider._authorised_value_updateable = "Disabled":U.
                                                            
  IF dAuthProviderObj <> 0.00 AND dAuthProviderObj <> ? THEN
  DO:
    RUN _getConfiguration IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                BUFFER btt_auth_provider,
                                                OUTPUT lcConfiguration).
    
    ASSIGN btt_auth_provider._provider_configuration = STRING(lcConfiguration).                                            
  END. /*IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN*/       
  
  IF iOption = 0
  THEN mipEnv:Health:maMember:getMemberOption(INPUT cMemNum, INPUT dProvStartDate, OUTPUT iOption). 
  
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                 INPUT  iOption,
                                                 INPUT  "ma_acAuthRuleTypeAuthProvider":U,
                                                 INPUT  "ProviderPayeeDefault":U,
                                                 INPUT  dProvStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
                                                 
  ASSIGN btt_auth_provider.payee_dm  = IF lValidRule AND cRuleValue <> "":U AND btt_auth_provider.payee_dm = "":U 
                                       THEN cRuleValue 
                                       ELSE btt_auth_provider.payee_dm. 

  ASSIGN iSearchDoctor = IF iGroupDocNum <> 0 THEN iGroupDocNum ELSE iDocNum.
  
  IF iOption <> 0 AND iSearchDoctor <> 0 THEN 
  DO:
    mipEnv:Health:maDoctor:getNegotiationGroup(INPUT iSearchDoctor, INPUT cMemNum, INPUT iOption, OUTPUT cNegGroup).
    
    mipEnv:Health:maDoctor:getProviderBaseRates(INPUT iSearchDoctor, INPUT cMemNum, INPUT iOption, INPUT dProvStartDate, OUTPUT cBaseRate, OUTPUT cArsRate).    
    
    ASSIGN btt_auth_provider._neg_group         = cNegGroup
           btt_auth_provider._neg_group_tooltip = "Base Rate: " + cBaseRate + "<br/> Negotiation Group: " + cNegGroup
           btt_auth_provider._base_rate         = cBaseRate
           btt_auth_provider._ars_rate          = cArsRate.
  END. /* IF iOption <> 0 */ 
  
  /*  Emergency Flags  */
  IF  btt_auth_provider.main_provider
  AND btt_auth_provider._emergency_flag = "" 
  AND (lActivatePenalty OR lActivateCopayment) THEN
  DO:
    ASSIGN dAuthRuleObj = 0
           cRuleType    = "ma_acAuthRuleTypeAuthFlag":U
           cRuleCode    = "EMERGENCY":U
           dStartDate   = btt_auth_provider.start_date.
    
    /*
      Get the auth rule obj
    */

    mipEnv:Health:AuthMaintenance:getAuthRuleDetails( INPUT dInsurerObj,
                                                      INPUT iOption,
                                                      INPUT cRuleType  ,
                                                      INPUT cRuleCode  ,
                                                      INPUT dStartDate ,
                                                      OUTPUT lValidRule ,
                                                      OUTPUT cRuleValue ,
                                                      OUTPUT dLinkAuthRuleObj,
                                                      OUTPUT dAuthRuleObj).

    FIND FIRST btt_auth_flag_value
         WHERE btt_auth_flag_value.owning_entity_mnemonic = "hatau":U
           AND btt_auth_flag_value.owning_obj             = btt_auth_provider.auth_obj 
           AND btt_auth_flag_value.owning_key             = "":U
           AND btt_auth_flag_value.auth_rule_obj          = dAuthRuleObj 
      NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

    IF AVAILABLE btt_auth_flag_value 
    THEN
      ASSIGN btt_auth_provider._emergency_flag = btt_auth_flag_value.auth_flag_value.

  END.  // IF btt_auth_provider.main_provider THEN

  /*  Penalty Flags */
  IF  btt_auth_provider.main_provider
  AND btt_auth_provider._penalty_flag = "" 
  AND lActivatePenalty  THEN
  DO:
    ASSIGN dAuthRuleObj = 0
           cRuleType    = "ma_acAuthRuleTypeAuthFlag":U
           cRuleCode    = "PENALTY":U
           dStartDate   = btt_auth_provider.start_date.
    
    /*
      Get the auth rule obj
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleDetails( INPUT dInsurerObj,
                                                      INPUT iOption,
                                                      INPUT cRuleType  ,
                                                      INPUT cRuleCode  ,
                                                      INPUT dStartDate ,
                                                      OUTPUT lValidRule ,
                                                      OUTPUT cRuleValue ,
                                                      OUTPUT dLinkAuthRuleObj,
                                                      OUTPUT dAuthRuleObj).
    
    FIND FIRST btt_auth_flag_value
         WHERE btt_auth_flag_value.owning_entity_mnemonic = "hatau":U
           AND btt_auth_flag_value.owning_obj             = btt_auth_provider.auth_obj 
           AND btt_auth_flag_value.owning_key             = "":U
           AND btt_auth_flag_value.auth_rule_obj          = dAuthRuleObj 
      NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

    IF AVAILABLE btt_auth_flag_value 
    THEN
      ASSIGN btt_auth_provider._penalty_flag = btt_auth_flag_value.auth_flag_value.

  END.  // IF btt_auth_provider.main_provider THEN
END. /* FOR EACH btt_auth_provider: */ 

IF CAN-FIND(FIRST tt_auth_provider
            WHERE tt_auth_provider.main_provider) THEN 
DO:
  FOR EACH tt_auth_provider NO-LOCK
        BY tt_auth_provider.doc_num:
   
    IF NOT tt_auth_provider.authorised_service
    THEN NEXT.
  
    FIND FIRST doctor NO-LOCK
         WHERE doctor.doc-num = tt_auth_provider.doc_num
           AND doctor.doc-num <> 0
      NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
    FIND FIRST prtype NO-LOCK
         WHERE prtype.pr-type = tt_auth_provider.pr_type
      NO-ERROR.
  
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
    IF AVAILABLE doctor OR AVAILABLE prtype
    THEN   
      ASSIGN cProviderList = cProviderList
                           + (IF cProviderList = "":U THEN "":U ELSE "^":U)
                           + (IF AVAILABLE doctor 
                              THEN UPPER(doctor.name) + " (":U + STRING(doctor.doc-num) + ")":U 
                              ELSE 
                                (IF AVAILABLE prtype 
                                 THEN UPPER(prtype.description) + " (":U + STRING(prtype.pr-type) + ")":U
                                 ELSE "":U))
                           + "=":U 
                           + STRING(tt_auth_provider.auth_provider_obj).
  END. /*FOR EACH tt_auth_provider NO-LOCK:*/
END. /* IF CAN-FIND(FIRST tt_auth_provider */

FOR EACH ham_auth_group NO-LOCK
   WHERE ham_auth_group.effective_date <= dStartDate
   AND  (ham_auth_group.end_date = ?
   OR    ham_auth_group.end_date > dStartDate)
   AND  (ham_auth_group.option_code = 0
   OR    ham_auth_group.option_code = iOption):
  ASSIGN cAuthGroupList = cAuthGroupList
                          + "|" + ham_auth_group.auth_group_code + "=":U + STRING(ham_auth_group.auth_group_obj).
END. /* FOR EACH ham_auth_group NO-LOCK */

