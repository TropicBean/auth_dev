/* maauthservgetratecontrol.i MEDSTAR Medical Aid System
                              Healthcare Auth data access service: getRateControl                              
                              (c) Copyright 2018 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
------------------------------------------------------------------------------
  Purpose:  Check whether any Rate Control setups apply for the auth according to the
            setups on the Rate Control tables and the values captured on the auth.

  Notes  :  Set iplIgnoreDefaultTriggers to TRUE if you want to ignore the hac_auth_rate_detail
            record trigger checks where the ICD/CPT/Flag/Crosswalk codes are checked to
            trigger the rate change
            
            Please note , temp tables tt_trigger, tt_exclusion and tt_day_clinic_exclusion are 
            defined in maauthservicestack.p
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcOwningEntityMnemonic      AS CHARACTER   NO-UNDO.
  DEFINE INPUT        PARAMETER ipdOwningEntityObj           AS DECIMAL     NO-UNDO.
  DEFINE INPUT        PARAMETER iplIgnoreDefaultTriggers     AS LOGICAL     NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthFlagValue.

  DEFINE       OUTPUT PARAMETER opcProviderOverrideBaseRate  AS CHARACTER   NO-UNDO.
  DEFINE       OUTPUT PARAMETER opcProviderOverrideArsRate   AS CHARACTER   NO-UNDO.
  DEFINE       OUTPUT PARAMETER opcCodeLinkCategory          AS CHARACTER   NO-UNDO.
  DEFINE       OUTPUT PARAMETER opdAuthRateControlObj        AS DECIMAL     NO-UNDO.
&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE oAuthRateControlSearch               AS cls.maauthratesearch      NO-UNDO.
  DEFINE VARIABLE oErrorObject                         AS cls.maerrorobject         NO-UNDO.
  DEFINE VARIABLE hErrorHandle                         AS HANDLE                    NO-UNDO.
                                                       
  DEFINE VARIABLE cAuthBodyRegion                      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthRateControlCodeLinkCategory     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthRateControlMessage              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthRateControlOverrideBaseRate     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthRateControlOverrideArsRate      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthRateControlAssocPrTypeList      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthRateControlBodyRegionExclusion  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthRateControlDescription          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCurrentBaseRate                     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCurrentArsRate                      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCptList                             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDriverCptList                       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDefaultBaseRate                     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDefaultArsRate                      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNegGroup                            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNewRate                             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCurrentOverrideBaseRate             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCurrentOverrideArsRate              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPreviousRate                        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cUserMessage                         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dAuthRateControlObj                  AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dMainWorkGroupObj                    AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE iNegNum                              AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iSearch                              AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iSearchDocNum                        AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iProviderCounter                     AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE lAuthRateControlFound                AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lAuthRateControlRevertDisabled       AS LOGICAL                   NO-UNDO.  
  DEFINE VARIABLE lOverrideApplied                     AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lSuccess                             AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lTriggerFound                        AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lWorkGroupMatch                      AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE oAcronymHelper                       AS cls.mipacronym            NO-UNDO.

  DEFINE BUFFER tt_auth              FOR tt_auth .
  DEFINE BUFFER tt_auth_provider     FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_provider    FOR tt_auth_provider.
  DEFINE BUFFER tt_auth_coding       FOR tt_auth_coding.
  DEFINE BUFFER tt_auth_rate_control FOR tt_auth_rate_control.
  DEFINE BUFFER tt_auth_rate_detail  FOR tt_auth_rate_detail.
  DEFINE BUFFER tt_auth_flag_value   FOR tt_auth_flag_value.
  DEFINE BUFFER tt_auth_crosswalk    FOR tt_auth_crosswalk.
  DEFINE BUFFER tt_trigger           FOR tt_trigger.
  DEFINE BUFFER tt_exclusion         FOR tt_exclusion.

  ASSIGN hErrorHandle              = IF ipcOwningEntityMnemonic = "hataf":U 
                                     THEN TEMP-TABLE tt_auth_flag_value_error:HANDLE
                                     ELSE TEMP-TABLE tt_auth_error:HANDLE                                       
         oErrorObject              = NEW cls.maerrorobject(hErrorHandle) .


 
  /*
    Find first auth
  */
  FIND FIRST tt_auth NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  /*
    If there's no auth record, just leave
  */
  IF NOT AVAILABLE tt_auth 
  THEN
    RETURN.

  /* 
    Now try to find a main provider 
  */
  FIND FIRST tt_auth_provider
       WHERE tt_auth_provider.auth_obj      = tt_auth.auth_obj
         AND tt_auth_provider.main_provider = TRUE NO-ERROR.

 { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE tt_auth_provider THEN
  DO:
   /****
     Determine the current base and ars rates
   ****/
   ASSIGN cAuthBodyRegion          = tt_auth.body_region
          cCurrentOverrideBaseRate = tt_auth_provider.override_base_rate 
          cCurrentOverrideArsRate  = tt_auth_provider.override_ars_rate  
          cDefaultBaseRate         = tt_auth_provider.default_base_rate 
          cDefaultArsRate          = tt_auth_provider.default_ars_rate
          dMainWorkGroupObj        = DECIMAL(ENTRY(1,tt_auth_provider._workgroup_obj))
                                   
          lOverrideApplied         =  IF  (cCurrentOverrideBaseRate <> "":U             OR cCurrentOverrideArsRate  <> "":U )
                                      AND (cCurrentOverrideBaseRate <> cDefaultBaseRate OR cCurrentOverrideArsRate  <> cDefaultArsRate )
                                      THEN TRUE
                                      ELSE FALSE.

   
   /*
     If default rates are not populated on the provider record, we need to fetch it .
   */
   IF  cDefaultBaseRate = "":U
   AND cDefaultArsRate  = "":U  
   THEN
      mipEnv:Health:maDoctor:getProviderBaseRates(INPUT  tt_auth_provider.doc_num, 
                                                  INPUT  tt_auth.mem_num, 
                                                  INPUT  tt_auth.option_code, 
                                                  INPUT  tt_auth_provider.start_date, 
                                                  OUTPUT cDefaultBaseRate , 
                                                  OUTPUT cDefaultArsRate ).

   /*
     If this is group provider, search for negotiation number by group doc num
   */
   ASSIGN iSearchDocNum = IF   tt_auth_provider.group_doc_num <> 0
                          THEN tt_auth_provider.group_doc_num
                          ELSE tt_auth_provider.doc_num.


   mipEnv:Health:maDoctor:getNegotiationGroup(INPUT  iSearchDocNum, 
                                              INPUT  tt_auth.mem_num, 
                                              INPUT  tt_auth.option_code, 
                                              OUTPUT cNegGroup).
   
   ASSIGN iNegNum = INTEGER(LEFT-TRIM(ENTRY(1,cNegGroup,":"),"(")) NO-ERROR.

   { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:76' &ResetIgnoredErrors = TRUE }
   
   /****
     Fetch Rate Control data
   ****/
   /*
     Now let's set filter criteria and get ready to fetch the auth rate control record 
   */
   DATASET dsAuthRateControl:EMPTY-DATASET().

   ASSIGN oAuthRateControlSearch = NEW cls.maauthratesearch(DATASET dsAuthRateControl BY-REFERENCE) 
          lSuccess               = oAuthRateControlSearch:SetCriteria("BufferList":U, "tt_auth_rate_control,tt_auth_rate_detail":U)

          lSuccess =                                               oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.insurer_obj":U             , "=":U , tt_auth.insurer_obj)
          lSuccess =                                               oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.insurer_obj":U             , "=":U , 0)
          lSuccess =                                               oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.option_code":U             , "=":U , tt_auth.option_code)
          lSuccess =                                               oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.option_code":U             , "=":U , 0)
          lSuccess =                                               oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.effective_date":U          , "<=":U, tt_auth_provider.start_date)
          lSuccess =                                               oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.main_provider_neg_num":U   , "=":U , 0)
          lSuccess = IF tt_auth_provider.end_date     <> ?    THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.end_date":U                , ">=":U, tt_auth_provider.end_date)       ELSE TRUE
          lSuccess = IF tt_auth_provider.end_date     <> ?    THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.end_date":U                , "=":U , "?":U)                           ELSE TRUE
          lSuccess =                                               oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.workgroup_obj":U           , "=":U , dMainWorkGroupObj) 
          .

    SEARCH-BLK:
    DO iSearch = 1 TO 3:
      CASE iSearch:
        WHEN 1 THEN
        DO:
          ASSIGN
            lSuccess = IF cDefaultBaseRate  <> "":U THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.main_provider_base_rate":U , "=":U , cDefaultBaseRate)                ELSE TRUE
            lSuccess = IF cDefaultArsRate   <> "":U THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.main_provider_ars_rate":U  , "=":U , cDefaultArsRate)                 ELSE TRUE
            lSuccess = IF  iNegNum          <> ? 
                       AND iNegNum          <> 0    THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.main_provider_neg_num":U   , "=":U , iNegNum )            ELSE TRUE .
    
        END. //WHEN 1
        WHEN 2 THEN
        DO: 
           ASSIGN
             lSuccess = oAuthRateControlSearch:removeFilterCriteria("tt_auth_rate_control.main_provider_base_rate":U)
             lSuccess = oAuthRateControlSearch:removeFilterCriteria("tt_auth_rate_control.main_provider_ars_rate":U)
             lSuccess = IF  iNegNum          <> ? 
                        AND iNegNum          <> 0    THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.main_provider_neg_num":U   , "=":U , iNegNum )  ELSE TRUE  .
           
        END. // WHEN 2
        WHEN 3 THEN
        DO:
           ASSIGN
             lSuccess = oAuthRateControlSearch:removeFilterCriteria("tt_auth_rate_control.main_provider_neg_num":U)
             lSuccess = IF cDefaultBaseRate  <> "":U THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.main_provider_base_rate":U , "=":U , cDefaultBaseRate)   ELSE TRUE
             lSuccess = IF cDefaultBaseRate  <> "":U THEN oAuthRateControlSearch:SetFilterCriteria("tt_auth_rate_control.main_provider_ars_rate":U  , "=":U , cDefaultArsRate)    ELSE TRUE.
           
        END. // WHEN 3
      END CASE. // CASE iSearch

      oAuthRateControlSearch:fetchData().

      FIND FIRST tt_auth_rate_control NO-ERROR .
    
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE tt_auth_rate_control 
      THEN
        LEAVE SEARCH-BLK.

    END. // SEARCH-BLK:


    
    /****
      Now use the most recent auth rate control record and check for triggers/exclusions 
    ****/
    EMPTY TEMP-TABLE tt_trigger.
    EMPTY TEMP-TABLE tt_exclusion.
    EMPTY TEMP-TABLE tt_day_clinic_exclusion.

    AUTH-RATE-CONTROL-BLK:
    FOR EACH tt_auth_rate_control 
          BY tt_auth_rate_control.effective_date DESCENDING:

      ASSIGN lAuthRateControlFound               =     TRUE
             cAuthRateControlBodyRegionExclusion =     tt_auth_rate_control.body_region_exclusion
             cAuthRateControlOverrideBaseRate    =     tt_auth_rate_control.override_base_rate
             cAuthRateControlOverrideArsRate     =     tt_auth_rate_control.override_ars_rate
             cAuthRateControlCodeLinkCategory    =     tt_auth_rate_control.code_link_category
             cAuthRateControlMessage             =     tt_auth_rate_control.rate_change_message
             dAuthRateControlObj                 =     tt_auth_rate_control.auth_rate_control_obj
             cAuthRateControlAssocPrTypeList     =     tt_auth_rate_control.associated_pr_type_list
             lAuthRateControlRevertDisabled      = NOT tt_auth_rate_control.revert_rate_change 
             cAuthRateControlDescription         =     tt_auth_rate_control.rate_control_description .


      /*
        Do trigger/exclusion checks for automatic rate change 
      */
      IF NOT iplIgnoreDefaultTriggers THEN
      DO:
        /*
          Body region exclusion check
        */
        IF  cAuthRateControlBodyRegionExclusion <> "":U
        AND cAuthRateControlBodyRegionExclusion  = cAuthBodyRegion THEN
        DO:
          ASSIGN oAcronymHelper = NEW cls.mipacronym(?, FALSE, "ma_acBodyRegion":U, ?).

          oAcronymHelper:focusAcronym("KEY":U, cAuthBodyRegion) NO-ERROR.
          
          {&ResetError}
          
          CREATE tt_exclusion.
          
          ASSIGN tt_exclusion.entity      = "Auth Body Region":U
                 tt_exclusion.entity_code = IF oAcronymHelper:AcronymInFocus THEN oAcronymHelper:AcronymLabel ELSE cAuthBodyRegion.
                 
        END. /* IF  cAuthRateControlBodyRegionExclusion <> "":U AND cAuthRateControlBodyRegionExclusion  = cAuthBodyRegion  */
         
        
        /*
          Coding triggers/exclusions
        */
        CODING-TRIGGER-BLK:
        FOR EACH tt_auth_coding 
           WHERE tt_auth_coding.auth_obj       = tt_auth.auth_obj
             AND tt_auth_coding.record_action <> "DELETE":U :
          
          /*
            Check if rate control trigger only applies to main/primary coding
          */
          IF  (tt_auth_rate_control.primary_code 
          AND NOT tt_auth_coding.primary_code)
          OR (tt_auth_rate_control.main_code
          AND NOT tt_auth_coding.main_code)
          THEN
            NEXT CODING-TRIGGER-BLK.  
          
          /*
            Try to find a auth rate detail record matching the coding record in the buffer
          */
          FIND FIRST  tt_auth_rate_detail 
               WHERE  tt_auth_rate_detail.auth_rate_control_obj  =  tt_auth_rate_control.auth_rate_control_obj
                 AND  tt_auth_rate_detail.owning_entity_mnemonic =  tt_auth_coding.owning_entity_mnemonic
                 AND  tt_auth_rate_detail.owning_alt_value       =  tt_auth_coding.owning_alt_value 
                 AND  tt_auth_rate_detail.effective_date         <= tt_auth_coding.start_date
                 AND (tt_auth_rate_detail.end_date               >= tt_auth_coding.end_date  
                  OR  tt_auth_rate_detail.end_date                = ? )  NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  
          IF  AVAILABLE tt_auth_rate_detail THEN
          DO:
            /*
              Check if this is a trigger or an exclusion
            */
            IF NOT tt_auth_rate_detail.exclusion THEN
            DO:
              CREATE tt_trigger.
              ASSIGN tt_trigger.entity      = "Coding":U
                     tt_trigger.entity_code = tt_auth_rate_detail.owning_alt_value.

            END. /* IF NOT tt_auth_rate_detail.exclusion */
            ELSE DO:
              CREATE tt_exclusion.
              ASSIGN tt_exclusion.entity      = "Coding":U
                     tt_exclusion.entity_code = tt_auth_rate_detail.owning_alt_value.
            END. /* ELSE DO */                   
          END. /* AVAILABLE tt_auth_rate_detail ....  */
        END. /* CODING-TRIGGER-BLK */
        
        /*
          If a relative value type is specified , we will assume we are working with a dayclinic case and we need to check
          that all coding on the auth also exists on the rate control setups
        */
        IF tt_auth_rate_control.cpt_relative_value_type <> "":U  THEN 
        DO:
          CODING-DAYCLINIC-BLK:
          FOR EACH tt_auth_coding 
             WHERE tt_auth_coding.auth_obj               = tt_auth.auth_obj
               AND tt_auth_coding.owning_entity_mnemonic = "hlmck":U
               AND tt_auth_coding.record_action <> "DELETE":U :
             
            FIND FIRST  tt_auth_rate_detail 
                 WHERE  tt_auth_rate_detail.auth_rate_control_obj  =  tt_auth_rate_control.auth_rate_control_obj
                   AND  tt_auth_rate_detail.owning_entity_mnemonic =  tt_auth_coding.owning_entity_mnemonic
                   AND  tt_auth_rate_detail.owning_alt_value       =  tt_auth_coding.owning_alt_value 
                   AND  tt_auth_rate_detail.effective_date         <= tt_auth_coding.start_date
                   AND (tt_auth_rate_detail.end_date               >= tt_auth_coding.end_date  
                    OR  tt_auth_rate_detail.end_date                = ? )  NO-ERROR.
          
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
          
            IF NOT AVAILABLE tt_auth_rate_detail THEN
            DO:
              CREATE tt_day_clinic_exclusion.
              ASSIGN tt_day_clinic_exclusion.entity      = "Coding":U
                     tt_day_clinic_exclusion.entity_code = tt_auth_coding.owning_alt_value.
                     
            END. /* IF NOT AVAILABLE tt_auth_coding  */
          END. /* CODING-DAYCLINIC-BLK */ 
        END. /* IF tt_auth_rate_control.cpt_relative_value_type <> "":U  */


        /*
          Flag triggers/exclusions
        */
        FLAG-TRIGGER-BLK:
        FOR EACH tt_auth_flag_value
           WHERE tt_auth_flag_value.owning_entity_mnemonic = "hatau":U
             AND tt_auth_flag_value.owning_obj             = tt_auth.auth_obj:

          /*
            Try to find a auth rate detail record matching the flag record in the buffer
          */
          FIND FIRST  tt_auth_rate_detail 
               WHERE  tt_auth_rate_detail.auth_rate_control_obj   =  tt_auth_rate_control.auth_rate_control_obj
                 AND  tt_auth_rate_detail.owning_entity_mnemonic  =  "hacar":U
                 AND  tt_auth_rate_detail.owning_obj              =  tt_auth_flag_value.auth_rule_obj              
                 AND  tt_auth_rate_detail.flag_value              =  tt_auth_flag_value.auth_flag_value  
                 AND  tt_auth_rate_detail.effective_date         <=  tt_auth.start_date
                 AND (tt_auth_rate_detail.end_date               >=  tt_auth.end_date  
                  OR  tt_auth_rate_detail.end_date                =  ? )  NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE tt_auth_rate_detail THEN
          DO:
            /*
              Check if this is a trigger or an exclusion
            */
            IF NOT tt_auth_rate_detail.exclusion THEN
            DO:
              CREATE tt_trigger.
              ASSIGN tt_trigger.entity      = "Flag":U
                     tt_trigger.entity_code = tt_auth_rate_detail.owning_alt_value.

            END. /* IF NOT tt_auth_rate_detail.exclusion */
            ELSE DO:
              CREATE tt_exclusion.
              ASSIGN tt_exclusion.entity      = "Flag":U
                     tt_exclusion.entity_code = tt_auth_rate_detail.owning_alt_value.
            END. /* ELSE DO */ 
          END. /* IF AVAILABLE tt_auth_rate_detail  */
        END. /* FLAG-TRIGGER-BLK */

        /*
          Crosswalk triggers/exclusions
        */
        CROSSWALK-TRIGGER-BLK:
        FOR EACH tt_auth_crosswalk 
           WHERE tt_auth_crosswalk.auth_obj = tt_auth.auth_obj:
           
          /*
            Try to find a auth rate detail record matching the crosswalk record in the buffer
          */
          FIND FIRST  tt_auth_rate_detail 
               WHERE  tt_auth_rate_detail.auth_rate_control_obj   =  tt_auth_rate_control.auth_rate_control_obj
                 AND  tt_auth_rate_detail.owning_entity_mnemonic  =  "hlmcr":U 
                 AND  tt_auth_rate_detail.owning_obj              =  tt_auth_crosswalk.crosswalk_obj              
                 AND  tt_auth_rate_detail.effective_date         <=  tt_auth.start_date
                 AND (tt_auth_rate_detail.end_date               >=  tt_auth.end_date  
                  OR  tt_auth_rate_detail.end_date                =  ? )  NO-ERROR.
        
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE tt_auth_rate_detail THEN
          DO:
            /*
              Check if this is a trigger or an exclusion
            */
            IF NOT tt_auth_rate_detail.exclusion THEN
            DO:
              CREATE tt_trigger.
              ASSIGN tt_trigger.entity      = "Crosswalk":U
                     tt_trigger.entity_code = tt_auth_rate_detail.owning_alt_value.
        
            END. /* IF NOT tt_auth_rate_detail.exclusion */
            ELSE DO:
              CREATE tt_exclusion.
              ASSIGN tt_exclusion.entity      = "Crosswalk":U
                     tt_exclusion.entity_code = tt_auth_rate_detail.owning_alt_value.
            END. /* ELSE DO */ 
          END. /* IF AVAILABLE tt_auth_rate_detail  */
        END. /* CROSSWALK-TRIGGER-BLK */
      END. /* IF NOT lIgnoreDefaultTriggers   */
      
      IF tt_auth_rate_control.cpt_relative_value_type <> "":U 
      AND NOT CAN-FIND(FIRST tt_exclusion ) THEN
      DO:

        FOR EACH tt_auth_coding 
           WHERE tt_auth_coding.owning_entity_mnemonic = "hlmck":U:

          FIND FIRST  tt_auth_rate_detail 
               WHERE  tt_auth_rate_detail.auth_rate_control_obj  =  tt_auth_rate_control.auth_rate_control_obj
                 AND  tt_auth_rate_detail.owning_entity_mnemonic =  "hlmck":U
                 AND  tt_auth_rate_detail.owning_alt_value       =  tt_auth_coding.owning_alt_value 
                 AND  tt_auth_rate_detail.effective_date         <= tt_auth_coding.start_date
                 AND (tt_auth_rate_detail.end_date               >= tt_auth_coding.end_date  
                  OR  tt_auth_rate_detail.end_date                = ? )  NO-ERROR.
                  
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
          IF AVAILABLE tt_auth_rate_detail 
          THEN       
            ASSIGN cCptList = cCptList 
                            + (IF cCptList <> "":U THEN "|":U ELSE "":U)
                            + tt_auth_coding.owning_alt_value + ",":U + tt_auth_rate_detail.override_ars_rate . 
        END. /* FOR EACH tt_auth_coding*/

        mipEnv:health:mamedical:getDriverCPT(INPUT cCptList,
                                             INPUT tt_auth.start_date,
                                             INPUT tt_auth_rate_control.cpt_relative_value_type ,
                                             OUTPUT cDriverCptList).

        IF NUM-ENTRIES(cDriverCptList, "|":U) = 1  THEN
        DO:
          FIND FIRST  tt_auth_rate_detail 
               WHERE  tt_auth_rate_detail.auth_rate_control_obj  =  tt_auth_rate_control.auth_rate_control_obj
                 AND  tt_auth_rate_detail.owning_entity_mnemonic =  "hlmck":U
                 AND  tt_auth_rate_detail.owning_alt_value       =  ENTRY(1, cDriverCptList) 
                 AND  tt_auth_rate_detail.effective_date         <= tt_auth.start_date
                 AND (tt_auth_rate_detail.end_date               >= tt_auth.end_date  
                  OR  tt_auth_rate_detail.end_date                = ? )  NO-ERROR.
                  
          IF AVAILABLE tt_auth_rate_detail 
          THEN
            ASSIGN  cAuthRateControlOverrideArsRate = tt_auth_rate_detail.override_ars_rate.

        END. /* IF NUM-ENTRIES(cDriverCptList, "|":U) = 1*/
        
      END. /* IF tt_auth_rate_control.cpt_relative_value_type <> "":U */
      /*
        After the first iteration, leave the block as we are only interested in the most recent rate control record...
      */
      LEAVE AUTH-RATE-CONTROL-BLK.
    END. /* AUTH-RATE-CONTROL-BLK */

    /****
      Now  check if override rates should apply - and handle the values of the output parameters here
    *****/
    
    /*
      If automatic revert is switched off and this main provider has already been overriden. Return immediately and return current rates
    */

    IF lAuthRateControlRevertDisabled 
    AND NOT lOverrideApplied 
    AND CAN-FIND(FIRST tt_trigger) 
    AND NOT CAN-FIND(FIRST tt_exclusion) 
    THEN
      ASSIGN lSuccess      = oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                                                   INPUT ipdOwningEntityObj,
                                                   INPUT "":U,
                                                   INPUT tt_auth.line_number,
                                                   INPUT "Please note that 'Automatically Revert Rate Change' is switched off so this Rate Change will not automatically be reversed.":U,
                                                   INPUT "WAR":U).

    /*
      Always return the code link category and auth rate control obj
    */
    ASSIGN 
      opcCodeLinkCategory          = cAuthRateControlCodeLinkCategory
      opdAuthRateControlObj        = dAuthRateControlObj.
      
      
    IF  lAuthRateControlRevertDisabled
    AND lOverrideApplied THEN
    DO:
      ASSIGN opcProviderOverrideBaseRate  = cCurrentOverrideBaseRate
             opcProviderOverrideArsRate   = cCurrentOverrideArsRate.

         
       //And leave the procedure ...
        RETURN.
        
    END. /* IF  lAuthRateControlRevertDisabled AND lOverrideApplied */
    
    IF iplIgnoreDefaultTriggers THEN
    DO:
      /*
        Manual
      
        No need to check triggers/exclusions - just check if a rate control record was found or not
      */
      IF lAuthRateControlFound 
      THEN
        ASSIGN opcProviderOverrideBaseRate = cAuthRateControlOverrideBaseRate
               opcProviderOverrideARSRate  = cAuthRateControlOverrideArsRate.
      ELSE
        ASSIGN opcProviderOverrideBaseRate = "":U
               opcProviderOverrideArsRate  = "":U.


    END. /* IF iplIgnoreDefaultTriggers */
    ELSE DO:
      /*
        Automatic
      */
      IF  NOT CAN-FIND(FIRST tt_exclusion) 
      AND NOT CAN-FIND(FIRST tt_day_clinic_exclusion)      
      AND     CAN-FIND(FIRST tt_trigger) THEN
      DO:
        /*
          A trigger was found with no exclusions, return override rates 
        */
        ASSIGN opcProviderOverrideBaseRate = cAuthRateControlOverrideBaseRate
               opcProviderOverrideARSRate  = cAuthRateControlOverrideArsRate.
  
      END. /* IF NOT CAN-FIND(FIRST tt_exclusion) AND CAN-FIND(FIRST tt_trigger) */
      ELSE
      DO:
        /*
          Either no trigger was found or an exclusion exists - no override rates will be applied
        */
        ASSIGN opcProviderOverrideBaseRate  = "":U
               opcProviderOverrideArsRate   = "":U.
 
      END. /* ELSE DO: */
    END. /* ELSE DO: */
    
    /***
      Now finally handle all warnings/errors that need to be returned .
    ****/
    
    /*
      Manual rate change warnings/errors
    */
    IF iplIgnoreDefaultTriggers THEN
    DO:
      IF lAuthRateControlFound 
      THEN
        ASSIGN cPreviousRate = IF lOverrideApplied THEN cCurrentOverrideBaseRate + " - ":U + cCurrentOverrideArsRate
                                                    ELSE cDefaultBaseRate         + " - ":U + cDefaultArsRate
                cNewRate      = opcProviderOverrideBaseRate + " - ":U +  opcProviderOverrideArsRate
                cUserMessage  = "Manual Override.":U + 
                                IF cAuthRateControlMessage <> "":U 
                                THEN SUBSTITUTE(cAuthRateControlMessage,cPreviousRate,cNewRate)
                                ELSE SUBSTITUTE("The base rate (&1) is no longer applicable. The new base rate (&2) is now applicable.",cPreviousRate,cNewRate) 
                lSuccess      = oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                                                      INPUT ipdOwningEntityObj,
                                                      INPUT "":U,
                                                      INPUT tt_auth.line_number,
                                                      INPUT cUserMessage,
                                                      INPUT "WAR":U).
      ELSE 
        ASSIGN  cUserMessage  = SUBSTITUTE("No rate controls were found. The default rate (&1) is still applicable.",cDefaultBaseRate + " - ":U +  cDefaultArsRate) 
                lSuccess      = oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                                                      INPUT ipdOwningEntityObj,
                                                      INPUT "":U,
                                                      INPUT tt_auth.line_number,
                                                      INPUT cUserMessage,
                                                      INPUT "ERR":U).
    END. /* IF iplIgnoreDefaultTriggers */
    
    /*
      Automatic rate change warnings/errors
    */
    IF NOT iplIgnoreDefaultTriggers THEN
    DO:
      /*
        If the new override rates differ from original override rates on the provider record and a rate control record was found
      */
      IF  opcProviderOverrideBaseRate <> cCurrentOverrideBaseRate
      AND opcProviderOverrideArsRate  <> cCurrentOverrideArsRate 
      AND opcProviderOverrideBaseRate <> "":U
      AND lAuthRateControlFound
      THEN
        ASSIGN  cPreviousRate = IF lOverrideApplied THEN cCurrentOverrideBaseRate + " - ":U + cCurrentOverrideArsRate
                                                    ELSE cDefaultBaseRate         + " - ":U + cDefaultArsRate
                cNewRate      = opcProviderOverrideBaseRate + " - ":U +  opcProviderOverrideArsRate
                cUserMessage  = IF cAuthRateControlMessage <> "":U 
                                THEN SUBSTITUTE(cAuthRateControlMessage,cPreviousRate,cNewRate) 
                                ELSE SUBSTITUTE("The base rate (&1) is no longer applicable. The new base rate (&2) is now applicable.",cPreviousRate,cNewRate) 
                lSuccess      = oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                                                      INPUT ipdOwningEntityObj,
                                                      INPUT "":U,
                                                      INPUT tt_auth.line_number,
                                                      INPUT cUserMessage,
                                                      INPUT "WAR":U).
      /*
        If override rates exist on the main provider, but they are being reverted back to blank - either because no rate control was found or exclusion exists
      */
      ELSE IF opcProviderOverrideBaseRate <> cCurrentOverrideBaseRate
          AND opcProviderOverrideArsRate  <> cCurrentOverrideArsRate 
          AND opcProviderOverrideBaseRate = "":U
          AND opcProviderOverrideArsRate  = "":U        
      THEN 
        ASSIGN  cNewRate      = cDefaultBaseRate + " - ":U +  cDefaultArsRate
                cUserMessage  = IF lAuthRateControlFound AND CAN-FIND(FIRST tt_exclusion ) 
                                THEN SUBSTITUTE("A rate control was found, but exclusions exist. The default rate (&1) is now applicable.",cNewRate)
                                ELSE SUBSTITUTE("No rate controls were found. The default rate (&1) is now applicable.",cNewRate) 
                                
                lSuccess      = oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                                                      INPUT ipdOwningEntityObj,
                                                      INPUT "":U,
                                                      INPUT tt_auth.line_number,
                                                      INPUT cUserMessage,
                                                      INPUT "WAR":U).
      /* 
        No overrides have been applied to the main provider and a rate control was found with a valid trigger, but ultimately an exclusion prevented it from applying
      */                              
      ELSE IF  cCurrentOverrideBaseRate    = "":U
           AND opcProviderOverrideArsRate  = "":U
           AND opcProviderOverrideBaseRate = "":U 
           AND opcProviderOverrideArsRate  = "":U
           AND lAuthRateControlFound   
           AND CAN-FIND(FIRST tt_trigger)
           AND (CAN-FIND(FIRST tt_exclusion) OR CAN-FIND(FIRST tt_day_clinic_exclusion)) 
      THEN 
        ASSIGN  cNewRate      = cDefaultBaseRate + " - ":U +  cDefaultArsRate
                cUserMessage  = SUBSTITUTE("A rate control was found, but exclusions exist. The default rate (&1) is still applicable.",cNewRate)
                lSuccess      = oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                                                      INPUT ipdOwningEntityObj,
                                                      INPUT "":U,
                                                      INPUT tt_auth.line_number,
                                                      INPUT cUserMessage,
                                                      INPUT "WAR":U).

      /*
        Only display trigger notifications if no exclusions exist
      */
      IF NOT CAN-FIND(FIRST tt_exclusion) THEN 
      DO:
        TRIGGER-NOTIFICATION-BLK:
        FOR EACH tt_trigger :
          oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                                INPUT ipdOwningEntityObj,
                                INPUT "":U,
                                INPUT tt_auth.line_number,
                                INPUT SUBSTITUTE("Rate change trigger found - &1 : &2 ",tt_trigger.entity,tt_trigger.entity_code),
                                INPUT "WAR":U).
        END. /* TRIGGER-NOTIFICATION-BLK */
      END.
      
      EXCLUSION-NOTIFICATION-BLK:
      FOR EACH tt_exclusion :
        oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                              INPUT ipdOwningEntityObj,
                              INPUT "":U,
                              INPUT tt_auth.line_number,
                              INPUT SUBSTITUTE("Rate change exclusion found - &1 : &2 ",tt_exclusion.entity , tt_exclusion.entity_code),
                              INPUT "WAR":U).        
      END. /* EXCLUSION-NOTIFICATION-BLK */

      DAY-CLINIC-EXCLUSION-NOTIFICATION-BLK:
      FOR EACH tt_day_clinic_exclusion :
        oErrorObject:addError(INPUT ipcOwningEntityMnemonic,
                              INPUT ipdOwningEntityObj,
                              INPUT "":U,
                              INPUT tt_auth.line_number,
                              INPUT SUBSTITUTE("&1 : &2 , Is not set up for &3 . Rate change will not apply",tt_day_clinic_exclusion.entity , tt_day_clinic_exclusion.entity_code , cAuthRateControlDescription),
                              INPUT "WAR":U).        
      END. /* EXCLUSION-NOTIFICATION-BLK */
    END. /* IF NOT iplIgnoreDefaultTriggers DO: */    
  END. /* IF AVAILABLE tt_auth_provider */
  ELSE DO:
    /*
      If this was called from the manual rate change, we need to return an error since they are trying to force a rate change on an auth without a main provider
    */
    IF iplIgnoreDefaultTriggers
    THEN
      oErrorObject:addError
        (INPUT ipcOwningEntityMnemonic,
         INPUT ipdOwningEntityObj,
         INPUT "":U,
         INPUT tt_auth.line_number,
         INPUT "No main provider has been captured for this auth. Can not find any rate control setups.",
         INPUT "ERR":U).
  END. /* ELSE DO - If no main provider exists */

 {mip/inc/mipcatcherror.i 
   &FINALLY="IF VALID-OBJECT(oAuthRateControlSearch) THEN DELETE OBJECT oAuthRateControlSearch .
             IF VALID-OBJECT(oAcronymHelper)         THEN DELETE OBJECT oAcronymHelper.      
             DATASET dsAuthRateControl:EMPTY-DATASET().    
             EMPTY TEMP-TABLE tt_trigger.               
             EMPTY TEMP-TABLE tt_exclusion. " }
   
&ENDIF   
   
   




