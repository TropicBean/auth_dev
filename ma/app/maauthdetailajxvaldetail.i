/* maauthdetailajxvaldetail.i  MEDSTAR Medical Aid System
                               Auth Detail Container Ajax Validation
                               maauthdetailuiservicestack.p -> ajaxValidationDetail
                               (c) Copyright 1990 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/ 
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cAlertMessage         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cArsRate              AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cAuthGroupObj         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cBaseRate             AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cError                AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cErrorMessage         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cErrorOrWarning       AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterField          AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterFieldList      AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterValue          AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterValueList      AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cLineRestriction      AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cMemberNumber         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cNegGroup             AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cOverrideArsRate      AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cOverrideBaseRate     AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cOwningAltValue       AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cPrType               AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReasonCode           AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReasonDescription    AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReasonType           AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cRecordRowId          AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnField          AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnFieldList      AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnValues         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cStatus               AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cStatusAction         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cSubPrType            AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cTariffCode           AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cTariffDesc           AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cTariffRequired       AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cTariffTypeCategory   AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cTariffTypeOrCategory AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cWarning              AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cWhereClause          AS CHARACTER                NO-UNDO.

  DEFINE VARIABLE dTariffCost           AS DECIMAL                  NO-UNDO.

  DEFINE VARIABLE dAuthObj              AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj        AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj      AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj          AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dInsurerObj           AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dMinutesTariffTypeObj AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dNappiLinkObj         AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj        AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dTariffObj            AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dTariffTypeObj        AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE dTrfCostObj           AS DECIMAL                  NO-UNDO.
    
  DEFINE VARIABLE dAuthProvStartDate    AS DATE                     NO-UNDO.
  DEFINE VARIABLE dStartDate            AS DATE                     NO-UNDO.
  DEFINE VARIABLE dTariffEffectDate     AS DATE                     NO-UNDO.

  DEFINE VARIABLE iClaimCode            AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iDependant            AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iFilterField          AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iOptionCode           AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iReturnField          AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iSearchDocNum         AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iStatus               AS INTEGER                  NO-UNDO.
  
  DEFINE VARIABLE lActivateMinutes      AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lMandatory            AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lValid                AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL                  NO-UNDO.
  
  DEFINE VARIABLE oAuthorisation        AS cls.maauthorisation      NO-UNDO.
  DEFINE VARIABLE oAuthSearch           AS cls.maauthsearch         NO-UNDO.
  DEFINE VARIABLE oNappi                AS cls.manappi              NO-UNDO.
  DEFINE VARIABLE oTariffLink           AS cls.matarifflink         NO-UNDO.
  
  DEFINE BUFFER buf_auth_schext FOR schext.

  &SCOPED-DEFINE AppendReturnValues ASSIGN cReturnValues = cReturnValues + (IF cReturnValues = "":U THEN "":U ELSE "|":U) +

  &SCOPED-DEFINE ValidationSuccess  ASSIGN ttValidation.cReturnValues = cReturnValues ~
                                           ttValidation.lValid        = TRUE          ~
                                           ttValidation.cMessage      = "Success".    ~
                                                                                      ~
                                    VALIDATE ttValidation.

  &SCOPED-DEFINE BlankResponse      ASSIGN ttValidation.cReturnValues = FILL("|":U, NUM-ENTRIES(cReturnFieldList) - 1) ~
                                           ttValidation.lValid        = TRUE.                                          ~
                                                                                                                       ~
                                    VALIDATE ttValidation.


  &SCOPED-DEFINE FocusAuthObject                                                            ~
    ASSIGN lSuccess   = mipEnv:Health:AuthService:getAuthObject(INPUT  dAuthObj,            ~
                                                                INPUT  "":U,                ~
                                                                OUTPUT oAuthorisation).     ~
                                                                                            ~
                                                                                            ~
    IF VALID-OBJECT(oAuthorisation)                                                         ~
    THEN                                                                                    ~
      ASSIGN dInsurerObj   = oAuthorisation:InsurerObj                                      ~
             iOptionCode   = oAuthorisation:MemberOptionCode                                ~
             cMemberNumber = oAuthorisation:MemNum                                          ~
             iDependant    = oAuthorisation:Dependant                                       ~
             dAuthTypeObj  = oAuthorisation:AuthTypeObj                                     ~
             dStartDate    = oAuthorisation:Startdate.                                      ~

  ASSIGN
    cFilterFieldList = get-value('FldLst':U)
    cFilterValueList = get-value('ValList':U)
    cReturnFieldList = get-value('RetFldList':U).

  { ma/msc/madispad.i &discipline = cPrType }

  CREATE ttValidation.

  DO iFilterField = 1 TO NUM-ENTRIES(cFilterFieldList):

    ASSIGN cFilterField = ENTRY(iFilterField, cFilterFieldList)
           cFilterValue = ENTRY(iFilterField, cFilterValueList).

    CASE cFilterField:

      WHEN "[OwningEntityMnemonic]":U THEN ASSIGN cOwningEntityMnemonic = TRIM(cFilterValue).
      WHEN "[OwningAltValue]":U       THEN ASSIGN cOwningAltValue       = TRIM(cFilterValue).
      WHEN "[PrType]":U               THEN ASSIGN cPrType               = TRIM(cFilterValue).
      WHEN "[SubPrType]":U            THEN ASSIGN cSubPrType            = TRIM(cFilterValue).
      WHEN "[ArsRate]":U              THEN ASSIGN cArsRate              = TRIM(cFilterValue).
      WHEN "[OverrideBaseRate]":U     THEN ASSIGN cOverrideBaseRate     = TRIM(cFilterValue).
      WHEN "[OverrideArsRate]":U      THEN ASSIGN cOverrideArsRate      = TRIM(cFilterValue).
      WHEN "[BaseRate]":U             THEN ASSIGN cBaseRate             = TRIM(cFilterValue).
      WHEN "[NegGroup]":U             THEN ASSIGN cNegGroup             = TRIM(cFilterValue).
      WHEN "[AuthProviderObj]":U      THEN ASSIGN dAuthProviderObj      = DECIMAL(TRIM(cFilterValue)).
      WHEN "[Status]":U               THEN ASSIGN cStatus               = cFilterValue.
      WHEN "[ReasonCode]":U           THEN ASSIGN cReasonCode           = TRIM(cFilterValue).
      WHEN "[ReasonType]":U           THEN ASSIGN cReasonType           = TRIM(cFilterValue).
      WHEN "[StartDate]":U            THEN ASSIGN dStartDate            = DATE(cFilterValue).
      WHEN "[AuthObj]":U              THEN ASSIGN dAuthObj              = DECIMAL(cFilterValue).
      WHEN "[ClaimCode]":U            THEN ASSIGN iClaimCode            = INTEGER(cFilterValue).

    END CASE.  /* CASE cFilterField: */

  END. /* DO iFilter = 1 TO NUM-ENTRIES(cFilterFields): */
  
  CASE ipcValidationArgument:
    WHEN "Detail":U THEN
    DO:
      IF dAuthProviderObj <> 0.00 THEN
      DO:
        /*
          Fetch auth data
        */
        oAuthSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).

        ASSIGN lSuccess = oAuthSearch:SetCriteria("BufferList":U , "tt_auth,tt_auth_provider":U)
               lSuccess = oAuthSearch:SetCriteria("DataSecured":U, "FALSE":U)
               lSuccess = oAuthSearch:SetFilterCriteria("tt_auth_provider.auth_provider_obj":U, "=":U, dAuthProviderObj).

        oAuthSearch:fetchData().

        FOR FIRST tt_auth_provider NO-LOCK
            WHERE tt_auth_provider.auth_provider_obj = dAuthProviderObj,
            FIRST tt_auth NO-LOCK
            WHERE tt_auth.auth_obj = tt_auth_provider.auth_obj:

          ASSIGN cPrType    = STRING(tt_auth_provider.pr_type)
                 cSubPrType = STRING(tt_auth_provider.sub_pr_type).

          { ma/msc/madispad.i &discipline = cPrType     &comment = "/* " }
          { ma/msc/madispad.i &discipline = cSubPrType  &comment = "/* " }

          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):

            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

            CASE cReturnField:
              WHEN "[PrType]":U    THEN {&AppendReturnValues} cPrType.
              WHEN "[SubPrType]":U THEN {&AppendReturnValues} cSubPrType.
              WHEN "[BaseRate]":U OR WHEN "[ArsRate]":U THEN
              DO:
                IF tt_auth_provider.override_base_rate <> "":U 
				        OR tt_auth_provider.override_ars_rate  <> "":U THEN 
				          ASSIGN cBaseRate = tt_auth_provider.override_base_rate
                         cArsRate  = tt_auth_provider.override_ars_rate.
                ELSE
                DO:
                  IF tt_auth.option_code <> 0 THEN
                    mipEnv:Health:maDoctor:getProviderBaseRates(INPUT tt_auth_provider.doc_num, 
                                                                INPUT tt_auth.mem_num, 
                                                                INPUT tt_auth.option_code, 
                                                                INPUT tt_auth_provider.start_date, 
                                                                OUTPUT cBaseRate, 
                                                                OUTPUT cArsRate).
                END. /* ELSE */

                IF cReturnField = "[BaseRate]":U THEN 
				         {&AppendReturnValues} cBaseRate.
                ELSE 
				         {&AppendReturnValues} cArsRate.
              END. /* WHEN "[BaseRate]":U OR WHEN "[ArsRate]":U THEN */

              WHEN "[NegGroup]":U THEN
              DO:
                ASSIGN iSearchDocNum = IF tt_auth_provider.group_doc_num <> 0
                                       THEN tt_auth_provider.group_doc_num
                                       ELSE tt_auth_provider.doc_num.

                IF tt_auth.option_code <> 0 THEN
                  mipEnv:Health:maDoctor:getNegotiationGroup(INPUT iSearchDocNum, 
                                                             INPUT tt_auth.mem_num, 
                                                             INPUT tt_auth.option_code, 
                                                             OUTPUT cNegGroup).

                {&AppendReturnValues} cNegGroup.
              END. /* WHEN "[NegGroup]":U THEN */

              WHEN "[LineRestriction]":U THEN
              DO:
                EMPTY TEMP-TABLE ttAuthTypeConfig.

                mipEnv:Health:AuthService:getAuthTypeConfig(BUFFER tt_auth, 
                                                            BUFFER tt_auth_provider, 
                                                            INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).

                FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

                { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

                IF AVAILABLE ttAuthTypeConfig THEN 
				          ASSIGN cLineRestriction = ttAuthTypeConfig.DefaultLineRestriction.

                {&AppendReturnValues} cLineRestriction.
              END. /* WHEN "[LineRestriction]":U THEN */
              
              WHEN "[AuthGroupObj]" THEN
              DO:
                IF tt_auth_provider.auth_group_obj > 0 THEN
                  ASSIGN cAuthGroupObj = STRING (tt_auth_provider.auth_group_obj).

                {&AppendReturnValues} cAuthGroupObj.
              END. /* [AuthGroupObj] */
            END CASE. /* CASE cReturnField: */
          END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */

          {&ValidationSuccess}
        END. /* FOR FIRST tt_crosswalk NO-LOCK: */

        IF NOT CAN-FIND(FIRST tt_auth_provider NO-LOCK
                        WHERE tt_auth_provider.auth_provider_obj = dAuthProviderObj) THEN
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("Auth provider record '&1' not found", STRING(dAuthProviderObj)).

          VALIDATE ttValidation.
        END. /* ELSE */
      END. /* IF dAuthProviderObj <> 0.00 THEN */
      ELSE
      DO:
        {&BlankResponse}
      END. /* ELSE */
    END. /* WHEN "Detail":U THEN */

    WHEN "Status":U THEN
    DO:
      ASSIGN lSuccess   = mipEnv:Health:AuthService:getAuthObject
                                (INPUT  dAuthObj,
                                 INPUT  "":U,
                                 OUTPUT oAuthorisation)

             dStartDate = IF dStartDate = ?
                          THEN oAuthorisation:StartDate
                          ELSE dStartDate.

      DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):

        ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

        CASE cReturnField:
          WHEN "[StatusReasonMandatory]":U THEN
          DO:
            ASSIGN lMandatory = IF TRIM(cStatus) = "":U
                                THEN FALSE
                                ELSE mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), 
                                                                                     INPUT oAuthorisation:InsurerObj, 
                                                                                     INPUT oAuthorisation:MemberOptionCode, 
                                                                                     INPUT dStartDate).

            {&AppendReturnValues} STRING(lMandatory).
          END. /* WHEN "[StatusReasonMandatory]":U THEN */
        END CASE.  /* CASE cReturnField: */
      END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */

      {&ValidationSuccess}
    END. /* WHEN "Status":U THEN */

    WHEN "Reason":U THEN
    DO:
      IF cReasonCode <> "":U THEN
      DO:
        ASSIGN cReasonType = "":U .

        mipEnv:health:AuthService:getStatusReasonDesc (INPUT        0.0,
                                                       INPUT        0,
                                                       INPUT        dStartDate,
                                                       INPUT        INTEGER(cStatus),
                                                       INPUT        cReasonCode,
                                                       INPUT-OUTPUT cReasonType,
                                                       OUTPUT       cReasonDescription,
                                                       OUTPUT       cErrorMessage).

        IF cErrorMessage = "":U THEN
        DO:
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):

            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

            CASE cReturnField:

              WHEN "[ReasonCode]":U THEN {&AppendReturnValues} cStatus.
              WHEN "[ReasonDesc]":U THEN {&AppendReturnValues} cReasonDescription.
              WHEN "[ReasonType]":U THEN {&AppendReturnValues} cReasonType.

            END CASE.  /* CASE cReturnField: */
          END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */

          ASSIGN ttValidation.cReturnValues = cReturnValues
                 ttValidation.lValid        = TRUE
                 ttValidation.cMessage      = cReasonDescription.

        END. /* IF cErrorMessage = "" THEN */
        ELSE
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = cErrorMessage.
         
        VALIDATE ttValidation.
      END. /* IF cReasonCode <> "":U THEN */
      ELSE
      DO:
        {&BlankResponse}
      END.  /* ELSE */
    END.  /* WHEN "Reason":U THEN */
    WHEN "OverrideNote":U THEN
    DO:
      /* 
        Reason type will be determined in gesStatusReasonDesc
      */
      mipEnv:health:maUtility:getNoteDescription(INPUT cReasonCode,
                                                 INPUT cReasonType,
                                                 OUTPUT cReasonDescription) .
      IF cReasonDescription <> "":U THEN 
      DO:
        DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
          CASE cReturnField:
            WHEN "[ReasonCode]":U THEN
            DO:
              {&AppendReturnValues} cReasonCode. 
            END. /* WHEN "[ReasonCode]":U THEN */
            WHEN "[ReasonDesc]":U THEN
            DO:
              {&AppendReturnValues} cReasonDescription. 
            END. /* WHEN "[ReasonDesc]":U THEN */
            WHEN "[ReasonType]":U THEN
            DO:
              {&AppendReturnValues} cReasonType. 
            END. /* WHEN [ReasonType] */
            END CASE.                                                                      
        END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */   
        ASSIGN ttValidation.cReturnValues = cReturnValues 
               ttValidation.lValid        = TRUE          
               ttValidation.cMessage      = cReasonDescription.     
      END. /* IF cReasonDescription <>  "":U  THEN */
      ELSE
      DO:
        {&BlankResponse} 
      END. /*ELSE*/      
    END.  /* WHEN "OverrideNote":U THEN */    

    WHEN "ClaimCode":U THEN
    DO: 
      {&FocusAuthObject}
      IF iClaimCode <> 0 AND VALID-OBJECT(oAuthorisation) THEN 
      DO:
        FIND FIRST ccode NO-LOCK
             WHERE ccode.claim-code  = iClaimCode
               AND ccode.scheme-code = iOptionCode
          NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
        
        IF AVAILABLE ccode THEN
        DO:
          mipEnv:Health:AuthBusinessLogic:validateClaimCode(INPUT  dInsurerObj,
                                                            INPUT  iOptionCode,
                                                            INPUT  cMemberNumber,
                                                            INPUT  iDependant,
                                                            INPUT  iClaimCode,
                                                            INPUT  dAuthTypeObj,
                                                            INPUT  dStartDate,
                                                            INPUT  "", /* Provider Type */
                                                            INPUT  0,  /* Discipline */
                                                            INPUT  0,  /* Sub-Discipline */
                                                            INPUT  0,  /* Negotiation Number */
                                                            INPUT "hat_auth_detail":U, /* auth level */
                                                            OUTPUT lValid,
                                                            OUTPUT cErrorOrWarning).
          IF lValid THEN
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
          
            CASE cReturnField:
              WHEN "[ValidateClaimCode]":U THEN
              DO:
                ASSIGN cErrorOrWarning = (IF NOT lValid THEN "error:":U ELSE "warning:":U) + cErrorOrWarning.
                {&AppendReturnValues} cErrorOrWarning.
              END. /* WHEN "[ValidateClaimCode]":U THEN */

              WHEN "[StatusAction]":U THEN
              DO:
                ASSIGN iStatus = INTEGER(cStatus) NO-ERROR.
          
          	    /* When validating if a claim-code is set up as a Tariff Cost Benefit claim-code:
                   If a detail line has been declined because of an invalid claim-code and the user
                   wants to override it with a valid claim-code, we want to Authorise the detail line 
                   if the TrfcostBenCCInvalid rule has set up with "Load". 
                   The Auth Status Reason is however cleared in JS, and already displays a message that it
                   needs to be entered, before we get to the Business Logic.
                   This prevents the newly "Auhorised" status to be assigned.
                   So...we are therefor setting the status to 1 (Authorised) in the Ajax validation to ensure
                   that the Reason is not mandatory further down, which is assigned to cStatusAction and picked
                   up by the JS.
                   Assign lValidRule to False to prevent interference with the rule value logic thereafter.
                */
                mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
		                                                           INPUT  iOptionCode,
		                                                           INPUT  "ma_acAuthRuleTypeAuthDetail":U,
		                                                           INPUT  "TrfcostBenCCInvalid":U,
		                                                           INPUT  dStartDate,
		                                                           OUTPUT lValidRule,
                                                               OUTPUT cRuleValue).
                                                        
                IF lValidRule AND ENTRY(1,cRuleValue,"|") = "LOAD":U
		            THEN iStatus = 1.
                
                ASSIGN lValidRule = FALSE. 
                
                IF NOT AVAILABLE buf_auth_schext 
                AND VALID-OBJECT(oAuthorisation) 
                THEN
			            FIND FIRST buf_auth_schext NO-LOCK
				               WHERE buf_auth_schext.scheme-code = iOptionCode
			              NO-ERROR.
				        {mip/inc/mipthrowerror.i &IgnoreErrors = 'Progress:565' &ResetIgnoredErrors = TRUE}
                
                IF AVAILABLE buf_auth_schext 
                AND iClaimCode = buf_auth_schext.claim-code[1] 
                THEN
                  ASSIGN iStatus     = 6 
                         lSuccess    = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                                                      INPUT  iOptionCode,
                                                                                      INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                                                      INPUT  "NilPaymentDefaultReason":U,
                                                                                      INPUT  dStartDate,
                                                                                      OUTPUT lValidRule,
                                                                                      OUTPUT cRuleValue).
          
                ASSIGN lMandatory   = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  iStatus,
                                                                                      INPUT  dInsurerObj,
                                                                                      INPUT  iOptionCode,
                                                                                      INPUT  dStartDate)
                    
                       cStatusAction = STRING(iStatus)  + ",":U            //Status code
                                    + (IF lMandatory THEN "MANDATORY":U 
                                       ELSE "OPTIONAL":U ) + ",":U         //Reason required 
                                    + (IF  lValidRule 
                                       AND iStatus = 6  
                                       THEN cRuleValue 
                                       ELSE "":U ).                        //Default reason code
          
                {&AppendReturnValues} cStatusAction.
          
                IF lValidRule 
                AND iClaimCode = buf_auth_schext.claim-code[1] 
                THEN			
                  ASSIGN ttValidation.cReturnValues = cReturnValues
                         ttValidation.lValid        = TRUE
                         ttValidation.cMessage      = cReasonDescription.

                ELSE 
                  IF NOT lValidRule 
                  AND iClaimCode = buf_auth_schext.claim-code[1] 
                  THEN
                    ASSIGN ttValidation.lValid   = FALSE 
                           ttValidation.cMessage = "Rule NilPaymentDefaultReason not valid" .

                IF iClaimCode <> buf_auth_schext.claim-code[1] THEN
                  ASSIGN ttValidation.cReturnValues = cReturnValues
                         ttValidation.lValid        = TRUE
                         ttValidation.cMessage      = "".

                VALIDATE ttValidation.

              END.  /* WHEN "[StatusAction]":U THEN */	
            END CASE. /* CASE cReturnField: */
          END.  /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */ 
          ELSE  /* If claim is invalid (lValid = False), return an error */
          DO:
            ASSIGN ttValidation.lValid   = FALSE
                   ttValidation.cMessage = SUBSTITUTE("Claim code '&1' not found for option &2":U, STRING(iClaimCode), STRING(iOptionCode)).
                 
            VALIDATE ttValidation.       
          END. /* ELSE */
        END. /* IF AVAILABLE ccode THEN */
        ELSE  /* If claim is invalid (ccode not available), return an error */
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("Claim code '&1' not found for option &2":U, STRING(iClaimCode), STRING(iOptionCode)).
                 
          VALIDATE ttValidation.       
        END.  /* ELSE */
      END.  /* IF iClaimCode <> 0 AND VALID-OBJECT(oAuthorisation) THEN */
      ELSE 
      DO:
        ASSIGN ttValidation.cReturnValues = cReturnValues
               ttValidation.lValid        = TRUE
               ttValidation.cMessage      = cReasonDescription.
        
        VALIDATE ttValidation.
      END. /* ELSE */
    END. /* WHEN "ClaimCode":U THEN */
    OTHERWISE
    DO:
      CASE cOwningEntityMnemonic:
        WHEN "htmtl":U THEN
        DO:
          IF cOwningAltValue <> "":U THEN
          DO:
            { ma/msc/matarpad.i &tariff     = cOwningAltValue              }
            { ma/msc/madispad.i &discipline = cPrType     &comment = "/* " }
            { ma/msc/madispad.i &discipline = cSubPrType  &comment = "/* " }
            {&FocusAuthObject}
            /*
              Fetch auth data
              We dont want to proceed if we do not have the ARS rates and base rates have not been returned yet
            */     
            IF cArsRate = "":U AND cBaseRate = "":U THEN
            DO:
                ASSIGN ttValidation.lValid = ?.
                RETURN.
            END.  /* IF cArsRate = "":U AND cBaseRate = "":U THEN */
            
            oAuthSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).

            ASSIGN lSuccess = oAuthSearch:SetCriteria("BufferList":U , "tt_auth,tt_auth_provider":U)
                   lSuccess = oAuthSearch:SetCriteria("DataSecured":U, "FALSE":U)
                   lSuccess = oAuthSearch:SetFilterCriteria("tt_auth_provider.auth_provider_obj":U, "=":U, dAuthProviderObj).

            oAuthSearch:fetchData().

            FIND FIRST tt_auth_provider NO-LOCK
                 WHERE tt_auth_provider.auth_provider_obj = dAuthProviderObj
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE tt_auth_provider THEN
              ASSIGN dAuthProvStartDate = tt_auth_provider.start_date.

            /*
             If the Override Base & ARS Rates are populated by the user, then we need to run the validation 
             again to ensure that the Entity (Tariff) for the record is still valid for the new Base & ARS Rate. 
             We will not allow the Override if the Tariff does not exist for the new rates.
            */
            ASSIGN 
              cBaseRate  = IF cOverrideBaseRate <> "":U
                           THEN cOverrideBaseRate ELSE cBaseRate
              cArsRate   = IF cOverrideArsRate <> "":U
                           THEN cOverrideArsRate ELSE cArsRate.              
              
            /* 
              Find the tariff link record 
            */
            ASSIGN lSuccess = mipEnv:Health:maMedical:getValidTariff
                                (INPUT-OUTPUT dTariffLinkObj,        /*  iodTariffLinkObj  */
                                 INPUT        cOwningAltValue,       /*  ipcTariffCode     */
                                 INPUT        cBaseRate,             /*  ipcBaseRate       */
                                 INPUT        cArsRate,              /*  ipcARSRate        */
                                 INPUT        INTEGER(cPrType),      /*  ipiPrType         */
                                 INPUT        INTEGER(cSubPrType),   /*  ipiSubPrType      */
                                 INPUT        dStartDate,            /*  ipdDate           */
                                 INPUT        iOptionCode,           /*  ipiOptionCode     */
                                 INPUT        "",                    /*  ipcAddValidations */
                                 OUTPUT       dTariffObj,            /*  opdTariffObj      */
                                 OUTPUT       dTrfCostObj,           /*  opdTrfCostObj     */
                                 OUTPUT       cError,                /*  opcError          */
                                 OUTPUT       cWarning,              /*  opcWarning        */
                                 OUTPUT       cAlertMessage).        /*  opcAlertMessage   */
            IF cError = "":U THEN
            DO:
              FIND FIRST htm_tariff_link NO-LOCK
                   WHERE htm_tariff_link.tariff_link_obj = dTariffLinkObj 
			          NO-ERROR.
              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
              
              IF AVAILABLE htm_tariff_link THEN
              DO:
                ASSIGN
                  cRecordRowId = STRING(ROWID(htm_tariff_link))
                  cTariffCode  = htm_tariff_link.tariff_code
                  cTariffDesc  = htm_tariff_link.tariff_description
                  cBaseRate    = htm_tariff_link.base_rate
                  cArsRate     = htm_tariff_link.ars_rate.

                /* find the tariff type link */
                FIND LAST htm_tariff_type_link NO-LOCK
                    WHERE htm_tariff_type_link.tariff_link_obj = dTariffLinkObj
                      AND htm_tariff_type_link.effective_date  <= dAuthProvStartDate
			            NO-ERROR.

                { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

                IF NOT AVAILABLE htm_tariff_type_link THEN
                DO:
                  /* searching for a default tariff link  */
                  ASSIGN 
                    cWhereClause = SUBSTITUTE("tariff_code = '&1' AND base_rate = '' AND ars_rate = '' AND
                                               pr_type = '000'":U, cOwningAltValue)
                    lSuccess     = mipEnv:miDBEntity:focusTable(cOwningEntityMnemonic)
                    lSuccess     = mipEnv:miDBEntity:findRecordWhere(cWhereClause).

                  IF mipEnv:miDBEntity:RecordAvailable THEN
                  DO:
                    FIND LAST htm_tariff_type_link NO-LOCK
                      WHERE htm_tariff_type_link.tariff_link_obj = mipEnv:miDBEntity:RecordObj
                        AND htm_tariff_type_link.effective_date <= dAuthProvStartDate
                      NO-ERROR.

                    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
                  END. /* IF mipEnv:miDBEntity:RecordAvailable THEN */
                END. /* IF NOT AVAILABLE htm_tariff_type_link THEN */

                IF AVAILABLE htm_tariff_type_link THEN
                DO:
                  FIND FIRST htm_tariff_type NO-LOCK
                       WHERE htm_tariff_type.tariff_type_obj = htm_tariff_type_link.tariff_type_obj
                       AND   htm_tariff_type.acronym_key     = "ma_acTariffTypeCatLOC":U
                    NO-ERROR.

				          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

                  IF AVAILABLE htm_tariff_type THEN
                    ASSIGN
                      dTariffTypeObj      = htm_tariff_type.tariff_type_obj
                      cTariffTypeCategory = htm_tariff_type.acronym_key.
                END.  /* IF AVAILABLE htm_tariff_type_link THEN */

                IF cTariffTypeCategory <> "ma_acTariffTypeCatLOC":U THEN
                DO:
                   mipEnv:Health:AuthBusinessLogic:activateMinutes(INPUT  dInsurerObj,                
                                                                   INPUT  iOptionCode,                
                                                                   INPUT  dStartDate,                 
                                                                   INPUT  dTariffLinkObj,           
                                                                   OUTPUT cError,                   
                                                                   OUTPUT dMinutesTariffTypeObj,    
                                                                   OUTPUT lActivateMinutes).

                END. /* IF cTariffTypeCategory <> "ma_acTariffTypeCatLOC":U THEN */


                DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):

                  ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

                  CASE cReturnField:

                    WHEN "[RecordRowid]":U          THEN {&AppendReturnValues} STRING(cRecordRowId).
                    WHEN "[RecordObj]":U            THEN {&AppendReturnValues} STRING(dTariffLinkObj).
                    WHEN "[RecordAltObj]":U         THEN {&AppendReturnValues} "0":U.
                    WHEN "[RecordCode]":U           THEN {&AppendReturnValues} cTariffCode.
                    WHEN "[RecordDesc]":U           THEN {&AppendReturnValues} cTariffDesc.
                    WHEN "[BaseRate]":U             THEN {&AppendReturnValues} cBaseRate.
                    WHEN "[ArsRate]":U              THEN {&AppendReturnValues} cArsRate.
                    WHEN "[TariffTypeObj]":U        THEN {&AppendReturnValues} (IF dTariffTypeObj > 0 AND cTariffTypeCategory = "ma_acTariffTypeCatLOC":U THEN STRING(dTariffTypeObj) ELSE "":U).
                    WHEN "[TariffTypeCategory]":U   THEN {&AppendReturnValues} (IF cTariffTypeCategory = "ma_acTariffTypeCatLOC":U THEN "ma_acTariffTypeCatLOC":U ELSE "":U).
                    WHEN "[MinutesTariffTypeObj]":U THEN {&AppendReturnValues} (IF lActivateMinutes AND dMinutesTariffTypeObj > 0 THEN STRING(dMinutesTariffTypeObj) ELSE "":U).

                    WHEN "[TariffCost]":U THEN
                    DO:
                      /*
                        MMP-452 TODO populate this when MMP-451 is done which is the service to get tariff cost
                      */
                      ASSIGN dTariffCost = 0.00.

                      {&AppendReturnValues} STRING(dTariffCost).
                    END. /* WHEN "[TariffCost]":U THEN */

                    WHEN "[TariffEffectiveDate]":U THEN
                    DO:
                      /*
                        Find latest applicable tariff for the tariff code,base rate, ars rate etc
                      */
                      TariffBlk:
                      FOR EACH tariff NO-LOCK
                         WHERE tariff.tariff-code  = cTariffCode
                           AND tariff.base-rate    = cBaseRate
                           AND tariff.pr-type      = cPrType
                           AND tariff.ars-rate     = cArsRate
                           AND tariff.effect-date <= dStartDate
                            BY tariff.effect-date DESCENDING:

                        ASSIGN dTariffEffectDate = tariff.effect-date.

                        LEAVE TariffBlk.
                      END. /* FOR EACH tariff NO-LOCK */

                      ASSIGN dTariffEffectDate = IF dTariffEffectDate <> ?
                                                 THEN dTariffEffectDate 
                                                 ELSE dStartDate.

                      {&AppendReturnValues} STRING(dTariffEffectDate).
                    END. /* WHEN "[TariffEffectDate]":U THEN */
                    
                    WHEN "[AuthNappiRequired]":U THEN
                    DO:
                      IF dTariffObj <> ? THEN 
                      DO:
                        FIND FIRST tariff NO-LOCK
                             WHERE tariff.tariff-obj = dTariffObj 
                          NO-ERROR.
                        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
                        
                        IF AVAILABLE tariff THEN
                        DO:
                          {&AppendReturnValues} tariff.nappi-required-auths.   
                        END.  /* IF AVAILABLE tariff THEN */   
                      END.  /* IF dTariffObj <> ? THEN */        
                    END. /* WHEN "[TariffRequired]":U THEN */
                    OTHERWISE
                      {&AppendReturnValues} "":U.

                  END CASE.  /* CASE cReturnField: */
                END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */

                {&ValidationSuccess}
              END. /* IF AVAIALBLE htm_tariff_link THEN */
              ELSE
              DO:
                ASSIGN ttValidation.lValid   = FALSE
                       ttValidation.cMessage = SUBSTITUTE("Tariff '&1' not found for discipline '&2' sub-discipline '&3' and base rate '&4'", cOwningAltValue, cPrType, cSubPrType, cBaseRate).

                VALIDATE ttValidation.
              END. /* ELSE */
            END. /* IF cError <> "":U THEN */
            ELSE
            DO: 
              ASSIGN ttValidation.lValid   = FALSE
                     ttValidation.cMessage = cError.

              VALIDATE ttValidation.
            END. /* ELSE */
          END. /* IF cOwningAltValue <> "":U THEN */
          ELSE
          DO:
            {&BlankResponse}
          END. /* ELSE */
        END. /* WHEN "htmtl":U THEN */
        
        WHEN "hlmnl":U THEN
        DO:
          IF cOwningAltValue <> "":U THEN
          DO:
            ASSIGN dNappiLinkObj   = 0
                   lSuccess = mipEnv:Health:maMedical:getValidNappi
			                         (INPUT-OUTPUT dNappiLinkObj,        /*  iodNappiLinkObj  */
                                      INPUT  cOwningAltValue,      /*  ipcNappiCode     */
                                      INPUT  dStartDate,           /*  ipdDate          */
                                      OUTPUT cError).              /*  opcError         */

            IF cError = "":U 
            AND dNappiLinkObj <> 0 THEN
            DO:
              FIND FIRST hlm_nappi_link NO-LOCK
                   WHERE hlm_nappi_link.nappi_link_obj = dNappiLinkObj NO-ERROR.
                   
              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
                   
              ASSIGN cOwningAltValue = STRING(hlm_nappi_link.nappi_code_prefix)
                     cWhereClause    = SUBSTITUTE("&1 = &2":U, "nappi_code_prefix":U, cOwningAltValue)
                     lSuccess        = mipEnv:miDBEntity:focusTable(cOwningEntityMnemonic)
                     lSuccess        = mipEnv:miDBEntity:findRecordWhere(cWhereClause).

              IF mipEnv:miDBEntity:RecordAvailable THEN
              DO:
                DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
                
                  ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
                
                  CASE cReturnField:
                    WHEN "[RecordRowid]":U       THEN {&AppendReturnValues} STRING(mipEnv:miDBEntity:RecordRowid).
                    WHEN "[RecordObj]":U         THEN {&AppendReturnValues} STRING(mipEnv:miDBEntity:RecordObj).
                    WHEN "[RecordCode]":U        THEN {&AppendReturnValues} mipEnv:miDBEntity:RecordCode.
                    WHEN "[RecordDesc]":U        THEN {&AppendReturnValues} mipEnv:miDBEntity:RecordDescription.
                    WHEN "[RecordAltObj]":U THEN
                    DO:
                      oNappi = NEW cls.manappi(cOwningAltValue).
                
                      {&AppendReturnValues} (IF oNappi:NappiInFocus
                                             THEN STRING(oNappi:NappiObj)
                                             ELSE "0":U).
                    END. /* WHEN "[RecordAltObj]":U THEN */
                
                    OTHERWISE
                      {&AppendReturnValues} "":U.
			     	      END CASE.  /* CASE cReturnField: */
                END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */
                
                {&ValidationSuccess}
              END. /* IF mipEnv:miDBEntity:RecordAvailable */  
            END. /* IF cError = "":U */
            ELSE DO:
              ASSIGN ttValidation.lValid   = FALSE
                     ttValidation.cMessage = cError.
                     
              VALIDATE ttValidation.
            END. /* ELSE */
          END. /* IF cOwningAltValue <> "":U THEN */
          ELSE DO:
            {&BlankResponse}

          END. /* ELSE */
        END. /* WHEN "hlmnl":U THEN */

        WHEN "hlmcr":U THEN
        DO:     
          IF cOwningAltValue <> "":U THEN
          DO:   
            ASSIGN cWhereClause = SUBSTITUTE("&1 = '&2'":U, "crosswalk_code":U, cOwningAltValue)
                   lSuccess     = mipEnv:miDBEntity:focusTable(cOwningEntityMnemonic)
                   lSuccess     = mipEnv:miDBEntity:findRecordWhere(cWhereClause).

            IF mipEnv:miDBEntity:RecordAvailable THEN
            DO:
              DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
                ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

                CASE cReturnField:
                  WHEN "[RecordRowid]":U  THEN {&AppendReturnValues} STRING(mipEnv:miDBEntity:RecordRowid).
                  WHEN "[RecordObj]":U    THEN {&AppendReturnValues} STRING(mipEnv:miDBEntity:RecordObj).
                  WHEN "[RecordAltObj]":U THEN {&AppendReturnValues} "0":U.
                  WHEN "[RecordCode]":U   THEN {&AppendReturnValues} mipEnv:miDBEntity:RecordCode.
                  WHEN "[RecordDesc]":U   THEN {&AppendReturnValues} mipEnv:miDBEntity:RecordDescription.

                  OTHERWISE
                    {&AppendReturnValues} "":U.
                END CASE.  /* CASE cReturnField: */
              END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */

              {&ValidationSuccess}
            END. /* IF mipEnv:miDBEntity:RecordAvailable THEN */
            ELSE
            DO:
              ASSIGN ttValidation.lValid   = FALSE
                     ttValidation.cMessage = SUBSTITUTE("Basket '&1' not found", cOwningAltValue).

              VALIDATE ttValidation.
            END. /* ELSE */
          END. /* IF cOwningAltValue <> "":U THEN */
          ELSE
          DO:
            {&BlankResponse}
          END. /* ELSE */
        END. /* WHEN "hlmcr":U THEN */
        WHEN "hatad":U THEN
        DO:
          IF cOwningAltValue <> "":U THEN
          DO:   
            ASSIGN cWhereClause = SUBSTITUTE("&1 = '&2'":U, "owning_alt_value":U, cOwningAltValue)
                   lSuccess     = mipEnv:miDBEntity:focusTable(cOwningEntityMnemonic)
                   lSuccess     = mipEnv:miDBEntity:findRecordWhere(cWhereClause).
                   
             IF mipEnv:miDBEntity:RecordAvailable THEN
            DO:
              DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
                ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

                CASE cReturnField:
                  WHEN "[RecordRowid]":U  THEN {&AppendReturnValues} STRING(mipEnv:miDBEntity:RecordRowid).
                  WHEN "[RecordObj]":U    THEN {&AppendReturnValues} STRING(mipEnv:miDBEntity:RecordObj).
                  WHEN "[RecordAltObj]":U THEN {&AppendReturnValues} "0":U.
                  WHEN "[RecordCode]":U   THEN {&AppendReturnValues} mipEnv:miDBEntity:RecordCode.
                  WHEN "[RecordDesc]":U   THEN {&AppendReturnValues} "".

                  OTHERWISE
                    {&AppendReturnValues} "":U.
                END CASE.  /* CASE cReturnField: */
              END. /* DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList): */

              {&ValidationSuccess}
            END. /* IF mipEnv:miDBEntity:RecordAvailable THEN */
            ELSE
            DO:
              ASSIGN ttValidation.lValid   = FALSE
                     ttValidation.cMessage = SUBSTITUTE("Detail '&1' not found", cOwningAltValue).

              VALIDATE ttValidation.
            END. /* ELSE */
          END. /*IF cOwningAltValue*/
        END. /*WHEN "hatad":U THEN*/
        OTHERWISE
        DO:
          {&BlankResponse}
        END. /* ELSE */
      END CASE.  /* CASE cReturnField: */
    END. /* OTHERWISE */
  END CASE. /* CASE ipcValidationArgument: */

  { mip/inc/mipcatcherror.i
    &FINALLY = "DATASET dsAuthorisation:EMPTY-DATASET().
    
                IF VALID-OBJECT(oAuthSearch)    THEN DELETE OBJECT oAuthSearch.
                IF VALID-OBJECT(oNappi)         THEN DELETE OBJECT oNappi.
                IF VALID-OBJECT(oTariffLink)    THEN DELETE OBJECT oTariffLink."}
&ENDIF




