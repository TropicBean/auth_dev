/* maauthbussavedetail.i MEDSTAR Medical Aid System
                         Save Authorisation Detail Record
                         (c) Copyright 1990 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/

  DEFINE PARAMETER BUFFER btt_auth_detail FOR tt_auth_detail.

  DEFINE VARIABLE cAddValidations         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAlertMessage           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cArsRate                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBaseRate               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cClaimCodeList          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDisallowTrfList        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cEntityDescr            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cEntityKey              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cEntityMnemonic         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cEntityValue            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cError                  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cErrorField             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cEventMsg               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cExclusion              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cLineRestriction        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cMessageType            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cMinutesCalcRule        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNappiCode              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNewStatus              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNewStatusDescr         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNonChargeType          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNote                   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOEMDescr               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cReason                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cREMDescr               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRelatedEntityMnemonic  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRelatedEntityDescr     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRelatedValue           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRestrictionMessage     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRestrictionTariffs     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRuleLoadStatus         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRuleValue              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRuleValueExceedLim     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRuleValueInvalid       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRuleValueUserDecision  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRuleValueWarn          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cStatus                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cStatusDesc             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cStatusReason           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cText                   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cTrackingMessage        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cWarning                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cWarnType               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cValidationErrorMessage AS CHARACTER                 NO-UNDO.

  DEFINE VARIABLE dAmountAuth             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthCopayObj           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthCopayTypeObj       AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj          AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCopayValue             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCostCalculated         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dEntityObj              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dItemCost               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dNappiLinkObj           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTariffObj              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dRelatedObj             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQuantityAuth           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTrfCostAmount          AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTrfCostBenefit%        AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTrfCostFixedAmount     AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTrfCostObj             AS DECIMAL                   NO-UNDO.

  DEFINE VARIABLE iAuthStatus             AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iClaimCode              AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iCount                  AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iField                  AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iNappiCodePrefix        AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iNegNum                 AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iPrType                 AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iSubPrType              AS INTEGER                   NO-UNDO.

  DEFINE VARIABLE l99Dependant            AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lAccept                 AS LOGICAL                   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lAcknowledge            AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lAutoReject             AS LOGICAL                   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lBodyRegionValid        AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lChildModifier          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lCopayValueType         AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lDisplayWarning         AS LOGICAL                   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lFoundTrfRestriction    AS LOGICAL                   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lGetNappiExcl           AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lGetNappiNonCharge      AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lGetTariffExcl          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lLimitAmtApplied        AS LOGICAL                   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lMandatory              AS LOGICAL                   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lNappiBenefits          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lOverrideBaseARSRate    AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lParentOfModifier       AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lProvider               AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lSuccess                AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lTrfCostBenefit         AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lUseCalculatedAmount    AS LOGICAL                   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE lValidationError        AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidRule              AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidRuleExceedLim     AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidRuleInvalid       AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidRuleUserDecision  AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidRuleWarn          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidStatus            AS LOGICAL                   NO-UNDO INITIAL FALSE.

  DEFINE VARIABLE oProtocolSearch         AS cls.maclinicaldocsearch   NO-UNDO.
  DEFINE VARIABLE oTFSearch               AS ma.cls.matariffsearch     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  /* Define 2 new variables to store the values needed for the Emergency Flag. */
  DEFINE VARIABLE cEmergencyFlag        LIKE tt_auth_provider._emergency_flag         NO-UNDO.
  DEFINE VARIABLE lEmergencyFlagUpdated LIKE tt_auth_provider._emergency_flag_updated NO-UNDO.

  DEFINE BUFFER btt_auth_provider   FOR tt_auth_provider.
  DEFINE BUFFER buf_auth_detail     FOR hat_auth_detail.
  DEFINE BUFFER buf_auth_provider   FOR hat_auth_provider.
  DEFINE BUFFER btt_auth            FOR tt_auth.
  DEFINE BUFFER trfcost             FOR trfcost.
  DEFINE BUFFER buf_auth_schext     FOR schext.
  DEFINE BUFFER mod_auth_detail     FOR tt_auth_detail.
  DEFINE BUFFER loc_auth_detail     FOR tt_auth_detail.
  DEFINE BUFFER bttt_auth_detail    FOR tt_auth_detail.
  DEFINE BUFFER buf_memdep          FOR memdep.
  DEFINE BUFFER btt_main_provider   FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_copay      FOR tt_auth_copay.

  IF  AVAILABLE btt_auth_detail
  AND CAN-DO("{&ActionList}":U, btt_auth_detail.record_action)
  AND NOT goErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U, "ERR":U)
  THEN
  DO:
    /*
      Clear dataset dsClinicalDocs
    */
    DATASET dsClinicalDocs:EMPTY-DATASET().

    /*
      Instantiate protocol search object
    */
    ASSIGN oProtocolSearch = NEW cls.maclinicaldocsearch(DATASET dsClinicalDocs BY-REFERENCE).

    /*
      Retain old auth obj in case this is a new record and we have a dummy obj which will need to be replaced
      on all the child records if this is a batch update.
    */

    ASSIGN dAuthDetailObj = btt_auth_detail.auth_detail_obj.

    FIND FIRST btt_auth NO-LOCK.

    /*
      If the authorisation has been ended, ensure we end the detail line as well if the end date is blank
    */
    IF goAuthorisation:EndDate <> ? AND btt_auth_detail.end_date = ?
    THEN
      ASSIGN btt_auth_detail.end_date = goAuthorisation:EndDate.

    IF AVAILABLE ttAuthTypeConfig AND NOT ttAuthTypeConfig.ActivateAmPm
    THEN
      ASSIGN btt_auth_detail.start_ampm = ?
             btt_auth_detail.end_ampm   = ?.

    IF NOT AVAILABLE buf_auth_schext THEN
    DO:
      FIND FIRST buf_auth_schext NO-LOCK
        WHERE buf_auth_schext.scheme-code = goAuthorisation:optionCode
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE buf_auth_schext THEN
      DO:
        ASSIGN cError = "Scheme Information record not available for option " + STRING(goAuthorisation:optionCode) +
                        ". Clinical detail line was not updated.".
        goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                           INPUT "":U,                                               /* ipcOwningEntityKey      */
                           INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                           INPUT cError).                                            /* ipcMessageText          */
        RETURN.
      END.  /* IF NOT AVAILABLE buf_auth_schext THEN */
    END.  /* IF NOT AVAILABLE buf_auth_schext THEN */

    /*
      Inherit PMB indicator from provider when the detail record is being created.
      Populate the pmb indicator on the temp table record before the record is saved.
    */
    FIND btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    FIND buf_auth_detail NO-LOCK
      WHERE buf_auth_detail.auth_detail_obj = dAuthDetailObj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE btt_auth_provider THEN
    DO:
      /*
        If the user updates the override base/ars rates on the detail line,
        we need to clear the owning_obj which contains the tariff_link_obj.
        getValidTariff will repopulate the owning_obj with a valid tariff_link_obj,
        if a valid tariff link is found using the default/override base/ars rate
      */
      IF AVAILABLE buf_auth_detail AND
        (buf_auth_detail.override_base_rate <> btt_auth_detail.override_base_rate OR
         buf_auth_detail.override_ars_rate  <> btt_auth_detail.override_ars_rate)
      THEN
        ASSIGN btt_auth_detail.owning_obj = 0
               lOverrideBaseARSRate       = TRUE.

      IF btt_auth_detail.override_base_rate <> "":U
      THEN
        ASSIGN cBaseRate = btt_auth_detail.override_base_rate
               cArsRate  = btt_auth_detail.override_ars_rate.
      ELSE
        ASSIGN cBaseRate = IF btt_auth_provider.override_base_rate <> "":U
                           THEN btt_auth_provider.override_base_rate
                           ELSE btt_auth_provider.default_base_rate
               cArsRate  = IF btt_auth_provider.override_ars_rate <> "":U
                           THEN btt_auth_provider.override_ars_rate
                           ELSE btt_auth_provider.default_ars_rate.

      IF cBaseRate = "" THEN
      DO:
        mipEnv:Health:maDoctor:getProviderBaseRates( INPUT  btt_auth_provider.doc_num,
                                                     INPUT  goAuthorisation:MemNum,
                                                     INPUT  goAuthorisation:MemberOptionCode,
                                                     INPUT  btt_auth_provider.start_date,
                                                     OUTPUT cBaseRate,
                                                     OUTPUT cArsRate).
      END.  /* IF cBaseRate = "" THEN */

      IF btt_auth_detail.quantity_los > 0
      THEN
        ASSIGN btt_auth_detail.default_base_rate = cBaseRate
               btt_auth_detail.default_ars_rate  = cArsRate.
      ELSE
        ASSIGN btt_auth_detail.default_base_rate = "":U
               btt_auth_detail.default_ars_rate  = "":U.

      IF btt_auth_detail.auth_detail_obj <= 0.00 THEN
      DO:
        ASSIGN btt_auth_detail.pmb_indicator = btt_auth_provider.pmb_indicator.

        VALIDATE btt_auth_detail.
      END. /* IF btt_auth_detail.auth_detail_obj <= 0.00 THEN */

      ASSIGN iPrType    = btt_auth_provider.pr_type
             iSubPrType = btt_auth_provider.sub_pr_type.
    END. /* IF AVAILABLE btt_auth_provider THEN */

    IF AVAILABLE ttAuthTypeconfig
    AND ttAuthTypeConfig.ActivateLos THEN
    DO:
      IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U
      THEN
        mipEnv:health:AuthService:determineTariffModifierType(INPUT  btt_auth_detail.owning_obj ,
                                                              INPUT  btt_auth_detail.start_date ,
                                                              OUTPUT lParentOfModifier ,
                                                              OUTPUT lChildModifier     ) .

      /*
        Modifier tariffs are automatically added/deleted with the LOC tariffs, therefor we
        cannot delete a modifier tariff manually.
      */
      IF btt_auth_detail.record_action = "DELETE":U THEN
      DO:


        IF  btt_auth_detail.quantity_los = 0
        AND btt_auth_detail.related_entity_mnemonic = "hatad":U
        AND btt_auth_detail.related_obj <> 0
        AND CAN-FIND(FIRST hat_auth_detail
                     WHERE hat_auth_detail.auth_detail_obj = btt_auth_detail.related_obj)
        AND lChildModifier THEN
        DO:

          goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                 INPUT btt_auth_detail.auth_detail_obj,
                                 INPUT "":U,
                                 INPUT "adjustment_paid":U,
                                 INPUT btt_auth_detail.line_number,
                                 INPUT "ma":U,
                                 INPUT 370,  /* &1 cannot be deleted, as &2 records exist. */
                                 INPUT "Modifier Tariff (":U + btt_auth_detail.owning_alt_value + "),LOC Tariff (":U + btt_auth_detail.related_value + ")":U).
        END.  /* IF  btt_auth_detail.quantity_los = 0 */

      END.  /* IF btt_auth_detail.record_action = "DELETE":U THEN */
      ELSE DO:


        /*
          If the tariff code has been set up as both a parent to a modifier tariff and a child modifier , we must block them from saving the tariff . This
          would be considered invalid setup , and the user will have to fix it before they can continue
        */
        IF  btt_auth_detail.owning_entity_mnemonic = "htmtl":U
        AND lParentOfModifier
        AND lChildModifier THEN
        DO:

           ASSIGN cError = SUBSTITUTE("Invalid Setup for tariff code &1." ,btt_auth_detail.owning_alt_value)
                         + "The tariff is set up as both a parent to a modifier tariff and a modifier tariff at the same time."
                         + "Please fix code link setup in order to capture this tariff code.".

           goErrorObject:addError
                      (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                       INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                       INPUT "":U,                                                /* ipcOwningEntityKey      */
                       INPUT "owning_alt_value":U,                                /* ipcFieldName            */
                       INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                       INPUT cError,                                              /* ipcMessageText          */
                       INPUT "ERR":U).                                            /* ipcMessageType          */

        END.  /*  btt_auth_detail.owning_entity_mnemonic = "htmtl":U AND lParentOfModifier  AND lChildModifier */
      END.  /* ELSE (btt_auth_detail.record_action = "DELETE":U) */
    END. /* IF AVAILABLE ttAuthTypeconfig AND ttAuthTypeConfig.ActivateLos */

    IF btt_auth_detail.record_action <> "DELETE":U THEN
    DO:
      IF btt_auth_detail.minutes_auth <> 0
      THEN DO:
        IF   btt_auth_detail.auth_detail_obj <= 0.00
        OR  (AVAILABLE buf_auth_detail
        AND (btt_auth_detail.minutes_auth    <> buf_auth_detail.minutes_auth
        OR   btt_auth_detail.start_date      <> buf_auth_detail.start_date))
        THEN DO:

          /* Calculate */
          mipEnv:Health:AuthService:calcAuthQtyFromMinutes
                                   (INPUT  goAuthorisation:InsurerObj,              // ipdInsurerObj
                                    INPUT  goAuthorisation:OptionCode,              // ipiOptionCode
                                    INPUT  btt_auth_detail.start_date,              // ipdStartDate
                                    INPUT  btt_auth_detail.minutes_tariff_type_obj, // ipdMinutesAuthTypeObj
                                    INPUT  btt_auth_detail.minutes_auth,            // ipiMinutesAuth
                                    OUTPUT dQuantityAuth,                           // opdQuantityAuth
                                    OUTPUT cMinutesCalcRule,                        // opcMinutesCalcRule
                                    OUTPUT cError).                                 // opcError

          IF cError <> ""
          THEN DO:
            goErrorObject:addError
                            (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic, // ipcOwningEntityMnemonic
                             INPUT btt_auth_detail.auth_detail_obj,                   // ipdOwningEntityObj
                             INPUT "":U,                                              // ipcOwningEntityKey
                             INPUT btt_auth_detail.line_number,                       // ipiLineNumber
                             INPUT cError).                                           // ipcMessageText

            LEAVE.

          END. /* IF cError <> "" */
          ELSE
           ASSIGN
             btt_auth_detail.quantity_auth            = dQuantityAuth
             btt_auth_detail.minutes_calculation_rule = cMinutesCalcRule.

        END. /* IF   btt_auth_detail.auth_detail_obj <= 0.00... */

        IF btt_auth_detail.minutes_calculation_rule = "":U
        THEN
          goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                 INPUT btt_auth_detail.auth_detail_obj,
                                 INPUT "":U,
                                 INPUT "minutes_calculation_rule":U,
                                 INPUT btt_auth_detail.line_number,
                                 INPUT "MA":U,
                                 INPUT 112,  /* The &1 specified is invalid. &2 */
                                 INPUT "Minutes Calculation Rule , Minutes Calculation Rule must have a value.").

      END. /* IF btt_auth_detail.minutes_auth <> 0 */

      IF    btt_auth_detail.repeat_item            = YES
      AND   btt_auth_detail.repeat_cycle_auth     <> 0
      AND   btt_auth_detail.repeat_cycle_quantity <> 0
      AND ((btt_auth_detail.quantity_auth          = 1 AND LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmnl") > 0)   /* Default value for a tariff or nappi. */
      OR   (btt_auth_detail.quantity_auth          = 0 AND btt_auth_detail.owning_entity_mnemonic = "hlmcr"))
      THEN	  /* Default value for a basket. */
        ASSIGN btt_auth_detail.quantity_auth = (btt_auth_detail.repeat_cycle_auth * btt_auth_detail.repeat_cycle_quantity).

      /*
        Validate the auth detail line status
        Updated code to also assign the status on the detail line from the
        provider when a new line is added
        Moved code to top of include, to assign status before other checks
        are done eg Tariff Restrictions
      */
      IF btt_auth_detail.auth_status = ? OR btt_auth_detail.auth_detail_obj < 0 THEN
      DO:
        FIND btt_auth_provider NO-LOCK
          WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
          NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
        IF AVAILABLE btt_auth_provider
        AND btt_auth_detail.auth_status <> btt_auth_provider.auth_status THEN
        DO:
          ASSIGN cTrackingMessage = SUBSTITUTE("SaveDetailStatus - Assign detail line status (currently &1-&2) from the provider (&3-&4)",
                                               STRING(btt_auth_detail.auth_status),btt_auth_detail.auth_status_note,
                                               STRING(btt_auth_provider.auth_status),btt_auth_provider.auth_status_note).

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

          ASSIGN btt_auth_detail.record_action    = "MODIFY":U
                 btt_auth_detail.auth_status      = btt_auth_provider.auth_status
                 btt_auth_detail.auth_status_note = btt_auth_provider.auth_status_note.
        END.  // IF AVAILABLE btt_auth_provider THEN
      END.  /* IF btt_auth_detail.auth_status = ? THEN */

      /*
        Business Rules for the Line Restrictions should be activated before any amounts are
        calculated, because we should compare what is captured against what is calculated
        when the line_restriction = amount.
        For the LOC detail lines where the amount authorised has been update the line restrictions also need to run - this will
        only apply when LineRestriction rule is "System".
	      Skip line restriction processing if the item cost and los quantity match up to the calculated amount - this would mean that
	      the system calculated the cost and we do not want line restriction validations to intervene - only when the user has manually
	      updated the amount.
        We also want to skip the line restriction if the detail line was copied (auto-created) from another line
        because they are part of the same auth group.
      */
      ASSIGN cTrackingMessage = "AuthAmountDetail - Before maauthbusdetlinerestrict.i - amount_auth=" + STRING(btt_auth_detail.amount_auth)
                              + " item_cost=" + STRING(btt_auth_detail.item_cost).
      { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

      IF btt_auth_detail._apply_line_restrict_rules
      AND (btt_auth_detail.quantity_los = 0
       OR (AVAILABLE buf_auth_detail
           AND btt_auth_detail.quantity_los <> 0
           AND btt_auth_detail.quantity_los  * btt_auth_detail.item_cost <> btt_auth_detail.amount_auth
           AND buf_auth_detail.amount_auth  <> btt_auth_detail.amount_auth)) THEN
      DO:
        { ma/app/maauthbusdetlinerestrict.i }
      END.  /* IF btt_auth_detail.quantity_los = 0 THEN */

      IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
      DO:
        IF btt_auth_detail.related_entity_mnemonic = "hlmnl":U THEN
        DO:
          /*
            Ensure that only valid nappi codes are captured
          */
          ASSIGN lSuccess = mipEnv:Health:maMedical:getValidNappi
                                  (INPUT-OUTPUT btt_auth_detail.related_obj,    /* ipdNappiLinkObj */
                                   INPUT        btt_auth_detail.related_value,  /* ipcNappiCode    */
                                   INPUT        btt_auth_detail.start_date,     /* ipdDate         */
                                   OUTPUT       cError).                        /* opcError        */

          IF cError <> "" THEN
          DO:
            goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                          INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                          INPUT "":U,                                               /* ipcOwningEntityKey      */
                          INPUT "related_value":U,                                  /* FieldName               */
                          INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                          INPUT cError,                                             /* ipcMessageText          */
                          INPUT "Err").                                             /* ErrorType               */
            LEAVE.
          END.  /* IF cError <> "" THEN */
        END. /* IF btt_auth_detail.related_entity_mnemonic = "hlmnl":U THEN... */

        FIND FIRST btt_auth NO-LOCK
          WHERE btt_auth.auth_obj = btt_auth_detail.auth_obj
          NO-ERROR.

        IF AVAILABLE btt_auth THEN
        DO:
          FIND FIRST buf_memdep NO-LOCK
               WHERE buf_memdep.mem-num   = btt_auth.mem_num
                 AND buf_memdep.dependant = btt_auth.dependant
            NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE buf_memdep
          THEN
            ASSIGN
              l99Dependant = TRUE.

        END. /* IF AVAILABLE btt_auth THEN */

        /*
          Ensure that only valid tariff codes are captured
        */
        IF l99Dependant THEN
          ASSIGN
            cAddValidations = "".
        ELSE
          ASSIGN
            cAddValidations = "age,":U     + STRING(goAuthorisation:DependantAgeYears) +
                              "|gender,":U + goAuthorisation:DependantGender   +
                              "|BMI,":U    + STRING(goAuthorisation:DependantBMI).
        ASSIGN
          lSuccess = mipEnv:Health:maMedical:getValidTariff(INPUT-OUTPUT btt_auth_detail.owning_obj,        /*  iopdTariffLinkObj */
                                                            INPUT        btt_auth_detail.owning_alt_value,  /*  ipcTariffCode     */
                                                            INPUT        cBaseRate,                         /*  ipcBaseRate       */
                                                            INPUT        cArsRate,                          /*  ipcARSRate        */
                                                            INPUT        iPrType,                           /*  ipiPrType         */
                                                            INPUT        iSubPrType,                        /*  ipiSubPrType      */
                                                            INPUT        btt_auth_detail.start_date,        /*  ipdDate           */
                                                            INPUT        goAuthorisation:OptionCode,        /*  ipiOptionCode     */
                                                            INPUT        cAddValidations,                   /*  ipcAddValidations */
                                                            OUTPUT       dTariffObj,                        /*  opdTariffObj      */
                                                            OUTPUT       dTrfCostObj,                       /*  opdTrfCostObj     */
                                                            OUTPUT       cError,                            /*  opcError          */
                                                            OUTPUT       cWarning,                          /*  opcWarning        */
                                                            OUTPUT       cAlertMessage).                    /*  opcAlertMessage   */

        IF cError <> "" THEN
        DO:
          goErrorObject:addError
                            (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                             INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                             INPUT "":U,                                               /* ipcOwningEntityKey      */
                             INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                             INPUT cError).                                            /* ipcMessageText          */
          LEAVE.
        END.  /* IF cError <> "" THEN */

        IF cWarning <> "" THEN
        DO:
          goErrorObject:addError
                            (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                             INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                             INPUT "":U,                                               /* ipcOwningEntityKey      */
                             INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                             INPUT cWarning,                                           /* ipcMessageText          */
                             INPUT "WAR":U).                                           /* ipcMessageType          */
        END.  /* IF cWarning <> "" THEN */

        IF cAlertMessage <> "" THEN
        DO:
          goErrorObject:addError
                            (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                             INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                             INPUT "":U,                                               /* ipcOwningEntityKey      */
                             INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                             INPUT cAlertMessage,                                      /* ipcMessageText          */
                             INPUT "WAR":U).                                           /* ipcMessageType          */
        END.  /* IF cAlertMessage <> "" THEN */

        IF dTariffObj <> 0 THEN
        DO:
          FIND tariff NO-LOCK
            WHERE tariff.tariff-obj = dTariffObj
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE tariff THEN
          DO:
            goErrorObject:addError
                           (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                            INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                            INPUT "":U,                                               /* ipcOwningEntityKey      */
                            INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                            INPUT "Tariff not available").                            /* ipcMessageText          */
          END. /* IF NOT AVAILABLE tariff */

          IF tariff.nappi-required-auths = "ma_acNappiRequiredEnforce":U THEN
          DO:
            IF btt_auth_detail.related_entity_mnemonic = ""
            THEN
              goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,     /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                       /* ipdOwningEntityObj      */
                           INPUT "":U,                                                  /* ipcOwningEntityKey      */
                           INPUT "related_entity_mnemonic":U,                           /* ipcFieldName            */
                           INPUT btt_auth_detail.line_number,                           /* ipiLineNumber           */
                           INPUT "Related Entity is required for the tariff code",      /* ipcMessageText          */
                           INPUT "ERR":U).                                              /* ipcMessageType          */

            ELSE IF  btt_auth_detail.related_obj =  0
                 AND btt_auth_detail.related_key = ""
            THEN
              goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,     /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                       /* ipdOwningEntityObj      */
                           INPUT "":U,                                                  /* ipcOwningEntityKey      */
                           INPUT "related_value":U,                                     /* ipcFieldName            */
                           INPUT btt_auth_detail.line_number,                           /* ipiLineNumber           */
                           INPUT "Related Entity Code is required for the tariff code", /* ipcMessageText          */
                           INPUT "ERR":U).                                              /* ipcMessageType          */


            ELSE IF btt_auth_detail.related_value = ""
            THEN
              goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,     /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                       /* ipdOwningEntityObj      */
                           INPUT "":U,                                                  /* ipcOwningEntityKey      */
                           INPUT "related_value":U,                                     /* ipcFieldName            */
                           INPUT btt_auth_detail.line_number,                           /* ipiLineNumber           */
                           INPUT "Related Entity Code is required for the tariff code", /* ipcMessageText          */
                           INPUT "ERR":U).                                              /* ipcMessageType          */
          END. /* IF tariff.nappi-required-auths = "ma_acNappiRequiredEnforce" */

          IF tariff.nappi-required-auths = "ma_acNappiRequiredOptional":U
          OR tariff.nappi-required-auths = "ma_acNappiRequiredEnforce":U THEN
          DO:
            IF btt_auth_detail.related_entity_mnemonic <> "" THEN
            DO:
              FIND FIRST mic_acronym NO-LOCK
                   WHERE mic_acronym.category_key  = "ma_acAuthDetailRelated":U
                     AND mic_acronym.acronym_value = btt_auth_detail.related_entity_mnemonic
                NO-ERROR.

              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

              IF NOT AVAILABLE mic_acronym
              THEN
                goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                           INPUT "":U,                                                /* ipcOwningEntityKey      */
                           INPUT "related_entity_mnemonic":U,                         /* ipcFieldName            */
                           INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                           INPUT "Invalid Related Entity",                            /* ipcMessageText          */
                           INPUT "ERR":U).                                            /* ipcMessageType          */

            END. /* IF tt_auth_detail.related_entity_mnemonic <> "" */

            IF btt_auth_detail.related_entity_mnemonic = "hlmcr":U  // Crosswalk
            AND (btt_auth_detail.related_obj   <> 0
            OR   btt_auth_detail.related_value <> "")
            THEN
              /* Validate that a valid Nappi is set up in the Basket details */
              mipEnv:Health:maMedical:validateCrosswalk(INPUT-OUTPUT btt_auth_detail.related_obj,
                                                        INPUT        btt_auth_detail.related_value,
                                                        INPUT 	     "ma_AcCrossTypeBasket",
                                                        INPUT 	     btt_auth_detail.start_date,
                                                        INPUT 	     TRUE,
                                                        INPUT 	     "hlmnl":U,
                                                        OUTPUT       cError).

            IF cError <> "" THEN
            DO:
              goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                           INPUT "":U,                                                /* ipcOwningEntityKey      */
                           INPUT "related_value":U,      	                            /* ipcFieldName            */
                           INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                           INPUT cError,                                              /* ipcMessageText          */
                           INPUT "ERR":U).                                            /* ipcMessageType          */
              LEAVE.
            END.  /* IF cError <> "" THEN */
          END. /* IF tariff.nappi-required-auths = "ma_acNappiRequiredOptional" OR */

          IF (tariff.nappi-required-auths = "ma_acNappiRequiredNone":U
          OR  tariff.nappi-required-auths = "")
          AND (btt_auth_detail.related_entity_mnemonic <> ""
          OR   btt_auth_detail.related_obj             <> 0
          OR   btt_auth_detail.related_key             <> "") THEN
          DO:
            FIND FIRST mic_acronym NO-LOCK
                 WHERE mic_acronym.category_key  = "ma_acAuthDetailRelated":U
                   AND mic_acronym.acronym_value = btt_auth_detail.related_entity_mnemonic
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE mic_acronym THEN
            DO:
              ASSIGN cError = mic_acronym.acronym_label + " not required for this tariff code".

              goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,    /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                           INPUT "":U,                                                /* ipcOwningEntityKey      */
                           INPUT "related_entity_mnemonic":U,                         /* ipcFieldName            */
                           INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                           INPUT cError,                                              /* ipcMessageText          */
                           INPUT "ERR":U).                                            /* ipcMessageType          */
            END. /* IF AVAILABLE mic_acronym */
          END. /* IF (tariff.nappi-required-auths = "ma_acNappiRequiredNone" OR */

          IF  btt_auth_detail.body_region   <> ""
          AND btt_auth_detail.record_action <> "delete":U THEN
          DO:
            mipEnv:Health:maMedical:validateTariffBodyRegion( INPUT   tariff.tariff-code,
                                                              INPUT   tariff.pr-type,
                                                              INPUT   tariff.effect-date,
                                                              INPUT   tariff.base-rate,
                                                              INPUT   tariff.ars-rate,
                                                              INPUT   btt_auth_detail.body_region,
                                                              OUTPUT  lBodyRegionValid,
                                                              OUTPUT  cValidationErrorMessage).

            IF NOT lBodyRegionValid AND cValidationErrorMessage <> "" THEN
              goErrorObject:addError
                         ( INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                           INPUT "":U,                                                /* ipcOwningEntityKey      */
                           INPUT "body_region":U,                                     /* ipcFieldName            */
                           INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                           INPUT cValidationErrorMessage,                             /* ipcMessageText          */
                           INPUT "ERR":U).                                            /* ipcMessageType          */

          END. /* IF btt_auth_detail.body_region <> "" THEN */

        END. /* IF dTariffObj <> 0 THEN */

        IF dTrfCostObj <> 0 THEN
        DO:
          FIND trfcost NO-LOCK
               WHERE trfcost.trfcost-obj = dTrfCostObj
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE trfcost
          THEN
            ASSIGN cTrackingMessage = "AuthAmountDetail - trfcost.trfcost-obj=" + STRING(trfcost.trfcost-obj)
                                    + " btt_auth_detail.claim_code=" + STRING(btt_auth_detail.claim_code)
                                    + " trfcost.claim-code="         + STRING(trfcost.claim-code)
                                    + " btt_auth_detail.claim_type=" + btt_auth_detail.claim_type
                                    + " trfcost.claim-type="         + trfcost.claim-type
                                    + " trfcost.amount="             + STRING(trfcost.amount).
          ELSE
            ASSIGN cTrackingMessage = "AuthAmountDetail - trfcost for obj " + STRING(dTrfCostObj) + " not available".

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

          /*
            Assign tariff claim code and claim type defaults
          */
          { ma/app/maauthbusdettariffdefaults.i }

          /*
            Activate tariff cost benefit rule processing
          */
          { ma/app/maauthbusdettrfcostben.i }

          /*
            Check if a tariff restriction applies
          */
          { ma/app/maauthbusdettariffrestr.i }

          /*
            Check if a tariff is an exclusion
          */
          { ma/app/maauthbusdettariffexcl.i }

          /*
            Tariff reason processing
          */
          { ma/app/maauthbusdettariffrsn.i }

        END.  /* IF dTrfCostObj <> 0 THEN */

        /*
          Only assign if no related item is linked to the tariff and if the detail line is not a LOC line
          and if the detail line was not auto-created as part of an auth group.
        */
        IF (btt_auth_detail.related_entity_mnemonic = ""
        OR  btt_auth_detail.related_entity_mnemonic = "hatad":U)
        AND btt_auth_detail.quantity_los            = 0
        AND btt_auth_detail._apply_line_restrict_rules
        THEN DO:
          /*
            Only overwrite Auth Detail amount & cost with trfcost amount if the detail line was added by the user or
            if the detail line was pulled in from the Crosswalk but no amount was specified on the Crosswalk details.
            Line restriction will be assigned to "ma_acAuthLineRestrictionAmount" if an amount was specified on the
            Crosswalk details
          */
          IF AVAILABLE trfcost
          AND (btt_auth_detail.added_by_user
           OR (NOT btt_auth_detail.added_by_user
          AND btt_auth_detail.line_restriction <> "ma_acAuthLineRestrictionAmount":U)) THEN
          DO:
            ASSIGN btt_auth_detail.item_cost       = IF lTrfCostBenefit AND (dTrfCostAmount <> 0 OR dTrfcostFixedAmount <> 0)
                                                     THEN dTrfCostAmount
                                                     ELSE trfcost.amount
                   btt_auth_detail.fixed_item_cost = IF lTrfcostBenefit AND (dTrfCostAmount <> 0 OR dTrfcostFixedAmount <> 0)
                                                     THEN dTrfCostFixedAmount
                                                     ELSE trfcost.fixed-amount.

            /*
              Only do this calc if the user did not update the Quantity Auth to zero,
              and the line restriction was adjusted to Unlimited
            */
            IF btt_auth_detail.line_restriction <> "ma_acAuthLineRestrictionUnlimited":U THEN
            DO:
              IF trfcost.cost-calculation = ""
              THEN
                ASSIGN btt_auth_detail.amount_auth = btt_auth_detail.fixed_item_cost + (btt_auth_detail.quantity_auth * btt_auth_detail.item_cost).
              ELSE DO:
                /* Calculate the Tariff Cost amount */
                mipEnv:Health:AuthService:calcAmountAuth(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE, /* dsAuthorisation          */
                                                         INPUT  btt_auth_detail.auth_detail_obj,            /* ipdAuthDetailObj         */
                                                         INPUT  trfcost.cost-calculation,                   /* ipcCostCalculation       */
                                                         INPUT  btt_auth_detail.item_cost,                  /* ipdTrfcostAmount         */
                                                         OUTPUT dItemCost,                                  /* opdItemCost              */
                                                         OUTPUT cLineRestriction,                           /* opcLineRestriction       */
                                                         OUTPUT cRelatedEntityMnemonic,                     /* opcRelatedEntityMnemonic */
                                                         OUTPUT dRelatedObj,                                /* opdRelatedObj            */
                                                         OUTPUT cRelatedValue,                              /* opcRelatedValue          */
                                                         OUTPUT dCostCalculated,                            /* opdCostCalculated        */
                                                         OUTPUT cError).                                    /* opcCostError             */

                IF cError = ""
                THEN
                  ASSIGN btt_auth_detail.item_cost               = IF dItemCost <> 0
                                                                   THEN dItemCost
                                                                   ELSE btt_auth_detail.item_cost
                         btt_auth_detail.line_restriction        = IF cLineRestriction <> ""
                                                                   THEN cLineRestriction
                                                                   ELSE btt_auth_detail.line_restriction
                         btt_auth_detail.related_entity_mnemonic = IF cRelatedEntityMnemonic <> ""
       	                                                           THEN cRelatedEntityMnemonic
                                                                   ELSE btt_auth_detail.related_entity_mnemonic
                         btt_auth_detail.related_obj             = IF dRelatedObj <> 0
                                                                   THEN dRelatedObj
                                                                   ELSE btt_auth_detail.related_obj
                         btt_auth_detail.related_value           = IF cRelatedValue <> ""
                                                                   THEN cRelatedValue
                                                                   ELSE btt_auth_detail.related_value
                         btt_auth_detail.amount_auth             = dCostCalculated.
                ELSE DO:
                  goErrorObject:addError
                               (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                                INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                                INPUT "":U,                                               /* ipcOwningEntityKey      */
                                INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                                INPUT cError).                                            /* ipcMessageText          */
                END.  /* ELSE - IF cError <> "" THEN */
              END.  /* ELSE - IF trfcost.cost-calculation = "" THEN */
            END. /* IF btt_auth_detail.line_restriction <> "ma_acAuthLineRestrictionUnlimited":U */
          END.  /* IF AVAILABLE trfcost AND btt_auth_detail.added_by_user THEN */
          ELSE
            ASSIGN btt_auth_detail.item_cost       = ROUND(btt_auth_detail.amount_auth / btt_auth_detail.quantity_auth,2)
                   btt_auth_detail.fixed_item_cost = 0.
        END. /* IF btt_auth_detail.related_entity_mnemonic = "" AND */
      END. /* IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN */

      /*
        Validate the nappi code
      */
      /*
        Nappi is captured as a related entity on a nappi
      */
      IF (btt_auth_detail.owning_entity_mnemonic  = "hlmnl":U
      AND btt_auth_detail.related_entity_mnemonic = "hlmnl":U) THEN
      DO:
        ASSIGN cError = "Related entity nappi not allowed with entity nappi".
        goErrorObject:addError
                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,    /* ipcOwningEntityMnemonic */
                      INPUT btt_auth_detail.auth_detail_obj,                      /* ipdOwningEntityObj      */
                      INPUT "":U,                                                 /* ipcOwningEntityKey      */
                      INPUT "related_value":U,                                    /* FieldName               */
                      INPUT btt_auth_detail.line_number,                          /* ipiLineNumber           */
                      INPUT "Related entity nappi not allowed with entity nappi", /* ipcMessageText          */
                      INPUT "Err":U).                                             /* ErrorType               */

        LEAVE.
      END. /* IF btt_auth_detail.owning_entity_mnemonic = "hlmnl":U... */
      ELSE DO:
        /* Nappi is captured as a detail entity */
        IF  btt_auth_detail.owning_entity_mnemonic  = "hlmnl":U THEN
        DO:
          /*
            Ensure that only valid nappi codes are captured
          */
          ASSIGN lSuccess = mipEnv:Health:maMedical:getValidNappi
                                  (INPUT-OUTPUT btt_auth_detail.owning_obj,        /* ipdNappiLinkObj */
                                   INPUT        btt_auth_detail.owning_alt_value,  /* ipcNappiCode    */
                                   INPUT        btt_auth_detail.start_date,        /* ipdDate         */
                                   OUTPUT       cError).                           /* opcError        */

          IF cError <> "" THEN
          DO:
            goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                          INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                          INPUT "":U,                                               /* ipcOwningEntityKey      */
                          INPUT "owning_alt_value":U,                               /* FieldName               */
                          INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                          INPUT cError,                                             /* ipcMessageText          */
                          INPUT "Err").                                             /* ErrorType               */
            LEAVE.
          END. /* IF cError <> "" THEN */
        END. /*ELSE - IF btt_auth_detail.owning_entity_mnemonic = "hlmnl":U */

        /*
          Calculate Nappi Price
        */
        { ma/app/maauthbusdetnappiprice.i }

        /*
          Check for nappi benefit setups
        */
        { ma/app/maauthbusdetnappibenefit.i }

        /*
          If no nappi benefits are assigned
        */
        IF NOT lNappiBenefits
        THEN DO:
          /*
            Get the Nappi default claim code and claim type
          */
          { ma/app/maauthbusdetnappidefaults.i }

          /*
            Check if a nappi item is an exclusion
          */
          { ma/app/maauthbusdetnappiexcl.i }
        END.  /* IF NOT lNappiBenefits  */

        /*
          Check if a nappi item is a non-chargeable
        */
        { ma/app/maauthbusdetnappinoncharge.i }

      END. /* ELSE DO: IF btt_auth_detail.owning_entity_mnemonic = "hlmnl":U... */

      /*
          Basket is captured as a detail entity
      */
      IF  btt_auth_detail.owning_entity_mnemonic  = "hlmcr":U THEN
      DO:
        /*
          Ensure that only valid basket codes are captured.  Check that the basket has valid Nappi and Tariff setup.
        */
        ASSIGN lSuccess = mipEnv:Health:maMedical:validateCrosswalk
                                (INPUT-OUTPUT  btt_auth_detail.owning_obj,  	  /* ipdCrosswalkObj  */
                                 INPUT         btt_auth_detail.owning_alt_value,  /* ipcCrosswalkCode */
                                 INPUT         "ma_acCrossTypeBasket":U,          /* ipcCrosswalkType */
                                 INPUT         btt_auth_detail.start_date,        /* ipdStartDate     */
                                 INPUT         YES,                               /* iplCheckDetails  */
                                 INPUT         "hlmnl,htmtl":U,                   /* ipcDetailEntity  */
                                 OUTPUT        cError).                           /* opcError         */

        IF cError <> "" THEN
        DO:
          goErrorObject:addError
                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                        INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                        INPUT "":U,                                               /* ipcOwningEntityKey      */
                        INPUT "owning_alt_value":U,                               /* FieldName               */
                        INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                        INPUT cError,                                             /* ipcMessageText          */
                        INPUT "Err").                                             /* ErrorType               */
          LEAVE.
        END. /* IF cError <> "" THEN */

        /*
          Get the Basket default claim code and claim type
        */
        { ma/app/maauthbusdetbasketdefaults.i }

      END. /*IF btt_auth_detail.owning_entity_mnemonic = "hlmcr":U */

      /*
        Line Restriction Validations
      */
      { ma/app/maauthbusdetlinerestrictval.i }

      /*
        Authorisation Type Detail Exclusions
      */
      { ma/app/maauthbusdetauthtypeexcl.i }

      /*
       	We need to make sure the auth detail line status is changed to 'declined' if the btt_auth_detail.claim_code is '99'.
       	Check if claim_code = 99, then assign auth_status = 6 before the btt_auth_detail.auth_status is validated.

        LC - Made adjustments to the IF to only do the detail line status change check, if the claim code was
        changed by the user
      */
      IF AVAILABLE buf_auth_detail
      AND buf_auth_detail.claim_code <> btt_auth_detail.claim_code
      AND btt_auth_detail.claim_code <> 0 THEN
      DO:
        IF NOT AVAILABLE buf_auth_schext
        AND VALID-OBJECT(goAuthorisation)
        THEN
          FIND FIRST buf_auth_schext NO-LOCK
            WHERE buf_auth_schext.scheme-code = goAuthorisation:OptionCode
            NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'Progress:565' &ResetIgnoredErrors = TRUE}

        IF AVAILABLE buf_auth_schext
        AND btt_auth_detail.claim_code = buf_auth_schext.claim-code[1] THEN
        DO:
          ASSIGN cTrackingMessage = SUBSTITUTE("SaveDetailStatus - Change the detail line status to 6 (Declined) because the cc='&1'",
                                               STRING(btt_auth_detail.claim_code)).

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

          ASSIGN cError                      = "[Status]":U WHEN  btt_auth_detail.auth_status <> 6
                 btt_auth_detail.auth_status = 6 /* Declined */
                 lSuccess                    = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                                                          (INPUT  goAuthorisation:InsurerObj,
                                                                           INPUT  goAuthorisation:OptionCode,
                                                                           INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                                           INPUT  "NilPaymentDefaultReason":U,
                                                                           INPUT  goAuthorisation:StartDate,
                                                                           OUTPUT lValidRule,
                                                                           OUTPUT cRuleValue).
          IF lValidRule
          AND TRIM(cRuleValue) <> "":U
          AND TRIM(btt_auth_detail.auth_status_note) = "":U
          THEN
            ASSIGN btt_auth_detail.auth_status_note = cRuleValue
                   cError                           = cError + " [StatusNote]":U.

          ASSIGN cError   = REPLACE(cError,"[Status]":U,"Detail line ~"" + btt_auth_detail.owning_alt_value + "~" status changed to Declined.")
                 cError   = REPLACE(cError,"[StatusNote]":U ,"Status reason changed to " + cRuleValue)
                 cError   = cError + ".[HELP=Auth Rule Code: NilPaymentDefaultReason]"
                 lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                /* ipcOwningEntityKey      */
                                          INPUT "auth_status":U,                                     /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                                          INPUT cError,                                              /* ipcMessageText          */
                                          INPUT "WAR":U) WHEN  cError <> "":U .                      /* ipcMessageType          */
        END.	/*IF AVAILABLE buf_auth_schext AND btt_auth_detail.claim_code  = buf_auth_schext.claim-code[1] THEN DO:*/
      END.  /*IF btt_auth_detail.claim_code <> 0 THEN*/

      /*
        Acivate the co-payment processing
      */
      { ma/app/maauthbussavedetcopay.i }

      /*
        Activate Limit checking
      */
      { ma/app/maauthbusdetchecklimits.i }

      /*
        Apply auth limit controls
      */
      { ma/app/maauthbusdetlimcntrl.i }

      /*
        Ensure that only a valid auth status is used according to system and rule setups
      */
      ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  btt_auth_detail.auth_status,
                                                                         INPUT  "System":U).

      IF NOT lValidStatus THEN
      DO:
        ASSIGN cError   = "Status Code: ":U + STRING(btt_auth_detail.auth_status)
                        + ",[HELP=Auth Rule Code: ValidStatuses]"
               lSuccess = goErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */
                                                 INPUT btt_auth_detail.auth_detail_obj,                   /* ipdOwningEntityObj      */
                                                 INPUT "":U,                                              /* ipcOwningEntityKey      */
                                                 INPUT "auth_status":U,                                   /* ipcFieldName            */
                                                 INPUT btt_auth_detail.line_number,                       /* ipiLineNumber           */
                                                 INPUT "MA":U,                                            /* ipcMessageGroup         */
                                                 INPUT 112,  /* The '&1' specified is invalid. &2 */      /* ipiMessageNumber        */
                                                 INPUT cError).                                           /* ipcReplaceTextList      */
      END.  /* IF NOT lValidStatus THEN */
      ELSE DO:
        /*
          If a detail line is declined, make sure the claim code is changed to '99'
        */
        IF  btt_auth_detail.auth_status = 6                                /* Declined. */
        AND btt_auth_detail.claim_code <> buf_auth_schext.claim-code[1]    /* Claim code =  99. */
        THEN
          ASSIGN btt_auth_detail.claim_code = buf_auth_schext.claim-code[1].

        IF AVAILABLE buf_auth_detail
        AND buf_auth_detail.auth_status <> btt_auth_detail.auth_status THEN
        DO:
          FIND buf_auth_provider NO-LOCK
            WHERE buf_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

          ASSIGN cError = mipEnv:Health:AuthService:validateAuthStatusUpdate(INPUT  goAuthorisation:InsurerObj,
                                                                             INPUT  goAuthorisation:OptionCode,
                                                                             INPUT  buf_auth_detail.auth_status,
                                                                             INPUT  buf_auth_detail.auth_status_note,
                                                                             INPUT  btt_auth_detail.auth_status,
                                                                             INPUT  btt_auth_detail.amount_paid,
                                                                             INPUT  btt_auth_detail.quantity_paid,
                                                                             INPUT  btt_auth_detail.start_date,
                                                                             INPUT  btt_auth_provider.auth_status,
                                                                             INPUT  btt_auth_provider.auth_status_note).

          IF cError <> "":U
          THEN
            ASSIGN lSuccess = goErrorObject:addError
                                           (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                                            INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                                            INPUT "":U,                                                /* ipcOwningEntityKey      */
                                            INPUT "auth_status":U,                                     /* ipcFieldName            */
                                            INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                                            INPUT cError,                                              /* ipcMessageText          */
                                            INPUT "ERR":U).                                            /* ipcMessageType          */

          /*
            When the detail line is authorised after it was declined we need to
            assign the original default claim code and default claim type
          */
          IF buf_auth_detail.auth_status = 6 /* Declined */ THEN
          DO:
            ASSIGN cNewStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                                    INPUT btt_auth_detail.auth_status)
                   cNewStatus      = STRING(btt_auth_detail.auth_status) + "-":U + cNewStatusDescr.
            ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                    (INPUT  goAuthorisation:InsurerObj,
                                     INPUT  goAuthorisation:OptionCode,
                                     INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                     INPUT  "Update6Declined":U,
                                     INPUT  btt_auth_detail.start_date,
                                     OUTPUT lValidRule,
                                     OUTPUT cRuleValue).

            IF LOOKUP(cNewStatus,cRuleValue) > 0
            THEN
              ASSIGN btt_auth_detail.claim_code = btt_auth_detail.default_claim_code
                     btt_auth_detail.claim_type = btt_auth_detail.default_claim_type.
          END.  /* IF buf_auth_detail.auth_status = 6 /* Declined */ THEN */
        END.  /* IF AVAILABLE buf_auth_detail AND ... */

        /*
          Check if the status reason/note on the auth detail is mandatory
        */
        IF btt_auth_detail.auth_status_note = "":U THEN
        DO:
          ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT btt_auth_detail.auth_status,
                                                                              INPUT goAuthorisation:InsurerObj,
                                                                              INPUT goAuthorisation:MemberOptionCode,
                                                                              INPUT btt_auth_detail.start_date).
          IF lMandatory THEN
          DO:
            /*
              Auth detail record added - default the reason from the provider
            */
            IF btt_auth_detail.auth_detail_obj <= 0 THEN
            DO:
              FIND btt_auth_provider NO-LOCK
                WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
                NO-ERROR.

              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

              IF AVAILABLE btt_auth_provider
              AND btt_auth_provider.auth_status = btt_auth_detail.auth_status
              THEN
                ASSIGN btt_auth_detail.auth_status_note = btt_auth_provider.auth_status_note.
            END.  /* IF btt_auth_detail.auth_detail_obj <= 0 THEN  */

            /*
              If the status reason is still blank at this stage , we have a problem
            */
            IF btt_auth_detail.auth_status_note = "":U
            THEN
              ASSIGN cError   = SUBSTITUTE("Status reason,A valid status reason must be provided for the status (&1) specified.":U,
                                           STRING(btt_auth_detail.auth_status))
                              + "[HELP=Auth Rule Code: EnforceStatusNote]":U
                     lSuccess = goErrorObject:addError
                                             (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                              INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                              INPUT "":U,                                                /* ipcOwningEntityKey       */
                                              INPUT "auth_status_note":U,                                /* ipcFieldName             */
                                              INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                              INPUT "MA":U,                                              /* ipcMessageGroup          */
                                              INPUT 112,       /* The '&1' specified is invalid. &2  */  /* ipiMessageNumber         */
                                              INPUT cError).                                             /* ipcReplaceTextList       */
          END.  /* IF lMandatory THEN */
        END.  /* IF btt_auth_detail.auth_status_note = "" THEN */

        /* Validate status reason if specified */
        IF btt_auth_detail.auth_status_note <> "":U THEN
        DO:
          mipEnv:Health:AuthService:ValidateStatusReason(INPUT  goAuthorisation:InsurerObj,
                                                         INPUT  00,
                                                         INPUT  btt_auth_detail.auth_status_note,
                                                         INPUT  INTEGER(btt_auth_detail.auth_status),
                                                         INPUT  btt_auth_detail.start_date,
                                                         OUTPUT cError).

          IF cError <> "":U THEN
          DO:
            goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                   INPUT btt_auth_detail.auth_detail_obj,
                                   INPUT "":U,
                                   INPUT "auth_status_note":U,
                                   INPUT btt_auth_detail.line_number,
                                   INPUT cError,
                                   INPUT "ERR":U).
          END. /* IF cError <> "":U THEN  */
        END. /* IF btt_auth_detail.auth_status_note <> "":U */
      END. /* ELSE - IF NOT lValidStatus THEN */

      /*
        Check for discounts and validate that the auth detail discount if entered, is valid
      */
      IF  btt_auth_detail.discount_auth <> 0
      AND btt_auth_detail.discount_auth <> ? THEN
      DO:
        mipEnv:Health:AuthService:checkForDiscounts(INPUT  0,
                                                    INPUT  0,
                                                    INPUT  btt_auth_detail.auth_detail_obj,
                                                    OUTPUT cError).
        IF cError <> "" THEN
        DO:
          IF buf_auth_detail.auth_status <> btt_auth_detail.auth_status
          THEN
            ASSIGN cErrorField = "auth_status":U.
          ELSE
            ASSIGN cErrorField = "discount_auth":U.

          goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                 INPUT btt_auth_detail.auth_detail_obj,
                                 INPUT "":U,
                                 INPUT cErrorField,
                                 INPUT btt_auth_detail.line_number,
                                 INPUT cError,
                                 INPUT "ERR":U).
        END.  // IF cError <> "" THEN
      END.  // IF btt_auth_detail.discount_auth <> 0 THEN
    END.  // IF btt_auth_detail.record_action <> "DELETE":U

    /*
      Tariff Restriction Deletions
    */
    { ma/app/maauthbusdettariffrestrfdel.i }

    /*
      Enforced field validations
    */
    { ma/app/maauthbusdetenforce.i }

    /*
      Validate claim code if specified
    */
    IF (btt_auth_detail.claim_code <> 0 AND btt_auth_detail.auth_detail_obj <= 0)
    OR (AVAILABLE buf_auth_detail AND buf_auth_detail.claim_code <> btt_auth_detail.claim_code AND btt_auth_detail.claim_code <> 0 ) THEN
    DO:
      FIND FIRST btt_auth_provider NO-LOCK
           WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      ASSIGN iNegNum  = IF AVAILABLE btt_auth_provider THEN INTEGER(TRIM(ENTRY(1, btt_auth_provider._neg_group, ":"), "(")) ELSE 0

             lSuccess = mipEnv:Health:AuthBusinessLogic:validateClaimCode(INPUT  goAuthorisation:InsurerObj,
                                                                          INPUT  goAuthorisation:OptionCode,
                                                                          INPUT  goAuthorisation:MemNum,
                                                                          INPUT  goAuthorisation:Dependant,
                                                                          INPUT  btt_auth_detail.claim_code,
                                                                          INPUT  goAuthorisation:AuthTypeObj,
                                                                          INPUT  btt_auth_detail.start_date,
                                                                          INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.provider_type ELSE "",   /* Provider Type */
                                                                          INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.pr_type ELSE 0,          /* Discipline */
                                                                          INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.sub_pr_type ELSE 0,      /* Sub-Discipline */
                                                                          INPUT  iNegNum ,                                                                      /* Negotiation Number */
                                                                          INPUT  "hat_auth_detail":U,                                                           /* Auth level */
                                                                          OUTPUT lValidationError,
                                                                          OUTPUT cError).
      IF cError <> "" THEN
      DO:
        ASSIGN cMessageType = (IF lValidationError = TRUE
                               THEN "WAR":U
                               ELSE "ERR":U)

               lSuccess = goErrorObject:addError
                              (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                               INPUT "":U,                                                /* ipcOwningEntityKey       */
                               INPUT "claim_code":U,                                      /* ipcFieldName             */
                               INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                               INPUT cError,                                              /* ipcMessageText           */
                               INPUT cMessageType).                                       /* ipcMessageType           */
      END. /* IF cError <> "" THEN */
    END.  /* IF btt_auth_detail.claim_code <> 0 THEN */

    /* Validate claim type if specified */
    IF (btt_auth_detail.claim_type <> "":U AND btt_auth_detail.auth_detail_obj <= 0 )
    OR (AVAILABLE buf_auth_detail AND buf_auth_detail.claim_type <> btt_auth_detail.claim_type AND btt_auth_detail.claim_type <> "":U ) THEN
    DO:
      IF NOT AVAILABLE btt_auth_provider THEN
      DO:
        FIND FIRST btt_auth_provider NO-LOCK
             WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      END.  /* IF NOT AVAILABLE btt_auth_provider THEN */

      ASSIGN lSuccess = mipEnv:Health:AuthBusinessLogic:validateClaimType(INPUT  goAuthorisation:InsurerObj,
                                                                          INPUT  goAuthorisation:OptionCode,
                                                                          INPUT  btt_auth_detail.claim_type,
                                                                          INPUT  goAuthorisation:AuthTypeObj,
                                                                          INPUT  btt_auth_detail.start_date,
                                                                          INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.provider_type ELSE "", /* Provider Type */
                                                                          INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.pr_type       ELSE 0,  /* Discipline */
                                                                          INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.sub_pr_type   ELSE 0,  /* Sub-Discipline */
                                                                          INPUT  iNegNum,                                                                     /* Negotiation Number */
                                                                          INPUT  "hat_auth_detail":U,                                                         /* Auth level */
                                                                          OUTPUT lValidationError,
                                                                          OUTPUT cError).
      IF cError <> "" THEN
      DO:
        ASSIGN cMessageType = (IF lValidationError = TRUE
                               THEN "WAR":U
                               ELSE "ERR":U)

               lSuccess = goErrorObject:addError
                                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                        INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                        INPUT "":U,                                                /* ipcOwningEntityKey       */
                                        INPUT "claim_type":U,                                      /* ipcFieldName             */
                                        INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                        INPUT cError,                                              /* ipcMessageText           */
                                        INPUT cMessageType).                                       /* ipcMessageType           */
      END. /* IF cError <> "" THEN */
    END.  /* IF btt_auth_detail.claim_type <> "":U THEN */

    /*
      Keep record of the old auth group for use further down
    */
    IF AVAILABLE buf_auth_detail
    THEN
      ASSIGN dAuthGroupObj = buf_auth_detail.auth_group_obj.

    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthDetail(BUFFER btt_auth_detail, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).

    /*
      When a co-payment is applied to a Clinical Detail Line that is added,
      the Clinical Detail Line Obj must be updated on table tt_auth_copay.
    */
    IF dAuthDetailObj <> btt_auth_detail.auth_detail_obj THEN
    DO:
      IF CAN-FIND(FIRST tt_auth_copay
        WHERE tt_auth_copay.auth_obj               = btt_auth_detail.auth_obj
        AND   tt_auth_copay.owning_entity_mnemonic = "hatad":U
        AND   tt_auth_copay.owning_obj             = dAuthDetailObj) THEN
      DO:
        ASSIGN cTrackingMessage = "DebugAuthCopayDet-UPDATE tt_auth_copay with:"
                                + "auth_obj="         + STRING(btt_auth_detail.auth_obj)
                                + ";oem='hatad'"
                                + ";prev_owning_obj=" + STRING(dAuthDetailObj)
               cTrackingMessage = cTrackingMessage + ";new_owning_obj="
                                + IF AVAILABLE btt_auth_detail
                                  THEN STRING(btt_auth_detail.auth_detail_obj)
                                  ELSE "btt_auth_detail not available".

       {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      END.  // IF CAN-FIND(FIRST tt_auth_copay
      FOR EACH tt_auth_copay
        WHERE tt_auth_copay.auth_obj               = btt_auth_detail.auth_obj
        AND   tt_auth_copay.owning_entity_mnemonic = "hatad":U
        AND   tt_auth_copay.owning_obj             = dAuthDetailObj:

        ASSIGN tt_auth_copay.owning_obj = btt_auth_detail.auth_detail_obj
               tt_auth_copay.record_action = "MODIFY":U.
        VALIDATE tt_auth_copay.

      END.  /* FOR EACH tt_auth_copay */
    END.  // IF dAuthDetailObj <> btt_auth_detail.auth_detail_obj THEN

    /*
      Check if any tariff modifiers must be deleted automatically
    */
    IF  AVAILABLE btt_auth_detail
    AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
    AND btt_auth_detail.auth_status  <> 6 /* Declined */
    AND btt_auth_detail.auth_status  <> 5 /* Cancelled */
    AND btt_auth_detail.record_action = "delete":U THEN
    DO:
      /*
        When we create a modifier tariff line from an LOC Tariff, then we assign the
        auth_detail_obj of the LOC tariff to the related_obj on the modifier tariff.
        So when we delete this LOC tariff record now, then we want to delete all the
        modifier tariff records as well that were auto created with the LOC Tariff...
      */
      FOR EACH mod_auth_detail EXCLUSIVE-LOCK
        WHERE mod_auth_detail.auth_obj                = btt_auth_detail.auth_obj
        AND   mod_auth_detail.related_entity_mnemonic = "hatad":U
        AND   mod_auth_detail.related_obj             = btt_auth_detail.auth_detail_obj
        AND   mod_auth_detail.quantity_los            = 0: /* This is so that we only pick up the modifier tariffs */

        /*
          Check to see if other loc tariffs were linked to this modifier
          When a rate control change was triggered, it can cause all the old LOC-tariffs
          to be deleted and then the modifier must be deleted with them.
          We added the "CAN-FIND" in this where-clause so that we don't find one of the
          other LOC-tariff records that were already deleted.
        */
        FIND FIRST loc_auth_detail NO-LOCK
             WHERE loc_auth_detail.auth_obj                = btt_auth_detail.auth_obj
             AND   loc_auth_detail.related_entity_mnemonic = "hatad":U
             AND   loc_auth_detail.related_obj             = mod_auth_detail.auth_detail_obj
             AND   loc_auth_detail.auth_detail_obj        <> btt_auth_detail.auth_detail_obj
             AND   loc_auth_detail.quantity_los           <> 0
             AND   loc_auth_detail.record_action          <> "delete":U
             /* The record action is cleared after the record was saved. Do
                this can-find to make sure the LOC-tariff was not deleted. */
             AND  CAN-FIND(hat_auth_detail WHERE hat_auth_detail.auth_detail_obj = loc_auth_detail.auth_detail_obj)
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = FALSE }

        /*
          If no other LOC tariffs on the auth is linked to the modifier tariff,
          then we can 'automatically' delete the modifier tariff as well.
        */
        IF NOT AVAILABLE loc_auth_detail
        THEN
          ASSIGN mod_auth_detail.record_action = "DELETE":U.
        ELSE
          /*
            However, if the modifier is linked to this LOC-tariff that we're deleting,
            we no longer want the modifier to point to this deleted LOC-tariff.
            Rather assign the next valid LOC to the modifier's related fields.
          */
          ASSIGN mod_auth_detail.related_obj   = loc_auth_detail.auth_detail_obj
                 mod_auth_detail.related_value = loc_auth_detail.owning_alt_value.

      END.  /* FOR EACH mod_auth_detail EXCLUSIVE-LOCK */
    END.  /* IF btt_auth_detail.owning_entity_mnemonic = "hlmtl":U AND... */

    /*
      When a detail line is deleted to which a co-payment applies, the co-payment must be deleted automatically
    */
    IF   btt_auth_detail.record_action = "DELETE":U
    AND (btt_auth_detail.copay_auth   <> 0
    OR   btt_auth_detail.copay_auth_% <> 0) THEN
    DO:
      FIND FIRST btt_auth_copay
        WHERE btt_auth_copay.auth_obj               = btt_auth_detail.auth_obj
        AND   btt_auth_copay.owning_entity_mnemonic	= "hatad":U
        AND   btt_auth_copay.owning_obj             = btt_auth_detail.auth_detail_obj
        AND   btt_auth_copay.owning_key             = ""
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = FALSE }
      IF AVAILABLE btt_auth_copay
      THEN
        ASSIGN btt_auth_copay.record_action = "DELETE":U.
    END. /* IF btt_auth_detail.record_action = "DELETE" AND … */

    /*
      No point in continuing if an error occurred when the record was passed to the data access layer to be saved or deleted
    */
    IF goErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U, "ERR":U)
    THEN
      LEAVE.

    /*
      Automatically add or delete a Clinical Detail Line that belongs to
      a Provider Authorisation Group.
    */
    IF btt_auth_detail.record_action = "DELETE":U
    OR dAuthDetailObj <= 0 THEN // The line was added
    DO:
      ASSIGN cTrackingMessage = "AuthGroup - Auto create/delete auth group if"
                              + " btt_auth_detail.auth_group_obj("          + STRING(btt_auth_detail.auth_group_obj)      + ")<>0"
                              + " AND (btt_auth_detail.auth_group_obj("     + STRING(btt_auth_detail.auth_group_obj)      + ")="
                              + "btt_auth_provider.auth_group_obj("         + STRING(btt_auth_provider.auth_group_obj)    + ")"
                              + " AND btt_auth_detail._auth_group_updated(" + STRING(btt_auth_detail._auth_group_updated) + ")=no".

      { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

      IF  btt_auth_detail.auth_group_obj <> 0
      AND btt_auth_detail.auth_group_obj = btt_auth_provider.auth_group_obj
      AND btt_auth_detail._auth_group_updated = NO THEN
      DO:
        { ma/app/maauthbussaveauthgroup.i }

        /*
          We want to copy all the fields from the line that is being added
          to the line(s) that were auto-created
        */
        IF btt_auth_detail.record_action <> "DELETE":U
        THEN
          RUN _changeDetailLineValues IN TARGET-PROCEDURE ( BUFFER btt_auth_detail ).
      END.  // IF btt_auth_detail.auth_group_obj <> 0
    END.  // IF btt_auth_detail.record_action = "DELETE":U OR dAuthDetailObj <= 0 THEN

    IF  btt_auth_detail.record_action = "Modify":U
    AND NOT {&ErrorStatus} THEN
    DO:
      ASSIGN cTrackingMessage = "AuthGroup - Run _changeDetailLineValues for"
                              + " auth_detail_obj="            + STRING(btt_auth_detail.auth_detail_obj)
                              + "(OE="                         + btt_auth_detail.owning_alt_value         + ") if"
                              + " quantity_los("               + STRING(btt_auth_detail.quantity_los)     + ")=0"
                              + " AND auth_group_obj("         + STRING(btt_auth_detail.auth_group_obj)   + ")<>0"
                              + " AND (auth_group_obj("        + STRING(btt_auth_detail.auth_group_obj)   + ")="
                              + "provider.auth_group_obj("     + STRING(btt_auth_provider.auth_group_obj) + ")"
                              + " OR provider.auth_group_obj(" + STRING(btt_auth_provider.auth_group_obj) + ")=0)"
                              + " AND dAuthDetailObj("         + STRING(dAuthDetailObj)                   + ")>0"
                              + " AND _auth_group_updated("    + STRING(btt_auth_detail._auth_group_updated,"YES/NO") + ")=NO".

      { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

      IF  btt_auth_detail.quantity_los        = 0
      AND dAuthDetailObj                      > 0  // This line is being modified
      AND btt_auth_detail._auth_group_updated = NO // This line was not auto-created
      THEN DO:
        IF   btt_auth_detail.auth_group_obj  <> 0
        AND (btt_auth_detail.auth_group_obj   = btt_auth_provider.auth_group_obj
        OR   btt_auth_provider.auth_group_obj = 0)
        THEN
          RUN _changeDetailLineValues IN TARGET-PROCEDURE ( BUFFER btt_auth_detail ).
      END.  // IF btt_auth_detail.quantity_los = 0 AND...

      ASSIGN lSuccess =  mipEnv:miDBEntity:focusTable(btt_auth_detail.owning_entity_mnemonic)  NO-ERROR.

      {&ResetError}

      IF mipEnv:miDBEntity:InFocus
      THEN
        ASSIGN cOEMDescr = mipEnv:miDBEntity:TableDescription .

      ASSIGN lSuccess = mipEnv:miDBEntity:focusTable(btt_auth_detail.related_entity_mnemonic) NO-ERROR .

      {&ResetError}

      IF mipEnv:miDBEntity:InFocus
      THEN
        ASSIGN cREMDescr = mipEnv:miDBEntity:TableDescription .

      IF btt_auth_detail._claim_code_updated THEN
      DO:
        ASSIGN cText     = SUBSTITUTE("User updated the clinical detail claim code for &1 &2 and &3 &4 from default value &5 to value &6.",
                                      cOEMDescr,btt_auth_detail.owning_alt_value,cREMDescr,btt_auth_detail.related_value,
                                      btt_auth_detail.default_claim_code, btt_auth_detail.claim_code)

               cEventMsg = "field":U    + "=":U + "Claim Code":U                + "&":U
                         + "username"   + "=":U + mipEnv:miUser:UserName        + "&":U
                         + "userobj":U  + "=":U + STRING(mipEnv:miUser:UserObj) + "&":U
                         + "usercode":U + "=":U + mipEnv:miUser:UserCode        + "&":U
                         + "text":U     + "=":U + cText.

        IF dAuthDetailObj > 0 THEN
        DO:
          mipEnv:miUtility:createEvent
            (INPUT "maUserUpdate":U,     /* ipcEventTypeKey         AS CHARACTER  */
             INPUT "maUserUpdate":U,     /* ipcContext              AS CHARACTER  */
             INPUT cEventMsg,            /* ipcEventParameters      AS CHARACTER  */
             INPUT "hatad":U,            /* ipcOwningEntityMnemonic AS CHARACTER  */
             INPUT dAuthDetailObj,       /* ipdOwningObj            AS DECIMAL    */
             INPUT "":U).                /* ipcOwningKey            AS CHARACTER  */
        END. /* IF dAuthDetailObj > 0 THEN */
      END. /* IF btt_auth_detail._claim_code_updated THEN */

      IF btt_auth_detail._claim_type_updated THEN
      DO:
        ASSIGN cText     = SUBSTITUTE("User updated the clinical detail claim type for &1 &2 and &3 &4 from default value &5 to value &6.",
                                      cOEMDescr,btt_auth_detail.owning_alt_value,cREMDescr,btt_auth_detail.related_value,
                                      btt_auth_detail.default_claim_type, btt_auth_detail.claim_type)

               cEventMsg = "field":U    + "=":U + "Claim Type":U                + "&":U
                         + "username"   + "=":U + mipEnv:miUser:UserName        + "&":U
                         + "userobj":U  + "=":U + STRING(mipEnv:miUser:UserObj) + "&":U
                         + "usercode":U + "=":U + mipEnv:miUser:UserCode        + "&":U
                         + "text":U     + "=":U + cText.

        IF dAuthDetailObj > 0
        THEN
          mipEnv:miUtility:createEvent
            (INPUT "maUserUpdate":U,     /* ipcEventTypeKey         AS CHARACTER  */
             INPUT "maUserUpdate":U,     /* ipcContext              AS CHARACTER  */
             INPUT cEventMsg,            /* ipcEventParameters      AS CHARACTER  */
             INPUT "hatad":U,            /* ipcOwningEntityMnemonic AS CHARACTER  */
             INPUT dAuthDetailObj,       /* ipdOwningObj            AS DECIMAL    */
             INPUT "":U).                /* ipcOwningKey            AS CHARACTER  */

      END. /* IF btt_auth_detail._claim_type_updated THEN */
    END. /* IF btt_auth_detail.record_action = "Modify":U THEN */

    /*
      If we remove the auth group from a detail line and the other lines in the
      auth group are not auto-deleted, we need to make sure that the item-cost
      on the other lines are correct. We do this by changing the record action
      on the first line to 'modify', which will cause the first line to be 
      validated again and the item-cost will be automatically changed if necessary.
    */
    IF  btt_auth_detail.quantity_los        = 0 
    AND dAuthGroupObj                       > 0 
    AND btt_auth_provider.auth_group_obj    = 0
    AND ((btt_auth_detail.record_action = "MODIFY":U AND btt_auth_detail.auth_group_obj = 0 AND btt_auth_detail._auth_group_updated = NO)
    OR    btt_auth_detail.record_action = "DELETE":U) THEN
    DO:
      /*
        We don't have to include the auth_detail_obj in this where clause, because:
        1. If the btt_auth_detail.record_action = "modify", then the btt_auth_detail.auth_group_obj = ''
        2. Otherwise the btt_auth_detail.record_action = "delete".
      */
      IF NOT CAN-FIND(FIRST ctt_auth_detail
        WHERE ctt_auth_detail.auth_group_obj = dAuthGroupObj
        AND   ctt_auth_detail.record_action  = "MODIFY":U) THEN
      DO:
        FIND FIRST ctt_auth_detail
          WHERE ctt_auth_detail.auth_group_obj = dAuthGroupObj
          AND   ctt_auth_detail.record_action <> "DELETE":U
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE ctt_auth_detail
        THEN
          ASSIGN ctt_auth_detail.record_action = "MODIFY":U.
      END.  // IF NOT CAN-FIND(FIRST ctt_auth_detail
    END.  // ELSE IF btt_auth_detail.auth_group_obj = 0 AND dAuthGroupObj <> 0 THEN

    /*
      Here we going to add warning messages for details line with protocols linked to them
    */
    IF btt_auth_detail.record_action <> "DELETE":U THEN
    DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT 0.00,
                                                     INPUT 0,
                                                     INPUT "ma_acAuthRuleTypeAuthDetail":U,
                                                     INPUT "ActivateProtocolWarning":U,
                                                     INPUT goAuthorisation:StartDate,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue
                                                     ).
      IF lValidRule AND LOOKUP(cRuleValue,"Warn,WarnAck":U) > 0 THEN
      DO:
        ASSIGN
          lSuccess = oProtocolSearch:SetFilterCriteria("tt_clinical_docs.document_type":U , "=":U, "ma_acDocTypeProtocol":U)
          lSuccess = oProtocolSearch:SetFilterCriteria("tt_clinical_docs.insurer_obj":U   , "=":U, goAuthorisation:InsurerObj) WHEN goAuthorisation:InsurerObj <> 0.00
          lSuccess = oProtocolSearch:SetFilterCriteria("tt_clinical_docs.insurer_obj":U   , "=":U, 0.00)
          lSuccess = oProtocolSearch:SetFilterCriteria("tt_clinical_docs.option_code":U   , "=":U, goAuthorisation:OptionCode) WHEN goAuthorisation:OptionCode <> 0
          lSuccess = oProtocolSearch:SetFilterCriteria("tt_clinical_docs.option_code":U   , "=":U, 0)

          lSuccess = oProtocolSearch:setFilterCriteria("tt_clinical_docs.owning_entity_mnemonic":U,  "=":U, btt_auth_detail.owning_entity_mnemonic) WHEN btt_auth_detail.owning_entity_mnemonic <> "":U
          lSuccess = oProtocolSearch:setFilterCriteria("tt_clinical_docs.owning_obj":U            ,  "=":U, btt_auth_detail.owning_obj)             WHEN btt_auth_detail.owning_obj <> 0.00
          lSuccess = oProtocolSearch:setFilterCriteria("tt_clinical_docs.owning_key":U            ,  "=":U, btt_auth_detail.owning_key)             WHEN btt_auth_detail.owning_key <> "":U
          lSuccess = oProtocolSearch:setFilterCriteria("tt_clinical_docs.effective_date":U        , "<=":U, btt_auth_detail.start_date)             WHEN btt_auth_detail.start_date <> ?
          lSuccess = oProtocolSearch:setFilterCriteria("tt_clinical_docs.end_date":U              , ">=":U, btt_auth_detail.end_date)               WHEN btt_auth_detail.end_date   <> ?
          lSuccess = oProtocolSearch:setFilterCriteria("tt_clinical_docs.end_date":U              ,  "=":U, "?":U).

        oProtocolSearch:fetchData().

        IF CAN-FIND(FIRST tt_clinical_docs) THEN
        DO:
          ASSIGN lAcknowledge = (IF cRuleValue = "WARNACK":U THEN TRUE ELSE FALSE).

          goErrorObject:addError
                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,
                          INPUT btt_auth_detail.auth_detail_obj,
                          INPUT "":U,
                          INPUT "":U,
                          INPUT btt_auth_detail.line_number,
                          INPUT "Protocol exist for Clinical detail line. [HELP=Auth Rule Code: ActivateProtocolWarning]",
                          INPUT "WAR":U,
                          INPUT lAcknowledge).

        END. /* IF CAN-FIND(FIRST tt_clinical_docs ) THEN */

        ASSIGN lSuccess = oProtocolSearch:reset(TRUE). //resets filter criteria

      END. /* IF lValidRule AND LOOKUP(cRuleValue,"Warn,WarnAck:U") > 0 THEN */
    END. /* IF btt_auth_detail.record_action <> "DELETE":U  THEN */

    /*
      If the record was flagged to be deleted and there are no validation errors , delete the temp table record now
    */
    IF NOT goErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U, "ERR":U)
    AND btt_auth_detail.record_action = "DELETE":U
    THEN
      DELETE btt_auth_detail.
    ELSE
      /*
        Clear record action
      */
      ASSIGN btt_auth_detail.record_action = "":U .

    VALIDATE btt_auth_detail.
  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_detail.record_action) THEN DO:*/

&ENDIF

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oTFSearch)       THEN DELETE OBJECT oTFSearch.
                IF VALID-OBJECT(oProtocolSearch) THEN DELETE OBJECT oProtocolSearch.
                DATASET dsTariff:EMPTY-DATASET()."}




