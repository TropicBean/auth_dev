 /* maauthservcalcauthlosdetails.i MEDSTAR Medical Aid System
                            Healthcare Auth data access service: calcAuthLOSDetails                              
                            (c) Copyright 2018 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
------------------------------------------------------------------------------
  Purpose   : Calculate Authorisation LOS details
  Notes     : This routine will accept as input parameters, the main provider
              ars rate,the auth start date as well as an authorisation dataset.
              The LOS information will be updated on the detail temp table
              in the authorisation dataset accordingly, the final discharge
              date and the total LOS will be returned.

              * MMP-399 ( RTB49002 )
                Developed

              * MMP-423 ( RTB49582 )
                Bug fix for multiple calculations being applicable
                across all detail lines. Changes made to cater for
                additional input dates which will be the end date
                of the previous line in the sequence if applicable.
                eg. Dependant moves between wards from general Ward
                    to maternity ward.

  Author    : Andrewd
------------------------------------------------------------------------------*/
 
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

  DEFINE VARIABLE cAlertMessage                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cArsRate                     AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cCalcRuleErrorValue          AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cDefaultLineRestrictionValue AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cEntry                       AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cErrorMessage                AS CHARACTER             NO-UNDO. 
  DEFINE VARIABLE cLOSQtyAdjustedRuleValue     AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cMessage                     AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cProcedure                   AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cRuleCode                    AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cRuleList                    AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cRuleValue                   AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTariffCode                  AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning                     AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWeekendPassStatusReason     AS CHARACTER             NO-UNDO. 
  DEFINE VARIABLE cTrackingMessage             AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE lAcknowledge                 AS LOGICAL               NO-UNDO. 
  DEFINE VARIABLE lCalcValidationError         AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lEndTime                     AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lTotalEndTime                AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lSuccess                     AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lValidCalcRuleError          AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lValidDefaultLineRestriction AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lValidRuleLOSQtyAdjusted     AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lValidRule                   AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lWeekendPass                 AS LOGICAL               NO-UNDO. 
  DEFINE VARIABLE lWeekendPassDeclineLOC       AS LOGICAL               NO-UNDO. 
  DEFINE VARIABLE lRunLosCalc                  AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE iCalcTime                    AS INTEGER               NO-UNDO.
  DEFINE VARIABLE iSequence                    AS INTEGER               NO-UNDO.
  DEFINE VARIABLE iRule                        AS INTEGER               NO-UNDO.
  DEFINE VARIABLE dEndDate                     AS DATE                  NO-UNDO.
  DEFINE VARIABLE dTariffObj                   AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj               AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj                  AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTotalEndDate                AS DATE                  NO-UNDO.
  DEFINE VARIABLE dLOS                         AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dQuantityLOS                 AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTotalLos                    AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE oTariffType                  AS cls.matarifftype      NO-UNDO.
  DEFINE VARIABLE oErrorObject                 AS cls.maerrorobject     NO-UNDO.
  DEFINE VARIABLE oTFSearch                    AS ma.cls.matariffsearch NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER btt_auth            FOR tt_auth.
  DEFINE BUFFER btt_auth_detail     FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_provider   FOR tt_auth_provider.
  DEFINE BUFFER buf_auth_detail     FOR hat_auth_detail.

  ASSIGN
     oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE)
     oTFSearch    = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE)
     oTariffType  = NEW cls.matarifftype().


  /* List of rules to check to determine which calculation we should be running.*/
  /* The rule code is mapped to the corresponding internal procedure which will */
  /* be run for the relevant calculation.                                       */
  ASSIGN
      cRuleList = "CalcDay=calcLOSDayCase,":U
                + "CalcFixedFeePerDiem=calcLOSPerDiemFixedFee,":U
                + "CalcMaternityPerDiem=calcLOSPerDiemMat,":U
                + "CalcPerDiem=calcLOSPerDiem,":U
                + "CalcPVTPerDiem=calcLOSPerDiemPVT,":U
                + "CalcDefault=calcLOSFeeForService":U

      cMessage  = "LOS/LOC could not be calculated for [ Ars Rate:[ARSRATE] LOC:[LOC] ], because ARS & LOC rule set up is missing ":U +
                  "for tariff [TARIFF].":U.

  FOR EACH btt_auth NO-LOCK:

    EMPTY TEMP-TABLE ttRule.

    ASSIGN
       iSequence     = 0

       dTotalLos     = 0
       dLOS          = 0
       dQuantityLOS  = 0 
           
       dTotalEndDate = ?
       dEndDate      = ?

       lTotalEndTime = ?
       lEndTime      = ?
       
       lCalcValidationError = FALSE
       /*
         If we are deleting a LOC record, we need to recalculate
       */
       lRunLosCalc   = (CAN-FIND(FIRST btt_auth_detail NO-LOCK
                                 WHERE btt_auth_detail.auth_obj               = btt_auth.auth_obj
                                   AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
                                   AND btt_auth_detail.quantity_los          <> 0.00
                                   AND btt_auth_detail.record_action          = "DELETE":U)).

    FOR EACH btt_auth_detail EXCLUSIVE-LOCK
       WHERE btt_auth_detail.auth_obj               = btt_auth.auth_obj
         AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
         AND btt_auth_detail.quantity_los          <> 0.00
         AND btt_auth_detail.record_action          = "MODIFY":U
          BY btt_auth_detail.loc_sequence:

      FIND FIRST buf_auth_detail NO-LOCK
           WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      ASSIGN
         btt_auth_detail.loc_sequence = 999 WHEN btt_auth_detail.loc_sequence <= 0

         /*
           Recalculate if new LOC record is added or LOC fields of existing record have been modified
         */
         lRunLosCalc = lRunLosCalc                   OR
                       NOT AVAILABLE buf_auth_detail OR
                          (AVAILABLE buf_auth_detail AND
                                    (buf_auth_detail.owning_obj       <> btt_auth_detail.owning_obj   OR
                                     buf_auth_detail.owning_key       <> btt_auth_detail.owning_key   OR
                                     buf_auth_detail.start_date       <> btt_auth_detail.start_date   OR
                                     buf_auth_detail.start_ampm       <> btt_auth_detail.start_ampm   OR
                                     buf_auth_detail.quantity_los     <> btt_auth_detail.quantity_los OR
                                     buf_auth_detail.loc_sequence     <> btt_auth_detail.loc_sequence OR
                                     buf_auth_detail.auth_status      <> btt_auth_detail.auth_status  OR
                                     buf_auth_detail.auth_status_note <> btt_auth_detail.auth_status_note)).

      /*
        If we a sequence change to an existing record or we have created a new line
        we need to check for conflicting line sequence numbers below
      */
      IF NOT AVAILABLE buf_auth_detail OR (AVAILABLE buf_auth_detail AND btt_auth_detail.loc_sequence <> buf_auth_detail.loc_sequence) THEN
      DO:
        ASSIGN iCalcTime = ETIME.

        RUN _updateAuthDetailLOCSequence IN TARGET-PROCEDURE (INPUT btt_auth.auth_obj,
                                                              BUFFER btt_auth_detail).
      END. /*IF NOT AVAILABLE buf_auth_detail OR (AVAILABLE buf_auth_detail AND btt_auth_detail.loc_sequence <> buf_auth_detail.loc_sequence) THEN*/
    END. /*FOR EACH btt_auth_detail EXCLUSIVE-LOCK*/

    cTrackingMessage = "System LOS Calc - Run LOS Calc? " + STRING(lRunLosCalc).

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
    
    /*
      - Ensure that record actions are set accordingly
      - Ensure that we have no gaps in our line sequence numbers
    */
    IF lRunLosCalc THEN
    DO:
      RUN _realignAuthDetailLOCSequence IN TARGET-PROCEDURE ( INPUT btt_auth.auth_obj,
                                                              BUFFER btt_auth_detail ).
      
      /* Prepare the rules we are interested in for the current insurer, option code etc. */
      Prepare-rule-blk:
      DO iRule = 1 TO NUM-ENTRIES(cRuleList):

        ASSIGN
           cEntry     = ENTRY(iRule, cRuleList)
           cRuleCode  = ENTRY(1, cEntry, "=":U)
           cProcedure = ENTRY(2, cEntry, "=":U).

        /* Get rule info for the current code */
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                       INPUT  cRuleCode,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).

        IF lValidRule THEN
        DO:
          CREATE ttRule.
          ASSIGN
             ttRule.RuleCode      = cRuleCode
             ttRule.RuleProcedure = "_":U + cProcedure
             ttRule.RuleValue     = cRuleValue.

          VALIDATE ttRule.
        END. /*IF lValidRule THEN*/
      END. /*DO iRule = 1 TO NUM-ENTRIES(cRuleList):*/

      ASSIGN
         lSuccess = oTFSearch:removeCriteria("*":U, "*":U)
         lSuccess = oTFSearch:setCriteria("BufferList":U, "tt_tariff_link,tt_tariff,tt_taiff_type_link":U).

      /* Populate line restriction */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                     INPUT  "DefaultLineRestriction":U,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidDefaultLineRestriction,
                                                     OUTPUT cDefaultLineRestrictionValue).
                                                     
      /* Get rule info for the LOS Quantity Adjusted */
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                     INPUT  "LOSQtyAdjusted",
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRuleLOSQtyAdjusted,
                                                     OUTPUT cLOSQtyAdjustedRuleValue).
      
      
      /* Certain lines may run different calculations so we will run through each line */
      /* and create a mapping between the line and the calculation to be run.          */
      Determine-calculation-blk:
      FOR EACH  btt_auth_detail EXCLUSIVE-LOCK
         WHERE  btt_auth_detail.auth_obj               = btt_auth.auth_obj
           AND  btt_auth_detail.owning_entity_mnemonic = "htmtl":U
           AND  btt_auth_detail.quantity_los          <> 0.00
           AND (btt_auth_detail.record_action          = "MODIFY":U
            OR  btt_auth_detail.record_action          = "DELETE":U),
         FIRST  btt_auth_provider NO-LOCK
         WHERE  btt_auth_provider.auth_provider_obj    = btt_auth_detail.auth_provider_obj
            BY  btt_auth_detail.loc_sequence:
 
        ASSIGN cArsRate = IF btt_auth_provider.override_ars_rate <> "":U
                          THEN btt_auth_provider.override_ars_rate
                          ELSE btt_auth_provider.default_ars_rate.

        IF lValidDefaultLineRestriction 
        AND btt_auth_detail.line_restriction <> "ma_acAuthLineRestrictionAmt&Qty":U
        THEN
          ASSIGN btt_auth_detail.line_restriction = cDefaultLineRestrictionValue.

        IF btt_auth_detail.record_action = "MODIFY":U THEN
        DO:

          /* We will temporarily use record action to store the calculation mapping per line */
          ASSIGN btt_auth_detail.record_action = "":U.
                 
          /* Fetch tariff link and tariff type link data required */
          DATASET dsTariff:EMPTY-DATASET().

          ASSIGN
             lSuccess = oTFSearch:removeCriteria(INPUT "Query-Filter":U, INPUT "tt_tariff_link":U) /* We are running in an iterative block so remove the filters applied from the previous iteration */

             lSuccess = oTFSearch:setFilterCriteria("tt_tariff_link.tariff_link_obj":U, "=":U, btt_auth_detail.owning_obj).

          /* And fetch tariff link and tariff type link data */
          oTFSearch:fetchTariffData().

          /* Populate the LOC tariff type obj if not populated */
          IF btt_auth_detail.loc_tariff_type_obj = 0.00 THEN
          DO:

            FIND FIRST tt_tariff_type_link NO-LOCK NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE tt_tariff_type_link
            THEN
              ASSIGN btt_auth_detail.loc_tariff_type_obj = tt_tariff_type_link.tariff_type_obj.

          END. /*IF btt_auth_detail.loc_tariff_type_obj = 0.00 THEN*/

          /* Populate loc_value/tariff type code if not populated */
          IF btt_auth_detail.loc_value = "":U AND btt_auth_detail.loc_tariff_type_obj <> 0.00 THEN
          DO:

             /* Find tariff type required to populate loc value if not populated */
             oTariffType:focusTariffType(INPUT btt_auth_detail.loc_tariff_type_obj).

             /* Assign tariff type if the tariff type is of type LOC */
             IF oTariffType:TariffTypeInFocus 
             AND oTariffType:TariffTypeKey = "ma_acTariffTypeCatLOC":U THEN
             DO:
               ASSIGN btt_auth_detail.loc_value = oTariffType:TariffTypeCode
                      cTrackingMessage = "Calc Auth LOS Details: LOC Tariff Type - " + oTariffType:TariffTypeCode +
                                         " (AuthObj=" + STRING(btt_auth.auth_obj) + ")".

               {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

             END.  /* IF oTariffType:TariffTypeInFocus AND... */

          END. /*IF btt_auth_detail.loc_value = "":U AND btt_auth_detail.loc_tariff_type_obj <> 0.00 THEN*/

          /* Find appropriate rule which will tell us which calculation to run for this line,   */
          /* the relevant procedure will be assigned to the record action field temporarily     */
          /* which we will then use later to run the appropriate calculation against each line. */
          Find-rule-blk:
          FOR EACH ttRule EXCLUSIVE-LOCK:
            /*  Check rule valid and that the ars rate is listed in the rule */
            IF LOOKUP(cArsRate + ",":U + btt_auth_detail.loc_value,TRIM(ttRule.RuleValue),"|") > 0 THEN
            DO:
              ASSIGN btt_auth_detail.record_action        = ttRule.RuleProcedure
                     btt_auth_detail.los_calculation_rule = ttRule.RuleProcedure
                     cTrackingMessage = "Calc Auth LOS Details: Rule picked up for calc - " + ttRule.RuleProcedure +
                                        " (AuthObj=" + STRING(btt_auth.auth_obj) + " cArsRate=" + cArsRate + ")".

               {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

              LEAVE Find-rule-blk.
            END. /*IF LOOKUP(cArsRate + ",":U + btt_auth_detail.loc_value, TRIM(ttRule.RuleValue), "|":U) > 0 THEN*/
          END. /*FOR EACH tt_rule NO-LOCK*/

          /* If we couldnt find a calculation procedure to run check if we should use default */
          IF btt_auth_detail.record_action = "":U THEN
          DO:
            FIND FIRST ttRule NO-LOCK
                 WHERE ttRule.RuleCode = "CalcDefault":U
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE ttRule AND LOOKUP(cArsRate, TRIM(ttRule.RuleValue), "|":U) > 0 THEN
            DO:
              ASSIGN btt_auth_detail.record_action        = ttRule.RuleProcedure
                     btt_auth_detail.los_calculation_rule = ttRule.RuleProcedure
                     cTrackingMessage = "Calc Auth LOS Details: LOS Rule not found - Using Default Calc rule: " + ttRule.RuleProcedure +
                                        " (AuthObj=" + STRING(btt_auth.auth_obj) + " cArsRate=" + cArsRate + ")".

               {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
            END.  /* IF AVAILABLE ttRule AND LOOKUP(cArsRate, TRIM(ttRule.RuleValue), "|":U) > 0 THEN */
          END. /*IF btt_auth_detail.record_action = "":U THEN*/

          /* Calculation could not be determined */
          IF btt_auth_detail.record_action = "":U THEN
          DO:
            /* Get rule which will determine how we should handle the */
            /* fact that we could not identify a calculation routine. */
            mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                           INPUT  btt_auth.option_code,
                                                           INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                           INPUT  "CalcRuleError":U,
                                                           INPUT  btt_auth.start_date,
                                                           OUTPUT lValidCalcRuleError,
                                                           OUTPUT cCalcRuleErrorValue).

            /*  Activate how a missing ARS&LOC set up must be handled.                                                                           */
            /*  Rule value may contain 3 entries - Action|Defaul Status|Status Reason.                                                           */
            /*  First entry will indicate the actions - what must happen - Block or Load:                                                        */
            /*  Block - Will generate an error message that LOS couldn't be calculated because ARS & LOC rule set ups are missing.               */
            /*  Load  - Authorisation detail line will be loaded but with a default status and status reason e.g. 0|100 where 0 = status pending */
            /*          and 100 will be the status reason which will indicate that the ARS&LOC set ups are missing for the LOS calculation.      */

            ASSIGN cTrackingMessage = "Calc Auth LOS Details: Calc Rule Error? " + STRING(lValidCalcRuleError) + " Rule Value=" + cCalcRuleErrorValue +
                                      " (AuthObj=" + STRING(btt_auth.auth_obj) + " cArsRate=" + cArsRate + ")".

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            /* Block - Add error message */
            IF lValidCalcRuleError AND TRIM(ENTRY(1, cCalcRuleErrorValue, "|":U)) = "BLOCK":U THEN
            DO:
              ASSIGN cErrorMessage = REPLACE(REPLACE(REPLACE(cMessage, "[LOC]":U, btt_auth_detail.loc_value),
                                                                       "[ARSRATE]":U, cArsRate),
                                                                       "[TARIFF]":U, btt_auth_detail.owning_alt_value)
                     cErrorMessage = cErrorMessage + ".[HELP=Auth Rule Code: CalcRuleError]".
              
	            oErrorObject:addError (INPUT "LOS":U,
                                     INPUT btt_auth_detail.auth_detail_obj,
                                     INPUT "":U,
                                     INPUT "owning_alt_value":U,
                                     INPUT btt_auth_detail.line_number,
                                     INPUT cErrorMessage,
                                     INPUT "ERR":U).
            END. /*IF lValidCalcRuleError AND TRIM(ENTRY(1, cCalcRuleErrorValue)) = "BLOCK":U THEN*/

            /* Load - Default status,status reason */
            IF lValidCalcRuleError AND TRIM(ENTRY(1, cCalcRuleErrorValue, "|":U)) = "LOAD":U THEN
            DO:

              /* Make sure the rule is properly formed, in other words we should not do anything */
              /* if the rule is not configured properly with a status and status reason.         */
              IF NUM-ENTRIES(cCalcRuleErrorValue, "|":U) = 3  AND
                 ENTRY(2, cCalcRuleErrorValue, "|":U) <> "":U AND
                 ENTRY(3, cCalcRuleErrorValue, "|":U) <> "":U
              THEN
              DO:
                /*  Default the status,status reason */

                ASSIGN
                   btt_auth_detail.record_action    = "MODIFY":U
                   btt_auth_detail.auth_status      = INTEGER(ENTRY(2, cCalcRuleErrorValue, "|":U))
                   btt_auth_detail.auth_status_note =         ENTRY(3, cCalcRuleErrorValue, "|":U)
                   btt_auth_detail.start_date       = btt_auth_provider.start_date
                   btt_auth_detail.start_ampm       = btt_auth_provider.start_ampm
                   btt_auth_detail.end_date         = btt_auth_provider.start_date
                   btt_auth_detail.end_ampm         = btt_auth_provider.start_ampm
                   btt_auth_detail.quantity_auth    = 0
                   btt_auth_detail.quantity_los     = 0.

                VALIDATE btt_auth_detail.
              END. /* IF NUM-ENTRIES(cRuleValue__CalcRuleError, "|":U) = 3  AND */
            END. /* IF lValidCalcRuleError AND TRIM(ENTRY(1, cCalcRuleErrorValue)) = "BLOCK":U THEN*/

            /* If we dont have a rule indicating how we should handle the fact that we could not determine  */
            /* a calculation routine to run then all we can do at this point is raise an error.             */
            IF NOT lValidCalcRuleError OR (lValidCalcRuleError AND TRIM(cCalcRuleErrorValue) = "":U) THEN
            DO:
              ASSIGN cErrorMessage = REPLACE(REPLACE(REPLACE(cMessage, "[LOC]":U, btt_auth_detail.loc_value),
                                                                       "[ARSRATE]":U, cArsRate),
                                                                       "[TARIFF]":U, btt_auth_detail.owning_alt_value)
                     cErrorMessage = cErrorMessage + ".[HELP=Auth Rule Code: CalcRuleError]".

              oErrorObject:addError (INPUT "LOS":U,
                                     INPUT btt_auth_detail.auth_detail_obj,
                                     INPUT "":U,
                                     INPUT "owning_alt_value":U,
                                     INPUT btt_auth_detail.line_number,
                                     INPUT cErrorMessage,
                                     INPUT "ERR":U).
            END. /* IF NOT lValidCalcRuleError OR (lValidCalcRuleError AND TRIM(cCalcRuleErrorValue) = "":U) THEN */
          END. /* IF btt_auth_detail.record_action = "":U THEN */

          /*
            Ok lets run the relevant calculation against the detail line
          */
          IF btt_auth_detail.record_action <> "":U AND btt_auth_detail.record_action <> "MODIFY":U THEN
          DO:
            DATASET dsTariff:EMPTY-DATASET().

            /*
              Fetch tariff data required for tariff cost/quantity and amount auth
            */
            ASSIGN 
               lSuccess = oTFSearch:removeCriteria("*":U, "*":U)
               lSuccess = oTFSearch:setCriteria("BufferList":U, "tt_tariff_link":U)
               lSuccess = oTFSearch:setFilterCriteria("tt_tariff_link.tariff_link_obj":U, "=":U, btt_auth_detail.owning_obj).
            
            oTFSearch:fetchTariffData().
               
            FIND FIRST tt_tariff_link NO-LOCK NO-ERROR.
            
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
            
            IF AVAILABLE tt_tariff_link 
            THEN ASSIGN dTariffLinkObj = tt_tariff_link.tariff_link_obj
                        cTariffCode    = tt_tariff_link.tariff_code.
            ELSE
              ASSIGN dTariffLinkObj = btt_auth_detail.owning_obj
                     cTariffCode    = btt_auth_detail.owning_alt_value.
                                      
            mipEnv:Health:maMedical:getValidTariff(INPUT-OUTPUT dTariffLinkObj,                 /*  iopdTariffLinkObj */
                                                   INPUT        cTariffCode,                    /*  ipcTariffCode     */
                                                   INPUT        "",                             /*  ipcBaseRate       */
                                                   INPUT        "",                             /*  ipcARSRate        */
                                                   INPUT        0,                              /*  ipiPrType         */
                                                   INPUT        0,                              /*  ipiSubPrType      */
                                                   INPUT        btt_auth_detail.start_date,     /*  ipdDate           */
                                                   INPUT        btt_auth.option_code ,          /*  ipiOptionCode     */
                                                   INPUT        "",                             /*  ipcAddValidations */
                                                   OUTPUT       dTariffObj,                     /*  opdTariffObj      */
                                                   OUTPUT       dTrfCostObj,                    /*  opdTrfCostObj     */
                                                   OUTPUT       cErrorMessage,                  /*  opcError          */
                                                   OUTPUT       cWarning,                       /*  opcWarning        */
                                                   OUTPUT       cAlertMessage).                 /*  opcAlertMessage   */
            
            IF dTrfCostObj <> 0 THEN
            DO:
              FIND trfcost NO-LOCK
                   WHERE trfcost.trfcost-obj = dTrfCostObj
                NO-ERROR.
            
              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
            
            END.  /* IF dTrfCostObj <> 0 THEN */
 
            IF AVAILABLE trfcost 
            AND btt_auth_detail.line_restriction <> "ma_acAuthLineRestrictionAmt&Qty":U
            THEN
              ASSIGN btt_auth_detail.item_cost       = trfcost.amount
                     btt_auth_detail.fixed_item_cost = trfcost.fixed-amount.
            
            ASSIGN iCalcTime        = ETIME
                   dQuantityLOS     = btt_auth_detail.quantity_los 
                   cTrackingMessage = "Calc Auth LOS Details: BEFORE Run Calculation ("                    + STRING(btt_auth_detail.record_action) + ") - " +
                                      " Provider Start Date&Time=" + STRING(btt_auth_provider.start_date)  + STRING(btt_auth_provider.start_ampm,"AM/PM") + 
                                      " Total End Date&Time="      + (IF dTotalEndDate = ? THEN "" ELSE STRING(dTotalEndDate)) + (IF lTotalEndTime = ? THEN "" ELSE STRING(lTotalEndTime,"AM/PM")) + 
                                      " (AuthObj=" + STRING(btt_auth.auth_obj) + " cArsRate=" + cArsRate + ")".

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
            
            RUN VALUE ( btt_auth_detail.record_action ) IN TARGET-PROCEDURE ( BUFFER btt_auth_detail,
                                                                              INPUT  btt_auth.insurer_obj,
                                                                              INPUT  btt_auth.option_code,
                                                                              INPUT  btt_auth_provider.start_date,   /* Provider start date                  */
                                                                              INPUT  btt_auth_provider.start_ampm,   /* Provider start time                  */
                                                                              INPUT  dTotalEndDate,                  /* Calculated end date up to this point */
                                                                              INPUT  lTotalEndTime,                  /* Calculated end time up to this point */
                                                                              OUTPUT dEndDate,
                                                                              OUTPUT lEndTime,
                                                                              OUTPUT dLOS ) NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6456' &ResetIgnoredErrors = TRUE }
            
            ASSIGN lCalcValidationError = oErrorObject:canFind("LOS":U, btt_auth_detail.auth_detail_obj, "":U, "ERR":U).  

            /* 
              We might have raised a validation error in the calc procedure. 
              If so, do not check quantity adjusted if an error was raised
            */
            IF NOT lCalcValidationError 
            THEN DO:
              IF dQuantityLOS <> btt_auth_detail.quantity_los AND lValidRuleLOSQtyAdjusted AND CAN-DO("Warn,WarnAck",cLOSQtyAdjustedRuleValue)
              THEN DO:
                ASSIGN lAcknowledge  = IF cLOSQtyAdjustedRuleValue = "WarnAck":U
                                       THEN TRUE
                                       ELSE FALSE
                       cErrorMessage = "LOS quantity is adjusted from " + STRING(dQuantityLOS) + " to " + STRING(btt_auth_detail.quantity_los) + " according to the " + btt_auth_detail.record_action + " calculation".           
                       lSuccess      = oErrorObject:addError
                                                    (INPUT "hatad:" + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                                     INPUT btt_auth_detail.auth_detail_obj,                   /* ipdOwningEntityObj       */
                                                     INPUT "":U,                                              /* ipcOwningEntityKey       */
                                                     INPUT btt_auth_detail.line_number,                       /* ipiLineNumber            */
                                                     INPUT cErrorMessage,                                     /* ipcMessageText           */
                                                     INPUT "WAR":U,                                           /* ipcMessageType           */
                                                     INPUT lAcknowledge).                                     /* iplAcknowledge           */  
              
              END. /* IF dQuantityLOS <> dLOS AND */
              
              ASSIGN cTrackingMessage = "Calc Auth LOS Details: AFTER Run Calculation ("                     + STRING(btt_auth_detail.record_action) + ") - " +
                                        " Provider Start Date&Time=" + STRING(btt_auth_provider.start_date)  + STRING(btt_auth_provider.start_ampm,"AM/PM") + 
                                        " Total End Date&Time="      + (IF dTotalEndDate = ? THEN "" ELSE STRING(dTotalEndDate)) + (IF lTotalEndTime = ? THEN "" ELSE STRING(lTotalEndTime,"AM/PM")) + 
                                        " Calculated End Date&Time=" + (IF dEndDate = ?      THEN "" ELSE STRING(dEndDate))      + (IF lEndTime = ?      THEN "" ELSE STRING(lEndTime,"AM/PM")) + 
                                        " (AuthObj="   + STRING(btt_auth.auth_obj) + " cArsRate=" + cArsRate + 
                                        ") (Duration=" + STRING(ETIME - iCalcTime) + " milliseconds)".
  
              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
  
              ASSIGN
                 btt_auth_provider.record_action = "MODIFY":U
                 btt_auth_provider.end_date      = dEndDate
                 btt_auth_provider.end_ampm      = lEndTime.
  
              VALIDATE btt_auth_provider.
            END. /* IF NOT lCalcValidationError  */ 
          END. /*IF btt_auth_detail.record_action <> "":U THEN*/
          
          /* 
            Check for Weekend Pass 
          */
          IF NOT lCalcValidationError
          THEN DO:
            FIND FIRST schext NO-LOCK
                 WHERE schext.scheme-code = btt_auth.option_code NO-ERROR. 

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            mipEnv:Health:AuthService:checkWeekendPass (INPUT  btt_auth.auth_type_obj,                 /* ipdAuthTypeObj         */
                                                        INPUT  btt_auth.insurer_obj,                   /* ipdInsurerObj          */
                                                        INPUT  btt_auth.option_code,                   /* ipiOptionCode          */
                                                        INPUT  btt_auth.start_date,                    /* ipdEffectiveDate       */
                                                        INPUT  btt_auth_detail.owning_alt_value,       /* ipcTariffCode          */
                                                        INPUT  btt_auth_detail.auth_status,            /* ipiAuthStatus          */
                                                        INPUT  btt_auth_detail.auth_status_note,       /* ipcAuthStatusReason    */
                                                        OUTPUT lWeekendPass,                           /* oplWeekendPass         */
                                                        OUTPUT lWeekendPassDeclineLOC,                 /* oplDeclineLOC          */
                                                        OUTPUT cWeekendPassStatusReason,               /* opcDeclineStatusReason */
                                                        OUTPUT cErrorMessage).                         /* opcError               */

            /* 
              Check for errors returned from Weekend Pass Check 
            */
            IF cErrorMessage <> ""
            THEN DO:
              oErrorObject:addError
                  (INPUT "LOS":U,
                   INPUT btt_auth_detail.auth_detail_obj,
                   INPUT "":U,
                   INPUT "owning_alt_value":U,
                   INPUT btt_auth_detail.line_number,
                   INPUT cErrorMessage,
                   INPUT "ERR":U).

              ASSIGN lCalcValidationError          = TRUE
                     btt_auth_detail.record_action = "":U
                     cTrackingMessage              = "checkWeekendPassError= ":U + cErrorMessage +
                                                     " (Auth Detail Obj=" + STRING(btt_auth_detail.auth_detail_obj) + ")" + 
                                                     " btt_auth.auth_type_obj="             + STRING(btt_auth.auth_type_obj) +
                                                     " btt_auth_detail.owning_alt_value="   + btt_auth_detail.owning_alt_value +
                                                     " btt_auth_detail.auth_status="        + STRING(btt_auth_detail.auth_status) +
                                                     " btt_auth_detail.auth_status_note="   + STRING(btt_auth_detail.auth_status_note). 
                                                                                                                               
              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
            END. /* IF cErrorMessage <> "" */

            /* 
              Weekend pass valid - Update detail lines
            */
            IF AVAILABLE schext AND lWeekendPass AND NOT lCalcValidationError
            THEN DO:     
              IF lWeekendPassDeclineLOC 
              THEN
                ASSIGN     
                  btt_auth_detail.record_action     = "MODIFY":U
                  btt_auth_detail.auth_status       = 6
                  btt_auth_detail.auth_status_note  = cWeekendPassStatusReason
                  btt_auth_detail.amount_auth       = 0
                  btt_auth_detail.quantity_auth     = 0
                  btt_auth_detail.add_to_total_los  = FALSE
                  btt_auth_detail.claim_code        = schext.claim-code[1]
                  dLOS                              = 0.
              
              IF NOT lWeekendPassDeclineLOC
              THEN 
                ASSIGN 
                  btt_auth_detail.record_action     = "MODIFY":U
                  btt_auth_detail.amount_auth       = 0
                  btt_auth_detail.quantity_auth     = 0
                  btt_auth_detail.add_to_total_los  = FALSE
                  btt_auth_detail.claim_code        = schext.claim-code[1]
                  dLOS                              = 0.

              ASSIGN cTrackingMessage = "Apply Weekend Pass: Decline LOC "     + STRING(lWeekendPassDeclineLOC) +
                                        " (Auth Detail Obj=" + STRING(btt_auth_detail.auth_detail_obj) + ")" + 
                                        " btt_auth.auth_type_obj="             + STRING(btt_auth.auth_type_obj) +
                                        " btt_auth_detail.owning_alt_value="   + btt_auth_detail.owning_alt_value +
                                        " btt_auth_detail.auth_status="        + STRING(btt_auth_detail.auth_status) +
                                        " btt_auth_detail.auth_status_note="   + STRING(btt_auth_detail.auth_status_note).
                  
              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
            END. /* IF AVAILABLE schext AND lWeekendPass */
          END. /* IF NOT lCalcValidationError */ 

          /* 
            We might have raised a validation error in the calc procedure. 
            If so, do not update Total End date and time
          */  
          IF NOT lCalcValidationError 
          THEN 
            ASSIGN
              dTotalEndDate  = (IF dEndDate <> ?
                                THEN dEndDate
                                ELSE dTotalEndDate)

              lTotalEndTime  = (IF dEndDate <> ?
                                THEN lEndTime
                                ELSE lTotalEndTime)

              dTotalLOS = dTotalLOS + dLOS.
        END. /*IF btt_auth_detail.record_action = "MODIFY":U THEN*/
      END. /* FOR EACH btt_auth_detail EXCLUSIVE-LOCK */

      /*
        We can only set the record action back to modify after processing all detail lines for the
        auth because in some scenarios during a calculation we need to count the amount of lines
        relevant to that particular calculation.
      */
      FOR EACH ttRule NO-LOCK,
          EACH btt_auth_detail EXCLUSIVE-LOCK
         WHERE btt_auth_detail.auth_obj = btt_auth.auth_obj
           AND btt_auth_detail.record_action = ttRule.RuleProcedure:

        ASSIGN btt_auth_detail.record_action = "MODIFY":U.

        VALIDATE btt_auth_detail.
      END. /*FOR EACH ttRule NO-LOCK,*/

      /* 
        We might have raised a validation error in the calc procedure. 
        If so, do not check update auth header 
      */                                
      IF NOT lCalcValidationError 
      THEN DO:
        EMPTY TEMP-TABLE ttAuthTypeConfig.
  
        /*
          Check the auth type to determine whether the auth is a period type auth or not and if not we will assign the
          calculated end date to the auth header as well ........
        */
        mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                                     BUFFER btt_auth_provider,
                                                     INPUT-OUTPUT TABLE ttAuthTypeConfig).
  
        FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
  
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
        IF AVAILABLE ttAuthTypeConfig AND (ttAuthTypeConfig.Period = 0 OR ttAuthTypeConfig.Period = ?) THEN
        DO:
          ASSIGN
             btt_auth.record_action = "MODIFY":U
             btt_auth.end_date      = dTotalEndDate
             btt_auth.end_ampm      = lTotalEndTime
             btt_auth.total_los     = dTotalLOS
             cTrackingMessage       = "Calc Auth LOS Details: Period Type Auth. Update Auth Header End Date/Time and Total LOS "    +
                                      " dTotalEndDate=" + (IF dTotalEndDate = ? THEN "" ELSE STRING(dTotalEndDate)) +
                                      " lTotalEndTime=" + (IF lTotalEndTime = ? THEN "" ELSE STRING(lTotalEndTime)) + 
                                      " dTotalLOS="     + STRING(dTotalLOS).
  
          VALIDATE btt_auth.
          
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END. /*IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.Period <> 0 THEN*/
      END. /* IF NOT lCalcValidationError  */        
    END. /* IF lRunLosCalc */
  END. /*FOR EACH btt_auth NO-LOCK:*/

&ENDIF

  /* And cleanup */
  { mip/inc/mipcatcherror.i
     &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject.
                 IF VALID-OBJECT(oTariffType)  THEN DELETE OBJECT oTariffType.
                 IF VALID-OBJECT(oTFSearch)    THEN DELETE OBJECT oTFSearch.

                 DATASET dsTariff:EMPTY-DATASET().

                 EMPTY TEMP-TABLE ttRule." }
