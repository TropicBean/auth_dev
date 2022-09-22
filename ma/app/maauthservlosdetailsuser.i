/* maauthservlosdetailsuser.i  MEDSTAR Medical Aid System
                               (c) Copyright 2020 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/          
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE BUFFER btt_auth                 FOR tt_auth.
  DEFINE BUFFER buf_auth                 FOR hat_auth.
  DEFINE BUFFER btt_auth_detail          FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_provider        FOR tt_auth_provider.
  DEFINE BUFFER btt_last_auth_detail     FOR tt_auth_detail.
  DEFINE BUFFER btt_usercalc_auth_detail FOR tt_auth_detail.
  DEFINE BUFFER buf_auth_detail          FOR hat_auth_detail.

  DEFINE VARIABLE cAlertMessage            AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cArsRate                 AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cRuleValue               AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cRuleValuePerDiem        AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cRuleValueFixedFee       AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTrackingMessage         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTariffCode              AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cError                   AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning                 AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWeekendPassStatusReason AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE dTariffObj               AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj           AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj              AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTotalLOS                AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTotalEndDate            AS DATE                  NO-UNDO.
  DEFINE VARIABLE iCalcTime                AS INTEGER               NO-UNDO.
  DEFINE VARIABLE lCalcValidationError     AS LOGICAL               NO-UNDO. 
  DEFINE VARIABLE lFixedFeePerDiem         AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lPVTPerDiem              AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lRunLosCalc              AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lSuccess                 AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lTotalEndTime            AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lValidRulePerDiem        AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lValidRuleFixedFee       AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lValidRule               AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lWeekendPass             AS LOGICAL               NO-UNDO. 
  DEFINE VARIABLE lWeekendPassDeclineLOC   AS LOGICAL               NO-UNDO.

  DEFINE VARIABLE oErrorObject             AS cls.maerrorobject     NO-UNDO.
  DEFINE VARIABLE oTFSearch                AS ma.cls.matariffsearch NO-UNDO.
  DEFINE VARIABLE oTariffType              AS cls.matarifftype      NO-UNDO.

  ASSIGN
     oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE)
     oTFSearch    = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE)
     oTariffType  = NEW cls.matarifftype().

   FOR EACH btt_auth:
    FIND buf_auth NO-LOCK
      WHERE buf_auth.auth_obj = btt_auth.auth_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    ASSIGN lRunLosCalc = CAN-FIND(FIRST btt_auth_detail NO-LOCK
                                  WHERE btt_auth_detail.auth_obj               = btt_auth.auth_obj
                                    AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
                                    AND btt_auth_detail.quantity_los          <> 0.00
                                    AND btt_auth_detail.record_action          = "DELETE":U).
                                    
    FOR EACH btt_auth_detail EXCLUSIVE-LOCK
       WHERE btt_auth_detail.auth_obj               = btt_auth.auth_obj
         AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
         AND btt_auth_detail.quantity_los          <> 0.00
         AND btt_auth_detail.record_action          = "MODIFY":U
          BY btt_auth_detail.loc_sequence:
          
      FIND FIRST buf_auth_detail NO-LOCK
        WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      ASSIGN
        btt_auth_detail.loc_sequence = 999 WHEN btt_auth_detail.loc_sequence <= 0

      /* Recalculate if new LOC record is added or LOC fields of existing record have been modified */
      lRunLosCalc = lRunLosCalc                                OR
                    (btt_auth_detail._start_date_ampm_updated  OR
                     btt_auth_detail._end_date_ampm_updated)   OR
                     NOT AVAILABLE buf_auth_detail             OR
                        (AVAILABLE buf_auth_detail             AND
                              (buf_auth_detail.owning_obj       <> btt_auth_detail.owning_obj   OR
                               buf_auth_detail.owning_key       <> btt_auth_detail.owning_key   OR
                               buf_auth_detail.quantity_los     <> btt_auth_detail.quantity_los OR
                               buf_auth_detail.loc_sequence     <> btt_auth_detail.loc_sequence OR 
                               buf_auth_detail.auth_status      <> btt_auth_detail.auth_status  OR 
                               buf_auth_detail.auth_status_note <> btt_auth_detail.auth_status_note)).

      /*
        If we did a sequence change to an existing record or we have created a new line
        we need to check for conflicting line sequence numbers below
      */
      IF NOT AVAILABLE buf_auth_detail
      OR (AVAILABLE buf_auth_detail AND btt_auth_detail.loc_sequence <> buf_auth_detail.loc_sequence) THEN
        RUN _updateAuthDetailLOCSequence IN TARGET-PROCEDURE (INPUT btt_auth.auth_obj,
                                                              BUFFER btt_auth_detail).
    END. //FOR EACH btt_auth_detail EXCLUSIVE-LOCK

    cTrackingMessage = "User LOS Calc - Run LOS Calc? " + STRING(lRunLosCalc).

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF lRunLOSCalc THEN
    DO:
      ASSIGN iCalcTime = ETIME.

      RUN _realignAuthDetailLOCSequence IN TARGET-PROCEDURE (INPUT btt_auth.auth_obj,
                                                             BUFFER btt_auth_detail ).

      //Private Per Diem
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT btt_auth.insurer_obj,
                                                     INPUT btt_auth.option_code,
                                                     INPUT "ma_acAuthRuleTypeLOC&LOS":U,
                                                     INPUT "CalcPVTPerDiem",
                                                     INPUT btt_auth.start_date,
                                                     OUTPUT lValidRulePerDiem,
                                                     OUTPUT cRuleValuePerDiem).

      //Fixed Fee Per Diem
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                     INPUT  "CalcFixedFeePerDiem",
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRuleFixedFee,
                                                     OUTPUT cRuleValueFixedFee).
                                                     
      /* For CalcFixedFeePerDiem records - assign FALSE to the add_to_total_los value, as it is defaulted to yes in the temp-table
         and we don't want to consider these records when determining the Provider dates */                                           
      FOR EACH btt_auth_detail EXCLUSIVE-LOCK
         WHERE btt_auth_detail.auth_obj               = btt_auth.auth_obj
           AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
           AND btt_auth_detail.quantity_los          <> 0.00:
         
         IF lValidRulePerDiem AND
           LOOKUP(cArsRate + ",":U + btt_auth_detail.loc_value,TRIM(cRuleValuePerDiem),"|") <> 0  
         THEN DO:
           ASSIGN btt_auth_detail.add_to_total_los = FALSE 
                  btt_auth_detail.record_action    = IF btt_auth_detail.record_action = "DELETE":U
               					           THEN btt_auth_detail.record_action
               					           ELSE "MODIFY":U.
           	  
           VALIDATE btt_auth_detail.
         END.  /* IF lValidRulePerDiem AND */           
      END.  /* FOR EACH btt_auth_detail EXCLUSIVE-LOCK */

      FOR EACH btt_auth_detail EXCLUSIVE-LOCK
         WHERE btt_auth_detail.auth_obj               = btt_auth.auth_obj
           AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
           AND btt_auth_detail.quantity_los          <> 0.00
           AND btt_auth_detail.record_action          = "MODIFY":U,
         FIRST btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_provider_obj    = btt_auth_detail.auth_provider_obj
            BY btt_auth_detail.loc_sequence:
            
        ASSIGN cArsRate    = IF btt_auth_provider.override_ars_rate <> "":U
                             THEN btt_auth_provider.override_ars_rate
                             ELSE btt_auth_provider.default_ars_rate
               lPvtPerDiem = FALSE
               lFixedFeePerDiem = FALSE.

        IF lValidRulePerDiem AND
           LOOKUP(cArsRate + ",":U + btt_auth_detail.loc_value,TRIM(cRuleValuePerDiem),"|") <> 0
        THEN ASSIGN lPVTPerDiem = TRUE.

        IF lValidRuleFixedFee AND
           LOOKUP(cArsRate + ",":U + btt_auth_detail.loc_value,TRIM(cRuleValueFixedFee),"|") <> 0
        THEN ASSIGN lFixedFeePerDiem = TRUE.

        FIND buf_auth_detail NO-LOCK
          WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

        IF btt_auth_detail._start_date_ampm_updated OR btt_auth_detail._end_date_ampm_updated
        THEN DO:
          IF btt_auth_provider.los_calculation
          THEN DO:
            ASSIGN btt_auth_provider.record_action      = "MODIFY":U
                   btt_auth_provider.los_calculation    = FALSE.                 
   
            VALIDATE btt_auth_provider.
            
            /* 
              Clear System LOS Calculation Rule for all LOC 
              lines for Provider, when we switch to User Calc
            */            
            FOR EACH btt_usercalc_auth_detail NO-LOCK
               WHERE btt_usercalc_auth_detail.auth_obj               = btt_auth_detail.auth_obj
                 AND btt_usercalc_auth_detail.auth_provider_obj      = btt_auth_provider.auth_provider_obj
                 AND btt_usercalc_auth_detail.owning_entity_mnemonic = "htmtl":U
                 AND btt_usercalc_auth_detail.quantity_los          <> 0.00:
                 
              ASSIGN btt_usercalc_auth_detail.record_action        = "MODIFY":U
                     btt_usercalc_auth_detail.los_calculation_rule = "":U.
                     
              VALIDATE btt_usercalc_auth_detail.        
            END. /* FOR EACH btt_usercalc_auth_detail NO-LOCK */ 
            
            ASSIGN cTrackingMessage = "Apply User LOS Details: Start/End Date/Time updated - Update Provider LOS calculation = FALSE. Provider: " + STRING(btt_auth_provider.doc_num,"9999999").

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END. //IF btt_auth_provider.los_calculation
          
          IF lPVTPerDiem AND btt_auth_detail.add_to_total_los
          THEN DO:
	       ASSIGN btt_auth_detail.add_to_total_los = FALSE
	              btt_auth_detail.record_action    = "MODIFY":U.
	           
	       VALIDATE btt_auth_detail.
	  
	       ASSIGN cTrackingMessage = "Apply User LOS Details: Private Ward Per Diem - Update Detail line to not add to total LOS " + 
	                                 "(Auth Detail Obj=" + STRING(btt_auth_detail.auth_detail_obj) +
	                                 ") btt_auth_detail.add_to_total_los=" + STRING(btt_auth_detail.add_to_total_los).
	  
              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END. //IF lPVTPerDiem AND btt_auth_detail.add_to_total_los
          
          IF btt_auth_detail._end_date_ampm_updated AND btt_auth_detail.add_to_total_los AND
             NOT CAN-FIND(FIRST btt_last_auth_detail
                          WHERE btt_last_auth_detail.auth_obj               = btt_auth_detail.auth_obj
                            AND btt_last_auth_detail.auth_provider_obj      = btt_auth_detail.auth_provider_obj
                            AND btt_last_auth_detail.owning_entity_mnemonic = "htmtl":U
                            AND btt_last_auth_detail.add_to_total_los       = true
                            AND btt_last_auth_detail.quantity_los          <> 0.00
                            AND btt_last_auth_detail.loc_sequence           > btt_auth_detail.loc_sequence)
          THEN DO:
            ASSIGN btt_auth_provider.record_action = "MODIFY":U
                   btt_auth_provider.end_date      = btt_auth_detail.end_date
                   btt_auth_provider.end_ampm      = btt_auth_detail.end_ampm.

            VALIDATE btt_auth_provider.
            
            ASSIGN cTrackingMessage = "Apply User LOS Details: End Date/Time updated on last detail line. Also update Provider End Date/Time".

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END. //IF btt_auth_detail.add_to_total_los AND...

          IF btt_auth_detail._start_date_ampm_updated AND btt_auth_detail.add_to_total_los AND
             NOT CAN-FIND(FIRST btt_last_auth_detail
                          WHERE btt_last_auth_detail.auth_obj               = btt_auth_detail.auth_obj
                            AND btt_last_auth_detail.auth_provider_obj      = btt_auth_detail.auth_provider_obj
                            AND btt_last_auth_detail.owning_entity_mnemonic = "htmtl":U
                            AND btt_last_auth_detail.quantity_los          <> 0.00
                            AND btt_last_auth_detail.loc_sequence           < btt_auth_detail.loc_sequence)
          THEN DO:
            ASSIGN btt_auth_provider.record_action = "MODIFY":U
                   btt_auth_provider.start_date    = btt_auth_detail.start_date
                   btt_auth_provider.start_ampm    = btt_auth_detail.start_ampm.

            VALIDATE btt_auth_provider.
            
            ASSIGN cTrackingMessage = "Apply User LOS Details: Start Date/Time updated on first detail line. Also update Provider Start Date/Time".

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            IF buf_auth_detail.start_date = buf_auth.start_date
            THEN DO:
              ASSIGN btt_auth.record_action = "MODIFY":U
                     btt_auth.start_date    = btt_auth_detail.start_date
                     btt_auth.start_ampm    = btt_auth_detail.start_ampm.

              VALIDATE btt_auth.
              
              ASSIGN cTrackingMessage = "Apply User LOS Details: Start Date/Time updated on first detail line. Also update Auth Header Start Date/Time".

              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
            END. //IF buf_auth_detail.start_date = buf_auth.start_date
          END. //IF btt_auth_detail.add_to_total_los AND...
        END. //IF btt_auth_detail._start_date_ampm_updated OR  btt_auth_detail._end_date_ampm_updated

	IF AVAILABLE buf_auth_detail AND
           NOT lFixedFeePerDiem      AND
           btt_auth_detail.quantity_los <> buf_auth_detail.quantity_los
        THEN DO:
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
                                                 INPUT        btt_auth.option_code,           /*  ipiOptionCode     */
                                                 INPUT        "",                             /*  ipcAddValidations */
                                                 OUTPUT       dTariffObj,                     /*  opdTariffObj      */
                                                 OUTPUT       dTrfCostObj,                    /*  opdTrfCostObj     */
                                                 OUTPUT       cError,                         /*  opcError          */
                                                 OUTPUT       cWarning,                       /*  opcWarning        */
                                                 OUTPUT       cAlertMessage).                 /*  opcAlertMessage   */
                                                 
          IF dTrfCostObj <> 0 THEN
          DO:
            FIND trfcost NO-LOCK
                 WHERE trfcost.trfcost-obj = dTrfCostObj
              NO-ERROR.
          
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
          
          END.  /* IF dTrfCostObj <> 0 THEN */
          
          ASSIGN btt_auth_detail.record_action   = "MODIFY":U
                 btt_auth_detail.quantity_auth   = btt_auth_detail.quantity_los
                 btt_auth_detail.item_cost       = IF AVAILABLE trfcost 
                                                   THEN trfcost.amount
                                                   ELSE 0
                 btt_auth_detail.fixed_item_cost = IF AVAILABLE trfcost
                                                   THEN trfcost.fixed-amount
                                                   ELSE 0
                 btt_auth_detail.amount_auth     = btt_auth_detail.fixed_item_cost + (btt_auth_detail.quantity_auth * btt_auth_detail.item_cost).

          VALIDATE btt_auth_detail.

          ASSIGN cTrackingMessage = "Apply User LOS Details: Quantity LOS updated on line that is not Fixed Fee - Also update Auth Qty and Auth Amount from LOS " +
                                    "(Auth Detail Obj=" + STRING(btt_auth_detail.auth_detail_obj) +
                                    ") btt_auth_detail.quantity_auth="  + STRING(btt_auth_detail.quantity_auth)   +
                                    " btt_auth_detail.item_cost="       + STRING(btt_auth_detail.item_cost)       + 
                                    " btt_auth_detail.fixed_item_cost=" + STRING(btt_auth_detail.fixed_item_cost) + 
                                    " btt_auth_detail.amount_auth="     + STRING(btt_auth_detail.amount_auth).

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END. //IF AVAILABLE buf_auth_detail AND...

        IF NOT AVAILABLE buf_auth_detail
        THEN DO:
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
                                                 INPUT        btt_auth.option_code,           /*  ipiOptionCode     */
                                                 INPUT        "",                             /*  ipcAddValidations */
                                                 OUTPUT       dTariffObj,                     /*  opdTariffObj      */
                                                 OUTPUT       dTrfCostObj,                    /*  opdTrfCostObj     */
                                                 OUTPUT       cError,                         /*  opcError          */
                                                 OUTPUT       cWarning,                       /*  opcWarning        */
                                                 OUTPUT       cAlertMessage).                 /*  opcAlertMessage   */
                                                 
          IF cError <> "" THEN
          DO:
            oErrorObject:addError
                        (INPUT "LOS":U,
                         INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                         INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                         INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                         INPUT cError).                                            /* ipcMessageText          */ 
            LEAVE.
          END.  /* IF cError <> "" THEN */
        
          IF cWarning <> "" THEN
          DO:
            oErrorObject:addError
                        (INPUT "LOS":U,
                         INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                         INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                         INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                         INPUT cWarning,                                           /* ipcMessageText          */ 
                         INPUT "WAR":U).                                           /* ipcMessageType          */ 
          END.  /* IF cWarning <> "" THEN */
      
          IF cAlertMessage <> "" THEN
          DO:
            oErrorObject:addError
                        (INPUT "LOS":U,
                         INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                         INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                         INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                         INPUT cAlertMessage,                                      /* ipcMessageText          */ 
                         INPUT "WAR":U).                                           /* ipcMessageType          */
          END.  /* IF cAlertMessage <> "" THEN */                                       
          
          IF dTrfCostObj <> 0 THEN
          DO:
            FIND trfcost NO-LOCK
                 WHERE trfcost.trfcost-obj = dTrfCostObj
              NO-ERROR.
          
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
          
          END.  /* IF dTrfCostObj <> 0 THEN */
          
         /* Populate the tariff type obj if not populated */
          IF btt_auth_detail.loc_tariff_type_obj = 0.00 THEN
          DO:
          
            FIND FIRST tt_tariff_type_link NO-LOCK NO-ERROR.
	  
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
	  
            IF AVAILABLE tt_tariff_type_link
            THEN
              ASSIGN btt_auth_detail.loc_tariff_type_obj = tt_tariff_type_link.tariff_type_obj.
	  
          END. /*IF btt_auth_detail.loc_tariff_type_obj = 0.00 THEN*/
          
          IF btt_auth_detail.loc_value = "":U AND btt_auth_detail.loc_tariff_type_obj <> 0.00 THEN
	    DO:	  
	      /* Find tariff type required to populate loc value if not populated */
	      oTariffType:focusTariffType(INPUT btt_auth_detail.loc_tariff_type_obj).
	               
	      /* Assign tariff type if the tariff type is of type LOC */
	      IF oTariffType:TariffTypeInFocus 
	      AND oTariffType:TariffTypeKey = "ma_acTariffTypeCatLOC":U THEN
	      DO:
	        ASSIGN btt_auth_detail.loc_value = oTariffType:TariffTypeCode
	               cTrackingMessage = "UserCalc Auth LOS Details: LOC Tariff Type - " + oTariffType:TariffTypeCode +
	                                  " (AuthObj=" + STRING(btt_auth.auth_obj) + ")".
	                                
	        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
	  
	      END.  /* IF oTariffType:TariffTypeInFocus AND... */	  
          END. /* IF btt_auth_detail.loc_value = "":U AND btt_auth_detail.loc_tariff_type_obj <> 0.00 THEN */

          //Populate line restriction
          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                         INPUT  btt_auth.option_code,
                                                         INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                         INPUT  "DefaultLineRestriction":U,
                                                         INPUT  btt_auth.start_date,
                                                         OUTPUT lValidRule,
                                                         OUTPUT cRuleValue).

          IF lValidRule THEN
            ASSIGN btt_auth_detail.record_action    = "MODIFY":U
                   btt_auth_detail.line_restriction = cRuleValue.

          ASSIGN btt_auth_detail.record_action   = "MODIFY":U
                 btt_auth_detail.quantity_auth   = btt_auth_detail.quantity_los
                 btt_auth_detail.item_cost       = IF AVAILABLE trfcost 
                                                   THEN trfcost.amount
                                                   ELSE 0
                 btt_auth_detail.fixed_item_cost = IF AVAILABLE trfcost
                                                   THEN trfcost.fixed-amount
                                                   ELSE 0
                 btt_auth_detail.amount_auth     = btt_auth_detail.fixed_item_cost + (btt_auth_detail.quantity_auth * btt_auth_detail.item_cost).

          VALIDATE btt_auth_detail.

        END. //IF NOT AVAILABLE buf_auth_detail

        ASSIGN btt_auth_detail._start_date_ampm_updated = FALSE
               btt_auth_detail._end_date_ampm_updated   = FALSE.
      END. //FOR EACH btt_auth_detail EXCLUSIVE-LOCK

      ASSIGN dTotalLos            = 0
             dTotalEndDate        = ?
             lTotalEndTime        = ?
             lCalcValidationError = FALSE.

      /* 
        Needed for weekend pass check 
      */
      FIND FIRST schext NO-LOCK
           WHERE schext.scheme-code = btt_auth.option_code NO-ERROR. 

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      FOR EACH btt_auth_detail NO-LOCK
         WHERE btt_auth_detail.auth_obj      = btt_auth.auth_obj
           AND btt_auth_detail.quantity_los <> 0
           AND btt_auth_detail.add_to_total_los
           AND btt_auth_detail.record_action <> "DELETE":U
            BY btt_auth_detail.loc_sequence:
        
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
                                                    OUTPUT cError).                                /* opcError               */
                                                    
	      /* 
          Check for errors returned from Weekend Pass Check 
        */
        IF cError <> ""
        THEN DO:
          oErrorObject:addError
              (INPUT "LOS":U,
               INPUT btt_auth_detail.auth_detail_obj,
               INPUT "":U,
               INPUT "owning_alt_value":U,
               INPUT btt_auth_detail.line_number,
               INPUT cError,
               INPUT "ERR":U).

          ASSIGN lCalcValidationError          = TRUE
                 btt_auth_detail.record_action = "":U
                 cTrackingMessage              = "checkWeekendPassError= ":U + cError +
                                                 " (Auth Detail Obj=" + STRING(btt_auth_detail.auth_detail_obj) + ")" + 
                                                 " btt_auth.auth_type_obj="             + STRING(btt_auth.auth_type_obj) +
                                                 " btt_auth_detail.owning_alt_value="   + btt_auth_detail.owning_alt_value +
                                                 " btt_auth_detail.auth_status="        + STRING(btt_auth_detail.auth_status) +
                                                 " btt_auth_detail.auth_status_note="   + STRING(btt_auth_detail.auth_status_note). 
                                                                                                                               
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END. /* IF cError <> "" */

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
              .
          
          IF NOT lWeekendPassDeclineLOC
          THEN 
            ASSIGN 
              btt_auth_detail.record_action     = "MODIFY":U
              btt_auth_detail.amount_auth       = 0
              btt_auth_detail.quantity_auth     = 0
              btt_auth_detail.add_to_total_los  = FALSE
              btt_auth_detail.claim_code        = schext.claim-code[1]
              .

          ASSIGN cTrackingMessage = "Apply Weekend Pass: Decline LOC "     + STRING(lWeekendPassDeclineLOC) +
                                    " (Auth Detail Obj=" + STRING(btt_auth_detail.auth_detail_obj) + ")" + 
                                    " btt_auth.auth_type_obj="             + STRING(btt_auth.auth_type_obj) +
                                    " btt_auth_detail.owning_alt_value="   + btt_auth_detail.owning_alt_value +
                                    " btt_auth_detail.auth_status="        + STRING(btt_auth_detail.auth_status) +
                                    " btt_auth_detail.auth_status_note="   + STRING(btt_auth_detail.auth_status_note).

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END. /* IF AVAILABLE schext AND lWeekendPass */


        IF NOT lCalcValidationError 
        THEN 
          ASSIGN dTotalLOS     = IF btt_auth_detail.add_to_total_los 
                                 THEN dTotalLOS + btt_auth_detail.quantity_los
                                 ELSE dTotalLOS
                 dTotalEndDate = IF btt_auth_detail.end_date <> ?
                                 THEN btt_auth_detail.end_date
                                 ELSE dTotalEndDate
                 lTotalEndTime = IF btt_auth_detail.end_date <> ?
                                 THEN btt_auth_detail.end_ampm
                                 ELSE lTotalEndTime.
      END. //FOR EACH btt_auth_detail NO-LOCK

      IF NOT lCalcValidationError
      THEN DO:
        EMPTY TEMP-TABLE ttAuthTypeConfig.
        
        /*
          Check the auth type to determine whether the auth is a period type auth or not and if not we will assign the
          calculated end date to the auth header as well ........
        */
        mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                                     INPUT-OUTPUT TABLE ttAuthTypeConfig).
        
        FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        IF AVAILABLE ttAuthTypeConfig AND (ttAuthTypeConfig.Period = 0 OR ttAuthTypeConfig.Period = ?) THEN
        DO:
          ASSIGN
            btt_auth.record_action = "MODIFY":U
            btt_auth.end_date      = dTotalEndDate
            btt_auth.end_ampm      = lTotalEndTime.
        
          VALIDATE btt_auth.
          
          ASSIGN cTrackingMessage = "Apply User LOS Details: Period Type Auth. Update Auth Header"    +
                                    " dTotalEndDate=" + (IF dTotalEndDate = ? THEN "" ELSE STRING(dTotalEndDate)) +
                                    " lTotalEndTime=" + (IF lTotalEndTime = ? THEN "" ELSE STRING(lTotalEndTime)).
        
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END. //IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.Period <> 0 THEN
        
        IF AVAILABLE buf_auth AND buf_auth.total_los <> dTotalLOS THEN
        DO:
          ASSIGN cTrackingMessage = "Apply User LOS Details: Total LOS changed. Update Auth Header "  +
                                    "dTotalLOS=" + STRING(dTotalLOS). 
        
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        
          ASSIGN btt_auth.record_action = "MODIFY":U
                 btt_auth.total_los     = dTotalLOS.
        
          VALIDATE btt_auth.
        END. //IF AVAILABLE buf_auth AND buf_auth.total_los <> dTotalLOS
      END. /* IF NOT lCalcValidationError */
    END. //IF lRunLOSCalc
  END. //FOR EACH btt_auth:

  {mip/inc/mipcatcherror.i
      &FINALLY = "IF VALID-OBJECT(oTFSearch)    THEN DELETE OBJECT oTFSearch.
      		  IF VALID-OBJECT(oTariffType)  THEN DELETE OBJECT oTariffType.
                  IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject.
                      
                  DATASET dsTariff:EMPTY-DATASET()."}
&ENDIF

