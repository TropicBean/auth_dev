/* maauthservcalcamountauth.i MEDSTAR Medical Aid System
                            Healthcare Auth data access service: calcAmountAuth                              
                            (c) Copyright 2020 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
------------------------------------------------------------------------------
  Purpose   : Calculate Auth Tariff Cost Amount
  Notes     : This routine will accept as input parameters:
                - dsAuthorisation dataset
                - Cost calculation indicator
                - Tariff cost amount
              And will produce as output parameters:
                - Cost calulated
                - Errors that might have occurred
------------------------------------------------------------------------------*/  
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.
  DEFINE INPUT        PARAMETER ipdAuthDetailObj         AS DECIMAL     NO-UNDO.
  DEFINE INPUT        PARAMETER ipcCostCalculation       AS CHARACTER   NO-UNDO.
  DEFINE INPUT        PARAMETER ipdTrfcostAmount         AS DECIMAL     NO-UNDO.
  DEFINE OUTPUT       PARAMETER opdItemCost              AS DECIMAL     NO-UNDO.
  DEFINE OUTPUT       PARAMETER opcLineRestriction       AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT       PARAMETER opcRelatedEntityMnemonic AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT       PARAMETER opdRelatedObj            AS DECIMAL     NO-UNDO.
  DEFINE OUTPUT       PARAMETER opcRelatedValue          AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT       PARAMETER opdCostCalculated        AS DECIMAL     NO-UNDO.
  DEFINE OUTPUT       PARAMETER opcCostError             AS CHARACTER   NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cAuthNum               AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cDependantGender       AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE iDependantAgeDays      AS INTEGER     INITIAL 0 NO-UNDO.
  DEFINE VARIABLE iDependantAgeYears     AS INTEGER     INITIAL 0 NO-UNDO.
  DEFINE VARIABLE iTotalLOS              AS INTEGER     INITIAL 0 NO-UNDO.
  
  /* Variables for Orthodontic Monthly fee */
  DEFINE VARIABLE cRelatedEntityMnemonic AS CHARACTER   NO-UNDO.  /* Use to save Initial Fee Entity            */
  DEFINE VARIABLE cRelatedValue          AS CHARACTER   NO-UNDO.  /* Use to save Initial Fee Tariff Code       */
  DEFINE VARIABLE dInitialFeeAmountAuth  AS DECIMAL     NO-UNDO.  /* Use to save Initial Fee Amount Authorised */
  DEFINE VARIABLE dInitialFeeItemCost    AS DECIMAL     NO-UNDO.  /* Use to save Initial Fee Item Cost         */
  DEFINE VARIABLE dRelatedObj            AS DECIMAL     NO-UNDO.  /* Use to save Initial Fee Obj               */
  DEFINE VARIABLE lInitialFee            AS LOGICAL     NO-UNDO.  /* Use to indicate if initial fee is found   */
  
  DEFINE BUFFER btt_auth            FOR tt_auth.
  DEFINE BUFFER hat_auth            FOR hat_auth.
  DEFINE BUFFER btt_auth_detail     FOR tt_auth_detail.
  DEFINE BUFFER current_auth_detail FOR tt_auth_detail.

  IF ipdAuthDetailObj <> 0 
  THEN DO:
    FIND current_auth_detail NO-LOCK
      WHERE current_auth_detail.auth_detail_obj = ipdAuthDetailObj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
  END.  /* IF ipdAuthDetailObj <> 0 */

  IF AVAILABLE current_auth_detail 
  THEN DO:
    FIND btt_auth NO-LOCK 
      WHERE btt_auth.auth_obj = current_auth_detail.auth_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE btt_auth THEN
    DO:
      FIND hat_auth NO-LOCK
        WHERE hat_auth.auth_obj = current_auth_detail.auth_obj NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE hat_auth 
      THEN
        ASSIGN opcCostError      = "Authorisation record not available to do Tariff Cost calculation."
               opdCostCalculated = 0.
      ELSE DO:
        IF ipcCostCalculation = "ma_acTrfCostCalcAgeModifier":U
        THEN DO:
          mipEnv:Health:maMember:getDependantAgeGender (INPUT  hat_auth.mem_num,
                                                        INPUT  hat_auth.dependant,
                                                        INPUT  hat_auth.start_date,
                                                        OUTPUT iDependantAgeYears,
                                                        OUTPUT iDependantAgeDays,
                                                        OUTPUT cDependantGender).
          ASSIGN iTotalLOS = hat_auth.total_los.
        END.  /* IF ipcCostCalculation = "ma_acTrfCostCalcAgeModifier":U */

        ASSIGN cAuthNum  = hat_auth.auth_num.
      END.  /* ELSE - IF NOT AVAILABLE hat_auth THEN */
    END.  /* IF NOT AVAILABLE btt_auth THEN */
    ELSE DO:
      IF ipcCostCalculation = "ma_acTrfCostCalcAgeModifier":U
      THEN 
        ASSIGN iDependantAgeYears = btt_auth._dependant_age_years
               iTotalLOS          = btt_auth.total_los.

      ASSIGN cAuthNum = btt_auth.auth_num.
    END.  /* ELSE - IF NOT AVAILABLE btt_auth THEN */
  END.  /* IF AVAILABLE current_auth_detail */
  ELSE 
    ASSIGN opcCostError      = "Authorisation record not available to do Tariff Cost calculation."
           opdCostCalculated = 0.

  IF opcCostError = "" THEN
  DO:
    CASE ipcCostCalculation:

      /* Per Diem Age modifier calc */
      WHEN "ma_acTrfCostCalcAgeModifier":U 
      THEN DO:
        IF ipcCostCalculation = "ma_acTrfCostCalcAgeModifier":U THEN 
          ASSIGN
            opdCostCalculated = ipdTrfcostAmount * iDependantAgeYears * iTotalLOS.

        IF ERROR-STATUS:ERROR THEN
          ASSIGN 
            opcCostError      = "Error occured with the Tariff Cost calculation for Auth: " + cAuthNum
            opdCostCalculated = 0.
      END.  /* WHEN "ma_acTrfCostCalcAgeModifier":U */ 

      /* Orthodontic Initial Fee Calculation. */
      WHEN "ma_acTrfCostCalcOrthoInitialFee":U
      THEN DO:
        ASSIGN opdCostCalculated  = (ipdTrfCostAmount * 20) / 100
               opcLineRestriction = "ma_acAuthLineRestrictionAmt&Qty":U.

        IF ERROR-STATUS:ERROR THEN
          ASSIGN 
            opcCostError      = "Error occured with the Tariff Cost calculation for Auth: " + cAuthNum
            opdCostCalculated = 0.
      END.  /* "ma_acTrfCostCalcOrthoInitialFee":U */

      /* Orthodontic Monthly Fee Calculation. */
      WHEN "ma_acTrfCostCalcOrthoMonthlyFee":U
      THEN DO:
        /* Determine the initial fee to which the monthly fee links by checking code link setups */
        ASSIGN lInitialFee = NO.
        
        IF AVAILABLE current_auth_detail THEN
        DO:
	
          INITIALFEE-BLK:
          FOR EACH btt_auth_detail NO-LOCK
            WHERE btt_auth_detail.auth_obj          = current_auth_detail.auth_obj
              AND btt_auth_detail.auth_provider_obj = current_auth_detail.auth_provider_obj
              AND btt_auth_detail.auth_detail_obj  <> current_auth_detail.auth_detail_obj:
              
            /* Use the Code-Link setup to find details of the Tariff initial fee to which the Monthly Fee links */
            FIND FIRST hlm_code_link NO-LOCK 
              WHERE  hlm_code_link.acronym_key       = "ma_acCodeLinkCatTariff":U
                AND  hlm_code_link.parent_entity     = "htmtl":U
                AND  hlm_code_link.parent_entity_obj = current_auth_detail.owning_obj
                AND  hlm_code_link.parent_entity_key = "":U
                AND  hlm_code_link.child_entity      = "htmtl":U
                AND  hlm_code_link.child_entity_obj  = btt_auth_detail.owning_obj
                AND  hlm_code_link.effective_date   <= current_auth_detail.start_date
                AND (hlm_code_link.end_date          = ?
                 OR  hlm_code_link.end_date         >= current_auth_detail.end_date)
                NO-ERROR.
            
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
            IF AVAILABLE hlm_code_link 
	    THEN
              ASSIGN dInitialFeeItemCost    = btt_auth_detail.item_cost
                     dInitialFeeAmountAuth  = btt_auth_detail.amount_auth
                     cRelatedEntityMnemonic = "hatad":U
                     dRelatedObj            = btt_auth_detail.auth_detail_obj
                     cRelatedValue          = btt_auth_detail.owning_alt_value
                     lInitialFee            = YES.
          
	  END.  /* FOR EACH btt_auth_detail NO-LOCK (INITIALFEE-BLK) */
              
          /* Calculate Monthly Fee */
          IF lInitialFee 
	  THEN
            ASSIGN opdCostCalculated 	    = (dInitialFeeItemCost - dInitialFeeAmountAuth)          /* Original cost - Initial Fee amount authorised */
                   opdItemCost              = opdCostCalculated / current_auth_detail.quantity_auth  /* Item cost for Monthly Fee */
                   opcLineRestriction       = "ma_acAuthLineRestrictionAmt&Qty":U                    /* Line restriction */
                   opdRelatedObj            = dRelatedObj					     /* Initial Fee Obj */
                   opcRelatedValue          = cRelatedValue					     /* Initial Fee Tariff Code */
                   opcRelatedEntityMnemonic = cRelatedEntityMnemonic.		                     /* Initial Fee Entity */
          ELSE                        
            ASSIGN opcCostError      = "Initial Fee must be captured before the Monthly Fee can be calculated."
                   opdCostCalculated = 0.
       
        END.  /* IF AVAILABLE current_auth_detail THEN */
      END.  /* "ma_acTrfCostCalcOrthoMonthlyFee":U */    

    END CASE.  /* CASE ipcCostCalculation */
  END.  /* IF opcCostError = "" THEN */

  {mip/inc/mipcatcherror.i}

&ENDIF

