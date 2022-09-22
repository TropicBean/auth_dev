/* maauthservcalcqtyfrommin.i MEDSTAR Medical Aid System
                              Healthcare Auth data access service: 
                              - calcAuthQtyFromMinutes                              
                              (c) Copyright 2022 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
  Purpose   : Theatre Time - Qty authorised calculation
              This method will be used in the Authorisation processing 
              if minutes are captured on a Clinical Detail Line
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipdInsurerObj          AS DECIMAL     NO-UNDO.  // Client. 
DEFINE INPUT  PARAMETER ipiOptionCode	         AS INTEGER     NO-UNDO.  // Member option code.
DEFINE INPUT  PARAMETER ipdStartDate	         AS DATE        NO-UNDO.  // Authorisation detail line start date.
DEFINE INPUT  PARAMETER ipdMinutesAuthTypeObj  AS DECIMAL     NO-UNDO.  // Tariff Type Obj for minutes.
DEFINE INPUT  PARAMETER ipiMinutesAuth	       AS INTEGER     NO-UNDO.  // Minutes Authorised on detail line. 	
DEFINE OUTPUT PARAMETER opdQuantityAuth	       AS DECIMAL     NO-UNDO.  // Quantity Authorised calculated.
DEFINE OUTPUT PARAMETER opcMinutesCalcRule     AS CHARACTER   NO-UNDO.  // Rule used in calculation of Quantity Authorised.
DEFINE OUTPUT PARAMETER opcError	             AS CHARACTER   NO-UNDO.  // If any error is picked up in calculation.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cRuleValue                   AS CHARACTER   NO-UNDO. // For output value from method getAuthRuleValues.
  DEFINE VARIABLE lValidRule                   AS LOGICAL     NO-UNDO. // For output value from method getAuthRuleValues.                                                                    
  DEFINE VARIABLE iGraceMinutes                AS INTEGER     NO-UNDO. // Will be used in the PerMinBand calculation.
  DEFINE VARIABLE iPerMinBand                  AS INTEGER     NO-UNDO. // Will be used in the PerMinBand calculation.
  DEFINE VARIABLE iRemainder                   AS INTEGER     NO-UNDO. // Will be used in the PerMinBand calculation.

  FIND htm_tariff_type NO-LOCK
    WHERE htm_tariff_type.tariff_type_obj = ipdMinutesAuthTypeObj
  NO-ERROR.
  IF NOT AVAILABLE htm_tariff_type
  THEN DO:
    ASSIGN opcError = "Tariff type is invalid. Quantity Authorised can’t be calculated from Minutes Authorised.".
    LEAVE.
  END. /* IF NOT AVAILABLE htm_tariff_type */

  /*
    Calculate quantity authorised according to per minute rule
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeMinCalc":U,
                                                 INPUT  "CalcPerMin":U,
                                                 INPUT  ipdStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  /*
    If the rule exists, ensure rule is setup correctly.
  */
  IF  lValidRule 
  AND cRuleValue = ""
  THEN DO:
    ASSIGN 
      opcError = "Rule 'CalcPerMin' setup is missing. Quantity Authorised can’t be calculated from Minutes Authorised.".
    LEAVE.
  END. /* IF lValidRule AND cRuleValue = "" */
         
  /*
    If the rule exists and the tariff type passed in exists in the rule value 
    – calculate quantity authorised. 
  */
  IF  lValidRule
  AND LOOKUP(htm_tariff_type.tariff_type_code,cRuleValue) > 0 
  THEN DO:
    ASSIGN 
      opdQuantityAuth    = ipiMinutesAuth
      opcMinutesCalcRule = "CalcPerMin"
      opcError           = "".
    LEAVE.
  END. /* IF lValidRule... */

  /*
    Calculate quantity authorised according to per minute band rule. 
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeMinCalc":U,
                                                 INPUT  "CalcPerMinBand":U,
                                                 INPUT  ipdStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  /*
    If the rule exists and the tariff type passed in exists in the rule value 
    – calculate quantity authorised.
  */
  IF lValidRule 
  THEN DO:
    /*
      If the rule exists, ensure rule is setup correctly.
    */
    IF NUM-ENTRIES(cRuleValue,"|") <> 3
    THEN DO:
      ASSIGN 
        opcError = "Rule 'CalcPerMinBand' setup is incorrect. 3 entries are not setup in Rule Value. Quantity Authorised can’t be calculated from Minutes Authorised.".
      LEAVE.
    END. /* IF num-entries(cRuleValue,"|") <> 3 */
       
    IF ENTRY(1,cRuleValue,"|") = ""
    THEN DO:
      ASSIGN 
        opcError = "Minutes setup is missing for rule 'CalcPerMinBand', field Rule Value. Quantity Authorised can’t be calculated from Minutes Authorised.".
      LEAVE.
    END. /* IF ENTRY(1,cRuleValue,"|") = "" */

    IF ENTRY(2,cRuleValue,"|") = ""
    THEN DO:
      ASSIGN 
        opcError = "Grace Minutes setup is missing for rule 'CalcPerMinBand', field Rule Value. Quantity Authorised can’t be calculated from Minutes Authorised.".
      LEAVE.
    END. /* IF ENTRY(2,cRuleValue,"|") = "" */

    IF entry(3,cRuleValue,"|") = ""
    THEN DO:
      ASSIGN 
        opcError = "Tariff Type setup is missing for rule 'CalcPerMinBand', field Rule Value. Quantity Authorised can’t be calculated from Minutes Authorised.".
      LEAVE.
    END. /* IF ENTRY(3,cRuleValue,"|") = "" */

    ASSIGN iPerMinBand = INTEGER(ENTRY(1,cRuleValue,"|")) NO-ERROR.
    IF ERROR-STATUS:ERROR
    THEN DO:
      ASSIGN 
        opcError = "Band Minutes setup must be a valid integer value in rule 'CalcPerMinBand'. Quantity Authorised can’t be calculated from Minutes Authorised.".
      LEAVE.
    END. /* IF ENTRY(1,cRuleValue,"|") = "" */
        
    ASSIGN iGraceMinutes = INTEGER(ENTRY(2,cRuleValue,"|")) NO-ERROR.
    IF ERROR-STATUS:ERROR
    THEN DO:
      ASSIGN 
        opcError = "Grace Minutes setup must be a valid integer value in rule 'CalcPerMinBand'. Quantity Authorised can’t be calculated from Minutes Authorised.".
      LEAVE.
    END. /* IF ENTRY(2,cRuleValue,"|") = "" */

    /* Calculate quantity authorised if the Tariff Type is setup in the rule value. */
    IF LOOKUP(htm_tariff_type.tariff_type_code,(ENTRY(3,cRuleValue,"|"))) > 0   
    THEN DO:  
      ASSIGN 
        ipiMinutesAuth     = IF ipiMinutesAuth > iPerMinBand                          // If minutes auth > per min band, subtract grace minutes.
                             THEN ipiMinutesAuth - iGraceMinutes                     
                             ELSE ipiMinutesAuth                                     
        iRemainder         = ipiMinutesAuth MODULO iPerMinBand                        // Get remainder value if divided by per min band.
        opdQuantityAuth    = IF iRemainder > 0                                        // Add 1 for remainder value if remainder > 0.
                             THEN (1 + ((ipiMinutesAuth - iRemainder) / iPerMinBand)) 
                             ELSE (ipiMinutesAuth / iPerMinBand)
        opcMinutesCalcRule = "CalcPerMinBand":U
        opcError           = "".
      LEAVE.
    END. /* IF LOOKUP(htm_tariff_type.tariff_type_code,(ENTRY(3,cRuleValue)) > 0 */
  END. /* IF lValidRule */

  /*
    Return an error if no setup was found for the Tariff Type 
    and Quantity Authorised is not calculated. 
  */
  IF  opcMinutesCalcRule = ""
  AND opcError           = ""
  THEN DO:
    ASSIGN 
      opcError = "Tariff Type setup is missing from Minute Calculation Rules. Quantity Authorised can’t be calculated from Minutes Authorised.".
    LEAVE.
  END. /* opcMinutesCalcRule = "" */

&ENDIF
