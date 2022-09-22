/* maauthservcalclosdaycase.i  MEDSTAR Medical Aid System
                              (c) Copyright 2020 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/                      
/*------------------------------------------------------------------------------
  Purpose:    Day Case LOS Calculation
  Parameters:  
  Notes:
              * MMP-399 ( RTB49002 )
                Developed

              * MMP-423 ( RTB49582 )
                Bug fix for multiple calculations being applicable
                across all detail lines. Changes made to cater for
                additional input dates which will be the end date
                of the previous line in the sequence if applicable.
                eg. Dependant moves between wards from general Ward
                    to maternity ward.
------------------------------------------------------------------------------*/
  {&CalcProcedureSignature}

  {&CalcStandardDefinitions}

  DEFINE VARIABLE cMessage         AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cRuleValue       AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE lValidRule       AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE iLine            AS INTEGER           NO-UNDO.
  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  /* Depending on the sequence, other detail lines may already have contributed to      */
  /* the Length of stay which is important in determining where this line should start. */
  ASSIGN iCalcTime = ETIME.

  {&CalcNextStartDateTime}

  ASSIGN
     cMessage       = "Duplicate day case LOS details captured."
     oErrorObject   = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  /* Get rule to determine how we should handle multiple lines */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                 INPUT  "CalcDayError":U,
                                                 INPUT  ipdStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  
  IF AVAILABLE {&DetailBuffer} THEN
  DO:

    /* Check how many LOS records */
    FOR EACH b{&DetailBuffer} NO-LOCK
       WHERE b{&DetailBuffer}.record_action = {&DetailBuffer}.record_action:  /* Record action will temporarily contain the relevant calculation procedure to run */

      ASSIGN iLine = iLine + 1.
    END. /*FOR EACH b{&DetailBuffer} NO-LOCK*/

    /* Day case LOS should always be 1 */
    IF iLine = 1 
    THEN
      ASSIGN
        {&DetailBuffer}.quantity_los  = 1
        {&DetailBuffer}.quantity_auth = {&DetailBuffer}.quantity_los
        
        {&DetailBuffer}.start_date    = {&NextStartDate}            /* If this is the first line then this would equal the provider start date    */
                                                                    /* otherwise the end date + 1 of the previous detail line if the previous line*/
                                                                    /* ended in the PM and the same as the previous line end date if the previous */
                                                                    /* line ended in the AM....                                                   */
        {&DetailBuffer}.start_ampm    = {&NextStartTime}            /* If this is the first line then this would equal the provider start         */
                                                                    /* time otherwise the inverse of the previous line end time                   */
        
        {&DetailBuffer}.end_date      = {&DetailBuffer}.start_date  /* Day case - discharged same day always */
        {&DetailBuffer}.end_ampm      = NO                          /* PM                                    */
        
        {&DetailBuffer}.amount_auth   = IF {&DetailBuffer}.line_restriction <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                        THEN ROUND(({&DetailBuffer}.fixed_item_cost + ({&DetailBuffer}.quantity_auth * {&DetailBuffer}.item_cost)), 2)    /* Calculate amount authorised */
                                        ELSE {&DetailBuffer}.amount_auth

        {&EndDate}                    = {&DetailBuffer}.end_date                                               /* Store last end date to be returned for final discharge date */
        {&EndTime}                    = {&DetailBuffer}.end_ampm
        {&TotalLOS}                   = {&DetailBuffer}.quantity_los.

    ASSIGN cTrackingMessage = "calcLOSDayCase Start=" + STRING({&DetailBuffer}.start_date,"99/99/99") + STRING({&DetailBuffer}.start_ampm,"AM/PM") +
                              " End=" + STRING({&DetailBuffer}.end_date,"99/99/99") + STRING({&DetailBuffer}.end_ampm,"AM/PM") +
                              " Item Cost=" + STRING({&DetailBuffer}.item_cost) +  
                              " Qty LOS=" + STRING({&DetailBuffer}.quantity_los) + 
                              " Fixed Item Cost=" + STRING({&DetailBuffer}.fixed_item_cost) + 
                              " Amount Auth=" + STRING({&DetailBuffer}.amount_auth).

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /* For day cases we should not have multiple los lines */
    IF iLine > 1 AND lValidRule THEN
    DO:

      ASSIGN cTrackingMessage = "CalcDayError=":U + cRuleValue + " Num Lines: " + STRING(iLine).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      /* Dont save if we are going to raise an error */
      ASSIGN {&DetailBuffer}.record_action = "":U WHEN iLine > 1.

      ASSIGN cMessage = cMessage + ".[Help=Auth Rule Code: CalcDayError]".
        
      /* Block */
      IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "BLOCK":U THEN
        oErrorObject:addError
          (INPUT "LOS":U,
           INPUT {&DetailBuffer}.auth_detail_obj,
           INPUT "":U,
           INPUT "owning_alt_value":U,
           INPUT {&DetailBuffer}.line_number,
           INPUT cMessage,
           INPUT "ERR":U).

      /* Load */
      IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "LOAD":U THEN
      DO:

        /* Make sure the rule is properly formed, in other words we should not do anything */
        /* if the rule is not configured properly with a status and status reason.         */
        IF NUM-ENTRIES(cRuleValue, "|":U) = 3  AND
           ENTRY(2, cRuleValue, "|":U) <> "":U AND
           ENTRY(3, cRuleValue, "|":U) <> "":U
        THEN
        DO:

          /* Default the status,status reason */
          ASSIGN
             {&DetailBuffer}.auth_status      = INTEGER(ENTRY(2, cRuleValue, "|":U))
             {&DetailBuffer}.auth_status_note =         ENTRY(3, cRuleValue, "|":U).

        END. /* IF NUM-ENTRIES(cRuleValue, "|":U) = 3  AND */
      END. /* IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "LOAD":U THEN */
    END. /* IF iLine > 1 AND lValidRule THEN  */

    VALIDATE {&DetailBuffer}.
  END . /*IF AVAILABLE {&DetailBuffer} THEN DO:*/

&ENDIF

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

