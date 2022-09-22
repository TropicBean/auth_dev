/* maauthservcalcfeeforservice.i  MEDSTAR Medical Aid System
                                  (c) Copyright 2020 - 2022
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/                      
/*------------------------------------------------------------------------------
  Purpose:     Fee For Service LOS Calculation
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

  DEFINE VARIABLE iDays            AS INTEGER           NO-UNDO.
  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  /* AM/PM Rules                                                                                                              */
  /* The calculation must start with the start date and time, use the length of stay (LOS) and must be calculated as follows: */
  /*   - AM to PM  =   1 Day                                                                                                  */
  /*   - AM to AM  = 0.5 Day                                                                                                  */
  /*   - PM to PM  = 0.5 Day                                                                                                  */
  /*   - PM to AM  =   1 Day                                                                                                  */

  /* Depending on the sequence, other detail lines may already have contributed to      */
  /* the Length of stay which is important in determining where this line should start. */
  {&CalcNextStartDateTime}
  
  ASSIGN
     oErrorObject   = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  IF AVAILABLE {&DetailBuffer} THEN
  DO:
    /* 
      Check that either half day or full day is entered. If incorrect decimal, raise an error 
    */ 
    IF ({&DetailBuffer}.quantity_los / 0.5) <> TRUNC({&DetailBuffer}.quantity_los / 0.5,0)
    THEN DO:
      /* Dont save if we are going to raise an error */
      ASSIGN {&DetailBuffer}.record_action = "":U. 
    
      oErrorObject:addError
            (INPUT "LOS":U,
             INPUT {&DetailBuffer}.auth_detail_obj,
             INPUT "":U,
             INPUT "":U,
             INPUT {&DetailBuffer}.line_number,
             INPUT "Invalid LOS quantity decimal value, only half days are allowed",
             INPUT "ERR":U).
    END. /* IF ({&DetailBuffer}.quantity_los / 0.5) <> TRUNC({&DetailBuffer}.quantity_los / 0.5,0) */
    
    ASSIGN
       {&DetailBuffer}.quantity_los  = {&DetailBuffer}.quantity_los
       {&DetailBuffer}.quantity_auth = {&DetailBuffer}.quantity_los

       {&DetailBuffer}.start_date    = {&NextStartDate} /* If this is the first line then this would equal the provider start date    */
                                                        /* otherwise the end date + 1 of the previous detail line if the previous line*/
                                                        /* ended in the PM and the same as the previous line end date if the previous */
                                                        /* line ended in the AM....                                                   */
       {&DetailBuffer}.start_ampm    = {&NextStartTime} /* If this is the first line then this would equal the provider start         */
                                                        /* time otherwise the inverse of the previous line end time                   */

       iDays                         = (IF {&DetailBuffer}.quantity_los >= 1
                                        THEN ROUND({&DetailBuffer}.quantity_los, 0)
                                        ELSE 0)

       {&DetailBuffer}.end_date      = {&DetailBuffer}.start_date
                                     + iDays
                                     + (IF (NOT {&DetailBuffer}.start_ampm AND {&DetailBuffer}.quantity_los - TRUNC({&DetailBuffer}.quantity_los, 0) = 0) OR iDays < 1
                                        THEN 0
                                        ELSE -1)                                                                        /* Quantity LOS represents the number of subsequent days                      */
                                                                                                                        /* Remove half day from quantity los before adding to the start date as this  */
                                                                                                                        /* will be represented by the start time.                                     */

       {&DetailBuffer}.end_ampm      = (IF {&DetailBuffer}.quantity_los - TRUNCATE({&DetailBuffer}.quantity_los, 0) > 0 /* See am/pm rules above                                                      */
                                        THEN {&DetailBuffer}.start_ampm                                                 /* If we have a half day in the mix then the start time and end time will be  */
                                        ELSE NOT {&DetailBuffer}.start_ampm)                                            /* the same otherwise it will switch.                                         */

       {&DetailBuffer}.amount_auth   = IF {&DetailBuffer}.line_restriction <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                       THEN ROUND(({&DetailBuffer}.fixed_item_cost + ({&DetailBuffer}.quantity_auth * {&DetailBuffer}.item_cost)), 2)    /* Calculate amount authorised */
                                       ELSE {&DetailBuffer}.amount_auth
            
       {&EndDate}                    = {&DetailBuffer}.end_date                                                         /* Store last end date to be returned for final discharge date                */
       {&EndTime}                    = {&DetailBuffer}.end_ampm                                                         /* Store last end time to be returned for final discharge time                */

       {&TotalLOS}                   = {&DetailBuffer}.quantity_los                                                     /* Add to total LOS                                                           */

       cTrackingMessage = "calcLOSFeeForService" + 
                          " iDays="                           + STRING(iDays)                               +
                          " {&DetailBuffer}.end_date/time="   + STRING({&DetailBuffer}.end_date,"9999/99/99") + 
                                                                STRING({&DetailBuffer}.end_ampm,"AM/PM")    +
                          " {&DetailBuffer}.quantity_los="    + STRING({&DetailBuffer}.quantity_los)        + 
                          " {&DetailBuffer}.item_cost="       + STRING({&DetailBuffer}.item_cost)           +
                          " {&DetailBuffer}.fixed_item_cost=" + STRING({&DetailBuffer}.fixed_item_cost)     + 
                          " {&DetailBuffer}.amount_auth="     + STRING({&DetailBuffer}.amount_auth).
    
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    VALIDATE {&DetailBuffer}.
  END. /*IF AVAILABLE {&DetailBuffer} THEN*/

&ENDIF

  { mip/inc/mipcatcherror.i 
        &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

