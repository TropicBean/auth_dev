/* maauthservcalcperdiempvt.i  MEDSTAR Medical Aid System
                              (c) Copyright 2020 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/                      
/*------------------------------------------------------------------------------
  Purpose:     Private Ward Per Diem LOS Calculation
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

&IF {&DBDFMA} >= 010195 &THEN

  /* Depending on the sequence, other detail lines may already have contributed to      */
  /* the Length of stay which is important in determining where this line should start. */
  {&CalcNextStartDateTime}

  IF AVAILABLE {&DetailBuffer} THEN
  DO:

    ASSIGN
       {&DetailBuffer}.quantity_los      = (IF {&DetailBuffer}.quantity_los < 1            /* No half days will be allowed in the LOS for private ward per diem */
                                           THEN 1
                                           ELSE TRUNCATE({&DetailBuffer}.quantity_los, 0)) /* Remove a decimal if specified as we do not accept partial days    */

       {&DetailBuffer}.quantity_auth     = {&DetailBuffer}.quantity_los

       {&DetailBuffer}.start_ampm        = (IF {&DetailBuffer}.start_date <> ?             /* Private ward user must specify start am/pm                        */
                                            THEN
                                              (IF {&DetailBuffer}.start_ampm <> ?
                                               THEN {&DetailBuffer}.start_ampm
                                               ELSE YES) /* AM */
                                            ELSE iplStartTime)

       {&DetailBuffer}.start_date        = (IF {&DetailBuffer}.start_date <> ?             /* We cant always assume the start date of a private ward was the following day so */
                                            THEN {&DetailBuffer}.start_date                /* if the start date has been specified we will use that otherwise we will use     */
                                            ELSE ipdStartDate)                             /* provider start date/time                                                        */

       {&DetailBuffer}.end_date          = {&DetailBuffer}.start_date + ({&DetailBuffer}.quantity_los - 1)        /* Quantity LOS represents the number of days start date inclusive */
       {&DetailBuffer}.end_ampm          = NO                                                                     /* PM                                                              */

       {&DetailBuffer}.amount_auth   = IF {&DetailBuffer}.line_restriction <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                       THEN ROUND(({&DetailBuffer}.fixed_item_cost + ({&DetailBuffer}.quantity_auth * {&DetailBuffer}.item_cost)), 2)    /* Calculate amount authorised */
                                       ELSE {&DetailBuffer}.amount_auth

       {&DetailBuffer}.add_to_total_los  = FALSE                                                                  /* Private ward per diem should not form part of total LOS         */

       cTrackingMessage = "calcLOSPerDiemPVT {&DetailBuffer}.start_date/time="                  + 
                                                                STRING({&DetailBuffer}.start_date,"99/99/99") + 
                                                                STRING({&DetailBuffer}.start_ampm,"AM/PM")    +
                          " {&DetailBuffer}.end_date/time="   + STRING({&DetailBuffer}.end_date,"99/99/99")   + 
                                                                STRING({&DetailBuffer}.end_ampm,"AM/PM")      +
                          " {&DetailBuffer}.quantity_los="    + STRING({&DetailBuffer}.quantity_los)          +
                          " {&DetailBuffer}.quantity_auth="   + STRING({&DetailBuffer}.quantity_auth)         +
                          " {&DetailBuffer}.item_cost="       + STRING({&DetailBuffer}.item_cost)             +
                          " {&DetailBuffer}.fixed_item_cost=" + STRING({&DetailBuffer}.fixed_item_cost)       + 
                          " {&DetailBuffer}.amount_auth="     + STRING({&DetailBuffer}.amount_auth).
    
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    VALIDATE {&DetailBuffer}.

    /* Note: Total LOS and final discharge date should not be affected by the Private Ward*/
    /*       so just pass back the same date time that was passed in.                     */
    ASSIGN
       {&EndDate}  = {&CalculatedEndDate}
       {&EndTime}  = {&CalculatedEndTime}
       {&TotalLOS} = 0.
  END. /*IF AVAILABLE {&DetailBuffer} THEN*/

&ENDIF

  { mip/inc/mipcatcherror.i }

