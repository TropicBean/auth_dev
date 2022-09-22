/* maauthservcalcperdiemfixed.i  MEDSTAR Medical Aid System
                                 (c) Copyright 2020 - 2022
                                 MIP Holdings (Pty) Ltd
                                 All rights reserved
*/                      
/*------------------------------------------------------------------------------
  Purpose:     Fixed Fee Per Diem LOS Calculation
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
       {&DetailBuffer}.quantity_los  = (IF {&DetailBuffer}.quantity_los < 1             /* No half days will be allowed in the LOS for private ward per diem */
                                        THEN 1
                                        ELSE TRUNCATE({&DetailBuffer}.quantity_los, 0)) /* Remove a decimal if specified as we do not accept partial days    */

       {&DetailBuffer}.quantity_auth = 1

       {&DetailBuffer}.start_date    = {&NextStartDate} /* If this is the first line then this would equal the provider start date     */
                                                        /* otherwise the end date + 1 of the previous detail line if the previous line */
                                                        /* ended in the PM and the same as the previous line end date if the previous  */
                                                        /* line ended in the AM....                                                    */
       {&DetailBuffer}.start_ampm    = {&NextStartTime} /* If this is the first line then this would equal the provider start          */
                                                        /* time otherwise the inverse of the previous line end time                    */

       {&DetailBuffer}.end_date      = {&DetailBuffer}.start_date + ({&DetailBuffer}.quantity_los - 1) /* Quantity LOS represents the number of days start date inclusive */
       {&DetailBuffer}.end_ampm      = NO                                                              /* PM                                                              */

       {&DetailBuffer}.amount_auth   = IF {&DetailBuffer}.line_restriction <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                       THEN ROUND(({&DetailBuffer}.fixed_item_cost + ({&DetailBuffer}.quantity_auth * {&DetailBuffer}.item_cost)), 2)    /* Calculate amount authorised */
                                       ELSE {&DetailBuffer}.amount_auth

       {&EndDate}                    = {&DetailBuffer}.end_date
       {&EndTime}                    = {&DetailBuffer}.end_ampm
       {&TotalLOS}                   = {&DetailBuffer}.quantity_los

       cTrackingMessage = "calcLOSPerDiemFixedFee {&DetailBuffer}.end_date/time=" + STRING({&DetailBuffer}.end_date,"99/99/99") + 
                                                                STRING({&DetailBuffer}.end_ampm,"AM/PM")    +
                          " {&DetailBuffer}.quantity_los="    + STRING({&DetailBuffer}.quantity_los)        + 
                          " {&DetailBuffer}.item_cost="       + STRING({&DetailBuffer}.item_cost)           +
                          " {&DetailBuffer}.fixed_item_cost=" + STRING({&DetailBuffer}.fixed_item_cost)     + 
                          " {&DetailBuffer}.amount_auth="     + STRING({&DetailBuffer}.amount_auth).
    
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    VALIDATE {&DetailBuffer}.
  END. /*IF AVAILABLE {&DetailBuffer} THEN*/

&ENDIF

  { mip/inc/mipcatcherror.i }

