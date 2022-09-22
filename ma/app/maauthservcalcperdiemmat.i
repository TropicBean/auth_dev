/* maauthservcalcperdiemmat.i  MEDSTAR Medical Aid System
                               (c) Copyright 2020 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/                      
/*------------------------------------------------------------------------------
  Purpose:     Maternity Per Diem LOS Calculation
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

  DEFINE VARIABLE cArsRate               AS CHARACTER         NO-UNDO. 
  DEFINE VARIABLE cRuleValue             AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE lFirstDay              AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lMaternityPerDiemFirst AS LOGICAL           NO-UNDO. 
  DEFINE VARIABLE lSubsequentDaysExist   AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lValidRule             AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oErrorObject           AS cls.maerrorobject NO-UNDO.
  
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.

  /* Depending on the sequence, other detail lines may already have contributed to      */
  /* the Length of stay which is important in determining where this line should start. */
  {&CalcNextStartDateTime}
  
  ASSIGN
     oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
     
     
  /* Get Maternity Per Diem First Day rule */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                 INPUT  "MaternityPerDiemFirst":U,
                                                 INPUT  ipdStartDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).   

  IF AVAILABLE {&DetailBuffer} THEN
  DO:
    ASSIGN 
      cArsRate               = "":U
      lMaternityPerDiemFirst = FALSE
      lFirstDay              = NOT CAN-FIND(FIRST b{&DetailBuffer} NO-LOCK
                                            WHERE b{&DetailBuffer}.los_calculation_rule = {&DetailBuffer}.los_calculation_rule
                                              AND b{&DetailBuffer}.loc_sequence         < {&DetailBuffer}.loc_sequence)
      lSubsequentDaysExist   = CAN-FIND(FIRST b{&DetailBuffer} NO-LOCK                                                            /* Check if we have another record which is relevant to maternity calc so       */
                                        WHERE b{&DetailBuffer}.los_calculation_rule = {&DetailBuffer}.los_calculation_rule        /* we can determine if we have a first day/subsequent day situation or          */
                                          AND b{&DetailBuffer}.auth_detail_obj     <> {&DetailBuffer}.auth_detail_obj)            /* a single maternity record has been specified                                 */
      . 
                                                                                                    
    IF lValidRule AND cRuleValue <> "":U
    THEN DO:
      FIND FIRST btt_auth_provider NO-LOCK
           WHERE btt_auth_provider.auth_provider_obj = {&DetailBuffer}.auth_provider_obj NO-ERROR. 
           
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
      
      IF AVAILABLE btt_auth_provider THEN 
        ASSIGN cArsRate = IF btt_auth_provider.override_ars_rate <> "":U
                          THEN btt_auth_provider.override_ars_rate
                          ELSE btt_auth_provider.default_ars_rate.
      
      IF LOOKUP(cArsRate + ",":U + {&DetailBuffer}.loc_value,TRIM(cRuleValue),"|") <> 0
      THEN ASSIGN lMaternityPerDiemFirst = TRUE. 
      
      /* 
        If this line is a Maternity Per Diem First Day Fee and 
        this is not a duplicate First Day Fee and 
        there exists Maternity Per Diem lines with a sequence smaller than 
        the current line, raise an error
      */
      IF lMaternityPerDiemFirst AND 
         NOT CAN-FIND(FIRST b{&DetailBuffer}
                  WHERE b{&DetailBuffer}.auth_provider_obj      = {&DetailBuffer}.auth_provider_obj
                    AND b{&DetailBuffer}.owning_entity_mnemonic = {&DetailBuffer}.owning_entity_mnemonic
                    AND b{&DetailBuffer}.owning_obj             = {&DetailBuffer}.owning_obj
                    AND b{&DetailBuffer}.owning_key             = {&DetailBuffer}.owning_key
                    AND b{&DetailBuffer}.auth_detail_obj       <> {&DetailBuffer}.auth_detail_obj) AND 
         CAN-FIND(FIRST b{&DetailBuffer}
                  WHERE b{&DetailBuffer}.auth_provider_obj    = {&DetailBuffer}.auth_provider_obj
                    AND b{&DetailBuffer}.los_calculation_rule = {&DetailBuffer}.los_calculation_rule
                    AND b{&DetailBuffer}.loc_sequence         < {&DetailBuffer}.loc_sequence) 
      THEN DO:
        /* Dont save if we are going to raise an error */
        ASSIGN {&DetailBuffer}.record_action = "":U. 
        
        oErrorObject:addError
            (INPUT "LOS":U,
             INPUT {&DetailBuffer}.auth_detail_obj,
             INPUT "":U,
             INPUT "":U,
             INPUT {&DetailBuffer}.line_number,
             INPUT "First Day Fee must be loaded first. [HELP=Auth Rule Code: MaternityPerDiemFirst]",
             INPUT "ERR":U).
      END. /* IF lSubsequentDaysExist AND lMaternityPerDiemFirst */
      
      IF lMaternityPerDiemFirst AND 
         CAN-FIND(FIRST b{&DetailBuffer}
                  WHERE b{&DetailBuffer}.auth_provider_obj      = {&DetailBuffer}.auth_provider_obj
                    AND b{&DetailBuffer}.owning_entity_mnemonic = {&DetailBuffer}.owning_entity_mnemonic
                    AND b{&DetailBuffer}.owning_obj             = {&DetailBuffer}.owning_obj
                    AND b{&DetailBuffer}.owning_key             = {&DetailBuffer}.owning_key
                    AND b{&DetailBuffer}.auth_detail_obj       <> {&DetailBuffer}.auth_detail_obj)
      THEN DO:
        /* Dont save if we are going to raise an error */
        ASSIGN {&DetailBuffer}.record_action = "":U. 
      
        oErrorObject:addError
            (INPUT "LOS":U,
             INPUT {&DetailBuffer}.auth_detail_obj,
             INPUT "":U,
             INPUT "":U,
             INPUT {&DetailBuffer}.line_number,
             INPUT "Duplicate First Day Fee not allowed. [HELP=Auth Rule Code: MaternityPerDiemFirst]",
             INPUT "ERR":U).       
      END. /* IF NOT lFirstDay AND lMaternityPerDiemFirst */
    END. /* IF lValidRule AND cRuleValue <> "":U */
    
    ASSIGN
       {&DetailBuffer}.quantity_los  = (IF (lFirstDay AND lMaternityPerDiemFirst) OR {&DetailBuffer}.quantity_los < 1    /* No half days will be allowed in the LOS for private ward per diem            */
                                        THEN 1
                                        ELSE TRUNCATE({&DetailBuffer}.quantity_los, 0))                                /* Remove a decimal if specified as we do not accept partial days               */


       {&DetailBuffer}.quantity_auth = (IF lFirstDay AND lMaternityPerDiemFirst
                                        THEN 1                                                                         /* First day quantity auth will be 1 and for subsequent days it will            */
                                        ELSE {&DetailBuffer}.quantity_los)                                             /* be the quantity los value                                                    */

       {&DetailBuffer}.start_date    = {&NextStartDate}                                                                /* If this is the first line then this would equal the provider start date      */
                                                                                                                       /* otherwise the end date + 1 of the previous detail line if the previous line  */
                                                                                                                       /* ended in the PM and the same as the previous line end date if the previous   */
                                                                                                                       /* line ended in the AM....                                                     */
       {&DetailBuffer}.start_ampm    = {&NextStartTime}                                                                /* If this is the first line then this would equal the provider start           */
                                                                                                                       /* time otherwise the inverse of the previous line end time                     */
                                                                                                                       /* If this is the first maternity day detail line then use the provider         */
                                                                                                                       /* start time otherwise AM                                                      */
       {&DetailBuffer}.end_date      = (IF lFirstDay AND lMaternityPerDiemFirst
                                        THEN {&DetailBuffer}.start_date                                                /* First record ends on the same day                                            */
                                        ELSE {&DetailBuffer}.start_date + ({&DetailBuffer}.quantity_los - 1))          /* Quantity LOS represents the number of subsequent days after the first day    */

       {&DetailBuffer}.end_ampm      = NO                                                                              /* PM                                                                           */
       {&DetailBuffer}.amount_auth   = IF {&DetailBuffer}.line_restriction <> "ma_acAuthLineRestrictionAmt&Qty":U 
                                       THEN ROUND(({&DetailBuffer}.fixed_item_cost + ({&DetailBuffer}.quantity_auth * {&DetailBuffer}.item_cost)), 2)    /* Calculate amount authorised */
                                       ELSE {&DetailBuffer}.amount_auth
       {&EndDate}                    = {&DetailBuffer}.end_date                                                        /* Store last end date to be returned for final discharge date                  */
       {&EndTime}                    = {&DetailBuffer}.end_ampm

       {&TotalLOS}                   = {&DetailBuffer}.quantity_los                                                   /* Add to total LOS                                                             */

       cTrackingMessage = "calcLOSPerDiemMat lFirstDay="               + STRING(lFirstDay)                           +
                          " lSubsequentDaysExist="                     + STRING(lSubsequentDaysExist)                +
                          " lMaternityPerDiemFirst="                   + STRING(lMaternityPerDiemFirst)              +
                          " {&DetailBuffer}.end_date/time="            + STRING({&DetailBuffer}.end_date,"99/99/99") + 
                                                                         STRING({&DetailBuffer}.end_ampm,"AM/PM")    +
                          " {&DetailBuffer}.quantity_los="             + STRING({&DetailBuffer}.quantity_los)        +
                          " {&DetailBuffer}.quantity_auth="            + STRING({&DetailBuffer}.quantity_auth)       +
                          " {&DetailBuffer}.item_cost="                + STRING({&DetailBuffer}.item_cost)           +
                          " {&DetailBuffer}.fixed_item_cost="          + STRING({&DetailBuffer}.fixed_item_cost)     + 
                          " {&DetailBuffer}.amount_auth="              + STRING({&DetailBuffer}.amount_auth).
    
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    VALIDATE {&DetailBuffer}.
    
    /* 
      Check if we are loading duplicate Subsequent Day Maternity Per Diems 
      in the same year 
    */
    IF NOT lMaternityPerDiemFirst AND
       CAN-FIND(FIRST b{&DetailBuffer}
                WHERE b{&DetailBuffer}.record_action          = {&DetailBuffer}.record_action    
                  AND b{&DetailBuffer}.auth_provider_obj      = {&DetailBuffer}.auth_provider_obj
                  AND b{&DetailBuffer}.owning_entity_mnemonic = {&DetailBuffer}.owning_entity_mnemonic
                  AND b{&DetailBuffer}.owning_obj             = {&DetailBuffer}.owning_obj
                  AND b{&DetailBuffer}.owning_key             = {&DetailBuffer}.owning_key
                  AND YEAR(b{&DetailBuffer}.start_date)       = YEAR({&DetailBuffer}.start_date)
                  AND b{&DetailBuffer}.auth_detail_obj       <> {&DetailBuffer}.auth_detail_obj) 
    THEN DO:
      /* Dont save if we are going to raise an error */
      ASSIGN {&DetailBuffer}.record_action = "":U. 
      
      oErrorObject:addError
            (INPUT "LOS":U,
             INPUT {&DetailBuffer}.auth_detail_obj,
             INPUT "":U,
             INPUT "":U,
             INPUT {&DetailBuffer}.line_number,
             INPUT "Duplicate Subsequent Day Fee not allowed. [HELP=Auth Rule Code: MaternityPerDiemFirst]",
             INPUT "ERR":U).         
    END. /* IF NOT lMaternityPerDiemFirst AND CAN-FIND(FIRST b{&DetailBuffer} */

  END. /*IF AVAILABLE {&DetailBuffer} THEN*/

  { mip/inc/mipcatcherror.i 
        &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}
&ENDIF

