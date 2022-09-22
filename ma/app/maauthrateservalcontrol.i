/* maauthrateservalcontrol.i  MEDSTAR Medical Aid System
                              Validate Auth Rate Control
                              (c) Copyright 2020 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/      
                
  DEFINE PARAMETER BUFFER btt_auth_rate_control FOR tt_auth_rate_control.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL.

&IF {&DBDFMA} >= 10195 &THEN
  

  DEFINE VARIABLE cKeylist            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cCodelist           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cDescriptionList    AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cLabellist          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cSequenceList       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatusDescr        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRuleValue          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValueList          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cEntry              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cPrType             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cSubDisciplineCode  AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE iCnt                AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lMandatory          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidRule          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.  
  
  DEFINE BUFFER buf_auth_rate_control FOR hac_auth_rate_control.
  DEFINE BUFFER buf_auth_type         FOR hac_auth_type.
  
  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_rate_control
  THEN 
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Rate as no buffer is available.'" }
    
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE) .
  
  /* Ensure that a valid insurer has been specified */
  IF btt_auth_rate_control.insurer_obj <> 0.00 AND btt_auth_rate_control.insurer_obj <> ?
  THEN DO:
  
    FIND FIRST erm_insurer NO-LOCK
         WHERE erm_insurer.insurer_obj = btt_auth_rate_control.insurer_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacrc":U,
                            INPUT btt_auth_rate_control.auth_rate_control_obj,
                            INPUT "":U,
                            INPUT "insurer_obj":U,
                            INPUT btt_auth_rate_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Rate Client:":U + STRING(btt_auth_rate_control.insurer_obj)).
    END. /* IF NOT AVAILABLE erm_insurer THEN */
  END. /*IF btt_auth_rate_control.insurer_obj <> 0.00 AND btt_auth_rate_control.insurer_obj <> ?*/

  /*If an scheme has been specified, make sure it exists*/
  IF btt_auth_rate_control.option_code <> 0 THEN
  DO:
    FIND FIRST scheme NO-LOCK 
         WHERE scheme.scheme-code = btt_auth_rate_control.option_code NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
   
    IF NOT AVAILABLE scheme THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "option_code":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Rate Control Scheme Option: ":U + STRING(btt_auth_rate_control.option_code)).                  
    END.  /*IF NOT AVAILABLE scheme THEN*/
  END. /*IF btt_auth_rate_control.option_code <> "" THEN*/  
  
  /* Ensure that a valid auth rate code has been specified */
  IF btt_auth_rate_control.rate_control_code = "":U
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "rate_control_code":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Code,":U ).
  END. /* btt_auth_rate_control.rate_control_code = "":U THEN */

  /* Ensure that a valid auth rate description has been specified */
  IF btt_auth_rate_control.rate_control_description = "":U
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "rate_control_description":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Description,":U ).
  END. /* btt_auth_rate_control.rate_control_description = "":U THEN */

  /*If a main provider negotiation group number has been specified, make sure it exists*/
  IF btt_auth_rate_control.main_provider_neg_num <> 0 THEN
  DO:
    FIND FIRST neggroup NO-LOCK 
         WHERE neggroup.neg-num = btt_auth_rate_control.main_provider_neg_num NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
   
    IF NOT AVAILABLE neggroup THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "main_provider_neg_num":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Main Provider Negotiation Group Number: ":U + STRING(btt_auth_rate_control.main_provider_neg_num)).                  
    END.  /*IF NOT AVAILABLE neggroup THEN*/
  END. /*IF btt_auth_rate_control.main_provider_neg_num <> "" THEN*/    

  /*If a main provider base rate has been specified, make sure it exists*/
  IF btt_auth_rate_control.main_provider_base_rate <> "" THEN
  DO:
    FIND FIRST baserate NO-LOCK 
         WHERE baserate.base-rate = btt_auth_rate_control.main_provider_base_rate NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
   
    IF NOT AVAILABLE baserate THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj,
                            INPUT "":U,
                            INPUT "main_provider_base_rate":U,
                            INPUT btt_auth_rate_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Main Provider Base Rate: ":U + STRING(btt_auth_rate_control.main_provider_base_rate)).
    END.  /*IF NOT AVAILABLE baserate THEN*/
  END. /*IF btt_auth_rate_control.main_provider_base_rate <> "" THEN*/       

  /*If a main provider ars rate has been specified, make sure it exists*/
  IF btt_auth_rate_control.main_provider_ars_rate <> "" THEN
  DO:
    FIND FIRST arsrate NO-LOCK 
         WHERE arsrate.ars-rate = btt_auth_rate_control.main_provider_ars_rate NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
   
    IF NOT AVAILABLE arsrate THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "main_provider_ars_rate":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Main Provider Ars Rate: ":U + STRING(btt_auth_rate_control.main_provider_ars_rate)).                  
    END.  /*IF NOT AVAILABLE arsrate THEN*/
  END. /*IF btt_auth_rate_control.main_provider_ars_rate <> "" THEN*/         
  
  IF  btt_auth_rate_control.main_provider_base_rate = btt_auth_rate_control.override_base_rate
  AND btt_auth_rate_control.main_provider_ars_rate  = btt_auth_rate_control.override_ars_rate  
  AND btt_auth_rate_control.cpt_relative_value_type  = "":U THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE
           lSuccess           = oErrorObject:addError(INPUT "hatau":U,                                                                /* ipcOwningEntityMnemonic  */
                                                      INPUT btt_auth_rate_control.auth_rate_control_obj,                              /* ipdOwningEntityObj       */
                                                      INPUT "":U,                                                                     /* ipcOwningEntityKey       */
                                                      INPUT btt_auth_rate_control.line_number,                                        /* ipiLineNumber            */
                                                      INPUT "The Main Base/ARS rate may not be the same as Override Base/ARS rates.", /* ipcMessageText           */
                                                      INPUT "ERR":U).                                                                 /* ipcMessageType           */

  END. /* IF btt_auth_rate_control.main_provider_base_rate = btt_auth_rate_control.main_provider_ars_rate  */

  /*Ensure that a valid auth rate control effective date has been specified*/
  IF btt_auth_rate_control.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Effective Date,":U) .
  END. /* btt_auth_rate_control.effective_date = ? THEN */

  /* If an end date is supplied, ensure that is not before the effective date */
  IF btt_auth_rate_control.end_date <> ? AND
     btt_auth_rate_control.end_date < btt_auth_rate_control.effective_date THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 11,  /* The End Date &1 cannot be before the Effective/Start Date &2 */
                          INPUT STRING(btt_auth_rate_control.end_date,"9999/99/99") + "," + STRING(btt_auth_rate_control.effective_date,"9999/99/99")).
  END. /*IF btt_auth_rate_control.end_date <> ? AND btt_auth_rate_control.end_date < btt_auth_rate_control.effective_date THEN*/


  /*If override base rate has been specified, make sure it exists*/
  IF btt_auth_rate_control.override_base_rate <> "" THEN
  DO:
    FIND FIRST baserate NO-LOCK 
         WHERE baserate.base-rate = btt_auth_rate_control.override_base_rate NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
   
    IF NOT AVAILABLE baserate THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "override_base_rate":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Override Base Rate: ":U + STRING(btt_auth_rate_control.override_base_rate)).                  
    END.  /*IF NOT AVAILABLE baserate THEN*/
  END. /*IF btt_auth_rate_control.override_base_rate <> "" THEN*/       

  /*If override ars rate has been specified, make sure it exists*/
  IF btt_auth_rate_control.override_ars_rate <> "" THEN
  DO:
    FIND FIRST arsrate NO-LOCK 
         WHERE arsrate.ars-rate = btt_auth_rate_control.override_ars_rate NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
   
    IF NOT AVAILABLE arsrate THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "override_ars_rate":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Override Ars Rate: ":U + STRING(btt_auth_rate_control.override_ars_rate)).                  
    END.  /*IF NOT AVAILABLE arsrate THEN*/
  END. /*IF btt_auth_rate_control.override_ars_rate <> "" THEN*/   

  /*If override auth type obj is specifid, make sure it's valid*/
  IF btt_auth_rate_control.override_auth_type_obj <> 0.00 THEN
  DO:
    IF NOT CAN-FIND(FIRST buf_auth_type NO-LOCK
                    WHERE buf_auth_type.auth_type_obj   = btt_auth_rate_control.override_auth_type_obj
                      AND buf_auth_type.effective_date <= btt_auth_rate_control.effective_date
                      AND buf_auth_type.end_date        = ?)
    THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacrc":U,
                            INPUT btt_auth_rate_control.auth_rate_control_obj,
                            INPUT "":U,
                            INPUT "override_auth_rate_obj":U,
                            INPUT btt_auth_rate_control.line_number,
                            INPUT "MA":U,
                            INPUT 101,  /* The "&1" is not available */
                            INPUT "Override Auth Type ":U).
    END. /* IF NOT CAN-FIND(FIRST buf_auth_type NO-LOCK */     
  END. /* IF btt_auth_rate_control.override_auth_type_obj <> 0.00 THEN */
  
  /*If the workgroup_obj is specified, make sure its valid*/
  IF btt_auth_rate_control.workgroup_obj <> 0.00 THEN
  DO:
    IF NOT CAN-FIND(FIRST hdm_workgroup NO-LOCK
                    WHERE hdm_workgroup.workgroup_obj = btt_auth_rate_control.workgroup_obj)
    THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacrc":U,
                            INPUT btt_auth_rate_control.auth_rate_control_obj,
                            INPUT "":U,
                            INPUT "workgroup_obj":U,
                            INPUT btt_auth_rate_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Workgroup ":U).
    END. /* IF NOT CAN-FIND(FIRST hdm_workgroup NO-LOCK */
  END. /* IF btt_auth_rate_control.workgroup_obj <> 0.00 THEN */
  
  /*Ensure that no unkown value is captured on the revert_rate_change*/
  IF btt_auth_rate_control.revert_rate_change = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
  
    oErrorObject:addError(INPUT "hacrc":U, 
                          INPUT btt_auth_rate_control.auth_rate_control_obj, 
                          INPUT "":U, 
                          INPUT "revert_rate_change":U,
                          INPUT btt_auth_rate_control.line_number, 
                          INPUT "MA":U, 
                          INPUT 112,  /* The "&1" specified is invalid. &2 */
                          INPUT "Revert Rate Change,Cannot be Unknown").
  END. /* IF btt_auth_rate_control.revert_rate_change = ? THEN */
  
  /*If the body region exclusion is specified, make sure its valid*/
  IF btt_auth_rate_control.body_region_exclusion <> "":U THEN
  DO:
    IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
                    WHERE mic_acronym.category_key = "ma_acBodyRegion":U
                      AND mic_acronym.acronym_key  = btt_auth_rate_control.body_region_exclusion)
    THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "body_region_exclusion":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 101,  /* The "&1" specified is invalid. &2 */
                            INPUT "Body region exclusion"). 
    END. /* IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK */
  END. /* IF btt_auth_rate_control.body_region_exclusion <> "":U THEN */
  
  /*If the associated_pr_type_list is specified, make sure the items are valid.*/
  /* Validate pr_type_list */
  /* The pr_type is made of the pr_type(first 3 digits) and the sub_pr_type(last 3 digits) */
  IF btt_auth_rate_control.associated_pr_type_list <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_rate_control.associated_pr_type_list):
  
    ASSIGN
      cEntry             = TRIM(ENTRY(iCnt, btt_auth_rate_control.associated_pr_type_list))
      cPrType            = SUBSTRING(cEntry,1,3)
      cSubDisciplineCode = SUBSTRING(cEntry,4,3).
      
    IF cSubDisciplineCode = "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hacrc":U,
                            INPUT btt_auth_rate_control.auth_rate_control_obj,
                            INPUT "":U,
                            INPUT "associated_pr_type_list":U,
                            INPUT btt_auth_rate_control.line_number,
                            INPUT "MA":U,
                            INPUT 111, /* The &1 must be specified. &2 */
                            INPUT "Sub Discipline for Practice Type ":U + cEntry + ",").
    END.  /* IF SUBSTRING(cEntry,4,3) = "" THEN */
    ELSE IF LENGTH(cEntry) <> 6
    OR NOT CAN-FIND(FIRST prtype NO-LOCK
                    WHERE prtype.pr-type = INTEGER(cPrType))
    OR NOT CAN-FIND(FIRST subdisc NO-LOCK
                    WHERE subdisc.pr-type      = INTEGER(cPrType)
                      AND subdisc.subdisp-code = INTEGER(cSubDisciplineCode)) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacrc":U,
                            INPUT btt_auth_rate_control.auth_rate_control_obj,
                            INPUT "":U,
                            INPUT "associated_pr_type_list":U,
                            INPUT btt_auth_rate_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Rate Practice Types Entry: ":U + cEntry).
    END. /* IF NOT CAN-FIND(FIRST prtype NO-LOCK */
  END. /* IF btt_auth_rate_control.associated_pr_type_list <> "":U THEN */
  
  ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0,
                                                                   INPUT  0,
                                                                   INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                                   INPUT  "CodingMainCode":U,
                                                                   INPUT  btt_auth_rate_control.effective_date,
                                                                   OUTPUT lValidRule,
                                                                   OUTPUT cRuleValue).

  /* 
    Rule doesn't exist
  */
  IF NOT lValidRule OR NOT lSuccess THEN
  DO:          
    IF btt_auth_rate_control.primary_code = ?
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "primary_code":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 112,  /* The "&1" specified is invalid. &2 */
                            INPUT "Primary Code,[HELP=Auth Rule Code: CodingMainCode]").           
    END. /* IF btt_auth_rate_control.primary_code */

    IF btt_auth_rate_control.main_code = ? OR 
       btt_auth_rate_control.main_code
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "main_code":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 112,  /* The "&1" specified is invalid. &2 */
                            INPUT "Main Code,Cannot be Yes [HELP=Auth Rule Code: CodingMainCode]").           
    END. /* IF btt_auth_rate_control.main_code = ? */ 
  END. /* IF NOT lValidRule OR NOT lSuccess THEN */
  ELSE DO:  
    IF btt_auth_rate_control.primary_code = ? OR 
       btt_auth_rate_control.primary_code
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "primary_code":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 112,  /* The "&1" specified is invalid. &2 */
                            INPUT "Primary Code,Cannot be Yes [HELP=Auth Rule Code: CodingMainCode]").           
    END. /* IF btt_auth_rate_control.primary_code = ? */

    IF btt_auth_rate_control.main_code = ?
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrc":U, 
                            INPUT btt_auth_rate_control.auth_rate_control_obj, 
                            INPUT "":U, 
                            INPUT "main_code":U,
                            INPUT btt_auth_rate_control.line_number, 
                            INPUT "MA":U, 
                            INPUT 112,  /* The "&1" specified is invalid. &2 */
                            INPUT "Main Code,[HELP=Auth Rule Code: CodingMainCode]").           
    END. /* IF btt_auth_rate_control.main_code */
  END. /* ELSE - IF NOT lValidRule OR NOT lSuccess THEN */

  IF btt_auth_rate_control.code_link_category = "":U THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
  
    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "code_link_category":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Code Link Category,":U).
  END.  /* IF btt_auth_rate_control.code_link_category = "":U THEN */

  /*As discussed, we will for now hard code on the Code Link Cat 
    We have made a note to build in a rule to control this
  */
  IF  btt_auth_rate_control.code_link_category <> "":U 
  AND btt_auth_rate_control.code_link_category <> "ma_acCodeLinkCatNone":U 
  AND btt_auth_rate_control.code_link_category <> "ma_acCodeLinkCatARSTariffConv":U
  THEN DO:
                                                                  
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "code_link_category":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 112,  /*  The "&1" specified is invalid. &2 */
                          INPUT "Code Link Category,Only ARS Tariff Conversion and None allowed":U).
    
  END. /* IF btt_auth_rate_control.code_link_category <> "":U AND  */
  IF btt_auth_rate_control.main_provider_base_rate = "":U
  AND btt_auth_rate_control.main_provider_neg_num = 0 
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.
  
    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "main_provider_neg_num":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Main Provider Negotiation Number or Main Provider Base Rate,Please enter atleast one value":U).

  END. /*IF btt_auth_rate_control.main_provider_base_rate = "":U AND btt_auth_rate_control.main_provider_neg_num = 0*/
  IF btt_auth_rate_control.override_base_rate = "":U 
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.
    oErrorObject:addError(INPUT "hacrc":U,
                          INPUT btt_auth_rate_control.auth_rate_control_obj,
                          INPUT "":U,
                          INPUT "overrride_base_rate":U,
                          INPUT btt_auth_rate_control.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Override Base Rate,Please enter a value":U).

  END. /*IF btt_auth_rate_control.overrride_base_rate = "":U*/

  IF  btt_auth_rate_control.cpt_relative_value_type <> "":U  THEN
  DO:

    IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
                    WHERE mic_acronym.category_key = "ma_acCptRvu":U
                      AND mic_acronym.acronym_key  = btt_auth_rate_control.cpt_relative_value_type )
    THEN DO:
                                                                    
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacrc":U,
                            INPUT btt_auth_rate_control.auth_rate_control_obj,
                            INPUT "":U,
                            INPUT "cpt_relative_value_type":U,
                            INPUT btt_auth_rate_control.line_number,
                            INPUT "MA":U,
                            INPUT 112,  /*  The "&1" specified is invalid. &2 */
                            INPUT "Cpt Relative value type,Ensure that acronym setup exists for ":U + btt_auth_rate_control.cpt_relative_value_type ).
      
    END. /* IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK WHERE mic_acronym.category_key */

    IF btt_auth_rate_control.override_ars_rate <> "":U 
    THEN
      ASSIGN  oplFailureOccurred = TRUE
              lSuccess           = oErrorObject:addError(INPUT "hacrc":U,
                                                         INPUT btt_auth_rate_control.auth_rate_control_obj,
                                                         INPUT "":U,
                                                         INPUT "override_ars_rate":U,
                                                         INPUT btt_auth_rate_control.line_number,
                                                         INPUT "MA":U,
                                                         INPUT 112,  /*  The "&1" specified is invalid. &2 */
                                                         INPUT "Override ARS Rate,When a Relative value type is set up the override ARS rate needs to be specified on auth rate detail level":U).

    IF btt_auth_rate_control.primary_code 
    OR btt_auth_rate_control.main_code 
    THEN
      ASSIGN oplFailureOccurred = TRUE
             lSuccess           = oErrorObject:addError(INPUT "hacrc":U,
                                                         INPUT btt_auth_rate_control.auth_rate_control_obj,
                                                         INPUT "":U,
                                                         INPUT btt_auth_rate_control.line_number,
                                                         INPUT "MA":U,
                                                         INPUT 112,  /*  The "&1" specified is invalid. &2 */
                                                         INPUT "Main/Primary code,When a Relative value type is set up the primary code will be determined by the CPT Relative value":U).
  END. /*  IF  btt_auth_rate_control.cpt_relative_value_type <> "":U */


{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
        
&ENDIF
