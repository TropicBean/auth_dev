/* maauthtypeservalautpcontrol.i  MEDSTAR Medical Aid System
                                  Validate Auth type Control Buffer
                                  (c) Copyright 2018
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/                      

  
  DEFINE PARAMETER BUFFER btt_auth_type_control    FOR tt_auth_type_control.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL     NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cCodelist           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cDescriptionList    AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cEntry              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cKeylist            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cLabellist          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessage            AS CHARACTER           NO-UNDO. 
  DEFINE VARIABLE cSequenceList       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValueList          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE iCnt                AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.  
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE oAuthTypeDetail     AS cls.mipacronym      NO-UNDO.
  DEFINE VARIABLE oAuthPeriod         As cls.mipacronym      NO-UNDO. 

  DEFINE BUFFER buf_auth_type         FOR hac_auth_type.
  DEFINE BUFFER buf_auth_type_control FOR hac_auth_type_control.

  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_type_control
  THEN
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Type Control as no buffer is available.'" }

  ASSIGN oErrorObject    = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

  /*Ensure that a valid insurer has been specified*/
  IF btt_auth_type_control.auth_type_obj <> 0.00 THEN
  DO:
    FIND FIRST buf_auth_type NO-LOCK
         WHERE buf_auth_type.auth_type_obj = btt_auth_type_control.auth_type_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_auth_type THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "auth_type_obj":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 101,
                            INPUT "Auth Type header ":U).
    END.  /*IF NOT AVAILABLE erm_insurer THEN*/
  END. /*IF btt_auth_type_control.insurer_obj <> 0.00 THEN*/

  /*Ensure that a valid auth type effective date has been specified*/
  IF btt_auth_type_control.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactc":U,
                          INPUT btt_auth_type_control.auth_type_control_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_type_control.line_number,
                          INPUT "MA":U,
                          INPUT 100,
                          INPUT "Auth Type Control Effective Date ":U).
  END. /* btt_auth_type_control.effective_date = ? THEN */
  ELSE DO:
    FIND FIRST buf_auth_type NO-LOCK
         WHERE buf_auth_type.auth_type_obj = btt_auth_type_control.auth_type_obj
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_auth_type AND btt_auth_type_control.effective_date < buf_auth_type.effective_date
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "effective_date":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 112,
                            INPUT "Authorisation type control effective date: " + STRING(btt_auth_type_control.effective_date)
                                  + ", cannot be before the authorisation type effective date: " + STRING(buf_auth_type.effective_date)).

    END. /* IF NOT AVAILABLE buf_auth_type AND btt_auth_type_control.effective_date < buf_auth_type.effective_date  */
  END. /* ELSE DO: */

  /* If an end date is supplied, ensure that is not before the effective date */
  IF btt_auth_type_control.end_date <> ? AND
     btt_auth_type_control.end_date < btt_auth_type_control.effective_date THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hactc":U,
                          INPUT btt_auth_type_control.auth_type_control_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_type_control.line_number,
                          INPUT "MA":U,
                          INPUT 100,
                          INPUT "Auth Type Control End Date ":U).
  END. /* btt_auth_type_control.end_date < btt_auth_type_control.effective_date THEN */

  /*Ensure that a valid Option Code has been specified*/
  IF btt_auth_type_control.option_code <> 0 THEN
  DO:
    FIND FIRST scheme NO-LOCK
         WHERE scheme.scheme-code = btt_auth_type_control.option_code
           AND scheme.active      = TRUE
     NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE scheme THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "option_code":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Option Code ":U + STRING(btt_auth_type_control.option_code)).
    END. /* IF NOT AVAILABLE scheme THEN */
  END. /* btt_auth_type_control.option_code = "":U */

  /*Ensure that a valid insurer has been specified*/
  IF btt_auth_type_control.insurer_obj <> 0.00 THEN
  DO:
    FIND FIRST erm_insurer NO-LOCK
         WHERE erm_insurer.insurer_obj = btt_auth_type_control.insurer_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "insurer_obj":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Client ":U + STRING(btt_auth_type_control.insurer_obj)).
    END.  /*IF NOT AVAILABLE erm_insurer THEN*/
  END. /*IF btt_auth_type_control.insurer_obj <> 0.00 THEN*/

  /* Validate comma delimited list of users */
  IF btt_auth_type_control.usage_override_user <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_control.usage_override_user):

    mipEnv:miUser:focusUser(ENTRY(iCnt, btt_auth_type_control.usage_override_user)) NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'mip_MsgUsrErr:4' &ResetIgnoredErrors = FALSE }

    IF cls.miperror:getMessageGroupNumber() = "mip_MsgUsrErr:4":U
    OR NOT mipEnv:miUser:UserInFocus
    OR mipEnv:miUser:UserStatus <> "mip_StMimusAct":U THEN
    DO:
      cls.miperror:resetError().

      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "usage_override_user":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Usage Override User ":U + STRING(ENTRY(iCnt, btt_auth_type_control.usage_override_user))).
    END. /* IF cls.miperror:getMessageGroupNumber() = "mip_MsgUsrErr:4":U */
  END. /* btt_auth_type_control.usage_override_user <> "":U THEN */


  /* Validate period type */
  IF btt_auth_type_control.period_type <> "":U THEN
  DO:
    FIND FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key = "ma_acAuthPeriod":U
           AND mic_acronym.acronym_key  = btt_auth_type_control.period_type
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "period_type":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Default Period Type ":U + STRING(btt_auth_type_control.period_type)).
    END. /* IF NOT AVAILABLE mic_acronym THEN */
  END. /* btt_auth_type_control.period_type <> "":U THEN */
  
  /* 
    kati begin
  */



  /* Validate usage type */
  IF btt_auth_type_control.usage_type <> "" THEN
  DO:
    FIND FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key = "ma_acAuthTypeUsage":U
           AND mic_acronym.acronym_key  = btt_auth_type_control.usage_type
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactc",
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT "usage_type":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Default Usage Type ":U + STRING(btt_auth_type_control.usage_type)).
    END. /* IF NOT AVAILABLE mic_acronym THEN */
  END. /* btt_auth_type_control.usage_type <> "" THEN */

  IF btt_auth_type_control.usage_quantity <> 0 THEN
  DO:

    ASSIGN cMessage = "".

    IF btt_auth_type_control.usage_period = 0 THEN
      ASSIGN cMessage = "Usage Period ":U.

    IF btt_auth_type_control.usage_period_type = "" THEN
      ASSIGN cMessage = IF cMessage = "" THEN "Usage Period Type ":U ELSE cMessage + " and Usage Period Type ":U.

    IF cMessage <> "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactc",
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT "usage_quantity":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 355,       /*&1 cannot be blank or unknown, please input a correct value.*/
                            INPUT cMessage).
    END. /* IF cMessage <> "" THEN */
  END. /* btt_auth_type_control.usage_quantity <> ""... THEN */

  IF btt_auth_type_control.usage_override_user <> "" THEN
  DO:

    ASSIGN cMessage = "".

    IF btt_auth_type_control.usage_quantity = 0 THEN
      ASSIGN cMessage = "Usage Quantity ":U.

    IF btt_auth_type_control.usage_period = 0 THEN
      ASSIGN cMessage = IF cMessage = "" THEN "Usage Period":U ELSE cMessage + ", Usage Period":U.

    IF btt_auth_type_control.usage_period_type = "" THEN
      ASSIGN cMessage = IF cMessage = "" THEN "Usage Period Type":U ELSE cMessage + ", Usage Period_type":U.

    IF cMessage <> "" THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hactc",
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "",
                            INPUT "usage_quantity":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 355,       /*&1 cannot be blank or unknown, please input a correct value.*/
                            INPUT cMessage).
    END. /* IF cMessage  <> "" THEN */
  END. /* btt_auth_type_control.usage_override_user <> ""... THEN */

   
  /* 
    kati einde
  */
  
  /* Validate usage period type */
  IF btt_auth_type_control.usage_period_type <> "":U THEN
  DO:
    FIND FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key = "ma_acAuthPeriod":U
           AND mic_acronym.acronym_key  = btt_auth_type_control.usage_period_type
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "usage_period_type":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Usage Period Type  ":U + STRING(btt_auth_type_control.usage_period_type)).
    END. /* IF NOT AVAILABLE mic_acronym THEN  */
  END. /* btt_auth_type_control.usage_period_type <> "":U THEN */

  /* Validate the restrictons */
  IF btt_auth_type_control.auth_type_restrictions <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_control.auth_type_restrictions):
    IF NOT CAN-FIND(FIRST buf_auth_type NO-LOCK
                    WHERE buf_auth_type.auth_type = ENTRY(iCnt, btt_auth_type_control.auth_type_restrictions))
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "auth_type_restrictions":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Restrictions ":U + STRING(ENTRY(iCnt, btt_auth_type_control.auth_type_restrictions))).
    END. /* IF NOT CAN-FIND(FIRST buf_auth_type NO-LOCK */
  END. /* btt_auth_type_control.auth_type_restrictions <> "":U THEN */

  /* Validate claim_codes_disallow */
  IF btt_auth_type_control.claim_codes_disallow <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_control.claim_codes_disallow):

    mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_control.option_code,
                                                       INPUT  INTEGER(ENTRY(iCnt,btt_auth_type_control.claim_codes_disallow)),
                                                       INPUT  btt_auth_type_control.effective_date,
                                                       OUTPUT lSuccess,
                                                       OUTPUT cMessage).
    IF NOT lSuccess
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "claim_codes_disallow":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Disallow Claim Code ":U + ENTRY(iCnt,btt_auth_type_control.claim_codes_disallow)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_control.claim_type_control <> "":U THEN */

  /* Validate claim_types_disallow */
  IF btt_auth_type_control.claim_types_disallow <> "":U THEN
  DO iCnt = 1 TO NUM-ENTRIES(btt_auth_type_control.claim_types_disallow):
    mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  ENTRY(iCnt,btt_auth_type_control.claim_types_disallow),
                                                       OUTPUT lSuccess,
                                                       OUTPUT cMessage).
    IF NOT lSuccess
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "claim_types_disallow":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Disallow Claim Type ":U + ENTRY(iCnt,btt_auth_type_control.claim_types_disallow)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_control.claim_type_control <> "":U THEN */



  /* Validate claim_code */
  IF btt_auth_type_control.claim_code <> 0
  THEN DO:

    mipEnv:Health:LimitBusinessLogic:validateClaimCode(INPUT  btt_auth_type_control.option_code,
                                                       INPUT  INTEGER(btt_auth_type_control.claim_code),
                                                       INPUT  btt_auth_type_control.effective_date,
                                                       OUTPUT lSuccess,
                                                       OUTPUT cMessage).

    IF NOT lSuccess
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "claim_code":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Claim Code ":U + STRING(btt_auth_type_control.claim_code)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_control.claim_code   <> 0 */

  /* Validate claim type */
  IF btt_auth_type_control.claim_type <> "":U THEN
  DO :
    mipEnv:Health:LimitBusinessLogic:validateClaimType(INPUT  btt_auth_type_control.claim_type,
                                                       OUTPUT lSuccess,
                                                       OUTPUT cMessage).
    IF NOT lSuccess
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hactc":U,
                            INPUT btt_auth_type_control.auth_type_control_obj,
                            INPUT "":U,
                            INPUT "claim_type":U,
                            INPUT btt_auth_type_control.line_number,
                            INPUT "MA":U,
                            INPUT 100,
                            INPUT "Auth Type Control Claim Type ":U + STRING(btt_auth_type_control.claim_type)).
    END. /* IF NOT lSuccess */
  END. /* IF btt_auth_type_control.claim_type <> "":U THEN */

  { mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject)    THEN DELETE OBJECT oErrorObject.
                IF VALID-OBJECT(oAuthTypeDetail) THEN DELETE OBJECT oAuthTypeDetail.
                IF VALID-OBJECT(oAuthPeriod)     THEN DELETE OBJECT oAuthPeriod."}
/*                                                                                                                                             */
&ENDIF      
