/* maauthbusvalauth.i  MEDSTAR Medical Aid System
                       Validate Auth Buffer
                       (c) Copyright 2019 - 2022
                       MIP Holdings (Pty) Ltd
                       All rights reserved
*/
DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.

DEFINE VARIABLE oAuthDetailLines                    AS cls.mipacronym       NO-UNDO.
DEFINE VARIABLE oCPTSearch                          AS ma.cls.macptsearch   NO-UNDO.
DEFINE VARIABLE oMemberSearch                       AS cls.mamembersearch   NO-UNDO.

DEFINE VARIABLE cAllowDiscountOnUnlimitedRuleValue  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cAuthTypeExclRuleValue              AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cClaimCode                          AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cClaimType                          AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cCodingMainRuleValue                AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cCodingOEM                          AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cCPTCodingType                      AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cCPTMandatoryErrType                AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cCPTMandatoryRuleValue              AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cDefAuthStatus                      AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cDefAuthStatusNote                  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cDiscountType                       AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cDuplCheckStatusRuleValue           AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cErrorMessage                       AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cErrType                            AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cFieldName                          AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cICDCode                            AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cICDCodingType                      AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cLineStatus                         AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cLineType                           AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cMessage                            AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cMessageType                        AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cOldStatusNote                      AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cOptionRuleLoadStatus               AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cPairingRuleValue                   AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cPendingLine                        AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cRuleValue                          AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cServiceType                        AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cStatusDescr                        AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cStatusOvrUser                      AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cTrackingMessage                    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cValidMessage                       AS CHARACTER            NO-UNDO.
DEFINE VARIABLE deTotAmtPaid                        AS DECIMAL              NO-UNDO.
DEFINE VARIABLE deTotQtyPaid                        AS DECIMAL              NO-UNDO.
DEFINE VARIABLE dAuthTypeObj                        AS DECIMAL              NO-UNDO.
DEFINE VARIABLE iCount                              AS INTEGER              NO-UNDO.
DEFINE VARIABLE iFlagNr                             AS INTEGER              NO-UNDO.
DEFINE VARIABLE iLogTime                            AS INTEGER              NO-UNDO.
DEFINE VARIABLE iNrOfDays                           AS INTEGER              NO-UNDO.
DEFINE VARIABLE iOldAuthStatus                      AS INTEGER              NO-UNDO.
DEFINE VARIABLE iOptionCode                         AS INTEGER              NO-UNDO.
DEFINE VARIABLE lAcknowledge                        AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lActivateMainCode                   AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lAgeValid                           AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lAllowDiscountOnUnlimitedValidRule  AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lAuthTypeExclValidRule              AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lAuthUpdatesAllowed                 AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lCodingMainValidRule                AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lCPTMandatoryAcknowledge            AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lCPTMandatoryValidRule              AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lCriticalAuthRule                   AS LOGICAL              NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lDefaultAuthHeadInfo                AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lDependantUpdatable                 AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lDuplCheckStatusValidRule           AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lFoundDuplStatus                    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lMandatory                          AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lPairingValidRule                   AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lPrimaryCodingFound                 AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lProviderSetupPresent               AS LOGICAL              NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lServiceTypeFound                   AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lSuccess                            AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lValidField                         AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lValidRule                          AS LOGICAL              NO-UNDO.
DEFINE VARIABLE lValidStatus                        AS LOGICAL              NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

DEFINE BUFFER bhat_auth           FOR hat_auth.
DEFINE BUFFER buf_auth            FOR hat_auth.         /* This buffer will contain the db-version of btt_auth */
DEFINE BUFFER buf_auth_coding     FOR hat_auth_coding.
DEFINE BUFFER buf_auth_detail     FOR hat_auth_detail.
DEFINE BUFFER buf_auth_provider   FOR hat_auth_provider.
DEFINE BUFFER hat_auth_coding     FOR hat_auth_coding.
DEFINE BUFFER bbt_auth_coding     FOR tt_auth_coding.
DEFINE BUFFER btt_auth_coding     FOR tt_auth_coding.
DEFINE BUFFER bbt_auth_provider   FOR tt_auth_provider.
DEFINE BUFFER btt_auth_provider   FOR tt_auth_provider.
DEFINE BUFFER btt_auth_detail     FOR tt_auth_detail.
DEFINE BUFFER bbt_auth_detail     FOR tt_auth_detail.
DEFINE BUFFER btt_auth_type       FOR tt_auth_type.
DEFINE BUFFER btt_memdep          FOR tt_memdep.

IF btt_auth.record_action = "MODIFY":U THEN
DO:
  ASSIGN cErrorMessage = "".

  FIND buf_auth NO-LOCK
    WHERE buf_auth.auth_obj = btt_auth.auth_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  /*
    We only want to validate the Member Option if it's empty or if the Auth Start-date has changed
  */
  IF btt_auth.option_code = 0
  OR iOptionCode          = 0
  OR (AVAILABLE buf_auth
  AND buf_auth.start_date <> btt_auth.start_date) THEN
  DO:

    ASSIGN iLogTime = ETIME
           lSuccess = mipEnv:Health:maMember:getMemberOption(INPUT  btt_auth.mem_num, INPUT  btt_auth.start_date, OUTPUT iOptionCode)

           cTrackingMessage = "maMember:getMemberOption completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN iLogTime   = ETIME
           lMandatory = mipEnv:Health:AuthBusinessLogic:authOptionMandatory(INPUT iOptionCode)

           cTrackingMessage = "AuthBusinessLogic:authOptionMandatory completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF lMandatory THEN
    DO:
      ASSIGN cErrorMessage = "Option Code,A valid option code must be specified.":U +
                             "[HELP=Auth Rule Code: ReserveLimits]":U
             lSuccess = goErrorObject:addError
                          (INPUT "hatau":U,                                               /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth.auth_obj,                                       /* ipdOwningEntityObj       */
                           INPUT "":U,                                                    /* ipcOwningEntityKey       */
                           INPUT "option_code":U,                                         /* ipcFieldName             */
                           INPUT btt_auth.line_number,                                    /* ipiLineNumber            */
                           INPUT "MA":U,                                                  /* ipcMessageGroup          */
                           INPUT 112,  /* The &1 specified is invalid. &2 */              /* ipiMessageNumber         */
                           INPUT cErrorMessage).                                          /* ipcReplaceTextList       */

      RETURN.
    END.  /* IF lMandatory THEN */
  END.  /* IF btt_auth.option_code = 0 OR (AVAILABLE buf_auth AND buf_auth.start_date <> btt_auth.start_date) THEN */

  /*
     Check 'InvalidSchemeOption'-rule
  */
  IF iOptionCode = 0 THEN
  DO:
    ASSIGN iLogTime = ETIME.

    RUN _validateInvalidSchemeOption IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT-OUTPUT goErrorObject ).

    ASSIGN cTrackingMessage = "_validateInvalidSchemeOption completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
  END.  // IF iOptionCode = 0 THEN
  ELSE
    ASSIGN btt_auth.option_code = iOptionCode.

  FIND FIRST buf_auth_schext NO-LOCK
    WHERE buf_auth_schext.scheme-code = iOptionCode
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF NOT AVAILABLE buf_auth_schext THEN
  DO:
    ASSIGN cTrackingMessage = "buf_auth_schext not available for option " + STRING(iOptionCode).

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
  END.  // IF NOT AVAILABLE buf_auth_schext THEN

  /*
     Find the Auth Type config values for the authorisation
  */
  EMPTY TEMP-TABLE ttAuthTypeConfig.

  ASSIGN iLogTime = ETIME.

  mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                               INPUT-OUTPUT TABLE ttAuthTypeConfig).

  ASSIGN cTrackingMessage = "AuthService:getAuthTypeConfig completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  /*
    Dependant validations
  */
  IF btt_auth.dependant <> 99 THEN
  DO:
    {ma/app/maauthbusvalauthdependant.i}

    /*
      Validating the auth type exclusion setups
    */
    IF   btt_auth.auth_obj <= 0
    OR  (AVAILABLE buf_auth
    AND (buf_auth.dependant  <> btt_auth.dependant
    OR   buf_auth.start_date <> btt_auth.start_date)) THEN
    DO:
      {ma/app/maauthbusvalauthexcl.i}
    END.  // IF btt_auth.auth_obj <= 0...
  END. // IF Dependant <> 99

  IF  btt_auth.dependant           = 99
  AND btt_auth.dependant_reference = "":U THEN
  DO:
    ASSIGN cErrorMessage = "Dependant '" + STRING(btt_auth.dependant,'99')
                         + "',The Dependant reference field must be filled in for Dependant 99":U.
           lSuccess      = goErrorObject:addError
                                (INPUT "hatau":U,                                  /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth.auth_obj,                          /* ipdOwningEntityObj       */
                                 INPUT "":U,                                       /* ipcOwningEntityKey       */
                                 INPUT "dependant":U,                              /* ipcFieldName             */
                                 INPUT btt_auth.line_number,                       /* ipiLineNumber            */
                                 INPUT "MA":U,                                     /* ipcMessageGroup          */
                                 INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                 INPUT cErrorMessage).
    RETURN.
  END. /* IF btt_auth.dependant = 99 AND btt_auth.dependant_reference = "":U THEN */

  /*
     Dependant number validations
  */
  ASSIGN
    dAuthTypeObj        = btt_auth.auth_type_obj
    lDependantUpdatable = mipEnv:Health:AuthBusinessLogic:authDependantUpdatable
                                                          (INPUT btt_auth.insurer_obj,
                                                           INPUT dAuthTypeObj,
                                                           INPUT iOptionCode,
                                                           INPUT btt_auth.start_date,
                                                           INPUT btt_auth.amount_paid,
                                                           INPUT btt_auth.quantity_paid).

  IF btt_auth.dependant = 99 AND NOT lDependantUpdatable THEN
  DO:
    ASSIGN
	  cValidMessage = "The dependant is not updatable for authorisation type.":U
	                + "[HELP=Auth Rule Code: AuthMoveValidTypes]"

          lSuccess      = goErrorObject:addError
                             (INPUT "hatau":U,             /* ipcOwningEntityMnemonic */
                              INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj      */
                              INPUT "":U,                  /* ipcOwningEntityKey      */
                              INPUT "auth_status":U,       /* ipcFieldName            */
                              INPUT btt_auth.line_number,  /* ipiLineNumber           */
                              INPUT cValidMessage,         /* ipcMessageText          */
                              INPUT "ERR":U).              /* ipcMessageType          */
  END. /* IF btt_auth.dependant = 99 AND NOT lDependantUpdatable */

  /*
     Check "SuspendMember"-rule
  */
  ASSIGN iLogTime = ETIME.

  RUN _validateAuthSuspendRule IN TARGET-PROCEDURE ( BUFFER btt_auth,  INPUT "Member", INPUT-OUTPUT goErrorObject ).

  ASSIGN cTrackingMessage = "_validateAuthSuspendRule(Member) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
     Check "SuspendCompany"-rule
  */
  ASSIGN iLogTime = ETIME.

  RUN _validateAuthSuspendRule IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT "Company", INPUT-OUTPUT goErrorObject ).

  ASSIGN cTrackingMessage = "_validateAuthSuspendRule(Company) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    Check for Auth Duplicates
  */
  IF btt_auth._data_load THEN
  DO:
    EMPTY TEMP-TABLE tt_auth_type.
    ASSIGN iLogTime = ETIME

           lSuccess = mipEnv:Health:AuthService:checkForDuplicateAuths(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                                       OUTPUT TABLE tt_auth_type,
                                                                       OUTPUT cMessage)

           cTrackingMessage = "checkForDuplicateAuths() completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

     IF cMessage <> "":U THEN
       lSuccess      = goErrorObject:addError
                                          (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                           INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                           INPUT "":U,                  /* ipcOwningEntityKey       */
                                           INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                           INPUT cMessage,              /* ipcMessageText           */
                                           INPUT "ERR":U).              /* ipcMessageType           */

  END. /* IF btt_auth.data_load THEN */




                               



          

    
  /*
     Validate the claim code on the auth header
  */
  IF btt_auth.option_code <> 0 THEN
  DO:
    ASSIGN
      iLogTime         = ETIME
      lSuccess         = mipEnv:Health:AuthBusinessLogic:validateClaimCode(INPUT  btt_auth.insurer_obj,
                                                                           INPUT  btt_auth.option_code,
                                                                           INPUT  btt_auth.mem_num,
                                                                           INPUT  btt_auth.dependant,
                                                                           INPUT  btt_auth.claim_code,
                                                                           INPUT  btt_auth.auth_type_obj,
                                                                           INPUT  btt_auth.start_date,
                                                                           INPUT  "", /* Provider Type  */
                                                                           INPUT  0,  /* Discipline     */
                                                                           INPUT  0,  /* Sub-Discipline */
                                                                           INPUT  0,  /* Negotiation Number */
                                                                           INPUT  "hat_auth":U , /* auth level */
                                                                           OUTPUT lValidField,
                                                                           OUTPUT cValidMessage)
      cTrackingMessage = "AuthBusinessLogic:validateClaimCode completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF cValidMessage <> ""
    THEN
      ASSIGN cMessageType  = (IF lValidField = TRUE
                              THEN "WAR":U
                              ELSE "ERR":U)
             lSuccess      = goErrorObject:addError
                                     (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                      INPUT "":U,                  /* ipcOwningEntityKey       */
                                      INPUT "claim_code":U,        /* ipcFieldName             */
                                      INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                      INPUT cValidMessage,         /* ipcMessageText           */
                                      INPUT cMessageType).         /* ipcMessageType           */
    /*
      Now that the header validations have run , ensure that the claim codes and types are valid
      according to auth type setups
    */
    RUN _validateAuthClaimCodesAndTypes IN TARGET-PROCEDURE (INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                             INPUT btt_auth.auth_obj ) .    


    ASSIGN lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                                    (INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                     INPUT  "NilPaymentDefaultReason":U,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).

    IF AVAILABLE buf_auth_schext
    AND btt_auth.claim_code = buf_auth_schext.claim-code[1] THEN
    DO:
      IF (AVAILABLE buf_auth AND buf_auth.auth_status <> 6)
      OR NOT AVAILABLE buf_auth
      THEN
        ASSIGN btt_auth.auth_status      = 6  /* Declined */
               btt_auth.auth_status_note = IF lValidRule AND cRuleValue <> "":U
                                           THEN cRuleValue
                                           ELSE btt_auth.auth_status_note.
    END.  /* IF AVAILABLE buf_auth_schext AND btt_auth.claim_code = buf_auth_schext.claim-code[1] THEN */
  END. /* IF btt_auth.option_code <> 0 THEN */


  IF  btt_auth.auth_obj  <= 0
  AND btt_auth.dependant  = 99 THEN
  DO:
    ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                                   (INPUT  btt_auth.insurer_obj,
                                                    INPUT  btt_auth.option_code,
                                                    INPUT  "ma_acAuthRuleTypeAuthCopy":U,
                                                    INPUT  "AuthMoveStatusDefault":U,
                                                    INPUT  btt_auth.start_date,
                                                    OUTPUT lValidRule,
                                                    OUTPUT cRuleValue).
    IF lValidRule
    THEN
      ASSIGN
        btt_auth.auth_status      = INTEGER(ENTRY(1,cRuleValue, "|":U))
        btt_auth.auth_status_note = ENTRY(2,cRuleValue, "|":U).
    ELSE DO:
      ASSIGN
         cValidMessage = "Please ensure that a Default Status and Status Reason has been set up for a 99 Dependant":U
                       + ".[HELP=Auth Rule Code: AuthMoveStatusDefault]"
         lSuccess      = goErrorObject:addError
                                      (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                       INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                       INPUT "":U,                  /* ipcOwningEntityKey       */
                                       INPUT "auth_status":U,       /* ipcFieldName             */
                                       INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                       INPUT cValidMessage,         /* ipcMessageText           */
                                       INPUT "ERR":U).              /* ipcMessageType           */

      RETURN.
    END. /* IF lValidRule */
  END. /* IF btt_auth.auth_obj <= 0 */

  /*
    Auth Status cannot be changed to 'Authorised' for a 99 Dependant
  */
  IF  btt_auth.dependant   = 99
  AND btt_auth.auth_status = 1 THEN
  DO:
    ASSIGN
      cValidMessage = "The Authorisation Status may not be Approved for a 99 Dependant."
      lSuccess      = goErrorObject:addError
                                   (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                    INPUT "":U,                  /* ipcOwningEntityKey       */
                                    INPUT "auth_status":U,       /* ipcFieldName             */
                                    INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                    INPUT cValidMessage,         /* ipcMessageText           */
                                    INPUT "ERR":U).              /* ipcMessageType           */

    RETURN.
  END. /* IF  btt_memdep.dependant = 99 */

  /*
    Update coding and detail lines to modify if dependant change from 99 to valid dependant
  */
  IF   AVAILABLE buf_auth
  AND  buf_auth.dependant  = 99
  AND  btt_auth.dependant <> 99
  AND (CAN-FIND(FIRST hat_auth_coding
                WHERE hat_auth_coding.auth_obj = btt_auth.auth_obj)
  OR   CAN-FIND(FIRST hat_auth_detail
                WHERE hat_auth_detail.auth_obj = btt_auth.auth_obj)) THEN
  DO:
    {ma/app/maauthbusvalauthmovedep.i}
  END. /* IF AVAILABLE buf_auth */

  /*
     Validating the update/change of a dependant number
  */
  IF AVAILABLE buf_auth
  AND buf_auth.dependant <> btt_auth.dependant THEN
  DO:

    ASSIGN iLogTime            = ETIME
           dAuthTypeObj        = btt_auth.auth_type_obj
           lDependantUpdatable = mipEnv:Health:AuthBusinessLogic:authDependantUpdatable
                                                          (INPUT btt_auth.insurer_obj,
                                                           INPUT dAuthTypeObj,
                                                           INPUT iOptionCode,
                                                           INPUT btt_auth.start_date,
                                                           INPUT btt_auth.amount_paid,
                                                           INPUT btt_auth.quantity_paid)

           cTrackingMessage = "AuthBusinessLogic:authDependantUpdatable completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF NOT lDependantUpdatable THEN
    DO:
      ASSIGN cErrorMessage = "Dependant '" + STRING(btt_auth.dependant,'99') + "',The dependant may not be changed.":U +
                             "[HELP=Auth Rule Code: AuthMoveValidTypes]":U
             lSuccess      = goErrorObject:addError
                                 (INPUT "hatau":U,                                  /* ipcOwningEntityMnemonic  */
                                  INPUT btt_auth.auth_obj,                          /* ipdOwningEntityObj       */
                                  INPUT "":U,                                       /* ipcOwningEntityKey       */
                                  INPUT "dependant":U,                              /* ipcFieldName             */
                                  INPUT btt_auth.line_number,                       /* ipiLineNumber            */
                                  INPUT "MA":U,                                     /* ipcMessageGroup          */
                                  INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                  INPUT cErrorMessage).                             /* ipcReplaceTextList       */
      RETURN.
    END.  /* IF NOT lDependantUpdatable THEN */

    /*
      When the Dependant is changed from a valid Dependant to any other Dependant value,
      return an error
    */
    IF buf_auth.dependant <> 99 THEN
    DO:
      ASSIGN cErrorMessage = "Dependant,You may not change the Dependant on an existing Authorisation unless you are changing from a '99' Dependant "
       lSuccess = goErrorObject:addError
                           (INPUT "hatau":U,                                  /* ipcOwningEntityMnemonic  */
                            INPUT btt_auth.auth_obj,                          /* ipdOwningEntityObj       */
                            INPUT "":U,                                       /* ipcOwningEntityKey       */
                            INPUT "dependant":U,                              /* ipcFieldName             */
                            INPUT btt_auth.line_number,                       /* ipiLineNumber            */
                            INPUT "MA":U,                                     /* ipcMessageGroup          */
                            INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                            INPUT cErrorMessage).                             /* ipcReplaceTextList       */
      RETURN.
    END.  /* ELSE DO: IF  buf_auth.dependant = 99 */

    /*
      When the Dependant is changed from a 99 Dependant to a new valid Dependant value
    */
    IF  NOT goErrorObject:CanFind("hatau":U, btt_auth.auth_obj, "":U, "ERR":U)
    AND buf_auth.dependant  = 99
    THEN
      ASSIGN cErrorMessage = "This temporary 99 Dependant Authorisation is being assigned to a valid Dependant number."
             lSuccess = goErrorObject:addError
                                 (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                  INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                  INPUT "":U,                  /* ipcOwningEntityKey       */
                                  INPUT "dependant":U,         /* ipcFieldName             */
                                  INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                  INPUT cErrorMessage,         /* ipcMessageText           */
                                  INPUT "WAR":U,               /* ipcMessageType           */
                                  INPUT lAcknowledge).         /* iplAcknowledge           */
  END. /* IF AVAILABLE buf_auth AND buf_auth.dependant <> btt_auth.dependant THEN */

  IF  btt_auth.dependant = 99 THEN
  DO:
    ASSIGN cErrorMessage = "Please note that the crosswalk, rate change and reguide buttons and functionality will not be active for a dependant 99":U.
           lSuccess      = goErrorObject:addError
                             (INPUT "hatau":U,
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "dependant":U,
                              INPUT btt_auth.line_number,
                              INPUT cErrorMessage,
                              INPUT "WAR":U).

  END. /* IF  btt_auth.dependant = 99 THEN */

  /*
    Check for discounts and validate that the auth discount if entered, is valid
  */
  IF AVAILABLE buf_auth
  AND (btt_auth.discount_auth <> buf_auth.discount_auth
  OR   btt_auth.auth_status   <> buf_auth.auth_status)THEN
  DO:
    IF LOOKUP(STRING(btt_auth.auth_status),"5,6") = 0 THEN
    DO:
      mipEnv:Health:AuthService:checkForDiscounts(INPUT  btt_auth.auth_obj,
                                                  INPUT  0,
                                                  INPUT  0,
                                                  OUTPUT cValidMessage).
      IF cValidMessage <> ""
      THEN
        goErrorObject:addError(INPUT "hatau":U,
                               INPUT btt_auth.auth_obj,
                               INPUT "":U,
                               INPUT "discount_auth":U,
                               INPUT btt_auth.line_number,
                               INPUT cValidMessage,
                               INPUT "ERR":U).
    END.  // IF btt_auth.discount_auth <> 0 THEN
  END.  // IF AVAILABLE buf_auth AND (btt_auth.discount_auth <> buf_auth.discount_auth...

  /*
     Validate the claim type on the auth header
  */
  ASSIGN iLogTime = ETIME.

  ASSIGN lSuccess = mipEnv:Health:AuthBusinessLogic:validateClaimType(INPUT  btt_auth.insurer_obj,
                                                                      INPUT  btt_auth.option_code,
                                                                      INPUT  btt_auth.claim_type,
                                                                      INPUT  btt_auth.auth_type_obj,
                                                                      INPUT  btt_auth.start_date,
                                                                      INPUT  "", /* Provider Type */
                                                                      INPUT  0,  /* Discipline */
                                                                      INPUT  0,  /* Sub-Discipline */
                                                                      INPUT  0,
                                                                      INPUT  "hat_auth":U , /*auth level*/
                                                                      OUTPUT lValidField,
                                                                      OUTPUT cValidMessage).

  ASSIGN cTrackingMessage = "AuthBusinessLogic:validateClaimType completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  IF cValidMessage <> ""
  THEN
    ASSIGN cMessageType = (IF lValidField = TRUE
                           THEN "WAR":U
                           ELSE "ERR":U)

           lSuccess = goErrorObject:addError
                          (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                           INPUT "":U,                  /* ipcOwningEntityKey       */
                           INPUT "claim_type":U,        /* ipcFieldName             */
                           INPUT btt_auth.line_number,  /* ipiLineNumber            */
                           INPUT cValidMessage,         /* ipcMessageText           */
                           INPUT cMessageType).         /* ipcMessageType           */

  IF AVAILABLE ttAuthTypeConfig THEN
  DO:
    /*
      End Date validations
    */
    IF NOT ttAuthTypeConfig.EndDateUpdAllowed
    THEN
      /*
        If End date is not updateable, always derive it from the fixed period set up
      */
      ASSIGN btt_auth.end_date = ttAuthTypeConfig.AuthEndDate.
    ELSE DO:
      /*
        If end date is updatable, but fixed period is set up. 
        Warn the user if the captured end date does not match the fixed period.
      */
      IF btt_auth.end_date <> ttAuthTypeConfig.AuthEndDate
      AND ttAuthTypeConfig.AuthEndDate <> ? THEN 
      DO:
        /* 
          If the Period Override is allowed (True), the date may be updated with a warning.
          If the Period Override is Not allowed (False), prevent the date update and return an error.
        */ 
        IF ttAuthTypeConfig.PeriodOverride THEN
        ASSIGN
          cMessage = "End date is incorrect according to fixed period set ups.":U
          lSuccess = goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                            INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                            INPUT "":U,                  /* ipcOwningEntityKey       */
                                            INPUT "end_date":U,          /* ipcFieldName             */
                                            INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                            INPUT cMessage,              /* ipcMessageText           */
                                            INPUT "WAR":U).
        ELSE  
          ASSIGN cMessage = "End date is incorrect according to fixed period set ups. Update not allowed as Period Override is not set.":U
                 lSuccess = goErrorObject:addError
                                       (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                        INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                        INPUT "":U,                  /* ipcOwningEntityKey       */
                                        INPUT "end_date":U,          /* ipcFieldName             */
                                        INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                        INPUT cMessage,              /* ipcMessageText           */
                                        INPUT "ERR":U).              /* ipcMessageType           */ 
      END.  /* IF btt_auth.end_date <> ttAuthTypeConfig.AuthEndDate */
    END. /* ELSE - IF ttAuthTypeConfig.EndDateUpdAllowed */

    /*
      Auth end date may not be updated for fixed-period type auths
    */
    IF NOT ttAuthTypeConfig.AuthEndDateUpdAllowed             
    AND btt_auth.end_date <> ?
    AND btt_auth.end_date <> ttAuthTypeConfig.AuthEndDate THEN
    DO:
      /* 
        If the Period Override is allowed (True), the Auth date may be updated with a warning.
        If the Period Override is Not allowed (False), prevent the Auth date update and return an error.
      */
      IF ttAuthTypeConfig.PeriodOverride THEN
        ASSIGN
          cMessage = "Authorisation End date is incorrect according to fixed period set ups.":U
          lSuccess = goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                            INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                            INPUT "":U,                  /* ipcOwningEntityKey       */
                                            INPUT "end_date":U,          /* ipcFieldName             */
                                            INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                            INPUT cMessage,              /* ipcMessageText           */
                                            INPUT "WAR":U).
      ELSE 
        ASSIGN cMessage = "Authorisation End date is incorrect according to fixed period set ups. Update not allowed as Period Override is not set.":U
             lSuccess      = goErrorObject:addError
                                       (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                        INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                        INPUT "":U,                  /* ipcOwningEntityKey       */
                                        INPUT "end_date":U,          /* ipcFieldName             */
                                        INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                        INPUT cMessage,              /* ipcMessageText           */
                                        INPUT "ERR":U).              /* ipcMessageType           */
    END.  /* IF NOT ttAuthTypeConfig.AuthEndDateUpdAllowed */
    /*
      Auth Type Configuration Validations
    */
    ASSIGN iLogTime = ETIME.

    mipEnv:Health:AuthService:validateAuthorisedValues(INPUT  btt_auth.amount_auth,
                                                       INPUT  btt_auth.quantity_auth,
                                                       INPUT  ttAuthTypeConfig.ActivateAuthorisedValues,
                                                       INPUT  ttAuthTypeConfig.HeaderValuesUnlimited,
                                                       INPUT  ttAuthTypeConfig.HeaderValuesAllowed,
                                                       OUTPUT cFieldName,
                                                       OUTPUT cValidMessage).

    ASSIGN cTrackingMessage = "AuthService:validateAuthorisedValues completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF cValidMessage <> ""
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                            (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                             INPUT "":U,                  /* ipcOwningEntityKey       */
                             INPUT cFieldName,            /* ipcFieldName             */
                             INPUT btt_auth.line_number,  /* ipiLineNumber            */
                             INPUT cValidMessage,         /* ipcMessageText           */
                             INPUT "ERR":U).              /* ipcMessageType           */

    IF (ttAuthTypeConfig.ActivateLos = FALSE OR ttAuthTypeConfig.Period <> 0)
    AND btt_auth.end_date <> ? AND btt_auth.end_date < btt_auth.start_date
    THEN
      ASSIGN cValidMessage = "The authorisation end date must be greater than or equal to the start date.":U
             lSuccess      = goErrorObject:addError
                                     (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                      INPUT "":U,                  /* ipcOwningEntityKey       */
                                      INPUT "start_date":U,        /* ipcFieldName             */
                                      INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                      INPUT cValidMessage,         /* ipcMessageText           */
                                      INPUT "ERR":U).              /* ipcMessageType           */

    /*
       Validate Auth Type Usage
    */
    IF (ttAuthTypeConfig.UsageType = "":U
    OR AuthTypeRestrictions <> "":U) THEN
    DO:
      ASSIGN iLogTime = ETIME

             lSuccess = mipEnv:Health:AuthBusinessLogic:validateAuthTypeUsage(INPUT  btt_auth.auth_type_obj,
                                                                              INPUT  btt_auth.insurer_obj,
                                                                              INPUT  btt_auth.option_code,
                                                                              INPUT  btt_auth.mem_num,
                                                                              INPUT  btt_auth.dependant,
                                                                              INPUT  btt_auth.start_date,
                                                                              INPUT  "Both":U,
                                                                              INPUT  "",
                                                                              OUTPUT lValidField,
                                                                              OUTPUT cValidMessage).

      ASSIGN cTrackingMessage = "AuthBusinessLogic:validateAuthTypeUsage completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF cValidMessage <> ""
      THEN
        ASSIGN cMessageType = IF lValidField = TRUE
                              THEN "WAR":U
                              ELSE "ERR":U

               lSuccess = goErrorObject:addError
                              (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "",                    /* ipcOwningEntityKey       */
                               INPUT "auth_type_obj":U,     /* ipcFieldName             */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT cValidMessage,         /* ipcMessageText           */
                               INPUT cMessageType).         /* ipcMessageType           */

    END. /* IF ttAuthTypeConfig.UsageType = "":U OR AuthTypeRestrictions <> "":U THEN */

    /*
       Episode Validations
    */
    IF AVAILABLE buf_auth
    AND NOT ttAuthTypeConfig.EpisodeUpdAllowed
    AND buf_auth.auth_episode_obj <> btt_auth.auth_episode_obj
    THEN
      ASSIGN cValidMessage = "The authorisation episode number may not be changed."
             lSuccess      = goErrorObject:addError
                                     (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                      INPUT "":U,                  /* ipcOwningEntityKey       */
                                      INPUT "auth_episode_obj":U,  /* ipcFieldName             */
                                      INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                      INPUT cValidMessage,         /* ipcMessageText           */
                                      INPUT "ERR":U).              /* ipcMessageType           */

    IF ttAuthTypeConfig.ActivateBodyRegion THEN
    DO:
      ASSIGN iLogTime= ETIME.

      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                     INPUT  "BodyRegionDuplicate":U,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).

      ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(BodyRegionDuplicate) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF lValidRule THEN
      DO:
        /*
           Check rule to get the authorisation statuses to be included in the Duplicate Status Check,
           when the Duplicate Body region is validated
        */
        ASSIGN iLogTime = ETIME.
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                       INPUT  "DuplicateCheckStatuses":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lDuplCheckStatusValidRule,
                                                       OUTPUT cDuplCheckStatusRuleValue).

        ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(DuplicateCheckStatuses) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        IF lDuplCheckStatusValidRule THEN
        DO:
          ASSIGN lFoundDuplStatus = FALSE.

          CHKINCLSTATUS:
          FOR EACH bhat_auth NO-LOCK
             WHERE bhat_auth.mem_num          = btt_auth.mem_num
               AND bhat_auth.dependant        = btt_auth.dependant
               AND bhat_auth.body_region      = btt_auth.body_region
               AND bhat_auth.body_region     <> "ma_acBodyRegionNone":U
               AND YEAR(bhat_auth.start_date) = YEAR(btt_auth.start_date)
               AND bhat_auth.auth_obj        <> btt_auth.auth_obj:

             ASSIGN iLogTime = ETIME
                    cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                                  INPUT bhat_auth.auth_status)
                    cStatusDescr = STRING(bhat_auth.auth_status) + "-":U + cStatusDescr.

             ASSIGN cTrackingMessage = "AuthService:getStatusDescription completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

             {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

             IF LOOKUP(cStatusDescr,cDuplCheckStatusRuleValue) <> 0 THEN
             DO:
               ASSIGN lFoundDuplStatus = TRUE.
               LEAVE CHKINCLSTATUS.
             END. /* IF LOOKUP(cStatusDescr,cDuplCheckStatusRuleValue) <> 0 */
          END. /* FOR EACH bhat_auth NO-LOCK */
        END. /* IF lDuplCheckStatusValidRule */
        ELSE
          FIND FIRST bhat_auth NO-LOCK
              WHERE bhat_auth.mem_num          = btt_auth.mem_num
                AND bhat_auth.dependant        = btt_auth.dependant
                AND bhat_auth.body_region      = btt_auth.body_region
                AND bhat_auth.body_region     <> "ma_acBodyRegionNone":U
                AND YEAR(bhat_auth.start_date) = YEAR(btt_auth.start_date)
                AND bhat_auth.auth_obj        <> btt_auth.auth_obj
              NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE bhat_auth
        OR lFoundDuplStatus THEN
        DO:
          IF cRuleValue BEGINS "Warn":U
          THEN
            ASSIGN cErrorMessage = "A possible duplicate authorisation (":U + bhat_auth.auth_num +
                                   ") was found. Please check. Auth will continue.":U.
          ELSE
            ASSIGN cErrorMessage = "A possible duplicate authorisation (":U + bhat_auth.auth_num +
                                   ") was found. Authorisation may not continue.":U.

          ASSIGN iLogTime = ETIME.

          RUN _validateRuleSetup IN TARGET-PROCEDURE ( BUFFER btt_auth,
                                                       INPUT "BodyRegionDuplicate":U,
                                                       INPUT cRuleValue,
                                                       INPUT FALSE,
                                                       INPUT cErrorMessage,
                                                       INPUT-OUTPUT goErrorObject ).

          ASSIGN cTrackingMessage = "_validateRuleSetup completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        END. /* IF AVAILABLE bhat_auth OR lFoundDuplStatus THEN */
      END.  /* IF lValidRule THEN */
    END.  /* IF ttAuthTypeConfig.ActivateBodyRegion THEN */

    /*
      Activate an error or warning if no primary / main procedure (CPT) coding details are captured.
      Rule value can contain 2 possible values, 'Warn' or 'Block'.
      Warn - Will just generate a warning message which will be hidden automatically.
      WarnAck - Will generate a warning message which the user must click to hide.
      Block - Won't allow the authorisation to be completed.
    */
    ASSIGN iLogTime = ETIME.

    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                   INPUT  btt_auth.option_code,
                                                   INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                   INPUT  "CPTMandatory":U,
                                                   INPUT  btt_auth.start_date,
                                                   OUTPUT lCPTMandatoryValidRule,
                                                   OUTPUT cCPTMandatoryRuleValue).

    ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(CPTMandatory) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN cTrackingMessage = "Rule: CPTMandatory - ValidRule? " + STRING(lCPTMandatoryValidRule) + " Rule Value: " + cCPTMandatoryRuleValue.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuthRule'" &LogMessage = cTrackingMessage}

    IF ttAuthTypeConfig.ActivateServiceType THEN
    DO:
      IF NOT lCPTMandatoryValidRule
      OR TRIM(cCPTMandatoryRuleValue) = "":U
      OR TRIM(cCPTMandatoryRuleValue) BEGINS "WARN":U THEN
      DO:
        /*
          If the service type is blank and we don't have any coding records, we default the service type to medical
        */
        IF btt_auth.service_type = "":U
        AND NOT CAN-FIND(FIRST btt_auth_coding NO-LOCK
                         WHERE btt_auth_coding.owning_entity_mnemonic = "hlmck":U
                           AND btt_auth_coding.coding_status         <> 5 )
        THEN
          ASSIGN btt_auth.service_type = "ma_acServiceTypeMedical":U.

        IF AVAILABLE buf_auth THEN
        DO:
          /*
            We only want to validate the auth service type if it has changed
          */
          IF buf_auth.service_type <> btt_auth.service_type THEN
          DO:

            DATASET dsCpt:EMPTY-DATASET().

            ASSIGN oCPTSearch =  NEW ma.cls.macptsearch(DATASET dsCpt:HANDLE).

            /*
              Determine whether we should use the primary or main code to check the service type
            */
            ASSIGN iLogTime = ETIME.

            mipEnv:Health:AuthBusinessLogic:activateMainCode(INPUT btt_auth.insurer_obj,
                                                             INPUT btt_auth.option_code,
                                                             INPUT btt_auth.start_date,
                                                             OUTPUT lActivateMainCode).

            ASSIGN cTrackingMessage = "AuthBusinessLogic:activateMainCode completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            /*
              Try to find the primary/main coding record. We will use the service type of the
              primary/main procedure(CPT) to validate if the service type on the auth is correct
            */
            IF lActivateMainCode
            THEN
              FIND FIRST btt_auth_coding NO-LOCK
                   WHERE btt_auth_coding.auth_obj               = btt_auth.auth_obj
                     AND btt_auth_coding.owning_entity_mnemonic = "hlmck":U
                     AND btt_auth_coding.main_code              = TRUE
                NO-ERROR.
            ELSE
              FIND FIRST btt_auth_coding NO-LOCK
                   WHERE btt_auth_coding.auth_obj               = btt_auth.auth_obj
                     AND btt_auth_coding.owning_entity_mnemonic = "hlmck":U
                     AND btt_auth_coding.primary_code           = TRUE
                NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE btt_auth_coding THEN
            DO:
              /*
                We found a primary/main coding record, now fetch the associating cpt record so we can check the service type.
              */
              ASSIGN
                lPrimaryCodingFound  = TRUE
                lSuccess             = oCPTSearch:SetFilterCriteria("tt_cpt_link.cpt_link_obj":U, "=":U, btt_auth_coding.owning_obj)
                lSuccess             = oCPTSearch:SetCriteria("BufferList":U, "tt_cpt_link,tt_cpt":U). /* Restrict the buffers to be filled by the data service */

              oCPTSearch:fetchCptData().

              /*
                Get the service type from the cpt record with most recent effective date
              */
              CPT-BLK:
              FOR EACH tt_cpt NO-LOCK
                WHERE  tt_cpt.cpt_code        = btt_auth_coding.owning_alt_value
                  AND  tt_cpt.effective_date <= btt_auth_coding.start_date
                  AND (tt_cpt.end_date       >= btt_auth_coding.start_date
                   OR  tt_cpt.end_date        = ?)
                   BY  tt_cpt.effective_date DESCENDING:

                ASSIGN cServiceType = tt_cpt.service_type.

                LEAVE CPT-BLK.
              END. /* CPT-BLK */
            END. /* IF AVAILABLE btt_auth_coding THEN */

            IF  cServiceType <> "":U
            AND btt_auth.service_type <> cServiceType
            THEN
              ASSIGN
                cErrorMessage = "Service Type,The service type must correspond to the service type on the "
                              + (IF lPrimaryCodingFound THEN
                                    IF lActivateMainCode
                                    THEN "Main"
                                    ELSE "Primary"
                                 ELSE "":U)
                              + " Procedure code."
                              + "[HELP=Auth Rule Code: ":U + IF lActivateMainCode
                                                             THEN "CodingMainCode, CPTMandatory]":U
                                                             ELSE "CPTMandatory]":U
                lSuccess      = goErrorObject:addError
                                  (INPUT "hatau":U,                                     /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth.auth_obj,                             /* ipdOwningEntityObj       */
                                   INPUT "":U,                                          /* ipcOwningEntityKey       */
                                   INPUT "service_type":U,                              /* ipcFieldName             */
                                   INPUT btt_auth.line_number,                          /* ipiLineNumber            */
                                   INPUT "MA":U,                                        /* ipcMessageGroup          */
                                   INPUT 112,  /* The &1 specified is invalid. &2 */    /* ipiMessageNumber         */
                                   INPUT cErrorMessage).                                /* ipcReplaceTextList       */

          END. /* IF buf_auth.service_type <> btt_auth.service_type THEN */
        END. /* IF AVAILABLE buf_auth THEN */

        IF btt_auth.service_type = "":U
        THEN
          goErrorObject:addError(INPUT "hatau":U,
                                 INPUT btt_auth.auth_obj,
                                 INPUT "":U,
                                 INPUT "service_type":U,
                                 INPUT btt_auth.line_number,
                                 INPUT "MA":U,
                                 INPUT 111,  /* The &1 must be specified. &2 */
                                 INPUT "Service Type,[HELP=Auth Rule Code: CPTMandatory]").
      END. /* IF NOT lCPTMandatoryValidRule OR... */
    END. /* IF ttAuthTypeConfig.ActivateServiceType THEN */
  END. /* IF AVAILABLE ttAuthTypeConfig */

  /*
    Financial Validations
  */

  /*
   Discount-Type and Discount-Auth Validations
  */
  IF btt_auth.discount_type <> ? THEN
  DO:

    IF (btt_auth.amount_auth   <> 0
    OR  btt_auth.quantity_auth <> 0)
    AND btt_auth.discount_auth <> 0 THEN
    DO:

      ASSIGN lCriticalAuthRule = FALSE
             cErrorMessage     = "The Auth Rule  has not been set up or does not have 'Header' information set up. ":U
                               + "Please check this rule set up before continuing.[HELP=Auth Rule Code: AllowDiscountOnLimited]".

      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                     INPUT  btt_auth.option_code,
                                                     INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                     INPUT  "AllowDiscountOnLimited":U,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).

      IF lValidRule AND (INDEX(cRuleValue, "Header=":U) > 0)
      THEN
        ASSIGN lProviderSetupPresent = TRUE
               lCriticalAuthRule     = TRUE.

    END. /* IF (btt_auth.amount_auth <> 0 OR btt_auth.quantity_auth <> 0) AND btt_auth.discount_auth <> 0 THEN */

    /*
       Below we have a reusable critical rules evaluation section
    */
    IF NOT lCriticalAuthRule
    THEN
      ASSIGN
        lSuccess = goErrorObject:addError(INPUT "hatau":U,
                                          INPUT btt_auth.auth_obj,
                                          INPUT "":U,
                                          INPUT "discount_auth":U,
                                          INPUT btt_auth.line_number,
                                          INPUT cErrorMessage,
                                          INPUT "ERR":U).
    ELSE DO:
      /*
        Discount Percentage validation
      */
      IF btt_auth.discount_type THEN
      DO:
        ASSIGN cDiscountType = " percentage ".

        IF lProviderSetupPresent THEN
        DO:

          ASSIGN cRuleValue = REPLACE(ENTRY(1,SUBSTRING(cRuleValue,INDEX(cRuleValue,"Header=")),"|"),"Header=", "").

          IF NOT LOOKUP(TRIM(cDiscountType),cRuleValue) > 0
          THEN
            ASSIGN
              cErrorMessage     = "Please note that the rule does not support a discount type of" + cDiscountType
                                + "selection. The rule setup for header only supports " + cRuleValue
                                + ".[HELP=Auth Rule Code: AllowDiscountOnLimited]"
              lCriticalAuthRule = FALSE
              lSuccess          = goErrorObject:addError(INPUT "hatau":U,
                                                         INPUT btt_auth.auth_obj,
                                                         INPUT "":U,
                                                         INPUT "discount_auth":U,
                                                         INPUT btt_auth.line_number,
                                                         INPUT cErrorMessage,
                                                         INPUT "ERR":U).
        END. /* IF lProviderSetupPresent THEN */

        IF  btt_auth.discount_auth > 100
        AND lCriticalAuthRule
        THEN
          ASSIGN
            cErrorMessage = "Discount percentage value can not be greater than 100."
                          + "[HELP=Auth Rule Code: AllowDiscountOnLimited]"
            lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                                   INPUT btt_auth.auth_obj,
                                                   INPUT "":U,
                                                   INPUT "discount_auth":U,
                                                   INPUT btt_auth.line_number,
                                                   INPUT cErrorMessage,
                                                   INPUT "ERR":U).
      END. /* IF btt_auth.discount_type = TRUE */

      /*
        Rand discount validation
      */
      IF btt_auth.discount_type = FALSE THEN
      DO:
        ASSIGN cDiscountType = " amount ".

        IF lProviderSetupPresent THEN
        DO:

          ASSIGN cRuleValue = REPLACE(ENTRY(1,SUBSTRING(cRuleValue,INDEX(cRuleValue,"Header=")),"|"),"Header=", "").

          IF NOT LOOKUP(TRIM(cDiscountType),cRuleValue) > 0
          THEN
            ASSIGN
              cErrorMessage     = "Please note that the rule does not support a discount type of" + cDiscountType +
                                  "selection. The rule setup for header only supports " + cRuleValue
                                + ".[HELP=Auth Rule Code: AllowDiscountOnLimited]"
              lCriticalAuthRule = FALSE
              lSuccess          = goErrorObject:addError(INPUT "hatau":U,
                                                         INPUT btt_auth.auth_obj,
                                                         INPUT "":U,
                                                         INPUT "discount_auth":U,
                                                         INPUT btt_auth.line_number,
                                                         INPUT cErrorMessage,
                                                         INPUT "ERR":U).
        END. /* IF lProviderSetupPresent THEN */

        IF  btt_auth.discount_auth > btt_auth.amount_auth
        AND btt_auth.amount_auth   > 0
        AND lCriticalAuthRule
        THEN
          ASSIGN
            cErrorMessage = "Discount Amount can not be greater than the Authorised Amount."
                          + "[HELP=Auth Rule Code: AllowDiscountOnLimited]"
            lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                                   INPUT btt_auth.auth_obj,
                                                   INPUT "":U,
                                                   INPUT "discount_auth":U,
                                                   INPUT btt_auth.line_number,
                                                   INPUT cErrorMessage,
                                                   INPUT "ERR":U).
      END. /* btt_auth.discount_type = FALSE */

      IF btt_auth.discount_auth <= 0 AND lCriticalAuthRule
      THEN
        ASSIGN
          cErrorMessage = "Discount" + cDiscountType + "value cannot be less than or equal to 0, "
                        + "amount will be reverted to previous value if available."
                        + "[HELP=Auth Rule Code: AllowDiscountOnLimited]"
          lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                                 INPUT btt_auth.auth_obj,
                                                 INPUT "":U,
                                                 INPUT "discount_auth":U,
                                                 INPUT btt_auth.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).

      /*
        Check the rule to see whether we should return an error or a warning.
      */
      IF  btt_auth.amount_auth   = 0
      AND btt_auth.quantity_auth = 0
      AND btt_auth.discount_auth > 0
      AND lCriticalAuthRule THEN
      DO:
        ASSIGN cErrorMessage = "An authorisation discount" + cDiscountType.
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                       INPUT  "AllowDiscountOnUnlimited":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lAllowDiscountOnUnlimitedValidRule,
                                                       OUTPUT cAllowDiscountOnUnlimitedRuleValue).

        /*
          If Rule = Allow, return a WARNING to the user
          If Rule = Block, return an error to the user
          If Rule is not set up, treat as Block, return an error to the user
        */
        IF  lAllowDiscountOnUnlimitedValidRule
        AND cAllowDiscountOnUnlimitedRuleValue = "Allow":U
        THEN
          ASSIGN
            cErrorMessage = cErrorMessage + "has been specified on an Unlimited Authorisation (Authorised Amount & Quantity is zero)."
            cMessageType  = "WAR":U.
        ELSE
          ASSIGN
            cMessageType  = "ERR":U.

        ASSIGN
          cErrorMessage = cErrorMessage + "[HELP=Auth Rule Code: AllowDiscountOnUnlimited, AllowDiscountOnLimited]"
          lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                                 INPUT btt_auth.auth_obj,
                                                 INPUT "":U,
                                                 INPUT "discount_auth":U,
                                                 INPUT btt_auth.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT cMessageType).
      END. /* IF  btt_auth.amount_auth = 0 ... */
    END. /* Else - IF NOT lCriticalAuthRule */
  END. /* IF btt_auth.discount_type <> ? THEN */

  /*
     Check if PMB Changes are allowed
  */
  /* PMB Validations */
  IF AVAILABLE buf_auth
  AND btt_auth.pmb_indicator <> buf_auth.pmb_indicator THEN
  DO:
    /* All associated services are required to be authorised */
    IF btt_auth.authorise_all_services
    THEN
      ASSIGN
        cErrorMessage = "PMB Indicator can't be updated on the Authorisation Header because "
                      + "PMB Indicator on providers will apply."
        lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                               INPUT btt_auth.auth_obj,
                                               INPUT "":U,
                                               INPUT "pmb_indicator":U,
                                               INPUT btt_auth.line_number,
                                               INPUT cErrorMessage,
                                               INPUT "ERR":U).
  END.  /* IF btt_auth.pmb_indicator <> buf_auth.pmb_indicator THEN */

  IF btt_auth.pmb_indicator <> YES THEN
  DO:
    IF btt_auth.pmb_benefit_% <> 0.00
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                          (INPUT "hatau":U ,                                              /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth.auth_obj,                                       /* ipdOwningEntityObj       */
                           INPUT "":U,                                                    /* ipcOwningEntityKey       */
                           INPUT "pmb_benefit_%":U,                                       /* ipcFieldName             */
                           INPUT btt_auth.line_number,                                    /* ipiLineNumber            */
                           INPUT "MA":U,                                                  /* ipcMessageGroup          */
                           INPUT 112,  /* The &1 specified is invalid. &2 */              /* ipiMessageNumber         */
                           INPUT "PMB Benefit Percentage,Update on the field is not allowed":U). /* ipcReplaceTextList*/

    IF btt_auth.pmb_pay_cost = YES
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                          (INPUT "hatau":U ,                                              /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth.auth_obj,                                       /* ipdOwningEntityObj       */
                           INPUT "":U,                                                    /* ipcOwningEntityKey       */
                           INPUT "pmb_pay_cost":U,                                        /* ipcFieldName             */
                           INPUT btt_auth.line_number,                                    /* ipiLineNumber            */
                           INPUT "MA":U,                                                  /* ipcMessageGroup          */
                           INPUT 112,  /* The &1 specified is invalid. &2 */              /* ipiMessageNumber         */
                           INPUT "PMB Pay Cost,Update on the field is not allowed":U).    /* ipcReplaceTextList       */

  END. /* IF btt_auth.pmb_indicator <> YES THEN */
  ELSE DO: /* IF pmb_indicator = yes OR pmb_indicator = ? */
    IF btt_auth.pmb_benefit_% < 0.00
    THEN
      ASSIGN
        cValidMessage = "Please enter PMB Benefit Percentage value greater than ~"0.00%~"."
        lSuccess      = goErrorObject:addError
                                    (INPUT "hatau":U,                                                   /* ipcOwningEntityMnemonic  */
                                     INPUT btt_auth.auth_obj,                                           /* ipdOwningEntityObj       */
                                     INPUT "":U,                                                        /* ipcOwningEntityKey       */
                                     INPUT "pmb_benefit_%":U,                                           /* ipcFieldName             */
                                     INPUT btt_auth.line_number,                                        /* ipiLineNumber            */
                                     INPUT "MA":U,                                                      /* ipcMessageGroup          */
                                     INPUT 112,  /* The &1 specified is invalid. &2 */                  /* ipiMessageNumber         */
                                     INPUT "PMB Benefit Percentage or PMB pay Cost," + cValidMessage ). /* ipcReplaceTextList       */

    ELSE IF  btt_auth.pmb_benefit_% > 0.00
    AND btt_auth.pmb_pay_cost  = YES
    THEN
      ASSIGN
        cValidMessage = "Either enter PMB Benefit Percentage value greater than ~"0.00%~" or check the ~"PMB Pay Cost~" box."
        lSuccess      = goErrorObject:addError
                                    (INPUT "hatau":U ,                                   /* ipcOwningEntityMnemonic  */
                                     INPUT btt_auth.auth_obj,                            /* ipdOwningEntityObj       */
                                     INPUT "":U,                                         /* ipcOwningEntityKey       */
                                     INPUT IF AVAILABLE buf_auth
                                           AND buf_auth.pmb_pay_cost <> btt_auth.pmb_pay_cost
                                           THEN "pmb_pay_cost":U
                                           ELSE IF AVAILABLE buf_auth
                                             THEN "pmb_benefit_%":U
                                             ELSE "":U,                                  /* ipcFieldName             */
                                     INPUT btt_auth.line_number,                         /* ipiLineNumber            */
                                     INPUT "MA":U,                                       /* ipcMessageGroup          */
                                     INPUT 112,  /* The &1 specified is invalid. &2 */   /* ipiMessageNumber         */
                                     INPUT "PMB Benefit Percentage and PMB pay Cost," + cValidMessage ).
  END. /* ELSE IF pmb_indicator = yes */

  /*
    Get all the display flags for the member and create warnings for each of them
  */
  RUN _DisplayFlags (INPUT btt_auth.insurer_obj,
                     INPUT btt_auth.option_code,
                     INPUT "M":U,
                     INPUT btt_auth.mem_num,
                     INPUT btt_auth.start_date,
                     INPUT "hatau":U,
                     INPUT btt_auth.auth_obj,
                     INPUT btt_auth.line_number).

  /*
    Limit Checking
  */
  { ma/app/maauthbusvalauthchecklimits.i }

  /*
     Ensure that only a valid auth status is used according to system and rule setups
  */
  ASSIGN iLogTime = ETIME

         lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  btt_auth.auth_status,
                                                                     INPUT  "System":U).

  ASSIGN cTrackingMessage = "AuthService:validateAuthStatus completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  IF NOT lValidStatus
  THEN
    ASSIGN cErrorMessage = "Status Code (" + STRING(btt_auth.auth_status) + "),[HELP=Auth Rule Code: ValidStatuses]"
           lSuccess      = goErrorObject:addError
                                        (INPUT "hatau":U,                                      /* ipcOwningEntityMnemonic */
                                         INPUT btt_auth.auth_obj,                              /* ipdOwningEntityObj      */
                                         INPUT "":U,                                           /* ipcOwningEntityKey      */
                                         INPUT "auth_status":U,                                /* ipcFieldName            */
                                         INPUT btt_auth.line_number,                           /* ipiLineNumber           */
                                         INPUT "MA":U,                                         /* ipcMessageGroup         */
                                         INPUT 112,  /* The "&1" specified is invalid. &2 */   /* ipiMessageNumber        */
                                         INPUT cErrorMessage).                                 /* ipcReplaceTextList      */
  ELSE DO:
    /*
       Compare the 'old' status against the 'new' status to see if the auth status may be
       changed to the 'new' status.
    */
    IF AVAILABLE buf_auth THEN
    DO:
      IF buf_auth.auth_status <> btt_auth.auth_status THEN
      DO:
        IF btt_auth.amount_paid <> 0 OR btt_auth.quantity_paid <> 0
        THEN
          ASSIGN cErrorMessage = "The Authorisation Header Status cannot be changed, "
                               + "as claims have already been paid against this Authorisation."
                 lSuccess      = goErrorObject:addError
                                         (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                          INPUT "":U,                  /* ipcOwningEntityKey       */
                                          INPUT "auth_status":U,       /* ipcFieldName             */
                                          INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                          INPUT cErrorMessage,         /* ipcMessageText           */
                                          INPUT "ERR":U)               /* ipcMessageType           */
                 lValidStatus  = FALSE.
        ELSE DO:
          ASSIGN
            iLogTime      = ETIME
            cErrorMessage = mipEnv:Health:AuthService:validateAuthStatusUpdate(INPUT  btt_auth.insurer_obj,
                                                                               INPUT  btt_auth.option_code,
                                                                               INPUT  buf_auth.auth_status,
                                                                               INPUT  buf_auth.auth_status_note,
                                                                               INPUT  btt_auth.auth_status,
                                                                               INPUT  btt_auth.amount_paid,
                                                                               INPUT  btt_auth.quantity_paid,
                                                                               INPUT  btt_auth.start_date,
                                                                               INPUT  0,
                                                                               INPUT  "").

          ASSIGN cTrackingMessage = "AuthService:validateAuthStatusUpdate completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          IF cErrorMessage <> "":U
          THEN
            ASSIGN lSuccess     = goErrorObject:addError
                                           (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                            INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                            INPUT "":U,                  /* ipcOwningEntityKey       */
                                            INPUT "auth_status":U,       /* ipcFieldName             */
                                            INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                            INPUT cErrorMessage,         /* ipcMessageText           */
                                            INPUT "ERR":U)               /* ipcMessageType           */
                   lValidStatus = FALSE.
          ELSE
            ASSIGN iOldAuthStatus = buf_auth.auth_status
                   cOldStatusNote = buf_auth.auth_status_note.

        END. /* ELSE DO: IF btt_auth.amount_paid <> 0 OR btt_auth.quantity_paid <> 0 */
      END.  /* IF buf_auth.auth_status <> btt_auth.auth_status THEN */
      ELSE
        ASSIGN iOldAuthStatus = buf_auth.auth_status
               cOldStatusNote = buf_auth.auth_status_note.
    END.  /* IF AVAILABLE buf_auth THEN */
    ELSE
      ASSIGN iOldAuthStatus = 0
             cOldStatusNote = "".

    IF lValidStatus THEN
    DO:
      ASSIGN cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                           INPUT btt_auth.auth_status)
             cStatusDescr = STRING(btt_auth.auth_status) + "-":U + cStatusDescr.

      /*
        If the auth status is changed to 'Declined', we need to change
        the claim code to '99'.
      */
      IF btt_auth.auth_status = 6 /* Declined */ THEN
      DO:
        ASSIGN cTrackingMessage = "buf_auth_schext NOT AVAILABLE for declined auth claim code update - Auth Claim Code = "
                                + STRING(btt_auth.claim_code,"999").

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        IF (    AVAILABLE buf_auth_schext AND btt_auth.claim_code <> buf_auth_schext.claim-code[1])
        OR (NOT AVAILABLE buf_auth_schext AND btt_auth.claim_code <> 99) /* Claim code = 99 */
        THEN
          ASSIGN btt_auth.claim_code = IF AVAILABLE buf_auth_schext
                                       THEN buf_auth_schext.claim-code[1]
                                       ELSE 99.
      END.  /* IF btt_auth.auth_status = 6 /* Declined */ THEN */

      IF btt_auth.auth_status <> iOldAuthStatus THEN
      DO:
        /*
          If the auth status was changed from 'Declined' to any other valid status, we need
          to change the claim code and claim type back to the original values.
        */
        IF iOldAuthStatus = 6 /* Declined */ THEN
        DO:
          ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                (INPUT  btt_auth.insurer_obj,
                                 INPUT  btt_auth.option_code,
                                 INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                 INPUT  "Update6Declined":U,
                                 INPUT  btt_auth.start_date,
                                 OUTPUT lValidRule,
                                 OUTPUT cRuleValue).

          IF LOOKUP(cStatusDescr,cRuleValue) > 0
          THEN
            ASSIGN btt_auth.claim_code = btt_auth.default_claim_code
                   btt_auth.claim_type = btt_auth.default_claim_type.
        END. /* ELSE IF iOldAuthStatus = 6 /* Declined */ THEN */

        /*
           If the auth status on the header is changed, then check rule to see if the status on the
           coding, provider and provider detail lines must also be changed.
        */
        ASSIGN iLogTime = ETIME
               lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                              (INPUT  btt_auth.insurer_obj,
                               INPUT  btt_auth.option_code,
                               INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                               INPUT  "DefaultAuthHeadInfo":U,
                               INPUT  btt_auth.start_date,
                               OUTPUT lValidRule,
                               OUTPUT cRuleValue).

        ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(DefaultAuthHeadInfo) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        IF lValidRule THEN
        DO:
          ASSIGN cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                               INPUT btt_auth.auth_status)
                 cStatusDescr = STRING(btt_auth.auth_status) + "-":U + cStatusDescr.

          IF LOOKUP(cStatusDescr,cRuleValue) > 0
          THEN
            ASSIGN lDefaultAuthHeadInfo = TRUE.
          ELSE
            ASSIGN lDefaultAuthHeadInfo = FALSE.
        END. /* IF lValidRule THEN */
        ELSE
          ASSIGN lDefaultAuthHeadInfo = FALSE.

        IF lDefaultAuthHeadInfo THEN
        DO:
          ASSIGN iLogTime = ETIME.

          RUN _updateAuthLineStatus IN TARGET-PROCEDURE (BUFFER btt_auth,
                                                         INPUT iOldAuthStatus,
                                                         INPUT cOldStatusNote).

          ASSIGN cTrackingMessage = "_updateAuthLineStatus completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END.  /* IF lDefaultAuthHeadInfo THEN  */

        /*
           Check if the status reason/note on the auth is mandatory
        */
        IF btt_auth.auth_status_note = "" THEN
        DO:
          ASSIGN iLogTime = ETIME

                 lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  btt_auth.auth_status,
                                                                              INPUT  btt_auth.insurer_obj,
                                                                              INPUT  btt_auth.option_code,
                                                                              INPUT  btt_auth.start_date).

          ASSIGN cTrackingMessage = "AuthService:statusReasonMandatory completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          IF lMandatory
          THEN
            ASSIGN cStatusDescr  = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                                  INPUT btt_auth.auth_status)
                   cErrorMessage = "Status Reason,The reason is mandatory for status ":U + cStatusDescr
                                 + ".[HELP=Auth Rule Code: EnforceStatusNote]"
                   lSuccess      = goErrorObject:addError
                                                 (INPUT "hatau":U,                                /* ipcOwningEntityMnemonic  */
                                                  INPUT btt_auth.auth_obj,                        /* ipdOwningEntityObj       */
                                                  INPUT "":U,                                     /* ipcOwningEntityKey       */
                                                  INPUT "auth_status_note":U,                     /* ipcFieldName             */
                                                  INPUT btt_auth.line_number,                     /* ipiLineNumber            */
                                                  INPUT "MA":U,                                   /* ipcMessageGroup          */
                                                  INPUT 111,  /* The &1 must be specified. &2 */  /* ipiMessageNumber         */
                                                  INPUT cErrorMessage).                           /* ipcReplaceTextList       */
        END.  /* IF btt_auth.auth_status_note = "" THEN */
      END.  /* IF btt_auth.auth_status <> iOldAuthStatus THEN */

      /*
         If the header's status is 1-Authorised, we need to make sure that
         pending/requested detail lines are allowed.
      */
      IF btt_auth.auth_status = 1 THEN
      DO:
        FIND FIRST bbt_auth_provider NO-LOCK
          WHERE bbt_auth_provider.auth_obj      = btt_auth.auth_obj
          AND   bbt_auth_provider.main_provider = TRUE
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF  AVAILABLE bbt_auth_provider
        AND AVAILABLE buf_auth
        AND buf_auth.request_date <> btt_auth.request_date
        THEN
          ASSIGN bbt_auth_provider._request_date_updated = TRUE
                 bbt_auth_provider.record_action = "MODIFY":U.

        /*
           If the "AuthHead1Authorised"-rule is activated, then an auth
           header may be authorised even if there are pending lines,
           UNLESS the main provider is not yet authorised.
        */
        ASSIGN iLogTime = ETIME

               lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                  (INPUT  btt_auth.insurer_obj,
                                   INPUT  btt_auth.option_code,
                                   INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                   INPUT  "AuthHead1Authorised":U,
                                   INPUT  btt_auth.start_date,
                                   OUTPUT lValidRule,
                                   OUTPUT cRuleValue).

        ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(AuthHead1Authorised) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        IF lValidRule
        AND LOOKUP(cRuleValue,"Activate,Active,Activated,Yes,True,Y,T") > 0 THEN
        DO:
          IF AVAILABLE bbt_auth_provider
          AND bbt_auth_provider.auth_status <> 1
          THEN
            ASSIGN cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                                 INPUT bbt_auth_provider.auth_status)
                   lSuccess = goErrorObject:addError
                                  (INPUT "hatau":U,                                         /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth.auth_obj,                                 /* ipdOwningEntityObj       */
                                   INPUT "":U,                                              /* ipcOwningEntityKey       */
                                   INPUT "auth_status":U,                                   /* ipcFieldName             */
                                   INPUT btt_auth.line_number,                              /* ipiLineNumber            */
                                   INPUT "ma_MsgAuth":U,                                    /* ipcMessageGroup          */
                                   INPUT 16,  /* The &1 can't be authorised if there are &2 &3 lines. Attend to the &2 lines first and then try again. */
                                   INPUT "auth header," + cStatusDescr + ",Main Provider"). /* ipcReplaceTextList       */

          FIND FIRST  bbt_auth_coding NO-LOCK
               WHERE  bbt_auth_coding.auth_obj     = btt_auth.auth_obj
                 AND (bbt_auth_coding.primary_code = TRUE
                  OR  bbt_auth_coding.main_code    = TRUE)
               NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE bbt_auth_coding
          AND bbt_auth_coding.coding_status <> 1
          THEN
            /*
               Auth Message 16 = "The &1 can't be authorised if there are &2 &3 lines.
                                  Attend to the &2 lines first and then try again."
            */
            ASSIGN cStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                                 INPUT bbt_auth_coding.coding_status)
                   lSuccess = goErrorObject:addError
                                  (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                   INPUT "":U,                  /* ipcOwningEntityKey       */
                                   INPUT "auth_status":U,       /* ipcFieldName             */
                                   INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                   INPUT "ma_MsgAuth":U,        /* ipcMessageGroup          */
                                   INPUT 16,  /* The &1 can't be authorised if there are &2 &3 lines. Attend to the &2 lines first and then try again. */
                                   INPUT "auth header," + cStatusDescr + ",Main/Primary ICD/CPT"). /* ipcReplaceTextList       */
        END. /* IF lValidRule ("AuthHead1Authorised") AND ... */
        ELSE DO:
          /*
             The auth header may not be authorised if there are any requested/pending lines.
             Make sure there are no pending, cancelled, declined or requested primary/main lines
          */
          ASSIGN iLogTime = ETIME.

          RUN _CheckLineStatus (btt_auth.auth_obj, OUTPUT cLineStatus, OUTPUT cLineType).

          ASSIGN cTrackingMessage = "_CheckLineStatus completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          IF cLineType <> "":U
          THEN
            ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatau":U,                                               /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth.auth_obj,                                       /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                    /* ipcOwningEntityKey       */
                                   INPUT "auth_status":U,                                         /* ipcFieldName             */
                                   INPUT btt_auth.line_number,                                    /* ipiLineNumber            */
                                   INPUT "ma_MsgAuth":U,                                          /* ipcMessageGroup          */
                                   INPUT 16,  /* The &1 can't be authorised if there are &2 &3 lines. Attend to the &2 lines first and then try again. */
                                   INPUT "auth header," + lower(cLineStatus) + "," + cLineType).  /* ipcReplaceTextList       */
        END.  /* ELSE: IF lValidRule ("AuthHead1Authorised") AND ... */
      END.  /* ELSE: IF btt_auth.auth_status = 1 THEN */
    END.  /* IF lValidStatus THEN */
  END. /* ELSE: IF NOT lValidStatus THEN */

  /*
    Validate status reason if specified
  */
  IF btt_auth.auth_status_note <> "":U THEN
  DO:
    ASSIGN iLogTime = ETIME.

    mipEnv:Health:AuthService:ValidateStatusReason(INPUT  btt_auth.insurer_obj,
                                                   INPUT  00,
                                                   INPUT  btt_auth.auth_status_note,
                                                   INPUT  INTEGER(btt_auth.auth_status),
                                                   INPUT  btt_auth.start_date,
                                                   OUTPUT cValidMessage).

    ASSIGN cTrackingMessage = "AuthService:ValidateStatusReason completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF cValidMessage <> "":U
    THEN
      goErrorObject:addError(INPUT "hatau":U,
                            INPUT btt_auth.auth_obj,
                            INPUT "":U,
                            INPUT "auth_status_note":U,
                            INPUT btt_auth.line_number,
                            INPUT cValidMessage,
                            INPUT "ERR":U).
  END. /* IF btt_auth.auth_status_note <> "":U */

  /*
    Validate global auth limits
      -amount
      -quantity
  */
 {ma/app/maauthbusvalauthlimcntrl.i}
  /*
    Auth is Complete
  */
  IF NOT btt_auth.auth_incomplete THEN
  DO:
    /*
      The auth end date is mandatory when the auth status is completed.
    */
    IF btt_auth.end_date = ?
    THEN
      ASSIGN cValidMessage = "The authorisation end date is mandatory when auth is completed.":U
             lSuccess      = goErrorObject:addError
                                     (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                      INPUT "":U,                  /* ipcOwningEntityKey       */
                                      INPUT "end_date":U,          /* ipcFieldName             */
                                      INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                      INPUT cValidMessage,         /* ipcMessageText           */
                                      INPUT "ERR":U).              /* ipcMessageType           */

    /*
      Amount auth/paid validation
    */
    IF  btt_auth.amount_paid <> 0.00
    AND btt_auth.amount_auth <> 0.00
    AND btt_auth.amount_auth < btt_auth.amount_paid
    THEN
      goErrorObject:addError(INPUT "hatau":U,
                             INPUT btt_auth.auth_obj,
                             INPUT "":U,
                             INPUT "amount_paid":U,
                             INPUT btt_auth.line_number,
                             INPUT "ma_MsgAuth":U,
                             INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                             INPUT "Amount Paid," + STRING(btt_auth.amount_paid) + ",Amount Authorised,":U + STRING(btt_auth.amount_auth)).

    /*
      Quantity auth/paid validation
    */
    IF  btt_auth.quantity_paid <> 0.00
    AND btt_auth.quantity_auth <>  0.00
    AND btt_auth.quantity_auth < btt_auth.quantity_paid
    THEN
      goErrorObject:addError(INPUT "hatau":U,
                             INPUT btt_auth.auth_obj,
                             INPUT "":U,
                             INPUT "quantity_paid":U,
                             INPUT btt_auth.line_number,
                             INPUT "ma_MsgAuth":U,
                             INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                             INPUT "Quantity Paid," + STRING(btt_auth.quantity_paid) + ",Quantity Authorised,":U + STRING(btt_auth.quantity_auth)).

    /*
      Check whether any claims have been paid before allowing changes
    */
    IF AVAILABLE buf_auth THEN
    DO:
      IF btt_auth.amount_paid   <> 0
      OR btt_auth.quantity_paid <> 0 THEN
      DO:
        /*
          The claim code may not be changed if claims were paid
        */
        IF btt_auth.claim_code <> buf_auth.claim_code
        THEN
          ASSIGN cValidMessage = "Claims were already paid from the authorisation. Claim code may not be changed"
                 lSuccess      = goErrorObject:addError
                                              (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                               INPUT "":U,                  /* ipcOwningEntityKey       */
                                               INPUT "claim_code":U ,       /* ipcFieldName             */
                                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                               INPUT cValidMessage,         /* ipcMessageText           */
                                               INPUT "ERR":U).              /* ipcMessageType           */
        /*
           The claim type may not be changed if claims were paid
        */
        IF btt_auth.claim_type <> buf_auth.claim_type
        THEN
          ASSIGN cValidMessage = "Claims were already paid from the authorisation. Claim type may not be changed.":U
                 lSuccess      = goErrorObject:addError
                                         (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                          INPUT "":U,                  /* ipcOwningEntityKey       */
                                          INPUT "claim_type":U ,       /* ipcFieldName             */
                                          INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                          INPUT cValidMessage,         /* ipcMessageText           */
                                          INPUT "ERR":U).              /* ipcMessageType           */

        /*
           Validate Auth dates
        */
        IF (buf_auth.end_date - buf_auth.start_date) > (btt_auth.end_date - btt_auth.start_date)
        THEN
          ASSIGN cValidMessage = "The authorisation period may not be reduced.":U
                 lSuccess      = goErrorObject:addError
                                       (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                        INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                        INPUT "":U,                  /* ipcOwningEntityKey       */
		                                INPUT "end_date",            /* ipcFieldName             */
                                        INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                        INPUT cValidMessage,         /* ipcMessageText           */
                                        INPUT "ERR":U).              /* ipcMessageType           */

        /*
         Check whether any claims have been paid before allowing Discount changes
        */
        IF btt_auth.discount_auth <> buf_auth.discount_auth
        OR btt_auth.discount_type <> buf_auth.discount_type
        THEN
          ASSIGN
            cErrorMessage = "The Discount Type/Amount can not be updated as Claims have already been paid against this Authorisation."
            lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                                   INPUT btt_auth.auth_obj,
                                                   INPUT "":U,
                                                   INPUT IF btt_auth.discount_auth <> buf_auth.discount_auth
                                                         THEN "discount_auth":U
                                                         ELSE "discount_type":U,
                                                   INPUT btt_auth.line_number,
                                                   INPUT cErrorMessage,
                                                   INPUT "ERR":U).
        /*
          PMB Validations when claims are paid
        */
        IF buf_auth.pmb_benefit_% <> btt_auth.pmb_benefit_%
        THEN
          ASSIGN
            cValidMessage = "PMB Benefit Percentage can not be updated for a paid authorisation."
            lSuccess      = goErrorObject:addError
                                        (INPUT "hatau":U,                                  /* ipcOwningEntityMnemonic  */
                                         INPUT btt_auth.auth_obj,                          /* ipdOwningEntityObj       */
                                         INPUT "":U,                                       /* ipcOwningEntityKey       */
                                         INPUT "pmb_benefit_%":U,                          /* ipcFieldName             */
                                         INPUT btt_auth.line_number,                       /* ipiLineNumber            */
                                         INPUT "MA":U,                                     /* ipcMessageGroup          */
                                         INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                         INPUT "PMB Benefit Percentage," + cValidMessage ).

        IF buf_auth.pmb_pay_cost <> btt_auth.pmb_pay_cost
        THEN
          ASSIGN
            cValidMessage = "PMB Pay Cost can not be updated for a paid authorisation."
            lSuccess      = goErrorObject:addError
                                        (INPUT "hatau":U,                                  /* ipcOwningEntityMnemonic  */
                                         INPUT btt_auth.auth_obj,                          /* ipdOwningEntityObj       */
                                         INPUT "":U,                                       /* ipcOwningEntityKey       */
                                         INPUT "pmb_pay_cost":U,                           /* ipcFieldName             */
                                         INPUT btt_auth.line_number,                       /* ipiLineNumber            */
                                         INPUT "MA":U,                                     /* ipcMessageGroup          */
                                         INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                         INPUT "PMB Pay Cost," + cValidMessage ).

      END. /* IF btt_auth.amount_paid > 0.0 OR  btt_auth.quantity_paid > 0 THEN */

      /* PMB Validations */
      IF btt_auth.pmb_indicator <> buf_auth.pmb_indicator THEN
      DO:
        /*  All associated services are NOT required to be authorised */
        IF NOT btt_auth.authorise_all_services THEN
        DO:
          /* Get total Auth Provider amount- and quantity paid values */
          FOR EACH buf_auth_provider NO-LOCK
             WHERE buf_auth_provider.auth_obj = buf_auth.auth_obj:

            ASSIGN deTotAmtPaid = deTotAmtPaid + buf_auth_provider.amount_paid
                   deTotQtyPaid = deTotQtyPaid + buf_auth_provider.quantity_paid.
          END.  /* FOR EACH buf_auth_provider NO-LOCK */

          /* PMB Indicator can't be updated if any associated services are paid  */
          IF (btt_auth.amount_paid   > deTotAmtPaid
          OR  btt_auth.quantity_paid > deTotQtyPaid) THEN
          DO:
            IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) > 0 THEN
            DO:
              ASSIGN
              cErrorMessage = "Associated claims are already paid for the Authorisation, claims should normally "
                            + "be reversed before PMB Indicator can be updated on Authorisation Header."
                            + "Please note that you are overriding this validation".
                lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                                     INPUT btt_auth.auth_obj,    /* ipdOwningEntityObj */
                                                     INPUT "":U,                 /* ipcOwningEntityKey */
                                                     INPUT "pmb_indicator":U,    /* ipcFieldName       */
                                                     INPUT btt_auth.line_number, /* ipiLineNumber      */
                                                     INPUT cErrorMessage,        /* ipcMessageText     */
                                                     INPUT "WAR":U,              /* ipcErrType         */
                                                     INPUT TRUE).                /* iplAcknowledge     */
            END. /* IF LOOKUP(ma_member_auth_pmbind_override,gscUserRole) > 0 */
            ELSE
              ASSIGN
                cErrorMessage = "Associated claims are already paid for the Authorisation, claims must be "
                              + "reversed before PMB Indicator can be updated on Authorisation Header."
                lSuccess      = goErrorObject:addError(INPUT "hatau":U,
                                                       INPUT btt_auth.auth_obj,
                                                       INPUT "":U,
                                                       INPUT "pmb_indicator":U,
                                                       INPUT btt_auth.line_number,
                                                       INPUT cErrorMessage,
                                                       INPUT "ERR":U).
          END.  /* IF (btt_auth.amount_paid > deTotAmtPaid OR btt_auth.quantity_paid > deTotQtyPaid) THEN */
        END.  /* IF NOT btt_auth.authorise_all_services THEN */
      END. /* IF btt_auth.pmb_indicator <> buf_auth.pmb_indicator THEN */
    END. /* IF AVAILABLE buf_auth THEN */

    /*
      At least one provider must be specified for any authorisation
    */
    IF NOT CAN-FIND(FIRST btt_auth_provider
                    WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj)
    THEN
      ASSIGN cMessage = "At least one provider must be linked to auth " + STRING(btt_auth.auth_num)
             lSuccess = goErrorObject:addError
                                       (INPUT "hatau":U,                      /* ipcOwningEntityMnemonic  */
                                        INPUT btt_auth.auth_obj,              /* ipdOwningEntityObj       */
                                        INPUT "":U,                           /* ipcOwningEntityKey       */
                                        INPUT btt_auth.line_number,           /* ipiLineNumber            */
                                        INPUT cMessage,                       /* ipcMessageText           */
                                        INPUT "ERR":U).                       /* ipiMessageType           */

    /*
      At this point of the final Save, we want to check that for every Provider
      that is linked to the Auth and has an Authorise_Detail_Lines = Mandatory,
      has at least one Detail line captured.
    */
    ASSIGN oAuthDetailLines = NEW cls.mipacronym(?, FALSE, "ma_acAuthTypeDetails":U, ?).

    FOR EACH btt_auth_provider NO-LOCK
       WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj:

      EMPTY TEMP-TABLE ttAuthTypeConfig.

      ASSIGN iLogTime = ETIME.

      mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                                   BUFFER btt_auth_provider,
                                                   INPUT-OUTPUT TABLE ttAuthTypeConfig).

      ASSIGN cTrackingMessage = "AuthService:getAuthTypeConfig completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      /*
        Check Authorised detail line setup for Provider.  At least ONE detail line must exist, but only when Auth is completed.
      */
      IF AVAILABLE ttAuthTypeConfig THEN
      DO:
        ASSIGN iLogTime = ETIME.

        oAuthDetailLines:focusAcronym("KEY":U, ttAuthTypeConfig.AuthoriseDetailLines) NO-ERROR.

        ASSIGN cTrackingMessage = "oAuthDetailLines:focusAcronym completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      END.  /* IF AVAILABLE ttAuthTypeConfig THEN */

      /*
        MMP-478 In the below IF statement a direct DB call was removed in favor of using the tt.
        The code was originally trying to make a direct DB call first to find the record and was then using the tt
        if the DB call did not return the values, this was not necessary as the tt has all the info we need
      */
      IF  oAuthDetailLines:AcronymInFocus
      AND oAuthDetailLines:AcronymLabel = "MANDATORY":U THEN
      DO:
        IF NOT CAN-FIND(FIRST btt_auth_detail NO-LOCK
                        WHERE btt_auth_detail.auth_obj          = btt_auth_provider.auth_obj
                          AND btt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj)
        THEN
          ASSIGN cMessage = "At least one detail line must exist for this Provider: ":U + STRING(btt_auth_provider.doc_num)
                 lSuccess = goErrorObject:addError
                                           (INPUT "hatau":U,                      /* ipcOwningEntityMnemonic  */
                                            INPUT btt_auth.auth_obj,               /* ipdOwningEntityObj       */
                                            INPUT "":U,                            /* ipcOwningEntityKey       */
                                            INPUT btt_auth.line_number,            /* ipiLineNumber            */
                                            INPUT cMessage,                        /* ipcMessageText           */
                                            INPUT "ERR":U).                        /* ipiMessageType           */
      END.  /* IF oAuthDetailLines:AcronymInFocus AND oAuthDetailLines:AcronymLabel = "MANDATORY":U THEN */
    END.  /* FOR EACH btt_auth_provider NO-LOCK */

    RUN _validateAuthTypeProviderMandatory IN TARGET-PROCEDURE (BUFFER btt_auth).

    /*
      Main code validations
      If the authorisation has been completed then we need to check if there is
      a main diagnosis code and depending on the rule configuration we may require
      a corresponding main procedure code with the correct coding type.
    */
    IF LOOKUP(STRING(btt_auth.auth_status),"5,6":U) = 0 THEN
    DO:
      /*
        First things first - see if we have a main ICD code checking db and tt
      */
      /*
        MMP-478 Removed a direct DB call from this location in favor of the btt_auth_coding temp-table
        In many cases the code was trying to do the direct DB call first then, if unable to get a result, it would use the tt
        This was not necessary as the tt has all the information we need
      */
      FIND FIRST  btt_auth_coding NO-LOCK
           WHERE  btt_auth_coding.auth_obj               = btt_auth.auth_obj
             AND  btt_auth_coding.owning_entity_mnemonic = "diagnos":U
             AND (btt_auth_coding.main_code              = TRUE
              OR  btt_auth_coding.primary_code           = TRUE)
             AND  btt_auth_coding.coding_status         <> 5
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE btt_auth_coding
      THEN
        ASSIGN cICDCodingType = btt_auth_coding.coding_type
               cICDCode       = btt_auth_coding.owning_alt_value.
      ELSE DO:

        ASSIGN cMessage = "The main/primary Coding Details (ICD) must be captured before the authorisation can be completed.":U.

        goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                               INPUT "":U,                  /* ipcOwningEntityKey       */
                               INPUT btt_auth.line_number,  /* ipiLineNumber            */
                               INPUT cMessage,              /* ipcMessageText           */
                               INPUT "ERR":U).
        RETURN.
      END.  /* ELSE - IF AVAILABLE btt_auth_coding */

      /*
        We need both rules to be valid for this validation, so it makes sense to
        only find the "CodingMainCode" rule if the "CPTMandatory" rule is valid.
      */
      IF lCPTMandatoryValidRule
      AND TRIM(cCPTMandatoryRuleValue) <> "":U THEN
      DO:

        IF cCPTMandatoryRuleValue BEGINS "WARN":U
        THEN
          ASSIGN cCPTMandatoryErrType     = "WAR":U
                 lCPTMandatoryAcknowledge = IF cCPTMandatoryRuleValue = "WarnAck":U
                                            THEN TRUE
                                            ELSE FALSE.
        ELSE
          ASSIGN cCPTMandatoryErrType     = "ERR":U
                 lCPTMandatoryAcknowledge = FALSE.

        /*
          If the primary/main CPT is not entered for the auth, then give an error
          or warning, because the CPT is Mandatory.
        */
        FIND FIRST  btt_auth_coding NO-LOCK
             WHERE  btt_auth_coding.auth_obj               = btt_auth.auth_obj
               AND  btt_auth_coding.owning_entity_mnemonic = "hlmck":U
               AND (btt_auth_coding.main_code              = TRUE
                OR  btt_auth_coding.primary_code           = TRUE)
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        /*
          If it doesn't exist in the dataset we may have updated the auth header without
          having updated the coding so we may find the relevant coding record in the db.

          Ok we did our best - return an error
        */
        IF NOT AVAILABLE btt_auth_coding
        THEN
          ASSIGN
            cMessage = "No primary or main procedure (CPT) details are available."
                     + "[HELP=Auth Rule Code: CodingMainCode, CPTMandatory]":U
            lSuccess = goErrorObject:addError(INPUT "hatau":U,                  /* ipcOwningEntityMnemonic  */
                                              INPUT btt_auth.auth_obj,          /* ipdOwningEntityObj       */
                                              INPUT "":U,                       /* ipcOwningEntityKey       */
                                              INPUT btt_auth.line_number,       /* ipiLineNumber            */
                                              INPUT cMessage,                   /* ipcMessageText           */
                                              INPUT cCPTMandatoryErrType,       /* ipcErrType               */
                                              INPUT lCPTMandatoryAcknowledge).  /* iplAcknowledge           */
      END.  /* IF lCPTMandatoryValidRule AND TRIM(cCPTMandatoryRuleValue) <> "":U THEN */

      /*
        Validate the ICD-CPT Coding Type Pairing
      */
      IF cICDCodingType <> "":U THEN
      DO:
        ASSIGN iLogTime = ETIME.

        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                       INPUT  "IcdCptTypePairing":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lPairingValidRule,
                                                       OUTPUT cPairingRuleValue).

        ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(IcdCptTypePairing) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        IF lPairingValidRule THEN
        DO:

          /*
            Find what the main procedure coding type should be according to the IcdCptTypePairing rule
            using the main ICD coding type to find it
          */
          ASSIGN iLogTime = ETIME

                 cCPTCodingType = mipEnv:miExpression:getNVPElement(INPUT TRIM(cPairingRuleValue), INPUT TRIM(cICDCodingType), "|":U, ",":U)

                 cCPTCodingType = (IF cCPTCodingType = ? THEN "":U ELSE TRIM(cCPTCodingType)). /*Our ICD coding type may not have been listed in the rule*/
                                                                                               /*in which case we will have a null value                 */

          ASSIGN cTrackingMessage = "miExpression:getNVPElement completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          /*
            If we were able to determine the main CPT coding type then we will check if we have a main
            procedure code with that type and if we weren't able to determine a CPT coding type then there
            might be an issue with the main ICD coding type, perhaps the coding type used on the main ICD
            is not listed in the CPTMandatory rule.
          */
          IF cCPTMandatoryErrType = "":U OR cCPTMandatoryErrType = ?
          THEN
            ASSIGN cCPTMandatoryErrType     = "ERR":U
                   lCPTMandatoryAcknowledge = FALSE.

          IF cCPTCodingType <> "":U THEN
          DO:
            FIND FIRST  btt_auth_coding NO-LOCK
                 WHERE  btt_auth_coding.auth_obj               = btt_auth.auth_obj
                   AND  btt_auth_coding.owning_entity_mnemonic = "hlmck":U
                   AND (btt_auth_coding.main_code              = TRUE
                    OR  btt_auth_coding.primary_code           = TRUE)
                   AND  btt_auth_coding.coding_type            = cCPTCodingType
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            /*
              Return an error/warning (depending on CPTMandatory setup)
            */
            IF NOT AVAILABLE btt_auth_coding
            THEN
              ASSIGN
                cMessage = SUBSTITUTE("A main procedure code must be specified with the same coding type (&1) as the coding type specified on the main diagnosis (&2).":U,
                                      cCPTCodingType, cICDCode) + "[HELP=Auth Rule Code: IcdCptTypePairing]":U
                lSuccess = goErrorObject:addError(INPUT "hatau":U,                  /* ipcOwningEntityMnemonic  */
                                                  INPUT btt_auth.auth_obj,          /* ipdOwningEntityObj       */
                                                  INPUT "":U,                       /* ipcOwningEntityKey       */
                                                  INPUT btt_auth.line_number,       /* ipiLineNumber            */
                                                  INPUT cMessage,                   /* ipcMessageText           */
                                                  INPUT cCPTMandatoryErrType,       /* ipcErrType               */
                                                  INPUT lCPTMandatoryAcknowledge).  /* iplAcknowledge           */

          END. /* IF cCPTCodingType <> "":U THEN */
          ELSE DO:
            /*
              We couldnt find a corresponding procedure coding type indicating something
              is not quite right with the coding type on our main diagnosis code.
            */
            ASSIGN
              cMessage = SUBSTITUTE("The coding type (&1) on the main diagnosis code (&2) could not be found in the Auth Rule.[HELP=Auth Rule Code: &3]":U,
                                    cICDCodingType, cICDCode, "IcdCptTypePairing":U)
              lSuccess = goErrorObject:addError(INPUT "hatau":U,                  /* ipcOwningEntityMnemonic  */
                                                INPUT btt_auth.auth_obj,          /* ipdOwningEntityObj       */
                                                INPUT "":U,                       /* ipcOwningEntityKey       */
                                                INPUT btt_auth.line_number,       /* ipiLineNumber            */
                                                INPUT cMessage,                   /* ipcMessageText           */
                                                INPUT cCPTMandatoryErrType,       /* ipcErrType               */
                                                INPUT lCPTMandatoryAcknowledge).  /* iplAcknowledge           */

          END. /* ELSE - IF cCPTCodingType <> "":U THEN */
        END.  /* IF lPairingValidRule THEN */
      END.  /* IF cICDCodingType <> "" THEN */
    END.  /* IF LOOKUP(STRING(btt_auth.auth_status),"5,6") = 0 THEN */

    /*
      If any detail lines have no end date, then the Auth may not be completed
    */
    FIND FIRST bbt_auth_detail NO-LOCK
         WHERE bbt_auth_detail.auth_obj = btt_auth.auth_obj
           AND bbt_auth_detail.end_date = ?
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE bbt_auth_detail
    THEN
      ASSIGN
        cMessage = "Auth detail lines with no end date exists."
        lSuccess = goErrorObject:addError(INPUT "hatad:":U + bbt_auth_detail.owning_entity_mnemonic,    /* ipcOwningEntityMnemonic  */
                                          INPUT bbt_auth_detail.auth_detail_obj,                        /* ipdOwningEntityObj       */
                                          INPUT "":U,                                                   /* ipcOwningEntityKey       */
                                          INPUT "end_date":U,                                           /* ipcFieldName             */
                                          INPUT bbt_auth_detail.line_number,                            /* ipiLineNumber            */
                                          INPUT cMessage,                                               /* ipcMessageText           */
                                          INPUT "ERR":U).                                               /* ipcError                 */

  END.  /* IF NOT btt_auth.auth_incomplete */
END. /* IF btt_auth.record_action = "MODIFY":U THEN */

IF btt_auth.record_action = "DELETE":U THEN
DO:
  /*
    A complete authorisation may not be removed
  */
  IF NOT btt_auth.auth_incomplete
  THEN
    ASSIGN lSuccess = goErrorObject:addError
                                   (INPUT "hatau":U,                                                    /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth.auth_obj,                                            /* ipdOwningEntityObj       */
                                    INPUT "":U,                                                         /* ipcOwningEntityKey       */
                                    INPUT btt_auth.line_number,                                         /* ipiLineNumber            */
                                    INPUT "Authorisation has been completed and may not be removed.":U, /* ipcMessageText           */
                                    INPUT "ERR":U).                                                     /* ipcMessageType           */
END.  /* ELSE IF btt_auth.record_action = "DELETE":U THEN */

&ENDIF

{ mip/inc/mipcatcherror.i
  &FINALLY = "IF VALID-OBJECT(oAuthDetailLines) THEN DELETE OBJECT oAuthDetailLines.
              IF VALID-OBJECT(oMemberSearch)    THEN DELETE OBJECT oMemberSearch.
              IF VALID-OBJECT(oCPTSearch)       THEN DELETE OBJECT oCPTSearch.
              DATASET dsCpt:EMPTY-DATASET().
              DATASET dsMember:EMPTY-DATASET(). "}
