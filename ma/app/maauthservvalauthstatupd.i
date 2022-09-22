/* maauthservvalauthstatupd.i  MEDSTAR Medical Aid System
                              (c) Copyright 2020 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/                      
/*------------------------------------------------------------------------------
  Purpose:     Validate the auth status when the record is updated (NOT a new
               record) and return an error if the auth status is not valid
  Parameters:  
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipdInsurerObj         AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiOptionCode         AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipiFromStatusCode     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcFromStatusReason   AS CHARACTER NO-UNDO. 
  DEFINE INPUT  PARAMETER ipiToStatusCode       AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAmountPaid         AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdQuantityPaid       AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdDate               AS DATE      NO-UNDO.
  DEFINE INPUT  PARAMETER ipiParentStatusCode   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcParentStatusReason AS CHARACTER NO-UNDO. 
  DEFINE OUTPUT PARAMETER opcErrorMessage       AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cAuthRule           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFromStatus         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFromStatusDescr    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMessageDescription AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMessageText        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMessageType        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMsgParm            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRolesAllowed       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cToStatus           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cToStatusDescr      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cUsersAllowed       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iFromStatusCode     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lValidRule          AS LOGICAL     NO-UNDO.

  /*
    Get the rule value for the 'old' status so that we can see what new statuses are allowed.
    We change statuses 2 and 4 to 1, because users will always see statuses 2 and 4 as 'authorised'
    and will therefor set up the rules only for '1-Authorised'.
  */
  IF ipiFromStatusCode = 2 /* Assessed  */
  OR ipiFromStatusCode = 4 /* Completed */ THEN
    ASSIGN iFromStatusCode = 1.  /* Authorised */
  ELSE
    ASSIGN iFromStatusCode = ipiFromStatusCode.

  ASSIGN cFromStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                           INPUT iFromStatusCode)
         cFromStatus      = STRING(iFromStatusCode) + "-":U + cFromStatusDescr.

  ASSIGN cAuthRule = "Update":U + STRING(iFromStatusCode) + cFromStatusDescr
         lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                      (INPUT  ipdInsurerObj,
                       INPUT  ipiOptionCode,
                       INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                       INPUT  cAuthRule,
                       INPUT  ipdDate,
                       OUTPUT lValidRule,
                       OUTPUT cRuleValue).
  IF NOT lValidRule OR cRuleValue = "":U THEN
  DO:
    /* Auth Message 13 = "The Auth Status may not be changed from '&1'&2" */
    ASSIGN cMessageText    = ""
           opcErrorMessage = fnGetErrorMessage(INPUT "ma_MsgAuth":U, 
                                               INPUT 13,
                                               INPUT cFromStatus)
           opcErrorMessage = opcErrorMessage + "[HELP=Auth Rule Code: ":U + cAuthRule + "]":U.
    RETURN.
  END.  /* IF NOT lValidRule THEN */
  ASSIGN cRuleValue = REPLACE(cRuleValue," ","").

  /*
     Validate the first entry on the rule: Valid new status codes allowed
     Get the new status description, because we need to compare the status code and
     description with the auth rule setup, e.g. '1-Authrised,5-Cancelled,6-Declined'
  */
  ASSIGN cToStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "Auth":U,
                                                                         INPUT ipiToStatusCode)
         cToStatus      = TRIM(STRING(ipiToStatusCode)) + "-":U + cToStatusDescr.
  IF LOOKUP(cToStatus,ENTRY(1,cRuleValue,"|":U)) = 0 THEN
  DO:
    /* Auth Message 13 = "The Auth Status may not be changed from '&1'&2" */
    ASSIGN cMessageText    = " to '" + cToStatus + "'"
           opcErrorMessage = fnGetErrorMessage(INPUT "ma_MsgAuth":U, 
                                               INPUT 13,
                                               INPUT cFromStatus + "," + cMessageText)
           opcErrorMessage = opcErrorMessage + "[HELP=Auth Rule Code: ":U + cAuthRule + "]":U.
    RETURN.
  END.  /* IF LOOKUP(cToStatus,ENTRY(1,cRuleValue,"|":U)) = 0 THEN */

  /*
     Validate the second entry on the rule: Users who are allowed to change the status.
     This entry is optional. If blank, then all users are allowed to change the auth status.
  */
  IF NUM-ENTRIES(cRuleValue,"|":U) > 1
  THEN ASSIGN cUsersAllowed = ENTRY(2,cRuleValue,"|":U).
  ELSE ASSIGN cUsersAllowed = "":U.

  IF cUsersAllowed <> "":U
  AND LOOKUP(mipEnv:miUser:UserCode,cUsersAllowed) = 0 THEN
  DO:
    /* Auth Message 14 = "You are not allowed to change the status of &1 auth." */
    ASSIGN cMsgParm = IF LOOKUP(SUBSTRING(cFromStatusDescr,1,1),"A,E,I,O,U") > 0
                      THEN 'an ' + cFromStatusDescr
                      ELSE 'a '  + cFromStatusDescr
           opcErrorMessage = fnGetErrorMessage(INPUT "ma_MsgAuth":U, 
                                               INPUT 14,
                                               INPUT cMsgParm)
           opcErrorMessage = opcErrorMessage + "[HELP=Auth Rule Code: ":U + cAuthRule + "]":U.
    RETURN.
  END.  /* IF cUsersAllowed <> "":U AND... */

  /*
     Validate the third entry on the rule: Roles who are allowed to change the status.
     This entry is optional. If blank, then all roles are allowed to change the auth status.
  */
  IF NUM-ENTRIES(cRuleValue,"|":U) > 2
  THEN ASSIGN cRolesAllowed = ENTRY(3,cRuleValue,"|":U).
  ELSE ASSIGN cRolesAllowed = "":U.

  IF cRolesAllowed <> "":U
  AND NOT fnUserHasRole(cRolesAllowed) THEN
  DO:
    /* Auth Message 14 = "You are not allowed to change the status of &1 auth." */
    ASSIGN cMsgParm = IF LOOKUP(SUBSTRING(cFromStatusDescr,1,1),"A,E,I,O,U") > 0
                      THEN 'an ' + cFromStatusDescr
                      ELSE 'a '  + cFromStatusDescr
           opcErrorMessage = fnGetErrorMessage(INPUT "ma_MsgAuth":U, 
                                               INPUT 14,
                                               INPUT cMsgParm)
           opcErrorMessage = opcErrorMessage + "[HELP=Auth Rule Code: ":U + cAuthRule + "]":U.
    RETURN.
  END.  /* IF cRolesAllowed <> "":U AND... */

  /*
     If claims have already been paid against the auth,
     the auth's status must remain 'Authorised'.
  */
  IF  LOOKUP(STRING(ipiFromStatusCode),"1,2,4") > 0
  AND LOOKUP(STRING(ipiToStatusCode),"0,5,6,7") > 0
  AND ipdAmountPaid <> 0 AND ipdQuantityPaid <> 0 THEN
  DO:
    /* Auth Message 15 = "Claims have already been paid against this auth. Reverse claims first and then try again." */
    ASSIGN opcErrorMessage = fnGetErrorMessage(INPUT "ma_MsgAuth":U, 
                                               INPUT 15, 
                                               INPUT "").
    RETURN.
  END.  /* IF LOOKUP(STRING(ipiFromStatusCode),"1,2") > 0 */

  /* 
    If EnforceParentStatus rule is available and the previous status is 
    listed in the rule value, return an error to prevent the user from 
    changing the auth status. 
  */
  IF ipiFromStatusCode = ipiParentStatusCode
  THEN DO:
    ASSIGN lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                        (INPUT  ipdInsurerObj,
                         INPUT  ipiOptionCode,
                         INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                         INPUT  "EnforceParentStatus":U,
                         INPUT  ipdDate,
                         OUTPUT lValidRule,
                         OUTPUT cRuleValue).

    IF lValidRule AND LOOKUP(cFromStatus,cRuleValue) > 0 
    THEN DO:
      /* Auth Message 13 = "The Auth Status may not be changed from '&1'&2" */
      ASSIGN cMessageText    = " to '" + cToStatus + "'"
             opcErrorMessage = fnGetErrorMessage(INPUT "ma_MsgAuth":U, 
                                                 INPUT 13,
                                                 INPUT cFromStatus + "," + cMessageText)
             opcErrorMessage = opcErrorMessage + "[HELP=Auth Rule Code: EnforceParentStatus]":U.
      RETURN.
    END. /* IF lValidRule AND */

  END. /* IF ipiFromStatusCode = ipiParentStatusCode */
  
  {mip/inc/mipcatcherror.i}
