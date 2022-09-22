/* maauthbusvalauthdependant.i  MEDSTAR Medical Aid System
                                Dependant validations
                                (c) Copyright 2022
                                MIP Holdings (Pty) Ltd
                                All rights reserved
*/ 

  /*
    Gender and age validations
  */
  IF  ttAuthTypeConfig.Gender <> "":U
  AND ttAuthTypeConfig.Gender <> "B":U
  AND ttAuthTypeConfig.Gender <> btt_auth._dependant_gender
  THEN
    goErrorObject:addError(INPUT "hatau":U,                                  /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth.auth_obj,                          /* ipdOwningEntityObj       */
                           INPUT "":U,                                       /* ipcOwningEntityKey       */
                           INPUT "dependant":U,                              /* ipcFieldName             */
                           INPUT btt_auth.line_number,                       /* ipiLineNumber            */
                           INPUT "MA":U,                                     /* ipcMessageGroup          */
                           INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                           INPUT "Gender (" + btt_auth._dependant_gender +
                                 "),The dependant's gender is not valid for the Authorisation Type (" + ttAuthTypeConfig.Gender + ").").  /* ipcReplaceTextList */

  IF (ttAuthTypeConfig.AgeRangeBothObj   <> 0 AND ttAuthTypeConfig.AgeRangeBothObj   <> ?) 
  OR (ttAuthTypeConfig.AgeRangeMaleObj   <> 0 AND ttAuthTypeConfig.AgeRangeMaleObj   <> ?)
  OR (ttAuthTypeConfig.AgeRangeFemaleObj <> 0 AND ttAuthTypeConfig.AgeRangeFemaleObj <> ?) THEN
  DO:
    ASSIGN lAgeValid = mipEnv:Health:maMember:validateDependantAge(INPUT btt_auth._dependant_age_years,
                                                                   INPUT btt_auth._dependant_gender,
                                                                   INPUT ttAuthTypeConfig.AgeRangeBothObj,
                                                                   INPUT ttAuthTypeConfig.AgeRangeFemaleObj,
                                                                   INPUT ttAuthTypeConfig.AgeRangeMaleObj,
                                                                   OUTPUT cErrorMessage).
    IF NOT lAgeValid THEN
    DO:
      ASSIGN cErrorMessage = "The Dependant Age specified is invalid for the Authorisation Type.".
  
      goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                             INPUT "":U,                  /* ipcOwningEntityKey       */
                             INPUT "dependant":U,         /* ipcFieldName             */
                             INPUT btt_auth.line_number,  /* ipiLineNumber            */
                             INPUT cErrorMessage,         /* ipcMessageText           */
                             INPUT "ERR":U).              /* ipcMessageType           */
    END.  // IF NOT lAgeValid THEN
  END.  // IF ttAuthTypeConfig.AgeRangeBothObj <> 0 OR...

  /*
     Validating the dependant's join period
  */
  ASSIGN iLogTime = ETIME.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                 INPUT  btt_auth.option_code,
                                                 INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                 INPUT  "DependantJoinWarn":U,
                                                 INPUT  btt_auth.start_date,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  ASSIGN cTrackingMessage = "AuthMaintenance:getAuthRuleValue(DependantJoinWarn) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.
  
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  IF lValidRule THEN
  DO:
    ASSIGN iNrOfDays    = INTEGER(ENTRY(1,cRuleValue, "|":U))
           iFlagNr      = IF NUM-ENTRIES(cRuleValue, "|") > 1
                          THEN INTEGER(ENTRY(2,cRuleValue, "|":U))
                          ELSE 0
           lAcknowledge = IF NUM-ENTRIES(cRuleValue, "|":U) > 2
                          THEN ENTRY(3, cRuleValue, "|":U) = "WarnAck":U
                          ELSE FALSE.

    oMemberSearch = NEW cls.mamembersearch(DATASET dsMember BY-REFERENCE).

    ASSIGN lSuccess = oMemberSearch:SetCriteria("BufferList","tt_memdet,tt_memdep")
           lSuccess = oMemberSearch:SetFilterCriteria("tt_memdet.mem-num":U, "=":U, btt_auth.mem_num)
           lSuccess = oMemberSearch:fetchData().

    FIND FIRST btt_memdep NO-LOCK
         WHERE btt_memdep.dependant = btt_auth.dependant
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE btt_memdep
    AND btt_auth.start_date >=  btt_memdep.joined-date
    AND btt_auth.start_date <= (btt_memdep.joined-date + iNrOfDays) THEN
    DO:
      FIND FIRST  flagdet NO-LOCK
           WHERE  flagdet.scheme-code  = 00
             AND  flagdet.key          = btt_auth.mem_num
             AND  flagdet.type         = "M":U
             AND  flagdet.flag-num     = iFlagNr
             AND  flagdet.effect-date <= btt_auth.start_date
             AND (flagdet.end-date     = ?
              OR  flagdet.end-date    >= btt_auth.start_date)
           NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF iFlagNr = 0
      OR NOT AVAILABLE flagdet
      OR flagdet.flag-value BEGINS "N":U
      THEN
        ASSIGN cMessage = "The dependant joined less than " + STRING(iNrOfDays) + " days ago."
               cMessage = cMessage + "[HELP=Auth Rule Code: DependantJoinWarn]"
               lSuccess = goErrorObject:addError
                                   (INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                                    INPUT "":U,                  /* ipcOwningEntityKey       */
                                    INPUT "dependant":U,         /* ipcFieldName             */
                                    INPUT btt_auth.line_number,  /* ipiLineNumber            */
                                    INPUT cMessage,              /* ipcMessageText           */
                                    INPUT "WAR":U,               /* ipcMessageType           */
                                    INPUT lAcknowledge).         /* iplAcknowledge           */
    END.  /* IF  AVAILABLE btt_memdep AND... */
  END.  /* IF lValidRule THEN */

  /*
     Check "SuspendDependant"-rule
  */  
  ASSIGN iLogTime = ETIME.

  RUN _validateAuthSuspendRule IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT "Dependant", INPUT-OUTPUT goErrorObject ).

  ASSIGN cTrackingMessage = "_validateAuthSuspendRule(Dependant) completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
     Dependant resignation date check on authorisation period
  */
  ASSIGN iLogTime = ETIME.

  RUN _validateAuthResignation IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT-OUTPUT goErrorObject ).

  ASSIGN cTrackingMessage = "_validateAuthResignation completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
     Dependant benefit date check
  */
  ASSIGN iLogTime = ETIME.

  RUN _validateAuthDepBenefitDate IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT-OUTPUT goErrorObject ).

  ASSIGN cTrackingMessage = "_validateAuthDepBenefitDate completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
     Dependant inactivity period check
  */
  ASSIGN iLogTime = ETIME.

  RUN _validateAuthDepInactivity IN TARGET-PROCEDURE ( BUFFER btt_auth, INPUT-OUTPUT goErrorObject ).

  ASSIGN cTrackingMessage = "_validateAuthDepInactivity completed in - " + STRING(ETIME - iLogTime) + " milliseconds":U.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

