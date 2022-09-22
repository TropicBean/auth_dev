/* maauthservchecklimits.i MEDSTAR Medical Aid System
                           Limit checking for authorisations
                           (c) Copyright 2022
                           MIP Holdings (Pty) Ltd
                           All rights reserved
------------------------------------------------------------------------------
  Purpose:  This procedure will do Limit checking on all the levels of the
            Authorisation where authorised values can be specified

  Notes  :  - ipcAuthLevel - Specify 'Header', 'Provider' or 'Detail'
            - Return an error message if applicable

------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipdInsurerObj      AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipiOptionCode      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcMemNum          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiDependant       AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipdDate            AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER ipiClaimCode       AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcClaimtype       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdAmount          AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipdQuantity        AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER iplPMBIndicator    AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcAuthLevel       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdAuthObj         AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipdAuthProviderObj AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opdAmount          AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opdQuantity        AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opdPMBValue        AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opiAuthStatus      AS INTEGER   NO-UNDO INITIAL ?.
DEFINE OUTPUT PARAMETER opcStatusReason    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcClaimType       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcError           AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplLimitsChecked   AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt_limitwarn.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cError                  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPMBType                AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValueLimitChecking AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTrackingMessage        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lValidRule              AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE dAuthorisedValue        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE lReserveLimits          AS LOGICAL     NO-UNDO.

  /*
    We want to limit the scope of these buffers to this procedure
  */
  DEFINE BUFFER hat_auth          FOR hat_auth.
  DEFINE BUFFER hat_auth_provider FOR hat_auth_provider.

  EMPTY TEMP-TABLE tt_limit.
  EMPTY TEMP-TABLE tt_limitwarn.

  /*
    Determine if limit checking must be done
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                 INPUT  ipiOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeLimits":U,
                                                 INPUT  "LimitChecking":U,
                                                 INPUT  ipdDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValueLimitChecking).
  /*
    If the rule exists with no rule value setup, or if the rule doesn't exist,
    then no limit checking must be done.
  */
  ASSIGN cTrackingMessage = "Checklimits - Rule: LimitChecking: ValidRule=" + STRING(lValidRule)
                          + " RuleValue=" + cRuleValueLimitChecking
                          + " ipcAuthLevel=" + ipcAuthLevel.

  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  IF NOT lValidRule
  OR cRuleValueLimitChecking = ""
  THEN
    RETURN.

  /*
    If the rule exists and Authorisation Level passed in exists in the rule value,
    then limit checking must continue.
  */
  IF lValidRule
  AND LOOKUP(ipcAuthLevel,cRuleValueLimitChecking) > 0 THEN
  DO:
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                   INPUT  ipiOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeLimits":U,
                                                   INPUT  "ReserveLimits":U,
                                                   INPUT  ipdDate,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).
    IF NOT lValidRule
    OR cRuleValue = ""
    THEN
      ASSIGN lReserveLimits = NO.
    ELSE
      ASSIGN lReserveLimits = IF LOOKUP(cRuleValue,"Yes,Y,True") > 0
                              THEN YES
                              ELSE NO.

    ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - Rule: ReserveLimits: ValidRule=" + STRING(lValidRule)
                            + " RuleValue=" + cRuleValue + " lReserveLimits=" + STRING(lReserveLimits).

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /*
      Determine if limit checking must apply if called from Provider or Clinical Details.
    */
    IF LOOKUP(ipcAuthLevel,"Provider,Detail":U) > 0 THEN
    DO:
      IF LOOKUP("Header":U,cRuleValueLimitChecking) > 0 THEN
      DO:
        FIND hat_auth NO-LOCK
          WHERE hat_auth.auth_obj = ipdAuthObj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

        /*
          Leave method if limit checking will apply on the header level.
        */
        IF AVAILABLE hat_auth
        AND  hat_auth.claim_code     = ipiClaimCode
        AND  hat_auth.claim_type     = ipcClaimType
        AND (hat_auth.amount_auth   <> 0
        OR   hat_auth.quantity_auth <> 0) THEN
        DO:
          ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - LimitChecking Applied on header:"
                                  + " ipiClaimCode=" + STRING(ipiClaimCode) + " ipcClaimType=" + ipcClaimType
                                  + " hat_auth.amount_auth=" + STRING(hat_auth.amount_auth)
                                  + " hat_auth.quantity_auth" + STRING(hat_auth.quantity_auth).

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          RETURN.
        END.  // IF AVAILABLE hat_auth AND...
      END. // IF LOOKUP("Header",cRuleValue) > 0 AND...

      /*
        Only determine if limits are checked on the Provider level, if limits are checked for a Clinical Detail line.
      */
      IF LOOKUP("Provider":U,cRuleValueLimitChecking) > 0
      AND ipcAuthLevel = "Detail":U THEN
      DO:
        FIND hat_auth_provider NO-LOCK
          WHERE hat_auth_provider.auth_provider_obj = ipdAuthProviderObj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

        /*
          Leave method if limit checking will apply on the provider level.
        */
        IF AVAILABLE hat_auth_provider
        AND  hat_auth_provider.claim_code     = ipiClaimCode
        AND  hat_auth_provider.claim_type     = ipcClaimType
        AND (hat_auth_provider.amount_auth   <> 0
        OR   hat_auth_provider.quantity_auth <> 0) THEN
        DO:
          ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - LimitChecking Applied on provider:"
                                  + " ipiClaimCode=" + STRING(ipiClaimCode) + " ipcClaimType=" + ipcClaimType
                                  + " hat_auth_provider.amount_auth=" + STRING(hat_auth_provider.amount_auth)
                                  + " hat_auth_provider.quantity_auth" + STRING(hat_auth_provider.quantity_auth).

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          RETURN.
        END. // IF AVAILABLE hat_auth_provider AND...
      END. // IF LOOKUP("Provider",cRuleValueLimitChecking) > 0 AND ipcAuthLevel = "Detail":U THEN

    END. // IF LOOKUP(ipcAuthLevel,"Provider,Detail":U) > 0 THEN

    mipEnv:Health:limitBusinessLogic:fetchAvailLimits(
                  INPUT        ipiOptionCode,     // ipiSchemeCode
                  INPUT        ipcMemNum    ,     // ipcMemNum
                  INPUT        ipiDependant ,     // ipiDependant
                  INPUT        ipdDate      ,     // ipdDate
                  INPUT        ipiClaimCode ,     // ipiClaimCode
                  INPUT        ipcClaimType ,     // ipcClaimType
                  INPUT        ""           ,     // ipcMenuOption
                  INPUT        ipdAmount    ,     // ipdBenefit
                  INPUT        ipdQuantity  ,     // ipiQuantity
                  INPUT        YES          ,     // iplAuthorisation
                  INPUT        YES          ,     // iplCalcMaxBenefit
                  INPUT        YES          ,     // iplCalcLimitLeft
                  OUTPUT       cError       ,     // opcError
                  OUTPUT TABLE tt_limit).

    /*
      Handle any error that might be returned from fetchAvailLimits
    */
    IF cError <> "" THEN
    DO:
      ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - Error returned from fetchAvailLimits=" + cError.

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      ASSIGN opcError = cError.

      RETURN.
    END.  // IF cError <> "" THEN

    /*
      Determine if normal limit checking should apply if PMB Indicator = yes.
    */
    IF iplPMBIndicator THEN
    DO:
      /* Check if the PMB limit is setup for claim code & claim type.  */
      /* No setup will indicate the PMB rules should not apply.        */
      FIND FIRST tt_limit
        WHERE tt_limit.modus_operandi = 18
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      /* Limits must be checked and PMB should not apply. */
      IF NOT AVAILABLE tt_limit THEN
      DO:
        ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - iplPMBIndicator=" + STRING(iplPMBIndicator)
                                + " lReserveLimits=" + STRING(lReserveLimits)
                                + ". No limit with m/o 18 set up on cc. iplPMBIndicator changed to 'no' and warning created: "
                                + "'PMB Indicator was selected, but PMB rules won't apply for limits'".

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        ASSIGN iplPMBIndicator = no.

        /* Create a warning to indicate that PMB rules won't apply.  */
        FIND tt_limitwarn
          WHERE tt_limitwarn.limit_# = 0
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE tt_limitwarn THEN
        DO:
          CREATE tt_limitwarn.
          ASSIGN tt_limitwarn.limit_#        = 0
                 tt_limitwarn.limit_sequence = 0
                 tt_limitwarn.limit_warning  = "PMB Indicator was selected, but PMB rules won't apply for limits".
        END. // IF NOT AVAILABLE tt_limitwarn THEN
      END. // IF NOT AVAILABLE tt_limit THEN
      ELSE DO:
        /*
           No limit checking is required if PMB Indicator = yes
           and no limits are reserved because claim will be paid if limits are exceeded.
        */
        IF NOT lReserveLimits THEN
        DO:
          ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - iplPMBIndicator="
                                  + STRING(iplPMBIndicator) + " lReserveLimits=" + STRING(lReserveLimits).

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          RETURN.
        END.  // IF NOT lReserveLimits THEN
      END. // ELSE - IF NOT AVAILABLE tt_limit THEN
    END. // IF iplPMBIndicator THEN

    /*
      Read through temp table tt_limit received back from fetchAvailableLimits
      which will contain the limits available, limits used and limits left.
      Only check limits where limit checking applies (tt_limit.check_limit = YES),
      and limit value must be enforced (tt_limit.enforce_limit = YES).
    */
    LIMITCHECK:
    FOR EACH tt_limit
      WHERE tt_limit.check_limit   = YES
      AND   tt_limit.enforce_limit = YES
      BY tt_limit.limit_sequence:

      ASSIGN oplLimitsChecked = YES.

      /*
         Assign variable dAuthorisedValue that will be used in limit checking.
         If authorised value was reduced in limit checking on a previous limit,
         the reduced value must be used when the rest of the limits are checked.
      */
      IF opcStatusReason = ""
      THEN
        ASSIGN dAuthorisedValue = tt_limit.to_limit
               opdAmount        = ipdAmount
               opdQuantity      = ipdQuantity.
    	ELSE
        ASSIGN dAuthorisedValue = IF tt_limit.to_limit_type
                  	              THEN opdAmount    /* If Limit setup is for an amount.  */
                                  ELSE opdQuantity. /* If Limit setup is for a quantity. */

      /*
        When the authorised value exceeds the limit and it is not a PMB.
      */
      ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - Limit#=" + STRING(tt_limit.limit_#)
                              + " ToLimit=" + STRING(tt_limit.to_limit)      + " dAuthorisedValue=" + STRING(dAuthorisedValue)
                              + " Left="    + STRING(tt_limit.limit_left)    + " Check="            + STRING(tt_limit.check_limit)
                              + " Enforce=" + STRING(tt_limit.enforce_limit) + " PMB="              + STRING(iplPMBIndicator).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF tt_limit.limit_left < dAuthorisedValue
      AND NOT iplPMBIndicator THEN
      DO:
        /*
          If the user is setup to override the limit (globally defined variable
          gscUserRoles will indicate it) a warning must be given to the user.
        */
        ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - gscUserRole=" + gscUserRole.

        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        IF LOOKUP("ma_member_auth_limit_override",gscUserRole) > 0 THEN
        DO:
          FIND tt_limitwarn
            WHERE tt_limitwarn.limit_# = tt_limit.limit_#
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE tt_limitwarn THEN
          DO:
            CREATE tt_limitwarn.
            ASSIGN tt_limitwarn.limit_#        = tt_limit.limit_#
                   tt_limitwarn.limit_sequence = tt_limit.limit_sequence
                   tt_limitwarn.limit_warning  = SUBSTITUTE("Authorised value &1 exceeds limit &2. Limit available: &3",
                                                            STRING(dAuthorisedValue), tt_limit.limit_description,
                                                            STRING(tt_limit.limit_left))
                   cTrackingMessage            = "Checklimits on " + ipcAuthLevel + " - User can Override - Warning=" + tt_limitwarn.limit_warning.

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END. // IF NOT AVAILABLE tt_limitwarn THEN
        END.  // IF LOOKUP("ma_member_auth_limit_override",gscUserRole) > 0 THEN
        ELSE DO:
          /*
            If the user is not setup to override the limit, authorised value must be
            reduced up to available limit and a warning must be given to the user.
          */
          IF tt_limit.to_limit_type
          THEN
            ASSIGN opdAmount   = tt_limit.limit_left.  /* If Limit setup is for an amount. */
          ELSE
            ASSIGN opdQuantity = tt_limit.limit_left.  /* If Limit setup is for a quantity */

          /*
            A warning must be given to the user that the authorised value will be reduced.
          */
          FIND tt_limitwarn
            WHERE tt_limitwarn.limit_# = tt_limit.limit_#
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

          IF NOT AVAILABLE tt_limitwarn THEN
          DO:
            CREATE tt_limitwarn.
            ASSIGN tt_limitwarn.limit_#        = tt_limit.limit_#
                   tt_limitwarn.limit_sequence = tt_limit.limit_sequence
                   tt_limitwarn.limit_warning  = IF dAuthorisedValue <> 9999999.99
                                                 THEN SUBSTITUTE("Limit &1 available limit &2 exceeded. Authorised value reduced to available limit.",
                                                                 tt_limit.limit_description, TRIM(STRING(tt_limit.limit_left,">,>>>,>>9.99")))
                                                 ELSE SUBSTITUTE("Available limit for limit &1 is &2. Authorised value will be reduced to available limit.",
                                                                 tt_limit.limit_description, TRIM(STRING(tt_limit.limit_left,">,>>>,>>9.99")))
                   cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - User cannot Override - Warning=" + tt_limitwarn.limit_warning.

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END. /* THEN - IF NOT AVAILABLE tt_limitwarn. */

          IF tt_limit.limit_left = 0 THEN
          DO:
            /*
              If no limit is available get default status reason according to Rule "NoLimitStatusReason".
            */
            mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                           INPUT  ipiOptionCode,
                                                           INPUT  "ma_acAuthRuleTypeLimits":U,
                                                           INPUT  "NoLimitStatusReason":U,
                                                           INPUT  ipdDate,
                                                           OUTPUT lValidRule,
                                                           OUTPUT cRuleValue).
            /*
              If the rule exists, with no rule value setup or if no rule exists -
              An error must be returned because setup is required to complete limit checking.
            */
            IF NOT lValidRule
            OR (lValidRule AND cRuleValue = "") THEN
            DO:
              IF NOT lValidRule
              THEN
                ASSIGN opcError = "Limit checking processing can't be completed. Authorisation Rule NoLimitStatusReason must be added."
                                + "[HELP=Auth Rule Code: NoLimitStatusReason]".
              ELSE
                ASSIGN opcError = "Limit checking processing can't be completed. Authorisation Rule NoLimitStatusReason requires a valid declined status reason."
                                + "[HELP=Auth Rule Code: NoLimitStatusReason]".

              ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - Rule: NoLimitStatusReason - lValidRule="
                                      + STRING(lValidRule) + " cRuleValue=" + cRuleValue + " Error=" + opcError.

              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

              RETURN.
            END.  // IF NOT lValidRule OR (lValidRule AND cRuleValue = "") THEN

            /*
              Authorised value must be declined, and the rest of limits are not required to check.
            */
            ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - Declining Auth with reason " + cRuleValue.

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            ASSIGN opiAuthStatus   = 6  // Decline
                   opcStatusReason = cRuleValue.

            /*
              No need to continue limit checking because authorised value will be declined.
            */
            RETURN.
          END.  // IF tt_limit.limit_left = 0 THEN
          ELSE DO:
            /*
              If part of limit is available get default status reasons according to Rule "ExceedLimitStatusReason".
            */
            mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  ipdInsurerObj,
                                                           INPUT  ipiOptionCode,
                                                           INPUT  "ma_acAuthRuleTypeLimits":U,
                                                           INPUT  "ExceedLimitStatusReason":U,
                                                           INPUT  ipdDate,
                                                           OUTPUT lValidRule,
                                                           OUTPUT cRuleValue).

            /* If the rule exists, with no rule value setup or if no rule exists - An error must be returned because setup is needed to complete limit checking. */
            IF NOT lValidRule
            OR (lValidRule AND cRuleValue = "") THEN
            DO:
              IF NOT lValidRule
              THEN
                ASSIGN opcError = "Limit checking processing can't be completed. Authorisation Rule ExceedLimitStatusReason must be added."
                                + "[HELP=Auth Rule Code: ExceedLimitStatusReason]".
              ELSE
                ASSIGN opcError = "Limit checking processing can't be completed. Authorisation Rule ExceedLimitStatusReason requires a valid status reason."
                                + "[HELP=Auth Rule Code: ExceedLimitStatusReason]".

              ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - Rule: ExceedLimitStatusReason - lValidRule="
                                      + STRING(lValidRule) + " cRuleValue=" + cRuleValue + " Error=" + opcError.

              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

              RETURN.
            END.  /* THEN - If rule setup is missing or rule setup is invalid. */

            ASSIGN cTrackingMessage = "Checklimits on " + ipcAuthLevel + " - Rule: ExceedLimitStatusReason - opcStatusReason=" + opcStatusReason.

            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            ASSIGN opcStatusReason = cRuleValue.
          END.  // ELSE - IF tt_limit.limit_left = 0 THEN
        END.  // ELSE - IF LOOKUP("ma_member_auth_limit_override",gscUserRole) > 0 THEN
      END.  // IF tt_limit.limit_left <  dAuthorisedValue AND NOT iplPMBIndicator THEN

      /*
        When the authorised value exceeds the limit and it is a PMB.
        PMB rules when limit is exceeded:
        - If no limit is left the claim type will be changed to P because the complete authorised value will be allocated to PMB.
        - If limit left is available but it is less than the authorised value the value that exceeds the limit will be allocated to PMB.
      */
      ASSIGN cTrackingMessage = "Checklimits on "    + ipcAuthLevel + " - Check if it is a PMB and the Authorised value exceeds the limit -"
                              + " Limit#="           + STRING(tt_limit.limit_#) + " Left="            + STRING(tt_limit.limit_left)
                              + " dAuthorisedValue=" + STRING(dAuthorisedValue) + " iplPMBIndicator=" + STRING(iplPMBIndicator)
                              + " ipcClaimType="     + ipcClaimType             + " lReserveLimits="  + STRING(lReserveLimits)
                              + " ipdAmount="        + STRING(ipdAmount)        + " ipdQuantity="     + STRING(ipdQuantity).

      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF  tt_limit.limit_left < dAuthorisedValue
      AND iplPMBIndicator
      AND ipcClaimType <> "P":U THEN
      DO:
        IF lReserveLimits THEN
        DO:
          IF tt_limit.limit_left = 0 THEN
          DO:
            ASSIGN opcClaimType = "P":U
                   opdAmount    = ipdAmount
                   opdQuantity  = ipdQuantity.
            /*
              No need to continue limit checking because
              authorised value will be allocated to PMB.
            */
            RETURN.
          END.  // IF tt_limit.limit_left = 0 THEN
          ELSE DO:
            /*
              If both the amount limit and the quantity limit are exceeded,
              the PMB value should rather reflect the amount exceeded.
              If only the quantity limit is exceeded, then the PMB Value should
              reflect the quantity exceeded.
            */
            IF opdPMBValue = 0
            OR cPMBType    = "Quantity":U
            THEN
              ASSIGN cPMBType    = IF tt_limit.to_limit_type
                                   THEN "Amount":U
                                   ELSE "Quantity":U
                     opdPMBValue = dAuthorisedValue - tt_limit.limit_left.
            ASSIGN opdAmount   = ipdAmount
                   opdQuantity = ipdQuantity.
          END.  // ELSE - IF tt_limit.limit_left = 0 THEN
        END.  // IF lReserveLimits THEN
      END.  // IF tt_limit.limit_left < dAuthorisedValue AND iplPMBIndicator AND ipcClaimType <> "P":U THEN
    END.  // LIMITCHECK: FOR EACH tt_limit
  END.  // IF lValidRule AND LOOKUP(ipcAuthLevel,cRuleValue) > 0 THEN

&ENDIF

{ mip/inc/mipcatcherror.i }
