/* maauthbusvalprovider.i  MEDSTAR Medical Aid System
                           Validate Auth Provider Buffer
                           (c) Copyright 1990 - 2022
                           MIP Holdings (Pty) Ltd
                           All rights reserved
*/

DEFINE PARAMETER BUFFER btt_auth_provider FOR tt_auth_provider.
DEFINE PARAMETER BUFFER btt_auth          FOR tt_auth.

DEFINE VARIABLE cAllowDiscountOnUnlimitedRuleValue AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthStatusNote                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthStatus                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthStatusReason                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBlockMessage                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCopayFlagValue                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDiscountType                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErrorField                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErrorMessage                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFieldName                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLateAuthFlag                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLateAuthApplies                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessage                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessageType                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewStatus                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewStatusDescr                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOldStatusNote                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPenaltyApplies                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPenaltyFlag                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPenaltyValueType                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cProviderField                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cProviderLabel                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cProviderList                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cProviderTypeLabel                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrType                            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSubPrType                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleCode                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleDescription                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleType                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleValidValues                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleValue                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cService                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTrackingMessage                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValidMessage                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValidProvider                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWarning                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWarningType                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dAuthCopayControlObj               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAuthCopayTypeObj                  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAuthRuleObj                       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dCopayValue                        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dInsurerObj                        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dLinkAuthRuleObj                   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOldCopayControlObj                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOldCopayValue                     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOldCopayValue%                    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPenaltyCopayTypeObj               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPenaltyValue                      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dEndDate                           AS DATE        NO-UNDO.
DEFINE VARIABLE dStartDate                         AS DATE        NO-UNDO.
DEFINE VARIABLE iAuthCopayObj                      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAuthStatus                        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCountProviders                    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDocNegNum                         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNegNum                            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iOldStatus                         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iOptionCode                        AS INTEGER     NO-UNDO.
DEFINE VARIABLE lAcknowledge                       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lAllowDiscountOnUnlimitedValidRule AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lCheckForCopay                     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lClearPenaltyReason                AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lCopayProvider                     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lCopayValueType                    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lCriticalAuthRule                  AS LOGICAL     NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lDefaultProvider                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lEmergency                         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMandatory                         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lProviderInGroup                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lProviderSetupPresent              AS LOGICAL     NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lRunCheckForPenalty                AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuccess                           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSystemOwned                       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValid                             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidationError                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidGroup                        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidProvider                     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidRule                         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidStatus                       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValuesUnlimited                   AS LOGICAL     NO-UNDO.

DEFINE VARIABLE oAcronymHelper                     AS cls.mipacronym.
DEFINE VARIABLE oAuthRuleSearch                    AS cls.maauthrulesearch  NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

&SCOPED-DEFINE NotSupported  /* The mipEnv class does not support the '&1' service at this time. */                               ~
                             ASSIGN lSuccess = goErrorObject:addError                                                             ~
                                                      (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */  ~
                                                       INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */  ~
                                                       INPUT "":U,                                /* ipcOwningEntityKey       */  ~
                                                       INPUT cFieldName,                          /* ipcFieldName             */  ~
                                                       INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */  ~
                                                       INPUT "mip_MsgEnvErr":U,                   /* ipcMessageGroup          */  ~
                                                       INPUT 23,                                  /* ipiMessageNumber         */  ~
                                                       INPUT cService).                           /* ipcReplaceTextList       */  ~
                             RETURN.                                                                                              ~

DEFINE BUFFER ctt_auth_provider      FOR tt_auth_provider.
DEFINE BUFFER buf_auth_provider      FOR hat_auth_provider. /* This buffer is found once in the beginning and if found, will be available throughout this include
                                                               Please do not use this buffer for any other finds. */
DEFINE BUFFER bhat_auth_provider     FOR hat_auth_provider. /* This buffer can be used when we need to "for each" through the providers, or for "other" finds */
DEFINE BUFFER bhat_auth              FOR hat_auth.
DEFINE BUFFER bbt_auth_detail        FOR tt_auth_detail.
DEFINE BUFFER bbt_auth_copay         FOR tt_auth_copay.
DEFINE BUFFER btt_auth_copay         FOR tt_auth_copay.
DEFINE BUFFER btt_auth_flag_penalty  FOR tt_auth_flag_value.
DEFINE BUFFER btt_auth_flag_lateauth FOR tt_auth_flag_value.
DEFINE BUFFER btt_auth_flag_copay    FOR tt_auth_flag_value.

IF goAuthorisation:InFocus AND AVAILABLE btt_auth_provider THEN
DO:

  IF NOT AVAILABLE buf_auth_schext THEN
  DO:
    FIND FIRST buf_auth_schext NO-LOCK
      WHERE buf_auth_schext.scheme-code = goAuthorisation:MemberOptionCode
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  END.  // IF NOT AVAILABLE buf_auth_schext THEN

  IF btt_auth_provider.auth_provider_obj <= 0.00
  THEN
    ASSIGN btt_auth_provider.pmb_indicator = goAuthorisation:PMBIndicator.

  mipEnv:Health:AuthService:getAuthTypeConfig
    (BUFFER btt_auth,
     BUFFER btt_auth_provider,
     INPUT-OUTPUT TABLE ttAuthTypeConfig).

  FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  ASSIGN oAcronymHelper = NEW cls.mipacronym ( INPUT ?,
                                               INPUT FALSE,
                                               INPUT "ma_acAuthProviderType":U
                                              ).

  FIND buf_auth_provider NO-LOCK
    WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  IF btt_auth_provider.record_action = "MODIFY":U THEN
  DO:
    /*
      Provider records that are added automatically by the system when a Rate Control Change is done
      should not be allowed to be changed or deleted by users.
    */
    IF  btt_auth_provider.rate_change_type <> "ma_acAuthRateChangeTypeNone":U
    AND btt_auth_provider.rate_change_type <> "ma_acAuthRateChangeTypeProviderRevert":U
    AND btt_auth_provider.auth_provider_obj > 0 THEN
    DO:
      IF AVAILABLE buf_auth_provider THEN
      DO:
        IF (btt_auth_provider.provider_type          <> buf_auth_provider.provider_type
        OR  btt_auth_provider.doc_num                <> buf_auth_provider.doc_num
        OR  btt_auth_provider.pr_type                <> buf_auth_provider.pr_type
        OR  btt_auth_provider.sub_pr_type            <> buf_auth_provider.sub_pr_type
        OR  btt_auth_provider.auth_group_obj         <> buf_auth_provider.auth_group_obj
        OR  btt_auth_provider.authorised_service     <> buf_auth_provider.authorised_service
        OR  btt_auth_provider.authorise_all_services <> buf_auth_provider.authorise_all_services
        OR  btt_auth_provider.claim_code             <> buf_auth_provider.claim_code
        OR  btt_auth_provider.claim_type             <> buf_auth_provider.claim_type
        OR  (btt_auth_provider.auth_status           <> buf_auth_provider.auth_status AND NOT goAuthorisation:AuthStatusUpdated)
        OR  (btt_auth_provider.auth_status_note      <> buf_auth_provider.auth_status_note AND NOT goAuthorisation:AuthStatusUpdated)
        OR  btt_auth_provider.amount_auth            <> buf_auth_provider.amount_auth
        OR  btt_auth_provider.quantity_auth          <> buf_auth_provider.quantity_auth)
        AND ( btt_auth_provider.rate_change_type = buf_auth_provider.rate_change_type) //if the rate_change_type differs from the db it means a conversion has taken place and we do not want to validate
        THEN
        DO:
          ASSIGN cErrorMessage = SUBSTITUTE("This Provider/Discipline [&1/&2] was created automatically for an Auth Rate Change - no updates are allowed.",
                                            btt_auth_provider.doc_num, btt_auth_provider.pr_type).
          goErrorObject:addError(INPUT "hatap":U,
                                 INPUT btt_auth_provider.auth_provider_obj,
                                 INPUT "":U,
                                 INPUT "":U,
                                 INPUT btt_auth_provider.line_number,
                                 INPUT cErrorMessage,
                                 INPUT "ERR":U).
        END.  /* IF btt_auth_provider.provider_type <> buf_auth_provider.provider_type... */
      END.  /* IF AVAILABLE buf_auth_provider THEN */
    END.  /* IF btt_auth_provider.rate_change_type <> "":U... */

    /*
       Validate whether the provider type on the provider is a valid acronym.
       This validation was already done in the data access validation (maauthdataservicevalidateauthprovider.i),
       so in theory we should not get an error here. But if for some reason that validation was bypassed,
       we don't want to run into trouble further down when we want to use the acronym-label
       in our error messages.
    */
    ASSIGN lSuccess = oAcronymHelper:focusAcronym
                           (INPUT "KEY",
                            INPUT btt_auth_provider.provider_type
                           ).

    IF NOT oAcronymHelper:AcronymInFocus
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                            (INPUT "hatap":U,                                                            /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth_provider.auth_provider_obj,                                  /* ipdOwningEntityObj       */
                             INPUT "":U,                                                                 /* ipcOwningEntityKey       */
                             INPUT "provider_type":U,                                                    /* ipcFieldName             */
                             INPUT btt_auth_provider.line_number,                                        /* ipiLineNumber            */
                             INPUT "MA":U,                                                               /* ipcMessageGroup          */
                             INPUT 100,    /* The "&1" specified is invalid */                           /* ipiMessageNumber         */
                             INPUT "Authorisation provider type: ":U + btt_auth_provider.provider_type). /* ipcReplaceTextList       */

    ELSE ASSIGN cProviderTypeLabel = oAcronymHelper:AcronymLabel
                lSuccess           = oAcronymHelper:unfocusAcronym().

     /*
      Validate start date
    */
    IF btt_auth_provider.start_date < goAuthorisation:StartDate
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                              (INPUT "hatap":U,                                                                            /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth_provider.auth_provider_obj,                                                  /* ipdOwningEntityObj       */
                               INPUT "":U,                                                                                 /* ipcOwningEntityKey       */
                               INPUT "start_date":U,                                                                       /* ipcFieldName             */
                               INPUT btt_auth_provider.line_number,                                                        /* ipiLineNumber            */
                               INPUT "ma_MsgAuth":U,                                                                       /* ipcMessageGroup          */
                               INPUT 24,   /* The &1 Date &2 cannot be before the &3 Date &4 */                            /* ipiMessageNumber         */
                               INPUT "Provider Start,(":U + STRING(btt_auth_provider.start_date,"9999/99/99") + "),"
                                     + "Auth Header Start,(" + STRING(goAuthorisation:StartDate,"9999/99/99") + ")").      /* ipcReplaceTextList       */

    IF btt_auth.end_date <> ? AND btt_auth_provider.start_date > btt_auth.end_date
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                              (INPUT "hatap":U,                                                                            /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth_provider.auth_provider_obj,                                                  /* ipdOwningEntityObj       */
                               INPUT "":U,                                                                                 /* ipcOwningEntityKey       */
                               INPUT "start_date":U,                                                                       /* ipcFieldName             */
                               INPUT btt_auth_provider.line_number,                                                        /* ipiLineNumber            */
                               INPUT "ma_MsgAuth":U,                                                                       /* ipcMessageGroup          */
                               INPUT 25,   /* The &1 Date &2 cannot be after the &3 Date &4 */                             /* ipiMessageNumber         */
                               INPUT "Provider Start,(" + STRING(btt_auth_provider.start_date,"9999/99/99") + "),"
                                     + "Auth End,(":U + STRING(btt_auth.end_date,"9999/99/99") + ")").                     /* ipcReplaceTextList       */

    /*
      Validate end date
    */
    IF btt_auth_provider.end_date <> ? THEN
    DO:
      IF btt_auth_provider.end_date < btt_auth_provider.start_date
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                              (INPUT "hatap":U,                                                                            /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth_provider.auth_provider_obj,                                                  /* ipdOwningEntityObj       */
                               INPUT "":U,                                                                                 /* ipcOwningEntityKey       */
                               INPUT "end_date":U,                                                                         /* ipcFieldName             */
                               INPUT btt_auth_provider.line_number,                                                        /* ipiLineNumber            */
                               INPUT "ma_MsgAuth":U,                                                                       /* ipcMessageGroup          */
                               INPUT 24,   /* The &1 Date &2 cannot be before the &3 Date &4 */                            /* ipiMessageNumber         */
                               INPUT "Provider End,(":U + STRING(btt_auth_provider.end_date,"9999/99/99") + "),"
                                     + "Provider Start,(" + STRING(btt_auth_provider.start_date,"9999/99/99") + ")").      /* ipcReplaceTextList       */
      ELSE IF btt_auth.end_date <> ? AND btt_auth_provider.end_date > btt_auth.end_date
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,                                                                  /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth_provider.auth_provider_obj,                                        /* ipdOwningEntityObj       */
                                 INPUT "":U,                                                                       /* ipcOwningEntityKey       */
                                 INPUT "end_date":U,                                                               /* ipcFieldName             */
                                 INPUT btt_auth_provider.line_number,                                              /* ipiLineNumber            */
                                 INPUT "ma_MsgAuth":U,                                                             /* ipcMessageGroup          */
                                 INPUT 25,   /* The &1 Date &2 cannot be after the &3 Date &4 */                   /* ipiMessageNumber         */
                                 INPUT "Provider End,(" + STRING(btt_auth_provider.end_date,"9999/99/99") + "),"
                                     + "Auth End,(":U + STRING(btt_auth.end_date,"9999/99/99") + ")").             /* ipcReplaceTextList       */
    END. /*IF (btt_auth_provider.end_date <> ? THEN */

    /*
      Validate provider discipline
      This will ensure that if a pr type restriction list has been specified against the auth type,
      that the providers discipline is valid according to that restriction list.
    */
    IF btt_auth_provider.doc_num <> 0 THEN
    DO:
      /***************DOING A FETCH AUTHTYPE CALL **************/
      mipEnv:Health:AuthService:validateProviderDiscipline(INPUT STRING(btt_auth_provider.doc_num),
                                                           INPUT STRING(btt_auth_provider.pr_type),
                                                           INPUT STRING(btt_auth_provider.sub_pr_type),
                                                           INPUT btt_auth_provider.start_date,
                                                           OUTPUT lValid,
                                                           OUTPUT cMessage).
      IF NOT lValid AND cMessage <> "":U
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                            (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                             INPUT "":U,                                /* ipcOwningEntityKey       */
                             INPUT "doc_num":U,                         /* ipcFieldName             */
                             INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                             INPUT cMessage,                            /* ipcMessageText           */
                             INPUT "ERR":U).                            /* ipcMessageType           */
    END. /*IF btt_auth_provider.doc_num <> 0 THEN*/

    /*
      Validate discipline
    */

    IF (btt_auth_provider.auth_provider_obj <= 0                                    /* new detail line is added*/
    AND (btt_auth_provider.pr_type <> 0 OR btt_auth_provider.sub_pr_type <>  0))
    OR
      (AVAILABLE buf_auth_provider                                                  /* the pr_type/sub_pr_type is updated on an existing provider record */
    AND ((buf_auth_provider.pr_type <> btt_auth_provider.pr_type
    AND   buf_auth_provider.pr_type <> 0)
    OR   (buf_auth_provider.sub_pr_type <> btt_auth_provider.pr_type
    AND   buf_auth_provider.sub_pr_type <> 0)))
    AND btt_auth_provider.auth_status <> 6 THEN                                     /* Declined records */
    DO:  
      ASSIGN
        cPrType    = STRING(btt_auth_provider.pr_type)
        cSubPrType = STRING(btt_auth_provider.sub_pr_type).

      { ma/msc/madispad.i &discipline = cPrType }
      { ma/msc/madispad.i &discipline = cSubPrType &comment = "/* "}

      IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.PrTypeValidList <> "":U
      AND LOOKUP(cPrType + cSubPrType, ttAuthTypeConfig.PrTypeValidList) <= 0 THEN
      DO:
        ASSIGN
          cErrorMessage = SUBSTITUTE("Discipline '&1' may not be used for auth type '&2'. Only the following disciplines may be used '&3'.":U, cPrType, ttAuthTypeConfig.AuthType, ttAuthTypeConfig.PrTypeValidList)
          lSuccess      = goErrorObject:addError(
                           INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                           INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                           INPUT "":U,                                /* ipcOwningEntityKey       */
                           INPUT "pr_type":U,                         /* ipcFieldName             */
                           INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                           INPUT cErrorMessage,                       /* ipcMessageText           */
                           INPUT "ERR":U).                            /* ipcMessageType           */

      END. /* IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.PrTypeValidList <> "":U */

      mipEnv:Health:AuthService:validateAuthTypeDisciplineExcl(INPUT goAuthorisation:AuthTypeObj,
                                                               INPUT goAuthorisation:InsurerObj,
                                                               INPUT goAuthorisation:MemberOptionCode,
                                                               INPUT ttAuthTypeConfig.AuthType,
                                                               INPUT btt_auth_provider.provider_type,
                                                               INPUT STRING(btt_auth_provider.pr_type),
                                                               INPUT btt_auth_provider.auth_group_obj,
                                                               INPUT STRING(btt_auth_provider.sub_pr_type),
                                                               INPUT btt_auth_provider.start_date,
                                                               OUTPUT cAuthStatus,
                                                               OUTPUT cAuthStatusReason,
                                                               OUTPUT lValid,
                                                               OUTPUT cMessage).

      IF NOT lValid  AND cMessage <> "":U
      AND INTEGER(cAuthStatus) <> 0 AND cAuthStatusReason <> "":U THEN
      DO:
        ASSIGN btt_auth_provider.auth_status		  = INTEGER(cAuthStatus)
               btt_auth_provider.auth_status_note	= cAuthStatusReason.

        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                       INPUT  goAuthorisation:OptionCode,
                                                       INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                       INPUT  "AuthTypeExclWarn":U,
                                                       INPUT  btt_auth_provider.start_date,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).

        IF lValidRule AND LOOKUP(cRuleValue, "Warn,WarnAck":U) > 0 THEN
        DO:
          IF cRuleValue = "WarnAck":U
          THEN ASSIGN lAcknowledge = TRUE.
          ELSE ASSIGN lAcknowledge = FALSE.

          lSuccess = goErrorObject:addError(
                    INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                    INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                    INPUT "":U,                                /* ipcOwningEntityKey       */
                    INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                    INPUT cMessage,                            /* ipcMessageText           */
                    INPUT "WAR":U,                             /* ipcMessageType           */
                    INPUT lAcknowledge).                       /* iplAcknowledge           */
        END. /* IF lValidRule THEN */
      END. /* IF NOT lValid THEN */
    END. /* IF (btt_auth_provider.auth_provider_obj <= 0 */

    /*
      Discount-Type
    */
    ASSIGN cErrorMessage = "".

    IF btt_auth_provider.discount_type <> ? THEN
    DO:
      IF (btt_auth_provider.amount_auth   <> 0
      OR  btt_auth_provider.quantity_auth <> 0)
      AND btt_auth_provider.discount_auth <> 0 THEN
      DO:
        ASSIGN lCriticalAuthRule = FALSE
               cErrorMessage     = "The Auth Rule has not been set up or does not have 'Provider' information set up."
                                 + "[HELP=Auth Rule Code: AllowDiscountOnLimited]".

        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                       INPUT  "AllowDiscountOnLimited":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).


        IF lValidRule AND (INDEX(cRuleValue, "Provider=":U) > 0)
        THEN
          ASSIGN lProviderSetupPresent = TRUE
                 lCriticalAuthRule     = TRUE.
      END. /* IF (btt_auth_provider.amount_auth <> 0 OR btt_auth_provider.quantity_auth <> 0) AND btt_auth_provider.discount_auth <> 0 THEN */

      IF NOT lCriticalAuthRule
      THEN
        ASSIGN
          lSuccess = goErrorObject:addError(INPUT "hatap":U,
                                            INPUT btt_auth_provider.auth_provider_obj,
                                            INPUT "":U,
                                            INPUT "discount_auth":U,
                                            INPUT btt_auth_provider.line_number,
                                            INPUT cErrorMessage,
                                            INPUT "ERR":U).
      ELSE DO:
        /*
          Discount percentage validation
        */
        IF btt_auth_provider.discount_type = TRUE THEN
        DO:
          ASSIGN cDiscountType = " percentage ".

          IF lProviderSetupPresent THEN
          DO:
            ASSIGN cRuleValue = REPLACE(ENTRY(1,SUBSTRING(cRuleValue,INDEX(cRuleValue,"Provider=")),"|"),"Provider=", "").
            IF NOT LOOKUP (TRIM(cDiscountType),cRuleValue) > 0
            THEN
              ASSIGN
                cErrorMessage     = "Please note that the rule does not support a discount type of" + cDiscountType
                             		  + "selection. The rule setup for detail lines only supports " + cRuleValue
                             		  + ".[HELP=Auth Rule Code: AllowDiscountOnLimited]"
                lCriticalAuthRule = FALSE
                lSuccess          =  goErrorObject:addError(INPUT "hatap":U,
                                                            INPUT btt_auth_provider.auth_provider_obj,
                                                            INPUT "":U,
                                                            INPUT "discount_auth":U,
                                                            INPUT btt_auth_provider.line_number,
                                                            INPUT cErrorMessage,
                                                            INPUT "ERR":U).
          END. /* IF lProviderSetupPresent THEN */

          IF btt_auth_provider.discount_auth > 100 AND lCriticalAuthRule
          THEN
            ASSIGN
              cErrorMessage = "Discount percentage value cannot be greater than 100, amount will be reverted to previous value if available."
              lSuccess      = goErrorObject:addError(INPUT "hatap":U,
                                                     INPUT btt_auth_provider.auth_provider_obj,
                                                     INPUT "":U,
                                                     INPUT "discount_auth":U,
                                                     INPUT btt_auth_provider.line_number,
                                                     INPUT cErrorMessage,
                                                     INPUT "ERR":U).

        END. /* IF btt_auth_provider.discount_type = TRUE */

        /*
          Discount amount (Rand) validation
        */
        IF btt_auth_provider.discount_type = FALSE THEN
        DO:
          ASSIGN cDiscountType = " amount ".

          IF lProviderSetupPresent THEN
          DO:

            ASSIGN cRuleValue = REPLACE(ENTRY(1,SUBSTRING(cRuleValue,INDEX(cRuleValue,"Provider=")),"|"),"Provider=", "").

            IF NOT LOOKUP (TRIM(cDiscountType),cRuleValue) > 0
            THEN
              ASSIGN
                cErrorMessage     = "Please note that the rule does not support a discount type of" + cDiscountType
                                  + "selection. The rule setup for detail lines only supports " + cRuleValue
                                  + ".[HELP=Auth Rule Code: AllowDiscountOnLimited]"
                lCriticalAuthRule = FALSE
                lSuccess          =  goErrorObject:addError(INPUT "hatap":U,
                                                            INPUT btt_auth_provider.auth_provider_obj,
                                                            INPUT "":U,
                                                            INPUT "discount_auth":U,
                                                            INPUT btt_auth_provider.line_number,
                                                            INPUT cErrorMessage,
                                                            INPUT "ERR":U).
          END. /* IF lProviderSetupPresent THEN */

          IF  btt_auth_provider.discount_auth > btt_auth_provider.amount_auth
          AND btt_auth_provider.amount_auth   > 0
          AND lCriticalAuthRule
          THEN
            ASSIGN
              cErrorMessage = "Discount amount can not be greater than the authorised amount, amount will be reverted to previous value if available":U
              lSuccess      = goErrorObject:addError(INPUT "hatap":U,
                                                     INPUT btt_auth_provider.auth_provider_obj,
                                                     INPUT "":U,
                                                     INPUT "discount_auth":U,
                                                     INPUT btt_auth_provider.line_number,
                                                     INPUT cErrorMessage,
                                                     INPUT "ERR":U).
        END. /* IF btt_auth_provider.discount_type = FALSE */

        IF  btt_auth_provider.discount_auth <= 0
        AND lCriticalAuthRule
        THEN
          ASSIGN
            cErrorMessage = "Discount" + cDiscountType + "cannot be less than or equal to 0, amount will be reverted to previous value if available"
            lSuccess      = goErrorObject:addError(INPUT "hatap":U,
                                                   INPUT btt_auth_provider.auth_provider_obj,
                                                   INPUT "":U,
                                                   INPUT "discount_auth":U,
                                                   INPUT btt_auth_provider.line_number,
                                                   INPUT cErrorMessage,
                                                   INPUT "ERR":U).

        /*
          Check rule to see whether we should return an error or a warning.
        */
        IF  btt_auth_provider.amount_auth   = 0
        AND btt_auth_provider.discount_auth > 0
        AND btt_auth_provider.discount_auth > 0
        AND lCriticalAuthRule THEN
        DO:
          ASSIGN cErrorMessage = "A" + cDiscountType.

          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                         INPUT  goAuthorisation:OptionCode,
                                                         INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                         INPUT  "AllowDiscountOnUnlimited":U,
                                                         INPUT  btt_auth_provider.start_date,
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
              cErrorMessage = cErrorMessage + "has been specified on an Unlimited Authorisation (Authorised Amount & Quantity is zero)"
              cMessageType  = "WAR":U.
          ELSE
            ASSIGN
              cErrorMessage = cErrorMessage + "may not be specified on an Unlimited Authorisation (Authorised Amount & Quantity is zero)"
              cMessageType  = "ERR":U.

          ASSIGN
	    cErrorMessage = cErrorMessage + "[HELP=Auth Rule Code: AllowDiscountOnUnlimited]"
            lSuccess 	  = goErrorObject:addError(INPUT "hatap":U,
                     	                           INPUT btt_auth_provider.auth_provider_obj,
                     	                           INPUT "":U,
                     	                           INPUT "discount_auth":U,
                     	                           INPUT btt_auth_provider.line_number,
                     	                           INPUT cErrorMessage,
                     	                           INPUT cMessageType).

        END. /* IF btt_auth_provider.amount_auth = 0... */
      END. /* ELSE DO: IF NOT lCriticalAuthRule */
    END. /* IF btt_auth_provider.discount_type <> ? THEN */

    IF   btt_auth_provider.discount_type = ?
    AND (btt_auth_provider.discount_auth <> 0.00 AND btt_auth_provider.discount_auth <> ?)
    THEN
      ASSIGN
        cErrorMessage = "There is a discount authorised value entered but no discount type. " +
                        "Please enter a discount type of either percent or Rand or remove the discount authorised value."
        lSuccess      = goErrorObject:addError(INPUT "hatap":U,
                                               INPUT btt_auth_provider.auth_provider_obj,
                                               INPUT "":U,
                                               INPUT btt_auth_provider.line_number,
                                               INPUT cErrorMessage,
                                               INPUT "ERR":U).

    /*
       MCUPRMEM Rule: Indicates whether a provider who is also a member on the auth should be blocked
    */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                   INPUT  0,
                                                   INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                   INPUT  "MCUPRMEM":U,
                                                   INPUT  btt_auth_provider.start_date,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).

    IF lValidRule AND cRuleValue = "yes":U THEN
    DO:
      FOR FIRST memdep NO-LOCK
          WHERE memdep.mem-num   = goAuthorisation:MemNum
            AND memdep.dependant = goAuthorisation:dependant:

        IF CAN-FIND(FIRST doctor
                    WHERE doctor.doc-num = btt_auth_provider.doc_num
                      AND doctor.id-num  = memdep.id-num)
        THEN
          ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                                                             /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                                   /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                                  /* ipcOwningEntityKey       */
                                   INPUT "doc_num":U,                                                           /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                         /* ipiLineNumber            */
                                   INPUT "MA":U,                                                                /* ipcMessageGroup          */
                                   INPUT 102,  /* Cannot run because &1. */                                     /* ipiMessageNumber         */
                                   INPUT "the provider is also the member.[HELP=Auth Rule Code: MCUPRMEM] ":U). /* ipcReplaceTextList       */
      END. /* FOR FIRST memdep */
    END. /* IF lValidRule (MCUPRMEM) AND cRuleValue = "yes":U THEN */

    /*
      If a group provider has been populated, ensure that an attending provider has also been specified
    */
    ASSIGN lValidationError = (btt_auth_provider.group_doc_num <> 0 AND
                               btt_auth_provider.doc_num        = 0 AND
                               mipEnv:Health:maDoctor:isProviderAValidGroup(INPUT btt_auth_provider.group_doc_num,
                                                                            INPUT btt_auth_provider.start_date))

           cProviderLabel   = IF btt_auth_provider.group_doc_num = 0
                              THEN "Provider"
                              ELSE "Attending Provider"

           cProviderField   = IF btt_auth_provider.group_doc_num = 0
                              THEN "doc_num":U
                              ELSE "group_doc_num":U .

    IF lValidationError
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                              (INPUT "hatap":U,                                   /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth_provider.auth_provider_obj,         /* ipdOwningEntityObj       */
                               INPUT "":U,                                        /* ipcOwningEntityKey       */
                               INPUT "doc_num":U,                                 /* ipcFieldName             */
                               INPUT btt_auth_provider.line_number,               /* ipiLineNumber            */
                               INPUT "MA":U,                                      /* ipcMessageGroup          */
                               INPUT 111,   /* The &1 must be specified. &2 */    /* ipiMessageNumber         */
                               INPUT cProviderLabel +
                                     ",This is a group practice and therefor the attending provider is mandatory."
                                     + "[HELP=Auth Rule Code: ProviderGroup]":U). /* ipcReplaceTextList       */

    /*
      Validate group doc num
    */
    IF btt_auth_provider.group_doc_num <> 0 THEN
    DO:
      ASSIGN
        lValidProvider = FALSE
        lValidProvider = mipEnv:Health:maDoctor:isProviderValid( INPUT btt_auth_provider.group_doc_num,
                                                                 INPUT btt_auth_provider.start_date,
                                                                 INPUT btt_auth_provider.end_date).
      IF NOT lValidProvider
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,                                                             /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth_provider.auth_provider_obj,                                   /* ipdOwningEntityObj       */
                                 INPUT "":U,                                                                  /* ipcOwningEntityKey       */
                                 INPUT "group_doc_num":U,                                                     /* ipcFieldName             */
                                 INPUT btt_auth_provider.line_number,                                         /* ipiLineNumber            */
                                 INPUT "ma_MsgProvider":U,                                                    /* ipcMessageGroup          */
                                 INPUT 2,  /* The &1 &2 is inactive. */                                       /* ipiMessageNumber         */
                                 INPUT "Group provider,(":U + STRING(btt_auth_provider.group_doc_num) + ")"). /* ipcReplaceTextList       */

    END. /* IF  btt_auth_provider.group_doc_num <> 0 THEN */
    ELSE DO: 
      /*
        Either a provider or a pr_type should be entered
      */
      IF  btt_auth_provider.doc_num = 0
      AND btt_auth_provider.pr_type = 0
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,                                                                             /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth_provider.auth_provider_obj,                                                   /* ipdOwningEntityObj       */
                                 INPUT "":U,                                                                                  /* ipcOwningEntityKey       */
                                 INPUT "doc_num":U,                                                                           /* ipcFieldName             */
                                 INPUT btt_auth_provider.line_number,                                                         /* ipiLineNumber            */
                                 INPUT "ma_MsgAuth":U,                                                                        /* ipcMessageGroup          */
                                 INPUT 9, /* Please ensure that either a valid provider number or discipline is specified */  /* ipiMessageNumber         */
                                 INPUT "").                                                                                   /* ipcReplaceTextList       */
    END.   /* ELSE - IF btt_auth_provider.group_doc_num <> 0 THEN */

    /*
      Validate doc num
    */
    IF btt_auth_provider.doc_num <> 0 THEN
    DO:
      ASSIGN
        lValidProvider = FALSE
        lValidProvider = mipEnv:Health:maDoctor:isProviderValid( INPUT btt_auth_provider.doc_num,
                                                                 INPUT btt_auth_provider.start_date,
                                                                 INPUT btt_auth_provider.end_date).
      IF NOT lValidProvider
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,                                                          /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth_provider.auth_provider_obj,                                /* ipdOwningEntityObj       */
                                 INPUT "":U,                                                               /* ipcOwningEntityKey       */
                                 INPUT cProviderField,                                                     /* ipcFieldName             */
                                 INPUT btt_auth_provider.line_number,                                      /* ipiLineNumber            */
                                 INPUT "ma_MsgProvider":U,                                                 /* ipcMessageGroup          */
                                 INPUT 2,  /* The &1 &2 is inactive. */                                    /* ipiMessageNumber         */
                                 INPUT cProviderLabel + ",(":U + STRING(btt_auth_provider.doc_num) + ")"). /* ipcReplaceTextList       */

    END. /* IF  btt_auth_provider.doc_num <> 0 THEN DO: */

    /*
      If a group provider and an attending provider has been specified ensure that the attending provider is valid for the group
    */
    IF  btt_auth_provider.group_doc_num <> 0
    AND btt_auth_provider.doc_num <> 0 THEN
    DO:
      ASSIGN lSuccess = mipEnv:Health:maDoctor:validateProviderInGroup(INPUT  btt_auth_provider.group_doc_num,
                                                                       INPUT  btt_auth_provider.doc_num,
                                                                       INPUT  btt_auth_provider.start_date,
                                                                       OUTPUT lValidGroup,
                                                                       OUTPUT cValidProvider,
                                                                       OUTPUT cMessage).
      IF lSuccess THEN
      DO:
        IF NOT lValidGroup
        THEN
          ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                                                     /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                           /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                          /* ipcOwningEntityKey       */
                                   INPUT "group_doc_num":U,                                             /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                 /* ipiLineNumber            */
                                   INPUT "ma":U,                                                        /* ipcMessageGroup          */
                                   INPUT 112,  /* The &1 specified is invalid. &2 */                    /* ipiMessageNumber         */
                                   INPUT "Group Practice: " + STRING(btt_auth_provider.group_doc_num) +
                                         "," + cMessage). /* ipcReplaceTextList       */

        ELSE IF cValidProvider = "Invalid":U
        THEN
          ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                                                                          /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                                                /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                                               /* ipcOwningEntityKey       */
                                   INPUT "group_doc_num":U,                                                                  /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                                      /* ipiLineNumber            */
                                   INPUT "ma_MsgProvider":U,                                                                 /* ipcMessageGroup          */
                                   INPUT 1,   /* The attending provider &1 is not valid for the group practice &2. */        /* ipiMessageNumber         */
                                   INPUT STRING(btt_auth_provider.doc_num) + "," +
                                         STRING(btt_auth_provider.group_doc_num) + cMessage).                                /* ipcReplaceTextList       */

		  ELSE IF cMessage <> "":U
		  THEN
		    ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                                                     /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                           /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                          /* ipcOwningEntityKey       */
                                   INPUT "group_doc_num":U,                                             /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                 /* ipiLineNumber            */
                                   INPUT "Group Practice: " + STRING(btt_auth_provider.group_doc_num) + ","
                                          + cMessage,
								   INPUT "Err").

      END. /* IF lSuccess THEN */
      ELSE DO:
         ASSIGN cFieldName = "doc_num":U
                cService   = "validateProviderInGroup":U.

         {&NotSupported}
      END. /* ELSE DO: (lSuccess = false)*/
    END.  /* IF btt_auth_provider.group_doc_num <> 0 AND btt_auth_provider.doc_num <> 0 THEN */

    /*
       Check auth provider values against the auth type provider setups.
       Since the auth type is specified on the auth header, goAuthorisation must be in focus.

       Validate the claim code and claim type on the auth provider record
    */
    IF btt_auth_provider.claim_code <> 0 THEN
    DO:
      ASSIGN iNegNum  =  INTEGER(TRIM(ENTRY(1, btt_auth_provider._neg_group, ":"), "("))
             lSuccess = mipEnv:Health:AuthBusinessLogic:validateClaimCode(INPUT  goAuthorisation:InsurerObj,
                                                                          INPUT  goAuthorisation:OptionCode,
                                                                          INPUT  goAuthorisation:MemNum,
                                                                          INPUT  goAuthorisation:Dependant,
                                                                          INPUT  btt_auth_provider.claim_code,
                                                                          INPUT  goAuthorisation:AuthTypeObj,
                                                                          INPUT  goAuthorisation:StartDate,
                                                                          INPUT  btt_auth_provider.provider_type,
                                                                          INPUT  btt_auth_provider.pr_type,
                                                                          INPUT  btt_auth_provider.sub_pr_type,
                                                                          INPUT  iNegNum,
                                                                          INPUT  "hat_auth_provider":U ,
                                                                          OUTPUT lValidationError,
                                                                          OUTPUT cValidMessage).
      IF lSuccess THEN
      DO:
        IF cValidMessage <> "" THEN
        DO:
          ASSIGN cMessageType  = (IF lValidationError = TRUE
                                  THEN "WAR":U
                                  ELSE "ERR":U)

                 lSuccess      = goErrorObject:addError
                                     (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                                      INPUT "":U,                                /* ipcOwningEntityKey       */
                                      INPUT "claim_code":U,                      /* ipcFieldName             */
                                      INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                                      INPUT cValidMessage,                       /* ipcMessageText           */
                                      INPUT cMessageType).                       /* ipcMessageType           */
        END. /* IF cValidMessage <> "" THEN */
      END.  /* IF lSuccess THEN */
      ELSE
      DO:
        ASSIGN cFieldName = "claim_code":U
               cService   = "validateClaimCode":U.

        {&NotSupported}
      END. /* ELSE DO: (lSuccess = false)*/
    END.  /* IF btt_auth_provider.claim_code <> 0 THEN */

    /*
      Claim type validation
    */
    IF btt_auth_provider.claim_type <> "":U THEN
    DO:
      ASSIGN iNegNum  = INTEGER(TRIM(ENTRY(1, btt_auth_provider._neg_group, ":"), "("))
             lSuccess = mipEnv:Health:AuthBusinessLogic:validateClaimType(INPUT  goAuthorisation:InsurerObj,
                                                                          INPUT  goAuthorisation:OptionCode,
                                                                          INPUT  btt_auth_provider.claim_type,
                                                                          INPUT  goAuthorisation:AuthTypeObj,
                                                                          INPUT  goAuthorisation:StartDate,
                                                                          INPUT  btt_auth_provider.provider_type,
                                                                          INPUT  btt_auth_provider.pr_type,
                                                                          INPUT  btt_auth_provider.sub_pr_type,
                                                                          INPUT  iNegNum,
                                                                          INPUT  "hat_auth_provider":U ,
                                                                          OUTPUT lValidationError,
                                                                          OUTPUT cValidMessage).
      IF lSuccess THEN
      DO:
        IF cValidMessage <> "" THEN
        DO:
          ASSIGN cMessageType = (IF lValidationError = TRUE
                                 THEN "WAR":U
                                 ELSE "ERR":U)

                 lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                                 INPUT "":U,                                /* ipcOwningEntityKey       */
                                 INPUT "claim_type":U,                      /* ipcFieldName             */
                                 INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                                 INPUT cValidMessage,                       /* ipcMessageText           */
                                 INPUT cMessageType).                       /* ipcMessageType           */
        END. /* IF cValidMessage <> "" THEN */
      END.  /* IF lSuccess THEN */
      ELSE DO:
        ASSIGN cFieldName = "claim_type":U
               cService   = "validateClaimType":U.

        {&NotSupported}
      END. /* ELSE DO: (lSuccess = false)*/
    END.  /* IF btt_auth_provider.claim_type <> "":U THEN */

    /*
      Auth Type Configuration Validations
    */
    IF AVAILABLE ttAuthTypeConfig THEN
    DO:
       mipEnv:Health:AuthService:validateAuthorisedValues(INPUT  btt_auth_provider.amount_auth,
                                                          INPUT  btt_auth_provider.quantity_auth,
                                                          INPUT  ttAuthTypeConfig.ActivateAuthorisedValues,
                                                          INPUT  ttAuthTypeConfig.HeaderValuesUnlimited,
                                                          INPUT  ttAuthTypeConfig.HeaderValuesAllowed,
                                                          OUTPUT cFieldName,
                                                          OUTPUT cValidMessage).
      IF cValidMessage <> ""
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                              (INPUT "hatap":U,                                   /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth_provider.auth_provider_obj,         /* ipdOwningEntityObj       */
                               INPUT "":U,                                        /* ipcOwningEntityKey       */
                               INPUT cFieldName,                                  /* ipcFieldName             */
                               INPUT btt_auth_provider.line_number,               /* ipiLineNumber            */
                               INPUT cValidMessage,                               /* ipcMessageText           */
                               INPUT "ERR":U).                                    /* ipcMessageType           */


      FIND FIRST tt_auth_type NO-LOCK
           WHERE tt_auth_type.auth_type_obj = goAuthorisation:AuthTypeObj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      /*
        Doc Num Mandatory
      */
      IF  AVAILABLE tt_auth_type
      AND ttAuthTypeConfig.DocNumMandatory = TRUE
      AND btt_auth_provider.doc_num        = 0
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                                                                                 /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                                                       /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                                                      /* ipcOwningEntityKey       */
                                   INPUT "doc_num":U,                                                                               /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                                             /* ipiLineNumber            */
                                   INPUT "ma_MsgAuth":U,                                                                            /* ipcMessageGroup          */
                                   INPUT 26,   /* The provider number must be specified for the &1 provider on auth type '&2'. */   /* ipiMessageNumber         */
                                   INPUT cProviderTypeLabel + "," + tt_auth_type.auth_type).                                        /* ipcReplaceTextList       */

      /*
        Authorised service validation
      */
      IF AVAILABLE tt_auth_type
      AND ttAuthTypeConfig.AuthorisedService = TRUE
      AND NOT btt_auth_provider.authorised_service
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                                                                                 /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                                                       /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                                                      /* ipcOwningEntityKey       */
                                   INPUT "authorised_service":U,                                                                    /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                                             /* ipiLineNumber            */
                                   INPUT "ma_MsgAuth":U,                                                                            /* ipcMessageGroup          */
                                   INPUT 27,   /* The authorised service must be selected for the &1 provider on auth type '&2'. */ /* ipiMessageNumber         */
                                   INPUT cProviderTypeLabel + "," + tt_auth_type.auth_type).                                        /* ipcReplaceTextList       */

      /*
         Only check the maximum number of providers allowed when the auth is complete
      */
      IF  goAuthorisation:AuthIncomplete = FALSE
      AND ttAuthTypeConfig.NumberProvidersAllowed <> ?
      AND ttAuthTypeConfig.NumberProvidersAllowed <> 0 THEN
      DO:
        /*
          MMP-874
          Find temp-table tt_provider_count for the auth obj & provider type before
          starting the "validation code"
        */
        FIND FIRST tt_provider_count NO-LOCK
             WHERE tt_provider_count.auth_obj      = goAuthorisation:AuthObj
             AND   tt_provider_count.provider_type = ttAuthTypeConfig.ProviderType
          NO-ERROR.
        IF NOT AVAILABLE tt_provider_count THEN
        DO:
          CREATE tt_provider_count.
          ASSIGN
            tt_provider_count.auth_obj       = goAuthorisation:AuthObj
            tt_provider_count.provider_type  = ttAuthTypeConfig.ProviderType
            tt_provider_count.provider_count = 0.
        END. /* IF NOT AVAILABLE tt_provider_count */

        /*
          If provider_count = 0, then count the records, and save it to tt_provider_count.provider_count field.
          If the counter <> 0, skip the validation (this provider type was already counted and validated
        */
        IF tt_provider_count.provider_count = 0 THEN
        DO:
          /*
             Count the records in the temp-table
          */
          FOR EACH ctt_auth_provider NO-LOCK
             WHERE ctt_auth_provider.auth_obj      = goAuthorisation:AuthObj
               AND ctt_auth_provider.provider_type = ttAuthTypeConfig.ProviderType:

            IF NOT CAN-DO("0,1,7":U, STRING(ctt_auth_provider.auth_status))
            THEN
              NEXT.

            ASSIGN tt_provider_count.provider_count = tt_provider_count.provider_count + 1.
          END.  /* FOR EACH btt_auth_provider NO-LOCK */

         /*
           Limit exceeded
         */
          IF tt_provider_count.provider_count > ttAuthTypeConfig.NumberProvidersAllowed THEN
          DO:

            ASSIGN cMessage = SUBSTITUTE("No more than &1 &2 provider&3 allowed on &4 &5 authorisation.":U,
                                       STRING(ttAuthTypeConfig.NumberProvidersAllowed),
                                       cProviderTypeLabel,
                                       (IF ttAuthTypeConfig.NumberProvidersAllowed > 1 THEN "s":U ELSE "":U),
                                       (IF LOOKUP(SUBSTRING(tt_auth_type.auth_type,1,1),"a,e,i,o,u":U) > 0 THEN "an":U ELSE "a":U),
                                       tt_auth_type.auth_type)

                   lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,                            /* ipcOwningEntityMnemonic  */
                                 INPUT btt_auth_provider.auth_provider_obj,  /* ipdOwningEntityObj       */
                                 INPUT "":U,                                 /* ipcOwningEntityKey       */
                                 INPUT btt_auth_provider.line_number,        /* ipiLineNumber            */
                                 INPUT cMessage,                             /* ipcMessageText           */
                                 INPUT "ERR":U).                             /* ipiMessageType           */

          END. /* IF tt_provider_count.provider_count > ttAuthTypeConfig.NumberProvidersAllowed THEN */
        END. /* IF tt_provider_count.provider_count = 0 */
      END. /* IF goAuthorisation:AuthIncomplete = FALSE AND ttAuthTypeConfig.NumberProvidersAllowed <> ? AND ttAuthTypeConfig.NumberProvidersAllowed <> 0 THEN */

      IF btt_auth_provider.main_provider = TRUE THEN
      DO:
        /*
          Validate Penalty override reason if penalty-flag = no for main provider
        */
        IF  ttAuthTypeConfig.ActivatePenalty
        AND btt_auth_provider.authorised_service
        AND btt_auth_provider._penalty_flag_updated
        AND btt_auth_provider._penalty_flag          = "NO":U
        AND btt_auth_provider._penalty_override_note = ""
        AND goAuthorisation:AuthIncomplete
        THEN
          ASSIGN
            cMessage = 'Penalty override reason is mandatory when penalty flag is set to no':U
            lSuccess = goErrorObject:addError
                                        (INPUT "hatap":U,                                /* ipcOwningEntityMnemonic  */
                                         INPUT btt_auth_provider.auth_provider_obj,      /* ipdOwningEntityObj       */
                                         INPUT "":U,                                     /* ipcOwningEntityKey       */
                                         INPUT "_penalty_override_note":U,               /* ipcFieldName             */
                                         INPUT btt_auth_provider.line_number,            /* ipiLineNumber            */
                                         INPUT "MA":U,                                   /* ipcMessageGroup          */
                                         INPUT 111, /* The &1 must be specified. &2 */   /* ipiMessageNumber         */
                                         INPUT "Penalty Override Reason," + cMessage).   /* ipcReplaceTextList       */

        IF ttAuthTypeConfig.ActivateCopayment = yes
        OR ttAuthTypeConfig.ActivatePenalty   = yes
        THEN DO:

            IF btt_auth_provider._emergency_flag = ""
            THEN
              ASSIGN
                cMessage = 'Emergency flag must be captured if co-payment or penalty is activated on the "Authorisation Type".':U
                lSuccess = goErrorObject:addError
                                         (INPUT "hatap":U,                             /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj       */
                                          INPUT "":U,                                  /* ipcOwningEntityKey       */
                                          INPUT "":U,                                  /* ipcFieldName             */
                                          INPUT btt_auth_provider.line_number,         /* ipiLineNumber            */
                                          INPUT cMessage,                              /* ipcMessageText           */
                                          INPUT "ERR":U).                              /* ipiMessageType           */

        END. /* IF ttAuthTypeConfig.ActivateCopayment = yes OR ttAuthTypeConfig.ActivatePenalty   = yes THEN DO:*/

      END. /* IF btt_auth_provider.main_provider = TRUE THEN */
    END. /*IF AVAILABLE ttAuthTypeConfig THEN */

    /*
      Multiple "Mandatory provider" validation
        - Check temp table and database
    */
    IF btt_auth_provider.main_provider THEN
    DO:
      ASSIGN iCountProviders = 0.

      FOR EACH ctt_auth_provider NO-LOCK
         WHERE ctt_auth_provider.auth_obj = goAuthorisation:AuthObj
           AND ctt_auth_provider.main_provider:

         ASSIGN iCountProviders = iCountProviders + 1.

      END.  /* FOR EACH ctt_auth_provider NO-LOCK */

      IF iCountProviders > 1
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                                                          /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                                /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                               /* ipcOwningEntityKey       */
                                   INPUT "main_provider":U,                                                  /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                      /* ipiLineNumber            */
                                   INPUT "ma_MsgProvider":U,                                                 /* ipcMessageGroup          */
                                   INPUT 3,   /* Only one provider may be marked as the main provider. &1 */ /* ipiMessageNumber         */
                                   INPUT "":U).                                                              /* ipcReplaceTextList       */
    END. /*IF btt_auth_provider.main_provider THEN */

    /*
      Validate if the auth group may change
    */
    IF AVAILABLE buf_auth_provider THEN
    DO:
      IF (buf_auth_provider.doc_num <> btt_auth_provider.doc_num
      OR  buf_auth_provider.pr_type <> btt_auth_provider.pr_type)
      AND CAN-FIND(FIRST bbt_auth_detail NO-LOCK
                   WHERE bbt_auth_detail.auth_obj          = btt_auth_provider.auth_obj
                     AND bbt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj)
      THEN
        ASSIGN
          cMessage = "The Provider/Discipline may not be changed while Clinical Detail records exist for [" +
                     STRING(buf_auth_provider.doc_num) + "/":U + STRING(buf_auth_provider.pr_type) + "]":U
          lSuccess = goErrorObject:addError
                                   (INPUT "hatap":U,                             /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj       */
                                    INPUT "":U,                                  /* ipcOwningEntityKey       */
                                    INPUT "doc_num":U,                           /* ipcFieldName             */
                                    INPUT btt_auth_provider.line_number,         /* ipiLineNumber            */
                                    INPUT cMessage,                              /* ipcMessageText           */
                                    INPUT "ERR":U).                              /* ipiMessageType           */

      IF buf_auth_provider.authorised_service <> btt_auth_provider.authorised_service
      AND CAN-FIND(FIRST bbt_auth_detail NO-LOCK
                   WHERE bbt_auth_detail.auth_obj          = btt_auth_provider.auth_obj
                     AND bbt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj)
      THEN
        ASSIGN
          cMessage = "The Authorised Service flag for a Provider may not be changed while Clinical Detail records exist for the Provider.":U
          lSuccess = goErrorObject:addError
                                   (INPUT "hatap":U,                             /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj       */
                                    INPUT "":U,                                  /* ipcOwningEntityKey       */
                                    INPUT "authorised_service":U,                /* ipcFieldName             */
                                    INPUT btt_auth_provider.line_number,         /* ipiLineNumber            */
                                    INPUT cMessage,                              /* ipcMessageText           */
                                    INPUT "ERR":U).                              /* ipiMessageType           */

    END. /* IF AVAILABLE buf_auth_provider THEN */

    /*
      Auth group validation
    */
    IF btt_auth_provider.auth_group_obj <> 0 THEN
    DO:
      FIND ham_auth_group NO-LOCK
        WHERE ham_auth_group.auth_group_obj = btt_auth_provider.auth_group_obj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE ham_auth_group THEN
      DO:
        goErrorObject:addError(INPUT "hatap":U,
                                 INPUT btt_auth_provider.auth_provider_obj,
                                 INPUT "":U,
                                 INPUT "auth_group_obj":U,
                                 INPUT btt_auth_provider.line_number,
                                 INPUT "MA":U,
                                 INPUT 100,  /* The &1 specified is invalid. */
                                 INPUT "Authorisation group").
      END.  /* IF NOT AVAILABLE ham_auth_group THEN */
      ELSE DO:
        IF  ham_auth_group.option_code <> 0
        AND ham_auth_group.option_code <> goAuthorisation:OptionCode THEN
        DO:
          goErrorObject:addError(INPUT "hatap":U,
                                 INPUT btt_auth_provider.auth_provider_obj,
                                 INPUT "":U,
                                 INPUT "auth_group_obj":U,
                                 INPUT btt_auth_provider.line_number,
                                 INPUT "MA":U,
                                 INPUT 112,  /* The &1 specified is invalid. &2 */
                                 INPUT "Auth group " + ham_auth_group.auth_group_code +
                                       ",Auth group is only valid for option code " + STRING(ham_auth_group.option_code,"99")).
        END.  /* IF ham_auth_group.option_code <> 0 AND... */

        IF CAN-FIND(FIRST ham_auth_group_detail
                    WHERE ham_auth_group_detail.auth_group_obj         = btt_auth_provider.auth_group_obj
                    AND   ham_auth_group_detail.owning_entity_mnemonic = 'prtype':U)
        AND NOT CAN-FIND(FIRST ham_auth_group_detail
                        WHERE ham_auth_group_detail.auth_group_obj  = btt_auth_provider.auth_group_obj
                        AND   ham_auth_group_detail.effective_date <= goAuthorisation:StartDate
                        AND  (ham_auth_group_detail.end_date        = ?                                         OR ham_auth_group_detail.end_date   >= goAuthorisation:EndDate)
                        AND   ham_auth_group_detail.owning_entity_mnemonic = 'prtype':U
                        AND  (ham_auth_group_detail.pr_type         = STRING(btt_auth_provider.pr_type,"999":U) OR ham_auth_group_detail.pr_type     = "000")
                        AND  (ham_auth_group_detail.sub_pr_type     = btt_auth_provider.sub_pr_type             OR ham_auth_group_detail.sub_pr_type = 0))
        THEN
          ASSIGN cMessage = IF AVAILABLE ham_auth_group
                            THEN SUBSTITUTE('Discipline/Sub-Discipline [&1/&2] is not valid according to the Auth Group [&3] setup.',
                                            STRING(btt_auth_provider.pr_type,"999":U),STRING(btt_auth_provider.sub_pr_type,"999":U),
                                            ham_auth_group.auth_group_code)
                            ELSE "Invalid auth group selected for provider " + STRING(btt_auth_provider.doc_num)
                 lSuccess = goErrorObject:addError
                                   (INPUT "hatap":U,                             /* ipcOwningEntityMnemonic  */
                                    INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj       */
                                    INPUT "":U,                                  /* ipcOwningEntityKey       */
                                    INPUT "auth_group_obj":U,                    /* ipcFieldName             */
                                    INPUT btt_auth_provider.line_number,         /* ipiLineNumber            */
                                    INPUT cMessage,                              /* ipcMessageText           */
                                    INPUT "ERR":U).                              /* ipiMessageType           */
      END.  /* ELSE - IF NOT AVAILABLE ham_auth_group THEN */
    END.  /* IF btt_auth_provider.auth_group_obj <> 0 THEN */

    /*
      Validate Co-payment override reason for Authorised Provider requiring co-payment
    */
    IF  btt_auth_provider.copay_provider          = YES
    AND btt_auth_provider.auth_copay_control_obj <> 0
    AND btt_auth_provider.auth_status             = 1
    AND btt_auth_provider.copay_override_note 	  = "":U
    THEN
      ASSIGN
          cValidMessage = "Co-payment override reason must be captured if the provider requires a co-payment and the provider is authorised."
          lSuccess      = goErrorObject:addError
                                      (INPUT "hatap":U,                                  /* ipcOwningEntityMnemonic  */
                                       INPUT btt_auth_provider.auth_provider_obj,        /* ipdOwningEntityObj       */
                                       INPUT "":U,                                       /* ipcOwningEntityKey       */
                                       INPUT "copay_override_note":U,                    /* ipcFieldName             */
                                       INPUT btt_auth_provider.line_number,              /* ipiLineNumber            */
                                       INPUT "MA":U,                                     /* ipcMessageGroup          */
                                       INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                       INPUT "Co-payment override reason," + cValidMessage ).
    ELSE DO:
      /* Co-payment processing */
      { ma/app/maauthbusvalprovcopay.i }
    END.  // ELSE - IF btt_auth_provider.copay_provider = YES...

    /* Co-payment Penalty processing */
    { ma/app/maauthbusvalprovpenalty.i }

    /* Activate Limit Checking for Providers */
    { ma/app/maauthbusvalprovchecklimits.i }

    /*
       Auth Provider Status validation:
         1) Ensure that only a valid auth provider status is used according to system and rule setups
    */
    ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  btt_auth_provider.auth_status,
                                                                       INPUT  "System":U).
    IF NOT lValidStatus
    THEN
      ASSIGN cErrorMessage = "Status Code: " + STRING(btt_auth_provider.auth_status) +
                             ",[HELP=Auth Rule Code: ValidStatuses]"
             lSuccess = goErrorObject:addError
                              (INPUT "hatap":U,                                    /* ipcOwningEntityMnemonic */
                               INPUT btt_auth_provider.auth_provider_obj,          /* ipdOwningEntityObj      */
                               INPUT "":U,                                         /* ipcOwningEntityKey      */
                               INPUT "auth_status":U,                              /* ipcFieldName            */
                               INPUT btt_auth_provider.line_number,                /* ipiLineNumber           */
                               INPUT "MA":U,                                       /* ipcMessageGroup         */
                               INPUT 112,  /* The &1 specified is invalid. &2 */   /* ipiMessageNumber        */
                               INPUT cErrorMessage).                               /* ipcReplaceTextList      */

    IF lValidStatus THEN
    DO:
      /*
        Auth Provider Status Validation:
          2) Ensure that the auth status is changed to a valid status according to rules set up
      */
      IF AVAILABLE buf_auth_provider
      AND buf_auth_provider.auth_status <> btt_auth_provider.auth_status THEN
      DO:
        ASSIGN cErrorMessage = mipEnv:Health:AuthService:validateAuthStatusUpdate(INPUT  goAuthorisation:InsurerObj,
                                                                                  INPUT  goAuthorisation:OptionCode,
                                                                                  INPUT  buf_auth_provider.auth_status,
                                                                                  INPUT  buf_auth_provider.auth_status_note,
                                                                                  INPUT  btt_auth_provider.auth_status,
                                                                                  INPUT  btt_auth_provider.amount_paid,
                                                                                  INPUT  btt_auth_provider.quantity_paid,
                                                                                  INPUT  btt_auth_provider.start_date,
                                                                                  INPUT goAuthorisation:AuthStatus,
                                                                                  INPUT goAuthorisation:AuthStatusNote).
        IF cErrorMessage <> "":U
        THEN
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT "hatap":U,                            /* ipcOwningEntityMnemonic  */
                                          INPUT btt_auth_provider.auth_provider_obj,  /* ipdOwningEntityObj       */
                                          INPUT "":U,                                 /* ipcOwningEntityKey       */
                                          INPUT "auth_status":U,                      /* ipcFieldName             */
                                          INPUT btt_auth_provider.line_number,        /* ipiLineNumber            */
                                          INPUT cErrorMessage,                        /* ipcMessageText           */
                                          INPUT "ERR":U)                              /* ipcMessageType           */
                 lValidStatus = FALSE.
        ELSE
          ASSIGN iOldStatus     = buf_auth_provider.auth_status
                 cOldStatusNote = buf_auth_provider.auth_status_note.
      END.  /* IF available buf_auth_providerAND... */
      ELSE
        ASSIGN iOldStatus     = 0
               cOldStatusNote = "".

      IF lValidStatus
      AND AVAILABLE buf_auth_provider
      AND buf_auth_provider.auth_status <> btt_auth_provider.auth_status THEN
      DO:
        /*
          Provider Status can't be updated if any claims are paid for the provider
        */
        IF btt_auth_provider.quantity_paid <> 0
        OR btt_auth_provider.amount_paid   <> 0
        THEN
          ASSIGN cMessage     = "The Provider Status cannot be changed, as claims have already been paid against this Provider."
                 lValidStatus = FALSE
                 lSuccess     = goErrorObject:addError
                                  (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic */
                                   INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj      */
                                   INPUT "":U,                                /* ipcOwningEntityKey      */
                                   INPUT "auth_status":U,                     /* ipcFieldName            */
                                   INPUT btt_auth_provider.line_number,       /* ipiLineNumber           */
                                   INPUT cMessage,                            /* ipcMessageText          */
                                   INPUT "ERR":U).                            /* ipiMessageType          */
        ELSE DO:
          ASSIGN cNewStatusDescr = mipEnv:Health:AuthService:getStatusDescription(INPUT "System":U,
                                                                                  INPUT btt_auth_provider.auth_status)
                 cNewStatus      = STRING(btt_auth_provider.auth_status) + "-":U + cNewStatusDescr.

          /*
            If the auth provider is declined, make sure the claim code is also assigned to 99.
          */
          IF btt_auth_provider.auth_status = 6  /* Declined */ THEN
          DO:
            IF AVAILABLE buf_auth_schext
            AND btt_auth_provider.claim_code <> buf_auth_schext.claim-code[1]    /* Claim code = 99. */
            THEN
              ASSIGN btt_auth_provider.claim_code = buf_auth_schext.claim-code[1].
            ELSE IF NOT AVAILABLE buf_auth_schext THEN
            DO:
              ASSIGN cErrorMessage = SUBSTITUTE("Scheme Information was not found for option &1. Please contact your CSM.",
                                                STRING(goAuthorisation:MemberOptionCode,{ma/cus/maschopt9.i})).
              goErrorObject:addError(INPUT "hatap":U,
                                     INPUT btt_auth_provider.auth_provider_obj,
                                     INPUT "":U,
                                     INPUT "":U,
                                     INPUT btt_auth_provider.line_number,
                                     INPUT cErrorMessage,
                                     INPUT "ERR":U).
            END.  /* ELSE IF NOT AVAILABLE buf_auth_schext THEN */
          END.  /* IF btt_auth_provider.auth_status = 6  /* Declined */ */

          /*
            If the provider was declined, but is now authorised, we need to reassign
            the claim code and claim type as well.
          */
          ELSE IF  AVAILABLE buf_auth_provider
               AND buf_auth_provider.auth_status = 6 /* Declined */ THEN
          DO:
            ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                    (INPUT  goAuthorisation:InsurerObj,
                                     INPUT  goAuthorisation:OptionCode,
                                     INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                     INPUT  "Update6Declined":U,
                                     INPUT  btt_auth_provider.start_date,
                                     OUTPUT lValidRule,
                                     OUTPUT cRuleValue).

            IF LOOKUP(cNewStatus,cRuleValue) > 0
            THEN
              ASSIGN btt_auth_provider.claim_code = btt_auth_provider.default_claim_code
                     btt_auth_provider.claim_type = btt_auth_provider.default_claim_type.
          END.  /* ELSE IF buf_auth_provider.auth_status = 6 /* Declined */ THEN */

          /*
             At this point we're not sure if the temp-table lines are populated. And because the auth
             provider may not be authorised if there are pending lines, we need to make sure the
             temp-table lines are populated with the correct data. Also, if the 'DefaultProvider'-rule
             applies, we might want to change the lines' statuses as well - we'll do this by changing
             the status of the line's temp-table record and changing the lines's record action to 'MODIFY'.
          */
          FOR EACH hat_auth_detail NO-LOCK
              WHERE hat_auth_detail.auth_obj          = btt_auth_provider.auth_obj
                AND hat_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj:
              IF NOT CAN-FIND(FIRST bbt_auth_detail NO-LOCK
                 WHERE bbt_auth_detail.auth_detail_obj = hat_auth_detail.auth_detail_obj) THEN
              DO:
                CREATE bbt_auth_detail.
                BUFFER-COPY hat_auth_detail TO bbt_auth_detail.
              END.  /* IF NOT CAN-FIND(FIRST bbt_auth_detail NO-LOCK */
          END.  /* FOR EACH hat_auth_detail NO-LOCK */

          ASSIGN lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                (INPUT  goAuthorisation:InsurerObj,
                                 INPUT  goAuthorisation:OptionCode,
                                 INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                 INPUT  "DefaultProvider":U,
                                 INPUT  btt_auth_provider.start_date,
                                 OUTPUT lValidRule,
                                 OUTPUT cRuleValue).
          IF lValidRule AND LOOKUP(cNewStatus,cRuleValue) > 0
          THEN
            ASSIGN lDefaultProvider = TRUE.
          ELSE
            ASSIGN lDefaultProvider = FALSE.

          /*
             If the provider's status is changed to 1-Authorised, more validation
             need to be done. So if the provider's status is not changed to 1-Authorised,
             we can run the _updateDetailStatus procedure (with some validation
             done in the procedure) to update the detail lines' statuses.
             But before we start changing all the line statuses, we need to make sure
             that there are no pending lines whose statuses won't be changed.
             (If the status reason on the pending line differs from the status reason
             on the header, then the line's status won't be changed.)
          */
          IF btt_auth_provider.auth_status = 1 THEN
          DO:
            IF CAN-FIND(FIRST bbt_auth_detail NO-LOCK
                     WHERE bbt_auth_detail.auth_obj          = btt_auth_provider.auth_obj
                     AND   bbt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj
                     AND   bbt_auth_detail.auth_status       = 0 /* Pending */
                     AND  (lDefaultProvider                  = FALSE
                     OR    bbt_auth_detail.auth_status_note <> cOldStatusNote))
            THEN
              ASSIGN cErrorMessage = "The auth provider can't be authorised if there are 'pending' detail lines. Attend to the 'pending' lines first and then try again."
              			 + "[HELP=Auth Rule Code: DefaultProvider]"
              	     lSuccess      = goErrorObject:addError
                                    	(INPUT "hatap":U,                                      /* ipcOwningEntityMnemonic  */
                                    	 INPUT btt_auth_provider.auth_provider_obj,            /* ipdOwningEntityObj       */
                                    	 INPUT "":U,                                           /* ipcOwningEntityKey       */
                                    	 INPUT "auth_status":U,                                /* ipcFieldName             */
                                    	 INPUT btt_auth_provider.line_number,                  /* ipiLineNumber            */
                                    	 INPUT cErrorMessage,				                           /* ipcMessage               */
                                    	 INPUT "ERR":U)                                        /* ipcReplaceTextList       */
                     lValidStatus  = FALSE.

            IF CAN-FIND(FIRST bbt_auth_detail NO-LOCK
                     WHERE bbt_auth_detail.auth_obj          = btt_auth_provider.auth_obj
                     AND   bbt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj
                     AND   bbt_auth_detail.auth_status       = 7 /* Requested */
                     AND   bbt_auth_detail.auth_status_note <> cOldStatusNote)
            THEN
              ASSIGN lSuccess = goErrorObject:addError
                                    (INPUT "hatap":U,                                                                                                       /* ipcOwningEntityMnemonic  */
                                     INPUT btt_auth_provider.auth_provider_obj,                                                                             /* ipdOwningEntityObj       */
                                     INPUT "":U,                                                                                                            /* ipcOwningEntityKey       */
                                     INPUT "auth_status":U,                                                                                                 /* ipcFieldName             */
                                     INPUT btt_auth_provider.line_number,                                                                                   /* ipiLineNumber            */
                                     INPUT "ma_MsgAuth":U,                                                                                                  /* ipcMessageGroup          */
                                     INPUT 16,  /* The &1 can't be authorised if there are &2 &3 lines. Attend to the &2 lines first and then try again. */ /* ipiMessageNumber         */
                                     INPUT "auth provider,'requested',detail")                                                                              /* ipcReplaceTextList       */
                     lValidStatus = FALSE.

          END. /* IF btt_auth_provider.auth_status = 1 THEN */

          /*
             If the auth status on the provider is changed, then check rule to see if the
             status on the provider detail lines must also be changed.
          */
          IF lDefaultProvider AND lValidStatus THEN
          DO:
            /*
               Change the status on the provider detail records where applicable
            */
            FOR EACH bbt_auth_detail EXCLUSIVE-LOCK
                WHERE bbt_auth_detail.auth_obj          = btt_auth_provider.auth_obj
                AND   bbt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj:
              IF fnAllowLineStatusChange (INPUT iOldStatus,
                                          INPUT cOldStatusNote,
                                          INPUT btt_auth_provider.auth_status,
                                          INPUT btt_auth_provider.auth_status_note,
                                          INPUT bbt_auth_detail.auth_status,
                                          INPUT bbt_auth_detail.auth_status_note)
              THEN
                ASSIGN bbt_auth_detail.auth_status      = btt_auth_provider.auth_status
                       bbt_auth_detail.auth_status_note = btt_auth_provider.auth_status_note
                       bbt_auth_detail.record_action    = "MODIFY":U WHEN bbt_auth_detail.record_action <> "DELETE":U.
            END. /* FOR EACH bbt_auth_detail EXCLUSIVE-LOCK */
          END. /* IF lDefaultProvider AND lValidStatus THEN */

          /*
             Check if the status reason/note on the auth provider is mandatory
          */
          IF btt_auth_provider.auth_status_note = "":U THEN
          DO:
            ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  btt_auth_provider.auth_status,
                                                                                INPUT  goAuthorisation:InsurerObj,
                                                                                INPUT  goAuthorisation:OptionCode,
                                                                                INPUT  btt_auth_provider.start_date).
            IF lMandatory
            THEN
              ASSIGN cErrorMessage = "Status Reason,The status reason is mandatory for auth status " + cNewStatus
                                   + ".[HELP=Auth Rule Code: EnforceStatusNote]":U
                     lSuccess      = goErrorObject:addError
                                             (INPUT "hatap":U,                                /* ipcOwningEntityMnemonic  */
                                              INPUT btt_auth_provider.auth_provider_obj,      /* ipdOwningEntityObj       */
                                              INPUT "":U,                                     /* ipcOwningEntityKey       */
                                              INPUT "auth_status_note":U,                     /* ipcFieldName             */
                                              INPUT btt_auth_provider.line_number,            /* ipiLineNumber            */
                                              INPUT "MA":U,                                   /* ipcMessageGroup          */
                                              INPUT 111, /* The &1 must be specified. &2 */   /* ipiMessageNumber         */
                                              INPUT cErrorMessage).                           /* ipcReplaceTextList       */

          END.  /* IF btt_auth_provider.auth_status_note = "" THEN */
          ELSE DO:
            mipEnv:Health:AuthService:ValidateStatusReason(INPUT  goAuthorisation:InsurerObj,
                                                           INPUT  00,
                                                           INPUT  btt_auth_provider.auth_status_note,
                                                           INPUT  btt_auth_provider.auth_status,
                                                           INPUT  btt_auth_provider.start_date,
                                                           OUTPUT cValidMessage).

            IF cValidMessage <> "":U
            THEN
              ASSIGN lSuccess  = goErrorObject:addError
                                      (INPUT "hatap":U,                                    /* ipcOwningEntityMnemonic  */
                                       INPUT btt_auth_provider.auth_provider_obj,          /* ipdOwningEntityObj       */
                                       INPUT "":U,                                         /* ipcOwningEntityKey       */
                                       INPUT "auth_status_note":U,                         /* ipcFieldName             */
                                       INPUT btt_auth_provider.line_number,                /* ipiLineNumber            */
                                       INPUT "MA":U,                                       /* ipcMessageGroup          */
                                       INPUT 112,  /* The &1 specified is invalid. &2 */   /* ipiMessageNumber         */
                                       INPUT "Status Reason," + cValidMessage ).           /* ipcReplaceTextList       */
          END. /* ELSE DO: IF btt_auth_provider.auth_status_note = "" */
        END. /* ELSE DO: IF btt_auth_provider.amount_paid <> 0... */
      END.  /* IF lValidStatus THEN */
    END. /* IF lValidStatus THEN */

    IF btt_auth_provider.discount_auth <> 0 THEN
    DO:
      mipEnv:Health:AuthService:checkForDiscounts(INPUT  0,
                                                  INPUT  btt_auth_provider.auth_provider_obj,
                                                  INPUT  0,
                                                  OUTPUT cErrorMessage).

      IF cErrorMessage <> "" THEN
      DO:
        IF AVAILABLE buf_auth_provider AND buf_auth_provider.auth_status <> btt_auth_provider.auth_status
        THEN
          ASSIGN cErrorField = "auth_status":U.
        ELSE
          ASSIGN cErrorField = "discount_auth":U.

        goErrorObject:addError(INPUT "hatap":U,
                               INPUT btt_auth_provider.auth_provider_obj,
                               INPUT "":U,
                               INPUT cErrorField,
                               INPUT btt_auth_provider.line_number,
                               INPUT cErrorMessage,
                               INPUT "ERR":U).

      END. //IF cErrorMessage <> "" THEN
    END. //IF bbt_auth_provider.discount_auth <> 0 THEN

    IF btt_auth_provider.pmb_indicator <> YES THEN
    DO:
      IF btt_auth_provider.pmb_benefit_% <> 0.00
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                            (INPUT "hatap":U,                                                      /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth_provider.auth_provider_obj,                            /* ipdOwningEntityObj       */
                             INPUT "":U,                                                           /* ipcOwningEntityKey       */
                             INPUT "pmb_benefit_%":U,                                              /* ipcFieldName             */
                             INPUT btt_auth_provider.line_number,                                  /* ipiLineNumber            */
                             INPUT "MA":U,                                                         /* ipcMessageGroup          */
                             INPUT 112,  /* The &1 specified is invalid. &2 */                     /* ipiMessageNumber         */
                             INPUT "PMB Benefit Percentage,Update on the field is not allowed":U). /* ipcReplaceTextList       */

      IF btt_auth_provider.pmb_pay_cost = YES
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                            (INPUT "hatap":U,                                               /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth_provider.auth_provider_obj,                     /* ipdOwningEntityObj       */
                             INPUT "":U,                                                    /* ipcOwningEntityKey       */
                             INPUT "pmb_pay_cost":U,                                        /* ipcFieldName             */
                             INPUT btt_auth_provider.line_number,                           /* ipiLineNumber            */
                             INPUT "MA":U,                                                  /* ipcMessageGroup          */
                             INPUT 112,  /* The &1 specified is invalid. &2 */              /* ipiMessageNumber         */
                             INPUT "PMB Pay Cost,Update on the field is not allowed":U).    /* ipcReplaceTextList       */

    END.  // IF btt_auth_provider.pmb_indicator <> YES THEN
    ELSE DO: /* IF pmb_indicator = YES */
      IF btt_auth_provider.pmb_benefit_% < 0.00
      THEN
        ASSIGN
          cValidMessage = "Please enter PMB Benefit Percentage value greater than ~"0.00%~"."
          lSuccess      = goErrorObject:addError
                                      (INPUT "hatap":U,                                  /* ipcOwningEntityMnemonic  */
                                       INPUT btt_auth_provider.auth_provider_obj,        /* ipdOwningEntityObj       */
                                       INPUT "":U,                                       /* ipcOwningEntityKey       */
                                       INPUT "pmb_benefit_%":U,                          /* ipcFieldName             */
                                       INPUT btt_auth_provider.line_number,              /* ipiLineNumber            */
                                       INPUT "MA":U,                                     /* ipcMessageGroup          */
                                       INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                       INPUT "PMB Benefit Percentage or PMB pay Cost ," + cValidMessage ).

      IF AVAILABLE buf_auth_provider THEN
      DO:
        IF btt_auth_provider.amount_paid   > 0.0
        OR btt_auth_provider.quantity_paid > 0    THEN
        DO:
          IF buf_auth_provider.pmb_benefit_% <> btt_auth_provider.pmb_benefit_%
          THEN
            ASSIGN
              cValidMessage = "PMB Benefit Percentage can not be updated for a paid provider."
              lSuccess      = goErrorObject:addError
                                          (INPUT "hatap":U,                                  /* ipcOwningEntityMnemonic  */
                                           INPUT btt_auth_provider.auth_provider_obj,        /* ipdOwningEntityObj       */
                                           INPUT "":U,                                       /* ipcOwningEntityKey       */
                                           INPUT "pmb_benefit_%":U,                          /* ipcFieldName             */
                                           INPUT btt_auth_provider.line_number,              /* ipiLineNumber            */
                                           INPUT "MA":U,                                     /* ipcMessageGroup          */
                                           INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                           INPUT "PMB Benefit Percentage," + cValidMessage ).

          IF buf_auth_provider.pmb_pay_cost <> btt_auth_provider.pmb_pay_cost
          THEN
            ASSIGN
              cValidMessage = "PMB Pay Cost can not be updated for a paid provider."
              lSuccess      = goErrorObject:addError
                                          (INPUT "hatap":U,                                  /* ipcOwningEntityMnemonic  */
                                           INPUT btt_auth_provider.auth_provider_obj,        /* ipdOwningEntityObj       */
                                           INPUT "":U,                                       /* ipcOwningEntityKey       */
                                           INPUT "pmb_pay_cost":U,                           /* ipcFieldName             */
                                           INPUT btt_auth_provider.line_number,              /* ipiLineNumber            */
                                           INPUT "MA":U,                                     /* ipcMessageGroup          */
                                           INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                           INPUT "PMB Pay Cost," + cValidMessage ).

        END. /* IF btt_auth_provider.amount_paid   > 0.0 OR  btt_auth_provider.quantity_paid > 0 THEN*/
      END.  /* IF AVAILABLE buf_auth_provider THEN */

      IF  btt_auth_provider.pmb_benefit_% > 0.00
      AND btt_auth_provider.pmb_pay_cost  = YES
      THEN
        ASSIGN
          cValidMessage = "Either enter PMB Benefit Percentage value greater than ~"0.00%~" or check the ~"PMB Pay Cost~" box."
          lSuccess      = goErrorObject:addError
                                      (INPUT "hatap":U,                                  /* ipcOwningEntityMnemonic  */
                                       INPUT btt_auth_provider.auth_provider_obj,        /* ipdOwningEntityObj       */
                                       INPUT "":U,                                       /* ipcOwningEntityKey       */
                                       INPUT IF AVAILABLE buf_auth_provider
                                             AND buf_auth_provider.pmb_pay_cost <> btt_auth_provider.pmb_pay_cost
                                             THEN "pmb_pay_cost":U
                                             ELSE IF AVAILABLE buf_auth_provider
                                               THEN "pmb_benefit_%":U
                                               ELSE "":U,                                /* ipcFieldName             */
                                       INPUT btt_auth_provider.line_number,              /* ipiLineNumber            */
                                       INPUT "MA":U,                                     /* ipcMessageGroup          */
                                       INPUT 112,  /* The &1 specified is invalid. &2 */ /* ipiMessageNumber         */
                                       INPUT "PMB Benefit Percentage and PMB pay Cost," + cValidMessage ).
    END. /*if pmb_indicator = yes*/

    /*
      Now that all claim codes and types have been set , ensure that there are no issues
    */
    RUN _validateAuthClaimCodesAndTypes IN TARGET-PROCEDURE (INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                             INPUT btt_auth_provider.auth_obj ) .
    /*
      Get all the display flags for the provider and create warnings for each of them
    */
    RUN _DisplayFlags (INPUT goAuthorisation:InsurerObj,
                       INPUT goAuthorisation:MemberOptionCode,
                       INPUT "D":U,
                       INPUT btt_auth_provider.doc_num,
                       INPUT btt_auth_provider.start_date,
                       INPUT "hatap":U,
                       INPUT btt_auth_provider.auth_provider_obj,
                       INPUT btt_auth_provider.line_number).

    /*
      If the provider belongs to a group, we need to display the flags for all.
      Get all the display flags for the provider and create warnings for each of them
    */
    IF btt_auth_provider.group_doc_num <> 0
    THEN
      RUN _DisplayFlags (INPUT goAuthorisation:InsurerObj,
                         INPUT goAuthorisation:MemberOptionCode,
                         INPUT "D":U,
                         INPUT btt_auth_provider.group_doc_num,
                         INPUT btt_auth_provider.start_date,
                         INPUT "hatap":U,
                         INPUT btt_auth_provider.auth_provider_obj,
                         INPUT btt_auth_provider.line_number).

    /*
      Auth is Completed
    */
    IF NOT btt_auth.auth_incomplete THEN
    DO:
      /*
        Provider update
        Ensure that if a claim has been paid against this line, that the claim code/ claim type is not updated.
      */
       /********************* POTENTIALLY DOING YET ANOTHER FETCH CALL TO AUTHTYPE */
      /*
        Amount/Quantity paid - ensure claim code/claim type has not been changed
      */
      IF AVAILABLE buf_auth_provider THEN
      DO:
        IF buf_auth_provider.amount_paid   <> 0.00
        OR buf_auth_provider.quantity_paid <> 0 THEN
        DO:
          /*
            Claim type changed
          */
          IF buf_auth_provider.claim_type <> btt_auth_provider.claim_type
          THEN
            ASSIGN lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,
                                 INPUT btt_auth_provider.auth_provider_obj,
                                 INPUT "":U,
                                 INPUT "claim_type":U,
                                 INPUT btt_auth_provider.line_number,
                                 INPUT "The claim type may not be changed once a claim has been paid.":U,
                                 INPUT "ERR":U).

          /*
            Claim code changed
          */
          IF buf_auth_provider.claim_code <> btt_auth_provider.claim_code
          THEN
            ASSIGN lSuccess = goErrorObject:addError
                                (INPUT "hatap":U,
                                 INPUT btt_auth_provider.auth_provider_obj,
                                 INPUT "":U,
                                 INPUT "claim_code":U,
                                 INPUT btt_auth_provider.line_number,
                                 INPUT "The claim code may not be changed once a claim has been paid.":U,
                                 INPUT "ERR":U).

          /*
            Amount auth/paid validation
          */
          IF  btt_auth_provider.amount_paid <> 0.00
          AND btt_auth_provider.amount_auth <> 0.00
          AND btt_auth_provider.amount_auth < btt_auth_provider.amount_paid THEN
          DO:
            goErrorObject:addError(INPUT "hatap":U,                                                        /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                              /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                             /* ipcOwningEntityKey       */
                                   INPUT "amount_auth":U,                                                  /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                    /* ipiLineNumber            */
                                   INPUT "ma_MsgAuth":U,                                                   /* ipcMessageGroup          */
                                   INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */       /* ipiMessageNumber         */
                                   INPUT "Amount Paid," + STRING(btt_auth_provider.amount_paid) +
                                         ",Amount Authorised,":U + STRING(btt_auth_provider.amount_auth)). /* ipcReplaceTextList       */
          END. /*IF btt_auth_provider.amount_paid <> 0.00 AND btt_auth_provider.amount_auth < btt_auth_provider.amount_paid THEN */

          /*
            Quantity auth/paid validation
          */
          IF  btt_auth_provider.quantity_paid <> 0.00
          AND btt_auth_provider.quantity_auth <> 0.00
          AND btt_auth_provider.quantity_auth < btt_auth_provider.quantity_paid THEN
          DO:
            goErrorObject:addError(INPUT "hatap":U,                                                            /* ipcOwningEntityMnemonic  */
                                   INPUT btt_auth_provider.auth_provider_obj,                                  /* ipdOwningEntityObj       */
                                   INPUT "":U,                                                                 /* ipcOwningEntityKey       */
                                   INPUT "quantity_auth":U,                                                    /* ipcFieldName             */
                                   INPUT btt_auth_provider.line_number,                                        /* ipiLineNumber            */
                                   INPUT "ma_MsgAuth":U,                                                       /* ipcMessageGroup          */
                                   INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */           /* ipiMessageNumber         */
                                   INPUT "Quantity Paid," + STRING(btt_auth_provider.quantity_paid) +
                                         ",Quantity Authorised,":U + STRING(btt_auth_provider.quantity_auth)). /* ipcReplaceTextList       */
          END. /* IF btt_auth_provider.quantity_paid <> 0.00 AND btt_auth_provider.quantity_auth < btt_auth_provider.quantity_paid THEN */

          /*
            PMB Validations - PMB Indicator can't be updated if any claims are paid for the provider
          */
          IF  btt_auth_provider.pmb_indicator <> buf_auth_provider.pmb_indicator
          AND btt_auth_provider.authorised_service THEN
          DO:
            IF LOOKUP("ma_member_auth_pmbind_override",gscUserRole) > 0 THEN
            DO:
              ASSIGN
                cMessage = "Please note that you are overriding the PMB Indicator validation for this provider."
                lSuccess = goErrorObject:addError(INPUT "hatap":U,
                                                  INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj */
                                                  INPUT "":U,                                /* ipcOwningEntityKey */
                                                  INPUT "pmb_indicator":U,                   /* ipcFieldName       */
                                                  INPUT btt_auth_provider.line_number,       /* ipiLineNumber      */
                                                  INPUT cMessage,                            /* ipcMessageText     */
                                                  INPUT "WAR":U,                             /* ipcErrType         */
                                                  INPUT TRUE).                               /* iplAcknowledge     */
            END. /* IF LOOKUP(ma_member_auth_pmbind_override,gscUserRole) > 0 THEN */
            ELSE

              ASSIGN
                cMessage = "PMB Indicator can't be updated, claims must be reversed before PMB Indicator can be updated on the Provider."
                lSuccess = goErrorObject:addError
                                  (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic */
                                   INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj      */
                                   INPUT "":U,                                /* ipcOwningEntityKey      */
                                   INPUT "pmb_indicator":U,                   /* ipcFieldName            */
                                   INPUT btt_auth_provider.line_number,       /* ipiLineNumber           */
                                   INPUT cMessage,                            /* ipcMessageText          */
                                   INPUT "ERR":U).                            /* ipiMessageType          */
          END. /* IF btt_auth_provider.pmb_indicator <> buf_auth_provider.pmb_indicator... THEN */
        END. /* IF buf_auth_provider.amount_paid <> 0.00 THEN */

        /*
          Discount-Type and Discount-Auth
        */
        IF btt_auth_provider.amount_paid   <> 0
        OR btt_auth_provider.quantity_paid <> 0 THEN
        DO:
          ASSIGN cErrorMessage = "".
          IF btt_auth_provider.discount_auth <> buf_auth_provider.discount_auth
          OR btt_auth_provider.discount_type <> buf_auth_provider.discount_type THEN
          DO:
            ASSIGN
              cErrorMessage = "The Discount Type/Amount can not be updated as Claims have already been paid against this Provider"
              lSuccess      =  goErrorObject:addError(INPUT "hatap":U,
                                                      INPUT btt_auth_provider.auth_provider_obj,
                                                      INPUT "":U,
                                                      INPUT IF btt_auth_provider.discount_auth <> buf_auth_provider.discount_auth
                                                            THEN "discount_auth":U
                                                            ELSE "discount_type":U,
                                                      INPUT btt_auth_provider.line_number,
                                                      INPUT cErrorMessage,
                                                      INPUT "ERR":U).
          END. /* IF btt_auth_provider.discount_auth <> buf_auth_provider.discount_auth...  */
        END. /* IF btt_auth_provider.amount_paid <> 0... THEN */

        IF buf_auth_provider.auth_group_obj <> btt_auth_provider.auth_group_obj THEN
        DO:
          IF buf_auth_provider.amount_paid <> 0 OR buf_auth_provider.quantity_paid <> 0
          THEN
            ASSIGN
              cMessage = "A claim has already been paid for this provider, auth group may not be changed":U
              lSuccess = goErrorObject:addError
                                    (INPUT "hatap":U,                             /* ipcOwningEntityMnemonic  */
                                     INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj       */
                                     INPUT "":U,                                  /* ipcOwningEntityKey       */
                                     INPUT "auth_group_obj":U,                    /* ipcFieldName             */
                                     INPUT btt_auth_provider.line_number,         /* ipiLineNumber            */
                                     INPUT cMessage,                              /* ipcMessageText           */
                                     INPUT "ERR":U).                              /* ipiMessageType           */
          ELSE DO:
            FOR EACH bhat_auth_provider NO-LOCK
                WHERE bhat_auth_provider.auth_obj        = btt_auth_provider.auth_obj
                  AND bhat_auth_provider.auth_group_obj <> 0.00
                  AND bhat_auth_provider.auth_group_obj  = btt_auth_provider.auth_group_obj:

              IF bhat_auth_provider.amount_paid <> 0 OR bhat_auth_provider.quantity_paid <> 0
              THEN
                ASSIGN
                  cMessage = "A claim has already been paid for a provider on this auth within a specific workgroup, auth group may not be changed":U
                  lSuccess = goErrorObject:addError
                                    (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic */
                                     INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj      */
                                     INPUT "":U,                                /* ipcOwningEntityKey      */
                                     INPUT "auth_group_obj":U,                  /* ipcFieldName            */
                                     INPUT btt_auth_provider.line_number,       /* ipiLineNumber           */
                                     INPUT cMessage,                            /* ipcMessageText          */
                                     INPUT "ERR":U).                            /* ipiMessageType          */
            END. /* FOR EACH bhat_auth_provider NO-LOCK */
          END. /* ELSE DO: IF buf_auth_provider.amount_paid <> 0 OR buf_auth_provider.quantity_paid <> 0 */
        END. /* IF buf_auth_provider.auth_group_obj <> btt_auth_provider.auth_group_obj THEN */

        IF buf_auth_provider.main_provider <> btt_auth_provider.main_provider THEN
        DO:
          FIND FIRST bhat_auth NO-LOCK
               WHERE bhat_auth.auth_obj       = btt_auth_provider.auth_obj
                 AND bhat_auth.amount_paid   <> 0
                 AND bhat_auth.quantity_paid <> 0
            NO-ERROR.
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE bhat_auth
          THEN
            ASSIGN
              cMessage = "The Main Provider may not be changed as claims have been paid against this Authorisation'.":U
              lSuccess = goErrorObject:addError
                                     (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic */
                                      INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj      */
                                      INPUT "":U,                                /* ipcOwningEntityKey      */
                                      INPUT "main_provider":U,                   /* ipcFieldName            */
                                      INPUT btt_auth_provider.line_number,       /* ipiLineNumber           */
                                      INPUT cMessage,                            /* ipcMessageText          */
                                      INPUT "ERR":U).                            /* ipiMessageType          */
        END. /* IF buf_auth_provider.main_provider <> btt_auth_provider.main_provider THEN */
      END. /* IF AVAILABLE buf_auth_provider THEN */

      IF btt_auth_provider.quantity_paid <> 0
      OR btt_auth_provider.amount_paid   <> 0 THEN
      DO:
        IF btt_auth_provider.main_provider = TRUE THEN
        DO:
          IF AVAILABLE ttAuthTypeConfig THEN
          DO:
            IF ttAuthTypeConfig.ActivateCopayment = yes
            OR ttAuthTypeConfig.ActivatePenalty   = yes
            THEN DO:
              IF btt_auth_provider._emergency_flag_updated  = YES
              THEN
                ASSIGN
                  cMessage = 'Emergency flag can not be updated as claims have already been paid for the provider':U
                  lSuccess = goErrorObject:addError
                                           (INPUT "hatap":U,                           /* ipcOwningEntityMnemonic  */
                                            INPUT btt_auth_provider.auth_provider_obj, /* ipdOwningEntityObj       */
                                            INPUT "":U,                                /* ipcOwningEntityKey       */
                                            INPUT "_emergency_flag":U,                                /* ipcFieldName             */
                                            INPUT btt_auth_provider.line_number,       /* ipiLineNumber            */
                                            INPUT cMessage,                            /* ipcMessageText           */
                                            INPUT "ERR":U).                            /* ipiMessageType           */
            END. /* IF ttAuthTypeConfig.ActivateCopayment = yes OR ttAuthTypeConfig.ActivatePenalty = yes THEN */
          END. /* IF AVAILABLE ttAuthTypeConfig THEN */

          /*
            Validations not to allow the update of the PENALTY flag when claims are paid for the main provider.
          */
          IF btt_auth_provider._penalty_flag_updated = yes
          THEN DO:

            ASSIGN
              cMessage = 'Penalty flag can not be updated as claims have already been paid for the provider':U
              lSuccess = goErrorObject:addError
                                       (INPUT "hatap":U,                             /* ipcOwningEntityMnemonic */
                                        INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj      */
                                        INPUT "":U,                                  /* ipcOwningEntityKey      */
                                        INPUT "_penalty_flag":U,                                  /* ipcFieldName            */
                                        INPUT btt_auth_provider.line_number,         /* ipiLineNumber           */
                                        INPUT cMessage,                              /* ipcMessageText          */
                                        INPUT "ERR":U).                              /* ipiMessageType          */

              IF cMessage <> "":U AND btt_auth_provider._penalty_flag = "NO" THEN
                lSuccess = goErrorObject:addError
                                       (INPUT "hatap":U,                             /* ipcOwningEntityMnemonic */
                                        INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj      */
                                        INPUT "":U,                                  /* ipcOwningEntityKey      */
                                        INPUT "_penalty_override_note":U,                                  /* ipcFieldName            */
                                        INPUT btt_auth_provider.line_number,         /* ipiLineNumber           */
                                        INPUT "Penalty note reverted",                              /* ipcMessageText          */
                                        INPUT "ERR":U).                              /* ipiMessageType          */
          END.
        END. /* IF btt_auth_provider.main_provider = TRUE THEN */
      END. /* IF btt_auth_provider.quantity_paid <> 0... THEN */
    END. /* IF NOT btt_auth_provider.auth_incomplete */

    /*
      Validate global auth limits
        -amount
        -quantity
    */
    {ma/app/maauthbusvalprovlimcntrl.i}
  END. /*IF btt_auth_provider.record_action = "MODIFY":U THEN*/

  IF btt_auth_provider.record_action = 'DELETE':U THEN
  DO:
    IF  btt_auth_provider.rate_change_type <> "":U
    AND btt_auth_provider.rate_change_type <> "ma_acAuthRateChangeTypeNone":U THEN
    DO:
      ASSIGN cErrorMessage = SUBSTITUTE("This Provider/Discipline [&1/&2] was created automatically for an Auth Rate Change - " +
                                        "it may not be deleted. Please use the Auth Reguide function to make changes.",
                                        btt_auth_provider.doc_num, btt_auth_provider.pr_type).
      goErrorObject:addError(INPUT "hatap":U,
                             INPUT btt_auth_provider.auth_provider_obj,
                             INPUT "":U,
                             INPUT "":U,
                             INPUT btt_auth_provider.line_number,
                             INPUT cErrorMessage,
                             INPUT "ERR":U).
    END.  /* IF btt_auth_provider.rate_change_type <> "" THEN */
    ELSE DO:
      ASSIGN lSuccess = oAcronymHelper:focusAcronym
                           (INPUT "KEY",
                            INPUT btt_auth_provider.provider_type).

      IF oAcronymHelper:AcronymInfocus
      THEN
        ASSIGN cProviderTypeLabel = oAcronymHelper:AcronymLabel
               lSuccess           = oAcronymHelper:unfocusAcronym() NO-ERROR.
      ELSE
        ASSIGN cProviderTypeLabel = "".

      IF CAN-FIND(FIRST hat_auth_detail NO-LOCK
                  WHERE hat_auth_detail.auth_obj = btt_auth_provider.auth_obj
                    AND hat_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj)
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                        (INPUT "hatap":U,                                                                       /* ipcOwningEntityMnemonic  */
                                         INPUT btt_auth_provider.auth_provider_obj,                                             /* ipdOwningEntityObj       */
                                         INPUT "":U,                                                                            /* ipcOwningEntityKey       */
                                         INPUT "doc_num":U,                                                                     /* ipcFieldName             */
                                         INPUT btt_auth_provider.line_number,                                                   /* ipiLineNumber            */
                                         INPUT "ma_MsgAuth":U,                                                                  /* ipcMessageGroup          */
                                         INPUT 28, /* &1 Provider record (&2) may not be deleted - details lines are linked */  /* ipiMessageNumber         */
                                         INPUT cProviderTypeLabel + "," + STRING(btt_auth_provider.doc_num)).                   /* ipcReplaceTextList       */
    END. /* ELSE - IF btt_auth_provider.rate_change_type <> "" THEN */

    /*
      Auth is complete
    */
    IF NOT goAuthorisation:AuthIncomplete THEN
    DO:
      /*
        Once claims have been paid, a Provider record may not be deleted.
      */
      IF btt_auth_provider.amount_paid   <> 0
      OR btt_auth_provider.quantity_paid <> 0
      THEN DO:
        ASSIGN cErrorMessage = "The Authorisation Provider record cannot be deleted, as claims have already been paid against this Provider".
        goErrorObject:addError(INPUT "hatap":U,
                               INPUT btt_auth_provider.auth_provider_obj,
                               INPUT "":U,
                               INPUT "":U,
                               INPUT btt_auth_provider.line_number,
                               INPUT cErrorMessage,
                               INPUT "ERR":U).
      END.  /* IIF btt_auth_provider.amount_paid <> 0... */
    END. /* IF NOT goAuthorisation:AuthIncomplete */
  END. /* IF btt_auth_provider.record_action = 'DELETE' THEN*/
END. /* IF goAuthorisation:InFocus AND AVAILABLE btt_auth_provider THEN */

&ENDIF

{ mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oAcronymHelper)  THEN DELETE OBJECT(oAcronymHelper).
                                      IF VALID-OBJECT(oAuthRuleSearch) THEN DELETE OBJECT(oAuthRuleSearch)."}
