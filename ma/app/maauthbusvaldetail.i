/* maauthbusvaldetail.i  MEDSTAR Medical Aid System
                         Validate Auth Detail Buffer
                         (c) Copyright 1990 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/                      

DEFINE PARAMETER BUFFER btt_auth_detail   FOR tt_auth_detail.
DEFINE PARAMETER BUFFER btt_auth_provider FOR tt_auth_provider.
DEFINE PARAMETER BUFFER btt_auth          FOR tt_auth.

DEFINE VARIABLE cAction                            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAllowDiscountOnUnlimitedRuleValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cARSRate                           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBaseRate                          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDiscountType                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMessage                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorType                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessageType                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRuleValue                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEntity                            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldChanged                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldChangedLabel                 AS CHARACTER NO-UNDO.

DEFINE VARIABLE dLOSVariance                       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dLOSCalculation                    AS DECIMAL   NO-UNDO.

DEFINE VARIABLE lAcknowledge                       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAllowDiscountOnUnlimitedValidRule AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMandatory                         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSuccess                           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidationError                   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidRule                         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidStatus                       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidGroupDetail                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFoundGroupDetail                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCriticalAuthRule                  AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lDetailSetupPresent                AS LOGICAL   NO-UNDO INITIAL FALSE.

&IF {&DBDFMA} >= 010195 &THEN

DEFINE VARIABLE oBodyRegion        AS cls.mipacronym            NO-UNDO.
DEFINE VARIABLE oAuthDetailLines   AS cls.mipacronym            NO-UNDO.
DEFINE VARIABLE oAuthRule          AS cls.maauthrule            NO-UNDO.
DEFINE VARIABLE oDetail            AS cls.maauthorisationdetail NO-UNDO.

DEFINE BUFFER buf_auth_detail      FOR hat_auth_detail.
DEFINE BUFFER bbt_auth_detail      FOR tt_auth_detail.
DEFINE BUFFER bbt_auth_provider    FOR tt_auth_provider.

IF goAuthorisation:InFocus AND AVAILABLE btt_auth_detail THEN
DO:

  ASSIGN oAuthRule = NEW cls.maauthrule()
         oDetail   = NEW cls.maauthorisationdetail().

  /* Check Auth Type setup for Provider to determine if detail lines may be authorised for Provider */
  IF AVAILABLE ttAuthTypeconfig THEN
  DO:
    oAuthDetailLines = NEW cls.mipacronym(?, FALSE, "ma_acAuthTypeDetails":U, ?).

    oAuthDetailLines:focusAcronym("KEY":U, ttAuthTypeConfig.AuthoriseDetailLines) NO-ERROR.

    IF  oAuthDetailLines:AcronymInFocus
    AND oAuthDetailLines:AcronymLabel = "NONE":U THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT "No details may be captured.  The Auth Type setup for this Provider does not allow it ":U,
                                      INPUT "ERR":U).
    END.  /*ELSE: IF NOT oAuthDetailLines:AcronymInFocus THEN*/
  END.  /*IF AVAILABLE ttAuthTypeconfig*/

  FIND buf_auth_detail NO-LOCK
    WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE buf_auth_detail THEN
  DO:
    /* Auth Status */
    IF   NOT btt_auth.auth_incomplete
    AND  btt_auth_detail.auth_status   <> buf_auth_detail.auth_status
    AND (btt_auth_detail.amount_paid   <> 0.00
    OR   btt_auth_detail.quantity_paid <> 0)
    THEN
      ASSIGN
        cErrorMessage = "The Clinical Detail Status cannot be changed, as claims have already been paid against this Clinical Detail record"
        lSuccess 	    = goErrorObject:addError
                 	                   (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,      /* ipcOwningEntityMnemonic */
                 	                    INPUT btt_auth_detail.auth_detail_obj, /* ipdOwningEntityObj      */
                 	                    INPUT "":U,                            /* ipcOwningEntityKey      */
                 	                    INPUT "auth_status":U,                 /* ipcFieldName            */
                 	                    INPUT btt_auth_detail.line_number,     /* ipiLineNumber           */
                 	                    INPUT cErrorMessage,                   /* ipcMessageText          */
                 	                    INPUT "ERR":U).                        /* ipiMessageType          */

    /*
      If the detail line was declined, it may have been declined for example
      due to a non-chargeable nappi in the related item.
      However, if the user changes the related item, we need to validate this
      item again, so we need to reverse the status change from declined
      back to authorised.
    */
    IF   buf_auth_detail.auth_status  = 6 /* Declined */
    AND (buf_auth_detail.related_obj <> btt_auth_detail.related_obj
    OR   buf_auth_detail.related_key <> btt_auth_detail.related_key) THEN
    DO:
      IF NOT AVAILABLE btt_auth_provider
      THEN
        FIND btt_auth_provider
          WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
          NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE btt_auth_provider
      THEN
        ASSIGN btt_auth_detail.auth_status      = btt_auth_provider.auth_status
               btt_auth_detail.auth_status_note = btt_auth_provider.auth_status_note
               btt_auth_detail.claim_code       = 0.
    END.  /* IF  buf_auth_detail.auth_status = 6 */
  END.  /* IF AVAILABLE buf_auth_detail THEN */

  /*
  Check if any tariff modifiers must be created automatically
  For dependant 99, this cannot happen
  */
  IF  btt_auth_detail.owning_entity_mnemonic              = "htmtl":U
  AND LOOKUP(STRING(btt_auth_detail.auth_status), "5, 6") = 0          /* Cancelled, Declined */
  AND CAN-FIND(FIRST hlm_code_link
               WHERE hlm_code_link.acronym_key   BEGINS "ma_acCodeLinkCatCreateTariff":U
               AND   hlm_code_link.parent_entity      = btt_auth_detail.owning_entity_mnemonic
               AND   hlm_code_link.parent_entity_obj  = btt_auth_detail.owning_obj
               AND   hlm_code_link.effective_date    <= btt_auth_detail.start_date
               AND  (hlm_code_link.end_date           = ?
               OR    hlm_code_link.end_date          >= btt_auth_detail.end_date))
  THEN DO:
    IF btt_auth.dependant = 99
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,        /* ipcOwningEntityMnemonic */
                                      INPUT btt_auth_detail.auth_detail_obj,                          /* ipdOwningEntityObj      */
                                      INPUT "":U,                                                     /* ipcOwningEntityKey      */
                                      INPUT "tariff_code":U,                                          /* ipcFieldName            */
                                      INPUT btt_auth_detail.line_number,                              /* ipiLineNumber           */
                                      INPUT "MA":U,                                                   /* ipcMessageGroup         */
                                      INPUT 112,  /* The '&1' specified is invalid. &2  */            /* ipiMessageNumber        */
                                      INPUT "Tariff Code: ":U  +
                                            ",Tariff cannot be loaded for an unknown dependant because dependant details are required for automatic creation of Modifier Tariffs that are linked to the Tariff":U).  /* ipcReplaceTextList */

  END. /* IF  btt_auth_detail.owning_entity_mnemonic  = "htmtl":U */

  /*
    LOS is activated
  */
  IF AVAILABLE ttAuthTypeconfig
  AND ttAuthTypeConfig.ActivateLos THEN
  DO:
    /* Quantity-LOS */
    IF AVAILABLE buf_auth_detail
    AND buf_auth_detail.loc_tariff_type_obj <> 0
    AND buf_auth_detail.quantity_auth        > 0
    AND btt_auth_detail.quantity_los        <= 0 THEN
    DO:
      FIND FIRST htm_tariff_type NO-LOCK
        WHERE htm_tariff_type.tariff_type_obj = buf_auth_detail.loc_tariff_type_obj
          AND htm_tariff_type.acronym_key     = "ma_acTariffTypeCatLOC"
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE htm_tariff_type THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,        /* ipcOwningEntityMnemonic */
                                        INPUT btt_auth_detail.auth_detail_obj,                          /* ipdOwningEntityObj      */
                                        INPUT "":U,                                                     /* ipcOwningEntityKey      */
                                        INPUT "quantity_los":U,                                         /* ipcFieldName            */
                                        INPUT btt_auth_detail.line_number,                              /* ipiLineNumber           */
                                        INPUT "MA":U,                                                   /* ipcMessageGroup         */
                                        INPUT 112,  /* The '&1' specified is invalid. &2  */            /* ipiMessageNumber        */
                                        INPUT "Quantity LOS: ":U + STRING(btt_auth_detail.quantity_los) +
                                              ",Must be greater than 0.":U).                            /* ipcReplaceTextList      */
      END.  /* IF AVAILABLE htm_tariff_type... */
    END.  /* IF AVAILABLE buf_auth_detail... */

    /* Ars-Rate */
    IF btt_auth_detail.default_ars_rate <> "":U THEN
    DO:
      FIND FIRST arsrate NO-LOCK
        WHERE arsrate.ars-rate = btt_auth_detail.default_ars_rate
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE arsrate THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,           /* ipcOwningEntityMnemonic */
                                        INPUT btt_auth_detail.auth_detail_obj,                             /* ipdOwningEntityObj      */
                                        INPUT "":U,                                                        /* ipcOwningEntityKey      */
                                        INPUT "default_ars_rate":U,                                        /* ipcFieldName            */
                                        INPUT btt_auth_detail.line_number,                                 /* ipiLineNumber           */
                                        INPUT "MA":U,                                                      /* ipcMessageGroup         */
                                        INPUT 100,  /* The '&1' specified is invalid */                    /* ipiMessageNumber        */
                                        INPUT "ARS rate: ":U + STRING(btt_auth_detail.default_ars_rate)).  /* ipcReplaceTextList      */
      END.  /* IF NOT AVAILABLE arsrate THEN */
    END.  /* IF btt_auth_detail.default_ars_rate <> "" THEN */

    /* Base-Rate */
    IF btt_auth_detail.default_base_rate <> "":U THEN
    DO:
      /* Only check base rate for Tariff and Crosswalk */
      IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr":U) > 0 THEN
      DO:
        FIND FIRST baserate NO-LOCK
          WHERE baserate.base-rate = btt_auth_detail.default_base_rate
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE baserate THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,            /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                              /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                         /* ipcOwningEntityKey      */
                                          INPUT "default_base_rate":U,                                        /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                  /* ipiLineNumber           */
                                          INPUT "MA":U,                                                       /* ipcMessageGroup         */
                                          INPUT 100,  /* The '&1' specified is invalid */                     /* ipiMessageNumber        */
                                          INPUT "Base rate: ":U + STRING(btt_auth_detail.default_base_rate)). /* ipcReplaceTextList      */
        END.  /* IF NOT AVAILABLE baserate THEN */
      END.  /* IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr") > 0 THEN */
      ELSE
        ASSIGN btt_auth_detail.default_base_rate = "".
    END.  /* IF btt_auth_detail.default_base_rate <> "" THEN */

    /* Ars-Rate */
    IF btt_auth_detail.override_ars_rate <> "":U THEN
    DO:
      FIND FIRST arsrate NO-LOCK
        WHERE arsrate.ars-rate = btt_auth_detail.override_ars_rate
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE arsrate THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,           /* ipcOwningEntityMnemonic */
                                        INPUT btt_auth_detail.auth_detail_obj,                             /* ipdOwningEntityObj      */
                                        INPUT "":U,                                                        /* ipcOwningEntityKey      */
                                        INPUT "override_ars_rate":U,                                       /* ipcFieldName            */
                                        INPUT btt_auth_detail.line_number,                                 /* ipiLineNumber           */
                                        INPUT "MA":U,                                                      /* ipcMessageGroup         */
                                        INPUT 100,  /* The '&1' specified is invalid */                    /* ipiMessageNumber        */
                                        INPUT "ARS rate: ":U + STRING(btt_auth_detail.override_ars_rate)). /* ipcReplaceTextList      */
      END.  /* IF NOT AVAILABLE arsrate THEN */
    END.  /* IF btt_auth_detail.override_ars_rate <> "" THEN */

    /* Base-Rate */
    IF btt_auth_detail.override_base_rate <> "":U THEN
    DO:
      /* Only check base rate for Tariff and Crosswalk */
      IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr":U) > 0 THEN
      DO:
        FIND FIRST baserate NO-LOCK
          WHERE baserate.base-rate = btt_auth_detail.override_base_rate
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE baserate THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                          INPUT "override_base_rate":U,                                        /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                          INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                          INPUT 100,  /* The '&1' specified is invalid */                      /* ipiMessageNumber        */
                                          INPUT "Base rate: ":U + STRING(btt_auth_detail.override_base_rate)). /* ipcReplaceTextList      */
        END.  /* IF NOT AVAILABLE baserate THEN */
      END.  /* IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr") > 0 THEN */
      ELSE
        ASSIGN btt_auth_detail.override_base_rate = "".
    END.  /* IF btt_auth_detail.override_base_rate <> "" THEN */
  END.  /* IF AVAILABLE ttAuthTypeconfig AND ttAuthTypeConfig.ActivateLos THEN */

  /* Validate Tariff Type for Theatre minutes - only for Tariffs */
  IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
  DO:
    /* Get Authrule 'EnforceMinutes' setup */
    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                   INPUT  0,
                                                   INPUT  "ma_acAuthRuleTypeMinCalc":U,
                                                   INPUT  "EnforceMinutes":U,
                                                   INPUT  btt_auth.start_date,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).

    IF lValidRule THEN
    DO:
      FIND htm_tariff_type NO-LOCK
        WHERE htm_tariff_type.tariff_type_obj = btt_auth_detail.minutes_tariff_type_obj NO-ERROR.

      IF AVAILABLE htm_tariff_type THEN
      DO:
        /* Tariff Type category is not set up in rule value */
        IF LOOKUP(htm_tariff_type.acronym_key, cRuleValue) = 0 THEN
        DO:
          IF btt_auth_detail.minutes_auth <> 0 THEN
            ASSIGN cErrorMessage = "Minutes authorised is not allowed."
                   lSuccess      = goErrorObject:addError
                                                (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                                 INPUT btt_auth_detail.auth_detail_obj,
                                                 INPUT "":U,
                                                 INPUT "minutes_auth":U,
                                                 INPUT btt_auth_detail.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).

          IF btt_auth_detail.minutes_requested <> 0 THEN
            ASSIGN cErrorMessage = "Minutes requested is not allowed."
                   lSuccess      = goErrorObject:addError
                                                (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                                 INPUT btt_auth_detail.auth_detail_obj,
                                                 INPUT "":U,
                                                 INPUT "minutes_requested":U,
                                                 INPUT btt_auth_detail.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).

          IF btt_auth_detail.minutes_tariff_type_obj <> 0 THEN
            ASSIGN cErrorMessage = "Tariff Type for minutes is not allowed."
                   lSuccess      = goErrorObject:addError
                                                (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                                 INPUT btt_auth_detail.auth_detail_obj,
                                                 INPUT "":U,
                                                 INPUT "minutes_tariff_type_obj":U,
                                                 INPUT btt_auth_detail.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).
        END.  /* IF LOOKUP(htm_tariff_type.acronym_key, cRuleValue) = 0 THEN  */
        ELSE DO:  /* Tariff Type category IS set up in rule value */
          IF btt_auth_detail.minutes_auth = 0 THEN
            ASSIGN cErrorMessage = "Minutes authorised is required."
                   lSuccess      = goErrorObject:addError
                                                (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                                 INPUT btt_auth_detail.auth_detail_obj,
                                                 INPUT "":U,
                                                 INPUT "minutes_auth":U,
                                                 INPUT btt_auth_detail.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).

          IF btt_auth_detail.quantity_requested <> 0 THEN
            ASSIGN cErrorMessage = "Quantity requested is not allowed to be captured if minutes are required."
                   lSuccess      = goErrorObject:addError
                                                (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                                 INPUT btt_auth_detail.auth_detail_obj,
                                                 INPUT "":U,
                                                 INPUT "quantity_requested":U,
                                                 INPUT btt_auth_detail.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).

          IF btt_auth_detail.minutes_tariff_type_obj = 0 THEN
            ASSIGN cErrorMessage = "Tariff Type for minutes is required."
                   lSuccess      = goErrorObject:addError
                                                (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                                 INPUT btt_auth_detail.auth_detail_obj,
                                                 INPUT "":U,
                                                 INPUT "minutes_tariff_type_obj":U,
                                                 INPUT btt_auth_detail.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).
        END.  /* ELSE - IF LOOKUP(htm_tariff_type.acronym_key, cRuleValue) = 0 THEN  */

        /*
          User should not be allowed to update Quantity Authorised if minutes apply, because
          Quantity Authorised will be calculated by the processing
        */
        IF AVAILABLE buf_auth_detail
        AND btt_auth_detail.quantity_auth <> buf_auth_detail.quantity_auth THEN
          ASSIGN cErrorMessage = "Quantity Authorised may not be updated if Minutes apply."
                 lSuccess      = goErrorObject:addError
                                              (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                               INPUT btt_auth_detail.auth_detail_obj,
                                               INPUT "":U,
                                               INPUT "quantity_auth":U,
                                               INPUT btt_auth_detail.line_number,
                                               INPUT cErrorMessage,
                                               INPUT "ERR":U).

      END.  /* IF AVAILABLE htm_tariff_type THEN */
    END.  /* IF lValidRule THEN */
    ELSE DO:
      IF btt_auth_detail.minutes_auth <> 0 THEN
        ASSIGN cErrorMessage = "Minutes authorised is not allowed."
               lSuccess      = goErrorObject:addError
                                            (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                             INPUT btt_auth_detail.auth_detail_obj,
                                             INPUT "":U,
                                             INPUT "minutes_auth":U,
                                             INPUT btt_auth_detail.line_number,
                                             INPUT cErrorMessage,
                                             INPUT "ERR":U).

      IF btt_auth_detail.minutes_requested <> 0 THEN
        ASSIGN cErrorMessage = "Minutes requested is not allowed."
               lSuccess      = goErrorObject:addError
                                            (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                             INPUT btt_auth_detail.auth_detail_obj,
                                             INPUT "":U,
                                             INPUT "minutes_requested":U,
                                             INPUT btt_auth_detail.line_number,
                                             INPUT cErrorMessage,
                                             INPUT "ERR":U).

      IF btt_auth_detail.minutes_tariff_type_obj <> 0 THEN
        ASSIGN cErrorMessage = "Tariff Type for minutes is not allowed."
               lSuccess      = goErrorObject:addError
                                            (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                             INPUT btt_auth_detail.auth_detail_obj,
                                             INPUT "":U,
                                             INPUT "minutes_tariff_type_obj":U,
                                             INPUT btt_auth_detail.line_number,
                                             INPUT cErrorMessage,
                                             INPUT "ERR":U).
    END.  /* ELSE - IF lValidRule THEN */
  END.  /* IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN */

  IF btt_auth_detail.quantity_los > 0
  AND (btt_auth_detail.override_base_rate <> "":U OR btt_auth_detail.override_ars_rate <> "":U) THEN
  DO:
    ASSIGN cErrorMessage = "[Base/ARS] Rate/s may not be changed for LOC Tariffs.":U
           lSuccess      = goErrorObject:addError
                                        (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                         INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                         INPUT "":U,                                                /* ipcOwningEntityKey       */
                                         INPUT "override_base_rate":U,                              /* ipcFieldName             */
                                         INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                         INPUT cErrorMessage,                                       /* ipcMessageText           */
                                         INPUT "ERR":U).                                            /* ipcMessageType           */
  END. /* IF btt_auth_detail.quantity_los > 0 */

  IF CAN-FIND(FIRST hat_auth_provider NO-LOCK
              WHERE hat_auth_provider.auth_obj = btt_auth_detail.auth_obj
                AND hat_auth_provider.rate_change_type <> "ma_acAuthRateChangeTypeNone":U)
  AND (btt_auth_detail.override_base_rate <> "":U OR btt_auth_detail.override_ars_rate <> "":U) THEN
  DO:
    ASSIGN cErrorMessage = "Base or ARS Rates may not be changed if an Authorisation Rate Change has been applied. Please use the Auth Reguide functionality.":U
           lSuccess      = goErrorObject:addError
                                        (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                         INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                         INPUT "":U,                                                /* ipcOwningEntityKey       */
                                         INPUT "override_base_rate":U,                              /* ipcFieldName             */
                                         INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                         INPUT cErrorMessage,                                       /* ipcMessageText           */
                                         INPUT "ERR":U).                                            /* ipcMessageType           */
  END. /* IF CAN-FIND(hat_auth_provider NO-LOCK WHERE hat_auth_provider.rate_change_type <> "":U) */

  IF btt_auth_detail.end_date <> ? AND btt_auth_detail.end_date < btt_auth_detail.start_date THEN
  DO:
    ASSIGN cErrorMessage = "The authorisation detail end date must be greater than or equal to the authorisation detail start date.":U
           lSuccess      = goErrorObject:addError
                                        (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                         INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                         INPUT "":U,                                                /* ipcOwningEntityKey       */
                                         INPUT "end_date":U,                                        /* ipcFieldName             */
                                         INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                         INPUT cErrorMessage,                                       /* ipcMessageText           */
                                         INPUT "ERR":U).                                            /* ipcMessageType           */
  END. /* IF btt_auth_detail.end_date <> ? AND btt_auth_detail.end_date < btt_auth_detail.start_date THEN */

  /*
    Date validations against Auth header
    Removed hat_auth DB call in favour of temp-table btt_auth that is provided
  */
  FIND btt_auth NO-LOCK
    WHERE btt_auth.auth_obj = btt_auth_detail.auth_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  FIND btt_auth_provider NO-LOCK
    WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  /*
    When we have switched to User Calc, we do not want to trigger date validations
    until Final Save of the Auth
  */
  IF AVAILABLE btt_auth
  AND AVAILABLE btt_auth_provider
  AND (btt_auth_provider.los_calculation = TRUE
  OR  (btt_auth_provider.los_calculation = FALSE
  AND  btt_auth._final_save              = TRUE))
  AND  btt_auth_detail.record_action = "MODIFY":U THEN
  DO:
    IF btt_auth_detail.start_date < btt_auth.start_date THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT "start_date":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT "ma_MsgAuth":U,
                                      INPUT 24,  /* The &1 Date &2 cannot be before the &3 Date &4 */
                                      INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") +
                                           ",Authorisation Header Start,":U + STRING(btt_auth.start_date, "9999/99/99")).
    END. /*IF btt_auth_detail.start_date < buf_auth.start_date THEN*/

    IF btt_auth.end_date <> ? THEN
    DO:
      IF btt_auth_detail.start_date > btt_auth.end_date THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                        INPUT btt_auth_detail.auth_detail_obj,
                                        INPUT "":U,
                                        INPUT "start_date":U,
                                        INPUT btt_auth_detail.line_number,
                                        INPUT "ma_MsgAuth":U,
                                        INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */
                                        INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") +
                                             ",Authorisation Header End,":U + STRING(btt_auth.end_date, "9999/99/99")).
      END. /*IF btt_auth_detail.start_date > buf_auth.end_date THEN*/

      IF  btt_auth_detail.end_date <> ?
      AND btt_auth_detail.end_date  > btt_auth.end_date THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                        INPUT btt_auth_detail.auth_detail_obj,
                                        INPUT "":U,
                                        INPUT "end_date":U,
                                        INPUT btt_auth_detail.line_number,
                                        INPUT "ma_MsgAuth":U,
                                        INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */
                                        INPUT "Authorisation Detail End,":U + STRING(btt_auth_detail.end_date, "9999/99/99") +
                                             ",Authorisation Header End,":U + STRING(btt_auth.end_date, "9999/99/99")).
      END. /*IF btt_auth_detail.end_date <> ? AND buf_auth.end_date <> ?*/
    END.  /* IF btt_auth.end_date <> ? THEN */
  END. /*IF AVAILABLE buf_auth THEN*/

  /*
    When we have switched to User Calc, we do not want to trigger date validations
    until Final Save of the Auth
  */
  IF  AVAILABLE btt_auth
  AND AVAILABLE btt_auth_provider
  AND (btt_auth_provider.los_calculation = TRUE
  OR  (btt_auth_provider.los_calculation = FALSE
  AND  btt_auth._final_save              = TRUE)) THEN
  DO:
    IF btt_auth_detail.record_action = "MODIFY":U THEN
    DO:
      IF btt_auth_detail.start_date < btt_auth_provider.start_date THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                        INPUT btt_auth_detail.auth_detail_obj,
                                        INPUT "":U,
                                        INPUT "start_date":U,
                                        INPUT btt_auth_detail.line_number,
                                        INPUT "ma_MsgAuth":U,
                                        INPUT 24,  /* The &1 Date &2 cannot be before the &3 Date &4 */
                                        INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") +
                                             ",Authorisation Provider Start,":U + STRING(btt_auth_provider.start_date, "9999/99/99")).
      END. /*IF btt_auth_detail.start_date < buf_auth_provider.start_date THEN*/

      IF btt_auth_provider.end_date <> ? THEN
      DO:
        IF btt_auth_detail.start_date > btt_auth_provider.end_date THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                          INPUT btt_auth_detail.auth_detail_obj,
                                          INPUT "":U,
                                          INPUT "start_date":U,
                                          INPUT btt_auth_detail.line_number,
                                          INPUT "ma_MsgAuth":U,
                                          INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */
                                          INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") +
                                               ",Authorisation Provider End,":U + STRING(btt_auth_provider.end_date, "9999/99/99")).
        END. /*IF btt_auth_detail.start_date > buf_auth_provider.end_date THEN*/

        IF  btt_auth_detail.end_date <> ?
        AND btt_auth_detail.end_date  > btt_auth_provider.end_date THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                          INPUT btt_auth_detail.auth_detail_obj,
                                          INPUT "":U,
                                          INPUT "end_date":U,
                                          INPUT btt_auth_detail.line_number,
                                          INPUT "ma_MsgAuth":U,
                                          INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */
                                          INPUT "Authorisation Detail End,":U + STRING(btt_auth_detail.end_date, "9999/99/99") +
                                               ",Authorisation Provider End,":U + STRING(btt_auth_provider.end_date, "9999/99/99")).
        END. /*IF btt_auth_detail.end_date <> ? AND buf_auth_provider.end_date <> ? THEN*/
      END.  /* IF btt_auth_provider.end_date <> ? THEN */
    END.  /* IF btt_auth_detail.record_action = "MODIFY":U THEN */

    /*
      User LOS Calculation
      Validations to ensure data manually captured is within variance
    */
    IF  btt_auth_provider.los_calculation = FALSE
    AND btt_auth_detail.loc_sequence     <> 0 THEN
    DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                     INPUT  0,
                                                     INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                     INPUT  "UserCalcLOSVariance":U,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).

      ASSIGN dLOSVariance    = 0
             dLOSCalculation = 0.

      IF lValidRule AND NUM-ENTRIES(cRuleValue,"|") > 1 THEN
      DO:
        ASSIGN cAction         = ENTRY(2,cRuleValue,"|")
               dLOSVariance    = DECIMAL(ENTRY(1,cRuleValue,"|")).
               dLOSCalculation = btt_auth_detail.end_date - btt_auth_detail.start_date
                               + (IF  btt_auth_detail.start_ampm = btt_auth_detail.end_ampm
                                  OR (btt_auth_detail.end_date - btt_auth_detail.start_date = 0)
                                  THEN 0.5
                                  ELSE 0).

        /*
          We use the calculated LOS and compare it to the actual LOS entered.
          If the difference if more than the variance allowed, as setup on the rule,
          we raise an error/warning
        */
        IF ABSOLUTE(btt_auth_detail.quantity_los - dLOSCalculation) > dLOSVariance THEN
        DO:
          IF CAN-DO("WARN,WARNACK,BLOCK":U,cAction) THEN
          DO:
            ASSIGN
              lAcknowledge  = (IF cAction = "WARNACK":U
                               THEN TRUE
                               ELSE FALSE)

              cErrorMessage = "Manually updated Start/End Date AM/PM exceeds variance allowed for Tariff " + btt_auth_detail.owning_alt_value + " LOC Seq "
                            + STRING(btt_auth_detail.loc_sequence,">>9")
                            + "[HELP=Auth Rule Code: UserCalcLOSVariance]".

              cErrorType    = IF LOOKUP(cAction,"WARN,WARNACK":U) > 0
                              THEN "WAR":U
                              ELSE "ERR":U.

              goErrorObject:addError
                           (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                            INPUT btt_auth_detail.auth_detail_obj,
                            INPUT "":U,
                            INPUT "":U,
                            INPUT btt_auth_detail.line_number,
                            INPUT cErrorMessage,
                            INPUT cErrorType,
                            INPUT lAcknowledge).

          END. /* IF CAN-DO("WARN,WARNACK,BLOCK",cAction) */
        END. /* IF ABSOLUTE(btt_auth_detail.quantity_los - dLOSCalculation) > dLOSVariance */
      END. /* IF lValidRule AND NUM-ENTRIES(cRuleValue,"|") > 1*/
    END. /* IF btt_auth_provider.los_calculation = FALSE */

  END. /*IF AVAILABLE buf_auth_provider THEN*/

  /*
    PMB Validations
  */
  IF btt_auth_detail.pmb_indicator <> YES THEN
  DO:
    IF btt_auth_detail.pmb_benefit_% <> 0.00 THEN
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,            /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth_detail.auth_detail_obj,                                /* ipdOwningEntityObj       */
                                      INPUT "":U,                                                           /* ipcOwningEntityKey       */
                                      INPUT "pmb_benefit_%":U,                                              /* ipcFieldName             */
                                      INPUT btt_auth_detail.line_number,                                    /* ipiLineNumber            */
                                      INPUT "MA":U,                                                         /* ipcMessageGroup          */
                                      INPUT 112,  /* The &1 specified is invalid. &2 */                     /* ipiMessageNumber         */
                                      INPUT "PMB Benefit Percentage,Update on the field is not allowed":U). /* ipcReplaceTextList*/

    IF btt_auth_detail.pmb_pay_cost = YES THEN
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,     /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj       */
                                      INPUT "":U,                                                    /* ipcOwningEntityKey       */
                                      INPUT "pmb_pay_cost":U,                                        /* ipcFieldName             */
                                      INPUT btt_auth_detail.line_number,                             /* ipiLineNumber            */
                                      INPUT "MA":U,                                                  /* ipcMessageGroup          */
                                      INPUT 112,  /* The &1 specified is invalid. &2 */              /* ipiMessageNumber         */
                                      INPUT "PMB Pay Cost,Update on the field is not allowed":U).    /* ipcReplaceTextList       */

  END. /*IF btt_auth_detail.pmb_indicator <> YES THEN*/
  ELSE DO: /*if pmb_indicator = yes*/
    IF btt_auth_detail.pmb_benefit_% < 0.00 THEN
      ASSIGN
        cErrorMessage = "Please enter PMB Benefit Percentage value greater than ~"0.00%~"."
        lSuccess      = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                      INPUT "":U,                                                /* ipcOwningEntityKey       */
                                      INPUT "pmb_benefit_%":U,                                   /* ipcFieldName             */
                                      INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                      INPUT "MA":U,                                              /* ipcMessageGroup          */
                                      INPUT 112,  /* The &1 specified is invalid. &2 */          /* ipiMessageNumber         */
                                      INPUT "PMB Benefit Percentage," + cErrorMessage ).

    IF  btt_auth_detail.pmb_benefit_%  > 0.00
    AND btt_auth_detail.pmb_pay_cost   = YES
    THEN
      ASSIGN
        cErrorMessage = "Either enter PMB Benefit Percentage value greater than ~"0.00%~" or check the ~"PMB Pay Cost~" box ."
        lSuccess      = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                      INPUT "":U,                                                /* ipcOwningEntityKey       */
                                      INPUT IF AVAILABLE buf_auth_detail
                                            AND buf_auth_detail.pmb_pay_cost <> btt_auth_detail.pmb_pay_cost
                                            THEN "pmb_pay_cost":U
                                            ELSE IF AVAILABLE buf_auth_detail
                                                 THEN "pmb_benefit_%":U
                                                 ELSE "":U,                                      /* ipcFieldName             */
                                      INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                      INPUT "MA":U,                                              /* ipcMessageGroup          */
                                      INPUT 112,  /* The &1 specified is invalid. &2 */          /* ipiMessageNumber         */
                                      INPUT "PMB Benefit Percentage and PMB pay Cost ," + cErrorMessage ).
  END. /*if pmb_indicator = yes*/

  /* Added_by_user */
  IF btt_auth_detail.added_by_user = ?
  THEN
    ASSIGN btt_auth_detail.added_by_user = TRUE.

  CASE btt_auth_detail.owning_entity_mnemonic:
    WHEN "hlmnl":U  THEN ASSIGN cEntity = "Nappi".
    WHEN "htmtl":U  THEN ASSIGN cEntity = "Tariff".
    WHEN "hlmcr":U  THEN ASSIGN cEntity = "Basket".
    OTHERWISE            ASSIGN cEntity = "Entity".
  END CASE.

  IF AVAILABLE ttAuthTypeConfig THEN
  DO:
    IF ttAuthTypeConfig.DefaultClaimCodeDetail = ?
    THEN
      ASSIGN ttAuthTypeConfig.DefaultClaimCodeDetail = 0.

    IF btt_auth_detail.claim_code = 0 OR btt_auth_detail.claim_code = ?
    THEN
      ASSIGN btt_auth_detail.claim_code = ttAuthTypeConfig.DefaultClaimCodeDetail.

    IF TRIM(btt_auth_detail.claim_type) = "":U OR btt_auth_detail.claim_type = ?
    THEN
      ASSIGN btt_auth_detail.claim_type = ttAuthTypeConfig.DefaultClaimTypeDetail .

    IF btt_auth_detail.added_by_user = TRUE THEN
    DO:
      IF ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE
      AND btt_auth_detail.claim_code <> ttAuthTypeConfig.DefaultClaimCodeDetail THEN
      DO:
        ASSIGN cErrorType    = "ERR":U
               lAcknowledge  =  TRUE
               cErrorMessage = "User update of detail claim code for detail line " + btt_auth_detail.owning_alt_value + " is not allowed":U.

        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT cErrorMessage,
                      INPUT cErrorType,
                      INPUT lAcknowledge).
      END. /*IF ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE AND btt_auth_detail.claim_code <> ttAuthTypeConfig.DefaultClaimCodeDetail THEN*/

      IF  ttAuthTypeConfig.ClaimTypeUpdAllowed = FALSE
      AND btt_auth_detail.claim_type <> ttAuthTypeConfig.DefaultClaimTypeDetail THEN
      DO:
        ASSIGN cErrorType    = "ERR":U
               lAcknowledge  =  TRUE
               cErrorMessage = "User update of detail claim type for detail line " + btt_auth_detail.owning_alt_value  + " is not allowed":U.

        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT cErrorMessage,
                      INPUT cErrorType,
                      INPUT lAcknowledge).
      END. /*IF btt_auth_detail.claim_type <> ttAuthTypeConfig.ClaimtypesDetail THEN*/

    END. /* IF btt_auth_detail.added_by_user = TRUE THEN */
    ELSE
    DO:
      IF btt_auth_detail.auth_detail_obj > 0 THEN
      DO:
        IF AVAILABLE buf_auth_detail THEN
        DO:
          IF  ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE
          AND btt_auth_detail.claim_code <> buf_auth_detail.claim_code THEN
          DO:
            ASSIGN cErrorType    = "ERR":U
                   lAcknowledge  =  TRUE
                   cErrorMessage = "User update of detail claim code for detail line " + btt_auth_detail.owning_alt_value + " is not allowed":U.

            goErrorObject:addError
                         (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                          INPUT btt_auth_detail.auth_detail_obj,
                          INPUT "":U,
                          INPUT "":U,
                          INPUT btt_auth_detail.line_number,
                          INPUT cErrorMessage,
                          INPUT cErrorType,
                          INPUT lAcknowledge).
          END. /*IF ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE AND btt_auth_detail.claim_code <> buf_auth_detail.claim_code THEN DO:*/

          IF  ttAuthTypeConfig.ClaimTypeUpdAllowed = FALSE
          AND btt_auth_detail.claim_type <> buf_auth_detail.claim_type THEN
          DO:
            ASSIGN cErrorType    = "ERR":U
                   lAcknowledge  =  TRUE
                   cErrorMessage = "User update of detail claim type for detail line " + btt_auth_detail.owning_alt_value + " is not allowed":U.

            goErrorObject:addError (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                    INPUT btt_auth_detail.auth_detail_obj,
                                    INPUT "":U,
                                    INPUT "":U,
                                    INPUT btt_auth_detail.line_number,
                                    INPUT cErrorMessage,
                                    INPUT cErrorType,
                                    INPUT lAcknowledge).
          END. /*IF ttAuthTypeConfig.ClaimTypeUpdAllowed = FALSE AND btt_auth_detail.claim_type <> buf_auth_detail.claim_type THEN*/
        END. /* IF AVAILABLE buf_auth_detail THEN DO */
      END. /* IF btt_auth_detail.auth_detail_obj > 0 THEN DO */
    END. /* ELSE - IF btt_auth_detail.added_by_user = TRUE THEN */
  END. /* IF AVAILABLE ttAuthTypeConfig THEN */

  /*
    Check to make sure that the auth group of the detail lines matches the auth group of the provider
  */
  IF  btt_auth_detail.record_action  <> "Delete":U
  AND btt_auth_detail.auth_group_obj <> 0.00 THEN
  DO:
    IF btt_auth_detail.quantity_los > 0 THEN
    DO:
      ASSIGN btt_auth_detail.auth_group_obj = 0.00.

      goErrorObject:addError
                   (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                    INPUT btt_auth_detail.auth_detail_obj,
                    INPUT "":U,
                    INPUT "auth_group_obj":U,
                    INPUT btt_auth_detail.line_number,
                    INPUT "You may not add a auth group to an LOC line.",
                    INPUT "WAR":U).
    END.  /* IF btt_auth_detail.quantity_los > 0 THEN */
    ELSE DO:
      FIND FIRST ham_auth_group NO-LOCK
        WHERE ham_auth_group.auth_group_obj = btt_auth_detail.auth_group_obj
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE ham_auth_group THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "auth_group_obj":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "The Auth group of detail line " + STRING (btt_auth_detail.line_number) + " is invalid.",
                      INPUT "ERR":U).
      END.  /* IF NOT AVAILABLE ham_auth_group THEN */
      ELSE IF (ham_auth_group.option_code <> 0 AND ham_auth_group.option_code <> goAuthorisation:OptionCode) THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "auth_group_obj":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "The Auth group (" + ham_auth_group.auth_group_code +
                            ") is not valid for option " + STRING(goAuthorisation:OptionCode),
                      INPUT "ERR":U).
      END.  /* IF NOT AVAILABLE ham_auth_group THEN */
      ELSE IF (ham_auth_group.effective_date > btt_auth_detail.start_date)
           OR (ham_auth_group.end_date <> ? AND ham_auth_group.end_date < btt_auth_detail.start_date) THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "auth_group_obj":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "The Auth group (" + ham_auth_group.auth_group_code +
                            ") of detail line " + STRING (btt_auth_detail.line_number) + " is not active.",
                      INPUT "ERR":U).
      END.  /* IF NOT AVAILABLE ham_auth_group THEN */
      ELSE DO:
        /*
          Check to make sure that the auth group of the detail lines matches the auth group of the provider
        */
        IF CAN-FIND(FIRST btt_auth_provider NO-LOCK
                    WHERE btt_auth_provider.auth_obj          = btt_auth_detail.auth_obj
                    AND   btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
                    AND   btt_auth_provider.auth_group_obj   <> 0.00
                    AND   btt_auth_provider.auth_group_obj   <> btt_auth_detail.auth_group_obj) THEN
        DO:
          goErrorObject:addError
                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                        INPUT btt_auth_detail.auth_detail_obj,
                        INPUT "":U,
                        INPUT "auth_group_obj":U,
                        INPUT btt_auth_detail.line_number,
                        INPUT "The Auth group of the detail line does not match the auth group of the respective provider.",
                        INPUT "ERR":U).
        END. /* IF CAN-FIND(FIRST btt_auth_provider NO-LOCK */

        /*
          Only Detail lines belonging to the same Provider may be added to the same Auth Group.
          However, if the same auth group is specified on the different providers, then the different
          provider detail lines may have the same auth group.
        */
        AUTHGRP:
        FOR EACH bbt_auth_detail NO-LOCK
          WHERE bbt_auth_detail.auth_obj           = btt_auth_detail.auth_obj
            AND bbt_auth_detail.auth_provider_obj <> btt_auth_detail.auth_provider_obj
            AND bbt_auth_detail.auth_group_obj     = btt_auth_detail.auth_group_obj:

          FIND bbt_auth_provider NO-LOCK
            WHERE bbt_auth_provider.auth_provider_obj = bbt_auth_detail.auth_provider_obj
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

          IF (bbt_auth_provider.auth_group_obj <> btt_auth_provider.auth_group_obj)
          OR  btt_auth_provider.auth_group_obj =  0 THEN
          DO:
            goErrorObject:addError
                         (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                          INPUT btt_auth_detail.auth_detail_obj,
                          INPUT "":U,
                          INPUT "auth_group_obj":U,
                          INPUT btt_auth_detail.line_number,
                          INPUT "Only Detail lines belonging to the same Provider may be added to the same Auth Group.",
                          INPUT "ERR":U).

            // No need in giving the same error message for all the detail lines
            LEAVE AUTHGRP.
          END.  /* IF (bbt_auth_provider.auth_group_obj <> btt_auth_provider.auth_group_obj) */
        END.  /* FOR EACH bbt_auth_detail NO-LOCK */

        /*
          If detail lines are set up on the Auth Group, we need to validate
          the btt_auth_detail against the valid auth group detail setups.
        */
        ASSIGN cErrorMessage = "".
        IF CAN-FIND(FIRST ham_auth_group_detail NO-LOCK
          WHERE ham_auth_group_detail.auth_group_obj   = btt_auth_detail.auth_group_obj
            AND  ham_auth_group_detail.effective_date <= goAuthorisation:StartDate
            AND (ham_auth_group_detail.end_date        = ?
             OR  ham_auth_group_detail.end_date       >= goAuthorisation:EndDate)
            AND  ham_auth_group_detail.owning_entity_mnemonic <> "prtype":U) THEN
        DO:
          /*
            Get the base rate for tariffs, because it will be used in the auth group validations
          */
          IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
          DO:
            IF NOT AVAILABLE btt_auth_provider THEN
            DO:
              FIND bbt_auth_provider NO-LOCK
                WHERE bbt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
                NO-ERROR.

              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
            END.  /* IF NOT AVAILABLE btt_auth_provider THEN */

            IF btt_auth_detail.override_base_rate <> "":U
            THEN
              ASSIGN cBaseRate = btt_auth_detail.override_base_rate
                     cArsRate  = btt_auth_detail.override_ars_rate.
            ELSE
              ASSIGN cBaseRate = IF btt_auth_provider.override_base_rate <> "":U
                                 THEN btt_auth_provider.override_base_rate
                                 ELSE btt_auth_provider.default_base_rate
                     cArsRate  = IF btt_auth_provider.override_ars_rate <> "":U
                                 THEN btt_auth_provider.override_ars_rate
                                 ELSE btt_auth_provider.default_ars_rate.

            IF cBaseRate = "" THEN
            DO:
              mipEnv:Health:maDoctor:getProviderBaseRates( INPUT  btt_auth_provider.doc_num,
                                                           INPUT  goAuthorisation:MemNum,
                                                           INPUT  goAuthorisation:MemberOptionCode,
                                                           INPUT  btt_auth_provider.start_date,
                                                           OUTPUT cBaseRate,
                                                           OUTPUT cArsRate).
            END.  /* IF cBaseRate = "" THEN */
          END.  /* IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN */

          ASSIGN lFoundGroupDetail = FALSE.

          VALIDGROUP:
          FOR EACH ham_auth_group_detail NO-LOCK
            WHERE  ham_auth_group_detail.auth_group_obj         = btt_auth_detail.auth_group_obj
              AND  ham_auth_group_detail.effective_date        <= goAuthorisation:StartDate
              AND (ham_auth_group_detail.end_date               = ?
               OR  ham_auth_group_detail.end_date              >= goAuthorisation:EndDate)
              AND  ham_auth_group_detail.owning_entity_mnemonic = btt_auth_detail.owning_entity_mnemonic
              AND  ham_auth_group_detail.owning_alt_value       = btt_auth_detail.owning_alt_value:

            ASSIGN lFoundGroupDetail = TRUE
                   lValidGroupDetail = TRUE.

            /*
              For tariffs we need to validate the base rate and ars rate against the auth group detail
            */
            IF ham_auth_group_detail.owning_entity_mnemonic = "htmtl":U /* Tariff */ THEN
            DO:
              FIND FIRST htm_tariff_link NO-LOCK
                WHERE htm_tariff_link.tariff_link_obj = ham_auth_group_detail.owning_obj
                NO-ERROR.

              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

              IF AVAILABLE htm_tariff_link
              AND htm_tariff_link.tariff_link_default <> TRUE THEN
              DO:
                /*
                  Validate the discipline on the auth group detail
                */
                IF  ham_auth_group_detail.pr_type <> ""
                AND ham_auth_group_detail.pr_type <> STRING(btt_auth_provider.pr_type,"999") THEN
                  ASSIGN lValidGroupDetail = FALSE.

                /*
                  Validate the sub-discipline on the auth group detail
                */
                ELSE IF ham_auth_group_detail.sub_pr_type <> 0
                AND ham_auth_group_detail.sub_pr_type <> btt_auth_provider.sub_pr_type THEN
                  ASSIGN lValidGroupDetail = FALSE.

                /*
                  Validate the base rate on the auth group detail
                */
                ELSE IF htm_tariff_link.base_rate <> ""
                AND htm_tariff_link.base_rate <> cBaseRate THEN
                  ASSIGN lValidGroupDetail = FALSE.

                /*
                  Validate the ARS rate on the auth group detail
                */
                ELSE IF htm_tariff_link.ars_rate <> ""
                AND htm_tariff_link.ars_rate <> cARSRate THEN
                  ASSIGN lValidGroupDetail = FALSE.
              END.  /* IF AVAILABLE htm_tariff_link AND htm_tariff_link.tariff_link_default <> FALSE THEN */
            END.  /* IF ham_auth_group_detail.owning_entity_mnemonic = "htmtl":U /* Tariff */ THEN */

            IF lValidGroupDetail THEN
              LEAVE VALIDGROUP.
          END.  /* FOR EACH ham_auth_group_detail NO-LOCK */

          IF NOT lValidGroupDetail
          OR NOT lFoundGroupDetail THEN
          DO:
            ASSIGN cErrorMessage = cEntity + " '" + btt_auth_detail.owning_alt_value + "' is not set up".

            IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
            DO:
              ASSIGN cErrorMessage = cErrorMessage + " for Discipline '" + STRING(btt_auth_provider.pr_type,"999").

              IF cARSRate <> "" THEN
                ASSIGN cErrorMessage = cErrorMessage +
                                      "', Sub-Discipline '" + STRING(btt_auth_provider.sub_pr_type,"999") +
                                      "', Base Rate '" + btt_auth_detail.default_base_rate +
                                      "' and ARS Rate '" + btt_auth_detail.default_ars_rate + "'".
              ELSE IF cBaseRate <> "" THEN
                ASSIGN cErrorMessage = cErrorMessage +
                                      "', Sub-Discipline '" + STRING(btt_auth_provider.sub_pr_type,"999") +
                                      "' and Base Rate '" + btt_auth_detail.default_base_rate + "'".
              ELSE
                ASSIGN cErrorMessage = cErrorMessage +
                                      "' and Sub-Discipline '" + STRING(btt_auth_provider.sub_pr_type,"999") + "'".
            END.  /* IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN */

            ASSIGN cErrorMessage = cErrorMessage + " on Auth Group '" + ham_auth_group.auth_group_code + "'".

          END. /* IF NOT lValidGroupDetail OR NOT lFoundGroupDetail THEN */
        END.  /* IF CAN-FIND(FIRST ham_auth_group_detail NO-LOCK */

        IF cErrorMessage = ""
        AND CAN-FIND(FIRST ham_auth_group_detail NO-LOCK
          WHERE  ham_auth_group_detail.auth_group_obj  = btt_auth_detail.auth_group_obj
            AND  ham_auth_group_detail.effective_date <= goAuthorisation:StartDate
            AND (ham_auth_group_detail.end_date        = ?
             OR  ham_auth_group_detail.end_date       >= goAuthorisation:EndDate)
            AND  ham_auth_group_detail.owning_entity_mnemonic = "prtype":U) THEN
        DO:
          IF NOT CAN-FIND(FIRST ham_auth_group_detail NO-LOCK
              WHERE  ham_auth_group_detail.auth_group_obj         = btt_auth_detail.auth_group_obj
                AND  ham_auth_group_detail.effective_date        <= goAuthorisation:StartDate
                AND (ham_auth_group_detail.end_date               = ?
                 OR  ham_auth_group_detail.end_date              >= goAuthorisation:EndDate)
                AND  ham_auth_group_detail.owning_entity_mnemonic = "prtype":U
                AND (ham_auth_group_detail.pr_type                = "000"
                 OR  ham_auth_group_detail.pr_type                = STRING(btt_auth_provider.pr_type,"999"))
                AND (ham_auth_group_detail.sub_pr_type            = 0
                 OR  ham_auth_group_detail.sub_pr_type            = btt_auth_provider.sub_pr_type)) THEN
            ASSIGN
              cErrorMessage = SUBSTITUTE("&1 '&2' is not set up for Discipline/Sub-Discipline [&3/&4] on Auth Group '&5'",
                                         cEntity,btt_auth_detail.owning_alt_value,STRING(btt_auth_provider.pr_type,"999"),
                                         STRING(btt_auth_provider.sub_pr_type,"999"),ham_auth_group.auth_group_code).
        END.  /* IF cErrorMessage = "" AND... */

        IF cErrorMessage <> "" THEN
        DO:
          goErrorObject:addError
                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                        INPUT btt_auth_detail.auth_detail_obj,
                        INPUT "":U,
                        INPUT "owning_obj":U,
                        INPUT btt_auth_detail.line_number,
                        INPUT cErrorMessage,
                        INPUT "ERR":U).
        END.  /* IF cErrorMessage <> "" THEN */
      END.  /* ELSE - IF NOT AVAILABLE ham_auth_group THEN */
    END.  /* ELSE - IF btt_auth_detail.quantity_los > 0 THEN */
  END.  /* IF btt_auth_detail.auth_group_obj <> 0.00 THEN */

  /*
    Discount-Type and Discount-Auth
  */
  IF  btt_auth_detail.record_action  = "MODIFY":U THEN
  DO:
    ASSIGN cErrorMessage = "".

    IF btt_auth_detail.discount_type <> ?  THEN
    DO:
      IF (btt_auth_detail.amount_auth <> 0
      OR  btt_auth_detail.quantity_auth <> 0)
      AND btt_auth_detail.discount_auth <> 0 THEN
      DO:
        ASSIGN lCriticalAuthRule = FALSE
               cErrorMessage = "The Auth Rule has not been set up or does not have 'Detail' information set up. ":U
                             + "Please check this rule set up before continuing.":U
                             + "[HELP=Auth Rule Code: AllowDiscountOnLimited]".

        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                       INPUT  "AllowDiscountOnLimited":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).

        IF lValidRule AND (INDEX(cRuleValue, "Detail=") > 0) THEN
          ASSIGN lDetailSetupPresent = TRUE
                 lCriticalAuthRule   = TRUE.
      END. /* IF (btt_auth_detail.amount_auth <> 0 OR btt_auth_detail.quantity_auth <> 0) AND btt_auth_detail.discount_auth <> 0 THEN */

      /* Below we have a reusable critical rules evaluation section */
      IF NOT lCriticalAuthRule
      THEN
        ASSIGN
          lSuccess =  goErrorObject:addError
                                   (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                    INPUT btt_auth.auth_obj,
                                    INPUT "":U,
                                    INPUT "discount_auth":U,
                                    INPUT btt_auth.line_number,
                                    INPUT cErrorMessage ,
                                    INPUT "ERR":U).
      ELSE DO:
        /*
          Discount percentage validation
        */
        IF btt_auth_detail.discount_type = TRUE
        THEN DO:
          ASSIGN cDiscountType = " percentage ".

          IF lDetailSetupPresent THEN
          DO:
            ASSIGN cRuleValue = REPLACE(ENTRY(1,SUBSTRING(cRuleValue,INDEX(cRuleValue,"Detail=")),"|"),"Detail=", "").

            IF NOT LOOKUP (TRIM(cDiscountType),cRuleValue) > 0
	          THEN
              ASSIGN
                cErrorMessage = "Please note that the rule does not support discount type of" + cDiscountType + "selection. The rule setup for detail lines only supports " + cRuleValue
                              + ".[HELP=Auth Rule Code: AllowDiscountOnLimited]":U
                lCriticalAuthRule = FALSE
                lSuccess      =  goErrorObject:addError
                                              (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                               INPUT btt_auth_detail.auth_detail_obj,
                                               INPUT "":U,
                                               INPUT "discount_auth":U,
                                               INPUT btt_auth_detail.line_number,
                                               INPUT cErrorMessage,
                                               INPUT "ERR":U).
          END. /* IF lDetailSetupPresent THEN */

          IF  btt_auth_detail.discount_auth > 100 AND lCriticalAuthRule
	        THEN
            ASSIGN
              cErrorMessage = "Detail line discount percentage value cannot be greater than 100, amount will be reverted to previous value if available."
              lSuccess      = goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                                     INPUT btt_auth_detail.auth_detail_obj,
                                                     INPUT "":U,
                                                     INPUT "discount_auth":U,
                                                     INPUT btt_auth_detail.line_number,
                                                     INPUT cErrorMessage,
                                                     INPUT "ERR":U).
        END. /* IF btt_auth_detail.discount_type = TRUE */

        /*
          Discount amount (Rand) validation
        */
        IF  btt_auth_detail.discount_type = FALSE THEN
        DO:
          ASSIGN cDiscountType = " amount ".

          IF lDetailSetupPresent THEN
          DO:
            ASSIGN cRuleValue = REPLACE(ENTRY(1,SUBSTRING(cRuleValue,INDEX(cRuleValue,"Detail=")),"|"),"Detail=", "").
            IF NOT LOOKUP (TRIM(cDiscountType),cRuleValue) > 0 THEN
              ASSIGN
                cErrorMessage = "Please note that the rule does not support discount type of" + cDiscountType
                	      + "selection. The rule setup for detail lines only supports " + cRuleValue
                              + ".[HELP=Auth Rule Code: AllowDiscountOnLimited]":U
                lCriticalAuthRule = FALSE
                lSuccess      =  goErrorObject:addError
                                              (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                               INPUT btt_auth_detail.auth_detail_obj,
                                               INPUT "":U,
                                               INPUT "discount_auth":U,
                                               INPUT btt_auth_detail.line_number,
                                               INPUT cErrorMessage,
                                               INPUT "ERR":U).
          END. /* IF lDetailSetupPresent THEN */

          IF  btt_auth_detail.discount_auth > btt_auth_detail.amount_auth
          AND btt_auth_detail.amount_auth   > 0 AND lCriticalAuthRule
          THEN
            ASSIGN
              cErrorMessage = "The detail line discount amount can not be greater than the authorised amount."	   // No need to add Authrule code here, msg not directly about Rule setup
              lSuccess      = goErrorObject:addError
                                           (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,                   /* ipcOwningEntityMnemonic */
                                            INPUT btt_auth_detail.auth_detail_obj,                                       /* ipdOwningEntityObj      */
                                            INPUT "":U,                                                                  /* ipcOwningEntityKey      */
                                            INPUT "discount_auth":U,                                                     /* ipcFieldName            */
                                            INPUT btt_auth_detail.line_number,                                           /* ipiLineNumber           */
                                            INPUT cErrorMessage,                                                         /* ipcMessageGroup         */
                                            INPUT  "ERR":U).
        END. /* IF btt_auth_detail.discount_type = FALSE */
      END. /* Else Do: */

      IF btt_auth_detail.discount_auth <= 0 AND lCriticalAuthRule
      THEN
        ASSIGN
          cErrorMessage = "The detail line discount" + cDiscountType
          		+ "cannot be less than or equal to 0, amount will be reverted to previous value if available."	  // No need to add Authrule code here, msg not directly about Rule setup

          lSuccess = goErrorObject:addError
                                  (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,                     /* ipcOwningEntityMnemonic */
                                   INPUT btt_auth_detail.auth_detail_obj,                                       /* ipdOwningEntityObj      */
                                   INPUT "":U,                                                                  /* ipcOwningEntityKey      */
                                   INPUT "discount_auth":U,                                                     /* ipcFieldName            */
                                   INPUT btt_auth_detail.line_number,                                           /* ipiLineNumber           */
                                   INPUT cErrorMessage,                                                         /* ipcMessageGroup         */
                                   INPUT "ERR":U).

      /*
        Check rule to see whether we should return an error or a warning.
      */
      IF  btt_auth_detail.amount_auth   = 0
      AND btt_auth_detail.quantity_auth = 0
      AND btt_auth_detail.discount_auth > 0
      AND lCriticalAuthRule THEN
      DO:
        ASSIGN cErrorMessage = "A detail line" + cDiscountType.

        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                       INPUT  goAuthorisation:OptionCode,
                                                       INPUT  "ma_acAuthRuleTypeFinancials":U,
                                                       INPUT  "AllowDiscountOnUnlimited":U,
                                                       INPUT  btt_auth_detail.start_date,
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
                          + "[HELP=Auth Rule Code: AllowDiscountOnUnlimited, AllowDiscountOnLimited]"
            cMessageType  = "WAR":U.
        ELSE
          ASSIGN
            cErrorMessage = cErrorMessage + "may not be specified on an Unlimited Authorisation (Authorised Amount & Quantity is zero)"
                          + "[HELP=Auth Rule Code: AllowDiscountOnUnlimited, AllowDiscountOnLimited]"
            cMessageType  = "ERR":U.

        ASSIGN
          lSuccess = goErrorObject:addError
                                  (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                   INPUT btt_auth_detail.auth_detail_obj,
                                   INPUT "":U,
                                   INPUT "discount_auth":U,
                                   INPUT btt_auth_detail.line_number,
                                   INPUT cErrorMessage,
                                   INPUT cMessageType).
      END. /* IF btt_auth_detail.amount_auth = 0 and  btt_auth_detail.amount_auth = 0 AND btt_auth_detail.discount_auth > 0 */
    END. /* btt_auth_detail.discount_type <> ? THEN */

    /* Co-payment override reason is not allowed if no co-payment is applied */
    IF  ((AVAILABLE buf_auth_detail AND buf_auth_detail.copay_override_note = "") OR NOT AVAILABLE buf_auth_detail)
    AND btt_auth_detail.copay_override_note <> "":U
    AND btt_auth_detail.copay_auth           = 0
    AND btt_auth_detail.copay_auth_%         = 0
    THEN
      ASSIGN
        cErrorMessage = "The Co-payment override reason can not be completed if no co-payment applies.":U
        lSuccess      = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT "copay_override_note":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT cErrorMessage,
                                      INPUT "ERR":U).
  END. /* btt_auth_detail.record_action  = "MODIFY":U THEN */

  IF   btt_auth_detail.record_action  = "MODIFY":U
  AND  btt_auth_detail.discount_type  = ?
  AND (btt_auth_detail.discount_auth <> 0.00 AND btt_auth_detail.discount_auth <> ?)
  AND lCriticalAuthRule
    THEN
      ASSIGN
        cErrorMessage = "There is a discount authorised value entered but no discount type. Please enter a discount type of either percent or Rand or remove the discount authorised value":U
                      + "[HELP=Auth Rule Code: AllowDiscountOnLimited]":U
        lSuccess      = goErrorObject:addError
                                     (INPUT "hatad":U,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT cErrorMessage,
                                      INPUT "ERR":U).

  /*
    Validate Start- and End AMPM if ActivateAmPm is set
  */
  IF AVAILABLE ttAuthTypeConfig
  AND ttAuthTypeConfig.ActivateAmPm THEN
  DO:
    IF  btt_auth_detail.start_ampm  = ?
    OR (btt_auth_detail.start_ampm <> ?
    AND LOOKUP(STRING(btt_auth_detail.start_ampm,"AM/PM":U),"AM,PM":U) = 0) THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT "start_ampm":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT "MA":U,
                                      INPUT 100,  /* The &1 specified is invalid */
                                      INPUT "Authorisation Detail Start AMPM ":U).
    END. /*IF (glActiveAmpm AND btt_auth_detail.start_ampm = ?) OR (btt_auth_detail.start_ampm <> ? AND LOOKUP(STRING(btt_auth_detail.start_ampm),"AM,PM") = 0) THEN*/

    IF (btt_auth_detail.end_ampm = ? AND btt_auth_detail.end_date <> ?)
    OR (btt_auth_detail.end_ampm <> ?
    AND LOOKUP(STRING(btt_auth_detail.end_ampm,"AM/PM":U),"AM,PM":U) = 0) THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT "end_ampm":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT "MA":U,
                                      INPUT 100,  /* The &1 specified is invalid */
                                      INPUT "Authorisation Detail End AMPM":U ).
    END. /*IF ((btt_auth_detail.end_ampm = ? AND btt_auth_detail.end_date <> ?) OR ...*/
  END.  /*IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.ActivateAmPm THEN*/

  /*
    Validate note if specified
  */
  IF btt_auth_detail.note <> "":U THEN
  DO:
    FIND FIRST note NO-LOCK
      WHERE note.scheme-code = goAuthorisation:OptionCode
        AND note.TYPE        = "AN":U
        AND note.KEY         = btt_auth_detail.note
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE note
    THEN
      FIND FIRST note NO-LOCK
        WHERE note.TYPE = "AN":U
          AND note.KEY  = btt_auth_detail.note
        NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE note THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,      /* ipcOwningEntityMnemonic */
                                      INPUT btt_auth_detail.auth_detail_obj,                        /* ipdOwningEntityObj      */
                                      INPUT "":U,                                                   /* ipcOwningEntityKey      */
                                      INPUT "note":U,                                               /* ipcFieldName            */
                                      INPUT btt_auth_detail.line_number,                            /* ipiLineNumber           */
                                      INPUT "MA":U,                                                 /* ipcMessageGroup         */
                                      INPUT 100,  /* The '&1' specified is invalid */               /* ipiMessageNumber        */
                                      INPUT "The Note Code: ":U + STRING(btt_auth_detail.note)).    /* ipcReplaceTextList      */
    END. /* IF cErrorMessage <> "":U THEN  */
  END.  /* IF btt_auth_detail.note <> "":U THEN */

  /* Validate body region - must be same as Auth's body region.  If 'both' on Auth header, any value allowed on detail
     The validation against the Acronym is done on Data level */
  IF btt_auth_detail.body_region <> "":U AND btt_auth_detail.record_action <> "DELETE":U THEN
  DO:
    IF  btt_auth_detail.body_region <> goAuthorisation:BodyRegion
    AND goAuthorisation:BodyRegion  <> "" THEN
    DO:
      mipEnv:Health:AuthService:validateBodyRegion(INPUT goAuthorisation:InsurerObj,
                                                   INPUT goAuthorisation:MemberOptionCode,
                                                   INPUT goAuthorisation:BodyRegion,
                                                   INPUT goAuthorisation:StartDate,
                                                   INPUT btt_auth_detail.body_region,
                                                   OUTPUT lValidationError,
                                                   OUTPUT cErrorMessage).

      IF NOT lValidationError AND cErrorMessage <> ""
      THEN
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "body_region":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT cErrorMessage,
                      INPUT "ERR":U).
    END.  /* IF  btt_auth_detail.body-region <> goAuthorisation:BodyRegion */
  END.  /* IF btt_auth_detail.body_region <> "":U THEN */

  /*
    Repeatitem - only allowed for Tariff/Nappi details
  */
  IF btt_auth_detail.repeat_item
  AND LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmnl":U) = 0 THEN
  DO:
    ASSIGN lSuccess = goErrorObject:addError
                                   (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */
                                    INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */
                                    INPUT "":U,                                                    /* ipcOwningEntityKey      */
                                    INPUT "repeat_item":U,                                         /* ipcFieldName            */
                                    INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */
                                    INPUT "MA":U,                                                  /* ipcMessageGroup         */
                                    INPUT 112,  /* The '&1' specified is invalid. &2 */            /* ipiMessageNumber        */
                                    INPUT "repeat_item,NOT allowed for this entity":U).            /* ipcReplaceTextList      */
  END.  /* IF btt_auth_detail.repeat_item AND LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmnl") = 0 THEN */

  /*
    Auth is Completed
  */
  IF NOT btt_auth.auth_incomplete THEN
  DO:
    /*
      Amount/Quantity paid validations
    */
    IF btt_auth_detail.amount_paid   <> 0.00
    OR btt_auth_detail.quantity_paid <> 0 THEN
    DO:
      IF AVAILABLE buf_auth_detail THEN
      DO:
        /* Claim Code */
        IF buf_auth_detail.claim_code <> btt_auth_detail.claim_code THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                          INPUT "claim_code":U,                                                /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                          INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                          INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                                          INPUT "Claim Code: ":U + STRING(btt_auth_detail.claim_code) +        /* ipcReplaceTextList      */
                                                ",No changes allowed after detail line was paid.":U).
        END.  /* IF buf_auth_detail.claim_code <> btt_auth_detail.claim_code THEN */

        /* Claim Type */
        IF buf_auth_detail.claim_type <> btt_auth_detail.claim_type THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                          INPUT "claim_type":U,                                                /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                          INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                          INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                                          INPUT "Claim Type: ":U + STRING(btt_auth_detail.claim_type) +        /* ipcReplaceTextList      */
                                                ",No changes allowed after detail line was paid.":U).
        END.  /* IF buf_auth_detail.claim_type <> btt_auth_detail.claim_type THEN */

        /* Benefit% */
        IF buf_auth_detail.benefit_% <> btt_auth_detail.benefit_% THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                          INPUT "benefit_%":U,                                                 /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                          INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                          INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                                          INPUT "Benefit%: ":U + STRING(btt_auth_detail.benefit_%) +           /* ipcReplaceTextList      */
                                                ",No changes allowed after detail line was paid.":U).
        END.  /* IF buf_auth_detail.benefit_% <> btt_auth_detail.benefit_% THEN */

        /* PMB PAY Cost */
        IF buf_auth_detail.pmb_pay_cost <> btt_auth_detail.pmb_pay_cost
        THEN
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,                /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                                  /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                             /* ipcOwningEntityKey      */
                                          INPUT "pmb_pay_cost":U,                                                 /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                      /* ipiLineNumber           */
                                          INPUT "MA":U,                                                           /* ipcMessageGroup         */
                                          INPUT 112,  /* The '&1' specified is invalid.  &2 */                    /* ipiMessageNumber        */
                                          INPUT "PMB Pay Cost,No changes allowed after detail line was paid.":U). /* ipcReplaceTextList      */
        /* PMB Benefit% */
        IF buf_auth_detail.pmb_benefit_% <> btt_auth_detail.pmb_benefit_% THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                          INPUT "pmb_benefit_%":U,                                             /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                          INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                          INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                                          INPUT "PMB Benefit%: ":U + STRING(btt_auth_detail.pmb_benefit_%) +   /* ipcReplaceTextList      */
                                                ",No changes allowed after detail line was paid.":U).
        END.  /* IF buf_auth_detail.pmb_benefit_% <> btt_auth_detail.pmb_benefit_% THEN */

        /* PMB Indicator */
        IF   btt_auth_detail.pmb_indicator <> buf_auth_detail.pmb_indicator
		    AND (btt_auth_detail.amount_paid <> 0
        OR   btt_auth_detail.quantity_paid <> 0) THEN
        DO:
          IF LOOKUP("ma_member_auth_pmbind_override",gscUserRole) > 0
          THEN
            ASSIGN
              cErrorMessage = "Please note that you are overriding the PMB Indicator validation for this detail line"
              lSuccess = goErrorObject:addError(INPUT "hatad":U,
                                                INPUT btt_auth_detail.auth_detail_obj,
                                                INPUT "":U,
                                                INPUT "":U,
                                                INPUT btt_auth_detail.line_number,
                                                INPUT cErrorMessage,
                                                INPUT "WARNACK":U,
                                                INPUT TRUE).
          ELSE
            ASSIGN cErrorMessage = "PMB Indicator can't be updated, claims must be reversed before PMB Indicator can be updated."
                   lSuccess      = goErrorObject:addError
                            	                  (INPUT "hatad":U,                       /* ipcOwningEntityMnemonic */
                            	                   INPUT btt_auth_detail.auth_detail_obj, /* ipdOwningEntityObj      */
                            	                   INPUT "":U,                            /* ipcOwningEntityKey      */
                            	                   INPUT "pmb_indicator":U,               /* ipcFieldName            */
                            	                   INPUT btt_auth_detail.line_number,     /* ipiLineNumber           */
                            	                   INPUT cErrorMessage,                   /* ipcMessageText          */
                            	                   INPUT "ERR":U).                        /* ipiMessageType          */
        END. /* IF   btt_auth_detail.pmb_indicator <> buf_auth_detail.pmb_indicator THEN */

        /* Minutes Auth */
        IF btt_auth_detail.minutes_auth < buf_auth_detail.minutes_auth THEN
        DO:
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,                /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                                  /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                             /* ipcOwningEntityKey      */
                                          INPUT "minutes_auth":U,                                                 /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                      /* ipiLineNumber           */
                                          INPUT "MA":U,                                                           /* ipcMessageGroup         */
                                          INPUT 112,  /* The '&1' specified is invalid.  &2 */                    /* ipiMessageNumber        */
                                          INPUT "Minutes Authorised: ":U + STRING(btt_auth_detail.minutes_auth) + /* ipcReplaceTextList      */
                                                ",Can't be less than what was authorised because claims are already paid.":U).
        END.  /* IF buf_auth_detail.minutes_auth <> btt_auth_detail.minutes_auth THEN */

        /*
          Check whether any claims have been paid before allowing Discount changes
        */
        IF  (btt_auth_detail.discount_auth <> buf_auth_detail.discount_auth
        OR   btt_auth_detail.discount_type <> buf_auth_detail.discount_type)
        AND (btt_auth_detail.discount_auth <> 0.00
        OR   btt_auth_detail.discount_auth <> ?)
        AND  btt_auth_detail.discount_type <> ?
        THEN
          ASSIGN
            cErrorMessage = "The Discount Type/Amount can not be updated as Claims have already been paid against this Detail line"
            lSuccess      =  goErrorObject:addError
                                          (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                           INPUT btt_auth_detail.auth_detail_obj,
                                           INPUT "":U,
                                           INPUT IF btt_auth_detail.discount_auth <> buf_auth_detail.discount_auth
                                                 THEN "discount_auth":U
                                                 ELSE "discount_type":U,
                                           INPUT btt_auth_detail.line_number,
                                           INPUT cErrorMessage,
                                           INPUT "ERR":U).
      END. /* IF AVAILABLE buf_auth_detail THEN */

      /* Amount auth/paid validation */
      IF  btt_auth_detail.amount_paid <> 0.00
      AND btt_auth_detail.amount_auth <> 0.00
      AND btt_auth_detail.amount_auth < btt_auth_detail.amount_paid THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                        INPUT btt_auth_detail.auth_detail_obj,
                                        INPUT "":U,
                                        INPUT "amount_paid":U,
                                        INPUT btt_auth_detail.line_number,
                                        INPUT "ma_MsgAuth":U,
                                        INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                                        INPUT "Amount Paid," + STRING(btt_auth_detail.amount_paid) + ",Amount Authorised,":U + STRING(btt_auth_detail.amount_auth)).
      END. /* IF btt_auth_detail.amount_paid <> 0.00 AND btt_auth_detail.amount_auth < btt_auth_detail.amount_paid THEN */

      /* Quantity auth/paid validation */
      IF  btt_auth_detail.quantity_paid <> 0.00
      AND btt_auth_detail.quantity_auth <> 0.00
      AND btt_auth_detail.quantity_auth < btt_auth_detail.quantity_paid THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "quantity_paid":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "ma_MsgAuth":U,
                      INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                      INPUT "Quantity Paid," + STRING(btt_auth_detail.quantity_paid) + ",Quantity Authorised,":U + STRING(btt_auth_detail.quantity_auth)).
      END. /*IF btt_auth_detail.quantity_paid <> 0.00 AND btt_auth_detail.quantity_auth < btt_auth_detail.quantity_paid THEN */

      /* Discount auth/paid validation */
      IF  btt_auth_detail.discount_paid <> 0.00
      AND btt_auth_detail.discount_auth < btt_auth_detail.discount_paid THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "discount_paid":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "ma_MsgAuth":U,
                      INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                      INPUT "Discount Paid," + STRING(btt_auth_detail.discount_paid) + ",Discount Authorised,":U + STRING(btt_auth_detail.discount_auth)).
      END. /* IF btt_auth_detail.discount_paid <> 0.00 AND btt_auth_detail.discount_auth < btt_auth_detail.discount_paid THEN */

      /* Co-Pay auth/paid validation */
      IF  btt_auth_detail.copay_paid <> 0.00
      AND btt_auth_detail.copay_auth < btt_auth_detail.copay_paid THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "copay_paid":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "ma_MsgAuth":U,
                      INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                      INPUT "Co-Pay Paid," + STRING(btt_auth_detail.copay_paid) + ",Co-Pay Authorised,":U + STRING(btt_auth_detail.copay_auth)).
      END. /*IF btt_auth_detail.copay_paid <> 0.00 AND btt_auth_detail.copay_auth < btt_auth_detail.copay_paid THEN */

      /* Adjustment auth/paid validation */
      IF  btt_auth_detail.adjustment_paid <> 0.00
      AND btt_auth_detail.adjustment_auth < btt_auth_detail.adjustment_paid THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "adjustment_paid":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "ma_MsgAuth":U,
                      INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                      INPUT "Adjustment Paid," + STRING(btt_auth_detail.adjustment_paid) + ",Adjustment Authorised,":U + STRING(btt_auth_detail.adjustment_auth)).
      END. /* IF btt_auth_detail.adjustment_paid <> 0.00 AND btt_auth_detail.adjustment_auth < btt_auth_detail.adjustment_paid THEN */

      /* Adjustment Private auth/paid validation */
      IF  btt_auth_detail.adjustment_private_paid <> 0.00
      AND btt_auth_detail.adjustment_private_auth < btt_auth_detail.adjustment_private_paid THEN
      DO:
        goErrorObject:addError
                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                      INPUT btt_auth_detail.auth_detail_obj,
                      INPUT "":U,
                      INPUT "adjustment_private_paid":U,
                      INPUT btt_auth_detail.line_number,
                      INPUT "ma_MsgAuth":U,
                      INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                      INPUT "Adjustment Private Paid," + STRING(btt_auth_detail.adjustment_private_paid) + ",Adjustment Private Authorised,":U + STRING(btt_auth_detail.adjustment_private_auth)).
      END.  /* IF btt_auth_detail.adjustment_private_paid <> 0.00 AND btt_auth_detail.adjustment_private_auth < btt_auth_detail.adjustment_private_paid THEN */

      IF btt_auth_detail.record_action = 'DELETE':U THEN
      DO:
        ASSIGN
          cErrorMessage = "The Clinical Detail record cannot be deleted, as claims have already been paid against this detail line"
          lSuccess      = goErrorObject:addError
                                       (INPUT "hatad":U,
                                        INPUT btt_auth_detail.auth_detail_obj,
                                        INPUT "":U,
                                        INPUT btt_auth_detail.line_number,
                                        INPUT cErrorMessage,
                                        INPUT "ERR":U).
      END. /* IF btt_auth_detail.record_action = 'DELETE':U THEN */
    END.  /* IF btt_auth_detail.amount_paid <> 0.00 OR btt_auth_detail.quantity_paid <> 0 THEN */

    /* Co-payment override reason is not allowed to be updated if claims have been paid on the detail line */
    IF  AVAILABLE buf_auth_detail
    AND (btt_auth_detail.copay_override_note <> buf_auth_detail.copay_override_note
    AND (btt_auth_detail.amount_paid         <> 0
    OR   btt_auth_detail.quantity_paid       <> 0))
    THEN
      ASSIGN
        cErrorMessage = "The Co-payment override reason can't be updated as claims have already been paid."
        lSuccess      = goErrorObject:addError
                                       (INPUT 'hatad:':U + btt_auth_detail.owning_entity_mnemonic,
                                        INPUT btt_auth_detail.auth_detail_obj,
                                        INPUT "":U,
                                        INPUT "adjustment_private_paid":U,
                                        INPUT btt_auth_detail.line_number,
                                        INPUT cErrorMessage,
                                        INPUT "ERR":U).
  END. /* IF NOT btt_auth.auth_incomplete */

  FIND buf_auth_detail NO-LOCK
    WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE buf_auth_detail THEN
  DO:
    IF  btt_auth_detail.amount_auth     <> buf_auth_detail.amount_auth
    AND btt_auth_detail.line_restriction = 'ma_acAuthLineRestrictionAmount':U
    AND btt_auth_detail.amount_paid     <> 0
    AND btt_auth_detail.quantity_auth   <> 0
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                        (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                         INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                         INPUT "":U,                                                          /* ipcOwningEntityKey      */
                         INPUT "amount_auth":U,                                               /* ipcFieldName            */
                         INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                         INPUT "MA":U,                                                        /* ipcMessageGroup         */
                         INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                         INPUT "Amount Auth ":U  + STRING(btt_auth_detail.amount_auth) +      /* ipcReplaceTextList      */
                               ",Amount Auth cannot be updated if Line Restriction is 'Amount'and Amount Paid is not zero and the Quantity Auth is not zero.":U).


    IF  btt_auth_detail.amount_auth      <> buf_auth_detail.amount_auth
    AND btt_auth_detail.line_restriction = 'ma_acAuthLineRestrictionUnlimited':U
    AND btt_auth_detail.amount_paid      <> 0
    AND btt_auth_detail.quantity_auth    =  0
    AND btt_auth_detail.amount_auth      =  0
    THEN
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                        INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                        INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                        INPUT "amount_auth":U,                                               /* ipcFieldName            */
                                        INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                        INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                        INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                                        INPUT "Amount Auth ":U + STRING(btt_auth_detail.amount_auth) +       /* ipcReplaceTextList      */
                                              ",Amount Auth cannot be updated if Line Restriction is 'Unlimited' and Amount Paid is not zero and the Quantity Auth is zero and the Amount Authorised is zero.":U).

    IF  btt_auth_detail.quantity_auth   <> buf_auth_detail.quantity_auth
    AND btt_auth_detail.line_restriction = 'ma_acAuthLineRestrictionAmount':U
    AND btt_auth_detail.amount_paid     <> 0
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                      INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                      INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                      INPUT "quantity_auth":U,                                             /* ipcFieldName            */
                                      INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                      INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                      INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                                      INPUT "Quantity Auth ":U + STRING(btt_auth_detail.quantity_auth) +   /* ipcReplaceTextList      */
                                            ",Quantity Auth cannot be updated if Line Restriction is 'Amount' and Amount Paid is not zero.":U ) .

    IF  btt_auth_detail.quantity_auth   <> buf_auth_detail.quantity_auth
    AND btt_auth_detail.line_restriction = 'ma_acAuthLineRestrictionUnlimited':U
    AND btt_auth_detail.quantity_paid   <> 0
    AND btt_auth_detail.quantity_auth    = 0
    AND btt_auth_detail.amount_auth      = 0
    THEN
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */
                                      INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */
                                      INPUT "":U,                                                          /* ipcOwningEntityKey      */
                                      INPUT "quantity_auth":U,                                             /* ipcFieldName            */
                                      INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */
                                      INPUT "MA":U,                                                        /* ipcMessageGroup         */
                                      INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */
                                      INPUT "Quantity Auth ":U + STRING(btt_auth_detail.quantity_auth) +   /* ipcReplaceTextList      */
                                            ",Quantity Auth cannot be updated if Line Restriction is 'Unlimited' and Quantity Paid is not zero, and the Quantity Auth is zero and the Amount Authorised is zero.":U ).

    IF   btt_auth_detail.quantity_los  <> 0
    AND (btt_auth_detail.quantity_paid <> 0
     OR  btt_auth_detail.amount_paid   <> 0)
    AND (btt_auth_detail.quantity_los  <> buf_auth_detail.quantity_los
     OR  btt_auth_detail.start_date    <> buf_auth_detail.start_date
     OR  btt_auth_detail.start_ampm    <> buf_auth_detail.start_ampm
     OR  btt_auth_detail.end_date      <> buf_auth_detail.end_date
     OR  btt_auth_detail.end_ampm      <> buf_auth_detail.end_ampm) THEN
    DO:
      CASE TRUE:
          WHEN btt_auth_detail.quantity_los <> buf_auth_detail.quantity_los THEN ASSIGN cFieldChangedLabel = "Quantity LOS":U
                                                                                        cFieldChanged      = "quantity_los":U.
          WHEN btt_auth_detail.start_date   <> buf_auth_detail.start_date   THEN ASSIGN cFieldChangedLabel = "Start Date":U
                                                                                        cFieldChanged      = "start_date":U.
          WHEN btt_auth_detail.start_ampm   <> buf_auth_detail.start_ampm   THEN ASSIGN cFieldChangedLabel = "Start AMPM":U
                                                                                        cFieldChanged      = "start_ampm":U.
          WHEN btt_auth_detail.end_date     <> buf_auth_detail.end_date     THEN ASSIGN cFieldChangedLabel = "End Date":U
                                                                                        cFieldChanged      = "end_date":U.
          WHEN btt_auth_detail.end_ampm     <> buf_auth_detail.end_ampm     THEN ASSIGN cFieldChangedLabel = "End AMPM":U.
      END CASE. /* CASE TRUE */

      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                                      INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                                      INPUT "":U,                                               /* ipcOwningEntityKey      */
                                      INPUT cFieldChanged,                                      /* ipcFieldName            */
                                      INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                                      INPUT "MA":U,                                             /* ipcMessageGroup         */
                                      INPUT 112,  /* The '&1' specified is invalid.  &2 */      /* ipiMessageNumber        */
                                      INPUT cFieldChangedLabel +                                /* ipcReplaceTextList      */
                                            ",":U + cFieldChangedLabel + " cannot be updated on LOC lines once payments have been made":U ).
    END. /* IF   tt_auth_detail.quantity_los  <> 0 AND (tt_auth_detail.quantity_paid <> 0  OR  tt_auth_detail.amount_paid   <> 0  ... */

    IF   btt_auth_detail.quantity_los  <> 0
    AND  btt_auth_detail.quantity_los   = buf_auth_detail.quantity_los
    AND (btt_auth_detail.quantity_auth <> buf_auth_detail.quantity_auth
     OR  btt_auth_detail.amount_auth   <> buf_auth_detail.amount_auth)
    AND  btt_auth_detail.claim_code     = buf_auth_detail.claim_code THEN
    DO:
      ASSIGN cFieldChangedLabel = "":U
             cFieldChanged      = "":U.

      CASE TRUE :
          WHEN btt_auth_detail.quantity_auth <> buf_auth_detail.quantity_auth THEN ASSIGN cFieldChangedLabel = "Quantity Auth":U
                                                                                          cFieldChanged      = "quantity_auth":U.
          WHEN btt_auth_detail.amount_auth   <> buf_auth_detail.amount_auth   THEN ASSIGN cFieldChangedLabel = IF cFieldChangedLabel <> "":U THEN cFieldChangedLabel + ",Amount Auth":U ELSE "Amount Auth":U
                                                                                          cFieldChanged      = IF cFieldChanged      <> "":U THEN cFieldChanged      + ",amount_auth":U ELSE "amount_auth":U .
      END CASE. /* CASE TRUE */

      IF CAN-DO(cFieldChanged ,"quantity_auth":U )
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */
                                        INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                                        INPUT "":U,                                                /* ipcOwningEntityKey      */
                                        INPUT cFieldChanged,                                       /* ipcFieldName            */
                                        INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                                        INPUT "MA":U,                                              /* ipcMessageGroup         */
                                        INPUT 112,  /* The '&1' specified is invalid.  &2 */       /* ipiMessageNumber        */
                                        INPUT "Quantity Auth" +                                    /* ipcReplaceTextList      */
                                            ",":U + "Quantity Auth cannot be updated on LOC lines.":U ) .

      IF CAN-DO(cFieldChanged ,"amount_auth":U) THEN
      DO:
        mipEnv:health:AuthMaintenance:getauthRuleValue(INPUT  goAuthorisation:InsurerObj ,
                                                       INPUT  goAuthorisation:MemberOptionCode,
                                                       INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                       INPUT  "LineRestriction":U,
                                                       INPUT  goAuthorisation:StartDate,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).

        /*
          The "LineRestriction" Authorisation Rule is setup with value "Rule". Update only allowed when it is "System"
        */
        IF lValidRule AND ENTRY(1,cRuleValue,"|") = "Rule":U
        THEN
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,          /* ipcOwningEntityMnemonic */
                                          INPUT btt_auth_detail.auth_detail_obj,                            /* ipdOwningEntityObj      */
                                          INPUT "":U,                                                       /* ipcOwningEntityKey      */
                                          INPUT cFieldChanged,                                              /* ipcFieldName            */
                                          INPUT btt_auth_detail.line_number,                                /* ipiLineNumber           */
                                          INPUT "MA":U,                                                     /* ipcMessageGroup         */
                                          INPUT 112,  /* The '&1' specified is invalid.  &2 */              /* ipiMessageNumber        */
                                          INPUT "Amount Auth,Amount Auth cannot be updated on LOC lines." + /* ipcReplaceTextList      */
                                                "[HELP=Auth Rule Code: LineRestriction]":U ).

      END.  /* IF CAN-DO(cFieldChanged ,"amount_auth":U) THEN */
    END.  /* IF btt_auth_detail.quantity_los <> 0 AND... */

    /*
      Orthodontic calculation validations:
    */
    /* Check if any dependencies exist for the tariff before a deletion or update of the authorised amount is allowed.                                         */
    /* For Orthodontics the Initial Fee can’t be deleted or the authorised amount can’t be updated if the Monthly Fee exists.                                  */
    /* The Monthly Fee will be linked to the Initial Fee in the hat_auth_detail.related* fields and related_entity_mnemonic = “hatad”.                         */
    /* No link will be done on the Initial Fee detail line.                      		                                                               */
    /* Check won’t pick up the Tariff Modifiers where detail lines are also linked, because the link will be done in the related fields on both detail lines.  */

    IF (btt_auth_detail.record_action		 = "DELETE":U
    OR (btt_auth_detail.record_action		 = "MODIFY":U
    AND AVAILABLE buf_auth_detail
    AND btt_auth_detail.amount_auth 		<> buf_auth_detail.amount_auth))
    AND btt_auth_detail.owning_entity_mnemonic 	 = "htmtl":U
    AND btt_auth_detail.related_entity           = "":U THEN
    DO:
      FIND FIRST bbt_auth_detail NO-LOCK
        WHERE bbt_auth_detail.auth_provider_obj       = btt_auth_detail.auth_provider_obj
          AND bbt_auth_detail.auth_detail_obj        <> btt_auth_detail.auth_detail_obj
          AND bbt_auth_detail.related_entity_mnemonic = "hatad":U
          AND bbt_auth_detail.related_obj             = btt_auth_detail.auth_detail_obj
          AND bbt_auth_detail.related_value           = btt_auth_detail.owning_alt_value
          NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE bbt_auth_detail
      AND btt_auth_detail.record_action = "DELETE":U
      THEN
        ASSIGN cErrorMessage = "Tariff " +  btt_auth_detail.owning_alt_value + " can’t be deleted before Tariff "
        		                 + bbt_auth_detail.owning_alt_value + " is deleted"

               lSuccess      = goErrorObject:addError
                                            (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */
                                             INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                                             INPUT "":U,                                                /* ipcOwningEntityKey      */
                                             INPUT "amount_auth":U,                                     /* ipcFieldName            */
                                             INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                                             INPUT cErrorMessage,                                       /* ipcMessageText          */
                                             INPUT "ERR":U).                                            /* ipcMessageType          */

      IF AVAILABLE bbt_auth_detail
      AND btt_auth_detail.record_action = "MODIFY":U
      AND AVAILABLE buf_auth_detail
      AND btt_auth_detail.amount_auth <> buf_auth_detail.amount_auth
      THEN
        ASSIGN cErrorMessage = "Tariff " +  btt_auth_detail.owning_alt_value + " authorised amount can’t be updated before Tariff "
                             + bbt_auth_detail.owning_alt_value + " is deleted"

               lSuccess      = goErrorObject:addError
                                            (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */
                                             INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                                             INPUT "":U,                                                /* ipcOwningEntityKey      */
                                             INPUT "amount_auth":U,                                     /* ipcFieldName            */
                                             INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                                             INPUT cErrorMessage,                                       /* ipcMessageText          */
                                             INPUT "ERR":U).                                            /* ipcMessageType          */

    END.  /* IF btt_auth_detail.record_action = "DELETE":U... */
  END.  /* IF AVAILABLE buf_auth_detail */

  IF  btt_auth_detail.auth_detail_obj <= 0
  OR (AVAILABLE buf_auth_detail AND buf_auth_detail.quantity_auth <> btt_auth_detail.quantity_auth )
  AND LOOKUP(STRING(btt_auth_detail.auth_status),"5,6":U ) = 0 THEN
  DO:
    {ma/app/maauthbusvaldetailusage.i}
  END. /* IF  btt_auth_detail.auth_detail_obj <= 0 OR (AVAILABLE buf_auth_detail AND buf_au...*/

END.  /* IF goAuthorisation:InFocus AND AVAILABLE btt_auth_detail THEN */

{ mip/inc/mipcatcherror.i
  &FINALLY = "IF VALID-OBJECT(oBodyRegion)       THEN DELETE OBJECT oBodyRegion.
              IF VALID-OBJECT(oAuthDetailLines)  THEN DELETE OBJECT oAuthDetailLines.
              IF VALID-OBJECT(oAuthRule)         THEN DELETE OBJECT oAuthRule.
              IF VALID-OBJECT(oDetail)           THEN DELETE OBJECT oDetail."
}

&ENDIF
