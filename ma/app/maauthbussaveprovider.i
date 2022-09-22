/* maauthbussaveprovider.i  MEDSTAR Medical Aid System
                            Save Authorisation Provider Record
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/

  DEFINE PARAMETER BUFFER btt_auth_provider FOR tt_auth_provider.

  DEFINE VARIABLE dAuthProviderObj    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cRuleCode           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValidValues    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleDescription    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleType           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRuleValue          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dAuthRuleObj        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dLinkAuthRuleObj    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dInsurerObj         AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iOptionCode         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dEndDate            AS DATE        NO-UNDO.
  DEFINE VARIABLE dStartDate          AS DATE        NO-UNDO.
  DEFINE VARIABLE lValidRule          AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lSystemOwned        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cErrorMessage       AS CHARACTER   NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE oAuthProvider       AS cls.maauthorisationprovider   NO-UNDO.
  
  DEFINE BUFFER btt_auth            FOR tt_auth.
  DEFINE BUFFER btt_auth_flag_value FOR tt_auth_flag_value.
  DEFINE BUFFER buf_auth_provider   FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_detail     FOR hat_auth_detail.
  DEFINE BUFFER btt_auth_detail     FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_copay      FOR tt_auth_copay.

  ASSIGN oAuthProvider = NEW cls.maauthorisationprovider().

  IF AVAILABLE btt_auth_provider AND
    CAN-DO("{&ActionList}":U, btt_auth_provider.record_action) AND NOT goErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj, "":U, "ERR":U)
  THEN
  DO:

    FIND FIRST btt_auth 
         WHERE btt_auth.auth_obj = btt_auth_provider.auth_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    ASSIGN lSuccess = oAuthProvider:focusRecord( INPUT btt_auth_provider.auth_provider_obj ).

    mipEnv:Health:AuthService:getAuthTypeConfig
      (BUFFER btt_auth , 
       BUFFER btt_auth_provider,
       INPUT-OUTPUT TABLE ttAuthTypeConfig).


    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    /*
      Retain old auth obj in case this is a new record and we have a dummy obj which will need
      to be replaced on all the child records if this is a batch update.
    */
    ASSIGN dAuthProviderObj = btt_auth_provider.auth_provider_obj.

    /*
      If the authorisation has been ended, ensure we end the provider line as well if the end date is blank
    */
    IF goAuthorisation:EndDate <> ? AND btt_auth_provider.end_date = ?
    THEN
      ASSIGN btt_auth_provider.end_date = goAuthorisation:EndDate
             btt_auth_provider.end_ampm = goAuthorisation:EndAmpm.

    IF AVAILABLE ttAuthTypeConfig AND NOT ttAuthTypeConfig.activateLos
    THEN
      ASSIGN
        btt_auth_provider.default_base_rate = "":U
	      btt_auth_provider.default_ars_rate  = "":U.

    IF AVAILABLE ttAuthTypeConfig AND NOT ttAuthTypeConfig.ActivateAmPm
    THEN
      ASSIGN btt_auth_provider.start_ampm = ?
             btt_auth_provider.end_ampm   = ?.

    IF btt_auth_provider._emergency_flag_updated THEN
    DO:

      ASSIGN cRuleType   = "ma_acAuthRuleTypeAuthFlag":U
             cRuleCode   = "EMERGENCY":U
             dStartDate  = btt_auth_provider.start_date
             dInsurerObj = goAuthorisation:InsurerObj
             iOptionCode = goAuthorisation:MemberOptionCode.

      /*
        Get the auth rule obj
      */
      mipEnv:Health:AuthMaintenance:getAuthRuleDetails( INPUT-OUTPUT dAuthRuleObj,
                                                        INPUT-OUTPUT dInsurerObj,
                                                        INPUT-OUTPUT iOptionCode,
                                                        INPUT-OUTPUT cRuleType,
                                                        INPUT-OUTPUT cRuleCode,
                                                        INPUT-OUTPUT dStartDate,
                                                              OUTPUT lValidRule,
                                                              OUTPUT cRuleValue,
                                                              OUTPUT cRuleValidValues,
                                                              OUTPUT dLinkAuthRuleObj,
                                                              OUTPUT cRuleDescription,
                                                              OUTPUT lSystemOwned,
                                                              OUTPUT dEndDate).

      FIND FIRST btt_auth_flag_value
           WHERE btt_auth_flag_value.owning_obj    = btt_auth_provider.auth_obj
             AND btt_auth_flag_value.auth_rule_obj = dAuthRuleObj NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

      IF NOT AVAILABLE btt_auth_flag_value THEN
      DO:
        CREATE btt_auth_flag_value.
        ASSIGN btt_auth_flag_value.record_action          = 'MODIFY':U
               btt_auth_flag_value.owning_entity_mnemonic = 'hatau':U
               btt_auth_flag_value.owning_obj             = goAuthorisation:AuthObj
               btt_auth_flag_value.owning_key             = "":U
               btt_auth_flag_value.owning_alt_value       = goAuthorisation:AuthNum
               btt_auth_flag_value.auth_rule_obj          = dAuthRuleObj
               btt_auth_flag_value.last_change_datetime   = NOW
               btt_auth_flag_value.auth_flag_value        = btt_auth_provider._emergency_flag.

        VALIDATE btt_auth_flag_value.
      END. /* IF NOT AVAILABLE tt_auth_flag_value THEN */
      ELSE IF btt_auth_flag_value.auth_flag_value <> btt_auth_provider._emergency_flag THEN
      DO:
        ASSIGN btt_auth_flag_value.record_action         = 'MODIFY':U
               btt_auth_flag_value.last_change_datetime  = NOW
               btt_auth_flag_value.auth_flag_value       = btt_auth_provider._emergency_flag.

        VALIDATE btt_auth_flag_value.
      END. /* IF btt_auth_flag_value.auth_flag_value <> btt_auth_provider._emergency_flag:U */

      /*
         Set record action for detail lines to modify because the Emergency Flag
         may affect co-payment processing on the Clinical Detail Lines.
         Record action of all records that are not "MODIFY" should be set to
         "MODIFY" to reapply the co-payment processing.
      */
      FOR EACH btt_auth_detail
        WHERE btt_auth_detail.auth_obj      = btt_auth_provider.auth_obj
        AND   btt_auth_detail.record_action = "":
        ASSIGN btt_auth_detail.record_action = "MODIFY":U.
        VALIDATE btt_auth_detail.
      END.  /* FOR EACH btt_auth_detail */

    END. // IF tt_auth_provider._emergency_flag_updated THEN

    IF btt_auth_provider.auth_provider_obj <= 0.00 THEN
    DO:
      ASSIGN btt_auth_provider.pmb_indicator = goAuthorisation:PMBIndicator.

      IF AVAILABLE ttAuthTypeConfig AND NUM-ENTRIES(ttAuthTypeConfig.ClaimCodes) = 1
      THEN
        ASSIGN btt_auth_provider.default_claim_code = INTEGER(ttAuthTypeConfig.ClaimCodes).
      ELSE
        ASSIGN btt_auth_provider.default_claim_code = goAuthorisation:DefaultClaimCode.

      IF AVAILABLE ttAuthTypeConfig AND NUM-ENTRIES(ttAuthTypeConfig.ClaimTypes) = 1
      THEN
        ASSIGN btt_auth_provider.default_claim_type = ttAuthTypeConfig.ClaimTypes.
      ELSE
        ASSIGN btt_auth_provider.default_claim_type = goAuthorisation:DefaultClaimType.

      VALIDATE btt_auth_provider.
    END. /*IF btt_auth_provider.auth_provider_obj <= 0.00 THEN*/

    /*
      MMP-289, MMP-445
      Inherit base and ars rate from Authorisation when the provider record is being created.
      Populate the pmb indicator, base and ars rate on the temp table record before the record is saved.
      If the record is being updated and the pmb indicator, base or ars rate has changed, then we should
      run through all detail lines for the provider record and update it.
    */

    IF oAuthProvider:Infocus THEN
    DO:
      /*
        If the auth group has changed we make sure all the auth detail records have the
        same auth group obj as the providers
      */

      FOR EACH btt_auth_detail EXCLUSIVE-LOCK
           WHERE btt_auth_detail.auth_obj           = btt_auth_provider.auth_obj
             AND btt_auth_detail.auth_provider_obj  = btt_auth_provider.auth_provider_obj
             AND btt_auth_detail.quantity_los       < 1 :
        /* MMP-704 */
        IF btt_auth_provider.auth_group_obj <> oAuthProvider:AuthGroupObj THEN
        DO:
            ASSIGN btt_auth_detail.auth_group_obj = btt_auth_provider.auth_group_obj
                   btt_auth_detail.record_action  = "MODIFY":U .

            VALIDATE btt_auth_detail.
        END. /* IF btt_auth_provider.main_provider THEN */

        IF  btt_auth_provider.pmb_indicator <> oAuthProvider:PmbIndicator THEN
        DO:
		      IF (btt_auth_detail.amount_paid   <> 0
          OR  btt_auth_detail.quantity_paid <> 0) THEN
          DO:
            IF LOOKUP("ma_member_auth_pmbind_override",gscUserRole) > 0 THEN
            DO:
              ASSIGN
                cErrorMessage = "Please note that you are overriding the PMB Indicator validation for this provider."
                lSuccess      = goErrorObject:addError(INPUT "hatad":U,
                                                       INPUT btt_auth_detail.auth_obj,    /* ipdOwningEntityObj */
                                                       INPUT "":U,                        /* ipcOwningEntityKey */
                                                       INPUT "pmb_indicator":U,           /* ipcFieldName       */
                                                       INPUT btt_auth_detail.line_number, /* ipiLineNumber      */
                                                       INPUT cErrorMessage,               /* ipcMessageText     */
                                                       INPUT "WARNACK":U,                 /* ipcErrType         */
                                                       INPUT TRUE).                       /* iplAcknowledge     */

		          ASSIGN btt_auth_detail.pmb_indicator = btt_auth_provider.pmb_indicator
                     btt_auth_detail.record_action = "MODIFY":U.

            END. /* IF LOOKUP(ma_member_auth_pmbind_override,gscUserRole) > 0 */
            ELSE DO:
              ASSIGN
                cErrorMessage = "Claims have been paid against this detail line."
			                  + "The claims must be reversed before PMB Indicator can be updated on the Detail record."
                lSuccess      = goErrorObject:addError(INPUT "hatad":U,
                                                       INPUT btt_auth_detail.auth_obj,
                                                       INPUT "":U,
                                                       INPUT "pmb_indicator":U,
                                                       INPUT btt_auth_detail.line_number,
                                                       INPUT cErrorMessage,
                                                       INPUT "WARNACK":U,
			  										 INPUT TRUE).

            END. /* ELSE DO: IF LOOKUP("ma_member_auth_pmbind_override",gscUserRole) > 0 */
          END. /* IF (btt_auth_detail.amount_paid   > 0 OR ... */
        END. /* IF btt_auth_provider.pmb_indicator <> oAuthProvider:PmbIndicator THEN */
      END. /* FOR EACH btt_auth_detail EXCLUSIVE-LOCK */
    END. /* IF  oAuthProvider:Infocus THEN */

    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthProvider(BUFFER btt_auth_provider, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).

    /*
      No point in continuing if an error occurred when the record was passed to the data access layer to be saved or deleted
    */
    IF goErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj, "":U, "ERR":U)
    THEN LEAVE.

    /*
      MMP-1007
      The existing saveAuthFlagValue must be moved so we can save the 3 flags EMERGENCY, PENALTY and LATEAUTH at once.
      Move saveAuthFlagValue after saveAuthProvider is run if no errors are returned from saveAuthProvider
      Only run saveAuthFlagValue if a record exists in tt_auth_flag_value where the record action <> "".
      Handle any errors that may be returned from saveAuthFlagValue
    */
    IF CAN-FIND(FIRST btt_auth_flag_value
           WHERE btt_auth_flag_value.owning_entity_mnemonic = "hatau":U
           AND   btt_auth_flag_value.owning_obj             = btt_auth_provider.auth_obj
           AND   btt_auth_flag_value.record_action         <> "" )

    THEN DO:

      EMPTY TEMP-TABLE tt_auth_flag_Value_error.

      mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).

      IF CAN-FIND(FIRST tt_auth_flag_value_error NO-LOCK)
      THEN
        TEMP-TABLE tt_auth_error:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_flag_value_error:HANDLE,TRUE).

      IF goErrorObject:CanFind("hatap":U, btt_auth_provider.auth_obj, "":U, "ERR":U)
      OR CAN-FIND(FIRST tt_auth_error
         WHERE tt_auth_error.owning_entity_mnemonic = "hataf":U
	         AND tt_auth_error.error_type             = "ERR":U)
      THEN LEAVE.
    END. // IF AVAILABLE btt_auth_flag_value...

    /*
      Update the copay records linked to the provider
    */
    FOR EACH btt_auth_copay
      WHERE btt_auth_copay.auth_obj               = btt_auth_provider.auth_obj
      AND   btt_auth_copay.owning_entity_mnemonic = "hatap":U
      AND   btt_auth_copay.owning_obj             = dAuthProviderObj:

      ASSIGN btt_auth_copay.owning_obj = btt_auth_provider.auth_provider_obj.
      VALIDATE btt_auth_copay.

    END. // IF AVAILABLE tt_auth_flag_value...

    /*
      Populate detail records auth obj
    */
    FOR EACH tt_auth_detail EXCLUSIVE-LOCK
       WHERE tt_auth_detail.auth_provider_obj = dAuthProviderObj:

      ASSIGN tt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj.

      VALIDATE tt_auth_detail.
    END. /*FOR EACH tt_auth_detail EXCLUSIVE-LOCK */

    /*
      Automatic creation of detail lines from Auth Type Control setup
    */
    IF dAuthProviderObj <= 0.00 THEN
    DO:
      {ma/app/maauthbussaveprovdetcreate.i}
    END. /* IF dAuthProviderObj <= 0.00 */

    /*
      Clear record action
    */
    ASSIGN btt_auth_provider.record_action = "":U.

    VALIDATE btt_auth_provider.

  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_provider.record_action) THEN*/

  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oAuthProvider ) THEN DELETE OBJECT(oAuthProvider)."}
&ENDIF



