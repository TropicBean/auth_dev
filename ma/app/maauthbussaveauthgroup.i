/* maauthbussaveauthgroup.i MEDSTAR Medical Aid System
                            Automatically add or delete a Clinical Detail Line
                            that belongs to a Provider Authorisation Group
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/
DEFINE BUFFER ctt_auth_provider FOR tt_auth_provider.
DEFINE BUFFER ctt_auth_detail   FOR tt_auth_detail.

DEFINE VARIABLE dOwningObj AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iRow       AS INTEGER     NO-UNDO.

ASSIGN cTrackingMessage = "AuthGroup - Check auto "
                        + (IF btt_auth_detail.record_action = "DELETE":U THEN "delete" ELSE "create")
                        + " for auth group obj " + STRING(btt_auth_detail.auth_group_obj).
{ ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

GroupProviderBlock:
FOR EACH ctt_auth_provider
  WHERE ctt_auth_provider.auth_obj           = btt_auth_provider.auth_obj
  AND   ctt_auth_provider.auth_group_obj     = btt_auth_detail.auth_group_obj
  AND   ctt_auth_provider.auth_provider_obj <> btt_auth_provider.auth_provider_obj:

  /*
     Check if a detail line exists for the provider that is the same as the detail line that is added.
     Please note the obj and key is omitted in the check because the obj for a Tariff
     will be different for another provider if the disciplines are different.
  */
  FIND FIRST ctt_auth_detail NO-LOCK
    WHERE ctt_auth_detail.auth_obj                = ctt_auth_provider.auth_obj
    AND   ctt_auth_detail.auth_provider_obj       = ctt_auth_provider.auth_provider_obj
    AND   ctt_auth_detail.owning_entity_mnemonic  = btt_auth_detail.owning_entity_mnemonic
    AND   ctt_auth_detail.owning_alt_value        = btt_auth_detail.owning_alt_value
    AND   ctt_auth_detail.related_entity_mnemonic = btt_auth_detail.related_entity_mnemonic
    AND   ctt_auth_detail.related_obj             = btt_auth_detail.related_obj
    AND   ctt_auth_detail.related_key             = btt_auth_detail.related_key
    AND   ctt_auth_detail.start_date              = btt_auth_detail.start_date
    AND   ctt_auth_detail.start_ampm              = btt_auth_detail.start_ampm
    AND   ctt_auth_detail.end_date                = btt_auth_detail.end_date
    AND   ctt_auth_detail.end_ampm                = btt_auth_detail.end_ampm
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = FALSE }

  ASSIGN cTrackingMessage = "AuthGroup - FIND ctt_auth_detail record WHERE "
                          + "auth_obj="                     + STRING(ctt_auth_provider.auth_obj)
                          + " AND auth_provider_obj="       + STRING(ctt_auth_provider.auth_provider_obj)
                          + " AND owning_entity_mnemonic="  + btt_auth_detail.owning_entity_mnemonic
                          + " AND owning_alt_value="        + btt_auth_detail.owning_alt_value
                          + " AND related_entity_mnemonic=" + btt_auth_detail.related_entity_mnemonic
                          + " AND related_obj="             + STRING(btt_auth_detail.related_obj)
                          + " AND related_key="             + btt_auth_detail.related_key
                          + " AND start_date="              + STRING(btt_auth_detail.start_date,"9999/99/99":U)
                          + " AND start_ampm="              + STRING(btt_auth_detail.start_ampm,"AM/PM":U)
                          + " AND end_date="                + STRING(btt_auth_detail.end_date,"9999/99/99":U)
                          + " AND end_ampm="                + STRING(btt_auth_detail.end_ampm,"AM/PM":U)
                          + ". AVAILABLE ctt_auth_detail?"  + STRING(AVAILABLE ctt_auth_detail,"Yes/No":U)
                          + " btt_auth_detail.record_action=" + btt_auth_detail.record_action.
  { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  IF NOT AVAILABLE ctt_auth_detail
  AND btt_auth_detail.record_action <> "DELETE":U THEN
  DO:
    /*
      For a tariff first determine if the tariff is valid for the Provider before the new line is created.
    */
    IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
    DO:
      ASSIGN cBaseRate = ""
             cArsRate  = ""
             dOwningObj = 0.

      mipEnv:Health:maDoctor:getProviderBaseRates( INPUT  ctt_auth_provider.doc_num,
                                                   INPUT  goAuthorisation:MemNum,
                                                   INPUT  goAuthorisation:MemberOptionCode,
                                                   INPUT  ctt_auth_provider.start_date,
                                                   OUTPUT cBaseRate,
                                                   OUTPUT cArsRate).

      IF l99Dependant
      THEN ASSIGN cAddValidations = "".
      ELSE ASSIGN cAddValidations = "age,":U     + STRING(goAuthorisation:DependantAgeYears) +
                                    "|gender,":U + goAuthorisation:DependantGender           +
                                    "|BMI,":U    + STRING(goAuthorisation:DependantBMI).
      ASSIGN
        lSuccess = mipEnv:Health:maMedical:getValidTariff(INPUT-OUTPUT dOwningObj,                        /*  iopdTariffLinkObj */
                                                          INPUT        btt_auth_detail.owning_alt_value,  /*  ipcTariffCode     */
                                                          INPUT        cBaseRate,                         /*  ipcBaseRate       */
                                                          INPUT        cArsRate,                          /*  ipcARSRate        */
                                                          INPUT        ctt_auth_provider.pr_type,         /*  ipiPrType         */
                                                          INPUT        ctt_auth_provider.sub_pr_type,     /*  ipiSubPrType      */
                                                          INPUT        ctt_auth_provider.start_date,      /*  ipdDate           */
                                                          INPUT        goAuthorisation:OptionCode,        /*  ipiOptionCode     */
                                                          INPUT        cAddValidations,                   /*  ipcAddValidations */
                                                          OUTPUT       dTariffObj,                        /*  opdTariffObj      */
                                                          OUTPUT       dTrfCostObj,                       /*  opdTrfCostObj     */
                                                          OUTPUT       cError,                            /*  opcError          */
                                                          OUTPUT       cWarning,                          /*  opcWarning        */
                                                          OUTPUT       cAlertMessage).                    /*  opcAlertMessage   */

      ASSIGN cTrackingMessage = "AuthGroup - Check Tariff valid for provider -"
                              + " -INPUT- dOwningObj=0"
                              +         " TariffCode="     + btt_auth_detail.owning_alt_value
                              +         " BaseRate="       + cBaseRate
                              +         " ARSRate="        + cArsRate
                              +         " PrType="         + STRING(ctt_auth_provider.pr_type)
                              +         " SubPrType="      + STRING(ctt_auth_provider.sub_pr_type)
                              +         " Date="           + STRING(ctt_auth_provider.start_date,"9999/99/99":u)
                              +         " OptionCode="     + STRING(goAuthorisation:OptionCode)
                              +         " AddValidations=" + cAddValidations
                              + " -OUTPUT- dOwningObj="    + STRING(dOwningObj)
                              +          " dTariffObj="    + STRING(dTariffObj)
                              +          " dTrfCostObj="   + STRING(dTrfCostObj)
                              +          " cError="        + cError
                              +          " cWarning="      + cWarning
                              +          " cAlertMessage=" + cAlertMessage
                              + " ERROR-STATUS:ERROR?" + STRING(ERROR-STATUS:ERROR).
      { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

      IF cError <> "" THEN
      DO:
        /*
          Populate a warning to the user and don't create the detail line.
          Read next ctt_auth_provider record.
        */
        IF ctt_auth_provider.doc_num <> 0
        THEN
          ASSIGN cWarning = "Tariff " + btt_auth_detail.owning_alt_value + " is not valid for the provider " + STRING(ctt_auth_provider.doc_num)
                          + " and discipline " + STRING(ctt_auth_provider.pr_type,"999") + ".  Detail line will not be created.".
        ELSE
          ASSIGN cWarning = "Tariff " + btt_auth_detail.owning_alt_value + " is not valid for the provider discipline "
                          + STRING(ctt_auth_provider.pr_type,"999") + ".  Detail line will not be created.".
        goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                           INPUT "":U,                                               /* ipcOwningEntityKey      */
                           INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                           INPUT cWarning,                                           /* ipcMessageText          */
                           INPUT "WAR":U).                                           /* ipcMessageType          */
        NEXT GroupProviderBlock.
      END.  /* IF cError <> "" THEN */
    END.  // IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN

    ASSIGN cTrackingMessage = "AuthGroup - Create Detail Line -"
                              + " line_number="            + STRING(iRow)
                              + " record_action="          + "MODIFY":u
                              + " auth_detail_obj="        + STRING(iRow * -1)
                              + " auth_obj="               + STRING(btt_auth_detail.auth_obj)
                              + " auth_provider_obj="      + STRING(ctt_auth_provider.auth_provider_obj)
                              + " start_date="             + STRING(ctt_auth_provider.start_date,"9999/99/99":U)
                              + " start_ampm="             + STRING(ctt_auth_provider.start_ampm,"AM/PM":U)
                              + " end_date="               + (IF ctt_auth_provider.end_date = ? THEN "?" ELSE STRING(ctt_auth_provider.end_date,"9999/99/99":U))
                              + " end_ampm="               + STRING(ctt_auth_provider.end_ampm,"AM/PM":U)
                              + " owning_entity_mnemonic=" + btt_auth_detail.owning_entity_mnemonic
                              + " owning_obj="             + (IF btt_auth_detail.owning_entity = "htmtl":U THEN STRING(dOwningObj) ELSE STRING(btt_auth_detail.owning_obj))
                              + " owning_key="             + btt_auth_detail.owning_key
                              + " owning_alt_value="       + STRING(btt_auth_detail.owning_alt_value)
                              + " quantity_auth="          + STRING(btt_auth_detail.quantity_auth)
                              + " related_entity="         + btt_auth_detail.related_entity
                              + " related_obj="            + STRING(btt_auth_detail.related_obj)
                              + " related_key="            + btt_auth_detail.related_key
                              + " related_value="          + btt_auth_detail.related_value
                              + " auth_status="            + STRING(btt_auth_detail.auth_status)
                              + " auth_status_note="       + STRING(btt_auth_detail.auth_status_note)
                              + " auth_group_obj="         + STRING(btt_auth_detail.auth_group_obj)
                              + " _auth_group_update=YES".
    { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

    CREATE ctt_auth_detail.
    ASSIGN ctt_auth_detail.line_number            = iRow
           ctt_auth_detail.record_action          = "MODIFY":u
           ctt_auth_detail.auth_detail_obj        = iRow * -1
           iRow                                   = iRow + 1
           ctt_auth_detail.auth_obj               = btt_auth_detail.auth_obj
           ctt_auth_detail.auth_provider_obj      = ctt_auth_provider.auth_provider_obj
           ctt_auth_detail.start_date             = ctt_auth_provider.start_date
           ctt_auth_detail.start_ampm             = ctt_auth_provider.start_ampm
           ctt_auth_detail.end_date               = ctt_auth_provider.end_date
           ctt_auth_detail.end_ampm               = ctt_auth_provider.end_ampm
           ctt_auth_detail.owning_entity_mnemonic = btt_auth_detail.owning_entity_mnemonic
           ctt_auth_detail.owning_obj             = IF btt_auth_detail.owning_entity = "htmtl":U
                                                    THEN dOwningObj
                                                    ELSE btt_auth_detail.owning_obj
           ctt_auth_detail.owning_key             = btt_auth_detail.owning_key
           ctt_auth_detail.owning_alt_value       = btt_auth_detail.owning_alt_value
           ctt_auth_detail.quantity_auth          = btt_auth_detail.quantity_auth
           ctt_auth_detail.related_entity         = btt_auth_detail.related_entity
           ctt_auth_detail.related_obj            = btt_auth_detail.related_obj
           ctt_auth_detail.related_key            = btt_auth_detail.related_key
           ctt_auth_detail.related_value          = btt_auth_detail.related_value
           ctt_auth_detail.auth_status            = btt_auth_detail.auth_status
           ctt_auth_detail.auth_status_note       = btt_auth_detail.auth_status_note
           ctt_auth_detail.auth_group_obj         = btt_auth_detail.auth_group_obj
           ctt_auth_detail._auth_group_update     = YES. // Variable to indicate if the line is created by group processing.

    VALIDATE ctt_auth_detail.

    IF ctt_auth_provider.doc_num <> 0
    THEN
      ASSIGN cWarning = ctt_auth_detail.owning_alt_value + " is created for the provider " + STRING(ctt_auth_provider.doc_num)
                      + " and discipline " + STRING(ctt_auth_provider.pr_type,"999":U) + " that belongs to the same Authorisation Group.".
    ELSE
      ASSIGN cWarning = ctt_auth_detail.owning_alt_value + " is created for the provider discipline " + STRING(ctt_auth_provider.pr_type,"999":U)
                      + " that belongs to the same Authorisation Group.".

    goErrorObject:addError( INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                            INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                            INPUT "":U,                                               /* ipcOwningEntityKey      */
                            INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                            INPUT cWarning,                                           /* ipcMessageText          */
                            INPUT "WAR":U).                                           /* ipcMessageType          */

    ASSIGN cTrackingMessage = "AuthGroup - " + cWarning.
    { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  END.  // IF NOT AVAILABLE ctt_auth_detail THEN
  ELSE IF AVAILABLE ctt_auth_detail
       AND btt_auth_detail.record_action = "DELETE":U THEN
  DO:
    ASSIGN ctt_auth_detail.record_action       = "DELETE":U
           ctt_auth_detail._auth_group_updated = yes.

    IF ctt_auth_provider.doc_num <> 0
    THEN
      ASSIGN cWarning = ctt_auth_detail.owning_alt_value + " will be deleted for the provider " + STRING(ctt_auth_provider.doc_num)
                      + " and discipline " + STRING(ctt_auth_provider.pr_type,"999":U) + " that belongs to the same Authorisation Group.".
    ELSE
      ASSIGN cWarning = ctt_auth_detail.owning_alt_value + " will be deleted for the provider discipline " + STRING(ctt_auth_provider.pr_type,"999":U)
                      + " that belongs to the same Authorisation Group.".

    goErrorObject:addError( INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */
                            INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */
                            INPUT "":U,                                               /* ipcOwningEntityKey      */
                            INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */
                            INPUT cWarning,                                           /* ipcMessageText          */
                            INPUT "WAR":U).                                           /* ipcMessageType          */

    ASSIGN cTrackingMessage = "AuthGroup - " + cWarning.
    { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

  END.  // ELSE - IF NOT AVAILABLE ctt_auth_detail THEN
END.  // FOR EACH ctt_auth_provider
