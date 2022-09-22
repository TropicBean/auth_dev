/* maauthcopayservaldetitem.i  MEDSTAR Medical Aid System
                               Validate Auth Copay Detail Items
                               (c) Copyright 2022 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/

  DEFINE PARAMETER BUFFER btt_auth_copay_detail_item FOR tt_auth_copay_detail_item.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred   AS LOGICAL.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cAcronymEntity            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cAlertMessage             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cEntity                   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cLabel                    AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cTrackingMessage          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cWarning                  AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE dTariffObj                AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dTrfCostObj               AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dTrfLinkObj               AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE iOptionCode               AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lSuccess                  AS LOGICAL             NO-UNDO.

  DEFINE VARIABLE oErrorObject              AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE oWarnMsgType              AS cls.mipacronym      NO-UNDO.
  DEFINE VARIABLE oCopayDetailItemEntities  AS cls.mipacronym      NO-UNDO.

  DEFINE BUFFER buf_auth_copay_type         FOR ham_auth_copay_type.
  DEFINE BUFFER buf_auth_copay_detail_item  FOR hac_auth_copay_detail_item.

  /*
    Make sure we have a valid buffer before we go any further
  */
  IF NOT AVAILABLE btt_auth_copay_detail_item THEN
  DO:
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Copay Detail Item as no buffer is available.'" }
  END.  // IF NOT AVAILABLE btt_auth_copay_detail_item THEN

  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_detail_item_error:HANDLE).

  /*
    Ensure that a valid insurer has been specified
  */
  IF  btt_auth_copay_detail_item.insurer_obj <> 0.00
  AND btt_auth_copay_detail_item.insurer_obj <> ? THEN
  DO:
    FIND FIRST erm_insurer NO-LOCK
      WHERE erm_insurer.insurer_obj = btt_auth_copay_detail_item.insurer_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE erm_insurer THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "insurer_obj":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Copay Detail Item Client:":U + STRING(btt_auth_copay_detail_item.insurer_obj)).
    END. /* IF NOT AVAILABLE erm_insurer THEN */
  END. /* IF btt_auth_copay_detail_item.insurer_obj <> 0.00 AND btt_auth_copay_detail_item.insurer_obj <> ? */

  /*
    If a scheme has been specified, make sure it exists
  */
  IF btt_auth_copay_detail_item.option_code <> 0 THEN
  DO:
    FIND FIRST scheme NO-LOCK
      WHERE scheme.scheme-code = btt_auth_copay_detail_item.option_code NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

    IF NOT AVAILABLE scheme THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "option_code":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Copay Detail Item Scheme Option: ":U + STRING(btt_auth_copay_detail_item.option_code)).
    END.  /* IF NOT AVAILABLE scheme THEN */
  END. /* IF btt_auth_copay_detail_item.option_code <> "" THEN */

  /*
    Ensure that a valid Auth Copay Type has been specified
  */
  IF btt_auth_copay_detail_item.auth_copay_type_obj = 0
  OR btt_auth_copay_detail_item.auth_copay_type_obj = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "auth_copay_type_obj":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Type,":U ).
  END. /* IF btt_auth_copay_detail_item.auth_copay_type_obj = 0 OR btt_auth_copay_detail_item.auth_copay_type_obj = ? THEN */
  ELSE DO:
    IF NOT CAN-FIND(FIRST buf_auth_copay_type
                    WHERE buf_auth_copay_type.auth_copay_type_obj = btt_auth_copay_detail_item.auth_copay_type_obj) THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "auth_copay_type_obj":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid */
                            INPUT "Auth Copay Type":U ).
    END.  /* IF NOT CAN-FIND(FIRST buf_auth_copay_type */
  END.  /* ELSE - IF btt_auth_copay_detail_item.auth_copay_type_obj = 0... */

  /*
    Ensure that a valid Auth Copay Detail Item effective date has been specified
  */
  IF btt_auth_copay_detail_item.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Detail Item Effective Date,":U) .
  END. /* IF btt_auth_copay_detail_item.effective_date = ? THEN */

  /*
    If an end date is supplied, ensure that is not before the effective date
  */
  IF  btt_auth_copay_detail_item.end_date <> ?
  AND btt_auth_copay_detail_item.end_date < btt_auth_copay_detail_item.effective_date THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 11,  /* The End Date &1 cannot be before the Effective/Start Date &2 */
                          INPUT STRING(btt_auth_copay_detail_item.end_date,"9999/99/99") + ","
                              + STRING(btt_auth_copay_detail_item.effective_date,"9999/99/99")).
  END. /* IF btt_auth_copay_detail_item.end_date <> ?...THEN */

  /*
    Ensure that a valid Owning entity mnemonic has been specified
  */
  IF btt_auth_copay_detail_item.owning_entity_mnemonic = "":U
  OR btt_auth_copay_detail_item.owning_entity_mnemonic = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "owning_entity_mnemonic":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Copay Detail Item entity,":U ).
  END.  /* IF btt_auth_copay_detail_item.owning_entity_mnemonic = "":U */
  ELSE DO:
    CASE btt_auth_copay_detail_item.owning_entity_mnemonic:
      WHEN "htmtl":U THEN ASSIGN cEntity = "Tariff":U.
      WHEN "hlmnl":U THEN ASSIGN cEntity = "Nappi":U.
      WHEN "hlmcr":U THEN ASSIGN cEntity = "Basket":U.
      WHEN "hlmac":U THEN ASSIGN cEntity = "ATC Class":U.
    END CASE.

    ASSIGN cAcronymEntity = "ma_acAuthCopayDetailItemEntities" + REPLACE(cEntity," ","").

    oCopayDetailItemEntities = NEW cls.mipacronym(?, FALSE, "ma_acAuthCopayDetailItemEntities":U, ?).

    oCopayDetailItemEntities:focusAcronym("KEY":U, cAcronymEntity) NO-ERROR.

    IF NOT oCopayDetailItemEntities:AcronymInFocus THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "owning_entity_mnemonic":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid*/
                            INPUT "Auth Copay Entity: " + btt_auth_copay_detail_item.owning_entity_mnemonic).
    END.  /* IF NOT oCopayDetailItemEntities:AcronymInFocus THEN  */
    ELSE DO:
      ASSIGN cLabel = oCopayDetailItemEntities:AcronymLabel.

      /*
        Validate the owning obj and owning key
      */
      IF btt_auth_copay_detail_item.owning_obj = ? THEN ASSIGN btt_auth_copay_detail_item.owning_obj = 0.
      IF btt_auth_copay_detail_item.owning_key = ? THEN ASSIGN btt_auth_copay_detail_item.owning_key = "".

      IF  btt_auth_copay_detail_item.owning_obj = 0
      AND btt_auth_copay_detail_item.owning_key = "" THEN
      DO:
        ASSIGN oplFailureOccurred = TRUE.

        oErrorObject:addError(INPUT "hacci":U,
                              INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                              INPUT "":U,
                              INPUT "owning_obj":U,
                              INPUT btt_auth_copay_detail_item.line_number,
                              INPUT "Either the Owning Obj or the Owning Key must be specified.",
                              INPUT "ERR":U).
      END.  /* IF btt_auth_copay_detail_item.owning_obj = 0 */

      /*
        Ensure that a valid owning alt value has been specified
      */
      ASSIGN btt_auth_copay_detail_item.owning_alt_value = IF btt_auth_copay_detail_item.owning_alt_value = ?
                                                           THEN "":U
                                                           ELSE TRIM(btt_auth_copay_detail_item.owning_alt_value).

      IF  btt_auth_copay_detail_item.owning_alt_value        = "":U
      AND btt_auth_copay_detail_item.owning_entity_mnemonic <> "hlmac":U THEN /* Not mandatory for ATC Classes, as they are found with the owning_key */
      DO:
        ASSIGN oplFailureOccurred = TRUE
               cErrorMessage      = SUBSTITUTE("The &1 must be specified.","value for the ":U + cLabel).

        oErrorObject:addError(INPUT "hacci":U,
                              INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                              INPUT "":U,
                              INPUT "owning_alt_value":U,
                              INPUT btt_auth_copay_detail_item.line_number,
                              INPUT cErrorMessage,
                              INPUT "ERR":U).
      END. /* btt_auth_copay_detail_item.owning_alt_value = "":U THEN */

      /*
        Ensure that a valid ATC Class has been specified
      */
      IF btt_auth_copay_detail_item.owning_entity_mnemonic = "hlmac":U THEN
      DO:
        FIND hlm_atc_class NO-LOCK
          WHERE hlm_atc_class.atc_class_key = btt_auth_copay_detail_item.owning_key NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE hlm_atc_class THEN
        DO:
          ASSIGN oplFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hacci":U,
                                INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                                INPUT "":U,
                                INPUT "owning_key":U,
                                INPUT btt_auth_copay_detail_item.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid*/
                                INPUT "Owning ATC Class entity: ":U +
                                      btt_auth_copay_detail_item.owning_key).
        END.  /* IF NOT AVAILABLE hlm_atc_class THEN */
        ELSE
          ASSIGN btt_auth_copay_detail_item.owning_obj = 0. 	/* ATC Classes don't have an Obj, don't save -1 or >0 value to DB */
      END.  /* IF btt_auth_copay_detail_item.owning_entity_mnemonic = "hlmac":U THEN */

      /*
        Ensure that a valid Basket has been specified.  No detail entries need to be checked.
      */
      IF btt_auth_copay_detail_item.owning_entity_mnemonic = "hlmcr":U THEN
      DO:
        ASSIGN lSuccess = mipEnv:Health:maMedical:validateCrosswalk
                                (INPUT-OUTPUT  btt_auth_copay_detail_item.owning_obj,     	 /* ipdCrosswalkObj  */
                                 INPUT         btt_auth_copay_detail_item.owning_alt_value,  /* ipcCrosswalkCode */
                                 INPUT         "ma_acCrossTypeBasket":U,                     /* ipcCrosswalkType */
                                 INPUT         btt_auth_copay_detail_item.effective_date,    /* ipdStartDate     */
                                 INPUT         NO,                                           /* iplCheckDetails  */
                                 INPUT         "":U,                                         /* ipcDetailEntity  */
                                 OUTPUT        cErrorMessage).                               /* opcError         */

        IF cErrorMessage <> "":U THEN
        DO:
          ASSIGN oplFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hacci":U,
                                INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                                INPUT "":U,
                                INPUT "owning_alt_value":U,
                                INPUT btt_auth_copay_detail_item.line_number,
                                INPUT cErrorMessage,
                                INPUT "Err").
        END.  /* IF cErrorMessage <> "":U THEN */
      END.  /* IF btt_auth_copay_detail_item.owning_entity_mnemonic = "hlmcr":U THEN */

      /*
        Ensure that a valid Nappi has been specified.
      */
      IF btt_auth_copay_detail_item.owning_entity_mnemonic = "hlmnl":U THEN
      DO:
        ASSIGN lSuccess = mipEnv:Health:maMedical:getValidNappi
                                  (INPUT-OUTPUT btt_auth_copay_detail_item.owning_obj,        /* ipdNappiLinkObj */
                                   INPUT        btt_auth_copay_detail_item.owning_alt_value,  /* ipcNappiCode    */
                                   INPUT        btt_auth_copay_detail_item.effective_date,    /* ipdDate         */
                                   OUTPUT       cErrorMessage).

        IF cErrorMessage <> "":U THEN
        DO:
          ASSIGN oplFailureOccurred = TRUE.
          oErrorObject:addError(INPUT "hacci":U,
                                INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                                INPUT "":U,
                                INPUT "owning_alt_value":U,
                                INPUT btt_auth_copay_detail_item.line_number,
                                INPUT cErrorMessage,
                                INPUT "Err").
        END.  /* IF cErrorMessage <> "":U THEN */
      END.  /* IF btt_auth_copay_detail_item.owning_entity_mnemonic = "hlmnl":U THEN */

      /*
        Ensure that a valid Tariff(link) has been specified.
      */
      IF btt_auth_copay_detail_item.owning_entity_mnemonic = "htmtl":U THEN
      DO:
        FIND htm_tariff_link NO-LOCK
          WHERE htm_tariff_link.tariff_link_obj = btt_auth_copay_detail_item.owning_obj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE htm_tariff_link THEN
        DO:
          ASSIGN oplFailureOccurred = TRUE
                 cTrackingMessage   = "CopayDetItemServVal - htm_tariff_link not available for btt_auth_copay_detail_item.owning_obj="
                                    + STRING(btt_auth_copay_detail_item.owning_obj).

          { ma/inc/malogging.i &MessageGroup = "'ma_DebugSetup'" &LogMessage = cTrackingMessage }

          oErrorObject:addError(INPUT "hacci":U,
                                INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                                INPUT "":U,
                                INPUT "owning_alt_value":U,
                                INPUT btt_auth_copay_detail_item.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid*/
                                INPUT "Tariff Code: ":U +
                                      btt_auth_copay_detail_item.owning_alt_value).
        END.  /* IF NOT AVAILABLE htm_tariff_link THEN */
        ELSE DO:
          IF htm_tariff_link.tariff_code <> btt_auth_copay_detail_item.owning_alt_value THEN
          DO:
            ASSIGN oplFailureOccurred = TRUE.
	          oErrorObject:addError(INPUT "hacci":U,
	                                INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
	                                INPUT "":U,
	                                INPUT "owning_alt_value":U,
	                                INPUT btt_auth_copay_detail_item.line_number,
	                                INPUT "MA":U,
	                                INPUT 100,  /* The "&1" specified is invalid */
	                                INPUT "Tariff Code for the Tariff Link: ":U +
                                         btt_auth_copay_detail_item.owning_alt_value).
          END.  /* IF htm_tariff_link.tariff_code <> btt_auth_copay_detail_item.owning_alt_value */

          IF NOT htm_tariff_link.tariff_link_default THEN 	/* Tariff doesn't link to default record */
          DO:
            /*
              Ensure that a non-default Tariff is valid for that Baserate, ARSrate and effective-date
            */
	          ASSIGN dTrfLinkObj = htm_tariff_link.tariff_link_obj.

            IF btt_auth_copay_detail_item.option_code = 0 THEN
            DO:
              FIND FIRST scheme NO-LOCK
                WHERE scheme.active  NO-ERROR.

              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

              IF AVAILABLE scheme
              THEN
                ASSIGN iOptionCode = scheme.scheme-code.
            END.
            ELSE
              ASSIGN iOptionCode = btt_auth_copay_detail_item.option_code.

            mipEnv:Health:mamedical:getValidTariff(INPUT-OUTPUT dTrfLinkObj,                                 // iopdTariffLinkObj
	                                                 INPUT        btt_auth_copay_detail_item.owning_alt_value, // ipcTariffCode
	                                                 INPUT        htm_tariff_link.base_rate,                   // ipcBaseRate
	                                                 INPUT        htm_tariff_link.ars_rate,                    // ipcARSRate
	                                                 INPUT        INTEGER(htm_tariff_link.pr_type),            // ipiPrType
	                                                 INPUT        0,                                           // ipiSubPrType
	                                                 INPUT        btt_auth_copay_detail_item.effective_date,   // ipdDate
	                                                 INPUT        iOptionCode,                                 // ipiOptionCode
	                                                 INPUT        "":U,                                        // ipcAddValidations
	                                                 OUTPUT       dTariffObj,                                  // opdTariffObj
	                                                 OUTPUT       dTrfCostObj,                                 // opdTrfCostObj
	                                                 OUTPUT       cErrorMessage,                               // opcError
	                                                 OUTPUT       cWarning,                                    // opcWarning
	                                                 OUTPUT       cAlertMessage).                              // cAlertMessage
	          IF cErrorMessage <> "":U THEN
	          DO:
	            ASSIGN oplFailureOccurred = TRUE.
	            oErrorObject:addError(INPUT "hacci":U,
	            	                    INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
	            	                    INPUT "":U,
	            	                    INPUT "owning_alt_value":U,
	            	                    INPUT btt_auth_copay_detail_item.line_number,
	            	                    INPUT cErrorMessage,
                                            INPUT "Err").
	          END.  /* IF cErrorMessage <> "":U THEN */

            /*
              Ensure that a valid PrType is entered for a non-default Tariff
            */
            IF (btt_auth_copay_detail_item.pr_type  = 0
            OR  btt_auth_copay_detail_item.pr_type <> INTEGER(htm_tariff_link.pr_type)) THEN
            DO:
              ASSIGN oplFailureOccurred = TRUE
                     cErrorMessage      = "Tariff is not a default tariff, therefore a Discipline code (" +
                                          STRING(btt_auth_copay_detail_item.pr_type,"999") + ") that match the Tariff Link (" +
                                          htm_tariff_link.pr_type + "), must be provided.".
              oErrorObject:addError(INPUT "hacci":U,
                                    INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                                    INPUT "":U,
                                    INPUT "owning_alt_value":U,
                                    INPUT btt_auth_copay_detail_item.line_number,
                                    INPUT cErrorMessage,
                                    INPUT "Err").
            END.  /* IF (btt_auth_copay_detail_item.pr_type  = 0... */
          END.  /* IF NOT htm_tariff_link.tariff_link_default... */
        END.  /* ELSE - IF NOT AVAILABLE htm_tariff_link THEN */
      END.  /* IF btt_auth_copay_detail_item.owning_entity_mnemonic = "htmtl":U THEN */
    END.  /* ELSE - IF NOT oBodyRegion:AcronymInFocus THEN */
  END.  /* ELSE - IF btt_auth_copay_detail_item.owning_entity_mnemonic = "":U OR btt_auth_copay_detail_item.owning_entity_mnemonic = ? THEN */

  /*
    If the Practice Type has been specified, make sure it is valid
  */
  IF btt_auth_copay_detail_item.pr_type <> 0 THEN
  DO:
    mipEnv:Health:maAdministration:validateDiscipline(INPUT  STRING(btt_auth_copay_detail_item.pr_type), /* PrType */
                                                      INPUT  0,                                          /* SubPrType */
                                                      OUTPUT cErrorMessage).                             /* ErrorMessage */
    IF cErrorMessage <> "":U THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.
      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "pr_type":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid*/
                            INPUT "Discipline Code: ":U +
                                  STRING(btt_auth_copay_detail_item.pr_type)).
    END. /* IF cErrorMessage <> "":U */
  END.  /* IF btt_auth_copay_detail_item.pr_type <> 0 THEN */

  /*
    Ensure that a valid Copayment Value Type has been specified for the Auth Copay Detail Item
  */
  IF btt_auth_copay_detail_item.copayment_value_type = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "copayment_value_type":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 112,  /* The &1 specified is invalid. &2 */    
                          INPUT "Copayment Value Type, Only values R or P is accepted.":U).  
  END. /* IF btt_auth_copay_detail_item.copayment_value_type = ? THEN */

  /*
    Ensure that a valid Copayment Value has been specified for the Auth Copay Detail Item
  */
  IF btt_auth_copay_detail_item.copayment_value <= 0 THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "copayment_value":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Copayment Value, Value must be greater than 0.").
  END. /* IF btt_auth_copay_detail_item.copayment_value_type <= 0 THEN */

  /*
    Ensure that a Copayment Value Type of "P"ercentage does not exceed a 100
  */
  IF  btt_auth_copay_detail_item.copayment_value_type
  AND btt_auth_copay_detail_item.copayment_value > 100 THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "copayment_value":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "The Copayment Value for a Percentage may not exceed a 100.",
                          INPUT "ERR":U).
  END. /* IF btt_auth_copay_detail_item.copayment_value_type = ? THEN */

  /*
    Ensure that a valid Apply To PMB has been specified for the Auth Copay Detail Item
  */
  IF btt_auth_copay_detail_item.apply_to_pmb = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "apply_to_pmb":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 100,  /* The "&1" specified is invalid*/
                          INPUT "Apply To PMB: ?":U).
  END. /* IF btt_auth_copay_detail_item.apply_to_pmb = ? THEN */

  /*
    Ensure that a valid Apply To Emergency has been specified for the Auth Copay Detail Item
  */
  IF btt_auth_copay_detail_item.apply_to_emergency = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacci":U,
                          INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                          INPUT "":U,
                          INPUT "apply_to_emergency":U,
                          INPUT btt_auth_copay_detail_item.line_number,
                          INPUT "MA":U,
                          INPUT 100,  /* The "&1" specified is invalid*/
                          INPUT "Apply To Emergency: ?":U).
  END. /* IF btt_auth_copay_detail_item.apply_to_emergency = ? THEN */

  /*
    Ensure that a valid Warning Message Type has been specified if a Warning Message is captured and vice versa
  */
  IF (btt_auth_copay_detail_item.warning_message      <> "":U
  AND btt_auth_copay_detail_item.warning_message_type  = "":U)
  OR (btt_auth_copay_detail_item.warning_message       = "":U
  AND btt_auth_copay_detail_item.warning_message_type <> "":U) THEN
  DO:
    IF btt_auth_copay_detail_item.warning_message_type = "":U THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "warning_message_type":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 111,  /* The &1 must be specified. &2 */
                            INPUT "Warning Message Type,":U ).
    END. /* IF btt_auth_copay_detail_item.warning_message_type = "":U THEN */

    IF btt_auth_copay_detail_item.warning_message = "":U THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "warning_message":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 111,  /* The &1 must be specified. &2 */
                            INPUT "Warning Message,":U ).
    END. /* IF btt_auth_copay_detail_item.warning_message = "":U THEN */
  END.  /* IF btt_auth_copay_detail_item.warning_message <> "":U... */

  IF btt_auth_copay_detail_item.warning_message_type <> "":U THEN
  DO:
    oWarnMsgType = NEW cls.mipacronym(?, FALSE, "ma_acAuthCopayWarnMessageType":U, ?).

    oWarnMsgType:focusAcronym("KEY":U, btt_auth_copay_detail_item.warning_message_type) NO-ERROR.

    IF NOT oWarnMsgType:AcronymInFocus THEN
    DO:
      ASSIGN oplFailureOccurred = TRUE.

      oErrorObject:addError(INPUT "hacci":U,
                            INPUT btt_auth_copay_detail_item.auth_copay_detail_item_obj,
                            INPUT "":U,
                            INPUT "warning_message_type":U,
                            INPUT btt_auth_copay_detail_item.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid*/
                            INPUT "Warning Message Type: ":U +
                                  btt_auth_copay_detail_item.warning_message_type).
    END.  /* IF NOT oWarnMsgType:AcronymInFocus THEN */
  END.  /* IF btt_auth_copay_detail_item.warning_message_type <> "":U THEN  */

{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }

&ENDIF

