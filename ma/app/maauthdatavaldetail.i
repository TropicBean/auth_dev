/* maauthdatavaldetail.i MEDSTAR Medical Aid System
                         Healthcare Auth data access service: Validate auth detail
                         (c) Copyright 2017 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Detail Buffer
  Parameters:
  Notes     : Basic field level validation only, all business logic type
              validation should be placed in the business logic stack
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE cEntity           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE lNappiRequired    AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess          AS LOGICAL             NO-UNDO.

  DEFINE VARIABLE oErrorObject      AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE oBodyRegion       AS cls.mipacronym      NO-UNDO.
  DEFINE VARIABLE oClaimCodeSource  AS cls.mipacronym      NO-UNDO.
  DEFINE VARIABLE oCopayPayeeDMInd  AS cls.mipacronym      NO-UNDO.
  DEFINE VARIABLE oLetterPrintInd   AS cls.mipacronym      NO-UNDO.

  DEFINE BUFFER buf_auth_group      FOR ham_auth_group.
  DEFINE BUFFER buf_auth_provider   FOR hat_auth_provider.
  DEFINE BUFFER buf_cpt_link        FOR hlm_cpt_link.
  DEFINE BUFFER btt_auth_detail     FOR tt_auth_detail.

  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  FOR EACH btt_auth_detail NO-LOCK:
    IF CAN-DO("{&ModifyList}":U, btt_auth_detail.record_action) THEN
    DO:
      /*
        Auth group validation
      */
      IF btt_auth_detail.auth_group_obj <> 0.00
      AND NOT CAN-FIND(FIRST buf_auth_group NO-LOCK
                       WHERE buf_auth_group.auth_group_obj  = btt_auth_detail.auth_group_obj
                       AND   buf_auth_group.effective_date <= btt_auth_detail.start_date
                       AND  (buf_auth_group.end_date        = ?
                       OR    buf_auth_group.end_date       >= btt_auth_detail.start_date)) THEN
      DO:
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "auth_group_obj":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Authorisation Detail Auth Group (Obj = ":U + STRING(btt_auth_detail.auth_group_obj) + ")").
      END. /*IF btt_auth_detail.auth_group_obj <> 0.00 THEN*/

      /*
        CPT validation
      */
      IF btt_auth_detail.cpt_link_obj <> 0.00
      AND NOT CAN-FIND(FIRST buf_cpt_link NO-LOCK
                       WHERE buf_cpt_link.cpt_link_obj = btt_auth_detail.cpt_link_obj) THEN
      DO:
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "cpt_link_obj":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Authorisation Detail CPT (Obj = ":U + STRING(btt_auth_detail.cpt_link_obj) + ")").
      END. /*IF btt_auth_detail.cpt_link_obj <> 0.00 THEN*/

      /*
        Owning entity validation
      */
      IF btt_auth_detail.owning_entity_mnemonic <> "":U THEN
      DO:
        CASE btt_auth_detail.owning_entity_mnemonic:
          WHEN "hlmnl":U  THEN ASSIGN cEntity = "Nappi".
          WHEN "htmtl":U  THEN ASSIGN cEntity = "Tariff".
          WHEN "hlmcr":U  THEN ASSIGN cEntity = "Basket".
          OTHERWISE            ASSIGN cEntity = "Entity".
        END CASE.

        mipEnv:miDBEntity:focusTable(btt_auth_detail.owning_entity_mnemonic).

        IF NOT mipEnv:miDBEntity:InFocus THEN
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT "owning_entity_mnemonic":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 107,  /* Could not find a "&1" record using "&2" */
                                INPUT "valid owning entity,":U + cEntity).

        ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U THEN
                             mipEnv:miDBEntity:findRecord(btt_auth_detail.owning_obj)
                           ELSE
                             mipEnv:miDBEntity:findRecord(btt_auth_detail.owning_key)).

        IF NOT mipEnv:miDBEntity:RecordAvailable
        THEN
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U
                                       THEN "btt_auth_detail.owning_obj":U
                                       ELSE "btt_auth_detail.owning_key":U),
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 107,  /* Could not find a "&1" record using "&2" */
                                INPUT mipEnv:miDBEntity:TableLabel + ",":U + (IF btt_auth_detail.owning_key <> ""
                                                                              THEN btt_auth_detail.owning_key
                                                                              ELSE btt_auth_detail.owning_alt_value)).
      END. /*IF btt_auth_detail.owning_entity_mnemonic <> "":U THEN*/

      /*
        Related entity validation
      */
      IF btt_auth_detail.related_entity_mnemonic <> "":U THEN
      DO:
        mipEnv:miDBEntity:focusTable(btt_auth_detail.related_entity_mnemonic).

        IF NOT mipEnv:miDBEntity:InFocus THEN
        DO:
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT "related_entity_mnemonic":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 107,  /* Could not find a "&1" record using "&2" */
                                INPUT "valid related entity,":U + btt_auth_detail.related_entity_mnemonic).
        END. /*IF NOT mipEnv:miDBEntity:InFocus THEN*/

        IF btt_auth_detail.related_obj <> 0.00 OR btt_auth_detail.related_key <> "":U THEN
        DO:
          ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U THEN
                               mipEnv:miDBEntity:findRecord(btt_auth_detail.related_obj)
                             ELSE
                               mipEnv:miDBEntity:findRecord(btt_auth_detail.related_key)).

          IF NOT mipEnv:miDBEntity:RecordAvailable
          THEN
            oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                  INPUT btt_auth_detail.auth_detail_obj,
                                  INPUT "":U,
                                  INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U
                                         THEN "btt_auth_detail.related_obj":U
                                         ELSE "btt_auth_detail.related_key":U),
                                  INPUT btt_auth_detail.line_number,
                                  INPUT "MA":U,
                                  INPUT 107,  /* Could not find a "&1" record using "&2" */
                                  INPUT mipEnv:miDBEntity:TableLabel + ",":U + (IF btt_auth_detail.related_key <> ""
                                                                                THEN btt_auth_detail.related_key
                                                                                ELSE btt_auth_detail.related_value)).
        END. /*IF btt_auth_detail.related_obj <> 0.00 OR btt_auth_detail.related_key <> "":U THEN*/
      END. /*IF btt_auth_detail.related_entity_mnemonic <> "":U THEN*/

      /*
        Ensure that a valid start date has been specified
      */
      IF btt_auth_detail.start_date = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "start_date":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 111,  /* The &1 must be specified. &2 */
                              INPUT "Authorisation Detail Start Date,":U).
      ELSE IF  btt_auth_detail.end_date  <> ?
           AND btt_auth_detail.start_date > btt_auth_detail.end_date THEN
                oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT "start_date":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT "ma_MsgAuth":U,
                                      INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */
                                      INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") +
                                            ",Authorisation Detail End," + STRING(btt_auth_detail.end_date, "9999/99/99")).

      /*
        Minutes Validations
      */
      IF btt_auth_detail.minutes_requested = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "minutes_requested":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 111, /* The &1 must be specified. &2 */
                              INPUT "Minutes Requested," ).

      IF btt_auth_detail.minutes_requested < 0
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "minutes_requested":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Minutes Requested, The Minutes Requested must be greater than or equal to 0.").

      IF btt_auth_detail.minutes_auth = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "minutes_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 111, /* The &1 must be specified. &2 */
                              INPUT "Minutes Authorised," ).

      IF btt_auth_detail.minutes_auth < 0
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "minutes_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Minutes Authorised, The Minutes Authorised must be greater than or equal to 0.").

      IF  btt_auth_detail.minutes_requested <> 0
      AND btt_auth_detail.minutes_requested <  btt_auth_detail.minutes_auth
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "minutes_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Minutes Authorised, Minutes Authorised can't be more than Minutes Requested.").

      IF btt_auth_detail.minutes_tariff_type_obj = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "minutes_tariff_type_obj":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 111, /* The &1 must be specified. &2 */
                              INPUT "Tariff Type for Minutes," ).

      IF btt_auth_detail.minutes_tariff_type_obj <> 0
      AND NOT CAN-FIND(htm_tariff_type WHERE htm_tariff_type.tariff_type_obj = btt_auth_detail.minutes_tariff_type_obj)
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "minutes_tariff_type_obj":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112, /* The &1 specified is invalid. &2 */
                              INPUT "Tariff Type for Minutes," ).

      /*
        Auth Detail Body Region Validation
      */
      IF btt_auth_detail.body_region <> "":U THEN
      DO:
        oBodyRegion = NEW cls.mipacronym(?, FALSE, "ma_acBodyRegion":U, ?).

        oBodyRegion:focusAcronym("KEY":U, btt_auth_detail.body_region) NO-ERROR.

        IF NOT oBodyRegion:AcronymInFocus THEN
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_obj,
                                INPUT "":U,
                                INPUT "body_region":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Body Region: ":U + btt_auth_detail.body_region).
      END. /*IF btt_auth_detail.body_region <> "":U THEN*/

      /*
        Auth Detail Default Claim Code Source Validation
      */
      IF btt_auth_detail.default_claim_code_source <> "":U THEN
      DO:
        oClaimCodeSource = NEW cls.mipacronym(?, FALSE, "ma_acAuthDefClaimCodeSource":U, ?).

        oClaimCodeSource:focusAcronym("KEY":U, btt_auth_detail.default_claim_code_source) NO-ERROR.

        IF NOT oClaimCodeSource:AcronymInFocus THEN
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_obj,
                                INPUT "":U,
                                INPUT "default_claim_code_source":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Default Claim Code Source":U + btt_auth_detail.body_region).
      END. /*IF btt_auth_detail.body_region <> "":U THEN*/

      /*
        Letter Print Indicator Detail Validation
      */
      IF btt_auth_detail.print_dm <> "":U THEN
      DO:
        oLetterPrintInd = NEW cls.mipacronym(?, FALSE, "ma_acAuthPrintLetter":U, ?).

        oLetterPrintInd:focusAcronym("KEY":U, btt_auth_detail.print_dm) NO-ERROR.

        IF NOT oLetterPrintInd:AcronymInFocus THEN
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT "print_dm":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Print Indicator: ":U + btt_auth_detail.print_dm).
      END. /*IF btt_auth_detail.print_dm <> "":U THEN*/

      /*
        PayeeDM Indicator Detail Validation
      */
      IF btt_auth_detail.payee_dm <> "":U THEN
      DO:
        oCopayPayeeDMInd = NEW cls.mipacronym(?, FALSE, "ma_acAuthPayee":U, ?).

        oCopayPayeeDMInd:focusAcronym("KEY":U, btt_auth_detail.payee_dm) NO-ERROR.

        IF NOT oCopayPayeeDMInd:AcronymInFocus THEN
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT "payee_dm":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Co-payment Payee Indicator: ":U + btt_auth_detail.payee_dm).
      END. /*IF btt_auth_detail.payee_dm <> "":U THEN*/

      /*
        Line Restriction Detail Validation
      */
      IF btt_auth_detail.line_restriction <> "":U THEN
      DO:
        ASSIGN lSuccess = mipEnv:Health:AuthService:validateLineRestriction(INPUT btt_auth_detail.line_restriction).

        IF NOT lSuccess THEN
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT "line_restriction":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Line Restriction: ":U + btt_auth_detail.line_restriction).
      END. /*IF btt_auth_detail.line_restriction <> "":U THEN*/

      /*
        Amount validations
      */
      IF btt_auth_detail.amount_auth = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Amount Authorised" ).

      IF btt_auth_detail.amount_auth < 0
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Amount Authorised , The Amount Authorised must be greater than or equal to 0 .").

      IF  btt_auth_detail.amount_auth > 0
      AND btt_auth_detail.amount_auth < btt_auth_detail.amount_paid
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Amount Authorised , The Amount Authorised can not be less than the amount paid. ").

      IF btt_auth_detail.amount_paid = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "amount_paid":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Amount Paid" ).

      IF btt_auth_detail.amount_paid < 0
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "amount_paid":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Amount Paid , The Amount Paid must be greater than or equal to 0 .").

      /*
        Quantity validations
      */
      IF btt_auth_detail.quantity_auth = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Quantity Authorised" ).

      IF btt_auth_detail.quantity_auth < 0
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Quantity Authorised , The Quantity Authorised must be greater than or equal to 0 .").

      IF  btt_auth_detail.quantity_auth > 0
      AND btt_auth_detail.quantity_auth < btt_auth_detail.quantity_paid
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Quantity Authorised , The Quantity Authorised can not be less than the Quantity paid. ").

      IF btt_auth_detail.quantity_paid = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "quantity_paid":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Quantity Paid" ).

      IF btt_auth_detail.quantity_paid < 0
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "quantity_paid":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Quantity Paid , The Quantity Paid must be greater than or equal to 0 .").

      /*
        Copay validations
      */
      IF btt_auth_detail.copay_auth = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid.*/
                              INPUT "Co-payment Auth,Please specify a valid copayment auth value.").

      IF btt_auth_detail.copay_auth < 0 THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid.*/
                              INPUT "Co-payment Auth,Please specify a value greater/equal to zero.").

      IF  btt_auth_detail.copay_auth   > btt_auth_detail.amount_auth
      AND btt_auth_detail.amount_auth <> 0 THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid.*/
                              INPUT "Co-payment Auth, Co-payment Auth can not be greater than the amount authorised.").

      IF btt_auth_detail.copay_auth_% = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_detail.line_number,
	                            INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid.*/
			                        INPUT "Co-payment Auth  Percentage, Please specify a valid copay auth percentage.":U).

      IF btt_auth_detail.copay_auth_% < 0
      OR btt_auth_detail.copay_auth_% > 100  THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid.*/
                              INPUT "Co-payment Auth  Percentage,Copay Auth Percentage can not be less than 0 or greater than 100.").

      IF btt_auth_detail.copay_override_note <> "":U 
      AND NOT CAN-FIND(note NO-LOCK
                        WHERE note.scheme-code = 0
                          AND note.type        = "AQ":U
                          AND note.key         = btt_auth_detail.copay_override_note) THEN 
      DO:
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                            INPUT btt_auth_detail.auth_detail_obj,
                            INPUT "":U,
                            INPUT "copay_override_note":U,
                            INPUT btt_auth_detail.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* 	The "&1" specified is invalid.*/
                            INPUT "Co-payment Detail Override Reason '" + btt_auth_detail.copay_override_note + "'").
      END.  /* IF btt_auth_detail.copay_override_note <> "":U AND NOT CAN-FIND(note NO-LOCK */

      /*
        Repeat validations
      */
      IF btt_auth_detail.repeat_cycle_auth = ? THEN
	    oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle can't be an unknown value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_auth  < 0 THEN
	    oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_auth":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle can't contain a negative value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_item        = yes
      AND btt_auth_detail.repeat_cycle_auth = 0   THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_item":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle is mandatory if the item is a repeat":U,
                              INPUT "ERR":U).

       IF  btt_auth_detail.repeat_item = no
      AND (btt_auth_detail.repeat_cycle_auth <> 0 OR btt_auth_detail.repeat_cycle_quantity   <> 0
       OR  btt_auth_detail.repeat_cycle_days <> 0 OR btt_auth_detail.repeat_cycle_grace_days <> 0)
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_item":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat item details could not be completed as one of the following fields have already been completed: repeat cycle auth, repeat cycle quantity, repeat cycle days or repeat cycle grace days.":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_paid  = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_paid":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle paid can't be an unknown value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_paid  < 0 THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_paid":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle paid can't contain a negative value.":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_quantity  = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_quantity":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle can't be an unknown value":U,
                              INPUT "ERR").

      IF btt_auth_detail.repeat_cycle_quantity  < 0 THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_quantity":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle quantity can't contain a negative value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_days  = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_days":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle days can’t be an unknown value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_days  < 0 THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_days":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle days can’t contain a negative value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_grace_days  = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_grace_days":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle grace days can't be an unknown value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_grace_days  < 0 THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_grace_days":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle grace days can't contain a negative value":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_cycle_grace_days > btt_auth_detail.repeat_cycle_days THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_cycle_grace_days":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat cycle grace days can't be bigger than the repeat_cycle_days":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_last_claimed_date < btt_auth_detail.start_date THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_last_claimed_date":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat last claimed date can't be before the detail line start date":U,
                              INPUT "ERR":U).

      IF btt_auth_detail.repeat_item = ? THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "repeat_item":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "Repeat item can't be an unknown value":U,
                              INPUT "ERR":U).

    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_detail.record_action) THEN*/
  END. /*FOR EACH btt_auth_detail NO-LOCK:*/

  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oBodyRegion)      THEN DELETE OBJECT oBodyRegion.
                IF VALID-OBJECT(oCopayPayeeDMInd) THEN DELETE OBJECT oCopayPayeeDMInd.  
                IF VALID-OBJECT(oLetterPrintInd)  THEN DELETE OBJECT oLetterPrintInd.
                IF VALID-OBJECT(oErrorObject)     THEN DELETE OBJECT oErrorObject."
   }
&ENDIF
