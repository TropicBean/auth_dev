/* maauthdatavalcopay.i MEDSTAR Medical Aid System
                        Healthcare Auth data access service: Validate Auth Co-payment Buffer
                        (c) Copyright 2017 - 2021
                        MIP Holdings (Pty) Ltd
                        All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Co-payment Buffer        
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack            
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 010195 &THEN 

  DEFINE VARIABLE oErrorObject      AS cls.maerrorobject NO-UNDO.

  DEFINE VARIABLE cKeylist          AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cCodelist         AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cLabellist        AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cDescriptionList  AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cSequenceList     AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cValueList        AS CHARACTER         NO-UNDO.
  
  DEFINE BUFFER btt_auth_copay     FOR tt_auth_copay.
  DEFINE BUFFER buf_auth           FOR hat_auth.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  FOR EACH btt_auth_copay NO-LOCK:
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_copay.record_action) THEN
    DO:
      
      /* Validate the Auth Obj */
      IF btt_auth_copay.auth_obj <> 0.00 THEN 
      DO:
        IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
                        WHERE buf_auth.auth_obj = btt_auth_copay.auth_obj) THEN
        DO:
          oErrorObject:addError(INPUT "hatcp":U, 
                                INPUT btt_auth_copay.auth_copay_obj, 
                                INPUT "":U, 
                                INPUT "auth_obj":U,
                                INPUT btt_auth_copay.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Header (Obj = ":U + STRING(btt_auth_copay.auth_obj) + ")").
          RETURN.
        END. /*IF NOT CAN-FIND(FIRST buf_auth NO-LOCK*/
      END. /*IF btt_autt_copay.auth_obj <> 0.00*/

      /* Validate the Auth Copay Type Obj */
      IF btt_auth_copay.auth_copay_type_obj <> 0.00 THEN
      DO:
        IF NOT CAN-FIND(FIRST ham_auth_copay_type NO-LOCK
                        WHERE ham_auth_copay_type.auth_copay_type_obj = btt_auth_copay.auth_copay_type_obj) THEN
        DO:
          oErrorObject:addError(INPUT "hatcp":U, 
                                INPUT btt_auth_copay.auth_copay_obj, 
                                INPUT "":U, 
                                INPUT "auth_copay_type_obj":U,
                                INPUT btt_auth_copay.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Co-payment Type (Obj = ":U + STRING(btt_auth_copay.auth_copay_type_obj) + ")").
          RETURN.
        END. /* IF NOT CAN-FIND(FIRST ham_auth_copay_type NO-LOCK */
      END. /* IF btt_auth_copay.auth_copay_type_obj <> 0.00 THEN */

      /* Validate the owning Entiy mnemonic */
      IF btt_auth_copay.owning_entity_mnemonic = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj, 
                              INPUT "":U, 
                              INPUT "owning_entiy_mnemonic":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Authorisation Co-payment Owning Entiy Mnemonic,":U).
      END. /* IF btt_auth_copay.ownig_entity_mnemonic = "":U THEN */
      ELSE DO:
        
        mipEnv:miUtility:getStatusOrAcronymDetails(INPUT  "Acronym":U, 
                                                   INPUT  "ma_acAuthCopayEntities":U,
                                                   OUTPUT cKeylist,
                                                   OUTPUT cCodelist,
                                                   OUTPUT cLabellist,
                                                   OUTPUT cDescriptionList,
                                                   OUTPUT cSequenceList,
                                                   OUTPUT cValueList).

        IF LOOKUP(STRING(btt_auth_copay.owning_entity_mnemonic), cValueList, {&Delim-ValueList}) = 0 THEN 
        DO: 
          oErrorObject:addError(INPUT "hatcp":U, 
                                INPUT btt_auth_copay.auth_copay_obj, 
                                INPUT "":U, 
                                INPUT "owning_entity_mnemonic":U,
                                INPUT btt_auth_copay.line_number, 
                                INPUT "MA":U, 
                                INPUT 112,  /* The &1 specified is invalid. &2 */
                                INPUT "Authorisation Co-payment: ":U + STRING(btt_auth_copay.owning_entity_mnemonic) +
                                      ",See acronym setup 'Authorisation Copay Entities' for list of valid values.").                  
          RETURN.
        END. /* IF LOOKUP(STRING(btt_auth_copay.owning_entity_mnemonic), cValueList, {&Delim-ValueList}) = 0 THEN */
      END. /* ELSE DO: */

      /* Validate Owning Obj and Owning Key */
      IF btt_auth_copay.owning_obj = 0.00 AND btt_auth_copay.owning_key = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj, 
                              INPUT "":U, 
                              INPUT "owning_obj":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Authorisation Co-payment Owning Obj or Owning Key,":U).
      END. /* IF btt_auth_copay.owning_obj = 0.00 THEN */

      IF btt_auth_copay.owning_obj <> 0.00 AND btt_auth_copay.owning_entity_mnemonic <> "":U THEN
      DO:
        CASE btt_auth_copay.owning_entity_mnemonic:
            WHEN "hatap":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth_provider NO-LOCK
                              WHERE hat_auth_provider.auth_provider_obj = btt_auth_copay.owning_obj)
              THEN
                oErrorObject:addError(INPUT "hatcp":U, 
                                      INPUT btt_auth_copay.auth_copay_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_obj":U,
                                      INPUT btt_auth_copay.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Provider (Obj = ":U + STRING(btt_auth_copay.owning_obj) + ")").
            END. /* WHEN "hatap":U THEN */

            WHEN "hatad":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth_detail NO-LOCK
                              WHERE hat_auth_detail.auth_detail_obj = btt_auth_copay.owning_obj)
              THEN
                oErrorObject:addError(INPUT "hatcp":U, 
                                      INPUT btt_auth_copay.auth_copay_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_obj":U,
                                      INPUT btt_auth_copay.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Detail (Obj = ":U + STRING(btt_auth_copay.owning_obj) + ")").
            END. /* WHEN "hatad":U THEN */
        END CASE.
      END. /* IF btt_auth_copay.owning_obj <> 0.00 AND btt_auth_copay.owning_entity_mnemonic <> "":U THEN */

      IF btt_auth_copay.owning_alt_value = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj, 
                              INPUT "":U, 
                              INPUT "owning_alt_value":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Authorisation Co-payment Entity Code,":U).

      END. /* IF btt_auth_copay.owning_alt_value = "":U THEN */

      IF btt_auth_copay.amount = ? 
      THEN
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj,
                              INPUT "":U,
                              INPUT "amount":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Authorisation Co-payment Amount" ).

      IF btt_auth_copay.amount < 0 
      THEN
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj,
                              INPUT "":U,
                              INPUT "amount":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Authorisation Co-payment Amount, The Authorisation Co-payment Amount must be greater than or equal to 0 .").

      IF btt_auth_copay.amount_% = ? 
      THEN
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj,
                              INPUT "":U,
                              INPUT "amount_%":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Authorisation Co-payment Amount Percentage" ).

      IF btt_auth_copay.amount_% < 0 OR btt_auth_copay.amount_% > 100 
      THEN
        oErrorObject:addError(INPUT "hatcp":U, 
                              INPUT btt_auth_copay.auth_copay_obj,
                              INPUT "":U,
                              INPUT "amount":U,
                              INPUT btt_auth_copay.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Authorisation Co-payment Amount Percentage, The Authorisation Co-payment Amount Percentage must be greater than 0 and less to 100 .").



    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_copay.record_action) THEN*/
  END. /*FOR EACH btt_auth_copay NO-LOCK:*/
  
  /* Clean up */
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject)   THEN DELETE OBJECT oErrorObject NO-ERROR."}

&ENDIF




