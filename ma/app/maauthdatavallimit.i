/* maauthdatavallimit.i MEDSTAR Medical Aid System
                        Healthcare Auth data access service: Validate Auth Limit Buffer
                        (c) Copyright 2017 - 2022
                        MIP Holdings (Pty) Ltd
                        All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Limit Buffer        
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
  
  DEFINE BUFFER btt_auth_limit     FOR tt_auth_limit.
  DEFINE BUFFER buf_auth           FOR hat_auth.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  FOR EACH btt_auth_limit NO-LOCK:
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_limit.record_action) THEN
    DO:
      
      /* Validate the Auth Obj */
      IF btt_auth_limit.auth_obj <> 0.00 THEN 
      DO:
        IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
                        WHERE buf_auth.auth_obj = btt_auth_limit.auth_obj) THEN
        DO:
          oErrorObject:addError(INPUT "hatal":U, 
                                INPUT btt_auth_limit.auth_limit_obj, 
                                INPUT "":U, 
                                INPUT "auth_obj":U,
                                INPUT btt_auth_limit.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Header (Obj = ":U + STRING(btt_auth_limit.auth_obj) + ")").
          RETURN.
        END. /*IF NOT CAN-FIND(FIRST buf_auth NO-LOCK*/
      END. /*IF btt_autt_limit.auth_obj <> 0.00*/

      IF btt_auth_limit.option_code = 0 THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "option_code":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Option Code,").
          RETURN.
      END. /* IF tt_auth_limit.option_code = "":U THEN */
      ELSE DO:
        FIND FIRST buf_auth NO-LOCK
             WHERE buf_auth.auth_obj  = btt_auth_limit.auth_obj
         NO-ERROR.
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }


        IF AVAILABLE buf_auth AND NOT CAN-FIND(FIRST memsch NO-LOCK
                                               WHERE memsch.mem-num     = buf_auth.mem_num
                                                 AND memsch.scheme-code = btt_auth_limit.option_code)
        THEN DO:
           oErrorObject:addError(INPUT "hatal":U, 
                                 INPUT btt_auth_limit.auth_limit_obj, 
                                 INPUT "":U, 
                                 INPUT "option_code":U,
                                 INPUT btt_auth_limit.line_number, 
                                 INPUT "MA":U, 
                                 INPUT 100,  /* The "&1" specified is invalid */
                                 INPUT "Option Code").
          RETURN.
        END. /* IF AVAILABLE buf_auth AND NOT CAN-FIND(FIRST memsch NO-LOCK */
       
      END. /* ELSE DO: */

      IF btt_auth_limit.dependant = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "dependant":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Dependant,").
          RETURN.
      END. /* IF btt_auth_limit.dependant = 0 THEN */
      ELSE DO:
        IF btt_auth_limit.rate_per = "F":U 
        THEN ASSIGN btt_auth_limit.dependant = 00.
        
        IF btt_auth_limit.rate_per = "D":U
        THEN DO:
          FIND FIRST buf_auth NO-LOCK
               WHERE buf_auth.auth_obj = btt_auth_limit.auth_obj
           NO-ERROR.
          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE buf_auth 
          THEN ASSIGN btt_auth_limit.dependant = buf_auth.dependant.
        END. /* IF btt_auth_limit.rate_per = "D":U */
      END. /* ELSE DO: */
      
      IF btt_auth_limit.limit_# = ? THEN
      DO:
         oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "limit_#":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Limit #,").
          RETURN.
      END. /* IF btt_auth_limit.limit_# = ? THEN */

      IF btt_auth_limit.claim_code = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "claim_code":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Claim Code,").
          RETURN.
      END. /* IF btt_auth_limit.claim_code ? THEN */

      IF btt_auth_limit.claim_type = "":U OR btt_auth_limit.claim_type = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "claim_type":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Claim Type,").
          RETURN.
      END. /* IF btt_auth_limit.claim_type = "":U THEN */

      IF btt_auth_limit.rate_per = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "rate_per":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Rate Per,").
          RETURN.
      END. /* IF btt_auth_limit.rate_per = "":U THEN */
      ELSE DO:
        IF LOOKUP(btt_auth_limit.rate_per, "F,D":U) = 0 THEN
        DO:
          oErrorObject:addError(INPUT "hatal":U, 
                                 INPUT btt_auth_limit.auth_limit_obj, 
                                 INPUT "":U, 
                                 INPUT "rate_per":U,
                                 INPUT btt_auth_limit.line_number, 
                                 INPUT "MA":U, 
                                 INPUT 100,  /* The "&1" specified is invalid */
                                 INPUT "Rate Per").
          RETURN.
        END. /* IF LOOKUP(btt_auth_limit.rate_per, "F,D":U) = 0 THEN */
      END. /* ELSE DO: */

      /* Validate the owning Entiy mnemonic */
      IF btt_auth_limit.owning_entity_mnemonic = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "owning_entiy_mnemonic":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Authorisation Limit Owning Entiy Mnemonic,":U).
      END. /* IF btt_auth_limit.ownig_entity_mnemonic = "":U THEN */
      ELSE DO:
        
        IF LOOKUP(btt_auth_limit.owning_entity_mnemonic, "hatau,hatap,hatad":U) = 0 THEN 
        DO: 
          oErrorObject:addError(INPUT "hatal":U, 
                                INPUT btt_auth_limit.auth_limit_obj, 
                                INPUT "":U, 
                                INPUT "owning_entity_mnemonic":U,
                                INPUT btt_auth_limit.line_number, 
                                INPUT "MA":U, 
                                INPUT 112,  /* The &1 specified is invalid. &2 */
                                INPUT "Authorisation Limit: " + STRING(btt_auth_limit.owning_entity_mnemonic) +
                                      ",See acronym setup 'Authorisation limit Entities' for list of valid values.").                  
          RETURN.
        END. /* IF LOOKUP(STRING(btt_auth_limit.owning_entity_mnemonic), cValueList, {&Delim-ValueList}) = 0 THEN */
      END. /* ELSE DO: */
      
      IF btt_auth_limit.owning_obj <> 0.00 AND btt_auth_limit.owning_entity_mnemonic <> "":U THEN
      DO:
        CASE btt_auth_limit.owning_entity_mnemonic:
            WHEN "hatau":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth NO-LOCK
                              WHERE hat_auth.auth_obj = btt_auth_limit.owning_obj)
              THEN
                oErrorObject:addError(INPUT "hatal":U, 
                                      INPUT btt_auth_limit.auth_limit_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_obj":U,
                                      INPUT btt_auth_limit.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Header (Obj = ":U + STRING(btt_auth_limit.owning_obj) + ")").
            END. /* WHEN "hatad":U THEN */
            
            WHEN "hatap":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth_provider NO-LOCK
                              WHERE hat_auth_provider.auth_provider_obj = btt_auth_limit.owning_obj)
              THEN
                oErrorObject:addError(INPUT "hatal":U, 
                                      INPUT btt_auth_limit.auth_limit_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_obj":U,
                                      INPUT btt_auth_limit.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Provider (Obj = ":U + STRING(btt_auth_limit.owning_obj) + ")").
            END. /* WHEN "hatap":U THEN */

            WHEN "hatad":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth_detail NO-LOCK
                              WHERE hat_auth_detail.auth_detail_obj = btt_auth_limit.owning_obj)
              THEN
                oErrorObject:addError(INPUT "hatal":U, 
                                      INPUT btt_auth_limit.auth_limit_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_obj":U,
                                      INPUT btt_auth_limit.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Detail (Obj = ":U + STRING(btt_auth_limit.owning_obj) + ")").
            END. /* WHEN "hatad":U THEN */
        END CASE.
      END. /* IF btt_auth_limit.owning_obj <> 0.00 AND btt_auth_limit.owning_entity_mnemonic <> "":U THEN */

      IF btt_auth_limit.owning_alt_value = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj, 
                              INPUT "":U, 
                              INPUT "owning_alt_value":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Authorisation Limit Entity Code,":U).

      END. /* IF btt_auth_limit.owning_alt_value = "":U THEN */
      ELSE DO:
        IF btt_auth_limit.owning_obj <> 0.00 AND btt_auth_limit.owning_entity_mnemonic <> "":U
        THEN DO:
          CASE btt_auth_limit.owning_entity_mnemonic:
            WHEN "hatau":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth NO-LOCK
                              WHERE hat_auth.auth_obj = btt_auth_limit.owning_obj
                                AND hat_auth.auth_num = btt_auth_limit.owning_alt_value)
              THEN
                oErrorObject:addError(INPUT "hatal":U, 
                                      INPUT btt_auth_limit.auth_limit_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_alt_value":U,
                                      INPUT btt_auth_limit.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Number: ":U + STRING(btt_auth_limit.owning_alt_value)).
            END. /* WHEN "hatad":U THEN */
            
            WHEN "hatap":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth_provider NO-LOCK
                              WHERE hat_auth_provider.auth_provider_obj = btt_auth_limit.owning_obj
                                AND hat_auth_provider.doc_num           = INTEGER(btt_auth_limit.owning_alt_value))
              THEN
                oErrorObject:addError(INPUT "hatal":U, 
                                      INPUT btt_auth_limit.auth_limit_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_alt_value":U,
                                      INPUT btt_auth_limit.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Provider: " + STRING(btt_auth_limit.owning_alt_value)).
            END. /* WHEN "hatap":U THEN */
          
            WHEN "hatad":U THEN
            DO:
              IF NOT CAN-FIND(FIRST hat_auth_detail NO-LOCK
                              WHERE hat_auth_detail.auth_detail_obj  = btt_auth_limit.owning_obj
                                AND hat_auth_detail.owning_alt_value = btt_auth_limit.owning_alt_value)
              THEN
                oErrorObject:addError(INPUT "hatal":U, 
                                      INPUT btt_auth_limit.auth_limit_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_alt_value":U,
                                      INPUT btt_auth_limit.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The "&1" specified is invalid */
                                      INPUT "Authorisation Detail: ":U + STRING(btt_auth_limit.owning_alt_value)).
            END. /* WHEN "hatad":U THEN */
          END CASE.
        END. /* IF btt_auth_limit.owning_obj <> 0.00 AND btt_auth_limit.owning_entity_mnemonic <> "":U */
      END. /* ELSE DO: */
      
      IF btt_auth_limit.limit_avail = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj,
                              INPUT "":U,
                              INPUT "limit_avail":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U,
                              INPUT 112, /* The &1 specified is invalid. &2 */
                              INPUT "Limit available, Value cannot contain an unknown." ).
      END. /* IF btt_auth_limit.limit_avail = ? THEN */

      IF btt_auth_limit.limit_used = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj,
                              INPUT "":U,
                              INPUT "limit_used":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U,
                              INPUT 112, /* The &1 specified is invalid. &2 */
                              INPUT "Limit used, Value cannot contain an unknown." ).
      END. /* IF btt_auth_limit.limit_used = ? THEN */

      IF btt_auth_limit.limit_reserved = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatal":U, 
                              INPUT btt_auth_limit.auth_limit_obj,
                              INPUT "":U,
                              INPUT "limit_reserved":U,
                              INPUT btt_auth_limit.line_number, 
                              INPUT "MA":U,
                              INPUT 112, /* The &1 specified is invalid. &2 */
                              INPUT "Limit Reserved, Value cannot contain an unknown.").
      END. /* IF btt_auth_limit.limit_reserved = ? THEN */
      
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_limit.record_action) THEN*/
  END. /*FOR EACH btt_auth_limit NO-LOCK:*/
 
  /* Clean up */
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject)   THEN DELETE OBJECT oErrorObject NO-ERROR."}

&ENDIF





