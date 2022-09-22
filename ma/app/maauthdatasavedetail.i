/* maauthdatasavedetail.i MEDSTAR Medical Aid System
                          Healthcare Auth data access service: Save authorisation detail records
                          (c) Copyright 2017 - 2022
                          MIP Holdings (Pty) Ltd
                          All rights reserved

------------------------------------------------------------------------------
  Purpose   : Save Authorisation Detail Record    
  Parameters:
  Notes     : Data Access Only !!!      
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER btt_auth_detail FOR tt_auth_detail.
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_result.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_auth_error.

&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE BUFFER buf_auth          FOR hat_auth.
  DEFINE BUFFER buf_auth_detail   FOR hat_auth_detail.
  DEFINE BUFFER buf_auth_provider FOR hat_auth_provider.
  DEFINE BUFFER buf_tariff_type   FOR htm_tariff_type.
  
  DEFINE VARIABLE iLineSequence           AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lBufferCompare          AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE cEntity                 AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cRelatedEntity          AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lNewRecordErrorMessage  AS LOGICAL           NO-UNDO INITIAL FALSE.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  IF AVAILABLE btt_auth_detail THEN
  DO:
    {&FindResultRecord}
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_detail.record_action) THEN 
    DO
    TRANSACTION:
      /* 
        Ensure auth obj is populated
      */
      IF btt_auth_detail.auth_obj = 0 OR btt_auth_detail.auth_obj = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Authorisation Detail Parent record (Obj = ":U + STRING(btt_auth_detail.auth_obj) + "),").

      /*
        Ensure that we have a valid parent
      */
      IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
        WHERE buf_auth.auth_obj = btt_auth_detail.auth_obj)THEN
      DO:
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Authorisation Detail Parent record (Obj = ":U + STRING(btt_auth_detail.auth_obj) + ")").
      END. /*IF NOT CAN-FIND(FIND FIRST buf_auth NO-LOCK*/
      
      /*
        Auth provider validation
      */
      FIND buf_auth_provider NO-LOCK
          WHERE buf_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
         NO-ERROR.
         
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
      
      IF NOT AVAILABLE buf_auth_provider
      THEN 
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                              INPUT btt_auth_detail.auth_detail_obj, 
                              INPUT "":U, 
                              INPUT "auth_provider_obj":U,
                              INPUT btt_auth_detail.line_number, 
                              INPUT "MA":U, 
                              INPUT 100,  /* The "&1" specified is invalid */ 
                              INPUT "Authorisation Detail Provider":U).

      /*
        Added By User - Default to YES if empty (Set to NO via Crosswalk)
      */  
      IF btt_auth_detail.added_by_user = ? 
      THEN ASSIGN btt_auth_detail.added_by_user = TRUE.

      /*
        Add to total LOS
      */  
      IF btt_auth_detail.add_to_total_los = ? 
      THEN 
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                              INPUT btt_auth_detail.auth_detail_obj, 
                              INPUT "":U, 
                              INPUT "add_to_total_los":U,
                              INPUT btt_auth_detail.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Add to total LOS,":U).
      /* 
        Tariff type obj    
      */
      IF btt_auth_detail.loc_tariff_type_obj <> 0 AND 
         btt_auth_detail.loc_tariff_type_obj <> ? AND 
         NOT CAN-FIND(FIRST buf_tariff_type NO-LOCK
                      WHERE buf_tariff_type.tariff_type_obj = btt_auth_detail.loc_tariff_type_obj
                        AND buf_tariff_type.acronym_key     = "ma_acTariffTypeCatLOC":U) 
      THEN DO:
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                              INPUT btt_auth_detail.auth_detail_obj,
                              INPUT "":U,
                              INPUT "tariff_type_obj":U,  
                              INPUT btt_auth_detail.line_number,
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2*/
                              INPUT "Non-LOC Tariff type ,Only LOC Tariff types are allowed.":U).
                              
         ASSIGN btt_auth_detail.loc_tariff_type_obj = 0.
      END.  /* IF btt_auth_detail.loc_tariff_type_obj <> 0 AND */
      
      /* 
        Repeat Item
      */
      IF btt_auth_detail.repeat_item = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                              INPUT btt_auth_detail.auth_detail_obj, 
                              INPUT "":U, 
                              INPUT "repeat_item":U,
                              INPUT btt_auth_detail.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "Repeat Item,":U).

      /* 
        PMB Indicator
      */
      IF btt_auth_detail.pmb_indicator = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                              INPUT btt_auth_detail.auth_detail_obj, 
                              INPUT "":U, 
                              INPUT "pmb_indicator":U,
                              INPUT btt_auth_detail.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "PMB Indicator,":U).
      
      /* 
        PMB Pay at cost
      */
      IF btt_auth_detail.pmb_pay_cost = ?
      THEN
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                              INPUT btt_auth_detail.auth_detail_obj, 
                              INPUT "":U, 
                              INPUT "pmb_pay_cost":U,
                              INPUT btt_auth_detail.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */ 
                              INPUT "PMB Pay at cost,":U).
                                               
      
      /* 
        Assign entities used in duplicate checks
      */
      CASE btt_auth_detail.owning_entity_mnemonic:
        WHEN "hlmnl":U  THEN ASSIGN cEntity = "Nappi".
        WHEN "htmtl":U  THEN ASSIGN cEntity = "Tariff".  
        WHEN "hlmcr":U  THEN ASSIGN cEntity = "Basket".
        OTHERWISE            ASSIGN cEntity = "Entity".
      END CASE.

      CASE btt_auth_detail.related_entity_mnemonic:
        WHEN "hlmnl":U  THEN ASSIGN cRelatedEntity = "Nappi".
        WHEN "htmtl":U  THEN ASSIGN cRelatedEntity = "Tariff".  
        WHEN "hlmcr":U  THEN ASSIGN cRelatedEntity = "Basket".
        OTHERWISE            ASSIGN cRelatedEntity = "".
      END CASE.
      
      /*
        Duplicate checks
      */ 
      FIND FIRST buf_auth_detail NO-LOCK
           WHERE buf_auth_detail.auth_obj                = btt_auth_detail.auth_obj 
             AND buf_auth_detail.auth_provider_obj       = btt_auth_detail.auth_provider_obj
             AND buf_auth_detail.owning_entity_mnemonic  = btt_auth_detail.owning_entity_mnemonic
             AND buf_auth_detail.owning_obj              = btt_auth_detail.owning_obj
             AND buf_auth_detail.owning_key              = btt_auth_detail.owning_key
             AND buf_auth_detail.related_entity_mnemonic = btt_auth_detail.related_entity_mnemonic
             AND buf_auth_detail.related_obj             = btt_auth_detail.related_obj
             AND buf_auth_detail.related_key             = btt_auth_detail.related_key
             AND ((buf_auth_detail.start_date            = btt_auth_detail.start_date 
             AND   buf_auth_detail.start_ampm            = btt_auth_detail.start_ampm)
              OR  (buf_auth_detail.start_date           <= btt_auth_detail.start_date
             AND  (buf_auth_detail.end_date              > btt_auth_detail.start_date OR buf_auth_detail.end_date = ?))
              OR  (buf_auth_detail.end_date              = btt_auth_detail.start_date 
             AND   buf_auth_detail.end_ampm             <= btt_auth_detail.start_ampm)
              OR  (buf_auth_detail.start_date 	         = btt_auth_detail.end_date 
             AND   buf_auth_detail.start_ampm 	        >= btt_auth_detail.end_ampm) 
              OR  (btt_auth_detail.end_date             <> ? 
             AND   buf_auth_detail.end_date              = btt_auth_detail.end_date 
             AND   buf_auth_detail.end_ampm              = btt_auth_detail.end_ampm) 
              OR  (buf_auth_detail.start_date           >= btt_auth_detail.start_date
             AND  (btt_auth_detail.end_date              > buf_auth_detail.start_date OR btt_auth_detail.end_date = ?)))
             AND buf_auth_detail.auth_detail_obj        <> btt_auth_detail.auth_detail_obj NO-ERROR.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE buf_auth_detail 
      THEN DO:
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                              INPUT btt_auth_detail.auth_detail_obj, 
                              INPUT "":U, 
                              INPUT "":U,
                              INPUT btt_auth_detail.line_number, 
                              INPUT "MA":U, 
                              INPUT 125,  /* &1 already exists with &2 */ 
                              INPUT "Authorisation Detail,":U + cEntity  + " ":U                + 
                                     btt_auth_detail.owning_alt_value    + " effective from ":U +
                                     STRING(buf_auth_detail.start_date, "9999/99/99") +
                                     (IF buf_auth_detail.end_date <> ? 
                                      THEN " to "  + STRING(buf_auth_detail.end_date, "9999/99/99") 
                                      ELSE "") +
                                     (IF cRelatedEntity <> "":U 
                                      THEN " Related Entity: " + cRelatedEntity + " Related Value: "  + btt_auth_detail.related_value
                                      ELSE "")).

      END. /* IF AVAILABLE buf_auth_detail  */
      ELSE DO:
        /*
          Nappi/Basket Duplicate check
        */ 
        IF LOOKUP(cEntity,"Nappi,Basket":U) <> 0
        THEN 
          FIND FIRST buf_auth_detail NO-LOCK
               WHERE buf_auth_detail.auth_obj                = btt_auth_detail.auth_obj 
                 AND buf_auth_detail.auth_provider_obj       = btt_auth_detail.auth_provider_obj
                 AND buf_auth_detail.related_entity_mnemonic = btt_auth_detail.owning_entity_mnemonic
                 AND buf_auth_detail.related_obj             = btt_auth_detail.owning_obj
                 AND buf_auth_detail.related_key             = btt_auth_detail.owning_key
                 AND ((buf_auth_detail.start_date            = btt_auth_detail.start_date 
                 AND   buf_auth_detail.start_ampm            = btt_auth_detail.start_ampm)
                  OR  (buf_auth_detail.start_date           <= btt_auth_detail.start_date
                 AND  (buf_auth_detail.end_date              > btt_auth_detail.start_date OR buf_auth_detail.end_date = ?))
                  OR  (buf_auth_detail.end_date              = btt_auth_detail.start_date 
                 AND   buf_auth_detail.end_ampm             <= btt_auth_detail.start_ampm)
                  OR  (buf_auth_detail.start_date            = btt_auth_detail.end_date 
                 AND   buf_auth_detail.start_ampm           >= btt_auth_detail.end_ampm) 
                  OR  (btt_auth_detail.end_date             <> ? 
                 AND   buf_auth_detail.end_date              = btt_auth_detail.end_date 
                 AND   buf_auth_detail.end_ampm              = btt_auth_detail.end_ampm) 
                  OR  (buf_auth_detail.start_date           >= btt_auth_detail.start_date
                 AND  (btt_auth_detail.end_date              > buf_auth_detail.start_date OR btt_auth_detail.end_date = ?)))
                 AND buf_auth_detail.auth_detail_obj        <> btt_auth_detail.auth_detail_obj NO-ERROR.
        ELSE IF LOOKUP(cRelatedEntity,"Nappi,Basket":U) <> 0
        THEN 
          FIND FIRST buf_auth_detail NO-LOCK
               WHERE buf_auth_detail.auth_obj                = btt_auth_detail.auth_obj 
                 AND buf_auth_detail.auth_provider_obj       = btt_auth_detail.auth_provider_obj
                 AND buf_auth_detail.owning_entity_mnemonic  = btt_auth_detail.related_entity_mnemonic
                 AND buf_auth_detail.owning_obj              = btt_auth_detail.related_obj
                 AND buf_auth_detail.owning_key              = btt_auth_detail.related_key
                 AND ((buf_auth_detail.start_date            = btt_auth_detail.start_date 
                 AND   buf_auth_detail.start_ampm            = btt_auth_detail.start_ampm)
                  OR  (buf_auth_detail.start_date           <= btt_auth_detail.start_date
                 AND  (buf_auth_detail.end_date              > btt_auth_detail.start_date OR buf_auth_detail.end_date = ?))
                  OR  (buf_auth_detail.end_date              = btt_auth_detail.start_date 
                 AND   buf_auth_detail.end_ampm             <= btt_auth_detail.start_ampm)
                  OR  (buf_auth_detail.start_date            = btt_auth_detail.end_date 
                 AND   buf_auth_detail.start_ampm           >= btt_auth_detail.end_ampm) 
                  OR  (btt_auth_detail.end_date             <> ? 
                 AND   buf_auth_detail.end_date              = btt_auth_detail.end_date 
                 AND   buf_auth_detail.end_ampm              = btt_auth_detail.end_ampm) 
                  OR  (buf_auth_detail.start_date           >= btt_auth_detail.start_date
                 AND  (btt_auth_detail.end_date              > buf_auth_detail.start_date OR btt_auth_detail.end_date = ?)))
                 AND buf_auth_detail.auth_detail_obj        <> btt_auth_detail.auth_detail_obj NO-ERROR.
            
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        IF AVAILABLE buf_auth_detail 
        THEN DO:
          CASE buf_auth_detail.owning_entity_mnemonic:
              WHEN "hlmnl":U  THEN ASSIGN cEntity = "Nappi".
              WHEN "htmtl":U  THEN ASSIGN cEntity = "Tariff".  
              WHEN "hlmcr":U  THEN ASSIGN cEntity = "Basket".
              OTHERWISE            ASSIGN cEntity = "Entity".
          END CASE.
          
          CASE buf_auth_detail.related_entity_mnemonic:
            WHEN "hlmnl":U  THEN ASSIGN cRelatedEntity = "Nappi".
            WHEN "htmtl":U  THEN ASSIGN cRelatedEntity = "Tariff".  
            WHEN "hlmcr":U  THEN ASSIGN cRelatedEntity = "Basket".
            OTHERWISE            ASSIGN cRelatedEntity = "".
          END CASE.
          
          oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                                INPUT btt_auth_detail.auth_detail_obj, 
                                INPUT "":U, 
                                INPUT "":U,
                                INPUT btt_auth_detail.line_number, 
                                INPUT "MA":U, 
                                INPUT 125,  /* &1 already exists with &2 */ 
                                INPUT cEntity + " Detail ":U +
                                      (IF cRelatedEntity <> "":U 
                                       THEN buf_auth_detail.owning_alt_value + ",":U + cRelatedEntity + " ":U + CAPS(buf_auth_detail.related_value)
                                       ELSE ",":U + cEntity + " ":U + CAPS(buf_auth_detail.owning_alt_value))    
                                       + " effective from ":U + STRING(buf_auth_detail.start_date, "9999/99/99") +
                                      (IF buf_auth_detail.end_date <> ? 
                                       THEN " to "  + STRING(buf_auth_detail.end_date, "9999/99/99") 
                                       ELSE "")).
            
        END. /* IF AVAILABLE buf_auth_detail  */
      END. /* ELSE DO - IF AVAILABLE buf_auth_detail */
      
      IF oErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U) 
      THEN
        RETURN.
        
      FIND buf_auth_detail EXCLUSIVE-LOCK
           WHERE buf_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj
        NO-ERROR NO-WAIT.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
      /*
        The record is locked by another user or process
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
      DO:
        {&ResetError}
        
        ASSIGN tt_auth_result.records_locked = tt_auth_result.records_locked + 1.
                     
        oErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                              INPUT btt_auth_detail.auth_detail_obj, 
                              INPUT "":U, 
                              INPUT btt_auth_detail.line_number, 
                              INPUT "MA":U, 
                              INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again */
                              INPUT "Authorisation Detail":U).
                         
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/
                    
      /*
        Record not found so we are creating
      */
      IF cls.miperror:getMessageGroupNumber() = "PROGRESS:138":U  THEN
      DO:
        {&ResetError}
                            
        IF NOT oErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj,"":U)
        THEN DO:   
          CREATE buf_auth_detail.
          ASSIGN lNewRecordErrorMessage = TRUE.
        END. /* IF NOT oErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj,"":U) */
      END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:138":U  THEN*/ 
       
      IF AVAILABLE buf_auth_detail 
      AND NOT oErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj,"":U) THEN
      DO:
        ASSIGN tt_auth_result.records_modified = tt_auth_result.records_modified + 1 WHEN NOT NEW buf_auth_detail.
        
        BUFFER-COPY btt_auth_detail 
             EXCEPT btt_auth_detail.auth_detail_obj
                 TO buf_auth_detail.        
        
        /*
          If this was a create we need to copy the newly generated obj back to the temp table
        */
        ASSIGN btt_auth_detail.auth_detail_obj = buf_auth_detail.auth_detail_obj.
        
        VALIDATE buf_auth_detail.          
        
        FIND CURRENT buf_auth_detail NO-LOCK.

        IF lNewRecordErrorMessage THEN
          FOR EACH tt_auth_error EXCLUSIVE-LOCK
             WHERE tt_auth_error.owning_entity_mnemonic BEGINS "hatad"
               AND tt_auth_error.owning_obj <= 0.00:

            ASSIGN tt_auth_error.owning_obj = btt_auth_detail.auth_detail_obj.
          END. /* FOR EACH tt_auth_error EXCLUSIVE-LOCK */
                     
      END. /*IF AVAILABLE buf_auth_detail AND NOT oErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj,"":U) THEN*/    
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_detail.record_action) THEN*/
    
    IF btt_auth_detail.record_action = "Delete":U 
    AND btt_auth_detail.auth_detail_obj > 0 THEN 
      RUN _deleteAuthDetail IN TARGET-PROCEDURE ( INPUT btt_auth_detail.auth_detail_obj, INPUT btt_auth_detail.line_number, INPUT btt_auth_detail.rate_change, INPUT-OUTPUT oErrorObject ).

  END. /*IF AVAILABLE btt_auth_detail THEN*/

  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

