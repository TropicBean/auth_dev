/* maauthgrpsrvstacksaveagdetail.i  MEDSTAR Medical Aid System
                         Procedure _SaveAuthGroupDetail for maauthgroupservicestack.p
                         (c) Copyright 1990 - 2021
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/ 

&IF {&DBDFMA} >= 10195 &THEN    
  
  DEFINE VARIABLE cMessage            AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cUsedInRateControl  AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL           NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE iError              AS INTEGER           NO-UNDO.
  DEFINE VARIABLE iSubPrType          AS INTEGER           NO-UNDO.
  
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject NO-UNDO.
  
  DEFINE BUFFER buf_auth_group_detail FOR ham_auth_group_detail.
  DEFINE BUFFER btt_auth_group_result FOR tt_auth_group_result.
  DEFINE BUFFER btt_auth_group_error  FOR tt_auth_group_error.
  DEFINE BUFFER btt_auth_group        FOR tt_auth_group.
  DEFINE BUFFER btt_auth_group_detail FOR tt_auth_group_detail.
  DEFINE BUFFER buf_tariff_link       FOR htm_tariff_link.
 
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_group_error:HANDLE).

    FIND FIRST btt_auth_group_result EXCLUSIVE-LOCK NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_group_result 
    THEN
      CREATE btt_auth_group_result.
    
    RECORD-BLK:                             
    FOR EACH btt_auth_group_detail EXCLUSIVE-LOCK
          BY btt_auth_group_detail.auth_group_detail_obj DESCENDING:
      
      ASSIGN 
         lFailureOccurred = FALSE       
         btt_auth_group_result.records_processed = btt_auth_group_result.records_processed + 1.
      
      IF btt_auth_group_detail.record_action = "MODIFY":U THEN
      DO:
        /*Ensure that a valid header has been specified*/
        IF btt_auth_group_detail.auth_group_obj = 0.00 THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
          
          oErrorObject:addError(INPUT "hamad":U, 
                                INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                INPUT "":U, 
                                INPUT "auth_group_obj":U,
                                INPUT btt_auth_group_detail.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Auth Group Obj (":U + STRING(btt_auth_group_detail.auth_group_obj) + ")":U).                  
        END. /*IF btt_auth_group_detail.auth_group_obj = 0.00 THEN*/
        
        /*Ensure that an effective date has been specified*/
        IF btt_auth_group_detail.effective_date = ? THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
          
          oErrorObject:addError(INPUT "hamad":U, 
                                INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                INPUT "":U, 
                                INPUT "effective_date":U,
                                INPUT btt_auth_group_detail.line_number, 
                                INPUT "MA":U, 
                                INPUT 111,  /* The &1 must be specified. &2 */
                                INPUT "Auth group detail effective date,":U).                  
        END. /*IF btt_auth_group_detail.effective_date = ? THEN*/
        
        /* Validate entity mnemonics - tariff / basket / nappi */
        IF LOOKUP (btt_auth_group_detail.owning_entity_mnemonic,"prtype":U) = 0 THEN
        DO:
          mipEnv:miDBEntity:focusTable(btt_auth_group_detail.owning_entity_mnemonic) NO-ERROR.
          
          IF NOT mipEnv:miDBentity:Infocus THEN 
          DO:
            ASSIGN lFailureOccurred = TRUE.
                
            oErrorObject:addError(INPUT "hamad":U, 
                                  INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                  INPUT "":U, 
                                  INPUT "owning_entity_mnemonic":U,
                                  INPUT btt_auth_group_detail.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 107,  /* Could not find a "&1" record using "&2" */
                                  INPUT "valid Owning Entity,":U + btt_auth_group_detail.owning_entity_mnemonic).                  
                                 
          END. /* IF NOT mipEnv:miDBentity:Infocus */
          ELSE DO:
            IF btt_auth_group_detail.owning_obj <> 0.00 THEN 
            DO:
              mipEnv:miDBEntity:findRecord(btt_auth_group_detail.owning_obj).
          
              IF btt_auth_group_detail.owning_alt_value <> "" THEN
                ASSIGN cMessage = btt_auth_group_detail.owning_alt_value .
              ELSE IF btt_auth_group_detail.owning_key <> "" THEN
                ASSIGN cMessage = btt_auth_group_detail.owning_key.
              ELSE 
                ASSIGN cMessage = STRING(btt_auth_group_detail.owning_obj).
            
              IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
              DO:
                ASSIGN lFailureOccurred = TRUE.
          
                oErrorObject:addError(INPUT "hamad":U, 
                                      INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_alt_value":U,
                                      INPUT btt_auth_group_detail.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 107,  /* Could not find a "&1" record using "&2" */
                                      INPUT mipEnv:miDBEntity:TableLabel + ",":U + cMessage).                  
                
              END. /* IF NOT mipEnv:miDBEntity:RecordAvailable */
            END.  /* IF btt_auth_group_detail.owning_obj <> 0.00 THEN */
            ELSE IF btt_auth_group_detail.owning_key <> "" THEN
            DO:
              mipEnv:miDBEntity:findRecord(btt_auth_group_detail.owning_key).
            
              IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
              DO:
                ASSIGN lFailureOccurred = TRUE.
          
                oErrorObject:addError(INPUT "hamad":U, 
                                      INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "owning_alt_value":U,
                                      INPUT btt_auth_group_detail.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 107,  /* Could not find a "&1" record using "&2" */
                                      INPUT mipEnv:miDBEntity:TableLabel + ",":U + STRING(btt_auth_group_detail.owning_key)).                  
                
              END. /* IF NOT mipEnv:miDBEntity:RecordAvailable */
            END.  /* ELSE IF btt_auth_group_detail.owning_key <> "" THEN */
            ELSE DO:
              ASSIGN lFailureOccurred = TRUE.
          
              CASE btt_auth_group_detail.owning_entity_mnemonic:
                WHEN "htmtl":U OR WHEN "tariff":U THEN cMessage = "Tariff Code".
                WHEN "hlmcr":U THEN cMessage = "Basket Code".
                WHEN "hlmnl":U THEN cMessage = "Nappi Code".
              END CASE.
          
              oErrorObject:addError(INPUT "hamad":U,
                                    INPUT btt_auth_group_detail.auth_group_detail_obj,
                                    INPUT "":U,
                                    INPUT "owning_alt_value":U,
                                    INPUT btt_auth_group_detail.line_number,
                                    INPUT "MA":U,
                                    INPUT 111,  /* The &1 must be specified. &2 */
                                    INPUT cMessage + ",":U).
            END.  /* ELSE - ELSE IF btt_auth_group_detail.owning_key <> "" THEN */
            
            IF mipEnv:miDBEntity:RecordAvailable  
            AND ( btt_auth_group_detail.owning_entity_mnemonic = "htmtl":U 
                  OR btt_auth_group_detail.owning_entity_mnemonic = "tariff":U ) THEN 
            DO:
              IF btt_auth_group_detail.owning_alt_value <> "" THEN
              DO:
                FIND FIRST buf_tariff_link NO-LOCK 
                     WHERE buf_tariff_link.tariff_code =  btt_auth_group_detail.owning_alt_value 
                       AND buf_tariff_link.base_rate   =  btt_auth_group_detail.base_rate        
                       AND buf_tariff_link.ars_rate    =  btt_auth_group_detail.ars_rate         
                       AND buf_tariff_link.pr_type     =  btt_auth_group_detail.pr_type          
                  NO-ERROR.
                
                { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
          
                IF NOT AVAILABLE buf_tariff_link THEN
                DO:
                  ASSIGN lFailureOccurred = TRUE
                         cMessage = CHR(10) + "Code:"      + btt_auth_group_detail.owning_alt_value 
                                  + CHR(10) + "Base Rate:" + btt_auth_group_detail.base_rate        
                                  + CHR(10) + "ARS Rate:"  + btt_auth_group_detail.ars_rate         
                                  + CHR(10) + "PR Type:"   + btt_auth_group_detail.pr_type.
          
                  oErrorObject:addError(INPUT "hamad":U, 
                                        INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                        INPUT "":U, 
                                        INPUT "owning_alt_value":U,
                                        INPUT btt_auth_group_detail.line_number, 
                                        INPUT "MA":U, 
                                        INPUT 107,  /* Could not find a "&1" record using "&2" */
                                        INPUT mipEnv:miDBEntity:TableLabel + ",":U + cMessage).
            
                END. /*IF NOT AVAILABLE buf_tariff_link THEN*/
                ELSE IF buf_tariff_link .tariff_link_obj <> btt_auth_group_detail.owning_obj THEN
                  ASSIGN btt_auth_group_detail.owning_obj = buf_tariff_link.tariff_link_obj.

              END. /*IF btt_auth_group_detail.owning_obj <= 0 THEN*/
            END. /*IF mipEnv:miDBEntity:RecordAvailable  AND ... */
          END. /* ELSE - IF NOT mipEnv:miDBentity:Infocus */
        END.  /* IF LOOKUP (btt_auth_group_detail.owning_entity_mnemonic,"prtype":U) = 0 THEN */
        ELSE DO:
          IF btt_auth_group_detail.pr_type = "":U
          OR btt_auth_group_detail.pr_type = "000":U THEN
          DO:
            ASSIGN lFailureOccurred = TRUE.

            oErrorObject:addError(INPUT "hamad":U,
                                  INPUT btt_auth_group_detail.auth_group_detail_obj,
                                  INPUT "":U,
                                  INPUT "pr_type":U,
                                  INPUT btt_auth_group_detail.line_number,
                                  INPUT "MA":U,
                                  INPUT 111,  /* The &1 must be specified. &2 */
                                  INPUT "Auth group detail discipline,":U).

          END.  /* IF btt_auth_group_detail.pr_type = "":U */
          ELSE IF NOT CAN-FIND(FIRST prtype NO-LOCK
              WHERE prtype.pr-type = INTEGER(btt_auth_group_detail.pr_type)) THEN
          DO:
            ASSIGN lFailureOccurred = TRUE.

            oErrorObject:addError(INPUT "hamad":U,
                                  INPUT btt_auth_group_detail.auth_group_detail_obj,
                                  INPUT "":U,
                                  INPUT "pr_type":U,
                                  INPUT btt_auth_group_detail.line_number,
                                  INPUT "MA":U,
                                  INPUT 107,  /* Could not find a "&1" record using "&2" */
                                  INPUT "Discipline,":U + btt_auth_group_detail.pr_type).
          END.  /* ELSE IF NOT CAN-FIND(FIRST prtype NO-LOCK... */
        END.  /* ELSE - IF LOOKUP (btt_auth_group_detail.owning_entity_mnemonic,"prtype":U) = 0 THEN */

        IF lFailureOccurred
        THEN 
          NEXT RECORD-BLK.
          
        FIND FIRST buf_auth_group_detail EXCLUSIVE-LOCK
             WHERE buf_auth_group_detail.auth_group_detail_obj = btt_auth_group_detail.auth_group_detail_obj
          NO-ERROR NO-WAIT.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE }
        
        /*The auth group detail record is locked by another user or process*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN 
             lFailureOccurred = TRUE
             btt_auth_group_result.records_locked = btt_auth_group_result.records_locked + 1.
                       
          oErrorObject:addError(INPUT "hamad":U, 
                                INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_group_detail.line_number, 
                                INPUT "MA":U, 
                                INPUT 200,   /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth group detail:":U + STRING(btt_auth_group_detail.auth_group_detail_obj)).
                           
        END. /*IF cls.miperror:getMessagedetailNumber() = "PROGRESS:445":U  THEN*/

        cls.miperror:resetError().
        
        /*Duplicate check*/ 
        IF CAN-FIND(FIRST buf_auth_group_detail NO-LOCK
                    WHERE buf_auth_group_detail.auth_group_obj         = btt_auth_group_detail.auth_group_obj
                      AND buf_auth_group_detail.owning_entity_mnemonic = btt_auth_group_detail.owning_entity_mnemonic
                      AND buf_auth_group_detail.owning_obj             = btt_auth_group_detail.owning_obj
                      AND buf_auth_group_detail.owning_key             = btt_auth_group_detail.owning_key
                      AND buf_auth_group_detail.pr_type                = btt_auth_group_detail.pr_type
                      AND buf_auth_group_detail.sub_pr_type            = btt_auth_group_detail.sub_pr_type
                      AND buf_auth_group_detail.effective_date         = btt_auth_group_detail.effective_date
                      AND buf_auth_group_detail.auth_group_detail_obj  <> btt_auth_group_detail.auth_group_detail_obj) 
        OR CAN-FIND(FIRST buf_auth_group_detail NO-LOCK
                    WHERE buf_auth_group_detail.auth_group_obj         = btt_auth_group_detail.auth_group_obj
                      AND buf_auth_group_detail.owning_entity_mnemonic = btt_auth_group_detail.owning_entity_mnemonic
                      AND buf_auth_group_detail.owning_obj             = btt_auth_group_detail.owning_obj
                      AND buf_auth_group_detail.owning_key             = btt_auth_group_detail.owning_key
                      AND buf_auth_group_detail.pr_type                = btt_auth_group_detail.pr_type
                      AND buf_auth_group_detail.sub_pr_type            = btt_auth_group_detail.sub_pr_type
                      AND buf_auth_group_detail.end_date               = btt_auth_group_detail.end_date
                      AND buf_auth_group_detail.auth_group_detail_obj  <> btt_auth_group_detail.auth_group_detail_obj)  
        THEN
        DO:
          ASSIGN lFailureOccurred = TRUE
                 cMessage         = CHR(10) + "Entity: " + 
                                    (IF mipEnv:miDBEntity:TableLabel = "Crosswalk" THEN
                                      "Basket"
                                    ELSE
                                      mipEnv:miDBEntity:TableLabel ).

          IF btt_auth_group_detail.owning_alt_value <> ? THEN
            ASSIGN cMessage = cMessage + CHR(10) +
                              "Item: " + btt_auth_group_detail.owning_alt_value .

          IF btt_auth_group_detail.pr_type <> ? THEN
            ASSIGN cMessage = cMessage   + CHR(10) +
                              "PRType: " + STRING(btt_auth_group_detail.pr_type).

          IF btt_auth_group_detail.sub_pr_type > 0 THEN
            ASSIGN cMessage = cMessage      + CHR(10) +
                              "SubPRType: " + STRING(btt_auth_group_detail.sub_pr_type).

          IF btt_auth_group_detail.effective_date <> ? THEN
            ASSIGN cMessage = cMessage    + CHR(10) +
                              "EffDate: " + STRING(btt_auth_group_detail.effective_date,"9999/99/99").

          IF btt_auth_group_detail.end_date <> ? THEN
            ASSIGN cMessage = cMessage    + CHR(10) +
                              "EndDate: " + STRING(btt_auth_group_detail.end_date,"9999/99/99"). 

          oErrorObject:addError(INPUT "hamad":U, 
                                INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                INPUT "":U, 
                                INPUT "":U,
                                INPUT btt_auth_group_detail.line_number, 
                                INPUT "MA":U, 
                                INPUT 125,   /* &1 already exists with &2 */
                                INPUT "Auth group Detail,":U + cMessage).

        END. /*IF CAN-FIND(FIRST buf_auth_group_detail NO-LOCK*/

        IF btt_auth_group_detail.sub_pr_type > 0 THEN
        DO:
          ASSIGN iSubPrType = INTEGER( btt_auth_group_detail.pr_type).

          IF NOT CAN-FIND(FIRST subdisc NO-LOCK
                          WHERE subdisc.pr-type      = iSubPrType
                            AND subdisc.subdisp-code = btt_auth_group_detail.sub_pr_type) 
                           
          THEN DO:
            ASSIGN lFailureOccurred = TRUE.
            
            oErrorObject:addError(INPUT "hamad":U,
                                  INPUT btt_auth_group_detail.auth_group_detail_obj,
                                  INPUT "":U,
                                  INPUT "sub_pr_type":U,
                                  INPUT btt_auth_group_detail.line_number,
                                  INPUT "MA":U,
                                  INPUT 100,  /* The "&1" specified is invalid */
                                  INPUT "Sub Practice Type : ":U + STRING(btt_auth_group_detail.sub_pr_type,"999")).
          END.  /* IF NOT CAN-FIND(FIRST prtype NO-LOCK */
        END.  /* IF btt_auth_group_detail.sub_pr_type > 0 THEN */
              
        IF CAN-FIND(FIRST buf_auth_group_detail NO-LOCK
                    WHERE buf_auth_group_detail.auth_group_detail_obj <> btt_auth_group_detail.auth_group_detail_obj
                      AND buf_auth_group_detail.auth_group_obj         = btt_auth_group_detail.auth_group_obj
                      AND buf_auth_group_detail.owning_entity_mnemonic = btt_auth_group_detail.owning_entity_mnemonic
                      AND buf_auth_group_detail.owning_obj             = btt_auth_group_detail.owning_obj
                      AND buf_auth_group_detail.owning_key             = btt_auth_group_detail.owning_key
                      AND buf_auth_group_detail.pr_type                = btt_auth_group_detail.pr_type
                      AND buf_auth_group_detail.sub_pr_type            = btt_auth_group_detail.sub_pr_type
                      AND buf_auth_group_detail.effective_date         < btt_auth_group_detail.effective_date
                      AND (   buf_auth_group_detail.end_date            = ?
                           OR buf_auth_group_detail.end_date            > btt_auth_group_detail.effective_date)
                    ) THEN 
        DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hamad":U,
                                INPUT btt_auth_group_detail.auth_group_detail_obj,
                                INPUT "":U,
                                INPUT "effective_date":U,
                                INPUT btt_auth_group_detail.line_number,
                                INPUT "Record dates are overlapping an existing record's dates",
                                INPUT "ERR":U).
        END.  /* IF CAN-FIND(FIRST buf_auth_group_detail NO-LOCK */

        IF  btt_auth_group_detail.end_date <> ? 
        AND btt_auth_group_detail.end_date < btt_auth_group_detail.effective_date 
        THEN DO:
          ASSIGN lFailureOccurred = TRUE.
          
          oErrorObject:addError(INPUT "hamad":U, 
                                INPUT btt_auth_group_detail.auth_group_detail_obj, 
                                INPUT "":U, 
                                INPUT "effective_date":U,
                                INPUT btt_auth_group_detail.line_number, 
                                INPUT "Effective date (" + STRING(btt_auth_group_detail.effective_date) + ") can not be greater than the end date(" + STRING(btt_auth_group_detail.end_date) + ")":U,  
                                INPUT "ERR":U ).  
        END.  /* IF btt_auth_group_detail.end_date <> ? */

        IF CAN-FIND(FIRST buf_auth_group_detail NO-LOCK
                    WHERE buf_auth_group_detail.auth_group_detail_obj <> btt_auth_group_detail.auth_group_detail_obj
                      AND buf_auth_group_detail.auth_group_obj         = btt_auth_group_detail.auth_group_obj
                      AND buf_auth_group_detail.owning_entity_mnemonic = btt_auth_group_detail.owning_entity_mnemonic
                      AND buf_auth_group_detail.owning_obj             = btt_auth_group_detail.owning_obj
                      AND buf_auth_group_detail.owning_key             = btt_auth_group_detail.owning_key
                      AND buf_auth_group_detail.pr_type                = btt_auth_group_detail.pr_type
                      AND buf_auth_group_detail.sub_pr_type            = btt_auth_group_detail.sub_pr_type
                      AND buf_auth_group_detail.effective_date         > btt_auth_group_detail.effective_date
                      AND buf_auth_group_detail.effective_date         < btt_auth_group_detail.end_date
                    )
        AND btt_auth_group_detail.end_date <> ?
        OR (btt_auth_group_detail.end_date = ? 
            AND 
            CAN-FIND(FIRST buf_auth_group_detail NO-LOCK
                    WHERE buf_auth_group_detail.auth_group_detail_obj <> btt_auth_group_detail.auth_group_detail_obj
                      AND buf_auth_group_detail.auth_group_obj         = btt_auth_group_detail.auth_group_obj
                      AND buf_auth_group_detail.owning_entity_mnemonic = btt_auth_group_detail.owning_entity_mnemonic
                      AND buf_auth_group_detail.owning_obj             = btt_auth_group_detail.owning_obj
                      AND buf_auth_group_detail.owning_key             = btt_auth_group_detail.owning_key
                      AND buf_auth_group_detail.pr_type                = btt_auth_group_detail.pr_type
                      AND buf_auth_group_detail.sub_pr_type            = btt_auth_group_detail.sub_pr_type
                      AND buf_auth_group_detail.effective_date         > btt_auth_group_detail.effective_date)
            ) THEN 
        DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hamad":U,
                                INPUT btt_auth_group_detail.auth_group_detail_obj,
                                INPUT "":U,
                                INPUT "end_date":U,
                                INPUT btt_auth_group_detail.line_number,
                                INPUT "Record dates are overlapping an existing record's dates",
                                INPUT "ERR":U).
        END. /* IF CAN-FIND(FIRST buf_auth_group_detail NO-LOCK */

        IF btt_auth_group_detail.end_date <> ?
        AND CAN-FIND(FIRST buf_auth_group_detail NO-LOCK
                 WHERE buf_auth_group_detail.auth_group_detail_obj <> btt_auth_group_detail.auth_group_detail_obj
                   AND buf_auth_group_detail.auth_group_obj         = btt_auth_group_detail.auth_group_obj
                   AND buf_auth_group_detail.owning_entity_mnemonic = btt_auth_group_detail.owning_entity_mnemonic
                   AND buf_auth_group_detail.owning_obj             = btt_auth_group_detail.owning_obj
                   AND buf_auth_group_detail.owning_key             = btt_auth_group_detail.owning_key
                   AND buf_auth_group_detail.pr_type                = btt_auth_group_detail.pr_type
                   AND buf_auth_group_detail.sub_pr_type            = btt_auth_group_detail.sub_pr_type
                   AND buf_auth_group_detail.effective_date         > btt_auth_group_detail.end_date
                   AND buf_auth_group_detail.end_date               < btt_auth_group_detail.end_date) THEN 
        DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hamad":U,
                                INPUT btt_auth_group_detail.auth_group_detail_obj,
                                INPUT "":U,
                                INPUT "effective_date":U,
                                INPUT btt_auth_group_detail.line_number,
                                INPUT "Record dates are overlapping an existing record's dates",
                                INPUT "ERR":U).
        END.  /* IF btt_auth_group_detail.end_date <> ? AND... */

        /*Record not found so we are creating*/
        IF NOT lFailureOccurred 
        AND NOT AVAILABLE buf_auth_group_detail
        THEN
          CREATE buf_auth_group_detail. 

        IF AVAILABLE buf_auth_group_detail AND NOT lFailureOccurred THEN
        DO:        
          /*An existing record is being updated*/
          IF btt_auth_group_detail.auth_group_detail_obj > 0 
          THEN 
            ASSIGN btt_auth_group_result.records_modified = btt_auth_group_result.records_modified + 1.
          ELSE 
            ASSIGN btt_auth_group_result.records_created  = btt_auth_group_result.records_created  + 1.
                             
          BUFFER-COPY btt_auth_group_detail 
               EXCEPT btt_auth_group_detail.auth_group_detail_obj 
                   TO buf_auth_group_detail.        
          
          ASSIGN 
             btt_auth_group_detail.auth_group_detail_obj = buf_auth_group_detail.auth_group_detail_obj
             btt_auth_group_detail.record_action         = "":U.
          
          VALIDATE buf_auth_group_detail.          
          
          FIND CURRENT buf_auth_group_detail NO-LOCK.
          
        END. /* IF AVAILABLE buf_auth_group_detail AND NOT lFailureOccurred THEN */
      END. /*IF btt_auth_group_detail.record_action = "MODIFY":U THEN*/
      
      IF btt_auth_group_detail.record_action = "DELETE":U THEN
      DO:    
        /*
          Check if there isn't an auth rate control record that  
          already uses this auth group and discipline setup.
        */
        ASSIGN cUsedInRateControl = "".
        IF btt_auth_group_detail.owning_entity_mnemonic = "prtype":U THEN
          FOR EACH hac_auth_rate_provider NO-LOCK
            WHERE hac_auth_rate_provider.auth_group_obj        = btt_auth_group_detail.auth_group_obj
            AND   hac_auth_rate_provider.auth_rate_pr_type     = INTEGER(btt_auth_group_detail.pr_type)
            AND  (hac_auth_rate_provider.auth_rate_sub_pr_type = btt_auth_group_detail.sub_pr_type
             OR   btt_auth_group_detail.sub_pr_type = 0):

            FIND hac_auth_rate_control NO-LOCK
              WHERE hac_auth_rate_control.auth_rate_control_obj = hac_auth_rate_provider.auth_rate_control_obj
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = FALSE }

            IF AVAILABLE hac_auth_rate_control
            AND LOOKUP(hac_auth_rate_control.rate_control_code,cUsedInRateControl) = 0 THEN
              ASSIGN cUsedInRateControl = cUsedInRateControl + "," + hac_auth_rate_control.rate_control_code.

          END.  /* FOR EACH hac_auth_rate_provider NO-LOCK */

        IF cUsedInRateControl <> "" THEN
        DO:
          ASSIGN cUsedInRateControl = SUBSTRING(cUsedInRateControl,2)
                 cMessage           = "Delete not allowed. Discipline" +
                                       IF btt_auth_group_detail.sub_pr_type <> 0
                                       THEN "/Sub-Discipline [&1/&2]"
                                       ELSE " [&1]"
                 cMessage           = cMessage + " is currently in use by Rate &3 [&4]."
                 cMessage           = IF NUM-ENTRIES(cUsedInRateControl) > 1 
                                      THEN SUBSTITUTE(cMessage,STRING(btt_auth_group_detail.pr_type,'999'),
                                                      STRING(btt_auth_group_detail.sub_pr_type,'999'),
                                                      "Controls",cUsedInRateControl)
                                      ELSE SUBSTITUTE(cMessage,STRING(btt_auth_group_detail.pr_type,'999'),
                                                      STRING(btt_auth_group_detail.sub_pr_type,'999'),
                                                      "Control",cUsedInRateControl).

          oErrorObject:addError(INPUT "hamad":U,
                                INPUT btt_auth_group_detail.auth_group_detail_obj,
                                INPUT "":U,
                                INPUT "pr_type":U,
                                INPUT btt_auth_group_detail.line_number,
                                INPUT cMessage,
                                INPUT "ERR":U).
        END.  /* IF cUsedInRateControl <> "" THEN */
        ELSE IF NOT lFailureOccurred THEN
        DO:
          /*This routine will ensure that all dependencies will also be removed*/
          RUN _deleteAuthGroupDetail IN TARGET-PROCEDURE (INPUT btt_auth_group_detail.auth_group_detail_obj).
          
          IF NOT CAN-FIND(FIRST btt_auth_group_error NO-LOCK
                          WHERE btt_auth_group_error.owning_entity_mnemonic = "hamad":U
                            AND btt_auth_group_error.owning_obj             = btt_auth_group_detail.auth_group_detail_obj)
          THEN
            DELETE btt_auth_group_detail.
        END.  /* IF NOT lFailureOccurred THEN */
      END. /*IF btt_auth_group_detail.record_action = "DELETE":U THEN*/           
    END. /*FOR EACH btt_auth_group_detail EXCLUSIVE-LOCK:*/      
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
  
  { mip/inc/mipcatcherror.i
       &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }    
&ENDIF
