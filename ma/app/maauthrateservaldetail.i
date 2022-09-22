/* maauthrateservaldetail.i  MEDSTAR Medical Aid System
                             Validate Auth Rate Detail
                             (c) Copyright 2020
                             MIP Holdings (Pty) Ltd
                             All rights reserved
*/      
                
  DEFINE PARAMETER BUFFER btt_auth_rate_detail FOR tt_auth_rate_detail.
  DEFINE INPUT-OUTPUT PARAMETER oplFailureOccurred AS LOGICAL.
  
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject   NO-UNDO.  

  DEFINE VARIABLE cKeylist            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cCodelist           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cDescriptionList    AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cLabellist          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cSequenceList       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatusDescr        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRuleValue          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValueList          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE iCnt                AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lMandatory          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidRule          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE cOEM                AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cAuthRateDetailEntities  AS CHARACTER   NO-UNDO.
  
  DEFINE BUFFER buf_auth_rate_detail  FOR hac_auth_rate_detail.
  DEFINE BUFFER buf_auth_rate_control FOR hac_auth_rate_control.
  
  /* Make sure we have a valid buffer before we go any further */
  IF NOT AVAILABLE btt_auth_rate_detail
  THEN 
    { mip/inc/mipthrowerror.i 'ma_MsgErr' ? "'Could not validate Authorisation Rate Detail as no buffer is available.'" }
    
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rate_error:HANDLE) .

  IF btt_auth_rate_detail.auth_rate_control_obj = 0 OR btt_auth_rate_detail.auth_rate_control_obj = ? 
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.
    
    oErrorObject:addError(INPUT "hacrd":U,
                          INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                          INPUT "":U,
                          INPUT "auth_rate_control_obj":U,
                          INPUT btt_auth_rate_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Control Obj,":U).
  END. /* IF btt_auth_rate_detail.auth_rate_control_obj = 0 OR btt_auth_rate_detail.auth_rate_control_obj = ? */

  IF NOT CAN-FIND(FIRST hac_auth_rate_control 
                  WHERE hac_auth_rate_control.auth_rate_control_obj = btt_auth_rate_detail.auth_rate_control_obj)
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrd":U, 
                          INPUT btt_auth_rate_detail.auth_rate_detail_obj, 
                          INPUT "":U, 
                          INPUT "auth_rate_control_obj":U,
                          INPUT btt_auth_rate_detail.line_number, 
                          INPUT "MA":U, 
                          INPUT 107,  /* Could not find a "&1" record using "&2" */
                          INPUT "Auth Rate Control,":U + STRING(btt_auth_rate_detail.auth_rate_control_obj)).       
  END. /* IF NOT CAN-FIND(FIRST hac_auth_rate_control */
    
  /*
    Owning entity validation
  */
  IF btt_auth_rate_detail.owning_entity_mnemonic <> "":U THEN
  DO:

    
    IF btt_auth_rate_detail.owning_entity_mnemonic = "hacar":U 
    THEN ASSIGN cAuthRateDetailEntities = "hacarf":U.
    ELSE IF btt_auth_rate_detail.owning_entity_mnemonic = "hlmcr":U 
    THEN ASSIGN cAuthRateDetailEntities = "hlmcrc":U.
    ELSE ASSIGN cAuthRateDetailEntities = btt_auth_rate_detail.owning_entity_mnemonic.

    FIND FIRST mic_acronym NO-LOCK
         WHERE mic_acronym.category_key  = "ma_acAuthRateDetailEntities":U
           AND mic_acronym.acronym_value = cAuthRateDetailEntities NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE mic_acronym
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.
  
      oErrorObject:addError(INPUT "hacrd":U,
                            INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                            INPUT "":U,
                            INPUT "owning_entity_mnemonic":U,
                            INPUT btt_auth_rate_detail.line_number,
                            INPUT "MA":U,
                            INPUT 100,  /* The "&1" specified is invalid*/
                            INPUT "Owning entity: ":U + btt_auth_rate_detail.owning_entity_mnemonic).    
    END.

    mipEnv:miDBEntity:focusTable(btt_auth_rate_detail.owning_entity_mnemonic).
    
    IF NOT mipEnv:miDBEntity:InFocus THEN
    DO:
      oErrorObject:addError(INPUT "hacrd":U, 
                            INPUT btt_auth_rate_detail.auth_rate_detail_obj, 
                            INPUT "":U, 
                            INPUT "owning_entity_mnemonic":U,
                            INPUT btt_auth_rate_detail.line_number, 
                            INPUT "MA":U, 
                            INPUT 107,  /* Could not find a "&1" record using "&2" */
                            INPUT "valid owning entity,":U + btt_auth_rate_detail.owning_entity_mnemonic).                  
                            
      RETURN.                      
    END. /*IF NOT mipEnv:miDBEntity:InFocus THEN*/
    
    ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U THEN 
                          mipEnv:miDBEntity:findRecord(btt_auth_rate_detail.owning_obj)
                       ELSE 
                          mipEnv:miDBEntity:findRecord(btt_auth_rate_detail.owning_key)).
                       
    IF NOT mipEnv:miDBEntity:RecordAvailable 
    THEN DO:
      ASSIGN oplFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacrd":U, 
                            INPUT btt_auth_rate_detail.auth_rate_detail_obj, 
                            INPUT "":U, 
                            INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                   THEN "btt_auth_rate_detail.owning_obj":U 
                                   ELSE "btt_auth_rate_detail.owning_key":U),
                            INPUT btt_auth_rate_detail.line_number, 
                            INPUT "MA":U, 
                            INPUT 107,  /* Could not find a "&1" record using "&2" */ 
                            INPUT mipEnv:miDBEntity:TableLabel + ",":U + (IF btt_auth_rate_detail.owning_key <> "" 
                                                                          THEN btt_auth_rate_detail.owning_key 
                                                                          ELSE btt_auth_rate_detail.owning_alt_value)).                  
    END. /* IF NOT mipEnv:miDBEntity:RecordAvailable */
    
    IF btt_auth_rate_detail.owning_entity_mnemonic = "hacar":U
    THEN DO:
      IF btt_auth_rate_detail.flag_value = ""
      THEN DO:
        ASSIGN oplFailureOccurred = TRUE.
    
        oErrorObject:addError(INPUT "hacrd":U,
                              INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                              INPUT "":U,
                              INPUT "flag_value":U,
                              INPUT btt_auth_rate_detail.line_number,
                              INPUT "MA":U,
                              INPUT 111,  /* The &1 must be specified. &2 */
                              INPUT "Flag value,":U) .                             
        
      END. /* IF btt_auth_rate_detail.flag_value = "" */
      ELSE DO:
        IF mipEnv:miDBEntity:EntityMnemonic = "hacar":U
        THEN DO:  
          mipEnv:miDBEntity:FocusField("rule_valid_values":U). 
          
          IF mipEnv:miDBEntity:FieldInFocus 
          THEN DO:
            IF mipEnv:miDBEntity:FieldBufferValue <> "" AND 
               LOOKUP(btt_auth_rate_detail.flag_value,mipEnv:miDBEntity:FieldBufferValue,"|") = 0
            THEN DO:
              ASSIGN oplFailureOccurred = TRUE.
              
              oErrorObject:addError(INPUT "hacrd":U, 
                                    INPUT btt_auth_rate_detail.auth_rate_detail_obj, 
                                    INPUT "":U, 
                                    INPUT "flag_value":U,
                                    INPUT btt_auth_rate_detail.line_number, 
                                    INPUT "MA":U, 
                                    INPUT 112,  /* The "&1" specified is invalid. &2 */
                                    INPUT "Flag value,Valid values: " + mipEnv:miDBEntity:FieldBufferValue).
            END. /* IF mipEnv:miDBEntity:FieldBufferValue <> "" AND  */
          END. /* IF mipEnv:miDBEntity:FieldInFocus */
        END. /* IF mipEnv:miDBEntity:EntityMnemonic = "hacar" */
      END. /* ELSE DO - IF btt_auth_rate_detail.flag_value = "" */ 
    END. /* cOEM = "hacar" */
  END. /*IF btt_auth_rate_detail.owning_entity_mnemonic <> "":U THEN*/  
  ELSE DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrd":U,
                          INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                          INPUT "":U,
                          INPUT "owning_entity_mnemonic":U,
                          INPUT btt_auth_rate_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Owning Entity Mnemonic,":U ).
  END.

  /* Ensure that a valid auth rate code has been specified */
  IF btt_auth_rate_detail.owning_alt_value = "":U
  THEN DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrd":U,
                          INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                          INPUT "":U,
                          INPUT "owning_alt_value":U,
                          INPUT btt_auth_rate_detail.line_number,
                          INPUT "MA":U,                          
                          INPUT 111,  /* The &1 must be specified. &2*/
                          INPUT "Owning Alt Value,":U ).
  END. /* btt_auth_rate_detail.owning_alt_value = "":U THEN */

  /*Ensure that a valid auth type effective date has been specified*/
  IF btt_auth_rate_detail.effective_date = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrd":U,
                          INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                          INPUT "":U,
                          INPUT "effective_date":U,
                          INPUT btt_auth_rate_detail.line_number,
                          INPUT "MA":U,
                          INPUT 111,  /* The &1 must be specified. &2 */
                          INPUT "Auth Rate Detail Effective Date,":U) .
  END. /* btt_auth_rate_detail.effective_date = ? THEN */

  /* If an end date is supplied, ensure that is not before the effective date */
  IF btt_auth_rate_detail.end_date <> ? AND
     btt_auth_rate_detail.end_date < btt_auth_rate_detail.effective_date THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.

    oErrorObject:addError(INPUT "hacrd":U,
                          INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                          INPUT "":U,
                          INPUT "end_date":U,
                          INPUT btt_auth_rate_detail.line_number,
                          INPUT "MA":U,
                          INPUT 11,  /* The End Date &1 cannot be before the Effective/Start Date &2 */
                          INPUT STRING(btt_auth_rate_detail.end_date,"9999/99/99") + "," + STRING(btt_auth_rate_detail.effective_date,"9999/99/99")).
  END. /*IF btt_auth_rate_detail.end_date <> ? AND btt_auth_rate_detail.end_date < btt_auth_rate_detail.effective_date THEN*/
  
  IF btt_auth_rate_detail.exclusion = ? THEN
  DO:
    ASSIGN oplFailureOccurred = TRUE.
  
    oErrorObject:addError(INPUT "hacrd":U, 
                          INPUT btt_auth_rate_detail.auth_rate_control_obj, 
                          INPUT "":U, 
                          INPUT "exclusion":U,
                          INPUT btt_auth_rate_detail.line_number, 
                          INPUT "MA":U, 
                          INPUT 112,  /* The "&1" specified is invalid. &2 */
                          INPUT "Exclusion,Cannot be Unknown").
  
  END. /* IF btt_auth_rate_detail.exclusion = ? THEN */
  
  FIND FIRST buf_auth_rate_control NO-LOCK
       WHERE buf_auth_rate_control.auth_rate_control_obj = btt_auth_rate_detail.auth_rate_control_obj NO-ERROR.

  { mip/inc/mipthrowerror.i &ResetIgnoredErrors = TRUE &IgnoreErrors = 'PROGRESS:565' } 

  IF AVAILABLE buf_auth_rate_control
  AND buf_auth_rate_control.cpt_relative_value_type <> "":U THEN
  DO:

    IF btt_auth_rate_detail.override_ars_rate = "":U 
    THEN
      ASSIGN oplFailureOccurred = TRUE
             lSuccess           = oErrorObject:addError(INPUT "hacrd":U,
                                                        INPUT btt_auth_rate_detail.auth_rate_detail_obj,
                                                        INPUT "":U,
                                                        INPUT "override_ars_rate":U,
                                                        INPUT btt_auth_rate_detail.line_number,
                                                        INPUT "MA":U,
                                                        INPUT 111,  /* The &1 must be specified. &2 */
                                                        INPUT "Auth Rate Detail Override ARS Rate,Relative value type has been specified on the auth rate control header":U) .
    ELSE DO:
      FIND FIRST arsrate NO-LOCK 
           WHERE arsrate.ars-rate = btt_auth_rate_detail.override_ars_rate NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}

      IF NOT AVAILABLE arsrate 
      THEN
        ASSIGN oplFailureOccurred = TRUE
               lSuccess           = oErrorObject:addError(INPUT "hacrc":U, 
                                                          INPUT btt_auth_rate_detail.auth_rate_control_obj, 
                                                          INPUT "":U, 
                                                          INPUT "override_ars_rate":U,
                                                          INPUT btt_auth_rate_detail.line_number, 
                                                          INPUT "MA":U, 
                                                          INPUT 100,  /* The "&1" specified is invalid */
                                                          INPUT "Override ARS Rate: ":U + STRING(btt_auth_rate_detail.override_ars_rate)).      

     IF	btt_auth_rate_detail.exclusion 
     THEN
       ASSIGN oplFailureOccurred  = TRUE 
               lSuccess           = oErrorObject:addError(INPUT "hacrc":U, 
                                                          INPUT btt_auth_rate_detail.auth_rate_control_obj, 
                                                          INPUT "":U, 
                                                          INPUT "override_ars_rate":U,
                                                          INPUT btt_auth_rate_detail.line_number, 
                                                          INPUT "MA":U, 
                                                          INPUT 111,  /* The "&1" specified is invalid . &2*/
                                                          INPUT "Exclusion, The Relative value type has been specified on the auth rate control and exclusions do not apply. ":U ).      

    END. /* ELSE DO */
   
  END. /* IF AVAILABLE buf_auth_rate_control AND buf_auth_rate_control.cpt_relative_value_type <> "":U THEN */
  
  IF AVAILABLE buf_auth_rate_control 
  AND (btt_auth_rate_detail.effective_date < buf_auth_rate_control.effective_date
    OR btt_auth_rate_detail.effective_date > buf_auth_rate_control.end_date )
  THEN
    ASSIGN oplFailureOccurred = TRUE 
           lSuccess           = oErrorObject:addError(INPUT "hacrc":U, 
                                                      INPUT btt_auth_rate_detail.auth_rate_control_obj, 
                                                      INPUT "":U, 
                                                      INPUT "effective_date":U,
                                                      INPUT btt_auth_rate_detail.line_number, 
                                                      INPUT "MA":U, 
                                                      INPUT 111,  /* The "&1" specified is invalid . &2*/
                                                      INPUT "Effective Date , The effective date must be within the auth rate control period.":U).

  IF AVAILABLE buf_auth_rate_control 
  AND (btt_auth_rate_detail.end_date < buf_auth_rate_control.effective_date
    OR btt_auth_rate_detail.end_date > buf_auth_rate_control.end_date )
  THEN
    ASSIGN oplFailureOccurred = TRUE
           lSuccess           = oErrorObject:addError(INPUT "hacrc":U, 
                                                      INPUT btt_auth_rate_detail.auth_rate_control_obj, 
                                                      INPUT "":U, 
                                                      INPUT "effective_date":U,
                                                      INPUT btt_auth_rate_detail.line_number, 
                                                      INPUT "MA":U, 
                                                      INPUT 111,  /* The "&1" specified is invalid . &2 */
                                                      INPUT "End Date , The end date must be within the auth rate control period":U).

{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
        
&ENDIF

