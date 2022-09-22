/* maauthdatavalmcsavings.i MEDSTAR Medical Aid System
                            Healthcare Auth data access service: Validate Auth Savings Buffer
                            (c) Copyright 2017 - 2018
                            MIP Holdings (Pty) Ltd
                            All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Managed Care Savings Buffer        
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack            
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 010195 &THEN 

  DEFINE VARIABLE oSavingsType      AS cls.mipacronym    NO-UNDO.
  DEFINE VARIABLE oSavingsStatus    AS cls.mipacronym    NO-UNDO.
  DEFINE VARIABLE oErrorObject      AS cls.maerrorobject NO-UNDO.
  
  DEFINE VARIABLE cKeylist          AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cCodelist         AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cLabellist        AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cDescriptionList  AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cSequenceList     AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cValueList        AS CHARACTER         NO-UNDO.
  
  DEFINE BUFFER btt_auth_mc_savings FOR tt_auth_mc_savings.
  DEFINE BUFFER buf_auth            FOR hat_auth.
  DEFINE BUFFER buf_note            FOR note.
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  FOR EACH btt_auth_mc_savings NO-LOCK:
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_mc_savings.record_action) THEN
    DO:
      
      /* Savings type validation */
      IF btt_auth_mc_savings.saving_type <> "":U THEN  
      DO:
        ASSIGN oSavingsType = NEW cls.mipacronym(?, FALSE, "ma_acAuthSavingsType":U, ?).
      
        oSavingsType:focusAcronym("KEY":U, btt_auth_mc_savings.saving_type) NO-ERROR.
      
        IF NOT oSavingsType:AcronymInFocus THEN
        DO:
          oErrorObject:addError(INPUT "hatms":U, 
                                INPUT btt_auth_mc_savings.auth_mc_savings_obj, 
                                INPUT "":U, 
                                INPUT "saving_type":U,
                                INPUT btt_auth_mc_savings.line_number, 
                                INPUT "MA":U, 
                                INPUT 112,  /* The "&1" specified is invalid. &2 */
                                INPUT "Authorisation savings type: ":U + btt_auth_mc_savings.saving_type +
                                      ",See acronym setup 'Authorisation Savings Type' for list of valid types.").                  
          RETURN.
        END. /*IF NOT oSavingsType:AcronymInFocus THEN*/
      END. /*IF btt_auth_mc_savings.saving_type <> "":U THEN*/
      
      /* Saving Status */
      mipEnv:miUtility:getStatusOrAcronymDetails(INPUT  "Acronym":U, 
                                                 INPUT  "ma_acAuthSavingStatus":U,
                                                 OUTPUT cKeylist,
                                                 OUTPUT cCodelist,
                                                 OUTPUT cLabellist,
                                                 OUTPUT cDescriptionList,
                                                 OUTPUT cSequenceList,
                                                 OUTPUT cValueList).
      
      IF LOOKUP(STRING(btt_auth_mc_savings.saving_status), cCodelist, {&Delim-ValueList}) = 0 THEN 
      DO: 
        oErrorObject:addError(INPUT "hatms":U, 
                              INPUT btt_auth_mc_savings.auth_mc_savings_obj, 
                              INPUT "":U, 
                              INPUT "saving_status":U,
                              INPUT btt_auth_mc_savings.line_number, 
                              INPUT "MA":U, 
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Authorisation Savings Status: ":U + STRING(btt_auth_mc_savings.saving_status) +
                                    ",See acronym setup 'Authorisation Savings Status' for list of valid statuses.").                  
        RETURN.
      END. /* IF LOOKUP(STRING(btt_auth_mc_savings.saving_status), cCodelist, {&Delim-ValueList}) = 0 THEN */
      
      /* Saving Date */
      IF btt_auth_mc_savings.saving_date <> ? THEN
      DO:
        FIND FIRST buf_auth NO-LOCK
             WHERE buf_auth.auth_obj = btt_auth_mc_savings.auth_obj
           NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
           
        IF AVAILABLE buf_auth THEN 
        DO:
          IF btt_auth_mc_savings.saving_date < buf_auth.start_date THEN
          DO:
            oErrorObject:addError(INPUT "hatms":U,  
                                  INPUT btt_auth_mc_savings.auth_mc_savings_obj, 
                                  INPUT "":U, 
                                  INPUT "saving_date":U,
                                  INPUT btt_auth_mc_savings.line_number, 
                                  INPUT "ma_MsgAuth":U,  
                                  INPUT 24,  /* The &1 Date &2 cannot be before the &3 Date &4 */
                                  INPUT "Managed Care Savings," + STRING(btt_auth_mc_savings.saving_date,"9999/99/99") +
                                        ",Authorisation Start," + STRING(buf_auth.start_date,"9999/99/99")).
          END. /* IF btt_auth_mc_savings.saving_date < buf_auth.start_date THEN */
        END. /* IF AVAILABLE buf_auth THEN DO: */
      END. /* IF btt_auth_mc_savings.saving_date <> ? THEN */
      
      /* Saving Status Note */
      IF btt_auth_mc_savings.saving_status_note <> "":U THEN 
      DO:
        IF NOT CAN-FIND(FIRST buf_note NO-LOCK
             WHERE buf_note.key  = btt_auth_mc_savings.saving_status_note
               AND buf_note.type BEGINS "AE":U ) THEN
        DO:
          oErrorObject:addError(INPUT "hatms":U, 
                                INPUT btt_auth_mc_savings.auth_mc_savings_obj, 
                                INPUT "":U, 
                                INPUT "saving_status_note":U,
                                INPUT btt_auth_mc_savings.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Managed Care Savings Status Reason: ":U + btt_auth_mc_savings.saving_status_note).
          RETURN.                      
        END. /* IF NOT CAN-FIND(FIRST buf_note NO-LOCK */
      END. /* IF btt_auth_mc_savings.saving_status_note <> "":U THEN  */
      
      /* Validate the Auth Obj */
      IF btt_auth_mc_savings.auth_obj <> 0.00 THEN 
      DO:
        IF NOT CAN-FIND(FIRST buf_auth NO-LOCK
             WHERE buf_auth.auth_obj = btt_auth_mc_savings.auth_obj) THEN
        DO:
          oErrorObject:addError(INPUT "hatms":U, 
                                INPUT btt_auth_mc_savings.auth_mc_savings_obj, 
                                INPUT "":U, 
                                INPUT "auth_obj":U,
                                INPUT btt_auth_mc_savings.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Header (Obj = ":U + STRING(btt_auth_mc_savings.auth_obj) + ")").
          RETURN.
        END. /*IF NOT CAN-FIND(FIRST buf_auth NO-LOCK*/
      END. /*IF btt_auth_mc_savings.auth_obj <> 0.00*/
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_mc_savings.record_action) THEN*/
  END. /*FOR EACH btt_auth_mc_savings NO-LOCK:*/
  
  /* Clean up */
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSavingsType)   THEN DELETE OBJECT oSavingsType.
                IF VALID-OBJECT(oSavingsStatus) THEN DELETE OBJECT oSavingsStatus.
                IF VALID-OBJECT(oErrorObject)   THEN DELETE OBJECT oErrorObject."}

&ENDIF



