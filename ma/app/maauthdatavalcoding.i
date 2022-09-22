/* maauthdatavalcoding.i MEDSTAR Medical Aid System
                         Healthcare Auth data access service: Validate authorisation coding
                         (c) Copyright 2017-2018
                         MIP Holdings (Pty) Ltd
                         All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Coding Buffer        
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack      
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 010195 &THEN
  
  DEFINE VARIABLE cMessage      AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE cCodingOEM    AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE lSuccess      AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE oCodingType   AS cls.mipacronym    NO-UNDO.
  DEFINE VARIABLE oErrorObject  AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lOverlap      AS LOGICAL           NO-UNDO.
  
  DEFINE BUFFER buf_diagnos     FOR diagnos.
  DEFINE BUFFER btt_auth_coding FOR tt_auth_coding.
  DEFINE BUFFER buf_auth        FOR hat_auth.
  
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  FOR EACH btt_auth_coding NO-LOCK:
    
    IF CAN-DO("{&ModifyList}":U, btt_auth_coding.record_action) THEN
    DO:              
      /*
        Coding Type Validation - changed from ma_acAuthPrimaryCodingTypes 
      */
      IF btt_auth_coding.coding_type <> "":U THEN
      DO:
        
        CASE btt_auth_coding.owning_entity_mnemonic:

          WHEN "diagnos":U  
          THEN ASSIGN oCodingType = NEW cls.mipacronym(?, FALSE, "ma_acICDCodingType":U, ?)
                      cCodingOEM  = "ICD":U. 
            
          WHEN "hlmck":U  
          THEN ASSIGN oCodingType = NEW cls.mipacronym(?, FALSE, "ma_acCPTCodingType":U, ?)
                      cCodingOEM  = "CPT":U.

        END CASE.
        
        oCodingType:focusAcronym("KEY":U, btt_auth_coding.coding_type) NO-ERROR.
              
        IF NOT oCodingType:AcronymInFocus THEN
        DO:
          oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, 
                                INPUT btt_auth_coding.auth_coding_obj, 
                                INPUT "":U, 
                                INPUT "coding_type":U,
                                INPUT btt_auth_coding.line_number, 
                                INPUT "MA":U, 
                                INPUT 112,  /* The &1 specified is invalid. &2 */
                                INPUT "Coding Type,":U + SUBSTITUTE("The coding type (&1) specified could not be found.":U, btt_auth_coding.coding_type)).        
          RETURN.                      
        END. /*IF NOT oCodingType:AcronymInFocus THEN*/                     
      END. /*IF btt_auth_coding.coding_type <> "":U THEN*/       
      
      /*
        Associated diagnosis validation
      */
      IF btt_auth_coding.ass_diag_obj <> 0.00 THEN
      DO:
        FIND FIRST buf_diagnos NO-LOCK
             WHERE buf_diagnos.diagnos-obj = btt_auth_coding.ass_diag_obj
          NO-ERROR.
           
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
        
        IF NOT AVAILABLE buf_diagnos THEN  
        DO:
          oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, 
                                INPUT btt_auth_coding.auth_coding_obj, 
                                INPUT "":U, 
                                INPUT "ass_diag_obj":U,
                                INPUT btt_auth_coding.line_number, 
                                INPUT "MA":U,
                                INPUT 112,  /* The &1 specified is invalid. &2 */
                                INPUT "Coding Associated Diagnosis,":U + SUBSTITUTE("An ICD/Diagnosis could not be found with Obj = &1.":U, STRING(btt_auth_coding.ass_diag_obj))).        
          RETURN.                      
        END. /*IF NOT AVAILABLE buf_diagnos THEN  */                        
      END. /*IF btt_auth_coding.ass_diag_obj <> 0.00 THEN*/
          
      /*
        Morphology diagnosis validation
      */
      IF btt_auth_coding.morph_diag_obj <> 0.00 THEN
      DO:
        FIND FIRST buf_diagnos NO-LOCK
             WHERE buf_diagnos.diagnos-obj = btt_auth_coding.morph_diag_obj
          NO-ERROR.
           
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
        
        IF NOT AVAILABLE buf_diagnos THEN  
        DO:
          oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                INPUT btt_auth_coding.auth_coding_obj, 
                                INPUT "":U, 
                                INPUT "morph_diag_obj":U,
                                INPUT btt_auth_coding.line_number, 
                                INPUT "MA":U,                                 
                                INPUT 112,  /* The &1 specified is invalid. &2 */
                                INPUT "Coding Morphology Diagnosis,":U + SUBSTITUTE("An ICD/Diagnosis could not be found with Obj: &1.":U, STRING(btt_auth_coding.morph_diag_obj))).                                        
          RETURN.                      
        END. /*IF NOT AVAILABLE buf_diagnos THEN  */                        
      END. /*IF btt_auth_coding.morph_diag_obj <> 0.00 THEN*/
      
      /*
        Ensure that a valid start date has been specified
      */
      IF btt_auth_coding.start_date = ? OR (btt_auth_coding.end_date <> ? AND btt_auth_coding.start_date > btt_auth_coding.end_date ) THEN
      DO:
        ASSIGN cMessage = "A coding start date must be specified for the " + cCodingOEM.
        IF btt_auth_coding.end_date <> ? 
        THEN
          ASSIGN cMessage = cMessage + " and must come before the end date " + STRING(btt_auth_coding.end_date,"9999/99/99").

        oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                              INPUT btt_auth_coding.auth_coding_obj, 
                              INPUT "":U, 
                              INPUT "start_date":U,
                              INPUT btt_auth_coding.line_number, 
                              INPUT "MA":U, 
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT cCodingOEM + " Coding Start Date,":U + cMessage + ".").                                                                      
        RETURN.                      
      END. /*IF btt_auth_coding.start_date = ? OR (btt_auth_coding.end_date <> ? AND btt_auth_coding.start_date > btt_auth_coding.end_date ) THEN*/
      
      /*  
        Ensure that Auth coding period is within the Auth period for ICD and CPT coding
      */
      IF btt_auth_coding.start_date <> ? THEN
      DO:
        FIND FIRST buf_auth NO-LOCK                                          
             WHERE buf_auth.auth_obj = btt_auth_coding.auth_obj 
          NO-ERROR.
    
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
        
        IF AVAILABLE buf_auth THEN 
        DO:
          mipEnv:Health:mautility:checkOverlappingPeriods(btt_auth_coding.start_date,
                                                          btt_auth_coding.end_date,
                                                          buf_auth.start_date,
                                                          buf_auth.end_date,
                                                          lOverlap).
          IF NOT lOverlap THEN 
          DO:
            oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                  INPUT btt_auth_coding.auth_coding_obj, 
                                  INPUT "":U, 
                                  INPUT "start_date":U,
                                  INPUT btt_auth_coding.line_number, 
                                  INPUT "MA":U,  
                                  INPUT 112,  /* The &1 specified is invalid. &2 */
                                  INPUT cCodingOEM + " Coding Period,The coding period should be within the Authorisation period.":U).
            RETURN.           
          END. /*IF NOT lOverlap THEN */
          
          /*
            Ensure that a valid start date has been specified
          */
          IF btt_auth_coding.start_date < buf_auth.start_date THEN
          DO:
            ASSIGN cMessage = SUBSTITUTE("The &1 coding start date (&2) specified may not come before the Authorisation start date (&3).":U, 
                                         cCodingOEM,
                                         STRING(btt_auth_coding.start_date, "99/99/9999":U), 
                                         STRING(buf_auth.start_date, "99/99/9999":U)).

            oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                  INPUT btt_auth_coding.auth_coding_obj, 
                                  INPUT "":U, 
                                  INPUT "start_date":U,
                                  INPUT btt_auth_coding.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 112,   /* The &1 specified is invalid. &2 */
                                  INPUT cCodingOEM + " Coding Start Date,":U + cMessage).
            RETURN.
          END. /*IF btt_auth_coding.start_date < buf_auth.start_date THEN*/
          
          IF btt_auth_coding.end_date <> ? AND buf_auth.end_date <> ? AND btt_auth_coding.end_date > buf_auth.end_date THEN
          DO:
            ASSIGN cMessage = SUBSTITUTE("The &1 coding end date (&2) specified may not come after the Authorisation end date (&3).":U, 
                                         cCodingOEM,
                                         STRING(btt_auth_coding.end_date, "99/99/9999":U), 
                                         STRING(buf_auth.end_date, "99/99/9999":U)).
            oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                  INPUT btt_auth_coding.auth_coding_obj, 
                                  INPUT "":U, 
                                  INPUT "end_date":U,
                                  INPUT btt_auth_coding.line_number, 
                                  INPUT "MA":U,
                                  INPUT 112,   /* The &1 specified is invalid. &2 */
                                  INPUT cCodingOEM + " Coding End Date,":U + cMessage).
            RETURN.
          END. /*IF btt_auth_coding.end_date <> ? AND buf_auth.end_date <> ? AND btt_auth_coding.end_date > buf_auth.end_date THEN*/  
        END.  /* IF AVAILABLE buf_auth */
      END. /*IF btt_auth_coding.start_date = ? THEN*/
      
      /*  
        CPT code validations
      */
      IF  btt_auth_coding.owning_entity_mnemonic = "hlmck":U THEN 
      DO:                 
        /* 
          Procedure date should only be completed for CPT codes
        */                                          
        IF btt_auth_coding.procedure_date <> ? THEN
        DO:
          mipEnv:Health:mautility:checkOverlappingPeriods(btt_auth_coding.procedure_date,
                                                          btt_auth_coding.procedure_date,
                                                          btt_auth_coding.start_date,
                                                          btt_auth_coding.end_date,
                                                          lOverlap).
          IF NOT lOverlap THEN 
          DO:
            ASSIGN cMessage = SUBSTITUTE(",The procedure date should be within the Authorisation coding period (&1 - &2)":U,
                                         STRING(btt_auth_coding.start_date,'9999/99/99'),
                                         STRING(btt_auth_coding.end_date,'9999/99/99')).

            oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                  INPUT btt_auth_coding.auth_coding_obj, 
                                  INPUT "":U, 
                                  INPUT "procedure_date":U,
                                  INPUT btt_auth_coding.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 112,   /* The &1 specified is invalid. &2 */
                                  INPUT "Procedure Date: " + STRING(btt_auth_coding.procedure_date,'9999/99/99') +
                                         cMessage).
            RETURN.                      
          END. /*IF NOT lOverlap THEN */
        END. /*IF btt_auth_coding.procedure_date = ? THEN*/
      END.  /* IF  btt_auth_coding.owning_entity_mnemonic = "hlmck" THEN */
      
      /*
        Owning entity validation - Make sure that an owning entity is specified
      */
      IF btt_auth_coding.owning_entity_mnemonic = "":U OR btt_auth_coding.owning_entity_mnemonic = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatac":U + btt_auth_coding.owning_entity_mnemonic,           /* ipcOwningEntityMnemonic  */
                              INPUT btt_auth_coding.auth_coding_obj,                              /* ipdOwningEntityObj       */
                              INPUT "":U,                                                         /* ipcOwningEntityKey       */
                              INPUT "owning_entity_mnemonic":U,                                   /* ipcFieldName             */
                              INPUT btt_auth_coding.line_number,                                  /* ipiLineNumber            */
                              INPUT "MA":U,                                                       /* ipcMessageGroup          */
                              INPUT 112,   /* The &1 specified is invalid. &2 */                  /* ipcMessageNumber         */
                              INPUT "Owning Entity,A valid owning entity must be specified.":U).  /* ipcMessageText           */
      END. /*IF btt_auth_coding.owning_entity_mnemonic = "":U OR btt_auth_coding.owning_entity_mnemonic = ? THEN*/

      /*
        Owning entity validation - Make sure that an owning obj or an owning key is specified
      */
      IF btt_auth_coding.owning_obj = 0.00 AND btt_auth_coding.owning_key = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatac":U + btt_auth_coding.owning_entity_mnemonic,                             /* ipcOwningEntityMnemonic  */
                              INPUT btt_auth_coding.auth_coding_obj,                                                /* ipdOwningEntityObj       */
                              INPUT "":U,                                                                           /* ipcOwningEntityKey       */
                              INPUT "owning_alt_value":U,                                                           /* ipcFieldName             */
                              INPUT btt_auth_coding.line_number,                                                    /* ipiLineNumber            */
                              INPUT "MA":U,                                                                         /* ipcMessageGroup          */
                              INPUT 112,   /* The &1 specified is invalid. &2 */                                    /* ipcMessageNumber         */
                              INPUT "Owning Obj/Key,Either a valid owning obj or owning key must be specified.":U). /* ipcMessageText           */
      END. /*IF btt_auth_coding.owning_obj = 0.00 AND btt_auth_coding.owning_key = "":U THEN*/

      /*
        Owning entity validation - Make sure that an owning alt value is specified
      */
      IF btt_auth_coding.owning_alt_value = "":U THEN
      DO:
        oErrorObject:addError(INPUT "hatac":U + btt_auth_coding.owning_entity_mnemonic,                     /* ipcOwningEntityMnemonic  */
                              INPUT btt_auth_coding.auth_coding_obj,                                        /* ipdOwningEntityObj       */
                              INPUT "":U,                                                                   /* ipcOwningEntityKey       */
                              INPUT "owning_alt_value":U,                                                   /* ipcFieldName             */
                              INPUT btt_auth_coding.line_number,                                            /* ipiLineNumber            */
                              INPUT "MA":U,                                                                 /* ipcMessageGroup          */
                              INPUT 112,   /* The &1 specified is invalid. &2 */                            /* ipcMessageNumber         */
                              INPUT "Owning Alt Value,An owning alt value must be specified.":U).           /* ipcMessageText           */
      END. /*IF btt_auth_coding.owning_alt_value = "":U THEN*/

      /*
        Owning entity validation - Make sure that the owning entity specified is valid
      */
      IF btt_auth_coding.owning_entity_mnemonic <> "":U THEN
      DO:
        mipEnv:miDBEntity:focusTable(btt_auth_coding.owning_entity_mnemonic).
        
        IF NOT mipEnv:miDBEntity:InFocus THEN
        DO:
          oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                INPUT btt_auth_coding.auth_coding_obj, 
                                INPUT "":U, 
                                INPUT "owning_entity_mnemonic":U,
                                INPUT btt_auth_coding.line_number, 
                                INPUT "MA":U, 
                                INPUT 107,  /* Could not find a "&1" record using "&2" */ 
                                INPUT "Owning Entity,":U + btt_auth_coding.owning_entity_mnemonic).                  
          RETURN.
        END. /*IF NOT mipEnv:miDBEntity:InFocus THEN*/
        
        ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                           THEN mipEnv:miDBEntity:findRecord(btt_auth_coding.owning_obj)
                           ELSE mipEnv:miDBEntity:findRecord(btt_auth_coding.owning_key)).
                           
        IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
        DO:
          oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                INPUT btt_auth_coding.auth_coding_obj, 
                                INPUT "":U, 
                                INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                       THEN "btt_auth_coding.owning_obj":U 
                                       ELSE "btt_auth_coding.owning_key":U),
                                INPUT btt_auth_coding.line_number, 
                                INPUT "MA":U, 
                                INPUT 107,  /* Could not find a "&1" record using "&2" */
                                INPUT mipEnv:miDBEntity:TableLabel + ",":U + (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                                                              THEN "Obj: " + STRING(btt_auth_coding.owning_obj) 
                                                                              ELSE "Key: " + btt_auth_coding.owning_key)).                  
          RETURN.
        END. /*IF NOT mipEnv:miDBEntity:RecordAvailable THEN */  
      END. /*IF btt_auth_coding.owning_entity_mnemonic <> "":U THEN*/
      
      /*
        Related entity validation
      */
      IF btt_auth_coding.related_entity_mnemonic <> "":U THEN
      DO:
        mipEnv:miDBEntity:focusTable(btt_auth_coding.related_entity_mnemonic).
        
        IF NOT mipEnv:miDBEntity:InFocus THEN
        DO:
          oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                INPUT btt_auth_coding.auth_coding_obj, 
                                INPUT "":U, 
                                INPUT "related_entity_mnemonic":U,
                                INPUT btt_auth_coding.line_number, 
                                INPUT "MA":U, 
                                INPUT 107,  /* Could not find a "&1" record using "&2" */
                                INPUT "Related Entity,":U + btt_auth_coding.related_entity_mnemonic).                  
          RETURN.
        END. /*IF NOT mipEnv:miDBEntity:InFocus THEN*/
        
        ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U
                           THEN mipEnv:miDBEntity:findRecord(btt_auth_coding.related_obj)
                           ELSE mipEnv:miDBEntity:findRecord(btt_auth_coding.related_key)).
                           
        IF NOT mipEnv:miDBEntity:RecordAvailable THEN 
        DO:
          oErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,  
                                INPUT btt_auth_coding.auth_coding_obj, 
                                INPUT "":U, 
                                INPUT (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                       THEN "btt_auth_coding.related_obj":U 
                                       ELSE "btt_auth_coding.related_key":U),
                                INPUT btt_auth_coding.line_number, 
                                INPUT "MA":U, 
                                INPUT 107,  /* Could not find a "&1" record using "&2" */
                                INPUT mipEnv:miDBEntity:TableLabel + ",":U + (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                                                              THEN "Obj: " + STRING(btt_auth_coding.related_obj) 
                                                                              ELSE "Key: " + btt_auth_coding.related_key)).    
          RETURN.
        END. /*IF NOT mipEnv:miDBEntity:RecordAvailable THEN */                      
      END. /*IF btt_auth_coding.related_entity_mnemonic <> "":U THEN*/
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_coding.record_action) THEN*/
  END. /*FOR EACH btt_auth_coding NO-LOCK:*/
  
  { mip/inc/mipcatcherror.i 
                &FINALLY = "IF VALID-OBJECT(oCodingType)  THEN DELETE OBJECT oCodingType.
                            IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF

