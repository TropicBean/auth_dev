
/* maauthdatavalauthcrosswalk.i MEDSTAR Medical Aid System
                                Healthcare Auth data access service: Validate Auth Crosswalk Buffer
                                (c) Copyright 2019
                                MIP Holdings (Pty) Ltd
                                All rights reserved
                                
------------------------------------------------------------------------------
  Purpose   : Validate Auth Crosswalk Buffer        
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack            
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 010195 &THEN 

  DEFINE VARIABLE oErrorObject  AS cls.maerrorobject NO-UNDO.
  
  DEFINE BUFFER btt_auth_crosswalk  FOR tt_auth_crosswalk.
  DEFINE BUFFER buf_auth            FOR hat_auth.
  DEFINE BUFFER buf_crosswalk       FOR hlm_crosswalk.
  
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  FOR EACH btt_auth_crosswalk:
    IF CAN-DO("{&ModifyList}":U, btt_auth_crosswalk.record_action) THEN
    DO:
      /* 
        Check that the linked crosswalk exists
      */
      IF btt_auth_crosswalk.crosswalk_obj <> 0.00  THEN
      DO:
        IF NOT CAN-FIND(FIRST buf_crosswalk NO-LOCK 
                        WHERE buf_crosswalk.crosswalk_obj = btt_auth_crosswalk.crosswalk_obj) THEN 
        DO:
          oErrorObject:addError(INPUT "hataw":U, 
                                INPUT btt_auth_crosswalk.auth_crosswalk_obj, 
                                INPUT "":U, 
                                INPUT "crosswalk_obj":U,
                                INPUT btt_auth_crosswalk.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Crosswalk (Obj = ":U + STRING(btt_auth_crosswalk.crosswalk_obj) + ")").
          RETURN.
        END. /* IF NOT CAN-FIND(FIRST buf_crosswalk NO-LOCK WHERE buf_crosswalk.crosswalk_obj = btt_auth_crosswalk.crosswalk_obj */
      END. /* IF btt_auth_crosswalk.crosswalk_obj <> 0.00   */
    END. /* CAN-DO("{&ModifyList}":U, btt_auth_crosswalk.record_action) */
  END. /* FOR EACH btt_auth_crosswalk */
    
  /* Clean up */
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF




