/* maauthdatavalprovider.i MEDSTAR Medical Aid System
                           Healthcare Auth data access service: Validate auth provider
                           (c) Copyright 2017 - 2021
                           MIP Holdings (Pty) Ltd
                           All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Provider Buffer        
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack            
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 010195 &THEN  
  
  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject   NO-UNDO.
  DEFINE VARIABLE cErrorMessage    AS CHARACTER           NO-UNDO.
   
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
  
  ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  BLKPROVIDER:
  FOR EACH btt_auth_provider NO-LOCK:
  
    IF CAN-DO("{&ModifyList}":U, btt_auth_provider.record_action) THEN
    DO:
      /*
        Ensure that we either have a provider number or a discipline
      */
      /*
      IF btt_auth_provider.doc_num = 0 AND btt_auth_provider.pr_type = 000 THEN
      DO:
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj, 
                              INPUT "":U, 
                              INPUT "doc_num":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "ma_MsgAuth":U, 
                              INPUT 9,  /* Please ensure that either a valid provider number or discipline is specified */
                              INPUT "":U).                  
        NEXT BLKPROVIDER.                        
      END. /*IF btt_auth_provider.doc_num = 0 AND btt_auth_provider.pr_type = 000 THEN*/
      */
      FIND FIRST mic_acronym NO-LOCK
           WHERE mic_acronym.acronym_key = btt_auth_provider.provider_type
        NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
      
      IF NOT AVAILABLE mic_acronym THEN
      DO:
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj, 
                              INPUT "":U, 
                              INPUT "provider_type":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U, 
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Authorisation provider type: ":U + btt_auth_provider.provider_type).                  
        NEXT BLKPROVIDER.                        
      END. /*IF NOT AVAILABLE mic_acronym THEN*/
      
      /*
        Ensure that a valid start date has been specified
      */
      IF btt_auth_provider.start_date = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj, 
                              INPUT "":U, 
                              INPUT "start_date":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */
                              INPUT "Authorisation Provider Start Date,":U).                  
        NEXT BLKPROVIDER.                      
      END. /* IF btt_auth_provider.start_date = ? THEN */ 
      ELSE IF btt_auth_provider.end_date <> ? AND btt_auth_provider.start_date > btt_auth_provider.end_date THEN
      DO:
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj, 
                              INPUT "":U, 
                              INPUT "start_date":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "ma_MsgAuth":U, 
                              INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */
                              INPUT "Provider Start Date," + STRING(btt_auth_provider.start_date,"9999/99/99") + "," +
                                    "Provider End Date,"   + STRING(btt_auth_provider.end_date,"9999/99/99")).                  
        NEXT BLKPROVIDER.                      
      END. /* ELSE IF btt_auth_provider.end_date <> ? AND btt_auth_provider.start_date > btt_auth_provider.end_date THEN*/ 
      
      /*
         Validate the auth provider discipline and sub-discipline
      */
      IF btt_auth_provider.pr_type <> 0 OR btt_auth_provider.sub_pr_type <> 0 THEN
      DO:
        mipEnv:Health:maAdministration:validateDiscipline(INPUT  STRING(btt_auth_provider.pr_type),
                                                          INPUT  btt_auth_provider.sub_pr_type,
                                                          OUTPUT cErrorMessage).
        IF cErrorMessage <> "":U
        THEN DO:
          oErrorObject:addError(INPUT "hatap":U,
                                INPUT btt_auth_provider.auth_provider_obj,
                                INPUT "":U,
                                INPUT "pr_type":U,
                                INPUT btt_auth_provider.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT cErrorMessage).
        END. /* IF cErrorMessage <> "":U */
      END. /* IF btt_auth_provider.pr_type <> 0 OR btt_auth_provider.sub_pr_type <> 0 THEN */
      
      /*
        Letter Print Indicator on Provider Validation 
      */
      IF btt_auth_provider.print_dm <> "":U THEN
      DO:
        FIND FIRST mic_acronym NO-LOCK
             WHERE mic_acronym.acronym_key = btt_auth_provider.print_dm
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
        
        IF NOT AVAILABLE mic_acronym THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "print_dm":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation print indicator: ":U + btt_auth_provider.print_dm).        
          NEXT BLKPROVIDER.                      
        END. /*IF NOT AVAILABLE mic_acronym THEN*/
      END. /*IF btt_auth_provider.print_dm <> "":U THEN*/       
      
      /*
        Payee Indicator on Provider Validation 
      */
      IF btt_auth_provider.payee_dm <> "":U THEN
      DO:
        FIND FIRST mic_acronym NO-LOCK
             WHERE mic_acronym.acronym_key = btt_auth_provider.payee_dm
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }     
        
        IF NOT AVAILABLE mic_acronym THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "payee_dm":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation payee indicator: ":U + btt_auth_provider.payee_dm).        
          
          NEXT BLKPROVIDER.                      
        END. /*IF NOT AVAILABLE mic_acronym THEN*/
      END. /*IF btt_auth_provider.payee_dm <> "":U THEN*/

      IF btt_auth_provider.default_base_rate <> "" THEN
      DO:
        FIND FIRST baserate NO-LOCK 
             WHERE baserate.base-rate = btt_auth_provider.default_base_rate 
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE baserate THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "default_base_rate":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Default Base Rate: ":U + btt_auth_provider.default_base_rate).        
          
          NEXT BLKPROVIDER.          
        END. /* IF NOT AVAILABLE baserate  */
      END. /* IF btt_auth_provider.default_base_rate <> ""  */
      
      IF btt_auth_provider.override_base_rate <> "" THEN
      DO:
        FIND FIRST baserate NO-LOCK 
             WHERE baserate.base-rate = btt_auth_provider.override_base_rate 
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE baserate THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "override_base_rate":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Override Base Rate: ":U + btt_auth_provider.override_base_rate).        
          
          NEXT BLKPROVIDER.          
        END. /* IF NOT AVAILABLE baserate  */
      END. /* IF btt_auth_provider.override_base_rate <> ""  */

      IF btt_auth_provider.default_ars_rate <> "" THEN
      DO:
        FIND FIRST arsrate NO-LOCK 
             WHERE arsrate.ars-rate = btt_auth_provider.default_ars_rate 
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE arsrate THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "default_ars_rate":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Default ARS Rate: ":U + btt_auth_provider.default_ars_rate).        
          
          NEXT BLKPROVIDER.          
        END. /* IF NOT AVAILABLE arsrate  */
      END. /* IF btt_auth_provider.default_ars_rate <> ""  */
      
      IF btt_auth_provider.override_ars_rate <> "" THEN
      DO:
        FIND FIRST arsrate NO-LOCK 
             WHERE arsrate.ars-rate = btt_auth_provider.override_ars_rate 
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE arsrate THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "override_ars_rate":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Override ARS Rate: ":U + btt_auth_provider.override_ars_rate).        
          
          NEXT BLKPROVIDER.          
        END. /* IF NOT AVAILABLE arsrate  */
      END. /* IF btt_auth_provider.override_ars_rate <> ""  */

      IF btt_auth_provider.auth_group_obj <> 0.00 THEN
      DO:
        FIND FIRST ham_auth_group NO-LOCK
             WHERE ham_auth_group.auth_group_obj = btt_auth_provider.auth_group_obj
           NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE ham_auth_group THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "auth_group_obj":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Auth group").
          NEXT BLKPROVIDER.
        END. /* IF NOT AVAILABLE ham_auth_group THEN */
        ELSE IF  ham_auth_group.effective_date > btt_auth_provider.start_date 
             OR (ham_auth_group.end_date <> ? AND ham_auth_group.end_date < btt_auth_provider.start_date) THEN
        DO:
          oErrorObject:addError(INPUT "hatap":U, 
                                INPUT btt_auth_provider.auth_provider_obj, 
                                INPUT "":U, 
                                INPUT "auth_group_obj":U,
                                INPUT btt_auth_provider.line_number, 
                                INPUT "MA":U, 
                                INPUT 112,  /* The "&1" specified is invalid. &2 */
                                INPUT "Auth group " + ham_auth_group.auth_group_code +
                                      ",The Auth group period is invalid for the Auth Date.").
          NEXT BLKPROVIDER.
        END.  /* ELSE IF ham_auth_group.effective_date > btt_auth_provider.start_date */
      END. /* IF btt_auth_provider.auth_group_obj <> 0.00 THEN */
      
      IF LOOKUP(STRING(btt_auth_provider.authorise_all_services), "YES,NO,TRUE,FALSE":U) = 0 THEN
      DO:
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj, 
                              INPUT "":U, 
                              INPUT "authorise_all_services":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U, 
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Authorise All Services value").

        NEXT BLKPROVIDER.
      END. /* IF LOOKUP(STRING(btt_auth_provider.authorise_all_services), "YES,NO,TRUE,FALSE":U) = 0 THEN */
      
      IF btt_auth_provider.amount_auth = ?  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Amount Authorised" ).
      
      IF btt_auth_provider.amount_auth < 0  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Amount Authorised , The Amount Authorised must be greater than or equal to 0 .").
      
      IF  btt_auth_provider.amount_auth > 0
      AND btt_auth_provider.amount_auth < btt_auth_provider.amount_paid 
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Amount Authorised , The Amount Authorised can not be less than the amount paid. ").
      
      
      IF btt_auth_provider.amount_paid = ?  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "amount_paid":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Amount Paid" ).
      
      IF btt_auth_provider.amount_paid < 0  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "amount_paid":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Amount Paid , The Amount Paid must be greater than or equal to 0 .").
      
      IF btt_auth_provider.quantity_auth = ?  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Quantity Authorised" ).
      
      IF btt_auth_provider.quantity_auth < 0  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Quantity Authorised , The Quantity Authorised must be greater than or equal to 0 .").
      
      IF  btt_auth_provider.quantity_auth > 0
      AND btt_auth_provider.quantity_auth < btt_auth_provider.quantity_paid 
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Quantity Authorised , The Quantity Authorised can not be less than the Quantity paid. ").
      
      
      IF btt_auth_provider.quantity_paid = ?  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "quantity_paid":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Quantity Paid" ).
      
      IF btt_auth_provider.quantity_paid < 0  
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "quantity_paid":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Quantity Paid , The Quantity Paid must be greater than or equal to 0 .").
    
      IF btt_auth_provider.auth_copay_control_obj <> 0 
      THEN DO: 
       FIND FIRST hac_auth_copay_control 
            WHERE hac_auth_copay_control.auth_copay_control_obj = btt_auth_provider.auth_copay_control_obj 
         NO-ERROR.
         
       IF NOT AVAILABLE hac_auth_copay_control 
       THEN 
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "auth_copay_control_obj":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Co-payment Control , Co-payment control record does not exist in the database.").
      END.

      IF btt_auth_provider.copay_auth = ? 
      THEN 
   	    oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Co-payment Auth , Co-payment Auth value must be specified.").

      IF btt_auth_provider.copay_auth < 0
      THEN 
   	    oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Co-payment Auth , Co-payment Auth value must be a value greater/equal than zero.").
  
      IF  btt_auth_provider.copay_auth > btt_auth_provider.amount_auth 
      AND btt_auth_provider.amount_auth <> 0 
      THEN 
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "copay_auth":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Co-payment Auth , Co-payment Auth value can not be greater than the provider authorised amount.").
      IF btt_auth_provider.copay_auth_% = ? 
      THEN 
        oErrorObject:addError(INPUT "hatap":U, 
                             INPUT btt_auth_provider.auth_provider_obj,
                             INPUT "":U,
                             INPUT "copay_auth_%":U,
                             INPUT btt_auth_provider.line_number, 
                             INPUT "MA":U,
                             INPUT 112,  /* The &1 specified is invalid. &2 */
                             INPUT "Co-payment Auth Percentage , Co-payment Auth Percentage must be specified.").

      IF btt_auth_provider.copay_auth_% < 0 
      OR btt_auth_provider.copay_auth_% > 100 
      THEN
        oErrorObject:addError(INPUT "hatap":U, 
                              INPUT btt_auth_provider.auth_provider_obj,
                              INPUT "":U,
                              INPUT "copay_auth_%":U,
                              INPUT btt_auth_provider.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The &1 specified is invalid. &2 */
                              INPUT "Co-payment Auth Percentage , Co-payment Auth Percentage must be a value from 0 and less/equal to 100.").
      

      IF btt_auth_provider.copay_override_note <> "" 
      THEN DO:
        FIND FIRST note NO-LOCK
             WHERE note.scheme-code = 0
               AND note.type        = "AO":U
               AND note.key         = btt_auth_provider.copay_override_note 
          NO-ERROR. 
          
        IF NOT AVAILABLE note  
        THEN
           oErrorObject:addError(INPUT "hatap":U, 
                                 INPUT btt_auth_provider.auth_provider_obj,
                                 INPUT "":U,
                                 INPUT "copay_override_note":U,
                                 INPUT btt_auth_provider.line_number, 
                                 INPUT "MA":U,
                                 INPUT 112,  /* The &1 specified is invalid. &2 */
                                 INPUT "Co-payment Override Note , Note with Key ~"" + btt_auth_provider.copay_override_note + "~" was not found.").

      END.   
    END. /*IF CAN-DO("{&ModifyList}":U, btt_auth_provider.record_action) THEN*/
  END. /*FOR EACH btt_auth_provider NO-LOCK:*/
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject. "}

&ENDIF
