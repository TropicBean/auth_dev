/* maauthdatavalauth.i MEDSTAR Medical Aid System
                      Healthcare Auth data access service: Validate auth records
                      (c) Copyright 2017 - 2022
                      MIP Holdings (Pty) Ltd
                      All rights reserved

------------------------------------------------------------------------------
  Purpose   : Validate Auth Buffer    
  Parameters:
  Notes     : Basic field level validation only, all business logic type 
              validation should be placed in the business logic stack      
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 010195 &THEN
  DEFINE VARIABLE oErrorObject   AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lNOKActivated  AS LOGICAL           NO-UNDO.
  
  /*
     Variables required to validate the crosswalk category
  */
  DEFINE VARIABLE cCategoryStructureKey AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cParentCategoryKey    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCategoryLevel        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iCategorySequence     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cCategoryCode         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCategoryLabel        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCategoryDescription  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCategoryValue        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCategoryRuleActive   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCategoryRuleDisplay  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCategoryRuleFormat   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCategoryWobNameList  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFieldName            AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cErrorMessage         AS CHARACTER NO-UNDO.
  DEFINE BUFFER btt_auth FOR tt_auth.
  
  
  oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
  
  
  FOR EACH btt_auth NO-LOCK:
  
    IF CAN-DO("{&ModifyList}":U, btt_auth.record_action) THEN
    DO:
      /*
        Member Validation
      */
      IF btt_auth.dependant <> 99  THEN
      DO:
        FIND FIRST memdep NO-LOCK
             WHERE memdep.mem-num   = btt_auth.mem_num
               AND memdep.dependant = btt_auth.dependant
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        IF NOT AVAILABLE memdep
        THEN  
          oErrorObject:addError(INPUT "hatau":U, 
                                INPUT btt_auth.auth_obj, 
                                INPUT "":U, 
                                INPUT "mem_num":U,
                                INPUT btt_auth.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Dependant (":U + btt_auth.mem_num + "|":U + STRING(btt_auth.dependant) + ")").      
      END. /* IF btt_auth.dependant <> 99 */

      /*
        Authorisation Type Validation
      */
      IF btt_auth.auth_type_obj = 0 THEN
      DO:
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj, 
                              INPUT "":U, 
                              INPUT "auth_type_obj":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U, 
                              INPUT 111,  /* The &1 must be specified. &2 */
                              INPUT "Authorisation Type,This field is required for further validations on the auth.":U).  
        RETURN.
      END.  /* IF btt_auth.auth_type_obj = 0 THEN */

      EMPTY TEMP-TABLE ttAuthTypeConfig.
  
      mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,    
                                                   INPUT-OUTPUT TABLE ttAuthTypeConfig).                               
                                                                                                                   
      FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.                                                                    

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF NOT AVAILABLE ttAuthTypeConfig THEN   
      DO:
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj, 
                              INPUT "":U, 
                              INPUT "auth_type_obj":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U, 
                              INPUT 100,  /* The "&1" specified is invalid */
                              INPUT "Authorisation Type (Obj=":U + STRING(btt_auth.auth_type_obj) + ")").  
        RETURN.                      
      END. /*IF NOT AVAILABLE ttAuthTypeConfig THEN   */                        
      ELSE 
      DO:
        ASSIGN cFieldName = "":U.
        
        IF ttAuthTypeConfig.EffectiveDate > btt_auth.start_date OR (ttAuthTypeConfig.EndDate <> ? AND ttAuthTypeConfig.EndDate < btt_auth.start_date) 
        THEN
          ASSIGN cFieldName = "start_date":U. 
        ELSE 
        IF (ttAuthTypeConfig.EndDate <> ? AND ttAuthTypeConfig.EndDate < btt_auth.end_date) 
        THEN
          ASSIGN cFieldName = "end_date":U.
          
        IF cFieldName <> "":U THEN
        DO:
          oErrorObject:addError(INPUT "hatau":U,                                                   /* ipcOwningEntityMnemonic  */
                                INPUT btt_auth.auth_obj,                                           /* ipdOwningEntityObj       */
                                INPUT "":U,                                                        /* ipcOwningEntityKey       */
                                INPUT cFieldName,                                                  /* ipcFieldName             */
                                INPUT btt_auth.line_number,                                        /* ipiLineNumber            */
                                INPUT "Authorisation Type not active on date of Authorisation.":U, /* ipcMessageText           */
                                INPUT "ERR":U).                                                    /* ipcMessageType           */
          RETURN.    
        END.  /* IF cFieldName <> "":U THEN */
      END.  /* ELSE: IF NOT AVAILABLE ttAuthTypeConfig THEN */
      
      /*
        Insurer Validation
      */                                
      IF btt_auth.insurer_obj <> 0.00 THEN
      DO:
        mipEnv:Health:maUtility:getInsurerDetails(INPUT btt_auth.insurer_obj, 
                                                  INPUT "", 
                                                  INPUT "", 
                                                  INPUT "", 
                                                  INPUT "", 
                                                  INPUT ? , 
                                                  INPUT ? , 
                                                  INPUT ? , 
                                                  OUTPUT TABLE tt_insurer).
           
        IF NOT CAN-FIND(FIRST tt_insurer) THEN
        DO:
          oErrorObject:addError(INPUT "hatau":U,
                                INPUT btt_auth.auth_obj,
                                INPUT "":U,
                                INPUT "insurer_obj":U,
                                INPUT btt_auth.line_number,
                                INPUT "MA":U,
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Client":U ).                                      
          RETURN.                      
        END. /*IF NOT CAN-FIND(FIRST tt_insurer) THEN*/                        
      END. /*IF btt_auth.insurer_obj <> 0.00 THEN*/

      /*
        Ensure that a valid start date has been specified
      */
      IF btt_auth.start_date = ? THEN
      DO:
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj, 
                              INPUT "":U, 
                              INPUT "start_date":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U, 
                              INPUT 111, /* The &1 must be specified. &2 */
                              INPUT "Authorisation Start Date,":U).                  
        RETURN.                      
      END. /*IF btt_auth.start_date = ? THEN*/
      ELSE IF btt_auth.end_date <> ? AND btt_auth.end_date < btt_auth.start_date 
      AND (NOT ttAuthTypeConfig.ActivateLos OR ttAuthTypeConfig.Period <> 0) 
      THEN
      DO:
        oErrorObject:addError(INPUT "hatau":U,
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "start_date":U,
                              INPUT btt_auth.line_number,
                              INPUT "ma_MsgAuth":U,
                              INPUT 24,  /* The &1 Date &2 cannot be before the &3 Date &4 */
                              INPUT  "Authorisation End," + STRING(btt_auth.end_date,"9999/99/99") +
                                    ",Authorisation Start," + STRING(btt_auth.start_date,"9999/99/99")).
        RETURN.                      
      END.  /* ELSE IF btt_auth.end_date < btt_auth.start_date THEN */
      
      /*
         Validate Start Time and End Time
      */ 
      IF ttAuthTypeConfig.ActivateAmPm THEN
      DO:
        IF btt_auth.start_ampm = ? THEN
        DO:
          oErrorObject:addError(INPUT "hatau":U, 
                                INPUT btt_auth.auth_obj, 
                                INPUT "":U, 
                                INPUT "start_ampm":U,
                                INPUT btt_auth.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid .&2*/
                                INPUT "Authorisation Start Time":U).                  
          RETURN.  
        END.  /* IF btt_auth.start_ampm = ? THEN */
        IF btt_auth.end_date <> ? AND btt_auth.end_ampm = ? THEN
        DO:
          oErrorObject:addError(INPUT "hatau":U, 
                                INPUT btt_auth.auth_obj, 
                                INPUT "":U, 
                                INPUT "end_ampm":U,
                                INPUT btt_auth.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation End Time":U).                  
          RETURN.  
        END.  /* ELSE IF btt_auth.end_ampm = ? THEN */
      END. /* IF ttAuthTypeConfig.activate_am_pm */
      ELSE 
        ASSIGN btt_auth.start_ampm = ?
               btt_auth.end_ampm   = ?.

      /*
         Make sure the Due Date falls within the auth period
      */ 
      IF btt_auth.due_date <> ? THEN
      DO:
        IF NOT ttAuthTypeConfig.ActivateDueDate 
        THEN
          ASSIGN btt_auth.due_date = ?.
      END.  /* IF btt_auth.due_date <> ? THEN */
      ELSE IF ttAuthTypeConfig.ActivateDueDate THEN
      DO: 
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj, 
                              INPUT "":U, 
                              INPUT "due_date":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U, 
                              INPUT 111, /* The &1 must be specified. &2 */
                              INPUT "Due Date,":U).                  
        RETURN.  
      END.
      
      /*
        Ensure that the due date is valid if one has been specified
      */
      IF btt_auth.due_date <> ? AND btt_auth.due_date < btt_auth.start_date THEN
      DO:
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj, 
                              INPUT "":U, 
                              INPUT "due_date":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U, 
                              INPUT 100, 
                              INPUT "Authorisation due date: ":U + STRING(btt_auth.due_date,"9999/99/99")).                  
        RETURN.                      
      END. /*IF btt_auth.start_date = ? THEN*/
      
      /*
         Validate the auth episode
      */ 
      IF btt_auth.auth_episode_obj <> 0 THEN
      DO:
        FIND FIRST hat_auth_episode NO-LOCK
             WHERE hat_auth_episode.auth_episode_obj = btt_auth.auth_episode_obj
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
          
        IF NOT AVAILABLE hat_auth_episode THEN
        DO:
          ASSIGN lSuccess = oErrorObject:addError
                              (INPUT "hatau":U,                                 /* ipcOwningEntityMnemonic  */
                               INPUT btt_auth.auth_obj,                         /* ipdOwningEntityObj       */
                               INPUT "":U,                                      /* ipcOwningEntityKey       */
                               INPUT btt_auth.line_number,                      /* ipiLineNumber            */
                               INPUT "MA":U,                                    /* ipcMessageGroup          */
                               INPUT 100,  /* The "&1" specified is invalid */  /* ipiMessageNumber         */
                               INPUT "Episode":U).                              /* ipcReplaceTextList       */
        END.  /* IF NOT AVAILABLE hat_auth_episode THEN */
      END. /* IF btt_auth.auth_episode_obj <> 0 THEN */
                                        
      /*
        Request Source Validation
      */
      IF btt_auth.request_source <> "":U THEN
      DO:
        
        FIND FIRST mic_acronym NO-LOCK
             WHERE mic_acronym.acronym_key = btt_auth.request_source
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
        IF NOT AVAILABLE mic_acronym THEN
        DO:
          oErrorObject:addError(INPUT "hatau":U, 
                                INPUT btt_auth.auth_obj, 
                                INPUT "":U, 
                                INPUT "request_source":U,
                                INPUT btt_auth.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Authorisation Request Source: ":U + btt_auth.request_source).        
          RETURN.                      
        END. /*IF NOT AVAILABLE mic_acronym THEN*/             
      END. /*IF btt_auth.request_source <> "":U THEN*/
      
      /*
        Request By Validation
      */
      IF btt_auth.request_by <> "":U THEN
      DO:
        FIND FIRST mic_acronym NO-LOCK
             WHERE mic_acronym.acronym_key = btt_auth.request_by
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
        IF NOT AVAILABLE mic_acronym THEN
        DO:
          oErrorObject:addError(INPUT "hatau":U, 
                                INPUT btt_auth.auth_obj, 
                                INPUT "":U, 
                                INPUT "request_by":U,
                                INPUT btt_auth.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Request By: ":U + btt_auth.request_by).        
          
          RETURN.                      
        END. /*IF NOT AVAILABLE mic_acronym THEN*/             
      END. /*IF btt_auth.request_by <> "":U THEN*/            
      
      /*
        Auth Body Region Validation
      */
      IF ttAuthTypeConfig.ActivateBodyRegion THEN
      DO:
        IF btt_auth.body_region <> "":U THEN
        DO:
          FIND FIRST mic_acronym NO-LOCK
               WHERE mic_acronym.acronym_key = btt_auth.body_region
            NO-ERROR.
            
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
            
          IF NOT AVAILABLE mic_acronym THEN
          DO:
            oErrorObject:addError(INPUT "hatau":U, 
                                  INPUT btt_auth.auth_obj, 
                                  INPUT "":U, 
                                  INPUT "body_region":U,
                                  INPUT btt_auth.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 100,  /* The "&1" specified is invalid */
                                  INPUT "Authorisation Body Region: " + btt_auth.body_region).        
            RETURN.                      
          END. /*IF NOT AVAILABLE mic_acronym THEN*/             
        END. /*IF btt_auth.body_region <> "":U THEN*/                        
        ELSE DO:
          oErrorObject:addError(INPUT "hatau":U, 
                                INPUT btt_auth.auth_obj, 
                                INPUT "":U, 
                                INPUT "body_region":U,
                                INPUT btt_auth.line_number, 
                                INPUT "MA":U, 
                                INPUT 111,  /* The &1 must be specified. &2 */
                                INPUT "Body Region,").        
          RETURN.
        END.  /* IF btt_auth.body_region <> "":U THEN */
      END.  /* IF ttAuthTypeConfig.ActivateBodyRegion THEN */
      ELSE 
        ASSIGN btt_auth.body_region = "":U.
      
      /*
         Validate Authorisation Service type
      */ 
      IF ttAuthTypeConfig.ActivateServiceType THEN   
      DO:
        /*
          Only apply vlidation if service type <> "". The service type will only be assigned after coding details are saved
        */
        IF btt_auth.service_type <> "":U THEN
        DO:
          IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK
                          WHERE mic_acronym.category_key = "ma_acServiceType":U
                            AND mic_acronym.acronym_key = TRIM(btt_auth.service_type)) THEN
          DO:
            oErrorObject:addError(INPUT "hatau":U, 
                                  INPUT btt_auth.auth_obj,
                                  INPUT "":U,
                                  INPUT "service_type":U,
                                  INPUT btt_auth.line_number, 
                                  INPUT "MA":U,
                                  INPUT 100, /* The "&1" specified is invalid */
                                  INPUT "Service Type: " + btt_auth.service_type).
            RETURN.
          END.  /* IF NOT CAN-FIND(FIRST mic_acronym NO-LOCK */
        END. /* IF btt_auth.service_type <> "":U */
      END.  /* IF ttAuthTypeConfig.ActivateServiceType THEN */
      ELSE 
        ASSIGN btt_auth.service_type = "":U.

      /*
         Validate Next of Kin
      */ 
      ASSIGN lNOKActivated = mipEnv:Health:AuthBusinessLogic:authNextOfKinActivated(INPUT  btt_auth.insurer_obj,
                                                                                    INPUT  btt_auth.option_code,
                                                                                    INPUT  btt_auth.start_date).
      IF NOT lNOKActivated 
      THEN
        ASSIGN btt_auth.next_of_kin = "":U.

      IF btt_auth.amount_auth = ?  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid */
                              INPUT "Amount Authorised" ).

      IF btt_auth.amount_auth < 0  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "amount_auth":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The "&1" specified is invalid.&2  */
                              INPUT "Amount Authorised , The Amount Authorised must be greater than or equal to 0 .").

      IF btt_auth.amount_paid = ?  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "amount_paid":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid. */
                              INPUT "Amount Paid" ).

      IF btt_auth.amount_paid < 0  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "amount_paid":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /*  The "&1" specified is invalid .&2 */
                              INPUT "Amount Paid , The Amount Paid must be greater than or equal to 0 .").

      IF btt_auth.quantity_auth = ?  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /*  The "&1" specified is invalid. */
                              INPUT "Quantity Authorised" ).

      IF btt_auth.quantity_auth < 0  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "quantity_auth":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The "&1" specified is invalid.&2* */
                              INPUT "Quantity Authorised , The Quantity Authorised must be greater than or equal to 0 .").

      IF btt_auth.quantity_paid = ?  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "quantity_paid":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 100, /* The "&1" specified is invalid . */
                              INPUT "Quantity Paid" ).

      IF btt_auth.quantity_paid < 0  
      THEN
        oErrorObject:addError(INPUT "hatau":U, 
                              INPUT btt_auth.auth_obj,
                              INPUT "":U,
                              INPUT "quantity_paid":U,
                              INPUT btt_auth.line_number, 
                              INPUT "MA":U,
                              INPUT 112,  /* The "&1" specified is invalid .&2 */
                              INPUT "Quantity Paid , The Quantity Paid must be greater than or equal to 0 .").
     
      IF btt_auth.discount_type  = ? AND 
        (btt_auth.discount_auth <> 0.00 AND btt_auth.discount_auth <> ?)
      THEN
        ASSIGN
          cErrorMessage = "There is a discount authorised value entered but no discount type. Please enter a discount type of either percent or Rand or remove the discount authorised value":U
          lSuccess      =  oErrorObject:addError(INPUT "hatau":U,
                                                 INPUT btt_auth.auth_obj,
                                                 INPUT "":U,
                                       //        INPUT "discount_type":U,  *** Parameter removed to NOT revert back to previous value ***
                                                 INPUT btt_auth.line_number,
                                                 INPUT cErrorMessage,
                                                 INPUT "ERR":U).
      
    END. /*IF btt_auth.record_action = "Modify":U THEN*/
  END. /*FOR EACH btt_auth NO-LOCK:*/
                                
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}

&ENDIF


