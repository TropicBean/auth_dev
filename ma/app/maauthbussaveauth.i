/* maauthbussaveauth.i  MEDSTAR Medical Aid System
                        Save Authorisation Record
                        (c) Copyright 1990 - 2022
                        MIP Holdings (Pty) Ltd
                        All rights reserved
*/                      
DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.
  
DEFINE VARIABLE cDefaultSequence AS CHARACTER          NO-UNDO.
DEFINE VARIABLE cErrorDetail     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE cRuleValue       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE cMessage         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE lcResult         AS LONGCHAR           NO-UNDO.
DEFINE VARIABLE dInsurerObj      AS DECIMAL            NO-UNDO.
DEFINE VARIABLE dAuthObj         AS DECIMAL            NO-UNDO.
DEFINE VARIABLE lFailureOccurred AS LOGICAL            NO-UNDO.
DEFINE VARIABLE lSequenceActive  AS LOGICAL            NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOGICAL            NO-UNDO.
DEFINE VARIABLE lValidRule       AS LOGICAL            NO-UNDO.
DEFINE VARIABLE iClient          AS INTEGER            NO-UNDO.
DEFINE VARIABLE iOptionCode      AS INTEGER            NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN  

DEFINE BUFFER buf_auth         FOR hat_auth.           /* This buffer will contain the db-version of btt_auth */ 

  IF AVAILABLE btt_auth AND 
    CAN-DO("{&ActionList}":U, btt_auth.record_action) AND NOT goErrorObject:CanFind("hatau":U, btt_auth.auth_obj, "":U, "ERR":U) 
  THEN
  DO:
  
    IF btt_auth.record_action = "MODIFY":U THEN
    DO:
      /*
        Retain old auth obj in case this is a new record and we have a dummy obj which will need to be replaced
        on all the child records if this is a batch update. 
      */
      ASSIGN dAuthObj         = btt_auth.auth_obj
             cDefaultSequence = "ma_AuthNumber":U. 
      
      IF btt_auth.auth_obj <= 0.00 THEN
      DO:
        /*
          Set some defaults
        */       
        ASSIGN btt_auth.user_id    = mipEnv:miUser:UserCode
               btt_auth.request_by = "ma_acAuthRequestByTypeHospital":U WHEN btt_auth.request_by = "":U. 
             
        /*
          Set Authorisation Number
        */
        EMPTY TEMP-TABLE ttAuthTypeConfig.
  
        mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,                                                       
                                                     INPUT-OUTPUT TABLE ttAuthTypeConfig).                               
                                                                                                                   
        FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.                                                                    
                                                                                                                   
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
  
        IF AVAILABLE ttAuthTypeConfig THEN
        DO:
          /*
            Ensure that the key configured in the auth type is active
          */
          IF ttAuthTypeConfig.SequenceKey <> "":U AND ttAuthTypeConfig.SequenceKey <> ?
          THEN 
            ASSIGN cDefaultSequence = ttAuthTypeConfig.SequenceKey.
          ELSE
            ASSIGN cDefaultSequence = "ma_AuthNumber":U.

          ASSIGN lSequenceActive  = mipEnv:miUtility:isSequenceActive(INPUT cDefaultSequence)
                 lFailureOccurred = (IF cDefaultSequence = "":U OR lSequenceActive = ? 
                                     THEN TRUE ELSE NOT lSequenceActive).
          
          IF lFailureOccurred 
          THEN 
            goErrorObject:addError
              (INPUT "hatau":U, 
               INPUT btt_auth.auth_obj, 
               INPUT "":U, 
               INPUT btt_auth.line_number, 
               INPUT "MA":U, 
               INPUT 100,  /* The "&1" specified is invalid */
               INPUT "Sequence key: ":U + cDefaultSequence).                          
          ELSE
            ASSIGN btt_auth.auth_num = (IF ttAuthTypeConfig.AuthTypePrefix <> "":U AND ttAuthTypeConfig.SequenceKey <> ?
                                        THEN ttAuthTypeConfig.AuthTypePrefix + " ":U 
                                        ELSE "":U)
                                     + mipEnv:miUtility:getNextSequence(cDefaultSequence)
                   btt_auth.default_claim_code = INTEGER(ttAuthTypeConfig.ClaimCodes)
                   btt_auth.default_claim_type = ttAuthTypeConfig.ClaimTypes
                   btt_auth.authorise_all_services = ttAuthTypeConfig.AuthoriseAllServices.
        END. /*IF AVAILABLE ttAuthTypeConfig THEN*/
        
        /*
          Determine the option for the auth date provided
        */
        ASSIGN lSuccess = mipEnv:Health:maMember:getMemberOption(INPUT btt_auth.mem_num, INPUT btt_auth.start_date, OUTPUT iOptionCode).
        
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  iOptionCode,
                                                       INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                       INPUT  "OptionCodeDefaultZero":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).
        IF lValidRule THEN
        DO:
          IF cRuleValue = "ALL":U 
          THEN
            ASSIGN btt_auth.option_code = 00.
          ELSE 
          DO:
            IF AVAILABLE ttAuthTypeConfig AND LOOKUP(ttAuthTypeConfig.AuthType, cRuleValue) > 0 
            THEN
              ASSIGN btt_auth.option_code = 00.
            ELSE 
              ASSIGN btt_auth.option_code = btt_auth.option_code.
          END.  /* ELSE: IF cRuleValue = "ALL" */
        END.  /* IF lValidRule THEN */
        ELSE 
          ASSIGN btt_auth.option_code = btt_auth.option_code.
                                                                   
        ASSIGN btt_auth.option_code = iOptionCode.
        
      END. /*IF btt_auth.auth_obj <= 0.00 THEN*/   
      

      /* 
        - Check for Auth Duplicates.
        - If the record is a new Auth (btt_auth.auth_obj <= 0.00), OR If the an existing Auth has been updated. 
      */
      IF btt_auth._data_load AND (btt_auth.auth_obj <= 0
      OR CAN-FIND(FIRST buf_auth NO-LOCK
                    WHERE buf_auth.auth_obj       = btt_auth.auth_obj
                      AND buf_auth.mem_num        = btt_auth.mem_num
                      AND buf_auth.dependant      = btt_auth.dependant 
                      AND buf_auth.option_code	 <> btt_auth.option_code
                      AND buf_auth.auth_type_obj <> btt_auth.auth_type_obj
                      AND buf_auth.start_date    <> btt_auth.start_date
                      AND buf_auth.end_date   	 <> btt_auth.end_date))
      THEN
      DO:
        mipEnv:Health:AuthService:checkForDuplicateAuths(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                         OUTPUT TABLE tt_auth_type,
                                                         OUTPUT cMessage).
      END. /* IF btt_auth.auth_obj <= 0.00  */
      
      /*
        Populate Auth Episode Obj
      */
      IF btt_auth.auth_episode_obj <= 0 OR btt_auth.auth_obj <= 0 THEN
      DO:  
        IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.ActivateEpisodeNumber 
        THEN      
          RUN _setEpisodeObj IN TARGET-PROCEDURE (BUFFER btt_auth).

        /*
           Assign Auth Reference Number when new auth is added 
           and 'AuthRefNumGenerate' rule is activated.
        */ 
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                       INPUT  iOptionCode,
                                                       INPUT  "ma_acAuthRuleTypeAUTHREG":U,
                                                       INPUT  "AuthRefNumGenerate":U,
                                                       INPUT  btt_auth.start_date,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).
                                                       
        IF lValidRule AND cRuleValue = "Activate":U THEN 
        DO:
          FIND FIRST tt_auth_episode NO-LOCK
               WHERE tt_auth_episode.mem_num   = btt_auth.mem_num
                 AND tt_auth_episode.dependant = btt_auth.dependant
            NO-ERROR. 
        
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
          IF AVAILABLE tt_auth_episode
          THEN 
            ASSIGN btt_auth.reference_auth_num = tt_auth_episode.episode_num + btt_auth.auth_num.
            
        END.  /*IF lValidRule AND cRuleValue = "Activate":U THEN*/
                    
      END. /*IF btt_auth.auth_episode_obj <= 0 OR btt_auth.auth_obj <= 0 THEN*/

      IF  AVAILABLE ttAuthTypeConfig 
      AND ttAuthTypeConfig.ActivateEpisodeNumber THEN
      DO:
        FIND FIRST buf_auth NO-LOCK
             WHERE buf_auth.auth_obj       = btt_auth.auth_obj
             AND   buf_auth.mem_num        = btt_auth.mem_num
             AND   buf_auth.dependant      = 99 
             AND   buf_auth.dependant     <> btt_auth.dependant
        NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
        IF AVAILABLE buf_auth THEN 
        DO:
          RUN _updateEpisodeObj IN TARGET-PROCEDURE ( BUFFER buf_auth, INPUT btt_auth.dependant ).

        END. /* IF AVAILABLE buf_auth */
      END. /* IF  AVAILABLE ttAuthTypeConfig... */
      
      /*
        Populate Insurer Obj
      */
      IF btt_auth.insurer_obj = 0.00 THEN
      DO:
        RUN _setInsurerObj IN TARGET-PROCEDURE (BUFFER btt_auth).
      END. /*IF btt_auth.insurer_obj = 0.00 THEN*/
             
      VALIDATE btt_auth.        
      
      /*
        No point in continuing if an error occurred 
      */
      IF lFailureOccurred THEN 
        LEAVE.
    END. /*IF btt_auth.record_action = "MODIFY":U THEN*/
         
    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuth(BUFFER btt_auth, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).      
    
    /*
      Check if an error occurred when the record was passed to the data access layer to be saved
    */
    IF goErrorObject:CanFind("hatau":U, btt_auth.auth_obj, "":U, "ERR":U)
    THEN LEAVE.
      
    IF btt_auth.record_action = "Modify":U THEN 
    DO:

      /*
        If a new record was created, we need to populate the new auth obj on the child records
      */
      IF dAuthObj <= 0.00 THEN
        RUN _populateAuthObj IN TARGET-PROCEDURE ( INPUT dAuthObj, INPUT btt_auth.auth_obj ).

      /*
      /*
        Authorisation Decisions
      */
      mipEnv:Health:AuthDecisions:highRiskIndicator 
        (INPUT NEW cls.maparameterobject(BUFFER btt_auth:HANDLE), OUTPUT lcResult, OUTPUT cErrorDetail).
      */                
    END. /*IF btt_auth.record_action = "Modify":U THEN */
    
    /*
      Clear record action
    */
    ASSIGN btt_auth.record_action = "":U.
    
    VALIDATE btt_auth.
  END. /*IF AVAILABLE btt_auth AND CAN-DO("{&ActionList}":U, btt_auth.record_action) AND NOT goErrorObject:CanFind("hatau":U, btt_auth.auth_obj, "":U, "ERR":U) THEN*/  

&ENDIF

  { mip/inc/mipcatcherror.i }
  
