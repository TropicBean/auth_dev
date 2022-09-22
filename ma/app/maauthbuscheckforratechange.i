/* maauthbuscheckforratechange.i  MEDSTAR Medical Aid System
                                  (c) Copyright 1990 - 2020
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved                      
--------------------------------------------------------------------------------
  Purpose: This procedure will iterate through each auth in the dataset and 
           firstly determine if a check for a rate change is neccesary.
           If a rate change check is done and a rate change should be applied 
           on the auth, conversions will apply according to auth rate control setup.
  Parameters: DATASET dsAuthorisation BY-REFERENCE
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.

  DEFINE VARIABLE lCheckForRateChange                   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cDefaultBaseRate                      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDefaultArsRate                       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOverrideBaseRate                     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOverrideArsRate                      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCodeLinkCategory                     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCurrentOverrideBaseRate              AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCurrentOverrideArsRate               AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTrackingMessage                      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNewRate                              AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cUserMessage                          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPreviousRate                         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDefaultRate                          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cWarningMessage                       AS CHARACTER NO-UNDO.
                                                        
  DEFINE VARIABLE iCalcTime                             AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lSuccess                              AS LOGICAL   NO-UNDO.
                                                        
  DEFINE VARIABLE lWorkGroupCheckApplies                AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cRateControlWarningApplies            AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE dBufWorkgroupObj                      AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cWorkGroupWarningMessage              AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dAuthRateControlObj                   AS DECIMAL   NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER btt_auth                    FOR tt_auth .
  DEFINE BUFFER buf_auth                    FOR hat_auth.
  DEFINE BUFFER tt_auth_provider            FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_provider           FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_coding             FOR tt_auth_coding.
  DEFINE BUFFER btt_auth_flag_value         FOR tt_auth_flag_value.
  DEFINE BUFFER buf_auth_provider           FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_coding             FOR hat_auth_coding.
  DEFINE BUFFER btt_auth_crosswalk          FOR tt_auth_crosswalk.
  DEFINE BUFFER buf_memdep                  FOR memdep.

  AUTH-BLK:
  FOR EACH btt_auth NO-LOCK
     WHERE btt_auth.auth_obj > 0
        OR (btt_auth.record_action = "MODIFY":U AND btt_auth.auth_obj <= 0 ):

    ASSIGN lCheckForRateChange = FALSE.
    
    FIND FIRST buf_memdep NO-LOCK
      WHERE buf_memdep.mem-num = btt_auth.mem_num 
        AND buf_memdep.dependant = btt_auth.dependant
    NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF NOT AVAILABLE buf_memdep THEN
    DO:
      cTrackingMessage = "Rate Change has not been applied due to the Dependant being invalid (Dependant 99)".
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      NEXT AUTH-BLK.
    END. /* IF NOT AVAILABLE buf_memdep THEN */

    /*
      Check if los is enabled for the Auth we are dealing with
    */
    EMPTY TEMP-TABLE ttAuthTypeConfig .
    mipEnv:Health:AuthService:getAuthTypeConfig(BUFFER btt_auth,
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig).
    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.ActivateLos THEN
    DO:
      /*
        Check if we have a main provider and check if the override base and ars rates are populated. If they are, we know a possible rate change may apply.
      */
      FIND FIRST btt_auth_provider NO-LOCK
           WHERE btt_auth_provider.auth_obj = btt_auth.auth_obj
             AND btt_auth_provider.main_provider NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

      IF AVAILABLE btt_auth_provider 
      AND btt_auth_provider.doc_num <> 0 AND btt_auth_provider.pr_type <> 0 THEN
      DO: 
        FIND FIRST buf_auth_provider NO-LOCK
             WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
            
        /*
          Check if we have multiple providers within the same workgroup
        */
        
        RUN _checkRateControlWorkgroup IN TARGET-PROCEDURE ( BUFFER btt_auth_provider ,
                                                             OUTPUT cWorkGroupWarningMessage) . 
        
        /*
          If a workgroup check does not apply, assign the _workgroup_obj on the main provider to 0
          so that when getRateControl is called , the workgroup is not taken in to consideration
        */
       
        IF cWorkGroupWarningMessage = "":U THEN
        DO:
          ASSIGN btt_auth_provider._workgroup_obj = "":U .
          
          /* 
            After _checkRateControlWorkgroup has run we will now run _checkRateControlWorkgroup to check any auth rate control records
            _checkRateControlWorkgroup may have missed
          */
          RUN _checkRateControlWarningMessage IN TARGET-PROCEDURE ( BUFFER btt_auth_provider,
                                                                    OUTPUT cRateControlWarningApplies). 
        END. /* cWorkGroupWarningMessage = "":U THEN */
        
        
        /*
          Only continue if no manual rate change has already taken place.
        */
        IF (AVAILABLE buf_auth_provider
                  AND buf_auth_provider.rate_change_type = "ma_acAuthRateChangeTypeManual":U)
        OR btt_auth_provider.rate_change_type = "ma_acAuthRateChangeTypeManual":U THEN
        DO:
          ASSIGN cTrackingMessage = "Manual rate change applied - Provider " + STRING(btt_auth_provider.doc_num,"9999999").

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          NEXT AUTH-BLK.
        END. /* AVAILABLE buf_auth_provider */

        /*
          Now check if any records have changed that could possibly trigger an automatic rate change.
        */

        /*
          Check if the body region field has been updated on the auth header by comparing the db to the auth tt
        */
        FIND FIRST buf_auth NO-LOCK
             WHERE buf_auth.auth_obj = btt_auth.auth_obj NO-ERROR.

        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

        IF  AVAILABLE buf_auth
        AND btt_auth.body_region <> buf_auth.body_region
        AND CAN-DO("{&ActionList}":U, btt_auth.record_action) THEN
        DO: 
          ASSIGN lCheckForRateChange = TRUE
                 cTrackingMessage    = "Check for Rate Change - Auth header Body region modified".

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END. /* IF  AVAILABLE buf_auth AND btt_auth.body_region <> buf_auth.body_region AND CAN-DO("{&ActionList}":U, btt_auth.record_action)*/

        /*
          Check if member has changed from an invalid member and if we need to redo thecheck for rate change
        */
        IF  AVAILABLE buf_auth
        AND btt_auth.dependant <> buf_auth.dependant 
        AND CAN-DO("{&ActionList}":U, btt_auth.record_action) THEN
        DO: 
          ASSIGN lCheckForRateChange = TRUE
                 cTrackingMessage    = "Check for Rate Change - Auth dependant has been changed and may have changed from 99 to a vaild dependant".

          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END.

        /*
          Provider
        */
        IF CAN-FIND (FIRST tt_auth_provider
        WHERE tt_auth_provider.auth_obj = btt_auth.auth_obj
        AND CAN-DO("{&ModifyList}", tt_auth_provider.record_action)) THEN
        DO:
        
            ASSIGN lCheckForRateChange = TRUE
                   cTrackingMessage    = "Check for Rate Change - Auth provider modified/deleted".
            
            IF (cWorkGroupWarningMessage <> "":U 
                OR cRateControlWarningApplies <> "":U) 
            AND (btt_auth_provider.rate_change_type = "":U
                OR btt_auth_provider.rate_change_type = "ma_acAuthRateChangeTypeNone") THEN
            DO:
              ASSIGN
                  cWarningMessage = IF cWorkGroupWarningMessage <> "":U THEN cWorkGroupWarningMessage ELSE cRateControlWarningApplies
                  lSuccess = goErrorObject:addError
                             (INPUT "hatap",                               /* ipcOwningEntityMnemonic  */
                              INPUT btt_auth_provider.auth_provider_obj,   /* ipdOwningEntityObj       */
                              INPUT "":U,                                  /* ipcOwningEntityKey       */
                              INPUT btt_auth_provider.line_number,         /* ipiLineNumber            */
                              INPUT cWarningMessage,                       /* ipcMessageText           */
                              INPUT "WAR":U,                               /* ipcMessageType           */
                              INPUT TRUE).                                 /* iplAcknowledge           */

            END. /* cWorkGroupWarningMessage <> "":U or cRateControlWarningApplies <> "":U THEN */
            
            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
        END.  /* IF CAN-DO("{&ActionList}":U, btt_auth_provider.record_action) */


        /*
          Coding
        */
        CODING-BLK:
        FOR EACH btt_auth_coding NO-LOCK
            WHERE btt_auth_coding.auth_obj = btt_auth.auth_obj
            AND CAN-DO("{&ActionList}", btt_auth_coding.record_action) :

          FIND FIRST buf_auth_coding NO-LOCK
               WHERE buf_auth_coding.auth_coding_obj = btt_auth_coding.auth_coding_obj NO-ERROR.

          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF (AVAILABLE buf_auth_coding AND CAN-DO("{&ModifyList}",btt_auth_coding.record_action ) AND buf_auth_coding.owning_alt_value <> btt_auth_coding.owning_alt_value)
          OR (AVAILABLE buf_auth_coding AND btt_auth_coding.record_action = "DELETE":U)
          OR (NOT AVAILABLE buf_auth_coding AND CAN-DO("{&ModifyList}",btt_auth_coding.record_action ) ) THEN
          DO:
            ASSIGN lCheckForRateChange = TRUE
                   cTrackingMessage    = "Check for Rate Change - Auth coding modified/deleted".
            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END.  /* IF (AVAILABLE buf_auth_coding AND... */

          LEAVE CODING-BLK.
        END. /* CODING-BLK */

        /*
          Flags
        */
        FLAG-BLK:
        FOR EACH btt_auth_flag_value NO-LOCK
           WHERE btt_auth_flag_value.owning_entity = "hatau":U
           AND btt_auth_flag_value.owning_obj = btt_auth.auth_obj
           AND CAN-DO("{&ActionList}",btt_auth_flag_value.record_action):

          ASSIGN lCheckForRateChange = TRUE
                 cTrackingMessage    = "Check for Rate Change - Auth flag value modified/deleted".
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          LEAVE FLAG-BLK.
        END. /* FLAG-BLK */

        /*
          Crosswalks
        */
        CROSSWALK-BLK:
        FOR EACH btt_auth_crosswalk NO-LOCK
           WHERE btt_auth_crosswalk.auth_obj = btt_auth.auth_obj
             AND CAN-DO("{&ActionList}",btt_auth_crosswalk.record_action):

          ASSIGN lCheckForRateChange = TRUE
                 cTrackingMessage    = "Check for Rate Change - Auth crosswalk modified/deleted".
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          LEAVE CROSSWALK-BLK.
        END. /* CROSSWALK-BLK */

        IF lCheckForRateChange AND btt_auth_provider.record_action <> "DELETE":U THEN
        DO:
          ASSIGN cCurrentOverrideBaseRate = btt_auth_provider.override_base_rate
                 cCurrentOverrideArsRate  = btt_auth_provider.override_ars_rate
                 iCalcTime                = MTIME.

          ASSIGN cTrackingMessage = "Rate Change Triggered - CurrentOverrideBaseRate=" + cCurrentOverrideBaseRate +
                                    " CurrentOverrideArsRate=" + cCurrentOverrideArsRate.
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          mipEnv:Health:maDoctor:getProviderBaseRates( INPUT  btt_auth_provider.doc_num,
                                                       INPUT  btt_auth.mem_num,
                                                       INPUT  btt_auth.option_code,
                                                       INPUT  btt_auth_provider.start_date,
                                                       OUTPUT cDefaultBaseRate,
                                                       OUTPUT cDefaultArsRate).

          ASSIGN cTrackingMessage = "Get Provider Default Rates: DefaultBaseRate=" + cDefaultBaseRate +
                                    " DefaultArsRate=" + cDefaultArsRate + " (Duration=" + STRING(MTIME - iCalcTime) +
                                    " milliseconds)."
                 iCalcTime        = MTIME.
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

          mipEnv:Health:AuthService:getRateControl(INPUT "hatau":U ,
                                                   INPUT btt_auth.auth_obj,
                                                   INPUT FALSE,
                                                   INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                   INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE,
                                                         OUTPUT cOverrideBaseRate,
                                                         OUTPUT cOverrideArsRate,
                                                         OUTPUT cCodeLinkCategory,
                                                         OUTPUT dAuthRateControlObj).

          ASSIGN cTrackingMessage = "Get Rate Control - OverrideBaseRate=" + cOverrideBaseRate +
                                    " OverrideArsRate=" + cOverrideArsRate + " CodeLinkCategory=" + cCodeLinkCategory +
                                    " (Duration=" + STRING(MTIME - iCalcTime) + " milliseconds).".
          {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          /*
            If the override rates are the same as our current override rates. Go no further ,because this would mean no conversions are necessary as the rates have already been applied
          */
          IF  cCurrentOverrideBaseRate = cOverrideBaseRate
          AND cCurrentOverrideArsRate  = cOverrideArsRate
          AND cOverrideBaseRate <> "":U
          AND cOverrideArsRate  <> "":U
          THEN DO:
            ASSIGN cTrackingMessage = "Current Overrides same as Override Rates in Rate Control. No changes necessary".
            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            LEAVE.

          END. /* IF  cCurrentOverrideBaseRate = cOverrideBaseRate  */

          /*
             Rates have been reverted back to the default. Clear the overrides and convert back to original Rates and reset rate change type back to none.
           */
          IF  cDefaultBaseRate = cOverrideBaseRate
          AND cDefaultArsRate  = cOverrideArsRate
          OR (cOverrideBaseRate = "":U
          AND cOverrideArsRate  = "":U)
          AND btt_auth_provider.rate_change_type <> "ma_acAuthRateChangeTypeNone":U THEN
          DO:
            ASSIGN
              btt_auth_provider.default_base_rate  = cDefaultBaseRate
              btt_auth_provider.default_ars_rate   = cDefaultArsRate
              btt_auth_provider.override_base_rate = "":U
              btt_auth_provider.override_ars_rate  = "":U
              btt_auth_provider.rate_change_type   = "ma_acAuthRateChangeTypeNone":U
              btt_auth.auth_incomplete             = TRUE 
              btt_auth.record_action               = "MODIFY":U
              btt_auth_provider.record_action      = "MODIFY":U  .
              
            IF cCurrentOverrideBaseRate <> "":U
            OR cCurrentOverrideArsRate  <> "":U THEN
            DO:
              ASSIGN iCalcTime     = MTIME.

              mipEnv:Health:AuthService:RevertAuthRateConversion(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                                 INPUT  btt_auth.auth_obj ,
                                                                 INPUT  dAuthRateControlObj,
                                                                 INPUT  cCodeLinkCategory  ,
                                                                 INPUT  cCurrentOverrideBaseRate ,
                                                                 INPUT  cCurrentOverrideArsRate  ,
                                                                 INPUT  cDefaultBaseRate,
                                                                 INPUT  cDefaultArsRate).

              ASSIGN cTrackingMessage = "Rates reverted back to default. " +
                                        " DefaultBaseRate=" + cDefaultBaseRate +
                                        " DefaultArsRate=" + cDefaultArsRate + " (Duration=" + STRING(MTIME - iCalcTime) +
                                        " milliseconds).".
              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            END. /* IF cCurrentOverrideBaseRate <> "":U OR... */
          END. /* IF  cDefaultBaseRate = cOverrideBaseRate  AND cDefaultArsRate  = cOverrideArsRate  OR (cOverrideBaseRate = "":U AND cOverrideArsRate  = "":U) */
          ELSE
          DO:
            /*
             This provider's rates are not being reverted back to defaults. If the overrides aren't blank apply them now
            */
            IF (cOverrideBaseRate <> "":U OR cOverrideArsRate <> "":U ) 
			AND (btt_auth_provider.override_base_rate <> cOverrideBaseRate
              OR btt_auth_provider.override_ars_rate  <> cOverrideArsRate
              OR btt_auth_provider.default_base_rate  <> cDefaultBaseRate
              OR btt_auth_provider.default_ars_rate   <> cDefaultArsRate)
			THEN
            DO:
              ASSIGN
                  btt_auth_provider.override_base_rate = cOverrideBaseRate
                  btt_auth_provider.override_ars_rate  = cOverrideArsRate
                  btt_auth_provider.default_base_rate  = cDefaultBaseRate
                  btt_auth_provider.default_ars_rate   = cDefaultArsRate
                  btt_auth_provider.rate_change_type   = "ma_acAuthRateChangeTypeAutomatic":U
                  btt_auth.auth_incomplete             = TRUE 
                  btt_auth.record_action               = "MODIFY":U
                  btt_auth_provider.record_action      = "MODIFY":U
                  iCalcTime                            = MTIME.

              mipEnv:Health:AuthService:applyAuthRateConversion(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                                                INPUT  btt_auth.auth_obj    ,
                                                                INPUT  dAuthRateControlObj  ,
                                                                INPUT  cCodeLinkCategory    ,
                                                                INPUT  cOverrideBaseRate    ,
                                                                INPUT  cOverrideArsRate     ).

              ASSIGN cTrackingMessage = "Automatic Rate Change Applied -" +
                                        " OverrideBaseRate=" + cOverrideBaseRate +
                                        " OverrideArsRate=" + cOverrideArsRate + " (Duration=" + STRING(MTIME - iCalcTime) +
                                        " milliseconds).".
              {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

            END. /* IF (cOverrideBaseRate <> "":U OR cOverrideArsRate <> "":U ) */
          END. /* ELSE */
        END. /* RATE-CHANGE-BLK  */
      END. /* IF AVAILABLE btt_auth_provider  */     
    END. /* IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.ActivateLos */
  END.  /* AUTH-BLK */

  FOR EACH btt_auth NO-LOCK
     WHERE btt_auth.auth_obj > 0
        OR (btt_auth.record_action = "MODIFY":U AND btt_auth.auth_obj <= 0 ):

    /* 
      IMPORTANT - Set the rate change type to none on all providers that do not have a rate_change_type value set
      as we do not want a rate change type of blank
    */
    FOR EACH btt_auth_provider NO-LOCK
       WHERE btt_auth_provider.auth_obj         = btt_auth.auth_obj
         AND btt_auth_provider.rate_change_type = "":
    
       ASSIGN btt_auth_provider.rate_change_type = "ma_acAuthRateChangeTypeNone":U
              btt_auth_provider.record_action    = "MODIFY":U WHEN btt_auth_provider.record_action <> "DELETE":U.
    END. /* For Each btt_auth_provider No-Lock */

  END. /* FOR EACH btt_auth NO-LOCK */
&ENDIF

{mip/inc/mipcatcherror.i}
