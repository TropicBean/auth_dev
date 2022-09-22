/* maauthbussaveauthorisation.i  MEDSTAR Medical Aid System
                                Save Authorisation Record
                                (c) Copyright 1990 - 2022
                                MIP Holdings (Pty) Ltd
                                All rights reserved
SaveAuthorisation procedure include
*/

  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthFlagValue.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER buf_auth_provider   FOR hat_auth_provider.
  DEFINE BUFFER btt_auth            FOR tt_auth.
  DEFINE BUFFER btt_auth_coding     FOR tt_auth_coding.
  DEFINE BUFFER btt_auth_provider   FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail     FOR tt_auth_detail.
  DEFINE BUFFER btt_auth_mc_savings FOR tt_auth_mc_savings.
  DEFINE BUFFER btt_auth_crosswalk  FOR tt_auth_crosswalk.
  DEFINE BUFFER btt_auth_copay      FOR tt_auth_copay.
  DEFINE BUFFER btt_auth_limit      FOR tt_auth_limit.
  DEFINE BUFFER btt_auth_flag_value FOR tt_auth_flag_value.

  DEFINE VARIABLE cAuthNum         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTrackingMessage AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dWIPObj          AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dProviderObj     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iCalcTime        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iCalcSavTime     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iCalcValTime     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lFailureOccurred AS LOGICAL     NO-UNDO.

  ASSIGN goErrorObject   = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

  /*
    Data Access Level Validation
  */
  ASSIGN iCalcTime = MTIME.

  mipEnv:Health:AuthDataAccess:validateAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).

  IF CAN-FIND(FIRST tt_auth_error
              WHERE tt_auth_error.error_type = "ERR":U )
  THEN
    RETURN.

  ASSIGN cTrackingMessage = "Data Access validateAuthorisation completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    Create a copy of the current auth dataset, containing service call data to be updated. Empty the current
    auth dataset, and refill with data from the database. Merge the data from the service call into
    the Auth dataset (containing data from the database). This gives us a fully populated auth dataset
    with records to be updated
  */
  ASSIGN iCalcTime = MTIME.

  ASSIGN cTrackingMessage = "gscUserRole value in BusSaveAuthorisation: ":U + gscUserRole.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  RUN _populateDataset IN TARGET-PROCEDURE.

  ASSIGN cTrackingMessage = "_populateDataset completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U
         iCalcTime        = MTIME.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  RUN _populateAuthFlags IN TARGET-PROCEDURE.

  ASSIGN iCalcTime = MTIME.

  RUN _updateDetailLineDates IN TARGET-PROCEDURE. 

  RUN populateAdditionalData IN TARGET-PROCEDURE(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).

  ASSIGN cTrackingMessage = "populateAdditionalData completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U
         iCalcTime        = MTIME.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    Apply date changes from header to coding, provider and detail lines
    Apply date changes from provider to detail lines
  */
  ASSIGN iCalcTime = MTIME.

  RUN _checkForDateChanges IN TARGET-PROCEDURE.

  ASSIGN cTrackingMessage = "_checkForDateChanges completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U
         iCalcTime = MTIME.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  RUN _setMainAuthProvider IN TARGET-PROCEDURE(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).

  ASSIGN cTrackingMessage = "_setMainAuthProvider completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U
         iCalcTime = MTIME.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  RUN checkForRateChange IN TARGET-PROCEDURE(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).

  ASSIGN cTrackingMessage = "checkForRateChange completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    Calculate LOS
  */
  ASSIGN iCalcTime = MTIME.

  RUN _runLOSCalculations IN TARGET-PROCEDURE.

  ASSIGN cTrackingMessage = "_runLOSCalculations completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    Check for date changes after LOS calculation
  */
  ASSIGN iCalcTime = MTIME.

  RUN _checkForDateChanges IN TARGET-PROCEDURE.

  ASSIGN cTrackingMessage = "_checkForDateChanges after LOS calc completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    LOC User Calc validations
  */
  ASSIGN iCalcTime = MTIME.

  RUN _validateLOSUserCalcs IN TARGET-PROCEDURE.

  ASSIGN cTrackingMessage = "_validateLOSUserCalcs completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /*
    Validate date changes done in checkForDateChanges
  */
  ASSIGN iCalcTime = MTIME.

  RUN _validateDateChanges IN TARGET-PROCEDURE.

  ASSIGN cTrackingMessage = "_validateDateChanges completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  AuthorisationBlock:
  FOR EACH btt_auth NO-LOCK TRANSACTION
        BY auth_obj DESCENDING:

    ASSIGN dWIPObj          = btt_auth.auth_obj
           lFailureOccurred = FALSE
           cAuthNum         = btt_auth.auth_num.

    /*
      Validate/Save Auth Buffer
    */
    IF CAN-DO("{&ActionList}":U, btt_auth.record_action)
    THEN DO:
      /*
        Validate auth temp table record
      */
      ASSIGN iCalcTime = MTIME.

      RUN _validateAuth IN TARGET-PROCEDURE ( BUFFER btt_auth ).

      ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _validateAuth completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      /*
        If there were any business logic validation errors appended to our error temp table for this record then we do not want to continue saving.
      */
      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE goErrorObject:CanFind("hatau":U, btt_auth.auth_obj, "":U, "ERR":U)).

      /*
        Save auth temp table record
      */
      IF NOT lFailureOccurred THEN
      DO:
        ASSIGN iCalcTime = MTIME.

        RUN _saveAuth IN TARGET-PROCEDURE ( BUFFER btt_auth ).

        ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _saveAuth completed in - " + STRING(MTIME - iCalcTime) + " milliseconds":U.
        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

        /*
          The save itself may have resulted in an error
        */
        ASSIGN lFailureOccurred = (IF lFailureOccurred
                                   THEN TRUE
                                   ELSE goErrorObject:CanFind("hatau":U, btt_auth.auth_obj, "":U, "ERR":U)).
      END. /*IF NOT lFailureOccurred THEN*/
    END. /* IF CAN-DO("{&ActionList}":U, btt_auth.record_action) */

    /*
      If we created a new auth record, the dummy auth obj on the temp table will have been
      updated with the generated obj, so we should use that for the child records in the
      section below as they will also have been updated with the relevant auth obj.
    */
    ASSIGN dWIPObj = btt_auth.auth_obj.
    /*
      This is to cater for new auths. At this point all error records related to the auth will have an owning_obj of 0.
      We now have a valid auth_obj and can put this into the owning_obj
    */
    FOR EACH tt_auth_error
        WHERE tt_auth_error.owning_entity = "hatau":U
        AND   tt_auth_error.owning_obj <= 0:

        ASSIGN tt_auth_error.owning_obj = dWIPObj.
    END.

    IF NOT lFailureOccurred THEN
    DO:

      /*
        Focus auth instance object and retrieve relevant auth type dataset for use globally so that each child record will have access to it.
        The btt_auth.auth_obj value will be updated if the record was a create and an obj was generated ( See _populateAuthObj ).
      */

      ASSIGN goAuthorisation = NEW cls.maauthorisation(dWIPObj) .

      IF goAuthorisation:InFocus THEN
      DO:
        ASSIGN goAuthorisation:AuthStatusUpdated = btt_auth._auth_status_updated WHEN AVAILABLE btt_auth .

        goAuthorisation:getAuthTypeDataset(OUTPUT DATASET dsAuthType) NO-ERROR.

      END. /* IF goAuthorisation:InFocus THEN */

      /*
        Create/update the auth flags when
          - no flags exist for the auth yet
          - main provider is flagged to be deleted
          - main provider does not exist
      */
      IF (NOT CAN-FIND(FIRST btt_auth_flag_value
                       WHERE btt_auth_flag_value.owning_entity ="hatau":U
                         AND btt_auth_flag_value.owning_obj = btt_auth.auth_obj )
           OR CAN-FIND(FIRST btt_auth_provider
                       WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
                         AND btt_auth_provider.main_provider = TRUE
                         AND btt_auth_provider.record_action = "DELETE":U )
           OR NOT CAN-FIND(FIRST btt_auth_provider
                           WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
                             AND btt_auth_provider.main_provider ))
      AND goAuthorisation:InFocus
      THEN
        RUN _createDefaultAuthFlags IN TARGET-PROCEDURE (BUFFER btt_auth,                 // Auth buffer
                                                         INPUT TABLE tt_auth_provider ,   // Provider temp table
                                                         INPUT FALSE ).	                  //iplResetToDefaults


    END. /* IF NOT lFailureOccurred THEN */
    ELSE
      NEXT AuthorisationBlock.

    ASSIGN iCalcValTime = 0
           iCalcSavTime = 0.

    /*
      Validate Auth Provider Buffer
    */
    ProviderBlock:
    FOR EACH btt_auth_provider NO-LOCK
       WHERE btt_auth_provider.auth_obj = dWIPObj
          BY btt_auth_provider.auth_provider_obj DESCENDING:
     
      /*
        If we do not have a relevant record action value then we do not want to waste time processing this record any further.
      */
      IF NOT CAN-DO("{&ActionList}":U, btt_auth_provider.record_action) THEN
        NEXT ProviderBlock.

      /*
        Validate auth provider temp table record
      */
      ASSIGN iCalcTime = MTIME.

      RUN _validateAuthProvider IN TARGET-PROCEDURE ( BUFFER btt_auth_provider, BUFFER btt_auth ).

      ASSIGN iCalcValTime = iCalcValTime + (MTIME - iCalcTime).

      /*
        If there were any business logic validation errors appended to our error temp table for this record then we do not want to continue saving.
      */
      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE goErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj, "":U, "ERR":U)).

      /*
        Save auth provider temp table record
      */
      IF NOT lFailureOccurred THEN
      DO:
        ASSIGN iCalcTime = MTIME.

        RUN _saveAuthProvider IN TARGET-PROCEDURE ( BUFFER btt_auth_provider ).

        ASSIGN iCalcSavTime = iCalcSavTime + (MTIME - iCalcTime).
        /*
          The save itself may have resulted in an error
        */
        ASSIGN lFailureOccurred = (IF lFailureOccurred
                                   THEN TRUE
                                   ELSE goErrorObject:CanFind("hatap":U, btt_auth_provider.auth_provider_obj, "":U, "ERR":U)).
      END. /*IF NOT lFailureOccurred THEN*/
    END. /*FOR EACH btt_auth_provider NO-LOCK:*/

    EMPTY TEMP-TABLE tt_provider_count.

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _validateAuthProvider completed in - " + STRING(iCalcValTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _saveAuthProvider completed in - "     + STRING(iCalcSavTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN iCalcValTime = 0
           iCalcSavTime = 0.

    /*
      Validate/Save Auth Coding Buffer
    */
    CodingBlock:
    FOR EACH btt_auth_coding NO-LOCK
       WHERE btt_auth_coding.auth_obj = dWIPObj
          BY auth_coding_obj DESCENDING:
      /*start saving existing records
        new records will have  highest obj value in the db
        and also in the temp table
        the next obj to iterate will be lower value obj and wont re iterate the newely created record again
        this will also helps with auth save speed*/


      /*
        If we do not have a relevant record action value then we do not want to waste time processing this record any further.
      */
      IF NOT CAN-DO("{&ActionList}":U, btt_auth_coding.record_action)
      THEN
        NEXT CodingBlock.

      /*
        Validate auth coding temp table record
        Even if an error has already occurred somewhere for this auth we want to collect ALL the errors
      */
      ASSIGN iCalcTime = MTIME.

      RUN _validateAuthCoding IN TARGET-PROCEDURE( BUFFER btt_auth_coding ).

      ASSIGN iCalcValTime = iCalcValTime + (MTIME - iCalcTime).

      /*
        If there were any business logic validation errors appended to our error temp table for this record then we do not want to continue saving.
      */
      // MMP-662 to get the error object to work correctly we just needed to add btt_auth_coding.owning_alt_value so that there is context
      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE goErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":" + btt_auth_coding.owning_alt_value, btt_auth_coding.auth_coding_obj, "":U, "ERR":U)).
      /*
        Save auth coding temp table record if an error hasnt occurred for this auth
      */
      IF NOT lFailureOccurred THEN
      DO:
        ASSIGN iCalcTime = MTIME.

        RUN _saveAuthCoding IN TARGET-PROCEDURE( BUFFER btt_auth_coding ).

        ASSIGN iCalcSavTime = iCalcSavTime + (MTIME - iCalcTime).

        /*
          The save itself may have resulted in an error
        */
        ASSIGN lFailureOccurred = (IF lFailureOccurred
                                   THEN TRUE
                                   ELSE goErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj, "":U, "ERR":U)).
      END. /*IF NOT lFailureOccurred THEN*/
    END. /*FOR EACH btt_auth_coding NO-LOCK:*/

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _validateAuthCoding completed in - " + STRING(iCalcValTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _saveAuthCoding completed in - "     + STRING(iCalcSavTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN iCalcValTime                   = 0
           iCalcSavTime                   = 0
           glSkipDetailLineValuesProcess  = FALSE.


    DetailBlock:
    DO WHILE TRUE:

      FIND FIRST btt_auth_detail
        WHERE btt_auth_detail.auth_obj          = dWIPObj
        AND   CAN-DO("{&ActionList}":U, btt_auth_detail.record_action)
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE btt_auth_detail
      THEN
        LEAVE DetailBlock.

      FIND FIRST buf_auth_provider
           WHERE buf_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      FIND FIRST btt_auth_provider
           WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE buf_auth_provider
      THEN
        NEXT DetailBlock.

      IF dProviderObj <> btt_auth_detail.auth_provider_obj THEN
      DO:

        ASSIGN dProviderObj = btt_auth_detail.auth_provider_obj .

        EMPTY TEMP-TABLE ttAuthTypeConfig.

        mipEnv:Health:AuthService:getAuthTypeConfig (BUFFER btt_auth,
                                                     BUFFER btt_auth_provider ,
                                                     INPUT-OUTPUT TABLE ttAuthTypeConfig).

        FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      END. //IF dProviderObj <> btt_auth_detail.auth_provider_obj

      /*
        Validate auth detail temp table record
        Even if an error has already occurred somewhere for this auth we want to collect ALL the errors
      */
      ASSIGN iCalcTime = MTIME.

      RUN _validateAuthDetail IN TARGET-PROCEDURE( BUFFER btt_auth_detail, BUFFER btt_auth_provider, BUFFER btt_auth ).

      ASSIGN iCalcValTime = iCalcValTime + (MTIME - iCalcTime).

      /*
        If there were any business logic validation errors appended to our error temp table for this record then we do not want to continue saving.
      */
      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE goErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U, "ERR":U)).

      /*
        Save auth detail temp table record if an error hasnt occurred for this auth
      */
      IF NOT lFailureOccurred THEN
      DO:
        ASSIGN iCalcTime = MTIME.

        RUN _saveAuthDetail IN TARGET-PROCEDURE( BUFFER btt_auth_detail ).

        IF AVAILABLE btt_auth_detail THEN
          ASSIGN btt_auth_detail.record_action = "":u.

        ASSIGN iCalcSavTime = iCalcSavTime + (MTIME - iCalcTime).

        /*
          The save itself may have resulted in an error
        */
        ASSIGN lFailureOccurred = (IF lFailureOccurred
                                   THEN TRUE
                                   ELSE AVAILABLE btt_auth_detail AND  goErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U, "ERR":U)).
      END.  /* IF NOT lFailureOccurred THEN */
      ELSE
        LEAVE DetailBlock.
    END.  /* DetailBlock: DO WHILE TRUE: */

    RUN _DetermineAuthAutoDetails IN TARGET-PROCEDURE.

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _validateAuthDetail completed in - " + STRING(iCalcValTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _saveAuthDetail completed in - "     + STRING(iCalcSavTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN iCalcValTime = 0
           iCalcSavTime = 0.

    /*
      Validate/Save Auth Managed Care Savings Buffer
    */
    SavingsBlock:
    FOR EACH btt_auth_mc_savings NO-LOCK
       WHERE btt_auth_mc_savings.auth_obj = dWIPObj
          BY auth_mc_savings_obj DESCENDING:

      /*
        If we do not have a relevant record action value then we do not want to waste time processing this record any further.
      */
      IF NOT CAN-DO("{&ActionList}":U, btt_auth_mc_savings.record_action)
      THEN
        NEXT SavingsBlock.

      /*
        Validate auth detail temp table record
        Even if an error has already occurred somewhere for this auth we want to collect ALL the errors
      */
      ASSIGN iCalcTime = MTIME.

      RUN _validateAuthMCSavings IN TARGET-PROCEDURE( BUFFER btt_auth_mc_savings ).

      ASSIGN iCalcValTime = iCalcValTime + (MTIME - iCalcTime).

      /*
        If there were any business logic validation errors appended to our error temp table for this record then we do not want to continue saving.
      */
      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE goErrorObject:CanFind("hatms":U, btt_auth_mc_savings.auth_mc_savings_obj, "":U, "ERR":U)).

      /*
        Save auth mc savings temp table record
      */
      IF NOT lFailureOccurred THEN
      DO:
        ASSIGN iCalcTime = MTIME.

        RUN _saveAuthMCSavings IN TARGET-PROCEDURE( BUFFER btt_auth_mc_savings ).

        ASSIGN iCalcSavTime = iCalcSavTime + (MTIME - iCalcTime).

        /*
          The save itself may have resulted in an error
        */
        ASSIGN lFailureOccurred = (IF lFailureOccurred
                                   THEN TRUE
                                   ELSE goErrorObject:CanFind("hatms":U, btt_auth_mc_savings.auth_mc_savings_obj, "":U, "ERR":U)).
      END. /*IF NOT lFailureOccurred THEN*/
    END. /*FOR EACH btt_auth_mc_savings NO-LOCK:*/

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _validateAuthMCSavings completed in - " + STRING(iCalcValTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _saveAuthMCSavings completed in - "     + STRING(iCalcSavTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    /*
      Validate/Save Auth Co-payment Buffer
    */
    CopayBlock:
    FOR EACH btt_auth_copay NO-LOCK
       WHERE btt_auth_copay.auth_obj = dWIPObj
          BY auth_copay_obj DESCENDING:

      /*
        If we do not have a relevant record action value then we do not want to waste time processing this record any further.
      */
      IF NOT CAN-DO("{&ActionList}":U, btt_auth_copay.record_action)
      THEN
        NEXT CopayBlock.

      /*
        Validate auth detail temp table record
        Even if an error has already occurred somewhere for this auth we want to collect ALL the errors
      */
      ASSIGN iCalcTime = MTIME.

      RUN _validateAuthCopay IN TARGET-PROCEDURE( BUFFER btt_auth_copay ).

      ASSIGN iCalcValTime = iCalcValTime + (MTIME - iCalcTime).

      /*
        If there were any business logic validation errors appended to our error temp table for this record then we do not want to continue saving.
      */
      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE goErrorObject:CanFind("hatcp":U, btt_auth_copay.auth_copay_obj, "":U, "ERR":U)).

      /*
        Save auth mc savings temp table record
      */
      IF NOT lFailureOccurred THEN
      DO:
        ASSIGN iCalcTime = MTIME.

        RUN _saveAuthCopay IN TARGET-PROCEDURE( BUFFER btt_auth_copay ).

        ASSIGN iCalcSavTime = iCalcSavTime + (MTIME - iCalcTime).

        /*
          The save itself may have resulted in an error
        */
        ASSIGN lFailureOccurred = (IF lFailureOccurred
                                   THEN TRUE
                                   ELSE goErrorObject:CanFind("hatcp":U, btt_auth_copay.auth_copay_obj, "":U, "ERR":U)).
      END. /*IF NOT lFailureOccurred THEN*/
    END. /*FOR EACH btt_auth_copay NO-LOCK:*/

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _validateAuthCopay completed in - " + STRING(iCalcValTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _saveAuthCopay completed in - "     + STRING(iCalcSavTime) + " milliseconds":U.
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}


    CrosswalkBlock:
    FOR EACH btt_auth_crosswalk
          BY auth_crosswalk_obj DESCENDING:
      /*
        If we do not have a relevant record action value then we do not want to waste time processing this record any further.
      */
      IF NOT CAN-DO("{&ActionList}":U, btt_auth_crosswalk.record_action)
      THEN
        NEXT CrosswalkBlock.


      /*
        If there were any business logic validation errors appended to our error temp table for this record then we do not want to continue saving.
      */
      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE goErrorObject:CanFind("hataw":U , btt_auth_crosswalk.auth_crosswalk_obj, "":U, "ERR":U)).


      IF NOT lFailureOccurred THEN
      DO:
        RUN _saveAuthCrosswalk IN TARGET-PROCEDURE (BUFFER btt_auth_crosswalk ) .

        /*
          The save itself may have resulted in an error
        */

        ASSIGN lFailureOccurred = IF lFailureOccurred
                                  THEN TRUE
                                  ELSE goErrorObject:CanFind("hataw":U , btt_auth_crosswalk.auth_crosswalk_obj , "":U , "ERR":U ) .

      END. /* IF NOT lFailureOccurred*/

    END. /* FOR EACH btt_auth_crosswalk*/

    /*
      Retrieve the limits applicable to this auth - only on final save
    */
    IF NOT btt_auth.auth_incomplete
    THEN
      RUN buildAuthLimitTT IN TARGET-PROCEDURE (INPUT btt_auth.auth_obj ,
                                                INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE ) .


    /*
      Now that we have the limit dataset , we can start saving it to the db
    */
    LimitBlock:
    FOR EACH btt_auth_limit
       WHERE btt_auth_limit.auth_obj = btt_auth.auth_obj :

      /*
        If we do not have a relevant record action value then we do not want to waste time processing this record any further.
      */
      IF NOT CAN-DO("{&ActionList}":U, btt_auth_limit.record_action)
      THEN
        NEXT LimitBlock.

      ASSIGN lFailureOccurred = (IF lFailureOccurred
                                 THEN TRUE
                                 ELSE (   goErrorObject:CanFind("hatau":U, btt_auth.auth_obj, "":U, "ERR":U))
                                       OR goErrorObject:CanFind("hatal":U, btt_auth_limit.auth_limit_obj, "":U, "ERR":U)).
      /*
        Save auth limit temp table record
      */
      IF NOT lFailureOccurred THEN
      DO:
        ASSIGN iCalcTime = MTIME.

        RUN _saveAuthLimit IN TARGET-PROCEDURE( BUFFER btt_auth_limit ).

        ASSIGN iCalcSavTime = iCalcSavTime + (MTIME - iCalcTime).

        /*
          The save itself may have resulted in an error
        */
        ASSIGN lFailureOccurred = (IF lFailureOccurred
                                   THEN TRUE
                                   ELSE goErrorObject:CanFind("hatal":U, btt_auth_limit.auth_limit_obj, "":U, "ERR":U)).
      END. /*IF NOT lFailureOccurred THEN*/
    END. //LimitBlock

     ASSIGN cTrackingMessage = "Auth " + cAuthNum + ": _saveAuthLimit completed in - "     + STRING(iCalcSavTime) + " milliseconds":U.

    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    DATASET dsAuthType:EMPTY-DATASET().

    /*
      It wouldnt make sense to save some records if others resulted in an error so lets undo anything
      which might have saved for this Authorization before an error occurred on another record related to
      this Authorization.
    */
    IF lFailureOccurred
    THEN
      UNDO AuthorisationBlock, NEXT AuthorisationBlock.

  END. /*FOR EACH btt_auth NO-LOCK:*/

&ENDIF

  { mip/inc/mipcatcherror.i
    &FINALLY = "DATASET dsAuthType:EMPTY-DATASET().
                EMPTY TEMP-TABLE tt_auth_flag_value.
                IF VALID-OBJECT(goErrorObject)   THEN DELETE OBJECT goErrorObject. "}

