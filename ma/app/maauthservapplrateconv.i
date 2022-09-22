/* maauthservapplrateconv.i MEDSTAR Medical Aid System
                            Healthcare Auth data access service: Rate Conversion                              
                            (c) Copyright 2018 - 2021
                            MIP Holdings (Pty) Ltd
                            All rights reserved
------------------------------------------------------------------------------
  Purpose: Apply rate conversion to Authorisation detail lines
  Parameters:  Authorisation dataset
               Auth obj
               Auth rate control obj
               Code Link Category
               Override base rate
               Override ars rate
  Notes:  This method converts auth detail lines in the dataset and returns the dataset
          with converted values.
          New detail line records will be created for converted tariffs and record
          action will be set to "MODIFY". Detail lines that need to be deleted will
          have record action set to "DELETE".
          
          If Provider defaults/overrides need to be applied, be sure to pass in 
          a valid auth rate control obj . 
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthorisation.
  DEFINE INPUT  PARAMETER ipdAuthObj                   AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipdAuthRateControlObj        AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER ipcCodeLinkCategory          AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcOverrideBaseRate          AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcOverrideArsRate           AS CHARACTER NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER btt_auth               FOR tt_auth.
  DEFINE BUFFER btt_auth_provider      FOR tt_auth_provider.
  DEFINE BUFFER bbt_auth_provider      FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail        FOR tt_auth_detail.
  DEFINE BUFFER buf_code_link          FOR hlm_code_link.
  DEFINE BUFFER btt_temp_auth_detail   FOR tt_temp_auth_detail.
  DEFINE BUFFER buf_memdep             FOR memdep.

  DEFINE VARIABLE lCodeLinkFound          AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lSuccess                AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lErrors                 AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE dCodeLinkChildObj       AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dDummyObj               AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dNewTariffLinkObj       AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj          AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTariffObj              AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj             AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE cAddValidations         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cAlertMessage           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cCodeLinkCategory       AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cError                  AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cMessage                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarnDetailLineDeletion AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning                AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cPrType                 AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTLArsRate              AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTLBaseRate             AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTLPrType               AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cTrackingMessage        AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE l99Dependant            AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE oErrorObject            AS cls.maerrorobject     NO-UNDO.

  ASSIGN oErrorObject           = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE)
         cCodeLinkCategory      = ipcCodeLinkCategory.

  FIND FIRST btt_auth NO-LOCK
       WHERE btt_auth.auth_obj = ipdAuthObj NO-ERROR.

  { mip/inc/mipthrowerror.i }

  EMPTY TEMP-TABLE tt_temp_auth_detail.
  
  /*
    Apply auth rate provider default/overrides and create linked entities
  */
  IF  ipdAuthRateControlObj  > 0 
  AND ipdAuthRateControlObj <> ?
  AND AVAILABLE btt_auth  THEN 
  DO:
    RUN _applyAuthRateProviders(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                INPUT ipdAuthObj,
                                INPUT ipdAuthRateControlObj).

    RUN _applyAuthRateCoding(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                             INPUT ipdAuthObj,
                             INPUT ipdAuthRateControlObj).

  END. /* IF  ipdAuthRateControlObj  > 0  AND ipdAuthRateControlObj <> ? */
  
  FIND FIRST btt_auth_provider NO-LOCK
       WHERE btt_auth_provider.auth_obj      = btt_auth.auth_obj
         AND btt_auth_provider.main_provider = TRUE NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  /*
     It can be that the auth provider was changed so that it is no longer marked as the main provider.
     This will also trigger a rate control change, in which case we want to make sure that the tariffs are
     still valid for the default rates.
     If we're going to do another story for reverting, then we might want to cater for this scenario in that task.
  */
  IF NOT AVAILABLE btt_auth_provider THEN
  DO:
    FOR EACH bbt_auth_provider NO-LOCK:
      IF CAN-DO("{&ActionList}":U, bbt_auth_provider.record_action) THEN
      DO:
        IF CAN-FIND(FIRST hat_auth_provider NO-LOCK
                    WHERE hat_auth_provider.auth_provider_obj = bbt_auth_provider.auth_provider_obj
                      AND hat_auth_provider.main_provider     = TRUE) THEN
          FIND FIRST btt_auth_provider NO-LOCK
               WHERE btt_auth_provider.auth_provider_obj = bbt_auth_provider.auth_provider_obj
               NO-ERROR.
      END.  // IF CAN-DO("{&ActionList}":U, btt_auth_provider.record_action) AND...
    END.  // FOR EACH btt_auth_provider NO-LOCK
  END.  // IF NOT AVAILABLE btt_auth_provider THEN

  IF AVAILABLE btt_auth_provider THEN
  DO:
    /*
      Run through all the tariffs in the auth detail and check if there are active
      code link setups for the tariff code, base rate, ars rate and discipline.
    */
    AUTH-DETAIL-BLK:
    FOR EACH btt_auth_detail NO-LOCK
       WHERE btt_auth_detail.auth_provider_obj      = btt_auth_provider.auth_provider_obj
         AND btt_auth_detail.owning_entity_mnemonic = "htmtl":U
         AND (btt_auth_detail.quantity_los         <> 0
          OR (btt_auth_detail.quantity_los = 0 AND btt_auth_detail.related_entity_mnemonic <> "hatad":U))
         AND btt_auth_detail.record_action         <> "DELETE":U:

      ASSIGN lCodeLinkFound = FALSE.

      /*
        If there are code link setups for a tariff, we need to
        convert the parent tariff to the new child tariff.
      */
      CODE-LINK-BLK:
      FOR EACH buf_code_link NO-LOCK
         WHERE buf_code_link.acronym_key       = cCodeLinkCategory
           AND buf_code_link.parent_entity     = btt_auth_detail.owning_entity_mnemonic
           AND buf_code_link.parent_entity_obj = btt_auth_detail.owning_obj
           AND buf_code_link.child_entity      = btt_auth_detail.owning_entity_mnemonic
           AND buf_code_link.effective_date   <= btt_auth_detail.start_date :

        /*
          The code link setup must be active
        */
        IF  buf_code_link.end_date <> ?
        AND buf_code_link.end_date < btt_auth_detail.start_date THEN
          NEXT CODE-LINK-BLK.

        /*
          Ensure that the child tariff is a valid tariff link obj, otherwise we won't know
          what the base rate, ars rate and pr type for the setup tariff is.
        */
        RUN _findTariffLink (buf_code_link.child_entity_obj,"","","","",
                             OUTPUT dTariffLinkObj, OUTPUT cTLBaseRate, OUTPUT cTLArsRate, OUTPUT cTLPrType).
        IF dTariffLinkObj = 0 THEN
          NEXT CODE-LINK-BLK.

        ASSIGN cPrType = STRING(btt_auth_provider.pr_type,"999").

        { ma/msc/madispad.i &discipline = cPrType }

        /*
           If we have a tariff link that is not a default, check that it matches
           the base rate, ARS rate and pr type we are converting to.
        */
        IF (cTLBaseRate <> "":U  AND cTLBaseRate <> ipcOverrideBaseRate)
        OR (cTLArsRate  <> "":U  AND cTLArsRate  <> ipcOverrideArsRate)
        OR (cTLPrType   <> "":U  AND cTLPrType   <> cPrType) THEN
          NEXT CODE-LINK-BLK.

        /*
          If this is a default tariff link, let's find a tariff link
          matching the base rate, ARS rate and pr type.
        */
        IF  cTLBaseRate = "":U
        AND cTLArsRate  = "":U
        AND cTLPrType   = "":U THEN
        DO:
          RUN _findTariffLink (0,buf_code_link.child_alt_value,ipcOverrideBaseRate,ipcOverrideArsRate,cPrType,
                               OUTPUT dTariffLinkObj, OUTPUT cTLBaseRate, OUTPUT cTLArsRate, OUTPUT cTLPrType).
          
          IF dTariffLinkObj = 0 THEN
            NEXT CODE-LINK-BLK.
        END. //IF cTLBaseRate = "":U AND....

        /*
          We're using the temp-table btt_temp_auth_detail to create all the new auth detail lines.
          Once all the new auth detail lines were created and the old detail lines were deleted, we
          will copy the new detaillines to the tt_auth_detail temp-table before saving it to the db.
        */
        IF NOT CAN-FIND( FIRST btt_temp_auth_detail NO-LOCK
                         WHERE btt_temp_auth_detail.auth_provider_obj      = btt_auth_detail.auth_provider_obj
                           AND btt_temp_auth_detail.owning_entity_mnemonic = buf_code_link.child_entity
                           AND btt_temp_auth_detail.owning_obj             = dTariffLinkObj
                           AND btt_temp_auth_detail.owning_key             = buf_code_link.child_alt_value
                           AND btt_temp_auth_detail.start_date             = btt_auth_detail.start_date) THEN
        DO:
          ASSIGN dTariffLinkObj  = 0
                 dTariffObj      = 0
                 dTrfCostObj     = 0
                 cError          = "":U
                 cWarning        = "":U
                 cAlertMessage   = "":U.

          IF AVAILABLE btt_auth THEN
          DO:
            FIND FIRST buf_memdep NO-LOCK
                 WHERE buf_memdep.mem-num = btt_auth.mem_num 
                   AND buf_memdep.dependant = btt_auth.dependant
            NO-ERROR.

            {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
            
            IF NOT AVAILABLE buf_memdep THEN
            DO:
              ASSIGN
                l99Dependant = TRUE.
            END. /* IF NOT AVAILABLE buf_memdep THEN */
          END. /* IF AVAILABLE btt_auth THEN */

          IF NOT l99Dependant 
          THEN
            ASSIGN     
              cAddValidations = "age,":U     + STRING(btt_auth._dependant_age_years) +
                                "|gender,":U + btt_auth._dependant_gender           +
                                "|bmi,":U    + STRING(btt_auth._dependant_bmi).
          ELSE
            ASSIGN
              cAddValidations = "":U.

          mipEnv:health:mamedical:getValidTariff( INPUT-OUTPUT dTariffLinkObj,                // iopdTariffLinkObj
                                                  INPUT        buf_code_link.child_alt_value, // ipcTariffCode
                                                  INPUT        ipcOverrideBaseRate,           // ipcBaseRate
                                                  INPUT        ipcOverrideArsRate ,           // ipcARSRate
                                                  INPUT        btt_auth_provider.pr_type ,    // ipiPrType
                                                  INPUT        btt_auth_provider.sub_pr_type, // ipiSubPrType
                                                  INPUT        btt_auth_detail.start_date,    // ipdDate
                                                  INPUT        btt_auth.option_code,          // ipiOptionCode
                                                  INPUT        cAddValidations,               // ipcAddValidations
                                                  OUTPUT       dTariffObj,                    // opdTariffObj
                                                  OUTPUT       dTrfCostObj,                   // opdTrfCostObj
                                                  OUTPUT       cError,                        // opcError
                                                  OUTPUT       cWarning,                      // opcWarning
                                                  OUTPUT       cAlertMessage).

          // If tariff is invalid, skip creation of detail line
          IF cError <> "":U THEN
          DO:
            ASSIGN lErrors = TRUE.

            oErrorObject:addError(INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic, // ipcOwningEntityMnemonic
                                  INPUT btt_auth_detail.auth_detail_obj,                   // ipdOwningEntityObj
                                  INPUT "":U,                                              // ipcOwningEntityKey
                                  INPUT btt_auth_detail.line_number,                       // ipiLineNumber
                                  INPUT SUBSTITUTE("Invalid Code Link set up. Tariff code &1 could not be converted.",btt_auth_detail.owning_alt_value) + cError).   // ipcMessageText

            NEXT CODE-LINK-BLK.
          END.  // IF cError <> "" THEN

          CREATE btt_temp_auth_detail.

          BUFFER-COPY btt_auth_detail EXCEPT btt_auth_detail.auth_detail_obj
                                             btt_auth_detail.owning_obj
                                             btt_auth_detail.owning_alt_value
                                             btt_auth_detail.loc_sequence
                   TO btt_temp_auth_detail.

          ASSIGN dDummyObj                              = dDummyObj - 1
                 btt_temp_auth_detail.auth_detail_obj   = dDummyObj
                 btt_temp_auth_detail.owning_obj        = dTariffLinkObj
                 btt_temp_auth_detail.owning_alt_value  = buf_code_link.child_alt_value
                 btt_temp_auth_detail.owning_key        = buf_code_link.child_alt_value
                 btt_temp_auth_detail.record_action     = "MODIFY":U
                 btt_temp_auth_detail.loc_sequence      = btt_auth_detail.loc_sequence * 100

                 lCodeLinkFound = TRUE.

          /*
	    If the old tariff was linked to a modifier, we don't 
	    want to copy the link over to the new tariff as well.
	  */
	  IF btt_temp_auth_detail.related_entity_mnemonic = "hatad":U 
	  THEN
            ASSIGN btt_temp_auth_detail.related_entity_mnemonic = "":U
                   btt_temp_auth_detail.related_obj             = 0
                   btt_temp_auth_detail.related_value           = "":U.
          
          VALIDATE btt_temp_auth_detail.
        END. // IF NOT AVAILABLE btt_temp_auth_detail
      END. // CODE-LINK-BLK

      // If we can't find any code links, try to check if the current tariff code is valid for the override base and ars rates
      ASSIGN cTrackingMessage = "Apply Rate Conversion: Code link found? " + STRING(lCodeLinkFound) + " (OwningObj=" + STRING(btt_auth_detail.owning_obj) +
                                " OwningAltValue=" + btt_auth_detail.owning_alt_value + " LocSeq=" +
                                STRING(btt_auth_detail.loc_sequence) + ")".
      {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

      IF NOT lCodeLinkFound
      AND btt_auth_detail.owning_obj <> 0 THEN
      DO:
        // Fetch tariff link record currently linked as owning record so we can access our tariff code
        ASSIGN dNewTariffLinkObj = 0.

        IF btt_auth_detail.owning_alt_value <> "":U THEN
        DO:
          ASSIGN cAddValidations = "age,":U     + STRING(btt_auth._dependant_age_years) +
                                   "|gender,":U + btt_auth._dependant_gender           +
                                   "|bmi,":U    + STRING(btt_auth._dependant_bmi).
          mipEnv:health:mamedical:getValidTariff( INPUT-OUTPUT dNewTariffLinkObj,                // iopdTariffLinkObj
                                                  INPUT        btt_auth_detail.owning_alt_value, // ipcTariffCode
                                                  INPUT        ipcOverrideBaseRate,              // ipcBaseRate
                                                  INPUT        ipcOverrideArsRate ,              // ipcARSRate
                                                  INPUT        btt_auth_provider.pr_type ,       // ipiPrType
                                                  INPUT        btt_auth_provider.sub_pr_type,    // ipiSubPrType
                                                  INPUT        btt_auth_detail.start_date,       // ipdDate
                                                  INPUT        btt_auth.option_code,             // ipiOptionCode
                                                  INPUT        cAddValidations,                  // ipcAddValidations
                                                  OUTPUT       dTariffObj,                       // opdTariffObj
                                                  OUTPUT       dTrfCostObj,                      // opdTrfCostObj
                                                  OUTPUT       cError,                           // opcError
                                                  OUTPUT       cWarning,                         // opcWarning
                                                  OUTPUT       cAlertMessage).                   // opcAlert

          IF cError = "":U AND dNewTariffLinkObj <> 0 THEN
          DO:
            ASSIGN btt_auth_detail.owning_obj    = dNewTariffLinkObj
                   btt_auth_detail.record_action = "MODIFY":U
                   btt_auth_detail.loc_sequence  = btt_auth_detail.loc_sequence * 100
                   cTrackingMessage = "Apply Rate Conversion: Valid Tariff - Update Owning Obj (OwningObj=" + 
                                      STRING(dNewTariffLinkObj) + " OwningAltValue=" + 
                                      btt_auth_detail.owning_alt_value + " LocSeq=" +
                                      STRING(btt_auth_detail.loc_sequence) + ")".

            VALIDATE btt_auth_detail.
            
            {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
          END.  // IF cError = "":U
          ELSE IF cError <> "":U THEN
            ASSIGN cWarnDetailLineDeletion = cWarnDetailLineDeletion + "," + btt_auth_detail.owning_alt_value. 
        END.  // IF btt_auth_detail.owning_alt_value <> "":U
        ELSE
          ASSIGN cWarnDetailLineDeletion = cWarnDetailLineDeletion + "," + btt_auth_detail.owning_alt_value.
      END.  // IF NOT lCodeLinkFound

      /*
        Finally , set record action to delete on the current auth detail line. 
        If a code link or valid tariff was found, a new detail line would have been created in tt_temp_auth_detail
      */
      IF cWarnDetailLineDeletion <> ""
      OR (lCodeLinkFound AND NOT lErrors) THEN
      DO:
        ASSIGN btt_auth_detail.record_action = "DELETE":U
               btt_auth_detail.rate_change   = TRUE
               cTrackingMessage = "Apply Rate Conversion: Delete Detail line (OwningObj=" + STRING(btt_auth_detail.owning_obj) +
                                  " OwningAltValue=" + btt_auth_detail.owning_alt_value + " LocSeq=" +
                                  STRING(btt_auth_detail.loc_sequence) + ") cWarnDetailLineDeletion" +
                                  cWarnDetailLineDeletion + " lCodeLinkFound=" + STRING(lCodeLinkFound) +
                                  " lErrors=" + STRING(lErrors).
        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      END. // IF cWarnDetailLineDeletion <> "" OR...
    END.  // AUTH-DETAIL-BLK

    // Now merge our changes in btt_temp_auth_detail in to tt_auth_detail in our current dataset
    FOR EACH btt_temp_auth_detail NO-LOCK
       WHERE btt_temp_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj:

      FIND FIRST btt_auth_detail EXCLUSIVE-LOCK
           WHERE btt_auth_detail.auth_provider_obj      = btt_temp_auth_detail.auth_provider_obj
             AND btt_auth_detail.owning_entity_mnemonic = btt_temp_auth_detail.owning_entity_mnemonic
             AND btt_auth_detail.owning_obj             = btt_temp_auth_detail.owning_obj
             AND btt_auth_detail.owning_key             = btt_temp_auth_detail.owning_key
             AND btt_auth_detail.start_date             = btt_temp_auth_detail.start_date
           NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE btt_auth_detail 
      THEN DO:
        CREATE btt_auth_detail.
        
        ASSIGN cTrackingMessage = "Apply Rate Conversion: Create Detail line (OwningObj/TariffLinkObj=" + STRING(btt_temp_auth_detail.owning_obj) +
                                  " OwningAltValue=" + btt_temp_auth_detail.owning_alt_value +
                                  " LocSeq=" + STRING(btt_temp_auth_detail.loc_sequence) + ")".
                                  
        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}                                  
      END. /* IF NOT AVAILABLE btt_auth_detail  */

      BUFFER-COPY btt_temp_auth_detail
               TO btt_auth_detail .

    END.  // FOR EACH btt_temp_auth_detail

    /*
      If we were not able to find a code link or a valid tariff for a detail line, we  delete the detail lines completely. In
      such a case we will flag cWarnDetailLineDeletion as true.Add a warning to notify the user that detail lines have been deleted
    */
    ASSIGN cTrackingMessage = "Apply Rate Conversion: End - cWarnDetailLineDeletion=" + 
                              cWarnDetailLineDeletion + " lErrors=" + STRING(lErrors).
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

    IF cWarnDetailLineDeletion <> "" THEN
      ASSIGN
        cMessage  = "Detail lines for tariffs " + TRIM(cWarnDetailLineDeletion,",") + " could not be converted, and consequently they were deleted."
        lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                          INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                          INPUT "":U,                 // ipcOwningEntityKey
                                          INPUT btt_auth.line_number, // ipiLineNumber
                                          INPUT cMessage,             // ipcMessageText
                                          INPUT "WAR":U).             // ipcMessageType
    // Notify the user that the rate change has been completed succesfully
    IF CAN-FIND(FIRST btt_auth_detail
                WHERE btt_auth_detail.auth_provider_obj = btt_auth_provider.auth_provider_obj
                  AND btt_auth_detail.record_action     = "MODIFY":U )THEN
      ASSIGN
        cMessage  = IF NOT lErrors
                    THEN "Rate conversion was completed succesfully."
                    ELSE "Due to errors Rate conversion was not completed."
        lSuccess  = oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                          INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                          INPUT "":U,                 // ipcOwningEntityKey
                                          INPUT btt_auth.line_number, // ipiLineNumber
                                          INPUT cMessage,             // ipcMessageText
                                          INPUT "WAR":U).             // ipcMessageType

  END.  // IF AVAILABLE btt_auth_provider
  ELSE DO:
    ASSIGN cTrackingMessage = "Apply Rate Conversion: No main provider for auth# " + btt_auth.auth_num + 
                              " (AuthObj=" + STRING(btt_auth.auth_obj) + ")".
    {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
    
      // No main provider was found
    ASSIGN
      cMessage = "No rate conversion could be applied, because no valid Main provider was found for the Authorisation."
      lSuccess = oErrorObject:addError(INPUT 'hatau' ,             // ipcOwningEntityMnemonic
                                       INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                       INPUT "":U,                 // ipcOwningEntityKey
                                       INPUT btt_auth.line_number, // ipiLineNumber
                                       INPUT cMessage,             // ipcMessageText
                                       INPUT "WAR":U).             // ipcMessageType
  END.  // ELSE - IF AVAILABLE btt_auth_provider THEN

  {mip/inc/mipcatcherror.i
    &FINALLY=" IF VALID-OBJECT(oErrorObject)           THEN DELETE OBJECT oErrorObject.
               DATASET dsAuthRateControl:EMPTY-DATASET()."}
&ENDIF
