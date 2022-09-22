/* maauthbussavedetail.i  MEDSTAR Medical Aid System
                         Save Authorisation Detail Record
                         (c) Copyright 1990 - 2020
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/                      

  DEFINE PARAMETER BUFFER btt_auth_detail FOR tt_auth_detail.

  DEFINE VARIABLE oTFSearch        AS ma.cls.matariffsearch NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj   AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE lSuccess         AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE dTariffObj       AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE dTrfCostObj      AS DECIMAL               NO-UNDO.
  DEFINE VARIABLE cArsRate         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cBaseRate        AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cError           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWarning         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cAddValidations  AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cAlertMessage    AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE iPrType          AS INTEGER               NO-UNDO.
  DEFINE VARIABLE iSubPrType       AS INTEGER               NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN  
  
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER buf_auth_detail   FOR hat_auth_detail. 
  DEFINE BUFFER trfcost           FOR trfcost.

  IF AVAILABLE btt_auth_detail AND 
    CAN-DO("{&ActionList}":U, btt_auth_detail.record_action) AND NOT goErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U, "ERR":U)
  THEN
  DO:                                  

    /*
      Retain old auth obj in case this is a new record and we have a dummy obj which will need to be replaced
      on all the child records if this is a batch update. 
    */
    ASSIGN dAuthDetailObj = btt_auth_detail.auth_detail_obj. 
    
    /*
      If the authorisation has been ended, ensure we end the detail line as well if the end date is blank
    */
    IF goAuthorisation:EndDate <> ? AND btt_auth_detail.end_date = ? 
    THEN 
      ASSIGN btt_auth_detail.end_date = goAuthorisation:EndDate.
      
    /*
      MMP-289
      Inherit PMB indicator from provider when the detail record is being created.
      Populate the pmb indicator on the temp table record before the record is saved.
    */
    FIND FIRST btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
      NO-ERROR.
      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
    
    IF AVAILABLE btt_auth_provider THEN
    DO:
      /* 
        If the user updates the override base/ars rates on the detail line, 
        we need to clear the owning_obj which contains the tariff_link_obj. 
        getValidTariff will repopulate the owning_obj with a valid tariff_link_obj, 
        if a valid tariff link is found using the default/override base/ars rate 
      */ 
      FIND FIRST buf_auth_detail NO-LOCK
           WHERE buf_auth_detail.auth_detail_obj = dAuthDetailObj NO-ERROR. 
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
           
      IF AVAILABLE buf_auth_detail AND 
        (buf_auth_detail.override_base_rate <> btt_auth_detail.override_base_rate OR 
         buf_auth_detail.override_ars_rate  <> btt_auth_detail.override_ars_rate)
      THEN ASSIGN btt_auth_detail.owning_obj = 0.
                  
      IF btt_auth_detail.override_base_rate <> "":U 
      THEN 
        ASSIGN cBaseRate                  = btt_auth_detail.override_base_rate
               cArsRate                   = btt_auth_detail.override_ars_rate
               .
      ELSE 
        ASSIGN cBaseRate = IF btt_auth_provider.override_base_rate <> "":U
                           THEN btt_auth_provider.override_base_rate
                           ELSE btt_auth_provider.default_base_rate
               cArsRate  = IF btt_auth_provider.override_ars_rate <> "":U
                           THEN btt_auth_provider.override_ars_rate
                           ELSE btt_auth_provider.default_ars_rate.
                           
      IF cBaseRate = "" THEN
      DO:
        mipEnv:Health:maDoctor:getProviderBaseRates( INPUT  btt_auth_provider.doc_num,    
                                                     INPUT  goAuthorisation:MemNum,    
                                                     INPUT  goAuthorisation:MemberOptionCode,
                                                     INPUT  btt_auth_provider.start_date,    
                                                     OUTPUT cBaseRate, 
                                                     OUTPUT cArsRate).
      END.  /* IF cBaseRate = "" THEN */

      IF btt_auth_detail.quantity_los > 0 
      THEN
        ASSIGN btt_auth_detail.default_base_rate = cBaseRate
               btt_auth_detail.default_ars_rate  = cArsRate.
      ELSE
        ASSIGN btt_auth_detail.default_base_rate = "":U
               btt_auth_detail.default_ars_rate  = "":U.

      IF btt_auth_detail.auth_detail_obj <= 0.00 THEN
      DO:
        ASSIGN btt_auth_detail.pmb_indicator = btt_auth_provider.pmb_indicator.
        
        VALIDATE btt_auth_detail.
      END. /* IF btt_auth_detail.auth_detail_obj <= 0.00 THEN */

      ASSIGN iPrType    = btt_auth_provider.pr_type
             iSubPrType = btt_auth_provider.sub_pr_type.
    END. /* IF AVAILABLE btt_auth_provider THEN */

    IF btt_auth_detail.owning_entity_mnemonic = "htmtl":U AND btt_auth_detail.record_action <> "DELETE":U THEN 
    DO:
      /*
        Ensure that only valid tariff codes are captured
      */
      ASSIGN cAddValidations = "age,":U     + STRING(goAuthorisation:DependantAgeYears) +
                               "|gender,":U + goAuthorisation:DependantGender           +
                               "|bmi,0":U
             lSuccess = mipEnv:Health:maMedical:getValidTariff(INPUT-OUTPUT btt_auth_detail.owning_obj,        /*  iopdTariffLinkObj */
                                                               INPUT        btt_auth_detail.owning_alt_value,  /*  ipcTariffCode     */
                                                               INPUT        cBaseRate,                         /*  ipcBaseRate       */
                                                               INPUT        cArsRate,                          /*  ipcARSRate        */
                                                               INPUT        iPrType,                           /*  ipiPrType         */
                                                               INPUT        iSubPrType,                        /*  ipiSubPrType      */
                                                               INPUT        btt_auth_detail.start_date,        /*  ipdDate           */
                                                               INPUT        goAuthorisation:OptionCode,        /*  ipiOptionCode     */
                                                               INPUT        cAddValidations,                   /*  ipcAddValidations */
                                                               OUTPUT       dTariffObj,                        /*  opdTariffObj      */
                                                               OUTPUT       dTrfCostObj,                       /*  opdTrfCostObj     */
                                                               OUTPUT       cError,                            /*  opcError          */
                                                               OUTPUT       cWarning,                          /*  opcWarning        */
                                                               OUTPUT       cAlertMessage).                    /*  opcAlertMessage   */ 

      IF cError <> "" THEN
      DO:
        goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
                           INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                           INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                           INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                           INPUT cError).                                            /* ipcMessageText          */ 
        LEAVE.
      END.  /* IF cError <> "" THEN */

      IF cWarning <> "" THEN
      DO:
        goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
                           INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                           INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                           INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                           INPUT cWarning,                                           /* ipcMessageText          */ 
                           INPUT "WAR":U).                                           /* ipcMessageType          */ 
      END.  /* IF cWarning <> "" THEN */

      IF cAlertMessage <> "" THEN
      DO:
        goErrorObject:addError
                          (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic */ 
                           INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj      */ 
                           INPUT "":U,                                               /* ipcOwningEntityKey      */ 
                           INPUT btt_auth_detail.line_number,                        /* ipiLineNumber           */ 
                           INPUT cAlertMessage,                                      /* ipcMessageText          */ 
                           INPUT "WAR":U).                                           /* ipcMessageType          */
      END.  /* IF cAlertMessage <> "" THEN */

      IF dTrfCostObj <> 0 THEN
      DO:
        FIND trfcost NO-LOCK
             WHERE trfcost.trfcost-obj = dTrfCostObj
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

      END.  /* IF dTrfCostObj <> 0 THEN */

      /* Only overwrite Auth Detail amount & cost with trfcost amount if the detail line was added by the user */
      IF AVAILABLE trfcost 
      AND btt_auth_detail.added_by_user THEN
        ASSIGN btt_auth_detail.amount_auth  = btt_auth_detail.quantity_auth * trfcost.amount
               btt_auth_detail._detail_cost = trfcost.amount.
      ELSE 
        ASSIGN btt_auth_detail._detail_cost = ROUND(btt_auth_detail.amount_auth / btt_auth_detail.quantity_auth,2).
        
    END. /*IF btt_auth_detail.oqning_entity_mnemonic = "htmtl":U THEN*/

    /*
      Save or remove the record in the data access layer
    */
    mipEnv:Health:AuthDataAccess:saveAuthDetail(BUFFER btt_auth_detail, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).
        
    /*
      No point in continuing if an error occurred when the record was passed to the data access layer to be saved or deleted
    */
    IF goErrorObject:CanFind("hatad:":U + btt_auth_detail.owning_entity_mnemonic, btt_auth_detail.auth_detail_obj, "":U, "ERR":U)
    THEN LEAVE.
    
    IF btt_auth_detail.record_action = "Modify":U THEN 
    DO:
      /*
        If a new record was created, we need to populate the new auth detail obj on the child records
      */
      IF dAuthDetailObj <= 0.00 THEN
      DO:
        /*
          Populate repeat records auth obj
        */
        FOR EACH tt_auth_repeat EXCLUSIVE-LOCK 
           WHERE tt_auth_repeat.auth_detail_obj = dAuthDetailObj:
           
          ASSIGN tt_auth_repeat.auth_detail_obj = btt_auth_detail.auth_detail_obj.
          VALIDATE tt_auth_repeat. 
        END. /*FOR EACH tt_auth_repeat EXCLUSIVE-LOCK */            
      END. /*IF dAuthDetailObj <= 0.00 THEN*/
    END. /*IF btt_auth_detail.record_action = "Modify":U THEN */
      
    /*
      Clear record action
    */
    ASSIGN btt_auth_detail.record_action = "":U.
    
    
    VALIDATE btt_auth_detail.
  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_detail.record_action) THEN DO:*/
  
&ENDIF  
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oTFSearch) THEN DELETE OBJECT oTFSearch.

                DATASET dsTariff:EMPTY-DATASET()."}



