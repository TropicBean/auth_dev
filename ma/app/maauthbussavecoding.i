/* maauthbussavecoding.i  MEDSTAR Medical Aid System
                          Save Authorisation Coding Record
                          (c) Copyright 1990 - 2021
                          MIP Holdings (Pty) Ltd
                          All rights reserved
*/                 
  DEFINE PARAMETER BUFFER btt_auth_coding FOR tt_auth_coding.
                                       
  DEFINE VARIABLE oICDPrimaryCode        AS cls.maauthrule  NO-UNDO.
  DEFINE VARIABLE oCPTPrimaryCode        AS cls.maauthrule  NO-UNDO.
  DEFINE VARIABLE oCPTPMBIndicator       AS cls.maauthrule  NO-UNDO.
  DEFINE VARIABLE oIcdCptTypePairing     AS cls.maauthrule  NO-UNDO.
  DEFINE VARIABLE iOptionCode            AS INTEGER         NO-UNDO.
  DEFINE VARIABLE iCount                 AS INTEGER         NO-UNDO.
  DEFINE VARIABLE lSuccess               AS LOGICAL         NO-UNDO. 
  DEFINE VARIABLE lActivateMainCode      AS LOGICAL         NO-UNDO.
  DEFINE VARIABLE dAmountPaid            AS DECIMAL         NO-UNDO.
  DEFINE VARIABLE lCPTMandatoryValidRule AS LOGICAL         NO-UNDO. 
  DEFINE VARIABLE cCPTMandatoryRuleValue AS CHARACTER       NO-UNDO. 
  DEFINE VARIABLE cErrorMessage          AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE lIcdPmbIndicator       AS LOGICAL         NO-UNDO.
  DEFINE VARIABLE IcdCptTypePair         AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE iQuantityPaid          AS INTEGER         NO-UNDO.
  DEFINE VARIABLE lUpdatePMB             AS LOGICAL         NO-UNDO. 
  DEFINE VARIABLE cDetailEntity          AS CHARACTER       NO-UNDO. 
 
&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE BUFFER btt_auth          FOR tt_auth.
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail   FOR tt_auth_detail.
  DEFINE BUFFER ctt_auth_coding   FOR tt_auth_coding.
  DEFINE BUFFER buf_auth          FOR hat_auth.
  DEFINE BUFFER buf_auth_provider FOR hat_auth_provider.
  DEFINE BUFFER buf_auth_coding   FOR hat_auth_coding.
  DEFINE BUFFER bbuf_auth_coding  FOR hat_auth_coding.
  DEFINE BUFFER buf_auth_detail   FOR hat_auth_detail.

  IF AVAILABLE btt_auth_coding AND 
    CAN-DO("{&ActionList}":U, btt_auth_coding.record_action) AND NOT goErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj, "":U, "ERR":U)
  THEN
  DO:
    FIND FIRST buf_auth_coding NO-LOCK
         WHERE buf_auth_coding.auth_coding_obj = btt_auth_coding.auth_coding_obj
      NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
    IF goAuthorisation:InFocus THEN 
    DO:
      
      ASSIGN
         oICDPrimaryCode    = NEW cls.maauthrule()                
         oCPTPrimaryCode    = NEW cls.maauthrule()
         oCPTPMBIndicator   = NEW cls.maauthrule()
         oIcdCptTypePairing = NEW cls.maauthrule()       
                      
         lSuccess        = oICDPrimaryCode:focusAuthRule(INPUT goAuthorisation:InsurerObj, 
                                                         INPUT goAuthorisation:MemberOptionCode, 
                                                         INPUT "ma_acAuthRuleTypeAUTHCODING":U, 
                                                         INPUT "ICDPrimaryCode":U, 
                                                         INPUT btt_auth_coding.start_date)
                                                         
         lSuccess        = oCPTPrimaryCode:focusAuthRule(INPUT goAuthorisation:InsurerObj, 
                                                         INPUT goAuthorisation:MemberOptionCode, 
                                                         INPUT "ma_acAuthRuleTypeAUTHCODING":U, 
                                                         INPUT "CPTPrimaryCode":U, 
                                                         INPUT btt_auth_coding.start_date)

         lSuccess        = oCPTPMBIndicator:focusAuthRule(INPUT goAuthorisation:InsurerObj, 
                                                          INPUT goAuthorisation:MemberOptionCode, 
                                                          INPUT "ma_acAuthRuleTypeAUTHCODING":U, 
                                                          INPUT "CptPmbIndicator":U, 
                                                          INPUT btt_auth_coding.start_date)

         lSuccess        = oIcdCptTypePairing:focusAuthRule(INPUT goAuthorisation:InsurerObj, 
                                                            INPUT goAuthorisation:MemberOptionCode, 
                                                            INPUT "ma_acAuthRuleTypeAUTHCODING":U, 
                                                            INPUT "IcdCptTypePairing":U, 
                                                            INPUT btt_auth_coding.start_date).

      /* MMP-420 (MM) */
      /* Populating the ICD/CPT coding type pairs from the IcdCptTypePairing Rule value. */
      IF oIcdCptTypePairing:AuthRuleInFocus THEN
      DO:
        EMPTY TEMP-TABLE tt_icd_cpt_pairs.

        DO iCount = 1 TO NUM-ENTRIES(oIcdCptTypePairing:RuleValue, "|"):
          ASSIGN IcdCptTypePair = ENTRY(iCount, oIcdCptTypePairing:RuleValue, "|").
          
          CREATE tt_icd_cpt_pairs.
          IF IcdCptTypePair <> "" THEN 
          DO:
            ASSIGN 
              tt_icd_cpt_pairs.icd_coding_type = TRIM(ENTRY(1, IcdCptTypePair))
              tt_icd_cpt_pairs.cpt_coding_type = TRIM(ENTRY(2, IcdCptTypePair)). 
          END. /* IF IcdCptTypePair <> "" THEN  */
        END. /* DO iCount = 1 TO NUM-ENTRIES(oIcdCptTypePairing:RuleValue, "|"): */
      END. /* IF oIcdCptTypePairing:AuthRuleInFocus THEN */
      
      /*
        Set Primary Code
      */
      CASE btt_auth_coding.owning_entity_mnemonic:
        WHEN "diagnos":U THEN 
          ASSIGN btt_auth_coding.primary_code = IF oICDPrimaryCode:AuthRuleInFocus 
                                                THEN TRIM(oICDPrimaryCode:RuleValue) = btt_auth_coding.coding_type
                                                ELSE NO.
        WHEN "hlmck":U THEN 
          ASSIGN btt_auth_coding.primary_code = IF oCPTPrimaryCode:AuthRuleInFocus 
                                                THEN TRIM(oCPTPrimaryCode:RuleValue) = btt_auth_coding.coding_type
                                                ELSE NO.
      END CASE.
  
      /* 
        Check if ICD code in UI is a dagger code, remove '+' indicator before Save 
      */
      ASSIGN btt_auth_coding.owning_alt_value = TRIM(btt_auth_coding.owning_alt_value,"+").  
      
      /*
        If the authorisation has been ended, ensure we end the coding line as well if the end date is blank
      */
      IF goAuthorisation:EndDate <> ? AND btt_auth_coding.end_date = ? 
      THEN 
        ASSIGN btt_auth_coding.end_date = goAuthorisation:EndDate.
      
      VALIDATE btt_auth_coding.
      
      /*
        MMP-289 - If we are dealing with the primary ICD code then we should populate 
                  the PMB indicator on auth header auth provider and detail lines.
      */
      IF btt_auth_coding.owning_entity_mnemonic = "diagnos":U AND (btt_auth_coding.primary_code OR btt_auth_coding.main_code) 
      AND (NOT AVAILABLE buf_auth_coding OR (AVAILABLE buf_auth_coding AND buf_auth_coding.pmb_indicator <> btt_auth_coding.pmb_indicator)) THEN
      DO:

        /*
          Check/uncheck PMB indicator on header - ensuring we update any temp table 
          data that may be in the dataset as well as the database record itself.
        */
        FIND FIRST btt_auth EXCLUSIVE-LOCK
             WHERE btt_auth.auth_obj = btt_auth_coding.auth_obj
          NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        IF AVAILABLE btt_auth THEN
        DO:
          IF btt_auth.pmb_indicator <> btt_auth_coding.pmb_indicator THEN
          DO:
            IF  btt_auth.authorise_all_services = NO THEN 
            DO:
              ASSIGN iQuantityPaid = 0
                     dAmountPaid   = 0.00.
  
              /*Calculate the  amount paid and quantities paid*/
              FOR EACH btt_auth_provider NO-LOCK
                 WHERE btt_auth_provider.auth_obj = btt_auth_coding.auth_obj:
  
                 ASSIGN iQuantityPaid = iQuantityPaid + btt_auth_provider.quantity_paid
                        dAmountPaid   = dAmountPaid   + btt_auth_provider.amount_paid .
              END. /*FOR EACH btt_auth_provider NO-LOCK*/
            
              ASSIGN lUpdatePMB = FALSE.
  
              IF btt_auth.amount_paid  > dAmountPaid 
              OR btt_auth.quantity_paid > iQuantityPaid THEN 
              DO:
                IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) > 0 THEN 
                DO: 
                  ASSIGN 
                    lUpdatePMB    = TRUE                  
                    cErrorMessage = "Associated claims are already paid for the Authorisation,"
                                  + "claims should be reversed before the PMB Indicator can be updated on Authorisation Header."
                                  + "You are overriding this functionality." 
                    lSuccess      = goErrorObject:addError
                                                (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                                 INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                                 INPUT "":U,                                                /* ipcOwningEntityKey       */
                                                 INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                                 INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                                 INPUT cErrorMessage,                                       /* ipcMessageText           */
                                                 INPUT "WARN":U,
                                                 INPUT TRUE).    
                END. /* IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) > 0 */
                ELSE DO:
                  ASSIGN 
                    lUpdatePMB    = FALSE
                    cErrorMessage = "Associated claims are already paid for the Authorisation,"
                                  + "claims must be reversed before PMB Indicator can be updated on Authorisation Header." 
                    lSuccess      = goErrorObject:addError
                                                  (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                                   INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */ 
                                                   INPUT "":U,                                                /* ipcOwningEntityKey       */
                                                   INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                                   INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                                   INPUT cErrorMessage,                                       /* ipcMessageText           */
                                                   INPUT "WARN":U,
                                                   INPUT TRUE).    
                END. /* ELSE DO */         
              END.  /* IF (btt_auth.amount_paid  > dAmountPaid OR btt_auth.quantity_paid > iQuantityPaid) */
              ELSE DO:
                IF btt_auth.amount_paid  = 0 
                OR btt_auth.quantity_paid = 0 
                THEN 
                  ASSIGN btt_auth.pmb_indicator = btt_auth_coding.pmb_indicator
                         btt_auth.record_action = "Modify":U.
  
                  FIND FIRST buf_auth EXCLUSIVE-LOCK
                       WHERE buf_auth.auth_obj = btt_auth_coding.auth_obj
                  NO-ERROR.
  
                 { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
                  IF AVAILABLE buf_auth 
                  THEN 
                    ASSIGN buf_auth.pmb_indicator = btt_auth_coding.pmb_indicator.
              END.
              IF lUpdatePMB THEN 
              DO: 
                ASSIGN btt_auth.pmb_indicator = btt_auth_coding.pmb_indicator
                       btt_auth.record_action = "Modify":U.
  
                FIND FIRST buf_auth EXCLUSIVE-LOCK
                     WHERE buf_auth.auth_obj = btt_auth_coding.auth_obj
                  NO-ERROR.
  
               { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
                IF AVAILABLE buf_auth 
                THEN 
                  ASSIGN buf_auth.pmb_indicator = btt_auth_coding.pmb_indicator.
              END. /*IF lUpdatePMB */  
            END.  /*IF  btt_auth.authorise_all_services = NO*/
            ELSE
              
              ASSIGN 
                cErrorMessage = "PMB Indicator won't be updated on the Authorisation Header because PMB Indicator on providers will apply." 
                lSuccess      = goErrorObject:addError
                                              (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                               INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                               INPUT "":U,                                                /* ipcOwningEntityKey       */
                                               INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                               INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                               INPUT cErrorMessage,                                       /* ipcMessageText           */
                                               INPUT "WARN":U,
                                               INPUT TRUE). 
              	 
          END.  /* IF btt_auth.pmb_indicator <> btt_auth_coding.pmb_indicator THEN */
        END. /*IF AVAILABLE btt_auth*/
        /*
          Check/uncheck PMB indicator on provider records - ensuring we update any 
          temp table data that may be in the dataset as well as the database records.
        */
        FOR EACH btt_auth_provider EXCLUSIVE-LOCK
           WHERE btt_auth_provider.auth_obj       = btt_auth_coding.auth_obj
           AND   btt_auth_provider.pmb_indicator <> btt_auth_coding.pmb_indicator:
      
        ASSIGN lUpdatePMB = FALSE.
 
        IF (btt_auth_provider.amount_paid  <> 0 OR  btt_auth_provider.quantity_paid <> 0)
        AND btt_auth_provider.authorised_service 
        THEN DO:
          IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) > 0 THEN 
          DO: 
            ASSIGN 
              lUpdatePMB    = TRUE
              cErrorMessage = SUBSTITUTE("PMB indicator is about  to  change please be aware that claims were paid  for provider &1",
                              STRING(btt_auth_provider.doc_num))
              lSuccess      = goErrorObject:addError
                                            (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                             INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                             INPUT "":U,                                                /* ipcOwningEntityKey       */
                                             INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                             INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                             INPUT cErrorMessage,                                       /* ipcMessageText           */
                                             INPUT "WARN":U,
                                             INPUT TRUE).
  
          END. /* IF LOOKUP("ma_member_auth_pmbind_override ...*/ 
          ELSE DO: 
            ASSIGN 
              lUpdatePMB    = FALSE
              cErrorMessage = SUBSTITUTE("PMB Indicator won't be updated on Provider &1. Claims must be reversed before PMB Indicator can be updated on the Provider.",
                              STRING(btt_auth_provider.doc_num))
              lSuccess      = goErrorObject:addError
                                            (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                             INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                             INPUT "":U,                                                /* ipcOwningEntityKey       */
                                             INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                             INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                             INPUT cErrorMessage,                                       /* ipcMessageText           */
                                             INPUT "WARN":U,
                                             INPUT TRUE).
          END. /* ELSE DO: IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) */
          IF lUpdatePMB THEN 
          DO:
            ASSIGN btt_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator
                   btt_auth_provider.record_action = "Modify":U.
   
            FIND buf_auth_provider EXCLUSIVE-LOCK
            WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj NO-ERROR.
     
	          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
	     
            IF AVAILABLE buf_auth_provider
            THEN
              ASSIGN buf_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator. 
              
          END. /*IF lUpdatePMB */ 
        END. /* IF (btt_auth_provider.amount_paid  <> 0 OR ... */		
        ELSE DO:
          IF btt_auth_provider.amount_paid  = 0 OR  btt_auth_provider.quantity_paid = 0
          THEN 
            ASSIGN btt_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator
                   btt_auth_provider.record_action = "Modify":U.
				   
	        FIND buf_auth_provider EXCLUSIVE-LOCK
	          WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj
	          NO-ERROR.
	     
	          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
	     
	          IF AVAILABLE buf_auth_provider
	          THEN
              ASSIGN buf_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator.  		  
		END. /* ELSE DO: IF btt_auth_provider.amount_paid = 0 ... */
		IF   btt_auth_provider.auth_copay_control_obj <> 0
             AND (btt_auth_provider.copay_auth <> 0 OR btt_auth_provider.copay_auth_% <> 0)
        THEN DO:
          FIND FIRST hac_auth_copay_control NO-LOCK
               WHERE hac_auth_copay_control.auth_copay_control_obj = btt_auth_provider.auth_copay_control_obj
               NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE hac_auth_copay_control 
            AND hac_auth_copay_control.apply_to_pmb = NO 
            AND CAN-FIND(FIRST hat_auth_copay WHERE hat_auth_copay.auth_obj               = btt_auth_provider.auth_obj
                           AND hat_auth_copay.auth_copay_type_obj    = hac_auth_copay_control.auth_copay_type_obj
                           AND hat_auth_copay.owning_entity_mnemonic = "hatap":U
                           AND hat_auth_copay.owning_obj             = btt_auth_provider.auth_provider_obj
                           AND hat_auth_copay.owning_key             = "")
            THEN 
              ASSIGN 
                cErrorMessage = "PMB Indicator won't be updated on Provider " + STRING(btt_auth_provider.doc_num) + ", user must update PMB Indicator on provider to re-apply co-payment rules." 
                lSuccess      = goErrorObject:addError
                                            (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                             INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                             INPUT "":U,                                                /* ipcOwningEntityKey       */
                                             INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                             INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                             INPUT cErrorMessage,                                       /* ipcMessageText           */
                                             INPUT "WARN":U,
                                             INPUT TRUE).
            ELSE DO:
              ASSIGN btt_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator
	             btt_auth_provider.record_action = "Modify":U.
	                        
	          FIND buf_auth_provider EXCLUSIVE-LOCK
	          WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj
	          NO-ERROR.
	     
	          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
	     
	          IF AVAILABLE buf_auth_provider
	          THEN
              ASSIGN buf_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator. 
                  
          END.  /* ELSE DO: - IF AVAILABLE hac_auth_copay_control AND hac_auth_copay_control.apply_to_pmb = NO */
        END.  /* ELSE IF  btt_auth_provider.auth_copay_control_obj <> 0 */
        ELSE DO:
            ASSIGN btt_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator
                   btt_auth_provider.record_action = "Modify":U.
                   
            FIND buf_auth_provider EXCLUSIVE-LOCK
              WHERE buf_auth_provider.auth_provider_obj = btt_auth_provider.auth_provider_obj
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE buf_auth_provider
            THEN
              ASSIGN buf_auth_provider.pmb_indicator = btt_auth_coding.pmb_indicator. 
          END. /* ELSE - ELSE IF  btt_auth_provider.auth_copay_control_obj <> 0 */
        END. /*FOR EACH btt_auth_provider EXCLUSIVE-LOCK*/

        /*
          Check/uncheck PMB indicator on detail records - ensuring we update any 
          temp table data that may be in the dataset as well as the database records.
        */
        FOR EACH btt_auth_detail EXCLUSIVE-LOCK
           WHERE btt_auth_detail.auth_obj = btt_auth_coding.auth_obj:
		   
          ASSIGN lUpdatePMB = FALSE.
		     
          IF btt_auth_detail.amount_paid   <> 0
          OR btt_auth_detail.quantity_paid <> 0
          THEN DO:
            IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) > 0 
            THEN DO: 
              IF btt_auth_detail.owning_entity_mnemonic = "htmtl" THEN ASSIGN cDetailEntity = "Tariff".
              IF btt_auth_detail.owning_entity_mnemonic = "hlmnl" THEN ASSIGN cDetailEntity = "Nappi".
              IF btt_auth_detail.owning_entity_mnemonic = "hlmcr" THEN ASSIGN cDetailEntity = "Basket".

              ASSIGN 
                lUpdatePMB    = TRUE
                cErrorMessage = SUBSTITUTE("PMB indicator is about to change  please be aware that  claims were  paid to &1 &2",
                                           cDetailEntity, btt_auth_detail.owning_alt_value)
                lSuccess      = goErrorObject:addError
                                              (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                               INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                               INPUT "":U,                                                /* ipcOwningEntityKey       */
                                               INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                               INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                               INPUT cErrorMessage,                                       /* ipcMessageText           */
                                               INPUT "WARN":U,
                                               INPUT TRUE).
            END. /* IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) > 0 */
            ELSE DO: 
              ASSIGN 
			    lUpdatePMB    = FALSE	
                cErrorMessage = SUBSTITUTE("PMB Indicator won't be updated on Clinical Detail Line for &1, claims must be reversed before PMB Indicator can be updated.",
                                           btt_auth_detail.owning_alt_value)
                lSuccess      = goErrorObject:addError
                                            (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                             INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                             INPUT "":U,                                                /* ipcOwningEntityKey       */
                                             INPUT "pmb_indicator":U,                                   /* ipcFieldName             */
                                             INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                             INPUT cErrorMessage,                                       /* ipcMessageText           */
                                             INPUT "WARN":U,
                                             INPUT TRUE).
          END. /* ELSE DO: IF LOOKUP("ma_member_auth_pmbind_override":U,gscUserRole) > 0 */ 	
        END. /* IF btt_auth_detail.amount_paid   <> 0 */
        ELSE DO:
          IF btt_auth_detail.amount_paid   = 0
          OR btt_auth_detail.quantity_paid = 0
          THEN 
            ASSIGN btt_auth_detail.pmb_indicator = btt_auth_coding.pmb_indicator
                   btt_auth_detail.record_action = "Modify":U.
			   
	  FIND FIRST buf_auth_detail EXCLUSIVE-LOCK
               WHERE buf_auth_detail.auth_obj = btt_auth_coding.auth_obj
               NO-ERROR.
  
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
            IF AVAILABLE buf_auth_detail 
            THEN 
            ASSIGN buf_auth_detail.pmb_indicator = btt_auth_coding.pmb_indicator.
		END. /* IF btt_auth_detail.amount_paid = 0... */
	IF lUpdatePMB THEN 
        DO: 
          ASSIGN btt_auth_detail.pmb_indicator = btt_auth_coding.pmb_indicator
                 btt_auth_detail.record_action = "Modify":U.
			   
	  FIND FIRST buf_auth_detail EXCLUSIVE-LOCK
               WHERE buf_auth_detail.auth_obj = btt_auth_coding.auth_obj
               NO-ERROR.
  
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
            IF AVAILABLE buf_auth_detail 
            THEN 
            ASSIGN buf_auth_detail.pmb_indicator = btt_auth_coding.pmb_indicator.
          END. /* IF lUpdatePMB THEN */
        END. /* FOR EACH btt_auth_provider EXCLUSIVE-LOCK */
        
        /* 
          If ICD PMB Indicator, then update the corresponding CPT PMB Indicator. 
        */
        IF oCPTPMBIndicator:AuthRuleInFocus AND LOOKUP(oCPTPMBIndicator:RuleValue, "Y,YES,TRUE":U) > 0 
        AND oIcdCptTypePairing:AuthRuleInFocus THEN
        DO:
          /* When Creating or Updating a ICD Coding Record */
          /* We need to update both the DB and Temp-table incase the DB record has not been updated yet */
          FIND FIRST tt_icd_cpt_pairs NO-LOCK
               WHERE tt_icd_cpt_pairs.icd_coding_type = btt_auth_coding.coding_type 
           NO-ERROR.
           { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE tt_icd_cpt_pairs THEN 
          DO:
            FOR EACH ctt_auth_coding EXCLUSIVE-LOCK
               WHERE ctt_auth_coding.auth_obj               = btt_auth_coding.auth_obj
                 AND ctt_auth_coding.owning_entity_mnemonic = "hlmck":U
                 AND ctt_auth_coding.coding_type            = tt_icd_cpt_pairs.cpt_coding_type :
              
              ASSIGN ctt_auth_coding.pmb_indicator = btt_auth_coding.pmb_indicator
                     ctt_auth_coding.record_action = "Modify":U.                   
            
            END. /* FOR EACH ctt_auth_coding EXCLUSIVE-LOCK */
            
            FOR EACH bbuf_auth_coding EXCLUSIVE-LOCK
               WHERE bbuf_auth_coding.auth_obj               = btt_auth_coding.auth_obj
                 AND bbuf_auth_coding.owning_entity_mnemonic = "hlmck":U
                 AND bbuf_auth_coding.coding_type            = tt_icd_cpt_pairs.cpt_coding_type:
              
              ASSIGN bbuf_auth_coding.pmb_indicator = btt_auth_coding.pmb_indicator NO-ERROR.
              
            END. /* FOR EACH bbuf_auth_coding EXCLUSIVE-LOCK */
          END. /* IF AVAILABLE tt_icd_cpt_coding_type THEN  */
        END. /* IF oCPTPMBIndicator:AuthRuleInFocus AND LOOKUP(oCPTPMBIndicator:RuleValue, "Y,YES,TRUE":U) > 0  */
      END. /*IF btt_auth_coding.owning_entity_mnemonic = "diagnos":U AND btt_auth_coding.primary_code THEN*/

      /* When adding a CPT Record */
      /* We need to update both the DB and Temp-table incase the DB record has not been updated yet */
      IF btt_auth_coding.owning_entity_mnemonic = "hlmck":U 
      AND oCPTPMBIndicator:AuthRuleInFocus AND LOOKUP(oCPTPMBIndicator:RuleValue, "Y,YES,TRUE":U) > 0 
      AND oIcdCptTypePairing:AuthRuleInFocus
      THEN DO:
        FIND FIRST tt_icd_cpt_pairs NO-LOCK
             WHERE tt_icd_cpt_pairs.cpt_coding_type = btt_auth_coding.coding_type 
         NO-ERROR.
         { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE tt_icd_cpt_pairs THEN
        DO:
          FOR EACH ctt_auth_coding EXCLUSIVE-LOCK
             WHERE ctt_auth_coding.auth_obj               = btt_auth_coding.auth_obj
               AND ctt_auth_coding.owning_entity_mnemonic = "diagnos":U
               AND ctt_auth_coding.coding_type            = tt_icd_cpt_pairs.icd_coding_type:
            
            ASSIGN btt_auth_coding.pmb_indicator = ctt_auth_coding.pmb_indicator.
            
          END. /* FOR EACH ctt_auth_coding EXCLUSIVE-LOCK */

          FOR EACH bbuf_auth_coding EXCLUSIVE-LOCK
             WHERE bbuf_auth_coding.auth_obj               = btt_auth_coding.auth_obj
               AND bbuf_auth_coding.owning_entity_mnemonic = "diagnos":U
               AND bbuf_auth_coding.coding_type            = tt_icd_cpt_pairs.icd_coding_type:

            ASSIGN btt_auth_coding.pmb_indicator = bbuf_auth_coding.pmb_indicator.
            
          END. /* FOR EACH bbuf_auth_coding EXCLUSIVE-LOCK */
        
        END. /* IF AVAILABLE tt_icd_cpt_pairs THEN */
      END. /* IF btt_auth_coding.owning_entity_mnemonic = "hlmck":U */
      
      /* 
         When the primary/main procedure(CPT) is deleted, we need to reset the service type on the auth 
      */
      IF btt_auth_coding.owning_entity_mnemonic = "hlmck":U AND btt_auth_coding.record_action = "DELETE":U THEN
      DO:

        mipEnv:Health:AuthBusinessLogic:activateMainCode(INPUT goAuthorisation:InsurerObj,
                                                         INPUT goAuthorisation:MemberOptionCode,
                                                         INPUT goAuthorisation:StartDate,
                                                         OUTPUT lActivateMainCode).

        IF ((btt_auth_coding.primary_code AND NOT lActivateMainCode) OR (btt_auth_coding.main_code AND lActivateMainCode)) THEN
        DO:
          mipEnv:Health:AuthService:getAuthTypeConfig(INPUT goAuthorisation:AuthTypeObj,
                                                      INPUT goAuthorisation:InsurerObj,
                                                      INPUT goAuthorisation:MemberOptionCode,
                                                      INPUT btt_auth_coding.start_date,
                                                      INPUT-OUTPUT TABLE ttAuthTypeConfig).

          FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
          
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE ttAuthTypeConfig THEN
          DO:
            IF ttAuthTypeConfig.ActivateServiceType THEN
            DO:

              /*
                Check if CPTMandatory rule is valid. If it is reset the service type to blank ,otherwise default it to Medical. 
              */

              mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                             INPUT  goAuthorisation:MemberOptionCode,
                                                             INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                             INPUT  "CPTMandatory":U,
                                                             INPUT  btt_auth_coding.start_date,
                                                             OUTPUT lCPTMandatoryValidRule,
                                                             OUTPUT cCPTMandatoryRuleValue).
  
                                                 
              FIND FIRST btt_auth EXCLUSIVE-LOCK
                   WHERE btt_auth.auth_obj = btt_auth_coding.auth_obj
                NO-ERROR.
                
              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
                     
              FIND FIRST buf_auth EXCLUSIVE-LOCK
                   WHERE buf_auth.auth_obj = btt_auth_coding.auth_obj
                NO-ERROR.
                
              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
              
              IF AVAILABLE btt_auth 
              THEN
                ASSIGN btt_auth.service_type = IF lCPTMandatoryValidRule AND cCPTMandatoryRuleValue = "BLOCK":U THEN "":U ELSE "ma_acServiceTypeMedical":U.
              
              IF AVAILABLE buf_auth 
              THEN
                ASSIGN buf_auth.service_type = IF lCPTMandatoryValidRule AND cCPTMandatoryRuleValue = "BLOCK":U THEN "":U ELSE "ma_acServiceTypeMedical":U.

            END. /* IF ttAuthTypeConfig.ActivateServiceType */
          END. /* IF AVAILABLE(ttAuthTypeConfig) */
        END. /* IF ((btt_auth_coding.primary_code AND NOT lActivateMainCode) OR (btt_auth_coding.main_code AND lActivateMainCode)) */
      END. /* IF btt_auth_coding.owning_entity_mnemonic = "hlmck":U AND btt_auth_coding.record_action = "DELETE":U  */
                                                                                                
      IF NOT goErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj, "":U, "ERR":U) THEN
      DO:
        /*
          Save or remove the record in the data access layer
        */
        mipEnv:Health:AuthDataAccess:saveAuthCoding(BUFFER btt_auth_coding, INPUT-OUTPUT TABLE tt_auth_result BY-REFERENCE, INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE).

        FOR EACH tt_auth_error
          WHERE tt_auth_error.owning_entity = "hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":":U +  btt_auth_coding.owning_alt_value
            AND tt_auth_error.owning_obj <= 0:
          
          ASSIGN tt_auth_error.owning_obj =  btt_auth_coding.auth_coding_obj.
        END.

      END. /* IF NOT goErrorObject:CanFind("hatac:":U + btt_auth_coding.owning_entity_mnemonic, btt_auth_coding.auth_coding_obj, "":U, "ERR":U) THEN */

      /*
        Clear record action
      */
      EMPTY TEMP-TABLE tt_icd_cpt_pairs.

      ASSIGN btt_auth_coding.record_action = "":U.
      
      VALIDATE btt_auth_coding.
      
    END. /*IF goAuthorisation:InFocus */
  END. /*IF CAN-DO("{&ActionList}":U, btt_auth_coding.record_action) THEN  */

&ENDIF

  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oICDPrimaryCode)    THEN DELETE OBJECT oICDPrimaryCode.
                IF VALID-OBJECT(oCPTPrimaryCode)    THEN DELETE OBJECT oCPTPrimaryCode.
                IF VALID-OBJECT(oCPTPMBIndicator)   THEN DELETE OBJECT oCPTPMBIndicator.  
                IF VALID-OBJECT(oIcdCptTypePairing) THEN DELETE OBJECT oIcdCptTypePairing."}




