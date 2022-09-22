/* maauthbusvalcoding.i  MEDSTAR Medical Aid System
                         Validate Auth Coding Buffer
                         (c) Copyright 2018 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/                      
DEFINE PARAMETER BUFFER btt_auth_coding FOR tt_auth_coding.

DEFINE VARIABLE cAction                    AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cAssociatedCode            AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cDiagnosisType             AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cDependantAge              AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cErrorField                AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cErrorMessage              AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cErrorType                 AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cICDlist                   AS CHARACTER                   NO-UNDO. 
DEFINE VARIABLE cMessage                   AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cMorphologyCode            AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cNote                      AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cOwningEntity              AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cProcedureDateAction       AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cPrimaryCode               AS CHARACTER                   NO-UNDO.     
DEFINE VARIABLE cPrType                    AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cRuleValue                 AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cValidIcds                 AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cValidMessage              AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE cValidationMessage         AS CHARACTER                   NO-UNDO.
DEFINE VARIABLE dAgeRangeObj               AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE dDiagnosObj                AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE dLinkAuthRuleObj           AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE dAuthRuleObj               AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE iDocNum                    AS INTEGER                     NO-UNDO.
DEFINE VARIABLE lAcknowledge               AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lActivateMainCode          AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lActiveCPTFound            AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lAuthTypeEnableBodyRegion  AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lEnableProcedureDate       AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lFiltersApplied            AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lICD10MITVal               AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lMandatory                 AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lPrimaryCode               AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lPrimaryCodeECC            AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lSuccess                   AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lValid                     AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lValidField                AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lValidRule                 AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lValidStatus               AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE lValidBodyRegion           AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE oAuthRule                  AS cls.maauthrule              NO-UNDO.
DEFINE VARIABLE oErrorObject               AS cls.maerrorobject           NO-UNDO.
DEFINE VARIABLE oFVSearch                  AS cls.maauthflagvaluesearch   NO-UNDO.
DEFINE VARIABLE oCPSearch                  AS ma.cls.macptsearch          NO-UNDO.
DEFINE VARIABLE oICDPrimaryCode            AS cls.maauthrule              NO-UNDO.
DEFINE VARIABLE oCPTPrimaryCode            AS cls.maauthrule              NO-UNDO.
DEFINE VARIABLE oBodyRegion                AS cls.mipacronym              NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

DEFINE BUFFER buf_auth            FOR hat_auth.
DEFINE BUFFER btt_auth            FOR tt_auth.
DEFINE BUFFER btt_auth_provider   FOR tt_auth_provider.
DEFINE BUFFER buf_auth_coding     FOR hat_auth_coding.
DEFINE BUFFER tt_auth_coding      FOR tt_auth_coding.
DEFINE BUFFER bbt_auth_coding     FOR btt_auth_coding.
DEFINE BUFFER tt_auth_flag_value  FOR tt_auth_flag_value.
DEFINE BUFFER btt_cpt             FOR tt_cpt.

&SCOPED-DEFINE FindAssociated FIND FIRST diagnos NO-LOCK WHERE diagnos.diagnos-obj = dDiagnosObj NO-ERROR. cls.miperror:resetError(). IF AVAILABLE diagnos THEN ASSIGN cAssociatedCode = diagnos.diagnosis. ELSE ASSIGN cAssociatedCode = ''.

&SCOPED-DEFINE FindMorphology FIND FIRST diagnos NO-LOCK WHERE diagnos.diagnos-obj = dDiagnosObj NO-ERROR. cls.miperror:resetError(). IF AVAILABLE diagnos THEN ASSIGN cMorphologyCode = diagnos.diagnosis. ELSE ASSIGN cMorphologyCode = ''.

IF AVAILABLE btt_auth_coding 
AND CAN-DO("{&ModifyList}":U, btt_auth_coding.record_action) THEN
DO:
  /*
    Validating authorisation type usage.
  */
  IF goAuthorisation:InFocus THEN
  DO:          

    FIND FIRST btt_auth
         WHERE btt_auth.auth_obj = goAuthorisation:AuthObj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 


    mipEnv:Health:AuthService:getAuthTypeConfig(BUFFER btt_auth,
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig).
   
    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
   
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
 
    /* Check if body region is enforced for auth type */
    IF AVAILABLE ttAuthTypeConfig 
    THEN
      ASSIGN lAuthTypeEnableBodyRegion = ttAuthTypeConfig.ActivateBodyRegion.
    ELSE 
      ASSIGN lAuthTypeEnableBodyRegion = FALSE.
   
    IF goAuthorisation:Dependant <> 99 THEN
    DO:
      IF AVAILABLE ttAuthTypeConfig                                         
      AND ttAuthTypeConfig.UsagePeriod            <> 0                         
      AND ((ttAuthTypeConfig.UsageType             = "ma_acAuthUsageTypeIcd":U 
      AND   btt_auth_coding.owning_entity_mnemonic = "diagnos":U)              
      OR   (ttAuthTypeConfig.UsageType             = "ma_acAuthUsageTypeCpt":U 
      AND   btt_auth_coding.owning_entity_mnemonic = "hlmck":U))               
      AND  (btt_auth_coding.primary_code           = YES                       
      OR    btt_auth_coding.main_code              = YES) THEN
      DO:
      
        ASSIGN lSuccess = mipEnv:Health:AuthBusinessLogic:validateAuthTypeUsage(INPUT  goAuthorisation:AuthTypeObj,       
                                                                                INPUT  goAuthorisation:InsurerObj,
                                                                                INPUT  goAuthorisation:MemberOptionCode,        
                                                                                INPUT  goAuthorisation:MemNum,      
                                                                                INPUT  goAuthorisation:Dependant,         
                                                                                INPUT  btt_auth_coding.start_date,          
                                                                                INPUT  "Usage":U,
                                                                                INPUT  btt_auth_coding.owning_alt_value,
                                                                                OUTPUT lValidField,
                                                                                OUTPUT cMessage).
        IF cMessage <> "":U THEN
        DO:
          ASSIGN cErrorType = (IF lValidField = TRUE
                               THEN "WAR":U
                               ELSE "ERR":U).
      
          goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                                  INPUT btt_auth_coding.auth_coding_obj,
                                  INPUT "",
                                  INPUT "owning_entity_mnemonic":U,
                                  INPUT btt_auth_coding.line_number,
                                  INPUT cMessage,
                                  INPUT cErrorType).
        END. /* IF cMessage <> "":U THEN */                            
      END. /* IF AVAILABLE ttAuthTypeConfig AND... */ 
    END. /* IF goAuthorisation:Dependant <> 99 THEN */

    ASSIGN cOwningEntity = IF btt_auth_coding.owning_entity_mnemonic = "diagnos":U
                           THEN "diagnosis (ICD)":U 
                           ELSE "procedure (CPT)":U.
  
    /*
      Check for primary code rule
    */
    ASSIGN
      oICDPrimaryCode = NEW cls.maauthrule()
      oCPTPrimaryCode = NEW cls.maauthrule()

      lSuccess        = oICDPrimaryCode:focusAuthRule(INPUT goAuthorisation:InsurerObj, 
                                                      INPUT goAuthorisation:MemberOptionCode, 
                                                      INPUT "ma_acAuthRuleTypeAUTHCODING":U, 
                                                      INPUT "ICDPrimaryCode":U, 
                                                      INPUT btt_auth_coding.start_date)

      lSuccess        = oCPTPrimaryCode:focusAuthRule(INPUT goAuthorisation:InsurerObj, 
                                                      INPUT goAuthorisation:MemberOptionCode, 
                                                      INPUT "ma_acAuthRuleTypeAUTHCODING":U, 
                                                      INPUT "CPTPrimaryCode":U, 
                                                      INPUT btt_auth_coding.start_date).
    /*
      Check if the current buffer is primary
    */                       
    ASSIGN lPrimaryCode = IF btt_auth_coding.owning_entity_mnemonic = "diagnos":U AND oICDPrimaryCode:AuthRuleInFocus 
                          THEN LOOKUP(btt_auth_coding.coding_type,TRIM(oICDPrimaryCode:RuleValue)) > 0
                          ELSE 
                            (IF btt_auth_coding.owning_entity_mnemonic = "hlmck":U AND oCPTPrimaryCode:AuthRuleInFocus
                             THEN LOOKUP(btt_auth_coding.coding_type,TRIM(oCPTPrimaryCode:RuleValue)) > 0
                             ELSE btt_auth_coding.primary_code).
      
    /*
      Primary code validation - If "MainCode" is not activated, ensure that only one primary code is captured.   
    */
    mipEnv:Health:AuthBusinessLogic:activateMainCode(INPUT goAuthorisation:InsurerObj,
                                                     INPUT goAuthorisation:MemberOptionCode,
                                                     INPUT goAuthorisation:StartDate,
                                                     OUTPUT lActivateMainCode).     
    IF NOT lActivateMainCode 
    AND lPrimaryCode THEN
    DO:
      /*
        Make sure we don't already have a primary code for this owning entity
      */
      IF CAN-FIND(FIRST bbt_auth_coding NO-LOCK
                  WHERE bbt_auth_coding.auth_obj               = btt_auth_coding.auth_obj
                    AND bbt_auth_coding.owning_entity_mnemonic = btt_auth_coding.owning_entity_mnemonic
                    AND bbt_auth_coding.coding_type            = btt_auth_coding.coding_type
                    AND bbt_auth_coding.auth_coding_obj       <> btt_auth_coding.auth_coding_obj
                    AND bbt_auth_coding.coding_status         <> 5  /* Cancelled */
                    AND bbt_auth_coding.coding_status         <> 6  /* Declined  */ )
      THEN
        ASSIGN cMessage = "The Authorisation can only have one primary code.":U +
                          "[HELP=Auth Rule Code: CodingMainCode, " + 
                          IF btt_auth_coding.owning_entity_mnemonic = "diagnos":U 
                          THEN "ICDPrimaryCode]":U
                          ELSE "CPTPrimaryCode]":U
               lSuccess = goErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                                                 INPUT btt_auth_coding.auth_coding_obj,
                                                 INPUT "":U,
                                                 INPUT "coding_type":U,
                                                 INPUT btt_auth_coding.line_number,
                                                 INPUT cMessage,
                                                 INPUT "ERR":U).                               
    END. /*IF NOT lActivateMainCode AND lPrimaryCode THEN*/

    IF NOT CAN-FIND(FIRST btt_auth_provider
                    WHERE btt_auth_provider.auth_obj      = goAuthorisation:AuthObj
                      AND btt_auth_provider.main_provider = TRUE)
    THEN
      goErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                             INPUT btt_auth_coding.auth_coding_obj,
                             INPUT "":U,
                             INPUT btt_auth_coding.line_number,
                             INPUT "Coding details may not be loaded. Main Authorisation Provider not loaded.",
                             INPUT "ERR":U).

  END. /*IF goAuthorisation:InFocus THEN*/
  
  /*
    Auth Coding Status validation - Ensure that only a valid auth status is used according to system and rule setups
  */
  ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  btt_auth_coding.coding_status, INPUT  "System":U).
  
  IF NOT lValidStatus 
  THEN
    ASSIGN cErrorMessage = "Status Code (":U + STRING(btt_auth_coding.coding_status) +
                           "),[HELP=Auth Rule Code: ValidStatuses]":U
           lSuccess = goErrorObject:addError
                            (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */ 
                             INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj      */ 
                             INPUT "":U,                                                /* ipcOwningEntityKey      */ 
                             INPUT "coding_status":U,                                   /* ipcFieldName            */ 
                             INPUT btt_auth_coding.line_number,                         /* ipiLineNumber           */ 
                             INPUT "MA":U,                                              /* ipcMessageGroup         */ 
                             INPUT 112,   /* The '&1' specified is invalid. &2 */       /* ipiMessageNumber        */ 
                             INPUT cErrorMessage).                                      /* ipcReplaceTextList      */ 
  
  /*
    If the auth coding status is modified, check if the status may be upated to the new status
  */ 
  IF btt_auth_coding.auth_coding_obj > 0 THEN
  DO:
    FIND buf_auth_coding NO-LOCK 
      WHERE buf_auth_coding.auth_coding_obj = btt_auth_coding.auth_coding_obj 
      NO-ERROR.
         
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE } 
  
    IF AVAILABLE buf_auth_coding 
    AND buf_auth_coding.coding_status <> btt_auth_coding.coding_status THEN
    DO:
      /*
        Compare the 'old' status against the 'new' status to see if the 
        auth coding status may be changed to the 'new' status.
      */
      IF goAuthorisation:InFocus 
      THEN
        ASSIGN cErrorMessage = mipEnv:Health:AuthService:validateAuthStatusUpdate(INPUT  goAuthorisation:InsurerObj,
                                                                                  INPUT  goAuthorisation:MemberOptionCode,
                                                                                  INPUT  buf_auth_coding.coding_status,
                                                                                  INPUT  buf_auth_coding.coding_status_note,
                                                                                  INPUT  btt_auth_coding.coding_status,
                                                                                  INPUT  goAuthorisation:AmountPaid,
                                                                                  INPUT  goAuthorisation:QuantityPaid,
                                                                                  INPUT  btt_auth_coding.start_date,
                                                                                  INPUT goAuthorisation:AuthStatus,
                                                                                  INPUT goAuthorisation:AuthStatusNote).
      IF cErrorMessage <> "":U 
      THEN
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                        INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */
                                        INPUT "":U,                                                /* ipcOwningEntityKey       */
                                        INPUT "coding_status":U,                                   /* ipcFieldName             */
                                        INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */
                                        INPUT cErrorMessage,                                       /* ipcMessageText           */
                                        INPUT "ERR":U).                                            /* ipcMessageType           */
    END. /* IF AVAILABLE buf_auth_coding AND buf_auth_coding.coding_status <> btt_auth_coding.coding_status THEN */
  END.  /* IF btt_auth_coding.auth_coding_obj > 0 THEN */
  
  /*
    Check if the status reason/note on the auth coding is mandatory
  */ 
  IF btt_auth_coding.coding_status_note = "":U THEN
  DO:
    ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  btt_auth_coding.coding_status,
                                                                        INPUT  goAuthorisation:InsurerObj,     
                                                                        INPUT  goAuthorisation:MemberOptionCode,
                                                                        INPUT  btt_auth_coding.start_date).
    IF lMandatory 
    THEN
      ASSIGN cErrorMessage = "Status reason,A valid status reason must be provided for the status specified.":U
                           + "[HELP=Auth Rule Code: EnforceStatusNote]"
             lSuccess = goErrorObject:addError
                            (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */ 
                             INPUT btt_auth_coding.auth_coding_obj,                     /* ipdOwningEntityObj       */  
                             INPUT "":U,                                                /* ipcOwningEntityKey       */ 
                             INPUT "coding_status_note":U,                              /* ipcFieldName             */ 
                             INPUT btt_auth_coding.line_number,                         /* ipiLineNumber            */  
                             INPUT "MA":U,                                              /* ipcMessageGroup          */ 
                             INPUT 112, /* The '&1' specified is invalid. &2  */        /* ipiMessageNumber         */ 
                             INPUT cErrorMessage).                                      /* ipcReplaceTextList       */ 
                             
  END.  /* IF btt_auth_coding.coding_status_note = "" THEN */  
  ELSE
  DO:
    mipEnv:Health:AuthService:ValidateStatusReason(INPUT  goAuthorisation:InsurerObj,
                                                   INPUT  00,
                                                   INPUT  btt_auth_coding.coding_status_note,
                                                   INPUT  INTEGER(btt_auth_coding.coding_status),
                                                   INPUT  btt_auth_coding.start_date,
                                                   OUTPUT cValidMessage).
    IF cValidMessage <> ""  
    THEN
      ASSIGN lSuccess  = goErrorObject:addError
                                   (INPUT "hatac":U + btt_auth_coding.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */ 
                                    INPUT btt_auth_coding.auth_coding_obj,                    /* ipdOwningEntityObj       */ 
                                    INPUT "":U,                                               /* ipcOwningEntityKey       */ 
                                    INPUT "coding_status_note":U,                             /* ipcFieldName             */ 
                                    INPUT btt_auth_coding.line_number,                        /* ipiLineNumber            */ 
                                    INPUT "MA":U,                                             /* ipcMessageGroup          */ 
                                    INPUT 112,  /* The &1 specified is invalid. &2 */         /* ipiMessageNumber         */ 
                                    INPUT "Coding Status Reason," + cValidMessage).           /* ipcReplaceTextList       */ 
  END. /* ELSE - IF btt_auth_coding.coding_status_note = "":U THEN */
  
  /*
    Duplicate main code validation
    If an existing record is being updated and the main code value changed to 
    true or the record is new and the main code value is true then we should 
    validate that we dont already have have a main code for that mnemonic.
  */
  IF btt_auth_coding.main_code = TRUE        
  AND (btt_auth_coding.auth_coding_obj <= 0.00 
  OR  (btt_auth_coding.auth_coding_obj >  0.00 
  AND  CAN-FIND(FIRST hat_auth_coding NO-LOCK
              WHERE hat_auth_coding.auth_coding_obj        = btt_auth_coding.auth_coding_obj
                AND hat_auth_coding.owning_entity_mnemonic = btt_auth_coding.owning_entity_mnemonic
                AND hat_auth_coding.main_code              = FALSE))) THEN /* main code updated from false to true*/   
  DO:
    /*
      Check if we already have a main code for this auth with the same mnemonic
    */
    FIND FIRST bbt_auth_coding NO-LOCK
         WHERE bbt_auth_coding.auth_coding_obj       <> btt_auth_coding.auth_coding_obj
           AND bbt_auth_coding.auth_obj               = btt_auth_coding.auth_obj
           AND bbt_auth_coding.owning_entity_mnemonic = btt_auth_coding.owning_entity_mnemonic
           AND bbt_auth_coding.main_code              = TRUE
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE bbt_auth_coding
    THEN
      goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                              INPUT btt_auth_coding.auth_coding_obj,
                              INPUT "":U,
                              INPUT "main_code":U,
                              INPUT btt_auth_coding.line_number,
                              INPUT SUBSTITUTE("Only one &1 code may be set as the main code and the &1 code (&2) has already been set as the main code.":U,
                                               cOwningEntity, bbt_auth_coding.owning_alt_value),
                              INPUT "ERR":U).
  END. /*IF btt_auth_coding.main_code = TRUE.......*/

  /*
    Diagnosis specific validation ( Please put all diagnosis specific validation here!!! )
  */
  IF btt_auth_coding.owning_entity_mnemonic = "diagnos":U THEN
  DO:
    /*
      Check if we should do ICD10MIT validations
      
       - ICDDATE      : Validate if the ICD code is active for the date captured.	       ( Partial and complete save )
                                                                                         
       - CLINICAL	    : Validate if the ICD code is clinically valid.	                   ( Partial and complete save )
                                                                                         
       - PRIMARY	    : Validate if the ICD code is valid if used as a primary code.     ( Partial and complete save )
                      
       - MORPHOLOGY	  : Validate that a morphology code is captured when a cancer code 
                        is captured as the first ICD code.                               ( Partial and complete save )
                        
       - GENDER	      : Validate if the gender is valid for the dependant for all ICD 
                        codes that are captured.	                                       ( Partial and complete save )
                         
       - AGE	        : Validate if the age is valid for the dependant
                        for all the ICD codes that are captured. 	                       ( Partial and complete save )
                                        
       - ASTERISKLINK	: Validate if an asterisk code is captured that it can be used     ( Partial and complete save )
                        with the dagger code.	
                                                
       - ASTERISK	    : Validate that an asterisk code can be captured as the first 
                        ICD code and used on its own.	On save of                         ( Partial and complete save )
      
      
       - ECC	        : Validate that an external cause code is captured where an 
                        "S" or a "T" code is captured in first ICD code.	               ( Complete save only !!! )
    */
    ASSIGN lICD10MITVal = (CAN-FIND(FIRST datalist 
                                    WHERE datalist.list-type = "ICD10MIT":U
                                      AND datalist.list-code = "ENABLEFOR":U
                                      AND datalist.link-from = "":U
                                      AND LOOKUP("MCA":U,datalist.description) > 0) AND NOT 
                           CAN-FIND(FIRST datalist      
                                    WHERE datalist.list-type = "ICD10MIT":U
                                      AND datalist.list-code = "MCAEXC":U 
                                      AND datalist.link-from = "":U
                                      AND LOOKUP(btt_auth_coding.owning_alt_value,datalist.description) > 0)) AND NOT CAN-DO("6":U, STRING(btt_auth_coding.coding_status)).
    
    /*
      ICD10 MIT Validation                  
    */
    IF  goAuthorisation:InFocus                  
    AND goAuthorisation:MemberOptionCode <> 0.00 
    AND lICD10MITVal THEN
    DO:
      EMPTY TEMP-TABLE tt-additionalicd.
            
      /*
        The main provider/doctor as well as the discipline 
        is required for the ICD10MIT validation
      */
      FIND FIRST btt_auth_provider NO-LOCK
           WHERE btt_auth_provider.auth_obj      = goAuthorisation:AuthObj
             AND btt_auth_provider.main_provider = TRUE
        NO-ERROR.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF NOT AVAILABLE btt_auth_provider
      THEN
        FIND FIRST btt_auth_provider NO-LOCK
             WHERE btt_auth_provider.auth_obj = goAuthorisation:AuthObj           
          NO-ERROR.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
                             
      IF AVAILABLE btt_auth_provider 
      THEN
        ASSIGN iDocNum = btt_auth_provider.doc_num 
               cPrType = STRING(btt_auth_provider.pr_type).
            
      /*
        Prepare additional icd code table used for external cause code check
      */
      FOR EACH tt_auth_coding NO-LOCK
         WHERE tt_auth_coding.auth_coding_obj <> btt_auth_coding.auth_coding_obj
         AND   tt_auth_coding.owning_entity_mnemonic = "diagnos":U:
        
        ASSIGN lPrimaryCodeECC = IF oICDPrimaryCode:AuthRuleInFocus
                                 THEN LOOKUP(TRIM(tt_auth_coding.coding_type),TRIM(oICDPrimaryCode:RuleValue)) > 0
                                 ELSE tt_auth_coding.primary_code.
        
        IF NOT lPrimaryCodeECC THEN 
        DO:
          /*
            Find Associated
          */
          ASSIGN dDiagnosObj = tt_auth_coding.ass_diag_obj.
          
          {&FindAssociated}          
          
          /*
            Find Morphology
          */
          ASSIGN dDiagnosObj = tt_auth_coding.morph_diag_obj.
          
          {&FindMorphology}
                                          
          CREATE tt-additionalicd.
          ASSIGN tt-additionalicd.diagnosis  = tt_auth_coding.owning_alt_value
                 tt-additionalicd.ass-code   = cAssociatedCode
                 tt-additionalicd.morph-code = cMorphologyCode.
                 
          VALIDATE tt-additionalicd.
        END. /*IF NOT lPrimaryCodeECC THEN */
      END. /*FOR EACH tt_auth_coding NO-LOCK*/      
      
      /*
        Find Associated
      */
      ASSIGN dDiagnosObj = btt_auth_coding.ass_diag_obj.
      
      {&FindAssociated}      
      
      /*
        Find Morphology
      */
      ASSIGN dDiagnosObj = btt_auth_coding.morph_diag_obj.
      
      {&FindMorphology}
      
      RUN ma/mem/maicd10mit.p
        (INPUT  TABLE tt-additionalicd BY-REFERENCE,                                                                                   /*Additional ICD's                       */
         INPUT  btt_auth_coding.owning_alt_value,                                                                                      /*Diagnosis                              */
         INPUT  cAssociatedCode,                                                                                                       /*Associated                             */
         INPUT  cMorphologyCode,                                                                                                       /*Morphology                             */
         INPUT  btt_auth_coding.start_date,                                                                                            /*Claim Date                             */
         INPUT  goAuthorisation:MemberOptionCode,                                                                                      /*Scheme Code                            */
         INPUT  goAuthorisation:MemNum,                                                                                                /*Member No.                             */
         INPUT  STRING(goAuthorisation:Dependant),                                                                                     /*Dependant                              */
         INPUT  cPrType,                                                                                                               /*PR Type                                */
         INPUT  iDocNum,                                                                                                               /*Doc No.                                */
         INPUT  lPrimaryCode,                                                                                                          /*Primary                                */
         INPUT  (LOOKUP(SUBSTRING(btt_auth_coding.owning_alt_value, 1, 1),"S,T":U) > 0 AND btt_auth_coding.record_action = 'MODIFY'),  /*ECC  - DO NOT DO THIS FOR PARTIAL SAVE */
         OUTPUT cErrorType,
         OUTPUT cMessage,
         OUTPUT cNote,
         OUTPUT cPrimaryCode,
         OUTPUT cDiagnosisType) NO-ERROR.
         
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:138,PROGRESS:91' &ResetIgnoredErrors = TRUE }   
            
      /*
        We have an error or warning
      */
      IF cErrorType <> "":U THEN 
      DO:
        /*
          Diagnosis Type : Can be used to determine which of the ICD codes are invalid and which field
                           must be marked with red to indicate that there is an error"
                           
                           -	Will contain "Main" if the ICD code field is invalid. 
                           -	Will contain "Asterisk" if the Asterisk code is invalid. 
                           -	Will contain "Morphology" if the the morphology code is invalid.
        */
        CASE cDiagnosisType:
        
          WHEN "Asterisk":U 
          THEN ASSIGN cErrorField = "ass_diag_obj":U.
          
          WHEN "Morphology":U 
          THEN ASSIGN cErrorField = "morph_diag_obj":U.
          
          OTHERWISE 
          DO:
            ASSIGN cErrorField = "owning_alt_value":U.
          END. /*OTHERWISE */
        
        END CASE. /*CASE cDiagnosisType:*/
        
        IF cErrorType = "REJECT":U THEN 
        DO:
          /*
            Find the status reason why the ICD was rejected and if it's a valid note code
            then assign that to the coding_status_note.
          */
          FIND FIRST note NO-LOCK
               WHERE note.type        = "AS6":U
                 AND note.key         = cNote
                 AND note.scheme-code = 00
            NO-ERROR.
          
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
          
          ASSIGN btt_auth_coding.coding_status      = 6 /* Declined */
                 btt_auth_coding.coding_status_note = (IF AVAILABLE note THEN note.key ELSE "":U).
                 
          VALIDATE btt_auth_coding.    
        
          /*
            If the note code is not correctly set up, or if no note code is set up in ASI - Codes
            or the particular issue (in cMessage), then we display the issue as a warning.
          */
          IF cMessage <> "":U THEN
          DO:
            ASSIGN cErrorType = "WARN":U
                   cMessage   = cMessage + IF AVAILABLE note 
                                           THEN (SUBSTITUTE(". The primary diagnosis has been rejected with status reason &1 (&2)",
                                                            note.key ,note.narration[1] ))
                                           ELSE "":U.
            
            goErrorObject:addError
                          (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                           INPUT btt_auth_coding.auth_coding_obj,
                           INPUT "":U,
                           INPUT cErrorField,
                           INPUT btt_auth_coding.line_number,
                           INPUT cMessage,
                           INPUT cErrorType).
          END.  /* IF cMessage <> "" AND... */

          /*
            If we are declining the main/primary code we need to mark the authorisation as incomplete
          */
          IF btt_auth_coding.main_code 
          OR btt_auth_coding.primary_code THEN 
          DO:
            FIND FIRST buf_auth EXCLUSIVE-LOCK
                 WHERE buf_auth.auth_obj = btt_auth_coding.auth_obj 
              NO-ERROR.

            FIND FIRST btt_auth
                 WHERE btt_auth.auth_obj = btt_auth_coding.auth_obj 
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
            
            IF  AVAILABLE buf_auth 
            AND AVAILABLE btt_auth 
            THEN
              ASSIGN buf_auth.auth_incomplete = TRUE
                     btt_auth.auth_incomplete = TRUE.

          END. /* IF btt_auth_coding.main_code */
        END. /*IF cErrorType = "REJECT":U THEN */
      END. /*IF cErrorType <> "":U THEN */                        
    END. /*IF lICD10MITVal THEN*/

    /*
      If status is declined (6) or canceled(5), skip validation.
    */
    IF  btt_auth_coding.coding_status <> 5 /* Cancelled */
    AND btt_auth_coding.coding_status <> 6 /* Declined  */ THEN
    DO:
      /*
         Validation for MVA (Motor Vehicle Accident) ICD's 
         SURELY THIS SHOULD GO INTO CODING BUSINESS LOGIC, WHAT IF VALIDATION DOWN THE 
         LINE ABANDONS THE CREATION OF THIS CODING RECORD????????
      */
      IF btt_auth_coding.owning_alt_value BEGINS "V":U THEN
      DO:
        /*
          Activate the MVA flag for update/create if the ICD code starts with a 'V'
        */
        ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleDetails
                            (INPUT  goAuthorisation:InsurerObj,            /* ipdInsurerObj       */
                             INPUT  goAuthorisation:MemberOptionCode,      /* ipiOptionCode       */
                             INPUT  "ma_acAuthRuleTypeAuthCoding":U,       /* ipcRuleType         */
                             INPUT  "MVAFLAG":U,                           /* ipcRuleCode         */
                             INPUT  btt_auth_coding.start_date,            /* ipdEffectiveDate    */
                             OUTPUT lValidRule,                            /* oplValidRule        */
                             OUTPUT cRuleValue,                            /* opcRuleValue        */
                             OUTPUT dLinkAuthRuleObj,                      /* opdLinkAuthRuleObj  */
                             OUTPUT dAuthRuleObj).                         /* opdAuthRuleObj      */

        IF lValidRule 
        AND LOOKUP(cRuleValue,"Activate,Active,Yes,Y,True,T":U) > 0 THEN
        DO:
          ASSIGN oAuthRule = NEW cls.maauthrule().
          
          /*
            Get instance object for auth rule
          */
          oAuthRule:focusAuthRule(dLinkAuthRuleObj).
          
          IF oAuthRule:AuthRuleInFocus AND oAuthRule:RuleType = "ma_acAuthRuleTypeAuthFlag":U THEN
          DO:
            
            DATASET dsAuthFlagValue:EMPTY-DATASET().

            oFVSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE).
          
            ASSIGN lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
                   lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, btt_auth_coding.auth_obj)
                   lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_key":U            , "=":U, "":U)
                   lSuccess = oFVSearch:SetFilterCriteria("tt_auth_flag_value.auth_rule_obj":U         , "=":U, oAuthRule:AuthRuleObj).
            
            oFVSearch:fetchData().
            
            FIND FIRST tt_auth_flag_value EXCLUSIVE-LOCK NO-ERROR.
            
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF NOT AVAILABLE tt_auth_flag_value THEN
            DO:                
              CREATE tt_auth_flag_value.
              ASSIGN tt_auth_flag_value.record_action          = 'MODIFY':U
                     tt_auth_flag_value.owning_entity_mnemonic = 'hatau':U
                     tt_auth_flag_value.owning_obj             = goAuthorisation:AuthObj
                     tt_auth_flag_value.owning_key             = "":U
                     tt_auth_flag_value.owning_alt_value       = goAuthorisation:AuthNum
                     tt_auth_flag_value.auth_rule_obj          = oAuthRule:AuthRuleObj
                     tt_auth_flag_value.last_change_datetime   = NOW
                     tt_auth_flag_value.auth_flag_value        = "Yes":U.
            
              VALIDATE tt_auth_flag_value.
            END. /*IF NOT AVAILABLE tt_auth_flag_value THEN*/        
            ELSE IF tt_auth_flag_value.auth_flag_value <> "Yes":U THEN
            DO:                
              ASSIGN tt_auth_flag_value.record_action         = 'MODIFY':U
                     tt_auth_flag_value.last_change_datetime  = NOW
                     tt_auth_flag_value.auth_flag_value       = "Yes":U.
            
              VALIDATE tt_auth_flag_value.
            END. /*IF tt_auth_flag_value.auth_flag_value <> "Yes":U THEN*/
            
            mipEnv:Health:AuthBusinessLogic:saveAuthFlagValue(INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE).
            
            /*
              Copy any potential errors to the auth error table
            */
            FOR EACH tt_auth_flag_value_error NO-LOCK:
              
              CREATE tt_auth_error.
              BUFFER-COPY tt_auth_flag_value_error TO tt_auth_error
                   ASSIGN tt_auth_error.owning_entity_mnemonic = 'hatac:':U + btt_auth_coding.owning_entity_mnemonic
                          tt_auth_error.owning_obj             = btt_auth_coding.auth_coding_obj.

            END. /* FOR EACH tt_auth_flag_value_error NO-LOCK: */                          
          END.  /* IF oAuthRule:AuthRuleInFocus THEN */
        END.  /* IF lValidRule THEN */
      END.  /* IF btt_auth_coding.owning_alt_value BEGINS "V" THEN */
      
      /*
        Registered ICD code (diagnosis) dependant exclusion validation - validation should only apply if rule exists.
      */
      IF goAuthorisation:Dependant <> 99 THEN
      DO:
        ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                (INPUT  goAuthorisation:InsurerObj,
                                 INPUT  goAuthorisation:MemberOptionCode,
                                 INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                 INPUT  "DependantExclusions":U,
                                 INPUT  btt_auth_coding.start_date,
                                 OUTPUT lValidRule,
                                 OUTPUT cRuleValue).
        IF lValidRule THEN
        DO:
          mipEnv:Health:maCondition:checkDepExclusions (INPUT  goAuthorisation:MemberOptionCode,
                                                        INPUT  goAuthorisation:MemNum,
                                                        INPUT  goAuthorisation:Dependant,
                                                        INPUT  btt_auth_coding.start_date,
                                                        INPUT  btt_auth_coding.owning_alt_value,
                                                        OUTPUT cMessage). 
        
          IF cMessage <> "":U THEN
          DO:
            IF LOOKUP(ENTRY(1,cRuleValue,"|":U),"WARN,WARNACK":U) > 0 THEN
            DO:
              ASSIGN cErrorType = "WARN":U
                     cMessage   = cMessage + "[HELP=Auth Rule Code: DependantExclusions]".
        
              IF ENTRY(1,cRuleValue,"|":U) = "WARNACK":U
              THEN ASSIGN lAcknowledge = TRUE.
              ELSE ASSIGN lAcknowledge = FALSE.
        
              goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic + 
                                            ":":U +  btt_auth_coding.owning_alt_value,
                                      INPUT btt_auth_coding.auth_coding_obj,
                                      INPUT "":U,
                                      INPUT "coding_status":U,
                                      INPUT btt_auth_coding.line_number,
                                      INPUT cMessage,
                                      INPUT cErrorType,
                                      INPUT lAcknowledge).
        
            END. /* IF LOOKUP(ENTRY(1,cRuleValue,"|":U),"WARN,WARNACK":U) > 0 THEN */
            ELSE IF TRIM(ENTRY(1,cRuleValue,"|":U)) = "DECLINE":U AND NUM-ENTRIES(cRuleValue,"|":U) > 1 
              THEN
                ASSIGN btt_auth_coding.coding_status      = 6  /* Declined */
                       btt_auth_coding.coding_status_note = TRIM(ENTRY(2,cRuleValue,"|":U)).
        
          END. /* IF cMessage <> "" */
        END. /* IF lValidRule */
      END.  /* IF goAuthorisation:Dependant <> 99 THEN */
    END. /* IF  btt_auth_coding.coding_status <> 5 AND btt_auth_coding.coding_status <> 6 THEN */

    IF goAuthorisation:InFocus AND AVAILABLE ttAuthTypeConfig THEN
    DO:
      /*
        Validate that ICD codes are valid for Auth Type setup
      */ 
      IF ttAuthTypeConfig.ValidIcds <> "":U 
      AND LOOKUP(btt_auth_coding.owning_alt_value, ttAuthTypeConfig.ValidIcds) = 0
      THEN 
        ASSIGN cValidIcds = REPLACE(ttAuthTypeConfig.ValidIcds, ",":U , "  ":U) 
               lSuccess   = goErrorObject:addError
                              (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":":U + btt_auth_coding.owning_alt_value,                             /* ipcOwningEntityMnemonic  */ 
                               INPUT btt_auth_coding.auth_coding_obj,                                                                                            /* ipdOwningEntityObj       */  
                               INPUT "":U,                                                                                                                       /* ipcOwningEntityKey       */ 
                               INPUT "owning_alt_value":U,                                                                                                       /* ipcFieldName             */ 
                               INPUT btt_auth_coding.line_number,                                                                                                /* ipiLineNumber            */  
                               INPUT "MA":U,                                                                                                                     /* ipcMessageGroup          */ 
                               INPUT 112,                                                                                                                        /* ipiMessageNumber         */ 
                               INPUT "ICD Code, The ICD code specified has not been set up for Authorisation Type. Valid ICD's are: ":U + CHR(10) + cValidIcds). /* ipcReplaceTextList       */     
      
      /*
        Validate that ICD codes are valid for Auth Type Condition Diagnosis setup
      */ 
      IF  ttAuthTypeConfig.IcdCondCode <> "":U
      AND ttAuthTypeConfig.IcdCondType <> "":U THEN 
      DO:
        IF btt_auth_coding.ass_diag_obj <> 0.00 
        THEN 
          FIND diagnos NO-LOCK 
            WHERE diagnos.diagnos-obj = btt_auth_coding.ass_diag_obj 
            NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
        
        IF AVAILABLE diagnos
        THEN ASSIGN cAssociatedCode = diagnos.diagnosis.
        ELSE ASSIGN cAssociatedCode = "":U.
         
        mipEnv:Health:maMedical:validateCondDiag(INPUT ttAuthTypeConfig.IcdCondCode,
                                                 INPUT ttAuthTypeConfig.IcdCondType,
                                                 INPUT btt_auth_coding.start_date,
                                                 INPUT btt_auth_coding.owning_alt_value,
                                                 INPUT cAssociatedCode,
                                                 INPUT 0,
                                                 INPUT "":U,
                                                 INPUT NO,
                                                 OUTPUT cICDlist,
                                                 OUTPUT oErrorObject).    
      
        /* 
          Return list of valid ICDs if ICD was invalid And/Or Error for other validations 
        */                                    
        IF cICDlist <> "":U   
        OR (VALID-OBJECT(oErrorObject) AND oErrorObject:ErrorNumber <> 0) 
        THEN
          ASSIGN cValidIcds = REPLACE(cICDlist, ",":U , "  ":U) 
                 lSuccess   = goErrorObject:addError
                                (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,                                                                 /* ipcOwningEntityMnemonic  */ 
                                 INPUT btt_auth_coding.auth_coding_obj,                                                                                     /* ipdOwningEntityObj       */  
                                 INPUT "":U,                                                                                                                /* ipcOwningEntityKey       */ 
                                 INPUT "owning_alt_value":U,                                                                                                /* ipcFieldName             */ 
                                 INPUT btt_auth_coding.line_number,                                                                                         /* ipiLineNumber            */  
                                 INPUT "MA":U,                                                                                                              /* ipcMessageGroup          */ 
                                 INPUT 112,                                                                                                                 /* ipiMessageNumber         */ 
                                 INPUT "ICD Code, Condition Diagnosis not set up for the Authorisation Type.  Valid ICDs are: ":U + CHR(10) + cValidIcds).  /* ipcReplaceTextList       */     
      
      END.  /* IF  ttAuthTypeConfig.IcdCondCode <> "" AND ttAuthTypeConfig.IcdCondType <> ""*/    
    END.  /* IF goAuthorisation:InFocus AND AVAILABLE ttAuthTypeConfig THEN */      
  END. /* IF btt_auth_coding.owning_entity_mnemonic = "diagnos" */

  /*
    CPT specific validation ( Please put all cpt specific validation here!!! )
  */
  IF btt_auth_coding.owning_entity_mnemonic = "hlmck":U THEN
  DO: 

    DATASET dsCpt:EMPTY-DATASET().

    /*
      Fetch CPT data required for CPT validations
    */
    oCPSearch = NEW ma.cls.macptsearch(DATASET dsCpt:HANDLE).

    /*
      We need to find all the modifiers for all the CPT's on the auth, because modifier 51 validation
      further down checks the CPT modifiers on other CPT's for the auth as well.
    */
    ASSIGN 
       lSuccess = oCPSearch:SetCriteria("BufferList":U, "tt_cpt_link,tt_cpt,tt_cpt_modifier":U) /* Restrict the buffers to be filled by the data service */
       lSuccess = oCPSearch:SetFilterCriteria("tt_cpt_link.cpt_link_obj":U, "=":U, btt_auth_coding.owning_obj)
       . 

    FOR EACH bbt_auth_coding NO-LOCK
       WHERE bbt_auth_coding.auth_obj               = btt_auth_coding.auth_obj
         AND bbt_auth_coding.owning_entity_mnemonic = btt_auth_coding.owning_entity_mnemonic
         AND bbt_auth_coding.auth_coding_obj       <> btt_auth_coding.auth_coding_obj:
      
      ASSIGN lSuccess = oCPSearch:SetFilterCriteria("tt_cpt_link.cpt_link_obj":U, "=":U, bbt_auth_coding.owning_obj).
             
    END. /* FOR EACH bbt_auth_coding NO-LOCK */

    FOR EACH bbt_auth_coding NO-LOCK
       WHERE bbt_auth_coding.auth_obj               = btt_auth_coding.auth_obj
         AND bbt_auth_coding.owning_entity_mnemonic = btt_auth_coding.owning_entity_mnemonic:
      
      ASSIGN lSuccess = oCPSearch:SetFilterCriteria("tt_cpt_link.cpt_link_obj":U, "=":U, bbt_auth_coding.owning_obj).
             
    END. /* FOR EACH bbt_auth_coding NO-LOCK */
    
    oCPSearch:fetchCptData().

    ASSIGN lActiveCPTFound = FALSE.

    /*
      Ensure that procedure(CPT) is a valid code and it is still active and we have the latest one
    */  
    CPT-BLK:
    FOR EACH  tt_cpt NO-LOCK
       WHERE  tt_cpt.cpt_code        = btt_auth_coding.owning_alt_value
         AND  tt_cpt.effective_date <= btt_auth_coding.start_date
         AND (tt_cpt.end_date       >= btt_auth_coding.start_date 
          OR  tt_cpt.end_date        = ?)
          BY  tt_cpt.effective_date DESCENDING:
    
      ASSIGN lActiveCPTFound = TRUE.

      mipEnv:Health:Authbusinesslogic:activateProcedureDate(INPUT  goAuthorisation:InsurerObj,
                                                            INPUT  goAuthorisation:MemberOptionCode,
                                                            INPUT  btt_auth_coding.start_date,
                                                            INPUT  btt_auth_coding.owning_alt_value,
                                                            INPUT  tt_cpt.service_type,
                                                            OUTPUT lEnableProcedureDate,
                                                            OUTPUT cProcedureDateAction).
      IF  lEnableProcedureDate THEN
      DO:
        IF  btt_auth_coding.procedure_date  = ?
        AND btt_auth_coding.coding_status  <> 5
        AND btt_auth_coding.coding_status  <> 6  THEN
        DO:
          ASSIGN cAction =  IF NUM-ENTRIES(cProcedureDateAction,"|":U) > 1 THEN ENTRY(2,cProcedureDateAction,"|")  ELSE "".

          CASE cAction:
            WHEN "WARN":U    OR 
            WHEN "WARNACK":U OR 
            WHEN "BLOCK":U THEN
            DO:
              ASSIGN
                lAcknowledge   = (IF cAction = "WARNACK":U
                                 THEN TRUE
                                 ELSE FALSE)

                cErrorMessage = "Procedure date for coding (" + btt_auth_coding.owning_alt_value + ") can not be blank or unknown." +
                                "[HELP=Auth Rule Code: CPTProcedureDate]":U

                cErrorType    = IF LOOKUP(cAction,"WARN,WARNACK":U) > 0
                                THEN "WAR":U
                                ELSE "ERR":U.

                goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":":U +  btt_auth_coding.owning_alt_value,
                                        INPUT btt_auth_coding.auth_coding_obj,
                                        INPUT "":U,
                                        INPUT "procedure_date":U,
                                        INPUT btt_auth_coding.line_number,
                                        INPUT cErrorMessage,
                                        INPUT cErrorType,
                                        INPUT lAcknowledge).

            END. /* WHEN "WARN":U OR WHEN "WARNACK":U OR WHEN "BLOCK":U THEN */

            WHEN "DECLINE":U
            THEN
              ASSIGN
                btt_auth_coding.coding_status      = 6
                btt_auth_coding.coding_status_note = IF NUM-ENTRIES(cProcedureDateAction,"|":U) > 2 THEN ENTRY(3,cProcedureDateAction,"|") ELSE "" .

          END CASE. /* CASE cAction: */
        END. /* IF AND btt_auth_coding.procedure_date = ? AND btt_auth_coding.coding_status  <> 5 AND btt_auth_coding.coding_status  <> 6  */
      END. /* IF lEnableProcedureDate THEN */
      ELSE
        ASSIGN btt_auth_coding.procedure_date = ?.

      /*
        The following validations will only be done if the record status is not 5(Cancelled) or 6(Declined).
      */
      IF LOOKUP(STRING(btt_auth_coding.coding_status),"5,6":U) = 0 THEN 
      DO:
        /*
          Validate the procedure(CPT) ages - if CPT age validations are active
        */
        IF goAuthorisation:Dependant <> 99 THEN
        DO:
          ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                                           INPUT  goAuthorisation:MemberOptionCode,
                                                                           INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                                           INPUT  "CPTAgeValidations":U,
                                                                           INPUT  btt_auth_coding.start_date,
                                                                           OUTPUT lValidRule,
                                                                           OUTPUT cRuleValue).
          IF lValidRule THEN
          DO:
            ASSIGN dAgeRangeObj = (IF goAuthorisation:DependantGender = "F":U AND tt_cpt.age_range_female_obj <> 0.00
                                   THEN tt_cpt.age_range_female_obj
                                   ELSE
                                     (IF goAuthorisation:DependantGender = "M":U AND tt_cpt.age_range_male_obj <> 0.00
                                      THEN tt_cpt.age_range_male_obj
                                      ELSE tt_cpt.age_range_both_obj)).
          
            /*
              Perform age validations if we have an age range obj from the CPT
            */
            IF dAgeRangeObj <> 0.00 THEN
            DO:
              /*
                Validate dependant's age against the age range configured against the procedure code if any
              */
              mipEnv:Health:maUtility:validateAge(INPUT  goAuthorisation:DependantAgeYears, /* Could the dependant be a newborn in which case we should check days??? */
                                                  INPUT  dAgeRangeObj,
                                                  OUTPUT lValid,
                                                  OUTPUT cErrorMessage).
          
              IF NOT lValid THEN
              DO:
                /*
                  If the rule has a value of "decline" then we will decline the 
                  coding line and use the reason code specified.
                */
                IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "DECLINE":U AND NUM-ENTRIES(cRuleValue, "|":U) > 1 THEN
                DO:
                  ASSIGN btt_auth_coding.coding_status      = 6 /* Declined */
                         btt_auth_coding.coding_status_note = ENTRY(2, cRuleValue, "|":U).
          
                  VALIDATE btt_auth_coding.
                END. /*IF TRIM(ENTRY(1, cRuleValue)) = "DECLINE":U AND NUM-ENTRIES(cRuleValue) > 1 THEN*/
                ELSE
                DO:
                  /*
                    Check if we should return an error or warning based on the CPTAgeValidations rule. 
                  */  
                  ASSIGN lAcknowledge  = (IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "WARNACK":U
                                          THEN TRUE
                                          ELSE FALSE)
                  
                         cErrorType    = (IF LOOKUP(ENTRY(1, cRuleValue, "|":U),"WARN,WARNACK":U) > 0
                                          THEN "WAR":U
                                          ELSE "ERR":U)
                                       
                         cErrorMessage = SUBSTITUTE("The dependant's age (&1) is invalid for the specified CPT(Procedure) code (&2).":U, 
                                                    STRING(goAuthorisation:DependantAgeYears), tt_cpt.cpt_code)
                         cErrorMessage = cErrorMessage + "[HELP=Auth Rule Code: CPTAgeValidations]".
          
                  /*
                    Add error/warning to the dataset for the invalid age
                  */
                  goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":":U +  btt_auth_coding.owning_alt_value,  
                                          INPUT btt_auth_coding.auth_coding_obj,
                                          INPUT "":U,
                                          INPUT "owning_alt_value":U,
                                          INPUT btt_auth_coding.line_number,
                                          INPUT cErrorMessage,
                                          INPUT cErrorType,
                                          INPUT lAcknowledge).
                END. /* ELSE */
              END. /* IF NOT lValid THEN */
            END. /* IF dAgeRangeObj <> 0.00 THEN */
          END. /* IF lValidRule THEN */
        END.  /* IF goAuthorisation:Dependant <> 99 THEN */

        /*
          Validate the procedure(CPT) gender only if the cpt gender is Male or Female
        */
        IF LOOKUP(TRIM(tt_cpt.cpt_gender),"F,M":U) > 0 THEN
        DO:
          /*
            Check if gender validations are active
          */
          IF goAuthorisation:Dependant <> 99 THEN
          DO:
            ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                                             INPUT  goAuthorisation:MemberOptionCode,
                                                                             INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                                             INPUT  "CPTGenderValidations":U,
                                                                             INPUT  btt_auth_coding.start_date,
                                                                             OUTPUT lValidRule,
                                                                             OUTPUT cRuleValue).
            /*
              CPT gender validations are active
            */
            IF lValidRule 
            AND TRIM(tt_cpt.cpt_gender) <> TRIM(goAuthorisation:DependantGender) THEN
            DO:
              /*
                If the rule has a value of "decline" then we will decline the 
                coding line and use the reason code specified.
              */
              IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "DECLINE":U 
              AND NUM-ENTRIES(cRuleValue, "|":U) > 1 THEN
              DO:
                ASSIGN btt_auth_coding.coding_status      = 6 /* Declined */
                       btt_auth_coding.coding_status_note = ENTRY(2, cRuleValue, "|":U).
            
                VALIDATE btt_auth_coding.
              END. /*IF TRIM(ENTRY(1, cRuleValue)) = "DECLINE":U AND NUM-ENTRIES(cRuleValue) > 1 THEN*/
              ELSE
              DO:
                /*
                  Check if we should return an error or warning based on the CPTGenderValidations rule.
                */
                ASSIGN lAcknowledge  = (IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "WARNACK":U
                                        THEN TRUE
                                        ELSE FALSE)
                  
                       cErrorType    = (IF LOOKUP(ENTRY(1, cRuleValue, "|":U),"WARN,WARNACK":U) > 0
                                        THEN "WAR":U
                                        ELSE "ERR":U)
            
                       cErrorMessage = SUBSTITUTE("The dependant's gender (&1) is invalid for the specified CPT(Procedure) code (&2).":U, STRING(goAuthorisation:DependantGender), tt_cpt.cpt_code)
		                   cErrorMessage = cErrorMessage + "[HELP=Auth Rule Code: CPTGenderValidations]".
            
                /*
                  Add error/warning to the dataset for the invalid gender
                */
                goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":":U +  btt_auth_coding.owning_alt_value,
                                        INPUT btt_auth_coding.auth_coding_obj,
                                        INPUT "":U,
                                        INPUT "owning_alt_value":U,
                                        INPUT btt_auth_coding.line_number,
                                        INPUT cErrorMessage,
                                        INPUT cErrorType,
                                        INPUT lAcknowledge).
              END. /* ELSE */
            END. /* IF lValidRule AND TRIM(tt_cpt.cpt_gender) <> TRIM(goAuthorisation:DependantGender) THEN */
          END. /* IF goAuthorisation:Dependant <> 99 THEN */
        END. /* IF LOOKUP(TRIM(tt_cpt..cpt_gender),"F,M":U) > 0 THEN */

        /*
          Check if Modifier 63 is set up for CPT-code
        */
        IF goAuthorisation:Dependant <> 99 THEN
        DO:
          ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                                           INPUT  goAuthorisation:MemberOptionCode,
                                                                           INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                                           INPUT  "CPTModifier63":U,
                                                                           INPUT  btt_auth_coding.start_date,
                                                                           OUTPUT lValidRule,
                                                                           OUTPUT cRuleValue).
          /*
            CPT modifier 63 is set up for CPT-code
          */        
          IF lValidRule THEN
          DO:
            FIND FIRST  tt_cpt_modifier NO-LOCK
                 WHERE  tt_cpt_modifier.cpt_obj         = tt_cpt.cpt_obj
                   AND  tt_cpt_modifier.modifier_code   = "M63":U
                   AND  tt_cpt_modifier.effective_date <= btt_auth_coding.start_date
                   AND (tt_cpt_modifier.end_date       >= btt_auth_coding.start_date 
                    OR  tt_cpt_modifier.end_date        = ?)
              NO-ERROR.
          
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
          
            IF AVAILABLE tt_cpt_modifier THEN
            DO:
              /* Procedure (CPT) is only allowed if dependant age <= 28 days */
              IF goAuthorisation:DependantAgeYears <> 0
              OR goAuthorisation:DependantAgeDays   > 28 THEN 
              DO:
                /*
                  If the rule has a value of "decline" then we will decline the 
                  coding line and use the reason code specified.
                */
                IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "DECLINE":U THEN
                DO:
                  ASSIGN btt_auth_coding.coding_status      = 6 /* Declined */
                         btt_auth_coding.coding_status_note = IF NUM-ENTRIES(cRuleValue, "|":U) > 1
                                                              THEN ENTRY(2, cRuleValue, "|":U)
                                                              ELSE btt_auth_coding.coding_status_note.
                  VALIDATE btt_auth_coding.
                END. /* IF TRIM(ENTRY(1, cRuleValue)) = "DECLINE":U THEN */
                ELSE
                DO:
                  /*
                    Check if we should return an error or warning based on the CPTMofifier63 rule.
                  */
                  ASSIGN lAcknowledge  = (IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "WARNACK":U
                                          THEN TRUE
                                          ELSE FALSE)
                  
                         cErrorType    = (IF LOOKUP(ENTRY(1, cRuleValue, "|":U),"WARN,WARNACK":U) > 0
                                          THEN "WAR":U
                                          ELSE "ERR":U)
          
                         cDependantAge = IF goAuthorisation:DependantAgeYears > 0
                                         THEN STRING(goAuthorisation:DependantAgeYears) + " yr"
                                         ELSE STRING(goAuthorisation:DependantAgeDays) + " days"
              
                         cErrorMessage = SUBSTITUTE("The dependant's age (&1) is >28 days.  According to Modifier 63, the specified CPT Procedure code (&2) is invalid for this dependant.",
                                                    cDependantAge, tt_cpt.cpt_code)
		                     cErrorMessage = cErrorMessage + "[HELP=Auth Rule Code: CPTModifier63]".
              
                  /*
                    Add error/warning to the dataset for the invalid dependant age
                  */
                  goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":":U +  btt_auth_coding.owning_alt_value,
                                          INPUT btt_auth_coding.auth_coding_obj,
                                          INPUT "":U,
                                          INPUT "owning_alt_value":U,
                                          INPUT btt_auth_coding.line_number,
                                          INPUT cErrorMessage,
                                          INPUT cErrorType,
                                          INPUT lAcknowledge).
          
                END.  /* ELSE: IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "DECLINE":U THEN */
              END.  /* IF goAuthorisation:DependantAgeYears <> 0 OR goAuthorisation:DependantAgeDays > 28 */
            END.  /* IF AVAILABLE tt_cpt_modifier THEN */
          END.  /* IF lValidRule THEN */
        END.  /* IF goAuthorisation:Dependant <> 99 THEN */

        /*
          Check if Modifier 51 is set up for CPT-code
        */
        ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                                         INPUT  goAuthorisation:MemberOptionCode,
                                                                         INPUT  "ma_acAuthRuleTypeAUTHCODING":U,
                                                                         INPUT  "CPTModifier51":U,
                                                                         INPUT  btt_auth_coding.start_date,
                                                                         OUTPUT lValidRule,
                                                                         OUTPUT cRuleValue).
        /*
          CPT modifier 51 rule is activated
        */
        IF lValidRule THEN
        DO:
          /* 
            We don't care if this modifier is for this coding line or another coding line on this auth.
            If the CPT Modifier is applicable to any of the coding lines on the auth, we need to 
            validate it.
          */
          FIND FIRST  tt_cpt_modifier NO-LOCK
               WHERE  tt_cpt_modifier.modifier_code   = "M51":U
                 AND  tt_cpt_modifier.effective_date <= btt_auth_coding.start_date
                 AND (tt_cpt_modifier.end_date       >= btt_auth_coding.start_date 
                  OR  tt_cpt_modifier.end_date        = ?)
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE tt_cpt_modifier THEN
          DO:
            /* Procedure (CPT) can't be authorised if any other procedure (CPT) has been authorised */
            FIND FIRST bbt_auth_coding NO-LOCK
                 WHERE bbt_auth_coding.auth_coding_obj       <> btt_auth_coding.auth_coding_obj        /* We don't want to find the same record that we're validating now */
                   AND bbt_auth_coding.auth_obj               = btt_auth_coding.auth_obj               /* but we do want to check only the coding records for this auth   */
                   AND bbt_auth_coding.owning_entity_mnemonic = btt_auth_coding.owning_entity_mnemonic /* and we only want to check the other cpt records (hlmck)         */   
                   AND bbt_auth_coding.coding_status          = 1                                      /* that are authorised.                                            */
              NO-ERROR.
            
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
            
            IF AVAILABLE bbt_auth_coding THEN 
            DO:
              /*
                If the rule has a value of "decline" then we will decline the 
                coding line and use the reason code specified.
              */
              IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "DECLINE":U THEN
              DO:
                ASSIGN btt_auth_coding.coding_status      = 6 /* Declined */
                       btt_auth_coding.coding_status_note = IF NUM-ENTRIES(cRuleValue, "|":U) > 1
                                                            THEN ENTRY(2, cRuleValue, "|":U)
                                                            ELSE btt_auth_coding.coding_status_note.
            
                VALIDATE btt_auth_coding.
              END. /*IF TRIM(ENTRY(1, cRuleValue)) = "DECLINE":U AND NUM-ENTRIES(cRuleValue) > 1 THEN*/
              ELSE DO:
                FIND FIRST btt_cpt NO-LOCK
                     WHERE btt_cpt.cpt_obj = tt_cpt_modifier.cpt_obj
                  NO-ERROR.

                { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

                /*
                  Check if we should return an error or warning based on the CPTModifier51 rule.
                */
                ASSIGN lAcknowledge  = (IF TRIM(ENTRY(1, cRuleValue, "|":U)) = "WARNACK":U
                                        THEN TRUE
                                        ELSE FALSE)
                
                       cErrorType    = (IF LOOKUP(ENTRY(1, cRuleValue, "|":U),"WARN,WARNACK":U) > 0
                                        THEN "WAR":U
                                        ELSE "ERR":U)
            
                       cErrorMessage = SUBSTITUTE("According to Modifier 51, the specified CPT Procedure code (&1) can't be used with other CPT's.", 
                                                  btt_cpt.cpt_code)
	               cErrorMessage = cErrorMessage + "[HELP=Auth Rule Code: CPTModifier51]".
                                                                                                                                           
                /*
                  Add error/warning to the dataset for the invalid dependant age
                */
                goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic + ":":U +  btt_auth_coding.owning_alt_value,
                                        INPUT btt_auth_coding.auth_coding_obj,
                                        INPUT "":U,
                                        INPUT "owning_alt_value":U,
                                        INPUT btt_auth_coding.line_number,
                                        INPUT cErrorMessage,
                                        INPUT cErrorType,
                                        INPUT lAcknowledge).
              END. /* ELSE cRuleValue*/
            END.  /* IF AVAILABLE bbt_auth_coding */
          END.  /* IF AVAILABLE tt_cpt_modifier THEN */
        END. /* IF lValidRule THEN */
      END. /*IF LOOKUP(STRING(btt_auth_coding.coding_status),"5,6":U) = 0 THEN*/

      IF  btt_auth_coding.coding_status = 6 /* Declined */
      AND goAuthorisation:AuthStatus    = 1 /* Authorised */ THEN
      DO:
        ASSIGN cErrorMessage = SUBSTITUTE("The Authorisation Header can't be authorised if the primary/main code &1 is declined.",
                                         cOwningEntity).

        goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                                      INPUT btt_auth_coding.auth_coding_obj,
                                      INPUT "":U,
                                      INPUT "owning_alt_value":U,
                                      INPUT btt_auth_coding.line_number,
                                      INPUT cErrorMessage,
                                      INPUT "WAR":U).
      END.  /* IF btt_auth_coding.coding_status = 6 AND... */

      IF AVAILABLE ttAuthTypeConfig THEN
      DO:

        IF ttAuthTypeConfig.ActivateServiceType THEN
        DO:

          /* 
             if activateMaincode is enabled, we derive the service type from the main procedure code 
             OR if activateMaincode is disabled, we derive the service type from the primary procedure code
          */

          IF  lActivateMainCode  
          AND btt_auth_coding.main_code = TRUE
          OR (NOT lActivateMainCode
          AND lPrimaryCode) THEN
          DO:
            FIND FIRST buf_auth EXCLUSIVE-LOCK
                 WHERE buf_auth.auth_obj = btt_auth_coding.auth_obj
              NO-ERROR NO-WAIT.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
 
            IF AVAILABLE buf_auth 
            THEN
              ASSIGN buf_auth.service_type = tt_cpt.service_type.

          END. /* IF  lActivateMainCode AND btt_auth_coding.main_code = TRUE OR (NOT lActivateMainCode AND lPrimaryCode)  */
        END. /* IF ttAuthTypeConfig.ActivateServiceType */
      END. /* IF AVAILABLE ttAuthTypeConfig */ 

      /*
        And we're done
      */
      LEAVE CPT-BLK.
    END. /*FOR EACH  tt_cpt NO-LOCK*/ 

    /*
      No active CPT could be found - return an error
    */
    IF NOT lActiveCPTFound THEN
    DO:
      goErrorObject:addError (INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic,
                              INPUT btt_auth_coding.auth_coding_obj,
                              INPUT "":U,
                              INPUT "owning_alt_value":U,
                              INPUT btt_auth_coding.line_number,
                              INPUT SUBSTITUTE("No valid CPT(Procedure) could be found for '&1' effective '&2'":U, 
                                               btt_auth_coding.owning_alt_value, STRING(btt_auth_coding.start_date, "99/99/9999":U)),
                              INPUT "ERR":U).
    END. /*IF NOT AVAILABLE tt_cpt THEN*/       

    mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  goAuthorisation:InsurerObj,
                                                   INPUT  goAuthorisation:MemberOptionCode,
                                                   INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                   INPUT  "CPTBodyRegion":U,
                                                   INPUT  goAuthorisation:StartDate,
                                                   OUTPUT lValidRule,
                                                   OUTPUT cRuleValue).                                                  

    IF lValidRule 
    AND cRuleValue = "Activate":U 
    AND lAuthTypeEnableBodyRegion 
    AND btt_auth_coding.record_action <> "DELETE":U THEN
    DO:

      /* If no body region is specified, use the body region on the auth header */
      IF btt_auth_coding.body_region = "":U
      THEN
        ASSIGN btt_auth_coding.body_region = goAuthorisation:BodyRegion.

      IF btt_auth_coding.body_region <> goAuthorisation:BodyRegion THEN
      DO: 
        mipEnv:Health:AuthService:validateBodyRegion(INPUT goAuthorisation:InsurerObj,
                                                     INPUT goAuthorisation:MemberOptionCode, 
                                                     INPUT goAuthorisation:BodyRegion, 
                                                     INPUT goAuthorisation:StartDate,  
                                                     INPUT btt_auth_coding.body_region,
                                                     OUTPUT lValidBodyRegion, 
                                                     OUTPUT cValidationMessage).

        IF NOT lValidBodyRegion 
        AND cValidationMessage <> "" 
        THEN
          ASSIGN 
            cValidationMessage = REPLACE(cValidationMessage,"[&HELP]":U,"[HELP=Auth Rule Code: CPTBodyRegion]":U)
            cValidationMessage = REPLACE(cValidationMessage,"[&RULE]":U,"CPTBodyRegion,":U)
            lSuccess = goErrorObject:addError(INPUT "hatac:":U + btt_auth_coding.owning_entity_mnemonic, 
                                              INPUT btt_auth_coding.auth_coding_obj, 
                                              INPUT "":U, 
                                              INPUT "body_region":U,
                                              INPUT btt_auth_coding.line_number, 
                                              INPUT "The " + cOwningEntity + " body region " + btt_auth_coding.body_region + " on " +
                                                    btt_auth_coding.owning_alt_value + " is invalid. " +
                                                    cValidationMessage,                                         
                                              INPUT "ERR":U). 
       END. /* IF btt_auth_coding.body_region <> goAuthorisation:BodyRegion THEN */
    END. /* IF lValidRule AND cRuleValue = "Activate":U */
  END. /* IF btt_auth_coding.owning_entity_mnemonic = "hlmck":U THEN */  
END. /* IF AVAILABLE btt_auth_coding THEN */

&ENDIF

{ mip/inc/mipcatcherror.i 
  &FINALLY = "EMPTY TEMP-TABLE tt-additionalicd.    
              EMPTY TEMP-TABLE ttAuthTypeConfig.

              DATASET dsAuthFlagValue:EMPTY-DATASET().                                        
              DATASET dsCPT:EMPTY-DATASET().

              IF VALID-OBJECT(oFVSearch)       THEN DELETE OBJECT oFVSearch.
              IF VALID-OBJECT(oCPSearch)       THEN DELETE OBJECT oCPSearch.
              IF VALID-OBJECT(oAuthRule)       THEN DELETE OBJECT oAuthRule.
              IF VALID-OBJECT(oICDPrimaryCode) THEN DELETE OBJECT oICDPrimaryCode.
              IF VALID-OBJECT(oCPTPrimaryCode) THEN DELETE OBJECT oCPTPrimaryCode.
              IF VALID-OBJECT(oErrorObject)    THEN DELETE OBJECT oErrorObject.
              IF VALID-OBJECT(oBodyRegion)     THEN DELETE OBJECT oBodyRegion."}
