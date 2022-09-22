/* maauthbusvaldetail.i  MEDSTAR Medical Aid System
                         Validate Auth Detail Buffer
                         (c) Copyright 1990 - 2019
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/                      

DEFINE PARAMETER BUFFER btt_auth_detail FOR tt_auth_detail.
DEFINE PARAMETER BUFFER btt_auth_provider FOR tt_auth_provider.
DEFINE PARAMETER BUFFER btt_auth FOR tt_auth.

DEFINE VARIABLE cAction          AS CHARACTER   NO-UNDo.
DEFINE VARIABLE cErrorMessage    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErrorType       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessageType     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleValue       AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dLOSVariance     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dLOSCalculation  AS DECIMAL     NO-UNDO. 

DEFINE VARIABLE lAcknowledge     AS LOGICAL     NO-UNDO. 
DEFINE VARIABLE lMandatory       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lNappiRequired   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidationError AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidRule       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lValidStatus     AS LOGICAL     NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

/*
  MMP-481 Buffers for hat_auth, hat_auth_provider and hat_auth_detail have been removed in favor of using tt_auth_detail, tt_auth_provider and tt_auth
  alternate buffer temp-table detail (abtt_auth_detail) and alternate buffer temp-table provedrer (abtt_auth_provider) were set up instead of buf_auth_provider and hat_auth_detail to eliminate
  direct DB calls in favor of using temp-tables provided by the new DS
*/
DEFINE BUFFER abtt_auth_detail      FOR tt_auth_detail.
DEFINE BUFFER abtt_auth_provider    FOR tt_auth_provider.

DEFINE VARIABLE oBodyRegion       AS cls.mipacronym NO-UNDO.
DEFINE VARIABLE oAuthDetailLines  AS cls.mipacronym NO-UNDO.
DEFINE VARIABLE oAuthRule         AS cls.maauthrule NO-UNDO.
DEFINE VARIABLE oDetail           AS cls.maauthorisationdetail NO-UNDO.

IF goAuthorisation:InFocus AND AVAILABLE btt_auth_detail THEN
DO:
  ASSIGN oAuthRule = NEW cls.maauthrule()
         oDetail   = NEW cls.maauthorisationdetail().
  /*
     Ensure that only a valid auth status is used according to system and rule setups
  */
  ASSIGN lValidStatus = mipEnv:Health:AuthService:validateAuthStatus(INPUT  btt_auth_detail.auth_status,
                                                                     INPUT  "System":U).
  IF NOT lValidStatus THEN
  DO:                                        
    ASSIGN lSuccess = goErrorObject:addError
                                   (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,         /* ipcOwningEntityMnemonic */ 
                                    INPUT btt_auth_detail.auth_detail_obj,                           /* ipdOwningEntityObj      */ 
                                    INPUT "":U,                                                      /* ipcOwningEntityKey      */ 
                                    INPUT "auth_status":U,                                           /* ipcFieldName            */ 
                                    INPUT btt_auth_detail.line_number,                               /* ipiLineNumber           */ 
                                    INPUT "MA":U,                                                    /* ipcMessageGroup         */ 
                                    INPUT 100,  /* The '&1' specified is invalid */                  /* ipiMessageNumber        */ 
                                    INPUT "Status Code: ":U + STRING(btt_auth_detail.auth_status)).  /* ipcReplaceTextList      */ 
  END.  /* IF NOT lValidStatus THEN */

  /* Check Auth Type setup for Provider to determine if detail lines may be authorised for Provider */
  IF AVAILABLE ttAuthTypeconfig
  THEN DO:
    oAuthDetailLines = NEW cls.mipacronym(?, FALSE, "ma_acAuthTypeDetails":U, ?). 
        
    oAuthDetailLines:focusAcronym("KEY":U, ttAuthTypeConfig.AuthoriseDetailLines) NO-ERROR.
   
    IF  oAuthDetailLines:AcronymInFocus 
    AND oAuthDetailLines:AcronymLabel = "NONE":U THEN
    DO:    
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                        INPUT btt_auth_detail.auth_detail_obj, 
                                        INPUT "":U,
                                        INPUT btt_auth_detail.line_number, 
                                        INPUT "No details may be captured.  The Auth Type setup for this Provider does not allow it ":U,
                                        INPUT "ERR":U).       
    END.  /*ELSE: IF NOT oAuthDetailLines:AcronymInFocus THEN*/
  END.  /*IF AVAILABLE ttAuthTypeconfig*/  
 
  /*
    Get Auth Detail record
    MMP-481 buf_auth_detail removed in favor of using a temp table of abtt_auth_detail
  */                                      
  IF btt_auth_detail.auth_detail_obj > 0 THEN
  FIND abtt_auth_detail NO-LOCK
    WHERE abtt_auth_detail.auth_detail_obj = btt_auth_detail.auth_detail_obj NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
  
  IF AVAILABLE abtt_auth_detail
  AND abtt_auth_detail.auth_status <> btt_auth_detail.auth_status THEN
  DO:
    ASSIGN cErrorMessage = mipEnv:Health:AuthService:validateAuthStatusUpdate(INPUT  goAuthorisation:InsurerObj,
                                                                              INPUT  goAuthorisation:OptionCode,
                                                                              INPUT  abtt_auth_detail.auth_status,
                                                                              INPUT  btt_auth_detail.auth_status,
                                                                              INPUT  btt_auth_detail.amount_paid,
                                                                              INPUT  btt_auth_detail.quantity_paid,
                                                                              INPUT  btt_auth_detail.start_date).
    IF cErrorMessage <> "":U THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic */ 
                                      INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj      */
                                      INPUT "":U,                                                /* ipcOwningEntityKey      */
                                      INPUT "auth_status":U,                                     /* ipcFieldName            */
                                      INPUT btt_auth_detail.line_number,                         /* ipiLineNumber           */
                                      INPUT cErrorMessage,                                       /* ipcMessageText          */
                                      INPUT "ERR":U).                                            /* ipcMessageType          */
    END.  /* IF cErrorMessage <> "":U THEN */
  END.  /* IF AVAILABLE hat_auth_detail AND ... */

  /* Get associated Provider Auth Status if blank */
  /*
    MMP-481
    Removed direct DB call hat_auth_detail in favor of using the temp-table btt_auth_detail
  */
  
  IF btt_auth_detail.auth_status = ? THEN
  DO:
    FIND btt_auth_provider NO-LOCK
      WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }  

    IF AVAILABLE btt_auth_provider 
    THEN ASSIGN btt_auth_detail.auth_status = btt_auth_provider.auth_status.
  END.


  /* Check if the status reason/note on the auth detail is mandatory */ 
  IF btt_auth_detail.auth_status_note = "":U THEN
  DO:
    ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  btt_auth_detail.auth_status,
                                                                        INPUT  goAuthorisation:InsurerObj,     
                                                                        INPUT  goAuthorisation:MemberOptionCode,
                                                                        INPUT  btt_auth_detail.start_date).
    
    IF lMandatory THEN
    DO:
      
      /* Auth detail record ADDed */
      IF btt_auth_detail.auth_detail_obj <= 0 THEN 
      DO: /* MMP-481 Direct DB call removed in favor of abtt_auth_provider */
        FIND abtt_auth_provider NO-LOCK
          WHERE abtt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }   

        IF AVAILABLE abtt_auth_provider
        AND abtt_auth_provider.auth_status = btt_auth_detail.auth_status
        THEN ASSIGN btt_auth_detail.auth_status_note = abtt_auth_provider.auth_status_note.
      END.  /* IF btt_auth_detail.auth_detail_obj <= 0 THEN  */
      ELSE DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,                                 /* ipcOwningEntityMnemonic  */ 
                                        INPUT btt_auth_detail.auth_detail_obj,                                                     /* ipdOwningEntityObj       */  
                                        INPUT "":U,                                                                                /* ipcOwningEntityKey       */ 
                                        INPUT "auth_status_note":U,                                                                /* ipcFieldName             */ 
                                        INPUT btt_auth_detail.line_number,                                                         /* ipiLineNumber            */  
                                        INPUT "MA":U,                                                                              /* ipcMessageGroup          */ 
                                        INPUT 112,       /* The '&1' specified is invalid. &2  */                                  /* ipiMessageNumber         */ 
                                        INPUT "Status reason,A valid status reason must be provided for the status specified.":U). /* ipcReplaceTextList       */ 
      END.  /* ELSE:  IF btt_auth_detail.auth_detail_obj <= 0 THEN */
    END.  /* IF lMandatory THEN */
  END.  /* IF btt_auth_detail.auth_status_note = "" THEN */ 

  /* Validate status reason if specified */
  IF btt_auth_detail.auth_status_note <> "":U THEN
  DO: 
    mipEnv:Health:AuthService:ValidateStatusReason(INPUT  goAuthorisation:InsurerObj,
                                                   INPUT  00,
                                                   INPUT  btt_auth_detail.auth_status_note,
                                                   INPUT  INTEGER(btt_auth_detail.auth_status),
                                                   INPUT  btt_auth_detail.start_date,
                                                   OUTPUT cErrorMessage).
    IF cErrorMessage <> "":U THEN
    DO:
      goErrorObject:addError(INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,
                             INPUT btt_auth_detail.auth_detail_obj,
                             INPUT "":U,
                             INPUT "auth_status_note":U,
                             INPUT btt_auth_detail.line_number,
                             INPUT cErrorMessage,
                             INPUT "ERR":U).
    END. /* IF cErrorMessage <> "":U THEN  */
  END. /* IF btt_auth_detail.auth_status_note <> "":U */
  
  /*
    LOS is activated
  */
  IF AVAILABLE ttAuthTypeconfig
  AND ttAuthTypeConfig.ActivateLos THEN
  DO:
    /* Quantity-LOS */
    IF AVAILABLE abtt_auth_detail
    AND abtt_auth_detail.tariff_type_obj <> 0
    AND abtt_auth_detail.quantity_auth    > 0
    AND btt_auth_detail.quantity_los    <= 0 THEN 
    DO: 
      FIND FIRST htm_tariff_type NO-LOCK
        WHERE htm_tariff_type.tariff_type_obj = abtt_auth_detail.tariff_type_obj
          AND htm_tariff_type.acronym_key     = "ma_acTariffTypeCatLOC"
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }   

      IF AVAILABLE htm_tariff_type THEN
      DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,        /* ipcOwningEntityMnemonic */ 
                                        INPUT btt_auth_detail.auth_detail_obj,                          /* ipdOwningEntityObj      */ 
                                        INPUT "":U,                                                     /* ipcOwningEntityKey      */ 
                                        INPUT "quantity_los":U,                                         /* ipcFieldName            */ 
                                        INPUT btt_auth_detail.line_number,                              /* ipiLineNumber           */ 
                                        INPUT "MA":U,                                                   /* ipcMessageGroup         */ 
                                        INPUT 112,  /* The '&1' specified is invalid. &2  */            /* ipiMessageNumber        */ 
                                        INPUT "Quantity LOS: ":U + STRING(btt_auth_detail.quantity_los) + 
                                              ",Must be greater than 0.":U).                            /* ipcReplaceTextList      */ 
      END.  /* IF AVAILABLE htm_tariff_type... */
    END.  /* IF AVAILABLE hat_auth_detail... */

    /* Ars-Rate */
    IF btt_auth_detail.default_ars_rate <> "":U THEN
    DO:
      FIND FIRST arsrate NO-LOCK
        WHERE arsrate.ars-rate = btt_auth_detail.default_ars_rate NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }   

      IF NOT AVAILABLE arsrate THEN
      DO:                                        
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */ 
                                        INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */ 
                                        INPUT "":U,                                                    /* ipcOwningEntityKey      */ 
                                        INPUT "default_ars_rate":U,                                    /* ipcFieldName            */ 
                                        INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */ 
                                        INPUT "MA":U,                                                  /* ipcMessageGroup         */ 
                                        INPUT 100,  /* The '&1' specified is invalid */                /* ipiMessageNumber        */ 
                                        INPUT "ARS rate: ":U + STRING(btt_auth_detail.default_ars_rate)).      /* ipcReplaceTextList      */ 
      END.  /* IF NOT AVAILABLE arsrate THEN */
    END.  /* IF btt_auth_detail.default_ars_rate <> "" THEN */

    /* Base-Rate */
    IF btt_auth_detail.default_base_rate <> "":U THEN
    DO:
      /* Only check base rate for Tariff and Crosswalk */
      IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr") > 0 THEN
      DO:
        FIND FIRST baserate NO-LOCK
        WHERE baserate.base-rate = btt_auth_detail.default_base_rate NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }   

        IF NOT AVAILABLE baserate THEN
        DO:                                        
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */ 
                                          INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */ 
                                          INPUT "":U,                                                    /* ipcOwningEntityKey      */ 
                                          INPUT "default_base_rate":U,                                   /* ipcFieldName            */ 
                                          INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */ 
                                          INPUT "MA":U,                                                  /* ipcMessageGroup         */ 
                                          INPUT 100,  /* The '&1' specified is invalid */                /* ipiMessageNumber        */ 
                                          INPUT "Base rate: ":U + STRING(btt_auth_detail.default_base_rate)).    /* ipcReplaceTextList      */ 
        END.  /* IF NOT AVAILABLE baserate THEN */
      END.  /* IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr") > 0 THEN */
      ELSE 
        ASSIGN btt_auth_detail.default_base_rate = "".
    END.  /* IF btt_auth_detail.default_base_rate <> "" THEN */
    /* Ars-Rate */
    IF btt_auth_detail.override_ars_rate <> "":U THEN
    DO:
      FIND FIRST arsrate NO-LOCK
        WHERE arsrate.ars-rate = btt_auth_detail.override_ars_rate NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }   
      IF NOT AVAILABLE arsrate THEN
      DO:                                        
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */ 
                                        INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */ 
                                        INPUT "":U,                                                    /* ipcOwningEntityKey      */ 
                                        INPUT "override_ars_rate":U,                                   /* ipcFieldName            */ 
                                        INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */ 
                                        INPUT "MA":U,                                                  /* ipcMessageGroup         */ 
                                        INPUT 100,  /* The '&1' specified is invalid */                /* ipiMessageNumber        */ 
                                        INPUT "ARS rate: ":U + STRING(btt_auth_detail.override_ars_rate)).      /* ipcReplaceTextList      */ 
      END.  /* IF NOT AVAILABLE arsrate THEN */
    END.  /* IF btt_auth_detail.override_ars_rate <> "" THEN */
    /* Base-Rate */
    IF btt_auth_detail.override_base_rate <> "":U THEN
    DO:
      /* Only check base rate for Tariff and Crosswalk */
      IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr") > 0 THEN
      DO:
        FIND FIRST baserate NO-LOCK
        WHERE baserate.base-rate = btt_auth_detail.override_base_rate NO-ERROR.
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }   
        IF NOT AVAILABLE baserate THEN
        DO:                                        
          ASSIGN lSuccess = goErrorObject:addError
                                         (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */ 
                                          INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */ 
                                          INPUT "":U,                                                    /* ipcOwningEntityKey      */ 
                                          INPUT "override_base_rate":U,                                  /* ipcFieldName            */ 
                                          INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */ 
                                          INPUT "MA":U,                                                  /* ipcMessageGroup         */ 
                                          INPUT 100,  /* The '&1' specified is invalid */                /* ipiMessageNumber        */ 
                                          INPUT "Base rate: ":U + STRING(btt_auth_detail.override_base_rate)).    /* ipcReplaceTextList      */ 
        END.  /* IF NOT AVAILABLE baserate THEN */
      END.  /* IF LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmcr") > 0 THEN */
      ELSE 
        ASSIGN btt_auth_detail.override_base_rate = "".
    END.  /* IF btt_auth_detail.override_base_rate <> "" THEN */
  END.  /* IF AVAILABLE ttAuthTypeconfig AND ttAuthTypeConfig.ActivateLos THEN */

  /*
    Date validations against Auth header 
    MMP-481
    Removed hat_auth DB call in favor of temp-table btt_auth that is provided
  */
  FIND btt_auth NO-LOCK
       WHERE btt_auth.auth_obj = btt_auth_detail.auth_obj 
    NO-ERROR.
    
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
  
  FIND btt_auth_provider NO-LOCK
       WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj 
    NO-ERROR.
    
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
  
  /* 
    When we have switched to User Calc, 
    we do not want to trigger date validations
    until Final Save of the Auth
  */ 
  IF AVAILABLE btt_auth                        AND 
     AVAILABLE btt_auth_provider               AND 
    (btt_auth_provider.los_calculation = TRUE  OR 
    (btt_auth_provider.los_calculation = FALSE AND 
     btt_auth._final_save              = TRUE)) THEN
  DO:
    IF btt_auth_detail.record_action = "MODIFY":U AND btt_auth_detail.start_date < btt_auth.start_date THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "start_date":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "ma_MsgAuth":U, 
                                      INPUT 24,  /* The &1 Date &2 cannot be before the &3 Date &4 */  
                                      INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") + 
                                           ",Authorisation Header Start,":U + STRING(btt_auth.start_date, "9999/99/99")).                  
    END. /*IF btt_auth_detail.start_date < buf_auth.start_date THEN*/ 

    IF btt_auth_detail.record_action = "MODIFY":U AND btt_auth_detail.start_date > btt_auth.end_date THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "start_date":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "ma_MsgAuth":U, 
                                      INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */  
                                      INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") + 
                                           ",Authorisation Header End,":U + STRING(btt_auth.end_date, "9999/99/99")).                  
    END. /*IF btt_auth_detail.start_date > buf_auth.end_date THEN*/ 

    IF btt_auth_detail.record_action = "MODIFY":U AND  
       btt_auth_detail.end_date     <> ?          AND 
       btt_auth.end_date            <> ?          AND 
       btt_auth_detail.end_date      > btt_auth.end_date 
    THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "end_date":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "ma_MsgAuth":U, 
                                      INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */   
                                      INPUT "Authorisation Detail End,":U + STRING(btt_auth_detail.end_date, "9999/99/99") + 
                                           ",Authorisation Header End,":U + STRING(btt_auth.end_date, "9999/99/99")).                  
    END. /*IF btt_auth_detail.end_date <> ? AND buf_auth.end_date <> ?*/ 
  END. /*IF AVAILABLE buf_auth THEN*/
  
  /*
    When we have switched to User Calc, 
    we do not want to trigger date validations
    until Final Save of the Auth
  */
  IF AVAILABLE btt_auth                        AND 
     AVAILABLE btt_auth_provider               AND 
    (btt_auth_provider.los_calculation = TRUE  OR 
    (btt_auth_provider.los_calculation = FALSE AND 
     btt_auth._final_save              = TRUE)) THEN
  DO:
    IF btt_auth_detail.record_action = "MODIFY":U AND btt_auth_detail.start_date < btt_auth_provider.start_date THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "start_date":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "ma_MsgAuth":U, 
                                      INPUT 24,  /* The &1 Date &2 cannot be before the &3 Date &4 */  
                                      INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") + 
                                           ",Authorisation Provider Start,":U + STRING(btt_auth_provider.start_date, "9999/99/99")).                  
    END. /*IF btt_auth_detail.start_date < buf_auth_provider.start_date THEN*/ 

    IF btt_auth_detail.record_action = "MODIFY":U AND btt_auth_detail.start_date > btt_auth_provider.end_date THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "start_date":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "ma_MsgAuth":U, 
                                      INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */  
                                      INPUT "Authorisation Detail Start,":U + STRING(btt_auth_detail.start_date, "9999/99/99") + 
                                           ",Authorisation Provider End,":U + STRING(btt_auth_provider.end_date, "9999/99/99")).                  
    END. /*IF btt_auth_detail.start_date > buf_auth_provider.end_date THEN*/ 
    
    IF btt_auth_detail.record_action = "MODIFY":U AND 
       btt_auth_detail.end_date     <> ?          AND 
       btt_auth_provider.end_date   <> ?          AND 
       btt_auth_detail.end_date      > btt_auth_provider.end_date 
    THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "end_date":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "ma_MsgAuth":U, 
                                      INPUT 25,  /* The &1 Date &2 cannot be after the &3 Date &4 */   
                                      INPUT "Authorisation Detail End,":U + STRING(btt_auth_detail.end_date, "9999/99/99") + 
                                           ",Authorisation Provider End,":U + STRING(btt_auth_provider.end_date, "9999/99/99")).                  
    END. /*IF btt_auth_detail.end_date <> ? AND buf_auth_provider.end_date <> ? THEN*/ 
    
    /* 
      User LOS Calculation
      Validations to ensure data manually captured is within variance
    */ 
    IF btt_auth_provider.los_calculation = FALSE 
    AND btt_auth_detail.loc_sequence <> 0 
    THEN DO:
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                     INPUT  0,
                                                     INPUT  "ma_acAuthRuleTypeLOC&LOS":U,
                                                     INPUT  "UserCalcLOSVariance":U,
                                                     INPUT  btt_auth.start_date,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).
                                                     
      ASSIGN dLOSVariance    = 0
             dLOSCalculation = 0. 
      
      IF lValidRule AND NUM-ENTRIES(cRuleValue,"|") > 1
      THEN DO: 
        ASSIGN cAction         = ENTRY(2,cRuleValue,"|")
               dLOSVariance    = DECIMAL(ENTRY(1,cRuleValue,"|")). 
               dLOSCalculation = btt_auth_detail.end_date - btt_auth_detail.start_date 
                               + (IF btt_auth_detail.start_ampm = btt_auth_detail.end_ampm OR 
                                    (btt_auth_detail.end_date - btt_auth_detail.start_date = 0)
                                  THEN 0.5
                                  ELSE 0). 
                                  
        /* 
          We use the calculated LOS and compare it to the actual LOS entered. 
          If the difference if more than the variance allowed, as setup on the rule, 
          we raise an error/warning
        */
        IF ABSOLUTE(btt_auth_detail.quantity_los - dLOSCalculation) > dLOSVariance 
        THEN DO:              
          IF CAN-DO("WARN,WARNACK,BLOCK",cAction) 
          THEN DO:
            ASSIGN
              lAcknowledge  = (IF cAction = "WARNACK":U
                               THEN TRUE
                               ELSE FALSE)

              cErrorMessage = "Manually updated Start/End Date AM/PM exceeds variance allowed for Tariff " + btt_auth_detail.owning_alt_value + " LOC Seq " + STRING(btt_auth_detail.loc_sequence,">>9")

              cErrorType    = IF LOOKUP(cAction,"WARN,WARNACK":U) > 0
                              THEN "WAR":U
                              ELSE "ERR":U.

              goErrorObject:addError (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT "":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT cErrorMessage,
                                      INPUT cErrorType,
                                      INPUT lAcknowledge).
                      
          END. /* IF CAN-DO("WARN,WARNACK,BLOCK",cAction) */
        END. /* IF ABSOLUTE(btt_auth_detail.quantity_los - dLOSCalculation) > dLOSVariance */
      END. /* IF lValidRule AND NUM-ENTRIES(cRuleValue,"|") > 1*/                            
    END. /* IF btt_auth_provider.los_calculation = FALSE */ 
       
  END. /*IF AVAILABLE buf_auth_provider THEN*/
             
  /* Added_by_user */
  IF btt_auth_detail.added_by_user = ? THEN
    ASSIGN btt_auth_detail.added_by_user = TRUE.

  /*MMP-591*/
  IF AVAILABLE ttAuthTypeConfig THEN
  DO: 
    IF ttAuthTypeConfig.ClaimCodesDetail = ? THEN
      ASSIGN ttAuthTypeConfig.ClaimCodesDetail = "0".

    IF btt_auth_detail.claim_code = 0 OR btt_auth_detail.claim_code = ? THEN
      ASSIGN btt_auth_detail.claim_code = INTEGER(ttAuthTypeConfig.ClaimCodesDetail).

    IF TRIM(btt_auth_detail.claim_type) = "":U OR btt_auth_detail.claim_type = ?  THEN
      ASSIGN btt_auth_detail.claim_type = ttAuthTypeConfig.ClaimTypesDetail .
 
    IF btt_auth_detail.added_by_user = TRUE THEN
    DO:
      IF ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE 
      AND btt_auth_detail.claim_code <> INTEGER(ttAuthTypeConfig.ClaimCodesDetail) THEN
      DO:
        ASSIGN cErrorType    = "ERR":U
               lAcknowledge  =  TRUE
               cErrorMessage = "User update of detail claim code for detail line " + btt_auth_detail.owning_alt_value + " is not allowed":U.
     
        goErrorObject:addError (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT "":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT cErrorMessage,
                                INPUT cErrorType,
                                INPUT lAcknowledge). 
      END. /*IF ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE AND btt_auth_detail.claim_code <> ttAuthTypeConfig.ClaimCodesDetail THEN*/

      IF ttAuthTypeConfig.ClaimTypeUpdAllowed = FALSE 
      AND btt_auth_detail.claim_type <> ttAuthTypeConfig.ClaimTypesDetail THEN
      DO:
        ASSIGN cErrorType    = "ERR":U
               lAcknowledge  =  TRUE
               cErrorMessage = "User update of detail claim type for detail line " + btt_auth_detail.owning_alt_value  + " is not allowed":U.

        goErrorObject:addError (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                INPUT btt_auth_detail.auth_detail_obj,
                                INPUT "":U,
                                INPUT "":U,
                                INPUT btt_auth_detail.line_number,
                                INPUT cErrorMessage,
                                INPUT cErrorType,
                                INPUT lAcknowledge). 
      END. /*IF btt_auth_detail.claim_type <> ttAuthTypeConfig.ClaimtypesDetail THEN*/
    END. /*IF btt_auth_detail.added_by_user = TRUE THEN*/
    ELSE
    DO: 
     IF btt_auth_detail.auth_detail_obj > 0 THEN DO:
       oDetail:focusRecord(btt_auth_detail.auth_detail_obj) .

       IF oDetail:Infocus THEN 
       DO:
         IF ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE 
         AND btt_auth_detail.claim_code <> oDetail:ClaimCode THEN 
         DO: 
           ASSIGN cErrorType    = "ERR":U
                  lAcknowledge  =  TRUE
                  cErrorMessage = "User update of detail claim code for detail line " + btt_auth_detail.owning_alt_value + " is not allowed":U.
         
           goErrorObject:addError (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                   INPUT btt_auth_detail.auth_detail_obj,
                                   INPUT "":U,
                                   INPUT "":U,
                                   INPUT btt_auth_detail.line_number,
                                   INPUT cErrorMessage,
                                   INPUT cErrorType,
                                   INPUT lAcknowledge). 
         END. /*IF ttAuthTypeConfig.ClaimCodeUpdAllowed = FALSE AND btt_auth_detail.claim_code <> oDetail:ClaimCode THEN DO:*/
         
         IF ttAuthTypeConfig.ClaimTypeUpdAllowed = FALSE 
         AND btt_auth_detail.claim_type <> oDetail:ClaimType THEN 
         DO: 
           ASSIGN cErrorType    = "ERR":U
                  lAcknowledge  =  TRUE
                  cErrorMessage = "User update of detail claim type for detail line " + btt_auth_detail.owning_alt_value + " is not allowed":U.
         
           goErrorObject:addError (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,
                                   INPUT btt_auth_detail.auth_detail_obj,
                                   INPUT "":U,
                                   INPUT "":U,
                                   INPUT btt_auth_detail.line_number,
                                   INPUT cErrorMessage,
                                   INPUT cErrorType,
                                   INPUT lAcknowledge). 
         END. /*IF ttAuthTypeConfig.ClaimTypeUpdAllowed = FALSE AND btt_auth_detail.claim_type <> oDetail:ClaimType THEN*/
       END. /*IF oDetail:Infocus THEN DO:*/
     END. /*IF btt_auth_detail.auth_detail_obj > 0 THEN DO*/
    END. /*when tt_auth_detail.added_by_user = no */ 
  END. /*IF AVAILABLE ttAuthTypeConfig THEN */

  /* Discount-Type and Discount-Auth */
  IF btt_auth_detail.record_action  = "MODIFY":U AND 
     btt_auth_detail.discount_type <> ?          AND 
     btt_auth_detail.discount_auth  = 0 
  THEN
  DO:
    ASSIGN lSuccess = goErrorObject:addError                                                          
                                   (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */ 
                                    INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */ 
                                    INPUT "":U,                                                          /* ipcOwningEntityKey      */ 
                                    INPUT "discount_auth":U,                                             /* ipcFieldName            */ 
                                    INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */ 
                                    INPUT "MA":U,                                                        /* ipcMessageGroup         */ 
                                    INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */ 
                                    INPUT "Discount Auth: ":U + STRING(btt_auth_detail.discount_auth) +  /* ipcReplaceTextList      */ 
                                          ",Please enter a value":U).  
  END.  /* IF  btt_auth_detail.discount_type <> ? AND btt_auth_detail.discount_auth  = 0 THEN */
               
  /* Validate claim code if specified */
  IF btt_auth_detail.claim_code <> 0 THEN
  DO:
    FIND FIRST btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    ASSIGN lSuccess = mipEnv:Health:AuthBusinessLogic:validateClaimCode(INPUT  goAuthorisation:InsurerObj,
                                                                        INPUT  goAuthorisation:OptionCode,    
                                                                        INPUT  goAuthorisation:MemNum,
                                                                        INPUT  goAuthorisation:Dependant,
                                                                        INPUT  btt_auth_detail.claim_code,     
                                                                        INPUT  goAuthorisation:AuthTypeObj,   
                                                                        INPUT  btt_auth_detail.start_date,
                                                                        INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.provider_type ELSE "",                              /* Provider Type */
                                                                        INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.pr_type ELSE 0,                                    /* Discipline */
                                                                        INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.sub_pr_type ELSE 0,                                /* Sub-Discipline */
                                                                        OUTPUT lValidationError,
                                                                        OUTPUT cErrorMessage).
    IF cErrorMessage <> "" THEN
    DO:
      ASSIGN cMessageType = (IF lValidationError = TRUE
                             THEN "WAR":U
                             ELSE "ERR":U)
                             
             lSuccess = goErrorObject:addError
                            (INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth_detail.auth_detail_obj,                    /* ipdOwningEntityObj       */
                             INPUT "":U,                                               /* ipcOwningEntityKey       */
                             INPUT "claim_code":U,                                     /* ipcFieldName             */
                             INPUT btt_auth_detail.line_number,                        /* ipiLineNumber            */
                             INPUT cErrorMessage,                                      /* ipcMessageText           */
                             INPUT cMessageType).                                      /* ipcMessageType           */
    END. /* IF cErrorMessage <> "" THEN */
  END.  /* IF btt_auth_detail.claim_code <> 0 THEN */
  
  /* Validate claim type if specified */
  IF btt_auth_detail.claim_type <> "":U THEN
  DO:
    FIND FIRST btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj NO-ERROR.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    ASSIGN lSuccess = mipEnv:Health:AuthBusinessLogic:validateClaimType(INPUT  goAuthorisation:InsurerObj,
                                                                        INPUT  goAuthorisation:OptionCode,    
                                                                        INPUT  btt_auth_detail.claim_type,     
                                                                        INPUT  goAuthorisation:AuthTypeObj,   
                                                                        INPUT  btt_auth_detail.start_date,      
                                                                        INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.provider_type ELSE "",                              /* Provider Type */ 
                                                                        INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.pr_type       ELSE 0,                               /* Discipline */    
                                                                        INPUT  IF AVAILABLE btt_auth_provider THEN btt_auth_provider.sub_pr_type   ELSE 0,                               /* Sub-Discipline */
                                                                        OUTPUT lValidationError,
                                                                        OUTPUT cErrorMessage).
    IF cErrorMessage <> "" THEN
    DO:
      ASSIGN cMessageType = (IF lValidationError = TRUE
                             THEN "WAR":U
                             ELSE "ERR":U)
                             
             lSuccess = goErrorObject:addError
                                     (INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,  /* ipcOwningEntityMnemonic  */
                                      INPUT btt_auth_detail.auth_detail_obj,                     /* ipdOwningEntityObj       */
                                      INPUT "":U,                                                /* ipcOwningEntityKey       */
                                      INPUT "claim_type":U,                                      /* ipcFieldName             */
                                      INPUT btt_auth_detail.line_number,                         /* ipiLineNumber            */
                                      INPUT cErrorMessage,                                       /* ipcMessageText           */
                                      INPUT cMessageType).                                       /* ipcMessageType           */
    END. /* IF cErrorMessage <> "" THEN */
  END.  /* IF btt_auth_detail.claim_type <> "":U THEN */
 
  /*
    Line Restriction Detail Validation 
  */
  IF btt_auth_detail.line_restriction = "":U THEN
  DO:                          
    ASSIGN lSuccess = goErrorObject:addError
                                   (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                                    INPUT btt_auth_detail.auth_detail_obj, 
                                    INPUT "":U, 
                                    INPUT "line_restriction":U,
                                    INPUT btt_auth_detail.line_number, 
                                    INPUT "MA":U, 
                                    INPUT 110,  /* "&1" needs to be filled in */   
                                    INPUT "Authorisation Line Restriction ":U).        
  END. /*IF btt_auth_detail.line_restriction <> "":U THEN*/  

  /*
    Validate Start- and End AMPM if ActivateAmPm is set
  */
  IF AVAILABLE ttAuthTypeConfig
  AND ttAuthTypeConfig.ActivateAmPm THEN 
  DO:
    IF  btt_auth_detail.start_ampm  = ? 
    OR (btt_auth_detail.start_ampm <> ? 
    AND LOOKUP(STRING(btt_auth_detail.start_ampm,"AM/PM"),"AM,PM") = 0) THEN
    DO:                            
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "start_ampm":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The &1 specified is invalid */ 
                                      INPUT "Authorisation Detail Start AMPM ":U).  
    END. /*IF (glActiveAmpm AND btt_auth_detail.start_ampm = ?) OR (btt_auth_detail.start_ampm <> ? AND LOOKUP(STRING(btt_auth_detail.start_ampm),"AM,PM") = 0) THEN*/ 

    IF  btt_auth_detail.end_ampm = ? 
    OR (btt_auth_detail.end_ampm <> ? 
    AND LOOKUP(STRING(btt_auth_detail.end_ampm,"AM/PM"),"AM,PM") = 0) THEN
    DO:    
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U, 
                                      INPUT "end_ampm":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "MA":U, 
                                      INPUT 100,  /* The &1 specified is invalid */ 
                                      INPUT "Authorisation Detail End AMPM ":U).
    END. /*IF (glActiveAmpm AND btt_auth_detail.end_ampm = ?) OR (btt_auth_detail.end_ampm <> ? AND LOOKUP(STRING(btt_auth_detail.end_ampm),"AM,PM") = 0) THEN*/               
  END.  /*IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.ActivateAmPm THEN*/
  
  /*
    Validate End Time againd Start Time
  */
  IF btt_auth_detail.start_time > btt_auth_detail.end_time THEN
  DO:
    ASSIGN lSuccess = goErrorObject:addError
                                   (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic, 
                                    INPUT btt_auth_detail.auth_detail_obj, 
                                    INPUT "":U, 
                                    INPUT "end_time":U,
                                    INPUT btt_auth_detail.line_number, 
                                    INPUT "MA":U, 
                                    INPUT 112,  /* The &1 specified is invalid. &2 */ 
                                    INPUT "Authorisation Detail End Time: ":U + STRING(btt_auth_detail.end_time) +
                                          ",Can't be smaller than the Start Time: ":U + STRING(btt_auth_detail.start_time)).
  END. /*IF btt_auth_detail.start_time > btt_auth_detail.end_time THEN*/               

  /* Validate note if specified */
  IF btt_auth_detail.note <> "":U THEN
  DO:
    FIND FIRST note NO-LOCK
      WHERE note.scheme-code = goAuthorisation:OptionCode
        AND note.TYPE        = "AN"
        AND note.KEY         = btt_auth_detail.note
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }   

    IF NOT AVAILABLE note THEN
      FIND FIRST note NO-LOCK
        WHERE note.TYPE = "AN"
          AND note.KEY  = btt_auth_detail.note
        NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }   

    IF NOT AVAILABLE note THEN
    DO:                                     
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,      /* ipcOwningEntityMnemonic */ 
                                      INPUT btt_auth_detail.auth_detail_obj,                        /* ipdOwningEntityObj      */ 
                                      INPUT "":U,                                                   /* ipcOwningEntityKey      */ 
                                      INPUT "note":U,                                               /* ipcFieldName            */ 
                                      INPUT btt_auth_detail.line_number,                            /* ipiLineNumber           */ 
                                      INPUT "MA":U,                                                 /* ipcMessageGroup         */ 
                                      INPUT 100,  /* The '&1' specified is invalid */               /* ipiMessageNumber        */ 
                                      INPUT "The Note Code: ":U + STRING(btt_auth_detail.note)).    /* ipcReplaceTextList      */ 
    END. /* IF cErrorMessage <> "":U THEN  */
  END.  /* IF btt_auth_detail.note <> "":U THEN */

  /* Validate body region - must be same as Auth's body region.  If 'both' on Auth header, any value allowed on detail
     The validation against the Acronym is done on Data level */
  IF btt_auth_detail.body_region <> "":U THEN 
  DO:
     IF  btt_auth_detail.body_region <> goAuthorisation:BodyRegion
     AND goAuthorisation:BodyRegion  <> ""
     AND goAuthorisation:BodyRegion  <> "ma_acAuthBodyRegionBoth" THEN
     DO:
       ASSIGN oBodyRegion = NEW cls.mipacronym(?,FALSE, "ma_acAuthBodyRegion":U, ?).
       
       oBodyRegion:focusAcronym("KEY":U, btt_auth_detail.body_region) NO-ERROR.

       IF oBodyRegion:AcronymInFocus THEN
         ASSIGN cErrorMessage = SUBSTITUTE("Body Region &1 specified is invalid. Must be the same as the Auth Header if that is not 'Both'",
                                            oBodyRegion:AcronymLabel).

       ASSIGN lSuccess = goErrorObject:addError
                                      (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */ 
                                       INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */ 
                                       INPUT "":U,                                                          /* ipcOwningEntityKey      */ 
                                       INPUT "body_region":U,                                               /* ipcFieldName            */ 
                                       INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */ 
                                       INPUT cErrorMessage,                                                 /* ipcMessageTest          */                                                           
                                       INPUT "ERR":U).                                                      /* ipcMessageType          */ 
     END.  /* IF  btt_auth_detail.body-region <> goAuthorisation:BodyRegion */
  END.  /* IF btt_auth_detail.body_region <> "":U THEN */

    
  /* Repeats - only allowed for Tariff/Nappi details */
  IF btt_auth_detail.repeats 
  AND LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmnl") = 0 THEN
  DO:
    ASSIGN lSuccess = goErrorObject:addError
                                   (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */ 
                                    INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */ 
                                    INPUT "":U,                                                    /* ipcOwningEntityKey      */ 
                                    INPUT "repeats":U,                                             /* ipcFieldName            */ 
                                    INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */ 
                                    INPUT "MA":U,                                                  /* ipcMessageGroup         */ 
                                    INPUT 112,  /* The '&1' specified is invalid. &2 */            /* ipiMessageNumber        */ 
                                    INPUT "Repeats,NOT allowed for this entity":U).                /* ipcReplaceTextList      */ 
  END.  /* IF btt_auth_detail.repeats AND LOOKUP(btt_auth_detail.owning_entity_mnemonic,"htmtl,hlmnl") = 0 THEN */
  
  /* Check if Nappi's required for Tariff */
  ASSIGN lNappiRequired = FALSE.

  IF btt_auth_detail.owning_entity_mnemonic = "htmtl" THEN
  DO:
    FOR FIRST htm_tariff_link NO-LOCK                      
        WHERE htm_tariff_link.tariff_link_obj = btt_auth_detail.owning_obj,     
        FIRST tariff NO-LOCK
        WHERE tariff.tariff-link-obj = htm_tariff_link.tariff_link_obj:

      IF tariff.nappi-required-auths = "ma_acNappiRequiredEnforce" 
      THEN ASSIGN lNappiRequired = TRUE.  
    END. /*FOR FIRST htm_tariff_link NO-LOCK*/  
  END.  /*IF btt_auth_detail.owning_entity_mnemonic = "htmtl" THEN*/

  IF  btt_auth_detail.related_entity_mnemonic = "hlmnl" THEN
  DO:
    /* Check if Nappi is valid */
    IF  btt_auth_detail.related_value <> "" 
    AND btt_auth_detail.related_value <> "0" THEN
    DO:
      FIND hlm_nappi_link NO-LOCK                      
          WHERE hlm_nappi_link.nappi_link_obj =  btt_auth_detail.related_obj NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }  

      IF NOT AVAILABLE hlm_nappi_link THEN
        FIND hlm_nappi_link NO-LOCK                      
          WHERE hlm_nappi_link.nappi_code_prefix = INTEGER(btt_auth_detail.related_value) NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }  

      IF NOT AVAILABLE hlm_nappi_link THEN
      DO:                                        
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */ 
                                        INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */ 
                                        INPUT "":U,                                                    /* ipcOwningEntityKey      */ 
                                        INPUT "related_value":U,                                       /* ipcFieldName            */ 
                                        INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */ 
                                        INPUT "MA":U,                                                  /* ipcMessageGroup         */ 
                                        INPUT 100,  /* The '&1' specified is invalid. */               /* ipiMessageNumber        */ 
                                        INPUT "Nappi Code":U).                                         /* ipcReplaceTextList      */    
      END.  /*IF NOT AVAILABLE hlm_nappi_link THEN*/
    END.  /*IF btt_auth_detail.related_value <> "" THEN*/
   
    /* Nappi Link = hlmnl Although related_value might be empty, the related_obj might not be */
    IF  btt_auth_detail.related_obj <> ? 
    AND btt_auth_detail.related_obj <> 0 THEN
    DO:
      FIND hlm_nappi_link NO-LOCK                      
          WHERE hlm_nappi_link.nappi_link_obj =  btt_auth_detail.related_obj NO-ERROR.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }  
      
      IF AVAILABLE hlm_nappi_link 
      THEN ASSIGN btt_auth_detail.related_value = string(hlm_nappi_link.nappi_code_prefix).
      ELSE DO:
        ASSIGN lSuccess = goErrorObject:addError
                                       (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,       /* ipcOwningEntityMnemonic */ 
                                        INPUT btt_auth_detail.auth_detail_obj,                         /* ipdOwningEntityObj      */ 
                                        INPUT "":U,                                                    /* ipcOwningEntityKey      */ 
                                        INPUT "related_value":U,                                       /* ipcFieldName            */ 
                                        INPUT btt_auth_detail.line_number,                             /* ipiLineNumber           */ 
                                        INPUT "MA":U,                                                  /* ipcMessageGroup         */ 
                                        INPUT 100,  /* The '&1' specified is invalid. */               /* ipiMessageNumber        */ 
                                        INPUT "Nappi Code":U).                                         /* ipcReplaceTextList      */  
      END.
    END.
    ELSE IF btt_auth_detail.record_action = "MODIFY":U AND lNappiRequired THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U,  
                                      INPUT "related_value":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "The Nappi Code must be specified":U,
                                      INPUT "ERR":U).      
    END. /* ELSE IF btt_auth_detail.record_action = "MODIFY":U AND lNappiRequired THEN */

    IF (btt_auth_detail.related_value = "" 
    OR  btt_auth_detail.related_value = "0")
    AND lNappiRequired THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                      INPUT btt_auth_detail.auth_detail_obj, 
                                      INPUT "":U,  
                                      INPUT "related_value":U,
                                      INPUT btt_auth_detail.line_number, 
                                      INPUT "The Nappi Code must be specified":U,
                                      INPUT "WAR":U).       
    END.  /*IF (btt_auth_detail.related_value = "" ... */
  END.  /*IF  btt_auth_detail.related_entity_mnemonic = "hlmnl" THEN*/

  IF  btt_auth_detail.related_entity_mnemonic <> ""
  AND btt_auth_detail.related_entity_mnemonic <> "hlmnl" THEN
  DO:
    ASSIGN lSuccess = goErrorObject:addError
                                   (INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  
                                    INPUT btt_auth_detail.auth_detail_obj, 
                                    INPUT "":U,  
                                    INPUT "related_entity_mnemonic":U,
                                    INPUT btt_auth_detail.line_number, 
                                    INPUT "Only a Nappi Code can be linked to this entity":U,
                                    INPUT "ERR":U).       
  END.  /*IF  btt_auth_detail.related_entity_mnemonic <> "" AND btt_auth_detail.related_entity_mnemonic <> "hlmnl" THEN*/
  
  /* Amount/Quantity paid validations */
  IF btt_auth_detail.amount_paid <> 0.00 OR btt_auth_detail.quantity_paid <> 0 THEN
  DO:
    /* Claim Code */
    IF AVAILABLE abtt_auth_detail AND abtt_auth_detail.claim_code <> btt_auth_detail.claim_code THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError                                                          
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */ 
                                      INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */ 
                                      INPUT "":U,                                                          /* ipcOwningEntityKey      */ 
                                      INPUT "claim_code":U,                                                /* ipcFieldName            */ 
                                      INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */ 
                                      INPUT "MA":U,                                                        /* ipcMessageGroup         */ 
                                      INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */ 
                                      INPUT "Claim Code: ":U + STRING(btt_auth_detail.claim_code) +        /* ipcReplaceTextList      */ 
                                            ",No changes allowed after detail line was paid.":U).  
    END.  /* IF AVAILABLE hat_auth_detail AND hat_auth_detail.claim_code <> btt_auth_detail.claim_code THEN */

    /* Claim Type */
    IF AVAILABLE abtt_auth_detail AND abtt_auth_detail.claim_type <> btt_auth_detail.claim_type THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError                                                          
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */ 
                                      INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */ 
                                      INPUT "":U,                                                          /* ipcOwningEntityKey      */ 
                                      INPUT "claim_type":U,                                                /* ipcFieldName            */ 
                                      INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */ 
                                      INPUT "MA":U,                                                        /* ipcMessageGroup         */ 
                                      INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */ 
                                      INPUT "Claim Type: ":U + STRING(btt_auth_detail.claim_type) +        /* ipcReplaceTextList      */ 
                                            ",No changes allowed after detail line was paid.":U).  
    END.  /* IF AVAILABLE hat_auth_detail AND hat_auth_detail.claim_code <> btt_auth_detail.claim_code THEN */

    /* Benefit% */
    IF AVAILABLE abtt_auth_detail AND abtt_auth_detail.benefit_% <> btt_auth_detail.benefit_% THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError                                                          
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */ 
                                      INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */ 
                                      INPUT "":U,                                                          /* ipcOwningEntityKey      */ 
                                      INPUT "benefit_%":U,                                                 /* ipcFieldName            */ 
                                      INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */ 
                                      INPUT "MA":U,                                                        /* ipcMessageGroup         */ 
                                      INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */ 
                                      INPUT "Benefit%: ":U + STRING(btt_auth_detail.benefit_%) +           /* ipcReplaceTextList      */ 
                                            ",No changes allowed after detail line was paid.":U).  
    END.  /* IF AVAILABLE hat_auth_detail AND hat_auth_detail.benefit_% <> btt_auth_detail.benefit_% THEN */

    /* PMB Benefit% */
    IF AVAILABLE abtt_auth_detail AND abtt_auth_detail.pmb_benefit_% <> btt_auth_detail.pmb_benefit_% THEN
    DO:
      ASSIGN lSuccess = goErrorObject:addError                                                          
                                     (INPUT 'hatad:' + btt_auth_detail.owning_entity_mnemonic,             /* ipcOwningEntityMnemonic */ 
                                      INPUT btt_auth_detail.auth_detail_obj,                               /* ipdOwningEntityObj      */ 
                                      INPUT "":U,                                                          /* ipcOwningEntityKey      */ 
                                      INPUT "pmb_benefit_%":U,                                             /* ipcFieldName            */ 
                                      INPUT btt_auth_detail.line_number,                                   /* ipiLineNumber           */ 
                                      INPUT "MA":U,                                                        /* ipcMessageGroup         */ 
                                      INPUT 112,  /* The '&1' specified is invalid.  &2 */                 /* ipiMessageNumber        */ 
                                      INPUT "PMB Benefit%: ":U + STRING(btt_auth_detail.pmb_benefit_%) +   /* ipcReplaceTextList      */ 
                                            ",No changes allowed after detail line was paid.":U).  
    END.  /* IF AVAILABLE hat_auth_detail AND hat_auth_detail.pmb_benefit_% <> btt_auth_detail.pmb_benefit_% THEN */

    /* Amount auth/paid validation */
    IF btt_auth_detail.amount_paid <> 0.00 AND btt_auth_detail.amount_auth < btt_auth_detail.amount_paid THEN 
    DO:
      ASSIGN lSuccess = goErrorObject:addError
                                     (INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,
                                      INPUT btt_auth_detail.auth_detail_obj,
                                      INPUT "":U,
                                      INPUT "amount_paid":U,
                                      INPUT btt_auth_detail.line_number,
                                      INPUT "ma_MsgAuth":U,
                                      INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                                      INPUT "Amount Paid," + STRING(btt_auth_detail.amount_paid) + ",Amount Authorised,":U + STRING(btt_auth_detail.amount_auth)).
    END. /*IF btt_auth_detail.amount_paid <> 0.00 AND btt_auth_detail.amount_auth < btt_auth_detail.amount_paid THEN */

    /* Quantity auth/paid validation */
    IF btt_auth_detail.quantity_paid <> 0.00 AND btt_auth_detail.quantity_auth < btt_auth_detail.quantity_paid THEN 
    DO:
      goErrorObject:addError(INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,
                             INPUT btt_auth_detail.auth_detail_obj,
                             INPUT "":U,
                             INPUT "quantity_paid":U,
                             INPUT btt_auth_detail.line_number,
                             INPUT "ma_MsgAuth":U,
                             INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                             INPUT "Quantity Paid," + STRING(btt_auth_detail.quantity_paid) + ",Quantity Authorised,":U + STRING(btt_auth_detail.quantity_auth)).
    END. /*IF btt_auth_detail.quantity_paid <> 0.00 AND btt_auth_detail.quantity_auth < btt_auth_detail.quantity_paid THEN */

    /* Discount auth/paid validation */
    IF btt_auth_detail.discount_paid <> 0.00 AND btt_auth_detail.discount_auth < btt_auth_detail.discount_paid THEN 
    DO:
      goErrorObject:addError(INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,
                             INPUT btt_auth_detail.auth_detail_obj,
                             INPUT "":U,
                             INPUT "discount_paid":U,
                             INPUT btt_auth_detail.line_number,
                             INPUT "ma_MsgAuth":U,
                             INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                             INPUT "Discount Paid," + STRING(btt_auth_detail.discount_paid) + ",Discount Authorised,":U + STRING(btt_auth_detail.discount_auth)).
    END. /*IF btt_auth_detail.discount_paid <> 0.00 AND btt_auth_detail.discount_auth < btt_auth_detail.discount_paid THEN */

    /* Co-Pay auth/paid validation */
    IF btt_auth_detail.copay_paid <> 0.00 AND btt_auth_detail.copay_auth < btt_auth_detail.copay_paid THEN 
    DO:
      goErrorObject:addError(INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,
                             INPUT btt_auth_detail.auth_detail_obj,
                             INPUT "":U,
                             INPUT "copay_paid":U,
                             INPUT btt_auth_detail.line_number,
                             INPUT "ma_MsgAuth":U,
                             INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                             INPUT "Co-Pay Paid," + STRING(btt_auth_detail.copay_paid) + ",Co-Pay Authorised,":U + STRING(btt_auth_detail.copay_auth)).
    END. /*IF btt_auth_detail.copay_paid <> 0.00 AND btt_auth_detail.copay_auth < btt_auth_detail.copay_paid THEN */

    /* Adjustment auth/paid validation */
    IF btt_auth_detail.adjustment_paid <> 0.00 AND btt_auth_detail.adjustment_auth < btt_auth_detail.adjustment_paid THEN 
    DO:
      goErrorObject:addError(INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,
                             INPUT btt_auth_detail.auth_detail_obj,
                             INPUT "":U,
                             INPUT "adjustment_paid":U,
                             INPUT btt_auth_detail.line_number,
                             INPUT "ma_MsgAuth":U,
                             INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                             INPUT "Adjustment Paid," + STRING(btt_auth_detail.adjustment_paid) + ",Adjustment Authorised,":U + STRING(btt_auth_detail.adjustment_auth)).
    END. /*IF btt_auth_detail.adjustment_paid <> 0.00 AND btt_auth_detail.adjustment_auth < btt_auth_detail.adjustment_paid THEN */

    /* Adjustment Private auth/paid validation */
    IF btt_auth_detail.adjustment_private_paid <> 0.00 AND btt_auth_detail.adjustment_private_auth < btt_auth_detail.adjustment_private_paid THEN 
    DO:
      goErrorObject:addError(INPUT "hatad":U + btt_auth_detail.owning_entity_mnemonic,
                             INPUT btt_auth_detail.auth_detail_obj,
                             INPUT "":U,
                             INPUT "adjustment_private_paid":U,
                             INPUT btt_auth_detail.line_number,
                             INPUT "ma_MsgAuth":U,
                             INPUT 11,  /* The &1 (&2) may not be greater than the &3 (&4). */
                             INPUT "Adjustment Private Paid," + STRING(btt_auth_detail.adjustment_private_paid) + ",Adjustment Private Authorised,":U + STRING(btt_auth_detail.adjustment_private_auth)).
    END. /*IF btt_auth_detail.adjustment_private_paid <> 0.00 AND btt_auth_detail.adjustment_private_auth < btt_auth_detail.adjustment_private_paid THEN */

  END.  /* IF btt_auth_detail.amount_paid <> 0.00 OR btt_auth_detail.quantity_paid <> 0 THEN */

END. /* IF goAuthorisation:InFocus AND AVAILABLE btt_auth_detail THEN */
                                             
                                                                                       
{ mip/inc/mipcatcherror.i
  &FINALLY = "IF VALID-OBJECT(oBodyRegion)       THEN DELETE OBJECT oBodyRegion.
              IF VALID-OBJECT(oAuthDetailLines)  THEN DELETE OBJECT oAuthDetailLines.
              IF VALID-OBJECT(oAuthRule)         THEN DELETE OBJECT oAuthRule.
              IF VALID-OBJECT(oDetail)           THEN DELETE OBJECT oDetail."
}

&ENDIF
