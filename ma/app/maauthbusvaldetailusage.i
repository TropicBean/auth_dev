/* maauthbusvaldetailusage.i  MEDSTAR Medical Aid System
                              Authorisation Type Usage limit
                              (c) Copyright 1990 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/                      
&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cError                 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProviderTypeIndicator AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTariffTypeList        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTariffTypeObjList     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTrackingMessage       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iIdx                   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj           AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dAuthTypeProviderObj   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTariffTypeObj         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTotalQuantityAuth     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lProcessUsage          AS LOGICAL   NO-UNDO.
  
  DEFINE BUFFER buf_auth_type_provider FOR hac_auth_type_provider .
  DEFINE BUFFER buf_auth_type_detail   FOR hac_auth_type_detail.
  DEFINE BUFFER b2tt_auth_detail       FOR tt_auth_detail.
  DEFINE BUFFER buf_tariff_type        FOR htm_tariff_type.

  /*** 
    First things first , lets check if there is any auth type provider and detail setup limiting usage
  ***/
  ASSIGN dAuthTypeObj           = goAuthorisation:AuthTypeObj
         cProviderTypeIndicator = "ma_acAuthProviderTypeIndicatorDef":U
         cTrackingMessage       = "Checking for usage set up on auth type provider".
         
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  FIND FIRST btt_auth_provider 
       WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj 
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

  IF AVAILABLE btt_auth_provider THEN
  DO:
    { ma/msc/maauthtypeproviderread.i 
                      &hac_auth_type_provider  = buf_auth_type_provider
                      &AuthTypeObj             = goAuthorisation:AuthTypeObj 
                      &InsurerObj              = goAuthorisation:InsurerObj 
                      &OptionCode              = goAuthorisation:OptionCode
                      &ProviderType            = btt_auth_provider.provider_type 
                      &AuthGroupObj            = btt_auth_provider.auth_group_obj
                      &ProviderTypeIndicator   = cProviderTypeIndicator
                      &EffectiveDate           = btt_auth_provider.start_date
                      &Lock                    = NO-LOCK } 
                      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE } 
    
    IF AVAILABLE buf_auth_type_provider
    AND CAN-FIND(FIRST  buf_auth_type_detail 
                 WHERE  buf_auth_type_detail.auth_type_obj           = dAuthTypeObj
                   AND  buf_auth_type_detail.auth_type_provider_obj  = buf_auth_type_provider.auth_type_provider_obj
                   AND  buf_auth_type_detail.detail_type_indicator   = "ma_acAuthDetailTypeIndicatorDef":U
                   AND  buf_auth_type_detail.effective_date         <= btt_auth_detail.start_date
                   AND (buf_auth_type_detail.end_date                = ? OR hac_auth_type_detail.end_date >= btt_auth_detail.start_date)
                   AND  buf_auth_type_detail.auth_usage_limit       <> 0)
    THEN
      ASSIGN dAuthTypeProviderObj   = buf_auth_type_provider.auth_type_provider_obj
             lProcessUsage          = TRUE
             cTrackingMessage       = "Auth type detail usage setup has been found. Validations will be done to ensure auth details fall within limits.":U .             
      
  END. //IF AVAILABLE btt_auth_provider 
  ELSE 
    ASSIGN cTrackingMessage = "No Auth type detail usage setup has been found. No validations necessary.":U .
    
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage} 
  
  IF lProcessUsage THEN 
  DO:

    /***
      Determine what tariff types, nappis and baskets we have on this auth, and the quantity 
      of times each of them have been used. Store it in temp table tt_usage_processing.
    ***/
    BUILD-PROCESSING-TT-BLK:
    FOR EACH b2tt_auth_detail 
       WHERE b2tt_auth_detail.auth_obj          = btt_auth_detail.auth_obj 
         AND b2tt_auth_detail.auth_provider_obj = btt_auth_detail.auth_provider_obj   
         AND b2tt_auth_detail.auth_status      <> 6
         AND b2tt_auth_detail.auth_status      <> 5 :
          
      /*
        Tariff types
      */
      IF b2tt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
      DO:
        mipEnv:health:maMedical:getTariffTypes(INPUT  b2tt_auth_detail.owning_obj ,   /* ipdTariffLinkObj     */
                                               INPUT  b2tt_auth_detail.start_date,    /* ipdDate              */       
                                               INPUT  "":U ,                          /* ipcCategory          */
                                               OUTPUT cTariffTypeList,                /* opcTariffTypeList    */
                                               OUTPUT cTariffTypeObjList,             /* opcTariffTypeObjList */
                                               OUTPUT cError ).                       /* opcError             */
        IF cError = "":U
        AND cTariffTypeObjList <> "":U THEN 
        DO:
          
          TARIFF-TYPE-BLK:
          DO iIdx = 1 TO NUM-ENTRIES(cTariffTypeObjList) :
            
            ASSIGN dTariffTypeObj  = DECIMAL(ENTRY(iIdx,cTariffTypeObjList)) .
            
            FIND FIRST tt_usage_processing 
                 WHERE tt_usage_processing.owning_obj             = dTariffTypeObj
                   AND tt_usage_processing.owning_entity_mnemonic = "htmtt":U 
              NO-ERROR.
            
            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
            
            /*
              Create/update the tt_usage_processing record
            */
            IF NOT AVAILABLE tt_usage_processing THEN                                                                                                   
            DO:                                                                                                                                         
              CREATE tt_usage_processing.                                                                                                             
                                                                                                                                                      
              ASSIGN tt_usage_processing.owning_obj              = dTariffTypeObj                                                                     
                     tt_usage_processing.owning_entity_mnemonic  = "htmtt":U                                                                          
                     tt_usage_processing.auth_detail_code_list   = STRING(b2tt_auth_detail.owning_alt_value)                                           
                     tt_usage_processing.total_quantity_auth     = b2tt_auth_detail.quantity_auth .                                                   
                                                                                                                                                        
            END. //IF NOT AVAILABLE tt_usage_processing                                                                                                 
            ELSE
              ASSIGN tt_usage_processing.auth_detail_code_list  = tt_usage_processing.auth_detail_code_list + ",":U + STRING(b2tt_auth_detail.owning_alt_value)  
                     tt_usage_processing.total_quantity_auth    = tt_usage_processing.total_quantity_auth  + b2tt_auth_detail.quantity_auth .          
            
          END. //TARIFF-TYPE-BLK
        END. //cError
      END. // IF b2tt_auth_detail.owning_entity_mnemonic = "htmtl":U THEN
      
      /*
        Nappi and basket 
      */
      ELSE IF b2tt_auth_detail.owning_entity_mnemonic = "hlmnl":U 
           OR b2tt_auth_detail.owning_entity_mnemonic = "hlmcr":U THEN
      DO:
        FIND FIRST tt_usage_processing 
             WHERE tt_usage_processing.owning_obj             = b2tt_auth_detail.owning_obj
               AND tt_usage_processing.owning_entity_mnemonic = b2tt_auth_detail.owning_entity_mnemonic 
          NO-ERROR.
            
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
         
        /*
          Create/update the tt_usage_processing record
        */
        IF NOT AVAILABLE tt_usage_processing THEN                                                                                                   
        DO:                                                                                                                                         
          CREATE tt_usage_processing.                                                                                                             
                                                                                                                                                  
          ASSIGN tt_usage_processing.owning_obj             = b2tt_auth_detail.owning_obj                                                                     
                 tt_usage_processing.owning_entity_mnemonic = b2tt_auth_detail.owning_entity_mnemonic                                                                         
                 tt_usage_processing.auth_detail_code_list  = STRING(b2tt_auth_detail.owning_alt_value)                                           
                 tt_usage_processing.total_quantity_auth    = b2tt_auth_detail.quantity_auth .                                                   
                                                                                                                                                  
        END. //IF NOT AVAILABLE tt_usage_processing                                                                                                 
        ELSE
          ASSIGN tt_usage_processing.auth_detail_code_list = tt_usage_processing.auth_detail_code_list + ",":U + STRING(b2tt_auth_detail.owning_alt_value)  
                 tt_usage_processing.total_quantity_auth   = tt_usage_processing.total_quantity_auth + b2tt_auth_detail.quantity_auth. 

      END. // ELSE IF b2tt_auth_detail.owning_entity_mnemonic = "hlmnl":U OR b2tt_auth_detail.owning_entity_mnemonic = "hlmcr":
      
      /* 
        Related entities need to be counted as well
      */
      IF  b2tt_auth_detail.related_obj > 0
      AND b2tt_auth_detail.related_entity_mnemonic <> "":U THEN 
      DO:
        FIND FIRST tt_usage_processing 
             WHERE tt_usage_processing.owning_obj             = b2tt_auth_detail.related_obj
               AND tt_usage_processing.owning_entity_mnemonic = b2tt_auth_detail.related_entity_mnemonic NO-ERROR.
            
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
         
         /*
           Create/update the tt_usage_processing record
         */
         IF NOT AVAILABLE tt_usage_processing THEN                                                                                                   
         DO:                                                                                                                                         
             CREATE tt_usage_processing.                                                                                                             
                                                                                                                                                     
             ASSIGN tt_usage_processing.owning_obj              = b2tt_auth_detail.related_obj                                                                     
                    tt_usage_processing.owning_entity_mnemonic  = b2tt_auth_detail.related_entity_mnemonic                                                                         
                    tt_usage_processing.auth_detail_code_list   = STRING(b2tt_auth_detail.related_value)                                           
                    tt_usage_processing.total_quantity_auth     = b2tt_auth_detail.quantity_auth .                                                   
                                                                                                                                                     
         END. //IF NOT AVAILABLE tt_usage_processing                                                                                                 
         ELSE DO:                                                                                                                                    
             ASSIGN tt_usage_processing.auth_detail_code_list  = tt_usage_processing.auth_detail_code_list + ",":U + STRING(b2tt_auth_detail.related_value)  
                    tt_usage_processing.total_quantity_auth    = tt_usage_processing.total_quantity_auth  + b2tt_auth_detail.quantity_auth .          
                                                                                                                                                     
         END. //ELSE (IF AVAILABLE tt_usage_processing) 
      END. //IF  b2tt_auth_detail.related_obj > 0 AND b2tt_auth_detail.related_entity_mnemonic <> "":U
      
    END. //  BUILD-PROCESSING-TT-BLK
    
    /***
      Now let's read the auth type configuration and validate if the usage falls within the bounds of the limits set up
    ***/
    USAGE-VALIDATION-BLK:
    FOR EACH tt_usage_processing :
      
      FIND FIRST buf_auth_type_detail NO-LOCK
           WHERE buf_auth_type_detail.auth_type_obj          =  dAuthTypeObj      
             AND buf_auth_type_detail.auth_type_provider_obj =  buf_auth_type_provider.auth_type_provider_obj  
             AND buf_auth_type_detail.owning_entity_mnemonic =  tt_usage_processing.owning_entity_mnemonic
             AND buf_auth_type_detail.owning_obj             =  tt_usage_processing.owning_obj
             AND buf_auth_type_detail.effective_date        <=  btt_auth_detail.start_date 
             AND (buf_auth_type_detail.end_date  = ? OR  buf_auth_type_detail.end_date >= btt_auth_detail.start_date) NO-ERROR.
   
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
 
      IF AVAILABLE buf_auth_type_detail THEN 
      DO:
      
        IF tt_usage_processing.total_quantity_auth > buf_auth_type_detail.auth_usage_limit 
        THEN
          ASSIGN cError   = (IF NUM-ENTRIES(tt_usage_processing.auth_detail_code_list) > 1 
                             THEN SUBSTITUTE("The combined quantity of codes(&1) is &2." , tt_usage_processing.auth_detail_code_list , tt_usage_processing.total_quantity_auth )
                             ELSE "":U )
                          + SUBSTITUTE("Detail Code &1 can not be saved. Usage quantity (&2) exceeds the maximum usage limit(&3)":U ,
                                        btt_auth_detail.owning_alt_value ,
                                        tt_usage_processing.total_quantity_auth ,
                                        buf_auth_type_detail.auth_usage_limit )  
                                        
                 lSuccess = goErrorObject:addError(INPUT "hatad:":U  + btt_auth_detail.owning_entity_mnemonic,   /* ipcOwningEntityMnemonic  */
                                                   INPUT btt_auth_detail.auth_detail_obj,                        /* ipdOwningEntityObj       */
                                                   INPUT "":U,                                                   /* ipcOwningEntityKey       */
                                                   INPUT btt_auth_detail.line_number,                            /* ipiLineNumber            */
                                                   INPUT cError,                                                 /* ipcErrorMessage          */
                                                   INPUT "ERR":U).                                               /* ipcErrorType             */

      END. // IF AVAILABLE buf_auth_type_detail
    END. //USAGE-VALIDATION-BLK
  END. // IF lProcessUsage THEN 
  
  EMPTY TEMP-TABLE tt_usage_processing.

&ENDIF
