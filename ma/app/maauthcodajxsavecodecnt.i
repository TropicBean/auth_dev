/* maauthcodajxsavecodecnt.i  MEDSTAR Medical Aid System
                              Authorisation Coding Container Auto Save Procedure
                              (c) Copyright 1990 - 2018
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/        

/* This file name does not follow the same naming convention as other ajax includes, so this code has been moved to ma/app/maauthcoduiajaxsave.i so we can be consistent with our naming conventions.*/ 
            
/*  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
                                     
  DEFINE VARIABLE oRequestHelper        AS cls.maajaxrequesthelper     NO-UNDO.
  DEFINE VARIABLE oResponseHelper       AS cls.maajaxresponsehelper    NO-UNDO.
  DEFINE VARIABLE oCoding               AS cls.maauthorisationcoding   NO-UNDO.
  DEFINE VARIABLE oSearch               AS cls.maauthsearch            NO-UNDO.
  DEFINE VARIABLE oAuthorisation        AS cls.maauthorisation         NO-UNDO.
  DEFINE VARIABLE oDiagnosis            AS cls.madiagnosis             NO-UNDO.                                   
  DEFINE VARIABLE cAsterisk             AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cBodyRegion           AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cCodingType           AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cConditionCode        AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cContainerCode        AS CHARACTER                   NO-UNDO.   
  DEFINE VARIABLE cObjList              AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cOEM                  AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cFieldName            AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cFilterField          AS CHARACTER                   NO-UNDO.  
  DEFINE VARIABLE cFilterValue          AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cFilterFieldList      AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cFilterValueList      AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cMorphology           AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cRuleValue            AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cOwningKey            AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cOwningAltValue       AS CHARACTER                   NO-UNDO.  
  DEFINE VARIABLE cQueryFieldList       AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cQueryFieldMapping    AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cRecordAction         AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cStatusNote           AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE iFilterField          AS INTEGER                     NO-UNDO.
  DEFINE VARIABLE iRow                  AS INTEGER                     NO-UNDO.
  DEFINE VARIABLE iStatus               AS INTEGER                     NO-UNDO.
  DEFINE VARIABLE dAsteriskObj          AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE dMorphologyObj        AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE dAuthCodingObj        AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE dAuthObj              AS DECIMAL                     NO-UNDO.    
  DEFINE VARIABLE dOwningObj            AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE dPmbBenefit           AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE dEndDate              AS DATE                        NO-UNDO.
  DEFINE VARIABLE dStartDate            AS DATE                        NO-UNDO.
  DEFINE VARIABLE dProcedureDate        AS DATE                        NO-UNDO.
  DEFINE VARIABLE lPrimaryCode          AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE lPMBApplicable        AS LOGICAL                     NO-UNDO.   
  DEFINE VARIABLE lMainCode             AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL                     NO-UNDO.
  DEFINE VARIABLE dInsurerObj           AS DECIMAL                     NO-UNDO.
  DEFINE VARIABLE iOptionCode           AS INTEGER                     NO-UNDO.
  
  
  DATASET dsAuthorisation:EMPTY-DATASET().
       
  ASSIGN oResponseHelper = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE).   
     
  IF ipcValidationArgument = "CompleteSave":U THEN 
  DO:
    ASSIGN
      cFilterFieldList = get-value('FldLst')
      cFilterValueList = get-value('ValList').
    
    DO iFilterField = 1 TO NUM-ENTRIES(cFilterFieldList, "|":U):
    
      ASSIGN cFilterField = REPLACE(ENTRY(iFilterField, cFilterFieldList, "|":U), "[-Cma-]":U, ",":U)
             cFilterValue = REPLACE(ENTRY(iFilterField, cFilterValueList, "|":U), "[-Cma-]":U, ",":U).
      
      CASE cFilterField:
      
        WHEN "[ObjList]":U
        THEN ASSIGN cObjList = TRIM(cFilterValue).
        
        WHEN "[QueryFieldMapping]":U
        THEN ASSIGN cQueryFieldMapping = TRIM(cFilterValue).            
                      
        WHEN "[AuthObj]":U
        THEN ASSIGN dAuthObj = DECIMAL(TRIM(cFilterValue)).                            
                      
      END CASE.
    
    END. /*DO iFilter = 1 TO NUM-ENTRIES(cFilterFields):*/
         
    IF dAuthObj > 0 THEN 
    DO:
      /*
        We want to retrieve all the coding records for this authorisation
      */
      IF NOT {&ErrorStatus}
      THEN
        oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE) NO-ERROR.
      
      IF NOT {&ErrorStatus}
      THEN
        ASSIGN 
            lSuccess = oSearch:SetCriteria("BufferList":U , "tt_auth,tt_auth_coding":U)
            lSuccess = oSearch:SetCriteria("DataSecured":U, "FALSE":U)
            
            lSuccess = oSearch:SetFilterCriteria("tt_auth.auth_obj":U, "=":U, dAuthObj) 
            
            lSuccess = oSearch:SetFilterCriteria("tt_auth_coding.owning_entity_mnemonic":U, "=":U, "diagnos":U)
            
            lSuccess = oSearch:fetchData()
           NO-ERROR.
    END. /*IF dAuthObj > 0 THEN */
    
    /*
      Set the modify action on all coding records and save the dataset, which will fire the validation on the coding records
    */
    IF NOT {&ErrorStatus} THEN
    DO:
      FOR EACH tt_auth_coding EXCLUSIVE-LOCK:
      
        ASSIGN tt_auth_coding.record_action = 'MODIFY'.
        
        VALIDATE tt_auth_coding.      
      END. /*FOR EACH tt_auth_coding EXCLUSIVE-LOCK:*/
    END. /*IF NOT {&ErrorStatus} THEN*/
    
    /*
      And save
    */
    IF NOT {&ErrorStatus} AND CAN-FIND(FIRST tt_auth_coding)
    THEN
      mipEnv:Health:AuthBusinessLogic:SaveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE) NO-ERROR.

    /*
      Everything saved correctly
    */
    IF NOT {&ErrorStatus} AND NOT CAN-FIND(FIRST tt_auth_error) THEN
    DO:
      ASSIGN 
         oResponseHelper:RequestValid    = TRUE
         oResponseHelper:ResponseMessage = "Record successfully saved":U
        NO-ERROR. 
    END. /*IF NOT {&ErrorStatus} AND NOT CAN-FIND(FIRST tt_auth_error) THEN*/
    
    /*
      Check if an error occurred
      We are looking for business logic validation errors
    */
    IF {&ErrorStatus} OR CAN-FIND(FIRST tt_auth_error) THEN
    DO:
      /*
        We have a bit of a problem here because we save all the records and not a single row so we have to do 
        some footwork to identify which row we should populate the error on, fortunately we pass in an obj to 
        row mapping so from the error which has an obj we can identify which row to return the error to. (AD)
      */
      FOR EACH tt_auth_error EXCLUSIVE-LOCK:
      
        ASSIGN iRow       = INTEGER(mipEnv:miExpression:getNVPElement(cObjList          , STRING(ROUND(tt_auth_error.owning_obj, 0)), ",":U, "=":U))        
               cFieldName =         mipEnv:miExpression:getNVPElement(cQueryFieldMapping, tt_auth_error.error_field_name            , ",":U, "=":U)
              NO-ERROR.                       
               
        IF cFieldName <> ? 
        THEN ASSIGN tt_auth_error.error_element_name = cFieldName + STRING(iRow) NO-ERROR.
        
        VALIDATE tt_auth_error NO-ERROR.
               
      END. /*FOR EACH tt_auth_error NO-LOCK:*/
    
      ASSIGN 
        oResponseHelper:RequestValid    = FALSE                
        
        oResponseHelper:ResponseMessage = 'Unable to perform action':U
        oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U)
       NO-ERROR.
    
      { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
    END.   /*IF oCoding:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
    
    /*
      Warnings will also be included in the error table
    */
    ASSIGN lSuccess = oResponseHelper:setError(TEMP-TABLE tt_auth_error:HANDLE) NO-ERROR.
        
  END. /*IF ipcValidateArgument = "CompleteSave":U THEN */
  ELSE
  DO:
    ASSIGN
       oRequestHelper  = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))       
       
       cContainerCode  = ipcValidationArgument
       
       cRecordAction   =         oRequestHelper:getFieldValue("fcAction":U         + cContainerCode)
       cStatusNote     =         oRequestHelper:getFieldValue("fcReason":U         + cContainerCode)
       cCodingType     =         oRequestHelper:getFieldValue("cbCodingType":U     + cContainerCode)
       cOwningKey      =         oRequestHelper:getFieldValue("fcOwningKey":U      + cContainerCode)  
       cOwningAltValue =         oRequestHelper:getFieldValue("fcOwningAltValue":U + cContainerCode)
       cAsterisk       =         oRequestHelper:getFieldValue("fcAsterisk":U       + cContainerCode)
       cMorphology     =         oRequestHelper:getFieldValue("fcMorphology":U     + cContainerCode) 
       cOEM            =         oRequestHelper:getFieldValue("_oemArgument":U     + cContainerCode) 
       cBodyRegion     =         oRequestHelper:getFieldValue("fcBodyRegion":U     + cContainerCode)
       iStatus         = INTEGER(oRequestHelper:getFieldValue("cbStatus":U         + cContainerCode))
       dAuthCodingObj  = DECIMAL(oRequestHelper:getFieldValue("fdAuthCodingObj":U  + cContainerCode))
       dPmbBenefit     = DECIMAL(oRequestHelper:getFieldValue("fdPMBBenefit":U     + cContainerCode))
       dOwningObj      = DECIMAL(oRequestHelper:getFieldValue("fdOwningObj":U      + cContainerCode)) 
       dAsteriskObj    = DECIMAL(oRequestHelper:getFieldValue("fdAsteriskObj":U    + cContainerCode))
       dMorphologyObj  = DECIMAL(oRequestHelper:getFieldValue("fdMorphologyObj":U  + cContainerCode))
       
       dAuthObj        = DECIMAL(oRequestHelper:getFieldValue("_authObjArgument":U + cContainerCode)) 
       
       dStartDate      = (IF oRequestHelper:getFieldValue("fdStartDate":U + cContainerCode) MATCHES "*YYYY*"   
                          THEN ?
                          ELSE DATE(oRequestHelper:getFieldValue("fdStartDate":U + cContainerCode)))
                          
       dEndDate        = (IF oRequestHelper:getFieldValue("fdEndDate":U + cContainerCode) MATCHES "*YYYY*":U  
                          THEN ? 
                          ELSE DATE(oRequestHelper:getFieldValue("fdEndDate":U + cContainerCode)))
                          
       dProcedureDate  = (IF oRequestHelper:getFieldValue("fdProcedureDate":U + cContainerCode) MATCHES "*YYYY*":U  
                          THEN ? 
                          ELSE DATE(oRequestHelper:getFieldValue("fdProcedureDate":U + cContainerCode)))                   
       
       lPrimaryCode    = CAN-DO("Y,YES,TRUE":U,oRequestHelper:getFieldValue("flPrimaryCode":U   + cContainerCode))             
       lPmbApplicable  = CAN-DO("Y,YES,TRUE":U,oRequestHelper:getFieldValue("flPMBIndicator":U  + cContainerCode))
       lMainCode       = CAN-DO("Y,YES,TRUE":U,oRequestHelper:getFieldValue("flMainCode":U      + cContainerCode))
    
       oCoding         = NEW cls.maauthorisationcoding()
      NO-ERROR.        
    
    IF NOT {&ErrorStatus} THEN
    DO:
      CASE cRecordAction:
        
        WHEN "modify":U THEN
        DO:
          oCoding:focusRecord(dAuthCodingObj) NO-ERROR.
          
          IF NOT {&ErrorStatus}
          THEN          
          ASSIGN            
            cOwningAltValue              = REPLACE(cOwningAltValue, "+":U, "":U)
            cAsterisk                    = REPLACE(cAsterisk      , "*":U, "":U)
              
            oCoding:RecordAction         = 'PARTIALSAVE'
            oCoding:OwningEntityMnemonic = cOEM
            oCoding:OwningObj            = dOwningObj
            oCoding:OwningKey            = cOwningKey
            oCoding:OwningAltValue       = cOwningAltValue
            oCoding:AuthObj              = dAuthObj
            oCoding:AssDiagObj           = dAsteriskObj
            oCoding:MorphDiagObj         = dMorphologyObj
            oCoding:CodingType           = cCodingType
            oCoding:CodingStatus         = iStatus
            oCoding:CodingStatusNote     = cStatusNote            
            oCoding:PrimaryCode          = lPrimaryCode  
            oCoding:PmbIndicator         = lPMBApplicable          
            oCoding:MainCode             = lMainCode
            oCoding:StartDate            = dStartDate
            oCoding:EndDate              = dEndDate
            oCoding:ProcedureDate        = dProcedureDate
            oCoding:BodyRegion           = cBodyRegion
    
            lSuccess                     = oCoding:saveRecord() 
           NO-ERROR.          
 
          IF NOT {&ErrorStatus} AND NOT oCoding:ErrorObject:ErrorsExist THEN
          DO:   
            ASSIGN         
              oAuthorisation  = NEW cls.maauthorisation()
              lSuccess        = oAuthorisation:focusRecord(oCoding:AuthObj)
              
              dInsurerObj     = oAuthorisation:InsurerObj
              iOptionCode     = oAuthorisation:MemberOptionCode
             NO-ERROR.
    
            /*
              Find asterisk code if applicable
            */            
            IF NOT {&ErrorStatus} AND dAsteriskObj <> 0.00 THEN
            DO:
              oDiagnosis = NEW cls.madiagnosis().

              oDiagnosis:focusDiagnosis(dAsteriskObj).   
              
              IF oDiagnosis:DiagnosisInFocus
              THEN 
                ASSIGN cAsterisk = oDiagnosis:Diagnosis.
            END. /*IF dAsteriskObj <> 0.00 THEN*/   
            
            /*
              Check if PMB applicable
            */
            ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                            (INPUT  dInsurerObj,
                             INPUT  iOptionCode,
                             INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                             INPUT  "PMBViewDetails":U,
                             INPUT  oAuthorisation:StartDate,
                             OUTPUT lValidRule,
                             OUTPUT cRuleValue)
                           NO-ERROR.     
                                         
            IF NOT {&ErrorStatus} AND LOOKUP(TRIM(cRuleValue),"Active,Activate":U) > 0
            THEN
              mipEnv:Health:maMedical:getConditionCode
                (INPUT  "PMB":U,
                 INPUT  cOwningAltValue,
                 INPUT  cAsterisk,
                 INPUT  dStartDate,
                 OUTPUT cConditionCode) NO-ERROR.                        
            
            /*
              Check if PMB indicator should be enabled/disabled/hidden/visible
            */
            IF NOT {&ErrorStatus}
            THEN 
            ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                            (INPUT  dInsurerObj,
                             INPUT  iOptionCode,
                             INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                             INPUT  "PMBDecision":U,
                             INPUT  oAuthorisation:StartDate,
                             OUTPUT lValidRule,
                             OUTPUT cRuleValue)
                           NO-ERROR.
             
            /*
              This block will handle values that may have been derived from the service that 
              we need to send back to the UI such as obj value, main code etc
            */                   
            IF NOT {&ErrorStatus}
            THEN
            ASSIGN              
              oResponseHelper:RequestValid    = TRUE
              oResponseHelper:ResponseMessage = "Record successfully saved":U 
              
              lSuccess                        = oResponseHelper:addFieldValue("cbStatusDispAuthContainer":U  , STRING(oAuthorisation:AuthStatus))   
              lSuccess                        = oResponseHelper:addFieldValue("cbSystemStatusAuthContainer":U, STRING(oAuthorisation:AuthStatus))   
              lSuccess                        = oResponseHelper:addFieldValue("fcReasonAuthContainer":U      , STRING(oAuthorisation:AuthStatusNote))
              lSuccess                        = oResponseHelper:addFieldValue("fcReasonDescAuthContainer":U  , STRING(oAuthorisation:AuthStatusNoteDescription), TRUE)   
                                                                                                                                                                                                                                                                                             
              lSuccess                        = oResponseHelper:addFieldValue("fdAuthCodingObj":U     + cContainerCode, STRING(oCoding:AuthCodingObj),TRUE) /* 'TRUE' will force an on-change event on the control */
              lSuccess                        = oResponseHelper:addFieldValue("cbStatus":U            + cContainerCode, STRING(oCoding:CodingStatus))                           
                                                                                                      
              lSuccess                        = oResponseHelper:addFieldValue("fcReason":U            + cContainerCode, oCoding:CodingStatusNote)              
              lSuccess                        = oResponseHelper:addFieldValue("fcReasonDesc":U        + cContainerCode, oCoding:CodingStatusNoteDescription, TRUE)       
              lSuccess                        = oResponseHelper:addFieldValue("fdEndDate":U           + cContainerCode, STRING(oCoding:EndDate,"99/99/9999":U))
              
              lSuccess                        = oResponseHelper:addFieldValue("flPrimaryCode":U       + cContainerCode, STRING(oCoding:PrimaryCode))                                                                                        
              lSuccess                        = oResponseHelper:addFieldValue("flMainCode":U          + cContainerCode, STRING(oCoding:MainCode), TRUE)                                                                                        
              lSuccess                        = oResponseHelper:addFieldValue("flPmbLinkEnabled":U    + cContainerCode, STRING(cConditionCode <> "":U), TRUE)
              lSuccess                        = oResponseHelper:addFieldValue("fcPMBIndicatorState":U + cContainerCode, cRuleValue, TRUE)                     
                                
             NO-ERROR.            
               
            IF NOT {&ErrorStatus} AND oCoding:AssDiagObj <> 0.00 AND cOwningAltValue <> "":U AND cAsterisk <> "":U THEN
            DO:
              ASSIGN 
                 lSuccess = oResponseHelper:addFieldValue("fcOwningAltValue":U + cContainerCode, cOwningAltValue + "+":U, TRUE)
                 lSuccess = oResponseHelper:addFieldValue("fcAsterisk":U       + cContainerCode, cAsterisk       + "*":U)
                NO-ERROR.
            END. /*IF oCoding:AssDiagObj <> 0.00 THEN*/            
          END. /*IF NOT {&ErrorStatus} AND NOT oCoding:ErrorObject:ErrorsExist THEN*/          
        END. /* WHEN "modify":U THEN */
        
        WHEN "delete":U THEN
        DO:  
          oCoding:focusRecord(dAuthCodingObj) NO-ERROR.
          
          IF NOT {&ErrorStatus} AND NOT oCoding:InFocus 
          THEN  
            ASSIGN 
              oResponseHelper:RequestValid    = FALSE
              oResponseHelper:ResponseMessage = "Record could not be deleted":U 
            NO-ERROR.
          ELSE 
            ASSIGN lSuccess = oCoding:deleteRecord() NO-ERROR.
          
          IF NOT {&ErrorStatus} AND NOT oCoding:ErrorObject:ErrorsExist
          THEN
            ASSIGN 
              oResponseHelper:RequestValid    = TRUE
              oResponseHelper:ResponseMessage = "Record successfully removed":U
              oResponseHelper:ReturnValue     = "Record successfully removed":U 
              NO-ERROR.    
        END. /* WHEN "delete":U THEN */
        
        OTHERWISE
        DO:
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported":U, cRecordAction)
            oResponseHelper:ResponseMessage = "Unable to perform action":U 
           NO-ERROR.
        END. /* OTHERWISE */       
      END CASE.            
    END. /*IF NOT {&ErrorStatus} THEN*/      
    
    IF {&ErrorStatus} OR oCoding:ErrorObject:ErrorsExist THEN
    DO:
      ASSIGN 
        oResponseHelper:RequestValid    = FALSE
        
        lSuccess                        = oRequestHelper:mapElementsFromQueryFields(oCoding:ErrorObject)
        
        oResponseHelper:ResponseMessage = 'Unable to perform action':U
        oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U)
       NO-ERROR.
      
      { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
    END.   /*IF oCoding:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
    
    /*
      Warnings will also be included in the error table
    */
    ASSIGN lSuccess = oResponseHelper:setError(oCoding:ErrorObject).
  END. /*ELSE*/
    
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oDiagnosis)      THEN DELETE OBJECT oDiagnosis      NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oCoding)         THEN DELETE OBJECT oCoding         NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oSearch)         THEN DELETE OBJECT oSearch         NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthorisation)  THEN DELETE OBJECT oAuthorisation  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        
                                        DATASET dsAuthorisation:EMPTY-DATASET()." }
 
*/


