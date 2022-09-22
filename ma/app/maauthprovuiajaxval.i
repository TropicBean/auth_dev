/*------------------------------------------------------------------------------
  maauthprovuiajaxval.i  MEDSTAR Medical Aid System
                         Auth Provider Container Ajax Validation    
                         (c) Copyright 2018 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved   
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE lcConfiguration      AS LONGCHAR            NO-UNDO.
  DEFINE VARIABLE cAttProviderNum      AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cArsRate             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cBaseRate            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cDiscipline          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cErrorMessage        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cFilterField         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cFilterFieldList     AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cFilterValue         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cFilterValueList     AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessageText         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessageDescription  AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessageType         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cProviderNum         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cPrType              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cProviderType        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cReasonCode          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cReasonType          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cReturnField         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cReturnFieldList     AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cReturnValues        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRuleValue           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cSubPrType           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cValidProvider       AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cReasonDescription   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatus              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cMessageError        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE dStartDate           AS DATE                NO-UNDO.
  DEFINE VARIABLE dAuthObj             AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj         AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dProviderObj         AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dInsurerObj          AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iFilterField         AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iDiscipline          AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iOptionCode          AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iReturnField         AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iSubDiscipline       AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iNegNum              AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lMandatory           AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lProviderGroup       AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidDiscipline     AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidGroup          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidRule           AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE oSearch              AS cls.maauthsearch    NO-UNDO.
  DEFINE VARIABLE oAuthorisation       AS cls.maauthorisation NO-UNDO.
  
  
  &SCOPED-DEFINE AppendReturnValues ASSIGN cReturnValues = cReturnValues + (IF cReturnValues = "":U THEN "":U ELSE "|":U) +
  
  &SCOPED-DEFINE ValidationSuccess  ASSIGN ttValidation.cReturnValues = cReturnValues ~
                                           ttValidation.lValid        = TRUE          ~
                                           ttValidation.cMessage      = "Success":U.  ~
                                                                                      ~
                                    VALIDATE ttValidation.  
  
  &SCOPED-DEFINE BlankResponse      ASSIGN ttValidation.cReturnValues = FILL("|":U, NUM-ENTRIES(cReturnFieldList) - 1) ~
                                           ttValidation.lValid        = TRUE.                                          ~
                                                                                                                       ~
                                    VALIDATE ttValidation.
  
  ASSIGN
    cFilterFieldList = get-value('FldLst')
    cFilterValueList = get-value('ValList')
    cReturnFieldList = get-value('RetFldList').
  
  CREATE ttValidation.
  
  DO iFilterField = 1 TO NUM-ENTRIES(cFilterFieldList):
  
    ASSIGN cFilterField = ENTRY(iFilterField, cFilterFieldList)
           cFilterValue = ENTRY(iFilterField, cFilterValueList).
    
    CASE cFilterField:
    
      WHEN "[ProviderType]":U
      THEN ASSIGN cProviderType = TRIM(cFilterValue).
      
      WHEN "[ProviderObj]":U
      THEN ASSIGN dProviderObj = DECIMAL(TRIM(cFilterValue)).
      
      WHEN "[ProviderNum]":U
      THEN ASSIGN cProviderNum = TRIM(cFilterValue).
      
      WHEN "[AttProviderNum]":U
      THEN ASSIGN cAttProviderNum = TRIM(cFilterValue).
      
      WHEN "[PrType]":U
      THEN ASSIGN cPrType = TRIM(cFilterValue).
      
      WHEN "[SubPrType]":U
      THEN ASSIGN cSubPrType = TRIM(cFilterValue).
      
      WHEN "[Status]":U
      THEN ASSIGN cStatus = cFilterValue.
      
      WHEN "[ReasonCode]":U
      THEN ASSIGN cReasonCode = TRIM(cFilterValue).

      WHEN "[ReasonType]":U
      THEN ASSIGN cReasonType = TRIM(cFilterValue).
      
      WHEN "[StartDate]":U 
      THEN ASSIGN dStartDate = DATE(cFilterValue).
      
      WHEN "[ProviderType]":U
      THEN ASSIGN cProviderType = TRIM(cFilterValue).
      
      WHEN "[AuthTypeObj]":U
      THEN ASSIGN dAuthTypeObj = DECIMAL(cFilterValue).
      
      WHEN "[AuthObj]":U
      THEN ASSIGN dAuthObj = DECIMAL(cFilterValue).

      WHEN "[NegNum]"
      THEN ASSIGN iNegNum = INTEGER(cFilterValue).
      
    END CASE.
  
  END. /*DO iFilter = 1 TO NUM-ENTRIES(cFilterFields):*/
  
  { ma/msc/madispad.i &discipline = cPrType } 
  
  CASE ipcValidationArgument:
  
    WHEN "Provider":U THEN
    DO:
      IF cProviderNum <> "":U AND TRIM(cProviderNum) <> "0":U THEN 
      DO:

        FOR FIRST doctor NO-LOCK
            WHERE doctor.doc-num = INTEGER(cProviderNum):
         
          ASSIGN cDiscipline  =  mipEnv:Health:maDoctor:getProviderDiscipline( INPUT  doctor.doc-num,INPUT  dStartDate, INPUT  "BOTH" )  
                 cPrType      = IF NUM-ENTRIES(cDiscipline,"|":U) = 2 THEN ENTRY(1,cDiscipline,"|":U) ELSE "":U
                 cSubPrType   = IF NUM-ENTRIES(cDiscipline,"|":U) = 2 THEN ENTRY(2,cDiscipline,"|":U) ELSE "":U.

          IF dAuthObj <> 0.00 THEN
          DO:

            ASSIGN iOptionCode = 0.
            
            mipEnv:Health:AuthService:getAuthObject
              ( INPUT  dAuthObj,
                INPUT  "":U,
                OUTPUT oAuthorisation ).
            
            IF oAuthorisation:InFocus THEN
            DO:
              ASSIGN iOptionCode = oAuthorisation:OptionCode.

              IF iOptionCode = 0
              THEN mipEnv:Health:maMember:getMemberOption(INPUT oAuthorisation:MemNum, INPUT oAuthorisation:StartDate, OUTPUT iOptionCode).
              
            END. /*IF oAuthorisation:InFocus THEN*/

            /*
              If an attending provider has been specified ensure that the attending provider is valid for the group. 
            */
            IF cAttProviderNum <> "" AND cAttProviderNum <> "0" 
            THEN
              ASSIGN lSuccess = mipEnv:Health:maDoctor:validateProviderInGroup(INPUT  INTEGER(cProviderNum),
                                                                               INPUT  INTEGER(cAttProviderNum),
                                                                               INPUT  dStartDate,
                                                                               OUTPUT lValidGroup,
                                                                               OUTPUT cValidProvider,
                                                                               OUTPUT cErrorMessage).

            /* 
              If the atending provider is valid for the group, retrieve the base rates of the attending provider 
            */

            IF lValidGroup AND cValidProvider = "Valid":U THEN
            DO:
              IF iOptionCode <> 0
              THEN mipEnv:Health:maDoctor:getProviderBaseRates(INPUT INTEGER(cAttProviderNum), INPUT oAuthorisation:MemNum, INPUT iOptionCode, INPUT dStartDate, OUTPUT cBaseRate, OUTPUT cArsRate).
            END. /* IF lValidGroup AND cValidProvider = "Valid":U */
            ELSE
            DO:
              IF iOptionCode <> 0
              THEN mipEnv:Health:maDoctor:getProviderBaseRates(INPUT INTEGER(cProviderNum),    INPUT oAuthorisation:MemNum, INPUT iOptionCode, INPUT dStartDate, OUTPUT cBaseRate, OUTPUT cArsRate).                
              
            END. /* ELSE */
          

          END. /*IF dAuthObj <> 0.00 THEN*/

          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):

            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

            CASE cReturnField:

              WHEN "[ProviderDesc]":U 
              THEN {&AppendReturnValues} doctor.name.

              WHEN "[PrType]":U 
              THEN {&AppendReturnValues} cPrType.

              WHEN "[SubPrType]":U 
              THEN {&AppendReturnValues} cSubPrType.

              WHEN "[GroupProvider]":U THEN 
              DO:
                ASSIGN lProviderGroup = mipEnv:Health:maDoctor:isProviderAValidGroup(doctor.doc-num, TODAY).

                {&AppendReturnValues} STRING(lProviderGroup).
              END. /*WHEN "[GroupProvider]":U THEN */

              WHEN "[DisciplineUpdatable]"
              THEN {&AppendReturnValues} "FALSE".

              WHEN "[ProviderBaseRate]":U 
              THEN {&AppendReturnValues} cBaseRate.

              WHEN "[ProviderArsRate]":U 
              THEN {&AppendReturnValues} cArsRate.
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}    
        END. /*FOR FIRST doctor NO-LOCK:*/
        
        IF NOT CAN-FIND(FIRST doctor NO-LOCK
                        WHERE doctor.doc-num = INTEGER(cProviderNum)) THEN
        DO:
           /*
              provider is not valid,we need to pass back a value to indicate that discipline and sub discipline are still updatable 
           */

          ASSIGN  lSuccess = mipEnv:miExpression:setNVPElement(cReturnFieldList,         /* iopcNameList     */
                                                               cReturnValues,            /* iopcValueList    */
                                                               "[DisciplineUpdatable]",  /* ipcElementName   */
                                                               "TRUE",                   /* ipcElementValue  */
                                                               ",":U,                    /* ipcNameDelimiter */
                                                               "|":U).                   /* ipcValueDelimiter*/

          ASSIGN  ttValidation.lValid        = FALSE
                  ttValidation.cMessage      = SUBSTITUTE("Doctor '&1' not found":U, cProviderNum)
                  ttValidation.cReturnValues = cReturnValues.

          VALIDATE ttValidation.

        END. /* NOT CAN-FIND FIRST doctor NO-LOCK */
      END. /*IF cProviderNum <> "":U THEN */
      ELSE
      DO:      

        ASSIGN  lSuccess = mipEnv:miExpression:setNVPElement(cReturnFieldList,         /* iopcNameList     */
                                                             cReturnValues,            /* iopcValueList    */
                                                             "[DisciplineUpdatable]",  /* ipcElementName   */
                                                             "TRUE",                   /* ipcElementValue  */
                                                             ",":U,                    /* ipcNameDelimiter */
                                                             "|":U).                   /* ipcValueDelimiter*/

        ASSIGN ttValidation.cReturnValues = cReturnValues
               ttValidation.lValid        = TRUE.                                          
                                                                                                                       
        VALIDATE ttValidation.
         
      END. /*ELSE*/
    END. /*WHEN "Provider":U THEN*/
    
    WHEN "AttendingProvider":U THEN
    DO:
      IF cAttProviderNum <> "":U AND TRIM(cAttProviderNum) <> "0":U THEN 
      DO:

        /*
          If a group provider and an attending provider has been specified ensure that the attending provider is valid for the group
        */
        ASSIGN lSuccess = mipEnv:Health:maDoctor:validateProviderInGroup(INPUT  INTEGER(cProviderNum),
                                                                         INPUT  INTEGER(cAttProviderNum),
                                                                         INPUT  dStartDate,
                                                                         OUTPUT lValidGroup,
                                                                         OUTPUT cValidProvider,
                                                                         OUTPUT cErrorMessage).
                                                                         
        IF lValidGroup AND ( cValidProvider = "Valid":U OR cValidProvider = "":U ) THEN
        DO:
          FOR FIRST doctor NO-LOCK
              WHERE doctor.doc-num = INTEGER(cAttProviderNum): 
              
            ASSIGN cDiscipline  = mipEnv:Health:maDoctor:getProviderDiscipline( INPUT doctor.doc-num, INPUT  dStartDate, INPUT  "BOTH" )  
                   cPrType      = IF NUM-ENTRIES(cDiscipline,"|":U) = 2 THEN ENTRY(1,cDiscipline,"|":U) ELSE "":U
                   cSubPrType   = IF NUM-ENTRIES(cDiscipline,"|":U) = 2 THEN ENTRY(2,cDiscipline,"|":U) ELSE "":U. 

            IF dAuthObj <> 0.00 THEN
            DO:
            
              ASSIGN iOptionCode = 0.
              
              mipEnv:Health:AuthService:getAuthObject
                ( INPUT  dAuthObj,
                  INPUT  "":U,
                  OUTPUT oAuthorisation ).
            
              IF oAuthorisation:InFocus THEN
              DO:
                ASSIGN iOptionCode = oAuthorisation:OptionCode.
            
                IF iOptionCode = 0
                THEN mipEnv:Health:maMember:getMemberOption(INPUT oAuthorisation:MemNum, INPUT oAuthorisation:StartDate, OUTPUT iOptionCode).
            
                IF iOptionCode <> 0
                THEN mipEnv:Health:maDoctor:getProviderBaseRates(INPUT INTEGER(cAttProviderNum), INPUT oAuthorisation:MemNum, INPUT iOptionCode, OUTPUT cBaseRate, OUTPUT cArsRate).  
                
              END. /*IF oAuthorisation:InFocus THEN*/
            
            END. /*IF dAuthObj <> 0.00 THEN*/

            DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
            
              ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
              
              CASE cReturnField:
              
                when "[AttProviderNum]":U
                then {&AppendReturnValues} STRING(doctor.doc-num).

                WHEN "[ProviderDesc]":U 
                THEN {&AppendReturnValues} doctor.name.
                
                WHEN "[PrType]":U 
                THEN {&AppendReturnValues} cPrType.
                
                WHEN "[SubPrType]":U 
                THEN {&AppendReturnValues} cSubPrType.
                
                WHEN "[DisciplineUpdatable]"
                THEN {&AppendReturnValues} "FALSE".   

                WHEN "[ProviderBaseRate]"
                THEN {&AppendReturnValues} cBaseRate.   

                WHEN "[ProviderArsRate]"
                THEN {&AppendReturnValues} cArsRate.      
                              
              END CASE.                                                                      
            END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
            
            {&ValidationSuccess}    
          END.  /* FOR FIRST doctor NO-LOCK... */
        END. /* IF lValidGroup AND ( cValidProvider = "Valid":U... */
        ELSE IF NOT lValidGroup THEN
        DO:

          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[AttProviderNum]":U
              THEN {&AppendReturnValues} STRING(cAttProviderNum).

            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}
           /*
              provider is not valid,we need to pass back a value to indicate that discipline and sub discipline are still updatable 
           */

          ASSIGN  lSuccess = mipEnv:miExpression:setNVPElement(cReturnFieldList,         /* iopcNameList     */
                                                               cReturnValues,            /* iopcValueList    */
                                                               "[DisciplineUpdatable]",  /* ipcElementName   */
                                                               "TRUE",                   /* ipcElementValue  */
                                                               ",":U,                    /* ipcNameDelimiter */
                                                               "|":U).                   /* ipcValueDelimiter*/
          ASSIGN 
            cMessageError              = IF cErrorMessage <> "":U 
                                         THEN cErrorMessage  // This message may contain a rule code
                                         ELSE SUBSTITUTE("The 'Group Practice &1' specified is invalid.":U, cProviderNum)
            ttValidation.lValid        = FALSE
            ttValidation.cMessage      = mipEnv:Health:maUiService:formatAjaxHelpMessage(INPUT cMessageError) // Use this service to remove possible 'Help='-string added in validateProviderInGroup()
            ttValidation.cReturnValues = cReturnValues.
                 
          VALIDATE ttValidation. 
 
        END.  /* IF NOT lValidGroup THEN */
        ELSE  
        DO:

          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[AttProviderNum]":U
              THEN {&AppendReturnValues} STRING(cAttProviderNum).

            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}  
           /*
              provider is not valid,we need to pass back a value to indicate that discipline and sub discipline are still updatable 
           */

          ASSIGN  lSuccess = mipEnv:miExpression:setNVPElement(cReturnFieldList,         /* iopcNameList     */
                                                               cReturnValues,            /* iopcValueList    */
                                                               "[DisciplineUpdatable]",  /* ipcElementName   */
                                                               "TRUE",                   /* ipcElementValue  */
                                                               ",":U,                    /* ipcNameDelimiter */
                                                               "|":U).                   /* ipcValueDelimiter*/
          
          ASSIGN 
            cMessageError              = IF cErrorMessage <> "":U 
                                         THEN cErrorMessage
                                         ELSE SUBSTITUTE("The attending provider &1 is not valid for the group practice &2.",cAttProviderNum, cProviderNum)
            ttValidation.lValid        = FALSE
            ttValidation.cMessage      = mipEnv:Health:maUiService:formatAjaxHelpMessage(INPUT cMessageError) // Use this service to remove possible 'Help='-string added in validateProviderInGroup()
            ttValidation.cReturnValues = cReturnValues.
                 
          VALIDATE ttValidation. 
        END.  /* ELSE */
      END. /* IF cAttProviderNum <> "":U THEN */
      ELSE 
      DO:      
        ASSIGN  lSuccess = mipEnv:miExpression:setNVPElement(cReturnFieldList,         /* iopcNameList     */
                                                             cReturnValues,            /* iopcValueList    */
                                                             "[DisciplineUpdatable]",  /* ipcElementName   */
                                                             "TRUE",                   /* ipcElementValue  */
                                                             ",":U,                    /* ipcNameDelimiter */
                                                             "|":U).                   /* ipcValueDelimiter*/

        ASSIGN ttValidation.cReturnValues = cReturnValues
               ttValidation.lValid        = TRUE.                                          
                                                                                                                       
        VALIDATE ttValidation.
      
      END. /* ELSE: IF cAttProviderNum <> "":U... */
    END. /* WHEN "AttendingProvider":U THEN */
    
    WHEN "ProviderType":U THEN
    DO:
      ASSIGN lSuccess = mipEnv:Health:AuthService:getAuthObject
                          ( INPUT  dAuthObj,
                            INPUT  "":U,
                            OUTPUT oAuthorisation )
      
             dStartDate     = (IF dStartDate = ? THEN oAuthorisation:StartDate ELSE dStartDate)             
             iOptionCode    = oAuthorisation:MemberOptionCode.

      IF iOptionCode <> 0 THEN
        mipEnv:Health:maDoctor:getProviderBaseRates(INPUT INTEGER(cProviderNum), INPUT oAuthorisation:MemNum, INPUT iOptionCode, INPUT dStartDate, OUTPUT cBaseRate, OUTPUT cArsRate).    
            
      IF cProviderType <> "":U THEN
      DO:
                 
        DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
        
          ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
          
          CASE cReturnField:    
           
            WHEN "[Configuration]":U THEN 
            DO:
              
              RUN _getConfiguration IN TARGET-PROCEDURE ( INPUT  oAuthorisation:AuthTypeObj, 
                                                          INPUT  oAuthorisation:InsurerObj,
                                                          INPUT  iOptionCode,
                                                          INPUT  dStartDate,
                                                          INPUT  cProviderType,    
                                                          INPUT  iNegNum,
                                                          INPUT  cPrType,
                                                          INPUT  cSubPrType,       
                                                          OUTPUT lcConfiguration).
              
              {&AppendReturnValues} STRING(lcConfiguration).
            END. /*WHEN "[Configuration]":U THEN */

            WHEN "[ProviderBaseRate]":U
            THEN {&AppendReturnValues} cBaseRate.   

            WHEN "[ProviderArsRate]":U
            THEN {&AppendReturnValues} cArsRate.      
          END CASE.                                                                      
        END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
        
        {&ValidationSuccess}    

      END. /* IF cProviderType <> "":U THEN */
      ELSE
      DO:      
        {&BlankResponse}       
      END. /* ELSE: IF cProviderType <> "":U THEN*/ 
    END. /* WHEN "ProviderType":U THEN */
    
    WHEN "Discipline":U OR WHEN "SubDiscipline":U THEN
    DO:
      ASSIGN iDiscipline    = INTEGER(cPrType)
             iSubDiscipline = INTEGER(cSubPrType).
         
      IF iDiscipline    <> 0 
      OR iSubDiscipline <> 0 THEN 
      DO:
        mipEnv:Health:maAdministration:validateDiscipline(INPUT  STRING(iDiscipline),
                                                          INPUT  iSubDiscipline,
                                                          OUTPUT cErrorMessage).
    
        IF cErrorMessage = "":U THEN 
        DO:
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[PrType]":U THEN 
              DO:
                { ma/msc/madispad.i &discipline = cPrType 
                                    &comment    = /* */ }
                {&AppendReturnValues} cPrType.
              END. /* WHEN "[PrType]":U THEN */
              
              WHEN "[SubPrType]":U THEN 
              DO:
                { ma/msc/madispad.i &discipline = cSubPrType 
                                    &comment    = /* */ } 
                {&AppendReturnValues} cSubPrType.
              END. /* WHEN "[SubPrType]":U THEN */
            
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}    
        END. /* IF cErrorMessage = "":U */
        ELSE DO:
          mipEnv:miUtility:getMessageText(INPUT "ma":U, 
                                          INPUT STRING(100), // The "&1" specified is invalid
                                          INPUT "ENG":U, 
                                          OUTPUT cMessageText, 
                                          OUTPUT cMessageDescription, 
                                          OUTPUT cMessageType).
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE(cMessageText,cErrorMessage).
                 
          VALIDATE ttValidation.       
        END. /* ELSE: IF cErrorMessage = "":U THEN */
      END. /* IF iDiscipline <> 0 THEN */
      ELSE
      DO:      
        {&BlankResponse}       
      END. /* ELSE: IF iDiscipline <> 0 THEN */
    END. /* WHEN "Discipline":U THEN */
    
    WHEN "Status":U THEN
    DO:
      ASSIGN lSuccess = mipEnv:Health:AuthService:getAuthObject
                          ( INPUT  dAuthObj,
                            INPUT  "":U,
                            OUTPUT oAuthorisation )
      
             dStartDate     = (IF dStartDate = ? 
                               THEN oAuthorisation:StartDate 
                               ELSE dStartDate)             
                               
             dInsurerObj    = oAuthorisation:InsurerObj
                               
             iOptionCode    = oAuthorisation:MemberOptionCode.
             
             
      DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
      
        ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
      
        CASE cReturnField:
        
          WHEN "[StatusReasonMandatory]":U THEN
          DO:
            ASSIGN lMandatory = (IF TRIM(cStatus) = "":U
                                 THEN FALSE
                                 ELSE mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), INPUT dInsurerObj, INPUT iOptionCode, INPUT dStartDate)).
        
            {&AppendReturnValues} STRING(lMandatory). 
          END. /*WHEN "[StatusReasonMandatory]":U THEN*/
        END CASE.
      END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   
      
      {&ValidationSuccess}       
    END. /*WHEN "Status":U THEN*/
    
    WHEN "Reason":U THEN
    DO:
      IF cReasonCode <> "":U THEN
      DO:
        /* 
          Reason type will be determined in gesStatusReasonDesc
        */
        ASSIGN cReasonType = "":U .

        mipEnv:health:AuthService:getStatusReasonDesc(INPUT 0.0,                 
                                                      INPUT 0,                   
                                                      INPUT dStartDate,          
                                                      INPUT INTEGER(cStatus),    
                                                      INPUT cReasonCode ,         
                                                      INPUT-OUTPUT cReasonType,        
                                                      OUTPUT cReasonDescription ,  
                                                      OUTPUT cErrorMessage ).     
                                                      
        
        IF cErrorMessage =  "":U  THEN 
        DO:
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[ReasonCode]":U THEN
              DO:
                {&AppendReturnValues} cReasonCode. 
              END. /*WHEN "[ReasonCode]":U THEN*/
              
              WHEN "[ReasonDesc]":U THEN
              DO:
                {&AppendReturnValues} cReasonDescription. 
              END. /*WHEN "[ReasonDesc]":U THEN*/
              WHEN "[ReasonType]":U  THEN
              DO:
                {&AppendReturnValues} cReasonType. 
              END. /* WHEN [ReasonType] */
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   
          
          ASSIGN ttValidation.cReturnValues = cReturnValues 
                 ttValidation.lValid        = TRUE          
                 ttValidation.cMessage      = cReasonDescription.     
        END. /*IF cErrorMessage =  "":U  THEN */
        ELSE
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = mipEnv:Health:maUiService:formatAjaxHelpMessage(INPUT cErrorMessage).
                 
          VALIDATE ttValidation.
        END. /*ELSE*/      
      END. /*IF cReasonCode <> "":U THEN*/
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END.  /* WHEN "Reason":U THEN */
    WHEN "OverrideNote":U THEN
    DO:

      /* 
        Reason type will be determined in gesStatusReasonDesc
      */
      mipEnv:health:maUtility:getNoteDescription(INPUT cReasonCode,
                                                 INPUT cReasonType,
                                                 OUTPUT cReasonDescription) .
 
      IF cReasonDescription <>  "":U  THEN 
      DO:
        DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
        
          ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
          
          CASE cReturnField:
          
            WHEN "[ReasonCode]":U THEN
            DO:
              {&AppendReturnValues} cReasonCode. 
            END. /*WHEN "[ReasonCode]":U THEN*/
            
            WHEN "[ReasonDesc]":U THEN
            DO:
              {&AppendReturnValues} cReasonDescription. 
            END. /*WHEN "[ReasonDesc]":U THEN*/
            WHEN "[ReasonType]":U  THEN
            DO:
              {&AppendReturnValues} cReasonType. 
            END. /* WHEN [ReasonType] */
          END CASE.                                                                      
        END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   
        
        ASSIGN ttValidation.cReturnValues = cReturnValues 
               ttValidation.lValid        = TRUE          
               ttValidation.cMessage      = cReasonDescription.     
      END. /* IF cReasonDescription <>  "":U  THEN */
      ELSE
      DO:
         {&BlankResponse} 
      END. /*ELSE*/      
    END.  /* WHEN "OverrideNote":U THEN */    
  END CASE.
  
  { mip/inc/mipcatcherror.i &FINALLY = "EMPTY TEMP-TABLE ttAuthTypeConfig.
  
                                        IF VALID-OBJECT(oSearch)        THEN DELETE OBJECT oSearch."}

  

