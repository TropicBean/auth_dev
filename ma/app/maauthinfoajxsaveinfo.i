/* maauthinfoajxsaveinfo.i MEDSTAR Medical Aid System
                         Auto save Auth Procedure
                         (c) Copyright 1990 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/                                                                              
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
  
  DEFINE VARIABLE oRequestHelper         AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper        AS cls.maajaxresponsehelper   NO-UNDO.
  DEFINE VARIABLE oAuthorisation         AS cls.maauthorisation        NO-UNDO.
                                        
  DEFINE VARIABLE hErrorHandle           AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cAuthContainerCode     AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cBodyRegion            AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cClaimType             AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cErrorMessage          AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cServiceType           AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cFinContainerCode      AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cMemberNumber          AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cNOK                   AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cPmbContainerCode      AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cReason                AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cReferenceNumber       AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRequestedBy           AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRequestedSource       AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRequestedSourceDetail AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction          AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cSavContainerCode      AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cTriggerField          AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cDependantReference    AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE lDiscountType          AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE lEndAmPm               AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE lSuccess               AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE lStartAmPm             AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE lPmbIndicator          AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE lPmbPayCost            AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE dAuthObj               AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dInsurerObj            AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE iClaimCode             AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iDependant             AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iOptionCode            AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iStatus                AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iMouthPartId           AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE dAuthDate              AS DATE                       NO-UNDO.
  DEFINE VARIABLE dCreateDate            AS DATE                       NO-UNDO.
  DEFINE VARIABLE dDueDate               AS DATE                       NO-UNDO.
  DEFINE VARIABLE dEndDate               AS DATE                       NO-UNDO.
  DEFINE VARIABLE dRequestDate           AS DATE                       NO-UNDO.
  DEFINE VARIABLE dStartDate             AS DATE                       NO-UNDO.
  DEFINE VARIABLE dAdjustmentAuth        AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAdjustmentPrivateAuth AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAuthEpisodeObj        AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj           AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAmountAuth            AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAmountRequested       AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAmountInterim         AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAmountClaimed         AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dCopayAuth             AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dDiscountAuth          AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dQuantityRequested     AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dQuantityAuth          AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dPmbBenefit            AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dPmbValue              AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dTotalLos              AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE lInFocus               AS LOGICAL                    NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE iOldAuthStatus         AS INTEGER                    NO-UNDO.
  
  ASSIGN
     cAuthContainerCode     = "AuthContainer":U
     cPmbContainerCode      = "PmbFinancialHolderContainer":U
     cFinContainerCode      = "FinancialFinancialHolderContainer":U
     cSavContainerCode      = "SavingsFinancialHolderContainer":U
     oRequestHelper         = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
     oResponseHelper        = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
                            
     cReferenceNumber       =         oRequestHelper:getFieldValue("fcReferenceNumber":U       + cAuthContainerCode)
     cRequestedBy           =         oRequestHelper:getFieldValue("cbRequestedBy":U           + cAuthContainerCode)
     cRequestedSource       =         oRequestHelper:getFieldValue("cbRequestedSource":U       + cAuthContainerCode)
     cRequestedSourceDetail =         oRequestHelper:getFieldValue("fcRequestedSourceDetail":U + cAuthContainerCode)
     cServiceType           =         oRequestHelper:getFieldValue("cbServiceType":U           + cAuthContainerCode)
     cNOK                   =         oRequestHelper:getFieldValue("fcNextOfKin":U             + cAuthContainerCode)
     cRecordAction          =         oRequestHelper:getFieldValue("fcAction":U                + cAuthContainerCode)
     cDependantReference    =         oRequestHelper:getFieldValue("fcDependantReference":U    + cAuthContainerCode)

     cClaimType             =         oRequestHelper:getFieldValue("cbClaimType":U             + cAuthContainerCode)
     cBodyRegion            =         oRequestHelper:getFieldValue("cbBodyRegion":U            + cAuthContainerCode)  
                                                                                              
     cMemberNumber          =         oRequestHelper:getFieldValue("_memberNumber":U           + cAuthContainerCode)
     
     dInsurerObj            = DECIMAL(oRequestHelper:getFieldValue("cbClient":U                + cAuthContainerCode))                 
     dAuthTypeObj           = DECIMAL(oRequestHelper:getFieldValue("fdAuthTypeObj":U           + cAuthContainerCode))
     dAuthEpisodeObj        = DECIMAL(oRequestHelper:getFieldValue("fdAuthEpisodeObj":U        + cAuthContainerCode))                       
     dAuthObj               = DECIMAL(oRequestHelper:getFieldValue("_authObj":U                + cAuthContainerCode))
                                                                                              
     iOptionCode            = INTEGER(oRequestHelper:getFieldValue("cbOptionCode":U            + cAuthContainerCode))
     iClaimCode             = INTEGER(oRequestHelper:getFieldValue("fiClaimCode":U             + cAuthContainerCode))
     cReason                =         oRequestHelper:getFieldValue("fcReason":U                + cAuthContainerCode)
     iStatus                = INTEGER(oRequestHelper:getFieldValue("cbStatus":U                + cAuthContainerCode))
     iDependant             = INTEGER(oRequestHelper:getFieldValue("fiDependant":U             + cAuthContainerCode))
     iMouthPartId           = INTEGER(oRequestHelper:getFieldValue("fiMouthPartId":U           + cAuthContainerCode))
                            
     dRequestDate           = (IF oRequestHelper:getFieldValue("fdRequestDate":U + cAuthContainerCode) MATCHES "*YYYY*":U THEN TODAY ELSE DATE(oRequestHelper:getFieldValue("fdRequestDate":U + cAuthContainerCode)))
     dCreateDate            = (IF oRequestHelper:getFieldValue("fdCreateDate":U  + cAuthContainerCode) MATCHES "*YYYY*":U THEN TODAY ELSE DATE(oRequestHelper:getFieldValue("fdCreateDate":U  + cAuthContainerCode)))
     dAuthDate              = (IF oRequestHelper:getFieldValue("fdCreateDate":U  + cAuthContainerCode) MATCHES "*YYYY*":U THEN TODAY ELSE DATE(oRequestHelper:getFieldValue("fdCreateDate":U  + cAuthContainerCode))) 
     dDueDate               = (IF oRequestHelper:getFieldValue("fdDueDate":U     + cAuthContainerCode) MATCHES "*YYYY*":U THEN TODAY ELSE DATE(oRequestHelper:getFieldValue("fdDueDate":U     + cAuthContainerCode)))
     dStartDate             = (IF oRequestHelper:getFieldValue("fdStartDate":U   + cAuthContainerCode) MATCHES "*YYYY*":U THEN TODAY ELSE DATE(oRequestHelper:getFieldValue("fdStartDate":U   + cAuthContainerCode)))
     dEndDate               = (IF oRequestHelper:getFieldValue("fdEndDate":U     + cAuthContainerCode) MATCHES "*YYYY*":U THEN ?     ELSE DATE(oRequestHelper:getFieldValue("fdEndDate":U     + cAuthContainerCode)))
                            
     lStartAmPm             = (IF CAN-DO("AM,PM":U,oRequestHelper:getFieldValue("cbStartAmPm":U + cAuthContainerCode))
                               THEN oRequestHelper:getFieldValue("cbStartAmPm":U + cAuthContainerCode) = "AM":U
                               ELSE ?)
                               
     lEndAmPm              = (IF CAN-DO("AM,PM":U,oRequestHelper:getFieldValue("cbEndAmPm":U + cAuthContainerCode))
                               THEN oRequestHelper:getFieldValue("cbEndAmPm":U + cAuthContainerCode) = "AM":U
                               ELSE ?)                               
     
     dAmountRequested       = DECIMAL(oRequestHelper:getFieldValue("fdAmountRequested":U    + cFinContainerCode))
     dQuantityRequested     = DECIMAL(oRequestHelper:getFieldValue("fdQuantityRequested":U  + cFinContainerCode))
     dAmountAuth            = DECIMAL(oRequestHelper:getFieldValue("fdAmountAuthorised":U   + cFinContainerCode))
     dQuantityAuth          = DECIMAL(oRequestHelper:getFieldValue("fdQuantityAuthorised":U + cFinContainerCode))
     dDiscountAuth          = DECIMAL(oRequestHelper:getFieldValue("fdDiscountAuthorised":U + cFinContainerCode))
     dCopayAuth             = DECIMAL(oRequestHelper:getFieldValue("fdCopayAuthorised":U    + cFinContainerCode))
     dAmountInterim         = DECIMAL(oRequestHelper:getFieldValue("fdAmountInterim":U      + cFinContainerCode)) 
     dAmountClaimed         = DECIMAL(oRequestHelper:getFieldValue("fdAmountClaimed":U      + cFinContainerCode))
     dTotalLos              = DECIMAL(oRequestHelper:getFieldValue("fdTotalLos":U           + cAuthContainerCode)) 
   
     lDiscountType          = (IF CAN-DO("P,R":U,oRequestHelper:getFieldValue("fcDiscountType":U + cFinContainerCode))
                               THEN oRequestHelper:getFieldValue("fcDiscountType":U + cFinContainerCode) = "P":U
                               ELSE ?)
     
     dPmbBenefit            = DECIMAL(oRequestHelper:getFieldValue("fdPmbBenefit":U + cPmbContainerCode))
     dPmbValue              = DECIMAL(oRequestHelper:getFieldValue("fdPmbValue":U   + cPmbContainerCode))
     
     lPmbIndicator          = CAN-DO("YES,Y,TRUE":U, oRequestHelper:getFieldValue("flPmbIndicator":U + cPmbContainerCode))
     lPmbPayCost            = CAN-DO("YES,Y,TRUE":U, oRequestHelper:getFieldValue("flPmbPayCost":U   + cPmbContainerCode))
     
     dAdjustmentAuth        = DECIMAL(oRequestHelper:getFieldValue("fdCurrentAuth":U  + cSavContainerCode))
     dAdjustmentPrivateAuth = DECIMAL(oRequestHelper:getFieldValue("fdPreviousAuth":U + cSavContainerCode))

     cTriggerField          = oRequestHelper:TriggerField

    NO-ERROR.
  
   
  IF NOT {&ErrorStatus} THEN
  DO:
    CASE cRecordAction:
      
      WHEN "modify":U THEN
      DO:
        EMPTY TEMP-TABLE ttAuthStatus.
        
        mipEnv:Health:AuthService:getStatusTable(INPUT "System":U, OUTPUT TABLE ttAuthStatus).
  
        mipEnv:Health:AuthService:getAuthObject
          ( INPUT  dAuthObj,
            INPUT  "":U,
            OUTPUT oAuthorisation ).

        ASSIGN
          lInFocus       = oAuthorisation:InFocus
          iOldAuthStatus = oAuthorisation:AuthStatus.

        ASSIGN
           oAuthorisation:AuthEpisodeObj        = dAuthEpisodeObj WHEN NOT oAuthorisation:InFocus AND dAuthEpisodeObj <> 0.00
           oAuthorisation:MemNum                = cMemberNumber
           oAuthorisation:Dependant             = iDependant
           oAuthorisation:DependantReference    = cDependantReference
           oAuthorisation:ReferenceAuthNum      = cReferenceNumber
           oAuthorisation:RequestBy             = cRequestedBy
           oAuthorisation:RequestSource         = cRequestedSource
           oAuthorisation:RequestSourceDetails  = cRequestedSourceDetail
           oAuthorisation:ServiceType           = cServiceType
           oAuthorisation:ClaimType             = cClaimType
           oAuthorisation:BodyRegion            = cBodyRegion
           oAuthorisation:AuthStatusNote        = cReason
                                                
           oAuthorisation:InsurerObj            = dInsurerObj
           oAuthorisation:AuthTypeObj           = dAuthTypeObj
                                                
           oAuthorisation:OptionCode            = IF dAuthObj <> 0 THEN iOptionCode ELSE 0 
           oAuthorisation:ClaimCode             = iClaimCode
           oAuthorisation:AuthStatus            = iStatus
           oAuthorisation:MouthPartId[1]        = iMouthPartId
                                                
           oAuthorisation:RequestDate           = dRequestDate
           oAuthorisation:DueDate               = dDueDate
           oAuthorisation:StartDate             = dStartDate
           oAuthorisation:EndDate               = dEndDate
           oAuthorisation:AuthDate              = dAuthDate
                                                
           oAuthorisation:StartAmPm             = lStartAmPm
           oAuthorisation:EndAmPm               = lEndAmPm
           
           oAuthorisation:DiscountType          = lDiscountType
           oAuthorisation:PmbIndicator          = lPmbIndicator
           oAuthorisation:PmbPayCost            = lPmbPayCost 
            
           oAuthorisation:AmountRequested       = dAmountRequested      
           oAuthorisation:QuantityRequested     = dQuantityRequested    
           oAuthorisation:AmountAuth            = dAmountAuth           
           oAuthorisation:QuantityAuth          = dQuantityAuth         
           oAuthorisation:DiscountAuth          = dDiscountAuth         
           oAuthorisation:AmountInterim         = dAmountInterim        
           oAuthorisation:AmountClaimed         = dAmountClaimed        
           oAuthorisation:PmbBenefit            = dPmbBenefit           
           oAuthorisation:PmbValue              = dPmbValue             
           oAuthorisation:AdjustmentAuth        = dAdjustmentAuth       
           oAuthorisation:AdjustmentPrivateAuth = dAdjustmentPrivateAuth

           oAuthorisation:AuthStatusUpdated     = IF lInFocus
                                                  AND (iOldAuthStatus <> iStatus) THEN TRUE ELSE FALSE
           
           oAuthorisation:NextOfKin             = cNOK
           oAuthorisation:TotalLos              = dTotalLos
           oAuthorisation:AuthIncomplete        = (IF dAuthObj <= 0 THEN TRUE ELSE oAuthorisation:AuthIncomplete)
           lSuccess                             = oAuthorisation:saveRecord()            
          NO-ERROR.               

        IF NOT {&ErrorStatus} AND NOT oAuthorisation:ErrorObject:ErrorsExist THEN
        DO:
          /*
            Building gcOption with OptionCode and scheme name to render together with AuthStatus, etc.  
            This is to ensure that the OptionCode is displayed on the screen whether clicking/tabbing through the fields.
          */  
          ASSIGN gcOption = STRING(oAuthorisation:OptionCode,{ma/cus/maschopt9.i}).
                 
          FIND FIRST scheme NO-LOCK                                                                                                                           
               WHERE scheme.scheme-code = oAuthorisation:OptionCode 
            NO-ERROR.  
               
          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}  
          
          ASSIGN gcOption = gcOption + IF AVAILABLE scheme THEN " - " + scheme.short-name ELSE "".                                                                                                                               

          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U 
            lSuccess                        = oResponseHelper:addFieldValue("_authObj":U              + cAuthContainerCode, STRING(oAuthorisation:AuthObj),TRUE)
            lSuccess                        = oResponseHelper:addFieldValue("fdAuthEpisodeObj":U      + cAuthContainerCode, STRING(oAuthorisation:AuthEpisodeObj))
            lSuccess                        = oResponseHelper:addFieldValue("fcEpisodeNumber":U       + cAuthContainerCode, oAuthorisation:AuthEpisodeNum)
            lSuccess                        = oResponseHelper:addFieldValue("fcAuthorisationNumber":U + cAuthContainerCode, oAuthorisation:AuthNum)
            lSuccess                        = oResponseHelper:addFieldValue("fcReason":U              + cAuthContainerCode, oAuthorisation:AuthStatusNote)
            lSuccess                        = oResponseHelper:addFieldValue("fcReasonDesc":U          + cAuthContainerCode, oAuthorisation:AuthStatusNoteDescription, TRUE)
            lSuccess                        = oResponseHelper:addFieldValue("cbStatusDisp":U          + cAuthContainerCode, STRING(oAuthorisation:AuthStatus))
            lSuccess                        = oResponseHelper:addFieldValue("cbServiceType":U         + cAuthContainerCode, STRING(oAuthorisation:ServiceType))
            lSuccess                        = oResponseHelper:addFieldValue("fcOptionCode":U          + cAuthContainerCode, gcOption)
            lSuccess                        = oResponseHelper:addFieldValue("fdAmountAuthorised":U    + cFinContainerCode,  STRING(oAuthorisation:AmountAuth,"->,>>>,>>9.99"))
            lSuccess                        = oResponseHelper:addFieldValue("fdQuantityAuthorised":U  + cFinContainerCode,  STRING(oAuthorisation:QuantityAuth))
           NO-ERROR.            

          FIND FIRST ttAuthStatus NO-LOCK
               WHERE ttAuthStatus.status_code = oAuthorisation:AuthStatus
            NO-ERROR.
            
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE ttAuthStatus 
          THEN 
            ASSIGN lSuccess = oResponseHelper:addFieldValue("cbSystemStatus":U + cAuthContainerCode, STRING(ttAuthStatus.status_code)).
          
          IF cTriggerField = "fiDependant":U + cAuthContainerCode OR cTriggerField = "fdStartDate":U + cAuthContainerCode 
          OR cTriggerField = "fdEndDate":U + cAuthContainerCode   OR oAuthorisation:AuthObj > 0 
          THEN DO:
            ASSIGN
                 lSuccess  = oResponseHelper:addFieldValue("_triggerDuplicateCheck":U  + cAuthContainerCode, "TRUE":U, TRUE).
          END.
        END. /*IF NOT {&ErrorStatus} AND NOT oAuthorisation:ErrorObject:ErrorsExist THEN*/              
      END. /* WHEN "modify":U THEN */
      
      
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

  ASSIGN cErrorMessage = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, "DESCRIPTION,STACK":U) NO-ERROR.   
  {&ResetError}

  IF cErrorMessage <> "":U OR oAuthorisation:ErrorObject:ErrorsExist THEN
  DO:
    ASSIGN 
      oResponseHelper:RequestValid    = FALSE

      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(oAuthorisation:ErrorObject)

      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = cErrorMessage
     NO-ERROR.
      
  END.   /*IF oAuthorisation:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
    
  ASSIGN lSuccess = oResponseHelper:setError(oAuthorisation:ErrorObject) NO-ERROR.
  
  { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  

  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "EMPTY TEMP-TABLE ttAuthStatus.
  
                                        IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthorisation)  THEN DELETE OBJECT oAuthorisation  NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }
 



