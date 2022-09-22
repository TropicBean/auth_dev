/* maauthdetailajxsavedetail.i  MEDSTAR Medical Aid System
                               Auth Detail Container Ajax Save
                               maauthdetailuiservicestack.p -> ajaxVSaveDetailContainer
                               (c) Copyright 1990 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/   
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE oRequestHelper        AS cls.maajaxrequesthelper   NO-UNDO.
  DEFINE VARIABLE oResponseHelper       AS cls.maajaxresponsehelper  NO-UNDO.
  DEFINE VARIABLE oDetail               AS cls.maauthorisationdetail NO-UNDO.
  DEFINE VARIABLE cOverrideArsRate      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOverrideBaseRate     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDefaultArsRate       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDefaultBaseRate      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBodyRegion           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cClaimType            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cContainerCode        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cErrorMessage         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cLineRestriction      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cLOSCalcRule          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNote                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningKey            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningAltValue       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOEM                  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPrintDM              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPayeeDM              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthStatusNote       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRecordAction         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cReasonDescription    AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cTariffReason         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCopayOverrideNote    AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE iClaimCode            AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iLineNumber           AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iLineSequence         AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iStatus               AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE dStartDate            AS DATE                      NO-UNDO.
  DEFINE VARIABLE dEndDate              AS DATE                      NO-UNDO.
  DEFINE VARIABLE dAmountReq            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAmountAuth           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAmountPaid           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthObj              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj        AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj      AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCopayAuth            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCopayPaid            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCPTObj               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dDiscountAuth         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dDiscountPaid         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dItemCost             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dPMBBenefit           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dPMBValue             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dOwningObj            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyAuth              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyLos               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyPaid              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyReq               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTariffTypeObj        AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dMinutesTariffTypeObj AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTariffCost           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dFixedItemCost        AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE lAddedByUser          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lCreated              AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lDiscountType         AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lEndAmPm              AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lStartAmPm            AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lStartDateAmPmUpdated AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lEndDateAmPmUpdated   AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lClaimCodeUpdated     AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lClaimTypeUpdated     AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lTriggerRebuild       AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE rRowid                AS ROWID                     NO-UNDO.
  DEFINE VARIABLE cRelatedEntity        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRelatedCode          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNappiRequired        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dRelatedObj           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE lAltValueChanged      AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lPreserveRelated      AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lRepeatItem           AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleAuth      AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCyclePaid      AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleQuantity  AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleDays      AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleGraceDays AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iMinutesAuth          AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iMinutesRequested     AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE dRepeatLastClaimDate  AS DATE                      NO-UNDO.
  
  ASSIGN
     oRequestHelper        = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
     oResponseHelper       = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
                           
     cContainerCode        = ipcValidationArgument
                           
     cRecordAction         =          oRequestHelper:getFieldValue("fcAction":U            + cContainerCode)
     cOEM                  =          oRequestHelper:getFieldValue("cbOEM":U               + cContainerCode)
     cOwningAltValue       =          oRequestHelper:getFieldValue("fcOwningAltValue":U    + cContainerCode)
     cClaimType            =          oRequestHelper:getFieldValue("cbClaimType":U         + cContainerCode)
     cLineRestriction      =          oRequestHelper:getFieldValue("cbLineRestriction":U   + cContainerCode)
     cLOSCalcRule          =          oRequestHelper:getFieldValue("fcLosCalcRule":U       + cContainerCode)
     cBodyRegion           =          oRequestHelper:getFieldValue("cbBodyRegion":U        + cContainerCode)
     cPrintDM              =          oRequestHelper:getFieldValue("cbPrintDM":U           + cContainerCode)
     cPayeeDM              =          oRequestHelper:getFieldValue("cbPayeeDM":U           + cContainerCode)
     cNote                 =          oRequestHelper:getFieldValue("fcNote":U              + cContainerCode)
     cCopayOverrideNote    =          oRequestHelper:getFieldValue("fcCopayOverrideNote":U + cContainerCode)
     cAuthStatusNote       =          oRequestHelper:getFieldValue("fcReason":U            + cContainerCode)
     cOverrideBaseRate     =          oRequestHelper:getFieldValue("fcOverrideBaseRate":U  + cContainerCode)
     cOverrideArsRate      =          oRequestHelper:getFieldValue("fcOverrideArsRate":U   + cContainerCode)
     cDefaultBaseRate      =          oRequestHelper:getFieldValue("fcDefaultBaseRate":U   + cContainerCode)
     cDefaultArsRate       =          oRequestHelper:getFieldValue("fcDefaultArsRate":U    + cContainerCode)
     cTariffReason         =          oRequestHelper:getFieldValue("fcTariffReason":U      + cContainerCode)
     cNappiRequired    =          oRequestHelper:getFieldValue("fcAuthNappiRequired":U + cContainerCode)
                           
     rRowid                = TO-ROWID(oRequestHelper:getFieldValue("frOwningRowid":U       + cContainerCode))
                           
     iLineNumber           =  INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U        + cContainerCode))
     iStatus               =  INTEGER(oRequestHelper:getFieldValue("cbStatus":U            + cContainerCode))
     iClaimCode            =  INTEGER(oRequestHelper:getFieldValue("fiClaimCode":U         + cContainerCode))
     iLineSequence         =  INTEGER(oRequestHelper:getFieldValue("fiLocSequence":U       + cContainerCode))
     iMinutesAuth          =  INTEGER(oRequestHelper:getFieldValue("fiMinAuthorised":U     + cContainerCode))               
     iMinutesRequested     =  INTEGER(oRequestHelper:getFieldValue("fiMinRequested":U      + cContainerCode))
                                                                    
     dStartDate            =     DATE(oRequestHelper:getFieldValue("fdStartDate":U         + cContainerCode))
     dEndDate              =       IF oRequestHelper:getFieldValue("fdEndDate":U           + cContainerCode) <> "YYYY/MM/DD" THEN 
                                 DATE(oRequestHelper:getFieldValue("fdEndDate":U           + cContainerCode)) 
                                 ELSE ?
                               
     dAuthGroupObj         =  DECIMAL(oRequestHelper:getFieldValue("fdAuthGroupObj":U      + cContainerCode))
     dAuthDetailObj        =  DECIMAL(oRequestHelper:getFieldValue("fdAuthDetailObj":U     + cContainerCode))
     dAuthProviderObj      =  DECIMAL(oRequestHelper:getFieldValue("cbProvider":U          + cContainerCode))
     dCPTObj               =  DECIMAL(oRequestHelper:getFieldValue("fdCPTObj":U            + cContainerCode))
     dQtyReq               =  DECIMAL(oRequestHelper:getFieldValue("fdQuantityRequested":U + cContainerCode))
     dQtyAuth              =  DECIMAL(oRequestHelper:getFieldValue("fdQuantityAuth":U      + cContainerCode))
     dQtyPaid              =  DECIMAL(oRequestHelper:getFieldValue("fdQuantityPaid":U      + cContainerCode))
     dQtyLos               =  DECIMAL(oRequestHelper:getFieldValue("fdQuantityLos":U       + cContainerCode))
     dPMBBenefit           =  DECIMAL(oRequestHelper:getFieldValue("fdPMBBenefit":U        + cContainerCode))
     dPMBValue             =  DECIMAL(oRequestHelper:getFieldValue("fdPMBValue":U          + cContainerCode))
     dAmountReq            =  DECIMAL(oRequestHelper:getFieldValue("fdAmountRequested":U   + cContainerCode))
     dAmountAuth           =  DECIMAL(oRequestHelper:getFieldValue("fdAmountAuth":U        + cContainerCode))
     dAmountPaid           =  DECIMAL(oRequestHelper:getFieldValue("fdAmountPaid":U        + cContainerCode))
     dDiscountAuth         =  DECIMAL(oRequestHelper:getFieldValue("fdDiscountAuth":U      + cContainerCode))
     dDiscountPaid         =  DECIMAL(oRequestHelper:getFieldValue("fdDiscountPaid":U      + cContainerCode))
     dCopayAuth            =  DECIMAL(oRequestHelper:getFieldValue("fdCopayAuth":U         + cContainerCode))
     dCopayPaid            =  DECIMAL(oRequestHelper:getFieldValue("fdCopayPaid":U         + cContainerCode))
     dItemCost             =  DECIMAL(oRequestHelper:getFieldValue("fdItemCost":U          + cContainerCode))
     dFixedItemCost        =  DECIMAL(oRequestHelper:getFieldValue("fdFixedItemCost":U     + cContainerCode))
     cRelatedEntity        =          oRequestHelper:getFieldValue("cbRelatedEntity":U     + cContainerCode)
     dRelatedObj           =  DECIMAL(oRequestHelper:getFieldValue("fdRelatedObj":U        + cContainerCode))
     cRelatedCode          =          oRequestHelper:getFieldValue("fcRelatedCode":U       + cContainerCode)
     dTariffTypeObj        =  DECIMAL(oRequestHelper:getFieldValue("cbTariffType":U        + cContainerCode))
     dMinutesTariffTypeObj =  DECIMAL(oRequestHelper:getFieldValue("fdMinTariffTypeObj":U  + cContainerCode))
     dAuthObj              =  DECIMAL(oRequestHelper:getFieldValue("_authObjArgument":U    + cContainerCode))

     lRepeatItem           = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flRepeatItem":U + cContainerCode))
     iRepeatCycleAuth      = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleAuth":U         + cContainerCode))
     iRepeatCyclePaid      = INTEGER(oRequestHelper:getFieldValue("fiRepeatCyclePaid":U         + cContainerCode))
     iRepeatCycleQuantity  = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleQuantity":U     + cContainerCode))
     iRepeatCycleDays      = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleDays":U         + cContainerCode))
     iRepeatCycleGraceDays = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleGraceDays":U    + cContainerCode))
     dRepeatLastClaimDate  =    DATE(oRequestHelper:getFieldValue("fdRepeatLastClaimedDate":U   + cContainerCode))
     
     lAddedByUser          =  CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flAddedByUser":U + cContainerCode))
                           
     lDiscountType         = IF oRequestHelper:getFieldValue("cbDiscountType":U + cContainerCode) <> "?":U
                             THEN LOGICAL(oRequestHelper:getFieldValue("cbDiscountType":U  + cContainerCode))
                             ELSE ?
                           
     lStartAmPm            = CAN-DO("AM":U, oRequestHelper:getFieldValue("cbStartAmPm":U   + cContainerCode))
     lEndAmPm              = CAN-DO("AM":U, oRequestHelper:getFieldValue("cbEndAmPm":U     + cContainerCode))

     lStartDateAmPmUpdated = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flStartDateAmPmUpdated":U + cContainerCode))
     lEndDateAmPmUpdated   = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flEndDateAmPmUpdated":U   + cContainerCode))

     lClaimCodeUpdated     = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flClaimCodeUpdated":U     + cContainerCode))
     lClaimTypeUpdated     = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flClaimTypeUpdated":U     + cContainerCode))

     oDetail          = NEW cls.maauthorisationdetail()
    NO-ERROR.

  IF NOT {&ErrorStatus} THEN
  DO:
    CASE cRecordAction:

      WHEN "modify":U THEN
      DO:

        IF cOEM <> "":U THEN
        DO:
          mipEnv:miDBEntity:focusTable(cOEM).

          IF mipEnv:miDBEntity:InFocus
          THEN mipEnv:miDBEntity:findRecord(rRowid).

          IF mipEnv:miDBEntity:RecordAvailable
          THEN
            ASSIGN cOwningKey = mipEnv:miDBEntity:RecordKey
                   dOwningObj = mipEnv:miDBEntity:RecordObj.

        END. /*IF cOEM <> "":U THEN*/

        oDetail:focusRecord(dAuthDetailObj) NO-ERROR.
        
        DO TRANSACTION:
        

          IF  oDetail:Infocus 
          AND oDetail:OwningAltValue <> cOwningAltValue THEN
          DO  :
              oDetail:deleteRecord() .
              
              ASSIGN 
                lAltValueChanged        = TRUE
                dAuthDetailObj          = -1 
                cLineRestriction        = "":U
                cLOSCalcRule            = "":U
                cBodyRegion             = "":U
                cPrintDM                = "":U
                cPayeeDM                = "":U
                cNote                   = "":U
                cCopayOverrideNote      = "":U
                cAuthStatusNote         = "":U
                cOverrideBaseRate       = "":U
                cOverrideArsRate        = "":U
                dAuthGroupObj           = 0 
                dCPTObj                 = 0 
                dQtyReq                 = 0 
                dQtyAuth                = 0 
                dQtyPaid                = 0 
                dPMBBenefit             = 0 
                dPMBValue               = 0 
                dAmountReq              = 0 
                dAmountAuth             = 0 
                dAmountPaid             = 0 
                dDiscountAuth           = 0 
                dDiscountPaid           = 0 
                dCopayAuth              = 0 
                dCopayPaid              = 0 
                dItemCost               = 0 
                dFixedItemCost          = 0 
                lRepeatItem             = FALSE
                iRepeatCycleAuth        = 0
                iRepeatCyclePaid        = 0
                iRepeatCycleQuantity    = 0
                iRepeatCycleDays        = 0
                iRepeatCycleGraceDays   = 0
                cRelatedEntity          = "":U
                dRelatedObj             = 0
                cRelatedCode            = "":U
                lAddedByUser            = TRUE 
                lDiscountType           = ?          
                lStartDateAmPmUpdated   = FALSE
                lEndDateAmPmUpdated     = FALSE
                lClaimCodeUpdated       = FALSE
                lClaimTypeUpdated       = FALSE .
                
               
          END.  // IF  oDetail:Infocus  AND oDetail:OwningAltValue <> cOwningAltValue 
              
          ASSIGN
            lCreated                       = (dAuthDetailObj <= 0.00)
                                           
            oDetail:AuthDetailObj          = dAuthDetailObj WHEN NOT oDetail:InFocus
            oDetail:OwningEntityMnemonic   = cOEM
            oDetail:OwningObj              = dOwningObj
            oDetail:OwningKey              = cOwningKey
            oDetail:OwningAltValue         = cOwningAltValue
            oDetail:AddedByUser            = lAddedByUser
            oDetail:AuthObj                = dAuthObj
            oDetail:AuthProviderObj        = dAuthProviderObj
            oDetail:AmountRequested        = dAmountReq         
            oDetail:AmountAuth             = dAmountAuth        
            oDetail:AmountPaid             = dAmountPaid        
            oDetail:QuantityRequested      = dQtyReq            
            oDetail:QuantityAuth           = dQtyAuth           
            oDetail:QuantityPaid           = dQtyPaid           
            oDetail:QuantityLos            = dQtyLos            
            oDetail:PMBBenefit             = dPMBBenefit        
            oDetail:PMBValue               = dPMBValue          
            oDetail:DiscountPaid           = dDiscountPaid      
            oDetail:CopayAuth              = dCopayAuth         
            oDetail:CopayPaid              = dCopayPaid         
            oDetail:DiscountType           = lDiscountType      
            oDetail:StartAmPm              = lStartAmPm         
            oDetail:EndAmPm                = lEndAmPm           
            oDetail:StartDate              = dStartDate         
            oDetail:EndDate                = dEndDate           
            oDetail:ClaimCode              = iClaimCode         
            oDetail:ClaimType              = cClaimType         
            oDetail:ClaimCodeUpdated       = lClaimCodeUpdated
            oDetail:ClaimTypeUpdated       = lClaimTypeUpdated
            oDetail:AuthStatus             = iStatus
            oDetail:AuthStatusNote         = cAuthStatusNote
            oDetail:Note                   = cNote
            oDetail:CopayOverrideNote      = cCopayOverrideNote           
            oDetail:CPTObj                 = dCPTObj
            oDetail:OverrideBaseRate       = cOverrideBaseRate
            oDetail:OverrideArsRate        = cOverrideArsRate
            oDetail:DefaultBaseRate        = cDefaultBaseRate
            oDetail:DefaultArsRate         = cDefaultArsRate
            oDetail:LineRestriction        = cLineRestriction
            oDetail:LOSCalculationRule     = cLosCalcRule
            oDetail:BodyRegion             = cBodyRegion
            oDetail:PrintDM                = cPrintDM
            oDetail:PayeeDM                = cPayeeDM
            oDetail:LineNumber             = iLineNumber
            oDetail:LocSequence            = IF iLineSequence = 0 THEN 0 ELSE iLineSequence
            oDetail:DiscountAuth           = dDiscountAuth
            oDetail:MinutesAuth            = iMinutesAuth
            oDetail:MinutesRequested       = iMinutesRequested
            oDetail:MinutesTariffTypeObj   = dMinutesTariffTypeObj
            oDetail:LOCTariffTypeObj       = dTariffTypeObj
            oDetail:StartDateAmPmUpdated   = lStartDateAmPmUpdated
            oDetail:EndDateAmPmUpdated     = lEndDateAmPmUpdated
            oDetail:AuthGroupObj           = dAuthGroupObj
            
            lPreserveRelated               = lAltValueChanged AND oDetail:RelatedEntityMnemonic = "hlmnl":U AND  (cNappiRequired = "ma_acNappiRequiredEnforce":U OR cNappiRequired = "ma_acNappiRequiredOptional":U ) 
            oDetail:RelatedEntityMnemonic  = IF lPreserveRelated THEN  oDetail:RelatedEntityMnemonic  ELSE cRelatedEntity             
            oDetail:RelatedKey             = IF lPreserveRelated THEN  oDetail:RelatedKey             ELSE mipEnv:miDBEntity:RecordKey
            oDetail:RelatedObj             = IF lPreserveRelated THEN  oDetail:RelatedObj             ELSE dRelatedObj                
            oDetail:RelatedValue           = IF lPreserveRelated THEN  oDetail:RelatedValue           ELSE cRelatedCode               
            oDetail:ItemCost               = dItemCost
            oDetail:FixedItemCost          = dFixedItemCost
            oDetail:Reason                 = cTariffReason
            oDetail:RepeatItem             = lRepeatItem          
            oDetail:RepeatCycleAuth        = iRepeatCycleAuth     
            oDetail:RepeatCyclePaid        = iRepeatCyclePaid     
            oDetail:RepeatCycleQuantity    = iRepeatCycleQuantity 
            oDetail:RepeatCycleDays        = iRepeatCycleDays     
            oDetail:RepeatCycleGraceDays   = iRepeatCycleGraceDays
            oDetail:RepeatLastClaimedDate  = dRepeatLastClaimDate 
            
            lSuccess                       = oDetail:saveRecord()
           NO-ERROR.
         

         
          IF NOT {&ErrorStatus} AND NOT oDetail:ErrorObject:ErrorsExist THEN
          DO:
            ASSIGN
              oResponseHelper:RequestValid    = TRUE
              oResponseHelper:ResponseMessage = "Record successfully saved":U
         
              lSuccess                        = oResponseHelper:addFieldValue("fdAuthDetailObj":U  + cContainerCode, STRING(oDetail:AuthDetailObj), TRUE)
              lSuccess                        = oResponseHelper:addFieldValue("fdQuantityAuth":U   + cContainerCode, STRING(oDetail:QuantityAuth, "zzzz9.99"))
              lSuccess                        = oResponseHelper:addFieldValue("fdQuantityLos":U    + cContainerCode, STRING(oDetail:QuantityLos,  "zzzz9.99"))
              lSuccess                        = oResponseHelper:addFieldValue("fdAmountAuth":U     + cContainerCode, STRING(oDetail:AmountAuth,   "-z,zzz,zz9.99"))
         
              lSuccess                        = oResponseHelper:addFieldValue("fiLocSequence":U    + cContainerCode, STRING(oDetail:LocSequence))
         
              lSuccess                        = oResponseHelper:addFieldValue("cbStatus":U         + cContainerCode, STRING(oDetail:AuthStatus))
              lSuccess                        = oResponseHelper:addFieldValue("fcAuthStatusNote":U + cContainerCode, STRING(oDetail:AuthStatusNote))
              lSuccess                        = oResponseHelper:addFieldValue("fcCopayOverrideNote":U  + cContainerCode, oDetail:CopayOverrideNote)
         
              lSuccess                        = oResponseHelper:addFieldValue("fdStartDate":U      + cContainerCode, STRING(oDetail:StartDate, "99/99/9999":U))
              lSuccess                        = oResponseHelper:addFieldValue("fdEndDate":U        + cContainerCode, STRING(oDetail:EndDate  , "99/99/9999":U))
         
              lSuccess                        = oResponseHelper:addFieldValue("cbStartAmPm":U      + cContainerCode, STRING(oDetail:StartAmPm, "AM/PM":U))
              lSuccess                        = oResponseHelper:addFieldValue("cbEndAmPm":U        + cContainerCode, STRING(oDetail:EndAmPm  , "AM/PM":U))
              lSuccess                        = oResponseHelper:addFieldValue("flPmbIndicator":U   + cContainerCode, STRING(oDetail:PMBIndicator))
              lSuccess                        = oResponseHelper:addFieldValue("flPMBPayCost":U     + cContainerCode, STRING(oDetail:PMBPayCost))
              lSuccess                        = oResponseHelper:addFieldValue("fdItemCost":U       + cContainerCode, STRING(oDetail:ItemCost,   "-z,zzz,zz9.99"))
         
              lSuccess                        = oResponseHelper:addFieldValue("cbClaimType":U      + cContainerCode, STRING(oDetail:ClaimType))
              lSuccess                        = oResponseHelper:addFieldValue("fiClaimCode":U      + cContainerCode, STRING(oDetail:ClaimCode))

              lSuccess                        = oResponseHelper:addFieldValue("fiMinAuthorised":U     + cContainerCode, STRING(oDetail:MinutesAuth))
              lSuccess                        = oResponseHelper:addFieldValue("fiMinRequested":U      + cContainerCode, STRING(oDetail:MinutesRequested))
              lSuccess                        = oResponseHelper:addFieldValue("fdMinTariffTypeObj":U  + cContainerCode, STRING(oDetail:MinutesTariffTypeObj))
              
             NO-ERROR.
         
          END. /*IF NOT {&ErrorStatus} AND NOT oDetail:ErrorObject:ErrorsExist THEN*/
          
          /*
            If there was an error when the owning alt value was changed , we need to undo the transaction so that the delete can be reversed
          */
          ELSE IF ({&ErrorStatus} OR oDetail:ErrorObject:ErrorsExist) AND lAltValueChanged 
          THEN 
            UNDO , LEAVE .
            
        END. //DO TRANSACTION
        
        
      END. /* WHEN "modify":U THEN */

      WHEN "delete":U THEN
      DO:
        oDetail:focusRecord(dAuthDetailObj) NO-ERROR.

        IF NOT {&ErrorStatus} AND NOT oDetail:InFocus
        THEN
          ASSIGN
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted"
          NO-ERROR.
        ELSE
          ASSIGN lSuccess = oDetail:deleteRecord() NO-ERROR.

        IF NOT {&ErrorStatus} AND NOT oDetail:ErrorObject:ErrorsExist
        THEN
          ASSIGN
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully removed"
            oResponseHelper:ReturnValue     = "Record successfully removed"

            /* Rebuild if we have deleted an los/loc type line */
            lSuccess                        = oResponseHelper:addFieldValue("_triggerRebuild":U  + cContainerCode, oRequestHelper:TriggerField, TRUE) WHEN oDetail:QuantityLos <> 0.00
            NO-ERROR.

      END. /* WHEN "delete":U THEN */

      OTHERWISE
      DO:
        ASSIGN
          oResponseHelper:RequestValid    = FALSE
          oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported", cRecordAction)
          oResponseHelper:ResponseMessage = "Unable to perform action"
         NO-ERROR.
      END. /* OTHERWISE */
    END CASE.
  END. /*IF NOT {&ErrorStatus} THEN*/

  ASSIGN cErrorMessage = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, "DESCRIPTION,STACK":U) NO-ERROR.
  {&ResetError}

  IF cErrorMessage <> "":U OR oDetail:ErrorObject:ErrorsExist THEN
  DO:

    ASSIGN
      oResponseHelper:RequestValid    = FALSE

      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(oDetail:ErrorObject)

      oResponseHelper:ResponseMessage = 'Unable to perform action'
      oResponseHelper:ReturnValue     = cErrorMessage
     NO-ERROR.

  END.   /*IF oDetail:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/

  ASSIGN lSuccess = oResponseHelper:setError(oDetail:ErrorObject) NO-ERROR.

  { mip/inc/mipmessageerror.i &ResetTheError = TRUE }


  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oDetail)         THEN DELETE OBJECT oDetail         NO-ERROR. ~{mip/inc/mipmessageerror.i~} " }
&ENDIF
