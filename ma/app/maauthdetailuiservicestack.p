&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Auth Detail UI Service stack

    Author : MMP

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation }

{ ma/inc/maauthds.i           }
{ ma/inc/maauthtypeds.i       }
{ ma/inc/matariffds.i         }
{ ma/inc/manappids.i          }
{ ma/inc/manappifieldvalues.i }
{ ma/inc/macrosswalkds.i      }
{ ma/inc/maauthtypeconfigtt.i }

DEFINE TEMP-TABLE ttControl
    FIELD ContainerCode AS CHARACTER
    FIELD ControlToken  AS CHARACTER
    FIELD ControlName   AS CHARACTER
    FIELD ControlClass  AS CHARACTER
    FIELD CellSnippet   AS CHARACTER
    INDEX idx1 ControlName.

DEFINE VARIABLE giOptionCode                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE gdInsurerObj                 AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gcBaseRateList               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcDefaultDateJSOnBlur        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcDefaultAmPmJSOnBlur        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcLineRestriction            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcProviderList               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gdProviderObj                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gdAuthDetailObj              AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gdAuthGroupObj               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gdOwningObj                  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gcOwningAltValue             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gdTariffTypeObj              AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gcArsRate                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcBaseRate                   AS CHARACTER   NO-UNDO.

&GLOBAL-DEFINE ContainerButton "<div class='&1' onclick='&2'>&3</div>":U

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 18.1
         WIDTH              = 56.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveDetailContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveDetailContainer Procedure 
PROCEDURE ajaxSaveDetailContainer :
/*------------------------------------------------------------------------------
  Purpose   : Authorisation Detail Container Specific Auto Save Handler
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  { ma/app/maauthdetailajxsavedetail.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxSaveMultipleNappiContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveMultipleNappiContainer Procedure 
PROCEDURE ajaxSaveMultipleNappiContainer :
/*------------------------------------------------------------------------------
  Purpose   : Authorisation Detail Multiple nappi Container Specific Auto Save Handler
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE oRequestHelper        AS cls.maajaxrequesthelper   NO-UNDO.
  DEFINE VARIABLE oResponseHelper       AS cls.maajaxresponsehelper  NO-UNDO.
  DEFINE VARIABLE oDetail               AS cls.maauthorisationdetail NO-UNDO.
  DEFINE VARIABLE cClaimType            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cContainerCode        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cErrorMessage         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cLineRestriction      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNote                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningKey            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningAltValue       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dOwningObj            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE cOEM                  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cReason               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRecordAction         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE iClaimCode            AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iLineNumber           AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iStatus               AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE dStartDate            AS DATE                      NO-UNDO.
  DEFINE VARIABLE dEndDate              AS DATE                      NO-UNDO.
  DEFINE VARIABLE dAmountReq            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthDetailNappiObj   AS DECIMAL                   NO-UNDO. 
  DEFINE VARIABLE dAmountAuth           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAmountPaid           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj        AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCopayAuth            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCopayPaid            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dDiscountAuth         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dDiscountPaid         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dItemCost             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dPMBBenefit           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dPMBValue             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyAuth              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyLos               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyPaid              AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dQtyReq               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE lDiscountType         AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lEndAmPm              AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lStartAmPm            AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lStartDateAmPmUpdated AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lEndDateAmPmUpdated   AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lClaimCodeUpdated     AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lClaimTypeUpdated     AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cRelatedEntity        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRelatedCode          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dRelatedObj           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthObj              AS DECIMAL                   NO-UNDO.

  DEFINE VARIABLE cAuthDetailIndexList  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dProviderObj          AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE cDfltBaseRate         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDfltArsRate          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dTariffTypeObj        AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE lRepeatItem           AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleAuth      AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCyclePaid      AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleQuantity  AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleDays      AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRepeatCycleGraceDays AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE dRepeatLastClaimDate  AS DATE                      NO-UNDO. 
  DEFINE VARIABLE cCopayOverrideNote    AS CHARACTER                 NO-UNDO.

    ASSIGN
      oRequestHelper       = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
      oResponseHelper      = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
                          
      cContainerCode       = ipcValidationArgument
      cRecordAction        =          oRequestHelper:getFieldValue("fcAction":U                  + cContainerCode)
      dAuthDetailObj       =  DECIMAL(oRequestHelper:getFieldValue("fdAuthDetailObjArgument":U   + cContainerCode))  // original auth detail obj we are adding this related nappi for
      
      dAuthDetailNappiObj  =  DECIMAL(oRequestHelper:getFieldValue("fdAuthDetailNappiObj":U      + cContainerCode))

      dAuthObj             =  DECIMAL(oRequestHelper:getFieldValue("_authObjArgument":U          + cContainerCode))
      
      cOEM                 =          oRequestHelper:getFieldValue("cbEOM":U                     + cContainerCode)
      cOwningAltValue      =          oRequestHelper:getFieldValue("_fcOwningAltValueArgument":U  + cContainerCode)

      cAuthDetailIndexList =          oRequestHelper:getFieldValue("_AuthDetailIndexListArgument":U      + cContainerCode)

      dProviderObj         =  DECIMAL(ENTRY(1, cAuthDetailIndexList, "|":U ))
      dAuthGroupObj        =  DECIMAL(ENTRY(2, cAuthDetailIndexList, "|":U ))
      cDfltBaseRate        =          ENTRY(3, cAuthDetailIndexList, "|":U )
      cDfltArsRate         =          ENTRY(4, cAuthDetailIndexList, "|":U )
      dTariffTypeObj       =  DECIMAL(ENTRY(5, cAuthDetailIndexList, "|":U ))
      dOwningObj           =  DECIMAL(ENTRY(6, cAuthDetailIndexList, "|":U ))

      cRelatedEntity       =          oRequestHelper:getFieldValue("cbREM":U               + cContainerCode)
      dRelatedObj          =  DECIMAL(oRequestHelper:getFieldValue("fdRelatedObj":U        + cContainerCode))
      cRelatedCode         =          oRequestHelper:getFieldValue("fcRelatedAltValue":U   + cContainerCode)
                           
      dStartDate           =     DATE(oRequestHelper:getFieldValue("_fdStartDateArgument":U + cContainerCode))
      dEndDate             =       IF oRequestHelper:getFieldValue("_fdEndDateArgument":U   + cContainerCode) <> "YYYY/MM/DD" THEN 
                                 DATE(oRequestHelper:getFieldValue("_fdEndDateArgument":U   + cContainerCode)) 
                                 ELSE ?
                           
      lStartAmPm           = CAN-DO("AM":U, oRequestHelper:getFieldValue("_cbStartAmPmArgument":U   + cContainerCode))
      lEndAmPm             = CAN-DO("AM":U, oRequestHelper:getFieldValue("_cbEndAmPmArgument":U     + cContainerCode)) 
                           
      dQtyReq              =  DECIMAL(oRequestHelper:getFieldValue("fdQuantityRequested":U + cContainerCode))
      dQtyAuth             =  DECIMAL(oRequestHelper:getFieldValue("fdQuantityAuth":U      + cContainerCode))
      dAmountReq           =  DECIMAL(oRequestHelper:getFieldValue("fdAmountRequested":U   + cContainerCode))
      dAmountAuth          =  DECIMAL(oRequestHelper:getFieldValue("fdAmountAuth":U        + cContainerCode))
      dAmountPaid          =  DECIMAL(oRequestHelper:getFieldValue("fdAmountPaid":U        + cContainerCode))
      dDiscountAuth        =  DECIMAL(oRequestHelper:getFieldValue("fdDiscountAuth":U      + cContainerCode))
      dDiscountPaid        =  DECIMAL(oRequestHelper:getFieldValue("fdDiscountPaid":U      + cContainerCode))
      dCopayAuth           =  DECIMAL(oRequestHelper:getFieldValue("fdCopayAuth":U         + cContainerCode))
      dCopayPaid           =  DECIMAL(oRequestHelper:getFieldValue("fdCopayPaid":U         + cContainerCode))
                           
      lRepeatItem           = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flRepeatItem":U + cContainerCode))
      iRepeatCycleAuth      = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleAuth":U         + cContainerCode))
      iRepeatCyclePaid      = INTEGER(oRequestHelper:getFieldValue("fiRepeatCyclePaid":U         + cContainerCode))
      iRepeatCycleQuantity  = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleQuantity":U     + cContainerCode))
      iRepeatCycleDays      = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleDays":U         + cContainerCode))
      iRepeatCycleGraceDays = INTEGER(oRequestHelper:getFieldValue("fiRepeatCycleGraceDays":U    + cContainerCode))
      dRepeatLastClaimDate  =    DATE(oRequestHelper:getFieldValue("fdRepeatLastClaimedDate":U   + cContainerCode))
     
      lDiscountType        = IF oRequestHelper:getFieldValue("cbDiscountType":U + cContainerCode) <> "?":U
                             THEN LOGICAL(oRequestHelper:getFieldValue("cbDiscountType":U  + cContainerCode))
                             ELSE ?
                           
                           
      iLineNumber          =  INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U        + cContainerCode))
      iStatus              =  INTEGER(oRequestHelper:getFieldValue("cbStatus":U            + cContainerCode))
      cReason              =          oRequestHelper:getFieldValue("fcReason":U            + cContainerCode)
      cCopayOverrideNote   =          oRequestHelper:getFieldValue("fcCopayOverrideNote":U + cContainerCode)  
      iClaimCode           =  INTEGER(oRequestHelper:getFieldValue("fiClaimCode":U         + cContainerCode))
      cClaimType           =          oRequestHelper:getFieldValue("cbClaimType":U         + cContainerCode)

      lClaimCodeUpdated    = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flClaimCodeUpdated":U     + cContainerCode))
      lClaimTypeUpdated    = CAN-DO("YES,TRUE":U, oRequestHelper:getFieldValue("flClaimTypeUpdated":U     + cContainerCode))

      oDetail              = NEW cls.maauthorisationdetail()      .
  
  
  { ma/msc/matarpad.i &tariff     = cOwningAltValue  } 
  
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:76' &ResetIgnoredErrors = TRUE }  

  IF NOT {&ErrorStatus} THEN
  DO:
    /*Use the saved tariff to make a copy*/
    ASSIGN lSuccess = oDetail:focusRecord(dAuthDetailObj) NO-ERROR. 
        
    /*if we have already saved the tariff detail line.we wil update it */
        IF (dAuthDetailNappiObj > 0 ) THEN
          ASSIGN lSuccess = oDetail:focusRecord(dAuthDetailNappiObj) NO-ERROR.
    CASE cRecordAction:

      WHEN "modify":U THEN
      DO:        
        ASSIGN
          oDetail:AuthDetailObj          = IF dAuthDetailNappiObj > 0  /*if the tariif was saved with a Nappi linked we will update the existing record*/
                                           THEN dAuthDetailNappiObj 
                                           ELSE -1 /*create a new record*/

          oDetail:RelatedEntityMnemonic  = cRelatedEntity
          oDetail:RelatedObj             = dRelatedObj
          oDetail:RelatedValue           = cRelatedCode .
        
        IF dAuthDetailObj <= 0 
        THEN
          ASSIGN 
            oDetail:AuthObj                = dAuthObj
            oDetail:OwningEntityMnemonic   = "htmtl"
            oDetail:OwningObj              = dOwningObj
            oDetail:OwningAltValue         = cOwningAltValue
            
            oDetail:AuthProviderObj        = dProviderObj
            oDetail:AuthGroupObj           = dAuthGroupObj
            oDetail:DefaultBaseRate        = cDfltBaseRate
            oDetail:DefaultArsRate         = cDfltArsRate
            oDetail:LOCTariffTypeObj       = dTariffTypeObj.
        
        ASSIGN
          oDetail:AddedByUser            = TRUE
          oDetail:AmountRequested        = dAmountReq
          oDetail:AmountAuth             = dAmountAuth
          oDetail:AmountPaid             = dAmountPaid
          oDetail:QuantityRequested      = dQtyReq
          oDetail:QuantityAuth           = dQtyAuth
          oDetail:QuantityPaid           = dQtyPaid
          oDetail:QuantityLos            = dQtyLos
          oDetail:DiscountPaid           = dDiscountPaid
          oDetail:CopayAuth              = dCopayAuth
          oDetail:CopayPaid              = dCopayPaid
          oDetail:DiscountType           = lDiscountType
          
          oDetail:StartDateAmPmUpdated   = lStartDateAmPmUpdated
          oDetail:EndDateAmPmUpdated     = lEndDateAmPmUpdated
          oDetail:StartAmPm              = lStartAmPm
          oDetail:EndAmPm                = lEndAmPm
          oDetail:StartDate              = dStartDate
          oDetail:EndDate                = dEndDate
          oDetail:ClaimCode              = iClaimCode
          oDetail:ClaimType              = cClaimType
          oDetail:ClaimCodeUpdated       = lClaimCodeUpdated
          oDetail:ClaimTypeUpdated       = lClaimTypeUpdated
          oDetail:AuthStatus             = iStatus
          oDetail:AuthStatusNote         = cReason
          oDetail:CopayOverrideNote      = cCopayOverrideNote

          oDetail:RepeatItem             = lRepeatItem           
          oDetail:RepeatCycleAuth        = iRepeatCycleAuth      
          oDetail:RepeatCyclePaid        = iRepeatCyclePaid      
          oDetail:RepeatCycleQuantity    = iRepeatCycleQuantity  
          oDetail:RepeatCycleDays        = iRepeatCycleDays      
          oDetail:RepeatCycleGraceDays   = iRepeatCycleGraceDays 
          oDetail:RepeatLastClaimedDate  = dRepeatLastClaimDate  

          oDetail:LineNumber             = iLineNumber
          oDetail:DiscountAuth           = dDiscountAuth

          oDetail:ItemCost               = dItemCost 
          
          lSuccess                       = oDetail:saveRecord()
         NO-ERROR.
         
        IF NOT {&ErrorStatus} AND NOT oDetail:ErrorObject:ErrorsExist THEN
        DO:
        
        
          ASSIGN
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U

            lSuccess                        =      oResponseHelper:addFieldValue("fdAuthDetailNappiObj":U     + cContainerCode, STRING(oDetail:AuthDetailObj), TRUE)
            lSuccess                        = IF    dAuthDetailObj <= 0 
                                              THEN oResponseHelper:addFieldValue("fdAuthDetailObjArgument":U  + cContainerCode, STRING(oDetail:AuthDetailObj), TRUE)
                                              ELSE FALSE 
            
            lSuccess                        =      oResponseHelper:addFieldValue("fdAmountAuth":U             + cContainerCode, STRING(oDetail:AmountAuth,   "-z,zzz,zz9.99"))
            lSuccess                        =      oResponseHelper:addFieldValue("fdQuantityAuth":U           + cContainerCode, STRING(oDetail:QuantityAuth,   "-z,zzz,zz9.99"))
            lSuccess                        =      oResponseHelper:addFieldValue("fdAmountPaid":U             + cContainerCode, STRING(oDetail:AmountPaid,   "-z,zzz,zz9.99"))
                                                   
            lSuccess                        =      oResponseHelper:addFieldValue("cbStatus":U         + cContainerCode, STRING(oDetail:AuthStatus))
            lSuccess                        =      oResponseHelper:addFieldValue("fcReason":U         + cContainerCode, STRING(oDetail:AuthStatusNote))
                                                   
            lSuccess                        =      oResponseHelper:addFieldValue("fdStartDate":U      + cContainerCode, STRING(oDetail:StartDate, "99/99/9999":U))
            lSuccess                        =      oResponseHelper:addFieldValue("fdEndDate":U        + cContainerCode, STRING(oDetail:EndDate  , "99/99/9999":U))
                                                   
            lSuccess                        =      oResponseHelper:addFieldValue("cbStartAmPm":U      + cContainerCode, STRING(oDetail:StartAmPm, "AM/PM":U))
            lSuccess                        =      oResponseHelper:addFieldValue("cbEndAmPm":U        + cContainerCode, STRING(oDetail:EndAmPm  , "AM/PM":U))
            lSuccess                        =      oResponseHelper:addFieldValue("fdItemCost":U       + cContainerCode, STRING(oDetail:ItemCost,  "-z,zzz,zz9.99"))
                                                   
            lSuccess                        =      oResponseHelper:addFieldValue("cbClaimType":U     + cContainerCode, STRING(oDetail:ClaimType))
            lSuccess                        =      oResponseHelper:addFieldValue("fiClaimCode":U     + cContainerCode, STRING(oDetail:ClaimCode))
           NO-ERROR.

        END. /*IF NOT {&ErrorStatus} AND NOT oDetail:ErrorObject:ErrorsExist THEN*/
      END. /* WHEN "modify":U THEN */

      WHEN "delete":U THEN
      DO:
        oDetail:focusRecord(dAuthDetailNappiObj) NO-ERROR.
        IF NOT {&ErrorStatus} AND NOT oDetail:InFocus
        THEN
          ASSIGN
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted"
          NO-ERROR.
        ELSE DO:
        
          IF dAuthDetailNappiObj = dAuthDetailObj 
          THEN
            ASSIGN oDetail:RelatedEntityMnemonic  = "":U
                   oDetail:RelatedObj             = 0
                   oDetail:RelatedValue           = "":U 
                   lSuccess                       = oDetail:saveRecord() .
          ELSE 
            ASSIGN lSuccess = oDetail:deleteRecord() NO-ERROR.
          
        END. // ELSE DO:


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

END PROCEDURE. /* ajaxSaveMultipleNappiContainer */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidationDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationDetail Procedure 
PROCEDURE ajaxValidationDetail :
/*------------------------------------------------------------------------------
  Purpose   : Auth Detail Container Ajax Validation
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  
  { ma/app/maauthdetailajxvaldetail.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdDetail Procedure 
PROCEDURE getCntUpdDetail :
/*------------------------------------------------------------------------------
  Purpose   : Detail update container definition.
  Parameters:
  Notes     : Combined clinical detail container including the following:
                - Tariff's
                - Nappi's
                - Basket's
------------------------------------------------------------------------------*/

  { ma/app/maauthdetailgetcntupddetail.i }

  { mip/inc/mipcatcherror.i } 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdDetailForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdDetailForm Procedure 
PROCEDURE getCntUpdDetailForm :
/*------------------------------------------------------------------------------
  Purpose   : Update authorisation details container definition form
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  { ma/app/maauthdetailgetcntupdform.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdDetailHolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdDetailHolder Procedure 
PROCEDURE getCntUpdDetailHolder :
/*------------------------------------------------------------------------------
  Purpose   : Authorisation detail holding container definition
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  ASSIGN
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "100%":U, "":U, "":U, FALSE)
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue

    oControl                           = opoContainer:addControl("DetailContainer":U, "":U, "":U, "":U, "":U, 1, 1, "":U)
    oControl:SubContainer              = mipEnv:Health:maUiService:getCntUpdDetail("DetailContainer":U)
    oControl:SubContainerType          = "TABLE":U
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2
    oControl:CellSnippet               = "align='center'":U
    .

&ENDIF

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdLOS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdLOS Procedure 
PROCEDURE getCntUpdLOS :
/*------------------------------------------------------------------------------
  Purpose   : LOS update container definition.
  Parameters:
  Notes     : MMP-452 This routine will no longer be used as we will
              capture los/loc records in the main detail container.
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oButtonContainer AS cls.mipwscontainer NO-UNDO.
  DEFINE VARIABLE oControl         AS cls.mipwscontrol   NO-UNDO.


  ASSIGN
    opoContainer                          = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle           = "LOS Details":U
    opoContainer:ViewOnly                 = FALSE
    opoContainer:NoDataMessage            = "Please specify LOS information in the empty line provided above":U
    opoContainer:ShowContainerSettings    = FALSE
    opoContainer:ContainerMode            = Warpspeed:SubmitValue
    opoContainer:QueryString              = "FOR EACH tt_auth_los NO-LOCK":U
                                          + "      BY tt_auth_los.line_number":U
                                          + "      BY tt_auth_los.line_sequence ":U
    opoContainer:DefaultContainerType     = "TABLE":U
    opoContainer:RowsToRender             = ?
    opoContainer:RowRenderProcedure       = "RowRenderProcedure":U
    opoContainer:RowRenderArgument        = "LOSContainer":U

    oControl                              = opoContainer:addControl("fdAuthDetailObj":U  + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_los.auth_detail_obj":U , "CHARACTER":U,   1, "":U)
    oControl:ControlClass                 = "+clHid":U

    oControl                              = opoContainer:addControl("fiSequence":U       + ipcContainerName, "wsInput":U       , "4":U , "tt_auth_los.line_sequence":U   , "INTEGER":U  ,   2, "Seq":U)
    oControl:ControlClass                 = "+clMan +clNumericOnly +clMaxLength:3":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthLosField(this);":U

    oControl                              = opoContainer:addControl("fdTariffObj":U      + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_los.owning_obj":U      , "DECIMAL":U  ,   3, "":U)
    oControl:ControlToken                 = "Hidden":U

    oControl                              = opoContainer:addControl("fcTariffCode":U     + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_los.owning_alt_value":U, "CHARACTER":U,   3, "":U)
    oControl:ControlClass                 = "+clMan":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthLosField(this);":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationLOS:Tariff":U
    oControl:FilterFields                 = "[TariffCode],[ArsRate],[BaseRate],[PrType],[SubPrType],[TariffTypeObj],[TariffTypeCode],[StartDate]":U
    oControl:FilterControls               = "fcTariffCode":U     + ipcContainerName + ",":U
                                          + "fcArsRate":U        + ipcContainerName + ",":U
                                          + "fcBaseRate":U       + ipcContainerName + ",":U
                                          + "fcPrType":U         + ipcContainerName + ",":U
                                          + "fcSubPrType":U      + ipcContainerName + ",":U
                                          + "fdTariffTypeObj":U  + ipcContainerName + ",":U
                                          + "fcTariffTypeCode":U + ipcContainerName + ",":U
                                          + "fdStartDate":U      + ipcContainerName + ",":U
    oControl:ReturnFields                 = "[TariffObj],[TariffTypeObj],[TariffTypeCode],[TariffCost]":U
    oControl:ReturnControls               = "fdTariffObj":U      + ipcContainerName + ",":U
                                          + "fdTariffTypeObj":U  + ipcContainerName + ",":U
                                          + "fcTariffTypeCode":U + ipcContainerName + ",":U
                                          + "fdItemCost":U     + ipcContainerName

    oControl                              = opoContainer:addControl("buTariffBtn":U      + ipcContainerName, "wsLookupButton":U, "":U  , "":U                            , "":U         ,   3, "Tariff Code":U)
    oControl:CellLayoutMask               = "&1&2&3":U
    oControl:LookupWobFLA                 = "htmttl":U
    oControl:LookupFields                 = "htm_tariff_link.tariff_link_obj":U
    oControl:LookupControls               = "fdTariffObj":U + ipcContainerName
    oControl:FilterFields                 = "htm_tariff_type.tariff_type_code,":U
                                          + "htm_tariff_link.tariff_code,":U
                                          + "htm_tariff_link.base_rate,":U
                                          + "htm_tariff_link.ars_rate,":U
                                          + "htm_tariff_link.pr_type":U

    oControl:FilterControls               = "fcTariffTypeCode":U + ipcContainerName + ",":U
                                          + "fcTariffCode":U     + ipcContainerName + ",":U
                                          + "fcBaseRate":U       + ipcContainerName + ",":U
                                          + "fcArsRate":U        + ipcContainerName + ",":U
                                          + "fcPrType":U         + ipcContainerName

    oControl:ReturnFields                 = "htm_tariff_type_link.tariff_type_obj,htm_tariff_link.tariff_link_obj,htm_tariff_link.tariff_code":U
    oControl:ReturnControls               = "fdTariffTypeObj":U + ipcContainerName + ",fdTariffObj":U + ipcContainerName + ",fcTariffCode":U + ipcContainerName

    oControl                              = opoContainer:addControl("fdTariffTypeObj":U  + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_los.tariff_type_obj":U , "DECIMAL":U  ,   4, "":U)
    oControl:ControlToken                 = "HIDDEN":U

    oControl                              = opoContainer:addControl("fcTariffTypeCode":U + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_los.loc_value":U       , "CHARACTER":U,   4, "":U)
    oControl:AjaxValidation               = "SERVICE:wsUIService:ajaxValidation:htmtt|tariff_type_code":U
    oControl:FilterFields                 = "[ArgumentFieldValue]":U
    oControl:ReturnFields                 = "tariff_type_obj,tariff_type_code":U
    oControl:ReturnControls               = "fdTariffTypeObj":U + ipcContainerName + ",fcTariffTypeCode":U + ipcContainerName
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("buTariffTypeBtn":U  + ipcContainerName, "wsLookupButton":U, "10":U, "":U                            , "":U         ,   4, "Tariff<br>Type":U)
    oControl:CellLayoutMask               = "&1&2&3":U
    oControl:LookupWobFLA                 = "mahtmtt":U
    oControl:LookupFields                 = "htm_tariff_type.tariff_type_obj":U
    oControl:LookupControls               = "fdTariffTypeObj":U + ipcContainerName
    oControl:FilterFields                 = "htm_tariff_type.tariff_type_code":U
    oControl:FilterControls               = "fcTariffTypeCode":U + ipcContainerName
    oControl:ReturnFields                 = "htm_tariff_type.tariff_type_obj,htm_tariff_type.tariff_type_code":U
    oControl:ReturnControls               = "fdTariffTypeObj":U + ipcContainerName + ",fcTariffTypeCode":U + ipcContainerName
    oControl:ControlToken                 = "Hidden":U /* Keep this button hidden in case we need the tariff type updatable in future */

    oControl                              = opoContainer:addControl("fdLosQuantity":U    + ipcContainerName, "wsInput":U       ,  "8":U, "tt_auth_los.quantity_los":U    , "DECIMAL":U  ,   5, "LOS<br>Quantity":U)
    oControl:ControlClass                 = "+clMan +clNumericOnly":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthLosField(this);":U
    oControl:JavascriptOnKeyup            = "fnOnChangeAuthLosQuantity(this);":U

    oControl                              = opoContainer:addControl("fdStartDate":U      + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_los.start_date":U      , "DATE":U     ,   6, "Start Date":U)
    oControl:ControlClass                 = "+clMan":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthLosField(this);":U

    oControl                              = opoContainer:addControl("cbStartAmPm":U      + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_los.start_ampm":U      , "LOGICAL":U  ,   6, "Start Date":U)
    oControl:AdditionalItems              = "AM=AM|PM=PM":U
    oControl:CellLayoutMask               = "&1 &2":U
    oControl:ControlClass                 = "+clSelVertAlign":U

    oControl                              = opoContainer:addControl("fdEndDate":U        + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_los.end_date":U        , "DATE":U     ,   7, "End Date":U)
    oControl:ControlToken                 = "ReadOnly":U
    oControl                              = opoContainer:addControl("cbEndAmPm":U        + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_los.end_ampm":U        , "LOGICAL":U  ,   7, "End Date":U)
    oControl:AdditionalItems              = "=|AM=AM|PM=PM":U
    oControl:CellLayoutMask               = "&1 &2":U
    oControl:ControlClass                 = "+clSelVertAlign":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fdQuantityAuth":U   + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_los.quantity_auth":U   , "DECIMAL":U  ,   8, "Quantity<br>Auth":U)
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fdItemCost":U     + ipcContainerName, "wsInput":U       , "10":U, "":U                            , "DECIMAL":U  ,   9, "Tariff<br>Cost":U)
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlFormat                = ">>>>>>9.99":U

    oControl                              = opoContainer:addControl("fdAmountAuth":U     + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_los.amount_auth":U     , "DECIMAL":U  ,  10, "Amount<br>Auth":U)
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlFormat                = ">>>>>>9.99":U

    oControl                              = opoContainer:addControl("btnAdd":U           + ipcContainerName, "wsInput":U       , "20":U, "":U                            , "":U         ,  11, "":U)
    oControl:SpanOverLabel                = TRUE
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthLosAdd":U

    oControl                              = opoContainer:addControl("btnDelete":U       + ipcContainerName, "wsInput":U       , "20":U, "":U                             , "":U         ,  12, "":U)
    oControl:SpanOverLabel                = TRUE
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthLosDelete":U.


  ASSIGN
     opoContainer:TitleContainer                 = NEW cls.mipwscontainer(opoContainer:ContainerCode + "Title":U)
     opoContainer:TitleContainer:Borders         = "FALSE":U
     opoContainer:TitleContainer:Collapsable     = FALSE
     opoContainer:TitleContainer:ViewOnly        = FALSE
     opoContainer:TitleContainer:ContainerWidth  = "100%":U

     oControl                                    = opoContainer:TitleContainer:addControl("fcArsRate":U   + opoContainer:ContainerCode, "wsInput":U, "":U, "":U, "":U, 1, 1, "":U)
     oControl:ControlToken                       = "Hidden":U
     oControl                                    = opoContainer:TitleContainer:addControl("fcBaseRate":U  + opoContainer:ContainerCode, "wsInput":U, "":U, "":U, "":U, 1, 2, "":U)
     oControl:ControlToken                       = "Hidden":U
     oControl                                    = opoContainer:TitleContainer:addControl("fcPrType":U    + opoContainer:ContainerCode, "wsInput":U, "":U, "":U, "":U, 1, 3, "":U)
     oControl:ControlToken                       = "Hidden":U
     oControl                                    = opoContainer:TitleContainer:addControl("fcSubPrType":U + opoContainer:ContainerCode, "wsInput":U, "":U, "":U, "":U, 1, 4, "":U)
     oControl:ControlToken                       = "Hidden":U
     oControl                                    = opoContainer:TitleContainer:addControl("fdStartDate":U + opoContainer:ContainerCode, "wsInput":U, "":U, "":U, "":U, 1, 5, "":U)
     oControl:ControlToken                       = "Hidden":U
     oControl                                    = opoContainer:TitleContainer:addControl("fcError":U     + opoContainer:ContainerCode, "wsInput":U, "":U, "":U, "":U, 1, 6, "":U)
     oControl:ControlToken                       = "Hidden":U

     opoContainer:FooterContainer                = NEW cls.mipwscontainer(opoContainer:ContainerCode + "Footer":U)
     opoContainer:FooterContainer:Borders        = "FALSE":U
     opoContainer:FooterContainer:Collapsable    = FALSE
     opoContainer:FooterContainer:ViewOnly       = FALSE
     opoContainer:FooterContainer:ContainerWidth = "100%":U

     oButtonContainer                            = wsUIService:getButtonContainer(opoContainer:ContainerCode + "ButtonBar":U, "Close:Calculate:CalculateSave":U)

     oControl                                    = opoContainer:FooterContainer:addControl("ButtonBar":U + opoContainer:ContainerCode, "":U, "":U, "":U, "":U, 1, 1, "":U)
     oControl:SubContainer                       = oButtonContainer
     oControl:SpanOverLabel                      = TRUE
     oControl:CellClass                          = WarpSpeed:BaseClass + "ButtonBar":U
     oControl:CellSnippet                        = "align='right'":U
     oControl:ColumnSpan                         = 2

     oControl                                    = oButtonContainer:getControl("btnClose":U)
     oControl:ButtonLabel                        = "Close":U
     oControl:ControlSubType                     = "Button":U
     oControl:JavascriptOnClick                  = "$(~"#LosModalDialog~").addClass(~"clHid~");vContinue = false;":U

     oControl                                    = oButtonContainer:getControl("btnCalculate":U)
     oControl:ButtonLabel                        = "Calculate/Preview":U
     oControl:ControlSubType                     = "Button":U
     oControl:JavascriptOnClick                  = SUBSTITUTE("fnSaveAuthLos(~"&1~",false);vContinue = false;":U, ipcContainerName)

     oControl                                    = oButtonContainer:getControl("btnCalculateSave":U)
     oControl:ButtonLabel                        = "Calculate and Save":U
     oControl:ControlSubType                     = "Button":U
     oControl:JavascriptOnClick                  = SUBSTITUTE("fnSaveAuthLos(~"&1~",true);vContinue = false;":U, ipcContainerName).


  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdNappiDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdNappiDetail Procedure 
PROCEDURE getCntUpdNappiDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

  { ma/app/maauthdetailgetcntupdnappidetail.i }

  { mip/inc/mipcatcherror.i } 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-renderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderProcedure Procedure 
PROCEDURE renderProcedure :
/*------------------------------------------------------------------------------
  Purpose   : Auth Detail Container Render Procedure
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/

  { ma/app/maauthdetailrenderprocedure.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rowRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowRenderProcedure Procedure 
PROCEDURE rowRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  
  { ma/app/maauthdetailrowrender.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
    
