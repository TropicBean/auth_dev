&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
" This code is based on the cgi-wrapper template a designed by Progress.

  MIP Holdings (Pty) Ltd.

  Use this template to create a new Custom CGI Wrapper Procedure and write WebSpeed code that dynamically generates HTML. No associated static HTML file is needed."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
/*------------------------------------------------------------------------------
  Filename    : ma/app/maauthcopaydetitemwobsuper.p
  Purpose     : Authorisation Co-payment Detail Item Control Maintenance
  Description :
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

// This helps to ensure proper clean-up
CREATE WIDGET-POOL.

// Include for HC DB delta checks
{ sysadmma.i }

// WarpSpeed's Shared Definitions
{ mip/inc/mipdefshared.i }
{ ma/inc/maauthcopaytypeds.i}
{ ma/inc/maauthcopaydetitemds.i }
{ ma/inc/maatcclassds.i }
{ ma/inc/manappids.i}
{ ma/inc/macrosswalkds.i }
{ ma/inc/matariffds.i }
{ ma/inc/mainsurertt.i  }
{ ma/inc/maaudittt.i  }

{ mip/inc/miptemptables.i  &TempTableName = ttValidation }

// Variables commonly used by WarpSpeed
DEFINE VARIABLE goWob                    AS cls.mipwswob            NO-UNDO.

// Variables for this specific WOB
DEFINE VARIABLE gcContainerOwningEntityList AS CHARACTER             NO-UNDO.
DEFINE VARIABLE gcSearchMethod              AS CHARACTER             NO-UNDO.
DEFINE VARIABLE glEnquiryWob                AS LOGICAL               NO-UNDO.
DEFINE VARIABLE glSuccess                   AS LOGICAL               NO-UNDO.

// Containers
DEFINE VARIABLE goCntSearchFilter        AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntATCClass            AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntNappi               AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntBasket              AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntTariff              AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntAudit               AS cls.mipwscontainer      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-appendAjaxReturnValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendAjaxReturnValues Procedure
FUNCTION appendAjaxReturnValues RETURNS LOGICAL
  (INPUT-OUTPUT ipocReturnValueList  AS CHARACTER,
   INPUT        ipcValue             AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-returnDBEntity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD returnDBEntity Procedure
FUNCTION returnDBEntity RETURNS cls.mipdbentity
  (INPUT ipcOwningEntityMnemonic  AS CHARACTER,
   INPUT iprRowID                 AS ROWID,
   INPUT ipdObj                   AS DECIMAL,
   INPUT ipcKey                   AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow:
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB)
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 16.24
         WIDTH              = 53.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure


/* ***************************  Main Block  *************************** */
/*------------------------------------------------------------------------------
  This is a WarpSpeed Warpspeed - include ma/inc/mastructure.i, and nothing else.
------------------------------------------------------------------------------*/

  ASSIGN goWob = WarpSpeed:CurrentObject.

  { mip/inc/mipcatcherror.i }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveAuthCopayDetItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveAuthCopayDetItem Procedure
PROCEDURE ajaxSaveAuthCopayDetItem :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters: ipcValidationArgument
              ttValidation
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE oRequestHelper    AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper   AS cls.maajaxresponsehelper   NO-UNDO.
  DEFINE VARIABLE oAuthCopayDetItem AS cls.maauthcopaydetitem     NO-UNDO.

  DEFINE VARIABLE hErrorHandle            AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dEffectiveDate          AS DATE       NO-UNDO.
  DEFINE VARIABLE dEndDate                AS DATE       NO-UNDO.
  DEFINE VARIABLE cAction                 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cContainerCode          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningKey              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningAltValue         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRecordAction           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWarningMessage         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWarningMessageType     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLineNumber             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOptionCode             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPrType                 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dAuthCopayDetailItemObj AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAuthCopayTypeObj       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCopaymentValue         AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dInsurerObj             AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dOwningObj              AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE lApplyToPmb             AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lApplyToEmergency       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lCopaymentValueType     AS LOGICAL    NO-UNDO.

  ASSIGN cContainerCode        = ipcValidationArgument
         oRequestHelper        = NEW cls.maajaxrequesthelper(INPUT get-value('FldLst'), INPUT get-value('ValList'))
         oResponseHelper       = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
         oAuthCopayDetItem     = NEW cls.maauthcopaydetitem()

         iLineNumber             =       INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U             + cContainerCode))
         cAction                 =               oRequestHelper:getFieldValue("fcAction":U                 + cContainerCode)
         cRecordAction           =               oRequestHelper:getFieldValue("fcRecordAction":U           + cContainerCode)
         dAuthCopayDetailItemObj =       DECIMAL(oRequestHelper:getFieldValue("fdAuthCopayDetailItemObj":U + cContainerCode))
         dAuthCopayTypeObj       =       DECIMAL(oRequestHelper:getFieldValue("fdCopaymentTypeObj":U       + cContainerCode))
         dInsurerObj             =       DECIMAL(oRequestHelper:getFieldValue("fdInsurerObj":U             + cContainerCode))
         iOptionCode             =       INTEGER(oRequestHelper:getFieldValue("fiOptionCode":U             + cContainerCode))
         cOwningEntityMnemonic   =               oRequestHelper:getFieldValue("fcOwningEntityArgument":U   + cContainerCode)
         dOwningObj              =       DECIMAL(oRequestHelper:getFieldValue("fdOwningObj":U              + cContainerCode))
         cOwningKey              =               oRequestHelper:getFieldValue("fcOwningKey":U              + cContainerCode)
         cOwningAltValue         =               oRequestHelper:getFieldValue("fcOwningAltValue":U         + cContainerCode)
         iPrType                 =       INTEGER(oRequestHelper:getFieldValue("fcPrType":U                 + cContainerCode))
         lCopaymentValueType     = IF            oRequestHelper:getFieldValue("cbCopaymentValueType"       + cContainerCode) = "":U
                                   THEN ?
                                   ELSE LOGICAL( oRequestHelper:getFieldValue("cbCopaymentValueType"       + cContainerCode) )
         dCopaymentValue         =       DECIMAL(oRequestHelper:getFieldValue("fdCopaymentValue":U         + cContainerCode))
         cWarningMessage         =               oRequestHelper:getFieldValue("fcWarningMessage":U         + cContainerCode)
         cWarningMessageType     =               oRequestHelper:getFieldValue("cbWarningMessageType":U     + cContainerCode)
         lApplyToPmb             =           NOT oRequestHelper:getFieldValue("flApplyToPMB":U             + cContainerCode) = "":U
         lApplyToEmergency       =           NOT oRequestHelper:getFieldValue("flApplyToEmergency":U       + cContainerCode) = "":U
         dEffectiveDate          =          DATE(oRequestHelper:getFieldValue("fdEffectiveDate"            + cContainerCode))
         dEndDate                =          DATE(oRequestHelper:getFieldValue("fdEndDate"                  + cContainerCode))   NO-ERROR.

  IF {&ErrorStatus} THEN
  DO:
    ASSIGN oResponseHelper:RequestValid    = FALSE
           oResponseHelper:ResponseMessage = "Invalid input recieved.":U
           oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'Description':U).

    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }

    RETURN .
  END. //IF {&ErrorStatus} THEN

  //Try to focus the current record
  IF dAuthCopayDetailItemObj > 0
  THEN
    oAuthCopayDetItem:focusAuthCopayDetailItem(dAuthCopayDetailItemObj) NO-ERROR.

  {&ResetError}

  CASE cAction:
    WHEN "modify":U THEN
    DO:
      ASSIGN  oAuthCopayDetItem:AuthCopayDetailItemObj  = dAuthCopayDetailItemObj WHEN NOT oAuthCopayDetItem:AuthCopayDetailItemInFocus
              oAuthCopayDetItem:AuthCopayTypeObj        = dAuthCopayTypeObj
              oAuthCopayDetItem:InsurerObj              = dInsurerObj
              oAuthCopayDetItem:OptionCode              = iOptionCode
              oAuthCopayDetItem:OwningEntityMnemonic    = cOwningEntityMnemonic
              oAuthCopayDetItem:OwningObj               = dOwningObj
              oAuthCopayDetItem:OwningKey               = cOwningKey
              oAuthCopayDetItem:OwningAltValue          = cOwningAltValue
              oAuthCopayDetItem:PrType                  = iPrType
              oAuthCopayDetItem:CopaymentValueType      = lCopaymentValueType
              oAuthCopayDetItem:CopaymentValue          = dCopaymentValue
              oAuthCopayDetItem:WarningMessage          = cWarningMessage
              oAuthCopayDetItem:WarningMessageType      = cWarningMessageType
              oAuthCopayDetItem:ApplyToPmb              = lApplyToPmb
              oAuthCopayDetItem:ApplyToEmergency        = lApplyToEmergency
              oAuthCopayDetItem:EffectiveDate           = dEffectiveDate
              oAuthCopayDetItem:EndDate                 = dEndDate .

      oAuthCopayDetItem:saveAuthCopayDetailItem().

    END. //WHEN "modify":U THEN
    WHEN "delete":U THEN
    DO:
      IF oAuthCopayDetItem:AuthCopayDetailItemInFocus  THEN
      DO:

        //Try to delete the record
        oAuthCopayDetItem:removeAuthCopayDetailItem() NO-ERROR.


      END. //IF oAuthCopayDetItem:InFocus THEN


    END. // WHEN "delete"
    OTHERWISE
    DO:
      ASSIGN oResponseHelper:RequestValid    = FALSE
             oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported":U, cAction)
             oResponseHelper:ResponseMessage = "Unable to perform action":U .

      RETURN.
    END. // OTHERWISE

  END CASE . //cAction

  IF {&ErrorStatus} OR oAuthCopayDetItem:ErrorObject:ErrorsExist THEN
  DO:
    ASSIGN oResponseHelper:RequestValid     = FALSE
           oResponseHelper:ResponseMessage  = SUBSTITUTE("Unable to perform action &1" , cAction)
           oResponseHelper:ReturnValue      = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'Description':U)
           hErrorHandle                     = oAuthCopayDetItem:ErrorObject:getErrorTableHandle()
           glSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
           glSuccess                        = oResponseHelper:setError(hErrorHandle)    .

  END. //IF {&ErrorStatus} OR oAuthCopayDetItem:ErrorObject:ErrorsExist
  ELSE DO:
    ASSIGN
      oResponseHelper:RequestValid    = TRUE
      oResponseHelper:ResponseMessage = SUBSTITUTE("Action &1 successfully completed.":U , cAction)
      oResponseHelper:ReturnValue     = SUBSTITUTE("Action &1 successfully completed.":U , cAction)

      glSuccess                       = oResponseHelper:addFieldValue("fdAuthCopayDetailItemObj":U + cContainerCode, STRING(oAuthCopayDetItem:AuthCopayDetailItemObj)).
  END. //ELSE DO

  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)    THEN DELETE OBJECT oRequestHelper    NO-ERROR.
                                        IF VALID-OBJECT(oResponseHelper)   THEN DELETE OBJECT oResponseHelper   NO-ERROR.
                                        IF VALID-OBJECT(oAuthCopayDetItem) THEN DELETE OBJECT oAuthCopayDetItem NO-ERROR." }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidation Procedure
PROCEDURE ajaxValidation :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE cAlertMessage             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cArsRate                  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cATCClassKey              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBaseRate                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCopayTypeCode            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cFilterField              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cFilterFieldList          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cFilterValue              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cFilterValueList          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cError                    AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cWarning                  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningKey                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningAltValue           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningRecordDescription  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPrType                   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cReturnFieldList          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cReturnField              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cReturnValueList          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cValidationArgument       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE iCount                    AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iResults                  AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE dStartDate                AS DATE                      NO-UNDO.
  DEFINE VARIABLE dOwningObj                AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTariffObj                AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTrfCostObj               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE rOwningRowID              AS ROWID                     NO-UNDO.
  DEFINE VARIABLE lTariffLinkDefault        AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE oDBEntity                 AS cls.mipdbentity           NO-UNDO.
  DEFINE VARIABLE oCrosswalkSearch          AS ma.cls.macrosswalksearch  NO-UNDO.
  DEFINE VARIABLE oTariffSearch             AS ma.cls.matariffsearch     NO-UNDO.
  DEFINE VARIABLE oNappiSearch              AS cls.manappisearch         NO-UNDO.
  DEFINE VARIABLE lcTariffTT                AS LONGCHAR                  NO-UNDO.

  CREATE ttValidation.

  ASSIGN
    cValidationArgument   = ENTRY(1,ipcValidationArgument, "|":U)
    cFilterFieldList      = get-value("FldLst":U)
    cFilterValueList      = get-value("ValList")
    cReturnFieldList      = get-value("RetFldList") .

  //Retrieve values from ajax request
  FILTER-FIELD-BLK:
  DO iCount = 1 TO NUM-ENTRIES(cFilterFieldList) :

    ASSIGN cFilterField = ENTRY(iCount, cFilterFieldList)
           cFilterValue = ENTRY(iCount, cFilterValueList) .

    CASE cFilterField:
      // Filter fields for default owning entity validation
      WHEN "[OwningEntityMnemonic]":U THEN ASSIGN  cOwningEntityMnemonic   =          cFilterValue  NO-ERROR.
      WHEN "[OwningObj]":U            THEN ASSIGN  dOwningObj              =  DECIMAL(cFilterValue) NO-ERROR.
      WHEN "[OwningKey]":U            THEN ASSIGN  cOwningKey              =          cFilterValue  NO-ERROR.
      WHEN "[OwningAltValue]":U       THEN ASSIGN  cOwningAltValue         =          cFilterValue  NO-ERROR.
      WHEN "[OwningRowId]":U          THEN ASSIGN  rOwningRowId            = TO-ROWID(cFilterValue) NO-ERROR.
      WHEN "[Default]":U              THEN ASSIGN  lTariffLinkDefault      =  LOGICAL(cFilterValue) NO-ERROR.
      WHEN "[ArsRate]":U              THEN ASSIGN  cArsRate                =          cFilterValue  NO-ERROR.
      WHEN "[BaseRate]":U             THEN ASSIGN  cBaseRate               =          cFilterValue  NO-ERROR.
      WHEN "[PrType]":U               THEN ASSIGN  cPrType                 =          cFilterValue  NO-ERROR.
      WHEN "[StartDate]":U            THEN ASSIGN  dStartDate              =     DATE(cFilterValue) NO-ERROR.
    END CASE . //CASE cFilterField

    {&ResetError}

  END. // FILTER-FIELD-BLK

  //Run argument specific validations
  CASE cValidationArgument:

    WHEN "OwningEntityValidation":U THEN
    DO:

      IF  cOwningKey      = "":U
      AND rOwningRowID    = ?
      AND dOwningObj      = 0
      AND cOwningAltValue = "":U THEN
      DO:

        ASSIGN ttValidation.cReturnValues = FILL(" |":U, NUM-ENTRIES(cReturnFieldList) - 1)
               ttValidation.lValid        = TRUE.

        RETURN.
      END. // IF cOwningKey = "" AND rOwningRowID = ? AND dOwningObj = 0

      // If no entity was specified in the filter fields, check if the entity is specified in the ajax validation argument
      IF cOwningEntityMnemonic = "":U
      THEN
        ASSIGN cOwningEntityMnemonic = IF NUM-ENTRIES(ipcValidationArgument , "|":U) > 1
                                       THEN   ENTRY(2,ipcValidationArgument,  "|":U)
                                       ELSE "":U .

      CASE cOwningEntityMnemonic:
        WHEN "htmtl":U THEN
        DO:

          ASSIGN dOwningObj = 0 .

          DATASET dsTariff:EMPTY-DATASET() .

          ASSIGN oTariffSearch        = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE)
                 glSuccess            = oTariffSearch:SetCriteria("BufferList":U, "tt_tariff_link":U)
                 glSuccess = oTariffSearch:SetCriteria("BatchSize":U,  "50":U)
                 glSuccess = oTariffSearch:SetFilterCriteria("tt_tariff_link.tariff_code":U,  "=":U, cOwningAltValue)
                 glSuccess =                              oTariffSearch:SetFilterCriteria("tt_tariff_link.tariff_link_default":U,  "=":U, lTariffLinkDefault)
                 glSuccess = IF cBaseRate <> "":U    THEN oTariffSearch:SetFilterCriteria("tt_tariff_link.base_rate":U, "=":U, cBaseRate) ELSE TRUE
                 glSuccess = IF cArsRate  <> "":U    THEN oTariffSearch:SetFilterCriteria("tt_tariff_link.ars_rate":U,  "=":U, cArsRate)  ELSE TRUE
                 glSuccess = IF cPrType   <> "000":U THEN oTariffSearch:SetFilterCriteria("tt_tariff_link.pr_type":U,   "=":U, cPrType)   ELSE TRUE .

          oTariffSearch:fetchTariffData() .

          TEMP-TABLE tt_tariff_link:WRITE-JSON("LONGCHAR",lcTariffTT , FALSE ) .

          ASSIGN ttValidation.cReturnValues = "PARTIAL":U  + REPLACE(STRING(lcTariffTT), "|":U, "-":U)
                 ttValidation.lValid        = TRUE .



          IF  cls.miperror:getMessageGroupNumber() = "PROGRESS:9324":U THEN
          DO:
            //When this happens , it means the search returned too many records . The data package is bigger than 32kb
            //In this case we'll try to narrow the criteria to find the applicable record
            {&ResetError}



          END. //IF cls.miperror:getMessageGroupNumber() = "PROGRESS:9324":U THEN

          VALIDATE ttValidation.

          RETURN .

        END. //WHEN "htmtl":U THEN
        WHEN "hlmcr":U THEN
        DO:
            DATASET dsCrosswalk:EMPTY-DATASET() .

            // If we get a basket code , we'll find the first matching record . There could be potentially be multiple baskets set up with the same code
            ASSIGN oCrosswalkSearch = NEW ma.cls.macrosswalksearch(DATASET dsCrosswalk:HANDLE)
                   glSuccess        = oCrosswalkSearch:setCriteria("BufferList":U, "tt_crosswalk":U)
                   glSuccess        = oCrosswalkSearch:setFilterCriteria("tt_crosswalk.crosswalk_code":U, "=":U, cOwningKey).

            oCrosswalkSearch:fetchCrosswalkData().

            FOR FIRST tt_crosswalk:
              ASSIGN dOwningObj = tt_crosswalk.crosswalk_obj .

            END. // FOR FIRST tt_crosswalk

        END. //  WHEN "hlmcr"
        WHEN "hlmnl":U THEN
        DO:

          DATASET dsNappi:EMPTY-DATASET() .

          ASSIGN oNappiSearch   = NEW cls.manappisearch(DATASET dsNappi BY-REFERENCE)
                 glSuccess      = oNappiSearch:setCriteria("activeLink":U, "tt_nappi_link":U)
                 glSuccess      = oNappiSearch:SetCriteria("BufferList":U, "tt_nappi_link":U).  /* Restrict the buffers to be filled by the data service */
                 glSuccess      = oNappiSearch:SetFilterCriteria("tt_nappi_link.nappi_code_prefix":U,   "=":U, cOwningKey) .

          //Fetch
          oNappiSearch:fetchData().

          FOR FIRST tt_nappi_link:
              ASSIGN dOwningObj = tt_nappi_link.nappi_link_obj
                     cOwningKey = "".

          END. // FOR FIRST tt_crosswalk

        END. //  WHEN "hlmnl"
        OTHERWISE
        DO:
          // nothing for now

        END. // OTHERWISE
      END. //CASE cOwningEntityMnemonic

      // Focus entity using key/obj/rowid
      ASSIGN oDBEntity = returnDBEntity(cOwningEntityMnemonic,
                                        rOwningRowId ,
                                        dOwningObj ,
                                        cOwningKey ) NO-ERROR.


      IF  oDBEntity:InFocus
      AND oDBEntity:RecordAvailable THEN
      DO:

        //Prepare return values
        RETURN-VALUE-BLK:
        DO iCount = 1 TO NUM-ENTRIES(cReturnFieldList):

          ASSIGN cReturnField = ENTRY(iCount , cReturnFieldList).

          CASE cReturnField :
            WHEN "[OwningKey]":U                THEN appendAjaxReturnValues(cReturnValueList,       oDBEntity:RecordKey) .
            WHEN "[OwningObj]":U                THEN appendAjaxReturnValues(cReturnValueList,STRING(oDBEntity:RecordObj)).
            WHEN "[OwningRecordDescription]":U  THEN appendAjaxReturnValues(cReturnValueList,       oDBEntity:RecordDescription).
            WHEN "[OwningRecordLabel]":U        THEN appendAjaxReturnValues(cReturnValueList,       oDBEntity:RecordLabel).
            WHEN "[OwningCode]":U               THEN appendAjaxReturnValues(cReturnValueList,       oDBEntity:RecordCode).
            OTHERWISE                                appendAjaxReturnValues(cReturnValueList,"":U).
          END CASE. // cReturnField
        END. // RETURN-VALUE-BLK

      END. //IF oDBEntity:RecordAvailable
      ELSE IF cMessage = "":U
      THEN
        ASSIGN cMessage = SUBSTITUTE("&1(&2) record could not be found using " ,oDBEntity:TableLabel, cOwningEntityMnemonic )
                        + IF rOwningRowId <> ? THEN "Row ID:"       + STRING(rOwningRowID)       ELSE "":U
                        + IF cOwningKey   <> ? THEN "Owning Key:~'" + STRING(cOwningKey) + "~'"  ELSE "":U
                        + IF dOwningObj   <> ? THEN "Owning Obj:"   + STRING(dOwningObj)         ELSE "":U .

      IF cReturnValueList = "":U
      THEN
        ASSIGN cReturnValueList =  FILL("|":U, NUM-ENTRIES(cReturnFieldList) - 1) .

      ASSIGN  ttValidation.lValid          = oDBEntity:RecordAvailable
              ttValidation.cMessage        = cMessage
              ttValidation.cReturnValues   = REPLACE( cReturnValueList, "[blank]":U , "":U ) .

    END. // WHEN "OwningEntityValidation":U
  END CASE. // CASE cValidationArgument

  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oDBEntity)        THEN DELETE OBJECT oDBEntity.
                                        IF VALID-OBJECT(oTariffSearch)    THEN DELETE OBJECT oTariffSearch.
                                        IF VALID-OBJECT(oCrosswalkSearch) THEN DELETE OBJECT oCrosswalkSearch.
                                        IF VALID-OBJECT(oNappiSearch)     THEN DELETE OBJECT oNappiSearch   .
                                        DATASET dsCrosswalk:EMPTY-DATASET() .
                                        DATASET dsTariff:EMPTY-DATASET() ." }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-businessLogic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE businessLogic Procedure
PROCEDURE businessLogic :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE INPUT  PARAMETER ipcWhatToDo  AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE oContainer           AS cls.mipwscontainer NO-UNDO.
  DEFINE VARIABLE iCount               AS INTEGER            NO-UNDO.
  DEFINE VARIABLE iRow                 AS INTEGER            NO-UNDO.
  DEFINE VARIABLE cOwningEntity        AS CHARACTER          NO-UNDO.

  DEFINE VARIABLE dEffectiveDate          AS DATE       NO-UNDO.
  DEFINE VARIABLE dEndDate                AS DATE       NO-UNDO.
  DEFINE VARIABLE dObjCounter             AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAction                 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cContainerCode          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningKey              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningAltValue         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRecordAction           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWarningMessage         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWarningMessageType     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLineNumber             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOptionCode             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPrType                 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dAuthCopayDetailItemObj AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAuthCopayTypeObj       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCopaymentValue         AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dInsurerObj             AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dOwningObj              AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE lApplyToPmb             AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lApplyToEmergency       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lCopaymentValueType     AS LOGICAL    NO-UNDO.

  DEFINE BUFFER btt_auth_copay_detail_item FOR tt_auth_copay_detail_item .

  DEFINE VARIABLE lSuccess    AS LOGICAL NO-UNDO.

  ASSIGN gcContainerOwningEntityList  = "hlmac,hlmnl,hlmcr,htmtl":U .

  IF goWob:SubmitValue = "SearchSubmit":U THEN
  DO:
    OWNING-CONTAINER-BLK:
    DO iCount = 1 TO NUM-ENTRIES(gcContainerOwningEntityList):

      ASSIGN cOwningEntity  = ENTRY(iCount , gcContainerOwningEntityList)
             oContainer     = goWob:getContainer(cOwningEntity)
             cContainerCode = oContainer:ContainerCode.

      ROW-BLK:
      DO iRow = 1  TO INTEGER(get-value(cContainerCode + "_rowsrendered":U)):

        ASSIGN iLineNumber             =       INTEGER(get-value("fiLineNumber":U             + cContainerCode + STRING(iRow)))
               cAction                 =               get-value("fcAction":U                 + cContainerCode + STRING(iRow))
               cRecordAction           =               get-value("fcRecordAction":U           + cContainerCode + STRING(iRow))
               dAuthCopayDetailItemObj =       DECIMAL(get-value("fdAuthCopayDetailItemObj":U + cContainerCode + STRING(iRow)))
               dAuthCopayTypeObj       =       DECIMAL(get-value("fdCopaymentTypeObj":U       + cContainerCode + STRING(iRow)))
               dInsurerObj             =       DECIMAL(get-value("fdInsurerObj":U             + cContainerCode + STRING(iRow)))
               iOptionCode             =       INTEGER(get-value("fiOptionCode":U             + cContainerCode + STRING(iRow)))
               cOwningEntityMnemonic   =               get-value("fcOwningEntityArgument":U   + cContainerCode + STRING(iRow))
               dOwningObj              =       DECIMAL(get-value("fdOwningObj":U              + cContainerCode + STRING(iRow)))
               cOwningKey              =               get-value("fcOwningKey":U              + cContainerCode + STRING(iRow))
               cOwningAltValue         =               get-value("fcOwningAltValue":U         + cContainerCode + STRING(iRow))
               iPrType                 =       INTEGER(get-value("fcPrType":U                 + cContainerCode + STRING(iRow)))
               lCopaymentValueType     = IF            get-value("cbCopaymentValueType"       + cContainerCode + STRING(iRow)) = "":U
                                         THEN ?
                                         ELSE LOGICAL( get-value("cbCopaymentValueType"       + cContainerCode + STRING(iRow)) )
               dCopaymentValue         =  DECIMAL(TRIM(get-value("fdCopaymentValue":U         + cContainerCode + STRING(iRow)), "%":U))
               cWarningMessage         =               get-value("fcWarningMessage":U         + cContainerCode + STRING(iRow))
               cWarningMessageType     =               get-value("cbWarningMessageType":U     + cContainerCode + STRING(iRow))
               lApplyToPmb             =           NOT get-value("flApplyToPMB":U             + cContainerCode + STRING(iRow)) = "":U
               lApplyToEmergency       =           NOT get-value("flApplyToEmergency":U       + cContainerCode + STRING(iRow)) = "":U
               dEffectiveDate          =          DATE(get-value("fdEffectiveDate"            + cContainerCode + STRING(iRow)))
               dEndDate                =          DATE(get-value("fdEndDate"                  + cContainerCode + STRING(iRow)))   NO-ERROR.

        {&ResetError}

        IF  cOwningEntityMnemonic  = "":U
        OR (dOwningObj             =  0
        AND cOwningKey             = "":U )
        OR  lCopaymentValueType    = ?
        OR  dEffectiveDate         = ?
        THEN
          NEXT ROW-BLK.

        FIND FIRST btt_auth_copay_detail_item
             WHERE btt_auth_copay_detail_item.insurer_obj             = dInsurerObj
               AND btt_auth_copay_detail_item.option_code             = iOptionCode
               AND btt_auth_copay_detail_item.owning_entity_mnemonic  = cOwningEntityMnemonic
               AND btt_auth_copay_detail_item.owning_obj              = dOwningObj
               AND btt_auth_copay_detail_item.owning_key              = cOwningKey
               AND btt_auth_copay_detail_item.pr_type                 = iPrType
               AND btt_auth_copay_detail_item.effective_date          = dEffectiveDate NO-ERROR .

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT AVAILABLE btt_auth_copay_detail_item THEN
        DO:

          CREATE tt_auth_copay_detail_item .

          ASSIGN dObjCounter                                          = dObjCounter - 1999
                 tt_auth_copay_detail_item.record_action              = IF  cRecordAction <> "DELETE"
                                                                        AND cRecordAction <> "":U
                                                                        THEN "MODIFY":U
                                                                        ELSE cRecordAction

                 tt_auth_copay_detail_item.line_number                = iLineNumber
                 tt_auth_copay_detail_item.insurer_obj                = dInsurerObj
                 tt_auth_copay_detail_item.option_code                = iOptionCode
                 tt_auth_copay_detail_item.auth_copay_detail_item_obj = IF dAuthCopayDetailItemObj > 0 THEN dAuthCopayDetailItemObj ELSE dObjCounter
                 tt_auth_copay_detail_item.auth_copay_type_obj        = dAuthCopayTypeObj
                 tt_auth_copay_detail_item.effective_date             = dEffectiveDate
                 tt_auth_copay_detail_item.end_date                   = dEndDate
                 tt_auth_copay_detail_item.owning_entity_mnemonic     = cOwningEntityMnemonic
                 tt_auth_copay_detail_item.owning_obj                 = dOwningObj
                 tt_auth_copay_detail_item.owning_key                 = cOwningKey
                 tt_auth_copay_detail_item.owning_alt_value           = cOwningAltValue
                 tt_auth_copay_detail_item.pr_type                    = iPrType
                 tt_auth_copay_detail_item.copayment_value_type       = lCopaymentValueType
                 tt_auth_copay_detail_item.copayment_value            = dCopaymentValue
                 tt_auth_copay_detail_item.apply_to_pmb               = lApplyToPmb
                 tt_auth_copay_detail_item.apply_to_emergency         = lApplyToEmergency
                 tt_auth_copay_detail_item.warning_message            = cWarningMessage
                 tt_auth_copay_detail_item.warning_message_type       = cWarningMessageType  .

          VALIDATE tt_auth_copay_detail_item.

        END . // IF NOT AVAILABLE btt_auth_copay_detail_item
      END. //ROW-BLK
    END. // OWNING-CONTAINER-BLK


    mipEnv:Health:AuthMaintenance:saveAuthCopayDetailItem(INPUT-OUTPUT DATASET dsAuthCopayDetailItem BY-REFERENCE) NO-ERROR.

    IF CAN-FIND(FIRST tt_auth_copay_detail_item_error )
    THEN
      ASSIGN Warpspeed:ValidationError = TRUE .


  END. //IF goWob:SubmitValue = "SearchSubmit":U THEN

  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-customRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customRenderProcedure Procedure
PROCEDURE customRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:    left in case future render arguments are required
  Parameters:  ipoControl object about to be rendered
  Notes:
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.

  DEFINE VARIABLE dCopaymentTypeObj   AS DECIMAL        NO-UNDO.
  DEFINE VARIABLE cCopaymentTypeCode  AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE oQuery              AS cls.mipquery   NO-UNDO.
  DEFINE VARIABLE cCopayValueType     AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE cWarningMessageType AS CHARACTER      NO-UNDO.

  DEFINE BUFFER btt_auth_copay_type FOR tt_auth_copay_type .
  DEFINE BUFFER btt_atc_class       FOR tt_atc_class .

  CASE ipoControl:RenderArgument:
    WHEN "WarningMessage":U THEN
    DO:
      ASSIGN cWarningMessageType =  ipoControl:ParentContainer:ContainerQuery:getFieldAttribute("tt_auth_copay_detail_item.warning_message_type":U, "BUFFER-VALUE":U).

      IF cWarningMessageType <> "":U
      THEN
        ASSIGN ipoControl:ControlClass = "+clMan":U .
      ELSE
        ASSIGN ipoControl:ControlClass = "-clMan":U .

      ipoControl:renderAsTextArea() .

    END. //WHEN "WarningMessage":U THEN

  END CASE. // CASE ipoControl:RenderArgument:

  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oQuery) THEN DELETE OBJECT oQuery." }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainer Procedure
PROCEDURE defineContainer :
/*------------------------------------------------------------------------------
  Purpose:  Define all containers on the wob
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE lUpdatableContainer  AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oContainer           AS cls.mipwscontainer        NO-UNDO.

  DEFINE VARIABLE iCount                       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cContainerCode               AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSearchOwningEntityMnemonic  AS CHARACTER NO-UNDO.

  ASSIGN glEnquiryWob         = get-value("wobMode") = "enquiry":U
         WarpSpeed:MenuStatus = "CLOSE"
         lUpdatableContainer  = NOT glEnquiryWob .

  // Search filter
  RUN defineContainerSearchFilter IN TARGET-PROCEDURE .

  goCntSearchFilter:populateFromRequest().

  ASSIGN gcContainerOwningEntityList  = "hlmac,hlmcr,hlmnl,htmtl":U
         cContainerCode               =  goCntSearchFilter:ContainerCode
         cSearchOwningEntityMnemonic  =  goCntSearchFilter:getControl("cbOwningEntity":U      + cContainerCode ):ControlValue.

  //*** Search results/maint containers
  RESULT-CONTAINER-DEFINITION-BLK:
  DO iCount = 1 TO NUM-ENTRIES(gcContainerOwningEntityList):

    ASSIGN cOwningEntityMnemonic  = ENTRY(iCount, gcContainerOwningEntityList) .

    RUN _defineContainerAuthCopayDetailItemGeneral IN TARGET-PROCEDURE(INPUT  cOwningEntityMnemonic , //ipcOwningEntityMnemonic
                                                                       OUTPUT oContainer).            //opoContainer

    RUN _customizeContainerForEntity               IN TARGET-PROCEDURE(INPUT cOwningEntityMnemonic ,  // ipcOwningEntityMnemonic
                                                                       INPUT-OUTPUT oContainer ).     // iopoContainer

    //Container properties
    ASSIGN
      oContainer:Collapsed                        = IF  cSearchOwningEntityMnemonic <> "":U
                                                    AND cSearchOwningEntityMnemonic <> oContainer:ContainerCode
                                                    THEN TRUE
                                                    ELSE FALSE
      oContainerProperties                        =  NEW cls.wscontainerproperties(oContainer)
      oContainerProperties:AutoSaveOperation      = "START:" + goWob:ObjectCode + ":ajaxSaveAuthCopayDetItem:":U + oContainer:ContainerCode
      oContainerProperties:DefaultLess            = FALSE
      oContainerProperties:CollapsableControlList = "fiOptionCode":U         + oContainer:ContainerCode
                                                  + ",fcInsurer":U           + oContainer:ContainerCode
                                                  + ",fdEndDate":U           + oContainer:ContainerCode
                                                  + ",flApplyToPMB":U        + oContainer:ContainerCode
                                                  + ",flApplyToEmergency":U  + oContainer:ContainerCode.

    { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = lUpdatableContainer &Container = oContainer &ContainerProperties = oContainerProperties}



    goWob:setContainer(cOwningEntityMnemonic, oContainer) .

  END. //RESULT-CONTAINER-DEFINITION-BLK

  ASSIGN goCntATCClass = goWob:getContainer("hlmac":U)
         goCntNappi    = goWob:getContainer("hlmnl":U)
         goCntBasket   = goWob:getContainer("hlmcr":U)
         goCntTariff   = goWob:getContainer("htmtl":U) .

 RUN defineContainerAudit IN TARGET-PROCEDURE .

  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerAudit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerAudit Procedure
PROCEDURE defineContainerAudit :
/*------------------------------------------------------------------------------
  Purpose:  Define search/maint container for audit container`
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 10195 &THEN
  ASSIGN
    goCntAudit                = mipEnv:Health:maUtility:getAuditContainer()
    goCntAudit:RowsToRender   = ?
    goCntAudit:ContainerWidth = "96%":U.

  { ma/inc/wsupdatetablecontainer.i &ContainerType  = "'TABLE'"
                                    &UpdatableTable = FALSE
                                    &Container      = goCntAudit
                                    &RenderSequence = 80}

  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerSearchFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerSearchFilter Procedure
PROCEDURE defineContainerSearchFilter :
/*------------------------------------------------------------------------------
  Purpose:  Define search filter container
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE oControl             AS cls.mipwscontrol   NO-UNDO.
DEFINE VARIABLE cContainerCode       AS CHARACTER          NO-UNDO.
&IF {&DBDFMA} >= 10195 &THEN
ASSIGN
  goCntSearchFilter                  = goWob:getContainer("SearchFilter":U)
  cContainerCode                     = goCntSearchFilter:ContainerCode
  goCntSearchFilter:ContainerTitle   = " Search":U

  oControl                           = goCntSearchFilter:addControl("fdCopaymentTypeObj":U  + cContainerCode,  "wsInput":U ,         "20":U, "":U, "character":U, 1, 1, "":U)
  oControl:ControlToken              = "hidden"
  oControl                           = goCntSearchFilter:addControl("fcCopaymentType":U  + cContainerCode,  "wsInput":U ,         "10":U, "":U, "character":U, 1, 1, "":U)
  oControl:AjaxValidation            = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation|hamct":U
  oControl:FilterFields              = "[OwningKey]":U
  oControl:FilterControls            = "fcCopaymentType":U  + cContainerCode
  oControl:ReturnFields              = "[OwningObj]":U
  oControl:ReturnControls            = "fdCopaymentTypeObj" + cContainerCode
  oControl                           = goCntSearchFilter:addControl("buCopaymentType":U  + cContainerCode,  "wsLookupButton":U ,  "20":U, "":U, "character":U, 1, 1, "Co-Payment Type":U)
  oControl:LookupWobFLA              = "hamct":U
  oControl:LookupControls            = "fcCopaymentType":U + cContainerCode
  oControl:LookupFields              = "ham_auth_copay_type.auth_copay_type":U
  oControl:FilterControls            = "fcCopaymentType":U + cContainerCode
  oControl:FilterFields              = "ham_auth_copay_type.auth_copay_type":U
  oControl:CellLayOutMask            = "&1&2&3":U

  oControl                           = goCntSearchFilter:addControl("cbOwningEntity":U    + cContainerCode,    "wsCombo":U ,      "10":U, "":U , "character":U, 2, 1, "Detail Entity":U)
  oControl:JavascriptOnChange        = "fnOnChangeOwningEntity(this);":U
  oControl:AdditionalItems           = "All=":U
  oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK":U
                                     + "   WHERE mic_acronym.category_key = 'ma_acAuthCopayDetailItemEntities'":U
                                     + "      BY mic_acronym.acronym_label":U
  oControl:KeyField                  = "mic_acronym.acronym_value":U
  oControl:DisplayFields             = "mic_acronym.acronym_label":U


  oControl                           = goCntSearchFilter:addControl("fdOwningObj":U      + cContainerCode,    "wsInput":U,        "10":U, "":U,  "character":U, 3, 1, "":U)
  oControl:ControlToken              = "Hidden":U

  oControl                           = goCntSearchFilter:addControl("frOwningRowId":U    + cContainerCode,    "wsInput":U,        "10":U, "":U,  "character":U, 3, 1,  "":U)
  oControl:ControlToken              = "Hidden":U

  oControl                           = goCntSearchFilter:addControl("fcOwningKey":U      + cContainerCode,    "wsInput":U,        "15":U, "":U,   "character":U, 3, 1, "":U)
  oControl:ControlToken              = "hidden":U
  oControl                           = goCntSearchFilter:addControl("fcOwningAltValue":U + cContainerCode,    "wsInput":U,        "10":U, "":U,   "character":U, 3, 1, "":U)
  oControl:AjaxValidation            = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation":U
  oControl:FilterFields              = "[OwningKey],[OwningEntityMnemonic]":U
  oControl:FilterControls            = "fcOwningAltValue":U  + cContainerCode  + ",cbOwningEntity" + cContainerCode
  oControl:ReturnFields              = "[fdOwningObj]":U
  oControl:ReturnControls            = "fdOwningObj" + cContainerCode
  oControl                           = goCntSearchFilter:addControl("buOwningBtn":U      + cContainerCode   , "wsLookupButton":U,   "":U, "":U,   "":U         , 3, 1, "Detail Entity Code":U)
  oControl:CellLayoutMask            = "&1&2&3&4&5":U
  oControl:LookupWobFLA              = "slent":U
  oControl:LookupFields              = "CODE_FIELD":U
  oControl:LookupControls            = "fcOwningAltValue":U   + cContainerCode
  oControl:FilterFields              = "QUERY_OEM":U
  oControl:FilterControls            = "cbOwningEntity":U     + cContainerCode
  oControl:ReturnFields              = "CODE_FIELD,ROWID":U
  oControl:ReturnControls            = "fcOwningAltValue":U   + cContainerCode + ",frOwningRowId" + cContainerCode

  oControl                           = goCntSearchFilter:addControl("fcPrType":U         + cContainerCode,  "wsInput":U ,          "5":U, "":U, "character":U, 4, 1, "":U)
  oControl:JavascriptOnChange        = "fnDisciplinePad(this,~"|~")":U
  oControl:AjaxValidation            = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation|prtype":U
  oControl:FilterFields              = "[OwningKey]":U
  oControl:FilterControls            = "fcPrType":U  + cContainerCode
  oControl                           = goCntSearchFilter:addControl("buPrType":U         + cContainerCode,  "wsLookupButton":U ,   "20":U, "":U, "character":U, 4, 1, "Discipline":U)
  oControl:LookupWobFLA              = "maprtype":U
  oControl:LookupControls            = "fcPrType":U + cContainerCode
  oControl:LookupFields              = "prtype.pr-type":U
  oControl:FilterControls            = "fcPrType":U + cContainerCode
  oControl:FilterFields              = "prtype.pr-type":U
  oControl:CellLayOutMask            = "&1&2":U

  oControl                           = goCntSearchFilter:addControl("flApplyToPMB":U         + cContainerCode  ,  "wsCombo":U  , "1":U, "":U, "character":U,             5, 1, "Apply to PMB":U)
  oControl:AdditionalItems           = "|Yes=true|No=false":U

  oControl                           = goCntSearchFilter:addControl("flApplyToEmergency":U         + cContainerCode  ,  "wsCombo":U  , "1":U, "":U, "character":U,    6, 1, "Apply to Emergency":U)
  oControl:AdditionalItems           = "|Yes=true|No=false":U

  oControl                           = goCntSearchFilter:addControl("fdInsurerObj":U         + cContainerCode ,      "wsInput":U,   "15":U, "":U, "decimal":U,   3, 2, "":U)
  oControl:ControlToken              = "Hidden":U
  oControl                           = goCntSearchFilter:addControl("fcInsurer":U            + cContainerCode,       "wsInput":U,   "10":U, "":U, "character":U, 3, 2, "":U)
  oControl:AjaxValidation            = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation|ermin":U
  oControl:FilterFields              = "[OwningKey]":U
  oControl:FilterControls            = "fcInsurer":U  + cContainerCode
  oControl:ReturnFields              = "[OwningObj]":U
  oControl:ReturnControls            = "fdInsurerObj" + cContainerCode
  oControl                           = goCntSearchFilter:addControl("buInsurerBtn":U         + cContainerCode, "wsLookupButton":U,    "":U, "":U, "":U,          3, 2, "Client":U)
  oControl:CellLayoutMask            = "&1&2&3":U
  oControl:LookupWobFLA              = "ermin":U
  oControl:LookupFields              = "erm_insurer.insurer_name":U
  oControl:LookupControls             = "fcInsurer":U + cContainerCode
  oControl:ReturnControls            = "erm_insurer.insurer_obj,erm_insurer.insurer_name":U
  oControl:ReturnFields              = "fdInsurerObj" + cContainerCode + ",fcInsurer":U + cContainerCode
  oControl                           = goCntSearchFilter:addControl("fiOptionCode":U        + cContainerCode,        "wsInput":U    ,"10":U,  "":U, "integer":U, 4, 2, "":U)
  oControl:AjaxValidation            = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation|scheme":U
  oControl:FilterFields              = "[OwningKey]":U
  oControl:FilterControls            = "fiOptionCode":U  + cContainerCode
  oControl                           = goCntSearchFilter:addControl("buOptionBtn":U         + cContainerCode, "wsLookupButton":U    , "":U  , "":U, "":U,         4, 2, "Option":U)
  oControl:CellLayoutMask            = "&1&2":U
  oControl:LookupWobFla              = "scheme":U
  oControl:LookupFields              = "scheme.scheme-code":U
  oControl:LookupControls            = "fiOptionCode":U + cContainerCode
  oControl:FilterFields              = "scheme.scheme-code":U
  oControl:FilterControls            = "fiOptionCode":U + cContainerCode
  oControl:ReturnFields              = "scheme.scheme-code":U
  oControl:ReturnControls            = "fiOptionCode"   + cContainerCode

  oControl                           = goCntSearchFilter:addControl("fdEffectiveDate":U     + cContainerCode,     "wsInput":U,      "10":U, "":U, "date":U,     5, 2, "Effective Date":U)
  oControl:ControlToolTip            = "Filter according to Effective Date":U

  oControl                           = goCntSearchFilter:addControl("fdEndDate":U           + cContainerCode,     "wsInput":U,      "10":U, "":U, "date":U,     6, 2, "End Date":U)  .
  oControl:ControlToolTip            = "Filter according to End Date":U .

  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-outputCustomHeaderJS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputCustomHeaderJS Procedure
PROCEDURE outputCustomHeaderJS :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.

  RUN SUPER.

  {&OUT}
  "~n <script type='text/javascript' language='JavaScript1.2' src='/js/maauthcopaydetailitem.js'></script>":U
  "<style>":U
  " .clSelectLookup~{  flex-direction: column;":U
  "                      margin-top: 24px;":U
  "                      padding-top: 0;":U
  "                      width: 80%;":U
  "                      max-width: 80px;":U
  "                      background-color: #fff;":U
  "                      box-shadow: 3px 3px 0 0 rgb(0 0 0 / 20%); ":U
  "                      list-style: none;":U
  "                      overflow: clip;":U
  "                      border-radius : 4% ;":U
  "                      z-index: 999;":U
  "                      position: absolute; ~}":U

  "  .clSelectLookupItem ~{   margin: 0;":U
  "                           padding: 3px;":U
  "                           border-bottom: 1px solid #ecf0f1;":U
  "                           cursor: pointer;":U
  "                           position: relative;  ~}":U

  "  .clGreen ~{   background-color: #6e8d0b;":U
  "                                ~}":U

  ".invalid~{":U
  "            background-image : url(~"/images/invalid.svg~"); ":U
  "            width: 20px;":U
  "            height: 20px;":U
  "            transition : 125ms transform ;":U
  "~}"

  ".correct~{":U
  "            width: 20px;":U
  "            height: 20px;":U
  "            background-image : url(~"/images/correct.svg~");":U
  "            transition : 125ms transform ;":U
  "~}"

  ".clValidationIndicator~{":U
  "            background-repeat: no-repeat;":U
  "            background-size: contain;":U
  "            position: relative ;":U
  "~}":U


  " .clValidationIndicator::before,":U
  " .clValidationIndicator::after~{":U
  "     --scale : 0 ;":U
  "     --tooltip-color : #333; ":U
  "     --arrow-size : 10px ;":U
  "      position: absolute;":U
  "      opacity : 1 ;":U
  "      top: -0.5rem;":U
  "      left : 50% ;":U
  "      transform: translateX(-50%) translateY(var(--translateY,0)) scale(var(--scale));":U
  "      transition : 125ms transform ;":U
  "      transform-origin : bottom center ;":U
  " ~}":U

  " .clValidationIndicator::before~{":U
  "      --translateY : calc(-100% - var(--arrow-size));":U
  "      content : attr(title) ;":U
  "      color : white ;":U
  "      padding : .5rem;":U
  "      border-radius: .3rem;":U
  "      width : max-content ; ":U
  "      text-align : center;  ":U
  "      background : var(--tooltip-color );":U
  "~}"

  " .clValidationIndicator:hover::before,":U
  " .clValidationIndicator:hover::after~{":U
  "     --scale : 1 ;":U
  "~}":U

  " .clValidationIndicator::after~{":U
  "   --translateY : calc(-1 * var(--arrow-size)) ; ":U
  "   content: '';":U
  "   border: var(--arrow-size) solid transparent ;":U
  "   border-top-color :  var(--tooltip-color ) ; ":U
  "   transform-origin : top center ; ":U
  "~}"

  ".clGrey~{background-color : #e3e3e3 ~} ":U
  ".clRoundLeftBorder~{ border-top-left-radius: 8%;   border-bottom-left-radius: 8%;  ~} ":U
  ".clRoundRightBorder~{ border-top-right-radius: 8%; border-bottom-right-radius: 8%; ~} ":U
  ".clCriteriaToggle~{   "
  "  height: 0.8rem;      "
  "  filter: invert(1);   "
  "  padding-left: 0.3rem;"
  "  cursor: pointer;     "
  "~}":U

  "</style>":U .

  {&OUT}
  {ws/inc/wsjavascriptopentag.i} .

  IF Warpspeed:ValidationError THEN
  DO:
    GENERATE-ERROR-NOTIFICATION-BLK:
    FOR EACH tt_auth_copay_detail_item_error:

      ASSIGN cErrorMessage = tt_auth_copay_detail_item_error.error_message .

      {&OUT}
      "  new wsNotification(~{ notification : ~"" + cErrorMessage +         "~",   ~n":U
      "                        type         : ~"error~",                           ~n":U
      "                        timeout      : 5000,                                ~n":U
      "                        position     : ~"top~"                              ~n":U
      "                     ~});                                                   ~n":U .

    END. //GENERATE-ERROR-NOTIFICATION-BLK:
  END. //IF Warpspeed:ValidationError THEN

  {&OUT}
  {ws/inc/wsjavascriptclosetag.i} .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainer Procedure
PROCEDURE prepareContainer :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcContainer   AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER ipcPrepareWhat AS CHARACTER  NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN
  //TODO: Write service that retreives the auth copay detail item dataset
  CASE goWob:Mode:
    WHEN "Search":U THEN
    DO:

      IF  LOOKUP(goWob:SubmitValue,"Search,SearchSubmit":U) > 0 THEN
      DO:
        RUN _retrieveData IN TARGET-PROCEDURE.

      END. /* IF  LOOKUP(goWob:SubmitValue,"Search":U) > 0  AND NOT WarpSpeed:ValidationError */

    END. /*WHEN "Search":U THEN*/
  END CASE. /*CASE Wob:Mode:*/

  // Fetch insurer data so we can populate containers
    mipEnv:Health:maUtility:getInsurerDetails(
    INPUT 0.00,
    INPUT "":U,
    INPUT "":U,
    INPUT "":U,
    INPUT "":U,
    INPUT ?,
    INPUT ?,
    INPUT 0.00,
    OUTPUT TABLE tt_insurer).


   ASSIGN goCntATCClass:QueryBufferList = STRING(TEMP-TABLE tt_auth_copay_detail_item:DEFAULT-BUFFER-HANDLE) + ",":U
                                        + STRING(TEMP-TABLE tt_auth_copay_type:DEFAULT-BUFFER-HANDLE )       + ",":U
                                        + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE )               + ",":U
                                        + STRING(TEMP-TABLE tt_atc_class:DEFAULT-BUFFER-HANDLE )

         goCntNappi:QueryBufferList    = STRING(TEMP-TABLE tt_auth_copay_detail_item:DEFAULT-BUFFER-HANDLE) + ",":U
                                       + STRING(TEMP-TABLE tt_auth_copay_type:DEFAULT-BUFFER-HANDLE )       + ",":U
                                       + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE )               + ",":U
                                       + STRING(TEMP-TABLE tt_nappi_link:DEFAULT-BUFFER-HANDLE )

         goCntBasket:QueryBufferList   = STRING(TEMP-TABLE tt_auth_copay_detail_item:DEFAULT-BUFFER-HANDLE) + ",":U
                                       + STRING(TEMP-TABLE tt_auth_copay_type:DEFAULT-BUFFER-HANDLE )       + ",":U
                                       + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE )               + ",":U
                                       + STRING(TEMP-TABLE tt_crosswalk:DEFAULT-BUFFER-HANDLE )

         goCntTariff:QueryBufferList   = STRING(TEMP-TABLE tt_auth_copay_detail_item:DEFAULT-BUFFER-HANDLE) + ",":U
                                       + STRING(TEMP-TABLE tt_auth_copay_type:DEFAULT-BUFFER-HANDLE )       + ",":U
                                       + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE )               + ",":U
                                       + STRING(TEMP-TABLE tt_tariff_link:DEFAULT-BUFFER-HANDLE )


         goCntAudit:QueryBufferList    = STRING(TEMP-TABLE ttAuditRecord:DEFAULT-BUFFER-HANDLE)

         glSuccess                     = goCntATCClass:populateFromQuery()
         glSuccess                     = goCntNappi:populateFromQuery()
         glSuccess                     = goCntBasket:populateFromQuery()
         glSuccess                     = goCntTariff:populateFromQuery()
         glSuccess                     = goCntAudit:populateFromQuery().

  { mip/inc/mipcatcherror.i  }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rowRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowRenderProcedure Procedure
PROCEDURE rowRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.
  DEFINE VARIABLE dAuthCopayDetailItemObj AS DECIMAL  NO-UNDO.
&IF {&DBDFMA} >= 10195 &THEN
  RUN SUPER (INPUT ipoContainer).

  CASE ipoContainer:RowRenderArgument:

    WHEN ipoContainer:ContainerCode THEN
    DO:
      IF Warpspeed:ValidationError THEN
      DO:
        ASSIGN dAuthCopayDetailItemObj = DECIMAL(ipoContainer:getControl("fdAuthCopayDetailItemObj":U + ipoContainer:ContainerCode):ControlValue) NO-ERROR.

        {&ResetError}

        mipEnv:Health:maUiService:setContainerErrors(TEMP-TABLE tt_auth_copay_detail_item_error:HANDLE, ipoContainer, "hacii":U, dAuthCopayDetailItemObj, "":U).
      END. // IF Warpspeed:ValidationError
    END. /* WHEN ipoContainer:ContainerCode */

  END CASE. //  CASE ipoContainer:RowRenderArgument:

  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shutdown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shutdown Procedure
PROCEDURE shutdown :
/*------------------------------------------------------------------------------
  Purpose: Do proper clean-up in this procedure, as the wob shuts down.
------------------------------------------------------------------------------*/

  //All registered containers are deleted in the base wob

  DATASET dsATCClass:EMPTY-DATASET() .
  DATASET dsAuthCopayDetailItem:EMPTY-DATASET() .

  { mip/inc/mipcatcherror.i }

END PROCEDURE.  // Shut down

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_customizeContainerForEntity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _customizeContainerForEntity Procedure
PROCEDURE _customizeContainerForEntity :
/*------------------------------------------------------------------------------
  Purpose:  Customize the container for the owning entity it belongs to.
            This includes things like column labels, titles ,query strings and
            controlqueryfiedls
  Parameters: OwningEntityMnemonic
              ContainerObject

  Notes:

------------------------------------------------------------------------------*/

  DEFINE INPUT        PARAMETER ipcOwningEntityMnemonic  AS CHARACTER          NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopoContainer            AS cls.mipwscontainer NO-UNDO.
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE cAjaxValidationControlList AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cControlName               AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cContainerCode             AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER            NO-UNDO.
  DEFINE VARIABLE oControl                   AS cls.mipwscontrol   NO-UNDO.

  ASSIGN cContainerCode = iopoContainer:ContainerCode .

  CASE ipcOwningEntityMnemonic :
    WHEN "hlmac":U THEN
    DO:
      ASSIGN
        iopoContainer:ContainerTitle   = "ATC Class":U
        iopoContainer:QueryString      = SUBSTITUTE(iopoContainer:QueryString, " ,  FIRST tt_atc_class WHERE tt_atc_class.atc_class_key = tt_auth_copay_detail_item.owning_alt_value  OUTER-JOIN ":U )


        oControl                       = iopoContainer:getControl("buOwningBtn"               + cContainerCode)
        oControl:ColumnLabel           = "ATC Class Code":U

        oControl                       = iopoContainer:getControl("fcOwningAltValue"          + cContainerCode)
        oControl:ControlQueryField     = "tt_atc_class.atc_class_key":U

        oControl                       = iopoContainer:addControl("fcOwningRecordDescription":U + cContainerCode , "wsTextArea":U, "14":U, "tt_atc_class.atc_class_desc":U, "character":U, 15, "ATC Class Description":U)
        oControl:LabelClass            = "+clTop":U
        oControl:ControlToken          = "Disabled":U  .


    END. // WHEN 'hlmac'
    WHEN "hlmnl":U THEN
    DO:
      ASSIGN
        iopoContainer:ContainerTitle   = "Nappi":U
        iopoContainer:QueryString      = SUBSTITUTE(iopoContainer:QueryString, " ,  FIRST tt_nappi_link WHERE tt_nappi_link.nappi_link_obj = tt_auth_copay_detail_item.owning_obj  OUTER-JOIN ":U )

        oControl                       = iopoContainer:getControl("buOwningBtn"                 + cContainerCode)
        oControl:ColumnLabel           = "Nappi Code":U

        oControl                       = iopoContainer:addControl("fcOwningRecordDescription":U + cContainerCode , "wsTextArea":U, "14":U, "tt_nappi_link.nappi_description[1]":U, "character":U, 15, "Nappi Description":U)
        oControl:LabelClass            = "+clTop":U
        oControl:ControlToken          = "Disabled":U        .
    END. // WHEN "hlmnl"
    WHEN "htmtl":U THEN
    DO:
      ASSIGN
        iopoContainer:ContainerTitle   = "Tariff":U
        iopoContainer:QueryString      = SUBSTITUTE(iopoContainer:QueryString, " ,  FIRST tt_tariff_link WHERE tt_tariff_link.tariff_link_obj = tt_auth_copay_detail_item.owning_obj  OUTER-JOIN ":U )


        oControl                       = iopoContainer:getControl("buOwningBtn"     + cContainerCode)
        oControl:ColumnLabel           = "Tariff Code":U
        oControl:LookupWobFLA          = "htmtl":U
        oControl:LookupFields          = "htm_tariff_link.tariff_code":U
        oControl:LookupControls        = "fcOwningAltValue":U + cContainerCode
        oControl:FilterFields          = "htm_tariff_link.pr_type,htm_tariff_link.base_rate,htm_tariff_link.ars_rate":U
        oControl:FilterControls        = "fcPrType":U    + cContainerCode + ",fcBaseRate":U + cContainerCode
                                       + ",fcArsRate":U  + cContainerCode
        oControl:ReturnFields          = "htm_tariff_link.pr_type,htm_tariff_link.base_rate,htm_tariff_link.ars_rate,htm_tariff_link.tariff_link_default":U
        oControl:ReturnControls        = "fcPrType":U    + cContainerCode + ",fcBaseRate":U + cContainerCode
                                       + ",fcArsRate":U  + cContainerCode + ",chkTariffLinkDefault" + cContainerCode

        oControl                       = iopoContainer:addControl("chkTariffLinkDefault":U           + cContainerCode , "wsCheckBox":U ,       "7":U, "tt_tariff_link.tariff_link_default":U, "character":U, 10, "Default":U)
        oControl:ControlValue          = "TRUE":U


        oControl                       = iopoContainer:addControl("fcBaseRate":U           + cContainerCode , "wsInput":U ,       "7":U, "tt_tariff_link.base_rate":U, "character":U, 11, "Base Rate":U)
        oControl                       = iopoContainer:addControl("buBaseRateBtn":U        + cContainerCode , "wsLookupButton":U , "":U ,                              "":U , "":U ,  11, "Base Rate":U)
        oControl:CellLayoutMask        = "&1&2":U
        oControl:LookupWobFLA          = "baserate":U
        oControl:LookupFields          = "baserate.base-rate":U
        oControl:LookupControls        = "fcBaseRate":U + cContainerCode
        oControl:FilterFields          = "baserate.base-rate":U
        oControl:FilterControls        = "fcBaseRate":U + cContainerCode
        oControl:ReturnFields          = "baserate.base-rate":U
        oControl:ReturnControls        = "fcBaseRate":U + cContainerCode

        oControl                       = iopoContainer:addControl("fcArsRate":U            + cContainerCode , "wsInput":U ,   "5":U, "tt_tariff_link.ars_rate":U, "character":U,  12, "Ars Rate":U)
        oControl                       = iopoContainer:addControl("buArsRateBtn":U         + cContainerCode , "wsLookupButton":U , "":U   , "":U                , "":U         ,  12, "ARS Rate":U)
        oControl:CellLayoutMask        = "&1 &2":U
        oControl:LookupWobFLA          = "arsrate":U
        oControl:LookupFields          = "arsrate.ars-rate":U
        oControl:LookupControls        = "fcArsRate":U + cContainerCode
        oControl:FilterFields          = "arsrate.ars-rate":U
        oControl:FilterControls        = "fcArsRate":U + cContainerCode
        oControl:ReturnFields          = "arsrate.ars-rate":U
        oControl:ReturnControls        = "fcArsRate":U + cContainerCode

        oControl                       = iopoContainer:getControl("fcPrType":U  + cContainerCode)
        oControl:TableColumn           = 13

        oControl                       = iopoContainer:getControl("buPrType":U  + cContainerCode)
        oControl:TableColumn           = 13

        oControl                       = iopoContainer:addControl("flcData":U    + cContainerCode , "wsInput":U ,   "5":U, "":U, "character":U,  99, "":U)
        oControl:ControlClass          = "+clHid":U
        oControl                       = iopoContainer:addControl("flPartial":U  + cContainerCode , "wsInput":U ,   "5":U, "":U, "character":U,  99, "":U)
        oControl:ControlClass          = "+clHid":U
        oControl:CellLayOutMask        = "&1&2":U


        oControl                       = iopoContainer:addControl("flValidationIndicator":U  + cContainerCode , "wsInput":U ,   "5":U, "":U, "character":U,  14, "":U)
        oControl:CellClass             = "+clPd2 +clGrey ":U
        oControl:CellLayOutMask        = "<div id='ValidationStatus' class='clValidationIndicator' title='' ></div>":U

        oControl                       = iopoContainer:addControl("fcOwningRecordDescription":U + cContainerCode , "wsTextArea":U, "14":U, "tt_tariff_link.tariff_description":U, "character":U, 15, "Tariff Description":U)
        oControl:ControlToken          = "Disabled":U
        oControl:CellClass             = "+clTop +clPd2 +clGrey +clRoundRightBorder":U .


        ASSIGN cAjaxValidationControlList = "fcOwningAltValue,chkTariffLinkDefault,fcBaseRate,fcArsRate,fcPrType":U .

        AJX-VAL-CONTROL-LIST:
        DO iCount = 1 TO NUM-ENTRIES(cAjaxValidationControlList) :

          ASSIGN cControlName              = ENTRY(iCount , cAjaxValidationControlList)  + cContainerCode

                 oControl                    = iopoContainer:getControl(cControlName)
                 oControl:JavaScriptOnClick  = oControl:JavaScriptOnClick + "":U
                 oControl:ControlJavascript  = "autocomplete='off'"
                 oControl:CellClass          = "+clPd2 +clGrey":U
                                             + (IF iCount = 1
                                                THEN " +clRoundLeftBorder":U
                                                ELSE "":U)
                 oControl:ControlClass       = "+clTariffLinkKeyField":U
                                             + (IF  cControlName <> "fcOwningAltValue":U     + cContainerCode
                                                AND cControlName <> "chkTariffLinkDefault":U + cContainerCode
                                                THEN " +clSelectLookupControl":U
                                                ELSE "":U)

                 oControl:CellLayOutMask     = "<div class='clSelectLookup'></div>":U + oControl:CellLayOutMask
                 oControl:AjaxValidation     = "":U
                 oControl:FilterFields       = "":U
                 oControl:FilterControls     = "":U
                 oControl:ReturnFields       = "":U
                 oControl:ReturnControls     = "":U .



        END. // AJX-VAL-CONTROL-LIST:

    END . // WHEN "htmtl"
    WHEN "hlmcr":U THEN
    DO:
      ASSIGN
        iopoContainer:ContainerTitle   = "Basket":U
        iopoContainer:QueryString      = SUBSTITUTE(iopoContainer:QueryString,  " ,  FIRST tt_crosswalk WHERE tt_crosswalk.crosswalk_obj = tt_auth_copay_detail_item.owning_obj  OUTER-JOIN ":U )

        oControl                       = iopoContainer:getControl("buOwningBtn"               + cContainerCode)
        oControl:ColumnLabel           = "Basket Code":U

        oControl                       = iopoContainer:addControl("fcOwningRecordDescription":U + cContainerCode , "wsTextArea":U, "14":U, "tt_crosswalk.crosswalk_description":U, "character":U, 15, "Basket Description":U)
        oControl:LabelClass            = "+clTop":U
        oControl:ControlToken          = "Disabled":U .

    END . // WHEN "hlmcr"
  END CASE. // CASE ipcOwningEntityMnemonic

  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_defineContainerAuthCopayDetailItemGeneral) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _defineContainerAuthCopayDetailItemGeneral Procedure
PROCEDURE _defineContainerAuthCopayDetailItemGeneral :
/*------------------------------------------------------------------------------
  Purpose:  Define search/maint container for general copay detail item controls
  Parameters:  OwningEntityMnemonic
               Container object
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcOwningEntityMnemonic    AS CHARACTER                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer               AS cls.mipwscontainer        NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE cContainerCode        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE oControl              AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE lUpdatableContainer   AS LOGICAL                   NO-UNDO.

  ASSIGN
    cContainerCode                      = ipcOwningEntityMnemonic
    opoContainer                        = NEW cls.mipwscontainer(cContainerCode, "96%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:RowsToRender           = ?
    opoContainer:ContainerMode          = goWob:SubmitValue
    opoContainer:ViewOnly               = FALSE
    opoContainer:RowRenderProcedure     = "rowRenderProcedure":U
    opoContainer:RowRenderArgument      = cContainerCode
    opoContainer:DefaultContainerType   = "TABLE":U
    opoContainer:QueryString            = "FOR EACH tt_auth_copay_detail_item NO-LOCK":U
                                         + "   WHERE tt_auth_copay_detail_item.owning_entity_mnemonic = ~'":U + ipcOwningEntityMnemonic + "~' ":U
                                         + ",  FIRST tt_auth_copay_type WHERE tt_auth_copay_type.auth_copay_type_obj = tt_auth_copay_detail_item.auth_copay_type_obj OUTER-JOIN ":U
                                         + ",  FIRST tt_insurer         WHERE tt_insurer.insurer_obj = tt_auth_copay_detail_item.insurer_obj OUTER-JOIN ":U
                                         + "   &1 "
                                         + "   BY tt_auth_copay_detail_item.line_number ":U

    oControl                             = opoContainer:addControl("fdAuthCopayDetailItemObj":U  + cContainerCode, "wsInput":U ,   "20":U, "tt_auth_copay_detail_item.auth_copay_detail_item_obj":U, "character":U, 4, "":U)
    oControl:ControlToken                = "hidden"

    oControl                             = opoContainer:addControl("fdCopaymentTypeObj":U  + cContainerCode, "wsInput":U ,         "20":U, "tt_auth_copay_detail_item.auth_copay_type_obj":U, "character":U, 5, "":U)
    oControl:ControlToken                = "hidden"
    oControl                             = opoContainer:addControl("fcCopaymentType":U     + cContainerCode, "wsInput":U ,         "15":U, "tt_auth_copay_type.auth_copay_type":U,            "character":U, 5, "":U)
    oControl:ControlClass                = "+clMan":U
    oControl:AjaxValidation              = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation|hamct":U
    oControl:FilterFields                = "[OwningKey]":U
    oControl:FilterControls              = "fcCopaymentType":U  + cContainerCode
    oControl:ReturnFields                = "[OwningObj]":U
    oControl:ReturnControls              = "fdCopaymentTypeObj" + cContainerCode

    oControl                             = opoContainer:addControl("buCopaymentType":U    + cContainerCode,  "wsLookupButton":U ,  "20":U, "":U,"character":U, 5, "Co-Payment Type":U)
    oControl:LookupWobFLA                = "hamct":U
    oControl:LookupControls              = "fcCopaymentType":U + cContainerCode
    oControl:LookupFields                = "ham_auth_copay_type.auth_copay_type":U
    oControl:ReturnFields                = "ham_auth_copay_type.auth_copay_type":U
    oControl:ReturnControls              = "fcCopaymentType"   + cContainerCode


    oControl:CellLayOutMask              = "&1&2&3":U
    oControl:ControlToolTip              = "Co-payment type that this setup applies to.":U

    oControl                             = opoContainer:addControl("fdInsurerObj":U   + cContainerCode ,      "wsInput":U,   "15":U, "":U, "decimal":U,   6, "":U)
    oControl:ControlToken                = "Hidden":U
    oControl                             = opoContainer:addControl("fcInsurer":U    + cContainerCode,         "wsInput":U,   "10":U, "tt_insurer.insurer_code":U, "character":U ,6, "":U)
    oControl:AjaxValidation              = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation|ermin":U
    oControl:FilterFields                = "[OwningKey]":U
    oControl:FilterControls              = "fcInsurer":U  + cContainerCode
    oControl:ReturnFields                = "[OwningObj]":U
    oControl:ReturnControls              = "fdInsurerObj" + cContainerCode
    oControl:ControlToolTip              = "Please enter a valid client.":U
    oControl                             = opoContainer:addControl("buInsurerBtn":U         + cContainerCode, "wsLookupButton":U,    "":U, "":U, "":U,          6, "Client":U)
    oControl:CellLayoutMask              = "&1&2&3":U
    oControl:LookupWobFLA                = "ermin":U
    oControl:LookupFields                = "erm_insurer.insurer_name":U
    oControl:LookupControls               = "fcInsurer":U + cContainerCode
    oControl:ReturnControls              = "erm_insurer.insurer_obj,erm_insurer.insurer_name":U
    oControl:ReturnFields                = "fdInsurerObj" + cContainerCode + ",fcInsurer":U + cContainerCode

    oControl                             = opoContainer:addControl("fiOptionCode":U        + cContainerCode,        "wsInput":U    ,"5":U,  "tt_auth_copay_detail_item.option_code":U, "integer":U,  7, "":U)
    oControl:ControlTooltip              = "Please enter a valid Option."
    oControl:AjaxValidation              = "START:":U + goWob:ObjectCode + ":ajaxValidation:OwningEntityValidation|scheme":U
    oControl:FilterFields                = "[OwningKey]":U
    oControl:FilterControls              = "fiOptionCode":U  + cContainerCode

    oControl                             = opoContainer:addControl("buOptionBtn":U         + cContainerCode, "wsLookupButton":U    , "":U  , "":U, "":U,                                              7, "Option":U)
    oControl:CellLayoutMask              = "&1&2":U
    oControl:LookupWobFla                = "scheme":U
    oControl:LookupFields                = "scheme.scheme-code":U
    oControl:LookupControls              = "fiOptionCode":U + cContainerCode
    oControl:ReturnFields                = "scheme.scheme-code":U
    oControl:ReturnControls              = "fiOptionCode"   + cContainerCode

    oControl                             = opoContainer:addControl("fcOwningEntityArgument":U + cContainerCode,    "wsInput":U , "20":U, "tt_auth_copay_detail_item.owning_entity_mnemonic":U ,     "character":U,  8, "":U)
    oControl:ControlValue                = ipcOwningEntityMnemonic
    oControl:ControlToken                = "hidden":U

    oControl                             = opoContainer:addControl("fdOwningObj":U            + cContainerCode,    "wsInput":U,       "10":U, "tt_auth_copay_detail_item.owning_obj":U,      "character":U,  8,  "":U)
    oControl:ControlToken                = "Hidden":U

    oControl                             = opoContainer:addControl("fcOwningKey":U            + cContainerCode,    "wsInput":U,        "10":U, "tt_auth_copay_detail_item.owning_key":U,       "character":U, 8, "":U)
    oControl:ControlToken                = "Hidden":U
    oControl                             = opoContainer:addControl("fcOwningAltValue":U       + cContainerCode,    "wsInput":U,        "10":U, "tt_auth_copay_detail_item.owning_alt_value":U, "character":U, 8, "":U)
    oControl:ControlTooltip              = "Select the co-payment Owning item that applies."
    oControl:AjaxValidation              = "START:" + goWob:ObjectCode + ":AjaxValidation:OwningEntityValidation|":U + ipcOwningEntityMnemonic
    oControl:ControlClass                = "+clMan":U
    oControl:FilterFields                = "[OwningKey]":U
    oControl:FilterControls              = "fcOwningAltValue":U + cContainerCode
    oControl:ReturnFields                = "[OwningKey]" + ",":U
                                         + "[OwningObj]" + ",":U
                                         + "[OwningRecordDescription]"
    oControl:ReturnControls              = "fcOwningKey":U               + cContainerCode + ",":U
                                         + "fdOwningObj":U               + cContainerCode + ",":U
                                         + "fcOwningRecordDescription":U + cContainerCode

    oControl                             = opoContainer:addControl("buOwningBtn":U            +   cContainerCode,    "wsLookupButton":U,   "":U, "":U,   "":U                                  , 8, "Owning Alt Code":U)
    oControl:CellLayoutMask              = "&1&2&3&4&5":U
    oControl:LookupWobFLA                = "slent":U
    oControl:LookupFields                = "CODE_FIELD":U
    oControl:LookupControls              = "fcOwningAltValue":U       + cContainerCode
    oControl:FilterFields                = "QUERY_OEM":U
    oControl:FilterControls              = "fcOwningEntityArgument":U + cContainerCode
    oControl:ReturnFields                = "CODE_FIELD":U
    oControl:ReturnControls              = "fcOwningAltValue":U       + cContainerCode .

    //******************We'll leave a couple of columns open so that we can insert any additional fields required for isolating a owning entity**********************//


   ASSIGN
    oControl                             = opoContainer:addControl("fcPrType":U  + cContainerCode,  "wsInput":U ,           "5":U, "tt_auth_copay_detail_item.pr_type":U, "integer":U, 16, "":U)
    oControl:JavascriptOnChange          = "" //"fnDisciplinePad(this,~"|~");":U
    oControl:ControlTooltip              = "The discipline to which the co-payment should apply."
    oControl                             = opoContainer:addControl("buPrType":U  + cContainerCode,  "wsLookupButton":U ,   "1":U, "":U, "character":U, 16, "Discipline":U)
    oControl:LookupWobFLA                = "maprtype":U
    oControl:LookupControls              = "fcPrType":U + cContainerCode
    oControl:LookupFields                = "prtype.pr-type":U
    oControl:FilterControls              = "fcPrType":U + cContainerCode
    oControl:FilterFields                = "prtype.pr-type":U
    oControl:CellLayOutMask              = "&1&2":U

    oControl                             = opoContainer:addControl("cbCopaymentValueType":U  + cContainerCode, "wsCombo":U , "7":U, "tt_auth_copay_detail_item.copayment_value_type":U , "character":U,  17, "Value Type":U)
    oControl:AdditionalItems             = "=|Rand=no|Percentage=yes":U
    oControl:ControlClass                = "+clMan":U
    oControl:JavascriptOnChange          = "fnOnChangeValueType(this) ;"
    oControl:ControlTooltip              = "Specify a Co-payment type (Rand or Percentage) which apply to the Value."


    oControl                             = opoContainer:addControl("fdCopaymentValue":U  + cContainerCode,  "wsInput":U ,   "8":U, "tt_auth_copay_detail_item.copayment_value":U,        "decimal":U, 18, "Co-payment Value":U)
    oControl:ControlFormat               = "->>>>>>>9.99":U
    oControl:ControlClass                = "+clNumericOnly":U
    oControl:JavascriptOnChange          = ""
    oControl:ControlTooltip              = "Rand or percentage amount that must be applied as the co-payment."


    oControl                             = opoContainer:addControl("cbWarningMessageType":U  + cContainerCode, "wsCombo":U , "8":U, "tt_auth_copay_detail_item.warning_message_type":U , "character":U,  19, "Warning <br>Message Type":U)
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AcronymSelect:ma_acAuthCopayWarnMessageType:=":U
    oControl:ControlTooltip              = "Specify the Warning Message Type."
    oControl:JavascriptOnChange          = "fnOnChangeWarningMessageType(this, ~"fcWarningMessage~");":U

    oControl                             = opoContainer:addControl("fcWarningMessage":U + cContainerCode , "wsTextArea":U, "15,3":U, "tt_auth_copay_detail_item.warning_message":U,    "character":U,  20, "Warning Message":U)
    oControl:ControlTooltip              = "Message that will be displayed to the user when a provider outside the co-payment group is used."
    oControl:RenderProcedure             = "customRenderProcedure":U
    oControl:RenderArgument              = "WarningMessage":U

    oControl                             = opoContainer:addControl("flApplyToPMB":U         + cContainerCode  ,  "wsCheckBox":U  , "1":U, "tt_auth_copay_detail_item.apply_to_pmb":U,       "logical":U, 21, "Apply to PMB":U)
    oControl:ControlTooltip              = "Indicates whether the co-payment must apply if the main diagnosis is a PMB.":U

    oControl                             = opoContainer:addControl("flApplyToEmergency":U   + cContainerCode  ,  "wsCheckBox":U  , "1":U, "tt_auth_copay_detail_item.apply_to_emergency":U, "logical":U, 22, "Apply to <br> Emergency":U)
    oControl:ControlTooltip              = "Indicates whether the co-payment must apply if the authorisation is an emergency.":U

    oControl                             = opoContainer:addControl("fdEffectiveDate":U      + cContainerCode,   "wsInput":U,      "10":U, "tt_auth_copay_detail_item.effective_date":U,    "date":U,    23, "Effective Date":U)
    oControl:ControlToolTip              = "Please enter an Effective Date":U
    oControl:ControlClass                = "+clMan":U

    oControl                             = opoContainer:addControl("fdEndDate":U            + cContainerCode,   "wsInput":U,      "10":U, "tt_auth_copay_detail_item.end_date":U,          "date":U,    24, "End Date":U)
    oControl:ControlToolTip              = "Please enter an End Date":U

    lUpdatableContainer                  = NOT glEnquiryWob.


  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_fetchATCClassDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _fetchATCClassDataset Procedure
PROCEDURE _fetchATCClassDataset :
/*------------------------------------------------------------------------------
  Purpose: Return the ATCClass dataset according to the Key list provided

  Parameters:   ipcATCClassKeyList - comma seperated list of ATC classes to filter on
                DATASET dsATCClass
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcATCClassKeyList AS CHARACTER NO-UNDO .
  DEFINE INPUT-OUTPUT PARAMETER DATASET            FOR dsATCClass  .
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE iCount                 AS INTEGER                 NO-UNDO.
  DEFINE VARIABLE cATCClassKey           AS CHARACTER               NO-UNDO.
  DEFINE VARIABLE lFilterCriteriaApplied AS LOGICAL                 NO-UNDO.
  DEFINE VARIABLE oATCClassSearch        AS ma.cls.maatcclasssearch NO-UNDO.

  DATASET dsATCClass:EMPTY-DATASET() .

  ASSIGN ipcATCClassKeyList =  IF ipcATCClassKeyList = ?
                               THEN "":U
                               ELSE ipcATCClassKeyList

         oATCClassSearch    =  NEW ma.cls.maatcclasssearch(DATASET dsATCClass:HANDLE)
         glSuccess          = oATCClassSearch:SetCriteria("BufferList":U, "tt_atc_class":U).

  //Set filter criteria on all the ATC Class keys
  DO iCount = 1 TO NUM-ENTRIES(ipcATCClassKeyList):

    ASSIGN cATCClassKey = ENTRY(iCount , ipcATCClassKeyList).

    IF cATCClassKey <> "":U
    THEN
      ASSIGN glSuccess              = oATCClassSearch:SetFilterCriteria("tt_atc_class.atc_class_key":U, "=":U, cATCClassKey)
             lFilterCriteriaApplied = TRUE.

  END.  // DO iCount = 1 TO NUM-ENTRIES(ipcATCClassKeyList)

  //Fetch
  IF lFilterCriteriaApplied
  THEN
    oATCClassSearch:fetchATCClassData().

  // Cleanup
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oATCClassSearch) THEN DELETE OBJECT oATCClassSearch . " }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_fetchAuthCopayTypeDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _fetchAuthCopayTypeDataset Procedure
PROCEDURE _fetchAuthCopayTypeDataset :
/*------------------------------------------------------------------------------
  Purpose: Fetch auth copay type dataset .
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcAuthCopayType  AS  CHARACTER       NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER DATASET           FOR dsAuthCopayType        .
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE lFilterCriteriaApplied  AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE oAuthCopayTypeSearch    AS cls.maauthcopaytypesearch NO-UNDO.

  DATASET dsAuthCopayType:EMPTY-DATASET().

  ASSIGN ipcAuthCopayType     = IF   ipcAuthCopayType = ?
                                THEN "":U
                                ELSE ipcAuthCopayType

         oAuthCopayTypeSearch = NEW cls.maauthcopaytypesearch(DATASET dsAuthCopayType BY-REFERENCE)
         glSuccess            = oAuthCopayTypeSearch:setFilterCriteria("tt_auth_copay_type.auth_copay_type":U , "begins":U , ipcAuthCopayType ) .

 oAuthCopayTypeSearch:fetchData() .


  { mip/inc/mipcatcherror.i }
&ENDIF
END PROCEDURE.  // Shut down

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_fetchBasketDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _fetchBasketDataset Procedure
PROCEDURE _fetchBasketDataset :
/*------------------------------------------------------------------------------
  Purpose: Return the Crosswalk/basket dataset according to the obj list provided

  Parameters:   ipcBasketObjList - comma seperated list of objs to filter on
                DATASET dsCrosswalk
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcBasketObjList   AS CHARACTER NO-UNDO .
  DEFINE INPUT-OUTPUT PARAMETER DATASET            FOR dsCrosswalk  .
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE iCount                 AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE dBasketObj             AS DECIMAL                  NO-UNDO.
  DEFINE VARIABLE lFilterCriteriaApplied AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE oBasketSearch          AS ma.cls.macrosswalksearch NO-UNDO.

  DATASET dsCrosswalk:EMPTY-DATASET() .

  ASSIGN ipcBasketObjList =  IF ipcBasketObjList = ?
                             THEN "":U
                             ELSE ipcBasketObjList

         oBasketSearch    =  NEW ma.cls.macrosswalksearch(DATASET dsCrosswalk:HANDLE)
         glSuccess          = oBasketSearch:SetCriteria("BufferList":U, "tt_crosswalk":U).

  //Set filter criteria
  DO iCount = 1 TO NUM-ENTRIES(ipcBasketObjList):

    ASSIGN dBasketObj = DECIMAL(ENTRY(iCount , ipcBasketObjList)) NO-ERROR.

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U
    THEN
      {&ResetError}

    IF dBasketObj <> ?
    THEN
      ASSIGN glSuccess              = oBasketSearch:SetFilterCriteria("tt_crosswalk.crosswalk_obj":U, "=":U, dBasketObj)
             lFilterCriteriaApplied = TRUE.

  END.  // DO iCount = 1 TO NUM-ENTRIES(ipcATCClassKeyList)

  //Fetch
  IF lFilterCriteriaApplied
  THEN
    oBasketSearch:fetchCrosswalkData().

  // Cleanup
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oBasketSearch) THEN DELETE OBJECT oBasketSearch . " }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_fetchNappiDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _fetchNappiDataset Procedure
PROCEDURE _fetchNappiDataset :
/*------------------------------------------------------------------------------
  Purpose: Return the nappi dataset according to the obj list provided

  Parameters:   ipcNappiLinkObjList - comma seperated list of objs to filter on
                DATASET dsNappi
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcNappiLinkObjList AS CHARACTER NO-UNDO .
  DEFINE INPUT-OUTPUT PARAMETER DATASET             FOR dsNappi  .
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE iCount                 AS INTEGER             NO-UNDO.
  DEFINE VARIABLE dNappiLinkObj          AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE lFilterCriteriaApplied AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE oNappiSearch           AS cls.manappisearch   NO-UNDO.

  DATASET dsNappi:EMPTY-DATASET() .

  ASSIGN ipcNappiLinkObjList =  IF ipcNappiLinkObjList = ?
                                THEN "":U
                                ELSE ipcNappiLinkObjList

         oNappiSearch         = NEW cls.manappisearch(DATASET dsNappi BY-REFERENCE)
         glSuccess            = oNappiSearch:setCriteria("activeLink":U, "tt_nappi_link":U)
         glSuccess            = oNappiSearch:SetCriteria("BufferList":U, "tt_nappi_link":U).  /* Restrict the buffers to be filled by the data service */

  //Set filter criteria on all the nappi link objs
  DO iCount = 1 TO NUM-ENTRIES(ipcNappiLinkObjList):

    ASSIGN dNappiLinkObj = DECIMAL(ENTRY(iCount , ipcNappiLinkObjList)) NO-ERROR.

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U
    THEN
      {&ResetError}

    IF dNappiLinkObj <> ?
    THEN
      ASSIGN glSuccess              = oNappiSearch:SetFilterCriteria("tt_nappi_link.nappi_link_obj":U,   "=":U, dNappiLinkObj)
             lFilterCriteriaApplied = TRUE .

  END.  // DO iCount = 1 TO NUM-ENTRIES(ipcNappiLinkObjList)

  //Fetch
  IF lFilterCriteriaApplied
  THEN
    oNappiSearch:fetchData().

  // Cleanup
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oNappiSearch) THEN DELETE OBJECT oNappiSearch . " }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_fetchTariffLinkDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _fetchTariffLinkDataset Procedure
PROCEDURE _fetchTariffLinkDataset :
/*------------------------------------------------------------------------------
  Purpose: Return the tariff dataset according to the obj list provided

  Parameters:   ipcTariffLinkObjList - comma seperated list of objs to filter on
                DATASET dsTariff
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcTariffLinkObjList AS CHARACTER NO-UNDO .
  DEFINE INPUT-OUTPUT PARAMETER DATASET              FOR dsTariff  .
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE VARIABLE iCount                 AS INTEGER                 NO-UNDO.
  DEFINE VARIABLE dTariffLinkObj         AS DECIMAL                 NO-UNDO.
  DEFINE VARIABLE lFilterCriteriaApplied AS LOGICAL                 NO-UNDO.
  DEFINE VARIABLE oTariffSearch          AS ma.cls.matariffsearch   NO-UNDO.

  DATASET dsTariff:EMPTY-DATASET() .

  ASSIGN ipcTariffLinkObjList =  IF ipcTariffLinkObjList = ?
                                 THEN "":U
                                 ELSE ipcTariffLinkObjList

         oTariffSearch        = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE)
         glSuccess            = oTariffSearch:SetCriteria("BufferList":U, "tt_tariff_link,tt_tariff":U). // Restrict the buffers to be filled by the data service

  //Set filter criteria
  DO iCount = 1 TO NUM-ENTRIES(ipcTariffLinkObjList):

    ASSIGN dTariffLinkObj = DECIMAL(ENTRY(iCount , ipcTariffLinkObjList)) NO-ERROR.

    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:76":U
    THEN
      {&ResetError}

    IF dTariffLinkObj <> ?
    THEN
      ASSIGN glSuccess              = oTariffSearch:SetFilterCriteria("tt_tariff_link.tariff_link_obj":U, "=":U, dTariffLinkObj)
             lFilterCriteriaApplied = TRUE.

  END.  // DO iCount = 1 TO NUM-ENTRIES(ipcTariffLinkObjList)

  //Fetch
  IF lFilterCriteriaApplied
  THEN
    oTariffSearch:fetchTariffData().

  // Cleanup
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oTariffSearch) THEN DELETE OBJECT oTariffSearch . " }
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_retrieveData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _retrieveData Procedure
PROCEDURE _retrieveData :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:
  Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE dEffectiveDate          AS DATE       NO-UNDO.
  DEFINE VARIABLE dEndDate                AS DATE       NO-UNDO.
  DEFINE VARIABLE cAction                 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cATCClassList           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAuthCopayType          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBasketObjList          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cContainerCode          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAuthCopayTypeObjList   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNappiLinkObjList       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningKey              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOwningAltValue         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRecordAction           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTables                 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTariffLinkObjList      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWarningMessage         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWarningMessageType     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWhereClauseList        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLineNumber             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNumRecords             AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iCount                  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOptionCode             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPrType                 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dAuthCopayDetailItemObj AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCopaymentValue         AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dInsurerObj             AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dOwningObj              AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE lApplyToPmb             AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lApplyToEmergency       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lApplyCriteriaEmergency AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lApplyCriteriaPMB       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lCopaymentValueType     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lTooMuchData            AS LOGICAL    NO-UNDO.
&IF {&DBDFMA} >= 10195 &THEN
  DEFINE BUFFER btt_auth_copay_detail_item FOR tt_auth_copay_detail_item.
  DEFINE BUFFER btt_nappi_link             FOR tt_nappi_link .

  DEFINE VARIABLE oAuthCopayTypeSearch    AS cls.maauthcopaytypesearch    NO-UNDO.
  DEFINE VARIABLE oAuthCopayDetItemSearch AS cls.maauthcopaydetitemsearch NO-UNDO.

  ASSIGN cContainerCode           =         goCntSearchFilter:ContainerCode
         iNumRecords              = INTEGER(goCntSearchFilter:getControl("fiRecords":U           + cContainerCode ):ControlValue)
         cAuthCopayType           =         goCntSearchFilter:getControl("fcCopaymentType":U     + cContainerCode ):ControlValue
         cOwningEntityMnemonic    =         goCntSearchFilter:getControl("cbOwningEntity":U      + cContainerCode ):ControlValue
         cOwningKey               =         goCntSearchFilter:getControl("fcOwningKey":U         + cContainerCode ):ControlValue
         cOwningAltValue          =         goCntSearchFilter:getControl("fcOwningAltValue":U    + cContainerCode ):ControlValue
         iPrType                  = INTEGER(goCntSearchFilter:getControl("fcPrType":U            + cContainerCode ):ControlValue)
         dInsurerObj              = DECIMAL(goCntSearchFilter:getControl("fdInsurerObj":U        + cContainerCode ):ControlValue)
         iOptionCode              = INTEGER(goCntSearchFilter:getControl("fiOptionCode":U        + cContainerCode ):ControlValue)
         dEffectiveDate           =    DATE(goCntSearchFilter:getControl("fdEffectiveDate":U     + cContainerCode ):ControlValue)
         dEndDate                 =    DATE(goCntSearchFilter:getControl("fdEndDate":U           + cContainerCode ):ControlValue)


         lApplyToPmb              =  IF      CAN-DO("y,yes,true", goCntSearchFilter:getControl("flApplyToPMB":U   + cContainerCode ):ControlValue ) THEN TRUE
                                     ELSE IF CAN-DO("n,no,true",  goCntSearchFilter:getControl("flApplyToPMB":U   + cContainerCode ):ControlValue ) THEN FALSE
                                     ELSE ?
         lApplyToEmergency        =  IF      CAN-DO("y,yes,true", goCntSearchFilter:getControl("flApplyToEmergency":U   + cContainerCode ):ControlValue ) THEN TRUE
                                     ELSE IF CAN-DO("n,no,true",  goCntSearchFilter:getControl("flApplyToEmergency":U   + cContainerCode ):ControlValue ) THEN FALSE
                                     ELSE ?

        NO-ERROR .

  //We'll try our best to filter on the criteria that was provided even if junk has been passed in - Reset the error
  {&ResetError}

  //We'll set a default limit of 50 records if no number is specified
  IF iNumRecords = ?
  THEN
    ASSIGN iNumRecords = 50 .

  RUN _fetchAuthCopayTypeDataset IN TARGET-PROCEDURE(INPUT cAuthCopayType ,
                                                     INPUT-OUTPUT DATASET dsAuthCopayType BY-REFERENCE) .

  IF NOT Warpspeed:ValidationError THEN
  DO:
    DATASET dsAuthCopayDetailItem:EMPTY-DATASET() .



    //Get ready to retrieve copay detail item data with criteria specified
    ASSIGN oAuthCopayDetItemSearch = NEW cls.maauthcopaydetitemsearch(DATASET dsAuthCopayDetailItem BY-REFERENCE)

           glSuccess               = oAuthCopayDetItemSearch:setCriteria("BatchSize" , STRING(iNumRecords) )
           glSuccess               = IF cOwningEntityMnemonic <> "":U THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.owning_entity_mnemonic":U,   "=":U, cOwningEntityMnemonic)   ELSE  TRUE
           glSuccess               = IF cOwningAltValue       <> "":U THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.owning_alt_value":U,         "=":U, cOwningAltValue)         ELSE  TRUE
           glSuccess               = IF iPrType               <> 0    THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.pr_type":U,                  "=":U, iPrType)                 ELSE  TRUE
           glSuccess               = IF lApplyToPmb           <> ?    THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.apply_to_pmb":U,             "=":U, lApplyToPmb)             ELSE  TRUE
           glSuccess               = IF lApplyToEmergency     <> ?    THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.apply_to_emergency":U,       "=":U, lApplyToEmergency)       ELSE  TRUE
           glSuccess               = IF dInsurerObj           <> 0    THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.insurer_obj":U,              "=":U, dInsurerObj)             ELSE  TRUE
           glSuccess               = IF iOptionCode           <> 0    THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.option_code":U,              "=":U, iOptionCode)             ELSE  TRUE
           glSuccess               = IF dEffectiveDate        <> ?    THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.effective_date":U,           "=":U, dEffectiveDate)          ELSE  TRUE
           glSuccess               = IF dEndDate              <> ?    THEN oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.end_date":U,                 "=":U, dEndDate)                ELSE  TRUE  .

      FOR EACH tt_auth_copay_type:
         ASSIGN glSuccess  = oAuthCopayDetItemSearch:SetFilterCriteria("tt_auth_copay_detail_item.auth_copay_type_obj":U,      "=":U, tt_auth_copay_type.auth_copay_type_obj)  .
      END.

     oAuthCopayDetItemSearch:fetchData() .

  END. // IF NOT Warpspeed:ValidationError THEN

  // Now that we have the copay detail item result set, we'll need the atc class ,nappi, basket, tariff and auth copay type datasets
  // This is so that we can render descriptions/codes/rates for owning entities/copay types
  FOR EACH btt_auth_copay_detail_item :

    CASE btt_auth_copay_detail_item.owning_entity_mnemonic:
      WHEN "hlmac":U THEN ASSIGN cATCClassList      = cATCClassList      + (IF cATCClassList      <> "":U THEN ",":U ELSE "":U) +         btt_auth_copay_detail_item.owning_key  NO-ERROR.
      WHEN "hlmnl":U THEN ASSIGN cNappiLinkObjList  = cNappiLinkObjList  + (IF cNappiLInkObjList  <> "":U THEN ",":U ELSE "":U ) + STRING(btt_auth_copay_detail_item.owning_obj) NO-ERROR.
      WHEN "hlmcr":U THEN ASSIGN cBasketObjList     = cBasketObjList     + (IF cBasketObjList     <> "":U THEN ",":U ELSE "":U ) + STRING(btt_auth_copay_detail_item.owning_obj) NO-ERROR.
      WHEN "htmtl":U THEN ASSIGN cTariffLinkObjList = cTariffLinkObjList + (IF cTariffLinkObjList <> "":U THEN ",":U ELSE "":U ) + STRING(btt_auth_copay_detail_item.owning_obj) NO-ERROR.

    END CASE.  // CASE btt_auth_copay_detail_item

    IF LOOKUP(STRING(btt_auth_copay_detail_item.auth_copay_type_obj) , cAuthCopayTypeObjList)  = 0
    THEN
      ASSIGN cAuthCopayTypeObjList = cAuthCopayTypeObjList + (IF cAuthCopayTypeObjList <> "":U THEN ",":U ELSE "":U ) + STRING(btt_auth_copay_detail_item.auth_copay_type_obj) NO-ERROR.

    mipEnv:Health:maUtility:getAuditRecordTT(INPUT "hac_auth_copay_detail_item",                                   // Pipe delimited table list
                                             INPUT STRING(btt_auth_copay_detail_item.auth_copay_detail_item_obj),  // Owning obj list or ^ delimited owning key list ( fields | delimited )
                                             INPUT "",                                                             // Pipe delimited where clause list
                                             INPUT "",                                                             // Pipe delimited table/field join list
                                             INPUT "",                                                             // User key
                                             INPUT ?,                                                              // From date
                                             INPUT ?,                                                              // To date
                                             INPUT "",                                                             // Action
                                             INPUT "",                                                             // Wordindex
                                             INPUT FALSE,                                                          // Include the audit trails for child records
                                             INPUT TRUE,                                                           // Roll up deleted
                                             INPUT ?,                                                              // Record limit
                                             OUTPUT lTooMuchData,
                                             OUTPUT TABLE ttAuditRecord APPEND).

  END. //FOR EACH btt_auth_copay_detail_item

  //  If one of the lists above failed due to the 32k character limit , we'll message the error to the log and reset it
  IF cls.miperror:getMessageGroupNumber() = "PROGRESS:11678":U
  THEN
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }


  // Now fetch the owning entity data
  RUN _fetchATCClassDataset      IN TARGET-PROCEDURE(INPUT  cATCClassList,
                                                     INPUT-OUTPUT DATASET dsATCClass BY-REFERENCE ) .

  RUN _fetchNappiDataset         IN TARGET-PROCEDURE(INPUT  cNappiLinkObjList,
                                                     INPUT-OUTPUT DATASET dsNappi BY-REFERENCE) .

  RUN _fetchBasketDataset        IN TARGET-PROCEDURE(INPUT  cBasketObjList,
                                                     INPUT-OUTPUT DATASET dsCrosswalk BY-REFERENCE) .

  RUN _fetchTariffLinkDataset    IN TARGET-PROCEDURE(INPUT  cTariffLinkObjList,
                                                     INPUT-OUTPUT DATASET dsTariff BY-REFERENCE ) .


  CREATE-DUMMY-RECORD-BLK:
  DO iCount = 1 TO NUM-ENTRIES(gcContainerOwningEntityList):

    CREATE tt_auth_copay_detail_item .

    ASSIGN tt_auth_copay_detail_item.record_action              = "":U
           tt_auth_copay_detail_item.line_number                = 99999
           tt_auth_copay_detail_item.insurer_obj                = dInsurerObj
           tt_auth_copay_detail_item.option_code                = ?
           tt_auth_copay_detail_item.auth_copay_detail_item_obj = iCount * -1
           tt_auth_copay_detail_item.auth_copay_type_obj        = -1
           tt_auth_copay_detail_item.effective_date             = ?
           tt_auth_copay_detail_item.end_date                   = ?
           tt_auth_copay_detail_item.owning_entity_mnemonic     = ENTRY(iCount, gcContainerOwningEntityList)
           tt_auth_copay_detail_item.owning_obj                 = -1
           tt_auth_copay_detail_item.owning_key                 = "":U
           tt_auth_copay_detail_item.owning_alt_value           = "":U
           tt_auth_copay_detail_item.pr_type                    = iPrType
           tt_auth_copay_detail_item.copayment_value_type       = ?
           tt_auth_copay_detail_item.copayment_value            = 0
           tt_auth_copay_detail_item.apply_to_pmb               = FALSE
           tt_auth_copay_detail_item.apply_to_emergency         = FALSE
           tt_auth_copay_detail_item.warning_message            = "":U
           tt_auth_copay_detail_item.warning_message_type       = "":U  .

  END. //  CREATE-DUMMY-RECORD-BLK:
&ENDIF
  { mip/inc/mipcatcherror.i }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_returnDBEntity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _returnDBEntity Procedure
PROCEDURE _returnDBEntity :
/*------------------------------------------------------------------------------
  Purpose: Return the record key and obj with the record obj provided

  Parameters:   ipcOwningEntityMnemonic
                ipdOwningRowId
                opdOwningObj
                opcOwningKey

        returns oDBEntity object

  Notes: Search in order
         1. RowID
         2. Obj/Key
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcOwningEntityMnemonic   AS CHARACTER       NO-UNDO.
  DEFINE INPUT  PARAMETER iprOwningRowId            AS ROWID           NO-UNDO.
  DEFINE INPUT  PARAMETER ipdOwningObj              AS DECIMAL         NO-UNDO.
  DEFINE INPUT  PARAMETER ipcOwningKey              AS CHARACTER       NO-UNDO.
  DEFINE OUTPUT PARAMETER opoDBEntity               AS cls.mipdbentity NO-UNDO.
&IF {&DBDFMA} >= 10195 &THEN
  ASSIGN ipcOwningEntityMnemonic  = IF ipcOwningEntityMnemonic  <> ? THEN ipcOwningEntityMnemonic   ELSE "":U
         iprOwningRowID           = IF iprOwningRowID           <> ? THEN iprOwningRowID            ELSE ?
         ipdOwningObj             = IF ipdOwningObj             <> ? THEN ipdOwningObj              ELSE 0
         ipcOwningKey             = IF ipcOwningKey             <> ? THEN ipcOwningKey              ELSE "":U

         opoDBEntity              = NEW cls.mipdbentity()
         glSuccess                = opoDBEntity:focusTable(ipcOwningEntityMnemonic) NO-ERROR.

  IF NOT opoDBEntity:Infocus
  THEN
    RETURN .

  // ROWID Search
  IF iprOwningRowID <> ?
  THEN
    opoDBEntity:findRecord(iprOwningRowID).

  // OBJ Search
  IF  NOT opoDBEntity:RecordAvailable
  AND ipdOwningObj <> 0
  THEN
    opoDBEntity:findRecord(ipdOwningObj).

  // KEY Search
  IF  NOT opoDBEntity:RecordAvailable
  AND ipcOwningKey <> "":U
  THEN
    opoDBEntity:findRecord(ipcOwningKey).
&ENDIF
  { mip/inc/mipcatcherror.i  }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-appendAjaxReturnValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendAjaxReturnValues Procedure
FUNCTION appendAjaxReturnValues RETURNS LOGICAL
  (INPUT-OUTPUT ipocReturnValueList  AS CHARACTER,
   INPUT        ipcValue             AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose: Append return values for ajax validation response to a pipe delimited
           list
------------------------------------------------------------------------------*/

  ASSIGN ipcValue            = "[blank]":U WHEN ipcValue = "":U
         ipocReturnValueList = ipocReturnValueList
                             + (IF  ipocReturnValueList  <> "":U
                                THEN "|":U ELSE "":U )
                             + ipcValue .

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-returnDBEntity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION returnDBEntity Procedure
FUNCTION returnDBEntity RETURNS cls.mipdbentity
  (INPUT ipcOwningEntityMnemonic  AS CHARACTER,
   INPUT iprRowID                 AS ROWID,
   INPUT ipdObj                   AS DECIMAL,
   INPUT ipcKey                   AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose: Return a cls.mipDBEntity object using the owning entity and rowid provided
------------------------------------------------------------------------------*/
  DEFINE VARIABLE opoDBEntity AS cls.mipdbentity NO-UNDO.

  RUN _returnDBEntity IN TARGET-PROCEDURE(INPUT ipcOwningEntityMnemonic,
                                          INPUT iprRowID ,
                                          INPUT ipdObj,
                                          INPUT ipcKey,
                                          OUTPUT opoDBEntity) NO-ERROR.


  RETURN opoDBEntity.  // Return the opoDBEntity

  {mip/inc/mipcatcherror.i}
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

