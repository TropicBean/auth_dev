&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
" This code is based on the cgi-wrapper template as designed by Progress.

  MIP Holdings (Pty) Ltd.

  Use this template to create a new Custom CGI Wrapper Procedure and write WebSpeed code that dynamically generates HTML. No associated static HTML file is needed."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Filename    : ma/app/maauthgroupwobsuper.p
  Purpose     : Maintain Authorisation Groups
  Description : Maintain Authorisation Groups
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

/* This helps to ensure proper clean-up */
CREATE WIDGET-POOL.

/* WarpSpeed's Shared Definitions */
{ mip/inc/mipdefshared.i }

{ sysadmma.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation}

{ ma/inc/maauthgroupds.i }
                         
{ ma/inc/maaudittt.i    }   

{ ma/inc/mainsurertt.i  } 

{ ma/inc/matariffds.i   }                     
                         
{ ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_error"}

DEFINE TEMP-TABLE tt_deleted
  FIELD owning_obj AS DECIMAL
  FIELD owning_key AS CHARACTER.

/* Variables commonly used by WarpSpeed */
DEFINE VARIABLE goWob              AS cls.mipwswob            NO-UNDO.

/* Variables for this specific WOB */
DEFINE VARIABLE gcFormat           AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcSearchMethod     AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcSelected         AS CHARACTER               NO-UNDO.
DEFINE VARIABLE gcAllTableList     AS CHARACTER               NO-UNDO.
DEFINE VARIABLE glEnquiryWob       AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE glSuccess          AS LOGICAL                 NO-UNDO.

/* Containers */
DEFINE VARIABLE goCntSearchFilter  AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntSearchResults AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntMaintDetail   AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntMaint         AS cls.mipwscontainer      NO-UNDO.
DEFINE VARIABLE goCntAudit         AS cls.mipwscontainer      NO-UNDO.

/* That's all Folks! */

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 18.52
         WIDTH              = 57.8.
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
  This is a WarpSpeed Warpspeed - include ws/inc/wsstructure.i, and nothing else.
------------------------------------------------------------------------------*/
  ASSIGN goWob = Warpspeed:CurrentObject.

  { mip/inc/mipcatcherror.i }

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxSaveMaintDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveMaintDetail Procedure 
PROCEDURE ajaxSaveMaintDetail :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
  
  DEFINE VARIABLE oAuthGroupDetail          AS cls.maauthgroupdetail      NO-UNDO.
  DEFINE VARIABLE oRequestHelper            AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper           AS cls.maajaxresponsehelper   NO-UNDO.
  DEFINE VARIABLE oTarifflinkSearch         AS ma.cls.matariffsearch      NO-UNDO.
  
  DEFINE VARIABLE hErrorHandle              AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cContainerCode            AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAction                   AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction             AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic     AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cOwningKey                AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAltValue                 AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cClaimType                AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cPrType                   AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cSubPrType                AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cBaseRate                 AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cARSRAte                  AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE iClaimCode                AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iQuantity                 AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iLineNumber               AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE dEffectiveDate            AS DATE                       NO-UNDO.
  DEFINE VARIABLE dEndDate                  AS DATE                       NO-UNDO.
  DEFINE VARIABLE dAmount                   AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj             AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dAuthGroupDetailObj       AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dOwningObj                AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE lSuccess                  AS LOGICAL                    NO-UNDO.

  ASSIGN
    cContainerCode              = ipcValidationArgument
    oRequestHelper              = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
    oResponseHelper             = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)   
    oAuthGroupDetail            = NEW cls.maauthgroupdetail()
    cRecordAction               =         oRequestHelper:getFieldValue("fcAction":U                  + cContainerCode)
    cOwningEntityMnemonic       =         oRequestHelper:getFieldValue("cbOwningEntityMnemonicTbl":U + cContainerCode)
    cOwningKey                  =         oRequestHelper:getFieldValue("fcOwningEntityKeyTbl":U      + cContainerCode)
    cAltValue                   =         oRequestHelper:getFieldValue("fcOwningEntityCodeTbl":U     + cContainerCode)
    cPrType                     =         oRequestHelper:getFieldValue("fiDisciplineTbl":U           + cContainerCode)
    cSubPrType                  =         oRequestHelper:getFieldValue("fiSubDisciplineTbl":U        + cContainerCode)
    cBaseRate                   =         oRequestHelper:getFieldValue("fcBaseRateTbl":U             + cContainerCode)
    cARSRAte                    =         oRequestHelper:getFieldValue("fcARSRateTbl":U              + cContainerCode)
    cClaimType                  =         oRequestHelper:getFieldValue("cbClaimTypeTbl":U            + cContainerCode)
    iLineNumber                 = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U              + cContainerCode))
    iClaimCode                  = INTEGER(oRequestHelper:getFieldValue("fiClaimCodeTbl":U            + cContainerCode))
    iQuantity                   = INTEGER(oRequestHelper:getFieldValue("fiQuantityTbl":U             + cContainerCode))
    dAmount                     = DECIMAL(oRequestHelper:getFieldValue("fiAmountTbl":U               + cContainerCode))
    dAuthGroupObj               = DECIMAL(oRequestHelper:getFieldValue("fdAuthGroupObjArgumentTbl":U + cContainerCode))
    dAuthGroupDetailObj         = DECIMAL(oRequestHelper:getFieldValue("fdGroupDetailObjTbl":U       + cContainerCode))
    dOwningObj                  = DECIMAL(oRequestHelper:getFieldValue("fdOwningEntityObjTbl":U      + cContainerCode))
                                
    dEffectiveDate              = (IF oRequestHelper:getFieldValue("fdEffectiveDateTbl":U + cContainerCode) <> "yyyy/mm/dd":U
                                   THEN DATE(oRequestHelper:getFieldValue("fdEffectiveDateTbl":U + cContainerCode))
                                   ELSE ?)
                                   
    dEndDate                    = (IF oRequestHelper:getFieldValue("fdEndDateTbl":U + cContainerCode) <> "yyyy/mm/dd":U 
                                   THEN DATE(oRequestHelper:getFieldValue("fdEndDateTbl":U + cContainerCode))
                                   ELSE ?) 
    NO-ERROR.                              

  IF oRequestHelper:getFieldValue("chDetailDefaultTbl":U + cContainerCode) = "YES":U 
  THEN DO:
    DATASET dsTariff:HANDLE:EMPTY-DATASET().
    ASSIGN
      oTarifflinkSearch = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE) 
      
      lSuccess          = oTarifflinkSearch:SetCriteria("BufferList","tt_tariff_link")
      lSuccess          = oTarifflinkSearch:SetFilterCriteria("tt_tariff_link.tariff_code":U, "=":U, cAltValue)
      lSuccess          = oTarifflinkSearch:SetFilterCriteria("tt_tariff_link.pr_type":U    , "=":U, cPrType)
      lSuccess          = oTarifflinkSearch:fetchTariffData()
      .

    FOR FIRST tt_tariff_link:
      ASSIGN dOwningObj = tt_tariff_link.tariff_link_obj. 
    END.  /* FOR FIRST tt_tariff_link: */
  END.  /* IF oRequestHelper:getFieldValue("chDetailDefaultTbl":U + cContainerCode) = "YES":U */

  IF NOT {&ErrorStatus} THEN
  DO:
    CASE cRecordAction:
      WHEN "modify":U THEN
      DO:

        oAuthGroupDetail:focusAuthGroupDetail(dAuthGroupDetailObj) NO-ERROR.

        ASSIGN           
          oAuthGroupDetail:LineNumber           = iLineNumber
          oAuthGroupDetail:EffectiveDate        = dEffectiveDate
          oAuthGroupDetail:EndDate              = dEndDate
          oAuthGroupDetail:AuthGroupObj         = dAuthGroupObj
          oAuthGroupDetail:OwningEntityMnemonic = cOwningEntityMnemonic
          oAuthGroupDetail:OwningObj            = dOwningObj
          oAuthGroupDetail:OwningKey            = cOwningKey
          oAuthGroupDetail:AltValue             = cAltValue
          oAuthGroupDetail:PrType               = cPrType
          oAuthGroupDetail:SubPrType            = INTEGER(cSubPrType)
          oAuthGroupDetail:BaseRate             = cBaseRate
          oAuthGroupDetail:ARSRate              = cARSRate
          oAuthGroupDetail:Quantity             = iQuantity
          oAuthGroupDetail:Amount               = dAmount
          oAuthGroupDetail:ClaimCode            = iClaimCode
          oAuthGroupDetail:ClaimType            = cClaimType
          lSuccess                              = oAuthGroupDetail:SaveAuthGroupDetail()            
        NO-ERROR.               

        IF NOT {&ErrorStatus} AND NOT oAuthGroupDetail:ErrorObject:ErrorsExist 
        THEN
          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U 
            lSuccess                        = oResponseHelper:addFieldValue("fdGroupDetailObjTbl":U    + cContainerCode, STRING(oAuthGroupDetail:AuthGroupDetailObj))
            lSuccess                        = oResponseHelper:addFieldValue("fdOwningEntityObjTbl":U   + cContainerCode, STRING(oAuthGroupDetail:OwningObj))
            lSuccess                        = oResponseHelper:addFieldValue("frOwningEntityRowidTbl":U + cContainerCode, "":U)
           NO-ERROR.            
            
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN            
      DO:               
        ASSIGN lSuccess = oAuthGroupDetail:focusAuthGroupDetail(dAuthGroupDetailObj) NO-ERROR.
         
        IF NOT {&ErrorStatus} AND NOT oAuthGroupDetail:AuthGroupDetailInFocus 
        THEN  
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U 
            NO-ERROR.
        ELSE 
          ASSIGN lSuccess = oAuthGroupDetail:removeauthgroupdetail() NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthGroupDetail:ErrorObject:ErrorsExist
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
          oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported":U, cAction)
          oResponseHelper:ResponseMessage = "Unable to perform action":U 
         NO-ERROR.
      END. /* OTHERWISE */       
    END CASE.
  END. /*IF NOT {&ErrorStatus} THEN*/
    
  IF {&ErrorStatus} OR oAuthGroupDetail:ErrorObject:ErrorsExist THEN
  DO:
    ASSIGN 
     
      oResponseHelper:RequestValid    = FALSE
      
      hErrorHandle                    = oAuthGroupDetail:ErrorObject:getErrorTableHandle()
       
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
      lSuccess                        = oResponseHelper:setError(hErrorHandle)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
    
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
  END.   /*IF oUserFlag:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
   
  /* Cleanup */ 
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)    THEN DELETE OBJECT oRequestHelper    NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper)   THEN DELETE OBJECT oResponseHelper   NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthGroupDetail)  THEN DELETE OBJECT oAuthGroupDetail  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oTarifflinkSearch) THEN DELETE OBJECT oTarifflinkSearch NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxSaveSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveSearchResults Procedure 
PROCEDURE ajaxSaveSearchResults :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
  
  DEFINE VARIABLE oAuthGroup                AS cls.maauthgroup            NO-UNDO.
  DEFINE VARIABLE oRequestHelper            AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper           AS cls.maajaxresponsehelper   NO-UNDO.
                                            
  DEFINE VARIABLE hErrorHandle              AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE cContainerCode            AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAction                   AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cRecordAction             AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthGroupCode            AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cAuthGroupDescription     AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE iOptionCode               AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE iLineNumber               AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj             AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dInsurerObj               AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dEffectiveDate            AS DATE                       NO-UNDO.
  DEFINE VARIABLE dEndDate                  AS DATE                       NO-UNDO.
  DEFINE VARIABLE lSuccess                  AS LOGICAL                    NO-UNDO.
  
    
  ASSIGN
    cContainerCode              = ipcValidationArgument
                                
    oRequestHelper              = NEW cls.maajaxrequesthelper (INPUT get-value('FldLst'), INPUT get-value('ValList'))
    oResponseHelper             = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
                                
    oAuthGroup                  = NEW cls.maauthgroup()
                                       
    cRecordAction               =         oRequestHelper:getFieldValue("fcAction":U           + cContainerCode)
    cAuthGroupCode              =         oRequestHelper:getFieldValue("fcAuthGroupCode":U    + cContainerCode)
    cAuthGroupDescription       =         oRequestHelper:getFieldValue("fcDescription":U      + cContainerCode)    
    iLineNumber                 = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U       + cContainerCode))
    iOptionCode                 = INTEGER(oRequestHelper:getFieldValue("fiOptionCode":U       + cContainerCode))
    dAuthGroupObj               = DECIMAL(oRequestHelper:getFieldValue("fdAuthGroupObj":U     + cContainerCode))
    dInsurerObj                 = DECIMAL(oRequestHelper:getFieldValue("fdInsurerObj":U       + cContainerCode))
                                
    dEffectiveDate              = (IF oRequestHelper:getFieldValue("fdEffectiveDate":U + cContainerCode) <> "yyyy/mm/dd":U
                                   THEN DATE(oRequestHelper:getFieldValue("fdEffectiveDate":U + cContainerCode))
                                   ELSE ?)
                                   
    dEndDate                    = (IF oRequestHelper:getFieldValue("fdEndDate":U + cContainerCode) <> "yyyy/mm/dd":U 
                                   THEN DATE(oRequestHelper:getFieldValue("fdEndDate":U + cContainerCode))
                                   ELSE ?) 
    NO-ERROR.                              
  
  IF NOT {&ErrorStatus} THEN
  DO:
    CASE cRecordAction:
      WHEN "modify":U THEN
      DO:
        oAuthGroup:focusAuthGroup(dAuthGroupObj) NO-ERROR.
       
        ASSIGN           
          oAuthGroup:LineNumber        = iLineNumber
          oAuthGroup:EffectiveDate     = dEffectiveDate
          oAuthGroup:EndDate           = dEndDate
          oAuthGroup:InsurerObj        = dInsurerObj           
          oAuthGroup:OptionCode        = iOptionCode
          oAuthGroup:AuthGroupCode     = cAuthGroupCode
          oAuthGroup:Description       = cAuthGroupDescription
          lSuccess                     = oAuthGroup:SaveAuthGroup()            
        NO-ERROR.    
                   
        IF NOT {&ErrorStatus} AND NOT oAuthGroup:ErrorObject:ErrorsExist 
        THEN
          ASSIGN 
            oResponseHelper:RequestValid    = TRUE
            oResponseHelper:ResponseMessage = "Record successfully saved":U 
            lSuccess                        = oResponseHelper:addFieldValue("fdAuthGroupObj":U + cContainerCode, STRING(oAuthGroup:AuthGroupObj))
           NO-ERROR.            
            
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN            
      DO:               
        ASSIGN lSuccess = oAuthGroup:focusAuthGroup (dAuthGroupObj) NO-ERROR.
         
        IF NOT {&ErrorStatus} AND NOT oAuthGroup:AuthGroupInFocus 
        THEN  
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U 
            NO-ERROR.
        ELSE 
          ASSIGN lSuccess = oAuthGroup:removeAuthGroup() NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthGroup:ErrorObject:ErrorsExist
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
          oResponseHelper:ReturnValue     = SUBSTITUTE("Request '&1' not supported":U, cAction)
          oResponseHelper:ResponseMessage = "Unable to perform action":U 
         NO-ERROR.
      END. /* OTHERWISE */       
    END CASE.
  END. /*IF NOT {&ErrorStatus} THEN*/
  
        
  IF {&ErrorStatus} OR oAuthGroup:ErrorObject:ErrorsExist THEN
  DO:
    ASSIGN 
      
      oResponseHelper:RequestValid    = FALSE
      
      hErrorHandle                    = oAuthGroup:ErrorObject:getErrorTableHandle()
       
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
      lSuccess                        = oResponseHelper:setError(hErrorHandle)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
  
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
  END.   /*IF oUserFlag:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
   
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)  THEN DELETE OBJECT oRequestHelper  NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper) THEN DELETE OBJECT oResponseHelper NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oAuthGroup)      THEN DELETE OBJECT oAuthGroup      NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidation Procedure 
PROCEDURE ajaxValidation :
/*------------------------------------------------------------------------------
  Purpose:     Ajax Validation for Auth Group Wob
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  { ma/app/maauthgrpwobajxval.i }
   
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
  DEFINE INPUT PARAMETER ipcWhatToDo AS CHARACTER  NO-UNDO.
                                          
&IF {&DBDFMA} >= 10195 &THEN
 
  DEFINE VARIABLE lSuccess                  AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cRecordAction             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthGroupCode            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthGroupDescription     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningKey                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAltValue                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cClaimType                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPrType                   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cSubDiscipline            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBaseRate                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cArsRate                  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE iClaimCode                AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iQuantity                 AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRow                      AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iLineNumber               AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iOptionCode               AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dInsurerObj               AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAmount                   AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthGroupDetailObj       AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dOwningObj                AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dEffectiveDate            AS DATE                      NO-UNDO.
  DEFINE VARIABLE dEndDate                  AS DATE                      NO-UNDO.
  DEFINE VARIABLE oSearch                   AS cls.maauthgroupsearch NO-UNDO.
  DEFINE VARIABLE oTarifflinkSearch         AS ma.cls.matariffsearch     NO-UNDO.
  
  DATASET dsAuthGroup:EMPTY-DATASET().
  
  ASSIGN oSearch = NEW cls.maauthgroupsearch(DATASET dsAuthGroup BY-REFERENCE).
  
  
  IF goWob:Mode = "Search":U THEN
  DO:
    IF CAN-DO("SearchSubmit":U, goWob:SubmitValue) THEN
    DO:        
      DO-BLK:
      DO iRow = 1 TO INTEGER(get-value(goCntSearchResults:ContainerCode + "_rowsrendered":U)):
      
        ASSIGN
          cRecordAction               =         get-value("fcAction":U        + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthGroupCode              =         get-value("fcAuthGroupCode":U + goCntSearchResults:ContainerCode + STRING(iRow))
          cAuthGroupDescription       =         get-value("fcDescription":U   + goCntSearchResults:ContainerCode + STRING(iRow))  
          iLineNumber                 = INTEGER(get-value("fiLineNumber":U    + goCntSearchResults:ContainerCode + STRING(iRow)))
          iOptionCode                 = INTEGER(get-value("fiOptionCode":U    + goCntSearchResults:ContainerCode + STRING(iRow)))
          dAuthGroupObj               = DECIMAL(get-value("fdAuthGroupObj":U  + goCntSearchResults:ContainerCode + STRING(iRow)))
                                      
          dAuthGroupObj               = (IF dAuthGroupObj <= 0 
                                         THEN iRow * -1 ELSE dAuthGroupObj) 
                                        
          dInsurerObj                 = DECIMAL(get-value("fdInsurerObj":U    + goCntSearchResults:ContainerCode + STRING(iRow)))
          dEffectiveDate              =    DATE(get-value("fdEffectiveDate":U + goCntSearchResults:ContainerCode + STRING(iRow)))
          dEndDate                    =    DATE(get-value("fdEndDate":U       + goCntSearchResults:ContainerCode + STRING(iRow)))
          .
          
        /* We dont want to update records that did not change but create a temp table record for all rows
           so that if there are any errors, the screen will be built for the user as it was when they submitted
           from this temp table*/
        IF cAuthGroupCode <> "":U THEN
        DO:
          CREATE tt_auth_group.
        
          ASSIGN
             tt_auth_group.record_action   = cRecordAction
             tt_auth_group.line_number     = iRow
             tt_auth_group.auth_group_obj  = dAuthGroupObj
             tt_auth_group.effective_date  = dEffectiveDate
             tt_auth_group.end_date        = dEndDate
             tt_auth_group.insurer_obj     = dInsurerObj
             tt_auth_group.option_code     = iOptionCode
             tt_auth_group.auth_group_code = cAuthGroupCode
             tt_auth_group.description     = cAuthGroupDescription.
                                                     
          VALIDATE tt_auth_group.
          
          IF cRecordAction = "Delete":U THEN
          DO:
            CREATE tt_deleted.
            ASSIGN tt_deleted.owning_obj = dAuthGroupObj
                   tt_deleted.owning_key = "":U.
                   
            VALIDATE tt_deleted.       
          END. /*IF cRecordAction = "Delete":U THEN*/
        END. /*IF cAuthRuleCode <> "":U*/      
      END. /* DO iRow = 1 */                                                                     
    END. /*IF CAN-DO("SearchSubmit":U, goWob:SubmitValue) THEN*/
  END. /*IF goWob:Mode = "Search":U THEN*/
  
  IF goWob:Mode = "Maint":U THEN
  DO:
    IF CAN-DO(  "Submit->Add,":U
              + "Submit->Change,":U
              + "Confirm->Delete":U,
              TRIM(ipcWhatToDo) ) THEN
    DO TRANSACTION:
    
      ASSIGN     
        cAuthGroupCode            =         get-value("fcAuthGroupCode":U + goCntMaint:ContainerCode)
        cAuthGroupDescription     =         get-value("fcDescription":U   + goCntMaint:ContainerCode)  
        iOptionCode               = INTEGER(get-value("fiOptionCode":U    + goCntMaint:ContainerCode))
        dInsurerObj               = DECIMAL(get-value("fdInsurerObj":U    + goCntMaint:ContainerCode))
        dEffectiveDate            =    DATE(get-value("fdEffectiveDate":U + goCntMaint:ContainerCode))
        dEndDate                  =    DATE(get-value("fdEndDate":U       + goCntMaint:ContainerCode)).
      
      IF goWob:CurrentObj <> "":U AND ipcWhatToDo <> "Submit->Add":U THEN
      DO:
        oSearch:SetFilterCriteria("tt_auth_group.auth_group_obj":U, "=":U, DECIMAL(goWob:CurrentObj)).
        
        oSearch:fetchData().
      END. /*IF goWob:CurrentObj <> "":U THEN*/
      
      
      FIND FIRST tt_auth_group NO-LOCK NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
      
      
      IF ipcWhatToDo = "Submit->Add":U OR ipcWhatToDo = "Submit->Change":U AND cAuthGroupCode <> "":U THEN
      DO:
        IF NOT AVAILABLE tt_auth_group THEN
        DO:
          CREATE tt_auth_group.
          
          ASSIGN tt_auth_group.auth_group_obj = (IF ipcWhatToDo = "Submit->Add":U 
                                                           THEN 0 ELSE DECIMAL(goWob:CurrentObj)).
        END. /*IF NOT AVAILABLE tt_auth_rule THEN*/
      
        IF AVAILABLE tt_auth_group
        THEN
          ASSIGN
             tt_auth_group.record_action   = "MODIFY":U
             tt_auth_group.line_number     = 1
             tt_auth_group.effective_date  = dEffectiveDate
             tt_auth_group.end_date        = dEndDate
             tt_auth_group.insurer_obj     = dInsurerObj
             tt_auth_group.option_code     = iOptionCode
             tt_auth_group.auth_group_code = cAuthGroupCode
             tt_auth_group.description     = cAuthGroupDescription.
        
        VALIDATE tt_auth_group.
      END. /*IF cAuthRuleCode <> "":U AND cRecordAction = "MODIFY":U THEN*/                                                                                 
      
      IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_group THEN
      DO:
        ASSIGN tt_auth_group.record_action = "DELETE":U.
        
        VALIDATE tt_auth_group.
      END. /*IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_rule THEN*/
    END. /*IF CAN-DO("Submit":U, goWob:SubmitValue) THEN*/
    
    IF CAN-DO("MaintSubmit":U, goWob:SubmitValue) THEN
    DO:        
      DO-BLK:
      DO iRow = 1 TO INTEGER(get-value(goCntMaintDetail:ContainerCode + "_rowsrendered":U)):
      
        ASSIGN
          cRecordAction               =         get-value("fcAction":U                  + goCntMaintDetail:ContainerCode + STRING(iRow))
          cOwningEntityMnemonic       =         get-value("cbOwningEntityMnemonicTbl":U + goCntMaintDetail:ContainerCode + STRING(iRow))
          cOwningKey                  =         get-value("fcOwningEntityKeyTbl":U      + goCntMaintDetail:ContainerCode + STRING(iRow))
          cAltValue                   =         get-value("fcOwningEntityCodeTbl":U     + goCntMaintDetail:ContainerCode + STRING(iRow))
          cPrType                     =         get-value("fiDisciplineTbl":U           + goCntMaintDetail:ContainerCode + STRING(iRow))
          cClaimType                  =         get-value("cbClaimTypeTbl":U            + goCntMaintDetail:ContainerCode + STRING(iRow))
          iLineNumber                 = INTEGER(get-value("fiLineNumber":U              + goCntMaintDetail:ContainerCode + STRING(iRow)))
          iClaimCode                  = INTEGER(get-value("fiClaimCodeTbl":U            + goCntMaintDetail:ContainerCode + STRING(iRow)))
          iQuantity                   = INTEGER(get-value("fiQuantityTbl":U             + goCntMaintDetail:ContainerCode + STRING(iRow)))
          dAmount                     = DECIMAL(get-value("fiAmountTbl":U               + goCntMaintDetail:ContainerCode + STRING(iRow)))
          dAuthGroupObj               = DECIMAL(get-value("fdAuthGroupObjArgumentTbl":U + goCntMaintDetail:ContainerCode + STRING(iRow)))
          dAuthGroupDetailObj         = DECIMAL(get-value("fdGroupDetailObjTbl":U       + goCntMaintDetail:ContainerCode + STRING(iRow)))
          dAuthGroupDetailObj         = (IF dAuthGroupDetailObj <= 0 THEN iRow * -1 ELSE dAuthGroupDetailObj)
          dOwningObj                  = DECIMAL(get-value("fdOwningEntityObjTbl":U      + goCntMaintDetail:ContainerCode + STRING(iRow)))
          dEffectiveDate              =    DATE(get-value("fdEffectiveDateTbl":U        + goCntMaintDetail:ContainerCode + STRING(iRow))) 
          dEndDate                    =    DATE(get-value("fdEndDateTbl":U              + goCntMaintDetail:ContainerCode + STRING(iRow)))
          cSubDiscipline              =         get-value("fiSubDisciplineTbl":U        + goCntMaintDetail:ContainerCode + STRING(iRow))
          cBaseRate                   =         get-value("fcBaseRateTbl":U             + goCntMaintDetail:ContainerCode + STRING(iRow))
          cArsRate                    =         get-value("fcArsRateTbl":U              + goCntMaintDetail:ContainerCode + STRING(iRow)).
                                      
        IF get-value("chDetailDefaultTbl":U + goCntMaintDetail:ContainerCode + STRING(iRow)) = "YES":U 
        THEN DO:

          ASSIGN
          oTarifflinkSearch = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE) 
          
          lSuccess          = oTarifflinkSearch:SetCriteria("BufferList","tt_tariff_link")
          lSuccess          = oTarifflinkSearch:SetFilterCriteria("tt_tariff_link.tariff_code":U,       "=":U, cAltValue)
          lSuccess          = oTarifflinkSearch:SetFilterCriteria("tt_tariff_link.tariff_link_defualt":U, "=":U, TRUE)
          lSuccess          = oTarifflinkSearch:fetchTariffData()
          .
          
          FOR FIRST tt_tariff_link:
            ASSIGN dOwningObj = tt_tariff_link.tariff_link_obj. 
          END. /* FOR FIRST tt_tariff_link */
        END.
        
        /* We dont want to update records that did not change but create a temp table record for all rows
           so that if there are any errors, the screen will be built for the user as it was when they submitted
           from this temp table*/
           
        IF dOwningObj <> 0.00 OR cOwningKey <> "" THEN
        DO:
          CREATE tt_auth_group_detail.
        
          ASSIGN
             tt_auth_group_detail.record_action          = cRecordAction
             tt_auth_group_detail.auth_group_detail_obj  = dAuthGroupDetailObj
             tt_auth_group_detail.line_number            = iLineNumber
             tt_auth_group_detail.effective_date         = dEffectiveDate
             tt_auth_group_detail.end_date               = dEndDate
             tt_auth_group_detail.auth_group_obj         = dAuthGroupObj
             tt_auth_group_detail.owning_entity_mnemonic = cOwningEntityMnemonic
             tt_auth_group_detail.owning_obj             = dOwningObj
             tt_auth_group_detail.owning_key             = cOwningKey
             tt_auth_group_detail.owning_alt_value       = cAltValue
             tt_auth_group_detail.pr_type                = cPrType
             tt_auth_group_detail.base_rate              = cBaseRate
             tt_auth_group_detail.ars_rate               = cARSRate
             tt_auth_group_detail.sub_pr_type            = INTEGER(cSubDiscipline)
             tt_auth_group_detail.quantity               = iQuantity
             tt_auth_group_detail.amount                 = dAmount
             tt_auth_group_detail.claim_code             = iClaimCode
             tt_auth_group_detail.claim_type             = cClaimType.
                                                     
          VALIDATE tt_auth_group_detail.

          IF cRecordAction = "Delete":U THEN
          DO:
            CREATE tt_deleted.
            ASSIGN tt_deleted.owning_obj = dAuthGroupDetailObj
                   tt_deleted.owning_key = "":U.
                   
            VALIDATE tt_deleted.       
          END. /*IF cRecordAction = "Delete":U THEN*/
        END. /*IF cAuthRuleCode <> "":U*/      
      END. /* DO iRow = 1 */                                                                     
    END. /* IF CAN-DO("MaintSubmit":U, goWob:SubmitValue) THEN */
  END. /*IF goWob:Mode = "Submit":U THEN*/
  
  IF CAN-DO("MaintSubmit,SearchSubmit,Submit,Confirm":U, goWob:SubmitValue) THEN
  DO:
&IF {&DBDFMA} >= 010195 &THEN
    mipEnv:Health:AuthMaintenance:saveAuthGroup(INPUT-OUTPUT DATASET dsAuthGroup BY-REFERENCE).
&ENDIF
    ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_auth_group_error).
    
    IF goWob:SubmitValue = "SearchSubmit":U 
    OR goWob:SubmitValue = "MaintSubmit":U THEN
    DO:
      FOR EACH tt_auth_group_error NO-LOCK:
      
        FIND FIRST tt_deleted NO-LOCK
             WHERE tt_deleted.owning_obj = tt_auth_group_error.owning_obj
          NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
        
        IF AVAILABLE tt_deleted 
        THEN DELETE tt_deleted. 
      
      END. /*FOR EACH tt_auth_rule_error NO-LOCK:*/
    END. /*IF goWob:SubmitValue = "SearchSubmit":U THEN*/
    
    IF goWob:Mode = "Maint":U THEN
    DO:
      IF Warpspeed:ValidationError
      THEN 
&IF {&DBDFMA} >= 010195 &THEN
        mipEnv:Health:maUiService:setContainerErrors(TEMP-TABLE tt_auth_group_error:HANDLE, goCntMaint, "hamag":U, DECIMAL(Warpspeed:CurrentObj), "":U).
&ENDIF
        
      IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN
      DO:
        FIND FIRST tt_auth_group NO-LOCK NO-ERROR.
        
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        ASSIGN 
           goWob:CurrentObj     = IF AVAILABLE tt_auth_group THEN STRING(tt_auth_group.auth_group_obj) ELSE ""
           Warpspeed:CurrentObj = goWob:CurrentObj
           lSuccess             = wsUIService:setObj(goWob:ObjectCode, goWob:CurrentObj).
      
      END. /*IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN*/      
    END. /*IF goWob:Mode = "Maint":U THEN*/
  END. /*IF CAN-DO("SearchSubmit,Search":U goWob:SubmitValue) THEN*/
  
  { mip/inc/mipcatcherror.i oTarifflinkSearch
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch.
                IF VALID-OBJECT(oTarifflinkSearch) THEN DELETE OBJECT oTarifflinkSearch."}
    
&ENDIF //&IF {&DBDFMA} >= 10195 &THEN 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-customRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customRenderProcedure Procedure 
PROCEDURE customRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN 
  DEFINE VARIABLE cOEM       AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cOwningKey AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cPrType    AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cBaseRate  AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE dOwningObj AS DECIMAL      NO-UNDO.
  DEFINE VARIABLE lSuccess   AS LOGICAL      NO-UNDO.
  DEFINE VARIABLE oQuery     AS cls.mipquery NO-UNDO.
  
  IF  VALID-OBJECT(ipoControl:ParentContainer) 
  AND VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery)
  THEN
    ASSIGN oQuery = ipoControl:ParentContainer:ContainerQuery.
                                                    
  CASE ipoControl:RenderArgument:
    
    WHEN "OwningEntityRowid":U THEN
    DO:
      IF VALID-OBJECT(oQuery) THEN
      DO:
        ASSIGN 
           cOEM        =         oQuery:getFieldAttribute("tt_auth_group_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U)
           cOwningKey  =         oQuery:getFieldAttribute("tt_auth_group_detail.owning_key":U            , "BUFFER-VALUE":U)
           dOwningObj  = DECIMAL(oQuery:getFieldAttribute("tt_auth_group_detail.owning_obj":U            , "BUFFER-VALUE":U)).
           
        IF cOEM <> "":U 
        THEN   
          ASSIGN 
             lSuccess    = mipEnv:miDBEntity:focusTable(INPUT cOEM) 
             
             lSuccess    = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U
                            THEN mipEnv:miDBEntity:findRecord(INPUT dOwningObj)
                            ELSE mipEnv:miDBEntity:findRecord(INPUT cOwningKey)).
                            
        
        ASSIGN ipoControl:ControlValue = (IF mipEnv:miDBEntity:RecordAvailable AND cOEM <> "":U AND (dOwningObj <> 0.00 OR cOwningKey <> "":U)
                                          THEN STRING(mipEnv:miDBEntity:RecordRowid)
                                          ELSE "":U).         

      END. /*IF VALID-OBJECT(oQuery) THEN*/  
      
      ipoControl:RenderASInput().
            
    END. /*WHEN "OwningEntityRowid":U THEN*/

    WHEN "DetailDefault":U THEN
    DO:
      IF VALID-OBJECT(oQuery) THEN
      DO:
        ASSIGN 
           cOEM        =         oQuery:getFieldAttribute("tt_auth_group_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U)
           cOwningKey  =         oQuery:getFieldAttribute("tt_auth_group_detail.owning_key":U            , "BUFFER-VALUE":U)
           dOwningObj  = DECIMAL(oQuery:getFieldAttribute("tt_auth_group_detail.owning_obj":U            , "BUFFER-VALUE":U)).
           
        IF cOEM = "htmtl":U 
        THEN DO:
          FIND htm_tariff_link NO-LOCK
            WHERE htm_tariff_link.tariff_link_obj = dOwningObj
            NO-ERROR.
          IF AVAILABLE htm_tariff_link THEN
            ASSIGN ipoControl:ControlValue = STRING(htm_tariff_link.tariff_link_default).
          ELSE 
            ASSIGN ipoControl:ControlValue = "":U.

          ASSIGN ipoControl:ControlToken = IF dOwningObj = 0
                                           THEN "Updatable":U
                                           ELSE "DISABLED":U.
        END.  /* IF cOEM = "htmtl":U  */
        ELSE
          ASSIGN ipoControl:ControlValue = "":U
                 ipoControl:ControlToken = IF LOOKUP(cOEM,"hlmnl,hlmcr,prtype":U) <> 0 
                                           THEN "DISABLED":U
                                           ELSE "Updatable":U.

      END. /*IF VALID-OBJECT(oQuery) THEN*/
      
      ipoControl:RenderASCheckBox().
    END.  /* WHEN "DetailDefault":U THEN */

    WHEN "DisableFields" THEN 
    DO:
    
      IF VALID-OBJECT(oQuery) THEN
      DO:
        ASSIGN cOEM       = oQuery:getFieldAttribute("tt_auth_group_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U)
               dOwningObj = DECIMAL(oQuery:getFieldAttribute("tt_auth_group_detail.owning_obj":U    , "BUFFER-VALUE":U)).
        
        CASE cOEM:
           
          WHEN "hlmnl":U THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.
          
          WHEN "hlmcr":U THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.

          WHEN "htmtl":U THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.

          WHEN "prtype":U THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.

          OTHERWISE 
            ASSIGN ipoControl:ControlToken = "":U.
        
        END CASE. /* CASE oQuery:getFieldAttribute("tt_auth_group_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U) */                                                                                 
      END. /*IF VALID-OBJECT(oQuery) THEN */

      ipoControl:RenderASInput().

    END. /* WHEN "DisableFields" THEN  */
    
    WHEN "DisableButtons" THEN 
    DO:
    
      IF VALID-OBJECT(oQuery) THEN
      DO:
        ASSIGN cOEM = oQuery:getFieldAttribute("tt_auth_group_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U).
        
        CASE cOEM:
           
          WHEN "hlmnl" THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.
          
          WHEN "hlmcr" THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.

          WHEN "htmtl" THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.

          WHEN "prtype" THEN
            ASSIGN ipoControl:ControlToken = "DISABLED":U.
          
          OTHERWISE 
            ASSIGN ipoControl:ControlToken = "":U.
        
        END CASE. /* CASE oQuery:getFieldAttribute("tt_auth_group_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U) */                                                                                 
      END. /*IF VALID-OBJECT(oQuery) THEN */

      ipoControl:RenderASLookupButton().

    END. /* WHEN "DisableFields" THEN  */
      
  END CASE.

  { mip/inc/mipcatcherror.i }

&ENDIF //&IF {&DBDFMA} >= 10195 &THEN 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainer Procedure 
PROCEDURE defineContainer :
/*------------------------------------------------------------------------------
  Purpose   : Define authorisation rule specific containers
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/

&IF {&DBDFMA} >= 10195 &THEN 

  RUN populateComboLists IN TARGET-PROCEDURE.
  
  
  IF goWob:Mode = "Search":U THEN 
  DO:
    RUN defineContainerSearchFilter IN TARGET-PROCEDURE.    
    
  END. /*IF goWob:Mode = "Search":U THEN */
  
  
  IF goWob:Mode = "Maint":U THEN 
  DO:
    RUN defineContainerMaint IN TARGET-PROCEDURE.
                
  END. /*IF goWob:Mode = "Maint":U THEN */
      
  { mip/inc/mipcatcherror.i }

&ENDIF //&IF {&DBDFMA} >= 10195 &THEN 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerMaint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerMaint Procedure 
PROCEDURE defineContainerMaint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
  /*Maintenance*/                                                                    
  ASSIGN                                                                   
    goCntMaint                  = goWob:getContainer("Maint":U)
    goCntMaint:ContainerMode    = goWob:SubmitValue
    goCntMaint:ContainerTitle   = "Authorisation Group"
    goCntMaint:ViewOnly         = TRUE
    goCntMaint:QueryString      = "FOR EACH tt_auth_group NO-LOCK":U
                                + "   WHERE tt_auth_group.auth_group_obj = &1":U
                                
    oControl                    = goCntMaint:addControl("fcAuthGroupCode":U           + goCntMaint:ContainerCode , "wsInput":U        , "20":U  , "tt_auth_group.auth_group_code":U   , "character":U, 1, 1, "Authorisation Group Code:":U)
    oControl:ControlToken       = "ReadOnly":U WHEN NOT CAN-DO("0,":U,goWob:CurrentObj) AND goWob:SubmitValue <> "Copy":U

    oControl                    = goCntMaint:addControl("fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode , "wsInput":U        , "5":U   , "":U                               , "character":U, 1, 2, "":U)
    oControl:ControlToken       = "Hidden":U
    oControl:ControlValue       = "ermin":U

    oControl                    = goCntMaint:addControl("fcInsurerArgumentField":U    + goCntMaint:ContainerCode , "wsInput":U        , "5":U   , "":U                               , "character":U, 1, 2, "":U)
    oControl:ControlToken       = "Hidden":U
    oControl:ControlValue       = "[CodeField]":U

    oControl                    = goCntMaint:addControl("fdInsurerObj":U              + goCntMaint:ContainerCode , "wsInput":U        , "1":U   , "tt_auth_group.insurer_obj":U , "character":U, 1, 2, "":U)
    oControl:ControlToken       = "Hidden":U

    oControl                    = goCntMaint:addControl("fcInsurer":U                 + goCntMaint:ContainerCode , "wsInput":U        , "20":U  , "":U                               , "character":U, 1, 2, "":U)
    oControl:ControlTooltip     = "Please enter a valid client":U
    oControl:ajaxValidation     = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields       = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls     = "fcInsurer":U + goCntMaint:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode + ",fcInsurerArgumentField":U + goCntMaint:ContainerCode
    oControl:ReturnFields       = "[RecordObj]":U
    oControl:ReturnControls     = "fdInsurerObj":U + goCntMaint:ContainerCode

    oControl                    = goCntMaint:addControl("buInsurerBtn":U              + goCntMaint:ContainerCode , "wsLookupButton":U , "":U    , "":U                               , "":U         , 1, 2, "Client:":U)
    oControl:LookupWobFLA       = "ermin":U
    oControl:LookupFields       = "erm_insurer.insurer_code":U
    oControl:LookupControls     = "fcInsurer":U + goCntMaint:ContainerCode
    oControl:ReturnFields       = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U
    oControl:ReturnControls     = "fcInsurerObj":U + goCntMaint:ContainerCode + ",fcInsurer":U + goCntMaint:ContainerCode
    oControl:CellLayoutMask     = "&1&2&3&4&5":U

    oControl                    = goCntMaint:addControl("fiOptionCode":U              + goCntMaint:ContainerCode , "wsCombo":U        , "20":U  , "tt_auth_group.option_code":U , "character":U  , 2, 1, "Option:":U)
    oControl:AdditionalItems    = "000 - All=0"
    oControl:QueryString        = "FOR EACH scheme NO-LOCK WHERE scheme.active = yes":U
    oControl:KeyField           = "scheme.scheme-code":U
    oControl:DisplayFields      = "scheme.scheme-code,scheme.short-name":U
    oControl:DisplayMask        = "&1 - &2"

    oControl                    = goCntMaint:addControl("fcDescription":U             + goCntMaint:ContainerCode , "wsTextArea":U     , "20,3":U, "tt_auth_group.description":U      , "character":U, 3, 1, "Description:":U)
    oControl                    = goCntMaint:addControl("fdEffectiveDate":U           + goCntMaint:ContainerCode , "wsInput":U        , "20":U  , "tt_auth_group.effective_date":U   , "date":U     , 4, 1, "Effective Date:":U)
    oControl                    = goCntMaint:addControl("fdEndDate":U                 + goCntMaint:ContainerCode , "wsInput":U        , "20":U  , "tt_auth_group.end_date":U         , "date":U     , 5, 1, "End Date:":U)
    .
  
                                          
  RUN defineContainerMaintDetail IN TARGET-PROCEDURE.                                                               
                                          
                                                                 
  ASSIGN goCntAudit              = mipEnv:Health:maUtility:getAuditContainer()
         goCntAudit:RowsToRender = ?.
  
  
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = FALSE &Container = goCntAudit }
  
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerMaintDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerMaintDetail Procedure 
PROCEDURE defineContainerMaintDetail :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl       AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl       AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE cControlList   AS CHARACTER         NO-UNDO INITIAL "fdOwningEntityObjTbl,fcOwningEntityCodeTbl,fiDisciplineTbl,fiSubDisciplineTbl,fcBaseRateTbl,fcArsRateTbl":U.
  DEFINE VARIABLE cControlBuList AS CHARACTER         NO-UNDO INITIAL "buDisciplineBtnTbl,buSubDisciplineBtnTbl,buBaseRateBtnTbl,buArsRateBtnTbl":U.

  /*Search Results*/
  ASSIGN
    goCntMaintDetail                        = NEW cls.mipwscontainer("MaintDetail":U + goWob:ObjectCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntMaintDetail:ContainerMode          = goWob:SubmitValue
    goCntMaintDetail:ShowGenericReportPrint = FALSE
    goCntMaintDetail:Collapsed              = FALSE
    goCntMaintDetail:ViewOnly               = NOT glEnquiryWob
    goCntMaintDetail:RowsToRender           = ?
    goCntMaintDetail:RowRenderProcedure     = "rowRenderProcedure":U
    goCntMaintDetail:RowRenderArgument      = goCntMaintDetail:ContainerCode
    goCntMaintDetail:DefaultContainerType   = "TABLE":U
    goCntMaintDetail:ContainerTitle         = "Authorisation Group Details":U
    goCntMaintDetail:QueryString            = "FOR EACH tt_auth_group_detail NO-LOCK":U
                                            + "   WHERE tt_auth_group_detail.auth_group_obj = &1":U
                                            + "      OR tt_auth_group_detail.auth_group_obj = 0.00":U
                                            + "      BY tt_auth_group_detail.line_number":U

    lSuccess                                = goWob:setContainer("MaintDetail":U,  goCntMaintDetail)

    oControl                                = goCntMaintDetail:addControl("fdGroupDetailObjTbl":U         + goCntMaintDetail:ContainerCode , "wsInput":U        , "20":U , "tt_auth_group_detail.auth_group_detail_obj":U, "decimal":U  ,  2, "":U)
    oControl:ControlToken                   = "Hidden":U

    oControl                                = goCntMaintDetail:addControl("cbOwningEntityMnemonicTbl":U   + goCntMaintDetail:ContainerCode , "wsCombo":U        , "20":U , "tt_auth_group_detail.owning_entity_mnemonic":U, "character":U,  3, "Entity":U)
    oControl:ControlClass                   = "+clMan":U
    oControl:AdditionalItems                = gcAllTableList
    oControl:JavascriptOnChange             = "fnCheckRowState(this,~"" + goCntMaintDetail:ContainerCode  + "~"); ":U

    oControl                                = goCntMaintDetail:addControl("fcOwningFieldArgumentTbl":U    + goCntMaintDetail:ContainerCode , "wsInput":U        , "15":U , "":U                                       , "character":U,  4, "":U)
    oControl:ControlToken                   = "Hidden":U
    oControl:ControlValue                   = "[CodeField]":U

    oControl                                = goCntMaintDetail:addControl("frOwningEntityRowidTbl":U      + goCntMaintDetail:ContainerCode , "wsInput":U        , "10":U , "":U                                       , "character":U,  4, "":U)
    oControl:ControlToken                   = "Hidden":U
    oControl:RenderProcedure                = "customRenderProcedure":U
    oControl:RenderArgument                 = "OwningEntityRowid":U

    oControl                                = goCntMaintDetail:addControl("fdOwningEntityObjTbl":U        + goCntMaintDetail:ContainerCode , "wsInput":U        , "10":U , "tt_auth_group_detail.owning_obj":U        , "decimal":U  ,  4, "":U)
    oControl:ControlClass                   = "+clHid":U

    oControl                                = goCntMaintDetail:addControl("fcOwningEntityKeyTbl":U        + goCntMaintDetail:ContainerCode , "wsInput":U        , "10":U , "tt_auth_group_detail.owning_key":U        , "character":U,  4, "":U)
    oControl:ControlToken                   = "Hidden":U

    oControl                                = goCntMaintDetail:addControl("fcOwningEntityCodeTbl":U       + goCntMaintDetail:ContainerCode , "wsInput":U        , "8":U  , "tt_auth_group_detail.owning_alt_value":U  , "character":U,  4, "":U)
    oControl:ControlTooltip                 = "Please enter a valid value":U

    oControl                                = goCntMaintDetail:addControl("buOwningEntityBtnTbl":U        + goCntMaintDetail:ContainerCode , "wsLookupButton":U , "":U   , "":U                                       , "":U         ,  4, "Item Code":U)
    oControl:CellLayoutMask                 = "&1&2&3&4&5&6":U
    oControl:JavascriptOnClick              = "fnSetChanged(this);":U
    oControl:LookupWobFLA                   = "slent":U
    oControl:LookupFields                   = "ROWID,CODE_FIELD":U
    oControl:LookupControls                 = "frOwningEntityRowidTbl":U + goCntMaintDetail:ContainerCode + ",fcOwningEntityCodeTbl":U + goCntMaintDetail:ContainerCode
    oControl:FilterFields                   = "QUERY_OEM":U
    oControl:FilterControls                 = "cbOwningEntityMnemonicTbl":U + goCntMaintDetail:ContainerCode
    oControl:ReturnFields                   = "ROWID,KEY_FIELD,CODE_FIELD":U
    oControl:ReturnControls                 = "frOwningEntityRowidTbl":U + goCntMaintDetail:ContainerCode + ",fdOwningEntityObjTbl":U + goCntMaintDetail:ContainerCode 
                                            + ",fcOwningEntityCodeTbl":U + goCntMaintDetail:ContainerCode 
    //oControl:ReturnFields                   = "ROWID,CODE_FIELD,[Discipline],[SubDiscipline],[BaseRate],[ArsRate]":U
    //oControl:ReturnControls                 = "frOwningEntityRowidTbl":U + goCntMaintDetail:ContainerCode + ",fcOwningEntityCodeTbl":U + goCntMaintDetail:ContainerCode 
    //                                        + ",fiDisciplineTbl":U       + goCntMaintDetail:ContainerCode + ",fiSubDisciplineTbl":U    + goCntMaintDetail:ContainerCode 
    //                                        + ",fcBaseRateTbl":U         + goCntMaintDetail:ContainerCode + ",fcArsRateTbl":U          + goCntMaintDetail:ContainerCode
    
    oControl                                = goCntMaintDetail:addControl("chDetailDefaultTbl":U          + goCntMaintDetail:ContainerCode , "wsCheckBox":U     , "5":U , "":U                                        , "logical":U  ,  5, "Default":U)
    oControl:JavascriptOnChange             = "fnSetDefaults(this,~"" + goCntMaintDetail:ContainerCode + "~");":U
    oControl:RenderProcedure                = "customRenderProcedure":U
    oControl:RenderArgument                 = "DetailDefault":U

    oControl:ajaxValidation                 = "START:" + Warpspeed:CurrentWob + ":ajaxValidation:":U
    oControl:FilterFields                   = "[TableMnemonic],[Rowid],[CodeField],[Discipline],[SubDiscipline],[BaseRate],[ArsRate],[Default]":U
    oControl:FilterControls                 = "cbOwningEntityMnemonicTbl":U + goCntMaintDetail:ContainerCode + ",frOwningEntityRowidTbl":U + goCntMaintDetail:ContainerCode
                                            + ",fcOwningEntityCodeTbl":U    + goCntMaintDetail:ContainerCode + ",fiDisciplineTbl":U        + goCntMaintDetail:ContainerCode + ",fiSubDisciplineTbl":U + goCntMaintDetail:ContainerCode
                                            + ",fcBaseRateTbl":U            + goCntMaintDetail:ContainerCode + ",fcArsRateTbl":U           + goCntMaintDetail:ContainerCode
                                            + ",chDetailDefaultTbl":U       + goCntMaintDetail:ContainerCode
    oControl:ReturnFields                   = "[Rowid],[RecordCode],[ObjField],[Discipline],[SubDiscipline],[BaseRate],[ArsRate]":U
    oControl:ReturnControls                 = "frOwningEntityRowidTbl":U + goCntMaintDetail:ContainerCode + ",fcOwningEntityCodeTbl":U + goCntMaintDetail:ContainerCode
                                            + ",fdOwningEntityObjTbl":U  + goCntMaintDetail:ContainerCode + ",fiDisciplineTbl":U + goCntMaintDetail:ContainerCode
                                            + ",fiSubDisciplineTbl":U    + goCntMaintDetail:ContainerCode + ",fcBaseRateTbl":U + goCntMaintDetail:ContainerCode
                                            + ",fcArsRateTbl":U          + goCntMaintDetail:ContainerCode

    oControl                                = goCntMaintDetail:addControl("fiDisciplineTbl":U + goCntMaintDetail:ContainerCode , "wsInput":U , "8":U   , "tt_auth_group_detail.pr_type":U , "integer":U  ,  6, "Discipline":U)
    oControl:ControlFormat                  = "999":U
    oControl:JavascriptOnChange             = "this.value = pad (this.value,3);":U

    oControl                                = goCntMaintDetail:addControl("buDisciplineBtnTbl":U + goCntMaintDetail:ContainerCode , "wsLookupButton":U , "":U    , "":U , "":U ,  6, "Discipline":U)
    oControl:CellLayoutMask                 = "&1 &2":U
    oControl:LookupWobFLA                   = "maprtype":U
    oControl:LookupFields                   = "prtype.pr-type":U
    oControl:LookupControls                 = "fiDisciplineTbl":U + goCntMaintDetail:ContainerCode
    oControl:FilterFields                   = "prtype.pr-type":U
    oControl:FilterControls                 = "fiDisciplineTbl":U + goCntMaintDetail:ContainerCode
    oControl:ReturnFields                   = "prtype.pr-type":U
    oControl:ReturnControls                 = "fiDisciplineTbl":U + goCntMaintDetail:ContainerCode

    oControl                                = goCntMaintDetail:addControl("fiSubDisciplineTbl":U + goCntMaintDetail:ContainerCode , "wsInput":U , "8":U , "tt_auth_group_detail.sub_pr_type":U , "integer":U , 7, "Sub<br>Discipline":U)
    oControl:ControlFormat                  = "999":U
    oControl:JavascriptOnChange             = "this.value = pad (this.value,3);":U

    oControl                                = goCntMaintDetail:addControl("buSubDisciplineBtnTbl":U + goCntMaintDetail:ContainerCode , "wsLookupButton":U , "":U , "":U , "":U , 7, "Sub<br>Discipline":U)
    oControl:CellLayoutMask                 = "&1 &2":U
    oControl:LookupWobFLA                   = "masubdisc":U
    oControl:LookupFields                   = "subdisc.subdisp-code":U
    oControl:LookupControls                 = "fiSubDisciplineTbl":U + goCntMaintDetail:ContainerCode
    oControl:FilterFields                   = "subdisc.pr-type,subdisc.subdisp-code":U
    oControl:FilterControls                 = "fiDisciplineTbl":U + goCntMaintDetail:ContainerCode + ",fiSubDisciplineTbl":U + goCntMaintDetail:ContainerCode
    oControl:ReturnFields                   = "subdisc.subdisp-code":U
    oControl:ReturnControls                 = "fiSubDisciplineTbl":U + goCntMaintDetail:ContainerCode

    oControl                                = goCntMaintDetail:addControl("fcBaseRateTbl":U + goCntMaintDetail:ContainerCode , "wsInput":U , "8":U , "tt_auth_group_detail.base_rate":U , "character":U, 8, "Base Rate":U)

    oControl                                = goCntMaintDetail:addControl("buBaseRateBtnTbl":U + goCntMaintDetail:ContainerCode , "wsLookupButton":U , "":U , "":U , "":U ,  8, "Base Rate":U)
    oControl:CellLayoutMask                 = "&1 &2":U
    oControl:LookupWobFLA                   = "baserate":U
    oControl:LookupFields                   = "baserate.base-rate":U
    oControl:LookupControls                 = "fcBaseRateTbl":U + goCntMaintDetail:ContainerCode
    oControl:FilterFields                   = "baserate.base-rate":U
    oControl:FilterControls                 = "fcBaseRateTbl":U + goCntMaintDetail:ContainerCode
    oControl:ReturnFields                   = "baserate.base-rate":U
    oControl:ReturnControls                 = "fcBaseRateTbl":U + goCntMaintDetail:ContainerCode

    oControl                                = goCntMaintDetail:addControl("fcArsRateTbl":U                + goCntMaintDetail:ContainerCode , "wsInput":U        , "8":U  , "tt_auth_group_detail.ars_rate":U          , "character":U,  9, "ARS Rate":U)

    oControl                                = goCntMaintDetail:addControl("buArsRateBtnTbl":U             + goCntMaintDetail:ContainerCode , "wsLookupButton":U , "":U   , "":U                                       , "":U         ,  9, "ARS Rate":U)
    oControl:CellLayoutMask                 = "&1 &2":U
    oControl:LookupWobFLA                   = "arsrate":U
    oControl:LookupFields                   = "arsrate.ars-rate":U
    oControl:LookupControls                 = "fcArsRateTbl":U + goCntMaintDetail:ContainerCode
    oControl:FilterFields                   = "arsrate.ars-rate":U
    oControl:FilterControls                 = "fcArsRateTbl":U + goCntMaintDetail:ContainerCode
    oControl:ReturnFields                   = "arsrate.ars-rate":U
    oControl:ReturnControls                 = "fcArsRateTbl":U + goCntMaintDetail:ContainerCode

    oControl                                = goCntMaintDetail:addControl("fiQuantityTbl":U             + goCntMaintDetail:ContainerCode , "wsInput":U        , "5":U  , "tt_auth_group_detail.quantity":U          , "integer":U  , 10, "Quantity":U)

    oControl                                = goCntMaintDetail:addControl("fiAmountTbl":U               + goCntMaintDetail:ContainerCode , "wsInput":U        , "5":U  , "tt_auth_group_detail.amount":U            , "integer":U  , 11, "Amount":U)

    oControl                                = goCntMaintDetail:addControl("fiClaimCodeTbl":U            + goCntMaintDetail:ContainerCode , "wsInput":U        , "8":U  , "tt_auth_group_detail.claim_code":U        , "integer":U  , 12, "Claim Code":U)
    oControl:ControlFormat                  = "999":U

    oControl                                = goCntMaintDetail:addControl("buClaimCodeBtnTbl":U         + goCntMaintDetail:ContainerCode , "wsLookupButton":U , "":U   , "":U                                       , "":U         , 12, "Claim Code":U)
    oControl:CellLayoutMask                 = "&1&2":U
    oControl:LookupWobFLA                   = "maclc":U
    oControl:LookupFields                   = "ccdesc.claim-code":U
    oControl:LookupControls                 = "fiClaimCodeTbl":U + goCntMaintDetail:ContainerCode
    oControl:ReturnFields                   = "ccdesc.claim-code":U
    oControl:ReturnControls                 = "fiClaimCodeTbl":U + goCntMaintDetail:ContainerCode

    oControl                                = goCntMaintDetail:addControl("cbClaimTypeTbl":U            + goCntMaintDetail:ContainerCode , "wsCombo":U        , "5":U  , "tt_auth_group_detail.claim_type":U        , "character":U, 13, "Claim<br>Type":U)
    oControl:AdditionalItems                = "|C|N|A|K|P|O":U
    oControl:ControlToolTip                 = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther":U

    oControl                                = goCntMaintDetail:addControl("fdEffectiveDateTbl":U        + goCntMaintDetail:ContainerCode , "wsInput":U        , "8":U  , "tt_auth_group_detail.effective_date":U    , "date":U     , 14, "Effective<br>Date":U)
    oControl:ControlClass                   = "+clMan":U

    oControl                                = goCntMaintDetail:addControl("fdEndDateTbl":U              + goCntMaintDetail:ContainerCode , "wsInput":U        , "8":U  , "tt_auth_group_detail.end_date":U          , "date":U     , 15, "End<br>Date":U)

    oControl                                = goCntMaintDetail:addControl("fdAuthGroupObjArgumentTbl":U + goCntMaintDetail:ContainerCode , "wsInput":U        , "20":U , "tt_auth_group_detail.auth_group_obj":U    , "decimal":U  , 16, "":U)
    oControl:ControlToken                   = "Hidden":U.

  DO iControl = 1 TO NUM-ENTRIES(cControlList):

    ASSIGN
      oControl                     = goCntMaintDetail:getControl(ENTRY(iControl, cControlList) + goCntMaintDetail:ContainerCode)

      oControl:ControlJavaScript   = ' onkeydown= "fnSetChanged(this)"':U

      oControl:JavascriptOnChange  = oControl:JavascriptOnChange
                                   + "var iRow           = this.name.replace(~"" + oControl:ControlName + "~",~"~"),":U
                                   + "    cContainerCode = ~"" + goCntMaintDetail:ContainerCode + "~";":U
                                   + "if(gvControlChanged!=~"~")~{":U
                                   + "  if(gvControlChanged!=~"buOwningEntityBtnTbl~"+cContainerCode+iRow)~{":U
                                   + "    fnSetControlValue(~"frOwningEntityRowidTbl~"+cContainerCode+iRow,~"~");":U
                                   + "  ~}else~{":U
                                   + "    gvControlChanged=~"fdOwningEntityObjTbl~"+cContainerCode+iRow;":U
                                   + "  ~}":U
                                   + "~}":U
                                   + "if(gvControlChanged!=this.name)return false;":U
                                   + "gvControlChanged=~"~";":U

      oControl:ajaxValidation      = "START:" + Warpspeed:CurrentWob + ":ajaxValidation:":U
      oControl:FilterFields        = "[TableMnemonic],[Rowid],[ObjField],[CodeField],[Discipline],[SubDiscipline],[BaseRate],[ArsRate],[Default]":U
      oControl:FilterControls      = "cbOwningEntityMnemonicTbl":U + goCntMaintDetail:ContainerCode  + ",frOwningEntityRowidTbl":U + goCntMaintDetail:ContainerCode
                                   + ",fdOwningEntityObjTbl":U + goCntMaintDetail:ContainerCode
                                   + ",fcOwningEntityCodeTbl":U + goCntMaintDetail:ContainerCode + ",fiDisciplineTbl":U + goCntMaintDetail:ContainerCode 
                                   + ",fiSubDisciplineTbl":U + goCntMaintDetail:ContainerCode + ",fcBaseRateTbl":U + goCntMaintDetail:ContainerCode 
                                   + ",fcArsRateTbl":U + goCntMaintDetail:ContainerCode + ",chDetailDefaultTbl":U + goCntMaintDetail:ContainerCode
      oControl:ReturnFields        = "[Rowid],[RecordCode],[ObjField],[Discipline],[SubDiscipline],[BaseRate],[ArsRate]":U
      oControl:ReturnControls      = "frOwningEntityRowidTbl":U + goCntMaintDetail:ContainerCode + ",fcOwningEntityCodeTbl":U + goCntMaintDetail:ContainerCode 
                                   + ",fdOwningEntityObjTbl":U + goCntMaintDetail:ContainerCode + ",fiDisciplineTbl":U + goCntMaintDetail:ContainerCode 
                                   + ",fiSubDisciplineTbl":U + goCntMaintDetail:ContainerCode + ",fcBaseRateTbl":U + goCntMaintDetail:ContainerCode 
                                   + ",fcArsRateTbl":U + goCntMaintDetail:ContainerCode
      oControl:RenderProcedure     = "customRenderProcedure":U
      oControl:RenderArgument      = "DisableFields":U.

    END. /*DO iControl = 1 TO NUM-ENTRIES(cControlList):*/

  DO iControl = 1 TO NUM-ENTRIES(cControlBuList):

    ASSIGN
      oControl                 = goCntMaintDetail:getControl(ENTRY(iControl, cControlBuList) + goCntMaintDetail:ContainerCode)
      oControl:RenderProcedure = "customRenderProcedure":U
      oControl:RenderArgument  = "DisableButtons":U.

  END. /*DO iControl = 1 TO NUM-ENTRIES(cControlBuList):*/

  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE &Container = goCntMaintDetail }

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerSearchFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerSearchFilter Procedure 
PROCEDURE defineContainerSearchFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
  /* SearchFilter */
  ASSIGN
    goCntSearchFilter                         = goWob:getContainer("SearchFilter":U) 
    
    goCntSearchFilter:ContainerTitle          = "Authorisation Group"                                    
    
    oControl                                  = goCntSearchFilter:addControl("fcAuthGroupCode":U           + goCntSearchFilter:ContainerCode  , "wsInput":U        , "20":U, "":U, "character":U, 1, 1, "Authorisation Group Code:":U)
    oControl                                  = goCntSearchFilter:addControl("fcDescription":U             + goCntSearchFilter:ContainerCode  , "wsTextArea":U     , "20,2":U, "":U, "character":U, 2, 1, "Description:":U)
                                             
    oControl                                  = goCntSearchFilter:addControl("fcInsurerArgumentMnemonic":U + goCntSearchFilter:ContainerCode  , "wsInput":U        , "5":U , "":U, "character":U, 3, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                                                                    
    oControl:ControlValue                     = "ermin":U                                                                                     
                                                                                                                                              
    oControl                                  = goCntSearchFilter:addControl("fcInsurerArgumentField":U    + goCntSearchFilter:ContainerCode  , "wsInput":U        , "5":U , "":U, "character":U, 3, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                           
    oControl:ControlValue                     = "[CodeField]":U                                      
                                           
    oControl                                  = goCntSearchFilter:addControl("fdInsurerObj":U              + goCntSearchFilter:ContainerCode  , "wsInput":U        , "1":U , "":U, "character":U, 3, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                                                                
    
    oControl                                  = goCntSearchFilter:addControl("fcInsurer":U                 + goCntSearchFilter:ContainerCode  , "wsInput":U        , "10":U, "":U, "character":U, 3, 2, "":U)
    oControl:ControlTooltip                   = "Please enter a valid client":U
    oControl:ajaxValidation                   = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields                     = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls                   = "fcInsurer":U + goCntSearchFilter:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntSearchFilter:ContainerCode + ",fcInsurerArgumentField":U + goCntSearchFilter:ContainerCode
    oControl:ReturnFields                     = "[RecordObj]":U
    oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchFilter:ContainerCode
  
    oControl                                  = goCntSearchFilter:addControl("buInsurerBtn":U              + goCntSearchFilter:ContainerCode  , "wsLookupButton":U , "":U  , "":U, "":U         , 3, 2, "Client:":U)
    oControl:LookupWobFLA                     = "ermin":U                                                                    
    oControl:LookupFields                     = "erm_insurer.insurer_code":U                                                 
    oControl:LookupControls                   = "fcInsurer":U + goCntSearchFilter:ContainerCode                                                               
    oControl:ReturnFields                     = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U                         
    oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchFilter:ContainerCode + ",fcInsurer":U + goCntSearchFilter:ContainerCode                                                   
    oControl:CellLayoutMask                   = "&1&2&3&4&5":U                                                             
                                                                                                                
    oControl                                  = goCntSearchFilter:addControl("fdEffectiveDate":U           + goCntSearchFilter:ContainerCode  , "wsInput":U        , "20":U, "":U, "date":U     , 3, 1, "Effective Date:":U)
        
    oControl                                  = goCntSearchFilter:addControl("fcOptionArgumentMnemonic":U  + goCntSearchFilter:ContainerCode  , "wsInput":U        , "5":U , "":U, "character":U, 4, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                         
    oControl:ControlValue                     = "scheme":U                                         
                                                                                                   
    oControl                                  = goCntSearchFilter:addControl("fcOptionArgumentField":U     + goCntSearchFilter:ContainerCode  , "wsInput":U        , "5":U , "":U, "character":U, 4, 2, "":U)
    oControl:ControlToken                     = "Hidden":U                                           
    oControl:ControlValue                     = "[CodeField]":U                                      
                                                                                                           
    oControl                                  = goCntSearchFilter:addControl("fiOptionCode":U              + goCntSearchFilter:ContainerCode  , "wsInput":U        , "10":U, "":U, "integer":U  , 4, 2, "":U)
    oControl:ControlFormat                    = "999":U          
    
    oControl:ajaxValidation                   = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields                     = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode + ",fcOptionArgumentMnemonic":U + goCntSearchFilter:ContainerCode + ",fcOptionArgumentField":U + goCntSearchFilter:ContainerCode
    
    oControl                                  = goCntSearchFilter:addControl("buOptionBtn":U               + goCntSearchFilter:ContainerCode  , "wsLookupButton":U , "":U  , "":U, "":U         , 4, 2, "Option:":U)
    oControl:LookupWobFLA                     = "scheme":U
    oControl:LookupFields                     = "scheme.scheme-code":U   
    oControl:LookupControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode                                                  
    oControl:FilterFields                     = "scheme.scheme-code":U                          
    oControl:FilterControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode                                                  
    oControl:ReturnFields                     = "scheme.scheme-code":U     
    oControl:ReturnControls                   = "fiOptionCode":U + goCntSearchFilter:ContainerCode                                                  
    oControl:CellLayoutMask                   = "&1&2&3&4":U   
                                             
    oControl                                  = goCntSearchFilter:addControl("fdEndDate":U                 + goCntSearchFilter:ContainerCode  , "wsInput":U        , "20":U, "":U, "date":U     , 4, 1, "End Date:":U)      
    .      
  
    RUN defineContainerSearchResults IN TARGET-PROCEDURE.
  
    { mip/inc/mipcatcherror.i }
  
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerSearchResults Procedure 
PROCEDURE defineContainerSearchResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
  /*Search Results*/
   ASSIGN                                                                                                                                                                                                                                                      
     goCntSearchResults                        = NEW cls.mipwscontainer("SearchResults":U + goWob:ObjectCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)                                                                                                        
     goCntSearchResults:ContainerMode          = goWob:SubmitValue                                                                                                                                                                                             
     goCntSearchResults:ShowGenericReportPrint = TRUE                                                                                                                                                                                                          
     goCntSearchResults:Collapsed              = FALSE                                                                                                                                                                                                         
     goCntSearchResults:ViewOnly               = NOT glEnquiryWob                                                                                                                                                                                              
     goCntSearchResults:RowsToRender           = ?                                                                                                                                                                                                             
     goCntSearchResults:RowRenderProcedure     = "rowRenderProcedure":U                                                                                                                                                                                        
     goCntSearchResults:RowRenderArgument      = goCntSearchResults:ContainerCode                                                                                                                                                                              
     goCntSearchResults:DefaultContainerType   = "TABLE":U                                                                                                                                                                                                     
     goCntSearchResults:ContainerTitle         = "Authorisation Group Results":U                                                                                                                                                                                                   
     goCntSearchResults:QueryString            = "FOR EACH tt_auth_group NO-LOCK":U                                                                                                                                                                       
                                               + "  ,FIRST tt_insurer NO-LOCK ":U                                                                                                                                                                              
                                               + "   WHERE tt_insurer.insurer_obj = tt_auth_group.insurer_obj OUTER-JOIN":U                                                                                                                               
                                               + "      BY tt_auth_group.line_number BY tt_auth_group.auth_group_code":U                                                                                                                        
                                                                                                                                                                                                                                                               
     lSuccess                                  = goWob:setContainer("SearchResults":U,  goCntSearchResults)                                                                                                                                                    
                                                                                                                                                                                                                                                               
     oControl                                  = goCntSearchResults:addControl("fdAuthGroupObj":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "20":U   , "tt_auth_group.auth_group_obj":U  , "character":U, 2, "":U)  
     oControl:ControlToken                     = "Hidden":U                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                              
     oControl                                  = goCntSearchResults:addControl("fcAuthGroupCode":U                + goCntSearchResults:ContainerCode  , "wsInput":U        , "10":U   , "tt_auth_group.auth_group_code":U , "character":U, 3, "Authorisation Group Code":U)  
     oControl:ControlClass                     = "+clMan":U                                                                                                                                                                                                                   
     oControl:ErrorMessage                     = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Authorisation group code'"}, "TEXT":U)                                                                                                                                    
     oControl:JavascriptOnBlur                 = "fnValidateField(this, true, ":U + QUOTER(oControl:ErrorMessage) + ", (this.value.length == 0));":U                                                                                                                          
  
     oControl                                  = goCntSearchResults:addControl("fcDescription":U             + goCntSearchResults:ContainerCode  , "wsTextArea":U     , "8,2":U  , "tt_auth_group.description":U          , "character":U, 4, "Description":U)  
     oControl:ErrorMessage                     = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Authorisation group description'"}, "TEXT":U)                                                                                                                    
     oControl:ControlClass                     = "+clMan":U                                                                                                                                                                                                          
     oControl:JavascriptOnBlur                 = "fnValidateField(this, true, ~"A valid authorisation group description must be entered~", (this.value.length==0));":U                                                                                                        
                                                                                                                                                                                                                                                                     
     oControl                                  = goCntSearchResults:addControl("fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode  , "wsInput":U       , "5":U     , "":U                                        , "character":U, 5, "":U)        
     oControl:ControlToken                     = "Hidden":U                                                                                                                                                                                                     
     oControl:ControlValue                     = "ermin":U                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                
     oControl                                  = goCntSearchResults:addControl("fcInsurerArgumentField":U    + goCntSearchResults:ContainerCode  , "wsInput":U       , "5":U     , "":U                                        , "character":U, 5, "":U)        
     oControl:ControlToken                     = "Hidden":U                                                                                                                                                                                                     
     oControl:ControlValue                     = "[CodeField]":U                                                                                                                                                                                                
                                                                                                                                                                                                                                                                
     oControl                                  = goCntSearchResults:addControl("fdInsurerObj":U              + goCntSearchResults:ContainerCode  , "wsInput":U        , "1":U    , "tt_auth_group.insurer_obj":U          , "character":U, 5, "":U)        
     oControl:ControlToken                     = "Hidden":U                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                
     oControl                                  = goCntSearchResults:addControl("fcInsurer":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "8":U    , "tt_insurer.insurer_code":U                 , "character":U, 5, "":U)        
     oControl:ControlTooltip                   = "Please enter a valid client":U                                                                                                                                                                                
     oControl:ajaxValidation                   = "SERVICE:wsUIService:ajaxValidation:":U                                                                                                                                                                        
     oControl:FilterFields                     = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U                                                                                                                                                       
     oControl:FilterControls                   = "fcInsurer":U + goCntSearchResults:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode + ",fcInsurerArgumentField":U + goCntSearchResults:ContainerCode                          
     oControl:ReturnFields                     = "[RecordObj]":U                                                                                                                                                                                                
     oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchResults:ContainerCode                                                                                                                                                            
                                                                                                                                                                                                                                                                
     oControl                                  = goCntSearchResults:addControl("buInsurerBtn":U              + goCntSearchResults:ContainerCode  , "wsLookupButton":U , "":U     , "":U                                        , "":U         , 5, "Client":U)  
     oControl:LookupWobFLA                     = "ermin":U                                                                                                                                                                                                      
     oControl:LookupFields                     = "erm_insurer.insurer_code":U                                                                                                                                                                                   
     oControl:LookupControls                   = "fcInsurer":U + goCntSearchResults:ContainerCode                                                                                                                                                               
     oControl:ReturnFields                     = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U                                                                                                                                                           
     oControl:ReturnControls                   = "fdInsurerObj":U + goCntSearchResults:ContainerCode + ",fcInsurer":U + goCntSearchResults:ContainerCode                                                                                                        
     oControl:CellLayoutMask                   = "&1&2&3&4&5":U                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                     
     oControl                                  = goCntSearchResults:addControl("fiOptionCode":U              + goCntSearchResults:ContainerCode  , "wsCombo":U        , "5":U    , "tt_auth_group.option_code":U          , "character":U  , 6, "Option":U)       
     oControl:AdditionalItems                  = "000 - All =0"                                                                                                                                                                                                        
     oControl:QueryString                      = "FOR EACH scheme NO-LOCK WHERE scheme.active = yes":U                                                                                                                                                               
     oControl:KeyField                         = "scheme.scheme-code":U                                                                                                                                                                                              
     oControl:DisplayFields                    = "scheme.scheme-code,scheme.short-name":U                                                                                                                                                                            
     oControl:DisplayMask                      = "&1 - &2"                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                     
     oControl                                  = goCntSearchResults:addControl("fdEffectiveDate":U           + goCntSearchResults:ContainerCode , "wsInput":U        , "10":U    , "tt_auth_group.effective_date":U       , "date":U     ,7, "Effective Date":U)
     oControl:ControlClass                     = "+clMan":U                                                                                                                                                                                                          
     oControl:ErrorMessage                     = mipEnv:formatMessage({mip/inc/miperrortext.i 'ma' 355 ? ? "'Effective date'"}, "TEXT":U)                                                                                                                            
                                                                                                                                                                                                                                                                     
     oControl                                  = goCntSearchResults:addControl("fdEndDate":U                 + goCntSearchResults:ContainerCode , "wsInput":U        , "10":U    , "tt_auth_group.end_date":U             ,  "date":U     ,8, "End Date":U)  
     .  
    
  
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE &Container = goCntSearchResults }
  
  
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dependencyCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dependencyCheck Procedure 
PROCEDURE dependencyCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipoContainer         AS cls.mipwscontainer NO-UNDO.
  DEFINE OUTPUT PARAMETER oplDependencyExists  AS LOGICAL            NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDependencyMessage AS CHARACTER          NO-UNDO.

  DEFINE VARIABLE dAuthGroupObj AS DECIMAL     NO-UNDO.
  
  
  IF VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) THEN
  DO:
    CASE ipoContainer:ContainerCode:
    
      WHEN "SearchResults":U + goWob:ObjectCode THEN
      DO:
        ASSIGN 
           dAuthGroupObj   = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_group.auth_group_obj":U, "BUFFER-VALUE":U))
           
           oplDependencyExists = CAN-FIND(FIRST tt_auth_group_detail NO-LOCK
                                          WHERE tt_auth_group_detail.auth_group_obj = dAuthGroupObj).
                                                                                    
        IF oplDependencyExists
        THEN       
          ASSIGN opcDependencyMessage = mipEnv:formatMessage({mip/inc/miperrortext.i 'MA' 370 ? ? "'Authorisation group'" "'auth group detail'"}, "TEXT":U).                                  
                                                  
      END. /*WHEN "SearchResults":U + goWob:ObjectCode THEN*/
    END CASE.
  END. /*IF VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) THEN*/

  { mip/inc/mipcatcherror.i }
  
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

  RUN SUPER.
  
  {&OUT}
  
    {ws/inc/wsjavascriptopentag.i}
     
    "~n  var gvControlChanged = ~"~";":U
    
    "~n  function fnCheckRowState(pControl, pContainerCode)~{":U
    "~n    var iRow          = parseInt(pControl.name.replace(/[^0-9\.]/g, ~"~"), 10),                 ":U 
    "~n        oMnemonic     = fnGetControls(~"cbOwningEntityMnemonicTbl~" + pContainerCode + iRow)[0],":U 
    "~n        oCodefield    = fnGetControls(~"fcOwningEntityCodeTbl~"     + pContainerCode + iRow)[0],":U 
    "~n        oCodelookupBtn= fnGetControls(~"buOwningEntityBtnTbl~"      + pContainerCode + iRow)[0],":U
    "~n        oDisc         = fnGetControls(~"fiDisciplineTbl~"           + pContainerCode + iRow)[0],":U
    "~n        oDflt         = fnGetControls(~"chDetailDefaultTbl~"        + pContainerCode + iRow)[0],":U
    "~n        oDiscBtn      = fnGetControls(~"buDisciplineBtnTbl~"        + pContainerCode + iRow)[0],":U 
    "~n        oSubDisc      = fnGetControls(~"fiSubDisciplineTbl~"        + pContainerCode + iRow)[0],":U
    "~n        oSubDiscBtn   = fnGetControls(~"buSubDisciplineBtnTbl~"     + pContainerCode + iRow)[0],":U
    "~n        oBaseRate     = fnGetControls(~"fcBaseRateTbl~"             + pContainerCode + iRow)[0],":U
    "~n        oBaseRateBtn  = fnGetControls(~"buBaseRateBtnTbl~"          + pContainerCode + iRow)[0],":U 
    "~n        oArsRate      = fnGetControls(~"fcArsRateTbl~"              + pContainerCode + iRow)[0],":U
    "~n        oArsRateBtn   = fnGetControls(~"buArsRateBtnTbl~"           + pContainerCode + iRow)[0];":U
    "~n                                               ":U
    "~n    if (oMnemonic.value ==~"~")~{              ":U
    "~n      oCodefield.value = ~"~",                 ":U
    "~n      oCodefield.disabled = true,              ":U
    "~n      oCodelookupBtn.disabled = true;~}        ":U
    "~n    else ~{                                    ":U
    "~n      oCodefield.disabled = false,             ":U
    "~n      oCodelookupBtn.disabled = false;~}       ":U
    "~n    console.log(oMnemonic.value.toLowerCase());":U

    "~n    if(oMnemonic.value.toLowerCase()==~"htmtl~"||oMnemonic.value.toLowerCase()==~"tariff~")~{   ":U
    "~n    console.log('pechi 2 ');        ":U
    "~n      oDisc.disabled=false;         ":U
    "~n      oDiscBtn.disabled=false;      ":U
    "~n      oDflt.disabled=false;         ":U
    "~n      oSubDisc.disabled=false;      ":U
    "~n      oSubDiscBtn.disabled=false;   ":U
    "~n      oBaseRate.disabled=false;     ":U
    "~n      oBaseRateBtn.disabled=false;  ":U
    "~n      oArsRate.disabled=false;      ":U
    "~n      oArsRateBtn.disabled=false;   ":U     
    "~n      oDisc.value=~"~";             ":U
    "~n      oSubDisc.value=~"~";          ":U
    "~n      oBaseRate.value=~"~";         ":U
    "~n      oArsRate.value=~"~";          ":U     
    "~n    ~}else if(oMnemonic.value.toLowerCase()==~"prtype~"||oMnemonic.value.toLowerCase()==~"discipline~")~{ ":U
    "~n      oCodefield.disabled=true;     ":U
    "~n      oCodelookupBtn.disabled=true; ":U
    "~n      oDisc.disabled=false;         ":U
    "~n      oDiscBtn.disabled=false;      ":U
    "~n      oDflt.disabled=true;          ":U
    "~n      oSubDisc.disabled=false;      ":U
    "~n      oSubDiscBtn.disabled=false;   ":U
    "~n      oBaseRate.disabled=true;      ":U
    "~n      oBaseRateBtn.disabled=true;   ":U
    "~n      oArsRate.disabled=true;       ":U
    "~n      oArsRateBtn.disabled=true;    ":U     
    "~n      oDisc.value=~"~";             ":U
    "~n      oSubDisc.value=~"~";          ":U
    "~n      oBaseRate.value=~"~";         ":U
    "~n      oArsRate.value=~"~";          ":U  
    "~n      oCodefield.value=~"~";        ":U
    "~n    ~}else~{                        ":U
    "~n      oDisc.disabled=true;          ":U
    "~n      oDiscBtn.disabled=true;       ":U
    "~n      oDflt.disabled=true;          ":U
    "~n      oSubDisc.disabled=true;       ":U
    "~n      oSubDiscBtn.disabled=true;    ":U
    "~n      oBaseRate.disabled=true;      ":U
    "~n      oBaseRateBtn.disabled=true;   ":U
    "~n      oArsRate.disabled=true;       ":U
    "~n      oArsRateBtn.disabled=true;    ":U
    "~n      oDisc.value=~"~";             ":U
    "~n      oSubDisc.value=~"~";          ":U
    "~n      oBaseRate.value=~"~";         ":U
    "~n      oArsRate.value=~"~";          ":U
    "~n    ~}                              ":U           
    "~n ~}                                 ":U   
    
    "~n  function fnSetDefaults(pControl,cContainerCode)~{                                             ":U
    "~n    fnSetChanged(pControl);                                                                     ":U
    "~n    var iRow          = parseInt(pControl.name.replace(/[^0-9\.]/g, ~"~"), 10);                 ":U 
    "~n    if(gvControlChanged==~"chDetailDefaultTbl~"+cContainerCode+iRow)~{                          ":U
    "~n        oDisc         = fnGetControls(~"fiDisciplineTbl~"           + cContainerCode + iRow)[0],":U
    "~n        oDflt         = fnGetControls(~"chDetailDefaultTbl~"        + cContainerCode + iRow)[0],":U
    "~n        oDiscBtn      = fnGetControls(~"buDisciplineBtnTbl~"        + cContainerCode + iRow)[0],":U
    "~n        oSubDisc      = fnGetControls(~"fiSubDisciplineTbl~"        + cContainerCode + iRow)[0],":U
    "~n        oSubDiscBtn   = fnGetControls(~"buSubDisciplineBtnTbl~"     + cContainerCode + iRow)[0],":U
    "~n        oBaseRate     = fnGetControls(~"fcBaseRateTbl~"             + cContainerCode + iRow)[0],":U
    "~n        oBaseRateBtn  = fnGetControls(~"buBaseRateBtnTbl~"          + cContainerCode + iRow)[0],":U
    "~n        oArsRate      = fnGetControls(~"fcArsRateTbl~"              + cContainerCode + iRow)[0],":U
    "~n        oArsRateBtn   = fnGetControls(~"buArsRateBtnTbl~"           + cContainerCode + iRow)[0];":U
    "~n                                                                                                ":U
    "~n        if (oDflt.checked )~{            ":U
    "~n          oDflt.disabled = false;        ":U
    "~n          oDisc.disabled=true;           ":U
    "~n          oDiscBtn.disabled=true;        ":U
    "~n          oSubDisc.disabled=true;        ":U
    "~n          oSubDiscBtn.disabled=true;     ":U
    "~n          oBaseRate.disabled=true;       ":U
    "~n          oBaseRateBtn.disabled=true;    ":U
    "~n          oArsRate.disabled=true;        ":U
    "~n          oArsRateBtn.disabled=true;     ":U
    "~n          oSubDisc.value=~"~";           ":U
    "~n          oBaseRate.value=~"~";          ":U
    "~n          oArsRate.value=~"~";           ":U
    "~n        ~} else ~{                       ":U
    "~n          oDflt.disabled = false;        ":U
    "~n          oDisc.disabled=false;          ":U
    "~n          oDiscBtn.disabled=false;       ":U
    "~n          oSubDisc.disabled=false;       ":U
    "~n          oSubDiscBtn.disabled=false;    ":U
    "~n          oBaseRate.disabled=false;      ":U
    "~n          oBaseRateBtn.disabled=false;   ":U
    "~n          oArsRate.disabled=false;       ":U
    "~n          oArsRateBtn.disabled=false;    ":U
    "~n          oDisc.value=~"~";              ":U
    "~n          oSubDisc.value=~"~";           ":U
    "~n          oBaseRate.value=~"~";          ":U
    "~n          oArsRate.value=~"~";           ":U
    "~n         ~}                              ":U
    "~n  ~}                                     ":U
    "~n ~}                                      ":U
    "~n  function fnSetChanged(pControl)~{      ":U
    "~n    gvControlChanged = pControl.name;    ":U
    "~n ~}                                      ":U  
    "~n function pad (str, max) ~{":U
    "~n return str.length < max ? pad(~"0~" + str, max) : str;":U
    "~n ~}":U

    {ws/inc/wsjavascriptclosetag.i}
    .
     
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-populateComboLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateComboLists Procedure 
PROCEDURE populateComboLists :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cCategoryKey     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKeylist         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCodelist        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLabellist       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDescriptionList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSequenceList    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValueList       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iTable           AS INTEGER   NO-UNDO.  
  
  
  ASSIGN   
    gcAllTableList = "":U. 
  
    cCategoryKey   = "ma_acAuthGroupDetail":U.
  
    
  mipEnv:miUtility:getStatusOrAcronymDetails(INPUT  "Acronym":U, 
                                             INPUT  cCategoryKey,
                                             OUTPUT cKeylist,
                                             OUTPUT cCodelist,
                                             OUTPUT cLabellist,
                                             OUTPUT cDescriptionList,
                                             OUTPUT cSequenceList,
                                             OUTPUT cValueList).

  tbl-blk:                     
  DO iTable = 1 TO NUM-ENTRIES(cValueList,{&Delim-ValueList}):
  
    ASSIGN gcAllTableList = gcAllTableList
                          + (IF INDEX(gcAllTableList,ENTRY(iTable,cValueList,{&Delim-ValueList})) = 0 
                             THEN 
                               (IF gcAllTableList = "":U THEN "":U ELSE "|":U) 
                                 + ENTRY(iTable,cLabelList,{&Delim-ValueList}) + "=":U + ENTRY(iTable,cValueList,{&Delim-ValueList})
                             ELSE
                               "":U).
                            
  END. /*DO iTable = 1 TO NUM-ENTRIES(cTempList):*/
  
  ASSIGN gcAllTableList = "|":U + gcAllTableList.
         gcAllTableList = mipEnv:miExpression:sortList(gcAllTableList,"|":U).
       
  { mip/inc/mipcatcherror.i }
    
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
  
  DEFINE VARIABLE cRecords              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cMode                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cContainerCode        AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE iRecords              AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iRowsRendered         AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iDeleted              AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iOptionCode           AS INTEGER                   NO-UNDO.   
  DEFINE VARIABLE lSuccess              AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lEnquiryMode          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cAuthGroupCode        AS CHARACTER                 NO-UNDO. 
  DEFINE VARIABLE cAuthGroupDescription AS CHARACTER                 NO-UNDO. 
  DEFINE VARIABLE dInsurerObj           AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dEffectiveDate        AS DATE                      NO-UNDO.
  DEFINE VARIABLE dEndDate              AS DATE                      NO-UNDO. 
  DEFINE VARIABLE oControl              AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oSearch               AS cls.maauthgroupsearch NO-UNDO.
  
  
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
                    
  ASSIGN oSearch      = NEW cls.maauthgroupsearch(INPUT DATASET dsAuthGroup BY-REFERENCE)
  
         lEnquiryMode = get-value("WobMode":U) = "enquiry":U.
  
  CASE goWob:Mode:
  
    WHEN "Search":U THEN
    DO: 
      ASSIGN 
         cRecords              =         goCntSearchFilter:getControl("fiRecords":U       + goCntSearchFilter:ContainerCode):ControlValue
         cMode                 =         goCntSearchFilter:getControl("fcSearchMode":U    + goCntSearchFilter:ContainerCode):ControlValue
                                    
         cAuthGroupCode        =         goCntSearchFilter:getControl("fcAuthGroupCode":U + goCntSearchFilter:ContainerCode):ControlValue
         cAuthGroupDescription =         goCntSearchFilter:getControl("fcDescription":U   + goCntSearchFilter:ContainerCode):ControlValue
         dInsurerObj           = DECIMAL(goCntSearchFilter:getControl("fdInsurerObj":U    + goCntSearchFilter:ContainerCode):ControlValue)
         iOptionCode           = INTEGER(goCntSearchFilter:getControl("fiOptionCode":U    + goCntSearchFilter:ContainerCode):ControlValue)
         dEffectiveDate        =    DATE(goCntSearchFilter:getControl("fdEffectiveDate":U + goCntSearchFilter:ContainerCode):ControlValue)
         dEndDate              =    DATE(goCntSearchFilter:getControl("fdEndDate":U       + goCntSearchFilter:ContainerCode):ControlValue).
         
         
      IF NOT Warpspeed:ValidationError THEN
      DO:
        DATASET dsAuthGroup:EMPTY-DATASET.
        
        ASSIGN     
          lSuccess  = (IF cAuthGroupCode <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_group.auth_group_code":U, cMode, cAuthGroupCode) ELSE TRUE)
                       
          lSuccess  = (IF cAuthGroupDescription <> "":U 
                       THEN oSearch:SetFilterCriteria("tt_auth_group.description":U, cMode, cAuthGroupDescription) ELSE TRUE)             
                       
          lSuccess  = (IF dInsurerObj <> 0.00 
                       THEN oSearch:SetFilterCriteria("tt_auth_group.insurer_obj":U, "=":U, dInsurerObj) ELSE TRUE)                          
                       
          lSuccess  = (IF iOptionCode <> 0
                       THEN oSearch:SetFilterCriteria("tt_auth_group.option_code":U, "=":U, iOptionCode) ELSE TRUE)                                       
                       
          lSuccess  = (IF dEffectiveDate <> ?
                       THEN oSearch:SetFilterCriteria("tt_auth_group.effective_date":U, ">=":U, dEffectiveDate) ELSE TRUE)                                       
                       
          lSuccess  = (IF dEndDate <> ?
                       THEN oSearch:SetFilterCriteria("tt_auth_group.end_date":U, "<=":U, dEndDate) ELSE TRUE)                                                    
                    
          lSuccess  = oSearch:setCriteria("BatchSize":U,  cRecords)
          lSuccess  = oSearch:setCriteria("Query-Sort":U, "tt_auth_group.auth_group_code":U)
          
          lSuccess  = oSearch:fetchData().     
        
      END. /*IF NOT Warpspeed:ValidationError THEN*/
      
      IF NOT lEnquiryMode
      THEN
        ASSIGN lSuccess = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_group:DEFAULT-BUFFER-HANDLE). 
      
      /* Get the number of records received from the fetch data */
      FOR EACH tt_auth_group NO-LOCK: 
        ASSIGN iRowsRendered = iRowsRendered + 1. 
      END. /*FOR EACH tt_auth_rule NO-LOCK: */
      
      FOR EACH tt_deleted NO-LOCK:
        ASSIGN iDeleted = iDeleted + 1.
      END. /*FOR EACH tt_deleted NO-LOCK:*/
      
      
      /* Set the container title with the number of records */
      ASSIGN
        goCntSearchResults:ViewOnly        = FALSE
        
        cContainerCode                     = goCntSearchResults:ContainerCode                                
  
        oControl                           = goCntSearchResults:getControl("fcRecordAction":U + cContainerCode)
        oControl:ControlQueryField         = "tt_auth_group.record_action":U
        
        oControl                           = goCntSearchResults:getControl("buEdit":U + cContainerCode)
        oControl:ControlToken              = "Updatable":U
        
        oControl                           = goCntSearchFilter:getControl("fcInsurerArgumentMnemonic":U  + goCntSearchFilter:ContainerCode)
        oControl:ControlValue              = "ermin":U                                                                                     
                                                                                                                                         
        oControl                           = goCntSearchFilter:getControl("fcInsurerArgumentField":U     + goCntSearchFilter:ContainerCode)
        oControl:ControlValue              = "[CodeField]":U                                      
        
        oControl                           = goCntSearchResults:getControl("fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode)
        oControl:ControlValue              = "ermin":U

        oControl                           = goCntSearchResults:getControl("fcInsurerArgumentField":U    + goCntSearchResults:ContainerCode)
        oControl:ControlValue              = "[CodeField]":U
        
        goCntSearchResults:QueryBufferList = STRING(TEMP-TABLE tt_auth_group:DEFAULT-BUFFER-HANDLE) + ",":U
                                           + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE)
        
        lSuccess                           = goCntSearchResults:PopulateFromQuery()
        
        goCntSearchResults:ContainerTitle  = goCntSearchResults:ContainerTitle + ": ":U 
                                           + (IF Warpspeed:ValidationError 
                                              THEN "There has been an error submitting your page":U
                                              ELSE
                                               ( IF iRowsRendered > INTEGER(cRecords)
                                                 THEN "Please refine your search criteria as it resulted in more than ":U + cRecords + " records...":U
                                                 ELSE STRING(iRowsRendered - (IF glEnquiryWob THEN 0 ELSE 1)) + " record/s found":U 
                                                        + (IF Warpspeed:ValidationError AND iDeleted > 0 THEN " ( " + STRING(iDeleted) + " record/s deleted )":U ELSE "":U))). 
    END. /*WHEN "Search":U THEN*/
    
    WHEN "Maint":U THEN
    DO:      
      IF NOT Warpspeed:ValidationError THEN
      DO:
        DATASET dsAuthGroup:EMPTY-DATASET.
        
        IF goWob:CurrentObj <> "":U
        THEN
          ASSIGN 
            lSuccess = oSearch:SetFilterCriteria("tt_auth_group.auth_group_obj":U, "=":U, DECIMAL(goWob:CurrentObj))                              
                      
            lSuccess = oSearch:fetchData().              
      END. /*IF NOT Warpspeed:ValidationError THEN*/
      
      IF NOT lEnquiryMode THEN
      DO:
        ASSIGN lSuccess = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_group_detail:DEFAULT-BUFFER-HANDLE). 
      
        IF goWob:CurrentObj <> "":U THEN
        DO:
          FOR EACH tt_auth_group_detail EXCLUSIVE-LOCK
             WHERE tt_auth_group_detail.auth_group_obj = 0.00:
          
            ASSIGN tt_auth_group_detail.auth_group_obj = DECIMAL(goWob:CurrentObj).
            
            VALIDATE tt_auth_group_detail.
          END. /*FOR EACH tt_auth_group_detail:*/
        END. /*IF goWob:CurrentObj <> "":U THEN*/
      END. /*IF NOT lEnquiryMode THEN*/
      
     
      mipEnv:Health:maUtility:getAuditRecordTT(
          INPUT "ham_auth_group":U,             
          INPUT goWob:CurrentObj,                                                                              
          INPUT "":U,                                                                      
          INPUT "":U,                                                                                              
          OUTPUT TABLE ttAuditRecord).
           
      ASSIGN                                            
         goCntMaintDetail:ViewOnly        = FALSE
      
         oControl                         = goCntMaint:getControl("fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode)
         oControl:ControlValue            = "ermin":U                                                                                     
                                                                                                                                        
         oControl                         = goCntMaint:getControl("fcInsurerArgumentField":U    + goCntMaint:ContainerCode)
         oControl:ControlValue            = "[CodeField]":U                        
         
         oControl                         = goCntMaintDetail:getControl("fcOwningFieldArgumentTbl":U + goCntMaintDetail:ContainerCode)
         oControl:ControlValue            = "[CodeField]":U                        
                                          
         goCntMaint:QueryString           = SUBSTITUTE(goCntMaint:QueryString, goWob:CurrentObj)
                                          
         goCntMaint:QueryBufferList       = STRING(TEMP-TABLE tt_auth_group:DEFAULT-BUFFER-HANDLE)
                                          
         lSuccess                         = goCntMaint:PopulateFromQuery()
         
         goCntMaintDetail:QueryString     = SUBSTITUTE(goCntMaintDetail:QueryString, goWob:CurrentObj)                                               
                                                        
         goCntMaintDetail:QueryBufferList = STRING(TEMP-TABLE tt_auth_group_detail:DEFAULT-BUFFER-HANDLE)
                                           
         lSuccess                         = goCntMaintDetail:PopulateFromQuery()
         
         goCntAudit:QueryBufferList       = STRING(TEMP-TABLE ttAuditRecord:DEFAULT-BUFFER-HANDLE)
                                          
         lSuccess                         = goCntAudit:PopulateFromQuery().                
      
    END. /*WHEN "Maint":U THEN*/
   
  END CASE. /*CASE goWob:Mode:*/

  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}

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
  
  DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
 
  RUN SUPER (INPUT ipoContainer).
  
  CASE ipoContainer:RowRenderArgument:
  
    WHEN "SearchResultsmahamag" THEN
    DO:
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_group_error:HANDLE,
         INPUT ipoContainer,
         INPUT "hamag":U,
         INPUT DECIMAL(ipoContainer:getControl("fdAuthGroupObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).
         
    END. /* WHEN ipoContainer:ContainerCode */
    
    WHEN "MaintDetailmahamag" THEN
    DO:
      
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_group_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hamad":U, 
         INPUT DECIMAL(ipoContainer:getControl("fdGroupDetailObjTbl":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).  
    
    END. /* WHEN "MaintDetailmahamag" THEN */ 
  
  END CASE.
  
  { mip/inc/mipcatcherror.i }

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
  
  /*All registered containers are deleted in the base wob*/
  
  { mip/inc/mipcatcherror.i } 
  
END PROCEDURE.  /* shutdown */

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

