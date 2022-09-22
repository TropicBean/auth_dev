&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
" This code is based on the cgi-wrapper template as designed by Progress.

  MIP Holdings (Pty) Ltd.

  Use this template to create a new Custom CGI Wrapper Procedure and write WebSpeed code that dynamically generates HTML. No associated static HTML file is needed."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------------
  Filename    : ma/app/maauthcopaycontrolwobsuper.p
  Purpose     : Authorisation Copay Control Wob Super
  Description : Authorisation Copay Control  Wob Super
  Author      : MandlaM
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

/* This helps to ensure proper clean-up */
CREATE WIDGET-POOL.

/* WarpSpeed's Shared Definitions */
{ mip/inc/mipdefshared.i }

{ sysadmma.i }

{ ma/inc/maaudittt.i           }
{ ma/inc/mainsurertt.i         }
{ ma/inc/maschemeds.i          }

&IF {&DBDFMA} >= 010195 &THEN
{ ma/inc/maauthcopayds.i }
&ENDIF

{ mip/inc/miptemptables.i &TempTableName = ttValidation }
{ mip/inc/miptemptables.i &TempTableName = ttEntityList }

{ ma/inc/maerrortt.i &TEMP-TABLE-NAME = tt_warning }


DEFINE TEMP-TABLE tt_deleted
  FIELD owning_obj           AS DECIMAL
  FIELD owning_key           AS CHARACTER.    

/* Variables commonly used by WarpSpeed */
DEFINE VARIABLE goWob                       AS cls.mipwswob         NO-UNDO.
DEFINE VARIABLE goWobType                   AS cls.mipacronym       NO-UNDO.
DEFINE VARIABLE goErrorObject               AS cls.maerrorobject    NO-UNDO.
                                            
/* Variables for this specific WOB */       
DEFINE VARIABLE gcSearchMethod              AS CHARACTER            NO-UNDO.
DEFINE VARIABLE glEnquiryMode               AS LOGICAL              NO-UNDO.
DEFINE VARIABLE glSuccess                   AS LOGICAL              NO-UNDO.

DEFINE VARIABLE glCopyScreen                AS LOGICAL              NO-UNDO.
                                            
/* Containers */                            
DEFINE VARIABLE goCntSearchFilter           AS cls.mipwscontainer   NO-UNDO.
DEFINE VARIABLE goCntSearchResults          AS cls.mipwscontainer   NO-UNDO.
DEFINE VARIABLE goCntMaint                  AS cls.mipwscontainer   NO-UNDO.
DEFINE VARIABLE goCntCopayDetailsFilter     AS cls.mipwscontainer   NO-UNDO.
DEFINE VARIABLE goCntCopayDetails           AS cls.mipwscontainer   NO-UNDO.
DEFINE VARIABLE goCntAudit                  AS cls.mipwscontainer   NO-UNDO.
DEFINE VARIABLE goCntButtons                AS cls.mipwscontainer   NO-UNDO.
DEFINE VARIABLE goCntDetailsCopy            AS cls.mipwscontainer   NO-UNDO.


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
         HEIGHT             = 28.43
         WIDTH              = 76.8.
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

&IF DEFINED(EXCLUDE-ajaxSaveCopayDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveCopayDetails Procedure 
PROCEDURE ajaxSaveCopayDetails :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE oAuthCopayDetail    AS cls.maauthcopaydetail      NO-UNDO.
  DEFINE VARIABLE oRequestHelper      AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper     AS cls.maajaxresponsehelper   NO-UNDO.  
  DEFINE VARIABLE hErrorHandle        AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE iLineNumber         AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE cAction             AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cContainerCode      AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cEffectiveDate      AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cEndDate            AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE dObj                AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dEffectiveDate      AS DATE                       NO-UNDO.
  DEFINE VARIABLE dEndDate            AS DATE                       NO-UNDO.

  ASSIGN
    cContainerCode      = ipcValidationArgument
                        
    oRequestHelper      = NEW cls.maajaxrequesthelper(INPUT get-value('FldLst'), INPUT get-value('ValList'))
    oResponseHelper     = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
    oAuthCopayDetail    = NEW cls.maauthcopaydetail()
   
    dObj                = DECIMAL(oRequestHelper:getFieldValue("fdObj":U           + cContainerCode))
                        
    iLineNumber         = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U    + cContainerCode)) 
                        
    cAction             =         oRequestHelper:getFieldValue("fcAction":U        + cContainerCode)                   
    cEffectiveDate      =         oRequestHelper:getFieldValue("fdEffectiveDate":U + cContainerCode)
    cEndDate            =         oRequestHelper:getFieldValue("fdEndDate":U       + cContainerCode)
                        
    dEffectiveDate      = (IF cEffectiveDate = "YYYY/MM/DD":U THEN TODAY ELSE DATE(cEffectiveDate))                   
    dEndDate            = (IF cEndDate       = "YYYY/MM/DD":U THEN ?     ELSE DATE(cEndDate))
    NO-ERROR.


  IF NOT {&ErrorStatus} THEN
  DO:
    
    CASE cAction:
      WHEN "modify":U THEN
      DO:
        
        oAuthCopayDetail:focusAuthCopayDetail(dObj) NO-ERROR.

        IF NOT {&ErrorStatus} THEN
        DO:
          ASSIGN            
            oAuthCopayDetail:AuthCopayControlObj         = DECIMAL(oRequestHelper:getFieldValue("fdCopayControlObjArgument":U       + cContainerCode))
            oAuthCopayDetail:OwningEntityMnemonic        =         oRequestHelper:getFieldValue("fcOwningEntityMnemonic":U          + cContainerCode)
            oAuthCopayDetail:OwningObj                   = DECIMAL(oRequestHelper:getFieldValue("fdOwningObj":U                     + cContainerCode))
            oAuthCopayDetail:OwningKey                   =         oRequestHelper:getFieldValue("fcOwningKey":U                     + cContainerCode)
            oAuthCopayDetail:OwningAltValue              =         oRequestHelper:getFieldValue("fcOwningCode":U                    + cContainerCode)
            oAuthCopayDetail:ExclusionEntityMnemonic     =         oRequestHelper:getFieldValue("fcExclusionEntityMnemonic":U + cContainerCode)
            oAuthCopayDetail:ExclusionObj                = DECIMAL(oRequestHelper:getFieldValue("fdExclusionObj":U            + cContainerCode))
            oAuthCopayDetail:ExclusionKey                =         oRequestHelper:getFieldValue("fcExclusionKey":U            + cContainerCode)
            oAuthCopayDetail:ExclusionAltValue           =         oRequestHelper:getFieldValue("fcExclusionCode":U           + cContainerCode)
            oAuthCopayDetail:EffectiveDate               = dEffectiveDate
            oAuthCopayDetail:EndDate                     = dEndDate      
            
            lSuccess                                      = oAuthCopayDetail:saveAuthCopayDetail() 
           NO-ERROR.

          IF NOT {&ErrorStatus} AND NOT oAuthCopayDetail:ErrorObject:ErrorsExist 
          THEN
            ASSIGN 
              oResponseHelper:RequestValid    = TRUE
              oResponseHelper:ResponseMessage = "Record successfully saved":U 
              lSuccess                        = oResponseHelper:addFieldValue("fdObj":U + cContainerCode, STRING(oAuthCopayDetail:AuthCopayDetailObj), TRUE)
             NO-ERROR.
        END. /*IF NOT {&ErrorStatus} THEN*/    
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN
      DO:
        oAuthCopayDetail:focusAuthCopayDetail(dObj) NO-ERROR.

        
        IF NOT {&ErrorStatus} AND NOT oAuthCopayDetail:AuthCopayDetailInFocus 
        THEN  
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U 
            NO-ERROR.
        ELSE 
          ASSIGN lSuccess = oAuthCopayDetail:removeAuthCopayDetail() NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthCopayDetail:ErrorObject:ErrorsExist
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
  
  IF oAuthCopayDetail:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN
  DO:
    ASSIGN 
      oResponseHelper:RequestValid    = FALSE
      
      hErrorHandle                    = oAuthCopayDetail:ErrorObject:getErrorTableHandle()
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
      lSuccess                        = oResponseHelper:setError(hErrorHandle)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action':U 
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
  
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
  END.   /*IF oAuthCopayDetail:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
  
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)     THEN DELETE OBJECT oRequestHelper     NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper)    THEN DELETE OBJECT oResponseHelper    NO-ERROR. ~{mip/inc/mipmessageerror.i~} 
                                        IF VALID-OBJECT(oAuthCopayDetail)   THEN DELETE OBJECT oAuthCopayDetail   NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxSaveSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxSaveSearchResults Procedure 
PROCEDURE ajaxSaveSearchResults :
/*------------------------------------------------------------------------------
  Purpose   :
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE oAuthCopayControl   AS cls.maauthcopaycontrol     NO-UNDO.
  DEFINE VARIABLE oRequestHelper      AS cls.maajaxrequesthelper    NO-UNDO.
  DEFINE VARIABLE oResponseHelper     AS cls.maajaxresponsehelper   NO-UNDO.  
  DEFINE VARIABLE hErrorHandle        AS HANDLE                     NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL                    NO-UNDO.
  DEFINE VARIABLE iLineNumber         AS INTEGER                    NO-UNDO.
  DEFINE VARIABLE cAction             AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cContainerCode      AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cEffectiveDate      AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE cEndDate            AS CHARACTER                  NO-UNDO.
  DEFINE VARIABLE dObj                AS DECIMAL                    NO-UNDO.
  DEFINE VARIABLE dEffectiveDate      AS DATE                       NO-UNDO.
  DEFINE VARIABLE dEndDate            AS DATE                       NO-UNDO.

  ASSIGN
    cContainerCode      = ipcValidationArgument
                        
    oRequestHelper      = NEW cls.maajaxrequesthelper(INPUT get-value('FldLst'), INPUT get-value('ValList'))
    oResponseHelper     = NEW cls.maajaxresponsehelper(TEMP-TABLE ttValidation:HANDLE)
    oAuthCopayControl   = NEW cls.maauthcopaycontrol()
   
    dObj                = DECIMAL(oRequestHelper:getFieldValue("fdObj":U           + cContainerCode))
                        
    iLineNumber         = INTEGER(oRequestHelper:getFieldValue("fiLineNumber":U    + cContainerCode)) 
                        
    cAction             =         oRequestHelper:getFieldValue("fcAction":U        + cContainerCode)                   
    cEffectiveDate      =         oRequestHelper:getFieldValue("fdEffectiveDate":U + cContainerCode)
    cEndDate            =         oRequestHelper:getFieldValue("fdEndDate":U       + cContainerCode)
                        
    dEffectiveDate      = (IF cEffectiveDate = "YYYY/MM/DD":U THEN TODAY ELSE DATE(cEffectiveDate))                   
    dEndDate            = (IF cEndDate       = "YYYY/MM/DD":U THEN ?     ELSE DATE(cEndDate))
    NO-ERROR.


  IF NOT {&ErrorStatus} THEN
  DO:
    
    CASE cAction:
      WHEN "modify":U THEN
      DO:
        
        oAuthCopayControl:focusAuthCopayControl(dObj) NO-ERROR.

        IF NOT {&ErrorStatus} THEN
        DO:
          ASSIGN            
            oAuthCopayControl:InsurerObj                  = DECIMAL(oRequestHelper:getFieldValue("fdInsurerObj":U       + cContainerCode)) 
            oAuthCopayControl:OptionCode                  = INTEGER(oRequestHelper:getFieldValue("fiOptionCode":U       + cContainerCode)) 
            oAuthCopayControl:AuthCopayTypeObj            = DECIMAL(oRequestHelper:getFieldValue("fdAuthCopayTypeObj":U + cContainerCode)) 
            oAuthCopayControl:EffectiveDate               = dEffectiveDate
            oAuthCopayControl:EndDate                     = dEndDate      
            oAuthCopayControl:CopaymentValueType          = LOOKUP(oRequestHelper:getFieldValue("fcCopayValueType":U    + cContainerCode),"Y,YES,TRUE,P":U) > 0
            oAuthCopayControl:CopaymentValue              = DECIMAL(oRequestHelper:getFieldValue("fcCopayValue":U       + cContainerCode)) 
            oAuthCopayControl:ApplyToPmb                  = LOOKUP(oRequestHelper:getFieldValue("fcApplyToPMB":U        + cContainerCode),"Y,YES,TRUE":U) > 0
            oAuthCopayControl:ApplyToEmergency            = LOOKUP(oRequestHelper:getFieldValue("fcApplytoEmergency":U  + cContainerCode),"Y,YES,TRUE":U) > 0
            oAuthCopayControl:AuthStatus                  = INTEGER(oRequestHelper:getFieldValue("fcAuthStatus":U       + cContainerCode))
            oAuthCopayControl:AuthStatusNote              =        oRequestHelper:getFieldValue("fcAuthStatusNote":U    + cContainerCode)    
            oAuthCopayControl:CopayApplyOverrideReasons   =        oRequestHelper:getFieldValue("fcOverrideReasons":U   + cContainerCode)        
            oAuthCopayControl:WarningMessage              =        oRequestHelper:getFieldValue("fcWarningMessage":U     + cContainerCode)        
            oAuthCopayControl:WarningMessageType          =        oRequestHelper:getFieldValue("fcWarningMessageType":U + cContainerCode) 
            oAuthCopayControl:ProviderType                =        oRequestHelper:getFieldValue("fcProviderType":U       + cContainerCode) 
            lSuccess                                      = oAuthCopayControl:saveAuthCopayControl() 
           NO-ERROR.

          IF NOT {&ErrorStatus} AND NOT oAuthCopayControl:ErrorObject:ErrorsExist 
          THEN
            ASSIGN 
              oResponseHelper:RequestValid    = TRUE
              oResponseHelper:ResponseMessage = "Record successfully saved":U 
              lSuccess                        = oResponseHelper:addFieldValue("fdObj":U + cContainerCode, STRING(oAuthCopayControl:AuthCopayControlObj), TRUE)
             NO-ERROR.
        END. /*IF NOT {&ErrorStatus} THEN*/    
      END. /* WHEN "modify":U THEN */
      
      WHEN "delete":U THEN
      DO:
        oAuthCopayControl:focusAuthCopayControl(dObj) NO-ERROR.

        
        IF NOT {&ErrorStatus} AND NOT oAuthCopayControl:AuthCopayControlInFocus 
        THEN  
          ASSIGN 
            oResponseHelper:RequestValid    = FALSE
            oResponseHelper:ResponseMessage = "Record could not be deleted":U 
            NO-ERROR.
        ELSE 
          ASSIGN lSuccess = oAuthCopayControl:removeAuthCopayControl() NO-ERROR.
        
        IF NOT {&ErrorStatus} AND NOT oAuthCopayControl:ErrorObject:ErrorsExist
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
  
  IF oAuthCopayControl:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN
  DO:
    ASSIGN 
      oResponseHelper:RequestValid    = FALSE
      
      hErrorHandle                    = oAuthCopayControl:ErrorObject:getErrorTableHandle()
      lSuccess                        = oRequestHelper:mapElementsFromQueryFields(hErrorHandle)
      lSuccess                        = oResponseHelper:setError(hErrorHandle)
      
      oResponseHelper:ResponseMessage = 'Unable to perform action':U
      oResponseHelper:ReturnValue     = mipEnv:formatMessage({mip/inc/mipreturnvalue.i}, 'DESCRIPTION':U).
  
    { mip/inc/mipmessageerror.i &ResetTheError = TRUE }  
  END.   /*IF oAuthCopayControl:ErrorObject:ErrorsExist OR {&ErrorStatus} THEN*/
  
  /* Cleanup */
  { mip/inc/mipcatcherror.i &FINALLY = "IF VALID-OBJECT(oRequestHelper)     THEN DELETE OBJECT oRequestHelper     NO-ERROR. ~{mip/inc/mipmessageerror.i~}
                                        IF VALID-OBJECT(oResponseHelper)    THEN DELETE OBJECT oResponseHelper    NO-ERROR. ~{mip/inc/mipmessageerror.i~} 
                                        IF VALID-OBJECT(oAuthCopayControl)  THEN DELETE OBJECT oAuthCopayControl  NO-ERROR. ~{mip/inc/mipmessageerror.i~}" }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidation Procedure 
PROCEDURE ajaxValidation :
/*------------------------------------------------------------------------------
  Purpose   : Basket/Crosswalk Ajax Validation    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.
  
  DEFINE VARIABLE cFilterFields      AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cFilterValues      AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cReturnFields      AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cReturnValues      AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cEntityMnemonic    AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cCodeField         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cCodeValue         AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cWhereClause       AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cNoteKey           AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE cNoteType          AS CHARACTER             NO-UNDO.
  DEFINE VARIABLE rRowid             AS ROWID                 NO-UNDO.
  DEFINE VARIABLE iField             AS INTEGER               NO-UNDO.
  DEFINE VARIABLE lSuccess           AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE lError             AS LOGICAL               NO-UNDO.
  

  ASSIGN
     cFilterFields   = get-value('FldLst')
     cFilterValues   = get-value('ValList')
     cReturnFields   = get-value('RetFldList')
     cEntityMnemonic = ipcValidationArgument
     rRowid          = ?.                           

 
  CREATE ttValidation.

  
  DO iField = 1 TO NUM-ENTRIES(cFilterFields):
  
    CASE ENTRY(iField,cFilterFields):
    
      WHEN "[TableMnemonic]":U 
      THEN
        ASSIGN cEntityMnemonic = TRIM(ENTRY(iField,cFilterValues)).
        
      WHEN "[Rowid]":U 
      THEN
        ASSIGN rRowid = TO-ROWID( (IF TRIM(ENTRY(iField,cFilterValues)) = "":U THEN ? ELSE TRIM(ENTRY(iField,cFilterValues))) ).
        
      WHEN "[CodeField]":U 
      THEN
        ASSIGN cCodeValue = TRIM(ENTRY(iField,cFilterValues)).

    END CASE.
  
  END. /*DO iField = 1 TO NUM-ENTRIES(cFilterFields):*/

       
  IF cCodeValue <> "":U THEN
  DO:

    mipEnv:miDBEntity:focusTable(cEntityMnemonic).

    

    IF rRowid = ? THEN
    DO:
      /*
        We cant always rely on the entity having been configured correctly with the "code field" if at all
      */

      CASE cEntityMnemonic:              
        
        WHEN "scheme":U 
        THEN 
          ASSIGN cWhereClause = SUBSTITUTE("scheme-code = '&1'":U, cCodeValue).

        WHEN "prtype":U 
        THEN 
          ASSIGN cWhereClause = SUBSTITUTE("pr-type = '&1'":U, cCodeValue).

        WHEN "neggroup":U 
        THEN 
          ASSIGN cWhereClause = SUBSTITUTE("neg-num = '&1'":U, cCodeValue).

        WHEN "ccdesc":U 
        THEN 
          ASSIGN cWhereClause = SUBSTITUTE("claim-code = &1":U, cCodeValue).

        WHEN "doctor":U 
        THEN 
          ASSIGN cWhereClause = SUBSTITUTE("doc-num = '&1'":U, cCodeValue).

        WHEN "datalist":U 
        THEN 
          ASSIGN cWhereClause = SUBSTITUTE("list-code = '&1' AND list-type = 'PACKDESC'":U, cCodeValue).

        OTHERWISE
             ASSIGN cWhereClause = SUBSTITUTE(mipEnv:miDBEntity:CodeField + " = '&1'":U, cCodeValue).
      
      END CASE.

      mipEnv:miDBEntity:findRecordWhere(cWhereClause).

    END. /*IF rRowid = ? THEN*/
    ELSE 
      mipEnv:miDBEntity:findRecord(rRowId).
    /*if no record is available by the time we get here then we raise an error*/ 
    IF NOT mipEnv:miDBEntity:RecordAvailable THEN
      ASSIGN lError = TRUE.
  END. /*IF cCodeValue <> "":U THEN*/

  
  DO iField = 1 TO NUM-ENTRIES(cReturnFields):

    /*only return these values if we had a cCodeValue or rRowId 
      because we are using these two values to find the record details to return to the screen.*/
    IF mipEnv:miDBEntity:RecordAvailable 
    AND (cCodeValue <> "":U OR rRowId <> ?) THEN 
    DO:
      IF ENTRY(iField,cReturnFields) = "[RecordRowid]":U 
      THEN
        ASSIGN cReturnValues = cReturnValues
                             + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                             + STRING(mipEnv:miDBEntity:RecordRowId).
                             
      IF ENTRY(iField,cReturnFields) = "[RecordObj]":U 
      THEN
        ASSIGN cReturnValues = cReturnValues
                             + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                             + STRING(mipEnv:miDBEntity:RecordObj) .  
      
      IF ENTRY(iField,cReturnFields) = "[RecordKey]":U 
      THEN
        ASSIGN cReturnValues = cReturnValues
                             + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                             + STRING(mipEnv:miDBEntity:RecordKey) .              
                             
      IF ENTRY(iField,cReturnFields) = "[RecordCode]":U 
      THEN        
        ASSIGN cReturnValues = cReturnValues
                             + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                             + mipEnv:miDBEntity:RecordCode.
      
      IF ENTRY(iField,cReturnFields) = "[RecordLabel]":U 
      THEN
        ASSIGN cReturnValues = cReturnValues
                             + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                             + STRING(mipEnv:miDBEntity:RecordLabel) . 
                             
      IF ENTRY(iField,cReturnFields) = "[RecordDescription]":U 
      AND cEntityMnemonic = "prtype":U THEN 
      DO:
        FIND FIRST prtype NO-LOCK
             WHERE prtype.pr-type = INTEGER(cCodeValue)
          NO-ERROR.

        ASSIGN cReturnValues = cReturnValues
                             + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                             + STRING(prtype.description) .
      END.  /* IF ENTRY(iField,cReturnFields) = "[RecordDescription]":U */

      IF ENTRY(iField,cReturnFields) = "[RecordDescription]":U 
      THEN
        ASSIGN cReturnValues = cReturnValues
                             + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                             + STRING(mipEnv:miDBEntity:RecordDescription) .
    
    END.  /* IF mipEnv:miDBEntity:RecordAvailable */
  END. /*DO iField = 1 TO NUM-ENTRIES(cReturnFields):*/

  IF cReturnValues <> "" OR NOT lError 
  THEN
    ASSIGN 
        ttValidation.cReturnValues = cReturnValues
        ttValidation.lValid        = TRUE
        ttValidation.cMessage      = "":U.
  ELSE 
    ASSIGN 
        ttValidation.cReturnValues = "":U
        ttValidation.lValid        = FALSE
        ttValidation.cMessage      = SUBSTITUTE("Record could not be found for '&1'.":U,cCodeValue).

  { mip/inc/mipcatcherror.i }
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ajaxValidationProcedures) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationProcedures Procedure 
PROCEDURE ajaxValidationProcedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cFilterFields  AS CHARACTER  NO-UNDO.                                       
  DEFINE VARIABLE cFilterValues  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReturnError   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReturnFields  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReturnValues  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iField         AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cDate          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAuthCopayType AS CHARACTER    NO-UNDO.

  
  ASSIGN
    cFilterFields  = get-value('FldLst')
    cFilterValues  = get-value('ValList')
    cReturnFields  = get-value('RetFldList')  .

  CREATE ttValidation.
  
  DO iField = 1 TO NUM-ENTRIES(cFilterFields):
    CASE ENTRY(iField,cFilterFields):
      WHEN "[Date]":U 
      THEN
        ASSIGN cDate = TRIM(ENTRY(iField,cFilterValues)).

      WHEN "[CopayType]":U 
      THEN
        ASSIGN cAuthCopayType = TRIM(ENTRY(iField,cFilterValues)).

    END CASE.
  END. /*DO iField = 1 TO NUM-ENTRIES(cFilterFields):*/

           
  ASSIGN
    ttValidation.cMessage = "":U
    ttValidation.lValid   = ?.

  CASE ipcValidationArgument:
    WHEN "DateFormat":U
    THEN DO:
      DO iField = 1 TO NUM-ENTRIES(cReturnFields):
        IF ENTRY(iField,cReturnFields) = "[FormatedDate]":U 
        THEN
          ASSIGN cReturnValues = { ws/inc/wsdateformat.i DATE(cDate) }.
      END. /* DO iField = 1 TO NUM-ENTRIES(cReturnFields): */   

      ASSIGN
        ttValidation.cReturnValues = cReturnValues         
        NO-ERROR.
      
    END. /*END WHEN "StartDate":U */

    WHEN "AuthCopayType":U
    THEN DO:
      FIND FIRST ham_auth_copay_type NO-LOCK
          WHERE ham_auth_copay_type.auth_copay_type = cAuthCopayType NO-ERROR.

      IF NOT AVAILABLE ham_auth_copay_type THEN 
      DO:
        ASSIGN
          ttValidation.lValid        = FALSE
          ttValidation.cMessage      = "The Authorisation co-payment type doesn't exists.":U
        NO-ERROR.
      END.
      ELSE DO:
        DO iField = 1 TO NUM-ENTRIES(cReturnFields):

          IF ENTRY(iField,cReturnFields) = "[CopayType]":U 
          THEN
            ASSIGN cReturnValues = cReturnValues
                                 + (IF cReturnValues = "":U THEN "":U ELSE "|":U) 
                                 + STRING(ham_auth_copay_type.auth_copay_type).
          
          IF ENTRY(iField,cReturnFields) = "[CopayTypeObj]":U 
          THEN
            ASSIGN cReturnValues = cReturnValues
                                 + (IF cReturnValues = "":U THEN "":U ELSE "|":U) 
                                 + STRING(ham_auth_copay_type.auth_copay_type_obj).
          
          IF ENTRY(iField,cReturnFields) = "[Description]":U 
          THEN
            ASSIGN cReturnValues = cReturnValues
                                 + (IF cReturnValues = "":U THEN "":U ELSE "|":U) 
                                 + ham_auth_copay_type.description.
        
        END. /* DO iField = 1 TO NUM-ENTRIES(cReturnFields): */

        ASSIGN
          ttValidation.lValid        = TRUE
          ttValidation.cMessage      = "":U
          ttValidation.cReturnValues = cReturnValues
         NO-ERROR.
      END. /* ELSE DO: */
    END. /* WHEN "AuthCopayType":U */
     
  END CASE.

&ENDIF

  /* Cleanup */
  { mip/inc/mipcatcherror.i }



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-businessLogic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE businessLogic Procedure 
PROCEDURE businessLogic :
/*------------------------------------------------------------------------------
  Purpose   : Business logic
  Parameters:
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcWhatToDo AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE oControl           AS cls.mipwscontrol     NO-UNDO.
  
  DEFINE VARIABLE lSuccess    AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lCopyDetail AS LOGICAL NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN  
  
  IF goWob:Mode = "Search":U THEN
  DO:                  
    RUN businessLogicSearchResults IN TARGET-PROCEDURE(ipcWhatToDo).                                                                        
  END. /*IF goWob:Mode = "Search":U THEN*/

  IF goWob:Mode = "Maint":U THEN
  DO:

    RUN businessLogicMaint IN TARGET-PROCEDURE(ipcWhatToDo).
    RUN businessLogicCopayDetails IN TARGET-PROCEDURE(ipcWhatToDo).
   
  END. /*IF goWob:Mode = "Maint":U THEN*/
  
  IF LOOKUP(goWob:SubmitValue, "SearchSubmit,MaintSubmit,Submit,Confirm":U) > 0 THEN
  DO: 
    /*Revert to the Search screen after delete*/
    IF goWob:SubmitValue = "Delete":U THEN
      ASSIGN goWob:Mode = "Search":U.

    IF VALID-OBJECT(goCntMaint) THEN
    DO:
        IF goCntMaint:ContainerMode = "Copy" AND LOOKUP(goWob:SubmitValue, "MaintSubmit,Submit":U) > 0 THEN
          ASSIGN 
            oControl           = goCntDetailsCopy:getControl("flCopyDetails":U + goCntDetailsCopy:ContainerCode ) WHEN VALID-OBJECT(goCntDetailsCopy)
            lCopyDetail        = LOOKUP(oControl:ControlValue, "YES,TRUE":U) > 0                                  WHEN VALID-OBJECT(goCntDetailsCopy).
        IF DECIMAL(goWob:CurrentObj) =  0 THEN 
          ASSIGN goWob:CurrentObj = "-99":U.
    END. /* IF VALID-OBJECT(goCntMaint) THEN */
    
  
    IF ipcWhatToDo = "Submit":U + "->":U + "Add":U 
    AND lCopyDetail 
    THEN
      mipEnv:Health:AuthMaintenance:copyAuthCopay ( INPUT lCopyDetail,
                                                    INPUT-OUTPUT DATASET dsAuthCopayControl BY-REFERENCE). 
    ELSE
      mipEnv:Health:AuthMaintenance:saveAuthCopay(INPUT-OUTPUT DATASET dsAuthCopayControl BY-REFERENCE).
 
    IF  ipcWhatToDo = "Submit->Add":U 
    AND lCopyDetail THEN
      /*
      Validation error during save
      */
      ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_auth_copay_error NO-LOCK 
                                                  WHERE tt_auth_copay_error.error_type = "ERR":U
                                                    AND tt_auth_copay_error.owning_entity_mnemonic = "haccc":U ).
    ELSE 
       ASSIGN WarpSpeed:ValidationError = CAN-FIND(FIRST tt_auth_copay_error NO-LOCK 
                                                WHERE tt_auth_copay_error.error_type = "ERR":U).
   
    IF LOOKUP(goWob:SubmitValue, "SearchSubmit,MaintSubmit":U) > 0 THEN
    DO:
      FOR EACH tt_auth_copay_error NO-LOCK:
      
        FIND FIRST tt_deleted NO-LOCK
             WHERE tt_deleted.owning_obj = tt_auth_copay_error.owning_obj
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 
        
        IF AVAILABLE tt_deleted 
        THEN DELETE tt_deleted. 
      
      END. /*FOR EACH tt_auth_copay_error NO-LOCK:*/
    END. /*IF LOOKUP(goWob:SubmitValue, "SearchSubmit":U) > 0 THEN*/

    
    IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN
    DO:    
      FIND FIRST tt_auth_copay_control NO-LOCK NO-ERROR.      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
      
      IF AVAILABLE tt_auth_copay_control
      THEN
        ASSIGN 
           goWob:CurrentObj     = STRING(tt_auth_copay_control.auth_copay_control_obj)
           Warpspeed:CurrentObj = goWob:CurrentObj
           lSuccess             = wsUIService:setObj(goWob:ObjectCode, goWob:CurrentObj).
         
    END. /*IF NOT Warpspeed:ValidationError AND CAN-DO("Submit->Add,Submit->Change":U, TRIM(ipcWhatToDo)) THEN*/ 
    /*
      Validation error during save - populate maint container with errors
    */
    IF Warpspeed:ValidationError AND VALID-OBJECT(goCntMaint) AND CAN-FIND(FIRST tt_auth_copay_error NO-LOCK 
                                                                            WHERE tt_auth_copay_error.error_type = "ERR":U
                                                                              AND tt_auth_copay_error.owning_entity_mnemonic = "haccc":U )
    THEN DO:
        mipEnv:Health:maUiService:setContainerErrors(TEMP-TABLE tt_auth_copay_error:HANDLE, goCntMaint, "haccc":U,(IF ipcWhatToDo = "Submit->Add":U THEN -99 ELSE DECIMAL(goWob:CurrentObj)) , "":U).
    
      FIND FIRST tt_auth_copay_error NO-LOCK
           WHERE tt_auth_copay_error.error_type = "ERR":U
       NO-ERROR.
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

      IF AVAILABLE tt_auth_copay_error AND (tt_auth_copay_error.error_number = 125 OR tt_auth_copay_error.error_number = 112) THEN
      DO:
        ASSIGN oControl                  = goCntMaint:getControl("fdEffectiveDate":U + goCntMaint:ContainerCode)
               Warpspeed:validationError = TRUE.
               oControl:setError(tt_auth_copay_error.error_message).
      END. 
    END. /* IF Warpspeed:ValidationError AND VALID-OBJECT(goCntMaint) */

    /*
      Handle any warnings returned from the service during save
      We will copy the warnings to another temp table as the auth dataset
      will be emptied when we retrieve date in webcreateresponse.
    */
    FOR EACH  tt_auth_copay_error NO-LOCK
       WHERE (tt_auth_copay_error.error_type = 'WAR':U
          OR  tt_auth_copay_error.error_type = 'WARN':U
          OR  tt_auth_copay_error.ERROR_type = 'WARNACK':U):
    
      CREATE tt_warning.
      BUFFER-COPY tt_auth_copay_error TO tt_warning.
    
    END. /*IF CAN-FIND(FIRST tt_auth_copay_error) THEN*/

  END. /*IF LOOKUP(goWob:SubmitValue, "SearchSubmit,MaintSubmit,Submit,Confirm":U) > 0 THEN*/
  
  { mip/inc/mipcatcherror.i }
  
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-businessLogicCopayDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE businessLogicCopayDetails Procedure 
PROCEDURE businessLogicCopayDetails :
/*------------------------------------------------------------------------------
  Purpose   : Search results business logic
  Parameters: 
  Notes     :
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcWhatToDo AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE iRow                        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cAction                     AS CHARACTER   NO-UNDO. 
  DEFINE VARIABLE cEffectiveDate              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cEndDate                    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dObj                        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dEffectiveDate              AS DATE        NO-UNDO.
  DEFINE VARIABLE dEndDate                    AS DATE        NO-UNDO.
  
  DEFINE VARIABLE dCopayControlObj            AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOwninigEntityMnemonic      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dOwningObj                  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOwningKey                  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOwningAltValue             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cExclusionEntityMnemonic    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dExclusionObj               AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cExclusionKey               AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cExclusionAltValue          AS CHARACTER   NO-UNDO.
  
  DEFINE VARIABLE oSearch    AS cls.maauthcopaysearch NO-UNDO.
  DEFINE VARIABLE hARDataset AS HANDLE                NO-UNDO.
  DEFINE VARIABLE lSuccess   AS LOGICAL               NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  /*we will run this block if we are copying details records*/
  IF LOOKUP(goCntCopayDetails:ContainerMode ,"Copy,Submit":U) > 0
  AND ipcWhatToDo = "Submit->Add":U 
  THEN 
  DO:
    IF LOOKUP( goCntDetailsCopy:getControl( "flCopyDetails" + goCntDetailsCopy:ContainerCode):ControlValue , "YES,TRUE":U) > 0 
    THEN DO:
      CREATE DATASET hARDataset .
      hARDataset:CREATE-LIKE (DATASET dsAuthCopayControl:HANDLE).
      hARDataset:COPY-DATASET(DATASET dsAuthCopayControl:HANDLE).    /*This will copy the data as well as the structure to our dynamic dataset*/
      DATASET dsAuthCopayControl:EMPTY-DATASET().
      
      /*we will fetch the details records from the DB and merge*/
      ASSIGN oSearch          = NEW cls.maauthcopaysearch(DATASET dsAuthCopayControl BY-REFERENCE)
             dCopayControlObj = DECIMAL(goCntDetailsCopy:getControl("fdObj" + goCntDetailsCopy:ContainerCode):ControlValue)
             lSuccess         = oSearch:SetCriteria("BufferList":U, "tt_auth_copay_detail":U )
             lSuccess         = oSearch:SetFilterCriteria("tt_auth_copay_detail.auth_copay_control_obj":U , "=":U , STRING(dCopayControlObj)).

      /*Note that the fetch will always fetch at least one copay_control record we don't need this record beacuse we will save the from the control container UI */     
      oSearch:fetchData().
      
      //merge the temp-tables
      DATASET dsAuthCopayControl:HANDLE:COPY-DATASET( hARDataset:HANDLE, TRUE).
      /*Because the oSearch:fetchData() statement will also fetch the parent record tt_auth_copay_control record we then need to delete the record copied to the dataset */
      FIND FIRST tt_auth_copay_control EXCLUSIVE-LOCK
           WHERE tt_auth_copay_control.auth_copay_control_obj = dCopayControlObj
        NO-ERROR.
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
      
      IF AVAILABLE tt_auth_copay_control THEN
        DELETE tt_auth_copay_control.

    END. /*IF LOOKUP( goCntDetailsCopy:getControl( "flCopyDetails" + goCntDetailsCopy:ContainerCode):ControlValue , "YES,TRUE":U) > 0 */
  END. /*IF goCntCopayDetails:ContainerMode = "Copy":U AND ipcWhatToDo = "Submit->Add":U THEN */

  IF CAN-DO("MaintSubmit":U, goWob:SubmitValue) 
  THEN
  DO-BLK:
  DO iRow = 1 TO INTEGER(get-value(goCntCopayDetails:ContainerCode + "_rowsrendered":U)):
 
    ASSIGN
      cAction                    =         get-value("fcAction":U               + goCntCopayDetails:ContainerCode + STRING(iRow))                                                                     
      cEffectiveDate             =         get-value("fdEffectiveDate":U        + goCntCopayDetails:ContainerCode + STRING(iRow))
      cEndDate                   =         get-value("fdEndDate":U              + goCntCopayDetails:ContainerCode + STRING(iRow))
                                 
      dObj                       = DECIMAL(get-value("fdObj":U                  + goCntCopayDetails:ContainerCode + STRING(iRow)))          
      dObj                       = (IF dObj <= 0                            
                                    THEN iRow * -1 ELSE dObj)               
                                  
      dCopayControlObj           = DECIMAL(get-value("fdCopayControlObjArgument":U  + goCntCopayDetails:ContainerCode + STRING(iRow)))
      cOwninigEntityMnemonic     =         get-value("fcOwningEntityMnemonic":U     + goCntCopayDetails:ContainerCode + STRING(iRow))
      dOwningObj                 = DECIMAL(get-value("fdOwningObj":U                + goCntCopayDetails:ContainerCode + STRING(iRow)))    
      cOwningKey                 =         get-value("fcOwningKey":U                + goCntCopayDetails:ContainerCode + STRING(iRow))
      cOwningAltValue            =         get-value("fcOwningCode":U               + goCntCopayDetails:ContainerCode + STRING(iRow))
      cExclusionEntityMnemonic   =         get-value("fcExclusionEntityMnemonic":U  + goCntCopayDetails:ContainerCode + STRING(iRow))
      dExclusionObj              = DECIMAL(get-value("fdExclusionObj":U             + goCntCopayDetails:ContainerCode + STRING(iRow)))
      cExclusionKey              =         get-value("fcExclusionKey":U             + goCntCopayDetails:ContainerCode + STRING(iRow))
      cExclusionAltValue         =         get-value("fcExclusionCode":U            + goCntCopayDetails:ContainerCode + STRING(iRow))
      
      dEffectiveDate             = (IF cEffectiveDate = "YYYY/MM/DD":U THEN TODAY ELSE DATE(cEffectiveDate))          
      dEndDate                   = (IF cEndDate       = "YYYY/MM/DD":U THEN ?     ELSE DATE(cEndDate)).
      
    /* 
      We dont want to update records that did not change but create a temp table record for all rows
      so that if there are any errors, the screen will be built for the user as it was when they submitted
      from this temp table
    */

    IF cAction <> "":U THEN
    DO:
    
      CREATE tt_auth_copay_detail.
      ASSIGN tt_auth_copay_detail.record_action                = cAction
             tt_auth_copay_detail.line_number                  = iRow
             tt_auth_copay_detail.auth_copay_detail_obj        = dObj
             tt_auth_copay_detail.auth_copay_control_obj       = dCopayControlObj
             tt_auth_copay_detail.owning_entity_mnemonic       = cOwninigEntityMnemonic  
             tt_auth_copay_detail.owning_obj                   = dOwningObj              
             tt_auth_copay_detail.owning_key                   = cOwningKey              
             tt_auth_copay_detail.owning_alt_value             = cOwningAltValue         
             tt_auth_copay_detail.exclusion_entity_mnemonic    = cExclusionEntityMnemonic
             tt_auth_copay_detail.exclusion_obj                = dExclusionObj           
             tt_auth_copay_detail.exclusion_key                = cExclusionKey           
             tt_auth_copay_detail.exclusion_alt_value          = cExclusionAltValue      
             tt_auth_copay_detail.effective_date               = dEffectiveDate                        
             tt_auth_copay_detail.end_date                     = dEndDate         
             .
             
      VALIDATE tt_auth_copay_detail.       

      IF cAction = "Delete":U THEN 
      DO:
        CREATE tt_deleted.
        ASSIGN tt_deleted.owning_obj = dObj
               tt_deleted.owning_key = "":U.
           
        VALIDATE tt_deleted.
      END. /*IF cAction = "Delete":U THEN*/       
    END. /*IF cAction <> "":U THEN*/        
  END. /* DO iRow = 1 */
  
  IF goWob:SubmitValue = "Confirm":U THEN
  DO:
    CREATE DATASET hARDataset .
      hARDataset:CREATE-LIKE (DATASET dsAuthCopayControl:HANDLE).
      hARDataset:COPY-DATASET(DATASET dsAuthCopayControl:HANDLE).    /*This will copy the data as well as the structure to our dynamic dataset*/
      DATASET dsAuthCopayControl:EMPTY-DATASET().
      
    /*we will fetch the details records from the DB and merge*/
    ASSIGN oSearch          = NEW cls.maauthcopaysearch(DATASET dsAuthCopayControl BY-REFERENCE)
           dCopayControlObj = DECIMAL(goWob:CurrentObj)
           lSuccess         = oSearch:SetCriteria("BufferList":U, "tt_auth_copay_detail, tt_auth_copay_control":U )
           lSuccess         = oSearch:SetFilterCriteria("tt_auth_copay_control.auth_copay_control_obj":U , "=":U , STRING(dCopayControlObj)).

    /* Note that the fetch will always fetch at least one copay_control record we don't need this record beacuse we will save the from the control container UI */     
    oSearch:fetchData().
      
    //merge the temp-tables
    DATASET dsAuthCopayControl:HANDLE:COPY-DATASET( hARDataset:HANDLE, TRUE).
  
    FIND FIRST tt_auth_copay_control EXCLUSIVE-LOCK
             WHERE tt_auth_copay_control.auth_copay_control_obj = dCopayControlObj.
                 
        ASSIGN 
          tt_auth_copay_control.record_action = "DELETE":U.
  END. /* IF goWob:SubmitValue = "Confirm":U THEN */
   
  { mip/inc/mipcatcherror.i }

&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-businessLogicMaint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE businessLogicMaint Procedure 
PROCEDURE businessLogicMaint :
/*------------------------------------------------------------------------------
  Purpose   : Maintenance business logic
  Parameters: 
  Notes     :
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcWhatToDo          AS CHARACTER     NO-UNDO.
                                              
  
  DEFINE VARIABLE iOption                     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cEffectiveDate              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cEndDate                    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dInsurerObj                 AS DECIMAL     NO-UNDO. 
  DEFINE VARIABLE dEffectiveDate              AS DATE        NO-UNDO.
  DEFINE VARIABLE dEndDate                    AS DATE        NO-UNDO.
  DEFINE VARIABLE fdCopayTypeObj              AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE lApplyToPMB                 AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lApplyToEmergency           AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lCopaymentValueType         AS LOGICAL     FORMAT "P/R"   NO-UNDO.
  DEFINE VARIABLE dCopaymentValue             AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cCopayApplyOverrideReasons  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAuthStatus                 AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cWarningMessage             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWarningMessageType         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cAuthStatusNote             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCopaymentValueType         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cProviderType               AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lSuccess                    AS LOGICAL     NO-UNDO.
&IF {&DBDFMA} >= 010195 &THEN
  
  IF CAN-DO(  "Submit->Add,":U
            + "Submit->Change,":U
            + "Confirm->Delete":U,
            TRIM(ipcWhatToDo) ) 
  THEN
  DO TRANSACTION:
    
    ASSIGN 
           cEffectiveDate             =         get-value("fdEffectiveDate":U            + goCntMaint:ContainerCode)
           cEndDate                   =         get-value("fdEndDate":U                  + goCntMaint:ContainerCode)               
           dEffectiveDate             = (IF cEffectiveDate = "YYYY/MM/DD":U THEN TODAY ELSE DATE(cEffectiveDate))          
           dEndDate                   = (IF cEndDate       = "YYYY/MM/DD":U THEN ?     ELSE DATE(cEndDate))
           dInsurerObj                = DECIMAL(get-value("fdInsurerObj":U               + goCntMaint:ContainerCode))
           iOption                    = INTEGER(get-value("fiOptionCode":U               + goCntMaint:ContainerCode))
           fdCopayTypeObj             = DECIMAL(get-value("fdAuthCopayTypeObj":U         + goCntMaint:ContainerCode))
           lApplyToPMB                = LOOKUP(get-value("flapplyToPMB":U                + goCntMaint:ContainerCode), "Y,YES,TRUE":U) > 0
           lApplyToEmergency          = LOOKUP(get-value("flApplyToEmergency":U          + goCntMaint:ContainerCode), "Y,YES,TRUE":U) > 0
           cCopaymentValueType        =        (get-value("fcCopaymentValueType":U       + goCntMaint:ContainerCode))  
           dCopaymentValue            = DECIMAL(get-value("fcCopaymentValue":U           + goCntMaint:ContainerCode))
           cCopayApplyOverrideReasons =        get-value("fcOverrideReasonsKeys":U       + goCntMaint:ContainerCode)
           iAuthStatus                = INTEGER(get-value("fcAuthStatus":U               + goCntMaint:ContainerCode))
           cWarningMessage            =        get-value("fcWarningMessage":U            + goCntMaint:ContainerCode)
           cAuthStatusNote            =        get-value("fcAuthStatusNote":U            + goCntMaint:ContainerCode)
           cWarningMessageType        =        get-value("fcWarningType":U               + goCntMaint:ContainerCode)
           cProviderType              =        get-value("fcProviderType":U              + goCntMaint:ContainerCode).
        
    IF cCopaymentValueType = "P" 
    OR cCopaymentValueType = "R" THEN
      ASSIGN lCopaymentValueType = LOGICAL(cCopaymentValueType,"P/R":U).
    ELSE 
      ASSIGN lCopaymentValueType = ?.

    CREATE tt_auth_copay_control.   
    ASSIGN tt_auth_copay_control.auth_copay_control_obj       = (IF ipcWhatToDo = "Submit->Add":U
                                                                THEN -99 
                                                                ELSE DECIMAL(goWob:CurrentObj))
                                                              
           tt_auth_copay_control.auth_copay_type_obj          = fdCopayTypeObj    
           tt_auth_copay_control.effective_date               = dEffectiveDate               
           tt_auth_copay_control.end_date                     = dEndDate
           tt_auth_copay_control.insurer_obj                  = dInsurerObj
           tt_auth_copay_control.option_code                  = iOption
           tt_auth_copay_control.apply_to_pmb                 = lApplyToPMB       
           tt_auth_copay_control.apply_to_emergency           = lApplyToEmergency  
           tt_auth_copay_control.copayment_value_type         = lCopaymentValueType
           tt_auth_copay_control.copayment_value              = dCopaymentValue     
           tt_auth_copay_control.auth_status                  = iAuthStatus
           tt_auth_copay_control.warning_message              = cWarningMessage
           tt_auth_copay_control.auth_status_note             = cAuthStatusNote
           tt_auth_copay_control.warning_message_type         = cWarningMessageType
           tt_auth_copay_control.copay_apply_override_reasons = cCopayApplyOverrideReasons
           tt_auth_copay_control.provider_type                = cProviderType.

    IF ipcWhatToDo = "Submit->Add":U OR ipcWhatToDo = "Submit->Change":U THEN
    DO:                                           
      ASSIGN tt_auth_copay_control.record_action = "MODIFY":U.
                  
      VALIDATE tt_auth_copay_control.
    END. /*IF ipcWhatToDo = "Submit->Add":U OR ipcWhatToDo = "Submit->Change":U OR ipcWhatToDo = "Submit->Copy":U THEN*/
    
    IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_copay_control THEN
    DO:
      ASSIGN tt_auth_copay_control.record_action = "DELETE":U.
      
      VALIDATE tt_auth_copay_control.
    END. /*IF ipcWhatToDo = "Confirm->Delete":U AND AVAILABLE tt_auth_copay_control THEN*/
  END. /*IF CAN-DO("Submit":U, goWob:SubmitValue) THEN*/
  
  

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-businessLogicSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE businessLogicSearchResults Procedure 
PROCEDURE businessLogicSearchResults :
/*------------------------------------------------------------------------------
  Purpose   : Search results business logic
  Parameters: 
  Notes     :
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcWhatToDo AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE iRow                        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iOption                     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cAction                     AS CHARACTER   NO-UNDO. 
  DEFINE VARIABLE cEffectiveDate              AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cEndDate                    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dInsurerObj                 AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dObj                        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dEffectiveDate              AS DATE        NO-UNDO.
  DEFINE VARIABLE dEndDate                    AS DATE        NO-UNDO.
  
  DEFINE VARIABLE dAuthCopayControlObj        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dAuthCopayTypeObj           AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE lCopaymentValueType         AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE dCopaymentValue             AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE lApplyToPmb                 AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lApplyToEmergency           AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE iAuthStatus                 AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cAuthStatusNote             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCopayApplyOverrideReasons  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWarningMessage             AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWarningMessageType         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cProviderType               AS CHARACTER   NO-UNDO.


&IF {&DBDFMA} >= 010195 &THEN

  IF CAN-DO("SearchSubmit":U, goWob:SubmitValue) 
  THEN
  DO-BLK:
  DO iRow = 1 TO INTEGER(get-value(goCntSearchResults:ContainerCode + "_rowsrendered":U)):
  
    ASSIGN
      cAction                     =         get-value("fcAction":U               + goCntSearchResults:ContainerCode + STRING(iRow))                                                                     
      cEffectiveDate              =         get-value("fdEffectiveDate":U        + goCntSearchResults:ContainerCode + STRING(iRow))
      cEndDate                    =         get-value("fdEndDate":U              + goCntSearchResults:ContainerCode + STRING(iRow))
                                  
      dInsurerObj                 = DECIMAL(get-value("fdInsurerObj":U           + goCntSearchResults:ContainerCode + STRING(iRow)))
      dObj                        = DECIMAL(get-value("fdObj":U                  + goCntSearchResults:ContainerCode + STRING(iRow)))          
      dObj                        = (IF dObj <= 0                            
                                     THEN iRow * -1 ELSE dObj)               
                                  
      iOption                     = INTEGER(get-value("fiOptionCode":U           + goCntSearchResults:ContainerCode + STRING(iRow)))

      dAuthCopayTypeObj           = DECIMAL(get-value("fdAuthCopayTypeObj":U     + goCntSearchResults:ContainerCode + STRING(iRow)))
      lCopaymentValueType         = LOOKUP(get-value("fcCopayValueType":U        + goCntSearchResults:ContainerCode + STRING(iRow)), "Y,YES,TRUE,P":U) > 0
      dCopaymentValue             = DECIMAL(get-value("fcCopayValue":U           + goCntSearchResults:ContainerCode + STRING(iRow)))        
      lApplyToPmb                 = LOOKUP(get-value("fcApplyToPMB":U            + goCntSearchResults:ContainerCode + STRING(iRow)), "Y,YES,TRUE":U) > 0 
      lApplyToEmergency           = LOOKUP(get-value("fcApplyToEmergency":U      + goCntSearchResults:ContainerCode + STRING(iRow)), "Y,YES,TRUE":U) > 0
      iAuthStatus                 = INTEGER(get-value("fcAuthStatus":U           + goCntSearchResults:ContainerCode + STRING(iRow)))
      cAuthStatusNote             =         get-value("fcAuthStatusNote":U       + goCntSearchResults:ContainerCode + STRING(iRow))
      cCopayApplyOverrideReasons  =         get-value("fcOverrideReasons":U      + goCntSearchResults:ContainerCode + STRING(iRow))
      cWarningMessage             =         get-value("fcWarningMessage":U       + goCntSearchResults:ContainerCode + STRING(iRow))
      cWarningMessageType         =         get-value("fcWarningMessageType":U   + goCntSearchResults:ContainerCode + STRING(iRow))

      dEffectiveDate              = (IF cEffectiveDate = "YYYY/MM/DD":U THEN TODAY ELSE DATE(cEffectiveDate))          
      dEndDate                    = (IF cEndDate       = "YYYY/MM/DD":U THEN ?     ELSE DATE(cEndDate))
      cProviderType               =         get-value("fcProviderType":U         + goCntSearchResults:ContainerCode + STRING(iRow)).
      
    /* 
      We dont want to update records that did not change but create a temp table record for all rows
      so that if there are any errors, the screen will be built for the user as it was when they submitted
      from this temp table
    */

    IF cAction <> "":U THEN
    DO:
    
      CREATE tt_auth_copay_control.
      ASSIGN tt_auth_copay_control.record_action                = cAction
             tt_auth_copay_control.line_number                  = iRow
             tt_auth_copay_control.auth_copay_control_obj       = dObj
             tt_auth_copay_control.auth_copay_type_obj          = dAuthCopayTypeObj
             tt_auth_copay_control.effective_date               = dEffectiveDate
             tt_auth_copay_control.end_date                     = dEndDate
             tt_auth_copay_control.insurer_obj                  = dInsurerObj
             tt_auth_copay_control.option_code                  = iOption
             tt_auth_copay_control.copayment_value_type         = lCopaymentValueType
             tt_auth_copay_control.copayment_value              = dCopaymentValue
             tt_auth_copay_control.apply_to_pmb                 = lApplyToPmb
             tt_auth_copay_control.apply_to_emergency           = lApplyToEmergency
             tt_auth_copay_control.auth_status                  = iAuthStatus
             tt_auth_copay_control.auth_status_note             = cAuthStatusNote
             tt_auth_copay_control.copay_apply_override_reasons = cCopayApplyOverrideReasons
             tt_auth_copay_control.warning_message              = cWarningMessage
             tt_auth_copay_control.warning_message_type         = cWarningMessageType
             tt_auth_copay_control.provider_type                = cProviderType
             .
             
      VALIDATE tt_auth_copay_control.       

      IF cAction = "Delete":U THEN 
      DO:
        CREATE tt_deleted.
        ASSIGN tt_deleted.owning_obj = dObj
               tt_deleted.owning_key = "":U.
           
        VALIDATE tt_deleted.
      END. /*IF cAction = "Delete":U THEN*/       
    END. /*IF cAction <> "":U THEN*/        
  END. /* DO iRow = 1 */
   
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
  Purpose   :
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.

  DEFINE VARIABLE cRuleValue            AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cAuthStatusList       AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cAuthStatus           AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cOverrideReasonKeys   AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cEntityMnemonic       AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE cKey                  AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE dObj                  AS DECIMAL      NO-UNDO.
  DEFINE VARIABLE dAuthCopayTypeObj     AS DECIMAL      NO-UNDO.
  DEFINE VARIABLE iCount                AS INTEGER      NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL      NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL      NO-UNDO.
  DEFINE VARIABLE cPercentage           AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE oQuery                AS cls.mipquery NO-UNDO.
      
&IF {&DBDFMA} >= 010195 &THEN

  
  CASE ipoControl:RenderArgument:
    
    WHEN "OverrideReasons":U THEN
    DO:
      ASSIGN oQuery               = ipoControl:ParentContainer:ContainerQuery
             cOverrideReasonKeys  = oQuery:getFieldAttribute("tt_auth_copay_control.copay_apply_override_reasons":U, "BUFFER-VALUE":U).
             
      IF  TRIM(cOverrideReasonKeys) <> "":U THEN 
        ipoControl:ControlToolTip = "Multiple override reasons selected ? please select the lookup for more details." .
      ELSE 
        ipoControl:ControlToolTip = "Please specify the co-payment override reasons for which a co-payment must apply." .
 
      ipoControl:RenderASTextArea().
    END. /*WHEN "OverrideReasons":U THEN*/
    WHEN "OptionCode":U THEN
    DO:
      ASSIGN ipoControl:ControlValue = IF ipoControl:ControlValue = "000":U
                                       THEN "":U
                                       ELSE ipoControl:ControlValue.
      ipoControl:RenderASInput().
    END. /*WHEN "OptionCode":U THEN*/

    WHEN "IntegerFormat":U THEN
    DO:
      ASSIGN ipoControl:ControlValue = IF ipoControl:ControlValue = "0":U OR ipoControl:ControlValue = "000":U
                                       THEN "":U
                                       ELSE ipoControl:ControlValue.
        
    END. /*WHEN "OptionCode":U THEN*/

    WHEN "CopayType":U THEN
    DO:
      ASSIGN oQuery            = ipoControl:ParentContainer:ContainerQuery
             dAuthCopayTypeObj = DECIMAL(oQuery:getFieldAttribute("tt_auth_copay_control.auth_copay_type_obj":U, "BUFFER-VALUE":U)).
              
      FIND FIRST ham_auth_copay_type NO-LOCK
           WHERE ham_auth_copay_type.auth_copay_type_obj = dAuthCopayTypeObj
          NO-ERROR.

      IF AVAILABLE ham_auth_copay_type 
      THEN
        ASSIGN 
          ipoControl:ControlValue = ham_auth_copay_type.auth_copay_type.
      ELSE 
        ASSIGN ipoControl:ControlValue = "":U.

      ipoControl:renderAsInput().

    END. /* WHEN "CopayType":U THEN */

    WHEN "CopayValueType":U THEN
    DO:
      ASSIGN oQuery      = ipoControl:ParentContainer:ContainerQuery
             dObj        = DECIMAL(oQuery:getFieldAttribute("tt_auth_copay_control.auth_copay_control_obj":U, "BUFFER-VALUE":U))
             cPercentage = STRING(oQuery:getFieldAttribute("tt_auth_copay_control.copayment_value_type":U, "BUFFER-VALUE":U)).
      IF dObj <= 0 THEN 
      DO:
         ASSIGN ipoControl:ControlValue = "?":U.
      END. /* IF dObj < 0 THEN */
      ELSE DO:
        IF LOOKUP(cPercentage , "Yes,true,P":U) > 0 
        THEN ASSIGN ipoControl:ControlValue = "P":U.
        
        IF LOOKUP(cPercentage,"no,false,R":U) > 0 
        THEN ASSIGN ipoControl:ControlValue = "R":U.
        
        IF LOOKUP(cPercentage, "?,") > 0 
        THEN ASSIGN ipoControl:ControlValue = "?":U.
       END. /* ELSE DO: */
      ASSIGN lSuccess = ipoControl:renderASComboORSelect().

    END. /* WHEN "CopayValueType":U THEN */

    WHEN "AuthStatus":U THEN
    DO:
      ASSIGN lSuccess  = mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0,
                                                                        INPUT  0,
                                                                        INPUT  "ma_acAuthRuleTypeAuthSetups":U,
                                                                        INPUT  "CopayValidStatuses":U,
                                                                        INPUT  TODAY,
                                                                        OUTPUT lValidRule,
                                                                        OUTPUT cRuleValue).

      IF lValidRule AND cRuleValue <> "":U THEN
      DO iCount = 1 TO NUM-ENTRIES(cRuleValue):
         ASSIGN 
           cAuthStatus     = SUBSTRING(ENTRY(iCount, cRuleValue), 3) + "=":U + SUBSTRING(ENTRY(iCount, cRuleValue), 1,1)
           cAuthStatusList = IF cAuthStatusList = "":U THEN cAuthStatus
                             ELSE cAuthStatusList + "|":U + cAuthStatus.
      END. /* IF lValidRule AND cRuleValue <> "":U THEN */

      ASSIGN 
        ipoControl:AdditionalItems = "|":U + cAuthStatusList
        lSuccess                   = ipoControl:renderASComboORSelect().     

    END. /* WHEN "AuthStatus":U THEN */
    WHEN "StatusNote":U THEN
    DO:
       ASSIGN oQuery      = ipoControl:ParentContainer:ContainerQuery
              cAuthStatus = oQuery:getFieldAttribute("tt_auth_copay_control.auth_status":U, "BUFFER-VALUE":U).
              
       IF cAuthStatus <> "" AND cAuthStatus <> "0" THEN
         FIND FIRST note NO-LOCK
              WHERE note.type = "AS" + cAuthStatus 
                AND note.key  = oQuery:getFieldAttribute("tt_auth_copay_control.auth_status_note":U, "BUFFER-VALUE":U) NO-ERROR.
                
       IF AVAILABLE note THEN
       ipoControl:ControlToolTip = note.narration[1].
       ELSE 
         ipoControl:ControlToolTip = "Please select a Status Reason which will indicate why the Provider is denied on the Authorisation.".
         
       ipoControl:renderAsInput().
       
    END.

    WHEN "WarningType":U THEN
    DO:
       ASSIGN oQuery = ipoControl:ParentContainer:ContainerQuery
             dObj   = DECIMAL(oQuery:getFieldAttribute("tt_auth_copay_control.auth_copay_control_obj":U, "BUFFER-VALUE":U)).

      IF dObj <= 0 
      THEN 
        ASSIGN ipoControl:ControlValue = "":U.

      ipoControl:renderAsComboOrSelect().

    END. /* WHEN "WarningType":U THEN */

    WHEN "Description":U THEN
    DO:
      ASSIGN oQuery          = ipoControl:ParentContainer:ContainerQuery.

      IF DECIMAL(oQuery:getFieldAttribute("tt_auth_copay_detail.auth_copay_detail_obj":U, "BUFFER-VALUE":U)) > 0.00 THEN
      DO:
        IF ipoControl:ControlName MATCHES "*Exclusion*":U 
        THEN
          ASSIGN oQuery          = ipoControl:ParentContainer:ContainerQuery
                 cEntityMnemonic =         oQuery:getFieldAttribute("tt_auth_copay_detail.exclusion_entity_mnemonic":U, "BUFFER-VALUE":U)
                 dObj            = DECIMAL(oQuery:getFieldAttribute("tt_auth_copay_detail.exclusion_obj":U,             "BUFFER-VALUE":U))
                 cKey            =         oQuery:getFieldAttribute("tt_auth_copay_detail.exclusion_key":U,             "BUFFER-VALUE":U).
        ELSE
          ASSIGN oQuery          = ipoControl:ParentContainer:ContainerQuery
                 cEntityMnemonic =         oQuery:getFieldAttribute("tt_auth_copay_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U)
                 dObj            = DECIMAL(oQuery:getFieldAttribute("tt_auth_copay_detail.owning_obj":U,             "BUFFER-VALUE":U))
                 cKey            =         oQuery:getFieldAttribute("tt_auth_copay_detail.owning_key":U,             "BUFFER-VALUE":U).
        
        CASE cEntityMnemonic:              
          
          WHEN "prtype":U THEN 
          DO:   
            FIND FIRST prtype NO-LOCK
                 WHERE prtype.pr-type = INTEGER(cKey)
              NO-ERROR.
        
            IF AVAILABLE prtype 
            THEN 
              ASSIGN ipoControl:ControlValue = prtype.description.
          END. /* WHEN "prtype":U THEN  */
        
          WHEN "neggroup":U THEN 
          DO:   
            FIND FIRST neggroup NO-LOCK
                 WHERE neggroup.neg-num = INTEGER(cKey)
              NO-ERROR.
        
            IF AVAILABLE neggroup
            THEN 
              ASSIGN ipoControl:ControlValue = neggroup.name.
          END. /* WHEN "neggroup":U THEN  */
        
          WHEN "doctor":U THEN
          DO: 
            FIND FIRST doctor NO-LOCK
                 WHERE doctor.doc-num = INTEGER(cKey)
              NO-ERROR.
        
            IF AVAILABLE doctor 
            THEN 
              ASSIGN ipoControl:ControlValue = doctor.name.
          END. /* WHEN "doctor":U THEN */
        
          WHEN "hdmwg":U THEN
          DO:
            FIND FIRST hdm_workgroup NO-LOCK
                 WHERE hdm_workgroup.workgroup_obj = dObj
              NO-ERROR.
        
            IF AVAILABLE hdm_workgroup 
            THEN 
              ASSIGN ipoControl:ControlValue = hdm_workgroup.workgroup_name.
          END. /* WHEN "hdmwg":U THEN */
        
        END CASE.
      END. /* IF DECIMAL(oQuery:getFieldAttribute("tt_auth_copay_detail.copay_detail_obj":U, "BUFFER-VALUE":U)) > 0 THEN */
      ELSE
        ASSIGN ipoControl:ControlValue = "":U.

      ipoControl:renderAsTextArea().
    END. /* WHEN "Description":U THEN */

    WHEN "EntityCode":U THEN
    DO:
       ASSIGN oQuery          = ipoControl:ParentContainer:ContainerQuery
              cEntityMnemonic = oQuery:getFieldAttribute("tt_auth_copay_detail.exclusion_entity_mnemonic":U, "BUFFER-VALUE":U).

       IF cEntityMnemonic = "":U
       THEN
         ASSIGN ipoControl:ControlToken = "Disabled":U.
       ELSE 
         ASSIGN ipoControl:ControlToken = "Updatable":U.

       ipoControl:renderAsInput().

    END. /* WHEN "EntityCode":U THEN */


  END CASE.

  { mip/inc/mipcatcherror.i }

&ENDIF

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainer Procedure 
PROCEDURE defineContainer :
/*------------------------------------------------------------------------------
  Purpose   : Define basket/crosswalk specific containers
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  IF goWob:Mode = "Search":U THEN 
  DO:
    RUN defineContainerSearchFilter  IN TARGET-PROCEDURE.    
    RUN defineContainerSearchResults IN TARGET-PROCEDURE.   
  END. /*IF goWob:Mode = "Search":U THEN */
   
  IF goWob:Mode = "Maint":U THEN 
  DO:
    RUN defineContainerMaint IN TARGET-PROCEDURE.
    RUN defineContainerDetailsCopy IN TARGET-PROCEDURE.
    RUN defineContainerCopayDetailFilter  IN TARGET-PROCEDURE.
    RUN defineContainerCopayDetails  IN TARGET-PROCEDURE.
    RUN defineContainerAuditTrail IN TARGET-PROCEDURE.
    
    
  END. /*IF goWob:Mode = "Maint":U THEN */
      
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerAuditTrail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerAuditTrail Procedure 
PROCEDURE defineContainerAuditTrail :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/

  ASSIGN goCntAudit                = mipEnv:Health:maUtility:getAuditContainer() 
         goCntAudit:RowsToRender   = ?
         goCntAudit:ContainerWidth = "80%":U.

  { ma/inc/wsupdatetablecontainer.i &ContainerType  = "'TABLE'" 
                                    &UpdatableTable = FALSE
                                    &Container      = goCntAudit 
                                    &RenderSequence = 100}
    
  { mip/inc/mipcatcherror.i }   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerCopayDetailFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerCopayDetailFilter Procedure 
PROCEDURE defineContainerCopayDetailFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.

                                           
  ASSIGN
        goCntCopayDetailsFilter                  = NEW cls.mipwscontainer(goWob:ObjectCode + "CopayControlDetailsFilter":U, "65%":U, "":U, WarpSpeed:BaseClass, TRUE)
        goCntCopayDetailsFilter:ContainerMode    = goWob:SubmitValue
        goCntCopayDetailsFilter:ViewOnly         = FALSE
        goCntCopayDetailsFilter:Collapsable      = FALSE
        goCntCopayDetailsFilter:Collapsed        = FALSE
        goCntCopayDetailsFilter:ContainerTitle   = "Filter Details":U
        lSuccess                                   = goWob:setContainer("CopayControlDetailsRecordsFilter":U, goCntCopayDetailsFilter)
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("fcOwningEntityMnemonic":U + goCntCopayDetailsFilter:ContainerCode , "wsCombo":U, "10":U, "":U, "character":U, 1, 1,"Co-payment Entity:":U)
        oControl:ControlTooltip                    = "Select the entity to which a co-payment will apply.":U
        oControl:ControlClass                      = "'style='width:140px;":U
        oControl:AdditionalItems                   = "<All>=":U
        oControl:QueryString                       = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthCopayDetailEntities'":U
        oControl:KeyField                          = "mic_acronym.acronym_value":U
        oControl:DisplayFields                     = "mic_acronym.acronym_label":U
        oControl:JavascriptOnChange                = "fnGetControls(~"fcOwningCode" + goCntCopayDetailsFilter:ContainerCode + "~")[0].value=~"~";":U
                                                   + "fnGetControls(~"fdOwningObj" + goCntCopayDetailsFilter:ContainerCode + "~")[0].value=~"~";":U
                                                   + "fnGetControls(~"fcOwningKey" + goCntCopayDetailsFilter:ContainerCode + "~")[0].value=~"~";":U
       
        oControl                                   = goCntCopayDetailsFilter:addControl("fdEffectiveDate":U        + goCntCopayDetailsFilter:ContainerCode    , "wsInput":U       , "10":U  , "":U             , "date":U     , 1, 2, "Effective Date:":U)
        oControl:ControlTooltip                    = "Please enter an Effective Date to filter the Auth Copay Control Details by.":U
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("fdOwningObj":U            + goCntCopayDetailsFilter:ContainerCode    , "wsInput":U       , "10":U  , "":U             , "decimal":U  , 2, 1, "":U)
        oControl:ControlToken                      = "Hidden":U   
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("fcOwningKey":U            + goCntCopayDetailsFilter:ContainerCode    , "wsInput":U       , "10":U  , "":U             , "character":U, 2, 1, "":U)
        oControl:ControlToken                      = "Hidden":U       
                                                                                                                                                                                  
        oControl                                   = goCntCopayDetailsFilter:addControl("fcOwningCode":U           + goCntCopayDetailsFilter:ContainerCode    , "wsInput":U       , "15":U  , "":U             , "character":U, 2, 1, "":U)
        oControl:ControlTooltip                    = "Co-payment entity that this setup applies to."
        oControl:AjaxValidation                    = "START:" + goWob:ObjectCode + ":ajaxValidation:":U                                                                  
        oControl:FilterFields                      = "[TableMnemonic],[CodeField]":U                                                                                                              
        oControl:FilterControls                    = "fcOwningEntityMnemonic":U + goCntCopayDetailsFilter:ContainerCode 
                                                   + ",fcOwningCode":U + goCntCopayDetailsFilter:ContainerCode   
        oControl:ReturnFields                      = "[RecordObj],[RecordKey],[RecordCode]":U                                                                                   
        oControl:ReturnControls                    = "fdOwningObj":U  + goCntCopayDetailsFilter:ContainerCode + ",":U
                                                   + "fcOwningKey":U  + goCntCopayDetailsFilter:ContainerCode + ",":U        
                                                   + "fcOwningCode":U + goCntCopayDetailsFilter:ContainerCode 
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("buOwningBtn":U            + goCntCopayDetailsFilter:ContainerCode    , "wsLookupButton":U, "":U    , "":U             , "":U         , 2, 1, "Co-payment Entity Code:":U)
        oControl:CellLayoutMask                    = "&1&2&3&4":U
        oControl:LookupWobFLA                      = "slent":U
        oControl:LookupFields                      = "CODE_FIELD":U
        oControl:LookupControls                    = "fcOwningCode":U   + goCntCopayDetailsFilter:ContainerCode
        oControl:FilterFields                      = "QUERY_OEM":U
        oControl:FilterControls                    = "fcOwningEntityMnemonic":U + goCntCopayDetailsFilter:ContainerCode
        oControl:ReturnFields                      = "CODE_FIELD":U
        oControl:ReturnControls                    = "fcOwningCode":U + goCntCopayDetailsFilter:ContainerCode                                           
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("fdEndDate":U                 + goCntCopayDetailsFilter:ContainerCode , "wsInput":U, "10":U, "":U, "date":U,      2, 2,"End Date:":U)
        oControl:ControlToolTip                    = "Please enter an End Date to filter the Auth Copay Control Details by.":U
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("fcExclusionEntityMnemonic":U + goCntCopayDetailsFilter:ContainerCode , "wsCombo":U, "10":U, "":U, "character":U, 3, 1,"Exclusion Entity:":U)
        oControl:ControlTooltip                    = "Select the entity which will be excluded from any co-payment.":U
        oControl:ControlClass                      = "'style='width:140px;":U
        oControl:AdditionalItems                   = "<All>=All|<None>=":U
        oControl:ControlValue                      = "All":U
        oControl:QueryString                       = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthCopayDetailEntities'":U
        oControl:KeyField                          = "mic_acronym.acronym_value":U
        oControl:DisplayFields                     = "mic_acronym.acronym_label":U
        oControl:JavascriptOnChange                = "fnGetControls(~"fcExclusionCode" + goCntCopayDetailsFilter:ContainerCode + "~")[0].value=~"~";":U
                                                   + "fnGetControls(~"fdExclusionObj" + goCntCopayDetailsFilter:ContainerCode + "~")[0].value=~"~";":U
                                                   + "fnGetControls(~"fcExclusionKey" + goCntCopayDetailsFilter:ContainerCode + "~")[0].value=~"~";":U
        
        oControl                                   = goCntCopayDetailsFilter:addControl("fdExclusionObj":U            + goCntCopayDetailsFilter:ContainerCode    , "wsInput":U       , "10":U  , "":U             , "decimal":U, 4, 1, "":U)
        oControl:ControlToken                      = "Hidden":U   
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("fcExclusionKey":U            + goCntCopayDetailsFilter:ContainerCode    , "wsInput":U       , "10":U  , "":U             , "character":U, 4, 1, "":U)
        oControl:ControlToken                      = "Hidden":U       
                                                                                                                                                                                  
        oControl                                   = goCntCopayDetailsFilter:addControl("fcExclusionCode":U           + goCntCopayDetailsFilter:ContainerCode    , "wsInput":U       , "15":U  , "":U             , "character":U, 4, 1, "":U)
        oControl:ControlTooltip                    = "Co-payment exclusion that this setup applies to."
        oControl:AjaxValidation                    = "START:" + goWob:ObjectCode + ":ajaxValidation:":U                                                                  
        oControl:FilterFields                      = "[TableMnemonic],[CodeField]":U                                                                                                              
        oControl:FilterControls                    = "fcExclusionEntityMnemonic":U + goCntCopayDetailsFilter:ContainerCode + ",fcExclusionCode":U + goCntCopayDetailsFilter:ContainerCode                                                                                                         
        oControl:ReturnFields                      = "[RecordObj],[RecordKey],[RecordCode]":U                                                                                   
        oControl:ReturnControls                    = "fdExclusionObj":U  + goCntCopayDetailsFilter:ContainerCode + ",":U
                                                   + "fExclusioncKey":U  + goCntCopayDetailsFilter:ContainerCode + ",":U        
                                                   + "fcExclusionCode":U + goCntCopayDetailsFilter:ContainerCode 
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("buExclusionOwningBtn":U            + goCntCopayDetailsFilter:ContainerCode    , "wsLookupButton":U, "":U    , "":U , "":U         , 4, 1, "Exclusion Entity Code:":U)
        oControl:CellLayoutMask                    = "&1&2&3&4":U
        oControl:LookupWobFLA                      = "slent":U
        oControl:LookupFields                      = "CODE_FIELD":U
        oControl:LookupControls                    = "fcExclusionCode":U   + goCntCopayDetailsFilter:ContainerCode
        oControl:FilterFields                      = "QUERY_OEM":U
        oControl:FilterControls                    = "fcExclusionEntityMnemonic":U + goCntCopayDetailsFilter:ContainerCode
        oControl:ReturnFields                      = "CODE_FIELD":U
        oControl:ReturnControls                    = "fcExclusionCode":U + goCntCopayDetailsFilter:ContainerCode                                           
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("fiRecords":U         + goCntCopayDetailsFilter:ContainerCode , "wsInput":U, "10":U, "":U, "integer":U, 4, 2,"Records:":U)
        oControl:SpanOverLabel                     = FALSE
        oControl:CellClass                         = WarpSpeed:BaseClass + "DetailsFilter":U
        oControl:ControlValue                      = "50":U
                                                   
                                                   
        oControl                                   = goCntCopayDetailsFilter:addControl("frmButtonBar":U, "":U, "":U, "":U, "":U, 6,2)
        oControl:SpanOverLabel                     = TRUE
        oControl:CellClass                         = WarpSpeed:BaseClass + "ButtonBar":U
        oControl:CellSnippet                       = "align='right'":U
        oControl:SubContainer                      = wsUiService:getButtonContainer(goCntCopayDetailsFilter:ContainerCode + "BtnBar":U, "Filter Results=Filter":U)
        oControl:AllowDoubleSubmit                 = TRUE.

  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'FORM'" &UpdatableTable = TRUE &Container = goCntCopayDetailsFilter }
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerCopayDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerCopayDetails Procedure 
PROCEDURE defineContainerCopayDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
  
  
&IF {&DBDFMA} >= 010195 &THEN
  
  ASSIGN 
    goCntCopayDetails                        = NEW cls.mipwscontainer("CopayDetails":U + goWob:ObjectCode, "100%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntCopayDetails:ContainerMode          = goWob:SubmitValue
    goCntCopayDetails:ShowGenericReportPrint = TRUE
    goCntCopayDetails:Collapsed              = FALSE
    goCntCopayDetails:ViewOnly               = NOT glEnquiryMode  
    goCntCopayDetails:RowsToRender           = ?
    goCntCopayDetails:RowRenderProcedure     = "rowRenderProcedure":U
    goCntCopayDetails:RowRenderArgument      = goCntCopayDetails:ContainerCode
    goCntCopayDetails:DefaultContainerType   = "TABLE":U                                                                                                                                                           
    goCntCopayDetails:ContainerTitle         = "Co-Payment Details":U  
    goCntCopayDetails:QueryString            = "FOR EACH tt_auth_copay_detail NO-LOCK BY tt_auth_copay_detail.line_number":U  

    oControl                        = goCntCopayDetails:addControl("fdObj":U                   + goCntCopayDetails:ContainerCode , "wsInput":U  , "5":U , "tt_auth_copay_detail.auth_copay_detail_obj":U   , "decimal":U  , 2, "":U)
    oControl:ControlToken           = "Hidden":U                                                                                                                                           
                                    
    oControl                        = goCntCopayDetails:addControl("fdCopayControlObjArgument":U + goCntCopayDetails:ContainerCode , "wsInput":U  , "5":U , "tt_auth_copay_detail.auth_copay_control_obj":U  , "decimal":U  , 3, "":U)
    oControl:ControlToken           = "Hidden":U

    oControl                        = goCntCopayDetails:addControl("fcOwningEntityMnemonic":U + goCntCopayDetails:ContainerCode , "wsCombo":U, "10":U, "tt_auth_copay_detail.owning_entity_mnemonic":U, "character":U, 4,"Co-payment</br>Entity":U)
    oControl:ControlTooltip         = "Select the entity to which a co-payment will apply'.":U
    oControl:ControlClass           = "+clMan":U
    oControl:AdditionalItems        = "<None>=":U
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthCopayDetailEntities'":U
    oControl:KeyField               = "mic_acronym.acronym_value":U
    oControl:DisplayFields          = "mic_acronym.acronym_label":U
    oControl:JavascriptOnChange     = "fnOnChangeOwningEntity(this);":U

    oControl                        = goCntCopayDetails:addControl("fdOwningObj":U            + goCntCopayDetails:ContainerCode    , "wsInput":U       , "10":U  , "tt_auth_copay_detail.owning_obj":U             , "decimal":U, 5, "":U)
    oControl:ControlToken           = "Hidden":U   
                                    
    oControl                        = goCntCopayDetails:addControl("fcOwningKey":U            + goCntCopayDetails:ContainerCode    , "wsInput":U       , "10":U  , "tt_auth_copay_detail.owning_key":U             , "character":U, 5, "":U)
    oControl:ControlToken           = "Hidden":U       
                                                                                                                                                                   
    oControl                        = goCntCopayDetails:addControl("fcOwningCode":U           + goCntCopayDetails:ContainerCode    , "wsInput":U       , "15":U  , "tt_auth_copay_detail.owning_alt_value":U       , "character":U, 5, "Co-payment</br>Entity</br>Code":U)
    oControl:ControlTooltip         = "Co-payment entity that this setup applies to."
    oControl:ControlClass           = "+clMan":U
    oControl:AjaxValidation         = "START:" + goWob:ObjectCode + ":ajaxValidation:":U                                                                  
    oControl:FilterFields           = "[TableMnemonic],[CodeField]":U                                                                                                              
    oControl:FilterControls         = "fcOwningEntityMnemonic":U + goCntCopayDetails:ContainerCode + ",fcOwningCode":U + goCntCopayDetails:ContainerCode                                                                                                         
    oControl:ReturnFields           = "[RecordObj],[RecordKey],[RecordCode],[RecordDescription]":U                                                                                   
    oControl:ReturnControls         = "fdOwningObj":U  + goCntCopayDetails:ContainerCode + ",":U
                                    + "fcOwningKey":U  + goCntCopayDetails:ContainerCode + ",":U        
                                    + "fcOwningCode":U + goCntCopayDetails:ContainerCode + ",":U 
                                    + "fcCopaymentDescription":U + goCntCopayDetails:ContainerCode
                                    
    oControl                        = goCntCopayDetails:addControl("buOwningBtn":U            + goCntCopayDetails:ContainerCode    , "wsLookupButton":U, "":U    , "":U                                           , "":U         , 5)
    oControl:CellLayoutMask         = "&1&2&3&4":U
    oControl:LookupWobFLA           = "slent":U
    oControl:LookupFields           = "CODE_FIELD":U
    oControl:LookupControls         = "fcOwningCode":U   + goCntCopayDetails:ContainerCode
    oControl:FilterFields           = "QUERY_OEM,CODE_FIELD":U
    oControl:FilterControls         = "fcOwningEntityMnemonic":U + goCntCopayDetails:ContainerCode + ",":U
                                    + "fcOwningCode":U + goCntCopayDetails:ContainerCode
    oControl:ReturnFields           = "CODE_FIELD":U
    oControl:ReturnControls         = "fcOwningCode":U + goCntCopayDetails:ContainerCode       

    oControl                        = goCntCopayDetails:addControl("fcCopaymentDescription":U           + goCntCopayDetails:ContainerCode    , "wsTextArea":U       , "15":U  , "":U       , "character":U, 6, "Co-payment</br>Entity</br>Description":U)
    oControl:ControlToken           = "Disabled":U       
    oControl:RenderProcedure        = "customRenderProcedure":U  
    oControl:RenderArgument         = "Description":U

    oControl                        = goCntCopayDetails:addControl("fcExclusionEntityMnemonic":U + goCntCopayDetails:ContainerCode , "wsCombo":U, "10":U, "tt_auth_copay_detail.exclusion_entity_mnemonic":U, "character":U, 7,"Exclusion</br>Entity":U)
    oControl:ControlTooltip         = "Select the entity which will be excluded from any co-payment.":U
    oControl:AdditionalItems        = "<None>=":U
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthCopayDetailEntities'":U
    oControl:KeyField               = "mic_acronym.acronym_value":U
    oControl:DisplayFields          = "mic_acronym.acronym_label":U
    oControl:JavascriptOnChange     = "fnOnChangeOwningEntity(this);":U
    
    oControl                        = goCntCopayDetails:addControl("fdExclusionObj":U            + goCntCopayDetails:ContainerCode    , "wsInput":U       , "10":U  , "tt_auth_copay_detail.exclusion_obj":U             , "decimal":U, 8, "":U)
    oControl:ControlToken           = "Hidden":U   
                                    
    oControl                        = goCntCopayDetails:addControl("fcExclusionKey":U            + goCntCopayDetails:ContainerCode    , "wsInput":U       , "10":U  , "tt_auth_copay_detail.exclusion_key":U             , "character":U, 8, "":U)
    oControl:ControlToken           = "Hidden":U       
                                                                                                                                                                   
    oControl                        = goCntCopayDetails:addControl("fcExclusionCode":U           + goCntCopayDetails:ContainerCode    , "wsInput":U       , "15":U  , "tt_auth_copay_detail.exclusion_alt_value":U       , "character":U, 8, "Exclusion</br>Entity</br>Code":U)
    oControl:ControlTooltip         = "Co-payment exclusion that this setup applies to."
    oControl:RenderProcedure        = "customRenderProcedure":U  
    oControl:RenderArgument         = "EntityCode":U
    oControl:AjaxValidation         = "START:" + goWob:ObjectCode + ":ajaxValidation:":U                                                                  
    oControl:FilterFields           = "[TableMnemonic],[CodeField]":U                                                                                                              
    oControl:FilterControls         = "fcExclusionEntityMnemonic":U + goCntCopayDetails:ContainerCode + ",fcExclusionCode":U + goCntCopayDetails:ContainerCode                                                                                                         
    oControl:ReturnFields           = "[RecordObj],[RecordKey],[RecordCode],[RecordDescription]":U                                                                                   
    oControl:ReturnControls         = "fdExclusionObj":U  + goCntCopayDetails:ContainerCode + ",":U
                                    + "fcExclusionKey":U  + goCntCopayDetails:ContainerCode + ",":U        
                                    + "fcExclusionCode":U + goCntCopayDetails:ContainerCode + ",":U
                                    + "fcExclusionDescription":U + goCntCopayDetails:ContainerCode
                                    
    oControl                        = goCntCopayDetails:addControl("buExclusionBtn":U            + goCntCopayDetails:ContainerCode    , "wsLookupButton":U, "":U    , "":U , "":U         , 8)
    oControl:CellLayoutMask         = "&1&2&3&4":U
    oControl:LookupWobFLA           = "slent":U
    oControl:LookupFields           = "CODE_FIELD":U
    oControl:LookupControls         = "fcExclusionCode":U   + goCntCopayDetails:ContainerCode
    oControl:FilterFields           = "QUERY_OEM,CODE_FIELD":U
    oControl:FilterControls         = "fcExclusionEntityMnemonic":U + goCntCopayDetails:ContainerCode + ",":U
                                    + "fcExclusionCode":U + goCntCopayDetails:ContainerCode
    oControl:ReturnFields           = "CODE_FIELD":U
    oControl:ReturnControls         = "fcExclusionCode":U + goCntCopayDetails:ContainerCode  

    oControl                        = goCntCopayDetails:addControl("fcExclusionDescription":U           + goCntCopayDetails:ContainerCode    , "wsTextArea":U       , "15":U  , "":U       , "character":U, 9, "Exclusion</br>Entity</br>Description":U)                                                                                                                                                  
    oControl:ControlToken           = "Disabled":U
    oControl:RenderProcedure        = "customRenderProcedure":U  
    oControl:RenderArgument         = "Description":U

    oControl                        = goCntCopayDetails:addControl("fdEffectiveDate":U         + goCntCopayDetails:ContainerCode , "wsInput":U  , "10":U , "tt_auth_copay_detail.effective_date":U  , "date":U      , 10, "Effective<br>Date":U)
    oControl:ControlClass           = "+clMan":U
    oControl:ControlTooltip         = "Please enter an effective date":U
    
    oControl                        = goCntCopayDetails:addControl("fdEndDate":U               + goCntCopayDetails:ContainerCode , "wsInput":U  , "10":U , "tt_auth_copay_detail.end_date":U        , "date":U      , 11, "End<br>Date":U)
    oControl:ControlTooltip         = "Please enter an End Date":U
    
    . 

  ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(goCntCopayDetails).

  /*if we are in copy screen we will render the details container as a non updatable container*/
  IF glCopyScreen OR goCntCopayDetails:ContainerMode = "Copy":U THEN DO:
    { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = FALSE &Container = goCntCopayDetails &ContainerProperties = oContainerProperties} .
  END.
  ELSE DO:
    { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE &Container = goCntCopayDetails &ContainerProperties  = oContainerProperties} .
  END.

 { mip/inc/mipcatcherror.i }                                                                                                                                         

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerDetailsCopy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerDetailsCopy Procedure 
PROCEDURE defineContainerDetailsCopy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------
*/
DEFINE VARIABLE oControl AS cls.mipwscontrol          NO-UNDO.
 
ASSIGN 
    goCntDetailsCopy                        = NEW cls.mipwscontainer("CopyDetails":U + goWob:ObjectCode, "10%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntDetailsCopy:ContainerMode          = goWob:SubmitValue
    goCntDetailsCopy:ViewOnly               = NO
    goCntDetailsCopy:RowsToRender           = ?
    goCntDetailsCopy:RowRenderProcedure     = "rowRenderProcedure":U
    goCntDetailsCopy:RowRenderArgument      = goCntDetailsCopy:ContainerCode
    goCntDetailsCopy:DefaultContainerType   = "FORM":U  
    goCntDetailsCopy:Collapsable            = FALSE
    goCntDetailsCopy:Borders                = "FALSE"
    oControl                                = goCntDetailsCopy:addControl("flCopyDetails":U + goCntDetailsCopy:ContainerCode , "wsCheckBox":U  , "20":U , "":U   , "logical":U  , 1,1, "Copy Details Lines:":U)
    oControl:CellSnippet                    = "align='center'":U
    oControl                                = goCntDetailsCopy:addControl("fdObj":U         + goCntDetailsCopy:ContainerCode , "wsInput":U  , "5":U , "":U   , "decimal":U  , 1,1, "Copy Details Lines:":U)
    oControl:ControlToken                   = "Hidden":U 
    oControl:CellLayoutMask                 = "&1&2"
    oControl:controlValue                   = goWob:CurrentObj WHEN NOT Warpspeed:ValidationError
    .
  
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'FORM'" &Container = goCntDetailsCopy }.

  { mip/inc/mipcatcherror.i }                                                                                                                                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerMaint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerMaint Procedure 
PROCEDURE defineContainerMaint :
/*------------------------------------------------------------------------------
  Purpose:Define Mait Containers     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 010195 &THEN  
  DEFINE VARIABLE lSuccess       AS LOGICAL            NO-UNDO. 
  DEFINE VARIABLE oContainer     AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oControl  AS cls.mipwscontrol   NO-UNDO.
 
  ASSIGN
    goCntMaint                     = goWob:getContainer("Maint":U) 
    goCntMaint:ContainerTitle      = "Authorisation Co-Payment Control Maintenance":U
    goCntMaint:ContainerWidth      = "70%":U

    goCntMaint:QueryString           = "FOR EACH tt_auth_copay_control NO-LOCK ":U
                                       + "   WHERE tt_auth_copay_control.auth_copay_control_obj = '&1',":U
                                       + "   FIRST tt_insurer NO-LOCK":U
                                       + "   WHERE tt_insurer.insurer_obj = tt_auth_copay_control.insurer_obj OUTER-JOIN":U
                                       + "      BY tt_auth_copay_control.line_number BY tt_auth_copay_control.effective_date DESCENDING":U
                                                
    oControl                           = goCntMaint:addControl("fcProviderType":U  + goCntMaint:ContainerCode , "wsCombo":U       , "10":U, "tt_auth_copay_control.provider_type":U, "character":U  , 1,1, "Provider Type":U)
    oControl:ControlToolTip            = "Please select a provider type":U
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthProviderType'":U
    oControl:KeyField                  = "mic_acronym.acronym_code":U
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:AdditionalItems           = "<None>=":U
    
    oControl                           = goCntMaint:addControl("fdObj":U                           + goCntMaint:ContainerCode  , "wsInput":U        , "20":U  , "tt_auth_copay_control.auth_copay_control_obj":U , "character":U, 2,1, "":U)
    oControl:ControlToken              = "Hidden":U        
    
    oControl                           = goCntMaint:addControl("fcAuthCopayTypeArgumentMnemonic":U + goCntMaint:ContainerCode  , "wsInput":U       , "5":U     , "":U                                        , "character":U, 2,1, "":U)        
    oControl:ControlToken              = "Hidden":U                                                                                                                                                                                                     
    oControl:ControlValue              = "hamct":U

    oControl                           = goCntMaint:addControl("fcAuthCopayTypeArgumentField":U    + goCntMaint:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,2,1, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                                          
    oControl:ControlValue              = "[CodeField]":U                                                                                                                                                                     
                                                                                                                                                                                                                      
    oControl                           = goCntMaint:addControl("fdAuthCopayTypeObj":U              + goCntMaint:ContainerCode  , "wsInput":U        , "1":U   , "tt_auth_copay_control.auth_copay_type_obj":U      , "decimal":U  ,2,1, "":U)
    oControl:ControlToken              = "Hidden":U

    oControl                           = goCntMaint:addControl("fcAuthCopayType":U                 + goCntMaint:ContainerCode  , "wsInput":U        , "8":U   , "":U                                 , "character":U,  2,1, "Co-payment Type:":U)
    oControl:ControlTooltip            = "Co-payment type that this setup applies to":U 
    oControl:RenderProcedure           = "customRenderProcedure":U  
    oControl:RenderArgument            = "CopayType":U
    oControl:AjaxValidation            = "START:" + goWob:ObjectCode + ":ajaxValidationProcedures:AuthCopayType":U
    oControl:FilterFields              = "[CopayType]":U
    oControl:FilterControls            = "fcAuthCopayType":U + goCntMaint:ContainerCode 
    oControl:ReturnFields              = "[CopayTypeObj],[CopayType]":U
    oControl:ReturnControls            = "fdAuthCopayTypeObj":U + goCntMaint:ContainerCode + ",":U 
                                       + "fcAuthCopayType":U + goCntMaint:ContainerCode 

    oControl                           = goCntMaint:addControl("buAuthCopayTypeBtn":U  + goCntMaint:ContainerCode , "wsLookupButton":U, "":U  , "":U, "":U         , 2,1 , "Co-payment Type:":U)
    oControl:CellLayoutMask            = "&1&2&3&4&5&6":U                                        
    oControl:LookupWobFla              = "hamct":U
    oControl:LookupFields              = "ham_auth_copay_type.auth_copay_type_obj":U
    oControl:LookupControls            = "fdAuthCopayTypeObj":U + goCntMaint:ContainerCode
    oControl:FilterFields              = "ham_auth_copay_type.auth_copay_type":U
    oControl:FilterControls            = "fcAuthCopayType":U + goCntMaint:ContainerCode
    oControl:ReturnFields              = "ham_auth_copay_type.auth_copay_type_obj,ham_auth_copay_type.auth_copay_type":U
    oControl:ReturnControls            = "fdAuthCopayTypeObj":U  + goCntMaint:ContainerCode + ",":U
                                       + "fcAuthCopayType":U + goCntMaint:ContainerCode 


    oControl                           = goCntMaint:addControl("fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode  , "wsInput":U       , "5":U     , "":U                                        , "character":U, 1,2, "":U)        
    oControl:ControlToken              = "Hidden":U                                                                                                                                                                                                     
    oControl:ControlValue              = "ermin":U

    oControl                           = goCntMaint:addControl("fcInsurerArgumentField":U    + goCntMaint:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,1,2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                                          
    oControl:ControlValue              = "[CodeField]":U                                                                                                                                                                     
                                                                                                                                                                                                                      
    oControl                           = goCntMaint:addControl("fdInsurerObj":U              + goCntMaint:ContainerCode  , "wsInput":U        , "1":U   , "tt_insurer.insurer_obj":U                         , "decimal":U  ,1,2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                                                     
                                                                                                                                                                                                                      
    oControl                           = goCntMaint:addControl("fcInsurer":U                 + goCntMaint:ContainerCode  , "wsInput":U        , "10":U   , "tt_insurer.insurer_code":U                        , "character":U,1,2, "":U)
    oControl:ControlTooltip            = "Please enter a valid client":U                                                                                                                                       
    oControl:AjaxValidation            = "SERVICE:wsUIService:ajaxValidation:":U                                                                                                                              
    oControl:FilterFields              = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls            = "fcInsurer":U + goCntMaint:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode + ",fcInsurerArgumentField":U + goCntMaint:ContainerCode
    oControl:ReturnFields              = "[RecordObj]":U
    oControl:ReturnControls            = "fdInsurerObj":U + goCntMaint:ContainerCode    
    
    oControl                           = goCntMaint:addControl("buInsurerBtn":U              + goCntMaint:ContainerCode  , "wsLookupButton":U , "":U    , "":U                                               , "":U         ,1,2, "Client:":U)
    oControl:LookupWobFLA              = "ermin":U                                                                                                                                                              
    oControl:LookupFields              = "erm_insurer.insurer_obj":U                                                                                                                                           
    oControl:LookupControls            = "fdInsurerObj":U + goCntMaint:ContainerCode                                                                                                                                                            
    oControl:FilterFields              = "erm_insurer.insurer_code":U                                                                                                                                           
    oControl:FilterControls            = "fcInsurer":U + goCntMaint:ContainerCode                                                                                                                                                            
    oControl:ReturnFields              = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U                                                                                                                   
    oControl:ReturnControls            = "fdInsurerObj":U + goCntMaint:ContainerCode + ",fcInsurer":U + goCntMaint:ContainerCode                                                                                             
    oControl:CellLayoutMask            = "&1&2&3&4&5":U
    
    oControl                           = goCntMaint:addControl("flApplyToPMB":U             + goCntMaint:ContainerCode, "wsCombo":U, "18":U, "tt_auth_copay_control.apply_to_pmb":U                        , "logical":U         , 3,1 , "Apply to PMB:":U)
    oControl:AdditionalItems           = "|No|Yes":U
    oControl:ControlToolTip            = "Indicates whether the co-payment must apply if the main diagnosis is a PMB."
    
     oControl                          = goCntMaint:addControl("fcOptionArgumentMnemonic":U  + goCntMaint:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,2,2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                                        
    oControl:ControlValue              = "scheme":U                                                                                                                                                                        
                                                                                                                                                                                                                    
    oControl                           = goCntMaint:addControl("fcOptionArgumentField":U     + goCntMaint:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,2,2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                             
    oControl:ControlValue              = "[CodeField]":U 
                                                                                                                    
    oControl                           = goCntMaint:addControl("fiOptionCode":U              + goCntMaint:ContainerCode  , "wsInput":U        , "10":U   , "tt_auth_copay_control.option_code":U             , "integer":U  ,2,2, "":U)                                                                                                                                                                    
    oControl:ControlClass              = "+clNumericOnly +clMaxLength:3":U
    oControl:ControlTooltip            = "Please enter a valid Option":U
    oControl:RenderProcedure           = "customRenderProcedure":U  
    oControl:RenderArgument            = "OptionCode":U
    oControl:AjaxValidation            = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields              = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls            = "fiOptionCode":U + goCntMaint:ContainerCode
                                       + ",fcOptionArgumentMnemonic":U + goCntMaint:ContainerCode 
                                       + ",fcOptionArgumentField":U + goCntMaint:ContainerCode
     
    oControl                           = goCntMaint:addControl("buOptionBtn":U               + goCntMaint:ContainerCode  , "wsLookupButton":U , "":U    , "":U                                               , "":U         ,2,2, "Option:":U)
    oControl:LookupWobFLA              = "scheme":U                                                                                                                                                                                                                                                                                
    oControl:CellLayoutMask            = "&1&2&3&4":U                                                                                                                                                           
    oControl:LookupFields              = "scheme.scheme-code":U                                                                                                                                                 
    oControl:LookupControls            = "fiOptionCode":U + goCntMaint:ContainerCode                                                                                                                    
    oControl:FilterFields              = "scheme.scheme-code":U                                                                                                                                                 
    oControl:FilterControls            = "fiOptionCode":U + goCntMaint:ContainerCode                                                                                                                    
    oControl:ReturnFields              = "scheme.scheme-code":U                                                                                                                                                 
    oControl:ReturnControls            = "fiOptionCode":U + goCntMaint:ContainerCode 

    oControl                           = goCntMaint:addControl("flApplyToEmergency":U          + goCntMaint:ContainerCode, "wsCombo":U, "18":U, "tt_auth_copay_control.apply_to_emergency":U                     , "logical":U  , 4,1 , "Apply to Emergency:":U)
    oControl:AdditionalItems           = "|No|Yes":U
    oControl:ControlToolTip            = "Indicates whether the co-payment must apply if the authorisation is an emergency."
    
    oControl                           = goCntMaint:addControl("fdEffectiveDate":U             + goCntMaint:ContainerCode, "wsInput":U, "10":U, "tt_auth_copay_control.effective_date":U                            , "date":U     , 3,2 , "Effective Date:":U)
    oControl:ControlToolTip            = "Please enter an effective date"
    
    oControl                           = goCntMaint:addControl("fcCopaymentValueType":U        + goCntMaint:ContainerCode, "wsCombo":U, "10":U, "tt_auth_copay_control.copayment_value_type":U                   , "logical":U,   5,1 , "Value Type:":U)
    oControl:ControlToolTip            = "Specify a Co-payment type (Rand or Percentage) which apply to the Value"
    oControl:AdditionalItems           = "None=?|Rand=R|Percentage=P":U
    oControl:RenderProcedure           = "customRenderProcedure":U  
    oControl:RenderArgument             = "CopayValueType":U

    oControl                           = goCntMaint:addControl("fdEndDate":U                    + goCntMaint:ContainerCode, "wsInput":U, "10":U, "tt_auth_copay_control.end_date":U                                  , "date":U,      4,2 , "End Date:":U)

    
    oControl                           = goCntMaint:addControl("fcCopaymentValue":U            + goCntMaint:ContainerCode, "wsInput":U, "10":U, "tt_auth_copay_control.copayment_value":U                           , "decimal":U  , 6,1 , "Value:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:javascriptOnchange        = "this.value = parseFloat(this.value).toFixed(2);"
    oControl:ControlToolTip            = "Rand or percentage amount that must be applied as the co-payment."
    
    oControl                           = goCntMaint:addControl("fcOverrideReasonsTypeArgument":U + goCntMaint:ContainerCode  , "wsInput":U            , "5":U                  , "":U           , "character":U, 5,2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                 
    oControl:ControlValue              = "AO*":U
    oControl:ControlClass              = "+clPreserveValue":U
        
    oControl                           = goCntMaint:addControl("fcOverrideReasonsKeys":U + goCntMaint:ContainerCode  , "wsInput":U       , "8":U     , "tt_auth_copay_control.copay_apply_override_reasons":U           , "character":U, 5,2, "Override Reasons:":U)        
    oControl:ControlToken              = "Hidden":U 
    oControl:JavaScriptOnChange        = 'if(this.value!="")~{var x = "fcOverrideReasons'      + goCntMaint:ContainerCode + '";'                            
                                       + ' fnGetControls(x)[0].value="~[MULTIPLE SELECTED~]";':U
                                       + ' fnGetControls(x)[0].title="Multiple override reasons selected ? please select the lookup for more details.";'
                                       + '~}else~{var x = "fcOverrideReasons'      + goCntMaint:ContainerCode + '";'                            
                                       + ' fnGetControls(x)[0].value="";':U
                                       + ' fnGetControls(x)[0].title="Please specify the co-payment override reasons for which a co-payment must apply";'
                                       + '~}'
                                                                
    oControl                           = goCntMaint:addControl("fcOverrideReasons":U + goCntMaint:ContainerCode  , "wsTextArea":U       , "10,3":U     , "tt_auth_copay_control.copay_apply_override_reasons":U           , "character":U, 5,2, "Override Reasons":U)        
    oControl:renderProcedure           = "customRenderProcedure":U
    oControl:RenderArgument            = "OverrideReasons":U

    oControl                           = goCntMaint:addControl("buOverrideReasonsMsc":U + goCntMaint:ContainerCode  , "wsMsComboButton":U       , "":U     , "":U                                        , "":U, 5,2, "Override Reasons:":U)
    oControl:CellLayoutMask            = "&1&2&3&4":U
    oControl:LookupWobFLA              = "note":U     
    oControl:LookupFields              = "note.key":U                                                                                                                                                 
    oControl:LookupControls            = "fcOverrideReasonsKeys":U + goCntMaint:ContainerCode                                                                                                                         
    oControl:FilterFields              = "note.type,note.key":U                                                                                                                                       
    oControl:FilterControls            = "fcOverrideReasonsTypeArgument":U + goCntMaint:ContainerCode + ",":U
                                       + "fcOverrideReasonsKeys":U + goCntMaint:ContainerCode
  
    oControl                           = goCntMaint:addControl("fcAuthStatus":U + goCntMaint:ContainerCode , "wsCombo":U            , "5":U                  , "tt_auth_copay_control.auth_status":U           , "character":U, 7,1, "Auth Status:":U)
    oControl:ControlTooltip            = "The status that will be assigned by default to the provider, if a co-payment applies.":U
    oControl:RenderProcedure           = "customRenderProcedure":U  
    oControl:RenderArgument            = "AuthStatus":U
    
    oControl                           = goCntMaint:addControl("fcWarningType":U               + goCntMaint:ContainerCode, "wsCombo":U, "18":U, "tt_auth_copay_control.warning_message_type":U                              , "character":U, 6,2 , "Warning Type:":U)
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                                                                                                    
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthCopayWarnMessageType:=":U
    oControl:ControlTooltip            = "Specify the warning message type."

    oControl                           = goCntMaint:addControl("fcWarningMessage":U + goCntMaint:ContainerCode  , "wsTextArea":U       , "10,3":U     , "tt_auth_copay_control.warning_message":U         , "character":U, 7,2, "Warning Message:":U)        
    oControl:ControlTooltip            = "Message that will be displayed to the user when a provider outside the co-payment group is used.":U
    
    oControl                           = goCntMaint:addControl("fcAuthStatusNoteTypeArgument":U + goCntMaint:ContainerCode  , "wsInput":U            , "5":U                  , "":U           , "character":U, 8,1, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                 
    oControl:ControlValue              = "AS":U
    oControl:ControlClass              = "+clPreserveValue":U
    
    oControl                           = goCntMaint:addControl("fcAuthStatusNote":U + goCntMaint:ContainerCode  , "wsInput":U            , "5":U                  , "tt_auth_copay_control.auth_status_note":U           , "character":U, 8,1, "Status Note:":U)
    oControl:ControlTooltip            = "The status that will be assigned by default to the provider, if a co-payment applies.":U
    oControl:RenderProcedure           = "customRenderProcedure":U                                                                                                                                                                                                                    
    oControl:RenderArgument            = "StatusNote":U
    
    oControl                           = goCntMaint:addControl("fcAuthStatusNoteDesc":U            + goCntMaint:ContainerCode, "wsInput":U       , "5":U , "":U                                     , "character":U, 8,1, "":U)            
    oControl:ControlToken              = "Hidden":U 
    oControl:JavaScriptOnChange        = 'fnGetControls("fcAuthStatusNote' + goCntMaint:ContainerCode + '")[0].title = this.value;':U
    

    
    oControl                           = goCntMaint:addControl("buAuthStatusNoteBtn":U             + goCntMaint:ContainerCode, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 8,1, "Status Note:":U)                                                                                                                                                                                                   
    oControl:CellLayoutMask            = "&1&2&3&4":U                                                                                                                                                   
    oControl:LookupWobFLA              = "note":U                                                                                                                                                     
    oControl:LookupFields              = "note.key":U                                                                                                                                                 
    oControl:LookupControls            = "fcAuthStatusNote":U + goCntMaint:ContainerCode                                                                                                                              
    oControl:FilterFields              = "note.key,note.type":U                                                                                                                                       
    oControl:FilterControls            = "fcAuthStatusNote":U + goCntMaint:ContainerCode + ",":U
                                       + "fcAuthStatusNoteTypeArgument":U + goCntMaint:ContainerCode                                                                                     
    oControl:ReturnFields              = "note.key,note.narration[1]":U                                                                                                                                           
    oControl:ReturnControls            = "fcAuthStatusNote":U + goCntMaint:ContainerCode + ",fcAuthStatusNoteDesc":U + goCntMaint:ContainerCode
    oControl:JavascriptOnClick         = 'fnGetControls("fcAuthStatusNoteTypeArgument' + goCntMaint:ContainerCode + '")[0].value = "AS" + fnGetControls("fcAuthStatus' + goCntMaint:ContainerCode + '")[0].value;':U
    
    
  
  .
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerSearchFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerSearchFilter Procedure 
PROCEDURE defineContainerSearchFilter :
/*------------------------------------------------------------------------------
  Purpose   : Define search result containers
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/ 
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN
  
  /* SearchFilter */
  ASSIGN
    goCntSearchFilter              = goWob:getContainer("SearchFilter":U) 

    oControl                       = goCntSearchFilter:addControl("fcProviderType":U  + goCntSearchFilter:ContainerCode , "wsCombo":U       , "10":U, "":U, "character":U  , 2 , 1, "Provider Type:":U)
    oControl:ControlToolTip        = "Please select a provider type":U
    oControl:QueryString           = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthProviderType'":U
    oControl:KeyField              = "mic_acronym.acronym_code":U
    oControl:DisplayFields         = "mic_acronym.acronym_label":U
    oControl:AdditionalItems       = "<None>=":U
    oControl:ControlValue          = "None":U
    
    oControl                       = goCntSearchFilter:addControl("fdAuthCopayTypeObj":U  + goCntSearchFilter:ContainerCode , "wsInput":U       , "10":U, "":U, "decimal":U  , 3 , 1, "":U)
    oControl:ControlToken          = "Hidden":U                                                      
                                                                                          
    oControl                       = goCntSearchFilter:addControl("fcAuthCopayType":U     + goCntSearchFilter:ContainerCode , "wsInput":U       , "10":U, "":U, "character":U, 3 , 1, "":U)
    oControl:ControlToolTip        = "Co-payment type that this setup applies to.":U
    oControl:AjaxValidation        = "START:" + goWob:ObjectCode + ":ajaxValidationProcedures:AuthCopayType":U
    oControl:FilterFields          = "[CopayType]":U
    oControl:FilterControls        = "fcAuthCopayType":U + goCntSearchFilter:ContainerCode 
    oControl:ReturnFields          = "[CopayTypeObj],[CopayType],[Description]":U
    oControl:ReturnControls        = "fdAuthCopayTypeObj":U + goCntSearchFilter:ContainerCode + ",":U 
                                   + "fcAuthCopayType":U + goCntSearchFilter:ContainerCode + ",":U
                                   + "fcAuthCopayDesc":U + goCntSearchFilter:ContainerCode

    oControl                       = goCntSearchFilter:addControl("buAuthCopayTypeBtn":U  + goCntSearchFilter:ContainerCode , "wsLookupButton":U, "":U  , "":U, "":U         , 3 , 1, "":U)
    oControl:CellLayoutMask        = "&1&2&3 &4":U                                        
    oControl:LookupWobFla          = "hamct":U
    oControl:LookupFields          = "ham_auth_copay_type.auth_copay_type_obj":U
    oControl:LookupControls        = "fdAuthCopayTypeObj":U + goCntSearchFilter:ContainerCode
    oControl:ReturnFields          = "ham_auth_copay_type.auth_copay_type_obj,ham_auth_copay_type.auth_copay_type,ham_auth_copay_type.description":U
    oControl:ReturnControls        = "fdAuthCopayTypeObj":U  + goCntSearchFilter:ContainerCode + ",":U
                                   + "fcAuthCopayType":U + goCntSearchFilter:ContainerCode + ",":U
                                   + "fcAuthCopayDesc":U + goCntSearchFilter:ContainerCode

    oControl                       = goCntSearchFilter:addControl("fcAuthCopayDesc":U     + goCntSearchFilter:ContainerCode , "wsSpan":U        , "":U  , "":U, "character":U, 3 , 1, "Co-Payment Type:":U)

    oControl                       = goCntSearchFilter:addControl("fdInsurerObj":U        + goCntSearchFilter:ContainerCode , "wsInput":U       , "10":U, "":U, "decimal":U  , 3 , 2, "":U)
    oControl:ControlToken          = "Hidden":U                                                      
                                                                                          
    oControl                       = goCntSearchFilter:addControl("fcInsurerCode":U       + goCntSearchFilter:ContainerCode , "wsInput":U       , "10":U, "":U, "character":U, 3 , 2, "":U)
    oControl:ControlToolTip        = "Please enter a valid client":U
    oControl:AjaxValidation        = "START:" + goWob:ObjectCode + ":ajaxValidation:ermin":U                                                                  
    oControl:FilterFields          = "[CodeField]":U                                                                                                              
    oControl:FilterControls        = "fcInsurerCode":U + goCntSearchFilter:ContainerCode                                                                                                         
    oControl:ReturnFields          = "[RecordObj],[RecordCode],[RecordDescription]":U                                                                                   
    oControl:ReturnControls        = "fdInsurerObj":U  + goCntSearchFilter:ContainerCode + ",":U
                                   + "fcInsurerCode":U + goCntSearchFilter:ContainerCode + ",":U
                                   + "fcInsurerDesc":U + goCntSearchFilter:ContainerCode 

    oControl                       = goCntSearchFilter:addControl("buInsurerBtn":U        + goCntSearchFilter:ContainerCode , "wsLookupButton":U, "":U  , "":U, "":U         , 3 , 2, "":U)
    oControl:CellLayoutMask        = "&1&2&3 &4":U                                        
    oControl:LookupWobFla          = "ermin":U
    oControl:LookupFields          = "erm_insurer.insurer_obj":U
    oControl:LookupControls        = "fdInsurerObj":U + goCntSearchFilter:ContainerCode
    oControl:FilterFields          = "erm_insurer.insurer_code":U
    oControl:FilterControls        = "fcInsurerCode":U + goCntSearchFilter:ContainerCode
    oControl:ReturnFields          = "erm_insurer.insurer_obj,erm_insurer.insurer_code,erm_insurer.insurer_description":U
    oControl:ReturnControls        = "fdInsurerObj":U  + goCntSearchFilter:ContainerCode + ",":U
                                   + "fcInsurerCode":U + goCntSearchFilter:ContainerCode + ",":U
                                   + "fcInsurerDesc":U + goCntSearchFilter:ContainerCode
    
    oControl                       = goCntSearchFilter:addControl("fcInsurerDesc":U       + goCntSearchFilter:ContainerCode , "wsSpan":U        , "":U  , "":U, "character":U, 3 , 2, "Client:":U)

    oControl                       = goCntSearchFilter:addControl("flApplyToPmb":U        + goCntSearchFilter:ContainerCode , "wsCombo":U       , "10":U, "":U, "character":U, 4 , 1, "Apply to PMB:":U)
    oControl:AdditionalItems       = "|No|Yes":U
    oControl:ControlTooltip        = "Indicates whether the co-payment must apply if the main diagnosis is a PMB.":U 
    
    oControl                       = goCntSearchFilter:addControl("fiOptionCode":U        + goCntSearchFilter:ContainerCode , "wsInput":U       , "10":U, "":U, "integer":U  , 4 , 2, "":U)
    oControl:ControlToolTip        = "Please enter a valid Option":U
    oControl:ControlClass          = "+clNumericOnly +clMaxLength:3":U
    oControl:AjaxValidation        = "START:" + goWob:ObjectCode + ":ajaxValidation:scheme":U                                                                  
    oControl:FilterFields          = "[CodeField]":U                                                                                                              
    oControl:FilterControls        = "fiOptionCode":U + goCntSearchFilter:ContainerCode                                                                                                         
    oControl:ReturnFields          = "[RecordCode]":U                                                                                   
    oControl:ReturnControls        = "fiOptionCode":U + goCntSearchFilter:ContainerCode
                                   
    oControl                       = goCntSearchFilter:addControl("buOptionCode":U        + goCntSearchFilter:ContainerCode , "wsLookupButton":U, "":U  , "":U, "":U         , 4 , 2, "Option:":U)
    oControl:CellLayoutMask        = "&1&2":U
    oControl:LookupWobFLA          = "scheme":U
    oControl:LookupFields          = "scheme.scheme-code":U
    oControl:LookupControls        = "fiOptionCode":U + goCntSearchFilter:ContainerCode
    oControl:FilterFields          = "scheme.scheme-code":U
    oControl:FilterControls        = "fiOptionCode":U + goCntSearchFilter:ContainerCode
    oControl:ReturnFields          = "scheme.scheme-code,scheme.short-name":U
    oControl:ReturnControls        = "fiOptionCode":U + goCntSearchFilter:ContainerCode   

    oControl                       = goCntSearchFilter:addControl("flApplyToEmergency":U  + goCntSearchFilter:ContainerCode , "wsCombo":U    , "10":U, "":U, "character":U  , 5 , 1, "Apply to Emergency:":U)
    oControl:AdditionalItems       = "|No|Yes":U
    oControl:ControlTooltip        = "Indicates whether the co-payment must apply if the authorisation is an emergency.":U

    oControl                       = goCntSearchFilter:addControl("fdEffectiveDate":U     + goCntSearchFilter:ContainerCode , "wsInput":U       , "10":U, "":U, "date":U     , 5 , 2, "Effective Date:":U)
    oControl:ControlToolTip        = "Please enter an Effective Date":U

    oControl                       = goCntSearchFilter:addControl("fdEndDate":U           + goCntSearchFilter:ContainerCode , "wsInput":U       , "10":U, "":U, "date":U     , 6 , 2, "End Date:":U)
    oControl:ControlToolTip        = "Please enter an End Date":U

    .      
  
  /* No need to register container as it has been built and registered for us in the base */   
  
  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-defineContainerSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defineContainerSearchResults Procedure 
PROCEDURE defineContainerSearchResults :
/*------------------------------------------------------------------------------
  Purpose   : Define rate control specific containers
  Parameters: 
  Notes     :
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE iControl             AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
                                       
&IF {&DBDFMA} >= 010195 &THEN
  ASSIGN              
    goCntSearchResults                        = NEW cls.mipwscontainer("SearchResults":U + goWob:ObjectCode, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    goCntSearchResults:ContainerMode          = goWob:SubmitValue
    goCntSearchResults:ShowGenericReportPrint = TRUE
    goCntSearchResults:Collapsed              = FALSE
    goCntSearchResults:ViewOnly               = NOT glEnquiryMode
    goCntSearchResults:RowsToRender           = ?
    goCntSearchResults:RowRenderProcedure     = "rowRenderProcedure":U
    goCntSearchResults:RowRenderArgument      = goCntSearchResults:ContainerCode
    goCntSearchResults:DefaultContainerType   = "TABLE":U                                                                                                                                                           
    goCntSearchResults:ContainerTitle         = "Results":U     
    goCntSearchResults:QueryString            = "FOR EACH tt_auth_copay_control NO-LOCK,":U
                                              + "   FIRST tt_insurer NO-LOCK":U
                                              + "   WHERE tt_insurer.insurer_obj = tt_auth_copay_control.insurer_obj OUTER-JOIN":U
                                              + "      BY tt_auth_copay_control.line_number BY tt_auth_copay_control.effective_date DESCENDING":U
                                                                                                                                                                                                                    
    lSuccess                    = goWob:setContainer("SearchResults":U,  goCntSearchResults)                                                                                                                 
                                                                                                                                                      
    oControl                    = goCntSearchResults:addControl("fdObj":U                     + goCntSearchResults:ContainerCode  , "wsInput":U      , "20":U  , "tt_auth_copay_control.auth_copay_control_obj":U , "character":U, 1, "":U)
    oControl:ControlToken       = "Hidden":U        

    oControl                    = goCntSearchResults:addControl("fcProviderType":U            + goCntSearchResults:ContainerCode  , "wsCombo":U      , "10":U, "tt_auth_copay_control.provider_type":U, "character":U  , 2, "Provider Type":U)
    oControl:ControlToolTip     = "Please select a provider type":U
    oControl:QueryString        = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthProviderType'":U
    oControl:KeyField           = "mic_acronym.acronym_code":U
    oControl:DisplayFields      = "mic_acronym.acronym_label":U
    oControl:AdditionalItems    = "<None>=":U

    
    oControl                    = goCntSearchResults:addControl("fcAuthCopayTypeArgumentMnemonic":U + goCntSearchResults:ContainerCode  , "wsInput":U       , "5":U     , "":U                                        , "character":U, 3, "":U)        
    oControl:ControlToken       = "Hidden":U                                                                                                                                                                                                     
    oControl:ControlValue       = "hamct":U

    oControl                    = goCntSearchResults:addControl("fcAuthCopayTypeArgumentField":U    + goCntSearchResults:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,3, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                                                          
    oControl:ControlValue       = "[CodeField]":U                                                                                                                                                                     
                                                                                                                                                                                                                      
    oControl                    = goCntSearchResults:addControl("fdAuthCopayTypeObj":U              + goCntSearchResults:ContainerCode  , "wsInput":U        , "1":U   , "tt_auth_copay_control.auth_copay_type_obj":U      , "decimal":U  ,3, "":U)
    oControl:ControlToken       = "Hidden":U

    oControl                    = goCntSearchResults:addControl("fcAuthCopayType":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "8":U   , "":U                                 , "character":U,  3, "Co-payment</br>Type":U)
    oControl:ControlTooltip     = "Co-payment type that this setup applies to":U 
    oControl:RenderProcedure    = "customRenderProcedure":U  
    oControl:RenderArgument     = "CopayType":U
    oControl:AjaxValidation     = "START:" + goWob:ObjectCode + ":ajaxValidationProcedures:AuthCopayType":U
    oControl:FilterFields       = "[CopayType]":U
    oControl:FilterControls     = "fcAuthCopayType":U + goCntSearchResults:ContainerCode 
    oControl:ReturnFields       = "[CopayTypeObj],[CopayType]":U
    oControl:ReturnControls     = "fdAuthCopayTypeObj":U + goCntSearchResults:ContainerCode + ",":U 
                                + "fcAuthCopayType":U + goCntSearchResults:ContainerCode 

    oControl                    = goCntSearchResults:addControl("buAuthCopayTypeBtn":U  + goCntSearchResults:ContainerCode , "wsLookupButton":U, "":U  , "":U, "":U         , 3 )
    oControl:CellLayoutMask     = "&1&2&3&4&5":U                                        
    oControl:LookupWobFla       = "hamct":U
    oControl:LookupFields       = "ham_auth_copay_type.auth_copay_type_obj":U
    oControl:LookupControls     = "fdAuthCopayTypeObj":U + goCntSearchResults:ContainerCode
    oControl:FilterFields       = "ham_auth_copay_type.auth_copay_type":U
    oControl:FilterControls     = "fcAuthCopayType":U + goCntSearchResults:ContainerCode
    oControl:ReturnFields       = "ham_auth_copay_type.auth_copay_type_obj,ham_auth_copay_type.auth_copay_type":U
    oControl:ReturnControls     = "fdAuthCopayTypeObj":U  + goCntSearchResults:ContainerCode + ",":U
                                + "fcAuthCopayType":U + goCntSearchResults:ContainerCode 

    oControl                    = goCntSearchResults:addControl("fcCopayValueType":U                 + goCntSearchResults:ContainerCode  , "wsCombo":U        , "8":U   , "tt_auth_copay_control.copayment_value_type":U     , "character":U,   4, "Value</br>Type":U)
    oControl:ControlTooltip     = "Specify a Co-payment type (Rand or Percentage) which apply to the Value.":U 
    oControl:AdditionalItems    = "None=?|Rand=R|Percentage=P":U
    oControl:RenderProcedure    = "customRenderProcedure":U  
    oControl:RenderArgument     = "CopayValueType":U

    oControl                    = goCntSearchResults:addControl("fcCopayValue":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "5":U   , "tt_auth_copay_control.copayment_value":U            , "decimal":U,   5, "Value":U)
    oControl:ControlTooltip     = "Rand or percentage amount that must be applied as the co-payment.":U 
    oControl:ControlClass       = "+clNumericOnly":U
    oControl:javascriptOnchange = "this.value = parseFloat(this.value).toFixed(2);"
    
    oControl                    = goCntSearchResults:addControl("fcApplyToPMB":U + goCntSearchResults:ContainerCode  , "wsCombo":U            , "":U                  , "tt_auth_copay_control.apply_to_pmb":U                      , "character":U, 6, "Apply</br>To</br>PMB":U)
    oControl:AdditionalItems    = "|No|Yes":U
    oControl:ControlTooltip     = "Indicates whether the co-payment must apply if the main diagnosis is a PMB.":U 

    oControl                    = goCntSearchResults:addControl("fcApplyToEmergency":U + goCntSearchResults:ContainerCode  , "wsCombo":U            , "":U                  , "tt_auth_copay_control.apply_to_emergency":U           , "character":U, 7, "Apply</br>To</br>Emerg":U)
    oControl:AdditionalItems    = "|No|Yes":U
    oControl:ControlTooltip     = "Indicates whether the co-payment must apply if the authorisation is an emergency.":U
    
    oControl                    = goCntSearchResults:addControl("fcAuthStatus":U + goCntSearchResults:ContainerCode  , "wsCombo":U            , "5":U                  , "tt_auth_copay_control.auth_status":U           , "character":U, 8, "Auth</br>Status":U)
    oControl:ControlTooltip     = "The status that will be assigned by default to the provider, if a co-payment applies.":U
    oControl:RenderProcedure    = "customRenderProcedure":U  
    oControl:RenderArgument     = "AuthStatus":U
    oControl:JavascriptOnChange = "fnOnChangeAuthStatus(this,~"fcAuthStatus~",~"fcAuthStatusNoteTypeArgument~");":U

    oControl                    = goCntSearchResults:addControl("fcAuthStatusNoteTypeArgument":U + goCntSearchResults:ContainerCode  , "wsInput":U            , "5":U                  , "":U           , "character":U, 9, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                 
    oControl:ControlValue       = "AS":U
    oControl:ControlClass       = "+clPreserveValue":U

    oControl                    = goCntSearchResults:addControl("fcAuthStatusNote":U + goCntSearchResults:ContainerCode  , "wsInput":U            , "3":U                  , "tt_auth_copay_control.auth_status_note":U           , "character":U, 9, "Status</br>Note":U)
    oControl:ControlTooltip     = "The status that will be assigned by default to the provider, if a co-payment applies.":U
    oControl:RenderProcedure    = "customRenderProcedure":U                                                                                                                                                                                                                    
    oControl:RenderArgument     = "StatusNote":U
    
    oControl                    = goCntSearchResults:addControl("fcAuthStatusNoteDesc":U            + goCntSearchResults:ContainerCode, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 9, "Status</br>Note":U)            
    oControl:ControlToken       = "Hidden":U   
    oControl:JavaScriptOnChange = "fnUpdateToolTip(this);":U
                

    /* Set reason description as tooltip when reason is selected client side */ 
  
    oControl                    = goCntSearchResults:addControl("buAuthStatusNoteBtn":U             + goCntSearchResults:ContainerCode, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 9)                                                                                                                                                                                                   
    oControl:CellLayoutMask     = "&1&2&3&4":U                                                                                                                                                   
    oControl:LookupWobFLA       = "note":U                                                                                                                                                     
    oControl:LookupFields       = "note.key":U                                                                                                                                                 
    oControl:LookupControls     = "fcAuthStatusNote":U + goCntSearchResults:ContainerCode                                                                                                                              
    oControl:FilterFields       = "note.key,note.type":U                                                                                                                                       
    oControl:FilterControls     = "fcAuthStatusNote":U + goCntSearchResults:ContainerCode + ",":U
                                + "fcAuthStatusNoteTypeArgument":U + goCntSearchResults:ContainerCode                                                                                     
    oControl:ReturnFields       = "note.key,note.narration[1]":U                                                                                                                                           
    oControl:ReturnControls     = "fcAuthStatusNote":U + goCntSearchResults:ContainerCode + ",fcAuthStatusNoteDesc":U + goCntSearchResults:ContainerCode
    oControl:JavascriptOnClick  = "fnUpdateNoteType(this);":U
    
    oControl                    = goCntSearchResults:addControl("fcOverrideReasonTypeArgument":U + goCntSearchResults:ContainerCode  , "wsInput":U            , "5":U                  , "":U           , "character":U, 10, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                 
    oControl:ControlValue       = "AO*":U
    oControl:ControlClass       = "+clPreserveValue":U
    
    oControl                    = goCntSearchResults:addControl("fcOverrideReasons":U + goCntSearchResults:ContainerCode  , "wsTextArea":U       , "5,3":U     , "tt_auth_copay_control.copay_apply_override_reasons":U           , "character":U, 10, "Override</br>Reasons":U)        
    oControl:ControlTooltip     = "Please specify the co-payment override reasons for which a co-payment must apply.":U
    oControl:renderProcedure    = "customRenderProcedure":U
    oControl:RenderArgument     = "OverrideReasons":U
    oControl:ControlClass       = "+clMan":U
        
    oControl                    = goCntSearchResults:addControl("fcOverrideReasonKey":U + goCntSearchResults:ContainerCode  , "wsInput":U       , "5":U     , "tt_auth_copay_control.copay_apply_override_reasons":U           , "character":U, 10, "Override</br>Reasons":U)        
    oControl:ControlToken       = "Hidden":U 
    oControl:JavaScriptOnChange = "fnOverrideReasons(this,~"fcOverrideReasonKey~",~"fcOverrideReasons~");":U
   
    oControl                    = goCntSearchResults:addControl("buOverrideReasonsMsc":U + goCntSearchResults:ContainerCode  , "wsMsComboButton":U       , "":U     , "":U                                        , "":U, 10)
    oControl:CellLayoutMask     = "&1&2&3&4":U
    oControl:LookupWobFLA       = "note":U     
    oControl:LookupFields       = "note.key":U                                                                                                                                                 
    oControl:LookupControls     = "fcOverrideReasonKey":U + goCntSearchResults:ContainerCode                                                                                                                         
    oControl:FilterFields       = "note.type,note.key":U                                                                                                                                       
    oControl:FilterControls     = "fcOverrideReasonTypeArgument":U + goCntSearchResults:ContainerCode + ",":U
                                + "fcOverrideReasonKey":U + goCntSearchResults:ContainerCode

    oControl                    = goCntSearchResults:addControl("fcWarningMessageType":U + goCntSearchResults:ContainerCode  , "wsCombo":U       , "5":U     , "tt_auth_copay_control.warning_message_type":U         , "character":U, 11, "Warning</br>Type":U)        
    oControl:ControlTooltip     = "Specify the warning message type.":U
    oControl:RenderProcedure    = "RenderProcedure":U                                                                                                                                                                                                                    
    oControl:RenderArgument     = "AcronymSelect:ma_acAuthCopayWarnMessageType:=":U
    oControl:JavaScriptOnChange = "fnOnChangeWarningType(this);":U

    oControl                    = goCntSearchResults:addControl("fcWarningMessage":U + goCntSearchResults:ContainerCode  , "wsTextArea":U       , "5,3":U     , "tt_auth_copay_control.warning_message":U         , "character":U, 12, "Warning</br>Message":U)        
    oControl:ControlTooltip     = "Message that will be displayed to the user when a provider outside the co-payment group is used.":U
    oControl:JavaScriptOnChange = "fnOnChangeWarningMessage(this);":U
        
    oControl                    = goCntSearchResults:addControl("fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode  , "wsInput":U       , "5":U     , "":U                                        , "character":U, 13, "":U)        
    oControl:ControlToken       = "Hidden":U                                                                                                                                                                                                     
    oControl:ControlValue       = "ermin":U

    oControl                    = goCntSearchResults:addControl("fcInsurerArgumentField":U    + goCntSearchResults:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,13, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                                                          
    oControl:ControlValue       = "[CodeField]":U                                                                                                                                                                     
                                                                                                                                                                                                                      
    oControl                    = goCntSearchResults:addControl("fdInsurerObj":U              + goCntSearchResults:ContainerCode  , "wsInput":U        , "1":U   , "tt_insurer.insurer_obj":U                         , "decimal":U  ,13, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                                                                     
                                                                                                                                                                                                                      
    oControl                    = goCntSearchResults:addControl("fcInsurer":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "8":U   , "tt_insurer.insurer_code":U                        , "character":U,13, "Client":U)
    oControl:ControlTooltip     = "Please enter a valid client":U                                                                                                                                       
    oControl:AjaxValidation     = "SERVICE:wsUIService:ajaxValidation:":U                                                                                                                              
    oControl:FilterFields       = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls     = "fcInsurer":U + goCntSearchResults:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntSearchResults:ContainerCode + ",fcInsurerArgumentField":U + goCntSearchResults:ContainerCode
    oControl:ReturnFields       = "[RecordObj]":U
    oControl:ReturnControls     = "fdInsurerObj":U + goCntSearchResults:ContainerCode    
    
    oControl                    = goCntSearchResults:addControl("buInsurerBtn":U              + goCntSearchResults:ContainerCode  , "wsLookupButton":U , "":U    , "":U                                               , "":U         ,13)
    oControl:LookupWobFLA       = "ermin":U                                                                                                                                                              
    oControl:LookupFields       = "erm_insurer.insurer_obj":U                                                                                                                                           
    oControl:LookupControls     = "fdInsurerObj":U + goCntSearchResults:ContainerCode                                                                                                                                                            
    oControl:FilterFields       = "erm_insurer.insurer_code":U                                                                                                                                           
    oControl:FilterControls     = "fcInsurer":U + goCntSearchResults:ContainerCode                                                                                                                                                            
    oControl:ReturnFields       = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U                                                                                                                   
    oControl:ReturnControls     = "fdInsurerObj":U + goCntSearchResults:ContainerCode + ",fcInsurer":U + goCntSearchResults:ContainerCode                                                                                             
    oControl:CellLayoutMask     = "&1&2&3&4&5":U  
                                                                     
    oControl                    = goCntSearchResults:addControl("fcOptionArgumentMnemonic":U  + goCntSearchResults:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,14, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                                                        
    oControl:ControlValue       = "scheme":U                                                                                                                                                                        
                                                                                                                                                                                                                    
    oControl                    = goCntSearchResults:addControl("fcOptionArgumentField":U     + goCntSearchResults:ContainerCode  , "wsInput":U        , "5":U   , "":U                                               , "character":U,14, "":U)
    oControl:ControlToken       = "Hidden":U                                                                                                                                                             
    oControl:ControlValue       = "[CodeField]":U 
                                                                                                                    
    oControl                    = goCntSearchResults:addControl("fiOptionCode":U              + goCntSearchResults:ContainerCode  , "wsInput":U        , "3":U   , "tt_auth_copay_control.option_code":U             , "integer":U  ,14, "Option":U)                                                                                                                                                                    
    oControl:ControlClass       = "+clNumericOnly +clMaxLength:3":U
    oControl:ControlTooltip     = "Please enter a valid Option":U
    oControl:RenderProcedure    = "customRenderProcedure":U  
    oControl:RenderArgument     = "OptionCode":U
    oControl:AjaxValidation     = "SERVICE:wsUIService:ajaxValidation:":U
    oControl:FilterFields       = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
    oControl:FilterControls     = "fiOptionCode":U + goCntSearchResults:ContainerCode
                                + ",fcOptionArgumentMnemonic":U + goCntSearchResults:ContainerCode 
                                + ",fcOptionArgumentField":U + goCntSearchResults:ContainerCode
     
    oControl                    = goCntSearchResults:addControl("buOptionBtn":U               + goCntSearchResults:ContainerCode  , "wsLookupButton":U , "":U    , "":U                                               , "":U         ,14)
    oControl:LookupWobFLA       = "scheme":U                                                                                                                                                                                                                                                                                
    oControl:CellLayoutMask     = "&1&2&3&4":U                                                                                                                                                           
    oControl:LookupFields       = "scheme.scheme-code":U                                                                                                                                                 
    oControl:LookupControls     = "fiOptionCode":U + goCntSearchResults:ContainerCode                                                                                                                    
    oControl:FilterFields       = "scheme.scheme-code":U                                                                                                                                                 
    oControl:FilterControls     = "fiOptionCode":U + goCntSearchResults:ContainerCode                                                                                                                    
    oControl:ReturnFields       = "scheme.scheme-code":U                                                                                                                                                 
    oControl:ReturnControls     = "fiOptionCode":U + goCntSearchResults:ContainerCode    
                                                                                                                                                                                                        
    oControl                    = goCntSearchResults:addControl("fdEffectiveDate":U           + goCntSearchResults:ContainerCode  , "wsInput":U        , "10":U  , "tt_auth_copay_control.effective_date":U          , "date":U       ,15, "Effective date":U)
    oControl:ControlClass       = "+clMan":U   
    oControl:ControlToolTip     = "Please enter an Effective Date":U
    oControl:AjaxValidation     = "START:" + goWob:ObjectCode + ":ajaxValidationProcedures:DateFormat":U
    oControl:FilterFields       = "[Date]":U
    oControl:FilterControls     = "fdEffectiveDate":U + goCntSearchResults:ContainerCode 
    oControl:ReturnFields       = "[FormatedDate]":U
    oControl:ReturnControls     = "fdEffectiveDate":U + goCntSearchResults:ContainerCode

    oControl                    = goCntSearchResults:addControl("fdEndDate":U                 + goCntSearchResults:ContainerCode  , "wsInput":U        , "10":U  , "tt_auth_copay_control.end_date":U                , "date":U       ,16, "End date":U)
    oControl:ControlToolTip     = "Please enter an End Date":U
    oControl:AjaxValidation     = "START:" + goWob:ObjectCode + ":ajaxValidationProcedures:DateFormat":U
    oControl:FilterFields       = "[Date]":U
    oControl:FilterControls     = "fdEndDate":U + goCntSearchResults:ContainerCode 
    oControl:ReturnFields       = "[FormatedDate]":U
    oControl:ReturnControls     = "fdEndDate":U + goCntSearchResults:ContainerCode
    .

  ASSIGN oContainerProperties                      = NEW cls.wscontainerproperties(goCntSearchResults)
        
         oContainerProperties:DisplayEditButton    = TRUE
         oContainerProperties:EditButtonAssignList = "&fdAuthCopayControlObj=[BufferField:tt_auth_copay_control.auth_copay_control_obj]":U

         oContainerProperties:DefaultLess            = TRUE
         oContainerProperties:CollapsableControlList = "fiOptionCode":U           + goCntSearchResults:ContainerCode 
         .

  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE &Container = goCntSearchResults &ContainerProperties = oContainerProperties }
  { mip/inc/mipcatcherror.i }
&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DependencyCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DependencyCheck Procedure 
PROCEDURE DependencyCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipoContainer         AS cls.mipwscontainer NO-UNDO.
  DEFINE OUTPUT PARAMETER oplDependencyExists  AS LOGICAL            NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDependencyMessage AS CHARACTER          NO-UNDO.

  DEFINE VARIABLE dAuthCopayControlObj         AS DECIMAL            NO-UNDO.
  
  
  &IF {&DBDFMA} >= 010195 &THEN
    IF VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) THEN
    DO:
      CASE ipoContainer:ContainerCode:
      
        WHEN "SearchResults":U + goWob:ObjectCode THEN
        DO:
          ASSIGN 
             dAuthCopayControlObj   = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_copay_control.auth_copay_control_obj":U, "BUFFER-VALUE":U))
             
             oplDependencyExists    = CAN-FIND(FIRST tt_auth_copay_detail NO-LOCK
                                                WHERE tt_auth_copay_detail.auth_copay_control_obj = dAuthCopayControlObj).
                                                                                      
          IF oplDependencyExists
          THEN       
            ASSIGN opcDependencyMessage = mipEnv:formatMessage({mip/inc/miperrortext.i 'MA' 370 ? ? "'Auth Co-pay Control'" "'Auth Co-pay Details'"}, "TEXT":U).                                  
                                                    
        END. /*WHEN "SearchResults":U + goWob:ObjectCode THEN*/
       
    
      END CASE.
    END. /*IF VALID-OBJECT(ipoContainer) AND VALID-OBJECT(ipoContainer:ContainerQuery) THEN*/

  &ENDIF /*&IF {&DBDFMA} >= 010195 &THEN */
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
  { ws/inc/wsjavascriptopentag.i }
  
  "~n function fnOnChangeAuthStatus(pControl, pAuthStatus, pReasonType)~{ ":U
  "~n    var oRow                = $(pControl).closest('tr'), ":U
  "~n        oAuthStatus = $(oRow).find(~"[name^='~" + pAuthStatus + ~"']~").first().get(0), ":U
  "~n        oReasonType = $(oRow).find(~"[name^='~" + pReasonType + ~"']~").first().get(0); ":U
  "~n  ":U      
  "~n    if ($(oAuthStatus).val()!= '')~{ ":U
  "~n        $(oReasonType).val('AS' + $(oAuthStatus).val());  ":U
  "~n      ~}":U
  "~n    else ~{ $(oReasonType).val('');~}":U
  "~n ~} ":U

  "~n function fnOverrideReasons(pControl, pOverrideReasonsKeys, pOverrideReasons)~{ ":U
  "~n    var oRow                 = $(pControl).closest('tr'), ":U
  "~n        oOverrideReasonsKeys = $(oRow).find(~"[name^='~" + pOverrideReasonsKeys + ~"']~").first().get(0), ":U
  "~n        oOverrideReasons     = $(oRow).find(~"[name^='~" + pOverrideReasons + ~"']~").first().get(0); ":U
  "~n  ":U      
  "~n    if ($(oOverrideReasonsKeys).val()!= '')~{ ":U
  "~n        $(oOverrideReasons).val($(oOverrideReasonsKeys).val());  ":U
  "~n        $(oOverrideReasons).prop('title','Multiple override reasons selected ? please select the lookup for more details');":U
  "~n      ~}else ~{$(oOverrideReasons).prop('title','Please specify the co-payment override reasons for which a co-payment must apply');":U
  "~n        $(oOverrideReasons).val('');  ":U
  "~n~}":U
  "~n ~} ":U


  { ws/inc/wsjavascriptclosetag.i }
  .
  {&OUT}
  { ws/inc/wsjavascriptopentag.i }
  "~n function fnUpdateToolTip(pObject)~{":U
  "~n $(~"[name^='~" + pObject.name.replace(~"fcAuthStatusNoteDesc~",~"fcAuthStatusNote~") + ~"']~").first().prop('title', pObject.value);":U
  "~n ~} ":U


  { ws/inc/wsjavascriptclosetag.i }
  .
  {&OUT}
  { ws/inc/wsjavascriptopentag.i }
  "~n function fnUpdateNoteType(pObject)~{":U
  "~n let cStatusType = 'AS' + $(~"[name^='~" + pObject.name.replace(~"buAuthStatusNoteBtn~",~"fcAuthStatus~") + ~"']~").first().val();"
  "~n $(pObject).closest(~"tr~").find(~"[name^='fcAuthStatusNoteTypeArgument']~").prop('value', cStatusType  );":U
  "~n~}":U
  
  "~n function fnOnChangeOwningEntity(pControl) ~{                                     ":U
  "~n   var oRow         = $(pControl).closest('tr'),                                  ":U
  "~n       oExclCode    = $(oRow).find(~"[name^='fcExclusionCode']~").first(),        ":U
  "~n       oExclObj     = $(oRow).find(~"[name^='fcExclusionObj']~").first(),         ":U
  "~n       oExclKey     = $(oRow).find(~"[name^='fcExclusionKey']~").first(),         ":U
  "~n       oExclDesc    = $(oRow).find(~"[name^='fcExclusionDescription']~").first(), ":U
  "~n       oExclButton  = $(oRow).find(~"[name^='buExclusionBtn']~").first();         ":U
  "~n                                                                   ":U
  "~n   if(pControl.name.indexOf('fcExclusionEntityMnemonic') > -1 )~{  ":U
  "~n     $(oExclCode).val('');   $(oExclObj).val('');                  ":U
  "~n     $(oExclDesc).val('');   $(oExclKey).val('');                  ":U
  "~n     if($(pControl).val()== '')~{                                  ":U
  "~n       $(oExclCode).prop(~"disabled~",true);                       ":U
  "~n       $(oExclButton).prop(~"disabled~",true); ~}                  ":U
  "~n        else~{ $(oExclCode).prop(~"disabled~",false);              ":U
  "~n               $(oExclButton).prop(~"disabled~",false);~}          ":U
  "~n   ~}                                                                    ":U  
  "~n   if(pControl.name.indexOf('fcOwningEntityMnemonic') > -1)~{            ":U
  "~n     $(oRow).find(~"[name^='fcOwningCode']~").first().val('');           ":U
  "~n     $(oRow).find(~"[name^='fcCopaymentDescription']~").first().val(''); ":U
  "~n   ~}                                                                    ":U
  "~n ~}                                                                      ":U

  { ws/inc/wsjavascriptclosetag.i }
  .
  
  {&OUT}
  { ws/inc/wsjavascriptopentag.i }
"~n   function fnOnChangeWarningMessage(pControl)~{                                       ":U
"~n   var oRow         = $(pControl).closest('tr'),                                       ":U
"~n        oWarningType    = $(oRow).find(~"[name^='fcWarningMessageType']~").first(),        ":U
"~n    oWarningMessage = $(oRow).find(~"[name^='fcWarningMessage']~").first().get(0);     ":U
"~n                                                                                       ":U
"~n       if ($(oWarningMessage).val() != '')~{                                               ":U
"~n         $(oWarningType).addClass(~"clMan~");                                              ":U
"~n         ~}                                                                            ":U
"~n   else ~{                                                                             ":U
"~n     $(oWarningType).removeClass(~"clMan~");                                           ":U
"~n        ~}                                                                             ":U 
"~n   ~}                                                                                  ":U
{ ws/inc/wsjavascriptclosetag.i }
  . 
  
  {&OUT}
  { ws/inc/wsjavascriptopentag.i }
"~n   function fnOnChangeWarningType(pControl)~{                                       ":U
"~n   var oRow         = $(pControl).closest('tr'),                                       ":U
"~n        oWarningType    = $(oRow).find(~"[name^='fcWarningMessageType']~").first(),        ":U
"~n    oWarningMessage = $(oRow).find(~"[name^='fcWarningMessage']~").first().get(0);     ":U
"~n                                                                                       ":U
"~n       if ($(oWarningType).val() != '')~{                                               ":U
"~n         $(oWarningMessage).addClass(~"clMan~");                                              ":U
"~n         ~}                                                                            ":U
"~n   else ~{                                                                             ":U
"~n     $(oWarningMessage).removeClass(~"clMan~");                                           ":U
"~n        ~}                                                                             ":U 
"~n   ~}                                                                                  ":U
{ ws/inc/wsjavascriptclosetag.i }
  . 
  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainer Procedure 
PROCEDURE prepareContainer :
/*------------------------------------------------------------------------------
  Purpose   : Prepare Containers    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcContainer   AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER ipcPrepareWhat AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE cButtonList AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cWhatToDo   AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE lSuccess    AS LOGICAL          NO-UNDO.  
  DEFINE VARIABLE oControl    AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oButtonBar  AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oButton     AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oContainer  AS cls.mipwscontainer NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN  
  /* 
    Fetch data 
  */
  RUN retrieveData IN TARGET-PROCEDURE.
    
  
  CASE goWob:Mode:
  
    WHEN "Search":U THEN
    DO: 
      
      RUN prepareContainerSearchResults IN TARGET-PROCEDURE. 
                                                        
    END. /*WHEN "Search":U THEN*/
    
    WHEN "Maint":U THEN
    DO: 
      ASSIGN glCopyScreen              = goCntMaint:ContainerMode = "Copy":U AND goCntMaint:Viewonly = NO.
      /*
        Maint container preparation
      */
        RUN prepareContainerMaint            IN TARGET-PROCEDURE. 
        RUN prepareContainerCopayDetailsCopy IN TARGET-PROCEDURE.
        RUN prepareContainerCopayDetails     IN TARGET-PROCEDURE.
        /*
        Audit trail container preparation
        */
        RUN prepareContainerAuditTrail IN TARGET-PROCEDURE.
        
      /* 
        Override default maint buttons which have been defined in the template base wob
      */
        ASSIGN cWhatToDo = (IF WarpSpeed:ValidationError THEN goCntMaint:ContainerMode ELSE goWob:SubmitValue).
        
        IF LOOKUP(cWhatToDo, "Add,Change,Delete,Copy":U) = 0 THEN 
        DO:
          
          ASSIGN
            cButtonList               = (IF NOT glEnquiryMode 
                                         THEN 
                                           "Return To Search":U                                       
                                         
                                         + ":Copy:Add:Change:Delete":U
                                         
                                         ELSE "":U).

          oButtonBar                = goCntMaint:getControl("frmButtonBar":U).
            
          /*  
            Delete the object ensuring that we dont have issues with duplicate objects registered with the environment.
          */
          IF VALID-OBJECT(oButtonBar:SubContainer)
          THEN 
            DELETE OBJECT oButtonBar:SubContainer.
          
          ASSIGN oButtonBar:SubContainer = wsUiService:getButtonContainer(goCntMaint:ContainerCode + "BtnBar":U, cButtonList).
          
          IF LOOKUP("btnReturnToSearch":U, oButtonBar:SubContainer:getControlNameList()) > 0
          THEN
            ASSIGN
              oButton                   = oButtonBar:SubContainer:getControl("btnReturnToSearch":U)
              oButton:Obj               = "[ClearObj]":U
              oButton:ControlValue      = "":U
              oButton:AllowDoubleSubmit = TRUE
              oButton:JavascriptOnClick = "fnGetControls(~"divdlg~")[0].style.display = ~"block~";":U.
          
        END. /*IF LOOKUP(cWhatToDo, "Add,Change,Delete,Copy":U) = 0 THEN */
        
        IF (cWhatToDo = "copy":U OR cWhatToDo = "change":U OR cWhatToDo = "add":U) THEN
        DO:
          IF WarpSpeed:ValidationError OR goWob:SubmitValue = "Copy":U THEN
          DO:
            ASSIGN goCntAudit:ContainerHidden              = TRUE
                   goCntCopayDetailsFilter:ContainerHidden = TRUE
                 .
            
          END. /*IF WarpSpeed:ValidationError THEN*/
        END. /*IF (cWhatToDo = "copy":U OR cWhatToDo = "change":U OR cWhatToDo = "add":U) THEN*/
      
    END. /*WHEN "Maint":U THEN*/ 
  END CASE. /*CASE goWob:Mode:*/

  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainerAuditTrail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainerAuditTrail Procedure 
PROCEDURE prepareContainerAuditTrail :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters:
  Notes     :        
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lTooMuchData       AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cTableList         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWhereClauseList   AS CHARACTER   NO-UNDO.
  
  /*
    Lets retrieve any audit trail data
  */
  
 ASSIGN
   cWhereClauseList = "auth_copay_control_obj = ":U + STRING(goWob:CurrentObj) + "|":U
                    + "auth_copay_control_obj = ":U + STRING(goWob:CurrentObj)
     .

  mipEnv:Health:maUtility:getAuditRecordTT(INPUT "hac_auth_copay_control|hac_auth_copay_detail":U,   /*Pipe delimited table list*/                                                                                    
                                           INPUT "",                                                 /*Owning obj list or ^ delimited owning key list ( fields | delimited )*/ 
                                           INPUT cWhereClauseList,                                   /*Pipe delimited where clause list*/                                      
                                           INPUT "",                                                 /*Pipe delimited table/field join list*/                                  
                                           INPUT "",                                                 /*User key*/                                                              
                                           INPUT ?,                                                  /*From date*/                                                             
                                           INPUT ?,                                                  /*To date*/                                                               
                                           INPUT "",                                                 /*Action*/                                                                
                                           INPUT "",                                                 /*Wordindex*/                                                             
                                           INPUT TRUE,                                               /*Include the audit trails for child records*/                            
                                           INPUT TRUE,                                               /*Roll up deleted*/                                                       
                                           INPUT ?,                                                  /*Record limit*/                                                          
                                           OUTPUT lTooMuchData,
                                           OUTPUT TABLE ttAuditRecord APPEND). 
    

  ASSIGN goCntAudit:QueryBufferList = STRING(TEMP-TABLE ttAuditRecord:DEFAULT-BUFFER-HANDLE).
  

  { mip/inc/mipcatcherror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainerCopayDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainerCopayDetails Procedure 
PROCEDURE prepareContainerCopayDetails :
/*------------------------------------------------------------------------------
  Purpose   : Detail Results Container Preparation        
  Parameters:
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iDeleted       AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iRowsRendered  AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRecords       AS CHARACTER        NO-UNDO.
    
&IF {&DBDFMA} >= 010195 &THEN  

  

  /*
    Add empty record for create
  */  
  IF (NOT glEnquiryMode AND NOT glCopyScreen)
  
  THEN
    ASSIGN 
      lSuccess = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_copay_detail:DEFAULT-BUFFER-HANDLE).
  
  /* 
    Get the number of records received from the fetch data and populate auth rate control obj
    if it is not populated in the event we have dummy records for the blank row
  */
  FOR EACH tt_auth_copay_detail EXCLUSIVE-LOCK: 
  
    ASSIGN iRowsRendered = iRowsRendered + 1.

    IF tt_auth_copay_detail.auth_copay_control_obj = 0.00 AND DECIMAL(goWob:CurrentObj) <> 0.00
    THEN
      ASSIGN tt_auth_copay_detail.auth_copay_control_obj = DECIMAL(goWob:CurrentObj).

    VALIDATE tt_auth_copay_detail.
  END. /*FOR EACH tt_auth_copay_detail NO-LOCK: */
  ASSIGN goCntCopayDetails:ViewOnly         = FALSE .


  ASSIGN
     cRecords                           = goCntCopayDetailsFilter:getControl("fiRecords":U + goCntCopayDetailsFilter:ContainerCode):ControlValue.

  IF goWob:SubmitValue <> "":U  THEN 
  DO:
    /* 
    Set the container title with the number of records 
    */
    ASSIGN
      goCntCopayDetails:ViewOnly         = glCopyScreen
      
      goCntCopayDetails:QueryBufferList  = STRING(TEMP-TABLE tt_auth_copay_detail:DEFAULT-BUFFER-HANDLE)
      
      lSuccess                           = goCntCopayDetails:PopulateFromQuery() WHEN NOT Warpspeed:ValidationError 
      .
  END. /* IF goWob:SubmitValue <> "":U  THEN  */
  ELSE 
    ASSIGN 
      goCntCopayDetails:QueryBufferList  = STRING(TEMP-TABLE tt_auth_copay_detail:DEFAULT-BUFFER-HANDLE)
      lSuccess                           = goCntCopayDetails:PopulateFromQuery() 
      .
 
  ASSIGN
      goCntCopayDetails:ContainerTitle   = goCntCopayDetails:ContainerTitle + ": ":U 
                                           + (IF Warpspeed:ValidationError 
                                              THEN "There has been an error submitting your page":U
                                              ELSE
                                               ( IF iRowsRendered > INTEGER(cRecords)
                                                 THEN "Please refine your search criteria as it resulted in more than ":U + cRecords + " records...":U
                                                 ELSE STRING(iRowsRendered - (IF glEnquiryMode THEN 0 ELSE 1)) + " record/s found":U 
                                                        + (IF Warpspeed:ValidationError AND iDeleted > 0 THEN " ( " + STRING(iDeleted) + " record/s deleted )":U ELSE "":U))).

  { mip/inc/mipcatcherror.i }

&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainerCopayDetailsCopy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainerCopayDetailsCopy Procedure 
PROCEDURE prepareContainerCopayDetailsCopy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
    goCntDetailsCopy:ContainerHidden        = NOT glCopyScreen.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainerMaint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainerMaint Procedure 
PROCEDURE prepareContainerMaint :
/*------------------------------------------------------------------------------
  Purpose   : Maint Container Preparation    
  Parameters:
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oButtonBar     AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oButton        AS cls.mipwscontrol NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  ASSIGN     
     goCntMaint:QueryString     = SUBSTITUTE(goCntMaint:QueryString, goWob:CurrentObj)
               
     goCntMaint:QueryBufferList = STRING(TEMP-TABLE tt_auth_copay_control:DEFAULT-BUFFER-HANDLE) + "," 
                                + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE)        
     /*Because we dont want to clear the values when we hit an error and we were making a field change or adding a new record,We will not ri=un the populate from query statement */
     /*if we hit an error while deleting we will populate from query because we need to display the values before we clicked confirm*/
     lSuccess                   = goCntMaint:PopulateFromQuery() WHEN (NOT WarpSpeed:ValidationError AND goWob:submitValue <> "Submit") 
                                                                   OR (goWob:submitValue = "Confirm" AND WarpSpeed:ValidationError).       
  { mip/inc/mipcatcherror.i }

&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainerMaintfilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainerMaintfilter Procedure 
PROCEDURE prepareContainerMaintfilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN goCntCopayDetailsFilter:ContainerToken = "Hidden":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareContainerSearchResults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareContainerSearchResults Procedure 
PROCEDURE prepareContainerSearchResults :
/*------------------------------------------------------------------------------
  Purpose   : Search Results Container Preparation        
  Parameters:
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE oControl       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE iDeleted       AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iRowsRendered  AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cContainerCode AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRecords       AS CHARACTER        NO-UNDO.
    
&IF {&DBDFMA} >= 010195 &THEN
  
  /* 
    Get the number of records received from the fetch data 
  */
  FOR EACH tt_auth_copay_control NO-LOCK: 
  
    ASSIGN iRowsRendered = iRowsRendered + 1.                       
  END. /*FOR EACH tt_nappi_price_control NO-LOCK: */

  FOR EACH tt_deleted NO-LOCK:
     ASSIGN iDeleted = iDeleted + 1.
  END. /*FOR EACH tt_deleted NO-LOCK:*/
  
  /*
    Add empty record for create
  */  
  IF NOT glEnquiryMode 
  THEN
    ASSIGN 
      lSuccess = DYNAMIC-FUNCTION("createRecord":U IN TARGET-PROCEDURE, TEMP-TABLE tt_auth_copay_control:DEFAULT-BUFFER-HANDLE).
   
  /* 
    Set the container title with the number of records 
  */
  ASSIGN
    cRecords                           = goCntSearchFilter:getControl("fiRecords":U + goCntSearchFilter:ContainerCode):ControlValue
    goCntSearchResults:ViewOnly        = FALSE    
    
    cContainerCode                     = goCntSearchResults:ContainerCode                                
 
    goCntSearchResults:QueryBufferList = STRING(TEMP-TABLE tt_auth_copay_control:DEFAULT-BUFFER-HANDLE) + ",":U                                        
                                       + STRING(TEMP-TABLE tt_insurer:DEFAULT-BUFFER-HANDLE)  
    
    lSuccess                           = goCntSearchResults:PopulateFromQuery()
    
    goCntSearchResults:ContainerTitle  = goCntSearchResults:ContainerTitle + ": ":U 
                                       + (IF Warpspeed:ValidationError 
                                          THEN "There has been an error submitting your page":U
                                          ELSE
                                           ( IF iRowsRendered > INTEGER(cRecords)
                                             THEN "Please refine your search criteria as it resulted in more than ":U + cRecords + " records...":U
                                             ELSE STRING(iRowsRendered) + " record/s found":U 
                                                    + (IF Warpspeed:ValidationError AND iDeleted > 0 THEN " ( " + STRING(iDeleted) + " record/s deleted )":U ELSE "":U))). 
                                           
  { mip/inc/mipcatcherror.i }

&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-restoreBuffersInError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreBuffersInError Procedure 
PROCEDURE restoreBuffersInError :
/*------------------------------------------------------------------------------
  Purpose   : Restore buffer data from a temporary dataset if there
              are any errors related to the buffer.    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iphSourceDataset AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER iphTargetDataset AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER ipcErrorBuffer   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipcMappingList   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iPair        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cBuffer      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMnemonic    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPair        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hSource      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hSourceError AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hTarget      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hTargetError AS HANDLE      NO-UNDO.

  /*
    Copy any errors back to target error table
  */
  ASSIGN hSourceError = iphSourceDataset:GET-BUFFER-HANDLE(ipcErrorBuffer)
         hTargetError = iphTargetDataset:GET-BUFFER-HANDLE(ipcErrorBuffer).
  
  hTargetError:TABLE-HANDLE:COPY-TEMP-TABLE(hSourceError:TABLE-HANDLE).
  
  /*
    Check which buffers have any errors and replace the contents of the buffers that have
    errors with the data that was submitted by the user for that buffer
  */
  DO iPair = 1 TO NUM-ENTRIES(ipcMappingList):
  
    ASSIGN cPair     = ENTRY(iPair, ipcMappingList)
      
           cMnemonic = ENTRY(1, cPair, "=":U)
           cBuffer   = ENTRY(2, cPair, "=":U)
      
           hSource   = iphSourceDataset:GET-BUFFER-HANDLE(cBuffer)
           hTarget   = iphTargetDataset:GET-BUFFER-HANDLE(cBuffer).
  
    /*
      Check if the buffer has an error
    */ 
    hSourceError:FIND-FIRST(SUBSTITUTE("WHERE &1.owning_entity_mnemonic = '&2' AND &1.error_type = 'ERR'":U, hSourceError:NAME, cMnemonic)) NO-ERROR.
  
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
         
    IF hSourceError:AVAILABLE THEN
    DO:               
      /*
        Restore the contents of the temp table we captured from the users submit
      */
      hTarget:TABLE-HANDLE:COPY-TEMP-TABLE(hSource:TABLE-HANDLE).
    
    END. /*IF hSource:AVAILABLE THEN*/
  END. /*DO iPair = 1 TO ipcMappingList:*/


  { mip/inc/mipcatcherror.i }              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-retrieveData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retrieveData Procedure 
PROCEDURE retrieveData :
/*------------------------------------------------------------------------------
  Purpose   :     
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE oSearch                   AS cls.maauthcopaysearch NO-UNDO.
  DEFINE VARIABLE oControl                  AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE cContainerCode            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cEffectiveDate            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cEndDate                  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMode                     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRecords                  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE dCopaymentTypeObj         AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE cApplyToPMB               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cApplyToEmergency         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cOwningEntityMnemonic     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE dOwningObj                AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE cOwningKey                AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cOwningAltValue           AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cExclusionEntityMnemonic  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE dExclusionObj             AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE cExclusionKey             AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cExclusionAltValue        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cProviderType             AS CHARACTER            NO-UNDO.
  
  DEFINE VARIABLE lSuccess                  AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE iOptionCode               AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dInsurerObj               AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dEffectiveDate            AS DATE                 NO-UNDO.
  DEFINE VARIABLE dEndDate                  AS DATE                 NO-UNDO.
                                            
  DEFINE VARIABLE hARDataset                AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hDataset                  AS HANDLE               NO-UNDO.
  DEFINE VARIABLE cRateEntity               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cEntityCode               AS CHARACTER            NO-UNDO.
  
  oSearch = NEW cls.maauthcopaysearch(DATASET dsAuthCopayControl BY-REFERENCE). 
  
  /*
    If there are errors we want to revert back to what was committed by the user so we will preserve what 
    we currently have populated in our datasets from the business logic section by copying to a temporary 
    structure which we will retore from for those buffers which have errors related to them.
  */
  CREATE DATASET hARDataset .
  hARDataset:CREATE-LIKE(DATASET dsAuthCopayControl:HANDLE).
  hARDataset:COPY-DATASET(DATASET dsAuthCopayControl:HANDLE).    /*This will copy the data as well as the structure to our dynamic dataset*/

  /*
    Get insurer data
  */
  mipEnv:Health:maUtility:getInsurerDetails
    (INPUT 0.00, INPUT "":U, INPUT "":U, INPUT "":U, INPUT "":U, INPUT ?, INPUT ?, INPUT 0.00, OUTPUT TABLE tt_insurer).

  /*
    Get scheme data
  */
  mipEnv:Health:maUtility:getSchemeDetails
    (INPUT 0, INPUT "":U, INPUT "":U, INPUT "":U,OUTPUT DATASET dsScheme).

  IF goWob:Mode = "SEARCH":U THEN
  DO:
   
    
    ASSIGN cContainerCode    =         goCntSearchFilter:ContainerCode                                            
           cRecords          =         goCntSearchFilter:getControl("fiRecords":U          + cContainerCode):ControlValue
           cMode             =         goCntSearchFilter:getControl("fcSearchMode":U       + cContainerCode):ControlValue                                            
           cEffectiveDate    =         goCntSearchFilter:getControl("fdEffectiveDate":U    + cContainerCode):ControlValue
           cEndDate          =         goCntSearchFilter:getControl("fdEndDate":U          + cContainerCode):ControlValue
           iOptionCode       = INTEGER(goCntSearchFilter:getControl("fiOptionCode":U       + cContainerCode):ControlValue)                             
           dInsurerObj       = DECIMAL(goCntSearchFilter:getControl("fdInsurerObj":U       + cContainerCode):ControlValue)
           
           dCopaymentTypeObj = DECIMAL(goCntSearchFilter:getControl("fdAuthCopayTypeObj":U + cContainerCode):ControlValue)
           cApplyToPMB       =         goCntSearchFilter:getControl("flApplyToPMB":U       + cContainerCode):ControlValue
           cApplyToEmergency =         goCntSearchFilter:getControl("flApplyToEmergency":U + cContainerCode):ControlValue
           cProviderType     =         goCntSearchFilter:getControl("fcProviderType":U     + cContainerCode):ControlValue


           dEffectiveDate  = (IF cEffectiveDate = "YYYY/MM/DD":U THEN TODAY ELSE DATE(cEffectiveDate))                   
           dEndDate        = (IF cEndDate       = "YYYY/MM/DD":U THEN ?     ELSE DATE(cEndDate)).

           

    /*
      Retrict the buffers that will be filled and apply filter
    */
    ASSIGN lSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth_copay_control":U)
    
           lSuccess = oSearch:setCriteria("BatchSize":U,"tt_auth_copay_control":U,  cRecords).

    IF goWob:SubmitValue = "Search":U THEN 
    DO:
      ASSIGN

           lSuccess = (IF (cProviderType <> "":U AND cProviderType <> "<None>":U)
                      THEN oSearch:SetFilterCriteria("tt_auth_copay_control.provider_type":U    , "=":U, cProviderType)
                      ELSE TRUE)

           lSuccess = (IF dCopaymentTypeObj <> 0.00 
                      THEN oSearch:SetFilterCriteria("tt_auth_copay_control.auth_copay_type_obj":U    , "=":U, dCopaymentTypeObj)
                      ELSE TRUE)

           lSuccess = (IF cApplyToPMB <> "":U 
                      THEN oSearch:SetFilterCriteria("tt_auth_copay_control.apply_to_pmb":U           , "=":U, LOGICAL(cApplyToPMB))
                      ELSE TRUE)
           lSuccess = (IF cApplyToEmergency <> "":U 
                      THEN oSearch:SetFilterCriteria("tt_auth_copay_control.apply_to_emergency":U     , "=":U, LOGICAL(cApplyToEmergency))
                      ELSE TRUE)
           
           lSuccess = (IF iOptionCode <> 0                                                             
                       THEN  oSearch:SetFilterCriteria("tt_auth_copay_control.option_code":U          , "=":U,  iOptionCode)
                       ELSE TRUE)             

           lSuccess = (IF dInsurerObj <> 0.00                                                          
                       THEN  oSearch:SetFilterCriteria("tt_auth_copay_control.insurer_obj":U          , "=":U,  dInsurerObj)
                       ELSE TRUE)                                                                   
                                                                                                       
           lSuccess = (IF dEffectiveDate <> ?                                                       
                       THEN  oSearch:SetFilterCriteria("tt_auth_copay_control.effective_date":U       , ">=":U, dEffectiveDate)
                       ELSE TRUE)                                                                   
                                                                                                       
           lSuccess = (IF dEndDate <> ?                                                             
                       THEN (oSearch:SetFilterCriteria("tt_auth_copay_control.end_date":U      , "<=":U, dEndDate) AND 
                             oSearch:SetFilterCriteria("tt_auth_copay_control.end_date":U      ,  "=":U, "?":U))
                       ELSE TRUE).
     END. /* IF goWob:SubmitValue = "Search":U THEN  */
                       
    oSearch:fetchData().

  END. /*IF goWob:Mode = "SEARCH":U THEN*/

  IF goWob:Mode = "Maint":U AND goWob:CurrentObj <> "":U AND NOT CAN-FIND(FIRST tt_auth_copay_error NO-LOCK 
                                                                          WHERE tt_auth_copay_error.error_type = "ERR":U)
    /*AND goWob:SubmitValue <> "Submit":U*/
  THEN
  DO:
     DATASET dsAuthCopayControl:EMPTY-DATASET().
     
     IF goWob:submitValue = "Submit":U THEN
     DO:
       ASSIGN
         oControl              = goCntCopayDetailsFilter:getControl("fcExclusionEntityMnemonic":U + goCntCopayDetailsFilter:ContainerCode)
         oControl:ControlValue = "ALL":U.
     END. /* IF goWob:submitValue = "Submit":U */
     
     ASSIGN 
       cContainerCode             = goCntCopayDetailsFilter:ContainerCode 
       cRecords                   =         goCntCopayDetailsFilter:getControl("fiRecords":U                 + cContainerCode):ControlValue
       cOwningEntityMnemonic      =         goCntCopayDetailsFilter:getControl("fcOwningEntityMnemonic":U    + cContainerCode):ControlValue
       dOwningObj                 = DECIMAL(goCntCopayDetailsFilter:getControl("fdOwningObj":U               + cContainerCode):ControlValue)
       cOwningKey                 =         goCntCopayDetailsFilter:getControl("fcOwningKey":U               + cContainerCode):ControlValue
       cOwningAltValue            =         goCntCopayDetailsFilter:getControl("fcOwningCode":U              + cContainerCode):ControlValue
       cExclusionEntityMnemonic   =         goCntCopayDetailsFilter:getControl("fcExclusionEntityMnemonic":U + cContainerCode):ControlValue  
       dExclusionObj              = DECIMAL(goCntCopayDetailsFilter:getControl("fdExclusionObj":U            + cContainerCode):ControlValue)
       cExclusionKey              =         goCntCopayDetailsFilter:getControl("fcExclusionKey":U            + cContainerCode):ControlValue 
       cExclusionAltValue         =         goCntCopayDetailsFilter:getControl("fcExclusionCode":U           + cContainerCode):ControlValue 
       cEffectiveDate             =         goCntCopayDetailsFilter:getControl("fdEffectiveDate":U           + cContainerCode):ControlValue
       cEndDate                   =         goCntCopayDetailsFilter:getControl("fdEndDate":U                 + cContainerCode):ControlValue
       dEffectiveDate             = (IF cEffectiveDate = "YYYY/MM/DD":U THEN TODAY ELSE DATE(cEffectiveDate))                   
       dEndDate                   = (IF cEndDate       = "YYYY/MM/DD":U THEN ?     ELSE DATE(cEndDate))
       
       lSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth_copay_control,tt_auth_copay_detail":U)
       lSuccess = oSearch:SetFilterCriteria("tt_auth_copay_control.auth_copay_control_obj":U    , "=":U, goWob:CurrentObj)
       NO-ERROR.

     ASSIGN
      lSuccess = IF TRIM(cOwningEntityMnemonic) <> "":U 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.owning_entity_mnemonic":U, "=":U, cOwningEntityMnemonic)
                 ELSE TRUE

      lSuccess = IF dOwningObj <> 0.00 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.owning_obj":U, "=":U, dOwningObj)
                 ELSE TRUE

      lSuccess = IF TRIM(cOwningKey) <> "":U 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.owning_key":U, "=":U, cOwningKey)
                 ELSE TRUE

      lSuccess = IF TRIM(cOwningAltValue) <> "":U 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.owning_alt_value":U, "=":U, cOwningAltValue)
                 ELSE TRUE

      lSuccess = IF TRIM(cExclusionEntityMnemonic) <> "All":U 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.exclusion_entity_mnemonic":U, "=":U, cExclusionEntityMnemonic)
                 ELSE TRUE

      lSuccess = IF dExclusionObj <> 0.00 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.exclusion_obj":U, "=":U, dExclusionObj)
                 ELSE TRUE

      lSuccess = IF TRIM(cExclusionKey) <> "":U 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.exclusion_key":U, "=":U, cExclusionKey)
                 ELSE TRUE

      lSuccess = IF TRIM(cExclusionAltValue) <> "":U 
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.exclusion_alt_value":U, "=":U, cExclusionAltValue)
                 ELSE TRUE

      lSuccess = IF dEffectiveDate <> ?    
                 THEN oSearch:SetFilterCriteria("tt_auth_copay_detail.effective_date":U, ">=":U, dEffectiveDate) 
                 ELSE TRUE.
   
    IF dEndDate <> ? AND dEndDate > dEffectiveDate THEN
      ASSIGN 
        lSuccess = oSearch:SetFilterCriteria("tt_auth_copay_detail.end_date":U, "<=":U, dEndDate ).
    
      
     oSearch:fetchData().

  END. /* IF goWob:Mode = "Maint":U THEN */

  FIND FIRST tt_auth_copay_error NO-LOCK 
       WHERE tt_auth_copay_error.error_type = "ERR":U NO-ERROR.
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

  IF AVAILABLE(tt_auth_copay_error) THEN
  DO:
    
    ASSIGN hDataset = DATASET dsAuthCopayControl:HANDLE.

    /*
      Repopulate buffers that have errors
    */
    RUN restoreBuffersInError IN TARGET-PROCEDURE(INPUT hARDataset,                         /*Source dataset             */ 
                                                  INPUT hDataset,                           /*Target dataset             */ 
                                                  INPUT "tt_auth_copay_error":U,           /*Error buffer               */ 
                                                  INPUT "haccc=tt_auth_copay_control,":U + /*Mnemonic to buffer mapping */ 
                                                        "haccd=tt_auth_copay_detail":U).    /*Mnemonic to buffer mapping */ 
                                                        
  END. /*IF Warpspeed:ValidationError THEN*/    


  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oSearch)    THEN DELETE OBJECT oSearch.
                IF VALID-HANDLE(hARDataset) THEN DELETE OBJECT hARDataset.
                IF VALID-HANDLE(hDataset)   THEN DELETE OBJECT hDataset."}
                    

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
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoContainer   AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oQuery                AS cls.mipquery       NO-UNDO.
  
  DEFINE VARIABLE cContainerCode        AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE dObj                  AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE lValidRule            AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL            NO-UNDO.
  
  DEFINE VARIABLE oControl              AS cls.mipwscontrol   NO-UNDO.
  
  
&IF {&DBDFMA} >= 010195 &THEN
  
  CASE ipoContainer:RowRenderArgument:
  
    WHEN "SearchResults":U + goWob:ObjectCode THEN
    DO:
      ASSIGN 
        cContainerCode = ipoContainer:ContainerCode
        dObj           = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_copay_control.auth_copay_control_obj":U, "BUFFER-VALUE":U)). 
                                                              
      /* Populate errors */ 
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_copay_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "haccc":U,
         INPUT dObj,
         INPUT "":U).
    END. /*WHEN "SearchResults":U + goWob:ObjectCode THEN*/ 

    WHEN "CopayDetails":U + goWob:ObjectCode THEN
    DO:
      ASSIGN 
        cContainerCode = ipoContainer:ContainerCode
        dObj           = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_copay_detail.auth_copay_detail_obj":U, "BUFFER-VALUE":U)). 
                                                              
      /* Populate errors */ 
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_copay_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "haccd":U,
         INPUT dObj,
         INPUT "":U).
    END. /*WHEN "AuthCopayDetails":U + goWob:ObjectCode THEN*/
    
  END. /*CASE ipoContainer:RowRenderArgument:*/
  
  RUN SUPER(ipoContainer).
  
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
  
  IF VALID-OBJECT(goErrorObject) THEN DELETE OBJECT goErrorObject.
  
  /*All registered containers are deleted in the base wob*/
  
  { mip/inc/mipcatcherror.i } 
  
END PROCEDURE.  /* shutdown */

/* That's all Folks! */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebAcceptRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebAcceptRequest Procedure 
PROCEDURE WebAcceptRequest :
/*------------------------------------------------------------------------------
  Purpose   : WebAcceptRequest implementation to setup global variables etc 
              that will be required throughout the wob.    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN

  /* 
    Check if running in enquiry mode
  */   
  ASSIGN glEnquiryMode = (get-value("WobMode":U) = "enquiry":U)                                                                                                                                                                
       
         goErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_error:HANDLE).
  
  
  RUN SUPER.    
  
  { mip/inc/mipcatcherror.i }

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebDeliverResponse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebDeliverResponse Procedure 
PROCEDURE WebDeliverResponse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lSuccess    AS LOGICAL          NO-UNDO.
  
  
  RUN SUPER.

  {&OUT}
     "<style type='text/css' media='all'>":U 
     " .tooltip ~{                          ":U
     "     position: relative;              ":U     
     " ~}                                   ":U

     " .tooltip .tooltiptext ~{             ":U
     "     visibility: hidden;              ":U
     "     width: 200px;                    ":U     
     "     overflow: hidden;                ":U          
     "     white-space:pre-wrap;            ":U     
     "     bottom: 100%;                    ":U
     "     left: 50%;                       ":U
     "     margin-left: -60px;              ":U
     "     background-color: grey;          ":U
     "     color: #fff;                     ":U
     "     text-align: center;              ":U
     "     padding: 5px 5px 5px 5px;        ":U
     "     border-radius: 6px;              ":U     
     "     position: absolute;              ":U
     "     z-index: 1;                      ":U
     " ~}                                   ":U
     "                                      ":U
     " .tooltip:hover .tooltiptext ~{       ":U
     "     visibility: visible;             ":U
     " ~}                                   ":U    
     
     "</style>".
 
  
  { mip/inc/mipcatcherror.i }


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WebRenderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WebRenderProcedure Procedure 
PROCEDURE WebRenderProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ipoControl  AS cls.mipwscontrol NO-UNDO.
 DEFINE VARIABLE lExistingRecord    AS LOGICAL          NO-UNDO.
 DEFINE VARIABLE lSuccess           AS LOGICAL          NO-UNDO.

 RUN SUPER(ipoControl).
 
 CASE ipoControl:RenderArgument:
    
    /* ---------------------------------------------------------------------- */
    /* The WebRenderProcedure is the main Rendering Procedure for this WOB    */
    WHEN "WebRenderProcedure":U THEN 
    DO:
       {&OUT}
          "<center>":U.

      CASE goWob:Mode:
        WHEN "Maint":U THEN
        DO:

          /* Output all warning notifications once the page has loaded */
          FOR EACH tt_warning NO-LOCK:

            cls.mipwsutility:renderNotification
              ( tt_warning.error_message,
                "warning":U,
                STRING(tt_warning.notification_timeout),
                "top":U,
                tt_warning.acknowledge,
                tt_warning.owning_entity_mnemonic,
                tt_warning.owning_obj,
                tt_warning.owning_key ).

          END. /* FOR EACH tt_warning NO-LOCK: */
        END. /* WHEN "Maint":U THEN */
      END CASE.  /* CASE goWob:Mode: */

      {&OUT}
          "</center>":U.

    END. /* WHEN "WebRenderProcedure":U */
  END CASE.  /* CASE ipoControl:RenderArgument: */

  
  { mip/inc/mipcatcherror.i &FINALLY = "EMPTY TEMP-TABLE tt_warning."}  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

