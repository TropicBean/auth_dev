/*------------------------------------------------------------------------
    Component: Healthcare SOA
    Author   : Andrewd

    Purpose  : Healthcare Authorisation Services
    Notes    : 
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthservice INHERITS cls.mipservice:

  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  { ma/inc/maauthautods.i         }
  { ma/inc/maauthcopaytypeds.i    }
  { ma/inc/maauthruleds.i         }
  { ma/inc/maauthtypeds.i         }
  { ma/inc/maauthflagvalueds.i    }
  { ma/inc/maexternalreferenceds.i}
  { ma/inc/maauthupdatescheckds.i }
  { ma/inc/maauthtypeconfigtt.i   }
  { ma/inc/madephealthds.i        }
  { ma/inc/maauthcopytt.i         } 
  { ma/inc/madeplimittt.i         }
  { ma/inc/madeplimitwarntt.i     }
  
  DEFINE TEMP-TABLE tt_auth_copy_error LIKE tt_auth_error 
    FIELD orig_auth_num AS CHARACTER
    FIELD new_auth_num  AS CHARACTER . 

  DEFINE TEMP-TABLE ttSessionAuth NO-UNDO
    FIELD oAuth    AS Progress.Lang.Object
    FIELD dAuthObj AS DECIMAL
    FIELD cAuthNum AS CHARACTER.

  DEFINE TEMP-TABLE ttAuthRequestValues NO-UNDO
   FIELD auth_obj                   AS DECIMAL
   FIELD total_amount_requested     AS DECIMAL
   FIELD total_amount_authorised    AS DECIMAL
   FIELD initial_fee_requested	    AS DECIMAL
   FIELD initial_fee_authorised	    AS DECIMAL
   FIELD instalment_requested       AS DECIMAL
   FIELD instalment_num_requested   AS DECIMAL
   FIELD instalment_authorised	    AS DECIMAL
   FIELD instalment_num_authorised  AS DECIMAL.
  
  DEFINE PUBLIC PROPERTY AuthService AS HANDLE               NO-UNDO GET. PROTECTED SET.
  DEFINE        VARIABLE oAuth       AS cls.maauthorisation  NO-UNDO.

  /* -- METHOD: initializeService ------------------------------------ */
  METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
  /*------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess      = SUPER:initializeService()

        AuthService = ObjectStack.

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */

  /* --- METHOD: getAuthObject ----------------------------------- */
  METHOD PUBLIC LOGICAL getAuthObject
    ( INPUT  ipdAuthObj AS DECIMAL,
	    INPUT  ipcAuthNum AS CHARACTER,
      OUTPUT opoAuth    AS cls.maauthorisation ):
    /*-----------------------------------------------------------------------------
       Purpose : Get auth object by providing either an auth num or auth obj.
                 In case focused succesfully, the instance of the auth object will 
                 be stored in ttSessionAuth ,which will be re-usable for the duration of a session. 
       Notes   : 
     ----------------------------------------------------------------------------- */
	  
	  ASSIGN
      ipdAuthObj = (IF ipdAuthObj = ? THEN 0    ELSE      ipdAuthObj)
	    ipcAuthNum = (IF ipcAuthNum = ? THEN "":U ELSE TRIM(ipcAuthNum)).
	  
    IF ipdAuthObj <= 0 AND ipcAuthNum = "" THEN
    DO:
      ASSIGN opoAuth = NEW cls.maauthorisation() .
      
      RETURN VALID-OBJECT(opoAuth).
    END. /* IF ipdAuthObj <= 0 AND ipcAuthNum = "" THEN */
    
	  IF ipdAuthObj > 0 AND ipcAuthNum > "":U
	  THEN
	    RETURN FALSE.  
	
	  FIND FIRST ttSessionAuth
	       WHERE ttSessionAuth.dAuthObj = ipdAuthObj
		     AND   ttSessionAuth.cAuthNum = "":U
	    NO-ERROR.
	  {mip/inc/mipthrowerror.i &IgnoreErrors='PROGRESS:565' &ResetIgnoredErrors=TRUE}
	
	  IF NOT AVAILABLE ttSessionAuth
	  THEN
	    FIND FIRST ttSessionAuth
	         WHERE ttSessionAuth.cAuthNum = ipcAuthNum
		       AND   ttSessionAuth.dAuthObj = 0
	      NO-ERROR.

	  {mip/inc/mipthrowerror.i &IgnoreErrors='PROGRESS:565' &ResetIgnoredErrors=TRUE}
	
	  IF AVAILABLE ttSessionAuth THEN 
    DO:
	    ASSIGN
	      opoAuth = CAST(ttSessionAuth.oAuth, "cls.maauthorisation":U).
    
      IF NOT VALID-OBJECT(opoAuth) 
      THEN
        ASSIGN
	  	    oAuth   = NEW cls.maauthorisation( INPUT ipdAuthObj )
          opoAuth = oAuth.
    END.  /* IF AVAILABLE ttSessionAuth THEN */
	  ELSE DO:
	    IF VALID-OBJECT(oAuth) THEN DELETE OBJECT oAuth.
	  
	    IF ipdAuthObj > 0
	    THEN
	      ASSIGN
	  	    oAuth = NEW cls.maauthorisation( INPUT ipdAuthObj ).
      ELSE
	      ASSIGN
	    	  oAuth = NEW cls.maauthorisation( INPUT ipcAuthNum ).
	    
	    CREATE ttSessionAuth.
	    
	    ASSIGN
	      ttSessionAuth.oAuth    = oAuth
	    	ttSessionAuth.dAuthObj = ipdAuthObj
	  	  ttSessionAuth.cAuthNum = ipcAuthNum
	  	
	  	  opoAuth = oAuth.
	  END. /* IF NOT AVAILABLE ttSessionAuth */
	
    RETURN VALID-OBJECT(oAuth).
	  {mip/inc/mipcatcherror.i}
  END METHOD. /* --- METHOD PUBLIC LOGICAL checkSessionAuth --------- */

  /* --- METHOD: getAuthRequestValues ----------------------------------- */
  METHOD PUBLIC LOGICAL getAuthRequestValues
      ( INPUT         ipdAuthObj AS DECIMAL,
        OUTPUT TABLE  ttAuthRequestValues):
    

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getAuthRequestValues"
        &ServiceCallSignature = "INPUT ipdAuthObj,
                                 OUTPUT TABLE ttAuthRequestValues BY-REFERENCE"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  END METHOD. /* --- METHOD PUBLIC LOGICAL getAuthRequestValues --------- */  

  /* --- METHOD: clearSessionAuth ----------------------------------- */
  METHOD PUBLIC LOGICAL clearSessionAuth():
    EMPTY TEMP-TABLE ttSessionAuth.

	IF VALID-OBJECT(oAuth) THEN DELETE OBJECT oAuth.

	RETURN TRUE.
	{mip/inc/mipcatcherror.i}
  END METHOD. /* --- METHOD PUBLIC LOGICAL clearSessionAuth --------- */  

/*-- METHOD: applyAuthLOSDetailsUser ----------------------------------------------- */
  METHOD PUBLIC LOGICAL applyAuthLOSDetailsUser
    (INPUT-OUTPUT DATASET FOR dsAuthorisation):
/*-----------------------------------------------------------------------------
    Purpose : Apply LOS start/end date/time changes as captured by user 
    Notes   : 
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "applyAuthLOSDetailsUser"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL applyAuthLOSDetailsUser*/


/*-- METHOD: calcAmountAuth --------------------------------------------------- */
  METHOD PUBLIC LOGICAL calcAmountAuth
    (INPUT-OUTPUT DATASET FOR dsAuthorisation,
     INPUT  ipdAuthDetailObj         AS DECIMAL,
     INPUT  ipcCostCalculation       AS CHARACTER,
     INPUT  ipdTrfcostAmount         AS DECIMAL,
     OUTPUT opdItemCost              AS DECIMAL,
     OUTPUT opcLineRestriction       AS CHARACTER,
     OUTPUT opcRelatedEntityMnemonic AS CHARACTER,
     OUTPUT opdRelatedObj            AS DECIMAL,
     OUTPUT opcRelatedValue          AS CHARACTER,
     OUTPUT opdCostCalculated        AS DECIMAL,
     OUTPUT opcCostError             AS CHARACTER):
/*-----------------------------------------------------------------------------
    Purpose : Calculate Tariff Cost on Authorisation 
    Notes   : This routine will accept as input parameters, the Authorisation 
              Detail line obj, the Tariff Cost calculation acronym value and the 
              Tariff Cost amount.
              The Item Cost, Line Restriction, Related Entity Mnemonic, -Obj and -Value
              will be returned where applicable.
              The Cost Calculation with be done an returned. 
              An error message will also be returned for any errors that might
              have occurred.                                                          
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "calcAmountAuth"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 INPUT  ipdAuthDetailObj,
                                 INPUT  ipcCostCalculation,
                                 INPUT  ipdTrfcostAmount,
                                 OUTPUT opdItemCost,
                                 OUTPUT opcLineRestriction,
                                 OUTPUT opcRelatedEntityMnemonic,
                                 OUTPUT opdRelatedObj,
                                 OUTPUT opcRelatedValue,
                                 OUTPUT opdCostCalculated,
                                 OUTPUT opcCostError"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL calcAmountAuth*/


/*-- METHOD: calcAuthLOSDetails ----------------------------------------------- */
  METHOD PUBLIC LOGICAL calcAuthLOSDetails
    (INPUT-OUTPUT DATASET FOR dsAuthorisation):
/*-----------------------------------------------------------------------------
    Purpose : Calculate Authorisation LOS details    
    Notes   : This routine will accept as input parameters, the main provider
              ars rate,the auth start date as well as an authorisation dataset.
              The LOS information will be updated on the detail temp table 
              in the authorisation dataset accordingly, the final discharge 
              date and the total LOS will be returned. 
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "calcAuthLOSDetails"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL calcAuthLOSDetails*/


/*-- METHOD: checkForUpdates -------------------------------------------------- */
  METHOD PUBLIC LOGICAL checkForUpdates
    (INPUT ipdAuthObj AS DECIMAL ,
     INPUT-OUTPUT DATASET FOR dsRequest,
     INPUT-OUTPUT DATASET FOR dsResponse):
/*-----------------------------------------------------------------------------
        Purpose: Automatic rebuild detection
                 This service will be used by a rest request to check if there 
                 are any updates to the specified authorisation from the last 
                 know update and if so, the relevant container html will be 
                 returned for a rebuild.
          Notes:
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "checkForUpdates"
        &ServiceCallSignature = "INPUT ipdAuthObj, 
                                 INPUT-OUTPUT DATASET dsRequest  BY-REFERENCE, 
                                 INPUT-OUTPUT DATASET dsResponse BY-REFERENCE"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL checkForUpdates*/

/*-- METHOD: checkForDuplicateAuths ----------------------------------------------- */
  METHOD PUBLIC LOGICAL checkForDuplicateAuths
    (INPUT-OUTPUT DATASET FOR dsAuthorisation,
     OUTPUT TABLE tt_auth_type,
     OUTPUT opcMessage AS CHARACTER):
/*-----------------------------------------------------------------------------
    Purpose : Check for Auth duplicates and cater for overlapping dates when 
              checking for duplicates.
    Notes   : 
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "checkForDuplicateAuths"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 OUTPUT TABLE tt_auth_type,
                                 OUTPUT opcMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL checkForDuplicateAuths*/

/*-- METHOD: clearAuthTypeConfigCache -------------------------------- */
  METHOD PUBLIC LOGICAL clearAuthTypeConfigCache
    ():
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    
    { mip/inc/mipservicecall.i &ServiceProcedureName = "clearAuthTypeConfigCache" } 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL clearAuthTypeConfigCache*/  


/*-- METHOD: getAuthTypeConfig --------------------------------------- */
  METHOD PUBLIC LOGICAL getAuthTypeConfig
    (BUFFER btt_auth FOR tt_auth,
     INPUT-OUTPUT TABLE ttAuthTypeConfig):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    
    DEFINE VARIABLE dAuthTypeObj   AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE dInsurerObj    AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE iOptionCode    AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE dEffectiveDate AS DATE      NO-UNDO. 
    DEFINE VARIABLE cProviderType  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iNegNumber     AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cDiscipline    AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cSubDiscipline AS CHARACTER NO-UNDO. 

    ASSIGN dAuthTypeObj    = btt_auth.auth_type_obj WHEN AVAILABLE btt_auth
           dInsurerObj     = btt_auth.insurer_obj   WHEN AVAILABLE btt_auth
           iOptionCode     = btt_auth.option_code   WHEN AVAILABLE btt_auth 
           dEffectiveDate  = btt_auth.start_date    WHEN AVAILABLE btt_auth
           cProviderType   = "":U
           iNegNumber      = 0
           cDiscipline     = "":U
           cSubDiscipline  = "":U .

    getAuthTypeConfig(INPUT dAuthTypeObj,
                      INPUT dInsurerObj,  
                      INPUT iOptionCode,    
                      INPUT dEffectiveDate, 
                      INPUT cProviderType,  
                      INPUT iNegNumber,     
                      INPUT cDiscipline,    
                      INPUT cSubDiscipline, 
                      INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

END METHOD. /*METHOD PUBLIC LOGICAL getAuthTypeConfig*/

/*-- METHOD: getAuthTypeConfig --------------------------------------- */
  METHOD PUBLIC LOGICAL getAuthTypeConfig
    (BUFFER btt_auth          FOR tt_auth,
     BUFFER btt_auth_provider FOR tt_auth_provider ,
     INPUT-OUTPUT TABLE ttAuthTypeConfig):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    
    DEFINE VARIABLE dAuthTypeObj   AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE dInsurerObj    AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE iOptionCode    AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE dEffectiveDate AS DATE      NO-UNDO. 
    DEFINE VARIABLE cProviderType  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iNegNumber     AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cDiscipline    AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cSubDiscipline AS CHARACTER NO-UNDO. 
   
    ASSIGN dAuthTypeObj    = btt_auth.auth_type_obj                                           WHEN AVAILABLE btt_auth
           dInsurerObj     = btt_auth.insurer_obj                                             WHEN AVAILABLE btt_auth
           iOptionCode     = btt_auth.option_code                                             WHEN AVAILABLE btt_auth 
           dEffectiveDate  = btt_auth.start_date                                              WHEN AVAILABLE btt_auth
           cProviderType   = btt_auth_provider.provider_type                                  WHEN AVAILABLE btt_auth_provider
           iNegNumber      = INTEGER(TRIM(ENTRY(1, btt_auth_provider._neg_group, ":"), "("))  WHEN AVAILABLE btt_auth_provider
           cDiscipline     = STRING(btt_auth_provider.pr_type)                                WHEN AVAILABLE btt_auth_provider
           cSubDiscipline  = STRING(btt_auth_provider.sub_pr_type)                            WHEN AVAILABLE btt_auth_provider.

    getAuthTypeConfig(INPUT dAuthTypeObj,
                      INPUT dInsurerObj,  
                      INPUT iOptionCode,    
                      INPUT dEffectiveDate, 
                      INPUT cProviderType,  
                      INPUT iNegNumber,     
                      INPUT cDiscipline,    
                      INPUT cSubDiscipline, 
                      INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

END METHOD. /*METHOD PUBLIC LOGICAL getAuthTypeConfig*/


/*-- METHOD: getAuthTypeConfig --------------------------------------- */
  METHOD PUBLIC LOGICAL getAuthTypeConfig
    (INPUT ipdAuthTypeObj   AS DECIMAL  ,
     INPUT ipdInsurerObj    AS DECIMAL  ,
     INPUT ipiOptionCode    AS INTEGER  ,
     INPUT ipdEffectiveDate AS DATE     ,
     INPUT-OUTPUT TABLE ttAuthTypeConfig):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    
    getAuthTypeConfig(INPUT ipdAuthTypeObj,
                      INPUT ipdInsurerObj,
                      INPUT ipiOptionCode,
                      INPUT ipdEffectiveDate,
                      INPUT "":U,
                      INPUT 0,
                      INPUT "":U,
                      INPUT "":U,
                      INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL getAuthTypeConfig*/

/*-- METHOD: getAuthTypeConfig --------------------------------------- */
   METHOD PUBLIC LOGICAL getAuthTypeConfig
    (INPUT ipdAuthTypeObj   AS DECIMAL  ,
     INPUT ipdInsurerObj    AS DECIMAL  ,
     INPUT ipiOptionCode    AS INTEGER  ,
     INPUT ipdEffectiveDate AS DATE     ,
     INPUT ipcProviderType  AS CHARACTER,
     INPUT ipiNegNumber     AS INTEGER  ,
     INPUT ipcDiscipline    AS CHARACTER,
     INPUT ipcSubDiscipline AS CHARACTER,
     INPUT-OUTPUT TABLE ttAuthTypeConfig):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE iProgram       AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cStackTrace    AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cProgram       AS CHARACTER NO-UNDO. 

    /*
      Prepare stack trace
    */
    DO iProgram = 7 TO 3 BY -1:

      IF PROGRAM-NAME(iProgram) <> ?
      THEN DO:
        ASSIGN 
            cProgram    = PROGRAM-NAME(iProgram)
            cStackTrace = cStackTrace 
                        + (IF cStackTrace <> "":U 
                           THEN "^":U
                           ELSE "":U)
                        + cProgram.   
          
      END.     
    END. /*DO iProgram = 10 TO 2 BY -1:*/
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getAuthTypeConfig"
        &ServiceCallSignature = "INPUT ipdAuthTypeObj  , 
                                 INPUT ipdInsurerObj   , 
                                 INPUT ipiOptionCode   , 
                                 INPUT ipdEffectiveDate, 
                                 INPUT ipcProviderType , 
                                 INPUT ipiNegNumber    ,
                                 INPUT ipcDiscipline   , 
                                 INPUT ipcSubDiscipline, 
                                 INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL getAuthTypeConfig*/
  

/*-- METHOD: getAuthExternalReference -------------------------------- */
  METHOD PUBLIC LOGICAL getAuthExternalReference
    (INPUT ipdAuthObj AS  DECIMAL,
     INPUT-OUTPUT DATASET FOR dsExternalReference):
/*--------------------------------------------------------------------
        Purpose: Returns set of external references for an auth
          Notes:
  -------------------------------------------------------------------- */
    
    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "getAuthExternalReference"
        &ServiceCallSignature = "INPUT ipdAuthObj, 
                                 INPUT-OUTPUT DATASET dsExternalReference BY-REFERENCE"}
                                 
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL getAuthExternalReference*/
  
/*-- METHOD: getRateControl ------------------------------------ */
  METHOD PUBLIC LOGICAL getRateControl
    (INPUT ipcOwningEntityMnemonic      AS CHARACTER,
     INPUT ipdOwningEntityObj           AS DECIMAL,
     INPUT iplIgnoreDefaultTriggers     AS LOGICAL,
     INPUT-OUTPUT DATASET FOR dsAuthorisation,
     INPUT-OUTPUT DATASET FOR dsAuthFlagValue,
     OUTPUT opcProviderOverrideBaseRate AS CHARACTER,
     OUTPUT opcProviderOverrideArsRate  AS CHARACTER,
     OUTPUT opcCodeLinkCategory         AS CHARACTER,
     OUTPUT opdAuthRateControlObj       AS DECIMAL):
/*--------------------------------------------------------------------
        Purpose: Check whether any rate control setups apply for the 
                 auth according to the setups on the rate control 
                 tables and the values captured on the auth.
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE cDescription AS CHARACTER   NO-UNDO.
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getRateControl"
        &ServiceCallSignature = "INPUT ipcOwningEntityMnemonic,
                                 INPUT ipdOwningEntityObj,
                                 INPUT iplIgnoreDefaultTriggers,
                                 INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE,
                                 OUTPUT opcProviderOverrideBaseRate,
                                 OUTPUT opcProviderOverrideArsRate,
                                 OUTPUT opcCodeLinkCategory,
                                 OUTPUT opdAuthRateControlObj"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER getRateControl */

/*-- METHOD: getStatusDescription ------------------------------------ */
  METHOD PUBLIC CHARACTER getStatusDescription
    (INPUT ipcStatusType AS CHARACTER, 
     INPUT ipiStatusCode AS INTEGER):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE cDescription AS CHARACTER   NO-UNDO.
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getStatusDescription"
        &ServiceCallSignature = "INPUT ipcStatusType,
                                 INPUT ipiStatusCode,
                                 OUTPUT cDescription"} 

    RETURN cDescription.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER getStatusDescription */
  
  
/*-- METHOD: getStatusTable ------------------------------------------ */
  METHOD PUBLIC LOGICAL getStatusTable
    (INPUT ipcStatusType AS CHARACTER, 
     OUTPUT TABLE ttAuthStatus):
/*--------------------------------------------------------------------
        Purpose: Populates a temp-table with all the auth status
                 codes and their corresponding descriptions for
                 a specific type, e.g. Auth Descr or System Descr
          Notes:
  -------------------------------------------------------------------- */
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getStatusTable"
        &ServiceCallSignature = "INPUT ipcStatusType,
                                 OUTPUT TABLE ttAuthStatus"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER getStatusTable */                


/*-- METHOD: statusReasonMandatory ----------------------------------- */
  METHOD PUBLIC LOGICAL statusReasonMandatory
    (INPUT  ipiStatusCode         AS INTEGER,  
     INPUT  ipdInsurerObj         AS DECIMAL,
     INPUT  ipiOptionCode         AS INTEGER,  
     INPUT  ipdAuthDate           AS DATE):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
  DEFINE VARIABLE lMandatory AS LOGICAL     NO-UNDO.
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "statusReasonMandatory"
        &ServiceCallSignature = "INPUT  ipiStatusCode,      
                                 INPUT  ipdInsurerObj,
                                 INPUT  ipiOptionCode,       
                                 INPUT  ipdAuthDate,         
                                 OUTPUT lMandatory"} 

    RETURN lMandatory.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL statusReasonMandatory */


/*-- METHOD: validateAuthStatus -------------------------------------- */
  METHOD PUBLIC LOGICAL validateAuthStatus
    (INPUT ipiStatusCode AS INTEGER,  
     INPUT ipcStatusType AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:  Validate the auth status and return an error if the  
                  auth status is not valid
          Notes:
  -------------------------------------------------------------------- */
  DEFINE VARIABLE lValidAuthStatus AS LOGICAL     NO-UNDO.
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateAuthStatus"
        &ServiceCallSignature = "INPUT  ipiStatusCode,      
                                 INPUT  ipcStatusType,       
                                 OUTPUT lValidAuthStatus"} 

    RETURN lValidAuthStatus.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER validateAuthStatus */
  

/*-- METHOD: validateAuthStatusUpdate -------------------------------- */
  METHOD PUBLIC CHARACTER validateAuthStatusUpdate
    (INPUT ipdInsurerObj         AS DECIMAL,
     INPUT ipiOptionCode         AS INTEGER,
     INPUT ipiFromStatusCode     AS INTEGER,
     INPUT ipcFromStatusReason   AS CHARACTER,
     INPUT ipiToStatusCode       AS INTEGER,
     INPUT ipdAmountPaid         AS DECIMAL,
     INPUT ipdQuantityPaid       AS DECIMAL,
     INPUT ipdDate               AS DATE,  
     INPUT ipiParentStatusCode	 AS INTEGER,
     INPUT ipcParentStatusReason AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:  Validate the auth status when the record is updated 
                  (NOT a new record) and return an error message if 
                  the auth status is not valid.
          Notes:
  -------------------------------------------------------------------- */
  DEFINE VARIABLE cErrorMessage AS CHARACTER     NO-UNDO.
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateAuthStatusUpdate"
        &ServiceCallSignature = "INPUT ipdInsurerObj,
                                 INPUT ipiOptionCode,
                                 INPUT ipiFromStatusCode,
                                 INPUT ipcFromStatusReason, 
                                 INPUT ipiToStatusCode,
                                 INPUT ipdAmountPaid,
                                 INPUT ipdQuantityPaid,
                                 INPUT ipdDate, 
                                 INPUT ipiParentStatusCode, 
                                 INPUT ipcParentStatusReason, 
                                 OUTPUT cErrorMessage"} 

    RETURN cErrorMessage.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER validateAuthStatusUpdate */
  
  /*-- METHOD: validateAuthTypeDiscipline ---------------------------- */
  METHOD PUBLIC LOGICAL validateAuthTypeDisciplineExcl
    (INPUT  ipdAuthTypeObj   AS DECIMAL,  
     INPUT  ipdInsurerObj    AS DECIMAL,
     INPUT  ipiOptionCode    AS INTEGER,
     INPUT  ipcAuthType      AS CHARACTER,
     INPUT  ipcProviderType  AS CHARACTER,
     INPUT  ipcDiscipline    AS CHARACTER,
     INPUT  ipdAuthGroupObj  AS DECIMAL,
     INPUT  ipcSubDiscipline AS CHARACTER,
     INPUT  ipdEffectiveDate AS DATE,
     OUTPUT opcStatus        AS CHARACTER,
     OUTPUT opcStatusReason  AS CHARACTER,
     OUTPUT oplValid         AS LOGICAL,
     OUTPUT opcMessage       AS CHARACTER):
  /*------------------------------------------------------------------
        Purpose:  Checks whether the specified discipline is valid for the
                  specified auth type
          Notes:
  -------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateAuthTypeDisciplineExcl"
        &ServiceCallSignature = "INPUT  ipdAuthTypeObj,  
                                 INPUT  ipdInsurerObj,
                                 INPUT  ipiOptionCode,
                                 INPUT  ipcAuthType,
                                 INPUT  ipcProviderType, 
                                 INPUT  ipcDiscipline, 
                                 INPUT  ipdAuthGroupObj,
                                 INPUT  ipcSubDiscipline, 
                                 INPUT  ipdEffectiveDate,
                                 OUTPUT opcStatus,
                                 OUTPUT opcStatusReason,
                                 OUTPUT oplValid,        
                                 OUTPUT opcMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL validateAuthTypeDiscipline */ 
  
  /*-- METHOD: validateAuthTypeProvider ------------------------------ */
  METHOD PUBLIC LOGICAL validateProviderDiscipline
    (INPUT  ipcProvider      AS CHARACTER,
     INPUT  ipcDiscipline    AS CHARACTER,
     INPUT  ipcSubDiscipline AS CHARACTER,
     INPUT  ipdEffectiveDate AS DATE,
     OUTPUT oplValid         AS LOGICAL,
     OUTPUT opcMessage       AS CHARACTER):
  /*------------------------------------------------------------------
        Purpose:  Checks whether the specified provider is valid for the
                  specified auth type
          Notes:
  -------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateProviderDiscipline"
        &ServiceCallSignature = "INPUT  ipcProvider, 
				                         INPUT  ipcDiscipline,     
                                 INPUT  ipcSubDiscipline,
                                 INPUT  ipdEffectiveDate,
                                 OUTPUT oplValid,        
                                 OUTPUT opcMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL validateAuthTypeProvider */
  
  /*-- METHOD: validateBodyRegion -------------------------------------- */
  METHOD PUBLIC LOGICAL validateBodyRegion
    (INPUT  ipdInsurerObj               AS DECIMAL,  
     INPUT  ipiOptionCode               AS INTEGER,  
     INPUT  ipcAuthHeaderBodyRegion     AS CHARACTER,
     INPUT  ipdtAuthHeaderDate          AS DATE,       
     INPUT  ipcAuthContainerBodyRegion  AS CHARACTER,
     OUTPUT oplValidBodyRegion          AS LOGICAL,   
     OUTPUT opcValidationErrorMessage   AS CHARACTER  ):
/*--------------------------------------------------------------------
  Purpose:    1. Validate the body region in the container
		          2. Validate that the container body region matches the header body region.
    		         The header body region is set up as a rule and the matching body regions
    		         are set up in the rule value.
  Notes:
  -------------------------------------------------------------------- */
  
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateBodyRegion"
        &ServiceCallSignature = "INPUT     ipdInsurerObj,               
                                 INPUT     ipiOptionCode,                
                                 INPUT     ipcAuthHeaderBodyRegion,  
                                 INPUT     ipdtAuthHeaderDate,        
                                 INPUT     ipcAuthContainerBodyRegion,
                                 OUTPUT    oplValidBodyRegion,       
                                 OUTPUT    opcValidationErrorMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER validateAuthStatus */


/*-- METHOD: validateStatusReason ------------------------------------ */
  METHOD PUBLIC LOGICAL validateStatusReason
    (INPUT  ipdInsurerObj   AS DECIMAL,  
     INPUT  ipiOptionCode   AS INTEGER,  
     INPUT  ipcStatusNote   AS CHARACTER,
     INPUT  ipiStatusCode   AS INTEGER,  
     INPUT  ipdDate         AS DATE,     
     OUTPUT opcErrorMessage AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:  Validate the authorisation status reason against the note table
          Notes:
  -------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateStatusReason"
        &ServiceCallSignature = "INPUT  ipdInsurerObj, 
                                 INPUT  ipiOptionCode,  
                                 INPUT  ipcStatusNote,  
                                 INPUT  ipiStatusCode,  
                                 INPUT  ipdDate,        
                                 OUTPUT opcErrorMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER validateStatusReason */
  
/*-- METHOD: validateAuthorisedValues -------------------------------- */
  METHOD PUBLIC LOGICAL validateAuthorisedValues
    (INPUT  ipdAmount                   AS  DECIMAL,   
     INPUT  ipiQuantity                 AS  INTEGER,  
     INPUT  iplActivateAuthorisedValues AS  LOGICAL,
     INPUT  iplHeaderValuesUnlimited    AS  LOGICAL,
     INPUT  ipcHeaderValuesAllowed      AS  CHARACTER,
     OUTPUT opcFieldName                AS  CHARACTER,
     OUTPUT opcMessage                  AS  CHARACTER):
/*--------------------------------------------------------------------
        Purpose:  Validate the header values allowed
          Notes:
  -------------------------------------------------------------------- */
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateAuthorisedValues"
        &ServiceCallSignature = "INPUT  ipdAmount,  
                                 INPUT  ipiQuantity,  
                                 INPUT  iplActivateAuthorisedValues,
                                 INPUT  iplHeaderValuesUnlimited,   
                                 INPUT  ipcHeaderValuesAllowed,  
                                 OUTPUT opcFieldName,        
                                 OUTPUT opcMessage"} 
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER validateAuthorisedValues */
  
/*-- METHOD: validateLineRestriction --------------------------------- */
  METHOD PUBLIC LOGICAL validateLineRestriction
    (INPUT  ipcLineRestriction AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:  Validate the auth line restriction and return FALSE if   
                  auth line restriction is not valid
          Notes:
  -------------------------------------------------------------------- */
  DEFINE VARIABLE lLineRestriction AS LOGICAL     NO-UNDO.
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateLineRestriction"
        &ServiceCallSignature = "INPUT  ipcLineRestriction, 
                                 OUTPUT lLineRestriction"} 

    RETURN lLineRestriction.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER validateLineRestriction */

/*-- METHOD: validateProviderType ------------------------------------ */
  METHOD PUBLIC LOGICAL validateProviderType
    (INPUT ipcProviderType AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:  Validate the provider type and return FALSE if the  
                  provider type is not valid
          Notes:
  -------------------------------------------------------------------- */
  DEFINE VARIABLE lProviderType AS LOGICAL     NO-UNDO.
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateProviderType"
        &ServiceCallSignature = "INPUT  ipcProviderType, 
                                 OUTPUT lProviderType"} 

    RETURN lProviderType.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER validateProviderType */

/*-- METHOD: applyAuthRateConversion ----------------------------------------------- */
  METHOD PUBLIC LOGICAL applyAuthRateConversion
    (INPUT-OUTPUT DATASET FOR dsAuthorisation,
     INPUT  ipdAuthObj              AS DECIMAL,
     INPUT  ipdAuthRateControlObj   AS DECIMAL,
     INPUT  ipcCodeLinkCategory     AS CHARACTER, 
     INPUT  ipcOverrideBaseRate     AS CHARACTER, 
     INPUT  ipcOverrideArsRate      AS CHARACTER):
/*-----------------------------------------------------------------------------
    Purpose : Apply rate conversions on detail lines    
    Notes   : This routine will apply rate conversions on all auth detail
              line records in the passed in dataset. It will not save the converted 
              detail lines to the db. It will only pass back the auth dataset in the state 
              where all detail lines have been converted and will flag all updated detail
              lines with record action "MODIFY" so that further processing can 
              be handled by the saveAuthorisation service.
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "applyAuthRateConversion"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 INPUT ipdAuthObj,
                                 INPUT ipdAuthRateControlObj,
                                 INPUT ipcCodeLinkCategory,
                                 INPUT ipcOverrideBaseRate,
                                 INPUT ipcOverrideArsRate"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL applyAuthRateConversion*/

/*-- METHOD: revertAuthRateConversion----------------------------------------------- */
  METHOD PUBLIC LOGICAL revertAuthRateConversion
    (INPUT-OUTPUT DATASET FOR dsAuthorisation,
     INPUT  ipdAuthObj              AS DECIMAL,
     INPUT  ipdAuthRateControlObj   AS DECIMAL,
     INPUT  ipcCodeLinkCategory     AS CHARACTER, 
     INPUT  ipcOverrideBaseRate     AS CHARACTER,
     INPUT  ipcOverrideArsRate      AS CHARACTER,
     INPUT  ipcRevertBaseRate       AS CHARACTER, 
     INPUT  ipcRevertArsRate        AS CHARACTER):
/*-----------------------------------------------------------------------------
    Purpose : Revert auth  rate conversions on detail lines    
    Notes   : This routine will revert rate conversions on all auth detail
              line records in the passed in dataset. It will not save the converted 
              detail lines to the db. It will only pass back the auth dataset in the state 
              where all detail lines have been converted and will flag all updated detail
              lines with record action "MODIFY" so that further processing can 
              be handled by the saveAuthorisation service.
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "revertAuthRateConversion"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 INPUT ipdAuthObj,
                                 INPUT ipdAuthRateControlObj,
                                 INPUT ipcCodeLinkCategory,
                                 INPUT ipcOverrideBaseRate,
                                 INPUT ipcOverrideArsRate,
                                 INPUT ipcRevertBaseRate,
                                 INPUT ipcRevertArsRate" } 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL revertAuthRateConversion */

/*-- METHOD: checkForDiscounts----------------------------------------------- */
  METHOD PUBLIC LOGICAL checkForDiscounts
    (INPUT  ipdAuthObj              AS DECIMAL,
     INPUT  ipdAuthProviderObj      AS DECIMAL,
     INPUT  ipdAuthDetailObj        AS DECIMAL, 
     OUTPUT  ipcErrorMessage        AS CHARACTER):
/*-----------------------------------------------------------------------------
    Purpose : Method to check for Discounts on other Auth Levels    
    Notes   : Either an auth_obj or an auth_provider_obj or an auth_detail_obj must be input. 
              This will be the record that the user is currently trying to update with a Discount value. 
              Return an error if more than ONE input parameter has been passed in
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "checkForDiscounts"
        &ServiceCallSignature = "INPUT  ipdAuthObj,        
                                 INPUT  ipdAuthProviderObj,
                                 INPUT  ipdAuthDetailObj,  
                                 OUTPUT  ipcErrorMessage" } 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL checkForDiscounts */

/*-- METHOD: getStatusReasonDesc ----------------------------------------------- */
  METHOD PUBLIC LOGICAL getStatusReasonDesc
    (INPUT        ipdInsurerObj                  AS DECIMAL,
     INPUT        ipiOption                      AS INTEGER,   
     INPUT        ipdStartDate                   AS DATE,   
     INPUT        ipiStatus                      AS INTEGER, 
     INPUT        ipcStatusReasonKey             AS CHARACTER, 
     INPUT-OUTPUT ipocReasonType                 AS CHARACTER,
     OUTPUT       opcReasonDescription           AS CHARACTER,
     OUTPUT       opcErrorMessage                AS CHARACTER):
/*-----------------------------------------------------------------------------
  Get the status reason description 
  
  Use this signature if you want the method to do the NotePerAuthStatus rule check. 
  
  Parameters:  ipdInsurerObj                - Insurer Obj (optional - default 0 )
               ipiOption                    - Option code (optional - default 0 )
               ipdStartDate                 - Start date ( optional - will use TODAY if ?)
               ipiStatus                    - Status (required)
               ipcStatusReasonKey           - Status reason key (required)
               ipocReasonType               - Reason type (optional)
               opcReasonDescription         - Reason description
               opcErrorMessage              - Error message
  
  ----------------------------------------------------------------------------- */
   DEFINE VARIABLE lNotePerAuthStatusValidRule AS LOGICAL NO-UNDO.

   ASSIGN lNotePerAuthStatusValidRule = ? .

   THIS-OBJECT:getStatusReasonDesc(INPUT        ipdInsurerObj,                 
                                   INPUT        ipiOption,                     
                                   INPUT        ipdStartDate,                  
                                   INPUT        ipiStatus,                     
                                   INPUT        ipcStatusReasonKey,            
                                   INPUT-OUTPUT ipocReasonType,                
                                   INPUT-OUTPUT lNotePerAuthStatusValidRule,
                                   OUTPUT       opcReasonDescription,          
                                   OUTPUT       opcErrorMessage ).              

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL getStatusReasonDesc*/


/*-- METHOD: getStatusReasonDesc ----------------------------------------------- */
  METHOD PUBLIC LOGICAL getStatusReasonDesc
    (INPUT        ipdInsurerObj                  AS DECIMAL,
     INPUT        ipiOption                      AS INTEGER,   
     INPUT        ipdStartDate                   AS DATE,   
     INPUT        ipiStatus                      AS INTEGER, 
     INPUT        ipcStatusReasonKey             AS CHARACTER, 
     INPUT-OUTPUT ipocReasonType                 AS CHARACTER,
     INPUT-OUTPUT ipolNotePerAuthStatusValidRule AS LOGICAL,
     OUTPUT       opcReasonDescription           AS CHARACTER,
     OUTPUT       opcErrorMessage                AS CHARACTER):
/*-----------------------------------------------------------------------------
  Get the status reason description 
  Parameters:  ipdInsurerObj                - Insurer Obj (optional - default 0 )
               ipiOption                    - Option code (optional - default 0 )
               ipdStartDate                 - Start date ( optional - will use TODAY if ?)
               ipiStatus                    - Status (required)
               ipcStatusReasonKey           - Status reason key (required)
               ipocReasonType               - Reason type (optional)
               ipolNotePerAuthStatusValidRule  - Rule value for NotePerAuthStatus rule( optional) 
               opcReasonDescription         - Reason description
               opcErrorMessage              - Error message
  
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getStatusReasonDesc"
        &ServiceCallSignature = "INPUT        ipdInsurerObj,       
                                 INPUT        ipiOption,           
                                 INPUT        ipdStartDate,        
                                 INPUT        ipiStatus,           
                                 INPUT        ipcStatusReasonKey,  
                                 INPUT-OUTPUT ipocReasonType,       
                                 INPUT-OUTPUT ipolNotePerAuthStatusValidRule,       
                                 OUTPUT       opcReasonDescription,
                                 OUTPUT       opcErrorMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL getStatusReasonDesc*/

  /*-- METHOD: getAuthBMI ----------------------------------------------- */
  METHOD PUBLIC LOGICAL getAuthBMI
    (INPUT TABLE tt_dephealth,
     BUFFER btt_auth FOR tt_auth,
     OUTPUT      opdBMIValue AS DECIMAL):
/*-----------------------------------------------------------------------------
    Purpose : Get the Dependant BMI VALUE 
    Notes   : 
  ----------------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getAuthBMI"
        &ServiceCallSignature = "INPUT TABLE tt_dephealth,
                                 BUFFER btt_auth,
                                 OUTPUT      opdBMIValue"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL getAuthBMI*/


  /* -- METHOD checkWeekendPass -------------------------------- */
  METHOD PUBLIC LOGICAL checkWeekendPass
    ( INPUT  ipdAuthTypeObj         AS DECIMAL, 
      INPUT  ipdInsurerObj          AS DECIMAL,
      INPUT  ipiOptionCode          AS INTEGER,
      INPUT  ipdAuthStartDate       AS DATE, 
      INPUT  ipcTariffCode          AS CHARACTER,
      INPUT  ipiAuthStatus          AS INTEGER, 
      INPUT  ipcAuthStatusReason    AS CHARACTER, 
      OUTPUT oplWeekendPass         AS LOGICAL, 
      OUTPUT oplDeclineLOC          AS LOGICAL, 
      OUTPUT opcDeclineStatusReason AS CHARACTER, 
      OUTPUT opcError               AS CHARACTER):
  /*------------------------------------------------------------------
    Purpose   : To check if Weekend Pass should apply on a LOC line
    Parameters: 
    Notes     : 
  -------------------------------------------------------------------- */
    DEFINE VARIABLE cRuleDescription AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dLinkAuthRuleObj AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dAuthRuleObj     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE lSystemOwned     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE tEndDate         AS DATE       NO-UNDO.

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "checkWeekendPass"
         &ServiceCallSignature = "INPUT  ipdAuthTypeObj, 
                                  INPUT  ipdInsurerObj,
                                  INPUT  ipiOptionCode,
                                  INPUT  ipdAuthStartDate, 
                                  INPUT  ipcTariffCode,
                                  INPUT  ipiAuthStatus, 
                                  INPUT  ipcAuthStatusReason, 
                                  OUTPUT oplWeekendPass, 
                                  OUTPUT oplDeclineLOC, 
                                  OUTPUT opcDeclineStatusReason, 
                                  OUTPUT opcError"}

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* checkWeekendPass  */

  /*-- METHOD: getAuthTariffRestriction--------------------------------- */
  METHOD PUBLIC LOGICAL getAuthTariffRestriction
    (INPUT  ipcTariffCode           AS  CHARACTER,   
     INPUT  ipdTariffEffectDate     AS  DATE,  
     INPUT  ipcTariffBaseRate       AS  CHARACTER,
     INPUT  ipcTariffArsRate        AS  CHARACTER,
     INPUT  ipdAuthProviderObj      AS  DECIMAL,
     INPUT-OUTPUT TABLE tt_auth_detail,
     OUTPUT opcRestrictionTariffs   AS  CHARACTER,
     OUTPUT opcRestrictionMessage   AS  CHARACTER):
  /*--------------------------------------------------------------------
        Purpose:  Check for Tariff Restrictions for a Tariff code
          Notes:
     Parameters:  ipcTariffCode              - Tariff code
                  ipdTariffEffectDate        - Tariff effective date 
                  ipcTariffBaseRate          - Tariff base rate 
                  ipcTariffArsRate           - Tariff ars rate 
                  ipdAuthProviderObj         - Authorisation Provider obj for which the tariff code is checked 
                  temp table tt_auth_detail  - Authorisation Clinical Details 
                  opcRestrictionTariffs      - List of tariff codes to which restriction apply 
                  opcRestrictionMessage      - Tariff restriction description 
    -------------------------------------------------------------------- */
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "getAuthTariffRestriction"
        &ServiceCallSignature = "INPUT       ipcTariffCode,  
                                 INPUT       ipdTariffEffectDate,  
                                 INPUT       ipcTariffBaseRate,
                                 INPUT       ipcTariffArsRate,   
                                 INPUT       ipdAuthProviderObj,
                                 INPUT-OUTPUT TABLE tt_auth_detail BY-REFERENCE,
                                 OUTPUT      opcRestrictionTariffs,        
                                 OUTPUT      opcRestrictionMessage"} 
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC CHARACTER getAuthTariffRestriction */

  /* -- METHOD checkForCopay  -------------------------------- */
  METHOD PUBLIC LOGICAL checkForCopay 
    ( INPUT  ipdInsurerObj          AS DECIMAL,
      INPUT  ipiOptionCode          AS INTEGER,
      INPUT  ipdDate                AS DATE, 
      INPUT  ipcEmergency           AS CHARACTER,
      INPUT  iplPMB                 AS LOGICAL,
      INPUT  ipiDocNum              AS INTEGER,
      INPUT  ipiPrType              AS INTEGER,
      INPUT  ipiDocNegNum           AS INTEGER,
      INPUT  ipdAuthCopayControlObj AS DECIMAL,
      INPUT  ipcCopayOverrideNote   AS CHARACTER,
      INPUT  iplCopayProvider       AS LOGICAL,
      INPUT  ipiAuthStatus          AS INTEGER,
      INPUT  ipcAuthStatusNote      AS CHARACTER,
      INPUT  ipcProviderType        AS CHARACTER,
      OUTPUT opdAuthCopayControlObj AS DECIMAL,
      OUTPUT oplCopayProvider       AS LOGICAL,
      OUTPUT opiAuthStatus          AS INTEGER,
      OUTPUT opcAuthStatusNote      AS CHARACTER,
      OUTPUT opcWarning             AS CHARACTER,
      OUTPUT opcWarningType         AS CHARACTER,
      OUTPUT opdAuthCopayTypeObj    AS DECIMAL,
      OUTPUT oplCopayValueType      AS LOGICAL,
      OUTPUT opdCopayValue          AS DECIMAL,
      OUTPUT opcError               AS CHARACTER):
  /*------------------------------------------------------------------
    Purpose   : To check if Co-payment should apply on an Auth Provider
    Parameters: Input parameters: ipdInsurerObj            - Client
                                  ipiOptionCode            - Member option code
                                  ipdDate                  - Date for which setups must be checked.
                                  ipcEmergency             - Indicate if it is an emergency.
                                  iplPMB                   - Indicate if it is a PMB.
                                  ipiDocNum                - Doctor number/Provider number.
                                  ipiPrType                - Doctor/Provider discipline.
                                  ipiDocNegNum             - Provider DNU group.
                                  ipdAuthCopayControlObj   - Co-payment control obj to which the provider currently links.
                                  ipcCopayOverrideNote     - Co-payment override note captured when the authorisation is authorised.
                                  iplCopayProvider         - Indicate if provider is a co-payment provider.
                                  ipiAuthStatus            - Authorisation status before co-payment processing.
                                  ipcAuthStatusNote        - Authorisation status reason before co-payment processing.
                                  ipcProviderType          - Provider Type
                Output parameters: opdAuthCopayControlObj  - hac_auth_copay_control.auth_copay_control_obj.  Will not contain a value if no co-payment should apply.
                                   oplCopayProvider        - Output parameter that will indicate if the provider is a co-payment provider.
                                   opiAuthStatus           - hat_auth_copay_control.auth_status.  Will not contain a value if no co-payment should apply.
                                   opcAuthStatusNote       - hat_auth_copay_control.auth_status_note.  Will not contain a value if no co-payment should apply.
                                   opcWarning              - hat_auth_copay_control.warning_message.  Will not contain a value if no co-payment should apply.
                                   opcWarningType          - hat_auth_copay_control.warning_message_type.  Will not contain a value if no co-payment should apply.
                                   opdAuthCopayTypeObj     - hat_auth_copay_control.auth_copay_type_obj.  Will not contain a value if no co-payment should apply.
                                   oplCopayValueType       - hat_auth_copay_control.copayment_value_type.  Will not contain a value if no co-payment should apply.
                                   opdCopayValue           - hat_auth_copay_control.copayment_value.  Will not contain a value if no co-payment should apply.
                                   opcError                - Return error that should be handled by calling procedure.
    Notes     : 
  -------------------------------------------------------------------- */

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "checkForCopay"
         &ServiceCallSignature = "INPUT  ipdInsurerObj,
                                  INPUT  ipiOptionCode,
                                  INPUT  ipdDate,
                                  INPUT  ipcEmergency,
                                  INPUT  iplPMB,
                                  INPUT  ipiDocNum,
                                  INPUT  ipiPrType,
                                  INPUT  ipiDocNegNum,
                                  INPUT  ipdAuthCopayControlObj,
                                  INPUT  ipcCopayOverrideNote,
                                  INPUT  iplCopayProvider,
                                  INPUT  ipiAuthStatus,
                                  INPUT  ipcAuthStatusNote,
                                  INPUT  ipcProviderType,
                                  OUTPUT opdAuthCopayControlObj,
                                  OUTPUT oplCopayProvider,
                                  OUTPUT opiAuthStatus,
                                  OUTPUT opcAuthStatusNote,
                                  OUTPUT opcWarning,
                                  OUTPUT opcWarningType,
                                  OUTPUT opdAuthCopayTypeObj,   
                                  OUTPUT oplCopayValueType,     
                                  OUTPUT opdCopayValue,
                                  OUTPUT opcError" }         

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* checkForCopay */

  /* -- METHOD checkForCopayDetailItem  -------------------------------- */
  METHOD PUBLIC LOGICAL checkForCopayDetailItem 
    ( INPUT  ipdInsurerObj       AS DECIMAL  ,
      INPUT  ipiOptioncode       AS INTEGER  ,
      INPUT  ipdDate             AS DATE     ,
      INPUT  ipcEntity           AS CHARACTER,
      INPUT  ipdEntityObj        AS DECIMAL  ,
      INPUT  ipcEntityKey        AS CHARACTER,
      INPUT  ipcEntityAltValue   AS CHARACTER,
      INPUT  ipiPrType           AS INTEGER  ,
      INPUT  ipcEmergency        AS CHARACTER,
      INPUT  iplPMB              AS LOGICAL  ,
      INPUT  iplAuthorisation    AS LOGICAL  ,
      OUTPUT opcWarning          AS CHARACTER,
      OUTPUT opcWarnType         AS CHARACTER,
      OUTPUT opdAuthCopayTypeObj AS DECIMAL  ,
      OUTPUT oplCopayValueType   AS LOGICAL  ,
      OUTPUT opdCopayValue       AS DECIMAL  ,
      OUTPUT opcError            AS CHARACTER ):
  /*------------------------------------------------------------------
    Purpose:     Determine if a co-payment must apply for:
                 o An Authorisation Clinical Detail Line when a detail line is 
                   loaded on an Authorisation.
                 o A Claim that links to an Authorisation when no Clinical 
                   Detail Line exists on the Authorisation.
    
    Parameters:  Input parameters:            
                       ipdInsurerObj     - Insurer obj.
                       ipiOptionCode     - Option code.
                       ipdDate           - Detail line start date or claim date.
                       ipcEntity         - Detail line entity mnemonic. Always required.
                       ipdEntityObj      - Detail line entity obj.
                       ipcEntityKey      - Detail line key.
                       ipcEntityAltValue - Detail line entity alt value - Not required if called from Auths. Required if called from claims.
                       ipiPrType         - Discipline.
                       ipcEmergency      - Indicate if it is an Emergency.
                       iplPMB            - Indicate if it is a PMB.
                       iplAuthorisations - Indicate where method is called from. Format Authorisation/Claim.
                 Output parameters:
                        opcWarning          - Details if a warning is returned.
                        opcWarnType         - Indicate the waring type - Warn or Warnack.
                        opdAuthCopayTypeObj - hac_auth_copay_detail_item.copay_type_obj. No value = no co-payment must apply.
                        oplCopayValueType   - hac_auth_copay_detail_item.copayment_value_type. No value = no co-payment must apply.
                        opdCopayValue       - hac_auth_copay_detail_item.copayment_value. No value = no co-payment must apply.
                        opcError            - Details if an error is returned.
    Notes     : 
  -------------------------------------------------------------------- */

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "checkForCopayDetailItem"
         &ServiceCallSignature = "INPUT  ipdInsurerObj      ,
                                  INPUT  ipiOptioncode      ,
                                  INPUT  ipdDate            ,
                                  INPUT  ipcEntity          ,
                                  INPUT  ipdEntityObj       ,
                                  INPUT  ipcEntityKey       ,
                                  INPUT  ipcEntityAltValue  ,
                                  INPUT  ipiPrType          ,
                                  INPUT  ipcEmergency       ,
                                  INPUT  iplPMB             ,
                                  INPUT  iplAuthorisation   ,
                                  OUTPUT opcWarning         ,
                                  OUTPUT opcWarnType        ,
                                  OUTPUT opdAuthCopayTypeObj,
                                  OUTPUT oplCopayValueType  ,
                                  OUTPUT opdCopayValue      ,
                                  OUTPUT opcError           " }         

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* checkForCopayDetailItem */

/* -- METHOD checkForPenalty  ------------------------------ */
  METHOD PUBLIC LOGICAL checkForPenalty 
    ( INPUT  ipdInsurerObj          AS DECIMAL,
      INPUT  ipiOptionCode          AS INTEGER,
      INPUT  ipdStartDate           AS DATE, 
      INPUT  ipdCreateDate          AS DATE, 
      INPUT  ipcEmergency           AS CHARACTER,
      INPUT  ipcPenalty             AS CHARACTER,
      INPUT  ipcLateAuth            AS CHARACTER,
      INPUT  ipiPrType              AS INTEGER,
      OUTPUT opcPenalty             AS CHARACTER,
      OUTPUT opcLateAuth            AS CHARACTER,
      OUTPUT opcPenaltyWarning      AS CHARACTER,
      OUTPUT opcPenaltyWarningType  AS CHARACTER,
      OUTPUT opdPenaltyCopayTypeObj AS DECIMAL,
      OUTPUT opcPenaltyValueType    AS CHARACTER,               
      OUTPUT opdPenaltyValue        AS DECIMAL,
      OUTPUT opcPenaltyError        AS CHARACTER,
      OUTPUT oplClearReason         AS LOGICAL):
  /*------------------------------------------------------------------
    Purpose   : To check if a Penalty should apply on an Auth Provider
    Parameters: Input parameters:  ipdInsurerObj           - Client
                                   ipiOptionCode           - Member option code
                                   ipdStartDate            - Authorisation start date.
                                   ipdCreateDate           - Date authorisation is captured.
                                   ipcEmergency            - Indicate if it is an emergency.
                                   ipcPenalty              - Indicate if a penalty must apply.
                                   ipcLateAuth             - Indicate if it is a late auth.
                                   ipiPrType               - Provider discipline exceptions.
                Output parameters: opcPenalty              - Indicate if a penalty must apply.
                                   opcLateAuth             - Indicate if it is a late auth.
                                   opcPenaltyWarning       - Indicate if a warning must be populated.           
                                   opcPenaltyWarningType   - Indicate warning type.                             
                                   opdPenaltyCopayTypeObj  - Co-payment type that must apply for a penalty.     
                                   opcPenaltyValueType     - Indicate if a rand value or percentage must apply. 
                                   opdPenaltyValue         - Penalty value that must apply.                     
                                   opcPenaltyError         - Error that will be returned if any setups are missing.
                                   oplClearReason          - Logical to indicate if we need to clear the penalty reason.
    Notes     : 
  -------------------------------------------------------------------- */

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "checkForPenalty"
         &ServiceCallSignature = "INPUT  ipdInsurerObj,
                                  INPUT  ipiOptionCode,
                                  INPUT  ipdStartDate,
                                  INPUT  ipdCreateDate,
                                  INPUT  ipcEmergency,
                                  INPUT  ipcPenalty,
                                  INPUT  ipcLateAuth,              
                                  INPUT  ipiPrType,
                                  OUTPUT opcPenalty,
                                  OUTPUT opcLateAuth,
                                  OUTPUT opcPenaltyWarning,
                                  OUTPUT opcPenaltyWarningType,
                                  OUTPUT opdPenaltyCopayTypeObj,   
                                  OUTPUT opcPenaltyValueType,     
                                  OUTPUT opdPenaltyValue,
                                  OUTPUT opcPenaltyError,
                                  OUTPUT oplClearReason" }         

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* checkForPenalty */

/* -- METHOD determineTariffModifierType  ------------------------------ */
  METHOD PUBLIC LOGICAL determineTariffModifierType 
    (INPUT  ipdTariffLinkObj    AS DECIMAL , 
     INPUT  ipdStartDate        AS DATE, 
     OUTPUT oplParentOfModifier AS LOGICAL,
     OUTPUT oplChildModifier    AS LOGICAL):
  /*------------------------------------------------------------------
    Purpose   : Determine if the tariff link obj is set up as a parent 
                to another modifer and/or if it set up as a child modifier
                to another tariff link                   
    Notes     : 
  -------------------------------------------------------------------- */

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "determineTariffModifierType"
         &ServiceCallSignature = "INPUT  ipdTariffLinkObj, 
                                  INPUT  ipdStartDate, 
                                  OUTPUT oplParentOfModifier, 
                                  OUTPUT oplChildModifier " }         

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* determineTariffModifierType */

/*-- METHOD: startAuthCopyJob  --------------------------------- */
  METHOD PUBLIC LOGICAL startAuthCopyJob
    (INPUT-OUTPUT TABLE tt_authcopy):
/*--------------------------------------------------------------------
        Purpose: To start an Authorisation Copy Job in Job Manager
          Notes:
  -------------------------------------------------------------------- */
  &IF {&DBDFMA} >= 010195 &THEN  
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "startAuthCopyJob"
        &ServiceCallSignature = "INPUT-OUTPUT TABLE tt_authcopy"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  &ENDIF
  END METHOD. /* METHOD PUBLIC CHARACTER startAuthCopyJob */

/* -- METHOD calcAuthQtyFromMinutes  ------------------------------ */
  METHOD PUBLIC LOGICAL calcAuthQtyFromMinutes 
    ( INPUT  ipdInsurerObj          AS DECIMAL,
      INPUT  ipiOptionCode          AS INTEGER,
      INPUT  ipdStartDate           AS DATE, 
      INPUT  ipdMinutesAuthTypeObj  AS DECIMAL, 
      INPUT  ipiMinutesAuth         AS INTEGER,
      OUTPUT opdQuantityAuth        AS DECIMAL,
      OUTPUT opcMinutesCalcRule     AS CHARACTER,
      OUTPUT opcError               AS CHARACTER):
  /*------------------------------------------------------------------
    Purpose   : Calculation of Quantity Authorised from Minutes Authorised
    Parameters: Input parameters:  ipdInsurerObj           - Client.                              
                                   ipiOptionCode           - Member option code.                  
                                   ipdStartDate            - Authorisation detail line start date.
                                   ipdMinutesAuthTypeObj   - Tariff Type Obj for minutes.         
                                   ipiMinutesAuth          - Minutes Authorised on detail line.
                Output parameters: opdQuantityAuth         - Quantity Authorised calculated.                 
                                   opcMinutesCalcRule      - Rule used in calculation of Quantity Authorised.
                                   opcError                - If any error is picked up in calculation. 
    Notes     : 
  -------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN  
    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "calcAuthQtyFromMinutes"
         &ServiceCallSignature = "INPUT  ipdInsurerObj,        
                                  INPUT  ipiOptionCode,        
                                  INPUT  ipdStartDate,         
                                  INPUT  ipdMinutesAuthTypeObj,
                                  INPUT  ipiMinutesAuth,       
                                  OUTPUT opdQuantityAuth,      
                                  OUTPUT opcMinutesCalcRule,      
                                  OUTPUT opcError" }         
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
&ENDIF
  END METHOD.  /* calcAuthQtyFromMinutes */

/*-- METHOD: copyAuth  --------------------------------- */
  METHOD PUBLIC LOGICAL copyAuth
    (INPUT-OUTPUT TABLE tt_authcopy,
     OUTPUT TABLE tt_auth_copy_error ):
/*--------------------------------------------------------------------
        Purpose: Copy an auth/batch of auths according to the criteria in
                 the tt_authcopy temp table . Return all errors related 
                 to the copy in tt_copy_error
          Notes:
  -------------------------------------------------------------------- */
  &IF {&DBDFMA} >= 010195 &THEN  
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "copyAuth"
        &ServiceCallSignature = "INPUT-OUTPUT TABLE tt_authcopy,
                                 OUTPUT TABLE tt_auth_copy_error"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  &ENDIF
  END METHOD. /* METHOD PUBLIC CHARACTER copyAuth */


/*-- METHOD: applyAuthLimitControls  --------------------------------- */
  METHOD PUBLIC LOGICAL applyAuthLimitControls 
    (INPUT-OUTPUT DATASET FOR dsAuthorisation,
     INPUT ipdAuthObj       AS DECIMAL,  
     INPUT ipcAuthLevel     AS CHARACTER,
     INPUT ipdAuthLevelObj  AS DECIMAL,  
     INPUT ipdAmount        AS DECIMAL,  
     INPUT ipdQuantity      AS DECIMAL  
 ):
/*--------------------------------------------------------------------
        Purpose: 
        
          Notes:
  -------------------------------------------------------------------- */
  &IF {&DBDFMA} >= 010195 &THEN  
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "applyAuthLimitControls "
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 INPUT ipdAuthObj ,     
                                 INPUT ipcAuthLevel  ,  
                                 INPUT ipdAuthLevelObj ,
                                 INPUT ipdAmount  ,     
                                 INPUT ipdQuantity  "} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  &ENDIF
  END METHOD. /* METHOD PUBLIC CHARACTER applyAuthLimitControls */

  /*-- METHOD: getCopayFlags  --------------------------------- */
  METHOD PUBLIC LOGICAL getCopayFlags
    (BUFFER btt_auth FOR tt_auth,
     OUTPUT opcCopayFlagList AS CHARACTER ):
/*--------------------------------------------------------------------
        Purpose: Determine the flags which will be applicable for the 
                 Copayment/penalty processing.
                 The following flags, depending on auth type set up, 
                 could be linked to the main provider:
                 -PENALTY
                 -LATEAUTH
                 -EMERGENCY
                 -COPAY 
          Notes:
  -------------------------------------------------------------------- */
  &IF {&DBDFMA} >= 010195 &THEN  
    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "getCopayFlags"
        &ServiceCallSignature = "BUFFER btt_auth ,
                                 OUTPUT opcCopayFlagList"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  &ENDIF
  END METHOD. /* METHOD PUBLIC CHARACTER getCopayFlags */

  /*-- METHOD: checkLimits  --------------------------------- */
  METHOD PUBLIC LOGICAL checkLimits
    (INPUT  ipdInsurerObj      AS DECIMAL,    /* Client obj. */
     INPUT  ipiOptionCode      AS INTEGER,    /* Member option code. */
     INPUT  ipcMemNum          AS CHARACTER,  /* Member number. */
     INPUT  ipiDependant       AS INTEGER,    /* Dependant number.  */
     INPUT  ipdDate            AS DATE,       /* Date for which limits must be checked.  */
     INPUT  ipiClaimCode       AS INTEGER,    /* Claim code for which limits must be checked. */
     INPUT  ipcClaimtype       AS CHARACTER,  /* Claim type for which limits must be checked. */
     INPUT  ipdAmount          AS DECIMAL,    /* Authorised amount. */
     INPUT  ipdQuantity        AS DECIMAL,    /* Authorised quantity. */
     INPUT  iplPMBIndicator    AS LOGICAL,    /* PMB Indicator. */
     INPUT  ipcAuthLevel       AS CHARACTER,  /* Indicate for which level on Authorisation limit checking must be done: Header, Provider or Detail. */
     INPUT  ipdAuthObj         AS DECIMAL,    /* hat_auth.auth_ob will be used to determine if limit checking must apply */
     INPUT  ipdAuthProviderObj AS DECIMAL,    /* hat_auth_provider.auth_provider_obj will be used to determine if limit checking must apply */
     OUTPUT opdAmount          AS DECIMAL,    /* Authorised amount allowed after Limit checking. */
     OUTPUT opdQuantity        AS DECIMAL,    /* Authorised quantity allowed after Limit checking. */
     OUTPUT opdPMBValue        AS DECIMAL,    /* PMB Value after Limit checking. */
     OUTPUT opiAuthStatus      AS INTEGER,    /* Authorised status allowed after Limit checking. Must be defined with an initial unknown value. */
     OUTPUT opcAuthStatusNote  AS CHARACTER,  /* Authorised status note after Limit checking.  */
     OUTPUT opcClaimType       AS CHARACTER,  /* Claim type after Limit checking. */
     OUTPUT opcError           AS CHARACTER,  /* Any errors returned from Limit checking. */
     OUTPUT oplLimitsChecked   AS LOGICAL,    /* Indicate if limit checking is applied */
     OUTPUT TABLE tt_limitwarn                /* Temp table with limit warnings after Limit checking. */
    ):
/*--------------------------------------------------------------------
        Purpose: Do Limit checking on all the levels of the 
                 Authorisation where authorised values can be specified 
          Notes:
  -------------------------------------------------------------------- */
  &IF {&DBDFMA} >= 010195 &THEN  
    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "checkLimits"
        &ServiceCallSignature = "INPUT  ipdInsurerObj     , 
                                 INPUT  ipiOptionCode     , 
                                 INPUT  ipcMemNum         , 
                                 INPUT  ipiDependant      , 
                                 INPUT  ipdDate           , 
                                 INPUT  ipiClaimCode      , 
                                 INPUT  ipcClaimtype      , 
                                 INPUT  ipdAmount         , 
                                 INPUT  ipdQuantity       , 
                                 INPUT  iplPMBIndicator   , 
                                 INPUT  ipcAuthLevel      , 
                                 INPUT  ipdAuthObj        ,
                                 INPUT  ipdAuthProviderObj,
                                 OUTPUT opdAmount         , 
                                 OUTPUT opdQuantity       , 
                                 OUTPUT opdPMBValue       , 
                                 OUTPUT opiAuthStatus     , 
                                 OUTPUT opcAuthStatusNote , 
                                 OUTPUT opcClaimType      , 
                                 OUTPUT opcError          , 
                                 OUTPUT oplLimitsChecked  ,
                                 OUTPUT TABLE tt_limitwarn"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  &ENDIF
  END METHOD. /* METHOD PUBLIC CHARACTER checkLimits */

END CLASS. /* ma.cls.basemaauthservice INHERITS cls.mipservice */

