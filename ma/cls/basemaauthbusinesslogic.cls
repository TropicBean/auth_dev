/*------------------------------------------------------------------------
    Component: Healthcare SOA
    Author   : MMP

    Purpose  : Healthcare Authorisation Business Logic Services
  ------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthbusinesslogic INHERITS cls.mipservice:

  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthds.i }
  
  { ma/inc/maauthflagvalueds.i }
  
DEFINE PUBLIC PROPERTY AuthBusinessLogic AS HANDLE NO-UNDO GET. PROTECTED SET.

/* -- METHOD: initializeService -------------------------------------- */
  METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess          = SUPER:initializeService()

        AuthBusinessLogic = ObjectStack.

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */


/* -- METHOD: activateMainCode --------------------------------------- */
  METHOD PUBLIC LOGICAL activateMainCode
    (INPUT  ipdInsurerObj      AS DECIMAL,
     INPUT  ipiOptionCode      AS INTEGER,
     INPUT  ipdDate            AS DATE,
     OUTPUT oplEnableMainCode  AS LOGICAL):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "activateMainCode"
        &ServiceCallSignature = "INPUT ipdInsurerObj,
                                 INPUT ipiOptionCode,
                                 INPUT ipdDate,  
                                 OUTPUT oplEnableMainCode"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL activateMainCode */  
  
/* -- METHOD: activateMainProvider ----------------------------------- */
  METHOD PUBLIC LOGICAL activateMainProvider
    (INPUT  ipdAuthTypeObj  AS DECIMAL,
     INPUT  ipdInsurerObj   AS DECIMAL,
     INPUT  ipiOptionCode   AS INTEGER,
     INPUT  ipdDate         AS DATE,
     OUTPUT oplEnable       AS LOGICAL):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "activateMainProvider"
        &ServiceCallSignature = "INPUT  ipdAuthTypeObj,
                                 INPUT  ipdInsurerObj,
                                 INPUT  ipiOptionCode,
                                 INPUT  ipdDate,  
                                 OUTPUT oplEnable"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL activateMainProvider */    

/* -- METHOD: activateProcedureDate ----------------------------------- */
  METHOD PUBLIC LOGICAL activateProcedureDate
    (INPUT  ipdInsurerObj           AS DECIMAL  ,
     INPUT  ipiOptionCode           AS INTEGER  ,
     INPUT  ipdDate                 AS DATE     ,
     INPUT  ipcCptCode              AS CHARACTER,
     INPUT  ipcServiceType          AS CHARACTER,
     OUTPUT oplEnableProcedureDate  AS LOGICAL  ,
     OUTPUT opcProcedureDateAction  AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "activateProcedureDate"
        &ServiceCallSignature = "INPUT  ipdInsurerObj,
                                 INPUT  ipiOptionCode,
                                 INPUT  ipdDate,
                                 INPUT  ipcCptCode,
                                 INPUT  ipcServiceType,
                                 OUTPUT oplEnableProcedureDate,
                                 OUTPUT opcProcedureDateAction"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL activateProcedureDate */    

/* -- METHOD: activateMinutes --------------------------------------- */
  METHOD PUBLIC LOGICAL activateMinutes
    (INPUT  ipdInsurerObj           AS  DECIMAL,  
     INPUT  ipiOptionCode           AS  INTEGER,  
     INPUT  ipdStartDate            AS  DATE,     
     INPUT  ipdTariffLinkObj        AS  DECIMAL,  
     OUTPUT opcError                AS  CHARACTER,
     OUTPUT opdMinutesTariffTypeObj AS  DECIMAL,  
     OUTPUT oplActivateMinutes      AS  LOGICAL ):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "activateMinutes"
        &ServiceCallSignature = "INPUT  ipdInsurerObj,           
                                 INPUT  ipiOptionCode,          
                                 INPUT  ipdStartDate,           
                                 INPUT  ipdTariffLinkObj,       
                                 OUTPUT opcError,               
                                 OUTPUT opdMinutesTariffTypeObj,
                                 OUTPUT oplActivateMinutes"}    

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL activateMinutes */    

/* -- METHOD: authDependantUpdatable --------------------------------- */
  METHOD PUBLIC LOGICAL authDependantUpdatable
    (INPUT ipdInsurerObj    AS DECIMAL,
	 INPUT ipdAuthTypeObj   AS DECIMAL,
     INPUT ipiOptionCode    AS INTEGER,
     INPUT ipdDate          AS DATE,
     INPUT ipdAmountPaid    AS DECIMAL,
     INPUT ipdQuantityPaid  AS DECIMAL):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lAllowUpdate AS LOGICAL     NO-UNDO.

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "authDependantUpdatable"
        &ServiceCallSignature = "INPUT ipdInsurerObj,
                                 INPUT ipdAuthTypeObj,
                                 INPUT ipiOptionCode,
                                 INPUT ipdDate,
                                 INPUT ipdAmountPaid,
                                 INPUT ipdQuantityPaid, 
                                 OUTPUT lAllowUpdate"} 

    RETURN lAllowUpdate.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL authDependantUpdatable*/  
  
/* -- METHOD: authNextOfKinActivated --------------------------------- */
  METHOD PUBLIC LOGICAL authNextOfKinActivated
    (INPUT ipdInsurerObj    AS DECIMAL,
     INPUT ipiOptionCode    AS INTEGER,
     INPUT ipdDate          AS DATE):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lNOKActivated AS LOGICAL     NO-UNDO.

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "authNextOfKinActivated"
        &ServiceCallSignature = "INPUT ipdInsurerObj,
                                 INPUT ipiOptionCode,
                                 INPUT ipdDate, 
                                 OUTPUT lNOKActivated"} 

    RETURN lNOKActivated.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL authNextOfKinActivated*/  

/* -- METHOD: authNumViewable ---------------------------------------- */
  METHOD PUBLIC LOGICAL authNumViewable
    (INPUT ipiAuthStatus   AS INTEGER,
     INPUT ipdInsurerObj   AS DECIMAL,
     INPUT ipiOptionCode   AS INTEGER,
     INPUT ipdAuthDate     AS DATE):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lAuthNumViewable AS LOGICAL     NO-UNDO.

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "authNumViewable"
        &ServiceCallSignature = "INPUT ipiAuthStatus,
                                 INPUT ipdInsurerObj,
                                 INPUT ipiOptionCode,
                                 INPUT ipdAuthDate,  
                                 OUTPUT lAuthNumViewable"} 

    RETURN lAuthNumViewable.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL authNumViewable*/  
  
/* -- METHOD: authOptionMandatory ------------------------------------ */
  METHOD PUBLIC LOGICAL authOptionMandatory
    (INPUT ipiOptionCode AS INTEGER):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lMandatory AS LOGICAL     NO-UNDO.
    

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "authOptionMandatory"
        &ServiceCallSignature = "INPUT ipiOptionCode,
                                 OUTPUT lMandatory"} 

    RETURN lMandatory.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL authOptionMandatory */
  
/* -- METHOD: authRefNumUpdatable ------------------------------------ */
  METHOD PUBLIC LOGICAL authRefNumUpdatable
    (INPUT ipdAuthTypeObj   AS DECIMAL,
     INPUT ipdInsurerObj    AS DECIMAL,
     INPUT ipiOptionCode    AS INTEGER,
     INPUT ipdEffectiveDate AS DATE):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lAllowUpdate AS LOGICAL     NO-UNDO.
    

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "authRefNumUpdatable"
        &ServiceCallSignature = "INPUT ipdAuthTypeObj,
                                 INPUT ipdInsurerObj,
                                 INPUT ipiOptionCode,
                                 INPUT ipdEffectiveDate, 
                                 OUTPUT lAllowUpdate"} 

    RETURN lAllowUpdate.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL authRefNumUpdatable*/

/* -- METHOD: authUpdatesAllowed ------------------------------------ */
  METHOD PUBLIC LOGICAL authUpdatesAllowed
    (INPUT  ipdAuthObj         AS DECIMAL,
     INPUT  ipdAuthTypeObj     AS DECIMAL,
     INPUT  ipdInsurerObj      AS DECIMAL,
     INPUT  ipiOptionCode      AS INTEGER,
     INPUT  ipdAuthDate        AS DATE,
     OUTPUT oplUpdatesAllowed  AS LOGICAL,
     OUTPUT opcValidMessage    AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lAllowUpdate AS LOGICAL     NO-UNDO.
    

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "authUpdatesAllowed"
        &ServiceCallSignature = "INPUT  ipdAuthObj,
                                 INPUT  ipdAuthTypeObj,
                                 INPUT  ipdInsurerObj,
                                 INPUT  ipiOptionCode,
                                 INPUT  ipdAuthDate, 
                                 OUTPUT oplUpdatesAllowed,
                                 OUTPUT opcValidMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL authUpdatesAllowed */

  /*-- METHOD: checkForRateChange ----------------------------------------------- */
  METHOD PUBLIC LOGICAL checkForRateChange
    (INPUT-OUTPUT DATASET dsAuthorisation):
/*-----------------------------------------------------------------------------
    Purpose : Check for Rate change
    Notes   : 
  ----------------------------------------------------------------------------- */
    
    {                                                 
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "checkForRateChange"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE"
     }                 
        
        RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL checkForRateChange*/
  
  /*-- METHOD: populateAdditionalData ----------------------------------------------- */
  METHOD PUBLIC LOGICAL populateAdditionalData
    (INPUT-OUTPUT DATASET dsAuthorisation):
/*-----------------------------------------------------------------------------
    Purpose : Populate Additional Data used for Rendering containers
    Notes   : 
  ----------------------------------------------------------------------------- */
    
    {                                                 
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "populateAdditionalData"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE"
     }                 
        
        RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL populateAdditionalData*/


  /*-- METHOD: reselectCrosswalk ---------------------------------------------- */
  METHOD PUBLIC LOGICAL reselectCrosswalk
    ( INPUT-OUTPUT DATASET dsAuthorisation ):
  /*---------------------------------------------------------------------------
    Purpose : Delete Authorisation details to reselect a new Crosswalk.
              A.k.a. Authorisation Reguide
    Notes   : 
  ----------------------------------------------------------------------------- */
    { mip/inc/mipservicecall.i
        &ServiceProcedureName = "reselectCrosswalk"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE" }

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /*METHOD PUBLIC LOGICAL reselectCrosswalk*/


  /* -- METHOD: authServiceTypeUpdatable  ---------------------------- */
  METHOD PUBLIC LOGICAL authServiceTypeUpdatable
    (INPUT  ipdInsurerObj              AS DECIMAL,
     INPUT  ipiOptionCode              AS INTEGER,
     INPUT  ipdDate                    AS DATE,
     OUTPUT oplServiceTypeAllowUpdate  AS LOGICAL):
  /*------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */   

    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "authServiceTypeUpdatable"
        &ServiceCallSignature = "INPUT  ipdInsurerObj,
                                 INPUT  ipiOptionCode,
                                 INPUT  ipdDate, 
                                 OUTPUT oplServiceTypeAllowUpdate"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL authServiceTypeUpdatable */

  /* -- METHOD deleteAuthFlagHistory --------------------------------- */
  METHOD PUBLIC LOGICAL deleteAuthFlagHistory
    ( INPUT ipcOwningEntityMnemonic AS CHARACTER,
      INPUT ipdOwningObj            AS DECIMAL,
      INPUT ipcOwningKey            AS CHARACTER,
      INPUT-OUTPUT ipoErrorObject   AS cls.maerrorobject) :
  /*------------------------------------------------------------------
        Purpose   : Delete flag value history records for a specified
                    owning entity.
        Parameters: 
  -------------------------------------------------------------------- */
    
    { mip/inc/mipservicecall.i 
      &ServiceProcedureName = "deleteAuthFlagHistory"
      &ServiceCallSignature = "INPUT  ipcOwningEntityMnemonic,   
                               INPUT  ipdOwningObj,   
                               INPUT  ipcOwningKey,
                               INPUT-OUTPUT ipoErrorObject"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* deleteAuthFlagHistory  */
  
  METHOD PUBLIC LOGICAL saveAuthorisation
    ( INPUT-OUTPUT DATASET FOR dsAuthorisation,
      INPUT-OUTPUT DATASET FOR dsAuthFlagValue  ):
      
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "saveAuthorisation"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE" } /*Passing one of these parameters to a local routine using the BY-REFERENCE option allows the calling 
                                                                                       routine and the called routine to access the same object instance*/
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  
  END METHOD.
  
  METHOD PUBLIC LOGICAL saveAuthorisation
    ( INPUT-OUTPUT DATASET FOR dsAuthorisation ):
      
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "saveAuthorisation"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE,
                                 INPUT-OUTPUT DATASET dsAuthFlagValue BY-REFERENCE" } /*Passing one of these parameters to a local routine using the BY-REFERENCE option allows the calling 
                                                                                       routine and the called routine to access the same object instance*/
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
  
  END METHOD.

  /*
    Fetch Methods
  */
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthFlagValueDataset"  &SearchObject = "cls.maauthflagvaluesearch" }          
        
  /*
    Save Methods
  */
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveAuthFlagValue" &DatasetName = "dsAuthFlagValue"  }    
    
  /*
    Validate Methods
  */
  { ma/inc/madataretrievalservice.i &Segment = "validateDataMethod" &MethodName = "validateAuthorisation"  &DatasetName = "dsAuthorisation"  }    

/* -- METHOD: validateAuthTypeUsage ---------------------------------- */
  METHOD PUBLIC LOGICAL validateAuthTypeUsage
    (INPUT  ipdAuthObj             AS DECIMAL,  
     INPUT  ipdInsurerObj          AS DECIMAL,
     INPUT  ipiOptionCode          AS INTEGER,  
     INPUT  ipcMemberNumber        AS CHARACTER,
     INPUT  ipiDependant           AS INTEGER,  
     INPUT  iptAuthDate            AS DATE,
     INPUT  ipcValidationType      AS CHARACTER,
     INPUT  ipcCode                AS CHARACTER,
     OUTPUT oplValidAuthTypeUsage  AS LOGICAL,
     OUTPUT opcValidMessage        AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateAuthTypeUsage"
        &ServiceCallSignature = "INPUT  ipdAuthObj           ,  
                                 INPUT  ipdInsurerObj        ,
                                 INPUT  ipiOptionCode        ,  
                                 INPUT  ipcMemberNumber      ,
                                 INPUT  ipiDependant         ,  
                                 INPUT  iptAuthDate          , 
                                 INPUT  ipcValidationType    ,
                                 INPUT  ipcCode              ,
                                 OUTPUT oplValidAuthTypeUsage,
                                 OUTPUT opcValidMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL validateAuthTypeUsage */

/* -- METHOD: validateClaimCode -------------------------------------- */
  METHOD PUBLIC LOGICAL validateClaimCode
    (INPUT  ipdInsurerObj      AS DECIMAL,
     INPUT  ipiOptionCode      AS INTEGER,  
     INPUT  ipcMemberNumber    AS CHARACTER,
     INPUT  ipiDependant       AS INTEGER,  
     INPUT  ipiClaimCode       AS INTEGER,  
     INPUT  ipdAuthTypeObj     AS DECIMAL,  
     INPUT  iptAuthDate        AS DATE,
     INPUT  ipcProviderType    AS CHARACTER,
     INPUT  ipiPrType          AS INTEGER,
     INPUT  ipiSubPrType       AS INTEGER,
     INPUT  ipiNegNumber       AS INTEGER,
     INPUT  ipcAuthLevel       AS CHARACTER,
     OUTPUT oplValidClaimCode  AS LOGICAL,
     OUTPUT opcValidMessage    AS CHARACTER):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateClaimCode"
        &ServiceCallSignature = "INPUT  ipdInsurerObj  ,
                                 INPUT  ipiOptionCode  ,  
                                 INPUT  ipcMemberNumber,
                                 INPUT  ipiDependant   ,  
                                 INPUT  ipiClaimCode   ,  
                                 INPUT  ipdAuthTypeObj ,  
                                 INPUT  iptAuthDate    , 
                                 INPUT  ipcProviderType,
                                 INPUT  ipiPrType      ,
                                 INPUT  ipiSubPrType   , 
                                 INPUT  ipiNegNumber   ,
                                 INPUT  ipcAuthLevel   ,
                                 OUTPUT oplValidClaimCode,
                                 OUTPUT opcValidMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL validateClaimCode */
  
/* -- METHOD: validateClaimType -------------------------------------- */
  METHOD PUBLIC LOGICAL validateClaimType
    (INPUT  ipdInsurerObj      AS DECIMAL,
     INPUT  ipiOptionCode      AS INTEGER,  
     INPUT  ipcClaimType       AS CHARACTER,  
     INPUT  ipdAuthTypeObj     AS DECIMAL,  
     INPUT  iptAuthDate        AS DATE,
     INPUT  ipcProviderType    AS CHARACTER,
     INPUT  ipiPrType          AS INTEGER,
     INPUT  ipiSubPrType       AS INTEGER,  
     INPUT  ipiNegNumber       AS INTEGER,
     INPUT  ipcAuthLevel       AS CHARACTER,
     OUTPUT oplValidClaimType  AS LOGICAL,
     OUTPUT opcValidMessage    AS CHARACTER):
/* -------------------------------------------------------------------
        Purpose:
          Notes:
   ------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateClaimType"
        &ServiceCallSignature = "INPUT  ipdInsurerObj    ,
                                 INPUT  ipiOptionCode    ,  
                                 INPUT  ipcClaimType     ,  
                                 INPUT  ipdAuthTypeObj   ,  
                                 INPUT  iptAuthDate      ,
                                 INPUT  ipcProviderType  ,
                                 INPUT  ipiPrType        ,
                                 INPUT  ipiSubPrType     ,
                                 INPUT  ipiNegNumber     ,
                                 INPUT  ipcAuthLevel     ,
                                 OUTPUT oplValidClaimType,    
                                 OUTPUT opcValidMessage"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL validateClaimType */

/* -- METHOD: activateEmergencyFlag  -------------------------------------- */
  METHOD PUBLIC LOGICAL activateEmergencyFlag  
    (INPUT  ipdAuthObj                AS DECIMAL,
     INPUT  ipdInsurerObj             AS DECIMAL,
     INPUT  ipiOptionCode             AS INTEGER,
     INPUT  ipdStartDate              AS DATE,
     OUTPUT oplActivateEmergencyFlag  AS LOGICAL ):
/* -------------------------------------------------------------------
        Purpose:
          Notes:
   ------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "activateEmergencyFlag  "
        &ServiceCallSignature = "INPUT  ipdAuthObj              ,
                                 INPUT  ipdInsurerObj           ,  
                                 INPUT  ipiOptionCode           ,  
                                 INPUT  ipdStartDate            ,  
                                 OUTPUT oplActivateEmergencyFlag"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL activateEmergencyFlag  */

/* -- METHOD: activatePenaltyFlag  -------------------------------------- */
  METHOD PUBLIC LOGICAL activatePenaltyFlag  
    (INPUT  ipdAuthObj                AS DECIMAL,
     INPUT  ipdInsurerObj             AS DECIMAL,
     INPUT  ipiOptionCode             AS INTEGER,
     INPUT  ipdStartDate              AS DATE,
     OUTPUT oplActivatePenaltyFlag    AS LOGICAL ):
/* -------------------------------------------------------------------
        Purpose:
          Notes:
   ------------------------------------------------------------------- */
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "activatePenaltyFlag"
        &ServiceCallSignature = "INPUT  ipdAuthObj              ,
                                 INPUT  ipdInsurerObj           ,  
                                 INPUT  ipiOptionCode           ,  
                                 INPUT  ipdStartDate            ,  
                                 OUTPUT oplActivatePenaltyFlag"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL activatePenaltyFlag  */

  
END CLASS. /* ma.cls.basemaauthbusinesslogic INHERITS cls.mipservice */


