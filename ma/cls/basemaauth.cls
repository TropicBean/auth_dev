/*------------------------------------------------------------------------
    $Id$

    Component: MIP SOA
       Author: Mip

      Purpose:
------------------------------------------------------------------------*/
CLASS ma.cls.basemaauth INHERITS cls.mipservice:

  {mip/inc/mipdefsharedclass.i}

    DEFINE TEMP-TABLE ttauth      NO-UNDO LIKE memauth
    FIELD  update-note            AS    CHARACTER  
    FIELD  requested-by           AS    CHARACTER  
    FIELD  note                   AS    CHARACTER
    FIELD  authlet-note           AS    CHARACTER
    FIELD  prim-diag-x            AS    CHARACTER
    FIELD  sec-diag-x             AS    CHARACTER
    FIELD  old-authstat           AS    INTEGER
    FIELD  paid                   AS    INTEGER
    .

  DEFINE TEMP-TABLE ttauthd  NO-UNDO LIKE memauthd
      FIELD old-start-date   AS   DATE
      FIELD old-end-date     AS   DATE
      FIELD old-claim-type   AS   CHARACTER
      FIELD old-claim-code   AS   INTEGER
      FIELD old-quantity     AS   DECIMAL
      FIELD old-amount-auth  AS   DECIMAL
      FIELD old-pmb-value    AS   DECIMAL.

  DEFINE TEMP-TABLE ttmemques NO-UNDO LIKE memques
      FIELD scheme-code   AS   INTEGER
      FIELD effect-date   AS   DATE
      FIELD question-man  AS   LOGICAL
      FIELD question-upd  AS   LOGICAL.

  /*Temp. Table to store additional external references captured.*/

  DEFINE TEMP-TABLE ttextref NO-UNDO
     FIELD reference-code      LIKE sysref.reference-code
     FIELD reference-value     LIKE extref.reference-value
     FIELD reference-mandatory LIKE schintref.reference-mandatory
     FIELD reference-desc      LIKE sysref.reference-desc
     FIELD old-reference-code  LIKE sysref.reference-code  
     FIELD old-reference-value LIKE extref.reference-value 
     FIELD reference-no        AS INTEGER.

DEFINE TEMP-TABLE ttmemauthic LIKE memauthic
  FIELD description AS CHARACTER
  FIELD deleted     AS LOGICAL
  FIELD code-x      AS CHARACTER
  FIELD ass-code-x  AS CHARACTER.

  DEFINE TEMP-TABLE ttmemauthc LIKE memauthc. 
   
  DEFINE TEMP-TABLE ttmemauthm LIKE memauthm.  

  DEFINE TEMP-TABLE tmpNote   NO-UNDO
     FIELD iSchemeCode    AS INTEGER
     FIELD cKey           AS CHARACTER
     FIELD cType          AS CHARACTER
     FIELD cNarration     AS CHARACTER
     FIELD tCreateDate    AS DATE
     FIELD cCreateUsrId   AS CHARACTER.

  DEFINE PUBLIC PROPERTY AuthNumber     AS CHARACTER      NO-UNDO GET. PROTECTED SET.
  DEFINE PUBLIC PROPERTY AuthInFocus    AS LOGICAL        NO-UNDO GET. PROTECTED SET.


  /* -- METHOD: initializeService --------------------------------- */
  METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  --------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess = SUPER:initializeService()

      NO-ERROR.

    {mip/inc/mipreturnerror.i}

    RETURN TRUE.

  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */

  /* -- METHOD: focusAuth ----------------------------------------- */
  METHOD PUBLIC LOGICAL focusAuth
    (INPUT ipcAuthNumber  AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  --------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    IF ipcAuthNumber = ? OR TRIM(ipcAuthNumber) = "":U
    THEN
      ASSIGN
          lSuccess = unfocusAuth()
        NO-ERROR.
    ELSE
      ASSIGN
          DataState     = "Focus":U
          AuthInFocus   = FALSE
          AuthNumber    = (IF ipcAuthNumber = ? THEN "":U ELSE ipcAuthNumber)
          AuthInFocus   = TRUE
          DataState     = "Updatable":U
          lSuccess      = TRUE
        NO-ERROR.

    {mip/inc/mipreturnerror.i}

    RETURN lSuccess.

  END METHOD. /* METHOD PUBLIC LOGICAL focusAuth */

  /* -- METHOD: saveChanges ----------------------------------------- */
  METHOD PUBLIC LOGICAL saveChanges
    ():
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  --------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess          AS LOGICAL     NO-UNDO.

    /* If there is nothing to save (Empty), the data did not change at
       all (Updatable) or the data should not have changed (ReadOnly)
       then we can return here without continuing */
    IF LOOKUP(DataState, "Empty,Updatable,ReadOnly":U) <> 0
    THEN
      RETURN TRUE.

    DO TRANSACTION:
      ASSIGN
        /*lSuccess = saveChanges()*/

          /* Now as part of this transaction, we should also commit any possible / multiple rem changes... */

          AuthInFocus = TRUE WHEN (AuthInFocus = FALSE)

          DataState = "Updatable":U
        NO-ERROR.

      {mip/inc/mipreturnerror.i}
    END. /* DO TRANSACTION */

    RETURN TRUE.

  END METHOD. /* METHOD PUBLIC LOGICAL saveChanges */

  /* -- METHOD: unfocusAuth --------------------------------------- */
  METHOD PUBLIC LOGICAL unfocusAuth
    ():
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  --------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        DataState     = "Focus":U
        AuthInFocus   = FALSE

      NO-ERROR.

    {mip/inc/mipreturnerror.i}

    RETURN TRUE.

  END METHOD. /* METHOD PUBLIC LOGICAL unfocusAuth */

  /* -- METHOD: xUnfocusAuth --------------------------------------- */
  METHOD PRIVATE CHARACTER xUnfocusAuth
    (INPUT ipcErrorMessage  AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  --------------------------------------------------------------------*/
    unfocusAuth() NO-ERROR.

    RETURN (ipcErrorMessage + (IF {&ErrorStatus} THEN {&Delim-Message} + {mip/inc/mipreturnvalue.i} ELSE "":U)).

  END METHOD. /* METHOD PRIVATE CHARACTER xUnfocusAuth */
  
/* -- METHOD: cancelAuth -------------------------------------------*/
  METHOD PUBLIC LOGICAL cancelAuth
    (INPUT  ipcMemNum      AS CHARACTER,
     INPUT  ipiAuthNum     AS INTEGER,
     INPUT  ipiClaimCode   AS INTEGER,
     INPUT  ipiSchemeCode  AS INTEGER,
     OUTPUT opcErrorText   AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: Cancel an auth
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'cancelAuth':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  ipiAuthNum,
                                 INPUT  ipiClaimCode,
                                 INPUT  ipiSchemeCode,
                                 OUTPUT opcErrorText"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL cancelAuth */ 

/* -- METHOD: followUpAuth -------------------------------------------*/
  METHOD PUBLIC LOGICAL followUpAuth
    (INPUT  ipiAuthNum         AS INTEGER,
     INPUT  ipdtFollowDate     AS DATE,
     INPUT  ipdtDischargeDate  AS DATE,
     INPUT  ipcFollowStatus    AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: Follow up on an auth
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'followUpAuth':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 INPUT  ipdtFollowDate,
                                 INPUT  ipdtDischargeDate,
                                 INPUT  ipcFollowStatus"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL followUpAuth */  
  
/* -- METHOD: createHistory -------------------------------------------*/
  METHOD PUBLIC LOGICAL createHistory
    (INPUT  ipiAuthNum         AS INTEGER,
     INPUT  iplQuestionnaire   AS LOGICAL,
     INPUT  iplTempTable       AS LOGICAL,
     INPUT  TABLE ttauth):
  /*--------------------------------------------------------------------
        Purpose: Follow up on an auth
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'createHistory':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 INPUT  iplQuestionnaire,
                                 INPUT  iplTempTable,
                                 INPUT TABLE ttauth"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL createHistory */  

/* -- METHOD: getAuthList -------------------------------------------*/
  METHOD PUBLIC LOGICAL getAuthList
    (INPUT        ipcMemNum        AS CHARACTER,
     INPUT        ipiAdminComp     AS INTEGER,
     INPUT        ipcUsrId         AS CHARACTER,
     INPUT        ipcMenuOption    AS CHARACTER,
     OUTPUT TABLE ttauth):
  /*--------------------------------------------------------------------
        Purpose: Get a list of all member's auths
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'getAuthList':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  ipiAdminComp,
                                 INPUT  ipcUsrId,
                                 INPUT  ipcMenuOption,
                                 OUTPUT TABLE ttauth"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL getAuthList */ 

/* -- METHOD: UpdateDetailLines -------------------------------------------*/
  METHOD PUBLIC LOGICAL UpdateDetailLines
    (INPUT       ipcMemNum        AS CHARACTER,
     INPUT       iplAddFlag       AS LOGICAL,
     INPUT       ipiSchemeCode    AS INTEGER,
     INPUT       ipiAuthNo        AS INTEGER,
     INPUT TABLE ttauth,
     INPUT TABLE ttauthd,
     OUTPUT      opcMessage       AS CHARACTER,  
     OUTPUT      opiClaimCode     AS INTEGER,
     OUTPUT      opdAuthAmt       AS DECIMAL):
  /*--------------------------------------------------------------------
        Purpose: Get a list of all member's auths
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'UpdateDetailLines':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  iplAddFlag,
                                 INPUT  ipiSchemeCode,
                                 INPUT  ipiAuthNo,
                                 INPUT TABLE ttauth,
                                 INPUT TABLE ttauthd,
                                 OUTPUT opcMessage,
                                 OUTPUT opiClaimCode,
                                 OUTPUT opdAuthAmt"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL UpdateDetailLines */ 

/* -- METHOD: updateNappiLines -------------------------------------------*/
  METHOD PUBLIC LOGICAL updateNappiLines
    (INPUT       ipcMemNum        AS CHARACTER,
     INPUT       ipiAuthNo        AS INTEGER,
     INPUT TABLE ttauth,
     INPUT TABLE ttmemauthm):
  /*--------------------------------------------------------------------
        Purpose: Get a list of all member's auths
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'updateNappiLines':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  ipiAuthNo,
                                 INPUT TABLE ttauth,
                                 INPUT TABLE ttmemauthm"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL updateNappiLines */ 

/* -- METHOD: updateSecondaryCodes -------------------------------------------*/
  METHOD PUBLIC LOGICAL updateSecondaryCodes
    (INPUT       ipcMemNum        AS CHARACTER,
     INPUT       ipiAuthNo        AS INTEGER,
     INPUT TABLE ttauth,
     INPUT TABLE ttmemauthic):
  /*--------------------------------------------------------------------
        Purpose: Get a list of all member's auths
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'updateSecondaryCodes':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  ipiAuthNo,
                                 INPUT TABLE ttauth,
                                 INPUT TABLE ttmemauthic"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL updateSecondaryCodes */ 

/* -- METHOD: getAuthNotes -------------------------------------------*/
  METHOD PUBLIC LOGICAL getAuthNotes
    (INPUT        ipiAuthNum       AS INTEGER,
     INPUT        ipcNoteType      AS CHARACTER,
     OUTPUT TABLE tmpNote):
  /*--------------------------------------------------------------------
        Purpose: Get auth notes
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'getAuthNotes':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 INPUT  ipcNoteType,
                                 OUTPUT TABLE tmpNote"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL getAuthNotes */  

/* -- METHOD: validateNappiLines -------------------------------------------*/
  METHOD PUBLIC LOGICAL validateNappiLines
    (INPUT  ipiAuthNum    AS INTEGER,  
     INPUT  ipcNappiCode  AS CHARACTER,
     INPUT  ipdAmountAuth AS DECIMAL,  
     INPUT  ipdAmountPaid AS DECIMAL,  
     INPUT  ipcRepeatSch  AS CHARACTER,
     OUTPUT opcMessage    AS CHARACTER,
     INPUT TABLE ttmemauthm):
  /*--------------------------------------------------------------------
        Purpose: Get auth notes
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'validateNappiLines':U"
        &ServiceCallSignature = "INPUT ipiAuthNum,   
                                 INPUT ipcNappiCode, 
                                 INPUT ipdAmountAuth,
                                 INPUT ipdAmountPaid,
                                 INPUT ipcRepeatSch, 
                                 OUTPUT opcMessage,
                                 INPUT TABLE ttmemauthm"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL validateNappiLines */  

/* -- METHOD: CommsAllowed -------------------------------------------*/
  METHOD PUBLIC LOGICAL CommsAllowed
    (INPUT  ipcAuthType   AS CHARACTER,
     INPUT  ipiAuthStat   AS INTEGER,
     OUTPUT oplAllowed    AS LOGICAL):
  /*--------------------------------------------------------------------
        Purpose: Get auth notes
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'CommsAllowed':U"
        &ServiceCallSignature = "INPUT  ipcAuthType,
                                 INPUT  ipiAuthStat,
                                 OUTPUT oplAllowed"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL CommsAllowed */ 

/* -- METHOD: SetAuthNum -------------------------------------------*/
  METHOD PUBLIC LOGICAL SetAuthNum
    (OUTPUT opiAuthNum    AS INTEGER):
  /*--------------------------------------------------------------------
        Purpose: Get auth notes
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'SetAuthNum':U"
        &ServiceCallSignature = "OUTPUT opiAuthNum"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL SetAuthNum */  

/* -- METHOD: AuthorisationValues -------------------------------------------*/
  METHOD PUBLIC LOGICAL AuthorisationValues
    (INPUT        ipiAuthNum      AS INTEGER,  
     INPUT        ipiSchemeCode   AS INTEGER,  
     INPUT        iplAddFlag      AS LOGICAL,  
     INPUT        ipiPrNum        AS INTEGER,  
     INPUT        ipdtAuthStaDate AS DATE,     
     INPUT        ipcPrimDiag     AS CHARACTER,
     INPUT        ipcPrimProc     AS CHARACTER,
     INPUT        ipcAuthType     AS CHARACTER,
     INPUT        ipiAuthStat     AS INTEGER,
     OUTPUT       oplAllowPMB     AS LOGICAL,  
     OUTPUT       opiClaimCode    AS INTEGER,  
     OUTPUT       opdAmountAuth   AS DECIMAL,  
     OUTPUT       opiQuantityAuth AS INTEGER,  
     INPUT-OUTPUT ipcClaimType    AS CHARACTER,
     INPUT-OUTPUT ipcAuthAmt      AS DECIMAL):
  /*--------------------------------------------------------------------
        Purpose: Determine the auth amount/qty - diff clients have diff methods
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'AuthorisationValues':U"
        &ServiceCallSignature = "INPUT        ipiAuthNum,     
                                 INPUT        ipiSchemeCode,  
                                 INPUT        iplAddFlag,     
                                 INPUT        ipiPrNum,       
                                 INPUT        ipdtAuthStaDate,
                                 INPUT        ipcPrimDiag,    
                                 INPUT        ipcPrimProc,    
                                 INPUT        ipcAuthType,    
                                 INPUT        ipiAuthStat,    
                                 OUTPUT       oplAllowPMB,    
                                 OUTPUT       opiClaimCode,   
                                 OUTPUT       opdAmountAuth,  
                                 OUTPUT       opiQuantityAuth,
                                 INPUT-OUTPUT ipcClaimType,   
                                 INPUT-OUTPUT ipcAuthAmt"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL AuthorisationValues */  

/* -- METHOD: addAuthNote -------------------------------------------*/
  METHOD PUBLIC LOGICAL addAuthNote
    (INPUT  ipiAuthNum    AS INTEGER,
     INPUT  ipiDependant  AS INTEGER,
     INPUT  ipcUserID     AS CHARACTER,
     INPUT  ipcNoteType   AS CHARACTER,
     INPUT  ipdExpiryDate AS DATE,
     INPUT  ipcSummary    AS CHARACTER,
     INPUT  ipcNarration  AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: Add Authorisation note to an Auth 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'addAuthNote':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 INPUT  ipiDependant,
                                 INPUT  ipcUserID,
                                 INPUT  ipcNoteType,
                                 INPUT  ipdExpiryDate,
                                 INPUT  ipcSummary,
                                 INPUT  ipcNarration"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL addAuthNote */  

/* -- METHOD: openCompleteAuth -------------------------------------------*/
  METHOD PUBLIC LOGICAL openCompleteAuth
    (INPUT  ipcMemNum      AS CHARACTER,
     INPUT  ipcSchemeCode  AS INTEGER,
     INPUT  ipiAuthNum     AS INTEGER):
  /*--------------------------------------------------------------------
        Purpose: Open a completed auth
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'openCompleteAuth':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  ipcSchemeCode,
                                 INPUT  ipiAuthNum"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL openCompleteAuth */  


/* -- METHOD: ConditionType -------------------------------------------*/
  METHOD PUBLIC LOGICAL ConditionType
    (INPUT  ipcPrimDiag  AS CHARACTER,
     OUTPUT ipcCondType  AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: Get condition type for primary diagnosis
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ConditionType':U"
        &ServiceCallSignature = "INPUT  ipcPrimDiag,
                                 OUTPUT  ipcCondType"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ConditionType */  

/* -- METHOD: DefaultClaimCode -------------------------------------------*/
  METHOD PUBLIC LOGICAL DefaultClaimCode
      (INPUT  ipiClaimCode AS INTEGER):
  /*--------------------------------------------------------------------
        Purpose: Get default claim code
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'DefaultClaimCode':U"
        &ServiceCallSignature = "INPUT  ipiClaimCode"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL DefaultClaimCode */  


/* -- METHOD: DiagAllowPMB -------------------------------------------*/
  METHOD PUBLIC LOGICAL DiagAllowPMB
    (INPUT  ipiDocNum         AS INTEGER,
     INPUT  ipiSchCode        AS INTEGER,
     INPUT  ipdAuthStartDate  AS DATE,
     INPUT  ipcPrimDiag       AS CHARACTER,
     OUTPUT iplAllowPMB       AS LOGICAL):
  /*--------------------------------------------------------------------
        Purpose: Check to see if diagnosis is allowed for PMB
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'DiagAllowPMB':U"
        &ServiceCallSignature = "INPUT  ipiDocNum,
                                 INPUT  ipiSchCode,
                                 INPUT  ipdAuthStartDate,
                                 INPUT  ipcPrimDiag,
                                 OUTPUT iplAllowPMB"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL DiagAllowPMB */ 

/* -- METHOD: UpdateAuthLimits -------------------------------------------*/
  METHOD PUBLIC LOGICAL UpdateAuthLimits
    (INPUT  ipcMemberNum  AS CHARACTER,
     INPUT  ipiAuthNum    AS INTEGER):
  /*--------------------------------------------------------------------
        Purpose: Update member's auth limits
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'UpdateAuthLimits':U"
        &ServiceCallSignature = "INPUT  ipcMemberNum,
                                 INPUT  ipiAuthNum"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL UpdateAuthLimits */ 

/* -- METHOD: dependantBenefit -------------------------------------------*/
  METHOD PUBLIC LOGICAL dependantBenefit
    (INPUT  ipcMemNum       AS CHARACTER,             
     INPUT  ipdtExpStaDate  AS DATE,                  
     INPUT  ipdtEndDate     AS DATE,                  
     OUTPUT opdtBenefitDate AS DATE EXTENT 99):
  /*--------------------------------------------------------------------
        Purpose: Update member's auth limits
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'dependantBenefit':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,      
                                 INPUT  ipdtExpStaDate, 
                                 INPUT  ipdtEndDate,    
                                 OUTPUT opdtBenefitDate"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL dependantBenefit */ 

/* -- METHOD: validateQuestionnaire -------------------------------------------*/
  METHOD PUBLIC LOGICAL validateQuestionnaire
    (INPUT  ipiAuthNum       AS INTEGER,
     INPUT  ipcQuestionaire  AS CHARACTER,
     INPUT  ipcValidate      AS CHARACTER,
     INPUT  ipiSchemeCode    AS INTEGER,
     INPUT  ipdtEffectDate   AS DATE,
     OUTPUT opcMessage       AS CHARACTER,
     OUTPUT oplCreateHistory AS LOGICAL,
     OUTPUT iprQuesRowID     AS ROWID):
  /*--------------------------------------------------------------------
        Purpose: Check to see if the user's input questionaire exists
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'validateQuestionnaire':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 INPUT  ipcQuestionaire,
                                 INPUT  ipcValidate,
                                 INPUT  ipiSchemeCode,
                                 INPUT  ipdtEffectDate,
                                 OUTPUT opcMessage,
                                 OUTPUT oplCreateHistory,
                                 OUTPUT iprQuesRowID"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL validateQuestionnaire */ 

/* -- METHOD: updateQuestionaire -------------------------------------------*/
  METHOD PUBLIC LOGICAL UpdateQuestionaire
    (INPUT        ipcMemNum        AS CHARACTER,
     INPUT        ipiAuthNum       AS INTEGER,
     INPUT        ipiDependant     AS INTEGER,
     INPUT        ipcQuesName      AS CHARACTER,
     INPUT        ipcAuthType      AS CHARACTER,
     OUTPUT       iplUpdated       AS LOGICAL,
     INPUT TABLE  ttMemQues):
  /*--------------------------------------------------------------------
        Purpose: Update an auth's questionaire
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'UpdateQuestionaire':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  ipiAuthNum,
                                 INPUT  ipiDependant,
                                 INPUT  ipcQuesName,
                                 INPUT  ipcAuthType,
                                 OUTPUT iplUpdated,
                                 INPUT TABLE ttMemQues"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL updateQuestionaire */  

/* -- METHOD: deleteQuestionaire -------------------------------------------*/
  METHOD PUBLIC LOGICAL deleteQuestionaire
    (INPUT        ipcMemNum        AS CHARACTER,
     INPUT        ipiAuthNum       AS INTEGER,
     INPUT        ipcQuesName      AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: Delete an auth's questionaire
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'deleteQuestionaire':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,
                                 INPUT  ipiAuthNum,
                                 INPUT  ipcQuesName"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL deleteQuestionaire */  

/* -- METHOD: createLetter -------------------------------------------*/
  METHOD PUBLIC LOGICAL createLetter
    (INPUT ipiAuthNum    AS INTEGER,  
     INPUT ipcMemNum     AS CHARACTER,
     INPUT ipiDocNum     AS INTEGER,  
     INPUT ipcTemplate   AS CHARACTER,
     INPUT ipcMethod     AS CHARACTER,
     INPUT ipiSchemeCode AS INTEGER,  
     INPUT ipcSendTo     AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: Delete an auth's questionaire
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'createLetter':U"
        &ServiceCallSignature = "INPUT ipiAuthNum,   
                                 INPUT ipcMemNum,    
                                 INPUT ipiDocNum,    
                                 INPUT ipcTemplate,  
                                 INPUT ipcMethod,    
                                 INPUT ipiSchemeCode,
                                 INPUT ipcSendTo"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL createLetter */  

/* -- METHOD: AvailableSavings -------------------------------------------*/
  METHOD PUBLIC LOGICAL AvailableSavings
    (INPUT        cAuthType        AS CHARACTER,
     INPUT        cSavType         AS CHARACTER,
     OUTPUT       cError           AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: Check if Savings are available
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'AvailableSavings':U"
        &ServiceCallSignature = "INPUT  cAuthType,
                                 INPUT  cSavType,
                                 OUTPUT cError"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL AvailableSavings */  

/* -- METHOD: BenefitValidation -------------------------------------------*/
  METHOD PUBLIC LOGICAL ValidateBenefit
    (INPUT        ipiAuthNum       AS INTEGER,  
     INPUT        ipcMemNum        AS CHARACTER,
     INPUT        ipiDependant     AS INTEGER,  
     INPUT        ipiClmCode       AS INTEGER,  
     INPUT        ipcClmDesc       AS CHARACTER,
     INPUT        ipcDepName       AS CHARACTER,
     INPUT        ipdtExpEndDate   AS DATE):
  /*--------------------------------------------------------------------
        Purpose: Validate benefits
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ValidateBenefit':U"
        &ServiceCallSignature = "INPUT ipiAuthNum,   
                                 INPUT ipcMemNum,    
                                 INPUT ipiDependant,
                                 INPUT ipiClmCode,   
                                 INPUT ipcClmDesc,   
                                 INPUT ipcDepName,   
                                 INPUT ipdtExpEndDate"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL BenefitValidation */  

/* -- METHOD: ClaimCodeForYearAdd -------------------------------------------*/
  METHOD PUBLIC LOGICAL ClaimCodeForYearAdd
    (INPUT         ipiSchemeCode     AS INTEGER,
     INPUT         ipiClmCode        AS INTEGER,  
     INPUT         ipdtExpStartDate  AS DATE,     
     OUTPUT        ipdeMaxAmount     AS DECIMAL,  
     OUTPUT        ipcMessage        AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ClaimCodeForYearAdd':U"
        &ServiceCallSignature = "INPUT  ipiSchemeCode,
                                 INPUT  ipiClmCode,
                                 INPUT  ipdtExpStartDate,
                                 OUTPUT ipdeMaxAmount,
                                 OUTPUT ipcMessage"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ClaimCodeForYearAdd */  

/* -- METHOD: ClaimCodeForYearUpdate -------------------------------------------*/
  METHOD PUBLIC LOGICAL ClaimCodeForYearUpdate
    (INPUT         ipiSchemeCode     AS INTEGER,
     INPUT         ipiClmCode        AS INTEGER,  
     INPUT         ipdtExpStartDate  AS DATE,     
     OUTPUT        ipdeMaxAmount     AS DECIMAL,  
     OUTPUT        ipcMessage        AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ClaimCodeForYearUpdate':U"
        &ServiceCallSignature = "INPUT  ipiSchemeCode,
                                 INPUT  ipiClmCode,
                                 INPUT  ipdtExpStartDate,
                                 OUTPUT ipdeMaxAmount,
                                 OUTPUT ipcMessage"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ClaimCodeForYearUpdate */  

/* -- METHOD: DependantValidation -------------------------------------------*/
  METHOD PUBLIC LOGICAL ValidateDependant
    (INPUT         ipdtBenefitDate  AS DATE,     
     INPUT         ipcMemNum        AS CHARACTER,
     INPUT         ipdtBenDate      AS DATE,     
     INPUT         ipdtJoinDate     AS DATE,     
     INPUT         ipdtAutStDate    AS DATE,     
     INPUT         ipdtExpStDate    AS DATE,     
     INPUT         ipdtResDate      AS DATE,     
     INPUT         ipdtSusDate      AS DATE,    
     INPUT         ipiDependant     AS INTEGER,  
     INPUT         ipcPrType        AS CHARACTER,
     INPUT         ipiDocNum        AS INTEGER,  
     INPUT         ipiSchemeCode    AS INTEGER,  
     INPUT         ipiPcpClmCode    AS INTEGER,  
     INPUT         ipcMode          AS CHARACTER,
     OUTPUT        ipiClmCode1      AS INTEGER,  
     OUTPUT        ipiClmCode2      AS INTEGER,  
     OUTPUT        ipiNegNum        AS INTEGER,  
     OUTPUT        ipcMessage       AS CHARACTER,
     OUTPUT        ipcAction        AS CHARACTER,
     OUTPUT        ipdtFlagStart    AS DATE,     
     OUTPUT        iplValidPcp      AS LOGICAL):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ValidateDependant':U"
        &ServiceCallSignature = "INPUT  ipdtBenefitDate,
                                 INPUT  ipcMemNum,      
                                 INPUT  ipdtBenDate,    
                                 INPUT  ipdtJoinDate,   
                                 INPUT  ipdtAutStDate,  
                                 INPUT  ipdtExpStDate,  
                                 INPUT  ipdtResDate,    
                                 INPUT  ipdtSusDate,    
                                 INPUT  ipiDependant,   
                                 INPUT  ipcPrType,      
                                 INPUT  ipiDocNum,      
                                 INPUT  ipiSchemeCode,  
                                 INPUT  ipiPcpClmCode,  
                                 INPUT  ipcMode,        
                                 OUTPUT ipiClmCode1,   
                                 OUTPUT ipiClmCode2,    
                                 OUTPUT ipiNegNum,      
                                 OUTPUT ipcMessage,     
                                 OUTPUT ipcAction,      
                                 OUTPUT ipdtFlagStart,  
                                 OUTPUT iplValidPcp"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL DependantValidation */  

/* -- METHOD: NonDSPAndClmCode -------------------------------------------*/
  METHOD PUBLIC LOGICAL NonDSPAndClmCode
    (INPUT        ipiClmCode        AS INTEGER,
     INPUT        ipiClmCode34      AS INTEGER,
     INPUT        ipiDocNum         AS INTEGER,
     INPUT        ipcPayMethod      AS CHARACTER
     ):
  /*--------------------------------------------------------------------
        Purpose: Delete an auth's questionaire
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'NonDSPAndClmCode':U"
        &ServiceCallSignature = "INPUT  ipiClmCode,  
                                 INPUT  ipiClmCode34,
                                 INPUT  ipiDocNum,   
                                 INPUT  ipcPayMethod"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL NonDSPAndClmCode */  

/* -- METHOD: ValidMCAClaimCode -------------------------------------------*/
  METHOD PUBLIC LOGICAL ValidMCAClaimCode
    (INPUT       ipcPrType        AS CHARACTER,
     INPUT       ipiClmCode34     AS INTEGER,  
     INPUT       ipiSchemeCode    AS INTEGER,  
     INPUT       ipiClmCode       AS INTEGER,  
     INPUT       ipiPcpClmCode    AS INTEGER,  
     INPUT       iplValidPcp      AS LOGICAL,  
     INPUT       ipiNegNum        AS INTEGER
     ):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ValidMCAClaimCode':U"
        &ServiceCallSignature = "INPUT   ipcPrType,    
                                 INPUT   ipiClmCode34, 
                                 INPUT   ipiSchemeCode,
                                 INPUT   ipiClmCode,   
                                 INPUT   ipiPcpClmCode,
                                 INPUT   iplValidPcp,  
                                 INPUT   ipiNegNum"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ValidMCAClaimCode */  

/* -- METHOD: ValidPcp -------------------------------------------*/
  METHOD PUBLIC LOGICAL ValidPCP
    (INPUT        ipcMemNum          AS CHARACTER,
     INPUT        dtExpStaDate       AS DATE,
     INPUT        ipiDocNum          AS INTEGER,  
     INPUT        ipiSchemeCode      AS INTEGER,  
     OUTPUT       opiPcpClmCode      AS INTEGER,
     OUTPUT       opcError           AS CHARACTER
     ):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ValidPCP':U"
        &ServiceCallSignature = "INPUT  ipcMemNum,    
                                 INPUT  dtExpStaDate,    
                                 INPUT  ipiDocNum,    
                                 INPUT  ipiSchemeCode,
                                 OUTPUT opiPcpClmCode,
                                 OUTPUT opcError"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ValidPcp */  
  
/* -- METHOD: getDetailLines -------------------------------------------*/
  METHOD PUBLIC LOGICAL getDetailLines
    (INPUT        ipiAuthNum         AS INTEGER,
     OUTPUT TABLE ttauthd 
     ):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'getDetailLines':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 OUTPUT TABLE ttauthd"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL getDetailLines */  
  
/* -- METHOD: getNappiLines -------------------------------------------*/
  METHOD PUBLIC LOGICAL getNappiLines
    (INPUT        ipiAuthNum         AS INTEGER,
     OUTPUT TABLE ttmemauthm 
     ):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'getNappiLines':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 OUTPUT TABLE ttmemauthm"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL getNappiLines */  
  
/* -- METHOD: getDiagProcCodes -------------------------------------------*/
  METHOD PUBLIC LOGICAL getDiagProcCodes
    (INPUT        ipiAuthNum         AS INTEGER,
     INPUT        ipcCodeInd         AS CHARACTER,
     OUTPUT TABLE ttmemauthic 
     ):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'getDiagProcCodes':U"
        &ServiceCallSignature = "INPUT  ipiAuthNum,
                                 INPUT  ipcCodeInd,
                                 OUTPUT TABLE ttmemauthic"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL getDiagProcCodes */  
    
/* -- METHOD: UpdateMemlauth -------------------------------------------*/
  METHOD PUBLIC LOGICAL UpdateMemlauth
    (INPUT iSchemeCode        AS INTEGER,  
     INPUT cMemNum            AS CHARACTER,
     INPUT iDependant         AS INTEGER,  
     INPUT cClaimType         AS CHARACTER,
     INPUT iClaimCode         AS INTEGER,  
     INPUT cTariffCode        AS CHARACTER,
     INPUT deAmount           AS DECIMAL,  
     INPUT deAuthPMBValue     AS DECIMAL,  
     INPUT dtAuthDate         AS DATE,     
     INPUT dtExpStaDate       AS DATE,     
     INPUT dtExpEndDate       AS DATE,     
     INPUT cUserID            AS CHARACTER,
     INPUT lDetails           AS LOGICAL,  
     INPUT-OUTPUT iAuthQty    AS INTEGER):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'UpdateMemlauth':U"
        &ServiceCallSignature = "INPUT iSchemeCode,     
                                 INPUT cMemNum,         
                                 INPUT iDependant,      
                                 INPUT cClaimType,      
                                 INPUT iClaimCode,      
                                 INPUT cTariffCode,     
                                 INPUT deAmount,        
                                 INPUT deAuthPMBValue,  
                                 INPUT dtAuthDate,      
                                 INPUT dtExpStaDate,    
                                 INPUT dtExpEndDate,    
                                 INPUT cUserID,         
                                 INPUT lDetails,        
                                 INPUT-OUTPUT iAuthQty"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL UpdateMemlauth */  

/* -- METHOD: ReverseAuthLimits -------------------------------------------*/
  METHOD PUBLIC LOGICAL ReverseAuthLimits
    (INPUT iSchemeCode        AS INTEGER,  
     INPUT cMemNum            AS CHARACTER,
     INPUT iDependant         AS INTEGER,  
     INPUT cClaimType         AS CHARACTER,
     INPUT iClaimCode         AS INTEGER,  
     INPUT cTariffCode        AS CHARACTER,
     INPUT deOldAmt           AS DECIMAL, 
     INPUT deNewAmt           AS DECIMAL, 
     INPUT deOldPMBValue      AS DECIMAL, 
     INPUT deNewPMBValue      AS DECIMAL, 
     INPUT dtAuthDate         AS DATE,     
     INPUT dtExpStaDate       AS DATE,     
     INPUT dtExpEndDate       AS DATE, 
     INPUT iOldAuthStat       AS INTEGER,    
     INPUT iM-o               AS INTEGER,
     INPUT-OUTPUT iOldQty     AS INTEGER,  
     INPUT-OUTPUT iNewQty     AS INTEGER):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ReverseAuthLimits':U"
        &ServiceCallSignature = "INPUT iSchemeCode,     
                                 INPUT cMemNum,         
                                 INPUT iDependant,      
                                 INPUT cClaimType,      
                                 INPUT iClaimCode,      
                                 INPUT cTariffCode,     
                                 INPUT deOldAmt,        
                                 INPUT deNewAmt,        
                                 INPUT deOldPMBValue,        
                                 INPUT deNewPMBValue,  
                                 INPUT dtAuthDate,      
                                 INPUT dtExpStaDate,    
                                 INPUT dtExpEndDate,    
                                 INPUT iOldAuthStat,         
                                 INPUT iM-o,        
                                 INPUT-OUTPUT iOldQty,
                                 INPUT-OUTPUT iNewQty"
                                 }

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ReverseAuthLimits */  

/* -- METHOD: CheckAuthLimits -------------------------------------------*/
  METHOD PUBLIC LOGICAL CheckAuthLimits
    (INPUT iSchemeCode        AS INTEGER,  
     INPUT cMemNum            AS CHARACTER,
     INPUT iDocNum            AS INTEGER,
     INPUT iDependant         AS INTEGER,  
     INPUT cClaimType         AS CHARACTER,
     INPUT iClaimCode         AS INTEGER,  
     INPUT cTariffCode        AS CHARACTER,
     INPUT deAmount           AS DECIMAL,  
     INPUT deAuthPMBValue     AS DECIMAL,  
     INPUT dtAuthDate         AS DATE,     
     INPUT dtExpStaDate       AS DATE,     
     INPUT dtExpEndDate       AS DATE,     
     INPUT lDetails           AS LOGICAL,  
     INPUT lAllowPMB          AS LOGICAL,  
     INPUT-OUTPUT iAuthQty    AS INTEGER,
     INPUT-OUTPUT deAuthAmt   AS DECIMAL,
     OUTPUT cWarning          AS CHARACTER,
     OUTPUT cResult           AS CHARACTER):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'CheckAuthLimits':U"
        &ServiceCallSignature = "INPUT iSchemeCode,     
                                 INPUT cMemNum,   
                                 INPUT iDocNum,      
                                 INPUT iDependant,      
                                 INPUT cClaimType,      
                                 INPUT iClaimCode,      
                                 INPUT cTariffCode,     
                                 INPUT deAmount,        
                                 INPUT deAuthPMBValue,  
                                 INPUT dtAuthDate,      
                                 INPUT dtExpStaDate,    
                                 INPUT dtExpEndDate,    
                                 INPUT lDetails,        
                                 INPUT lAllowPMB,        
                                 INPUT-OUTPUT iAuthQty,
                                 INPUT-OUTPUT deAuthAmt,
                                 OUTPUT cWarning,
                                 OUTPUT cResult"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL CheckAuthLimits */  
  
/* -- METHOD: CreateAuthorisation -------------------------------------------*/
  METHOD PUBLIC LOGICAL CreateAuthorisation
     (INPUT pcMemNum          AS CHARACTER,
     INPUT piSchemeCode       AS INTEGER,
     INPUT piAuthNo           AS INTEGER,
     INPUT pdAmountAuth       AS DECIMAL,
     INPUT piDependant        AS INTEGER,
     INPUT plAddFlag          AS LOGICAL,
     INPUT TABLE ttmemques,
     INPUT TABLE ttauth,
     INPUT TABLE ttextref,
     INPUT TABLE ttmemauthic,
     INPUT TABLE ttmemauthc,
     OUTPUT pdAmountLimit     AS DECIMAL):
  /*--------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'CreateAuthorisation':U"
        &ServiceCallSignature = "INPUT pcMemNum, 
                                 INPUT piSchemeCode, 
                                 INPUT piAuthNo, 
                                 INPUT pdAmountAuth, 
                                 INPUT piDependant, 
                                 INPUT plAddFlag, 
                                 INPUT TABLE ttmemques,
                                 INPUT TABLE ttauth,
                                 INPUT TABLE ttextref,
                                 INPUT TABLE ttmemauthic,
                                 INPUT TABLE ttmemauthc,
                                 OUTPUT pdAmountLimit"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL CreateAuthorisation */  

/* -- METHOD: ValidateDetailLines -------------------------------------------*/
  METHOD PUBLIC LOGICAL ValidateDetailLines
     (INPUT plAddFlag       AS LOGICAL,  
      INPUT piSchemeCode    AS INTEGER,  
      INPUT TABLE ttAuth,    
      INPUT TABLE ttAuthd):
  /*--OUTPUT cType       ------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ValidateDetailLines':U"
        &ServiceCallSignature = "INPUT plAddFlag,
                                 INPUT piSchemeCode,
                                 INPUT TABLE ttAuth,    
                                 INPUT TABLE ttAuthd"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ValidateDetailLines */  

  /* -- METHOD: ValidateAuthorisation -------------------------------------------*/
  METHOD PUBLIC LOGICAL ValidateAuthorisation
     (INPUT ipiSchemeCode    AS INTEGER,
      INPUT iplAddFlag       AS LOGICAL,
      INPUT TABLE ttauth,
      OUTPUT opcField        AS CHARACTER,
      OUTPUT opcError        AS CHARACTER,
      OUTPUT opcType         AS CHARACTER):
  /*--OUTPUT cType       ------------------------------------------------------------------
        Purpose: 
          Notes: 
  --------------------------------------------------------------------*/ 

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = "'ValidateAuthorisation':U"
        &ServiceCallSignature = "INPUT ipiSchemeCode,
                                 INPUT iplAddFlag,
                                 INPUT TABLE ttauth,
                                 OUTPUT opcField,
                                 OUTPUT opcError,
                                 OUTPUT opcType"
    } 

    RETURN TRUE.
  END METHOD. /* METHOD PUBLIC LOGICAL ValidateAuthorisation */  

END CLASS. /* ma.cls.basemiphealth INHERITS cls.mipapplication */
/* That's all folks! */

