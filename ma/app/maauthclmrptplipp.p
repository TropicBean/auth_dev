&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    $Id: procedur.p,v 1.2 2008/09/30 12:26:17 chrisk Exp $ 

    Purpose:
  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
  {mip/inc/mipdefshared.i}
  {gen/inc/rxrepparameter.i}
  
  DEFINE VARIABLE ghDataBuffer  AS HANDLE        NO-UNDO.
  DEFINE VARIABLE ghMemauth     AS HANDLE        NO-UNDO.
  
  DEFINE VARIABLE ghQuery        AS HANDLE        NO-UNDO.
  DEFINE VARIABLE cQuery        AS CHARACTER     NO-UNDO.
  
  DEFINE VARIABLE cAuthType     AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE dDateFrom     AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE dDateTo       AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cAuthStatus   AS CHARACTER     NO-UNDO.

  DEFINE VARIABLE cErrorMessage      AS CHARACTER     NO-UNDO.
  
  DEFINE TEMP-TABLE ttReportHead NO-UNDO RCODE-INFORMATION
    FIELD HeaderDescription AS CHARACTER  LABEL "Extract Description MZ" FORMAT "x(80)":U.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-maGetAuthClmData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maGetAuthClmData Procedure 
PROCEDURE maGetAuthClmData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE phParmTable.
  DEFINE OUTPUT       PARAMETER TABLE-HANDLE phHeaderTable.
  DEFINE OUTPUT       PARAMETER TABLE-HANDLE phBodyTable.

  DEFINE VARIABLE iCnt               AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE dTotAssocClaims    AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE dTotProvClaims     AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE iAssocClms         AS INTEGER       NO-UNDO.
  DEFINE VARIABLE iProvClms          AS INTEGER       NO-UNDO.
  DEFINE VARIABLE iCount             AS INTEGER       NO-UNDO. 
  DEFINE VARIABLE iTotalRecords      AS INTEGER       NO-UNDO.
  DEFINE VARIABLE lShowDetail        AS LOGICAL       NO-UNDO.
  DEFINE VARIABLE lShowAuthDetail    AS LOGICAL       NO-UNDO.
  DEFINE VARIABLE cDepCond           AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cProcCode          AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cDiagCode          AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cMemNum            AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE iAuthNum           AS INTEGER       NO-UNDO.
  DEFINE VARIABLE iDepNum            AS INTEGER       NO-UNDO.
  DEFINE VARIABLE dStartDate         AS DATE          NO-UNDO.
  DEFINE VARIABLE dEndDate           AS DATE          NO-UNDO.
  DEFINE VARIABLE cClaimCodeList     AS CHARACTER     NO-UNDO.


  ASSIGN iTotalRecords  = 0 NO-ERROR.
  { mip/inc/mipreturnerror.i }

  ASSIGN phHeaderTable = TEMP-TABLE ttReportHead:HANDLE NO-ERROR.
  { mip/inc/mipreturnerror.i }

  ASSIGN phHeaderTable = TEMP-TABLE ttReportHead:HANDLE NO-ERROR.
    { mip/inc/mipreturnerror.i }

  ASSIGN 
    cAuthStatus = getParameter(phParmTable, "fcAuthStatus":U,   "":U)
    cAuthType   = getParameter(phParmTable, "fcAuthTypes":U,    "":U)
    dDateFrom   = getParameter(phParmTable, "fcAuthDateFrom":U, "":U)
    dDateTo     = getParameter(phParmTable, "fcAuthDateTo":U,   "":U)
    NO-ERROR.
    { mip/inc/mipreturnerror.i }

  RUN setResourceTable IN TARGET-PROCEDURE (OUTPUT phBodyTable)
  NO-ERROR.
  { mip/inc/mipreturnerror.i }

  IF NOT {&ErrorStatus} AND VALID-HANDLE(phBodyTable) THEN
  DO:
    ASSIGN
      ghDataBuffer   = phBodyTable:DEFAULT-BUFFER-HANDLE
      ghMemauth      = BUFFER memauth:HANDLE
    NO-ERROR.
    { mip/inc/mipreturnerror.i }
  END.

  IF cAuthType = "ALL":U THEN
  DO:
    FIND FIRST datalist NO-LOCK
         WHERE datalist.list-type = "MCACUST":U      
           AND datalist.list-code = "HOSPAUTH":U     
           AND datalist.link-from = "" NO-ERROR.
    IF AVAILABLE datalist THEN 
      ASSIGN cAuthType = datalist.DESCRIPTION NO-ERROR.
  END.

  CREATE QUERY ghQuery NO-ERROR.
  { mip/inc/mipreturnerror.i }

  DO iCount = 1 TO NUM-ENTRIES(cAuthStatus):

    ASSIGN cQuery = "FOR EACH memauth NO-LOCK" 
                  + " WHERE memauth.authstat   = " + ENTRY(iCount,cAuthStatus) 
                  + "   AND memauth.auth-date >= " + dDateFrom 
                  + "   AND memauth.auth-date <= " + dDateTo
                  + "   AND LOOKUP(memauth.auth-type," + QUOTER(cAuthType) + ")  > 0 "
    NO-ERROR.  
    {mip/inc/mipreturnerror.i}
    
    ghQuery:SET-BUFFERS(ghMemauth) NO-ERROR.
    { mip/inc/mipreturnerror.i }
    
    IF NOT {&ErrorStatus} THEN ghQuery:QUERY-PREPARE(cQuery) NO-ERROR.
    { mip/inc/mipreturnerror.i }
    IF NOT {&ErrorStatus} THEN ghQuery:QUERY-OPEN()          NO-ERROR.
    { mip/inc/mipreturnerror.i }
    IF NOT {&ErrorStatus} THEN ghQuery:GET-FIRST()           NO-ERROR.
    { mip/inc/mipreturnerror.i }
    
    DO WHILE NOT ghQuery:QUERY-OFF-END:

      ASSIGN cMemNum    = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE
             iDepNum    = ghMemauth:BUFFER-FIELD("dependant":U):BUFFER-VALUE
             iAuthNum   = ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE
             dStartDate = ghMemauth:BUFFER-FIELD("expected-start-date":U):BUFFER-VALUE
             dEndDate   = ghMemauth:BUFFER-FIELD("expected-end-date":U):BUFFER-VALUE.
      ghDataBuffer:BUFFER-CREATE NO-ERROR.
        {mip/inc/mipreturnerror.i}
    
      ASSIGN 
        ghDataBuffer:BUFFER-FIELD("sort_order":U):BUFFER-VALUE          = ghMemauth:BUFFER-FIELD("auth-date":U):BUFFER-VALUE
        ghDataBuffer:BUFFER-FIELD("auth_num":U):BUFFER-VALUE            = STRING(ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE)
        ghDataBuffer:BUFFER-FIELD("reference_auth_num":U):BUFFER-VALUE  = ghMemauth:BUFFER-FIELD("reference-auth-num":U):BUFFER-VALUE
        ghDataBuffer:BUFFER-FIELD("mem_num":U):BUFFER-VALUE             = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE            
        ghDataBuffer:BUFFER-FIELD("dependant":U):BUFFER-VALUE           = ghMemauth:BUFFER-FIELD("dependant":U):BUFFER-VALUE
        ghDataBuffer:BUFFER-FIELD("doc_num":U):BUFFER-VALUE             = STRING(ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE)
        ghDataBuffer:BUFFER-FIELD("req-pr-num":U):BUFFER-VALUE          = STRING(ghMemauth:BUFFER-FIELD("req-pr-num":U):BUFFER-VALUE)
        ghDataBuffer:BUFFER-FIELD("auth_type":U):BUFFER-VALUE           = ghMemauth:BUFFER-FIELD("auth-type":U):BUFFER-VALUE
        ghDataBuffer:BUFFER-FIELD("auth_date":U):BUFFER-VALUE           = ghMemauth:BUFFER-FIELD("auth-date":U):BUFFER-VALUE          
        ghDataBuffer:BUFFER-FIELD("expected_start_date":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("expected-start-date":U):BUFFER-VALUE
        ghDataBuffer:BUFFER-FIELD("expected_end_date":U):BUFFER-VALUE   = ghMemauth:BUFFER-FIELD("expected-end-date":U):BUFFER-VALUE  
        ghDataBuffer:BUFFER-FIELD("amount_auth":U):BUFFER-VALUE         = ghMemauth:BUFFER-FIELD("amount-auth":U):BUFFER-VALUE        
        ghDataBuffer:BUFFER-FIELD("amount_paid":U):BUFFER-VALUE         = ghMemauth:BUFFER-FIELD("amount-paid":U):BUFFER-VALUE       
        ghDataBuffer:BUFFER-FIELD("auth_end_date":U):BUFFER-VALUE       = ghMemauth:BUFFER-FIELD("auth-end-date":U):BUFFER-VALUE       
        NO-ERROR.
      {mip/inc/mipreturnerror.i}
    
      FIND FIRST memdep NO-LOCK
        WHERE memdep.mem-num   = cMemNum
          AND memdep.dependant = iDepNum
        NO-ERROR.
      IF AVAILABLE memdep THEN
        ASSIGN ghDataBuffer:BUFFER-FIELD("dep_age":U):BUFFER-VALUE    = INTERVAL(TODAY,memdep.birth-date,"years")
               ghDataBuffer:BUFFER-FIELD("dep_gender":U):BUFFER-VALUE = IF memdep.sex = "M" THEN "Male" ELSE (IF memdep.sex = "F" THEN "Female" ELSE "Unknown")  
        NO-ERROR.
        {mip/inc/mipreturnerror.i}

      FIND FIRST datalist NO-LOCK
           WHERE datalist.list-type = "MCATYPES":U
             AND datalist.list-code = ghMemauth:BUFFER-FIELD("auth-type":U):BUFFER-VALUE NO-ERROR.
      IF AVAILABLE datalist THEN 
        ASSIGN ghDataBuffer:BUFFER-FIELD("auth_type":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("auth-type":U):BUFFER-VALUE + " - " + datalist.DESCRIPTION NO-ERROR.
      ELSE
        ASSIGN ghDataBuffer:BUFFER-FIELD("auth_type":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("auth-type":U):BUFFER-VALUE NO-ERROR.
    
      CASE ENTRY(iCount,cAuthStatus) :
        WHEN "0" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "0 - Pending".
        WHEN "1" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "1 - Authorised".
        WHEN "2" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "2 - Assessed".
        WHEN "3" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "3 - Pulled".
        WHEN "4" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "4 - Complete".
        WHEN "5" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "5 - Cancelled".
        WHEN "6" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "6 - Declined".
      END CASE.
      {mip/inc/mipreturnerror.i}
                  
      ASSIGN cDepCond = "".
      FOR EACH depcond NO-LOCK
        WHERE depcond.mem-num   = cMemNum
          AND depcond.dependant = iDepNum:
        IF (depcond.start-date <= dStartDate
        OR  depcond.end-date >= dEndDate)
        AND LOOKUP(depcond.cond-code,cDepCond,"|") = 0 THEN
          ASSIGN cDepCond = cDepCond + (IF cDepCond = "" THEN "" ELSE "|") + depcond.cond-code.
      END. /* FOR EACH depcond */            
                                                                                        
      ASSIGN cProcCode = ghMemauth:BUFFER-FIELD("prim-proc":U):BUFFER-VALUE.                                             
      FOR EACH memauthic NO-LOCK                                                        
        WHERE memauthic.auth-num = iAuthNum
          AND memauthic.code-ind = "P":
        IF LOOKUP(memauthic.code,cProcCode,"|") = 0 THEN
          ASSIGN cProcCode = cProcCode + (IF cProcCode = "" THEN "" ELSE "|") + memauthic.code.
      END. /* FOR EACH memauthic */
      
      ASSIGN cDiagCode = ghMemauth:BUFFER-FIELD("prim-diag":U):BUFFER-VALUE.
      FOR EACH memauthic NO-LOCK
        WHERE memauthic.auth-num = iAuthNum
          AND memauthic.code-ind = "D":
        IF LOOKUP(memauthic.code,cDiagCode,"|") = 0 THEN
          ASSIGN cDiagCode = cDiagCode + (IF cDiagCode = "" THEN "" ELSE "|") + memauthic.code.
      END. /* FOR EACH memauthic */

      ASSIGN iAssocClms      = 0
             iProvClms       = 0
             dTotProvClaims  = 0
             dTotAssocClaims = 0
             cClaimCodeList  = "":U.

      ASSIGN cClaimCodeList = ghMemauth:BUFFER-FIELD("claim-code":U):BUFFER-VALUE.

      FOR EACH scheme NO-LOCK,
        EACH claim NO-LOCK
          WHERE claim.scheme-code = scheme.scheme-code
            AND claim.mem-num     = cMemNum
            AND claim.auth-num    = iAuthNum:

        IF LOOKUP(STRING(claim.claim-code),cClaimCodeList,"|") = 0 THEN
          ASSIGN cClaimCodeList = cClaimCodeList + (IF cClaimCodeList = "" THEN "" ELSE "|") + STRING(claim.claim-code).

        IF claim.doc-num = ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE THEN
          ASSIGN iProvClms      = iProvClms + 1
                 dTotProvClaims = dTotProvClaims + claim.benefit. 
        ELSE                                                               
          ASSIGN iAssocClms      = iAssocClms + 1
                 dTotAssocClaims = dTotAssocClaims + claim.benefit.
      END. /* FOR EACH SCHEME, CLAIM */
    
      ASSIGN 
        ghDataBuffer:BUFFER-FIELD("depcond":U):BUFFER-VALUE            = cDepCond
        ghDataBuffer:BUFFER-FIELD("proc_code":U):BUFFER-VALUE          = cProcCode
        ghDataBuffer:BUFFER-FIELD("diag_code":U):BUFFER-VALUE          = cDiagCode
        ghDataBuffer:BUFFER-FIELD("prov_claims":U):BUFFER-VALUE        = iProvClms      
        ghDataBuffer:BUFFER-FIELD("total_prov-claims":U):BUFFER-VALUE  = dTotProvClaims      
        ghDataBuffer:BUFFER-FIELD("assoc_claims":U):BUFFER-VALUE       = iAssocClms      
        ghDataBuffer:BUFFER-FIELD("total_assoc_claims":U):BUFFER-VALUE = dTotAssocClaims
        ghDataBuffer:BUFFER-FIELD("claim_code":U):BUFFER-VALUE         = cClaimCodeList
      NO-ERROR.
      {mip/inc/mipreturnerror.i}
    
      ghQuery:GET-NEXT() NO-ERROR.
      { mip/inc/mipreturnerror.i } 
    END. /* DO WHILE NOT ghQuery: */
  END. /* DO iCount = */

  /* Total line for top of report */
  CREATE ttReportHead NO-ERROR.
  IF NOT {&ErrorStatus} THEN
    ASSIGN ttReportHead.HeaderDescription = "Total Records: " + STRING(iTotalRecords)
           NO-ERROR.
  {mip/inc/mipreturnerror.i}

  /* Final totals */
  ghDataBuffer:BUFFER-CREATE NO-ERROR.
  IF NOT {&ErrorStatus} THEN
    ASSIGN
          ghDataBuffer:BUFFER-FIELD("_Style_"):BUFFER-VALUE     = "FONT:BOLD":U
          NO-ERROR.
    {mip/inc/mipreturnerror.i}

  FINALLY:
    ASSIGN cErrorMessage = {mip/inc/mipreturnvalue.i}
           NO-ERROR.

    IF VALID-HANDLE(ghQuery)      THEN DELETE OBJECT ghQuery       NO-ERROR. {mip/inc/mipmessageerror.i &ResetError = TRUE}   
    IF VALID-HANDLE(ghMemauth)    THEN DELETE OBJECT ghMemauth     NO-ERROR. {mip/inc/mipmessageerror.i &ResetError = TRUE}
                                                                                                                        
    IF cErrorMessage <> "":U THEN
      RETURN ERROR cErrorMessage.

  END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setResourceTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setResourceTable Procedure 
PROCEDURE setResourceTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETER phBodyTable AS HANDLE     NO-UNDO.

  DEFINE VARIABLE iNumField           AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL    NO-UNDO.
  
  CREATE TEMP-TABLE phBodyTable NO-ERROR.
  { mip/inc/mipreturnerror.i }
                                        
  ASSIGN 
    lSuccess = phBodyTable:ADD-NEW-FIELD("sort_order":U,         "DATE":U,         0, "99/99/9999":U,     "":U, "?":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("auth_num":U,           "CHARACTER":U,    0, "":U,               "":U, "Auth Num":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("reference_auth_num":U, "CHARACTER":U,    0, "x(15)":U,          "":U, "Reference Auth Num":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("mem_num":U,            "CHARACTER":U,    0, "x(15)":U,          "":U, "Member":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("dependant":U,          "INTEGER":U,      0, "99":U,             "":U, "Dependant":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("dep_age":U,            "INTEGER":U,      0, "999":U,            "":U, "Dependant Age":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("dep_gender":U,         "CHARACTER":U,    0, "x(7)":U,           "":U, "Dependant Gender":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("depcond":U,            "CHARACTER":U,    0, "x(30)":U,          "":U, "Dependant Conditions":U) 
    lSuccess = phBodyTable:ADD-NEW-FIELD("doc_num":U,            "CHARACTER":U,    0, "x(30)":U,          "":U, "Service Provider":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("req-pr-num":U,         "CHARACTER":U,    0, "x(30)":U,          "":U, "Requesting Provider":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("diag_code":U,          "CHARACTER":U,    0, "x(30)":U,          "":U, "Primary Diagnosis":U) 
    lSuccess = phBodyTable:ADD-NEW-FIELD("proc_code":U,          "CHARACTER":U,    0, "x(30)":U,          "":U, "Procedure Codes":U) 
    lSuccess = phBodyTable:ADD-NEW-FIELD("auth_type":U,          "CHARACTER":U,    0, "x(15)":U,          "":U, "Auth Type":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("auth_stat":U,          "CHARACTER":U,    0, "x(15)":U,          "":U, "Status":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("amount_auth":U,        "DECIMAL":U,      0, "-ZZZ,ZZZ,ZZ9.99",  "":U, "Auth Amount":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("amount_paid":U,        "DECIMAL":U,      0, "-ZZZ,ZZZ,ZZ9.99",  "":U, "Amount Paid":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("auth_date":U,          "DATE":U,         0, "99/99/9999":U,     "":U, "Auth Date":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("expected_start_date":U,"DATE":U,         0, "99/99/9999":U,     "":U, "Expected Start Date":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("expected_end_date":U,  "DATE":U,         0, "99/99/9999":U,     "":U, "Expected End Date":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("auth_end_date":U,      "DATE":U,         0, "99/99/9999":U,     "":U, "Auth End Date":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("claim_code":U,         "CHARACTER":U,    0, "x(30)":U,          "":U, "Claim Codes":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("prov_claims":U,        "INTEGER":U,      0, "":U,               "":U, "Provider Claims Count":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("total_prov-claims":U,  "DECIMAL":U,      0, "-ZZZ,ZZZ,ZZ9.99",  "":U, "Total Provider Claims":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("assoc_claims":U,       "INTEGER":U,      0, "":U,               "":U, "Associated Claims Count":U)
    lSuccess = phBodyTable:ADD-NEW-FIELD("total_assoc_claims":U, "DECIMAL":U,      0, "-ZZZ,ZZZ,ZZ9.99",  "":U, "Total Associated Claims":U)
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  ASSIGN  
    lSuccess = phBodyTable:ADD-NEW-FIELD("_Style_":U,       "CHARACTER":U,    0, "x(100)":U, "":U, "?":U)
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  ASSIGN
    lSuccess = phBodyTable:ADD-NEW-INDEX("ixSortOrder":U,  FALSE, TRUE)
    lSuccess = phBodyTable:ADD-INDEX-FIELD("ixSortOrder":U,"sort_order":U, "asc")
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  phBodyTable:TEMP-TABLE-PREPARE("DataTable":U) NO-ERROR.
  { mip/inc/mipreturnerror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


