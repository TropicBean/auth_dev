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

DEFINE VARIABLE ghDataBuffer        AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghMemauth           AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghMemauthh          AS HANDLE        NO-UNDO.

DEFINE TEMP-TABLE ttReportHead NO-UNDO RCODE-INFORMATION
  FIELD HeaderDescription AS CHARACTER  LABEL "Extract Description" FORMAT "x(80)":U.
  
DEFINE TEMP-TABLE ttAuth NO-UNDO
  FIELD MemberNo    AS CHARACTER
  FIELD AuthNo      AS CHARACTER
  FIELD Dep         AS INTEGER
  FIELD Scheme      AS CHARACTER
  FIELD Authdt      AS DATE
  FIELD ReqBy       AS CHARACTER
  FIELD AuthType    AS CHARACTER
  FIELD ReqServProv AS INTEGER
  FIELD ServProf    AS INTEGER
  FIELD AuthStDate  AS DATE
  FIELD AuthEndDate AS DATE
  FIELD AuthAmount  AS DECIMAL
  FIELD PrimICD10   AS CHARACTER
  FIELD SecICD10    AS CHARACTER
  FIELD PrimProc    AS CHARACTER
  FIELD SecProc     AS CHARACTER
  FIELD User_ID     AS CHARACTER
  FIELD Date_Time   AS DATETIME
  FIELD DetailLink  AS INTEGER
  FIELD ChangeDate  AS DATETIME
  INDEX indexByDate
    AuthNo     DESCENDING
    ChangeDate DESCENDING  
.
  
DEFINE TEMP-TABLE ttAuthDetail NO-UNDO
  FIELD DetailLine       AS INTEGER
  FIELD TariffCode       AS CHARACTER
  FIELD Quantity         AS DECIMAL
  FIELD Amount           AS DECIMAL
  FIELD DetailStartDt    AS DATE
  FIELD DetailEndDt      AS DATE
  FIELD DetailLink       AS INTEGER
  FIELD DetailNum        AS INTEGER
  FIELD DetailChangeDate AS DATE
  FIELD DetailAuthNum    AS CHARACTER
  INDEX indexDetailByDate
    DetailChangeDate DESCENDING
    DetailAuthNum    DESCENDING
    DetailLine       DESCENDING
.

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

&IF DEFINED(EXCLUDE-maGetAuthUpdateData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maGetAuthUpdateData Procedure 
PROCEDURE maGetAuthUpdateData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE phParmTable.
  DEFINE OUTPUT       PARAMETER TABLE-HANDLE phHeaderTable.
  DEFINE OUTPUT       PARAMETER TABLE-HANDLE phBodyTable.

  DEFINE VARIABLE dHeaderObj         AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE hTTHandle          AS HANDLE        NO-UNDO. 
  DEFINE VARIABLE hTTBuffer          AS HANDLE        NO-UNDO.

  DEFINE VARIABLE hQuery             AS HANDLE        NO-UNDO.
  DEFINE VARIABLE cQuery             AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE lCheck             AS LOGICAL       NO-UNDO.
  
  DEFINE VARIABLE cErrorMessage      AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cAuthNum           AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cMemNum            AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cUserCode          AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cAuthYear          AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cFromAuthDate      AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cToAuthDate        AS CHARACTER     NO-UNDO.
  
  DEFINE VARIABLE iHighestDetailCnt  AS INTEGER       NO-UNDO.
  DEFINE VARIABLE iCurrentDetailCnt  AS INTEGER       NO-UNDO.
  DEFINE VARIABLE iCount             AS INTEGER       NO-UNDO.
  DEFINE VARIABLE iTotalRecords      AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE iCnt               AS DECIMAL       NO-UNDO.
  
  ASSIGN iTotalRecords  = 0 NO-ERROR.
  
  ASSIGN phHeaderTable = TEMP-TABLE ttReportHead:HANDLE NO-ERROR.
  { mip/inc/mipreturnerror.i }
  
  ASSIGN
    cAuthNum      = getParameter(phParmTable, "fiAuthNum":U,   "":U)
    cMemNum       = getParameter(phParmTable, "fcMemNum":U,    "":U)
    cUserCode     = getParameter(phParmTable, "fcUserCode":U,  "":U)
    cAuthYear     = getParameter(phParmTable, "fcAuthYear":U,  "":U)
    cFromAuthDate = getParameter(phParmTable, "fdtFromDate":U, "":U)
    cToAuthDate   = getParameter(phParmTable, "fdtToDate":U,   "":U)   
  NO-ERROR.
  {mip/inc/mipreturnerror.i}
  
  CREATE QUERY hQuery NO-ERROR.
  { mip/inc/mipreturnerror.i }
  
  ASSIGN cQuery  = "FOR EACH memauth NO-LOCK "
         lCheck  = FALSE
         NO-ERROR.
  
  /* cAuthNum */       
  IF cAuthNum <> "" THEN
  DO:
    ASSIGN cQuery = cQuery
                  + "  WHERE memauth.auth-num = " + QUOTER(cAuthNum)
           lCheck = TRUE
    NO-ERROR.
  END. /* cAuthNum */
  
  /* cMemNum */
  IF cMemNum <> "" THEN
  DO:
    IF lCheck THEN
      ASSIGN cQuery = cQuery
                    + "  AND memauth.mem-num = " + QUOTER(cMemNum)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
                    + "  WHERE memauth.mem-num = " + QUOTER(cMemNum)
             lCheck = TRUE
       NO-ERROR.
  END. /* cMemNum */
  
  /* Query memauth History Table */
  ASSIGN cQuery  = cQuery
                 + ", EACH memauthh NO-LOCK"
                 + " WHERE memauthh.auth-num = memauth.auth-num"
  NO-ERROR.
  
  /* cUserCode */
  IF cUserCode <> "" THEN
    ASSIGN cQuery = cQuery
                  + " AND memauthh.user-id = " + QUOTER(cUserCode)
    NO-ERROR.
  
  /* cAuthYear */  
  IF cAuthYear <> "" THEN
    CASE cAuthYear:
      WHEN "Cur"  THEN ASSIGN cQuery = cQuery + " AND YEAR(memauthh.chg-date-time) = YEAR(TODAY)"     NO-ERROR.
      WHEN "Prev" THEN ASSIGN cQuery = cQuery + " AND YEAR(memauthh.chg-date-time) = YEAR(TODAY - 1)" NO-ERROR.
      WHEN "Hist" THEN ASSIGN cQuery = cQuery + " AND YEAR(memauthh.chg-date-time) < YEAR(TODAY - 1)" NO-ERROR.
    END CASE.
  
  /* cFromAuthDate */
  IF cFromAuthDate <> "" THEN
    ASSIGN cQuery = cQuery
                  + "  AND memauthh.chg-date-time >= " + QUOTER(cFromAuthDate)
    NO-ERROR.

  /* cToAuthDate */
  IF cToAuthDate <> "" THEN
    ASSIGN cQuery = cQuery
                  + "  AND memauthh.chg-date-time <= " + QUOTER(cToAuthDate)
    NO-ERROR.

  ASSIGN
    ghMemauth      = BUFFER memauth:HANDLE
    ghMemauthh     = BUFFER memauthh:HANDLE
  NO-ERROR.
  
  /* Query */
  hQuery:SET-BUFFERS(ghMemauth,ghMemauthh) NO-ERROR.
  
  IF NOT {&ErrorStatus} THEN hQuery:QUERY-PREPARE(cQuery) NO-ERROR.
  {mip/inc/mipreturnerror.i}
  IF NOT {&ErrorStatus} THEN hQuery:QUERY-OPEN()          NO-ERROR.
  {mip/inc/mipreturnerror.i}
  IF NOT {&ErrorStatus} THEN hQuery:GET-FIRST()           NO-ERROR.
  {mip/inc/mipreturnerror.i}

  ASSIGN 
    iCount = 0 
    iHighestDetailCnt = 0
  NO-ERROR.

  DO WHILE NOT hQuery:QUERY-OFF-END:
    
    ASSIGN iCount = iCount + 1. 
    
    /* Build Temp Tables from the query as the detail band columns need
     to be dynamic and we need to find how many there are for each query 
     run before creating the output table. */

    FIND FIRST ttAuth 
         WHERE ttAuth.AuthNo           = STRING(ghMemauthh:BUFFER-FIELD("auth-num":U):BUFFER-VALUE)
           AND DATE(ttAuth.ChangeDate) = DATE(ghMemauthh:BUFFER-FIELD("chg-date-time":U):BUFFER-VALUE)
     USE-INDEX indexByDate
      NO-ERROR.

    IF AVAILABLE ttAuth THEN hQuery:GET-NEXT() NO-ERROR.

    CREATE ttAuth NO-ERROR. 
      
    ASSIGN
      ttAuth.MemberNo    = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE
      ttAuth.AuthNo      = STRING(ghMemauthh:BUFFER-FIELD("auth-num":U):BUFFER-VALUE)
      ttAuth.Dep         = ghMemauthh:BUFFER-FIELD("dependant":U):BUFFER-VALUE  
      ttAuth.Authdt      = ghMemauthh:BUFFER-FIELD("auth-date":U):BUFFER-VALUE
      ttAuth.ReqBy       = ghMemauthh:BUFFER-FIELD("requested-by":U):BUFFER-VALUE
      ttAuth.AuthType    = ghMemauthh:BUFFER-FIELD("auth-type":U):BUFFER-VALUE
      ttAuth.ReqServProv = ghMemauthh:BUFFER-FIELD("req-pr-num":U):BUFFER-VALUE
      ttAuth.ServProf    = ghMemauthh:BUFFER-FIELD("pr-num":U):BUFFER-VALUE
      ttAuth.AuthStDate  = ghMemauthh:BUFFER-FIELD("auth-start-date":U):BUFFER-VALUE
      ttAuth.AuthEndDate = ghMemauthh:BUFFER-FIELD("auth-end-date":U):BUFFER-VALUE
      ttAuth.AuthAmount  = ghMemauthh:BUFFER-FIELD("amount-auth":U):BUFFER-VALUE
      ttAuth.PrimICD10   = ghMemauthh:BUFFER-FIELD("prim-diag":U):BUFFER-VALUE
      ttAuth.SecICD10    = ghMemauthh:BUFFER-FIELD("sec-diag":U):BUFFER-VALUE
      ttAuth.PrimProc    = ghMemauthh:BUFFER-FIELD("prim-proc":U):BUFFER-VALUE
      ttAuth.SecProc     = ghMemauthh:BUFFER-FIELD("sec-proc":U):BUFFER-VALUE
      ttAuth.ChangeDate  = ghMemauthh:BUFFER-FIELD("chg-date-time":U):BUFFER-VALUE
      ttAuth.User_ID     = ghMemauthh:BUFFER-FIELD("user-id":U):BUFFER-VALUE
      ttAuth.DetailLink  = iCount
    NO-ERROR.
    {mip/inc/mipreturnerror.i}
    
    FIND FIRST memsch NO-LOCK
         WHERE memsch.mem-num = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE 
      NO-ERROR.
    {&reseterror}
    
    IF AVAILABLE memsch THEN
    DO:
      FIND FIRST scheme NO-LOCK
           WHERE scheme.scheme-code = memsch.scheme-code
        NO-ERROR.
      {&reseterror}
      
      IF AVAILABLE scheme THEN
        ASSIGN ttAuth.Scheme = scheme.name.
      ELSE
        ASSIGN ttAuth.Scheme = "".
            
    END. /* AVAILABLE memsch */
    
    ASSIGN iCurrentDetailCnt = 1.
    
    DetailBLK:
    FOR EACH memauthdh NO-LOCK
       WHERE memauthdh.auth-num            = ghMemauthh:BUFFER-FIELD("auth-num":U):BUFFER-VALUE
         AND DATE(memauthdh.chg-date-time) = DATE(ghMemauthh:BUFFER-FIELD("chg-date-time":U):BUFFER-VALUE)
          BY memauthdh.chg-date-time:

      FIND FIRST ttAuthDetail
           WHERE ttAuthDetail.DetailChangeDate = DATE(memauthdh.chg-date-time)
             AND ttAuthDetail.DetailAuthNum    = STRING(memauthdh.auth-num)
             AND ttAuthDetail.DetailLine       = memauthdh.line-#
       USE-INDEX indexDetailByDate
        NO-ERROR.

      IF AVAILABLE ttAuthDetail THEN NEXT DetailBLK.

      CREATE ttAuthDetail NO-ERROR.
      
      ASSIGN
        ttAuthDetail.DetailLine       = memauthdh.line-#
        ttAuthDetail.TariffCode       = memauthdh.tariff-code
        ttAuthDetail.Quantity         = memauthdh.quantity
        ttAuthDetail.Amount           = memauthdh.amount-auth
        ttAuthDetail.DetailStartDt    = memauthdh.auth-start-date
        ttAuthDetail.DetailEndDt      = memauthdh.auth-end-date
        ttAuthDetail.DetailLink       = ttAuth.DetailLink
        ttAuthDetail.DetailChangeDate = DATE(memauthdh.chg-date-time)
        ttAuthDetail.DetailAuthNum    = STRING(memauthdh.auth-num)
        ttAuthDetail.DetailNum        = iCurrentDetailCnt
      NO-ERROR.
      
      ASSIGN iCurrentDetailCnt = iCurrentDetailCnt + 1.
      
      /* Get the max amount of detail bands required */
      IF iCurrentDetailCnt > iHighestDetailCnt THEN 
        ASSIGN iHighestDetailCnt = iCurrentDetailCnt.
         
    END. /* FOR EACH memauthdh */
      
    hQuery:GET-NEXT() NO-ERROR.
    {mip/inc/mipreturnerror.i}
      
  END. /* hQuery */

  IF iCurrentDetailCnt > 1 THEN
    ASSIGN iCurrentDetailCnt = iCurrentDetailCnt - 1.

  /* Build Return Table */
  RUN setResourceTable IN TARGET-PROCEDURE (INPUT  iHighestDetailCnt ,
                                            OUTPUT phBodyTable       )
                                            NO-ERROR.
  {mip/inc/mipreturnerror.i}
  
  IF NOT {&ErrorStatus} AND VALID-HANDLE(phBodyTable) THEN
  DO:
    ASSIGN
      ghDataBuffer = phBodyTable:DEFAULT-BUFFER-HANDLE
    NO-ERROR.
  { mip/inc/mipreturnerror.i }
  END. /* VALID-HANDLE(phBodyTable) */
  
  FOR EACH ttAuth
        BY ttAuth.ChangeDate DESCENDING:
  
    ghDataBuffer:BUFFER-CREATE NO-ERROR.
      {mip/inc/mipreturnerror.i}
      
    ASSIGN iTotalRecords = iTotalRecords + 1 NO-ERROR.
    
    /* Fill Return Table using the Temp Tables */
    ASSIGN
      ghDataBuffer:BUFFER-FIELD("sort_order":U):BUFFER-VALUE  = ttAuth.Date_Time
      ghDataBuffer:BUFFER-FIELD("mem_num"):BUFFER-VALUE       = ttAuth.MemberNo
      ghDataBuffer:BUFFER-FIELD("dep"):BUFFER-VALUE           = ttAuth.Dep
      ghDataBuffer:BUFFER-FIELD("scheme"):BUFFER-VALUE        = ttAuth.Scheme
      ghDataBuffer:BUFFER-FIELD("auth_no"):BUFFER-VALUE       = ttAuth.AuthNo
      ghDataBuffer:BUFFER-FIELD("auth_date"):BUFFER-VALUE     = ttAuth.Authdt
      ghDataBuffer:BUFFER-FIELD("req_by"):BUFFER-VALUE        = ttAuth.ReqBy
      ghDataBuffer:BUFFER-FIELD("auth_type"):BUFFER-VALUE     = ttAuth.AuthType
      ghDataBuffer:BUFFER-FIELD("req_prov"):BUFFER-VALUE      = ttAuth.ReqServProv
      ghDataBuffer:BUFFER-FIELD("prov"):BUFFER-VALUE          = ttAuth.ServProf
      ghDataBuffer:BUFFER-FIELD("auth_start_dt"):BUFFER-VALUE = ttAuth.AuthStDate
      ghDataBuffer:BUFFER-FIELD("auth_end_dt"):BUFFER-VALUE   = ttAuth.AuthEndDate
      ghDataBuffer:BUFFER-FIELD("amount"):BUFFER-VALUE        = ttAuth.AuthAmount
      ghDataBuffer:BUFFER-FIELD("prim_10"):BUFFER-VALUE       = ttAuth.PrimICD10
      ghDataBuffer:BUFFER-FIELD("sec_10"):BUFFER-VALUE        = ttAuth.SecICD10
      ghDataBuffer:BUFFER-FIELD("prim_proc"):BUFFER-VALUE     = ttAuth.PrimProc
      ghDataBuffer:BUFFER-FIELD("sec_proc"):BUFFER-VALUE      = ttAuth.SecProc
      ghDataBuffer:BUFFER-FIELD("user_updt"):BUFFER-VALUE     = ttAuth.User_ID
      ghDataBuffer:BUFFER-FIELD("date_time"):BUFFER-VALUE     = ttAuth.ChangeDate
    NO-ERROR.
    {mip/inc/mipreturnerror.i}

    IF iHighestDetailCnt >= 0 THEN
      FOR EACH ttAuthDetail
         WHERE ttAuthDetail.DetailLink = ttAuth.DetailLink
            BY ttAuthDetail.DetailNum:
         
        ASSIGN
          ghDataBuffer:BUFFER-FIELD("detail_line_"   + STRING(ttAuthDetail.DetailNum)):BUFFER-VALUE = ttAuthDetail.DetailLine
          ghDataBuffer:BUFFER-FIELD("tariff_code_"   + STRING(ttAuthDetail.DetailNum)):BUFFER-VALUE = ttAuthDetail.TariffCode
          ghDataBuffer:BUFFER-FIELD("quantity_"      + STRING(ttAuthDetail.DetailNum)):BUFFER-VALUE = ttAuthDetail.Quantity
          ghDataBuffer:BUFFER-FIELD("amount_"        + STRING(ttAuthDetail.DetailNum)):BUFFER-VALUE = ttAuthDetail.Amount
          ghDataBuffer:BUFFER-FIELD("dt_start_date_" + STRING(ttAuthDetail.DetailNum)):BUFFER-VALUE = ttAuthDetail.DetailStartDt
          ghDataBuffer:BUFFER-FIELD("dt_end_date_"   + STRING(ttAuthDetail.DetailNum)):BUFFER-VALUE = ttAuthDetail.DetailEndDt
        NO-ERROR.
        {mip/inc/mipreturnerror.i}         
         
      END. /* EACH ttAuthDetail */
  
  END. /* ttAuth */
  
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
          ghDataBuffer:BUFFER-FIELD("sort_order"):BUFFER-VALUE  = iCnt
          ghDataBuffer:BUFFER-FIELD("_Style_"):BUFFER-VALUE     = "FONT:BOLD":U
          iCnt                                                 = iCnt + 1
          NO-ERROR.
  {mip/inc/mipreturnerror.i}
    
  FINALLY:
    ASSIGN cErrorMessage = {mip/inc/mipreturnvalue.i}
           NO-ERROR.

    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery NO-ERROR. {mip/inc/mipmessageerror.i &ResetError = TRUE}

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

  DEFINE INPUT  PARAMETER ipiDetailBands    AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER phBodyTable       AS HANDLE     NO-UNDO.
  
  DEFINE VARIABLE lSuccess            AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iCnt                AS INTEGER     NO-UNDO.
  
  CREATE TEMP-TABLE phBodyTable NO-ERROR.
  {mip/inc/mipreturnerror.i}
  
  ASSIGN
    lSuccess = phBodyTable:ADD-NEW-FIELD( "user_updt":U,     "CHARACTER":U, 0, "x(15)":U,           "":U, "User Updated":U         )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "date_time":U,     "DATETIME":U,  0, "":U,                "":U, "Change Date and time":U )
    lSuccess = phBodyTable:ADD-NEW-FIELD("sort_order":U,     "INTEGER":U,   0, "":U,                "":U, "?":U                    ) 
    lSuccess = phBodyTable:ADD-NEW-FIELD( "mem_num":U,       "CHARACTER":U, 0, "x(15)":U,           "":U, "Member Number":U        )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "dep":U,           "INTEGER":U,   0, "":U,                "":U, "Dependant":U            )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "scheme":U,        "CHARACTER":U, 0, "x(15)":U,           "":U, "Scheme":U               )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "auth_no":U,       "CHARACTER":U, 0, "x(15)":U,           "":U, "Auth No":U              )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "auth_date":U,     "DATE":U,      0, "99/99/9999":U,      "":U, "Auth Date":U            )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "req_by":U,        "CHARACTER":U, 0, "x(15)":U,           "":U, "Requested by":U         )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "auth_type":U,     "CHARACTER":U, 0, "x(15)":U,           "":U, "Auth Type":U            )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "req_prov":U,      "INTEGER":U,   0, "":U,                "":U, "ReqServProv":U          )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "prov":U,          "INTEGER":U,   0, "":U,                "":U, "ServProf":U             )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "auth_start_dt":U, "DATE":U,      0, "99/99/9999":U,      "":U, "AuthStartDate":U        )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "auth_end_dt":U,   "DATE":U,      0, "99/99/9999":U,      "":U, "AuthEndDate":U          )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "amount":U,        "DECIMAL":U,   0, "-ZZZ,ZZZ,ZZ9.99":U, "":U, "AuthAmount":U           )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "prim_10":U,       "CHARACTER":U, 0, "x(15)":U,           "":U, "PrimICD10":U            )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "sec_10":U,        "CHARACTER":U, 0, "x(15)":U,           "":U, "SecICD10":U             )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "prim_proc":U,     "CHARACTER":U, 0, "x(15)":U,           "":U, "PrimProc":U             )
    lSuccess = phBodyTable:ADD-NEW-FIELD( "sec_proc":U,      "CHARACTER":U, 0, "x(15)":U,           "":U, "SecProc":U              )
  NO-ERROR.
  {mip/inc/mipreturnerror.i}
  
  /* Build Detail Bands */
  IF ipiDetailBands >= 1 THEN
  DO iCnt = 1 TO ipiDetailBands:
    
    ASSIGN
      lSuccess = phBodyTable:ADD-NEW-FIELD( "detail_line_":U   + STRING(iCnt), "INTEGER":U,   0, "":U,                "":U, "Detail Line":U       )
      lSuccess = phBodyTable:ADD-NEW-FIELD( "tariff_code_":U   + STRING(iCnt), "CHARACTER":U, 0, "x(15)":U,           "":U, "Tariff Code":U       )
      lSuccess = phBodyTable:ADD-NEW-FIELD( "quantity_":U      + STRING(iCnt), "DECIMAL":U,   0, "-ZZZ,ZZZ,ZZ9.99":U, "":U, "Quantity":U          )
      lSuccess = phBodyTable:ADD-NEW-FIELD( "amount_":U        + STRING(iCnt), "DECIMAL":U,   0, "-ZZZ,ZZZ,ZZ9.99":U, "":U, "Amount":U            )
      lSuccess = phBodyTable:ADD-NEW-FIELD( "dt_start_date_":U + STRING(iCnt), "DATE":U,      0, "99/99/9999":U,      "":U, "Detail Start Date":U )
      lSuccess = phBodyTable:ADD-NEW-FIELD( "dt_end_date_":U   + STRING(iCnt), "DATE":U,      0, "99/99/9999":U,      "":U, "Detail End Date":U   )
    NO-ERROR.
    {mip/inc/mipreturnerror.i}
    
  END. /* ipiDetailBands => 1 */
  
  ASSIGN  
    lSuccess = phBodyTable:ADD-NEW-FIELD("_Style_":U, "CHARACTER":U, 0, "x(100)":U, "":U, "?":U)
    lSuccess = phBodyTable:ADD-NEW-INDEX("ixSortOrder":U, FALSE, TRUE)
    lSuccess = phBodyTable:ADD-INDEX-FIELD("ixSortOrder":U, "sort_order":U, "desc")
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  phBodyTable:TEMP-TABLE-PREPARE("DataTable":U) NO-ERROR.
  {mip/inc/mipreturnerror.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
