&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    $Id: maexusplipp.p,v 1.1.2.5 2010/05/52 09:24:46 carolb Exp $

    Purpose:
  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
{mip/inc/mipdefshared.i}
{gen/inc/rxrepparameter.i}

DEFINE VARIABLE ghDataBuffer        AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghMemauth           AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghExtref            AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghMemauthm          AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghDoctor            AS HANDLE        NO-UNDO.

DEFINE TEMP-TABLE ttReportHead NO-UNDO RCODE-INFORMATION
  FIELD HeaderDescription AS CHARACTER  LABEL "Extract Description" FORMAT "x(80)":U.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getLastExtRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastExtRef Procedure 
FUNCTION getLastExtRef RETURNS CHARACTER
  ( INPUT ipcRefCode AS CHARACTER ,
    INPUT ipiAuthNum AS INTEGER)  FORWARD.

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
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.52
         WIDTH              = 48.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-maGetAuthData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maGetAuthData Procedure 
PROCEDURE maGetAuthData :
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
                                                      
  DEFINE VARIABLE cErrorMessage      AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cAuthNum           AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cDocNum            AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cDocName           AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cAuthYear          AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cDependant         AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cFromAuthDate      AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cToAuthDate        AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE iStatus            AS INTEGER       NO-UNDO.
  DEFINE VARIABLE cPrimDiag          AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cNAPPICode         AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cFromExpDate       AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cToExpDate         AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cRefAuthNum        AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cExternalRef       AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cAuthType          AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cMemNum            AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cStatReason        AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cCheck             AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cAuthStatus        AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cMoreDetails       AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE lShowDetail        AS LOGICAL       NO-UNDO.
  DEFINE VARIABLE cAuthDetails       AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE lShowAuthDetail    AS LOGICAL       NO-UNDO.
  DEFINE VARIABLE cInpatientStatus   AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cRxProcessStatus   AS CHARACTER     NO-UNDO.

  DEFINE VARIABLE iTotalRecords      AS DECIMAL       NO-UNDO.
                                                      
  DEFINE VARIABLE iCnt               AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE iRoleCnt           AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE iLoop              AS DECIMAL       NO-UNDO.
  DEFINE VARIABLE iCount             AS DECIMAL       NO-UNDO.

  ASSIGN
      iTotalRecords  = 0
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  ASSIGN phHeaderTable = TEMP-TABLE ttReportHead:HANDLE NO-ERROR.
  { mip/inc/mipreturnerror.i }

  ASSIGN
    cAuthNum                        = getParameter(phParmTable, "fiAuthNum":U, "":U)
    cDocNum                         = getParameter(phParmTable, "fiDocNum":U, "":U) 
    cDocName                        = getParameter(phParmTable, "fcDocName":U, "":U)
    cAuthYear                       = getParameter(phParmTable, "fcAuthYear":U, "":U)
    cDependant                      = getParameter(phParmTable, "fiDependant":U, "":U)
    cFromAuthDate                   = getParameter(phParmTable, "fdtFromDate":U, "":U)
    cToAuthDate                     = getParameter(phParmTable, "fdtToDate":U, "":U) 
    iStatus                         = INTEGER(getParameter(phParmTable, "fcStatus":U, "":U)) 
    cPrimDiag                       = getParameter(phParmTable, "fcPrimDiag":U, "":U)
    cNAPPICode                      = getParameter(phParmTable, "fcNappiCode":U, "":U)
    cFromExpDate                    = getParameter(phParmTable, "fcFromExpDate":U, "":U)
    cToExpDate                      = getParameter(phParmTable, "fcToExpDate":U, "":U)
    cRefAuthNum                     = getParameter(phParmTable, "fcRefAuthNum":U, "":U)
    cExternalRef                    = getParameter(phParmTable, "fcExternalRef":U, "":U)
    cAuthType                       = getParameter(phParmTable, "fcAuthType":U, "":U)
    cMemNum                         = getParameter(phParmTable, "fcMemNum":U, "":U)
    cStatReason                     = getParameter(phParmTable, "fcStatReason":U, "":U)
    cMoreDetails                    = getParameter(phParmTable, "fcMoreDetails":U, "":U)
    lShowDetail                     = (IF cMoreDetails = "":U    THEN FALSE ELSE TRUE)
    cAuthDetails                    = getParameter(phParmTable, "fcAuthDetails":U, "":U)
    lShowAuthDetail                 = (IF cAuthDetails = "":U    THEN FALSE ELSE TRUE) 
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  RUN setResourceTable IN TARGET-PROCEDURE (INPUT lShowDetail,
                                            INPUT lShowAuthDetail,
                                            OUTPUT phBodyTable)
  NO-ERROR.
  { mip/inc/mipreturnerror.i }

  IF NOT {&ErrorStatus} AND VALID-HANDLE(phBodyTable) THEN
  DO:
    ASSIGN
      ghDataBuffer   = phBodyTable:DEFAULT-BUFFER-HANDLE
      ghMemauth      = BUFFER memauth:HANDLE
      ghMemauthm     = BUFFER memauthm:HANDLE
      ghExtref       = BUFFER extref:HANDLE
      ghDoctor       = BUFFER doctor:HANDLE
    NO-ERROR.
  { mip/inc/mipreturnerror.i }
  END.

  CREATE QUERY hQuery NO-ERROR.
  { mip/inc/mipreturnerror.i }

  ASSIGN cQuery = "FOR EACH memauth NO-LOCK "
         cCheck = "no"
         NO-ERROR.  

  IF cAuthNum <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.auth-num = " + QUOTER(cAuthNum)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.auth-num = " + QUOTER(cAuthNum)
             cCheck = "yes"
       NO-ERROR.
  END.
  
  IF cMemNum <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.mem-num = " + QUOTER(cMemNum)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.mem-num = " + QUOTER(cMemNum)
             cCheck = "yes"
       NO-ERROR.
  END.
  
  IF cDocNum <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.pr-num = " + STRING(cDocNum)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.pr-num = " + STRING(cDocNum)
             cCheck = "yes"
       NO-ERROR.
  END.
  
  IF cAuthYear <> "" THEN
  DO:
    IF cCheck = "yes" THEN
    DO:
      CASE cAuthYear:
        WHEN "Cur"  THEN ASSIGN cQuery = cQuery + "  AND YEAR(memauth.auth-date) = YEAR(TODAY)" NO-ERROR.
        WHEN "Prev" THEN ASSIGN cQuery = cQuery + "  AND YEAR(memauth.auth-date) = YEAR(TODAY - 1)" NO-ERROR.
        WHEN "Hist" THEN ASSIGN cQuery = cQuery + "  AND YEAR(memauth.auth-date) < YEAR(TODAY - 1)" NO-ERROR.
      END CASE.
    END.
    ELSE DO:
      CASE cAuthYear:
        WHEN "Cur"  THEN ASSIGN cQuery = cQuery + " WHERE YEAR(memauth.auth-date) = YEAR(TODAY)" NO-ERROR.
        WHEN "Prev" THEN ASSIGN cQuery = cQuery + " WHERE YEAR(memauth.auth-date) = YEAR(TODAY - 1)" NO-ERROR.
        WHEN "Hist" THEN ASSIGN cQuery = cQuery + " WHERE YEAR(memauth.auth-date) < YEAR(TODAY - 1)" NO-ERROR.
      END CASE.

      ASSIGN cCheck = "yes" NO-ERROR.
    END.
  END.

  IF cDependant <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.dependant = " + STRING(cDependant)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.dependant = " + STRING(cDependant)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cFromAuthDate <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.auth-date >= " + QUOTER(cFromAuthDate)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.auth-date >= " + QUOTER(cFromAuthDate)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cToAuthDate <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.auth-date <= " + QUOTER(cToAuthDate)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.auth-date <= " + QUOTER(cToAuthDate)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF iStatus <> 7 THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.authstat = " + STRING(iStatus)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.authstat = " + STRING(iStatus)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cPrimDiag <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.prim-diag = " + QUOTER(cPrimDiag)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.prim-diag = " + QUOTER(cPrimDiag)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cFromExpDate <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.expected-start-date >= " + QUOTER(cFromExpDate)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.expected-start-date >= " + QUOTER(cFromExpDate)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cToExpDate <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.expected-end-date <= " + QUOTER(cToExpDate)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.expected-end-date <= " + QUOTER(cToExpDate)
             cCheck = "yes"
       NO-ERROR.
  END.
  
  IF cRefAuthNum <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.reference-auth-num = " + QUOTER(cRefAuthNum)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.reference-auth-num = " + QUOTER(cRefAuthNum)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cAuthType <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.auth-type = " + QUOTER(cAuthType)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.auth-type = " + QUOTER(cAuthType)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cStatReason <> "" THEN
  DO:
    IF cCheck = "yes" THEN
      ASSIGN cQuery = cQuery
              + "  AND memauth.authstat-note = " + QUOTER(cStatReason)
       NO-ERROR.
    ELSE
      ASSIGN cQuery = cQuery
              + "  WHERE memauth.authstat-note = " + QUOTER(cStatReason)
             cCheck = "yes"
       NO-ERROR.
  END.

  IF cExternalRef <> "":U AND NOT {&ErrorStatus} THEN
    ASSIGN cQuery   = cQuery
                    + " , EACH extref NO-LOCK":U
                    + "  WHERE extref.owning-entity-mnemonic = 'memauth'":U 
                    + "    AND extref.owning-key = string(memauth.auth-num)":U 
                    + "    AND extref.reference-value CONTAINS ":U + QUOTER(cExternalRef) 
           NO-ERROR.
           {mip/inc/mipreturnerror.i}

  IF cNappiCode <> "" AND NOT {&ErrorStatus} THEN
    ASSIGN cQuery   = cQuery 
                    + " , FIRST memauthm NO-LOCK "
                    + "   WHERE memauthm.auth-num = memauth.auth-num "
                    + "     AND memauthm.nappi-code = " + QUOTER(cNappiCode)
      NO-ERROR.
  {mip/inc/mipreturnerror.i}


  IF cDocName <> "" AND NOT {&ErrorStatus} THEN
    ASSIGN cQuery   = cQuery 
                    + " , FIRST doctor NO-LOCK "
                    + "   WHERE doctor.doc-num = memauth.pr-num "
                    + "     AND doctor.name BEGINS " + QUOTER(cDocName)
      NO-ERROR.
  {mip/inc/mipreturnerror.i}

    MESSAGE "cQuery: " cQuery
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF cNappiCode = "" AND cExternalRef = "" AND cDocName = "" THEN
    hQuery:SET-BUFFERS(ghMemauth) NO-ERROR.
  ELSE
  IF cNappiCode <> "" AND cExternalRef = "" AND cDocName = "" THEN
    hQuery:SET-BUFFERS(ghMemauth,ghMemauthm) NO-ERROR.
  ELSE
  IF cNappiCode = "" AND cExternalRef <> "" AND cDocName = "" THEN
    hQuery:SET-BUFFERS(ghMemauth,ghExtref) NO-ERROR.
  ELSE
  IF cNappiCode = "" AND cExternalRef = "" AND cDocName <> "" THEN
    hQuery:SET-BUFFERS(ghMemauth,ghDoctor) NO-ERROR.
  ELSE
  IF cNappiCode <> "" AND cExternalRef <> "" AND cDocName = "" THEN
    hQuery:SET-BUFFERS(ghMemauth,ghExtref,ghMemauthm) NO-ERROR.
  ELSE
  IF cNappiCode <> "" AND cExternalRef = "" AND cDocName <> "" THEN
    hQuery:SET-BUFFERS(ghMemauth,ghMemauthm,ghDoctor) NO-ERROR.
  ELSE
  IF cNappiCode = "" AND cExternalRef <> "" AND cDocName <> "" THEN
    hQuery:SET-BUFFERS(ghMemauth,ghExtref,ghDoctor) NO-ERROR.
  ELSE
  IF cNappiCode <> "" AND cExternalRef <> "" AND cDocName <> "" THEN
    hQuery:SET-BUFFERS(ghMemauth,ghExtref,ghMemauthm,ghDoctor) NO-ERROR.
    { mip/inc/mipreturnerror.i }

  /*The following piece of code is to cater for users that don't
    have a role allocated, otherwise it doesn't show any records for them*/
  
  IF NOT {&ErrorStatus} THEN hQuery:QUERY-PREPARE(cQuery) NO-ERROR.
  { mip/inc/mipreturnerror.i }
  IF NOT {&ErrorStatus} THEN hQuery:QUERY-OPEN()          NO-ERROR.
  { mip/inc/mipreturnerror.i }
  IF NOT {&ErrorStatus} THEN hQuery:GET-FIRST()           NO-ERROR.
  { mip/inc/mipreturnerror.i }
  
  DO WHILE NOT hQuery:QUERY-OFF-END:
  
    ghDataBuffer:BUFFER-CREATE NO-ERROR.
      {mip/inc/mipreturnerror.i}
    
    ASSIGN iCount        = iCount + INTEGER(ghMemauth:BUFFER-FIELD("auth-num"):BUFFER-VALUE)
           iTotalRecords = iTotalRecords + 1
           NO-ERROR.
          { mip/inc/mipreturnerror.i }

    ASSIGN 
      ghDataBuffer:BUFFER-FIELD("sort_order":U):BUFFER-VALUE     = ghMemauth:BUFFER-FIELD("auth-date":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("auth_num":U):BUFFER-VALUE       = ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("mem_num":U):BUFFER-VALUE        = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("auth_date":U):BUFFER-VALUE      = ghMemauth:BUFFER-FIELD("auth-date":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("auth_amt":U):BUFFER-VALUE       = ghMemauth:BUFFER-FIELD("amount-auth":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("stat_reason":U):BUFFER-VALUE    = ghMemauth:BUFFER-FIELD("authstat-note":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("start_date":U):BUFFER-VALUE     = ghMemauth:BUFFER-FIELD("auth-start-date":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("end_date":U):BUFFER-VALUE       = ghMemauth:BUFFER-FIELD("auth-end-date":U):BUFFER-VALUE
      ghDataBuffer:BUFFER-FIELD("auth_type":U):BUFFER-VALUE      = ghMemauth:BUFFER-FIELD("auth-type":U):BUFFER-VALUE 
      NO-ERROR.
    {mip/inc/mipreturnerror.i}

    IF cMoreDetails = "" THEN
      ASSIGN ghDataBuffer:BUFFER-FIELD("reference":U):BUFFER-VALUE      = ghMemauth:BUFFER-FIELD("reference-auth-num":U):BUFFER-VALUE
             NO-ERROR. 
    
    FIND FIRST memdep NO-LOCK
         WHERE memdep.mem-num   = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE
           AND memdep.dependant = ghMemauth:BUFFER-FIELD("dependant":U):BUFFER-VALUE
      NO-ERROR.

    IF AVAILABLE memdep THEN DO:
      IF cMoreDetails <> "" THEN
        ASSIGN ghDataBuffer:BUFFER-FIELD("dependant":U):BUFFER-VALUE =  STRING(memdep.dependant) NO-ERROR.
      ELSE
       ASSIGN ghDataBuffer:BUFFER-FIELD("dependant":U):BUFFER-VALUE  = STRING(memdep.dependant) + " - " + replace(memdep.first-name, ",", " ") NO-ERROR.
    END.
    ELSE
       ASSIGN ghDataBuffer:BUFFER-FIELD("dependant":U):BUFFER-VALUE  = ghMemauth:BUFFER-FIELD("dependant":U):BUFFER-VALUE NO-ERROR.
    
    FIND FIRST doctor NO-LOCK
         WHERE doctor.doc-num = ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE
      NO-ERROR.
     
    IF AVAILABLE doctor THEN DO:
       IF cMoreDetails <> "" THEN
          ASSIGN  ghDataBuffer:BUFFER-FIELD("serv_name":U):BUFFER-VALUE = replace(doctor.name, ",", " ")
                  ghDataBuffer:BUFFER-FIELD("serv_num":U):BUFFER-VALUE  = STRING(doctor.doc-num, "9999999")
          NO-ERROR.
       ELSE           
          ASSIGN ghDataBuffer:BUFFER-FIELD("serv_prov":U):BUFFER-VALUE  = STRING(doctor.doc-num, "9999999") + " - " + replace(doctor.name, ",", " ") NO-ERROR.
    END.   
    ELSE DO:
       IF cMoreDetails <> "" THEN
          ASSIGN  ghDataBuffer:BUFFER-FIELD("serv_name":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE NO-ERROR.
       ELSE 
        ASSIGN ghDataBuffer:BUFFER-FIELD("serv_prov":U):BUFFER-VALUE    = ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE NO-ERROR.
    END.
    
    FIND FIRST diagnos NO-LOCK
         WHERE diagnos.diagnosis = ghMemauth:BUFFER-FIELD("prim-diag":U):BUFFER-VALUE
      NO-ERROR.

    IF AVAILABLE diagnos THEN DO:
       IF cMoreDetails <> "" THEN
          ASSIGN ghDataBuffer:BUFFER-FIELD("diag_code":U):BUFFER-VALUE = STRING(diagnos.diagnosis)
                 ghDataBuffer:BUFFER-FIELD("diag_desc":U):BUFFER-VALUE = replace(diagnos.description, ",", " ")
          NO-ERROR.
       ELSE          
          ASSIGN ghDataBuffer:BUFFER-FIELD("prim_diag":U):BUFFER-VALUE = STRING(diagnos.diagnosis) + " - " + replace(diagnos.description, ",", " ") NO-ERROR.
    END.
    ELSE DO:
       IF cMoreDetails <> "" THEN
          ASSIGN ghDataBuffer:BUFFER-FIELD("diag_code":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("prim-diag":U):BUFFER-VALUE NO-ERROR.
       ELSE 
          ASSIGN ghDataBuffer:BUFFER-FIELD("prim_diag":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("prim-diag":U):BUFFER-VALUE NO-ERROR.
    END.
    
    ASSIGN cAuthStatus = ghMemauth:BUFFER-FIELD("authstat":U):BUFFER-VALUE NO-ERROR.

    CASE cAuthStatus:
      WHEN "0" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "Pending".
      WHEN "1" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "Authorised".
      WHEN "2" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "Assessed".
      WHEN "3" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "Pulled".
      WHEN "4" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "Complete".
      WHEN "5" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "Cancelled".
      WHEN "6" THEN ASSIGN ghDataBuffer:BUFFER-FIELD("auth_stat":U):BUFFER-VALUE = "Declined".
    END CASE.
    
    /* High cost auth */
    IF CAN-FIND(datalist WHERE datalist.list-type   = "MCACUST":U
                           AND datalist.list-code   = "DOHIGHCOST":U
                           AND datalist.description = "YES":U
                           AND datalist.link-from   = "":U)
    THEN DO:
      FIND FIRST extref NO-LOCK
      WHERE extref.interface-type         = "MIP"
        AND extref.reference-code         = "HighCost"
        AND extref.owning-entity-mnemonic = "memauth"
        AND extref.owning-key             = ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE
        AND extref.owning-obj             = 0
        AND extref.reference-value        = "Yes"
      NO-ERROR.
      
      IF AVAILABLE extref 
      THEN DO:
         ASSIGN ghDataBuffer:BUFFER-FIELD("highcost_auth":U):BUFFER-VALUE = extref.reference-value NO-ERROR.
      END.  
    END. /*IF CAN-FIND(datalist ... */
    
    IF ssystem.admin-comp = 20
    THEN DO:
      ASSIGN cRxProcessStatus = getLastExtRef(INPUT "RXHProcessStatus",  ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE)
             cInpatientStatus = getLastExtRef(INPUT "RXHInpatientStatus", ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE).
      
      ASSIGN ghDataBuffer:BUFFER-FIELD("rx_inpatient_status":U):BUFFER-VALUE = IF cInpatientStatus = "" THEN "N/A" ELSE cInpatientStatus
             ghDataBuffer:BUFFER-FIELD("rx_process_status":U):BUFFER-VALUE   = IF cRxProcessStatus = "" THEN "N/A" ELSE cRxProcessStatus.
    END.
    
    /* More details */ 
    IF cMoreDetails <> "" THEN
       RUN maGetMoreDetails IN TARGET-PROCEDURE NO-ERROR.

    IF cAuthDetails <> "" THEN
       RUN maGetAuthDetails IN TARGET-PROCEDURE NO-ERROR.
    
    hQuery:GET-NEXT() NO-ERROR.
    { mip/inc/mipreturnerror.i } 
  END.
    
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

&IF DEFINED(EXCLUDE-maGetAuthDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maGetAuthDetails Procedure 
PROCEDURE maGetAuthDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iIndex             AS INTEGER       NO-UNDO.
   
  DEFINE BUFFER memauthd    FOR memauthd.

  AUTH-DET:
  FOR EACH memauthd NO-LOCK                          
    WHERE memauthd.auth-num  = ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE:
    
    ASSIGN iIndex = iIndex + 1.

    /* Note: The auth details can be any number of records, so we are using 
       15 iterations to hopefully capture the most prevalent possibilities.
       If more than 15 then show no 15 as values continue... */
    IF iIndex = 16 THEN DO:
      ASSIGN ghDataBuffer:BUFFER-FIELD("tariff_code15":U):BUFFER-VALUE  = "Ctd.."
             ghDataBuffer:BUFFER-FIELD("quantity15":U):BUFFER-VALUE     = 0
             ghDataBuffer:BUFFER-FIELD("claim_code15":U):BUFFER-VALUE   = 0
             NO-ERROR.      
      {mip/inc/mipreturnerror.i}
      LEAVE AUTH-DET.
    END.

    ASSIGN ghDataBuffer:BUFFER-FIELD("tariff_code":U + STRING(iIndex,"99":U)):BUFFER-VALUE  = memauthd.tariff-code
           ghDataBuffer:BUFFER-FIELD("quantity":U + STRING(iIndex,"99":U)):BUFFER-VALUE     = memauthd.quantity
           ghDataBuffer:BUFFER-FIELD("claim_code":U + STRING(iIndex,"99":U)):BUFFER-VALUE   = memauthd.claim-code
           NO-ERROR.      
    {mip/inc/mipreturnerror.i}

  END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-maGetMoreDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maGetMoreDetails Procedure 
PROCEDURE maGetMoreDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCounter           AS INTEGER       NO-UNDO.
  DEFINE VARIABLE dAgeDate           AS DATE          NO-UNDO.
   
  DEFINE BUFFER memdet      FOR memdet.
  DEFINE BUFFER memsch      FOR memsch.
  DEFINE BUFFER scheme      FOR scheme.
  DEFINE BUFFER company     FOR company.
  DEFINE BUFFER doctor      FOR doctor. 
  DEFINE BUFFER docneggroup FOR docneggroup.
  DEFINE BUFFER neggroup    FOR neggroup.
  DEFINE BUFFER docdisp     FOR docdisp.
  DEFINE BUFFER prtype      FOR prtype.
  DEFINE BUFFER proccode    FOR proccode.
  DEFINE BUFFER conddiag    FOR conddiag.
  DEFINE BUFFER memauthic   FOR memauthic.
  DEFINE BUFFER diagnos     FOR diagnos.
  DEFINE BUFFER bconddiag   FOR conddiag.
       
  FIND memdet NO-LOCK
    WHERE memdet.mem-num = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE
  NO-ERROR.
  FIND FIRST memsch NO-LOCK
    WHERE memsch.mem-num = ghMemauth:BUFFER-FIELD("mem-num":U):BUFFER-VALUE
  NO-ERROR.
  IF AVAILABLE memsch THEN DO:
    /* Scheme */
    FIND FIRST scheme NO-LOCK
      WHERE scheme.scheme-code = memsch.scheme-code
    NO-ERROR.
    IF AVAILABLE scheme THEN 
      ASSIGN ghDataBuffer:BUFFER-FIELD("scheme_name":U):BUFFER-VALUE = scheme.short-name NO-ERROR.
    ELSE 
      ASSIGN ghDataBuffer:BUFFER-FIELD("scheme_name":U):BUFFER-VALUE = "":U NO-ERROR.
    
    /* company/Employer */  
    FIND FIRST company NO-LOCK
      WHERE company.comp-code = memsch.comp-code
    NO-ERROR.
    IF AVAILABLE company THEN
      ASSIGN ghDataBuffer:BUFFER-FIELD("company":U):BUFFER-VALUE = company.name NO-ERROR.
    ELSE
      ASSIGN ghDataBuffer:BUFFER-FIELD("company":U):BUFFER-VALUE = "":U NO-ERROR.
  END. /* available memsch */
  
  /* Hospital group */
  FIND FIRST doctor NO-LOCK
    WHERE doctor.doc-num = ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE
  NO-ERROR.      
  IF AVAILABLE doctor THEN DO:
    IF AVAILABLE memsch THEN
      FIND FIRST docneggroup NO-LOCK
        WHERE (docneggroup.scheme-code = memsch.scheme-code
           OR docneggroup.scheme-code = 00)
          AND docneggroup.doc-num = ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE
          AND docneggroup.active-date < DATE(TODAY)
          AND (docneggroup.term-date = ?
           OR docneggroup.term-date > DATE(TODAY))
      NO-ERROR.  
    ELSE
      FIND FIRST docneggroup NO-LOCK
        WHERE docneggroup.scheme-code = 00
          AND docneggroup.doc-num = ghMemauth:BUFFER-FIELD("pr-num":U):BUFFER-VALUE
          AND docneggroup.active-date < DATE(TODAY)
          AND (docneggroup.term-date = ?
           OR docneggroup.term-date > DATE(TODAY))
      NO-ERROR.  
      
    IF AVAILABLE docneggroup THEN DO:
      FIND FIRST neggroup NO-LOCK
        WHERE neggroup.neg-num = docneggroup.neg-num
      NO-ERROR.
      IF AVAILABLE neggroup THEN                 
        ASSIGN ghDataBuffer:BUFFER-FIELD("neggroup":U):BUFFER-VALUE = neggroup.name NO-ERROR.
      ELSE
        ASSIGN ghDataBuffer:BUFFER-FIELD("neggroup":U):BUFFER-VALUE = "" NO-ERROR. 
    END. /* available docneggroup */
    ELSE
      ASSIGN ghDataBuffer:BUFFER-FIELD("neggroup":U):BUFFER-VALUE = "" NO-ERROR.
  END. /* available doctor */
  
  /* referring doctor */   
  FIND FIRST doctor NO-LOCK
    WHERE doctor.doc-num = ghMemauth:BUFFER-FIELD("ref-pr-num":U):BUFFER-VALUE 
  NO-ERROR.
  IF AVAILABLE doctor THEN
    ASSIGN ghDataBuffer:BUFFER-FIELD("ref_name":U):BUFFER-VALUE = REPLACE(doctor.name, ",", " ")
           ghDataBuffer:BUFFER-FIELD("ref_num":U):BUFFER-VALUE  = doctor.doc-num
           NO-ERROR.
  ELSE
    ASSIGN ghDataBuffer:BUFFER-FIELD("ref_name":U):BUFFER-VALUE = "":U
           ghDataBuffer:BUFFER-FIELD("ref_num":U):BUFFER-VALUE  = ghMemauth:BUFFER-FIELD("ref-pr-num":U):BUFFER-VALUE 
           NO-ERROR.
        
  /* requesting/treating doctor */         
  FIND FIRST doctor NO-LOCK
    WHERE doctor.doc-num = ghMemauth:BUFFER-FIELD("req-pr-num":U):BUFFER-VALUE
  NO-ERROR.
  IF AVAILABLE doctor THEN DO:
    ASSIGN ghDataBuffer:BUFFER-FIELD("req_name":U):BUFFER-VALUE = REPLACE(doctor.name, ",", " ")
           ghDataBuffer:BUFFER-FIELD("req_num":U):BUFFER-VALUE  = STRING(doctor.doc-num)
           NO-ERROR. 
           
    /* Specialist type */
    FIND FIRST docdisp NO-LOCK
        WHERE docdisp.doc-num = ghMemauth:BUFFER-FIELD("req-pr-num":U):BUFFER-VALUE
    NO-ERROR.
    IF AVAILABLE docdisp THEN DO:
      FIND FIRST prtype NO-LOCK 
        WHERE prtype.pr-type = docdisp.disp-code
      NO-ERROR.
      IF AVAILABLE prtype THEN
        ASSIGN ghDataBuffer:BUFFER-FIELD("reqprdesc":U):BUFFER-VALUE = REPLACE(prtype.description, ",", " ")
               NO-ERROR.
      ELSE
        ASSIGN ghDataBuffer:BUFFER-FIELD("reqprdesc":U):BUFFER-VALUE = "":U
               NO-ERROR.         
    END. /* available docdisp */      
  END. /* available doctor */
  ELSE 
    ASSIGN ghDataBuffer:BUFFER-FIELD("req_name":U):BUFFER-VALUE = "":U
           ghDataBuffer:BUFFER-FIELD("req_num":U):BUFFER-VALUE  = ghMemauth:BUFFER-FIELD("req-pr-num":U):BUFFER-VALUE
           NO-ERROR. 
  
  /* procedure code and description */
  FIND FIRST proccode NO-LOCK
    WHERE proccode.proc-code = ghMemauth:BUFFER-FIELD("prim-proc":U):BUFFER-VALUE
  NO-ERROR.
  IF AVAILABLE proccode THEN
    ASSIGN ghDataBuffer:BUFFER-FIELD("cpt_code":U):BUFFER-VALUE = proccode.proc-code
           ghDataBuffer:BUFFER-FIELD("cpt_desc":U):BUFFER-VALUE = REPLACE(proccode.description, ",", " ")
           NO-ERROR.
  ELSE  
    ASSIGN ghDataBuffer:BUFFER-FIELD("cpt_code":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("prim-proc":U):BUFFER-VALUE
           ghDataBuffer:BUFFER-FIELD("cpt_desc":U):BUFFER-VALUE = "":U
           NO-ERROR.          

  ASSIGN dAgeDate = ghDataBuffer:BUFFER-FIELD("auth_date":U):BUFFER-VALUE
         NO-ERROR.
  {ma/msc/maage.i
     &birthdate = memdep.birth-date
     &agedate   = dAgeDate}             
                 
  ASSIGN ghDataBuffer:BUFFER-FIELD("age":U):BUFFER-VALUE      = age - 1
         ghDataBuffer:BUFFER-FIELD("surname":U):BUFFER-VALUE  = TRIM(memdet.surname)  
         ghDataBuffer:BUFFER-FIELD("initial":U):BUFFER-VALUE  = TRIM(memdet.initials)
         ghDataBuffer:BUFFER-FIELD("gender":U):BUFFER-VALUE   = TRIM(memdep.sex)
         ghDataBuffer:BUFFER-FIELD("adm_type":U):BUFFER-VALUE = "":U
         ghDataBuffer:BUFFER-FIELD("paid_amt":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("amount-paid":U):BUFFER-VALUE
         ghDataBuffer:BUFFER-FIELD("req_by":U):BUFFER-VALUE   = ghMemauth:BUFFER-FIELD("user-id":U):BUFFER-VALUE
         NO-ERROR.  
       
  /* Start and End time */       
  ASSIGN    
      ghDataBuffer:BUFFER-FIELD("start_time":U):BUFFER-VALUE     = IF ghMemauth:BUFFER-FIELD("auth-start-time":U):BUFFER-VALUE THEN "AM" ELSE "PM"
      ghDataBuffer:BUFFER-FIELD("end_time":U):BUFFER-VALUE       = IF ghMemauth:BUFFER-FIELD("auth-end-time":U):BUFFER-VALUE THEN "AM" ELSE "PM"
      NO-ERROR.

  /* Length of Stay */       
  IF ghMemauth:BUFFER-FIELD("auth-end-date"):BUFFER-VALUE <> ? THEN DO:
    ASSIGN ghDataBuffer:BUFFER-FIELD("days":U):BUFFER-VALUE = ghMemauth:BUFFER-FIELD("auth-end-date"):BUFFER-VALUE - ghMemauth:BUFFER-FIELD("auth-start-date"):BUFFER-VALUE + 1.
    IF ghMemauth:BUFFER-FIELD("auth-start-time":U):BUFFER-VALUE = NO THEN
      ASSIGN ghDataBuffer:BUFFER-FIELD("days":U):BUFFER-VALUE = ghDataBuffer:BUFFER-FIELD("days":U):BUFFER-VALUE - 0.5.
    IF ghMemauth:BUFFER-FIELD("auth-end-time":U):BUFFER-VALUE   = YES THEN
      ASSIGN ghDataBuffer:BUFFER-FIELD("days":U):BUFFER-VALUE = ghDataBuffer:BUFFER-FIELD("days":U):BUFFER-VALUE - 0.5.
  END.
  ELSE
    ASSIGN ghDataBuffer:BUFFER-FIELD("days":U):BUFFER-VALUE = 0.

/* PMB */
  FIND FIRST conddiag NO-LOCK 
    WHERE conddiag.cond-type = "PMB" 
  NO-ERROR.
  IF AVAILABLE conddiag THEN DO:
    FIND FIRST bconddiag NO-LOCK
      WHERE bconddiag.icd-code    = ghMemauth:BUFFER-FIELD("prim-diag"):BUFFER-VALUE
        AND bconddiag.effect-date = conddiag.effect-date 
    NO-ERROR.
    IF AVAILABLE bconddiag THEN
      ASSIGN ghDataBuffer:BUFFER-FIELD("pmb":U):BUFFER-VALUE = "Y":U NO-ERROR.
    ELSE
      ASSIGN ghDataBuffer:BUFFER-FIELD("pmb":U):BUFFER-VALUE = "N":U NO-ERROR. 
  END. /* available conddiag */
     
  ASSIGN iCounter = 0.
      
  /* Primary and secondary diagnosis code and description */
  FOR EACH memauthic NO-LOCK                          
    WHERE memauthic.auth-num  = ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE
      AND memauthic.code-ind = "D":
       
    FIND FIRST diagnos NO-LOCK
      WHERE diagnos.diagnosis = memauthic.code
    NO-ERROR.
     
    ASSIGN iCounter = iCounter + 1.
    IF iCounter = 1 THEN 
      ASSIGN ghDataBuffer:BUFFER-FIELD("diag1_code":U):BUFFER-VALUE  = memauthic.code
             ghDataBuffer:BUFFER-FIELD("diag1_desc":U):BUFFER-VALUE  = REPLACE(diagnos.description, ",", " ") 
             NO-ERROR.      
    IF iCounter = 2 THEN
      ASSIGN ghDataBuffer:BUFFER-FIELD("diag2_code":U):BUFFER-VALUE  = memauthic.code
             ghDataBuffer:BUFFER-FIELD("diag2_desc":U):BUFFER-VALUE  = REPLACE(diagnos.description, ",", " ") 
             NO-ERROR.        
  END. /* for each memauthic */
    
  ASSIGN iCounter = 0.
    
  /* Secondary CPT4 code and description */
  FOR EACH memauthic NO-LOCK                          
    WHERE memauthic.auth-num  = ghMemauth:BUFFER-FIELD("auth-num":U):BUFFER-VALUE 
      AND memauthic.code-ind = "P": 
      
    FIND FIRST proccode NO-LOCK
      WHERE proccode.proc-code = memauthic.code
    NO-ERROR.
       
    ASSIGN iCounter = iCounter + 1.        
              
    IF iCounter = 1 THEN                            
      ASSIGN ghDataBuffer:BUFFER-FIELD("cpt1_code":U):BUFFER-VALUE = memauthic.code
             ghDataBuffer:BUFFER-FIELD("cpt1_desc":U):BUFFER-VALUE = if available proccode then REPLACE(proccode.description, ",", " ") else ""
             NO-ERROR.  
  END. /* for each memauthic */   
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
  DEFINE INPUT  PARAMETER lShowDetail       AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER lShowAuthDetail   AS LOGICAL    NO-UNDO.
  DEFINE OUTPUT PARAMETER phBodyTable       AS HANDLE     NO-UNDO.

  DEFINE VARIABLE iNumField           AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL    NO-UNDO.
  
  CREATE TEMP-TABLE phBodyTable NO-ERROR.
  { mip/inc/mipreturnerror.i }
                                        
  /* More Details */  
  IF lShowDetail THEN DO:
    ASSIGN
      lSuccess = phBodyTable:ADD-NEW-FIELD("sort_order":U,    "INTEGER":U,      0, "":U,               "":U, "?":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("mem_num":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Member":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("surname":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Surname":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("initial":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Initials":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("dependant":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Dependant":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("age":U,           "INTEGER":U,      0, "":U,               "":U, "Age":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("gender":U,        "CHARACTER":U,    0, "x(15)":U,          "":U, "Gender":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("scheme_name":U,   "CHARACTER":U,    0, "x(15)":U,          "":U, "Scheme Name":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("company":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Employer Name":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("ref_name":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "Referring Dr Name":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("ref_num":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Referring Dr No":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("req_name":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "Treating Dr Name":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("req_num":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Treating Dr No":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("reqprdesc":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Specialist Type":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("serv_name":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Hospital Name":U) 
      lSuccess = phBodyTable:ADD-NEW-FIELD("serv_num":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "Hospital PR No":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("neggroup":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "Hospital Group":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_num":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "Auth Num":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_type":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Auth Type":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_date":U,     "DATE":U,         0, "99/99/9999":U,     "":U, "Auth Date":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("start_date":U,    "DATE":U,         0, "99/99/9999":U,     "":U, "Start Date":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("start_time":U,    "CHARACTER":U,    0, "x(2)":U,           "":U, "AM/PM":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("end_date":U,      "DATE":U,         0, "99/99/9999":U,     "":U, "End Date":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("end_time":U,      "CHARACTER":U,    0, "x(2)":U,           "":U, "AM/PM":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("days":U,          "DECIMAL":U,      0, ">>9.<":U,          "":U, "Length Stay":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("diag_code":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Primary ICD10":U)      
      lSuccess = phBodyTable:ADD-NEW-FIELD("diag_desc":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "ICD10 Description":U) 
      lSuccess = phBodyTable:ADD-NEW-FIELD("diag1_code":U,    "CHARACTER":U,    0, "x(15)":U,          "":U, "Secondary ICD10":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("diag1_desc":U,    "CHARACTER":U,    0, "x(15)":U,          "":U, "Secondary ICD10 Desc":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("diag2_code":U,    "CHARACTER":U,    0, "x(15)":U,          "":U, "Tertiary ICD10":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("diag2_desc":U,    "CHARACTER":U,    0, "x(15)":U,          "":U, "Tertiary ICD10 Desc":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("cpt_code":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "Primary CPT 4":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("cpt_desc":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "CPT 4 Description":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("cpt1_code":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Secondary CPT 4":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("cpt1_desc":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Secondary CPT 4 Desc":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("adm_type":U,      "CHARACTER":U,    0, "x(15)":U,          "":U, "Admission Type":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("pmb":U,           "CHARACTER":U,    0, "x(15)":U,          "":U, "PMB":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_amt":U,      "DECIMAL":U,      0, "-ZZZ,ZZZ,ZZ9.99",  "":U, "Auth Amount":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("paid_amt":U,      "DECIMAL":U,      0, "-ZZZ,ZZZ,ZZ9.99",  "":U, "Paid Amount":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("req_by":U,        "CHARACTER":U,    0, "x(15)",            "":U, "Requested by":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_stat":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Status":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("stat_reason":U,   "CHARACTER":U,    0, "x(15)":U,          "":U, "Status Reason":U)
      NO-ERROR.
    { mip/inc/mipreturnerror.i }     
  END.
  ELSE DO:
    ASSIGN 
      lSuccess = phBodyTable:ADD-NEW-FIELD("sort_order":U,      "INTEGER":U,      0, "":U,               "":U, "?":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_num":U,        "CHARACTER":U,    0, "x(15)":U,          "":U, "Auth Num":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("mem_num":U,         "CHARACTER":U,    0, "x(15)":U,          "":U, "Member":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("dependant":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Dependant":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_date":U,       "DATE":U,         0, "99/99/9999":U,     "":U, "Auth Date":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("serv_prov":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Service Provider":U)  
      lSuccess = phBodyTable:ADD-NEW-FIELD("prim_diag":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Primary Diagnosis":U) 
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_type":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Auth Type":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("reference":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Reference":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_amt":U,        "DECIMAL":U,      0, "-ZZZ,ZZZ,ZZ9.99",  "":U, "Auth Amount":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("auth_stat":U,       "CHARACTER":U,    0, "x(15)":U,          "":U, "Status":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("stat_reason":U,     "CHARACTER":U,    0, "x(15)":U,          "":U, "Reason":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("start_date":U,      "DATE":U,         0, "99/99/9999":U,     "":U, "Start Date":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("end_date":U,        "DATE":U,         0, "99/99/9999":U,     "":U, "End Date":U)
      NO-ERROR.
    { mip/inc/mipreturnerror.i }
  END.
  ASSIGN  
    lSuccess = phBodyTable:ADD-NEW-FIELD("_Style_":U,       "CHARACTER":U,    0, "x(100)":U, "":U, "?":U)
    NO-ERROR.
  { mip/inc/mipreturnerror.i }

  IF lShowAuthDetail THEN DO:  
    DO iNumField = 1 to 15:
      ASSIGN
        lSuccess = phBodyTable:ADD-NEW-FIELD("tariff_code":U + STRING(iNumField,"99":U),      "CHARACTER":U,      0, "x(8)":U,               "":U, "Tariff Code ":U  + STRING(iNumField,"99":U))
        lSuccess = phBodyTable:ADD-NEW-FIELD("quantity":U + STRING(iNumField,"99":U),         "INTEGER":U,        0, "ZZZ,ZZ9":U,            "":U, "Quantity ":U  + STRING(iNumField,"99":U))
        lSuccess = phBodyTable:ADD-NEW-FIELD("claim_code":U + STRING(iNumField,"99":U),       "INTEGER":U,        0, "999":U,                "":U, "Claim Code ":U  + STRING(iNumField,"99":U))
        NO-ERROR.
      { mip/inc/mipreturnerror.i }
    END.
  END.

  ASSIGN
    lSuccess = phBodyTable:ADD-NEW-INDEX("ixSortOrder":U,  FALSE, TRUE)
    lSuccess = phBodyTable:ADD-INDEX-FIELD("ixSortOrder":U,"sort_order":U, "desc")
    NO-ERROR.
  { mip/inc/mipreturnerror.i }
  
  /* Add the high cost field */
  IF CAN-FIND(datalist WHERE datalist.list-type   = "MCACUST":U
                         AND datalist.list-code   = "DOHIGHCOST":U
                         AND datalist.description = "YES":U
                         AND datalist.link-from   = "":U)
  THEN DO:
    ASSIGN
      lSuccess = phBodyTable:ADD-NEW-FIELD("highcost_auth":U,   "CHARACTER":U,    0, "x(3)":U,           "":U, "High Cost Auth":U)
      NO-ERROR.
    { mip/inc/mipreturnerror.i }
  END.
  
  IF ssystem.admin-comp = 20
  THEN DO:
    ASSIGN
      lSuccess = phBodyTable:ADD-NEW-FIELD("rx_inpatient_status":U,  "CHARACTER":U, 0, "x(25)":U, "":U, "InPatient Status":U)
      lSuccess = phBodyTable:ADD-NEW-FIELD("rx_process_status":U,    "CHARACTER":U, 0, "x(35)":U, "":U, "RX Process Status":U)
    NO-ERROR.
    { mip/inc/mipreturnerror.i }
  END.
  
  phBodyTable:TEMP-TABLE-PREPARE("DataTable":U) NO-ERROR.
  { mip/inc/mipreturnerror.i }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLastExtRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastExtRef Procedure 
FUNCTION getLastExtRef RETURNS CHARACTER
  ( INPUT ipcRefCode AS CHARACTER,
    INPUT ipiAuthNum AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRefValue AS CHARACTER INITIAL "" NO-UNDO.
  FOR FIRST extref NO-LOCK
      WHERE extref.interface-type         = 'MIP'
        AND extref.owning-entity-mnemonic = 'memauth'
        AND extref.owning-key             = STRING(ipiAuthNum) 
        AND extref.reference-code         = ipcRefCode
        USE-INDEX extref-datetime
         BY extref.last-change-datetime DESCENDING:
      ASSIGN cRefValue = TRIM(ENTRY(1,extref.reference-value,"|")).
  END. 
  
  RETURN cRefValue.   
  /* Function return value. */
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

