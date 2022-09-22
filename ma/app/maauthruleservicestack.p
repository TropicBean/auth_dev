&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    $Id: maauthruleservicestack.p       Exp $ 

    Purpose: Stack procedure for Auth rule Class 

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */   
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ ma/inc/maauthruleds.i}

{mip/inc/miptemptables.i &TempTableName = ttDsCriteria      }
{mip/inc/miptemptables.i &TempTableName = ttDsQueryDetail   }

DEFINE TEMP-TABLE ttRuleCache
  FIELD dAuthRuleObj     AS DECIMAL
  FIELD dInsurerObj      AS DECIMAL
  FIELD iOptionCode      AS INTEGER
  FIELD cRuleType        AS CHARACTER
  FIELD cRuleCode        AS CHARACTER
  FIELD tEffectiveDate   AS DATE
  FIELD cRuleValue       AS CHARACTER
  FIELD cRuleValidValues AS CHARACTER
  FIELD dLinkAuthRuleObj AS DECIMAL
  FIELD cRuleDescription AS CHARACTER
  FIELD lSystemOwned     AS LOGICAL
  FIELD tEndDate         AS DATE.

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
         HEIGHT             = 22.62
         WIDTH              = 72.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-clearAuthRuleCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearAuthRuleCache Procedure 
PROCEDURE clearAuthRuleCache :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE ttRuleCache.

  {mip/inc/mipcatcherror.i}
END PROCEDURE. /* clearAuthRuleCache */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fetchAuthRuleDataset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchAuthRuleDataset Procedure 
PROCEDURE fetchAuthRuleDataset :
/*------------------------------------------------------------------------------
    Purpose   : Retrieve Auth Rule Data    
    Parameters: Parameter 1 - Filter temp table handle
                Parameter 2 - Dataset handle
                Parameter 3 - List of buffer names that should be filled or '*' for all 
    Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }            
    Author    : Kati
    
      NB- DEVELOPERS ******  ALL CHANGES BEFORE DELTA 185 SHOULD GO IN THE SUPER PROCEDURE
          AND ALL CHANGES AFTER DELTA 185 SHOULD BE IN THE STANDARD STACK ************
  ------------------------------------------------------------------------------*/   
  DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE   iophFilterCriteriaTableHandle.
  DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE iophDatasetHandle.
  DEFINE INPUT        PARAMETER ipcWhatToGet AS CHARACTER NO-UNDO. 
  
&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE cBatchSize                 AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cDatasetField              AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cSourceField               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cSourceMap                 AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cValue                     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cWhereClause               AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iBatchSize                 AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iLookup                    AS INTEGER              NO-UNDO.
  DEFINE VARIABLE lSuccess                   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE iCount                     AS INTEGER              NO-UNDO.
  DEFINE VARIABLE hFilterCriteriaTableHandle AS HANDLE               NO-UNDO. 
  DEFINE VARIABLE hDatasetHandle             AS HANDLE               NO-UNDO. 
  DEFINE VARIABLE hBuffer                    AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hField                     AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthRule                  AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthRuleError             AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hAuthRuleResult            AS HANDLE               NO-UNDO.
  DEFINE VARIABLE oSearchObject              AS cls.maauthrulesearch NO-UNDO.
  
  DEFINE QUERY qryAuthRule FOR hac_auth_rule.
  
  
  DEFINE DATA-SOURCE srcAuthRule FOR QUERY qryAuthRule.
  
  
  DEFINE BUFFER bufDSCriteria for ttDSCriteria.
  
  
  EMPTY TEMP-TABLE ttDSCriteria.
  
  
  ASSIGN 
     hFilterCriteriaTableHandle = iophFilterCriteriaTableHandle
     hDatasetHandle             = iophDatasetHandle
     
     oSearchObject              = NEW cls.maauthrulesearch(INPUT DATASET-HANDLE iophDatasetHandle)
     
     lSuccess                   = oSearchObject:SetCriteria(TABLE-HANDLE iophFilterCriteriaTableHandle).
     
  
  TEMP-TABLE ttDSCriteria:COPY-TEMP-TABLE(oSearchObject:CriteriaTableHandle).
     
  
  ASSIGN
     hAuthRule                 = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rule":U)
     hAuthRuleError            = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rule_error":U)
     hAuthRuleResult           = oSearchObject:DatasetHandle:GET-BUFFER-HANDLE("tt_auth_rule_result":U)
                                     
     lSuccess                  = hAuthRule:ATTACH-DATA-SOURCE(DATA-SOURCE srcAuthRule:HANDLE, oSearchObject:getMappedFieldList(BUFFER hac_auth_rule:HANDLE, hAuthRule))
     
     hAuthRuleError:FILL-MODE  = "NO-FILL":U 
     hAuthRuleResult:FILL-MODE = "NO-FILL":U.
      

  ASSIGN hBuffer = oSearchObject:DatasetHandle:GET-TOP-BUFFER().
  
      
  FOR EACH ttDSCriteria NO-LOCK
     WHERE ttDSCriteria.cCriteriaType = "Query-Filter":U
       AND ttDSCriteria.cBufferName = hBuffer:NAME
     BREAK BY ttDSCriteria.cBufferName 
           BY ttDsCriteria.cFieldName:
        
    ASSIGN
       cDatasetField = ttDsCriteria.cBufferName + ".":U + ttDsCriteria.cFieldName
       
       cSourceMap    = hBuffer:DATA-SOURCE-COMPLETE-MAP
       
       iLookup       = LOOKUP(cDatasetField, cSourceMap)
       
       cSourceField  = (IF iLookup = 0 THEN "":U ELSE ENTRY(iLookup + 1, cSourceMap))

       hField        = BUFFER ttDsCriteria:BUFFER-FIELD("v":U + ttDsCriteria.cDataType)
                     
       cValue        = hField:BUFFER-VALUE
       cValue        = QUOTER(IF cValue = ? THEN "?":U ELSE cValue).
                     
       cWhereClause  = cWhereClause
                     + (IF cWhereClause = "":U 
                        THEN "":U 
                        ELSE
                         (IF FIRST-OF(ttDsCriteria.cFieldName) 
                          THEN "~nAND ":U 
                          ELSE
                           (IF CAN-FIND(FIRST bufDsCriteria NO-LOCK
                                        WHERE bufDsCriteria.cCriteriaType = ttDsCriteria.cCriteriaType
                                          AND bufDsCriteria.cBufferName   = ttDsCriteria.cBufferName
                                          AND bufDsCriteria.cFieldName    = ttDsCriteria.cFieldName
                                          AND bufDsCriteria.cOperator     = "=":U) 
                             THEN "~nOR ":U 
                             ELSE "~nAND ":U)))
                             
                     + (IF FIRST-OF(ttDsCriteria.cFieldName) AND NOT LAST-OF(ttDsCriteria.cFieldName) 
                        THEN "(":U 
                        ELSE "":U)
                        
                     + cSourceField + " ":U + ttDsCriteria.cOperator + " ":U + cValue
                     
                     + (IF LAST-OF(ttDsCriteria.cFieldName) AND NOT FIRST-OF(ttDsCriteria.cFieldName) 
                        THEN ")":U 
                        ELSE "":U).

  END. /*FOR EACH ttDSCriteria NO-LOCK*/
  
  
  ASSIGN cWhereClause       = (IF cWhereClause = "":U THEN "":U ELSE " WHERE ":U + cWhereClause)
                            
         cWhereClause       = SUBSTITUTE("FOR EACH &1 NO-LOCK &2":U, hBuffer:DATA-SOURCE:GET-SOURCE-BUFFER:NAME , cWhereClause)
                            
         lSuccess           = oSearchObject:getCriteria("BatchSize":U, cBatchSize)
         cBatchSize         = (IF cBatchSize = ? 
                               THEN "50":U
                               ELSE cBatchSize)
                               
         iBatchSize         = INTEGER(cBatchSize)
         
         hBuffer:BATCH-SIZE = iBatchSize.
  
  QUERY qryAuthRule:QUERY-PREPARE(cWhereClause).

  {mip/inc/miplog.i "'mip_QueryInfo'" "'Query String: ' + QUERY qryAuthRule:PREPARE-STRING"}

  oSearchObject:DatasetHandle:FILL().
  
                                                         
  ASSIGN
     iophDatasetHandle             = oSearchObject:DatasetHandle
     iophFilterCriteriaTableHandle = oSearchObject:CriteriaTableHandle.
  
                                 
  {mip/inc/mipcatcherror.i
     &FORMAT  = TRUE
     &FINALLY = "hAuthRule:DETACH-DATA-SOURCE().        
                 
                 EMPTY TEMP-TABLE ttDSCriteria.        
     
                 IF VALID-HANDLE(hFilterCriteriaTableHandle)     THEN DELETE OBJECT hFilterCriteriaTableHandle.
                 IF VALID-HANDLE(hDatasetHandle)                 THEN DELETE OBJECT hDatasetHandle.
                 IF VALID-OBJECT(oSearchObject)                  THEN DELETE OBJECT oSearchObject.
                 IF VALID-HANDLE(iophFilterCriteriaTableHandle)  THEN DELETE OBJECT iophFilterCriteriaTableHandle.
                 IF VALID-HANDLE(iophDatasetHandle)              THEN DELETE OBJECT iophDatasetHandle. 
                 "}
&ENDIF
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAuthRuleDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAuthRuleDetails Procedure 
PROCEDURE getAuthRuleDetails :
/*------------------------------------------------------------------------------
  Purpose:     Get the auth rule value if the rule is set up.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER iopdAuthRuleObj     AS DECIMAL    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopdInsurerObj      AS DECIMAL    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopiOptionCode      AS INTEGER    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopcRuleType        AS CHARACTER  NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopcRuleCode        AS CHARACTER  NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ioptEffectiveDate   AS DATE       NO-UNDO.
  DEFINE       OUTPUT PARAMETER oplValidRule        AS LOGICAL    NO-UNDO.
  DEFINE       OUTPUT PARAMETER opcRuleValue        AS CHARACTER  NO-UNDO.
  DEFINE       OUTPUT PARAMETER opcRuleValidValues  AS CHARACTER  NO-UNDO.
  DEFINE       OUTPUT PARAMETER opdLinkAuthRuleObj  AS DECIMAL    NO-UNDO.
  DEFINE       OUTPUT PARAMETER opcRuleDescription  AS CHARACTER  NO-UNDO.
  DEFINE       OUTPUT PARAMETER oplSystemOwned      AS LOGICAL    NO-UNDO.
  DEFINE       OUTPUT PARAMETER optEndDate          AS DATE       NO-UNDO.

  DEFINE VARIABLE cWhereClause AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQueryString AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lSuccess     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hQuery       AS HANDLE     NO-UNDO.  
  
&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER hac_auth_rule FOR hac_auth_rule.

  ASSIGN
    iopdAuthRuleObj    = 0 WHEN iopdAuthRuleObj = ?
    iopdInsurerObj     = 0 WHEN iopdInsurerObj  = ?
    iopiOptionCode     = 0 WHEN iopiOptionCode  = ?

    iopcRuleType = (IF iopcRuleType = ? THEN "":U ELSE TRIM(iopcRuleType))
    iopcRuleCode = (IF iopcRuleCode = ? THEN "":U ELSE TRIM(iopcRuleCode)).

    /* Should we return an error if the effective date is ? */

  IF iopdAuthRuleObj = 0 AND (iopcRuleType = "":U OR iopcRuleCode = "":U OR ioptEffectiveDate = ?)
  THEN
    RETURN. /* We haven't been given the correct input parameters to get an auth rule */

  IF iopdAuthRuleObj > 0
  THEN DO: 
    FIND FIRST ttRuleCache
         WHERE ttRuleCache.dAuthRuleObj = iopdAuthRuleObj
      NO-ERROR.
    {mip/inc/mipthrowerror.i &IgnoreErrors='PROGRESS:565' &ResetIgnoredErrors=TRUE}

    IF NOT AVAILABLE ttRuleCache
    THEN DO:
      CREATE QUERY hQuery.
      hQuery:ADD-BUFFER(BUFFER hac_auth_rule:HANDLE).

      hQuery:QUERY-PREPARE( "FOR EACH hac_auth_rule NO-LOCK ":U
                          + "   WHERE hac_auth_rule.auth_rule_obj = ":U + STRING(iopdAuthRuleObj) ).
      hQuery:QUERY-OPEN().
      
      IF NOT hQuery:GET-FIRST()
      THEN
        RETURN.

      CREATE ttRuleCache.

      ASSIGN
        ttRuleCache.dAuthRuleObj     = iopdAuthRuleObj
        ttRuleCache.dInsurerObj      = hac_auth_rule.insurer_obj
        ttRuleCache.iOptionCode      = hac_auth_rule.option_code
        ttRuleCache.cRuleType        = hac_auth_rule.rule_type
        ttRuleCache.cRuleCode        = hac_auth_rule.rule_code
        ttRuleCache.tEffectiveDate   = hac_auth_rule.effective_date
        ttRuleCache.cRuleValue       = hac_auth_rule.rule_value
        ttRuleCache.cRuleValidValues = hac_auth_rule.rule_valid_values
        ttRuleCache.dLinkAuthRuleObj = hac_auth_rule.link_auth_rule_obj
        ttRuleCache.cRuleDescription = hac_auth_rule.rule_description
        ttRuleCache.lSystemOwned     = hac_auth_rule.system_owned
        ttRuleCache.tEndDate         = hac_auth_rule.end_date.

      hQuery:QUERY-CLOSE().
    END. /* IF NOT AVAILABLE ttRuleCache */
  END. /* IF iopdAuthRuleObj > 0 */
  ELSE DO: 
    FIND FIRST ttRuleCache
         WHERE ttRuleCache.dInsurerObj    = iopdInsurerObj   
           AND ttRuleCache.iOptionCode    = iopiOptionCode   
           AND ttRuleCache.cRuleType      = iopcRuleType     
           AND ttRuleCache.cRuleCode      = iopcRuleCode     
           AND ttRuleCache.tEffectiveDate = ioptEffectiveDate
      NO-ERROR.
    {mip/inc/mipthrowerror.i &IgnoreErrors='PROGRESS:565' &ResetIgnoredErrors=TRUE}

    IF NOT AVAILABLE ttRuleCache
    THEN DO:
      CREATE QUERY hQuery.
      hQuery:ADD-BUFFER(BUFFER hac_auth_rule:HANDLE).

      /* We need to check 4 cases, depending on wether the Option Code or Insurer Obj was supplied,
         and if a rule was set up for that specified option/insurer.
         If not, see if a fall-back rule was set up with option = 0, insurer = 0, or both = 0 instead.:
         Case 1: Option Code     + Insurer Obj
         Case 2: Option Code = 0 + Insurer Obj
         Case 3: Option Code     + Insurer Obj = 0
         Case 4: Option Code = 0 + Insurer Obj = 0 */
      
      ASSIGN
        cWhereClause = "FOR EACH  hac_auth_rule NO-LOCK ":U
                     + "   WHERE  hac_auth_rule.insurer_obj     = &1 ":U
                     + "     AND  hac_auth_rule.option_code     = &2 ":U
                     + "     AND  hac_auth_rule.rule_type       = ":U + QUOTER(REPLACE(iopcRuleType, "&":U, "&&":U))
                     + "     AND  hac_auth_rule.rule_code       = ":U + QUOTER(REPLACE(iopcRuleCode, "&":U, "&&":U))
                     + "     AND  hac_auth_rule.effective_date <= ":U + STRING(ioptEffectiveDate, "99/99/9999":U)
                     + "     AND (hac_auth_rule.end_date        = ? ":U
                     + "      OR  hac_auth_rule.end_date       >= ":U + STRING(ioptEffectiveDate, "99/99/9999":U) + ")":U
        
        /* Case 1: Option and Insurer supplied */
        cQueryString = SUBSTITUTE(cWhereClause, iopdInsurerObj, iopiOptionCode).
      
      hQuery:QUERY-PREPARE(cQueryString).
      hQuery:QUERY-OPEN().
      
      IF NOT hQuery:GET-FIRST()
      THEN DO:
        ASSIGN
          /* Case 2: Option = 0, Insurer supplied */
          cQueryString = SUBSTITUTE(cWhereClause, iopdInsurerObj, "0":U).
      
        hQuery:QUERY-PREPARE(cQueryString).
        hQuery:QUERY-OPEN().
      END. /* IF NOT Case 1 */
      
      
      IF NOT hQuery:GET-FIRST()
      THEN DO:
        ASSIGN
          /* Case 3: Option supplied, Insurer = 0 */
          cQueryString = SUBSTITUTE(cWhereClause, "0":U, iopiOptionCode).
      
        hQuery:QUERY-PREPARE(cQueryString).
        hQuery:QUERY-OPEN().
      END. /* IF NOT Case 2 */
      
      IF NOT hQuery:GET-FIRST()
      THEN DO:
        ASSIGN
          /* Case 4: Option and Insurer = 0 */
          cQueryString = SUBSTITUTE(cWhereClause, "0":U, "0":U).
      
        hQuery:QUERY-PREPARE(cQueryString).
        hQuery:QUERY-OPEN().
      END. /* IF NOT Case 3 */
      
      IF NOT hQuery:GET-FIRST()
      THEN
        RETURN.

      CREATE ttRuleCache.

      ASSIGN
        ttRuleCache.dAuthRuleObj     = hac_auth_rule.auth_rule_obj
        ttRuleCache.dInsurerObj      = hac_auth_rule.insurer_obj 
        ttRuleCache.iOptionCode      = hac_auth_rule.option_code 
        ttRuleCache.cRuleType        = hac_auth_rule.rule_type   
        ttRuleCache.cRuleCode        = hac_auth_rule.rule_code   
        ttRuleCache.tEffectiveDate   = hac_auth_rule.effective_date  
        ttRuleCache.cRuleValue       = hac_auth_rule.rule_value
        ttRuleCache.cRuleValidValues = hac_auth_rule.rule_valid_values
        ttRuleCache.dLinkAuthRuleObj = hac_auth_rule.link_auth_rule_obj
        ttRuleCache.cRuleDescription = hac_auth_rule.rule_description
        ttRuleCache.lSystemOwned     = hac_auth_rule.system_owned
        ttRuleCache.tEndDate         = hac_auth_rule.end_date.

      hQuery:QUERY-CLOSE().
    END. /* IF NOT AVAILABLE ttRuleCache */
  END. /* Else DO: */

  ASSIGN
    iopdAuthRuleObj    = ttRuleCache.dAuthRuleObj    
    iopdInsurerObj     = IF iopdInsurerObj <> 0
                         THEN iopdInsurerObj
			 ELSE ttRuleCache.dInsurerObj     
    iopiOptionCode     = IF iopiOptionCode <> 0
                         THEN iopiOptionCode
			 ELSE ttRuleCache.iOptionCode     
    iopcRuleType       = ttRuleCache.cRuleType       
    iopcRuleCode       = ttRuleCache.cRuleCode       
    ioptEffectiveDate  = ttRuleCache.tEffectiveDate  
    opcRuleValue       = ttRuleCache.cRuleValue      
    opcRuleValidValues = ttRuleCache.cRuleValidValues
    opdLinkAuthRuleObj = ttRuleCache.dLinkAuthRuleObj
    opcRuleDescription = ttRuleCache.cRuleDescription
    oplSystemOwned     = ttRuleCache.lSystemOwned    
    optEndDate         = ttRuleCache.tEndDate        

    oplValidRule       = TRUE.  

  {mip/inc/mipcatcherror.i
    &FINALLY="
      IF VALID-HANDLE(hQuery)
      THEN DO:
        IF hQuery:IS-OPEN
        THEN DO:
          hQuery:QUERY-CLOSE() NO-ERROR.
          ~{mip/inc/mipmessageerror.i~}
        END.

        DELETE OBJECT hQuery NO-ERROR.
        ~{mip/inc/mipmessageerror.i~}
      END.
    "
  }

&ENDIF

END PROCEDURE. /* getAuthRuleDetails */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveAuthRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAuthRule Procedure 
PROCEDURE saveAuthRule :
/*------------------------------------------------------------------------------
  Purpose   : Save Auth rules from dataset    
  Parameters: dsAuthRule as defined in maauthruleds.i
  Notes     : - Delta 010185
              - MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : Kati
  
    NB- DEVELOPERS ******  ALL CHANGES BEFORE DELTA 185 SHOULD GO IN THE SUPER PROCEDURE
          AND ALL CHANGES AFTER DELTA 185 SHOULD BE IN THE STANDARD STACK ************
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAuthRule.
  
&IF {&DBDFMA} >= 10195 &THEN                             
  
  RUN _saveAuthRule IN TARGET-PROCEDURE.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
                              
    FIND FIRST tt_auth_rule_result EXCLUSIVE-LOCK NO-ERROR.
      
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
    
    IF NOT AVAILABLE tt_auth_rule_result 
    THEN CREATE tt_auth_rule_result.
        
    FOR EACH tt_auth_rule_error NO-LOCK:    
      ASSIGN tt_auth_rule_result.number_of_errors = tt_auth_rule_result.number_of_errors + 1.
    END. /*FOR EACH tt_auth_rule_error NO-LOCK:    */
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
    
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_deleteAuthRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _deleteAuthRule Procedure 
PROCEDURE _deleteAuthRule :
/*------------------------------------------------------------------------------
  Purpose   : Remove Auth rule record    
  Parameters: Auth Rule obj
  Notes     : - This will run the procedures to remove any
                dependency records as well.
              - Delta 010185
              - MIP catch error handling used { mip/inc/mipcatcherror.i }               
  Author    : Kati
  
    NB- DEVELOPERS ******  ALL CHANGES BEFORE DELTA 185 SHOULD GO IN THE SUPER PROCEDURE
          AND ALL CHANGES AFTER DELTA 185 SHOULD BE IN THE STANDARD STACK ************
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipdAuthRuleObj AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE oErrorObject     AS cls.maerrorobject NO-UNDO. 
  DEFINE VARIABLE lFailureOccurred AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE cRuleList        AS CHARACTER         NO-UNDO.
    
&IF {&DBDFMA} >= 10195 &THEN                             
  
  DEFINE BUFFER btt_auth_rule_result  FOR tt_auth_rule_result.
  DEFINE BUFFER btt_auth_rule_error   FOR tt_auth_rule_error.
  DEFINE BUFFER buf_auth_rule         FOR hac_auth_rule.
  DEFINE BUFFER buf_link_auth_rule    FOR hac_auth_rule.
  
  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rule_error:HANDLE).
    
    FIND FIRST btt_auth_rule_result EXCLUSIVE-LOCK NO-ERROR.
      
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = TRUE }
    
    IF NOT AVAILABLE btt_auth_rule_result 
    THEN CREATE btt_auth_rule_result.
    
    FIND FIRST buf_auth_rule EXCLUSIVE-LOCK
         WHERE buf_auth_rule.auth_rule_obj = ipdAuthRuleObj
      NO-ERROR NO-WAIT.
    
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:445':U &ResetIgnoredErrors = FALSE }
      
    /*Unable to remove - record is already locked*/  
    IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
    DO:
      cls.miperror:resetError().
      
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacar":U, 
                            INPUT ipdAuthRuleObj,
                            INPUT "":U,
                            INPUT 1,
                            INPUT "MA":U,
                            INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                            INPUT "Auth rule":U).            
                       
    END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN*/  
        
    /*Unable to delete System Owned Record*/ 
    IF NOT mipEnv:miUser:DevelopmentUser 
    AND buf_auth_rule.system_owned = YES  THEN
    DO:
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacar":U, 
                            INPUT ipdAuthRuleObj,
                            INPUT "":U,
                            INPUT "system_owned":U,
                            INPUT 1,
                            INPUT "ma_MsgAuth":U,
                            INPUT 1,  /* System owned authorisation rule cannot be deleted */
                            INPUT "":U).            
                       
    END. /* IF NOT mipEnv:miUser:DevelopmentUser */  
    
    /*
      Unable to delete an auth rule if that rule exists as a linked rule on another auth rule
    */
    FOR EACH buf_link_auth_rule NO-LOCK 
       WHERE buf_link_auth_rule.link_auth_rule_obj = buf_auth_rule.auth_rule_obj:
      
      ASSIGN cRuleList  = cRuleList 
                        + (IF cRuleList <> "":U THEN "|":U ELSE "":U)
                        + buf_link_auth_rule.rule_code.
    END. /* FOR EACH buf_link_auth_rule */

    IF cRuleList <> "":U THEN DO:   
      ASSIGN lFailureOccurred = TRUE.
      
      oErrorObject:addError(INPUT "hacar":U, 
                            INPUT ipdAuthRuleObj,
                            INPUT "":U,
                            INPUT "auth_rule_obj":U,
                            INPUT 1,
                            INPUT "ma_MsgAuth":U,
                            INPUT 22,  /* Rule '&1' may not be removed as the following rules are linked to it: '&2' */
                            INPUT buf_auth_rule.rule_code + ",":U + cRuleList).            
         
    END. /*IF cRuleList <> "":U */    

    IF AVAILABLE buf_auth_rule AND NOT lFailureOccurred THEN    
    DO:  
      DELETE buf_auth_rule.
      
      ASSIGN btt_auth_rule_result.records_removed = btt_auth_rule_result.records_removed + 1.
      
    END. /*IF AVAILABLE buf_auth_rule AND NOT lFailureOccurred THEN    */
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
   
&ENDIF                            
  
  { mip/inc/mipcatcherror.i &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_saveAuthRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _saveAuthRule Procedure 
PROCEDURE _saveAuthRule :
/*------------------------------------------------------------------------------
  Purpose   : Create/update/delete Auth rule
  Parameters: 
  Notes     : MIP catch error handling used { mip/inc/mipcatcherror.i }            
  Author    : Kati
  
  NB- DEVELOPERS ******  ALL CHANGES BEFORE DELTA 185 SHOULD GO IN THE SUPER PROCEDURE
          AND ALL CHANGES AFTER DELTA 185 SHOULD BE IN THE STANDARD STACK ************
------------------------------------------------------------------------------*/
  
&IF {&DBDFMA} >= 10195 &THEN                               
  DEFINE VARIABLE lSuccess            AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE lFailureOccurred    AS LOGICAL           NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE oErrorObject        AS cls.maerrorobject NO-UNDO.
  DEFINE VARIABLE oAuthRuleType       AS cls.mipacronym    NO-UNDO.
  
  DEFINE BUFFER buf_auth_rule        FOR hac_auth_rule.
  DEFINE BUFFER btt_auth_rule_result FOR tt_auth_rule_result.
  DEFINE BUFFER btt_auth_rule_error  FOR tt_auth_rule_error.
  DEFINE BUFFER btt_auth_rule        FOR tt_auth_rule.


  DO TRANSACTION ON ERROR UNDO, THROW:
  
    ASSIGN oErrorObject  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_rule_error:HANDLE)
           oAuthRuleType = NEW cls.mipacronym(?, FALSE, "ma_acAuthRuleType", ?).
    
    FIND FIRST btt_auth_rule_result EXCLUSIVE-LOCK NO-ERROR.
    
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
    
    IF NOT AVAILABLE btt_auth_rule_result 
    THEN
      CREATE btt_auth_rule_result.
        
            
    RECORD-BLK:                             
    FOR EACH btt_auth_rule EXCLUSIVE-LOCK
          BY btt_auth_rule.auth_rule_obj DESCENDING:
    
      ASSIGN 
         lFailureOccurred = FALSE       
         btt_auth_rule_result.records_processed = btt_auth_rule_result.records_processed + 1.
      
      IF btt_auth_rule.record_action = "MODIFY":U THEN
      DO:           
        /*Ensure that Non-MIP Users cannot modify System Owned Rules*/
        IF NOT mipEnv:miUser:DevelopmentUser
        AND btt_auth_rule.system_owned = YES THEN 
        DO:
          ASSIGN lFailureOccurred = TRUE.
                                                                        
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj,
                                INPUT "":U,
                                INPUT "system_owned":U,
                                INPUT btt_auth_rule.line_number,
                                INPUT "ma_MsgAuth":U,
                                INPUT 2,  /* System owned authorisation rule cannot be modified */
                                INPUT "":U).            
          
        END. /* IF IF NOT mipEnv:miUser:DevelopmentUser AND btt_auth_rule.system_owned = YES THEN */
        
        /*Ensure that a valid auth rule code has been specified*/
        IF btt_auth_rule.rule_code = "":U THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
                                                                        
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT "rule_code":U,
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 13,  /* The &1 must be specified. */
                                INPUT "Auth Rule Code":U).                  
        END. /* btt_auth_rule.rule_code = "":U THEN */
        
        /*Ensure that a valid auth rule description has been specified*/
        IF btt_auth_rule.rule_description = "":U THEN 
        DO:
          ASSIGN lFailureOccurred = TRUE.
                                                                        
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT "rule_description":U,
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 13,  /* The &1 must be specified. */
                                INPUT "Auth Rule Description":U).                  
        END. /* btt_auth_rule.rule_description = "":U THEN */
        
        /*Ensure that a valid auth rule effective date has been specified*/
        IF btt_auth_rule.effective_date = ? THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
                                                                        
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT "effective_date":U,
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 13,  /* The &1 must be specified. */
                                INPUT "Auth Rule Effective Date":U) .                  
        END. /* btt_auth_rule.effective_date = ? THEN */
        
        /*If an end date is supplied, ensure that is not before the effective date */
        IF btt_auth_rule.end_date <> ? AND btt_auth_rule.end_date < btt_auth_rule.effective_date THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
                                                                        
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT "end_date":U,
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 11,  /* The End Date &1 cannot be before the Effective/Start Date &2 */
                                INPUT "(":U + STRING(btt_auth_rule.end_date,"9999/99/99") + "),(" + 
                                       STRING(btt_auth_rule.effective_date,"9999/99/99") + ")").                  
        END. /* btt_auth_rule.end_date < btt_auth_rule.effective_date THEN */        
        
        /*Ensure that a valid auth rule type has been specified*/
        oAuthRuleType:focusAcronym("key":U, btt_auth_rule.rule_type) NO-ERROR.
        
        IF cls.miperror:ErrorStatus OR NOT oAuthRuleType:AcronymInFocus THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN lFailureOccurred = TRUE.
        
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT "rule_type":U,
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 100,  /* The "&1" specified is invalid */
                                INPUT "Auth Rule Type: ":U + STRING(btt_auth_rule.rule_type)).                  
        END. /*IF cls.miperror:ErrorStatus OR NOT oAuthRuleType:AcronymInFocus THEN*/
        
        /*If an insurer has been specified, make sure it exists*/
        IF btt_auth_rule.insurer_obj <> 0.00 THEN
        DO:
          FIND FIRST erm_insurer NO-LOCK 
               WHERE erm_insurer.insurer_obj = btt_auth_rule.insurer_obj NO-ERROR.
            
          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
          
          IF NOT AVAILABLE erm_insurer THEN
          DO:
            ASSIGN lFailureOccurred = TRUE.
          
            oErrorObject:addError(INPUT "hacar":U, 
                                  INPUT btt_auth_rule.auth_rule_obj, 
                                  INPUT "":U, 
                                  INPUT "insurer_obj":U,
                                  INPUT btt_auth_rule.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 100,  /* The "&1" specified is invalid */
                                  INPUT "Auth Rule Client: ":U + STRING(btt_auth_rule.insurer_obj)).                  
          END.  /*IF NOT AVAILABLE erm_insurer THEN*/
        END. /*IF btt_auth_rule.insurer_obj <> 0.00 THEN*/
        
        /*If an scheme has been specified, make sure it exists*/
        IF btt_auth_rule.option_code <> 0 THEN
        DO:
          FIND FIRST scheme NO-LOCK 
               WHERE scheme.scheme-code = btt_auth_rule.option_code NO-ERROR.
            
          {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
          
          IF NOT AVAILABLE scheme THEN
          DO:
            ASSIGN lFailureOccurred = TRUE.
          
            oErrorObject:addError(INPUT "hacar":U, 
                                  INPUT btt_auth_rule.auth_rule_obj, 
                                  INPUT "":U, 
                                  INPUT "option_code":U,
                                  INPUT btt_auth_rule.line_number, 
                                  INPUT "MA":U, 
                                  INPUT 100,  /* The "&1" specified is invalid */
                                  INPUT "Auth Rule Scheme: ":U + STRING(btt_auth_rule.option_code)).                  
          END.  /*IF NOT AVAILABLE scheme THEN*/
        END. /*IF btt_auth_rule.option_code <> "" THEN*/

        /*Ensure that a valid auth rule value has been specified*/
        IF TRIM(btt_auth_rule.rule_value) = "":U THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.
                                                                        
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT "rule_value":U,
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 13,  /* The &1 must be specified. */
                                INPUT "Auth Rule Value":U).                  
        END. /* btt_auth_rule.rule_value = "":U THEN */

        /*Duplicate check*/ 
        IF CAN-FIND(FIRST buf_auth_rule NO-LOCK
                    WHERE buf_auth_rule.insurer_obj    = btt_auth_rule.insurer_obj
                      AND buf_auth_rule.option_code    = btt_auth_rule.option_code
                      AND buf_auth_rule.rule_type      = btt_auth_rule.rule_type
                      AND buf_auth_rule.rule_code      = btt_auth_rule.rule_code
                      AND buf_auth_rule.effective_date = btt_auth_rule.effective_date
                      AND buf_auth_rule.auth_rule_obj <> btt_auth_rule.auth_rule_obj) 
        THEN
        DO:
          ASSIGN lFailureOccurred = TRUE.

          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT "rule_code":U,
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 125,  /* &1 already exists with &2 */
                                INPUT "Auth Rule,:":U + 
                                      CHR(10) + "  Rule Code: " + btt_auth_rule.rule_code + 
                                      CHR(10) + "  Rule Type: " + oAuthRuleType:AcronymLabel + 
                                      CHR(10) + "  Eff.Date: "  + STRING(btt_auth_rule.effective_date,"9999/99/99")).
                         
        END. /*IF CAN-FIND(FIRST buf_auth_rule NO-LOCK*/                        
        
        IF lFailureOccurred
        THEN 
          NEXT RECORD-BLK.
        
        FIND FIRST buf_auth_rule EXCLUSIVE-LOCK
             WHERE buf_auth_rule.auth_rule_obj = btt_auth_rule.auth_rule_obj
          NO-ERROR NO-WAIT.
        
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565,PROGRESS:445':U &ResetIgnoredErrors = FALSE}
        
        /*The auth rule record is locked by another user or process*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:445":U  THEN
        DO:
          cls.miperror:resetError().
          
          ASSIGN 
             lFailureOccurred = TRUE
             
             btt_auth_rule_result.records_locked = btt_auth_rule_result.records_locked + 1.
                       
          oErrorObject:addError(INPUT "hacar":U, 
                                INPUT btt_auth_rule.auth_rule_obj, 
                                INPUT "":U, 
                                INPUT btt_auth_rule.line_number, 
                                INPUT "MA":U, 
                                INPUT 200,  /* Record "&1" is locked and cannot be read for updating, please try again. */
                                INPUT "Auth Rule: ":U + btt_auth_rule.rule_code).
                           
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
        
        /*Record not found so we are creating*/
        IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN
        DO:
          cls.miperror:resetError().
                                
          IF NOT lFailureOccurred THEN
            CREATE buf_auth_rule.
            
        END. /*IF cls.miperror:getMessageGroupNumber() = "PROGRESS:565":U  THEN*/
        
        IF AVAILABLE buf_auth_rule AND NOT lFailureOccurred THEN
        DO:        
          /*An existing record is being updated*/
          IF btt_auth_rule.auth_rule_obj <= 0 
          THEN ASSIGN btt_auth_rule_result.records_created  = btt_auth_rule_result.records_created  + 1.
          ELSE ASSIGN btt_auth_rule_result.records_modified = btt_auth_rule_result.records_modified + 1.
         
          BUFFER-COPY btt_auth_rule 
               EXCEPT btt_auth_rule.auth_rule_obj 
                   TO buf_auth_rule.        
          
          ASSIGN 
             btt_auth_rule.auth_rule_obj = buf_auth_rule.auth_rule_obj
             btt_auth_rule.record_action = "":U.
          
          VALIDATE buf_auth_rule.          
          
          FIND CURRENT buf_auth_rule NO-LOCK.

          cls.miperror:resetError().
        END. /*IF AVAILABLE buf_auth_rule AND NOT lFailureOccurred THEN*/
      END. /*IF btt_auth_rule.record_action = "MODIFY":U THEN*/
      
      IF btt_auth_rule.record_action = "DELETE":U THEN
      DO: 
        /*This routine will ensure that all dependencies will also be removed*/
        RUN _deleteAuthRule IN TARGET-PROCEDURE (INPUT btt_auth_rule.auth_rule_obj).
        
        IF NOT CAN-FIND(FIRST btt_auth_rule_error NO-LOCK
                        WHERE btt_auth_rule_error.owning_entity_mnemonic = 'hacar':U
                          AND btt_auth_rule_error.owning_obj = btt_auth_rule.auth_rule_obj)
        THEN DELETE btt_auth_rule.                                    
      END. /*END. /*IF btt_auth_rule.record_action = "DELETE":U THEN*/*/           
    END. /*FOR EACH btt_auth_rule EXCLUSIVE-LOCK:*/      
  END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
  
{ mip/inc/mipcatcherror.i
      &FINALLY="IF VALID-OBJECT(oErrorObject)  THEN DELETE OBJECT oErrorObject.
                IF VALID-OBJECT(oAuthRuleType) THEN DELETE OBJECT oAuthRuleType.
               " }    
&ENDIF  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

