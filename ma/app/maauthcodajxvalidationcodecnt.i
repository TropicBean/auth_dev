 /* DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE lSuccess             AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lAsterisk            AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lDagger              AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lEnableProcedureDate AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lDecline             AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE lMorphology          AS LOGICAL                  NO-UNDO.
  DEFINE VARIABLE cFilterFieldList     AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterField         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterValueList     AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cFilterValue         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnFieldList     AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnField         AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReturnValues        AS CHARACTER                NO-UNDO.  
  DEFINE VARIABLE cICDCode             AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cCPTCode             AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cProcedureDateAction AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReasonType          AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cReasonCode          AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cAsteriskCode        AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cMorphologyCode      AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE cStatus              AS CHARACTER                NO-UNDO.
  DEFINE VARIABLE iFilterField         AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iReturnField         AS INTEGER                  NO-UNDO.  
  DEFINE VARIABLE dAuthObj             AS INTEGER                  NO-UNDO.                                                                         
  DEFINE VARIABLE dInsurerObj          AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE iOptionCode          AS INTEGER                  NO-UNDO.
  DEFINE VARIABLE dStartDate           AS DATE                     NO-UNDO.
  DEFINE VARIABLE lMandatory           AS LOGICAL                  NO-UNDO.    
  DEFINE VARIABLE oAuthorisation       AS cls.maauthorisation      NO-UNDO.
                           
&IF {&DBDFMA} >= 10195 &THEN  
  
  &SCOPED-DEFINE AppendReturnValues ASSIGN cReturnValues = cReturnValues + (IF cReturnValues = "":U THEN "":U ELSE "|":U) +
  
  &SCOPED-DEFINE ValidationSuccess  ASSIGN ttValidation.cReturnValues = cReturnValues ~
                                           ttValidation.lValid        = TRUE          ~
                                           ttValidation.cMessage      = "Success":U.  ~
                                                                                      ~
                                    VALIDATE ttValidation.  
  
  &SCOPED-DEFINE BlankResponse      ASSIGN ttValidation.cReturnValues = FILL("|":U, NUM-ENTRIES(cReturnFieldList) - 1) ~
                                           ttValidation.lValid        = TRUE.                                          ~
                                                                                                                      ~
                                    VALIDATE ttValidation.
                                   
                                   
  ASSIGN
    cFilterFieldList = get-value('FldLst')
    cFilterValueList = get-value('ValList')
    cReturnFieldList = get-value('RetFldList').
  
  CREATE ttValidation.
  
  DO iFilterField = 1 TO NUM-ENTRIES(cFilterFieldList):
  
    ASSIGN cFilterField = ENTRY(iFilterField, cFilterFieldList)
           cFilterValue = ENTRY(iFilterField, cFilterValueList).
    
    CASE cFilterField:
    
      WHEN "[CPTCode]":U
      THEN ASSIGN cCPTCode = TRIM(cFilterValue).
      
      WHEN "[ICDCode]":U
      THEN ASSIGN cICDCode = TRIM(cFilterValue).            
      
      WHEN "[AsteriskCode]":U
      THEN ASSIGN cAsteriskCode = TRIM(cFilterValue).            
      
      WHEN "[MorphologyCode]":U
      THEN ASSIGN cMorphologyCode = TRIM(cFilterValue).            
      
      WHEN "[Status]":U
      THEN ASSIGN cStatus = cFilterValue.
                    
      WHEN "[ReasonCode]":U
      THEN ASSIGN cReasonCode = cFilterValue.
      
      WHEN "[ReasonType]":U 
      THEN ASSIGN cReasonType = cFilterValue.     

      WHEN "[StartDate]":U 
      THEN ASSIGN dStartDate = DATE(cFilterValue).

      WHEN "[AuthObj]":U
      THEN ASSIGN dAuthObj = DECIMAL(cFilterValue).
      
    END CASE.
  
  END. /*DO iFilter = 1 TO NUM-ENTRIES(cFilterFields):*/
  
  /*
    Strip asterisk/dagger symbols from codes
  */
  ASSIGN
     cICDCode      = REPLACE(cICDCode     , "+":U, "":U)
     cAsteriskCode = REPLACE(cAsteriskCode, "*":U, "":U).
  
  
  CASE ipcValidationArgument:
  
    WHEN "ICD":U THEN
    DO:
      IF cICDCode <> "":U THEN 
      DO:
        ASSIGN lAsterisk = CAN-FIND(FIRST hlm_code_link NO-LOCK WHERE hlm_code_link.child_alt_value = cICDCode AND hlm_code_link.acronym_key = 'ma_acCodeLinkCatAsterisk').
        
        FOR FIRST hlm_icd_industry NO-LOCK
            WHERE hlm_icd_industry.icd_code        = cICDCode
              AND hlm_icd_industry.effective_date <= dStartDate
          :
         
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[ICDCode]":U 
              THEN {&AppendReturnValues} cICDCode.

              WHEN "[ICDObj]":U 
              THEN {&AppendReturnValues} STRING(hlm_icd_industry.icd_industry_obj).
              
              WHEN "[ICDDescription]":U 
              THEN {&AppendReturnValues} hlm_icd_industry.icd_description.
              
              WHEN "[CheckDagger]":U THEN 
              DO:
                FIND FIRST diagnos NO-LOCK
                     WHERE diagnos.diagnosis = cIcdCode
                  NO-ERROR.

                { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
                
                IF AVAILABLE diagnos
                THEN                                                       
                  mipEnv:Health:maMedical:checkAsterisk(INPUT diagnos.diagnos-obj, OUTPUT lDagger).
                
                {&AppendReturnValues} STRING(lDagger).
                                
              END. /*WHEN "[CheckDagger]":U THEN */
              
              WHEN "[CheckMorphology]":U THEN 
              DO:                           
                mipEnv:Health:maMedical:checkMorphology(INPUT cIcdCode, OUTPUT lMorphology).
                                   
                {&AppendReturnValues} STRING(lMorphology).                
              END. /*WHEN "[CheckMorphology]":U THEN */
              
              WHEN "[CheckAsterisk]":U THEN
              DO:
                ASSIGN lDecline = (CAN-FIND(FIRST datalist 
                                            WHERE datalist.list-type = "ICD10MIT":U
                                              AND datalist.list-code = "ENABLEFOR":U
                                              AND datalist.link-from = "":U
                                              AND LOOKUP("MCA":U ,datalist.description) > 0) AND NOT 
                                   CAN-FIND(FIRST datalist 
                                            WHERE datalist.list-type = "ICD10MIT":U
                                              AND datalist.list-code = "MCAEXC":U 
                                              AND datalist.link-from = "":U
                                              AND LOOKUP(cIcdCode,datalist.description) > 0))
                                              
                       lDecline = lDecline AND CAN-FIND(LAST datalist 
                                                       WHERE datalist.list-type  = "ICD10MIT"
                                                         AND datalist.list-code  = "ASTERISK"
                                                         AND datalist.link-from <= STRING(TODAY,'99/99/9999')).                                                
                                                  
                {&AppendReturnValues} STRING(lAsterisk) + "^":U + STRING(lDecline).
              END. /*WHEN "[CheckAsterisk]":U THEN*/
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          IF lAsterisk AND lDecline THEN
          DO:
            ASSIGN ttValidation.cReturnValues = cReturnValues 
                   ttValidation.lValid        = FALSE          
                   ttValidation.cMessage      = "Asterisk may not be captured in the primary position":U.  
                                                              
            VALIDATE ttValidation. 
          END. /*IF lAsterisk THEN*/
          ELSE
            {&ValidationSuccess}    
            
        END. /*FOR FIRST hlm_icd_industry NO-LOCK:*/
        
        IF NOT CAN-FIND(FIRST hlm_icd_industry NO-LOCK
                        WHERE hlm_icd_industry.icd_code = cICDCode) THEN
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("ICD '&1' not found":U, cICDCode).
                 
          VALIDATE ttValidation.       
        END. /*ELSE*/
      END. /*IF cICDCode <> "":U THEN */
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END. /*WHEN "ICD":U THEN*/
    WHEN "CPT":U THEN
    DO:
      IF cCPTCode <> "":U THEN 
      DO:
        ASSIGN oAuthorisation = NEW cls.maauthorisation(dAuthObj)
        
               dStartDate     = (IF dStartDate = ? 
                                 THEN oAuthorisation:StartDate 
                                 ELSE dStartDate).

        /* determine if the procedure date must be included in the container */ 
        mipEnv:Health:Authbusinesslogic:activateProcedureDate(INPUT  oAuthorisation:InsurerObj,         
                                                              INPUT  oAuthorisation:MemberOptionCode,         
                                                              INPUT  dStartDate,               
                                                              INPUT  cCPTCode ,            
                                                              INPUT  "" ,        
                                                              OUTPUT lEnableProcedureDate,
                                                              OUTPUT cProcedureDateAction). 
        FOR FIRST hlm_cpt_link NO-LOCK
            WHERE hlm_cpt_link.cpt_code   = cCPTCode
          , FIRST hlm_cpt NO-LOCK
            WHERE hlm_cpt.cpt_code        = cCPTCode
              AND hlm_cpt.effective_date <= dStartDate :
         
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[CPTObj]":U 
              THEN {&AppendReturnValues} STRING(hlm_cpt_link.cpt_link_obj).
              
              WHEN "[CPTCode]":U 
              THEN {&AppendReturnValues} hlm_cpt_link.cpt_code.
              
              WHEN "[CPTDescription]":U 
              THEN {&AppendReturnValues} hlm_cpt_link.cpt_description.

              WHEN "[CPTAltObj]":U
              THEN {&AppendReturnValues} STRING(hlm_cpt.cpt_obj).
                
              WHEN "[ShowProcDate]":U
              THEN {&AppendReturnValues} IF lEnableProcedureDate THEN "SHOW" ELSE "HIDE".
    
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}    
        END. /*FOR FIRST hlm_cpt_link NO-LOCK:*/
        
        IF NOT CAN-FIND(FIRST hlm_cpt_link NO-LOCK
                        WHERE hlm_cpt_link.cpt_code = cCPTCode) THEN
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("CPT '&1' not found":U, cCPTCode).
                 
          VALIDATE ttValidation.       
        END. /*ELSE*/
      END. /*IF cCPTCode <> "":U THEN */
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END. /*WHEN "CPT":U THEN*/
    
    WHEN "Asterisk":U THEN
    DO:
      IF cAsteriskCode <> "":U AND cIcdCode <> "":U THEN 
      DO:
        
        FOR FIRST hlm_code_link NO-LOCK
            WHERE hlm_code_link.parent_alt_value = cIcdCode
              AND hlm_code_link.child_alt_value  = cAsteriskCode
              AND hlm_code_link.acronym_key      = "ma_acCodeLinkCatAsterisk":U,
            FIRST diagnos NO-LOCK
            WHERE diagnos.diagnosis = cAsteriskCode:
         
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[AsteriskObj]":U 
              THEN {&AppendReturnValues} STRING(diagnos.diagnos-obj).
              
              WHEN "[AsteriskCode]":U 
              THEN {&AppendReturnValues} cAsteriskCode.
              
              WHEN "[AsteriskDescription]":U 
              THEN {&AppendReturnValues} diagnos.description.
                            
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}    
        END. /*FOR FIRST hlm_code_link NO-LOCK:*/
        
        IF NOT CAN-FIND(FIRST hlm_code_link NO-LOCK
                        WHERE hlm_code_link.parent_alt_value = cIcdCode
                          AND hlm_code_link.child_alt_value  = cAsteriskCode
                          AND hlm_code_link.acronym_key      = "ma_acCodeLinkCatAsterisk":U) THEN
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("Asterisk '&1' not found for ICD10 '&2'":U, cAsteriskCode, cICDCode).
                 
          VALIDATE ttValidation.       
        END. /*ELSE*/
      END. /*IF cAsteriskCode <> "":U THEN */
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END. /*WHEN "Asterisk":U THEN*/
    
    WHEN "Morphology":U THEN
    DO:
      IF cMorphologyCode <> "":U AND cIcdCode <> "":U THEN 
      DO:
        
        FOR FIRST hlm_code_link NO-LOCK
            WHERE hlm_code_link.parent_alt_value = cIcdCode
              AND hlm_code_link.child_alt_value  = cMorphologyCode
              AND hlm_code_link.acronym_key      = "ma_acCodeLinkCatMorph":U,
            FIRST diagnos NO-LOCK
            WHERE diagnos.diagnosis = cMorphologyCode:
         
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[MorphologyObj]":U 
              THEN {&AppendReturnValues} STRING(diagnos.diagnos-obj).
              
              WHEN "[MorphologyCode]":U 
              THEN {&AppendReturnValues} cMorphologyCode.
              
              WHEN "[MorphologyDescription]":U 
              THEN {&AppendReturnValues} diagnos.description.
                            
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}    
        END. /*FOR FIRST hlm_code_link NO-LOCK:*/
        
        IF NOT CAN-FIND(FIRST hlm_code_link NO-LOCK
                        WHERE hlm_code_link.parent_alt_value = cIcdCode
                          AND hlm_code_link.child_alt_value  = cMorphologyCode
                          AND hlm_code_link.acronym_key      = "ma_acCodeLinkCatMorph":U) THEN
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("Morphology '&1' not found for ICD10 '&2'":U, cMorphologyCode, cICDCode).
                 
          VALIDATE ttValidation.       
        END. /*ELSE*/
      END. /*IF cMorphologyCode <> "":U THEN */
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END. /*WHEN "Morphology":U THEN*/
    
    WHEN "Status":U THEN
    DO:
      ASSIGN oAuthorisation = NEW cls.maauthorisation(dAuthObj)
      
             dStartDate     = (IF dStartDate = ? 
                               THEN oAuthorisation:StartDate 
                               ELSE dStartDate)             
                               
             dInsurerObj    = oAuthorisation:InsurerObj
                               
             iOptionCode    = oAuthorisation:MemberOptionCode.
             
             
      DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
      
        ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
      
        CASE cReturnField:
        
          WHEN "[StatusReasonMandatory]":U THEN
          DO:
            ASSIGN lMandatory = (IF TRIM(cStatus) = "":U 
                                 THEN FALSE
                                 ELSE mipEnv:Health:AuthService:statusReasonMandatory(INPUT INTEGER(cStatus), INPUT dInsurerObj, INPUT iOptionCode, INPUT dStartDate)).
        
            {&AppendReturnValues} STRING(lMandatory). 
          END. /*WHEN "[StatusReasonMandatory]":U THEN*/
        END CASE.
      END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   
      
      {&ValidationSuccess}       
    END. /*WHEN "Status":U THEN*/
    
    WHEN "Reason":U THEN
    DO:
      IF cReasonCode <> "":U THEN
      DO:        
        FIND FIRST note NO-LOCK
             WHERE note.type BEGINS TRIM(cReasonType,"*":U)
               AND note.key  = cReasonCode
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
        
        IF AVAILABLE note THEN 
        DO:
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[ReasonCode]":U THEN
              DO:
                {&AppendReturnValues} note.key. 
              END. /*WHEN "[ReasonCode]":U THEN*/
              
              WHEN "[ReasonDesc]":U THEN
              DO:
                {&AppendReturnValues} note.narration[1]. 
              END. /*WHEN "[ReasonDesc]":U THEN*/            
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   
          
          {&ValidationSuccess}     
        END. /*IF AVAILABLE notedet THEN */
        ELSE
        DO: 
          ASSIGN ttValidation.lValid   = FALSE                                                                            
                 ttValidation.cMessage = SUBSTITUTE("Reason code '&1' not found for '&2'":U, cReasonCode, cReasonType).
                
                 
          VALIDATE ttValidation.
        END. /*ELSE*/      
      END. /*IF cReasonCode <> "":U THEN*/
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END. /*WHEN "Reason":U THEN*/
                
  END CASE.
  
&ENDIF  
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "IF VALID-OBJECT(oAuthorisation) THEN DELETE OBJECT oAuthorisation."} */
