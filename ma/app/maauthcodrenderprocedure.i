/* maauthcodrenderprocedure.i  MEDSTAR Medical Aid System
                               Authorisation Coding Container Procedure ( renderProcedure )
                               (c) Copyright 1990 - 2020
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/

  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  
  DEFINE VARIABLE lActivateMainCode AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lAsterisk         AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lExtraDetails     AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lMandatory        AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lMorphology       AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidRule        AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE cAsteriskCode     AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cCondCode         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cCondTypeList     AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cConditionCode    AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOEM              AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOwningAltValue   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOwningKey        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRuleValue        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStartDate        AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE dAsteriskObj      AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthCodingObj    AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthObj          AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dCodingObj        AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dOwningObj        AS DECIMAL             NO-UNDO.  
  DEFINE VARIABLE dMorphologyObj    AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dInsurerObj       AS DECIMAL             NO-UNDO.       
  DEFINE VARIABLE dStartDate        AS DATE                NO-UNDO.
  DEFINE VARIABLE iOptionCode       AS INTEGER             NO-UNDO.
  DEFINE VARIABLE hBuffer           AS HANDLE              NO-UNDO.
  DEFINE VARIABLE hQuery            AS HANDLE              NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE BUFFER buf_cpt_link     FOR hlm_cpt_link.
  DEFINE BUFFER buf_cpt          FOR hlm_cpt.
  DEFINE BUFFER buf_icd_industry FOR hlm_icd_industry.
  DEFINE BUFFER buf_diagnos      FOR diagnos.

  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).
  
  CASE ipoControl:RenderArgument:
  
    WHEN "AuthCodingViewDetail":U THEN 
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer    = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)
          dCodingObj = hBuffer::auth_coding_obj
          cOEM       = hBuffer::owning_entity_mnemonic
          cOwningKey = hBuffer::owning_key
          dOwningObj = hBuffer::owning_obj
          dStartDate = hBuffer::start_date.
  
      IF cOEM <> "":U AND dCodingObj > 0.00  AND NOT ipoControl:ControlName BEGINS "fcLinkTemplate":U THEN 
      DO:
        mipEnv:miDBEntity:focusTable(cOEM).
  
        CASE cOEM:
 
          WHEN "diagnos":U THEN 
          DO:
            FIND LAST buf_icd_industry NO-LOCK
                WHERE buf_icd_industry.icd_code = cOwningKey
                  AND buf_icd_industry.effective_date <= dStartDate 
             NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

            IF AVAILABLE buf_icd_industry THEN
              ASSIGN ipoControl:Wob = 'mahlmii':U
                     ipoControl:Obj = STRING(buf_icd_industry.icd_industry_obj).
          END. /*WHEN "diagnos":U THEN */

          WHEN "hlmck":U THEN 
          DO:
            FIND FIRST buf_cpt_link NO-LOCK
                 WHERE buf_cpt_link.cpt_link_obj = dOwningObj 
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

            IF AVAILABLE buf_cpt_link THEN
              FIND FIRST buf_cpt NO-LOCK
                   WHERE buf_cpt.cpt_code = buf_cpt_link.cpt_code
                     AND buf_cpt.effective_date <= dStartDate 
                NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
            
            IF AVAILABLE buf_cpt THEN 
              ASSIGN ipoControl:Wob = 'mahlmcp':U
                     ipoControl:Obj = STRING(buf_cpt.cpt_obj). 
            
          END. /*WHEN "hlmck":U THEN */
        END CASE. /*CASE cOEM:*/

        IF ipoControl:Wob <> "":U THEN
          ASSIGN ipoControl:ControlValue  = ipoControl:Obj
                 ipoControl:ControlTarget = "popupwindow":U
                 ipoControl:AssignList    = "popupmode=true&wobMode=enquiry":U
                 ipoControl:LinkText      = "View Detail":U
                 .
                 
      END. /*IF cOEM <> "":U THEN*/
      ELSE
        ASSIGN ipoControl:Wob           = "[wob]":U
               ipoControl:Obj           = "[obj]":U         
               ipoControl:ControlValue  = "[obj]":U
               ipoControl:ControlTarget = "PopupWindow":U
               ipoControl:AssignList    = "popupMode=true&wobMode=enquiry[assignlist]":U
               ipoControl:LinkText      = "View Detail":U
               ipoControl:ControlClass  = "clHid":U
               .

     ipoControl:RenderAsHref().
    END. /*WHEN "AuthCodingViewDetail":U THEN*/
      
    /* If PMB's are applicable for the current line - display PMB detail href */
    WHEN "AuthCodingViewPMB":U THEN 
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer         = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)
          cOwningAltValue = hBuffer::owning_alt_value
          dCodingObj      = hBuffer::auth_coding_obj
          dOwningObj      = hBuffer::owning_obj
          dStartDate      = hBuffer::start_date
          hBuffer         = hQuery:GET-BUFFER-HANDLE("tt_auth":U)
          iOptionCode     = hBuffer::option_code
          dInsurerObj     = hBuffer::insurer_obj.

      IF dCodingObj <> ? AND dCodingObj > 0.00 THEN 
      DO:
        ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                            (INPUT  dInsurerObj,
                             INPUT  iOptionCode,
                             INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                             INPUT  "PMBViewDetails":U,
                             INPUT  dStartDate,
                             OUTPUT lValidRule,
                             OUTPUT cRuleValue).  
        /*
          Check if PMB's are applicable
        */
        mipEnv:Health:maMedical:getConditionCode
            (INPUT  "PMB":U,
             INPUT  cOwningAltValue,
             INPUT  "":U,
             INPUT  dStartDate,
             OUTPUT cConditionCode).

        ASSIGN ipoControl:ControlClass = (IF cConditionCode <> "":U AND LOOKUP(cRuleValue,"Active,Activate":U) > 0
                                          THEN "-clHid":U
                                          ELSE "+clHid":U).
      END. /*IF dCodingObj <> ? AND dCodingObj > 0.00 THEN*/
      ELSE
        ASSIGN ipoControl:ControlClass = "+clHid":U.

      ASSIGN ipoControl:JavascriptOnClick = "fnOnClickAuthCodingPMBDetails(this,~"fdAuthCodingObj~");return false;":U.

      ipoControl:RenderASHref().

    END. /*WHEN "AuthCodingViewPMB":U THEN*/

    WHEN "AuthCodingOwningAltValue":U THEN 
    DO:
      ASSIGN dAsteriskObj = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)::ass_diag_obj WHEN VALID-HANDLE(hQuery).

      IF dAsteriskObj > 0.00 AND ipoControl:ControlValue <> "":U THEN 
      DO:
        ASSIGN ipoControl:ControlValue = ipoControl:ControlValue + "+":U.
      END. /*IF dAsteriskObj > 0.00 AND ipoControl:ControlValue <> "":U THEN*/

      ipoControl:RenderASInput().
    END. /*WHEN "AuthCodingOwningAltValue":U THEN*/

    WHEN "AuthCodingAsterisk":U THEN 
    DO:
      ASSIGN lAsterisk = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)::_check_asterisk WHEN VALID-HANDLE(hQuery).

      IF lAsterisk THEN 
        ASSIGN ipoControl:ControlToken = "Updatable":U.
      ELSE 
        ASSIGN ipoControl:ControlToken = "Disabled":U.

      IF ipoControl:ControlType = 'wsLookupButton':U
      THEN ipoControl:RenderASLookupButton().
      ELSE ipoControl:RenderASInput().

    END. /*WHEN "AuthCodingAsterisk":U THEN*/

    WHEN "AuthCodingMorphology":U THEN 
    DO:

      ASSIGN lMorphology = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)::_check_morphology WHEN VALID-HANDLE(hQuery).

      IF lMorphology THEN 
        ASSIGN ipoControl:ControlToken = "Updatable":U.
      ELSE 
        ASSIGN ipoControl:ControlToken = "Disabled":U.

      IF ipoControl:ControlType = 'wsLookupButton':U
      THEN ipoControl:RenderASLookupButton().
      ELSE ipoControl:RenderASInput(). 
    END. /*WHEN "AuthCodingMorphology":U THEN*/

    WHEN "AuthCodingDaggerCode":U THEN 
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer      = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)
          lAsterisk    = hBuffer::_check_asterisk
          dAsteriskObj = hBuffer::ass_diag_obj.

      IF lAsterisk THEN 
        ASSIGN ipoControl:ControlToken = "Updatable":U
               ipoControl:ControlValue = IF dAsteriskObj <> 0.00 THEN "TRUE":U ELSE "FALSE":U.
      ELSE 
        ASSIGN ipoControl:ControlToken = "Disabled":U
               ipoControl:ControlValue = "FALSE":U.

      ipoControl:RenderASCheckBox().
    END. /*WHEN "AuthCodingDaggerCode":U THEN*/
    
    WHEN "AuthCodingCheckDaggerCode":U THEN 
    DO:
      ASSIGN lAsterisk               = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)::_check_asterisk WHEN VALID-HANDLE(hQuery)
             ipoControl:ControlValue = LOWER(STRING(lAsterisk)).

      ipoControl:RenderASInput().
    END. /*WHEN "AuthCodingCheckDaggerCode":U THEN*/

    WHEN "AuthCodingCheckMorphologyCode":U THEN 
    DO:
      ASSIGN lMorphology             = hQuery:GET-BUFFER-HANDLE("tt_auth_coding":U)::_check_morphology WHEN VALID-HANDLE(hQuery)
             ipoControl:ControlValue = LOWER(STRING(lMorphology)).

      ipoControl:RenderASInput().
    END. /*WHEN "AuthCodingCheckMorphologyCode":U THEN*/
    
    WHEN "DisplayExtraDetails":U THEN 
    DO:
      ASSIGN lExtraDetails = hQuery:GET-BUFFER-HANDLE("tt_depcond":U)::_display-extra-details WHEN VALID-HANDLE(hQuery).
      
      IF lExtraDetails THEN
        ASSIGN ipoControl:ControlToken = "Disabled":U.
      ELSE
        ASSIGN ipoControl:ControlToken = "Hidden":U
               ipoControl:ControlClass = "clHid":U.

      ipoControl:RenderAsInput().

    END. /* WHEN "DisplayExtraDetails":U  */

    WHEN "AuthFormat":U THEN
    DO:
      ASSIGN ipoControl:ControlFormat = "99999999":U.  
     
      ipoControl:RenderASInput().
       
    END. /* WHEN "AuthFormat":U */
      
    WHEN "DepFormat":U THEN
    DO:

      ASSIGN ipoControl:ControlFormat = "99":U.

      ipoControl:RenderASInput().
    END. /* WHEN "DepFormat":U */
     
    WHEN "DiagnosisLink":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer       = hQuery:GET-BUFFER-HANDLE("tt_depcond":U)
          cCondCode     = hBuffer::cond-code
          cStartDate    = hBuffer::start-date
          cCondTypeList = hBuffer::cond-type-list.

      ASSIGN
        ipoControl:Wob           = "maconddiag":U
        ipoControl:ControlTarget = "PopUpWindow":U
        ipoControl:AssignList    = "&condcode=":U     + cCondCode    
                                 + "&startdate=":U    + cStartDate   
                                 + "&condtypelist=":U + cCondTypeList .                                                                          

      ipoControl:RenderASHref().
    END. /* WHEN "DiagnosisLink":U */

    OTHERWISE RUN SUPER(ipoControl).
  END. /*CASE ipoControl:RenderArgument:*/    
  
  { mip/inc/mipcatcherror.i }
                                               
&ENDIF          
