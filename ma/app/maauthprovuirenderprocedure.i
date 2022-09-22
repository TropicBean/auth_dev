/*------------------------------------------------------------------------------
  maauthprovrenderprocedure.i  MEDSTAR Medical Aid System
                               Auth Provider UI Service Render Procedure include
                               (c) Copyright 2020 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved   

  Author    : MMP     
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  
  DEFINE VARIABLE dAuthObj                  AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj              AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj          AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAmountPaid               AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dQuantityPaid             AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dInsurerObj               AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dAuthRuleObj              AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE dLinkAuthRuleObj          AS DECIMAL          NO-UNDO.  
  DEFINE VARIABLE dStartDate                AS DATE             NO-UNDO.
  DEFINE VARIABLE cAdditional               AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cArsRate                  AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAttDocDesc               AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAttDiscipline            AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAttSubDiscipline         AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAuthProviderObj          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cAuthMode                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cBaseRate                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRateChangeType           AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cTitle                    AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cBaseRateList             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cClassName                AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cDocDesc                  AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cDiscipline               AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cSubDiscipline            AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cImageSrc                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cMemNum                   AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cClass                    AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cNegGroup                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cProviderList             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cProviderType             AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleValue                AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cStatus                   AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cStatusNote               AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cStatusNoteUpdAllow       AS CHARACTER        NO-UNDO. 
  DEFINE VARIABLE cAuthGroupList            AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cContainerCode            AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleCode                 AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleValidValues          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleDescription          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRuleType                 AS CHARACTER        NO-UNDO.  
  DEFINE VARIABLE cOverrideNoteCode         AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cPenaltyOverrideNoteCode  AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cNoteDescription          AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE cRateChangeTypeValue      AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE iCount                    AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iGroupDocNum              AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iDocNum                   AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iNegNum                   AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iSearchDoctor             AS INTEGER          NO-UNDO.
  DEFINE VARIABLE iOption                   AS INTEGER          NO-UNDO.
  DEFINE VARIABLE lActivateAuthorisedValues AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lGroupProvider            AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lStandardValuesAssigned   AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSuccess                  AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lMandatory                AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lMainProvider             AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lUpdatable                AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lValidRule                AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lSystemOwned              AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lCopayProvider            AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE cPenaltyFlag              AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE lActivateEmergencyFlag    AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lActivatePenaltyFlag      AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE lcConfiguration           AS LONGCHAR         NO-UNDO.
  DEFINE VARIABLE hBuffer                   AS HANDLE           NO-UNDO.
  DEFINE VARIABLE hQuery                    AS HANDLE           NO-UNDO.
  DEFINE VARIABLE dEndDate                  AS DATE             NO-UNDO.   
  
  DEFINE VARIABLE oAuthFlagSearch  AS cls.maauthflagvaluesearch NO-UNDO.


 
  &SCOPED-DEFINE StandardValues IF VALID-HANDLE(hQuery) AND VALID-HANDLE(hQuery:GET-BUFFER-HANDLE("tt_auth":U))             ~
                                AND hQuery:GET-BUFFER-HANDLE("tt_auth":U):AVAILABLE                                         ~
                                THEN                                                                                        ~
                                  ASSIGN                                                                                    ~
                                    hBuffer                   = hQuery:GET-BUFFER-HANDLE("tt_auth":U)                       ~
                                    cMemNum                   = hBuffer::mem_num                                            ~
                                    dAuthObj                  = hBuffer::auth_obj                                           ~
                                    dInsurerObj               = hBuffer::insurer_obj                                        ~
                                    dAuthTypeObj              = hBuffer::auth_type_obj                                      ~
                                    iOption                   = hBuffer::option_code                                        ~
                                    hBuffer                   = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)              ~
                                    dStartDate                = hBuffer::start_date                                         ~
                                    cProviderType             = hBuffer::provider_type                                      ~
                                    iGroupDocNum              = hBuffer::group_doc_num                                      ~
                                    iDocNum                   = hBuffer::doc_num                                            ~
                                    iNegNum                   =  INTEGER(TRIM(ENTRY(1, hBuffer::_neg_group, ":"), "("))     ~
                                    dAuthProviderObj          = hBuffer::auth_provider_obj                                  ~
                                    cDiscipline               = hBuffer::pr_type                                            ~
                                    cSubDiscipline            = hBuffer::sub_pr_type                                        ~
                                    lMainProvider             = hBuffer::main_provider                                      ~
                                    lCopayProvider            = hBuffer::copay_provider                                     ~
                                    cOverrideNoteCode         = hBuffer::copay_override_note                                ~
                                    cPenaltyOverrideNoteCode  = hBuffer::_penalty_override_note                             ~
                                    cPenaltyFlag              = hBuffer::_penalty_flag                                      ~
                                    lStandardValuesAssigned = TRUE.                                                         ~
                                ELSE                                                                                        ~
                                  ASSIGN                                                                                    ~
                                    lStandardValuesAssigned = FALSE.
  
  &SCOPED-DEFINE FindAuthTypeConfig   mipEnv:Health:AuthService:getAuthTypeConfig                               ~
                                        (INPUT dAuthTypeObj,                                                    ~
                                         INPUT dInsurerObj,                                                     ~
                                         INPUT iOption,                                                         ~
                                         INPUT dStartDate,                                                      ~
                                         INPUT cProviderType,                                                   ~
                                         INPUT iNegNum,                                                         ~
                                         INPUT cDiscipline,                                                     ~
                                         INPUT cSubDiscipline,                                                  ~
                                         INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).                     ~                                                                                                                                                                     
                                                                                                                ~
                                      FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.                             ~
                                                                                                                ~
                                      cls.miperror:resetError().                                                                
  
&IF {&DBDFMA} >= 010195 &THEN

  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).

  CASE ipoControl:RenderArgument:     
    
    WHEN "ProviderSequence":U THEN 
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer                       = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          dAuthProviderObj              = hBuffer::auth_provider_obj
          cRateChangeType               = hBuffer::rate_change_type
          ipoControl:JavascriptOnChange = SUBSTITUTE("fnOnChangeProviderSequence(this,~"&1~");":U, ipoControl:ParentContainer:ContainerCode)
          ipoControl:ControlClass       = ipoControl:ControlClass + (IF dAuthProviderObj <= 0 THEN " +clHid":U ELSE "":U)
          lSuccess                      = ipoControl:RenderAsInput()
          cClass                        = "clLineWarn":U
          cTitle                        = (IF cRateChangeType = "ma_acAuthRateChangeTypeAutomatic":U THEN "Automatic":U ELSE "Manual":U) + ' Rate Change has been applied on this provider'.
      
      /* 
        Render a Warning icon after sequence control if a rate change has been applied on this provider
      */
      IF  cRateChangeType = "ma_acAuthRateChangeTypeAutomatic":U 
      OR  cRateChangeType = "ma_acAuthRateChangeTypeManual":U  
      THEN
         {&OUT} { ma/inc/maiconbutton.i &ButtonSource  = "'/img/ratechangewarning.png'" 
                                        &ButtonOnClick = ipoControl:JavascriptOnClick
                                        &ButtonClass   = cClass
                                        &ButtonTitle   = cTitle
                                        &ButtonControl = ipoControl } .
    END. /*WHEN "Sequence":U THEN */

    WHEN "RateChangeTypeDisplay":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer              = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          cRateChangeTypeValue = hBuffer::rate_change_type.
             
        IF cRateChangeTypeValue <> "" THEN
          ASSIGN ipoControl:ControlValue = REPLACE(cRateChangeTypeValue, "ma_acAuthRateChangeType", "").
        ELSE
          ipoControl:ControlValue = "".
      
      ipoControl:RenderASInput(). 
    END. /* WHEN "RateChangeTypeDisplay":U THEN */
    
    WHEN "AuthProviderEditButton":U THEN 
    DO:
      ASSIGN 
         dAuthProviderObj             = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::auth_provider_obj WHEN VALID-HANDLE(hQuery)
         
         cImageSrc                    = WarpSpeed:ThemePath + "img/modify.png":U
         
         cClassName                   = (IF dAuthProviderObj <= 0.00 THEN "clHid":U ELSE "":U)
         
         cAuthProviderObj             = (IF dAuthProviderObj <= 0.00 THEN "[obj]":U ELSE STRING(dAuthProviderObj))

         ipoControl:ControlJavascript = " onmouseover=~"$(this).fadeTo('fast', 0.7);~" onmouseout=~"$(this).fadeTo('fast', 1);~"":U
         
         ipoControl:JavascriptOnClick = "event.stopPropagation();":U
                                      + {ws/inc/wshref.i &wob        = "'mahatap'":U
                                                         &obj        = cAuthProviderObj
                                                         &linktext   = "'BTN'"                
                                                         &linkargs   = "'popupMode=true'+ (IF get-value('wobMode') = 'Enquiry' THEN '&wobmode=Enquiry' ELSE '')"
                                                         &hreftarget = "'popupwindow'"
                                                         &hreftype   = 'javascript'}.
      
      {&OUT}
        "<a href='#' class = '" + cClassName + "' onclick='event.preventDefault(); $(this).find(~"img~").click();'>"
        "<img name = '":U + ipoControl:InstanceName + "' src='":U + cImageSrc + "' onclick = '":U + ipoControl:JavascriptOnClick + "' ":U + ipoControl:ControlJavascript + " class='" + cClassName + "' title='":U + mipEnv:miUtility:encode-url(ipoControl:ControlTooltip, "html":U) + "'" + (IF ipoControl:ControlTabOrder = 0 THEN "":U ELSE " tabindex='" + STRING(ipoControl:ControlTabOrder) + "' ") + " ></img>"
        "</a>":U
        .
    END. /*WHEN "AuthProviderEditButton":U THEN */
    
    WHEN "AuthProviderNum":U     OR WHEN "AuthProviderDesc":U    OR WHEN "AuthProviderDiscipline":U    OR WHEN "AuthProviderSubDiscipline":U  OR 
    WHEN "AuthAttProviderNum":U  OR WHEN "AuthAttProviderDesc":U OR WHEN "AuthAttProviderDiscipline":U OR WHEN "AuthAttProviderSubDiscipline":U
    THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN 
          hBuffer           = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          iGroupDocNum      = hBuffer::group_doc_num
          iDocNum           = hBuffer::doc_num
          cAttDiscipline    = hBuffer::pr_type
          cAttSubDiscipline = hBuffer::sub_pr_type
          lGroupProvider    = hBuffer::_group_provider

          hBuffer           = hQuery:GET-BUFFER-HANDLE("group_discipline":U)
          cDiscipline       = hBuffer::disp-code
          cSubDiscipline    = hBuffer::subdisp-code

          cDocDesc          = hQuery:GET-BUFFER-HANDLE("group_doctor":U)::name

          cAttDocDesc       = hQuery:GET-BUFFER-HANDLE("attending_doctor":U)::name.
         
      { ma/msc/madispad.i &discipline = cDiscipline                        }
      { ma/msc/madispad.i &discipline = cSubDiscipline    &comment = "/* " }
      { ma/msc/madispad.i &discipline = cAttDiscipline    &comment = "/* " }
      { ma/msc/madispad.i &discipline = cAttSubDiscipline &comment = "/* " }
             
                         
      CASE ipoControl:RenderArgument:
      
        WHEN "AuthProviderNum":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN STRING(iGroupDocNum) 
                                            ELSE STRING(iDocNum))
          
                 ipoControl:ControlValue = (IF ipoControl:ControlValue = "0":U OR (INTEGER(ipoControl:ControlValue) < 0) 
                                            THEN "":U 
                                            ELSE ipoControl:ControlValue).
        
        WHEN "AuthProviderDesc":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF iDocNum = 0 
                                            THEN "":U 
                                            ELSE (IF lGroupProvider THEN cDocDesc ELSE cAttDocDesc)).
        
        WHEN "AuthProviderDiscipline":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN cDiscipline 
                                            ELSE cAttDiscipline)
                                            
                 ipoControl:ControlValue = (IF ipoControl:ControlValue = "000":U 
                                            THEN "":U 
                                            ELSE ipoControl:ControlValue).
          
        WHEN "AuthProviderSubDiscipline":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN cSubDiscipline 
                                            ELSE cAttSubDiscipline).
          
        WHEN " ":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN STRING(iDocNum) 
                                            ELSE "":U)
          
                 ipoControl:ControlValue = (IF ipoControl:ControlValue = "0":U 
                                            THEN "":U 
                                            ELSE ipoControl:ControlValue)
                 
                 ipoControl:ControlClass = (IF lGroupProvider 
                                            THEN "+clMan":U 
                                            ELSE "-clMan":U).
          
        WHEN "AuthAttProviderDesc":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF iDocNum = 0 
                                            THEN "":U 
                                            ELSE (IF lGroupProvider THEN cAttDocDesc ELSE "":U)).
        
        WHEN "AuthAttProviderDiscipline":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN cAttDiscipline 
                                            ELSE "000":U)
                                            
                 ipoControl:ControlClass = (IF lGroupProvider AND iGroupDocNum = 0
                                            THEN "+clMan":U 
                                            ELSE "-clMan":U).
        
        WHEN "AuthAttProviderSubDiscipline":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN cAttSubDiscipline 
                                            ELSE "000":U)
                                            
                 ipoControl:ControlClass = (IF lGroupProvider AND iGroupDocNum = 0
                                            THEN "+clMan":U 
                                            ELSE "-clMan":U).
                
        WHEN "AuthAttProviderNum":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN STRING(iDocNum) 
                                            ELSE "":U)
                                            
                 ipoControl:ControlClass = (IF lGroupProvider 
                                            THEN "+clMan":U 
                                            ELSE "-clMan":U).
             
      END CASE.
      
      CASE ipoControl:ControlType:
      
        WHEN "wsTextArea":U 
        THEN ipoControl:RenderASTextArea().
        
        WHEN "wsInput":U 
        THEN ipoControl:RenderASInput().
        
        WHEN "wsCombo":U 
        THEN ipoControl:RenderASComboORSelect().
        
      END CASE.  
    END. /*WHEN "AuthProviderNum":U OR WHEN "AuthProviderDesc":U OR WHEN "AuthProviderDiscipline":U OR WHEN "AuthProviderSubDiscipline":U  OR ....*/
    
    WHEN "GroupProvider":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer        = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          iGroupDocNum   = hBuffer::group_doc_num
          iDocNum        = hBuffer::doc_num
          lGroupProvider = hBuffer::_group_provider
          lGroupProvider = IF iGroupDocNum = iDocNum THEN NO ELSE lGroupProvider.
             
      ASSIGN ipoControl:ControlValue = STRING(lGroupProvider).
      
      ipoControl:RenderASInput(). 
                                         
    END. /*WHEN "GroupProvider":U THEN*/
    
    /* For tables this logic has been moved to a RowRenderProcedure for performance reasons */
    /* but we need to hang on to this RenderProcedure block for the provider form           */
    WHEN "AuthProviderStatusNote":U THEN 
    DO:
      /*
        Check if this field should be mandatory or not
      */
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer             = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          cStatus             = hBuffer::auth_status
          cStatusNote         = hBuffer::auth_status_note
          cStatusNoteUpdAllow = hBuffer::_status_note_upd_allow
          lMandatory          = hBuffer::_status_note_mandatory.
             
      
      ASSIGN ipoControl:ControlClass = ipoControl:ControlClass 
                                     + (IF lMandatory THEN " +":U ELSE " -":U)
                                     + "clMan":U.
      
      /*
        Set note description as tooltip
      */                             
      IF cStatusNote <> "":U THEN
      DO:
        ASSIGN ipoControl:ControlToolTip = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_note_narration WHEN VALID-HANDLE(hQuery).
      END. /*IF ipoControl:ControlValue <> "":U THEN*/
      
      /*
        Status Update Allowed
      */
      ASSIGN ipoControl:ControlToken = cStatusNoteUpdAllow.
      
      /*
        Render control
      */
      IF ipoControl:ControlType = 'wsLookupButton'
      THEN
        ipoControl:RenderASLookupButton().
      ELSE
        ipoControl:RenderASInput().
    END. /*WHEN "AuthProviderStatusNote":U THEN */
        
    /* For tables this logic has been moved to a RowRenderProcedure for performance reasons */
    /* but we need to hang on to this RenderProcedure block for the provider form           */
    WHEN "AuthProviderClaimCode":U THEN
    DO:
      /*
        Check field _claim_code_updateable assigned in maauthbusinesslogicstack.p to determine 
        whether claim code is updatable or not
      */
      ASSIGN ipoControl:ControlToken = "Updatable":U . // (IF VALID-HANDLE(hQuery) THEN hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_claim_code_updateable ELSE "":U).
      
      IF ipoControl:ControlType = 'wsLookupButton'
      THEN
        ipoControl:RenderASLookupButton().
      ELSE
        ipoControl:RenderASInput().
    END. /*WHEN "AuthProviderClaimCode":U THEN*/
    
    /* For tables this logic has been moved to a RowRenderProcedure for performance reasons */
    /* but we need to hang on to this RenderProcedure block for the provider form           */
    WHEN "AuthProviderClaimType":U THEN
    DO:
      /*
        Check field _claim_type_updateable assigned in maauthbusinesslogicstack.p to determine 
        whether claim code is updatable or not
      */
      ASSIGN ipoControl:ControlToken = "Updatable":U.  //(IF VALID-HANDLE(hQuery) THEN hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_claim_type_updateable ELSE "":U).
      
      ipoControl:RenderASComboORSelect().
    END. /*WHEN "AuthProviderClaimType":U THEN*/

    WHEN "AuthProviderPayeeIndicator":U THEN 
    DO:
      ASSIGN ipoControl:ControlValue = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::payee_dm WHEN VALID-HANDLE(hQuery).
      
      ipoControl:RenderASComboORSelect().                                               
    END. /*WHEN "AuthProviderPayeeIndicator":U THEN */
    
    WHEN "AuthProviderAuthorisedValue":U THEN
    DO:
      ASSIGN ipoControl:ControlToken = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_authorised_value_updateable WHEN VALID-HANDLE(hQuery).

      ipoControl:RenderASInput().         
            
    END. /*WHEN "AuthProviderAuthorisedValue":U THEN*/    

    WHEN "AuthProviderAuthorisedService":U THEN
    DO:
      {&StandardValues}
      IF lStandardValuesAssigned THEN
      DO:
        EMPTY TEMP-TABLE ttAuthTypeConfig.
        IF cProviderType <> "":U THEN
        DO:
          {&FindAuthTypeConfig}
          FIND FIRST ttAuthTypeConfig NO-LOCK 
            WHERE ttAuthTypeConfig.ProviderType = cProviderType
            NO-ERROR.  
        END.  /* IF cProviderType <> "":U THEN */
      END.  /* IF lStandardValuesAssigned THEN */

      ASSIGN ipoControl:ControlToken = IF AVAILABLE ttAuthTypeConfig 
                                       THEN "Disabled":U
                                       ELSE "Updatable":U. 
      ipoControl:renderAsCheckBox().

    END.  /* WHEN "AuthProviderAuthorisedService":U THEN */

    /* For tables this logic has been moved to a RowRenderProcedure for performance reasons */
    /* but we need to hang on to this RenderProcedure block for the provider form           */
    WHEN "AuthProviderReasonType":U THEN 
    DO:
      ASSIGN ipoControl:ControlValue = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_reason_type WHEN VALID-HANDLE(hQuery).
              
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthProviderReasonType":U */
    
    WHEN "AuthProviderConfiguration":U THEN 
    DO:
      ASSIGN ipoControl:ControlValue = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_provider_configuration WHEN VALID-HANDLE(hQuery).
                                                            
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthProviderConfiguration":U */

    OTHERWISE RUN SUPER(ipoControl).
  END. /*CASE ipoControl:RenderArgument:      */
  
  CASE ENTRY(1, ipoControl:RenderArgument, "|":U):
  
    WHEN "ProviderNegGroup":U THEN 
    DO:
      IF NUM-ENTRIES(ipoControl:RenderArgument, "|":U) > 1 THEN 
      DO:
        IF ENTRY(2,ipoControl:RenderArgument, "|":U) = "field":U 
        THEN 
          ASSIGN ipoControl:ControlValue = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_neg_group WHEN VALID-HANDLE(hQuery).
        ELSE        
          ASSIGN ipoControl:ControlToolTip = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_neg_group_tooltip WHEN VALID-HANDLE(hQuery).
      END. /*IF NUM-ENTRIES(ipoControl:RenderArgument, "|":U) > 1*/
      
      ipoControl:RenderAsInput().
    END. /* WHEN "ProviderNegGroup":U */
    WHEN "BaseRateProvider":U THEN
    DO:
      ASSIGN cBaseRate  = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::default_base_rate WHEN VALID-HANDLE(hQuery).
      
      /*
        If the  default base rate on line level is blank, GET THE PROVIDER DEFAULTS
      */
      IF cBaseRate = "":U THEN
       ASSIGN cBaseRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_base_rate WHEN VALID-HANDLE(hQuery).
       
      ASSIGN ipoControl:ControlValue = cBaseRate.
      
      ipoControl:RenderASInput(). 
    
    END. /* WHEN "BaseRateProvider" */
    
    WHEN "ArsRateProvider":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN 
          hBuffer   = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          cBaseRate = hBuffer::default_base_rate
          cArsRate  = hBuffer::default_ars_rate.
      
      /*
        If the  default ars rate on line level is blank, GET THE PROVIDER DEFAULTS
      */
      IF  cArsRate  = "":U
      AND cBaseRate = "":U THEN
      DO:
        ASSIGN cArsRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::_ars_rate WHEN VALID-HANDLE(hQuery).
      END. /* IF  cArsRate  = "":U AND cBaseRate = "":U  */
        
      ASSIGN ipoControl:ControlValue = cArsRate.
      
      ipoControl:RenderASInput(). 
    
    END. /* WHEN "ArsRateProvider" */

    WHEN "AuthGroupCode":U THEN
    DO:
      ASSIGN 
        cAuthGroupList = hQuery:GET-BUFFER-HANDLE("tt_auth":U)::_auth_group_list WHEN VALID-HANDLE(hQuery)
          
        ipoControl:AdditionalItems = cAuthGroupList.

      ipoControl:RenderAsComboOrSelect().  
          
    END. /* WHEN "AuthGroupCode":U THEN */
    
    WHEN "AuthAttProviderNumLkp":U THEN
    DO:
      {&StandardValues}
      
      IF lStandardValuesAssigned THEN
      DO:
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                       INPUT  iOption,
                                                       INPUT  "ma_acAuthRuleTypeAuthProvider":U,
                                                       INPUT  "ProviderGroupValidate":U,
                                                       INPUT  dStartDate,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).
                                                       
        ASSIGN
          cContainerCode = ipoControl:ParentContainer:ContainerCode.                                             
                                                        
        IF lValidRule AND cRuleValue = "[Discipline]":U 
        THEN
          ASSIGN ipoControl:LookupWobFLA  = "prassoc2":U.
        ELSE
          ASSIGN ipoControl:LookupWobFLA  = "prassoc":U.
        
        ASSIGN  
          ipoControl:LookupFields     = "doctor.doc-num":U                                                                                                                        
          ipoControl:LookupControls   = "fiAttNum":U + cContainerCode                                                                                                            
          ipoControl:FilterFields     = "doctor.doc-num,prassoc.pr-assoc":U                                                                                                                    
          ipoControl:FilterControls   = "fiAttNum":U + cContainerCode + ",fiProvNum":U + cContainerCode                                                                        
          ipoControl:ReturnFields     = "doctor.doc-num":U                                                                                                                         
          ipoControl:ReturnControls   = "fiAttNum":U + cContainerCode.  
          
      END. /* IF lStandardValuesAssigned THEN */ 
      
      ipoControl:RenderASLookupButton().
      
    END. /* WHEN "AuthAttProviderNumLkp":U THEN */

    WHEN "EmergencyFlagValue":U OR
    WHEN "MainProvider"   THEN
    DO:
      {&StandardValues}

      ASSIGN cRuleType = "ma_acAuthRuleTypeAuthFlag":U
             cRuleCode = IF ipoControl:ControlName MATCHES ("cbPenaltyValue*") 
                         OR ipoControl:ControlName MATCHES ("fcPenaltyOverrideNote*":U) THEN "PENALTY":U 
                         ELSE "EMERGENCY":U.
      
      mipEnv:Health:AuthMaintenance:getAuthRuleDetails( INPUT-OUTPUT dAuthRuleObj   ,
                                                        INPUT-OUTPUT dInsurerObj    ,
                                                        INPUT-OUTPUT iOption    ,
                                                        INPUT-OUTPUT cRuleType  ,
                                                        INPUT-OUTPUT cRuleCode  ,
                                                        INPUT-OUTPUT dStartDate ,
                                                              OUTPUT lValidRule      ,
                                                              OUTPUT cRuleValue      ,
                                                              OUTPUT cRuleValidValues,
                                                              OUTPUT dLinkAuthRuleObj,
                                                              OUTPUT cRuleDescription,
                                                              OUTPUT lSystemOwned    ,
                                                              OUTPUT dEndDate).

     IF ipoControl:ControlName MATCHES ("cbEmergencyFlagValue*":U) 
     OR ipoControl:ControlName MATCHES ("flMainProvider*":U)  
     THEN 
       mipEnv:health:AuthBusinessLogic:activateEmergencyFlag( INPUT  dAuthTypeObj , 
                                                              INPUT  dInsurerObj, 
                                                              INPUT  iOption, 
                                                              INPUT  dStartDate ,
                                                              OUTPUT lActivateEmergencyFlag).
     
     /*
       This block is for the main provider checkbox .
     */
     IF ipoControl:ControlName MATCHES ("flMainProvider*":U)  THEN 
     DO:
     
       ASSIGN ipoControl:JavascriptOnChange = "fnEnforceEmergencyFlag(this);" WHEN lActivateEmergencyFlag.
       
       ipoControl:renderAsCheckBox() .
       
       RETURN.
       
     END. //ipoControl:ControlName = "flMainProvider":U AND lActivateEmergencyFlag .
      

     IF ipoControl:ControlName MATCHES ("cbPenaltyValue*":U) 
     OR ipoControl:ControlName MATCHES ("fcPenaltyOverrideNote*":U)
     THEN 
       mipEnv:health:AuthBusinessLogic:activatePenaltyFlag( INPUT  dAuthTypeObj , 
                                                            INPUT  dInsurerObj, 
                                                            INPUT  iOption, 
                                                            INPUT  dStartDate ,
                                                            OUTPUT lActivateEmergencyFlag).
     
                                      
     IF  lMainProvider 
     AND lActivateEmergencyFlag THEN 
     DO:
     
       ASSIGN ipoControl:ControlClass  = "+clMan":U    WHEN ipoControl:ControlName MATCHES ("cbEmergencyFlagValue*":U)
              ipoControl:ControlToken  = "updatable":U WHEN ipoControl:ControlName MATCHES ("cbEmergencyFlagValue*":U).
        
       EMPTY TEMP-TABLE tt_auth_flag_value.
      
       ASSIGN oAuthFlagSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE)
              lSuccess        = mipEnv:RegisterForCleanup(oAuthFlagSearch)
              lSuccess        = oAuthFlagSearch:SetCriteria("BufferList":U, "tt_auth_flag_value":U)
              lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
              lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, dAuthObj)
              lSuccess        = oAuthFlagSearch:SetFilterCriteria("tt_auth_flag_value.auth_rule_obj":U         , "=":U, dAuthRuleObj)
              lSuccess        = oAuthFlagSearch:fetchData().

       FIND FIRST tt_auth_flag_value 
            WHERE tt_auth_flag_value.owning_entity_mnemonic = "hatau":U
              AND tt_auth_flag_value.owning_obj             = dAuthObj
              AND tt_auth_flag_value.auth_rule_obj          = dAuthRuleObj NO-ERROR.
       
       { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
       
       IF AVAILABLE(tt_auth_flag_value) 
       AND dAuthProviderObj > 0 
       THEN
         ASSIGN cPenaltyFlag            = tt_auth_flag_value.auth_flag_value
                ipoControl:ControlValue = IF ipoControl:ControlName MATCHES ("fcPenaltyOverrideNote*":U)
                                          THEN tt_auth_flag_value.override_note 
                                          ELSE tt_auth_flag_value.auth_flag_value.
       ELSE 
         ASSIGN ipoControl:ControlValue = "":U .  
         
     END. //lMainProvider
     ELSE 
       ASSIGN ipoControl:ControlToken  = "Disabled":U  
              ipoControl:ControlClass  = "-clMan":U WHEN ipoControl:ControlName MATCHES ("cbEmergencyFlagValue*":U). 

     /*
       Do this piece here to ensure that if there are blank values setup , we assign it to " " (space) characters, otherwise the renderAsComboOrSelect() method below fails
     */
     IF SUBSTRING(cRuleValidValues, 1, 1) = "|" THEN
     ASSIGN cRuleValidValues = " ":U + cRuleValidValues .
     
     IF SUBSTRING(cRuleValidValues, LENGTH(cRuleValidValues) ,1) = "|" THEN
         ASSIGN cRuleValidValues =  " |":U + RIGHT-TRIM(cRuleValidValues,"|":U )  .
     
     DO WHILE INDEX(cRuleValidValues , "||":U) > 0:
     
         ASSIGN cRuleValidValues = SUBSTRING(cRuleValidValues, 1 , INDEX(cRuleValidValues , "||":U ) ) + " ":U  + SUBSTRING(cRuleValidValues, INDEX(cRuleValidValues , "||":U ) + 1, LENGTH(cRuleValidValues) ).
     
     END. //DO WHILE INDEX(cRuleValidValues , "||":U) > 0

     IF ipoControl:ControlName MATCHES ("fcPenaltyOverrideNote*":U) THEN 
     DO: 
       ipoControl:ControlToken = IF cPenaltyFlag = "":U OR cPenaltyFlag = "YES":U THEN "Disabled":U ELSE "Updatable":U.
       ipoControl:RenderAsInput().
     END.
     ELSE DO:
       IF lValidRule AND cRuleValidValues <> "":U
       THEN 
         ASSIGN ipoControl:AdditionalItems = STRING(cRuleValidValues) .
       ELSE
         ASSIGN ipoControl:AdditionalItems = "<None>":U.
       
       ipoControl:RenderAsComboOrSelect().
     END.

    END. //WHEN "EmergencyFlagValue":U THEN
    
    WHEN "CopayOverrideNote":U THEN
    DO:
      {&StandardValues}
      
      
      IF dAuthProviderObj <= 0 
      OR dAuthProviderObj = ? THEN
      DO:
        IF NOT lCopayProvider 
        THEN
          ASSIGN ipoControl:ControlToken  = "Updatable":U .
        ELSE 
          ASSIGN ipoControl:ControlToken  = "Disabled":U .
        
        mipEnv:health:maUtility:getNoteDescription(INPUT cOverrideNoteCode,
                                                   INPUT "AO":U ,
                                                   OUTPUT cNoteDescription) .
        
        ASSIGN ipoControl:ControlToolTip = IF cNoteDescription <> "":U THEN cNoteDescription ELSE "Please enter a Copay Override Reason.":U.
        
      END.

      ipoControl:RenderAsInput(). 
    END. //WHEN "CopayOverrideNote":U THEN

    WHEN "PenaltyOverrideBtn":U THEN
    DO:
      {&StandardValues}

      mipEnv:health:AuthBusinessLogic:activatePenaltyFlag( INPUT  dAuthTypeObj , 
                                                           INPUT  dInsurerObj, 
                                                           INPUT  iOption, 
                                                           INPUT  dStartDate ,
                                                           OUTPUT lActivatePenaltyFlag).
      IF lActivatePenaltyFlag
      THEN 
        ASSIGN ipoControl:ControlToken = "Updatable":U.
      ELSE
        ASSIGN ipoControl:ControlToken = "Hidden":U. 
      
      ipoControl:renderAsLookupButton().
    END. /* WHEN "PenaltyOverrideBtn":U THEN */

  END CASE. /* CASE ENTRY(1, ipoControl:RenderArgument): */   
  
  { mip/inc/mipcatcherror.i }
                
&ENDIF

