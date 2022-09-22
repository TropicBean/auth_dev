/* maauthdetailrenderprocedure.i  MEDSTAR Medical Aid System
                                  Auth ui service specific render procedure
                                  (c) Copyright 2021 - 2022
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/ 
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  

&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE VARIABLE cAlertMessage       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cArsRate            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAttDiscipline      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAttDocDesc         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAttSubDiscipline   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthDetailObj      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthGroupList      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cAuthsNappiRequired AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBaseRateList       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cBaseRate           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cClaimCode          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cClassName          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cCrosswalkParamObj  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDiscipline         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cDocDesc            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cError              AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cImageSrc           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cLineRestriction    AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cMandatory          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cMemNum             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNappiObj           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNegGroup           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cNote               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOEM                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOEMMain            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOEMValue           AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningAltValue     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cOwningKey          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cProviderBaseRate   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cProviderArsRate    AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cProviderList       AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPrType             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRelatedCode        AS CHARACTER                 NO-UNDO. 
  DEFINE VARIABLE cRelatedKey         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cREM                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRuleValue          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cSelectList         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cSlentCode          AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cStatus             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cStatusNote         AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cSubDiscipline      AS CHARACTER                 NO-UNDO.  
  DEFINE VARIABLE cWarning            AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE dEffectiveDate      AS DATE                      NO-UNDO.
  DEFINE VARIABLE dStartDate          AS DATE                      NO-UNDO.
  DEFINE VARIABLE dTariffEffecDate    AS DATE                      NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj      AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj    AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj       AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dAuthObj            AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dCPTObj             AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dOwningObj          AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dRelatedObj         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTariffObj          AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dMinTariffTypeObj   AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE dTrfCostObj         AS DECIMAL                   NO-UNDO.
  DEFINE VARIABLE iCount              AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iDocNum             AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iGroupDocNum        AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iOption             AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iSearchDoctor       AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE lGroupProvider      AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lMandatory          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lSuccess            AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lValidRule          AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lcConfiguration     AS LONGCHAR                  NO-UNDO.
  DEFINE VARIABLE lErrorOccured       AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE hBuffer             AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE hQuery              AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE oControl            AS cls.mipwscontrol          NO-UNDO.

  /*
    Define dispad variables
  */
  { ma/msc/madispad.i &discipline = cDiscipline }
  { ma/msc/madispad.i &discipline = cSubDiscipline    &comment = "/* " }     
  { ma/msc/madispad.i &discipline = cAttDiscipline    &comment = "/* " }       
  { ma/msc/madispad.i &discipline = cAttSubDiscipline &comment = "/* " }         
  
  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).         
     
  CASE ipoControl:RenderArgument:      
  
    WHEN "AuthDetailProviderList":U THEN
    DO:

      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer        = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
          dAuthDetailObj = hBuffer::auth_detail_obj
          hBuffer        = hQuery:GET-BUFFER-HANDLE("tt_auth":U)
          cSelectList    = hBuffer::_provider_list
          cSelectList    = REPLACE(cSelectList, "^":U, "|":U).

      ASSIGN 
        ipoControl:AdditionalItems = IF  cSelectList  <> "":U 
                                     AND cSelectList  <> ?   THEN "<None>=|":U + cSelectList
                                                             ELSE "<None>=":U .
      
      ipoControl:RenderASComboOrSelect().
    
    END. /*WHEN "DetailProviderList":U THEN*/
    
    WHEN "AuthDetSlentButtonSet":U THEN
    DO: 
      
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer      = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
          cRelatedCode = hBuffer::related_value
          cOEMValue    = hBuffer::related_entity_mnemonic.
          
          IF cOEMValue = "hlmnl":U
          THEN
            FIND FIRST hlm_nappi NO-LOCK
              WHERE hlm_nappi.nappi_code = cRelatedCode NO-ERROR.
            
          IF AVAILABLE hlm_nappi THEN
            ASSIGN ipoControl:ControlToken = "Disabled":U
                   ipoControl:ControlValue = hlm_nappi.nappi_description.
          
          IF cOEMValue = "hlmcr":U
          THEN
            FIND FIRST hlm_crosswalk NO-LOCK
                 WHERE hlm_crosswalk.crosswalk_code = cRelatedCode NO-ERROR.
          
          IF AVAILABLE hlm_crosswalk THEN
            ASSIGN ipoControl:ControlToken = "Disabled":U
                   ipoControl:ControlValue = hlm_crosswalk.crosswalk_label.
              
          ipoControl:RenderASTextArea().
        
    END.  /* WHEN "AuthDetSlentButtonSet":U THEN */
    
    WHEN "AuthDetailRelatedOEMRestrict":U THEN
    DO:
      ASSIGN
        hBuffer  = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
        cOEMMain = hBuffer::owning_entity_mnemonic.
        
      IF cOEMMain = "hlmnl":U OR cOEMMain = "hlmcr":U
      THEN
        ASSIGN ipoControl:ControlToken = "Disabled":U.
        
      ASSIGN cOEMValue    = hBuffer::related_entity_mnemonic.
      
      IF cOEMValue = "Hatad":U
      THEN
        ASSIGN ipoControl:ControlToken = "Disabled":U.
            
        ipoControl:RenderASComboORSelect().
    END.  /* WHEN "AuthDetailRelatedOEMRestrict":U THEN */
    
    WHEN "AuthDetailRelatedValueRestrict":U THEN
    DO:
      ASSIGN
        hBuffer  = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
        cOEMMain = hBuffer::owning_entity_mnemonic.
        
        IF cOEMMain = "hlmnl":U OR cOEMMain = "hlmcr":U
        THEN
          ASSIGN ipoControl:ControlToken = "Disabled":U.
      
      ipoControl:RenderASInput().
    END.  /* WHEN "AuthDetailRelatedValueRestrict":U THEN */
        
    WHEN "MinRequestedAndAuthorised":U THEN
    DO:
      ASSIGN
        hBuffer           = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
        dMinTariffTypeObj = hBuffer::minutes_tariff_type_obj.
        
        IF dMinTariffTypeObj > 0.00
        THEN
          ASSIGN ipoControl:ControlToken = "Updatable":U.
        ELSE
          ASSIGN ipoControl:ControlToken = "Disabled":U.
      
      ipoControl:RenderASInput().

    END. /* WHEN "MinRequestedAndAuthorised":U THEN */

    WHEN "AuthDetailNappiButton":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer            = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
          dAuthDetailObj     = hBuffer::auth_detail_obj
          cOEM               = hBuffer::owning_entity_mnemonic
          dOwningObj         = hBuffer::owning_obj
          cOwningKey         = hBuffer::owning_key 
          cOwningAltValue    = hBuffer::owning_alt_value
          dRelatedObj        = hBuffer::related_obj 
          cBaseRate          = hBuffer::default_base_rate
          cArsRate           = hBuffer::default_ars_rate
          cDiscipline        = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::pr_type
          cSubDiscipline     = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::sub_pr_type
          dStartDate         = hBuffer::start_date.
          
		IF VALID-HANDLE(hBuffer) THEN 
		  ASSIGN 
		    ipoControl:JavascriptOnClick = "fnShowAuthNappi(this);":U
		    ipoControl:JavascriptOnClick = REPLACE(REPLACE(ipoControl:JavascriptOnClick,"~n","")," ","") 
		    NO-ERROR.       
      
        IF cOEM = "htmtl":U THEN
        DO:
        
          IF cBaseRate = "":U THEN
          DO:
            ASSIGN cBaseRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::override_base_rate WHEN VALID-HANDLE(hQuery).
        
            IF cBaseRate = "":U 
            THEN ASSIGN cBaseRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::default_base_rate WHEN VALID-HANDLE(hQuery).
        
            IF cBaseRate = "":U 
            THEN ASSIGN cBaseRate = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_base_rate WHEN VALID-HANDLE(hQuery).
          END. /* IF cBaseRate = "":U  */
          
          IF cArsRate = "":U THEN
          DO:
            ASSIGN cArsRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::override_ars_rate WHEN VALID-HANDLE(hQuery).
            
            IF cArsRate = "":U 
            THEN ASSIGN cArsRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::default_ars_rate WHEN VALID-HANDLE(hQuery).
          
            IF cArsRate = "":U 
            THEN ASSIGN cArsRate = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_ars_rate WHEN VALID-HANDLE(hQuery).
          END. /* IF cArsRate = "":U  */

          ASSIGN iOption = INTEGER(hQuery:GET-BUFFER-HANDLE("tt_auth":U)::option_code) WHEN VALID-HANDLE(hQuery).
        
          /* Find the tariff link record */
          ASSIGN lSuccess = mipEnv:Health:maMedical:getValidTariff
			                         (INPUT-OUTPUT dOwningObj,              /*  iodTariffLinkObj  */
                                INPUT        cOwningAltValue,         /*  ipcTariffCode     */
                                INPUT        cBaseRate,               /*  ipcBaseRate       */
                                INPUT        cArsRate,                /*  ipcARSRate        */
                                INPUT        INTEGER(cDiscipline),    /*  ipiPrType         */
                                INPUT        INTEGER(cSubDiscipline), /*  ipiSubPrType      */
                                INPUT        dStartDate,              /*  ipdDate           */
                                INPUT        iOption,                 /*  ipiOptionCode     */
                                INPUT        "",                      /*  ipcAddValidations */
				                        OUTPUT       dTariffObj,              /*  opdTariffObj      */
                                OUTPUT       dTrfCostObj,             /*  opdTrfCostObj     */
                                OUTPUT       cError,                  /*  opcError          */
                                OUTPUT       cWarning,                /*  opcWarning        */
                                OUTPUT       cAlertMessage).          /*  opcAlertMessage   */
                                      
          IF cError = "":U THEN
          DO:
            IF dTariffObj <> ? THEN 
            DO:
              FIND FIRST tariff NO-LOCK
                   WHERE tariff.tariff-obj = dTariffObj 
                NO-ERROR.
              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
              
              IF AVAILABLE tariff 
              THEN
                ASSIGN cAuthsNappiRequired = tariff.nappi-required-auths.
            END.
          END.                            
      
          IF dAuthDetailObj > 0 
          AND dAuthDetailObj <> ? 
          THEN DO:
            IF (cAuthsNappiRequired = "ma_acNappiRequiredNone":U OR cAuthsNappiRequired = "":U OR cAuthsNappiRequired = ?)
            THEN ASSIGN cClassName = "clHid":U .
            ELSE cClassName = "":U . 
          END.  
          ELSE 
            ASSIGN cClassName = "clHid":U . 
        
        END. /* IF cOEM = "htmtl":U THEN */
        ELSE
          ASSIGN cClassName = "clHid":U . 
           
    
      {&OUT} { ma/inc/maiconbutton.i &ButtonSource  = "'/img/rx.png'" 
                                     &ButtonOnClick = ipoControl:JavascriptOnClick
                                     &ButtonClass   = cClassName
                                     &ButtonTitle   = ''
                                     &ButtonControl = ipoControl } .
                                                                                                                       
    END. /* WHEN "AuthDetailNappiButton":U */

    WHEN "AuthDetailEditButton":U THEN 
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer        = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
          dAuthObj       = hBuffer::auth_obj
          dAuthDetailObj = hBuffer::auth_detail_obj.

      ASSIGN 
         cImageSrc                    = WarpSpeed:ThemePath + "img/modify.png":U
         
         cClassName                   = (IF dAuthDetailObj <= 0.00 THEN "clHid":U ELSE "":U)
         cAuthDetailObj               = (IF dAuthDetailObj <= 0.00 THEN "[obj]":U ELSE STRING(dAuthDetailObj))
         
         ipoControl:ControlJavascript = " onmouseover=~"$(this).fadeTo('fast', 0.7);~" onmouseout=~"$(this).fadeTo('fast', 1);~"":U
         
         ipoControl:JavascriptOnClick = "event.stopPropagation();":U
                                      + {ws/inc/wshref.i &wob        = "'mahatad'"
                                                         &obj        = cAuthDetailObj
                                                         &linktext   = "'BTN'"                                                  
                                                         &linkargs   = "'&PopupMode=true' + (IF get-value('wobMode') = 'Enquiry' THEN '&wobmode=Enquiry' ELSE '')"                                 
                                                         &hreftarget = "'popupwindow'"
                                                         &hreftype   = 'javascript'}.
      
      /*
        Edit button to run form for updating detail line
      */
      {&OUT}
        "<a href='#' class = '" + cClassName + "' onclick='event.preventDefault(); $(this).find(~"img~").click();'>"
        "<img name = '":U + ipoControl:InstanceName + "' src='":U + cImageSrc + "' onclick = '":U + ipoControl:JavascriptOnClick + "' ":U + ipoControl:ControlJavascript + " class='" + cClassName + "' title='":U + mipEnv:miUtility:encode-url(ipoControl:ControlTooltip, "html":U) + "'" + (IF ipoControl:ControlTabOrder = 0 THEN "":U ELSE " tabindex='" + STRING(ipoControl:ControlTabOrder) + "' ") + " ></img>"
        "</a>":U
        .
    END. /*WHEN "AuthDetailEditButton":U THEN */       
    
    WHEN "AuthDetProviderNum":U     OR WHEN "AuthDetProviderDesc":U    OR WHEN "AuthDetProviderDiscipline":U    OR WHEN "AuthDetProviderSubDiscipline":U  OR 
    WHEN "AuthDetAttProviderNum":U  OR WHEN "AuthDetAttProviderDesc":U OR WHEN "AuthDetAttProviderDiscipline":U OR WHEN "AuthDetAttProviderSubDiscipline":U
    THEN
    DO:      
        IF VALID-HANDLE(hQuery)
        THEN
          ASSIGN
            hBuffer           = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
            iGroupDocNum      = hBuffer::group_doc_num
            iDocNum           = hBuffer::doc_num
            dEffectiveDate    = hBuffer::start_date
            cAttDiscipline    = hBuffer::pr_type
            cAttSubDiscipline = hBuffer::sub_pr_type

            hBuffer           = hQuery:GET-BUFFER-HANDLE("group_discipline":U)
            cDiscipline       = hBuffer::disp-code
            cSubDiscipline    = hBuffer::subdisp-code

            cDocDesc          = hQuery:GET-BUFFER-HANDLE("group_doctor":U)::name
            cAttDocDesc       = hQuery:GET-BUFFER-HANDLE("attending_doctor":U)::name
            lGroupProvider    = (iGroupDocNum <> 0 AND mipEnv:Health:maDoctor:isProviderAValidGroup(INPUT iGroupDocNum, INPUT dEffectiveDate) OR ( (iGroupDocNum <> 0.00 AND iDocNum <> 0.00))). 

      CASE ipoControl:RenderArgument:
      
        WHEN "AuthDetProviderNum":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN STRING(iGroupDocNum) 
                                            ELSE STRING(iDocNum))
          
                 ipoControl:ControlValue = (IF ipoControl:ControlValue = "0":U OR (INTEGER(ipoControl:ControlValue) < 0) 
                                            THEN "":U 
                                            ELSE ipoControl:ControlValue).
        
        WHEN "AuthDetProviderDesc":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF iDocNum = 0 
                                            THEN "":U 
                                            ELSE (IF lGroupProvider THEN cDocDesc ELSE cAttDocDesc)).
        
        WHEN "AuthDetProviderDiscipline":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN cDiscipline 
                                            ELSE cAttDiscipline)
                                            
                 ipoControl:ControlValue = (IF ipoControl:ControlValue = "000":U 
                                            THEN "":U 
                                            ELSE ipoControl:ControlValue).
          
        WHEN "AuthDetProviderSubDiscipline":U 
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
                                            ELSE ipoControl:ControlValue).
          
        WHEN "AuthDetAttProviderDesc":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF iDocNum = 0 
                                            THEN "":U 
                                            ELSE (IF lGroupProvider THEN cAttDocDesc ELSE "":U)).
        
        WHEN "AuthDetAttProviderDiscipline":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN cAttDiscipline 
                                            ELSE "000":U).
          
        WHEN "AuthDetAttProviderSubDiscipline":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider 
                                            THEN cAttSubDiscipline 
                                            ELSE "000":U).
                
        WHEN "AuthDetAttProviderNum":U 
        THEN 
          ASSIGN ipoControl:ControlValue = (IF lGroupProvider AND iDocNum <> iGroupDocNum  
                                            THEN STRING(iDocNum) 
                                            ELSE "":U).
           
      END CASE.
      
      ipoControl:RenderASInput().
        
    END. /*WHEN "AuthDetProviderNum":U OR WHEN "AuthDetProviderDesc":U OR WHEN "AuthDetProviderDiscipline":U OR WHEN "AuthDetProviderSubDiscipline":U  OR ....*/
      
    WHEN "BaseRate" THEN
    DO:
      ASSIGN cBaseRate  = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::default_base_rate WHEN VALID-HANDLE(hQuery).
      
      /*
        If the base rate on detail line level is blank, check to see if there is an override base rate on the provider, if there is none use the default base rate
      */
      IF cBaseRate = "":U THEN
      DO:
        ASSIGN cBaseRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::override_base_rate WHEN VALID-HANDLE(hQuery).
        
        IF cBaseRate = "":U 
        THEN
          ASSIGN cBaseRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::default_base_rate WHEN VALID-HANDLE(hQuery).
        
        IF cBaseRate = "":U 
        THEN
          ASSIGN cBaseRate = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_base_rate WHEN VALID-HANDLE(hQuery).
      END. /* IF cBaseRate = "":U  */
        
      ASSIGN ipoControl:ControlValue = cBaseRate.
      
      ipoControl:RenderASInput(). 
    END. /* WHEN "BaseRate" */
    
    WHEN "ArsRate" THEN
    DO:
      ASSIGN cArsRate  = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::default_ars_rate WHEN VALID-HANDLE(hQuery).
      
      /*
        If the ars rate on detail line level is blank, check to see if there is an override ars rate on the provider, if there is none use the default ars rate
      */
      IF cArsRate = "":U THEN
      DO:
        ASSIGN cArsRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::override_ars_rate WHEN VALID-HANDLE(hQuery).
        
        IF cArsRate = "":U 
        THEN
          ASSIGN cArsRate = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::default_ars_rate WHEN VALID-HANDLE(hQuery).

        IF cArsRate = "":U 
        THEN
          ASSIGN cArsRate = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_ars_rate WHEN VALID-HANDLE(hQuery).
      END. /* IF cArsRate = "":U  */
        
      ASSIGN ipoControl:ControlValue = cArsRate.
      
      ipoControl:RenderASInput(). 
    END. /* WHEN "ArsRate" */
    
    WHEN "AuthDetailOwningInfo":U THEN
    DO:
      IF VALID-HANDLE(hQuery)
      THEN
        ASSIGN
          hBuffer         = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
          cOEM            = hBuffer::owning_entity_mnemonic
          cOwningAltValue = hBuffer::owning_alt_value
          dOwningObj      = hBuffer::owning_obj
          hBuffer         = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          cDiscipline     = hBuffer::pr_type
          cSubDiscipline  = hBuffer::sub_pr_type.

      ASSIGN ipoControl:LinkText      = IF ipoControl:ControlName BEGINS "fcViewItemCode":U
                                        THEN "View Item Code Details" 
                                        ELSE "View Detail"
             ipoControl:ControlClass  = "-clHid":U.
     
      /*
        We need to configure the detail href to launch the relevant wob with the required parameters for the particular wob depending on the entity 
      */                  
      IF cOEM <> "":U AND cOEM <> ? AND NOT ipoControl:ControlName BEGINS "fcLinkTemplate":U THEN
      DO:
        CASE cOEM:
            
          /*
            Basket's
          */  
          WHEN "hlmcr":U THEN 
          DO:
            ASSIGN 
              cCrosswalkParamObj = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_crosswalk_parameter_obj WHEN VALID-HANDLE(hQuery).
                
            ASSIGN ipoControl:Wob           = "mabasket":U
                   ipoControl:Obj           = STRING(dOwningObj)         
                   ipoControl:ControlValue  = STRING(dOwningObj)
                   ipoControl:ControlTarget = "PopupWindow":U
                   ipoControl:AssignList    = "&popupMode=true&wobMode=enquiry&wobtype=ma_acCrossTypeBasket&fdCrosswalkParameterObj=":U + cCrosswalkParamObj. 
                                               
          END. /*WHEN "hlmcr":U THEN*/
          
          /*
            Nappi's
          */
          WHEN "hlmnl":U THEN 
          DO:
            ASSIGN
              cNappiObj = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_nappi_obj WHEN VALID-HANDLE(hQuery).
                
            IF DECIMAL(cNappiObj) <> 0.00 AND DECIMAL(cNappiObj) <> ?
            THEN      
              ASSIGN ipoControl:Wob           = "mahlmna":U
                     ipoControl:Obj           = cNappiObj         
                     ipoControl:ControlValue  = cNappiObj
                     ipoControl:ControlTarget = "PopupWindow":U
                     ipoControl:AssignList    = "&popupMode=true&wobMode=enquiry":U.
            ELSE
              ASSIGN ipoControl:ControlClass  = "+clHid":U. 
                       
          END. /*WHEN "hlmnl":U THEN*/
          
          /*
            Tariff's
          */
          WHEN "htmtl":U THEN 
          DO:
            IF VALID-HANDLE(hQuery)
            THEN
              ASSIGN
                hBuffer          = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
                dTariffEffecDate = hBuffer::_tariff_effec_date
                cBaseRate        = hBuffer::_base_rate
                cArsRate         = hBuffer::_ars_rate.

            /*
              Make sure the discipline is padded before searching with it
            */
            { ma/msc/madispad.i &discipline = cDiscipline    &comment = "/* " }     
            { ma/msc/madispad.i &discipline = cSubDiscipline &comment = "/* " }       
            
            ASSIGN ipoControl:Wob           = "matariffmaint":U
                   ipoControl:Obj           = "[ClearObj]":U         
                   ipoControl:ControlValue  = "[ClearObj]":U
                   ipoControl:ControlTarget = "PopupWindow":U
                   ipoControl:AssignList    = "&popupMode=true&wobMode=enquiry":U
                                            + "&fcTariffFrom=":U    + cOwningAltValue
                                            + "&fcTariffTo=":U      + cOwningAltValue
                                            + "&fdEffectiveDate=":U + STRING(dTariffEffecDate, "99/99/9999":U)
                                            + "&fcBaseRate=":U      + cBaseRate
                                            + "&fcARSRate=":U       + cArsRate
                                            + "&fcDiscipline=":U    + cDiscipline
                                            + "&fcSubDiscipline=":U + cSubDiscipline.
            
                            
          END. /*WHEN "htmtl":U THEN*/     
        END CASE.                                  
      END. /*IF cOEM <> "":U THEN*/    
      ELSE
      DO:
        ASSIGN ipoControl:Wob           = "[wob]":U
               ipoControl:Obj           = "[obj]":U         
               ipoControl:ControlValue  = "[obj]":U
               ipoControl:ControlTarget = "PopupWindow":U
               ipoControl:AssignList    = "popupMode=true&wobMode=enquiry[assignlist]":U
               ipoControl:ControlClass  = "clHid":U.  
      END. /*ELSE*/              
      
      ipoControl:RenderASHref().
    END. /*WHEN "AuthDetailOwningInfo":U THEN*/   
    
    WHEN "AuthDetailSequence":U THEN 
    DO:
      ASSIGN
        dAuthDetailObj                = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::auth_detail_obj WHEN VALID-HANDLE(hQuery)
        ipoControl:JavascriptOnChange = SUBSTITUTE("fnOnChangeAuthDetailSequence(this,~"&1~");":U, ipoControl:ParentContainer:ContainerCode)
        
        lSuccess                      = ipoControl:RenderAsInput().
      
    END. /*WHEN "AuthDetailSequence":U THEN */
    
    WHEN "IntegerField":U THEN 
    DO:
      /* 
        Render as integer field as a blank instead of zero's
      */
      IF INTEGER(ipoControl:ControlValue) = 0 
      THEN 
        ASSIGN ipoControl:ControlValue = "":U.
      
      ipoControl:renderAsInput().
    END. /* WHEN "IntegerField":U THEN */
    
    /* For tables this logic has been moved to a RowRenderProcedure for performance reasons */  
    /* but we need to hang on to this RenderProcedure block for the detail form             */
    WHEN "AuthDetailStatusNote":U THEN 
    DO:
      
      IF ipoControl:ControlClass = "clErr":U OR ipoControl:ControlClass = "clAjaxErr":U
      THEN
        ASSIGN lErrorOccured = TRUE.
      
      ASSIGN 
        lMandatory                = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_mandatory WHEN VALID-HANDLE(hQuery)

        ipoControl:ControlClass   = ipoControl:ControlClass 
                                  + (IF lMandatory THEN " +":U ELSE " -":U)
                                  + "clMan":U.
      IF lErrorOccured <> TRUE
      THEN
        ASSIGN ipoControl:ControlToolTip = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::_auth_status_narration WHEN VALID-HANDLE(hQuery).
      
      /*
        Render control
      */
      IF ipoControl:ControlType = 'wsLookupButton'
      THEN 
        ipoControl:RenderASLookupButton().
      ELSE
        ipoControl:RenderASInput().
    END. /*WHEN "AuthProviderStatusNote":U THEN */

    WHEN "AuthLosAdd":U THEN
    DO:
      
      ASSIGN ipoControl:JavascriptOnClick  = SUBSTITUTE("fnOnClickAuthLosAdd(this,~"&1~");":U, ipoControl:ParentContainer:ContainerCode).

      {&OUT} { ma/inc/maiconbutton.i &ButtonSource      = "'img/add.png'" 
                                     &ButtonTitle       = "'Add'" 
                                     &ButtonControl     = ipoControl 
                                     &ApplyClickOnFocus = TRUE}.
                                                                           
    END. /*WHEN "CrosswalkFilterAdd":U THEN*/
    
    WHEN "AuthLosDelete":U THEN
    DO:
      
      ASSIGN ipoControl:JavascriptOnClick  = SUBSTITUTE("fnOnClickAuthLosDelete(this,~"&1~");":U, ipoControl:ControlName).
                                                                     
      {&OUT} { ma/inc/maiconbutton.i &ButtonSource  = "'img/delete.png'" 
                                     &ButtonTitle   = "'Delete'" 
                                     &ButtonControl = ipoControl }.
                                                                           
    END. /*WHEN "CrosswalkFilterDelete":U THEN*/
    
    WHEN "ClaimCode":U THEN
    DO:
      ASSIGN 
        cClaimCode  = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::claim_code WHEN VALID-HANDLE(hQuery).
      
      /*
        If the claim-code on detail line level is blank, use the value on the provider
      */
      IF cClaimCode = "":U 
      THEN ASSIGN cClaimCode = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::claim_code WHEN VALID-HANDLE(hQuery).
        
      ASSIGN ipoControl:ControlValue = cClaimCode.
      
      ipoControl:RenderASInput(). 

    END. /* WHEN "ClaimCode" */

    WHEN "BaseRateList" THEN
    DO:
      FOR EACH baserate NO-LOCK
            BY baserate.base-rate:
    
        ASSIGN cBaseRateList = cBaseRateList
                             + (IF cBaseRateList = "":U THEN "":U ELSE "|":U)
                             + baserate.base-rate.
      END. /*FOR EACH baserate NO-LOCK*/

      ASSIGN
        ipoControl:AdditionalItems = "=|":U + cBaseRateList.
          
      ipoControl:renderAsComboOrSelect().
    END. /* WHEN "ProviderList" THEN */
   
    WHEN "LineRestriction":U THEN
    DO:
      FOR EACH mic_acronym NO-LOCK
          WHERE mic_acronym.category_key = "ma_acAuthLineRestriction":U
             BY mic_acronym.acronym_sequence:
    
        ASSIGN cLineRestriction = cLineRestriction
                                + (IF cLineRestriction = "" THEN "":U ELSE "|":U)
                                + mic_acronym.acronym_label + "=":U + mic_acronym.acronym_key .
      END. /* FOR EACH mic_acronym NO-LOCK */

      ASSIGN
         ipoControl:AdditionalItems = cLineRestriction.

      ipoControl:renderAsComboOrSelect().
    END. /* WHEN "LineRestriction":U THEN */

    WHEN "TariffType":U THEN
    DO:
  	  ASSIGN oControl = ipoControl:ParentContainer:getControl("cbTariffType":U + ipoControl:ParentContainer:ContainerCode).
      IF DECIMAL(oControl:ControlValue) > 0.0 OR trim(oControl:ControlValue) <> "" 
      THEN
        ASSIGN ipoControl:ControlToken = "readOnly":U
  		         ipoControl:ControlClass = "-clHid":U.
      ELSE 
        ASSIGN ipoControl:ControlToken = "Hidden":U
  		         ipoControl:ControlClass = "+clHid":U.
      
          ipoControl:renderAsComboOrSelect().		

    END.  /* WHEN "TariffType":U THEN */

    WHEN "RelatedEntity":U THEN
    DO:
  	   ASSIGN hBuffer   = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)
          cOEMValue    = hBuffer::related_entity_mnemonic.
      IF cOEMValue = "Hatad":U
      THEN
        ASSIGN ipoControl:ControlToken = "Hidden":U
  		         ipoControl:ControlClass = "+clHid":U.
      ELSE 
        ASSIGN ipoControl:ControlClass = "-clHid":U.
      
          ipoControl:renderAsComboOrSelect().		

    END.  /* WHEN "RelatedEntity":U THEN */

    WHEN "AuthDetailPayeeIndicator":U THEN 
    DO:
      ASSIGN ipoControl:ControlValue = hQuery:GET-BUFFER-HANDLE("tt_auth_detail":U)::payee_dm WHEN VALID-HANDLE(hQuery).

      ipoControl:RenderASComboORSelect().                                               
    END. /*WHEN "AuthDetailPayeeIndicator":U THEN */

    WHEN "AuthDetAuthGroupList":U THEN
    DO:
      ASSIGN
          hBuffer        = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
          dAuthGroupObj  = hBuffer::auth_group_obj
          cPrType        = hBuffer::pr_type
          hBuffer        = hQuery:GET-BUFFER-HANDLE("tt_auth":U)
          dStartDate     = hBuffer::start_date.

      FOR EACH ham_auth_group NO-LOCK
          WHERE (dAuthGroupObj = 0 OR ham_auth_group.auth_group_obj = dAuthGroupObj)
          AND    ham_auth_group.effective_date <= dStartDate
          AND   (ham_auth_group.end_date = ? OR ham_auth_group.end_date > dStartDate):

        IF NOT CAN-FIND(FIRST ham_auth_group_detail
                        WHERE ham_auth_group_detail.auth_group_obj = ham_auth_group.auth_group_obj)
        OR CAN-FIND(FIRST ham_auth_group_detail
                    WHERE ham_auth_group_detail.auth_group_obj = ham_auth_group.auth_group_obj
                    AND  (ham_auth_group_detail.pr_type = "000":U
                    OR    ham_auth_group_detail.pr_type = cPrType)) THEN
          ASSIGN cAuthGroupList = cAuthGroupList
                                + (IF cAuthGroupList = "" THEN "=|":U ELSE "|":U)
                                + ham_auth_group.auth_group_code + "=":U + STRING(ham_auth_group.auth_group_obj).
    
      END. /* FOR EACH ham_auth_group NO-LOCK */

      ASSIGN
         ipoControl:AdditionalItems = cAuthGroupList.

      ipoControl:renderAsComboOrSelect().
    END.  /* WHEN "AuthDetAuthGroupList":U THEN */
	
    OTHERWISE RUN SUPER( INPUT ipoControl ).

  END. /*CASE ipoControl:RenderArgument:*/
    
  { mip/inc/mipcatcherror.i }
                
                                
&ENDIF          


