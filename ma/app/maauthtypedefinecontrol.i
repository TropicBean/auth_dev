/* maauthtypedefinecontrol.i MEDSTAR Medical Aid System
                            Save Authorisation Detail Record
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
   Note: If you change any of the headings on the controls, please make sure
          you change the label accordingly in maauthtypecontroltt.i.
*/  
   DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
   DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.

   DEFINE VARIABLE cControlIndicatorTypeFieldChange AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cControlMakeFieldsMandatory      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cGenderFieldChange               AS CHARACTER NO-UNDO. 

   ASSIGN cControlIndicatorTypeFieldChange = "ma_acAuthControlTypeIndicatorDef=fcDefaultAuthStatus,fcReasonKey,fcStatusReasonBtn|ma_acAuthControlTypeIndicatorExcl=fcClaimCode,fcClaimCodeLkp,fcClaimType,"
                                           + "fcClaimCodeDis,fcClaimCodesControlLkp,fcClaimTypeDis,fcUsageType,fcUsageQuantity,fcUsagePeriod,fcUsagePeriodType,"
                                           + "fcUsagePeriodOver,fcUserMsc,fcPeriod,fcPeriodType,fcPeriodOverride,fcRestrictions,fcRestrictionMsc,fcQuantity,"
                                           + "fcAmount,fcActivateAuthorised,fcEnforceAthorised":U
          cControlMakeFieldsMandatory      = "ma_acAuthControlTypeIndicatorDef=|ma_acAuthControlTypeIndicatorExcl=fcReasonKey":U
          cGenderFieldChange               = "B=fcAgeRangeMaleDesc,fcAgeRangeFemaleDesc,fcAgeRangeBothDesc|M=fdAgeRangeFemaleObj,fcAgeRangeFemale,buAgeRangeFemaleBtn,fdAgeRangeBothObj,":U
                                           + "fcAgeRangeBoth,buAgeRangeBothBtn,fcAgeRangeMaleDesc,fcAgeRangeFemaleDesc,fcAgeRangeBothDesc":U
                                           + "|F=fdAgeRangeMaleObj,fcAgeRangeMale,buAgeRangeMaleBtn,fdAgeRangeBothObj,fcAgeRangeBoth,buAgeRangeBothBtn,fcAgeRangeMaleDesc,fcAgeRangeFemaleDesc,fcAgeRangeBothDesc":U.


   ASSIGN                                                                                                                                                                             
    goCntTypeControl                           = NEW cls.mipwscontainer("AuthTypeControls":U, "100%":U, "Authorisation Type Control/Defaults":U, WarpSpeed:BaseClass, TRUE)  
    goCntTypeControl:ContainerMode             = goWob:SubmitValue                                                                                                                
    goCntTypeControl:ViewOnly                  = FALSE                                                                                                                                
    goCntTypeControl:ShowGenericReportPrint    = TRUE                                                                                                                                 
    goCntTypeControl:Collapsed                 = FALSE                                                                                                                                
    goCntTypeControl:ViewOnly                  = NOT glEnquiryWob                                                                                                                     
    goCntTypeControl:RowsToRender              = ?                                                                                                                                    
    goCntTypeControl:RowRenderProcedure        = "rowRenderProcedure":U                                                                                                               
    goCntTypeControl:RowRenderArgument         = goCntTypeControl:ContainerCode                                                                                                       
    goCntTypeControl:DefaultContainerType      = "TABLE":U  
    goCntTypeControl:ContainerHidden           = FALSE
                                                                                                                                                                                      
    goCntTypeControl:QueryString               = "FOR EACH tt_auth_type_control NO-LOCK":U
                                               + ",  FIRST tt_sequence no-lock":U
                                               + "   where tt_sequence.sequence_link_obj = tt_auth_type_control.auth_type_control_obj":U
                                               + ",  FIRST male_age_range NO-LOCK":U 
                                               + "   WHERE male_age_range.age_range_obj = tt_auth_type_control.age_range_male_obj OUTER-JOIN":U
                                               + ",  FIRST female_age_range NO-LOCK":U 
                                               + "   WHERE female_age_range.age_range_obj = tt_auth_type_control.age_range_female_obj OUTER-JOIN":U
                                               + ",  FIRST both_age_range NO-LOCK":U 
                                               + "   WHERE both_age_range.age_range_obj = tt_auth_type_control.age_range_both_obj OUTER-JOIN":U
                                               + "   BY tt_sequence.sequence_value DESCENDING ":U
    goCntTypeControl:QueryBufferList           = STRING(TEMP-TABLE tt_auth_type_control:DEFAULT-BUFFER-HANDLE)   + ",":U
                                               + STRING(TEMP-TABLE tt_sequence:DEFAULT-BUFFER-HANDLE)            + ",":U 
                                               + STRING(TEMP-TABLE tt_age_range:DEFAULT-BUFFER-HANDLE)           + ",":U 
                                               + STRING(TEMP-TABLE tt_age_range:DEFAULT-BUFFER-HANDLE)           + ",":U
                                               + STRING(TEMP-TABLE tt_age_range:DEFAULT-BUFFER-HANDLE)           
    
    oControl                                   = goCntTypeControl:addControl("fcControlIndicatorType":U          + goCntTypeControl:ContainerCode, "wsCombo":U,           "10":U, "tt_auth_type_control.control_type_indicator":U,             "character":U,      2, "Control<br>Indicator<br>Type":U)
    oControl:QueryString                       = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthControlTypeIndicator'":U                                                                                                                                                                                              
    oControl:KeyField                          = "mic_acronym.acronym_code":U                                                                                                                                                                                                    
    oControl:DisplayFields                     = "mic_acronym.acronym_label":U
    //oControl:JavascriptOnChange                = "fnEnableDisableFieldControl(this, ~"" + cControlIndicatorTypeFieldChange + "~"); fnSetToDeclined(this, ~"fcDefaultAuthStatus~");":U
    oControl:JavascriptOnChange                = "fnEnableDisableFieldControl(this, ~"" + cControlIndicatorTypeFieldChange + "~", true, true); fnMandatoryFieldToggleControl(this, ~"" + cControlMakeFieldsMandatory + "~"); fnSetExclusionValues(this);":U 
    oControl:JavaScriptOnFocus                 = "fnEnableDisableFieldControl(this, ~"" + cControlIndicatorTypeFieldChange + "~"); "                                                                                                                                                                                                            
                                                                                                                                          
    oControl                                   = goCntTypeControl:addControl("fcInsurer":U + goCntTypeControl:ContainerCode,              "wsCombo":U,             "8":U,  "tt_auth_type_control.insurer_obj":U,     "character":U, 3, "Client":U)                              
    oControl:AdditionalItems                   = "="                                                                                                                                                                                                                            
    oControl:QueryString                       = "FOR EACH erm_insurer NO-LOCK ":U                                                                                                                                                                                              
    oControl:KeyField                          = "erm_insurer.insurer_obj":U                                                                                                                                                                                                    
    oControl:DisplayFields                     = "erm_insurer.insurer_name":U                                                                                                                                                                                                   
                                                                                                                                                                   
    oControl                                   = goCntTypeControl:addControl("fcOption":U + goCntTypeControl:ContainerCode,               "wsCombo":U,             "3":U,  "tt_auth_type_control.option_code":U,  "character":U, 4, "Option":U)                 
    oControl:AdditionalItems                   = "0 - All =0"                                                                                                                                                                                                                    
    oControl:QueryString                       = "FOR EACH scheme NO-LOCK WHERE scheme.active = yes":U                                                                                                                                                                  
    oControl:KeyField                          = "scheme.scheme-code":U                                                                                                                                                                                                 
    oControl:DisplayFields                     = "scheme.scheme-code,scheme.short-name":U                                                                                                                                                                               
    oControl:DisplayMask                       = "&1 - &2"  

    oControl                                   = goCntTypeControl:addControl("fcClaimCode":U + goCntTypeControl:ContainerCode ,           "wsTextArea":U,             "3":U,  "tt_auth_type_control.claim_codes_header":U,      "integer":U,  5, "Claim<br>Code<br>Header":U)
    oControl:ControlToolTip                    = "Claim Code assigned to Authorisation Header where one Claim Code is specified. Multiple Claim Codes indicate the valid Claim Codes for the Authorisation.":U

    oControl                                   = goCntTypeControl:addControl("fcDumyClaimCode":U + goCntTypeControl:ContainerCode,                 "wsInput":U,             "8":U,  "":U,                                     "character":U, 5, "Claim<br>Code<br>Header":U)                              
    oControl:ControlToken                      = "HIDDEN":U  

    oControl                                   = goCntTypeControl:addControl("fcClaimCodeHid":U + goCntTypeControl:ContainerCode ,        "wsInput":U,      "1":U,  "":U,                                     "":U,         5, "Claim<br>Code<br>Header":U)               
    oControl:ControlToken                      = "Hidden":U
    oControl:JavascriptOnChange                = 'if(this.value!="")~{fnGetControls("fcClaimCode'    + goCntTypeControl:ContainerCode + '" + this.name.replace("fcClaimCodeHid' + goCntTypeControl:ContainerCode + '", ""))[0].value = this.value; this.value=~"~"; ~}'
    
    oControl                                   = goCntTypeControl:addControl("fcClaimCodeLkp":U + goCntTypeControl:ContainerCode ,        "wsMsComboButton":U,      "1":U,  "":U,                                     "":U,         5, "Claim<br>Code<br>Header":U)               
    oControl:LookupWobFLA                      = "maclc":U                                                                                                                                                                                                         
    oControl:LookupFields                      = "ccdesc.claim-code":U                                                                                                                                                                                             
    oControl:LookupControls                    = "fcClaimCodeHid":U + goCntTypeControl:ContainerCode                                                                                                                                                                  
    oControl:FilterFields                      = "ccode.effect-date":U                                                                                                                                                                                             
    oControl:FilterControls                    = "fcEffectDate":U + goCntTypeControl:ContainerCode                                                                                                                                                                  
    oControl:ReturnFields                      = "ccdesc.claim-code":U                                                                                                                                                                                             
    oControl:ReturnControls                    = "fcDumyClaimCode":U + goCntTypeControl:ContainerCode                                                                                                                                                                  
    oControl:CellLayoutMask                    = "&1&2&3&4":U
    oControl:ControlToolTip                    = "Lookup valid Claim Code."
    /*
    oControl                                   = goCntTypeControl:addControl("fcClaimType":U + goCntTypeControl:ContainerCode,            "wsSelect":U,             "1":U,  "tt_auth_type_control.claim_types_header":U,      "character":U, 7, "Claim<br>Type<br>Header":U)           
    oControl:AdditionalItems                   = gcClaimType                                                                                                                                                                                                                   
    oControl:ControlTooltip                    = "C=consultation, O=other, N=non eligible, A=acute, K=Chronic":U  
    oControl:ControlJavaScript                 = "multiple=~"multiple~"":U
    */

    oControl                                   = goCntTypeControl:addControl("fcClaimType":U + goCntTypeControl:ContainerCode ,           "wsTextArea":U,             "3":U,  "tt_auth_type_control.claim_types_header":U,      "character":U,  6, "Claim<br>Type<br>Header":U)
    oControl:ControlToolTip                    = "C=consultation, O=other, N=non eligible, A=acute, K=Chronic. Please ensure your selection matches one of the aformentioned values":U

    oControl                                   = goCntTypeControl:addControl("fcDumy":U + goCntTypeControl:ContainerCode,                 "wsInput":U,             "8":U,  "":U,                                     "character":U, 10, "Disallow<br>Claim<br>Codes":U)                              
    oControl:ControlToken                      = "HIDDEN":U                                                                                                                                                         
    oControl                                   = goCntTypeControl:addControl("fcClaimCodesControlHid":U + goCntTypeControl:ContainerCode, "wsInput":U,             "8":U,  "":U,                                     "character":U, 10, "Disallow<br>Claim<br>Codes":U)                              
    oControl:ControlToken                      = "HIDDEN":U
    oControl:JavaScriptOnChange                = 'if(this.value!="")~{fnGetControls("fcClaimCodeDis'    + goCntTypeControl:ContainerCode + '" + this.name.replace("fcClaimCodesControlHid' + goCntTypeControl:ContainerCode + '", ""))[0].value = this.value; this.value=~"~"; ~}'  
    oControl                                   = goCntTypeControl:addControl("fcClaimCodeDis":U + goCntTypeControl:ContainerCode ,        "wsTextArea":U,          "10":U, "tt_auth_type_control.claim_codes_disallow":U, "character":U, 10, "Disallow<br>Claim<br>Codes":U)  
    oControl                                   = goCntTypeControl:addControl("fcClaimCodesControlLkp":U     + goCntTypeControl:ContainerCode, "wsMsComboButton":U, "":U,   "":U,                                          "":U,          10, "Disallow<br>Claim<br>Codes":U)                              
    oControl:LookupWobFLA                      = "maclc":U
    oControl:LookupFields                      = "ccdesc.claim-code":U
    oControl:LookupControls                    = "fcClaimCodesControlHid":U  + goCntTypeControl:ContainerCode
    oControl:FilterFields                      = "ccdesc.claim-code":U
    oControl:FilterControls                    = "fcClaimCodesControlHid":U + goCntTypeControl:ContainerCode
    oControl:ReturnFields                      = "ccdesc.claim-code":U
    oControl:ReturnControls                    = "fcDumy":U    + goCntTypeControl:ContainerCode 
    oControl:CellLayoutMask                    = "&1&2&3&4":U 
    oControl:ControlToolTip                    = "Lookup valid claim code."
    
    oControl                                   = goCntTypeControl:addControl("fcClaimTypeDis":U + goCntTypeControl:ContainerCode,         "wsSelect":U,            "5":U, "tt_auth_type_control.claim_types_disallow":U,  "character":U, 11, "Disallow<br>Claim<br>Types":U)  
    oControl:AdditionalItems                   = gcClaimType
    oControl:ControlTooltip                    = "C=consultation, O=other, N=non eligible, A=acute, K=Chronic":U 
    oControl:ControlJavaScript                 = "multiple=~"multiple~"":U
    .

  ASSIGN
    oControl                                   = goCntTypeControl:addControl("fcUsageType":U           + goCntTypeControl:ContainerCode, "wsCombo":U,           "8":U,  "tt_auth_type_control.usage_type":U,                 "character":U, 16, "Usage<br>Type":U)                  
    oControl:RenderProcedure                   = "RenderProcedure":U         
    oControl:RenderArgument                    = "AcronymSelect:ma_acAuthUsageType:=":U                                                                                                                                                                                              
                                                                                                                                                                                                                            
    oControl                                   = goCntTypeControl:addControl("fcUsageQuantity":U       + goCntTypeControl:ContainerCode, "wsInput":U,           "5":U,  "tt_auth_type_control.usage_quantity":U,             "integer":U,   17, "Usage Quantity":U)                      
    oControl                                   = goCntTypeControl:addControl("fcUsagePeriod":U         + goCntTypeControl:ContainerCode, "wsInput":U,           "5":U,  "tt_auth_type_control.usage_period":U,               "integer":U,   18, "Usage<br>Period":U)             
    oControl                                   = goCntTypeControl:addControl("fcUsagePeriodType":U     + goCntTypeControl:ContainerCode, "wsCombo":U,           "5":U,  "tt_auth_type_control.usage_period_type":U,          "character":U, 19, "Usage<br>Period<br>Type":U)                     
    oControl:RenderProcedure                   = "RenderProcedure":U                                                                                                                                                                                                       
    oControl:RenderArgument                    = "AcronymSelect:ma_acAuthPeriod:=":U                                                                                   
                                                                                                                                                                       
    oControl                                   = goCntTypeControl:addControl("fcUserObjs":U            + goCntTypeControl:ContainerCode, "wsInput":U,          "5":U,   "":U,                                                "character":U, 20, "Usage<br>Override":U)
    oControl:ControlToken                      = "Hidden"
    /* Displays [MULTIPLE SELECTED] on the Usage override field after selecting multiple users." */
    
    oControl:JavaScriptOnChange                = 'if(this.value!="")~{var x = "fcUsagePeriodOver'      + goCntTypeControl:ContainerCode + '" + this.name.replace("fcUserObjs' + goCntTypeControl:ContainerCode + '","");'                            
                                               + ' fnGetControls(x)[0].value="~[MULTIPLE SELECTED~]"~}'                                                                                                                                                             
    
    oControl                                   = goCntTypeControl:addControl("fcUsagePeriodOver":U     + goCntTypeControl:ContainerCode, "wsTextArea":U,       "5":U,   "tt_auth_type_control.usage_override_user":U,        "character":U, 20, "Usage<br>Override":U) 
    /*oControl:CellClass                         = "+clLLb":U*/
    oControl                                   = goCntTypeControl:addControl("fcUserMsc":U             + goCntTypeControl:ContainerCode, "wsMsComboButton":U,  "":U,    "":U,                                                "":U         , 20, "Usage<br>Override":U)                                                                 
    oControl:LookupWobFLA                      = "mimus":U                                           
    oControl:LookupFields                      = "mim_user.user_obj":U                               
    oControl:LookupControls                    = "fcUserObjs" + goCntTypeControl:ContainerCode       
    oControl:FilterFields                      = "mim_user.user_obj":U                               
    oControl:FilterControls                    = "fcUserObjs" + goCntTypeControl:ContainerCode       
    oControl:CellLayoutMask                    = "&1&2&3"                                            
                                                                                                     
    oControl                                   = goCntTypeControl:addControl("fcPeriod":U              + goCntTypeControl:ContainerCode, "wsInput":U,          "5":U,   "tt_auth_type_control.period":U,                     "integer":U,   22, "Period":U)                              
    oControl                                   = goCntTypeControl:addControl("fcPeriodType":U          + goCntTypeControl:ContainerCode, "wsCombo":U,          "8":U,   "tt_auth_type_control.period_type":U,                "character":U, 23, "Period<br>Type":U)                  
    oControl:RenderProcedure                   = "RenderProcedure":U                                                                                                                                                                                                                    
    oControl:RenderArgument                    = "AcronymSelect:ma_acAuthPeriod:=":U                                                                                                                                                                                                    
    oControl                                   = goCntTypeControl:addControl("fcPeriodOverride":U      + goCntTypeControl:ContainerCode, "wsCheckBox":U,       "1":U,   "tt_auth_type_control.period_override":U,            "character":U, 24, "Period<br>Override":U)                  
                                                                                                                                                                                                                                                                                        
    oControl                                   = goCntTypeControl:addControl("fcRestrictionObjs":U     + goCntTypeControl:ContainerCode, "wsInput":U,          "10":U,  "":U,                                                "character":U, 25, "Authorisation<br>Type<br>Restrictions":U)                                           
    /* Displays [MULTIPLE SELECTED] on the Restrictions field after selecting multiple auth types. */
    oControl:JavaScriptOnChange                = 'if(this.value!="")~{var x = "fcRestrictions' + goCntTypeControl:ContainerCode + '" + this.name.replace("fcRestrictionObjs' + goCntTypeControl:ContainerCode + '","");'                            
                                               + ' fnGetControls(x)[0].value="~[MULTIPLE SELECTED~]"~}'                                                                                                                                                             
    oControl:ControlToken                      = "Hidden":U                                                                                                                                                                                                         
    oControl                                   = goCntTypeControl:addControl("fcRestrictions":U        + goCntTypeControl:ContainerCode, "wsTextArea":U,       "10":U,  "tt_auth_type_control.auth_type_restrictions":U,     "character":U, 25, "Authorisation<br>Type<br>Restrictions":U)           
    oControl                                   = goCntTypeControl:addControl("fcRestrictionMsc":U      + goCntTypeControl:ContainerCode, "wsMsComboButton":U,  "":U,    "":U,                                                "":U,          25, "Authorisation<br>Type<br>Restrictions")                                                                 
    oControl:LookupWobFLA                      = "mahacat":U                                                                                                                                                                                                        
    oControl:LookupFields                      = "hac_auth_type.auth_type_obj":U                                                                                                                                                                                    
    oControl:LookupControls                    = "fcRestrictionObjs" + goCntTypeControl:ContainerCode                                                                                                                                                               
    oControl:LookupFields                      = "hac_auth_type.auth_type_obj":U                                                                                                                                                                                    
    oControl:LookupControls                    = "fcRestrictionObjs" + goCntTypeControl:ContainerCode                                                                                                                                                               
    oControl:CellLayoutMask                    = "&1&2&3"
       
    oControl                                   = goCntTypeControl:addControl("fcQuantity":U            + goCntTypeControl:ContainerCode, "wsInput":U,           "5":U,  "tt_auth_type_control.quantity_auth":U,              "integer":U,   27, "Quantity<br>Authorised":U)
    oControl                                   = goCntTypeControl:addControl("fcAmount":U              + goCntTypeControl:ContainerCode, "wsInput":U,           "5":U,  "tt_auth_type_control.amount_auth":U,                "decimal":U,   28, "Amount<br>Authorised":U)                              
    oControl                                   = goCntTypeControl:addControl("fcActivateAuthorised":U  + goCntTypeControl:ContainerCode, "wsCheckBox":U,        "8":U,  "tt_auth_type_control.activate_authorised_values":U, "character":U, 29, "Activate<br>Authorised<br>Values":U)                              
    oControl                                   = goCntTypeControl:addControl("fcEnforceAthorised":U    + goCntTypeControl:ContainerCode, "wsCheckBox":U,        "8":U,  "tt_auth_type_control.enforce_authorised_values":U,  "character":U, 30, "Enforce<br>Authorised<br>Values":U)   

    
    oControl                                   = goCntTypeControl:addControl("fcGender":U              + goCntTypeControl:ContainerCode, "wsCombo":U,           "10":U, "tt_auth_type_control.gender":U,             "character":U,      31, "Valid<br>Gender":U)
    oControl:AdditionalItems                   = "=|Both=B|Male=M|Female=F":U
    oControl:JavascriptOnChange                = "fnEnableDisableFieldControl(this, ~"" + cGenderFieldChange + "~", true, true);":U

    // Male Controls
    oControl                                   = goCntTypeControl:addControl("fdAgeRangeMaleObj":U     + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "tt_auth_type_control.age_range_male_obj":U,             "decimal":U,      32, "":U)
    oControl:ControlToken                      = "Hidden":U 
    oControl                                   = goCntTypeControl:addControl("fcAgeRangeMale":U        + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "male_age_range.age_range_code":U,             "character":U,      32, "":U)
    oControl:AjaxValidation                    = "START:" + Warpspeed:CurrentWob + ":ajaxValidationAuthType:hlmar":U                                                                  
    oControl:FilterFields                      = "[AgeRange]":U                                                                                                              
    oControl:FilterControls                    = "fcAgeRangeMale":U + goCntTypeControl:ContainerCode                                                                                                         
    oControl:ReturnFields                      = "[RecordObj],[RecordCode],[RecordLabel]":U                                                                                   
    oControl:ReturnControls                    = "fdAgeRangeMaleObj":U  + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeMale":U + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeMaleDesc":U + goCntTypeControl:ContainerCode  
    oControl:JavascriptOnChange                = "fnDeactivateTextFields(this);"
    oControl                                   = goCntTypeControl:addControl("buAgeRangeMaleBtn":U     + goCntTypeControl:ContainerCode, "wsLookupButton":U,    "10":U, "":U,             "":U,               32, "Valid<br>Age Range<br>Male":U)
    oControl:CelllayoutMask                    = "&1&2&3":U
    oControl:LookupWobFLA                      = "mahlmar":U
    oControl:LookupFields                      = "hlm_age_range.age_range_obj":U  
    oControl:LookupControls                    = "fdAgeRangeMaleObj":U  + goCntTypeControl:ContainerCode           
    oControl:FilterFields                      = "hlm_age_range.age_range_code":U  
    oControl:FilterControls                    = "fcAgeRangeMale":U + goCntTypeControl:ContainerCode             
    oControl:ReturnFields                      = "hlm_age_range.age_range_obj,hlm_age_range.age_range_code,hlm_age_range.age_range_description":U                                                 
    oControl:ReturnControls                    = "fdAgeRangeMaleObj":U  + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeMale":U + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeMaleDesc":U + goCntTypeControl:ContainerCode 
    oControl                                   = goCntTypeControl:addControl("fcAgeRangeMaleDesc":U    + goCntTypeControl:ContainerCode, "wsTextArea":U,        "10":U, "male_age_range.age_range_description":U,             "":U,     33)
    oControl:ControlToken                      = "Disabled":U 

    // Female Controls
    oControl                                   = goCntTypeControl:addControl("fdAgeRangeFemaleObj":U   + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "tt_auth_type_control.age_range_female_obj":U,             "decimal":U,      34, "":U)
    oControl:ControlToken                      = "Hidden":U 
    oControl                                   = goCntTypeControl:addControl("fcAgeRangeFemale":U      + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "female_age_range.age_range_code":U,             "character":U,      34, "":U)
    oControl:AjaxValidation                    = "START:" + Warpspeed:CurrentWob + ":ajaxValidationAuthType:hlmar":U                                                                  
    oControl:FilterFields                      = "[AgeRange]":U                                                                                                              
    oControl:FilterControls                    = "fcAgeRangeFemale":U + goCntTypeControl:ContainerCode                                                                                                         
    oControl:ReturnFields                      = "[RecordObj],[RecordCode],[RecordLabel]":U                                                                                   
    oControl:ReturnControls                    = "fdAgeRangeFemaleObj":U  + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeFemale":U + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeFemaleDesc":U + goCntTypeControl:ContainerCode
    oControl:JavascriptOnChange                = "fnDeactivateTextFields(this);"
    oControl                                   = goCntTypeControl:addControl("buAgeRangeFemaleBtn":U   + goCntTypeControl:ContainerCode, "wsLookupButton":U,    "10":U, "":U,             "":U,               34, "Valid<br>Age Range<br>Female":U)
    oControl:CelllayoutMask                    = "&1&2&3":U
    oControl:LookupWobFLA                      = "mahlmar":U
    oControl:LookupFields                      = "hlm_age_range.age_range_obj":U  
    oControl:LookupControls                    = "fdAgeRangeFemaleObj":U  + goCntTypeControl:ContainerCode           
    oControl:FilterFields                      = "hlm_age_range.age_range_code":U  
    oControl:FilterControls                    = "fcAgeRangeFemale":U + goCntTypeControl:ContainerCode             
    oControl:ReturnFields                      = "hlm_age_range.age_range_obj,hlm_age_range.age_range_code,hlm_age_range.age_range_description":U                                                 
    oControl:ReturnControls                    = "fdAgeRangeFemaleObj":U  + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeFemale":U + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeFemaleDesc":U + goCntTypeControl:ContainerCode
    oControl                                   = goCntTypeControl:addControl("fcAgeRangeFemaleDesc":U  + goCntTypeControl:ContainerCode, "wsTextArea":U,        "10":U, "female_age_range.age_range_description":U,             "":U,       35)
    oControl:ControlToken                      = "Disabled":U 

    // Both Controls
    oControl                                   = goCntTypeControl:addControl("fdAgeRangeBothObj":U     + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "tt_auth_type_control.age_range_both_obj":U,             "decimal":U,      36, "":U)
    oControl:ControlToken                      = "Hidden":U 
    oControl                                   = goCntTypeControl:addControl("fcAgeRangeBoth":U        + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "both_age_range.age_range_code":U,             "character":U,      36, "":U)
    oControl:AjaxValidation                    = "START:" + Warpspeed:CurrentWob + ":ajaxValidationAuthType:hlmar":U                                                                  
    oControl:FilterFields                      = "[AgeRange]":U                                                                                                              
    oControl:FilterControls                    = "fcAgeRangeBoth":U + goCntTypeControl:ContainerCode                                                                                                         
    oControl:ReturnFields                      = "[RecordObj],[RecordCode],[RecordLabel]":U                                                                                   
    oControl:ReturnControls                    = "fdAgeRangeBothObj":U  + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeBoth":U + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeBothDesc":U + goCntTypeControl:ContainerCode 
    oControl:JavascriptOnChange                = "fnDeactivateTextFields(this);"
    oControl                                   = goCntTypeControl:addControl("buAgeRangeBothBtn":U     + goCntTypeControl:ContainerCode, "wsLookupButton":U,    "10":U, "":U,             "":U,               36, "Valid<br>Age Range<br>Both":U)
    oControl:CelllayoutMask                    = "&1&2&3":U
    oControl:LookupWobFLA                      = "mahlmar":U
    oControl:LookupFields                      = "hlm_age_range.age_range_obj":U  
    oControl:LookupControls                    = "fdAgeRangeBothObj":U  + goCntTypeControl:ContainerCode           
    oControl:FilterFields                      = "hlm_age_range.age_range_code":U  
    oControl:FilterControls                    = "fcAgeRangeBoth":U + goCntTypeControl:ContainerCode             
    oControl:ReturnFields                      = "hlm_age_range.age_range_obj,hlm_age_range.age_range_code,hlm_age_range.age_range_description":U                                                 
    oControl:ReturnControls                    = "fdAgeRangeBothObj":U  + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeBoth":U + goCntTypeControl:ContainerCode + ",":U
                                               + "fcAgeRangeBothDesc":U + goCntTypeControl:ContainerCode
    oControl                                   = goCntTypeControl:addControl("fcAgeRangeBothDesc":U    + goCntTypeControl:ContainerCode, "wsTextArea":U,        "10":U, "both_age_range.age_range_description":U,             "":U,       37)
    oControl:ControlToken                      = "Disabled":U 

    oControl                                   = goCntTypeControl:addControl("fcDefaultAuthStatus":U     + goCntTypeControl:ContainerCode, "wsCombo":U,           "10":U, "tt_auth_type_control.default_auth_status":U,             "character":U,      38, "Exclusion<br>Authorisation<br>Status":U) 
    oControl:ControlClass                      = "+clAuthService":U 
    oControl:AdditionalItems                   = "|Declined=6":U
    oControl:JavascriptOnChange                = "fnPreventUserSet(this, ~"val=6~", ~"fcControlIndicatorType~", ~"Excl~");":U
                                               + "fnReasonTypeArgUpdate(this);"
                                    
    oControl                                   = goCntTypeControl:addControl("fcReasonKey":U          + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "tt_auth_type_control.default_auth_status_note":U,             "character":U,      39, "Exclusion<br>Status<br>Reason":U)             
    oControl:ControlClass                      = "+clAuthService":U 
    oControl:AjaxValidation                    = "START:" + Warpspeed:CurrentWob + ":ajaxValidationAuthType:note":U
    oControl:FilterFields                      = "[NoteKey]":U                                                                                                              
    oControl:FilterControls                    = "fcReasonKey":U + goCntTypeControl:ContainerCode
    oControl:ReturnFields                      = "[RecordKey],[RecordLabel]":U                                                                                   
    oControl:ReturnControls                    = "fcReasonKey":U  + goCntTypeControl:ContainerCode + ",":U
                                               + "fcNarration":U  + goCntTypeControl:ContainerCode 
    oControl:JavascriptOnBlur                  = "fnMandatoryReleaseCheck(this);":U
    
    oControl                                   = goCntTypeControl:addControl("fcNarration":U          + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "":U,             "character":U,      39, "Exclusion<br>Status<br>Reason":U)             
    oControl:ControlToken                      = "Hidden":U
    oControl:JavascriptOnChange                = "fnChangeToolTip(this, ~"fcReasonKey~", ~"Please enter a valid status reason~");"

    oControl                                   = goCntTypeControl:addControl("fcReasonTypeArgument":U + goCntTypeControl:ContainerCode, "wsInput":U , "5":U , "":U , "CHARACTER":U, 39, "":U)
    oControl:ControlToken                      = "Hidden":U

    oControl                                   = goCntTypeControl:addControl("fcStatusReasonBtn":U       + goCntTypeControl:ContainerCode, "wsLookupButton":U,    "10":U, "":U,             "":U,               39, "Exclusion<br>Status<br>Reason":U)
    oControl:LookupWobFLA                      = "note":U
    oControl:LookupFields                      = "note.key":U
    oControl:LookupControls                    = "fcReasonKey":U + goCntTypeControl:ContainerCode
    oControl:FilterFields                      = "note.key,note.type":U
    oControl:FilterControls                    = "fcReasonKey":U + goCntTypeControl:ContainerCode + ",fcReasonTypeArgument":U + goCntTypeControl:ContainerCode
    oControl:ReturnFields                      = "note.key,note.narration[1]":U
    oControl:ReturnControls                    = "fcReasonKey":U + goCntTypeControl:ContainerCode + ",fcNarration":U + goCntTypeControl:ContainerCode
    oControl:CellLayoutMask                    = "&1&2&3&4":U 
    oControl:ControlToolTip                    = "Lookup valid status reason." 

    oControl                                   = goCntTypeControl:addControl("fcEffectDateHid":U          + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "":U,             "date":U,      40, "":U) 
    oControl:RenderProcedure                   = "customRenderProcedure":U
    oControl:RenderArgument                    = "DefaultHiddenDateSet":U
    oControl:ControlToken                      = "Hidden":U  

    oControl                                   = goCntTypeControl:addControl("fcEffectDate":U          + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "tt_auth_type_control.effective_date":U,             "date":U,      42, "Effective<br>Date":U)             
    oControl:ControlClass                      = "+clMan":U
    oControl:JavascriptOnChange                = "fnSetHiddenDate(this)"

    oControl                                   = goCntTypeControl:addControl("fcEndDate":U             + goCntTypeControl:ContainerCode, "wsInput":U,           "10":U, "tt_auth_type_control.end_date":U,                   "date":U,      43, "End<br>Date":U)                
    oControl                                   = goCntTypeControl:addControl("fcDetailObj":U           + goCntTypeControl:ContainerCode, "wsInput":U,           "8":U,  "tt_auth_type_control.auth_type_control_obj":U,      "character":U, 44, "Detail Obj":U)             
    oControl:ControlToken                      = "Hidden":U                                                                                                                                                                                                         
                                                                                                                                                                                                                                                                    
    oControl                                   = goCntTypeControl:addControl("fcAuthTypeObjArgument":U + goCntTypeControl:ContainerCode, "wsInput":U,           "8":U,  "tt_auth_type_control.auth_type_obj":U,              "character":U, 45, "Auth Type Obj":U)  
    oControl:ControlToken                      = "Hidden":U                                                                                                                                                                                                         
    oControl:ControlValue                      = goWob:CurrentObj    

    oControl                                   = goCntTypeControl:addControl("fcAuthTypeCounter":U + goCntTypeControl:ContainerCode, "wsInput":U,           "8":U,  "tt_sequence.sequence_value":U,              "integer":U, 46, "Auth Type Obj":U)  
    oControl:ControlToken                      = "Hidden":U .                                                                                                                                                                                                        
        
  ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(goCntTypeControl)
  
         oContainerProperties:DefaultLess            = TRUE
         oContainerProperties:CollapsableControlList = "fcClaimCodeDis":U      + goCntTypeControl:ContainerCode + ",":U + "fcClaimTypeDis":U       + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcUsagePeriod":U       + goCntTypeControl:ContainerCode + ",":U + "fcUsagePeriodType":U    + goCntTypeControl:ContainerCode + ",":U + 
                                                       "fcUsagePeriodOver":U   + goCntTypeControl:ContainerCode + ",":U + "fcUserMsc":U            + goCntTypeControl:ContainerCode + ",":U + 
                                                       "fcPeriod":U            + goCntTypeControl:ContainerCode + ",":U + "fcPeriodType":U         + goCntTypeControl:ContainerCode + ",":U + 
                                                       "fcPeriodOverride":U    + goCntTypeControl:ContainerCode + ",":U + "fcRestrictions":U       + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcRestrictionMsc":U    + goCntTypeControl:ContainerCode + ",":U + "fcQuantity":U           + goCntTypeControl:ContainerCode + ",":U + 
                                                       "fcAmount":U            + goCntTypeControl:ContainerCode + ",":U + "fcActivateAuthorised":U + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcEnforceAthorised":U  + goCntTypeControl:ContainerCode + ",":U + "fcUsageType":U          + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcUsageQuantity":U     + goCntTypeControl:ContainerCode + ",":U + "fcGender":U             + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcAgeRangeMale":U      + goCntTypeControl:ContainerCode + ",":U + "buAgeRangeMaleBtn":U    + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcAgeRangeMaleDesc":U  + goCntTypeControl:ContainerCode + ",":U + "fcAgeRangeFemale":U     + goCntTypeControl:ContainerCode + ",":U +
                                                       "buAgeRangeFemaleBtn":U + goCntTypeControl:ContainerCode + ",":U + "fcAgeRangeFemaleDesc":U + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcAgeRangeBoth":U      + goCntTypeControl:ContainerCode + ",":U + "buAgeRangeBothBtn":U    + goCntTypeControl:ContainerCode + ",":U +
                                                       "fcAgeRangeBothDesc":U  + goCntTypeControl:ContainerCode.  
         
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE  &Container = goCntTypeControl &ContainerProperties = oContainerProperties } 
  
  { mip/inc/mipcatcherror.i }
