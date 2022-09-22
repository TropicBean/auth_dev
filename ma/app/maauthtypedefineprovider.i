/* maauthtypedefineprovider.i MEDSTAR Medical Aid System
                            Save Authorisation Detail Record
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
                            
   Note: If you change any of the headings on the controls, please make sure
          you change the label accordingly in maauthtypeprovidertt.i.
*/   

DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.

DEFINE VARIABLE cControlIndicatorTypeFieldChange AS CHARACTER NO-UNDO.
DEFINE VARIABLE cControlMakeFieldsMandatory      AS CHARACTER NO-UNDO.

   ASSIGN cControlIndicatorTypeFieldChange = "ma_acAuthProviderTypeIndicatorDef=fcDefaultStatus,fcReasonKey,buReasonLkp|ma_acAuthProviderTypeIndicatorExcl=fcProviderType,fcAuthorisedService,fcMainProvider,":U
                                           + "fcMandatory,fcClaimCodesProvider,fcClaimCodesProvLkp,fcClaimTypesCombo,fcDefaultClaimCodeDetail,fcClaimCodesDetLkp,fcDefaultClaimTypeDetail,":U
                                           + "fcClaimCodesDisallow,fcClaimCodesDisLkp,fcClaimTypesDisallowCombo,fcValidClaimCodesDetail,fcValidClaimCodesDetailLkp,fcValidClaimTypesDetailCombo,fcHeaderValuesAllowed,":U
                                           + "fcHeaderValuesUnlimited,fcAuthDetaiLines,fiQuantityAuth,fdAuthorisedAmount,fcStatusUsers,fcStatusUsersLkp,fcStatusRoles,fcStatusRolesLkp,fcBaseRateUpdUser,":U
                                           + "fcBaseRateUpdUserLkp,fcBaseRateUpdRole,fcBaseRateUpdRoleLkp,fcArsRateUpdUser,fcArsRateUpdUserLkp,fcArsRateUpdRole,fcArsRateUpdRoleLkp":U
          cControlMakeFieldsMandatory      = "ma_acAuthProviderTypeIndicatorDef=|ma_acAuthProviderTypeIndicatorExcl=fcReasonKey":U.

   ASSIGN                                                                                                                                                                             
    goCntAuthTypeProvider                           = NEW cls.mipwscontainer("AuthTypeProvider":U, "100%":U, "Authorisation Type Provider Controls":U, WarpSpeed:BaseClass, TRUE)  
    goCntAuthTypeProvider:ContainerMode             = goWob:SubmitValue                                                                                                                
    goCntAuthTypeProvider:ViewOnly                  = FALSE                                                                                                                                
    goCntAuthTypeProvider:ShowGenericReportPrint    = TRUE                                                                                                                                 
    goCntAuthTypeProvider:Collapsed                 = FALSE                                                                                                                              
    goCntAuthTypeProvider:RowsToRender              = ? 
    goCntAuthTypeProvider:RowRenderProcedure        = "rowRenderProcedure":U
    goCntAuthTypeProvider:RowRenderArgument         = goCntAuthTypeProvider:ContainerCode
    goCntAuthTypeProvider:ContainerHidden           = FALSE

    goCntAuthTypeProvider:QueryString               = "FOR EACH tt_auth_type_provider":U
                                                    + ",  FIRST tt_sequence no-lock":U
                                                    + "   where tt_sequence.sequence_link_obj = tt_auth_type_provider.auth_type_provider_obj":U
                                                    + "   BY tt_sequence.sequence_value DESCENDING ":U                    
    goCntAuthTypeProvider:QueryBufferList           = STRING(TEMP-TABLE tt_auth_type_provider:DEFAULT-BUFFER-HANDLE) + ",":U
                                                    + STRING(TEMP-TABLE tt_sequence:DEFAULT-BUFFER-HANDLE)  


    oControl                        = goCntAuthTypeProvider:addControl("buUpdate":U               + goCntAuthTypeProvider:ContainerCode, "wsImage":U,  "8":U, "":U,              "character":U, 2, "":U)    
    oControl:ControlToolTip         = "Modify Record":U
    oControl:RenderProcedure        = "WebRenderProcedure":U
    oControl:RenderArgument         = "ModifyButtonRender":U

    oControl                        = goCntAuthTypeProvider:addControl("fcInsurer":U               + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,  "8":U, "tt_auth_type_provider.insurer_obj":U,              "character":U, 3, "Client":U)                              
    oControl:AdditionalItems        = "="                                                                                                                                                                                                            
    oControl:QueryString            = "FOR EACH erm_insurer NO-LOCK ":U
    oControl:KeyField               = "erm_insurer.insurer_obj":U
    oControl:DisplayFields          = "erm_insurer.insurer_name":U
    oControl:RenderProcedure        = "WebRenderProcedure":U
    oControl:RenderArgument         = "KeyField":U


    oControl                        = goCntAuthTypeProvider:addControl("fcOption":U                + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,   "3":U, "tt_auth_type_provider.option_code":U,             "character":U, 4, "Option":U)                 
    oControl:AdditionalItems        = "0 - All =0":U
    oControl:QueryString            = "FOR EACH scheme NO-LOCK WHERE scheme.active = yes":U
    oControl:KeyField               = "scheme.scheme-code":U
    oControl:DisplayFields          = "scheme.scheme-code,scheme.short-name":U
    oControl:DisplayMask            = "&1 - &2"                                                                                                                                                                  
    oControl:ControlClass           = "+clMan":U
    oControl:RenderProcedure        = "WebRenderProcedure":U
    oControl:RenderArgument         = "KeyField":U
  
    oControl                        = goCntAuthTypeProvider:addControl("fcomProviderTypeIndicator":U  + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,  "8":U, "tt_auth_type_provider.provider_type_indicator":U, "character":U, 5, "Provider<br>Type<br>Indicator":U)                    
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK":U
                                    + "   WHERE mic_acronym.category_key = 'ma_acAuthProviderTypeIndicator'":U
                                    + "      BY mic_acronym.acronym_label":U 
    oControl:KeyField               = "mic_acronym.acronym_key":U
    oControl:DisplayFields          = "mic_acronym.acronym_label":U
    oControl:JavascriptOnChange     = "fnEnableDisableFieldControl(this, ~"" + cControlIndicatorTypeFieldChange + "~"); fnMandatoryFieldToggleControl(this, ~"" + cControlMakeFieldsMandatory + "~"); fnIndicatorChanged(this);":U
    oControl:ControlClass           = "+clMan":U
    oControl:RenderProcedure        = "WebRenderProcedure":U
    oControl:RenderArgument         = "KeyField":U 
         
    oControl                        = goCntAuthTypeProvider:addControl("fcProviderType":U          + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,  "8":U, "tt_auth_type_provider.provider_type":U,            "character":U, 6, "Provider<br>Type":U)                    
    oControl:AdditionalItems        = "=":U
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK":U    
                                    + "   WHERE mic_acronym.category_key = 'ma_acAuthProviderType'":U
                                    + "      BY mic_acronym.acronym_label":U 
    oControl:KeyField               = "mic_acronym.acronym_key":U
    oControl:DisplayFields          = "mic_acronym.acronym_label":U
    oControl:RenderProcedure        = "WebRenderProcedure":U
    oControl:RenderArgument         = "KeyField":U
                                          
    oControl                        = goCntAuthTypeProvider:addControl("fiProviderSeq":U        + goCntAuthTypeProvider:ContainerCode, "wsInput":U   , "8":U , "tt_auth_type_provider.provider_sequence":U       , "integer":U,   7, "Provider<br>Sequence":U)                              
    oControl:ControlClass           = "+clMan":U    
    oControl:RenderProcedure        = "customRenderProcedure":U
    oControl:RenderArgument         = "TrimInt":U
                                                                                                                                                                 
    oControl                        = goCntAuthTypeProvider:addControl("fcAuthorisedService":U  + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U, "8":U , "tt_auth_type_provider.authorised_service":U      , "character":U, 8, "Authorised<br>Service":U)                              
    ocOntrol:JavascriptOnChange     = "fnAuthorisedService(this);":U
    oControl:ControlClass           = "+clUserAccess +clAuthServe":U
    
    oControl                        = goCntAuthTypeProvider:addControl("fcNegGroup":U            + goCntAuthTypeProvider:ContainerCode, "wsInput":U, "6":U , "tt_auth_type_provider.neg_num":U            , "character":U, 9, "Negotiation<br>Group":U)  
    //oControl:AjaxValidation         = "START:" + goWob:ObjectCode + ":ajaxValidation:neggroup":U                                                                  
    oControl:FilterFields           = "[fiNegNum]":U                                                                                                              
    oControl:FilterControls         = "fiNegNum":U + goCntAuthTypeProvider:ContainerCode                                                                                                         
    oControl:ReturnFields           = "[RecordCode]":U                                                                                   
    oControl:ReturnControls         = "fiNegNum":U + goCntAuthTypeProvider:ContainerCode
    oControl                        = goCntAuthTypeProvider:addControl("buNegGroupLkp":U             + goCntAuthTypeProvider:ContainerCode, "wsLookupButton":U, "10":U, "":U,                                               "":U,          9, "Negotiation<br>Group":U)
    oControl:LookupWobFLA           = "mangg":U
    oControl:LookupFields           = "neggroup.neg-num":U
    oControl:LookupControls         = "fcNegGroup":U + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "neggroup.neg-num":U
    oControl:FilterControls         = "fcNegGroup":U + goCntAuthTypeProvider:ContainerCode
    oControl:ReturnFields           = "neggroup.neg-num":U
    oControl:ReturnControls         = "fcNegGroup":U + goCntAuthTypeProvider:ContainerCode
    oControl:CellLayoutMask         = "&1&2"

    oControl                        = goCntAuthTypeProvider:addControl("fcAuthGroup":U            + goCntAuthTypeProvider:ContainerCode, "wsCombo":U, "6":U , "tt_auth_type_provider.auth_group_obj":U            , "character":U, 10, "Authorisation<br>Group":U)    
    oControl:AdditionalItems        = "="                                                                                                                                                                                                            
    oControl:QueryString            = "FOR EACH ham_auth_group NO-LOCK ":U
    oControl:KeyField               = "ham_auth_group.auth_group_obj":U
    oControl:DisplayFields          = "ham_auth_group.auth_group_code":U
    oControl:ControlTooltip         = "Groups the Providers and Clinical Detail Lines to a specific Authorisation Group.":U

    oControl                        = goCntAuthTypeProvider:addControl("fcPrTypes":U            + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "6":U , "tt_auth_type_provider.pr_type_list":U            , "character":U, 11, "":U)           
    oControl:RenderProcedure        = "WebRenderProcedure":U
    oControl:RenderArgument         = "KeyField":U 
    oControl                        = goCntAuthTypeProvider:addControl("fcPrTypeLkp":U          + goCntAuthTypeProvider:ContainerCode, "wsButton":U  , "":U  , "":U                                              , "":U,          11, "Practice<br>Types")                             
    oControl:RenderProcedure        = "customRenderProcedure":U
    oControl:RenderArgument         = "PRTYPE":U
    oControl:CellLayoutMask         = "&1&2":U

    oControl                        = goCntAuthTypeProvider:addControl("fcPrTypeValidList":U    + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "6":U , "tt_auth_type_provider.pr_type_valid_list":U      , "character":U, 12, "Valid<br>Practice<br>Types":U)           
    oControl:RenderProcedure        = "WebRenderProcedure":U
    oControl:RenderArgument         = "KeyField":U
    oControl                        = goCntAuthTypeProvider:addControl("fcExclPrTypeLkp":U      + goCntAuthTypeProvider:ContainerCode, "wsButton":U  , "":U  , "":U                                              , "":U         , 12, "Valid<br>Practice<br>Types")                                                                                       
    oControl:RenderProcedure        = "customRenderProcedure":U
    oControl:RenderArgument         = "PRTYPE":U
    oControl:CellLayoutMask         = "&1&2":U
    
    oControl                        = goCntAuthTypeProvider:addControl("fcMainProvider":U       + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U, "8":U , "tt_auth_type_provider.main_provider":U           , "logical":U  , 13, "Main<br>Provider":U)                              
    oControl:ControlClass           = "+clUserAccess +clAuthService":U
    
    oControl                        = goCntAuthTypeProvider:addControl("fcMandatory":U          + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U, "8":U , "tt_auth_type_provider.mandatory":U               , "character":U, 14, "Provider<br>Type<br>Mandatory":U)  

    oControl                        = goCntAuthTypeProvider:addControl("fcAuthAutoCreate":U     + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U, "8":U , "tt_auth_type_provider.auth_auto_create":U        , "character":U, 15, "Auto<br>Create":U)
    oControl:ControlTooltip         = "Specify whether the default Provider should be created on the Authorisation when defaults are populated on the Authorisation.":U

    oControl                        = goCntAuthTypeProvider:addControl("fcNumProviderAllowed":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U   , "8":U , "tt_auth_type_provider.number_providers_allowed":U, "integer":U,   16, "Number<br>Providers<br>Allowed":U)                              
    oControl:ControlClass           = "+clMan":U         
    oControl:ControlFormat          = "":U
    oControl:RenderProcedure        = "customRenderProcedure":U
    oControl:RenderArgument         = "TrimInt":U
                                                                                                    
    oControl                        = goCntAuthTypeProvider:addControl("fcDocNumMandatory":U    + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U, "8":U , "tt_auth_type_provider.doc_num_mandatory":U       , "character":U, 17, "Provider<br>Number<br>Mandatory":U)                              
    
    oControl                        = goCntAuthTypeProvider:addControl("ftEffectiveDate":U      + goCntAuthTypeProvider:ContainerCode, "wsInput":U,    "10":U, "tt_auth_type_provider.effective_date":U,          "date":U,       18, "Effective<br>Date":U)                              
    oControl:ControlClass           = "+clMan":U 
    //oControl:RenderProcedure        = "customRenderProcedure":U
    //oControl:RenderArgument         = "DefEffectiveDate":U

    oControl                        = goCntAuthTypeProvider:addControl("ftEndDate":U            + goCntAuthTypeProvider:ContainerCode, "wsInput":U,  "10":U, "tt_auth_type_provider.end_date":U,                   "date":U,      19, "End<br>Date":U)                                                                                                                                                                                                                                                                                                               

    oControl                        = goCntAuthTypeProvider:addControl("fcDumy":U               + goCntAuthTypeProvider:ContainerCode, "wsInput":U   , "8":U , "":U                                              , "integer":U  , 20, "":U)                              
    oControl:ControlToken           = "HIDDEN":U
    oControl                        = goCntAuthTypeProvider:addControl("fcClaimCodesProviderHid":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U, "8":U , "":U                                              , "integer":U  , 20, "":U)                              
    oControl:ControlToken           = "HIDDEN":U                                                                                                                                                                                   
    /* Sets Claim code to an empty field instead of displaying 000. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnGetControls("fcClaimCodesProvider'    + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcClaimCodesProviderHid' + goCntAuthTypeProvider:ContainerCode + '", ""))[0].value = this.value; this.value="" ~}'  
    
    oControl                        = goCntAuthTypeProvider:addControl("fcClaimCodesProvider":U    + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "10":U, "tt_auth_type_provider.claim_codes_provider":U , "character":U, 20, "Claim<br>Codes":U)                              
    oControl:ControlClass           = "+clAuthService":U    

    oControl                        = goCntAuthTypeProvider:addControl("fcClaimCodesProvLkp":U     + goCntAuthTypeProvider:ContainerCode, "wsMsComboButton":U, "":U, "":U, "":U, 20)                              
    oControl:ControlClass           = "+clAuthService":U                                         
    oControl:LookupWobFLA           = "maclc":U
    oControl:LookupFields           = "ccdesc.claim-code":U
    oControl:LookupControls         = "fcClaimCodesProviderHid":U  + goCntAuthTypeProvider:ContainerCode
    oControl:FilterControls         = "ftEffectiveDate":U  + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "ccode.effect-date":U
    oControl:ReturnFields           = "ccdesc.claim-code"
    oControl:ReturnControls         = "fcDumy":U    + goCntAuthTypeProvider:ContainerCode 
    oControl:CellLayoutMask         = "&1&2&3&4".

  ASSIGN
    oControl                        = goCntAuthTypeProvider:addControl("fcEnforceCCMatch":U + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U,     "5":U, "tt_auth_type_provider.enforce_header_claim_code_match":U, "character":U, 21, "Enforce<br>CC Match":U)
    oControl:ControlTooltip         = "Specifies whether to enforce the Authorisation Provider Claim Code to match the Authorisation Header Claim Code.":U 

    oControl                        = goCntAuthTypeProvider:addControl("fcClaimTypesCombo":U + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U,     "10":U, "tt_auth_type_provider.claim_types_provider":U, "character":U, 22, "Claim<br>Types":U)                              
    //oControl:AdditionalItems        = gcClaimType
    oControl:ControlTooltip         = "C=consultation, O=other, N=non eligible, A=acute, K=Chronic":U 
    oControl:ControlJavaScript      = "multiple=~"multiple~"":U
    oControl:ControlClass           = "+clUserAccess +clAuthService":U 
   
    oControl                        = goCntAuthTypeProvider:addControl("fcEnforceCTMatch":U + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U,     "5":U, "tt_auth_type_provider.enforce_header_claim_type_match":U, "character":U, 23, "Enforce<br>CT Match":U)
    oControl:ControlTooltip         = "Specifies whether to enforce the Authorisation Provider Claim Type to match the Authorisation Header Claim Type.":U 

    oControl                        = goCntAuthTypeProvider:addControl("fcDefaultClaimCodeDetailHid":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U,     "8":U , "":U,                                        "integer":U  , 24, "":U)                              
    oControl:ControlToken           = "HIDDEN":U

    oControl                        = goCntAuthTypeProvider:addControl("fcDefaultClaimCodeDetail":U    + goCntAuthTypeProvider:ContainerCode, "wsInput":U,     "8":U, "tt_auth_type_provider.default_claim_code_detail":U, "character":U, 24, "Default<br>Claim Code<br>Detail":U)                              
    oControl:RenderProcedure        = "customRenderProcedure":U
    oControl:RenderArgument         = "ClaimCode":U
    oControl:ControlFormat          = "999":U
    oControl:ControlClass           = "+clNumericOnly +clAuthService":U 
    oControl                        = goCntAuthTypeProvider:addControl("fcClaimCodesDetLkp":U   + goCntAuthTypeProvider:ContainerCode, "wsLookupButton":U,     "":U, "":U, "":U, 24)  
    oControl:ControlClass           = "+clAuthService":U  
    oControl:LookupWobFLA           = "maclc":U                                                                                                                                               
    oControl:LookupFields           = "ccdesc.claim-code":U                                                                                                                                        
    oControl:LookupControls         = "fcDefaultClaimCodeDetailHid":U + goCntAuthTypeProvider:ContainerCode                                                                                                                          
    oControl:FilterFields           = "ccdesc.claim-code,ccdesc.scheme-code":U                                                                                                                                        
    oControl:FilterControls         = "fcDefaultClaimCodeDetail":U + goCntAuthTypeProvider:ContainerCode
                                    + ",fcOption":U         + goCntAuthTypeProvider:ContainerCode             
    oControl:ReturnFields           = "ccdesc.claim-code":U                                                                                                                                        
    oControl:ReturnControls         = "fcDefaultClaimCodeDetail":U + goCntAuthTypeProvider:ContainerCode
    oControl:CellLayoutMask         = "&1&2&3"
    
    oControl                        = goCntAuthTypeProvider:addControl("fcDefaultClaimTypeDetail":U    + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,       "8":U, "tt_auth_type_provider.default_claim_type_detail":U, "character":U, 25, "Default<br>Claim Type<br>Detail":U)                              
    oControl:AdditionalItems        = gcClaimType 
    oControl:ControlClass           = "+clAuthService":U  
    oControl:ControlTooltip         = "Please enter a valid default claim type for the clinical detail records":U

    oControl                        = goCntAuthTypeProvider:addControl("fcDummy":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U   , "8":U , "":U, "integer":U  , 26, "":U)                              
    oControl:ControlToken           = "HIDDEN":U
    
    oControl                        = goCntAuthTypeProvider:addControl("fcClaimCodesDisallowHid":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U   , "8":U , "":U, "integer":U  , 26, "":U)                              
    oControl:ControlToken           = "HIDDEN":U
    /* Sets claim code disallow to an empty field instead of displaying 000. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnGetControls("fcClaimCodesDisallow'    + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcClaimCodesDisallowHid' + goCntAuthTypeProvider:ContainerCode + '", ""))[0].value = this.value; this.value="" ~}'  
    oControl                        = goCntAuthTypeProvider:addControl("fcClaimCodesDisallow":U    + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "10":U, "tt_auth_type_provider.claim_codes_disallow":U, "character":U, 26, "Claim<br>Codes<br>Disallow":U)                              
    oControl:ControlClass           = "+clUserAccess +clAuthService":U  
    oControl                        = goCntAuthTypeProvider:addControl("fcClaimCodesDisLkp":U      + goCntAuthTypeProvider:ContainerCode, "wsMsComboButton":U,     "":U, "":U, "":U, 26)                              
    oControl:LookupWobFLA           = "maclc":U
    oControl:LookupFields           = "ccdesc.claim-code":U
    oControl:LookupControls         = "fcClaimCodesDisallowHid":U + goCntAuthTypeProvider:ContainerCode
    oControl:FilterControls         = "ftEffectiveDate":U  + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "ccode.effect-date":U
    oControl:ReturnFields           = "ccdesc.claim-code"
    oControl:ReturnControls         = "fcDummy":U + goCntAuthTypeProvider:ContainerCode  
    oControl:CellLayoutMask         = "&1&2&3&4"

    oControl                        = goCntAuthTypeProvider:addControl("fcClaimTypesDisallowCombo":U + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U,     "10":U, "tt_auth_type_provider.claim_types_disallow":U, "character":U, 27, "Claim<br>Types<br>Disallow":U)                              
  //  oControl:AdditionalItems        = gcClaimType  
    oControl:ControlJavaScript      = "multiple=~"multiple~"":U
    oControl:ControlClass           = "+clUserAccess +clAuthService":U.

  ASSIGN
    oControl                        = goCntAuthTypeProvider:addControl("fcDummyValidClaimCodesDetail":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U   , "8":U , "":U, "integer":U  , 28, "":U)                              
    oControl:ControlToken           = "HIDDEN":U
    
    oControl                        = goCntAuthTypeProvider:addControl("fcValidClaimCodesDetailHid":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U   , "8":U , "":U, "integer":U  , 28, "":U)                              
    oControl:ControlToken           = "HIDDEN":U
    /* Sets claim code disallow to an empty field instead of displaying 000. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnGetControls("fcValidClaimCodesDetail'    + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcValidClaimCodesDetailHid' + goCntAuthTypeProvider:ContainerCode + '", ""))[0].value = this.value; this.value="" ~}'  
    oControl                        = goCntAuthTypeProvider:addControl("fcValidClaimCodesDetail":U    + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "10":U, "tt_auth_type_provider.valid_claim_codes_detail":U, "character":U, 28, "Valid<br>Claim Codes<br>Detail":U)                              
    oControl:ControlClass           = "+clUserAccess +clAuthService":U  
    oControl                        = goCntAuthTypeProvider:addControl("fcValidClaimCodesDetailLkp":U + goCntAuthTypeProvider:ContainerCode, "wsMsComboButton":U,     "":U, "":U, "":U, 28)                              
    oControl:LookupWobFLA           = "maclc":U
    oControl:LookupFields           = "ccdesc.claim-code":U
    oControl:LookupControls         = "fcValidClaimCodesDetailHid":U + goCntAuthTypeProvider:ContainerCode
    oControl:FilterControls         = "ftEffectiveDate":U  + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "ccode.effect-date":U
    oControl:ReturnFields           = "ccdesc.claim-code"
    oControl:ReturnControls         = "fcDummyValidClaimCodesDetail":U + goCntAuthTypeProvider:ContainerCode  
    oControl:CellLayoutMask         = "&1&2&3&4"

    oControl                        = goCntAuthTypeProvider:addControl("fcValidClaimTypesDetailCombo":U + goCntAuthTypeProvider:ContainerCode, "wsSelect":U,     "5":U, "tt_auth_type_provider.valid_claim_types_detail":U, "character":U, 29, "Valid<BR>Claim Types<BR>Detail":U)                              
    oControl:AdditionalItems        = "= |A=A|C=C|K=K|N=N|O=O|P=P":U
    oControl:ControlJavaScript      = "multiple=~"multiple~"":U
    oControl:ControlClass           = "+clUserAccess +clAuthService":U 

    oControl                        = goCntAuthTypeProvider:addControl("fcHeaderValuesAllowed":U   + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,  "8":U, "tt_auth_type_provider.header_values_allowed":U, "character":U, 30, "Header<br>Values<br>Allowed":U)                               
    oControl:AdditionalItems        = "=":U
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK":U    
                                    + "   WHERE mic_acronym.category_key = 'ma_acAuthHeadValAllowed'":U
                                    + "      BY mic_acronym.acronym_label":U 
    oControl:KeyField               = "mic_acronym.acronym_key":U                                
    oControl:DisplayFields          = "mic_acronym.acronym_label":U                                
    oControl:ControlClass           = "+clUserAccess +clAuthService":U
    
    oControl                        = goCntAuthTypeProvider:addControl("fcHeaderValuesUnlimited":U + goCntAuthTypeProvider:ContainerCode, "wsCheckBox":U, "8":U, "tt_auth_type_provider.header_values_unlimited":U, "character":U, 31, "Header<br>Value<br>Unlimited":U)                              
    oControl:ControlClass           = "+clUserAccess +clAuthService":U  
    
    oControl                        = goCntAuthTypeProvider:addControl("fcAuthDetaiLines":U        + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,    "8":U, "tt_auth_type_provider.authorise_detail_lines":U,  "character":U, 32, "Authorise<br>Detail<br>Lines":U)                              
    oControl:AdditionalItems        = "=":U
    oControl:QueryString            = "FOR EACH mic_acronym NO-LOCK":U    
                                    + "   WHERE mic_acronym.category_key = 'ma_acAuthTypeDetails'":U
                                    + "      BY mic_acronym.acronym_label":U 
    oControl:KeyField               = "mic_acronym.acronym_key":U                                
    oControl:DisplayFields          = "mic_acronym.acronym_label":U                                
    oControl:ControlClass           = "+clAuthService":U
    
    oControl                        = goCntAuthTypeProvider:addControl("fiQuantityAuth":U          + goCntAuthTypeProvider:ContainerCode, "wsInput":U,        "8":U,  "tt_auth_type_provider.quantity_auth":U,            "integer":U,   33, "Authorised<br>Quantity":U)                              
    oControl:ControlClass           = "+clAuthService +clUserAccess":U                                                                                                                                                    
    oControl                        = goCntAuthTypeProvider:addControl("fdAuthorisedAmount":U      + goCntAuthTypeProvider:ContainerCode, "wsInput":U,        "8":U,  "tt_auth_type_provider.amount_auth":U,              "decimal":U,   34, "Authorised<br>Amount":U)                              
    oControl:ControlClass           = "+clAuthService +clUserAccess":U                                                                                                                                                    
    oControl                        = goCntAuthTypeProvider:addControl("fcDefaultStatus":U         + goCntAuthTypeProvider:ContainerCode, "wsCombo":U,        "20":U, "tt_auth_type_provider.default_auth_status":U,      "character":U, 35, "Exclusion<br>Authorisation<br>Status":U)
    oControl:AdditionalItems        = "|Authorised=1|Cancelled=5|Declined=6|Requested=7|Pending=0":U                                                            
    oControl:ControlClass           = "+clUserAccess +clAuthService":U                                                                                        
    oControl                        = goCntAuthTypeProvider:addControl("fcReasonKey":U             + goCntAuthTypeProvider:ContainerCode, "wsInput":U,        "10":U, "tt_auth_type_provider.default_auth_status_note":U, "character":U, 36, "Exclusion<br>Status<br>Reason":U)
    oControl:ControlClass           = "+clAuthService":U  
    oControl                        = goCntAuthTypeProvider:addControl("buReasonLkp":U             + goCntAuthTypeProvider:ContainerCode, "wsLookupButton":U, "10":U, "":U,                                               "":U,          36, "Exclusion<br>Status<br>Reason":U)
    oControl:LookupWobFLA           = "note":U
    oControl:LookupFields           = "note.key":U
    oControl:LookupControls         = "fcReasonKey":U + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "note.type":U
    oControl:FilterControls         = "AS*":U
    oControl:ReturnFields           = "note.key":U
    oControl:ReturnControls         = "fcReasonKey":U + goCntAuthTypeProvider:ContainerCode
    oControl:CellLayoutMask         = "&1&2"

    oControl                        = goCntAuthTypeProvider:addControl("fcStatusUserObjs":U        + goCntAuthTypeProvider:ContainerCode  , "wsInput":U,      "20":U, "":U,                                                   "character":U,  37, "":U)
    oControl:CellLayoutMask         = "&1&2&3"
    oControl:ControlToken           = "Hidden"       
    /* Displays [MULTIPLE SELECTED] on the 'Status update user' field after selecting multiple users. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnGetControls("fcStatusUsers' + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcStatusUserObjs' + goCntAuthTypeProvider:ContainerCode + '", ""))[0].value="~[MULTIPLE SELECTED~]"~}' 
    oControl                        = goCntAuthTypeProvider:addControl("fcStatusUsers":U + goCntAuthTypeProvider:ContainerCode  ,           "wsTextArea":U,   "10":U, "tt_auth_type_provider.default_auth_status_upd_user":U, "character":U,  37,  "Status<br>Update<br>(Users)":U)
    oControl:ControlTooltip         = "Please enter a valid user(s) or enter None Allowed if no users are allowed.":U 
    oControl:ControlClass           = "+clAuthService":U  
    oControl                        = goCntAuthTypeProvider:addControl("fcStatusUsersLkp":U       + goCntAuthTypeProvider:ContainerCode  ,  "wsMsComboButton":U, "":U,  "":U,                                                 "":U,           37,  "Status<br>Update<br>(Users)")
    oControl:LookupWobFLA           = "mimus":U
    oControl:LookupFields           = "mim_user.user_obj":U
    oControl:LookupControls         = "fcStatusUserObjs" + goCntAuthTypeProvider:ContainerCode 
    oControl:FilterFields           = "mim_user.user_obj":U
    oControl:FilterControls         = "fcStatusUserObjs" + goCntAuthTypeProvider:ContainerCode 
    
    oControl                        = goCntAuthTypeProvider:addControl("fcStatusRoleObjs":U        + goCntAuthTypeProvider:ContainerCode  , "wsInput":U,         "20":U, "":U,                                                "character":U,  38, "":U)
    oControl:ControlToken           = "Hidden" 
    oControl:CellLayoutMask         = "&1&2&3"
    /* Displays [MULTIPLE SELECTED] on the 'Status update role' field after selecting multiple roles. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnGetControls("fcStatusRoles'    + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcStatusRoleObjs' + goCntAuthTypeProvider:ContainerCode + '", ""))[0].value="~[MULTIPLE SELECTED~]"~}'  
    oControl                        = goCntAuthTypeProvider:addControl("fcStatusRoles":U    + goCntAuthTypeProvider:ContainerCode  , "wsTextArea":U,          "10":U, "tt_auth_type_provider.default_auth_status_upd_role":U, "character":U,  38,  "Status<br>Update<br>(Roles)":U)
    oControl:ControlTooltip         = "Please enter a valid role(s) or enter None Allowed if no roles are allowed.":U 
    oControl:ControlClass           = "+clAuthService":U                                                                                                                                                       
    oControl                        = goCntAuthTypeProvider:addControl("fcStatusRolesLkp":U + goCntAuthTypeProvider:ContainerCode  , "wsMsComboButton":U,  "":U,  "":U,                                                       "":U,           38,  "Status<br>Update<br>(Roles)")
    oControl:LookupWobFLA           = "mimro":U
    oControl:LookupFields           = "mim_role.role_obj":U
    oControl:LookupControls         = "fcStatusRoleObjs" + goCntAuthTypeProvider:ContainerCode  
    oControl:FilterFields           = "mim_role.role_obj":U
    oControl:FilterControls         = "fcStatusRoleObjs" + goCntAuthTypeProvider:ContainerCode 

    oControl                        = goCntAuthTypeProvider:addControl("fcDetailObj":U             + goCntAuthTypeProvider:ContainerCode, "wsInput":U,  "8":U, "tt_auth_type_provider.auth_type_provider_obj":U,           "character":U,     39, "Auth Provider Obj":U)             
    oControl:ControlToken           = "Hidden":U                                                                                                                                                                                                         
                                                                                                                                                                                                                                                         
    oControl                        = goCntAuthTypeProvider:addControl("fcAuthTypeObjArgument":U   + goCntAuthTypeProvider:ContainerCode, "wsInput":U,  "8":U, "tt_auth_type_provider.auth_type_obj":U,                     "character":U,    40, "Auth Type Obj":U)  
    oControl:ControlToken           = "Hidden":U                                                                                                                                                                                                       
    oControl:ControlValue           = goWob:CurrentObj
    
    oControl                        = goCntAuthTypeProvider:addControl("fcBaseRateUpdUser":U       + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "10":U , "tt_auth_type_provider.base_rate_upd_user":U           , "character":U  ,  41, "":U)                              
    oControl:ControlClass           = "+clAuthService":U                                                                                                                                                       
    oControl:ControlTooltip         = "Please enter a valid user(s) or enter None Allowed if no users are allowed.":U 
    oControl                        = goCntAuthTypeProvider:addControl("fcBaseRateUpdUserObj":U    + goCntAuthTypeProvider:ContainerCode, "wsInput":U,    "20":U, "":U,                                                    "character":U,     41, "":U)
    oControl:ControlToken           = "Hidden"       
    /* Displays [MULTIPLE SELECTED] on the 'Base rate update user' field after selecting multiple users. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnSetControlValue("fcBaseRateUpdUser'   + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcBaseRateUpdUserObj' + goCntAuthTypeProvider:ContainerCode + '", ""),"~[MULTIPLE SELECTED~]",true)~}'
    oControl                        = goCntAuthTypeProvider:addControl("fcBaseRateUpdUserLkp":U    + goCntAuthTypeProvider:ContainerCode, "wsMsComboButton":U,  "":U,   "":U,                                              "":U,              41, "Base Rate <br>Update Allowed <br>(Users)")
    oControl:LookupWobFLA           = "mimus":U
    oControl:LookupFields           = "mim_user.user_obj":U
    oControl:LookupControls         = "fcBaseRateUpdUserObj" + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "mim_user.user_obj":U
    oControl:FilterControls         = "fcBaseRateUpdUserObj" + goCntAuthTypeProvider:ContainerCode
    oControl:CellLayoutMask         = "&1&2&3"

    oControl                        = goCntAuthTypeProvider:addControl("fcBaseRateUpdRole":U       + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "10":U , "tt_auth_type_provider.base_rate_upd_role":U           ,"character":U,     42, "Base Rate <br>Update Allowed <br>(Roles)":U)                              
    oControl:ControlClass           = "+clAuthService":U                                                                                                                                                       
    oControl:ControlTooltip         = "Please enter a valid role(s) or enter None Allowed if no roles are allowed.":U 
    oControl                        = goCntAuthTypeProvider:addControl("fcBaseRateUpdRoleObjs":U   + goCntAuthTypeProvider:ContainerCode  , "wsInput":U,          "20":U, "":U,                                            "character":U,     42, "":U)
    oControl:ControlToken           = "Hidden"
    /* Displays [MULTIPLE SELECTED] on the 'Base rate update role' field after selecting multiple roles. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnSetControlValue("fcBaseRateUpdRole'   + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcBaseRateUpdRoleObjs' + goCntAuthTypeProvider:ContainerCode + '", ""),"~[MULTIPLE SELECTED~]",true)~}'
    oControl                        = goCntAuthTypeProvider:addControl("fcBaseRateUpdRoleLkp":U    + goCntAuthTypeProvider:ContainerCode, "wsMsComboButton":U,  "":U,  "":U,                                               "":U,              42, "Base Rate <br>Update Allowed <br>(Roles)")
    oControl:LookupWobFLA           = "mimro":U                                                                                                                                                                                               
    oControl:LookupFields           = "mim_role.role_obj":U                                                                                                                                                                                   
    oControl:LookupControls         = "fcBaseRateUpdRoleObjs" + goCntAuthTypeProvider:ContainerCode                                                                                                                                           
    oControl:FilterFields           = "mim_role.role_obj":U                                                                                                                                                                                   
    oControl:FilterControls         = "fcBaseRateUpdRoleObjs" + goCntAuthTypeProvider:ContainerCode                                                                                                                                           
    oControl:CellLayoutMask         = "&1&2&3"

    oControl                        = goCntAuthTypeProvider:addControl("fcArsRateUpdUser":U        + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "10":U , "tt_auth_type_provider.ars_rate_upd_user":U           , "character":U  ,   43, "":U)                              
    oControl:ControlTooltip         = "Please enter a valid user(s) or enter None Allowed if no users are allowed":U 
    oControl:ControlClass           = "+clAuthService":U                                                                                                                                                       
    oControl                        = goCntAuthTypeProvider:addControl("fcArsRateUpdUserObj":U     + goCntAuthTypeProvider:ContainerCode, "wsInput":U,    "20":U, "":U,                                                    "character":U,     43, "":U)
    oControl:ControlToken           = "Hidden"
    /* Displays [MULTIPLE SELECTED] on the 'Ars Rate update user' field after selecting multiple users. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnSetControlValue("fcArsRateUpdUser'    + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcArsRateUpdUserObj' + goCntAuthTypeProvider:ContainerCode + '", ""),"~[MULTIPLE SELECTED~]",true)~}'
    oControl                        = goCntAuthTypeProvider:addControl("fcArsRateUpdUserLkp":U     + goCntAuthTypeProvider:ContainerCode, "wsMsComboButton":U,  "":U,   "":U,                                              "":U,              43, "Ars Rate <br>Update Allowed <br>(Users)")
    oControl:LookupWobFLA           = "mimus":U
    oControl:LookupFields           = "mim_user.user_obj":U
    oControl:LookupControls         = "fcArsRateUpdUserObj" + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "mim_user.user_obj":U
    oControl:FilterControls         = "fcArsRateUpdUserObj" + goCntAuthTypeProvider:ContainerCode
    oControl:CellLayoutMask         = "&1&2&3"

    oControl                        = goCntAuthTypeProvider:addControl("fcArsRateUpdRoleObjs":U    + goCntAuthTypeProvider:ContainerCode  , "wsInput":U,          "20":U, "":U,                                          "character":U,       44 , "":U)
    oControl:ControlToken           = "Hidden":U
    /* Displays [MULTIPLE SELECTED] on the 'Ars Rate update role' field after selecting multiple role. */
    oControl:JavaScriptOnChange     = 'if(this.value!="")~{fnSetControlValue("fcArsRateUpdRole'    + goCntAuthTypeProvider:ContainerCode + '" + this.name.replace("fcArsRateUpdRoleObjs' + goCntAuthTypeProvider:ContainerCode + '", ""),"~[MULTIPLE SELECTED~]",true)~}'
    oControl                        = goCntAuthTypeProvider:addControl("fcArsRateUpdRole":U        + goCntAuthTypeProvider:ContainerCode, "wsTextArea":U, "10":U , "tt_auth_type_provider.ars_rate_upd_role":U           ,"character":U  ,    44, "":U)                              
    oControl:ControlTooltip         = "Please enter a valid role(s) or enter None Allowed if no roles are allowed.":U 
    oControl:ControlClass           = "+clAuthService":U                                                                                                                                                       
    oControl                        = goCntAuthTypeProvider:addControl("fcArsRateUpdRoleLkp":U     + goCntAuthTypeProvider:ContainerCode, "wsMsComboButton":U,  "":U,  "":U,                                               "":U,              44, "Ars Rate <br>Update Allowed <br>(Roles)")
    oControl:LookupWobFLA           = "mimro":U
    oControl:LookupFields           = "mim_role.role_obj":U
    oControl:LookupControls         = "fcArsRateUpdRoleObjs" + goCntAuthTypeProvider:ContainerCode
    oControl:FilterFields           = "mim_role.role_obj":U
    oControl:FilterControls         = "fcArsRateUpdRoleObjs" + goCntAuthTypeProvider:ContainerCode  
    oControl:CellLayoutMask         = "&1&2&3":U    

    oControl                        = goCntAuthTypeProvider:addControl("fcAuthTypeCounter":U + goCntAuthTypeProvider:ContainerCode, "wsInput":U,           "8":U,  "tt_sequence.sequence_value":U,              "integer":U, 46, "":U)  
    oControl:ControlToken           = "Hidden":U 
 .
  
  ASSIGN oContainerProperties                         = NEW cls.wscontainerproperties(goCntAuthTypeProvider)
         oContainerProperties:DefaultLess             = TRUE
         oContainerProperties:CollapsableControlList  = "fcInsurer":U                     + goCntAuthTypeProvider:ContainerCode + ",":U + "fcValidClaimCodesDetail":U      + goCntAuthTypeProvider:ContainerCode + ",":U 
                                                      + "fcValidClaimCodesDetailLkp":U    + goCntAuthTypeProvider:ContainerCode + ",":U + "fcValidClaimTypesDetailCombo":U + goCntAuthTypeProvider:ContainerCode + ",":U
                                                      + "fcAuthAutoCreate"                + goCntAuthTypeProvider:ContainerCode + ",":U + "fcAuthGroup":U                  + goCntAuthTypeProvider:ContainerCode
         oContainerProperties:DisplayEditButton       = TRUE
         oContainerProperties:EditButtonAssignList    = "&fcDetailObj=[BufferField:tt_auth_type_provider.auth_type_provider_obj]":U.
         
  { ma/inc/wsupdatetablecontainer.i &ContainerType = "'TABLE'" &UpdatableTable = TRUE  &Container = goCntAuthTypeProvider &ContainerProperties = oContainerProperties } 
   
  { mip/inc/mipcatcherror.i }  
