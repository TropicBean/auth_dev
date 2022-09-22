/* maauthdetailgetcntupdform.i MEDSTAR Medical Aid System
                               Authorisation Detail Container Procedure ( getCntUpdDetailForm )
                               (c) Copyright 1990 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/
  
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE VARIABLE oControl            AS cls.mipwscontrol   NO-UNDO.
  DEFINE VARIABLE cFieldErrorMessage  AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cButtonErrorMessage AS CHARACTER          NO-UNDO.

  ASSIGN
    cFieldErrorMessage = "Please ensure you have selected a related entity before selecting a Related Item Code"
    cButtonErrorMessage = "To activate this button please select a related entity before continuing, "
                        + "if the related entity field is disabled it may be because the owning item is either a nappi or a basket".

  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "98%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle        = "Clinical Detail Maintenance":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth_detail NO-LOCK":U
                                       + "   WHERE tt_auth_detail.auth_detail_obj = '&1',":U
                                       + "   FIRST tt_auth_provider NO-LOCK":U
                                       + "   WHERE tt_auth_provider.auth_provider_obj = tt_auth_detail.auth_provider_obj,":U
                                       + "   FIRST tt_auth NO-LOCK":U
                                       + "   WHERE tt_auth.auth_obj = tt_auth_detail.auth_obj,"
                                       + "   FIRST attending_doctor NO-LOCK":U
                                       + "   WHERE attending_doctor.doc-num = tt_auth_provider.doc_num,":U
                                       + "   FIRST group_doctor NO-LOCK":U
                                       + "   WHERE group_doctor.doc-num = tt_auth_provider.group_doc_num OUTER-JOIN,":U
                                       + "   FIRST group_discipline NO-LOCK":U
                                       + "   WHERE group_discipline.doc-num = tt_auth_provider.group_doc_num OUTER-JOIN":U
                                       
    /*
       Display provider details captured in the Provider Form                          
    */
    oControl                           = opoContainer:addControl("cbProviderSequence":U      + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_provider.provider_sequence":U      , "integer":U,    1, 1, "Sequence:")
    oControl:ControlToken              = "readonly":U     
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fdAuthDetailObj":U         + ipcContainerName, "wsInput":U       , "5":U   , "tt_auth_detail.auth_detail_obj":U          , "character":U,  1, 2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                      
                                                                                                                                                                                         
    oControl                           = opoContainer:addControl("cbProviderType":U          + ipcContainerName, "wsCombo":U       , "18":U  , "tt_auth_provider.provider_type":U          , "character":U,  2, 1, "Provider Type:")
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                               
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthProviderType:=":U                                                                                                                         
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                                              
                                                                                                                                                                                         
    oControl                           = opoContainer:addControl("fiProviderNum":U           + ipcContainerName, "wsInput":U       , "18":U  , "":U                                        , "integer":U  ,  3, 1, "Provider Number:")
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                                      
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetProviderNum":U                                                                                                                          
                                                                                                                                                                                         
    oControl                           = opoContainer:addControl("flGroupProvider":U         + ipcContainerName, "wsCheckbox":U    , "2":U   , "tt_auth_detail._group_provider":U          , "logical":U  ,  3, 1, "Provider Number:")
    oControl:CelllayoutMask            = "&1&2":U                                                                                                                                        
    oControl:ControlToken              = "Hidden":U                                                                                                                                      
    oControl:ControlValue              = "?":U                                                                                                                                           
                                                                                                                          
    oControl                           = opoContainer:addControl("fiAttProviderNum":U        + ipcContainerName, "wsInput":U       , "18":U  , "":U                                        , "integer":U  ,  3, 2, "Attending Number:")
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetAttProviderNum":U                                                                                                                       
    oControl:CellClass                 = "+clHid +clAtt":U                                                                                                                               
    oControl:LabelClass                = "+clHid +clAtt":U                                                                                                                               
                                                                                                                                                                                                           
    oControl                           = opoContainer:addControl("fcProvDisc":U              + ipcContainerName, "wsInput":U       , "18":U  , "group_discipline.disp-code":U              , "character":U,  4, 1, "Provider Discipline:")
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                     
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetProviderDiscipline":U                                                                                                                   
                                                                                                                                                                                                           
    oControl                           = opoContainer:addControl("fcAttDisc":U               + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_provider.pr_type":U                , "character":U,  4, 2, "Attending Discipline:")
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetAttProviderDiscipline":U                                                                                                                
    oControl:CellClass                 = "+clHid +clAtt":U                                                                                                                               
    oControl:LabelClass                = "+clHid +clAtt":U                                                                                                                               
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    
                                                                                                                                                                                         
    oControl                           = opoContainer:addControl("fcProvSubDisc":U           + ipcContainerName, "wsInput":U       , "18":U  , "group_discipline.subdisp-code":U           , "character":U,  5, 1, "Provider Sub-Discipline:")
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetProviderSubDiscipline":U                                                                                                                
                                                                                                                                                                                         
    oControl                           = opoContainer:addControl("fcAttProvSubDisc":U        + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_provider.sub_pr_type":U            , "character":U,  5, 2, "Attending Sub-Discipline:")
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetAttProviderSubDiscipline":U                                                                                                             
    oControl:CellClass                 = "+clHid +clAtt":U                                                                                                                               
    oControl:LabelClass                = "+clHid +clAtt":U                                                                                                                               
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    
                                                                                                                                                                                         
    oControl                           = opoContainer:addControl("fcProvName":U              + ipcContainerName, "wsInput":U       , "18":U  , "group_doctor.name":U                       , "character":U,  6, 1, "Provider Name:")
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetProviderDesc":U                                                                                                                         
                                                                                                                                                                                                           
    oControl                           = opoContainer:addControl("fcAttName":U               + ipcContainerName, "wsInput":U       , "18":U  , "attending_doctor.name":U                   , "character":U,  6, 2, "Attending Name:")
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetAttProviderDesc":U                                                                                                                      
    oControl:CellClass                 = "+clHid +clAtt":U                                                                                                                               
    oControl:LabelClass                = "+clHid +clAtt":U                                                                                                                               
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    

    oControl                           = opoContainer:addControl("fcProvNegGroup":U          + ipcContainerName, "wsInput":U       , "18":U  , "":U                                        , "character":U,  7, 1, "Provider Negotiation Group:")
    oControl:ControlToken              = "Readonly":U   
    
    oControl                           = opoContainer:addControl("fdAuthGroupObj":U          + ipcContainerName, "wsCombo":U       , "18":U  , "tt_auth_detail.auth_group_obj":U           , "character":U,  8, 1, "Authorisation Group:")
    oControl:AdditionalItems           = "=":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetAuthGroupList":U
    oControl:ControlTooltip            = "Groups the Providers and Clinical Details to a specific Authorisation group. Only one of these Providers/Disciplines will be allowed to be claimed from"

    /*   BlankLine    */                                                                                                                                                                 
    oControl                           = opoContainer:addControl("fcFirstBlkLine":U          + ipcContainerName, "":U              , "":U    , "":U                                        , "":U         ,  9, 1, "")
    oControl:CellLayoutMask            = "&1<br/>":U                                         
    oControl:SpanOverLabel             = TRUE                                                
    oControl:ColumnSpan                = 2                                                   
                                               
    /* Although this control is disabled for now, we may use in the future */
    oControl                           = opoContainer:addControl("fcTariffTypeCategory":U    + ipcContainerName, "wsCombo":U       , "18":U  , "tt_auth_detail._loc_tariff_type_category":U, "character":U,  10, 1, "Tariff Type Category:":U)    
    oControl:AdditionalItems           = "=":U
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK":U
                                       + "   WHERE mic_acronym.category_key = 'ma_acTariffTypeCat'":U
    oControl:KeyField                  = "mic_acronym.acronym_key":U
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "TariffType":U 
    oControl:ControlClass              = "+clHid"
	
    	

    /* Although this control is disabled for now, we may use in the future */
    oControl                           = opoContainer:addControl("cbTariffType":U            + ipcContainerName, "wsCombo":U       , "18":U , "tt_auth_detail.loc_tariff_type_obj":U      , "character":U,  10,2, "Tariff Type:")    
    oControl:AdditionalItems           = "=":U
    oControl:QueryString               = "FOR EACH htm_tariff_type NO-LOCK BY htm_tariff_type.tariff_type_code":U
    oControl:KeyField                  = "htm_tariff_type.tariff_type_obj":U
    oControl:DisplayFields             = "htm_tariff_type.tariff_type_code":U  
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "TariffType":U 
    oControl:ControlClass              = "+clHid"

    oControl                           = opoContainer:addControl("cbEntity":U                + ipcContainerName, "wsCombo":U       , "18":U  , "tt_auth_detail.owning_entity_mnemonic":U  , "character":U, 11, 1, "Entity:")
    oControl:AdditionalItems           = "=":U
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK":U    
                                       + "   WHERE mic_acronym.category_key = 'ma_acAuthDetailEntities'":U
                                       + "      BY mic_acronym.acronym_label":U 
    oControl:KeyField                  = "mic_acronym.acronym_value":U                                
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:ControlJavascript         = "style='width:128px'":U
    oControl:ControlToken              = "ReadOnly":U
    oControl:JavascriptOnChange        = "fnSetQtyAuth(this);":U
    
    oControl                           = opoContainer:addControl("cbRelatedEntity":U         + ipcContainerName, "wsCombo":U       , "18":U  , "tt_auth_detail.related_entity_mnemonic":U , "character":U,  11, 2, "Related Entity:")
    oControl:AdditionalItems           = "=":U
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK":U    
                                       + "   WHERE mic_acronym.category_key = 'ma_acAuthDetailRelated'":U
                                       + "      BY mic_acronym.acronym_label":U 
    oControl:KeyField                  = "mic_acronym.acronym_value":U                                
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:ControlJavascript         = "style='width:128px'":U
    oControl:JavascriptOnChange        = "fnSetQtyAuth(this); fnReactivateSlentLookupBtn(this, ~{triggerField: ~"cbRelatedEntity~", slentInputField: ~"fcRelatedCode~", slentButtonField: ~"buDetailRelatedBtn~"~});":U 
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetailRelatedOEMRestrict":U  
                                                                                             
    oControl                           = opoContainer:addControl("fcDetailCode":U            + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_detail.owning_alt_value":U       , "character":U, 12, 1, " Item Code:")
    oControl:ControlToken              = "ReadOnly":U                                        
                                                                                                                                                                                                           
    oControl                           = opoContainer:addControl("buProtocolView":U     + ipcContainerName, "cls.mipwsembedcontainer":U, "10":U  , "tt_auth_detail.related_obj":U        , "character":U, 12,1, "Item Code:":U)      
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:ControlToolTip            = "View Protocol":U
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "RenderArgument=ProtocolViewButton&BufferName=tt_auth_detail&OwningMnemonicField=owning_entity_mnemonic&OwningObjField=owning_obj&OwningKeyField=owning_key":U
    oControl:CellLayoutMask            = "&1&nbsp;&nbsp;&nbsp;&nbsp;&2"

    oControl                           = opoContainer:addControl("fdRelatedObj":U            + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_detail.related_obj":U            , "character":U, 12, 2, "Related Item Code:")
    oControl:ControlToken              = "Hidden":U.
    
 ASSIGN
    oControl                           = opoContainer:addControl("fcRelatedCode":U           + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_detail.related_value":U          , "character":U, 12, 2, "Related Item Code:")
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationDetail:"
    oControl:FilterFields              = "[OwningEntityMnemonic],[OwningAltValue],[StartDate],[AuthObj]":U  
    oControl:FilterControls            = "cbRelatedEntity":U + ipcContainerName + ",fcRelatedCode":U + ipcContainerName + ",fdStartDate":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName
    oControl:ReturnFields              = "[RecordObj],[RecordDesc],[RecordClaimCode],[RecordClaimType]":U                                                                                 
    oControl:ReturnControls            = "fdRelatedObj":U  + ipcContainerName + ",":U + "fcRelatedDescr":U  + ipcContainerName  + ",":U + "fiClaimCode":U + ipcContainerName  + ",":U + "cbClaimType":U  + ipcContainerName 
    oControl:JavaScriptOnChange        = "fnRevealSlentErrorMessage(this, ~{triggerField: ~"cbRelatedEntity~", thisSlentField: ~"fcRelatedCode~", isFormWindow: true, errorMessage: ~"" + cFieldErrorMessage + "~" ~} );":U
    oControl:ControlJavascript         = " onkeydown= 'fnRevealSlentErrorMessage(this, ~{triggerField: ~"cbRelatedEntity~", thisSlentField: ~"fcRelatedCode~", isFormWindow: true, errorMessage: ~"" + cFieldErrorMessage + "~" ~} );'":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetailRelatedValueRestrict":U 
    
    oControl                           = opoContainer:addControl("buDetailRelatedBtn":U      + ipcContainerName, "wsLookupButton":U, "18":U  , "":U                                      , "":U         , 12, 2, "Related Item Code:")
    oControl:JavascriptOnClick         = "fnRevealSlentErrorMessage(this, ~{triggerField: ~"cbRelatedEntity~", thisSlentField: ~"buDetailRelatedBtn~", isFormWindow: true, errorMessage: ~"" + cButtonErrorMessage + "~" ~} );":U
    oControl:LookupWobFLA              = "slent":U                                                                                                                                       
    oControl:LookupFields              = "KEY_FIELD,CODE_FIELD":U
    oControl:LookupControls            = "fdRelatedObj":U + ipcContainerName + ",fcRelatedCode":U + ipcContainerName 
    oControl:FilterControls            = "cbRelatedEntity":U + ipcContainerName
    oControl:FilterFields              = "QUERY_OEM":U
    oControl:ReturnFields              = "CODE_FIELD,DESC_FIELD":U
    oControl:ReturnControls            = "fcRelatedCode":U + ipcContainerName + ",fcDetailDescr":U + ipcContainerName
    oControl:CellLayoutMask            = "&1&2&3":U
    oControl:ControlToken              = "disabled":U
    oControl:ControlJavascript         = " onmouseover= 'fnReactivateSlentLookupBtn(this, ~{triggerField: ~"cbRelatedEntity~", slentInputField: ~"fcRelatedCode~", slentButtonField: ~"buDetailRelatedBtn~"~});'":U
    
    oControl                           = opoContainer:addControl("fcDetailDescr":U           + ipcContainerName, "wsTextArea":U    , "15,5":U, "tt_auth_detail._detail_owning_description":U, "character":U, 13, 1, "Item Description:")
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                    
                                                                                                                                                                                         
    oControl                           = opoContainer:addControl("fcRelatedDescr":U          + ipcContainerName, "wsTextArea":U    , "15":U, "tt_auth_detail._detail_related_description":U, "character":U, 13, 2, "Related Item Description:")
    oControl:ControlToken              = "disabled":U     
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthDetSlentButtonSet":U  

    oControl                           = opoContainer:addControl("fiLocSequence":U           + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.loc_sequence":U             , "INTEGER":U  , 14,1, "LOC Seq:":U)
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                                                                      

    oControl                           = opoContainer:addControl("fdQuantityLos":U           + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.quantity_los":U             , "INTEGER":U  , 14,2, "Qty LOS:":U)
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                                                                      

    oControl                           = opoContainer:addControl("fiMinRequested":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.minutes_requested":U        , "INTEGER":U  , 15,1, "Minutes Requested:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "MinRequestedAndAuthorised":U                                                                                                                                                                                      
    
    oControl                           = opoContainer:addControl("fiMinAuthorised":U         + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.minutes_auth":U             , "INTEGER":U  , 15,2, "Minutes Authorised:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "MinRequestedAndAuthorised":U 

    /* BlankLine */                                                                                                                                                                      
    oControl                           = opoContainer:addControl("fcSecBlkLine":U            + ipcContainerName, "":U              , "":U    , "":U                                      , "":U         , 16, 1, "")
    oControl:CellLayoutMask            = "&1<br/>":U                                                                                                                                     
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2 

    /* Capture Details */                                                                                                                                                                                   
    oControl                           = opoContainer:addControl("fdStartDate":U             + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.start_date":U             , "date":U     , 17, 1, "Start Date:")
    oControl                           = opoContainer:addControl("cbStartAmPm":U             + ipcContainerName, "wsCombo":U       , "15":U  , "tt_auth_detail.start_ampm":U             , "logical":U  , 17, 1, "Start Date:")
    oControl:AdditionalItems           = "AM=yes|PM=no":U
    oControl:CellLayoutMask            = "&1 &2":U
    oControl:ControlClass              = "+clSelVertAlign":U

    oControl                           = opoContainer:addControl("cbLineRestriction":U       + ipcContainerName, "wsCombo":U       , "15":U  , "tt_auth_detail.line_restriction":U       , "character":U, 17, 2, "Line Restriction:")
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthLineRestriction:<None>=":U
    oControl:ControlJavascript         = "style='width:128px'":U
    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("fdEndDate":U               + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.end_date":U               , "date":U     , 18, 1, "End Date:")
    oControl                           = opoContainer:addControl("cbEndAmPm":U               + ipcContainerName, "wsCombo":U       , "15":U  , "tt_auth_detail.end_ampm":U               , "logical":U  , 18, 1, "End Date:")
    oControl:AdditionalItems           = "AM=yes|PM=no":U
    oControl:CellLayoutMask            = "&1 &2":U
    oControl:ControlClass              = "+clSelVertAlign":U

    oControl                           = opoContainer:addControl("fcNote":U                  + ipcContainerName, "wsTextArea":U    , "15":U  , "tt_auth_detail._note_narration":U        , "character":U, 18, 2, "Note:")
    oControl:ControlToken              = "Disabled":U
    oControl                           = opoContainer:addControl("fcNoteKey":U               + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.note":U                   , "character":U, 18, 2, "")
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("lkpNote":U                 + ipcContainerName, "wsLookupButton":U, "":U    , "":U                                      , "":U         , 18, 2, "Note:")  
    oControl:LookupWobFLA              = "note":U
    oControl:LookupControls            = "fcNoteKey":U + ipcContainerName
    oControl:LookupFields              = "note.key":U
    oControl:FilterControls            = "fcNoteKey":U + ipcContainerName
    oControl:FilterFields              = "note.key":U
    oControl:ReturnControls            = "fcNote":U    + ipcContainerName
    oControl:ReturnFields              = "note.narration[1]":U
    oControl:CellLayOutMask            = "&1&2&3":U

    oControl                           = opoContainer:addControl("fdFixedItemCost":U         + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.fixed_item_cost":U           , "integer":U  , 19,1, "Fixed Item Cost:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("fdItemCost":U              + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.item_cost":U                 , "integer":U  , 20,1, "Item Cost:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("fiClaimCode":U             + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.claim_code":U             , "integer":U  , 21, 1, "Claim Code:")
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "renderProcedure":U
    oControl:RenderArgument            = "IntegerField":U
    oControl:ControlFormat             = "999":U

    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationDetail:ClaimCode":U                                                                                     
    oControl:FilterFields              = "[ClaimCode],[AuthObj]":U                                           
    oControl:FilterControls            = "fiClaimCode":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName                                                                                                
    oControl:ReturnFields              = "[ValidateClaimCode],[StatusAction]":U                                                                                                                  
    oControl:ReturnControls            = "_errorOrWarning":U + ipcContainerName   + ",_AuthStatusAction":U + ipcContainerName 

    oControl                           = opoContainer:addControl("lkpClaimCode":U            + ipcContainerName, "wsLookupButton":U, "":U    , "":U                                      , "character":U, 21, 1, "Claim Code:")
    oControl:CellLayoutMask            = "&1&2":U
    oControl:LookupWobFLA              = "maclc":U
    oControl:LookupFields              = "ccdesc.claim-code":U
    oControl:LookupControls            = "fiClaimCode":U + ipcContainerName
    oControl:FilterFields              = "ccdesc.claim-code":U
    oControl:FilterControls            = "fiClaimCode":U + ipcContainerName
    oControl:ReturnFields              = "ccdesc.claim-code":U
    oControl:ReturnControls            = "fiClaimCode":U + ipcContainerName.

 ASSIGN
    oControl                           = opoContainer:addControl("fdCPTObj":U                + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.cpt_link_obj":U           , "decimal":U  , 21, 2, "CPT:")
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("fcCPTCode":U               + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail._cpt_code":U              , "character":U, 21, 2, "CPT:")
    oControl                           = opoContainer:addControl("lkpCpt":U                  + ipcContainerName, "wsLookupButton":U, "":U    , "":U                                      , "":U         , 21, 2, "CPT:")
    oControl:CellLayOutMask            = "&1&2&3":U
    oControl:LookupWobFLA              = "hlmck":U
    oControl:LookupControls            = "fdCPTObj":U + ipcContainerName
    oControl:LookupFields              = "hlm_cpt_link.cpt_link_obj":U
    oControl:FilterControls            = "fcCPTCode":U + ipcContainerName
    oControl:FilterFields              = "hlm_cpt_link.cpt_code":U
    oControl:ReturnControls            = "fcCPTCode":U + ipcContainerName + ",fdCPTObj":U + ipcContainerName
    oControl:ReturnFields              = "hlm_cpt_link.cpt_code,hlm_cpt_link.cpt_link_obj":U

    oControl                           = opoContainer:addControl("cbClaimType":U             + ipcContainerName, "wsCombo":U       , "15":U  , "tt_auth_detail.claim_type":U             , "character":U, 22, 1, "Claim Type:")
    oControl:AdditionalItems           = "|C=C|N=N|A=A|K=K|P=P|O=O":U
    oControl:ControlToolTip            = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther":U
    oControl:ControlJavascript         = "style='width:128px'":U






    oControl                           = opoContainer:addControl("cbStatus":U                + ipcContainerName, "wsCombo":U       , "15":U  , "tt_auth_detail.auth_status":U            , "character":U, 22, 2, "Status:")
    oControl:ControlTooltip            = "Select a status from the drop-down list.":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthStatusCombo":U
    oControl:ControlJavascript         = "style='width:128px'":U
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationDetail:Status":U
    oControl:FilterFields              = "[Status],[AuthObj]":U
    oControl:FilterControls            = "cbStatus":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName
    oControl:ReturnFields              = "[StatusReasonMandatory]":U
    oControl:ReturnControls            = "flStatusReasonMandatory":U + ipcContainerName
    oControl:ControlClass              = "+clMan":U
    oControl:JavaScriptOnChange        = "fnOnChangeAuthDetailStatus(this,~"form~", ~"" + ipcContainerName + "~" );":U
                                       + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"form~");":U

    oControl                           = opoContainer:addControl("flStatusReasonMandatory":U + ipcContainerName, "wsInput":U       , "15":U  , "":U                                      , "character":U, 22, 2, "Authorisation Status:":U)
    oControl:CellLayoutMask            = "&1&2":U
    oControl:ControlToken              = "Hidden":U

    /* On status change, check whether reason is mandatory or not */
    oControl:JavaScriptOnChange        = "fnOnChangeAuthDetailStatus(this,~"form~", ~"" + ipcContainerName + "~" );":U
                                       + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"form~");":U
    oControl                           = opoContainer:addControl("cbPayeeDM":U               + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_detail.payee_dm":U                , "character":U, 23, 1, "Payee Indicator:")
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthPayee' BY mic_acronym.acronym_label":U                                             
    oControl:KeyField                  = "mic_acronym.acronym_key":U                                                                                                                                 
    oControl:DisplayFields             = "mic_acronym.acronym_label":U                                                                                                                               
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "AuthDetailPayeeIndicator":U                                                                                                                              
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                                     

    oControl                           = opoContainer:addControl("fcReason":U                + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.auth_status_note":U       , "character":U, 23, 2, "":U)
    oControl:ControlTooltip            = "Status Reason":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthDetailStatusNote":U
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationDetail:Reason":U
    oControl:FilterFields              = "[ReasonCode],[ReasonType]":U
    oControl:FilterControls            = "fcReason":U + ipcContainerName + ",fcReasonType":U + ipcContainerName
    oControl:ReturnFields              = "[ReasonDesc]":U
    oControl:ReturnControls            = "fcReasonDesc":U + ipcContainerName

    oControl                           = opoContainer:addControl("fcReasonTypePrefix":U      + ipcContainerName, "wsInput":U       , "5":U   , "":U                                      , "character":U, 23, 2, "":U)
    oControl:ControlToken              = "Hidden":U
    oControl:ControlValue              = "AS":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthReasonTypePrefix":U

    oControl                           = opoContainer:addControl("fcReasonDesc":U            + ipcContainerName, "wsInput":U       , "5":U   , "":U                                      , "character":U, 23, 2, "":U)
    oControl:ControlToken              = "Hidden":U

    /* Set reason description as tooltip when reason is selected client side */
    oControl:JavascriptOnChange        = "fnSetReasonDescription(this, ~"fcReason":U + ipcContainerName + "~", ~"buReasonBtn":U + ipcContainerName + "~", ~"FORM~");":U

    oControl                           = opoContainer:addControl("fcReasonType":U            + ipcContainerName, "wsInput":U       , "5":U   , "":U                                      , "character":U, 23, 2, "":U)
    oControl:ControlToken              = "Hidden":U
    oControl:ControlValue              = "AS*":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthReasonType":U

    oControl                           = opoContainer:addControl("buReasonBtn":U             + ipcContainerName, "wsLookupButton":U, "":U    , "":U                                      , "":U         , 23, 2, "Status Reason:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthDetailStatusNote":U
    oControl:CellLayoutMask            = "&1&2&3&4&5":U
    oControl:LookupWobFLA              = "note":U
    oControl:LookupFields              = "note.key":U
    oControl:LookupControls            = "fcReason":U + ipcContainerName
    oControl:FilterFields              = "note.key,note.type":U
    oControl:FilterControls            = "fcReason":U + ipcContainerName + ",fcReasonType":U + ipcContainerName
    oControl:ReturnFields              = "note.key":U
    oControl:ReturnControls            = "fcReason":U + ipcContainerName

    oControl                           = opoContainer:addControl("cbBodyRegion":U            + ipcContainerName, "wsCombo":U       , "15":U  , "tt_auth_detail.body_region":U            , "character":U, 24, 1, "Body Region:":U)
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AcronymSelect:ma_acBodyRegion:=":U
    oControl:ControlJavascript         = "style='width:128px'":U

    oControl                           = opoContainer:addControl("cbSystemStatus":U          + ipcContainerName, "wsCombo":U       , "15":U  , "tt_auth_detail.auth_status":U            , "character":U, 24, 2, "System Status:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthSystemStatusCombo":U
    oControl:ControlJavascript         = "style='width:128px'":U
    oControl:ControlToken              = "ReadOnly":U
   .

  ASSIGN
    oControl                           = opoContainer:addControl("fcDefaultBaseRate":U       + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.default_base_rate":U      , "character":U, 25, 1, "Default Base Rate:")
    oControl:ControlToken              = "ReadOnly":U
    oControl:ControlToken              = "ReadOnly":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "BaseRate":U

    oControl                           = opoContainer:addControl("fcOverrideBaseRate":U      + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.override_base_rate":U     , "character":U, 25, 2, "Override Base Rate:")
    oControl                           = opoContainer:addControl("lkpBaseRate":U             + ipcContainerName, "wsLookupButton":U, "":U    , "":U                                      , "character":U, 25, 2, "Override Base Rate:")
    oControl:LookupWobFLA              = "baserate":U
    oControl:LookupControls            = "fcOverrideBaseRate":U + ipcContainerName
    oControl:LookupFields              = "baserate.base-rate":U
    oControl:FilterControls            = "fcOverrideBaseRate":U + ipcContainerName
    oControl:FilterFields              = "baserate.base-rate":U
    oControl:CellLayOutMask            = "&1&2":U

    oControl                           = opoContainer:addControl("fcDefaultArsRate":U        + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.default_ars_rate":U       , "character":U, 26, 1, "Default Ars Rate:")
    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("fcOverrideArsRate":U       + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.override_ars_rate":U      , "character":U, 26, 2, "Override Ars Rate:")
    oControl                           = opoContainer:addControl("lkpArsRate":U              + ipcContainerName, "wsLookupButton":U, "":U    , "":U                                      , "character":U, 26, 2, "Override Ars Rate:")
    oControl:LookupWobFLA              = "arsrate":U
    oControl:LookupControls            = "fcOverrideArsRate":U + ipcContainerName
    oControl:LookupFields              = "arsrate.ars-rate":U
    oControl:FilterControls            = "fcOverrideArsRate":U + ipcContainerName
    oControl:FilterFields              = "arsrate.ars-rate":U
    oControl:CellLayOutMask            = "&1&2":U

    oControl                           = opoContainer:addControl("cbPrintDM":U               + ipcContainerName, "wsCombo":U       , "15":U , "tt_auth_detail.print_dm":U                , "character":U, 27, 1, "Print Indicator:")
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthPrintLetter:=":U
    oControl:ControlJavascript         = "style='width:128px'":U

    oControl                           = opoContainer:AddControl("fcViewItemCode":U          + ipcContainerName, "wsHref":U        , "15":U  , "":U                                      , "character":U, 28, 1, "")
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthDetailOwningInfo":U

    oControl                           = opoContainer:addControl("fcTariffReason":U          + ipcContainerName, "wsTextArea":U    , "15":U  , "tt_auth_detail.reason":U                 , "character":U, 27, 2, "Reason:")
    
    oControl                           = opoContainer:addControl("flRepeatItem":U               + ipcContainerName, "wsCheckBox":U , "2":U , "tt_auth_detail.repeat_item":U              , "logical":U,   31, 2, "Repeat Item:":U)
    oControl:ControlTooltip            = "Indicate whether this item code is a Repeat.":U                                                                                                                 
                                                                                                                                                                                                          
    oControl                           = opoContainer:addControl("fiRepeatCycleAuth":U          + ipcContainerName, "wsInput":U    , "5":U , "tt_auth_detail.repeat_cycle_auth":U        , "integer":U,   32, 2, "Repeat Cycle Auth:":U)
    oControl:ControlTooltip            = "Indicates the authorised number of repeats for the prescription.":U                                                                                             
                                                                                                                                                                                                          
    oControl                           = opoContainer:addControl("fiRepeatCyclePaid":U          + ipcContainerName, "wsInput":U    , "5":U , "tt_auth_detail.repeat_cycle_paid":U        , "integer":U,   33, 2, "Repeat Cycle Paid:":U)
    oControl:ControlTooltip            = "Indicates the authorised number of repeats that have been claimed.":U                                                                                           
    oControl:ControlToken              = "Disabled":U                                                                                                                                                     
                                                                                                                                                                                                          
    oControl                           = opoContainer:addControl("fiRepeatCycleQuantity":U      + ipcContainerName, "wsInput":U    , "5":U , "tt_auth_detail.repeat_cycle_quantity":U    , "integer":U,   34, 2, "Repeat Cycle Qty:":U)
    oControl:ControlTooltip            = "Indicates the quantity that will be repeated.":U                                                                                                                
                                                                                                                                                                                                          
    oControl                           = opoContainer:addControl("fiRepeatCycleDays":U          + ipcContainerName, "wsInput":U    , "5":U , "tt_auth_detail.repeat_cycle_days":U        , "integer":U,   35, 2, "Repeat Cycle Days:":U)
    oControl:ControlTooltip            = "Indicates the number of days in one cycle and when a new script can be requested.":U                                                                            
                                                                                                                                                                                                          
    oControl                           = opoContainer:addControl("fiRepeatCycleGraceDays":U     + ipcContainerName, "wsInput":U    , "5":U , "tt_auth_detail.repeat_cycle_grace_days":U  , "integer":U,   36, 2, "Repeat Cycle Grace Days:":U)
    oControl:ControlTooltip            = "Indicates when the script can be requested earlier that the Repeat Cycle Days.":U                                                                               
                                                                                                                                                                                                          
    oControl                           = opoContainer:addControl("fdRepeatLastClaimedDate":U    + ipcContainerName, "wsInput":U    , "8":U , "tt_auth_detail.repeat_last_claimed_date":U , "date":U,      37, 2, "Repeat Last Claimed Date:":U)
    oControl:ControlTooltip            = "Indicates the last date that this item was claimed":U
    oControl:ControlToken              = "ReadOnly":U
    
    oControl                           = opoContainer:addControl("flAddedByUser":U           + ipcContainerName, "wsCheckbox":U    , "2":U   , "tt_auth_detail.added_by_user":U          , "logical":U  , 29, 1, "Added by User:")
    oControl:ControlToken              = "Readonly":U

    oControl                           = opoContainer:addControl("fcCopayOverrideNote":U     + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_detail.copay_override_note":U , "INTEGER":U    ,29, 2, "":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                            
    oControl:RenderArgument            = "CopayOverrideNote":U        
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationDetail:OverrideNote":U                                                                                          
    oControl:FilterFields              = "[ReasonCode],[ReasonType],[DetailObj]":U                                                                                                         
    oControl:FilterControls            = "fcCopayOverrideNote":U + ipcContainerName + ",":U 
                                       + "fcOverrideNoteTypeArgument":U + ipcContainerName + ",":U
                                       + "fdAuthDetailObj":U + ipcContainerName 
    oControl:ReturnFields              = "[ReasonDesc]":U                                                                                                                  
    oControl:ReturnControls            = "fcOverrideNoteDesc":U + ipcContainerName                                                                                 

    oControl                           = opoContainer:addControl("fcOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U,29, 2, "":U)                  
    oControl:ControlToken              = "Hidden":U
    oControl:ControlValue              = "AQ":U
    oControl:ControlClass              = "+clPreserveValue":U

    oControl                           = opoContainer:addControl("fcOverrideNoteDesc":U         + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U,29, 2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                     

    /* Set copay override note description as tooltip when note is selected client side */ 
    oControl:JavascriptOnChange        = "fnSetReasonDescription(this, ~"fcCopayOverrideNote":U + ipcContainerName + "~", ~"buOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U  

    oControl                           = opoContainer:addControl("buOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         ,29, 2, "Co-payment Override Reason:":U)
    oControl:ControlTooltip            = "Specify reason why co-payment should not apply.":U
    oControl:CellLayoutMask            = "&1&2&3&4":U                                                                                                                                                      
    oControl:LookupWobFLA              = "note":U                                                                                                                                                    
    oControl:LookupFields              = "note.key":U                                                                                                                                                
    oControl:LookupControls            = "fcCopayOverrideNote":U + ipcContainerName                                                                                                                             
    oControl:FilterFields              = "note.key,note.type":U                                                                                                                                      
    oControl:FilterControls            = "fcCopayOverrideNote":U + ipcContainerName + ",":U
                                       + "fcOverrideNoteTypeArgument":U + ipcContainerName                                                                                      
    oControl:ReturnFields              = "note.key":U                                                                                                                                           
    oControl:ReturnControls            = "fcCopayOverrideNote":U + ipcContainerName      

    oControl                           = opoContainer:addControl("flLosCalc":U               + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_provider.los_calculation":U        , "character":U, 30, 1, "System LOS Calculation:":U)
    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("fcLosCalcRule":U           + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.los_calculation_rule":U     , "character":U, 30, 2, "LOS Calculation Rule:":U)
    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("fcMinutesCalcRule":U       + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.minutes_calculation_rule":U , "character":U, 31, 1, "Minutes Calculation Rule:":U)

    oControl                           = opoContainer:addControl("fiMouthPartID":U           + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.mouth_part_id[1]":U       , "integer":U,   32, 1, "Mouth Part:")
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "renderProcedure":U
    oControl:RenderArgument            = "IntegerField":U

    oControl                           = opoContainer:addControl("buMouthPartIdBtn":U        + ipcContainerName, "wsLookupButton":U, "":U    , "":U                                      , "":U         , 32, 1, "Mouth Part:")
    oControl:CellLayoutMask            = "&1&2":U                                                                                                                                                           
    oControl:LookupWobFLA              = "mouthid":U
    oControl:LookupFields              = "mouthid.mouth-part-id":U
    oControl:LookupControls            = "fiMouthPartID":U + ipcContainerName
    oControl:FilterFields              = "mouthid.mouth-part-id":U
    oControl:FilterControls            = "fiMouthPartID":U + ipcContainerName
    oControl:ReturnFields              = "mouthid.mouth-part-id":U
    oControl:ReturnControls            = "fiMouthPartID":U + ipcContainerName

    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("flAddToTotalLos":U         + ipcContainerName, "wsInput":U       , "2":U , "tt_auth_detail.add_to_total_los":U         , "logical":U  , 37, 1, "")
    oControl:ControlToken              = "Hidden":U                                                                                                                                    
                                                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcThirdBlkLine":U          + ipcContainerName, "":U              , "":U  , "":U                                        , "":U         , 38, 1, "")
    oControl:CellLayoutMask            = "&1<br/>":U
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2

    /* PMB Container */
    oControl                           = opoContainer:addControl("PmbContainer":U            + ipcContainerName, "":U              , "":U    , "":U                                      , "":U         , 40, 1, "":U)                                                                                                                               
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "DecisionPMBRender":U   
    oControl:SubContainer              = mipEnv:Health:maUIService:getCntUpdAuthDetPmb("Pmb":U + ipcContainerName)                                                                                 
    oControl:SubContainerType          = "FORM":U
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2
    oControl:CellSnippet               = "align='center'":U   

    /* Financial Information Container */
    oControl                           = opoContainer:addControl("FinancialContainer":U      + ipcContainerName, "":U              , "":U    , "":U                                      , "":U         , 41, 1, "":U)                                                                                                                               
    oControl:SubContainer              = mipEnv:Health:maUIService:getCntUpdAuthDetFinancial("Financial":U + ipcContainerName)                                                                         
    oControl:SubContainerType          = "FORM":U
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2                                                                                                                                                              
    oControl:CellSnippet               = "align='center'":U

    /* Savings Container */                                                                                                                                                              
    oControl                           = opoContainer:addControl("SavingsContainer":U        + ipcContainerName, "":U              , "":U    , "":U                                      , "":U         , 42, 1, "":U)                                                                                                                               
    oControl:SubContainer              = mipEnv:Health:maUIService:getCntUpdAuthDetSavings("Savings":U + ipcContainerName)                                                               
    oControl:SubContainerType          = "FORM":U
    oControl:SpanOverLabel             = TRUE
    oControl:ColumnSpan                = 2
    oControl:CellSnippet               = "align='center'":U

    /* Buttonbar */                                                                                                                                                                      
    oControl                           = opoContainer:addControl("frmSubmit":U               + ipcContainerName, "":U              , "":U    , "":U                                      , "character":U, 43, 1, "":U)
    oControl:SpanOverlabel             = TRUE
    oControl:CellClass                 = WarpSpeed:BaseClass + "ButtonBar":U                                                                                                             
    oControl:CellSnippet               = "align='right'":U
    oControl:ColumnSpan                = 3
    oControl:SubContainer              = wsUiService:getButtonContainer(WarpSpeed:CurrentWob + ipcContainerName + "BtnBar":U, "Submit:Clear":U)
    
    oControl                           = opoContainer:addControl("_errorOrWarning":U          + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 44,1, "_authtypewarning:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:JavascriptOnChange        = "fnOnChangeErrorOrWarning(this);":U  

    oControl                           = opoContainer:addControl("_AuthStatusAction":U        + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 45,1, ":":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:JavascriptOnChange        = "fnAuthStatusActionDetail(this);":U 

    oControl                           = opoContainer:addControl("_configuration":U          + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail._detail_configuration":U , "character":U, 46, 1, "_configuration:":U)
    oControl:ControlToken              = "Hidden":U
    oControl:JavascriptOnChange        = "fnOnChangeAuthConfiguration(this);":U
    oControl                           = opoContainer:addControl("_authObjArgument":U        + ipcContainerName, "wsInput":U       , "15":U  , "tt_auth_detail.auth_obj":U               , "character":U, 47, 1, "_configuration:":U)
    oControl:ControlToken              = "Hidden":U

    oControl                           = opoContainer:addControl("_authMode":U               + ipcContainerName, "wsInput":U       , "10":U  , "":U                                      , "character":U, 50, 1, "_authMode:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                      
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument            = "AuthMode":U .

&ENDIF
  
  { mip/inc/mipcatcherror.i }       


