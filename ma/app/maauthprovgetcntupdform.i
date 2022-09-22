/* maauthprovgetcntupdform.i MEDSTAR Medical Aid System
                             Authorisation Provider Container Procedure ( getCntUpdProviderForm )
                             (c) Copyright 1990 - 2022
                             MIP Holdings (Pty) Ltd
                             All rights reserved
*/

  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE VARIABLE oControl  AS cls.mipwscontrol   NO-UNDO.

  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "98%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle        = "Provider Maintenance":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth_provider NO-LOCK":U
                                       + "   WHERE tt_auth_provider.auth_provider_obj = '&1'":U
                                       + ",  FIRST tt_auth NO-LOCK":U                               
                                       + "   WHERE tt_auth.auth_obj = tt_auth_provider.auth_obj":U  
                                       + ",  FIRST attending_doctor NO-LOCK":U
                                       + "   WHERE attending_doctor.doc-num = tt_auth_provider.doc_num OUTER-JOIN":U
                                       + ",  FIRST group_doctor NO-LOCK":U
                                       + "   WHERE group_doctor.doc-num = tt_auth_provider.group_doc_num OUTER-JOIN":U
                                       + ",  FIRST group_discipline NO-LOCK":U
                                       + "   WHERE group_discipline.doc-num = tt_auth_provider.group_doc_num OUTER-JOIN":U
                                                                                            
    oControl                           = opoContainer:addControl("cbProvType":U              + ipcContainerName, "wsCombo":U       , "18":U, "tt_auth_provider.provider_type":U     , "character":U, 1, 1, "Provider Type:":U)
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthProviderType:=":U                                                                                                                   
    oControl:ControlToken              = "Disabled":U                                       
    oControl:ControlJavascript         = "style='width:128px'":U     

    oControl                           = opoContainer:addControl("fcChangeRateTypeDisplay":U + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.rate_change_type":U     , "character":U, 1, 2, "Rate Change Type:":U)
    oControl:ControlToken              = "disabled":U 
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "RateChangeTypeDisplay":U  
                                                                                            
    oControl                           = opoContainer:addControl("fdAuthProviderObj":U       + ipcContainerName, "wsInput":U       , "20":U, "tt_auth_provider.auth_provider_obj":U , "character":U, 2, 1, "":U)
    oControl:ControlToken              = "Hidden":U                                         
                                                                                                                               
    oControl                           = opoContainer:addControl("fiProvNum":U               + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.group_doc_num":U     , "integer":U  , 2, 1, "Provider Number:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthProviderNum":U  
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationProvider:Provider":U
    oControl:FilterFields              = "[ProviderNum],[AttProviderNum],[AuthObj],[StartDate]":U
    oControl:FilterControls            = "fiProvNum":U + ipcContainerName + ",fiAttNum":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName + ",fdStartDate":U + ipcContainerName
    oControl:ReturnFields              = "[ProviderDesc],[PrType],[SubPrType],[GroupProvider],[ProviderBaseRate],[ProviderArsRate],[DisciplineUpdatable]":U
    oControl:ReturnControls            = "fcProvName":U + ipcContainerName + ",fcProvDisc":U + ipcContainerName + ",fcProvSubDisc":U + ipcContainerName + ",flGroupProvider":U + ipcContainerName + ",fcProvBaseRate":U + ipcContainerName + ",fcProvArsRate":U + ipcContainerName + ",fcDisciplineUpdatable":U + ipcContainerName                                                      
    
    oControl                           = opoContainer:addControl("flGroupProvider":U         + ipcContainerName, "wsCheckbox":U    , "18":U, "":U                                   , "logical":U  , 2, 1, "Provider Number:":U)
    oControl:ControlToken              = "hidden":U
    oControl:ControlValue              = "?":U   
    oControl:JavascriptonChange        = ' var assocfields = document.getElementsByClassName("clAtt");'
                                       + ' if(this.value=="yes")~{ '
                                       + '    for(var i=0;i<assocfields.length;i++)~{wsSetClassName(assocfields[i],"-clHid");  ~} fnGetControls("lkpAttNumProviderForm")[0].onclick();'
                                       + ' ~}'
                                       + ' else ~{ '
                                       + ' for(var i=0;i<assocfields.length;i++)~{wsSetClassName(assocfields[i],"+clHid"); fnGetControls("fiAttNumProviderForm")[0].value==""; ~}'
                                       + ' ~} ':U    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "GroupProvider":U                                                                                                                          
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("lkpProvNum":U              + ipcContainerName, "wsLookupButton":U, "18":U, "":U                                   , "character":U, 2, 1, "Provider Number:":U)
    oControl:LookupWobFLA              = "madoc":U
    oControl:LookupControls            = "fiProvNum":U + ipcContainerName
    oControl:LookupFields              = "doctor.doc-num":U
    oControl:FilterControls            = "fiProvNum":U + ipcContainerName                                                                                                           
    oControl:FilterFields              = "doctor.doc-num":U                                                                                                                         
    oControl:CellLayOutMask            = "&1&2&3"                                                                                                                                   
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fiAttNum":U                + ipcContainerName, "wsInput":U       , "18":U, "":U                                   , "integer":U  , 2, 2, "":U)
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationProvider:AttendingProvider":U                                                                           
    oControl:FilterFields              = "[ProviderNum],[AttProviderNum],[StartDate],[AuthObj]":U                                                                                                         
    oControl:FilterControls            = "fiProvNum":U + ipcContainerName + ",fiAttNum":U + ipcContainerName  + ",fdStartDate":U  + ipcContainerName + ",_authObjArgument":U + ipcContainerName                                                                        
    oControl:ReturnFields              = "[ProviderDesc],[PrType],[SubPrType],[ProviderBaseRate],[ProviderArsRate],[DisciplineUpdatable]":U                                                                                                    
    oControl:ReturnControls            = "fcAttName":U + ipcContainerName + ",fcAttDisc":U + ipcContainerName + ",fcAttSubDisc":U + ipcContainerName  + ",fcProvBaseRate":U + ipcContainerName + ",fcProvArsRate":U + ipcContainerName + ",fcAttDisciplineUpdatable":U + ipcContainerName                                                   
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthAttProviderNum":U                                                                                                                     
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("lkpAttNum":U               + ipcContainerName, "wsLookupButton":U, "18":U, "":U                                   , "character":U, 2, 2, "Attending Number:":U)
    oControl:CellClass                 = "+clHid +clAtt":U                                                                                                                          
    oControl:LabelClass                = "+clHid +clAtt":U
    oControl:CellLayOutMask            = "&1&2"
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthAttProviderNumLkp":U
                                                                                                                                                                                   
    oControl                           = opoContainer:addControl("fcProvDisc":U              + ipcContainerName, "wsInput":U       , "18":U, "group_discipline.disp-code":U         , "integer":U  , 3, 1, "Provider Discipline:":U)
    oControl:ControlClass              = "+clMaxLength:3":U                                                                                                                         
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthProviderDiscipline":U                                                                                                                 
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("buProvDisc":U              + ipcContainerName, "wsLookupButton":U, "18":U, "":U                                   , "":U         , 3, 1, "Provider Discipline:":U)
    oControl:CellLayoutMask            = "&1&2":U
    oControl:LookupWobFLA              = "maprtype":U
    oControl:LookupFields              = "prtype.pr-type":U
    oControl:LookupControls            = "fcProvDisc":U + ipcContainerName
    oControl:FilterFields              = "prtype.pr-type":U
    oControl:FilterControls            = "fcProvDisc":U + ipcContainerName
    oControl:ReturnFields              = "prtype.pr-type":U
    oControl:ReturnControls            = "fcProvDisc":U + ipcContainerName                                                                                                          

    oControl                           = opoContainer:addControl("fcAttDisc":U               + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.pr_type":U           , "integer":U  , 3, 2, "Attending Discipline:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthAttProviderDiscipline":U
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("buAttDisc":U               + ipcContainerName, "wsLookupButton":U, "18":U, "":U                                   , "":U         , 3, 2, "Attending Discipline:":U)
    oControl:CellClass                 = "+clHid +clAtt":U                                                                                                                          
    oControl:LabelClass                = "+clHid +clAtt":U                                                                                                                          
    oControl:CellLayoutMask            = "&1&2":U
    oControl:LookupWobFLA              = "maprtype":U
    oControl:LookupFields              = "prtype.pr-type":U
    oControl:LookupControls            = "fcAttDisc":U + ipcContainerName
    oControl:FilterFields              = "prtype.pr-type":U
    oControl:FilterControls            = "fcAttDisc":U + ipcContainerName
    oControl:ReturnFields              = "prtype.pr-type":U
    oControl:ReturnControls            = "fcAttDisc":U + ipcContainerName

    oControl                           = opoContainer:addControl("fcProvSubDisc":U           + ipcContainerName, "wsInput":U       , "18":U, "group_discipline.subdisp-code":U      , "integer":U  , 4, 1, "Provider Sub-Discipline:":U)
    oControl:ControlClass              = "+clMaxLength:3":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthProviderSubDiscipline":U

    oControl                           = opoContainer:addControl("buProvSubDisc":U           + ipcContainerName, "wsLookupButton":U, "18":U, "":U                                   , "":U         , 4, 1, "Provider Sub-Discipline:":U)
    oControl:CellLayoutMask            = "&1&2":U
    oControl:LookupWobFLA              = "masubdisc":U
    oControl:LookupFields              = "subdisc.subdisp-code":U
    oControl:LookupControls            = "fcProvSubDisc":U + ipcContainerName
    oControl:FilterFields              = "subdisc.subdisp-code,subdisc.pr-type":U
    oControl:FilterControls            = "fcProvSubDisc":U + ipcContainerName + ",fcProvDisc":U + ipcContainerName
    oControl:ReturnFields              = "subdisc.subdisp-code":U
    oControl:ReturnControls            = "fcProvSubDisc":U + ipcContainerName

    oControl                           = opoContainer:addControl("fcAttSubDisc":U            + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.sub_pr_type":U       , "integer":U  , 4, 2, "Attending Sub-Discipline:":U)
    oControl:ControlClass              = "+clMaxLength:3":U                                                                                                                         
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthAttProviderSubDiscipline":U

    oControl                           = opoContainer:addControl("buAttSubDisc":U            + ipcContainerName, "wsLookupButton":U, "18":U, "":U                                   , "":U         , 4, 2, "Attending Sub-Discipline:":U)
    oControl:CellClass                 = "+clHid +clAtt":U
    oControl:LabelClass                = "+clHid +clAtt":U
    oControl:CellLayoutMask            = "&1&2":U
    oControl:LookupWobFLA              = "masubdisc":U
    oControl:LookupFields              = "subdisc.subdisp-code":U
    oControl:LookupControls            = "fcProvSubDisc":U + ipcContainerName
    oControl:FilterFields              = "subdisc.subdisp-code,subdisc.pr-type":U
    oControl:FilterControls            = "fcAttSubDisc":U + ipcContainerName + ",fcAttDisc":U + ipcContainerName
    oControl:ReturnFields              = "subdisc.subdisp-code":U
    oControl:ReturnControls            = "fcProvSubDisc":U + ipcContainerName
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fcProvName":U              + ipcContainerName, "wsInput":U       , "18":U, "group_doctor.name":U                  , "character":U, 5, 1, "Provider Name:":U)

    oControl                           = opoContainer:addControl("fcAttName":U               + ipcContainerName, "wsInput":U       , "18":U, "attending_doctor.name":U              , "character":U, 5, 2, "Attending Name:":U)
    oControl:CellClass                 = "+clHid +clAtt":U
    oControl:LabelClass                = "+clHid +clAtt":U

    oControl                           = opoContainer:addControl("fcProvBaseRate":U          + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.default_base_rate":U , "character":U, 6, 1, "Default Base Rate:":U)
    oControl:ControlToken              = "Readonly":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "BaseRateProvider":U  

    oControl                           = opoContainer:addControl("fcBaseRate":U              + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.override_base_rate":U , "character":U, 6, 2, "Override Base Rate:":U) 
    oControl:ControlToken              = "Readonly":U

    oControl                           = opoContainer:addControl("fcProvArsRate":U           + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.default_ars_rate":U   , "character":U, 7, 1, "Default Ars Rate:":U)
    oControl:ControlToken              = "Readonly":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "ArsRateProvider":U  

    oControl                           = opoContainer:addControl("fcArsRate":U               + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.override_ars_rate":U ,  "character":U, 7, 2, "Override Ars Rate:":U)
    oControl:ControlToken              = "ReadOnly":U

    oControl                           = opoContainer:addControl("fcProvNegGroup":U          + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.group_doc_num":U     , "character":U, 8, 1, "Provider Negotiation Group:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "ProviderNegGroup|Field":U
    oControl:ControlToken              = "Readonly":U
    
    oControl                           = opoContainer:addControl("fcAttNegGroup":U           + ipcContainerName, "wsInput":U       , "18":U, "tt_auth_provider.doc_num":U           , "character":U, 8, 2, "Attending Provider Negotiation Group:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "ProviderNegGroup|Field":U
    oControl:CellClass                 = "+clHid +clAtt":U
    oControl:LabelClass                = "+clHid +clAtt":U
    oControl:ControlToken              = "Readonly":U

    oControl                           = opoContainer:addControl("fdAuthGroupObj":U         + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.auth_group_obj":U    , "character":U , 9, 1, "Auth Group:":U)
    oControl:ControlToolTip            = "Groups the Providers and Clinical Details to a specific Authorisation group. Only one of these Providers will be allowed to be claimed from.":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthGroupCode":U                                                                                                                                                                                
    .

  ASSIGN                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbStatus":U                + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.auth_status":U       , "integer":U  , 9, 2, "":U)
    oControl:ControlTooltip            = "Select a status from the drop-down list.":U                                                                                                   
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthStatusCombo":U                                                                                                                        
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                    
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationProvider:Status":U                                                                                      
    oControl:FilterFields              = "[Status],[AuthObj]":U                                                                                                                     
    oControl:FilterControls            = "cbStatus":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName                                                                 
    oControl:ReturnFields              = "[StatusReasonMandatory]":U                                                                                                                
    oControl:ReturnControls            = "flStatusReasonMandatory":U + ipcContainerName  
 
    /* On status change, check whether reason is mandatory or not */                                                                                                                                                                           
    oControl:JavaScriptOnChange        = "fnOnChangeAuthProviderStatus(this,~"form~", ~"" + ipcContainerName + "~" );":U                                                            
                                       + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"form~");":U                                                            

    oControl                           = opoContainer:addControl("flStatusReasonMandatory":U + ipcContainerName, "wsInput":U       , "15":U, "":U                                   , "character":U, 9, 2, "Authorisation Status:":U)
    oControl:ControlToken              = "Hidden":U              
    oControl:CellLayoutMask            = "&1&2":U
    
    /* On status change, check whether reason is mandatory or not */                                                                                                                                                                           
    oControl:JavaScriptOnChange        = "fnOnChangeAuthProviderStatus(this,~"form~", ~"" + ipcContainerName + "~" );":U                                                            
                                       + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"form~");":U                                                            

    oControl                           = opoContainer:addControl("fdStartDate":U             + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.start_date":U        , "date":U     , 10, 1, "Start Date:":U)
    oControl                           = opoContainer:addControl("cbStartAmPm":U             + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.start_ampm":U        , "LOGICAL":U, 10, 1, "Start Date:":U)
    oControl:AdditionalItems           = "=?|AM=AM|PM=PM":U                                                                                                                              
    oControl:CellLayoutMask            = "&1&2":U                                                                                                                                        
    oControl:ControlClass              = "+clSelVertAlign":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "DateAmPm":U  
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fcReason":U                + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.auth_status_note":U  , "character":U, 10, 2, "Status Reason:":U)
    oControl:ControlTooltip            = "Status Reason":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthProviderStatusNote":U 
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationProvider:Reason":U                                                                                        
    oControl:FilterFields              = "[ReasonCode],[ReasonType]":U                                                                                                            
    oControl:FilterControls            = "fcReason":U + ipcContainerName + ",fcReasonType":U + ipcContainerName                                                                   
    oControl:ReturnFields              = "[ReasonDesc]":U                                                                
    oControl:ReturnControls            = "fcReasonDesc":U + ipcContainerName                                                                                                                                                                                                                     
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fcReasonTypePrefix":U      + ipcContainerName, "wsInput":U       , "5":U , "":U                                   , "character":U, 10, 2, "":U)                  
    oControl:ControlToken              = "Hidden":U                                                                                                                                              
    oControl:ControlValue              = "AS":U      
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthReasonTypePrefix":U

    oControl                           = opoContainer:addControl("fcReasonDesc":U            + ipcContainerName, "wsInput":U       , "5":U   , "":U                                 , "character":U, 10, 2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                                                                                                                                          
                                   
    /* Set reason description as tooltip when reason is selected client side */ 
    oControl:JavascriptOnChange        = "fnSetReasonDescription(this, ~"fcReason":U + ipcContainerName + "~", ~"buReasonBtn":U + ipcContainerName + "~", ~"FORM~");":U             
    
    oControl                           = opoContainer:addControl("fcReasonType":U            + ipcContainerName, "wsInput":U       , "5":U   , "":U                                 , "character":U, 10, 2, "":U)                  
    oControl:ControlToken              = "Hidden":U                                                                                                                                 
    oControl:ControlValue              = "AS*":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthProviderReasonType":U
                                                                                                                                             
    oControl                           = opoContainer:addControl("buReasonBtn":U             + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                   , "":U         , 10, 2, "Status Reason:":U)                                                                                                                                                                                                   
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthProviderStatusNote":U                                                                                                                 
    oControl:CellLayoutMask            = "&1&2&3&4&5":U                                                                                                                                       
    oControl:LookupWobFLA              = "note":U                                                                                                                                         
    oControl:LookupFields              = "note.key":U                                                                                                                                       
    oControl:LookupControls            = "fcReason":U + ipcContainerName                                                                                                                    
    oControl:FilterFields              = "note.key,note.type":U                                                                                                                                  
    oControl:FilterControls            = "fcReason":U + ipcContainerName + ",fcReasonType":U + ipcContainerName                                                                                  
    oControl:ReturnFields              = "note.key":U                                                                                                                                           
    oControl:ReturnControls            = "fcReason":U + ipcContainerName

    oControl                           = opoContainer:addControl("fdEndDate":U               + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.end_date":U          , "date":U     , 11, 1, "End Date:":U)
    oControl                           = opoContainer:addControl("cbEndAmPm":U               + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.end_ampm":U          , "logical":U, 11, 1, "End Date:":U)
    oControl:AdditionalItems           = "=?|AM=AM|PM=PM":U                                                                                                                              
    oControl:CellLayoutMask            = "&1&2":U                                                                                                                                        
    oControl:ControlClass              = "+clSelVertAlign":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "DateAmPm":U  

    oControl                           = opoContainer:addControl("cbSystemStatus":U          + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.auth_status":U       , "integer":U  , 11, 2, "System Status:":U)
    oControl:AdditionalItems           = "=|":U + fnPrepareStatus("System":U)                                                                                                       
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                    
    oControl:ControlToken              = "Disabled":U 
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fiClaimCode":U             + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.claim_code":U        , "integer":U  , 12, 1, "Claim Code:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthProviderClaimCode":U                                                                                                                  
    oControl:ControlClass              = "+clNumericOnly +clMaxLength:5":U       

    oControl                           = opoContainer:addControl("lkpClaimCode":U            + ipcContainerName, "wsLookupButton":U, "15":U, "":U                                   , "character":U, 12, 1, "Claim Code:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthProviderClaimCode":U                                                                                                                  
    oControl:CellLayoutMask            = "&1&2":U                                                                                                                                                               
    oControl:LookupWobFLA              = "maclc":U                                                                                                                                                              
    oControl:LookupFields              = "ccdesc.claim-code":U                                                                                                                                                  
    oControl:LookupControls            = "fiClaimCode":U + ipcContainerName                                                                                                                                     
    oControl:FilterFields              = "ccdesc.claim-code":U                                                                                                                                                  
    oControl:FilterControls            = "fiClaimCode":U + ipcContainerName                                                                                                         
    oControl:ReturnFields              = "ccdesc.claim-code":U                                                                                                                                                  
    oControl:ReturnControls            = "fiClaimCode":U + ipcContainerName                                                                                                         

    oControl                           = opoContainer:addControl("cbPrintDM":U               + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.print_dm":U          , "character":U, 12, 2, "Print Indicator:":U)
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                          
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthPrintLetter:":U                                                                                                                    
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("cbClaimType":U             + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.claim_type":U        , "character":U, 13, 1, "Claim Type:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                            
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                        
    oControl:RenderArgument            = "AuthProviderClaimType":U                                                                                                                  
    oControl:AdditionalItems           = "|C=C|N=N|A=A|K=K|P=P|O=O":U                                                                                                                                              
    oControl:ControlToolTip            = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther":U                                                                         
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                    

    oControl                           = opoContainer:addControl("fcAccountReference":U      + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.account_reference":U , "character":U, 13, 2, "Account Reference:":U)                                                                                                                       
    
    oControl                           = opoContainer:addControl("cbPayeeDM":U               + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_provider.payee_dm":U          , "character":U, 14, 1, "Payee Indicator:":U)
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthPayee' BY mic_acronym.acronym_label":U                                             
    oControl:KeyField                  = "mic_acronym.acronym_key":U                                                                                                                                 
    oControl:DisplayFields             = "mic_acronym.acronym_label":U                                                                                                                               
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "AuthProviderPayeeIndicator":U                                                                                                                              
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                                     
                                                                                                                                                                                                     
    oControl                           = opoContainer:addControl("flAuthorised":U            + ipcContainerName, "wsCheckbox":U    , "18":U, "tt_auth_provider.authorised_service":U,     "logical":U  , 14, 2, "Authorised Service:":U)

    oControl                           = opoContainer:addControl("flAuthoriseAllServices":U  + ipcContainerName, "wsCheckbox":U    , "18":U, "tt_auth_provider.authorise_all_services":U, "logical":U  , 15, 1, "Authorise All Services:":U)
    oControl:ControlToolTip            = "Indicates that only authorised services on authorisation clinical details will be paid.":U 
    
    oControl                           = opoContainer:addControl("fcCopayOverrideNote":U     + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_provider.copay_override_note":U , "INTEGER":U    ,15, 2, "":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                            
    oControl:RenderArgument            = "CopayOverrideNote":U        
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationProvider:OverrideNote":U                                                                                          
    oControl:FilterFields              = "[ReasonCode],[ReasonType],[ProviderObj]":U                                                                                                         
    oControl:FilterControls            = "fcCopayOverrideNote":U + ipcContainerName + ",":U 
                                       + "fcOverrideNoteTypeArgument":U + ipcContainerName + ",":U
                                       + "fdAuthProviderObj":U + ipcContainerName 
    oControl:ReturnFields              = "[ReasonDesc]":U                                                                                                                  
    oControl:ReturnControls            = "fcOverrideNoteDesc":U + ipcContainerName                                                                                 

    oControl                           = opoContainer:addControl("fcOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U,15, 2, "":U)                  
    oControl:ControlToken              = "Hidden":U
    oControl:ControlValue              = "AO":U
    oControl:ControlClass              = "+clPreserveValue":U
    
    oControl                           = opoContainer:addControl("fcOverrideNoteDesc":U         + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U,15, 2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                     
                                                                                                                                                                                           
    /* Set copay override note description as tooltip when note is selected client side */ 
    oControl:JavascriptOnChange        = "fnSetReasonDescription(this, ~"fcCopayOverrideNote":U + ipcContainerName + "~", ~"buOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U  
    
    oControl                           = opoContainer:addControl("buOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         ,15, 2, "Co-payment Override<br> Reason:":U)
    oControl:ControlTooltip            = "Lookup on Copay Override Reasons.":U
    oControl:CellLayoutMask            = "&1&2&3&4":U                                                                                                                                                      
    oControl:LookupWobFLA              = "note":U                                                                                                                                                    
    oControl:LookupFields              = "note.key":U                                                                                                                                                
    oControl:LookupControls            = "fcCopayOverrideNote":U + ipcContainerName                                                                                                                             
    oControl:FilterFields              = "note.key,note.type":U                                                                                                                                      
    oControl:FilterControls            = "fcCopayOverrideNote":U + ipcContainerName + ",":U
                                       + "fcOverrideNoteTypeArgument":U + ipcContainerName                                                                                      
    oControl:ReturnFields              = "note.key":U                                                                                                                                           
    oControl:ReturnControls            = "fcCopayOverrideNote":U + ipcContainerName      
    
    oControl                           = opoContainer:addControl("cbEmergencyFlagValue":U   + ipcContainerName, "wsCombo":U       , "10":U , "tt_auth_provider._emergency_flag":U    , "CHARACTER":U  , 16,2, "Emergency Flag:":U)
    oControl:ControlTooltip            = "Select a emergency flag value":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "EmergencyFlagValue":U
    oControl:JavascriptOnChange        = "fnOnChangeEmergencyFlagValue(this) ;"
                                     
    oControl                           = opoContainer:addControl("flEmergencyFlagValueUpdated":U + ipcContainerName, "wsInput":U     , "10":U , "tt_auth_provider._emergency_flag_updated":U , "LOGICAL":U  , 16,2, "Emergency Flag:":U)
    oControl:ControlToken              = "Hidden":U 
    oControl:CellLayoutMask            = "&1&2":U    
   .

  ASSIGN
    oControl                           = opoContainer:addControl("flPenaltyValueUpdated":U + ipcContainerName, "wsInput":U     , "10":U , "tt_auth_provider._penalty_flag_updated":U , "LOGICAL":U  , 17,2, "Penalty Flag:":U)
    oControl:ControlToken              = "Hidden":U 
    oControl:CellLayoutMask            = "&1&2":U

    oControl                           = opoContainer:addControl("cbPenaltyValue":U   + ipcContainerName, "wsCombo":U       , "10":U , "tt_auth_provider._penalty_flag":U    , "CHARACTER":U  , 17,2, "Penalty Flag:":U)
    oControl:ControlTooltip            = "Please select a valid value for the Penalty Flag":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "EmergencyFlagValue":U
    oControl:JavascriptOnChange        = "fnOnChangePenaltyFlagValue(this);":U
    
    oControl                           = opoContainer:addControl("fcPenaltyOverrideNote":U        + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_provider._penalty_override_note":U , "INTEGER":U  , 18,2, "":U)
    oControl:ControlTooltip            = "Please select a Co-payment Penalty Status Reason":U    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "EmergencyFlagValue":U     
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationProvider:OverrideNote":U                                                                                          
    oControl:FilterFields              = "[ReasonCode],[ReasonType],[ProviderObj]":U                                                                                                         
    oControl:FilterControls            = "fcPenaltyOverrideNote":U + ipcContainerName + ",":U 
                                       + "fcPenaltyOverrideNoteTypeArgument":U + ipcContainerName + ",":U
                                       + "fdAuthProviderObj":U + ipcContainerName 
    oControl:ReturnFields              = "[ReasonDesc]":U                                                                                                                  
    oControl:ReturnControls            = "   ":U + ipcContainerName 

    oControl                           = opoContainer:addControl("fcPenaltyOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 18,2, "":U)                  
    oControl:ControlToken              = "Hidden":U
    oControl:ControlValue              = "AP":U
    oControl:ControlClass              = "+clPreserveValue":U

    oControl                           = opoContainer:addControl("fcPenaltyOverrideNoteDesc":U         + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 18,2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                     

    /* Set copay override note description as tooltip when note is selected client side */ 
    oControl:JavascriptOnChange        = "fnSetReasonDescription(this, ~"fcPenaltyOverrideNote":U + ipcContainerName + "~", ~"buPenaltyOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U  

    oControl                           = opoContainer:addControl("buPenaltyOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 18,2, "Penalty Override Reason:":U)
    oControl:ControlTooltip            = "Lookup on Penalty Override Reasons.":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "PenaltyOverrideBtn":U
    oControl:CellLayoutMask            = "&1&2&3&4":U                                                                                                                                                      
    oControl:LookupWobFLA              = "note":U                                                                                                                                                    
    oControl:LookupFields              = "note.key":U                                                                                                                                                
    oControl:LookupControls            = "fcPenaltyOverrideNote":U + ipcContainerName                                                                                                                             
    oControl:FilterFields              = "note.key,note.type":U                                                                                                                                      
    oControl:FilterControls            = "fcPenaltyOverrideNote":U + ipcContainerName + ",":U
                                       + "fcPenaltyOverrideNoteTypeArgument":U + ipcContainerName                                                                                      
    oControl:ReturnFields              = "note.key":U                                                                                                                                           
    oControl:ReturnControls            = "fcPenaltyOverrideNote":U + ipcContainerName
    
    
    oControl                           = opoContainer:addControl("PmbContainer":U               + ipcContainerName, "":U              , "":U  , "":U                               , "":U         , 19, 1, "":U)                                                                                                                               
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "DecisionPMBRender":U                                                                                                                                                                                               
    oControl:SubContainer              = mipEnv:Health:maUIService:getCntUpdAuthProvPmb("Pmb":U + ipcContainerName)                                                                                  
    oControl:SubContainerType          = "FORM":U                                                                                                                                                    
    oControl:SpanOverLabel             = TRUE                                                                                                                                                        
    oControl:ColumnSpan                = 2                                                                                                                                                           
    oControl:CellSnippet               = "align='center'":U    

    
                                                                                                                                                                                                     
    oControl                           = opoContainer:addControl("FinancialContainer":U      + ipcContainerName, "":U              , "":U  , "":U                                   , "":U         , 20, 1, "":U)                                                                                                                               
                                                                                                                                                                                                     
    oControl:SubContainer              = mipEnv:Health:maUIService:getCntUpdAuthProvFinancial("Financial":U + ipcContainerName)                                                                      
    oControl:SubContainerType          = "FORM":U                                                                                                                                                    
    oControl:SpanOverLabel             = TRUE                                                                                                                                                        
    oControl:ColumnSpan                = 2                                                                                                                                                           
    oControl:CellSnippet               = "align='center'":U                                                                                                                                          
                                                                                                                                                                                                     
    oControl                           = opoContainer:addControl("SavingsContainer":U        + ipcContainerName, "":U              , "":U  , "":U                                   , "":U         , 21, 1, "":U)                                                                                                                               
                                                                                                                                                                                                     
    oControl:SubContainer              = mipEnv:Health:maUIService:getCntUpdAuthProvSavings("Savings":U + ipcContainerName)                                                                          
    oControl:SubContainerType          = "FORM":U                                                                                                                                                    
    oControl:SpanOverLabel             = TRUE                                                                                                                                                        
    oControl:ColumnSpan                = 2                                                                                                                                                           
    oControl:CellSnippet               = "align='center'":U                                                                                                                                          
                                                                                                                                                                                                     
    oControl                           = opoContainer:addControl("frmSubmit":U               + ipcContainerName, "":U              , "":U  ,"":U                                    , "character":U, 22, 1, "":U)
    oControl:SpanOverlabel             = TRUE
    oControl:CellClass                 = WarpSpeed:BaseClass + "ButtonBar":U
    oControl:CellSnippet               = "align='right'":U
    oControl:ColumnSpan                = 3
    oControl:SubContainer              = wsUiService:getButtonContainer(WarpSpeed:CurrentWob + ipcContainerName + "BtnBar":U, "Submit:Clear")
                                                                                          
    oControl                           = opoContainer:addControl("_configuration":U          + ipcContainerName, "wsInput":U       , "15":U, "":U                                   , "character":U, 24, 1, "_configuration:":U)
    oControl:ControlToken              = "Hidden":U
    oControl:JavascriptOnChange        = "fnOnChangeAuthConfiguration(this);":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthConfiguration":U

    oControl                           = opoContainer:addControl("_authObjArgument":U        + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_provider.auth_obj":U          , "character":U, 25, 1, "_authobj:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                   

    oControl                           = opoContainer:addControl("_authMode":U               + ipcContainerName, "wsInput":U       , "10":U, "":U                                   , "character":U, 26, 1, "_authMode:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                                                  
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "AuthMode":U         

    oControl                           = opoContainer:addControl("fcDisciplineUpdatable":U   + ipcContainerName, "wsInput":U       , "12":U , "":U                                  , "character":U, 98, 1, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                              
    /* This function will disable discipline fields when a valid provider is captured */                                                                                            
    oControl:JavascriptOnChange        = "fnOnChangeDisciplineUpdatable(this,~"~");":U                                                                                           
                                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fcAttDisciplineUpdatable":U + ipcContainerName,"wsInput":U       , "12":U , "":U                                  , "character":U, 99, 1, "":U)
    oControl:ControlToken              = "Hidden":U 
    /* This function will disable Atteinding discipline fields when a valid Attending provider is captured */
    oControl:JavascriptOnChange        = "fnOnChangeDisciplineUpdatable(this,~"Attending~");":U                                                                                                                                  
    .

&ENDIF

  { mip/inc/mipcatcherror.i } 
