/* maauthgetcntupdauth.i MEDSTAR Medical Aid System
                         Authorisation Container Procedure ( getCntUpdAuth )
                         (c) Copyright 1990 - 2021
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/
  
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN  
                                   
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE iControl             AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE cContext             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cControlNameList     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cQueryFieldList      AS CHARACTER                 NO-UNDO.  
  DEFINE VARIABLE cUsageString         AS CHARACTER                 NO-UNDO. 
  

  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "80%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle        = "Authorisation Header Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth NO-LOCK":U
                                       + "   WHERE tt_auth.auth_obj = '&1',":U
                                       + "   FIRST tt_auth_type NO-LOCK":U 
                                       + "   WHERE tt_auth_type.auth_type_obj = tt_auth.auth_type_obj,":U
                                       + "   FIRST tt_auth_episode NO-LOCK":U 
                                       + "   WHERE tt_auth_episode.auth_episode_obj = tt_auth.auth_episode_obj OUTER-JOIN":U
                                          
    oControl                           = opoContainer:addControl("cbOptionCode":U            + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.option_code":U              , "CHARACTER":U, 1 , 1, "Option Code:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl                           = opoContainer:addControl("fcOptionCode":U            + ipcContainerName, "wsInput":U       , "15":U, "tt_auth._option_code":U             , "CHARACTER":U, 1 , 1, "Option Code:":U)
    oControl:ControlToken              = "ReadOnly":U                                                                                                                             
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "OptionCodeRender":U                                                                                                                     
    oControl:CellLayoutMask            = "&1&2":U                                                                                                                                 
    oControl:ControlToolTip            = "Please enter a valid Option Code":U                                                                                                                                                  
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbClient":U                + ipcContainerName, "wsCombo":U       , "20":U, "tt_auth.insurer_obj":U              , "CHARACTER":U, 1 , 2, "Client:":U)
    oControl:AdditionalItems           = "=":U                                                                                                                                                        
    oControl:Querystring               = "FOR EACH erm_insurer NO-LOCK BY erm_insurer.insurer_code":U                                                                                                 
    oControl:KeyField                  = "erm_insurer.insurer_obj":U                                                                                                                                  
    oControl:DisplayFields             = "erm_insurer.insurer_code":U                                                                                                                                 
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                  
    oControl:ControlToolTip            = "Please choose a valid Client":U                                                                                                         
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("flDependantUpdatable":U    + ipcContainerName, "wsInput":U       , "20":U, "tt_auth._auth_dependant_updatable":U, "CHARACTER":U, 2 , 1, "Dependant:":U)
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("fiDependant":U             + ipcContainerName, "wsCombo":U       , "20":U, "tt_auth.dependant":U                , "CHARACTER":U, 2 , 1, "Dependant:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "DependantList":U                                                                                                                        
    oControl:ControlToolTip            = "Please choose a valid Dependant.":U                                                                                                     
    oControl:CellLayoutMask            = "&1&2":U 
    oControl:JavaScriptOnChange        = "fnOnChangeDependant(this); "
                                    
    oControl                           = opoContainer:addControl("fiDOBDependant":U             + ipcContainerName, "wsInput":U       , "25":U, "tt_auth.dependant":U             , "CHARACTER":U, 2 , 2, "Dependant DOB:":U)
    oControl:ControlToken              = "ReadOnly":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle   
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "DOBDisplay":U 

    oControl                           = opoContainer:addControl("fcDependantReference":U    + ipcContainerName, "wsInput":U       , "20":U, "tt_auth.dependant_reference":U       , "CHARACTER":U, 3 , 1, "Dependant Reference:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "DependantReference":U
    oControl:ControlToolTip            = "Please capture a value to identify a Dependant. Used for Dependant 99.":U
	

    oControl                           = opoContainer:addControl("fdRequestDate":U           + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.request_date":U             , "DATE":U     , 4 , 1, "Request Date:":U)    
    oControl:ControlToolTip            = "Please enter a valid Date.":U                                                                                                           
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fdCreateDate":U            + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.auth_date":U                , "DATE":U     , 4 , 2, "Create Date:":U)  
    oControl:ControlToken              = "ReadOnly":U                                                                                                                             
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbRequestedBy":U           + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.request_by":U               , "CHARACTER":U, 5 , 1, "Request By:":U)
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthRequestByType:=":U                                                                                                                    
    oControl:ControlClass              = "+clMan":U                                                                                                                               
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                  
    oControl:ControlToolTip            = "Please choose a valid Requested By Entity":U                                                                                                
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcCreatedBy":U             + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.user_id":U                  , "CHARACTER":U, 5 , 2, "Created By:":U)
    oControl:ControlToken              = "ReadOnly":U                                                                                                                             
    oControl:ControlToolTip            = "Authorisation Created By":U                                                                                                             
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbRequestedSource":U       + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.request_source":U           , "CHARACTER":U, 6 , 1, "Request Source:":U)
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                       
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthRequestSource:None=":U                                                                                           
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                  
    oControl:ControlToolTip            = "Please choose a valid Request Source":U                                                                                                 
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcRequestedSourceDetail":U + ipcContainerName, "wsInput":U       , "40":U, "tt_auth.request_source_details":U   , "CHARACTER":U, 6 , 1, "Request Source:":U)
    oControl:CellLayoutMask            = "&1<br>&2":U                                                                                                                             
    oControl:ControlToolTip            = "Request Source Details":U                                                                                                               
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fdAuthEpisodeObj":U        + ipcContainerName, "wsInput":U       , "20":U, "tt_auth.auth_episode_obj":U         , "DECIMAL":U  , 6 , 2, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcEpisodeNumber":U         + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_episode.episode_num":U      , "CHARACTER":U, 6 , 2, "Episode Number:":U)
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationAuth:Episode":U                                                                                   
    oControl:FilterFields              = "[EpisodeNumber]":U                                                                                                                  
    oControl:FilterControls            = "fcEpisodeNumber":U + ipcContainerName                                                                                               
    oControl:ReturnFields              = "[EpisodeObj]":U                                                                                                                     
    oControl:ReturnControls            = "fdAuthEpisodeObj":U + ipcContainerName                                                                                              
    oControl:ControlToolTip            = "Please enter a valid Episode Number":U                                                                                             
                                                                                                                                                                              
    oControl                           = opoContainer:addControl("buEpisodeNumber":U         + ipcContainerName, "wsLookupButton":U, "20":U, "":U                                 , "CHARACTER":U, 6 , 2, "Episode Number:":U)
    oControl:LookupWobFLA              = "hatae":U                                                                                                                                               
    oControl:LookupFields              = "hat_auth_episode.auth_episode_obj":U                                                                                                                         
    oControl:LookupControls            = "fdAuthEpisodeObj":U + ipcContainerName                                                                                                                    
    oControl:FilterFields              = "hat_auth_episode.episode_num,hat_auth_episode.dependant,hat_auth_episode.mem_num":U                                                                                                                          
    oControl:FilterControls            = "fcEpisodeNumber":U + ipcContainerName + ",fiDependant":U + ipcContainerName + ",_memberNumber":U + ipcContainerName                      
    oControl:ReturnFields              = "hat_auth_episode.auth_episode_obj,hat_auth_episode.episode_num":U                                                                       
    oControl:ReturnControls            = "fdAuthTypeObj":U + ipcContainerName + ",fcEpisodeNumber":U + ipcContainerName                                                           
    oControl:CellLayoutMask            = "&1&2&3":U                                                                                                                               
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcReferenceNumberDisp":U   + ipcContainerName, "wsInput":U       , "40":U, "tt_auth.reference_auth_num":U       , "CHARACTER":U, 7 , 1, "Reference Authorisation Number:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "RefNumUpdatable":U                                                                                                                      
    oControl:ControlValue              = get-value("fcReferenceNumber":U + ipcContainerName)                                                                                      
    oControl:ControlToolTip            = "Please enter a valid Reference Authorisation Number.":U                                                                                 
    oControl:JavascriptOnChange        = "fnSetControlValue(~"fcReferenceNumber":U + ipcContainerName + "~",this.value);":U                                                       
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcReferenceNumber":U       + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.reference_auth_num":U       , "CHARACTER":U, 7 , 1, "Reference Authorisation Number:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:CelllayoutMask            = "&1&2":U                                                                                                                                 
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcAuthorisationNumber":U   + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.auth_num":U                 , "CHARACTER":U, 7 , 2, "Authorisation Number:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                             
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                         
    oControl:RenderArgument            = "AuthNumViewableMaint":U         
    oControl                           = opoContainer:addControl("fcAuthNumViewable":U       + ipcContainerName, "wsInput":U       , "15":U, "tt_auth._authnum_viewable":U        , "CHARACTER":U, 7 , 2, "Authorisation Number:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:CelllayoutMask            = "&1&2":U

    oControl                           = opoContainer:addControl("fcAuthTypeGroup":U         + ipcContainerName, "wsInput":U     , "15":U, "tt_auth_type.auth_type_group":U       , "CHARACTER":U, 8 , 1, "":U)                                                                                                                                                                                                                                                                                                                                                  
    oControl:ControlToken              = "Hidden":U                                                                                                                               
                                                                                                                                                                                                                                                                                                                                             
    oControl                           = opoContainer:addControl("fdAuthTypeObj":U           + ipcContainerName, "wsInput":U     , "15":U, "tt_auth.auth_type_obj":U              , "DECIMAL":U  , 8 , 1, "":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    
    oControl                           = opoContainer:addControl("flUpdatesAllowed":U        + ipcContainerName, "wsInput":U     , "15":U, "":U                                   , "LOGICAL":U  , 8 , 1, "":U)
    oControl:ControlValue              = "YES":U
    oControl:ControlToken              = "Hidden":U
                                                                                                                                                                      
    oControl                           = opoContainer:addControl("fcAuthTypeCode":U          + ipcContainerName, "wsInput":U     , "15":U, "tt_auth_type.auth_type":U             , "CHARACTER":U, 8 , 1, "":U)
    oControl:ControlClass              = "+clMan":U 
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationAuth:AuthType":U                                                                                                         
    oControl:FilterFields              = "[MemberNumber],[Dependant],[AuthTypeCode],[AuthTypeGroup],[InsurerObj],[OptionCode],[StartDate],[UpdatesAllowed]":U                                                                                                      
    oControl:FilterControls            = "_memberNumber":U    + ipcContainerName + ",fiDependant":U      + ipcContainerName + ",fcAuthTypeCode":U + ipcContainerName                   
                                       + ",fcAuthTypeGroup":U + ipcContainerName + ",cbClient":U         + ipcContainerName + ",cbOptionCode":U   + ipcContainerName                   
                                       + ",fdStartDate":U     + ipcContainerName + ",flUpdatesAllowed":U + ipcContainerName                                                                                              
    oControl:ReturnFields              = "[AuthTypeObj],[AuthTypeCode],[AuthTypeDesc],[OptionCode],[OptionDisplay],[Configuration],[ValidateAuthTypeUsage],[ValidateAuthUpdatesAllowed]":U                                    
    oControl:ReturnControls            = "fdAuthTypeObj":U    + ipcContainerName + ",fcAuthTypeCode":U  + ipcContainerName                                                         
                                       + ",fcAuthTypeDesc":U  + ipcContainerName + ",cbOptionCode":U    + ipcContainerName 
                                       + ",fcOptionCode":U    + ipcContainerName + ",_configuration":U  + ipcContainerName 
                                       + ",_errorOrWarning":U + ipcContainerName + ",_errorOrWarning":U + ipcContainerName                  
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthTypeRender":U                                 
    oControl:JavascriptOnChange        = "fnOnChangeAuthType();":U
    oControl:ControlToolTip            = "Please enter a valid Authorisation Type.":U                                                                                             
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("buAuthTypeBtn":U           + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                 , "":U         , 8 , 1, "":U)
    oControl:LookupWobFLA              = "hacat":U                                                                                                                                                 
    oControl:LookupFields              = "hac_auth_type.auth_type_obj":U                                                                                                                           
    oControl:LookupControls            = "fdAuthTypeObj":U + ipcContainerName                                                                                                                      
    oControl:FilterFields              = "hac_auth_type.auth_type,hac_auth_type.auth_type_group,hac_auth_type.updates_allowed":U                                                                                                                              
    oControl:FilterControls            = "fcAuthTypeCode":U + ipcContainerName + ",fcAuthTypeGroup":U + ipcContainerName + ",flUpdatesAllowed":U + ipcContainerName                                                        
    oControl:ReturnFields              = "hac_auth_type.auth_type_obj,hac_auth_type.auth_type,hac_auth_type.description":U                                                                         
    oControl:ReturnControls            = "fdAuthTypeObj":U + ipcContainerName + ",fcAuthTypeCode":U + ipcContainerName + ",fcAuthTypeDesc":U + ipcContainerName                                    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthTypeRender":U                                                                                                                       
    oControl                           = opoContainer:addControl("fcAuthTypeDesc":U          + ipcContainerName, "wsSpan":U        , "15":U, "tt_auth_type.description":U         , "CHARACTER":U, 8 , 1, "Authorisation Type:":U)
    oControl:CellLayoutMask            = "&1&2&3&4&5 &6":U                                                                                                                      
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fdStartDate":U             + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.start_date":U               , "DATE":U     , 9 , 1, "Authorisation Start Date:":U)                                                                                                                                                                                                                     
    oControl:ControlClass              = "+clMan":U     
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationAuth:AuthType":U                                                                                                         
    oControl:FilterFields              = "[MemberNumber],[Dependant],[AuthTypeCode],[InsurerObj],[OptionCode],[StartDate],[AuthTypeGroup]":U                                                                                                    
    oControl:FilterControls            = "_memberNumber":U    + ipcContainerName + ",fiDependant":U  + ipcContainerName + ",fcAuthTypeCode":U + ipcContainerName                    
                                       + ",cbClient":U        + ipcContainerName + ",cbOptionCode":U + ipcContainerName + ",fdStartDate":U    + ipcContainerName + ",fcAuthTypeGroup":U + ipcContainerName                                                                                                
    oControl:ReturnFields              = "[AuthTypeObj],[AuthTypeDesc],[OptionCode],[OptionDisplay],[Configuration],[ValidateAuthTypeUsage],[ValidateAuthUpdatesAllowed]":U                                    
    oControl:ReturnControls            = "fdAuthTypeObj":U    + ipcContainerName  + ",fcAuthTypeDesc":U  + ipcContainerName                                                          
                                       + ",cbOptionCode":U    + ipcContainerName  + ",fcOptionCode":U    + ipcContainerName                                                            
                                       + ",_configuration":U  + ipcContainerName  + ",_errorOrWarning":U + ipcContainerName 
                                       + ",_errorOrWarning":U + ipcContainerName                                                         
    oControl:ControlToolTip            = "Please specify a valid Start Date":U
    oControl:JavascriptOnChange        = "fnOnChangeAuthDate(~"fdStartDate~");":U                                                                                                                                                                                                                                                                 
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbStartAmPm":U             + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.start_ampm":U               , "LOGICAL":U  , 9 , 1, "Authorisation Start Date:":U)                                                                                                                                                                                                                     
    oControl:AdditionalItems           = "=?|AM=AM|PM=PM":U                                                                                                               
    oControl:CellLayoutMask            = "&1 &2":U                                                                                                                            
    oControl:ControlClass              = "+clSelVertAlign +clHid":U                                                                                                           
    oControl:ControlToolTip            = "Please choose AM or PM"
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "DateAmPm":U                                                                                                                      
    
    oControl                           = opoContainer:addControl("fdTotalLos":U              + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.total_los":U                , "DECIMAL":U  , 9 , 2, "Total LOS:":U)
    oControl:ControlToken              = "ReadOnly":U
    oControl:ControlToolTip            = "Total Length Of Stay"                                                 

    oControl                           = opoContainer:addControl("fdEndDate":U               + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.end_date":U                 , "DATE":U     , 10 , 1, "Authorisation End Date:":U)                                                                                                                                                                                                                                                                                                                                  
    oControl:JavascriptOnChange        = "fnOnChangeAuthDate(~"fdEndDate~");":U                                                                                                   
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthEndDate":U                                                                                                                          
    oControl:ControlToolTip            = "Please specify a valid End Date"                                                                                                                                                                                                                                                                               
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbEndAmPm":U               + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.end_ampm":U                 , "LOGICAL":U  , 10 , 1, "Authorisation End Date:":U)                                                                                                                                                                                                                     
    oControl:AdditionalItems           = "=?|AM=AM|PM=PM":U                                                                                                                       
    oControl:CellLayoutMask            = "&1 &2":U                                                                                                                                
    oControl:ControlClass              = "+clSelVertAlign +clHid":U                                                                                                           
    oControl:ControlToolTip            = "Please choose AM or PM":U                                                                                                                                                                                                                                     
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "DateAmPm":U                                                                                                                      
    
    oControl                           = opoContainer:addControl("fdDueDate":U               + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.due_date":U                 , "DATE":U     , 10 , 2, "Due Date:":U)                                                                                                                                                                                           
    oControl:ControlToolTip            = "Please enter a valid Date.":U                                                                                                                                                                                                                   
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbStatusDisp":U            + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.auth_status":U              , "INTEGER":U  , 11 , 2, "Authorisation Status:":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthStatusCombo":U                                                                                                                      
    oControl:ControlClass              = "+clMan":U                                                                                                                               
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationAuth:Status":U                                                                                        
    oControl:FilterFields              = "[StatusCode],[StartDate],[InsurerObj],[OptionCode],[AuthTypeObj]":U                                                                                                      
    oControl:FilterControls            = "cbStatus":U       + ipcContainerName + ",fdStartDate":U  + ipcContainerName                                                             
                                       + ",cbClient":U      + ipcContainerName + ",cbOptionCode":U + ipcContainerName                                                             
                                       + ",fdAuthTypeObj":U + ipcContainerName                                                                                                    
    oControl:ReturnFields              = "[StatusReasonMandatory]":U                                                                                                              
    oControl:ReturnControls            = "flStatusReasonMandatory":U + ipcContainerName                                                                                           
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                  
    oControl:JavascriptOnChange        = "fnOnChangeAuthStatus();":U                                                                                                              
                                       + "fnSetControlValue(~"cbStatus":U + ipcContainerName + "~",this.value);":U                                                                
    oControl:ControlValue              = get-value("cbStatus":U + ipcContainerName)                                                                                               
    oControl:ControlToolTip            = "Please enter a valid Authorisation Status.":U                                                                                           
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("cbStatus":U                + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.auth_status":U              , "INTEGER":U  , 11 , 2, "Authorisation Status:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("flStatusReasonMandatory":U + ipcContainerName, "wsInput":U       , "15":U, "tt_auth._reason_mandatory":U        , "CHARACTER":U, 11 , 2, "Authorisation Status:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:CellLayoutMask            = "&1&2&3":U.
  
  ASSIGN
    /* On status change, check whether reason is mandatory or not */                                                                                                                               
    oControl:JavascriptOnChange        = "if(!glRevertingStatus)~{fnOnChangeAuthStatus();~}else~{glRevertingStatus=false~}":U                                                     
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                               
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                           
    oControl:RenderArgument            = "ReasonMandatory":U                                                                                                                      
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fiClaimCodeDisp":U         + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.claim_code":U               , "INTEGER":U  , 12 , 1, "Claim Code:":U)                                                                                                                               
                                                                                                                                                                                  
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationAuth:ClaimCode":U                                                                                     
    oControl:FilterFields              = "[InsurerObj],[OptionCode],[MemberNumber],[Dependant],[ClaimCode],[AuthTypeObj],[StartDate]":U                                           
    oControl:FilterControls            = "cbClient":U         + ipcContainerName + ",cbOptionCode":U  + ipcContainerName                                                          
                                       + ",_memberNumber":U   + ipcContainerName + ",fiDependant":U   + ipcContainerName                                                          
                                       + ",fiClaimCodeDisp":U + ipcContainerName + ",fdAuthTypeObj":U + ipcContainerName                                                          
                                       + ",fdStartDate":U     + ipcContainerName                                                                                                  
    oControl:ReturnFields              = "[HelpMessage],[ValidateClaimCode],[StatusAction]":U                                                                                                                  
    oControl:ReturnControls            =  "_helpMessage":U + ipcContainerName  + ",_errorOrWarning":U + ipcContainerName   + ",_AuthStatusAction":U + ipcContainerName                                                                                            
    oControl:ControlClass              = "+clNumericOnly +clMaxLength:5":U                                                                                                        
    oControl:JavascriptonChange        = "fnSetControlValue(~"fiClaimCode":U + ipcContainerName + "~",this.value);":U                                                             
    oControl:ControlValue              = get-value("fiClaimCode":U + ipcContainerName)                                                                                            
    oControl:ControlClass              = "+clMan":U                                                                                                                               
    oControl:ControlToolTip            = "Please enter a valid Claim Code.":U                                                                                                     
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fiClaimCode":U             + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.claim_code":U               , "INTEGER":U  , 12 , 1, "Claim Code:":U)                                                                                                                               
    oControl:ControlToken              = "Hidden":U                                                                                                                               
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("buClaimBtn":U              + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                 , "":U         , 12 , 1, "Claim Code:":U)
    oControl:CellLayoutMask            = "&1&2&3":U                                                                                                                                                      
    oControl:LookupWobFLA              = "maclc":U                                                                                                                                                     
    oControl:LookupFields              = "ccdesc.claim-code":U                                                                                                                                         
    oControl:LookupControls            = "fiClaimCodeDisp":U + ipcContainerName                                                                                                                            
    oControl:FilterFields              = "ccdesc.claim-code,ccdesc.scheme-code":U                                                                                                                                         
    oControl:FilterControls            = "fiClaimCodeDisp":U + ipcContainerName + ",cbOptionCode":U + ipcContainerName                                                                                                                            
    oControl:ReturnFields              = "ccdesc.claim-code":U                                                                                                                                         
    oControl:ReturnControls            = "fiClaimCodeDisp":U + ipcContainerName                                                                                                                            
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcReason":U                + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.auth_status_note":U         , "CHARACTER":U, 12 , 2, "":U)            
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationAuth:Reason":U                                                                                        
    oControl:FilterFields              = "[ReasonCode],[ReasonType],[StatusCode]":U                                                                                                            
    oControl:FilterControls            = "fcReason":U + ipcContainerName + ",fcReasonType":U + ipcContainerName  + ",cbStatusDisp" + ipcContainerName	                                                             
    oControl:ReturnFields              = "[ReasonCode],[ReasonDesc]":U                                                                                                            
    oControl:ReturnControls            = "fcReasonCode":U + ipcContainerName + ",fcReasonDesc":U + ipcContainerName                                                     
    oControl:ControlToolTip            = "Please enter a valid Status Note.":U                                                                                                    
    oControl:JavascriptOnChange        = "fnOnChangeAuthReason();":U                                                                                                              
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthStatusNote":U   
                                                                                                                    
    oControl                           = opoContainer:addControl("fcReasonDesc":U            + ipcContainerName, "wsInput":U       , "5":U , "":U                                 , "CHARACTER":U, 12 , 2, "":U)            
    oControl:ControlToken              = "Hidden":U     

   /* Set reason description as tooltip when reason is selected client side */                                                                                                                           
    oControl:JavascriptOnChange        = "fnSetReasonDescription(this, ~"fcReason" + ipcContainerName + "~",~"buReasonBtn" + ipcContainerName + "~", ~"FRM~");":U                 
                                                                                                                                                                                                                                                                                            
    oControl                           = opoContainer:addControl("fcReasonType":U            + ipcContainerName, "wsInput":U       , "5":U , "":U                                 , "CHARACTER":U, 12 , 2, "":U)                  
    oControl:ControlToken              = "Hidden":U                                                                                                                                                    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthReasonType":U                                                                                                                                                      
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("buReasonBtn":U             + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                 , "":U         , 12 , 2, "Status Reason:":U)                                                                                                                                                                                                   
    oControl:CellLayoutMask            = "&1&2&3&4":U                                                                                                                                                 
    oControl:LookupWobFLA              = "note":U                                                                                                                                               
    oControl:LookupFields              = "note.key":U                                                                                                                                           
    oControl:LookupControls            = "fcReason":U + ipcContainerName                                                                                                                        
    oControl:FilterFields              = "note.key,note.type":U                                                                                                                                 
    oControl:FilterControls            = "fcReason":U + ipcContainerName + ",fcReasonType":U + ipcContainerName                                                                                 
    oControl:ReturnFields              = "note.key,note.narration[1]":U                                                                                                                                           
    oControl:ReturnControls            = "fcReason":U + ipcContainerName + ",fcReasonDesc":U + ipcContainerName  
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthStatusNote":U                                                                                                                       

    oControl                           = opoContainer:addControl("cbClaimTypeDisp":U         + ipcContainerName, "wsCombo":U       , "3":U , "tt_auth.claim_type":U               , "CHARACTER":U, 13 , 1, "Claim Type:":U)
    oControl:AdditionalItems           = "=|C=C|N=N|A=A|K=K|P=P|O=O":U                                                                                                                                       
    oControl:ControlToolTip            = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther":U 
    oControl:ControlJavascript         = "style='width:128px'":U      
    oControl:JavascriptOnChange        = "fnSetControlValue(~"cbClaimType":U + ipcContainerName + "~",this.value);":U    
    oControl:ControlValue              = get-value("cbClaimType":U + ipcContainerName) 
    oControl:ControlClass              = "+clMan":U
    oControl:ControlValue              = "C":U /* initialize in case user does not change claim type */ 
    oControl:ControlToolTip            = "Please choose a valid Claim Type":U
                                                                                                                                         
    oControl                           = opoContainer:addControl("cbClaimType":U             + ipcContainerName, "wsInput":U       , "3":U , "tt_auth.claim_type":U               , "CHARACTER":U, 13 , 1, "Claim Type:":U)
    oControl:ControlToken              = "Hidden":U  
    oControl:ControlValue              = "C":U /* initialize in case user does not change claim type */                                                                                                                                      
    oControl:CellLayoutMask            = "&1&2":U                                                                                                                                         
                                                                                                                                                                                          
    oControl                           = opoContainer:addControl("fiMouthPartId":U           + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.mouth_part_id[1]":U         , "INTEGER":U  , 14 , 1, "Mouth Part ID:":U)                                                                                                                               
    oControl:ControlToolTip            = "Please enter a valid Mouth Part ID"
    oControl:ControlClass              = "+clNumericOnly":U
                                                                                                                                                                                        
    oControl                           = opoContainer:addControl("cbSystemStatus":U          + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.auth_status":U              , "CHARACTER":U, 14 , 2, "System Authorisation Status:":U)
    oControl:ControlJavascript         = "style='width:128px' disabled ":U                                                                                                                                   
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                  
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                              
    oControl:RenderArgument            = "AuthSystemStatusCombo":U 
    oControl:ControlToolTip            = "System Authorisation Status.":U    
    
    oControl                           = opoContainer:addControl("cbBodyRegion":U            + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.body_region":U              , "CHARACTER":U, 15 , 1, "Body Region:":U)     
    oControl:AdditionalItems           = "=":U
    oControl:QueryString               = SUBSTITUTE("FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = '&1' BY mic_acronym.acronym_sequence":U, "ma_acBodyRegion":U)
    oControl:KeyField                  = "mic_acronym.acronym_key":U
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:ControlJavascript         = "style='width:128px'":U                                                                                                                                             
    oControl:ControlToolTip            = "Please choose a valid Body Region":U  
                                                                                                                                                                                                           
    oControl                           = opoContainer:addControl("flAuthoriseAllServices":U  + ipcContainerName, "wsCheckBox":U    , "15":U, "tt_auth.authorise_all_services":U   , "LOGICAL":U  , 15 , 2, "Authorise All Services:":U)
    oControl:ControlToken              = "ReadOnly":U                                                                                                                                                        
    oControl:ControlToolTip            = "Authorise All Service":U                                                                                                                                          
    
    oControl                           = opoContainer:addControl("cbServiceType":U           + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth.service_type":U             , "CHARACTER":U, 16 , 1, "Service Type:":U)     
    oControl:AdditionalItems           = "=":U
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acServiceType' BY mic_acronym.acronym_label":U
    oControl:KeyField                  = "mic_acronym.acronym_key":U
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:ControlJavascript         = "style='width:128px'":U              
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                        
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                    
    oControl:RenderArgument            = "ServiceType":U
    oControl:ControlToolTip            = "Please choose a valid Service Type.":U  
    
    oControl                           = opoContainer:addControl("fcNextOfKin":U             + ipcContainerName, "wsInput":U       , "40":U, "tt_auth.next_of_kin":U              , "CHARACTER":U, 17 , 1, "Next Of Kin:":U)     
    oControl:ControlToolTip            = "Please specify a Next Of Kin"
    
    oControl                           = opoContainer:addControl("flAuthIncomplete":U        + ipcContainerName, "wsCheckBox":U    , "15":U, "tt_auth.auth_incomplete":U          , "CHARACTER":U, 18 , 1, "":U)
    oControl:ControlToken              = "HIDDEN":U
    oControl:ColumnSpan                = 4
    oControl                           = opoContainer:addControl("fcAuthIncomplete":U        + ipcContainerName, "wsInput":U       , "15":U, "":U          , "CHARACTER":U, 18 , 1, "":U)
    oControl:ColumnSpan                = 4
    oControl:CellLayoutMask            = "&1<div style=~"text-align:center~"><font color='red'><b>&2</b></font></div>":U  
    oControl:SpanOverLabel             = TRUE
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                        
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                                    
    oControl:RenderArgument            = "AuthIncomplete":U 

    oControl                           = opoContainer:addControl("FinancialHolderContainer":U + ipcContainerName, "":U              , "":U  , "":U                                , "":U         , 19 , 1 , "":U)                                                                                                                               
                                                                                                                                                                                  
    oControl:SubContainer              = mipEnv:Health:maUiService:getCntUpdAuthHeadFinancial(INPUT "FinancialHolderContainer":U)                                                 
    oControl:SubContainerType          = "FORM":U                                                                                                                                 
    oControl:SpanOverLabel             = TRUE                                                                                                                                     
    oControl:ColumnSpan                = 2                                                                                                                                        
    oControl:CellSnippet               = "align='center'":U                                                                                                                       
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("_authObj":U                 + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.auth_obj":U                , "CHARACTER":U, 20, 1 , "_authobj:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:JavascriptonChange        = "fnOnChangeAuthObj();":U                                                                                                                 
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("_memberNumber":U            + ipcContainerName, "wsInput":U       , "15":U, "tt_auth.mem_num":U                 , "CHARACTER":U, 21, 1 , "_membernumber:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("_errorOrWarning":U          + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 22, 1 , "_authtypewarning:":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:JavascriptOnChange        = "fnOnChangeErrorOrWarning(this);":U         
    oControl                           = opoContainer:addControl("_helpMessage":U             + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 22, 1 , ":":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:CellLayoutMask            = "&1&2":U
     
    oControl                           = opoContainer:addControl("_AuthStatusAction":U        + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 23, 1 , ":":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:JavascriptOnChange        = "fnAuthStatusAction(this);":U  

    oControl                           = opoContainer:addControl("_triggerDuplicateCheck":U   + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 23, 2 , ":":U)
    oControl:ControlToken              = "Hidden":U                                                                                                                               
    oControl:JavascriptOnChange        = "fnOnChangeDuplicateField();":U  
    
    oControl                           = opoContainer:addControl("_authMode":U                + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 24, 1 , "_authmode:":U)
    oControl:ControlToken              = "Hidden":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument            = "AuthMode":U

    oControl                           = opoContainer:addControl("_configuration":U           + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 26, 1 , "_configuration:":U)
    oControl:ControlToken              = "Hidden":U
    oControl:JavascriptOnChange        = "fnOnChangeAuthConfiguration(this);":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthConfiguration":U.


  DO iControl = 1 TO NUM-ENTRIES(opoContainer:getControlNameList()):
  
    ASSIGN oControl             = opoContainer:getControl(ENTRY(iControl, opoContainer:getControlNameList()))
    
           oControl:CellSnippet = "style=~"padding-top:2px~"":U.
  
  END. /*DO iControl = 1 TO NUM-ENTRIES(opoContainer:getControlNameList()):*/


  ASSIGN 
     oContainerProperties                   = NEW cls.wscontainerproperties(opoContainer)
     oContainerProperties:AutoSaveOperation = "SERVICE:maUIService:ajaxSaveAuth":U
     oContainerProperties:IgnoreFieldList   = "_errorOrWarning":U + opoContainer:ContainerCode  + ",":U 
                                            + "_authMode":U       + opoContainer:ContainerCode  + ",":U
                                            + "_configuration":U  + opoContainer:ContainerCode.                                                     
  
  
  mipEnv:Health:maUiService:prepareCustomizedForm(INPUT opoContainer, INPUT oContainerProperties).   

  
&ENDIF 
    
  { mip/inc/mipcatcherror.i } 
    






