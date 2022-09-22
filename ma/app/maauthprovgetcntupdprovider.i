/* maauthprovgetcntupdprovider.i MEDSTAR Medical Aid System
                                 Authorisation Provider Container Procedure ( getCntUpdProvider )
                                 (c) Copyright 1990 - 2020
                                 MIP Holdings (Pty) Ltd
                                 All rights reserved
*/
  
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  DEFINE VARIABLE oContainer           AS cls.mipwscontainer        NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oWob                 AS cls.mipwswob              NO-UNDO.  
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE cControlNameList     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cQueryFieldList      AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.

       
  ASSIGN 
    oWob                                  = Warpspeed:CurrentObject

    opoContainer                          = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle           = "Provider Information":U
    opoContainer:ViewOnly                 = FALSE
    opoContainer:NoDataMessage            = "Please specify the provider information in the empty line provided above":U
    opoContainer:ShowContainerSettings    = FALSE
    opoContainer:Collapsable              = TRUE
    opoContainer:RowRenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle
    opoContainer:RowRenderProcedure       = "RowRenderProcedure":U
    opoContainer:RowRenderArgument        = "AuthProviderContainer":U
    opoContainer:ContainerMode            = Warpspeed:SubmitValue
    opoContainer:QueryString              = "FOR EACH tt_auth_provider NO-LOCK":U
                                          + ",  FIRST tt_auth NO-LOCK":U
                                          + "   WHERE tt_auth.auth_obj = tt_auth_provider.auth_obj":U
                                          + ",  FIRST attending_doctor NO-LOCK":U
                                          + "   WHERE attending_doctor.doc-num = tt_auth_provider.doc_num OUTER-JOIN ":U
                                          + ",  FIRST group_doctor NO-LOCK":U
                                          + "   WHERE group_doctor.doc-num = tt_auth_provider.group_doc_num OUTER-JOIN":U
                                          + ",  FIRST group_discipline NO-LOCK":U
                                          + "   WHERE group_discipline.doc-num = tt_auth_provider.group_doc_num OUTER-JOIN":U
                                          + "      BY tt_auth_provider.line_number BY tt_auth_provider.provider_sequence ":U
                                                                                                                                                                                     
    oControl                              = opoContainer:addControl("fiLineNumber":U               + ipcContainername, "wsInput":U       , "25":U , "tt_auth_provider.line_number":U                                   , "INTEGER":U  ,  1, "#":U)    
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                          
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                                      
    oControl:RenderArgument               = "LineNumber":U  
    oControl:ControlToken                 = "Hidden":U                                                                                                                                         
    oControl:JavascriptOnChange           = "fnOnChangeAuthProviderLineNumber(this);":U
    
    oControl                              = opoContainer:addControl("fdAuthProviderObj":U          + ipcContainerName, "wsInput":U       , "20":U , "tt_auth_provider.auth_provider_obj":U , "CHARACTER":U,  2, "":U)
    oControl:ControlClass                 = "+clObjControl +clHid":U
    
    /*Enable sequence field when a detail line successfully created                            */
    /*Enable edit button which launches detail form when a detail line is successfully created */
    oControl:JavascriptOnChange           = "fnOnChangeProviderObj(this);":U 
    
    oControl                              = opoContainer:addControl("buUpdate":U                   + ipcContainerName, "wsImage":U       , "20":U , "":U                                   , "CHARACTER":U,  3, "":U)
    oControl:ControlToolTip               = "Modify record"                                        
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                       
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                   
    oControl:RenderArgument               = "AuthProviderEditButton":U                                                                            
                                                                                                   
    oControl                              = opoContainer:addControl("cbSequence":U                 + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_provider.provider_sequence":U ,"CHARACTER":U,   4, "Seq":U)
    oControl:ControlClass                 = "+clSequence +clMaxLength:3":U                         
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                                
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                                            
    oControl:RenderArgument               = "ProviderSequence":U                                   
                                                                                                   
    oControl                              = opoContainer:addControl("fcRateChangeType":U             + ipcContainerName, "wsInput":U    , "12":U, "tt_auth_provider.rate_change_type":U   , "CHARACTER":U,  5, "Rate Change<br>Type":U)  
    oControl:ControlToken                 = "disabled":U                                                                                                                                
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                             
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument               = "RateChangeTypeDisplay":U   

    oControl                              = opoContainer:addControl("flAuthorised":U               + ipcContainerName, "wsCheckBox":U    , "20":U , "tt_auth_provider.authorised_service":U, "LOGICAL":U ,   6, "A/S":U)
    oControl:ControlTooltip               = "Select to Authorised Service for Provider Type":U  
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                                
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                                            
    oControl:RenderArgument               = "AuthProviderAuthorisedService":U
                                                                                                                                               
    oControl                              = opoContainer:addControl("cbProviderType":U             + ipcContainerName, "wsCombo":U       , "10":U , "tt_auth_provider.provider_type":U     , "CHARACTER":U,  7, "Provider Type":U)
    oControl:ControlClass                 = "+clMan":U                                                                                      
    oControl:RenderProcedure              = "RenderProcedure":U                                                                             
    oControl:RenderArgument               = "AcronymSelect:ma_acAuthProviderType:=":U 
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:ProviderType":U
    oControl:FilterFields                 = "[ProviderType],[StartDate],[AuthObj],[ProviderNum]":U
    oControl:FilterControls               = "cbProviderType":U + ipcContainerName + ",fdStartDate":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName + ",fiProviderNum":U + ipcContainerName
    oControl:ReturnFields                 = "[Configuration]":U
    oControl:ReturnControls               = "_configuration":U + ipcContainerName                                                      
    oControl:ControlTooltip               = "Select a Provider Type from the drop-down list":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthProviderType(this);":U                                                     

    oControl                              = opoContainer:addControl("fiProviderNum":U              + ipcContainerName, "wsInput":U       , "8":U  , "tt_auth_provider.doc_num":U           , "CHARACTER":U,  8, "":U)
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:8":U   
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthProviderNum":U  
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:Provider":U
    oControl:FilterFields                 = "[ProviderNum],[AttProviderNum],[AuthObj],[StartDate]":U
    oControl:FilterControls               = "fiProviderNum":U + ipcContainerName + ",fiAttProviderNum":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName + ",fdStartDate":U + ipcContainerName
    oControl:ReturnFields                 = "[ProviderDesc],[PrType],[SubPrType],[GroupProvider],[ProviderBaseRate],[ProviderArsRate],[DisciplineUpdatable]":U
    oControl:ReturnControls               = "fcProviderDesc":U + ipcContainerName + ",fiDiscipline":U + ipcContainerName + ",fiSubDiscipline":U + ipcContainerName + ",flGroupProvider":U + ipcContainerName + ",fcProviderBaseRate":U + ipcContainerName + ",fcProviderArsRate":U + ipcContainerName + ",fcDisciplineUpdatable":U + ipcContainerName
    oControl:ControlToolTip               = "Either the Provider and/or Discipline field must be entered.":U

    oControl                              = opoContainer:addControl("buProviderBtn":U              + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         ,  8, "":U)
    oControl:ControlTooltip               = "Lookup on Providers."
    oControl:LookupWobFLA                 = "madoc":U                                                                                                                             
    oControl:LookupFields                 = "doctor.doc-num":U                                                                                                                    
    oControl:LookupControls               = "fiProviderNum":U + ipcContainerName                                                                                                  
    oControl:FilterFields                 = "doctor.doc-num":U                                                                                                                    
    oControl:FilterControls               = "fiProviderNum":U + ipcContainerName                                                                                                  
    oControl:ReturnFields                 = "doctor.doc-num,doctor.name,docdisp.disp-code,docdisp.subdisp-code":U                                                                                                          
    oControl:ReturnControls               = "fiProviderNum":U + ipcContainerName + ",fcProviderDesc":U + ipcContainerName + ",fiDiscipline":U + ipcContainerName + ",fiSubDiscipline":U + ipcContainerName                                                        

    oControl                              = opoContainer:addControl("flGroupProvider":U            + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U,  8, "Provider":U)
    oControl:ControlToken                 = "hidden":U                                                                                                                               
    oControl:CellLayoutMask               = "&1&2&3":U                                                                                                                               
    oControl:JavascriptonChange           = "fnOnChangeAuthProviderGroup(this, ~"" + ipcContainerName + "~");":U                                                           
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument               = "GroupProvider":U                                                                                                                        

    oControl                              = opoContainer:addControl("fcProviderDesc":U             + ipcContainerName, "wsTextArea":U    , "7,2":U, "":U                                   , "CHARACTER":U,  9, "Provider<br>Desc":U)  
    oControl:ControlToken                 = "disabled":U                                                                                                                                
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                             
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument               = "AuthProviderDesc":U         

    oControl                              = opoContainer:addControl("fdAuthGroupObj":U             + ipcContainerName, "wsCombo":U       , "10":U , "tt_auth_provider.auth_group_obj":U    , "CHARACTER":U,  10, "Auth Group":U)
    oControl:ControlToolTip               = "Groups the Providers and Clinical Details to a specific Authorisation group. Only one of these Providers will be allowed to be claimed from.":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument               = "AuthGroupCode":U 
                                                                                                                                                                                                      
    oControl                              = opoContainer:addControl("fiDiscipline":U               + ipcContainerName, "wsInput":U       , "3":U  , "":U                                   , "CHARACTER":U,  11, "Disc":U)
    oControl:ControlClass                 = "+clMan +clNumericOnly +clMaxLength:3":U                                                                                                 
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument               = "AuthProviderDiscipline":U                                                                                                               
    oControl:ControlToolTip               = "Provider Discipline":U                                                                                                                  
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:ProviderType":U
    oControl:FilterFields                 = "[ProviderType],[PrType],[SubPrType],[StartDate],[AuthObj],[ProviderNum]":U
    oControl:FilterControls               = "cbProviderType":U + ipcContainerName + ",fiDiscipline":U + ipcContainerName + ",fiSubDiscipline":U + ipcContainerName + ",fdStartDate":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName + ",fiProviderNum":U + ipcContainerName
    oControl:ReturnFields                 = "[Configuration],[ProviderBaseRate],[ProviderArsRate]":U
    oControl:ReturnControls               = "_configuration":U + ipcContainerName + ",fcProviderBaseRate":U + ipcContainerName + ",fcProviderArsRate":U + ipcContainerName
    
    /* Clear provider number and provider group when disciplineis changed by the user */     
    oControl:JavascriptOnKeyup            = "fnOnKeyupAuthProviderDiscipline(this, ~"fiProviderNum":U + ipcContainerName + "~", ~"":U + ipcContainerName + "~");":U
    
    /* Correct padding before save fires */
    oControl:JavascriptOnChange           = "fnOnChangeAuthProviderDiscipline(this);":U

    oControl                              = opoContainer:addControl("buDiscipline":U               + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         ,  11, "Disc":U)
    oControl:CellLayoutMask               = "&1&2":U
    oControl:LookupWobFLA                 = "maprtype":U
    oControl:LookupFields                 = "prtype.pr-type":U
    oControl:LookupControls               = "fiDiscipline":U + ipcContainerName
    oControl:FilterFields                 = "prtype.pr-type":U
    oControl:FilterControls               = "fiDiscipline":U + ipcContainerName
    oControl:ReturnFields                 = "prtype.pr-type":U
    oControl:ReturnControls               = "fiDiscipline":U + ipcContainerName
    oControl:ControlTooltip               = "Lookup on Provider Disciplines.":U

    oControl                              = opoContainer:addControl("fiSubDiscipline":U            + ipcContainerName, "wsInput":U       , "3":U  , "":U                                   , "CHARACTER":U, 12, "Sub-Dis":U)                                  
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U              
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                 
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthProviderSubDiscipline":U                                                                    
    oControl:ControlToolTip               = "Provider Sub-Discipline Code":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:ProviderType":U
    oControl:FilterFields                 = "[ProviderType],[PrType],[SubPrType],[StartDate],[AuthObj],[ProviderNum]":U
    oControl:FilterControls               = "cbProviderType":U + ipcContainerName + ",fiDiscipline":U + ipcContainerName + ",fiSubDiscipline":U + ipcContainerName + ",fdStartDate":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName + ",fiProviderNum":U + ipcContainerName
    oControl:ReturnFields                 = "[Configuration]":U
    oControl:ReturnControls               = "_configuration":U + ipcContainerName       
  
    /* Correct padding before save fires. */
    oControl:JavascriptOnChange           = "fnOnChangeAuthProviderDiscipline(this);":U
                                       
    oControl                              = opoContainer:addControl("buSubDiscipline":U            + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 12, "Sub-Disc":U)
    oControl:CellLayoutMask               = "&1&2":U
    oControl:LookupWobFLA                 = "masubdisc":U
    oControl:LookupFields                 = "subdisc.subdisp-code":U
    oControl:LookupControls               = "fiSubDiscipline":U + ipcContainerName
    oControl:FilterFields                 = "subdisc.subdisp-code,subdisc.pr-type":U
    oControl:FilterControls               = "fiSubDiscipline":U + ipcContainerName + ",fiDiscipline":U + ipcContainerName                                                            
    oControl:ReturnFields                 = "subdisc.subdisp-code":U
    oControl:ReturnControls               = "fiSubDiscipline":U + ipcContainerName
    oControl:ControlTooltip               = "Lookup on Provider Sub-disciplines."                                                                                                    

    oControl                              = opoContainer:addControl("fcProviderBaseRate":U         + ipcContainerName, "wsInput":U       , "18":U , "tt_auth_provider.default_base_rate":U , "CHARACTER":U, 13, "Default<br>Base<br>Rate":U)
    oControl:ControlToken                 = "Readonly":U                                                                                                                                   
    oControl:ControlToolTip               = "Default Base Rate":U  
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument               = "BaseRateProvider":U     

    oControl                              = opoContainer:addControl("fcProviderArsRate":U          + ipcContainerName, "wsInput":U       , "18":U , "tt_auth_provider.default_ars_rate":U   , "CHARACTER":U, 14, "Default<br>Ars<br>Rate":U)
    oControl:ControlToken                 = "Readonly":U                                                                                                                                   
    oControl:ControlToolTip               = "Default Ars Rate":U                                                                                                                             
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                          
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                      
    oControl:RenderArgument               = "ArsRateProvider":U  
    
    oControl                              = opoContainer:addControl("cbOverrideBaseRate":U         + ipcContainerName, "wsInput":U       , "5":U  , "tt_auth_provider.override_base_rate":U, "CHARACTER":U, 15, "Override<br>Base<br>Rate":U)
    oControl:ControlToolTip               = "Override Base Rate":U   
    oControl:ControlToken                 = "ReadOnly":U                            
    oControl:ColumnLabelClass             = oControl:ColumnLabelClass + " +clHideOverrides":U
    oControl:ControlClass                 = "+clHideOverrides":U

    oControl                              = opoContainer:addControl("cbOverrideArsRate":U          + ipcContainerName, "wsInput":U       , "5":U  , "tt_auth_provider.override_ars_rate":U , "CHARACTER":U, 16, "Override<br>Ars<br>Rate":U)
    oControl:ControlToolTip               = "Override Ars Rate":U      
    oControl:ControlToken                 = "ReadOnly":U                            
    oControl:ColumnLabelClass             = oControl:ColumnLabelClass + " +clHideOverrides":U
    oControl:ControlClass                 = "+clHideOverrides":U
                                                                                                   
    oControl                              = opoContainer:addControl("flMainProvider":U             + ipcContainerName, "wsCheckBox":U    , "18":U , "tt_auth_provider.main_provider":U     , "LOGICAL":U  , 17, "M":U)
    oControl:ControlTooltip               = "Main Provider":U  
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle    
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "MainProvider":U

    oControl                              = opoContainer:addControl("cbEmergencyFlagValue":U       + ipcContainerName, "wsCombo":U       , "10":U , "tt_auth_provider._emergency_flag":U    , "INTEGER":U  , 18, "Emergency<br>Flag":U)
    oControl:ControlTooltip               = "Select a emergency flag value":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "EmergencyFlagValue":U
    oControl:JavascriptOnChange           = "fnOnChangeEmergencyFlagValue(this) ;"

    oControl                              = opoContainer:addControl("flEmergencyFlagValueUpdated":U + ipcContainerName, "wsInput":U     , "10":U , "tt_auth_provider._emergency_flag_updated":U , "INTEGER":U  , 18, "Emergency<br>Flag":U)
    oControl:ControlToken                 = "Hidden":U 
    oControl:CellLayoutMask               = "&1&2":U                                 
                                 
    oControl                              = opoContainer:addControl("flAuthoriseAllServices":U     + ipcContainerName, "wsCheckBox":U    , "18":U , "tt_auth_provider.authorise_all_services":U     , "LOGICAL":U  , 19, "Authorise<br>All<br>Services":U)
    oControl:ControlTooltip               = "Indicates that only authorised services on authorisation clinical details will be paid.":U .

  ASSIGN
    oControl                              = opoContainer:addControl("fiAttProviderNum":U           + ipcContainerName, "wsInput":U       , "10":U , "tt_auth_provider.group_doc_num":U      , "CHARACTER":U, 20, "":U)
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthAttProviderNum":U                                                                                                                   
    oControl:ControlClass                 = "+clNumericOnly":U        
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:AttendingProvider":U
    oControl:FilterFields                 = "[ProviderNum],[AttProviderNum],[StartDate],[AuthObj]":U
    oControl:FilterControls               = "fiProviderNum":U + ipcContainerName + ",fiAttProviderNum":U + ipcContainerName + ",fdStartDate":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName
    oControl:ReturnFields                 = "[AttProviderNum],[ProviderDesc],[PrType],[SubPrType],[ProviderBaseRate],[ProviderArsRate],[DisciplineUpdatable]":U
    oControl:ReturnControls               = "fiAttProviderNum":U + ipcContainerName + ",fcAttProviderDesc":U + ipcContainerName + ",fiAttDiscipline":U + ipcContainerName + ",fiAttSubDiscipline":U + ipcContainerName + ",fcProviderBaseRate":U + ipcContainerName + ",fcProviderArsRate":U + ipcContainerName + ",fcAttDisciplineUpdatable":U + ipcContainerName
    oControl:ControlToolTip               = "Either the Attending Provider and/or Attending Discipline field must be entered.":U
                                                                                                                                            
    oControl                              = opoContainer:addControl("buAttProviderBtn":U           + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 20, "Att<br>Provider":U)
    oControl:ControlTooltip               = "Lookup on Attending Providers.":U
    oControl:CellLayoutMask               = "&1&2":U                                                                                                                          
    oControl:LookupWobFLA                 = "prassoc":U                                                                                                                            
    oControl:LookupFields                 = "doctor.doc-num":U                                                                                                                   
    oControl:LookupControls               = "fiAttProviderNum":U + ipcContainerName                                                                                              
    oControl:FilterFields                 = "doctor.doc-num,prassoc.pr-assoc":U                                                                                                                   
    oControl:FilterControls               = "fiAttProviderNum":U + ipcContainerName + ",fiProviderNum":U + ipcContainerName
    oControl:ReturnFields                 = "doctor.doc-num,doctor.name":U                                                                                                         
    oControl:ReturnControls               = "fiAttProviderNum":U + ipcContainerName + ",fcAttProviderDesc":U + ipcContainerName                                                  

    oControl                              = opoContainer:addControl("fcAttProviderDesc":U          + ipcContainerName, "wsTextArea":U    , "7,2":U, "":U                                   , "CHARACTER":U, 21, "Att<br>Desc":U)
    oControl:ControlToken                 = "disabled":U                                                                                                                                
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle        
    oControl:RenderProcedure              = "RenderProcedure":U                                    
    oControl:RenderArgument               = "AuthAttProviderDesc":U                                                                          

    oControl                              = opoContainer:addControl("fiAttDiscipline":U            + ipcContainerName, "wsInput":U       , "3":U  , "":U                                   , "CHARACTER":U, 22, "Att<br>Disp":U)
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U                                                               
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthAttProviderDiscipline":U                                                                    
    oControl:ControlToolTip               = "Attending Discipline":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:Discipline":U
    oControl:FilterFields                 = "[PrType]":U
    oControl:FilterControls               = "fiAttDiscipline":U + ipcContainerName
    oControl:ReturnFields                 = "[PrType]":U
    oControl:ReturnControls               = "fiAttDiscipline":U + ipcContainerName

    /* Clear provider number when user manually enters a discipline */
    oControl:JavascriptOnKeyup            = "fnOnKeyupAuthProviderDiscipline(this, ~"fiAttProviderNum":U + ipcContainerName + "~", ~"":U + ipcContainerName + "~");":U

    /* Correct padding before save fires  */                                                                                          
    oControl:JavascriptOnChange           = "fnOnChangeAuthProviderDiscipline(this);":U

    oControl                              = opoContainer:addControl("buAttDiscipline":U            + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 22, "Att<br>Disp":U)
    oControl:ControlTooltip               = "Lookup on Attending Disciplines."                                                                                                             
    oControl:CellLayoutMask               = "&1&2":U
    oControl:LookupWobFLA                 = "maprtype":U
    oControl:LookupFields                 = "prtype.pr-type":U
    oControl:LookupControls               = "fiAttDiscipline":U + ipcContainerName
    oControl:FilterFields                 = "prtype.pr-type":U
    oControl:FilterControls               = "fiAttDiscipline":U + ipcContainerName
    oControl:ReturnFields                 = "prtype.pr-type":U
    oControl:ReturnControls               = "fiAttDiscipline":U + ipcContainerName

    oControl                              = opoContainer:addControl("fiAttSubDiscipline":U         + ipcContainerName, "wsInput":U       , "3":U  , "":U                                   , "CHARACTER":U, 23, "Att<br>Sub-Disp":U)
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthAttProviderSubDiscipline":U
    oControl:ControlToolTip               = "Attending Sub-Discipline":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:SubDiscipline":U                                                                                   
    oControl:FilterFields                 = "[PrType],[SubPrType]":U                                                                                                                       
    oControl:FilterControls               = "fiAttDiscipline" + ipcContainerName + ",fiAttSubDiscipline":U + ipcContainerName                                                              
    oControl:ReturnFields                 = "[SubPrType]":U                                                                                                                                
    oControl:ReturnControls               = "fiAttSubDiscipline":U + ipcContainerName                                                                                                      

    /* Correct padding before save fires  */                                                                                                                                                                                     
    oControl:JavascriptOnChange           = "fnOnChangeAuthProviderDiscipline(this);":U.

  ASSIGN 
    oControl                              = opoContainer:addControl("buAttSubDiscipline":U         + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 23, "Att<br>Sub-Disp":U)
    oControl:ControlTooltip               = "Lookup on attending sub-disciplines."                                                                                                         
    oControl:CellLayoutMask               = "&1&2":U
    oControl:LookupWobFLA                 = "masubdisc":U
    oControl:LookupFields                 = "subdisc.subdisp-code":U
    oControl:LookupControls               = "fiAttSubDiscipline":U + ipcContainerName
    oControl:FilterFields                 = "subdisc.subdisp-code,subdisc.pr-type":U
    oControl:FilterControls               = "fiAttSubDiscipline":U + ipcContainerName + ",fiAttDiscipline":U + ipcContainerName                                                            
    oControl:ReturnFields                 = "subdisc.subdisp-code":U                                                                                                                       
    oControl:ReturnControls               = "fiAttSubDiscipline":U + ipcContainerName                                                                                                      

    oControl                              = opoContainer:addControl("fdStartDate":U                + ipcContainerName, "wsInput":U       , "10":U , "tt_auth_provider.start_date":U        , "DATE":U     , 24, "Start Date":U)
    oControl:ControlClass                 = "+clMan +clPreserveValue":U                                                                                                                    
    oControl:ControlTooltip               = "Authorised Start Date for the provider.":U                                                                                                    
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:ProviderType":U                                                                                    
    oControl:FilterFields                 = "[ProviderType],[PrType],[SubPrType],[StartDate],[AuthObj],[ProviderNum]":U                                                                                  
    oControl:FilterControls               = "cbProviderType":U + ipcContainerName + ",fiDiscipline":U + ipcContainerName + ",fiSubDiscipline":U + ipcContainerName                         
                                          + ",fdStartDate":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName + ",fiProviderNum":U + ipcContainerName                                                               
    oControl:ReturnFields                 = "[Configuration],[ProviderBaseRate],[ProviderArsRate]":U                                                                                                                            
    oControl:ReturnControls               = "_configuration":U + ipcContainerName + ",fcProviderBaseRate":U + ipcContainerName + ",fcProviderArsRate":U + ipcContainerName                                                                                                          

    oControl                              = opoContainer:addControl("cbStartAmPm":U                + ipcContainerName, "wsCombo":U       , "3":U  , "tt_auth_provider.start_ampm":U        , "LOGICAL":U  , 24, "Start Date":U)
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
    oControl:CellLayoutMask               = "&1 &2":U                                                                                                                                      
    oControl:ControlClass                 = "+clSelVertAlign":U                                                                                                                            
    oControl:ControlTooltip               = "Select a Start Time for the provider.":U                                                                                                                            
                                                                                                                                                                                           
    oControl                              = opoContainer:addControl("fdEndDate":U                  + ipcContainerName, "wsInput":U       , "10":U , "tt_auth_provider.end_date":U          , "DATE":U     , 25, "End Date":U)
    oControl:ControlTooltip               = "Authorised End Date for the provider.":U                                                                                                      

    oControl                              = opoContainer:addControl("cbEndAmPm":U                  + ipcContainerName, "wsCombo":U       , "3":U  , "tt_auth_provider.end_ampm":U          , "LOGICAL":U  , 25, "End Date":U)
    oControl:ControlTooltip               = "Select an end time for the provider.":U                                                                                                       
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                                                                                               
    oControl:CellLayoutMask               = "&1 &2":U                                                                                                                                      
    oControl:ControlClass                 = "+clSelVertAlign":U                                                                                                                            

    oControl                              = opoContainer:addControl("fiClaimCode":U                + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_provider.claim_code":U        , "INTEGER":U  , 26, "Claim<br>Code":U)
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U                                                                     
    oControl:ControlTooltip               = "Claim Code that the provider is authorised for":U                                                    
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationAuth:ClaimCode":U                                                                                     
    oControl:FilterFields                 = "[AuthObj],[StartDate],[ClaimCode]":U                                           
    oControl:FilterControls               = "_authObjArgument":U + ipcContainerName                                                          
                                          + ",fdStartDate":U     + ipcContainerName  
                                          + ",fiClaimCode":U     + ipcContainerName
    oControl:ReturnFields                 = "[StatusAction]":U                                                                                                                  
    oControl:ReturnControls               = "_AuthStatusAction":U + ipcContainerName                                                                                                

    oControl                              = opoContainer:addControl("buClaimBtn":U                 + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 26, "Claim<br>Code":U)
    oControl:ControlTooltip               = "Lookup on Claim Codes.":U
    oControl:CellLayoutMask               = "&1&2":U                                                                                                                                                      
    oControl:LookupWobFLA                 = "maclc":U                                                                                                                                                     
    oControl:LookupFields                 = "ccdesc.claim-code":U                                                                                                                                        
    oControl:LookupControls               = "fiClaimCode":U + ipcContainerName                                                                                                                           
    oControl:FilterFields                 = "ccdesc.claim-code":U                                                                                                                                        
    oControl:FilterControls               = "fiClaimCode":U + ipcContainerName               
    oControl:ReturnFields                 = "ccdesc.claim-code":U                                                                                                                                        
    oControl:ReturnControls               = "fiClaimCode":U + ipcContainerName
                                                                                                                                                                                   
    oControl                              = opoContainer:addControl("cbClaimType":U                + ipcContainerName, "wsCombo":U       , "3":U  , "tt_auth_provider.claim_type":U        , "CHARACTER":U, 27, "Claim<br>Type":U)
    oControl:AdditionalItems              = "|C=C|N=N|A=A|K=K|P=P|O=O":U                                                                                                                                             
    oControl:ControlToolTip               = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther":U                                                                             
                                                                                                                                                                                           
    oControl                              = opoContainer:addControl("cbStatus":U                   + ipcContainerName, "wsCombo":U       , "10":U , "tt_auth_provider.auth_status":U       , "INTEGER":U  , 28, "":U)
    oControl:ControlTooltip               = "Select a status from the drop-down list.":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                            
    oControl:RenderArgument               = "AuthStatusCombo":U                                                                                                                            
    oControl:ControlClass                 = "+clMan +clPreserveValue":U                                                                                                                    
    oControl:JavaScriptOnChange           = "fnOnChangeAuthProviderStatus(this, ~"tbl~", ~"" + ipcContainerName + "~" );":U                                                            
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:Status":U                                                                                          
    oControl:FilterFields                 = "[Status],[AuthObj]":U                                                                                                                         
    oControl:FilterControls               = "cbStatus":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName                                                                     
    oControl:ReturnFields                 = "[StatusReasonMandatory]":U                                                                                                                    
    oControl:ReturnControls               = "flStatusReasonMandatory":U + ipcContainerName                                                                                                 
    oControl:ControlClass                 = "+clMan":U
                                                                                                                                                                                       
    oControl                              = opoContainer:addControl("flStatusReasonMandatory":U    + ipcContainerName, "wsInput":U       , "15":U , "":U                                   , "CHARACTER":U, 28 ,"Status":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                     
    oControl:CellLayoutMask               = "&1&2":U                                                                                                                                       
                                                                                                                                                                                           
    /* On status change, check whether reason is mandatory or not */ 
    oControl:JavaScriptOnChange           = "fnOnChangeAuthProviderStatus(this);":U                                                                                                        
                                          + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"TBL~");":U 
                                                                                                                                                                                           
    /* See RowRenderProcedure - Check whether each reason control is mandatory or not depending on status */
    /* The rendering was moved from a render procedure to a row render procedure for performance reasons  */
    oControl                              = opoContainer:addControl("fcReason":U                   + ipcContainerName, "wsInput":U       , "5":U  , "tt_auth_provider.auth_status_note":U  , "CHARACTER":U, 29, "Status<br>Reason":U)
    oControl:ControlTooltip               = "Status Reason":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:Reason":U                                                                                          
    oControl:FilterFields                 = "[ReasonCode],[ReasonType],[ProviderObj],[StartDate],[Status]":U                                                                                                         
    oControl:FilterControls               = "fcReason":U + ipcContainerName + ",":U 
                                          + "fcReasonTypeArgument":U + ipcContainerName + ",":U
                                          + "fdAuthProviderObj":U + ipcContainerName + ",":U   
                                          + "fdStartDate":U + ipcContainerName + ",":U
                                          + "cbStatus":U + ipcContainerName
    oControl:ReturnFields                 = "[ReasonDesc],[ReasonType]":U                                                                                                                  
    oControl:ReturnControls               = "fcReasonDesc":U + ipcContainerName + ",":U + "fcReasonTypeArgument":U + ipcContainerName
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                            
    oControl:RenderArgument               = "AuthStatusReason":U      

    oControl                              = opoContainer:addControl("fcReasonTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 29, "":U)                  
    oControl:ControlToken                 = "Hidden":U
    oControl:ControlValue                 = "AS":U
    oControl:ControlClass                 = "+clPreserveValue":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                            
    oControl:RenderArgument               = "AuthReasonType":U                                                                                                                       
                                                                                                                                                                                           
    oControl                              = opoContainer:addControl("fcReasonDesc":U               + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 29, "Status<br>Reason":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                     
                                                                                                                                                                                           
    /* Set reason description as tooltip when reason is selected client side */ 
    oControl:JavascriptOnChange           = "fnSetReasonDescription(this, ~"fcReason":U + ipcContainerName + "~", ~"buReasonBtn":U + ipcContainerName + "~", ~"TBL~");":U              
                                                                                                                                                                                           
    oControl                              = opoContainer:addControl("buReasonBtn":U                + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 29, "Status<br>Reason":U)                                                                                                                                                                                                   
    oControl:ControlTooltip               = "Lookup on Status Reasons.":U                                                                                                                  
    oControl:CellLayoutMask               = "&1&2&3&4":U                                                                                                                                                  
    oControl:LookupWobFLA                 = "note":U                                                                                                                                                    
    oControl:LookupFields                 = "note.key":U                                                                                                                                                
    oControl:LookupControls               = "fcReason":U + ipcContainerName                                                                                                                             
    oControl:FilterFields                 = "note.key,note.type":U                                                                                                                                      
    oControl:FilterControls               = "fcReason":U + ipcContainerName + ",":U
                                          + "fcReasonTypeArgument":U + ipcContainerName                                                                                      
    oControl:ReturnFields                 = "note.key":U                                                                                                                                           
    oControl:ReturnControls               = "fcReason":U + ipcContainerName                        
 
    oControl                              = opoContainer:addControl("fcCopayOverrideNote":U        + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_provider.copay_override_note":U , "INTEGER":U  , 30, "":U)
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "CopayOverrideNote":U        
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:OverrideNote":U                                                                                          
    oControl:FilterFields                 = "[ReasonCode],[ReasonType],[ProviderObj]":U                                                                                                         
    oControl:FilterControls               = "fcCopayOverrideNote":U + ipcContainerName + ",":U 
                                          + "fcOverrideNoteTypeArgument":U + ipcContainerName + ",":U
                                          + "fdAuthProviderObj":U + ipcContainerName 
    oControl:ReturnFields                 = "[ReasonDesc]":U                                                                                                                  
    oControl:ReturnControls               = "fcOverrideNoteDesc":U + ipcContainerName 
    
    oControl                              = opoContainer:addControl("fcOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 30, "":U)                  
    oControl:ControlToken                 = "Hidden":U
    oControl:ControlValue                 = "AO":U
    oControl:ControlClass                 = "+clPreserveValue":U
    
    oControl                              = opoContainer:addControl("fcOverrideNoteDesc":U         + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 30, "":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                     
                                                                                                                                                                                           
    /* Set copay override note description as tooltip when note is selected client side */ 
    oControl:JavascriptOnChange           = "fnSetReasonDescription(this, ~"fcCopayOverrideNote":U + ipcContainerName + "~", ~"buOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U  
    
    oControl                              = opoContainer:addControl("buOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 30, "Co-payment<br>Override<br> Reason":U)
    oControl:ControlTooltip               = "Lookup on Copay Override Reasons.":U
    oControl:CellLayoutMask               = "&1&2&3&4":U                                                                                                                                                      
    oControl:LookupWobFLA                 = "note":U                                                                                                                                                    
    oControl:LookupFields                 = "note.key":U                                                                                                                                                
    oControl:LookupControls               = "fcCopayOverrideNote":U + ipcContainerName                                                                                                                             
    oControl:FilterFields                 = "note.key,note.type":U                                                                                                                                      
    oControl:FilterControls               = "fcCopayOverrideNote":U + ipcContainerName + ",":U
                                          + "fcOverrideNoteTypeArgument":U + ipcContainerName                                                                                      
    oControl:ReturnFields                 = "note.key":U                                                                                                                                           
    oControl:ReturnControls               = "fcCopayOverrideNote":U + ipcContainerName   

    oControl                              = opoContainer:addControl("cbPenaltyValue":U       + ipcContainerName, "wsCombo":U    , "10":U , "tt_auth_provider._penalty_flag":U    , "INTEGER":U  , 31, "Penalty<br>Flag":U)
    oControl:ControlTooltip               = "Please select a valid value for the Penalty Flag":U
    oControl:JavascriptOnChange           = "fnOnChangePenaltyFlagValue(this);"

    oControl                              = opoContainer:addControl("flPenaltyValueUpdated":U + ipcContainerName, "wsInput":U   , "10":U , "tt_auth_provider._penalty_flag_updated":U , "INTEGER":U  , 31, "Penalty<br>Flag":U)
    oControl:ControlToken                 = "Hidden":U 
    oControl:CellLayoutMask               = "&1&2":U 
    
    oControl                              = opoContainer:addControl("fcPenaltyOverrideNote":U        + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_provider._penalty_override_note":U , "INTEGER":U  , 32, "":U)
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "EmergencyFlagValue":U   
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationProvider:OverrideNote":U                                                                                          
    oControl:FilterFields                 = "[ReasonCode],[ReasonType],[ProviderObj]":U                                                                                                         
    oControl:FilterControls               = "fcPenaltyOverrideNote":U + ipcContainerName + ",":U 
                                          + "fcPenaltyOverrideNoteTypeArgument":U + ipcContainerName + ",":U
                                          + "fdAuthProviderObj":U + ipcContainerName 
    oControl:ReturnFields                 = "[ReasonDesc]":U                                                                                                                  
    oControl:ReturnControls               = "fcPenaltyOverrideNoteDesc":U + ipcContainerName 

    oControl                              = opoContainer:addControl("fcPenaltyOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 32, "":U)                  
    oControl:ControlToken                 = "Hidden":U
    oControl:ControlValue                 = "AP":U
    oControl:ControlClass                 = "+clPreserveValue":U
    
    oControl                              = opoContainer:addControl("fcPenaltyOverrideNoteDesc":U         + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 32, "":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                     
                                                                                                                                                                                           
    /* Set copay override note description as tooltip when note is selected client side */ 
    oControl:JavascriptOnChange           = "fnSetReasonDescription(this, ~"fcPenaltyOverrideNote":U + ipcContainerName + "~", ~"buPenaltyOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U  
    oControl:JavascriptOnFocus            = "fnCheckForRollback(this);"
    
    oControl                              = opoContainer:addControl("buPenaltyOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 32, "Penalty<br>Override<br> Reason":U)
    oControl:ControlTooltip               = "Lookup on Penalty Override Reasons.":U
    oControl:CellLayoutMask               = "&1&2&3&4":U                                                                                                                                                      
    oControl:LookupWobFLA                 = "note":U                                                                                                                                                    
    oControl:LookupFields                 = "note.key":U                                                                                                                                                
    oControl:LookupControls               = "fcPenaltyOverrideNote":U + ipcContainerName                                                                                                                             
    oControl:FilterFields                 = "note.key,note.type":U                                                                                                                                      
    oControl:FilterControls               = "fcPenaltyOverrideNote":U + ipcContainerName + ",":U
                                          + "fcPenaltyOverrideNoteTypeArgument":U + ipcContainerName                                                                                      
    oControl:ReturnFields                 = "note.key":U                                                                                                                                           
    oControl:ReturnControls               = "fcPenaltyOverrideNote":U + ipcContainerName
    
    oControl                              = opoContainer:addControl("fcAccountReference":U         + ipcContainerName, "wsInput":U       , "10":U , "tt_auth_provider.account_reference":U , "CHARACTER":U, 33, "Account<br>Reference":U)
    oControl:ControlTooltip               = "Account Reference":U

    oControl                              = opoContainer:addControl("fdQuantityAuth":U             + ipcContainerName, "wsInput":U       , "5":U  , "tt_auth_provider.quantity_auth":U     , "DECIMAL":U  , 34, "Quantity<br>Auth":U)
    oControl:ControlTooltip               = "Authorised Quantity for the provider.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdAmountAuth":U               + ipcContainerName, "wsInput":U       , "12":U , "tt_auth_provider.amount_auth":U       , "DECIMAL":U  , 35, "Amount<br>Auth":U)
    oControl:ControlTooltip               = "Authorised Amount for the provider.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdAmountReq":U               + ipcContainerName, "wsInput":U       , "12":U , "tt_auth_provider.amount_requested":U       , "DECIMAL":U  , 36, "Amount<br>Request":U)
    oControl:ControlTooltip               = "The amount requested/quoted by the Provider.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdQtyReq":U                  + ipcContainerName, "wsInput":U       , "12":U , "tt_auth_provider.quantity_requested":U       , "DECIMAL":U  , 37, "Qty<br>Request":U)
    oControl:ControlTooltip               = "The amount authorised for the Provider.":U
    oControl:ControlClass                 = "+clNumericOnly":U
    
    oControl                              = opoContainer:addControl("flPmbIndicator":U             + ipcContainerName, "wsCheckBox":U    , "12":U , "tt_auth_provider.pmb_indicator":U     , "LOGICAL":U  , 40, "PMB<br>Indicator":U)
    oControl:ControlToken                 = "Hidden":U        
    
    oControl                              = opoContainer:addControl("flLosCalculation":U           + ipcContainerName, "wsCheckBox":U    , "12":U , "tt_auth_provider.los_calculation":U   , "LOGICAL":U  , 41, "System<br>LOS<br>Calculation":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                     
                                                                                                                                                                                           
    oControl                              = opoContainer:addControl("_authObjArgument":U           + ipcContainerName, "wsInput":U       , "8":U  , "tt_auth_provider.auth_obj":U          , "CHARACTER":U, 42, "":U)
    oControl:ControlToken                 = "Hidden":U                          
    oControl:ControlClass                 = "+clPreserveValue":U
    
    oControl                              = opoContainer:addControl("flDefault":U                  + ipcContainerName, "wsInput":U       , "10":U , "tt_auth_provider._default":U          , "CHARACTER":U, 43, "Default":U)
    oControl:ControlToken                 = "Hidden":U   

    oControl                              = opoContainer:addControl("_CopaymentProviderAction":U   + ipcContainerName, "wsInput":U       , "15":U, "":U                                    , "CHARACTER":U, 96 , ":":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                               
    oControl:JavascriptOnChange           = "fnCopaymentProviderAction(this);":U  
                                                        
    oControl                              = opoContainer:addControl("_AuthStatusAction":U          + ipcContainerName, "wsInput":U       , "15":U, "":U                                    , "CHARACTER":U, 97 , ":":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                               
    oControl:JavascriptOnChange           = "fnAuthStatusActionProvider(this);":U  
    
    oControl                              = opoContainer:addControl("fcDisciplineUpdatable":U      + ipcContainerName, "wsInput":U       , "12":U , "":U                                   , "CHARACTER":U, 98, "":U)
    oControl:ControlToken                 = "Hidden":U 
    /* This function will disable discipline fields when a valid provider is captured */
    oControl:JavascriptOnChange           = "fnOnChangeDisciplineUpdatable(this,~"~");":U

    oControl                              = opoContainer:addControl("fcAttDisciplineUpdatable":U    + ipcContainerName, "wsInput":U      , "12":U , "":U                                   , "CHARACTER":U, 99, "":U)
    oControl:ControlToken                 = "Hidden":U 
    /* This function will disable Atteinding discipline fields when a valid Attending provider is captured */
    oControl:JavascriptOnChange           = "fnOnChangeDisciplineUpdatable(this,~"Attending~");":U
    
    .
                  
  ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(opoContainer)
         
         /* AD - Find out if we should be disabling unique key fields on update for this container ??????????                              
         oContainerProperties:AlternateKeyFields     = "cbProviderType":U  + ipcContainerName + ",":U
                                                     + "fiProviderNum":U   + ipcContainerName + ",":U
                                                     + "fiDiscipline":U    + ipcContainerName + ",":U
                                                     + "fiSubDiscipline":U + ipcContainerName + ",":U  
                                                     + "fdStartDate":U     + ipcContainerName                
         */ 
     
         oContainerProperties:AutoSaveOperation      = "SERVICE:maUIService:ajaxSave":U + ipcContainerName + ":":U + ipcContainerName
         oContainerProperties:DefaultLess            = TRUE
         oContainerProperties:NumberVisibleControls  = 1
         oContainerProperties:CollapsableControlList = "fiAttProviderNum":U     + ipcContainerName + ",":U + "buAttProviderBtn":U       + ipcContainerName + ",":U   
                                                     + "fcAttProviderDesc":U    + ipcContainerName + ",":U + "fiAttDiscipline":U        + ipcContainerName + ",":U 
                                                     + "buAttDiscipline":U      + ipcContainerName + ",":U + "fiAttSubDiscipline":U     + ipcContainerName + ",":U 
                                                     + "buAttSubDiscipline":U   + ipcContainerName + ",":U + "cbOverrideBaseRate":U     + ipcContainerName + ",":U 
                                                     + "cbOverrideArsRate":U    + ipcContainerName + ",":U + "fcAccountReference":U     + ipcContainerName + ",":U
                                                     + "fdAuthGroupObj":U       + ipcContainerName + ",":U + "flAuthoriseAllServices":U + ipcContainerName + ",":U
                                                     + "fcCopayOverrideNote":U  + ipcContainerName + ",":U + "fcRateChangeType":U       + ipcContainerName + ",":U
                                                     + "fdAmountReq":U          + ipcContainerName + ",":U + "fdQtyReq":U               + ipcContainerName.                                                     
  
  mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties).   
  
  /*
    We dont want these fields to be sent in the auto save request built in prepareCustomizedContainer
  */
  ASSIGN 
    oControl                       = opoContainer:addControl("_authProviderList":U                 + ipcContainerName, "wsInput":U       , "8":U  , "":U                                   , "CHARACTER":U, 50, "":U)
    oControl:ControlClass          = "+clProvlist +clHid":U
    oControl:JavascriptOnChange    = "fnOnChangeProviderList(this);":U                                                                                                                     
                                                                                                                                                                                           
    oControl                       = opoContainer:addControl("_configuration":U                    + ipcContainerName, "wsInput":U       , "15":U , "":U                                   , "CHARACTER":U, 51, "_configuration:":U)
    oControl:ControlToken          = "Hidden":U    
    oControl:JavascriptOnChange    = "fnOnChangeAuthProviderConfiguration(this,~"":U + ipcContainerName + "~");":U
    oControl:RenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure       = "RenderProcedure":U
    oControl:RenderArgument        = "AuthProviderConfiguration":U.
    
  IF VALID-OBJECT(opoContainer:TitleContainer)
  THEN
  ASSIGN oControl                     = opoContainer:TitleContainer:addControl("fcSeqCombolist":U,  "wsInput":U, "20":U, "":U, "character":U, 1, 5, "":U)
         oControl:ControlClass        = "+clHid":U   
         oControl:JavascriptOnChange  = "fnSequenceListOnChange(this);":U.

&ENDIF
    
  { mip/inc/mipcatcherror.i } 
    





