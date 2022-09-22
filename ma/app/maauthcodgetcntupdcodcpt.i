/* maauthcodgetcntupdcodcpt.i  MEDSTAR Medical Aid System
                               Authorisation Coding Container Procedure ( getCntUpdCodingCPT )
                               (c) Copyright 1990 - 2021
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/
  
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oContainerProperties  AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oWob                  AS cls.mipwswob              NO-UNDO.
  DEFINE VARIABLE oControl              AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE lSuccess              AS LOGICAL                   NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  RUN _defineSharedControls IN TARGET-PROCEDURE(INPUT ipcContainerName, OUTPUT opoContainer).

  ASSIGN 
    oWob                                  = Warpspeed:CurrentObject
        
    opoContainer:RowRenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle
    opoContainer:RowRenderProcedure       = "RowRenderProcedure":U
    opoContainer:RowRenderArgument        = "AuthCPTCodingContainer":U
    opoContainer:ContainerTitle           = "Coding Details - Procedure(CPT)":U
    opoContainer:QueryString              = "FOR EACH tt_auth_coding NO-LOCK":U
                                          + "   WHERE tt_auth_coding.owning_entity_mnemonic = 'hlmck',":U            
                                          + "   FIRST tt_auth NO-LOCK":U                           
                                          + "   WHERE tt_auth.auth_obj = tt_auth_coding.auth_obj OUTER-JOIN":U
                                          + "      BY tt_auth_coding.line_number":U 
    
    oControl                           = opoContainer:addControl("cbCodingType":U          + ipcContainerName, "wsCombo":U        , "":U    , "tt_auth_coding.coding_type":U     , "CHARACTER":U,  4, "Coding Type":U)
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                   
    oControl:RenderArgument            = "AcronymSelect:ma_acCPTCodingType:=":U 
    oControl:ControlClass              = "+clMan":U                                                                                                                            
   
    oControl                           = opoContainer:addControl("fdOwningObj":U           + ipcContainerName, "wsInput":U       , "8":U   , "tt_auth_coding.owning_obj":U      , "DECIMAL":U  ,  5)
    oControl:ControlToken              = "Hidden":U
    oControl:JavascriptOnChange        = "fnSetAuthCodingLink(this,             ":U
                                       + "                 ~"_oemArgument~",   ":U
                                       + "                 ~"fdOwningObj~",     ":U
                                       + "                 ~"fdOwningAltObj~",  ":U
                                       + "                 ~"fcOwningAltValue~",":U
                                       + "                 ~"fcViewDetail~",    ":U
                                       + "                 ~"fcLinkTemplate~"); ":U

    oControl                           = opoContainer:addControl("fcOwningKey":U           + ipcContainerName, "wsInput":U       , "8":U   , "tt_auth_coding.owning_key":U      , "CHARACTER":U,  5)
    oControl:ControlToken              = "Hidden":U                                                                                                                 
                                                                                                                                                                    
    oControl                           = opoContainer:addControl("fdOwningAltObj":U        + ipcContainerName, "wsInput":U       , "8":U   , "":U                               , "DECIMAL":U,  5)
    oControl:ControlToken              = "Hidden":U

    oControl                           = opoContainer:addControl("fcOwningAltValue":U      + ipcContainerName, "wsInput":U       , "8":U   , "tt_auth_coding.owning_alt_value":U, "CHARACTER":U,  5)
    oControl:ControlToolTip            = "Please enter a valid CPT code":U
    oControl:ControlClass              = "+clMan":U
    oControl:AjaxValidation            = "SERVICE:maUIService:ajaxValidationCoding:CPT":U
    oControl:FilterFields              = "[CPTCode],[StartDate],[AuthObj]":U
    oControl:FilterControls            = "fcOwningAltValue":U + ipcContainerName + ", fdStartDate":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName
    oControl:ReturnFields              = "[CPTAltObj],[CPTObj],[CPTDescription],[ShowProcDate]":U
    oControl:ReturnControls            = "fdOwningAltObj":U + ipcContainerName + ",fdOwningObj" + ipcContainerName
                                       + ",fcOwningDesc":U + ipcContainerName + ",fcShowProcDate":U + ipcContainerName
    
    oControl                           = opoContainer:addControl("buOwningBtn":U           + ipcContainerName, "wsLookupButton":U, "":U    , "":U                               , "CHARACTER":U,  5, "CPT Code":U)
    oControl:CellLayoutMask            = "&1&2&3&4&5":U
    oControl:LookupWobFLA              = "mahlmck":U
    oControl:LookupFields              = "hlm_cpt_link.cpt_code":U
    oControl:LookupControls            = "fcOwningAltValue":U + ipcContainerName
    oControl:FilterFields              = "hlm_cpt_link.cpt_code":U
    oControl:FilterControls            = "fcOwningAltValue":U + ipcContainerName
    oControl:ReturnFields              = "hlm_cpt_link.cpt_code,hlm_cpt_link.cpt_description":U                                
    oControl:ReturnControls            = "fcOwningAltValue":U + ipcContainerName + ",fcOwningDesc":U + ipcContainerName 
    
    oControl                           = opoContainer:addControl("fcOwningDesc":U          + ipcContainerName, "wsTextArea":U    , "15,3":U, "tt_auth_coding._owning_description":U, "CHARACTER":U,  6)
    oControl:ControlToken              = "Disabled":U
    
    oControl                           = opoContainer:addControl("fcViewDetail":U          + ipcContainerName, "wsHref":U        , "":U    , "":U                               , "":U         ,  7, "":U)
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthCodingViewDetail":U
    
    /* This field is required to keep a "template" of the view more detail link which will be used to dynamically    */
    /* change the view detail link onclick and href attributes depending on the owning entity and item code selected.*/      
    oControl                           = opoContainer:addControl("fcLinkTemplate":U        + ipcContainerName, "wsHref":U        , "10":U, "":U                                 , "CHARACTER":U, 8, "")    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AuthCodingViewDetail":U

    oControl                           = opoContainer:addControl("flMainCode":U            + ipcContainerName, "wsCheckBox":U    , "":U    , "tt_auth_coding.main_code":U       , "LOGICAL":U  ,  9, "Main<br>Code":U)
    
    /*When a main code is selected , this function will run through all the other rows and    */ 
    /*if the current main code is checked then any other row with a checked main code will be */
    /*unchecked as there can be only one main code.                                           */
    oControl:JavascriptOnClick         = "fnOnClickAuthCodingMainCode(this, ~"":U + oControl:ControlName + "~");":U
   
    oControl                           = opoContainer:addControl("fdProcedureDate":U       + ipcContainerName, "wsInput":U       , "10":U  , "tt_auth_coding.procedure_date":U  , "DATE":U      , 23, "Procedure Date":U)
    oControl:ControlToken              = "updatable":U

    oControl                           = opoContainer:addControl("fcBodyRegion":U          + ipcContainerName, "wsCombo":U       , "10":U  , "tt_auth_coding.body_region":U     , "CHARACTER":U , 24, "Body Region":U)   
    oControl:ControlToken              = "updatable":U   
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AcronymSelect:ma_acBodyRegion:=":U 

    oControl = opoContainer:addControl("buProtocolView":U + opoContainer:ContainerCode, "cls.mipwsembedcontainer":U, "10":U,       "tt_auth_coding.owning_obj":U,                 "CHARACTER":U,  25, "":U) 

    oControl:RenderProcedureHandle     = mipEnv:Health:maUiService:RenderProcedureHandle 
    oControl:RenderProcedure           = "RenderProcedure":U 
    oControl:ControlTooltip            = "View Protocol(s)":U                      
    oControl:RenderArgument            = "RenderArgument=ProtocolViewButton":U 
                                       + "&BufferName=tt_auth_coding":U 
                                       + "&OwningMnemonicField=owning_entity_mnemonic":U 
                                       + "&OwningObjField=owning_obj":U 
                                       + "&OwningKeyField=owning_key":U    
                                                                                
    oControl                           = opoContainer:addControl("fcShowProcDate":U        + ipcContainerName, "wsInput":U       , "10":U  , "":U                               , "LOGICAL":U   , 98, "":U)
    oControl:ControlToken              = "hidden":U
    oControl:JavascriptOnChange        = "fnOnChangeAuthCodingProcDate(this);":U .
      
                                         
  ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(opoContainer)
  
         /* AD - Find out if we should be disabling unique key fields on update for this container ??????????
         oContainerProperties:AlternateKeyFields     = "cbCodingType":U     + ipcContainerName + ",":U 
                                                     + "fcOwningAltValue":U + ipcContainerName + ",":U
                                                     + "fdStartDate":U      + ipcContainerName
         */
         oContainerProperties:AutoSaveOperation      = "SERVICE:maUIService:ajaxSaveCodingContainer:":U + opoContainer:ContainerCode 
         oContainerProperties:DefaultLess            = TRUE
         oContainerProperties:CollapsableControlList = "":U.  
                                                     
                                                                                
  mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties).               
  
&ENDIF
  
  { mip/inc/mipcatcherror.i } 
    




