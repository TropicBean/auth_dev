/* maauthwobrowrenderprocedure.i  MEDSTAR Medical Aid System
                                  Authorisation Wob RowRenderProcedure Common Code
                                  (c) Copyright 1990 - 2018
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/ 

  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.

  
  CASE ipoContainer:RowRenderArgument:
  
    WHEN "AuthProviderContainer":U THEN
    DO:      

      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hatap":U,
         INPUT DECIMAL(ipoContainer:getControl("fdAuthProviderObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U). 
         
    END. /*WHEN "AuthProviderContainer":U THEN*/
    
    WHEN "AuthCPTCodingContainer":U THEN
    DO:      

      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hatac:hlmck":U,
         INPUT DECIMAL(ipoContainer:getControl("fdAuthCodingObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).  
         
    END. /*WHEN "AuthCPTCodingContainer":U THEN*/
    
    WHEN "AuthICDCodingContainer":U THEN
    DO:      

      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hatac:diagnos":U,
         INPUT DECIMAL(ipoContainer:getControl("fdAuthCodingObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).  
         
    END. /*WHEN "AuthICDCodingContainer":U THEN*/
    
    WHEN "AuthFlagContainer":U THEN
    DO:
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_flag_value_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hataf":U,
         INPUT DECIMAL(ipoContainer:getControl("fdFlagValueObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).  
           
    END. /*WHEN "AuthFlagContainer":U THEN*/
    
    WHEN "AuthDetailContainer":U THEN
    DO:
      
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hatad:":U + ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_detail.owning_entity_mnemonic":U, "BUFFER-VALUE":U), 
         INPUT DECIMAL(ipoContainer:getControl("fdAuthDetailObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).  
         
    END. /*WHEN "AuthDetailContainer":U THEN*/
    
    WHEN "AuthMCSavingsContainer":U THEN
    DO:
      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hatms":U,
         INPUT DECIMAL(ipoContainer:getControl("fdAuthMCSavingsObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).  
           
    END. /*WHEN "AuthMCSavingsContainer":U THEN*/    
      
  END CASE.
  
  { mip/inc/mipcatcherror.i }

