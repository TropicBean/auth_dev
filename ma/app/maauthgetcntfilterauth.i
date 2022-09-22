/*------------------------------------------------------------------------------
  Program   : maauthgetcntfilterauth.i
  Purpose   : Filter authorisation container definition   
  Parameters: 
  Notes     :     
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
     
&IF {&DBDFMA} >= 010195 &THEN  
    
  DEFINE VARIABLE lNotePerStatus            AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE lValidRule                AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE cRuleValue                AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE lValidHideThirdPAuthRule  AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE cHideThirdPAuthRuleValue  AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cMemNum                   AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE oControl                  AS cls.mipwscontrol   NO-UNDO.

  mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                         (INPUT  0,
                                          INPUT  0,
                                          INPUT  "ma_acAuthRuleTypeAUTHSETUPS":U,
                                          INPUT  "NotePerAuthStatus":U,
                                          INPUT  TODAY,
                                          OUTPUT lValidRule,
                                          OUTPUT cRuleValue).

  mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                         (INPUT  0,
                                          INPUT  0,
                                          INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                          INPUT  "Hide3rdPartyAuth":U,
                                          INPUT  TODAY,
                                          OUTPUT lValidHideThirdPAuthRule,
                                          OUTPUT cHideThirdPAuthRuleValue).


  ASSIGN 
    lNotePerStatus                     = (lValidRule AND LOOKUP(cRuleValue, "YES,Y,TRUE,T":U) > 0 )
    cMemNum                            = STRING(Warpspeed:CurrentObject:getItem("MemberNumber":U)) 

    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "98%":U, "":U, WarpSpeed:BaseClass, TRUE)
    opoContainer:ContainerTitle        = "Authorisation Search":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    
    oControl                           = opoContainer:addControl("fcAuthNum":U   + ipcContainerName, "wsInput":U        ,          "20":U, "":U , "character":U, 1, 1, "Authorisation Number:":U)
    oControl                           = opoContainer:addControl("fcMemNum":U    + ipcContainerName, "wsInput":U        ,          "20":U, "":U , "character":U, 1, 1, "Authorisation Number:":U)    
    oControl:ControlValue              = cMemNum
    oControl:ControlToken              = "Hidden":U
    oControl                           = opoContainer:addControl("lkpAuthNum":U  + ipcContainerName, "wsLookupButton":U ,          "20":U, "":U , "character":U, 1, 1, "Authorisation Number:":U)
    oControl:CellLayOutMask            = "&1&2" 
    oControl:LookupWobFLA              = "hatau":U
    oControl:LookupControls            = "fcAuthNum":U + ipcContainerName
    oControl:LookupFields              = "hat_auth.auth_num":U
    oControl:FilterControls            = "fcMemNum":U + ipcContainerName
    oControl:FilterFields              = "hat_auth.mem_num":U
    oControl:CellLayOutMask            = "&1&2&3":U 

    oControl                           = opoContainer:addControl("fcRefAuthNum":U + ipcContainerName,       "wsInput":U,           "20":U, "":U,  "character":U, 1, 2, "Reference Auth Number:":U)
    

    oControl                           = opoContainer:addControl("fdAuthCreateDateFrom":U + ipcContainerName, "wsInput":U,          "20":U, "":U, "date":U, 2, 1, "From Authorisation Create Date:":U)
    
    oControl                           = opoContainer:addControl("fdAuthFromDate":U + ipcContainerName,     "wsInput":U,           "20":U, "":U, "date":U, 2, 2, "From Authorisation Date:":U)  
    oControl:ControlToolTip            = "Filter according to 'Auth Start Date'"

    oControl                           = opoContainer:addControl("fdAuthCreateDateTo":U + ipcContainerName, "wsInput":U,          "20":U, "":U, "date":U, 3, 1, "To Authorisation Create Date:":U)

    oControl                           = opoContainer:addControl("fdAuthToDate":U   + ipcContainerName,       "wsInput":U,           "20":U, "":U,  "date":U,    3, 2, "To Authorisation Date:":U)
    oControl:ControlToolTip            = "Filter according to 'Auth End Date'"

    oControl                           = opoContainer:addControl("fcDocNum":U + ipcContainerName,  "wsInput":U ,                     "20":U, "":U, "character":U, 4, 1, "Service Provider Number:":U)  
    oControl                           = opoContainer:addControl("lkpDocNum":U + ipcContainerName, "wsLookupButton":U ,              "20":U, "":U, "character":U, 4, 1, "Service Provider Number:":U)
    oControl:LookupWobFLA              = "madoc":U
    oControl:LookupControls            = "fcDocNum":U + ipcContainerName
    oControl:LookupFields              = "doctor.doc-num":U
    oControl:FilterControls            = "fcDocNum":U + ipcContainerName
    oControl:FilterFields              = "doctor.doc-num":U
    oControl:CellLayOutMask            = "&1&2":U
    
    oControl                           = opoContainer:addControl("cbStatus":U + ipcContainerName,           "wsCombo":U,           "20":U, "":U,  "character":U, 4, 2, "Authorisation Status:":U)
    oControl:ControlClass              = "'style='width:140px;":U
    oControl:JavascriptOnChange        =  IF lNotePerStatus THEN "var x = fnGetControls(fcReasonType" + ipcContainerName + ")[0]; x.value = ~"AS~" + this.value":U ELSE oControl:JavascriptOnChange
    oControl:JavascriptOnChange        = oControl:JavascriptOnChange + "var oControl = fnGetControls(cbSystemStatus" + ipcContainerName + ")[0]; if(this.value!=~"~")~{oControl.value=~"~"; oControl.disabled = true~}else~{oControl.disabled = false~}":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "authRenderProcedure":U
    oControl:RenderArgument            = "AuthStatusCombo":U
    
    oControl                           = opoContainer:addControl("fcDocName":U + ipcContainerName,            "wsInput":U ,        "20":U, "":U , "character":U, 5, 1, "Service Provider Name:":U)  
    oControl:AjaxValidation            = "SERVICE:wsUIService:ajaxValidation:doctor|name":U                                                                              
    oControl:FilterFields              = "[ArgumentFieldValue]":U                                                                                                             
    oControl:ReturnFields              = "doc-num,name":U                                                                                           
    oControl:ReturnControls            = "fdAuthTypeObj" + ipcContainerName + ",fcDocName":U + ipcContainerName

    oControl                           = opoContainer:addControl("cbSystemStatus":U + ipcContainerName,       "wsCombo":U,           "20":U, "":U,  "character":U, 5, 2, "System Authorisation Status:":U)
    oControl:JavascriptOnChange        =  IF lNotePerStatus THEN "var x = fnGetControls(cbStatus" + ipcContainerName + ")[0]; x.value = ~"AS~" + this.value":U ELSE oControl:JavascriptOnChange
    oControl:JavascriptOnChange        = oControl:JavascriptOnChange + "var oControl = fnGetControls(cbStatus" + ipcContainerName + ")[0]; if(this.value!=~"~")~{oControl.value=~"~"; oControl.disabled = true~}else~{oControl.disabled = false~}":U
    oControl:ControlClass              = "'style='width:140px;":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "authRenderProcedure":U
    oControl:RenderArgument            = "FullSystemStatusCombo":U  
    
   

    oControl                           = opoContainer:addControl("cbAdditionalDocType":U + ipcContainerName,  "wsCombo":U ,        "20":U, "":U , "character":U, 6, 1, "Additional Provider Type:":U)  
    oControl:ControlClass              = "'style='width:140px;":U
    oControl:RenderProcedure           = "RenderProcedure":U
    oControl:RenderArgument            = "AcronymSelect:ma_acAuthProviderType:None=":U 

    oControl                           = opoContainer:addControl("fcReason":U                + ipcContainerName, "wsInput":U,        "15":U, "":U, "character":U, 6, 2, "":U)            
    oControl                           = opoContainer:addControl("fcReasonType":U            + ipcContainerName, "wsInput":U,        "5":U , "":U, "character":U, 6, 2, "":U)                  
    oControl:ControlToken              = "Hidden":U    
    oControl:ControlValue              = "AS*":U  
    oControl                           = opoContainer:addControl("buReasonBtn":U             + ipcContainerName, "wsLookupButton":U, "":U  , "":U, "":U,          6 , 2, "Status Reason:":U)                                                                                                                                                                                                   
    oControl:CellLayoutMask            = "&1&2&3":U                                                                                                                                             
    oControl:LookupWobFLA              = "note":U                                                                                                                                               
    oControl:LookupFields              = "note.key":U                                                                                                                                           
    oControl:LookupControls            = "fcReason":U + ipcContainerName                                                                                                                        
    oControl:FilterFields              = "note.key,note.type":U                                                                                                                                 
    oControl:FilterControls            = "fcReason":U + ipcContainerName + ",fcReasonType":U + ipcContainerName                                                                                 
    oControl:ReturnFields              = "note.key,note.narration[1]":U                                                                                                                                           
    oControl:ReturnControls            = "fcReason":U + ipcContainerName + ",fcReasonDesc":U + ipcContainerName  
  
    oControl                           = opoContainer:addControl("fcAdditionalDocNum":U + ipcContainerName,   "wsInput":U ,        "20":U, "":U , "character":U, 7, 1, "Additional Provider Number:":U)  
    oControl                           = opoContainer:addControl("lkpAdditionalDocNum":U + ipcContainerName,  "wsLookupButton":U , "20":U, "":U , "character":U, 7, 1, "Additional Provider Number:":U)
    oControl:LookupWobFLA              = "madoc":U
    oControl:FilterFields              = "doctor.doc-num":U                                                                                                                                 
    oControl:FilterControls            = "fcAdditionalDocNum":U + ipcContainerName
    oControl:LookupControls            = "fcAdditionalDocNum":U + ipcContainerName
    oControl:LookupFields              = "doctor.doc-num":U
    oControl:CellLayOutMask            = "&1&2":U 

    oControl                           = opoContainer:addControl("cbPrimaryType":U + ipcContainerName,      "wsCombo":U,           "20":U, "":U,  "character":U, 7, 2, "Primary Code Type:":U)
    oControl:ControlClass              = "'style='width:140px;":U
    oControl:AdditionalItems           = "None=":U
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthPrimaryCodingTypes'":U
    oControl:KeyField                  = "mic_acronym.acronym_value":U
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:DisplayMask               = "&1":U
    oControl:JavascriptOnChange        = " var lkpButton  = fnGetControls(lkpPrimaryCode" + ipcContainerName + ")[0],":U
                                       + " lkpField       = fnGetControls(fcPrimaryCode" + ipcContainerName + ")[0],":U
                                       + " DetailObjField = fnGetControls(fdPrimaryObj"   + ipcContainerName + ")[0];":U
                                       + " lkpButton.disabled   = (this.value == ~"~" || this.value == null) ;":U
                                       + " lkpField.disabled    = (this.value == ~"~" || this.value == null) ;":U 
                                       + " lkpField.value       = ~"~"; ":U
                                       + " DetailObjField.value = ~"~"; ":U
   
    
    oControl                           = opoContainer:addControl("cbAuthYear":U + ipcContainerName,           "wsCombo":U,         "20":U, "":U,  "character":U, 8, 1, "Authorisation Year:":U)
    oControl:ControlClass              = "'style='width:140px;":U
    oControl:DisplayMask               = "&1":U
    oControl:AdditionalItems           = "All=All|":U                                   
                                       + STRING(YEAR(TODAY),"9999") + "=Cur|":U         
                                       + STRING(YEAR(TODAY) - 1,"9999") + "=Prev|< ":U  
                                       + STRING(YEAR(TODAY) - 1,"9999") + "=Hist":U  

    oControl                           = opoContainer:addControl("fdPrimaryObj":U + ipcContainerName,         "wsInput":U ,        "20":U, "":U , "character":U, 8, 2, "Primary Item Code:":U)
    oControl:ControlToken              = "Hidden":U                                                                                
    oControl                           = opoContainer:addControl("fcPrimaryCode":U + ipcContainerName,        "wsInput":U ,        "20":U, "":U , "character":U, 8, 2, "Primary Item Code:":U)  
    oControl:RenderProcedure           = "WEbRenderProcedure":U
    oControl:RenderArgument            = "PrimaryCode":U

    oControl                           = opoContainer:addControl("lkpPrimaryCode":U + ipcContainerName,       "wsLookupButton":U , "20":U, "":U , "character":U, 8, 2, "Primary Item Code:":U)
    oControl:LookupWobFLA              = "slent":U
    oControl:LookupFields              = "KEY_FIELD,CODE_FIELD":U
    oControl:LookupControls            = "fdPrimaryObj" + ipcContainerName + ",fcPrimaryCode":U + ipcContainerName
    oControl:FilterControls            = "cbPrimaryType":U + ipcContainerName
    oControl:FilterFields              = "QUERY_OEM":U  
    oControl:ReturnControls            = "fcPrimaryCode" + ipcContainerName + ",fdPrimaryObj":U  + ipcContainerName 
    oControl:ReturnFields              = "CODE_FIELD,KEY_FIELD":U
    oControl:CellLayOutMask            = "&1&2&3":U 
    oControl:RenderProcedure           = "WEbRenderProcedure":U
    oControl:RenderArgument            = "PrimaryCode":U

    oControl                           = opoContainer:addControl("fiDependant":U + ipcContainerName,          "wsCombo":U,         "20":U, "":U,  "character":U, 9, 1, "Dependant:":U)
    oControl:ControlClass              = "'style='width:140px;":U 
    oControl:AdditionalItems           = "All=":U
    oControl:QueryString               = "FOR EACH memdep NO-LOCK WHERE memdep.mem-num = " + QUOTER(cMemNum) + "AND memdep.first-name <> 'Zero Zero'"
    oControl:KeyField                  = "memdep.dependant":U
    oControl:DisplayFields             = "memdep.dependant,memdep.first-name":U
    oControl:DisplayMask               = "&1 - &2":U
    
    oControl                           = opoContainer:addControl("fdAuthTypeObj":U + ipcContainerName,        "wsInput":U ,        "20":U, "":U , "character":U, 9, 2, "Authorisation Type:":U)
    oControl:ControlToken              = "Hidden":U                                                           
    oControl                           = opoContainer:addControl("fcAuthType":U + ipcContainerName,           "wsInput":U ,        "20":U, "":U , "character":U, 9, 2, "Authorisation Type:":U)
    oControl:AjaxValidation            = "SERVICE:wsUIService:ajaxValidation:hacat|auth_type":U                                                                                
    oControl:FilterFields              = "[ArgumentFieldValue]":U                                                                                                               
    oControl:ReturnFields              = "auth_type_obj,auth_type":U                                                                                            
    oControl:ReturnControls            = "fdAuthTypeObj" + ipcContainerName + ",fcAuthType":U + ipcContainerName                                                                                  
    oControl                           = opoContainer:addControl("lkpAuthType":U + ipcContainerName,          "wsLookupButton":U , "20":U, "":U , "character":U, 9, 2, "Authorisation Type:":U)
    oControl:LookupWobFLA              = "hacat":U
    oControl:LookupControls            = "fcAuthType":U + ipcContainerName
    oControl:LookupFields              = "hac_auth_type.auth_type":U
    oControl:CellLayOutMask            = "&1&2&3":U 



    oControl                           = opoContainer:addControl("fcExternalRef":U + ipcContainerName,        "wsInput":U,         "20":U, "":U,  "character":U, 10, 1, "External Reference:":U)
    
    oControl                           = opoContainer:addControl("cbDetailEntity":U + ipcContainerName,       "wsCombo":U,         "20":U, "":U , "character":U, 10, 2, "Detail Entity:":U)
    oControl:ControlClass              = "'style='width:140px;":U
    oControl:AdditionalItems           = "None=":U
    oControl:QueryString               = "FOR EACH mic_acronym NO-LOCK WHERE mic_acronym.category_key = 'ma_acAuthDetailEntities'":U
    oControl:KeyField                  = "mic_acronym.acronym_value":U
    oControl:DisplayFields             = "mic_acronym.acronym_label":U
    oControl:DisplayMask               = "&1":U 
    oControl:JavascriptOnChange        = " var lkpButton  = fnGetControls(lkpDetailCode" + ipcContainerName + ")[0],":U
                                       + " lkpField       = fnGetControls(fcDetailCode"  + ipcContainerName + ")[0],":U
                                       + " DetailObjField = fnGetControls(fdDetailObj"   + ipcContainerName + ")[0];":U
                                       + " lkpButton.disabled   = (this.value == ~"~" || this.value == null) ;":U
                                       + " lkpField.disabled    = (this.value == ~"~" || this.value == null) ;":U 
                                       + " lkpField.value       = ~"~"; ":U
                                       + " DetailObjField.value = ~"~"; ":U

    oControl                           = opoContainer:addControl("fcAuthIncomplete":U + ipcContainerName,     "wsCombo":U,         "20":U, "":U,  "character":U, 11, 1, "Incomplete/Complete:":U)
    oControl:ControlClass              = "'style='width:140px;":U
    oControl:AdditionalItems           = "Both=|Complete=no|Incomplete=yes":U

    oControl                           = opoContainer:addControl("fdDetailObj":U + ipcContainerName,          "wsInput":U ,        "20":U, "":U , "character":U, 11, 2, "Detail Item Code:":U)
    oControl:ControlToken              = "Hidden":U                                                                                
    oControl                           = opoContainer:addControl("fcDetailCode":U + ipcContainerName,         "wsInput":U ,        "20":U, "":U , "character":U, 11, 2, "Detail Item Code:":U)  
    oControl:RenderProcedure           = "WEbRenderProcedure":U
    oControl:RenderArgument            = "AuthDetailFilter":U 
 
    oControl                           = opoContainer:addControl("lkpDetailCode":U + ipcContainerName,        "wsLookupButton":U , "20":U, "":U , "character":U, 11, 2, "Detail Item Code:":U)
    oControl:LookupWobFLA              = "slent":U
    oControl:LookupFields              = "KEY_FIELD,CODE_FIELD":U
    oControl:LookupControls            = "fdDetailObj" + ipcContainerName + ",fcDetailCode":U + ipcContainerName
    oControl:FilterControls            = "cbDetailEntity":U + ipcContainerName
    oControl:FilterFields              = "QUERY_OEM":U  
    oControl:ReturnControls            = "fcDetailCode" + ipcContainerName + ",fdDetailObj":U + ipcContainerName 
    oControl:ReturnFields              = "CODE_FIELD,KEY_FIELD":U
    oControl:CellLayOutMask            = "&1&2&3":U 
    oControl:RenderProcedure           = "WEbRenderProcedure":U
    oControl:RenderArgument            = "AuthDetailFilter":U  
    
    oControl                           = opoContainer:addControl("fiNumRecords":U  + ipcContainerName,         "wsInput":U ,        "5":U,  "":U ,"integer":U,   12, 1, "Records:":U)  
    oControl:ControlValue              = "5":U

    oControl                           = opoContainer:addControl("flShowMKAuths":U + ipcContainerName,         "wsCheckBox":U,     "20":U, "":U,  "logical":U,   12, 2, "Show MK Auths?":U)
   
    oControl                           = opoContainer:addControl("flIncludeThrdPtyAuths":U + ipcContainerName, "wsCheckBox":U,      "20":U, "":U, "logical":U,   13, 2, "Include 3rd Party Auths?":U)
    oControl:CellClass                 = IF NOT lValidHideThirdPAuthRule THEN "+clHid":U ELSE "":U
    oControl:LabelClass                = IF NOT lValidHideThirdPAuthRule THEN "+clHid":U ELSE "-clHid":U
 

    oControl                           = opoContainer:addControl("buSearchAuths":U + ipcContainerName,        "":U,                "":U  , "1":U, "character":U, 14, 2, "":U)
    oControl:SpanOverlabel             = TRUE
    oControl:CellClass                 = WarpSpeed:BaseClass + "ButtonBar":U
    oControl:CellSnippet               = "align='right'":U
    oControl:ColumnSpan                = 3
    oControl:SubContainer              = wsUiService:getButtonContainer(WarpSpeed:CurrentWob + ipcContainerName + "BtnBar":U, "Search:Clear").

{ mip/inc/mipcatcherror.i } 

&ENDIF

