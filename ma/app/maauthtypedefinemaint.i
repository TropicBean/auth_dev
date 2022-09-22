 /* maauthtypedefinemaint.i MEDSTAR Medical Aid System
                            Save Authorisation Maint Header Record
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/ 
  DEFINE VARIABLE oControl  AS cls.mipwscontrol  NO-UNDO.
  DEFINE VARIABLE iControl  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
  
  IF goWob:Mode = "Maint":U 
  THEN DO:
    ASSIGN
      goCntMaint                                = goWob:getContainer("Maint":U).

    IF gdAuthTypeProviderObj <> 0.00 OR goWob:SubmitValue = "MaintSubmit" THEN
      ASSIGN
        goCntMaint:QueryString                    = ("FOR EACH tt_auth_type NO-LOCK":U
                                                  + "    WHERE tt_auth_type.auth_type_obj = '&1'":U
                                                  + "   ,FIRST erm_insurer NO-LOCK ":U
                                                  + "    WHERE erm_insurer.insurer_obj = tt_auth_type.insurer_obj OUTER-JOIN":U
                                                  + "   ,FIRST tt_auth_type_provider NO-LOCK ":U
                                                  + "    WHERE tt_auth_type_provider.auth_type_provider_obj = ":U + STRING(gdAuthTypeProviderObj) + " OUTER-JOIN":U).
    ELSE
      ASSIGN
        goCntMaint:QueryString                    = ("FOR EACH tt_auth_type NO-LOCK":U
                                                  + "    WHERE tt_auth_type.auth_type_obj = '&1'":U
                                                  + "   ,FIRST erm_insurer NO-LOCK ":U
                                                  + "    WHERE erm_insurer.insurer_obj = tt_auth_type.insurer_obj OUTER-JOIN":U).
    ASSIGN
      oControl                                  = goCntMaint:addControl("fcAuthType":U                + goCntMaint:ContainerCode  , "wsInput":U,      "20":U, "tt_auth_type.auth_type":U,      "character":U, 1, 1, "Authorisation Type:":U)
      oControl:ControlClass                     = "+clMan":U


      oControl                                  = goCntMaint:addControl("fcAuthTSeqKey":U             + goCntMaint:ContainerCode  , "wsInput":U,        "30":U, "tt_auth_type.sequence_key":U,   "character":U, 1, 2, "":U)
      oControl:ControlTooltip                   = "If no sequence is specified, the default sequence will be used."
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "SeqKey":U

      oControl                                  = goCntMaint:addControl("flSeqActive":U               + goCntMaint:ContainerCode  , "wsInput":U,        "":U,   "":U,                          "logical":U,   1, 2, "":U)
      oControl:ControlValue                     = "TRUE":U
      oControl:ControlToken                     = "Hidden":U

      oControl                                  = goCntMaint:addControl("fcAuthTypeSequenceKey":U     + goCntMaint:ContainerCode  , "wsInput":U,        "":U,   "":U,                          "character":U, 1, 2, "":U)
      oControl:ControlValue                     = "ma_Authorisation_Sequence":U
      oControl:ControlToken                     = "Hidden":U

      oControl                                  = goCntMaint:addControl("buSequenceBtn":U             + goCntMaint:ContainerCode  , "wsLookupButton":U, "":U,   "":U,                          "":U,          1, 2, "SequenceKey:":U)
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "SeqBtn":U

      oControl:LookupWobFLA                     = "micsq":U
      oControl:LookupFields                     = "mic_sequence.sequence_key ":U
      oControl:LookupControls                   = "fcAuthTSeqKey":U + goCntMaint:ContainerCode
      oControl:FilterFields                     = "mic_sequence.category_key,mic_sequence.sequence_active":U
      oControl:FilterControls                   = "fcAuthTypeSequenceKey":U + goCntMaint:ContainerCode + ",flSeqActive":U + goCntMaint:ContainerCode
      oControl:ReturnFields                     = "mic_sequence.sequence_key":U
      oControl:ReturnControls                   = "fcAuthTSeqKey":U + goCntMaint:ContainerCode

      oControl:CellLayoutMask                   = "&1&2&3&4":U

      oControl                                  = goCntMaint:addControl("fcPrefix":U                  + goCntMaint:ContainerCode  , "wsInput":U,    "20":U, "tt_auth_type.auth_type_prefix":U, "character":U,   2, 2, "Prefix:":U)
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "Prefix":U
                                                                                                                                                                                                                
      oControl                                  = goCntMaint:addControl("fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode  , "wsInput":U,    "5":U , "":U,                              "character":U,   2, 1, "":U)
      oControl:ControlToken                     = "Hidden":U                                                                                                                                                    
      oControl:ControlValue                     = "ermin":U                                                                                                                                                     
                                                                                                                                                                                                                
      oControl                                  = goCntMaint:addControl("fcInsurerArgumentField":U    + goCntMaint:ContainerCode  , "wsInput":U,    "5":U , "":U,                              "character":U,   2, 1, "":U)
      oControl:ControlToken                     = "Hidden":U                                                                                                                                                    
      oControl:ControlValue                     = "[CodeField]":U                                                                                                                                               
                                                                                                                                                                                                                
      oControl                                  = goCntMaint:addControl("fdInsurerObj":U              + goCntMaint:ContainerCode  , "wsInput":U   , "1":U , "tt_auth_type.insurer_obj":U,      "character":U,   2, 1, "":U)
      oControl:ControlToken                     = "Hidden":U                                                                                                                                                    
      oControl                                  = goCntMaint:addControl("fcInsurer":U                 + goCntMaint:ContainerCode  , "wsInput":U   , "20":U, "erm_insurer.insurer_name":U,      "character":U,   2, 1, "":U)
      oControl:ControlTooltip                   = "Please enter a valid client":U
      oControl:AjaxValidation                   = "SERVICE:wsUIService:ajaxValidation:":U
      oControl:FilterFields                     = "[ArgumentFieldValue],[TableMnemonic],[ArgumentField]":U
      oControl:FilterControls                   = "fcInsurer":U + goCntMaint:ContainerCode + ",fcInsurerArgumentMnemonic":U + goCntMaint:ContainerCode + ",fcInsurerArgumentField":U + goCntMaint:ContainerCode
      oControl:ReturnFields                     = "[RecordObj]":U
      oControl:ReturnControls                   = "fdInsurerObj":U + goCntMaint:ContainerCode
  
      oControl                                  = goCntMaint:addControl("buInsurerBtn":U                    + goCntMaint:ContainerCode  , "wsLookupButton":U, "":U  , "":U,                    "":U          , 2, 1, "Client:":U)
      oControl:LookupWobFLA                     = "ermin":U
      oControl:LookupFields                     = "erm_insurer.insurer_code":U
      oControl:LookupControls                   = "fcInsurer":U + goCntMaint:ContainerCode
      oControl:ReturnFields                     = "erm_insurer.insurer_obj,erm_insurer.insurer_code":U
      oControl:ReturnControls                   = "fdInsurerObj":U + goCntMaint:ContainerCode + ",fcInsurer":U + goCntMaint:ContainerCode
      oControl:CellLayoutMask                   = "&1&2&3&4&5":U

      oControl                                  = goCntMaint:addControl("fcClaimCodeUserObjs":U             + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "":U,                  "character":U, 3, 2, "":U)
      oControl:ControlToken                     = "Hidden"                                       
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcClaimCodeUsers'     + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'
      oControl                                  = goCntMaint:addControl("fcClaimCodeUsers":U                + goCntMaint:ContainerCode  , "wsTextArea":U,       "10":U, "tt_auth_type.claim_code_upd_user":U, "character":U,  3, 2, "":U)
      oControl:ControlTooltip                   = "Please enter a valid user(s) or enter None Allowed if no users are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcClaimCodeUsersLkp":U             + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,   "":U,                                 "":U,           3, 2, "Claim Code Update Allowed (Users):")
      oControl:LookupWobFLA                     = "mimus":U
      oControl:LookupFields                     = "mim_user.user_obj":U
      oControl:LookupControls                   = "fcClaimCodeUserObjs" + goCntMaint:ContainerCode 
      oControl:FilterFields                     = "mim_user.user_obj":U
      oControl:FilterControls                   = "fcClaimCodeUserObjs" + goCntMaint:ContainerCode 
      oControl:CellLayoutMask                   = "&1&2&3"

      oControl                                  = goCntMaint:addControl("fcDescription":U                   + goCntMaint:ContainerCode, "wsTextArea":U, "20":U, "tt_auth_type.description":U,                "character":U , 3, 1, "Description:":U)
      oControl:ControlClass                     = "+clMan":U  
                                                                                                                                    
      oControl                                  = goCntMaint:addControl("fcEffectiveDate":U                 + goCntMaint:ContainerCode  , "wsInput":U,  "20":U, "tt_auth_type.effective_date":U,              "date":U     , 4, 1, "Effective Date:":U)
      oControl:ControlClass                     = "+clMan":U

      oControl                                  = goCntMaint:addControl("fcClaimCodeRoleObjs":U             + goCntMaint:ContainerCode, "wsInput":U,    "20":U, "":U,                                        "character":U,  4, 2, "":U)
      oControl:ControlToken                     = "Hidden"
      oControl:CellLayoutMask                   = "&1&2&3"
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcClaimCodeRoles'     + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'
      oControl                                  = goCntMaint:addControl("fcClaimCodeRoles":U                + goCntMaint:ContainerCode, "wsTextArea":U,       "10":U, "tt_auth_type.claim_code_upd_role":U, "character":U,   4, 2,  "":U)
      oControl:ControlTooltip                   = "Please enter a valid role(s) or enter None Allowed if no roles are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcClaimCodeRolesLkp":U             + goCntMaint:ContainerCode, "wsMsComboButton":U,  "":U,   "":U,                                 "":U,            4, 2,  "Claim Code Update Allowed (Roles):")
      oControl:LookupWobFLA                     = "mimro":U
      oControl:LookupFields                     = "mim_role.role_obj":U
      oControl:LookupControls                   = "fcClaimCodeRoleObjs" + goCntMaint:ContainerCode
      oControl:FilterFields                     = "mim_role.role_obj":U
      oControl:FilterControls                   = "fcClaimCodeRoleObjs" + goCntMaint:ContainerCode

      oControl                                  = goCntMaint:addControl("fcEndDate":U                        + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "tt_auth_type.end_date":U,            "date":U      , 5, 1, "End Date:":U)

      oControl                                  = goCntMaint:addControl("fcClaimTypeUserObjs":U              + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "":U,                                 "character":U,  5, 2, "":U)
      oControl:ControlToken                     = "Hidden":U
      /* Displays [MULTIPLE SELECTED] on the 'Claim type user' field after selecting multiple users. */                                                                                                                                                     
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcClaimTypeUsers'      + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'                                   
      oControl                                  = goCntMaint:addControl("fcClaimTypeUsers":U                 + goCntMaint:ContainerCode  , "wsTextArea":U,       "10":U, "tt_auth_type.claim_type_upd_user":U,  "character":U,  5, 2, "":U)
      oControl:ControlTooltip                   = "Please enter a valid user(s) or enter None Allowed if no users are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcClaimTypeUsersLkp":U              + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,   "":U,                                  "":U,           5, 2, "Claim Type Update Allowed (Users):")
      oControl:LookupWobFLA                     = "mimus":U
      oControl:LookupFields                     = "mim_user.user_obj":U
      oControl:LookupControls                   = "fcClaimTypeUserObjs" + goCntMaint:ContainerCode
      oControl:FilterFields                     = "mim_user.user_obj":U
      oControl:FilterControls                   = "fcClaimTypeUserObjs" + goCntMaint:ContainerCode
      oControl:CellLayoutMask                   = "&1&2&3":U

      oControl                                  = goCntMaint:addControl("fcClaimTypeRoleObjs":U          + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "":U,                                     "character":U,  6,2, "":U)
      oControl:ControlToken                     = "Hidden":U
      oControl:CellLayoutMask                   = "&1&2&3":U
      /* Displays [MULTIPLE SELECTED] on the 'Claim type roles' field after selecting multiple roles. */ 
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcClaimTypeRoles'  + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'                                  
      oControl                                  = goCntMaint:addControl("fcClaimTypeRoles":U             + goCntMaint:ContainerCode  , "wsTextArea":U,       "10":U, "tt_auth_type.claim_type_upd_role":U,     "character":U,  6,2,  "":U)
      oControl:ControlTooltip                   = "Please enter a valid role(s) or enter None Allowed if no roles are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcClaimTypeRolesLkp":U          + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,   "":U,                                     "":U,           6,2,  "Claim Type Update Allowed (Roles):")
      oControl:LookupWobFLA                     = "mimro":U
      oControl:LookupFields                     = "mim_role.role_obj":U
      oControl:LookupControls                   = "fcClaimTypeRoleObjs" + goCntMaint:ContainerCode
      oControl:FilterFields                     = "mim_role.role_obj":U
      oControl:FilterControls                   = "fcClaimTypeRoleObjs" + goCntMaint:ContainerCode

      oControl                                  = goCntMaint:addControl("fcTypeAcronym":U                + goCntMaint:ContainerCode  , "wsCombo":U,          "20":U, "tt_auth_type.auth_type_group":U,         "character":U , 7,1, "Authorisation Type Group:":U)
      oControl:RenderProcedure                  = "RenderProcedure":U
      oControl:RenderArgument                   = "AcronymSelect:ma_acAuthTypeGroup:=":U
      oControl:ControlClass                     = "+clMan":U

      oControl                                  = goCntMaint:addControl("fcMultipleCCMessageType":U      + goCntMaint:ContainerCode  , "wsCombo":U,          "20":U, "tt_auth_type.multiple_cc_message_type":U,         "character":U , 7,2, "Multiple CC Message Type:":U)
      oControl:AdditionalItems                  = "=":U
      oControl:QueryString                      = "FOR EACH mic_acronym NO-LOCK":U
                                                + "   WHERE mic_acronym.category_key = 'ma_acAuthTypeMultipleCCMessageType'":U
                                                + "      BY mic_acronym.acronym_label":U
      oControl:KeyField                         = "mic_acronym.acronym_key":U
      oControl:DisplayFields                    = "mic_acronym.acronym_label":U 
      oControl:ControlTooltip                   = "Select the warning message type used to warn the user where there are multiple valid Claim Codes or Claim Types":U

      oControl                                  = goCntMaint:addControl("fcExtType":U                    + goCntMaint:ContainerCode  , "wsTextArea":U,        "10":U, "tt_auth_type.external_auth_type_group":U, "character":U,  8, 2, "External Authorisation Type Group:":U)

      oControl                                  = goCntMaint:addControl("chAllowUpdates":U               + goCntMaint:ContainerCode  , "wsCheckBox":U,       "20":U, "tt_auth_type.updates_allowed":U,         "character":U,  8, 1, "Updates Allowed:":U)

      oControl                                  = goCntMaint:addControl("fcHeaderValue":U                + goCntMaint:ContainerCode  , "wsCombo":U,          "20":U, "tt_auth_type.header_values_allowed":U,   "character":U , 9, 1, "Header Values Allowed:":U)
      oControl:RenderProcedure                  = "RenderProcedure":U                                    
      oControl:RenderArgument                   = "AcronymSelect:ma_acAuthHeadValAllowed:=":U            

      oControl                                  = goCntMaint:addControl("chUnlimitedHeader":U            + goCntMaint:ContainerCode  , "wsCheckBox":U,       "20":U, "tt_auth_type.header_values_unlimited":U, "character":U,  9, 2, "Header Values Unlimited:":U)
                                                                                                         
      oControl                                  = goCntMaint:addControl("fcStatusDefault":U              + goCntMaint:ContainerCode  , "wsCombo":U,          "20":U, "tt_auth_type.default_auth_status":U,     "character":U,  10,1, "Default Authorisation Status:":U)
      oControl:ControlTooltip                   = "Select a status from the drop-down list.":U 
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "Status":U

      oControl:AjaxValidation                   = "START:":U + Warpspeed:CurrentWob + ":ajaxValidationAuthType:Status":U
      oControl:FilterFields                     = "[Status],[InsurerObj],[StartDate]":U                                                                                               
      oControl:FilterControls                   = "fcStatusDefault":U + goCntMaint:ContainerCode + ",fdInsurerObj":U + goCntMaint:ContainerCode + ",fcEffectiveDate":U + goCntMaint:ContainerCode                                    
      oControl:ReturnFields                     = "[StatusReasonMandatory]":U                           
      oControl:ReturnControls                   = "flStatusReasonMandatory":U + goCntMaint:ContainerCode

      oControl                                  = goCntMaint:addControl("flStatusReasonMandatory":U + goCntMaint:ContainerCode  , "wsInput":U,         "15":U, "":U,                                           "character":U,  10, 1, "Default Authorisation Status:":U)
      oControl:ControlToken                     = "Hidden":U                                                                                                                                      
      oControl:JavaScriptOnChange               = "fnOnChangeStatusReason(this);":U                                                                                                                                       

      oControl                                  = goCntMaint:addControl("fcNoteType":U              + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "":U,                                           "character":U,  10, 1, "Default Authorisation Status:":U)
      oControl:ControlToken                     = "Hidden":U
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "NoteType":U
      oControl:CellLayoutMask                   = "&1&2&3":U
      .

    ASSIGN
      oControl                                  = goCntMaint:addControl("fcReasonKey":U            + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "tt_auth_type.default_auth_status_note":U,           "character":U,  10, 2, "":U)
      oControl:RenderProcedure                  = "customRenderProcedure":U                                                                                                                                    
      oControl:RenderArgument                   = "StatusReason":U  
      
      oControl                                  = goCntMaint:addControl("buReasonLkp":U            + goCntMaint:ContainerCode  , "wsLookupButton":U,   "20":U, "":U,                                                "":U,           10, 2, "Default Status Reason:":U)
      oControl:LookupWobFLA                     = "note":U
      oControl:LookupFields                     = "note.key":U
      oControl:LookupControls                   = "fcReasonKey":U + goCntMaint:ContainerCode
      oControl:FilterFields                     = "note.type":U
      oControl:FilterControls                   = "fcNoteType":U + goCntMaint:ContainerCode
      oControl:ReturnFields                     = "note.key":U
      oControl:ReturnControls                   = "fcReasonKey":U + goCntMaint:ContainerCode
      oControl:CellLayoutMask                   = "&1&2"
      oControl:RenderProcedure                  = "customRenderProcedure":U                                                                                                                                    
      oControl:RenderArgument                   = "StatusReason":U  
      
      oControl                                  = goCntMaint:addControl("fcStatusUserObjs":U             + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "":U,                                          "character":U,  11, 1, "":U)
      oControl:ControlToken                     = "Hidden"                                       
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcStatusUsers'     + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'
      oControl                                  = goCntMaint:addControl("fcStatusUsers":U                + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "tt_auth_type.default_auth_status_upd_user":U, "character":U,  11, 1, "":U)
      oControl:ControlTooltip                   = "Please enter a valid user(s) or enter None Allowed if no users are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcStatusUsersLkp":U             + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,  "":U,                                           "":U,           11, 1, "Authorisation Status Update Allowed (Users):")
      oControl:LookupWobFLA                     = "mimus":U
      oControl:LookupFields                     = "mim_user.user_obj":U
      oControl:LookupControls                   = "fcStatusUserObjs" + goCntMaint:ContainerCode 
      oControl:FilterFields                     = "mim_user.user_obj":U
      oControl:FilterControls                   = "fcStatusUserObjs" + goCntMaint:ContainerCode 
      oControl:CellLayoutMask                   = "&1&2&3"
      
      oControl                                  = goCntMaint:addControl("fcStatusRoleObjs":U        + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "":U,                                               "character":U,  11,2, "":U)
      oControl:ControlToken                     = "Hidden"
      oControl:CellLayoutMask                   = "&1&2&3"
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcStatusRoles' + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'
      oControl                                  = goCntMaint:addControl("fcStatusRoles":U + goCntMaint:ContainerCode  , "wsTextArea":U,          "10":U, "tt_auth_type.default_auth_status_upd_role":U,             "character":U,  11,2,  "":U)
      oControl:ControlTooltip                   = "Please enter a valid role(s) or enter None Allowed if no roles are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcStatusRolesLkp":U    + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,  "":U,                                                    "":U,           11,2,  "Authorisation Status Update Allowed (Roles):")
      oControl:LookupWobFLA                     = "mimro":U
      oControl:LookupFields                     = "mim_role.role_obj":U
      oControl:LookupControls                   = "fcStatusRoleObjs" + goCntMaint:ContainerCode 
      oControl:FilterFields                     = "mim_role.role_obj":U
      oControl:FilterControls                   = "fcStatusRoleObjs" + goCntMaint:ContainerCode
      
      oControl                                  = goCntMaint:addControl("fcIcdCondition":U               + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "tt_auth_type.icd_cond_code":U,                "character":U,  12, 1, "":U)
      oControl                                  = goCntMaint:addControl("fcIcdConditionLkp":U            + goCntMaint:ContainerCode  , "wsLookupButton":U,   "20":U, "":U,                                          "":U,           12, 1, "ICD Condition Code:":U)
      oControl:LookupWobFla                     = "condition":U
      oControl:LookupFields                     = "condition.cond-code":U
      oControl:LookupControls                   = "fcIcdCondition":U + goCntMaint:ContainerCode
      oControl:FilterFields                     = "condition.cond-code":U
      oControl:FilterControls                   = "fcIcdCondition":U + goCntMaint:ContainerCode
      oControl:ReturnFields                     = "condition.cond-code,condition.description[1]":U
      oControl:ReturnControls                   = "fcIcdCondition":U + goCntMaint:ContainerCode
      oControl:CellLayoutMask                   = "&1&2"
      
      oControl                                  = goCntMaint:addControl("fcIcdCondType":U                + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "tt_auth_type.icd_cond_type":U,                "character":U,  12, 2, "":U)
      oControl                                  = goCntMaint:addControl("fcIcdCondTypeLkp":U             + goCntMaint:ContainerCode  , "wsLookupButton":U,   "20":U, "":U,                                          "":U,           12, 2, "ICD Condition Type:":U)
      oControl:LookupWobFLA                     = "condtype":U
      oControl:LookupFields                     = "condtype.cond-type":U
      oControl:LookupControls                   = "fcIcdCondType":U + goCntMaint:ContainerCode
      oControl:ReturnFields                     = "condtype.cond-type":U
      oControl:ReturnControls                   = "fcIcdCondType":U + goCntMaint:ContainerCode
      oControl:CellLayoutMask                   = "&1&2":U
      
      oControl                                  = goCntMaint:addControl("fcIcdObjs":U                    + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "":U,                                          "character":U,  13, 1, "":U)
      oControl:ControlToken                     = "Hidden":U
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcIcds'            + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'                                 
      oControl                                  = goCntMaint:addControl("fcIcds":U                       + goCntMaint:ContainerCode  , "wsInput":U,          "20":U, "tt_auth_type.valid_icds":U,                   "character":U,  13, 1, "":U)
      oControl                                  = goCntMaint:addControl("fcIcdsLkp":U                    + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,   "":U,                                          "":U,           13, 1, "Valid ICD Code(s):")
      // MMP-546 Changes done for new multi select
      oControl:LookupWobFLA                     = "diagnos":U
      oControl:LookupFields                     = "diagnos.diagnos-obj":U
      oControl:LookupControls                   = "fcIcdObjs" + goCntMaint:ContainerCode
      oControl:FilterFields                     = "hlm_icd_industry.effective_date <=":U
      oControl:FilterControls                   = "fcEffectiveDate":U + goCntMaint:ContainerCode                                                                                                                       
      oControl:LookupFields                     = "diagnos.diagnos-obj":U
      oControl:LookupControls                   = "fcIcdObjs" + goCntMaint:ContainerCode
      oControl:CellLayoutMask                   = "&1&2&3"                                                                                                                                                    
      
      oControl                                  = goCntMaint:addControl("fcAuthAllService":U             + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.authorise_all_services":U,         "character":U,  14, 1, "Authorise All Services:":U) 
                                                                                                                                                                                                    
      oControl                                  = goCntMaint:addControl("fcDetailBase":U                 + goCntMaint:ContainerCode  , "wsCombo":U,        "20":U, "tt_auth_type.default_line_restriction":U,       "character":U,  14, 2, "Default Line Restrictions:":U)
      oControl:RenderProcedure                  = "RenderProcedure":U                                                                                                                                         
      oControl:RenderArgument                   = "AcronymSelect:ma_acAuthLineRestriction:=":U                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                  
      oControl                                  = goCntMaint:addControl("fcActivateBodyRegion":U         + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_body_region":U,           "character":U,  15, 1, "Activate Body Region:":U) 
      
      oControl                                  = goCntMaint:addControl("fcEndDateUpdUserObjs":U         + goCntMaint:ContainerCode  , "wsInput":U,        "20":U, "":U,                                            "character":U,  15, 2, "":U)
      oControl:ControlToken                     = "Hidden"                                                                                                                                                    
      /* Displays [MULTIPLE SELECTED] on the 'End date user' field after selecting multiple users. */                                                                                                                                             
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcEndDateUpdUsers' + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'                                 
      oControl                                  = goCntMaint:addControl("fcEndDateUpdUsers":U            + goCntMaint:ContainerCode  , "wsTextArea":U,       "10":U, "tt_auth_type.end_date_upd_user":U,            "character":U,  15, 2, "":U)
      oControl:ControlTooltip                   = "Please enter a valid user(s) or enter None Allowed if no users are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcEndDateUpdUsersLkp":U         + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,   "":U,                                          "":U,           15, 2, "End Date Update Allowed (Users):")
      oControl:LookupWobFLA                     = "mimus":U
      oControl:LookupFields                     = "mim_user.user_obj":U
      oControl:LookupControls                   = "fcEndDateUpdUserObjs" + goCntMaint:ContainerCode
      oControl:FilterFields                     = "mim_user.user_obj":U
      oControl:FilterControls                   = "fcEndDateUpdUserObjs" + goCntMaint:ContainerCode
      oControl:CellLayoutMask                   = "&1&2&3"
      
      oControl                                  = goCntMaint:addControl("fcActMoouthpartId":U             + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_mouth_part_id":U,        "character":U,  16, 1, "Activate Mouth Part ID:":U) 
      
      oControl                                  = goCntMaint:addControl("fcEndDateUpdRoleObjs":U          + goCntMaint:ContainerCode  , "wsInput":U,        "20":U, "":U,                                           "character":U,  16, 2, "":U)
      oControl:ControlToken                     = "Hidden"                                                                                                                                                    
      oControl:CellLayoutMask                   = "&1&2&3"                                                                                                                                                    
      /* Displays [MULTIPLE SELECTED] on the 'End date roles' field after selecting multiple roles. */                                                                                                                                            
      oControl:JavaScriptOnChange               = 'if(this.value!="")~{fnGetControls("fcEndDateUpdRoles'  + goCntMaint:ContainerCode + '")[0].value="~[MULTIPLE SELECTED~]"~}'                                
      oControl                                  = goCntMaint:addControl("fcEndDateUpdRoles":U             + goCntMaint:ContainerCode  , "wsTextArea":U,       "10":U, "tt_auth_type.end_date_upd_role":U,           "character":U,  16,2,  "":U)
      oControl:ControlTooltip                   = "Please enter a valid role(s) or enter None Allowed if no roles are allowed.":U 
      oControl                                  = goCntMaint:addControl("fcEndDateUpdRolesLkp":U          + goCntMaint:ContainerCode  , "wsMsComboButton":U,  "":U,   "":U,                                         "":U,           16,2,  "End Date Update Allowed (Roles):")
      oControl:LookupWobFLA                     = "mimro":U
      oControl:LookupFields                     = "mim_role.role_obj":U
      oControl:LookupControls                   = "fcEndDateUpdRoleObjs" + goCntMaint:ContainerCode
      oControl:FilterFields                     = "mim_role.role_obj":U
      oControl:FilterControls                   = "fcEndDateUpdRoleObjs" + goCntMaint:ContainerCode 
      
      oControl                                  = goCntMaint:addControl("fcActivateEpisodeNum":U          + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_episode_number":U,       "character":U,  17, 1, "Activate Episode Number:":U)
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "ActivateEpisode":U
      
      oControl                                  = goCntMaint:addControl("fcActivateDueDate":U             + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_due_date":U,             "character":U,  18, 1, "Activate Due Date:":U) 
      oControl                                  = goCntMaint:addControl("fcActivateServiceType":U         + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_service_type":U,         "character":U,  19, 1, "Activate Service Type:":U) 
      oControl                                  = goCntMaint:addControl("fcActivateLos":U                 + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_los":U,                  "character":U,  20, 1, "Activate LOS:":U) 
      oControl:JavaScriptOnChange               = SUBSTITUTE('fnEnableActivateWeekend(this.checked,"&1");' , "fcActivateLosWeekendPass":U + goCntMaint:ContainerCode )
      
      oControl                                  = goCntMaint:addControl("fcActivateLosWeekendPass":U      + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_los_weekend_pass":U,     "character":U,  21, 1, "Activate LOS Weekend Pass:":U) 
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "ActivateWeekendPass":U
      oControl                                  = goCntMaint:addControl("fcActivateAmPm":U                + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_am_pm":U,                "character":U,  22, 1, "Activate AM&PM:":U) 
      oControl:JavaScriptOnChange               = SUBSTITUTE('fnEnableActivatePenalty(this.checked,"&1");' , "flActivatePenalty":U + goCntMaint:ContainerCode )
      
      
      oControl                                  = goCntMaint:addControl("flActivateCrosswalk":U           + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_crosswalk":U,            "logical":U  ,  23, 1, "Activate Crosswalk:":U) 
      oControl:ControlTooltip                   = "Please check the box to enable crosswalk for this auth type.There is a rule 'CrosswalkEnabled' which can be set up to indicate whether the Crosswalks should be enable for the Authorisation module,if this rule is set to TRUE, and this box is checked ,then the crosswalk will be activated in this auth type.":U 
      
      oControl                                  = goCntMaint:addControl("flActivateCodeLink":U           + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_code_link":U,             "logical":U  ,  24, 1, "Activate Code Link:":U) 
      oControl:ControlTooltip                   = "Please check the box to enable code link for this auth type.There is a rule 'CrosswalkEnabled' which can be set up to indicate whether the Crosswalks should be enable for the Authorisation module,if this rule is set to TRUE, and this box is checked ,then the code link will be activated in this auth type.":U 

      oControl                                  = goCntMaint:addControl("flActivateCopayment":U           + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_copayment":U,            "logical":U  ,  25, 1, "Activate Co-payment:":U) 
      
      oControl                                  = goCntMaint:addControl("flActivatePenalty":U             + goCntMaint:ContainerCode  , "wsCheckBox":U,     "20":U, "tt_auth_type.activate_penalty":U,              "logical":U  ,  26, 1, "Activate Penalty:":U) 
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "ActivatePenalty":U
      
      oControl                                  = goCntMaint:addControl("fcHealthOption":U                + goCntMaint:ContainerCode  , "wsCombo":U,        "10":U, "":U,                                           "character":U,  27,1, "Dependant Health Categories" + ":":U)
      oControl:AdditionalItems                  = SUBSTITUTE("&1=RULE|&2=NONE|&3=ALL|&4=CUSTOM":U, "System Default", "None", "All", "Custom Selection")
      oControl:JavaScriptOnChange               = SUBSTITUTE('fnEnableActivateHealth("&1", this.value);':U, "fcActivateHealth":U + goCntMaint:ContainerCode)
      
      oControl                                  = goCntMaint:addControl("fcActivateHealth":U              + goCntMaint:ContainerCode  , "wsSelect":U,       "10":U, "tt_auth_type.activate_health":U,               "character":U,  28,1, "Selected Health Categories" + ":":U)
      oControl:ControlTooltip                   = "Please select health containers to be displayed in this authorisation type."
      oControl:RenderArgument                   = "CategorySelect:ma_acDepHealthCat::FALSE":U
      oControl:RenderProcedure                  = "RenderProcedure":U
      oControl:SelectMultiple                   = TRUE
      oControl:LabelClass                       = "+clTop":U
      
      oControl                                  = goCntMaint:addControl("fcMntAuthTypeCorres":U           + goCntMaint:ContainerCode  , "wsHRef":U,         "20":U, "tt_auth_type.auth_type_obj":U,                 "character":U,  29, 1, "Authorisation Correspondence:":U)
      oControl:RenderProcedure                  = "customRenderProcedure":U 
      oControl:RenderArgument                   = "Communications":U
      .

  IF gdAuthTypeProviderObj <> 0.00 OR goWob:SubmitValue = "MaintSubmit" 
  OR CAN-FIND(FIRST tt_auth_type_error NO-LOCK WHERE tt_auth_type_error.owning_entity_mnemonic = "hactd")
  THEN
    ASSIGN  
      oControl                                  = goCntMaint:addControl("fcMntAuthProviderTitle":U           + goCntMaint:ContainerCode  , "wsHRef":U,         "20":U, "":U,                 "character":U,  30, 1, "<br><br><strong><u>Auth Provider Information</u></strong>":U)

      oControl                                  = goCntMaint:addControl("fdProviderObjHeader":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.auth_type_provider_obj":U,                 "decimal":U,  31, 1, "":U)
      oControl:ControlToken                     = "Hidden":U

      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderOptionCode":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.option_code":U,                 "character":U,  32, 1, "Option Code:":U)
      oControl:ControlToken                     = "ReadOnly":U
    
      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderInsurer":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.insurer_obj":U,                 "character":U,  33, 1, "Insurer:":U)
      oControl:QueryString                      = "FOR EACH erm_insurer NO-LOCK ":U
      oControl:KeyField                         = "erm_insurer.insurer_obj":U
      oControl:DisplayFields                    = "erm_insurer.insurer_name":U 
      oControl:ControlToken                     = "ReadOnly":U
      
      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderProviderTypeIndicator":U  + goCntMaint:ContainerCode  , "wsInput":U,         "30":U, "tt_auth_type_provider.provider_type_indicator":U,                 "character":U,  34, 1, "Provider Type Indicator:":U)
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "GetAcronym":U
      oControl:ControlToken                     = "ReadOnly":U
      
      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderProviderType":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.provider_type":U,                 "character":U,  35, 1, "Provider Type:":U)
      oControl:QueryString                      = "FOR EACH mic_acronym NO-LOCK":U    
                                                + "   WHERE mic_acronym.category_key = 'ma_acAuthProviderType'":U
                                                + "      BY mic_acronym.acronym_label":U 
      oControl:KeyField                         = "mic_acronym.acronym_key":U
      oControl:DisplayFields                    = "mic_acronym.acronym_label":U
      oControl:ControlToken                     = "ReadOnly":U
      
      
      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderProviderSeq":U             + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.provider_sequence":U,                 "character":U,  36, 1, "Provider Sequence:":U)
      oControl:ControlToken                     = "ReadOnly":U 
      
      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderAuthorisedService":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.authorised_service":U,                 "character":U,  37, 1, "Authorised Service:":U)
      oControl:ControlToken                     = "ReadOnly":U 

      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderNegotiationGroup":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.neg_num":U,                 "character":U,  38, 1, "Negotiation Group:":U)
      oControl:ControlToken                     = "ReadOnly":U

      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderValidPracticeType":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.pr_type_valid_list":U,                 "character":U,  39, 1, "Valid Practice Type:":U)
      oControl:ControlToken                     = "ReadOnly":U 

      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderEffectiveDate":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.effective_date":U,                 "character":U,  40, 1, "Effective Date:":U)
      oControl:ControlToken                     = "ReadOnly":U 
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "FormatEffectiveDates":U

      oControl                                  = goCntMaint:addControl("fcMntAuthTypeProviderEndeDate":U           + goCntMaint:ContainerCode  , "wsInput":U,         "20":U, "tt_auth_type_provider.end_date":U,                 "character":U,  41, 1, "End Date:":U)
      oControl:ControlToken                     = "ReadOnly":U 
      oControl:RenderProcedure                  = "customRenderProcedure":U
      oControl:RenderArgument                   = "FormatEndDates":U
      . 

    ASSIGN
      goCntAudit                                = mipEnv:Health:maUtility:getAuditContainer()
      goCntAudit:RowsToRender                   = ?
      .
      
    IF goWob:SubmitValue <> "ADD" THEN
    DO:
      RUN defineAuthTypeDetail   IN TARGET-PROCEDURE.
      RUN defineAuthTypeControl  IN TARGET-PROCEDURE.
      RUN defineAuthTypeProvider IN TARGET-PROCEDURE.
      
&IF {&DBDFMA} >= 10195 &THEN
    
      ASSIGN
        goCntQuestionnaire                = mipEnv:Health:maUiService:getCntUpdQuestionnaire("QuestionnaireContainer":U)
        goCntQuestionnaire:ContainerWidth = IF gdAuthTypeProviderObj <> 0.00 OR goWob:SubmitValue = "MaintSubmit":U THEN goCntAuthTypeDetail:ContainerWidth ELSE goCntTypeControl:ContainerWidth
        goCntQuestionnaire:Collapsed      = TRUE.
&ENDIF
    END.  /* IF Warpspeed:SubmitValue <> "ADD" THEN */ 
          

  END.  /* IF goWob:Mode = "Maint":U */
  
  { mip/inc/mipcatcherror.i }
