/* maauthdetailgetcntupdnappidetail.i  MEDSTAR Medical Aid System
                                       Auth Detail Multiple Nappi Container 
                                       maauthdetailuiservicestack.p -> getCntUpdNappiDetail
                                       (c) Copyright 2021 - 2022
                                       MIP Holdings (Pty) Ltd
                                       All rights reserved
*/ 
    
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oHeaderContainer     AS cls.mipwscontainer        NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oWob                 AS cls.mipwswob              NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE oButtonContainer     AS cls.mipwscontainer        NO-UNDO.
  
  ASSIGN    
    oWob                                  = Warpspeed:CurrentObject                                                                                                                          

    opoContainer                          = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)                                       
    opoContainer:ContainerTitle           = "Clinical Details":U
    opoContainer:TitleLayoutMask          = "<div id='NappiTitle' > &1 </div>":U
    opoContainer:ViewOnly                 = FALSE
    opoContainer:NoDataMessage            = "Please specify clinical information in the empty line provided above"
    opoContainer:ShowContainerSettings    = FALSE
    opoContainer:ContainerMode            = Warpspeed:SubmitValue
    
    opoContainer:DefaultContainerType     = "TABLE":U                                   
    opoContainer:RowsToRender             = ?
    opoContainer:RowRenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle       
    opoContainer:RowRenderProcedure       = "RowRenderProcedure":U
    opoContainer:RowRenderArgument        = "DetailNappiContainer":U
    
    oControl                              = opoContainer:addControl("fiLineNumber":U               + ipcContainername, "wsInput":U       , "25":U, "":U                                     , "INTEGER":U  , 1, "#")
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument               = "LineNumber":U                                         
    oControl:ControlToken                 = "Hidden":U                                             
                                                                                                                                
    oControl                              = opoContainer:addControl("fdAuthDetailObjArgument":U    + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 4, "")
    oControl:ControlClass                 = "+clObjControl":U 
    oControl:ControlToken                 = "Hidden":U
     
    oControl                              = opoContainer:addControl("fdAuthDetailNappiObj":U       + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.auth_detail_obj":U       , "CHARACTER":U, 5, "")
    oControl:ControlClass                 = "+clObjControl":U 
    oControl:ControlToken                 = "Hidden":U
     
     oControl                              = opoContainer:addControl("cbProvider":U                + ipcContainerName, "wsCombo":U       , "5":U , "tt_auth_detail.auth_provider_obj":U     , "CHARACTER":U, 6, "Provider")
     oControl:ControlToken                 = "Hidden":U
     
     
    /* Enable sequence field when a detail line is successfully created                         */
    /* Disable primary fields once a detail record has been successfully created                */
    /* Enable edit button which launches detail form when a detail line is successfully created */
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailObj(this);":U
                                          + "fnOnChangeEnableUpdateButton(this, ~"buUpdate" + ipcContainerName + "~");":U                                                                                                                        

    oControl                              = opoContainer:addControl("cbOEM":U                      + ipcContainerName, "wsCombo":U       , "5":U , "tt_auth_detail.owning_entity_mnemonic":U, "CHARACTER":U, 9, "Detail<br>Entity")
    oControl:AdditionalItems              = "Nappi=hlmnl":U                            
    oControl:ControlToken                 = "Hidden":U    
                                     
    oControl                              = opoContainer:addControl("_fdOwningObjArgument":U        + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail.owning_obj":U            , "CHARACTER":U, 10, "")
    oControl:ControlToken                 = "Hidden":U    
                                         
    /* Using this field for nappi's where we typically use nappi link obj, but if we want to view details we */
    /* will launch the nappi wob and need the relevant nappi obj instead of the nappi link obj (AD)          */                                                                                              
    oControl                              = opoContainer:addControl("_fdOwningAltObjArgument":U     + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 10, "")
    oControl:ControlToken                 = "Hidden":U                                        
                                                                                                   
    oControl                              = opoContainer:addControl("frOwningRowid":U              + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail._row_id":U, "CHARACTER":U, 10, "")
    oControl:ControlToken                 = "Hidden":U                                             
   
    /* This will be disabled on update - See AlternateKeyFields below */                                                                                                                                    
    oControl                              = opoContainer:addControl("_fcOwningAltValueArgument":U   + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.owning_alt_value":U      , "CHARACTER":U, 10, "")         
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1&2&3&4":U
    
    oControl                              = opoContainer:addControl("cbREM":U                      + ipcContainerName, "wsCombo":U       , "5":U , "tt_auth_detail.related_entity_mnemonic":U, "CHARACTER":U, 14, "Related<br>Entity")
    oControl:AdditionalItems              = "=":U  
    oControl:QueryString                  = "FOR EACH mic_acronym NO-LOCK":U
                                          + "  WHERE mic_acronym.category_key = 'ma_acAuthDetailRelated'":U
                                          + "     BY mic_acronym.acronym_label":U
    oControl:KeyField                     = "mic_acronym.acronym_value":U
    oControl:DisplayFields                = "mic_acronym.acronym_label":U
    oControl:ControlClass                 = "+clMan":U							
    oControl:ControlToolTip               = "Select Nappi or Basket" 
    oControl:JavascriptOnChange           = "fnSetQtyAuth(this,true)":U.

  ASSIGN
    oControl                              = opoContainer:addControl("fdRelatedObj":U                + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail.related_obj":U            , "CHARACTER":U, 15, "")
    oControl:ControlToken                 = "Hidden":U    
                                         
    /* Using this field for nappi's where we typically use nappi link obj, but if we want to view details we */
    /* will launch the nappi wob and need the relevant nappi obj instead of the nappi link obj (AD)          */                                                                                              
    oControl                              = opoContainer:addControl("fdRelatedAltObj":U             + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 15, "")
    oControl:ControlToken                 = "Hidden":U                                        
                                                                                                   
    oControl                              = opoContainer:addControl("frRelatedRowid":U              + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail._row_id":U, "CHARACTER":U, 15, "")
    oControl:ControlToken                 = "Hidden":U                                             
   
    /* This will be disabled on update - See AlternateKeyFields below */                                                                                                                                    
    oControl                              = opoContainer:addControl("fcRelatedAltValue":U           + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.related_value":U      , "CHARACTER":U, 15, "")        
    oControl:ControlClass                 = "+clMan":U  
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:"
    oControl:FilterFields                 = "[OwningEntityMnemonic],[OwningAltValue],[StartDate]":U  
    oControl:FilterControls               = "cbREM":U + ipcContainerName + ",fcRelatedAltValue":U + ipcContainerName + ",_fdStartDateArgument":U  + ipcContainerName 
    oControl:ReturnFields                 = "[RecordObj],[RecordDesc]":U                                                                                 
    oControl:ReturnControls               = "fdRelatedObj":U  + ipcContainerName + ",":U + "fcRelatedDescr":U  + ipcContainerName 
    
    oControl                              = opoContainer:addControl("ftTariffEffecDate":U          + ipcContainerName, "wsInput":U       , "8":U , "":U                                     , "DATE":U     , 15, "")
    oControl:ControlToken                 = "HIDDEN":U 
    
    /* This will be disabled on update - See AlternateKeyFields below */                                                               
    oControl                              = opoContainer:addControl("buRelatedBtn":U               + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 15, "Related<br>Item Code")
    oControl:CellLayoutMask               = "&1&2&3&4&5&6":U    
    oControl:CellClass                    = "+clNoW":U       
    oControl:LookupWobFLA                 = "slent":U
    oControl:LookupFields                 = "CODE_FIELD":U
    oControl:LookupControls               =  "fcRelatedAltValue":U + ipcContainerName 
    oControl:FilterControls               = "cbREM":U + ipcContainerName
    oControl:FilterFields                 = "QUERY_OEM":U
    oControl:ReturnFields                 = "CODE_FIELD,DESC_FIELD":U
    oControl:ReturnControls               = "fcRelatedAltValue":U + ipcContainerName + ",fcRelatedDesc":U + ipcContainerName
                                    
    oControl                              = opoContainer:addControl("fcRelatedDesc":U               + ipcContainerName, "wsTextArea":U    , "8,3":U, "":U , "CHARACTER":U, 16, "")
    oControl:ControlToken                 = "Disabled":U                                          
                                                                                                
    /* This field is required to keep a "template" of the view more detail link which will be used to dynamically    */
    /* change the view detail link onclick and href attributes depending on the owning entity and item code selected.*/  
    oControl                              = opoContainer:addControl("fcLinkTemplate":U             + ipcContainerName, "wsHref":U        , "10":U, "":U                                     , "CHARACTER":U, 18, "")    
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle       
    oControl:RenderProcedure              = "RenderProcedure":U                                   
    oControl:RenderArgument               = "AuthDetailOwningInfo":U                              
                                                                                                                                                                                              
    /* This will be disabled on update - See AlternateKeyFields below */                                                                                                                                      
    oControl                              = opoContainer:addControl("_fdStartDateArgument":U       + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail.start_date":U               , "DATE":U     , 21, "Start Date")  
    oControl:ControlClass                 = "+clMan +clRetainValue +clNow":U  
   // oControl:JavascriptOnBlur             = "&1":U
    oControl:CellClass                    = "+clNoW":U

    oControl                              = opoContainer:addControl("_cbStartAmPmArgument":U       + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_detail.start_ampm":U               , "LOGICAL":U  , 21, "Start Date")                                                                                                                                                                                                                     
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                       
    oControl:ControlClass                 = "+clSelVertAlign +clNoW":U                                   
   // oControl:JavascriptOnBlur             = "&1":U     
    oControl:CellClass                    = "+clNoW":U    
                                                  
    oControl                              = opoContainer:addControl("flStartDateAmPmUpdated":U     + ipcContainerName, "wsCheckBox":U    , "15":U , "tt_auth_detail._start_date_ampm_updated":U , "CHARACTER":U, 21, "Start Date":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1 &2&3":U
    oControl:CellClass                    = "+clNoW":U
    
    oControl                              = opoContainer:addControl("_fdEndDateArgument":U         + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail.end_date":U                 , "DATE":U     , 22, "End Date")  
    //oControl:JavascriptOnBlur             = "&1":U                                               
    oControl:CellClass                    = "+clNoW":U                                             
    oControl                              = opoContainer:addControl("_cbEndAmPmArgument":U         + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_detail.end_ampm":U                 , "LOGICAL":U  , 22, "End Date")                                                                                                                                                                                                                     
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                       
    oControl:ControlClass                 = "+clSelVertAlign +clNow":U
    oControl:CellClass                    = "+clNoW":U
    //oControl:JavascriptOnBlur             = "&1":U                                                                                                                                       
    
    oControl                              = opoContainer:addControl("flEndDateAmPmUpdated":U       + ipcContainerName, "wsCheckBox":U   , "15":U , "tt_auth_detail._end_date_ampm_updated":U   , "CHARACTER":U, 22, "End Date":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellClass                    = "+clNoW":U
    oControl:CellLayoutMask               = "&1 &2&3":U.
    

  ASSIGN 
    oControl                              = opoContainer:addControl("fdItemCost":U                 + ipcContainerName, "wsInput":U       , "3":U , "":U                                     , "INTEGER":U  , 23, "Item<br>Cost")
    oControl:ControlClass                 = "+clNumericOnly":U  
    oControl:ControlToken                 = "ReadOnly":U     
    
    oControl                              = opoContainer:addControl("fdQuantityRequested":U        + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.quantity_requested":U    , "INTEGER":U  , 24, "Qty<br>Req")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              

    oControl                              = opoContainer:addControl("fdQuantityAuth":U             + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.quantity_auth":U         , "INTEGER":U  , 25, "Qty<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              

    oControl                              = opoContainer:addControl("fdAmountRequested":U          + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_requested":U      , "DECIMAL":U  , 26, "Amount<br>Req")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              

    oControl                              = opoContainer:addControl("fdAmountAuth":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_auth":U           , "DECIMAL":U  , 27, "Amount<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                                              
    oControl                              = opoContainer:addControl("fdAmountPaid":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_paid":U           , "DECIMAL":U  , 28, "Amount<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
  
    oControl                              = opoContainer:addControl("fiClaimCode":U                + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.claim_code":U            , "INTEGER":U  , 29, "Claim<br>Code")                                                                                                                               
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U                                                                                                                              
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                 
    oControl:RenderArgument               = "ClaimCode":U
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                                      
                                                                                                                                                                                            
    oControl                              = opoContainer:addControl("buClaimBtn":U                 + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 29, "Claim<br>Code")
    oControl:CellLayoutMask               = "&1&2":U       
    oControl:CellClass                    = "+clNoW":U       
    oControl:LookupWobFLA                 = "maclc":U                                                                                                                                                      
    oControl:LookupFields                 = "ccdesc.claim-code":U                                                                                                                                          
    oControl:LookupControls               = "fiClaimCode":U + ipcContainerName                                                                                                                             
    oControl:FilterFields                 = "ccdesc.claim-code":U                                                                                                                                          
    oControl:FilterControls               = "fiClaimCode":U + ipcContainerName                                                                                                        
    oControl:ReturnFields                 = "ccdesc.claim-code":U                                                                                                                                          
    oControl:ReturnControls               = "fiClaimCode":U + ipcContainerName                                                                                                                             
                                                                                                                                                                                      
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                                      
    oControl                              = opoContainer:addControl("flClaimCodeUpdated":U         + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._claim_code_updated":U  , "CHARACTER":U, 29, "Claim<br>Code":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1&2&3":U

    oControl                              = opoContainer:addControl("cbClaimType":U                + ipcContainerName, "wsCombo":U       , "3":U , "tt_auth_detail.claim_type":U            , "CHARACTER":U, 30, "Claim<br>Type")
    oControl:AdditionalItems              = "|C=C|N=N|A=A|K=K|P=P|O=O":U                                                                                                                                               
    oControl:ControlToolTip               = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther"
    oControl:JavascriptOnChange           = "fnClaimTypeUpdated(this);"
    
    oControl                              = opoContainer:addControl("flClaimTypeUpdated":U         + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._claim_type_updated":U   , "CHARACTER":U, 30, "Claim<br>Type":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1&2":U                                                                                                                                                                                            
    
    oControl                              = opoContainer:addControl("cbDiscountType":U             + ipcContainerName, "wsCombo":U       , "3":U , "tt_auth_detail.discount_type":U         , "CHARACTER":U, 33, "Discount<br>Type")
    oControl:AdditionalItems              = "<None>=?|Percentage=YES|Rand=NO":U

    oControl                              = opoContainer:addControl("fdDiscountAuth":U             + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.discount_auth":U         , "DECIMAL":U  , 34, "Discount<br>Authorised")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                                               
    oControl                              = opoContainer:addControl("fdDiscountPaid":U             + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.discount_paid":U         , "DECIMAL":U  , 35, "Discount<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                            
    oControl                              = opoContainer:addControl("fdCopayAuth":U                + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_auth":U            , "DECIMAL":U  , 36, "Co-pay Amt<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
    oControl:ControlToken                 = "Readonly":U

    oControl                              = opoContainer:addControl("fdCopayPercentage":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_auth_%":U          , "DECIMAL":U  , 37, "Co-pay %<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
    oControl:ControlToken                 = "Readonly":U

    oControl                              = opoContainer:addControl("fdCopayPaid":U                + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_paid":U            , "DECIMAL":U  , 38, "Co-pay<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
    oControl:ControlToken                 = "Readonly":U

    oControl                              = opoContainer:addControl("cbStatus":U                   + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_detail.auth_status":U           , "INTEGER":U  , 39, "Status")
    oControl:ControlTooltip               = "Select a status from the drop-down list.":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthStatusCombo":U
    oControl:ControlClass                 = "+clMan +clRetainValue":U                                                                                                                                
    oControl:JavaScriptOnChange           = "fnOnChangeAuthDetailStatus(this, ~"tbl~", ~"" + ipcContainerName + "~" );":U                                                            
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationCoding:Status":U    
    oControl:FilterFields                 = "[Status],[AuthObj]":U                                                                                               
    oControl:FilterControls               = "cbStatus":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName                                    
    oControl:ReturnFields                 = "[StatusReasonMandatory]":U                           
    oControl:ReturnControls               = "flStatusReasonMandatory":U + ipcContainerName

    oControl                              = opoContainer:addControl("flStatusReasonMandatory":U    + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 39, "Status":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                                         
    oControl:CellLayoutMask               = "&1&2":U

    /* On status change, check whether reason is mandatory or not */ 
    oControl:JavaScriptOnChange           = "fnOnChangeAuthDetailStatus(this);":U
                                          + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"TBL~");":U 
 
    /* See RowRenderProcedure - Checks were moved to RowRenderProcedure for performance reasons */
    /* Check whether each reason control is mandatory or not depending on status                */                                  
    oControl                              = opoContainer:addControl("fcReason":U                   + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.auth_status_note":U    , "CHARACTER":U, 40, "Status<br>Reason":U)            
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:Reason":U                                                                                        
    oControl:FilterFields                 = "[ReasonCode],[ReasonType],[Status]":U                                                                                                             
    oControl:FilterControls               = "fcReason":U + ipcContainerName + ",":U
                                          + "fcReasonTypeArgument":U + ipcContainerName + ",":U
                                          + "cbStatus":U + ipcContainerName
    oControl:ReturnFields                 = "[ReasonDesc],[ReasonType]":U                                                                
    oControl:ReturnControls               = "fcReasonDesc":U + ipcContainerName + ",":U
                                          + "fcReasonTypeArgument":U + ipcContainerName
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthStatusReason":U
                                                                                                                                                                                      
    oControl                              = opoContainer:addControl("fcReasonTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 40, "Status<br>Reason":U)                  
    oControl:ControlToken                 = "Hidden":U                                                                                                                                 
    oControl:ControlValue                 = "AS":U
    oControl:ControlClass                 = "+clPreserveValue":U                                                                                                                                      
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthReasonType":U

    oControl                              = opoContainer:addControl("fcReasonDesc":U         + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 40, "Status<br>Reason":U)            
    oControl:ControlToken                 = "Hidden":U                   

    /* Set reason description as tooltip when reason is selected client side */ 
    oControl:JavascriptOnChange          = "fnSetReasonDescription(this, ~"fcReason":U + ipcContainerName + "~", ~"buReasonBtn":U + ipcContainerName + "~", ~"TBL~");":U              
                                         
                                                                                                                                                                                             
    oControl                              = opoContainer:addControl("buReasonBtn":U          + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 40, "Status<br>Reason":U)                                                                                                                                                                                                   
    oControl:CellLayoutMask               = "&1&2&3&4":U              
    oControl:CellClass                    = "+clNoW":U       
    oControl:LookupWobFLA                 = "note":U                                                                                                                                                     
    oControl:LookupFields                 = "note.key":U                                                                                                                                                 
    oControl:LookupControls               = "fcReason":U + ipcContainerName                                                                                                                              
    oControl:FilterFields                 = "note.key,note.type":U                                                                                                                                       
    oControl:FilterControls               = "fcReason":U + ipcContainerName + ",":U
                                          + "fcReasonTypeArgument":U + ipcContainerName                                                                                     
    oControl:ReturnFields                 = "note.key":U                                                                                                                                           
    oControl:ReturnControls               = "fcReason":U + ipcContainerName            

    oControl                              = opoContainer:addControl("fcCopayOverrideNote":U        + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_detail.copay_override_note":U , "INTEGER":U  , 41, "":U)
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:OverrideNote":U                                                                                          
    oControl:FilterFields                 = "[ReasonCode],[ReasonType]":U 
    oControl:FilterControls               = "fcCopayOverrideNote":U + ipcContainerName + ",":U 
                                          + "fcOverrideNoteTypeArgument":U + ipcContainerName 
    oControl:ReturnFields                 = "[ReasonDesc]":U                                                                                                                  
    oControl:ReturnControls               = "fcOverrideNoteDesc":U + ipcContainerName 
    
    oControl                              = opoContainer:addControl("fcOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 41, "":U)                  
    oControl:ControlToken                 = "Hidden":U
    oControl:ControlValue                 = "AQ":U
    oControl:ControlClass                 = "+clPreserveValue":U
    
    oControl                              = opoContainer:addControl("fcOverrideNoteDesc":U         + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 41, "":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                     
                                                                                                                                                                                           
    /* Set copay override note description as tooltip when note is selected */ 
    oControl:JavascriptOnChange           = "fnSetReasonDescription(this, ~"fcCopayOverrideNote":U + ipcContainerName + "~", ~"buOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U  
    
    oControl                              = opoContainer:addControl("buOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 41, "Co-payment<br>Override<br> Reason":U)
    oControl:ControlTooltip               = "Specify reason why co-payment should not apply.":U
    oControl:CellLayoutMask               = "&1&2&3&4":U                                                                                                                                                      
    oControl:CellClass                    = "+clNoW":U  
    oControl:LookupWobFLA                 = "note":U                                                                                                                                                    
    oControl:LookupFields                 = "note.key":U                                                                                                                                                
    oControl:LookupControls               = "fcCopayOverrideNote":U + ipcContainerName                                                                                                                             
    oControl:FilterFields                 = "note.key,note.type":U                                                                                                                                      
    oControl:FilterControls               = "fcCopayOverrideNote":U + ipcContainerName + ",":U
                                          + "fcOverrideNoteTypeArgument":U + ipcContainerName                                                                                      
    oControl:ReturnFields                 = "note.key":U                                                                                                                                           
    oControl:ReturnControls               = "fcCopayOverrideNote":U + ipcContainerName   

    oControl                              = opoContainer:addControl("flRepeatItem":U               + ipcContainerName, "wsCheckBox":U    , "2":U , "tt_auth_detail.repeat_item":U              , "LOGICAL":U, 42, "Repeat<br>Item":U)
    oControl:ControlTooltip               = "Indicate whether this item code is a Repeat.":U
    oControl:JavaScriptOnChange           = "fnOnChangeRepeatItem(this);":U

    oControl                              = opoContainer:addControl("fiRepeatCycleAuth":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_auth":U        , "INTEGER":U, 43, "Repeat<br>Cycle<br>Auth":U)
    oControl:ControlTooltip               = "Indicates the authorised number of repeats for the prescription.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fiRepeatCyclePaid":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_paid":U        , "INTEGER":U, 44, "Repeat<br>Cycle<br>Paid":U)
    oControl:ControlTooltip               = "Indicates the authorised number of repeats that have been claimed.":U
    oControl:ControlToken                 = "Disabled":U

    oControl                              = opoContainer:addControl("fiRepeatCycleQuantity":U      + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_quantity":U    , "INTEGER":U, 45, "Repeat<br>Cycle<br>Qty":U)
    oControl:ControlTooltip               = "Indicates the quantity that will be repeated.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fiRepeatCycleDays":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_days":U        , "INTEGER":U, 46, "Repeat<br>Cycle<br>Days":U)
    oControl:ControlTooltip               = "Indicates the number of days in one cycle and when a new script can be requested.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fiRepeatCycleGraceDays":U     + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_grace_days":U  , "INTEGER":U, 47, "Repeat<br>Cycle<br>Grace<br>Days":U)
    oControl:ControlTooltip               = "Indicates when the script can be requested earlier that the Repeat Cycle Days.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdRepeatLastClaimedDate":U    + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.repeat_last_claimed_date":U , "DATE":U,    48, "Repeat<br>Last<br>Claimed<br>Date":U)
    oControl:ControlTooltip               = "Indicates the last date that this item was claimed.":U
    oControl:ControlToken                 = "ReadOnly":U
    
    oControl                              = opoContainer:addControl("flPmbIndicator":U               + ipcContainerName, "wsCheckBox":U  , "12":U, "tt_auth_detail.pmb_indicator":U         , "LOGICAL":U  , 49, "PMB<br>Indicator":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                                        
                                                                                                     
    oControl                              = opoContainer:addControl("_authObjArgument":U             + ipcContainerName, "wsInput":U     , "8":U , "tt_auth_detail.auth_obj":U              , "DECIMAL":U  , 50, "":U)
    oControl:ControlToken                 = "Hidden":U                                               
                                                                                                     
    oControl                              = opoContainer:addControl("_oemArgument":U                 + ipcContainerName, "wsInput":U     , "8":U , "tt_auth_detail.owning_entity_mnemonic":U, "CHARACTER":U, 51, "":U)
    oControl:ControlToken                 = "Hidden":U                                               
                                                                                                     
    oControl                              = opoContainer:addControl("_configuration":U               + ipcContainerName, "wsInput":U     , "15":U, "tt_auth_detail._detail_configuration":U , "CHARACTER":U, 52, "_configuration:":U)
    oControl:ControlToken                 = "Hidden":U
    
    oControl                              = opoContainer:addControl("_AuthDetailIndexListArgument":U + ipcContainerName, "wsInput":U     , "15":U, "":U , "CHARACTER":U, 53, "":U)
    oControl:ControlToken                 = "Hidden":U

    oControl                              = opoContainer:addControl("_triggerRebuild":U              + ipcContainerName, "wsInput":U     , "8":U , "":U                                     , "CHARACTER":U, 54, "":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailRebuild(this);":U .

    ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(opoContainer)   
           oContainerProperties:AutoSaveOperation      = "SERVICE:maUIService:ajaxSaveMultipleNappiContainer:":U + ipcContainerName
           oContainerProperties:ShowMoreLess           = TRUE
           oContainerProperties:DefaultLess            = TRUE
           oContainerProperties:DisplayFilter          = FALSE 
           oContainerProperties:NavigationPane         = FALSE
           oContainerProperties:CollapsableControlList = "cbRelatedEntity":U        + ipcContainerName + ",":U + "fcRelatedCode":U           + ipcContainerName + ",":U                
                                                       + "buDetailRelatedBtn":U     + ipcContainerName + ",":U + "fcRelatedDescr":U          + ipcContainerName + ",":U 
                                                       + "fdPaid":U                 + ipcContainerName + ",":U + "fdCopayPaid":U             + ipcContainerName + ",":U 
                                                       + "buNoteBtn":U              + ipcContainerName + ",":U + "fdQuantityRequested":U     + ipcContainerName + ",":U 
                                                       + "fdAmountRequested":U      + ipcContainerName + ",":U + "cbDiscountType":U          + ipcContainerName + ",":U  
                                                       + "fdDiscountAuth":U         + ipcContainerName + ",":U + "fdDiscountPaid":U          + ipcContainerName + ",":U 
                                                       + "fdCopayAuth":U            + ipcContainerName + ",":U + "fdCopayPercentage":U       + ipcContainerName + ",":U 
                                                       + "fcCopayOverrideNote":U    + ipcContainerName + ",":U + "flRepeatItem":U            + ipcContainerName + ",":U 
                                                       + "fiRepeatCycleAuth":U      + ipcContainerName + ",":U + "fiRepeatCyclePaid":U       + ipcContainerName + ",":U 
                                                       + "fiRepeatCycleQuantity":U  + ipcContainerName + ",":U + "fiRepeatCycleDays":U       + ipcContainerName + ",":U 
                                                       + "fiRepeatCycleGraceDays":U + ipcContainerName + ",":U + "fdRepeatLastClaimedDate":U + ipcContainerName
                                                       .

  mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties). 
                              

  /*
    Add buttons to footer container
  */
  ASSIGN 
    opoContainer:FooterContainer                 = NEW cls.mipwscontainer(opoContainer:ContainerCode + "Footer":U)
    opoContainer:FooterContainer:Borders         = "FALSE":U
    opoContainer:FooterContainer:Collapsable     = FALSE
    opoContainer:FooterContainer:ViewOnly        = FALSE
    opoContainer:FooterContainer:ContainerWidth  = "100%":U
    
    oButtonContainer                             = wsUIService:getButtonContainer(opoContainer:ContainerCode + "ButtonBar":U, "Close":U)

    /* 
      Register - so we easily retrieve the object later 
    */
    lSuccess                                     = oWob:setContainer("NappiButtonContainer":U, oButtonContainer)
     
    oControl                                     = opoContainer:FooterContainer:addControl("ButtonBar":U + opoContainer:ContainerCode, "":U, "":U, "":U, "":U, 1, 1, "":U)
    oControl:SubContainer                        = oButtonContainer
    oControl:SpanOverLabel                       = TRUE
    oControl:CellClass                           = WarpSpeed:BaseClass + "ButtonBar":U
    oControl:CellSnippet                         = "align='right'":U
    oControl:ColumnSpan                          = 2
    
    oControl                                     = oButtonContainer:getControl("btnClose":U) 
    oControl:JavascriptOnClick                   = "fnCloseAuthNappi();":U

                                                 /* Please do not copy this button                                    */
                                                 /* The only reason we are creating custom buttons here is because    */
                                                 /* buttons break the document when a container is rebuilt on the fly */
    oControl:CellLayoutMask                      = SUBSTITUTE({&ContainerButton}, "clHcBtn clHcFtrBtn":U, oControl:JavascriptOnClick, oControl:ButtonLabel).                              .
   
   
