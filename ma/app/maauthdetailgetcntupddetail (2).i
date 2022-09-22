/* maauthdetailgetcntupddetail.i  MEDSTAR Medical Aid System
                                  Authorisation Detail Container Procedure ( getCntUpdDetail )
                                  (c) Copyright 1990 - 2019
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/
  
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
    
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oWob                 AS cls.mipwswob              NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
  
  
&IF {&DBDFMA} >= 10195 &THEN  

  ASSIGN    
    oWob                                  = Warpspeed:CurrentObject                                                                                                                          

    opoContainer                          = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, TRUE)                                       
    opoContainer:ContainerTitle           = "Clinical Details":U
    opoContainer:ViewOnly                 = FALSE
    opoContainer:NoDataMessage            = "Please specify clinical information in the empty line provided above"
    opoContainer:ShowContainerSettings    = FALSE
    opoContainer:ContainerMode            = Warpspeed:SubmitValue
    opoContainer:QueryString              = "FOR EACH tt_auth_detail NO-LOCK":U
                                          + ",  FIRST tt_auth_provider NO-LOCK":U
                                          + "   WHERE tt_auth_provider.auth_provider_obj = tt_auth_detail.auth_provider_obj OUTER-JOIN ":U
                                          + "      BY tt_auth_detail.line_number BY tt_auth_provider.provider_sequence BY tt_auth_detail.loc_sequence":U         
    opoContainer:DefaultContainerType     = "TABLE":U                                   
    opoContainer:RowsToRender             = ?   
    opoContainer:RowRenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle       
    opoContainer:RowRenderProcedure       = "RowRenderProcedure":U
    opoContainer:RowRenderArgument        = "AuthDetailContainer":U    
                                                                                                   
    oControl                              = opoContainer:addControl("fiLineNumber":U               + ipcContainername, "wsInput":U       , "25":U, "":U                                     , "INTEGER":U  , 1, "#")
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument               = "LineNumber":U                                         
    oControl:ControlToken                 = "Hidden":U                                             
                                                                                                                                
    oControl                              = opoContainer:addControl("buUpdate":U                   + ipcContainerName, "wsInput":U       , "":U  , "":U                                     , "CHARACTER":U, 2, "")
    oControl:ControlToolTip               = "Modify record":U                                                                                                                               
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                 
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                             
    oControl:RenderArgument               = "AuthDetailEditButton":U                               
                                                                                                                           
    oControl                              = opoContainer:addControl("fdAuthDetailObj":U            + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.auth_detail_obj":U       , "CHARACTER":U, 3, "")
    oControl:ControlClass                 = "+clObjControl +clHid":U 
    
    /* Enable sequence field when a detail line is successfully created                         */
    /* Disable primary fields once a detail record has been successfully created                */
    /* Enable edit button which launches detail form when a detail line is successfully created */
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailObj(this);":U
                                          + "fnOnChangeEnableUpdateButton(this, ~"buUpdate" + ipcContainerName + "~");":U                                                                                                                        

    /* This will be disabled on update - See AlternateKeyFields below */                                     
    oControl                              = opoContainer:addControl("cbProvider":U                 + ipcContainerName, "wsCombo":U       , "5":U , "tt_auth_detail.auth_provider_obj":U     , "CHARACTER":U, 4, "Provider")
    oControl:ControlClass                 = "+clMan":U          
    oControl:ControlTooltip               = "Provider":U
                                         
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:Detail":U
    oControl:FilterFields                 = "[AuthProviderObj]":U
    oControl:FilterControls               = "cbProvider":U + ipcContainerName
    oControl:ReturnFields                 = "[PrType],[SubPrType],[BaseRate],[ArsRate],[NegGroup],[LineRestriction]":U
    oControl:ReturnControls               = "fiDiscipline":U       + ipcContainerName + ",fiSubDiscipline":U   + ipcContainerName 
                                          + ",fcDefaultBaseRate":U + ipcContainerName + ",fcDefaultArsRate":U  + ipcContainerName
                                          + ",fcNegGroup":U        + ipcContainerName + ",cbLineRestriction":U + ipcContainerName

    oControl                              = opoContainer:addControl("fiDiscipline":U               + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_provider.pr_type":U             , "INTEGER":U  , 5, "Disc")                                                                                                            
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U /*Do not allow characters in this field and restrict to 3 digits*/  
    oControl:ControlFormat                = "999":U   
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlTooltip               = "Provider Discipline":U

    oControl                              = opoContainer:addControl("fiSubDiscipline":U            + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_provider.sub_pr_type":U         , "INTEGER":U  , 6, "Sub<br>Disc")                                  
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U /*Do not allow characters in this field and restrict to 3 digits*/
    oControl:ControlFormat                = "999":U                                                                                                                                   
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlTooltip               = "Provider Sub-Discipline"

    oControl                              = opoContainer:addControl("fcNegGroup":U                 + ipcContainerName, "wsInput":U       , "5":U, "":U                                      , "CHARACTER":U, 7, "Neg<br>Group")
    oControl:ControlToken                 = "ReadOnly":U
    
    oControl                              = opoContainer:addControl("fcDefaultBaseRate":U          + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.default_base_rate":U     , "CHARACTER":U, 8, "Default<br>Base<br>Rate")
    oControl:ControlToken                 = "ReadOnly":U                                                                                                                                                                                     
    oControl:ControlTooltip               = "Default Base Rate":U    
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                 
    oControl:RenderArgument               = "BaseRate":U
    oControl:RenderProcedure              = "RenderProcedure":U                                                                                                                                      
                                                                                                                                                                                                                                         
    oControl                              = opoContainer:addControl("fcDefaultArsRate":U           + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.default_ars_rate":U      , "CHARACTER":U, 9, "Default<br>ARS<br>Rate")
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlTooltip               = "Default Ars Rate":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle                                                                                                 
    oControl:RenderArgument               = "ArsRate":U
    oControl:RenderProcedure              = "RenderProcedure":U    
    
    /* Clear owning alt value on change of base rate if the base rate is blank                             */
    /* If the base rate is not blank trigger onchange event on the owning alt value field so that the ajax */
    /* will fire to return the new obj if the base rate changed and we are dealing with a tariff           */
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailBaseRate(this,~"cbOEM~",~"fcOwningAltValue~");":U
    
    /* Although this control is disabled for now, we may use in the future */
    oControl                              = opoContainer:addControl("cbTariffTypeCategory":U       + ipcContainerName, "wsCombo":U       , "10":U, "":U                                     , "CHARACTER":U, 10, "Tariff Type<br>Category")    
    oControl:RenderProcedure              = "RenderProcedure":U 
    oControl:RenderArgument               = "AcronymSelect:ma_acTariffTypeCat:=":U
    oControl:ControlToken                 = "Disabled":U  

    /* Although this control is disabled for now, we may use in the future */
    oControl                              = opoContainer:addControl("cbTariffType":U               + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_detail.tariff_type_obj":U       , "CHARACTER":U, 11, "Tariff<br>Type")    
    oControl:AdditionalItems              = "=":U
    oControl:QueryString                  = "FOR EACH htm_tariff_type NO-LOCK BY htm_tariff_type.tariff_type_code":U
    oControl:KeyField                     = "htm_tariff_type.tariff_type_obj":U
    oControl:DisplayFields                = "htm_tariff_type.tariff_type_code":U  
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailTariffType(this);":U  
    oControl:ControlToken                 = "Disabled":U

    /* This will be disabled on update - See AlternateKeyFields below */                                     
    oControl                              = opoContainer:addControl("cbOEM":U                      + ipcContainerName, "wsCombo":U       , "5":U , "tt_auth_detail.owning_entity_mnemonic":U, "CHARACTER":U, 12, "Detail<br>Entity")
    oControl:AdditionalItems              = "=":U
    oControl:QueryString                  = "FOR EACH mic_acronym NO-LOCK":U
                                          + "   WHERE mic_acronym.category_key = 'ma_acAuthDetailEntities'":U
                                          + "      BY mic_acronym.acronym_label":U
    oControl:KeyField                     = "mic_acronym.acronym_value":U
    oControl:DisplayFields                = "mic_acronym.acronym_label":U                                   
    oControl:ControlClass                 = "+clMan":U

    /* Clear item code on change of OEM                                                            */
    /* Rebuild view detail link on change of OEM as this will determine the WOB that should be run */
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailOwningEntity(this,~"fcOwningAltValue~");":U                                           
                                          + "fnSetAuthDetailLink(this,                ":U
                                          + "                   ~"cbOEM~",            ":U
                                          + "                   ~"fdOwningObj~",      ":U
                                          + "                   ~"fdOwningAltObj~",   ":U
                                          + "                   ~"fiDiscipline~",     ":U
                                          + "                   ~"fiSubDiscipline~",  ":U
                                          + "                   ~"fcDefaultBaseRate~",":U
                                          + "                   ~"fcDefaultArsRate~", ":U
                                          + "                   ~"ftTariffEffecDate~",":U
                                          + "                   ~"fcOwningAltValue~", ":U
                                          + "                   ~"fcDetails~",        ":U
                                          + "                   ~"fcLinkTemplate~");  ":U
    oControl:ControlToolTip               = "Select Basket,Nappi or Tariff"
                                         
    oControl                              = opoContainer:addControl("fdOwningObj":U                + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail.owning_obj":U            , "CHARACTER":U, 13, "")
    oControl:ControlToken                 = "Hidden":U    
    
    /* Rebuild view detail link when the owning obj value is changed */
    oControl:JavascriptOnChange           = "fnSetAuthDetailLink(this,                ":U
                                          + "                   ~"cbOEM~",            ":U
                                          + "                   ~"fdOwningObj~",      ":U
                                          + "                   ~"fdOwningAltObj~",   ":U
                                          + "                   ~"fiDiscipline~",     ":U
                                          + "                   ~"fiSubDiscipline~",  ":U
                                          + "                   ~"fcDefaultBaseRate~",":U
                                          + "                   ~"fcDefaultArsRate~", ":U
                                          + "                   ~"ftTariffEffecDate~",":U
                                          + "                   ~"fcOwningAltValue~", ":U
                                          + "                   ~"fcDetails~",        ":U
                                          + "                   ~"fcLinkTemplate~");  ":U
                                       
    /* Using this field for nappi's where we typically use nappi link obj, but if we want to view details we */
    /* will launch the nappi wob and need the relevant nappi obj instead of the nappi link obj (AD)          */                                                                                              
    oControl                              = opoContainer:addControl("fdOwningAltObj":U             + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 13, "")
    oControl:ControlToken                 = "Hidden":U                                        
                                                                                                   
    oControl                              = opoContainer:addControl("frOwningRowid":U              + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 13, "")
    oControl:ControlToken                 = "Hidden":U                                             
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle        
    oControl:RenderProcedure              = "RenderProcedure":U                                    
    oControl:RenderArgument               = "AuthDetailOwningRowid":U                              
    
    /* This will be disabled on update - See AlternateKeyFields below */                                                                                                                                    
    oControl                              = opoContainer:addControl("fcOwningAltValue":U           + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.owning_alt_value":U      , "CHARACTER":U, 13, "")        
    oControl:ControlClass                 = "+clMan":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:":U
    oControl:FilterFields                 = "[PrType],[SubPrType],[BaseRate],[ArsRate],[StartDate],[OwningEntityMnemonic],[OwningAltValue],[AuthProviderObj]":U
    oControl:FilterControls               = "fiDiscipline":U       + ipcContainerName + ",fiSubDiscipline":U  + ipcContainerName
                                          + ",fcDefaultBaseRate":U + ipcContainerName + ",fcDefaultArsRate":U + ipcContainerName
                                          + ",fdStartDate"         + ipcContainerName + ",cbOEM":U            + ipcContainerName 
                                          + ",fcOwningAltValue":U  + ipcContainerName + ",cbProvider":U       + ipcContainerName
    oControl:ReturnFields                 = "[RecordRowid],[RecordDesc],[TariffTypeCategory],[TariffTypeObj],[TariffCost],[TariffEffectiveDate],[RecordAltObj],[RecordObj]":U
    oControl:ReturnControls               = "frOwningRowid":U         + ipcContainerName + ",fcOwningDesc":U      + ipcContainerName 
                                          + ",cbTariffTypeCategory":U + ipcContainerName + ",cbTariffType":U      + ipcContainerName 
                                          + ",fdTariffCost":U         + ipcContainerName + ",ftTariffEffecDate":U + ipcContainerName 
                                          + ",fdOwningAltObj":U       + ipcContainerName + ",fdOwningObj":U       + ipcContainerName

    /* If the owning entity is a tariff, ensure we have padded to 5 before the ajax validation fires */                                   
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailOwningValue(this,~"cbOEM~");":U    
    
    oControl                              = opoContainer:addControl("ftTariffEffecDate":U          + ipcContainerName, "wsInput":U       , "8":U , "":U                                     , "DATE":U     , 13, "")
    oControl:ControlToken                 = "HIDDEN":U 
    
    /* This will be disabled on update - See AlternateKeyFields below */                                                               
    oControl                              = opoContainer:addControl("buOwningBtn":U                + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 13, "Detail<br>Item Code")
    oControl:CellLayoutMask               = "&1&2&3&4&5&6":U                                                                                                                                                   
    oControl:LookupWobFLA                 = "slent":U
    oControl:LookupFields                 = "ROWID,CODE_FIELD":U
    oControl:LookupControls               = "frOwningRowid":U + ipcContainerName + ",fcOwningAltValue":U + ipcContainerName 
    oControl:FilterControls               = "cbOEM":U + ipcContainerName
    oControl:FilterFields                 = "QUERY_OEM":U
    oControl:ReturnFields                 = "CODE_FIELD,DESC_FIELD":U
    oControl:ReturnControls               = "fcOwningAltValue":U + ipcContainerName + ",fcOwningDesc":U + ipcContainerName
                                     
    oControl                              = opoContainer:addControl("fcOwningDesc":U               + ipcContainerName, "wsTextArea":U    , "15,3":U, "":U                                   , "CHARACTER":U, 14, "")
    oControl:ControlToken                 = "Disabled":U                                          
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle       
    oControl:RenderProcedure              = "RenderProcedure":U                                   
    oControl:RenderArgument               = "AuthDetailOwningDescription":U                       
                                                                                                  
    oControl                              = opoContainer:addControl("fcDetails":U                  + ipcContainerName, "wsHref":U        , "10":U, "":U                                     , "CHARACTER":U, 15, "")    
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                                                    
    oControl:RenderProcedure              = "RenderProcedure":U                                   
    oControl:RenderArgument               = "AuthDetailOwningInfo":U                              

    /* This field is required to keep a "template" of the view more detail link which will be used to dynamically    */
    /* change the view detail link onclick and href attributes depending on the owning entity and item code selected.*/  
    oControl                              = opoContainer:addControl("fcLinkTemplate":U             + ipcContainerName, "wsHref":U        , "10":U, "":U                                     , "CHARACTER":U, 16, "")    
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle       
    oControl:RenderProcedure              = "RenderProcedure":U                                   
    oControl:RenderArgument               = "AuthDetailOwningInfo":U                              
    
    oControl                              = opoContainer:addControl("fiLocSequence":U              + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.loc_sequence":U             , "INTEGER":U  , 17, "LOC<br>Seq")  
                                                                                                                                                                                                
    oControl                              = opoContainer:addControl("fdQuantityLos":U              + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.quantity_los":U             , "INTEGER":U  , 18, "Qty<br>LOS")  
                                                                                                                                                                                               
    /* This will be disabled on update - See AlternateKeyFields below */                                                                                                                                      
    oControl                              = opoContainer:addControl("fdStartDate":U                + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.start_date":U               , "DATE":U     , 19, "Start Date")  
    oControl:ControlClass                 = "+clMan +clRetainValue":U                                                                                                                                         
                                                                                                                                                                                               
    oControl                              = opoContainer:addControl("cbStartAmPm":U                + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_detail.start_ampm":U               , "LOGICAL":U  , 19, "Start Date")                                                                                                                                                                                                                     
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                       
    oControl:ControlClass                 = "+clSelVertAlign":U                                   
    
    oControl                              = opoContainer:addControl("flStartDateAmPmUpdated":U     + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._start_date_ampm_updated":U , "CHARACTER":U, 19, "Start Date":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1 &2&3":U
    
    oControl                              = opoContainer:addControl("fdEndDate":U                  + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.end_date":U                 , "DATE":U     , 20, "End Date")  
    oControl                              = opoContainer:addControl("cbEndAmPm":U                  + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_detail.end_ampm":U                 , "LOGICAL":U  , 20, "End Date")                                                                                                                                                                                                                     
    oControl:AdditionalItems              = "AM=AM|PM=PM":U                                       
    oControl:ControlClass                 = "+clSelVertAlign":U
    
    oControl                              = opoContainer:addControl("flEndDateAmPmUpdated":U       + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._end_date_ampm_updated":U   , "CHARACTER":U, 20, "End Date":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1 &2&3":U.

  ASSIGN 
    oControl                              = opoContainer:addControl("fdTariffCost":U               + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail._detail_cost":U          , "INTEGER":U  , 21, "Tariff<br>Cost")
    oControl:ControlClass                 = "+clNumericOnly":U  
    oControl:ControlToken                 = "ReadOnly":U     
    
    oControl                              = opoContainer:addControl("fdQuantityRequested":U        + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.quantity_requested":U    , "INTEGER":U  , 22, "Qty<br>Req")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              

    oControl                              = opoContainer:addControl("fdQuantityAuth":U             + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.quantity_auth":U         , "INTEGER":U  , 23, "Qty<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              

    oControl                              = opoContainer:addControl("fdAmountRequested":U          + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_requested":U      , "DECIMAL":U  , 24, "Amount<br>Req")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              

    oControl                              = opoContainer:addControl("fdAmountAuth":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_auth":U           , "DECIMAL":U  , 25, "Amount<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                                              
    oControl                              = opoContainer:addControl("fdAmountPaid":U               + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.amount_paid":U           , "DECIMAL":U  , 26, "Amount<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              

    oControl                              = opoContainer:addControl("fiClaimCode":U                + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_provider.claim_code":U          , "INTEGER":U  , 27, "Claim<br>Code")                                                                                                                               
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U                                                                                                                              
                                                                                                                                                                                            
    oControl                              = opoContainer:addControl("buClaimBtn":U                 + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 27, "Claim<br>Code")
    oControl:CellLayoutMask               = "&1&2":U                                                                                                                                                       
    oControl:LookupWobFLA                 = "maclc":U                                                                                                                                                      
    oControl:LookupFields                 = "ccdesc.claim-code":U                                                                                                                                          
    oControl:LookupControls               = "fiClaimCode":U + ipcContainerName                                                                                                                             
    oControl:FilterFields                 = "ccdesc.claim-code":U                                                                                                                                          
    oControl:FilterControls               = "fiClaimCode":U + ipcContainerName                                                                                                        
    oControl:ReturnFields                 = "ccdesc.claim-code":U                                                                                                                                          
    oControl:ReturnControls               = "fiClaimCode":U + ipcContainerName                                                                                                                             
                                                                                                                                                                                      
    oControl                              = opoContainer:addControl("cbClaimType":U                + ipcContainerName, "wsCombo":U       , "3":U , "tt_auth_detail.claim_type":U            , "CHARACTER":U, 28, "Claim<br>Type")
    oControl:AdditionalItems              = "|C=C|N=N|A=A|K=K|P=P|O=O":U                                                                                                                                               
    oControl:ControlToolTip               = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther"
    
    oControl                              = opoContainer:addControl("cbLineRestriction":U          + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_detail.line_restriction":U      , "CHARACTER":U, 29, "Line<br>Restriction")
    oControl:ControlJavascript            = "style='width:128px'":U 
    oControl:ControlClass                 = "+clMan":U                           
                                                                                                   
    oControl                              = opoContainer:addControl("fcNote":U                     + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.note":U                  , "CHARACTER":U, 30, "Note")            
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:Reason":U                                                                                        
    oControl:FilterFields                 = "[ReasonCode],[ReasonType]":U                                                                                                             
    oControl:FilterControls               = "fcNote":U + ipcContainerName + ",fcNoteType":U + ipcContainerName                                                                        
    oControl:ReturnFields                 = "[ReasonCode]":U                                                                                                                          
    oControl:ReturnControls               = "fcNote":U + ipcContainerName                                                                                                             
                                                                                                                                                                                      
    oControl                              = opoContainer:addControl("fcNoteType":U                 + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 30, "Note")                  
    oControl:ControlToken                 = "Hidden":U                                                                                                                                       
    oControl:ControlValue                 = "AN":U                                                                                                                                           
                                                                                                                                                                                             
    oControl                              = opoContainer:addControl("buNoteBtn":U                  + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 30, "Note")                                                                                                                                                                                                   
    oControl:CellLayoutMask               = "&1&2&3":U                                                                                                                                                   
    oControl:LookupWobFLA                 = "note":U                                                                                                                                                     
    oControl:LookupFields                 = "note.key":U                                                                                                                                                 
    oControl:LookupControls               = "fcNote":U + ipcContainerName                                                                                                                              
    oControl:FilterFields                 = "note.key,note.type":U                                                                                                                                       
    oControl:FilterControls               = "fcNote":U + ipcContainerName + ",fcNoteType":U + ipcContainerName                                                                                     
    oControl:ReturnFields                 = "note.key":U                                                                                                                                               
    oControl:ReturnControls               = "fcNote":U + ipcContainerName                                                                                                             
                                                                                                                                                                                            
    oControl                              = opoContainer:addControl("cbDiscountType":U             + ipcContainerName, "wsCombo":U       , "3":U , "tt_auth_detail.discount_type":U         , "CHARACTER":U, 31, "Discount<br>Type")
    
    oControl                              = opoContainer:addControl("fdDiscountAuth":U             + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.discount_auth":U         , "DECIMAL":U  , 32, "Discount<br>Authorised")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                                               
    oControl                              = opoContainer:addControl("fdDiscountPaid":U             + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.discount_paid":U         , "DECIMAL":U  , 33, "Discount<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                            
    oControl                              = opoContainer:addControl("fdCopayAuth":U                + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_auth":U            , "DECIMAL":U  , 34, "Copay<br>Authorised")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                            
    oControl                              = opoContainer:addControl("fdCopayPaid":U                + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_paid":U            , "DECIMAL":U  , 35, "Copay<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U                                                                                                                              
                                                                                                                                                                                            
    oControl                              = opoContainer:addControl("cbStatus":U                   + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_detail.auth_status":U           , "INTEGER":U  , 36, "Status")
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

    oControl                              = opoContainer:addControl("flStatusReasonMandatory":U    + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 36, "Status":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                                         
    oControl:CellLayoutMask               = "&1&2":U

    /* On status change, check whether reason is mandatory or not */ 
    oControl:JavaScriptOnChange           = "fnOnChangeAuthDetailStatus(this);":U
                                          + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"TBL~");":U 
  
    /* See RowRenderProcedure - Checks were moved to RowRenderProcedure for performance reasons */
    /* Check whether each reason control is mandatory or not depending on status                */                                  
    oControl                              = opoContainer:addControl("fcReason":U                   + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.auth_status_note":U    , "CHARACTER":U, 37, "Status<br>Reason":U)            
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:Reason":U                                                                                        
    oControl:FilterFields                 = "[ReasonCode],[ReasonType]":U                                                                                                             
    oControl:FilterControls               = "fcReason":U + ipcContainerName + ",":U
                                          + "fcReasonTypeArgument":U + ipcContainerName                                                                    
    oControl:ReturnFields                 = "[ReasonDesc]":U                                                                
    oControl:ReturnControls               = "fcReasonDesc":U + ipcContainerName
                                                                                                                                                                                      
    oControl                              = opoContainer:addControl("fcReasonTypePrefixArgument":U + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 37, "Status<br>Reason":U)                  
    oControl:ControlToken                 = "Hidden":U                                                                                                                                 
    oControl:ControlValue                 = "AS":U
    oControl:ControlClass                 = "+clPreserveValue":U                                                                                                                                      
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthReasonTypePrefix":U

    oControl                              = opoContainer:addControl("fcReasonDesc":U               + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 37, "Status<br>Reason":U)            
    oControl:ControlToken                 = "Hidden":U                   

    /* Set reason description as tooltip when reason is selected client side */ 
    oControl:JavascriptOnChange          = "fnSetReasonDescription(this, ~"fcReason":U + ipcContainerName + "~", ~"buReasonBtn":U + ipcContainerName + "~", ~"TBL~");":U              
                                          
    oControl                              = opoContainer:addControl("fcReasonTypeArgument":U       + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 37, "Status<br>Reason":U)                  
    oControl:ControlToken                 = "Hidden":U                                            
                                                                                                                                                                                             
    oControl                              = opoContainer:addControl("buReasonBtn":U                + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 37, "Status<br>Reason":U)                                                                                                                                                                                                   
    oControl:CellLayoutMask               = "&1&2&3&4&5":U                                                                                                                                                   
    oControl:LookupWobFLA                 = "note":U                                                                                                                                                     
    oControl:LookupFields                 = "note.key":U                                                                                                                                                 
    oControl:LookupControls               = "fcReason":U + ipcContainerName                                                                                                                              
    oControl:FilterFields                 = "note.key,note.type":U                                                                                                                                       
    oControl:FilterControls               = "fcReason":U + ipcContainerName + ",":U
                                          + "fcReasonTypeArgument":U + ipcContainerName                                                                                     
    oControl:ReturnFields                 = "note.key":U                                                                                                                                           
    oControl:ReturnControls               = "fcReason":U + ipcContainerName            

    oControl                              = opoContainer:addControl("fcOverrideBaseRate":U         + ipcContainerName, "wsCombo":U       , "8":U , "tt_auth_detail.override_base_rate":U    , "CHARACTER":U, 38, "Override<br>Base<br>Rate":U)
    oControl                              = opoContainer:addControl("fcOverrideArsRate":U          + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.override_ars_rate":U     , "CHARACTER":U, 39, "":U)
    oControl                              = opoContainer:addControl("buOverrideArsRateBtn":U       + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 39, "Override<br>ARS<br>Rate":U)         
    oControl:CellLayoutMask               = "&1&2":U
    oControl:LookupWobFLA                 = "maarsrate":U
    oControl:LookupFields                 = "arsrate.ars-rate":U                                                                                                                                                 
    oControl:LookupControls               = "fcOverrideArsRate":U + ipcContainerName                                                                                                                              
    oControl:ReturnFields                 = "arsrate.ars-rate":U                                                                                                                                           
    oControl:ReturnControls               = "fcOverrideArsRate":U + ipcContainerName            
    
    oControl                              = opoContainer:addControl("flAddedByUser":U              + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail.added_by_user":U         , "CHARACTER":U, 40, "Added<br>By<br>User":U)
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlValue                 = "TRUE":U
    
    oControl                              = opoContainer:addControl("flLosCalc":U                  + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_provider.los_calculation":U     , "CHARACTER":U, 41, "System<br>LOS<br>Calculation":U)
    oControl:ControlToken                 = "ReadOnly":U
    
    oControl                              = opoContainer:addControl("fcLosCalcRule":U              + ipcContainerName, "wsInput":U       , "22":U, "tt_auth_detail.los_calculation_rule":U  , "CHARACTER":U, 42, "LOS<br>Calculation<br>Rule":U)
    oControl:ControlToken                 = "ReadOnly":U
    
    oControl                              = opoContainer:addControl("flPmbIndicator":U             + ipcContainerName, "wsCheckBox":U    , "12":U, "tt_auth_detail.pmb_indicator":U         , "LOGICAL":U  , 43, "PMB<br>Indicator":U)
    oControl:ControlToken                 = "Hidden":U                                                                                                                                                        
                              
    oControl                              = opoContainer:addControl("_authObjArgument":U           + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.auth_obj":U              , "DECIMAL":U  , 44, "":U)
    oControl:ControlToken                 = "Hidden":U                                            
                                                                                                  
    oControl                              = opoContainer:addControl("_oemArgument":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.owning_entity_mnemonic":U, "CHARACTER":U, 45, "":U)
    oControl:ControlToken                 = "Hidden":U                                            
   
    oControl                              = opoContainer:addControl("_configuration":U             + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 46, "_configuration:":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthDetailConfiguration":U

    oControl                              = opoContainer:addControl("_triggerRebuild":U            + ipcContainerName, "wsInput":U       , "8":U , "":U                                     , "CHARACTER":U, 47, "":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailRebuild(this);":U.


  ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(opoContainer)           

         oContainerProperties:AlternateKeyFields     = "cbProvider":U       + ipcContainerName + ",":U
                                                     + "cbOEM":U            + ipcContainerName + ",":U
                                                     + "fcOwningAltValue":U + ipcContainerName

         oContainerProperties:AutoSaveOperation      = "SERVICE:maUIService:ajaxSaveDetailContainer:":U + ipcContainerName
         oContainerProperties:DefaultLess            = TRUE
         oContainerProperties:CollapsableControlList = "cbTariffTypeCategory":U + ipcContainerName + ",":U 
                                                     + "fdPaid":U               + ipcContainerName + ",":U + "fdCopayPaid":U         + ipcContainerName + ",":U 
                                                     + "buNoteBtn":U            + ipcContainerName + ",":U + "fdQuantityRequested":U + ipcContainerName + ",":U 
                                                     + "fdAmountRequested":U    + ipcContainerName + ",":U + "cbDiscountType":U      + ipcContainerName + ",":U  
                                                     + "fdDiscountAuth":U       + ipcContainerName + ",":U + "fdDiscountPaid":U      + ipcContainerName + ",":U 
                                                     + "fdCopayAuth":U          + ipcContainerName + ",":U + "flLosCalc":U           + ipcContainerName + ",":U 
                                                     + "fcLosCalcRule":U        + ipcContainerName
              
         oContainerProperties:IgnoreFieldList        = "_configuration":U + ipcContainerName. 

  mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties). 
  
&ENDIF
  
  { mip/inc/mipcatcherror.i } 
    




