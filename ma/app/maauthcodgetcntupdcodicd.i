/* maauthcodgetcntupdcodicd.i  MEDSTAR Medical Aid System
                               Authorisation Coding Container Procedure ( getCntUpdCodingICD )
                               (c) Copyright 1990 - 2020
                               MIP Holdings (Pty) Ltd
                               All rights reserved
*/
  
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oWob                 AS cls.mipwswob              NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.  
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cContext             AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cUsage               AS CHARACTER                 NO-UNDO.  
  DEFINE VARIABLE cQueryFieldList      AS CHARACTER                 NO-UNDO.
    
&IF {&DBDFMA} >= 10195 &THEN


  RUN _defineSharedControls IN TARGET-PROCEDURE(INPUT ipcContainerName, OUTPUT opoContainer).

  ASSIGN     
    oWob                                  = Warpspeed:CurrentObject     
    cUsage                                = "SERVICE:maUIService:ajaxSaveCodingContainer:CompleteSave":U     
    cContext                              = wsUiService:translateContext(Warpspeed:CurrentContext, TRUE) 
                                   
    opoContainer:RowRenderProcedureHandle = mipEnv:Health:maUIService:RenderProcedureHandle
    opoContainer:RowRenderProcedure       = "RowRenderProcedure":U
    opoContainer:RowRenderArgument        = "AuthICDCodingContainer":U
    opoContainer:ContainerTitle           = "Coding Details - Diagnosis(ICD)":U
    opoContainer:QueryString              = "FOR EACH tt_auth_coding NO-LOCK":U
                                          + "   WHERE tt_auth_coding.owning_entity_mnemonic = 'diagnos',":U                                         
                                          + "   FIRST tt_auth NO-LOCK":U
                                          + "   WHERE tt_auth.auth_obj = tt_auth_coding.auth_obj OUTER-JOIN":U
                                          + "      BY tt_auth_coding.line_number":U

    oControl                             = opoContainer:addControl("cbCodingType":U              + ipcContainerName, "wsCombo":U        , "":U   , "tt_auth_coding.coding_type":U     , "CHARACTER":U,  4, "Coding Type":U)
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AcronymSelect:ma_acICDCodingType:=":U
    oControl:ControlClass                = "+clMan":U
    oControl                             = opoContainer:addControl("fcCodingTypeRule":U          + ipcContainerName, "wsInput":U        , "":U   , "":U                              , "CHARACTER":U,  4, "Coding Type":U)
    oControl:CellLayoutMask              = "&1&2":U
    oControl:ControlToken                = "Hidden":U
    
    oControl                             = opoContainer:addControl("fdOwningObj":U               + ipcContainerName, "wsInput":U       , "8":U   , "tt_auth_coding.owning_obj":U      , "DECIMAL":U  ,  5)
    oControl:ControlToken                = "Hidden":U

    oControl                             = opoContainer:addControl("fcOwningKey":U               + ipcContainerName, "wsInput":U       , "8":U   , "tt_auth_coding.owning_key":U      , "CHARACTER":U,  5)
    oControl:ControlToken                = "Hidden":U
    oControl:JavascriptOnChange          = "fnSetAuthCodingLink(this,             ":U
                                         + "                 ~"_oemArgument~",   ":U
                                         + "                 ~"fcOwningKey~",     ":U
                                         + "                 ~"fdOwningAltObj~",  ":U
                                         + "                 ~"fcOwningAltValue~",":U
                                         + "                 ~"fcViewDetail~",    ":U
                                         + "                 ~"fcLinkTemplate~"); ":U
                                         
    oControl                             = opoContainer:addControl("fdOwningAltObj":U            + ipcContainerName, "wsInput":U       , "8":U   , "":U                               , "DECIMAL":U,  5)
    oControl:ControlToken                = "Hidden":U

    oControl                             = opoContainer:addControl("fcOwningAltValue":U          + ipcContainerName, "wsInput":U       , "8":U   , "tt_auth_coding.owning_alt_value":U, "CHARACTER":U,  5)
    oControl:ControlToolTip              = "Please enter a valid ICD10 code":U
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AuthCodingOwningAltValue":U
    oControl:ControlClass                = "+clMan":U   
    oControl:AjaxValidation              = "SERVICE:maUIService:ajaxValidationCoding:ICD":U
    oControl:FilterFields                = "[ICDCode],[StartDate]":U
    oControl:FilterControls              = "fcOwningAltValue":U + ipcContainerName + ", fdStartDate":U + ipcContainerName
    oControl:ReturnFields                = "[ICDObj],[ICDCode],[ICDCode],[ICDDescription],[CheckDagger],[CheckMorphology],[CheckAsterisk]":U
    oControl:ReturnControls              = "fdOwningAltObj":U + ipcContainerName + ",fcOwningKey":U + ipcContainerName + ",fcOwningAltValue":U + ipcContainerName + ",fcOwningDesc":U + ipcContainerName 
                                         + ",fcCheckDagger":U + ipcContainerName + ",fcCheckMorphology":U + ipcContainerName + ",fcCheckAsterisk":U + ipcContainerName
                                         
    /*This function will remove the error class from all diagnosis controls with a certain name */
    /*when a diagnosis is changed, which was to cater for ECC validation.                       */                                     
    oControl:JavascriptOnChange          = "fnOnChangeAuthCodingDiagnosis(~"" + oControl:ControlName + "~");":U
    
    oControl                             = opoContainer:addControl("fcCheckDagger":U             + ipcContainerName, "wsInput":U       , "5":U   , "":U                               , "CHARACTER":U,  5)
    oControl:ControlToken                = "Hidden":U  
    
    /*If the check dagger/morphology controls indicate dagger/morphology, this function */
    /*will enable the asterisk/morphology controls for the user to select.              */  
    oControl:JavascriptOnChange          = "fnOnChangeEnableLookup(this,~"fcAsterisk":U + ipcContainerName + "~",~"buAsteriskBtn":U + ipcContainerName + "~", true);":U
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                   
    oControl:RenderProcedure             = "RenderProcedure":U                                                                                                               
    oControl:RenderArgument              = "AuthCodingCheckDaggerCode":U 

    oControl                             = opoContainer:addControl("fcCheckMorphology":U         + ipcContainerName, "wsInput":U       , "5":U  , "tt_auth_coding._check_morphology":U, "CHARACTER":U,  5)                                     
    oControl:ControlToken                = "Hidden":U
    
    /*If the check dagger/morphology controls indicate dagger/morphology, this function */
    /*will enable the asterisk/morphology controls for the user to select.              */
    oControl:JavascriptOnChange          = "fnOnChangeEnableLookup(this,~"fcMorphology":U + ipcContainerName + "~",~"buMorphologyBtn":U + ipcContainerName + "~", false);":U
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle                                                                                   
    oControl:RenderProcedure             = "RenderProcedure":U                                                                                                               
    oControl:RenderArgument              = "AuthCodingCheckMorphologyCode":U                                                                                                       
    oControl                             = opoContainer:addControl("fcCheckAsterisk":U           + ipcContainerName, "wsInput":U       , "5":U   , "":U                               , "CHARACTER":U,  5)
    oControl:ControlToken                = "Hidden":U 
    
    /*Asterisk captured in the primary position will be moved to the asterisk code field */ 
    /*and the user will then need to populate the ICD code from the lookup provided.     */   
    oControl:JavascriptOnChange          = "fnOnChangeAuthCodingCheckAsterisk(this,~"fcOwningKey":U      + ipcContainerName + "~", ":U
                                         + "                                       ~"fcOwningAltValue":U + ipcContainerName + "~", ":U
                                         + "                                       ~"fcAsterisk":U       + ipcContainerName + "~", ":U
                                         + "                                       ~"buAsteriskBtn":U    + ipcContainerName + "~", ":U
                                         + "                                       ~"cbStatus":U         + ipcContainerName + "~");":U
    oControl                             = opoContainer:addControl("buOwningBtn":U               + ipcContainerName, "wsLookupButton":U, "":U    , "":U                               , "CHARACTER":U,  5, "ICD Code":U)
    oControl:CellLayoutMask              = "&1&2&3&4&5&6&7&8":U
    // MMP-546 Changes for new diagnosis lookup
    oControl:LookupWobFla                = "diagnos":U                                                                                                           
    oControl:LookupFields                = "diagnos.diagnosis":U                                                                                                 
    oControl:LookupControls              = "fcOwningAltValue":U + ipcContainerName                                                                                         
    oControl:FilterFields                = "diagnos.diagnosis,hlm_icd_industry.effective_date <=":U                                                                                      
    oControl:FilterControls              = "fcOwningAltValue":U + ipcContainerName + "," + "fdStartDate":U + ipcContainerName                                                                                             
    oControl:ReturnFields                = "diagnos.diagnosis,diagnos.description":U                                                                             
    oControl:ReturnControls              = "fcOwningAltValue":U + ipcContainerName + ",fcOwningDesc":U + ipcContainerName                                                                                                            
    oControl                             = opoContainer:addControl("fcOwningDesc":U              + ipcContainerName, "wsTextArea":U    , "15,3":U, "tt_auth_coding._owning_description":U, "CHARACTER":U,  6)
    oControl:ControlToken                = "Disabled":U                                                                                                       
    
    oControl                             = opoContainer:addControl("fcViewDetail":U              + ipcContainerName, "wsHref":U        , "":U    , "":U                               , "":U         ,  7, "":U)
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AuthCodingViewDetail":U  

    /* This field is required to keep a "template" of the view more detail link which will be used to dynamically    */
    /* change the view detail link onclick and href attributes depending on the owning entity and item code selected.*/     
    oControl                             = opoContainer:addControl("fcLinkTemplate":U            + ipcContainerName, "wsHref":U        , "10":U, "":U                                 , "CHARACTER":U, 8, "")    
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AuthCodingViewDetail":U
    
    oControl                             = opoContainer:addControl("flMainCode":U                + ipcContainerName, "wsCheckBox":U    , "":U    , "tt_auth_coding.main_code":U       , "LOGICAL":U  , 10, "Main<br>Code":U)
    
    /*When a main code is selected , this function will run through all the other rows and    */ 
    /*if the current main code is checked then any other row with a checked main code will be */
    /*unchecked as there can be only one main code.                                           */
    oControl:JavascriptOnClick           = "fnOnClickAuthCodingMainCode(this, ~"":U + oControl:ControlName + "~");":U
    
    oControl                             = opoContainer:addControl("fcAsteriskParentArgument":U  + ipcContainerName, "wsInput":U       , "5":U   , "":U                               , "CHARACTER":U, 11, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden':U
    oControl:ControlValue                = 'diagnos':U                    
                                                                                      
    oControl                             = opoContainer:addControl("fcAsteriskAcronymArgument":U + ipcContainerName, "wsInput":U       , "5":U   , "":U                               , "CHARACTER":U, 11, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden':U
    oControl:ControlValue                = 'ma_acCodeLinkCatAsterisk'

    oControl                             = opoContainer:addControl("fdAsteriskObj":U             + ipcContainerName, "wsInput":U       , "5":U   , "tt_auth_coding.ass_diag_obj":U    , "DECIMAL":U  , 11, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden':U

    oControl                             = opoContainer:addControl("flAsterisk":U                + ipcContainerName, "wsInput":U       , "5":U   , "tt_auth_coding._check_asterisk":U , "LOGICAL":U  , 11, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden':U
    
    oControl                             = opoContainer:addControl("fcAsterisk":U                + ipcContainerName, "wsInput":U       , "5":U   , "tt_auth_coding._asterisk_code":U  , "CHARACTER":U, 11, "":U)    
    oControl:ControlToolTip              = "Please enter a valid asterisk code":U
    oControl:AjaxValidation              = "SERVICE:maUIService:ajaxValidationCoding:Asterisk":U                                                                                        
    oControl:FilterFields                = "[ICDCode],[AsteriskCode]":U                      
    oControl:FilterControls              = "fcOwningAltValue":U + ipcContainerName + ",fcAsterisk":U + ipcContainerName
    oControl:ReturnFields                = "[AsteriskObj],[AsteriskCode]":U                
    oControl:ReturnControls              = "fdAsteriskObj":U + ipcContainerName + ",fcAsterisk":U + ipcContainerName 
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AuthCodingAsterisk":U

    oControl                             = opoContainer:addControl("buAsteriskBtn":U             + ipcContainerName, "wsLookupButton":U, "":U    , "":U                               , "":U         , 11, "Asterisk<br>Code":U)
    oControl:CelllayoutMask              = "&1&2&3&4&5&6":U
    oControl:LookupWobFla                = "mahlmcl":U
    oControl:LookupFields                = "hlm_code_link.child_entity_obj":U
    oControl:LookupControls              = "fdAsteriskObj":U + ipcContainerName
    oControl:FilterFields                = "hlm_code_link.parent_entity,hlm_code_link.parent_alt_value,hlm_code_link.acronym_key,hlm_code_link.child_alt_value":U
    oControl:FilterControls              = "fcAsteriskParentArgument":U + ipcContainerName + ",fcOwningKey":U + ipcContainerName + ",fcAsteriskAcronymArgument":U + ipcContainerName + ",fcAsterisk":U + ipcContainerName
    oControl:ReturnFields                = "hlm_code_link.parent_alt_value,hlm_code_link.child_alt_value":U
    oControl:ReturnControls              = "fcOwningAltValue":U + ipcContainerName + ",fcAsterisk":U + ipcContainerName                                             
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AuthCodingAsterisk":U
                                                                                                                                                                                  
    oControl                             = opoContainer:addControl("fcMorphParentArgument":U     + ipcContainerName, "wsInput":U       , "5":U   , "":U                               , "CHARACTER":U, 12, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden'
    oControl:ControlValue                = 'diagnos'                         
                                                                                 
    oControl                             = opoContainer:addControl("fcMorphAcronymArgument":U    + ipcContainerName, "wsInput":U       , "5":U   , "":U                               , "CHARACTER":U, 12, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden'
    oControl:ControlValue                = 'ma_acCodeLinkCatMorph'

    oControl                             = opoContainer:addControl("fdMorphologyObj":U           + ipcContainerName, "wsInput":U       , "5":U   , "tt_auth_coding.morph_diag_obj":U  , "DECIMAL":U  , 12, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden'

    oControl                             = opoContainer:addControl("flMorphology":U              + ipcContainerName, "wsInput":U       , "5":U   , "tt_auth_coding._check_morphology":U, "LOGICAL":U , 12, "":U)                                                                                                                                                                           
    oControl:ControlToken                = 'Hidden'

    oControl                             = opoContainer:addControl("fcMorphology":U              + ipcContainerName, "wsInput":U       , "7":U   , "tt_auth_coding._morphology_code":U, "CHARACTER":U, 12, "":U)
    oControl:ControlToolTip              = "Please enter a valid morphology code":U
    oControl:AjaxValidation              = "SERVICE:maUIService:ajaxValidationCoding:Morphology":U 
    oControl:FilterFields                = "[ICDCode],[MorphologyCode]":U
    oControl:FilterControls              = "fcOwningKey":U + ipcContainerName + ",fcMorphology":U + ipcContainerName
    oControl:ReturnFields                = "[MorphologyObj],[MorphologyCode]":U
    oControl:ReturnControls              = "fdMorphologyObj":U + ipcContainerName + ",fcMorphology":U + ipcContainerName 
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AuthCodingMorphology":U

    oControl                             = opoContainer:addControl("buMorphologyBtn":U           + ipcContainerName, "wsLookupButton":U, "":U    , "":U                               , "":U         , 12, "Morphology<br>Code":U)
    oControl:CelllayoutMask              = "&1&2&3&4&5&6":U
    oControl:LookupWobFla                = "mahlmcl":U
    oControl:LookupFields                = "hlm_code_link.child_alt_value":U
    oControl:LookupControls              = "fcMorphology":U + ipcContainerName
    oControl:FilterFields                = "hlm_code_link.parent_entity,hlm_code_link.parent_alt_value,hlm_code_link.acronym_key,hlm_code_link.child_alt_value":U
    oControl:FilterControls              = "fcMorphParentArgument":U + ipcContainerName + ",fcOwningKey":U + ipcContainerName + ",fcMorphAcronymArgument":U + ipcContainerName + ",fcMorphology":U + ipcContainerName
    oControl:ReturnFields                = "hlm_code_link.child_alt_value":U
    oControl:ReturnControls              = "fcMorphology":U + ipcContainerName
    oControl:RenderProcedureHandle       = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure             = "RenderProcedure":U
    oControl:RenderArgument              = "AuthCodingMorphology":U

    oControl                             = opoContainer:addControl("flPMBIndicator":U             + ipcContainerName, "wsCheckBox":U    , "":U  , "tt_auth_coding.pmb_indicator":U         , "LOGICAL":U  , 18, "PMB<br>Indicator":U)    
    oControl                             = opoContainer:addControl("buProtocolView":U             + ipcContainerName, "cls.mipwsembedcontainer":U, "10":U, "tt_auth_coding.owning_obj":U,    "CHARACTER":U, 25, "":U) 
    oControl:ControlTooltip              = "View Protocol(s)":U                      
    oControl:RenderProcedureHandle       = mipEnv:Health:maUiService:RenderProcedureHandle 
    oControl:RenderProcedure             = "RenderProcedure":U 
    oControl:RenderArgument              = "RenderArgument=ProtocolViewButton":U 
                                         + "&BufferName=tt_auth_coding":U 
                                         + "&OwningMnemonicField=owning_entity_mnemonic":U 
                                         + "&OwningObjField=owning_obj":U 
                                         + "&OwningKeyField=owning_key":U 
.

    
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
  
  
  ASSIGN cQueryFieldList                      = fnGetQueryFieldList(opoContainer)
    
         opoContainer:TableCompleteJavascript = opoContainer:TableCompleteJavascript 
         
                                              /* Coding container complete event handler                            */
                                              /* This function will configure the coding container focus in event   */
                                              /* which will launch crosswalks for a new authorisation when the user */
                                              /* enters the coding container                                        */ 
                                              + SUBSTITUTE("fnCodingContainerComplete('&1');":U, opoContainer:ContainerCode)
                                                                                            
                                              /* This function will configure the coding container focus out event             */
                                              /* which will save all rows in the container when the user leaves the container. */
                                              + SUBSTITUTE("fnSaveOnLeaveContainer('&1','&2','&3','&4','&5','&6','&7', '&8');":U, 
                                                  opoContainer:ContainerCode, cContext, cUsage, "_authObjAuthContainer", "fdAuthCodingObj", "fcTitleValidationFilter":U + ipcContainerName ,"fcTitleValidationResponse":U + ipcContainerName, cQueryFieldList).

  /*
    Add title container if we dont have one already 
    The title container may not be available in certain scenarios such as the user having view access 
    only as prepareCustomizedContainer above will only add a title container if not view only.
  */
  IF NOT VALID-OBJECT(opoContainer:TitleContainer)
  THEN
    ASSIGN opoContainer:TitleContainer                 = NEW cls.mipwscontainer("Title":U + opoContainer:ContainerCode)
           opoContainer:TitleContainer:Borders         = "FALSE":U
           opoContainer:TitleContainer:Collapsable     = FALSE
           opoContainer:TitleContainer:ViewOnly        = FALSE
           opoContainer:TitleContainer:ContainerWidth  = "100%":U.
  
  /*
    The following controls will be used for the container leave handler which will do a 
    complete save against all the coding records which is required for the S&T validation.
  */
  ASSIGN oControl                    = opoContainer:TitleContainer:addControl("fcTitleValidationFilter":U   + ipcContainerName, "wsInput":U, "5":U, "":U, "":U, 1, 98, "":U)
         oControl:ControlToken       = "Hidden":U                                             
         oControl                    = opoContainer:TitleContainer:addControl("fcTitleValidationResponse":U + ipcContainerName, "wsInput":U, "5":U, "":U, "":U, 1, 99, "":U)
         oControl:ControlToken       = "Hidden":U         
         oControl:JavascriptOnChange = "fnOnLeaveContainerResponse(this, ~"" + ipcContainerName + "~", ~"fcTitleValidationFilter" + ipcContainerName + "~");":U
/*                                     + "fnAuthCodingRebuild();":U AD - This is causing some problems, discuss an alternate solution with EM/CK */
   .
  
&ENDIF
  
  { mip/inc/mipcatcherror.i } 
    

