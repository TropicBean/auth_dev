&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
BLOCK-LEVEL ON ERROR UNDO, THROW.
/*------------------------------------------------------------------------
    Purpose: Healthcare Auth UI Service stack
    
    Author : Andrewd

  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
CREATE WIDGET-POOL.

{ sysadmma.i}

{ mip/inc/mipdefshared.i }

{ mip/inc/miptemptables.i &TempTableName = ttValidation }

{ ma/inc/maauthds.i }

{ ma/inc/maauthtypeconfigtt.i }

DEFINE VARIABLE goAuthorisation AS cls.maauthorisation NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 16.1
         WIDTH              = 95.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxValidationFinancial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationFinancial Procedure 
PROCEDURE ajaxValidationFinancial :
/*------------------------------------------------------------------------------
  Purpose   : Financial Container Ajax Validation    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE lSuccess      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cFilterFields AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFilterValues AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReturnFields AS CHARACTER   NO-UNDO.


  ASSIGN
    cFilterFields = get-value('FldLst')
    cFilterValues = get-value('ValList')
    cReturnFields = get-value('RetFldList').
    
  { mip/inc/mipcatcherror.i }
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthDetFinancial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthDetFinancial Procedure 
PROCEDURE getCntUpdAuthDetFinancial :
/*------------------------------------------------------------------------------
  Purpose   : Update financial container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
     
  RUN _defineFinancialContainer IN TARGET-PROCEDURE(INPUT ipcContainerName, OUTPUT opoContainer).
     
  ASSIGN                                                                                                                                                                                    
    opoContainer:QueryString           = "FOR EACH tt_auth_detail NO-LOCK":U
                                       + "   WHERE tt_auth_detail.auth_detail_obj = '&1'":U        
                                       
                                                  
    oControl                           = opoContainer:getControl("fcAuthorisedTitle":U    + ipcContainername)                                                                                                                                                          
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "DetailFinancialAuthorisedValue":U            
    
    oControl                           = opoContainer:getControl("fdAmountRequested":U    + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.amount_requested":U
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdQuantityRequested":U  + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.quantity_requested":U
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fcDiscountType":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.discount_type":U     
    oControl:CalculatedField           = FALSE
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "DetailFinancialDiscountType":U 
    
    oControl                           = opoContainer:getControl("fdAmountAuthorised":U   + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.amount_auth":U 
    oControl:CalculatedField           = FALSE
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "DetailFinancialAuthorisedValue":U            
    
    oControl                           = opoContainer:getControl("fdQuantityAuthorised":U + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.quantity_auth":U
    oControl:CalculatedField           = FALSE
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "DetailFinancialAuthorisedValue":U                 
    
    oControl                           = opoContainer:getControl("fdDiscountAuthorised":U + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.discount_auth":U
    oControl:CalculatedField           = FALSE  
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "DetailFinancialAuthorisedValue":U               
    
    oControl                           = opoContainer:getControl("fcCopayTitle":U         + ipcContainername)
    oControl:ColumnSpan               = 2
    oControl:CellSnippet               = "style='padding-left: 200px;'":U
    
    oControl                           = opoContainer:getControl("fdCopayAuthorised":U    + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.copay_auth":U
    oControl:CalculatedField           = FALSE
    oControl:CellLayoutMask            = "&1":U    
    oControl:ControlToken              = "Disabled":U
    oControl:CellSnippet               = "align='left'":U      
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "DetailFinancialAuthorisedValue":U            
    
    oControl                           = opoContainer:addControl("fcCopayAmountTitle":U     + ipcContainername, "wsInput":U   , "15":U, "":U, "":U, 2, 5)
    oControl:SpanOverLabel             = TRUE       
    oControl:ControlToken              = "ReadOnly":U    
    oControl:CellLayoutMask            = "&1":U
    oControl:ControlValue              = "Amount:":U
    oControl:CellClass                 = WarpSpeed:BaseClass + "Title":U
    oControl:CellSnippet               = "align='left'":U        
    
    oControl                           = opoContainer:addControl("fcPercentageTitle":U       + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 2, 6)
    oControl:ControlToken              = "ReadOnly":U
    oControl:ControlValue              = "Percentage:":U
     oControl:CellClass                = WarpSpeed:BaseClass + "Title":U  
     oControl:CellLayoutMask           = "&1":U
     oControl:SpanOverLabel            = TRUE
     oControl:CellSnippet              = "align='left'":U
    
    oControl                           = opoContainer:addControl("fcCopayPercentage":U       + ipcContainername, "wsInput":U   , "decimal":U  , "tt_auth_detail.copay_auth_%":U, "":U       , 3, 6)
    oControl:ControlToken              = "disabled":U
    oControl:CellSnippet               = "align='left'":U
    oControl:CalculatedField           = FALSE
    oControl:SpanOverLabel             = TRUE
 
    
    oControl                           = opoContainer:getControl("fdAmountPaid":U         + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.amount_paid":U       
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdQuantityPaid":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.quantity_paid":U     
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdDiscountPaid":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.discount_paid":U     
    oControl:CalculatedField           = FALSE

    oControl                           = opoContainer:getControl("fdCopayPaid":U          + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.copay_paid":U        
    oControl:CalculatedField           = FALSE        

    oControl                           = opoContainer:getControl("fdBenefit":U            + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_detail.benefit_%":U   
    oControl:CalculatedField           = FALSE   
    .                              
    
&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthFinancial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthFinancial Procedure 
PROCEDURE getCntUpdAuthFinancial :
/*------------------------------------------------------------------------------
  Purpose   : Update financial container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
     
  RUN _defineFinancialContainer IN TARGET-PROCEDURE(INPUT ipcContainerName, OUTPUT opoContainer).
     
  ASSIGN            
    opoContainer:QueryString           = "FOR EACH tt_auth NO-LOCK":U        
    
    oControl                           = opoContainer:getControl("fcAuthorisedTitle":U    + ipcContainername)                                                                                                                                                          
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "AuthFinancialAuthorisedValue":U            
                                       
    oControl                           = opoContainer:getControl("fdAmountRequested":U    + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.amount_requested":U
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdQuantityRequested":U  + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.quantity_requested":U
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fcDiscountType":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.discount_type":U
    oControl:CalculatedField           = FALSE 
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "AuthFinancialDiscountType":U 
    
    oControl                           = opoContainer:getControl("fdAmountAuthorised":U   + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.amount_auth":U
    oControl:CalculatedField           = FALSE      
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "AuthFinancialAuthorisedValue":U 
    
    oControl                           = opoContainer:getControl("fdQuantityAuthorised":U + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.quantity_auth":U
    oControl:CalculatedField           = FALSE 
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "AuthFinancialAuthorisedValue":U     
    
    oControl                           = opoContainer:getControl("fdDiscountAuthorised":U + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.discount_auth":U
    oControl:CalculatedField           = FALSE     
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "AuthFinancialAuthorisedValue":U 
    
    oControl                           = opoContainer:getControl("fdCopayAuthorised":U    + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.discount_auth":U // can't set this to blank otherwise the program crashes , so set it to discount_auth - it won't be displayed in anyways so it's not a problem
    oControl:CalculatedField           = FALSE   
    oControl:ControlToken              = "hidden":U

    
    oControl                           = opoContainer:getControl("fdAmountInterim":U      + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.amount_interim":U    
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdAmountClaimed":U      + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.amount_claimed":U    
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdAmountPaid":U         + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.amount_paid":U       
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdQuantityPaid":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.quantity_paid":U     
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdDiscountPaid":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.discount_paid":U     
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdCopayPaid":U          + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.discount_paid":U  // can't set this to blank otherwise the program crashes , so set it to discount_paid - it won't be displayed in anyways so it's not a problem
    oControl:ControlToken              = "hidden":U
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdAmountTotal":U        + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.amount_total":U      
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdBenefit":U            + ipcContainername)
    oControl:ControlQueryField         = "tt_auth.benefit_%":U
    oControl:CalculatedField           = FALSE
    .                              
    
&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthHeadFinancial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthHeadFinancial Procedure 
PROCEDURE getCntUpdAuthHeadFinancial :
/*------------------------------------------------------------------------------
  Purpose: Update Authorisation Financial container definition     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName  AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER iopoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN

  ASSIGN
    iopoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "100%":U, "":U, WarpSpeed:BaseClass, FALSE)
    iopoContainer:ContainerTitle        = "PMB and Financial Header Information":U
    iopoContainer:ViewOnly              = FALSE
    iopoContainer:ShowContainerSettings = FALSE
    iopoContainer:Collapsable           = TRUE
    iopoContainer:ContainerMode         = WarpSpeed:SubmitValue
         
    oControl                            = iopoContainer:addControl("PmbContainer":U       + iopoContainer:ContainerCode, "":U , "":U  , "":U, "":U , 1 , 1, "":U)                                                                                                                               
    oControl:RenderProcedureHandle      = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure            = "RenderProcedure":U
    oControl:RenderArgument             = "AuthHeaderPMBRender":U 
    oControl:SubContainer               = mipEnv:Health:maUIService:getCntUpdAuthPmb("Pmb":U + iopoContainer:ContainerCode)
    oControl:SubContainerType           = "FORM":U
    oControl:SpanOverLabel              = TRUE
    oControl:ColumnSpan                 = 2
    oControl:CellSnippet                = "align='center'":U
                                        
    oControl                            = iopoContainer:addControl("FinancialContainer":U + iopoContainer:ContainerCode, "":U , "":U  , "":U, "":U , 2 , 1, "":U)                                                                                                                               
    oControl:SubContainer               = mipEnv:Health:maUIService:getCntUpdAuthFinancial("Financial":U + iopoContainer:ContainerCode)
    oControl:SubContainerType           = "FORM":U
    oControl:SpanOverLabel              = TRUE
    oControl:ColumnSpan                 = 2
    oControl:CellSnippet                = "align='center'":U
                                        
    oControl                            = iopoContainer:addControl("SavingsContainer":U   + iopoContainer:ContainerCode, "":U , "":U  , "":U, "":U , 3, 1, "":U)                                                                                                                               
    oControl:SubContainer               = mipEnv:Health:maUIService:getCntUpdAuthSavings("Savings":U + iopoContainer:ContainerCode)
    oControl:SubContainerType           = "FORM":U
    oControl:SpanOverLabel              = TRUE
    oControl:ColumnSpan                 = 2
    oControl:CellSnippet                = "align='center'":U
   .

&ENDIF

{ mip/inc/mipcatcherror.i }  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthProvFinancial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthProvFinancial Procedure 
PROCEDURE getCntUpdAuthProvFinancial :
/*------------------------------------------------------------------------------
  Purpose   : Update financial container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
     
  RUN _defineFinancialContainer IN TARGET-PROCEDURE(INPUT ipcContainerName, OUTPUT opoContainer).
     
  ASSIGN                                                                                                                                                                                   
    opoContainer:QueryString           = "FOR EACH tt_auth_provider NO-LOCK":U                                                                                                                                                                             
                                       + "   WHERE tt_auth_provider.auth_provider_obj = '&1'":U                                                                                                                                                                             
      
    oControl                           = opoContainer:getControl("fcAuthorisedTitle":U    + ipcContainername)                                                                                                                                                          
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "ProviderFinancialAuthorisedValue":U            
                                       
    oControl                           = opoContainer:getControl("fdAmountRequested":U    + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.amount_requested":U
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdQuantityRequested":U  + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.quantity_requested":U
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fcCopayTitle":U         + ipcContainername)
    oControl:ColumnSpan               = 2
    oControl:CellSnippet               = "style='padding-left: 200px;'":U
    
    oControl                           = opoContainer:getControl("fcDiscountType":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.discount_type":U     
    oControl:CalculatedField           = FALSE
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "ProviderFinancialDiscountType":U 
 
    oControl                           = opoContainer:getControl("fdAmountAuthorised":U   + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.amount_auth":U   
    oControl:CalculatedField           = FALSE   
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "ProviderFinancialAuthorisedValue":U 
    
    oControl                           = opoContainer:getControl("fdQuantityAuthorised":U + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.quantity_auth":U
    oControl:CalculatedField           = FALSE 
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "ProviderFinancialAuthorisedValue":U     
    
    oControl                           = opoContainer:getControl("fdDiscountAuthorised":U + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.discount_auth":U
    oControl:CalculatedField           = FALSE    
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "ProviderFinancialAuthorisedValue":U 
    
    oControl                           = opoContainer:getControl("fdCopayAuthorised":U    + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.copay_auth":U   
    oControl:CellLayoutMask            = "&1":U    
    oControl:CalculatedField           = FALSE
    oControl:ControlToken              = "Disabled":U
    oControl:CellSnippet               = "align='left'":U        
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                                         
    oControl:RenderArgument            = "ProviderFinancialAuthorisedValue":U   
    
    oControl                           = opoContainer:addControl("fcCopayAmountTitle":U     + ipcContainername, "wsInput":U   , "15":U, "":U, "":U, 2, 5)
    oControl:SpanOverLabel             = TRUE       
    oControl:ControlToken              = "ReadOnly":U    
    oControl:CellLayoutMask            = "&1":U
    oControl:ControlValue              = "Amount:":U
    oControl:CellClass                 = WarpSpeed:BaseClass + "Title":U
    oControl:CellSnippet               = "align='left'":U        
    
    oControl                           = opoContainer:addControl("fcPercentageTitle":U       + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 2, 6)
    oControl:ControlToken              = "ReadOnly":U
    oControl:ControlValue              = "Percentage:":U
     oControl:CellClass                 = WarpSpeed:BaseClass + "Title":U  
     oControl:CellLayoutMask            = "&1":U
     oControl:SpanOverLabel             = TRUE
     oControl:CellSnippet               = "align='left'":U
    
    oControl                           = opoContainer:addControl("fcCopayPercentage":U       + ipcContainername, "wsInput":U   , "decimal":U  , "tt_auth_provider.copay_auth_%":U, "":U       , 3, 6)
    oControl:ControlToken              = "disabled":U
    oControl:CellSnippet               = "align='left'":U
    oControl:CalculatedField           = FALSE
    oControl:SpanOverLabel             = TRUE
    
    oControl                           = opoContainer:getControl("fdAmountInterim":U      + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.amount_interim":U    
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdAmountClaimed":U      + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.amount_claimed":U    
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdAmountPaid":U         + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.amount_paid":U       
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdQuantityPaid":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.quantity_paid":U     
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdDiscountPaid":U       + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.discount_paid":U     
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdCopayPaid":U          + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.copay_paid":U        
    oControl:CalculatedField           = FALSE
    oControl:ControlToken              = "updatable":U
    
    oControl                           = opoContainer:getControl("fdAmountTotal":U        + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.amount_total":U      
    oControl:CalculatedField           = FALSE
    
    oControl                           = opoContainer:getControl("fdBenefit":U            + ipcContainername)
    oControl:ControlQueryField         = "tt_auth_provider.benefit_%":U   
    oControl:CalculatedField           = FALSE   
    .                              
    
&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-renderProcedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderProcedure Procedure 
PROCEDURE renderProcedure :
/*------------------------------------------------------------------------------
  Purpose   : Auth ui service specific render procedure    
  Parameters: Control to be rendered
  Notes     :  
  Author    : MMP     
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  
  DEFINE VARIABLE cBufferName    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cBuffName      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDiscountType  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dAmountAuth    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dAuthObj       AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dQuantityAuth  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE lSuccess       AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE hBuffer        AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hQuery         AS HANDLE      NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN  

  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).

  CASE ipoControl:RenderArgument:      
  
    WHEN "DetailFinancialAuthorisedValue":U OR WHEN "ProviderFinancialAuthorisedValue":U OR WHEN "AuthFinancialAuthorisedValue":U  THEN
    DO:
        
      CASE ipoControl:RenderArgument:
      
        WHEN "DetailFinancialAuthorisedValue":U 
        THEN 
          ASSIGN cBufferName = "tt_auth_detail":U.
        
        WHEN "ProviderFinancialAuthorisedValue":U 
        THEN 
          ASSIGN cBufferName = "tt_auth_provider":U.
        
        WHEN "AuthFinancialAuthorisedValue":U     
        THEN 
          ASSIGN cBufferName = "tt_auth":U.
        
      END CASE. /*CASE ipoControl:RenderArgument:*/
      
      
      IF VALID-HANDLE(hQuery)
      AND hQuery:GET-BUFFER-HANDLE(cBufferName):AVAILABLE
      THEN
        ASSIGN
          hBuffer       = hQuery:GET-BUFFER-HANDLE(cBufferName)
          dAuthObj      = hBuffer::auth_obj
          dAmountAuth   = hBuffer::amount_auth
          dQuantityAuth = hBuffer::quantity_auth.
      
      IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN
      DO:
        IF NOT VALID-OBJECT(goAuthorisation)
        OR NOT goAuthorisation:InFocus
        OR goAuthorisation:AuthObj <> dAuthObj
        THEN 
          mipEnv:Health:AuthService:getAuthObject
            ( INPUT  dAuthObj,
              INPUT  "":U,
              OUTPUT goAuthorisation ).

        mipEnv:Health:AuthService:getAuthTypeConfig(INPUT goAuthorisation:AuthTypeObj,                       
                                                    INPUT goAuthorisation:InsurerObj,                        
                                                    INPUT goAuthorisation:MemberOptionCode,                        
                                                    INPUT goAuthorisation:StartDate,                         
                                                    INPUT-OUTPUT TABLE ttAuthTypeConfig).     
                        
        FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
                                                                                     
        { mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
        
        
        IF AVAILABLE ttAuthTypeConfig AND NOT ttAuthTypeConfig.ActivateAuthorisedValues AND dAmountAuth = 0.00 AND dQuantityAuth = 0.00 
        THEN 
          ASSIGN ipoControl:ControlToken = "Hidden":U.
      
      END. /*IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN*/ 
        
      ipoControl:RenderASInput().         
            
    END. /*WHEN "AuthProviderAuthorisedValue":U THEN*/

    WHEN "ProviderFinancialDiscountType":U OR WHEN "AuthFinancialDiscountType" OR WHEN "DetailFinancialDiscountType" THEN
    DO:
        CASE ipoControl:RenderArgument:
      
          WHEN "ProviderFinancialDiscountType":U 
          THEN 
            ASSIGN cBuffName = "tt_auth_provider":U.
          
          WHEN "AuthFinancialDiscountType":U 
          then
              ASSIGN cBuffName = "tt_auth":U.

          WHEN "DetailFinancialDiscountType":U 
          THEN 
            ASSIGN cBuffName = "tt_auth_detail":U.
        
        END CASE. /*CASE ipoControl:RenderArgument:*/

        IF VALID-HANDLE(hQuery)
        AND hQuery:GET-BUFFER-HANDLE(cBuffName):AVAILABLE
        THEN
          ASSIGN
            hBuffer       = hQuery:GET-BUFFER-HANDLE(cBuffName)
            dAuthObj      = hBuffer::auth_obj.
      
      IF dAuthObj <> 0.00 AND dAuthObj <> ?
      AND NOT WarpSpeed:ValidationError THEN
      DO:
        ASSIGN ipoControl:ControlValue = hQuery:GET-BUFFER-HANDLE(cBuffName)::discount_type WHEN VALID-HANDLE(hQuery).
        
        IF ipoControl:ControlValue = STRING(yes) OR ipoControl:ControlValue = "yes" THEN
            ASSIGN ipoControl:ControlValue = "P".
        
        IF ipoControl:ControlValue = STRING(no) OR ipoControl:ControlValue = "no" THEN
            ASSIGN ipoControl:ControlValue = "R".
        
        IF ipoControl:ControlValue = STRING(?) OR ipoControl:ControlValue = "?" THEN
            ASSIGN ipoControl:ControlValue = "?".
        
      END. /* IF dAuthObj <> 0.00 or dAuthObj <> ? THEN */
      ipoControl:RenderASComboORSelect(). 

    END. /* WHEN "ProviderFinancialDiscountType":U or when "AuthFinancialDiscountType" or when "DetailFinancialDiscountType" THEN */

    OTHERWISE RUN SUPER(ipoControl).
  END. /*CASE ipoControl:RenderArgument:      */
    
  
  { mip/inc/mipcatcherror.i }
                               
                
&ENDIF          
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_defineFinancialContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _defineFinancialContainer Procedure 
PROCEDURE _defineFinancialContainer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose   : Financial container definition    
  Parameters: 
  Notes     :       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.
  
  DEFINE VARIABLE oControl AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL          NO-UNDO.
  
&IF {&DBDFMA} >= 10195 &THEN
     
  ASSIGN 
    opoContainer                       = NEW cls.mipwscontainer(ipcContainerName, "99%":U, "":U, WarpSpeed:BaseClass, FALSE)
    opoContainer:ContainerTitle        = "Financial Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
                                                                                                                                                                                  
    oControl                           = opoContainer:addControl("fcAmountTitle":U        + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 1, 2)
    oControl:CellClass                 = WarpSpeed:BaseClass + "Title":U                                                    
    oControl:CellLayoutMask            = "Amounts":U                                                                        
    oControl:SpanOverLabel             = TRUE
    oControl                           = opoContainer:addControl("fcQuantityTitle":U      + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 1, 3)
    oControl:CellClass                 = WarpSpeed:BaseClass + "Title":U                                                    
    oControl:CellLayoutMask            = "Quantities":U                                                                     
    oControl:SpanOverLabel             = TRUE
    oControl                           = opoContainer:addControl("fcDiscountsTitle":U     + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 1, 4)
    oControl:CellClass                 = WarpSpeed:BaseClass + "Title":U                                                                               
    oControl:CellLayoutMask            = "Discounts":U                                                                                                 
    oControl:SpanOverLabel             = TRUE
    oControl                           = opoContainer:addControl("fcCopayTitle":U         + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 1, 5)
    oControl:CellClass                 = WarpSpeed:BaseClass + "Title":U                                                                               
    oControl:CellLayoutMask            = "Co-payments":U                                                                                               
    oControl:SpanOverLabel             = TRUE                                                                                                          
                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcRequestTitle":U       + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 2, 1)
    oControl:ControlToken              = "ReadOnly":U                                                                                                  
    oControl:ControlValue              = "Requested:":U                                                                                                
    oControl:CellSnippet               = "align='right'":U                                                                                             
    oControl:ControlClass              = "+clLbl":U                                                                                                    
                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcAuthorisedTitle":U    + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 3, 1)
    oControl:ControlToken              = "ReadOnly":U                                                                                                  
    oControl:ControlValue              = "Authorised:":U                                                                                               
    oControl:CellSnippet               = "align='right'"                                                                                               
    oControl:ControlClass              = "+clLbl":U                
                                                                                              
    oControl                           = opoContainer:addControl("fcInterimTitle":U       + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 4, 1)
    oControl:ControlToken              = "ReadOnly":U                                                                                                  
    oControl:ControlValue              = "Interim:":U                                                                                                  
    oControl:CellSnippet               = "align='right'":U                                                                                             
    oControl:ControlClass              = "+clLbl":U                                                                                                    
                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcClaimedTitle":U       + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 5, 1)
    oControl:ControlToken              = "ReadOnly":U                                                                                                  
    oControl:ControlValue              = "Claimed:":U                                                                                                  
    oControl:CellSnippet               = "align='right'"                                                                                               
    oControl:ControlClass              = "+clLbl":U                                                                                                    
                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcPaidTitle":U          + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 6, 1)
    oControl:ControlToken              = "ReadOnly":U                                                                                                  
    oControl:ControlValue              = "Paid:":U                                                                                                     
    oControl:CellSnippet               = "align='right'":U                                                                                             
    oControl:ControlClass              = "+clLbl":U                                                                                                    
                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcTotalTitle":U         + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 7, 1)
    oControl:ControlToken              = "ReadOnly":U                                                                                                  
    oControl:ControlValue              = "Total:":U                                                                                                    
    oControl:CellSnippet               = "align='right'":U                                                                                             
    oControl:ControlClass              = "+clLbl":U                                                                                                    
                                                                                                                                                       
    oControl                           = opoContainer:addControl("fcBenefitTitle":U       + ipcContainername, "wsInput":U   , "":U  , "":U, "":U       , 8, 1)
    oControl:ControlToken              = "ReadOnly":U
    oControl:ControlValue              = "Benefit:":U
    oControl:CellSnippet               = "align='right'":U
    oControl:ControlClass              = "+clLbl":U

    oControl                           = opoContainer:addControl("fdAmountRequested":U    + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 2, 2)
    oControl:ControlClass              = "+clNumericOnly":U  
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlTooltip            = "Please enter a valid Amount Requested.":U
    
    oControl                           = opoContainer:addControl("fdQuantityRequested":U  + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 2, 3)
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Quantity Requested.":U           
    
    oControl                           = opoContainer:addControl("fcDiscountType":U       + ipcContainername, "wsCombo":U   , "15":U, "":U, "CHARACTER":U, 2, 4)
    oControl:AdditionalItems           = "<None>=?|Percentage=P|Rand=R":U
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please choose a valid Discount Type.":U 
    
    oControl                           = opoContainer:addControl("fdAmountAuthorised":U   + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 3, 2)
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Authorisation Amount.":U    
                                                                                          
    oControl                           = opoContainer:addControl("fdQuantityAuthorised":U + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 3, 3)
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Authorisation Quantity.":U 
                                                                                                                  
    oControl                           = opoContainer:addControl("fdDiscountAuthorised":U + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 3, 4)
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE         
    oControl:ControlTooltip            = "Please enter a valid Authorisation Discount Amount.":U  
                                                                                                              
    oControl                           = opoContainer:addControl("fdCopayAuthorised":U    + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 3, 5)
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE
    oControl:ControlToken              = "readonly":U
    oControl:RenderProcedureHandle     = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure           = "RenderProcedure":U                                                                                                 
    oControl:RenderArgument            = "AuthFinancialAuthorisedValue":U                   
    
    oControl                           = opoContainer:addControl("fdAmountInterim":U      + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 4, 2)
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Interim Amount.":U           
    
    oControl                           = opoContainer:addControl("fdQuantityInterim":U    + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 4, 3)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdDiscountInterim":U    + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 4, 4)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdCopayInterim":U       + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 4, 5)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdAmountClaimed":U      + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 5, 2)
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Amount Claimed.":U           
                                                                                                                            
    oControl                           = opoContainer:addControl("fdQuantityClaimed":U    + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 5, 3)
    oControl:ControlClass              = "+clNumericOnly":U             
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Qauntity Claimed.":U           
    
    oControl                           = opoContainer:addControl("fdDiscountClaimed":U    + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 5, 4)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdCopayClaimed":U       + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 5, 5)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdAmountPaid":U         + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 6, 2)
    oControl:ControlToken              = "disabled":U                                                                 
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Paid Amount.":U           
                                                                                                                            
    oControl                           = opoContainer:addControl("fdQuantityPaid":U       + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 6, 3)
    oControl:ControlToken              = "disabled":U                                                                 
    oControl:SpanOverLabel             = TRUE 
    oControl:ControlTooltip            = "Please enter a valid Paid Quantity.":U          
                                                                                                                            
    oControl                           = opoContainer:addControl("fdDiscountPaid":U       + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 6, 4)
    oControl:ControlToken              = "disabled":U                                                                 
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Paid Discount Amount.":U           
                                                                                                                            
    oControl                           = opoContainer:addControl("fdCopayPaid":U          + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 6, 5)
    oControl:ControlToken              = "disabled":U                                                                 
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Paid Co-payment Amount.":U          
    
    oControl                           = opoContainer:addControl("fdAmountTotal":U        + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 7, 2)
    oControl:ControlToken              = "disabled":U                                                                 
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Total Amount.":U                     
    
    oControl                           = opoContainer:addControl("fdQuantityTotal":U      + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 7, 3)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdDiscountTotal":U      + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 7, 4)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdCopayTotal":U         + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 7, 5)
    oControl:SpanOverLabel             = TRUE           
    oControl:ControlToken              = "readonly":U
    oControl:CellLayoutMask            = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-":U
    
    oControl                           = opoContainer:addControl("fdBenefit":U            + ipcContainername, "wsInput":U   , "15":U, "":U, "DECIMAL":U, 8, 2)
    oControl:ControlToken              = "disabled":U                                                                 
    oControl:SpanOverLabel             = TRUE
    oControl:ControlTooltip            = "Please enter a valid Benefit %.":U.                              
&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

