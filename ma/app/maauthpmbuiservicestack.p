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
         HEIGHT             = 9.95
         WIDTH              = 50.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ajaxValidationPmb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ajaxValidationPmb Procedure 
PROCEDURE ajaxValidationPmb :
/*------------------------------------------------------------------------------
  Purpose   : PMB Container Ajax Validation    
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

&IF DEFINED(EXCLUDE-getCntUpdAuthDetPmb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthDetPmb Procedure 
PROCEDURE getCntUpdAuthDetPmb :
/*------------------------------------------------------------------------------
  Purpose   : Update provider container definition    
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
    opoContainer:ContainerTitle        = "PMB Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth_detail NO-LOCK":U                                                                                                                                                                             
                                                                                                                                                                             
    oControl                           = opoContainer:addControl("flPmbIndicator":U + ipcContainername, "wsCheckBox":U, "25":U, "tt_auth_detail.pmb_indicator":U, "LOGICAL":U, 1, 1, "PMB Indicator:":U)
    oControl:CellSnippet               = "height='35px'":U
    
    oControl                           = opoContainer:addControl("flPmbPayCost":U   + ipcContainername, "wsCheckBox":U, "25":U, "tt_auth_detail.pmb_pay_cost":U , "LOGICAL":U, 1, 2, "PMB Pay Cost:":U)
    
    oControl                           = opoContainer:addControl("fdPmbBenefit":U   + ipcContainername, "wsInput":U   , "5":U , "tt_auth_detail.pmb_benefit_%":U, "DECIMAL":U, 1, 3, "PMB Benefit%:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    
    oControl                           = opoContainer:addControl("fdPmbValue":U     + ipcContainername, "wsInput":U   , "5":U , "tt_auth_detail.pmb_value":U    , "DECIMAL":U, 1, 4, "PMB Value:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlToken              = "ReadOnly":U
    .                              
    
&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthPmb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthPmb Procedure 
PROCEDURE getCntUpdAuthPmb :
/*------------------------------------------------------------------------------
  Purpose   : Update provider container definition    
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
    opoContainer:ContainerTitle        = "PMB Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth NO-LOCK":U                                                                                                                                                                             
                                                                                                                                                                             
    oControl                           = opoContainer:addControl("flPmbIndicator":U + ipcContainername, "wsCheckBox":U, "25":U, "tt_auth.pmb_indicator":U, "LOGICAL":U, 1, 1, "PMB Indicator:":U)
    oControl:CellSnippet               = "height='35px'":U
    oControl:ControlTooltip            = "Please enter a valid a PMB Indicator.":U
    
    oControl                           = opoContainer:addControl("flPmbPayCost":U   + ipcContainername, "wsCheckBox":U, "25":U, "tt_auth.pmb_pay_cost":U , "LOGICAL":U, 1, 2, "PMB Pay Cost:":U)
    oControl:ControlTooltip            = "Please enter a valid a PMB Pay Cost.":U
    
    oControl                           = opoContainer:addControl("fdPmbBenefit":U   + ipcContainername, "wsInput":U   , "5":U , "tt_auth.pmb_benefit_%":U, "DECIMAL":U, 1, 3, "PMB Benefit%:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlTooltip            = "Please enter a valid a PMB Benefit %.":U
    
    oControl                           = opoContainer:addControl("fdPmbValue":U     + ipcContainername, "wsInput":U   , "5":U , "tt_auth.pmb_value":U    , "DECIMAL":U, 1, 4, "PMB Value:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    oControl:ControlTooltip            = "Please enter a valid a PMB Value.":U
    .                              
    

&ENDIF
    
  { mip/inc/mipcatcherror.i }  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCntUpdAuthProvPmb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCntUpdAuthProvPmb Procedure 
PROCEDURE getCntUpdAuthProvPmb :
/*------------------------------------------------------------------------------
  Purpose   : Update provider container definition    
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
    opoContainer:ContainerTitle        = "PMB Information":U
    opoContainer:ViewOnly              = FALSE
    opoContainer:ShowContainerSettings = FALSE
    opoContainer:Collapsable           = FALSE
    opoContainer:ContainerMode         = Warpspeed:SubmitValue
    opoContainer:QueryString           = "FOR EACH tt_auth_provider NO-LOCK":U                                                                                                                                                                             
                                                                                                                                                                             
    oControl                           = opoContainer:addControl("flPmbIndicator":U + ipcContainername, "wsCheckBox":U, "25":U, "tt_auth_provider.pmb_indicator":U, "LOGICAL":U, 1, 1, "PMB Indicator:":U)
    oControl:CellSnippet               = "height='35px'":U
    
    oControl                           = opoContainer:addControl("flPmbPayCost":U   + ipcContainername, "wsCheckBox":U, "25":U, "tt_auth_provider.pmb_pay_cost":U , "LOGICAL":U, 1, 2, "PMB Pay Cost:":U)
    
    oControl                           = opoContainer:addControl("fdPmbBenefit":U   + ipcContainername, "wsInput":U   , "5":U , "tt_auth_provider.pmb_benefit_%":U, "DECIMAL":U, 1, 3, "PMB Benefit%:":U)
    oControl:ControlClass              = "+clNumericOnly":U
    
    oControl                           = opoContainer:addControl("fdPmbValue":U     + ipcContainername, "wsInput":U   , "5":U , "tt_auth_provider.pmb_value":U    , "DECIMAL":U, 1, 4, "PMB Value:":U)
    oControl:ControlClass              = "+clNumericOnly":U
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
  DEFINE INPUT PARAMETER ipoControl  AS cls.mipwscontrol NO-UNDO.
  
  DEFINE VARIABLE cControlNameList   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRuleValue         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE dAuthObj           AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dInsurerObj        AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE tStartDate         AS DATE                NO-UNDO.
  DEFINE VARIABLE hBufferHandle      AS HANDLE              NO-UNDO.
  DEFINE VARIABLE iControl           AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iOptionCode        AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lSuccess           AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lValidRule         AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE oAuthorisation     AS cls.maauthorisation NO-UNDO.
  DEFINE VARIABLE oContainer         AS cls.mipwscontainer  NO-UNDO.
  DEFINE VARIABLE oControl           AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oQuery             AS cls.mipquery        NO-UNDO.
  DEFINE VARIABLE hQuery             AS HANDLE              NO-UNDO.
  DEFINE VARIABLE cBufferName        AS CHARACTER           NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN

  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).
   
  CASE ipoControl:RenderArgument:      
    
    WHEN "AuthHeaderPMBRender":U OR WHEN "DecisionPMBRender":U THEN 
    DO:
                                     
      ASSIGN oContainer = ipoControl:ParentContainer.
                                      
      /*
        Make sure we get the topmost container with a valid query object
      */
      FindSuitableContainerBlk:
      DO WHILE VALID-OBJECT(oContainer:ParentContainer):
        
        ASSIGN oContainer = oContainer:ParentContainer.
        
        /*
          If the container has a valid query then we have found what we are looking for
          and there is no need to go any further.
        */
        IF VALID-OBJECT(oContainer:ContainerQuery)
        THEN 
          LEAVE FindSuitableContainerBlk.
      END. /*DO WHILE VALID-OBJECT(ipoControl:ParentContainer):*/

      
      ASSIGN oQuery = oContainer:ContainerQuery.

      /*
          Whether we are dealing with tt_auth,tt_auth_provider or tt_auth_detail we should
          always have an auth_obj
      */

      IF VALID-HANDLE(hQuery)
      THEN 
        ASSIGN
          hBufferHandle = HANDLE(oQuery:GetBuffer(1))
          cBufferName   = hBufferHandle:NAME
          hBufferHandle = hQuery:GET-BUFFER-HANDLE(cBufferName)
          dAuthObj      = (IF VALID-HANDLE(hBufferHandle)
                                AND hBufferHandle:AVAILABLE
                                THEN hBufferHandle::auth_obj
                                ELSE 0.00).
     
      /*
        Focus the relevant authorisation and find the required rules using
        the authorisation start date, insurer and option code
      */
      IF dAuthObj <> ? AND dAuthObj <> 0.00 THEN
      DO:
        /*
          Focus auth
        */
  mipEnv:Health:AuthService:getAuthObject
    ( INPUT  dAuthObj,
      INPUT  "":U,
      OUTPUT oAuthorisation ).
       
      END. /* IF dAuthObj <> ? AND dAuthObj <> 0.00 THEN */

      ASSIGN
        dInsurerObj = IF lSuccess THEN oAuthorisation:InsurerObj       ELSE 0.00
        iOptionCode = IF lSuccess THEN oAuthorisation:MemberOptionCode ELSE 0
        tStartDate  = IF lSuccess THEN oAUthorisation:StartDate        ELSE TODAY .

        /*
          PMB Decision Rule
        */
        mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                       INPUT  iOptionCode,
                                                       INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                       INPUT  "PMBDecision":U,
                                                       INPUT  tStartDate,
                                                       OUTPUT lValidRule,
                                                       OUTPUT cRuleValue).
      
      /*
        We hide the whole container.
      */
      IF NOT lValidRule 
      THEN 
        ASSIGN ipoControl:SubContainer:ContainerHidden = TRUE.
      ELSE 
      DO:
        ASSIGN cControlNameList = ipoControl:SubContainer:getControlNameList().
        
        IF LOOKUP("System":U, cRuleValue) > 0 
        THEN 
        DO iControl = 1 TO NUM-ENTRIES(cControlNameList):

          ASSIGN 
            oControl              = ipoControl:SubContainer:getControl(ENTRY(iControl, cControlNameList))
            oControl:ControlToken = "Disabled":U.
          
        END. /* IF LOOKUP("System":U, cRuleValue) > 0 */
        
        IF LOOKUP("User":U, cRuleValue) > 0
        THEN 
        DO iControl = 1 TO NUM-ENTRIES(cControlNameList):

          ASSIGN oControl = ipoControl:SubContainer:getControl(ENTRY(iControl, cControlNameList)).

          /*
            Auth Header Financial - PMB
          */
          IF ipoControl:renderArgument = "AuthHeaderPMBRender":U THEN
          DO:
            
            /*
             PMB Header Financial Rule
            */
            mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                           INPUT  iOptionCode,
                                                           INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                                                           INPUT  "PMBHeaderFinancialInfo":U,
                                                           INPUT  tStartDate,
                                                           OUTPUT lValidRule,
                                                           OUTPUT cRuleValue).
            
            IF lValidRule THEN 
            DO:
              ASSIGN oControl:ControlToken = "Disabled":U.
            
            END. /* IF lValidRule THEN  */
            ELSE 
            DO:
              IF oControl:ControlName BEGINS "fdPmbValue":U
              THEN 
                ASSIGN oControl:ControlToken = "Disabled":U.
              ELSE
                ASSIGN oControl:ControlToken = "Updatable":U. 
            END. /* ELSE DO: */
          END. /* IF ipoControl:renderArgument = "AuthHeaderPMBRender":U THEN */
          ELSE 
          DO:
            IF oControl:ControlName BEGINS "fdPmbValue":U
            THEN 
              ASSIGN oControl:ControlToken = "Disabled":U.

          END. /* ELSE DO: */          
        END. /* IF LOOKUP("User":U, cRuleValue) > 0 */
        
        IF LOOKUP("None":U, cRuleValue) > 0
        THEN
          ASSIGN ipoControl:SubContainer:ContainerHidden = TRUE.
      END. /* ELSE DO: */

      ipoControl:render().
      
    END. /* WHEN "AuthPMBRender" THEN */

    OTHERWISE RUN SUPER(ipoControl).
  END. /*CASE ipoControl:RenderArgument:      */
    
  
  { mip/inc/mipcatcherror.i }
                
                
                
&ENDIF          
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

