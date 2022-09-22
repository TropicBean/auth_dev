/*------------------------------------------------------------------------------
  maauthinforenderprocedure.i  MEDSTAR Medical Aid System
                               Auth UI Service Render Procedure include
                               (c) Copyright 2018 - 2020
                               MIP Holdings (Pty) Ltd
                               All rights reserved   

  Author    : MMP     
------------------------------------------------------------------------------*/  
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN  

  DEFINE VARIABLE dAuthObj                AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj            AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dInsurerObj             AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE cMemNum                 AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusDesc             AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusType             AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iOptionCode             AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dStartDate              AS DATE                 NO-UNDO.
  DEFINE VARIABLE lAuthNumViewable        AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lIncomplete             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lMandatory              AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lRefNumUpdatable        AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lServiceTypeUpdatable   AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSuccess                AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lUpdatable              AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lStartDateAmPm          AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lEndDateAmPm            AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lcConfiguration         AS LONGCHAR             NO-UNDO.
  DEFINE VARIABLE oAuthorisation          AS cls.maauthorisation  NO-UNDO.
  DEFINE VARIABLE hBufferHandle           AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hQuery                  AS HANDLE               NO-UNDO.
  DEFINE VARIABLE lStandardValuesAssigned AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lAuthDependantUpdatable AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE cDependantList          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iDependant              AS INTEGER              NO-UNDO.
  DEFINE VARIABLE cDependantDOB           AS CHARACTER            NO-UNDO FORMAT "9999/99/99".

  DEFINE BUFFER btt_auth FOR tt_auth.
  
  &SCOPED-DEFINE DOBValues IF VALID-HANDLE(hQuery) AND VALID-HANDLE(hQuery:GET-BUFFER-HANDLE("tt_auth":U)) ~
                           AND hQuery:GET-BUFFER-HANDLE("tt_auth":U):AVAILABLE                             ~
                           THEN                                                                            ~
                             ASSIGN                                                                        ~
                               hBufferHandle           = hQuery:GET-BUFFER-HANDLE("tt_auth":U)             ~
                               cMemNum                 = hBufferHandle::mem_num                            ~
                               iDependant              = hBufferHandle::dependant                          ~
                               dStartDate              = hBufferHandle::start_date                         ~
                               dStartDate              = (IF dStartDate = ? THEN TODAY ELSE dStartDate).   ~

  &SCOPED-DEFINE StandardValues IF VALID-HANDLE(hQuery) AND VALID-HANDLE(hQuery:GET-BUFFER-HANDLE("tt_auth":U)) ~
                                AND hQuery:GET-BUFFER-HANDLE("tt_auth":U):AVAILABLE                             ~
                                THEN                                                                            ~
                                  ASSIGN                                                                        ~
                                    hBufferHandle           = hQuery:GET-BUFFER-HANDLE("tt_auth":U)             ~
                                    cMemNum                 = hBufferHandle::mem_num                            ~
                                    dAuthObj                = hBufferHandle::auth_obj                           ~
                                    dInsurerObj             = hBufferHandle::insurer_obj                        ~
                                    dAuthTypeObj            = hBufferHandle::auth_type_obj                      ~
                                    iOptionCode             = hBufferHandle::option_code                        ~
                                    dStartDate              = hBufferHandle::start_date                         ~
                                    dStartDate              = (IF dStartDate = ? THEN TODAY ELSE dStartDate)    ~
                                    lStandardValuesAssigned = TRUE.                                             ~
                                ELSE                                                                            ~
                                  ASSIGN                                                                        ~
                                    lStandardValuesAssigned = FALSE.
  
  &SCOPED-DEFINE FindAuthTypeConfig   mipEnv:Health:AuthService:getAuthTypeConfig  ~
                                        (INPUT dAuthTypeObj,                       ~
                                         INPUT dInsurerObj,                        ~
                                         INPUT iOptionCode,                        ~
                                         INPUT dStartDate,                         ~
                                         INPUT-OUTPUT TABLE ttAuthTypeConfig).     ~
                                                                                   ~
                                      FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.~
                                                                                   ~
                                      cls.miperror:resetError().
  EMPTY TEMP-TABLE ttAuthTypeConfig.

  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).
  
  CASE ipoControl:RenderArgument:
  
    WHEN "AuthEndDate":U THEN
    DO:
      {&StandardValues}
      
      IF lStandardValuesAssigned
      THEN DO:
        {&FindAuthTypeConfig}
        
        IF AVAILABLE ttAuthTypeConfig 
        THEN 
          ASSIGN ipoControl:ControlToken = (IF ttAuthTypeConfig.EndDateUpdAllowed 
                                            THEN "Updatable":U 
                                            ELSE "Disabled":U).
        
      END. /* IF lStandardValuesAssigned */
      
      IF ipoControl:ControlType = "wsCombo":U 
      THEN 
        ipoControl:RenderAsComboOrSelect().
      ELSE 
        ipoControl:RenderASInput().

    END. /*WHEN "AuthEndDate":U THEN*/
    
    WHEN "RefNumUpdatable":U THEN
    DO:
      {&StandardValues}

      IF lStandardValuesAssigned
      THEN
        ASSIGN 
          lRefNumUpdatable        = hBufferHandle::_refnum_updatable WHEN VALID-HANDLE(hQuery)
          ipoControl:ControlToken = IF lRefNumUpdatable THEN "Updatable":U ELSE "Disabled":U.
      
      ipoControl:RenderASInput().                                  
      
    END. /*WHEN "RefNumUpdatable":U THEN*/
    WHEN "ReasonMandatory":U THEN
    DO:
      {&StandardValues}
      
      IF lStandardValuesAssigned
      THEN
        ASSIGN ipoControl:ControlValue = hBufferHandle::_reason_mandatory WHEN VALID-HANDLE(hQuery).
      
      ipoControl:RenderASInput().
    END. /*WHEN "ReasonMandatory":U THEN*/
    
    WHEN "AuthTypeRender":U THEN
    DO:
      IF VALID-HANDLE(hQuery) AND hQuery:GET-BUFFER-HANDLE("tt_auth":U):AVAILABLE
      THEN
        ASSIGN
          dAuthObj = hQuery:GET-BUFFER-HANDLE("tt_auth":U)::auth_obj.
      
      IF ipoControl:ControlType = "wsInput":U THEN 
      DO:
        IF dAuthObj > 0.00 AND dAuthObj <> ?
        THEN 
            ASSIGN 
              ipoControl:ControlToken = "ReadOnly":U
              ipoControl:ControlClass = ipoControl:ControlClass + " -clMan":U.
        
        ipoControl:RenderASInput().
      END. /*IF ipoControl:ControlType = 'wsInput' AND dAuthObj <= 0.00 THEN */
      
      IF ipoControl:ControlType = "wsLookupButton":U AND (dAuthObj <= 0.00 OR dAuthObj = ?)
      THEN 
        ipoControl:RenderASLookupButton().
      
    END. /*WHEN "AuthTypeRender":U THEN*/
  
    WHEN "DateAmPm" THEN
    DO:
      {&StandardValues}

      IF VALID-HANDLE(hQuery) AND hQuery:GET-BUFFER-HANDLE("tt_auth":U):AVAILABLE
      THEN
        ASSIGN
          dAuthObj        = hQuery:GET-BUFFER-HANDLE("tt_auth":U)::auth_obj
          lStartDateAmPm  = LOGICAL(hQuery:GET-BUFFER-HANDLE("tt_auth":U)::start_ampm)
          lEndDateAmPm    = LOGICAL(hQuery:GET-BUFFER-HANDLE("tt_auth":U)::end_ampm) .
              
      IF dAuthObj > 0.00 AND dAuthObj <> ? THEN
      DO:
        IF ipoControl:ControlName Matches "cbStartAmPm*":U 
        THEN DO:
          ASSIGN ipoControl:ControlValue = IF lStartDateAmPm THEN "AM" ELSE IF NOT lStartDateAmPm THEN "PM":U ELSE ?.
        END.
        ELSE DO: 
          ASSIGN ipoControl:ControlValue = IF lEndDateAmPm THEN "AM" ELSE IF NOT lEndDateAmPm THEN "PM":U ELSE ?.
        END.
      END.

      ipoControl:renderASComboOrSelect().
    END. /* WHEN "DateAmPm" THEN */


    WHEN "AuthIncomplete":U THEN 
    DO:
      IF VALID-HANDLE(hQuery)
      AND hQuery:GET-BUFFER-HANDLE("tt_auth":U):AVAILABLE 
      AND hQuery:GET-BUFFER-HANDLE("tt_auth":U)::auth_incomplete = TRUE
      THEN
        ASSIGN
          ipoControl:ControlToken = "ReadOnly":U 
          ipoControl:ControlValue = "AUTHORISATION INCOMPLETE":U.
      ELSE 
        ASSIGN 
          ipoControl:ControlToken = "Hidden":U.
      
      ipoControl:RenderAsInput().
    
    END. /* WHEN "AuthIncomplete":U THEN  */ 

    WHEN "DOBDisplay" THEN
    DO:
      {&DOBValues}
      
      IF ipoControl:ControlValue = "All":U OR ipoControl:ControlValue = "99":U THEN
        ASSIGN cDependantDOB = "".
      ELSE
      DO:
        FIND FIRST memdep NO-LOCK
          WHERE memdep.mem-num = cMemNum
            AND memdep.dependant = iDependant NO-ERROR.

        IF AVAILABLE memdep THEN
          ASSIGN cDependantDOB = STRING(STRING(YEAR(memdep.birth-date),"9999") + "/" + STRING(MONTH(memdep.birth-date),"99") + "/" + STRING(DAY(memdep.birth-date),"99")).
      END.
      ipoControl:ControlValue = cDependantDOB.
      ipoControl:RenderAsInput().
    END.

    WHEN "DependantList":U THEN 
    DO:
      {&StandardValues}
      
      /*No need to do this check if we are adding*/
      IF lStandardValuesAssigned
      THEN       
        ASSIGN lAuthDependantUpdatable = hBufferHandle::_auth_dependant_updatable.
	  ELSE	  
	    ASSIGN dAuthObj = 0.
		
	  ASSIGN lUpdatable = lAuthDependantUpdatable OR dAuthObj <= 0.
		 
		/* 
          If we couldn't get the member number from our tt_auth record, it must mean that we're dealing with a new auth. 
		  So now we need to try and retrieve the member number from the member that was searched through the member menu.
		 */		  
      IF cMemNum = "":U 
      THEN 
		ASSIGN cMemNum    = wsUIService:getObj("mamqc":U).
		      
      mipEnv:Health:maMember:getAllDependants(INPUT cMemNum, OUTPUT TABLE tt_activeDep BY-REFERENCE). 

      FOR EACH tt_activeDep:
   	
		ASSIGN
		  cDependantList = (IF cDependantList = "":U 
		                    THEN cDependantList + STRING(tt_activeDep.Dependant) + "-":U + tt_activeDep.first-name + "=":U + STRING(tt_activeDep.Dependant)
			                ELSE cDependantList + "|":U + STRING(tt_activeDep.Dependant) + "-":U + tt_activeDep.first-name + "=":U + STRING(tt_activeDep.Dependant)).
		
	  END. /* FOR EACH tt_activeDep NO-LOCK */ 
	  	  	  
      IF lAuthDependantUpdatable = TRUE 
	  THEN 
        ASSIGN 	
          ipoControl:AdditionalItems = "All=":U + "|":U + cDependantList + "|":U + "99-Unknown=99":U.
		
      IF lAuthDependantUpdatable = FALSE 
		THEN 
	      ASSIGN
            ipoControl:AdditionalItems = "All=":U + "|":U + cDependantList + "|":U + "99-Unknown=99":U.
			
	  ASSIGN ipoControl:ControlToken = IF lUpdatable THEN "Updatable":U ELSE "Disabled":U.
		
      EMPTY TEMP-TABLE tt_activeDep.
    
      ipoControl:renderAsComboOrSelect().
    
    END. /* WHEN "DependantList":U THEN  */
    
    WHEN "OptionCodeRender":U THEN 
    DO:
      {&StandardValues}
      
      CASE ipoControl:ControlType:
        WHEN "wsLookupButton":U THEN ASSIGN lSuccess = ipoControl:renderAsLookupButton().
        WHEN "wsInput":U        THEN ASSIGN lSuccess = ipoControl:renderAsInput().  
      END CASE.
    
    END. /* WHEN "OptionCodeRender":U THEN  */
   
    WHEN "AuthNumViewableMaint":U THEN
    DO:
      {&StandardValues}

      IF lStandardValuesAssigned
      THEN
        ASSIGN
          lAuthNumViewable = hQuery:GET-BUFFER-HANDLE("tt_auth":U)::_authnum_viewable.
      
      IF lStandardValuesAssigned
      THEN DO:
        mipEnv:Health:AuthService:getAuthObject
          ( INPUT  dAuthObj,
            INPUT  "":U,
            OUTPUT oAuthorisation ).
        
        IF lAuthNumViewable AND oAuthorisation:InFocus
        THEN 
            ASSIGN ipoControl:ControlValue = oAuthorisation:AuthNum.
        ELSE 
            ASSIGN ipoControl:ControlValue = "********":U.  

      END. /* IF lStandardValuesAssigned */
      ELSE
        ASSIGN ipoControl:ControlValue = "[Auto-Generate]":U.

      ASSIGN
        ipoControl:ControlToken = "ReadOnly":U
        lSuccess                = ipoControl:renderAsInput().
    END. /*WHEN "AuthNumViewableMaint" THEN*/
    
    WHEN "AuthMode":U THEN 
    DO:
      IF lStandardValuesAssigned
      THEN
        ASSIGN
          lIncomplete = hQuery:GET-BUFFER-HANDLE("tt_auth":U)::auth_incomplete.
    
      ASSIGN ipoControl:ControlValue = (IF lIncomplete = ? OR lIncomplete = TRUE THEN "NEW":U ELSE "UPDATE":U).
                      
      ipoControl:RenderASInput().
    END. /*WHEN "AuthMode":U THEN */
    
    WHEN "AuthStatusNote":U THEN 
    DO:
      {&StandardValues}

      /* Check if this field should be mandatory or not */
      IF lStandardValuesAssigned
      THEN
        ASSIGN
          cStatusDesc               = hBufferHandle::_note_narration
          lMandatory                = hBufferHandle::_reason_mandatory
          ipoControl:ControlClass   = ipoControl:ControlClass 
                                    + (IF lMandatory THEN " +":U ELSE " -":U)
                                    + "clMan":U
          ipoControl:ControlToolTip = cStatusDesc
          ipoControl:ControlToken   = (IF lMandatory THEN "Updatable":U ELSE "Disabled":U).
      
      /*
        Render control
      */
      IF ipoControl:ControlType = 'wsLookupButton':U
      THEN 
        ipoControl:RenderASLookupButton().
      ELSE
        ipoControl:RenderASInput().
    END. /*WHEN "AuthStatusNote":U THEN */
    
    WHEN "AuthConfiguration":U THEN 
    DO:
      {&StandardValues}
      
      IF lStandardValuesAssigned
      THEN DO:
        RUN _getConfiguration IN TARGET-PROCEDURE ( INPUT  dAuthTypeObj, 
                                                    INPUT  dInsurerObj,
                                                    INPUT  iOptionCode,
                                                    INPUT  dStartDate,
                                                    OUTPUT lcConfiguration).
                                                    
        ASSIGN ipoControl:ControlValue = STRING(lcConfiguration).                                            
      END. /* IF lStandardValuesAssigned */
                                                            
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthConfiguration":U */
    
    WHEN "ServiceType":U THEN
    DO:
      {&StandardValues}

      IF lStandardValuesAssigned
      THEN DO:
        {&FindAuthTypeConfig}

        IF AVAILABLE ttAuthTypeConfig THEN
        DO:
          IF ttAuthTypeConfig.ActivateServiceType THEN
          DO:
            ASSIGN 
              lSuccess  = mipEnv:Health:AuthBusinessLogic:authServiceTypeUpdatable(INPUT  dInsurerObj,
                                                                                   INPUT  iOptionCode,
                                                                                   INPUT  dStartDate,
                                                                                   OUTPUT lServiceTypeUpdatable)

              ipoControl:ControlToken = IF lServiceTypeUpdatable THEN "Updatable":U           ELSE "Disabled":U 
              ipoControl:ControlClass = IF lServiceTypeUpdatable THEN ipoControl:ControlClass ELSE "+clDisabled":U . /* apply the grey colour */ 

          END. /* IF ttAuthTypeConfig.ActivateServiceType  */
          ELSE
            ASSIGN ipoControl:ControlToken = "Hidden":U .

        END. /* IF AVAILABLE ttAuthTypeConfig */
      END. /* IF lStandardValuesAssigned */

      ipoControl:renderAsComboOrSelect(). 
    END. /* WHEN "ServiceType" */

    WHEN "DependantReference":U THEN
    DO:
	  {&StandardValues}
      
      IF lStandardValuesAssigned
      THEN DO:
	    ASSIGN
	      lUpdatable = hBufferHandle::_auth_dependant_updatable
		  iDependant = hBufferHandle::dependant.
		  
	    IF iDependant = 99 
        THEN
          ASSIGN
            ipoControl:ControlToken = "Updatable":U.
        ELSE
          ASSIGN
            ipoControl:ControlToken = "Disabled".
			
      END. /* IF lStandardValuesAssigned */ 
      ELSE 
        ASSIGN 
          ipoControl:ControlToken = "Disabled".
      
      ipoControl:RenderASInput().
      
    END. /* DependantReference */


    OTHERWISE RUN SUPER(ipoControl).
  END. /*CASE ipoControl:RenderArgument:      */
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "EMPTY TEMP-TABLE tt_acronym.
                
                DATASET dsAuthType:EMPTY-DATASET().
                
                EMPTY TEMP-TABLE ttAuthTypeConfig."}                
&ENDIF
