/*------------------------------------------------------------------------------
  Purpose   : Auth ui service specific render procedure    
  Parameters: Control to be rendered
  Notes     :  
  Author    : Andrewd     
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipoControl AS cls.mipwscontrol NO-UNDO.
  
&IF {&DBDFMA} >= 010195 &THEN  
  
  DEFINE VARIABLE cAttachments       AS CHARACTER            NO-UNDO.  
  DEFINE VARIABLE cOnClick           AS CHARACTER            NO-UNDO.  
  DEFINE VARIABLE cContainerCode     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cContainerType     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cDiscipline        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cHeaderRuleValue   AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cHeaderStatuses    AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cItemList          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusList        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cMemNum            AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cOnchange          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cProviderRuleValue AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cProviderStatuses  AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cProviderType      AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleValue         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cSubDiscipline     AS CHARACTER            NO-UNDO. 
  DEFINE VARIABLE cStatusType        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusDesc        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iAuthStatus        AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iDependant         AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iCount             AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iNegNum            AS INTEGER              NO-UNDO.
  DEFINE VARIABLE dAuthObj           AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE lMandatory         AS LOGICAL              NO-UNDO. 
  DEFINE VARIABLE lSuccess           AS LOGICAL              NO-UNDO.    
  DEFINE VARIABLE lcLongchar         AS LONGCHAR             NO-UNDO.    
  DEFINE VARIABLE lValidRule         AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE oAuthorisation     AS cls.maauthorisation  NO-UNDO.
  DEFINE VARIABLE hBufferHandle      AS HANDLE               NO-UNDO.
  DEFINE VARIABLE hQuery             AS HANDLE               NO-UNDO.

  DEFINE BUFFER bdoctor           FOR doctor.
  DEFINE BUFFER buf_auth          FOR hat_auth.
  DEFINE BUFFER buf_auth_type     FOR hac_auth_type.
  DEFINE BUFFER buf_auth_provider FOR hat_auth_provider.

  ASSIGN hQuery = ipoControl:ParentContainer:ContainerQuery:QueryObject WHEN VALID-OBJECT(ipoControl:ParentContainer:ContainerQuery).
  
  CASE ipoControl:RenderArgument:      
  
    WHEN "ProviderName":U THEN
    DO:
      DATASET dsAuthorisation:EMPTY-DATASET().

      ASSIGN dAuthObj = hQuery:GET-BUFFER-HANDLE("tt_auth":U)::auth_obj WHEN VALID-HANDLE(hQuery).
      
      IF ipoControl:ControlValue <> "":U THEN 
      DO:
        FIND FIRST doctor NO-LOCK
             WHERE doctor.doc-num = INTEGER(ipoControl:ControlValue)             
          NO-ERROR.          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
        
        IF AVAILABLE doctor 
        THEN
          ASSIGN ipoControl:ControlValue = ipoControl:ControlValue + " - ":U + doctor.name.
      END. /*IF ipoControl:ControlValue <> "":U THEN */      
      
      IF CAN-FIND(FIRST buf_auth_provider
                  WHERE buf_auth_provider.auth_obj = dAuthObj)
      THEN DO:
        EMPTY TEMP-TABLE tt_auth_provider_str.

        FOR EACH buf_auth_provider NO-LOCK
           WHERE buf_auth_provider.auth_obj = dAuthObj
              BY buf_auth_provider.main_provider:

          FIND FIRST mic_acronym NO-LOCK
               WHERE mic_acronym.acronym_key = buf_auth_provider.provider_type
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }         

          CREATE tt_auth_provider_str.
          ASSIGN tt_auth_provider_str.main_provider      =  IF buf_auth_provider.main_provider
                                                            THEN "TRUE":U
                                                            ELSE "FALSE":U
                 tt_auth_provider_str.provider_type      =  IF AVAILABLE mic_acronym
                                                            THEN mic_acronym.acronym_label
                                                            ELSE buf_auth_provider.provider_type
                 tt_auth_provider_str.authorised_service =  IF buf_auth_provider.authorised_service
                                                            THEN "TRUE":U
                                                            ELSE "FALSE":U
                 tt_auth_provider_str.account_reference  =  buf_auth_provider.account_reference
                 tt_auth_provider_str.start_date         =  IF buf_auth_provider.start_date <> ?
                                                            THEN STRING(buf_auth_provider.start_date,"9999/99/99":U)
                                                            ELSE "":U
                 tt_auth_provider_str.end_date           =  IF buf_auth_provider.end_date <> ?
                                                            THEN STRING(buf_auth_provider.end_date,"9999/99/99":U)
                                                            ELSE "":U.

          IF buf_auth_provider.group_doc_num <> 0
          THEN DO:
            FIND FIRST doctor NO-LOCK
                 WHERE doctor.doc-num = buf_auth_provider.group_doc_num
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            IF AVAILABLE doctor
            THEN 
            FIND FIRST docdisp NO-LOCK
                 WHERE docdisp.doc-num = doctor.doc-num 
              NO-ERROR.

            { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

            ASSIGN tt_auth_provider_str.group_doc_num     = TRIM(STRING(buf_auth_provider.group_doc_num))
                   tt_auth_provider_str.group_pr_type     = IF AVAILABLE docdisp
                                                            THEN STRING(docdisp.disp-code,"999":U)
                                                            ELSE "":U
                   tt_auth_provider_str.group_sub_pr_type = IF AVAILABLE docdisp
                                                            THEN STRING(docdisp.subdisp-code,"999":U)
                                                            ELSE "":U
                   tt_auth_provider_str.group_name        = IF AVAILABLE doctor
                                                            THEN doctor.name
                                                            ELSE "":U
                   tt_auth_provider_str.doc_num           = TRIM(STRING(buf_auth_provider.doc_num))
                   tt_auth_provider_str.pr_type           = TRIM(STRING(buf_auth_provider.pr_type,"999":U))
                   tt_auth_provider_str.sub_pr_type       = TRIM(STRING(buf_auth_provider.sub_pr_type,"999":U)).

            IF tt_auth_provider_str.doc_num <> "" THEN 
            DO:
              FIND FIRST bdoctor NO-LOCK
                   WHERE bdoctor.doc-num = INTEGER(tt_auth_provider_str.doc_num) 
                NO-ERROR.

              { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

              IF AVAILABLE bdoctor
              THEN ASSIGN tt_auth_provider_str.doc_name = bdoctor.name.
            END.  /* IF tt_auth_provider_str.doc_num <> "" THEN  */
            ELSE ASSIGN tt_auth_provider_str.doc_name = "":U.
          END.
          ELSE DO:
             FIND FIRST doctor NO-LOCK
                  WHERE doctor.doc-num = INTEGER(tt_auth_provider_str.doc_num) 
               NO-ERROR.

             { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

             IF AVAILABLE doctor
             THEN ASSIGN tt_auth_provider_str.group_doc_num     = TRIM(STRING(buf_auth_provider.doc_num))
                         tt_auth_provider_str.group_pr_type     = TRIM(STRING(buf_auth_provider.pr_type,"999":U))
                         tt_auth_provider_str.group_sub_pr_type = TRIM(STRING(buf_auth_provider.sub_pr_type,"999":U))
                         tt_auth_provider_str.group_name        = doctor.name
                         tt_auth_provider_str.doc_num           = "":U
                         tt_auth_provider_str.pr_type           = "":U
                         tt_auth_provider_str.sub_pr_type       = "":U
                         tt_auth_provider_str.doc_name          = "":U.
          END.

          VALIDATE tt_auth_provider_str.

        END. /*FOR EACH buf_auth_provider NO-LOCK */

        TEMP-TABLE tt_auth_provider_str:WRITE-JSON("LONGCHAR":U, lcLongchar, FALSE, ?, FALSE, TRUE).

        ASSIGN cOnClick                     = "var vTableConfig = [~{fieldName : ~"main_provider~"      , fieldLabel : ~"Main Provider~"           ~},":U
                                            + "                    ~{fieldName : ~"provider_type~"      , fieldLabel : ~"Provider Type~"           ~},":U 
                                            + "                    ~{fieldName : ~"authorised_service~" , fieldLabel : ~"Authorised Service~"      ~},":U
                                            + "                    ~{fieldName : ~"group_doc_num~"      , fieldLabel : ~"Provider Number~"         ~},":U 
                                            + "                    ~{fieldName : ~"group_pr_type~"      , fieldLabel : ~"Discipline~"              ~},":U
                                            + "                    ~{fieldName : ~"group_sub_pr_type~"  , fieldLabel : ~"Sub Discipline~"          ~},":U 
                                            + "                    ~{fieldName : ~"group_name~"         , fieldLabel : ~"Provider Name~"           ~},":U
                                            + "                    ~{fieldName : ~"doc_num~"            , fieldLabel : ~"Attending Provider~"      ~},":U 
                                            + "                    ~{fieldName : ~"pr_type~"            , fieldLabel : ~"Attending Discipline~"    ~},":U 
                                            + "                    ~{fieldName : ~"sub_pr_type~"        , fieldLabel : ~"Attending Sub Discipline~"~},":U
                                            + "                    ~{fieldName : ~"doc_name~"           , fieldLabel : ~"Attending Name~"          ~},":U
                                            + "                    ~{fieldName : ~"account_reference~"  , fieldLabel : ~"Account Reference~"       ~},":U 
                                            + "                    ~{fieldName : ~"start_date~"         , fieldLabel : ~"Start Date~"              ~},":U 
                                            + "                    ~{fieldName : ~"end_date~"           , fieldLabel : ~"End Date~"               ~}],":U
                                            + "    vDataSource  = ":U + STRING(lcLongchar) + ";":U

                                            + "new wsModalDialog(~{ title : ~"Providers~", tableConfig : vTableConfig, dataSource : vDataSource ~});":U.

        IF ipoControl:ResolvedToken = "Updatable":U
        THEN
        {&OUT} "<a href='javascript:void(0);' onclick='event.preventDefault();" + cOnClick + "'>" + mipEnv:miUtility:encode-url(ipoControl:ControlValue, "HTML") + "</a>":U.

      END. /*IF CAN-FIND(FIRST buf_auth_provider) THEN*/
 
    END. /*WHEN "ProviderName" THEN*/
    
    WHEN "AuthResource":U THEN
    DO:
      /* 
        relationship mm_RelMmtrs2Hatau - should have mm document as owning and auth as related 
      */

      ASSIGN cAttachments = hQuery:GET-BUFFER-HANDLE("mit_relationship":U)::owning_obj WHEN VALID-HANDLE(hQuery).
      
      IF DECIMAL(cAttachments) <> 0 AND DECIMAL(cAttachments) <> ? THEN 
      DO:
        ASSIGN
          ipoControl:ButtonImageClass = "clmView":U
          ipoControl:Obj           = ENTRY(1, cAttachments)
          ipoControl:ControlTarget = "documentwindow"
          ipoControl:Wob           = "mmfetch"
          ipoControl:ControlValue  = ENTRY(1, cAttachments)
          lSuccess                 = ipoControl:renderAsHref().                            
          
      END. /*IF DECIMAL(cAttachments) <> 0 AND DECIMAL(cAttachments) <> ? THEN */      
    
    END. /* WHEN "AuthResource":U THEN */   
 
    WHEN "FullSystemStatusCombo" THEN
    DO: 
 
      IF ipoControl:ParentContainer:CurrentRow = 1 THEN
      DO:
        /* No need to build up a status list for each row, so we only do it for the first row. The subsequent rows will inherit the AdditionalItems property value */ 

        EMPTY TEMP-TABLE ttAuthStatus.
        
        mipEnv:Health:AuthService:getStatusTable(INPUT "System", OUTPUT TABLE ttAuthStatus).
        
        FOR EACH ttAuthStatus NO-LOCK BY ttAuthStatus.status_description:
        
          ASSIGN cStatusList = cStatusList
                             + (IF cStatusList = "":U THEN  "":U ELSE "|":U)
                             + ttAuthStatus.status_description + "=":U + STRING(ttAuthStatus.status_code).
        END. /*FOR EACH ttAuthStatus NO-LOCK BY ttAuthStatus.status_description:*/
        
        ASSIGN ipoControl:AdditionalItems = "=|":U + cStatusList.

      END. /* IF ipoControl:CurrentRow = 1 */


      ipoControl:RenderASComboOrSelect().
      
    END. /*WHEN "FullSystemStatusCombo" THEN*/
    
    WHEN "AuthGroupSelect":U THEN
    DO:
      EMPTY TEMP-TABLE tt_acronym.

      DATASET dsAuthType:EMPTY-DATASET().

      mipEnv:Health:maUtility:getAcronymDetails
        (INPUT "ma_acAuthTypeGroup":U,
         INPUT "":U,
         OUTPUT TABLE tt_acronym BY-REFERENCE).

      FOR EACH tt_acronym NO-LOCK:

        IF CAN-FIND(FIRST buf_auth_type NO-LOCK
                    WHERE buf_auth_type.auth_type_group = tt_acronym.acronym_key) 
        THEN
          ASSIGN cItemList = cItemList 
                           + (IF cItemList = "":U THEN "":U ELSE "|":U)
                           + tt_acronym.acronym_label + "=":U + tt_acronym.acronym_key.

      END. /* FOR EACH tt_acronym NO-LOCK */

      IF cItemList = "":U 
      THEN ASSIGN ipoControl:ControlToken    = "Disabled":U.   
      ELSE ASSIGN ipoControl:AdditionalItems = "<None>=|" + cItemList.

      ipoControl:RenderASComboOrSelect().

    END. /*WHEN "AuthGroupSelect":U THEN*/
    
    WHEN "AuthStatusCombo":U OR WHEN "AuthSystemStatusCombo":U THEN
    AUTH-STATUS-COMBO-BLK:
    DO:
    
      ASSIGN cContainerCode = ipoControl:ParentContainer:ContainerCode.
      
      /* 
         When this is > row 1 of the auth search results container, we will already have the additional items value populated 
         so there is no need to run prepareAuthStatusCombo() again to rebuild the addtional items value. 
      */ 
      IF  ipoControl:ParentContainer:CurrentRow > 1 
      AND cContainerCode = "AuthSearchContainer":U THEN 
      DO:
        /*
          Render the control and leave the render block
        */
        ipoControl:RenderASComboOrSelect().  
        
        LEAVE AUTH-STATUS-COMBO-BLK.                                                                        
      END. /*  IF  ipoControl:ParentContainer:CurrentRow > 1 AND cContainerCode = "AuthSearchContainer":U */
      
      mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                     INPUT  0,
                                                     INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                     INPUT  "UpdateConfirmation":U, 
                                                     INPUT  TODAY,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue).


      mipEnv:Health:maUIService:prepareAuthStatusCombo(INPUT-OUTPUT ipoControl, INPUT (IF ipoControl:RenderArgument = "AuthSystemStatusCombo":U THEN "System":U ELSE "":U)).

      CASE cContainerCode:
      
        WHEN "ProviderContainer":U OR WHEN "ProviderForm":U THEN
        DO:           
          /* 
            We need to apply this rule to specific containers if run in a stack the container code could potentially be different  
          */
          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                         INPUT  0,
                                                         INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                         INPUT  "DefaultProvider":U,
                                                         INPUT  TODAY,
                                                         OUTPUT lValidRule,
                                                         OUTPUT cProviderRuleValue).

          ASSIGN dAuthObj = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)::auth_obj WHEN VALID-HANDLE(hQuery).
          
          IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN
          DO:
            mipEnv:Health:AuthService:getAuthObject
              ( INPUT  dAuthObj,
                INPUT  "":U,
                OUTPUT oAuthorisation ).
            
            EMPTY TEMP-TABLE ttAuthTypeConfig.              

            IF VALID-HANDLE(hQuery)
            THEN
              ASSIGN 
                hBufferHandle  = hQuery:GET-BUFFER-HANDLE("tt_auth_provider":U)
                cProviderType  = hBufferHandle::provider_type
                iNegNum        = INTEGER(TRIM(ENTRY(1, hBufferHandle::_neg_group, ":"), "("))
                cDiscipline    = hBufferHandle::pr_type
                cSubDiscipline = hBufferHandle::sub_pr_type.

            mipEnv:Health:AuthService:getAuthTypeConfig(INPUT oAuthorisation:AuthTypeObj,                       
                                                        INPUT oAuthorisation:InsurerObj,                        
                                                        INPUT oAuthorisation:MemberOptionCode,                        
                                                        INPUT oAuthorisation:StartDate,     
                                                        INPUT cProviderType,    
                                                        INPUT iNegNum ,
                                                        INPUT cDiscipline,                                                                     
                                                        INPUT cSubDiscipline,                                                                  
                                                        INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).  
            
            FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
                                                                                         
            { mip/inc/mipthrowerror.i &IgnoreErrors= 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
            
            ASSIGN ipoControl:ControlToken = (IF AVAILABLE ttAuthTypeConfig AND ttAuthTypeConfig.DefaultAuthStatusUpdAllowed = FALSE THEN "Disabled":U ELSE "Updatable":U).
          
          END. /*IF dAuthObj <> 0.00 AND dAuthObj <> ? THEN*/
          
          IF LOOKUP("Provider", cRuleValue) > 0 OR cProviderRuleValue <> "" THEN
          DO:
            
            DO iCount = 1 TO NUM-ENTRIES(cProviderRuleValue):
              
              ASSIGN cProviderStatuses = TRIM(cProviderStatuses) + TRIM(SUBSTRING(ENTRY(iCount, cProviderRuleValue),1,1)) + ",":U.
            END. /*DO iCount = 1 TO NUM-ENTRIES(cProviderRuleValue):*/
            
            ASSIGN cContainerType                = IF cContainerCode = "ProviderContainer":U THEN "table":U ELSE "form":U

                   cProviderStatuses             = TRIM(cProviderStatuses, ",":U)
                   
                   cOnchange                     = "var cStatusList = ~"" + cProviderStatuses + "~";":U
                                                 + " if(cStatusList.indexOf(this.value) >= 0)~{":U
                                                 + "   fnAuthStatusNotification(this,~"fdAuthProviderObj~",~"" + cContainerCode + "~",~"" + cContainerType + "~" ); ":U
                                                 + " ~}":U
                                                 + " if(glSaveCancel)~{ return false; ~}":U
                                                 
                   ipoControl:JavascriptOnBlur   = "if(glSaveCancel)~{ glSaveCancel = false; return false; ~}":U 
                                                 + ipoControl:JavascriptOnBlur 
                   
                   ipoControl:JavascriptOnChange = ipoControl:JavascriptOnChange 
                                                 + (IF cProviderRuleValue <> "":U 
                                                    THEN cOnchange 
                                                    ELSE "fnAuthStatusNotification(this,~"fdAuthProviderObj~",~"" + cContainerCode + "~",~"" + cContainerType + "~" );if(glSaveCancel)~{ glSaveCancel = false; return false; ~}":U).          
          END. /*IF LOOKUP("Provider", cRuleValue) > 0 OR cProviderRuleValue <> "" THEN*/
        END. /*WHEN "ProviderContainer":U THEN*/
        
        WHEN "AuthContainer":U THEN
        DO:
          
          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0.00,
                                                         INPUT  0,
                                                         INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                         INPUT  "DefaultAuthHeadInfo":U,
                                                         INPUT  TODAY,
                                                         OUTPUT lValidRule,
                                                         OUTPUT cHeaderRuleValue).
          
          IF LOOKUP("HeaderInformation", cRuleValue) > 0 OR cHeaderRuleValue <> "" THEN
          DO:
          
            DO iCount = 1 TO NUM-ENTRIES(cHeaderRuleValue):
              
              ASSIGN cHeaderStatuses = TRIM(cHeaderStatuses) + TRIM(SUBSTRING(ENTRY(iCount, cHeaderRuleValue),1,1)) + ",".
            END. /*DO iCount = 1 TO NUM-ENTRIES(cHeaderRuleValue):*/
          
            ASSIGN cHeaderStatuses                  = TRIM(cHeaderStatuses, ",")
            
                   cOnchange                        = "var cStatusList = ~"" + cHeaderStatuses + "~";":U
                                                    + " if(cStatusList.indexOf(this.value) > -1)~{":U
                                                    + "   fnAuthStatusNotification(this,~"_authObj~",~"" + cContainerCode  + "~",~"form~" ); ":U
                                                    + " ~}":U
                                                    + " if(glSaveCancel)~{return true;~}":U
                                                    
                   ipoControl:JavascriptOnBlur      = "if(glSaveCancel)~{ glSaveCancel = false; return false; ~}":U 
                                                    + ipoControl:JavascriptOnBlur 
                                                    
                   ipoControl:JavascriptOnChange    = ipoControl:JavascriptOnChange 
                                                    + (IF cHeaderRuleValue <> "":U 
                                                       THEN cOnchange 
                                                       ELSE "fnAuthStatusNotification(this,~"_authObj~",~"" + cContainerCode + "~",~"form~" ); if(glSaveCancel)~{ glSaveCancel = false; return false; ~}":U).
          END. /*IF LOOKUP("HeaderInformation", cRuleValue) > 0 OR cHeaderRuleValue <> "" THEN*/
        END. /*WHEN "AuthContainer":U THEN*/
        
        WHEN "CPTCodingContainer":U OR WHEN "ICDCodingContainer":U THEN
        DO:
          
          IF LOOKUP("Coding":U, cRuleValue) > 0 THEN
          DO:
            ASSIGN
              ipoControl:JavascriptOnBlur    = " if(glSaveCancel)~{ glSaveCancel = false; return false; ~}":U 
                                             + ipoControl:JavascriptOnBlur 
                                             
              ipoControl:JavascriptOnChange  = "fnAuthStatusNotification(this,~"fdAuthCodingObj~",~"" + cContainerCode + "~",~"table~" );":U 
                                             + " if(glSaveCancel)~{ return false; ~} "
                                             + ipoControl:JavascriptOnChange.
                                             
          END. /*IF LOOKUP("Coding", cRuleValue) > 0 THEN*/
        END. /*WHEN "CPTCodingContainer":U OR WHEN "ICDCodingContainer":U THEN*/

        WHEN "DetailContainer":U OR WHEN "DetailForm":U THEN 
        DO:
          ASSIGN
            cContainerType                 = IF cContainerCode = "DetailContainer":U THEN "table":U ELSE "form":U
            
            ipoControl:JavascriptOnBlur    = " if(glSaveCancel)~{ glSaveCancel = false; return false; ~}":U 
                                           + ipoControl:JavascriptOnBlur 
                                           
            ipoControl:JavascriptOnChange  = "fnAuthStatusNotification(this,~"fdAuthDetailObj~",~"" + cContainerCode + "~",~"" + cContainerType + "~" );":U 
                                           + " if(glSaveCancel)~{ return false; ~} "
                                           + ipoControl:JavascriptOnChange.
          
        END. /* WHEN "DetailContainer":U OR WHEN "DetailForm":U THEN */

      END CASE.
      
      ipoControl:RenderASComboOrSelect().                                                                          
    END. /*WHEN "AuthStatusCombo":U OR WHEN "AuthSystemStatusCombo":U THEN*/

    /* NB ...  "AuthReasonType"
       This render argument is used across Auth header, provider,coding and detail containers when the
       reason type field is rendered. Be very careful when changing this as it will impact all the containers.
    */
    WHEN "AuthReasonType":U THEN 
    DO:

      ASSIGN hBufferHandle       = HANDLE(ENTRY(1 ,ipoControl:ParentContainer:QueryBufferList)) NO-ERROR.
	
	  ASSIGN hBufferHandle  = hQuery:GET-BUFFER-HANDLE(hBufferHandle:NAME) NO-ERROR . /*sometimes the querybufferlist has entries of memory addresses
	                                                                                  , if the entry is an addres we then get its name and use the name 
																					  to get the buffer handle in the query object*/
	  ASSIGN 
	    cStatusType             = hBufferHandle::_reason_type WHEN hBufferHandle:AVAILABLE
        ipoControl:ControlValue = cStatusType NO-ERROR.
	   
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthReasonType":U */

    WHEN "AuthStatusReason":U THEN 
    DO:
      ASSIGN cContainerCode = ipoControl:ParentContainer:ContainerCode.

      ASSIGN 
        hBufferHandle = HANDLE(ENTRY(1 ,ipoControl:ParentContainer:QueryBufferList)) NO-ERROR. 
		
	  /*
        Sometimes the querybufferlist has entries of memory addresses,
	    if the entry is an addres we then get its name and use the name 
		to get the buffer handle in the query object
      */
      ASSIGN 
        hBufferHandle  = hQuery:GET-BUFFER-HANDLE(hBufferHandle:NAME) NO-ERROR . 

      CASE cContainerCode:
        WHEN "ProviderContainer":U OR WHEN "ProviderForm":U THEN
          ASSIGN 
            lMandatory = hBufferHandle::_status_note_mandatory WHEN hBufferHandle:AVAILABLE.  
        WHEN "AuthContainer":U THEN
          ASSIGN 
            lMandatory = hBufferHandle::_reason_mandatory WHEN hBufferHandle:AVAILABLE.  
      END CASE. 

      ASSIGN 
	   cStatusDesc = hBufferHandle::_note_narration WHEN hBufferHandle:AVAILABLE
       ipoControl:ControlTooltip = cStatusDesc NO-ERROR.

      ASSIGN ipoControl:ControlToken = (IF lMandatory THEN "Updatable":U ELSE "Disabled":U).
                                 
      ipoControl:RenderAsInput().
    END. /* WHEN "AuthReasonType":U */
  END CASE. /* CASE ipoControl:RenderArgument: */
    
  { mip/inc/mipcatcherror.i 
    &FINALLY = "DATASET dsAuthorisation:EMPTY-DATASET().

                EMPTY TEMP-TABLE ttAuthTypeConfig.
                EMPTY TEMP-TABLE tt_acronym."}

&ENDIF
