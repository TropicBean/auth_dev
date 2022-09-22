/* maauthtyperowrender.i MEDSTAR Medical Aid System
                            Save Authorisation Detail Record
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/ 

  DEFINE VARIABLE oMaleAgeRange        AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oMaleAgeRangeBtn     AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oFemaleAgeRange      AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oFemaleAgeRangeBtn   AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oBothAgeRange        AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oBothAgeRangeBtn     AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oDefaultStatusReason AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oDefaultStatus       AS cls.mipwscontrol NO-UNDO.
  DEFINE VARIABLE oDefaultStatusBtn    AS cls.mipwscontrol NO-UNDO.

  DEFINE VARIABLE cGender                      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cControlTypeIndicator        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProviderTypeIndicator       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDetailTypeIndicator         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNoteDescription             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNoteCode                    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount                       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dInsurerObj                  AS DECIMAL   NO-UNDO.      
  DEFINE VARIABLE iOption                      AS INTEGER   NO-UNDO.      
  DEFINE VARIABLE dStartDate                   AS DATE      NO-UNDO.      
  DEFINE VARIABLE iStatus                      AS INTEGER   NO-UNDO.      
  DEFINE VARIABLE cStatusNote                  AS CHARACTER NO-UNDO.      
  DEFINE VARIABLE cStatusReasonType            AS CHARACTER NO-UNDO.      
  DEFINE VARIABLE lNotePerAuthStatusValidRule  AS LOGICAL   NO-UNDO.      
  DEFINE VARIABLE cStatusDescription           AS CHARACTER NO-UNDO.      
  DEFINE VARIABLE cErrorMessage                AS CHARACTER NO-UNDO.      


  
  RUN SUPER (INPUT ipoContainer).
  
  CASE ipoContainer:RowRenderArgument:
  


    WHEN "AuthTypeControls":U THEN
    DO:

      ASSIGN
        cGender               = ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_control.gender":U, "BUFFER-VALUE":U)
        oBothAgeRange         = ipoContainer:getControl("fcAgeRangeBoth":U        + ipoContainer:ContainerCode)
        oBothAgeRangeBtn      = ipoContainer:getControl("buAgeRangeBothBtn":U     + ipoContainer:ContainerCode)    
        oMaleAgeRange         = ipoContainer:getControl("fcAgeRangeMale":U        + ipoContainer:ContainerCode)
        oMaleAgeRangeBtn      = ipoContainer:getControl("buAgeRangeMaleBtn":U     + ipoContainer:ContainerCode)   
        oFemaleAgeRange       = ipoContainer:getControl("fcAgeRangeFemale":U      + ipoContainer:ContainerCode)
        oFemaleAgeRangeBtn    = ipoContainer:getControl("buAgeRangeFemaleBtn":U   + ipoContainer:ContainerCode)
        oDefaultStatusReason  = ipoContainer:getControl("fcReasonKey":U           + ipoContainer:ContainerCode)  
        oDefaultStatus        = ipoContainer:getControl("fcDefaultAuthStatus":U   + ipoContainer:ContainerCode)
        oDefaultStatusBtn     = ipoContainer:getControl("fcStatusReasonBtn":U     + ipoContainer:ContainerCode)
          . 
        
      CASE cGender :
        WHEN "M":U THEN 
        DO:
          ASSIGN oMaleAgeRange:ControlToken      = "updatable":U
                 oMaleAgeRangeBtn:ControlToken   = "updatable":U
                 oBothAgeRange:ControlToken      = "disabled":U
                 oBothAgeRangeBtn:ControlToken   = "disabled":U
                 oFemaleAgeRange:ControlToken    = "disabled":U
                 oFemaleAgeRangeBtn:ControlToken = "disabled":U .
        END. /* WHEN "M":U */
        WHEN "F":U THEN 
        DO:
          ASSIGN oFemaleAgeRange:ControlToken    = "updatable":U
                 oFemaleAgeRangeBtn:ControlToken = "updatable":U
                 oBothAgeRange:ControlToken      = "disabled":U
                 oBothAgeRangeBtn:ControlToken   = "disabled":U
                 oMaleAgeRange:ControlToken      = "disabled":U
                 oMaleAgeRangeBtn:ControlToken   = "disabled":U.
        END. /* WHEN "F":U */
        WHEN "B":U THEN 
        DO:
          ASSIGN oMaleAgeRange:ControlToken      = "updatable":U
                 oMaleAgeRangeBtn:ControlToken   = "updatable":U
                 oBothAgeRange:ControlToken      = "updatable":U
                 oBothAgeRangeBtn:ControlToken   = "updatable":U
                 oFemaleAgeRange:ControlToken    = "updatable":U
                 oFemaleAgeRangeBtn:ControlToken = "updatable":U.

          IF oMaleAgeRange:ControlValue <> "":U OR oFemaleAgeRange:ControlValue <> "":U  
          THEN
            ASSIGN oBothAgeRange:ControlToken      = "disabled":U
                   oBothAgeRangeBtn:ControlToken   = "disabled":U.

          IF oBothAgeRange:ControlValue <> "":U 
          THEN
            ASSIGN oMaleAgeRange:ControlToken      = "disabled":U
                   oMaleAgeRangeBtn:ControlToken   = "disabled":U
                   oFemaleAgeRange:ControlToken    = "disabled":U
                   oFemaleAgeRangeBtn:ControlToken = "disabled":U.

        END. /* WHEN "B":U */
        OTHERWISE
        DO:
          ASSIGN oMaleAgeRange:ControlToken      = "disabled":U
                 oMaleAgeRangeBtn:ControlToken   = "disabled":U
                 oBothAgeRange:ControlToken      = "disabled":U
                 oBothAgeRangeBtn:ControlToken   = "disabled":U
                 oFemaleAgeRange:ControlToken    = "disabled":U
                 oFemaleAgeRangeBtn:ControlToken = "disabled":U .
        END. /* OTHERWISE  */
      END CASE. /* CASE cGender */

      ASSIGN
        cControlTypeIndicator   = ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_control.control_type_indicator":U, "BUFFER-VALUE":U).
      
      IF cControlTypeIndicator = "ma_acAuthControlTypeIndicatorExcl":U THEN
      DO:
        ASSIGN oControl                   = ipoContainer:getControl("fcClaimCode":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodeLkp":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimType":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodeDis":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodesControlLkp":U  + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimTypeDis":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcUsageType":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcUsageQuantity":U         + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcUsagePeriod":U           + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcUsagePeriodType":U       + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcUsagePeriodOver":U       + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcUserMsc":U               + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcPeriod":U                + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcPeriodType":U            + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcPeriodOverride":U        + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcRestrictions":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcRestrictionMsc":U        + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcAmount":U                + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcActivateAuthorised":U    + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcEnforceAthorised":U      + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcDefaultAuthStatus":U     + ipoContainer:ContainerCode)
               oControl:ControlToken      = "updatable":U
               oControl                   = ipoContainer:getControl("fcReasonKey":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "updatable":U
               oControl                   = ipoContainer:getControl("fcStatusReasonBtn":U       + ipoContainer:ContainerCode)
               oControl:ControlToken      = "updatable":U
               .
      END. /* IF cControlTypeIndicator = "ma_acAuthControlTypeIndicatorExcl":U THEN */
      ELSE
      DO:
        ASSIGN  oControl                   = ipoContainer:getControl("fcClaimCode":U             + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcClaimCodeLkp":U          + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcClaimType":U             + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcClaimCodeDis":U          + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcClaimCodesControlLkp":U  + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcClaimTypeDis":U          + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcUsageType":U             + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcUsageQuantity":U         + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcUsagePeriod":U           + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcUsagePeriodType":U       + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcUsagePeriodOver":U       + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcUserMsc":U               + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcPeriod":U                + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcPeriodType":U            + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcPeriodOverride":U        + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcRestrictions":U          + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcRestrictionMsc":U        + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcAmount":U                + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcActivateAuthorised":U    + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcEnforceAthorised":U      + ipoContainer:ContainerCode)
                oControl:ControlToken      = "updatable":U
                oControl                   = ipoContainer:getControl("fcDefaultAuthStatus":U     + ipoContainer:ContainerCode)
                oControl:ControlToken      = "disabled":U
                oControl                   = ipoContainer:getControl("fcReasonKey":U             + ipoContainer:ContainerCode)
                oControl:ControlToken      = "disabled":U
                oControl                   = ipoContainer:getControl("fcStatusReasonBtn":U       + ipoContainer:ContainerCode)
                oControl:ControlToken      = "disabled":U
                .
      END. /* Else Do: */
        
      IF oDefaultStatusReason:ControlValue <> "":U 
      THEN
      DO: 
          ASSIGN cNoteCode                     = oDefaultStatusReason:ControlValue
                 dInsurerObj                   = DECIMAL(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_control.insurer_obj":U, "BUFFER-VALUE":U))
                 iOption                       = INTEGER(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_control.option_code":U, "BUFFER-VALUE":U))
                 dStartDate                    = DATE(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_control.effective_date":U, "BUFFER-VALUE":U))
                 iStatus                       = INTEGER(ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_control.default_auth_status":U, "BUFFER-VALUE":U))
                 cStatusNote                   = ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_control.default_auth_status_note":U, "BUFFER-VALUE":U)
                 cStatusReasonType             = ipoContainer:getControl("fcNarration":U             + ipoContainer:ContainerCode):ControlValue
                 lNotePerAuthStatusValidRule   = ?.

            mipEnv:health:AuthService:getStatusReasonDesc( INPUT        dInsurerObj,
                                                           INPUT        iOption,
                                                           INPUT        dStartDate,
                                                           INPUT        iStatus,
                                                           INPUT        oDefaultStatusReason:ControlValue,
                                                           INPUT-OUTPUT cStatusReasonType,
                                                           INPUT-OUTPUT lNotePerAuthStatusValidRule,
                                                           OUTPUT       cStatusDescription,
                                                           OUTPUT       cErrorMessage).
          
          IF cErrorMessage = "":U
          THEN
            ASSIGN cNoteDescription  = cStatusDescription.
                  // btt_auth._reason_type     = cStatusReasonType.

          
          ASSIGN oDefaultStatusReason:ControlTooltip = IF cNoteDescription <> "":U THEN cNoteDescription ELSE "Please enter a valid default status reason".
      END.  
      ELSE
        ASSIGN oDefaultStatusReason:ControlTooltip = "Please enter a valid default status reason".


      IF oDefaultStatus:ControlValue <> "":U THEN
        ASSIGN oDefaultStatusReason:ControlToken = "updatable":U
               oDefaultStatusBtn:ControlToken    = "updatable":U.
      ELSE
        ASSIGN oDefaultStatusReason:ControlToken = "disabled":U
               oDefaultStatusBtn:ControlToken    = "disabled":U.

      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_type_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hactc":U, 
         INPUT DECIMAL(ipoContainer:getControl("fcDetailObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).    
               
    END. /* WHEN ipoContainer:ContainerCode */
    
    WHEN "AuthTypeProvider":U THEN
    DO:

      ASSIGN
        cControlTypeIndicator   = ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_provider.provider_type_indicator":U, "BUFFER-VALUE":U).

      IF cControlTypeIndicator = "ma_acAuthProviderTypeIndicatorExcl":U THEN
      DO:
        ASSIGN oControl                   = ipoContainer:getControl("fcProviderType":U               + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcAuthorisedService":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcMainProvider":U               + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcMandatory":U                  + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("ftEffectiveDate":U              + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("ftEndDate":U                    + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodesProvider":U         + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodesProvLkp":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimTypesCombo":U            + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcDefaultClaimCodeDetail":U     + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodesDetLkp":U           + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcDefaultClaimTypeDetail":U     + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodesDisallow":U         + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimCodesDisLkp":U           + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcClaimTypesDisallowCombo":U    + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcValidClaimCodesDetail":U      + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcValidClaimCodesDetailLkp":U   + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcValidClaimTypesDetailCombo":U + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcHeaderValuesUnlimited":U      + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcAuthDetaiLines":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fiQuantityAuth":U               + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fdAuthorisedAmount":U           + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcStatusUsers":U                + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcStatusUsersLkp":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcStatusRoles":U                + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcStatusRolesLkp":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcBaseRateUpdUser":U            + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcBaseRateUpdUserLkp":U         + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcBaseRateUpdRole":U            + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcBaseRateUpdRoleLkp":U         + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcArsRateUpdUser":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcArsRateUpdUserLkp":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcArsRateUpdRole":U             + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               oControl                   = ipoContainer:getControl("fcArsRateUpdRoleLkp":U          + ipoContainer:ContainerCode)
               oControl:ControlToken      = "disabled":U
               .
      END. /* IF cControlTypeIndicator = "ma_acAuthControlTypeIndicatorExcl":U THEN */
      ELSE
      DO:
        ASSIGN oControl                 = ipoContainer:getControl("fcProviderType":U               + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcAuthorisedService":U          + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcMainProvider":U               + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcMandatory":U                  + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("ftEffectiveDate":U              + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("ftEndDate":U                    + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcClaimCodesProvider":U         + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcClaimCodesProvLkp":U          + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcClaimTypesCombo":U            + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcDefaultClaimCodeDetail":U     + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcClaimCodesDetLkp":U           + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcDefaultClaimTypeDetail":U     + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcClaimCodesDisallow":U         + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcClaimCodesDisLkp":U           + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcClaimTypesDisallowCombo":U    + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcValidClaimCodesDetail":U      + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcValidClaimCodesDetailLkp":U   + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcValidClaimTypesDetailCombo":U + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcHeaderValuesUnlimited":U      + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcAuthDetaiLines":U             + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fiQuantityAuth":U               + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fdAuthorisedAmount":U           + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcStatusUsers":U                + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcStatusUsersLkp":U             + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcStatusRoles":U                + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcStatusRolesLkp":U             + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcBaseRateUpdUser":U            + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcBaseRateUpdUserLkp":U         + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcBaseRateUpdRole":U            + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcBaseRateUpdRoleLkp":U         + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcArsRateUpdUser":U             + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcArsRateUpdUserLkp":U          + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcArsRateUpdRole":U             + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
             oControl                   = ipoContainer:getControl("fcArsRateUpdRoleLkp":U          + ipoContainer:ContainerCode)
             oControl:ControlToken      = "updatable":U
                .
      END. /* Else Do: */


      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_type_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hactp":U, 
         INPUT DECIMAL(ipoContainer:getControl("fcDetailObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).    
               
    END. /* WHEN ipoContainer:ContainerCode */

    WHEN "AuthTypeDetail":U THEN
    DO:

      ASSIGN
        cControlTypeIndicator   = ipoContainer:ContainerQuery:getFieldAttribute("tt_auth_type_detail.detail_type_indicator":U, "BUFFER-VALUE":U).

      IF cControlTypeIndicator = "ma_acAuthProviderTypeIndicatorExcl":U THEN
      DO:
      
          ASSIGN oControl                   = ipoContainer:getControl("fcAuthUsageLimit":U         + ipoContainer:ContainerCode)
                 oControl:ControlToken      = "disabled":U
                 oControl                   = ipoContainer:getControl("fcDefaultLineRestriction":U + ipoContainer:ContainerCode)
                 oControl:ControlToken      = "disabled":U
                 oControl                   = ipoContainer:getControl("fcAutoCreate":U             + ipoContainer:ContainerCode)
                 oControl:ControlToken      = "disabled":U
                 .
      END. /* IF cControlTypeIndicator = "ma_acAuthControlTypeIndicatorExcl":U THEN */
      ELSE
      DO:
          ASSIGN  oControl                  = ipoContainer:getControl("fcAuthUsageLimit":U         + ipoContainer:ContainerCode)
                  oControl:ControlToken     = "updatable":U
                  oControl                  = ipoContainer:getControl("fcDefaultLineRestriction":U + ipoContainer:ContainerCode)
                  oControl:ControlToken     = "updatable":U
                  oControl                  = ipoContainer:getControl("fcAutoCreate":U             + ipoContainer:ContainerCode)
                  oControl:ControlToken     = "updatable":U
                  .
      END. /* Else Do: */

      mipEnv:Health:maUiService:setContainerErrors
        (INPUT TEMP-TABLE tt_auth_type_error:HANDLE, 
         INPUT ipoContainer, 
         INPUT "hactd":U, 
         INPUT DECIMAL(ipoContainer:getControl("fdAuthTypeDetailObj":U + ipoContainer:ContainerCode):ControlValue),
         INPUT "":U).    
               
    END. /* WHEN ipoContainer:ContainerCode */
    
  END CASE.
  
  { mip/inc/mipcatcherror.i }
