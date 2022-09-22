/* maauthdetailrowrender.i  MEDSTAR Medical Aid System
                            Auth Detail Container Ajax Save
                            maauthdetailuiservicestack.p -> rowRenderProcedure
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/   
  DEFINE INPUT PARAMETER ipoContainer AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE iControl                AS INTEGER             NO-UNDO.
  DEFINE VARIABLE dStartDate              AS DATE                NO-UNDO.
  DEFINE VARIABLE dAuthObj                AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthDetailObj          AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthGroupObj           AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthProviderObj        AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dOwningObj              AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dTariffTypeObj          AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dAuthDetailNappiObj     AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dQuantityLos            AS DECIMAL             NO-UNDO.   
  DEFINE VARIABLE dTariffLinkObj          AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dMinutesTariffTypeObj   AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE dInsurerObj             AS DECIMAL             NO-UNDO.
  DEFINE VARIABLE cError                  AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cArsRate                AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cBaseRate               AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cDiscipline             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOwningAltValue         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cSubDiscipline          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cNegGroup               AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cOEM                    AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatus                 AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatusNote             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cStatusType             AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cTariffTypeAcronymKey   AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cProviderList           AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cLosCalcRule            AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRelatedEntity          AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE cRateChangeType         AS CHARACTER           NO-UNDO.
  DEFINE VARIABLE iSearchDoctor           AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iOptionCode             AS INTEGER             NO-UNDO.
  DEFINE VARIABLE lBaseRateUpdAllowed     AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lArsRateUpdAllowed      AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lMandatory              AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lSuccess                AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lFieldToBeDeactivated   AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lStartDateAmPm          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lEndDateAmPm            AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lLosCalc                AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lParentOfModifier       AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lChildModifier          AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE lActivateMinutes        AS LOGICAL             NO-UNDO.
  DEFINE VARIABLE hWob                    AS HANDLE              NO-UNDO.
  DEFINE VARIABLE oControl                AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oConfiguration          AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oOverrideBaseRate       AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oOverrideArsRate        AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oOverrideArsRateBtn     AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oDiscountType           AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oLineRestriction        AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oLosCalc                AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oLocQuantity            AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oLocSequence            AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oNegGroup               AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oProvider               AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oStatusNote             AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oStatusNoteBtn          AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oTariffTypeCategory     AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oItemCost               AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oStartDate              AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oStartDateAmPm          AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oEndDate                AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oEndDateAmPm            AS cls.mipwscontrol    NO-UNDO.  
  DEFINE VARIABLE oRelatedEntity          AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oRelatedCode            AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oOwningObj              AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oOwningAltValue         AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oAuthDetailIndexList    AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oOwningBtn              AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oOwningTrfBtn           AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oLosCalcRule            AS cls.mipwscontrol    NO-UNDO.  
  DEFINE VARIABLE oMinutesAuth            AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oMinutesRequested       AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oMinTariffTypeObj       AS cls.mipwscontrol    NO-UNDO.
  DEFINE VARIABLE oQuery                  AS cls.mipquery        NO-UNDO.
  DEFINE VARIABLE oAuthorisation          AS cls.maauthorisation NO-UNDO.

  /*
    RowRenderProcedure will be run in the current wob to attach any business logic validation errors generated
    during a submit to this container. RowRenderProcedure container code which should be applied to this container
    ragardless of which wob this container is used in should be placed in the case block below.
  */
  IF LOOKUP(ipoContainer:RowRenderArgument, "AuthDetailContainer":U) = 0 THEN
  DO:
    RUN SUPER(INPUT ipoContainer) NO-ERROR.

    /*
      The super may not have a rowRenderProcedure
    */
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6439' &ResetIgnoredErrors = TRUE }

  END. /*IF LOOKUP(ipoContainer:RenderArgument, "AuthDetailContainer":U) = 0 THEN*/
  ELSE
  DO:
    ASSIGN hWob = mipEnv:miProcedure:getProcedureHandle(INPUT "Wob_":U + WarpSpeed:CurrentWob).

    /*
      If the wob is not run persistently it will be added as a super
      to the ui service render procedure handle
    */
    IF NOT VALID-HANDLE(hWob)
    THEN
      ASSIGN hWob = wsUIService:RenderProcedureHandle.


    IF VALID-HANDLE(hWob)
    THEN
      RUN rowRenderProcedure IN hWob(INPUT ipoContainer) NO-ERROR.


    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:6456,PROGRESS:3234' &ResetIgnoredErrors = FALSE }

  END. /*ELSE*/

&IF {&DBDFMA} >= 010195 &THEN

  CASE ipoContainer:RowRenderArgument:

    WHEN "AuthDetailContainer":U THEN
    DO: 
      ASSIGN oProvider           = ipoContainer:getControl("cbProvider":U           + ipoContainer:ContainerCode)
             oOverrideBaseRate   = ipoContainer:getControl("fcOverrideBaseRate":U   + ipoContainer:ContainerCode)
             oOverrideArsRate    = ipoContainer:getControl("fcOverrideArsRate":U    + ipoContainer:ContainerCode)
             oOverrideArsrateBtn = ipoContainer:getControl("buOverrideArsRateBtn":U + ipoContainer:ContainerCode)
             oDiscountType       = ipoContainer:getControl("cbDiscountType":U       + ipoContainer:ContainerCode)
             oLineRestriction    = ipoContainer:getControl("cbLineRestriction":U    + ipoContainer:ContainerCode)
             oLosCalc            = ipoContainer:getControl("flLosCalc":U            + ipoContainer:ContainerCode)
             oLocQuantity        = ipoContainer:getControl("fdQuantityLos":U        + ipoContainer:ContainerCode)
             oLocSequence        = ipoContainer:getControl("fiLocSequence":U        + ipoContainer:ContainerCode)
             oNegGroup           = ipoContainer:getControl("fcNegGroup":U           + ipoContainer:ContainerCode)
             oStatusNote         = ipoContainer:getControl("fcReason":U             + ipoContainer:ContainerCode)
             oStatusNoteBtn      = ipoContainer:getControl("buReasonBtn":U          + ipoContainer:ContainerCode)
             oItemCost           = ipoContainer:getControl("fdItemCost":U           + ipoContainer:ContainerCode)
             oTariffTypeCategory = ipoContainer:getControl("cbTariffTypeCategory":U + ipoContainer:ContainerCode)
             oConfiguration      = ipoContainer:getControl("_configuration":U       + ipoContainer:ContainerCode)
             oStartDate          = ipoContainer:getControl("fdStartDate":U          + ipoContainer:ContainerCode)
             oStartDateAmPm      = ipoContainer:getControl("cbStartAmPm":U          + ipoContainer:ContainerCode)
             oEndDate            = ipoContainer:getControl("fdEndDate":U            + ipoContainer:ContainerCode)
             oEndDateAmPm        = ipoContainer:getControl("cbEndAmPm":U            + ipoContainer:ContainerCode)
             oRelatedEntity      = ipoContainer:getControl("cbRelatedEntity":U      + ipoContainer:ContainerCode)
             oRelatedCode        = ipoContainer:getControl("fcRelatedCode":U        + ipoContainer:ContainerCode)
             
             oOwningAltValue     = ipoContainer:getControl("fcOwningAltValue":U     + ipoContainer:ContainerCode)
             oOwningBtn          = ipoContainer:getControl("buOwningBtn":U          + ipoContainer:ContainerCode)
             oOwningTrfBtn       = ipoContainer:getControl("buOwningTrfBtn":U       + ipoContainer:ContainerCode)
             oLosCalc            = ipoContainer:getControl("flLosCalc":U            + ipoContainer:ContainerCode)
             oLosCalcRule        = ipoContainer:getControl("fcLosCalcRule":U        + ipoContainer:ContainerCode) 
             oMinutesAuth        = ipoContainer:getControl("fiMinAuthorised":U      + ipoContainer:ContainerCode)  
             oMinutesRequested   = ipoContainer:getControl("fiMinRequested":U       + ipoContainer:ContainerCode)
             oMinTariffTypeObj   = ipoContainer:getControl("fdMinTariffTypeObj":U   + ipoContainer:ContainerCode)
             
             oQuery              = ipoContainer:ContainerQuery
      
             dStartDate          =    DATE(oQuery:getFieldAttribute("tt_auth_detail.start_date":U               , "BUFFER-VALUE":U))
                                                                                                                
             lLosCalc            = LOGICAL(oQuery:getFieldAttribute("tt_auth_provider.los_calculation":U        , "BUFFER-VALUE":U))
             cLosCalcRule        =         oQuery:getFieldAttribute("tt_auth_detail.los_calculation_rule":U     , "BUFFER-VALUE":U)
             cRelatedEntity      =         oQuery:getFieldAttribute("tt_auth_detail.related_entity_mnemonic":U  , "BUFFER-VALUE":U) 
             dQuantityLos        = DECIMAL(oQuery:getFieldAttribute("tt_auth_detail.quantity_los":U             , "BUFFER-VALUE":U) )
             cRateChangeType     =         oQuery:getFieldAttribute("tt_auth_provider.rate_change_type":U       , "BUFFER-VALUE":U) 
             
             
             dAuthObj            = DECIMAL(oQuery:getFieldAttribute("tt_auth_detail.auth_obj":U               , "BUFFER-VALUE":U))
             dAuthDetailObj      = DECIMAL(oQuery:getFieldAttribute("tt_auth_detail.auth_detail_obj":U        , "BUFFER-VALUE":U))
             dAuthProviderObj    = DECIMAL(oQuery:getFieldAttribute("tt_auth_provider.auth_provider_obj":U    , "BUFFER-VALUE":U))
             dOwningObj          = DECIMAL(oQuery:getFieldAttribute("tt_auth_detail.owning_obj":U             , "BUFFER-VALUE":U))
             
             cOEM                =         oQuery:getFieldAttribute("tt_auth_detail.owning_entity_mnemonic":U , "BUFFER-VALUE":U)
             cDiscipline         =         oQuery:getFieldAttribute("tt_auth_provider.pr_type":U              , "BUFFER-VALUE":U)
             cSubDiscipline      =         oQuery:getFieldAttribute("tt_auth_provider.sub_pr_type":U          , "BUFFER-VALUE":U)
             
             cStatusType         =         oQuery:getFieldAttribute("tt_auth_detail._reason_type":U           , "BUFFER-VALUE":U)

             dInsurerObj         = DECIMAL(oQuery:getFieldAttribute("tt_auth.insurer_obj":U                   , "BUFFER-VALUE":U))
             iOptionCode         = INTEGER(oQuery:getFieldAttribute("tt_auth.option_code":U                   , "BUFFER-VALUE":U))

             lStartDateAmPm      = logical(oQuery:getFieldAttribute("tt_auth_detail.start_ampm":U , "BUFFER-VALUE":U))
             lEndDateAmPm        = logical(oQuery:getFieldAttribute("tt_auth_detail.end_ampm":U ,   "BUFFER-VALUE":U))

             dStartDate          = (IF dStartDate = ?
                                    THEN TODAY
                                    ELSE dStartDate).

      /*
        For performance reasons, there are some things we need to only do once, so
        common variables will be set/determined once when rendering the first row
      */
      IF ipoContainer:CurrentRow <= 1 THEN
      DO:
        ASSIGN gcDefaultDateJSOnBlur  = oStartDate:JavascriptOnBlur
               gcDefaultAmPmJSOnBlur  = oStartDateAmPm:JavascriptOnBlur
               .
      END. /*IF ipoContainer:CurrentRow = 1 THEN */

      /*
         - Prepare a base rate list for the user to select
         - Prepare a list of Line Restrictions
      */
      ASSIGN
        cNegGroup              = oQuery:getFieldAttribute("tt_auth_detail._negotiation_group":U,    "BUFFER-VALUE":U) 
        oNegGroup:ControlValue = IF dAuthProviderObj <= 0.00 OR dAuthProviderObj = ? 
                                 THEN "":U
                                 ELSE cNegGroup.
        
      /*
        Check if the current record requires a reason depending on the status
      */
      ASSIGN lMandatory = LOGICAL(oQuery:getFieldAttribute("tt_auth_detail._mandatory":U,    "BUFFER-VALUE":U)).
           

      /*
        When we are dealing with an LOS/LOC record then the loc quantity and loc sequence fields should be enabled
        otherwise we will restore the control token back to what it was when it was defined.
      */
      ASSIGN
        cTariffTypeAcronymKey  = oQuery:getFieldAttribute("tt_auth_detail._tariff_type_acronym_key":U,    "BUFFER-VALUE":U).

      IF cTariffTypeAcronymKey <> "":U AND cTariffTypeAcronymKey <> ? THEN
      DO:
        ASSIGN
          oLocSequence:ControlToken        = "Updatable":U
          oLocQuantity:ControlToken        = "Updatable":U
          oLocSequence:ControlClass        = "+clMan":U
          oLocQuantity:ControlClass        = "+clMan":U
          oLosCalc:ControlToken            = "Disabled":U
          oTariffTypeCategory:ControlValue = cTariffTypeAcronymKey.

        /* set the Start/End date am/pm updated to true when the date and am/pm fields are updated. */
        ASSIGN
          oStartDate:JavaScriptOnChange     = "fnOnChangeDateAmPm(this,~"flStartDateAmPmUpdated~"); ":U
          oStartDateAmPm:JavaScriptOnChange = "fnOnChangeDateAmPm(this,~"flStartDateAmPmUpdated~"); ":U
          oEndDate:JavaScriptOnChange       = "fnOnChangeDateAmPm(this,~"flEndDateAmPmUpdated~");   ":U
          oEndDateAmPm:JavaScriptOnChange   = "fnOnChangeDateAmPm(this,~"flEndDateAmPmUpdated~");   ":U
          oStartDate:JavaScriptOnBlur       = SUBSTITUTE(gcDefaultDateJSOnBlur, " return true ; ":U )
          oStartDateAmPm:JavaScriptOnBlur   = SUBSTITUTE(gcDefaultAmPmJSOnBlur, " return true ; ":U )
          oEndDate:JavaScriptOnBlur         = SUBSTITUTE(gcDefaultDateJSOnBlur, " return true ; ":U )
          oEndDateAmPm:JavaScriptOnBlur     = SUBSTITUTE(gcDefaultAmPmJSOnBlur, " return true ; ":U )
          ipoContainer:BackgroundClass      = "+clLOS -clNonLOS":U
          .
      END. /* IF cTariffTypeAcronymKey <> "":U AND cTariffTypeAcronymKey <> ? THEN */
      ELSE
      DO:
        ASSIGN
          oLocSequence:ControlToken          = "Disabled":U
          oLocQuantity:ControlToken          = "Disabled":U
          oLocSequence:ControlClass          = "-clMan":U
          oLocQuantity:ControlClass          = "-clMan":U
          oLosCalc:ControlToken              = "Hidden":U
          oTariffTypeCategory:ControlValue   = "":U
          oStartDate:JavaScriptOnBlur        = SUBSTITUTE(gcDefaultDateJSOnBlur, " ":U )
          oStartDateAmPm:JavaScriptOnBlur    = SUBSTITUTE(gcDefaultAmPmJSOnBlur, " ":U )
          oEndDate:JavaScriptOnBlur          = SUBSTITUTE(gcDefaultDateJSOnBlur, " ":U )
          oEndDateAmPm:JavaScriptOnBlur      = SUBSTITUTE(gcDefaultAmPmJSOnBlur, " ":U ) 
          ipoContainer:BackgroundClass       = "-clLOS +clNonLOS":U
          .

      END. /*DO:*/

      ASSIGN
        lBaseRateUpdAllowed = LOGICAL(oQuery:getFieldAttribute("tt_auth_detail._base_rate_upd_allowed":U, "BUFFER-VALUE":U))
        lArsRateUpdAllowed  = LOGICAL(oQuery:getFieldAttribute("tt_auth_detail._ars_rate_upd_allowed":U,  "BUFFER-VALUE":U)).


      IF LOOKUP(cOEM, "htmtl,hlmcr":U) > 0
      THEN DO:
        ASSIGN
          oOverrideBaseRate:ControlToken   = IF lBaseRateUpdAllowed THEN "Updatable":U
                                             ELSE "Disabled":U
          oOverrideArsRate:ControlToken    = IF lArsRateUpdAllowed THEN "Updatable":U
                                             ELSE "Disabled":U
          oOverrideArsRateBtn:ControlToken = IF lArsRateUpdAllowed THEN "Updatable":U
                                             ELSE "Disabled":U.

      END. /* IF INDEX(cOEM, "htmtl,hlmcr":U) > 0 */
      ELSE DO:
          ASSIGN oOverrideBaseRate:ControlToken   = "Disabled":U
                 oOverrideArsRate:ControlToken    = "Disabled":U
                 oOverrideArsRateBtn:ControlToken = "Disabled":U.
      END. /* ELSE DO: */

      IF cOEM = "hlmcr":U OR cOEM = "hlmnl":U THEN
        ASSIGN lFieldToBeDeactivated = TRUE.

      ASSIGN cProviderList                     = oQuery:getFieldAttribute("tt_auth._provider_list":U, "BUFFER-VALUE":U)
             oProvider:AdditionalItems         = cProviderList
             oOverrideBaseRate:ControlValue    = "":U WHEN dAuthDetailObj <= 0.00
             oOverrideBaseRate:ControlToken    = "Disabled":U WHEN dAuthDetailObj <= 0.00
             
             oRelatedEntity:ControlToken       = (IF lFieldToBeDeactivated THEN "disabled":U ELSE "updatable":U)
             oRelatedCode:ControlToken         = (IF lFieldToBeDeactivated THEN "disabled":U ELSE "updatable":U)

             oOverrideArsRate:ControlToken     = "Disabled":U WHEN dAuthDetailObj <= 0.00
             oOverrideArsRateBtn:ControlToken  = "Disabled":U WHEN dAuthDetailObj <= 0.00

             oDiscountType:AdditionalItems     = "<None>=?|Percentage=YES|Rand=NO":U
             oDiscountType:ControlValue        = "?":U WHEN dAuthDetailObj <= 0.00

             oStatusNote:ControlClass          = (IF lMandatory THEN " +":U        ELSE " -":U) + "clMan":U  /*Set appropriate mandatory class depending on status           */
             oStatusNote:ControlToken          = (IF lMandatory THEN "Updatable":U ELSE "Disabled":U)        /*Control should be disabled/updatable depending on status      */
             oStatusNoteBtn:ControlToken       = (IF lMandatory THEN "Updatable":U ELSE "Disabled":U).       /*Control should be disabled/updatable depending on status      */

      ASSIGN
         oStartDateAmPm:ControlValue = if lStartDateAmPm then "AM":U else if not lStartDateAmPm then "PM":U else ?
         oEndDateAmPm:ControlValue   = if lEndDateAmPm   then "AM":U else if not lEndDateAmPm   then "PM":U else ?.

       
       IF cOEM = "htmtl":U THEN
       DO:
         mipEnv:health:AuthService:determineTariffModifierType(INPUT  dOwningObj ,
                                                               INPUT  dStartDate,
                                                               OUTPUT lParentOfModifier ,
                                                               OUTPUT lChildModifier   ) .
         ASSIGN oOwningTrfBtn:ControlClass = "-clHid":U .
       END. // IF cOEM = "htmtl":U THEN
       ELSE 
         ASSIGN oOwningTrfBtn:ControlClass = "+clHid":U . 
                                        
       IF  ( (NOT lLosCalc               AND dQuantityLos > 0 )   
       OR    (cRelatedEntity = "hatad":U AND dQuantityLos = 0 AND lChildModifier ) )
       OR  cRateChangeType <> "ma_acAuthRateChangeTypeNone":U
       AND dAuthDetailObj > 0 
       THEN
         ASSIGN oOwningAltValue:ControlToken  = "Disabled":U
                oOwningBtn:ControlToken       = "Disabled":U 
                oOwningAltValue:ControlClass  = "-clMan":U
                oOwningBtn:ControlClass       = "-clMan":U   . 
       ELSE
         ASSIGN oOwningAltValue:ControlToken  = "Updatable":U
                oOwningBtn:ControlToken       = "Updatable":U
                oOwningAltValue:ControlClass  = "+clMan":U
                oOwningBtn:ControlClass       = "+clMan":U  . 
       

       mipEnv:Health:AuthBusinessLogic:activateMinutes(INPUT  dInsurerObj,                
                                                       INPUT  iOptionCode,                
                                                       INPUT  dStartDate,                 
                                                       INPUT  dOwningObj,           //tariff link Obj
                                                       OUTPUT cError,                   
                                                       OUTPUT dMinutesTariffTypeObj,    
                                                       OUTPUT lActivateMinutes).

       IF cOEM = "htmtl":U AND lActivateMinutes AND dMinutesTariffTypeObj > 0 THEN
       DO:
         ASSIGN
           oMinutesAuth:ControlToken        = "Updatable":U 
           oMinutesAuth:ControlClass        = "+clMan":U
           oMinutesRequested:ControlToken   = "Updatable":U
           oMinTariffTypeObj:ControlValue   = STRING(dMinutesTariffTypeObj).
       END.
       ELSE DO:
         ASSIGN
           oMinutesAuth:ControlToken        = "Disabled":U 
           oMinutesAuth:ControlClass        = "-clMan":U
           oMinutesRequested:ControlToken   = "Disabled":U.
       END.


    END. /*WHEN "AuthDetailContainer":U THEN*/

    WHEN "DetailNappiContainer":U THEN
    DO:
      ASSIGN
        oOwningObj           = ipoContainer:getControl("_fdOwningObjArgument":U           + ipoContainer:ContainerCode)
        oOwningAltValue      = ipoContainer:getControl("_fcOwningAltValueArgument":U      + ipoContainer:ContainerCode)
        oAuthDetailIndexList = ipoContainer:getControl("_AuthDetailIndexListArgument":U   + ipoContainer:ContainerCode)
        .

      ASSIGN
        oOwningObj:ControlValue            = get-value("OwningObj":U)
        oOwningAltValue:ControlValue       = get-value("OwningAltValue":U)
      
        oAuthDetailIndexList:ControlValue = get-value("AuthIndexList":U).
    END. /* WHEN "DetailNappiContainer":U THEN */


  END CASE.

&ENDIF

 { mip/inc/mipcatcherror.i }
