/* maauthdetailgetcntupddetail.i  MEDSTAR Medical Aid System
                                  Authorisation Detail Container Procedure ( getCntUpdDetail )
                                  (c) Copyright 1990 - 2022
                                  MIP Holdings (Pty) Ltd
                                  All rights reserved
*/

  DEFINE INPUT  PARAMETER ipcContainerName AS CHARACTER          NO-UNDO.
  DEFINE OUTPUT PARAMETER opoContainer     AS cls.mipwscontainer NO-UNDO.

  DEFINE VARIABLE oContainerProperties AS cls.wscontainerproperties NO-UNDO.
  DEFINE VARIABLE oHeaderContainer     AS cls.mipwscontainer        NO-UNDO.
  DEFINE VARIABLE oControl             AS cls.mipwscontrol          NO-UNDO.
  DEFINE VARIABLE oWob                 AS cls.mipwswob              NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cSaveLOSButtonName   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cButtonErrorMessage  AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cInputErrorMessage   AS CHARACTER                 NO-UNDO.

&IF {&DBDFMA} >= 10195 &THEN

  ASSIGN
    cButtonErrorMessage = "To activate this button please select a related entity before continuing, "
                        + "if the related entity field is disabled it may be because the owning item is either a nappi or a basket"
    cInputErrorMessage  = "Please ensure you have selected a related entity before selecting a Related Item Code".

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
                                          + ",  FIRST tt_auth NO-LOCK":U
                                          + "   WHERE tt_auth.auth_obj = tt_auth_detail.auth_obj OUTER-JOIN":U
                                          + "      BY tt_auth_detail.line_number BY tt_auth_provider.provider_sequence BY tt_auth_detail._loc_line DESCENDING BY tt_auth_detail.loc_sequence  BY tt_auth_detail.owning_obj":U
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

    oControl                              = opoContainer:addControl("buNappi":U                   + ipcContainerName, "wsImage":U       , "20":U , "":U                                   , "CHARACTER":U,  3, "":U)
    oControl:ControlToolTip               = "Add Nappis"
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:JavaScriptOnClick            = 'fnShowAuthNappi(this);':U
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthDetailNappiButton":U

    oControl                              = opoContainer:addControl("fdAuthDetailObj":U            + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.auth_detail_obj":U       , "CHARACTER":U, 4, "")
    oControl:ControlClass                 = "+clObjControl +clHid":U

    /* Enable sequence field when a detail line is successfully created                         */
    /* Disable primary fields once a detail record has been successfully created                */
    /* Enable edit button which launches detail form when a detail line is successfully created */
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailObj(this);":U
                                          + "fnOnChangeEnableUpdateButton(this, ~"buUpdate" + ipcContainerName + "~");":U

    /* This will be disabled on update - See AlternateKeyFields below */
    oControl                              = opoContainer:addControl("cbProvider":U                 + ipcContainerName, "wsCombo":U       , "5":U , "tt_auth_detail.auth_provider_obj":U     , "CHARACTER":U, 5, "Provider")
    oControl:ControlClass                 = "+clMan":U
    oControl:ControlTooltip               = "Provider":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthDetailProviderList":U

    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:Detail":U
    oControl:FilterFields                 = "[AuthProviderObj]":U
    oControl:FilterControls               = "cbProvider":U + ipcContainerName
    oControl:ReturnFields                 = "[PrType],[SubPrType],[BaseRate],[ArsRate],[NegGroup],[LineRestriction],[AuthGroupObj]":U
    oControl:ReturnControls               = "fiDiscipline":U       + ipcContainerName + ",fiSubDiscipline":U   + ipcContainerName
                                          + ",fcDefaultBaseRate":U + ipcContainerName + ",fcDefaultArsRate":U  + ipcContainerName
                                          + ",fcNegGroup":U        + ipcContainerName + ",cbLineRestriction":U + ipcContainerName
                                          + ",fdAuthGroupObj":U    + ipcContainerName

    oControl                              = opoContainer:addControl("fiDiscipline":U               + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_provider.pr_type":U             , "INTEGER":U  , 6, "Disc")
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U /*Do not allow characters in this field and restrict to 3 digits*/
    oControl:ControlFormat                = "999":U
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlTooltip               = "Provider Discipline":U

    oControl                              = opoContainer:addControl("fiSubDiscipline":U            + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_provider.sub_pr_type":U         , "INTEGER":U  , 7, "Sub<br>Disc")
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U /*Do not allow characters in this field and restrict to 3 digits*/
    oControl:ControlFormat                = "999":U
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlTooltip               = "Provider Sub-Discipline"

    oControl                              = opoContainer:addControl("fdAuthGroupObj":U            + ipcContainerName, "wsCombo":U       , "10":U , "tt_auth_detail.auth_group_obj":U         , "CHARACTER":U  , 8, "Auth<br>Group")
    oControl:AdditionalItems              = "=":U
    oControl:QueryString                  = "FOR EACH ham_auth_group NO-LOCK":U
    oControl:KeyField                     = "ham_auth_group.auth_group_obj":U
    oControl:DisplayFields                = "ham_auth_group.auth_group_code":U
    oControl:ControlTooltip               = "Groups the Providers and Clinical Details to a specific Authorisation group. Only one of these Providers/Disciplines will be allowed to be claimed from"

    oControl                              = opoContainer:addControl("fcNegGroup":U                 + ipcContainerName, "wsInput":U       , "5":U, "":U                                      , "CHARACTER":U, 9, "Neg<br>Group")
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fcDefaultBaseRate":U          + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.default_base_rate":U     , "CHARACTER":U, 10, "Default<br>Base<br>Rate")
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlTooltip               = "Default Base Rate":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderArgument               = "BaseRate":U
    oControl:RenderProcedure              = "RenderProcedure":U

    oControl                              = opoContainer:addControl("fcDefaultArsRate":U           + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.default_ars_rate":U      , "CHARACTER":U, 11, "Default<br>ARS<br>Rate")
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
    oControl                              = opoContainer:addControl("cbTariffTypeCategory":U       + ipcContainerName, "wsCombo":U       , "10":U, "":U                                      , "CHARACTER":U, 12, "Tariff Type<br>Category")
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AcronymSelect:ma_acTariffTypeCat:=":U
    oControl:ControlToken                 = "Disabled":U

    /* Although this control is disabled for now, we may use in the future */
    oControl                              = opoContainer:addControl("cbTariffType":U               + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_detail.loc_tariff_type_obj":U    , "CHARACTER":U, 13, "Tariff<br>Type")
    oControl:AdditionalItems              = "=":U
    oControl:QueryString                  = "FOR EACH htm_tariff_type NO-LOCK BY htm_tariff_type.tariff_type_code":U
    oControl:KeyField                     = "htm_tariff_type.tariff_type_obj":U
    oControl:DisplayFields                = "htm_tariff_type.tariff_type_code":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailTariffType(this);":U
    oControl:ControlToken                 = "Disabled":U

    oControl                              = opoContainer:addControl("fdMinTariffTypeObj":U         + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.minutes_tariff_type_obj":U, "DECIMAL":U  , 13, "")
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1&2":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailMinutesTariffType(this);":U

    /* This will be disabled on update - See AlternateKeyFields below */
    oControl                              = opoContainer:addControl("cbOEM":U                      + ipcContainerName, "wsCombo":U       , "5":U , "tt_auth_detail.owning_entity_mnemonic":U , "CHARACTER":U, 14, "Detail<br>Entity")
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
                                          + "                   ~"fcLinkTemplate~",   ":U
                                          + "                   ~"fdStartDate~",      ":U
                                          + "                   ~"buOwningBtn~",      ":U
                                          + "                   ~"buOwningTrfBtn~");  ":U
    oControl:ControlToolTip               = "Select Basket,Nappi or Tariff"

    oControl                              = opoContainer:addControl("fdOwningObj":U                + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail.owning_obj":U            , "CHARACTER":U, 15, "")
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
                                          + "                   ~"fcLinkTemplate~",   ":U
                                          + "                   ~"fdStartDate~",      ":U
                                          + "                   ~"buOwningBtn~",      ":U
                                          + "                   ~"buOwningTrfBtn~");  ":U

    /* Using this field for nappi's where we typically use nappi link obj, but if we want to view details we */
    /* will launch the nappi wob and need the relevant nappi obj instead of the nappi link obj (AD)          */
    oControl                              = opoContainer:addControl("fdOwningAltObj":U             + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 15, "")
    oControl:ControlToken                 = "Hidden":U

    oControl                              = opoContainer:addControl("frOwningRowid":U              + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail._row_id":U, "CHARACTER":U, 15, "")
    oControl:ControlToken                 = "Hidden":U
    oControl                              = opoContainer:addControl("fcAuthNappiRequired":U        + ipcContainerName, "wsInput":U       , "15":U, "":U, "CHARACTER":U, 15, "")
    oControl:ControlToken                 = "Hidden":U
    oControl:JavascriptOnChange           = "fnTriggerNappiCnt(this);"

    /* This will be disabled on update - See AlternateKeyFields below */
    oControl                              = opoContainer:addControl("fcOwningAltValue":U           + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.owning_alt_value":U      , "CHARACTER":U, 15, "")
    oControl:ControlClass                 = "+clMan":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:":U
    oControl:FilterFields                 = "[PrType],[SubPrType],[BaseRate],[ArsRate],[StartDate],[OwningEntityMnemonic],[OwningAltValue],[AuthProviderObj],[OverrideBaseRate],[OverrideArsRate],[AuthObj]":U
    oControl:FilterControls               = "fiDiscipline":U       + ipcContainerName + ",fiSubDiscipline":U   + ipcContainerName
                                          + ",fcDefaultBaseRate":U + ipcContainerName + ",fcDefaultArsRate":U  + ipcContainerName
                                          + ",fdStartDate"         + ipcContainerName + ",cbOEM":U             + ipcContainerName
                                          + ",fcOwningAltValue":U  + ipcContainerName + ",cbProvider":U        + ipcContainerName
                                          + ",fcOverrideBaseRate"  + ipcContainerName + ",fcOverrideArsRate":U + ipcContainerName
                                          + ",_authObjArgument"    + ipcContainerName

    oControl:ReturnFields                 = "[RecordRowid],[RecordDesc],[TariffTypeCategory],[TariffTypeObj],[TariffCost],[TariffEffectiveDate],[RecordAltObj],[RecordObj],[AuthNappiRequired],[RecordCode],[MinutesTariffTypeObj]":U
    oControl:ReturnControls               = "frOwningRowid":U         + ipcContainerName + ",fcOwningDesc":U      + ipcContainerName
                                          + ",cbTariffTypeCategory":U + ipcContainerName + ",cbTariffType":U      + ipcContainerName
                                          + ",fdItemCost":U           + ipcContainerName + ",ftTariffEffecDate":U + ipcContainerName
                                          + ",fdOwningAltObj":U       + ipcContainerName + ",fdOwningObj":U       + ipcContainerName
                                          + ",fcAuthNappiRequired":U  + ipcContainerName + ",fcOwningAltValue":U  + ipcContainerName
                                          + ",fdMinTariffTypeObj"     + ipcContainerName

    /* If the owning entity is a tariff, ensure we have padded to 5 before the ajax validation fires */
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailOwningValue(this,~"cbOEM~");":U

    oControl                              = opoContainer:addControl("ftTariffEffecDate":U          + ipcContainerName, "wsInput":U       , "8":U , "":U                                     , "DATE":U     , 15, "")
    oControl:ControlToken                 = "HIDDEN":U

    /* This will be disabled on update - See AlternateKeyFields below */
    oControl                              = opoContainer:addControl("buOwningBtn":U                + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 15, "de")
    oControl:LookupWobFLA                 = "slent":U
    oControl:LookupFields                 = "ROWID,CODE_FIELD":U
    oControl:LookupControls               = "frOwningRowid":U + ipcContainerName + ",fcOwningAltValue":U + ipcContainerName
    oControl:FilterControls               = "cbOEM":U + ipcContainerName
    oControl:FilterFields                 = "QUERY_OEM":U
    oControl:ReturnFields                 = "CODE_FIELD,DESC_FIELD":U
    oControl:ReturnControls               = "fcOwningAltValue":U + ipcContainerName + ",fcOwningDesc":U + ipcContainerName

    oControl                              = opoContainer:addControl("buOwningTrfBtn":U                + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 15, "Detail<br>Item Code")
    oControl:CellLayoutMask               = "&1&2&3&4&5&6<div id='OwningLkpDiv' class='clHid' style='display : inline'>&7&8</div>":U
    oControl:LookupWobFLA                 = "htmtl":U
    oControl:LookupFields                 = "htm_tariff_link.tariff_code":U
    oControl:LookupControls               = "fcOwningAltValue":U + ipcContainerName

    oControl:FilterFields                 = "htm_tariff_link.pr_type,htm_tariff_link.base_rate,htm_tariff_link.ars_rate,tariff.effect-date":U
    oControl:FilterControls               = "fiDiscipline":U       + ipcContainerName + ",fcDefaultBaseRate":U + ipcContainerName
                                          + ",fcDefaultArsRate":U  + ipcContainerName + ",fdStartDate"         + ipcContainerName
    oControl:ControlClass                 = "+clHid":U

    oControl:ReturnFields                 = "htm_tariff_link.tariff_code,htm_tariff_link.tariff_description":U
    oControl:ReturnControls               = "fcOwningAltValue":U + ipcContainerName + ",fcOwningDesc":U + ipcContainerName

    oControl                              = opoContainer:addControl("fcOwningDesc":U               + ipcContainerName, "wsTextArea":U    , "15,3":U, "tt_auth_detail._detail_owning_description":U , "CHARACTER":U, 16, "")
    oControl:ControlToken                 = "Disabled":U

    oControl                              = opoContainer:addControl("fcDetails":U                  + ipcContainerName, "wsHref":U        , "10":U, "":U                                     , "CHARACTER":U, 17, "")
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthDetailOwningInfo":U

    /* This field is required to keep a "template" of the view more detail link which will be used to dynamically    */
    /* change the view detail link onclick and href attributes depending on the owning entity and item code selected.*/
    oControl                              = opoContainer:addControl("fcLinkTemplate":U             + ipcContainerName, "wsHref":U        , "10":U, "":U                                     , "CHARACTER":U, 18, "")
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthDetailOwningInfo":U

    oControl                              = opoContainer:addControl("fiLocSequence":U              + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.loc_sequence":U             , "INTEGER":U  , 19, "LOC<br>Seq")

    oControl                              = opoContainer:addControl("fdQuantityLos":U              + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.quantity_los":U             , "INTEGER":U  , 20, "Qty<br>LOS")

    oControl                              = opoContainer:addControl("fiMinRequested":U             + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.minutes_requested":U        , "INTEGER":U  , 22, "Min<br>Request")

    oControl                              = opoContainer:addControl("fiMinAuthorised":U            + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.minutes_auth":U             , "INTEGER":U  , 23, "Min<br>Auth")

    /* This will be disabled on update - See AlternateKeyFields below */
    oControl                              = opoContainer:addControl("fdStartDate":U                + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.start_date":U               , "DATE":U     , 24, "Start Date")
    oControl:ControlClass                 = "+clMan +clRetainValue":U
    oControl:JavascriptOnBlur             = "&1":U

    oControl                              = opoContainer:addControl("cbStartAmPm":U                + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_detail.start_ampm":U               , "LOGICAL":U  , 24, "Start Date")
    oControl:AdditionalItems              = "AM=AM|PM=PM":U
    oControl:ControlClass                 = "+clSelVertAlign":U
    oControl:JavascriptOnBlur             = "&1":U

    oControl                              = opoContainer:addControl("flStartDateAmPmUpdated":U     + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._start_date_ampm_updated":U , "CHARACTER":U, 24, "Start Date":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1 &2&3":U

    oControl                              = opoContainer:addControl("fdEndDate":U                  + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.end_date":U                 , "DATE":U     , 25, "End Date")
    oControl:JavascriptOnBlur             = "&1":U
    oControl                              = opoContainer:addControl("cbEndAmPm":U                  + ipcContainerName, "wsCombo":U       , "15":U, "tt_auth_detail.end_ampm":U                 , "LOGICAL":U  , 25, "End Date")
    oControl:AdditionalItems              = "AM=AM|PM=PM":U
    oControl:ControlClass                 = "+clSelVertAlign":U
    oControl:JavascriptOnBlur             = "&1":U

    oControl                              = opoContainer:addControl("flEndDateAmPmUpdated":U       + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._end_date_ampm_updated":U   , "CHARACTER":U, 25, "End Date":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1 &2&3":U.

  ASSIGN
    oControl                              = opoContainer:addControl("fdFixedItemCost":U            + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.fixed_item_cost":U         , "INTEGER":U  , 26, "Fixed<br>Item<br>Cost")
    oControl:ControlClass                 = "+clNumericOnly":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fdItemCost":U                 + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.item_cost":U               , "INTEGER":U  , 27, "Item<br>Cost")
    oControl:ControlClass                 = "+clNumericOnly":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fdQuantityRequested":U        + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.quantity_requested":U    , "INTEGER":U  , 28, "Qty<br>Request")
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdQuantityAuth":U             + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.quantity_auth":U         , "INTEGER":U  , 29, "Qty<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdQuantityPaid":U             + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_detail.quantity_paid":U         , "INTEGER":U  , 30, "Qty<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U
    oControl:ControlTooltip               = "Quantity Paid value":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fdAmountRequested":U          + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_requested":U      , "DECIMAL":U  , 31, "Amount<br>Request")
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdAmountAuth":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_auth":U           , "DECIMAL":U  , 32, "Amount<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdAmountPaid":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.amount_paid":U           , "DECIMAL":U  , 33, "Amount<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fiClaimCode":U                + ipcContainerName, "wsInput":U       , "3":U , "tt_auth_provider.claim_code":U          , "INTEGER":U  , 34, "Claim<br>Code")
    oControl:ControlClass                 = "+clNumericOnly +clMaxLength:3":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderArgument               = "ClaimCode":U
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:JavascriptOnChange           = "fnClaimCodeUpdated(this);"

    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:ClaimCode":U
    oControl:FilterFields                 = "[ClaimCode],[AuthObj],[Status]":U
    oControl:FilterControls               = "fiClaimCode":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName + ",cbStatus":U + ipcContainerName
    oControl:ReturnFields                 = "[ValidateClaimCode],[StatusAction]":U
    oControl:ReturnControls               = "_errorOrWarning":U + ipcContainerName   + ",_AuthStatusAction":U + ipcContainerName

    oControl                              = opoContainer:addControl("buClaimBtn":U                 + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 34, "Claim<br>Code")
    oControl:LookupWobFLA                 = "maclc":U
    oControl:LookupFields                 = "ccdesc.claim-code":U
    oControl:LookupControls               = "fiClaimCode":U + ipcContainerName
    oControl:FilterFields                 = "ccdesc.claim-code":U
    oControl:FilterControls               = "fiClaimCode":U + ipcContainerName
    oControl:ReturnFields                 = "ccdesc.claim-code":U
    oControl:ReturnControls               = "fiClaimCode":U + ipcContainerName

    oControl                              = opoContainer:addControl("flClaimCodeUpdated":U         + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._claim_code_updated":U  , "CHARACTER":U, 34, "Claim<br>Code":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1&2&3":U

    oControl                              = opoContainer:addControl("cbClaimType":U                + ipcContainerName, "wsCombo":U       , "3":U , "tt_auth_detail.claim_type":U            , "CHARACTER":U, 35, "Claim<br>Type")
    oControl:AdditionalItems              = "|C=C|N=N|A=A|K=K|P=P|O=O":U
    oControl:ControlToolTip               = "(C)onsultation,(N)on Eligible,(A)cute,(K)Chronic,(P)MB,(O)ther"
    oControl:JavascriptOnChange           = "fnClaimTypeUpdated(this);"

    oControl                              = opoContainer:addControl("flClaimTypeUpdated":U         + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail._claim_type_updated":U   , "CHARACTER":U, 35, "Claim<br>Type":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1&2":U

    oControl                              = opoContainer:addControl("cbLineRestriction":U          + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_detail.line_restriction":U      , "CHARACTER":U, 36, "Line<br>Restriction")
    oControl:ControlJavascript            = "style='width:128px'":U
    oControl:ControlToken                 = "ReadOnly":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderArgument               = "LineRestriction":U
    oControl:RenderProcedure              = "RenderProcedure":U

    oControl                              = opoContainer:addControl("fcNote":U                     + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.note":U                  , "CHARACTER":U, 37, "Note")
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:Reason":U
    oControl:FilterFields                 = "[ReasonCode],[ReasonType]":U
    oControl:FilterControls               = "fcNote":U + ipcContainerName + ",fcNoteType":U + ipcContainerName
    oControl:ReturnFields                 = "[ReasonCode]":U
    oControl:ReturnControls               = "fcNote":U + ipcContainerName

    oControl                              = opoContainer:addControl("fcNoteType":U                 + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 37, "Note")
    oControl:ControlToken                 = "Hidden":U
    oControl:ControlValue                 = "AN":U

    oControl                              = opoContainer:addControl("buNoteBtn":U                  + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 37, "Note")
    oControl:CellLayoutMask               = "&1&2&3":U
    oControl:LookupWobFLA                 = "note":U
    oControl:LookupFields                 = "note.key":U
    oControl:LookupControls               = "fcNote":U + ipcContainerName
    oControl:FilterFields                 = "note.key,note.type":U
    oControl:FilterControls               = "fcNote":U + ipcContainerName + ",fcNoteType":U + ipcContainerName
    oControl:ReturnFields                 = "note.key":U
    oControl:ReturnControls               = "fcNote":U + ipcContainerName

    oControl                              = opoContainer:addControl("cbDiscountType":U             + ipcContainerName, "wsCombo":U       , "3":U , "tt_auth_detail.discount_type":U         , "CHARACTER":U, 38, "Discount<br>Type")

    oControl                              = opoContainer:addControl("fdDiscountAuth":U             + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.discount_auth":U         , "DECIMAL":U  , 39, "Discount<br>Authorised")
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdDiscountPaid":U             + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.discount_paid":U         , "DECIMAL":U  , 40, "Discount<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdCopayAuth":U                + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_auth":U            , "DECIMAL":U  , 41, "Co-pay Amt<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fdCopayPercentage":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_auth_%":U          , "DECIMAL":U  , 42, "Co-pay %<br>Auth")
    oControl:ControlClass                 = "+clNumericOnly":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fdCopayPaid":U                + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.copay_paid":U            , "DECIMAL":U  , 43, "Co-pay<br>Paid")
    oControl:ControlClass                 = "+clNumericOnly":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("cbStatus":U                   + ipcContainerName, "wsCombo":U       , "10":U, "tt_auth_detail.auth_status":U           , "INTEGER":U  , 44, "Status")
    oControl:ControlTooltip               = "Select a status from the drop-down list.":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthStatusCombo":U
    oControl:ControlClass                 = "+clMan +clRetainValue":U
    oControl:JavaScriptOnChange           = "fnOnChangeAuthDetailStatus(this, ~"tbl~", ~"" + ipcContainerName + "~" );":U
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:Status":U
    oControl:FilterFields                 = "[Status],[AuthObj]":U
    oControl:FilterControls               = "cbStatus":U + ipcContainerName + ",_authObjArgument":U + ipcContainerName
    oControl:ReturnFields                 = "[StatusReasonMandatory]":U
    oControl:ReturnControls               = "flStatusReasonMandatory":U + ipcContainerName

    oControl                              = opoContainer:addControl("flStatusReasonMandatory":U    + ipcContainerName, "wsInput":U       , "15":U, "":U                                     , "CHARACTER":U, 44, "Status":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:CellLayoutMask               = "&1&2":U
    oControl:ControlValue                 = "null":U

    /* On status change, check whether reason is mandatory or not */
    oControl:JavaScriptOnChange           = "fnOnChangeAuthDetailStatus(this);":U
                                          + "fnClearStatusReason(this, ~"fcReason":U + ipcContainerName + "~",~"TBL~");":U

    /* See RowRenderProcedure - Checks were moved to RowRenderProcedure for performance reasons */
    /* Check whether each reason control is mandatory or not depending on status                */
    oControl                              = opoContainer:addControl("fcReason":U                   + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.auth_status_note":U    , "CHARACTER":U, 45, "Status<br>Reason":U)
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

    oControl                              = opoContainer:addControl("fcReasonTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 45, "Status<br>Reason":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:ControlValue                 = "AS":U
    oControl:ControlClass                 = "+clPreserveValue":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "AuthReasonType":U

    oControl                              = opoContainer:addControl("fcReasonDesc":U               + ipcContainerName, "wsInput":U       , "5":U , "":U                                     , "CHARACTER":U, 45, "Status<br>Reason":U)
    oControl:ControlToken                 = "Hidden":U

    /* Set reason description as tooltip when reason is selected client side */
    oControl:JavascriptOnChange          = "fnSetReasonDescription(this, ~"fcReason":U + ipcContainerName + "~", ~"buReasonBtn":U + ipcContainerName + "~", ~"TBL~");":U


    oControl                              = opoContainer:addControl("buReasonBtn":U                + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 45, "Status<br>Reason":U)
    oControl:CellLayoutMask               = "&1&2&3&4":U
    oControl:LookupWobFLA                 = "note":U
    oControl:LookupFields                 = "note.key":U
    oControl:LookupControls               = "fcReason":U + ipcContainerName
    oControl:FilterFields                 = "note.key,note.type":U
    oControl:FilterControls               = "fcReason":U + ipcContainerName + ",":U
                                          + "fcReasonTypeArgument":U + ipcContainerName
    oControl:ReturnFields                 = "note.key":U
    oControl:ReturnControls               = "fcReason":U + ipcContainerName

    oControl                              = opoContainer:addControl("fcCopayOverrideNote":U        + ipcContainerName, "wsInput":U       , "3":U  , "tt_auth_detail.copay_override_note":U , "INTEGER":U  , 46, "":U)
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:OverrideNote":U
    oControl:FilterFields                 = "[ReasonCode],[ReasonType],[DetailObj]":U
    oControl:FilterControls               = "fcCopayOverrideNote":U + ipcContainerName + ",":U
                                          + "fcOverrideNoteTypeArgument":U + ipcContainerName + ",":U
                                          + "fdAuthDetailObj":U + ipcContainerName
    oControl:ReturnFields                 = "[ReasonDesc]":U
    oControl:ReturnControls               = "fcOverrideNoteDesc":U + ipcContainerName

    oControl                              = opoContainer:addControl("fcOverrideNoteTypeArgument":U + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 46, "":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:ControlValue                 = "AQ":U
    oControl:ControlClass                 = "+clPreserveValue":U

    oControl                              = opoContainer:addControl("fcOverrideNoteDesc":U         + ipcContainerName, "wsInput":U       , "5":U  , "":U                                   , "CHARACTER":U, 46, "":U)
    oControl:ControlToken                 = "Hidden":U

    /* Set copay override note description as tooltip when note is selected */
    oControl:JavascriptOnChange           = "fnSetReasonDescription(this, ~"fcCopayOverrideNote":U + ipcContainerName + "~", ~"buOverrideNoteBtn":U + ipcContainerName + "~", ~"TBL~");":U

    oControl                              = opoContainer:addControl("buOverrideNoteBtn":U          + ipcContainerName, "wsLookupButton":U, "":U   , "":U                                   , "":U         , 46, "Co-payment<br>Override<br> Reason":U)
    oControl:ControlTooltip               = "Specify reason why co-payment should not apply.":U
    oControl:CellLayoutMask               = "&1&2&3&4":U
    oControl:LookupWobFLA                 = "note":U
    oControl:LookupFields                 = "note.key":U
    oControl:LookupControls               = "fcCopayOverrideNote":U + ipcContainerName
    oControl:FilterFields                 = "note.key,note.type":U
    oControl:FilterControls               = "fcCopayOverrideNote":U + ipcContainerName + ",":U
                                          + "fcOverrideNoteTypeArgument":U + ipcContainerName
    oControl:ReturnFields                 = "note.key":U
    oControl:ReturnControls               = "fcCopayOverrideNote":U + ipcContainerName
  .

  ASSIGN
    oControl                              = opoContainer:addControl("flRepeatItem":U               + ipcContainerName, "wsCheckBox":U    , "2":U , "tt_auth_detail.repeat_item":U              , "LOGICAL":U, 47, "Repeat<br>Item":U)
    oControl:ControlTooltip               = "Indicate whether this item code is a Repeat.":U
    oControl:JavaScriptOnChange           = "fnOnChangeRepeatItem(this);":U

    oControl                              = opoContainer:addControl("fiRepeatCycleAuth":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_auth":U        , "INTEGER":U, 48, "Repeat<br>Cycle<br>Auth":U)
    oControl:ControlTooltip               = "Indicates the authorised number of repeats for the prescription.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fiRepeatCyclePaid":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_paid":U        , "INTEGER":U, 49, "Repeat<br>Cycle<br>Paid":U)
    oControl:ControlTooltip               = "Indicates the authorised number of repeats that have been claimed.":U
    oControl:ControlToken                 = "Disabled":U

    oControl                              = opoContainer:addControl("fiRepeatCycleQuantity":U      + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_quantity":U    , "INTEGER":U, 50, "Repeat<br>Cycle<br>Qty":U)
    oControl:ControlTooltip               = "Indicates the quantity that will be repeated.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fiRepeatCycleDays":U          + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_days":U        , "INTEGER":U, 51, "Repeat<br>Cycle<br>Days":U)
    oControl:ControlTooltip               = "Indicates the number of days in one cycle and when a new script can be requested.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fiRepeatCycleGraceDays":U     + ipcContainerName, "wsInput":U       , "5":U , "tt_auth_detail.repeat_cycle_grace_days":U  , "INTEGER":U, 52, "Repeat<br>Cycle<br>Grace<br>Days":U)
    oControl:ControlTooltip               = "Indicates when the script can be requested earlier that the Repeat Cycle Days.":U
    oControl:ControlClass                 = "+clNumericOnly":U

    oControl                              = opoContainer:addControl("fdRepeatLastClaimedDate":U    + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.repeat_last_claimed_date":U , "DATE":U,    53, "Repeat<br>Last<br>Claimed<br>Date":U)
    oControl:ControlTooltip               = "Indicates the last date that this item was claimed.":U
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("fcOverrideBaseRate":U         + ipcContainerName, "wsCombo":U       , "8":U , "tt_auth_detail.override_base_rate":U    , "CHARACTER":U, 54, "Override<br>Base<br>Rate":U)
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "BaseRateList":U

    oControl                              = opoContainer:addControl("fcOverrideArsRate":U          + ipcContainerName, "wsInput":U       , "10":U, "tt_auth_detail.override_ars_rate":U     , "CHARACTER":U, 55, "":U)
    oControl                              = opoContainer:addControl("buOverrideArsRateBtn":U       + ipcContainerName, "wsLookupButton":U, "":U  , "":U                                     , "":U         , 55, "Override<br>ARS<br>Rate":U)
    oControl:CellLayoutMask               = "&1&2":U
    oControl:LookupWobFLA                 = "maarsrate":U
    oControl:LookupFields                 = "arsrate.ars-rate":U
    oControl:LookupControls               = "fcOverrideArsRate":U + ipcContainerName
    oControl:ReturnFields                 = "arsrate.ars-rate":U
    oControl:ReturnControls               = "fcOverrideArsRate":U + ipcContainerName

    oControl                              = opoContainer:addControl("cbRelatedEntity":U                      + ipcContainerName, "wsCombo":U    , "5":U , "tt_auth_detail.related_entity_mnemonic":U         , "CHARACTER":U, 56, "Related<br>Entity":U)
    oControl:AdditionalItems              = "=":U
    oControl:QueryString                  = "FOR EACH mic_acronym NO-LOCK":U
                                          + "   WHERE mic_acronym.category_key = 'ma_acAuthDetailRelated'":U
                                          + "      BY mic_acronym.acronym_label":U
    oControl:KeyField                     = "mic_acronym.acronym_value":U
    oControl:DisplayFields                = "mic_acronym.acronym_label":U
    oControl:ControlJavascript            = "style='width:128px'":U
    oControl:JavascriptOnChange           = "fnClearRelatedValues(this);fnSetQtyAuth(this); fnReactivateSlentLookupBtn(this, ~{triggerField: ~"cbRelatedEntity~", slentInputField: ~"fcRelatedCode~", slentButtonField: ~"buDetailRelatedBtn~"~});":U
    oControl:RenderProcedureHandle        = mipEnv:Health:maUiService:RenderProcedureHandle
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "RelatedEntity":U

    oControl                              = opoContainer:addControl("fcRelatedCode":U           + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_detail.related_value":U          , "character":U, 57, "Related Item Code")
    oControl:AjaxValidation               = "SERVICE:maUIService:ajaxValidationDetail:"
    oControl:FilterFields                 = "[OwningEntityMnemonic],[OwningAltValue],[StartDate]":U
    oControl:FilterControls               = "cbRelatedEntity":U + ipcContainerName + ",fcRelatedCode":U + ipcContainerName + ",fdStartDate":U  + ipcContainerName
    oControl:ReturnFields                 = "[RecordObj],[RecordDesc]":U
    oControl:ReturnControls               = "frRelatedObj":U  + ipcContainerName + ",":U + "fcRelatedDescr":U  + ipcContainerName
    oControl:JavaScriptOnChange           = "fnRevealSlentErrorMessage(this, ~{triggerField: ~"cbRelatedEntity~", thisSlentField: ~"fcRelatedCode~", isFormWindow: true, errorMessage: ~"" + cInputErrorMessage + "~" ~} );":U
    oControl:ControlJavascript            = " onkeydown= 'fnRevealSlentErrorMessage(this, ~{triggerField: ~"cbRelatedEntity~", thisSlentField: ~"fcRelatedCode~", isFormWindow: true, errorMessage: ~"" + cInputErrorMessage + "~" ~} );'":U

    oControl                              = opoContainer:addControl("fdRelatedObj":U            + ipcContainerName, "wsInput":U       , "18":U  , "tt_auth_detail.related_obj":U            , "character":U, 57, "Related Item Code")
    oControl:ControlToken                 = "Hidden":U

    oControl                              = opoContainer:addControl("buDetailRelatedBtn":U      + ipcContainerName, "wsLookupButton":U, "18":U  , "":U                                      , "":U         , 57, "Related Item Code")
    oControl:JavascriptOnClick            = "fnRevealSlentErrorMessage(this, ~{triggerField: ~"cbRelatedEntity~", thisSlentField: ~"buDetailRelatedBtn~", errorMessage: ~"" + cButtonErrorMessage + "~" ~} );":U
    oControl:LookupWobFLA                 = "slent":U
    oControl:LookupFields                 = "KEY_FIELD,CODE_FIELD":U
    oControl:LookupControls               = "fdRelatedObj":U + ipcContainerName + ",fcRelatedCode":U + ipcContainerName
    oControl:FilterControls               = "cbRelatedEntity":U + ipcContainerName
    oControl:FilterFields                 = "QUERY_OEM":U
    oControl:ReturnFields                 = "CODE_FIELD,DESC_FIELD":U
    oControl:ReturnControls               = "fcRelatedCode":U + ipcContainerName + ",fcDetailDescr":U + ipcContainerName
    oControl:CellLayoutMask               = "&1&2&3":U
    oControl:ControlToken                 = "disabled":U
    oControl:ControlJavascript            = " onmouseover= 'fnReactivateSlentLookupBtn(this, ~{triggerField: ~"cbRelatedEntity~", slentInputField: ~"fcRelatedCode~", slentButtonField: ~"buDetailRelatedBtn~"~});'":U

    oControl                              = opoContainer:addControl("fcRelatedDescr":U              + ipcContainerName, "wsTextArea":U    , "15":U , "tt_auth_detail._detail_related_description":U         , "CHARACTER":U, 58, "Related<br>Item<br>Description":U)
    oControl:ControlToken                 = "disabled":U

    oControl                              = opoContainer:addControl("fcTariffReason":U             + ipcContainerName, "wsTextArea":U, "15,3":U, "tt_auth_detail.reason":U,                   "CHARACTER":U, 59, "Reason":U)

    oControl                              = opoContainer:addControl("flAddedByUser":U              + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_detail.added_by_user":U         , "CHARACTER":U, 60, "Added<br>By<br>User":U)
    oControl:ControlToken                 = "ReadOnly":U
    oControl:ControlValue                 = "TRUE":U

    oControl                              = opoContainer:addControl("buProtocolView":U      + opoContainer:ContainerCode, "cls.mipwsembedcontainer":U, "10":U  , "tt_auth_detail.related_obj":U            , "CHARACTER":U, 61, "":U)
    oControl:RenderProcedureHandle        = mipEnv:Health:maUIService:RenderProcedureHandle
    oControl:ControlToolTip               = "View Protocol":U
    oControl:RenderProcedure              = "RenderProcedure":U
    oControl:RenderArgument               = "RenderArgument=ProtocolViewButton&BufferName=tt_auth_detail&OwningMnemonicField=owning_entity_mnemonic&OwningObjField=owning_obj&OwningKeyField=owning_key":U

    oControl                              = opoContainer:addControl("flLosCalc":U                  + ipcContainerName, "wsCheckBox":U    , "5":U , "tt_auth_provider.los_calculation":U     , "CHARACTER":U, 62, "System<br>LOS<br>Calculation":U)

    oControl                              = opoContainer:addControl("fcLosCalcRule":U              + ipcContainerName, "wsInput":U       , "22":U, "tt_auth_detail.los_calculation_rule":U  , "CHARACTER":U, 63, "LOS<br>Calculation<br>Rule":U)
    oControl:ControlToken                 = "ReadOnly":U

    oControl                              = opoContainer:addControl("flPmbIndicator":U             + ipcContainerName, "wsCheckBox":U    , "12":U, "tt_auth_detail.pmb_indicator":U         , "LOGICAL":U  , 64, "PMB<br>Indicator":U)
    oControl:ControlToken                 = "Hidden":U.

  ASSIGN
    oControl                              = opoContainer:addControl("_errorOrWarning":U          + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 65, "_authtypewarning:":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:JavascriptOnChange           = "fnOnChangeErrorOrWarning(this);":U

    oControl                              = opoContainer:addControl("_AuthStatusAction":U        + ipcContainerName, "wsInput":U       , "15":U, "":U                                , "CHARACTER":U, 66, ":":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:JavascriptOnChange           = "fnAuthStatusActionDetail(this);":U

    oControl                              = opoContainer:addControl("_authObjArgument":U           + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.auth_obj":U              , "DECIMAL":U  , 67, "":U)
    oControl:ControlToken                 = "Hidden":U

    oControl                              = opoContainer:addControl("_oemArgument":U               + ipcContainerName, "wsInput":U       , "8":U , "tt_auth_detail.owning_entity_mnemonic":U, "CHARACTER":U, 68, "":U)
    oControl:ControlToken                 = "Hidden":U

    oControl                              = opoContainer:addControl("_configuration":U             + ipcContainerName, "wsInput":U       , "15":U, "tt_auth_detail._detail_configuration":U , "CHARACTER":U, 69, "_configuration:":U)
    oControl:ControlToken                 = "Hidden":U

    oControl                              = opoContainer:addControl("_triggerRebuild":U            + ipcContainerName, "wsInput":U       , "8":U , "":U                                     , "CHARACTER":U, 70, "":U)
    oControl:ControlToken                 = "Hidden":U
    oControl:JavascriptOnChange           = "fnOnChangeAuthDetailRebuild(this);":U

    oHeaderContainer                      = NEW cls.mipwscontainer(ipcContainerName + "Header" , "99%":U, "":U, WarpSpeed:BaseClass, FALSE)

    oControl                              = oHeaderContainer:addControl("fcHeaderTitle":U  + ipcContainerName, "wsSpan":U , "10":U, "":U , "CHARACTER":U, 1,1, "":U)
    oControl:SpanOverLabel                = TRUE
    oControl:ControlValue                 = "Manual date changes - Auto save disabled for LOS lines."

    oControl                              = oHeaderContainer:addControl("buSaveDetail":U  + ipcContainerName, "wsButton":U , "10":U, "":U , "CHARACTER":U, 1,2, "":U)
    oControl:ControlSubType               = "Submit":U
    oControl:ControlToken                 = "disabled":U
    oControl:SpanOverLabel                = TRUE
    oControl:ButtonLabel                  = "Save LOS"
    oControl:CellSnippet                  = "align=right":U
    oControl:SubmitValue                  = "SaveAuthorisationFromDetail":U.


    // MMP 621 - Keeping this here , we might need this in future when whe change the Save LOS button to work via an ajax request instead of page submit. Do not remove before talking to Karl
    //oControl:JavaScriptOnClick            = "fnRebuildContainer(gcAuthDetailContainerCode, ~"&fcRequestMode=Reconstruct~", ~"mahatau~", true, ~"Saving Changes, Please Wait...~"); ":U
    // oControl:JavaScriptOnClick           = "_loadingObject = new wsLoadingDialog('Saving changes...' , true ); setTimeout(function()~{  _loadingObject.closeDialog(); _loadingObject = null ;  goDetailSaveBtn.disable();let oDetailContainer = eval(gcAuthDetailContainerCode), oLastRow   = $(oDetailContainer.containertable).find('.clmRw1,.clmRw2').last() ; if(oLastRow) oLastRow.slideDown('fast'); ~},10000);"
    ASSIGN
	  oControl:JavaScriptOnClick            = "glManualSaveDetailContainerEnabled = false; ":U
                                           	//We need to find all the disabled fields and enable, as these values will not be submitted to the server
	                                          + "$(~"body~").find(~"input:disabled~").prop(~"disabled~", false);     ":U
	                                          + "$(~"body~").find(~"select:disabled~").prop(~"disabled~", false);    ":U
	                                          + "$(~"body~").find(~"checkbox:disabled~").prop(~"disabled~", false);  ":U
                                            + " _loadingObject = new wsLoadingDialog('Saving changes...' , true ); ":U
    cSaveLOSButtonName                    = oControl:ControlName

    opoContainer:HeaderContainer          = oHeaderContainer
    opoContainer:HeaderContainerType      = "simple:form":U


    opoContainer:TableCompleteJavascript =             /* Get the warning notiication bar's div element and the Save LOS button object. NOTE: These variables are globally assigned so that they are available for use in other javascript functions(See authorisationdetail.js) */
                                            SUBSTITUTE(" goHeaderContainerDiv  = $(`[id^='&1_&1']`).first() ;                                          ":U , ipcContainerName + "Header":U )
                                         +             " goDetailSaveBtn  = eval(goHeaderContainerDiv.find(`[name^='buSaveDetail']`).attr('name'));    ":U

                                                       /* Get the tr which contains the warning bar's div */
                                         +             " var oHeaderRow  = goHeaderContainerDiv.parents('tr').first() ,                                ":U
                                                       /* Get the parent tr of the warning bar's tr . We'll call this the content row ,because this tr contains all the scrollable content in the container */
                                         +             "     oContentRow = oHeaderRow.parents('tr').first() ,                                          ":U
                                                       /* Get the detail container object */
                                         +             "     oDetail     = eval(gcAuthDetailContainerCode);                                            ":U

                                                       /* Insert the Warning bar's tr element before content row, because we do not want the warning bar to be scrollable with the rest of the contents. */
                                         +             " oContentRow.before(oHeaderRow);                                                               ":U
                                                       /* Add the notification css class (clmNot) and hide the warning    bar initially */
                                         +             " goHeaderContainerDiv.addClass('clmNot') ;                                                     ":U
                                         +             " goHeaderContainerDiv.hide() ;                                                                 ":U

                                                       /* Check if there are any rows in the container where the system calc is false. If there are then we need to display the warning bar ,but also disable the
                                                          Save LOS button. The button will enable once changes have been made to one of the date or ampm fields. */
                                         +             " $(oDetail.containertable).find('tr .clmRw1,tr .clmRw2').each(function(index,oRow)~{           ":U
                                         +             "     let oSyCalc = $(oRow).find(`[name^='flLosCalcDetail']`).first() ,                         ":U
                                         +             "         oDetailObj = $(oRow).find(`[name^='fdAuthDetailObj']`).first() ,                      ":U
                                         +             "         oOEM = $(oRow).find(`[name^='cbOEM']`).first() ,                                      ":U
                                         +             "         oOwningBtnDiv = $(oRow).find(`[id='OwningLkpDiv']`).first() ,                       ":U
                                         +             "         oOwningBtn = $(oRow).find(`[name^='buOwningBtn']`).first() ,                          ":U
                                         +             "         oOwningTrfBtn = $(oRow).find(`[name^='buOwningTrfBtn']`).first() ;                    ":U
                                         +             "     if( oSyCalc.length > 0  && $(oSyCalc).prop('checked') == false && oSyCalc.prop('type') !== 'hidden' && oDetailObj.val() > 0 )~{ ":U
                                         +             "         goHeaderContainerDiv.slideDown('fast');                                                ":U
                                         +             "         goDetailSaveBtn.disable();                                                             ":U
                                         +             "     ~}                                                                                         ":U
                                         +             "  $(oOwningBtnDiv).removeClass('clHid') ;                                                       ":U
                                         +             "     if( oOEM.length > 0  && oOEM.val() == 'htmtl' )~{                                          ":U
                                         +             "                                                                                                ":U
                                         +             "         $(oOwningBtn).addClass('clHid') ;                                                      ":U
                                         +             "     ~}                                                                                         ":U
                                         +             "     else if(oOEM.val() !== '')~{                                                               ":U
                                         +             "                                                                                                ":U
                                         +             "         $(oOwningTrfBtn).addClass('clHid') ;                                                   ":U
                                         +             "     ~}                                                                                         ":U
                                         +             "     else~{                                                                                     ":U
                                         +             "         $(oOwningBtn).addClass('clHid') ;                                                      ":U
                                         +             "         $(oOwningTrfBtn).addClass('clHid') ;                                                   ":U
                                         +             "     ~}                                                                                         ":U
                                         +             " ~});                                                                                           ":U  .

  ASSIGN oContainerProperties                        = NEW cls.wscontainerproperties(opoContainer)

         oContainerProperties:AlternateKeyFields     = "cbProvider":U       + ipcContainerName + ",":U
                                                     + "cbOEM":U            + ipcContainerName

         oContainerProperties:AutoSaveOperation      = "SERVICE:maUIService:ajaxSaveDetailContainer:":U + ipcContainerName
         oContainerProperties:DefaultLess            = TRUE
         oContainerProperties:CollapsableControlList = "cbTariffTypeCategory":U    + ipcContainerName + ",":U
                                                     + "cbRelatedEntity":U         + ipcContainerName + ",":U + "fcRelatedCode":U          + ipcContainerName + ",":U
                                                     + "buDetailRelatedBtn":U      + ipcContainerName + ",":U + "fcRelatedDescr":U         + ipcContainerName + ",":U
                                                     + "fdPaid":U                  + ipcContainerName + ",":U + "fdCopayPaid":U            + ipcContainerName + ",":U
                                                     + "buNoteBtn":U               + ipcContainerName + ",":U + "fdQuantityRequested":U    + ipcContainerName + ",":U
                                                     + "fdAmountRequested":U       + ipcContainerName + ",":U + "cbDiscountType":U         + ipcContainerName + ",":U
                                                     + "fdDiscountAuth":U          + ipcContainerName + ",":U + "fdDiscountPaid":U         + ipcContainerName + ",":U
                                                     + "fdCopayAuth":U             + ipcContainerName + ",":U + "fdCopayPercentage":U      + ipcContainerName + ",":U
                                                     + "fcCopayOverrideNote":U     + ipcContainerName + ",":U + "flLosCalc":U              + ipcContainerName + ",":U
                                                     + "fcLosCalcRule":U           + ipcContainerName + ",":U + "fdAuthGroupObj":U         + ipcContainerName + ",":U
                                                     + "fcTariffReason":U          + ipcContainerName + ",":U + "fdFixedItemCost":U        + ipcContainerName + ",":U
                                                     + "fcOverrideBaseRate":U      + ipcContainerName + ",":U + "fcOverrideArsRate":U      + ipcContainerName + ",":U
                                                     + "buOverrideArsRateBtn":U    + ipcContainerName + ",":U + "flAddedByUser":U          + ipcContainerName + ",":U
                                                     + "flRepeatItem":U            + ipcContainerName + ",":U + "fiRepeatCycleAuth":U      + ipcContainerName + ",":U
                                                     + "fiRepeatCyclePaid":U       + ipcContainerName + ",":U + "fiRepeatCycleQuantity":U  + ipcContainerName + ",":U
                                                     + "fiRepeatCycleDays":U       + ipcContainerName + ",":U + "fiRepeatCycleGraceDays":U + ipcContainerName + ",":U
                                                     + "fdRepeatLastClaimedDate":U + ipcContainerName + ",":U + "fiMinRequested":U         + ipcContainerName

       oContainerProperties:IgnoreFieldList        = "_configuration":U + ipcContainerName.

  mipEnv:Health:maUiService:prepareCustomizedContainer(INPUT opoContainer, INPUT oContainerProperties).

&ENDIF








