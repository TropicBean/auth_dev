/* maauthbuspopadddetail.i  MEDSTAR Medical Aid System
                            Populate additional data for Detail Container
                            Used for performance improvement when
                            rendering the container
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/
DEFINE VARIABLE cEntityMnemonic AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dObj            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cKey            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE oNappi          AS cls.manappi  NO-UNDO.

DEFINE BUFFER bbtt_auth_detail FOR btt_auth_detail.

FOR EACH btt_auth_detail:

  ASSIGN
    cOEM             = btt_auth_detail.owning_entity_mnemonic
    cOwningAltValue  = btt_auth_detail.owning_alt_value
    dOwningObj       = btt_auth_detail.owning_obj
    dEffectiveDate   = btt_auth_detail.start_date
   .

  FIND FIRST btt_auth_provider NO-LOCK
    WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
    NO-ERROR.
  {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE btt_auth_provider THEN
  DO:
    ASSIGN
      dStartDate     = DATE(btt_auth_provider.start_date)
      iGroupDocNum   = INTEGER(btt_auth_provider.group_doc_num)
      iDocNum        = INTEGER(btt_auth_provider.doc_num)
      cDiscipline    = STRING(btt_auth_provider.pr_type)
      cSubDiscipline = STRING(btt_auth_provider.sub_pr_type)

      lGroupProvider = (iGroupDocNum <> 0 AND mipEnv:Health:maDoctor:isProviderAValidGroup(INPUT iGroupDocNum, INPUT dStartDate) OR (iGroupDocNum <> 0.00 AND iDocNum <> 0.00))
      lGroupProvider = IF iGroupDocNum = iDocNum THEN NO ELSE lGroupProvider.

    RUN _getConfiguration IN TARGET-PROCEDURE (BUFFER btt_auth,
                                               BUFFER btt_auth_provider,
                                               OUTPUT lcConfiguration).

    ASSIGN
      btt_auth_detail._group_provider        = lGroupProvider
      btt_auth_detail._detail_configuration  = lcConfiguration.

  END. /* IF AVAILABLE btt_auth_provider THEN */

  /*
    When we are dealing with an LOS/LOC record then the loc quantity and loc sequence fields should be enabled
    otherwise we will restore the control token back to what it was when it was defined.
  */
  FIND FIRST htm_tariff_type NO-LOCK
       WHERE htm_tariff_type.tariff_type_obj = btt_auth_detail.loc_tariff_type_obj
         AND htm_tariff_type.acronym_key     = "ma_acTariffTypeCatLOC":U
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE htm_tariff_type THEN
  DO:
    ASSIGN
      btt_auth_detail._tariff_type_acronym_key = htm_tariff_type.acronym_key.
  END. /* IF AVAILABLE htm_tariff_type THEN */

  IF cOEM <> "":U AND cOEM <> ? THEN
  DO:
    CASE cOEM:
      /*
        Basket's
      */
      WHEN "hlmcr":U THEN
      DO:
        FIND FIRST hlm_crosswalk_parameter NO-LOCK
             WHERE hlm_crosswalk_parameter.crosswalk_obj = btt_auth_detail.owning_obj
               AND hlm_crosswalk_parameter.end_date      = ?
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}

        IF AVAILABLE hlm_crosswalk_parameter
        THEN
          ASSIGN btt_auth_detail._crosswalk_parameter_obj = hlm_crosswalk_parameter.crosswalk_parameter_obj.
        ELSE
          ASSIGN btt_auth_detail._crosswalk_parameter_obj = 0.00.
      END. /* WHEN "hlmcr":U THEN */

      /*
        Nappi's
      */
      WHEN "hlmnl":U THEN
      DO:
        oNappi = NEW cls.manappi(cOwningAltValue).

        IF oNappi:NappiInFocus
        THEN
          ASSIGN btt_auth_detail._nappi_obj = oNappi:NappiObj.
        ELSE
          ASSIGN btt_auth_detail._nappi_obj = 0.00.
      END. /* WHEN "hlnl":U THEN */

      /*
       Tariff's
      */
      WHEN "htmtl":U THEN
      DO:
        /*
          Find base rate and ars rate for the provider
        */
        IF btt_auth_detail.start_date <> ?
        THEN
          mipEnv:Health:maDoctor:getProviderBaseRates(INPUT iDocNum,
                                                      INPUT cMemNum,
                                                      INPUT iOption,
                                                      INPUT btt_auth_detail.start_date,
                                                      OUTPUT cBaseRate,
                                                      OUTPUT cArsRate).
        ELSE
          mipEnv:Health:maDoctor:getProviderBaseRates(INPUT iDocNum,
                                                      INPUT cMemNum,
                                                      INPUT iOption,
                                                      INPUT dStartDate,
                                                      OUTPUT cBaseRate,
                                                      OUTPUT cArsRate).
        ASSIGN
          btt_auth_detail._base_rate = cBaseRate
          btt_auth_detail._ars_rate  = cArsRate.

        /* Pad discipline */
        DO WHILE LENGTH(cDiscipline ,"CHARACTER":U) < 3 :
          ASSIGN cDiscipline = "0":U + cDiscipline.
        END.  /* DO WHILE LENGTH(cDiscipline ,"CHARACTER":U) < 3 : */

        /*
          Make sure the discipline is padded before searching with it
        */
        FIND FIRST htm_tariff_link NO-LOCK
             WHERE htm_tariff_link.tariff_code = btt_auth_detail.owning_alt_value
               AND htm_tariff_link.base_rate   = cBaseRate
               AND htm_tariff_link.pr_type     = cDiscipline
               AND htm_tariff_link.ars_rate    = cArsRate
          NO-ERROR.
        {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
        IF AVAILABLE htm_tariff_link THEN
        DO:
          /*
            Find latest applicable tariff for the tariff code,base rate, ars rate etc
          */
          TariffBlk:
          FOR EACH tariff NO-LOCK
             WHERE tariff.tariff-code  = btt_auth_detail.owning_alt_value
               AND tariff.base-rate    = cBaseRate
               AND tariff.pr-type      = cDiscipline
               AND tariff.ars-rate     = cArsRate
               AND tariff.effect-date <= btt_auth_detail.start_date
                BY tariff.effect-date DESCENDING:
            ASSIGN
              btt_auth_detail._nappi_required    = tariff.nappi-required-auths = "ma_acNappiRequiredEnforce":U
              btt_auth_detail._tariff_effec_date = tariff.effect-date.
            LEAVE TariffBlk.
          END. /*FOR EACH tariff NO-LOCK */
        END.  /* IF AVAILABLE htm_tarif_link THEN */

      END. /* WHEN "htmtl":U THEN */

    END CASE. /* CASE cOEM: */
  END. /* IF cOEM <> "":U AND cOEM <> ? THEN */

  IF btt_auth_detail.owning_entity_mnemonic <> "":U
  THEN DO:
    mipEnv:miDBEntity:focusTable(btt_auth_detail.owning_entity_mnemonic).

    /*
      Because we are using a slent lookup for multiple entitites, we will be using a rowid for
      the lookup return values
    */
    IF mipEnv:miDBEntity:InFocus THEN
    DO:

      ASSIGN lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ"
                         THEN mipEnv:miDBEntity:findRecord(btt_auth_detail.owning_obj)
                         ELSE mipEnv:miDBEntity:findRecord(btt_auth_detail.owning_key)).

      IF mipEnv:miDBEntity:RecordAvailable
      THEN
        ASSIGN
          btt_auth_detail._row_id                    = STRING(mipEnv:miDBEntity:RecordRowid)
          btt_auth_detail._detail_owning_description = STRING(mipEnv:miDBEntity:RecordDescription).

    END. /*IF mipEnv:miDBEntity:InFocus THEN*/
    ELSE
      ASSIGN
        btt_auth_detail._row_id                    = "":U
        btt_auth_detail._detail_owning_description = "":U.

  END. /* IF tt_auth_detail.owning_entity_mnemonic <> "":U */

  IF btt_auth_detail.related_entity_mnemonic <> "":U
  THEN DO:
    IF btt_auth_detail.related_entity_mnemonic = "hatad":U THEN
      ASSIGN btt_auth_detail._detail_related_description = "".
    ELSE
    DO:
      ASSIGN cEntityMnemonic = btt_auth_detail.related_entity_mnemonic
             dObj            = btt_auth_detail.related_obj
             cKey            = btt_auth_detail.related_key.
      mipEnv:miDBEntity:focusTable(cEntityMnemonic).

      IF mipEnv:miDBEntity:InFocus THEN
      DO:
        ASSIGN
          lSuccess = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U
                      THEN mipEnv:miDBEntity:findRecord(dObj)
                      ELSE mipEnv:miDBEntity:findRecord(cKey)).

        IF mipEnv:miDBEntity:RecordAvailable
        THEN
          ASSIGN
            btt_auth_detail._detail_related_description = STRING(mipEnv:miDBEntity:RecordDescription).

      END. /*IF mipEnv:miDBEntity:InFocus THEN*/
      ELSE
        ASSIGN
          btt_auth_detail._detail_related_description = "":U.

    END. /*ELSE*/
  END. /*IF btt_auth_detail.related_entity_mnemonic <> "":U*/

  IF btt_auth_detail.loc_tariff_type_obj <> 0 AND btt_auth_detail.loc_tariff_type_obj <> ?
  THEN
  DO:
    FOR FIRST htm_tariff_type NO-LOCK
        WHERE htm_tariff_type.tariff_type_obj = btt_auth_detail.loc_tariff_type_obj :

      FIND FIRST mic_acronym NO-LOCK
           WHERE mic_acronym.acronym_key  = htm_tariff_type.acronym_key
        NO-ERROR.
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE mic_acronym
      THEN
        ASSIGN
          btt_auth_detail._loc_tariff_type_category = TRIM(mic_acronym.acronym_key).
    END. /* FOR FIRST htm_tariff_type */
  END. /* IF btt_auth_detail.loc_tariff_type_obj <> 0 AND btt_auth_detail.loc_tariff_type_obj <> ? */
  ELSE
    ASSIGN
      btt_auth_detail._loc_tariff_type_category = "":U.

  /* Determine default Payee indicator value from Rule setup */  
  IF iOption = 0
  THEN mipEnv:Health:maMember:getMemberOption(INPUT  cMemNum, 
                                              INPUT  btt_auth_detail.start_date, 
                                              OUTPUT iOption). 

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj,
                                                 INPUT  iOption,
                                                 INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                 INPUT  "DetailPayeeDefault":U,
                                                 INPUT  btt_auth_detail.start_date,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).

  ASSIGN btt_auth_detail.payee_dm  = IF lValidRule AND cRuleValue <> "":U AND btt_auth_detail.payee_dm = "":U 
                                     THEN cRuleValue 
                                     ELSE btt_auth_detail.payee_dm.

  IF btt_auth_detail.note <> "":U THEN
  DO:
    FIND FIRST note NO-LOCK
         WHERE note.key = btt_auth_detail.note
      NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE note
    THEN
      ASSIGN btt_auth_detail._note_narration = note.narration[1].
  END. /* IF btt_auth_detail.note <> "":U THEN */

  ASSIGN
    lMandatory                 = mipEnv:Health:AuthService:statusReasonMandatory
                                 (INPUT INTEGER(btt_auth_detail.auth_status),
                                  INPUT dInsurerObj,
                                  INPUT iOption,
                                  INPUT DATE(btt_auth_detail.start_date))

    btt_auth_detail._mandatory = lMandatory.

  IF btt_auth_detail.cpt_link_obj <> 0.00 AND btt_auth_detail.cpt_link_obj <> ? THEN
  DO:
    FOR FIRST hlm_cpt_link NO-LOCK
        WHERE hlm_cpt_link.cpt_link_obj = btt_auth_detail.cpt_link_obj:

      ASSIGN btt_auth_detail._cpt_code = hlm_cpt_link.cpt_code.
    END. /*FOR FIRST hlm_cpt_link NO-LOCK*/
  END. /* IF btt_auth_detail.cpt_link_obj <> 0.00 AND btt_auth_detail.cpt_link_obj <> ? THEN */
  ELSE
    ASSIGN btt_auth_detail._cpt_code = "":U.

  FOR EACH btt_auth_provider NO-LOCK
     WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj,
     FIRST tt_auth NO-LOCK
     WHERE tt_auth.auth_obj = btt_auth_provider.auth_obj:
    /*
      Negotiation Group
    */
    ASSIGN
      iSearchDoctor = IF btt_auth_provider.group_doc_num <> 0
                      THEN btt_auth_provider.group_doc_num
                      ELSE btt_auth_provider.doc_num
          cNegGroup = "":U .

    IF tt_auth.option_code <> 0 AND iSearchDoctor > 0
    THEN
      mipEnv:Health:maDoctor:getNegotiationGroup(INPUT iSearchDoctor,
                                                 INPUT tt_auth.mem_num,
                                                 INPUT tt_auth.option_code,
                                                 OUTPUT cNegGroup).


    ASSIGN
      btt_auth_detail._negotiation_group = cNegGroup.

    /*
      Override Base Rate and Ars Rate
    */
    EMPTY TEMP-TABLE ttAuthTypeConfig.

    /*
      Get config to determine whether to enable/disable base rate, ars rate
    */
    mipEnv:Health:AuthService:getAuthTypeConfig(BUFFER btt_auth,
                                                BUFFER btt_auth_provider, 
                                                INPUT-OUTPUT TABLE ttAuthTypeConfig BY-REFERENCE).

    FIND FIRST ttAuthTypeConfig NO-LOCK NO-ERROR.
    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE ttAuthTypeConfig THEN
    DO:

      IF LOOKUP(btt_auth_detail.owning_entity_mnemonic, "htmtl,hlmcr":U) > 0
      THEN DO:
        ASSIGN
          btt_auth_detail._base_rate_upd_allowed = ttAuthTypeConfig.BaseRateUpdAllowed
          btt_auth_detail._ars_rate_upd_allowed  = ttAuthTypeConfig.ArsRateUpdAllowed.
      END. /* IF INDEX(btt_auth_detail.owning_entity_mnemonic, "htmtl,hlmcr":U) > 0 */

    END. /* IF AVAILABLE ttAuthTypeConfig THEN  */

  END. /* FOR EACH btt_auth_provider NO-LOCK */

  IF btt_auth_detail.quantity_los <> 0
  THEN ASSIGN btt_auth_detail._loc_line = TRUE.
 END. /* FOR EACH btt_auth_detail: */


/*
Prepare a base rate list for the user to select
*/
FOR EACH baserate NO-LOCK
  BY baserate.base-rate:

  ASSIGN cBaseRateList = cBaseRateList
                       + (IF cBaseRateList = '' THEN '' ELSE "|":U)
                       + baserate.base-rate.
END. /*FOR EACH baserate NO-LOCK*/

FOR EACH mic_acronym NO-LOCK
  WHERE mic_acronym.category_key = "ma_acAuthLineRestriction":U
     BY mic_acronym.acronym_sequence:

  ASSIGN cLineRestriction = cLineRestriction
                          + (IF cLineRestriction = "" THEN "":U ELSE "|":U)
                          + mic_acronym.acronym_label + "=":U + mic_acronym.acronym_key .
END. /* FOR EACH mic_acronym NO-LOCK */
