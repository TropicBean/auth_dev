/* maauthservgetauthtrfrestr.i MEDSTAR Medical Aid System
                               Healthcare Auth data access service: getAuthTariffRestriction                          
                               (c) Copyright 2021 - 2022
                               MIP Holdings (Pty) Ltd
                               All rights reserved
------------------------------------------------------------------------------
  Purpose:  Check whether any Tariff Restrictions apply for a tariff code captured on 
            the clinicial detail lines of the auth.

  Notes  :  Return a list of Tariff Restrictions that apply, as well as a message
            
------------------------------------------------------------------------------*/
  DEFINE INPUT         PARAMETER ipcTariffCode          AS CHARACTER   NO-UNDO.
  DEFINE INPUT         PARAMETER ipdTariffEffectDate    AS DATE        NO-UNDO.
  DEFINE INPUT         PARAMETER ipcTariffBaseRate      AS CHARACTER   NO-UNDO.
  DEFINE INPUT         PARAMETER ipcTariffArsRate       AS CHARACTER   NO-UNDO.
  DEFINE INPUT         PARAMETER ipdAuthProviderObj     AS DECIMAL     NO-UNDO.
  DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR tt_auth_detail.
  DEFINE OUTPUT        PARAMETER opcRestrictionTariff   AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT        PARAMETER opcRestrictionMessage  AS CHARACTER   NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE VARIABLE lSuccess                       AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE lProvider                      AS LOGICAL  FORMAT "Y/N"  NO-UNDO.
  DEFINE VARIABLE lValidRule                     AS LOGICAL                NO-UNDO.
  DEFINE VARIABLE cRuleValue                     AS CHARACTER              NO-UNDO.

  DEFINE BUFFER btt_auth FOR tt_auth.

  /*
    Find first auth
  */
  FIND FIRST btt_auth NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  /*
    If there's no auth record, just leave
  */
  IF NOT AVAILABLE btt_auth 
  THEN
    RETURN.

  /*
    Determine if provider must be included in the Tariff Restriction check 
  */
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                 INPUT  btt_auth.option_code,
                                                 INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                 INPUT  "TariffRestrictionProvider":U,
                                                 INPUT  ipdTariffEffectDate,
                                                 OUTPUT lValidRule,
                                                 OUTPUT cRuleValue).
  IF lValidRule
  AND cRuleValue = "Activate":U 
  THEN ASSIGN lProvider = YES.
  ELSE ASSIGN lProvider = NO.

  /* 
    Disallow-code : Check if any records exist on table 'trfdisallow' for trfdisallow.disallow-code
  */
  FOR EACH trfdisallow NO-LOCK
    WHERE trfdisallow.disallow-code = ipcTariffCode
      AND trfdisallow.effect-date   = ipdTariffEffectDate
      AND trfdisallow.base-rate     = ipcTariffBaseRate
      AND trfdisallow.ars-rate      = ipcTariffArsRate:
    
    IF (lProvider 
    AND CAN-FIND(FIRST tt_auth_detail NO-LOCK
                    WHERE tt_auth_detail.auth_provider_obj = ipdAuthProviderObj
                      AND tt_auth_detail.owning_alt_value  = trfdisallow.tariff-code
                      AND tt_auth_detail.auth_status       <> 6))     /* not declined */
    OR (NOT lProvider
    AND CAN-FIND(FIRST tt_auth_detail NO-LOCK
                    WHERE tt_auth_detail.owning_alt_value = trfdisallow.tariff-code
                      AND tt_auth_detail.auth_status <> 6))           /* not declined */
    THEN 
      ASSIGN opcRestrictionTariff = IF opcRestrictionTariff = ""
                                    THEN trfdisallow.tariff-code
                                    ELSE opcRestrictionTariff + "," + trfdisallow.tariff-code.

  END.  /* FOR EACH trfdisallow NO-LOCK */

  IF opcRestrictionTariff <> "" THEN 
  DO: 
    ASSIGN opcRestrictionMessage = "Tariff code conflicts with other tariff codes used. [HELP=Auth Rule Code: TariffRestrictionProvider]".
    LEAVE.
  END.  /* IF opcRestrictionTariff <> "" THEN */

  /* 
    Tariff-code : Check if any records exist on table 'trfdisallow' for trfdisallow.tariff-code
  */
  FOR EACH trfdisallow NO-LOCK
    WHERE trfdisallow.tariff-code = ipcTariffCode
      AND trfdisallow.effect-date = ipdTariffEffectDate 
      AND trfdisallow.base-rate   = ipcTariffBaseRate
      AND trfdisallow.ars-rate    = ipcTariffArsRate:    
    
    IF (lProvider 
    AND CAN-FIND(FIRST tt_auth_detail NO-LOCK
                    WHERE tt_auth_detail.auth_provider_obj = ipdAuthProviderObj
                      AND tt_auth_detail.owning_alt_value  = trfdisallow.disallow-code
                      AND tt_auth_detail.auth_status      <> 6))     /* not declined */
    OR (NOT lProvider
    AND CAN-FIND(FIRST tt_auth_detail NO-LOCK
                    WHERE tt_auth_detail.owning_alt_value = trfdisallow.disallow-code
                      AND tt_auth_detail.auth_status     <> 6))      /* not declined */
    THEN 
      ASSIGN opcRestrictionTariff = IF opcRestrictionTariff = ""
                                    THEN trfdisallow.disallow-code
                                    ELSE opcRestrictionTariff + "," + trfdisallow.disallow-code.

  END.  /* FOR EACH trfdisallow NO-LOCK */

  IF opcRestrictionTariff <> "" THEN 
  DO: 
    ASSIGN opcRestrictionMessage = "Tariff code may not be used with ":U.
    LEAVE.
  END.  /* IF opcRestrictionTariff <> "" THEN */
  
&ENDIF   
   
   





