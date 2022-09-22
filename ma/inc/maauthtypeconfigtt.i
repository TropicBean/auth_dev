/* maauthtypeconfigtt.i MEDSTAR Medical Aid System
                        Healthcare temp table definition
                        (c) Copyright 2018 - 2022
                        MIP Holdings (Pty) Ltd
                        All rights reserved
*/

/* ************************************  Definitions  *********************************** */
/* &ACCESS is used to make temp table NEW GLOBAL SHARED PROTECTED PRIVATE or STATIC       */
/* BEFORE is used to define a before table for the temp table                             */

/* Some other options that can be specified on fields :                                   */
/*   FORMAT "x(36)":U LABEL "MenuGuid":T INIT 0                                           */
/*   SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U       */

DEFINE TEMP-TABLE ttAuthTypeConfig NO-UNDO 
  FIELD AuthTypeObj                         AS DECIMAL     INITIAL ?    FORMAT "->>>,>>9.99"
  FIELD InsurerObj                          AS DECIMAL     INITIAL ?    FORMAT "->>>,>>9.99"
  FIELD OptionCode                          AS INTEGER     INITIAL ?    FORMAT ">9"
  FIELD ActivateAmPm                        AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateAuthorisedValues            AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateBodyRegion                  AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateCopayment                   AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateDueDate                     AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateEpisodeNumber               AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateLosWeekendPass              AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateMouthPartId                 AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivatePenalty                     AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateServiceType                 AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD AgeRangeBothObj                     AS DECIMAL     INITIAL ?    FORMAT ">>>>>>>>>>>>>>>>>>9.999999"
  FIELD AgeRangeFemaleObj                   AS DECIMAL     INITIAL ?    FORMAT ">>>>>>>>>>>>>>>>>>9.999999"
  FIELD AgeRangeMaleObj                     AS DECIMAL     INITIAL ?    FORMAT ">>>>>>>>>>>>>>>>>>9.999999"
  FIELD AmountAuth                          AS DECIMAL     INITIAL ?    FORMAT "->>>,>>9.99"
  FIELD ArsRateUpdAllowed                   AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ArsRateUpdAllowedRole               AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ArsRateUpdAllowedUser               AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD AuthEndDate                         AS DATE        INITIAL ?    FORMAT 9999/99/99
  FIELD AuthEndDateUpdAllowed               AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD AuthoriseAllServices                AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD AuthoriseDetailLines                AS CHARACTER   INITIAL ?    FORMAT "x(30)"
  FIELD AuthorisedService                   AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD AuthType                            AS CHARACTER   INITIAL ?    FORMAT "x(10)"
  FIELD AuthTypePrefix                      AS CHARACTER   INITIAL ?    FORMAT "x(5)"
  FIELD AuthTypeRestrictions                AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD BaseRateUpdAllowed                  AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD BaseRateUpdAllowedRole              AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD BaseRateUpdAllowedUser              AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ClaimCodes                          AS CHARACTER   INITIAL ?    FORMAT "x(80)"  
  FIELD ClaimCodesDetail                    AS CHARACTER   INITIAL ?    FORMAT "x(80)"  
  FIELD ClaimCodesDisallow                  AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ClaimCodeUpdAllowed                 AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ClaimCodeUpdRole                    AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ClaimCodeUpdUser                    AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ClaimTypes                          AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ClaimTypesDetail                    AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ClaimTypesDisallow                  AS CHARACTER   INITIAL ?    FORMAT "x(10)"
  FIELD ClaimTypeUpdAllowed                 AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ClaimTypeUpdRole                    AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ClaimTypeUpdUser                    AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ControlTypeIndicator                AS CHARACTER   INITIAL ?    FORMAT "x(40)"
  FIELD DefaultAuthStatus                   AS CHARACTER   INITIAL ?    FORMAT "x(1)"
  FIELD DefaultAuthStatusNote               AS CHARACTER   INITIAL ?    FORMAT "x(3)"
  FIELD DefaultAuthStatusUpdAllowed         AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD DefaultAuthStatusUpdRole            AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD DefaultAuthStatusUpdUser            AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD DefaultClaimCodeDetail              AS INTEGER     INITIAL 0    FORMAT "999"
  FIELD DefaultClaimTypeDetail              AS CHARACTER   INITIAL ?    FORMAT "x(1)"
  FIELD DefaultLineRestriction              AS CHARACTER   INITIAL ?    FORMAT "x(20)"
  FIELD DocNumMandatory                     AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD EffectiveDate                       AS DATE        INITIAL ?    FORMAT 9999/99/99 
  FIELD EndDate                             AS DATE        INITIAL ?    FORMAT 9999/99/99  
  FIELD EndDateUpdAllowed                   AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD EndDateUpdRole                      AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD EndDateUpdUsr                       AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD EnforceAuthorisedValues             AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD EnforceHeaderClaimCodeMatch         AS LOGICAL     INITIAL ?    FORMAT "Y/N"
  FIELD EnforceHeaderClaimTypeMatch         AS LOGICAL     INITIAL ?    FORMAT "Y/N"
  FIELD EpisodeUpdAllowed                   AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD Gender                              AS CHARACTER   INITIAL "B"  FORMAT "!(1)"
  FIELD HeaderValuesAllowed                 AS CHARACTER   INITIAL ?    FORMAT "x(50)"
  FIELD HeaderValuesUnlimited               AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD IcdCondCode                         AS CHARACTER   INITIAL ?    FORMAT "x(8)"
  FIELD IcdCondType                         AS CHARACTER   INITIAL ?    FORMAT "x(8)"
  FIELD MultipleCCMessageType               AS CHARACTER   INITIAL ?    FORMAT "x(10)"
  FIELD MainProvider                        AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD Mandatory                           AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD NegNum                              AS INTEGER     INITIAL ?    FORMAT ">>>9"
  FIELD NumberProvidersAllowed              AS INTEGER     INITIAL ?    FORMAT ">9"
  FIELD Period                              AS INTEGER     INITIAL ?    FORMAT ">>>>9"
  FIELD PeriodOverride                      AS LOGICAL     INITIAL ?    FORMAT "Y/N"
  FIELD PeriodType                          AS CHARACTER   INITIAL ?    FORMAT "x(8)"
  FIELD ProviderSequence                    AS INTEGER     INITIAL ?    FORMAT ">>9"
  FIELD ProviderType                        AS CHARACTER   INITIAL ?    FORMAT "x(20)"
  FIELD ProviderTypeIndicator               AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD PrTypeList                          AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD PrTypeValidList                     AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD QuantityAuth                        AS INTEGER     INITIAL ?    FORMAT ">>>>9"
  FIELD SequenceKey                         AS CHARACTER   INITIAL ?    FORMAT "x(20)"
  FIELD UpdatesAllowed                      AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD UsageOverrideUser                   AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD UsagePeriod                         AS INTEGER     INITIAL ?    FORMAT ">>>>9"
  FIELD UsagePeriodType                     AS CHARACTER   INITIAL ?    FORMAT "x(8)"
  FIELD UsageQuantity                       AS INTEGER     INITIAL ?    FORMAT ">>>>9"
  FIELD ValidClaimCodesDetail               AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ValidClaimTypesDetail               AS CHARACTER   INITIAL ?    FORMAT "x(10)"
  FIELD ValidIcds                           AS CHARACTER   INITIAL ?    FORMAT "x(10)"
  FIELD SessionDateFormat                   AS CHARACTER   INITIAL ?    FORMAT "x(30)"
  FIELD StatusWarnContainers                AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD AuthTypeConfigKey                   AS CHARACTER       
  FIELD UsageType                           AS CHARACTER   INITIAL ?    FORMAT "x(20)"
  FIELD ActivateLos                         AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD AuthTypeGroup                       AS CHARACTER   INITIAL ?    FORMAT "x(20)"
  FIELD AuthTypeGroupLabel                  AS CHARACTER   INITIAL ?    FORMAT "x(20)"
  FIELD ActivateHealth                      AS CHARACTER   INITIAL ?    FORMAT "x(80)"
  FIELD ActivateCrosswalk                   AS LOGICAL     INITIAL ?    FORMAT "yes/no"
  FIELD ActivateCodelink                    AS LOGICAL     INITIAL ?    FORMAT "yes/no"

  
  INDEX idx1 AS PRIMARY UNIQUE AuthTypeConfigKey
  .

