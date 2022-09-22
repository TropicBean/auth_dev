/* maauthds.i MEDSTAR Medical Aid System
              Healthcare auth dataset definition
              (c) Copyright 2015 - 2022
              MIP Holdings (Pty) Ltd
              All rights reserved                
*/

/* ***************************  Definitions  ************************** */
{sysadmma.i}


&IF DEFINED(REFERENCE-ONLY) > 0 
&THEN 
  &SCOPED-DEFINE ReferenceOnly {&REFERENCE-ONLY} 
&ELSE 
  &SCOPED-DEFINE ReferenceOnly 
&ENDIF

{ ma/inc/maauthtt.i                &TEMP-TABLE-NAME = "tt_auth"                    &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthdetailtt.i          &TEMP-TABLE-NAME = "tt_auth_detail"             &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthcodingtt.i          &TEMP-TABLE-NAME = "tt_auth_coding"             &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthepisodett.i         &TEMP-TABLE-NAME = "tt_auth_episode"            &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthmcsavingstt.i       &TEMP-TABLE-NAME = "tt_auth_mc_savings"         &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthprovidertt.i        &TEMP-TABLE-NAME = "tt_auth_provider"           &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthcrosswalktt.i       &TEMP-TABLE-NAME = "tt_auth_crosswalk"          &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthcopaytt.i           &TEMP-TABLE-NAME = "tt_auth_copay"              &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthlimittt.i           &TEMP-TABLE-NAME = "tt_auth_limit"              &REFERENCE-ONLY = {&ReferenceOnly}}

/*
  History tables
*/
{ ma/inc/maauthhisttt.i          &TEMP-TABLE-NAME = "tt_auth_history"            &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthdethisttt.i       &TEMP-TABLE-NAME = "tt_auth_detail_history"     &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthcodinghisttt.i    &TEMP-TABLE-NAME = "tt_auth_coding_history"     &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthepihisttt.i       &TEMP-TABLE-NAME = "tt_auth_episode_history"    &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthmcsavhisttt.i     &TEMP-TABLE-NAME = "tt_auth_mc_savings_history" &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthprovhisttt.i      &TEMP-TABLE-NAME = "tt_auth_provider_history"   &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthcrosswalkhisttt.i &TEMP-TABLE-NAME = "tt_auth_crosswalk_history"  &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthcopayhisttt.i     &TEMP-TABLE-NAME = "tt_auth_copay_history"      &REFERENCE-ONLY = {&ReferenceOnly}}                                                                              
{ ma/inc/maauthlimithisttt.i     &TEMP-TABLE-NAME = "tt_auth_limit_history"      &REFERENCE-ONLY = {&ReferenceOnly}}

/*                                                                            
  Result & Error                                                              
*/                                                                            
{ ma/inc/maerrortt.i          &TEMP-TABLE-NAME = "tt_auth_error"  &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maresulttt.i         &TEMP-TABLE-NAME = "tt_auth_result" &REFERENCE-ONLY = {&ReferenceOnly}}

DEFINE DATASET dsAuthorisation {&ReferenceOnly}
  FOR tt_auth,
      tt_auth_provider,
      tt_auth_detail,
      tt_auth_coding,
      tt_auth_episode,
      tt_auth_mc_savings,
      tt_auth_crosswalk,
      tt_auth_copay,
      tt_auth_limit,
      tt_auth_history,
      tt_auth_detail_history,
      tt_auth_coding_history,
      tt_auth_episode_history,
      tt_auth_mc_savings_history,
      tt_auth_provider_history,
      tt_auth_crosswalk_history,
      tt_auth_copay_history,
      tt_auth_limit_history,
      tt_auth_result,
      tt_auth_error
  
  DATA-RELATION FOR tt_auth           , tt_auth_episode            RELATION-FIELDS(tt_auth.auth_episode_obj           , tt_auth_episode.auth_episode_obj         ) NESTED   
  DATA-RELATION FOR tt_auth           , tt_auth_coding             RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_coding.auth_obj                  ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_provider           RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_provider.auth_obj                ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_mc_savings         RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_mc_savings.auth_obj              ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_detail             RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_detail.auth_obj                  ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_crosswalk          RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_crosswalk.auth_obj               ) NESTED
  DATA-RELATION FOR tt_auth           , tt_auth_copay              RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_copay.auth_obj                   ) NESTED
  DATA-RELATION FOR tt_auth           , tt_auth_limit              RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_limit.auth_obj                   ) NESTED
                                                                                                                                                                   
  /*                                                                                                                                                               
    History Relations                                                                                                                                              
  */                                                                                                                                                               
  DATA-RELATION FOR tt_auth           , tt_auth_history            RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_history.auth_obj                 ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_coding_history     RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_coding_history.auth_obj          ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_mc_savings_history RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_mc_savings_history.auth_obj      ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_provider_history   RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_provider_history.auth_obj        ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_detail_history     RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_detail_history.auth_obj          ) NESTED 
  DATA-RELATION FOR tt_auth           , tt_auth_crosswalk_history  RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_crosswalk_history.auth_obj       ) NESTED
  DATA-RELATION FOR tt_auth           , tt_auth_copay_history      RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_copay_history.auth_obj           ) NESTED
  DATA-RELATION FOR tt_auth           , tt_auth_limit_history      RELATION-FIELDS(tt_auth.auth_obj                   , tt_auth_limit_history.auth_obj           ) NESTED
  DATA-RELATION FOR tt_auth_episode   , tt_auth_episode_history    RELATION-FIELDS(tt_auth_episode.auth_episode_obj   , tt_auth_episode_history.auth_episode_obj ) NESTED 
  .
    
/* *********************  Temp-Table Definitions ********************** */
  DEFINE TEMP-TABLE ttAuthStatus NO-UNDO
    FIELD status_code        AS INTEGER   FORMAT "9"
    FIELD status_description AS CHARACTER FORMAT "X(10)".
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */








