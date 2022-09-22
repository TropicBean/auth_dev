
/* maauthflagvalueds.i MEDSTAR Medical Aid System
                       Healthcare Auth Flag Values Dataset
                       (c) Copyright 2016 - 2018
                       MIP Holdings (Pty) Ltd
                       All rights reserved                
*/
  
{sysadmma.i}

{ ma/inc/maauthflagvaluett.i      &TEMP-TABLE-NAME = "tt_auth_flag_value"         &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthrulett.i           &TEMP-TABLE-NAME = "tt_auth_flag_value_rule"    &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthflagvaluehisttt.i  &TEMP-TABLE-NAME = "tt_auth_flag_value_history" &REFERENCE-ONLY = {&ReferenceOnly}}  
{ ma/inc/maerrortt.i              &TEMP-TABLE-NAME = "tt_auth_flag_value_error"   &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maresulttt.i             &TEMP-TABLE-NAME = "tt_auth_flag_value_result"  &REFERENCE-ONLY = {&ReferenceOnly}}

DEFINE DATASET dsAuthFlagValue
  FOR tt_auth_flag_value, 
      tt_auth_flag_value_rule,
      tt_auth_flag_value_history,
      tt_auth_flag_value_error,
      tt_auth_flag_value_result
      
  DATA-RELATION FOR tt_auth_flag_value, tt_auth_flag_value_rule    RELATION-FIELDS(tt_auth_flag_value.auth_rule_obj      , tt_auth_flag_value_rule.auth_rule_obj)
  DATA-RELATION FOR tt_auth_flag_value, tt_auth_flag_value_history RELATION-FIELDS(tt_auth_flag_value.auth_flag_value_obj, tt_auth_flag_value_history.auth_flag_value_obj).
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


