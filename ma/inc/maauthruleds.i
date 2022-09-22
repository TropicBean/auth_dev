/* maauthruleds.i MEDSTAR Medical Aid System
                  Healthcare Auth Rules Dataset
                  (c) Copyright 2017
                  MIP Holdings (Pty) Ltd
                  All rights reserved                
*/
  
{sysadmma.i}

{ ma/inc/maauthrulett.i        &TEMP-TABLE-NAME = "tt_auth_rule"        }
{ ma/inc/maerrortt.i           &TEMP-TABLE-NAME = "tt_auth_rule_error"  }
{ ma/inc/maresulttt.i          &TEMP-TABLE-NAME = "tt_auth_rule_result" }

DEFINE DATASET dsAuthRule 
  FOR tt_auth_rule, 
      tt_auth_rule_error,
      tt_auth_rule_result.
    

/* ***************************  Main Block  *************************** */
