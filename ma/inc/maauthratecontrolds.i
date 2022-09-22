/* maauthratecontrolds.i MEDSTAR Medical Aid System
                         Healthcare auth rate control dataset definition
                         (c) Copyright 2020
                         MIP Holdings (Pty) Ltd
                         All rights reserved                
*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(REFERENCE-ONLY) > 0 
&THEN 
  &SCOPED-DEFINE ReferenceOnly {&REFERENCE-ONLY} 
&ELSE 
  &SCOPED-DEFINE ReferenceOnly 
&ENDIF

{ ma/inc/maauthratecontroltt.i   &TEMP-TABLE-NAME = "tt_auth_rate_control"   &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthratedetailtt.i    &TEMP-TABLE-NAME = "tt_auth_rate_detail"    &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthrateprovidertt.i  &TEMP-TABLE-NAME = "tt_auth_rate_provider"  &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maerrortt.i             &TEMP-TABLE-NAME = "tt_auth_rate_error"     &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maresulttt.i            &TEMP-TABLE-NAME = "tt_auth_rate_result"    &REFERENCE-ONLY = {&ReferenceOnly}}


DEFINE DATASET dsAuthRateControl {&ReferenceOnly}
  FOR tt_auth_rate_control,
      tt_auth_rate_detail,
      tt_auth_rate_provider,
      tt_auth_rate_result,
      tt_auth_rate_error
      
  DATA-RELATION AuthRateControl_AuthRateDetail FOR tt_auth_rate_control, tt_auth_rate_detail
      RELATION-FIELDS(tt_auth_rate_control.auth_rate_control_obj,tt_auth_rate_detail.auth_rate_control_obj) NESTED
      
  DATA-RELATION AuthRateControl_AuthRateProvider FOR tt_auth_rate_control, tt_auth_rate_provider    
      RELATION-FIELDS(tt_auth_rate_control.auth_rate_control_obj,tt_auth_rate_provider.auth_rate_control_obj) NESTED.
  
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */







