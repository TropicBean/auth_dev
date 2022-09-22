/* maauthcopayds.i MEDSTAR Medical Aid System
                   Healthcare auth copay control dataset definition
                   (c) Copyright 2021
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

{ ma/inc/maauthcopaycontroltt.i  &TEMP-TABLE-NAME = "tt_auth_copay_control"   &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthcopaydetailtt.i   &TEMP-TABLE-NAME = "tt_auth_copay_detail"    &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maerrortt.i             &TEMP-TABLE-NAME = "tt_auth_copay_error"     &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maresulttt.i            &TEMP-TABLE-NAME = "tt_auth_copay_result"    &REFERENCE-ONLY = {&ReferenceOnly}}

DEFINE DATASET dsAuthCopayControl {&ReferenceOnly}
  FOR tt_auth_copay_control,
      tt_auth_copay_detail,
      tt_auth_copay_result,
      tt_auth_copay_error
      
  DATA-RELATION AuthCopayControl_AuthCopayDetail FOR tt_auth_copay_control, tt_auth_copay_detail
      RELATION-FIELDS(tt_auth_copay_control.auth_copay_control_obj,tt_auth_copay_detail.auth_copay_control_obj) NESTED.
      
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

