/* maauthtypeds.i MEDSTAR Medical Aid System
                  Healthcare auth type dataset definition
                  (c) Copyright 2015 - 2022
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

{ ma/inc/maauthtypett.i          &TEMP-TABLE-NAME = "tt_auth_type"          &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthtypecontroltt.i   &TEMP-TABLE-NAME = "tt_auth_type_control"  &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthtypeprovidertt.i  &TEMP-TABLE-NAME = "tt_auth_type_provider" &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maauthtypedetailtt.i    &TEMP-TABLE-NAME = "tt_auth_type_detail"   &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maerrortt.i             &TEMP-TABLE-NAME = "tt_auth_type_error"    &REFERENCE-ONLY = {&ReferenceOnly}}
{ ma/inc/maresulttt.i            &TEMP-TABLE-NAME = "tt_auth_type_result"   &REFERENCE-ONLY = {&ReferenceOnly}}


DEFINE DATASET dsAuthType {&ReferenceOnly}
  FOR tt_auth_type, 
      tt_auth_type_control,
      tt_auth_type_provider,
      tt_auth_type_detail,
      tt_auth_type_result,
      tt_auth_type_error
      
  DATA-RELATION AuthType_AuthTypeControl FOR tt_auth_type, tt_auth_type_control
      RELATION-FIELDS(tt_auth_type.auth_type_obj,tt_auth_type_control.auth_type_obj) NESTED
  
  DATA-RELATION AuthType_AuthTypeProvider FOR tt_auth_type, tt_auth_type_provider
      RELATION-FIELDS(tt_auth_type.auth_type_obj,tt_auth_type_provider.auth_type_obj) NESTED
  
  DATA-RELATION AuthType_AuthTypeDetail FOR tt_auth_type, tt_auth_type_detail
      RELATION-FIELDS(tt_auth_type.auth_type_obj,tt_auth_type_detail.auth_type_obj) NESTED.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */






