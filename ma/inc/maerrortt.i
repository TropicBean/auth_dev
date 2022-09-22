/* maerrortt.i MEDSTAR Medical Aid System
               Healthcare error definitions
               (c) Copyright 2015 - 2018
               MIP Holdings (Pty) Ltd
               All rights reserved
*/

/* ************************************  Definitions  *********************************** */
/* &ACCESS is used to make temp table NEW GLOBAL SHARED PROTECTED PRIVATE or STATIC       */
/* BEFORE is used to define a before table for the temp table                             */

&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_error
&ENDIF

&IF DEFINED(REFERENCE-ONLY) > 0 
&THEN 
  &SCOPED-DEFINE ReferenceOnly {&REFERENCE-ONLY} 
&ELSE 
  &SCOPED-DEFINE ReferenceOnly 
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO SERIALIZE-NAME "tt_error" {&ReferenceOnly} 
  FIELD line_number            AS INTEGER         FORMAT "999"
  FIELD owning_entity_mnemonic AS CHARACTER       FORMAT "x(20)"
  FIELD owning_obj             AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_key             AS CHARACTER       FORMAT "x(20)"
  FIELD error_type             AS CHARACTER       FORMAT "x(20)"
  FIELD error_number           AS INTEGER         FORMAT "9999" 
  FIELD error_group            AS CHARACTER       FORMAT "x(20)"
  FIELD error_message          AS CHARACTER       FORMAT "x(200)"
  FIELD error_help             AS CHARACTER       FORMAT "x(60)"
  FIELD error_field_name       AS CHARACTER       FORMAT "x(20)"
  FIELD error_element_name     AS CHARACTER       FORMAT "x(20)"
  FIELD stack_trace            AS CHARACTER       FORMAT "x(100)"
  FIELD acknowledge            AS LOGICAL         INITIAL FALSE
  FIELD notification_timeout   AS INTEGER         FORMAT "999999" 
  INDEX idx1 owning_entity_mnemonic owning_obj owning_key.

