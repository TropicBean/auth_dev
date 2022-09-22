/* maauthflagvaluett.i MEDSTAR Medical Aid System
                     Healthcare Healthcare Flag Value temp-table definition
                     (c) Copyright 2016-2021
                     MIP Holdings (Pty) Ltd
                     All rights reserved                
*/
  
/* ************************************  Definitions  *********************************** */
/* &ACCESS is used to make temp table NEW GLOBAL SHARED PROTECTED PRIVATE or STATIC       */
/* BEFORE is used to define a before table for the temp table                             */
  
/* Some other options that can be specified on fields :                                   */
/*   FORMAT "x(36)":U LABEL "MenuGuid":T INIT 0                                           */
/*   SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U       */
  
/* Do not define the Temp table or fields using LIKE. This would make the front end       */
/* dependant on database connection and would also mean that we have to Delta protect     */
/* front end code as soon as we roll out database changes.                                */
  
&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_flag_value
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         
  FIELD auth_flag_value_obj                 AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_flag_value                     AS CHARACTER       FORMAT "x(100)"
  FIELD auth_rule_obj                       AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD last_change_datetime                AS DATETIME        FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD override_note                       AS CHARACTER       FORMAT "x(5)"
  FIELD owning_alt_value                    AS CHARACTER       FORMAT "x(20)"
  FIELD owning_entity_mnemonic              AS CHARACTER       FORMAT "x(8)"
  FIELD owning_key                          AS CHARACTER       FORMAT "x(40)"
  FIELD owning_obj                          AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  
  INDEX xPK_hat_auth_flag_value AS PRIMARY UNIQUE
    auth_flag_value_obj
  
  INDEX xAK1_hat_auth_flag_value AS UNIQUE
    owning_entity_mnemonic
    owning_obj
    owning_key
    owning_alt_value
    auth_rule_obj
  
  INDEX xIE1_hat_auth_flag_value
    owning_entity_mnemonic
    last_change_datetime
    
  .

