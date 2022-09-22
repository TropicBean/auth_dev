/* maauthflagvaluehisttt.i MEDSTAR Medical Aid System
                     Healthcare Healthcare Flag Value History temp-table definition
                     (c) Copyright 2016
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_flag_value_history
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD record_action                AS CHARACTER  FORMAT "x(60)"
  FIELD line_number                  AS INTEGER         
  FIELD auth_flag_value_history_obj  AS DECIMAL    FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_flag_value_obj          AS DECIMAL    FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_flag_value              AS CHARACTER  FORMAT "x(100)"
  FIELD auth_rule_obj                AS DECIMAL    FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD change_date_time             AS DATETIME   FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD change_usr_id                AS CHARACTER  FORMAT "x(8)"
  FIELD last_change_datetime         AS DATETIME   FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD owning_entity_mnemonic       AS CHARACTER  FORMAT "x(8)"
  FIELD owning_obj                   AS DECIMAL    FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_key                   AS CHARACTER  FORMAT "x(20)"
  FIELD owning_alt_value             AS CHARACTER  FORMAT "x(20)"
  FIELD sequence                     AS INTEGER    FORMAT ">>>>9"
  FIELD action                       AS CHARACTER       FORMAT "x(6)"
  
  INDEX xPK_hah_auth_flag_value_history AS PRIMARY UNIQUE
    auth_flag_value_history_obj
  
  INDEX xAK1_hah_auth_flag_value_history AS UNIQUE
    auth_flag_value_obj
    sequence
    change_date_time
  
  .


