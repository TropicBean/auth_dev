/* maauthrulett.i MEDSTAR Medical Aid System
                  Healthcare Auth rules temp-table definition
                  (c) Copyright 2017
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_rule
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD auth_rule_obj                       AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999" 
  FIELD insurer_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD effective_date                      AS DATE            FORMAT "9999/99/99"
  FIELD end_date                            AS DATE            FORMAT "9999/99/99"
  FIELD link_auth_rule_obj                  AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD option_code                         AS INTEGER         FORMAT "999"
  FIELD rule_code                           AS CHARACTER       FORMAT "x(10)"
  FIELD rule_description                    AS CHARACTER       FORMAT "x(50)"
  FIELD rule_type                           AS CHARACTER       FORMAT "x(8)"
  FIELD rule_valid_values                   AS CHARACTER       FORMAT "x(20)"
  FIELD rule_value                          AS CHARACTER       FORMAT "x(100)"
  FIELD system_owned                        AS LOGICAL         FORMAT "yes/no"
  
  INDEX xPK_hac_auth_rule AS PRIMARY UNIQUE
    auth_rule_obj
  
  INDEX xAK1_hac_auth_rule
    insurer_obj
    option_code
    rule_type
    rule_code
    link_auth_rule_obj
    effective_date.

