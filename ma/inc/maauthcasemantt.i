/* maauthcasemantt.i MEDSTAR Medical Aid System
                     Healthcare Auth Case Management temp-table definition
                     (c) Copyright 2018
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_case_manager
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD case_manager_obj                    AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999" 
  FIELD insurer_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD option_code                         AS INTEGER         FORMAT "999"
  FIELD user_id                             AS CHARACTER       FORMAT "x(8)"
  FIELD user_profile                        AS CHARACTER       FORMAT "x(40)"
  FIELD start_date                          AS DATE            FORMAT "9999/99/99"
  FIELD end_date                            AS DATE            FORMAT "9999/99/99"
  
  
  INDEX xPK_ham_case_manager AS PRIMARY UNIQUE
    case_manager_obj
  
  INDEX xAK1_ham_case_manager
    insurer_obj
    option_code
    user_id
    start_date

  INDEX xAK2_ham_case_manager
    insurer_obj
    option_code
    user_profile
    start_date.
