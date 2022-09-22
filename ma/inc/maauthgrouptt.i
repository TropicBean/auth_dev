/* maauthgrouptt.i MEDSTAR Medical Aid System
                 Healthcare temp table definition
                 (c) Copyright 2015 - 2020
                 MIP Holdings (Pty) Ltd
                 All rights reserved
*/

/* ************************************  Definitions  *********************************** */
/* &ACCESS is used to make temp table NEW GLOBAL SHARED PROTECTED PRIVATE or STATIC       */
/* BEFORE is used to define a before table for the temp table                             */

/* Some other options that can be specified on fields :                                   */
/*   FORMAT "x(36)":U LABEL "MenuGuid":T INIT 0                                       */
/*   SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U */

&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_group
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD auth_group_code                     AS CHARACTER       FORMAT "x(25)"
  FIELD auth_group_obj                      AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD description                         AS CHARACTER       FORMAT "x(80)"
  FIELD effective_date                      AS DATE            FORMAT "9999/99/99"
  FIELD end_date                            AS DATE            FORMAT "99/99/9999"
  FIELD insurer_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD option_code                         AS INTEGER         FORMAT "999"
  INDEX XPK_ham_auth_group AS PRIMARY UNIQUE
    auth_group_obj
  INDEX xAK1_ham_auth_group 
    insurer_obj
    option_code
    auth_group_code
    effective_date
  INDEX xAK2_ham_auth_group 
    insurer_obj
    option_code
    auth_group_code
    end_date
    .

