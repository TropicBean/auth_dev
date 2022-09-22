/* maauthcasemaninactivett.i MEDSTAR Medical Aid System
                             Healthcare temp table definition for ham_cae_manager_inactive table
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

&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_case_manager_inactive
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  
  FIELD case_manager_inactive_obj           AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD case_manager_obj                    AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD inactive_reason                     AS CHARACTER       FORMAT "x(40)"  
  FIELD inactive_start_date                 AS DATE            FORMAT "9999/99/99"
  FIELD inactive_end_date                   AS DATE            FORMAT "9999/99/99"

  INDEX xAK1_ham_case_manager_inactive AS UNIQUE
    case_manager_obj
    inactive_start_date DESCENDING
    
  INDEX xAK2_ham_case_manager_inactive AS UNIQUE
    case_manager_obj
    inactive_end_date DESCENDING
    
  INDEX xPK_ham_case_manager_inactive AS UNIQUE
    case_manager_inactive_obj.

