
/* maauthcrosswalkhisttt.i MEDSTAR Medical Aid System
                       Healthcare temp table definition
                       (c) Copyright 2015 - 2020 
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_crosswalk_history
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action               AS CHARACTER     FORMAT "x(60)"
  FIELD line_number                 AS INTEGER       FORMAT "999"
  FIELD auth_crosswalk_history_obj  AS DECIMAL       FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_crosswalk_obj          AS DECIMAL       FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_obj                    AS DECIMAL       FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD crosswalk_obj               AS DECIMAL       FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD change_user_id              AS CHARACTER     FORMAT "x(8)"
  FIELD change_date_time            AS DATETIME      FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD sequence                    AS INTEGER       FORMAT ">>>>9"
  FIELD change_usr_id               AS CHARACTER     FORMAT "x(30)"
  FIELD action                      AS CHARACTER     FORMAT "x(6)"

  INDEX xPK_hat_auth_crosswalk_history AS PRIMARY UNIQUE
    auth_crosswalk_history_obj 
    
  INDEX xAK1_hat_auth_crosswalk_history  AS UNIQUE
    auth_obj 
    sequence
    change_date_time .
