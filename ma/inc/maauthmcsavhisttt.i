/* maauthmcsavhisttt.i MEDSTAR Medical Aid System
                 Healthcare temp table definition
                 (c) Copyright 2015 - 2017
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_mc_savings_history
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD amount_actual                       AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD amount_base                         AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD auth_obj                            AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_mc_savings_history_obj         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_mc_savings_obj                 AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD change_date_time                    AS DATETIME        FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD change_usr_id                       AS CHARACTER       FORMAT "x(8)"
  FIELD saving_date                         AS DATE            FORMAT "9999/99/99"
  FIELD saving_status                       AS CHARACTER       FORMAT "x(20)" 
  FIELD saving_status_note                  AS CHARACTER       FORMAT "x(3)"
  FIELD saving_type                         AS CHARACTER       FORMAT "x(8)"
  FIELD sequence                            AS INTEGER         FORMAT ">>>>9"
  FIELD user_id                             AS CHARACTER       FORMAT "x(8)"
  FIELD action                              AS CHARACTER       FORMAT "x(6)"
  
  INDEX xPK_hah_auth_mc_savings_histo AS PRIMARY UNIQUE
    auth_mc_savings_history_obj
  
  INDEX xAK1_hah_mc_auth_coding_histo AS UNIQUE
    auth_obj
    change_date_time  
    sequence
    .

