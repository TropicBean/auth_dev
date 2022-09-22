/* maauthcodinghisttt.i MEDSTAR Medical Aid System
                 Healthcare temp table definition
                 (c) Copyright 2015 - 2018
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_coding_history
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD ass_diag_obj                        AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_coding_history_obj             AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_coding_obj                     AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_obj                            AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD change_date_time                    AS DATETIME        FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD change_usr_id                       AS CHARACTER       FORMAT "x(8)"
  FIELD coding_status                       AS INTEGER         FORMAT "9"
  FIELD coding_status_note                  AS CHARACTER       FORMAT "x(3)"
  FIELD coding_type                         AS CHARACTER       FORMAT "x(20)"
  FIELD end_date                            AS DATE            FORMAT "9999/99/99"
  FIELD morph_diag_obj                      AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_alt_value                    AS CHARACTER       FORMAT "x(20)"
  FIELD owning_entity_mnemonic              AS CHARACTER       FORMAT "x(8)"
  FIELD owning_key                          AS CHARACTER       FORMAT "x(20)"
  FIELD owning_obj                          AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD pmb_indicator                       AS LOGICAL         FORMAT "yes/no"
  FIELD primary_code                        AS LOGICAL         FORMAT "yes/no"
  FIELD related_alt_value                   AS CHARACTER       FORMAT "x(20)"
  FIELD related_entity_mnemonic             AS CHARACTER       FORMAT "x(8)"
  FIELD related_key                         AS CHARACTER       FORMAT "x(20)"
  FIELD related_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD sequence                            AS INTEGER         FORMAT ">>>>9"
  FIELD start_date                          AS DATE            FORMAT "9999/99/99"
  FIELD action                              AS CHARACTER       FORMAT "x(6)"
  FIELD body_region                         AS CHARACTER       FORMAT "x(30)"

  INDEX xPK_hah_auth_coding_history AS PRIMARY UNIQUE
    auth_coding_history_obj
    .
