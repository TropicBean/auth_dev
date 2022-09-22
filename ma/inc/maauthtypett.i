/* maauthtypett.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_type
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD activate_am_pm                      AS LOGICAL         FORMAT "yes/no"
  FIELD activate_code_link                  AS LOGICAL         FORMAT "yes/no" INITIAL YES
  FIELD activate_body_region                AS LOGICAL         FORMAT "yes/no"
  FIELD activate_copayment                  AS LOGICAL         FORMAT "yes/no"
  FIELD activate_crosswalk                  AS LOGICAL         FORMAT "yes/no" INITIAL YES
  FIELD activate_due_date                   AS LOGICAL         FORMAT "yes/no"
  FIELD activate_episode_number             AS LOGICAL         FORMAT "yes/no"
  FIELD activate_health                     AS CHARACTER       FORMAT "x(80)"
  FIELD activate_los                        AS LOGICAL         FORMAT "yes/no"
  FIELD activate_los_weekend_pass           AS LOGICAL         FORMAT "yes/no" INITIAL NO
  FIELD activate_mouth_part_id              AS LOGICAL         FORMAT "yes/no"
  FIELD activate_penalty                    AS LOGICAL         FORMAT "yes/no"
  FIELD activate_service_type               AS LOGICAL         FORMAT "yes/no"
  FIELD authorise_all_services              AS LOGICAL         FORMAT "yes/no"
  FIELD auth_type                           AS CHARACTER       FORMAT "x(10)"
  FIELD auth_type_group                     AS CHARACTER       FORMAT "x(20)"
  FIELD auth_type_obj                       AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_type_prefix                    AS CHARACTER       FORMAT "x(5)"
  FIELD claim_code_upd_role                 AS CHARACTER       FORMAT "x(80)"
  FIELD claim_code_upd_user                 AS CHARACTER       FORMAT "x(80)"
  FIELD claim_type_upd_role                 AS CHARACTER       FORMAT "x(80)"
  FIELD claim_type_upd_user                 AS CHARACTER       FORMAT "x(80)"
  FIELD default_auth_status                 AS CHARACTER       FORMAT "x(1)"
  FIELD default_auth_status_note            AS CHARACTER       FORMAT "x(3)"
  FIELD default_auth_status_upd_role        AS CHARACTER       FORMAT "x(80)"
  FIELD default_auth_status_upd_user        AS CHARACTER       FORMAT "x(80)"
  FIELD default_line_restriction            AS CHARACTER       FORMAT "x(20)"
  FIELD description                         AS CHARACTER       FORMAT "x(80)"
  FIELD effective_date                      AS DATE            FORMAT "9999/99/99"
  FIELD end_date                            AS DATE            FORMAT "9999/99/99"
  FIELD end_date_upd_user                   AS CHARACTER       FORMAT "9999/99/99"
  FIELD end_date_upd_role                   AS CHARACTER       FORMAT "9999/99/99"
  FIELD external_auth_type_group            AS CHARACTER       FORMAT "x(20)"
  FIELD header_values_allowed               AS CHARACTER       FORMAT "x(50)"
  FIELD header_values_unlimited             AS LOGICAL         FORMAT "yes/no"
  FIELD icd_cond_code                       AS CHARACTER       FORMAT "x(8)"
  FIELD icd_cond_type                       AS CHARACTER       FORMAT "x(8)"
  FIELD insurer_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD multiple_cc_message_type            AS CHARACTER       FORMAT "x(10)"
  FIELD sequence_key                        AS CHARACTER       FORMAT "x(20)"
  FIELD updates_allowed                     AS LOGICAL         FORMAT "yes/no"
  FIELD valid_icds                          AS CHARACTER       FORMAT "x(10)"
  INDEX xAK1_hac_auth_type AS PRIMARY UNIQUE
    auth_type
    insurer_obj
    .

