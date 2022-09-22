/* maauthratedetailtt.i MEDSTAR Medical Aid System
                        Healthcare temp table definition
                        (c) Copyright 2020
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_rate_detail
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action            AS CHARACTER                 FORMAT "x(60)"
  FIELD line_number              AS INTEGER                   FORMAT "999"
  FIELD auth_rate_control_obj    AS DECIMAL                   FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_rate_detail_obj     AS DECIMAL                   FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD override_ars_rate        AS CHARACTER                 FORMAT "x(8)"
  FIELD owning_entity_mnemonic   AS CHARACTER                 FORMAT "x(8)"
  FIELD owning_obj               AS DECIMAL                   FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_key               AS CHARACTER                 FORMAT "x(20)"
  FIELD owning_alt_value         AS CHARACTER                 FORMAT "x(20)"
  FIELD flag_value               AS CHARACTER                 FORMAT "x(100)"
  FIELD effective_date           AS DATE                      FORMAT "9999/99/99"
  FIELD end_date                 AS DATE                      FORMAT "9999/99/99"
  FIELD apply_to_all             AS LOGICAL                   FORMAT "Y/N"
  FIELD exclusion                AS LOGICAL   INITIAL FALSE   FORMAT "yes/no"

  INDEX xPK_auth_rate_detail  IS PRIMARY UNIQUE 
    auth_rate_detail_obj
  INDEX xAK1_auth_rate_detail IS UNIQUE      
    auth_rate_control_obj
    auth_rate_detail_obj
  INDEX xAK2_auth_rate_detail IS UNIQUE   
    auth_rate_control_obj
    owning_entity_mnemonic
    owning_obj
    owning_key
    effective_date DESCENDING
  INDEX xAK3_auth_rate_detail IS UNIQUE
    auth_rate_control_obj
    owning_entity_mnemonic
    owning_obj
    owning_key
    end_date DESCENDING.

