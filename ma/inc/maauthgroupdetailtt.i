/* maauthgroupdetailtt.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_group_detail
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD ars_rate                            AS CHARACTER       FORMAT "x(20)"
  FIELD base_rate                           AS CHARACTER       FORMAT "x(20)"
  FIELD sub_pr_type                         AS INTEGER         FORMAT "999"
  FIELD owning_alt_value                    AS CHARACTER       FORMAT "x(20)"
  FIELD amount                              AS DECIMAL         FORMAT "->>>,>>9.99"
  FIELD auth_group_detail_obj               AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_group_obj                      AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD claim_code                          AS INTEGER         FORMAT "999"
  FIELD claim_type                          AS CHARACTER       FORMAT "!(1)"
  FIELD effective_date                      AS DATE            FORMAT "9999/99/99"
  FIELD end_date                            AS DATE            FORMAT "99/99/9999"
  FIELD owning_entity_mnemonic              AS CHARACTER       FORMAT "x(10)"
  FIELD owning_key                          AS CHARACTER       FORMAT "x(20)"
  FIELD owning_obj                          AS DECIMAL         FORMAT "->>>>>>>>>>>>>>>>>9.999999999"
  FIELD pr_type                             AS CHARACTER       FORMAT "x(3)"
  FIELD quantity                            AS INTEGER         FORMAT ">>>>9"
  INDEX XPK_ham_auth_group_detail AS PRIMARY UNIQUE
    auth_group_detail_obj
  INDEX xAK1_ham_auth_group_detail 
    auth_group_obj
    owning_entity_mnemonic
    owning_obj
    owning_key
    pr_type
    effective_date
  INDEX xAK2_ham_auth_group_detail 
    auth_group_obj
    owning_entity_mnemonic
    owning_obj
    owning_key
    pr_type
    end_date
    .

