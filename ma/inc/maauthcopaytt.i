/* maauthcopaytt.i MEDSTAR Medical Aid System
                 Healthcare temp table definition
                 (c) Copyright 2015 - 2022
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_copay
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER             FORMAT "x(60)"
  FIELD line_number                         AS INTEGER               FORMAT "999"
  FIELD amount                              AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_%                            AS DECIMAL               FORMAT ">>9.99"
  FIELD auth_obj                            AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_copay_obj                      AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_copay_type_obj                 AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_entity_mnemonic              AS CHARACTER             FORMAT "x(80)"
  FIELD owning_obj                          AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_key                          AS CHARACTER             FORMAT "x(20)"
  FIELD owning_alt_value                    AS CHARACTER             FORMAT "x(20)"
  FIELD _auth_provider                      AS CHARACTER             FORMAT "x(40)"

  INDEX xPK_hat_auth_copay AS PRIMARY UNIQUE
    auth_copay_obj

  INDEX xAK1_hat_auth_copay
    auth_obj
    auth_copay_obj

  INDEX xAK2_hat_auth_copay
    auth_obj
    auth_copay_type_obj
    owning_entity_mnemonic
    owning_obj
    owning_key
    .



