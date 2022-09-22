/* maauthcodingtt.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_coding
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD ass_diag_obj                        AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_coding_obj                     AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_obj                            AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
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
  FIELD start_date                          AS DATE            FORMAT "9999/99/99"
  FIELD main_code                           AS LOGICAL         FORMAT "yes/no"
  FIELD procedure_date                      AS DATE            FORMAT "9999/99/99"
  FIELD body_region                         AS CHARACTER       FORMAT "x(30)"
  // Additional fields added to eliminate the need for web render procedure
  FIELD _asterisk_code                      AS CHARACTER       FORMAT "x(8)"
  FIELD _auth_owning_alt_value              AS CHARACTER       FORMAT "x(20)"
  FIELD _dagger_code                        AS CHARACTER       FORMAT "x(8)"
  FIELD _morphology_code                    AS CHARACTER       FORMAT "x(8)"
  FIELD _note_narration                     AS CHARACTER       FORMAT "x(60)"
  FIELD _owning_description                 AS CHARACTER       FORMAT "x(20)"
  FIELD _pmbindicator_enabled               AS CHARACTER       FORMAT "yes/no"
  FIELD _reason_type                        AS CHARACTER       FORMAT "x(20)"
  FIELD _check_asterisk                     AS LOGICAL         FORMAT "yes/no"
  FIELD _check_dagger                       AS LOGICAL         FORMAT "yes/no"
  FIELD _check_morphology                   AS LOGICAL         FORMAT "yes/no"
  
  INDEX xPK_hat_auth_coding AS PRIMARY UNIQUE
    auth_coding_obj
  
  INDEX xAK1_hat_auth_coding 
    auth_obj
    owning_entity_mnemonic
    owning_key
    owning_obj     
    .
