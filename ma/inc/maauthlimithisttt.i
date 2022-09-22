/* maauthlimithisttt.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_limit_history
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD action                              AS CHARACTER       FORMAT "x(8)"
  FIELD auth_obj                            AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_limit_obj                      AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_limit_history_obj              AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD claim_code                          AS INTEGER         FORMAT "999"
  FIELD claim_type                          AS CHARACTER       FORMAT "x(1)"
  FIELD dependant                           AS INTEGER         FORMAT "99"
  FIELD effective_date                      AS DATE            FORMAT "9999/99/99"
  FIELD fetch_date_time                     AS DATETIME        FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD limit_#                             AS INTEGER         FORMAT ">>>>,>>9.99"
  FIELD limit_avail                         AS DECIMAL         FORMAT ">>>>,>>9.99"
  FIELD limit_used                          AS DECIMAL         FORMAT ">>>>,>>9.99"
  FIELD limit_reserved                      AS DECIMAL         FORMAT ">>>>,>>9.99"
  FIELD option_code                         AS INTEGER         FORMAT "999"
  FIELD owning_entity_mnemonic              AS CHARACTER       FORMAT "x(80)"
  FIELD owning_obj                          AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_alt_value                    AS CHARACTER       FORMAT "x(20)"
  FIELD rate_per                            AS CHARACTER       FORMAT "x(1)"
  FIELD change_date_time                    AS DATETIME        FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD change_usr_id                       AS CHARACTER       FORMAT "x(8)"
  FIELD sequence                            AS INTEGER         FORMAT ">>>>9"
  
  INDEX xPK_hah_auth_limit_history AS PRIMARY UNIQUE
    auth_limit_history_obj
    
  INDEX xAK1_hah_auth_limit_history 
    auth_obj  
    sequence
    change_date_time
    .


