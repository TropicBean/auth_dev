/* maauthrateprovidertt.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_rate_provider
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action              AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                AS INTEGER         FORMAT "999"
  FIELD auth_rate_control_obj      AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_rate_provider_obj     AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_rate_indicator        AS CHARACTER       FORMAT "x(20)"
  FIELD auth_rate_provider_type    AS CHARACTER       FORMAT "x(20)"
  FIELD auth_rate_pr_type          AS INTEGER         FORMAT ">>>9"
  FIELD authorise_all_services     AS LOGICAL         FORMAT "YES/NO"
  FIELD auth_group_obj             AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD related_entity_mnemonic    AS CHARACTER       FORMAT "x(20)"
  FIELD related_key                AS CHARACTER       FORMAT "x(20)"
  FIELD related_obj                AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD related_alt_value          AS CHARACTER       FORMAT "x(20)"
  FIELD provider_amount            AS DECIMAL         
  FIELD auth_rate_sub_pr_type      AS INTEGER         FORMAT "999"
  FIELD override_auth_status       AS INTEGER         FORMAT ">>>9"
  FIELD override_auth_status_note  AS CHARACTER       FORMAT "x(3)"
  FIELD revert_auth_status         AS INTEGER         FORMAT ">>>9"
  FIELD revert_auth_status_note    AS CHARACTER       FORMAT "x(3)"
  FIELD effective_date             AS DATE            FORMAT "9999/99/99"
  FIELD end_date                   AS DATE            FORMAT "9999/99/99"
  

  INDEX xPK_auth_rate_provider  IS PRIMARY UNIQUE 
    auth_rate_provider_obj
  
  INDEX xAK1_auth_rate_provider IS UNIQUE      
    auth_rate_control_obj
    auth_rate_provider_obj
  
  INDEX xAK2_auth_rate_provider IS UNIQUE   
    auth_rate_control_obj
    auth_rate_indicator
    auth_rate_provider_type
    auth_rate_pr_type
    auth_rate_sub_pr_type 
    effective_date DESCENDING
  
  INDEX xAK3_auth_rate_provider IS UNIQUE
    auth_rate_control_obj
    auth_rate_indicator
    auth_rate_provider_type
    auth_rate_pr_type
    auth_rate_sub_pr_type 
    end_date DESCENDING.


