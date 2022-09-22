/* maauthtt.i MEDSTAR Medical Aid System
                 Healthcare temp table definition
                 (c) Copyright 2015 - 2021
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
                                                                     
  FIELD record_action                       AS CHARACTER             FORMAT "x(60)"
  FIELD line_number                         AS INTEGER               FORMAT "999"
  FIELD adjustment_auth                     AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD adjustment_paid                     AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD adjustment_private_auth             AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD adjustment_private_paid             AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_auth                         AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_claimed                      AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_interim                      AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_paid                         AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_requested                    AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_total                        AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD auth_date                           AS DATE                  FORMAT "9999/99/99"
  FIELD auth_episode_obj                    AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_incomplete                     AS LOGICAL               FORMAT "yes/no"
  FIELD auth_num                            AS CHARACTER             FORMAT "x(15)"
  FIELD auth_obj                            AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_status                         AS INTEGER               FORMAT "9"
  FIELD auth_status_note                    AS CHARACTER             FORMAT "x(3)"
  FIELD auth_type_obj                       AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD authorise_all_services              AS LOGICAL               FORMAT "yes/no"
  FIELD benefit_%                           AS DECIMAL               FORMAT "999.99"
  FIELD body_region                         AS CHARACTER             FORMAT "x(10)"
  FIELD claim_code                          AS INTEGER               FORMAT "999"
  FIELD claim_type                          AS CHARACTER             FORMAT "!(1)"
  FIELD default_claim_code                  AS INTEGER               FORMAT "999"
  FIELD default_claim_type                  AS CHARACTER             FORMAT "x(1)"
  FIELD dependant                           AS INTEGER               FORMAT "99"
  FIELD dependant_reference                 AS CHARACTER             FORMAT "x(40)"
  FIELD discount_auth                       AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD discount_paid                       AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD discount_type                       AS LOGICAL INITIAL ?     FORMAT "P/R"
  FIELD due_date                            AS DATE                  FORMAT "9999/99/99"
  FIELD end_ampm                            AS LOGICAL               FORMAT "AM/PM"
  FIELD end_date                            AS DATE                  FORMAT "9999/99/99"
  FIELD insurer_obj                         AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD mem_num                             AS CHARACTER             FORMAT "x(13)"
  FIELD mouth_part_id                       AS INTEGER               FORMAT "99" EXTENT 10
  FIELD next_of_kin                         AS CHARACTER             FORMAT "x(60)"
  FIELD option_code                         AS INTEGER               FORMAT "999"
  FIELD pmb_benefit_%                       AS DECIMAL               FORMAT "999.99"
  FIELD pmb_indicator                       AS LOGICAL               FORMAT "yes/no"
  FIELD pmb_pay_cost                        AS LOGICAL               FORMAT "yes/no"
  FIELD pmb_value                           AS DECIMAL               FORMAT "->>>,>>9.99"
  FIELD quantity_auth                       AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_claimed                    AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_paid                       AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_requested                  AS INTEGER               FORMAT ">>>>9"
  FIELD receive_date                        AS DATE                  FORMAT "9999/99/99"
  FIELD reference_auth_num                  AS CHARACTER             FORMAT "x(20)"
  FIELD request_by                          AS CHARACTER             FORMAT "x(60)"
  FIELD request_date                        AS DATE                  FORMAT "9999/99/99"
  FIELD request_source                      AS CHARACTER             FORMAT "x(20)"
  FIELD request_source_details              AS CHARACTER             FORMAT "x(40)"
  FIELD service_type                        AS CHARACTER             FORMAT "x(40)"
  FIELD start_ampm                          AS LOGICAL               FORMAT "AM/PM"
  FIELD start_date                          AS DATE                  FORMAT "9999/99/99"
  FIELD total_los                           AS DECIMAL               FORMAT "->>>9.99"
  FIELD user_id                             AS CHARACTER             FORMAT "x(8)"
   
// Additional fields added to eliminate the need for web render procedure
  FIELD dependant_name                      AS CHARACTER             FORMAT "x(20)"
  FIELD primary_cpt                         AS CHARACTER             FORMAT "x(20)"
  FIELD primary_diagnosis                   AS CHARACTER             FORMAT "x(20)"
  FIELD _auth_dependant_updatable           AS LOGICAL               FORMAT "yes/no"
  FIELD _auth_group_list                    AS CHARACTER             FORMAT "x(60)"
  FIELD _authnum_viewable                   AS LOGICAL               FORMAT "yes/no"
  FIELD _base_rate_list                     AS CHARACTER             FORMAT "x(60)"
  FIELD _data_load                          AS LOGICAL               FORMAT "yes/no"
  FIELD _final_save                         AS LOGICAL               FORMAT "yes/no"
  FIELD _line_restriction                   AS CHARACTER             FORMAT "x(60)"
  FIELD _note_narration                     AS CHARACTER             FORMAT "x(60)"
  FIELD _option_code                        AS CHARACTER             FORMAT "999"
  FIELD _provider_list                      AS CHARACTER             FORMAT "x(20)"
  FIELD _reason_mandatory                   AS LOGICAL               FORMAT "yes/no"
  FIELD _reason_type                        AS CHARACTER             FORMAT "x(60)"
  FIELD _refnum_updatable                   AS LOGICAL               FORMAT "yes/no"
// Additional fields added for tariff validations
  FIELD _dependant_age_days                 AS INTEGER               FORMAT "999"
  FIELD _dependant_age_years                AS INTEGER               FORMAT "999"
  FIELD _dependant_bmi                      AS DECIMAL               FORMAT ">>9.9"
  FIELD _dependant_gender                   AS CHARACTER             FORMAT "x(8)"
  FIELD _auth_status_updated                AS LOGICAL
  
  INDEX xPK_hat_auth AS PRIMARY UNIQUE
    auth_obj
    
  INDEX xAK_hat_auth 
    auth_num
    
  INDEX xIE1_hat_auth  
    auth_episode_obj
  
  INDEX xIED1_hat_auth
    mem_num
    dependant
    start_date
    end_date.
