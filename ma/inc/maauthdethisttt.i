/* maauthdethisttt.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_detail_history
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD action                              AS CHARACTER       FORMAT "x(6)"
  FIELD add_to_total_los                    AS LOGICAL INITIAL TRUE  FORMAT "yes/no"
  FIELD added_by_user                       AS LOGICAL INITIAL TRUE  FORMAT "yes/no"
  FIELD adjustment_auth                     AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD adjustment_paid                     AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD adjustment_private_auth             AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD adjustment_private_paid             AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD amount_auth                         AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD amount_paid                         AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD amount_requested                    AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD auth_group_obj                      AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_detail_history_obj             AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_detail_obj                     AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_obj                            AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_provider_obj                   AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_status                         AS INTEGER         FORMAT "9"
  FIELD auth_status_note                    AS CHARACTER       FORMAT "x(3)"
  FIELD benefit_%                           AS DECIMAL         FORMAT "999.99"  
  FIELD body_region                         AS CHARACTER       FORMAT "x(10)"
  FIELD change_date_time                    AS DATETIME        FORMAT "99/99/9999 HH:MM:SS.SSS"
  FIELD change_usr_id                       AS CHARACTER       FORMAT "x(8)"
  FIELD claim_code                          AS INTEGER         FORMAT "999"
  FIELD claim_type                          AS CHARACTER       FORMAT "!(1)"
  FIELD copay_auth_%                        AS DECIMAL         FORMAT ">>9.99"
  FIELD copay_auth                          AS DECIMAL         FORMAT "->>>,>>9.99"
  FIELD copay_override_note                 AS CHARACTER       FORMAT "x(3)"
  FIELD copay_paid                          AS DECIMAL         FORMAT "->>>,>>9.99"
  FIELD cpt_link_obj                        AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD default_base_rate                   AS CHARACTER       FORMAT "x(8)"
  FIELD default_ars_rate                    AS CHARACTER       FORMAT "x(8)"
  FIELD default_claim_code                  AS INTEGER         FORMAT "999"
  FIELD default_claim_code_source           AS CHARACTER       FORMAT "x(40)"
  FIELD default_claim_type                  AS CHARACTER       FORMAT "x(1)"
  FIELD discount_auth                       AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD discount_paid                       AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD discount_type                       AS LOGICAL         FORMAT "P/R"
  FIELD end_ampm                            AS LOGICAL         FORMAT "AM/PM"
  FIELD end_date                            AS DATE            FORMAT "9999/99/99"
  FIELD fixed_item_cost                     AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD line_restriction                    AS CHARACTER       FORMAT "x(20)"
  FIELD los_calculation_rule                AS CHARACTER       FORMAT "x(40)"
  FIELD loc_sequence                        AS INTEGER         FORMAT ">>>9"
  FIELD loc_tariff_type_obj                 AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD loc_value                           AS CHARACTER       FORMAT "x(16)"
  FIELD minutes_auth                        AS INTEGER         FORMAT ">>>>9" 
  FIELD minutes_calculation_rule            AS CHARACTER       FORMAT "x(40)"
  FIELD minutes_requested                   AS INTEGER         FORMAT ">>>>9"
  FIELD minutes_tariff_type_obj             AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD mouth_part_id                       AS INTEGER         FORMAT "99" EXTENT 10
  FIELD note                                AS CHARACTER       FORMAT "x(3)"
  FIELD override_ars_rate                   AS CHARACTER       FORMAT "x(8)"  
  FIELD override_base_rate                  AS CHARACTER       FORMAT "x(8)"
  FIELD owning_alt_value                    AS CHARACTER       FORMAT "x(20)"
  FIELD owning_entity_mnemonic              AS CHARACTER       FORMAT "x(8)"
  FIELD owning_key                          AS CHARACTER       FORMAT "x(20)"
  FIELD owning_obj                          AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD payee_dm                            AS CHARACTER       FORMAT "!(1)"
  FIELD pmb_benefit_%                       AS DECIMAL         FORMAT "999.99"
  FIELD pmb_indicator                       AS LOGICAL         FORMAT "yes/no"
  FIELD pmb_pay_cost                        AS LOGICAL         FORMAT "yes/no"
  FIELD pmb_value                           AS DECIMAL         FORMAT "->>>,>>9.99"
  FIELD print_dm                            AS CHARACTER       FORMAT "x(8)"
  FIELD quantity_auth                       AS DECIMAL         FORMAT ">>>>9.99"
  FIELD quantity_claimed                    AS INTEGER         FORMAT ">>>>9"
  FIELD quantity_los                        AS DECIMAL         FORMAT "->,>>>,>>9.99"
  FIELD quantity_paid                       AS INTEGER         FORMAT ">>>>9"
  FIELD quantity_requested                  AS INTEGER         FORMAT ">>>>9"
  FIELD rate_change                         AS LOGICAL         FORMAT "yes/no"
  FIELD reason                              AS CHARACTER       FORMAT "x(800)"
  FIELD related_entity_mnemonic             AS CHARACTER       FORMAT "x(8)"
  FIELD related_key                         AS CHARACTER       FORMAT "x(20)"
  FIELD related_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD related_value                       AS CHARACTER       FORMAT "x(20)"
  FIELD repeat_item                         AS LOGICAL         FORMAT "yes/no"
  FIELD repeat_cycle_auth                   AS INTEGER         FORMAT ">>9"
  FIELD repeat_cycle_paid                   AS INTEGER         FORMAT ">>9"
  FIELD repeat_cycle_quantity               AS INTEGER         FORMAT ">>>>9"
  FIELD repeat_cycle_days                   AS INTEGER         FORMAT ">>9"
  FIELD repeat_cycle_grace_days             AS INTEGER         FORMAT ">>9"
  FIELD repeat_last_claimed_date            AS DATE            FORMAT "9999/99/99"
  FIELD sequence                            AS INTEGER         FORMAT ">>>>9"
  FIELD start_ampm                          AS LOGICAL         FORMAT "AM/PM"
  FIELD start_date                          AS DATE            FORMAT "9999/99/99"
  
  INDEX xPK_hah_auth_detail_history AS PRIMARY UNIQUE
    auth_detail_history_obj
    
  INDEX xAK1_hah_auth_detail_history 
    auth_obj  
    sequence
    change_date_time
    .
