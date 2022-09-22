/* maauthdetailtt.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_detail
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER             FORMAT "x(60)"
  FIELD line_number                         AS INTEGER               FORMAT "999"
  FIELD add_to_total_los                    AS LOGICAL INITIAL TRUE  FORMAT "yes/no"
  FIELD added_by_user                       AS LOGICAL INITIAL TRUE  FORMAT "yes/no"
  FIELD adjustment_auth                     AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD adjustment_paid                     AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD adjustment_private_auth             AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD adjustment_private_paid             AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_auth                         AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_paid                         AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD amount_requested                    AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD auth_group_obj                      AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_detail_obj                     AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_obj                            AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_provider_obj                   AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_status                         AS INTEGER               FORMAT "9"
  FIELD auth_status_note                    AS CHARACTER             FORMAT "x(3)"
  FIELD benefit_%                           AS DECIMAL               FORMAT "999.99"
  FIELD body_region                         AS CHARACTER             FORMAT "x(10)"
  FIELD claim_code                          AS INTEGER               FORMAT "999"
  FIELD claim_type                          AS CHARACTER             FORMAT "!(1)"
  FIELD copay_auth                          AS DECIMAL               FORMAT "->>>,>>9.99"
  FIELD copay_paid                          AS DECIMAL               FORMAT "->>>,>>9.99"
  FIELD cpt_link_obj                        AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD default_base_rate                   AS CHARACTER             FORMAT "x(8)"
  FIELD default_ars_rate                    AS CHARACTER             FORMAT "x(8)"
  FIELD default_claim_code                  AS INTEGER               FORMAT "999"
  FIELD default_claim_type                  AS CHARACTER             FORMAT "x(1)"
  FIELD discount_auth                       AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD discount_paid                       AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD discount_type                       AS LOGICAL               FORMAT "P/R" 
  FIELD end_ampm                            AS LOGICAL               FORMAT "AM/PM"
  FIELD end_date                            AS DATE                  FORMAT "9999/99/99"
  FIELD end_time                            AS INTEGER               FORMAT ">>>>9"
  FIELD item_cost                           AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD line_restriction                    AS CHARACTER             FORMAT "x(20)"
  FIELD loc_sequence                        AS INTEGER               FORMAT ">>>9"
  FIELD loc_value                           AS CHARACTER             FORMAT "x(16)"
  FIELD los_calculation_rule                AS CHARACTER             FORMAT "x(40)"
  FIELD mouth_part_id                       AS INTEGER               FORMAT "99" EXTENT 10
  FIELD note                                AS CHARACTER             FORMAT "x(3)"
  FIELD override_ars_rate                   AS CHARACTER             FORMAT "x(8)"  
  FIELD override_base_rate                  AS CHARACTER             FORMAT "x(8)"
  FIELD owning_alt_value                    AS CHARACTER             FORMAT "x(20)"
  FIELD owning_entity_mnemonic              AS CHARACTER             FORMAT "x(8)"
  FIELD owning_key                          AS CHARACTER             FORMAT "x(20)"
  FIELD owning_obj                          AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD pmb_benefit_%                       AS DECIMAL               FORMAT "999.99"
  FIELD pmb_indicator                       AS LOGICAL               FORMAT "yes/no"
  FIELD pmb_pay_cost                        AS LOGICAL               FORMAT "yes/no"
  FIELD pmb_value                           AS DECIMAL               FORMAT "->>>,>>9.99"
  FIELD print_dm                            AS CHARACTER             FORMAT "x(8)"
  FIELD quantity_auth                       AS DECIMAL               FORMAT ">>>>9.99"
  FIELD quantity_claimed                    AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_los                        AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD quantity_paid                       AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_requested                  AS INTEGER               FORMAT ">>>>9"
  FIELD rate_change                         AS LOGICAL               FORMAT "yes/no"
  FIELD related_entity_mnemonic             AS CHARACTER             FORMAT "x(8)"
  FIELD related_key                         AS CHARACTER             FORMAT "x(20)"
  FIELD related_obj                         AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD related_value                       AS CHARACTER             FORMAT "x(20)"
  FIELD repeats                             AS LOGICAL               FORMAT "yes/no"
  FIELD start_ampm                          AS LOGICAL               FORMAT "AM/PM"
  FIELD start_date                          AS DATE                  FORMAT "9999/99/99"
  FIELD start_time                          AS INTEGER               FORMAT ">>>>9"
  FIELD tariff_type_obj                     AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD reason                              AS CHARACTER             FORMAT "x(800)"
  FIELD fixed_item_cost                     AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD default_claim_code_source           AS CHARACTER             FORMAT "x(40)"
  
  FIELD _activate_authorised_values         AS LOGICAL INITIAL FALSE FORMAT "yes/no"
  FIELD _ars_rate                           AS CHARACTER             FORMAT "x(20)"
  FIELD _ars_rate_upd_allowed               AS LOGICAL               FORMAT "yes/no"
  FIELD _auth_status_narration              AS CHARACTER             FORMAT "x(60)"
  FIELD _base_rate                          AS CHARACTER             FORMAT "x(20)"
  FIELD _base_rate_upd_allowed              AS LOGICAL               FORMAT "yes/no"
  FIELD _claim_code_updated                 AS LOGICAL               FORMAT "yes/no"
  FIELD _claim_type_updated                 AS LOGICAL               FORMAT "yes/no"
  FIELD _cpt_code                           AS CHARACTER             FORMAT "x(15)"
  FIELD _crosswalk_parameter_obj            AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD _detail_configuration               AS CHARACTER             FORMAT "x(20)"   
  FIELD _detail_owning_description          AS CHARACTER             FORMAT "x(60)"
  FIELD _detail_related_description         AS CHARACTER             FORMAT "x(60)"       
  FIELD _end_date_ampm_updated              AS LOGICAL               FORMAT "yes/no"
  FIELD _group_provider                     AS LOGICAL               FORMAT "yes/no"
  FIELD _loc_line                           AS LOGICAL               FORMAT "yes/no"
  FIELD _mandatory                          AS LOGICAL               FORMAT "yes/no"
  FIELD _nappi_obj                          AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD _negotiation_group                  AS CHARACTER             FORMAT "x(20)"
  FIELD _note_narration                     AS CHARACTER             FORMAT "x(60)"
  FIELD _provider_list                      AS CHARACTER             FORMAT "x(20)"    
  FIELD _reason_type                        AS CHARACTER             FORMAT "x(20)"
  FIELD _row_id                             AS CHARACTER             FORMAT "x(20)"
  FIELD _start_date_ampm_updated            AS LOGICAL               FORMAT "yes/no"
  FIELD _tariff_effec_date                  AS DATE                  FORMAT "9999/99/99"
  FIELD _tariff_type_acronym_key            AS CHARACTER             FORMAT "x(20)"
  FIELD _tariff_type_category               AS CHARACTER             FORMAT "x(20)"
  FIELD _nappi_required                     AS LOGICAL INITIAL FALSE FORMAT "yes/no"

  INDEX xPk_hat_auth_detail AS PRIMARY UNIQUE
    auth_detail_obj
    
  INDEX xAK1_hat_auth_detail 
    auth_provider_obj
    loc_sequence
    
  INDEX xAK2_hat_auth_detail  
    auth_provider_obj
    owning_entity_mnemonic
    owning_obj
    owning_key
    related_entity_mnemonic
    related_obj
    related_key
    start_date
    start_ampm
    start_time
    
  INDEX xAK3_hat_auth_detail  
    auth_provider_obj
    owning_entity_mnemonic
    owning_obj
    owning_key
    related_entity_mnemonic
    related_obj
    related_key
    end_date
    end_ampm
    end_time  
    
  INDEX xIE1_hat_auth_detail 
    auth_provider_obj
    owning_alt_value
    start_date
    end_date
    .

                           
