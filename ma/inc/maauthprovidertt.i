/* maauthprovidertt.i MEDSTAR Medical Aid System
                      Healthcare temp table definition
                      (c) Copyright 2015 - 2021
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_provider
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER             FORMAT "x(60)"
  FIELD line_number                         AS INTEGER               FORMAT "99999"
  FIELD account_reference                   AS CHARACTER             FORMAT "x(20)"
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
  FIELD auth_copay_control_obj              AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_group_obj                      AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_obj                            AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_provider_obj                   AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_status                         AS INTEGER               FORMAT "9"
  FIELD auth_status_note                    AS CHARACTER             FORMAT "x(3)"
  FIELD authorise_all_services              AS LOGICAL               FORMAT "yes/no"
  FIELD authorised_service                  AS LOGICAL               FORMAT "yes/no"       /* renamed from service_provider */
  FIELD benefit_%                           AS DECIMAL               FORMAT "999.99"
  FIELD claim_code                          AS INTEGER               FORMAT "999"
  FIELD claim_type                          AS CHARACTER             FORMAT "!(1)"
  FIELD copay_auth_%                        AS DECIMAL               FORMAT "->>>,>>9.99"
  FIELD copay_auth                          AS DECIMAL               FORMAT "->>>,>>9.99" 
  FIELD copay_override_note                 AS CHARACTER             FORMAT "x(3)"
  FIELD copay_paid                          AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD copay_provider                      AS LOGICAL INITIAL ?     FORMAT "yes/no"
  FIELD default_ars_rate                    AS CHARACTER             FORMAT "x(8)"
  FIELD default_base_rate                   AS CHARACTER             FORMAT "x(8)"
  FIELD default_claim_code                  AS INTEGER               FORMAT "999"
  FIELD default_claim_type                  AS CHARACTER             FORMAT "x(1)"
  FIELD discount_auth                       AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD discount_paid                       AS DECIMAL               FORMAT "->,>>>,>>9.99"
  FIELD discount_type                       AS LOGICAL INITIAL ?     FORMAT "P/R"
  FIELD doc_num                             AS INTEGER               FORMAT "9999999"
  FIELD end_ampm                            AS LOGICAL               FORMAT "AM/PM"
  FIELD end_date                            AS DATE                  FORMAT "9999/99/99"
  FIELD group_doc_num                       AS INTEGER               FORMAT "9999999"
  FIELD los_calculation                     AS LOGICAL INITIAL YES   FORMAT "system/user" 
  FIELD main_provider                       AS LOGICAL               FORMAT "yes/no"
  FIELD override_ars_rate                   AS CHARACTER             FORMAT "x(8)"
  FIELD override_base_rate                  AS CHARACTER             FORMAT "x(8)"
  FIELD payee_dm                            AS CHARACTER             FORMAT "!(1)"
  FIELD pmb_benefit_%                       AS DECIMAL               FORMAT "999.99"
  FIELD pmb_indicator                       AS LOGICAL               FORMAT "yes/no"
  FIELD pmb_pay_cost                        AS LOGICAL               FORMAT "yes/no"
  FIELD pmb_value                           AS DECIMAL               FORMAT "->>>,>>9.99"
  FIELD pr_type                             AS INTEGER               FORMAT ">>9"
  FIELD print_dm                            AS CHARACTER             FORMAT "x(8)"
  FIELD provider_sequence                   AS INTEGER               FORMAT ">>9"
  FIELD provider_type                       AS CHARACTER             FORMAT "x(20)"
  FIELD quantity_auth                       AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_claimed                    AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_paid                       AS INTEGER               FORMAT ">>>>9"
  FIELD quantity_requested                  AS INTEGER               FORMAT ">>>>9"
  FIELD rate_change_type                    AS CHARACTER             FORMAT "x(20)"
  FIELD start_ampm                          AS LOGICAL               FORMAT "AM/PM"
  FIELD start_date                          AS DATE                  FORMAT "9999/99/99"
  FIELD sub_pr_type                         AS INTEGER               FORMAT ">>9"
  
  FIELD _ars_rate                           AS CHARACTER             FORMAT "x(20)"
  FIELD _authorised_value_updateable        AS CHARACTER             FORMAT "x(20)"  
  FIELD _base_rate                          AS CHARACTER             FORMAT "x(20)"
  FIELD _claim_code_updateable              AS CHARACTER             FORMAT "x(20)"
  FIELD _claim_type_updateable              AS CHARACTER             FORMAT "x(20)"
  FIELD _default                            AS LOGICAL INITIAL NO    FORMAT "yes/no"
  FIELD _emergency_flag                     AS CHARACTER             FORMAT "x(100)"
  FIELD _emergency_flag_updated             AS LOGICAL INITIAL NO    FORMAT "yes/no"
  FIELD _group_provider                     AS LOGICAL INITIAL NO    FORMAT "yes/no"
  FIELD _neg_group                          AS CHARACTER             FORMAT "x(20)"
  FIELD _neg_group_tooltip                  AS CHARACTER             FORMAT "x(20)"
  FIELD _note_narration                     AS CHARACTER             FORMAT "x(20)"  
  FIELD _penalty_flag                       AS CHARACTER             FORMAT "x(100)"
  FIELD _penalty_flag_updated               AS LOGICAL INITIAL NO    FORMAT "yes/no"
  FIELD _penalty_override_note              AS CHARACTER             FORMAT "x(5)"
  FIELD _provider_configuration             AS CHARACTER             FORMAT "x(20)"
  FIELD _reason_type                        AS CHARACTER             FORMAT "x(20)"
  FIELD _request_date_updated               AS LOGICAL INITIAL NO    FORMAT "yes/no"
  FIELD _status_note_mandatory              AS LOGICAL INITIAL NO    FORMAT "yes/no"
  FIELD _status_note_upd_allow              AS CHARACTER             FORMAT "x(20)"
  FIELD _workgroup_obj                      AS CHARACTER             FORMAT "x(150)"
  
  INDEX xPK_hat_auth_provider AS PRIMARY UNIQUE
    auth_provider_obj 
    
  INDEX xAK1_hat_auth_service_provider 
    auth_obj
    group_doc_num
    doc_num
    pr_type
    sub_pr_type    
 
  INDEX xAK2_hat_auth_service_provider 
    auth_obj
    main_provider
    group_doc_num
    doc_num
    pr_type
    sub_pr_type
    .


