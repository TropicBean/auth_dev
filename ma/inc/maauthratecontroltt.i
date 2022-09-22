/* maauthratecontroltt.i MEDSTAR Medical Aid System
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_rate_control
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER                FORMAT "x(60)"
  FIELD line_number                         AS INTEGER                  FORMAT "999"
  FIELD insurer_obj                         AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD option_code                         AS INTEGER                  FORMAT "999"
  FIELD rate_control_code                   AS CHARACTER                FORMAT "x(25)"
  FIELD rate_control_description            AS CHARACTER                FORMAT "x(70)"
  FIELD main_provider_neg_num               AS INTEGER                  FORMAT ">>>9"
  FIELD main_provider_base_rate             AS CHARACTER                FORMAT "x(8)"
  FIELD main_provider_ars_rate              AS CHARACTER                FORMAT "x(8)"
  FIELD effective_date                      AS DATE                     FORMAT "9999/99/99"
  FIELD end_date                            AS DATE                     FORMAT "9999/99/99"
  FIELD override_base_rate                  AS CHARACTER                FORMAT "x(8)"
  FIELD override_ars_rate                   AS CHARACTER                FORMAT "x(8)"
  FIELD primary_code                        AS LOGICAL                  FORMAT "yes/no"
  FIELD main_code                           AS LOGICAL                  FORMAT "yes/no"
  FIELD code_link_category                  AS CHARACTER                FORMAT "x(40)"
  FIELD auth_rate_control_obj               AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD rate_change_message                 AS CHARACTER                FORMAT "x(1000)"
  FIELD workgroup_obj                       AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD associated_pr_type_list             AS CHARACTER                FORMAT "x(80)"
  FIELD warning_message                     AS CHARACTER                FORMAT "x(1000)"
  FIELD revert_rate_change                  AS LOGICAL     INITIAL TRUE FORMAT "Automatic/Manual"
  FIELD body_region_exclusion               AS CHARACTER                FORMAT "x(10)"
  FIELD override_auth_type_obj              AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD cpt_relative_value_type             AS CHARACTER                FORMAT "x(30)"
  
  INDEX xPK_auth_rate_control  AS PRIMARY UNIQUE
    auth_rate_control_obj
  INDEX xAK1_auth_rate_control AS UNIQUE 
    insurer_obj
    option_code
    main_provider_neg_num
    main_provider_base_rate
    main_provider_ars_rate
    workgroup_obj
    associated_pr_type_list
    effective_date DESCENDING
  INDEX xAK2_auth_rate_control AS UNIQUE
    insurer_obj
    option_code
    main_provider_neg_num
    main_provider_base_rate
    main_provider_ars_rate
    workgroup_obj
    associated_pr_type_list
    end_date DESCENDING
  INDEX xAK3_auth_rate_control AS UNIQUE  
    rate_control_code. 
 


