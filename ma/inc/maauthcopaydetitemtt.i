/* maauthcopaydetitemtt.i MEDSTAR Medical Aid System
                          Healthcare temp table definition
                          (c) Copyright 2022
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_copay_detail_item
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER                FORMAT "x(60)"
  FIELD line_number                         AS INTEGER                  FORMAT "999"
  FIELD insurer_obj                         AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD option_code                         AS INTEGER                  FORMAT "999"
  FIELD auth_copay_detail_item_obj          AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD auth_copay_type_obj                 AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD effective_date                      AS DATE                     FORMAT "9999/99/99"
  FIELD end_date                            AS DATE                     FORMAT "9999/99/99"
  FIELD owning_entity_mnemonic              AS CHARACTER                FORMAT "x(8)"
  FIELD owning_obj                          AS DECIMAL                  FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD owning_key                          AS CHARACTER                FORMAT "x(20)"
  FIELD owning_alt_value                    AS CHARACTER                FORMAT "x(20)"
  FIELD pr_type                             AS INTEGER                  FORMAT "999"
  FIELD copayment_value_type                AS LOGICAL                  FORMAT "P/R"
  FIELD copayment_value                     AS DECIMAL                  FORMAT ">>,>>>,>>9.99"
  FIELD apply_to_pmb                        AS LOGICAL                  FORMAT "yes/no"
  FIELD apply_to_emergency                  AS LOGICAL                  FORMAT "yes/no"
  FIELD warning_message                     AS CHARACTER                FORMAT "x(1000)"
  FIELD warning_message_type                AS CHARACTER                FORMAT "x(10)"
  
  INDEX xPK_auth_copay_detail_item  AS PRIMARY UNIQUE
    auth_copay_detail_item_obj
  INDEX xAK1_auth_copay_detail_item AS UNIQUE 
    insurer_obj
    option_code
    owning_entity_mnemonic
    owning_obj
    owning_key
    pr_type
    effective_date DESCENDING
  INDEX xAK2_auth_copay_detail_item AS UNIQUE
    insurer_obj
    option_code
    owning_entity_mnemonic
    owning_obj
    owning_key
    pr_type
    end_date DESCENDING
  INDEX xIE1_auth_copay_detail_item 
    insurer_obj
    option_code
    owning_entity_mnemonic
    owning_alt_value
    pr_type
    effective_date
    end_date. 
 



