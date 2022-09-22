/* maauthtypedetailtt.i MEDSTAR Medical Aid System
                        Healthcare temp table definition for Auth Type Detail
                        (c) Copyright 2015 - 2022
                        MIP Holdings (Pty) Ltd
                        All rights reserved
*/

/* ************************************  Definitions  *********************************** */

/* 
  Original Table replaced by hac_auth_type_control and code moved to maauthtypecontroltt.i
  Reuse program now for table hac_auth_type_detail
*/

/* ************************************  Definitions  *********************************** */
/* &ACCESS is used to make temp table NEW GLOBAL SHARED PROTECTED PRIVATE or STATIC       */
/* BEFORE is used to define a before table for the temp table                             */

/* Some other options that can be specified on fields :                                   */
/*   FORMAT "x(36)":U LABEL "MenuGuid":T INIT 0                                           */
/*   SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U       */

&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_type_detail
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD auth_auto_create                    AS LOGICAL         FORMAT "yes/no"                     LABEL "Auto Create"  
  FIELD auth_type_detail_obj                AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"   
  FIELD auth_type_obj                       AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"   
  FIELD auth_type_provider_obj              AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"   
  FIELD auth_usage_limit                    AS INTEGER         FORMAT ">>9"                        LABEL "Authorisation Usage Limit" 
  FIELD default_auth_status                 AS CHARACTER       FORMAT "x(1)"                       LABEL "Authorisation Default Status" 
  FIELD default_auth_status_note            AS CHARACTER       FORMAT "x(3)"                       LABEL "Default Status Reason" 
  FIELD default_line_restriction            AS CHARACTER       FORMAT "x(20)"                      LABEL "Default Line Restriction" 
  FIELD detail_type_indicator               AS CHARACTER       FORMAT "x(40)"                      LABEL "Detail Type Indicator" 
  FIELD effective_date                      AS DATE            FORMAT "9999/99/99"                 LABEL "Effective Date" 
  FIELD end_date                            AS DATE            FORMAT "9999/99/99"                 LABEL "End Date" 
  FIELD owning_alt_value                    AS CHARACTER       FORMAT "x(20)"                      LABEL "Item Code" 
  FIELD owning_entity_mnemonic              AS CHARACTER       FORMAT "x(8)"                       LABEL "Detail Entity"  
  FIELD owning_key                          AS CHARACTER       FORMAT "x(20)"                      
  FIELD owning_obj                          AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"  
  FIELD pr_type                             AS INTEGER         FORMAT ">>9"                        LABEL "Discipline"
  FIELD quantity_auth                       AS DECIMAL         FORMAT ">>>>9"                      LABEL "Quantity Auth"
  INDEX xPK1_hac_auth_type_detail AS PRIMARY UNIQUE
    auth_type_detail_obj
  
  INDEX xAK1_hac_auth_type_detail AS UNIQUE
    auth_type_obj            ASCENDING 
    auth_type_provider_obj   ASCENDING
    pr_type                  ASCENDING
    owning_entity_mnemonic   ASCENDING
    owning_obj               ASCENDING 
    owning_key               ASCENDING 
    effective_date           DESCENDING  

  INDEX xAK2_hac_auth_type_detail
    auth_type_obj            ASCENDING 
    auth_type_provider_obj   ASCENDING
    pr_type                  ASCENDING
    owning_entity_mnemonic   ASCENDING
    owning_obj               ASCENDING 
    owning_key               ASCENDING 
    end_date                DESCENDING
   .


