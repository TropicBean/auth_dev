/* maauthtypecontroltt.i MEDSTAR Medical Aid System
                        Healthcare: Temp table definition for Auth Type Control
                        (hac_auth_type_control)
                        (c) Copyright 2017 - 2022
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_type_control
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD activate_authorised_values          AS LOGICAL         FORMAT "Y/N"                          LABEL "Activate Authorised Values"
  FIELD age_range_both_obj                  AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"    LABEL "Age Range Both"
  FIELD age_range_female_obj                AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"    LABEL "Age Range Female"
  FIELD age_range_male_obj                  AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"    LABEL "Age Range Male"
  FIELD amount_auth                         AS DECIMAL         FORMAT "->>>,>>9.99"                  LABEL "Amount Authorised"
  FIELD auth_type_control_obj               AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"    LABEL "Auth Type Control Obj"
  FIELD auth_type_obj                       AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"    LABEL "Auth Type Obj"
  FIELD auth_type_restrictions              AS CHARACTER       FORMAT "x(80)"                        LABEL "Authorisation Type Restrictions"
  FIELD claim_codes_header                  AS CHARACTER       FORMAT "x(80)"                        LABEL "Claim Codes Header"
  FIELD claim_codes_disallow                AS CHARACTER       FORMAT "x(80)"                        LABEL "Disallow Claim Codes"
  FIELD claim_types_header                  AS CHARACTER       FORMAT "x(80)"                        LABEL "Claim Types"
  FIELD claim_types_disallow                AS CHARACTER       FORMAT "x(10)"                        LABEL "Disallow Claim Types"
  FIELD control_type_indicator              AS CHARACTER       FORMAT "x(40)"                        LABEL "Control Indicator Type"
  FIELD default_auth_status                 AS CHARACTER       FORMAT "x(1)"                         LABEL "Default Auth Status"
  FIELD default_auth_status_note            AS CHARACTER       FORMAT "x(3)"                         LABEL "Status Reason"
  FIELD effective_date                      AS DATE            FORMAT "9999/99/99"                   LABEL "Effective Date"
  FIELD enforce_authorised_values           AS LOGICAL         FORMAT "Y/N"                          LABEL "Enforce Authorised Values"
  FIELD end_date                            AS DATE            FORMAT "9999/99/99"                   LABEL "End Date"
  FIELD gender                              AS CHARACTER       FORMAT "!(1)"            INITIAL "B"  LABEL "Gender"
  FIELD insurer_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"    LABEL "Client"
  FIELD option_code                         AS INTEGER         FORMAT "999"                          LABEL "Option"
  FIELD period                              AS INTEGER         FORMAT ">>>>9"                        LABEL "Period"
  FIELD period_override                     AS LOGICAL         FORMAT "Y/N"                          LABEL "Period Override"
  FIELD period_type                         AS CHARACTER       FORMAT "x(8)"                         LABEL "Period Type"
  FIELD quantity_auth                       AS INTEGER         FORMAT ">>>>9"                        LABEL "Quantity Authorised"
  FIELD usage_override_user                 AS CHARACTER       FORMAT "x(80)"                        LABEL "Usage Override"
  FIELD usage_period                        AS INTEGER         FORMAT ">>>>9"                        LABEL "Usage Period"
  FIELD usage_period_type                   AS CHARACTER       FORMAT "x(8)"                         LABEL "Usage Period Type"
  FIELD usage_quantity                      AS INTEGER         FORMAT ">>>>9"                        LABEL "Usage Quantity"
  FIELD usage_type                          AS CHARACTER       FORMAT "x(20)"                        LABEL "Usage Type"
  
  INDEX xPK1_hac_auth_type_control AS PRIMARY UNIQUE
    auth_type_control_obj
  
  INDEX xAK1_hac_auth_type_control AS UNIQUE
    auth_type_obj
    insurer_obj
    option_code
    control_type_indicator
    effective_date DESCENDING
  
  INDEX xAK2_hac_auth_type_control AS UNIQUE
    auth_type_obj
    insurer_obj
    option_code
    control_type_indicator
    end_date DESCENDING
    .
