/* maauthtypeprovidertt.i MEDSTAR Medical Aid System
                          Healthcare temp table definition
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_type_provider
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
   FIELD line_number                         AS INTEGER         FORMAT "999"
   FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
   FIELD amount_auth                         AS DECIMAL         FORMAT "->>>,>>9.99"                LABEL "Authorised Amount"
   FIELD ars_rate_upd_role                   AS CHARACTER       FORMAT "x(80)"                      LABEL "Ars Rate Update Allowed (Roles)"
   FIELD ars_rate_upd_user                   AS CHARACTER       FORMAT "x(80)"                      LABEL "Ars Rate Update Allowed (Users)"
   FIELD auth_type_obj                       AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"  LABEL "Auth Type Obj"
   FIELD auth_type_provider_obj              AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"  LABEL "Auth Provider Obj"
   FIELD authorise_detail_lines              AS CHARACTER       FORMAT "x(30)"                      LABEL "Authorise Detail Lines"
   FIELD authorised_service                  AS LOGICAL         FORMAT "yes/no"                     LABEL "Authorised Service"
   FIELD base_rate_upd_role                  AS CHARACTER       FORMAT "x(80)"                      LABEL "Base Rate Update Allowed (Roles)"
   FIELD base_rate_upd_user                  AS CHARACTER       FORMAT "x(80)"                      LABEL "Base Rate Update Allowed (Users)"
   FIELD claim_codes_disallow                AS CHARACTER       FORMAT "x(80)"                      LABEL "Claim Codes Disallow"
   FIELD claim_codes_provider                AS CHARACTER       FORMAT "x(80)"                      LABEL "Claim Codes"
   FIELD claim_types_disallow                AS CHARACTER       FORMAT "x(10)"                      LABEL "Claim Types Disallow"
   FIELD claim_types_provider                AS CHARACTER       FORMAT "!(10)"                      LABEL "Claim Types"
   FIELD default_auth_status                 AS CHARACTER       FORMAT "x(1)"                       LABEL "Default Auth Status"
   FIELD default_auth_status_note            AS CHARACTER       FORMAT "x(3)"                       LABEL "Default Status Reason"
   FIELD default_auth_status_upd_role        AS CHARACTER       FORMAT "x(80)"                      LABEL "Status Update (Roles)"
   FIELD default_auth_status_upd_user        AS CHARACTER       FORMAT "x(80)"                      LABEL "Status Update (Users)"
   FIELD default_claim_code_detail           AS INTEGER         FORMAT "999"                        LABEL "Default Claim Code Detail"
   FIELD default_claim_type_detail           AS CHARACTER       FORMAT "x(1)"                       LABEL "Defaul Claim Type Detail"
   FIELD doc_num_mandatory                   AS LOGICAL         FORMAT "yes/no"                     LABEL "Provider Number Mandatory"
   FIELD enforce_header_claim_code_match     AS LOGICAL         FORMAT "YES/NO"                     LABEL "Enforce Header Claim Code Match"
   FIELD enforce_header_claim_type_match     AS LOGICAL         FORMAT "YES/NO"                     LABEL "Enforce Header Claim Type Match"
   FIELD effective_date                      AS DATE            FORMAT "9999/99/99"                 LABEL "Effective Date"
   FIELD end_date                            AS DATE            FORMAT "9999/99/99"                 LABEL "End Date"
   FIELD header_values_allowed               AS CHARACTER       FORMAT "x(50)"                      LABEL "Header Values Allowed"
   FIELD header_values_unlimited             AS LOGICAL         FORMAT "yes/no"                     LABEL "Header Value Unlimited"
   FIELD insurer_obj                         AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"  LABEL "Client"
   FIELD main_provider                       AS LOGICAL         FORMAT "yes/no"                     LABEL "Main Provider"
   FIELD mandatory                           AS LOGICAL         FORMAT "yes/no"                     LABEL "Provider Type Mandatory"
   FIELD neg_num                             AS INTEGER         FORMAT ">>>9"                       LABEL "Negotiation Number"
   FIELD number_providers_allowed            AS INTEGER         FORMAT "999"                        LABEL "Number Providers Allowed"
   FIELD option_code                         AS INTEGER         FORMAT ">9"                         LABEL "Option Code"
   FIELD pr_type_list                        AS CHARACTER       FORMAT "x(30)"                      LABEL "Practice Types"
   FIELD pr_type_valid_list                  AS CHARACTER       FORMAT "x(30)"                      LABEL "Valid Practice Types"
   FIELD provider_sequence                   AS INTEGER         FORMAT ">>9"                        LABEL "Provider Sequence"
   FIELD provider_type                       AS CHARACTER       FORMAT "x(20)"                      LABEL "Provider Type"
   FIELD provider_type_indicator             AS CHARACTER       FORMAT "x(30)"                      LABEL "Provider Type Indicator"
   FIELD quantity_auth                       AS INTEGER         FORMAT ">>>>9"                      LABEL "Authorised Quantity"
   FIELD valid_claim_codes_detail            AS CHARACTER       FORMAT "x(80)"                      LABEL "Valid Claim Codes Detail"
   FIELD valid_claim_types_detail            AS CHARACTER       FORMAT "x(10)"                      LABEL "Valid Claim Types Detail"
   FIELD auth_group_obj                      AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"  LABEL "Auth Group Obj"
   FIELD auth_auto_create                    AS LOGICAL         FORMAT "YES/NO"                     LABEL "Auth Auto Create"
  
  INDEX xPK1_hac_auth_type_provider AS PRIMARY UNIQUE
    auth_type_provider_obj
  
  INDEX xAK1_hac_auth_type_provider AS UNIQUE
    auth_type_obj            ASCENDING 
    insurer_obj              ASCENDING 
    option_code              ASCENDING 
    provider_type_indicator  ASCENDING 
    provider_type            ASCENDING 
    neg_num                  ASCENDING
    pr_type_list             ASCENDING 
    effective_date           DESCENDING  

  INDEX xAK2_hac_auth_type_provider
    auth_type_obj            ASCENDING 
    insurer_obj              ASCENDING 
    option_code              ASCENDING 
    provider_type_indicator  ASCENDING 
    provider_type            ASCENDING 
    neg_num                  ASCENDING
    pr_type_list             ASCENDING 
    end_date                 DESCENDING
   .

