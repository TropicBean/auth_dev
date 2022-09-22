
/*------------------------------------------------------------------------------
  MEDSTAR Medical Aid System
  Healthcare temp table definition
  (c) Copyright 2015 - 2021
  MIP Holdings (Pty) Ltd
  All rights reserved
  
  Filename    : ma/inc/matariffcopytt.i
  Purpose     : For use in auth copy processing 
------------------------------------------------------------------------------*/

&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_authcopy
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF   
  FIELD record_action           AS CHARACTER   LABEL "Action":U
  FIELD copy_type	            AS CHARACTER   LABEL "Copy Type":U
  FIELD auth_num                AS CHARACTER   LABEL "Authorisation Number":U
  FIELD from_start_date	        AS DATE        LABEL "From Start Date":U
  FIELD from_end_date           AS DATE	       LABEL "From End Date":U
  FIELD option_code             AS INTEGER	   LABEL "Option":U
  FIELD insurer_obj	            AS DECIMAL	   LABEL "Client":U
  FIELD auth_type_list          AS CHARACTER   LABEL "Auth Type List":U
  FIELD to_start_date           AS DATE	       LABEL "To Start Date":U
  FIELD to_end_date             AS DATE        LABEL "To End Date":U
  FIELD provider_info           AS LOGICAL	   LABEL "Copy Provider Info":U
  FIELD coding_icd              AS LOGICAL	   LABEL "Copy Coding Diagnosis (ICD)":U
  FIELD coding_cpt              AS LOGICAL     LABEL "Copy Coding Procedure (CPT)":U
  FIELD clinical_details        AS LOGICAL	   LABEL "Copy Clinical Details":U
  FIELD flag_info               AS LOGICAL	   LABEL "Copy Flag Information":U
  FIELD external_ref            AS LOGICAL     LABEL "Copy External References List":U
  FIELD job_reference           AS CHARACTER   LABEL "Job Reference":U . 


