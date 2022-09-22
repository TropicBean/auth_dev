/* maresulttt.i MEDSTAR Medical Aid System
                Healthcare temp table definition
                (c) Copyright 2015 - 2018
                MIP Holdings (Pty) Ltd
                All rights reserved
*/

/* ************************************  Definitions  *********************************** */
&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_result
&ENDIF

&IF DEFINED(REFERENCE-ONLY) > 0 
&THEN 
  &SCOPED-DEFINE ReferenceOnly {&REFERENCE-ONLY} 
&ELSE 
  &SCOPED-DEFINE ReferenceOnly 
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO SERIALIZE-NAME "tt_result" {&ReferenceOnly}
  FIELD records_processed AS INTEGER
  FIELD records_created   AS INTEGER
  FIELD records_modified  AS INTEGER 
  FIELD records_removed   AS INTEGER
  FIELD records_found     AS INTEGER
  FIELD records_locked    AS INTEGER
  FIELD number_of_errors  AS INTEGER.

