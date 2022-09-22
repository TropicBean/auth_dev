/* maauthcopaytypett.i MEDSTAR Medical Aid System
                  Healthcare Copay Type temp-table definition
                  (c) Copyright 2021
                  MIP Holdings (Pty) Ltd
                  All rights reserved   
                  
   Author:  GrahamW                            
*/
  
/* ************************************  Definitions  *********************************** */
/* &ACCESS is used to make temp table NEW GLOBAL SHARED PROTECTED PRIVATE or STATIC       */
/* BEFORE is used to define a before table for the temp table                             */
  
/* Some other options that can be specified on fields :                                   */
/*   FORMAT "x(36)":U LABEL "MenuGuid":T INIT 0                                           */
/*   SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U       */
  
/* Do not define the Temp table or fields using LIKE. This would make the front end       */
/* dependant on database connection and would also mean that we have to Delta protect     */
/* front end code as soon as we roll out database changes.                                */
  
&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_copay_type
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         
  FIELD auth_copay_type                     AS CHARACTER       FORMAT "x(25)"
  FIELD description                         AS CHARACTER       FORMAT "x(80)"
  FIELD auth_copay_type_obj                 AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  
  INDEX xPK_ham_auth_copay_type AS PRIMARY UNIQUE
    auth_copay_type_obj
  
  INDEX xAK1_ham_auth_auth_copay_type
    auth_copay_type 
  .
