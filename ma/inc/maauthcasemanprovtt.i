/* maauthcasemanprovtt.i MEDSTAR Medical Aid System
                        Healthcare temp table definition for ham_cae_manager_provider table
                        (c) Copyright 2015 - 2018
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
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_case_manager_provider
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF
  
  FIELD record_action                       AS CHARACTER             FORMAT "x(60)"
  FIELD line_number                         AS INTEGER               FORMAT "999"
                                                                    
  FIELD case_manager_provider_obj           AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD case_manager_obj                    AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD neg_num                             AS INTEGER               FORMAT ">>>9"
  FIELD doc_num                             AS INTEGER               FORMAT "9999999"
  FIELD start_date                          AS DATE                  FORMAT "9999/99/99"
  FIELD end_date                            AS DATE                  FORMAT "9999/99/99"
  FIELD allocated_from_case_manager_obj     AS DECIMAL               FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD reallocated                         AS LOGICAL INITIAL FALSE FORMAT "true/false"

  INDEX xPK_ham_case_manager_provider AS PRIMARY UNIQUE
    case_manager_provider_obj
  
  INDEX xAK1_ham_case_manager_provider AS UNIQUE
    case_manager_obj
    neg_num
    doc_num
    start_date DESCENDING
    
  INDEX xAK2_ham_case_manager_provider AS UNIQUE
    case_manager_obj
    neg_num
    doc_num
    end_date DESCENDING.
    
