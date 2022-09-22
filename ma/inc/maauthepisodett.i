/* maauthepisodett.i MEDSTAR Medical Aid System
                 Healthcare temp table definition
                 (c) Copyright 2015 - 2017
                 MIP Holdings (Pty) Ltd
                 All rights reserved
*/

/* ************************************  Definitions  *********************************** */
/* &ACCESS is used to make temp table NEW GLOBAL SHARED PROTECTED PRIVATE or STATIC       */
/* BEFORE is used to define a before table for the temp table                             */

/* Some other options that can be specified on fields :                                   */
/*   FORMAT "x(36)":U LABEL "MenuGuid":T INIT 0                                       */
/*   SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U */

&IF DEFINED(TEMP-TABLE-NAME) = 0 &THEN
  &SCOPED-DEFINE TEMP-TABLE-NAME tt_auth_episode
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE {&TEMP-TABLE-NAME} NO-UNDO {&REFERENCE-ONLY}
  &IF {&BEFORE} &THEN BEFORE-TABLE {&TEMP-TABLE-NAME}_before &ENDIF

  FIELD record_action                       AS CHARACTER       FORMAT "x(60)"
  FIELD line_number                         AS INTEGER         FORMAT "999"
  FIELD dependant                           AS INTEGER         FORMAT "99"
  FIELD episode_num                         AS CHARACTER       FORMAT "x(15)"
  FIELD auth_episode_obj                    AS DECIMAL         FORMAT ">>>>>>>>>>>>>>>>>9.999999"
  FIELD mem_num                             AS CHARACTER       FORMAT "x(13)"
  
  INDEX xPK_hat_auth_episode AS PRIMARY UNIQUE
    auth_episode_obj 
    
  INDEX xAK1_hat_auth_episode 
    episode_num
    .
