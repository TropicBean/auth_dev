/* maauthbuspopaddcoding.i  MEDSTAR Medical Aid System
                              Populate additional data for Coding Containers
                              Used for performance improvement when 
                              rendering the containers
                              (c) Copyright 1990 - 2020
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/       
DEFINE VARIABLE cOwningKey           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dAsteriskObj         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dCodingObj           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dCodingStartDate     AS DATE        NO-UNDO.
DEFINE VARIABLE dDepCondStartDate    AS DATE        NO-UNDO.
DEFINE VARIABLE lAsterisk            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lDisplayExtraDetails AS LOGICAL     NO-UNDO.

DEFINE BUFFER btt_auth_coding FOR tt_auth_coding.
DEFINE BUFFER buf_diagnos     FOR diagnos.

FOR EACH btt_auth_coding:

  /* CodingOwningDescription */
  ASSIGN cOEM       =         btt_auth_coding.owning_entity_mnemonic
         cOwningKey =         btt_auth_coding.owning_key
         dOwningObj = DECIMAL(btt_auth_coding.owning_obj).

  IF cOEM <> "":U THEN 
  DO:
    mipEnv:miDBEntity:focusTable(cOEM).

    IF mipEnv:miDBEntity:InFocus THEN
      ASSIGN lSuccess                            = (IF mipEnv:miDBEntity:MainIndexType = "OBJ":U 
                                                    THEN mipEnv:miDBEntity:findRecord(dOwningObj)
                                                    ELSE mipEnv:miDBEntity:findRecord(cOwningKey))
             btt_auth_coding._owning_description = IF mipEnv:miDBEntity:RecordAvailable 
                                                   THEN mipEnv:miDBEntity:RecordDescription
                                                   ELSE "":U. 
  END. /*IF cOEM <> "":U THEN*/

  /* AuthCodingPMBIndicatorEnabled */
  ASSIGN dCodingObj       = DECIMAL(btt_auth_coding.auth_coding_obj)
         dCodingStartDate = IF DATE(btt_auth_coding.start_date) = ?
                            THEN TODAY
                            ELSE DATE(btt_auth_coding.start_date).

  ASSIGN lSuccess = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                      (INPUT  dInsurerObj,
                       INPUT  iOption,
                       INPUT  "ma_acAuthRuleTypeAuthCoding":U,
                       INPUT  "PMBDecision":U,
                       INPUT  dCodingStartDate,
                       OUTPUT lValidRule,
                       OUTPUT cRuleValue)

         btt_auth_coding._pmbindicator_enabled = (IF LOOKUP("none,no,false":U, cRuleValue) > 0
                                                  THEN "Hidden":U
                                                  ELSE
                                                    (IF LOOKUP("user":U, cRuleValue) > 0
                                                     THEN "Updatable":U
                                                     ELSE "Disabled":U)).

  /* 
     Check Asterisk 
  */
  ASSIGN cOwningKey = btt_auth_coding.owning_key.

  FIND FIRST buf_diagnos NO-LOCK
       WHERE buf_diagnos.diagnosis = cOwningKey 
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE buf_diagnos THEN
    mipEnv:Health:maMedical:checkAsterisk(INPUT buf_diagnos.diagnos-obj, OUTPUT btt_auth_coding._check_asterisk).

  IF btt_auth_coding._check_asterisk THEN
  DO:
    FIND FIRST buf_diagnos NO-LOCK
         WHERE buf_diagnos.diagnos-obj = btt_auth_coding.ass_diag_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE buf_diagnos THEN 
      ASSIGN btt_auth_coding._asterisk_code = buf_diagnos.diagnosis + "*":U.
    ELSE
      ASSIGN btt_auth_coding._asterisk_code = "":U.

  END.  /* IF btt_auth_coding._check_asterisk THEN */

  /*
     Check Morphology
  */ 
  ASSIGN cOwningKey = btt_auth_coding.owning_key.

  IF CAN-FIND(FIRST buf_diagnos NO-LOCK
              WHERE buf_diagnos.diagnosis = btt_auth_coding.owning_key) THEN
    mipEnv:Health:maMedical:checkMorphology(INPUT btt_auth_coding.owning_key, OUTPUT btt_auth_coding._check_morphology).

  IF btt_auth_coding._check_morphology THEN 
  DO:
    FIND FIRST buf_diagnos NO-LOCK
         WHERE buf_diagnos.diagnos-obj = btt_auth_coding.morph_diag_obj 
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE buf_diagnos THEN 
      ASSIGN btt_auth_coding._morphology_code = buf_diagnos.diagnosis.
    ELSE 
      ASSIGN btt_auth_coding._morphology_code = "":U.

  END. /* btt_auth_coding._check_morphology */

END. /* FOR EACH btt_auth_coding: */

