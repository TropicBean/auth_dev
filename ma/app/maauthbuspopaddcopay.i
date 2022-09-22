/* maauthbuspopaddcopay.i  MEDSTAR Medical Aid System
                           Populate additional data for Auth Copay Containers
                           Used for performance improvement when
                           rendering the container
                           (c) Copyright 1990 - 2022
                           MIP Holdings (Pty) Ltd
                           All rights reserved
*/

FOR EACH btt_auth_copay
  WHERE btt_auth_copay.auth_obj               = btt_auth.auth_obj
  AND   btt_auth_copay.owning_entity_mnemonic = "hatad":U:

  FIND btt_auth_detail NO-LOCK
       WHERE btt_auth_detail.auth_detail_obj = btt_auth_copay.owning_obj
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE btt_auth_detail THEN
  DO:
    FIND btt_auth_provider NO-LOCK
         WHERE btt_auth_provider.auth_provider_obj = btt_auth_detail.auth_provider_obj
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE btt_auth_provider THEN
    DO:
      FIND doctor NO-LOCK
           WHERE doctor.doc-num = btt_auth_provider.doc_num
           AND   doctor.doc-num <> 0
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE doctor
      THEN
        ASSIGN btt_auth_copay._auth_provider = UPPER(doctor.name) + " (":U + STRING(doctor.doc-num) + ")":U.
      ELSE DO:
        FIND prtype NO-LOCK
             WHERE prtype.pr-type = btt_auth_provider.pr_type
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }
      
        IF AVAILABLE prtype
        THEN
          ASSIGN btt_auth_copay._auth_provider = UPPER(prtype.description) + " (":U + STRING(prtype.pr-type) + ")":U.
      
      END.  // ELSE - IF AVAILABLE doctor
    END.  // IF AVAILABLE btt_auth_provider THEN
  END.  // IF AVAILABLE btt_auth_detail THEN
END.  // FOR EACH btt_auth_copay:

FOR EACH btt_auth_copay_history
  WHERE btt_auth_copay_history.auth_obj               = btt_auth.auth_obj
  AND   btt_auth_copay_history.owning_entity_mnemonic = "hatad":U:

  FIND LAST btt_auth_detail_history NO-LOCK
       WHERE btt_auth_detail_history.auth_detail_obj   = btt_auth_copay_history.owning_obj
       AND   btt_auth_detail_history.change_date_time <= btt_auth_copay_history.change_date_time
    NO-ERROR.

  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

  IF AVAILABLE btt_auth_detail_history THEN
  DO:
    FIND FIRST btt_auth_provider_history NO-LOCK
         WHERE btt_auth_provider_history.auth_provider_obj = btt_auth_detail_history.auth_provider_obj
         AND   btt_auth_provider_history.change_date_time <= btt_auth_detail_history.change_date_time
      NO-ERROR.

    { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

    IF AVAILABLE btt_auth_provider_history THEN
    DO:
      FIND doctor NO-LOCK
           WHERE doctor.doc-num = btt_auth_provider_history.doc_num
           AND   doctor.doc-num <> 0
        NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

      IF AVAILABLE doctor
      THEN
        ASSIGN btt_auth_copay_history._auth_provider = UPPER(doctor.name) + " (":U + STRING(doctor.doc-num) + ")":U.
      ELSE DO:
        FIND prtype NO-LOCK
             WHERE prtype.pr-type = btt_auth_provider_history.pr_type
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138' &ResetIgnoredErrors = TRUE }

        IF AVAILABLE prtype
        THEN
          ASSIGN btt_auth_copay_history._auth_provider = UPPER(prtype.description) + " (":U + STRING(prtype.pr-type) + ")":U.
      END.  // ELSE - IF AVAILABLE doctor
    END.  // IF AVAILABLE btt_auth_provider_history THEN
  END.  // IF AVAILABLE btt_auth_detail_history THEN
END.  // FOR EACH btt_auth_copay_history
