/* maauthbusvalauthexcl.i  MEDSTAR Medical Aid System
                           Validate Auth type exclusion setups
                           (c) Copyright 2022
                           MIP Holdings (Pty) Ltd
                           All rights reserved
*/ 
DEFINE VARIABLE dAgeRangeBothObj   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAgeRangeFemaleObj AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAgeRangeMaleObj   AS DECIMAL     NO-UNDO.

ASSIGN cTrackingMessage = "Auth Type Control - Searching for Exclusion: AuthTypeObj=":U + STRING(btt_auth.auth_type_obj)
                        + " InsurerObj=" + STRING(btt_auth.insurer_obj) + " Option=" + STRING(iOptionCode) + " StartDate=" + STRING(btt_auth.start_date).

{ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

{ ma/msc/maauthtypecontrolread.i 
                   &hac_auth_type_control = hac_auth_type_control
                   &AuthTypeObj           = btt_auth.auth_type_obj
                   &InsurerObj            = btt_auth.insurer_obj
                   &OptionCode            = iOptionCode
                   &ControlTypeIndicator  = "'ma_acAuthControlTypeIndicatorExcl'":U
                   &DATE                  = btt_auth.start_date
                   &Lock                  = NO-LOCK }

IF AVAILABLE hac_auth_type_control THEN
DO:
  ASSIGN dAgeRangeBothObj   = IF hac_auth_type_control.age_range_both_obj = ?
                              THEN 0
                              ELSE hac_auth_type_control.age_range_both_obj
         dAgeRangeFemaleObj = IF hac_auth_type_control.age_range_female_obj = ?
                              THEN 0
                              ELSE hac_auth_type_control.age_range_female_obj
         dAgeRangeMaleObj   = IF hac_auth_type_control.age_range_male_obj = ?
                              THEN 0
                              ELSE hac_auth_type_control.age_range_male_obj.

  ASSIGN cTrackingMessage = "Auth Type Control - Exclusion found: ":U + STRING(hac_auth_type_control.auth_type_control_obj)
                          + " Gender=" + hac_auth_type_control.gender + " FemaleObj=" + STRING(dAgeRangeFemaleObj)
                          + " MaleOb=" + STRING(dAgeRangeMaleObj) + " BothObj=" + STRING(dAgeRangeBothObj)
                          + " Option=" + STRING(hac_auth_type_control.option_code).
  
  {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}

  /* 
     Exclusion setup exists to indicate no benefits are allowed for the 
     Authorisation Type for the scheme option.  Authorisation must be declined.  
     OR
     Exclusion setup exists with gender setup for the Authorisation Type.  
     Authorisation must be declined if gender is invalid.                                     
  */
   mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  btt_auth.insurer_obj,
                                                  INPUT  btt_auth.option_code,
                                                  INPUT  "ma_acAuthRuleTypeAuthReg":U,
                                                  INPUT  "AuthTypeExclWarn":U,
                                                  INPUT  btt_auth.start_date,
                                                  OUTPUT lAuthTypeExclValidRule,
                                                  OUTPUT cAuthTypeExclRuleValue).
   /* Scheme exclusion for specific option. */
   IF ((hac_auth_type_control.gender = "B":U OR hac_auth_type_control.gender = "")
   AND  dAgeRangeFemaleObj = 0 	
   AND  dAgeRangeMaleObj   = 0 
   AND  dAgeRangeBothObj   = 0 
   AND  hac_auth_type_control.option_code <> 0)
   /* Gender exclusion. */
   OR  (hac_auth_type_control.gender <> ""      
   AND  hac_auth_type_control.gender <> "B":U
   AND  hac_auth_type_control.gender <> btt_auth._dependant_gender) THEN 
   DO:
     IF NOT AVAILABLE buf_auth_schext THEN
     DO:
       ASSIGN cTrackingMessage = "buf_auth_schext not available in maauthbusvalauthexcl.i". 
  
       {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
     END.  // IF NOT AVAILABLE buf_auth_schext THEN

     ASSIGN btt_auth.auth_status		  = INTEGER(hac_auth_type_control.default_auth_status)
            btt_auth.auth_status_note	= hac_auth_type_control.default_auth_status_note
            btt_auth.claim_code       = IF AVAILABLE buf_auth_schext
                                        THEN buf_auth_schext.claim-code[1] /*Assign claim code to 99. */
                                        ELSE 99.
  
     IF lAuthTypeExclValidRule
     AND cAuthTypeExclRuleValue = "WarnAck":U
     THEN
       ASSIGN lAcknowledge = TRUE.
     ELSE
       ASSIGN lAcknowledge = FALSE.
  
     IF hac_auth_type_control.gender = "B":U 
     OR hac_auth_type_control.gender = "" 
     THEN
       ASSIGN cMessage = "Authorsation Type Control option exclusion applied; authorisation is declined.".
     ELSE
       ASSIGN cMessage = "Authorsation Type Control gender exclusion applied; authorisation is declined.".
  
     ASSIGN cTrackingMessage = cMessage + " Gender=":U + hac_auth_type_control.gender. 
  
     {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
  
     ASSIGN cMessage = cMessage + "[HELP=Auth Rule Code: AuthTypeExclWarn]":U.
  
     goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                            INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                            INPUT "":U,                  /* ipcOwningEntityKey       */
                            INPUT "dependant":U,         /* ipcFieldName             */
                            INPUT btt_auth.line_number,  /* ipiLineNumber            */
                            INPUT cMessage,              /* ipcMessageText           */
                            INPUT "WAR":U,               /* ipcMessageType           */
                            INPUT lAcknowledge).         /* iplAcknowledge           */
  END.  // IF ((hac_auth_type_control.gender = "B" OR...
  
  /* 
     Exclusion setup exists with age setup for the Authorisation Type.
     Authorisations must be declined if age is invalid.
  */
  ELSE IF dAgeRangeBothObj   <> 0 
       OR dAgeRangeFemaleObj <> 0 
       OR dAgeRangeMaleObj   <> 0 THEN
  DO:
    ASSIGN lAgeValid = mipEnv:Health:maMember:validateDependantAge(INPUT btt_auth._dependant_age_years,
                                                                   INPUT btt_auth._dependant_gender,
                                                                   INPUT dAgeRangeBothObj,
                                                                   INPUT dAgeRangeFemaleObj,
                                                                   INPUT dAgeRangeMaleObj,
                                                                   OUTPUT cErrorMessage).
    IF NOT lAgeValid THEN
    DO:
      IF NOT AVAILABLE buf_auth_schext THEN
      DO:
        ASSIGN cTrackingMessage = "buf_auth_schext not available in maauthbusvalauthexcl.i". 
      
        {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
      END.  // IF NOT AVAILABLE buf_auth_schext THEN

      ASSIGN btt_auth.auth_status      = INTEGER(hac_auth_type_control.default_auth_status)
             btt_auth.auth_status_note = hac_auth_type_control.default_auth_status_note
             btt_auth.claim_code       = IF AVAILABLE buf_auth_schext
                                         THEN buf_auth_schext.claim-code[1]  /* Assign claim code to 99 */
                                         ELSE 99.
      
      IF  lAuthTypeExclValidRule
      AND cAuthTypeExclRuleValue = "WarnAck":U
      THEN
        ASSIGN lAcknowledge = TRUE.
      ELSE
        ASSIGN lAcknowledge = FALSE.
      
      ASSIGN cMessage = "Authorsation Type Control age exclusion applied - " + cErrorMessage + "; Authorisation is declined."
             cTrackingMessage = "Auth=" + STRING(btt_auth.auth_num) + " AuthGender=" + btt_auth._dependant_gender
                              + " AuthAge=" + STRING(btt_auth._dependant_age_years) + "|"
                              + cMessage + " Gender=":U + hac_auth_type_control.gender
                              + " AgeRangeBothObj=":U   + STRING(dAgeRangeBothObj) 
                              + " AgeRangeMaleObj=":U   + STRING(dAgeRangeFemaleObj)
                              + " AgeRangeFemaleObj=":U + STRING(dAgeRangeMaleObj).
  
     {ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage}
  
      ASSIGN cMessage = cMessage + "[HELP=Auth Rule Code: AuthTypeExclWarn]":U.
      
      goErrorObject:addError(INPUT "hatau":U,             /* ipcOwningEntityMnemonic  */
                             INPUT btt_auth.auth_obj,     /* ipdOwningEntityObj       */
                             INPUT "":U,                  /* ipcOwningEntityKey       */
                             INPUT "dependant":U,         /* ipcFieldName             */
                             INPUT btt_auth.line_number,  /* ipiLineNumber            */
                             INPUT cMessage,              /* ipcMessageText           */
                             INPUT "WAR":U,               /* ipcMessageType           */
                             INPUT lAcknowledge).         /* iplAcknowledge           */
    END.  // IF NOT lAgeValid THEN
  END.  // ELSE IF hac_auth_type_control,age_range_both_obj <> 0 AND...
END.  // IF AVAILABLE hac_auth_type_control THEN
