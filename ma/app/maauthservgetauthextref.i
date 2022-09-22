/* maauthservgetauthextref.i MEDSTAR Medical Aid System
                             Healthcare Auth data access service: Get auth external references                              
                             (c) Copyright 2018 - 2019
                             MIP Holdings (Pty) Ltd
                             All rights reserved
------------------------------------------------------------------------------
  Purpose   : Get resolved auth external reference records for auth
  Parameters:
  Notes     : 
------------------------------------------------------------------------------*/
  DEFINE INPUT        PARAMETER ipdAuthObj       AS DECIMAL     NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsExternalReference.
  
  DEFINE VARIABLE dDummyObj          AS DECIMAL                 NO-UNDO.
  DEFINE VARIABLE oSearch            AS cls.maexternalrefsearch NO-UNDO.
  DEFINE VARIABLE lSuccess           AS LOGICAL                 NO-UNDO.
  DEFINE VARIABLE cPrTypeList        AS CHARACTER               NO-UNDO.
  DEFINE VARIABLE cPrType            AS CHARACTER               NO-UNDO.
  DEFINE VARIABLE lSkipRef           AS LOGICAL                 NO-UNDO.
  DEFINE VARIABLE lValidUpdateExtRef AS LOGICAL                 NO-UNDO.
  DEFINE VARIABLE lValidRule         AS LOGICAL                 NO-UNDO.
  DEFINE VARIABLE cRuleValue         AS CHARACTER               NO-UNDO.
  DEFINE VARIABLE lUpdateValidRule   AS LOGICAL                 NO-UNDO.
  DEFINE VARIABLE cUpdateRuleValue   AS CHARACTER               NO-UNDO.
  DEFINE VARIABLE cUpdateExtRefValue AS CHARACTER               NO-UNDO.
  DEFINE VARIABLE iEntry             AS INTEGER                 NO-UNDO.
  DEFINE VARIABLE iOptionCode        AS INTEGER                 NO-UNDO.

&IF {&DBDFMA} >= 010195 &THEN

  DEFINE BUFFER hat_auth_provider FOR hat_auth_provider.
  DEFINE BUFFER hat_auth          FOR hat_auth.

  DATASET dsExternalReference:EMPTY-DATASET().
  
  /*
     Fetch the auth record so that we can use the details on the auth
  */
  IF ipdAuthObj <= 0 
  OR ipdAuthObj  = ?
  THEN RETURN.
    
  FIND FIRST hat_auth NO-LOCK
       WHERE hat_auth.auth_obj = ipdAuthObj
    NO-ERROR.
  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
    
  IF NOT AVAILABLE hat_auth
  THEN 
    RETURN.
  
  IF hat_auth.option_code = 0
  THEN DO:
    mipEnv:Health:maMember:getMemberOption
      ( INPUT  hat_auth.mem_num,
        INPUT  hat_auth.start_date,
        OUTPUT iOptionCode ).
  END. /* IF hat_auth.option_code = 0 */
  ELSE
    ASSIGN iOptionCode = hat_auth.option_code.

  /*
    Make sure we dont retrieve the record if we already have it
  */
  oSearch = NEW cls.maexternalrefsearch(DATASET dsExternalReference BY-REFERENCE).

  oSearch:SetCriteria("BufferList":U, "tt_extref,tt_schintref":U).

  oSearch:fetchExternalReferences(INPUT "hatau":U, INPUT ipdAuthObj, INPUT "":U).

  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0,
                                                 INPUT  iOptionCode,
                                                 INPUT  "ma_acAuthRuleTypeExtRef":U,
                                                 INPUT  "ExtRefUpd":U,
                                                 INPUT  hat_auth.start_date,
                                                 OUTPUT lUpdateValidRule,
                                                 OUTPUT cUpdateRuleValue). 

  FOR EACH tt_extref EXCLUSIVE-LOCK:   
    /* 
      Assigning the updateable flag to records already in the database 
    */
    IF lUpdateValidRule = TRUE THEN 
    DO:

      IF LOOKUP(tt_extref.interface-type, cUpdateRuleValue) > 0
      THEN ASSIGN tt_extref.reference-updateable = TRUE.
      ELSE ASSIGN tt_extref.reference-updateable = FALSE.

    END.  /* IF lUpdateValidRule = TRUE THEN */
    ELSE ASSIGN tt_extref.reference-updateable = TRUE.

    VALIDATE tt_extref.

  END.  /* FOR EACH tt_extref NO-LOCK: */

  /*
     Get the list of practice types for the auth
  */ 
  FOR EACH hat_auth_provider NO-LOCK
     WHERE hat_auth_provider.auth_obj = hat_auth.auth_obj:

    ASSIGN cPrTypeList = cPrTypeList + "," + STRING(hat_auth_provider.pr_type).
  END.  /* FOR EACH hat_auth_provider NO-LOCK */

  ASSIGN cPrTypeList = SUBSTRING(cPrTypeList,2).
  
  /*
     Step through the interface reference setup for the scheme option and create tt_extref
     records if none has been created yet. MMP277 this should only be done if the auth hasnt been save by the user yet
  */
  SCHINT-LOOP:
  FOR EACH sysint NO-LOCK
      WHERE sysint.interface-active
      AND  (sysint.interface-end  = ? OR sysint.interface-end > hat_auth.start_date) :

      /* 
        MMP 277 only records that the user can update must be returned
      */
      IF lUpdateValidRule = TRUE THEN 
      DO:

        IF LOOKUP(sysint.interface-type, cUpdateRuleValue) = 0
        THEN NEXT SCHINT-LOOP.
        
      END.  /* IF lUpdateValidRule = TRUE THEN */

      SCHINTREF-LOOP:
      FOR EACH schintref NO-LOCK
          WHERE (schintref.scheme-code            = 0
          OR     schintref.scheme-code            = iOptionCode)
          AND    schintref.interface-type         = sysint.interface-type
          AND    schintref.owning-entity-mnemonic = "hatau":U:
          
          mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  0,
                                                         INPUT  iOptionCode,
                                                         INPUT  "ma_acAuthRuleTypeExtRef":U,
                                                         INPUT  schintref.reference-code,
                                                         INPUT  hat_auth.start_date,
                                                         OUTPUT lValidRule,
                                                         OUTPUT cRuleValue). 
          IF lValidRule THEN 
          DO:
            ASSIGN lSkipRef = TRUE.
            
            DO iEntry = 1 TO NUM-ENTRIES(cPrTypeList,","):
              
              ASSIGN cPrType = ENTRY(iEntry,cPrTypeList,",").
              
              { ma/msc/madispad.i &discipline = cPrType }
              
              IF LOOKUP(cPrType,cRuleValue) <> 0 THEN
                ASSIGN lSkipRef = FALSE.
            END.  /* DO iEntry = 1 TO NUM-ENTRIES(cPrTypeList,","): */

            IF lSkipRef THEN
              NEXT SCHINTREF-LOOP.
          
          END.  /* IF lValidRule THEN */

          FIND FIRST tt_extref EXCLUSIVE-LOCK
               WHERE tt_extref.interface-type         = schintref.interface-type
               AND   tt_extref.owning-entity-mnemonic = schintref.owning-entity-mnemonic
               AND   tt_extref.owning-key             = "":U
               AND   tt_extref.owning-obj             = hat_auth.auth_obj
               AND   tt_extref.reference-code         = schintref.reference-code
               NO-ERROR.
  
          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors= TRUE }

          IF NOT AVAILABLE tt_extref THEN
          DO:
              CREATE tt_extref.
              ASSIGN dDummyObj                        = dDummyObj - 1
                     tt_extref.interface-type         = schintref.interface-type
                     tt_extref.owning-entity-mnemonic = schintref.owning-entity-mnemonic
                     tt_extref.owning-key             = ""
                     tt_extref.owning-obj             = hat_auth.auth_obj
                     tt_extref.reference-code         = schintref.reference-code
                     tt_extref.extref-obj             = dDummyObj      
                     tt_extref.owning-alt-value       = hat_auth.auth_num
                     tt_extref.transaction-date       = hat_auth.start_date
                     tt_extref.last-change-datetime   = NOW
                     tt_extref.record_action          = "MODIFY":U.
          END. /* IF NOT AVAILABLE tt_extref THEN */
        
          ASSIGN tt_extref.reference-mandatory    = schintref.reference-mandatory
                 tt_extref.reference-multiple     = schintref.reference-multiple
                 tt_extref.reference-updateable   = TRUE.
                 
          VALIDATE tt_extref.
          
      END. /* FOR EACH schintref NO-LOCK */
  END.  /* FOR EACH sysint NO-LOCK */
  
&ENDIF  

{ mip/inc/mipcatcherror.i 
  &FINALLY = "DATASET dsAuthorisation:EMPTY-DATASET().
              
              IF VALID-OBJECT(oSearch)     THEN DELETE OBJECT oSearch."
}
