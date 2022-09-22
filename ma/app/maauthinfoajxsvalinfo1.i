/* maauthinfoajxsvalinfo.i MEDSTAR Medical Aid System
                         Auth Container Ajax Validations Procedure
                         (c) Copyright 1990 - 2022
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/

  DEFINE INPUT PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

  DEFINE VARIABLE lcConfiguration      AS LONGCHAR             NO-UNDO. 
  DEFINE VARIABLE cAuthStatus          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStatusAction        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthTypeCode        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthTypeDesc        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cAuthTypeGroup       AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cErrorOrWarning      AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cErrorMessage        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cFilterFieldList     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cFilterField         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cFilterValueList     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cFilterValue         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cHelpString          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cHelpMessage         AS CHARACTER            NO-UNDO.         
  DEFINE VARIABLE cMemberNumber        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cReasonCode          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cReasonType          AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cReturnFieldList     AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cReturnField         AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cReturnValues        AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cRuleValue           AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cReasonDescription   AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE cStringWithHelp      AS CHARACTER            NO-UNDO.        
  DEFINE VARIABLE cValidationType      AS CHARACTER            NO-UNDO.
  DEFINE VARIABLE iAuthStatus          AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iClaimCode           AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iDependant           AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iEpisodeNumber       AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iEndOfHelpIndex      AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iFilterField         AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iOptionCode          AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iReturnField         AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iStatusCode          AS INTEGER              NO-UNDO.
  DEFINE VARIABLE iNumReturnFields     AS INTEGER              NO-UNDO.
  DEFINE VARIABLE lMandatory           AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lSuccess             AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lUpdatesAllowed      AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lAuthTypeUpdtAllowed AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE lValid               AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE dAuthObj             AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dAuthTypeObj         AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dInsurerObj          AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE dStartDate           AS DATE                 NO-UNDO.
  DEFINE VARIABLE lValidRule           AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE oAuthorisation       AS cls.maauthorisation  NO-UNDO.

  DEFINE BUFFER hac_auth_type FOR hac_auth_type.

  &SCOPED-DEFINE AppendReturnValues ASSIGN cReturnValues = cReturnValues + (IF cReturnValues = "":U THEN "":U ELSE "|":U) +
  
  &SCOPED-DEFINE ValidationSuccess  ASSIGN ttValidation.cReturnValues = cReturnValues ~
                                           ttValidation.lValid        = TRUE          ~
                                           ttValidation.cMessage      = "Success":U.  ~
                                                                                      ~
                                    VALIDATE ttValidation.  
  
  &SCOPED-DEFINE BlankResponse      ASSIGN ttValidation.cReturnValues = FILL("|":U, NUM-ENTRIES(cReturnFieldList) - 1) ~
                                           ttValidation.lValid        = TRUE.                                          ~
                                                                                                                      ~
                                    VALIDATE ttValidation.

  ASSIGN
    cFilterFieldList = get-value('FldLst':U)
    cFilterValueList = get-value('ValList':U)
    cReturnFieldList = get-value('RetFldList':U)
    iNumReturnFields = NUM-ENTRIES(cReturnFieldList).
  
  CREATE ttValidation.
  
  DO iFilterField = 1 TO NUM-ENTRIES(cFilterFieldList):
  
    ASSIGN cFilterField = ENTRY(iFilterField, cFilterFieldList)
           cFilterValue = ENTRY(iFilterField, cFilterValueList).
    
    CASE cFilterField:
      WHEN "[AuthObj]":U
      THEN ASSIGN dAuthObj = DECIMAL(cFilterValue).
        
      WHEN "[AuthTypeObj]":U 
      THEN ASSIGN dAuthTypeObj = DECIMAL(cFilterValue).
      
      WHEN "[AuthTypeCode]":U 
      THEN ASSIGN cAuthTypeCode = cFilterValue.

      WHEN "[UpdatesAllowed]":U 
      THEN ASSIGN lAuthTypeUpdtAllowed = LOGICAL(cFilterValue).
      
      WHEN "[AuthTypeGroup]":U 
      THEN ASSIGN cAuthTypeGroup = cFilterValue.
      
      WHEN "[InsurerObj]":U 
      THEN ASSIGN dInsurerObj = DECIMAL(cFilterValue).
      
      WHEN "[OptionCode]":U 
      THEN ASSIGN iOptionCode = INTEGER(cFilterValue).
              
      WHEN "[StartDate]":U THEN 
      DO:
        ASSIGN dStartDate = DATE(cFilterValue) NO-ERROR.
        {&ResetError}
      END.
      
      WHEN "[ClaimCode]":U 
      THEN ASSIGN iClaimCode = INTEGER(cFilterValue).
      
      WHEN "[MemberNumber]":U 
      THEN ASSIGN cMemberNumber = cFilterValue.
      
      WHEN "[Dependant]":U 
      THEN ASSIGN iDependant = INTEGER(cFilterValue).
      
      WHEN "[StatusCode]":U 
      THEN ASSIGN iStatusCode = INTEGER(cFilterValue).
      
      WHEN "[ReasonCode]":U
      THEN ASSIGN cReasonCode = cFilterValue.
      
      WHEN "[ReasonType]":U 
      THEN ASSIGN cReasonType = cFilterValue.
      
      WHEN "[EpisodeNumber]":U 
      THEN ASSIGN iEpisodeNumber = INTEGER(cFilterValue).
      
    END CASE.
  END. /*DO iFilter = 1 TO NUM-ENTRIES(cFilterFields):*/
  
  CASE ipcValidationArgument:
  
    WHEN "AuthType":U
    THEN DO:
      ASSIGN dStartDate = (IF dStartDate = ? THEN TODAY ELSE dStartDate).
      
      IF cAuthTypeCode <> "":U AND cMemberNumber <> "":U
      THEN DO:
        /*
          Find current option if blank
        */
        IF iOptionCode = 0 
        THEN 
          mipEnv:Health:maMember:getMemberOption
            ( INPUT  cMemberNumber,
              INPUT  dStartDate,
              OUTPUT iOptionCode ).

        FOR FIRST hac_auth_type NO-LOCK
            WHERE hac_auth_type.auth_type = cAuthTypeCode:

          mipEnv:Health:AuthBusinessLogic:AuthUpdatesAllowed
            ( INPUT  dAuthObj,   
              INPUT  hac_auth_type.auth_type_obj,
              INPUT  dInsurerObj,
              INPUT  iOptionCode,
              INPUT  dStartDate, 
              OUTPUT lUpdatesAllowed,
              OUTPUT cErrorOrWarning ).

          DO iReturnField = 1 TO iNumReturnFields:
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[AuthTypeObj]":U
              THEN DO:
                IF lUpdatesAllowed 
                THEN
                  ASSIGN dAuthTypeObj = hac_auth_type.auth_type_obj.
                ELSE
                  ASSIGN dAuthTypeObj = 0.00.

                {&AppendReturnValues} STRING(dAuthTypeObj).
              END. /* WHEN "[AuthTypeObj]":U */

              WHEN "[AuthTypeCode]":U
              THEN DO:
                IF lUpdatesAllowed
                THEN DO:
                  IF hac_auth_type.auth_type_group <> cAuthTypeGroup 
                  THEN
                    ASSIGN
                      ttValidation.lValid   = FALSE
                      ttValidation.cMessage = SUBSTITUTE("The Authorisation type &1 is not valid for the Authorisation Type Group selected", cAuthTypeCode).
                  ELSE  
                     ASSIGN cAuthTypeCode = hac_auth_type.auth_type.
                 
                END.
                ELSE // IF NOT lUpdatesAllowed THEN
                  ASSIGN cAuthTypeCode = "":U.

               {&AppendReturnValues} cAuthTypeCode.
              END. /* WHEN "[AuthTypeCode]":U */

              WHEN "[AuthTypeDesc]":U
              THEN DO:
                IF lUpdatesAllowed
                THEN
                  ASSIGN cAuthTypeDesc = hac_auth_type.description.
                ELSE
                  ASSIGN cAuthTypeDesc = "":U.

               {&AppendReturnValues} cAuthTypeDesc.
              END. /* WHEN "[AuthTypeDesc]":U */

              WHEN "[AuthTypeGroup]":U 
              THEN
                {&AppendReturnValues} cAuthTypeDesc.
              
              WHEN "[ValidateAuthTypeUsage]":U
              THEN DO:
                IF iDependant <> 99 
                THEN DO:
                  mipEnv:Health:AuthBusinessLogic:validateAuthTypeUsage
                    ( INPUT  hac_auth_type.auth_type_obj,
                      INPUT  dInsurerObj,
                      INPUT  iOptionCode,
                      INPUT  cMemberNumber,
                      INPUT  iDependant, 
                      INPUT  dStartDate,
                      INPUT  "Both":U, 
                      INPUT  "":U,
                      OUTPUT lValid,
                      OUTPUT cErrorOrWarning ).
                
                  {&AppendReturnValues} (IF NOT lValid THEN "error":U ELSE "warning":U) + ":":U + cErrorOrWarning.
                END. /* IF iDependant <> 99 */
              END. /* WHEN "[ValidateAuthTypeUsage]":U */

              WHEN "[ValidateAuthUpdatesAllowed]":U
              THEN DO:
                IF hac_auth_type.auth_type_obj <> 0.00
                THEN DO:
                  IF lUpdatesAllowed = NO
                  THEN
                    ASSIGN
                      cErrorOrWarning = "error:":U + SUBSTITUTE("The Authorisation type &1 - &2 is set up as no Updates Allowed, so it may not be used",
                                                              hac_auth_type.auth_type,
                                                              hac_auth_type.description).
                  ELSE
                    ASSIGN
                      cErrorOrWarning = "":U.
                END. /* IF hac_auth_type.auth_type_obj <> 0.00 */

                {&AppendReturnValues} cErrorOrWarning.
              END. /* WHEN "[ValidateAuthUpdatesAllowed]":U */

              WHEN "[OptionCode]":U
              THEN
                {&AppendReturnValues} STRING(iOptionCode).

              WHEN "[OptionDisplay]":U
              THEN DO:
                ASSIGN gcOption = STRING(iOptionCode,{ma/cus/maschopt9.i}).
                
                FIND FIRST scheme NO-LOCK                                                                                                                           
                     WHERE scheme.scheme-code = iOptionCode 
                  NO-ERROR.                      
                { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
                
                ASSIGN gcOption = gcOption + IF AVAILABLE scheme THEN " - " + scheme.short-name ELSE "".                                                                                                                               
                
                {&AppendReturnValues} gcOption.                 
              END. /* WHEN "[OptionDisplay]":U */

              WHEN "[Configuration]":U
              THEN DO:
                RUN _getConfiguration IN TARGET-PROCEDURE
                      ( INPUT  hac_auth_type.auth_type_obj, 
                        INPUT  dInsurerObj,
                        INPUT  iOptionCode,
                        INPUT  dStartDate,
                        OUTPUT lcConfiguration ).

                {&AppendReturnValues} STRING(lcConfiguration).
              END. /* WHEN "[Configuration]":U */

            END CASE. /* cReturnField */
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        

          {&ValidationSuccess}
        END. /* FOR FIRST hac_auth_type NO-LOCK */

        IF CAN-FIND(FIRST hac_auth_type NO-LOCK
                    WHERE hac_auth_type.auth_type = cAuthTypeCode)
        AND lUpdatesAllowed = FALSE
        THEN DO:
          ASSIGN
            ttValidation.lValid   = FALSE
            ttValidation.cMessage = SUBSTITUTE("The Authorisation type &1 is set up as no Updates Allowed, so it may not be used", cAuthTypeCode).

          VALIDATE ttValidation.
        END. /* IF CAN-FIND(FIRST hac_auth_type...) AND lUpdatesAllowed = FALSE */
        ELSE
          IF NOT CAN-FIND(FIRST hac_auth_type NO-LOCK
                          WHERE hac_auth_type.auth_type = cAuthTypeCode)
          THEN DO:
            ASSIGN
              ttValidation.lValid   = FALSE
              ttValidation.cMessage = SUBSTITUTE("Authorisation type '&1' not found.":U, cAuthTypeCode).

            VALIDATE ttValidation.
          END. /* IF NOT CAN-FIND(FIRST hac_auth_type) */
       
      END. /* IF cAuthTypeCode <> "":U AND cMemberNumber <> "":U */
      ELSE DO:
        {&BlankResponse}
      END. /* ELSE of IF cAuthTypeCode <> "":U AND cMemberNumber <> "":U */
    END. /* WHEN "AuthType":U */

    WHEN "ClaimCode":U THEN
    DO:
      IF iClaimCode <> 0 THEN 
      DO:
        IF dAuthObj > 0 AND dAuthObj <> ? THEN
        DO:
          ASSIGN oAuthorisation = NEW cls.maauthorisation(dAuthObj).
          
          IF oAuthorisation:InFocus 
          THEN
            ASSIGN 
              dInsurerObj   = oAuthorisation:InsurerObj
              iOptionCode   = oAuthorisation:MemberOptionCode
              cMemberNumber = oAuthorisation:MemNum
              iDependant    = oAuthorisation:Dependant
              dAuthTypeObj  = oAuthorisation:AuthTypeObj .
            
        END. /* IF dAuthObj > 0 AND dAuthObj <> ?  */
      
        FIND FIRST ccode NO-LOCK
             WHERE ccode.claim-code = iClaimCode
               AND ccode.scheme-code = iOptionCode
          NO-ERROR.
          
        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
        
        IF AVAILABLE ccode THEN
        DO:
          mipEnv:Health:AuthBusinessLogic:validateClaimCode
            (INPUT  dInsurerObj,
             INPUT  iOptionCode,
             INPUT  cMemberNumber,
             INPUT  iDependant,
             INPUT  iClaimCode,
             INPUT  dAuthTypeObj,
             INPUT  dStartDate,
             INPUT  "",          /* Provider Type */
             INPUT  0,           /* Discipline */
             INPUT  0,           /* Sub-Discipline */
             INPUT  0,           /* Negotiation Number */
             INPUT "hat_auth":U, /* Auth level     */
             OUTPUT lValid,
             OUTPUT cErrorOrWarning).
       
       FIND FIRST schext NO-LOCK 
            WHERE schext.scheme-code = iOptionCode NO-ERROR.
        
       { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE}
       
       
       ASSIGN iAuthStatus  = IF AVAILABLE schext AND schext.claim-code[1] = iClaimCode 
                             THEN 6
                             ELSE 1
              lSuccess     = mipEnv:Health:AuthMaintenance:getAuthRuleValue
                                                    (INPUT  dInsurerObj,
                                                     INPUT  iOptionCode,
                                                     INPUT  "ma_acAuthRuleTypeAuthStatus":U,
                                                     INPUT  "NilPaymentDefaultReason":U,
                                                     INPUT  dStartDate,
                                                     OUTPUT lValidRule,
                                                     OUTPUT cRuleValue)
                                                     
              lMandatory   = mipEnv:Health:AuthService:statusReasonMandatory(INPUT  iAuthStatus,
                                                                             INPUT  dInsurerObj,
                                                                             INPUT  iOptionCode,
                                                                             INPUT  dStartDate).


         IF INDEX(cErrorOrWarning , "[HELP=":U ) > 0
         THEN                                                                                                                      
           ASSIGN   cStringWithHelp   = SUBSTRING(cErrorOrWarning , INDEX(cErrorOrWarning , "[HELP=":U ) ) //extract string containing the help(and potentially other text) 
                    iEndOfHelpIndex   = INDEX(cStringWithHelp , "]":U )                                    //determine end point of help string which will be indicated by the "]"
                                                                                                           
                    cHelpString       = SUBSTRING(cStringWithHelp , 1 , iEndOfHelpIndex )                  //extract the help string
                    cHelpMessage      = REPLACE(cHelpString, "HELP=":U , "":U )                            //remove leading "HELP=" from string         

                    cErrorOrWarning   = REPLACE(cErrorOrWarning , cHelpString , "":U ).                    //wipe help string from original text 
                    

          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).
            
            CASE cReturnField:
            
              WHEN "[ValidateClaimCode]":U THEN
              DO:
                ASSIGN cErrorOrWarning = (IF NOT lValid THEN "error:":U ELSE "warning:":U) + cErrorOrWarning + ":":U .
                
                {&AppendReturnValues} cErrorOrWarning.
              END. /*WHEN "[ClaimCodeWarning]":U THEN*/  
              WHEN "[HelpMessage]" THEN
              DO:
                 {&AppendReturnValues} cHelpMessage.
              END.
              WHEN "[StatusAction]":U THEN
              DO:

                  ASSIGN cStatusAction = STRING(iAuthStatus)                + ",":U    //Status code
                                       + (IF lMandatory THEN "MANDATORY":U             
                                                        ELSE "OPTIONAL":U ) + ",":U    //Reason required 
                                       + (IF  lValidRule                               
                                          AND cRuleValue <> "":U                       
                                          AND iAuthStatus = 6  THEN cRuleValue         
                                                               ELSE "":U ).            //Default reason code  
                 {&AppendReturnValues} cStatusAction.
              END. /* WHEN "[StatusAction]":U */
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/        
          
          {&ValidationSuccess}
        END. /*IF AVAILABLE ccode THEN*/
        ELSE
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = SUBSTITUTE("Claim code '&1' not found for option &2":U, STRING(iClaimCode), STRING(iOptionCode)).
                 
          VALIDATE ttValidation.       
        END. /*ELSE*/
      END. /*IF cClaimCode <> "":U THEN */
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END. /*WHEN "ClaimCode":U THEN*/
    
    WHEN "Status":U THEN
    DO:
      
      DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):

        ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

        CASE cReturnField:

          WHEN "[StatusReasonMandatory]":U THEN
          DO:
            ASSIGN lMandatory = mipEnv:Health:AuthService:statusReasonMandatory(INPUT iStatusCode, INPUT dInsurerObj, INPUT iOptionCode, INPUT dStartDate).

            {&AppendReturnValues} STRING(lMandatory). 
          END. /*WHEN "[StatusReasonMandatory]":U THEN*/
        END CASE.
      END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   

      {&ValidationSuccess}       
    END. /*WHEN "Status":U THEN*/

    WHEN "Reason":U THEN
    DO:
      IF cReasonCode <> "":U THEN
      DO:
        /* 
          Reason type will be determined in gesStatusReasonDesc
        */
        ASSIGN cReasonType = "":U .
        
        mipEnv:health:AuthService:getStatusReasonDesc(INPUT 0.0,                 
                                                      INPUT 0,                   
                                                      INPUT dStartDate,          
                                                      INPUT iStatusCode,    
                                                      INPUT cReasonCode ,         
                                                      INPUT-OUTPUT cReasonType,        
                                                      OUTPUT cReasonDescription ,  
                                                      OUTPUT cErrorMessage ).

        
        IF cErrorMessage = "":U THEN 
        DO:
          DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):
          
            ASSIGN cReturnField = ENTRY(iReturnField, cReturnFieldList).

            CASE cReturnField:

              WHEN "[ReasonCode]":U THEN
              DO:
                {&AppendReturnValues} cReasonCode. 
              END. /*WHEN "[ReasonCode]":U THEN*/

              WHEN "[ReasonDesc]":U THEN
              DO:
                {&AppendReturnValues} cReasonDescription. 
              END. /*WHEN "[ReasonDesc]":U THEN*/
              WHEN "[ReasonType]":U  THEN
              DO:
                {&AppendReturnValues} cReasonType. 
              END. /* WHEN [ReasonType] */
            END CASE.                                                                      
          END. /*DO iReturnField = 1 TO NUM-ENTRIES(cReturnFieldList):*/   

          ASSIGN ttValidation.cReturnValues = cReturnValues 
                 ttValidation.lValid        = TRUE          
                 ttValidation.cMessage      = cReasonDescription.  

        END. /*IF AVAILABLE notedet THEN */
        ELSE
        DO:
          ASSIGN ttValidation.lValid   = FALSE
                 ttValidation.cMessage = cErrorMessage.
                 
          VALIDATE ttValidation.
        END. /*ELSE*/      
      END. /*IF cReasonCode <> "":U THEN*/
      ELSE
      DO:      
        {&BlankResponse}       
      END. /*ELSE*/
    END. /*WHEN "Reason":U THEN*/
    
    
  END CASE. /* CASE ipcValidationArgument: */
  
  { mip/inc/mipcatcherror.i 
    &FINALLY = "DATASET dsAuthType:EMPTY-DATASET().
    
                IF VALID-OBJECT(oAuthorisation) THEN DELETE OBJECT oAuthorisation  NO-ERROR. ~{mip/inc/mipmessageerror.i~} 
                EMPTY TEMP-TABLE ttAuthTypeConfig." }



