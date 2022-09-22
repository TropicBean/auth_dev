/*------------------------------------------------------------------------------
  maauthservauthlimcntrl.i.i  MEDSTAR Medical Aid System
                              (c) Copyright 2019 - 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved 
                        
  Purpose   : This procedure will ensure that the provider and detail quantity/amounts fall within the global
              limit of the auth header amount/quantity. 
              
              It will also ensure that the detail amount/quantity falls within the limit of the related provider amount/quantity              
              
              It can be called from Header, provider and detail level.
              
              AuthGlobalLimit and AuthProviderLimit will determine if the user will be notified via error, warning or warn acknowledgment
              when an amount/quantity exceeds a limit 
              
  Parameters: DATASET dsAuthorisation {ma/inc/maauthds.i}
              ipdAuthObj      
              ipcAuthLevel    - Auth level to be validated. Valid values:
                                "hatau" - Header 
                                "hatap" - Provider
                                "hatad" - Detail 
              ipdAuthLevelObj - Obj related to the level specified above
                                 when "hatap" - auth provider obj
                                 when "hatad" - auth detail obj
                                 
              ipdAmount       - Amount of the auth level to be validated 
              ipdQuantity     - Quantity of the auth level to be validated 
              
  Notes     : Errors will be returned in the tt_auth_error temp-table , which 
              is included in the dsAuthorisation 
------------------------------------------------------------------------------*/
  
  
  DEFINE INPUT-OUTPUT PARAMETER DATASET          FOR dsAuthorisation .
  DEFINE INPUT        PARAMETER ipdAuthObj       AS DECIMAL   NO-UNDO.
  DEFINE INPUT        PARAMETER ipcAuthLevel     AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER ipdAuthLevelObj  AS DECIMAL   NO-UNDO.  
  DEFINE INPUT        PARAMETER ipdAmount        AS DECIMAL   NO-UNDO.
  DEFINE INPUT        PARAMETER ipdQuantity      AS DECIMAL   NO-UNDO.

  DEFINE BUFFER btt_auth          FOR tt_auth .
  DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
  DEFINE BUFFER btt_auth_detail   FOR tt_auth_detail.

  DEFINE VARIABLE cMessage           AS CHARACTER         NO-UNDO .
  DEFINE VARIABLE cMessageType       AS CHARACTER         NO-UNDO .
  DEFINE VARIABLE cRuleValueGlobal   AS CHARACTER         NO-UNDO .
  DEFINE VARIABLE cRuleValueProvider AS CHARACTER         NO-UNDO .
  DEFINE VARIABLE dAuthProviderObj   AS DECIMAL           NO-UNDO .
  DEFINE VARIABLE dAuthDetailObj     AS DECIMAL           NO-UNDO .
  DEFINE VARIABLE dInsurerObj        AS DECIMAL           NO-UNDO .
  DEFINE VARIABLE iOptionCode        AS INTEGER           NO-UNDO .
  DEFINE VARIABLE lAcknowledge       AS LOGICAL           NO-UNDO .
  DEFINE VARIABLE lValidRuleGlobal   AS LOGICAL           NO-UNDO .
  DEFINE VARIABLE lValidRuleProvider AS LOGICAL           NO-UNDO .
  DEFINE VARIABLE lSuccess           AS LOGICAL           NO-UNDO .
  DEFINE VARIABLE dStartDate         AS DATE              NO-UNDO . 
  DEFINE VARIABLE oErrorObject       AS cls.maerrorobject NO-UNDO .

  //Sanitize inputs 
  ASSIGN ipdAmount            =  IF ipdAmount           = ? THEN 0       ELSE ipdAmount    
         ipdQuantity          =  IF ipdQuantity         = ? THEN 0       ELSE ipdQuantity  
         ipcAuthLevel         =  IF ipcAuthLevel        = ? THEN "":U    ELSE ipcAuthLevel   
         ipdAuthObj           =  IF ipdAuthObj          = ? THEN 0       ELSE ipdAuthObj   
         ipdAuthLevelObj      =  IF ipdAuthLevelObj     = ? THEN 0       ELSE ipdAuthLevelObj  

         dAuthProviderObj     =  IF ipcAuthLevel = "hatap":U THEN ipdAuthLevelObj  ELSE 0
         dAuthDetailObj       =  IF ipcAuthLevel = "hatad":U THEN ipdAuthLevelObj  ELSE 0

         oErrorObject         = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).
 
         
    FIND FIRST btt_auth 
         WHERE btt_auth.auth_obj = ipdAuthObj NO-ERROR.
  
    {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
  
    IF NOT AVAILABLE btt_auth THEN
    DO:
  
      ASSIGN cMessage = SUBSTITUTE("The Auth Obj(&1) is invalid." , STRING(ipdAuthObj )) 
      
             lSuccess =  oErrorObject:addError(INPUT 'hatau',            // ipcOwningEntityMnemonic
                                               INPUT ipdAuthObj,         // ipdOwningEntityObj
                                               INPUT "":U,               // ipcOwningEntityKey
                                               INPUT 99,                 // ipiLineNumber
                                               INPUT cMessage,           // ipcMessageText
                                               INPUT "ERR":U) NO-ERROR.  // ipcMessageType



      RETURN.
    END. //IF NOT AVAILABLE btt_auth THEN 
  
  ASSIGN dInsurerObj = btt_auth.insurer_obj 
         iOptionCode = btt_auth.option_code
         dStartDate  = btt_auth.start_date .


  //Find global auth limit rule
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj ,
                                                 INPUT  iOptionCode ,
                                                 INPUT  "ma_acAuthRuleTypeLimits":U,
                                                 INPUT  "AuthGlobalLimit":U,
                                                 INPUT  dStartDate ,
                                                 OUTPUT lValidRuleGlobal,
                                                 OUTPUT cRuleValueGlobal).


  //Find provider limit rule
  mipEnv:Health:AuthMaintenance:getAuthRuleValue(INPUT  dInsurerObj ,
                                                 INPUT  iOptionCode ,
                                                 INPUT  "ma_acAuthRuleTypeLimits":U,
                                                 INPUT  "AuthProviderLimit":U,
                                                 INPUT  dStartDate ,
                                                 OUTPUT lValidRuleProvider,
                                                 OUTPUT cRuleValueProvider).

  IF (cRuleValueGlobal = ""
  OR LOOKUP(cRuleValueGlobal , "Block,Warn,Warnack":U) = 0 )
  AND lValidRuleGlobal THEN
  DO:

    ASSIGN cMessage = "Global Limit controls can not be applied. AuthGlobalLimit rule is not set up correctly.":U
                    + "[HELP=Auth Rule Code: AuthGlobalLimit]":U 
           lSuccess =  oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                             INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                             INPUT "":U,                 // ipcOwningEntityKey
                                             INPUT btt_auth.line_number, // ipiLineNumber
                                             INPUT cMessage,             // ipcMessageText
                                             INPUT "ERR":U) NO-ERROR.    // ipcMessageType

           
    RETURN.
    
  END. //IF cRuleValueGlobal = "" OR NOT lValidRuleGlobal THEN

  IF (cRuleValueProvider = "" 
  OR  LOOKUP(cRuleValueProvider , "Block,Warn,Warnack":U) = 0 )
  AND  lValidRuleProvider THEN
  DO:

    ASSIGN cMessage = "Provider Limit controls can not be applied. AuthProviderLimit rule is not set up correctly.":U
                    + "[HELP=Auth Rule Code: AuthProviderLimit]":U 
           
           lSuccess =  oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                             INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                             INPUT "":U,                 // ipcOwningEntityKey
                                             INPUT btt_auth.line_number, // ipiLineNumber
                                             INPUT cMessage,             // ipcMessageText
                                             INPUT "ERR":U) NO-ERROR.    // ipcMessageType
           
    RETURN.
    
  END. //IF cRuleValueGlobal = "" OR NOT lValidRuleGlobal THEN

  IF  NOT lValidRuleProvider 
  AND NOT lValidRuleGlobal
  THEN 
    RETURN.

  CASE ipcAuthLevel:
    WHEN "hatau":U  THEN 
    DO:
      // If the amount and quantity on header level is 0 , no validation will be neccesary 
      IF  ipdAmount   = 0 
      AND ipdQuantity = 0 
      THEN  
        LEAVE .

      ASSIGN cMessageType = IF cRuleValueGlobal = "Block":U
                            THEN "ERR":U
                            ELSE "WAR":U
             lAcknowledge = cRuleValueGlobal = "WarnAck" .

      PROVIDER-BLK:
      FOR EACH btt_auth_provider 
         WHERE btt_auth_provider.auth_obj           = btt_auth.auth_obj 
           AND btt_auth_provider.authorised_service = TRUE 
           AND btt_auth_provider.auth_status        = 1 :

        IF  btt_auth_provider.amount_auth > ipdAmount 
        AND ipdAmount > 0 
        THEN 
          ASSIGN cMessage  = SUBSTITUTE("Auth Provider &1 Amount(&2) exceeds header Amount(&3) &4":U ,
                                        STRING(btt_auth_provider.doc_num),
                                        STRING(btt_auth_provider.amount_auth),
                                        STRING(ipdAmount) ,
                                        "[HELP=Auth Rule Code: AuthGlobalLimit]":U )

                 lSuccess  = oErrorObject:addError(INPUT "hatau":U,             // ipcOwningEntityMnemonic
                                                   INPUT btt_auth.auth_obj,     // ipdOwningEntityObj
                                                   INPUT "":U,                  // ipcOwningEntityKey
                                                   INPUT btt_auth.line_number,  // ipiLineNumber
                                                   INPUT cMessage,              // ipcMessageText
                                                   INPUT cMessageType,          // ipcMessageType
                                                   INPUT lAcknowledge).         // iplAcknowledge

        IF  btt_auth_provider.quantity_auth  > ipdQuantity 
        AND ipdQuantity > 0 
        THEN 
        ASSIGN cMessage       = SUBSTITUTE("Auth Provider &1 Quantity(&2) exceeds header Quantity(&3) &4":U ,
                                           STRING(btt_auth_provider.doc_num),
                                           STRING(btt_auth_provider.quantity_auth),
                                           STRING(ipdAmount),
                                           "[HELP=Auth Rule Code: AuthGlobalLimit]":U )

               lSuccess  = oErrorObject:addError(INPUT "hatau":U,             // ipcOwningEntityMnemonic
                                                 INPUT btt_auth.auth_obj,     // ipdOwningEntityObj
                                                 INPUT "":U,                  // ipcOwningEntityKey
                                                 INPUT btt_auth.line_number,  // ipiLineNumber
                                                 INPUT cMessage,              // ipcMessageText
                                                 INPUT cMessageType,          // ipcMessageType
                                                 INPUT lAcknowledge).         // iplAcknowledge                 
                                          
      END. //PROVIDER-BLK:
      
      DETAIL-BLK:
      FOR EACH btt_auth_detail 
         WHERE btt_auth_detail.auth_obj    = btt_auth.auth_obj 
           AND btt_auth_detail.auth_status = 1 :

        IF  btt_auth_detail.amount_auth > ipdAmount 
        AND ipdAmount > 0 
        THEN 
          ASSIGN cMessage   = SUBSTITUTE("Auth Detail &1 Amount(&2) exceeds header Amount(&3) &4":U ,
                                         btt_auth_detail.owning_alt_value,
                                         STRING(btt_auth_detail.amount_auth),
                                         STRING(ipdAmount),
                                         "[HELP=Auth Rule Code: AuthGlobalLimit]":U )

                 lSuccess   = oErrorObject:addError(INPUT "hatau":U,             // ipcOwningEntityMnemonic
                                                    INPUT btt_auth.auth_obj,     // ipdOwningEntityObj
                                                    INPUT "":U,                  // ipcOwningEntityKey
                                                    INPUT "amount_auth":U,       // ipcFieldName
                                                    INPUT btt_auth.line_number,  // ipiLineNumber
                                                    INPUT cMessage,              // ipcMessageText
                                                    INPUT cMessageType,          // ipcMessageType
                                                    INPUT lAcknowledge).         // iplAcknowledge.
                                       

        IF  btt_auth_detail.quantity_auth  > ipdQuantity 
        AND ipdQuantity > 0 
        THEN 
        ASSIGN cMessage    = SUBSTITUTE("Auth Detail  &1 Quantity(&2) exceeds the header Quantity(&3) &4":U ,
                                       btt_auth_detail.owning_alt_value,
                                       STRING(btt_auth_detail.quantity_auth),
                                       STRING(ipdQuantity)  ,
                                       "[HELP=Auth Rule Code: AuthGlobalLimit]":U )

               lSuccess    = oErrorObject:addError(INPUT "hatau":U,            // ipcOwningEntityMnemonic
                                                   INPUT btt_auth.auth_obj,    // ipdOwningEntityObj
                                                   INPUT "":U,                 // ipcOwningEntityKey
                                                   INPUT "quantity_auth":U,    // ipcFieldName
                                                   INPUT btt_auth.line_number, // ipiLineNumber
                                                   INPUT cMessage,             // ipcMessageText
                                                   INPUT cMessageType,         // ipcMessageType
                                                   INPUT lAcknowledge).        // iplAcknowledge.             
    
      END. //DETAIL-BLK 

    END. //WHEN "header":U  THEN 
    WHEN "hatap":U  THEN 
    DO:


      FIND FIRST btt_auth_provider 
           WHERE btt_auth_provider.auth_obj          = btt_auth.auth_obj 
             AND btt_auth_provider.auth_provider_obj = dAuthProviderObj NO-ERROR.
        
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

      IF NOT AVAILABLE btt_auth_provider THEN 
      DO:
        ASSIGN cMessage = "Provider was not found with auth_provider_obj = " + STRING(dAuthProviderObj) 
               lSuccess =  oErrorObject:addError(INPUT 'hatau',            // ipcOwningEntityMnemonic
                                                 INPUT ipdAuthObj,         // ipdOwningEntityObj
                                                 INPUT "":U,               // ipcOwningEntityKey
                                                 INPUT 99,                 // ipiLineNumber
                                                 INPUT cMessage,           // ipcMessageText
                                                 INPUT "ERR":U).           // ipcMessageType 

        RETURN.
      END. //IF NOT AVAILABLE btt_auth_provider THEN 

      IF btt_auth_provider.auth_status <> 1 
      OR btt_auth_provider.authorised_service = FALSE 
      THEN 
        LEAVE . 

      ASSIGN cMessageType = IF cRuleValueGlobal = "Block":U
                            THEN "ERR":U
                            ELSE "WAR":U
             lAcknowledge = cRuleValueGlobal = "WarnAck" .

      IF  ipdAmount > btt_auth.amount_auth  
      AND btt_auth.amount_auth > 0 
      THEN 
        ASSIGN cMessage  = SUBSTITUTE("Auth Provider &1 Amount(&2) exceeds header Amount(&3) &4":U ,
                                      STRING(btt_auth_provider.doc_num),
                                      STRING(ipdAmount),
                                      STRING(btt_auth.amount_auth),
                                      "[HELP=Auth Rule Code: AuthGlobalLimit]":U)  
               
               lSuccess  = oErrorObject:addError(INPUT "hatap":U,                              // ipcOwningEntityMnemonic
                                                 INPUT btt_auth_provider.auth_provider_obj,    // ipdOwningEntityObj
                                                 INPUT "":U,                                   // ipcOwningEntityKey
                                                 INPUT "amount_auth":U,                        // ipcFieldName
                                                 INPUT btt_auth.line_number,                   // ipiLineNumber
                                                 INPUT cMessage,                               // ipcMessageText
                                                 INPUT cMessageType,                           // ipcMessageType
                                                 INPUT lAcknowledge).                          // iplAcknowledge
        
                

      IF  ipdQuantity > btt_auth.quantity_auth
      AND btt_auth.quantity_auth > 0 
      THEN 
      ASSIGN cMessage  = SUBSTITUTE("Auth Provider &1 Quantity(&2) exceeds the header Quantity(&3) &4":U ,
                                     STRING(btt_auth_provider.doc_num), 
                                     STRING(btt_auth_provider.quantity_auth),
                                     STRING(btt_auth.quantity_auth),
                                     "[HELP=Auth Rule Code: AuthGlobalLimit]":U)  

              lSuccess  = oErrorObject:addError(INPUT "hatap":U,                              // ipcOwningEntityMnemonic
                                                INPUT btt_auth_provider.auth_provider_obj,    // ipdOwningEntityObj
                                                INPUT "":U,                                   // ipcOwningEntityKey
                                                INPUT "quantity_auth":U,                      // ipcFieldName
                                                INPUT btt_auth.line_number,                   // ipiLineNumber
                                                INPUT cMessage,                               // ipcMessageText
                                                INPUT cMessageType,                           // ipcMessageType
                                                INPUT lAcknowledge).                          // iplAcknowledge.

    
      ASSIGN cMessageType = IF cRuleValueProvider = "Block":U
                            THEN "ERR":U
                            ELSE "WAR":U
             lAcknowledge = cRuleValueProvider = "WarnAck" .

      DETAIL-BLK:
      FOR EACH btt_auth_detail 
         WHERE btt_auth_detail.auth_obj    = btt_auth.auth_obj 
           AND btt_auth_detail.auth_status = 1 :
      

        IF  btt_auth_detail.amount_auth  > ipdAmount 
        AND ipdAmount > 0
        THEN 
          ASSIGN cMessage  = SUBSTITUTE("Auth Detail &1 Amount(&2) exceeds the related Provider &3 Amount(&4) &5":U ,
                                        btt_auth_detail.owning_alt_value,
                                        btt_auth_detail.amount_auth,
                                        STRING(btt_auth_provider.doc_num), 
                                        STRING(ipdAmount),
                                        "[HELP=Auth Rule Code: AuthProviderLimit]":U)  

                 lSuccess  = oErrorObject:addError(INPUT "hatap":U,                              // ipcOwningEntityMnemonic
                                                   INPUT btt_auth_provider.auth_provider_obj,    // ipdOwningEntityObj
                                                   INPUT "":U,                                   // ipcOwningEntityKey
                                                   INPUT "amount_auth":U,                        // ipcFieldName
                                                   INPUT btt_auth.line_number,                   // ipiLineNumber
                                                   INPUT cMessage,                               // ipcMessageText
                                                   INPUT cMessageType,                           // ipcMessageType
                                                   INPUT lAcknowledge).                          // iplAcknowledge.

        IF  btt_auth_detail.quantity_auth > ipdQuantity 
        AND ipdQuantity > 0
        THEN
          ASSIGN cMessage  = SUBSTITUTE("Auth Detail &1 Quantity(&2) exceeds the related provider &3 Quantity(&4) &5":U ,
                                        btt_auth_detail.owning_alt_value,
                                        btt_auth_detail.quantity_auth,
                                        STRING(btt_auth_provider.doc_num), 
                                        STRING(ipdQuantity),
                                        "[HELP=Auth Rule Code: AuthProviderLimit]":U)  
                                        
                 lSuccess  = oErrorObject:addError(INPUT "hatap":U,                            // ipcOwningEntityMnemonic
                                                   INPUT btt_auth_provider.auth_provider_obj,  // ipdOwningEntityObj
                                                   INPUT "":U,                                 // ipcOwningEntityKey
                                                   INPUT "quantity_auth":U,                    // ipcFieldName
                                                   INPUT btt_auth.line_number,                 // ipiLineNumber
                                                   INPUT cMessage,                             // ipcMessageText
                                                   INPUT cMessageType,                         // ipcMessageType
                                                   INPUT lAcknowledge).                        // iplAcknowledge.
      
      END. //DETAIL-BLK

    END. // WHEN "provider":U  THEN 
    WHEN "hatad":U  THEN 
    DO:

 
      FIND FIRST btt_auth_detail 
           WHERE btt_auth_detail.auth_detail_obj = dAuthDetailObj NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

      IF NOT AVAILABLE btt_auth_detail THEN
      DO:
        
        ASSIGN cMessage = "Detail was not found with auth_detail_obj = " + STRING(dAuthDetailObj) 
               lSuccess =  oErrorObject:addError(INPUT 'hatau',            // ipcOwningEntityMnemonic
                                                 INPUT ipdAuthObj,         // ipdOwningEntityObj
                                                 INPUT "":U,               // ipcOwningEntityKey
                                                 INPUT 99,                 // ipiLineNumber
                                                 INPUT cMessage,           // ipcMessageText
                                                 INPUT "ERR":U) .          // ipcMessageType 
        RETURN.                                              
      END. // //IF NOT AVAILABLE btt_auth_detail THEN

      // Only run the validations if we are dealing with a 
      IF btt_auth_detail.auth_status <> 1 
      THEN
        LEAVE .

      ASSIGN cMessageType = IF cRuleValueGlobal = "Block":U
                            THEN "ERR":U
                            ELSE "WAR":U
             lAcknowledge = cRuleValueGlobal = "WarnAck" .

      IF  ipdAmount > btt_auth.amount_auth
      AND btt_auth.amount_auth >  0 
      THEN
        ASSIGN cMessage  = SUBSTITUTE("Auth Detail &1 Amount(&2) exceeds the header Amount(&3) &4":U ,
                                      btt_auth_detail.owning_alt_value,
                                      ipdAmount,
                                      STRING(btt_auth.amount_auth), 
                                      "[HELP=Auth Rule Code: AuthGlobalLimit]":U)  
        
               lSuccess  = oErrorObject:addError(INPUT "hatad":U,                        // ipcOwningEntityMnemonic
                                                 INPUT btt_auth_detail.auth_detail_obj,  // ipdOwningEntityObj
                                                 INPUT "":U,                             // ipcOwningEntityKey
                                                 INPUT "amount_auth":U,                  // ipcFieldName
                                                 INPUT btt_auth.line_number,             // ipiLineNumber
                                                 INPUT cMessage,                         // ipcMessageText
                                                 INPUT cMessageType,                     // ipcMessageType
                                                 INPUT lAcknowledge).                    // iplAcknowledge
                                                 
      IF  ipdQuantity > btt_auth.quantity_auth
      AND btt_auth.quantity_auth >  0 
      THEN
        ASSIGN cMessage  = SUBSTITUTE("Auth Detail &1 Quantity(&2) exceeds the header Quantity(&3) &4":U ,
                                      btt_auth_detail.owning_alt_value,
                                      ipdQuantity,
                                      STRING(btt_auth.quantity_auth), 
                                      "[HELP=Auth Rule Code: AuthGlobalLimit]":U)  
        
               lSuccess  = oErrorObject:addError(INPUT "hatad":U,                        // ipcOwningEntityMnemonic
                                                 INPUT btt_auth_detail.auth_detail_obj,  // ipdOwningEntityObj
                                                 INPUT "":U,                             // ipcOwningEntityKey
                                                 INPUT "quantity_auth":U,                // ipcFieldName
                                                 INPUT btt_auth.line_number,             // ipiLineNumber
                                                 INPUT cMessage,                         // ipcMessageText
                                                 INPUT cMessageType,                     // ipcMessageType
                                                 INPUT lAcknowledge).                    // iplAcknowledge    
     
      ASSIGN cMessageType = IF cRuleValueProvider = "Block":U
                            THEN "ERR":U
                            ELSE "WAR":U
             lAcknowledge = cRuleValueProvider = "WarnAck" .

      FIND FIRST btt_auth_provider 
           WHERE btt_auth_provider.auth_obj           = btt_auth_detail.auth_obj 
             AND btt_auth_provider.auth_provider_obj  = btt_auth_detail.auth_provider_obj NO-ERROR .
      
      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE } 

      IF NOT AVAILABLE btt_auth_provider THEN
      DO:
        
        ASSIGN cMessage = "Provider was not found with auth_provider_obj = " + STRING(btt_auth_detail.auth_provider_obj) 
               lSuccess =  oErrorObject:addError(INPUT 'hatad',                            // ipcOwningEntityMnemonic
                                                 INPUT btt_auth_detail.auth_detail_obj,    // ipdOwningEntityObj
                                                 INPUT "":U,                               // ipcOwningEntityKey
                                                 INPUT btt_auth.line_number,               // ipiLineNumber
                                                 INPUT cMessage,                           // ipcMessageText
                                                 INPUT "ERR":U) .                          // ipcMessageType 

        RETURN .
      END. // IF NOT AVAILABLE btt_auth_provider THEN

      // Skip provider validations  if the provider is not authorised or authorised service
      IF btt_auth_provider.auth_status <> 1 
      OR btt_auth_provider.authorised_service = FALSE 
      THEN 
        LEAVE.

      IF  btt_auth_provider.amount_auth < ipdAmount
      AND btt_auth_provider.amount_auth > 0 
      THEN
          ASSIGN cMessage  = SUBSTITUTE("Auth Detail &1 Amount(&2) exceeds the related Provider &3 Amount(&4) &5":U ,
                                        btt_auth_detail.owning_alt_value,
                                        ipdAmount,
                                        STRING(btt_auth_provider.doc_num), 
                                        STRING(btt_auth_provider.amount_auth),
                                        "[HELP=Auth Rule Code: AuthProviderLimit]":U)  

                 lSuccess  = oErrorObject:addError(INPUT "hatad":U,                        // ipcOwningEntityMnemonic
                                                   INPUT btt_auth_detail.auth_detail_obj,  // ipdOwningEntityObj
                                                   INPUT "":U,                             // ipcOwningEntityKey
                                                   INPUT "amount_auth":U,                  // ipcFieldName
                                                   INPUT btt_auth.line_number,             // ipiLineNumber
                                                   INPUT cMessage,                         // ipcMessageText
                                                   INPUT cMessageType,                     // ipcMessageType
                                                   INPUT lAcknowledge).                    // iplAcknowledge
                                                   
                                                   
      IF  btt_auth_provider.quantity_auth < ipdQuantity
      AND btt_auth_provider.quantity_auth > 0 
      THEN 
        ASSIGN cMessage  = SUBSTITUTE("Auth Detail &1 Quantity(&2) exceeds the related Provider &3 Quantity(&4) &5":U ,
                                      btt_auth_detail.owning_alt_value,
                                      ipdQuantity,
                                      STRING(btt_auth_provider.doc_num), 
                                      STRING(btt_auth_provider.quantity_auth),
                                      "[HELP=Auth Rule Code: AuthProviderLimit]":U)  

               lSuccess  = oErrorObject:addError(INPUT "hatad":U,                        // ipcOwningEntityMnemonic
                                                 INPUT btt_auth_detail.auth_detail_obj,  // ipdOwningEntityObj
                                                 INPUT "":U,                             // ipcOwningEntityKey
                                                 INPUT "quantity_auth":U,                // ipcFieldName
                                                 INPUT btt_auth.line_number,             // ipiLineNumber
                                                 INPUT cMessage,                         // ipcMessageText
                                                 INPUT cMessageType,                     // ipcMessageType
                                                 INPUT lAcknowledge).                    // iplAcknowledge.
                                              
    END. // WHEN "detail":U  THEN 
    OTHERWISE 
    DO:
      ASSIGN cMessage = "applyAuthLimitControls does not support ipcAuthLevel = " + ipcAuthLevel 
                      + ".Valid values are: hatau,hatap,hatad"
             lSuccess =  oErrorObject:addError(INPUT 'hatau',              // ipcOwningEntityMnemonic
                                               INPUT ipdAuthObj,           // ipdOwningEntityObj
                                               INPUT "":U,                 // ipcOwningEntityKey
                                               INPUT 99,                   // ipiLineNumber
                                               INPUT cMessage,             // ipcMessageText
                                               INPUT "ERR":U) .            // ipcMessageType 

    END. //OTHERWISE           
  END CASE. 
  

  {mip/inc/mipcatcherror.i 
   &FINALLY="IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject." }
