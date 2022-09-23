/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Andrewd

      Purpose: Base Authorisation Provider Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisationprovider INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  DEFINE PUBLIC PROPERTY InFocus                   AS LOGICAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthProviderObj           AS DECIMAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY LineNumber                AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AccountReference          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentAuth            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentPaid            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentPrivateAuth     AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentPrivatePaid     AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountAuth                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountClaimed             AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountInterim             AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountPaid                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountRequested           AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountTotal               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthCopayControlObj       AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthObj                   AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthGroupObj              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatus                AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatusNote            AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthoriseAllServices      AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ArsRate                   AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY BaseRate                  AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Benefit                   AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCode                 AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimType                 AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayAuth%                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayAuth                 AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayOverrideNote         AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayPaid                 AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayProvider             AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultBaseRate           AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultArsRate            AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimCode          AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimType          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountAuth              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountPaid              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountType              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DocNum                    AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EmergencyFlagValue        AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EmergencyFlagValueUpdated AS LOGICAL           NO-UNDO GET. SET.    
  DEFINE PUBLIC PROPERTY EndAmPm                   AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                   AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY GroupDocNum               AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LosCalculation            AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MainProvider              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OverrideBaseRate          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OverrideArsRate           AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PayeeDm                   AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PenaltyFlag               AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PenaltyFlagUpdated        AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PenaltyOverrideNote       AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbBenefit                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbIndicator              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbPayCost                AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbValue                  AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PrintDm                   AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ProviderSequence          AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ProviderType              AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PrType                    AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityAuth              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityClaimed           AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityPaid              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityRequested         AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthorisedService         AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RateChangeType            AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartAmPm                 AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartDate                 AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY SubPrType                 AS INTEGER           NO-UNDO GET. SET.  
                                                   
  DEFINE PUBLIC PROPERTY ErrorObject               AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.


  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }
  

  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationprovider() :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationprovider */

                                                           
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationprovider
    ( INPUT ipdAuthProviderObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthProviderObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationprovider */
  
                                                           
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisationprovider() :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisationprovider */

  
  /* -- METHOD focusRecord --------------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthProviderObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
   
    ASSIGN InFocus         = NOT unfocusRecord()
           
           AuthProviderObj = ipdAuthProviderObj
           
           InFocus         = getRecordDetails().
           
    RETURN InFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusRecord  */
  

  /* -- METHOD getRecordDetails ---------------------------------------- */
  METHOD PRIVATE LOGICAL getRecordDetails
    (  ) :
  /*--------------------------------------------------------------------
        Purpose: Internal method for retrieveing authorisation data
                 using the authorisation data retrieval services.
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearch AS cls.maauthsearch NO-UNDO.
    
                                           
    DATASET dsAuthorisation:EMPTY-DATASET().
    
    
    IF AuthProviderObj <> 0.00 THEN
    DO:
      oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_provider":U)
      
             glSuccess = oSearch:SetFilterCriteria("tt_auth_provider.auth_provider_obj":U, "=":U, AuthProviderObj).
             
      oSearch:fetchData().             
      
    END. /*IF AuthProviderObj <> 0.00 THEN*/
    
  
    FOR FIRST tt_auth_provider NO-LOCK
        WHERE tt_auth_provider.auth_provider_obj = AuthProviderObj:
    
      ASSIGN
         AccountReference          = tt_auth_provider.account_reference
         AdjustmentAuth            = tt_auth_provider.adjustment_auth
         AdjustmentPaid            = tt_auth_provider.adjustment_paid
         AdjustmentPrivateAuth     = tt_auth_provider.adjustment_private_auth 
         AdjustmentPrivatePaid     = tt_auth_provider.adjustment_private_paid 
         AmountAuth                = tt_auth_provider.amount_auth             
         AmountClaimed             = tt_auth_provider.amount_claimed
         AmountInterim             = tt_auth_provider.amount_interim
         AmountPaid                = tt_auth_provider.amount_paid
         AmountRequested           = tt_auth_provider.amount_requested
         AmountTotal               = tt_auth_provider.amount_total
         AuthCopayControlObj       = tt_auth_provider.auth_copay_control_obj
         AuthObj                   = tt_auth_provider.auth_obj
&IF {&DBDFMA} >= 010195 &THEN      
         AuthGroupObj              = tt_auth_provider.auth_group_obj
         AuthoriseAllServices      = tt_auth_provider.authorise_all_services
&ENDIF                             
         AuthProviderObj           = tt_auth_provider.auth_provider_obj
         AuthStatus                = tt_auth_provider.auth_status
         AuthStatusNote            = tt_auth_provider.auth_status_note  
         Benefit                   = tt_auth_provider.benefit_%
         ClaimCode                 = tt_auth_provider.claim_code
         ClaimType                 = tt_auth_provider.claim_type
         CopayAuth%                = tt_auth_provider.copay_auth_%
         CopayAuth                 = tt_auth_provider.copay_auth
         CopayOverrideNote         = tt_auth_provider.copay_override_note
         CopayPaid                 = tt_auth_provider.copay_paid
         CopayProvider             = tt_auth_provider.copay_provider
         DefaultArsRate            = tt_auth_provider.default_ars_rate
         DefaultBaseRate           = tt_auth_provider.default_base_rate
         DefaultClaimCode          = tt_auth_provider.default_claim_code
         DefaultClaimType          = tt_auth_provider.default_claim_type
         DiscountAuth              = tt_auth_provider.discount_auth
         DiscountPaid              = tt_auth_provider.discount_paid
         DiscountType              = tt_auth_provider.discount_type
         DocNum                    = tt_auth_provider.doc_num
         EmergencyFlagValue        = tt_auth_provider._emergency_flag
         EmergencyFlagValueUpdated = tt_auth_provider._emergency_flag_updated
         EndAmPm                   = tt_auth_provider.end_ampm
         EndDate                   = tt_auth_provider.end_date
         GroupDocNum               = tt_auth_provider.group_doc_num
         LosCalculation            = tt_auth_provider.los_calculation
         MainProvider              = tt_auth_provider.main_provider
         OverrideArsRate           = tt_auth_provider.override_ars_rate
         OverrideBaseRate          = tt_auth_provider.override_base_rate
         PayeeDm                   = tt_auth_provider.payee_dm
         PenaltyFlag               = tt_auth_provider._penalty_flag
         PenaltyFlagUpdated        = tt_auth_provider._penalty_flag_updated
         PenaltyOverrideNote       = tt_auth_provider._penalty_override_note
         PmbBenefit                = tt_auth_provider.pmb_benefit_%
         PmbIndicator              = tt_auth_provider.pmb_indicator
         PmbPayCost                = tt_auth_provider.pmb_pay_cost
         PmbValue                  = tt_auth_provider.pmb_value
         PrintDm                   = tt_auth_provider.print_dm
         ProviderSequence          = tt_auth_provider.provider_sequence
         ProviderType              = tt_auth_provider.provider_type
         PrType                    = tt_auth_provider.pr_type
         QuantityAuth              = tt_auth_provider.quantity_auth
         QuantityClaimed           = tt_auth_provider.quantity_claimed
         QuantityPaid              = tt_auth_provider.quantity_paid
         QuantityRequested         = tt_auth_provider.quantity_requested
         AuthorisedService         = tt_auth_provider.authorised_service
         RateChangeType            = tt_auth_provider.rate_change_type
         StartAmPm                 = tt_auth_provider.start_ampm
         StartDate                 = tt_auth_provider.start_date
         SubPrType                 = tt_auth_provider.sub_pr_type.
                   
    END. /*FOR FIRST tt_auth_provider NO-LOCK:*/


    RETURN CAN-FIND(FIRST tt_auth_provider NO-LOCK).


    { mip/inc/mipcatcherror.i 
      &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}

  END METHOD.  /* getRecordDetails  */


  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject
    (  ) :
  /*--------------------------------------------------------------------
        Purpose : Internal method for configuration required by the object.
          Notes :
  ---------------------------------------------------------------------- */

    ASSIGN ErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */
  
  
  /* -- METHOD deleteRecord -------------------------------------------- */
  METHOD PUBLIC LOGICAL deleteRecord
    (  ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE BUFFER btt_auth_provider FOR tt_auth_provider.
    
      
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation provider not in focus'" }
      
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth_provider EXCLUSIVE-LOCK
           WHERE btt_auth_provider.auth_provider_obj = AuthProviderObj.
           
      ASSIGN btt_auth_provider.record_action = "delete":U.     
    
    END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
    
&IF {&DBDFMA} >= 010195 &THEN        
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).
&ENDIF    
    
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* deleteRecord  */
                                    

  /* -- METHOD saveRecord ---------------------------------------------- */
  METHOD PUBLIC LOGICAL saveRecord
    () :
  /*--------------------------------------------------------------------
        Purpose : Create/Update authorisation record using the instance 
                  properties specified.   
        
          Notes :
  ---------------------------------------------------------------------- */
    
    DATASET dsAuthorisation:EMPTY-DATASET().
    
    
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      CREATE tt_auth_provider.
      
      ASSIGN
         tt_auth_provider.line_number              = LineNumber
         tt_auth_provider.record_action            = "Modify":U
         tt_auth_provider.account_reference        = AccountReference     
         tt_auth_provider.adjustment_auth          = AdjustmentAuth       
         tt_auth_provider.adjustment_paid          = AdjustmentPaid       
         tt_auth_provider.adjustment_private_auth  = AdjustmentPrivateAuth
         tt_auth_provider.adjustment_private_paid  = AdjustmentPrivatePaid
         tt_auth_provider.amount_auth              = AmountAuth           
         tt_auth_provider.amount_claimed           = AmountClaimed        
         tt_auth_provider.amount_interim           = AmountInterim        
         tt_auth_provider.amount_paid              = AmountPaid           
         tt_auth_provider.amount_requested         = AmountRequested      
         tt_auth_provider.amount_total             = AmountTotal  
         tt_auth_provider.auth_copay_control_obj   = AuthCopayControlObj              
         tt_auth_provider.auth_obj                 = AuthObj
&IF {&DBDFMA} >= 010195 &THEN         
         tt_auth_provider.auth_group_obj           = AuthGroupObj
         tt_auth_provider.authorise_all_services   = AuthoriseAllServices
&ENDIF
         tt_auth_provider.auth_provider_obj        = AuthProviderObj      
         tt_auth_provider.auth_status              = AuthStatus           
         tt_auth_provider.auth_status_note         = AuthStatusNote  
         tt_auth_provider.benefit_%                = Benefit                
         tt_auth_provider.claim_code               = ClaimCode            
         tt_auth_provider.claim_type               = ClaimType
         tt_auth_provider.copay_auth_%             = CopayAuth%
         tt_auth_provider.copay_auth               = CopayAuth
         tt_auth_provider.copay_override_note      = CopayOverrideNote                    
         tt_auth_provider.copay_paid               = CopayPaid 
         tt_auth_provider.copay_provider           = CopayProvider               
         tt_auth_provider.default_ars_rate         = DefaultArsRate 
         tt_auth_provider.default_base_rate        = DefaultBaseRate 
         tt_auth_provider.default_claim_code       = DefaultClaimCode
         tt_auth_provider.default_claim_type       = DefaultClaimType
         tt_auth_provider.discount_auth            = DiscountAuth         
         tt_auth_provider.discount_paid            = DiscountPaid         
         tt_auth_provider.discount_type            = DiscountType         
         tt_auth_provider.doc_num                  = DocNum               
         tt_auth_provider.end_ampm                 = EndAmPm              
         tt_auth_provider.end_date                 = EndDate       
         tt_auth_provider._emergency_flag          = EmergencyFlagValue       
         tt_auth_provider._emergency_flag_updated  = EmergencyFlagValueUpdated        
         tt_auth_provider.group_doc_num            = GroupDocNum 
         tt_auth_provider.los_calculation          = LosCalculation         
         tt_auth_provider.main_provider            = MainProvider   
         tt_auth_provider.override_ars_rate        = OverrideArsRate 
         tt_auth_provider.override_base_rate       = OverrideBaseRate   
         tt_auth_provider.payee_dm                 = PayeeDm   
         tt_auth_provider._penalty_flag            = PenaltyFlag        
         tt_auth_provider._penalty_flag_updated    = PenaltyFlagUpdated 
         tt_auth_provider._penalty_override_note   = PenaltyOverrideNote
         tt_auth_provider.pmb_benefit              = PmbBenefit           
         tt_auth_provider.pmb_indicator            = PmbIndicator         
         tt_auth_provider.pmb_pay_cost             = PmbPayCost           
         tt_auth_provider.pmb_value                = PmbValue             
         tt_auth_provider.print_dm                 = PrintDm              
         tt_auth_provider.provider_type            = ProviderType
         tt_auth_provider.pr_type                  = PrType
         tt_auth_provider.quantity_auth            = QuantityAuth 
         tt_auth_provider.quantity_claimed         = QuantityClaimed          
         tt_auth_provider.quantity_paid            = QuantityPaid         
         tt_auth_provider.quantity_requested       = QuantityRequested    
         tt_auth_provider.authorised_service       = AuthorisedService      
         tt_auth_provider.rate_change_type         = RateChangeType
         tt_auth_provider.start_ampm               = StartAmPm            
         tt_auth_provider.start_date               = StartDate            
         tt_auth_provider.sub_pr_type              = SubPrType
         tt_auth_provider.provider_sequence        = ProviderSequence.
      
      VALIDATE tt_auth_provider.                 
    
    END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
    
    
    EMPTY TEMP-TABLE tt_temp_error.
    
    
&IF {&DBDFMA} >= 010195 &THEN                     
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).
&ENDIF    
    
    
    /*
      When the record is re-focused the entire dataset is emptied, meaning all errors and warnings are cleared as well,
      but we would like to keep track of any warnings even though the record created successfully so that these warnings
      can be returned to the user, so we will temporarily keep track of these warnings and copy them back to the dataset.
    */
    IF ErrorObject:WarningsExist 
    THEN TEMP-TABLE tt_temp_error:COPY-TEMP-TABLE(TEMP-TABLE tt_auth_error:HANDLE).
    
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:

      IF AuthProviderObj <= 0.00
      THEN 
      FOR FIRST tt_auth_provider NO-LOCK
          WHERE tt_auth_provider.auth_obj      = AuthObj
            AND tt_auth_provider.group_doc_num = GroupDocNum
            AND tt_auth_provider.doc_num       = DocNum
            AND tt_auth_provider.pr_type       = PrType
            AND tt_auth_provider.sub_pr_type   = SubPrType
            AND tt_auth_provider.start_date    = StartDate:
        
        ASSIGN AuthProviderObj = tt_auth_provider.auth_provider_obj.
                       
      END. /*FOR FIRST tt_auth_provider NO-LOCK:*/

      ASSIGN glSuccess = focusRecord(AuthProviderObj).
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    
    /*
      Copy warnings back to the dataset error temp table
    */
    IF TEMP-TABLE tt_temp_error:HAS-RECORDS
    THEN TEMP-TABLE tt_auth_error:COPY-TEMP-TABLE(TEMP-TABLE tt_temp_error:HANDLE).
    
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* saveRecord  */
  
  
  /* -- METHOD unfocusRecord ------------------------------------------- */
  METHOD PUBLIC LOGICAL unfocusRecord
     () :
  /*--------------------------------------------------------------------
        Purpose:  
          Notes:
  ---------------------------------------------------------------------- */
    
    ErrorObject:ClearErrors().
    
    
    ASSIGN 
       LineNumber                = 0
       InFocus                   = FALSE
       AccountReference          = "":U
       AdjustmentAuth            = 0.00                  
       AdjustmentPaid            = 0.00 
       AdjustmentPrivateAuth     = 0.00  
       AdjustmentPrivatePaid     = 0.00                  
       AmountAuth                = 0.00
       AmountClaimed             = 0.00
       AmountInterim             = 0.00
       AmountPaid                = 0.00    
       AmountRequested           = 0.00
       AmountTotal               = 0.00
       AuthCopayControlObj       = 0.00
       AuthObj                   = 0.00
&IF {&DBDFMA} >= 010195 &THEN    
       AuthGroupObj              = 0.00
       AuthoriseAllServices      = FALSE 
&ENDIF                           
       AuthProviderObj           = 0.00
       AuthStatus                = 0         
       AuthStatusNote            = "":U 
       Benefit                   = 0.00      
       ClaimCode                 = 0
       ClaimType                 = "":U   
       CopayAuth%                = 0.00                           
       CopayAuth                 = 0.00
       CopayOverrideNote         = ""
       CopayPaid                 = 0.00
       CopayProvider             = ?
       DefaultArsRate            = "":U
       DefaultBaseRate           = "":U
       DefaultClaimCode          = 0
       DefaultClaimType          = "":U
       DiscountAuth              = 0.00
       DiscountPaid              = 0.00
       DiscountType              = ?
       DocNum                    = 0
       EndAmPm                   = FALSE                             
       EndDate                   = ?       
       EmergencyFlagValue        = "":U
       EmergencyFlagValueUpdated = FALSE      
       GroupDocNum               = 0     
       LosCalculation            = TRUE                            
       MainProvider              = FALSE   
       OverrideArsRate           = "":U
       OverrideBaseRate          = "":U                          
       PayeeDm                   = "":U 
       PenaltyFlag               = "":U
       PenaltyFlagUpdated        = FALSE
       PenaltyOverrideNote       = "":U
       PmbBenefit                = 0.00                              
       PmbIndicator              = FALSE                             
       PmbPayCost                = FALSE                             
       PmbValue                  = 0.00                              
       PrintDm                   = "":U                              
       ProviderSequence          = 0                               
       ProviderType              = "":U                              
       PrType                    = 0                                 
       QuantityAuth              = 0 
       QuantityClaimed           = 0                                
       QuantityPaid              = 0                                 
       QuantityRequested         = 0                                 
       AuthorisedService         = FALSE                             
       RateChangeType            = "":U                          
       StartAmPm                 = FALSE                             
       StartDate                 = ?                                 
       SubPrType                 = 0.                    
     
          
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisationprovider */




