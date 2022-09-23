/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Andrewd

      Purpose: Base Authorisation Detail Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisationdetail INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  DEFINE PUBLIC PROPERTY InFocus                 AS LOGICAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY LineNumber              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AddedByUser             AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AddToTotalLos           AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentAuth          AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentPaid          AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentPrivateAuth   AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AdjustmentPrivatePaid   AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountAuth              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountPaid              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountRequested         AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthGroupObj            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthDetailObj           AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthObj                 AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthProviderObj         AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatus              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatusNote          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY BodyRegion              AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCode               AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimType               AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCodeUpdated        AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimTypeUpdated        AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayAuth%              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayAuth               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayOverrideNote       AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayPaid               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CptObj                  AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultArsRate          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultBaseRate         AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimCode        AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimCodeSource  AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimType        AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ItemCost                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountAuth            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountPaid            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountType            AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EndAmPm                 AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                 AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EndDateAmPmUpdated      AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY FixedItemCost           AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LineRestriction         AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LocSequence             AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LocTariffTypeObj        AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LocValue                AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LosCalculationRule      AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MinutesAuth             AS INTEGER           NO-UNDO GET. SET.  
  DEFINE PUBLIC PROPERTY MinutesCalculationRule  AS CHARACTER         NO-UNDO GET. SET.  
  DEFINE PUBLIC PROPERTY MinutesRequested        AS INTEGER           NO-UNDO GET. SET.  
  DEFINE PUBLIC PROPERTY MinutesTariffTypeObj    AS DECIMAL           NO-UNDO GET. SET.   
  DEFINE PUBLIC PROPERTY MouthPartId             AS INTEGER EXTENT 10 NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Note                    AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OverrideArsRate         AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OverrideBaseRate        AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningAltValue          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic    AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningKey               AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PayeeDM                 AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbBenefit              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbIndicator            AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbPayCost              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbValue                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PrintDm                 AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityAuth            AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityClaimed         AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityLos             AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityPaid            AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityRequested       AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RateChange              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Reason                  AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedEntityMnemonic   AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedKey              AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedObj              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedValue            AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RepeatItem              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RepeatCycleAuth         AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RepeatCyclePaid         AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RepeatCycleQuantity     AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RepeatCycleDays         AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RepeatCycleGraceDays    AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RepeatLastClaimedDate   AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartAmPm               AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartDate               AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartDateAmPmUpdated    AS LOGICAL           NO-UNDO GET. SET.
                                                 
  DEFINE PUBLIC PROPERTY ErrorObject             AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.
 
 { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }


  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationdetail() :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationdetail */

                                                           
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationdetail
    ( INPUT ipdAuthDetailObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthDetailObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationdetail */
  
                                                           
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisationdetail() :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisationdetail */

  
  /* -- METHOD focusRecord -------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthDetailObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
   
    ASSIGN InFocus       = NOT unfocusRecord()
           
           AuthDetailObj = ipdAuthDetailObj
           
           InFocus       = getRecordDetails().
           
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
    DEFINE VARIABLE oSearch   AS cls.maauthsearch NO-UNDO.
    DEFINE VARIABLE iExtent   AS INTEGER          NO-UNDO.
    DEFINE VARIABLE hRelation AS HANDLE           NO-UNDO.
                                           
    DATASET dsAuthorisation:EMPTY-DATASET().
    
    IF AuthDetailObj <> 0.00 THEN
    DO:
      oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_provider,tt_auth_detail":U)
      
             glSuccess = oSearch:SetFilterCriteria("tt_auth_detail.auth_detail_obj":U, "=":U, AuthDetailObj).
             
      oSearch:fetchData().            
      
    END. /*IF AuthObj <> 0.00 THEN*/
    
    FOR FIRST tt_auth_detail NO-LOCK:

      ASSIGN
        LineNumber              = tt_auth_detail.line_number
        AddedByUser             = tt_auth_detail.added_by_user
        AddToTotalLos           = tt_auth_detail.add_to_total_los
        AdjustmentAuth          = tt_auth_detail.adjustment_auth        
        AdjustmentPaid          = tt_auth_detail.adjustment_paid        
        AdjustmentPrivateAuth   = tt_auth_detail.adjustment_private_auth
        AdjustmentPrivatePaid   = tt_auth_detail.adjustment_private_paid
        AmountAuth              = tt_auth_detail.amount_auth            
        AmountPaid              = tt_auth_detail.amount_paid            
        AmountRequested         = tt_auth_detail.amount_requested       
        AuthGroupObj            = tt_auth_detail.auth_group_obj    
        AuthDetailObj           = tt_auth_detail.auth_detail_obj        
        AuthObj                 = tt_auth_detail.auth_obj               
        AuthProviderObj         = tt_auth_detail.auth_provider_obj      
        AuthStatus              = tt_auth_detail.auth_status            
        AuthStatusNote          = tt_auth_detail.auth_status_note       
        BodyRegion              = tt_auth_detail.body_region            
        ClaimCode               = tt_auth_detail.claim_code             
        ClaimType               = tt_auth_detail.claim_type   
        ClaimCodeUpdated        = tt_auth_detail._claim_code_updated
        ClaimTypeUpdated        = tt_auth_detail._claim_type_updated
        CopayAuth%              = tt_auth_detail.copay_auth_%
        CopayAuth               = tt_auth_detail.copay_auth             
        CopayOverrideNote       = tt_auth_detail.copay_override_note             
        CopayPaid               = tt_auth_detail.copay_paid             
        CptObj                  = tt_auth_detail.cpt_link_obj 
        DefaultArsRate          = tt_auth_detail.default_ars_rate
        DefaultBaseRate         = tt_auth_detail.default_base_rate
        DefaultClaimCode        = tt_auth_detail.default_claim_code
        DefaultClaimCodeSource  = tt_auth_detail.default_claim_code_source
        DefaultClaimType        = tt_auth_detail.default_claim_type
        ItemCost                = tt_auth_detail.item_cost      
        DiscountAuth            = tt_auth_detail.discount_auth          
        DiscountPaid            = tt_auth_detail.discount_paid          
        DiscountType            = tt_auth_detail.discount_type          
        EndAmPm                 = tt_auth_detail.end_ampm               
        EndDate                 = tt_auth_detail.end_date               
        EndDateAmPmUpdated      = tt_auth_detail._end_date_ampm_updated
        FixedItemCost           = tt_auth_detail.fixed_item_cost
        LineRestriction         = tt_auth_detail.line_restriction       
        LocSequence             = tt_auth_detail.loc_sequence         
        LocTariffTypeObj        = tt_auth_detail.loc_tariff_type_obj
        LocValue                = tt_auth_detail.loc_value
        LosCalculationRule      = tt_auth_detail.los_calculation_rule
        MinutesAuth             = tt_auth_detail.minutes_auth
        MinutesCalculationRule  = tt_auth_detail.minutes_calculation_rule
        MinutesRequested        = tt_auth_detail.minutes_requested
        MinutesTariffTypeObj    = tt_auth_detail.minutes_tariff_type_obj
        Note                    = tt_auth_detail.note                   
        OverrideArsRate         = tt_auth_detail.override_ars_rate
        OverrideBaseRate        = tt_auth_detail.override_base_rate
        OwningAltValue          = tt_auth_detail.owning_alt_value       
        OwningEntityMnemonic    = tt_auth_detail.owning_entity_mnemonic 
        OwningKey               = tt_auth_detail.owning_key             
        OwningObj               = tt_auth_detail.owning_obj             
        PayeeDM                 = tt_auth_detail.payee_dm
        PmbBenefit              = tt_auth_detail.pmb_benefit         
        PmbIndicator            = tt_auth_detail.pmb_indicator          
        PmbPayCost              = tt_auth_detail.pmb_pay_cost           
        PmbValue                = tt_auth_detail.pmb_value              
        PrintDm                 = tt_auth_detail.print_dm               
        QuantityAuth            = tt_auth_detail.quantity_auth          
        QuantityClaimed         = tt_auth_detail.quantity_claimed    
        QuantityLos             = tt_auth_detail.quantity_los          
        QuantityPaid            = tt_auth_detail.quantity_paid          
        QuantityRequested       = tt_auth_detail.quantity_requested
        RateChange              = tt_auth_detail.rate_change
        Reason                  = tt_auth_detail.reason
        RelatedEntityMnemonic   = tt_auth_detail.related_entity_mnemonic
        RelatedKey              = tt_auth_detail.related_key
        RelatedObj              = tt_auth_detail.related_obj
        RelatedValue            = tt_auth_detail.related_value
        RepeatItem              = tt_auth_detail.repeat_item               
        RepeatCycleAuth         = tt_auth_detail.repeat_cycle_auth          
	      RepeatCyclePaid         = tt_auth_detail.repeat_cycle_paid          
	      RepeatCycleQuantity     = tt_auth_detail.repeat_cycle_quantity      
	      RepeatCycleDays         = tt_auth_detail.repeat_cycle_days          
	      RepeatCycleGraceDays    = tt_auth_detail.repeat_cycle_grace_days    
	      RepeatLastClaimedDate   = tt_auth_detail.repeat_last_claimed_date   		 
        StartAmPm               = tt_auth_detail.start_ampm             
        StartDate               = tt_auth_detail.start_date             
        StartDateAmPmUpdated    = tt_auth_detail._start_date_ampm_updated
        . 
        
      DO iExtent = 1 TO EXTENT(MouthPartId):  

        ASSIGN MouthPartId[iExtent] = tt_auth_detail.mouth_part_id[iExtent].

      END. /*DO iExtent = 1 TO EXTENT(MouthPartId):*/                                   
    END. /*FOR FIRST tt_auth_detail NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth_detail NO-LOCK).
    
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
    DEFINE BUFFER btt_auth_detail FOR tt_auth_detail.
      
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation detail not in focus'" }
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth_detail EXCLUSIVE-LOCK
        WHERE btt_auth_detail.auth_detail_obj = AuthDetailObj.
           
      ASSIGN btt_auth_detail.record_action = "delete":U.     
    
    END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
    
&IF {&DBDFMA} >= 010195 &THEN        
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).
&ENDIF    
   
    FOR EACH tt_auth_error EXCLUSIVE-LOCK
      WHERE NOT tt_auth_error.owning_entity_mnemonic BEGINS "hatad":U
        AND     tt_auth_error.error_type                 <> "ERR":U
        AND     tt_auth_error.error_type                 <> "ERROR":U :
      
      DELETE tt_auth_error.
       
    END.  /* FOR EACH tt_auth_error */ 
    
    ASSIGN InFocus =  ErrorObject:ErrorsExist . 
    
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* removeAuthorisation  */
                                    

  /* -- METHOD saveRecord ---------------------------------------------- */
  METHOD PUBLIC LOGICAL saveRecord() :
  /*--------------------------------------------------------------------
        Purpose : Create/Update authorisation record using the instance 
                  properties specified.   
        
          Notes :
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
    
    DATASET dsAuthorisation:EMPTY-DATASET().
    
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      CREATE tt_auth_detail.

      ASSIGN
        tt_auth_detail.record_action             = "Modify":U
        tt_auth_detail.line_number               = LineNumber 
        tt_auth_detail.added_by_user             = AddedByUser 
        tt_auth_detail.add_to_total_los          = AddToTotalLos
        tt_auth_detail.adjustment_auth           = AdjustmentAuth       
        tt_auth_detail.adjustment_paid           = AdjustmentPaid       
        tt_auth_detail.adjustment_private_auth   = AdjustmentPrivateAuth
        tt_auth_detail.adjustment_private_paid   = AdjustmentPrivatePaid
        tt_auth_detail.amount_auth               = AmountAuth           
        tt_auth_detail.amount_paid               = AmountPaid           
        tt_auth_detail.amount_requested          = AmountRequested      
        tt_auth_detail.auth_group_obj            = AuthGroupObj     
        tt_auth_detail.auth_detail_obj           = AuthDetailObj        
        tt_auth_detail.auth_obj                  = AuthObj              
        tt_auth_detail.auth_provider_obj         = AuthProviderObj      
        tt_auth_detail.auth_status               = AuthStatus           
        tt_auth_detail.auth_status_note          = AuthStatusNote       
        tt_auth_detail.body_region               = BodyRegion           
        tt_auth_detail.claim_code                = ClaimCode            
        tt_auth_detail.claim_type                = ClaimType    
        tt_auth_detail._claim_code_updated       = ClaimCodeUpdated            
        tt_auth_detail._claim_type_updated       = ClaimTypeUpdated 
        tt_auth_detail.copay_auth_%              = CopayAuth%  
        tt_auth_detail.copay_auth                = CopayAuth            
        tt_auth_detail.copay_override_note       = CopayOverrideNote           
        tt_auth_detail.copay_paid                = CopayPaid            
        tt_auth_detail.cpt_link_obj              = CptObj    
        tt_auth_detail.default_ars_rate          = DefaultArsRate
        tt_auth_detail.default_base_rate         = DefaultBaseRate
        tt_auth_detail.default_claim_code        = DefaultClaimCode
        tt_auth_detail.default_claim_code_source = DefaultClaimCodeSource
        tt_auth_detail.default_claim_type        = DefaultClaimType
        tt_auth_detail.discount_auth             = DiscountAuth         
        tt_auth_detail.discount_paid             = DiscountPaid         
        tt_auth_detail.discount_type             = DiscountType         
        tt_auth_detail.end_ampm                  = EndAmPm              
        tt_auth_detail.end_date                  = EndDate              
        tt_auth_detail._end_date_ampm_updated    = EndDateAmPmUpdated
        tt_auth_detail.fixed_item_cost           = FixedItemCost
        tt_auth_detail.item_cost                 = ItemCost
        tt_auth_detail.line_restriction          = LineRestriction      
        tt_auth_detail.loc_sequence              = LocSequence 
        tt_auth_detail.loc_tariff_type_obj       = LocTariffTypeObj
        tt_auth_detail.loc_value                 = LocValue        
        tt_auth_detail.los_calculation_rule      = LosCalculationRule
        tt_auth_detail.minutes_auth              = MinutesAuth
        tt_auth_detail.minutes_calculation_rule  = MinutesCalculationRule
        tt_auth_detail.minutes_requested         = MinutesRequested
        tt_auth_detail.minutes_tariff_type_obj   = MinutesTariffTypeObj
        tt_auth_detail.note                      = Note           
        tt_auth_detail.override_ars_rate         = OverrideArsRate
        tt_auth_detail.override_base_rate        = OverrideBaseRate      
        tt_auth_detail.owning_alt_value          = OwningAltValue       
        tt_auth_detail.owning_entity_mnemonic    = OwningEntityMnemonic 
        tt_auth_detail.owning_key                = OwningKey            
        tt_auth_detail.owning_obj                = OwningObj            
        tt_auth_detail.payee_dm                  = PayeeDM
        tt_auth_detail.pmb_benefit               = PmbBenefit           
        tt_auth_detail.pmb_indicator             = PmbIndicator         
        tt_auth_detail.pmb_pay_cost              = PmbPayCost           
        tt_auth_detail.pmb_value                 = PmbValue             
        tt_auth_detail.print_dm                  = PrintDm              
        tt_auth_detail.quantity_auth             = QuantityAuth         
        tt_auth_detail.quantity_los              = QuantityLos
        tt_auth_detail.quantity_claimed          = QuantityClaimed         
        tt_auth_detail.quantity_paid             = QuantityPaid         
        tt_auth_detail.quantity_requested        = QuantityRequested 
        tt_auth_detail.rate_change               = RateChange
        tt_auth_detail.reason                    = Reason
        tt_auth_detail.related_entity_mnemonic   = RelatedEntityMnemonic 
        tt_auth_detail.related_key               = RelatedKey            
        tt_auth_detail.related_obj               = RelatedObj            
        tt_auth_detail.related_value             = RelatedValue          
        tt_auth_detail.repeat_item               = RepeatItem
	      tt_auth_detail.repeat_cycle_auth         = RepeatCycleAuth
	      tt_auth_detail.repeat_cycle_paid         = RepeatCyclePaid
	      tt_auth_detail.repeat_cycle_quantity     = RepeatCycleQuantity
	      tt_auth_detail.repeat_cycle_days         = RepeatCycleDays
	      tt_auth_detail.repeat_cycle_grace_days   = RepeatCycleGraceDays
	      tt_auth_detail.repeat_last_claimed_date  = RepeatLastClaimedDate
        tt_auth_detail.start_ampm                = StartAmPm            
        tt_auth_detail.start_date                = StartDate            
        tt_auth_detail._start_date_ampm_updated  = StartDateAmPmUpdated
        .            
        
      DO iExtent = 1 TO EXTENT(MouthPartId):
      
        ASSIGN tt_auth_detail.mouth_part_id[iExtent] = MouthPartId[iExtent].
        
      END. /*DO iExtent = 1 TO EXTENT(MouthPartId):*/
        
      VALIDATE tt_auth_detail.                 
    
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

      IF AuthDetailObj <= 0.00
      THEN 
      FOR FIRST tt_auth_detail NO-LOCK
        WHERE tt_auth_detail.auth_obj                = AuthObj
          AND tt_auth_detail.auth_provider_obj       = AuthProviderObj
          AND tt_auth_detail.owning_entity_mnemonic  = OwningEntityMnemonic
          AND tt_auth_detail.owning_obj              = OwningObj
          AND tt_auth_detail.owning_key              = OwningKey
          AND tt_auth_detail.related_entity_mnemonic = RelatedEntityMnemonic 
          AND tt_auth_detail.related_obj             = RelatedObj            
          AND tt_auth_detail.related_key             = RelatedKey
          AND tt_auth_detail.start_date              = StartDate
          AND tt_auth_detail.start_ampm              = StartAmPm:
        
        ASSIGN AuthDetailObj = tt_auth_detail.auth_detail_obj.
                       
      END. /*FOR FIRST tt_auth_detail NO-LOCK:*/

      ASSIGN glSuccess = focusRecord(AuthDetailObj).
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    
    /*
      Copy warnings back to the dataset error temp table
    */
    IF TEMP-TABLE tt_temp_error:HAS-RECORDS
    THEN TEMP-TABLE tt_auth_error:COPY-TEMP-TABLE(TEMP-TABLE tt_temp_error:HANDLE).

    FOR EACH tt_auth_error EXCLUSIVE-LOCK
      WHERE  NOT  tt_auth_error.owning_entity_mnemonic BEGINS "hatad":U
        AND       tt_auth_error.error_type                 <> "ERR":U
        AND       tt_auth_error.error_type                 <> "ERROR":U  :
       
      DELETE tt_auth_error.
       
    END.  /* FOR EACH tt_auth_error */ 
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* saveRecord  */
  
  
  /* -- METHOD unfocusRecord ------------------------------------------- */
  METHOD PUBLIC LOGICAL unfocusRecord() :
  /*--------------------------------------------------------------------
        Purpose:  
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
    
    ErrorObject:ClearErrors().
    
    ASSIGN 
       InFocus                 = FALSE
       LineNumber              = 0    
       AddedByUser             = FALSE             
       AddToTotalLos           = TRUE
       AdjustmentAuth          = 0.00             
       AdjustmentPaid          = 0.00             
       AdjustmentPrivateAuth   = 0.00             
       AdjustmentPrivatePaid   = 0.00             
       AmountAuth              = 0.00             
       AmountPaid              = 0.00             
       AmountRequested         = 0.00 
       AuthGroupObj            = 0.00             
       AuthDetailObj           = 0.00             
       AuthObj                 = 0.00             
       AuthProviderObj         = 0.00             
       AuthStatus              = 0                
       AuthStatusNote          = "":U             
       BodyRegion              = "":U             
       ClaimCode               = 0                
       ClaimType               = "":U    
       ClaimCodeUpdated        = FALSE    
       ClaimTypeUpdated        = FALSE 
       CopayAuth%              = 0.00
       CopayAuth               = 0             
       CopayOverrideNote       = "":U                   
       CopayPaid               = 0                
       CptObj                  = 0
       DefaultArsRate          = "":U     
       DefaultBaseRate         = "":U     
       DefaultClaimCode        = 0 
       DefaultClaimCodeSource  = "":U
       DefaultClaimType        = "":U
       ItemCost                = 0.00        
       DiscountAuth            = 0                
       DiscountPaid            = 0                
       DiscountType            = FALSE            
       EndAmPm                 = FALSE            
       EndDate                 = ?                
       EndDateAmPmUpdated      = FALSE
       FixedItemCost           = 0.00
       LineRestriction         = "":U             
       LocSequence             = 0   
       LocTariffTypeObj        = 0.00
       LocValue                = "":U             
       LosCalculationRule      = "":U    
       MinutesAuth             = 0
       MinutesCalculationRule  = "":U
       MinutesRequested        = 0
       MinutesTariffTypeObj    = 0.00
       Note                    = "":U     
       OverrideArsRate         = "":U     
       OverrideBaseRate        = "":U             
       OwningAltValue          = "":U             
       OwningEntityMnemonic    = "":U             
       OwningKey               = "":U             
       OwningObj               = 0.00  
       PayeeDM                 = "":U
       PmbBenefit              = 0.00             
       PmbIndicator            = FALSE            
       PmbPayCost              = FALSE            
       PmbValue                = 0.00             
       PrintDm                 = "":U             
       QuantityAuth            = 0      
       QuantityClaimed         = 0      
       QuantityLos             = 0      
       QuantityPaid            = 0                
       QuantityRequested       = 0     
       RateChange              = FALSE
       Reason                  = "":U
       RelatedEntityMnemonic   = "":U
       RelatedKey              = "":U
       RelatedObj              = 0.00
       RelatedValue            = "":U
       RepeatItem              = FALSE 
       RepeatCycleAuth         = 0
       RepeatCyclePaid         = 0
       RepeatCycleQuantity     = 0
       RepeatCycleDays         = 0
       RepeatCycleGraceDays    = 0 
       RepeatLastClaimedDate   = ?
       StartAmPm               = FALSE            
       StartDate               = ?                
       StartDateAmPmUpdated    = FALSE
       .    
                  
    DO iExtent = 1 TO EXTENT(MouthPartId):
    
      ASSIGN MouthPartId[iExtent] = 0.
      
    END. /*DO iExtent = 1 TO EXTENT(MouthPartId):*/
                  
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisationdetail */




