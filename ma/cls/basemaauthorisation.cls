
/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Andrewd

      Purpose: Base Authorisation Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisation INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  { ma/inc/maauthtypeds.i }
  { ma/inc/madephealthds.i  }                                                 

  DEFINE PUBLIC PROPERTY InFocus                   AS LOGICAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthObj                   AS DECIMAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY LineNumber                AS INTEGER           NO-UNDO GET. SET.
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
  DEFINE PUBLIC PROPERTY AuthoriseAllServices      AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthDate                  AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthEpisodeObj            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthEpisodeNum            AS CHARACTER         NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthIncomplete            AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthNum                   AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatus                AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatusNote            AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatusNoteDescription AS CHARACTER         NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthTypeObj               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Benefit                   AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY BodyRegion                AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCode                 AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimType                 AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimCode          AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DefaultClaimType          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Dependant                 AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DependantReference        AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountAuth              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountPaid              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DiscountType              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DueDate                   AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EndAmPm                   AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                   AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MemNum                    AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MouthPartId               AS INTEGER EXTENT 10 NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OptionCode                AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MemberOptionCode          AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbBenefit                AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbIndicator              AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbPayCost                AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbValue                  AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityAuth              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityClaimed           AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityPaid              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY QuantityRequested         AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ReferenceAuthNum          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RequestBy                 AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RequestDate               AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RequestSource             AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RequestSourceDetails      AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartAmPm                 AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartDate                 AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthUserId                AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ServiceType               AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY NextOfKin                 AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY TotalLos                  AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthStatusUpdated         AS LOGICAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY DependantAgeDays          AS INTEGER           NO-UNDO GET. PRIVATE SET. 
  DEFINE PUBLIC PROPERTY DependantAgeYears         AS INTEGER           NO-UNDO GET. PRIVATE SET. 
  DEFINE PUBLIC PROPERTY DependantGender           AS CHARACTER         NO-UNDO GET. PRIVATE SET.  
  DEFINE PUBLIC PROPERTY DependantBMI              AS DECIMAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY ErrorObject               AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.
  
  
  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }


  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisation
     () :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisation */

                                                           
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisation
    ( INPUT ipdAuthorisationObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthorisationObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisation */
  
  
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisation
    ( INPUT ipcAuthorisationNum AS CHARACTER ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipcAuthorisationNum).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisation */
  
                                                           
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisation() :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.    
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisation */

  
  /* -- METHOD focusRecord --------------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthorisationObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
    
    ASSIGN InFocus  = unfocusRecord()
    
           AuthObj  = ipdAuthorisationObj
           
           InFocus  = getRecordDetails().
      
    RETURN InFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusRecord  */
  
  
  /* -- METHOD focusRecord --------------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipcAuthorisationNum AS CHARACTER ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
    
    ASSIGN InFocus  = unfocusRecord()
           
           AuthNum  = ipcAuthorisationNum
           
           InFocus  = getRecordDetails().
           
    RETURN InFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusRecord  */
  
  
  /* -- METHOD getAuthType --------------------------------------------- */
  METHOD PUBLIC cls.maauthtype getAuthType
    ( ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE lSuccess  AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE oAuthType AS cls.maauthtype NO-UNDO.
    
    
    ASSIGN oAuthType = NEW cls.maauthtype()
    
           lSuccess  = oAuthType:focusAuthType(AuthTypeObj)
           
           lSuccess  = mipEnv:registerForCleanup(oAuthType).
    
    RETURN oAuthType.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* getAuthType */
  
                                 
  /* -- METHOD getAuthTypeDataset -------------------------------------- */
  METHOD PUBLIC LOGICAL getAuthTypeDataset
    ( OUTPUT DATASET FOR dsAuthType ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE lSuccess AS LOGICAL              NO-UNDO.
    DEFINE VARIABLE oSearch  AS cls.maauthtypesearch NO-UNDO.
    
    IF AuthTypeObj <> 0.00 THEN
    DO:
      DATASET dsAuthType:EMPTY-DATASET().
      
      oSearch = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).
      
      ASSIGN lSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth_type,tt_auth_type_provider,tt_auth_type_control":U)
      
             lSuccess = oSearch:SetFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, AuthTypeObj). 
             
      oSearch:fetchData().       
    END. /*IF AuthTypeObj <> 0.00 THEN*/
    
    RETURN TRUE.    
        
    { mip/inc/mipcatcherror.i 
     &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}

  END METHOD.  /* getAuthTypeDataset  */
  

  /* -- METHOD getRecordDetails ---------------------------------------- */
  METHOD PRIVATE LOGICAL getRecordDetails
    (  ) :
  /*--------------------------------------------------------------------
        Purpose: Internal method for retrieveing authorisation data
                 using the authorisation data retrieval services.
          Notes:
  ---------------------------------------------------------------------- */    
    DEFINE VARIABLE oSearch          AS cls.maauthsearch       NO-UNDO.
    DEFINE VARIABLE oDepHealthSearch AS cls.madephealthsearch  NO-UNDO.
    DEFINE VARIABLE iExtent          AS INTEGER                NO-UNDO.
    DEFINE VARIABLE iOption          AS INTEGER                NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL                NO-UNDO.
    
                                           
    DATASET dsAuthorisation:EMPTY-DATASET().
    DATASET dsDepHealth:EMPTY-DATASET().
    
    IF AuthObj <> 0.00 OR AuthNum <> "":U THEN
    DO:
      oSearch          = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U , "tt_auth,tt_auth_episode":U) 
             glSuccess = oSearch:SetCriteria("DataSecured":U, "FALSE":U)
      
             glSuccess = (IF AuthObj <> 0.00 
                          THEN oSearch:SetFilterCriteria("tt_auth.auth_obj":U, "=":U, AuthObj)
                          ELSE TRUE)
                          
             glSuccess = (IF AuthNum <> "":U
                          THEN oSearch:SetFilterCriteria("tt_auth.auth_num":U, "=":U, AuthNum)
                          ELSE TRUE).
             
      oSearch:fetchData().             
      
    END. /*IF AuthObj <> 0.00 THEN*/
    
  
    FOR FIRST tt_auth NO-LOCK:
      
      ASSIGN
         LineNumber            = tt_auth.line_number             
         AdjustmentAuth        = tt_auth.adjustment_auth         
         AdjustmentPaid        = tt_auth.adjustment_paid         
         AdjustmentPrivateAuth = tt_auth.adjustment_private_auth 
         AdjustmentPrivatePaid = tt_auth.adjustment_private_paid 
         AmountAuth            = tt_auth.amount_auth             
         AmountClaimed         = tt_auth.amount_claimed          
         AmountInterim         = tt_auth.amount_interim          
         AmountPaid            = tt_auth.amount_paid             
         AmountRequested       = tt_auth.amount_requested        
         AmountTotal           = tt_auth.amount_total            
         AuthDate              = tt_auth.auth_date
         AuthIncomplete        = tt_auth.auth_incomplete               
         AuthNum               = tt_auth.auth_num                
         AuthObj               = tt_auth.auth_obj                
         AuthStatus            = tt_auth.auth_status             
         AuthStatusNote        = tt_auth.auth_status_note        
         AuthTypeObj           = tt_auth.auth_type_obj           
         AuthoriseAllServices  = tt_auth.authorise_all_services
         Benefit               = tt_auth.benefit 
         BodyRegion            = tt_auth.body_region             
         ClaimCode             = tt_auth.claim_code              
         ClaimType             = tt_auth.claim_type              
         DefaultClaimCode      = tt_auth.default_claim_code
         DefaultClaimType      = tt_auth.default_claim_type
         Dependant             = tt_auth.dependant  
         DependantReference    = tt_auth.dependant_reference
         DiscountAuth          = tt_auth.discount_auth           
         DiscountPaid          = tt_auth.discount_paid           
         DiscountType          = tt_auth.discount_type           
         DueDate               = tt_auth.due_date
         EndAmPm               = tt_auth.end_ampm                
         EndDate               = tt_auth.end_date                
         AuthEpisodeObj        = tt_auth.auth_episode_obj             
         insurerObj            = tt_auth.insurer_obj             
         MemNum                = tt_auth.mem_num                 
         OptionCode            = tt_auth.option_code             
         MemberOptionCode      = tt_auth.option_code             
         PmbBenefit            = tt_auth.pmb_benefit           
         PmbIndicator          = tt_auth.pmb_indicator           
         PmbPayCost            = tt_auth.pmb_pay_cost            
         PmbValue              = tt_auth.pmb_value               
         QuantityAuth          = tt_auth.quantity_auth 
         QuantityClaimed       = tt_auth.quantity_claimed          
         QuantityPaid          = tt_auth.quantity_paid           
         QuantityRequested     = tt_auth.quantity_requested      
         ReferenceAuthNum      = tt_auth.reference_auth_num      
         RequestBy             = tt_auth.request_by              
         RequestDate           = tt_auth.request_date            
         RequestSource         = tt_auth.request_source          
         RequestSourceDetails  = tt_auth.request_source_details          
         ServiceType           = tt_auth.service_type
         NextOfKin             = tt_auth.next_of_kin
         StartAmPm             = tt_auth.start_ampm              
         StartDate             = tt_auth.start_date              
         AuthUserId            = tt_auth.user_id
         TotalLos              = tt_auth.total_los.                 
    
      DO iExtent = 1 TO EXTENT(MouthPartId):
      
        ASSIGN MouthPartId[iExtent] = tt_auth.mouth_part_id[iExtent].
      END. /*DO iExtent = 1 TO EXTENT(MouthPartId):*/    
      
      /*
        Set member option code from relevant member scheme record
      */
      FIND FIRST memsch NO-LOCK
           WHERE memsch.mem-num = MemNum 
             AND memsch.start-date <= StartDate
        NO-ERROR.
       
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
      
      IF AVAILABLE memsch 
      THEN 
        ASSIGN MemberOptionCode = memsch.scheme-code.
      
      /*
        Set status note description if applicable
      */
      FIND FIRST note NO-LOCK
           WHERE note.scheme-code = MemberOptionCode
             AND note.type = "AS":U
             AND note.key = AuthStatusNote
        NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE note
      THEN 
        ASSIGN AuthStatusNoteDescription = note.narration[1].

      /*
        Set episode number property if episode exists
      */
      FIND FIRST tt_auth_episode NO-LOCK
           WHERE tt_auth_episode.auth_episode_obj = AuthEpisodeObj
        NO-ERROR.  
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }  
      
      IF AVAILABLE tt_auth_episode
      THEN 
        ASSIGN AuthEpisodeNum = tt_auth_episode.episode_num.  

      /*
        Set member's age and gender
      */
      IF Dependant <> 99 THEN 
      DO:
        mipEnv:Health:maMember:getDependantAgeGender
          (INPUT  MemNum,
           INPUT  Dependant,
           INPUT  StartDate,
           OUTPUT DependantAgeYears,
           OUTPUT DependantAgeDays,
           OUTPUT DependantGender).
        
        ASSIGN
          oDepHealthSearch = NEW cls.madephealthsearch(DATASET dsDepHealth BY-REFERENCE)
          glSuccess        = oDepHealthSearch:SetCriteria("BufferList":U , "tt_dephealth":U) 
          glSuccess        = oDepHealthSearch:SetCriteria("DataSecured":U, "FALSE":U)
          glSuccess        = oDepHealthSearch:setFilterCriteria("tt_dephealth.mem_num":U,   "=":U, tt_auth.mem_num)          
          glSuccess        = oDepHealthSearch:setFilterCriteria("tt_dephealth.dependant":U, "=":U, tt_auth.dependant)        
          glSuccess        = oDepHealthSearch:fetchData().
      
        /*
          Set Dependant BMI
        */
        mipEnv:Health:AuthService:getAuthBMI(INPUT TABLE tt_dephealth,
                                             BUFFER      tt_auth,
                                             OUTPUT      DependantBMI).
      END. /* IF Dependant <> 99 */
    END. /*FOR FIRST tt_auth NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth NO-LOCK).
    
    { mip/inc/mipcatcherror.i 
      &FINALLY = "IF VALID-OBJECT(oSearch)          THEN DELETE OBJECT oSearch.
                  IF VALID-OBJECT(oDepHealthSearch) THEN DELETE OBJECT oDepHealthSearch."}

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
    DEFINE BUFFER btt_auth FOR tt_auth.
    
      
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation not in focus'" }
      
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth EXCLUSIVE-LOCK
           WHERE btt_auth.auth_obj = AuthObj
        NO-ERROR.

      {mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE}
      
      IF AVAILABLE btt_auth THEN
      ASSIGN btt_auth.record_action = "delete":U.     
    
    END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
    
&IF {&DBDFMA} >= 010195 &THEN    
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).
&ENDIF    
        
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* deleteRecord  */
                                    

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
    
      CREATE tt_auth.
      
      ASSIGN
        tt_auth.record_action             = "Modify":U
        tt_auth.line_number               = LineNumber           
        tt_auth.adjustment_auth           = AdjustmentAuth       
        tt_auth.adjustment_paid           = AdjustmentPaid       
        tt_auth.adjustment_private_auth   = AdjustmentPrivateAuth
        tt_auth.adjustment_private_paid   = AdjustmentPrivatePaid
        tt_auth.amount_auth               = AmountAuth           
        tt_auth.amount_claimed            = AmountClaimed        
        tt_auth.amount_interim            = AmountInterim        
        tt_auth.amount_paid               = AmountPaid           
        tt_auth.amount_requested          = AmountRequested      
        tt_auth.amount_total              = AmountTotal          
        tt_auth.auth_date                 = AuthDate 
        tt_auth.auth_incomplete           = AuthIncomplete            
        tt_auth.auth_num                  = AuthNum              
        tt_auth.auth_obj                  = AuthObj              
        tt_auth.auth_status               = AuthStatus           
        tt_auth.auth_status_note          = AuthStatusNote       
        tt_auth.auth_type_obj             = AuthTypeObj 
        tt_auth.authorise_all_services    = AuthoriseAllServices 
        tt_auth.benefit                   = Benefit
        tt_auth.body_region               = BodyRegion           
        tt_auth.claim_code                = ClaimCode            
        tt_auth.claim_type                = ClaimType            
        tt_auth.default_claim_code        = DefaultClaimCode
        tt_auth.default_claim_type        = DefaultClaimType
        tt_auth.dependant                 = Dependant     
        tt_auth.dependant_reference       = DependantReference
        tt_auth.discount_auth             = DiscountAuth         
        tt_auth.discount_paid             = DiscountPaid         
        tt_auth.discount_type             = DiscountType 
        tt_auth.due_date                  = DueDate
        tt_auth.end_ampm                  = EndAmPm              
        tt_auth.end_date                  = EndDate              
        tt_auth.auth_episode_obj          = AuthEpisodeObj           
        tt_auth.insurer_obj               = insurerObj           
        tt_auth.mem_num                   = MemNum               
        tt_auth.mouth_part_id             = MouthPartId          
        tt_auth.option_code               = OptionCode           
        tt_auth.pmb_benefit               = PmbBenefit           
        tt_auth.pmb_indicator             = PmbIndicator         
        tt_auth.pmb_pay_cost              = PmbPayCost           
        tt_auth.pmb_value                 = PmbValue             
        tt_auth.quantity_auth             = QuantityAuth 
        tt_auth.quantity_claimed          = QuantityClaimed        
        tt_auth.quantity_paid             = QuantityPaid         
        tt_auth.quantity_requested        = QuantityRequested    
        tt_auth.reference_auth_num        = ReferenceAuthNum     
        tt_auth.request_by                = RequestBy            
        tt_auth.request_date              = RequestDate          
        tt_auth.request_source            = RequestSource        
        tt_auth.request_source_details    = RequestSourceDetails        
        tt_auth.service_type              = ServiceType
        tt_auth.next_of_kin               = NextOfKin
        tt_auth.start_ampm                = StartAmPm            
        tt_auth.start_date                = StartDate            
        tt_auth.user_id                   = AuthUserId           
        tt_auth.total_los                 = TotalLos
        tt_auth._auth_status_updated      = AuthStatusUpdated.
        
      DO iExtent = 1 TO EXTENT(MouthPartId):
      
        ASSIGN tt_auth.mouth_part_id[iExtent] = MouthPartId[iExtent].
        
      END. /*DO iExtent = 1 TO EXTENT(MouthPartId):*/
      
      VALIDATE tt_auth.                 
          
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
      FOR FIRST tt_auth NO-LOCK:
        
        ASSIGN AuthObj   = tt_auth.auth_obj
        
               glSuccess = focusRecord(AuthObj).                              
      END. /*FOR FIRST tt_auth NO-LOCK:*/
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
    DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
    
    
    ErrorObject:ClearErrors().
    
    
    ASSIGN                       
       InFocus                   = FALSE                   
       LineNumber                = 0                       
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
       AuthDate                  = ?      
       AuthIncomplete            = FALSE                 
       AuthNum                   = ""                  
       AuthObj                   = 0.00                    
       AuthStatus                = 0                       
       AuthStatusNote            = ""                  
       AuthTypeObj               = 0.00    
       AuthoriseAllServices      = FALSE
       Benefit                   = 0.00              
       BodyRegion                = ""                   
       ClaimCode                 = 0                       
       ClaimType                 = ""                    
       DefaultClaimCode          = 0
       DefaultClaimType          = ""
       Dependant                 = 0                       
       DependantReference        = "":U
       DiscountAuth              = 0.00                    
       DiscountPaid              = 0.00                    
       DiscountType              = FALSE   
       DueDate                   = ?                
       EndAmPm                   = FALSE                   
       EndDate                   = ?                       
       AuthEpisodeObj            = 0.00            
       AuthEpisodeNum            = "":U        
       insurerObj                = 0.00                    
       MemNum                    = ""                  
       OptionCode                = 0                       
       PmbBenefit                = 0.00                    
       PmbIndicator              = FALSE                   
       PmbPayCost                = FALSE                   
       PmbValue                  = 0.00                    
       QuantityAuth              = 0   
       QuantityClaimed           = 0                    
       QuantityPaid              = 0                       
       QuantityRequested         = 0                       
       ReferenceAuthNum          = ""                   
       RequestBy                 = ""                   
       RequestDate               = ?                       
       RequestSource             = ""                   
       RequestSourceDetails      = ""                   
       ServiceType               = ""                   
       NextOfKin                 = ""                   
       StartAmPm                 = FALSE                   
       StartDate                 = ?                       
       AuthUserId                = ""
       AuthStatusNoteDescription = ""
       TotalLos                  = 0.00
       DependantAgeYears         = 0
       DependantAgeDays          = 0
       DependantGender           = "":U
       DependantBMI              = 0.00
       AuthStatusUpdated         = FALSE.                    
     
          
    DO iExtent = 1 TO EXTENT(MouthPartId):
    
      ASSIGN MouthPartId[iExtent] = 0.
      
    END. /*DO iExtent = 1 TO EXTENT(MouthPartId):*/         
     
     
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisation */




