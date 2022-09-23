/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandlam

      Purpose: Base Authorisation Limit Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisationlimit INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  DEFINE PUBLIC PROPERTY InFocus                 AS LOGICAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY LineNumber              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthObj                 AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthLimitObj            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCode               AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ClaimType               AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Dependant               AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate           AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY FetchDateTime           AS DATETIME          NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Limit#                  AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LimitAvail              AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LimitUsed               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY LimitReserved           AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OptionCode              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic    AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningAltValue          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RatePer                 AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ErrorObject             AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.
 
 { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }

  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationlimit() :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationlimit */

  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationlimit
    ( INPUT ipdAuthLimitObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthLimitObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationlimit */
  
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisationlimit() :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisationlimit */

  /* -- METHOD focusRecord -------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthLimitObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
   
    ASSIGN InFocus      = NOT unfocusRecord()
           AuthLimitObj = ipdAuthLimitObj
           InFocus      = getRecordLimit().
           
    RETURN InFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusRecord  */
  

  /* -- METHOD getRecordlimit ---------------------------------------- */
  METHOD PRIVATE LOGICAL getRecordLimit
    (  ) :
  /*--------------------------------------------------------------------
        Purpose: Internal method for retrieveing authorisation data
                 using the authorisation data retrieval services.
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearch   AS cls.maauthsearch NO-UNDO.
    
    DATASET dsAuthorisation:EMPTY-DATASET().
        
    IF AuthlimitObj <> 0.00 THEN
    DO:
      oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_limit":U)
             glSuccess = oSearch:SetFilterCriteria("tt_auth_limit.auth_limit_obj":U, "=":U, AuthLimitObj).
             
      oSearch:fetchData().            
      
    END. /*IF AuthObj <> 0.00 THEN*/
    
    FOR FIRST tt_auth_limit NO-LOCK:

      ASSIGN
         LineNumber           = tt_auth_limit.line_number
         AuthObj              = tt_auth_limit.auth_obj              
         AuthLimitObj         = tt_auth_limit.auth_limit_obj        
         ClaimCode            = tt_auth_limit.claim_code            
         ClaimType            = tt_auth_limit.claim_type            
         Dependant            = tt_auth_limit.dependant             
         EffectiveDate        = tt_auth_limit.effective_date        
         FetchDateTime        = tt_auth_limit.fetch_date_time       
         Limit#               = tt_auth_limit.limit_#               
         LimitAvail           = tt_auth_limit.limit_avail       
         LimitUsed            = tt_auth_limit.limit_used            
         LimitReserved        = tt_auth_limit.limit_reserved        
         OptionCode           = tt_auth_limit.option_code           
         OwningEntityMnemonic = tt_auth_limit.owning_entity_mnemonic
         OwningObj            = tt_auth_limit.owning_obj            
         OwningAltValue       = tt_auth_limit.owning_alt_value      
         RatePer              = tt_auth_limit.rate_per.              

    END. /*FOR FIRST tt_auth_limit NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth_limit NO-LOCK).
    
    { mip/inc/mipcatcherror.i 
      &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}

  END METHOD.  /* getRecordlimit  */
  
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
    DEFINE BUFFER btt_auth_limit FOR tt_auth_limit.
    
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation Limit not in focus'" }
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth_limit EXCLUSIVE-LOCK
           WHERE btt_auth_limit.auth_limit_obj = AuthLimitObj.
           
      ASSIGN btt_auth_limit.record_action = "delete":U.     
    
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
    
    DATASET dsAuthorisation:EMPTY-DATASET().
    
    CREATE tt_auth_limit.

    ASSIGN
       tt_auth_limit.record_action           = "Modify":U
       tt_auth_limit.line_number             = LineNumber 
       tt_auth_limit.auth_obj                = AuthObj              
       tt_auth_limit.auth_limit_obj          = AuthLimitObj        
       tt_auth_limit.claim_code              = ClaimCode           
       tt_auth_limit.claim_type              = ClaimType           
       tt_auth_limit.dependant               = Dependant           
       tt_auth_limit.effective_date          = EffectiveDate       
       tt_auth_limit.fetch_date_time         = FetchDateTime       
       tt_auth_limit.limit_#                 = Limit#              
       tt_auth_limit.limit_avail             = LimitAvail
       tt_auth_limit.limit_used              = LimitUsed           
       tt_auth_limit.limit_reserved          = LimitReserved       
       tt_auth_limit.option_code             = OptionCode          
       tt_auth_limit.owning_entity_mnemonic  = OwningEntityMnemonic
       tt_auth_limit.owning_obj              = OwningObj           
       tt_auth_limit.owning_alt_value        = OwningAltValue      
       tt_auth_limit.rate_per                = RatePer .            
  
    VALIDATE tt_auth_limit.                 
    
&IF {&DBDFMA} >= 010195 &THEN                     
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).
&ENDIF    
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:

      IF AuthLimitObj <= 0.00
      THEN 
        FOR FIRST tt_auth_limit NO-LOCK
            WHERE tt_auth_limit.auth_obj                = AuthObj
              AND tt_auth_limit.effective_date          = EffectiveDate
              AND tt_auth_limit.claim_code              = ClaimCode
              AND tt_auth_limit.claim_type              = ClaimType
              AND tt_auth_limit.limit_#                 = Limit#
              AND tt_auth_limit.option_code             = OptionCode
              AND tt_auth_limit.dependant               = Dependant
              AND tt_auth_limit.owning_entity_mnemonic  = OwningEntityMnemonic
              AND tt_auth_limit.owning_obj              = OwningObj:
          
          ASSIGN AuthLimitObj = tt_auth_limit.auth_limit_obj.
                         
        END. /*FOR FIRST tt_auth_limit NO-LOCK:*/

      ASSIGN glSuccess = focusRecord(AuthLimitObj).
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
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
       AuthObj                 = 0.00
       AuthLimitObj            = 0.00
       ClaimCode               = 0
       ClaimType               = "":U
       Dependant               = 0
       EffectiveDate           = ?
       FetchDateTime           = ?
       Limit#                  = 0
       LimitAvail              = 0.00
       LimitUsed               = 0.00
       LimitReserved           = 0.00
       OptionCode              = 0
       OwningEntityMnemonic    = "":U
       OwningObj               = 0.00
       OwningAltValue          = "":U
       RatePer                 = "":U.

    
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisationlimit */






