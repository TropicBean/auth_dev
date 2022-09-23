/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandlam

      Purpose: Base Authorisation Copay Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisationcopay INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  DEFINE PUBLIC PROPERTY InFocus                 AS LOGICAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY LineNumber              AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Amount                  AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY Amount%                 AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthObj                 AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthCopayObj            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthCopayTypeObj        AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic    AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj               AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningKey               AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningAltValue          AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CopayOverrideNote       AS CHARACTER         NO-UNDO GET. SET.
  
  DEFINE PUBLIC PROPERTY ErrorObject             AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.
 
 { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }

  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationcopay() :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationcopay */

  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationcopay
    ( INPUT ipdAuthCopayObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthCopayObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationcopay */
  
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisationcopay() :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisationcopay */

  /* -- METHOD focusRecord -------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthCopayObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
   
    ASSIGN InFocus      = NOT unfocusRecord()
           AuthCopayObj = ipdAuthCopayObj
           InFocus      = getRecordCopay().
           
    RETURN InFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusRecord  */
  

  /* -- METHOD getRecordCopay ---------------------------------------- */
  METHOD PRIVATE LOGICAL getRecordCopay
    (  ) :
  /*--------------------------------------------------------------------
        Purpose: Internal method for retrieveing authorisation data
                 using the authorisation data retrieval services.
          Notes:
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearch   AS cls.maauthsearch NO-UNDO.
    
    DATASET dsAuthorisation:EMPTY-DATASET().
        
    IF AuthCopayObj <> 0.00 THEN
    DO:
      oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_copay":U)
             glSuccess = oSearch:SetFilterCriteria("tt_auth_copay.auth_copay_obj":U, "=":U, AuthCopayObj).
             
      oSearch:fetchData().            
      
    END. /*IF AuthObj <> 0.00 THEN*/
    
    FOR FIRST tt_auth_copay NO-LOCK:

      ASSIGN
         LineNumber           = tt_auth_copay.line_number
         Amount               = tt_auth_copay.amount                
         Amount%              = tt_auth_copay.amount_%              
         AuthObj              = tt_auth_copay.auth_obj              
         AuthCopayObj         = tt_auth_copay.auth_copay_obj        
         AuthCopayTypeObj     = tt_auth_copay.auth_copay_type_obj   
         OwningEntityMnemonic = tt_auth_copay.owning_entity_mnemonic
         OwningObj            = tt_auth_copay.owning_obj            
         OwningKey            = tt_auth_copay.owning_key            
         OwningAltValue       = tt_auth_copay.owning_alt_value
         CopayOverrideNote    = tt_auth_copay.copay_override_note . 
         
    END. /*FOR FIRST tt_auth_copay NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth_copay NO-LOCK).
    
    { mip/inc/mipcatcherror.i 
      &FINALLY = "IF VALID-OBJECT(oSearch) THEN DELETE OBJECT oSearch."}

  END METHOD.  /* getRecordCopay  */
  
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
    DEFINE BUFFER btt_auth_copay FOR tt_auth_copay.
    
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation Copay not in focus'" }
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth_copay EXCLUSIVE-LOCK
           WHERE btt_auth_copay.auth_copay_obj = AuthCopayObj.
           
      ASSIGN btt_auth_copay.record_action = "delete":U.     
    
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
    
    CREATE tt_auth_copay.

    ASSIGN
       tt_auth_copay.record_action           = "Modify":U
       tt_auth_copay.line_number             = LineNumber 
       tt_auth_copay.amount                  = Amount              
       tt_auth_copay.amount_%                = Amount%             
       tt_auth_copay.auth_obj                = AuthObj             
       tt_auth_copay.auth_copay_obj          = AuthCopayObj        
       tt_auth_copay.auth_copay_type_obj     = AuthCopayTypeObj    
       tt_auth_copay.owning_entity_mnemonic  = OwningEntityMnemonic
       tt_auth_copay.owning_obj              = OwningObj           
       tt_auth_copay.owning_key              = OwningKey           
       tt_auth_copay.owning_alt_value        = OwningAltValue
       tt_auth_copay.copay_override_note     = CopayOverrideNote.
                   
    VALIDATE tt_auth_copay.                 
    
&IF {&DBDFMA} >= 010195 &THEN                     
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).
&ENDIF    
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:

      IF AuthCopayObj <= 0.00
      THEN 
        FOR FIRST tt_auth_copay NO-LOCK
            WHERE tt_auth_copay.auth_obj                    = AuthObj
              AND tt_auth_copay.auth_copay_type_obj         = AuthCopayTypeObj
              AND tt_auth_copay.owning_entity_mnemonic      = OwningEntityMnemonic
              AND tt_auth_copay.owning_obj                  = OwningObj        
              AND tt_auth_copay.owning_key                  = OwningKey:
          
          ASSIGN AuthCopayObj = tt_auth_copay.auth_copay_obj.
                         
        END. /*FOR FIRST tt_auth_copay NO-LOCK:*/

      ASSIGN glSuccess = focusRecord(AuthCopayObj).
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
       Amount                  = 0.00  
       Amount%                 = 0.00
       AuthObj                 = 0.00
       AuthCopayObj            = 0.00
       AuthCopayTypeObj        = 0.00
       OwningEntityMnemonic    = "":U
       OwningObj               = 0.00
       OwningKey               = "":U
       OwningAltValue          = "":U
       CopayOverrideNote       = "":U.    
    
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisationcopay */





