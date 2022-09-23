/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandlam

      Purpose: Base Managed Care Authorisation Savings Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisationmcsavings INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  DEFINE PUBLIC PROPERTY InFocus            AS LOGICAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthMCSavingsObj   AS DECIMAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY LineNumber         AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountActual       AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AmountBase         AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthObj            AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY SavingDate         AS DATE              NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY SavingStatus       AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY SavingStatusNote   AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY SavingType         AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY SavingsUserId      AS CHARACTER         NO-UNDO GET. SET.
  
  DEFINE PUBLIC PROPERTY ErrorObject        AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.

  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }
  
  
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationmcsavings() :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationmcsavings */

                                                           
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationmcsavings
    ( INPUT ipdAuthMCSavingsObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthMCSavingsObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationmcsavings */
  
                                                           
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisationmcsavings
    () :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisationmcsavings */

  
  /* -- METHOD focusRecord --------------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthMCSavingsObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
   
    ASSIGN InFocus          = NOT unfocusRecord()
           
           AuthMCSavingsObj = ipdAuthMCSavingsObj
           
           InFocus          = getRecordDetails().
           
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
    
    
    IF AuthMCSavingsObj <> 0.00 THEN
    DO:
      oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_mc_savings":U)
      
             glSuccess = oSearch:SetFilterCriteria("tt_auth_mc_savings.auth_mc_savings_obj":U, "=":U, AuthMCSavingsObj).
             
      oSearch:fetchData().             
      
    END. /*IF AuthMCSavingsObj <> 0.00 THEN*/
    
  
    FOR FIRST tt_auth_mc_savings NO-LOCK
        WHERE tt_auth_mc_savings.auth_mc_savings_obj = AuthMCSavingsObj:
    
      ASSIGN
         AmountActual     = tt_auth_mc_savings.amount_actual     
         AmountBase       = tt_auth_mc_savings.amount_base       
         AuthObj          = tt_auth_mc_savings.auth_obj          
         AuthMCSavingsObj = tt_auth_mc_savings.auth_mc_savings_obj  
         SavingDate       = tt_auth_mc_savings.saving_date       
         SavingStatus     = tt_auth_mc_savings.saving_status     
         SavingStatusNote = tt_auth_mc_savings.saving_status_note
         SavingType       = tt_auth_mc_savings.saving_type       
         SavingsUserId    = tt_auth_mc_savings.user_id.                 
          
    END. /*FOR FIRST tt_auth_mc_savings NO-LOCK:*/
    
                                     
    RETURN CAN-FIND(FIRST tt_auth_mc_savings NO-LOCK).
    
                                          
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
    DEFINE BUFFER btt_auth_mc_savings FOR tt_auth_mc_savings.
    
      
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation Managed Care savings not in focus'" }
      
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth_mc_savings EXCLUSIVE-LOCK
           WHERE btt_auth_mc_savings.auth_mc_savings_obj = AuthMCSavingsObj.
           
      ASSIGN btt_auth_mc_savings.record_action = "delete":U.     
    
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
    
    CREATE tt_auth_mc_savings.
      
    ASSIGN
       tt_auth_mc_savings.record_action       = "Modify":U
       tt_auth_mc_savings.line_number         = LineNumber
       tt_auth_mc_savings.amount_actual       = AmountActual    
       tt_auth_mc_savings.amount_base         = AmountBase      
       tt_auth_mc_savings.auth_obj            = AuthObj         
       tt_auth_mc_savings.auth_mc_savings_obj = AuthMCSavingsObj  
       tt_auth_mc_savings.saving_date         = SavingDate      
       tt_auth_mc_savings.saving_status       = SavingStatus    
       tt_auth_mc_savings.saving_status_note  = SavingStatusNote
       tt_auth_mc_savings.saving_type         = SavingType      
       tt_auth_mc_savings.user_id             = SavingsUserId.            
    
    VALIDATE tt_auth_mc_savings.                 
    
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      
      IF AuthMCSavingsObj <= 0.00
      THEN
      FOR FIRST tt_auth_mc_savings NO-LOCK
          WHERE tt_auth_mc_savings.auth_obj     = AuthObj
            AND tt_auth_mc_savings.saving_date = SavingDate
            AND tt_auth_mc_savings.saving_type = SavingType:

        ASSIGN AuthMCSavingsObj = tt_auth_mc_savings.auth_mc_savings_obj.
      END. /*FOR FIRST tt_auth_mc_savings NO-LOCK:*/

      ASSIGN glSuccess = focusRecord(AuthMCSavingsObj).
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* saveRecord  */
  
  
  /* -- METHOD unfocusRecord ------------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusRecord
    () :
  /*-------------------------------------------------------------------
        Purpose:  
          Notes:
  --------------------------------------------------------------------- */
    
    ErrorObject:ClearErrors().
    
    
    ASSIGN 
       InFocus            = FALSE
       LineNumber         = 0
       AmountActual       = 0.00        
       AmountBase         = 0.00        
       AuthObj            = 0.00        
       AuthMCSavingsObj   = 0.00        
       SavingDate         = ?           
       SavingStatus       = "":U       
       SavingStatusNote   = "":U        
       SavingType         = "":U        
       SavingsUserId      = "":U.                    
     
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisationmcsavings */







