/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Andrewd

      Purpose: Base Authorisation Coding Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisationcoding INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
                                                     
  DEFINE PUBLIC PROPERTY RecordAction                AS CHARACTER  INITIAL 'MODIFY' NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY InFocus                     AS LOGICAL                     NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY LineNumber                  AS INTEGER                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AssDiagObj                  AS DECIMAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthCodingObj               AS DECIMAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthObj                     AS DECIMAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CodingStatus                AS INTEGER                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CodingStatusNote            AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY CodingStatusNoteDescription AS CHARACTER                   NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY CodingType                  AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                     AS DATE                        NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MorphDiagObj                AS DECIMAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningAltValue              AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic        AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningKey                   AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj                   AS DECIMAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PmbIndicator                AS LOGICAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY PrimaryCode                 AS LOGICAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedAltValue             AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedEntityMnemonic       AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedKey                  AS CHARACTER                   NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY RelatedObj                  AS DECIMAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY StartDate                   AS DATE                        NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY ProcedureDate               AS DATE                        NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MainCode                    AS LOGICAL                     NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY BodyRegion                  AS CHARACTER                   NO-UNDO GET. SET.
                                                    
  DEFINE PUBLIC PROPERTY ErrorObject                 AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.


  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = "tt_temp_error" }
  
  
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationcoding() :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationcoding */

                                                           
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationcoding
    ( INPUT ipdAuthCodingObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthCodingObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationcoding */
  
                                                           
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisationcoding() :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisationcoding */

  
  /* -- METHOD focusRecord --------------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthCodingObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
   
    ASSIGN InFocus       = NOT unfocusRecord()
           
           AuthCodingObj = ipdAuthCodingObj
           
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
    DEFINE VARIABLE oSearch AS cls.maauthsearch NO-UNDO.
    
                                           
    DATASET dsAuthorisation:EMPTY-DATASET().
    
    
    IF AuthCodingObj <> 0.00 THEN
    DO:
      oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_coding":U)
      
             glSuccess = oSearch:SetFilterCriteria("tt_auth_coding.auth_coding_obj":U, "=":U, AuthCodingObj).
             
      oSearch:fetchData().             
      
    END. /*IF AuthCodingObj <> 0.00 THEN*/
    
  
    FOR FIRST tt_auth_coding NO-LOCK
        WHERE tt_auth_coding.auth_coding_obj = AuthCodingObj:
    
      ASSIGN
         AssDiagObj            = tt_auth_coding.ass_diag_obj           
         AuthCodingObj         = tt_auth_coding.auth_coding_obj        
         AuthObj               = tt_auth_coding.auth_obj               
         CodingStatus          = tt_auth_coding.coding_status          
         CodingStatusNote      = tt_auth_coding.coding_status_note     
         CodingType            = tt_auth_coding.coding_type            
         EndDate               = tt_auth_coding.end_date               
         MorphDiagObj          = tt_auth_coding.morph_diag_obj         
         OwningAltValue        = tt_auth_coding.owning_alt_value       
         OwningEntityMnemonic  = tt_auth_coding.owning_entity_mnemonic 
         OwningKey             = tt_auth_coding.owning_key             
         OwningObj             = tt_auth_coding.owning_obj          
         PmbIndicator          = tt_auth_coding.pmb_indicator          
         PrimaryCode           = tt_auth_coding.primary_code           
         RelatedAltValue       = tt_auth_coding.related_alt_value      
         RelatedEntityMnemonic = tt_auth_coding.related_entity_mnemonic
         RelatedKey            = tt_auth_coding.related_key            
         RelatedObj            = tt_auth_coding.related_obj            
         StartDate             = tt_auth_coding.start_date
         ProcedureDate         = tt_auth_coding.procedure_date
         MainCode              = tt_auth_coding.main_code
         BodyRegion            = tt_auth_coding.body_region
         .                                      
          
      FIND FIRST note NO-LOCK
           WHERE note.key = CodingStatusNote
        NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE note
      THEN ASSIGN CodingStatusNoteDescription = note.narration[1].
          
    END. /*FOR FIRST tt_auth_coding NO-LOCK:*/
    
                                     
    RETURN CAN-FIND(FIRST tt_auth_coding NO-LOCK).
    
                                          
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
    DEFINE BUFFER btt_auth_coding FOR tt_auth_coding.
    
      
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation coding not in focus'" }
      
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth_coding EXCLUSIVE-LOCK
           WHERE btt_auth_coding.auth_coding_obj = AuthCodingObj.
           
      ASSIGN 
         RecordAction                  = 'delete'
         btt_auth_coding.record_action = RecordAction.     
    
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
    
    
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      CREATE tt_auth_coding.
      
      ASSIGN
         tt_auth_coding.line_number              = LineNumber
         tt_auth_coding.record_action            = RecordAction
         tt_auth_coding.ass_diag_obj             = AssDiagObj           
         tt_auth_coding.auth_coding_obj          = AuthCodingObj        
         tt_auth_coding.auth_obj                 = AuthObj              
         tt_auth_coding.coding_status            = CodingStatus         
         tt_auth_coding.coding_status_note       = CodingStatusNote     
         tt_auth_coding.coding_type              = CodingType           
         tt_auth_coding.end_date                 = EndDate             
         tt_auth_coding.morph_diag_obj           = MorphDiagObj         
         tt_auth_coding.owning_alt_value         = OwningAltValue       
         tt_auth_coding.owning_entity_mnemonic   = OwningEntityMnemonic 
         tt_auth_coding.owning_key               = OwningKey            
         tt_auth_coding.owning_obj               = OwningObj            
         tt_auth_coding.pmb_indicator            = PmbIndicator         
         tt_auth_coding.primary_code             = PrimaryCode          
         tt_auth_coding.related_alt_value        = RelatedAltValue      
         tt_auth_coding.related_entity_mnemonic  = RelatedEntityMnemonic
         tt_auth_coding.related_key              = RelatedKey           
         tt_auth_coding.related_obj              = RelatedObj           
         tt_auth_coding.start_date               = StartDate 
         tt_auth_coding.procedure_date           = ProcedureDate
         tt_auth_coding.main_code                = MainCode
         tt_auth_coding.body_region              = BodyRegion
         .            
      
      VALIDATE tt_auth_coding.                 
    
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

      IF AuthCodingObj <= 0.00 
      THEN
      FOR FIRST tt_auth_coding NO-LOCK
          WHERE tt_auth_coding.auth_obj                = AuthObj
            AND tt_auth_coding.owning_entity_mnemonic  = OwningEntityMnemonic
            AND tt_auth_coding.owning_obj              = OwningObj
            AND tt_auth_coding.owning_key              = OwningKey
            AND tt_auth_coding.related_entity_mnemonic = RelatedEntityMnemonic
            AND tt_auth_coding.related_obj             = RelatedObj
            AND tt_auth_coding.related_key             = RelatedKey
            AND tt_auth_coding.start_date              = StartDate:
        
        ASSIGN AuthCodingObj = tt_auth_coding.auth_coding_obj.
      END. /*FOR FIRST tt_auth_coding NO-LOCK:*/
  
      ASSIGN glSuccess = focusRecord(AuthCodingObj).
      
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    
    /*
      Copy warnings back to the dataset error temp table
    */
    IF TEMP-TABLE tt_temp_error:HAS-RECORDS
    THEN TEMP-TABLE tt_auth_error:COPY-TEMP-TABLE(TEMP-TABLE tt_temp_error:HANDLE).
    
        
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* saveRecord  */
  
  
  /* -- METHOD unfocusRecord ------------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusRecord() :
  /*-------------------------------------------------------------------
        Purpose:  
          Notes:
  --------------------------------------------------------------------- */
    
    ErrorObject:ClearErrors().
    
    
    ASSIGN                        
       RecordAction                = 'Modify'
       LineNumber                  = 0
       InFocus                     = FALSE
       AssDiagObj                  = 0.00                          
       AuthCodingObj               = 0.00                          
       AuthObj                     = 0.00                          
       CodingStatus                = 0                             
       CodingStatusNote            = "":U                          
       CodingType                  = "":U                          
       EndDate                     = ?                             
       MorphDiagObj                = 0.00                          
       OwningAltValue              = "":U                          
       OwningEntityMnemonic        = "":U                          
       OwningKey                   = "":U                          
       OwningObj                   = 0.00                          
       PmbIndicator                = FALSE                          
       PrimaryCode                 = FALSE                          
       RelatedAltValue             = "":U                          
       RelatedEntityMnemonic       = "":U                          
       RelatedKey                  = "":U                          
       RelatedObj                  = 0.00                          
       StartDate                   = ? 
       ProcedureDate               = ?
       MainCode                    = FALSE
       CodingStatusNoteDescription = "":U
       BodyRegion                  = "":U
       .                            
       
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisationcoding */






