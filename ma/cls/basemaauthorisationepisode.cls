/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Andrewd

      Purpose: Base Authorisation Episode Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthorisationepisode INHERITS cls.mip:

  { sysadmma.i } 

  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthds.i }
  
  DEFINE PUBLIC PROPERTY LineNumber     AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY InFocus        AS LOGICAL           NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY Dependant      AS INTEGER           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY EpisodeNum     AS CHARACTER         NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY AuthEpisodeObj AS DECIMAL           NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY MemNum         AS CHARACTER         NO-UNDO GET. SET.
                                                            
  DEFINE PUBLIC PROPERTY ErrorObject   AS cls.maerrorobject NO-UNDO GET. PRIVATE SET.
  
  
  DEFINE PRIVATE VARIABLE glSuccess AS LOGICAL NO-UNDO.


  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationepisode() :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationepisode */

                                                           
  /* -- CONSTRUCTOR ---------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthorisationepisode
    ( INPUT ipdAuthEpisodeObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
      Purpose:  
        Notes:  
  ---------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusRecord(ipdAuthEpisodeObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthorisationepisode */
  
                                                           
  /* -- DESTRUCTOR ----------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthorisationepisode() :
  /*--------------------------------------------------------------------
      Purpose:
        Notes:
  ---------------------------------------------------------------------- */

    IF VALID-OBJECT(ErrorObject) THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthorisationepisode */

  
  /* -- METHOD focusRecord --------------------------------------------- */
  METHOD PUBLIC LOGICAL focusRecord
    ( INPUT ipdAuthEpisodeObj AS DECIMAL ) :
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  ---------------------------------------------------------------------- */
   
    ASSIGN InFocus        = NOT unfocusRecord()
           
           AuthEpisodeObj = ipdAuthEpisodeObj
           
           InFocus        = getRecordDetails().
           
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
    
    
    IF AuthEpisodeObj <> 0.00 THEN
    DO:
      oSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).
      
      ASSIGN glSuccess = oSearch:SetCriteria("BufferList":U, "tt_auth,tt_auth_episode":U)
      
             glSuccess = oSearch:SetFilterCriteria("tt_auth_episode.auth_episode_obj":U, "=":U, AuthEpisodeObj).
             
      oSearch:fetchData().             
      
    END. /*IF AuthEpisodeObj <> 0.00 THEN*/
    
  
    FOR FIRST tt_auth_episode NO-LOCK:
    
      ASSIGN
         Dependant      = tt_auth_episode.dependant  
         EpisodeNum     = tt_auth_episode.episode_num
         AuthEpisodeObj = tt_auth_episode.auth_episode_obj
         MemNum         = tt_auth_episode.mem_num.                 
          
    END. /*FOR FIRST tt_auth_episode NO-LOCK:*/
    
                                     
    RETURN CAN-FIND(FIRST tt_auth_episode NO-LOCK).
    
                                          
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
    DEFINE BUFFER btt_auth_episode FOR tt_auth_episode.
    
      
    IF NOT InFocus 
    THEN
      { mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Authorisation savings not in focus'" }
      
      
    DO TRANSACTION ON ERROR UNDO, THROW:
    
      FIND FIRST btt_auth_episode EXCLUSIVE-LOCK
           WHERE btt_auth_episode.auth_episode_obj = AuthEpisodeObj.
           
      ASSIGN btt_auth_episode.record_action = "delete":U.     
    
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
    
      CREATE tt_auth_episode.
      
      ASSIGN
         tt_auth_episode.line_number      = LineNumber
         tt_auth_episode.record_action    = "Modify":U
         tt_auth_episode.dependant        = Dependant   
         tt_auth_episode.episode_num      = EpisodeNum  
         tt_auth_episode.auth_episode_obj = AuthEpisodeObj  
         tt_auth_episode.mem_num          = MemNum                  
         .            
      
      VALIDATE tt_auth_episode.                 
    
    END. /*DO TRANSACTION ON ERROR UNDO, THROW:*/
    
&IF {&DBDFMA} >= 010195 &THEN                     
    mipEnv:Health:AuthBusinessLogic:saveAuthorisation(INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE).
&ENDIF    
    
    IF NOT ErrorObject:ErrorsExist AND NOT ErrorObject:WarningsExist THEN
    DO:

      IF AuthEpisodeObj <= 0.00
      THEN
      FOR FIRST tt_auth_episode NO-LOCK
          WHERE tt_auth_episode.episode_num = EpisodeNum
            AND tt_auth_episode.mem_num     = MemNum
            AND tt_auth_episode.dependant   = Dependant:
        
        ASSIGN AuthEpisodeObj = tt_auth_episode.auth_episode_obj.
                       
      END. /*FOR FIRST tt_auth_episode NO-LOCK:*/

      ASSIGN glSuccess = focusRecord(AuthEpisodeObj).
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
       InFocus        = FALSE
       LineNumber     = 0
       Dependant      = 0
       EpisodeNum     = "":U
       AuthEpisodeObj = 0.00
       MemNum         = "":U.                    
     
          
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusRecord  */
    
END CLASS.  /* mip.cls.basemaauthorisationepisode */







