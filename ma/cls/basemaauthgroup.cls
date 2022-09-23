
/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Andrewd

      Purpose: Base Auto Group Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthgroup INHERITS cls.mip:

  { sysadmma.i } 
  
  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthgroupds.i }
  
 
  DEFINE PRIVATE VARIABLE glSuccess          AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthGroupDataset AS HANDLE  NO-UNDO.
  
  DEFINE PUBLIC PROPERTY AuthGroupInFocus    AS LOGICAL   GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthGroupObj        AS DECIMAL   GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthGroupCode       AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY Description         AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate       AS DATE      GET. SET.
  DEFINE PUBLIC PROPERTY EndDate             AS DATE      GET. SET.
  DEFINE PUBLIC PROPERTY LineNumber          AS INTEGER   GET. SET.
  DEFINE PUBLIC PROPERTY OptionCode          AS INTEGER   GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj          AS DECIMAL   GET. SET.
  
  DEFINE PUBLIC PROPERTY ErrorObject         AS cls.maerrorobject GET. PRIVATE SET.
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthgroup() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthgroup */

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthgroup
    ( INPUT ipdAuthGroupObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthGroup(ipdAuthGroupObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthgroup */
  
                                                           
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthgroup() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
    
    IF VALID-HANDLE( ghAuthGroupDataset  ) THEN DELETE OBJECT ghAuthGroupDataset.
    IF VALID-OBJECT( ErrorObject )         THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthgroup */

  /* -- METHOD focusAuthGroup ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthGroup
    ( INPUT ipdAuthGroupObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find a treament plan with the specified obj 
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Treatment plan obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess        = unfocusAuthGroup()    /*Clear any properties*/
        
        AuthGroupObj     = ipdAuthGroupObj
        
        glSuccess        = getAuthGroupDetails() /*Retrieve all data for the specified obj 
                                                   and populate instance properties*/                       
        AuthGroupInFocus = glSuccess.
        
    RETURN AuthGroupInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthGroup  */
  

  /* -- METHOD getAuthGroupDetails --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthGroupDetails(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing treatment plan data
                    using the treatment plan data retrieval services.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthgroupsearch NO-UNDO.
    
    
    DATASET dsAuthGroup:EMPTY-DATASET().
    
    IF AuthGroupObj <> 0.00 THEN
    DO:
      oSearchObject = NEW cls.maauthgroupsearch(DATASET dsAuthGroup BY-REFERENCE).
      
      /*We arent interested in all of the buffers that have been defined in the treatment plan dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_group":U).
      
      IF AuthGroupObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_group.auth_group_obj":U, "=":U, AuthGroupObj).
      
      oSearchObject:fetchData().
           
    END. /*IF AuthGroupObj <> 0.00 */
    
    FOR FIRST tt_auth_group NO-LOCK:
    
      ASSIGN
         AuthGroupCode = tt_auth_group.auth_group_code
         LineNumber    = tt_auth_group.line_number
         Description   = tt_auth_group.description      
         EffectiveDate = tt_auth_group.effective_date   
         EndDate       = tt_auth_group.end_date    
         OptionCode    = tt_auth_group.option_code      
         InsurerObj    = tt_auth_group.insurer_obj.
         
               
    END. /*FOR FIRST tt_auth_group NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth_group NO-LOCK).
    
    { mip/inc/mipcatcherror.i  FINALLY = "IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthGroupDetails  */
  
                        
  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None                   
  ---------------------------------------------------------------------- */

    ASSIGN 
       ghAuthGroupDataset = DATASET dsAuthGroup:HANDLE
       
       ErrorObject        = NEW cls.maerrorobject(TEMP-TABLE tt_auth_group_error:HANDLE).
       
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */
  
  
  /* -- METHOD removeAuthGroup ------------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthGroup (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused treatment plan record.
        Parameters: None                   
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN        
    DEFINE BUFFER btt_auth_group FOR tt_auth_group.
    
    
    IF NOT AuthGroupInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth  group not in focus'"}
    
    FIND FIRST btt_auth_group EXCLUSIVE-LOCK
         WHERE btt_auth_group.auth_group_obj = AuthGroupObj.
      
    ASSIGN btt_auth_group.record_action = "DELETE":U.
    
    mipEnv:Health:AuthMaintenance:saveAuthGroup(INPUT-OUTPUT DATASET dsAuthGroup BY-REFERENCE).
    
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }
&ENDIF
  END METHOD.  /* initializeObject  */
                                    
  /* -- METHOD saveAuthGroup --------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthGroup() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update treatment record using the instance 
                    properties specified.  
        Parameters: None                   
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN        
    DATASET dsAuthGroup:EMPTY-DATASET().
    
    CREATE tt_auth_group.
    
    ASSIGN 
       tt_auth_group.auth_group_obj  = AuthGroupObj
       tt_auth_group.record_action   = "MODIFY":U
       tt_auth_group.line_number     = LineNumber
       tt_auth_group.auth_group_code = AuthGroupCode
       tt_auth_group.description     = Description     
       tt_auth_group.effective_date  = EffectiveDate   
       tt_auth_group.end_date        = EndDate         
       tt_auth_group.option_code     = OptionCode      
       tt_auth_group.insurer_obj     = InsurerObj.        
    
    VALIDATE tt_auth_group.
    
    mipEnv:Health:AuthMaintenance:saveAuthGroup(INPUT-OUTPUT DATASET dsAuthGroup BY-REFERENCE) NO-ERROR.
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_group NO-LOCK:
        ASSIGN 
           AuthGroupObj = tt_auth_group.auth_group_obj
           glSuccess    = THIS-OBJECT:focusAuthGroup(AuthGroupObj).
      END. /*FOR FIRST tt_auth_group NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF    
  END METHOD.  /* saveAuthGroup  */
  
  
  /* -- METHOD unfocusAuthGroup ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthGroup() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the 
                 treatment plan dataset.           
  ---------------------------------------------------------------------- */

    DATASET dsAuthGroup:EMPTY-DATASET().
    
    ErrorObject:clearErrors().
    
    ASSIGN 
       AuthGroupInFocus = FALSE  
       Description      = "":U   
       EffectiveDate    = ?      
       EndDate          = ?      
       OptionCode       = 0      
       AuthGroupObj     = 0.00   
       InsurerObj       = 0.00.   
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusAuthGroup  */    
    
END CLASS.  /* mip.cls.basemaAuthGroup */





