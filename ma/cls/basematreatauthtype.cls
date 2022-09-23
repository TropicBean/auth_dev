/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandla Msibi

      Purpose: Base Treatment Auth Type Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basematreatauthtype INHERITS cls.mip:

  { sysadmma.i } 
  
  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/matreatauthtypeds.i }
  
  
  DEFINE PRIVATE VARIABLE glSuccess              AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghTreatAuthTypeDataset AS HANDLE  NO-UNDO.
  
  DEFINE PUBLIC PROPERTY TreatAuthTypeInFocus    AS LOGICAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY TreatAuthTypeObj        AS DECIMAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthTypePrefix          AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY Description             AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY SequenceKey             AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY TreatAuthType           AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj              AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY ErrorObject             AS cls.maerrorobject GET. PRIVATE SET.
  
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basematreatauthtype() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basematreatauthtype */

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basematreatauthtype
    ( INPUT ipdTreatAuthTypeObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusTreatAuthType(ipdTreatAuthTypeObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basematreatauthtype */
  
                                                           
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basematreatauthtype() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
    
    IF VALID-HANDLE( ghTreatAuthTypeDataset  ) THEN DELETE OBJECT ghTreatAuthTypeDataset.
    IF VALID-OBJECT( ErrorObject )             THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basematreatauthtype */

  
  /* -- METHOD focusTreatAuthType ------------------------------------ */
  METHOD PUBLIC LOGICAL focusTreatAuthType
    ( INPUT ipdTreatAuthTypeObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find a treament plan with the specified obj 
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Treatment Auth Type obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess            = unfocusTreatAuthType()    /*Clear any properties*/
        
        TreatAuthTypeObj     = ipdTreatAuthTypeObj
        
        glSuccess            = getTreatAuthTypeDetails()
        
        TreatAuthTypeInFocus = glSuccess.
        
    RETURN TreatAuthTypeInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusTreatAuthType  */
  
  /* -- METHOD focusTreatAuthType ------------------------------------ */
  METHOD PUBLIC LOGICAL focusTreatAuthType
    ( INPUT ipdInsurerObj    AS DECIMAL,
      INPUT ipcTreatAuthType AS CHARACTER ) :
  /*------------------------------------------------------------------
        Purpose   : Find a treament plan with the specified auth type 
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Treatment Auth Type 
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess            = unfocusTreatAuthType()    /*Clear any properties*/
        
        InsurerObj           = ipdInsurerObj
        TreatAuthType        = ipcTreatAuthType
        
        glSuccess            = getTreatAuthTypeDetails()
        
        TreatAuthTypeInFocus = glSuccess.
        
    RETURN TreatAuthTypeInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusTreatAuthType  */
  
  /* -- METHOD getTreatAuthTypeDetails --------------------------------- */
  METHOD PRIVATE LOGICAL getTreatAuthTypeDetails(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing treatment auth type data
                    using the treatment plan data retrieval services.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS ma.cls.matreatauthtypesearch NO-UNDO.
    
    DATASET dsTreatAuthType:EMPTY-DATASET().
    
    IF TreatAuthTypeObj <> 0.00 OR TreatAuthType <> "":U THEN
    DO:
      oSearchObject = NEW ma.cls.matreatauthtypesearch(DATASET dsTreatAuthType:HANDLE).
     
      /*We arent interested in all of the buffers that have been defined in the treat auth type dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_treat_auth_type":U).
      
      IF TreatAuthTypeObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_treat_auth_type.treat_auth_type_obj":U, "=":U, TreatAuthTypeObj).
      
      IF InsurerObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_treat_auth_type.insurer_obj":U, "=":U, InsurerObj).
      
      IF TreatAuthType <> "":U 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_treat_auth_type.treat_auth_type":U, "=":U, TreatAuthType).
      
      oSearchObject:fetchTreatAuthTypeData().
           
    END. /*IF TreatmentPlanObj <> 0.00 */
    
    FOR FIRST tt_treat_auth_type NO-LOCK:
    
      ASSIGN 
         AuthTypePrefix   = tt_treat_auth_type.auth_type_prefix    
         Description      = tt_treat_auth_type.description
         SequenceKey      = tt_treat_auth_type.sequence_key
         TreatAuthType    = tt_treat_auth_type.treat_auth_type
         TreatAuthTypeObj = tt_treat_auth_type.treat_auth_type_obj
         InsurerObj       = tt_treat_auth_type.insurer_obj.
               
    END. /*FOR FIRST tt_treatplan NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_treat_auth_type NO-LOCK).
    
    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getTreatAuthTypeDetails  */
  
   /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None                   
  ---------------------------------------------------------------------- */

    ASSIGN 
       ghTreatAuthTypeDataset = DATASET dsTreatAuthType:HANDLE
       ErrorObject            = NEW cls.maerrorobject(TEMP-TABLE tt_treat_auth_type_error:HANDLE).
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */
  
  
  /* -- METHOD removeTreatAuthType ------------------------------------- */
  METHOD PUBLIC LOGICAL removeTreatAuthType (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused treatment plan record.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE BUFFER btt_treat_auth_type FOR tt_treat_auth_type.
    
    
    IF NOT TreatAuthTypeInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Treatment auth type not in focus'"}
    
    FIND FIRST btt_treat_auth_type EXCLUSIVE-LOCK
         WHERE btt_treat_auth_type.treat_auth_type_obj = TreatAuthTypeObj.
      
    ASSIGN btt_treat_auth_type.record_action = "DELETE":U.
    
    mipEnv:Health:maCondition:saveTreatAuthType(INPUT-OUTPUT DATASET dsTreatAuthType BY-REFERENCE).
    
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* removeTreatAuthType  */
                                    

  /* -- METHOD saveTreatmentPlan --------------------------------------- */
  METHOD PUBLIC LOGICAL saveTreatAuthType() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update treatment record using the instance 
                    properties specified.  
        Parameters: None                   
  ---------------------------------------------------------------------- */
    
    DATASET dsTreatAuthType:EMPTY-DATASET().
    
    CREATE tt_treat_auth_type.
    
    ASSIGN 
       tt_treat_auth_type.treat_auth_type_obj = TreatAuthTypeObj
       tt_treat_auth_type.record_action       = "MODIFY":U
       tt_treat_auth_type.auth_type_prefix    = AuthTypePrefix        
       tt_treat_auth_type.description         = Description
       tt_treat_auth_type.sequence_key        = SequenceKey
       tt_treat_auth_type.treat_auth_type     = TreatAuthType
       tt_treat_auth_type.insurer_obj         = InsurerObj.        
    
    VALIDATE tt_treat_auth_type.
    
    mipEnv:Health:maCondition:saveTreatAuthType(INPUT-OUTPUT DATASET dsTreatAuthType BY-REFERENCE) NO-ERROR.
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_treat_auth_type NO-LOCK:
        ASSIGN 
           TreatAuthTypeObj = tt_treat_auth_type.treat_auth_type_obj
           glSuccess        = THIS-OBJECT:focusTreatAuthType(TreatAuthTypeObj).
      END. /*FOR FIRST tt_treat_auth_type NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* saveTreatAuthType  */
  
  
  /* -- METHOD unfocusTreatAuthType ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusTreatAuthType() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the 
                 treatment plan dataset.           
  ---------------------------------------------------------------------- */

    DATASET dsTreatAuthType:EMPTY-DATASET().
    
    ErrorObject:clearErrors().
    
    ASSIGN 
       TreatAuthTypeInFocus = FALSE  
       AuthTypePrefix       = "":U   
       Description          = "":U   
       SequenceKey          = "":U
       TreatAuthType        = "":U      
       TreatAuthTypeObj     = 0.00
       InsurerObj           = 0.00.   
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusTreatAuthType  */
   
END CLASS.    /* ma.cls.basematreatauthtype */  


