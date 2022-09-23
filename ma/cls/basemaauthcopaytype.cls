/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: GrahamW

      Purpose: Base Adjustment Type Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthcopaytype INHERITS cls.mip:

  { sysadmma.i } 
  
  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthcopaytypeds.i }
  
  DEFINE PRIVATE VARIABLE glSuccess               AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthCopayTypeDataset  AS HANDLE  NO-UNDO.
  
  DEFINE PUBLIC PROPERTY AuthCopayTypeInFocus     AS LOGICAL           GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthCopayTypeObj         AS DECIMAL           GET. PRIVATE SET.
                                                                      
  DEFINE PUBLIC PROPERTY AuthCopayType            AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY Description              AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY LineNumber               AS INTEGER           GET. SET.
  
  DEFINE PUBLIC PROPERTY ErrorObject              AS cls.maerrorobject GET. PRIVATE SET.

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaytype() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaytype */

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaytype
    ( INPUT ipdAuthCopayTypeObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthCopayType(ipdAuthCopayTypeObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaytype */
  
                                                           
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthcopaytype() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
    
    IF VALID-HANDLE( ghAuthCopayTypeDataset  ) THEN DELETE OBJECT ghAuthCopayTypeDataset.
    IF VALID-OBJECT( ErrorObject )             THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthcopaytype */


  /* -- METHOD focusAuthCopayType ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthCopayType
    ( INPUT ipdAuthCopayTypeObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an Adjustment Type with the specified obj 
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Adjustment Type obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess             = unfocusAuthCopayType()    /*Clear any properties*/
                             
        AuthCopayTypeObj     = ipdAuthCopayTypeObj
                             
        glSuccess             = getAuthCopayTypeDetails() /*Retrieve all data for the specified obj 
                                                             and populate instance properties*/                       
        AuthCopayTypeInFocus = glSuccess.
        
    RETURN AuthCopayTypeInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthCopayType  */
  
  
  /* -- METHOD getAuthCopayTypeDetails --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthCopayTypeDetails(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing Adjustment Type data
                    using the Adjustment Type data retrieval services.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthcopaytypesearch NO-UNDO.                              
    
    DATASET dsAuthCopayType:EMPTY-DATASET().
    
    IF AuthCopayTypeObj <> 0.00 THEN
    DO:
      oSearchObject = NEW cls.maAuthCopayTypesearch(DATASET dsAuthCopayType BY-REFERENCE).
      
      /*We arent interested in all of the buffers that have been defined in the Authorisation Copay Type dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_copay_type":U).
      
      IF AuthCopayTypeObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_type.auth_copay_type_obj":U, "=":U, AuthCopayTypeObj).
      
      oSearchObject:fetchData().   
           
    END. /*IF AuthCopayTypeObj <> 0.00 */
    
    FOR FIRST tt_auth_copay_type NO-LOCK:
    
      ASSIGN 
         AuthCopayType    = tt_auth_copay_type.auth_copay_type
         Description      = tt_auth_copay_type.description
         LineNumber       = tt_auth_copay_type.line_number.

    END. /*FOR FIRST tt_auth_copay_type NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth_copay_type NO-LOCK).
    
    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthCopayTypeDetails  */
  
                        
  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None                   
  ---------------------------------------------------------------------- */

    ASSIGN 
       ghAuthCopayTypeDataset = DATASET dsAuthCopayType:HANDLE
       
       ErrorObject            = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_type_error:HANDLE).
       
  { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */
  
  
  /* -- METHOD removeAuthCopayType --=--------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthCopayType (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused Adjustment Type record.
        Parameters: None                   
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN     
    DEFINE BUFFER btt_auth_copay_type FOR tt_auth_copay_type.
    
    IF NOT AuthCopayTypeInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'AuthCopayType not in focus'"}
    
    FIND FIRST btt_auth_copay_type EXCLUSIVE-LOCK
         WHERE btt_auth_copay_type.auth_copay_type_obj = AuthCopayTypeObj.
      
    ASSIGN btt_auth_copay_type.record_action = "DELETE":U.
    
    mipEnv:Health:AuthMaintenance:saveAuthCopayType(INPUT-OUTPUT DATASET dsAuthCopayType BY-REFERENCE).
          
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }
    
&ENDIF
  END METHOD.  /* initializeObject  */
                                    

  /* -- METHOD saveAuthCopayType -------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthCopayType() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update Auth Copay Type record using the instance 
                    properties specified.  
        Parameters: None                   
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN            
    
    DATASET dsAuthCopayType:EMPTY-DATASET().
    
    CREATE tt_auth_copay_type.
    
    ASSIGN 
       tt_auth_copay_type.record_action   = "MODIFY":U
       tt_auth_copay_type.line_number     = LineNumber
       tt_auth_copay_type.auth_copay_type_obj = AuthCopayTypeObj
       tt_auth_copay_type.auth_copay_type     = AuthCopayType   
       tt_auth_copay_type.description     = Description.
    
    VALIDATE tt_auth_copay_type.
    
    mipEnv:Health:AuthMaintenance:saveAuthCopayType(INPUT-OUTPUT DATASET dsAuthCopayType BY-REFERENCE).
        
    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_copay_type NO-LOCK:
        ASSIGN 
           AuthCopayTypeObj = tt_auth_copay_type.auth_copay_type_obj
           glSuccess        = THIS-OBJECT:focusAuthCopayType(AuthCopayTypeObj).
      END. /*FOR FIRST tt_auth_copay_type NO-LOCK:*/
      
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
    
&ENDIF    

  END METHOD.  /* saveAuthCopayType */
  
  
  /* -- METHOD unfocusAuthCopayType ----------------------------------- */
  METHOD PUBLIC LOGICAL unfocusAuthCopayType() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the 
                 Adjustment Type dataset.           
  ---------------------------------------------------------------------- */

    DATASET dsAuthCopayType:EMPTY-DATASET().
    
    ErrorObject:clearErrors().
    
    ASSIGN 
       AuthCopayTypeInFocus = FALSE   
       AuthCopayTypeObj     = 0.00  
       AuthCopayType        = "":U
       Description          = "":U.
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusAuthCopayType  */    
    
END CLASS.  /* mip.cls.basemaauthcopaytype */
