
/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Andrewd

      Purpose: Base Auth Group Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthgroupdetail INHERITS cls.mip:

  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }
  
  { ma/inc/maauthgroupds.i }
 
  DEFINE PRIVATE VARIABLE glSuccess                AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthGroupDetailDataset AS HANDLE  NO-UNDO.

  DEFINE PUBLIC PROPERTY AuthGroupDetailInFocus   AS LOGICAL   GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthGroupDetailObj       AS DECIMAL   GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic     AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY ClaimType                AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY OwningKey                AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY AltValue                 AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY PrType                   AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY BaseRate                 AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY ARSRAte                  AS CHARACTER GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate            AS DATE      GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                  AS DATE      GET. SET.
  DEFINE PUBLIC PROPERTY AuthGroupObj             AS DECIMAL   GET. SET.
  DEFINE PUBLIC PROPERTY Amount                   AS DECIMAL   GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj                AS DECIMAL   GET. SET.
  DEFINE PUBLIC PROPERTY Quantity                 AS INTEGER   GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCode                AS INTEGER   GET. SET.
  DEFINE PUBLIC PROPERTY SubPrType                AS INTEGER   GET. SET.
  DEFINE PUBLIC PROPERTY LineNumber               AS INTEGER   GET. SET.

  DEFINE PUBLIC PROPERTY ErrorObject              AS cls.maerrorobject GET. PRIVATE SET.

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthgroupdetail() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthgroupdetail */


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthgroupdetail
    ( INPUT ipdAuthGroupDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    THIS-OBJECT:focusAuthGroupDetail(ipdAuthGroupDetailObj).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthgroupdetail */


  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthgroupdetail() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthGroupDetailDataset  ) THEN DELETE OBJECT ghAuthGroupDetailDataset.
    IF VALID-OBJECT( ErrorObject )               THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthgroupdetail */

  /* -- METHOD focusAuthGroupDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthGroupDetail
    ( INPUT ipdAuthGroupDetailObj AS DECIMAL ):
  /*------------------------------------------------------------------
        Purpose   : Find a treament plan with the specified obj 
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Treatment plan obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess              = unfocusAuthGroupDetail()    /*Clear any properties*/
                               
        AuthGroupDetailObj     = ipdAuthGroupDetailObj
                               
        glSuccess              = getAuthGroupDetailDetails() /*Retrieve all data for the specified obj 
                                                               and populate instance properties*/                       
        AuthGroupDetailInFocus = glSuccess.
        
    RETURN AuthGroupDetailInFocus.    
        
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthGroupDetail  */
  

  /* -- METHOD getAuthGroupDetailDetails --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthGroupDetailDetails(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing treatment plan data
                    using the treatment plan data retrieval services.
        Parameters: None                   
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthgroupsearch NO-UNDO.
    
    
    DATASET dsAuthGroup:EMPTY-DATASET().
    
    IF AuthGroupDetailObj <> 0.00 THEN
    DO:
      oSearchObject = NEW cls.maauthgroupsearch(DATASET dsAuthGroup BY-REFERENCE) no-error.
      
      oSearchObject:SetCriteria("BufferList", "tt_auth_group,tt_auth_group_detail") no-error.
      
      /*We arent interested in all of the buffers that have been defined in the auth group plan dataset
        so we are going to specify which ones should be filled.*/
      ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_group_detail.auth_group_detail_obj":U, "=":U, AuthGroupDetailObj) no-error.
      
      oSearchObject:fetchData() no-error.
      
    END. /*IF AuthGroupDetailObj <> 0.00 */
    
    FOR FIRST tt_auth_group_detail NO-LOCK:
    
      ASSIGN
         AuthGroupObj         = tt_auth_group_detail.auth_group_obj
         OwningEntityMnemonic = tt_auth_group_detail.owning_entity_mnemonic
         OwningObj            = tt_auth_group_detail.owning_obj
         OwningKey            = tt_auth_group_detail.owning_key
         AltValue             = tt_auth_group_detail.owning_alt_value 
         PrType               = tt_auth_group_detail.pr_type
         Quantity             = tt_auth_group_detail.quantity
         Amount               = tt_auth_group_detail.amount
         ClaimCode            = tt_auth_group_detail.claim_code
         ClaimType            = tt_auth_group_detail.claim_type
         EffectiveDate        = tt_auth_group_detail.effective_date
         EndDate              = tt_auth_group_detail.end_date
         SubPrType            = tt_auth_group_detail.sub_pr_type
         BaseRate             = tt_auth_group_detail.base_rate
         ARSRate              = tt_auth_group_detail.ars_rate
         LineNumber           = tt_auth_group_detail.line_number.
    END. /*FOR FIRST tt_auth_group_group NO-LOCK:*/
    
    RETURN CAN-FIND(FIRST tt_auth_group_detail NO-LOCK).
    
    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getauthgroupdetailDetails  */
  
                        
  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None                   
  ---------------------------------------------------------------------- */

    ASSIGN 
       ghAuthGroupDetailDataset = DATASET dsAuthGroup:HANDLE
       
       ErrorObject              = NEW cls.maerrorobject(TEMP-TABLE tt_auth_group_error:HANDLE).

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */
  
  
  /* -- METHOD removeAuthGroupDetail ------------------------------------- */
  METHOD PUBLIC LOGICAL removeAuthGroupDetail (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused treatment plan record.
        Parameters: None                   
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN        
    DEFINE BUFFER btt_auth_group_detail FOR tt_auth_group_detail.
    
    
    IF NOT AuthGroupDetailInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth group not in focus'"}
    
    FIND FIRST btt_auth_group_detail EXCLUSIVE-LOCK
         WHERE btt_auth_group_detail.auth_group_detail_obj = AuthGroupDetailObj.
      
    ASSIGN btt_auth_group_detail.record_action = "DELETE":U.
    
    mipEnv:Health:AuthMaintenance:SaveAuthGroup(INPUT-OUTPUT DATASET dsAuthGroup BY-REFERENCE).
    
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }
&ENDIF
  END METHOD.  /* initializeObject  */
                                    

  /* -- METHOD saveAuthGroupDetail --------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthGroupDetail() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update treatment record using the instance 
                    properties specified.  
        Parameters: None                   
  ---------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN        
    DATASET dsAuthGroup:EMPTY-DATASET().
    
    CREATE tt_auth_group_detail.
    
    ASSIGN 
        tt_auth_group_detail.record_action          = "MODIFY"
        tt_auth_group_detail.auth_group_detail_obj  = AuthGroupDetailObj
        tt_auth_group_detail.auth_group_obj         = AuthGroupObj     
        tt_auth_group_detail.owning_entity_mnemonic = OwningEntityMnemonic 
        tt_auth_group_detail.owning_obj             = OwningObj            
        tt_auth_group_detail.owning_key             = OwningKey            
        tt_auth_group_detail.owning_alt_value       = AltValue             
        tt_auth_group_detail.pr_type                = PrType               
        tt_auth_group_detail.quantity               = Quantity             
        tt_auth_group_detail.amount                 = Amount               
        tt_auth_group_detail.claim_code             = ClaimCode            
        tt_auth_group_detail.claim_type             = ClaimType            
        tt_auth_group_detail.effective_date         = EffectiveDate        
        tt_auth_group_detail.end_date               = EndDate
        tt_auth_group_detail.sub_pr_type            = SubPrType
        tt_auth_group_detail.base_rate              = BaseRate
        tt_auth_group_detail.ars_rate               = ARSRate
        tt_auth_group_detail.line_number            = LineNumber.              
    
    VALIDATE tt_auth_group_detail.
    
    mipEnv:Health:AuthMaintenance:SaveAuthGroup(INPUT-OUTPUT DATASET dsAuthGroup BY-REFERENCE) NO-ERROR.
    
    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_group_detail NO-LOCK:
        ASSIGN 
           AuthGroupDetailObj = tt_auth_group_detail.auth_group_detail_obj
           glSuccess          = THIS-OBJECT:focusAuthGroupDetail(AuthGroupDetailObj).
      END. /*FOR FIRST tt_auth_group NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/
    
    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF    
  END METHOD.  /* saveAuthGroupDetail  */
  
  
  /* -- METHOD unfocusAuthGroupDetail ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthGroupDetail() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the 
                 treatment plan dataset.           
  ---------------------------------------------------------------------- */

    DATASET dsAuthGroup:EMPTY-DATASET().
    
    ErrorObject:clearErrors(). 
    
    ASSIGN 
       AuthGroupDetailInFocus = FALSE  
       AuthGroupObj           = 0
       OwningEntityMnemonic   = ""
       OwningObj              = 0
       OwningKey              = ""
       AltValue               = ""
       PrType                 = ""
       Quantity               = 0
       Amount                 = 0
       ClaimCode              = 0
       ClaimType              = ""
       EffectiveDate          = ? 
       EndDate                = ?
       SubPrType              = 0
       BaseRate               = ""
       ARSRate                = ""
      .                     
           
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD.  /* unfocusAuthGroupDetail  */    
    
END CLASS.  /* mip.cls.basemaauthgroupdetail */

