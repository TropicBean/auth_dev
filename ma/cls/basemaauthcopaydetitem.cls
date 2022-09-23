/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Miranda

      Purpose: Auth Copay Detail Item Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthcopaydetitem INHERITS cls.mip:

  { sysadmma.i } 
  
  { mip/inc/mipdefsharedclass.i }

&IF {&DBDFMA} >= 010195 &THEN
  
  { ma/inc/maauthcopaydetitemds.i }
  
 
  DEFINE PRIVATE VARIABLE glSuccess                    AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthCopayDetailItemDataset AS HANDLE  NO-UNDO.
  
  DEFINE PUBLIC PROPERTY AuthCopayDetailItemInFocus    AS LOGICAL           GET. PRIVATE SET. 
  
  DEFINE PUBLIC PROPERTY LineNumber                    AS INTEGER           GET. SET.

  DEFINE PUBLIC PROPERTY AuthCopayDetailItemObj        AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthCopayTypeObj              AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY ApplyToEmergency              AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ApplyToPMB                    AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY CopaymentValue                AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY CopaymentValueType            AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate                 AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                       AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj                    AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY OptionCode                    AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY OwningAltValue                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OwningEntityMnemonic          AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OwningKey                     AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY OwningObj                     AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY PrType                        AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY WarningMessage                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY WarningMessageType            AS CHARACTER         GET. SET.
                                                    
&ENDIF
  
  DEFINE PUBLIC PROPERTY ErrorObject                   AS cls.maerrorobject GET. PRIVATE SET.
                                                    
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaydetitem() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaydetitem */

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaydetitem
    ( INPUT ipdAuthCopayDetailItemObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthCopayDetailItem(ipdAuthCopayDetailItemObj).

    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaydetitem */
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthcopaydetitem
    ( INPUT ipdInsurerObj    AS DECIMAL,
      INPUT ipiOptionCode    AS INTEGER,
      INPUT ipcOEM           AS CHARACTER,
      INPUT ipdOwningObj     AS DECIMAL,
      INPUT ipcOwningKey     AS CHARACTER,
      INPUT ipiPrType        AS INTEGER,
      INPUT ipdEffectiveDate AS DATE ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    initializeObject().
    
    THIS-OBJECT:focusAuthCopayDetailItem(ipdInsurerObj,
                                         ipiOptionCode,
                                         ipcOEM,
                                         ipdOwningObj,
                                         ipcOwningKey,
                                         ipiPrType,
                                         ipdEffectiveDate).

    { mip/inc/mipcatcherror.i }
                              
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaydetitem */
  
  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthcopaydetitem() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN    
    IF VALID-HANDLE( ghAuthCopayDetailItemDataset  ) THEN DELETE OBJECT ghAuthCopayDetailItemDataset.
&ENDIF

    IF VALID-OBJECT( ErrorObject )                   THEN DELETE OBJECT ErrorObject.
    
  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthcopaydetitem */

  
  /* -- METHOD focusAuthCopayDetailItem ------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthCopayDetailItem
    ( INPUT ipdAuthCopayDetailItemObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth copay detail item with the specified 
                    obj value and populate the instance properties 
                    with the relevant values.
        Parameters: Auth Copay Detail Item obj
  -------------------------------------------------------------------- */
&IF {&DBDFMA} >= 010195 &THEN
    ASSIGN
        glSuccess                = unfocusAuthCopayDetailItem()    /* Clear any properties */
        
        AuthCopayDetailItemObj   = ipdAuthCopayDetailItemObj
        
        glSuccess                = getAuthCopayDetailItemDetails() /* Retrieve all data for the specified obj 
                                                                      and populate instance properties */                       
        AuthCopayDetailItemInFocus = glSuccess.
        
    RETURN AuthCopayDetailItemInFocus.    
        
    { mip/inc/mipcatcherror.i }

&ENDIF
  
  END METHOD.  /* focusAuthCopayDetailItem */
  
  /* -- METHOD focusAuthCopayDetailItem ------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthCopayDetailItem
    ( INPUT ipdInsurerObj    AS DECIMAL,
      INPUT ipiOptionCode    AS INTEGER,
      INPUT ipcOEM           AS CHARACTER,
      INPUT ipdOwningObj     AS DECIMAL,
      INPUT ipcOwningKey     AS CHARACTER,
      INPUT ipiPrType        AS INTEGER,
      INPUT ipdEffectiveDate AS DATE ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth copay detail item with the specified 
                    parameters and populate the instance properties 
                    with the relevant values.
        Parameters: 1. Insurer Obj 
                    2. Option Code
                    3. Owning Entity Mnemonic
                    4. Owning Obj
                    5. Owning Key
                    6. PrType
                    7. Effective Date        
  -------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN
    
  ASSIGN
        glSuccess                = unfocusAuthCopayDetailItem()    /* Clear any properties */
                                 
        InsurerObj               = ipdInsurerObj 
        OptionCode               = ipiOptionCode 
        OwningEntityMnemonic     = ipcOEM 
        OwningObj                = ipdOwningObj
        OwningKey                = ipcOwningKey
        PrType                   = ipiPrType 
        EffectiveDate            = ipdEffectiveDate
                                 
        glSuccess                = getAuthCopayDetailItemDetails() /* Retrieve all data for the specified auth copay detail item and populate instance properties */
        
        AuthCopayDetailItemInFocus = glSuccess.
        
    RETURN AuthCopayDetailItemInFocus.    
        
    { mip/inc/mipcatcherror.i }

&ENDIF

  END METHOD.  /* focusAuthCopayDetailItem */

  
  /* -- METHOD getAuthCopayDetailItemDetails ------------------------- */
  METHOD PRIVATE LOGICAL getAuthCopayDetailItemDetails(  ) :
  /*------------------------------------------------------------------
        Purpose   : Internal method for retrieving auth copay detail 
                    item data using the auth copay detail item data 
                    retrieval services.
        Parameters: None                   
  -------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN
  
    DEFINE VARIABLE oSearchObject AS cls.maauthcopaydetitemsearch NO-UNDO.
    
    DATASET dsAuthCopayDetailItem:EMPTY-DATASET().

    oSearchObject = NEW cls.maauthcopaydetitemsearch(DATASET dsAuthCopayDetailItem BY-REFERENCE).
    
    ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_copay_detail_item":U).
    
    IF AuthCopayDetailItemObj <> 0 
    THEN  
      ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.auth_copay_detail_item_obj":U, "=":U, AuthCopayDetailItemObj).
    ELSE DO:
      IF InsurerObj <> 0 
      THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.insurer_obj":U, "=":U, InsurerObj).
      
      IF OptionCode <> 0 THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.option_code":U, "=":U, OptionCode).
      
      IF OwningEntityMnemonic <> "" THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.owning_entity_mnemonic":U, "=":U, OwningEntityMnemonic).
      
      IF OwningObj <> 0 THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.owning_obj":U, "=":U, OwningObj).
      
      IF OwningKey <> "":U THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.owning_key":U, "=":U, OwningKey).
      
      IF OwningAltValue <> "":U THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.owning_alt_value":U, "=":U, OwningAltValue).
      
      IF PrType <> 0 THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.pr_type":U, "=":U, PrType).
      
      IF EffectiveDate <> ? THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.effective_date":U, "<=":U, EffectiveDate).
      
      IF EndDate <> ? THEN
        ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_copay_detail_item.end_date":U, ">=":U, EndDate).
    END.  /* IF AuthCopayDetailItemObj <> 0 THEN */

    oSearchObject:fetchdata().
      
    FOR FIRST tt_auth_copay_detail_item NO-LOCK:
      ASSIGN 
         AuthCopayDetailItemObj  = tt_auth_copay_detail_item.auth_copay_detail_item_obj
         InsurerObj              = tt_auth_copay_detail_item.insurer_obj
         OptionCode              = tt_auth_copay_detail_item.option_code
         OwningEntityMnemonic    = tt_auth_copay_detail_item.owning_entity_mnemonic
         OwningObj               = tt_auth_copay_detail_item.owning_obj
         OwningKey               = tt_auth_copay_detail_item.owning_key
         OwningAltValue          = tt_auth_copay_detail_item.owning_alt_value
         PrType                  = tt_auth_copay_detail_item.pr_type 
         EffectiveDate           = tt_auth_copay_detail_item.effective_date 
         AuthCopayTypeObj        = tt_auth_copay_detail_item.auth_copay_type_obj 
         CopaymentValueType      = tt_auth_copay_detail_item.copayment_value_type
         CopaymentValue          = tt_auth_copay_detail_item.copayment_value               
         ApplyToPMB              = tt_auth_copay_detail_item.apply_to_pmb
         ApplyToEmergency        = tt_auth_copay_detail_item.apply_to_emergency
         OwningAltValue          = tt_auth_copay_detail_item.owning_alt_value
         WarningMessage          = tt_auth_copay_detail_item.warning_message     
         WarningMessageType      = tt_auth_copay_detail_item.warning_message_type
         EndDate                 = tt_auth_copay_detail_item.end_date
        NO-ERROR.      
    END. /* FOR FIRST tt_auth_copay_detail_item NO-LOCK: */
    
    RETURN CAN-FIND(FIRST tt_auth_copay_detail_item NO-LOCK).
    
    { mip/inc/mipcatcherror.i  &FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

&ENDIF

  END METHOD.  /* getAuthCopayDetailItemDetails */
  
                        
  /* -- METHOD initializeObject -------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None                   
  -------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    ASSIGN 
       ghAuthCopayDetailItemDataset = DATASET dsAuthCopayDetailItem:HANDLE
       
       ErrorObject                  = NEW cls.maerrorobject(TEMP-TABLE tt_auth_copay_detail_item_error:HANDLE).
       
    { mip/inc/mipcatcherror.i }

&ENDIF 

  END METHOD.  /* initializeObject */
     

  /* -- METHOD removeAuthCopayDetailItem ---------------------------- */
  METHOD PUBLIC LOGICAL removeAuthCopayDetailItem (  ) :
  /*------------------------------------------------------------------
        Purpose   : Removes the focused auth copay detail item record.
        Parameters: None                   
  -------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN        
    
    DEFINE BUFFER btt_auth_copay_detail_item FOR tt_auth_copay_detail_item.
    
    IF NOT AuthCopayDetailItemInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth copay detail item not in focus'"}
      
    FIND FIRST btt_auth_copay_detail_item EXCLUSIVE-LOCK
         WHERE btt_auth_copay_detail_item.auth_copay_detail_item_obj = AuthCopayDetailItemObj.

    ASSIGN btt_auth_copay_detail_item.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthCopayDetailItem(INPUT-OUTPUT DATASET dsAuthCopayDetailItem BY-REFERENCE).
    
    RETURN NOT ErrorObject:ErrorsExist.
    
    { mip/inc/mipcatcherror.i }

&ENDIF
  
  END METHOD.  /* initializeObject */

  /* -- METHOD saveAuthCopayDetailItem ------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthCopayDetailItem() :
  /*------------------------------------------------------------------
        Purpose   : Create/Update auth copay detail item record using 
                    the instance properties specified.  
        Parameters: None                   
  -------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN      
  
    DATASET dsAuthCopayDetailItem:EMPTY-DATASET().
    
    CREATE tt_auth_copay_detail_item.
    
    ASSIGN 
       tt_auth_copay_detail_item.line_number                = LineNumber
       tt_auth_copay_detail_item.record_action              = "MODIFY":U
       tt_auth_copay_detail_item.auth_copay_detail_item_obj = AuthCopayDetailItemObj
       tt_auth_copay_detail_item.insurer_obj                = InsurerObj
       tt_auth_copay_detail_item.effective_date             = EffectiveDate
       tt_auth_copay_detail_item.end_date                   = EndDate
       tt_auth_copay_detail_item.auth_copay_type_obj        = AuthCopayTypeObj
       tt_auth_copay_detail_item.option_code                = OptionCode
       tt_auth_copay_detail_item.owning_entity_mnemonic     = OwningEntityMnemonic
       tt_auth_copay_detail_item.owning_obj                 = OwningObj
       tt_auth_copay_detail_item.owning_key                 = OwningKey
       tt_auth_copay_detail_item.owning_alt_value           = OwningAltValue
       tt_auth_copay_detail_item.pr_type                    = PrType
       tt_auth_copay_detail_item.copayment_value_type       = CopaymentValueType 
       tt_auth_copay_detail_item.copayment_value            = CopaymentValue     
       tt_auth_copay_detail_item.apply_to_pmb               = ApplyToPMB       
       tt_auth_copay_detail_item.apply_to_emergency         = ApplyToEmergency 
       tt_auth_copay_detail_item.warning_message            = WarningMessage     
       tt_auth_copay_detail_item.warning_message_type       = WarningMessageType.

    VALIDATE tt_auth_copay_detail_item.

    mipEnv:Health:AuthMaintenance:saveAuthCopayDetailItem(INPUT-OUTPUT DATASET dsAuthCopayDetailItem BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_copay_detail_item NO-LOCK:
        ASSIGN 
           AuthCopayDetailItemObj = tt_auth_copay_detail_item.auth_copay_detail_item_obj
           glSuccess              = THIS-OBJECT:focusAuthCopayDetailItem(AuthCopayDetailItemObj).
      END. /* FOR FIRST tt_auth_copay_detail_item NO-LOCK: */
    END. /* IF NOT ErrorObject:ErrorsExist THEN */

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }

&ENDIF    

  END METHOD.  /* saveAuthCopayDetailItem */
  
  
  /* -- METHOD unfocusAuthCopayDetailItem ---------------------------- */
  METHOD PUBLIC LOGICAL unfocusAuthCopayDetailItem() :
  /*------------------------------------------------------------------
        Purpose: Clearsall instance properties and empty the dataset.           
  -------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthCopayDetailItem:EMPTY-DATASET().
    
    ErrorObject:clearErrors().

    ASSIGN 
       AuthCopayDetailItemInFocus  = FALSE
       LineNumber                  = 0
       AuthCopayDetailItemObj      = 0.00
       InsurerObj                  = 0.00
       EffectiveDate               = ?
       EndDate                     = ?
       AuthCopayTypeObj            = 0.00
       OptionCode                  = 0
       OwningEntityMnemonic        = "":U
       OwningObj                   = 0.00
       OwningKey                   = "":U
       OwningAltValue              = "":U
       PrType                      = 0
       CopaymentValueType          = ?
       CopaymentValue              = 0.00
       ApplyToPMB                  = ?
       ApplyToEmergency            = ?
       WarningMessage              = "":U
       WarningMessageType          = "":U.
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

&ENDIF
    
  END METHOD.  /* unfocusAuthCopayDetailItem */    
    
END CLASS.  /* mip.cls.basemaauthcopaydetitem */






