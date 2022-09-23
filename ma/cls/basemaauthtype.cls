/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Kati

      Purpose: Auth Type Instance Class
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthtype INHERITS cls.mip:

  { sysadmma.i }

  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthtypeds.i }
                                                  
                                                  
  DEFINE PRIVATE VARIABLE glSuccess                          AS LOGICAL NO-UNDO.
  DEFINE PRIVATE VARIABLE ghAuthTypeDataset                  AS HANDLE  NO-UNDO.
                                                             
  DEFINE PUBLIC PROPERTY AuthTypeInFocus                     AS LOGICAL           GET. PRIVATE SET.
                                                             
  DEFINE PUBLIC PROPERTY LineNumber                          AS INTEGER           GET. SET.
  DEFINE PUBLIC PROPERTY AuthType                            AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeObj                         AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypePrefix                      AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY Description                         AS CHARACTER         GET. SET.  
  DEFINE PUBLIC PROPERTY ClaimCodeUpdateRoles                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimCodeUpdateUsers                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimTypeUpdateRoles                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ClaimTypeUpdateUsers                AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatusUpdateRoles        AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultAuthStatus                   AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultStatusNote                   AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY InsurerObj                          AS DECIMAL           GET. SET.
  DEFINE PUBLIC PROPERTY SequenceKey                         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY EffectiveDate                       AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY EndDate                             AS DATE              GET. SET.
  DEFINE PUBLIC PROPERTY ExternalAuthTypeGroup               AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY IcdCondCode                         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY IcdCondType                         AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ValidIcds                           AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY UpdatesAllowed                      AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY DefaultStatusUpdateUsers            AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY DefaultLineRestriction              AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY HeaderValuesAllowed                 AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY HeaderValuesUnlimited               AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateAmPm                        AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateBodyRegion                  AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateCodeLink                    AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateCopayment                   AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateCrosswalk                   AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateEpisodeNumber               AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateLos                         AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateLosWeekendPass              AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivateMouthPartId                 AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY ActivatePenalty                     AS LOGICAL           GET. SET.        
  DEFINE PUBLIC PROPERTY ActivateServiceType                 AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthoriseAllServices                AS LOGICAL           GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeGroup                       AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY EndDateUpdRole                      AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY EndDateUpdUser                      AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY ActivateHealth                      AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY MultipleCCMessageType               AS CHARACTER         GET. SET.
  DEFINE PUBLIC PROPERTY AuthTypeGroupLabel                  AS CHARACTER         GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY AuthTypeGroupValue                  AS CHARACTER         GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY ErrorObject                         AS cls.maerrorobject GET. PRIVATE SET.
    

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthtype() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtype */


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemaauthtype
    ( INPUT ipdAuthTypeObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    initializeObject().

    THIS-OBJECT:focusAuthType(ipdAuthTypeObj).

    { mip/inc/mipcatcherror.i }

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthtype */


  /* -- DESTRUCTOR --------------------------------------------------- */
  DESTRUCTOR PUBLIC basemaauthtype() :
  /*------------------------------------------------------------------
      Purpose:
        Notes:
  -------------------------------------------------------------------- */

    IF VALID-HANDLE( ghAuthTypeDataset  ) THEN DELETE OBJECT ghAuthTypeDataset.    
    IF VALID-OBJECT( ErrorObject )        THEN DELETE OBJECT ErrorObject.

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemaauthtype */


  /* -- METHOD focusAuthType ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthType
    ( INPUT ipdAuthTypeObj AS DECIMAL ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type with the specified obj
                    value and populate the instance properties with
                    the relevant values.
        Parameters: Auth type obj
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess       = unfocusAuthType()    /*Clear any properties*/

        AuthTypeObj     = ipdAuthTypeObj

        glSuccess       = THIS-OBJECT:getAuthType()  /* Retrieve all data for the specified obj
                                                        and populate instance properties*/
        AuthTypeInFocus = glSuccess.

    RETURN AuthTypeInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthType  */
  
  /* -- METHOD focusAuthType ------------------------------------ */
  METHOD PUBLIC LOGICAL focusAuthType
    (INPUT ipcAuthType      AS CHARACTER,
     INPUT ipdInsurerObj    AS DECIMAL,
     INPUT ipdEffectiveDate AS DATE) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth type with the specified auth type,
                    insurer obj and effective date and populate the 
                    instance properties with the relevant values.
        Parameters: Auth type, Insurer Obj and Date
  -------------------------------------------------------------------- */

    ASSIGN
        glSuccess       = unfocusAuthType()    /*Clear any properties*/

        AuthType        = ipcAuthType
        
        InsurerObj      = ipdInsurerObj
        
        EffectiveDate   = ipdEffectiveDate

        glSuccess       = THIS-OBJECT:getAuthType()  /* Retrieve all data for the specified auth type, insurer obj and effective date
                                                        and populate instance properties*/
        AuthTypeInFocus = glSuccess.

    RETURN AuthTypeInFocus.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* focusAuthType  */


  /* -- METHOD getAuthTypeDetails --------------------------------- */
  METHOD PRIVATE LOGICAL getAuthType( ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for retrieveing auth type data
                    using the auth type data retrieval services.
        Parameters: None
  ---------------------------------------------------------------------- */
    DEFINE VARIABLE oSearchObject AS cls.maauthtypesearch NO-UNDO.

    DATASET dsAuthType:EMPTY-DATASET().

    IF AuthTypeObj <> 0.00 OR 
       AuthType    <> "" 
    THEN DO:
      oSearchObject = NEW cls.maauthtypesearch(DATASET dsAuthType BY-REFERENCE).

      /*
        We arent interested in all of the buffers that have been defined in the auth type dataset
        so we are going to specify which ones should be filled.
      */
      ASSIGN glSuccess = oSearchObject:SetCriteria("BufferList":U, "tt_auth_type":U).

      IF AuthTypeObj <> 0.00 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type.auth_type_obj":U, "=":U, AuthTypeObj).

      IF AuthType <> "" 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type.auth_type":U, "=":U, AuthType).
      
      IF InsurerObj <> 0 
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type.insurer_obj":U, "=":U, InsurerObj).
      
      IF EffectiveDate <> ?
      THEN ASSIGN glSuccess = oSearchObject:SetFilterCriteria("tt_auth_type.effective_date":U, "<=":U, EffectiveDate).
      
      oSearchObject:fetchData().

    END. /*IF AuthTypeObj <> 0.00 */

    FOR FIRST tt_auth_type EXCLUSIVE-LOCK:

      ASSIGN
         AuthType                      = tt_auth_type.auth_type                                     
         AuthTypeObj                   = tt_auth_type.auth_type_obj                            
         AuthTypePrefix                = tt_auth_type.auth_type_prefix                        
         Description                   = tt_auth_type.description                           
         AuthTypeGroup                 = tt_auth_type.auth_type_group    
         ClaimCodeUpdateRoles          = tt_auth_type.claim_code_upd_role
         ClaimCodeUpdateUsers          = tt_auth_type.claim_code_upd_user
         ClaimTypeUpdateRoles          = tt_auth_type.claim_type_upd_role
         ClaimTypeUpdateUsers          = tt_auth_type.claim_type_upd_user
         DefaultAuthStatusUpdateRoles  = tt_auth_type.default_auth_status_upd_role
         DefaultAuthStatus             = tt_auth_type.default_auth_status                     
         DefaultStatusNote             = tt_auth_type.default_auth_status_note                           
         InsurerObj                    = tt_auth_type.insurer_obj                     
         SequenceKey                   = tt_auth_type.sequence_key                             
         EffectiveDate                 = tt_auth_type.effective_date                                 
         EndDate                       = tt_auth_type.end_date                         
         ExternalAuthTypeGroup         = tt_auth_type.external_auth_type_group           
         IcdCondCode                   = tt_auth_type.icd_cond_code                    
         IcdCondType                   = tt_auth_type.icd_cond_type                              
         ValidIcds                     = tt_auth_type.valid_icds                                 
         UpdatesAllowed                = tt_auth_type.updates_allowed                             
         DefaultStatusUpdateUsers      = tt_auth_type.default_auth_status_upd_user                  
         DefaultLineRestriction        = tt_auth_type.default_line_restriction               
         HeaderValuesAllowed           = tt_auth_type.header_values_allowed                        
         HeaderValuesUnlimited         = tt_auth_type.header_values_unlimited   
         MultipleCCMessageType         = tt_auth_type.multiple_cc_message_type
         ActivateBodyRegion            = tt_auth_type.activate_body_region 
         ActivateCodeLink              = tt_auth_type.activate_code_link
         ActivateCopayment             = tt_auth_type.activate_copayment
         ActivateCrosswalk             = tt_auth_type.activate_crosswalk
         ActivateHealth                = tt_auth_type.activate_health
         ActivateEpisodeNumber         = tt_auth_type.activate_episode_number                      
         ActivateMouthPartId           = tt_auth_type.activate_mouth_part_id   
         ActivatePenalty               = tt_auth_type.activate_penalty
         ActivateLos                   = tt_auth_type.activate_los
         ActivateLosWeekendPass        = tt_auth_type.activate_los_weekend_pass
         ActivateServiceType           = tt_auth_type.activate_service_type
         ActivateAmPm                  = tt_auth_type.activate_am_pm       
         AuthoriseAllServices          = tt_auth_type.authorise_all_services                   
         EndDateUpdUser                = tt_auth_type.end_date_upd_user    
         EndDateUpdRole                = tt_auth_type.end_date_upd_role.
      
      FIND FIRST mic_acronym NO-LOCK 
           WHERE mic_acronym.acronym_key = AuthTypeGroup
        NO-ERROR.
      
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }
      
      IF AVAILABLE mic_acronym
      THEN
        ASSIGN AuthTypeGroupValue = mic_acronym.acronym_value
               AuthTypeGroupLabel = mic_acronym.acronym_label.
               
    END. /*FOR FIRST tt_auth_type NO-LOCK:*/

    RETURN CAN-FIND(FIRST tt_auth_type NO-LOCK).

    { mip/inc/mipcatcherror.i  FINALLY="IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}

  END METHOD.  /* getAuthTypeDetails  */


  /* -- METHOD initializeObject ---------------------------------------- */
  METHOD PRIVATE VOID initializeObject(  ) :
  /*--------------------------------------------------------------------
        Purpose   : Internal method for configuration required by the object.
                    Set the global dataset and ErrorObject.
        Parameters: None
  ---------------------------------------------------------------------- */

    ASSIGN
       ghAuthTypeDataset  = DATASET dsAuthType:HANDLE

       ErrorObject        = NEW cls.maerrorobject(TEMP-TABLE tt_auth_type_error:HANDLE).

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* initializeObject  */


  /* -- METHOD removeAuthType ------------------------------------------ */
  METHOD PUBLIC LOGICAL removeAuthType (  ) :
  /*--------------------------------------------------------------------
        Purpose   : Removes the focused auth type record.
        Parameters: None
  ---------------------------------------------------------------------- */
  
&IF {&DBDFMA} >= 010195 &THEN

    DEFINE BUFFER btt_auth_type FOR tt_auth_type.


    IF NOT AuthTypeInFocus THEN
      {mip/inc/mipthrowerror.i 'ma_MsgError':U ? ? ? "'Auth type not in focus'"}

    FIND FIRST btt_auth_type EXCLUSIVE-LOCK
         WHERE btt_auth_type.auth_type_obj = AuthTypeObj.

    ASSIGN btt_auth_type.record_action = "DELETE":U.

    mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE).

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* removeAuthType  */


  /* -- METHOD saveAuthType -------------------------------------------- */
  METHOD PUBLIC LOGICAL saveAuthType() :
  /*--------------------------------------------------------------------
        Purpose   : Create/Update auth type record using the instance
                    properties specified.
        Parameters: None
  ---------------------------------------------------------------------- */

&IF {&DBDFMA} >= 010195 &THEN

    DATASET dsAuthType:EMPTY-DATASET().

    CREATE tt_auth_type.

    ASSIGN
       tt_auth_type.line_number                  = LineNumber
       tt_auth_type.record_action                = "MODIFY":U
       tt_auth_type.auth_type                    = AuthType
       tt_auth_type.auth_type_obj                = AuthTypeObj
       tt_auth_type.auth_type_prefix             = AuthTypePrefix
       tt_auth_type.description                  = Description
       tt_auth_type.auth_type_group              = AuthTypeGroup
       tt_auth_type.claim_code_upd_role          = ClaimCodeUpdateRoles        
       tt_auth_type.claim_code_upd_user          = ClaimCodeUpdateUsers        
       tt_auth_type.claim_type_upd_role          = ClaimTypeUpdateRoles        
       tt_auth_type.claim_type_upd_user          = ClaimTypeUpdateUsers        
       tt_auth_type.default_auth_status_upd_role = DefaultAuthStatusUpdateRoles
       tt_auth_type.default_auth_status          = DefaultAuthStatus
       tt_auth_type.default_auth_status_note     = DefaultStatusNote
       tt_auth_type.insurer_obj                  = InsurerObj
       tt_auth_type.sequence_key                 = SequenceKey
       tt_auth_type.effective_date               = EffectiveDate
       tt_auth_type.end_date                     = EndDate
       tt_auth_type.external_auth_type_group     = ExternalAuthTypeGroup
       tt_auth_type.icd_cond_code                = IcdCondCode
       tt_auth_type.icd_cond_type                = IcdCondType
       tt_auth_type.valid_icds                   = ValidIcds
       tt_auth_type.updates_allowed              = UpdatesAllowed
       tt_auth_type.default_auth_status_upd_user = DefaultStatusUpdateUsers
       tt_auth_type.default_line_restriction     = DefaultLineRestriction
       tt_auth_type.header_values_allowed        = HeaderValuesAllowed
       tt_auth_type.header_values_unlimited      = HeaderValuesUnlimited
       tt_auth_type.multiple_cc_message_type     = MultipleCCMessageType
       tt_auth_type.activate_am_pm               = ActivateAmPm
       tt_auth_type.activate_body_region         = ActivateBodyRegion
       tt_auth_type.activate_code_link           = ActivateCodeLink
       tt_auth_type.activate_copayment           = ActivateCopayment
       tt_auth_type.activate_crosswalk           = ActivateCrosswalk
       tt_auth_type.activate_episode_number      = ActivateEpisodeNumber
       tt_auth_type.activate_health              = ActivateHealth
       tt_auth_type.activate_mouth_part_id       = ActivateMouthPartId
       tt_auth_type.activate_penalty             = ActivatePenalty
       tt_auth_type.activate_los                 = ActivateLos
       tt_auth_type.activate_los_weekend_pass    = ActivateLosWeekendPass
       tt_auth_type.activate_service_type        = ActivateServiceType
       tt_auth_type.authorise_all_services       = AuthoriseAllServices
       tt_auth_type.end_date_upd_user            = EndDateUpdUser
       tt_auth_type.end_date_upd_role            = EndDateUpdRole.

    VALIDATE tt_auth_type.
    
    mipEnv:Health:AuthMaintenance:saveAuthType(INPUT-OUTPUT DATASET dsAuthType BY-REFERENCE) NO-ERROR.

    IF NOT ErrorObject:ErrorsExist THEN
    DO:
      FOR FIRST tt_auth_type NO-LOCK:
        ASSIGN
           AuthTypeObj = tt_auth_type.auth_type_obj
           glSuccess   = THIS-OBJECT:focusAuthType(AuthTypeObj).
      END. /*FOR FIRST tt_auth_type NO-LOCK:*/
    END. /*IF NOT ErrorObject:ErrorsExist THEN*/

    RETURN NOT ErrorObject:ErrorsExist.

    { mip/inc/mipcatcherror.i }
&ENDIF

  END METHOD.  /* saveAuthType  */


  /* -- METHOD unfocusAuthType ------------------------------------ */
  METHOD PUBLIC LOGICAL unfocusAuthType() :
  /*--------------------------------------------------------------------
        Purpose: Clears all instance properties and empties the dataset.
  ---------------------------------------------------------------------- */

    DATASET dsAuthType:EMPTY-DATASET().

    ErrorObject:clearErrors().

    ASSIGN
       AuthTypeInFocus              = FALSE
       LineNumber                   = 0
       AuthTypeObj                  = 0.00
       InsurerObj                   = 0.00
       EffectiveDate                = ?
       EndDate                      = ?
       Description                  = "":U
       AuthType                     = "":U
       AuthTypePrefix               = "":U
       AuthTypeGroup                = "":U
       AuthTypeGroupLabel           = "":U
       AuthTypeGroupValue           = "":U
       ClaimCodeUpdateRoles         = "":U
       ClaimCodeUpdateUsers         = "":U
       ClaimTypeUpdateRoles         = "":U
       ClaimTypeUpdateUsers         = "":U
       DefaultAuthStatusUpdateRoles = "":U
       DefaultAuthStatus            = "":U
       DefaultStatusNote            = "":U
       InsurerObj                   = 0.00
       SequenceKey                  = "":U
       ExternalAuthTypeGroup        = "":U
       IcdCondCode                  = "":U
       IcdCondType                  = "":U
       ValidIcds                    = "":U
       UpdatesAllowed               = FALSE
       DefaultStatusUpdateUsers     = "":U
       DefaultLineRestriction       = "":U
       HeaderValuesAllowed          = "":U
       MultipleCCMessageType        = "":U
       HeaderValuesUnlimited        = FALSE
       ActivateAmPm                 = FALSE       
       ActivateBodyRegion           = FALSE
       ActivateCodeLink             = FALSE
       ActivateCopayment            = FALSE
       ActivateCrosswalk            = FALSE
       ActivateEpisodeNumber        = FALSE
       ActivateHealth               = "":U
       ActivateLos                  = FALSE
       ActivateLosWeekendPass       = FALSE
       ActivatePenalty              = FALSE
       ActivateMouthPartId          = FALSE
       ActivateServiceType          = FALSE
       AuthoriseAllServices         = FALSE
       EndDateUpdUser               = "":U    
       EndDateUpdRole               = "":U
       .
    
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* unfocusAuthType  */

END CLASS.  /* mip.cls.basemaauthtype */
