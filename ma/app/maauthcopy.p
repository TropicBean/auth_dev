&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    $Id: procedur.p,v 1.2 2008/09/30 12:26:17 chrisk Exp $ 

    Purpose:
  ----------------------------------------------------------------------*/

/* ---------------------------  Definitions  -------------------------- */
{ mip/inc/mipdefshared.i  }

{ ma/inc/maauthcopytt.i }

{ ma/inc/masuccesstt.i &TEMP-TABLE-NAME = tt_auth_copy_success }

{ ma/inc/maauthds.i     }

{ ma/own/mammressend.i    }

DEFINE TEMP-TABLE tt_auth_copy_error NO-UNDO LIKE tt_auth_error
FIELD orig_auth_num AS CHARACTER
FIELD new_auth_num  AS CHARACTER.

DEFINE VARIABLE glSuccess                     AS LOGICAL          NO-UNDO.

DEFINE VARIABLE glcErrorMessage               AS LONGCHAR         NO-UNDO.

DEFINE STREAM strTTSBat.
DEFINE STREAM strTTErrors.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getFilePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilePath Procedure 
FUNCTION getFilePath RETURNS CHARACTER
  ( INPUT ipcCopyType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 20.29
         WIDTH              = 49.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
  DEFINE INPUT PARAMETER oJob    AS cls.mipjmjob          NO-UNDO.

  DEFINE VARIABLE cEmailAddress         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cJobReference         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFilePath             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAuthCopyEmailMessage AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCopyType             AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE tt_authcopy.

  CREATE tt_authcopy.

  ASSIGN
    cEmailAddress           = mipEnv:miUtility:getPreference("cm_PrefServInfoSMTPFrom":U)
    tt_authcopy.copy_type = oJob:JobRefs:getRefValue("TypeOfCopy":U).
    
  CASE tt_authcopy.copy_type:
  
    /* Getting copy parameters for the group copy procedure */
    WHEN "BATCH":U THEN
    DO:
      ASSIGN
        tt_authcopy.from_start_date =  DATE(oJob:JobRefs:getRefValue("CopyFromStartDate":U))
        tt_authcopy.from_end_date   =  DATE(oJob:JobRefs:getRefValue("CopyFromEndDate":U))
        tt_authcopy.auth_num        = "":U
        cCopyType                   = "BATCH":U.

    END. /* GROUP */

    /* Getting copy parameters for the year copy procedure */
    WHEN "SINGLE":U THEN
    DO:
      ASSIGN
        tt_authcopy.auth_num   = oJob:JobRefs:getRefValue("AuthNumber":U)
        cCopyType              = "SINGLE":U.

    END. /* YEAR */


  END CASE. /* CASE gcTypeOfCopy */

  ASSIGN
    tt_authcopy.to_start_date       =    DATE( oJob:JobRefs:getRefValue("CopyToStartDate":U))
    tt_authcopy.to_end_date         =    DATE( oJob:JobRefs:getRefValue("CopyToEndDate":U))
    tt_authcopy.option_code         = INTEGER( oJob:JobRefs:getRefValue("OptionCode":U))
    tt_authcopy.insurer_obj         = DECIMAL( oJob:JobRefs:getRefValue("InsurerObj":U))
    tt_authcopy.auth_type_list      =          oJob:JobRefs:getRefValue("AuthType":U)
    tt_authcopy.provider_info       = LOGICAL( oJob:JobRefs:getRefValue("CopyProviders":U))
    tt_authcopy.coding_icd          = LOGICAL( oJob:JobRefs:getRefValue("CopyICDs":U))
    tt_authcopy.coding_cpt          = LOGICAL( oJob:JobRefs:getRefValue("CopyCPTs":U))
    tt_authcopy.clinical_details    = LOGICAL( oJob:JobRefs:getRefValue("CopyDetails":U))
    tt_authcopy.flag_info           = LOGICAL( oJob:JobRefs:getRefValue("CopyFlags":U))
    tt_authcopy.external_ref        = LOGICAL( oJob:JobRefs:getRefValue("CopyReferences":U))
    tt_authcopy.job_reference       = oJob:JobReference.

  VALIDATE tt_authcopy.

  ASSIGN oJob:JobProgressDescription = "Authorisation copy service started":U.

  {ws/inc/wslog.i "'ma_MsgPerfInfo'" "'START Copy for Job ':U + oJob:JobReference"}
  
  CASE tt_authcopy.copy_type:
  
    /* Copies a rate group set, ie all tariff information to another rate group */
    WHEN "BATCH":U THEN
    DO:
      /* For filepath of error file */
      ASSIGN
        cFilePath = getFilePath("AuthBatchCopy_":U + STRING(oJob:JobReference)).

    END. /* WHEN "GROUP" */

    /* Copies a year set from one rate group to another rategroup */
    WHEN "SINGLE":U THEN
    DO:
      /* For filepath of error file */
      ASSIGN
        cFilePath = getFilePath("AuthSingleCopy_":U + STRING(oJob:JobReference)).
        
    END.

  END CASE. /* CASE tt_authcopy.copy_type: */
  
  mipEnv:Health:AuthService:copyAuth(INPUT-OUTPUT TABLE tt_authcopy, OUTPUT TABLE tt_auth_copy_error ).

  {ws/inc/wslog.i "'ma_MsgPerfInfo'" "'END Copy for Job ':U + oJob:JobReference"}
  
  ASSIGN 
    cJobReference = oJob:JobReference.

  IF CAN-FIND(FIRST tt_auth_copy_error) THEN 
  DO:
    IF cFilePath <> "ERROR":U THEN 
    DO:
      /* if any errors are returned create the file */
      OUTPUT STREAM strTTErrors TO VALUE(cFilePath).     
  
      EXPORT STREAM strTTErrors DELIMITER ",":U
        "Table Name"
        "Table Key"
        "Error Type"
        "Error Message"
        "Field Name".
  
      FOR EACH tt_auth_copy_error NO-LOCK:
  
        EXPORT STREAM strTTErrors DELIMITER ",":U
          tt_auth_copy_error.owning_entity_mnemonic
          tt_auth_copy_error.owning_key
          tt_auth_copy_error.error_type
          tt_auth_copy_error.error_message
          tt_auth_copy_error.error_field_name.

      END. /* FOR EACH tt_tariff_error NO-LOCK */ 
  
      OUTPUT STREAM strTTErrors CLOSE.
      
      ASSIGN
        ipcAction      = "EMAIL"
        ipcDescription = "The Following Error/s Occurred with the Authorisation Copy Job: " + cJobReference
        ipcResource    = ""
        ipcSendTo      = mipEnv:miUser:UserEmail
        ipcSendBody    = "Find attached a file containing the errors that occurred during processing"
        ipcSendAttch   = cFilePath
        ipcResGrp      = "mm_TmplCor"
        ipcResTempl    = "ma_mm_Cor"
        ipcResREM      = ""
        ipcResREF      = "".
        
      ASSIGN
        oJob:JobProgressDescription = "The Authorisation Copy Job: ":U + cJobReference + " completed with errors.":U.
    END. /* IF cFilePath <> ERROR */
    ELSE 
    DO: 
      ASSIGN
        ipcAction      = "EMAIL":U
        ipcDescription = "The Following Error/s Occurred with the Authorisation Copy Job: ":U + cJobReference
        ipcResource    = ""
        ipcSendTo      = mipEnv:miUser:UserEmail
        ipcSendBody    = "Errors occurred with the copy processing however a file path and a file name could not be found to print the errors to."
        ipcSendAttch   = "":U
        ipcResGrp      = "mm_TmplCor":U
        ipcResTempl    = "ma_mm_Cor":U
        ipcResREM      = "":U
        ipcResREF      = "":U
  
        oJob:JobProgressDescription = "The Authorisation Copy Job: ":U + cJobReference + " completed with errors":U.
  
    END. /* cFilePath = "ERROR" */
  END. /* IF CAN-FIND(FIRST tt_auth_copy_error) THEN  */
  ELSE
  DO:
    IF CAN-FIND(FIRST tt_auth_copy_success) THEN 
    DO:
    
      CASE cCopyType:
  
         /* Copies a rate group set, ie all tariff information to another rate group */
         WHEN "BATCH":U THEN
         DO:
           /* For filepath of error file */
           ASSIGN
             cAuthCopyEmailMessage = "The batch copy has run successfully, please check the batch success log file for all authorisations created"
             cFilePath = getFilePath("SuccessfulBatchCopy_":U + STRING(oJob:JobReference)).

           IF cFilePath <> "ERROR":U OR cFilePath <> "":U THEN 
           DO:
             /* if any errors are returned create the file */
             OUTPUT STREAM strTTSBat TO VALUE(cFilePath).     
           
             EXPORT STREAM strTTSBat DELIMITER ",":U
               "New Authoristion Number".
           
             FOR EACH tt_auth_copy_success NO-LOCK:
           
               EXPORT STREAM strTTSBat DELIMITER ",":U
                 tt_auth_copy_success.owning_key + " : " + tt_auth_copy_success.success_message
                 .
           
             END. /* FOR EACH tt_tariff_error NO-LOCK */ 
           
             OUTPUT STREAM strTTSBat CLOSE.
           END. /* IF cFilePath <> ERROR */
         
         END. /* WHEN "BATCH" */
         
         /* Copies a year set from one rate group to another rategroup */
         WHEN "SINGLE":U THEN
         DO:

           FIND FIRST tt_auth_copy_success NO-LOCK NO-ERROR.

           { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

           IF AVAILABLE tt_auth_copy_success THEN
             ASSIGN
               cAuthCopyEmailMessage = "The single auth copy job has been completed successfully. " + tt_auth_copy_success.owning_key + " has been created. " + tt_auth_copy_success.success_message.
           MESSAGE "tt_auth_copy_success.success_message" tt_auth_copy_success.success_message.  
         END. /* WHEN "SINGLE" */

      END CASE. /* CASE tt_authcopy.copy_type: */

    END. /* IF CAN-FIND(FIRST tt_auth_copy_success) THEN */

    ASSIGN
      oJob:JobProgressDescription = "The Authorisation Copy Job: ":U + cJobReference + " has been completed.":U
      oJob:JobProgressPercent     = 100.
      
    ASSIGN
      ipcAction      = "EMAIL":U
      ipcDescription = "The Auth Copy Job: ":U + cJobReference + " has been completed.":U
      ipcResource    = "":U
      ipcSendTo      = mipEnv:miUser:UserEmail
      ipcSendBody    = cAuthCopyEmailMessage
      ipcSendAttch   = "":U
      ipcResGrp      = "mm_TmplCor":U
      ipcResTempl    = "ma_mm_Cor":U
      ipcResREM      = "":U
      ipcResREF      = "":U.
      
  END. /* Copy completed without issues */

  /* Only call mammressend.p if details have been setup */
  IF  ipcAction      <> "":U
  AND ipcDescription <> "":U THEN
  DO:
  
    RUN ma/own/mammressend.p  ( INPUT  ipcAction
                              , INPUT  ipcDescription
                              , INPUT  ipcResource
                              , INPUT  ipcSendTo
                              , INPUT  ipcSendBody
                              , INPUT  ipcSendAttch
                              , INPUT  ipcResGrp
                              , INPUT  ipcResTempl
                              , INPUT  ipcResREM
                              , INPUT  ipcResREF
                              , OUTPUT opcResult
                              ) . 

    RUN maCommunication IN (mipEnv:Health:maUtility:maUtility) 
        (INPUT "EMAIL":U,
         INPUT ipcSendTo,
         INPUT mipEnv:miUser:UserEmail,
         INPUT "":U,
         INPUT "":U,
         INPUT ipcDescription,
         INPUT ipcSendBody,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U,
         INPUT "":U).

  END. /* IF  ipcAction      <> "":U AND ipcDescription <> "":U THEN */
{ mip/inc/mipcatcherror.i }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getFilePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilePath Procedure 
FUNCTION getFilePath RETURNS CHARACTER
  ( INPUT ipcCopyType AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose: Returns the filepath of the error log file  
    Notes: 
------------------------------------------------------------------------------*/ 
  DEFINE VARIABLE cFilePath AS CHARACTER   NO-UNDO.

  /* Get filepath name for error file */
  RUN ma/msc/mafile.p ( INPUT  "CopyResults":U,              /* filemask   */
                        INPUT  0,                            /* ???        */
                        INPUT  ipcCopyType,                  /* Filename   */
                        INPUT  STRING(TODAY, '99999999':U),  /* Run date   */
                        INPUT  "":U,                         /* ???        */
                        OUTPUT cFilePath)                    /* filepath   */
                        NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
  DO:
    RETURN "ERROR":U.
  END. /* IF ERROR-STATUS:ERROR THEN */
  ELSE 
  DO: 
    ASSIGN cFilePath = TRIM(cFilePath)
           cFilePath = REPLACE(cFilePath,".pdf":U,"":U) + ".csv":U.

    IF NUM-ENTRIES(cFilePath,".":U) > 2 THEN
      ASSIGN cFilePath = ENTRY(1,cFilePath,".":U) + ".":U + ENTRY(3,cFilePath,".":U).      

    RETURN cFilePath. /* Function return value. */      

  END. 
  
{mip/inc/mipcatcherror.i}
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
