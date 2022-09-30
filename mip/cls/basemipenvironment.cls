/*------------------------------------------------------------------------------
    Component: MIP SOA Environment
       Author: Chris Koster
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS mip.cls.basemipenvironment INHERITS cls.mipconnection:
  {mip/inc/mipdefsharedclass.i}

  {mip/inc/dsmipxmlconfig.i}
  {mip/inc/miptemptables.i &TempTableName = ttObjectCollection}
  {mip/inc/miptemptables.i &TempTableName = ttObjectCleanup}

  DEFINE VARIABLE gcEnvTypeAcronymKey              AS CHARACTER    NO-UNDO.

  DEFINE PUBLIC PROPERTY EnvironmentKey            AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EnvironmentCode           AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EnvironmentLabel          AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EnvironmentDescription    AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EnvironmentBasePathApp    AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EnvironmentBasePathDLC    AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EnvironmentPropath        AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EnvironmentDevWipPropath  AS CHARACTER    NO-UNDO GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY SystemKey                 AS CHARACTER    NO-UNDO GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY VersionKey                AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY VersionTag                AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY VersionLabel              AS CHARACTER    NO-UNDO GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY CapabilityNameList        AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY InheritedEnvironments     AS CHARACTER    NO-UNDO GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY DefaultPrefSearchSequence AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY DefaultSessionMnemonic    AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY DefaultUserMnemonic       AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY DefaultRoleMnemonic       AS CHARACTER    NO-UNDO GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY CopyrightNotice           AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY CurrencyFormat            AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY DateTimeFormat            AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY DateFormat                AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY TimeFormat                AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY QueryTranslation          AS CHARACTER    NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY ShowStartupInfo           AS LOGICAL      NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY Initialized               AS LOGICAL      NO-UNDO GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY RemoteConnectTimeout      AS INTEGER      NO-UNDO INITIAL   60 GET. PRIVATE SET. 
  DEFINE PUBLIC PROPERTY RemoteCallTimeout         AS INTEGER      NO-UNDO INITIAL 1800 GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY MemoryLeakDetection       AS LOGICAL      NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY MemoryEfficientStacks     AS LOGICAL      NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY TamperedWithStacks        AS LOGICAL      NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY TraceLogCapable           AS LOGICAL      NO-UNDO GET. PRIVATE SET.
  DEFINE PUBLIC PROPERTY EtimeInterval             AS INTEGER      NO-UNDO GET.         SET.
  DEFINE PUBLIC PROPERTY EtimeValue                AS INTEGER      NO-UNDO GET.         SET.

  DEFINE PUBLIC PROPERTY EnvironmentType AS cls.mipacronym         NO-UNDO GET. PRIVATE SET.

  /* == APPLICATIONS ======================================================== */
  &SCOPED-DEFINE Segment ApplicationProperty

  {cls/inc/mip.cls.i &PropertyName = Questionnaire  &ApplicationClass = cls.mipquestionnaire}
  {cls/inc/mip.cls.i &PropertyName = Communication  &ApplicationClass = cls.mipcommunication}
  {cls/inc/mip.cls.i &PropertyName = TaskManager    &ApplicationClass = cls.miptaskmanager  }
  {cls/inc/mip.cls.i &PropertyName = MultiMedia     &ApplicationClass = cls.mipmultimedia   }
  {cls/inc/mip.cls.i &PropertyName = Calendar       &ApplicationClass = cls.mipcalendar     }
  {cls/inc/mip.cls.i &PropertyName = JobManager     &ApplicationClass = cls.mipjobmanager   }
  {cls/inc/mip.cls.i &PropertyName = CallCenter     &ApplicationClass = cls.mipcallcenter   }
  {cls/inc/mip.cls.i &PropertyName = Gamification   &ApplicationClass = cls.mipgamification }
  {cls/inc/mip.cls.i &PropertyName = OneClient      &ApplicationClass = cls.miponeclient    }
  {cls/inc/mip.cls.i &PropertyName = WarpSpeed      &ApplicationClass = cls.mipwarpspeed    }
  {cls/inc/mip.cls.i &PropertyName = Reporting      &ApplicationClass = cls.mipreporting    }
  {cls/inc/mip.cls.i &PropertyName = WorkFlow       &ApplicationClass = cls.mipworkflow     }
  {cls/inc/mip.cls.i &PropertyName = Dynamics       &ApplicationClass = cls.mipdynamics     }
  {cls/inc/mip.cls.i &PropertyName = Financials     &ApplicationClass = cls.mipfinancials   }
  {cls/inc/mip.cls.i &PropertyName = Health         &ApplicationClass = cls.miphealth       }
  {cls/inc/mip.cls.i &PropertyName = WayTag         &ApplicationClass = cls.mipwaytag       }
  {cls/inc/mip.cls.i &PropertyName = ERM            &ApplicationClass = cls.miperm          }
  {cls/inc/mip.cls.i &PropertyName = HRM            &ApplicationClass = cls.miphrm          }
  {cls/inc/mip.cls.i &PropertyName = ILS            &ApplicationClass = cls.mipils          }
  {cls/inc/mip.cls.i &PropertyName = PF             &ApplicationClass = cls.mippf           }
  {cls/inc/mip.cls.i &PropertyName = EB             &ApplicationClass = cls.mipeb           }
  


  &UNDEFINE Segment

  /* == SERVICES ============================================================ */
  &SCOPED-DEFINE Segment ServiceProperty

  {cls/inc/mip.cls.i &PropertyName = miDataInterface  &ServiceClass = cls.mipdatainterface  }
  {cls/inc/mip.cls.i &PropertyName = miDecision       &ServiceClass = cls.mipdecisionservice}
  {cls/inc/mip.cls.i &PropertyName = miUtility        &ServiceClass = cls.miputility        }
  {cls/inc/mip.cls.i &PropertyName = miSession        &ServiceClass = cls.mipsession        }
  {cls/inc/mip.cls.i &PropertyName = miSystem         &ServiceClass = cls.mipsystem         }
  {cls/inc/mip.cls.i &PropertyName = miTenant         &ServiceClass = cls.miptenant         }
  {cls/inc/mip.cls.i &PropertyName = miAgent          &ServiceClass = cls.mipagent          }
  {cls/inc/mip.cls.i &PropertyName = miUser           &ServiceClass = cls.mipuser           }

  &UNDEFINE Segment

  DEFINE PUBLIC    PROPERTY miOSFunctions   AS mip.cls.miposfunctions NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY miExpression    AS cls.mipexpression      NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY miProcedure     AS cls.mipprocedure       NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY miDatabase      AS cls.mipdatabase        NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY miDBEntity      AS cls.mipdbentity        NO-UNDO GET. PRIVATE   SET.

  DEFINE PUBLIC    PROPERTY DynamicApplicationClassList AS CHARACTER  NO-UNDO GET. PROTECTED SET.
  DEFINE PUBLIC    PROPERTY DynamicApplicationNameList  AS CHARACTER  NO-UNDO GET. PROTECTED SET.
  DEFINE PUBLIC    PROPERTY DynamicServiceClassList     AS CHARACTER  NO-UNDO GET. PROTECTED SET.
  DEFINE PUBLIC    PROPERTY DynamicServiceNameList      AS CHARACTER  NO-UNDO GET. PROTECTED SET.

  DEFINE PUBLIC    PROPERTY StartupTenantName         AS CHARACTER    NO-UNDO GET.           SET.
  DEFINE PUBLIC    PROPERTY SupportedTenantMask       AS CHARACTER    NO-UNDO GET.           SET.
  DEFINE PUBLIC    PROPERTY AuthenticationServerError AS CHARACTER    NO-UNDO GET.           SET.
  DEFINE PUBLIC    PROPERTY AuthenticationServerName  AS CHARACTER    NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY CanAuthenticateLocally    AS LOGICAL      NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY ReplicateNonGroupedUser   AS LOGICAL      NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY MaintainUserRoles         AS LOGICAL      NO-UNDO GET. PRIVATE   SET.
  DEFINE PUBLIC    PROPERTY AuthenticationMethod      AS CHARACTER    NO-UNDO GET.
    PRIVATE SET(INPUT ipcAuthenticationMethod AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:  Set the type of authentication to be done by the system
     *** Note:  Invalid values to default to an empty string
  ----------------------------------------------------------------------------*/
      ASSIGN
          ipcAuthenticationMethod = (IF ipcAuthenticationMethod = ? THEN "":U ELSE TRIM(ipcAuthenticationMethod))
          AuthenticationMethod    = ENTRY(LOOKUP(ipcAuthenticationMethod, "LDAP,REMOTE-MIP":U) + 1, ",LDAP,REMOTE-MIP":U)
        .
      {mip/inc/mipcatcherror.i}
    END SET.

  DEFINE PUBLIC    PROPERTY ProfilerContext      AS CHARACTER  NO-UNDO GET. SET.
  DEFINE PUBLIC    PROPERTY ProfilerEnabled      AS LOGICAL    NO-UNDO GET.
    SET(INPUT iplProfilerEnabled AS LOGICAL):

        /*----------------------------------------------------------------------------
        Purpose:  Turns the profiler on/off
            Notes:  Uses ProfilerContext in the output file name if set
        ----------------------------------------------------------------------------*/
    
        DEFINE VARIABLE cOutputDirectory         AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cProfileSession          AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cProfileOutput           AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE lProfilerListingsEnabled AS LOGICAL    NO-UNDO INITIAL FALSE.
        
        IF iplProfilerEnabled THEN
        DO:
            /* Start Profiling */
            ASSIGN
              cProfileSession            = mipEnv:miUser:UserCode + "_":U + (IF ProfilerContext > "":U THEN ProfilerContext + "_":U ELSE "":U) + STRING(TODAY, "99-99-9999":U) + "_":U + REPLACE(STRING(TIME, "HH:MM:SS":U), ":":U, "-":U)
              cOutputDirectory           = mipEnv:miUtility:getPreference("mip_PrefProfilerDir":U + OPSYS)
              cOutputDirectory           = (IF cOutputDirectory = ? OR TRIM(cOutputDirectory) = "":U THEN "":U ELSE RIGHT-TRIM(REPLACE(TRIM(cOutputDirectory), "~\":U, "/":U), "/":U) + "/":U) 
              FILE-INFORMATION:FILE-NAME = cOutputDirectory
              cOutputDirectory           = (IF FILE-INFORMATION:FULL-PATHNAME <> ? THEN cOutputDirectory ELSE RIGHT-TRIM(REPLACE(TRIM(SESSION:TEMP-DIRECTORY), "~\":U, "/":U), "/":U) + "/":U)
              cProfileOutput             = cOutputDirectory + "Profile_":U + cProfileSession + ".out":U.
    
            ASSIGN  
              lProfilerListingsEnabled  = LOGICAL(mipEnv:miUtility:getPreference("mip_PrefProfilerListingsEnabled":U))
              lProfilerListingsEnabled  = (IF lProfilerListingsEnabled = ? THEN FALSE ELSE lProfilerListingsEnabled)
            NO-ERROR.
            {&ResetError}  /* Reset any data type errors */  
    
            ASSIGN
              PROFILER:ENABLED     = FALSE
              PROFILER:DESCRIPTION = "Profiling - Session:":U + cProfileSession
              PROFILER:DIRECTORY   = cOutputDirectory
              PROFILER:FILENAME    = cProfileOutput
              PROFILER:LISTINGS    = lProfilerListingsEnabled
              PROFILER:ENABLED     = TRUE
              PROFILER:PROFILING   = TRUE
              ProfilerEnabled      = TRUE.
    
            IF SESSION:DISPLAY-TYPE <> "GUI" THEN
                MESSAGE "PROFILER: Profiling started. " + PROFILER:DESCRIPTION + " writing to: " + cProfileOutput.
        END.
        ELSE
        DO:
            DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    
            ASSIGN
                  PROFILER:PROFILING   = FALSE
                  lSuccess             = PROFILER:WRITE-DATA()
                  PROFILER:ENABLED     = FALSE
                  ProfilerEnabled      = FALSE. /* Clear out session */
            
            IF SESSION:DISPLAY-TYPE <> "GUI" THEN
                MESSAGE "PROFILER: Profiling completed.".
        END.
    
        {mip/inc/mipcatcherror.i}

    END SET.

  DEFINE BUFFER gbuf_applicationservice FOR mipapplicationservice.
  DEFINE BUFFER gbuf_application        FOR mipapplication.

  DEFINE PRIVATE VARIABLE gcRestartDetails      AS CHARACTER  NO-UNDO.
  DEFINE PRIVATE VARIABLE gcRebuildStacks       AS CHARACTER  NO-UNDO.
  DEFINE PRIVATE VARIABLE giSequence            AS INTEGER    NO-UNDO.
  DEFINE PRIVATE VARIABLE ghBaseEnvironmentProc AS HANDLE     NO-UNDO.
  DEFINE PRIVATE VARIABLE ghCallBackProc        AS HANDLE     NO-UNDO.

  DEFINE PUBLIC PROPERTY ResolvedParam AS CHARACTER  NO-UNDO
    GET():
  /*----------------------------------------------------------------------------
      Purpose:  Resolved PARAM and ICFPARAM List after Token Replacement
  ----------------------------------------------------------------------------*/
      IF ResolvedParam <> "":U
      THEN
        RETURN ResolvedParam.

      RUN mip/app/mipreadxmlconfig.p
          (INPUT  DATASET dsMipXmlConfig BIND,
           OUTPUT ResolvedParam)
        .
      RETURN ResolvedParam.

      {mip/inc/mipcatcherror.i}

    END GET . PRIVATE SET.

  /* -- CONSTRUCTOR --------------------------------------------------------- */
  CONSTRUCTOR PUBLIC basemipenvironment
    (INPUT  ipcRestartDetails AS  CHARACTER,
     INPUT  DATASET dsMipXmlConfig):
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */
    DEFINE VARIABLE oObject       AS Progress.Lang.Object NO-UNDO.
    DEFINE VARIABLE cEnvironment  AS CHARACTER            NO-UNDO.

    /* We will self-terminate this object if another instance of this object has
       already been started */
    oObject = SESSION:FIRST-OBJECT.

    /* Make sure there aren't any other environment objects floating about */
    DO WHILE VALID-OBJECT(oObject):
      IF oObject <> THIS-OBJECT AND oObject:GetClass():TypeName = "cls.mipenvironment":U
      THEN
        RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 1}.

      oObject = oObject:NEXT-SIBLING.
    END. /* DO WHILE VALID-OBJECT(oObject) */

    ASSIGN
        DynamicApplicationClassList = "{&DynamicApplicationClassList}":U
        DynamicApplicationNameList  = "{&DynamicApplicationNameList}":U
        DynamicServiceClassList     = "{&DynamicServiceClassList}":U
        DynamicServiceNameList      = "{&DynamicServiceNameList}":U

        gcRestartDetails = (IF ipcRestartDetails = ? THEN "":U ELSE TRIM(ipcRestartDetails))

        cEnvironment = getStartupParam("mipEnv":U)
        ServerHandle = SESSION
        ServerType   = "Local":U

        ShowStartupInfo = CAN-DO("yes,true":U, TRIM(getStartupParam("mipShowStartupInfo":U)))
        EnvironmentCode = TRIM(ENTRY(MAXIMUM(1, NUM-ENTRIES(cEnvironment, "|":U)), cEnvironment, "|":U))

        ENTRY(MAXIMUM(1, NUM-ENTRIES(cEnvironment, "|":U)), cEnvironment, "|":U) = "":U

        InheritedEnvironments = REPLACE(TRIM(cEnvironment, "| ":U), "|":U, ",":U)

        CapabilityNameList = TRIM(REPLACE(getStartupParam("mipEnvCapabilities":U), "|":U, ",":U))

        cEnvironment = TRIM(getStartupParam("mipStartEnv":U))
      .
    IF CAN-DO(",yes,true":U, cEnvironment)
    THEN
      initializeEnvironment().

    {mip/inc/mipcatcherror.i &MESSAGE = TRUE}

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemipenvironment */

  /* -- DESTRUCTOR ---------------------------------------------------------- */
  DESTRUCTOR PUBLIC basemipenvironment
    ():
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */
    &SCOPED-DEFINE ResetTheError TRUE

    /* Clean up all the applications */
    IF VALID-OBJECT(Questionnaire)  THEN DELETE OBJECT Questionnaire  NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(Communication)  THEN DELETE OBJECT Communication  NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(TaskManager)    THEN DELETE OBJECT TaskManager    NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(MultiMedia)     THEN DELETE OBJECT MultiMedia     NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(Calendar)       THEN DELETE OBJECT Calendar       NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(JobManager)     THEN DELETE OBJECT JobManager     NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(CallCenter)     THEN DELETE OBJECT CallCenter     NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(Gamification)   THEN DELETE OBJECT Gamification   NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(OneClient)      THEN DELETE OBJECT OneClient      NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(WarpSpeed)      THEN DELETE OBJECT WarpSpeed      NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(Reporting)      THEN DELETE OBJECT Reporting      NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(WorkFlow)       THEN DELETE OBJECT WorkFlow       NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(Dynamics)       THEN DELETE OBJECT Dynamics       NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(Financials)     THEN DELETE OBJECT Financials     NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(Health)         THEN DELETE OBJECT Health         NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(WayTag)         THEN DELETE OBJECT WayTag         NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(HRM)            THEN DELETE OBJECT HRM            NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(ILS)            THEN DELETE OBJECT ILS            NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(PF)             THEN DELETE OBJECT PF             NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(EB)             THEN DELETE OBJECT EB             NO-ERROR. {mip/inc/mipmessageerror.i}

    /* Clean up all the services and helper objects */
    IF VALID-OBJECT(EnvironmentType)  THEN DELETE OBJECT EnvironmentType  NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miOSFunctions)    THEN DELETE OBJECT miOSFunctions    NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miTenant)         THEN DELETE OBJECT miTenant         NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miExpression)     THEN DELETE OBJECT miExpression     NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miProcedure)      THEN DELETE OBJECT miProcedure      NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miDecision)       THEN DELETE OBJECT miDecision       NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miDatabase)       THEN DELETE OBJECT miDatabase       NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miDBEntity)       THEN DELETE OBJECT miDBEntity       NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miUtility)        THEN DELETE OBJECT miUtility        NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miSession)        THEN DELETE OBJECT miSession        NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miSystem)         THEN DELETE OBJECT miSystem         NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miUser)           THEN DELETE OBJECT miUser           NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miAgent)          THEN DELETE OBJECT miAgent          NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-OBJECT(miDataInterface)  THEN DELETE OBJECT miDataInterface  NO-ERROR. {mip/inc/mipmessageerror.i}

    IF VALID-HANDLE(ghBaseEnvironmentProc)  THEN DELETE PROCEDURE ghBaseEnvironmentProc NO-ERROR. {mip/inc/mipmessageerror.i}
    IF VALID-HANDLE(ghCallBackProc)         THEN DELETE PROCEDURE ghCallBackProc        NO-ERROR. {mip/inc/mipmessageerror.i}

    &UNDEFINE ResetTheError

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC basemipenvironment */

  /* == METHODS ============================================================= */
  /* -- METHOD: configureEnvironment ---------------------------------------- */
  METHOD PUBLIC LOGICAL configureEnvironment
    (INPUT iplStartOfRequest AS LOGICAL):
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */
    DEFINE VARIABLE cPropath              AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iStack                AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cRebuildStacks        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStackName            AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE hStack                AS HANDLE     NO-UNDO.

    /* TODO: This would be better served as a separate method to check broken/severed connections, so that it can be run from the GUI as well */
    DEFINE VARIABLE cIntactConnectionsProcessed AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cErrorMessage               AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE oServiceConnection          AS cls.mipconnection.

    DEFINE BUFFER mipapplicationservice FOR mipapplicationservice.
    DEFINE BUFFER ttObjectCollection    FOR ttObjectCollection.

    IF Initialized AND iplStartOfRequest
    THEN
    ServiceBlock:
    FOR EACH mipapplicationservice,
       FIRST ttObjectCollection
       WHERE ttObjectCollection.cObjectID BEGINS "MIPService_":U + mipapplicationservice.cServiceCode:

      oServiceConnection = CAST(ttObjectCollection.oObject, "cls.mipservice":U).

      IF oServiceConnection:ServerType <> "AppServer":U OR LOOKUP(oServiceConnection:ServerConnectString,cIntactConnectionsProcessed,CHR(3)) > 0 
      THEN 
        NEXT ServiceBlock.

      /* If this connection is the same as a connection that we fixed already, don't even bother 
         checking the CONNECTED property, just force the reconnect. This saves the overhead of
         running to the appserver just to see if it is connected. */
      IF VALID-HANDLE(oServiceConnection:ServerHandle) 
      THEN
        IF oServiceConnection:ServerHandle:CONNECTED()
        THEN
        DO:
          cIntactConnectionsProcessed = cIntactConnectionsProcessed
                                      + (IF cIntactConnectionsProcessed = "":U THEN "":U ELSE CHR(3))
                                      + oServiceConnection:ServerConnectString.
          NEXT ServiceBlock.
        END.

      {mip/inc/miplog.i 'mip_Connection' "'WARNING: It seems that an appserver connection has dropped, we will attempt to restore the connection for service : ' + mipapplicationservice.cServiceCode}  

      /* Force a reconnect */
      oServiceConnection:connectToServer() NO-ERROR.

      IF {&ErrorStatus}
      THEN
      DO:
        cErrorMessage = {mip/inc/mipreturnvalue.i}.

        {mip/inc/miplog.i 'mip_Connection' "'Error: ' + cErrorMessage}

        {&ResetError}
      END. /* IF {&ErrorStatus} */
    END. /* FOR EACH mipapplicationservice */
    
    ASSIGN
        AuthenticationServerError = "":U

        iplStartOfRequest = FALSE WHEN (iplStartOfRequest = ?)

        cPropath = (IF iplStartOfRequest AND EnvironmentDevWipPropath <> "":U AND miUser:DevelopmentUser
                    THEN REPLACE(EnvironmentDevWipPropath, "[DevUser]":U, LC(miUser:UserCode)) + ",":U
                       + EnvironmentPropath
                    ELSE EnvironmentPropath)

        PROPATH = (IF cPropath = "":U OR cPropath = ? THEN PROPATH ELSE cPropath) WHEN (LOOKUP("LeavePROPATH":U, gcRestartDetails) = 0)
      .
    IF iplStartOfRequest
    THEN
    DO:
      ASSIGN
          gcRebuildStacks = miUtility:getPreference("mip_PrefRebuildStacks":U)
          gcRebuildStacks = (IF gcRebuildStacks = ? THEN "":U ELSE TRIM(gcRebuildStacks))

          TamperedWithStacks = (gcRebuildStacks <> "":U)
        .
      Loop_Block:
      DO iStack = 1 TO NUM-ENTRIES(gcRebuildStacks):
        ASSIGN
            cStackName = TRIM(ENTRY(iStack, gcRebuildStacks))

            hStack = mipEnv:miProcedure:getProcedureHandle(cStackName)
          .
        IF hStack = ?
        THEN
        DO:
          MESSAGE "There was an error rebuilding stack:" cStackName ", the environment will attempt to continue without this stack.~n~n":U
                  {mip/inc/mipreturnvalue.i}.

          NEXT Loop_Block.
        END. /* IF hStack = ? */
        ELSE
          /* Remove invalid/problem entries from the stack list*/
          cRebuildStacks = cRebuildStacks + (IF cRebuildStacks = "" THEN "" ELSE ",":U) + cStackName.

        /* Rebuild the stack without destroying the base... */
        mipEnv:miProcedure:rebuildStack(cStackName, FALSE).
      END. /* DO iStack = 1 TO NUM-ENTRIES(gcRebuildStacks) */

      /* Set the stack list to only valid entries */
      gcRebuildStacks = cRebuildStacks.
    END. /* IF iplStartOfRequest */
    ELSE
    DO:
      Loop_Block:
      DO iStack = 1 TO NUM-ENTRIES(gcRebuildStacks):
        /* Rebuild the stack without destroying the base... */
        mipEnv:miProcedure:rebuildStack(TRIM(ENTRY(iStack, gcRebuildStacks)), FALSE) NO-ERROR.

        IF {&ErrorStatus}
        THEN
        DO:
          MESSAGE "The Progress session will shutdown due to an error rebuilding stacks:~n~n":U
                  {mip/inc/mipreturnvalue.i}
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

          /* QUIT doesn't work?!? Desperate times call for desperate measures...! */
          RUN xSelfDestruct IN ghBaseEnvironmentProc.
        END. /* IF {&ErrorStatus} */
      END. /* DO iStack = 1 TO NUM-ENTRIES(gcRebuildStacks) */

      ASSIGN
          TamperedWithStacks = FALSE
          gcRebuildStacks    = "":U
        .
    END. /* ELSE of IF iplStartOfRequest */

    /* is this environment capable of producing trace log information */
    TraceLogCapable = (LOOKUP(TRIM(miUtility:getPreference("mip_PrefTraceLogCapable":U)), "yes,true":U) > 0).

    IF TraceLogCapable = ?
    THEN
      TraceLogCapable = FALSE.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL configureEnvironment */
  
  /* -- METHOD: initializeEnvironment --------------------------------------- */
  METHOD PUBLIC CHARACTER getStartPreference
    (INPUT ipcStartPreferenceName AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */
  
    DEFINE VARIABLE opcStartPreferenceValue AS CHARACTER   NO-UNDO.

    {mip/cls/inc/basemipenvironment.cls.i
      &Segment          = getStartPreference
      &PreferenceName   = ipcStartPreferenceName
      &AssignToVariable = opcStartPreferenceValue}    
  
    RETURN opcStartPreferenceValue.
  
    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC CHARACTER getStartPreference */

  /* -- METHOD: initializeEnvironment --------------------------------------- */
  METHOD PUBLIC LOGICAL initializeEnvironment
    ():
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */
    DEFINE VARIABLE oService              AS cls.mipservice NO-UNDO.
    DEFINE VARIABLE cExternalConfig       AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE cAuthServer           AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE cAuthMethod           AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE cValue                AS CHARACTER      NO-UNDO EXTENT 4.
    DEFINE VARIABLE cAllow                AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE iLoop                 AS INTEGER        NO-UNDO.
    DEFINE VARIABLE lAlertBox             AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE lSuccess              AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE iRemoteConnectTimeout AS INTEGER        NO-UNDO.
    DEFINE VARIABLE iRemoteCallTimeout    AS INTEGER        NO-UNDO.

    DEFINE BUFFER mipstartpreference  FOR mipstartpreference.
    DEFINE BUFFER mipapplication      FOR mipapplication.
    DEFINE BUFFER mipenvironment      FOR mipenvironment.
    DEFINE BUFFER dynservice          FOR dynservice.

    IF Initialized
    THEN
      RETURN TRUE.

    ASSIGN
        lAlertBox = SESSION:APPL-ALERT-BOXES

        SESSION:APPL-ALERT-BOXES = FALSE

        miOSFunctions = NEW mip.cls.miposfunctions()
        miProcedure   = NEW cls.mipprocedure()
        miDatabase    = NEW cls.mipdatabase()
        miDBEntity    = NEW cls.mipdbentity()
        
        giSequence = 0
      .
    RUN cls/app/mipenvironmentprot.p PERSISTENT SET ghBaseEnvironmentProc
        (INPUT THIS-OBJECT)
      .

    IF InheritedEnvironments <> "":U
    THEN
      RUN inheritEnvironments IN ghBaseEnvironmentProc.
    
    FIND FIRST mipapplication NO-LOCK
         WHERE mipapplication.cEnvironmentCode = EnvironmentCode
           AND mipapplication.cApplicationCode = "mipEnv":U
      NO-ERROR.

    {&ResetError}

    IF AVAILABLE mipapplication
        AND TRIM(mipapplication.cServerConnectString) <> "":U
        AND      mipapplication.cServerConnectString  <> ?
    THEN
    DO:
      prepareConnection(mipapplication.cServerConnectString).

      IF ServerType = "WebService":U
      THEN
        RUN {&BaseManagerWSCB} PERSISTENT SET ghCallBackProc.

      ASSIGN
          ConnectionCallBack = ghCallBackProc WHEN (VALID-HANDLE(ghCallBackProc))

          /* Only connect immediately to anything that is not in the list (... currently only WebService...) */
          lSuccess = (IF LOOKUP(ServerType, "WebService":U) = 0 THEN connectToServer() ELSE TRUE)
        .
    END. /* IF AVAILABLE mipapplication ... */

    IF NOT CONNECTED("MIPDB":U)
    THEN
    Database_Config:
    DO:
      FIND FIRST dynservice NO-LOCK
           WHERE dynservice.cEnvironmentCode = EnvironmentCode
             AND dynservice.cServiceType     = "Database":U
             AND dynservice.cServiceName     = "MIPDB":U
        NO-ERROR.

      {&ResetError}

      IF NOT AVAILABLE dynservice
      THEN
        LEAVE Database_Config.

      /* Connect the MIP database as we need to get possible supplementary environment details from it */
      miDatabase:connectDB(TRIM(dynservice.cConnectParams)
                          + (IF LOOKUP("-ld":U,   dynservice.cConnectParams, " ":U) = 0
                             THEN     " -ld ":U + dynservice.cServiceName
                             ELSE "":U)).
    END. /* IF NOT {&ErrorStatus} AND NOT CONNECTED("MIPDB":U) */

    /* Let us clean out the sessions that are not applicable from the dsMipXmlConfig dataset.
     This will save pass unnecessarily large datasets to other procedures...  */
    RUN removeUnwantedSessions IN ghBaseEnvironmentProc.
    RUN fixStackSequence       IN ghBaseEnvironmentProc.

    {mip/cls/inc/basemipenvironment.cls.i &Segment = getStartPreference   &PropertyName = StartupTenantName       &DataType="CHARACTER"}
    {mip/cls/inc/basemipenvironment.cls.i &Segment = getStartPreference   &PropertyName = SupportedTenantMask     &DataType="CHARACTER"}

    /* We can now focus the tenant who has the correct startup data */
    lSuccess = cls.mipmultitenancy:focusDomain(StartupTenantName).

    DO ON STOP UNDO, RETURN ERROR {mip/inc/mipreturnvalue.i}:
       /* If the appropriate database is not connected and ServerHandle points
        to this Session, this call should raise the ERROR-STATUS */
       RUN mip/app/mipreaddbconfig.p ON ServerHandle
           (INPUT        EnvironmentCode,
            INPUT        CapabilityNameList,
                  OUTPUT EnvironmentKey,
                  OUTPUT gcEnvTypeAcronymKey,
                  OUTPUT EnvironmentLabel,
                  OUTPUT EnvironmentDescription,
                  OUTPUT SystemKey,
                  OUTPUT VersionKey,
                  OUTPUT VersionTag,
                  OUTPUT VersionLabel,
            INPUT-OUTPUT DATASET dsMipXmlConfig BIND)
         .
    END. /* DO ON STOP  UNDO, RETURN ERROR {mip/inc/mipreturnvalue.i} */

    mip.applicationfactory:EnvironmentConfigComplete = TRUE.
    
    {mip/cls/inc/basemipenvironment.cls.i &Segment = getStartPreference   &PropertyName = MemoryEfficientStacks}
    {mip/cls/inc/basemipenvironment.cls.i &Segment = getStartPreference   &PropertyName = MemoryLeakDetection}

    IF SESSION:CLIENT-TYPE = "4GLCLIENT":U AND SESSION:DISPLAY-TYPE = "GUI":U AND NOT SESSION:BATCH-MODE
    THEN
      MemoryLeakDetection = FALSE.

    FIND FIRST mipenvironment NO-LOCK
         WHERE mipenvironment.cEnvironmentCode = EnvironmentCode.

    ASSIGN
        gcEnvTypeAcronymKey = (IF gcEnvTypeAcronymKey = ? THEN "":U ELSE TRIM(gcEnvTypeAcronymKey))

        EnvironmentDevWipPropath = mipenvironment.cEnvironmentCustomPath
        EnvironmentPropath       = (IF mipenvironment.cEnvironmentPropath = "":U THEN PROPATH ELSE mipenvironment.cEnvironmentPropath)

        EnvironmentBasePathApp = mipenvironment.cBasePathApp
        EnvironmentBasePathDLC = mipenvironment.cBasePathDLC

        PROPATH = EnvironmentPropath WHEN (LOOKUP("LeavePROPATH":U, gcRestartDetails) = 0).

    /* Connect the rest of the databases before we start any
       code - they could depend on the database connections */
    FOR EACH dynservice NO-LOCK
       WHERE dynservice.cEnvironmentCode = EnvironmentCode
         AND dynservice.cServiceType     = "Database":U
      ON ERROR UNDO, THROW:

      miDatabase:connectDB(TRIM(dynservice.cConnectParams)
                          + (IF LOOKUP("-ld":U,   dynservice.cConnectParams, " ":U) = 0
                             THEN     " -ld ":U + dynservice.cServiceName
                             ELSE "":U)).
    END. /* FOR EACH dynservice NO-LOCK ... */

    FIND FIRST gbuf_applicationservice NO-LOCK
         WHERE gbuf_applicationservice.cEnvironmentCode = EnvironmentCode
           AND gbuf_applicationservice.cApplicationCode = "mipEnv":U
           AND gbuf_applicationservice.cServiceCode     = "miExpression":U
      NO-ERROR.

    {&ResetError}

    IF AVAILABLE gbuf_applicationservice
    THEN
    DO:
      IF gbuf_applicationservice.cServiceClassFile <> "":U
      THEN miExpression = DYNAMIC-NEW gbuf_applicationservice.cServiceClassFile().
      ELSE miExpression =         NEW cls.mipexpression().

      miExpression:setStack(INPUT gbuf_applicationservice.cProviderStack,
                            INPUT MemoryEfficientStacks).
    END. /* IF AVAILABLE miphelperobject */
    ELSE
      miExpression = NEW cls.mipexpression().
      
    /* We must refocus the start up tenant on the newly connected databases */
    lSuccess = cls.mipmultitenancy:focusDomain(StartupTenantName).      

    DEFINE VARIABLE cBasePathAppOverride  AS CHARACTER  NO-UNDO.

    cBasePathAppOverride = TRIM(getStartPreference("BasePathAppOverride":U + OPSYS)).

    IF cBasePathAppOverride <> ? AND cBasePathAppOverride <> "":U
    THEN
      EnvironmentBasePathApp = cBasePathAppOverride.

    IF ShowStartupInfo
    THEN
      RUN showStartupInfo IN ghBaseEnvironmentProc
        (INPUT "Config":U).

    ASSIGN
        EnvironmentType = NEW cls.mipacronym(?, FALSE, "mip_AcMicen":U)

        lSuccess = EnvironmentType:focusAcronymPermanently("Key":U, gcEnvTypeAcronymKey).

    /* We have to let applications be constructed at least, so that ones that
       don't support services-on-demand, still initialize like they used to */
    FOR EACH gbuf_application NO-LOCK
       WHERE gbuf_application.cEnvironmentCode = EnvironmentCode:

      /* The applications in this list have already been converted to deliver services-on-demand, therefore we do not need to pre-initialize them */
      IF LOOKUP(gbuf_application.cApplicationCode, "Calendar,CallCenter,Communication,ERM,Gamification,JobManager,mipEnv,MultiMedia,Questionnaire,Reporting,WarpSpeed,WorkFlow":U) > 0
      THEN
        NEXT.

      
      mip.applicationfactory:getApplication(gbuf_application.cApplicationCode).
    END. /* FOR EACH gbuf_application NO-LOCK ... */



    IF ShowStartupInfo
    THEN
      MESSAGE
        "-----------------------":U
        (IF CapabilityNameList = "":U
        THEN "":U
        ELSE "~n  Environment Capabilities: ":U
           + CapabilityNameList
           + "~n-----------------------":U).

    IF ShowStartupInfo
    THEN
      RUN showStartupInfo IN ghBaseEnvironmentProc
        (INPUT "ProcedureStacks":U).

    /* The ASSIGN is split up due to the WHEN clauses */
    ASSIGN
        lSuccess = miSession:unfocusSession()

        SESSION:APPL-ALERT-BOXES = lAlertBox

        /* It is important to note that the duplication of code here is intentional, because we need a value in the defSearchSequence to get the DefSearchSequence */
        DefaultPrefSearchSequence = "Environment:MICEN,System:MICSY":U
        DefaultPrefSearchSequence = TRIM(miUtility:getPreference("mip_PrefDefaultSearchSeq":U)).

    ASSIGN
        DefaultPrefSearchSequence = "Environment:MICEN,System:MICSY":U WHEN (DefaultPrefSearchSequence = ? OR DefaultPrefSearchSequence = "":U)

        DefaultSessionMnemonic    = TRIM(miUtility:getPreference("mip_PrefSessionMnemonic":U))
        DefaultUserMnemonic       = TRIM(miUtility:getPreference("mip_PrefUserMnemonic":U))
        DefaultRoleMnemonic       = TRIM(miUtility:getPreference("mip_PrefRoleMnemonic":U))

        QueryTranslation = TRIM(miUtility:getPreference("mip_PrefQueryTranslation":U))
        CopyrightNotice  = TRIM(miUtility:getPreference("mip_PrefCopyright":U))
        CurrencyFormat   = TRIM(miUtility:getPreference("mip_PrefCurrencyFormat":U))
        DateTimeFormat   = TRIM(miUtility:getPreference("mip_PrefDateTimeFormat":U))
        DateFormat       = TRIM(miUtility:getPreference("mip_PrefDateFormat":U))
        TimeFormat       = TRIM(miUtility:getPreference("mip_PrefTimeFormat":U)).

    ASSIGN iRemoteConnectTimeout = INTEGER(miUtility:getPreference("mip_PrefRemoteConnectTimeout":U)) NO-ERROR.
    ASSIGN iRemoteCallTimeout    = INTEGER(miUtility:getPreference("mip_PrefRemoteCallTimeout":U))    NO-ERROR.
    {&ResetError}

    /* The ASSIGN is split up due to the WHEN clauses */
    ASSIGN
        DefaultSessionMnemonic = "MITSE":U WHEN (DefaultSessionMnemonic = ? OR DefaultSessionMnemonic = "":U)
        DefaultUserMnemonic    = "MIMUS":U WHEN (DefaultUserMnemonic    = ? OR DefaultUserMnemonic    = "":U)
        DefaultRoleMnemonic    = "MIMRO":U WHEN (DefaultRoleMnemonic    = ? OR DefaultRoleMnemonic    = "":U)

        DateFormat = TRIM(REPLACE(REPLACE(REPLACE(SESSION:DATE-FORMAT, "d":U, "99/":U), "m":U, "99/":U), "y":U, "9999/":U), "/":U) WHEN (DateFormat = ? OR DateFormat = "":U)

        CopyrightNotice = "Copyright &copy; 2004-[ThisYear] MIP Holdings (Pty) Ltd. All Rights Reserved.":U WHEN (CopyrightNotice = ? OR CopyrightNotice = "":U)
        CurrencyFormat  = "R ->>>,>>>,>>>,>>9.99":U                                                         WHEN (CurrencyFormat  = ? OR CurrencyFormat  = "":U)
        TimeFormat      = "HH:MM:SS":U                                                                      WHEN (TimeFormat      = ? OR TimeFormat      = "":U)
        DateTimeFormat  = DateFormat + " ":U + TimeFormat                                                   WHEN (DateTimeFormat  = ? OR DateTimeFormat  = "":U)

        RemoteConnectTimeout = iRemoteConnectTimeout WHEN iRemoteConnectTimeout > 0
        RemoteCallTimeout    = iRemoteCallTimeout    WHEN iRemoteCallTimeout    > 0

        CopyrightNotice = REPLACE(CopyrightNotice, "[ThisYear]":U, STRING(YEAR(TODAY)))

        QueryTranslation = "":U WHEN (LOOKUP(QueryTranslation, "[AsAndWhen],[Global]":U) = 0).

    {mip/cls/inc/basemipenvironment.cls.i
      &Segment          = getStartPreference
      &AssignToVariable = cAuthMethod
      &PreferenceName   = 'AuthenticationMethod':U}

    IF cAuthMethod <> "":U
    THEN
      ASSIGN
          cAuthServer = TRIM(ENTRY(2, cAuthMethod, ":":U)) WHEN (NUM-ENTRIES(cAuthMethod, ":":U) > 1)
          cAuthMethod = TRIM(ENTRY(1, cAuthMethod, ":":U)).
    ELSE
      cAuthMethod = TRIM(miUtility:getPreference("mip_PrefExtUserSource":U)).

    CASE cAuthMethod:
      WHEN "REMOTE-MIP":U
      THEN
        IF cAuthServer <> EnvironmentCode
        THEN
          ASSIGN
              AuthenticationServerName = cAuthServer
              AuthenticationMethod     = cAuthMethod
              ReplicateNonGroupedUser  = FALSE
              CanAuthenticateLocally   = TRUE
              MaintainUserRoles        = TRUE.

      OTHERWISE
        AuthenticationMethod = cAuthMethod.
    END CASE. /* CASE cAuthMethod */

    IF AuthenticationMethod <> "":U
    THEN
    DO:
      ASSIGN
          cExternalConfig = miUtility:getPreference("mip_PrefConfig":U + cAuthMethod)

          cValue[1] = TRIM(mipEnv:miExpression:getNVPElement(cExternalConfig, "ReplicateNonGroupedUser":U,  "&":U, ":":U))
          cValue[2] = TRIM(mipEnv:miExpression:getNVPElement(cExternalConfig, "CanAuthenticateLocally":U,   "&":U, ":":U))
          cValue[3] = TRIM(mipEnv:miExpression:getNVPElement(cExternalConfig, "MaintainUserRoles":U,        "&":U, ":":U))
          cValue[4] = TRIM(mipEnv:miExpression:getNVPElement(cExternalConfig, "AuthenticationServerName":U, "&":U, ":":U)).

      ASSIGN
          cValue[1] = "":U WHEN (cValue[1] = ?)
          cValue[2] = "":U WHEN (cValue[2] = ?)
          cValue[3] = "":U WHEN (cValue[3] = ?)
          cValue[4] = "":U WHEN (cValue[4] = ?)

          AuthenticationServerName = cValue[4] WHEN (AuthenticationServerName = "":U)

          ReplicateNonGroupedUser  = (CAN-DO( "YES,TRUE":U, cValue[1]))
          CanAuthenticateLocally   = (CAN-DO(",YES,TRUE":U, cValue[2]))
          MaintainUserRoles        = (CAN-DO( "YES,TRUE":U, cValue[3])).
    END. /* IF AuthenticationMethod <> "":U */

    mip.agent:recordAgentStart().

    Initialized = TRUE.

    IF SESSION:REMOTE OR ShowStartupInfo
    THEN
      MESSAGE "Environment startup complete...":U.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL initializeEnvironment */

  /* -- METHOD: startApplication -------------------------------------------- */
  METHOD PUBLIC cls.mipapplication startApplication
    (INPUT ipcApplicationCode AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */

    RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgErr' 0 ? ? "'The Environment class does not support the ' + QUOTER(IF ipcApplicationCode = ? THEN '?':U ELSE TRIM(ipcApplicationCode)) + ' application at this time.'"}.

  END METHOD. /* METHOD PROTECTED LOGICAL startApplication */

  /* -- METHOD: setReturnValue ---------------------------------------------- */
	METHOD PUBLIC FINAL LOGICAL setReturnValue
	  (INPUT ipcReturnValue AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */
    DEFINE VARIABLE lErrorStatus  AS LOGICAL  NO-UNDO.

    /* Store and clear the ERROR-STATUS so we can reliably trap errors on the
       next statement. */
    ASSIGN
        lErrorStatus = {&ErrorStatus}
      NO-ERROR.

    RUN setReturnValue IN ghBaseEnvironmentProc
        (INPUT ipcReturnValue)
      NO-ERROR.

    IF {&ErrorStatus}
    THEN
      MESSAGE formatMessage({mip/inc/mipreturnvalue.i}, "":U)
        VIEW-AS ALERT-BOX.
    ELSE
      /* We deliberately do not use NO-ERROR as that will clear what we are trying to set... */
      IF lErrorStatus
      THEN
        ERROR-STATUS:ERROR = TRUE.

    RETURN TRUE.

	END METHOD. /* METHOD PUBLIC FINAL LOGICAL setReturnValue */

  /* -- METHOD: getStartupParam --------------------------------------------- */
	METHOD PUBLIC CHARACTER getStartupParam
	  (INPUT ipcParameterName AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose: To interogate both the session's startup parameters,
               interpreting them as comma-delimited name=value pairs-lists,
               looking for a parameter with the specified name. Preference
               is given to SESSION:PARAMETER and only failing to find a
               value will it check in SESSION:ICFPARAMETER.

        Notes: - We should never return an unknown value (?) from here.
  ----------------------------------------------------------------------------*/
      {mip/cls/inc/basemipenvironment.cls.i &Segment = getStartupParam}

	END METHOD. /* METHOD PUBLIC CHARACTER getStartupParam */

  /* -- METHOD: formatMessage ----------------------------------------------- */
	METHOD PUBLIC CHARACTER formatMessage
	  (INPUT ipcMessages      AS CHARACTER,
	   INPUT ipcWhatToReturn  AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:  Translate system formatted messages
  ----------------------------------------------------------------------------*/
    RETURN cls.miperror:formatMessage(INPUT ipcMessages,
                                      INPUT ipcWhatToReturn).
    {mip/inc/mipcatcherror.i}

	END METHOD. /* METHOD PUBLIC CHARACTER formatMessage */

  /* -- METHOD: registerObject ---------------------------------------------- */
  METHOD PUBLIC LOGICAL registerObject
    (INPUT ipcObjectID  AS CHARACTER,
     INPUT ipoObject    AS Progress.Lang.Object):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCollection  FOR ttObjectCollection.

    DEFINE VARIABLE cObjectClass AS CHARACTER   NO-UNDO.

    IF NOT VALID-OBJECT(ipoObject)
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 21}.

    ASSIGN
        ipcObjectID = (IF ipcObjectID = ? THEN "":U ELSE TRIM(ipcObjectID))
        cObjectClass = ipoObject:GetClass():TypeName
      .
    IF ipcObjectID = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 24}.

    FIND FIRST ttObjectCollection NO-LOCK
         WHERE ttObjectCollection.cObjectID    = ipcObjectID
           AND ttObjectCollection.cObjectClass = cObjectClass
      NO-ERROR.

    {&ResetError}

    IF AVAILABLE ttObjectCollection
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 25 ? ? ipcObjectID cObjectClass}.

    CREATE ttObjectCollection.

    ASSIGN
        ttObjectCollection.cObjectID    = ipcObjectID
        ttObjectCollection.cObjectClass = cObjectClass
        ttObjectCollection.oObject      = ipoObject
      .
    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL registerObject */

  /* -- METHOD: registerForCleanup ------------------------------------------ */
  METHOD PUBLIC LOGICAL registerForCleanup
    (INPUT ipoObject  AS Progress.Lang.Object):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCleanup FOR ttObjectCleanup.

    IF NOT VALID-OBJECT(ipoObject) OR CAN-FIND(FIRST ttObjectCleanup NO-LOCK
                                               WHERE ttObjectCleanup.oObject = ipoObject)
    THEN
      RETURN TRUE.

    CREATE ttObjectCleanup.

    ttObjectCleanup.oObject = ipoObject.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL registerForCleanup */

  /* -- METHOD: registerForCleanup ------------------------------------------ */
  METHOD PUBLIC LOGICAL registerForCleanup
    (INPUT iphObject  AS HANDLE):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCleanup FOR ttObjectCleanup.

    IF NOT VALID-HANDLE(iphObject) OR CAN-FIND(FIRST ttObjectCleanup NO-LOCK
                                               WHERE ttObjectCleanup.hObject = iphObject)
    THEN
      RETURN TRUE.

    CREATE ttObjectCleanup.

    ttObjectCleanup.hObject = iphObject.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL registerForCleanup */

  /* -- METHOD: unregisterObject -------------------------------------------- */
  METHOD PUBLIC LOGICAL unregisterObject
    (INPUT ipcObjectID    AS CHARACTER,
     INPUT ipcObjectClass AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCollection  FOR ttObjectCollection.

    ASSIGN
        ipcObjectClass = (IF ipcObjectClass = ? THEN "":U ELSE TRIM(ipcObjectClass))
        ipcObjectID    = (IF ipcObjectID    = ? THEN "":U ELSE TRIM(ipcObjectID))
      .
    IF ipcObjectID = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 24}.

    IF ipcObjectClass = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 26}.

    FIND FIRST ttObjectCollection NO-LOCK
         WHERE ttObjectCollection.cObjectID    = ipcObjectID
           AND ttObjectCollection.cObjectClass = ipcObjectClass
      NO-ERROR.
    {&ResetError}
    IF NOT AVAILABLE ttObjectCollection
    THEN
      {mip/inc/miplog.i 'mip_Cleanup' "SUBSTITUTE('WARNING: Could not find a ttObjectCollection to cleanup with an id of &1, and a class of &2  : ',ipcObjectID,ipcObjectClass)}
    ELSE
      DELETE ttObjectCollection.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL unregisterObject */

  /* -- METHOD: unregisterFromCleanup --------------------------------------- */
  METHOD PUBLIC LOGICAL unregisterFromCleanup
    (INPUT ipoObject  AS Progress.Lang.Object):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCleanup FOR ttObjectCleanup.

    FIND FIRST ttObjectCleanup NO-LOCK
         WHERE ttObjectCleanup.oObject = ipoObject
      NO-ERROR.

    {&ResetError}

    IF AVAILABLE ttObjectCleanup
    THEN
      DELETE ttObjectCleanup.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL unregisterFromCleanup */

  /* -- METHOD: unregisterFromCleanup --------------------------------------- */
  METHOD PUBLIC LOGICAL unregisterFromCleanup
    (INPUT iphObject  AS HANDLE):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCleanup FOR ttObjectCleanup.

    FIND FIRST ttObjectCleanup NO-LOCK
         WHERE ttObjectCleanup.hObject = iphObject
      NO-ERROR.

    {&ResetError}

    IF AVAILABLE ttObjectCleanup
    THEN
      DELETE ttObjectCleanup.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL unregisterFromCleanup */

  /* -- METHOD: getRegisteredObject ----------------------------------------- */
  METHOD PUBLIC Progress.Lang.Object getRegisteredObject
    (INPUT ipcObjectID    AS CHARACTER,
     INPUT ipcObjectClass AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCollection  FOR ttObjectCollection.

    ASSIGN
        ipcObjectClass = (IF ipcObjectClass = ? THEN "":U ELSE TRIM(ipcObjectClass))
        ipcObjectID    = (IF ipcObjectID    = ? THEN "":U ELSE TRIM(ipcObjectID))
      .
    IF ipcObjectID = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 24}.

    IF ipcObjectClass = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 26}.

    FIND FIRST ttObjectCollection NO-LOCK
         WHERE ttObjectCollection.cObjectID    = ipcObjectID
           AND ttObjectCollection.cObjectClass = ipcObjectClass
      NO-ERROR.

    IF NOT AVAILABLE ttObjectCollection
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 27 ? ? ipcObjectID ipcObjectClass}.

    RETURN ttObjectCollection.oObject.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC Progress.Lang.Object getRegisteredObject */

  /* -- METHOD: getRegisteredObjects ---------------------------------------- */
  METHOD PUBLIC CHARACTER getRegisteredObjects
    (INPUT ipcObjectIDPattern AS CHARACTER,
     INPUT ipcObjectClass     AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCollection  FOR ttObjectCollection.

    DEFINE VARIABLE cRegisteredObjects AS CHARACTER   NO-UNDO.

    ASSIGN
        ipcObjectClass     = (IF ipcObjectClass     = ? THEN "":U ELSE TRIM(ipcObjectClass))
        ipcObjectIDPattern = (IF ipcObjectIDPattern = ? THEN "":U ELSE TRIM(ipcObjectIDPattern))
      .
    IF ipcObjectClass = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 26}.

    FOR EACH ttObjectCollection NO-LOCK
       WHERE ttObjectCollection.cObjectClass = ipcObjectClass
         AND CAN-DO(ipcObjectIDPattern, ttObjectCollection.cObjectID)
      ON ERROR UNDO, THROW:

      cRegisteredObjects = cRegisteredObjects
                         + (IF cRegisteredObjects = "":U THEN "":U ELSE ",":U)
                         + ttObjectCollection.cObjectID.
    END. /* FOR EACH ttObjectCollection NO-LOCK */

    RETURN cRegisteredObjects.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC Progress.Lang.Object getRegisteredObjects */

  /* -- METHOD: getRequestHandler ------------------------------------------- */
  METHOD PUBLIC CHARACTER getRequestHandler
    ():
  /*----------------------------------------------------------------------------
    Purpose: To determine what the request handler (entry point) for this call
             was.
  ----------------------------------------------------------------------------*/
    DEFINE VARIABLE cRequestHandler AS CHARACTER  NO-UNDO INITIAL "Other":U.
    DEFINE VARIABLE cProgram        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iProgram        AS INTEGER    NO-UNDO INITIAL 1.

    cProgram = PROGRAM-NAME(iProgram).

    DO WHILE cProgram <> ? AND cRequestHandler = "Other":U :
      ASSIGN
          cProgram = REPLACE(cProgram, "~\":U, "/":U)
          cProgram = ENTRY(NUM-ENTRIES(cProgram, "/":U), cProgram, "/":U)
          cProgram = SUBSTRING(cProgram, 1, R-INDEX(cProgram, ".":U) - 1)
        .
      CASE cProgram:
        WHEN "ajax":U THEN cRequestHandler = "AJAX":U.
        WHEN "rest":U THEN cRequestHandler = "Rest":U.
        WHEN "run":U  THEN cRequestHandler = "Web":U.
      END CASE. /* CASE cProgram */

      ASSIGN
          iProgram = iProgram + 1
          cProgram = PROGRAM-NAME(iProgram)
        .
    END. /* DO WHILE AND cProgram <> ? AND cRequestHandler = "Other":U */

    RETURN cRequestHandler.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC CHARACTER getRequestHandler */

  /* -- METHOD: printItem --------------------------------------------------- */
  METHOD PUBLIC LOGICAL printItem
   (INPUT ipcPreferredPrinter AS CHARACTER,
    INPUT ipmMemPtr           AS MEMPTR):
  /*----------------------------------------------------------------------------
      Purpose:  PrintItem
  ----------------------------------------------------------------------------*/
    DEFINE VARIABLE cTempFilename     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFullFilename     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPreferenceName   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPreference       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFilePath         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFilename         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPrintCommand     AS CHARACTER  NO-UNDO.

    ASSIGN
        cPreferenceName = ipcPreferredPrinter + OPSYS
        cPreference     = mipEnv:miUtility:getPreference(cPreferenceName)
      .
    IF cPreference = ? OR cPreference = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgError':U 11 "'The printer preference &1 could not be found.'" ? QUOTER(cPreferenceName)}.

    ASSIGN
        cPrintCommand = mipEnv:miExpression:getNVPElement(cPreference, "PrintCommand":U, "~n":U , "=":U)
        cFilePath     = mipEnv:miExpression:getNVPElement(cPreference, "FilePath":U,     "~n":U , "=":U)

        cFilePath     = (IF cFilePath = ? THEN SESSION:TEMP-DIR ELSE RIGHT-TRIM(REPLACE(cFilePath, "~\":U, "/":U), "/":U))

        FILE-INFO:FILE-NAME = cFilePath
        cFilePath           = (IF FILE-INFO:PATHNAME = ? THEN cFilePath ELSE RIGHT-TRIM(REPLACE(cFilePath, "~\":U, "/":U), "/":U))

        cFullFilename = cFilePath + "/p" + GUID + "prn.tmp":U
      .
    IF cPrintCommand = ? OR cPrintCommand = "":U
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgError':U 11 "'The print command is not setup correctly.'"}.

    IF FILE-INFO:PATHNAME = ?
    THEN
      RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgError':U 11 "'The file path &1 configured on the &2 preference does not exist.'" ? QUOTER(cFilePath) QUOTER(cPreferenceName) }.

    DO WHILE cFullFilename <> ?:
      ASSIGN
          cFilename     = "p":U + GUID + "prn.tmp":U
          cTempFilename = cFilePath + "/":U + cFilename
          cFullFilename = SEARCH(cTempFilename)
        .
    END. /* DO WHILE cFullFilename <> ? */

    COPY-LOB FROM ipmMemPtr TO FILE cTempFilename NO-CONVERT.

    CASE OPSYS:
      WHEN "WIN32":U THEN OS-COMMAND SILENT VALUE("cd ":U + REPLACE(cFilePath, "/":U, "~\":U) + " && ":U + REPLACE(cPrintCommand, "[filename]":U, cFilename)).
      WHEN "UNIX":U  THEN OS-COMMAND SILENT VALUE("cd ":U +         cFilePath                 +  " ; ":U + REPLACE(cPrintCommand, "[filename]":U, cFilename)).
    END CASE. /* CASE OPSYS */

    IF OS-ERROR <> 0
    THEN
    CASE OS-ERROR:
      WHEN 1 THEN RETURN ERROR "OS Error - Not owner.".
      WHEN 2 THEN RETURN ERROR "OS Error - No such file or directory.".
      WHEN 3 THEN RETURN ERROR "OS Error - Interrupted system call.".
      WHEN 4 THEN RETURN ERROR "OS Error - I/O error.".
      WHEN 5 THEN RETURN ERROR "OS Error - Bad file number.".
      WHEN 6 THEN RETURN ERROR "OS Error - No more processes.".
      WHEN 7 THEN RETURN ERROR "OS Error - Not enough core memory.".
      WHEN 8 THEN RETURN ERROR "OS Error - Permission denied.".
      OTHERWISE   RETURN ERROR "OS Error":U.
    END CASE. /* CASE OS-ERROR */

    RETURN TRUE.

    {mip/inc/mipcatcherror.i &FINALLY = "
      IF SEARCH(cTempFilename) <> ? THEN OS-DELETE VALUE(cTempFilename).
    "}

  END METHOD. /* METHOD PUBLIC CHARACTER printItem */

  /* -- METHOD: purgeObjects ------------------------------------------------ */
  METHOD PUBLIC LOGICAL purgeObjects
    ():
  /*----------------------------------------------------------------------------
      Purpose: Do cleanup of registered objects
          Notes: I left ttObjectCollection as a seperate TEMP-TABLE.
                 We might get to a situation where not everything in
                 ttObjectCollection needs to be purged...
  ----------------------------------------------------------------------------*/
    DEFINE BUFFER ttObjectCollection  FOR ttObjectCollection.
    DEFINE BUFFER ttObjectCleanup     FOR ttObjectCleanup.

    FOR EACH ttObjectCleanup EXCLUSIVE-LOCK
      ON ERROR UNDO, THROW:

      FIND FIRST ttObjectCollection EXCLUSIVE-LOCK
           WHERE ttObjectCollection.oObject = ttObjectCleanup.oObject
        NO-ERROR.

      {&ResetError}

      IF AVAILABLE ttObjectCollection
      THEN
        DELETE ttObjectCollection.

      IF VALID-OBJECT(ttObjectCleanup.oObject) THEN DELETE OBJECT ttObjectCleanup.oObject.
      IF VALID-HANDLE(ttObjectCleanup.hObject) THEN DELETE OBJECT ttObjectCleanup.hObject.

      DELETE ttObjectCleanup.
    END. /* FOR EACH ttObjectCleanup EXCLUSIVE-LOCK */

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL purgeObjects */

  /* -- METHOD: objectHandle ------------------------------------------------ */
  METHOD PUBLIC Progress.Lang.Object objectHandle
    (INPUT ipcObjectReference  AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  ----------------------------------------------------------------------------*/
    DEFINE VARIABLE oObject AS Progress.Lang.Object NO-UNDO.

    ASSIGN
        ipcObjectReference = (IF ipcObjectReference = ?    THEN "":U ELSE TRIM(ipcObjectReference))
        oObject            = (IF ipcObjectReference = "":U THEN ?    ELSE SESSION:FIRST-OBJECT)
      .
    /* Try to find the correct object */
    DO WHILE VALID-OBJECT(oObject):
      IF STRING(oObject) = ipcObjectReference
      THEN
        RETURN oObject.

      oObject = oObject:NEXT-SIBLING.
    END. /* DO WHILE VALID-OBJECT(oObject) */

    RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 21 ? ?  ipcObjectReference}.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC Progress.Lang.Object objectHandle */

  /* -- METHOD: applicationRuntimeStatus ------------------------------------ */
  METHOD PUBLIC CHARACTER applicationRuntimeStatus
    (INPUT ipcApplicationCode AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:  Applications on demand will get an application running if it is
                configured, so we'll report it as running, even though it might
                not be... Schrodinger at work... :)
  ----------------------------------------------------------------------------*/
    RETURN (IF mip.applicationfactory:isApplicationConfigured(ipcApplicationCode)
            THEN "Running":U
            ELSE "NotStarted":U).

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC CHARACTER applicationRuntimeStatus */

  /* -- METHOD: serviceRuntimeStatus ---------------------------------------- */
  METHOD PUBLIC CHARACTER serviceRuntimeStatus
    (INPUT ipcServiceCode AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:  Services on demand will get a service running if it is
                configured, so we'll report it as running, even though it might
                not be... Schrodinger at work... again :)
  ----------------------------------------------------------------------------*/
    RETURN (IF mip.applicationfactory:isServiceConfigured(ipcServiceCode)
            THEN "Running":U
            ELSE "NotStarted":U).

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC CHARACTER serviceRuntimeStatus */

  /* -- METHOD: supports ---------------------------------------------------- */
  METHOD PUBLIC LOGICAL supports
    (INPUT ipcCapabilityList  AS CHARACTER):
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */
    DEFINE VARIABLE iLoop AS INTEGER  NO-UNDO.

    IF NUM-ENTRIES(ipcCapabilityList) > 1
    THEN
    DO iLoop = 1 TO NUM-ENTRIES(ipcCapabilityList):
      IF NOT CAN-DO(CapabilityNameList, ENTRY(iLoop, ipcCapabilityList))
      THEN
        RETURN FALSE.
    END. /* DO iLoop = 1 TO NUM-ENTRIES(ipcCapabilityList) */
    ELSE
      RETURN CAN-DO(CapabilityNameList, ipcCapabilityList).

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC LOGICAL supports */

  /* -- METHOD: getMipXmlConfigDataSet -------------------------------------- */
	METHOD PUBLIC FINAL LOGICAL getMipXmlConfigDataSet
	  (OUTPUT DATASET dsMipXmlConfig BIND):
  /*----------------------------------------------------------------------------
      Purpose:
  --------------------------------------------------------------------------- */

    RETURN TRUE.

	END METHOD. /* METHOD PUBLIC FINAL LOGICAL getMipXmlConfigDataSet */

  /* -- METHOD: xGetCurrentMipAppSrvRowId ----------------------------------- */
	METHOD PUBLIC FINAL ROWID xGetCurrentMipAppSrvRowId
	  ():
  /*----------------------------------------------------------------------------
        Purpose:
  --------------------------------------------------------------------------- */

    RETURN (IF AVAILABLE gbuf_applicationservice THEN ROWID(gbuf_applicationservice) ELSE ?).

	END METHOD. /* METHOD PUBLIC FINAL ROWID xGetCurrentMipAppSrvRowId */

  /* -- METHOD: xGetCurrentMipAppSrvRowId ----------------------------------- */
	METHOD PUBLIC FINAL ROWID xGetCurrentMipAppRowId
	  ():
  /*----------------------------------------------------------------------------
        Purpose:
  --------------------------------------------------------------------------- */

    RETURN (IF AVAILABLE gbuf_application THEN ROWID(gbuf_application) ELSE ?).

	END METHOD. /* METHOD PUBLIC FINAL ROWID xGetCurrentMipAppSrvRowId */

  /* -- METHOD: getCentralService ----------------------------------------- */
  METHOD PUBLIC cls.mipservice getCentralService
    (INPUT ipcServiceCode AS CHARACTER):
  /*----------------------------------------------------------------------------
    Notes: This API needs to change to determine where the Central environment is running.
           For now, seeing that WorkFlow runs in Central, we take the shortcut to return
           its service objects - until we can implement the proper mechanism.
  ----------------------------------------------------------------------------*/
   
    CASE ipcServiceCode:
      WHEN "miUtility":U THEN RETURN mipEnv:Workflow:wfmiUtility.
      WHEN "miUser":U    THEN RETURN mipEnv:Workflow:wfmiUser.
      WHEN "miSession":U THEN RETURN mipEnv:Workflow:wfmiSession.
      OTHERWISE 
      {mip/inc/mipthrowerror.i 'mip_MsgEnvErr' 42 "'Invalid service code provided: &1'" ? ipcServiceCode}
    END CASE. /* CASE ipcServiceCode */

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC cls.mipservice getCentralService */

END CLASS. /* CLASS mip.cls.basemipenvironment */



