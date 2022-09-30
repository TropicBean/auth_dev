/*------------------------------------------------------------------------------
    Component: Call Center
      Purpose: Call Center Application Class
  Description: This is the Application Class for Call Center
        Notes: INHERITS cls.mipapplication
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS cls.mipcallcenter INHERITS cls.mipapplication:
  {mip/inc/mipdefsharedclass.i}

  /* -- PROPERTIES ---------------------------------------------------------- */
  &SCOPED-DEFINE Segment ServiceProperty

  {cls/inc/mip.cls.i &PropertyName = ccDataService  &ServiceClass = cls.mipccdataservice}
  {cls/inc/mip.cls.i &PropertyName = ccUtility      &ServiceClass = cls.mipccutility    }
  {cls/inc/mip.cls.i &PropertyName = ccCall         &ServiceClass = cls.mipcccall       }
  {cls/inc/mip.cls.i &PropertyName = ccmiUtility    &ServiceClass = cls.miputility      }
  {cls/inc/mip.cls.i &PropertyName = ccmiSession    &ServiceClass = cls.mipsession      }
  {cls/inc/mip.cls.i &PropertyName = ccmiUser       &ServiceClass = cls.mipuser         }

  &UNDEFINE Segment

  /* -- BUILT-IN / REQUIRED METHODS ----------------------------------------- */
  /* -- METHOD: instantiateApplication -------------------------------------- */
  METHOD PUBLIC OVERRIDE LOGICAL instantiateApplication
    ():
  /*----------------------------------------------------------------------------
      Purpose:
        Notes:
  ----------------------------------------------------------------------------*/

    ASSIGN
        DynamicServiceClassList = "{&DynamicServiceClassList}":U
        DynamicServiceNameList  = "{&DynamicServiceNameList}":U.

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD PUBLIC OVERRIDE LOGICAL instantiateApplication */

   /* -- METHOD: initializeApplication -------------------------------------- */
  METHOD OVERRIDE PUBLIC LOGICAL initializeApplication
    ():
  /*----------------------------------------------------------------------------
      Purpose:
        Notes:
  ----------------------------------------------------------------------------*/

    SUPER:initializeApplication().

    /* A prerequesite of a distributed environment is that both ccmiUtility and ccmiSession are configured to run in the same place */
    IF ccmiSession:ConsumerOnly <> ccmiUtility:ConsumerOnly OR
       ccmiSession:ConsumerOnly <> ccmiUser:ConsumerOnly
    THEN 
       RETURN ERROR {mip/inc/miperrortext.i 'mip_MsgEnvErr':U 106 "'Configuration issue: miSession, miUtility and miUser must be configured to run in the same place. miSession:ConsumerOnly = &1, miUtility:ConsumerOnly = &2 and miUser:ConsumerOnly = &3 ':U" ? ccmiSession:ConsumerOnly ccmiUtility:ConsumerOnly ccmiUser:ConsumerOnly}.
    
    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeApplication */

  /* -- METHOD: serviceInitialized ------------------------------------------ */
  METHOD OVERRIDE LOGICAL serviceInitialized
    (INPUT ipoService AS cls.mipservice):
  /*----------------------------------------------------------------------------
      Purpose:
        Notes:
  ----------------------------------------------------------------------------*/
    ipoService:setParentSession(INPUT ccmiSession).

    RETURN TRUE.

    {mip/inc/mipcatcherror.i}

  END. /* METHOD OVERRIDE LOGICAL serviceInitialized */

END CLASS. /* CLASS cls.mipcallcenter INHERITS cls.mipapplication */
