/*------------------------------------------------------------------------
    Component: Healthcare SOA
    Author   : HC

    Purpose  : Healthcare Auth UI Services
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthuiservice INHERITS cls.mipservice:

  { sysadmma.i }
  { mip/inc/mipdefsharedclass.i }

  
  DEFINE PUBLIC PROPERTY RenderProcedureHandle AS HANDLE NO-UNDO GET. PROTECTED SET.
                                 

  /* -- METHOD: initializeService -------------------------------------- */
  METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
    -------------------------------------------------------------------- */
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess              = SUPER:initializeService()

        RenderProcedureHandle = ObjectStack.

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */    

END CLASS. /* ma.cls.basemaauthuiservice INHERITS cls.mipservice */



