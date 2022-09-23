/*------------------------------------------------------------------------
    Component: Healthcare SOA
    Author   : Andrewd

    Purpose  : Healthcare Auth Business Rules Service
------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthdecisions INHERITS cls.mipservice:

  { sysadmma.i }
  { mip/inc/mipdefsharedclass.i }
  
  
  DEFINE PUBLIC PROPERTY AuthDecisions AS HANDLE NO-UNDO GET. PROTECTED SET.
  
  
  /* -- METHOD: initializeService ------------------------------------ */
  METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
  /*------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess      = SUPER:initializeService()

        AuthDecisions = ObjectStack.

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */
   
    
  { ma/inc/maauthbusinessrule.i &Segment = "BusinessRuleMethod" &MethodName = "highRiskIndicator" }
  

END CLASS. /* ma.cls.basemaauthdecisions INHERITS cls.mipservice */




