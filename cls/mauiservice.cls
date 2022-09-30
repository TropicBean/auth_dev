CLASS cls.mauiservice INHERITS cls.mipservice:
  {mip/inc/mipdefsharedclass.i}

  { ma/inc/maerrortt.i &TEMP-TABLE-NAME = tt_error }  
  { ma/inc/maauthds.i }

 
  DEFINE PUBLIC PROPERTY maUIService           AS HANDLE NO-UNDO GET. PROTECTED SET.
  DEFINE PUBLIC PROPERTY RenderProcedureHandle AS HANDLE NO-UNDO GET. PROTECTED SET.

  /* -- CONSTRUCTOR ------------------------------------------------- */
  CONSTRUCTOR PUBLIC mauiservice():

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC mauiservice */

  /* -- METHOD: initializeService --------------------------------- */
    METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  --------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess              = SUPER:initializeService()

        maUIService           = ObjectStack
        RenderProcedureHandle = maUIService
        .

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */

  /* -- DESTRUCTOR -------------------------------------------------- */
  DESTRUCTOR PUBLIC mauiservice ():

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC mauiservice */
END CLASS. /* cls.mauiservice INHERITS cls.mipservice */