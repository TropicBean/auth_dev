/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthorisation INHERITS ma.cls.basemaauthorisation:

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisation() :
  /*--------------------------------------------------------------------
    Purpose:  
    Notes:
  ----------------------------------------------------------------------*/
    
    SUPER().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisation */

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisation
    (INPUT ipdAuthorisationObj AS DECIMAL):
  /*--------------------------------------------------------------------
    Purpose: Constructor to build an instance with a record in focus 
    Notes:  
  ----------------------------------------------------------------------*/
    
    SUPER(INPUT ipdAuthorisationObj).
         
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisation */
  
  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisation
    (INPUT ipcAuthorisationNum AS CHARACTER):
  /*--------------------------------------------------------------------
    Purpose: Constructor to build an instance with a record in focus 
    Notes:  
  ----------------------------------------------------------------------*/
    
    SUPER(INPUT ipcAuthorisationNum).
         
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisation */

END CLASS. /* cls.maauthorisation INHERITS ma.cls.basemaauthorisation */



