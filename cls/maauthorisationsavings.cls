/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
  
  NB: This class have been deprecated see maauthorisationmcsavings.cls
----------------------------------------------------------------------------- */
CLASS cls.maauthorisationsavings INHERITS ma.cls.basemaauthorisationsavings:
/*
  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisationsavings() :
  /*--------------------------------------------------------------------
    Purpose:  
    Notes:
  ----------------------------------------------------------------------*/
    
    SUPER().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisationsavings */

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisationsavings
    (INPUT ipdAuthSavingsObj AS DECIMAL):
  /*--------------------------------------------------------------------
    Purpose: Constructor to build an instance with a record in focus 
    Notes:  
  ----------------------------------------------------------------------*/
    
    SUPER(INPUT ipdAuthSavingsObj).
         
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisationsavings */
*/
END CLASS. /* cls.maauthorisationsavings INHERITS ma.cls.basemaauthorisationsavings */





