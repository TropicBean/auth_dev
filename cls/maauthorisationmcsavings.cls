
/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthorisationmcsavings INHERITS ma.cls.basemaauthorisationmcsavings:

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisationmcsavings() :
  /*--------------------------------------------------------------------
    Purpose:  
    Notes:
  ----------------------------------------------------------------------*/
    
    SUPER().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisationmcsavings */

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisationmcsavings
    (INPUT ipdAuthMCSavingsObj AS DECIMAL):
  /*--------------------------------------------------------------------
    Purpose: Constructor to build an instance with a record in focus 
    Notes:  
  ----------------------------------------------------------------------*/
    
    SUPER(INPUT ipdAuthMCSavingsObj).
         
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisationmcsavings */

END CLASS. /* cls.maauthorisationmcsavings INHERITS ma.cls.basemaauthorisationmcsavings */






