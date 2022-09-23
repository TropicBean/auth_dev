/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthorisationcoding INHERITS ma.cls.basemaauthorisationcoding:

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisationcoding() :
  /*--------------------------------------------------------------------
    Purpose:  
    Notes:
  ----------------------------------------------------------------------*/
    
    SUPER().
    
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisationcoding */

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC maauthorisationcoding
    (INPUT ipdAuthCodingObj AS DECIMAL):
  /*--------------------------------------------------------------------
    Purpose: Constructor to build an instance with a record in focus 
    Notes:  
  ----------------------------------------------------------------------*/
    
    SUPER(INPUT ipdAuthCodingObj).
         
    { mip/inc/mipcatcherror.i }
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthorisationcoding */

END CLASS. /* cls.maauthorisationcoding INHERITS ma.cls.basemaauthorisationcoding */




