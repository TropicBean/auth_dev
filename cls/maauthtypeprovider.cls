
/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthtypeprovider INHERITS ma.cls.basemaauthtypeprovider:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthtypeprovider() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypeprovider*/
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthtypeprovider(ipdAuthTypeProviderObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthTypeProviderObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypeprovider */
  
END CLASS. /* cls.maauthtypeprovider INHERITS ma.cls.basemaauthtypeprovider */





