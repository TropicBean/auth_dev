/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthtypecontrol INHERITS ma.cls.basemaauthtypecontrol:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthtypecontrol() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypecontrol*/
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthtypecontrol(ipdAuthTypeControlObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthTypeControlObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypecontrol */
  
END CLASS. /* cls.maauthtypecontrol INHERITS ma.cls.basemaauthtypecontrol */





