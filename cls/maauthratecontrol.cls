/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthratecontrol INHERITS ma.cls.basemaauthratecontrol:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthratecontrol() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratecontrol */
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthratecontrol(ipdAuthRateControlObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthRateControlObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthratecontrol */
  
END CLASS. /* cls.maauthratecontrol INHERITS ma.cls.basemaauthratecontrol */




