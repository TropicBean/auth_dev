/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthratedetail INHERITS ma.cls.basemaauthratedetail:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthratedetail() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthratedetail */
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthratedetail(ipdAuthRateDetailObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthRateDetailObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthratedetail */
  
END CLASS. /* cls.maauthratedetail INHERITS ma.cls.basemaauthratedetail */




