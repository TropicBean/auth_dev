
/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthtypedetail INHERITS ma.cls.basemaauthtypedetail:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthtypedetail() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypedetail*/
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthtypedetail(ipdAuthTypeDetailObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthTypeDetailObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypedetail */
  
END CLASS. /* cls.maauthtypedetail INHERITS ma.cls.basemaauthtypedetail */





