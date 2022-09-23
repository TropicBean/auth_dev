/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthcopaycontrol INHERITS ma.cls.basemaauthcopaycontrol:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaycontrol() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaycontrol */
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaycontrol(ipdAuthCopayControlObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthCopayControlObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaycontrol */
  
END CLASS. /* cls.maauthcopaycontrol INHERITS ma.cls.basemaauthcopaycontrol */
