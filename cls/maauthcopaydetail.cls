/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthcopaydetail INHERITS ma.cls.basemaauthcopaydetail:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaydetail() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaydetail */
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaydetail(ipdauthcopayDetailObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdauthcopayDetailObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaydetail */
  
END CLASS. /* cls.maauthcopaydetail INHERITS ma.cls.basemaauthcopaydetail */
