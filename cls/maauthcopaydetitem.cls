/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthcopaydetitem INHERITS ma.cls.basemaauthcopaydetitem:

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaydetitem() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthcopaydetitem */
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaydetitem(ipdAuthCopayDetailItemObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthCopayDetailItemObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaydetitem */
  
END CLASS. /* cls.maauthcopaydetitem INHERITS ma.cls.basemaauthcopaydetitem */





