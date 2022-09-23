
/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthgroupdetail INHERITS ma.cls.basemaauthgroupdetail:
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthgroupdetail() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaAuthAutoDetail */  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthgroupdetail
    ( INPUT ipdAuthGroupDetailObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthGroupDetailObj).
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthautoDetail */
END CLASS. /* cls.maauthgroupdetail INHERITS ma.cls.basemaauthgroupdetail */





