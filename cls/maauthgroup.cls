
/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthgroup INHERITS ma.cls.basemaauthgroup:
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthgroup() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaAuthGroup */  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthgroup
    ( INPUT ipdAuthGroupObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthGroupObj).
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthgroup */
END CLASS. /* cls.maauthgroup INHERITS ma.cls.basemaauthgroup */




