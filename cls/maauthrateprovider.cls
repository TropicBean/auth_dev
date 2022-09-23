/*------------------------------------------------------------------------
    Component: Healthcare SOA
       Author: Mandla M

      Purpose: Auth Rate Provider Instance Class
------------------------------------------------------------------------*/

/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthrateprovider INHERITS ma.cls.basemaauthrateprovider:


  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthrateprovider() :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
       
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC basemaauthrateprovider */
  
  
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthrateprovider(ipdAuthRateProviderObj AS DECIMAL) :
  /*------------------------------------------------------------------
      Purpose: 
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthRateProviderObj).
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthrateprovider */
  
END CLASS. /* cls.maauthrateprovider INHERITS ma.cls.basemaauthrateprovider */





