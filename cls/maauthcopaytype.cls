
/* ------------------------------------------------------------------------------
  Component:  Healthcare SOA
     Author: GrahamW

    Purpose: This is a wrapper to provide easy configuration of Class over-rides.
--------------------------------------------------------------------------------- */
CLASS cls.maauthcopaytype INHERITS ma.cls.basemaauthcopaytype:

  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaytype() :
  /*------------------------------------------------------------------
      Purpose: Empty constructor   
        Notes:
  -------------------------------------------------------------------- */
    
    SUPER().
        
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaytype*/

                                                           
  /* -- CONSTRUCTOR -------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaytype
    ( INPUT ipdAuthCopayTypeObj AS DECIMAL ) :
  /*------------------------------------------------------------------
      Purpose:  
        Notes:  
  -------------------------------------------------------------------- */
    
    SUPER(ipdAuthCopayTypeObj).    
    
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaytype*/

END CLASS. /* cls.maauthcopaytype INHERITS ma.cls.basemaauthcopaytype*/
