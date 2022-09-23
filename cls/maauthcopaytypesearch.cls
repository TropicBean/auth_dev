/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
      Author:  GrahamW              
      Purpose: Auth Copay Type Search Class
----------------------------------------------------------------------------- */
CLASS cls.maauthcopaytypesearch INHERITS ma.cls.basemaauthcopaytypesearch:

  CONSTRUCTOR PUBLIC maauthcopaytypesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(). 

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaytypesearch*/

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaytypesearch
    (INPUT DATASET-HANDLE iphDataset):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
     SUPER(  INPUT DATASET-HANDLE iphDataset BY-REFERENCE  ) .

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaytypesearch*/

END CLASS. /* CLASS cls.maauthcopaytypesearch INHERITS ma.cls.basemaauthcopaytypesearch */
