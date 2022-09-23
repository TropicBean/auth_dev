
/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthrulesearch INHERITS ma.cls.basemaauthrulesearch:

  CONSTRUCTOR PUBLIC maauthrulesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER().

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthrulesearch */

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthrulesearch
    (INPUT DATASET-HANDLE iphDataset):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(INPUT DATASET-HANDLE iphDataset BY-REFERENCE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthrulesearch */

END CLASS. /* CLASS cls.maauthrulesearch INHERITS ma.cls.basemaauthrulesearch */




