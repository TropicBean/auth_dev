/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthsearch INHERITS ma.cls.basemaauthsearch:

  CONSTRUCTOR PUBLIC maauthsearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER().

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthsearch */

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthsearch
    (INPUT DATASET-HANDLE iphDataset):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(INPUT DATASET-HANDLE iphDataset BY-REFERENCE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthsearch */

END CLASS. /* CLASS cls.maauthsearch INHERITS ma.cls.basemaauthsearch */




