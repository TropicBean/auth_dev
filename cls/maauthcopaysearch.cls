/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthcopaysearch INHERITS ma.cls.basemaauthcopaysearch:

  CONSTRUCTOR PUBLIC maauthcopaysearch 
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER().

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaysearch */

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaysearch
    (INPUT DATASET-HANDLE iphDataset):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(INPUT DATASET-HANDLE iphDataset BY-REFERENCE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaysearch */

END CLASS. /* CLASS cls.maauthcopaysearch INHERITS ma.cls.basemaauthcopaysearch */




