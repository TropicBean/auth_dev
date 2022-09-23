/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthcopaydetitemsearch INHERITS ma.cls.basemaauthcopaydetitemsearch:

  CONSTRUCTOR PUBLIC maauthcopaydetitemsearch 
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER().

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaydetitemsearch */

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthcopaydetitemsearch
    (INPUT DATASET-HANDLE iphDataset):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(INPUT DATASET-HANDLE iphDataset BY-REFERENCE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthcopaydetitemsearch */

END CLASS. /* CLASS cls.maauthcopaydetitemsearch INHERITS ma.cls.basemaauthcopaydetitemsearch */





