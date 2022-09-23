
/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthtypesearch INHERITS ma.cls.basemaauthtypesearch:

  CONSTRUCTOR PUBLIC maauthtypesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER().

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypesearch */

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthtypesearch
    (INPUT DATASET-HANDLE iphDataset):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(INPUT DATASET-HANDLE iphDataset BY-REFERENCE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthtypesearch */

END CLASS. /* CLASS cls.maauthtypesearch INHERITS ma.cls.basemaauthtypesearch */



