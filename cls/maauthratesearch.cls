/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.maauthratesearch INHERITS ma.cls.basemaauthratesearch:

  CONSTRUCTOR PUBLIC maauthratesearch
    ():
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER().

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthratesearch */

  /* -- CONSTRUCTOR ---------------------------------------------------------- */
  CONSTRUCTOR PUBLIC maauthratesearch
    (INPUT DATASET-HANDLE iphDataset):
  /*-----------------------------------------------------------------------------
      Purpose:
  -----------------------------------------------------------------------------*/
    SUPER(INPUT DATASET-HANDLE iphDataset BY-REFERENCE).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC maauthratesearch */

END CLASS. /* CLASS cls.maauthratesearch INHERITS ma.cls.basemaauthratesearch */



