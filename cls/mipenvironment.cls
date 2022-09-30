/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.mipenvironment INHERITS mip.cls.basemipenvironment:
  {mip/inc/dsmipxmlconfig.i}

  /* -- CONSTRUCTOR ------------------------------------------------- */
  CONSTRUCTOR PUBLIC mipenvironment
    ():
  /*------------------------------------------------------------------------------
      Purpose:
        Notes:
  ------------------------------------------------------------------------------*/
    SUPER("":U, INPUT DATASET dsMipXmlConfig).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC mipenvironment */

  /* -- CONSTRUCTOR ------------------------------------------------- */
  CONSTRUCTOR PUBLIC mipenvironment
    (INPUT  DATASET dsMipXmlConfig):
  /*------------------------------------------------------------------------------
      Purpose:
        Notes:
  ------------------------------------------------------------------------------*/
    SUPER("":U, INPUT DATASET dsMipXmlConfig).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC mipenvironment */

  /* -- CONSTRUCTOR ------------------------------------------------- */
  CONSTRUCTOR PUBLIC mipenvironment
    (INPUT  ipcRestartDetails AS  CHARACTER):
  /*------------------------------------------------------------------------------
      Purpose:
        Notes:
  ------------------------------------------------------------------------------*/
    SUPER(ipcRestartDetails, INPUT DATASET dsMipXmlConfig).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC mipenvironment */

  /* -- CONSTRUCTOR ------------------------------------------------- */
  CONSTRUCTOR PUBLIC mipenvironment
    (INPUT  ipcRestartDetails AS  CHARACTER,
     INPUT  DATASET dsMipXmlConfig):
  /*------------------------------------------------------------------------------
      Purpose:
        Notes:
  ------------------------------------------------------------------------------*/
    SUPER(INPUT ipcRestartDetails, INPUT  DATASET dsMipXmlConfig).

  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC mipenvironment */

END CLASS. /* cls.mipenvironment INHERITS mip.cls.basemipenvironment */
