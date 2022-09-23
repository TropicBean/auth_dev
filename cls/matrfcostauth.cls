/* -----------------------------------------------------------------------------
  This is a wrapper to provide easy configuration of Class over-rides.
----------------------------------------------------------------------------- */
CLASS cls.matrfcostauth INHERITS ma.cls.basematrfcostauth:

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC matrfcostauth() :
  /*--------------------------------------------------------------------
    Purpose:  
    Notes:
  ----------------------------------------------------------------------*/
    
    SUPER().
    
    {mip/inc/mipcatcherror.i}
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC matrfcostauth */

  /* -- CONSTRUCTOR ----------------------------------------------------*/
  CONSTRUCTOR PUBLIC matrfcostauth
    (INPUT ipcTariffCode AS CHARACTER,
     INPUT ipdEffectDate AS DATE,
     INPUT ipcPrType     AS CHARACTER,
     INPUT ipcBaseRate   AS CHARACTER,
     INPUT ipcARSRate    AS CHARACTER):
  /*--------------------------------------------------------------------
    Purpose: Constructor to build an instance with a record in focus 
    Notes:  
  ----------------------------------------------------------------------*/
    
    SUPER(INPUT ipcTariffCode,
          INPUT ipdEffectDate,
          INPUT ipcPrType,
          INPUT ipcBaseRate,
          INPUT ipcARSRate).
         
    {mip/inc/mipcatcherror.i}
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC matrfcostauth */

END CLASS. /* cls.matrfcostauth INHERITS ma.cls.basematrfcostauth */




