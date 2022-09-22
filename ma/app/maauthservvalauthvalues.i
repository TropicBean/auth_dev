/*------------------------------------------------------------------------------
  maauthservvalauthvalues.i.i  MEDSTAR Medical Aid System
                               (c) Copyright 2020
                               MIP Holdings (Pty) Ltd
                               All rights reserved
                       
  Purpose:    Validate that Amount_Auth and Amount_Quantity values have only
              been entered where they can be. If not, then return an error.
  Parameters:  
  Notes:
------------------------------------------------------------------------------*/  
  
  DEFINE INPUT  PARAMETER ipdAmount                   AS  DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipiQuantity                 AS  INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER iplActivateAuthorisedValues AS  LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER iplHeaderValuesUnlimited    AS  LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER ipcHeaderValuesAllowed      AS  CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER cFieldName                  AS  CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER opcMessage                  AS  CHARACTER  NO-UNDO.

  /*
     Validate amount and quantity
  */
  IF  iplActivateAuthorisedValues = no THEN
  DO:
    IF (ipdAmount <> 0 OR ipiQuantity <> 0) THEN
    DO:
      ASSIGN opcMessage = "Amount and quantity Authorised may not be entered."
             cFieldName = IF ipdAmount <> 0
                          THEN "amount_auth":U
                          ELSE "quantity_auth":U.
      RETURN.
    END. /* IF ipdAmount <> 0 OR ipiQuantity <> 0 THEN */ /* IF iplActivateAuthorisedValues = no... */
  END.  /* IF iplActivateAuthorisedValues = no... */
  ELSE IF NOT iplHeaderValuesUnlimited
  THEN DO:
    /* Only an amount must be captured */
    IF  ipcHeaderValuesAllowed = "ma_acAuthHeadValAllowedAmt"
    THEN DO:
      ASSIGN opcMessage = "":U.
      IF  ipiQuantity <> 0
      THEN ASSIGN opcMessage = "Quantity authorised may not be entered. ":U
                  cFieldName = "quantity_auth":U.

      IF ipdAmount = 0
      THEN ASSIGN opcMessage = opcMessage + "Please enter amount authorised.":U
                  cFieldName = "amount_auth":U.

      IF opcMessage <> "":U
      THEN RETURN.
    END. /* IFipcHeaderValuesAllowed = "ma_acAuthHeadValAllowedAmt".... */


    /* Only quantity must be captured */
    IF  ipcHeaderValuesAllowed = "ma_acAuthHeadValAllowedQty"
    THEN DO:
      ASSIGN opcMessage = "":U.
      if ipdAmount <> 0
      THEN ASSIGN opcMessage = "Amount authorised may not be entered. ":U
                  cFieldName = "quantity_auth":U.

      IF  ipiQuantity = 0
      THEN ASSIGN opcMessage = opcMessage + "Please enter quantity authorised.":U
                  cFieldName = "quantity_auth":U.

      IF opcMessage <> "":U
      THEN RETURN.
    END. /* IFipcHeaderValuesAllowed = "ma_acAuthHeadValAllowedQty".... */

    /* An amount and quantity must be captured */
    IF  ipcHeaderValuesAllowed = "ma_acAuthHeadValAllowedAmtQty"
    AND  (ipdAmount = 0 OR ipiQuantity = 0)
    THEN DO:
      ASSIGN opcMessage = "Amount AND quantity must be entered. Please enter ":U
             cFieldName = "amount_auth":U.

      IF ipdAmount = 0 AND ipiQuantity = 0
      THEN ASSIGN opcMessage = opcMessage + "amount and quantity authorised.":U.
      ELSE IF ipdAmount = 0
      THEN ASSIGN opcMessage = opcMessage + "amount authorised.":U.
      ELSE IF ipiQuantity = 0
      THEN ASSIGN opcMessage = opcMessage + "quantity authorised.":U
                  cFieldName = "quantity_auth":U.
      IF opcMessage <> "":U
      THEN RETURN.
    END. /* IFipcHeaderValuesAllowed = "ma_acAuthHeadValAllowedAmtQty".... */
  END.  /* ELSE IF NOT iplHeaderValuesUnlimited */
  
  { mip/inc/mipcatcherror.i }
