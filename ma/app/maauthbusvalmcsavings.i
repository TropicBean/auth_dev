
/* maauthbusvalmcsavings.i  MEDSTAR Medical Aid System
                            Validate Auth Managed Care Savings Buffer
                            (c) Copyright 1990 - 2017
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/                      
DEFINE PARAMETER BUFFER btt_auth_mc_savings FOR tt_auth_mc_savings.

DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.

ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

IF AVAILABLE btt_auth_mc_savings THEN
DO:
  
END. /*IF AVAILABLE btt_auth_mc_savings THEN*/

{ mip/inc/mipcatcherror.i 
  &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject."}


