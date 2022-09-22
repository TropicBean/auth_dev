/* maauthbusvalcopay.i  MEDSTAR Medical Aid System
                        Validate Auth Co-payment Buffer
                        (c) Copyright 1990 - 2021
                        MIP Holdings (Pty) Ltd
                        All rights reserved
*/                      
DEFINE PARAMETER BUFFER btt_auth_copay FOR tt_auth_copay.

DEFINE VARIABLE oErrorObject AS cls.maerrorobject NO-UNDO.

ASSIGN oErrorObject = NEW cls.maerrorobject(TEMP-TABLE tt_auth_error:HANDLE).

IF AVAILABLE btt_auth_copay THEN
DO:
  
END. /*IF AVAILABLE btt_auth_copay THEN*/

{ mip/inc/mipcatcherror.i 
  &FINALLY = "IF VALID-OBJECT(oErrorObject) THEN DELETE OBJECT oErrorObject NO-ERROR."}



