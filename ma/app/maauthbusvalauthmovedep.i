/* maauthbusvalauthmovedep.i  MEDSTAR Medical Aid System
                              Changing from Dependant 99 to a valid Dependant
                              (c) Copyright 2022
                              MIP Holdings (Pty) Ltd
                              All rights reserved
*/ 

FOR EACH btt_auth_coding NO-LOCK
    WHERE btt_auth_coding.auth_obj = btt_auth.auth_obj:

  IF btt_auth_coding.record_action <> "DELETE":U 
  THEN
    ASSIGN btt_auth_coding.record_action = "MODIFY":U.

END. /* FOR EACH btt_auth_coding */

FOR EACH bbt_auth_detail NO-LOCK
    WHERE btt_auth_detail.auth_obj = btt_auth.auth_obj:

  IF btt_auth_detail.record_action <> "DELETE":U 
  THEN
    ASSIGN btt_auth_detail.record_action = "MODIFY":U.

END. /* FOR EACH bbt_auth_detail */ 
                                     

