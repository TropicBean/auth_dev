/* maauthorisationdefs.i MEDSTAR Medical Aid System
                         Healthcare authorisation definitions
                         (c) Copyright 2015 - 2017
                         MIP Holdings (Pty) Ltd
                         All rights reserved                
*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE tt_authorisation 
  FIELD auth_obj AS DECIMAL.
  
DEFINE DATASET dsAuthorisation
  FOR tt_authorisation
  .  

