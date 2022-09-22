/* maauthcopaytypeds.i MEDSTAR Medical Aid System
                       Healthcare Adjustment Type Dataset
                       (c) Copyright 2021
                       MIP Holdings (Pty) Ltd
                       All rights reserved                
   Author:  GrahamW
*/
  
{sysadmma.i}

{ ma/inc/maauthcopaytypett.i   &TEMP-TABLE-NAME = "tt_auth_copay_type"        }
{ ma/inc/maerrortt.i           &TEMP-TABLE-NAME = "tt_auth_copay_type_error"  }
{ ma/inc/maresulttt.i          &TEMP-TABLE-NAME = "tt_auth_copay_type_result" }

DEFINE DATASET dsAuthCopayType
  FOR tt_auth_copay_type, 
      tt_auth_copay_type_error,
      tt_auth_copay_type_result.
    

/* ***************************  Main Block  *************************** */
