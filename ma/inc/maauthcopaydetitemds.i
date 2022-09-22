/* maauthcopaydetitemds.i MEDSTAR Medical Aid System
                          Healthcare Auth Copay Detail Item dataset for table hac_auth_copay_detail_item
                          (c) Copyright 2022
                          MIP Holdings (Pty) Ltd
                          All rights reserved                
*/
  
{sysadmma.i}

{ ma/inc/maauthcopaydetitemtt.i  &TEMP-TABLE-NAME = "tt_auth_copay_detail_item"        }
{ ma/inc/maerrortt.i             &TEMP-TABLE-NAME = "tt_auth_copay_detail_item_error"  }
{ ma/inc/maresulttt.i            &TEMP-TABLE-NAME = "tt_auth_copay_detail_item_result" }

DEFINE DATASET dsAuthCopayDetailItem 
  FOR tt_auth_copay_detail_item, 
      tt_auth_copay_detail_item_error,
      tt_auth_copay_detail_item_result.
    

/* ***************************  Main Block  *************************** */

