
/* maauthgroupds.i MEDSTAR Medical Aid System
                 Healthcare auth auto dataset definition
                 (c) Copyright 2015 - 2020
                 MIP Holdings (Pty) Ltd
                 All rights reserved                
*/

/* ***************************  Definitions  ************************** */
{ ma/inc/maauthgrouptt.i       &TEMP-TABLE-NAME = "tt_auth_group"    }    
{ ma/inc/maauthgroupdetailtt.i &TEMP-TABLE-NAME = "tt_auth_group_detail"   }
{ ma/inc/maerrortt.i           &TEMP-TABLE-NAME = "tt_auth_group_error"    }
{ ma/inc/maresulttt.i          &TEMP-TABLE-NAME = "tt_auth_group_result"   }

DEFINE DATASET dsAuthGroup 
  FOR tt_auth_group, 
      tt_auth_group_detail,
      tt_auth_group_result,
      tt_auth_group_error
  
  DATA-RELATION FOR tt_auth_group,tt_auth_group_detail  RELATION-FIELDS(tt_auth_group.auth_group_obj, tt_auth_group_detail.auth_group_obj) NESTED
  .
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */







