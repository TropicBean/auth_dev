/* maauthcasemands.i MEDSTAR Medical Aid System
                     Healthcare Auth Case Manager Dataset
                     (c) Copyright 2018
                     MIP Holdings (Pty) Ltd
                     All rights reserved                
*/              
  
{sysadmma.i}

{ ma/inc/maauthcasemantt.i           &TEMP-TABLE-NAME = "tt_case_manager"                 }
{ ma/inc/maauthcasemanprovtt.i       &TEMP-TABLE-NAME = "tt_case_manager_provider"        }
{ ma/inc/maauthcasemaninactivett.i   &TEMP-TABLE-NAME = "tt_case_manager_inactive"        }
{ ma/inc/maneggrouptt.i              &TEMP-TABLE-NAME = "tt_neggroup"                     }
{ ma/inc/maprovidertt.i              &TEMP-TABLE-NAME = "tt_doctor"                       }
{ ma/inc/maauthcasemantt.i           &TEMP-TABLE-NAME = "tt_allocated_from_case_manager"  }
{ ma/inc/mausertt.i                  &TEMP-TABLE-NAME = "tt_case_manager_user"            }
{ ma/inc/maacronymtt.i               &TEMP-TABLE-NAME = "tt_inactive_reason"              }
{ ma/inc/maacronymtt.i               &TEMP-TABLE-NAME = "tt_user_profile"                 }

{ ma/inc/maerrortt.i                 &TEMP-TABLE-NAME = "tt_case_manager_error"           }
{ ma/inc/maresulttt.i                &TEMP-TABLE-NAME = "tt_case_manager_result"          }

DEFINE DATASET dsAuthCaseMan 
  FOR tt_case_manager, 
      tt_case_manager_provider,
      tt_case_manager_inactive, 
      tt_case_manager_user,
      tt_neggroup,
      tt_doctor,
      tt_allocated_from_case_manager,
      tt_inactive_reason,
      tt_user_profile,
      tt_case_manager_error,
      tt_case_manager_result
      
  DATA-RELATION CaseManProvider_CaseMan FOR tt_case_manager, tt_case_manager_provider
      RELATION-FIELDS(tt_case_manager.case_manager_obj,tt_case_manager_provider.case_manager_obj) NESTED
  
  DATA-RELATION CaseManProv_NegGroup FOR tt_case_manager_provider, tt_neggroup
      RELATION-FIELDS(tt_case_manager_provider.neg_num,tt_neggroup.neg-num) 
      
  DATA-RELATION CaseManProv_DocNum FOR tt_case_manager_provider, tt_doctor
      RELATION-FIELDS(tt_case_manager_provider.doc_num,tt_doctor.doc-num) 
  
  DATA-RELATION CaseManProv_AllocatedCaseMan FOR tt_case_manager_provider, tt_allocated_from_case_manager
      RELATION-FIELDS(tt_case_manager_provider.allocated_from_case_manager_obj,tt_allocated_from_case_manager.case_manager_obj) 
  
  DATA-RELATION CaseManInactive_CaseMan FOR tt_case_manager, tt_case_manager_inactive
      RELATION-FIELDS(tt_case_manager.case_manager_obj,tt_case_manager_inactive.case_manager_obj) NESTED
      
  DATA-RELATION CaseManInactive_Reason FOR tt_case_manager_inactive, tt_inactive_reason
      RELATION-FIELDS(tt_case_manager_inactive.inactive_reason,tt_inactive_reason.acronym_code)
        
  DATA-RELATION CaseMan_User FOR tt_case_manager, tt_case_manager_user
      RELATION-FIELDS(tt_case_manager.user_id,tt_case_manager_user.user_code) 
  
  DATA-RELATION CaseMan_UserProfile FOR tt_case_manager, tt_user_profile
      RELATION-FIELDS(tt_case_manager.user_profile,tt_user_profile.acronym_code) 
  
  .    
    

/* ***************************  Main Block  *************************** */
                                             
