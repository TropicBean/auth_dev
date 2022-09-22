/* maauthtyperebuildmaint.i MEDSTAR Medical Aid System
                            Save Authorisation Detail Record
                            (c) Copyright 1990 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/ 
DEFINE VARIABLE oControl       AS cls.mipwscontrol     NO-UNDO.

  ASSIGN
    oControl = goCntMaint:getControl("fcAuthTSeqKey":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("flSeqActive":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcAuthTypeSequenceKey":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("buSequenceBtn":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcPrefix":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimCodeUserObjs":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U
    oControl = goCntMaint:getControl("fcClaimCodeUsers":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimCodeUsersLkp":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimCodeRoleObjs":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimCodeRoles":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimCodeRolesLkp":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimTypeUserObjs":U + goCntMaint:ContainerCode) 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimTypeUsers":U + goCntMaint:ContainerCode)    
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimTypeUsersLkp":U + goCntMaint:ContainerCode) 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimTypeRoleObjs":U + goCntMaint:ContainerCode) 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimTypeRoles":U + goCntMaint:ContainerCode)    
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcClaimTypeRolesLkp":U + goCntMaint:ContainerCode) 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcExtType":U + goCntMaint:ContainerCode)           
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("chUnlimitedHeader":U + goCntMaint:ContainerCode)   
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcReasonKey":U + goCntMaint:ContainerCode)         
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("buReasonLkp":U + goCntMaint:ContainerCode)         
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusRoleObjs":U + goCntMaint:ContainerCode)            
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusRoles":U + goCntMaint:ContainerCode)         
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusRolesLkp":U + goCntMaint:ContainerCode)                                        
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdCondType":U + goCntMaint:ContainerCode)                                                    
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdCondTypeLkp":U + goCntMaint:ContainerCode)                                                                                   
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcDetailBase":U + goCntMaint:ContainerCode)          
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdUserObjs":U + goCntMaint:ContainerCode)                                    
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdUsers":U + goCntMaint:ContainerCode)                                                                         
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdUsersLkp":U + goCntMaint:ContainerCode)                                                                                                        
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdRoleObjs":U + goCntMaint:ContainerCode)  
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdRoles":U + goCntMaint:ContainerCode)                                       
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdRolesLkp":U + goCntMaint:ContainerCode)                                                                      
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcTypeAcronym":U + goCntMaint:ContainerCode)                                                                                                               
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("chAllowUpdates":U + goCntMaint:ContainerCode)                                                                                                                                                
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcHeaderValue":U + goCntMaint:ContainerCode)        
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("chUnlimitedHeader":U + goCntMaint:ContainerCode)                                      
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusDefault":U + goCntMaint:ContainerCode)                                                                          
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("flStatusReasonMandatory":U + goCntMaint:ContainerCode)                                                                                                    
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcNoteType":U + goCntMaint:ContainerCode)                                                                                                                                                   
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcReasonKey":U + goCntMaint:ContainerCode)             
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("buReasonLkp":U + goCntMaint:ContainerCode)                                               
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusUserObjs":U + goCntMaint:ContainerCode)                                                                            
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusUsers":U + goCntMaint:ContainerCode)                                                                                                                 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusUsersLkp":U + goCntMaint:ContainerCode)                                                                                                                                                
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusRoleObjs":U + goCntMaint:ContainerCode)       
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusRoles":U + goCntMaint:ContainerCode)                                            
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcStatusRolesLkp":U + goCntMaint:ContainerCode)                                                                           
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdCondition":U + goCntMaint:ContainerCode)                                                                                                               
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdConditionLkp":U + goCntMaint:ContainerCode)                                                                                                                                              
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdCondType":U + goCntMaint:ContainerCode)        
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdCondTypeLkp":U + goCntMaint:ContainerCode)                                       
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdObjs":U + goCntMaint:ContainerCode)                                                                                
    oControl:ControlToken = "Hidden":U 

    oControl = goCntMaint:getControl("fcIcds":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcIcdsLkp":U + goCntMaint:ContainerCode)
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcAuthAllService":U + goCntMaint:ContainerCode)                                                                                                           
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcDetailBase":U + goCntMaint:ContainerCode)                                                                                                                                                 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateBodyRegion":U + goCntMaint:ContainerCode) 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdUserObjs":U + goCntMaint:ContainerCode)                                   
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdUsers":U + goCntMaint:ContainerCode)                                                                        
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdUsersLkp":U + goCntMaint:ContainerCode)                                                                                                       
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActMoouthpartId":U + goCntMaint:ContainerCode)                                                                                                                                            
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdRoleObjs":U + goCntMaint:ContainerCode) 
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdRoles":U + goCntMaint:ContainerCode)                                      
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcEndDateUpdRolesLkp":U + goCntMaint:ContainerCode)                                                                     
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateEpisodeNum":U + goCntMaint:ContainerCode)                                                                                                       
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateDueDate":U + goCntMaint:ContainerCode)                                                                                                                                            
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateServiceType":U + goCntMaint:ContainerCode)     
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateLos":U + goCntMaint:ContainerCode)                                               
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateLosWeekendPass":U + goCntMaint:ContainerCode)                                                                      
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateAmPm":U + goCntMaint:ContainerCode)                                                                                                                  
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("flActivateCrosswalk":U + goCntMaint:ContainerCode)                                                                                                                                               
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("flActivateCopayment":U + goCntMaint:ContainerCode)  
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("flActivatePenalty":U + goCntMaint:ContainerCode)                                      
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcHealthOption":U + goCntMaint:ContainerCode)                                                                           
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcActivateHealth":U + goCntMaint:ContainerCode)                                                                                                           
    oControl:ControlToken = "Hidden":U

    oControl = goCntMaint:getControl("fcMntAuthTypeCorres":U + goCntMaint:ContainerCode)                                                                                                                                        
    oControl:ControlToken = "Hidden":U
    .

  { mip/inc/mipcatcherror.i }
