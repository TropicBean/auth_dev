/*------------------------------------------------------------------------
    Component: Healthcare SOA
    Purpose  : Healthcare Authorisation Maintenance Service
  ------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthmaintenance INHERITS cls.mipservice:

  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i      }
                                     
  { ma/inc/maauthgroupds.i           }
  { ma/inc/maauthcopaytypeds.i       }
  { ma/inc/maauthruleds.i            }
  { ma/inc/maauthtypeds.i            }
  { ma/inc/maauthcasemands.i         }
  { ma/inc/maauthratecontrolds.i     }
  { ma/inc/maauthcopayds.i           }
  { ma/inc/maauthcopaydetitemds.i    }

  DEFINE TEMP-TABLE tt_copy_record
  FIELD old_auth_rate_control_obj   AS DECIMAL
  FIELD rate_provider_information   AS LOGICAL
  FIELD all_rate_detail_information AS LOGICAL
  FIELD cpt_information             AS LOGICAL
  FIELD cpt_exclusions              AS LOGICAL
  FIELD icd_information             AS LOGICAL
  FIELD icd_exclusions              AS LOGICAL
  FIELD flag_information            AS LOGICAL
  FIELD flag_exclusion              AS LOGICAL
  FIELD CW_information              AS LOGICAL
  FIELD CW_exclusion                AS LOGICAL
  FIELD override_ars_rate           AS CHARACTER.

  DEFINE PUBLIC PROPERTY AuthMaintenance AS HANDLE NO-UNDO GET. PROTECTED SET.

  /* -- METHOD: initializeService --------------------------------- */
  METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
  /*--------------------------------------------------------------------
        Purpose:
          Notes:
  --------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess        = SUPER:initializeService()

        AuthMaintenance = ObjectStack.

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */
  
  /* -- METHOD clearAuthRuleCache ------------------------------------ */
  METHOD PUBLIC LOGICAL clearAuthRuleCache
    (  ) :
  /*------------------------------------------------------------------
        Purpose   : Clear cached auth rule data.
        Parameters: 
  -------------------------------------------------------------------- */
    
    { mip/inc/mipservicecall.i &ServiceProcedureName = "clearAuthRuleCache"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* clearAuthRuleCache  */
     
  /* -- METHOD copyAuthRate -------------------------------- */
  METHOD PUBLIC LOGICAL copyAuthRate
    ( INPUT TABLE tt_copy_record,  
      INPUT-OUTPUT DATASET FOR dsAuthRateControl ) :
  /*------------------------------------------------------------------
        Purpose   : 
        Parameters: 
        Notes     : 
  -------------------------------------------------------------------- */

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "copyAuthRate"
         &ServiceCallSignature = "INPUT TABLE tt_copy_record,
                                  INPUT-OUTPUT DATASET dsAuthRateControl BY-REFERENCE"}

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* copyAuthRate  */

  /* -- METHOD getAuthRuleValidValues -------------------------------- */
  METHOD PUBLIC LOGICAL getAuthRuleValidValues
    ( INPUT  ipdInsurerObj      AS DECIMAL,  
      INPUT  ipiOptionCode      AS INTEGER,  
      INPUT  ipcRuleType        AS CHARACTER,
      INPUT  ipcRuleCode        AS CHARACTER,
      INPUT  ipdEffectiveDate   AS DATE,     
      OUTPUT oplValidRule       AS LOGICAL,  
      OUTPUT opcRuleValue       AS CHARACTER,
      OUTPUT opcRuleValidValues AS CHARACTER ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth rule with the specified parameters 
                    and return a logical to indicate if the rule was 
                    found or not and if avail, the relevant rule value
                    as well as the valid values for the rule.
        Parameters: 
        Notes     : There has been a signature changes to
                    getAuthRuleDetails in ma/app/maauthruleservicestack.p
                    that will allow us to return data to the auth rule
                    instance object while still mainting backwards
                    compatibilty with these (getAuthRuleValue) methods.
  -------------------------------------------------------------------- */
    DEFINE VARIABLE cRuleDescription AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dLinkAuthRuleObj AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dAuthRuleObj     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE lSystemOwned     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE tEndDate         AS DATE       NO-UNDO.

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "getAuthRuleDetails"
         &ServiceCallSignature = "INPUT-OUTPUT dAuthRuleObj,
                                  INPUT-OUTPUT ipdInsurerObj,
                                  INPUT-OUTPUT ipiOptionCode,
                                  INPUT-OUTPUT ipcRuleType,
                                  INPUT-OUTPUT ipcRuleCode,
                                  INPUT-OUTPUT ipdEffectiveDate,
                                        OUTPUT oplValidRule,
                                        OUTPUT opcRuleValue,
                                        OUTPUT opcRuleValidValues,
                                        OUTPUT dLinkAuthRuleObj,
                                        OUTPUT cRuleDescription,
                                        OUTPUT lSystemOwned,
                                        OUTPUT tEndDate"}

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* getAuthRuleValidValues  */
  
    /* -- METHOD getAuthRuleValue ------------------------------------ */
  METHOD PUBLIC LOGICAL getAuthRuleValue
    ( INPUT  ipdInsurerObj    AS DECIMAL,  
      INPUT  ipiOptionCode    AS INTEGER,  
      INPUT  ipcRuleType      AS CHARACTER,
      INPUT  ipcRuleCode      AS CHARACTER,
      INPUT  ipdEffectiveDate AS DATE,     
      OUTPUT oplValidRule     AS LOGICAL,  
      OUTPUT opcRuleValue     AS CHARACTER ) :
  /*------------------------------------------------------------------
        Purpose   : Find an auth rule with the specified parameters 
                    and return a logical to indicate if the rule was 
                    found or not and if avail, the relevant rule value.
        Parameters: 
        Notes     : There has been a signature changes to
                    getAuthRuleDetails in ma/app/maauthruleservicestack.p
                    that will allow us to return data to the auth rule
                    instance object while still mainting backwards
                    compatibilty with these (getAuthRuleValue) methods.
  -------------------------------------------------------------------- */
    DEFINE VARIABLE cRuleValidValues AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRuleDescription AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dLinkAuthRuleObj AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dAuthRuleObj     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE lSystemOwned     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE tEndDate         AS DATE       NO-UNDO.
    
    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "getAuthRuleDetails"
         &ServiceCallSignature = "INPUT-OUTPUT dAuthRuleObj,
                                  INPUT-OUTPUT ipdInsurerObj,   
                                  INPUT-OUTPUT ipiOptionCode,   
                                  INPUT-OUTPUT ipcRuleType,     
                                  INPUT-OUTPUT ipcRuleCode,     
                                  INPUT-OUTPUT ipdEffectiveDate,
                                        OUTPUT oplValidRule,   
                                        OUTPUT opcRuleValue,
                                        OUTPUT cRuleValidValues,
                                        OUTPUT dLinkAuthRuleObj,
                                        OUTPUT cRuleDescription,
                                        OUTPUT lSystemOwned,
                                        OUTPUT tEndDate"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* getAuthRuleValue  */
    
    /* -- METHOD getAuthRuleDetails ------------------------------------ */
  METHOD PUBLIC LOGICAL getAuthRuleDetails
    ( INPUT  ipdInsurerObj        AS DECIMAL,  
      INPUT  ipiOptionCode        AS INTEGER,  
      INPUT  ipcRuleType          AS CHARACTER,
      INPUT  ipcRuleCode          AS CHARACTER,
      INPUT  ipdEffectiveDate     AS DATE,     
      OUTPUT oplValidRule         AS LOGICAL,  
      OUTPUT opcRuleValue         AS CHARACTER,
      OUTPUT opdLinkAuthRuleObj   AS DECIMAL,
      OUTPUT opdAuthRuleObj       AS DECIMAL):
  /*------------------------------------------------------------------
        Purpose   : Find an auth rule with the specified parameters 
                    and return a logical to indicate if the rule was 
                    found or not and if avail, the relevant rule value.
        Parameters: 
        Notes     : There has been a signature changes to
                    getAuthRuleDetails in ma/app/maauthruleservicestack.p
                    that will allow us to return data to the auth rule
                    instance object while still mainting backwards
                    compatibilty with these (getAuthRuleValue) methods.
  -------------------------------------------------------------------- */
    DEFINE VARIABLE cRuleValidValues AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRuleDescription AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dAuthRuleObj     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE lSystemOwned     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE tEndDate         AS DATE       NO-UNDO.

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "getAuthRuleDetails"
         &ServiceCallSignature = "INPUT-OUTPUT dAuthRuleObj,
                                  INPUT-OUTPUT ipdInsurerObj,   
                                  INPUT-OUTPUT ipiOptionCode,   
                                  INPUT-OUTPUT ipcRuleType,     
                                  INPUT-OUTPUT ipcRuleCode,  
                                  INPUT-OUTPUT ipdEffectiveDate,
                                        OUTPUT oplValidRule,   
                                        OUTPUT opcRuleValue,
                                        OUTPUT cRuleValidValues,
                                        OUTPUT opdLinkAuthRuleObj,
                                        OUTPUT cRuleDescription,
                                        OUTPUT lSystemOwned,
                                        OUTPUT tEndDate"} 

    ASSIGN opdAuthRuleObj = dAuthRuleObj.
    
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }

  END METHOD.  /* getAuthRuleDetails  */

  METHOD PUBLIC LOGICAL getAuthRuleDetails
    ( INPUT-OUTPUT iopdAuthRuleObj    AS DECIMAL,
      INPUT-OUTPUT iopdInsurerObj     AS DECIMAL,
      INPUT-OUTPUT iopiOptionCode     AS INTEGER,
      INPUT-OUTPUT iopcRuleType       AS CHARACTER,
      INPUT-OUTPUT iopcRuleCode       AS CHARACTER,
      INPUT-OUTPUT iopdEffectiveDate  AS DATE,
            OUTPUT oplValidRule       AS LOGICAL,
            OUTPUT opcRuleValue       AS CHARACTER,
            OUTPUT opcRuleValidValues AS CHARACTER,
            OUTPUT opdLinkAuthRuleObj AS DECIMAL,
            OUTPUT opcRuleDescription AS CHARACTER,
            OUTPUT oplSystemOwned     AS LOGICAL,
            OUTPUT optEndDate         AS DATE ):
  /* ------------------------------------------------------------
    Purpose: Return data to set the properties on the auth rule
             instance object.
  ------------------------------------------------------------ */
    {mip/inc/mipservicecall.i
      &ServiceProcedureName = "getAuthRuleDetails"
      &ServiceCallSignature = "INPUT-OUTPUT iopdAuthRuleObj,
                               INPUT-OUTPUT iopdInsurerObj,   
                               INPUT-OUTPUT iopiOptionCode,   
                               INPUT-OUTPUT iopcRuleType,     
                               INPUT-OUTPUT iopcRuleCode,  
                               INPUT-OUTPUT iopdEffectiveDate,
                                     OUTPUT oplValidRule,   
                                     OUTPUT opcRuleValue,
                                     OUTPUT opcRuleValidValues,
                                     OUTPUT opdLinkAuthRuleObj,
                                     OUTPUT opcRuleDescription,
                                     OUTPUT oplSystemOwned,
                                     OUTPUT optEndDate"}     
    RETURN TRUE.
    {mip/inc/mipcatcherror.i}
  END METHOD. /* getAuthRuleDetails */

  /* -- METHOD validateCopayAuthStatus -------------------------------- */
  METHOD PUBLIC LOGICAL validateCopayAuthStatus
    ( INPUT  ipiCopayStatusCode AS INTEGER,
      OUTPUT oplValid           AS LOGICAL ) :
  /*------------------------------------------------------------------
        Purpose   : Validate auth status on co-payment
        Parameters:
        Notes     :
  -------------------------------------------------------------------- */

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "validateCopayAuthStatus"
         &ServiceCallSignature = "INPUT  ipiCopayStatusCode,
                                  OUTPUT oplValid"}

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* validateCopayAuthStatus  */

  /* -- METHOD copyAuthCopay -------------------------------- */
  METHOD PUBLIC LOGICAL copyAuthCopay
    ( INPUT  iplCopyDetails AS LOGICAL,
      INPUT-OUTPUT DATASET FOR dsAuthCopayControl ) :
  /*------------------------------------------------------------------
        Purpose   : Validate auth status on co-payment
        Parameters:
        Notes     :
  -------------------------------------------------------------------- */

    { mip/inc/mipservicecall.i
         &ServiceProcedureName = "copyAuthCopay"
         &ServiceCallSignature = "INPUT iplCopyDetails,
                                  INPUT-OUTPUT DATASET dsAuthCopayControl BY-REFERENCE" }

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD.  /* copyAuthCopay  */

  /*
    Fetch Methods
  */                                                                                                                                                            
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthCopayTypeDataset"       &SearchObject = "cls.maauthcopaytypesearch"    }  
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthGroupDataset"           &SearchObject = "cls.maauthgroupsearch"        }  
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthRuleDataset"            &SearchObject = "cls.maauthrulesearch"         }  
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthTypeDataset"            &SearchObject = "cls.maauthtypesearch"         }  
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchCaseManDataset"             &SearchObject = "cls.macasemansearch"          }  
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthRateDataset"            &SearchObject = "cls.maauthratesearch"         }
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthCopayDataset"           &SearchObject = "cls.maauthcopaysearch"        }
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthCopayDetailItemDataset" &SearchObject = "cls.maauthcopaydetitemsearch" }
  
  /*
    Save Methods
  */
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveAuthCopayType"       &DatasetName = "dsAuthCopayType"       }  
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveAuthGroup"           &DatasetName = "dsAuthGroup"           } 
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveAuthRule"            &DatasetName = "dsAuthRule"            }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveAuthType"            &DatasetName = "dsAuthType"            }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveCaseMan"             &DatasetName = "dsAuthCaseMan"         }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveAuthRate"            &DatasetName = "dsAuthRateControl"     }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "SaveAuthCopay"           &DatasetName = "dsAuthCopayControl"    }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataMethod" &MethodName = "saveAuthCopayDetailItem" &DatasetName = "dsAuthCopayDetailItem" }
  
END CLASS. /* ma.cls.basemaauthmaintenance INHERITS cls.mipservice */


