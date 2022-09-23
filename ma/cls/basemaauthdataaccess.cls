/*------------------------------------------------------------------------
    Component: Healthcare SOA
    Author   : Andrewd

    Purpose  : Healthcare Authorisation Data Service
  ------------------------------------------------------------------------*/

CLASS ma.cls.basemaauthdataaccess INHERITS cls.mipservice:

  { sysadmma.i }
  
  { mip/inc/mipdefsharedclass.i }

  { ma/inc/maauthds.i           }
  
  { ma/inc/maauthflagvalueds.i  }
  
  DEFINE PUBLIC PROPERTY DataAccessStack AS HANDLE NO-UNDO GET. PROTECTED SET.

              
/* -- METHOD: initializeService -------------------------------------- */
  METHOD OVERRIDE PUBLIC LOGICAL initializeService
    ():
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
    DEFINE VARIABLE lSuccess  AS LOGICAL  NO-UNDO.

    ASSIGN
        lSuccess        = SUPER:initializeService()

        DataAccessStack = ObjectStack.

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeService */
  
  METHOD PUBLIC LOGICAL fetchAuthorisations
    (INPUT ipdAuthObj   AS DECIMAL,
     INPUT ipcMemNum    AS CHARACTER,
     INPUT ipcWhatToGet AS CHARACTER,
     OUTPUT DATASET-HANDLE ophAuthDataset):
  /*-------------------------------------------------------------------*/

    {mip/inc/mipservicecall.i
        &ServiceProcedureName = fetchAuthorisations
        &ServiceCallSignature = "INPUT ipdAuthObj,
                                 INPUT ipcMemNum,   
                                 INPUT ipcWhatToGet,                              
                                 OUTPUT DATASET-HANDLE ophAuthDataset"
    }

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }

  END METHOD. /* METHOD PUBLIC LOGICAL fetchAuthorisations */

  /*
    Fetch Methods
  */
  { ma/inc/madataretrievalservice.i &Segment = "fetchDataMethod" &MethodName = "fetchAuthorisationDataset"  &SearchObject = "cls.maauthsearch"          }  
  
  /*
    Data Access Save Methods
  */
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuth"           &TempTableName = "tt_auth"             &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthCoding"     &TempTableName = "tt_auth_coding"      &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthCrosswalk"  &TempTableName = "tt_auth_crosswalk"   &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthDetail"     &TempTableName = "tt_auth_detail"      &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthProvider"   &TempTableName = "tt_auth_provider"    &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthEpisode"    &TempTableName = "tt_auth_episode"     &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthMCSavings"  &TempTableName = "tt_auth_mc_savings"  &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthCopay"      &TempTableName = "tt_auth_copay"       &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
  { ma/inc/madataretrievalservice.i &Segment = "saveDataAccessMethod" &MethodName = "saveAuthLimit"      &TempTableName = "tt_auth_limit"       &ErrorTableName = "tt_auth_error" &ResultTableName = "tt_auth_result"  }
    
/*-- METHOD: prepareAuthDataset -------------------------------------- */
  METHOD PUBLIC LOGICAL prepareAuthDataset
    (INPUT-OUTPUT DATASET-HANDLE iophDataset):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */  
    
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "prepareAuthDataset"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET-HANDLE iophDataset"} 

    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i 
      &FINALLY = "IF VALID-HANDLE(iophDataset) THEN DELETE OBJECT iophDataset."}

  END METHOD. /* METHOD PUBLIC LOGICAL prepareAuthDataset */
  
  
/*-- METHOD: reselectCrosswalk --------------------------------------- */
  METHOD PUBLIC LOGICAL reselectCrosswalk
    ( INPUT ipdAuthObj AS DECIMAL,
      INPUT-OUTPUT TABLE tt_auth_error ):
/*--------------------------------------------------------------------
        Purpose:  Delete the Auth records (except main provider)
                  for a Reselect Crosswalk (Authorisation Reguide)
          Notes:
  -------------------------------------------------------------------- */  
    { mip/inc/mipservicecall.i
        &ServiceProcedureName = "reselectCrosswalk"
        &ServiceCallSignature = "INPUT ipdAuthObj,
                                 INPUT-OUTPUT TABLE tt_auth_error BY-REFERENCE" }

    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
  END METHOD. /* METHOD PUBLIC LOGICAL reselectCrosswalk */


/*-- METHOD: validateAuthorisation ----------------------------------- */
  METHOD PUBLIC LOGICAL validateAuthorisation
    (INPUT-OUTPUT DATASET FOR dsAuthorisation):
/*--------------------------------------------------------------------
        Purpose:
          Notes:
  -------------------------------------------------------------------- */
  
    {
     mip/inc/mipservicecall.i
        &ServiceProcedureName = "validateAuthorisation"
        &ServiceCallSignature = "INPUT-OUTPUT DATASET dsAuthorisation BY-REFERENCE"} 
        
    RETURN TRUE.
    
    { mip/inc/mipcatcherror.i }
    
  END METHOD. /*METHOD PUBLIC LOGICAL validateAuthorisation*/
              
END CLASS. /* ma.cls.basemaauthdataaccess INHERITS cls.mipservice */



