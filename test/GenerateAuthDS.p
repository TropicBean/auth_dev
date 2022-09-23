  {mip/inc/mipdefshared.i}
  {ma/inc/maauthds.i} 
  
  DEFINE VARIABLE oAuthSearch  AS cls.maauthsearch   NO-UNDO.
  DEFINE VARIABLE cAuthNum AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.  

  ASSIGN 
    oAuthSearch = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE) 
    lSuccess    = oAuthSearch:fetchData().


FOR EACH tt_auth :
  MESSAGE tt_auth.auth_obj .
END. 

DATASET dsAuthorisation:EMPTY-DATASET() .


IF VALID-OBJECT(oAuthSearch) THEN DELETE OBJECT oAuthSearch .  

{mip/inc/mipcatcherror.i &MESSAGE=TRUE &FORMAT=TRUE}