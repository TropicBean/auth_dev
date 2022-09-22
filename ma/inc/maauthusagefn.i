/* maauthusagefn.i   MEDSTAR Medical Aid System
                     Functions for Auth Usage
                     (c) Copyright 2020
                     MIP Holdings (Pty) Ltd
                     All rights reserved
*/
{ma/inc/maauthusagett.i &comment = {&comment}}

FUNCTION getTrfUsageLevel RETURNS LOGICAL
  (INPUT-OUTPUT TABLE ttAuthUsage):
  DEFINE VARIABLE cAlertList  AS CHARACTER INITIAL["O,R,W"]  NO-UNDO.
  DEFINE VARIABLE cAlert      AS CHARACTER                   NO-UNDO.
  DEFINE VARIABLE cBase       AS CHARACTER EXTENT 2          NO-UNDO.
  DEFINE VARIABLE cArs        AS CHARACTER EXTENT 2          NO-UNDO.
  DEFINE VARIABLE iAlert      AS INTEGER                     NO-UNDO.
  DEFINE VARIABLE iBase       AS INTEGER                     NO-UNDO.
    
  IF LOOKUP(ttAuthUsage.level-of-alert, cAlertList) = 0 THEN RETURN FALSE.
  IF ttAuthUsage.level-of-alert = "W" THEN RETURN TRUE.
  IF ttAuthUsage.scheme-code <> 00 
  THEN FIND FIRST schdate NO-LOCK 
            WHERE schdate.scheme-code  = ttAuthUsage.scheme-code
              AND schdate.effect-date <= ttAuthUsage.effect-date NO-ERROR.
  {&ResetError}
  IF AVAILABLE schdate 
  THEN ASSIGN cBase[2] = schdate.default-base-rate
              cArs[2]  = schdate.default-ars-rate.
  ASSIGN cBase[1] = ttAuthUsage.base-rate
         cArs[1]  = ttAuthUsage.ars-rate.
  LEVEL:
  DO iAlert = LOOKUP(ttAuthUsage.level-of-alert, cAlertList) + 1 TO 3:
    ASSIGN cAlert = ENTRY(iAlert,cAlertList).
    DO iBase = 1 TO 2:
      IF cBase[iBase] = "" THEN NEXT.
      IF NOT CAN-FIND(trfcostauth NO-LOCK
          WHERE trfcostauth.tariff-code       =  ttAuthUsage.tariff-code
            AND trfcostauth.base-rate         =  cBase[iBase]
            AND trfcostauth.ars-rate          =  cArs[iBase]
            AND trfcostauth.effect-date       <= ttAuthUsage.effect-date
            AND trfcostauth.pr-type           =  ttAuthUsage.pr-type
            AND trfcostauth.level-of-alert    =  cAlert
            AND YEAR(trfcostauth.effect-date) =  YEAR(ttAuthUsage.effect-date)) THEN NEXT.
      FIND LAST trfcostauth NO-LOCK
          WHERE trfcostauth.tariff-code       =  ttAuthUsage.tariff-code
            AND trfcostauth.base-rate         =  cBase[iBase]
            AND trfcostauth.ars-rate          =  cArs[iBase]
            AND trfcostauth.effect-date       <= ttAuthUsage.effect-date
            AND trfcostauth.pr-type           =  ttAuthUsage.pr-type
            AND trfcostauth.level-of-alert    =  cAlert
            AND ((cAlert = "R"
            AND trfcostauth.auth-on-usage-num <> ?)
             OR (cAlert <> "R"
            AND trfcostauth.auth-on-usage-num <> 99))
            AND YEAR(trfcostauth.effect-date) =  YEAR(ttAuthUsage.effect-date)
            USE-INDEX trfcostauth-key NO-ERROR.
      {&ResetError}
      IF AVAILABLE trfcostauth 
      THEN DO:
        ASSIGN ttAuthUsage.tariff-code        = trfcostauth.tariff-code       
               ttAuthUsage.base-rate          = trfcostauth.base-rate         
               ttAuthUsage.ars-rate           = trfcostauth.ars-rate          
               ttAuthUsage.effect-date        = trfcostauth.effect-date       
               ttAuthUsage.pr-type            = trfcostauth.pr-type           
               ttAuthUsage.level-of-alert     = trfcostauth.level-of-alert    
               ttAuthUsage.auth-on-usage-num  = trfcostauth.auth-on-usage-num 
               ttAuthUsage.effect-date        = trfcostauth.effect-date
               ttAuthUsage.level-message      = trfcostauth.level-message
               ttAuthUsage.level-default-note = trfcostauth.level-default-note.
        LEAVE LEVEL.
      END. /* IF AVAILABLE trfcostauth  */
    END. /* DO iBase = 1 TO 2 */
  END. /* LEVEL: */
  RETURN TRUE.
END FUNCTION.

FUNCTION getTrfAuthUsage RETURNS INTEGER
  (INPUT-OUTPUT TABLE ttAuthUsage):  
  DEFINE VARIABLE lOk         AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cTariffList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSchemeList AS CHARACTER   NO-UNDO.    
  DEFINE VARIABLE iCntUsage   AS INTEGER     NO-UNDO.   
  DEFINE VARIABLE ndx         AS INTEGER     NO-UNDO.
   
  DEFINE BUFFER btrfgrp FOR trfgrpcode. 
  DEFINE BUFFER sschext FOR schext.
  DEFINE BUFFER eclaim  FOR claim.      
         
  ASSIGN iCntUsage   = 0.
  ASSIGN lOk = getTrfUsageLevel(INPUT-OUTPUT TABLE ttAuthUsage) NO-ERROR.     
  IF ttAuthUsage.level-of-alert = "O" THEN RETURN iCntUsage.
    
  /* Now we need to check if this tariff is in a group */
  CHECK-TARIFF-GROUP:
  FOR EACH trfgrpcode NO-LOCK
     WHERE trfgrpcode.tariff-code = ttAuthUsage.tariff-code
     AND   trfgrpcode.base-rate   = ttAuthUsage.base-rate
     AND   trfgrpcode.ars-rate    = ttAuthUsage.ars-rate:
    FIND FIRST tariffgroup NO-LOCK
        WHERE tariffgroup.trfgrp-num = trfgrpcode.trfgrp-num
    NO-ERROR.
    IF tariffgroup.usage-lapse-days <> 0 THEN NEXT CHECK-TARIFF-GROUP.
    FOR EACH btrfgrp NO-LOCK
       WHERE btrfgrp.trfgrp-num = tariffgroup.trfgrp-num:
      IF LOOKUP(btrfgrp.tariff-code,ttAuthUsage.tariffList) = 0 
        THEN ASSIGN ttAuthUsage.tariffList = ttAuthUsage.tariffList + (IF ttAuthUsage.tariffList = "" THEN "" ELSE ",") + btrfgrp.tariff-code.
    END.
  END. /* for each trfgrpcode */
  IF ttAuthUsage.tariffList = "" 
  THEN ASSIGN ttAuthUsage.tariffList = ttAuthUsage.tariff-code.

  /* Count the usage of the tariff if set */
  IF ttAuthUsage.auth-on-usage-num <> ?  
  THEN DO:
    ASSIGN cSchemeList = STRING(ttAuthUsage.scheme-code).
    FIND FIRST sschext NO-LOCK 
         WHERE sschext.scheme-code = ttAuthUsage.scheme-code
    NO-ERROR.
    {&ResetError}
    IF NOT AVAILABLE sschext 
    THEN DO:
      ASSIGN ttAuthUsage.level-of-alert = "O".
      RETURN iCntUsage.
    END.

    /* check all old claims */ 
    FIND FIRST datalist NO-LOCK
         WHERE datalist.list-type = "TRFCOSTAUTH"
         AND   datalist.list-code = "COUNTALL"
         AND   datalist.link-from = "SCHEMES" NO-ERROR.
    {&ResetError}
    IF AVAILABLE datalist THEN DO:
        FIND FIRST datalist NO-LOCK
             WHERE datalist.list-type    = "TRFCOSTAUTH"
             AND   datalist.list-code    = "SCHEMES"
             AND   datalist.link-from    = "" 
             AND   datalist.description <> "" NO-ERROR.
        {&ResetError}
        IF AVAILABLE datalist THEN ASSIGN cSchemeList = datalist.description.
    END. /* FIND FIRST datalist */
    DO ndx = 1 TO NUM-ENTRIES(cSchemeList):
      FIND FIRST datalist NO-LOCK
           WHERE datalist.list-type = "TRFCOSTAUTH"
           AND   datalist.list-code = "TRFNAPPI"
           AND  LOOKUP(entry(1,datalist.link-from,"|"),ttAuthUsage.tariffList) > 0 NO-ERROR.
      {&ResetError}
      FOR EACH eclaim NO-LOCK
         WHERE eclaim.scheme-code        =  INTEGER(ENTRY(ndx,cSchemeList))
         AND   eclaim.mem-num            =  ttAuthUsage.mem-num  
         AND   eclaim.dependant          =  ttAuthUsage.dependant
         AND   eclaim.auth-num           =  0
         AND   eclaim.reference-auth-num =  ""
         AND   eclaim.treatplan-auth-num =  ""
         AND   YEAR(eclaim.claim-date)   =  YEAR(ttAuthUsage.effect-date)
         AND   eclaim.claim-code         <> sschext.claim-code[1]
         AND   LOOKUP(eclaim.tariff-code,ttAuthUsage.tariffList) > 0:
        IF eclaim.claimed >= 0 THEN
            ASSIGN iCntUsage = iCntUsage + (IF eclaim.units <> 0 THEN eclaim.units ELSE 1).
        ELSE
            ASSIGN iCntUsage = iCntUsage - (IF eclaim.units <> 0 THEN eclaim.units ELSE 1).
      END. /* for each eclaim */
      IF AVAILABLE datalist THEN DO: 
        FOR EACH mclaim NO-LOCK
             WHERE mclaim.scheme-code       = INTEGER(ENTRY(ndx,cSchemeList))
             AND   mclaim.mem-num           = ttAuthUsage.mem-num 
             AND   YEAR(mclaim.claim-date)  = YEAR(ttAuthUsage.effect-date)
             AND   LOOKUP(mclaim.nappi-code,datalist.description) > 0:
          FIND eclaim NO-LOCK
            WHERE eclaim.gen-claimnum       = mclaim.gen-claimnum
              AND eclaim.dependant          = ttAuthUsage.dependant
              AND eclaim.auth-num           =  0
              AND eclaim.reference-auth-num =  ""
              AND eclaim.treatplan-auth-num =  "" NO-ERROR.
          IF AVAILABLE eclaim THEN DO:
            IF eclaim.claimed >= 0 THEN
                ASSIGN iCntUsage = iCntUsage + 1.
            ELSE
                ASSIGN iCntUsage = iCntUsage - 1. 
          END.
        END. /* for each mclaim */
      END. /* IF AVAILABLE datalist */
    END. /* DO ndx = 1 to */
  END. /* IF ttAuthUsage.auth-on-usage-num <> ?  */
  
  RETURN iCntUsage.
END FUNCTION.  

FUNCTION getTrfAuthTemplate RETURNS CHARACTER
  (INPUT ipcMemNum AS CHARACTER):
  
  DEFINE BUFFER bdatalist FOR datalist.
  
  FIND FIRST memsch NO-LOCK
    WHERE memsch.mem-num = ipcMemNum
  NO-ERROR.
  {&ResetError}
  IF AVAILABLE memsch
  THEN DO:
    FIND bdatalist NO-LOCK
      WHERE bdatalist.list-type = "TRFCOSTAUTH"
      AND   bdatalist.link-from = "SCHEMES"
      AND   LOOKUP(STRING(memsch.scheme-code),bdatalist.description) > 0
      NO-ERROR.
    {&ResetError}
    IF AVAILABLE bdatalist 
    THEN
      RETURN bdatalist.list-code.
    ELSE
      RETURN "".
  END.
  ELSE
    RETURN "".
END FUNCTION.                             

FUNCTION setTrfAuthComm RETURNS LOGICAL
  (INPUT  ipcTariff-code    AS CHARACTER,
   INPUT  ipcLevel-of-alert AS CHARACTER,
   INPUT  ipiCntUsage       AS INTEGER,
   INPUT  ipcMemNum         AS CHARACTER,
   INPUT  ipiDependant      AS INTEGER,
   OUTPUT TABLE ttCommunications):
  DEFINE VARIABLE cCommType AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCommTemp AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cInitials AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSurname  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLinkFrom AS CHARACTER   NO-UNDO.

  ASSIGN cCommType  = ""
         cCommTemp  = ""
         cLinkFrom  = getTrfAuthTemplate(ipcMemNum).

  FIND LAST datalist NO-LOCK
      WHERE datalist.list-type = "TRFCOSTAUTH"
      AND   datalist.list-code = ipcTariff-code + "|" + ipcLevel-of-alert
      AND   datalist.link-from = cLinkFrom
      NO-ERROR.
  {&ResetError}
  IF AVAILABLE datalist 
    AND NUM-ENTRIES(datalist.description,"|") = 3 
  THEN DO:
    assign vc-sch-list = "".
    if num-entries(entry(1,datalist.description,"|"),"-") > 1 then do:
      do vi-sch-range = integer(entry(1,entry(1,datalist.description,"|"),"-")) to integer(entry(2,entry(1,datalist.description,"|"),"-")):
        assign vc-sch-list = vc-sch-list + string(vi-sch-range) + ",".
      end.
    end.
    IF ENTRY(1,datalist.description,"|") = "*" 
      OR LOOKUP(TRIM(STRING(ipiCntUsage,">>9")),ENTRY(1,datalist.description,"|")) > 0 
      OR LOOKUP(TRIM(STRING(ipiCntUsage,">>9")),vc-sch-list) > 0
    THEN DO:
      
             
      FIND memdet NO-LOCK WHERE memdet.mem-num = ipcMemNum NO-ERROR.
      {&ResetError}
      IF AVAILABLE memdet  AND
          ((memdet.cell-no <> "" OR memdet.cell-no <> ?) OR 
           (memdet.email   <> "" OR memdet.email <> ?))
      THEN DO:
        IF ENTRY(2,datalist.description,"|") = "SMS>EMAIL" 
        THEN DO:
          IF memdet.cell-no = "" OR memdet.cell-no = ? 
          THEN ASSIGN cCommType = "EMAIL"
                      cCommTemp = ENTRY(2,ENTRY(3,datalist.description,"|"), ">") NO-ERROR.
          ELSE ASSIGN cCommType = "SMS"
                      cCommTemp = ENTRY(1,ENTRY(3,datalist.description,"|"), ">") NO-ERROR.
        END.
        ELSE IF ENTRY(2,datalist.description,"|") = "EMAIL>SMS"
        THEN DO:
            IF memdet.email = "" OR memdet.email = ? 
            THEN ASSIGN cCommType = "SMS"
                        cCommTemp = ENTRY(2,ENTRY(3,datalist.description,"|"), ">") NO-ERROR.
            ELSE ASSIGN cCommType = "EMAIL"
                        cCommTemp = ENTRY(1,ENTRY(3,datalist.description,"|"), ">") NO-ERROR.
        END.
        ELSE ASSIGN cCommType  = ENTRY(2,datalist.description,"|")
                    cCommTemp  = ENTRY(3,datalist.description,"|") NO-ERROR.
      END. /* IF AVAILABLE memdet */
      ELSE ASSIGN cCommType  = ENTRY(2,datalist.description,"|")
                  cCommTemp  = ENTRY(3,datalist.description,"|") NO-ERROR.
                  
      FIND memdep NO-LOCK WHERE memdep.mem-num   = ipcMemNum
                            AND memdep.dependant = ipiDependant
      NO-ERROR.
      {&ResetError}
      IF AVAILABLE memdep
      THEN ASSIGN cInitials = memdep.initials
                  cSurname  = memdep.surname.
      ELSE IF AVAILABLE memdet 
        THEN ASSIGN cInitials = memdet.initials
                    cSurname  = memdet.surname.
                 
      CREATE ttCommunications.
      ASSIGN ttCommunications.cCommType   = cCommType
             ttCommunications.cTemplate   = cCommTemp
             ttCommunications.cEntity     = "member"
             ttCommunications.cEntityKey  = trim(ipcMemNum)
             ttCommunications.iSchGrp     = 1
             ttCommunications.cResource   = ""
             ttCommunications.cAdditional = "Initials=" + trim(cInitials)               + "&" +
                                            "Surname="  + trim(cSurname)                + "&" +
                                            "Usage="    + trim(string(ipiCntUsage,">>9")).
    END. /* if entry(1,datalist.description,"|") = "*" or ... */
  END. /* if available datalist and num-entries(datalist.description,"|") = 3 */
  RETURN TRUE.      
END FUNCTION.
