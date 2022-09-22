/* maauthgrpwobajxval.i  MEDSTAR Medical Aid System
                         Ajax Validation for maauthgroupwobsuper.p
                         (c) Copyright 1990 - 2021
                         MIP Holdings (Pty) Ltd
                         All rights reserved
*/  

DEFINE INPUT        PARAMETER ipcValidationArgument AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttValidation.

DEFINE VARIABLE cFilterFields      AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cFilterValues      AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cReturnFields      AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cReturnValues      AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cEntityMnemonic    AS CHARACTER             NO-UNDO.
                                                            
DEFINE VARIABLE dObjValue          AS DECIMAL               NO-UNDO.
DEFINE VARIABLE cCodeValue         AS CHARACTER             NO-UNDO.
DEFINE Variable cDisciplineCode    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cSubDisciplineCode AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cBaseRate          AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cArsRate           AS CHARACTER             NO-UNDO.
                                                            
DEFINE VARIABLE cWhereClause       AS CHARACTER             NO-UNDO.
DEFINE VARIABLE rRowid             AS ROWID                 NO-UNDO.
DEFINE VARIABLE iField             AS INTEGER               NO-UNDO.
DEFINE VARIABLE lDefault           AS LOGICAL               NO-UNDO.
DEFINE VARIABLE lSuccess           AS LOGICAL               NO-UNDO.
DEFINE VARIABLE lAjaxError         AS LOGICAL               NO-UNDO.
DEFINE VARIABLE lSetFields         AS LOGICAL               NO-UNDO.

DEFINE VARIABLE oSearchObject      AS ma.cls.matariffsearch NO-UNDO.

DATASET dsTariff:HANDLE:EMPTY-DATASET().                         
ASSIGN
   cFilterFields = get-value('FldLst':U)
   cFilterValues = get-value('ValList':U)
   cReturnFields = get-value('RetFldList':U)
   oSearchObject = NEW ma.cls.matariffsearch(DATASET dsTariff:HANDLE).        

CREATE ttValidation.

DO iField = 1 TO NUM-ENTRIES(cFilterFields):

  CASE ENTRY(iField,cFilterFields):
    WHEN "[TableMnemonic]":U 
    THEN
      ASSIGN cEntityMnemonic = ENTRY(iField,cFilterValues).
      
    WHEN "[Rowid]":U 
    THEN
      ASSIGN rRowid = TO-ROWID(ENTRY(iField,cFilterValues)).
    
    WHEN "[Default]":U  
    THEN
      ASSIGN lDefault = IF ENTRY(iField,cFilterValues) = "YES":U 
                        THEN TRUE
                        ELSE FALSE.                                   
          
    WHEN "[CodeField]":U 
    THEN
      ASSIGN cCodeValue = ENTRY(iField,cFilterValues).
             
    WHEN "[Discipline]":U  
    THEN 
      ASSIGN cDisciplineCode = (IF ENTRY(iField,cFilterValues) <> ""
                                THEN ENTRY(iField,cFilterValues)
                                ELSE "000").
    
    WHEN "[SubDiscipline]":U  
    THEN 
      ASSIGN cSubDisciplineCode = (IF ENTRY(iField,cFilterValues) <> ""
                                   THEN ENTRY(iField,cFilterValues)
                                   ELSE "000").
      
    WHEN "[BaseRate]":U  
    THEN
      ASSIGN cBaseRate = ENTRY(iField,cFilterValues).
      
    WHEN "[ArsRate]":U  
    THEN
      ASSIGN cArsRate = ENTRY(iField,cFilterValues).
      
    WHEN "[ObjField]":U  
    THEN
      ASSIGN dObjValue = DECIMAL(ENTRY(iField,cFilterValues)).
  
  END CASE.

END. /*DO iField = 1 TO NUM-ENTRIES(cFilterFields):*/

/*
  We need to check if the obj has changed, in case the user selected another
  tariff with different discipline/basket/option from the lookup, but the 
  tariff-code stays the same.
*/

IF dObjValue <> 0.00 OR cCodeValue <> "":U THEN 
DO:

  CASE cEntityMnemonic:
    
    WHEN "htmtl":U OR WHEN "tariff":U THEN 
    DO:
      IF lDefault THEN 
      DO:
        
        EMPTY TEMP-TABLE tt_tariff_link.

        ASSIGN
          lSuccess = oSearchObject:SetCriteria("BufferList":U,"tt_tariff_link":U)
          lSuccess = oSearchObject:SetFilterCriteria("tt_tariff_link.tariff_code":U,         "=":U, cCodeValue)
          lSuccess = oSearchObject:SetFilterCriteria("tt_tariff_link.tariff_link_default":U, "=":U, lDefault)

          lSuccess = oSearchObject:fetchTariffData().

        FIND FIRST tt_tariff_link NO-LOCK NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF AVAILABLE tt_tariff_link
        THEN
          ASSIGN
            dObjValue = tt_tariff_link.tariff_link_obj.

      END. /* IF lDefault */

      IF rRowid <> ? THEN 
      DO:
        FIND FIRST htm_tariff_link NO-LOCK
             WHERE ROWID(htm_tariff_link) = rRowid
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        ASSIGN lSetFields = TRUE.
      END.  /* ELSE IF rRowid <> ? */
      ELSE IF NOT lDefault THEN
      DO:

        EMPTY TEMP-TABLE tt_tariff_link.

        ASSIGN lSuccess = oSearchObject:SetCriteria("BufferList":U,"tt_tariff_link":U)
               lSetFields = FALSE.

        IF cCodeValue <> "" 
        THEN
          ASSIGN lSuccess = oSearchObject:SetFilterCriteria("tt_tariff_link.tariff_code":U, "=":U, cCodeValue).

        IF cDisciplineCode <> "000" 
        THEN
          ASSIGN lSuccess = oSearchObject:SetFilterCriteria("tt_tariff_link.pr_type":U    , "=":U, cDisciplineCode).

        IF cBaseRate <> "" 
        THEN
          ASSIGN lSuccess = oSearchObject:SetFilterCriteria("tt_tariff_link.base_rate":U  , "=":U, cBaseRate).

        IF cArsRate <> "" 
        THEN
          lSuccess = oSearchObject:SetFilterCriteria("tt_tariff_link.ars_rate":U   , "=":U, cArsRate).

        ASSIGN lSuccess = oSearchObject:fetchTariffData().

        FIND FIRST tt_tariff_link NO-LOCK NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF AVAILABLE tt_tariff_link 
        THEN
          ASSIGN dObjValue = tt_tariff_link.tariff_link_obj.
        ELSE
          ASSIGN dObjValue = 0.

        IF dObjValue > 0 THEN
        DO:
          FIND FIRST htm_tariff_link NO-LOCK
               WHERE htm_tariff_link.tariff_link_obj = dObjValue
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
        END.  /* IF dObjValue > 0 THEN */
      END. /* ELSE IF NOT lDefault */
      ELSE
      DO:
        FIND FIRST htm_tariff_link
             WHERE htm_tariff_link.tariff_code         = cCodeValue
               AND htm_tariff_link.tariff_link_default = TRUE
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
      END. /* ELSE DO */

      IF AVAILABLE htm_tariff_link THEN
      DO:
        DO iField = 1 TO NUM-ENTRIES(cReturnFields):

          CASE ENTRY(iField,cReturnFields):

            WHEN "[Rowid]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                 + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                 + STRING(ROWID(htm_tariff_link)).
            WHEN "[ObjField]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                 + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                 + STRING(htm_tariff_link.tariff_link_obj).

            WHEN "[RecordCode]":U 
            THEN 
              ASSIGN cReturnValues = cReturnValues
                                 + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                 + STRING(htm_tariff_link.tariff_code).

            WHEN "[RecordDescription]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                 + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                 + STRING(htm_tariff_link.tariff_description).

            WHEN "[Discipline]":U THEN
            DO:
              IF NOT lDefault THEN
              DO:
                IF lSetFields 
                THEN
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + STRING(htm_tariff_link.pr_type).
                ELSE
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cDisciplineCode.
              END.  /* IF NOT lDefault THEN */
              ELSE
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "000".
            END.  /* WHEN "[Discipline]":U THEN */

            WHEN "[SubDiscipline]":U THEN
            DO:
              IF NOT lDefault THEN
              DO:
                IF lSetFields
                AND AVAILABLE htm_tariff_link
                AND INTEGER(htm_tariff_link.pr_type) > 0
                AND (cSubDisciplineCode <> "" OR cSubDisciplineCode <> "000":U) THEN
                DO:

                  FIND FIRST subdisc NO-LOCK
                       WHERE subdisc.pr-type      = INTEGER(htm_tariff_link.pr_type)
                         AND subdisc.subdisp-code = INTEGER(cSubDisciplineCode) 
                    NO-ERROR.

                  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

                  IF AVAILABLE subdisc 
                  THEN
                    ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + STRING(subdisc.subdisp-code).
                  ELSE
                    ASSIGN lAjaxError    = TRUE
                           cReturnValues = cReturnValues
                                         + (IF cReturnValues = "":U THEN "":U ELSE "|":U).
                END.  /* IF lSetFields AND AVAILABLE htm_tariff_link */
                ELSE
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cSubDisciplineCode.
              END.  /* IF NOT lDefault THEN */
              ELSE
                ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "000":U ELSE "|000":U).
            END.  /* WHEN "[SubDiscipline]":U THEN */

            WHEN "[BaseRate]":U THEN
            DO:
              IF NOT lDefault THEN
              DO:
                IF lSetFields 
                THEN
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + STRING(htm_tariff_link.base_rate).
                ELSE
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cBaseRate.
              END.  /* IF NOT lDefault THEN */
              ELSE
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "":U.
            END.  /* WHEN "[BaseRate]":U THEN */

            WHEN "[ArsRate]":U THEN
            DO:
              IF NOT lDefault THEN
              DO:
                IF lSetFields 
                THEN
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + STRING( htm_tariff_link.ars_rate).
                ELSE
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + cArsRate.
              END.  /* IF NOT lDefault THEN */
              ELSE
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "":U.
            END.  /* WHEN "[ArsRate]":U THEN */

          END CASE.  /* CASE ENTRY(iField,cReturnFields): */
        END. /* DO iField = 1 TO NUM-ENTRIES(cReturnFields) */

        ASSIGN
          ttValidation.cReturnValues = cReturnValues
          ttValidation.lValid        = TRUE
          ttValidation.cMessage      = "":U.

      END. /* IF AVAILABLE htm_tariff_link THEN */
      ELSE
        ASSIGN lAjaxError = TRUE.

      IF lAjaxError = FALSE 
      THEN
        ASSIGN
          ttValidation.cReturnValues = cReturnValues
          ttValidation.lValid        = TRUE
          ttValidation.cMessage      = "":U.
      ELSE
        ASSIGN
          ttValidation.cReturnValues = ""
          ttValidation.lValid        = FALSE
          ttValidation.cMessage      = SUBSTITUTE("Record could not be found for '&1'.",cCodeValue).

    END. /* WHEN "htmtl":U OR WHEN "tariff":U  */

    WHEN "hlmnl":U THEN 
    DO:
      IF rRowid <> ? 
      THEN
        FIND FIRST hlm_nappi_link NO-LOCK
             WHERE ROWID(hlm_nappi_link) = rRowid
          NO-ERROR.
      ELSE IF cCodeValue <> "" 
      THEN
        FIND FIRST hlm_nappi_link NO-LOCK
             WHERE hlm_nappi_link.nappi_code_prefix = INTEGER(cCodeValue)
          NO-ERROR.
      ELSE
        FIND FIRST hlm_nappi_link NO-LOCK
             WHERE hlm_nappi_link.nappi_link_obj = dObjValue
          NO-ERROR.

      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

      IF AVAILABLE hlm_nappi_link THEN
      DO:
        DO iField = 1 TO NUM-ENTRIES(cReturnFields):
          CASE ENTRY(iField,cReturnFields):
            WHEN "[Rowid]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(ROWID(hlm_nappi_link)).
            WHEN "[ObjField]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(hlm_nappi_link.nappi_link_obj).
            WHEN "[RecordCode]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(hlm_nappi_link.nappi_code_prefix).
            WHEN "[RecordDescription]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(hlm_nappi_link.nappi_description[1]).
            WHEN "[Discipline]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
            WHEN "[SubDiscipline]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
            WHEN "[BaseRate]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
            WHEN "[ArsRate]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
          END CASE.  /* CASE ENTRY(iField,cReturnFields): */
        END. /*DO iField = 1 TO NUM-ENTRIES(cReturnFields):*/

        ASSIGN
          ttValidation.cReturnValues = cReturnValues
          ttValidation.lValid        = TRUE
          ttValidation.cMessage      = "":U.

      END. /* IF AVAILABLE hlm_nappi_link THEN */
      ELSE
        ASSIGN
          ttValidation.cReturnValues = ""
          ttValidation.lValid        = FALSE
          ttValidation.cMessage      = SUBSTITUTE("Record could not be found for '&1'.",cCodeValue).

    END. /* WHEN "hlmnl":U  */
    
    WHEN "hlmcr":U THEN 
    DO:
      IF rRowid <> ? 
      THEN
        FIND FIRST hlm_crosswalk NO-LOCK
             WHERE ROWID(hlm_crosswalk) = rRowid
          NO-ERROR.
      ELSE IF cCodeValue <> "" 
      THEN
        FIND FIRST hlm_crosswalk NO-LOCK
             WHERE hlm_crosswalk.crosswalk_code = cCodeValue
          NO-ERROR.
      ELSE
        FIND FIRST hlm_crosswalk NO-LOCK
             WHERE hlm_crosswalk.crosswalk_obj = dObjValue
          NO-ERROR.
        
      { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }     

      IF AVAILABLE hlm_crosswalk THEN
      DO:
        DO iField = 1 TO NUM-ENTRIES(cReturnFields):
        
          CASE ENTRY(iField,cReturnFields):
            WHEN "[Rowid]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(ROWID(hlm_crosswalk)).
            WHEN "[ObjField]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(hlm_crosswalk.crosswalk_obj).
            WHEN "[RecordCode]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(hlm_crosswalk.crosswalk_code).
            WHEN "[RecordDescription]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + STRING(hlm_crosswalk.crosswalk_description).                                                
            WHEN "[Discipline]":U 
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
            WHEN "[SubDiscipline]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
            WHEN "[BaseRate]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
            WHEN "[ArsRate]":U
            THEN
              ASSIGN cReturnValues = cReturnValues
                                   + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                   + " ".
          END CASE.  /* CASE ENTRY(iField,cReturnFields): */
        END. /*DO iField = 1 TO NUM-ENTRIES(cReturnFields):*/

        ASSIGN 
           ttValidation.cReturnValues = cReturnValues
           ttValidation.lValid        = TRUE
           ttValidation.cMessage      = "":U.     
    
      END. /* IF AVAILABLE hlm_crosswalk THEN */
      ELSE 
         ASSIGN 
            ttValidation.cReturnValues = ""
            ttValidation.lValid        = FALSE
            ttValidation.cMessage      = SUBSTITUTE("Record could not be found for '&1'.",cCodeValue).  

    END. /* WHEN "hlmcr":U  */
  END CASE.
END.  /* IF dObjValue <> 0.00 OR cCodeValue <> "":U  */
ELSE DO:
  CASE cEntityMnemonic:
    WHEN "prtype":U THEN
    DO:
      IF cDisciplineCode <> "000":U THEN
      DO:
        FIND prtype NO-LOCK
             WHERE prtype.pr-type = INTEGER(cDisciplineCode)
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:138':U &ResetIgnoredErrors = TRUE }

        IF AVAILABLE prtype THEN
        DO:
          DO iField = 1 TO NUM-ENTRIES(cReturnFields):

            CASE ENTRY(iField,cReturnFields):
              WHEN "[Rowid]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + STRING(ROWID(prtype)).
              WHEN "[ObjField]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "0".
              WHEN "[RecordCode]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "".
              WHEN "[RecordDescription]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "".
              WHEN "[Discipline]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + STRING(prtype.pr-type,"999").

              WHEN "[SubDiscipline]":U THEN 
              DO:
                IF cSubDisciplineCode <> "000":U THEN 
                DO:
                  FIND FIRST subdisc NO-LOCK
                       WHERE subdisc.pr-type      = prtype.pr-type
                       AND   subdisc.subdisp-code = INTEGER(cSubDisciplineCode)
                    NO-ERROR.
  
                  { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }
  
                  IF AVAILABLE subdisc
                  THEN
                    ASSIGN cReturnValues = cReturnValues
                                         + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                         + STRING(subdisc.subdisp-code).
                  ELSE
                    ASSIGN lAjaxError    = TRUE
                           cReturnValues = cReturnValues
                                         + (IF cReturnValues = "":U THEN "":U ELSE "|":U).
                END.  /* IF cSubDisciplineCode <> "000":U THEN */
                ELSE
                  ASSIGN cReturnValues = cReturnValues
                                       + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                       + "".
              END.  /* WHEN "[SubDiscipline]":U */

              WHEN "[BaseRate]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + " ".
              WHEN "[ArsRate]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + " ".
            END CASE.  /* CASE ENTRY(iField,cReturnFields): */
          END. /*DO iField = 1 TO NUM-ENTRIES(cReturnFields):*/

          ASSIGN
             ttValidation.cReturnValues = cReturnValues
             ttValidation.lValid        = TRUE
             ttValidation.cMessage      = "":U.

        END. /* IF AVAILABLE prtype THEN */
        ELSE
           ASSIGN
              ttValidation.cReturnValues = ""
              ttValidation.lValid        = FALSE
              ttValidation.cMessage      = SUBSTITUTE("Record could not be found for '&1'.",cDisciplineCode).

      END.  /* IF cDisciplineCode <> "000":U THEN */
      ELSE IF cSubDisciplineCode <> "000":U THEN
      DO:
        FIND FIRST subdisc NO-LOCK
             WHERE subdisc.subdisp-code = INTEGER(cSubDisciplineCode)
          NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565':U &ResetIgnoredErrors = TRUE }

        IF AVAILABLE subdisc THEN
        DO:
          DO iField = 1 TO NUM-ENTRIES(cReturnFields):

            CASE ENTRY(iField,cReturnFields):
              WHEN "[Rowid]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + STRING(ROWID(subdisc)).
              WHEN "[ObjField]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "0".
              WHEN "[RecordCode]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "".
              WHEN "[RecordDescription]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "".
              WHEN "[Discipline]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + "000".
              WHEN "[SubDiscipline]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + STRING(subdisc.subdisp-code).
              WHEN "[BaseRate]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + " ".
              WHEN "[ArsRate]":U
              THEN
                ASSIGN cReturnValues = cReturnValues
                                     + (IF cReturnValues = "":U THEN "":U ELSE "|":U)
                                     + " ".
            END CASE.  /* CASE ENTRY(iField,cReturnFields): */
          END. /*DO iField = 1 TO NUM-ENTRIES(cReturnFields):*/

          ASSIGN
             ttValidation.cReturnValues = cReturnValues
             ttValidation.lValid        = TRUE
             ttValidation.cMessage      = "":U.

        END. /* IF AVAILABLE subdisc THEN */
        ELSE 
           ASSIGN
              ttValidation.cReturnValues = ""
              ttValidation.lValid        = FALSE
              ttValidation.cMessage      = SUBSTITUTE("Record could not be found for '&1'.",cSubDisciplineCode).
      END.  /* IF cSubDisciplineCode <> "000":U THEN */
    END.  /* WHEN "prtype":U THEN */

    OTHERWISE DO:
      ASSIGN
        ttValidation.cReturnValues = FILL("|":U,NUM-ENTRIES(cReturnFields,",":U) - 1)
        ttValidation.lValid        = TRUE
        ttValidation.cMessage      = "":U.
    END.  /* OTHERWISE DO */
  END CASE.
END. /* ELSE - IF cCodeValue <> "":U  */

{ mip/inc/mipcatcherror.i 
  &FINALLY = "IF VALID-OBJECT(oSearchObject) THEN DELETE OBJECT oSearchObject."}
