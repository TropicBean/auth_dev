/* maauthservcheckforupd.i  MEDSTAR Medical Aid System
                             Automatic rebuild detection
                             This include will be used by a rest request to check if
                             there are any updates to the specified authorisation from the
                             last know update and if so, the relevant container html will be
                             returned for a rebuild.
                             (c) Copyright 2020 - 2022
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/          
  DEFINE INPUT        PARAMETER ipdAuthObj AS DECIMAL NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRequest.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsResponse.

  DEFINE VARIABLE oAUSearch       AS cls.maauthsearch          NO-UNDO.
  DEFINE VARIABLE oFVSearch       AS cls.maauthflagvaluesearch NO-UNDO.
  DEFINE VARIABLE lChangeDetected AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE lSuccess        AS LOGICAL                   NO-UNDO.
  DEFINE VARIABLE cBufferList     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRebuildList    AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cResolvedName   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE iBuffer         AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iDataset        AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE hDataset        AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE hHSBuffer       AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE hTSBuffer       AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE hQuery          AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE dModDatetime    AS DATETIME                  NO-UNDO.


&IF {&DBDFMA} >= 010195 &THEN

  DATASET dsResponse:EMPTY-DATASET().
  DATASET dsAuthorisation:EMPTY-DATASET().


  IF ipdAuthObj <> 0.00 THEN
  DO:
    /* Timestamp buffer */
    ASSIGN hTSBuffer = TEMP-TABLE ttRequestTimestamp:DEFAULT-BUFFER-HANDLE.

    /* Fetch the relevant auth data so we can run through history records */
    oAUSearch  = NEW cls.maauthsearch(DATASET dsAuthorisation BY-REFERENCE).

    ASSIGN
       lSuccess = oAUSearch:setCriteria( INPUT "BufferList":U,                   /* We are only interested in bringing back the history data */
                                         INPUT "tt_auth,":U                     +
                                               "tt_auth_history,":U             +
                                               "tt_auth_coding_history,":U      +
                                               "tt_auth_provider_history,":U    +
                                               "tt_auth_detail_history,":U      +
                                               "tt_auth_crosswalk_history,":U   +
                                               "tt_auth_mc_savings_history":U)

       lSuccess = oAUSearch:setFilterCriteria("tt_auth.auth_obj":U, "=":U, ipdAuthObj).

    /* And fetch...*/
    oAUSearch:fetchData().

    /* Flag changes are also important */
    ASSIGN
       oFVSearch = NEW cls.maauthflagvaluesearch(DATASET dsAuthFlagValue BY-REFERENCE)

       lSuccess  = oFVSearch:SetCriteria(INPUT "BufferList":U, INPUT "tt_auth_flag_value,tt_auth_flag_value_history":U)

       lSuccess  = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_entity_mnemonic":U, "=":U, "hatau":U)
       lSuccess  = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_obj":U            , "=":U, ipdAuthObj)
       lSuccess  = oFVSearch:SetFilterCriteria("tt_auth_flag_value.owning_key":U            , "=":U, "":U)

       lSuccess  = oFVSearch:fetchData().

    /* Run through all the history records and check for any updates.           */
    /* Transaction block required required for dynamic FIND-FIRST EXCLUSIVE-LOCK*/
    DO iDataset = 1 TO 2:

      ASSIGN hDataset = (IF iDataset = 1
                         THEN DATASET dsAuthorisation:HANDLE
                         ELSE DATASET dsAuthFlagValue:HANDLE).

      BufferBlk:
      DO TRANSACTION iBuffer = 1 TO hDataset:NUM-BUFFERS:

        ASSIGN
           hHSBuffer     = hDataset:GET-BUFFER-HANDLE(iBuffer)

           cResolvedName = REPLACE(REPLACE(REPLACE(hHSBuffer:NAME, "_":U, "":U),"tt":U, "":U), "history":U, "":U). /*Only storing the bare minimum as a reference*/

        /* We are only interested in the history table for now */
        IF NOT hHSBuffer:NAME MATCHES "*_history*":U
        THEN
          NEXT BufferBlk.

        /* Find the equivalent timestamp record for the history record and if it doesnt exist create one now. */
        hTSBuffer:FIND-FIRST(SUBSTITUTE("WHERE &1.BufferName = '&2'":U, hTSBuffer:NAME, cResolvedName), EXCLUSIVE-LOCK) NO-ERROR.

        { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

        IF NOT hTSBuffer:AVAILABLE THEN
        DO:

          hTSBuffer:BUFFER-CREATE().

          ASSIGN
             hTSBuffer::BufferName = cResolvedName
             hTSBuffer::Timestamp  = ADD-INTERVAL(NOW, -30, "minutes":U).

        END. /*IF NOT hTSBuffer:AVAILABLE THEN*/


        ASSIGN lChangeDetected = FALSE.

        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hHSBuffer).
        hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK BY &1.change_date_time DESCENDING":U, hHSBuffer:NAME)). /* Query starting with the latest history record for this buffer */
        hQuery:QUERY-OPEN().
        hQuery:GET-FIRST().

        ResultBlk:
        DO WHILE NOT hQuery:QUERY-OFF-END:

          IF hHSBuffer:AVAILABLE THEN
          DO:

            /* If the timestamp record has a modified datetime less than that in the history record then we need to        */
            /* rebuild the container associated with this buffer otherwise we should already have an up to date container. */
            ASSIGN
               dModDatetime         = DATETIME(hHSBuffer::change_date_time)          /* Get the timestamp off of the latest history record */

               lChangeDetected      = DATETIME(hTSBuffer::Timestamp) < dModDatetime  /* If the timestamp of the timestamp record is before the lastest history */
                                                                                     /* record, then a refresh is required for this buffer                     */

               hTSBuffer::Timestamp = dModDatetime.                                  /* Store the latest timestamp on the records we pass to and from the UI   */

          END. /*IF hHSBuffer:AVAILABLE THEN*/

          /* We are only interested in the latest record so we are done with this buffer. */
          LEAVE ResultBlk.
        END. /*DO WHILE NOT hQuery:QUERY-OFF-END:*/

        /* Ok so we have identified that there was a change so we will list the relevant */
        /* container associated with the buffer so that it can be rebuilt.               */
        IF lChangeDetected THEN
        DO:
          FIND FIRST ttBufferMapping EXCLUSIVE-LOCK
               WHERE ttBufferMapping.BufferName = cResolvedName
            NO-ERROR.

          { mip/inc/mipthrowerror.i &IgnoreErrors = 'PROGRESS:565' &ResetIgnoredErrors = TRUE }

          IF AVAILABLE ttBufferMapping THEN
          DO:
            ASSIGN cRebuildList = cRebuildList
                                + (IF cRebuildList <> "":U
                                   THEN ",":U
                                   ELSE "":U)
                                + ttBufferMapping.ContainerCode.

          END. /*IF AVAILABLE ttBufferMapping THEN*/
        END. /*IF lChangeDetected THEN*/

        /* Cleanup dynamic query for this buffer */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
      END. /*DO iBuffer = 1 TO NUM-ENTRIES(cBufferList):*/
    END. /*DO iDataset = 1 TO 2:*/

    ASSIGN lChangeDetected = cRebuildList <> "":U.

    /* Store the html for the response */
    CREATE ttPayload.
    ASSIGN ttPayload.ReconstructContainers = cRebuildList
           ttPayload.DataModified          = lChangeDetected.

    VALIDATE ttPayload.

  END. /*IF ipdAuthObj <> 0.00 THEN*/

  /* We have a request timestamp temp-table and one for the response because we have two datasets, */
  /* one for request and one for response, so we will populate out response timestamp temp-table   */
  /* from the request timestamp we potentially updated above...                                    */
  TEMP-TABLE ttResponseTimestamp:COPY-TEMP-TABLE(TEMP-TABLE ttRequestTimestamp:HANDLE).

&ENDIF

  /* Cleanup query in case there was an issue and the cleanup was skipped.        */
  /* Make sure we restore output to the webstream.                                */
  /* Delete any temporary files which we created for the container html.          */
  /* Cleanup after datasets so we dont have data persisting for the current agent.*/
  { mip/inc/mipcatcherror.i
    &FINALLY = "IF VALID-OBJECT(oAUSearch) THEN DELETE OBJECT oAUSearch.
                IF VALID-OBJECT(oFVSearch) THEN DELETE OBJECT oFVSearch.

                IF VALID-HANDLE(hQuery) THEN
                DO:
                  IF hQuery:IS-OPEN
                  THEN
                    hQuery:QUERY-CLOSE().

                  DELETE OBJECT hQuery.
                END.

                DATASET dsAuthorisation:EMPTY-DATASET().
                DATASET dsAuthFlagValue:EMPTY-DATASET().
                "}

