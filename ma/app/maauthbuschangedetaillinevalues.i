/* maauthbuschangedetaillinevalues.i  MEDSTAR Medical Aid System
                                      Save Authorisation Provider Record
                                      (c) Copyright 1990 - 2022
                                      MIP Holdings (Pty) Ltd
                                      All rights reserved
*/
DEFINE PARAMETER BUFFER btt_auth_detail FOR tt_auth_detail.

DEFINE VARIABLE lValidRule         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cCompareFields     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErrorMessage      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFieldName         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOldValue          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cQuery             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTrackingMessage   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRuleValue         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hdbAuthDetail      AS HANDLE      NO-UNDO.
DEFINE VARIABLE httAuthDetail      AS HANDLE      NO-UNDO.
DEFINE VARIABLE htt2AuthDetail     AS HANDLE      NO-UNDO.
DEFINE VARIABLE httQuery           AS HANDLE      NO-UNDO.
DEFINE VARIABLE htt2Query          AS HANDLE      NO-UNDO.
DEFINE VARIABLE iDetailLineCounter AS INTEGER     NO-UNDO.
DEFINE VARIABLE iField             AS INTEGER     NO-UNDO.
DEFINE VARIABLE lAcknowledge       AS LOGICAL     NO-UNDO.

DEFINE BUFFER btt1_auth_detail FOR tt_auth_detail.
DEFINE BUFFER btt2_auth_detail FOR tt_auth_detail.

DEFINE QUERY AuthDetailQuery FOR btt1_auth_detail SCROLLING .

/*
  Find the rule to determine if we should give a warning or warning acknowledgement
*/
mipEnv:Health:AuthMaintenance:getAuthRuleValue( INPUT  0,
                                                INPUT  0,
                                                INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                INPUT  "AuthGroupAutoCalcMessage":U,
                                                INPUT  TODAY,
                                                OUTPUT lValidRule,
                                                OUTPUT cRuleValue ).

ASSIGN lAcknowledge = IF cRuleValue = "WarnAck":U THEN TRUE ELSE FALSE.

/*
  Find the rule that lists all the fields that need to be compared and
  automatically updated for the auth group.
  If the rule is not setup, then there is no point in continuing.
*/
mipEnv:Health:AuthMaintenance:getAuthRuleValue( INPUT  0,
                                                INPUT  0,
                                                INPUT  "ma_acAuthRuleTypeAuthDetail":U,
                                                INPUT  "AuthGroupAutoUpdFields":U,
                                                INPUT  TODAY,
                                                OUTPUT lValidRule,
                                                OUTPUT cCompareFields ).
ASSIGN cTrackingMessage = "AuthGroupChangeLineValues - Rule AuthGroupAutoUpdFields:"
                        + "lValidRule=" + STRING(lValidRule) + " cCompareFields=" + cCompareFields.
{ ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

IF NOT lValidRule OR cCompareFields = "":U THEN
DO:
  ASSIGN
    cErrorMessage = "Please ensure that Auth Rule to compare and update Auth Group Fields, is setup correctly for rule type 'Auth Detail'." +
                    "[Help=Auth Rule Code: AuthGroupAutoUpdFields]":U.

    goErrorObject:addError(INPUT "hatad:" + btt_auth_detail.owning_entity_mnemonic, /* ipcOwningEntityMnemonic */
                           INPUT btt_auth_detail.auth_detail_obj,                   /* ipdOwningEntityObj      */
                           INPUT "":U,                                              /* ipcOwningEntityKey      */
                           INPUT btt_auth_detail.line_number,                       /* ipiLineNumber           */
                           INPUT cErrorMessage,                                     /* ipcMessageText          */
                           INPUT "ERR":U).                                          /* ipcMessageType          */
  RETURN.
END.  /* IF cCompareFields = "":U THEN */

/*
  Build the query to be used in query prepare.
  We want to step through all the auth detail records on the auth that are not
  being deleted, and for each record, we want to compare each field (listed in the
  rule above) with the same field on the other auth detail records for that auth group.
*/
ASSIGN httAuthDetail = BUFFER btt1_auth_detail:HANDLE
       httQuery      = QUERY AuthDetailQuery:HANDLE
       cQuery        = SUBSTITUTE("FOR EACH btt1_auth_detail NO-LOCK ":U +
                                  "WHERE btt1_auth_detail.auth_obj            = &1 ":U         +
                                  "AND   btt1_auth_detail.auth_group_obj      = &2 ":U         +
                                  "AND   btt1_auth_detail.auth_group_obj     <> 0 ":U          +
                                  "AND   btt1_auth_detail.quantity_los        = 0 ":U          +
                                  "AND   btt1_auth_detail.record_action      <> 'DELETE':U ":U +
                                  "AND   btt1_auth_detail.record_action      <> '':U ":U       +
                                  "AND   btt1_auth_detail._auth_group_updated = false ":U,
                                  goAuthorisation:AuthObj, btt_auth_detail.auth_group_obj).
httQuery:QUERY-PREPARE(cQuery).
httQuery:QUERY-OPEN().
httQuery:GET-FIRST().

/*
  Load all the records we'll be working with in to the query object so that we can use the NUM-RESULTS attribute on the query
*/
DO WHILE NOT httQuery:QUERY-OFF-END:
  httQuery:GET-NEXT().
END. /* DO WHILE NOT httQuery:QUERY-OFF */

/*
  If the query has returned more than one result, it would mean that there are multiple auth detaiils in this group which are being
  modified and we would be unable to determine which detail must get prioriry when we assign the field values . Leave the procedure now
  and do not run any further processing .
*/
ASSIGN cTrackingMessage = "AuthGroupChangeLineValues - httQuery:NUM-RESULTS=" + STRING(httQuery:NUM-RESULTS)
                        + " glSkipDetailLineValuesProcess=" + STRING(glSkipDetailLineValuesProcess).
{ ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

IF httQuery:NUM-RESULTS <> 1
OR glSkipDetailLineValuesProcess THEN
DO:
  /*
    Don't do any further processing on the detail line values on subsequent iterations
  */
  ASSIGN glSkipDetailLineValuesProcess = TRUE.

  RETURN.
END. //IF httQuery:NUM-RESULTS <> 1 THEN

httQuery:GET-FIRST().

/*
  Step through all the auth detail records selected by the httQuery.
*/
DO WHILE NOT httQuery:QUERY-OFF-END:
  IF httAuthDetail:AVAILABLE THEN
  DO:
    /*
      If more than one record needs to be changed, then rather raise an error
      and let the user manually fix the data.
    */
    IF iDetailLineCounter < 1
    OR goAuthorisation:AuthIncomplete THEN
    DO:
      ASSIGN iDetailLineCounter = iDetailLineCounter + 1.

      /*
        Fetch all other detail lines linked to the same auth group.
      */
      ASSIGN htt2AuthDetail = BUFFER btt2_auth_detail:HANDLE
             cQuery = SUBSTITUTE("FOR EACH btt2_auth_detail EXCLUSIVE-LOCK ":U
                                 + "WHERE  btt2_auth_detail.auth_obj        =  &1   ":U
                                 + "AND    btt2_auth_detail.auth_detail_obj <> &2   ":U
                                 + "AND    btt2_auth_detail.auth_group_obj  <> 0.00 ":U
                                 + "AND    btt2_auth_detail.auth_group_obj  =  &3   ":U
                                 + "AND    btt2_auth_detail.quantity_los    =  0    ":U,
                                 httAuthDetail::auth_obj,
                                 httAuthDetail::auth_detail_obj,
                                 httAuthDetail::auth_group_obj).
      CREATE QUERY htt2Query.
      htt2Query:SET-BUFFERS(htt2AuthDetail).
      htt2Query:QUERY-PREPARE(cQuery).
      htt2Query:QUERY-OPEN().

      htt2Query:GET-FIRST().

      IF htt2AuthDetail:AVAILABLE THEN
      DO:
        DO WHILE NOT htt2Query:QUERY-OFF-END:
          ASSIGN cTrackingMessage = SUBSTITUTE("AuthGroupChangeLineValues - Comparing current record (&1_&2-&3_&4) with record (&5_&6-&7_&8) in same auth group",
                                               STRING(httAuthDetail:BUFFER-FIELD("auth_detail_obj"):BUFFER-VALUE()),
                                               STRING(httAuthDetail:BUFFER-FIELD("owning_entity_mnemonic"):BUFFER-VALUE()),
                                               STRING(httAuthDetail:BUFFER-FIELD("owning_alt_value"):BUFFER-VALUE()),
                                               STRING(httAuthDetail:BUFFER-FIELD("auth_provider_obj"):BUFFER-VALUE()),
                                               STRING(htt2AuthDetail:BUFFER-FIELD("auth_detail_obj"):BUFFER-VALUE()),
                                               STRING(htt2AuthDetail:BUFFER-FIELD("owning_entity_mnemonic"):BUFFER-VALUE()),
                                               STRING(htt2AuthDetail:BUFFER-FIELD("owning_alt_value"):BUFFER-VALUE()),
                                               STRING(htt2AuthDetail:BUFFER-FIELD("auth_provider_obj"):BUFFER-VALUE())).
          { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

          /*
            Step through all the fields listed in the "AuthGroupAutoUpdFields"-rule value
          */
          DO iField = 1 TO NUM-ENTRIES(cCompareFields):

            ASSIGN cFieldName = TRIM(ENTRY(iField,cCompareFields)).

            /*
              If the htt2AuthDetail record was auth-created because of the auth group,
              and the line_restriction field is one of the fields that must be compared
              for the group, we don't want to apply the line restriction rules.
              The line_restriction, item_cost and amount_auth fields are a 'package', so
              if anyone of these three fields are compared, we don't want to apply the
              line restriction rules.
            */
            IF LOOKUP(cFieldName,"item_cost,amount_auth,line_restriction":U) > 0
            THEN
              ASSIGN htt2AuthDetail::_apply_line_restrict_rules = NO.

            IF httAuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE <>
               htt2AuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE THEN
            DO:

              ASSIGN
                cErrorMessage = SUBSTITUTE("The detail line values for '&1' are not the same, it will be automatically updated":U,
                                           httAuthDetail:BUFFER-FIELD(cFieldName):LABEL).

              goErrorObject:addError(INPUT "hatad:" + httAuthDetail::owning_entity_mnemonic, /* ipcOwningEntityMnemonic  */
                                     INPUT httAuthDetail::auth_detail_obj,                   /* ipdOwningEntityObj       */
                                     INPUT "":U,                                             /* ipcOwningEntityKey       */
                                     INPUT httAuthDetail::line_number,                       /* ipiLineNumber            */
                                     INPUT cErrorMessage,                                    /* ipcMessageText           */
                                     INPUT "WAR":U,                                          /* ipcMessageType           */
                                     INPUT lAcknowledge).                                    /* iplAcknowledge           */

              ASSIGN cOldValue = STRING(htt2AuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE)
                     htt2AuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE = httAuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE
                     htt2AuthDetail::record_action = "MODIFY":U
                     htt2AuthDetail::_auth_group_updated = TRUE
                     cTrackingMessage = SUBSTITUTE("AuthGroupChangeLineValues - Changed '&1' for auth_detail_obj=&2 from '&3' to '&4'":U,
                                                   cFieldName,STRING(htt2AuthDetail:BUFFER-FIELD("auth_detail_obj"):BUFFER-VALUE),
                                                   cOldValue,STRING(httAuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE)).
              { ma/inc/malogging.i &MessageGroup = "'ma_DebugAuth'" &LogMessage = cTrackingMessage }

            END.  // IF httAuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE <> htt2AuthDetail:BUFFER-FIELD(cFieldName):BUFFER-VALUE THEN
          END.  // DO iField = 1 TO NUM-ENTRIES(cCompareFields):

          htt2Query:GET-NEXT().
        END.  // DO WHILE NOT htt2Query:QUERY-OFF-END:
      END.  // IF htt2AuthDetail:AVAILABLE THEN
    END.  // IF iDetailLineCounter < 1
  END.  // IF httAuthDetail:AVAILABLE THEN

  httQuery:GET-NEXT().

END.  /* DO WHILE NOT hQuery:QUERY-OFF-END */

{ mip/inc/mipcatcherror.i
     &FINALLY = "IF VALID-HANDLE(httQuery) THEN
                 DO:
                   IF httQuery:IS-OPEN THEN httQuery:QUERY-CLOSE().
                   DELETE OBJECT httQuery.
                 END.  /* IF VALID-HANDLE(httQuery) THEN */
                 IF VALID-HANDLE(htt2Query) THEN
                 DO:
                   IF htt2Query:IS-OPEN THEN htt2Query:QUERY-CLOSE().
                   DELETE OBJECT htt2Query.
                 END.  /* IF VALID-HANDLE(htt2Query) THEN */
                 "
                 }
