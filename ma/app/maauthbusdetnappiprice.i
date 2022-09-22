/* maauthbusdetnappiprice.i MEDSTAR Medical Aid System
                            Nappi price business logic on auth clinical detail line
                            (c) Copyright 1990 - 2020
                            MIP Holdings (Pty) Ltd
                            All rights reserved
*/
DEFINE VARIABLE cPriceNappiCode   AS CHARACTER            FORMAT "x(11)"                     NO-UNDO.
DEFINE VARIABLE dQuantity         AS DECIMAL   DECIMALS 2 FORMAT "->>>>9.99"                 NO-UNDO.
DEFINE VARIABLE iPriceNegNum      AS INTEGER              FORMAT  ">>>9"                     NO-UNDO.

DEFINE VARIABLE dNappiPrice       AS DECIMAL   DECIMALS 2 FORMAT  "->,>>>,>>9.99"            NO-UNDO.
DEFINE VARIABLE dUpdClaimedAmount AS DECIMAL   DECIMALS 2 FORMAT  "->,>>>,>>9.99"            NO-UNDO.
DEFINE VARIABLE cPriceError       AS CHARACTER            FORMAT "x(40)"                     NO-UNDO.


IF  btt_auth_detail.owning_entity_mnemonic  = "hlmnl"    /* Nappi is captured as the detail line entity. */
OR (btt_auth_detail.owning_entity_mnemonic  = "htmtl"    /* Tariff is captured as the detail line entity but with a nappi as a related entity. */
AND btt_auth_detail.related_entity_mnemonic = "hlmnl")
THEN DO:
  IF  btt_auth_detail.added_by_user = TRUE /* When the detail line was added by the user and not pulled in from the Crosswalk. */
  OR (btt_auth_detail.added_by_user = FALSE 
  AND btt_auth_detail.line_restriction <> "ma_acAuthLineRestrictionAmount")
  THEN DO:
      
    IF   btt_auth_detail.auth_detail_obj         <= 0 
    OR  (AVAILABLE buf_auth_detail 
    AND (btt_auth_detail.quantity_auth            <> buf_auth_detail.quantity_auth
    OR   btt_auth_detail.amount_auth              <> buf_auth_detail.amount_auth 
    OR   btt_auth_detail.start_date               <> buf_auth_detail.start_date 
    OR   btt_auth_detail.related_entity_mnemonic  <> buf_auth_detail.related_entity_mnemonic
    OR   btt_auth_detail.related_value            <> buf_auth_detail.related_value)) 
    THEN DO:
      ASSIGN cPriceNappiCode = IF btt_auth_detail.owning_entity_mnemonic = "hlmnl"
                               THEN btt_auth_detail.owning_alt_value
                               ELSE IF btt_auth_detail.owning_entity_mnemonic = "htmtl " AND
                                       btt_auth_detail.related_entity_mnemonic = "hlmnl"
                               THEN btt_auth_detail.related_value
                               ELSE ""
             dQuantity       = IF btt_auth_detail.quantity_auth = 0 /* There can be scenarios where quantity auth = 0 - lines with an unlimited value.  */
                               THEN 1
                               ELSE btt_auth_detail.quantity_auth
             /*
               Get the neg num from btt_auth_detail.negotiation_group and only pass in the negotiation 
               group number to getNappiNonCharge.
               The value in btt_auth_detail.negotiation_group is saved as "(" + neg_num + ":" + neggroup.name + ")". 
               Eg. "(3:MEDCLINIC)"
             */
             iPriceNegNum    = INTEGER(LEFT-TRIM(ENTRY(1,btt_auth_detail._negotiation_group,":":U),"(":U)).
                                                                
      mipEnv:Health:maMedical:calcNappiPrice(INPUT  goAuthorisation:InsurerObj,
                                             INPUT  goAuthorisation:OptionCode,
                                             INPUT  cPriceNappiCode,
                                             INPUT  btt_auth_detail.start_date,
                                             INPUT  dQuantity,
                                             INPUT  btt_auth_detail.claim_code,
                                             INPUT  btt_auth_provider.doc_num,
                                             INPUT  btt_auth_provider.pr_type,
                                             INPUT  iPriceNegNum,
                                             INPUT  0,
                                             OUTPUT dNappiPrice,
                                             OUTPUT dUpdClaimedAmount, 
                                             OUTPUT cPriceError).

      IF cPriceError <> "":U THEN
      DO:
        goErrorObject:addError(INPUT "hatad:":U + btt_auth_detail.owning_entity_mnemonic,  // ipcOwningEntityMnemonic
                               INPUT btt_auth_detail.auth_detail_obj,                      // ipdOwningEntityObj
                               INPUT "":U,                                                 // ipcOwningEntityKey
                               INPUT "related_value":U,                                    // ipcFieldName      
                               INPUT btt_auth_detail.line_number,                          // ipiLineNumber     
                               INPUT cPriceError,                                               // ipcMessageText    
                               INPUT "ERR":U).                                             // ipcMessageType 
      END.  // IF cPriceError <> "" THEN
      ELSE 
        IF btt_auth_detail.quantity_auth = 0                         /* Scenarios where quantity auth = 0 we should only update the item cost.  */ 
        THEN ASSIGN btt_auth_detail.item_cost   = dNappiPrice.
        ELSE ASSIGN btt_auth_detail.amount_auth = dNappiPrice
                    btt_auth_detail.item_cost   = dNappiPrice / btt_auth_detail.quantity_auth.
      
    END. /* IF btt_auth_detail.auth_detail_obj <= 0 */
  END. /* IF  btt_auth_detail.added_by_user = TRUE */

  /* 
    When the detail is pulled in from the Crosswalk and an amount is specified on the Crosswalk 
    the nappi pricing should not be run
  */
  IF btt_auth_detail.added_by_user = FALSE AND 
     btt_auth_detail.line_restriction = "ma_acAuthLineRestrictionAmount"
  THEN ASSIGN btt_auth_detail.item_cost = btt_auth_detail.amount_auth / btt_auth_detail.quantity_auth.

END. /* IF  btt_auth_detail.owning_entity_mnemonic = "hlmnl" */ 
