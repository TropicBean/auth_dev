/* maauthusagett.i   MEDSTAR Medical Aid System
                     Temp tables for Auth Usage
                     (c) Copyright 2020
                     MIP Holdings (Pty) Ltd
                     All rights reserved
*/

DEFINE TEMP-TABLE ttAuthUsage LIKE trfcostauth
  FIELD scheme-code AS INTEGER
  FIELD mem-num     AS CHARACTER
  FIELD dependant   AS INTEGER
  FIELD tariffList  AS CHARACTER.
  
{&comment}
DEFINE TEMP-TABLE ttCommunications
      FIELD cCommType      AS CHARACTER
      FIELD cTemplate      AS CHARACTER
      FIELD cEntity        AS CHARACTER
      FIELD cEntityKey     AS CHARACTER
      FIELD iSchGrp        AS INTEGER
      FIELD cResource      AS CHARACTER
      FIELD cAdditional    AS CHARACTER.
/{&comment}* */  
