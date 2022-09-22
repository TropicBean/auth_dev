/* maauthupdatescheckds.i MEDSTAR Medical Aid System
                          Healthcare dataset definition
                          (c) Copyright 2018
                          MIP Holdings (Pty) Ltd
                          All rights reserved
*/

{ ma/inc/matimestamptt.i &TEMP-TABLE-NAME = ttRequestTimestamp  }
{ ma/inc/matimestamptt.i &TEMP-TABLE-NAME = ttResponseTimestamp }

DEFINE TEMP-TABLE ttBufferMapping NO-UNDO SERIALIZE-NAME "mapping":U
  FIELD ContainerId   AS CHARACTER SERIALIZE-NAME "containerid":U
  FIELD ContainerCode AS CHARACTER SERIALIZE-NAME "containercode":U
  FIELD BufferName    AS CHARACTER SERIALIZE-NAME "buffername":U
  INDEX idx1 BufferName.

DEFINE TEMP-TABLE ttPayload NO-UNDO SERIALIZE-NAME "payload":U
   FIELD ReconstructContainers AS CHARACTER SERIALIZE-NAME "reconstruct":U
   FIELD DataModified          AS LOGICAL   SERIALIZE-NAME "modified":U
   FIELD Data                  AS CLOB      SERIALIZE-NAME "data".

DEFINE DATASET dsResponse 
   FOR ttResponseTimestamp, 
       ttPayload.

DEFINE DATASET dsRequest 
   FOR ttRequestTimestamp, 
       ttBufferMapping.
