/*------------------------------------------------------------------------
    File        : maAuthDet.i
    Purpose     : Include for Authorisation Details.
    Description :

    Syntax      :

     Date        Ref.        Author        Description
    ----------  ----------  ----------     ----------------------------------
    24/07/2006  Initial      GB            Initial Version
    23/01/2015               Erika Algera  Add Mouth ID's
------------------------------------------------------------------------*/
  DEFINE VARIABLE lMandatory    AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cString       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE tExpectedDate AS DATE       NO-UNDO.
  DEFINE VARIABLE cDisableQLkp  AS CHARACTER  NO-UNDO. 
  DEFINE VARIABLE cDiscUpd      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOrigAmtUpd   AS CHARACTER  NO-UNDO.

  /*             FormName    HTMLControlName         HashOptions           Hidden Seq  DBField                            Mand.  DataType       Upd.   Sp Field Label Tooltip Size  Value                                     CellClass        LblClass   FldClass   Function                        FH FunctionArgument   AdditionalFields */
  wsv1WebUi:addFormField(gcFormName1, "frmWizardStep":U,    "#CalculatedField#":U, TRUE,   10, "":U,                              FALSE, "character":U, FALSE, 2, "":U,       "":U,   "":U, get-value("frmWizardStep":U),             "clInp,clChr":U, "":U,      "":U,      "":U,                           ?, "":U,              "":U).

  /*             FormName      HTMLControlName       #Opt                   Hidden Seq  DBField                           Mand.  DataType       Upd.   Sp  Field Label                 Ttip  Size      Value                  CCls             LblCl FldCl  Func                                     FH                   FncArg             AdditionalFields */
  wsv1WebUi:addFormField(gcFormName1, "frmNoteType":U,       "#CalculatedField#":U, TRUE,   40, "":U,                            FALSE, "character":U, FALSE,  2,  "":U,                              "":U,   "":U,    "AS":U,                "clInp,clChr":U, "":U,  "":U, "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmExpectedYear":U,   "#CalculatedField#":U, TRUE,   50, "":U,                            FALSE, "DATE":U,      FALSE,  2,  "":U,                              "":U,   "":U,    STRING(tExpectedDate), "clInp,clChr":U, "":U,  "":U, "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautMemNo":U ,      "#CalculatedField#":U, FALSE, 100, "":U,                            FALSE, "character":U, FALSE,  1,  "Member Number:":U,               "":U,   "13":U,  "":U,                   cCls,           "":U,  "":U, "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautXrefNo":U ,     "#CalculatedField#":U, FALSE, 120, "":U,                            FALSE, "character":U, FALSE,  1,  "Cross Reference No:":U,           "":U,   "20":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmMemstatus":U,      "#CalculatedField#":U, FALSE, 130, "":U,                            FALSE, "character":U, FALSE,  1,  "Member Status:":U,                "":U,   "":U,    gcMemStatus,            "clChr,clSus":U, "":U, "":U,  "":U,                                   ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautAuthNo":U ,     "":U,                  FALSE, 200, "ttauth.auth-num":U,             FALSE, "integer":U,   FALSE,  1,  "Auth Number:":U,                  "":U,   "8":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautSchemeCode":U,  cCFd,                  FALSE, 210, "":U,                            FALSE, "integer":U,   TRUE,   2,  "Scheme:":U,                       "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "SchemeList":U,    "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautDependant":U,   "":U,                  FALSE, 215, "ttauth.dependant":U,            FALSE, "integer":U,   TRUE,   1,  "Dependant:":U,                    "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "Dependant":U,     "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmDepName":U,        cCFd,                  FALSE, 220, "":U,                            FALSE, "character":U, FALSE,  2,  "Surname:":U,                      "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautRefAuthNo":U,   "":U,                  FALSE, 230, "ttauth.reference-auth-num":U,   FALSE, "character":U, FALSE,  1,  "Ref Auth Number:":U,              "":U,   "14":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautMsoNo":U,       "":U,                  FALSE, 240, "ttauth.mso-auth-num":U,         FALSE, "character":U, FALSE,  1,  "MSO Auth:":U,                     "":U,   "14":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautAuthDate":U,    "":U,                  FALSE, 250, "ttauth.auth-date":U,            FALSE, "date":U,      FALSE,  1,  "Auth Date:":U,                    "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautReqDocNo":U,    "":U,                  FALSE, 260, "ttauth.req-pr-num":U,           TRUE,  "integer":U,    TRUE,  1,  "Req Serv Prov:":U,                "":U,   "7":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmdocReqName":U,     cOpt,                  FALSE, 270, "":U,                            FALSE, "character":U,  TRUE,  1,  "Name:":U,                         "":U,   "30":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmdocReqNegGroup":U, cCFd,                  FALSE, 280, "":U,                            FALSE, "character":U, FALSE,  1,  "Neg Group:":U,                    "":U,   "30":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautRefDocNo":U,    "":U,                  FALSE, 290, "ttauth.ref-pr-num":U,           FALSE, "character":U,  TRUE,  1,  "Ref Serv Prov:":U,                "":U,   "7":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmdocRefName":U,     cOpt,                  FALSE, 300, "":U,                            FALSE, "character":U,  TRUE,  1,  "Name:":U,                         "":U,   "30":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmdocRefNegGroup":U, cCFd,                  FALSE, 310, "":U,                            FALSE, "character":U,  FALSE, 1,  "Neg Group:":U,                    "":U,   "30":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautDocNo":U,       "":U,                  FALSE, 320, "ttauth.pr-num":U,               TRUE,  "integer":U,     TRUE, 1,  "Serv Prov:":U,                    "":U,   "7":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmdocName":U,        cOpt,                  FALSE, 330, "":U,                            FALSE, "character":U,   TRUE, 1,  "Name:":U,                         "":U,   "30":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmdocNegGroup":U,    cCFd,                  FALSE, 339, "":U,                            FALSE, "character":U,  FALSE, 1,  "Neg Group:":U,                    "":U,   "30":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  IF NOT CAN-FIND(datalist no-lock
            WHERE datalist.list-type = "MCACUST"
            AND   datalist.list-code = "NOWOBEXTRF" 
            AND   datalist.link-from = "") THEN DO:
  wsv1WebUi:addFormField(gcFormName1, "lblExternalRefs":U,   cOpt,                  TRUE , 340, "":U,                            FALSE, "character":U,  FALSE, 3,  "":U,                              "":U,   "":U,    "External References":U,cHdr,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtCode1":U,    cCFd,                  TRUE , 341, "":U,                            FALSE, "character":U, TRUE,   1,  "Code 1:":U,                       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtCode2":U,    cCFd,                  TRUE , 342, "":U,                            FALSE, "character":U, TRUE,   1,  "Code 2:":U,                       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtCode3":U,    cCFd,                  TRUE , 343, "":U,                            FALSE, "character":U, TRUE,   1,  "Code 3:":U,                       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtValue1":U,   cCFd,                  TRUE , 344, "":U,                            FALSE, "character":U, TRUE,   1,  "Value 1:":U,                      "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtValue2":U,   cCFd,                  TRUE , 345, "":U,                            FALSE, "character":U, TRUE,   1,  "Value 2:":U,                      "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtValue3":U,   cCFd,                  TRUE , 346, "":U,                            FALSE, "character":U, TRUE,   1,  "Value 3:":U,                      "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtCode4":U,    cCFd,                  TRUE , 347, "":U,                            FALSE, "character":U, TRUE,   1,  "Code 4:":U,                       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtCode5":U,    cCFd,                  TRUE , 348, "":U,                            FALSE, "character":U, TRUE,   2,  "Code 5:":U,                       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtValue4":U,   cCFd,                  TRUE , 350, "":U,                            FALSE, "character":U, TRUE,   1,  "Value 4:":U,                      "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExtValue5":U,   cCFd,                  TRUE , 351, "":U,                            FALSE, "character":U, TRUE,   2,  "Value 5:":U,                      "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  END.
  wsv1WebUi:addFormField(gcFormName1, "lblExpDates":U,       cOpt,                  FALSE, 352, "":U,                            FALSE, "character":U,  FALSE, 1,  "":U,                              "":U,   "":U,    "Expected Dates":U,     cHdr,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  IF LOOKUP(gscUserRole, gscExternalUserRoles) > 0 THEN
  wsv1WebUi:addFormField(gcFormName1, "lblAuthDates":U,      cOpt,                  FALSE, 360, "":U,                            FALSE, "character":U,  FALSE, 2,  "":U,                      "":U,   "":U,    "Authorized Dates":U,   cHdr,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN
  wsv1WebUi:addFormField(gcFormName1, "lblAuthDates":U,      cOpt,                  FALSE, 360, "":U,                            FALSE, "character":U,  FALSE, 1,  "":U,                      "":U,   "":U,    "Authorized Dates":U,   cHdr,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  wsv1WebUi:addFormField(gcFormName1, "frmautExpStaDate":U,  "":U,                  FALSE, 370, "ttauth.expected-start-date":U,  FALSE, "date":U,        TRUE, 1,  "Start Date:":U,           "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  IF LOOKUP(gscUserRole, gscExternalUserRoles) > 0 THEN
  wsv1WebUi:addFormField(gcFormName1, "frmautAutStaDate":U,  "":U,                  FALSE, 380, "ttauth.auth-start-date":U,      FALSE, "date":U,        TRUE, 2,  "Start Date:":U,           "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN
  wsv1WebUi:addFormField(gcFormName1, "frmautAutStaDate":U,  "":U,                  FALSE, 380, "ttauth.auth-start-date":U,      FALSE, "date":U,        TRUE, 1,  "Start Date:":U,           "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  wsv1WebUi:addFormField(gcFormName1, "frmautExpEndDate":U,  "":U,                  FALSE, 390, "ttauth.expected-end-date":U,    FALSE, "date":U,        TRUE, 1,  "End Date:":U,             "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  IF LOOKUP(gscUserRole, gscExternalUserRoles) > 0 THEN
  wsv1WebUi:addFormField(gcFormName1, "frmautAutEndDate":U,  "":U,                  FALSE, 400, "ttauth.auth-end-date":U,        FALSE, "date":U,        TRUE, 2,  "End Date:":U,             "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN
  wsv1WebUi:addFormField(gcFormName1, "frmautAutEndDate":U,  "":U,                  FALSE, 400, "ttauth.auth-end-date":U,        FALSE, "date":U,        TRUE, 1,  "End Date:":U,             "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  wsv1WebUi:addFormField(gcFormName1, "frmautExpStaTime":U,  cCFd,                  FALSE, 410, "":U,                            FALSE, "character":U,   TRUE, 1,  "Start Time:":U,           "":U,   "2":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautAutStaTime":U,  cCFd,                  FALSE, 420, "":U,                            FALSE, "character":U,   TRUE, 1,  "Start Time:":U,           "":U,   "2":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautExpEndTime":U,  cCFd,                  FALSE, 430, "":U,                            FALSE, "character":U,   TRUE, 1,  "End Time:":U,             "":U,   "2":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautAutEndTime":U,  cCFd,                  FALSE, 440, "":U,                            FALSE, "character":U,   TRUE, 1,  "End Time:":U,             "":U,   "2":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmExpDays":U,        cCFd,                  FALSE, 445, "":U,                            FALSE, "integer":U,    FALSE, 1,  "Days:":U,                 "":U,   "5":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmAuthDays":U,       cCFd,                  FALSE, 450, "":U,                            FALSE, "integer":U,    FALSE, 1,  "Days:":U,                 "":U,   "5":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautClmCode":U,     "":U,                  FALSE, 465, "ttauth.claim-code":U,           FALSE, "integer":U,     TRUE, 1,  "Claim Code:":U,           "":U,   "2":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmcdeClaimDesc":U,   cOpt,                  FALSE, 470, "":U,                            FALSE, "character":U,   TRUE, 1,  "Description:":U,          "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautClmType":U,     "":U,                  FALSE, 475, "ttauth.claim-type":U,           FALSE, "character":U,   TRUE, 1,  "Claim Type:":U,           "":U,   "20":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "ClaimType":U,     "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautPrimDiag":U,    "":U,                  FALSE, 485, "ttauth.prim-diag":U,            TRUE,  "character":U,   TRUE, 1,  "Primary Diagnosis:":U,    "":U,   "5":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmDiagCondType":U,   cCFd,                  FALSE, 541, "":U,                            FALSE, "character":U,  FALSE, 1,  "Diagnosis Condition Type:":U, "":U, "8":U, "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmPdiagDesc":U,      cOpt,                  FALSE, 488, "":U,                            FALSE, "character":U,   TRUE, 1,  "Description:":U,          "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautPrimProc":U,    "":U,                  FALSE, 525, "ttauth.prim-proc":U,            FALSE, "character":U,   TRUE, 1,  "Primary Procedure:":U,    "":U,   "5":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmPrimProcDesc":U,   cOpt,                  FALSE, 540, "":U,                            FALSE, "character":U,   TRUE, 1,  "Description:":U,          "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautDesc":U,        "":U,                  FALSE, 542, "ttauth.description":U,          FALSE, "character":U,   TRUE, 1,  "Description:":U,          "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  
  IF ssystem.admin-comp = 20 
  THEN DO:
    wsv1WebUi:addFormField(gcFormName1, "frmautInPatStatus":U,  cCFd,               FALSE, 546, "":U,                            FALSE, "character":U,   TRUE, 1,  "Inpatient Status:":U,     "":U,   "41":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "RXHAuthStatus":U, "":U). 
    wsv1WebUi:addFormField(gcFormName1, "frmautRXProcStatus":U, cCFd,               FALSE, 548, "":U,                            FALSE, "character":U,   TRUE, 2,  "Rx Process Status:":U,    "":U,   "41":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "RXHAuthStatus":U, "":U). 
  END.
  
&IF {&DBDFMA} >= 010182 &THEN 
  wsv1WebUi:addFormField(gcFormName1, "frmautCopayAuth":U,   cCFd,                  FALSE, 556, "":U,                            FALSE, "decimal":U,     FALSE, 1,  "Copay Authed:":U,        "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautCopayAuthPaid":U, cCFd,                FALSE, 558, "":U,                            FALSE, "decimal":U,     FALSE, 1,  "Copay Paid:":U,          "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
&ENDIF  
  wsv1WebUi:addFormField(gcFormName1, "frmFaxstatus":U,      "#CalculatedField#":U, FALSE, 560, "":U,                            FALSE, "character":U,  FALSE, 2,  "Fax Status:":U,           "":U,   "11":U,  gcFaxStatus,            cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautPermTxt":U,     cOpt,                  FALSE, 563, "":U,                            FALSE, "character":U,  FALSE, 2,  "":U,                      "":U,   "50":U,  "":U,                   cMsg,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  
  IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN DO:
  wsv1WebUi:addFormField(gcFormName1, "frmautAmount":U,      cCFd,                  FALSE, 425, "ttauth.amount-auth":U,          FALSE, "decimal":U,     TRUE, 1,  "Auth Amount:":U,          "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautQuantity":U,    cCFd,                  FALSE, 443, "ttauth.quantity":U,             FALSE, "character":U,   TRUE, 1,  "Auth Quantity:":U,        "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmAmountPaid":U,     cCFd,                  FALSE, 365, "":U,                            FALSE, "decimal":U,    FALSE, 1,  "Paid Amount:":U,          "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "PaidCaseAmt":U,   "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmQuantityPaid":U,   cCFd,                  FALSE, 385, "ttauth.paid":U,                 FALSE, "decimal":U,    FALSE, 1,  "Paid Quantity:":U,        "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  
  ASSIGN cOrigAmtUpd = mipEnv:miUtility:getPreference("ma_PrefAuthMaintOriginalAmt":U) NO-ERROR.
  IF (cOrigAmtUpd = "" OR cOrigAmtUpd = ?) THEN
    ASSIGN cOrigAmtUpd = "FALSE" NO-ERROR.
  {mip/inc/mipreturnerror.i} 
  wsv1WebUi:addFormField(gcFormName1, "frmAmountOrig":U,     cCFd,                  FALSE, 460, "":U,                            FALSE, "decimal":U,    LOGICAL(cOrigAmtUpd), 1,  "Original Amount:":U,      "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  
  ASSIGN cDiscUpd = mipEnv:miUtility:getPreference("ma_PrefAuthMaintDiscount") NO-ERROR.
  {mip/inc/mipreturnerror.i}
  IF (cDiscUpd = "" OR cDiscUpd = ?) THEN
    ASSIGN cDiscUpd = "FALSE" NO-ERROR.
  {mip/inc/mipreturnerror.i}  
  wsv1WebUi:addFormField(gcFormName1, "frmAmountDiscount":U, cCFd,                  FALSE, 483, "":U,                            FALSE, "decimal":U,    LOGICAL(cDiscUpd), 1,  "Discount:":U, "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  wsv1WebUi:addFormField(gcFormName1, "frmAmountPenalty":U, cCFd,                   FALSE, 474, "":U,                            FALSE, "decimal":U,    FALSE, 1,  "Late penalty:":U, "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  
  wsv1WebUi:addFormField(gcFormName1, "frmDiscountType":U,   cCFd,                  FALSE, 500, "ttauth.discount-type":U,        FALSE, "character":U,  FALSE, 1,  "Discount Type:":U,        "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  END.

  IF ssystem.admin-comp = 20 THEN
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautQuantity":U,    "FieldUpdatable":U, "no":U).
     

  wsv1WebUi:addFormField(gcFormName1, "frmautStatus":U,      "":U,                  FALSE, 515, "ttauth.authstat":U,             FALSE, "integer":U,     TRUE, 1,  "Auth Status:":U,          "":U,   "20":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "AuthStat":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautStatRea":U,     "":U,                  FALSE, 522, "ttauth.authstat-note":U,        FALSE, "character":U,   TRUE, 1,  "Status Reason:":U,        "":U,   "80":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "AuthStat-Note":U, "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautSecDiag":U,     "":U,                  FALSE, 510, "ttauth.sec-diag":U,             FALSE, "character":U,   TRUE, 1,  "Asterisk Code:":U,        "":U,   "5":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmSdiagDesc":U,      cOpt,                  FALSE, 512, "":U,                            FALSE, "character":U,   TRUE, 1,  "Description:":U,          "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
                                       
&IF {&DBDFMA} >= 010180 &THEN 
  wsv1WebUi:addFormField(gcFormName1,  "frmautMorphCode":U,   "":U,                 FALSE, 518, "ttauth.morph-code":U,          FALSE, "character":U,   TRUE,  1,  "Morphology Code:":U,       "":U,  "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautMorphCode":U, "LookupWobFla":U,         "mamorph":U).                                                             /* Required */
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautMorphCode":U, "LookupFields":U,         "diagnos.diagnosis":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautMorphCode":U, "ClearReturnControls":U,  "TRUE":U).
  
  wsv1WebUi:addFormField(gcFormName1, "frmMorphDesc":U,      cOpt,                  FALSE, 520, "":U,                            FALSE, "character":U,   TRUE, 1,  "Description:":U,          "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "Disabled":U,      "":U).
&ENDIF
  
  IF LOOKUP(gscUserRole, gscExternalUserRoles) = 0 THEN
  wsv1WebUi:addFormField(gcFormName1, "frmAmountLimit":U,    cCFd,                  FALSE, 405, "":U,                            FALSE, "decimal":U,    FALSE, 1,  "Avail Amount:":U,         "":U,   "12":U,  "":U,                   cCls,           "":U, "":U,  "formatFormFields":U,                    ?,                   "AvailCaseAmt":U,  "":U).

  wsv1WebUi:addFormField(gcFormName1, "frmautType":U,        "":U,                  FALSE, 480, "ttauth.auth-type":U,            FALSE, "character":U,   TRUE, 1,  "Auth Type:":U,            "":U,   "40":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "AuthType":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautUser":U,        "#CalculatedField#",   FALSE, 550, "ttauth.user-id":U,              FALSE, "character":U,  FALSE, 1,  "Requested By:":U,         "":U,   "8":U,   "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautMedicentre":U,  "#CalculatedField#",   TRUE,  555, "":U,                            FALSE, "character":U,   TRUE, 3,  "Medicentre:":U,           "":U,   "20":U,  "":U,                   cCls,           "":U, "":U,  "FormatComboFields":U,                   TARGET-PROCEDURE,    "Medicentre":U,    "":U).
  
  wsv1WebUi:addFormField(gcFormName1, "lblAuthNote":U,       cOpt,                  TRUE,  561, "":U,                            FALSE, "character":U,  FALSE, 3,  "":U,                      "":U,   "":U,     "Notes":U,             cHdr,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautNote":U,        "":U,                  TRUE,  562, "ttauth.note":U,                 FALSE, "character":U,   TRUE, 3,  "General Notes:":U,        "":U,   "80x3":U, "":U,                  cCls,           "":U, "":U,  "formatFields":U,            wsv1WebUi:FormatProcedureHandle, "TextArea":U,      "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautLetNote":U,     "#CalculatedField#":U, TRUE,  566, "ttauth.authlet-note":U,         FALSE, "character":U,   TRUE, 3,  "Auth Letter Note:":U,     "":U,   "80x3":U, "":U,                  cCls,           "":U, "":U,  "formatFields":U,            wsv1WebUi:FormatProcedureHandle, "TextArea":U,      "":U).

  wsv1WebUi:addFormField(gcFormName1, "lblProcedure":U,       cOpt,                 TRUE,  567, "":U,                            FALSE, "character":U,  FALSE, 3,  "":U,                      "":U,   "":U,     "Additional Procedures":U, cHdr,       "":U, "":U, "":U,  ?,  "":U,  "":U).

  wsv1WebUi:addFormField(gcFormName1, "frmautsecProcCode1":U, cCFd,                 TRUE , 568, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecProcCode2":U, cCFd,                 TRUE , 569, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecProcCode3":U, cCFd,                 TRUE , 570, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecProcCode4":U, cCFd,                 TRUE , 571, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecProcCode5":U, cCFd,                 TRUE , 572, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecProcCode6":U, cCFd,                 TRUE , 573, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  wsv1WebUi:addFormField(gcFormName1, "lblDiagnosis":U,       cOpt,                 TRUE,  574, "":U,                            FALSE, "character":U,  FALSE, 3,  "":U,                      "":U,   "":U,     "Additional Diagnoses":U, cHdr,       "":U, "":U, "":U,  ?,  "":U,  "":U).

&IF {&DBDFMA} < 010180 &THEN

  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode1":U, cCFd,                 TRUE , 575, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode2":U, cCFd,                 TRUE , 576, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode3":U, cCFd,                 TRUE , 577, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode4":U, cCFd,                 TRUE , 578, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode5":U, cCFd,                 TRUE , 579, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode6":U, cCFd,                 TRUE , 580, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode7":U, cCFd,                 TRUE , 581, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode8":U, cCFd,                 TRUE , 582, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode9":U, cCFd,                 TRUE , 583, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

&ELSE /*astonm*/
  IF ssystem.admin-comp = 20 THEN DO:
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode1":U, cCFd,               TRUE , 575, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode2":U, cCFd,               TRUE , 576, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode3":U, cCFd,               TRUE , 577, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode4":U, cCFd,               TRUE , 578, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode5":U, cCFd,               TRUE , 579, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode6":U, cCFd,               TRUE , 580, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode7":U, cCFd,               TRUE , 581, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode8":U, cCFd,               TRUE , 582, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode9":U, cCFd,               TRUE , 583, "":U,                            FALSE, "character":U, TRUE,   1,  "Code:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  END.
  ELSE DO:
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode1":U,  cCFd,              TRUE , 575, "":U,                            FALSE, "character":U, TRUE,   1,  "ICD Code:":U,            "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecAssCode1":U,   cCFd,              TRUE , 576, "":U,                            FALSE, "character":U, TRUE,   1,  "Asterisk Code:":U,       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecMorphCode1":U, cCFd,              TRUE , 577, "":U,                            FALSE, "character":U, TRUE,   1,  "Morphology Code:":U,     "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode2":U,  cCFd,              TRUE , 578, "":U,                            FALSE, "character":U, TRUE,   1,  "ICD Code:":U,            "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecAssCode2":U,   cCFd,              TRUE , 579, "":U,                            FALSE, "character":U, TRUE,   1,  "Asterisk Code:":U,       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecMorphCode2":U, cCFd,              TRUE , 580, "":U,                            FALSE, "character":U, TRUE,   1,  "Morphology Code:":U,     "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecDiagCode3":U,  cCFd,              TRUE , 581, "":U,                            FALSE, "character":U, TRUE,   1,  "ICD Code:":U,            "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecAssCode3":U,   cCFd,              TRUE , 582, "":U,                            FALSE, "character":U, TRUE,   1,  "Asterisk Code:":U,       "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmautsecMorphCode3":U, cCFd,              TRUE , 583, "":U,                            FALSE, "character":U, TRUE,   1,  "Morphology Code:":U,     "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmthicParent":U,       cCFd,              TRUE,  160, "":U,                            FALSE, "character":U, TRUE,   1,  "Parent:":U,              "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmthicKey":U,          cCFd,              TRUE,  170, "":U,                            FALSE, "character":U, TRUE,   1,  "Key:":U,                 "":U,   "10":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
    
    wsv1WebUi:setFFAttribute(gcFormName1, "frmthicParent":U, "FieldValue":U, "diagnos":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmthicKey":U,    "FieldValue":U, "ma_acCodeLinkCatAsterisk":U).
  END.


&ENDIF  
  
  /*Case Limit details*/
  RUN CaseValidation IN TARGET-PROCEDURE (OUTPUT glCaseHidden) NO-ERROR.
  {mip/inc/mipreturnerror.i}

  wsv1WebUi:addFormField(gcFormName1, "lblCaseDetails":U,     cOpt,                 TRUE,  584, "":U,                            FALSE, "character":U,  FALSE, 3,  "":U,                      "":U,   "":U,     "Case Details":U,      cHdr,           "":U, "":U, "":U,  ?,  "":U,  "":U).
  
  wsv1WebUi:addFormField(gcFormName1, "frmautCaseLim":U,      cCFd,                 TRUE , 585, "":U,                            FALSE, "character":U, FALSE,  1,  "Case Limit:":U,           "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautCaseClmCode":U,  cCFd,                 TRUE , 586, "":U,                            FALSE, "integer":U,   FALSE,  1,  "Claim Code:":U,           "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautCaseClmType":U,  cCFd,                 TRUE , 587, "":U,                            FALSE, "character":U, FALSE,  1,  "Claim Type:":U,           "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautCaseFromDt":U,   cCFd,                 TRUE , 588, "":U,                            FALSE, "date":U,      TRUE,   1,  "From:":U,                 "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).
  wsv1WebUi:addFormField(gcFormName1, "frmautCaseToDt":U,     cCFd,                 TRUE , 589, "":U,                            FALSE, "date":U,      TRUE,   2,  "To:":U,                   "":U,   "15":U,  "":U,                   cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).

  /* Field for high cost auths */
  IF CAN-FIND(datalist WHERE datalist.list-type   = "MCACUST":U
                         AND datalist.list-code   = "DOHIGHCOST":U
                         AND datalist.description = "YES":U
                         AND datalist.link-from   = "":U)
  THEN DO: 
    /*             FormName    HTMLControlName         HashOptions           Hidden Seq  DBField                                   Mand.  DataType       Upd.   Sp   Field Label                Tooltip Size     Value                   CellClass       LblClass FldClass   Function                         FH                    FunctionArgument   AdditionalFields */
    wsv1WebUi:addFormField(gcFormName1, "frmHighCostAuth":U,    cCFd,                TRUE , 554, "":U,                            FALSE, "character":U, FALSE,  1,  "High Cost:":U,            "":U,   "15":U,  "YES":U,                cCls,           "":U, "":U,  "":U,                                    ?,                   "":U,              "":U).                     
  END. /* IF CAN-FIND(datalist... */
    
  IF ssystem.web-member-lookup = "C":U THEN
  DO:
    wsV1WebUi:setFFAttribute(gcFormName1, "frmautMemNo":U  , "FieldLabel":U, "Administration Member No:":U).
    wsV1WebUi:setFFAttribute(gcFormName1, "frmautXrefNo":U , "FieldLabel":U, "Scheme Member No:":U).
  END. /* IF ssystem.web-member-lookup = "C":U */

  IF NOT ERROR-STATUS:ERROR THEN
         RUN DoctorDetails IN TARGET-PROCEDURE NO-ERROR. /*"formatFormFields":U,                 ?,  "Disabled":U,  "":U).*/

  /*Doctor*/
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautDocNo":U,     "LookupWobFla":U,        "madoc":U).                                                             /* Required */
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautDocNo":U,     "LookupFields":U,        "doctor.doc-num":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautDocNo":U,     "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautDocNo":U,     "ExtraReturnControls":U, "frmdocName":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautDocNo":U,     "ExtraReturnFields":U,   "doctor.name":U).
  /*Requesting Doctor*/
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautReqDocNo":U,  "LookupWobFla":U,        "madoc":U).                                                             /* Required */
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautReqDocNo":U,  "LookupFields":U,        "doctor.doc-num":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautReqDocNo":U,  "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautReqDocNo":U,  "ExtraReturnControls":U, "frmdocReqName":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautReqDocNo":U,  "ExtraReturnFields":U,   "doctor.name":U).
  /*Refering Doctor*/
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautRefDocNo":U,  "LookupWobFla":U,        "madoc":U).                                                             /* Required */
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautRefDocNo":U,  "LookupFields":U,        "doctor.doc-num":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautRefDocNo":U,  "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautRefDocNo":U,  "ExtraReturnControls":U, "frmdocRefName":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautRefDocNo":U,  "ExtraReturnFields":U,   "doctor.name":U).
  /*Primary Diagnosis*/
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimDiag":U,  "LookupWobFla":U,        "madia":U).                                                             /* Required */
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimDiag":U,  "LookupFields":U,        "diagnos.diagnosis":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimDiag":U,  "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimDiag":U,  "ExtraReturnControls":U, "frmPdiagDesc":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimDiag":U,  "ExtraReturnFields":U,   "diagnos.description":U).
  /*primary link code - asterisk*/
  
&IF {&DBDFMA} < 010180 &THEN
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "LookupWobFla":U,        "madil":U).                                                             /* Required */
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "LookupFields":U,        "diagnoslink.diag-link-code":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ExtraReturnControls":U, "frmSdiagDesc":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ExtraReturnFields":U,   "diagnos.description":U).
&ELSE
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "LookupWobFla":U,        "mahlmcl":U).                                                             /* Required */
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "LookupFields":U,        "hlm_code_link.child_alt_value":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ExtraFilterControls":U, "frmautPrimDiag|frmthicParent|frmthicKey").
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ExtraFilterFields":U,   "hlm_code_link.parent_alt_value|hlm_code_link.parent_entity|hlm_code_link.acronym_key":U).    
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ExtraReturnControls":U, "frmautSecDiag":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautSecDiag":U,   "ExtraReturnFields":U,   "hlm_code_link.child_alt_value":U).
&ENDIF   /*Primary Procedure*/

  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "LookupWobFla":U,        "mapro":U).                                                             /* Required */
&IF {&DBDFMA} < 010180 &THEN	  
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "LookupFields":U,        "proccode.proc-code":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "ExtraReturnControls":U,  "frmPrimProcDesc":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "ExtraReturnFields":U,    "proccode.description":U).
&ELSE
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "LookupFields":U,        "hlm_cpt_link.cpt_code":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "ClearReturnControls":U, "TRUE":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "ExtraReturnControls":U,  "frmPrimProcDesc":U).
  wsv1WebUi:setFFAttribute(gcFormName1, "frmautPrimProc":U,  "ExtraReturnFields":U,    "hlm_cpt_link.cpt_description":U).
&ENDIF  

  
  IF scheme.use-ccode-table = YES THEN DO:
    /*Claim Code*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "LookupWobFla":U,        "maclc":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "LookupFields":U,        "ccdesc.claim-code":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ClearReturnControls":U, "TRUE":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraFilterControls":U, "frmautSchemeCode|frmautExpStaDate":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraFilterFields":U,   "ccode.scheme-code|ccode.effect-date":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraReturnControls":U, "frmcdeClaimDesc":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraReturnFields":U,   "ccdesc.description":U).
  END.
    
  ELSE DO:
  /*Claim Code*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "LookupWobFla":U,        "macclc":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "LookupFields":U,        "ccdesc.claim-code":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ClearReturnControls":U, "TRUE":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraFilterControls":U, "frmautSchemeCode|frmautExpStaDate":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraFilterFields":U,   "schcmpccode.scheme-code|schcmpccode.effect-date":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraReturnControls":U, "frmcdeClaimDesc":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U,   "ExtraReturnFields":U,   "ccdesc.description":U).
    
  END.                                                                                       
    
  

  IF LOOKUP(STRING(ssystem.admin-comp), "20,31") > 0 THEN
  DO:
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmCode":U, "FieldUpdatable":U, "no":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautClmType":U, "FieldUpdatable":U, "no":U).
  END.
    

  DO giCount = 1 TO 5:
    /*Reference*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautExtCode":U + STRING(giCount),  "LookupWobFla":U,        "sysref":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautExtCode":U + STRING(giCount),  "LookupFields":U,        "sysref.reference-code":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautExtCode":U + STRING(giCount),  "ClearReturnControls":U, "TRUE":U).
  END.

&IF {&DBDFMA} < 010180 &THEN	  
  DO giCount = 1 TO 6:
    /*Secondary Procedure*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecProcCode":U + STRING(giCount), "LookupWobFla":U,        "mapro":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecProcCode":U + STRING(giCount), "LookupFields":U,        "proccode.proc-code":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecProcCode":U + STRING(giCount), "ClearReturnControls":U, "TRUE":U).

  END.
&ELSE 
  DO giCount = 1 TO 6:
    /*Secondary Procedure*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecProcCode":U + STRING(giCount), "LookupWobFla":U,        "mapro":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecProcCode":U + STRING(giCount), "LookupFields":U,        "hlm_cpt_link.cpt_code":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecProcCode":U + STRING(giCount), "ClearReturnControls":U, "TRUE":U).

  END.
&ENDIF

&IF {&DBDFMA} < 010180 &THEN	  
  DO giCount = 1 TO 9:
    /*Secondary Diagnosis*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "LookupWobFla":U,        "madia":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "LookupFields":U,        "diagnos.diagnosis":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "ClearReturnControls":U, "TRUE":U).
  END.
&ELSE
  IF ssystem.admin-comp = 20 THEN 
    DO giCount = 1 TO 9:
      /*Secondary Diagnosis*/
      wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "LookupWobFla":U,        "madia":U).                                                             /* Required */
      wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "LookupFields":U,        "diagnos.diagnosis":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "ClearReturnControls":U, "TRUE":U).
    END.
  ELSE
    DO giCount = 1 TO 3:
      /*Secondary Diagnosis*/
      wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "LookupWobFla":U,        "madia":U).                                                             /* Required */
      wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "LookupFields":U,        "diagnos.diagnosis":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecDiagCode":U + STRING(giCount),  "ClearReturnControls":U, "TRUE":U).
    END.
  
  DO giCount = 1 TO 3:
    /*Secondary Diagnosis*/
    wsv1WebUi:setFFAttribute(gcFormName1,  "frmautsecAssCode":U + STRING(giCount), "LookupWobFla":U,        "mahlmcl":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1,  "frmautsecAssCode":U + STRING(giCount), "LookupFields":U,        "hlm_code_link.child_alt_value":U).
    wsv1WebUi:setFFAttribute(gcFormName1,  "frmautsecAssCode":U + STRING(giCount), "ClearReturnControls":U, "TRUE":U).
    wsv1WebUi:setFFAttribute(gcFormName1,  "frmautsecAssCode":U + STRING(giCount), "ExtraFilterControls":U, "frmautsecDiagCode":U + STRING(giCount) + "|frmthicParent|frmthicKey").
    wsv1WebUi:setFFAttribute(gcFormName1,  "frmautsecAssCode":U + STRING(giCount), "ExtraFilterFields":U,   "hlm_code_link.parent_alt_value|hlm_code_link.parent_entity|hlm_code_link.acronym_key":U).
                                                          
  END.
  
  
  DO giCount = 1 TO 3:
    /*Secondary Diagnosis*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecMorphCode":U + STRING(giCount),  "LookupWobFla":U,        "mamorph":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecMorphCode":U + STRING(giCount),  "LookupFields":U,        "diagnos.diagnosis":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmautsecMorphCode":U + STRING(giCount),  "ClearReturnControls":U, "TRUE":U).
  END.
&ENDIF  
  /*Questionaire*/
  IF get-value("frmautSchemeCode":U) <> "" THEN
    giSchemeCode = INTEGER(get-value("frmautSchemeCode":U)).

  IF giSchemeCode <> 0 AND giSchemeCode <> ?  THEN
  DO:
    RUN Questionaire IN TARGET-PROCEDURE NO-ERROR.
    {mip/inc/mipreturnerror.i}

    IF CAN-FIND(FIRST ttmemques) AND WarpSpeed:SubmitValue = {&lblWzNext} THEN
    DO:
       wsv1WebUi:addFormField(gcFormName1, "lblQuestionnaire":U,  cOpt, FALSE, 590, "":U,   FALSE, "character":U,  FALSE,  3, "":U,  "":U, "":U,  "Questionnaire":U, cHdr, "":U, "":U, "":U,  ?,  "":U,  "":U).
       wsv1WebUi:addFormField(gcFormName1, "lblName":U,           cOpt, FALSE, 595, "":U,   FALSE, "character":U,  FALSE,  1, "":U,  "":U, "":U,  "Name":U,          cHdr, "":U, "":U, "":U,  ?,  "":U,  "":U).
       wsv1WebUi:addFormField(gcFormName1, "lbltext":U,           cOpt, FALSE, 597, "":U,   FALSE, "character":U,  FALSE,  1, "":U,  "":U, "":U,  "Text":U,          cHdr, "":U, "":U, "":U,  ?,  "":U,  "":U).
       wsv1WebUi:addFormField(gcFormName1, "lblResult":U,         cOpt, FALSE, 599, "":U,   FALSE, "character":U,  FALSE,  1, "":U,  "":U, "":U,  "Result":U,        cHdr, "":U, "":U, "":U,  ?,  "":U,  "":U).
    END. /*IF CAN-FIND(FIRST ttmemques) THEN*/

    ASSIGN giSequence = 600 NO-ERROR.

    FOR EACH ttmemques NO-LOCK
       BREAK
          BY ttmemques.name:

      ASSIGN
        giSequence = giSequence + 10
      NO-ERROR.

       /*             FormName    HTMLControlName         HashOptions           Hidden Seq          DBField    Mand.        DataType      Upd.Sp Field Label                Tooltip Size  Value CellClass LblClass FldClass Function FH FunctionArgument  AdditionalFields */
      wsv1WebUi:addFormField(gcFormName1,   "frmEffectDate":U + STRING(giSequence),  cCFd,  TRUE, giSequence + 1,  "":U,   FALSE,      "DATE":U,       FALSE, 1, "":U,  "":U, "10":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1,   "frmQuestNum":U   + STRING(giSequence),  cCFd,  TRUE, giSequence + 2,  "":U,   FALSE,      "INTEGER":U,    FALSE, 1, "":U,  "":U, "4":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1,   "frmSchemeCode":U + STRING(giSequence),  cCFd,  TRUE, giSequence + 4,  "":U,   FALSE,      "INTEGER":U,    FALSE, 1, "":U,  "":U, "2":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1,   "frmQuesName":U   + STRING(giSequence),  cCFd,  TRUE, giSequence,      "":U,   FALSE,      "character":U,  FALSE, 1, "":U,  "":U, "20":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1,   "frmQuest":U      + STRING(giSequence),  cCFd,  TRUE, giSequence + 6,  "":U,   lMandatory, "character":U,  TRUE,  1, "":U,  "":U, "20":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1,   "frmResult":U     + STRING(giSequence),  cCFd,  TRUE, giSequence + 8,  "":U,   lMandatory, "character":U,  TRUE,  1, "":U,  "":U, "20":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      
      /*De Beers - Check Preference to determine if lookup should be disabled or not*/                         
      ASSIGN cDisableQLkp = mipEnv:miUtility:getPreference("ma_PrefAuthDisableQuesResponse") NO-ERROR.
      {mip/inc/mipreturnerror.i}
      
      IF (cDisableQLkp = "" OR cDisableQLkp = ?) THEN
        ASSIGN cDisableQLkp = "FALSE" NO-ERROR.
      {mip/inc/mipreturnerror.i}  
      
      IF cDisableQLkp <> "TRUE" THEN DO:
        ASSIGN
          cString = "frmSchemeCode":U + STRING(giSequence) + "|frmQuesName":U + STRING(giSequence) + "|frmEffectDate":U + STRING(giSequence) + "|frmQuestNum":U + STRING(giSequence).
         /*Result lookup*/
        wsv1WebUi:setFFAttribute(gcFormName1, "frmResult":U   + STRING(giSequence),  "LookupWobFla":U,        "maqes":U).
        wsv1WebUi:setFFAttribute(gcFormName1, "frmResult":U   + STRING(giSequence),  "LookupFields":U,        "schquesr.result-value":U).
        wsv1WebUi:setFFAttribute(gcFormName1, "frmResult":U   + STRING(giSequence),  "ClearReturnControls":U, "TRUE":U).
        wsv1WebUi:setFFAttribute(gcFormName1, "frmResult":U   + STRING(giSequence),  "ExtraFilterControls":U, cString).
        wsv1WebUi:setFFAttribute(gcFormName1, "frmResult":U   + STRING(giSequence),  "ExtraFilterFields":U,   "schquesr.scheme-code|schquesr.NAME|schquesr.effect-date|schquesr.question-num":U).
      END. /*IF cDisableQLkp = "TRUE"*/
            
      wsv1WebUi:setFFAttribute(gcFormName1, "frmQuest":U      + STRING(giSequence), "FieldValue":U, ttmemques.question-text).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmResult":U     + STRING(giSequence), "FieldValue":U, ttmemques.result-value).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmQuesName":U   + STRING(giSequence), "FieldValue":U, ttmemques.name).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmEffectDate":U + STRING(giSequence), "FieldValue":U, STRING(ttmemques.effect-date)).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmQuestNum":U   + STRING(giSequence), "FieldValue":U, STRING(ttmemques.question-num)).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmSchemeCode":U + STRING(giSequence), "FieldValue":U, STRING(ttmemques.scheme-code)).
    END.  /*FOR EACH ttmemques*/
  END.  /*/IF giSchemeCode <> 0 AND giSchemeCode <> ? AND giWizardStep = 5*/

                                                                                                       
  DO giCount = 1 TO giDetLinePref:                                                                                
    IF giCount = 1   THEN                                                                              
    DO:                                                                                                
        ASSIGN giSequence = 800 NO-ERROR.

        IF WarpSpeed:SubmitValue = {&lblDetailLines} OR WarpSpeed:SubmitValue = {&lblAdd} THEN
          wsv1WebUi:addFormField(gcFormName1, "lblDetailLines":U,  cOpt, FALSE, giSequence,     "":U,   FALSE, "character":U,  FALSE,  3, "":U,  "":U, "":U,  "Add Detail Lines":U, cHdr, "":U, "":U, "":U,  ?,  "":U,  "":U).
        
        wsv1WebUi:addFormField(gcFormName1, "lblMessage":U,      cOpt, FALSE, giSequence + 1, "":U,   FALSE, "character":U,  FALSE,  3, "":U,  "":U, "":U,  "":U, cMsg, "":U, "":U, "":U,  ?,  "":U,  "":U).
    END.
    IF giCount = 6   THEN ASSIGN giSequence = 925 NO-ERROR.
    IF giCount = 11  THEN ASSIGN giSequence = 1050 NO-ERROR.
    ASSIGN giSequence = giSequence + 25.
    
    /*                     FormName      HTMLControlName                           #Opt  Hidden Seq             DBField    Mand.  DataType       Upd.   Sp Field Label           Ttip  Size    Value CCls  LblCl FldCl Func  FH FncArg AdditionalFields */
    wsv1WebUi:addFormField(gcFormName1, "frmaudTarCode":U    + STRING(giSequence), cCFd, FALSE, giSequence +  2,  "":U,  FALSE, "character":U,  TRUE, 1, "Tariff Code:":U,     "":U,  "8":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudClmCode":U    + STRING(giSequence), cCFd, FALSE, giSequence +  3,  "":U,  FALSE, "character":U,  TRUE, 1, "Claim Code:":U,      "":U,  "5":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudProcCode":U   + STRING(giSequence), cCFd, FALSE, giSequence +  4,  "":U,  FALSE, "character":U,  TRUE, 1, "Procedure:":U,       "":U,  "7":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudDesc":U       + STRING(giSequence), cCFd, FALSE, giSequence +  5,  "":U,  FALSE, "character":U,  TRUE, 1, "Description:":U,     "":U, "30":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudQuantity":U   + STRING(giSequence), cCFd, FALSE, giSequence +  6,  "":U,  FALSE, "decimal":U,    TRUE, 1, "Quantity:":U,        "":U,  "6":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudAmount":U     + STRING(giSequence), cCFd, FALSE, giSequence +  7,  "":U,  FALSE, "decimal":U,    TRUE, 1, "Amount:":U,          "":U, "15":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudStaDate":U    + STRING(giSequence), cCFd, FALSE, giSequence +  8,  "":U,  FALSE, "date":U,       TRUE, 1, "Start Date:":U,      "":U, "10":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudStaTime":U    + STRING(giSequence), cCFd, FALSE, giSequence +  9,  "":U,  FALSE, "character":U,  TRUE, 1, "Start Time:":U,      "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudClmType":U    + STRING(giSequence), cCFd, FALSE, giSequence + 10,  "":U,  FALSE, "character":U,  TRUE, 1, "Claim Type:":U,      "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
    wsv1WebUi:addFormField(gcFormName1, "frmaudEndDate":U    + STRING(giSequence), cCFd, FALSE, giSequence + 11,  "":U,  FALSE, "date":U,       TRUE, 1, "End Date:":U,        "":U, "10":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).


    IF CAN-FIND(FIRST schintref WHERE schintref.scheme-code                               = 0
                                  AND schintref.interface-type                            = "MIP"
                                  AND schintref.reference-code                            = "AuthQuote"
                                  AND LOOKUP('memauthd',schintref.owning-entity-mnemonic) > 0) THEN
    DO:
      wsv1WebUi:addFormField(gcFormName1, "frmaudEndTime":U    + STRING(giSequence), cCFd, FALSE, giSequence + 12,  "":U,  FALSE, "character":U,  TRUE, 1, "End Time:":U,        "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudQuoted":U     + STRING(giSequence), cCFd, FALSE, giSequence + 13,  "":U,  FALSE, "decimal":U,    TRUE, 1, "Quoted Amount:":U,   "":U, "15":U,  "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID1":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 14,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID1:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID2":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 15,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID2:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID3":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 16,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID3:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID4":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 17,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID4:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID5":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 18,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID5:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID6":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 19,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID6:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID7":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 20,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID7:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID8":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 21,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID8:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID9":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 22,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID9:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID10":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 23,  "":U,  FALSE, "character":U, TRUE, 3, "Mouth ID10:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).   
      wsv1WebUi:addFormField(gcFormName1, "lblMessage":U       + STRING(giSequence), cOpt, FALSE, giSequence + 24, "":U,   FALSE, "character":U,  FALSE,  3, "":U,  "":U, "":U,  "":U, cMsg, "":U, "":U, "":U,  ?,  "":U,  "":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "ExtraReturnControls":U, "frmaudClmCode":U + STRING(giSequence) + "|frmaudAmount" + STRING(giSequence) + "|frmaudQuoted" + STRING(giSequence)+ "|frmaudClmType" + STRING(giSequence)).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "ExtraReturnFields":U,   "trfcost.claim-code|trfcost.amount|trfcost.amount|trfcost.claim-type":U).

    END.
    ELSE
    DO:
      wsv1WebUi:addFormField(gcFormName1, "frmaudEndTime":U    + STRING(giSequence), cCFd, FALSE, giSequence + 12,  "":U,  FALSE, "character":U,  TRUE, 2, "End Time:":U,        "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID1":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 13,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID1:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID2":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 14,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID2:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID3":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 15,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID3:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID4":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 16,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID4:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID5":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 17,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID5:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID6":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 18,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID6:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID7":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 19,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID7:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID8":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 20,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID8:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID9":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 21,  "":U,  FALSE, "character":U,  TRUE, 1, "Mouth ID9:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).
      wsv1WebUi:addFormField(gcFormName1, "frmaudMouthID10":U   + STRING(giSequence), cCFd, LOGICAL(gcHideMouthIDPref), giSequence + 22,  "":U,  FALSE, "character":U,  TRUE, 3, "Mouth ID10:":U,       "":U, "3":U,   "":U, cCls, "":U, "":U, "":U, ?, "":U,  "":U).   
      wsv1WebUi:addFormField(gcFormName1, "lblMessage":U       + STRING(giSequence), cOpt, FALSE, giSequence + 23, "":U,   FALSE, "character":U,  FALSE,  3, "":U,  "":U, "":U,  "":U, cMsg, "":U, "":U, "":U,  ?,  "":U,  "":U).
      
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "ExtraReturnControls":U, "frmaudClmCode":U + STRING(giSequence) + "|frmaudAmount" + STRING(giSequence) + "|frmaudClmType" + STRING(giSequence)).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "ExtraReturnFields":U,   "trfcost.claim-code|trfcost.amount|trfcost.claim-type":U).
    END.


    /*Tariff Code*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "LookupWobFla":U,        "matar":U).                                                             /* Required */
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "LookupFields":U,        "tariff.tariff-code":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "ClearReturnControls":U, "TRUE":U).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "ExtraFilterControls":U, "frmautDocNo|frmautSchemeCode|frmaudStaDate" + STRING(giSequence)).
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudTarCode":U  + STRING(giSequence), "ExtraFilterFields":U,   "tariff.base-rate|tariff.ars-rate|tariff.effect-date":U).    /* By putting >= in it overrides default behaviour of = */
    IF LOOKUP(STRING(ssystem.admin-comp,"99"),"18") > 0 THEN
    DO:
        wsv1WebUi:setFFAttribute(gcFormName1, "frmautRefDocNo", "FieldMandatory":U, "yes").
    END.

    /*Claim Code*/
    IF scheme.use-ccode-table = YES THEN DO:
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "LookupWobFla":U,        "maclc":U).                                                             /* Required */
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "LookupFields":U,        "ccdesc.claim-code":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "ClearReturnControls":U, "TRUE":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "ExtraFilterControls":U, "frmautSchemeCode|frmautExpStaDate":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "ExtraFilterFields":U,   "ccode.scheme-code|ccode.effect-date":U).
    END.  
    ELSE DO:
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "LookupWobFla":U,        "macclc":U).                                                             /* Required */
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "LookupFields":U,        "ccdesc.claim-code":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "ClearReturnControls":U, "TRUE":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "ExtraFilterControls":U, "frmautSchemeCode|frmautExpStaDate":U).
      wsv1WebUi:setFFAttribute(gcFormName1, "frmaudClmCode":U + STRING(giSequence),   "ExtraFilterFields":U,   "schcmpccode.scheme-code|schcmpccode.effect-date":U).
    END.
 
 
     /*Primary Procedure*/
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudProcCode":U + STRING(giSequence), "LookupWobFla":U,        "mapro":U).                                                             /* Required */
    
&IF {&DBDFMA} < 010180 &THEN	  
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudProcCode":U + STRING(giSequence), "LookupFields":U,        "proccode.proc-code":U).
&ELSE
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudProcCode":U + STRING(giSequence), "LookupFields":U,        "hlm_cpt_link.cpt_code":U).
&ENDIF    
    wsv1WebUi:setFFAttribute(gcFormName1, "frmaudProcCode":U + STRING(giSequence), "ClearReturnControls":U, "TRUE":U).

  END. /*do iCount = 1 to 5  */







