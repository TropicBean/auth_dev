/* -- Include: maauthbusinessrule.i -------------------------------------------
   Author: Andrewd

  Purpose: This include file is meant to contain code that might be duplicated in 
           a class file. So instead of duplicating code and introducing 
           maintainability issues, we will place the common code in here.    
----------------------------------------------------------------------------------- */

&IF "{&Segment}":U = "BusinessRuleMethod":U &THEN

/* -- METHOD: {&MethodName} ------------------------------------------------------- */
  METHOD PUBLIC LOGICAL {&MethodName}
    (INPUT  ipoParameter           AS cls.maparameterobject,
     OUTPUT oplcDecisionResult     AS LONGCHAR,
     OUTPUT opcDecisionErrorDetail AS CHARACTER):
/*---------------------------------------------------------------------------------
      Purpose: Prepare dataset and make decision service request
      Notes  : 
      Author : Andrewd         
  ---------------------------------------------------------------------------------*/
   
    {mip/inc/mipservicecall.i
        &TreatErrorNumbersAsWarnings = '132':U

        &ServiceProcedureName = '{&MethodName}':U
        &ServiceCallSignature = "INPUT  ipoParameter,
                                 OUTPUT oplcDecisionResult,
                                 OUTPUT opcDecisionErrorDetail"} 
                                 
    RETURN TRUE.

    { mip/inc/mipcatcherror.i }
    
  END METHOD. /* METHOD PUBLIC LOGICAL {&MethodName} */

&ENDIF











