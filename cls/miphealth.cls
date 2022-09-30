/*------------------------------------------------------------------------------
    Component: Call Center
      Purpose: Call Center Application Class
  Description: This is the Application Class for Call Center
        Notes: INHERITS cls.mipapplication
------------------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS cls.miphealth INHERITS cls.mipapplication:
  {mip/inc/mipdefsharedclass.i}
    
  /* -- PROPERTIES ---------------------------------------------------------------- */
  &SCOPED-DEFINE Segment ServiceProperty
  {cls/inc/mip.cls.i &PropertyName = trUtility &ServiceClass = cls.trutility}

  &UNDEFINE Segment
  /* -- BUILT-IN / REQUIRED METHODS ----------------------------------------------- */
  /* -- METHOD: instantiateApplication -------------------------------------------- */

  METHOD PUBLIC OVERRIDE LOGICAL instantiateApplication():
   
    MESSAGE "Instantiate Health App!" VIEW-AS ALERT-BOX.

    ASSIGN
      DynamicServiceClassList = "{&DynamicServiceClassList}":U
      DynamicServiceNameList  = "{&DynamicServiceNameList}":U.
    
    RETURN TRUE.

    {mip/inc/mipcatcherror.i}
  END METHOD. /* METHOD PUBLIC OVERRIDE LOGICAL instantiateApplication */

   /* -- METHOD: initializeApplication -------------------------------------- */
   METHOD OVERRIDE PUBLIC LOGICAL initializeApplication
   ():
 /*----------------------------------------------------------------------------
     Purpose:
       Notes:
 ----------------------------------------------------------------------------*/


   SUPER:initializeApplication().

   RETURN TRUE.

   {mip/inc/mipcatcherror.i}

 END METHOD. /* METHOD OVERRIDE PUBLIC LOGICAL initializeApplication */  

END CLASS. /* cls.miphealth INHERITS cls.mipapplication */
