CLASS cls.trutility INHERITS cls.mipservice:
  {mip/inc/mipdefsharedclass.i}

  /* -- CONSTRUCTOR ------------------------------------------------- */
  CONSTRUCTOR PUBLIC trutility():
      message "Creating trUtility service" view-as alert-box.
  END CONSTRUCTOR. /* CONSTRUCTOR PUBLIC trutilityservice */
  /* -- DESTRUCTOR -------------------------------------------------- */
  DESTRUCTOR PUBLIC trutility():

  END DESTRUCTOR. /* DESTRUCTOR PUBLIC trutilityservice */
END CLASS. /* cls.trutility INHERITS cls.mipservice */