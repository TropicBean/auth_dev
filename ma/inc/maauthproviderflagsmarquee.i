/*------------------------------------------------------------------------
    File        : maauthproviderflagsmarquee.i
    Purpose     : Include for Auth Privider Flags Marquee.
    Description :   
    
    { ma/inc/maauthproviderflagsmarquee.i &ProviderList = "ipcProviderList"}
    
    
    Syntax      :
     Date        Ref.        Author        Description
    ----------  ----------  -----------    ---------------------------------
    16/05/2022  Initial     Gift Shipalana Initial Version

------------------------------------------------------------------------*/
DEFINE VARIABLE cProviderList       AS CHARACTER   NO-UNDO. /*This variable will be used to get the provider flags the list will be like providerNumber|startDate|endDate  e.g 2152312|05/06/2022|?,525654|05/06/2022|06/06/2022*/
DEFINE VARIABLE iCountProviders     AS INTEGER     NO-UNDO. 
DEFINE VARIABLE iProviderNumber     AS INTEGER     NO-UNDO.
DEFINE VARIABLE dProviderStartDate  AS DATE        NO-UNDO.
DEFINE VARIABLE dProviderEndDate    AS DATE        NO-UNDO.
DEFINE VARIABLE cDocMarqueePref     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDocFlagsMarqueeMsg AS CHARACTER   NO-UNDO INITIAL "":U.
DEFINE VARIABLE iOption             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCount              AS INTEGER     NO-UNDO.

ASSIGN
  cDocMarqueePref = mipEnv:miUtility:getPreference("ma_PrefDocMarquee":U)
NO-ERROR.
{mip/inc/mipreturnerror.i}

&IF DEFINED(ProviderList) = 0
&THEN
&GLOBAL-DEFINE ProviderList 
&ENDIF

/*assign the provider list  to a local variable */
ASSIGN cProviderList = {&ProviderList}.

{ ws/inc/wslog.i "'ma_DebugAuth'" "SUBSTITUTE('Auth Provider List : &1',cProviderList)" }.

{ ws/inc/wslog.i "'ma_DebugAuth'" "SUBSTITUTE('ma_PrefDocMarquee Value = : &1',cDocMarqueePref)" }.

CASE cDocMarqueePref:
  WHEN "True":U THEN
  DO:
  MarqueeProviderFlags:
    DO:
       ASSIGN iCountProviders = 1.
       IF cProviderList > "":U THEN 
       DO WHILE iCountProviders <= NUM-ENTRIES(cProviderList) :
         /* Provider  */
         ASSIGN iProviderNumber    = INTEGER (ENTRY(1,ENTRY(iCountProviders, cProviderList),"|"))
                iOption            = INTEGER (ENTRY(2,ENTRY(iCountProviders, cProviderList),"|"))
                dProviderStartDate = DATE    (ENTRY(3,ENTRY(iCountProviders, cProviderList),"|"))
                dProviderEndDate   = DATE    (ENTRY(4,ENTRY(iCountProviders, cProviderList),"|")).

         IF dProviderEndDate = ? THEN
           ASSIGN dProviderEndDate = TODAY.

         IF dProviderStartDate = ? THEN
           ASSIGN dProviderStartDate = TODAY.

         /*find the doctor*/
         IF iProviderNumber > 0 THEN
           FIND FIRST doctor NO-LOCK
                WHERE doctor.doc-num = iProviderNumber NO-ERROR.
         {&ResetError}
         IF NOT AVAILABLE doctor THEN 
         DO:
           ASSIGN iCountProviders = iCountProviders + 1. /*move to the next provider*/
           NEXT. /*if the doctor is not found go to the next doctor on the cProviderlist*/
         END.

         FOR EACH flagdet NO-LOCK
            WHERE (flagdet.scheme-code  = iOption
               OR  flagdet.scheme-code  = 00)
              AND  flagdet.type         = "D":U
              AND  flagdet.key          = STRING(doctor.doc-num)
              AND  flagdet.effect-date <= dProviderStartDate:
              
              IF flagdet.end-date <> ?
              AND flagdet.end-date < dProviderStartDate
              THEN
                NEXT.

             FIND FIRST flagsys NO-LOCK
                  WHERE flagsys.scheme-code   = flagdet.scheme-code
                    AND   flagsys.type        = flagdet.type
                    AND   flagsys.flag-num    = flagdet.flag-num
               NO-ERROR. 
  
           IF NOT AVAILABLE flagsys THEN 
           DO:
             {&ResetError}
             NEXT.
           END.

&IF {&DBDFHMA} >= 010018 &THEN   
           IF NOT flagsys.web-disp-yn THEN NEXT.
&ELSE            
           IF NOT flagsys.disp-yn THEN NEXT.
&ENDIF
             
           ASSIGN cDocFlagsMarqueeMsg = cDocFlagsMarqueeMsg + IF cDocFlagsMarqueeMsg <> "":U 
                                                        THEN ",  " + flagdet.KEY + "    " + (IF flagsys.description  = ? THEN "":U ELSE flagsys.description)
                                                        ELSE  flagdet.KEY + "    " + (IF flagsys.description  = ? THEN "":U ELSE flagsys.description ) NO-ERROR.
                  
           IF flagdet.flag-value <> "":U THEN
             ASSIGN cDocFlagsMarqueeMsg = cDocFlagsMarqueeMsg + " Value: " + (IF flagdet.flag-value = ? THEN "" ELSE flagdet.flag-value) NO-ERROR.
          
           IF flagdet.effect-date <> ? THEN
             ASSIGN cDocFlagsMarqueeMsg = cDocFlagsMarqueeMsg + " Effective: " + (IF flagdet.effect-date = ? THEN "":U ELSE STRING(flagdet.effect-date,"9999/99/99")) NO-ERROR.
           {mip/inc/mipreturnerror.i}

           ASSIGN cDocFlagsMarqueeMsg = cDocFlagsMarqueeMsg + " End Date: " + (IF flagdet.end-date = ? THEN "Unknown" ELSE STRING(flagdet.end-date)).   
         END. /* Each flagdet */
         ASSIGN iCountProviders = iCountProviders + 1.
       END. /* IF NUM-ENTRIES(cProviderList) > 0 */
       IF cDocFlagsMarqueeMsg <> "":U THEN
       DO:
        ASSIGN iCount = 1.
        {&OUT}
          "<center>" 
          + "<MARQUEE WIDTH='90%' HEIGHT='25' STYLE='color:Orange' >".

        DO WHILE iCount <= NUM-ENTRIES(cDocFlagsMarqueeMsg):
            {&OUT}  
            "<BIG> <BIG> <B> <ProviderFlags>" + mipEnv:miUtility:encode-url(ENTRY(iCount,cDocFlagsMarqueeMsg),"html":U) + "</ProviderFlags> </B> </BIG> </BIG>".
          ASSIGN iCount = iCount + 1 .
        END.

        {&OUT}
        " </MARQUEE>":U SKIP.
        {&OUT}
         "</center>".
       END.  
    END. /*ProviderMarquee*/
  END. /* when = "True" */
  WHEN "False":U THEN 
     DO:
     END.
   OTHERWISE MESSAGE "The preference 'ma_PrefDocMarquee' has an Invalid value".
 END CASE.
