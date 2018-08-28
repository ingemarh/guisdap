function [ OUTF,OARR,context ] = IRI_SUB( context, JF,JMAG,ALATI,ALONG,IYYYY,...
  MMDD,DHOUR,HEIBEG,HEIEND,HEISTP,OARR )
%IRI_SUB compute IRI parameters
% irisub.for, version number can be found at the end of this comment.
%-----------------------------------------------------------------------        
% Includes subroutines IRI_SUB and IRI_WEB to compute IRI parameters 
% for specified location, date, time, and altitude range and subroutine 
% IRI_WEB to computes IRI parameters for specified location, date, time 
% and variable range; variable can be altitude, latitude, longitude, 
% year, month, day of month, day of year, or HOUR (UT or LT). 
% IRI_WEB requires IRI_SUB. Both subroutines require linking with the 
% following library files IRIFUN.FOR, IRITEC.FOR, IRIDREG.FOR, 
% CIRA.FOR, IGRF.FOR
%-----------------------------------------------------------------------        
% Programs using IRISUB need to include (see IRITEST):
% 
%       COMMON/const2/icalls,nmono,iyearo,idaynro,rzino,igino,ut0
%
%       context.icalls=0;
%       context.nmono=-1;
%       context.iyearo=-1;
%       context.idaynro=-1;
%       context.rzino=-1;
%       context.igino=-1;
%       context.ut0=-1;
%        
%       for i=1:50
%         OAR(i,1)=-1.0;
%       end
%-----------------------------------------------------------------------        
% Required i/o units:  
%  KONSOL= 6 IRISUB: Program messages (used when JF(12)=true -> konsol)
%  IUCCIR=10 IRISUB: CCIR and URSI coefficients (CCIR%%.ASC, %%=month+10)
%  KONSOL=11 IRISUB: Program messages (used when JF(12)=false -> MESSAGES.TXT)
%     KONSOL=6/11 is also used in IRIFUN and IGRF. COMMON/iounit/konsol 
%     is used to pass the value of KONSOL. If KONSOL=1 messages are turned off.
%  UNIT=12 IRIFUN/TCON:  Solar/ionospheric indices IG12, R12 (IG_RZ.DAT) 
%  UNIT=13 IRIFUN/APF..: Magnetic indices and F10.7 (APF107.DAT 
%  UNIT=14 IGRF/GETSHC:  IGRF coeff. (DGRF%%%%.DAT or IGRF%%%%.DAT, %%%%=year)
%-----------------------------------------------------------------------        
% CHANGES FROM  IRIS11.FOR  TO   IRIS12.FOR:
%    - CIRA-1986 INSTEAD OF CIRA-1972 FOR NEUTRAL TEMPERATURE
%    - 10/30/91 VNER FOR NIGHTTIME LAY-VERSION:  abs(..)
%    - 10/30/91 XNE(..) IN CASE OF LAY-VERSION
%    - 10/30/91 CHANGE SSIN=F/T TO IIQU=0,1,2
%    - 10/30/91 Te > Ti > Tn ENFORCED IN FINAL PROFILE
%    - 10/30/91 SUB ALL NAMES WITH 6 OR MORE CHARACTERS
%    - 10/31/91 CORRECTED HF1 IN HST SEARCH:  NE(HF1)>NME
%    - 11/14/91 C1=0 IF NO F1-REGION
%    - 11/14/91 CORRECTED HHMIN AND HZ FOR LIN. APP.
%    -  1/28/92 RZ12=0 included
%    -  1/29/92 NEQV instead of NE between URSIF2 and URSIFO
%    -  5/ 1/92 CCIR and URSI input as in IRID12
%    -  9/ 2/94 Decimal month (ZMONTH) for IONCOM
%    -  9/ 2/94 Replace B0POL with B0_TAB; better annually
%    -  1/ 4/95 DY for h>hmF2
%    -  2/ 2/95 IG for foF2, topside; RZ for hmF2, B0_TAB, foF1, NmD
%    -  2/ 2/95 winter no longer exclusive for F1 occurrrence
%    -  2/ 2/95 RZ and IG incl as DATA statement; smooth annual var.
% CHANGES FROM  IRIS12.FOR  TO   IRIS13.FOR:
%    - 10/26/95 incl year as input and corrected MODA; nrm for ZMONTH
%    - 10/26/95 use TCON and month-month interpolation in foF2, hmF2
%    - 10/26/95 TCON only if date changes
%    - 11/25/95 take out logicals TOPSI, BOTTO, and BELOWE
%    - 12/ 1/95 UT_LT for (date-)correct UT<->LT conversion
%    - 12/22/95 Change ZETA COV term to COV < 180; use COV inst covsat
%    -  2/23/96 take covmax(R<150) for topside; lyear,.. for lt
%    -  3/26/96 topside: 94.5/BETA inst 94.45/..; COV -> covsat(<=188)
%    -  5/01/96 No longer DY for h>hmF2 (because of discontinuity)
%    - 12/01/96 IRIV13: HOUR for IVAR=1 (height)
%    -  4/25/97 D-region: XKK le 10 with D1 calc accordingly.
%    -  1/12/97 DS model for lower ion compoistion DY model
%    -  5/19/98 seamon=ZMONTH if LATI>0; ZMONTH= ...(1.0*iday)/..
%    -  5/19/98 DY ion composition model below 300 km now DS model
%    -  5/19/98 DS model includes N+, Cl down to 75 km HNIA changed
%    -  5/28/98 User input for Rz12, foF1/NmF1, hmF1, foE/NmE, hmE
%    -  9/ 2/98 1 instead of 0 in MODA after UT_LT call
%    -  4/30/99 constants moved from DATA statement into program
%    -  4/30/99 changed konsol-unit to 13 (12 is for IG_RZ).
%    -  5/29/99 the limit for IG comp. from Rz12-input is 174 not 274
%    - 11/08/99 JF(18)=t simple UT to LT conversion, otherwise UT_LT
%    - 11/09/99 added COMMON/const1/humr,dumr also for CIRA86
% CHANGES FROM  IRIS13.FOR  TO   IRISUB.FOR:
%-----------------------------------------------------------------------        
%-Version-MM/DD/YY-Description (person reporting correction)
% 2000.01 05/09/00 B0_98 replaces B0_TAB and B1: 1.9/day to 2.6/night
% 2000.02 06/11/00 including new F1 and indermediate region
% 2000.03 10/15/00 include Scherliess-Fejer drift model
% 2000.04 10/29/00 include special option for D region models
% 2000.05 12/07/00 change name IRIS13 to IRISUB
% 2000.06 12/14/00 JF(30),OUTF(20,100),OARR(50)
% 2000.07 03/17/01 include Truhlik-Triskova Te model and IGRF
% 2000.08 05/07/01 include Fuller-Rowell-Condrescu storm model 
% 2000.09 07/09/01 LATI instead of LAT1 in F00 call -------- M. Torkar
% 2000.10 07/09/01 sdte instead of dte in ELTEIK call --- P. Wilkinson
% 2000.11 09/18/01 correct computation of foF2 for Rz12 user input
% 2000.12 09/19/01 Call APF only if different date and time -- P. Webb
% 2000.13 10/28/02 replace TAB/6 blanks, enforce 72/line -- D. Simpson
% 2000.14 11/08/02 change unit for message file to 11 (13 is Kp)
% 2000.15 01/27/03 change F1_prob output; Te-IK for fix h and ELTE(h)
% 2000.16 02/04/03 along<0 -> along=along+360; F1 occ for hmf1&foF1
% 2000.17 02/05/03 zyear =12.97 (Dec 31); idayy=#days per year
% 2000.18 02/06/03 JF(27) for IG12 user input; all F1 prob in oar
% 2000.19 07/14/04 covsat<188 instead of covsat=<f(IG)<188
% 2000.19 02/09/05 declare INVDIP as real ------------------ F. Morgan
% 2000.20 11/09/05 replace B0B1 with BCOEF --------------- T. Gulyaeva
% 2005.01 11/09/05 new topside ion composition; F107D from file 
% 2005.02 11/14/05 JF(18)=T: dip,MLAT IGRF10 (igrf_dip igrf.for); F:POGO-75;
% 2005.03 11/15/05 sunrise/sunset/night for D,E,F1,F2; UT_LT removed
% 2005.04 05/06/06 FIRI D-region option not tied to peak
% 2005.04 05/06/06 Spread-F included, NeQuick included
% 2005.05 01/15/07 NeQuick uses CCIR-M3000F2 even if user-hmF2  
% 2007.00 05/18/07 Release of IRI-2007
% 2007.01 01/23/08 ryear = .. (daynr-1.0)/idayy ---------- R. Scharroo
% 2007.02 10/31/08 OUTF(100) -> OUTF(500), numhei=numstp=500 
% 2007.03 02/12/09 Jf(24)=false-> OUTF(1,60-140km)=FIRI- M. Friedrich
% 2007.04 03/14/09 SOCO(70->80;500->300km) --------------- R. Davidson
% 2007.05 03/26/09 call for APF_ONLY includes F107M
% 2007.09 08/17/09 STROM off if input; FOF2IN, FOF1IN,FOEIN corr
% 2007.10 02/03/10 F10.7D = F10.7M = COV if EOF
% 2007.11 04/19/10 Corrections in irifun.for, cira.for 
% 2007.12 11/23/10 FNIGHT computed twice at 8334 --------- C. Vasly 
%
% 2012.00 10/05/11 IRI-2012: bottomside B0 B1 model (SHAMDB0D, SHAB1D),
% 2012.00 10/05/11  bottomside Ni model (iriflip.for), auroral foE
% 2012.00 10/05/11  storm model (storme_ap), Te with PF10.7 (elteik),
% 2012.00 10/05/11  oval kp model (auroral_boundary),IGRF-11(igrf.for), 
% 2012.00 10/05/11  NRLMSIS00 (cira.for), CGM coordinates, F10.7 daily
% 2012.00 10/05/11  81-day 365-day indices (apf107.dat), ap->kp (ckp),
% 2012.00 10/05/11  array size change JF(50) OUTF(20,1000), OARR(100).
% 2012.01 11/01/11 delete TEDER from EXTERNAL; GTD7 call 0 to 0.0
% 2012.01 12/12/11 put FMODIP in EXTERNAL; cgn_lon -> cgm_lon
% 2012.01 01/24/12 Change FLAT to LATI in SHAB1D call [D. Altadill]
% 2012.01 08/09/12 add JF(36)=t/f foF2 for hmF2 wout/with storm
% 2012.01 08/09/12 replace foF2_storm with foF2 for topside (NeQ, corr)
% 2012.01 08/09/12 call stormE_ap only if ap available
% 2012.01 08/09/12 If ap not available then auroral boundary for Kp=3
% 2012.02 12/17/12 Add magnetic declination as OARR(84) output
% 2012.03 02/13/13 Move B1 before B0 for Gulyaeva-1987
% 2012.03 02/20/13 Use foot-point for CGM to be closer to AACGM
% 2012.03 02/20/13 DAT(11,*) is UT time of MLT=0
%
%*****************************************************************
%********* INTERNATIONAL REFERENCE IONOSPHERE (IRI). *************
%*****************************************************************
%**************** ALL-IN-ONE SUBROUTINE  *************************
%*****************************************************************
%
%
%       SUBROUTINE IRI_SUB(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,DHOUR,
%          HEIBEG,HEIEND,HEISTP,OUTF,OARR)
%-----------------------------------------------------------------
%
% INPUT:  JF(1:50)      true/false switches for several options
%         JMAG          =0 geographic   = 1 geomagnetic coordinates
%         ALATI,ALONG   LATITUDE NORTH AND LONGITUDE EAST IN DEGREES
%         IYYYY         Year as YYYY, e.g. 1985
%         MMDD (-DDD)   DATE (OR DAY OF YEAR AS A NEGATIVE NUMBER)
%         DHOUR         LOCAL TIME (OR UNIVERSAL TIME + 25) IN DECIMAL 
%                          HOURS
%         HEIBEG,       HEIGHT RANGE IN KM; maximal 100 heights, i.e.
%          HEIEND,HEISTP        floor((heiend-heibeg)/heistp)+1 <= 100
%
%    JF switches to turn off/on (true/false) several options
%
%    i       true                  false          standard version
%    -----------------------------------------------------------------
%    1    Ne computed            Ne not computed                     t
%    2    Te, Ti computed        Te, Ti not computed                 t
%    3    Ne & Ni computed       Ni not computed                     t
%    4    B0,B1 - Bil-2000       B0,B1 - other models JF(31)     false
%    5    foF2 - CCIR            foF2 - URSI                     false
%    6    Ni - DS-1995 & DY-1985 Ni - RBV-2010 & TTS-2003        false
%    7    Ne - Tops: f10.7<188   f10.7 unlimited                     t            
%    8    foF2 from model        foF2 or NmF2 - user input           t
%    9    hmF2 from model        hmF2 or M3000F2 - user input        t
%   10    Te - Standard          Te - Using Te/Ne correlation        t
%   11    Ne - Standard Profile  Ne - Lay-function formalism         t
%   12    Messages to unit 6     to messages.text on unit 11         t
%   13    foF1 from model        foF1 or NmF1 - user input           t
%   14    hmF1 from model        hmF1 - user input (only Lay version)t
%   15    foE  from model        foE or NmE - user input             t
%   16    hmE  from model        hmE - user input                    t
%   17    Rz12 from file         Rz12 - user input                   t
%   18    IGRF dip, magbr, modip old FIELDG using POGO68/10 for 1973 t
%   19    F1 probability model   critical solar zenith angle (old)   t
%   20    standard F1            standard F1 plus L condition        t
%   21    ion drift computed     ion drift not computed          false
%   22    ion densities in %     ion densities in m-3                t
%   23    Te_tops (Bil-1985)     Te_topside (TBT-2012)           false
%   24    D-region: IRI-1990     FT-2001 and all opts in OUTF(14,*)  t
%   25    F107D from APF107.DAT  F107D user input (OARR(41))         t
%   26    foF2 storm model       no storm updating                   t
%   27    IG12 from file         IG12 - user                         t
%   28    spread-F probability 	 not computed                    false
%   29    IRI01-topside          new options as def. by JF(30)   false
%   30    IRI01-topside corr.    NeQuick topside model   	     false 
% (29,30) = (t,t) IRIold, (f,t) IRIcor, (f,f) NeQuick, (t,f) Gulyaeva
%   31    B0,B1 ABT-2009	     B0 Gulyaeva-1987 h0.5               t   
% (4,31) = (t,t) Bil-00, (f,t) ABT-09, (f,f) Gul-87, (t,f) not used
%   32    F10.7_81 from file     F10.7_81 - user input (OARR(46))    t
%   33    Auroral boundary model on/off  true/false	             false
%   34    Messages on            Messages off                        t
%   35    foE storm model        no foE storm updating           false
%   36    hmF2 w/out foF2_storm  with foF2-storm                     t
%   37    topside w/out foF2-storm  with foF2-storm                  t
%   38    turn WRITEs off in IRIFLIP   turn WRITEs on                t
%   ..    ....
%   50    ....
%   ------------------------------------------------------------------
%
%  Depending on the JF() settings additional INPUT parameters may 
%  be required:
%
%       Setting              INPUT parameter
%    -----------------------------------------------------------------
%    JF(8)  =false     OARR(1)=user input for foF2/MHz or NmF2/m-3
%    JF(9)  =false     OARR(2)=user input for hmF2/km or M(3000)F2
%    JF(10 )=false     OARR(15),OARR(16)=user input for Ne(300km),
%       Ne(400km)/m-3. Use OARR()=-1 if one of these values is not 
%       available. If JF(23)=false then Ne(300km), Ne(550km)/m-3.
%    JF(13) =false     OARR(3)=user input for foF1/MHz or NmF1/m-3 
%    JF(14) =false     OARR(4)=user input for hmF1/km
%    JF(15) =false     OARR(5)=user input for foE/MHz or NmE/m-3 
%    JF(16) =false     OARR(6)=user input for hmE/km
%    JF(17) =false     OARR(33)=user input for Rz12
%    JF(21) =true      OARR(41)=user input for daily F10.7 index
%    JF(23) =false     OARR(41)=user input for daily F10.7 index
%    JF(24) =false     OARR(41)=user input for daily F10.7 index
%          optional for JF(21:24); default is F10.7D=COV
%    JF(25) =false     OARR(41)=user input for daily F10.7 index
%          if OARR(41) <= 0 then 12-month running mean is 
%          taken from internal file]
%    JF(27) =false     OARR(39)=user input for IG12
%    JF(28) =false     OARR(41)=user input for daily F10.7 index
%
%
%  OUTPUT:  OUTF(1:20,1:1000)
%               OUTF(1,*)  ELECTRON DENSITY/M-3
%               OUTF(2,*)  NEUTRAL TEMPERATURE/K
%               OUTF(3,*)  ION TEMPERATURE/K
%               OUTF(4,*)  ELECTRON TEMPERATURE/K
%               OUTF(5,*)  O+ ION DENSITY/% or /M-3 if JF(22)=f 
%               OUTF(6,*)  H+ ION DENSITY/% or /M-3 if JF(22)=f
%               OUTF(7,*)  HE+ ION DENSITY/% or /M-3 if JF(22)=f
%               OUTF(8,*)  O2+ ION DENSITY/% or /M-3 if JF(22)=f
%               OUTF(9,*)  NO+ ION DENSITY/% or /M-3 if JF(22)=f
%                 AND, IF JF(6)=false:
%               OUTF(10,*)  CLUSTER IONS DEN/% or /M-3 if JF(22)=f
%               OUTF(11,*)  N+ ION DENSITY/% or /M-3 if JF(22)=f
%               OUTF(12,*)  
%               OUTF(13,*)  
%  if JF(24)    OUTF(14,1:11) standard IRI-Ne for 60,65,..,110km 
%     =false)        12:22) Friedrich (FIRI) model at these heights 
%                      23:33) standard Danilov (SW=0, WA=0) 
%                      34:44) for minor Stratospheric Warming (SW=0.5) 
%                      45:55) for major Stratospheric Warming (SW=1) 
%                      56:66) weak Winter Anomaly (WA=0.5) conditions
%                      67:77) strong Winter Anomaly (WA=1) conditions
%               OUTF(15-20,*)  free
%
%            OARR(1:100)   ADDITIONAL OUTPUT PARAMETERS         
%
%      #OARR(1) = NMF2/M-3           #OARR(2) = HMF2/KM
%      #OARR(3) = NmF1/M-3           #OARR(4) = HMF1/KM
%      #OARR(5) = NME/M-3            #OARR(6) = HME/KM
%       OARR(7) = NMD/M-3             OARR(8) = HMD/KM
%       OARR(9) = HHALF/KM            OARR(10) = B0/KM
%       OARR(11) =VALLEY-BASE/M-3     OARR(12) = VALLEY-TOP/KM
%       OARR(13) = TE-PEAK/K          OARR(14) = TE-PEAK HEIGHT/KM
%      #OARR(15) = TE-MOD(300KM)     #OARR(16) = TE-MOD(400KM)/K
%       OARR(17) = TE-MOD(600KM)      OARR(18) = TE-MOD(1400KM)/K
%       OARR(19) = TE-MOD(3000KM)     OARR(20) = TE(120KM)=TN=TI/K
%       OARR(21) = TI-MOD(430KM)      OARR(22) = X/KM, WHERE TE=TI
%       OARR(23) = SOL ZENITH ANG/DEG OARR(24) = SUN DECLINATION/DEG
%       OARR(25) = DIP/deg            OARR(26) = DIP LATITUDE/deg
%       OARR(27) = MODIFIED DIP LAT.  OARR(28) = Geographic latitude
%       OARR(29) = sunrise/dec. hours OARR(30) = sunset/dec. hours
%       OARR(31) = ISEASON (1=spring) OARR(32) = Geographic longitude
%      #OARR(33) = Rz12               OARR(34) = Covington Index
%       OARR(35) = B1                 OARR(36) = M(3000)F2
%      $OARR(37) = TEC/m-2           $OARR(38) = TEC_top/TEC*100.
%      #OARR(39) = GIND (IG12)        OARR(40) = F1 probability 
%      #OARR(41) = F10.7 daily        OARR(42) = c1 (F1 shape)
%       OARR(43) = daynr              OARR(44) = equatorial vertical 
%       OARR(45) = foF2_storm/foF2_quiet         ion drift in m/s
%      #OARR(46) = F10.7_81           OARR(47) = foE_storm/foE_quiet 
%       OARR(48) = spread-F probability          
%       OARR(49) = Geomag. latitude   OARR(50) = Geomag. longitude  
%       OARR(51) = ap at current time OARR(52) = daily ap
%       OARR(53) = INVDIP/degree      OARR(54) = MLT-Te
%       OARR(55) = CGM-latitude       OARR(56) = CGM-longitude
%       OARR(57) = CGM-MLT            OARR(58) = CGM lat eq. aurl bodry
%       OARR(59) = CGM-LATI(MLT=0)    OARR(60) = CGM-LATI for MLT=1
%       OARR(61) = CGM-LATI(MLT=2)    OARR(62) = CGM-LATI for MLT=3
%       OARR(63) = CGM-LATI(MLT=4)    OARR(64) = CGM-LATI for MLT=5
%       OARR(65) = CGM-LATI(MLT=6)    OARR(66) = CGM-LATI for MLT=7
%       OARR(67) = CGM-LATI(MLT=8)    OARR(68) = CGM-LATI for MLT=9
%       OARR(69) = CGM-LATI(MLT=10)   OARR(70) = CGM-LATI for MLT=11
%       OARR(71) = CGM-LATI(MLT=12)   OARR(72) = CGM-LATI for MLT=13
%       OARR(73) = CGM-LATI(MLT=14)   OARR(74) = CGM-LATI for MLT=15
%       OARR(75) = CGM-LATI(MLT=16)   OARR(76) = CGM-LATI for MLT=17
%       OARR(77) = CGM-LATI(MLT=18)   OARR(78) = CGM-LATI for MLT=19
%       OARR(79) = CGM-LATI(MLT=20)   OARR(80) = CGM-LATI for MLT=21
%       OARR(81) = CGM-LATI(MLT=22)   OARR(82) = CGM-LATI for MLT=23
%       OARR(83) = Kp at current time OARR(84) = magnetic declination 
%                # INPUT as well as OUTPUT parameter
%                $ special for IRIWeb (only place-holders)
%-----------------------------------------------------------------------        
%*****************************************************************
%*** THE ALTITUDE LIMITS ARE:  LOWER (DAY/NIGHT)  UPPER        ***
%***     ELECTRON DENSITY         60/80 KM       1500 KM       ***
%***     TEMPERATURES               60 KM        2500/3000 KM  ***
%***     ION DENSITIES             100 KM        1500 KM       ***
%*****************************************************************
%*****************************************************************
%*********            INTERNALLY                    **************
%*********       ALL ANGLES ARE IN DEGREE           **************
%*********       ALL DENSITIES ARE IN M-3           **************
%*********       ALL ALTITUDES ARE IN KM            **************
%*********     ALL TEMPERATURES ARE IN KELVIN       **************
%*********     ALL TIMES ARE IN DECIMAL HOURS       **************
%*****************************************************************
%*****************************************************************
%*****************************************************************

%      INTEGER    DAYNR,DDO,DO2,SEASON,SEADAY
%      REAL       LATI,LONGI,MO2,MO,MODIP,NMF2,MAGBR,INVDIP,IAPO,  
%     &           NmF1,NME,NMD,MM,MLAT,MLONG,NmF2s,NmEs
%      CHARACTER  FILNAM*12
%-web-for webversion
%      CHARACTER FILNAM*53
%
%      DIMENSION  ARIG(3),RZAR(3),F(3),E(4),XDELS(4),DNDS(4),
%     &  FF0(988),XM0(441),F2(13,76,2),FM3(9,49,2),ddens(5,11),
%     &  elg(7),FF0N(988),XM0N(441),F2N(13,76,2),FM3N(9,49,2),
%     &  INDAP(13),AMP(4),HXL(4),SCL(4),XSM(4),MM(5),DTI(4),AHH(7),
%     &  STTE(6),DTE(5),ATE(7),TEA(6),XNAR(2),param(2),OARR(100),
%     &  OUTF(20,1000),DDO(4),DO2(2),DION(7),
%     &  osfbr(25),D_MSIS(9),T_MSIS(2),IAPO(7),SWMI(25),ab_mlat(48),
%     &  DAT(11,4), PLA(4), PLO(4)
%
%      LOGICAL  EXT,SCHALT,TECON(2),sam_mon,sam_yea,sam_ut,sam_date,
%     &  F1REG,FOF2IN,HMF2IN,URSIF2,LAYVER,DY,DREG,rzino,FOF1IN,
%     &  HMF1IN,FOEIN,HMEIN,RZIN,sam_doy,F1_OCPRO,F1_L_COND,NODEN,
%     &  NOTEM,NOION,TENEOP,OLD79,JF(50),URSIFO,IGIN,igino,
%     &  dnight,enight,FNIGHT,TOPO,TOPC,fstorm_on,estorm_on
%
%      COMMON /CONST/UMR  /const1/humr,dumr   /ARGEXP/ARGMAX
%     &	 /const2/icalls,nmono,iyearo,idaynro,rzino,igino,ut0
%     &   /BLOCK1/HMF2,NMF2,HMF1,F1REG  /BLOCK2/B0,B1,C1  
%     &   /BLOCK3/HZ,T,HST              /BLOCK4/HME,NME,HEF 
%     &   /BLOCK5/ENIGHT,E              /BLOCK6/HMD,NMD,HDX
%     &   /BLOCK7/D1,XKK,FP30,FP3U,FP1,FP2
%     &   /BLOCK8/HS,TNHS,XSM,MM,DTI,MXSM       
%c     &   /BLOTN/XSM1,TEXOS,TLBDH,SIGMA /BLOTE/AHH,ATE1,STTE,DTE
%     &   /BLOTE/AHH,ATE1,STTE,DTE
%     &   /BLO10/BETA,ETA,DELTA,ZETA    /findRLAT/FLON,RYEAR   
%     &   /BLO11/B2TOP,TC3,itopn,alg10,hcor1       
%     &   /iounit/konsol/QTOP/Y05,H05TOP,QF,XNETOP,XM3000,HHALF,TAU
  XNAR = zeros(2,1,IRI2012.float_t);
  TECON = zeros(2,1,IRI2012.float_t);
  %   DDO = zeros(4,1);
  %   DO2 = zeros(2,1);
  FF0N = zeros(IRI2012.F2numI*IRI2012.F2numJ,1,IRI2012.float_t);
  FF0 = zeros(IRI2012.F2numI*IRI2012.F2numJ,1,IRI2012.float_t);
  XM0N = zeros(IRI2012.FM3numI*IRI2012.FM3numJ,1,IRI2012.float_t);
  XM0 = zeros(IRI2012.FM3numI*IRI2012.FM3numJ,1,IRI2012.float_t);
  F = zeros(3,1,IRI2012.float_t);
  ddens = zeros(IRI2012.numDregTypes,IRI2012.numDregHeights,IRI2012.float_t);
  ATE = zeros(7,1,IRI2012.float_t);
  %      EXTERNAL          XE1,XE2,XE3_1,XE4_1,XE5,XE6,FMODIP

  %        save
        
  nummax=1000;

  if nargin < 10 || HEIEND < 0
    HEIEND = HEIBEG;
    HEISTP = 1; % irrelavent
    numhei = 1;
  elseif nargin < 11 || HEISTP <= 0
    HEISTP = HEIEND-HEIBEG;
    if HEISTP == 0
      numhei = 1;
    else
      numhei = 2;
    end
  else
    numhei = floor(abs(HEIEND-HEIBEG)/abs(HEISTP))+1;
  end
  if numhei > nummax
    numhei=nummax;
  end
  if ~JF(IRI2012.D_REGION_IRI1990_SW) && numhei < 7*IRI2012.numDregHeights
    numout = 7*IRI2012.numDregHeights;
  else
    numout = numhei;
  end
  OUTF = zeros(IRI2012.numResults,numout);
  for KI=1:IRI2012.numResults
    for kk=1:numout
      OUTF(KI,kk)=-1.;
    end
  end
  if nargin < 12
    OARR = [];
    if nargout > 1
      NO_ADDITIONAL_OUTPUT = false;
    else
      NO_ADDITIONAL_OUTPUT = true;
    end
  else
    NO_ADDITIONAL_OUTPUT = false;
  end
  if ~NO_ADDITIONAL_OUTPUT
    if isempty(OARR)
      OARR = zeros(IRI2012.numAdditionalResults,1);
    end
    %
    % OARR(1:6,15,16,33,39:41) is used for inputs
    % 
    for kind=IRI2012.NMD_OUT:IRI2012.TE_PEAK_HEIGHT_OUT
      OARR(kind)=-1.;
    end
    for kind=IRI2012.TE_MOD600_OUT:IRI2012.GEOG_LON_OUT
      OARR(kind)=-1.;
    end
    for kind=IRI2012.COV_IND_OUT:IRI2012.TEC_TOP_OUT
      OARR(kind)=-1.;
    end
    OARR(IRI2012.F1_PROB_OUT)=-1.;
    for kind=IRI2012.C1_OUT:IRI2012.numAdditionalResults
      if kind ~= IRI2012.F107_81_IN_OUT
        OARR(kind)=-1.;
      end
    end
  else
    JF(IRI2012.IONDRIFT_COMPUTED_SW) = false;
    JF(IRI2012.SPREAD_F_PROB_COMPUTED_SW) = false;
  end
  
  %
  % NEW-GUL------------------------------
  context.QF=1.;
  context.H05TOP=0.;
  % NEW-GUL------------------------------

  %
  % Code inserted to aleviate block data problem for PC version.
  % Thus avoiding DATA statement with parameters from COMMON block.
  %
  XDELS(1)=5.; % spring
  XDELS(2)=5.; % summer
  XDELS(3)=5.; % autumn
  XDELS(4)=10.; % winter
  DNDS(1)=.016; % spring
  DNDS(2)=.01; % summer
  DNDS(3)=.016; % autumn
  DNDS(4)=.016; % winter
  %   DDO(1)=9;
  %   DDO(2)=5;
  %   DDO(3)=5;
  %   DDO(4)=25;
  %   DO2(1)=5;
  %   DO2(2)=5;
  XNAR(1)=0.0;
  XNAR(2)=0.0;
  context.DTI(1)=10.;
  context.DTI(2)=10.;
  %
  % FIRST SPECIFY YOUR COMPUTERS CHANNEL NUMBERS ....................
  % AGNR=OUTPUT (OUTPUT IS DISPLAYED OR STORED IN FILE OUTPUT.IRI)...
  % IUCCIR=UNIT NUMBER FOR CCIR COEFFICIENTS ........................
  %
    %IUCCIR=10;
  %-web- special for web version
  %-web- messages should be turned off with JF(IRI2012.MESSAGES_ON_SW)=false and 
  %-web- konsol=1 used in IRIFUN

  if ~JF(IRI2012.MESSAGES_ON_SW)
    context.KONSOL = 0; % no output
  elseif JF(IRI2012.STANDARD_OUT_SW)
    context.KONSOL = 1; % standard out
  else
    context.KONSOL = fopen('messages.txt','a'); % append messages
  end
  context.ciractx.KONSOL = context.KONSOL;
  context.igrfctx.KONSOL = context.KONSOL;
  %
  % selection of density, temperature and ion composition options ......
  %

  NODEN=(~JF(IRI2012.Ne_COMPUTED_SW));
  NOTEM=(~JF(IRI2012.TeTi_COMPUTED_SW));
  NOION=(~JF(IRI2012.NeNi_COMPUTED_SW));
  if ~NOION
    NODEN=false;
  end
  DY=(~JF(IRI2012.Ni_DS_1995_SW));
  LAYVER=(~JF(IRI2012.Ne_STANDARD_SW));
  OLD79=(~JF(IRI2012.Ne_Tops_SW));
  F1_OCPRO=(JF(IRI2012.F1PROB_MODEL_SW));
  F1_L_COND=(~JF(IRI2012.F1_STANDARD_SW));
  DREG=JF(IRI2012.D_REGION_IRI1990_SW);
  TOPO=JF(IRI2012.IRI01_TOPSIDE_SW);
  TOPC=JF(IRI2012.IRI01_TOPSIDE_CORR_SW);
  %
  % rz12, IG12, F10.7D, PF10.7 input option ............................
  %
  RZIN=(~JF(IRI2012.RZ12_FILE_SW));
  if ~NO_ADDITIONAL_OUTPUT
    if RZIN
      ARZIN=OARR(IRI2012.RZ12_IN_OUT);
    else
      OARR(IRI2012.RZ12_IN_OUT)=-1.;
    end
  else
    RZIN = false;
  end

  IGIN=(~JF(IRI2012.IG12_FILE_SW));
  if ~NO_ADDITIONAL_OUTPUT
    if IGIN
      AIGIN=OARR(IRI2012.IG12_IN_OUT);
    else
      OARR(IRI2012.IG12_IN_OUT)=-1.;
    end

    if ~JF(IRI2012.F107D_FILE_SW)
      f107din=OARR(IRI2012.F107D_IN_OUT);
    else
      OARR(IRI2012.F107D_IN_OUT)=-1.;
    end

    if ~JF(IRI2012.F107_81_FILE_SW)
      f10781in=OARR(IRI2012.F107_81_IN_OUT);
    else
      OARR(IRI2012.F107_81_IN_OUT)=-1.;
    end
  else
    IGIN = false;
    JF(IRI2012.F107D_FILE_SW) = true;
    JF(IRI2012.F107_81_FILE_SW) = true;
  end
  %
  % Topside density ....................................................
  %
  if TOPO
    if TOPC
      context.itopn=IRI2012.TOPSIDE_IRI2001;
    else
      context.itopn=IRI2012.TOPSIDE_GulH05;
    end
  else 
    if TOPC
      context.itopn=IRI2012.TOPSIDE_CORRECTED;
    else
      context.itopn=IRI2012.TOPSIDE_NeQuick;
    end
  end
  %
  % F2 peak density ....................................................
  %
  FOF2IN=(~JF(IRI2012.FOF2_MODEL_SW));
  if FOF2IN && OARR(IRI2012.NMF2_IN_OUT) <= 0
    FOF2IN = false;
  end
  if ~NO_ADDITIONAL_OUTPUT
    if FOF2IN
      OARR1=OARR(IRI2012.NMF2_IN_OUT);
      if OARR1 < IRI2012.F_N_LIM
        AFOF2=OARR1;
        ANMF2=IRI2012.resonance*AFOF2*AFOF2;
      else %if OARR1 >= IRI2012.F_N_LIM
        ANMF2=OARR1;
        AFOF2=sqrt(ANMF2/IRI2012.resonance);
      end
      JF(IRI2012.FOF2_STORM_MODEL_SW)=false; % turn F2 storm model off
    else
      OARR(IRI2012.NMF2_IN_OUT)=-1.;
    end
  else
    FOF2IN = false;
  end
  URSIF2=(~JF(IRI2012.FOF2_CCIR_SW));
  %
  % F2 peak altitude ..................................................
  %
  HMF2IN=(~JF(IRI2012.HMF2_MODEL_SW));
  if HMF2IN && OARR(IRI2012.HMF2_IN_OUT) <= 0
    HMF2IN = false;
  end
  if ~NO_ADDITIONAL_OUTPUT
    if HMF2IN
      AHMF2=OARR(IRI2012.HMF2_IN_OUT);
    else
      OARR(IRI2012.HMF2_IN_OUT)=-1.;
    end
  else
    HMF2IN = false;
  end
  %
  % F1 peak density ...................................................
  %
  FOF1IN=(~JF(IRI2012.FOF1_MODEL_SW));
  if FOF1IN && OARR(IRI2012.NMF1_IN_OUT) <= 0
    FOF1IN = false;
  end
  if ~NO_ADDITIONAL_OUTPUT
    if FOF1IN
      OARR3=OARR(IRI2012.NMF1_IN_OUT);
      if OARR3 < IRI2012.F_N_LIM
        AFOF1=OARR3;
        ANMF1=IRI2012.resonance*AFOF1*AFOF1;
      else % if OARR3 >= IRI2012.F_N_LIM
        ANMF1=OARR3;
        AFOF1=sqrt(ANMF1/IRI2012.resonance);
      end
    else
      OARR(IRI2012.NMF1_IN_OUT)=-1.;
    end
  else
    FOF1IN = false;
  end
  %
  % F1 peak altitude ..................................................
  %
  HMF1IN=(~JF(IRI2012.HMF1_MODEL_SW));
  if HMF1IN && (OARR(IRI2012.HMF1_IN_OUT) <= 0 || ~LAYVER)
    HMF1IN = false;
  end
  if ~NO_ADDITIONAL_OUTPUT
    if HMF1IN
      AHMF1=OARR(IRI2012.HMF1_IN_OUT);
      if ~LAYVER && JF(IRI2012.MESSAGES_ON_SW)
        fprintf(context.KONSOL, ...
          ' *Ne* User input of hmF1 is only possible for the LAY-version\n');
      end
    else
      OARR(IRI2012.HMF1_IN_OUT)=-1.;
    end
  else
    HMF1IN = false;
  end
  %
  % E peak density ....................................................
  %
  FOEIN=(~JF(IRI2012.FOE_MODEL_SW));
  if FOEIN && OARR(IRI2012.NME_IN_OUT) <= 0
    FOEIN = false;
  end
  if ~NO_ADDITIONAL_OUTPUT
    if FOEIN
      OARR5=OARR(IRI2012.NME_IN_OUT);
      if OARR5 < IRI2012.F_N_LIM
        AFOE=OARR5;
        ANME=IRI2012.resonance*AFOE*AFOE;
      else % if OARR5 >= IRI2012.F_N_LIM
        ANME=OARR5;
        AFOE=sqrt(ANME/IRI2012.resonance);
      end
      JF(IRI2012.FOE_STORM_MODEL_SW)=false; % turn E storm model off
    else
      OARR(IRI2012.NME_IN_OUT)=-1.;
    end
  else
    FOEIN = false;
  end
  %
  % E peak altitude ..................................................
  %
  HMEIN=(~JF(IRI2012.HME_MODEL_SW));
  if HMEIN && OARR(IRI2012.HME_IN_OUT) <= 0
    HMEIN = false;
  end
  if ~NO_ADDITIONAL_OUTPUT
    if HMEIN
      AHME=OARR(IRI2012.HME_IN_OUT);
    else
      OARR(IRI2012.HME_IN_OUT)=-1.;
    end
  else
    HMEIN = false;
  end
  %
  % TE-NE MODEL OPTION ..............................................
  %
  TENEOP=(~JF(IRI2012.Te_STANDARD_SW));
  if ~NO_ADDITIONAL_OUTPUT
    if TENEOP
      for JXNAR=1:2
        XNAR(JXNAR)=OARR(JXNAR-1+IRI2012.TE_MOD300_IN_OUT);
        TECON(JXNAR)=false;
        if XNAR(JXNAR) > 0.
          TECON(JXNAR)=true;
        end
      end
    else
      OARR(IRI2012.TE_MOD300_IN_OUT)=-1.;
      OARR(IRI2012.TE_MOD400_IN_OUT)=-1.;
    end
  else
    TENEOP = false;
  end
  %
  % lists the selected options before starting the table
  %

  if context.icalls <= 1 && JF(IRI2012.MESSAGES_ON_SW)
    fprintf(context.KONSOL, ...
      '*** IRI parameters are being calculated ***\n');
    if ~NODEN
      if LAYVER
        fprintf(context.KONSOL, ...
          'Ne, E-F: The LAY-Version is prelimenary. Erroneous profile features can occur.\n');
      end
      if OLD79
        fprintf(context.KONSOL, ...
          'Ne: No upper limit for F10.7 in topside formula.\n');
      end
      if context.itopn == IRI2012.TOPSIDE_IRI2001
        fprintf(context.KONSOL,'Ne: IRI-2001 for Topside\n');
      end
      if context.itopn == IRI2012.TOPSIDE_CORRECTED
        fprintf(context.KONSOL,'Ne: Corrected Topside Formula\n');
      end
      if context.itopn == IRI2012.TOPSIDE_NeQuick
        fprintf(context.KONSOL,'Ne: NeQuick for Topside\n');
      end
      if context.itopn == IRI2012.TOPSIDE_GulH05
        fprintf(context.KONSOL,'Ne: Gul-h0.5 for Topside\n');
      end
      if FOF2IN
        fprintf(context.KONSOL,'Ne, foF2/NmF2: provided by user.\n');
      else
        if URSIF2
          fprintf(context.KONSOL,'Ne, foF2: URSI model is used.\n');
        else
          fprintf(context.KONSOL,'Ne, foF2: CCIR model is used.\n');
        end
        if HMF2IN
          fprintf(context.KONSOL,'Ne, hmF2/M3000F2: provided by user.\n');
        end
        if FOF1IN
          fprintf(context.KONSOL,'Ne, foF1/NmF1: provided by user.\n');
        end
        if HMF1IN && LAYVER
          fprintf(context.KONSOL,'Ne, hmF1: provided by user.\n');
        end
        if JF(IRI2012.B0B1_Bil2000_SW)
          fprintf(context.KONSOL,'Ne: B0,B1 Bil-2000\n');
        else 
          if JF(IRI2012.B0B1_ABT_2009_SW)
            fprintf(context.KONSOL,'Ne: B0,B1-ABT-2009\n');
          else
            fprintf(context.KONSOL,'Ne: B0 Gul-1987\n');
          end
        end
        if FOEIN
          fprintf(context.KONSOL,'Ne, foE/NmE: provided by user.\n');
        end
        if HMEIN
          fprintf(context.KONSOL,'Ne, hmE: provided by user.\n');
        end
        if F1_OCPRO
          fprintf(context.KONSOL,'Ne, foF1: probability function used.\n');
        end
        if F1_L_COND
          fprintf(context.KONSOL,'Ne, foF1: L condition cases included.\n');
        end
        if DREG
          fprintf(context.KONSOL,'Ne, D: IRI1990\n');
        else
          fprintf(context.KONSOL,'Ne, D: FT2001; IRI-90, FT-01, DRS-95)\n');
        end
        if JF(IRI2012.FOF2_STORM_MODEL_SW)
          if FOF2IN
            fprintf(context.KONSOL, ...
              'Ne, foF2: Storm model turned off if foF2 or NmF2 user input\n');
            JF(IRI2012.FOF2_STORM_MODEL_SW)=false;
          else
            fprintf(context.KONSOL,'Ne, foF2: storm model included\n');
          end
        end
      end
    end

    if (~NOION) && (DY)
      fprintf(context.KONSOL,'Ion Com.: RBV-10 & TTS-03\n');
    end
    if (~NOION) && (~DY)
      fprintf(context.KONSOL,'Ion Com.: DS-95 & DY-85\n');
    end

    if ~NOTEM
      if TENEOP
        fprintf(context.KONSOL,'Te: Temperature-density correlation is used.\n');
      end
      if JF(IRI2012.Te_TOPS_Bil_1985_SW)
        fprintf(context.KONSOL,'Te: Aeros/AE/ISIS model\n');
      else
        fprintf(context.KONSOL,'Te: TBT-2012 model\n');
      end

      if JF(IRI2012.AUR_BOUND_MODEL_SW)
          fprintf(context.KONSOL,'Auroral boundary model on\n');
      else
          fprintf(context.KONSOL,'Auroral boundary model off\n');
      end

      if JF(IRI2012.FOE_STORM_MODEL_SW)
        fprintf(context.KONSOL,'Ne, foE: storm model on\n');
      else
        fprintf(context.KONSOL,'Ne, foE: storm model off\n');
      end
    end
  end

  %
  % CALCULATION OF DAY OF YEAR OR MONTH/DAY AND DECIMAL YEAR 
  % NRDAYM is the number of days in the current month 
  % IDAYY is the number of days in the current year
  %
  %  leap year rule: years evenly divisible by 4 are leap years, except
  %  years also evenly divisible by 100 are not leap years, except years 
  %  also evenly divisible by 400 are leap years. The year 2000 is a 100 
  %  and 400 year exception and therefore it is a normal leap year. 
  %  The next 100 year exception will be in the year 2100!
  %

  IYEAR=floor(double(IYYYY));
  if IYEAR < 30
    IYEAR=IYEAR+2000;
  elseif IYEAR < 100
    IYEAR=IYEAR+1900;
  end
  idayy=365.0;
  if IRI2012.IS_LEAPYEAR( IYEAR )
    idayy=idayy+1;    % leap year
  end

  if MMDD < 0
    DAYNR=-floor(MMDD);
    [MONTH,IDAY,DAYNR,nrdaym] = IRI2012.MODA(1,IYEAR,DAYNR);
  else
    MONTH=floor(MMDD/100);
    IDAY=floor(MMDD)-MONTH*100;
    [MONTH,IDAY,DAYNR,nrdaym] = IRI2012.MODA(0,IYEAR,MONTH,IDAY);
  end
  context.RYEAR = double(IYEAR) + (double(DAYNR)-1.0)/idayy;
  IYD = double(IYEAR)*1000 + double(DAYNR);

  %
  % calculate center height for CGM computation
  %

  height_center=(HEIBEG+HEIEND)/2.;
        

  %
  % CALCULATION OF GEODETIC/GEOMAGNETIC COORDINATES (LATI, LONGI AND 
  % MLAT, MLONG), MAGNETIC INCLINATION (DIP), DIP LATITUDE (MAGBR) 
  % AND MODIFIED DIP (MODIP), ALL IN DEGREES
  %

  ALATI = double(ALATI);
  ALONG = mod(double(ALONG),360.0);
  if ALONG < 0.
    ALONG = ALONG + 360.; % -180/180 to 0-360
  end

  if JMAG == IGRF.GEOMAGNETIC_COORDINATES
    MLAT=ALATI;
    MLONG=ALONG;
    [LATI,LONGI,context.igrfctx] = context.igrfctx.GEODIP(IYEAR,MLAT,MLONG,JMAG);
  else
    LATI=ALATI;
    LONGI=ALONG;
    [MLAT,MLONG,context.igrfctx] = context.igrfctx.GEODIP(IYEAR,LATI,LONGI,JMAG);
  end
  [DEC,DIP,MAGBR,MODIP,context.igrfctx] = context.igrfctx.IGRF_DIP(LATI,LONGI,...
    context.RYEAR,300.0);
  if ~JF(IRI2012.IGRF_DIP_SW)
    [~,~,~,~,DIP,DEC,MODIP] = IGRF.FIELDG(LATI,LONGI,300.0);
    MAGBR=atan(0.5*tan(DIP*IRI2012.UMR))/IRI2012.UMR;
  end

  ABSLAT=abs(LATI);
  ABSMLT=abs(MLAT);
  ABSMDP=abs(MODIP);
  ABSMBR=abs(MAGBR);

  %
  % CALCULATION OF UT/LT and XMLT  ...............
  %
  if DHOUR <= IRI2012.UT_INDICATOR
    HOUR=DHOUR;					% dhour =< 24 is LT
    HOURUT=HOUR-LONGI/15.;
    if HOURUT < 0
      HOURUT=HOURUT+24.;
    end
  else
    HOURUT=DHOUR-IRI2012.UT_INDICATOR;				 % dhour>24 is UT+25
    HOUR=HOURUT+LONGI/15.;	 		 % HOUR becomes LT
    if HOUR > 24.
      HOUR=HOUR-24.;
    end
  end

  % original fortran has DAYNR but CLCMLT requires zero-based day numbers
  XMLT = context.igrfctx.CLCMLT(IYEAR,DAYNR-1.0,HOURUT,LATI,LONGI);

  %
  % SEASON assumes equal length seasons (92 days) with spring 
  % (SEASON=1) starting at day-of-year=45; for LATI < 0 adjustment 
  % for southern hemisphere is made. Some models require the
  % seasonal month (ISEAMON) or the seasonal day-of year (SEADAY)
  % ZMONTH is decimal month (Jan 1 = 1.0 and Dec 31 = 12.97)
  % SDAY is the day number reduced to a 360 day year (TOPH05)
  % NRDAYM is the number of days in the current month 
  % IDAYY is the number of days in the current year
  % 
      
  SEASON=floor((DAYNR+45.0)/92.0);
  if SEASON < IRI2012.SPRING
    SEASON=IRI2012.WINTER;
  end
  NSEASN=SEASON;				% Northern hemisphere season
  ZMONTH = double(MONTH) + (double(IDAY)-1)/double(nrdaym);
  % NEW-GUL------------------------------
  SDAY=double(DAYNR)/double(idayy)*360.;
  % NEW-GUL------------------------------
  SEADAY=DAYNR;
  ISEAMON=MONTH;
  if LATI < 0.0
    SEASON=SEASON-2;
    if SEASON < 1
      SEASON=SEASON+4;
    end
    ISEAMON=MONTH+6;
    if ISEAMON > 12
      ISEAMON=ISEAMON-12;
    end
    SEADAY=DAYNR+idayy/2.;
    if SEADAY > idayy
      SEADAY=SEADAY-idayy;
    end
    % NEW-GUL------------------------------
    SDAY=SDAY+180.;
    if SDAY > 360.
      SDAY=SDAY-360.;
    end
    % NEW-GUL------------------------------
  end

  %
  % 12-month running mean sunspot number (RSSN) and Ionospheric Global 
  % index (GIND), daily F10.7 cm solar radio flux (F107D) and monthly 
  % F10.7 (COV) index   
  %

  %sam_mon=(MONTH == MONTHO);
  sam_yea=(IYEAR == context.iyearo);
  sam_doy=(DAYNR == context.idaynro);
  sam_date=(sam_yea && sam_doy);
  sam_ut=(HOURUT == context.ut0);

  %if ~sam_date || context.rzino || RZIN || IGIN || context.igino

    [context.RZAR,context.ARIG,context.ttt,context.NMONTH] = context.TCON( ...
      IYEAR,MONTH,IDAY,DAYNR);
    if context.NMONTH < 0
      % out of range for IG_RZ.dat
      % jump to end of program
      context.icalls=context.icalls+1;
      if ~JF(IRI2012.STANDARD_OUT_SW) && JF(IRI2012.MESSAGES_ON_SW)
        fclose(context.KONSOL);
      end
      return;
    end

    if RZIN
      rrr = ARZIN;
      context.RZAR(1) = rrr;
      context.RZAR(2) = rrr;
      context.RZAR(3) = rrr;
      %       	zi=-12.349154+(1.4683266-2.67690893e-03*rrr)*rrr
      %       	if zi > 174.0) zi=174.0
      %       	context.ARIG(1) = zi
      %       	context.ARIG(2) = zi
      %       	context.ARIG(3) = zi
    end
    if IGIN
      zi = AIGIN;
      context.ARIG(1) = zi;
      context.ARIG(2) = zi;
      context.ARIG(3) = zi;
    end
    RSSN=context.RZAR(3);
    GIND=context.ARIG(3);
    context.COV=63.75+RSSN*(0.728+RSSN*0.00089);
    %        rlimit=GIND
    %        COVSAT=63.75+rlimit*(0.728+rlimit*0.00089)
    context.COVSAT=context.COV;
    if context.COVSAT > 188.
      context.COVSAT=188;
    end

    [F107_daily,f107pd,F107_81, ...
          F107_365,IAP_daily] = context.APF_ONLY(IYEAR,MONTH,IDAY);
    if F107_daily > -11.1
      F107D=F107_daily;
      F107Y=f107pd;
      F10781=F107_81;
      f107365=F107_365;
    else
      F107D=context.COV;
      F107Y=context.COV;
      F10781=context.COV;
      f107365=context.COV;
    end
    if ~JF(IRI2012.F107D_FILE_SW)
      F107D=f107din; 		% F10.7D user input
      F107Y=f107din; 		% F10.7PD user input
    end
    if ~JF(IRI2012.F107_81_FILE_SW)
      F10781=f10781in; 	% F10.7_81 user input
    end

    pf107 = (F107D+F10781)/2.;
  %end

  %
  % CALCULATION OF SOLAR ZENITH ANGLE (XHI/DEG), SUN DECLINATION ANGLE 
  % (SUNDEC),SOLAR ZENITH ANGLE AT NOON (XHINON) AND TIME OF LOCAL 
  % SUNRISE/SUNSET (SAX, SUX; dec. hours) AT 70 KM (D-REGION), 110 KM
  % (E-REGION), 200 KM (F1-REGION), AND 500 KM (TE, TI).
  %

  [SUNDEC,     ~, SAX80, SUX80] = IRI2012.SOCO(DAYNR,HOUR,LATI,LONGI, 80.);
  [     ~,     ~,SAX110,SUX110] = IRI2012.SOCO(DAYNR,HOUR,LATI,LONGI,110.);
  [     ~,   XHI,SAX200,SUX200] = IRI2012.SOCO(DAYNR,HOUR,LATI,LONGI,200.);
  [     ~,     ~,SAX300,SUX300] = IRI2012.SOCO(DAYNR,HOUR,LATI,LONGI,300.);
  [     ~,XHINON,     ~,     ~] = IRI2012.SOCO(DAYNR,12.0,LATI,LONGI,110.);
  DNIGHT=false;
  if abs(SAX80) == IRI2012.SUN_NEVER_RISES
    if SAX80 < 0.0
      DNIGHT=true;
    end
  else
    if SAX80 > SUX80
      if (HOUR > SUX80) && (HOUR < SAX80)
        DNIGHT=true;
      end
    else
      if (HOUR > SUX80) || (HOUR < SAX80)
        DNIGHT=true;
      end
    end
  end
  context.ENIGHT=false;
  if abs(SAX110) == IRI2012.SUN_NEVER_RISES
    if SAX110 < 0.0
      context.ENIGHT=true;
    end
  else
    if SAX110 > SUX110
      if (HOUR > SUX110) && (HOUR < SAX110)
        context.ENIGHT=true;
      end
    else
      if (HOUR > SUX110) || (HOUR < SAX110)
        context.ENIGHT=true;
      end
    end
  end
  FNIGHT=false;
  if abs(SAX200) == IRI2012.SUN_NEVER_RISES
    if SAX200 < 0.0
      FNIGHT=true;
    end
  else
    if SAX200 > SUX200
      if (HOUR > SUX200) && (HOUR < SAX200)
        FNIGHT=true;
      end
    else
      if (HOUR > SUX200) || (HOUR < SAX200)
        FNIGHT=true;
      end
    end
  end
  %
  % CALCULATION OF ELECTRON DENSITY PARAMETERS................
  % lower height boundary (HNEA), upper boundary (HNEE)
  %
      
  HTE=Inf;
  HNEA=65.;
  if DNIGHT
    HNEA=80.;
  end
  HNEE=2000.;
  if ~NODEN

    if ABSMDP >= 18.
      DELA=1.0+exp(-(ABSMDP-30.0)/10.0);
    else
      DELA=1.0+exp(-(18.-30.0)/10.0);
    end
    %DELL=1+exp(-(ABSLAT-20.)/10.);
    %
    % E peak critical frequency (foE), density (NmE), and height (hmE)
    %
    if FOEIN
      foE=AFOE;
      context.NME=ANME;
    else
      foE=IRI2012.FOEEDI(context.COV,XHI,XHINON,ABSLAT);
      context.NME=IRI2012.resonance*foE*foE;
    end
    if HMEIN
      context.HME=AHME;
    else
      context.HME=110.0;
    end
    %
    % F2 peak critical frequency foF2, density NmF2, and height hmF2
    %
    if ~FOF2IN || ~HMF2IN || context.itopn == IRI2012.TOPSIDE_NeQuick
      %if RZIN || context.rzino || IGIN || context.igino

        %
        % LINEAR INTERPOLATION IN SOLAR ACTIVITY. IG12 used for foF2
        %
        if URSIF2
          F2 = IRI2012.F2ursi;
        else
          F2 = IRI2012.F2ccir;
        end

        RR2=cast(context.ARIG(1)/100.,IRI2012.float_t);
        RR2N=cast(context.ARIG(2)/100.,IRI2012.float_t);
        RR1=cast(1.,IRI2012.float_t)-RR2;
        RR1N=cast(1.,IRI2012.float_t)-RR2N;
        for I=1:IRI2012.F2numJ
          for J=1:IRI2012.F2numI
            K=J+IRI2012.F2numI*(I-1);
            FF0N(K)=F2(context.NMONTH,J,I,1)*RR1N+F2(context.NMONTH,J,I,2)*RR2N;
            FF0(K)=F2(MONTH,J,I,1)*RR1+F2(MONTH,J,I,2)*RR2;
          end
        end

        RR2=cast(context.RZAR(1)/100.,IRI2012.float_t);
        RR2N=cast(context.RZAR(2)/100.,IRI2012.float_t);
        RR1=cast(1.,IRI2012.float_t)-RR2;
        RR1N=cast(1.,IRI2012.float_t)-RR2N;
        for I=1:IRI2012.FM3numJ
          for J=1:IRI2012.FM3numI
            K=J+IRI2012.FM3numI*(I-1);
            XM0N(K)=IRI2012.FM3ccir(context.NMONTH,J,I,1)*RR1N+IRI2012.FM3ccir(context.NMONTH,J,I,2)*RR2N;
            XM0(K)=IRI2012.FM3ccir(MONTH,J,I,1)*RR1+IRI2012.FM3ccir(MONTH,J,I,2)*RR2;
          end
        end
      %end
      zfof2  = IRI2012.FOUT(MODIP,LATI,LONGI,HOURUT,FF0);
      fof2n  = IRI2012.FOUT(MODIP,LATI,LONGI,HOURUT,FF0N);
      zm3000 = IRI2012.XMOUT(MODIP,LATI,LONGI,HOURUT,XM0);
      xm300n = IRI2012.XMOUT(MODIP,LATI,LONGI,HOURUT,XM0N);
      midm=15;
      if MONTH == 2
        midm=midm-1;
      end
      if IDAY < midm
        yfof2 = fof2n + context.ttt * (zfof2-fof2n);
        context.XM3000= xm300n+ context.ttt * (zm3000-xm300n);
      else
        yfof2 = zfof2 + context.ttt * (fof2n-zfof2);
        context.XM3000= zm3000+ context.ttt * (xm300n-zm3000);
      end
    end
    if FOF2IN
      foF2=AFOF2;
      context.NMF2=ANMF2;
    else
      foF2=yfof2;
      context.NMF2=IRI2012.resonance*foF2*foF2;
    end
    %
    % stormtime updating for foF2 (foF2s, NmF2s) and foE (foEs,
    % NmEs) and auroral boundary computation.
    %
    fstorm_on=JF(IRI2012.FOF2_STORM_MODEL_SW) && JF(IRI2012.FOF2_MODEL_SW);
    estorm_on=JF(IRI2012.FOE_STORM_MODEL_SW) && JF(IRI2012.FOE_MODEL_SW);
    if (fstorm_on || JF(IRI2012.AUR_BOUND_MODEL_SW) || estorm_on) && ...
        (~sam_date || ~sam_ut)
      indap = context.APF(IYEAR,MONTH,IDAY,HOURUT);
    else
      indap(1) = IRI2012.BAD_AP;
      indap(13) = IRI2012.BAD_AP;
    end
    %
    % stormtime updating for foF2 (foF2s, NmF2s) 
    %
    if fstorm_on && (indap(1) ~= IRI2012.BAD_AP)
      icoord=1;
      kut=floor(HOURUT);
      [stormcorr,~] = IRI2012.STORM(indap,LATI,LONGI,icoord,kut,DAYNR);
      foF2s=foF2*stormcorr;
      NmF2s=IRI2012.resonance*foF2s*foF2s;
    else
      foF2s=foF2;
      NmF2s=context.NMF2;
      stormcorr=-1.;
    end
    %
    % stormtime updating for foE (foEs, NmEs)
    %
    if estorm_on && (indap(13) ~= IRI2012.BAD_AP)
      estormcor = context.STORME_AP(DAYNR,MLAT,indap(13));
      if estormcor > -2.0
        foEs=foE*estormcor;
      else
        foEs=foE;
      end
      NmEs=IRI2012.resonance*foEs*foEs;
    else
      NmEs=context.NME;
      estormcor=-1.;
    end
    %
    % calculation of equatorward auroral boundary
    %
    if JF(IRI2012.AUR_BOUND_MODEL_SW)
      if indap(13) ~= IRI2012.BAD_AP
        xkp=IRI2012.CKP(indap(13));
      else     
        xkp=3.0;
      end
      % Corrected magnetic latitude CGM of equatorward boundary, 
      % ab_mlat(48), for MLT=0.0,0.5,1.0 ... 23.5 h and kp=xkp
      context.DAT(1,1)=LATI;
      context.DAT(2,1)=LONGI;
      [context.DAT,~,~,context.igrfctx] = context.igrfctx.GEOCGM01(1,IYEAR, ...
        height_center,context.DAT);
      cgm_lat=context.DAT(3,1);
      cgm_lon=context.DAT(4,1);
      cgm_mlt00_ut=context.DAT(11,1);
      cgm_mlt=HOURUT-cgm_mlt00_ut;
      if cgm_mlt < 0.
        cgm_mlt=24.+HOURUT-cgm_mlt00_ut;
      end
      %
      % CGM latitude of boundary (cgmlat) for present MLT value
      % 2012.02 12/17/12 Add magnetic declination as OARR(84) output
      zmlt=XMLT;
      %            zmlt=cgm_mlt
      if zmlt >= 0.0 && zmlt <= 24.0
        [cgmlat,ab_mlat] = IRI2012.AURORAL_BOUNDARY(xkp,zmlt);
      else
        cgmlat=100.0;
        [~,ab_mlat] = IRI2012.AURORAL_BOUNDARY(xkp,-1.0);
      end
    else
      cgm_lat = 100.0;
      cgm_lon = 0.0;
      cgm_mlt = -1.0;
      cgmlat = 100.0;
      ab_mlat = zeros(2*24,1);
      xkp=3.0;
    end
    %
    % computing hmF2. set JF(36)=false to use foF2_storm in hmF2 formula
    %  
    if HMF2IN
      if AHMF2 < 50.0
        context.XM3000=AHMF2;
        context.HMF2=IRI2012.HMF2ED(MAGBR,RSSN,foF2/foE,context.XM3000);
      else
        context.HMF2=AHMF2;
        %context.XM3000=IRI2012.XM3000HM(MAGBR,RSSN,foF2/foE,context.HMF2);
      end
    else
      if JF(IRI2012.HMF2_WO_STORM_SW)
        ratf=foF2/foE;
      else
        ratf=foF2s/foE;
      end
      context.HMF2=IRI2012.HMF2ED(MAGBR,RSSN,ratf,context.XM3000);
    end

    context.nmono=context.nmono;
    context.iyearo=IYEAR;
    context.idaynro=DAYNR;
    context.rzino=RZIN;
    context.igino=IGIN;
    context.ut0=HOURUT;

    %
    % topside profile parameters .............................
    %
    COS2=cos(MLAT*IRI2012.UMR);
    COS2=COS2*COS2;
    %
    % option to use unlimited F10.7M for the topside
    % previously: if OLD79) ETA1=-0.0070305*COS2
    %
    if OLD79
      FLU=(context.COV-40.0)/30.0;
    else
      FLU=(context.COVSAT-40.0)/30.0;
    end
    if JF(IRI2012.TOPSIDE_WO_STORM_SW)
      FO1 = foF2;
    else
      FO1 = foF2s;
    end
    EX=exp(-MLAT/15.);
    EX1=EX+1;
    EPIN=4.*EX/(EX1*EX1);
    ETA1=-0.02*EPIN;
    context.ETA = 0.058798 + ETA1 - ...
      FLU * (0.014065  - 0.0069724 * COS2) + ...
      FO1* (0.0024287 + 0.0042810 * COS2  - 0.0001528 * FO1);
    context.ZETA = 0.078922 - 0.0046702 * COS2 -  ...
      FLU * (0.019132  - 0.0076545 * COS2) + ...
      FO1* (0.0032513 + 0.0060290 * COS2  - 0.00020872 * FO1);
    context.BETA=-128.03 + 20.253 * COS2 - ...
      FLU * (8.0755  + 0.65896 * COS2) + ...
      FO1* (0.44041 + 0.71458 * COS2 - 0.042966 * FO1);
    Z=exp(94.5/context.BETA);
    Z1=Z+1;
    Z2=Z/(context.BETA*Z1*Z1);
    context.DELTA=(context.ETA/Z1-context.ZETA/2.0)/ ...
      (context.ETA*Z2+context.ZETA/400.0);
    %
    % Correction term for topside (Bilitza)
    %
    if context.itopn == IRI2012.TOPSIDE_CORRECTED
      zmp1 = exp(MODIP / 10.);
      zmp11 = 1. + zmp1;
      zmp111 = zmp1 / (zmp11 * zmp11);
      zmp2 = exp(MODIP / 19.);
      zmp22 = 1. + zmp2;
      zmp222 = zmp2 / (zmp22 * zmp22);
      r2n = -0.84 - 1.6 * zmp111;
      r2d = -0.84 - 0.64 * zmp111;
      x1n = 230. - 700. * zmp222;
      x1d = 550. - 1900. * zmp222;
      r2 = IRI2012.HPOL(HOUR,r2d,r2n,SAX300,SUX300,1.,1.);
      x1 = IRI2012.HPOL(HOUR,x1d,x1n,SAX300,SUX300,1.,1.);
      context.hcor1 = context.HMF2 + x1;
      x12 = 1500. - x1;
      context.TC3 = r2 / x12;
      % NEW-GUL--------------------------------
      %
      % Correction term for topside (Gulyaeva)
      %
    elseif context.itopn == IRI2012.TOPSIDE_GulH05
      %HEI05=0.;
      HEI05 = IRI2012.TOPH05(context.COV,MLAT,HOUR,context.HMF2,SDAY);
      context.H05TOP=HEI05;
      context.XNETOP=context.XE_1(context.H05TOP);
      % NEW-GUL--------------------------------

      %
      % NeQuick topside parameters (use CCIR-M3000F2 even if user-hmF2)
      %
    elseif context.itopn == IRI2012.TOPSIDE_NeQuick
      if JF(IRI2012.TOPSIDE_WO_STORM_SW)
        fo2=foF2;
      else
        fo2=foF2s;
      end
      dNdHmx=-3.467+1.714*log(fo2)+2.02*log(context.XM3000);
      dNdHmx=exp(dNdHmx)*0.01;
      B2bot=0.04774*fo2*fo2/dNdHmx;
      b2k = 3.22-0.0538*fo2-0.00664*context.HMF2+0.113*context.HMF2/B2bot ...
       +0.00257*RSSN;
      ee=exp(2.0*(b2k-1.0));
      b2k=(b2k*ee+1.0)/(ee+1.0);
      context.B2TOP=b2k*B2bot;
    end
    %
    % Bottomside thickness parameter B0 and shape parameters B1
    %                              
    GRAT = 0.0;
    if JF(IRI2012.B0B1_Bil2000_SW)
      context.B0=IRI2012.B0_98(HOUR,SAX200,SUX200,NSEASN,RSSN,LONGI,MODIP);
      context.B1=IRI2012.HPOL(HOUR,1.9,2.6,SAX200,SUX200,1.,1.);
    elseif JF(IRI2012.B0B1_ABT_2009_SW)
      context.FLON=LONGI+15.*HOURUT;
      if context.FLON > 360.
        context.FLON=context.FLON-360.;
      end
      X11=-90;
      X22=90;
      [FX11,context]=context.FMODIP(X11);
      [FX22,context]=context.FMODIP(X22);
      [SCHALT,XRLAT,context] = context.REGFA1(X11,X22,FX11,...
        FX22,0.001,MODIP,@FMODIP);
      if SCHALT
        XRLAT=LATI;
        if JF(IRI2012.MESSAGES_ON_SW)
          fprintf(context.KONSOL,...
            ' *NE* ABT-B0 computed with RLAT=LATI=%6.2g\n', LATI);
        end
      end
      RLAT=XRLAT;
      [B0L,context] = context.SHAMDB0D (RLAT,context.FLON,ZMONTH,RSSN);
      [B1L,context] = context.SHAB1D (LATI,context.FLON,ZMONTH,RSSN);
      context.B0 = B0L;
      context.B1 = B1L;
    else
      if FNIGHT
        GRAT = 0.91 - context.HMF2/4000.0;
      else
        [~,GRAT] = IRI2012.ROGUL(SEADAY,XHI);
      end
      context.B1=IRI2012.HPOL(HOUR,1.9,2.6,SAX200,SUX200,1.,1.);
      BCOEF = context.B1*(context.B1*(0.0046*context.B1-0.0548)+0.2546) + 0.3606;
      B0CNEW = context.HMF2*(1.0-GRAT);
      context.B0 = B0CNEW/BCOEF;
    end
    %
    % F1 layer height hmF1, critical frequency foF1, peak density NmF1
    %
    if FOF1IN
      foF1=AFOF1;
      NmF1=ANMF1;
    else
      foF1=IRI2012.FOF1ED(ABSMBR,RSSN,XHI);
      NmF1=IRI2012.resonance*foF1*foF1;
    end
    %
    % F1 layer thickness parameter c1
    %
    context.C1 = IRI2012.F1_C1(MODIP,HOUR,SUX200,SAX200);
    %
    % F1 occurrence probability with Scotto et al. 1997 or Ducharme et al. 
    % if JF(19)=F1_OCPRO=true or false
    % If ~JF(20)=F1_L_COND=true than Scotto model with L-condition
    %
    if F1_OCPRO
      [f1pbw,f1pbl] = IRI2012.F1_PROB(XHI,MLAT,RSSN);
      if F1_L_COND
        f1pb = f1pbl;
      else
        f1pb = f1pbw;
      end
      if FOF1IN || (f1pb >= 0.5)
        context.F1REG=true;
      else
        context.F1REG=false;
      end
    else			
      if FOF1IN || ((~FNIGHT) && (foF1 > 0.0))
        f1pb = 1.;
      else
        f1pb = 0.0;
      end
      if f1pb > 0.0
        context.F1REG=true;
      else
        context.F1REG=false;
      end
    end
            
    %
    % E-valley: depth, width, height of deepest point (HDEEP),
    % height of valley top (HEF)
    %
    XDEL=XDELS(SEASON)/DELA;
    DNDHBR=DNDS(SEASON)/DELA;
    HDEEP=IRI2012.HPOL(HOUR,10.5/DELA,28.,SAX110,SUX110,1.,1.);
    WIDTH=IRI2012.HPOL(HOUR,17.8/DELA,45.+22./DELA,SAX110,SUX110,1.,1.);
    DEPTH=IRI2012.HPOL(HOUR,XDEL,81.,SAX110,SUX110,1.,1.);
    DLNDH=IRI2012.HPOL(HOUR,DNDHBR,.06,SAX110,SUX110,1.,1.);
    if DEPTH >= 1.0
      if context.ENIGHT
        DEPTH=-DEPTH;
      end
      [EXT,context.E] = IRI2012.TAL(HDEEP,DEPTH,WIDTH,DLNDH);
      if EXT
        if JF(IRI2012.MESSAGES_ON_SW)
          fprintf(context.KONSOL,' *NE* E-REGION VALLEY CAN NOT BE MODELLED\n');
        end
        WIDTH=.0;
      end
    else
      WIDTH=.0;
    end
    context.HEF=context.HME+WIDTH;
    hefold=context.HEF;
    VNER = (1. - abs(DEPTH) / 100.) * NmEs;

    %
    % Parameters below E  .............................
    %

    %hmex=context.HME-9.;
    context.NMD=IRI2012.XMDED(XHI,RSSN,4.0E8);
    context.HMD=IRI2012.HPOL(HOUR,81.0,88.0,SAX80,SUX80,1.,1.);
    F(1)=IRI2012.HPOL(HOUR,0.02+0.03/DELA,0.05,SAX80,SUX80,1.,1.);
    F(2)=IRI2012.HPOL(HOUR,4.6,4.5,SAX80,SUX80,1.,1.);
    F(3)=IRI2012.HPOL(HOUR,-11.5,-4.0,SAX80,SUX80,1.,1.);
    context.FP1=F(1);
    context.FP2=-context.FP1*context.FP1/2.0;
    context.FP30=(-F(2)*context.FP2-context.FP1+1.0/F(2))/(F(2)*F(2));
    context.FP3U=(-F(3)*context.FP2-context.FP1-1.0/F(3))/(F(3)*F(3));
    context.HDX=context.HMD+F(2);
    %
    % indermediate region between D and E region; parameters xkk
    % and d1 are found such that the function reaches hdx/XDX/dxdh
    %
    X=context.HDX-context.HMD;
    XDX=context.NMD*exp(X*(context.FP1+X*(context.FP2+X*context.FP30)));
    DXDX=XDX*(context.FP1+X*(2.0*context.FP2+X*3.0*context.FP30));
    X=context.HME-context.HDX;
    context.XKK=-DXDX*X/(XDX*log(XDX/NmEs));
    %
    % if exponent xkk is larger than xkkmax, then xkk will be set to 
    % xkkmax and d1 will be determined such that the point hdx/XDX is 
    % reached; derivative is no longer continuous.
    %
    xkkmax=5.;
    if context.XKK > xkkmax
      context.XKK=xkkmax;
      context.D1=-log(XDX/NmEs)/(X^context.XKK);
    else
      context.D1=DXDX/(XDX*context.XKK*X^(context.XKK-1.0));
    end
    %
    % compute Danilov et al. (1995) D-region model values
    %
    if ~DREG
      vKp=1.;
      f5SW = [0.0,0.5,1.0,0.0,0.0];
      f6WA = [0.0,0.0,0.0,0.5,1.0];
      for jj=1:IRI2012.numDregTypes
        elg = IRI2012.DRegion(XHI,MONTH,F107D,vKp,f5SW(jj),f6WA(jj));
        for ii=1:IRI2012.numDregHeights
          if ii <= length(elg)
            ddens(jj,ii)=10.^(elg(ii)+6);
          else
            ddens(jj,ii)=0.;
          end
        end
      end
    end
    %
    % SEARCH FOR HMF1 ..................................................
    %

    if ~LAYVER
      context.HMF1=0;
      if context.F1REG
        HALT = 3;
        while HALT == 3
          % omit F1 feature if NmF1*0.9 is smaller than nme
          bnmf1=0.9*NmF1;
          HALT = 1;
          SCHALT = true;
          if NmEs < bnmf1
            %9245
            HALT = 0;
            while HALT == 0
              [XE2H,~,dXE2H]=context.XE2(context.HEF);
              if XE2H > bnmf1
                %context.HEF=context.HEF-1;
                %if dXE2H ~= 0
                %  context.HEF=context.HEF - floor((XE2H - bnmf1)/dXE2H);
                %else
                	context.HEF=context.HEF-1;
                %end
                if context.HEF <= 0
                  HALT = 1;
                end
                if context.HEF <= context.HME
                  HALT = 1;
                end
              else
                HALT = 2;
              end
            end
            if HALT == 2
              [SCHALT,context.HMF1] = context.REGFA1(context.HEF, ...
                 context.HMF2,XE2H,NmF2s,0.001,NmF1,@XE2);
            end
          end
          %9427
          if HALT == 1 || SCHALT == true
            %
            % omit F1 feature ....................................................
            %
            if JF(IRI2012.MESSAGES_ON_SW)
              fprintf(context.KONSOL, ...
                ' *NE* HMF1 IS NOT EVALUATED BY THE FUNCTION XE2\n');
              fprintf(context.KONSOL, ...
                ' CORR.: NO F1 REGION, HmF2=%f, B1=3, C1=0.0, NmF1=%e\n',context.HMF2,NmF1);
            end
            context.HMF1=0.;
            context.F1REG=false;
            %        NmF1=0.;
            %        context.C1=0.0;
          end
          %
          % Determine E-valley parameters if HEF was changed
          %

          if context.HEF ~= hefold
            WIDTH=context.HEF-context.HME;
            if context.ENIGHT
              DEPTH=-DEPTH;
            end
            [EXT,context.E] = IRI2012.TAL(HDEEP,DEPTH,WIDTH,DLNDH);
            if EXT
              if JF(IRI2012.MESSAGES_ON_SW)
                fprintf(context.KONSOL, ...
                  ' *NE* E-REGION VALLEY CAN NOT BE MODELLED\n');
              end
              %WIDTH=.0;
              context.HEF=context.HME;
              hefold=context.HEF;
              HALT = 3; % retry
              %goto 9245
            end
          end
        end
      end
      %
      % SEARCH FOR HST [NE3(HST)=NmEs] ......................................
      %
      %380
      if context.F1REG
        HF1=context.HMF1;
        XF1=NmF1;
      else
        HF1=(context.HMF2+context.HEF)/2.;
        XF1=XE2(context,HF1);
      end

      HF2=context.HEF;
      XF2=context.XE3_1(HF2);
      if XF2 <= NmEs
        [SCHALT,context.HST] = context.REGFA1(HF1,HF2,XF1,XF2,0.001,NmEs,@XE3_1);
      end
      if XF2 <= NmEs && ~SCHALT

        context.HZ=(context.HST+HF1)/2.0;
        D=context.HZ-context.HST;
        context.T=D*D/(context.HZ-context.HEF-D);
      else
        %
        % assume linear interpolation between HZ and HEF ..................
        %
        context.HZ=(context.HEF+HF1)/2.;
        XNEHZ=XE3_1(context,context.HZ);
        if JF(IRI2012.MESSAGES_ON_SW)
          fprintf(context.KONSOL,' *NE* HST IS NOT EVALUATED BY THE FUNCTION XE3\n');
          fprintf(context.KONSOL,'CORR.: LIN. APP. BETWEEN HZ=%5.1g AND HEF=%5.1g\n', ...
            context.HZ,context.HEF);
        end
        context.T=(XNEHZ-NmEs)/(context.HZ-context.HEF);
        context.HST=-333.;
      end
    else
      %
      % LAY-functions for middle ionosphere
      %
      %6153
      if HMF1IN
        HMF1M=AHMF1;
      else
        HMF1M=165.+0.6428*XHI;
      end
      context.HHALF = GRAT * context.HMF2;
      HV1R = context.HME + WIDTH;
      HV2R = context.HME + HDEEP;
      HHMF2 = context.HMF2;
      [HXL,SCL,AMP,IIQU] = IRI2012.INILAY(FNIGHT,context.F1REG,NmF2s, ...
        NmF1,NmEs,VNER,HHMF2,HMF1M,context.HME, ...
        HV1R,HV2R,context.HHALF);
      if (IIQU == 1) && JF(IRI2012.MESSAGES_ON_SW)
        fprintf(context.KONSOL, ...
          '*NE* LAY amplitudes found with 2nd choice of HXL(1).\n');
      end
      if (IIQU == 2) && JF(IRI2012.MESSAGES_ON_SW)
        fprintf(context.KONSOL,'*NE* LAY amplitudes could not be found.\n');
      end
    end
  end
  %
  %---------- CALCULATION OF NEUTRAL TEMPERATURE PARAMETER-------
  %
  HTA=60.0;
  HEQUI=120.0;
  SEC=HOURUT*3600.;
  IAPO = context.APFMSIS(IYEAR,MONTH,IDAY,HOUR);
  %           TRETRV(context.SWMI);
  if IAPO(2) == IRI2012.BAD_AP
    context.SWMI(CIRA.DAILY_AP_SW)=0.;
  else
    context.SWMI(CIRA.DAILY_AP_SW)=-1.0;
  end
  context.ciractx = context.ciractx.TSELEC(context.SWMI);
  INVDIP = -1.0;
  if ~NOTEM
    [~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SEC,HEQUI,LATI,LONGI,...
      HOUR,F10781,F107Y,IAPO,0.0,0);
    TN120=T_MSIS(CIRA.ALTITUDE_TEMP);
    if HOUR ~= 0.0
      if JF(IRI2012.IGRF_DIP_SW)
        SECNI=(24.-LONGI/15.0)*3600.;
      else
        iyz=IYEAR;
        idz=DAYNR;
        utni = 0.0;
        [utni,~,~,~] = IRI2012.UT_LT(IRI2012.LT_TO_UT,utni,0.0,LONGI,iyz,idz);
        SECNI=utni*3600.;
      end
      %[~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SECNI,HEQUI,LATI,LONGI,...
      %  0.0,F10781,F107Y,IAPO,0.0,0.0);
      %TN1NI=T_MSIS(CIRA.ALTITUDE_TEMP);
    else
      SECNI=0.;
      %TN1NI=T_MSIS(CIRA.ALTITUDE_TEMP);
    end

    %
    %--------- CALCULATION OF ELECTRON TEMPERATURE PARAMETER--------
    %

    % Te(120km) = Tn(120km)

    context.AHH(1)=120.;
    ATE(1)=TN120;

    % Te-MAXIMUM based on JICAMARCA and ARECIBO data 

    HMAXD=60.*exp(-(MLAT/22.41)^2)+210.;
    HMAXN=150.;
    context.AHH(2)=IRI2012.HPOL(HOUR,HMAXD,HMAXN,SAX200,SUX200,1.,1.);
    TMAXD=800.*exp(-(MLAT/33.)^2)+1500.;
    [~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SEC,HMAXN,LATI,LONGI,...
      HOUR,F10781,F107Y,IAPO,0.0,0.0);
    TMAXN=T_MSIS(CIRA.ALTITUDE_TEMP);
    ATE(2)=IRI2012.HPOL(HOUR,TMAXD,TMAXN,SAX200,SUX200,1.,1.);

    % Te(300km), Te(400km) from AE-C, Te(1400km), Te(3000km) from 
    % ISIS, Brace and Theis

    DIPLAT=MAGBR;
    TEA = IRI2012.TEBA(DIPLAT,HOUR,NSEASN);

    %icd=0;
    if JF(IRI2012.Te_TOPS_Bil_1985_SW)

      % Te at fixed heights taken from Brace and Theis

      context.AHH(3)=300.;
      context.AHH(4)=400.;
      context.AHH(5)=600.;
      context.AHH(6)=1400.;
      context.AHH(7)=3000.;
      HTE=3000;
      ATE(3)=TEA(1);
      ATE(4)=TEA(2);
      ATE(6)=TEA(3);
      ATE(7)=TEA(4);

      % Te(600km) from AEROS, Spenner and Plugge (1979)

      ETT=exp(-MLAT/11.35);
      TET=2900.-5600.*ETT/((ETT+1)^2.);
      TEN=839.+1161./(1.+exp(-(ABSMLT-45.)/5.));
      ATE(5)=IRI2012.HPOL(HOUR,TET,TEN,SAX300,SUX300,1.5,1.5);
    else

      % New model with solar activity effects included (Truhlik et al., 2011)
      % Te at fixed heights 350, 550, 650, 1400, and 2000 km

      context.AHH(3)=350.;
      context.AHH(4)=550.;
      context.AHH(5)=850.;
      context.AHH(6)=1400.;
      context.AHH(7)=2000.;
      HTE=2500;
      dimo=0.311653;
      icd=1;    % compute INVDIP
      isa=1;    % solar activity correction on
      %              ise=0;    % season correction off
      for ijk=3:7
        [xl,~,dipl,babs,context.igrfctx] = context.igrfctx.IGRF_SUB(LATI,LONGI,...
          context.RYEAR,context.AHH(ijk));
        if xl > 10.
          xl=10.;
        end
        [teh2,~,INVDIP]=IRI2012.ELTEIK(icd,isa,INVDIP,xl,dimo,babs,dipl, ...
            XMLT,context.AHH(ijk),DAYNR,pf107);
        ATE(ijk)=teh2;
      end
    end
    % Option to use Te = f(Ne) relation at ahh(3), ahh(4)

    if TENEOP
      for I=1:2
        if TECON(I)
          ATE(I+2)=IRI2012.TEDE(context.AHH(I+2),XNAR(I),-context.COV);
        end
      end
    end

    % Te corrected and Te > Tn enforced

    [~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SEC,context.AHH(2),...
      LATI,LONGI,HOUR,F10781,F107Y,IAPO,0.0,0.0);
    TNAHH2=T_MSIS(CIRA.ALTITUDE_TEMP);
    if ATE(2) < TNAHH2
      ATE(2)=TNAHH2;
    end
    STTE1=(ATE(2)-ATE(1))/(context.AHH(2)-context.AHH(1));
    for I=2:6
      [~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SEC,context.AHH(I+1),...
        LATI,LONGI,HOUR,F10781,F107Y, ...
           IAPO,0.0,0.0);
      TNAHHI=T_MSIS(CIRA.ALTITUDE_TEMP);
      if ATE(I+1) < TNAHHI
        ATE(I+1)=TNAHHI;
      end
      STTE2=(ATE(I+1)-ATE(I))/(context.AHH(I+1)-context.AHH(I));
      ATE(I)=ATE(I)-(STTE2-STTE1)*IRI2012.DTE(I-1)*IRI2012.ALOG2;
      STTE1=STTE2;
    end

    % Te gradients STTE are computed for each segment

    for I=1:6
      context.STTE(I)=(ATE(I+1)-ATE(I))/(context.AHH(I+1)-context.AHH(I));
    end
    context.ATE1=ATE(1);

    %
    %------------ CALCULATION OF ION TEMPERATURE PARAMETERS--------
    %

    % Ti(430km) during daytime from AEROS data

    XSM1=430.0;
    context.XSM(1)=XSM1;
    Z1=exp(-0.09*MLAT);
    Z2=Z1+1.;
    TID1 = 1240.0 - 1400.0 * Z1 / ( Z2 * Z2 );
    context.MM(2)=IRI2012.HPOL(HOUR,3.0,0.0,SAX300,SUX300,1.,1.);

    % Ti(430km) duirng nighttime from AEROS data

    Z1=ABSMLT;
    Z2=Z1*(0.47+Z1*0.024)*IRI2012.UMR;
    Z3=cos(Z2);
    if Z3 >= 0
      TIN1=1200.0-300.0*sqrt(abs(Z3));
    else
      TIN1=1200.0+300.0*sqrt(abs(Z3));
    end

    % Ti(430km) for specified time using HPOL

    TI1=TIN1;
    if TID1 > TIN1
      TI1=IRI2012.HPOL(HOUR,TID1,TIN1,SAX300,SUX300,1.,1.);
    end

    % Tn < Ti < Te enforced

    TEN1=context.ELTE(XSM1);
    [~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SECNI,XSM1,...
      LATI,LONGI,0.0,F10781,F107Y, ...
            IAPO,0.0,0.0);
    TNN1=T_MSIS(CIRA.ALTITUDE_TEMP);
    if TEN1 < TNN1
      TEN1=TNN1;
    end
    if TI1 > TEN1
      TI1=TEN1;
    end
    if TI1 < TNN1
      TI1=TNN1;
    end

    % Tangent on Tn profile determines HS

    context.HS=200.;
    [~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SEC,context.HS,...
      LATI,LONGI,HOUR,F10781,F107Y, ...
            IAPO,0.0,0.0);
    context.TNHS=T_MSIS(CIRA.ALTITUDE_TEMP);
    context.MM(1)=(TI1-context.TNHS)/(XSM1-context.HS);
    context.MXSM=2;

    % XTETI is altitude where Te=Ti

    XTTS=500.;
    X=500.+XTTS;
    XTETI = -1.0;
    while X < context.AHH(7)
      TEX=context.ELTE(X);
      TIX=context.TI(X);
      if TIX >= TEX
        X=X-XTTS;
        XTTS=XTTS/10.;
        if XTTS <= 0.1
          XTETI=X+XTTS*5.;

          % Ti=Te above XTETI 

          context.MXSM=3;
          context.MM(3)=context.STTE(6);
          context.XSM(2)=XTETI;
          if XTETI > context.AHH(6)
            break;
          end
          context.MXSM=4;
          context.MM(3)=context.STTE(5);
          context.MM(4)=context.STTE(6);
          context.XSM(3)=context.AHH(6);
          if XTETI > context.AHH(5)
            break;
          end
          context.MXSM=5;
          context.DTI(1)=5.;
          context.DTI(2)=5.;
          context.MM(3)=context.STTE(4);
          context.MM(4)=context.STTE(5);
          context.MM(5)=context.STTE(6);
          context.XSM(3)=context.AHH(5);
          context.XSM(4)=context.AHH(6);
        end
      end
      X=X+XTTS;
    end
  end
  %
  % CALCULATION OF ION DENSITY PARAMETER..................
  %
  if ~NOION
    HNIA=75.;
    if DY
      HNIA=80.;
    end
    HNIE=2000.;
  end
  %
  % CALCULATION FOR THE REQUIRED HEIGHT RANGE.......................
  % In the absence of an F1 layer hmf1=hz since hmf1 is used in XE
  %

  XHMF1=context.HMF1;
  if context.HMF1 <= 0.0
    context.HMF1=context.HZ;
  end

  HEIGHT=HEIBEG;

  for kk=1:numhei
    if ~NODEN
      %if (HNEA <= HEIGHT) && (HEIGHT <= HNEE)
        if LAYVER
          if IIQU < 2
            [ELEDE,dELEDE] = context.XEN(HEIGHT,context.HMF2,NmF2s,...
                  context.HME,length(SCL),HXL,SCL,AMP);
          else
            ELEDE=-9.;
            dELEDE=0.;
          end
        else
          [ELEDE,dELEDE]=context.XE_1(HEIGHT);
        end
      %else
      %  ELEDE = 0.0;
      %end
      %
      % electron density in m-3 in OUTF(1,*)
      %
      OUTF(IRI2012.EL_DENS_OUT,kk)=ELEDE;
      OUTF(IRI2012.EL_GRAD_OUT,kk)=dELEDE;
    end
    %
    % plasma temperatures
    %
    if ~NOTEM || (~NOION && DY && HEIGHT <= 300.)
      [~,T_MSIS,context.ciractx] = context.ciractx.GTD7(IYD,SEC,HEIGHT,...
        LATI,LONGI,HOUR,F10781,F107Y, ...
        IAPO,0.0,0.0);
      TNH=T_MSIS(CIRA.ALTITUDE_TEMP);
      dTNH=T_MSIS(CIRA.GRADIENT_TEMP);
      TIH=TNH;
      TEH=TNH;
      dTIH=dTNH;
      dTEH=dTNH;
      if (HTA <= HEIGHT) && (HEIGHT <= HTE)
        if HEIGHT > context.HS
          [TIH,dTIH]=context.TI(HEIGHT);
          if TIH < TNH
            TIH=TNH;
            dTIH=dTNH;
          end
        end
        if HEIGHT > HEQUI
          [TEH,dTEH]=context.ELTE(HEIGHT);
          if TEH < TIH
            TEH=TIH;
            dTEH=dTIH;
          end
        end
      end
      OUTF(IRI2012.NT_TEMP_OUT,kk)=TNH;
      OUTF(IRI2012.IO_TEMP_OUT,kk)=TIH;
      OUTF(IRI2012.EL_TEMP_OUT,kk)=TEH;
      OUTF(IRI2012.NT_GRAD_OUT,kk)=dTNH;
      OUTF(IRI2012.IO_GRAD_OUT,kk)=dTIH;
      OUTF(IRI2012.ET_GRAD_OUT,kk)=dTEH;
    end
    %
    % ion composition
    %
    if ~NOION
      %if (HNIA <= HEIGHT) && (HEIGHT <= HNIE)

        if DY
          if HEIGHT > 300.
            % Triskova-Truhlik-Smilauer-2003 model
            [xl,~,dipl,babs,context.igrfctx] = context.igrfctx.IGRF_SUB(LATI,...
              LONGI,context.RYEAR,HEIGHT);
            if xl > 10.
              xl=10.;
            end
            dimo=0.311653;
            [xic_O,xic_H,xic_He,xic_N] = IRI2012.CALION(1,0.0,xl,dimo,...
              babs,dipl,XMLT,HEIGHT,DAYNR,F107D);
            ROX=xic_O*100.;
            RHX=xic_H*100.;
            RNX=xic_N*100.;
            RHEX=xic_He*100.;
            RNOX=0.;
            RO2X=0.;
            RCLUST=0.;
          else
            % Richards-Bilitza-Voglozin-2010 IDC model
            [D_MSIS,~,context.ciractx] = context.ciractx.GTD7(IYD,SEC,HEIGHT,...
              LATI,LONGI,HOUR,F10781,F107Y,IAPO,context.ciractx.MT(CIRA.ALL_MASS),0.0);
            XN4S = 0.5 * D_MSIS(CIRA.N_DENS);
            if ELEDE < 1.0e60
              EDENS=ELEDE/1.e6;
            else
              EDENS=1.0e60/1.e6;
            end
            if JF(IRI2012.NO_WRITES_IRIFLIP_SW) || ~JF(IRI2012.MESSAGES_ON_SW)
              jprint=0;
            else
              jprint=1;
            end
            [ro,ro2,rno,~,rn,~,~,~] = context.CHEMION(jprint,...
              HEIGHT,F107Y,F10781,TEH,TIH,TNH, ...
              D_MSIS(CIRA.O_DENS),D_MSIS(CIRA.O2_DENS),D_MSIS(CIRA.N2_DENS),...
              D_MSIS(CIRA.HE_DENS),-1.0,XN4S,EDENS,-1.0,XHI);                              

            %			if INEWT > 0
            sumion = EDENS/100.;
            if sumion == 0.0
              ROX=ro;
              RHX=0.;
              RHEX=0.;
              RNX=rn;
              RNOX=rno;
              RO2X=ro2;
              RCLUST=0.;
            else
              ROX=ro/sumion;
              RHX=0.;
              RHEX=0.;
              RNX=rn/sumion;
              RNOX=rno/sumion;
              RO2X=ro2/sumion;
              RCLUST=0.;
            end
            %        		endif
          end
        else
          % Danilov-Smirnova-1995 model and Danilov-Yaichnikov-1985 model (upper)
          DION = IRI2012.IONDANI(IDAY,ISEAMON,HEIGHT,XHI,LATI,f107365);
          ROX=DION(1);
          RHX=DION(2);
          RNX=DION(3);
          RHEX=DION(4);
          RNOX=DION(5);
          RO2X=DION(6);
          RCLUST=DION(7);
        end

        %
        % ion densities are given in percent of total electron density;
        %

        if JF(IRI2012.IONDENS_PERCENT_SW)
          xnorm=1;
        elseif ELEDE == 0 || ELEDE == Inf;
          xnorm=1/100.;
        else
          xnorm=ELEDE/100.;
        end
        OUTF(IRI2012.O_DENS_OUT,kk)=ROX*xnorm;
        OUTF(IRI2012.H_DENS_OUT,kk)=RHX*xnorm;
        OUTF(IRI2012.HE_DENS_OUT,kk)=RHEX*xnorm;
        OUTF(IRI2012.O2_DENS_OUT,kk)=RO2X*xnorm;
        OUTF(IRI2012.NO_DENS_OUT,kk)=RNOX*xnorm;
        OUTF(IRI2012.CL_DENS_OUT,kk)=RCLUST*xnorm;
        OUTF(IRI2012.N_DENS_OUT,kk)=RNX*xnorm;
      %else
      %  OUTF(IRI2012.O_DENS_OUT,kk)=-1.0;
      %  OUTF(IRI2012.H_DENS_OUT,kk)=-1.0;
      %  OUTF(IRI2012.HE_DENS_OUT,kk)=-1.0;
      %  OUTF(IRI2012.O2_DENS_OUT,kk)=-1.0;
      %  OUTF(IRI2012.NO_DENS_OUT,kk)=-1.0;
      %  OUTF(IRI2012.CL_DENS_OUT,kk)=-1.0;
      %  OUTF(IRI2012.N_DENS_OUT,kk)=-1.0;
      %end
    end
    %
    % D region special: Friedrich&Torkar model in OUTF(13,*)
    %

    if ~DREG && HEIGHT <= 140.
      [EDENS,IERROR,dEDENS] = IRI2012.F00(HEIGHT,LATI,DAYNR,XHI,F107D);
      if IERROR == 0 || IERROR == 2
        OUTF(IRI2012.EL_DENS_OUT,kk)=EDENS;
        OUTF(IRI2012.EL_GRAD_OUT,kk)=dEDENS;
      else
        OUTF(IRI2012.EL_DENS_OUT,kk)=-1.;
        OUTF(IRI2012.EL_GRAD_OUT,kk)=-1.;
      end
    end

    HEIGHT=HEIGHT+HEISTP;
  end

  %
  % END OF PARAMETER COMPUTATION LOOP 
  %

  %
  % D region special: densities for 11 heights (60,65,70,..,110km)
  % OUTF(14,1:11)=IRI-07, OUTF(14,12:22)=FIRI, 
  % OUTF(14,23:33)= Danilov et al.(1995) with SW=0,WA=0 
  % OUTF(14,34:44)= with SW=0.5,WA=0, 
  % OUTF(14,45:55)= with SW=1,WA=0,  
  % OUTF(14,56:66)= with SW=0,WA=0.5, 
  % OUTF(14,67:77)= with SW=0,WA=1,  
  %

  if ~DREG
    for ii=1:IRI2012.numDregHeights
      Htemp=55+ii*5;
      if Htemp >= 65.
        [EDENS,dEDENS]=context.XE6(Htemp);
        OUTF(IRI2012.D_DENS_OUT,ii)=EDENS;
        OUTF(IRI2012.D_GRAD_OUT,ii)=dEDENS;
      else
        OUTF(IRI2012.D_DENS_OUT,ii)=-1.;
        OUTF(IRI2012.D_GRAD_OUT,ii)=-1.;
      end
      [EDENS,IERROR,dEDENS] = IRI2012.F00(Htemp,LATI,DAYNR,XHI,F107D);
      if IERROR == 0 || IERROR == 2
        OUTF(IRI2012.D_DENS_OUT,IRI2012.numDregHeights+ii)=EDENS;
        OUTF(IRI2012.D_GRAD_OUT,IRI2012.numDregHeights+ii)=dEDENS;
      else
        OUTF(IRI2012.D_DENS_OUT,IRI2012.numDregHeights+ii)=-1.0;
        OUTF(IRI2012.D_GRAD_OUT,IRI2012.numDregHeights+ii)=-1.0;
      end
      for jj=1:IRI2012.numDregTypes
        OUTF(IRI2012.D_DENS_OUT,(jj+1)*IRI2012.numDregHeights+ii)=ddens(jj,ii);
        OUTF(IRI2012.D_GRAD_OUT,(jj+1)*IRI2012.numDregHeights+ii)=-1.0;
      end
    end
  end

  %
  % equatorial vertical ion drift
  %

  if JF(IRI2012.IONDRIFT_COMPUTED_SW) && abs(MAGBR) < 25.0
    param(1)=DAYNR;
    param(2)=F107D;
    drift = IRI2012.VDRIFT(HOUR,LONGI,param);
  else
    drift=-1.;
  end
  %
  % spread-F occurrence probability
  %
  spreadf=-1.;
  if JF(IRI2012.SPREAD_F_PROB_COMPUTED_SW)
    if HOUR <= 7.25 || HOUR >= 17.75
      if abs(LATI) <= 25.0
        spfhour=HOUR;
        %daynr1=DAYNR;
        if HOUR < 12.0
          spfhour=HOUR+24.0;
          %daynr1=DAYNR-1;
          %if daynr1 < 1
          %  daynr1=idayy;
          %end
        end
        [osfbr,context] = context.SPREADF_BRAZIL(DAYNR,idayy,F107D,LATI);
        ispf=floor((spfhour-17.75)/0.5)+1;
        if ispf > 0 && ispf < 26
          spreadf=osfbr(ispf);
        end
      end
    end
  end
  %
  % ADDITIONAL PARAMETER FIELD OARR
  %

  if ~NO_ADDITIONAL_OUTPUT
    if ~NODEN
      OARR(IRI2012.NMF2_IN_OUT)=NmF2s;
      OARR(IRI2012.HMF2_IN_OUT)=context.HMF2;
      if context.F1REG
        OARR(IRI2012.NMF1_IN_OUT)=NmF1;
        OARR(IRI2012.HMF1_IN_OUT)=XHMF1;
      end
      OARR(IRI2012.NME_IN_OUT)=NmEs;
      OARR(IRI2012.HME_IN_OUT)=context.HME;
      OARR(IRI2012.NMD_OUT)=context.NMD;
      OARR(IRI2012.HMD_OUT)=context.HMD;
      OARR(IRI2012.HHALF_OUT)=context.HHALF;
      OARR(IRI2012.B0_OUT)=context.B0;
      OARR(IRI2012.VALLEY_BASE_OUT)=VNER;
      OARR(IRI2012.VALLEY_TOP_OUT)=context.HEF;
      OARR(IRI2012.F1_PROB_OUT)=f1pb;
      OARR(IRI2012.FOF2_STORM_OUT)=stormcorr;
      OARR(IRI2012.FOE_STORM_OUT)=estormcor;
      OARR(IRI2012.AP_CURRENT_OUT)=indap(13);   % ap for current UT
      OARR(IRI2012.CGM_LAT_OUT)=cgm_lat;
      OARR(IRI2012.CGM_LON_OUT)=cgm_lon;
      OARR(IRI2012.CGM_MLT_OUT)=cgm_mlt;
      OARR(IRI2012.CGM_LAT_AUR_BOUND_OUT)=cgmlat;   % CGM latitude of equatorward boundary
      % include only every second auroral boundary point (MLT=0,1,2..23)
      jjj=IRI2012.CGM_LAT_MLT0_OUT-1;
      for iii=1:2:2*24-1
        jjj=jjj+1;
        OARR(jjj)=ab_mlat(iii);
      end
      OARR(IRI2012.KP_CURRENT_OUT)=xkp;
    end
    if ~NOTEM
      OARR(IRI2012.TE_PEAK_OUT)=ATE(2);
      OARR(IRI2012.TE_PEAK_HEIGHT_OUT)=context.AHH(2);
      OARR(IRI2012.TE_MOD300_IN_OUT)=ATE(3);
      OARR(IRI2012.TE_MOD400_IN_OUT)=ATE(4);
      OARR(IRI2012.TE_MOD600_OUT)=ATE(5);
      OARR(IRI2012.TE_MOD1400_OUT)=ATE(6);
      OARR(IRI2012.TE_MOD3000_OUT)=ATE(7);
      OARR(IRI2012.TE_120_OUT)=ATE(1);
      OARR(IRI2012.TI_MOD430_OUT)=TI1;
      OARR(IRI2012.H_TE_TI_OUT)=XTETI;
    end
    OARR(IRI2012.SOL_ZENITH_ANG_OUT)=XHI;
    OARR(IRI2012.SUN_DEC_OUT)=SUNDEC;
    OARR(IRI2012.DIP_OUT)=DIP;
    OARR(IRI2012.DIP_LAT_OUT)=MAGBR;
    OARR(IRI2012.MODIP_LAT_OUT)=MODIP;
    OARR(IRI2012.GEOG_LAT_OUT)=LATI;
    OARR(IRI2012.SUNRISE_OUT)=SAX200;
    OARR(IRI2012.SUNSET_OUT)=SUX200;
    OARR(IRI2012.SEASON_OUT)=SEASON;
    OARR(IRI2012.GEOG_LON_OUT)=LONGI;
    OARR(IRI2012.RZ12_IN_OUT)=RSSN;
    OARR(IRI2012.COV_IND_OUT)=context.COV;
    OARR(IRI2012.B1_OUT)=context.B1;
    OARR(IRI2012.M3000_F2_OUT)=context.XM3000;
    % OARR(IRI2012.TEC_OUT) used for TEC and IRI2012.TEC_TOP_OUT for TEC-top
    OARR(IRI2012.IG12_IN_OUT)=GIND;
    OARR(IRI2012.F107D_IN_OUT)=F107D;
    OARR(IRI2012.C1_OUT)=context.C1;
    OARR(IRI2012.DAYNR_OUT)=DAYNR;
    OARR(IRI2012.EQ_VERT_OUT)=drift;
    OARR(IRI2012.F107_81_IN_OUT)=F10781;
    OARR(IRI2012.SPREAD_F_PROB_OUT)=spreadf;
    OARR(IRI2012.GEOM_LAT_OUT)=MLAT;
    OARR(IRI2012.GEOM_LON_OUT)=MLONG;
    OARR(IRI2012.AP_DAILY_OUT)=IAP_daily;   % daily ap
    OARR(IRI2012.INVDIP_OUT)=INVDIP;
    OARR(IRI2012.MLT_TE_OUT)=XMLT;
    OARR(IRI2012.MAG_DEC_OUT)=DEC;
  else
    OARR = cell.empty();
  end
  if ~JF(IRI2012.STANDARD_OUT_SW) && JF(IRI2012.MESSAGES_ON_SW)
    fclose(context.KONSOL);
  end

end

