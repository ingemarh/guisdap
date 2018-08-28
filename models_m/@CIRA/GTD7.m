function [ D,T, context ] = GTD7( context, IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D1 )
%GTD7 Neutral Atmosphere Empirical Model from the surface to lower exosphere
%      SUBROUTINE GTD7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
%-----------------------------------------------------------------------
%
%     NRLMSISE-00
%     -----------
%        Neutral Atmosphere Empirical Model from the surface to lower
%        exosphere
%        J.M. Picone, A.E. Hedin, D.P. Drob, and A.C. Aikin, NRLMSISE-00 
%             empirical model of the atmosphere: Statistical comparisons 
%             and scientific issues, J. Geophys. Res., 107(A12), 1468, 
%             doi:10.1029/2002JA009430, 2002.
%
%        NEW FEATURES:
%          *Extensive satellite drag database used in model generation
%          *Revised O2 (and O) in lower thermosphere
%          *Additional nonlinear solar activity term
%          *"ANOMALOUS OXYGEN" NUMBER DENSITY, OUTPUT D(9)
%           At high altitudes (> 500 km), hot atomic oxygen or ionized
%           oxygen can become appreciable for some ranges of subroutine
%           inputs, thereby affecting drag on satellites and debris. We
%           group these species under the term "anomalous oxygen," since
%           their individual variations are not presently separable with
%           the drag data used to define this model component.
%
%        SUBROUTINES FOR SPECIAL OUTPUTS:
%        
%        HIGH ALTITUDE DRAG: EFFECTIVE TOTAL MASS DENSITY 
%        (SUBROUTINE GTD7D, OUTPUT D(6))
%           For atmospheric drag calculations at altitudes above 500 km,
%           call SUBROUTINE GTD7D to compute the "effective total mass
%           density" by including contributions from "anomalous oxygen."
%           See "NOTES ON OUTPUT VARIABLES" below on D(6).
%
%        PRESSURE GRID (SUBROUTINE GHP7)
%          See subroutine GHP7 to specify outputs at a pressure level
%          rather than at an altitude.
%
%        OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.)
% 
%     INPUT VARIABLES:
%        IYD - YEAR AND DAY AS YYDDD (day of year from 1 to 365 (or 366))
%              (Year ignored in current model)
%        SEC - UT(SEC)
%        ALT - ALTITUDE(KM)
%        GLAT - GEODETIC LATITUDE(DEG)
%        GLONG - GEODETIC LONGITUDE(DEG)
%        STL - LOCAL APPARENT SOLAR TIME(HRS; see Note below)
%        F107A - 81 day AVERAGE OF F10.7 FLUX (centered on day DDD)
%        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
%        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
%           - ARRAY CONTAINING:
%             (1) DAILY AP
%             (2) 3 HR AP INDEX FOR CURRENT TIME
%             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
%             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
%             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
%             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
%                    TO CURRENT TIME
%             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR
%                    TO CURRENT TIME
%        MASS - MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
%                 CALCULATED.  MASS 0 IS TEMPERATURE.  MASS 48 FOR ALL.
%                 MASS 17 IS Anomalous O ONLY.)
%
%     NOTES ON INPUT VARIABLES: 
%        UT, Local Time, and Longitude are used independently in the
%        model and are not of equal importance for every situation.  
%        For the most physically realistic calculation these three
%        variables should be consistent (STL=SEC/3600+GLONG/15).
%        The Equation of Time departures from the above formula
%        for apparent local time can be included if available but
%        are of minor importance.
%
%        F107 and F107A values used to generate the model correspond
%        to the 10.7 cm radio flux at the actual distance of the Earth
%        from the Sun rather than the radio flux at 1 AU. The following
%        site provides both classes of values:
%        ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SOLAR_RADIO/FLUX/
%
%        F107, F107A, and AP effects are neither large nor well
%        established below 80 km and these parameters should be set to
%        150., 150., and 4. respectively.
%
%     OUTPUT VARIABLES:
%        D(1) - HE NUMBER DENSITY(CM^-3)
%        D(2) - O NUMBER DENSITY(CM^-3)
%        D(3) - N2 NUMBER DENSITY(CM^-3)
%        D(4) - O2 NUMBER DENSITY(CM^-3)
%        D(5) - AR NUMBER DENSITY(CM^-3)                       
%        D(6) - TOTAL MASS DENSITY(GM/CM^3)
%        D(7) - H NUMBER DENSITY(CM^-3)
%        D(8) - N NUMBER DENSITY(CM^-3)
%        D(9) - Anomalous oxygen NUMBER DENSITY(CM^-3)
%        D(10) - HE NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(11) - O NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(12) - N2 NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(13) - O2 NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(14) - AR NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(15) - TOTAL MASS DENSITY GRADIENT (GM CM^-3/KM)
%        D(16) - H NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(17) - N NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(18) - Anomalous oxygen NUMBER DENSITY GRADIENT (CM^-3/KM)
%        T(1) - EXOSPHERIC TEMPERATURE (K)
%        T(2) - TEMPERATURE AT ALT (K)
%        T(3) - TEMPERATURE GRADIENT AT ALT (K/KM)
%
%     NOTES ON OUTPUT VARIABLES:
%        TO GET OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.) 
%
%        O, H, and N are set to zero below 72.5 km
%
%        T(1), Exospheric temperature, is set to global average for
%        altitudes below 120 km. The 120 km gradient is left at global
%        average value for altitudes below 72 km.
%
%        D(6), TOTAL MASS DENSITY, is NOT the same for subroutines GTD7 
%        and GTD7D
%
%          SUBROUTINE GTD7 -- D(6) is the sum of the mass densities of the
%          species labeled by indices 1-5 and 7-8 in output variable D.
%          This includes He, O, N2, O2, Ar, H, and N but does NOT include
%          anomalous oxygen (species index 9).
%
%          SUBROUTINE GTD7D -- D(6) is the "effective total mass density
%          for drag" and is the sum of the mass densities of all species
%          in this model, INCLUDING anomalous oxygen.
%        
%     SWITCHES: The following is for test and special purposes:
%          
%        TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SW),
%        WHERE SW IS A 25 ELEMENT ARRAY CONTAINING 0. FOR OFF, 1. 
%        FOR ON, OR 2. FOR MAIN EFFECTS OFF BUT CROSS TERMS ON
%        FOR THE FOLLOWING VARIATIONS
%               1 - F10.7 EFFECT ON MEAN  2 - TIME INDEPENDENT
%               3 - SYMMETRICAL ANNUAL    4 - SYMMETRICAL SEMIANNUAL
%               5 - ASYMMETRICAL ANNUAL   6 - ASYMMETRICAL SEMIANNUAL
%               7 - DIURNAL               8 - SEMIDIURNAL
%               9 - DAILY AP             10 - ALL UT/LONG EFFECTS
%              11 - LONGITUDINAL         12 - UT AND MIXED UT/LONG
%              13 - MIXED AP/UT/LONG     14 - TERDIURNAL
%              15 - DEPARTURES FROM DIFFUSIVE EQUILIBRIUM
%              16 - ALL TINF VAR         17 - ALL TLB VAR
%              18 - ALL TN1 VAR           19 - ALL S VAR
%              20 - ALL TN2 VAR           21 - ALL NLB VAR
%              22 - ALL TN3 VAR           23 - TURBO SCALE HEIGHT VAR
%
%        To get current values of SW: CALL TRETRV(SW)
%
% Simplified version for F77 compilers and IRI (Sep 27,2010, dbilitza):
%		GTS7, GLOBE7: AP(1), P(1) -> AP(7), P(150)
%   		PD(150,9) -> PDA1(150),..,PDA9(150) 
%-----------------------------------------------------------------------

%      CHARACTER*4 ISDATE,ISTIME,NAME,ISD,IST,NAM
%      DIMENSION D(9),T(2),AP(7),DS(9),TS(2)
%      DIMENSION context.ZN3(5),context.ZN2(4),SV(25)
%      COMMON/GTS3C/TLB,S,DB04,DB16,DB28,DB32,DB40,DB48,DB01,ZA,T0,Z0
%     & ,G0,RL,DD,DB14,TR12
%      COMMON/MESO7/TN1(5),TN2(4),TN3(5),TGN1(2),TGN2(2),TGN3(2)
%      COMMON/LOWER7/PTM(10),PDM(10,8)
%      COMMON/PARM7/PT(150),PDA1(150),PDA2(150),PDA3(150),
%     $ PDA4(150),PDA5(150),PDA6(150),PDA7(150),PDA8(150),PDA9(150),
%     $ PS(150),PDL(25,2),PTL(100,4),PMA(100,10),SAM(100)
%      COMMON/DATIM7/ISD(3),IST(2),NAM(2)
%      COMMON/DATIME/ISDATE(3),ISTIME(2),NAME(2)
%      COMMON/CSW/SW(25),ISW,SWC(25)
%      COMMON/MAVG7/PAVGM(10)
%      COMMON/DMIX/DM04,DM16,DM28,DM32,DM40,DM01,DM14
%      COMMON/PARMB/GSURF,RE
%      COMMON/METSEL/IMR
%      SAVE
%      EXTERNAL GTD7BK
  T = zeros(CIRA.numTemperatures,1);
  D = zeros(CIRA.numDensities,1);
  if context.ISW ~= CIRA.ISWinitialized
    context = context.TSELEC(CIRA.allSwitchesOn);
  end
  %      Put identification data into common/datime/
  %for I=1:3
  %  context.ISDATE{I}=CIRA.ISD{I};
  %end
  %for I=1:2
  %  context.ISTIME{I}=CIRA.IST{I};
  %  context.NAME{I}=CIRA.NAM{I};
  %end
  if nargin < 12
    D1 = 0.0;
  end
  if nargin < 11
    MASS = CIRA.ALL_MASS;
  end
  if nargin < 10
    AP = zeros(1,CIRA.maxAP);
    for i=1:length(AP)
      AP(i) = context.defaultAp;
    end
  end
  if length(AP) < CIRA.maxAP
    AP1 = zeros(1,CIRA.maxAP);
    for i=1:length(AP)
      AP1(i) = AP(i);
    end
    for i=length(AP)+1:CIRA.maxAP
      AP1(i) = AP(length(AP));
    end
    AP = AP1;
  end
  if nargin < 9
    F107 = context.defaultF107;
  end
  if nargin < 8
    F107A = context.defaultF107;
  end
  if nargin < 7
    STLdLon = CIRA.DGTR/context.HR;
    STLdSec = context.SR/context.HR;
    STL = SEC*STLdSec + GLONG*STLdLon;
  end
  %
  %        Test for changed input
  [V1,context]=context.VTST7(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,1);
  context.XALT = ALT;
  %       Latitude variation of gravity (none for SW(2)=0)
  if context.SW(CIRA.TIME_INDEP_SW) == 0
    XLAT=context.defaultLatitude;
  else
    XLAT=GLAT;
  end
  %[GV,RE,GVdLat,REdLat] = context.GLATF(XLAT);
  [GV,RE,GVdLat,REdLat] = context.GLATFELLIPSOID(XLAT);
  context.GSURF(1,CIRA.Der0) = GV;
  context.RE(1,CIRA.Der0) = RE;
  if context.SW(CIRA.TIME_INDEP_SW) == 0
    context.GSURF(1,CIRA.DerLat)=0;
    context.RE(1,CIRA.DerLat)=0;
  else
    context.GSURF(1,CIRA.DerLat)=GVdLat;
    context.RE(1,CIRA.DerLat)=REdLat;
  end
  
  XMM=context.PDM(5,context.mainConstituent);
  %
  % THERMOSPHERE/MESOSPHERE (above ZN2(1))
  ALTT = zeros(1,CIRA.DerLast);
  if ALT > context.ZN2(1)
    ALTT(1,CIRA.Der0)=ALT;
    ALTT(1,CIRA.DerAlt) = 1.0;
  else
    ALTT(1,CIRA.Der0)=context.ZN2(1);
  end
  MSS=MASS;
  % Only calculate N2 in thermosphere if alt in mixed region
  if ALT < context.ZMIX && MASS ~= 0.0
    MSS=context.MT(context.mainConstituent);
  end
  % Only calculate thermosphere if input parameters changed
  % or altitude above ZN2(1) in mesosphere
  if V1 == 1. ||                            ... % if inputs changed
     ALT > context.ZN2(1) ||                ... % if altitude in mesosphere
     context.GTD7_ALAST > context.ZN2(1) || ... % if previous altitude in mesosphere
     MSS ~= context.MSSL                        % if molar mass different than previous
    [DSL,TSL,context] = context.GTS7(IYD,SEC,ALTT,GLAT,GLONG,STL,F107A,F107,AP,MSS,D1);
    context.DS = DSL;
    context.TS = TSL;
    context.DM28M=context.DM28;
    % metric adjustment
    if context.IMR == 1
      context.DM28M=context.DM28M*CIRA.CM3PERM3; % cm^-3 -> m^-3
    end
    context.MSSL=MSS;
  end
  for J=1:CIRA.numTemperatures
    T(J)=context.TS(J);
  end
  if ALT >= context.ZN2(1)
    for J=1:CIRA.numDensities
      D(J)=context.DS(J);
    end
  else
    %
    %       LOWER MESOSPHERE/UPPER STRATOSPHERE [between ZN3(1) and ZN2(1)]
    %         Temperature at nodes and gradients at end nodes
    %         Inverse temperature a linear function of spherical harmonics
    %         Only calculate nodes if input changed
    if (V1 == 1. || context.GTD7_ALAST >= context.ZN2(1)) && ALT < context.ZN2(1)
      [context.TN2,context.TGN2] = SplineCoefficients( context, context.MN1, context.TN1, context.TGN1, ...
        context.MN2, context.PMA, context.PAVGM, ...
        context.SW(CIRA.ALL_TN2_SW), context.SW(CIRA.ALL_TN3_SW), 11, -1, -2 );
    end
    %
    %       LOWER STRATOSPHERE AND TROPOSPHERE [below ZN3(1)]
    %         Temperature at nodes and gradients at end nodes
    %         Inverse temperature a linear function of spherical harmonics
    %         Only calculate nodes if input changed
    if (V1 == 1. || context.GTD7_ALAST >= context.ZN3(1)) && ALT < context.ZN3(1)
      [context.TN3,context.TGN3] = SplineCoefficients( context, context.MN2, context.TN2, context.TGN2, ...
        context.MN3, context.PMA, context.PAVGM, ...
        context.SW(CIRA.ALL_TN3_SW), 1.0, 6, 2, 2 );
    end
    if MASS ~= 0.0
      %          LINEAR TRANSITION TO FULL MIXING BELOW ZN2(1)
      DMC = zeros(1,CIRA.DerLast);
      if ALT > context.ZMIX
        DMC(1,CIRA.Der0)=1.-(context.ZN2(1)-ALT)/(context.ZN2(1)-context.ZMIX);
        DMC(1,CIRA.DerAlt)=1.0/(context.ZN2(1)-context.ZMIX);
      else
        DMC(1,CIRA.Der0)=0;
        DMC(1,CIRA.DerAlt)=0;
      end
      mainMassI = context.mainConstituent;
      DZ28 = CIRA.GetOutput( context.DS, mainMassI );
      ZH = zeros(1,CIRA.DerLast);
      ZH(1,CIRA.Der0) = ALT;
      ZH(1,CIRA.DerAlt) = 1.0;
      %      ***** N2 DENSITY ****
      [DZ,TZ]=context.DENSM(ZH,context.DM28M,XMM, ...
                         context.MN3,context.ZN3,context.TN3,context.TGN3,...
                         context.MN2,context.ZN2,context.TN2,context.TGN2);
      DZ28_2 = context.MixingCorrection(DZ,context.DM28M,DZ28,DMC,mainMassI);
      [D,T] = context.SetOutput(D,T,DZ28_2,TZ,mainMassI);

      for m=CIRA.HE_MASS:CIRA.ANOM_O_MASS
        %      ***** CONSTITUENT DENSITY ****
        if m == mainMassI || m == CIRA.HOT_O_MASS
          continue; % already done
        elseif (MASS == context.MT(m) || ...
                MASS == context.MT(CIRA.ALL_MASS)) && ...
            context.DISASSOCIATED(m) == 0
          DZm = CIRA.GetOutput( context.DS, m );
          DD = context.MixingCorrection(DZ28_2,DZ28,DZm,DMC,m);
          D = context.SetOutput(D,T,DD,TZ,m);
        else
          D = context.SetOutput(D,T,0.0,TZ,m);
        end
      end
      %
      %       TOTAL MASS DENSITY
      %
      if MASS == context.MT(CIRA.ALL_MASS)
        for d=CIRA.Der0:CIRA.DerLast
          if d == CIRA.DerSTL
            continue;
          end
          di = CIRA.DensityIndex(d,CIRA.ALL_MASS);
          DD = 0.0;
          for m = CIRA.HE_MASS:CIRA.N_MASS
            if m ~= CIRA.HOT_O_MASS
              DD = DD + context.MT(m)*D(CIRA.DensityIndex(d,m));
            end
          end
          D(di) = CIRA.INVAVOG*DD;
          if context.IMR == 1
            D(di)=D(di)*CIRA.KGPERGM; % g -> kg
          end
        end
      end
    else
      ZH = zeros(1,CIRA.DerLast);
      ZH(1,CIRA.Der0) = ALT;
      ZH(1,CIRA.DerAlt) = 1.0;
      [DZ,TZ]=context.DENSM(ZH,1.,0.0, ...
                 context.MN3,context.ZN3,context.TN3,context.TGN3,...
                 context.MN2,context.ZN2,context.TN2,context.TGN2);
      [~,T] = context.SetOutput(D,T,DZ,TZ,CIRA.ALL_MASS);
    end
  end
  context.GTD7_ALAST=ALT;

end
function [TN,TGN] = SplineCoefficients( context, MN1, TN1, TGN1, MN, PMA, PAVGM, ...
  SW, SW3, J, I1, I2 )
  %
  %       LOWER MESOSPHERE/UPPER STRATOSPHERE [between ZN3(1) and ZN2(1)]
  %         Temperature at nodes and gradients at end nodes
  %         Inverse temperature a linear function of spherical harmonics
  %         Only calculate nodes if input changed
  TN = NodeAccelerations( context, MN, MN1, TN1, PMA, PAVGM, SW, SW3, I1 );
  TGN = EndPointAccelerations( context, MN, TN, TGN1, PMA, PAVGM, SW*SW3, J, I1, I2 );
end
function TN = NodeAccelerations( context, MN, MN1, TN1, PMA, PAVGM, SW, SW3, I1 )
  TN = zeros(MN,CIRA.DerLast);
  if length(TN1) > 1
    for d=CIRA.Der0:CIRA.DerLast
      TN(1,d) = TN1(MN1,d);
    end
  else
    TN(1,CIRA.Der0) = TN1;
  end
  for I=2:MN
    if I == MN
      SW = SW * SW3;
    end
    TL = context.GLOB7S(PMA,I+I1);
    a = PMA(1,I+I1)*PAVGM(I+I1);
    TN(I,CIRA.Der0) = a/(1.-SW*TL(1,CIRA.Der0));
    for d=CIRA.Der0+1:CIRA.DerLast
      TN(I,d) = -TN(I,CIRA.Der0)*(-SW*TL(1,d))/(1.-SW*TL(1,CIRA.Der0));
    end
  end
end
function TGN = EndPointAccelerations( context, MN, TN, TGN1, PMA, PAVGM, SW, I, I1, I2 )
  TGN = zeros(2,CIRA.DerLast);
  if length(TGN1) > 1
    for d=CIRA.Der0:CIRA.DerLast
      TGN(1,d) = TGN1(2,d);
    end
  else
    TGN(1,CIRA.Der0) = TGN1;
  end
  TL = context.GLOB7S(PMA,I+I1);
  T2 = TN(MN,CIRA.Der0)*TN(MN,CIRA.Der0);
  f = PMA(1,I+I1)*PAVGM(I+I2)/(PMA(1,MN+I1)*PAVGM(MN+I1))^2;
  TGN(2,CIRA.Der0)=(1.+SW*TL(1,CIRA.Der0))*T2*f;
  for d=CIRA.Der0+1:CIRA.DerLast
    dT2 = 2*TN(MN,d)*TN(MN,CIRA.Der0);
    TGN(2,d)=SW*TL(1,d)*T2*f+(1.+SW*TL(1,CIRA.Der0))*dT2*f;
  end
end
