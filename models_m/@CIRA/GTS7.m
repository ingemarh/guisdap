function [ D,T,context ] = GTS7( context, IYD,SEC,GALT,GLAT,GLONG,STL, ...
  F107A,F107,AP,MASS,D1 )
%GTS7 Thermospheric portion of NRLMSISE-00
%      SUBROUTINE GTS7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
%-----------------------------------------------------------------------
%
%     Thermospheric portion of NRLMSISE-00
%     See GTD7 for more extensive comments
%
%        OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.)
% 
%     INPUT VARIABLES:
%        IYD - YEAR AND DAY AS YYDDD (day of year from 1 to 365 (or 366))
%              (Year ignored in current model)
%        SEC - UT(SEC)
%        ALT - ALTITUDE(KM) (>72.5 km)
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
%        D(6) - TOTAL MASS DENSITY(GM/CM^3) [Anomalous O NOT included]
%        D(7) - H NUMBER DENSITY(CM^-3)
%        D(8) - N NUMBER DENSITY(CM^-3)
%        D(9) - Anomalous oxygen NUMBER DENSITY(CM^-3)
%        D(10) - HE NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(11) - O NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(12) - N2 NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(13) - O2 NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(14) - AR NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(15) - TOTAL MASS DENSITY GRADIENT (CM^-3/KM) [Anomalous O NOT incl.]
%        D(16) - H NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(17) - N NUMBER DENSITY GRADIENT (CM^-3/KM)
%        D(18) - Anomalous oxygen NUMBER DENSITY GRADIENT (CM^-3/KM)
%        T(1) - EXOSPHERIC TEMPERATURE (K)
%        T(2) - TEMPERATURE AT ALT (K)
%        T(3) - TEMPERATURE GRADIENT AT ALT (K/km)
%-----------------------------------------------------------------------

%      DIMENSION context.ZN1(5),context.ALPHA(9),APLOW(7)
%      COMMON/GTS3C/TLB,S,DB04,DB16,DB28,DB32,DB40,DB48,DB01,ZA,T0,Z0
%     & ,G0,RL,DD,DB14,TR12
%      COMMON/MESO7/TN1(5),TN2(4),TN3(5),TGN1(2),TGN2(2),TGN3(2)
%c      DIMENSION D(9),T(2),MT(11),AP(1),context.ALTL(8)
%      DIMENSION D(9),T(2),MT(11),AP(7),context.ALTL(8)
%      COMMON/LOWER7/PTM(10),PDM(10,8)
%c      COMMON/PARM7/PT(150),PD(150,9),PS(150),PDL(25,2),PTL(100,4),
%c     $ PMA(100,10),SAM(100)
%      COMMON/PARM7/PT(150),PDA1(150),PDA2(150),PDA3(150),PDA4(150),
%     $ PDA5(150),PDA6(150),PDA7(150),PDA8(150),PDA9(150),
%     $ PS(150),PDL(25,2),PTL(100,4),PMA(100,10),SAM(100)
%      COMMON/CSW/SW(25),ISW,SWC(25)
%      COMMON/TTEST/TINFG,GB,ROUT,TT(15)
%      COMMON/DMIX/DM04,DM16,DM28,DM32,DM40,DM01,DM14
%      COMMON/METSEL/IMR
%      SAVE
  if D1 < 0
    TNMOD=-D1;   %..  PGR
  else
    TNMOD=0;   %.. for switching on mod MSIS
  end
  if context.GSURF(1,CIRA.Der0) == 0
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
  end
  %        Test for changed input
  [V2,context]=context.VTST7(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,2);
  
  YRD=double(IYD);
  ZA=context.PDL(16,2);
  context.ZN1(1)=ZA;
  D = zeros(CIRA.numDensities,1);
  T = zeros(CIRA.numTemperatures,1);
  if length(GALT) > 1
    ALT = GALT;
  else
    ALT = zeros(1,CIRA.DerLast);
    ALT(1,CIRA.Der0) = GALT;
  end

  if TNMOD > context.TINFLim
    context.GTS7_TINF = zeros(1,CIRA.DerLast);
    context.GTS7_TINF(1,CIRA.Der0)=TNMOD;      %.. PGR
  else
    % VARIATIONS NOT IMPORTANT BELOW ZA OR context.ZN1(1)
    if ALT(1,CIRA.Der0) > context.ZN1(1)
      if V2 == 1. || context.GTS7_ALAST <= context.ZN1(1)
        [TINF,context] = Variation( context,YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP, ...
          context.PT,1, context.PTM(1), context.SW(CIRA.ALL_TINF_SW) );
        context.GTS7_TINF = TINF;
      end
    else
      TINF = zeros(1,CIRA.DerLast);
      TINF(1,CIRA.Der0)=context.PTM(1)*context.PT(1,1);
      context.GTS7_TINF = TINF;
    end
  end

  T(CIRA.EXOSPHERIC_TEMP)=context.GTS7_TINF(1,CIRA.Der0);
  %T(CIRA.EXOSPHERIC_LON_GRAD_TEMP)=context.GTS7_TINF(1,CIRA.DerLon);
  %T(CIRA.EXOSPHERIC_LAT_GRAD_TEMP)=context.GTS7_TINF(1,CIRA.DerLat);
  %          GRADIENT VARIATIONS NOT IMPORTANT BELOW context.ZN1(5)
  if ALT(1,CIRA.Der0) > context.ZN1(context.MN1)
    if V2 == 1. || context.GTS7_ALAST <= context.ZN1(context.MN1)
      [G0,context] = Variation( context,YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP, ...
        context.PS,1, context.PTM(4), context.SW(CIRA.ALL_S_SW) );
      context.GTS7_G0 = G0;
    end
  else
    G0 = zeros(1,CIRA.DerLast);
    G0(1,CIRA.Der0)=context.PTM(4)*context.PS(1,1);
    context.GTS7_G0 = G0;
  end
  %      Calculate these temperatures only if input changed
  if V2 == 1. || ALT(1,CIRA.Der0) < context.LTHHT
    [TLB,context] = Variation( context,YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP, ...
      context.PDA,CIRA.ALL_MASS, context.PTM(2), context.SW(CIRA.ALL_TLB_INDEP_SW) );
    context.GTS7_TLB = TLB;
  end
  a = context.GTS7_TINF(1,CIRA.Der0)-context.GTS7_TLB(1,CIRA.Der0);
  context.S(1,CIRA.Der0)=context.GTS7_G0(1,CIRA.Der0)/a;
  for d=CIRA.Der0+1:CIRA.DerLast
    da = context.GTS7_TINF(1,d)-context.GTS7_TLB(1,d);
    context.S(1,d)=context.GTS7_G0(1,d)/a-context.S(1,CIRA.Der0)*da/a;
  end
  %       Lower thermosphere temp variations not significant for
  %        density above 300 km
  [context.TN1,context.TGN1] = SplineCoefficients1( context, ALT(1,CIRA.Der0), context.LTHHT, ...
    context.MN1, context.PMA, context.PTL, context.PTM, V2 );

  %context.Z0=context.ZN1(4);
  %context.T0=context.TN1(4);
  %context.TR12=1.;

  if MASS ~= 0.0
    %       N2 variation factor at Zlb
    XMM=context.PDM(5,context.mainConstituent);
    Z=ALT;
    found = false;
    for J = 1:CIRA.numMasses
      if MASS == context.MT(J)
        found = true;
        break;
      end
    end
    if found
      departEquilibrium = context.SW(CIRA.EQUILIB_DEPART_SW) ~= 0.;
      if Z(1,CIRA.Der0) <= context.ALTL(CIRA.ALL_MASS) || ... % in lower atm
         J == context.mainConstituent || ... % main constituent
         J == CIRA.ALL_MASS % calculate all constituents
        %
        %       **** N2 DENSITY ****
        %
        %      Diffusive density at Zlb
        massI = context.mainConstituent;
        [DZ,~,DB,context] = BaseDensity(context,YRD,SEC,Z,GLAT,GLONG,STL,F107A,F107,AP,massI);
        %DD=D(CIRA.DensityIndex(CIRA.Der0,massI));
        %      Turbopause
        DAY=mod(YRD,CIRA.YRDSHIFT) + double(SEC)/86400.0;
        DAYdSec=1.0/86400.0;
        %        VARIATION OF TURBOPAUSE HEIGHT
        a = context.SW(CIRA.ASYM_ANNUAL_SW)*context.PDL(25,1) ...
            *cos(context.DR*(DAY-context.PT(14)));
        adSec = -context.SW(CIRA.ASYM_ANNUAL_SW)*context.PDL(25,1) ...
            *sin(context.DR*(DAY-context.PT(14)))*context.DR*DAYdSec;
        sl = sin(CIRA.DGTR*double(GLAT));
        ZHF=context.PDL(25,2)*(1.+a*sl);
        ZHFdLat=context.PDL(25,2)*a*cos(CIRA.DGTR*double(GLAT))*CIRA.DGTR;
        ZHFdSec=context.PDL(25,2)*adSec*sl;
        ZH = zeros(1,CIRA.DerLast);
        ZH(1,CIRA.Der0)=context.PDM(3,massI)*ZHF;
        ZH(1,CIRA.DerLat)=context.PDM(3,massI)*ZHFdLat;
        ZH(1,CIRA.DerSec)=context.PDM(3,massI)*ZHFdSec;
        ZHM28=context.PDM(4,massI)*context.PDL(6,2);
        [DZ28,TZ,B28] = TurbopauseCorrection(context,Z,ZH,DZ,DB,XMM, ...
          context.ALPHA(massI),ZHM28,massI);
        if Z(1,CIRA.Der0) <= context.ALTL(massI) && departEquilibrium
          DZ = DZ28;
          %DZ = GroundMixingCorrection(context,Z,DZ,B28,B28,0,0,0,massI);
          %DZ = ChemistryCorrection(context,Z,DZ,0,massI);
          %DZ = EquilibriumDepartureCorrection(context,Z,DZ,F107A,0,0,0,0,0,massI);
        end
        [D,T] = context.SetOutput(D,T,DZ,TZ,massI);
      end
      for massI = CIRA.HE_MASS:CIRA.N_MASS
        if massI == context.mainConstituent || massI == CIRA.HOT_O_MASS
          continue;
        elseif J == CIRA.ALL_MASS || ...
               J == massI || ...
               (J == CIRA.HOT_O_MASS && (massI == CIRA.O_MASS || massI == CIRA.O2_MASS))
          [DZ,TZ,DB,context] = BaseDensity(context,YRD,SEC,Z,GLAT,GLONG,STL,F107A,F107,AP,massI);
           %DD=D(CIRA.DensityIndex(CIRA.Der0,massI));
          %DDHO=D(CIRA.DensityIndex(CIRA.Der0,CIRA.O_MASS))+2.*D(CIRA.DensityIndex(CIRA.Der0,CIRA.O2_MASS));
          if departEquilibrium
            if Z(1,CIRA.Der0) <= context.ALTL(massI)
              [DZ,TZ,B,context] = TurbopauseCorrection(context,Z,context.PDM(3,massI), ...
                DZ,DB,XMM,0.0,ZHM28,massI);
              DZ = GroundMixingCorrection(context,Z,DZ,B,B28,massI);
              DZ = ChemistryCorrection(context,Z,DZ,massI);
            end
            DZ = EquilibriumDepartureCorrection(context,Z,DZ,F107A,massI);
          end
          [D,T] = context.SetOutput(D,T,DZ,TZ,massI);
        end
      end
      if J == CIRA.ALL_MASS || J == CIRA.ANOM_O_MASS
        %
        % **** Anomalous OXYGEN DENSITY ****
        %
        massI = CIRA.ANOM_O_MASS;
        [DD,TZ,THO,context] = BaseDensity(context,YRD,SEC,Z,GLAT,GLONG,STL,F107A,F107,AP,massI);
        [DD] = AnomalousOCorrection(context,Z,DD,THO,massI);
        D = context.SetOutput(D,T,DD,TZ,massI);
      end
      if J == CIRA.ALL_MASS
        %
        %       TOTAL MASS DENSITY
        %
        for d=CIRA.Der0:CIRA.DerLast
          if d == CIRA.DerSTL
            continue;
          end
          DT = 0.0;
          for m = CIRA.HE_MASS:CIRA.N_MASS
            if m ~= CIRA.HOT_O_MASS
              DT = DT + context.MT(m)*D(CIRA.DensityIndex(d,m));
            end
          end
          D(CIRA.DensityIndex(d,CIRA.ALL_MASS)) = CIRA.INVAVOG*DT;
        end
%         DB48=CIRA.INVAVOG*(context.MT(CIRA.HE_MASS)*DB04 ...
%                                   +context.MT(CIRA.O_MASS)*DB16 ...
%                                   +context.MT(CIRA.N2_MASS)*context.DB28 ...
%                                   +context.MT(CIRA.O2_MASS)*DB32 ...
%                                   +context.MT(CIRA.AR_MASS)*DB40 ...
%                                   +context.MT(CIRA.H_MASS)*DB01 ...
%                                   +context.MT(CIRA.N_MASS)*DB14);
      end
    elseif context.KONSOL > 0
      fprintf(context.KONSOL,'MASS %f NOT VALID\n',MASS);
    end
  else
    %       TEMPERATURE AT ALTITUDE
    [DZ,TZ,context.TN1,context.TGN1]  = context.DENSU(ALT,1.,context.GTS7_TINF,context.GTS7_TLB,...
      0.0,0.,context.PTM(6),context.S,context.MN1,context.ZN1,context.TN1,context.TGN1);
    [~,T] = context.SetOutput(D,T,DZ,TZ,CIRA.ALL_MASS);
  end
  %       ADJUST DENSITIES FROM CGS TO KGM
  if context.IMR == 1
    for I=1:CIRA.numDensities
      D(I)=D(I)*CIRA.CM3PERM3; % cm^-3 -> m^-3 and g/cm^3 -> g/m^3
    end
    mt = CIRA.ALL_MASS;
    for d=CIRA.Der0:CIRA.DerLast
      if d == CIRA.DerSTL
        continue;
      end
      D(CIRA.DensityIndex(d,mt))=D(CIRA.DensityIndex(d,mt))*CIRA.KGPERGM; % g/m^3 -> kg/m^3
    end
  end
  context.GTS7_ALAST=ALT(1,CIRA.Der0);

end
function [DZ,TZ,DB,context] = BaseDensity(context,YRD,SEC,Z,GLAT,GLONG,STL,F107A,F107,AP,massI)
  [TL,context] = context.GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,...
    context.PDA,massI);
  % Density variation factor at Zlb
  G = context.SW(CIRA.ALL_NLB_SW)*TL(1,CIRA.Der0);
  % Diffusive density at Zlb
  DB = zeros(1,CIRA.DerLast);
  DB(1,CIRA.Der0) = context.PDM(1,massI)*exp(G)*context.PDA(1,massI);
  DBdTL = DB(1,CIRA.Der0)*context.SW(CIRA.ALL_NLB_SW);
  for d=CIRA.Der0+1:CIRA.DerLast
    DB(1,d) = DBdTL*TL(1,d);
  end
  % Diffusive density at Alt
  if massI ~= CIRA.ANOM_O_MASS
    [DZ,TZ,context.TN1,context.TGN1]=context.DENSU(Z,DB,context.GTS7_TINF,context.GTS7_TLB,...
                 context.MT(massI),context.ALPHA(massI),context.PTM(6),context.S, ...
                 context.MN1,context.ZN1,context.TN1,context.TGN1);
  else
    THO=context.PDM(10,massI)*context.PDL(7,1);
    [DZ,TZ,context.TN1,context.TGN1]=context.DENSU(Z,DB,THO,THO,...
      context.MT(CIRA.O_MASS),context.ALPHA(massI),context.PTM(6),context.S, ...
      context.MN1,context.ZN1,context.TN1,context.TGN1);
    DB = THO; % return THO instead
  end
end
function [DD] = AnomalousOCorrection(context,Z,DD,THO,massI)
  ZSHTIndex = 6;
  ZMHOIndex = 5;
  ZSHT=context.PDM(ZSHTIndex,massI);
  ZMHO=context.PDM(ZMHOIndex,massI);
  ZSHO = zeros(1,CIRA.DerLast);
  [ZSHO(1,CIRA.Der0),~,ZSHOdR,ZSHOdGS]=context.SCALH(ZMHO,context.RE(1,CIRA.Der0),...
    context.GSURF(1,CIRA.Der0),context.MT(CIRA.O_MASS),THO);
  for d=CIRA.Der0+1:CIRA.DerLast
    ZSHO(1,d) = ZSHOdR*context.RE(1,d)+ZSHOdGS*context.GSURF(1,d);
  end
  EX = zeros(1,CIRA.DerLast);
  EX1 = zeros(1,CIRA.DerLast);
  EX(1,CIRA.Der0) = exp(-(Z(1,CIRA.Der0)-ZMHO)/ZSHT);
  EX1(1,CIRA.Der0) = exp(-ZSHT/ZSHO(1,CIRA.Der0)*(EX(1,CIRA.Der0)-1.));
  oldDD = DD(1,CIRA.Der0);
  DD(1,CIRA.Der0)=oldDD*EX1(1,CIRA.Der0);
  for d=CIRA.Der0+1:CIRA.DerLast
    EX(1,d) = EX(1,CIRA.Der0)*(-Z(1,d)/ZSHT);
    EX1(1,d) = EX1(1,CIRA.Der0)*(-ZSHT/ZSHO(1,CIRA.Der0)*EX(1,d) ...
      +(ZSHT*ZSHO(1,d)/ZSHO(1,CIRA.Der0)/ZSHO(1,CIRA.Der0)*(EX(1,CIRA.Der0)-1.)));
    DD(1,d)=DD(1,d)*EX1(1,CIRA.Der0) + oldDD*EX1(1,d);
  end
end
function [D] = ChemistryCorrection(context,Z,D,massI)
  i = context.DISASSOCIATED(massI);
  if i <= 0
    return;
  end
  RCIndex = 4;
  ZCIndex = 7;
  HCIndex = 8;
  % Chemistry correction
  RC =context.PDM(RCIndex,massI)*context.PDL(i+2,2);
  HCC=context.PDM(HCIndex,massI)*context.PDL(i+1,2);
  ZCC=context.PDM(ZCIndex,massI)*context.PDL(i+0,2);
  % Net density corrected at Alt
  [CCOR,CCORdZ] = CIRA.CCOR(Z(1,CIRA.Der0),RC,HCC,ZCC);
  oldD = D(1,CIRA.Der0);
  D(1,CIRA.Der0)=oldD*CCOR;
  for d=CIRA.Der0+1:CIRA.DerLast
    D(1,d)=D(1,d)*CCOR + oldD*CCORdZ*Z(1,d);
  end
end
function [DZ,TZ,B,context] = TurbopauseCorrection(context,Z,ZH,DZ,DB,XMM,A,ZHM28,massI)
  % Turbopause
  
  %      Mixed density at Zlb
  [B,~,context.TN1,context.TGN1] = context.DENSU(ZH,DB,context.GTS7_TINF,context.GTS7_TLB,...
     context.MT(massI)-XMM,context.ALPHA(massI)-1.,context.PTM(6),context.S, ...
     context.MN1,context.ZN1,context.TN1,context.TGN1);
  %      Mixed density at Alt
  [DM,TZ,context.TN1,context.TGN1]=context.DENSU(Z,B,context.GTS7_TINF,context.GTS7_TLB,...
                        XMM,A,context.PTM(6),context.S,context.MN1,context.ZN1, ...
                        context.TN1,context.TGN1);
  if massI == context.mainConstituent
    context.DM28 = DM;
  end
  DZ = NetDensityCorrection(context,DZ,DM,ZHM28,XMM,massI);
end
function [D] = NetDensityCorrection(context,D,DM,ZHM28,XMM,massI)
  % Net density at Alt
  [DD,DDdD,DDdDM]=CIRA.DNET(D(1,CIRA.Der0),DM(1,CIRA.Der0),ZHM28,XMM,context.MT(massI));
  D(1,CIRA.Der0) = DD;
  for d=CIRA.Der0+1:CIRA.DerLast
    D(1,d) = D(1,d)*DDdD + DM(1,d)*DDdDM;
  end
end
function [D] = GroundMixingCorrection(context,Z,D,B,B28,massI)
  % Correction to specified mixing ratio at ground
  i = context.MIXINDEX(1,massI);
  j = context.MIXINDEX(2,massI);
  k = context.MIXINDEX(3,massI);
  if i <= 0
    return;
  end
  ZCIndex = 5;
  HCIndex = 6;
  RLIndex = 2;
  RL = zeros(1,CIRA.DerLast);
  RL(1,CIRA.Der0)=log(B28(1,CIRA.Der0)*context.PDM(RLIndex,massI) ...
    *abs(context.PDL(j,k))/B(1,CIRA.Der0));
  for d=CIRA.Der0+1:CIRA.DerLast
    RL(1,d)=B28(1,d)/B28(1,CIRA.Der0)-B(1,d)/B(1,CIRA.Der0);
  end
  HC=context.PDM(HCIndex,massI)*context.PDL(i+1,k);
  ZC=context.PDM(ZCIndex,massI)*context.PDL(i,k);
  [CCOR,CCORdZ,CCORdR] = CIRA.CCOR(Z(1,CIRA.Der0),RL(1,CIRA.Der0),HC,ZC);
  oldD = D(1,CIRA.Der0);
  D(1,CIRA.Der0)=oldD*CCOR;
  for d=CIRA.Der0+1:CIRA.DerLast
    D(1,d)=D(1,d)*CCOR ...
      + oldD*CCORdZ*Z(1,d) + oldD*CCORdR*RL(1,d);
  end
end
function [D] = EquilibriumDepartureCorrection(context,Z,D,F107A,massI)
  % Correction for general departure from diffusive equilibrium above Zlb
  i = context.EQUILIBINDEX(1,massI);
  j = context.EQUILIBINDEX(2,massI);
  k = context.EQUILIBINDEX(3,massI);
  l = context.EQUILIBINDEX(4,massI);
  m = context.EQUILIBINDEX(5,massI);
  n = 24;
  if i <= 0
    return;
  end
  RC=context.PDM(i,massI)*context.PDL(m,2)*(1.+context.SW(CIRA.F107_SW)*...
                          context.PDL(n,1)*(F107A-context.defaultF107));
  HCC=context.PDM(i+4,massI)*context.PDL(j+1,2);
  ZCC=context.PDM(i+3,massI)*context.PDL(j,2);
  HCC2=context.PDM(i+4,massI)*context.PDL(k,l);
  % Net density corrected at Alt
  [CCOR2,CCOR2dZ] = CIRA.CCOR2(Z(1,CIRA.Der0),RC,HCC,ZCC,HCC2);
  oldD = D(1,CIRA.Der0);
  D(1,CIRA.Der0)=oldD*CCOR2;
  for d=CIRA.Der0+1:CIRA.DerLast
    D(1,d)=D(1,d)*CCOR2 + oldD*CCOR2dZ*Z(1,d);
  end
end
function [TN,TGN] = SplineCoefficients1( context, ALT, ZN, MN, PMA, PTL, PTM, V2 )
  SW = context.SW(CIRA.ALL_TN1_SW);
  SW2 = context.SW(CIRA.ALL_TN2_SW);
  I1 = -1;
  J = 10;
  if ALT < ZN
    if V2 == 1. || context.GTS7_ALAST >= ZN
      TN = zeros(MN,CIRA.DerLast);
      TGN = zeros(2,CIRA.DerLast);
      % TN1(1) is filled out later in DENSU
      for I=2:MN
        if I==MN
          SW = SW * SW2;
        end
        TL = context.GLOB7S(PTL,I+I1);
        a = PTM(context.PTMINDEX(I))*PTL(1,I+I1);
        TN(I,CIRA.Der0) = a/(1.-SW*TL(1,CIRA.Der0));
        for d=CIRA.Der0+1:CIRA.DerLast
          TN(I,d)=TN(I,CIRA.Der0)*SW*TL(1,d)/(1.-SW*TL(1,CIRA.Der0));
        end
      end
      I = J;
      TL = context.GLOB7S(PMA,I+I1);
      f = PTM(I+I1)*PMA(1,I+I1)/(PTM(MN)*PTL(1,MN+I1))^2;
      TGN(2,CIRA.Der0)=(1.+SW*TL(1,CIRA.Der0))*TN(MN,CIRA.Der0)*TN(MN,CIRA.Der0)*f;
      for d=CIRA.Der0+1:CIRA.DerLast
        TGN(2,d)=(SW*TL(1,d)*TN(MN,CIRA.Der0) ...
           +(1.+SW*TL(1,CIRA.Der0))*2.0*TN(MN,d))*TN(MN,CIRA.Der0)*f;
      end
    else
      TN = context.TN1;
      TGN = context.TGN1;
    end
  else
    TN = zeros(MN,CIRA.DerLast);
    TGN = zeros(2,CIRA.DerLast);
    for I=2:MN
      TN(I,CIRA.Der0)=PTM(context.PTMINDEX(I))*PTL(1,I+I1);
      for d=CIRA.Der0+1:CIRA.DerLast
        TN(I,d)=0.0;
      end
    end
    I = J;
    a = PTM(I+I1)*PMA(1,I+I1)/(PTM(MN)*PTL(1,MN+I1))^2;
    TGN(2,CIRA.Der0)=TN(MN,CIRA.Der0)*TN(MN,CIRA.Der0)*a;
    for d=CIRA.Der0+1:CIRA.DerLast
      TGN(2,d)=2.0*TN(MN,CIRA.Der0)*TN(MN,d)*a;
    end
  end
end
function [v,context] = Variation( context,YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP, ...
  PS, m, PTM, SW )
  v = zeros(1,CIRA.DerLast);
  [TL,context] = context.GLOBE7(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP, ...
    PS,m);
  a = PTM*PS(1,m);
  v(1,CIRA.Der0) = a*(1.+SW*TL(1,CIRA.Der0));
  for d=CIRA.Der0+1:CIRA.DerLast
    v(1,d)=a*SW*TL(1,d);
  end
end
