classdef CIRA < matlab.unittest.TestCase
%CIRA Class for running the COSPAR International Reference Atmosphere (CIRA) MSIS-E 2000 Atmosphere model
% Original from the Naval Research Lab MSIS-E 2000 Atmosphere model
% This is a port of the IRI-2012 fortran.
% Copyright(c) 2014 by Jonathan Kipling Knight (<a href="matlab:
% web('mailto:drkipknight@aol.com')">drkipknight@aol.com</a>)
% DEPENDENCY: this class is dependent on CODATA2006 and OEIS classes
% For more information, see the <a href="matlab:
% web('http://www.nrl.navy.mil/research/nrl-review/2003/atmospheric-science/picone/')">MSISE00 Online</a>.
% Also, see the <a href="matlab:
% web('http://spaceweather.usu.edu/files/uploads/PDF/COSPAR_INTERNATIONAL_REFERENCE_ATMOSPHERE-CHAPTER-1_3(rev-01-11-08-2012).pdf')">CIRA specification</a>.
% See also CODATA2006, OEIS and <a href="matlab:web(fullfile(fileparts(which('CIRA.m')),'doc','Example.html'))">example</a>
  properties (Constant)
    Der0 = 1; % column index for zeroth derivative (that value itself)
    DerAlt = 2; % column index for derivative w.r.t. altitude
    DerLon = 3; % column index for derivative w.r.t. longitude
    DerLat = 4; % column index for derivative w.r.t. latitude
    DerSec = 5; % column index for derivative w.r.t. seconds in the day
    DerSTL = 6; % column index for derivative w.r.t. seconds in the day
    DerLast = CIRA.DerSTL; % column index for last derivative
  end
  properties (Constant)
    HE_DENS      = 1; % index into D for helium (He) number density (cm^-3 or m^-3)
    O_DENS       = 2; % index into D for elemental oxygen (O) number density (cm^-3 or m^-3)
    N2_DENS      = 3; % index into D for molecular nitrogen (N2) number density (cm^-3 or m^-3)
    O2_DENS      = 4; % index into D for molecular oxygen (O2) number density (cm^-3 or m^-3)
    AR_DENS      = 5; % index into D for argon (Ar) number density (cm^-3 or m^-3)
    TOTAL_DENS   = 6; % index into D for total mass density (g/cm^3 or kg/m^3)
    H_DENS       = 7; % index into D for hydrogen (H) number density (cm^-3 or m^-3)
    N_DENS       = 8; % index into D for elemental nitrogen (N) number density (cm^-3 or m^-3)
    ANOM_O_DENS  = 9; % index into D for anomalous oxygen (O) number density (cm^-3 or m^-3)
    HE_GDENS     = 10; % index into D for helium (He) number density gradient (cm^-3/km or m^-3/km)
    O_GDENS      = 11; % index into D for elemental oxygen (O) number density gradient (cm^-3/km or m^-3/km)
    N2_GDENS     = 12; % index into D for molecular nitrogen (N2) number density gradient (cm^-3/km or m^-3/km)
    O2_GDENS     = 13; % index into D for molecular oxygen (O2) number density gradient (cm^-3/km or m^-3/km)
    AR_GDENS     = 14; % index into D for argon (Ar) number density gradient (cm^-3/km or m^-3/km)
    TOTAL_GDENS  = 15; % index into D for total mass density gradient (g/cm^3/km or kg/m^3/km)
    H_GDENS      = 16; % index into D for hydrogen (H) number density gradient (cm^-3/km or m^-3/km)
    N_GDENS      = 17; % index into D for elemental nitrogen (N) number density gradient (cm^-3/km or m^-3/km)
    ANOM_O_GDENS = 18; % index into D for anomalous oxygen (O) number density gradient (cm^-3/km or m^-3/km)
    HE_LONGDENS  = 19; % index into D for helium (He) number density longitude gradient (cm^-3/deg or m^-3/deg)
    O_LONGDENS      = 20; % index into D for elemental oxygen (O) number density longitude gradient (cm^-3/deg or m^-3/deg)
    N2_LONGDENS     = 21; % index into D for molecular nitrogen (N2) number density longitude gradient (cm^-3/deg or m^-3/deg)
    O2_LONGDENS     = 22; % index into D for molecular oxygen (O2) number density longitude gradient (cm^-3/deg or m^-3/deg)
    AR_LONGDENS     = 23; % index into D for argon (Ar) number density longitude gradient (cm^-3/deg or m^-3/deg)
    TOTAL_LONGDENS  = 24; % index into D for total mass density longitude gradient (g/cm^3/deg or kg/m^3/deg)
    H_LONGDENS      = 25; % index into D for hydrogen (H) number density longitude gradient (cm^-3/deg or m^-3/deg)
    N_LONGDENS      = 26; % index into D for elemental nitrogen (N) number longitude density gradient (cm^-3/deg or m^-3/deg)
    ANOM_O_LONGDENS = 27; % index into D for anomalous oxygen (O) number density longitude gradient (cm^-3/deg or m^-3/deg)
    HE_LATGDENS     = 28; % index into D for helium (He) number density latitude gradient (cm^-3/deg or m^-3/deg)
    O_LATGDENS      = 29; % index into D for elemental oxygen (O) number latitude density gradient (cm^-3/deg or m^-3/deg)
    N2_LATGDENS     = 30; % index into D for molecular nitrogen (N2) number latitude density gradient (cm^-3/deg or m^-3/deg)
    O2_LATGDENS     = 31; % index into D for molecular oxygen (O2) number latitude density gradient (cm^-3/deg or m^-3/deg)
    AR_LATGDENS     = 32; % index into D for argon (Ar) number density latitude gradient (cm^-3/deg or m^-3/deg)
    TOTAL_LATGDENS  = 33; % index into D for total mass density latitude gradient (g/cm^3/deg or kg/m^3/deg)
    H_LATGDENS      = 34; % index into D for hydrogen (H) number density latitude gradient (cm^-3/deg or m^-3/deg)
    N_LATGDENS      = 35; % index into D for elemental nitrogen (N) number density latitude gradient (cm^-3/deg or m^-3/deg)
    ANOM_O_LATGDENS = 36; % index into D for anomalous oxygen (O) number density latitude gradient (cm^-3/deg or m^-3/deg)
    HE_TIMEGDENS    = 37; % index into D for helium (He) number density time gradient (cm^-3/s or m^-3/s)
    O_TIMEGDENS      = 38; % index into D for elemental oxygen (O) number time density gradient (cm^-3/deg or m^-3/s)
    N2_TIMEGDENS     = 39; % index into D for molecular nitrogen (N2) number time density gradient (cm^-3/s or m^-3/s)
    O2_TIMEGDENS     = 40; % index into D for molecular oxygen (O2) number time density gradient (cm^-3/s or m^-3/s)
    AR_TIMEGDENS     = 41; % index into D for argon (Ar) number density time gradient (cm^-3/s or m^-3/s)
    TOTAL_TIMEGDENS  = 42; % index into D for total mass density time gradient (g/cm^3/s or kg/m^3/s)
    H_TIMEGDENS      = 43; % index into D for hydrogen (H) number density time gradient (cm^-3/s or m^-3/s)
    N_TIMEGDENS      = 44; % index into D for elemental nitrogen (N) number density time gradient (cm^-3/s or m^-3/s)
    ANOM_O_TIMEGDENS = 45; % index into D for anomalous oxygen (O) number density time gradient (cm^-3/s or m^-3/s)
    numDensities = CIRA.ANOM_O_TIMEGDENS-CIRA.HE_DENS+1; % number of densities calculated
  end
  properties (Constant)
    EXOSPHERIC_TEMP = 1; % index into T for exospheric temperature (K)
    ALTITUDE_TEMP   = 2; % index into T for temperature at altitude (K)
    GRADIENT_TEMP   = 3; % (deprecated) index into T for temperature altitude gradient at altitude (K/km)
    LON_GRADIENT_TEMP = 4; % index into T for temperature longitude gradient at altitude (K/deg)
    LAT_GRADIENT_TEMP = 5; % index into T for temperature latitude gradient at altitude (K/deg)
    TIME_GRADIENT_TEMP = 6; % index into T for temperature latitude gradient at altitude (K/deg)
    numTemperatures = CIRA.TIME_GRADIENT_TEMP-CIRA.EXOSPHERIC_TEMP+1; % number of temperatures calculated
  end
  properties (Constant)
    DAILY_AP           = 1; % DAILY AP
    CURRENT_AP         = 2; % 3 HR AP INDEX FOR CURRENT TIME
    CURRENT_M_3_AP     = 3; % 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
    CURRENT_M_6_AP     = 4; % 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
    CURRENT_M_9_AP     = 5; % 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
    CURRENT_M_12_33_AP = 6; % AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR TO CURRENT TIME
    CURRENT_M_36_57_AP = 7; % AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR TO CURRENT TIME
    maxAP = CIRA.CURRENT_M_36_57_AP-CIRA.DAILY_AP+1; % maximum number of Ap indices in array input
  end
  properties (Constant)
    ALL_MASS    = 1; % index into MT indicating calculate all constituent densities
    HE_MASS     = 2; % index into MT indicating calculate helium density
    O_MASS      = 3; % index into MT indicating calculate elemental oxygen density
    N2_MASS     = 4; % index into MT indicating calculate nitrogen molecule density
    O2_MASS     = 5; % index into MT indicating calculate oxygen molecule density
    AR_MASS     = 6; % index into MT indicating calculate argon density
    H_MASS      = 7; % index into MT indicating calculate elemental hydrogen density
    HOT_O_MASS  = 8; % index into MT indicating calculate hot elemental oxygen density
    N_MASS      = 9; % index into MT indicating calculate elemental nitrogen density
    ANOM_O_MASS = 10; % index into MT indicating calculate anomalous oxygen density
    % lookup table for densities and their derivatives
    DensityIndex = [CIRA.TOTAL_DENS,CIRA.HE_DENS,CIRA.O_DENS,CIRA.N2_DENS,CIRA.O2_DENS,CIRA.AR_DENS,CIRA.H_DENS,CIRA.O_DENS,CIRA.N_DENS,CIRA.ANOM_O_DENS; ...
                    CIRA.TOTAL_GDENS,CIRA.HE_GDENS,CIRA.O_GDENS,CIRA.N2_GDENS,CIRA.O2_GDENS,CIRA.AR_GDENS,CIRA.H_GDENS,CIRA.O_GDENS,CIRA.N_GDENS,CIRA.ANOM_O_GDENS; ...
                    CIRA.TOTAL_LONGDENS,CIRA.HE_LONGDENS,CIRA.O_LONGDENS,CIRA.N2_LONGDENS,CIRA.O2_LONGDENS,CIRA.AR_LONGDENS,CIRA.H_LONGDENS,CIRA.O_LONGDENS,CIRA.N_LONGDENS,CIRA.ANOM_O_LONGDENS; ...
                    CIRA.TOTAL_LATGDENS,CIRA.HE_LATGDENS,CIRA.O_LATGDENS,CIRA.N2_LATGDENS,CIRA.O2_LATGDENS,CIRA.AR_LATGDENS,CIRA.H_LATGDENS,CIRA.O_LATGDENS,CIRA.N_LATGDENS,CIRA.ANOM_O_LATGDENS; ...
                    CIRA.TOTAL_TIMEGDENS,CIRA.HE_TIMEGDENS,CIRA.O_TIMEGDENS,CIRA.N2_TIMEGDENS,CIRA.O2_TIMEGDENS,CIRA.AR_TIMEGDENS,CIRA.H_TIMEGDENS,CIRA.O_TIMEGDENS,CIRA.N_TIMEGDENS,CIRA.ANOM_O_TIMEGDENS; ...
                    CIRA.TOTAL_TIMEGDENS,CIRA.HE_TIMEGDENS,CIRA.O_TIMEGDENS,CIRA.N2_TIMEGDENS,CIRA.O2_TIMEGDENS,CIRA.AR_TIMEGDENS,CIRA.H_TIMEGDENS,CIRA.O_TIMEGDENS,CIRA.N_TIMEGDENS,CIRA.ANOM_O_TIMEGDENS];
    % lookup table for temperatures and their derivatives
    TemperatureIndex = [CIRA.ALTITUDE_TEMP;CIRA.GRADIENT_TEMP; ...
                        CIRA.LON_GRADIENT_TEMP;CIRA.LAT_GRADIENT_TEMP; ...
                        CIRA.TIME_GRADIENT_TEMP;CIRA.TIME_GRADIENT_TEMP];
    numMasses = CIRA.ANOM_O_MASS - CIRA.ALL_MASS + 1; % number of molar mass constants
  end
  properties (Constant)
    F107_SW            = 1; % index into switch for F10.7 EFFECT ON MEAN
    TIME_INDEP_SW      = 2; % index into switch for TIME INDEPENDENT
    SYM_ANNUAL_SW      = 3; % index into switch for SYMMETRICAL ANNUAL
    SYM_SEMIANNUAL_SW  = 4; % index into switch for SYMMETRICAL SEMIANNUAL
    ASYM_ANNUAL_SW     = 5; % index into switch for ASYMMETRICAL ANNUAL
    ASYM_SEMIANNUAL_SW = 6; % index into switch for ASYMMETRICAL SEMIANNUAL
    DIURNAL_SW         = 7; % index into switch for DIURNAL
    SEMIDIURNAL_SW     = 8; % index into switch for SEMIDIURNAL
    DAILY_AP_SW        = 9; % index into switch for DAILY AP
    ALL_UTLONG_EFF_SW  = 10; % index into switch for ALL UT/LONG EFFECTS
    LONG_SW            = 11; % index into switch for LONGITUDINAL
    UT_MIXED_UTLONG_SW = 12; % index into switch for UT AND MIXED UT/LONG
    MIXED_APUTLONG_SW  = 13; % index into switch for MIXED AP/UT/LONG
    TERDIURNAL_SW      = 14; % index into switch for TERDIURNAL
    EQUILIB_DEPART_SW  = 15; % index into switch for DEPARTURES FROM DIFFUSIVE EQUILIBRIUM
    ALL_TINF_SW        = 16; % index into switch for ALL TINF VAR
    ALL_TLB_INDEP_SW   = 17; % index into switch for ALL TLB VAR
    ALL_TN1_SW         = 18; % index into switch for ALL TN1 VAR
    ALL_S_SW           = 19; % index into switch for ALL S VAR
    ALL_TN2_SW         = 20; % index into switch for ALL TN2 VAR
    ALL_NLB_SW         = 21; % index into switch for ALL NLB VAR
    ALL_TN3_SW         = 22; % index into switch for ALL TN3 VAR
    TURBO_SCALE_SW     = 23; % index into switch for TURBO SCALE HEIGHT VAR
    maxSW = 25; % maximum number of switches
    allSwitchesOn = ones(CIRA.maxSW,1); % turn on all options
    allSwitchesOff = zeros(CIRA.maxSW,1); % turn off all options
  end

  properties (Constant)

    % alternate values to remove external dependencies
    %RGAS = 831.4;
    %ARGMAX = 709.7; % for MatLab double exp
    %BM = 1.3806E-19;
    %pi = pi();
    %DGTR = 1.74533E-2; % radians per degree
    %HR = .2618; % radians per hour
    %SR = 7.2722E-5; % radians per second
    %INVAVOG = 1.66E-24;

    ARGMAX = 50.0; % maximum input acceptable to exponential function without producing infinity
    ISWinitialized=64999; % indicator whether switches have been set
    RGAS = CODATA2006.molarGasConstant*100; % molar gas constant ( N cm/(mol K) )
    BM = CODATA2006.boltzmannConstant*1.0e4; % Boltzmann Constant (hPa cm^3 / K)
    pi = OEIS.Pi; % circle ratio pi (3.14...)
    DGTR = OEIS.PiOver180; % radians per degree
    INVAVOG = 1.0/CODATA2006.avogadroConstant; % inverse of Avogadro's constant (mol)
    YRDSHIFT = 1000.0; % shift factor for composite YEAR-DayOfYear integer
    KGPERGM = 1.0/1000.0; % conversion from grams to kilograms
    CM3PERM3 = 1.E6; % conversion from cubic meters to cubic centimeters
  end

  properties (Constant)
    maxIC = 2; % maximum number of IC
    maxLegDeg = 7; % maximum Associated Legendre Polynomial degree
    maxLegOrd = 3; % maximum Associated Legendre Polynomial order
  end
  properties
    % data from file
    
    constituentName; % array of chemical names of constituents
    DISASSOCIATED; % array of flags indicating molecule is dissassociated
    MT; % array of molar mass constants for atmospheric constituents (g/mol)
    ALPHA; % power factor for component
    defaultF107; % default value for solar flux at 10.7cm (Jansky)
    defaultAp; % default Ap index
    defaultLatitude; % default Geodetic latitude (degrees)
    DR; % radians per day of annual cycle
    SR; % radians per second of daily cycle
    HR; % radians per hour of solar daily cycle
    mainConstituent; % mass index for primary component
    REQ; % radius of the Earth at the equator (km)
    FI; % inverse flattening
    GPL; % gravity at the poles (cm/s^2)
    GEQ; % gravity at the equator (cm/s^2)
    TINFLim; % modified MSIS TINF limit (K)
    LTHHT; % Lower thermosphere temp variations not significant below (km)
    ALTL; % altitude (km)
    MIXINDEX; % indices for ground mixing correction
    EQUILIBINDEX; % indices for equilibrium departure
    PTMINDEX; % indices into PTM for nodes of ZN1
    MN1; % 1 altitude array size
    MN2; % 2 altitude array size
    MN3; % 3 altitude array size
    ZN1; % 1 altitude array (km)
    ZN2; % 2 altitude array (km)
    ZN3; % 3 altitude array (km)
    ZMIX; % mix altitude (km)
    p; % polynomial coefficients for surface radius
    g; % polynomial coefficients for surface gravity
    PTM; % LOWER BOUNDARY  (6 altitude (km))
    PDM; % DENSITY
    PT; % TEMPERATURE
    PDA; % DENSITY
    PS; % S PARAM
    PDL; % TURBO
    % Neutral Temperatures
    % TN1(2), TN1(3), TN1(4), TN1(5) TN2(1)
    PTL;
    % Neutral Temperatures
    % TN2(2), TN2(3), TN2(4) TN3(1), TN3(2), TN3(3), TN3(4),
    % TN3(5) SURFACE TEMP TSL, TGN3(2) SURFACE GRAD TSLG, TGN2(1) TGN1(2),
    % TGN3(1) TGN2(2)
    PMA;
    SAM; % SEMIANNUAL MULT (SAM)
    PAVGM; % MIDDLE ATMOSPHERE AVERAGES
    ISD; % date of data
    IST; % time of data generation
    NAM; % name of data generation
  end
  properties
    % member variables
    
    S = zeros(1,CIRA.DerLast); % scale with d^0, dz, dLon, dLat
    TN1; % TN1 temperature coefficients 1 (K/km)
    TN2; % TN2 temperature coefficients 2 (K/km)
    TN3; % TN3 temperature coefficients 3 (K/km)
    TGN1 = zeros(2,CIRA.DerLast); % TGN1 temperature gradient coefficients 1 (K/km)
    TGN2; % TGN2 temperature gradient coefficients 2 (K/km)
    TGN3 = zeros(2,CIRA.DerLast); % TGN3 temperature gradient coefficients 3 (K/km)
    SW = ones(CIRA.maxSW,1); % stored logical switches
    ISW = 0; % indicator whether switches have been set
    SWC = ones(CIRA.maxSW,1); % stored scale switches
    DM28 = zeros(1,CIRA.DerLast); % N2 mix density
    GSURF = zeros(1,CIRA.DerLast); % local surface gravity (cm/s^2)
    RE = zeros(1,CIRA.DerLast); % local radius of the Earth (km)
    IMR = 0; % logical true if want meters and kilograms returned
    PLG; % legendre polynomials
    PLGdLat; % latitude derivatives of legendre polynomials
    CTLOC = 0; % cosine of time
    STLOC = 0; % sine of time
    C2TLOC = 0; % cosine squared of time
    S2TLOC = 0; % sine squared of time
    C3TLOC = 0; % cosine cubed of time
    S3TLOC = 0; % sine cubed of time
    CTLOCdSTL = 0; % cosine of time
    STLOCdSTL = 0; % sine of time
    C2TLOCdSTL = 0; % cosine squared of time
    S2TLOCdSTL = 0; % sine squared of time
    C3TLOCdSTL = 0; % cosine cubed of time
    S3TLOCdSTL = 0; % sine cubed of time
    DAY = 0; % day
    DFA = 0; % F10.7 avg minus 150.0
    APDF = 0; % Apdf
    APT = zeros(4,CIRA.DerLast); % Ap factor
    LONG = 0; % longitude (degrees)
    XL = 1000.; % latitude (degrees)
    TLL = 1000.; % time
    GTD7_ALAST = 99999.0; % last altitude used for GTD7
    MSSL = -999.0; % last mass used for GTD7
    DM28M = zeros(1,CIRA.DerLast); % N2 mix density
    GTS7_ALAST = -999.0; % last value of altitude for GTS7 (km)
    GTS7_TINF = zeros(1,CIRA.DerLast); % last value of TINF for GTS7 (K)
    GTS7_G0 = zeros(1,CIRA.DerLast); % last value of G0 for GTS7
    GTS7_TLB = zeros(1,CIRA.DerLast); % last value of TLB for GTS7 (K)
    DS = zeros(CIRA.numDensities,1); % previous density calculations
    TS = zeros(CIRA.numTemperatures,1); % previous temperature calculations
    KONSOL = 1; % output (1 standard)
    IYDL = zeros(CIRA.maxIC,1); % previous year/day
    SECL = zeros(CIRA.maxIC,1); % previous seconds in day
    GLATL = zeros(CIRA.maxIC,1); % previous latitude
    GLL = zeros(CIRA.maxIC,1); % previous longitude
    STLL = zeros(CIRA.maxIC,1); % previous STL
    FAL = zeros(CIRA.maxIC,1); % previous F10.7 avg
    FL = zeros(CIRA.maxIC,1); % previous F10.7
    APL = zeros(CIRA.maxAP,CIRA.maxIC); % previous Ap's
    SWL = zeros(CIRA.maxSW,CIRA.maxIC); % previous logical switches
    SWCL = zeros(CIRA.maxSW,CIRA.maxIC); % previous scale switches
    SAV = zeros(CIRA.maxSW,1); % previous switches
    XALT = -1000.0; % previous altitude
  end
  methods
    function cira = CIRA(nm)
      % constructor
      if nargin > 0
        cira = cira.GETDATA(nm);
      else
        cira = cira.GETDATA('msise00.dat');
      end
      for I=1:CIRA.maxIC
        cira.IYDL(I) = -999.;
        cira.SECL(I) = -999.;
        cira.GLATL(I) = -999.;
        cira.GLL(I) = -999.;
        cira.STLL(I) = -999.;
        cira.FAL(I) = -999.;
        cira.FL(I) = -999.;
      end
      for I=1:CIRA.maxAP
        for J=1:CIRA.maxIC
          cira.APL(I,J) = -999.;
        end
      end
      for I=1:CIRA.maxSW
        for J=1:CIRA.maxIC
          cira.SWL(I,J) = -999.;
          cira.SWCL(I,J) = -999.;
        end
      end
    end
    [ D,T,ALT, context ] = GHP7( context, IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PRESS );
    [ D,T,context ] = GTD7( context, IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D1 );
    [ D,T,context ] = GTD7D( context, IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D1 );
    [ D,T,context ] = GTS7( context, IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D1 );
    [ context,retrieveFirst ] = TSELEC( context, SV );
    context = METERS( context, METER );
    w = geostrophicWind( context, D, T );
    P = totalPressure( context, D, T, incAnom );
    dP = totalPressureGradient( context, D, T, incAnom );
    dP = totalPressure3DGradient( context, D, T, incAnom );
    totMassDens = totalMassDensity( context, D, incAnom );
    delDens = totalMassDensityGradient( context, D, incAnom );
    delDens = totalMassDensity3DGradient( context, D, incAnom );
    tm = totalMolarMass( context, D, incAnom );
    dm = totalMolarMassGradient( context, D, incAnom );
    dm = totalMolarMass3DGradient( context, D, incAnom );
   
    [ context ] = GETDATA( context, FN );
    [ D, TZ ] = DENSM( context, ALT,D0,XM,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2 );
    [ D, TZ, TN1, TGN1 ] = DENSU( context,ALT,DLB,TINF,TLB,XM,ALPHA,ZLB,S,MN1,ZN1,TN1,TGN1 );
    [ D ] = MixingCorrection(context,D,DM,DZ28,DS,DMC,massI);
    [ T ] = GLOB7S( context, P, K );
    [ T,context,TL ] = GLOBE7( context, YRD,SEC,LAT,LONG,TLOC,F107A,F107,AP,P,m );
    [ VTST7,context ] = VTST7( context, IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,IC );
    [ GV, REFF, GVdLat, REFFdLat ] = GLATF( context, LAT );
    [ GV, REFF, GVdLat, REFFdLat ] = GLATFELLIPSOID( context, LAT );
    [D,T] = SetOutput(context,D,T,DZ,TZ,massI);
  end
  methods (Static)
    SW = defaultSwitches( );
    [ DZ ] = GetOutput( D, massI );
    function dT = temperature3DGradient( T )
      dT = [T(CIRA.TemperatureIndex(CIRA.DerLat)), ...
            T(CIRA.TemperatureIndex(CIRA.DerLon)), ...
            T(CIRA.TemperatureIndex(CIRA.DerAlt))];
    end
    totNumDens = totalNumberDensity( D, incAnom );
    delDens = totalNumberDensityGradient( D, incAnom );
    delDens = totalNumberDensity3DGradient( D, incAnom );
    [ SCALH,SCALHdZ,SCALHdRE,SCALHdGS,SCALHdM,SCALHdT ] = SCALH( ALT,RE,GSURF,XM,TEMP );
    [ CCOR, CCORdZ, CCORdR, CCORdH1, CCORdZH ] = CCOR( ALT, R, H1, ZH );
    [ CCOR2, CCOR2dZ, CCOR2dR ] = CCOR2( ALT, R, H1, ZH, H2 );
    [ DNET, DNETdDD, DNETdDM ] = DNET( DD,DM,ZHM,XMM,XM );
    [ ZETA, ZETAdRE, ZETAdZZ, ZETAdZL ] = ZETA( RE,ZZ,ZL );
    [ Y2,Y2dX,Y2dY,Y2dYP1,Y2dYPN ] = SPLINEM( X,Y,M,N,C,YP1,YPN );
    [ YI,YIdX,YIdXA,YIdYA,YIdYD1,YIdYD2 ] = SPLINI( XA,YA,Y2A,M,N,C,X,Y2AdXA,Y2AdYA,Y2AdYD1,Y2AdYD2 );
    [ Y,YdX,YdXA,YdYA,YdYD1,YdYD2 ] = SPLINTM( XA,YA,Y2A,M,N,C,X,Y2AdXA,Y2AdYA,Y2AdYD1,Y2AdYD2 );
    [ Y, YI ] = SPLINE(XS,YS,NM,MN,C,YD1,YD2,X);
    [ sg0, SG0dEX ] = SG0( EX, P25, P26, AP, defAp );
    [ TZ, YI, ZGDIF ] = Temperature(Z,RE,TGN,TN,ZN,MN);
    [ DZ2 ] = DensityCorrection(DZ1,TZ,YI,ALPHA,GSURF,RE,TN,ZN,XM,ZGDIF,addGamma,lowLim,highLim);
    [ TT, ZG2 ] = BatesTemperature(Z,RE,S,ZLB,TINF,TLB);
    [ DTA ] = BatesTemperatureGradient(Z,RE,S,ZLB,TINF,TT);
    [ PLG, PLGdLat ] = LegendrePolynomials( LAT, W, N, M );
    [ PCH, PCHdLat ] = ChebyshevPolynomials( LAT, W, N );
    [ YD, SECS, STL ] = year_dayofyear( tjd, GLONG );
    rtn = TEST( input, testCase );
  end
  methods (Test)
    testKnownValues(testCase);
    function testLowAlt(testCase)
      CIRA.TEST(' 40.0 -104.0 2012 265 16.0333333 0.0000001 0.1 104 105 4.1 5.0 4.2 4.3 4.4 4.5 4.6', testCase);
    end
    function testMedAlt(testCase)
      CIRA.TEST(' 40.0 -104.0 2012 265 16.0333333 100.0 0.0 104 105 4.1 5.0 4.2 4.3 4.4 4.5 4.6', testCase);
    end
    function testHighAlt(testCase)
      CIRA.TEST(' 40.0 -104.0 2012 265 16.0333333 1000.0 0.0 104 105 4.1 5.0 4.2 4.3 4.4 4.5 4.6', testCase);
    end
    function testSouth(testCase)
      CIRA.TEST(' -40.0 -104.0 2012 265 16.0333333 1000.0 0.0 104 105 4.1 5.0 4.2 4.3 4.4 4.5 4.6', testCase);
    end
    function testEast(testCase)
      CIRA.TEST(' 40.0 104.0 2012 265 16.0333333 1000.0 0.0 104 105 4.1 5.0 4.2 4.3 4.4 4.5 4.6', testCase);
    end
    function testEarly(testCase)
      CIRA.TEST(' 40.0 104.0 2012 265 0.0333333 1000.0 0.0 104 105 4.1 5.0 4.2 4.3 4.4 4.5 4.6', testCase);
    end
    testSpline(testCase);
    testPolynomials(testCase);
    testTimeRange(testCase);
    testTempContinuity(testCase);
    %testAltDerivative(testCase);
    %testLonDerivative(testCase);
    %testLatDerivative(testCase);
    %testTimeDerivative(testCase);
    function testPressureHeight(testCase)
      context = CIRA();
      IYD = 20120701;
      SEC = 10000.0;
      ALT = 1234.5;
      GLAT = 12.3;
      GLONG = -123.4;
      STL = 9.10;
      F107A = 104.5;
      F107 = 112.3;
      AP = 4.5;
      MASS = context.MT(CIRA.ALL_MASS);
      [ D,T,context ] = context.GTD7( IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS );
      PRESS = context.totalPressure(D,T);
      [ ~,~,ALT1, ~ ] = context.GHP7( IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PRESS );
      testCase.verifyEqual(ALT1,ALT,'AbsTol',1.0e-1);
    end
  end
end

