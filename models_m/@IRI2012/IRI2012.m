classdef IRI2012 < matlab.unittest.TestCase
%IRI2012 Class for calculating ionosphere parameters from IRI2012
% International Reference Ionosphere 2012
% This is a port of the IRI-2012 fortran.
% Copyright(c) 2014 by Jonathan Kipling Knight (<a href="matlab:
% web('mailto:drkipknight@aol.com')">drkipknight@aol.com</a>)
% DEPENDENCIES: This class requires CIRA, IGRF, WGS84, CODATA2006, and OEIS classes
% See also CIRA, IGRF, WGS84, CODATA2006, OEIS and
% <a href="matlab:web(fullfile(fileparts(which('IRI2012.m')),'doc','Example.html'))">example</a>.
  properties (Constant)
    TOPSIDE_IRI2001   = 0; % indicator for Topside calculating using IRI-2001 model
    TOPSIDE_CORRECTED = 1; % indicator for Topside calculating using corrected model
    TOPSIDE_NeQuick   = 2; % indicator for Topside calculating using Ne-Quick model
    TOPSIDE_GulH05    = 3; % indicator for Topside calculating using Gulyaeva H0.5 model
  end
  properties (Constant)
    FAST_TEC     = 0; % IRI_TEC fast, but higher uncertainty <5%
    STANDARD_TEC = 1; % IRI_TEC standard, recommended
    BEST_TEC     = 2; % IRI_TEC stepsize of 1 km; best TEC, longest CPU time
  end
  properties (Constant)
    UT_TO_LT = 0; % indicator to convert Universal Time into Solar Local Time
    LT_TO_UT = 1; % indicator to convert Solar Local Time into Universal Time
  end
  properties (Constant)
    SPRING = 1; % indicator of spring season
    SUMMER = 2; % indicator of summer season
    FALL   = 3; % indicator of fall season
    WINTER = 4; % indicator of winter season
  end
  properties (Constant)
    %ivar for IRI_WEB interface
    
    ALTITUDE_VARIATION  = 1; % IRI_WEB option for altitude variation
    LATITUDE_VARIATION  = 2; % IRI_WEB option for latitude variation
    LONGITUDE_VARIATION = 3; % IRI_WEB option for longitude variation
    YEAR_VARIATION      = 4; % IRI_WEB option for year variation
    MONTH_VARIATION     = 5; % IRI_WEB option for month variation
    DAY_VARIATION       = 6; % IRI_WEB option for day variation
    DOY_VARIATION       = 7; % IRI_WEB option for day of year variation
    HOUR_VARIATION      = 8; % IRI_WEB option for hour variation
  end
  properties (Constant)
    EL_DENS_OUT = 1; % row index into OUTF for electron density/m^-3
    NT_TEMP_OUT = 2; % row index into OUTF for neutral temperature/K
    IO_TEMP_OUT = 3; % row index into OUTF for ion temperature/K
    EL_TEMP_OUT = 4; % row index into OUTF for electron temperature/K
    O_DENS_OUT  = 5; % row index into OUTF for O+ ion density/% or /m^-3 if JF(22)=f 
    H_DENS_OUT  = 6; % row index into OUTF for H+ ion density/% or /m^-3 if JF(22)=f
    HE_DENS_OUT = 7; % row index into OUTF for HE+ ion density/% or /m^-3 if JF(22)=f
    O2_DENS_OUT = 8; % row index into OUTF for O2+ ion density/% or /m^-3 if JF(22)=f
    NO_DENS_OUT = 9; % row index into OUTF for NO+ ion density/% or /m^-3 if JF(22)=f
    CL_DENS_OUT = 10; % row index into OUTF for cluster ions density/% or /m^-3 if JF(22)=f
    N_DENS_OUT  = 11; % row index into OUTF for N+ ion density/% or /m^-3 if JF(22)=f
    D_DENS_OUT  = 14; % row index into OUTF for standard IRI-Ne for 60,65,..,110km 
    EL_GRAD_OUT = 15; % row index into OUTF for electron density altitude gradient /m^-3/km
    D_GRAD_OUT  = 16; % row index into OUTF for standard IRI-Ne gradient for 60,65,..,110km  m^-3/km
    NT_GRAD_OUT = 17; % row index into OUTF for neutral temperature gradient/K/km
    IO_GRAD_OUT = 18; % row index into OUTF for ion temperature gradient/K/km
    ET_GRAD_OUT = 19; % row index into OUTF for electron temperature gradient/K/km
    numResults = 20; % maximum number of rows in OUTF
  end
  properties (Constant)
    NMF2_IN_OUT        = 1; % index into OARR for maximum F2 layer electron density NmF2/m^-3
    HMF2_IN_OUT        = 2; % index into OARR for height of NmF2, HmF2/km
    NMF1_IN_OUT        = 3; % index into OARR for maximum F1 layer electron density NmF1/m^-3
    HMF1_IN_OUT        = 4; % index into OARR for height of NmF2, HmF1/km
    NME_IN_OUT         = 5; % index into OARR for maximum E layer electron density NmE/m^-3
    HME_IN_OUT         = 6; % index into OARR for height of NmE, HmE/km
    NMD_OUT            = 7; % index into OARR for maximum D layer electron density NmD/m^-3
    HMD_OUT            = 8; % index into OARR for height of NmD, HmD/km
    HHALF_OUT          = 9; % index into OARR for HHALF/km
    B0_OUT             = 10; % index into OARR for B0/km
    VALLEY_BASE_OUT    = 11; % index into OARR for VALLEY-BASE/m^-3
    VALLEY_TOP_OUT     = 12; % index into OARR for VALLEY-TOP/km
    TE_PEAK_OUT        = 13; % index into OARR for peak electron temperature/K
    TE_PEAK_HEIGHT_OUT = 14; % index into OARR for height of TE-PEAK/km
    TE_MOD300_IN_OUT   = 15; % index into OARR for TE-MOD(300KM)/K
    TE_MOD400_IN_OUT   = 16; % index into OARR for TE-MOD(400KM)/K
    TE_MOD600_OUT      = 17; % index into OARR for TE-MOD(600KM)/K
    TE_MOD1400_OUT     = 18; % index into OARR for TE-MOD(1400KM)/K
    TE_MOD3000_OUT     = 19; % index into OARR for TE-MOD(3000KM)/K
    TE_120_OUT         = 20; % index into OARR for TE(120KM)=TN=TI/K
    TI_MOD430_OUT      = 21; % index into OARR for TI-MOD(430KM)
    H_TE_TI_OUT        = 22; % index into OARR for height/km, where electron temperature and ion temperature are equal
    SOL_ZENITH_ANG_OUT = 23; % index into OARR for solar zenith angle/degree
    SUN_DEC_OUT        = 24; % index into OARR for Sun declination/degree
    DIP_OUT            = 25; % index into OARR for dip/degree
    DIP_LAT_OUT        = 26; % index into OARR for dip latitude/deg
    MODIP_LAT_OUT      = 27; % index into OARR for modified dip latitude/degree
    GEOG_LAT_OUT       = 28; % index into OARR for Geographic latitude/degree
    SUNRISE_OUT        = 29; % index into OARR for sunrise/decimal hours
    SUNSET_OUT         = 30; % index into OARR for sunset/decimal hours
    SEASON_OUT         = 31; % index into OARR for ISEASON (1=spring)
    GEOG_LON_OUT       = 32; % index into OARR for Geographic longitude/degree
    RZ12_IN_OUT        = 33; % index into OARR for Rz12
    COV_IND_OUT        = 34; % index into OARR for Covington Index
    B1_OUT             = 35; % index into OARR for B1
    M3000_F2_OUT       = 36; % index into OARR for M(3000)F2
    TEC_OUT            = 37; % index into OARR for TEC/m-2
    TEC_TOP_OUT        = 38; % index into OARR for TEC_top/TEC*100. (percent)
    IG12_IN_OUT        = 39; % index into OARR for GIND (IG12)
    F1_PROB_OUT        = 40; % index into OARR for F1 probability (0-1)
    F107D_IN_OUT       = 41; % index into OARR for F10.7 daily (Jy)
    C1_OUT             = 42; % index into OARR for c1 (F1 shape)
    DAYNR_OUT          = 43; % index into OARR for day of the year
    EQ_VERT_OUT        = 44; % index into OARR for equatorial vertical 
    FOF2_STORM_OUT     = 45; % index into OARR for foF2_storm/foF2_quiet ion drift (m/s)
    F107_81_IN_OUT     = 46; % index into OARR for F10.7_81 (Jy)
    FOE_STORM_OUT      = 47; % index into OARR for foE_storm/foE_quiet
    SPREAD_F_PROB_OUT  = 48; % index into OARR for spread-F probability (0-1)          
    GEOM_LAT_OUT       = 49; % index into OARR for Geomag. latitude/degree
    GEOM_LON_OUT       = 50; % index into OARR for Geomag. longitude/degree
    AP_CURRENT_OUT     = 51; % index into OARR for Ap at current time
    AP_DAILY_OUT       = 52; % index into OARR for daily Ap
    INVDIP_OUT         = 53; % index into OARR for INVDIP/degree
    MLT_TE_OUT         = 54; % index into OARR for MLT-Te
    CGM_LAT_OUT        = 55; % index into OARR for CGM-latitude
    CGM_LON_OUT        = 56; % index into OARR for CGM-longitude
    CGM_MLT_OUT        = 57; % index into OARR for CGM-MLT
    CGM_LAT_AUR_BOUND_OUT = 58; % index into OARR for CGM lat equatorial auroral boundary
    CGM_LAT_MLT0_OUT   = 59; % index into OARR for CGM-LATI (MLT=0)
    CGM_LAT_MLT1_OUT   = 60; % index into OARR for CGM-LATI (MLT=1)
    CGM_LAT_MLT2_OUT   = 61; % index into OARR for CGM-LATI (MLT=2)
    CGM_LAT_MLT3_OUT   = 62; % index into OARR for CGM-LATI (MLT=3)
    CGM_LAT_MLT4_OUT   = 63; % index into OARR for CGM-LATI (MLT=4)
    CGM_LAT_MLT5_OUT   = 64; % index into OARR for CGM-LATI (MLT=5)
    CGM_LAT_MLT6_OUT   = 65; % index into OARR for CGM-LATI (MLT=6)
    CGM_LAT_MLT7_OUT   = 66; % index into OARR for CGM-LATI (MLT=7)
    CGM_LAT_MLT8_OUT   = 67; % index into OARR for CGM-LATI (MLT=8)
    CGM_LAT_MLT9_OUT   = 68; % index into OARR for CGM-LATI (MLT=9)
    CGM_LAT_MLT10_OUT  = 69; % index into OARR for CGM-LATI (MLT=10)
    CGM_LAT_MLT11_OUT  = 70; % index into OARR for CGM-LATI (MLT=11)
    CGM_LAT_MLT12_OUT  = 71; % index into OARR for CGM-LATI (MLT=12)
    CGM_LAT_MLT13_OUT  = 72; % index into OARR for CGM-LATI (MLT=13)
    CGM_LAT_MLT14_OUT  = 73; % index into OARR for CGM-LATI (MLT=14)
    CGM_LAT_MLT15_OUT  = 74; % index into OARR for CGM-LATI (MLT=15)
    CGM_LAT_MLT16_OUT  = 75; % index into OARR for CGM-LATI (MLT=16)
    CGM_LAT_MLT17_OUT  = 76; % index into OARR for CGM-LATI (MLT=17)
    CGM_LAT_MLT18_OUT  = 77; % index into OARR for CGM-LATI (MLT=18)
    CGM_LAT_MLT19_OUT  = 78; % index into OARR for CGM-LATI (MLT=19)
    CGM_LAT_MLT20_OUT  = 79; % index into OARR for CGM-LATI (MLT=20)
    CGM_LAT_MLT21_OUT  = 80; % index into OARR for CGM-LATI (MLT=21)
    CGM_LAT_MLT22_OUT  = 81; % index into OARR for CGM-LATI (MLT=22)
    CGM_LAT_MLT23_OUT  = 82; % index into OARR for CGM-LATI (MLT=23)
    KP_CURRENT_OUT     = 83; % index into OARR for Kp at current time 
    MAG_DEC_OUT        = 84; % index into OARR for magnetic declination
    numAdditionalResults = 100; % maximum number of additional results
  end
  properties (Constant)
    Ne_COMPUTED_SW        = 1; % index into JF, true for Ne computed, false for Ne not computed, default true
    TeTi_COMPUTED_SW      = 2; % index into JF, true for Te, Ti computed, false for Te, Ti not computed, default true
    NeNi_COMPUTED_SW      = 3; % index into JF, true for Ne & Ni computed, false for Ni not computed, default true
    B0B1_Bil2000_SW       = 4; % index into JF, true for B0,B1 - Bil-2000, false for B0,B1 - other models indicated by JF(31), default false
    FOF2_CCIR_SW          = 5; % index into JF, true for foF2 - CCIR, false for foF2 - URSI, default false
    Ni_DS_1995_SW         = 6; % index into JF, true for Ni - DS-1995 & DY-1985, false Ni - RBV-2010 & TTS-2003, default false
    Ne_Tops_SW            = 7; % index into JF, true for Ne - Tops: f10.7<188, false F10.7 unlimited, default true
    FOF2_MODEL_SW         = 8; % index into JF, true for foF2 from model, false foF2 or NmF2 - user input, default true
    HMF2_MODEL_SW         = 9; % index into JF, true for HmF2 from model, false HmF2 or M3000F2 - user input, default true
    Te_STANDARD_SW        = 10; % index into JF, true for Te - Standard, false Te - Using Te/Ne correlation, default true
    Ne_STANDARD_SW        = 11; % index into JF, true for Ne - Standard Profile, false Ne - Lay-function formalism, default true
    STANDARD_OUT_SW       = 12; % index into JF, true for Messages to standard output, false to messages.txt, default true
    FOF1_MODEL_SW         = 13; % index into JF, true for foF1 from model, false foF1 or NmF1 - user input, default true
    HMF1_MODEL_SW         = 14; % index into JF, true for HmF1 from model, false HmF1 - user input (only Lay version), default true
    FOE_MODEL_SW          = 15; % index into JF, true for foE  from model, false foE or NmE - user input, default true
    HME_MODEL_SW          = 16; % index into JF, true for hmE  from model, false HmE - user input, default true
    RZ12_FILE_SW          = 17; % index into JF, true for Rz12 from file, false Rz12 - user input, default true
    IGRF_DIP_SW           = 18; % index into JF, true for IGRF dip, magbr, modip, false old FIELDG using POGO68/10 for 1973, default true
    F1PROB_MODEL_SW       = 19; % index into JF, true for F1 probability model, false critical solar zenith angle (old), default true
    F1_STANDARD_SW        = 20; % index into JF, true for standard F1, false standard F1 plus L condition, default true
    IONDRIFT_COMPUTED_SW  = 21; % index into JF, true for ion drift computed, false ion drift not computed, default false
    IONDENS_PERCENT_SW    = 22; % index into JF, true for ion densities in %, false ion densities in m^-3, default true
    Te_TOPS_Bil_1985_SW   = 23; % index into JF, true for Te_tops (Bil-1985), false Te_topside (TBT-2012), default false
    D_REGION_IRI1990_SW   = 24; % index into JF, true for D-region: IRI-1990, false FT-2001 and all opts in OUTF(14,*), default true
    F107D_FILE_SW         = 25; % index into JF, true for F107D from APF107.DAT, false F107D user input (OARR(41)), default true
    FOF2_STORM_MODEL_SW   = 26; % index into JF, true for foF2 storm model, false no storm updating, default true
    IG12_FILE_SW          = 27; % index into JF, true for IG12 from file, false IG12 - user, default true
    SPREAD_F_PROB_COMPUTED_SW = 28; % index into JF, true for spread-F probability, false not computed, default false
    IRI01_TOPSIDE_SW      = 29; % index into JF, true for IRI01-topside, false new options as def. by JF(30), default false
    IRI01_TOPSIDE_CORR_SW = 30; % index into JF, true for IRI01-topside corr., false NeQuick topside model, default false
    B0B1_ABT_2009_SW      = 31; % index into JF, true for B0,B1 ABT-2009, false B0 Gulyaeva-1987 h0.5, default true
    F107_81_FILE_SW       = 32; % index into JF, true for F10.7_81 from file, false F10.7_81 - user input OARR(46), default true
    AUR_BOUND_MODEL_SW    = 33; % index into JF, true for Auroral boundary model on, false off, default false
    MESSAGES_ON_SW        = 34; % index into JF, true for Messages on, false Messages off, default true
    FOE_STORM_MODEL_SW    = 35; % index into JF, true for foE storm model, false no foE storm updating, default false
    HMF2_WO_STORM_SW      = 36; % index into JF, true for HmF2 w/out foF2_storm, false with foF2-storm, default true
    TOPSIDE_WO_STORM_SW   = 37; % index into JF, true for topside w/out foF2-storm, false with foF2-storm, default true
    NO_WRITES_IRIFLIP_SW  = 38; % index into JF, true for turn WRITEs off in IRIFLIP, false turn WRITEs on, default true
    numSwitches = 50; % maximum number of switches
  end
  properties (Constant)
    O_ION  = 0; % O+ ion species
    H_ION  = 1; % H+ ion species
    He_ION = 2; % He+ ion species
    N_ION  = 3; % N+ ion species
  end
  properties (Constant)
    MINYEAR = 1958.0; % Minimum year for IRI2012, limited by ig_rz.dat
    MAXYEAR = IGRF.MAXYEAR; % Maximum year for IRI2012, limited by IGRF
    
    float_t = 'double'; % type for floating calculations
    pi = OEIS.Pi; % circle ratio pi (3.14...)
    UMR = OEIS.PiOver180; % degrees to radians conversion factor
    humr = OEIS.PiOver12; % hours to radians conversion factor
    ALOG2 = OEIS.Log2; % natural logarithm of two
    alg10 = OEIS.Log10; % natural logarithm of ten
    ALG100 = 2.0*OEIS.Log10; % natural logarithm of one hundred
    % electron plasma resonance factor (m^-3 MHz^-2)
    resonance = (1.0e6*1.0e6)*4*OEIS.Pi*OEIS.Pi*CODATA2006.electronMass* ...
                                       CODATA2006.electricConstant/ ...
                                      (CODATA2006.elementaryCharge* ...
                                       CODATA2006.elementaryCharge);
    %ARGMAX = 709.7; % maximum input to exponential function for MatLab double exp

    %resonance = cast(1.24E10,IRI2012.float_t); % resonance factor (m^-3 MHz^-2)
    %pi=cast(atan(1.0)*4.,IRI2012.float_t); % circle ratio pi (3.14...)
    %ALOG2  = cast(log(2.0),IRI2012.float_t); % natural logarithm of two
    %alg10 = cast(log(10.0),IRI2012.float_t); % natural logarithm of ten
    %ALG100 = cast(log(100.0),IRI2012.float_t); % natural logarithm of one hundred
    ARGMAX = cast(88.0,IRI2012.float_t); % maximum input to exponential function
    %UMR = cast(IRI2012.pi/180,IRI2012.float_t); % degrees to radians conversion factor
    %humr = cast(IRI2012.pi/12,IRI2012.float_t); % hours to radians conversion factor
    
    dumr = cast(2.0*IRI2012.pi/365,IRI2012.float_t); % days of year to radians conversion factor
    BAD_AP = -1.0; % indicator for bad or missing Ap index
    UT_INDICATOR = 25.; % indicator for UTC time input instead of local time
    SUN_NEVER_SETS = 99.; % indicator for the Sun never setting
    SUN_NEVER_RISES = 99.; % indicator for the Sun never rising
    F_N_LIM = 100.; % MHz frequencies are smaller than this value, Num Densities are bigger
    
    numDregHeights = 11; % number of D-region heights
    numDregTypes = 5; % number of D-region types
    Y05 = cast(.6931473,IRI2012.float_t); % Y05 value
    DTE = cast([5.,5.,10.,20.,20.],IRI2012.float_t); % DTE
    F2numI = 13; % number of rows of F2 table
    F2numJ = 76; % number of columns of F2 table
    FM3numI = 9; % number of rows of Fm3 table
    FM3numJ = 49; % number of columns of Fm3 table
    F2ccir = IRI2012.getCCIRF2(); % F2 table from CCIR
    FM3ccir = IRI2012.getCCIRFM3(); % Fm3 table from CCIR
    F2ursi = IRI2012.getURSI(); % F2 table from URSI

  end
  properties
    ciractx = CIRA(); % context for CIRA atmosphere model
    igrfctx = IGRF(); % context for IGRF magnetic field model

    SWMI = ones(CIRA.maxSW,1); % local copy of switches
    DAT = zeros(11,4,  IRI2012.float_t); % DAT matrix
    HMF2 = cast(0.0,  IRI2012.float_t); % HmF2 (km)
    NMF2 = cast(0.0,  IRI2012.float_t); % NmF2 (m^-3)
    HMF1 = cast(0.0,  IRI2012.float_t); % HmF1 (km)
    F1REG = false; % F1 region indicator
    B0 = cast(0.0,  IRI2012.float_t); % B0 model parameter
    B1 = cast(0.0,  IRI2012.float_t); % B1 model parameter
    C1 = cast(0.0,  IRI2012.float_t); % C1 model parameter
    HZ = cast(0.0,  IRI2012.float_t); % HZ model parameter
    T = cast(0.0,  IRI2012.float_t); % T model parameter
    HST = cast(0.0,  IRI2012.float_t); % HST model parameter
    HME = cast(0.0,  IRI2012.float_t); % HmE (km)
    NME = cast(0.0,  IRI2012.float_t); % NmE (m^-3)
    HEF = cast(0.0,  IRI2012.float_t); % HEF (km)
    ENIGHT = false; % true if E layer is in Earth's shadow                    
    E = zeros(4,1,  IRI2012.float_t); % E array                    
    HMD = cast(0.0,  IRI2012.float_t); % HmD (km)
    NMD = cast(0.0,  IRI2012.float_t); % NmD (m^-3)
    HDX = cast(0.0,  IRI2012.float_t); % HDX model parameter
    D1 = cast(0.0,  IRI2012.float_t); % D1 model parameter    
    XKK = cast(0.0,  IRI2012.float_t); % XKK model parameter    
    FP30 = cast(0.0,  IRI2012.float_t); % FP30 model parameter    
    FP3U = cast(0.0,  IRI2012.float_t); % FP3U model parameter    
    FP1 = cast(0.0,  IRI2012.float_t); % FP1 model parameter    
    FP2 = cast(0.0,  IRI2012.float_t); % FP2 model parameter    
    HS = cast(0.0,  IRI2012.float_t); % HS model parameter
    TNHS = cast(0.0,  IRI2012.float_t); % TNHS model parameter
    XSM = zeros(4,1,  IRI2012.float_t); % XSM model parameters
    MM = zeros(5,1,  IRI2012.float_t); % MM model parameters
    DTI = cast([10.,10.,20.,20.],  IRI2012.float_t); % DTI model parameters
    MXSM = cast(0.0,  IRI2012.float_t); % MXSM model parameter      
    BETA = cast(0.0,  IRI2012.float_t); % BETA model parameter
    ETA = cast(0.0,  IRI2012.float_t); % ETA model parameter
    DELTA = cast(0.0,  IRI2012.float_t); % DELTA model parameter
    ZETA = cast(0.0,  IRI2012.float_t); % ZETA model parameter
    B2TOP = cast(0.0,  IRI2012.float_t); % B2TOP model parameter
    TC3 = cast(0.0,  IRI2012.float_t); % TC3 model parameter
    itopn = cast(0.0,  IRI2012.float_t); % TOPSIDE model indicator
    hcor1 = cast(0.0,  IRI2012.float_t); % hcor1 model parameter
    AHH = zeros(7,1,  IRI2012.float_t); % AHH model parameters
    ATE1 = cast(0.0,  IRI2012.float_t); % ATE1 model parameter
    STTE = zeros(6,1,  IRI2012.float_t); % STTE model parameter
    H05TOP = cast(0.0,  IRI2012.float_t); % H05TOP model parameter
    QF = cast(1.0,  IRI2012.float_t); % QF model parameter
    dQF = cast(0.0,  IRI2012.float_t);
    XNETOP = cast(0.0,  IRI2012.float_t); % XNETOP model parameter
    XM3000 = cast(0.0,  IRI2012.float_t); % XM3000 model parameter
    HHALF = cast(0.0,  IRI2012.float_t); % HHALF model parameter
    tau = cast(0.0,  IRI2012.float_t); % tau model parameter
    icalls = 0; % indicator for IRI_SUB previously being called
    nmono = -1; % month of previous call to IRI_SUB
    iyearo = -1; % year of previous call to IRI_SUB
    idaynro = -1; % day of year of previous call to IRI_SUB
    rzino = -1; % Rz12 of previous call to IRI_SUB
    igino = -1; % IG of previous call to IRI_SUB
    ut0 = -1; % UT of previous call to IRI_SUB
    KONSOL = 1; % output of messages (1 is standard output, use fopen for other files)
    FLON = cast(0.0,  IRI2012.float_t); % longitude (degrees)   
    RYEAR = cast(0.0,  IRI2012.float_t); % decimal year   
    EUVION = zeros(3,12,  IRI2012.float_t); % EUVION model parameter
    PEXCIT = zeros(3,12,  IRI2012.float_t); % PEXCIT model parameter
    PEPION = zeros(3,12,  IRI2012.float_t); % PEPION model parameter
    OTHPR1 = zeros(6,1,  IRI2012.float_t); % OTHPR1 model parameter
    OTHPR2 = zeros(6,1,  IRI2012.float_t); % OTHPR2 model parameter
    ZFLUX = zeros(37,1,  IRI2012.float_t); % ZFLUX model parameter
    SIGABS = zeros(3,37,  IRI2012.float_t); % SIGABS model parameter
    ZLAM = zeros(37,1,  IRI2012.float_t); % ZLAM model parameter
    SIGION = zeros(3,37,  IRI2012.float_t); % SIGION model parameter
    TPOT = zeros(3,10,  IRI2012.float_t); % TPOT model parameter
    NNI = zeros(3,1,  IRI2012.float_t); % NNI model parameter
    LAMAX = cast(0.0,  IRI2012.float_t); % LAMAX model parameter
    UVFAC = zeros(59,1,  IRI2012.float_t); % UVFAC model parameter
    EUV = cast(0.0,  IRI2012.float_t); % EUV model parameter
    LMAX = 0; % LMAX model parameter
    F107SV = cast(0.0,  IRI2012.float_t); % F107 saved values
    IPROBS = 0; % IPROBS model parameter
    TPROB = zeros(3,6,37,  IRI2012.float_t); % TPROB model parameter
    kf; % kf model parameter
    n; % n model parameter
    RE = 0.0; % RE model parameter
    TZERO = 0.0; % TZERO model parameter
    IFIT = 0; % IFIT model parameter
    IB = 0; % IB model parameter
    KINT = 0; % KINT model parameter
    LINT = 0; % LINT model parameter
    KEXT = 0; % KEXT model parameter
    LEXT = 0; % LEXT model parameter
    KMAX = 0; % KMAX model parameter
    FN; % FN model parameter
    BINT; % BINT model parameter
    BEXT; % BEXT model parameter
    COV = 0; % Covington index
    COVSAT = 0;
    RZAR = zeros(3,1);
    ARIG = zeros(3,1);
    NMONTH = 0;
    ttt = 0;
  end
  methods
    function iri = IRI2012()
      % constructor
      % ensure output is common to sub-models
      iri.ciractx.KONSOL = iri.KONSOL;
      iri.igrfctx.KONSOL = iri.KONSOL;
    end
    [ IAP ] = APF( context, IYYYY,IMN,ID,HOUR );
    [ F107D,F107PD,F107_81,F107_365,IAPDA ] = APF_ONLY( context,IYYYY,IMN,ID );
    [ IAPO ] = APFMSIS( context,IYYYY,IMN,ID,HOUR );
    [ bspl2f ] = BSPL2F( context, i,t1 );
    [ OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT ] = CHEMION( context, ...
       JPRINT,ALT,F107,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,USER_NO,N4S,NE, ...
       USER_NPLUS,SZAD  );
    [ DXE1N ] = DXE1N( context, H );
    [ ELTE,dELTE ] = ELTE( context, H );
    [ PEFLUX,IMAX,AFAC,DE,EV ] = FLXCAL( context,IDIM,ALT,SZADEG,TE,TN,XN, ...
       XNE,XN2D,XOP2D,IMAX );
    [ fmodip,context ] = FMODIP( context,xlat );
    [ OUTF,OARR,context ] = IRI_SUB( context, JF,JMAG,ALATI,ALONG,IYYYY,...
       MMDD,DHOUR,HEIBEG,HEIEND,HEISTP,OARR );
    [ tectot,tectop,tecbot ] = IRI_TEC( context,hstart,hend,istep );
    [ a,b,context ] = IRI_WEB( context,jmag,jf,alati,along,iyyyy,mmdd,iut,...
       dhour,height,h_tec_max,ivar,vbeg,vend,vstp,b );
    [ context,LMAX ] = PARAMS( context,ISW );
    [ context ] = PRIMPR( context,IJ,Z,ZOX,ZN2,ZO2,HE,SZA,TN,F107,F107A,N4S );
    [ BN,BE,BV,context ] = SCHNEVPD( context, RZ,FLAT,FLON,R,T,L );
    [ SCHUPR,SCHUHT ] = SCHUMN( context,J,Z,ZO2,COLUMN,SCHUPR,SCHUHT );
    [ N2APRD, context ] = SECIPRD( context,ALT,SZADEG,F107,F107A,TE,TN,OXN,...
       O2N,N2N,XNE );
    [ B,context ] = SHAB1D( context, FLAT,FLON,T,RZ )
    [ B, context ] = SHAMDB0D( context, RLAT,FLON,T,RZ );
    [ osfbr, context ] = SPREADF_BRAZIL( context, idoy,idiy,f107,geolat );
    [ STORME_AP ] = STORME_AP( context,JDOY,XMLAT,AP );
    [ rz,ig,rsn,nmonth ] = TCON( context, yr,mm,day,idn );
    [ TI,dTI ] = TI( context, H );
    [ XE1,dXE_1 ] = XE1( context, H );
    [ XE2,context,dXE2 ] = XE2( context, H );
    [ XE3_1,context,dXE3_1 ] = XE3_1( context, H );
    [ XE4_1,dXE4_1 ] = XE4_1( context, H );
    [ XE5,dXE5 ] = XE5( context, H );
    [ XE6,dXE6 ] = XE6( context, H );
    [ XEN,dXEN ] = XEN( context,H,HMF2,XNMF2,HME,NL,HX,SC,AMP );
    [ ZERO ] = ZERO( context, DELTA );
  end
  methods (Static)
    [ R1,R2 ] = APROK( j1m,j2m,h1,h2,R1m,R2m,rk1m,rk2m,hei,xhi );
    [ cgmlat,ab_mlat ] = AURORAL_BOUNDARY( xkp,xmlt );
    [ bspl2l ] = BSPL2L( i,t1 );
    [ bspl2s ] = BSPL2S( i,t1 );
    [ bspl4_long ] = BSPL4_LONG( i,x1 );
    [ bspl4_time ] = BSPL4_TIME( i,x1 );
    [ bspl4_ptime ] = BSPL4_PTIME( i,x1 );
    [ bspl4t ] = BSPL4T( i,t1 );
    [ NO,NH,NHE,NN ] = CALION( CRD,INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,DDD,F107 );
    [ RATCH ] = CHEBISH( COVS,HOURLT,ABMLAT,SG );
    [ ckp ] = CKP( ap );
    [ N2A ] = CN2A( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,P3X1,P3X2,P3X3,P3X4 );
    [ P1,L1 ] = CN2D( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NOP,NE,N2PLS,DISN2D,UVDISN, ...
       NPLUS,N2P,N2D,OPLS,NNO,N2A );
    [ N2PLS ] = CN2PLS( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,PUN2PX,PUN2PA,PUN2PB, ...
       PEN2PX,PEN2PA,PEN2PB,OP2D,OP2P,HEPLUS,NPLUS,NNO,N4S );
    [ P1,L1 ] = CN4S( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,N4S,DISN4S,N2D,N2P,OPLS, ...
       N2PLS,UVDISN,NOP,NPLUS,NNO,O2P,PDNOSR,VCON );
    [ P1,L1 ] = CNO( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,N2D,N4S,N2P,NNO,O2P,OPLS, ...
       PDNOSR,PLYNOP,N2A,NPLUS );
    [ P1,NOP ] = CNOP( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,OPLS,N2PLS,O2P,N4S,NNO, ...
       NPLUS,N2P,PLYNOP,VCON,N2D,OP2D );
    [ NPLUS ] = CNPLS( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,DISNP,OPLS,N2D,OP2P, ...
       HEPLUS,PHOTN,O2P,N4S,OP2D,N2PLS,NNO );
    [ P1,O2P ] = CO2P( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD5,OPLS,OP2D,N2PLS, ...
       NPLUS,N4S,NNO,OP2P );
    [ rgma ] = CONVER( rga,rgo );
    [ OP2D ] = COP2D( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD2,OP2P,HEPLUS,N4S,NNO,PSEC );
    [ OP2P ] = COP2P( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD3,PSEC,HEPLUS,N4S,NNO,TE );
    [ OPLS ] = COP4S( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD1,OP2D,OP2P,PEPION, ...
       PDISOP,N2PLS,N2D,NNO,VCON,HEPLUS );
    [ D1LAY ] = D1LAY( X, XM, SC, HX );
    [ D2LAY ] = D2LAY( X, XM, SC, HX );
    [ JF ] = defaultIRIswitches( );
    [ elg ] = DRegion( z,it,f,vKp,f5SW,f6WA );
    [ TE,SIGTE,INVDIP ] = ELTEIK( CRD,PF107Y,INVDIP,FL,DIMO,B0,...
                                             DIPL,MLT,ALT,DDD,PF107 );
    [ EPLA ] = EPLA( X, SC, HX );
    [ EPTR, dEPTR ] = EPTR( X, SC, HX );
    [ EPST ] = EPST( X, SC, HX );
    [ EPSTEP ] = EPSTEP( Y2, Y1, SC, HX, X );
    [ EDENS,IERROR,dEDENS ] = F00( HGT,GLAT1,IDAY,ZANG,F107T );
    [ f1_c1 ] = F1_C1( xmodip,hour,suxnon,saxnon );
    [ f1prob, f1probl ] = F1_PROB( sza,glat,rz12 );
    [ UVFAC ] = FACEUV( UVFAC,F107,F107A );
    [ FFAC ] = FACFLX( EE,UVFAC );
    [ UVFAC ] = FACSR( UVFAC,F107,F107A );
    [ FOEEDI ] = FOEEDI( COV,XHI,XHIM,XLATI );
    [ FOF1ED ] = FOF1ED( YLATI,R,CHI );
    [ FOUT ] = FOUT( XMODIP,XLATI,XLONGI,UT,FF0 );
    [ funct ] = G( param,x );
    [ GAMMA1 ] = GAMMA1( SMODIP,SLAT,SLONG,HOUR,IHARM,NQ,K1,M,MM,M3,SFE );
    [ HMF2ED, X ] = HMF2ED( XMAGBR,R,X,XM3 );
    [ HPOL ] = HPOL( HOUR,TW,XNW,SA,SU,DSA,DSU );
    [ HXL,SCL,AMP,IQUAL ] = INILAY( NIGHT,F1REG,XNMF2,XNMF1,XNME,...
        VNE,HMF2,HMF1,HME,HV1,HV2,HHALF );
    [ INTERP ] = INTERP( N,L,V,K,X,XOUT );
    [ INVDPC ] = INVDPC( FL,DIMO,B0,DIPL,DTOR );
    [ cn ] = IONCO1( h,zd,fd,fs,t );
    [ R1,R2,R3,R4 ] = IONCO2( hei,xhi,it,F );
    [ dion ] = IONDANI( id,ismo,hx,zd,fd,fs );
    [ NION, D ] = IONHIGH( CRD,INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,DDD,D,ION );
    [ NION, D ] = IONLOW( CRD,INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,DDD,D,ION );
    [ DOUT ] = KODERR( MIRREQ );
    [ DOUT ] = KOEFD( MIRREQ );
    [ PG1O ] = KOEFP1(  );
    [ PG2O ] = KOEFP2(  );
    [ PG3O ] = KOEFP3( );
    [ DOUT ] = KOF107( MIRREQ );
    [ P,DP,PMS ] = LEGFUN( M,FN,CONST,COLAT,IPRT );
    [ A,B,AUS ] = LNGLSN( N, A, B );
    [ j ] = LOCATE( xx,n,x );
    [ VAR,SING ] = LSKNM( N, M, M0, M1, HM, SC, HX, W,X,Y );
    [ MONTH,IDAY,IDOY,NRDAYMO,IYEAR ] = MODA( IN,IYEAR,MONTHh,IDAYy );
    [ R4S,R2D,R2P ] = OXRAT( E );
    [ SIGEX,SIGEXT ] = OXSIGS( E );
    [ PROB ] = PROBN2( ISW,L,ZLAM,PROB,JPTS );
    [ PROB ] = PROBO2( ISW,L,ZLAM,PROB,JPTS );
    [ PROB ] = PROBS( ISW,ZLAM,LMAX,NNI );
    [ RTS ] = RATS( J,TE,TI,TN );
    [ RDH,RDHE ] = RDHHE( H,HB,RDOH,RDO2H,RNO,PEHE );
    [ RDNO ] = RDNO( H,HB,RDO2H,RDOH,RNO );
    [ RLAY,dRLAY ] = RLAY( X, XM, SC, HX );
    [ XE2TO5,dXE2TO5 ] = XE2TO5( H,HMF2,NL,HX,SC,AMP );
    [ B0_98 ] = B0_98( HOUR, SAX, SUX, NSEASN, R, ZLO, ZMODIP );
    [ SX,GRO ] = ROGUL( IDAY,XHI );
    [ RPID ] = RPID( H, H0, N0, M, ST, ID, XS );
    [ COLUMN ] = SCOLUM( J,CHI,Z,TN,XN );
    [ SIGOX,SIGN2,SIGEE ] = SIGEXS( E,TE,XNE );
    [ DECLIN, ZENITH, SUNRSE, SUNSET ] = SOCO( ld,t,flat,Elon,height );
    [ C ] = SPHARM( L,M,COLAT,AZ );
    [ C ] = SPHARM_IK( L,M,COLAT,AZ );
    [ y2 ] = SPLINE( x,y,n,yp1,ypn );
    [ y ] = SPLINT( xa,ya,y2a,n,x );
    [ cf,rgma ] = STORM( ap,rga,rgo,coor,ut,doy );
    [ PromptVd,DynamoVd,Vd ] = StormVd( FLAG,iP,AE,SLT );
    [ FE ] = SUFE( FIELD,RFE,M );
    [ A ] = SWAPEL( N,A );
    [ T_XS_N2 ] = T_XS_N2( EP );
    [ T_XS_OX ] = T_XS_OX( EP );
    [ AUS6,SPT ] = TAL( SHABR,SDELTA,SHBR,SDTDH0 );
    [ THINT,TZERO ] = TBFIT( T1,T2,IBF );
    [ TE ] = TEBA( DIPL,SLT,NS );
    [ TEDE ] = TEDE( H,DEN,COV );
    [ TEDIF ] = TEDIFI( F107IN,TEXN,TEDN,F107DF );
    [ HT05 ] = TOPH05( COVI,AMLAT,TIME,HMAX,SG );
    [ TOPQ, dTOPQ ] = TOPQ( h,No,hmax,Ho );
    [ TPASEA ] = TPCAS( MLTRAD,PF107,PF107M,XNDI,DNDI,PD,XNNI,DNNI,PN );
    [ TP350A,TP350B,TP550A,TP550B,TP850A,TP850B,TP140A,TP140B,TP200A,TP200B ] ...
      = TPCORR( INVDIP,MLT,DDD,PF107,P350A,P350B,P550A,P550B,P850A,P850B,P1400A,P1400B, ...
      P2000A,P2000B );
    [ SIGIT ] = TXSION( E );
    [ ut,slt,iyyy,ddd ] = UT_LT( mode,ut,slt,glong,iyyy,ddd );
    [ HVB,VWU,VWA,VDP ] = VALGUL( XHI );
    [ y ] = VDRIFT( xt,xl,param );
    [ XMOUT ] = XMOUT( XMODIP,XLATI,XLONGI,UT,XM0 );
    [ XM3000HM, X ] = XM3000HM( XMAGBR,R,X,HMF2 );
    [ XMDED ] = XMDED( XHI,R,YW );
    [ YIELD ] = YLDISS( ISW,ZLAM );
    rtn = getAP( i );
    function fm3ccir = getCCIRFM3()
      % retrieve the FM3 CCIR table
      [~,fm3ccir] = IRI2012.getCCIRF2();
    end
    [f2,fm3] = getCCIRF2();
    f2 = getURSI();
    leap = IS_LEAPYEAR( yyyy );
    rtn = TEST(testCase);
  end
  methods (Test)
    function testKnown(testCase)
      % test known condition
      IRI2012.TEST(testCase);
    end
    %function testNaNs(testCase,prnt)
    function testNaNs(testCase)
      % test all switch configurations
      jf = zeros(IRI2012.numSwitches,1);
      if nargin < 2
        %prnt = true;
        prnt = false;
      end
      jmag = 0;
      xlat = 40.0;
      xlon = -104.0;
      iy = 2012;
      mmdd = 0921;
      iut = 1;
      hour = 16.0333333;
      hxx = 0.0;
      htec_max = 2000.0;
%       IVAR = IRI2012.ALTITUDE_VARIATION;
%       vbeg = 0.0;
%       vend = 2000.0;
%       vstp = 10.0;
%       numstp=1;
      IVAR = IRI2012.DOY_VARIATION;
      vbeg = 1.0;
      vend = 100.0;
      vstp = 10.0;
      numstp=floor((vend-vbeg)/vstp)+1;
      oar = zeros(IRI2012.numAdditionalResults,numstp);
      for i=1:numstp
        oar(IRI2012.NMF2_IN_OUT,i) = 1.0e12;
        oar(IRI2012.HMF2_IN_OUT,i) = 432.0;
        oar(IRI2012.NMF1_IN_OUT,i) = 1.0e10;
        oar(IRI2012.HMF1_IN_OUT,i) = 342.0;
        oar(IRI2012.NME_IN_OUT,i) = 1.0e9;
        oar(IRI2012.HME_IN_OUT,i) = 232.0;
        oar(IRI2012.TE_MOD300_IN_OUT,i) = 543.0;
        oar(IRI2012.TE_MOD400_IN_OUT,i) = 453.0;
        oar(IRI2012.RZ12_IN_OUT,i) = 5.2;
        oar(IRI2012.IG12_IN_OUT,i) = 3.3;
        oar(IRI2012.F107D_IN_OUT,i) = 170.0;
        oar(IRI2012.F107_81_IN_OUT,i) = 180.0;
      end
      context = IRI2012();
      jf1 = jf;
      % check for NaNs in all switch conditions
      for i=IRI2012.Ne_COMPUTED_SW:IRI2012.NO_WRITES_IRIFLIP_SW
        for j=0:1
          jf1(i) = j;
          fprintf(1,'testing switch %d/%d\n',i,j);
          [outf,oar,context] = context.IRI_WEB(jmag,jf1,xlat,xlon,iy,mmdd,iut,hour, ...
                  hxx,htec_max,IVAR,vbeg,vend,vstp,oar);
          if prnt
            [nr,nc] = size(outf);
            for irii=1:nr
              for irjj=1:nc
                str = sprintf('outf: %d,%d %.10f\n', irii,irjj,outf(irii,irjj));
                testCase.verifyFalse(isnan(outf(irii,irjj)),str);
              end
            end
            [nr,nc] = size(oar);
            for irii=1:nr
              for irjj=1:nc
                str = sprintf('oar: %d,%d %.10f\n', irii,irjj,oar(irii,irjj));
                testCase.verifyFalse(isnan(oar(irii,irjj)),str);
              end
            end
          end
        end
      end
    end
    function testPlots(testCase)
      % test plots
      numPoints = 1000;
      startYear = IRI2012.MINYEAR;
      endYear = IRI2012.MAXYEAR;
      ALATI = 40;
      ALONG = -104;
      hour = 0.0;
      DHOUR=hour;
      hxx = 150.0;
      inc = (endYear-startYear)/(numPoints-1);
      eldens = zeros(numPoints,1);
      years = zeros(numPoints,1);
      ctx = IRI2012();
      JF = IRI2012.defaultIRIswitches();
      JF(IRI2012.MESSAGES_ON_SW) = false;
      JF(IRI2012.TeTi_COMPUTED_SW) = false; % don't need temperature
      JF(IRI2012.AUR_BOUND_MODEL_SW) = false; % don't need aurora
      JMAG = IGRF.GEOGRAPHIC_COORDINATES;
      i = 1;
      for YEAR = startYear:inc:endYear
        years(i) = YEAR;
        MMDD = -floor(mod(YEAR,1.0)*365.2425+1);
        [outf,~,ctx] = ctx.IRI_SUB(JF,JMAG,ALATI,ALONG,floor(YEAR),...
          MMDD,DHOUR,hxx);
        eldens(i) = outf(IRI2012.EL_DENS_OUT,1);
        i = i + 1;
      end
      plot(years,eldens);
      testCase.verifyEqual(YEAR,endYear);
    end
  end
end
