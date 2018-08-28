classdef IGRF < matlab.unittest.TestCase
%IGRF International Geomagnetic Reference Field version 11
% This is a port of the IRI-2012 fortran.
% Copyright(c) 2014 Jonathan Kipling Knight (<a href="matlab:
% web('mailto:drkipknight@aol.com')">drkipknight@aol.com</a>)
% DEPENDENCIES: This class needs the classes WGS84 and OEIS
% For more information, see the online IAGA <a href="matlab:
% web('http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html')">web site</a>.
% See also WGS84, OEIS and <a href="matlab:web(fullfile(fileparts(which('IGRF.m')),'doc','Example.html'))">example</a>
  properties (Constant)
    GEOGRAPHIC_COORDINATES = 0; % indicator for Geographic coordinates for input
    GEOMAGNETIC_COORDINATES = 1; % indicator for Geomagnetic coordinates for input
  end
  properties (Constant)
    SPHERICAL_INPUT = 1; % indicator for spherical coordinates for input
    CARTESIAN_INPUT = -1; % indicator for Cartesian coordinates for input
  end
  properties (Constant)
    version = 11; % version of IGRF model
    %EREQU=NOVAS.ERAD/1000.0;
    %ERPOL=IGRF.EREQU *(1.0 - NOVAS.F);
    %EREQU=6378.16;
    %ERPOL=6356.775;
    %UMR=atan(1.0)*4./180.;
    %pi = IGRF.UMR*180.0;
    
    MINYEAR = 1900.0; % Minimum year for Version 11
    MAXYEAR = 2020.0; % Maximum year for Version 11
    EREQU=WGS84.equatorialRadius/1000.0; % Earth's equatorial radius (km)
    ERPOL=WGS84.polarRadius/1000.0; % Earth's polar radius (km)
    AQUAD=IGRF.EREQU*IGRF.EREQU; % square of equatorial radius (km^2)
    BQUAD=IGRF.ERPOL*IGRF.ERPOL; % square of polar radius (km^2)
    pi = OEIS.Pi; % circle ratio (3.1415...)
    twopi = IGRF.pi*2.0; % two pi
    UMR = OEIS.PiOver180; % degrees to radians conversion factor
    numCoeff = 14*14; % maximum number of coefficients
    BAD_ANGLE = 999.99; % indicator for bad angle
    % rotation matrix
    U = transpose([+0.3511737,-0.9148385,-0.1993679;  
                   +0.9335804,+0.3583680,+0.0000000;
                   +0.0714471,-0.1861260,+0.9799247]);
  end
  properties
    ERA=6371.2; % effective Earth radius (km), replaced when files are read in
    ST0 = 0.0; % stored sin of T0
    CT0 = 0.0; % stored cos of T0
    SL0 = 0.0; % stored sin of L0
    CL0 = 0.0; % stored cos of L0
    CTCL = 0.0; % stored cos of TCL
    STCL = 0.0; % stored sin of TCL
    CTSL = 0.0; % stored cos of TSL
    STSL = 0.0; % stored sin of TSL
    SFI = 0.0; % stored sin of FI
    CFI = 0.0; % stored cos of FI
    SPS = 0.0; % stored sin of PSI
    CPS = 0.0; % stored cos of PSI
    SHI = 0.0; % stored sin of HI
    CHI = 0.0; % stored cos of HI
    HI = 0.0; % stored HI
    PSI = 0.0; % stored PSI
    XMUT = 0.0; % stored magnetic time
    A11 = 0.0; % stored matrix element 1,1
    A21 = 0.0; % stored matrix element 2,1
    A31 = 0.0; % stored matrix element 3,1
    A12 = 0.0; % stored matrix element 1,2
    A22 = 0.0; % stored matrix element 2,2
    A32 = 0.0; % stored matrix element 3,2
    A13 = 0.0; % stored matrix element 1,3
    A23 = 0.0; % stored matrix element 2,3
    A33 = 0.0; % stored matrix element 3,3
    DS3 = 0.0; % stored DS3
    K = 0.0; % stored K
    IY = 0.0; % stored IY
    BA = zeros(8,1); % stored BA
    NMAX = 0.0; % stored maximum number of coefficients read in
    TIME = 0.0; % stored start time of coefficients
    GH1 = zeros(IGRF.numCoeff,1); % stored coefficients
    FIL1 = 0.0; % file name of coefficients
    GHI1 = 0.0; % first element of GH1
    GHI2 = 0.0; % second element of GH1
    GHI3 = 0.0; % third element of GH1
    KONSOL = 1; % output unit for error messages (1 for standard output)
    IYR = 0; % stored year
    NM = 0; % stored NM
    clat = 0; % stored latitude (degrees)
    cr360 = 0; % stored cr360
    cr0 = 0; % stored cr0
    rh = 0; % stored rh
    IDE = 0.0; % stored IDE
  end
  methods
    function igrf = IGRF()
      % constructor
      %igrf = igrf.INITIZE();
    end
    OVL_ANG = OVL_ANG( context, sla,slo,cla,clo,rr );
    cgmgla = CGMGLA( context,clon );
    cgmglo = CGMGLO( context,clon );
    [ DFRIDR,err ] = DFRIDR( context,func,x,h );
    [ DIMO,context ] = FELDCOF( context, YEAR );
    [ xout,yout,zout ] = GEOMAG( context, xin,yin,zin,J );
    context = RECALC( context, IYR,IDAY,IHOUR,MIN,ISEC );
    [ CLAR,CLOR,RBM,SLAC,SLOC ] = GEOLOW( context,SLAR,SLOR,RH );
    [ H,D,Z ] = MFC( context, SLA,SLO,R );
    [ R1,R2,R3 ] = RIGHT( context, X,Y,Z, DS3 );
    [ DAT,PLA,PLO, context ] = GEOCGM01( context, ICOR,IYEAR,HI,DAT );
    [ SLA,SLO,DLA,DLO,PMI ] = CORGEO( context, RH,CLA,CLO );
    [ DLA,DLO,CLA,CLO,PMI ] = GEOCOR( context, SLA,SLO,RH );
    B = FELDC( context, V );
    [ BNORTH,BEAST,BDOWN,BABS ] = FELDG( context, GLAT,GLON,ALT );
    [ H, BABS, BEAST, BNORTH, BDOWN ] = FELDI( context, RQ, XXX, YYY, ZZZ, ...
      IS, CP, SP, CT, ST );
    [ ACLA,ACLO,SLAF,SLOF ] = FTPRNT( context, RH,SLA,SLO,CLA,CLO,RF );
    [ FL,ICODE,B0 ] = SHELLG( context, GLAT,GLON,ALT,DIMO );
    [ P,BQ,R ] = STOER( context, P, k );
    [ DEC,DIP,DIPL,YMODIP,context ] = IGRF_DIP( context, xlat,xlong,YEAR,height );
    [ XL,ICODE,DIPL,BABS,context ] = IGRF_SUB( context, xlat,xlong,YEAR,HEIGHT );
    [ lat, lon, context ] = GEODIP( context, IYR,latin,lonin,J );
    [ NMAX, ERAD, GH, IER ] = GETSHC( context, FSPEC );
    [ X,Y,Z ] = SHAG( context, X,Y,Z, DS );
    MLT = CLCMLT( context,IYYYY,DDD,UTHR,GLAT,GLON );
    [ XM,YM,ZM ] = DPMTRX( context,IYYYY,DDD );
  end
  methods (Static)
    function vector = newVector()
      vector = zeros(1,3);
    end
    [ XXX,YYY,ZZZ, CT,ST,CP,SP ] = GEODETIC2CARTESIAN( GLAT, GLON, ALT );
    [ NMAX, GH ] = EXTRASHC( DATE, DTE1, NMAX1, GH1, NMAX2, GH2 );
    [ NMAX, GH ] = INTERSHC( DATE, DTE1, NMAX1, GH1, DTE2, NMAX2, GH2 );
    AZM_ANG = AZM_ANG( sla,slo,cla,pla,plo );
    [ BX,BY,BZ ] = BSPCAR( TETA,PHI,BR,BTET,BPHI );
    [ BR,BT,BF ] = IGRFM( IY,NM,R,T,F );
    UT = MLTUT( SLA,SLO,CLA,PLA,PLO );
    [ xout, yout, zout ] = SPHCAR( xin,yin,zin,J );
    [ GST,SLONG,SRASN,SDEC,SOB,COB ] = SUN( IYEAR,IDAY,IHOUR,MIN,ISEC );
    [ X,Y,Z,F,DIP,DEC,SMODIP ] = FIELDG( DLAT,DLONG,ALT );
  end
  methods (Test)
    function testPlot(testCase)
      numPoints = 100;
      startYear = 1900;
      endYear = 2015;
      xlat = 40;
      xlong = -104;
      HEIGHT = 0.0;
      inc = (endYear-startYear)/(numPoints-1);
      mags = zeros(numPoints,1);
      years = zeros(numPoints,1);
      context = IGRF();
      i = 1;
      for YEAR = startYear:inc:endYear
        years(i) = YEAR;
        [ ~,~,~,mags(i),context ] = context.IGRF_SUB( xlat,xlong,YEAR,HEIGHT );
        i = i + 1;
      end
      plot(years,mags);
      testCase.verifyEqual(i-1,numPoints);
    end
  end
end

