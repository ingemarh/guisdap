function [ GST,SLONG,SRASN,SDEC,SOB,COB ] = SUN( IYEAR,IDAY,IHOUR,MIN,ISEC )
%SUN CALCULATES FOUR QUANTITIES NECESSARY FOR COORDINATE TRANSFORMATIONS
%  WHICH DEPEND ON SUN POSITION
%      SUBROUTINE SUN (IYEAR,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)
%-----------------------------------------------------------------------------
%  CALCULATES FOUR QUANTITIES NECESSARY FOR COORDINATE TRANSFORMATIONS
%  WHICH DEPEND ON SUN POSITION (AND, HENCE, ON UNIVERSAL TIME AND SEASON)
%
%-------  INPUT PARAMETERS:
%  IYR,IDAY,IHOUR,MIN,ISEC -  YEAR, DAY, AND UNIVERSAL TIME IN HOURS, MINUTES,
%    AND SECONDS  (IDAY=1 CORRESPONDS TO JANUARY 1).
%
%-------  OUTPUT PARAMETERS:
%  GST - GREENWICH MEAN SIDEREAL TIME, SLONG - LONGITUDE ALONG ECLIPTIC
%  SRASN - RIGHT ASCENSION,  SDEC - DECLINATION  OF THE SUN (RADIANS)
%  ORIGINAL VERSION OF THIS SUBROUTINE HAS BEEN COMPILED FROM:
%  RUSSELL, C.T., COSMIC ELECTRODYNAMICS, 1971, V.2, PP.184-196.
%
%  LAST MODIFICATION:  MARCH 31, 2003 (ONLY SOME NOTATION CHANGES)
%
%     ORIGINAL VERSION WRITTEN BY:    Gilbert D. Mead
%-----------------------------------------------------------------------------

%      DOUBLE PRECISION DJ,FDAY
  persistent MINYEAR MAXYEAR W0 W1 L0 L1 M0 e ob;
  if isempty(MINYEAR)
    MINYEAR = 1901;
    MAXYEAR = 2099;
    W0 = 0.9856473354; % = 360(w*86400/2pi-1) Earth rotation (degrees/day)
    W1 = 0.985600267; % Earth revolution (degrees/day)
    L0 = 279.696678; % Ecliptic Longitude at Epoch 1900.0 (degrees)
    L1 = 279.690983; % GST at Epoch 1900.0 (degrees)
    M0 = 358.475845; % Mean Longitude at Epoch 1900.0 (degrees)
    e = [-0.004789,1.91946]; % = 180/pi * 2*e Eccentricity of Earth's orbit
    ob = [-0.0130125,23.45229];
  end

  if IYEAR < MINYEAR || IYEAR > MAXYEAR
    GST = 0.0;
    SLONG = 0.0;
    SRASN = 0.0;
    SDEC = 0.0;
    return;
  end
  FDAY=(double(IHOUR)*3600+double(MIN)*60+double(ISEC))/86400.0;
  % days since J1900.0 (Jan 0.0, 1900): good from Jan 1, 1901 to Dec 31, 2100
  DJ=365*(double(IYEAR)-1900)+floor((double(IYEAR)-1901)/4)+double(IDAY)-0.5+FDAY;
  T=DJ/36525.;
  VL=mod(L0+W0*DJ,360.0);
  GST=mod(L1+W0*DJ+360.*FDAY+180.,360.0)*IGRF.UMR;
  G=mod(M0+W1*DJ,360.0)*IGRF.UMR;
  SLONG=(VL+(e(2)+e(1)*T)*sin(G)+0.020094*sin(2.*G))*IGRF.UMR;
  if SLONG > IGRF.twopi
    SLONG=SLONG-IGRF.twopi;
  end
  if SLONG < 0.
    SLONG=SLONG+IGRF.twopi;
  end
  OBLIQ=(ob(2)+ob(1)*T)*IGRF.UMR;
  SOB=sin(OBLIQ);
  COB=cos(OBLIQ);
  SLP=SLONG-9.924E-5;
  %
  %   THE LAST CONSTANT IS A CORRECTION FOR THE ANGULAR ABERRATION  DUE TO
  %   THE ORBITAL MOTION OF THE EARTH
  %
  SIN1=SOB*sin(SLP);
  COS1=sqrt(1.-SIN1^2);
  SC=SIN1/COS1;
  SDEC=atan(SC);
  y = COB/SOB*SC;
  x = -cos(SLP)/COS1;
  if x == 0 && y == 0
    SRASN = IGRF.pi;
  else
    SRASN = IGRF.pi-atan2(y,x); % [0-2pi)
  end

end

