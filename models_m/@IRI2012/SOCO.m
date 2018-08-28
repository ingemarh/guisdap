function [ DECLIN, ZENITH, SUNRSE, SUNSET ] = SOCO( ld,t,flat,Elon,height )
%SOCO calculate the solar declination, zenith angle, and sunrise & sunset times
%
%******************************************************************
%********** ZENITH ANGLE, DAY OF YEAR, TIME ***********************
%******************************************************************
%
%
%        subroutine soco (ld,t,flat,Elon,height,
%                DECLIN, ZENITH, SUNRSE, SUNSET)
%--------------------------------------------------------------------
%       s/r to calculate the solar declination, zenith angle, and
%       sunrise & sunset times  - based on Newbern Smith's algorithm
%       [leo mcnamara, 1-sep-86, last modified 16-jun-87]
%       {dieter bilitza, 30-oct-89, modified for IRI application}
%
% in:   ld      local day of year
%       t       local hour (decimal)
%       flat    northern latitude in degrees
%       elon    east longitude in degrees
%		height	height in km
%
% out:  declin      declination of the sun in degrees
%       zenith      zenith angle of the sun in degrees
%       sunrse      local time of sunrise in hours 
%       sunset      local time of sunset in hours 
%-------------------------------------------------------------------

%        common/const/   dtr     /const1/humr,dumr
  persistent p1 p2 p3 p4 p6;
  if isempty(p1)
% amplitudes of Fourier coefficients  --  1955 epoch.................
    p1 = 0.017203534;
    p2 = 0.034407068;
    p3 = 0.051610602;
    p4 = 0.068814136;
    p6 = 0.103221204;
  end
%
% s/r is formulated in terms of WEST longitude.......................
  Wlon = 360. - double(Elon);
%
% time of equinox for 1980...........................................
  td = double(ld) + (double(t) + Wlon/15.) / 24.;
  te = td + 0.9369;
%
% declination of the sun..............................................
  dcl = 23.256 * sin(p1*(te-82.242)) + 0.381 * sin(p2*(te-44.855)) ...
      + 0.167 * sin(p3*(te-23.355)) - 0.013 * sin(p4*(te+11.97)) ...
      + 0.011 * sin(p6*(te-10.41)) + 0.339137;
  DECLIN = dcl;
  dc = dcl * IRI2012.UMR;
%
% the equation of time................................................
  tf = te - 0.5;
  eqt = -7.38*sin(p1*(tf-4.)) - 9.87*sin(p2*(tf+9.)) ...
      + 0.27*sin(p3*(tf-53.)) - 0.2*cos(p4*(tf-17.));
  et = eqt * IRI2012.UMR / 4.;
%
  fa = flat * IRI2012.UMR;
  phi = IRI2012.humr * ( t - 12.) + et;
%
  a = sin(fa) * sin(dc);
  b = cos(fa) * cos(dc);
  cosx = a + b * cos(phi);
  if abs(cosx) > 1.
    cosx=sign(cosx);
  end
  ZENITH = acos(cosx) / IRI2012.UMR;
  %
  % calculate sunrise and sunset times --  at the ground...........
  % see Explanatory Supplement to the Ephemeris (1961) pg 401......
  % sunrise at height h metres is at...............................
  h=height*1000.;
  chih = 90.83 + 0.0347 * sqrt(h);
  % this includes corrections for horizontal refraction and........
  % semi-diameter of the solar disk................................
  ch = cos(chih * IRI2012.UMR);
  cosphi = (ch -a ) / b;
  % if abs(secphi) > 1., sun does not rise/set.....................
  % allow for sun never setting - high latitude summer.............
  secphi = 999999.;
  if cosphi ~= 0.
    secphi = 1./cosphi;
  end
  SUNSET = IRI2012.SUN_NEVER_SETS;
  SUNRSE = IRI2012.SUN_NEVER_RISES;
  if secphi > -1.0 && secphi <= 0.
    return;
  end
  % allow for sun never rising - high latitude winter..............
  SUNSET = -IRI2012.SUN_NEVER_SETS;
  SUNRSE = -IRI2012.SUN_NEVER_RISES;
  if secphi > 0.0 && secphi < 1.
    return;
  end

  if cosphi > 1.
    cosphi=sign(cosphi);
  end
  phi = acos(cosphi);
  et = et / IRI2012.humr;
  phi = phi / IRI2012.humr;
  SUNRSE = 12. - phi - et;
  SUNSET = 12. + phi - et;
  if SUNRSE < 0.
    SUNRSE = SUNRSE + 24.;
  end
  if SUNSET >= 24.
    SUNSET = SUNSET - 24.;
  end

end

