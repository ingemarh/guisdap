function [ OVL_ANG ] = OVL_ANG( context, sla,slo,cla,clo,rr )
%OVL_ANG This function returns an estimate at the given location of the angle
%      real function OVL_ANG(sla,slo,cla,clo,rr)
%  *********************************************************************
%  This function returns an estimate at the given location of the angle
%  (oval_angle) between the directions (tangents) along the constant
%  CGM and geographic latitudes by utilizing the function DFRIDR from
%  Numerical Recipes for FORTRAN.
%
%  This angle can be taken as the azimuth to the local "magnetic" north
%  (south) if the eastward (westward) tangent to the local CGM latitude
%  points south (north) from the local geographic latitude.
%
%  Written by Therese Moretto in August 1994 (revised by V. Papitashvili
%  in January 1999).
%  *********************************************************************

%      real cgmgla,cgmglo,dfridr
%      logical cr360,cr0
%
%      external cgmgla,cgmglo,dfridr
%
%      common/cgmgeo/clat,cr360,cr0,rh

%  Ignore points which nearly coincide with the geographic or CGM poles
%  within 0.01 degree in latitudes; this also takes care if SLA or CLA
%  are dummy values (e.g., 999.99)

  if abs(sla) >= 89.99 || ...
     abs(cla) >= 89.99 || ...
     abs(sla) < 30.
    OVL_ANG = IGRF.BAD_ANGLE;
    return;
  end

%  Initialize values for the cgmglo and cgmgla functions

  context.rh = rr;
  context.clat = cla;
  context.cr360 = false;
  context.cr0 = false;

%  Judge if SLO may be crossing the 360-0 limit. If geocentric
%  longitude of the location is larger than 270 deg, then cr360 is
%  set "true"; if it is less than 90 deg, then cr0 is set "true".

  if slo >= 270.
    context.cr360 = true;
  end
  if slo <= 90.
    context.cr0 = true;
  end

%  An initial stepsize (in degrees)

  step = 10.;

%  Note that in the near-pole region the functions CGMGLA and CGMGLO
%  could be called from DFRIDR with the CGM latitudes exceeded 90 or
%  -90 degrees (e.g., 98 or -98) when STEP is added or subtracted to a
%  given CGM latitude (CLA). This does not produce discontinuities in
%  the functions because GEOCOR calculates GEOLAT smoothly for the
%  points lying behind the pole (e.g., as for 82 or - 82 deg. in the
%  above-mentioned example). However, it could be discontinuity in
%  GEOLON if |GEOLAT| = 90 deg. - see CGMGLO for details.

  [hom,~] = context.DFRIDR(@CGMGLA,clo,step);

  [denom,~] = context.DFRIDR(@CGMGLO,clo,step);

  denom = denom*cos(sla*IGRF.UMR);

  if hom == 0 && denom == 0
    OVL_ANG = 0.0;
  else
    OVL_ANG = -atan2(hom,denom);
  end

  OVL_ANG = OVL_ANG/IGRF.UMR;


end

