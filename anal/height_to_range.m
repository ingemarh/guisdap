% height_to_range.m : changes height and elevation to range
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% height_to_range.m
% function that calculates the range of a volume (in us)
% when the height (in km) and antenna direction (in deg) is known.
% assumes spherical earth for the moment
% optional unit for other units
%
% See also: range_to_height
%
% function range=height_to_range(height,el,unit)
function range=height_to_range(height,el,unit)
Earth_radius=6372e3;
sin_el=sin(pi*el/180);
if nargin<3
  unit=1e3;
  global p_dtau v_lightspeed
  fac=p_dtau*1e-6*v_lightspeed/2/unit;
else
  fac=1;
end
hei=height*unit/Earth_radius;
range=Earth_radius/unit/fac*(sqrt(sin_el.^2+2*hei+hei.^2)-sin_el);
