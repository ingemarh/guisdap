% range_to_height.m: function that calculates the height of a volume (in km)
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% when the range (in us) and antenna direction (in deg) is known.
% optional unit for other units
%
% See also: loc2gg height_to_range
%
% function height=range_to_height(range,el,unit)
function height=range_to_height(range,el,unit)
Earth_radius=6372e3;
sin_el=sin(pi*el/180);
if nargin<3
  unit=1e3;
  global p_dtau v_lightspeed
  fac=p_dtau*1e-6*v_lightspeed/2/unit;
else
  fac=1;
end
ran=range*unit/Earth_radius*fac;
height=Earth_radius/unit*(sqrt(1+2*ran*sin_el+ran.^2)-1);

% This code for elliptical earth
%  scale=(p_dtau*1e-6*v_lightspeed/2);
%  gg_site=[69,25,0];
%  gg_sp=loc2gg(gg_site,[el,180,range*scale/1000])
%  [height,gg_sp(3)]
