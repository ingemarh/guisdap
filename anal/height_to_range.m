% height_to_range.m : changes height and elevation to range
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% height_to_range.m
% function that calculates the range of a volume (in us)
% when the height (in km) and antenna direction (in deg) is known.
% assumes spherical earth for the moment
%
% See also: range_to_height
%
% function range=height_to_range(height,el)
  function range=height_to_range(height,el)
  
  global p_dtau v_lightspeed
  
  Earth_radius=6372;
  hei=height/Earth_radius;
  sin_el=sin(pi*el/180);
  range=1000*Earth_radius*(sqrt(sin_el^2+2*hei+hei.^2)-sin_el);
  range=range/(p_dtau*1e-6*v_lightspeed/2);
