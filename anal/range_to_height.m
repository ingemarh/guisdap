% range_to_height.m: function that calculates the height of a volume (in km)
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% when the range (in us) and antenna direction (in deg) is known.
%
% See also: loc2gg height_to_range
%
% function height=range_to_height(range,el)
  function height=range_to_height(range,el)
  
  global p_dtau v_lightspeed
  
  Earth_radius=6372;
  ran=range*(p_dtau*1e-6*v_lightspeed/2)/1000/Earth_radius;
  sin_el=sin(pi*el/180);
  height=Earth_radius*(sqrt(1+2*ran*sin_el+ran.^2)-1);

% This code for elliptical earth
%  scale=(p_dtau*1e-6*v_lightspeed/2);
%  gg_site=[69,25,0];
%  gg_sp=loc2gg(gg_site,[el,180,range*scale/1000])
%  [height,gg_sp(3)]
