% gc2gg: transforms coordinates from geocentric to geographic (lat, lon, h)
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Thanks to J. Murgin (KGI Reprot NO 80:2), EISCAT analysis package, and P.Pollari
%
% See also: gg2gc, loc2gg
%
% function gg=gc2gg(gc)
function gg=gc2gg(gc)

factor=pi/180;        % conversion factor from degrees to radians
% earth radius (km) and flatness factor
r_earth=6378.135;
g=1.00673944;

if gc(1)==0 & gc(2)==0,
  fprintf('Beware of the spinning earth axis!\n');
  gg=[90, 0, gc(3)-r_earth/g];
else
  gg(2)=atan2(gc(2),gc(1))/factor;
  r0=sqrt(sum(gc(1:2).*gc(1:2)));
  xi0=gc(3)/(r0*sqrt(g));
  xi_iter=r_earth*(g-1)/(g*r0);
  tanxi=xi0;
  tanxi=xi0+xi_iter*tanxi/sqrt(1+tanxi^2);
  tanxi=xi0+xi_iter*tanxi/sqrt(1+tanxi^2);
  gg(1)=atan(sqrt(g)*tanxi)/factor;
  gg(3)=sqrt(1+g*tanxi^2)*(r0-r_earth/sqrt(1+tanxi^2));
end;

