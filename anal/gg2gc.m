% gg2gc: transforms coordinates from geographic (lat, lon, h) to geocentric
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Thanks to J. Murgin (KGI Reprot NO 80:2), EISCAT analysis package, and P.Pollari
%
% See also: gc2gg, loc2gg
%
% function gc=gg2gc(gg)
function gc=gg2gc(gg)

factor=pi/180;        % conversion factor from degrees to radians
% earth radius (km) and flatness factor
r_earth=6378.135;
g=1.00673944;

lat=gg(1)*factor;
lon=gg(2)*factor;
h=gg(3);

hor=(r_earth/sqrt(1+tan(lat)^2/g)+h*cos(lat));
gc=[hor*[cos(lon), sin(lon)], r_earth/sqrt((g+g^2/tan(lat)^2))+h*sin(lat)];
