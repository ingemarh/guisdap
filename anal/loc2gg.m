% loc2gg.m:  transforms the scattering point location given in local coordinates
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% loc   [elevation, azimuth, range] at location
% site1 [latitude, longitude, height]  to geographic coordinates
% Thanks to J. Murgin (KGI Reprot NO 80:2), EISCAT analysis package, and P.Pollari
%
% In bistatic case, where another reference site (site2) is given
% the routine returns the scattering angle and ranges.
%
% See also: gg2gc, gc2gg, radar_eq
%
%function [gg_sp,angle,ranges]=loc2gg(site1,loc,site2)
function [gg_sp,angle,ranges]=loc2gg(site1,loc,site2)

factor=pi/180;        % conversion factor from degrees to radians
% earth radius (km) and flatness factor
r_earth=6378.135;
g=1.00673944;

% first calculate the  transformation matrices
sinlat=sin(site1(1)*factor);
coslat=cos(site1(1)*factor);
tanlat=tan(site1(1)*factor);
sinlon=sin(site1(2)*factor);
coslon=cos(site1(2)*factor);

rlocgc=  [ sinlat*coslon -sinlon coslat*coslon;
           sinlat*sinlon  coslon coslat*sinlon;
          -coslat            0      sinlat    ];

s1=loc(1);s2=loc(2);s3=loc(3);
loc=s3*[-cos(factor*s2)*cos(factor*s1),...
         sin(factor*s2)*cos(factor*s1),...
         sin(factor*s1)];

gc_site1=gg2gc(site1);          % Site1 to geogentric
gc_sp=gc_site1+(rlocgc*loc')';  % Add scattering distance in geocentric
gg_sp=gc2gg(gc_sp);             % Transform back to geographic

if nargin==3 % bistatic case
  gc_site2=gg2gc(site2);     % Site2 to geogentric
  gc_site1=gc_site1-gc_sp;   % Move origin to scattering point
  gc_site2=gc_site2-gc_sp;   % And same for site2
  ranges=[sqrt(sum(gc_site1.^2)), sqrt(sum(gc_site2.^2))];
  angle=pi-acos(sum(gc_site1.*gc_site2)./ranges(1)/ranges(2));
end
