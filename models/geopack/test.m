RE=6371.2;PI=3.14159263;
iyear=2003;iday=1;ihour=0;min=0;isec=0;
alt=100;lat=69.6;lon=19.22;
alt=100;lat=55.6;lon=300.22;
alt=100;lat=70.6;lon=300.22;

r=(alt+RE)/RE;theta=(90.-lat)/180.*PI;phi=lon/180.*PI;
GEOPACK_RECALC (iyear,iday,ihour,min,isec);
[br,bt,bf] = GEOPACK_IGRF_GEO(r,theta,phi);disp([br,bt,bf]);
end
