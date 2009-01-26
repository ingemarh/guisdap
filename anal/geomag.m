function B=geomag(pos,r_time)
%function B=geomag(pos,r_time)
degrad=pi/180;
r_earth=6378.135; % earth radius (km)
iday=(r_time(2)-1)*30+r_time(3);
GEOPACK_RECALC(r_time(1),iday,r_time(4),r_time(5),r_time(6));
r=pos(3,:)/r_earth+1;
theta=(90.-pos(1,:))*degrad;
phi=pos(2,:)*degrad;
np=size(pos,2);
B=zeros(4,np);
% BR, BTHETA, BPHI - SPHERICAL COMPONENTS OF THE MAIN
% GEOMAGNETIC FIELD IN NANOTESLA
% (POSITIVE BR OUTWARD, BTHETA SOUTHWARD, BPHI EASTWARD)
for i=1:np
 [br,bt,bf]=GEOPACK_IGRF_GEO(r(i),theta(i),phi(i));
 B(1:3,i)=[bf;-bt;br]*1e-9; %[E N U] Tesla
end
B(4,:)=atan(B(3,:)/2../sqrt(sum(B(1:2,:).^2))); %dip latitude
