function B=igeomag(pos,r_time)
%function B=geomag(pos,r_time)
magF=IGRF();
[secs,years]=tosecs(r_time);
YEAR = years+secs/365/86400;
np=size(pos,2);
B=zeros(4,np);
degrad=pi/180;
for i=1:np
 xlat=pos(1,i); xlong=pos(2,i); HEIGHT=pos(3,i);
 [ ~,~,B(4,i),Bmag,magF ] = magF.IGRF_SUB( xlat,xlong,YEAR,HEIGHT );
 [ B(2,i),B(1,i),B(3,i),BABS ] = magF.FELDG( xlat,xlong,HEIGHT );
end
B=[B(1:3,:).*1e-4;B(4,:)*degrad];
