function B=geomag(loc,tim,iripath)
% function B=geomag(pos,tim)
% loc locations, default [69.2;19.2;100]
% tim date as [y m d h m s], default now
if nargin<3, global path_GUP, iripath=fullfile(path_GUP,'share','iri'); end
if nargin<2, tim=[]; end
if nargin<1, loc=[]; end
if isempty(loc), loc=[69.2;19.2;100]; end
if isempty(tim), tim=datevec(now); end

if libisloaded('libiri')

jf=libpointer('int32Ptr',ones(50,1));
jf.value([4 5 21 23 28:30 33:35 39])=0;
outf=libpointer('singlePtr',zeros(20,1000));
oarr=libpointer('singlePtr',zeros(100,1));
bn=libpointer('singlePtr',0);
be=libpointer('singlePtr',0);
bd=libpointer('singlePtr',0);
ba=libpointer('singlePtr',0);

iy=tim(1);
id=tim(2)*100+tim(3);
ut=tim(4)+tim(5)/60.+tim(6)/3600;

nh=size(loc,2);
B=ones(4,nh);

calllib('libiri','read_ig_rz_',int32(iripath),length(iripath));
calllib('libiri','readapf107_');

for i=1:nh
 calllib('libiri','iri_sub_',jf,0,loc(1,i),loc(2,i),iy,id,ut+25,loc(3,i),loc(3,i),1.,outf,oarr);
 calllib('libiri','feldg_',loc(1,i),loc(2,i),loc(3,i),bn,be,bd,ba);
 B(:,i)=[[be.value;bn.value;-bd.value]*1e-4;atan2(bd.value,2*sqrt(bn.value^2+be.value^2))];
end

else

magF=IGRF();
[secs,years]=tosecs(tim);
YEAR = years+secs/365/86400;
np=size(loc,2);
B=zeros(4,np);
degrad=pi/180;
for i=1:np
 xlat=loc(1,i); xlong=loc(2,i); HEIGHT=loc(3,i);
 [ ~,~,B(4,i),Bmag,magF ] = magF.IGRF_SUB( xlat,xlong,YEAR,HEIGHT );
 [ B(2,i),B(1,i),B(3,i),BABS ] = magF.FELDG( xlat,xlong,HEIGHT );
end
B=[B(1:2,:).*1e-4;-B(3,:).*1e-4;B(4,:)*degrad];

end
