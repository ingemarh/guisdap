function m_iri=iri(par,tim,loc,h,iripath)
% function m_iri=iri(par,tim,loc,h)
% par parameter list, default [1 4 3]
%   	 fields 1  electron density/m-3
%   		2  neutral temperature/K
%   		3  ion temperature/K
%   		4  electron temperature/K
%   		5  O+ density/m-3
%   		6  H+ densitY/m-3
%   		7  He+ density/m-3
%   		8  O2+ density/m-3
%   		9  NO+ density/m-3
%              12  heights
% tim date as [tsec year], default [15552090 1996]
% loc location, default [69.2 19.2]
% h heights limits and step in km, default [100 590 0] max 100 heights
%*********************************************
%* Altitude limits: lower (day/night) upper  *
%* electron density      60/80 km    1000 km *
%* temperatures            120 km    3000 km *
%* ion densities           100 km    1000 km *
%*********************************************
if nargin<5, global path_GUP, iripath=fullfile(path_GUP,'share','iri'); end
if nargin<4, h=[]; end
if nargin<3, loc=[]; end
if nargin<2, tim=[]; end
if nargin<1, par=[]; end
if isempty(h), h=[100 590 0]; end
if length(h)<3 || h(3)==0, h(3)=diff(h(1:2))/99; end
if isempty(loc), loc=[69.2 19.2]; end
if isempty(tim), tim=[15552090 1996]; end
if isempty(par), par=[1 4 3]; end
nh=round(diff(h(1:2))/h(3))+1;

if libisloaded('libiri')

jf=libpointer('int32Ptr',ones(50,1));
jf.value([4 5 21 23 28:30 33:35 39])=0;
id=fix(-tim(1)/86400);
ut=tim(1)/3600+id*24;
iy=round(tim(2));
if nh>500, error('Max 500 heights'), end
outf=libpointer('singlePtr',zeros(20,1000));
oarr=libpointer('singlePtr',zeros(100,1));

calllib('libiri','read_ig_rz_',int32(iripath),length(iripath));
calllib('libiri','readapf107_');
calllib('libiri','iri_sub_',jf,0,loc(1),loc(2),iy,id-1,ut+25,h(1),h(2),h(3),outf,oarr);

m_iri=double(outf.value(par,1:nh)');
d=find(par==12);
if d
 m_iri(:,d)=h(1)+(0:nh-1)'*h(3);
end
m_iri(find(m_iri==-1))=NaN;

else

iri_m=IRI2012();
JF = IRI2012.defaultIRIswitches();
JF(IRI2012.MESSAGES_ON_SW)=false;

JMAG = IGRF.GEOGRAPHIC_COORDINATES;
id=-tim(1)/86400.;
ut=tim(1)/3600.+id*24;
iy=round(tim(2));
[outf,oarr,iri_m] = iri_m.IRI_SUB(JF,JMAG,loc(1),loc(2),iy,id-1,ut+25,h(1),h(2),h(3));
m_iri=outf(par,:)';
d=find(par==12); if length(d)==1, m_iri(:,d)=h(1)+h(3)*(0:nh-1)'; end
d=find(m_iri==-1); if length(d), m_iri(d)=NaN; end

end
