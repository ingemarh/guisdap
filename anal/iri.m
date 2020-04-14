function m_iri=iri(par,tim,loc,h)
% function m_iri=iri(par,tim,loc,h)
% par parameter list, default [1 4 3]
% tim date as [tsec year], default [15552090 1996]
% loc location, default [69.2 19.2]
% h heights in km, default [100 590 0]
if nargin<4, h=[]; end
if nargin<3, loc=[]; end
if nargin<2, tim=[]; end
if nargin<1, par=[]; end
if isempty(h), h=[100 590 0]; end
if length(h)<3 || h(3)==0, h(3)=diff(h(1:2))/99; end
if isempty(loc), loc=[69.2 19.2]; end
if isempty(tim), tim=[15552090 1996]; end
if isempty(par), par=[1 4 3]; end
if ~libisloaded('libiri')
 global path_GUP
 libdir=fullfile(path_GUP,'lib');
 loadlibrary(fullfile(libdir,'libiri.so'),fullfile(libdir,'iri.h'))
end

jf=libpointer('int32Ptr',ones(50,1));
jf.value([4 5 21 23 28:30 33:35 39])=0;
id=fix(-tim(1)/86400);
ut=tim(1)/3600+id*24;
iy=round(tim(2));
nh=diff(h(1:2))/h(3)+1;
if nh>500, error('Max 500 heights'), end
outf=libpointer('singlePtr',zeros(20,1000));
oarr=libpointer('singlePtr',zeros(100,1));

calllib('libiri','read_ig_rz_');
calllib('libiri','readapf107_');
calllib('libiri','iri_sub_',jf,0,loc(1),loc(2),iy,id-1,ut+25,h(1),h(2),h(3),outf,oarr);

m_iri=double(outf.value(par,1:nh)');
if find(par==12)
 m_iri(:,find(par==12))=h(1)+(0:nh-1)'*h(3);
end
m_iri(find(m_iri==-1))=NaN;
