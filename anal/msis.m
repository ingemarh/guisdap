function [d,texo]=msis(h,tim,loc,ap)
% function [d,texo]=msis(h,tim,loc,ap);
% h heights in m, default 200e3
% tim date as dayno secinday, default [180 43200]
% loc location, default [69.2 19.2]
% ap vector [f107a f107 ap], default [100 100 5];
% d matrix (h,[He O N2 O2 Ar H N mass T])

if nargin<4, ap=[]; end
if nargin<3, loc=[]; end
if nargin<2, tim=[]; end
if nargin<1, h=[]; end
if isempty(ap), ap=[100 100 5]; end
if isempty(loc), loc=[69.2 19.2]; end
if isempty(tim), tim=[180 43200]; end
if isempty(h), h=200e3; end
if ~libisloaded('libmsis')
 global path_GUP
 libdir=fullfile(path_GUP,'lib');
 loadlibrary(fullfile(libdir,'libmsis.so'),fullfile(libdir,'msis.h'))
end

nh=length(h);
it=round(tim);
stl=tim(2)/3600+loc(2)/15;
dens=libpointer('singlePtr',zeros(1,8));
t=libpointer('singlePtr',zeros(2,1));
dval=[1:5 7 8 6];
d=ones(nh,9)*NaN;

calllib('libmsis','meters_',1);
for i=1:nh
 calllib('libmsis','gtd7_',it(1),it(2),h(i)/1000,loc(1),loc(2),stl,ap(1),ap(2),ap(3),48,dens,t);
 d(i,:)=[dens.value(dval) t.value(2)];
end
texo=double(t.value(1));
