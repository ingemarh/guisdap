function [d,texo]=msis(h,tim,loc,ap)
% function [d,texo]=msis(h,tim,loc,ap);
% h heights in m, default 200e3
% tim date as dayno secinday, default [180 43200]
% loc location, default [69.2 19.2]
% ap vector [f107a f107 ap], default [100 100 5];
% d matrix [He O N2 O2 Ar H N mass T]
% texo exospheric temperature

if nargin<4, ap=[]; end
if nargin<3, loc=[]; end
if nargin<2, tim=[]; end
if nargin<1, h=[]; end
if isempty(ap), ap=[100 100 5]; end
if isempty(loc), loc=[69.2 19.2]; end
if isempty(tim), tim=[180 43200]; end
if isempty(h), h=200e3; end

if libisloaded('libmsis')

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

else

xlat=loc(1); xlon=loc(2);
iyd=tim(1); sec=tim(2);
stl=sec/3600+xlon/15;
hxx=h/1000.;
f107a=ap(1); f107=ap(2); ap=ap(3);

%ap = zeros(CIRA.maxAP,1); % magnetic index
ap(CIRA.DAILY_AP) = ap;
%ap(CIRA.CURRENT_AP) = 4.2;
%ap(CIRA.CURRENT_M_3_AP) = 4.3;
%ap(CIRA.CURRENT_M_6_AP) = 4.4;
%ap(CIRA.CURRENT_M_9_AP) = 4.5;
%ap(CIRA.CURRENT_M_12_33_AP) = 4.6;
%ap(CIRA.CURRENT_M_36_57_AP) = 4.7;
atm = CIRA();
mass = atm.MT(CIRA.ALL_MASS); % calculate all constituents
sw = CIRA.allSwitchesOn;
sw(CIRA.TURBO_SCALE_SW) = 0; % turn off turbo scale option
atm = atm.TSELEC(sw);
atm.METERS(1);
d=NaN*ones(length(hxx),9); i=0;
for h=row(hxx)
 i=i+1;
 [ D,T,atm ] = atm.GTD7(iyd,sec,h,xlat,xlon,stl,f107a,f107,ap,mass);
 d(i,:)=[D([1 2 3 4 5 7 8 6])' T(2)];
end
texo=[T(1)];

end
