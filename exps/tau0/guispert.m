% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
%ch_Pt=ch_Pt(1);
d_date=datenum(d_time(1,:));
if d_date<datenum(2002,6,1)
 %data dump changes with integration time
 shift=length(d_data)-32034;
 if lpg_ra(478)==15537
   lpg_ra(478:end)=lpg_ra(478:end)+shift;
   form_adpar
 end
else
 shift=0;
end


lp=366;

g=gainfit(mean([d_data((2:lp)+15159) d_data((2:lp)+30696+shift)],2),10);
g=[g(1)-diff(g(1:2));g]/g(end);
g0=conv(ones(45,1),g)./[1:45 45*ones(1,lp-45-1) 45:-1:1]';

s1=0;
d_data((1:lp)+s1)=d_data((1:lp)+s1)./g;
s1=s1+lp;
for l=1:26
 addr=s1+(1:lp-l); s1=addr(end);
 d_data(addr)=d_data(addr)./g0(1+l:lp);
end
d_data((1:lp)+15159)=d_data((1:lp)+15159)./g;

s1=15537+shift;
d_data((1:lp)+s1)=d_data((1:lp)+s1)./g;
s1=s1+lp;
for l=1:26
 addr=s1+(1:lp-l); s1=addr(end);
 d_data(addr)=d_data(addr)./g0(1+l:lp);
end
