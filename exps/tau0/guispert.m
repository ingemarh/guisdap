% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);

lp=366;
b0=d_data((2:lp)+15159);
b1=d_data((2:lp)+30696);
g=mean([gainfit(b0,10) gainfit(b1,10)],2); g=[g(1);g];
g0=conv(ones(45,1),g)./[1:45 45*ones(1,lp-45-1) 45:-1:1]';

s1=0;
d_data((1:lp)+s1)=d_data((1:lp)+s1)./g;
s1=s1+lp;
for l=1:26
 addr=s1+(1:lp-l); s1=addr(end);
 d_data(addr)=d_data(addr)./g0(1+l:lp);
end
d_data((1:lp)+15159)=d_data((1:lp)+15159)./g;

s1=15537;
d_data((1:lp)+s1)=d_data((1:lp)+s1)./g;
s1=s1+lp;
for l=1:26
 addr=s1+(1:lp-l); s1=addr(end);
 d_data(addr)=d_data(addr)./g0(1+l:lp);
end
d_data((1:lp)+30696)=d_data((1:lp)+30696)./g;
