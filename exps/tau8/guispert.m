% guispert.m: special experiment specific hacks
% GUISDAP v1.80   02-04-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
%ch_Pt=ch_Pt(1)/2;
if ch_Pt(1)==0
 polhv=[0;0]; hv=0;
else
 polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
 hv=max(roots(sum(polhv)));
end

if exist('sigpath') & strcmp(sigpath,'phased')
 [ch_el ch_az ch_gain]=vhf_elaz(90,12,10^4.31/2);
 d_data=d_data(54339+(1:54339));
 d_var1=d_var1(54339+(1:54339));
 d_var2=d_var2(54339+(1:54339));
 ch_Pt=polyval(polhv(2,:),hv)*1000;
else
 [ch_el ch_az ch_gain]=vhf_elaz(90,0,10^4.31/2);
 ch_Pt=polyval(polhv(1,:),hv)*1000;
end
clear polhv hv
