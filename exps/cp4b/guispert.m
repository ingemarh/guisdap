% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if length(a_code)==1
 polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
 hv=max(roots(sum(polhv)));
 ch_Pt=polyval(polhv(a_code,:),hv)*1000;
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(a_code),12*(a_code-1),10^4.31/2);
else
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el,0,10^4.31/2);
end
