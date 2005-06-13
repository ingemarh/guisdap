% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if ch_el(2)==0, ch_el(2)=ch_el(1); end
d_date=datenum(d_time(1,:));
if length(a_code)==1
 if d_date<datenum(2001,03,18) | ( d_date>datenum(2001,09,18) & d_date<datenum(2002,10,04) ) 
  polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
  hv=max(roots(sum(polhv)));
  ch_Pt=polyval(polhv(a_code,:),hv)*1000;
 end
 if d_date<datenum(2000,12,31) & d_date>datenum(2000,11,23)
  %channels are reversed in early data
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(3-a_code),12*(2-a_code),10^4.31/2); 
 else
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(a_code),12*(a_code-1),10^4.31/2);
 end
else
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el,0,10^4.31/2);
end
