% guispert.m: special experiment specific hacks
% GUISDAP v1.70   01-11-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
global p_rep
ch_Pt=ch_Pt(1);
if name_site=='K' | name_site=='S'
  p_rep=312480;
  ch_height=292.9;
end
d_date=datenum(d_time(1,:));
if d_date>datenum(2000,09,06,12,0,0) & d_date<datenum(2000,09,06,23,0,0)
 polhv=[1.7980 -178.1795 4508.4];
 %read hv from txlog
 if ~exist('txlog','var')
  txlog=get_tx('U0900.txt',[52]);
  d=find(txlog(:,2)<25); txlog(d,2)=25; %ch_Pt<0
 end
 hv=interp1(txlog(:,1),txlog(:,2),mean(datenum(d_time)),'nearest');
 ch_Pt=polyval(polhv,hv)*1000;
 fprintf('GUISPERT: HV=%g kV -> TX=%.1f MW\n',hv,ch_Pt/1e6)
end

if name_site=='K' && d_date>datenum(2001,04,02) & d_date<datenum(2001,06,25)
 ch_Pt=ch_Pt(1)/2.0;  % one klystron but 2 assumed.
end
