% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
%ch_Pt=ch_Pt(1)/2;
d_date=datenum(d_time(1,:));
if ch_Pt(1)==0
 polhv=[0;0]; hv=0;
else
 polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
 hv=max(roots(sum(polhv)));
end
if d_date>datenum(2002,8,12,0,0,0) & d_date<datenum(2002,8,18,0,0,0)
 %read hv from txlog
 if ~exist('txlog','var')
  txlog=get_tx('VAB0802a.txt',[53]);
  d=find(txlog(:,2)<25); txlog(d,2)=25; %ch_Pt<0
 end
 hv=interp1(txlog(:,1),txlog(:,2),mean(datenum(d_time)),'nearest');
 polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23];
end
if d_date<datenum(2001,12,1,0,0,0)
 ch_el=30;
end
if d_date>datenum(2001,3,9,0,0,0) & d_date<datenum(2001,3,18,0,0,0) & lpg_ra(438)==20304
 %data dump changed due to error
 lpg_ra(438:end)=lpg_ra(438:end)+20;
 lpg_ra(440:end)=lpg_ra(440:end)+20;
 lpg_ra(877:end)=lpg_ra(877:end)+20;
 form_adpar
 a_satch.lpg_skip=438:length(lpg_ra);
end
if length(d_data)<20872
 a_code=1;
elseif isempty(a_code) | length(a_code)==2
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
elseif length(a_code)==1 & a_code==2
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),12,10^4.31/2);
 if (d_date>=datenum(2001,09,17,12,0,0) & d_date<=datenum(2001,09,20,15,0,0))
  % 17-20 September 2001 - tau1v (really tau1a)
  % during this period the uhf and vhf were run together in a pseudo-cp2 mode
  % uhf was pointed az=90 el=75 i.e. east
  % vhf beam 1 (west panel, cp4 boresight beam) was pointed az=360 (boresight) el 90 (vertical)
  % vhf beam 2 (east panel, cp4 west beam) was pointed az=360 (boresight) el 75 (north)
     [ch_el ch_az ch_gain]=vhf_elaz(75,0,10^4.31/2);
 end
 if d_date>datenum(2002,8,19,7,0,0) & d_date<datenum(2002,10,04,0,0,0)
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2); 
 end
 ch_Pt=polyval(polhv(2,:),hv)*1000;
elseif length(a_code)==1 & a_code==1
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
 if (d_date>=datenum(2001,09,17,12,0,0) & d_date<=datenum(2001,09,20,15,0,0))
  % 17-20 September 2001 - tau1v (really tau1a)
  % during this period the uhf and vhf were run together in a pseudo-cp2 mode
  % uhf was pointed az=90 el=75 i.e. east
  % vhf beam 1 (west panel, cp4 boresight beam) was pointed az=360 (boresight) el 90 (vertical)
  % vhf beam 2 (east panel, cp4 west beam) was pointed az=360 (boresight) el 75 (north)
     [ch_el ch_az ch_gain]=vhf_elaz(90,0,10^4.31/2);
 end
 if d_date>datenum(2002,8,19,7,0,0) & d_date<datenum(2002,10,04,0,0,0)
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),3,10^4.31/2);
 end
 if d_date<datenum(2002,11,04,0,0,0) | d_date>datenum(2002,11,05,06,0,0) % whole antenna to side-a  
   ch_Pt=polyval(polhv(1,:),hv)*1000;
 end
else
 error('No such analysis_code')
end
clear polhv hv
