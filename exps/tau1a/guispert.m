% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-10-30 Copyright EISCAT
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

d_date=datenum(d_time(1,:));
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
 ch_Pt=polyval(polhv(2,:),hv)*1000;
elseif length(a_code)==1 & a_code==1
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
 ch_Pt=polyval(polhv(1,:),hv)*1000,
else
 error('No such analysis_code')
end
clear polhv hv
