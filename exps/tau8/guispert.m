% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if name_site=='V'
 if ch_Pt(1)==0
  polhv=[0;0]; hv=0;
 else
  polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
  hv=max(roots(sum(polhv)));
 end
 d_date=datenum(d_time(1,:));
 if d_date<datenum(2003,12,1,0,0,0)
  ch_el=90;
 end
 if fix(d_date)==fix(datenum(2002,02,17,0,0,0)) | fix(d_date)==fix(datenum(2007,07,19,0,0,0)) | fix(d_date)==fix(datenum(2007,08,09,0,0,0)) | fix(d_date)==fix(datenum(2007,08,10,0,0,0))
  ch_el=30;
 end
 if isempty(a_code) | length(a_code)==2
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
 elseif length(a_code)==1 & a_code==2
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),12,10^4.31/2);
  ch_Pt=polyval(polhv(2,:),hv)*1000;
 elseif length(a_code)==1 & a_code==1
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
  ch_Pt=polyval(polhv(1,:),hv)*1000;
 else
  error('No such analysis_code')
 end
 clear polhv hv
elseif name_site=='K'
 ch_gain=700*ones(size(ch_gain));
 ch_fradar=224e6*ones(size(ch_fradar));
end
