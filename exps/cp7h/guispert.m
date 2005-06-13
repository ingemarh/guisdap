% guispert.m: special experiment specific hacks
% GUISDAP v1.70   01-11-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if strcmp(name_ant(1:3),'vhf')
 d_date=datenum(d_time(1,:));
 polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
 if d_date>datenum(2000,01,01,0,0,0) & d_date<datenum(2000,05,25,0,0,0) 
%  Two klystrons but only ca. 1.3 MW in parameter block- wrong
  hv=max(roots(sum(polhv)));
  if d_date>datenum(2000,05,20,0,0,0) & d_date<datenum(2000,05,20,23,0,0), hv=67; end
  if d_date>datenum(2000,05,20,12,18,0) & d_date<datenum(2000,05,20,12,41,0), hv=66; end
  if d_date>datenum(2000,05,20,12,03,0) & d_date<datenum(2000,05,20,12,18,0), hv=71; end
  if d_date>datenum(2000,05,22,8,0,0) & d_date<datenum(2000,05,23,23,0,0)
   %read hv from txlog
   if ~exist('txlog','var')
    txlog=get_tx('V0500.txt',[53]);
    d=find(txlog(:,2)<25); txlog(d,2)=25; %ch_Pt<0
   end
   hv=interp1(txlog(:,1),txlog(:,2),mean(datenum(d_time)),'nearest');
  end
  fprintf('GUISPERT: fixing vhf tx power estimate using %.0fkV\n',hv)
  ch_Pt=(polyval(polhv(1,:),hv)+polyval(polhv(2,:),hv))*1000;
 end
 if d_date>datenum(2000,11,08,0,0,0) & d_date<datenum(2000,11,08,23,0,0) 
% Klystron A only on this day. Logbook says 77-80 kV.
  fprintf('GUISPERT: fixing vhf tx power estimate using 79kV\n')
  ch_Pt=polyval(polhv(1,:),79)*1000;
 end
end


%lpg_bac(find(lpg_bcs=='s'))=0;

%dsplit=27405;
dsplit=0;
d_data(1:dsplit)=mean([d_data(1:dsplit) d_data((1:dsplit)+dsplit)],2);
d_var1(1:dsplit)=mean([d_var1(1:dsplit) d_var1((1:dsplit)+dsplit)],2);
d_var2(1:dsplit)=mean([d_var2(1:dsplit) d_var2((1:dsplit)+dsplit)],2);

d_data(dsplit+(1:dsplit))=d_data(1:dsplit);
d_var1(dsplit+(1:dsplit))=d_var1(1:dsplit);
d_var2(dsplit+(1:dsplit))=d_var2(1:dsplit);
