% guispert.m: special experiment specific hacks
% GUISDAP v1.70   01-11-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if strcmp(name_ant(1:3),'vhf')
 d_date=datenum(d_time(1,:));
 if d_date>datenum(2000,01,01,0,0,0) & d_date<datenum(2000,05,25,0,0,0) 
%   Two klystrons but only ca. 1.3 MW in parameter block- wrong
    polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
    hv=max(roots(sum(polhv)));
    if d_date>datenum(2000,05,20,0,0,0) & d_date<datenum(2000,05,20,23,0,0), hv=67; end
    if d_date>datenum(2000,05,20,12,18,0) & d_date<datenum(2000,05,20,12,41,0), hv=66; end
    if d_date>datenum(2000,05,20,12,03,0) & d_date<datenum(2000,05,20,12,18,0), hv=71; end
    fprintf(' hv= %g\n',hv)
    fprintf('GUISPERT: fixing vhf tx power estimate by roughly doubling power\n')
    ch_Pt=(polyval(polhv(1,:),hv)+polyval(polhv(2,:),hv))*1000;
 end
 if d_date>datenum(2000,11,08,0,0,0) & d_date<datenum(2000,11,08,23,0,0) 
%   Klystron A only on this day. Logbook says 77-80 kV.
     polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
     fprintf('GUISPERT: fixing vhf tx power estimate using 78kV\n')
     ch_Pt=polyval(polhv(1,:),78)*1000;
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
