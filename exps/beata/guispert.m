% guispert.m: special experiment specific hacks
% GUISDAP v8.5   06-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
d_date=datenum(d_time(1,:));
if name_site=='T'
 if d_date<2007
  lpg_ra(1305:1306)=[25547 25563];
 end
end
if name_site=='L'
 glp=1182;
 grps=[1 1 lpg_h(1);2 1180 lpg_h(1)+lpg_w(1)/2
      1181 1182 lpg_h(1182)];
 gaincorrect(glp,grps)
end
