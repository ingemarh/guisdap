% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if a_code(1)==3
 name_ant='42m'; ch_el=81.6; ch_az=182.1; calTemp=163;
 ch_gain=10^4.52;
 glp=[932 1295];
 grps=[934 934 lpg_h(934);935 1294 lpg_h(935)+lpg_w(935)/2
      932 933 lpg_h(932)
      1297 1297 lpg_h(1297);1298 1862 lpg_h(1298)+lpg_w(1298)/2
      1295 1296 lpg_h(1295)];
else
 glp=[928 930];
 grps=[1 1 lpg_h(1);2 361 lpg_h(2)+lpg_w(2)/2
      928 931 lpg_h(928)
      362 362 lpg_h(362);363 927 lpg_h(363)+lpg_w(363)/2];
end
gaincorrect(glp,grps)
