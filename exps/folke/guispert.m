% guispert.m: special experiment specific hacks
% GUISDAP v8.2   08-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if a_code(1)==3
 name_ant='42m'; calTemp=163; ch_gain=10^4.52;
 if d_date>datenum(2009,9,10)
  ch_el=81.6; ch_az=184.5;
 else
  ch_el=81.6; ch_az=182.1;
 end
 ch_gain=10^4.52;
 glp=1436;
 grps=[1438 1438 lpg_h(1438);1439 2307 lpg_h(1439)+lpg_w(1439)/2
      1436 1437 lpg_h(1436)];
else
 glp=[1 563];
 grps=[2 4 lpg_h(4);5 562 lpg_h(5)+lpg_w(5)/2;1 1 lpg_h(1)
    564 566 lpg_h(566);567 1435 lpg_h(567)+lpg_w(567)/2;563 563 lpg_h(563)];
end
%aincorrect(glp,grps,9)
