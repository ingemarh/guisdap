% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
%lpg_bcs([1417 1927])='x';
if a_code(1)==1
 name_ant='42m'; calTemp=163;
 if d_date>datenum(2019,10,09)
  ch_el=82.1; ch_az=185.5;
 elseif d_date>datenum(2016,12,16)
  ch_el=81.6; ch_az=185.5;
 elseif d_date>datenum(2009,9,10)
  ch_el=81.6; ch_az=184.5;
 else
  ch_el=81.6; ch_az=182.1;
 end
 ch_gain=10^4.52;
end
