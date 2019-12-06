% guizard.m: special experiment specific hacks
% GUISDAP v8.5   07-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if name_site=='L' & length(d_data)>290942 | name_site=='V' & expver>1
  lpg_rep(2)
end
