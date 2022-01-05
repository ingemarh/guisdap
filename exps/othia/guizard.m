% guizard.m: special experiment specific hacks
% GUISDAP v8.5   07-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ld=length(d_data);
if name_site=='V' || name_site=='L' && ld/2>=max(lpg_ra)+ADDR_SHIFT
  lpg_rep(2)
end
