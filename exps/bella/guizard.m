% guizard.m: special experiment specific hacks
% GUISDAP v8.5   07-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ld=length(d_data);
if name_site=='V' & ld==2*14473 | (name_site=='K' | name_site=='S') & ld==2*4293
  lpg_rep(2)
end
