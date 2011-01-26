% guizard.m: special experiment specific hacks
% GUISDAP v8.5   07-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ld=length(d_data);
if ld==39061 | ld==57166
  lpg_cut(find(lpg_h*.15>300))
elseif ld==2*69400
  lpg_rep(2)
end
