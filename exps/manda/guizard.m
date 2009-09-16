% guizard.m: special experiment specific hacks
% GUISDAP v8.5   07-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if length(d_data)==39061  | length(d_data)==57166
  lpg_cut(find(lpg_h*.15>300))
end
