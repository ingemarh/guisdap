% guispert.m: special experiment specific hacks
% GUISDAP v1.70   01-11-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
global p_rep
ch_Pt=ch_Pt(1);
if name_site=='K' | name_site=='S'
  p_rep=312480;
  ch_height=292.9;
end
