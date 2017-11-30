% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if length(d_data)==2*6352
  calTemp=[163 163 228 228];
  if all(ch_gain==ch_gain(1))
    if isempty(a_code)
      a_satch.clutter=repmat(a_satch.clutter,1,2);
      a_satch.repair=repmat(a_satch.repair,1,2);
    end
    ch_gain(3:4)=ch_gain(3:4)*0.78; % for 4 march 2007
  end
end
