% GUISPERT.m: special hacks - makes some more space available
% GUISDAP v1.81   03-01-30 Copyright EISCAT
%
% script for GUISPERTs and other power users. Put here any commands by
% which you wish to change the data before the analysis is started
%
% See also: GUIZARD
if isempty(ch_range) & (name_site=='K' | name_site=='S')
  global v_lightspeed
  if exist('ch_height','var')
    ch_range=height_to_range(ch_height,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
  else
    ch_range=height_to_range(300,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
    disp('Warning: range is dummy!')
  end
end
