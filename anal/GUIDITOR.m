% GUIDITOR.m: script for changing of the result parameters 
% GUISDAP v.8.3 03-09-27 Copyright EISCAT
%
% Put here any commands by
% which you wish to change the results before saving the data
%
% See also: GUISPERT
%
if name_site=='S' & d_time(1,1)==2003 & tosecs(d_time(1,:))>22809600 & tosecs(d_time(2,:))<23037600
  %correct a 10 kHz LO2 error in sept 2003 at Sodankyla
  r_phasepush=10e3;
end
if exist('a_phasepush','var')
  r_phasepush=a_phasepush;
end
if isempty(r_phasepush) & d_date>datenum(2009,1,23) & ~isempty(strfind('KST',name_site))
  r_phasepush=-21;
end
if ~isempty(r_param) & ~isempty(r_phasepush)
  r_param(:,5)=r_param(:,5)-r_phasepush*v_lightspeed/ch_fradar(1)/2*sin(sc_angle/2);
end
