% GUIDITOR.m: script for changing of the result parameters 
% GUISDAP v.8.3 03-09-27 Copyright EISCAT
%
% Put here any commands by
% which you wish to change the results before saving the data
%
% See also: GUISPERT
%
if name_site=='S' & d_time(1,1)==2003 & tosecs(d_time(1,:))>22809600 & tosecs(d_time(2,:))<23037600
  global sc_angle
  %correct a 10 kHz LO2 error in sept 2003 at Sodankyla
  r_param(:,5)=r_param(:,5)-10e3*v_lightspeed/ch_fradar(1)/2*sin(sc_angle/2);
end

