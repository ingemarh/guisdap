% GUISPERT.m: special hacks - makes some more space available
% GUISDAP v8.1  03-04-30 Copyright EISCAT
%
% script for GUISPERTs and other power users. Put here any commands by
% which you wish to change the data before the analysis is started
%
% See also: GUIZARD GUIDITOR

% Correct the doppler sign
d_secs=tosecs(d_time(1,:));
if (name_site=='V' & d_time(1,1)>2000) | ...
   (name_site=='L' & d_time(1,1)==2003 & d_secs>6857400 & d_secs<25660800)
  d_data=conj(d_data);
end

% Range not always recorded
if isempty(ch_range) & (name_site=='K' | name_site=='S')
  global v_lightspeed
  if exist('ch_height','var')
    ch_range=height_to_range(ch_height,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
  else
    ch_range=height_to_range(300,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
    disp('Warning: range is dummy!')
  end
end
% Correct transmitter power when wrongly recorded
%d_secs=tosecs(d_time(1,:));
%if (name_site=='T' & d_time(1,1)==2001 & d_secs>27334800 & d_secs<25660800) 
% After 09:38 UT 9 Nov 2001 
%  Pt=Pt*2
%end

  % The 1 March date in the next if statement is still a guess. After Feb 6 but before March 9.

    if (name_site=='T' & ...
         ( datenum(d_time(1,1),d_time(1,2),d_time(1,3)) >= datenum(2000,05,01) & ...
           datenum(d_time(1,1),d_time(1,2),d_time(1,3)) <= datenum(2001,03,01)   ) ...
       | ...
         ( datenum(d_time(1,1),d_time(1,2),d_time(1,3)) >= datenum(2001,06,28) & ...
           datenum(d_time(1,1),d_time(1,2),d_time(1,3),d_time(1,4),d_time(1,5),d_time(1,6)) ...
                                                        <= datenum(2001,10,15,13,31,00) )... 
       )

      fprintf('GUISPERT: doubling uhf tx power estimate\n')
      ch_Pt =ch_Pt*2;
  
  % Between April 2 and April 10 2001, the UHF transmitter ran with only one klystron,
  % but this was not reflected in the power values recorded in the parameter block.
  % Should only affect the UP run on April 3-4.

    elseif datenum(d_time(1,1),d_time(1,2),d_time(1,3)) >= datenum(2001,4,2) ...
          & datenum(d_time(1,1),d_time(1,2),d_time(1,3)) <= datenum(2001,4,10) 
      fprintf('GUISPERT: halving uhf tx power estimate\n')
      ch_Pt = ch_Pt / 2.0;
    end  

