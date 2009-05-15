% GUISPERT.m: special hacks
% GUISDAP v8.1  03-04-30 Copyright EISCAT
%
% script for GUISPERTs and other power users. Put here any commands by
% which you wish to change the data before the analysis is started
%
% See also: GUIZARD GUIDITOR

% Correct the doppler sign
d_secs=tosecs(d_time(1,:));
if (strcmp(name_ant(1:3),'vhf') & d_time(1,1)>1999) | ...
   (strcmp(name_ant(2:3),'2m') & d_time(1,1)==2003 & d_secs>6857400 & d_secs<25660800)
  d_data=conj(d_data);
end
if ~exist('Magic_const')
 if strcmp(name_ant(1:3),'vhf')
  if d_date<datenum(2007,8,14)
   a_Magic_const=1.367;
  end
 elseif strcmp(name_ant(1:3),'uhf') & d_time(1,1)>2004
  a_Magic_const=0.80;
 elseif strcmp(name_ant(1:3),'42m')
  if d_date<datenum(2007,3,1)
   a_Magic_const=1.5;
  elseif d_date<datenum(2007,3,18)
   a_Magic_const=1.87;
  elseif d_date<datenum(2007,5,21)
   a_Magic_const=2.45; %AW
  elseif d_date<datenum(2007,10,15)
   a_Magic_const=1.59; %after snow clearence IH
  elseif d_date<datenum(2009,05,01)
   a_Magic_const=2.0;
  elseif d_date<datenum(2009,06,01)
   a_Magic_const=4.0;
  else
   a_Magic_const=1.5;
  end
 end
end
if strcmp(name_ant(1:3),'42m') & ~exist('ad_coeff_no_Pt','var')
 d=find(ch_gain~=ch_gain(1));
 % Only transmission affected for tx on 42m and rx on 32m
 ch_gain(d)=sqrt(a_Magic_const)*ch_gain(d);
end

% Range not always recorded
if isempty(ch_range) & (name_site=='K' | name_site=='S')
  global v_lightspeed
  if exist('ch_height','var')
    ch_range=height_to_range(ch_height,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
  else
    ch_range=height_to_range(300,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
    warning('GUISDAP:default','range is dummy!')
  end
end
% Correct transmitter power when wrongly recorded
%d_secs=tosecs(d_time(1,:));
%if (name_site=='T' & d_time(1,1)==2001 & d_secs>27334800 & d_secs<25660800) 
% After 09:38 UT 9 Nov 2001 
%  Pt=Pt*2
%end

if strcmp(name_ant(1:3),'uhf')
 if (d_date>=datenum(2000,10,01) & d_date<=datenum(2001,02,08)) | ...
    (d_date>=datenum(2001,06,28) & d_date<=datenum(2001,10,15,13,31,00)) 
     fprintf('GUISPERT: doubling uhf tx power estimate\n')
     ch_Pt=ch_Pt*2;
 elseif d_date>=datenum(2001,4,2) & d_date<=datenum(2001,4,10) 
% 2-10 April 2001, the UHF transmitter ran with only one klystron, but this was
% not reflected in the power values recorded in the parameter block.
% Should only affect the UP run on April 3-4.
  fprintf('GUISPERT: halving uhf tx power estimate\n')
  ch_Pt=ch_Pt/2;
 end
end
