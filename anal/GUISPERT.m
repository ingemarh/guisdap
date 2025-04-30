% GUISPERT.m: special hacks
% GUISDAP v8.1  03-04-30 Copyright EISCAT
%
% script for GUISPERTs and other power users. Put here any commands by
% which you wish to change the data before the analysis is started
%
% See also: GUIZARD GUIDITOR

% Correct the doppler sign
if (strcmp(name_ant(1:3),'vhf') & d_time(1,1)>1999) | ...
   (strcmp(name_ant(2:3),'2m') & d_date>datenum(2003,3,21,8,50,0) & d_date<datenum(2003,10,25)) | ...
   (strcmp(name_ant(2:3),'2m') & d_date>datenum(2009,11,1) & d_date<datenum(2010,1,1)) | ...
   (strcmp(name_ant(1:3),'kir') & d_date>datenum(2009,11,23) & d_date<datenum(2009,11,23,18,42,0)) | ...
   (strcmp(name_ant(1:3),'kir') & d_date>datenum(2013,1,1)) | ...
   (strcmp(name_ant(1:3),'sod') & d_date>datenum(2013,1,1))
  d_data=conj(d_data);
end
if ~exist('Magic_const')
 if strcmp(name_ant(1:3),'vhf')
  if d_date<datenum(2007,8,14)
   a_Magic_const=1.367;
  end
 elseif strcmp(name_ant(1:3),'uhf')
  if d_time(1,1)>2004
   a_Magic_const=0.80;
   if d_date>datenum(2009,10,26) % polariser back
    a_Magic_const=1.14;  %MTR
   end
   if d_date>datenum(2010,02,02) % polariser calibrated against Kiruna
    a_Magic_const=0.70;  %MTR
   end
  end
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
if contains('KSWD',name_site)
 if isempty(ch_range)
  global v_lightspeed
  if contains('WD',name_site)
    [gintersect,dbeam]=beam_intersect(p_XMITloc,d_parbl([62 61]),p_RECloc,[ch_az ch_el]);
    ch_height=gintersect(3);
    if dbeam(1)>1
       warning('GUISDAP:default',sprintf('Large distance between beams: %g km',dbeam(1)))
    end
  end
  if exist('ch_height','var')
    ch_range=height_to_range(ch_height,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
  else
    ch_range=height_to_range(300,ch_el(1))*(v_lightspeed*p_dtau*1e-6/2/1e3);
    warning('GUISDAP:default','range is dummy!')
  end
 end
 if contains('KS',name_site) & d_date>datenum(2013,1,1)
  ch_fradar(:)=radar_freqs(3);
 end
end

% Correct transmitter power when wrongly recorded
%d_secs=tosecs(d_time(1,:));
%if (name_site=='T' & d_time(1,1)==2001 & d_secs>27334800 & d_secs<25660800) 
% After 09:38 UT 9 Nov 2001 
%  Pt=Pt*2
%end

if name_site=='V' & d_date>datenum(2003,1,1)
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
end
if contains('3WD',name_site)
 if name_site=='3'
  if d_date<datenum(2023,1,1)
   ch_gain=10^4.3*sin(ch_el*pi/180)^2.5;
   ch_Pt=2e6;
  else
   ch_gain=10^4.6*sin(ch_el*pi/180)^2.5; %1 array + 1 element + 0.5 CETC
   ch_Pt=4.75e6;
  end
 else
  if name_site=='W'
   dir_ant=[90-20 221.86]*pi/180;
  else
   dir_ant=[90-20 159.35]*pi/180;
  end
  [dx,dy,dz]=sph2cart(-dir_ant(2),dir_ant(1),1); dir_ant=[dx dy dz];
  [dx,dy,dz]=sph2cart(-ch_az*pi/180,ch_el*pi/180,1); dir_beam=[dx dy dz];
  antel=atan2(norm(cross(dir_ant,dir_beam)),dot(dir_ant,dir_beam));
  ch_gain=10^4.3*sin(pi/2-antel)^2.5;
  ch_Pt=4.75e6;
  sysTemp=analysis_Tsys*(2-sin(ch_el*pi/180));
 end
 Tsky=20;
 [sun_az,sun_el]=solar_angle(p_RECloc(1),p_RECloc(2),d_date);
 [dx,dy,dz]=sph2cart(-sun_az*pi/180,sun_el*pi/180,1); dir_ant=[dx dy dz];
 [dx,dy,dz]=sph2cart(-ch_az*pi/180,ch_el*pi/180,1); dir_beam=[dx dy dz];
 antsun=atan2(norm(cross(dir_ant,dir_beam)),dot(dir_ant,dir_beam))*180/pi;
 antang=sqrt(2*pi/ch_gain)*180/pi;
 Tsky=d_parbl(59);
 Tsun=6000*exp(-(antsun/antang)^2);
 Tground=300/2*cos(ch_el*pi/180);
 sysTemp=analysis_Tsys+Tsky+Tsun+Tground;
 %sysTemp=analysis_Tsys*(2-sin(ch_el*pi/180)); % empiric based on background levels
end

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
