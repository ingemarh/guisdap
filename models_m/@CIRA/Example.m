%% How to use the CIRA class
% This example shows various ways to use the CIRA class.

%% Section 1 Setting Up
% CIRA uses the Matlab class system and must be set up specifically the way
% Matlab is designed for.  The class must be in the @ folders.
% 
% mypath/OEIS.m
%
% mypath/CODATA2006.m
%
% mypath/@CIRA/CIRA.m
%
% mypath/@CIRA/GTD7.m
%
% ...
%
% Caution, these download links may break in the future but the procedure 
% remains the same.  The files can be downloaded and unzipped manually as well.
%
outputdir = 'mypath';
fexFiles = {'45604-cira-atmosphere','45544-oeis','45590-codata-2006'};

website = 'http://www.mathworks.com';
for i=1:length(fexFiles)
  url = sprintf('%s/matlabcentral/fileexchange/%s',website,fexFiles{i});
  entry=urlread(url);
  ptr1=strfind(entry,'"btn download"');
  ptr2=strfind(entry,'" itemprop="downloadUrl"');
  link = sprintf('%s%s',website,entry(ptr1+24:ptr2-1));
  unzip(link,outputdir);
end
addpath(outputdir);

%% Section 2 Running the Tests
% This example shows how to run all of the tests.  This will take a long time as
% they are exhaustive.  The tests are as follows:
% 
% # Test low altitude
% # Test medium altitude
% # Test high altitude
% # Test southern latitude
% # Test eastern longitude
% # Test earlier in day
% # Test plotting the variation in time of the pressure at a specific
% location
% 

atm=CIRA();
atm.run

%% Section 3 Calculating Pressure
% This is a simple example with no data overrides for New Year's Eve.
xlat = 40; % degrees
xlon = -104; % degrees
hxx = 0.0; % km
tjd = 2451545.0; % Julian day for 2000 Jan 1.5
[ iyd, sec, stl ] = CIRA.year_dayofyear( tjd, xlon );
f107a = 152.0; % Jansky
f107 = 154.0; % Jansky
ap = zeros(CIRA.maxAP,1); % magnetic index
ap(CIRA.DAILY_AP) = 4.1;
ap(CIRA.CURRENT_AP) = 4.2;
ap(CIRA.CURRENT_M_3_AP) = 4.3;
ap(CIRA.CURRENT_M_6_AP) = 4.4;
ap(CIRA.CURRENT_M_9_AP) = 4.5;
ap(CIRA.CURRENT_M_12_33_AP) = 4.6;
ap(CIRA.CURRENT_M_36_57_AP) = 4.7;
atm = CIRA();
mass = atm.MT(CIRA.ALL_MASS); % calculate all constituents
sw = CIRA.allSwitchesOn;
sw(CIRA.TURBO_SCALE_SW) = 0; % turn off turbo scale option
atm = atm.TSELEC(sw);
[ D,T,atm ] = atm.GTD7(iyd,sec,hxx,xlat,xlon,stl,f107a,f107,ap,mass);
press = atm.totalPressure(D,T);
disp(T(CIRA.TemperatureIndex(CIRA.Der0))-273.15); % 12.3561 C
disp(press); %  1.0173e+03 hPa


%% Section 4 Calculate Lapse Rate
% This is a simple example with data overrides for Spring Equinox.
xlat = 40; % degrees
xlon = -104; % degrees
hxx = 10.0; % km
YEAR = 2012;
DOY = 32; % Feb 1
sec = 43200.0; % noon
iyd = YEAR*CIRA.YRDSHIFT + DOY;
stl = sec/3600.0 + xlon/15.0;
f107a = 152.0; % Jansky
f107 = 154.0; % Jansky
ap = zeros(CIRA.maxAP,1); % magnetic index
ap(CIRA.DAILY_AP) = 4.1;
ap(CIRA.CURRENT_AP) = 4.2;
ap(CIRA.CURRENT_M_3_AP) = 4.3;
ap(CIRA.CURRENT_M_6_AP) = 4.4;
ap(CIRA.CURRENT_M_9_AP) = 4.5;
ap(CIRA.CURRENT_M_12_33_AP) = 4.6;
ap(CIRA.CURRENT_M_36_57_AP) = 4.7;
atm = CIRA();
mass = atm.MT(CIRA.ALL_MASS); % calculate all constituents
sw = CIRA.allSwitchesOn;
atm = atm.TSELEC(sw);
[ D,T,atm ] = atm.GTD7(iyd,sec,hxx,xlat,xlon,stl,f107a,f107,ap,mass);
dpress = atm.totalPressureGradient(D,T);
disp(T(CIRA.TemperatureIndex(CIRA.DerAlt))); % -4.0992 C/km
disp(D(CIRA.DensityIndex(CIRA.DerAlt,CIRA.ALL_MASS))); % -5.4971e-05 g/cm^3/km
disp(dpress); %  -40.1322 hPa/km
