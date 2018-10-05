%% How to use the IGRF class
% This example shows various ways to use the IGRF class.

%% Section 1 Setting Up
% IGRF uses the Matlab class system and must be set up specifically the way
% Matlab is designed for.  The class must be in the @ folders.
% 
% mypath/OEIS.m
%
% mypath/@WGS84/WGS84.m
%
% ...
%
% mypath/@IGRF/IGRF.m
%
% mypath/@IGRF/IGRF_SUB.m
%
% ...
%
% Caution, these download links may break in the future but the procedure 
% remains the same.  The files can be downloaded and unzipped manually as well.
%
outputdir = 'mypath';
fexFiles = {'45603-wgs84-earth-shape','45606-igrf-magnetic-field','45544-oeis'};

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
% This example shows how to run all of the tests.  The tests are as follows:
% 
% # Test plotting magnitude of field at one location for all available dates
% 

magF=IGRF();
magF.run

%% Section 3 Calculate Magnetic Field Strength
% This is a simple example for calculating dip latitude and magnetic field magnitude.
magF=IGRF();
xlat = 40; % geodetic latitude degrees North
xlong = -104; % longitude degrees East
HEIGHT = 0.0; % geodetic altitude (km)
YEAR = 2012.3; % decimal years
[ ~,~,dipl,Bmag,magF ] = magF.IGRF_SUB( xlat,xlong,YEAR,HEIGHT );
dipl % dip latitude 49.4273 degrees
Bmag % 0.5295 Gauss

%% Section 4 Calculate magnetic field vector
% This is a simple example to calculate the magnetic field vector
magF=IGRF();
xlat = 40; % geodetic latitude degrees North
xlong = -104; % longitude degrees East
HEIGHT = 0.0; % geodetic altitude (km)
[X,Y,Z] = IGRF.GEODETIC2CARTESIAN(xlat,xlong,HEIGHT); % km
YEAR = 2012.3; % decimal years
[~,magF] = magF.FELDCOF(YEAR);
B = magF.FELDC( [X,Y,Z] / magF.ERA );
B % [0.1515    0.4832   -0.1549] Gauss

