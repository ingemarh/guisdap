%% How to use the IRI2012 class
% This example shows various ways to use the IRI2012 class.

%% Section 1 Setting Up
% IRI2012 uses the Matlab class system and must be set up specifically the way
% Matlab is designed for.  The class must be in the @ folders.
% 
% mypath/@IRI2012/IRI2012.m
%
% mypath/@IRI2012/IRI_SUB.m
%
% ...
%
% mypath/OEIS.m
%
% mypath/CODATA2006.m
%
% mypath/@WGS84/WGS84.m
%
% ...
%
% mypath/@CIRA/CIRA.m
%
% mypath/@CIRA/GTD7.m
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
fexFiles = {'45612-iri-2012','45603-wgs84-earth-shape','45604-cira-atmosphere',...
  '45606-igrf-magnetic-field','45544-oeis','45590-codata-2006'};

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
% # Known conditions, known output
% # Test for the production of NaN's during a call to every possible option, on
% and off
% # Test plotting the variation in time of the electron density at a specific
% location
% 

iri=IRI2012();
iri.run

%% Section 3 Calling Main Interface
% This is a simple example with no data overrides for New Year's Eve.
iri=IRI2012();
JF = IRI2012.defaultIRIswitches();
JF(IRI2012.AUR_BOUND_MODEL_SW) = false; % don't need aurora
JMAG = IGRF.GEOGRAPHIC_COORDINATES;
YEAR = 2012;
MMDD = 101; % Jan 1
ALATI = 40; % geodetic latitude (degrees North)
ALONG = -104; % longitude (degrees East) 
DHOUR = 0.0+IRI2012.UT_INDICATOR; % midnight UTC
ALT = 150.0; % geodetic altitude (km)
[outf,oarr,iri] = iri.IRI_SUB(JF,JMAG,ALATI,ALONG,YEAR,...
          MMDD,DHOUR,ALT);
% electron density (6.9827e+08 electrons/m^3)
eldens = outf(IRI2012.EL_DENS_OUT,1);
% altitude of maximum electon density in F2 region (296.1204 km)
heightMaxF2 = oarr(IRI2012.HMF2_IN_OUT);

%% Section 4 Main Interface with Data Overrides
% This is a simple example with data overrides for Spring Equinox.
iri_2=IRI2012();
oarr = zeros(IRI2012.numAdditionalResults,1);
oarr(IRI2012.NMF2_IN_OUT) = 54.3; % resonant frequency of F2 peak (MHz)
JF = IRI2012.defaultIRIswitches();
JF(IRI2012.FOF2_MODEL_SW) = true; % use frequency of NMF2 input
JF(IRI2012.AUR_BOUND_MODEL_SW) = false; % don't need aurora
JMAG = IGRF.GEOGRAPHIC_COORDINATES;
YEAR = 2012;
MMDD = 320; % Mar 20
ALATI = 40; % geodetic latitude (degrees North)
ALONG = -104; % longitude (degrees East) 
DHOUR = 5.2333+IRI2012.UT_INDICATOR; % 5:14 UTC
ALT = 150.0; % geodetic altitude (km)
[outf,oarr,iri_2] = iri_2.IRI_SUB(JF,JMAG,ALATI,ALONG,YEAR,...
          MMDD,DHOUR,ALT,ALT,0,oarr);
% electron density (8.4726e+08 electrons/m^3)
eldens_2 = outf(IRI2012.EL_DENS_OUT,1);
% altitude of maximum electon density in F2 region (329.1938 km)
heightMaxF2_2 = oarr(IRI2012.HMF2_IN_OUT);
