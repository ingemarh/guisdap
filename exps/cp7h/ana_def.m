% Analysis defaults
global iono_model
 iono_model='iri';
global fit_altitude
 fit_altitude=[0 Inf;0000 Inf;107 Inf;0 107;0 Inf;0 0;0 0];

first=350, last=2500; d1=40; d2=5;
altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
analysis_altit=first+cumsum(altd);
clear analysis_maxwidth
%analysis_maxwidth=[altd(2:end) altd(end)];
