% Analysis defaults
global iono_model
 iono_model='iri';
global fit_altitude
 fit_altitude=[0 Inf;160 Inf;200 Inf;0 0;0 Inf];

first=90; last=1400; d1=4; d2=2;
altd=[0 ones(1,4)*d1 d1:d2:(sqrt((last-first)*2*d2))];
analysis_altit=first+cumsum(altd);
analysis_maxwidth=[altd(2:end) Inf];
clear analysis_range
