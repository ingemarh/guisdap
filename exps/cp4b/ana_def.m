% Analysis defaults
fit_altitude=[0 Inf;100 Inf;120 1500;0 0;0 Inf;0 0;0 Inf;0 Inf];
first=210; last=1500; d1=30; d2=4;
altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
analysis_altit=first+cumsum(altd);
clear analysis_maxwidth
