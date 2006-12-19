% Analysis defaults
fit_altitude(6:7,2)=[Inf;Inf];

first=350; last=2500; d1=40; d2=5;
altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
analysis_altit=first+cumsum(altd);
clear analysis_maxwidth
%analysis_maxwidth=[altd(2:end) altd(end)];
