fit_altitude=[0 Inf;80 Inf;107 1500;0 0];
first=90; last=800; d1=4; d2=3;
altd=[0 ones(1,4)*d1 d1:d2:(sqrt((last-first)*2*d2))];
analysis_altit=first+cumsum(altd);
analysis_maxwidth=[altd(2:end) altd(end)];
