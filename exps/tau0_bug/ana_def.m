first=80; last=1250; d1=7; d2=1; n1=6;
altd=[0 ones(1,n1)*d1 d1:d2:(sqrt((last-first)*2*d2))];
analysis_altit=first+cumsum(altd);
analysis_maxwidth=[altd(2:end) altd(end)];
