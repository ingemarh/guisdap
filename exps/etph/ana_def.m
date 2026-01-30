clear analysis_maxwidth

first=110; last=2500; d1=10; d2=5;
altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
analysis_altit=first+cumsum(altd);
a_satch.cut=1;
a_satch.clutter=4;
