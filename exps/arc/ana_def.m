% Analysis defaults
if name_site=='T'
 first=95; last=800; d1=1; d2=0.7;
 altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 analysis_maxwidth=[altd(2:end) altd(end)];
end
