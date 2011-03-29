% Analysis defaults
if name_site=='T'
% first=90; last=700; d1=3; d2=3.1;
 first=85; last=700; d1=4; d2=4;
 altd=[0 ones(1,7)*d1 d1:d2:(sqrt((last-first-7*d1)*2*d2))];
 analysis_altit=first+cumsum(altd);
 analysis_maxwidth=[altd(2:end) altd(end)]*1.5;
 a_satch.lpg_skip=[1 20 23];
 a_satch.skip=2;
 a_satch.sigma=5;
else
 analysis_range=0:500:1000;
end

