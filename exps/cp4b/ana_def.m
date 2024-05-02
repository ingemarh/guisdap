% Analysis defaults
if expver==0
 analysis_classic=1;
 if ~exist('analysis_code')
   analysis_code=3:4; %bore sight
 end
else
 if ~exist('analysis_code')
   analysis_code=1; %bore sight
 end
 fit_altitude(7:8,2)=[Inf;Inf];
 first=210; last=1500; d1=30; d2=4;
 altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 clear analysis_maxwidth
end
