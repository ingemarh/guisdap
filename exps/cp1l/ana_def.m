% Analysis defaults
global iono_model
 iono_model='iri';
global fit_altitude
 fit_altitude=[0 Inf;0 Inf;107 1500;0 107;0 Inf];

if name_site=='T'
 first=90; last=700; d1=3; d2=3.1;
 altd=[0 ones(1,7)*d1 d1:d2:(sqrt((last-first-7*d1)*2*d2))];
 analysis_altit=first+cumsum(altd);
 analysis_maxwidth=[altd(2:end) altd(end)]*1.5;
else
 analysis_range=0:500:1000;
end
