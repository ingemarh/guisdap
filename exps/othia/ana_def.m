% Analysis defaults
%a_satch.sigma=4;
a_satch.cut=1;
analysis_gating=1;
analysis_ppshortlags=1;
if strfind(data_path,'@32p')
 analysis_txpower=8;
 analysis_intfix(5:6)=47:48;
 analysis_plasmaline=1;
elseif name_site=='L'
 a_satch.clutter=[0 40];
 first=100; last=2000; d1=10; d2=5;
 altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 clear analysis_maxwidth
elseif name_site=='V'
 a_satch.clutter=[0 12];
 first=100; last=2000; d1=10; d2=5;
 altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 clear analysis_maxwidth
elseif name_site=='T'
 a_satch.clutter=[0 33];
 first=90; last=2000; d1=10; d2=5;
 altd=[0 0 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 clear analysis_maxwidth
end
