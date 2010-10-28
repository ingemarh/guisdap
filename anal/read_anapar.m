global iono_model fit_altitude a_satch a_Offsetppd d_saveint
fit_altitude=NaN*ones(9,8);

first=75; last=2500; d1=3; d2=1; n1=10;

if name_site=='L'
 d1=4; n1=8;
elseif name_site=='V'
 d1=5; n1=6;
end

if name_site=='T' | name_site=='L' | name_site=='V' | name_site=='P'
 altd=[0 ones(1,n1)*d1 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 analysis_maxwidth=[altd(2:end) altd(end)];
else
 analysis_range=(0:1)*1500;
end

if exist([path_expr 'ana_def.m'])==2
  run([path_expr 'ana_def'])
end
