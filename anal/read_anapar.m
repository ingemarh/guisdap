global iono_model fit_altitude a_satch a_Offsetppd
 iono_model='iri';
 if exist('iri')~=3, iono_model='giveme'; end
 fit_altitude=[0 Inf;80 Inf;107 1500;90 107;0 Inf];

first=75; last=2500; d1=3; d2=1; n1=10;

if name_site=='L'
 fit_altitude=[0 Inf;90 Inf;113 1500;0 0;0 Inf];
 d1=4; n1=8;
elseif name_site=='V'
 fit_altitude=[0 Inf;100 Inf;120 1500;0 0;0 Inf];
 d1=5; n1=6;
end

if name_site=='T' | name_site=='L' | name_site=='V'
 altd=[0 ones(1,n1)*d1 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 analysis_maxwidth=[altd(2:end) altd(end)];
else
 analysis_range=0:500:1000;
end

if exist([path_expr 'ana_def.m'])==2
  run([path_expr 'ana_def'])
end
