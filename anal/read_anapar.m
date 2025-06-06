global iono_model fit_altitude a_satch a_Offsetppd d_saveint lpg_s
fit_altitude=NaN*ones(9,8);

first=75; last=2500; d1=3; d2=1; n1=10;

if name_site=='L' | name_site=='3'
 d1=4; n1=8;
elseif name_site=='V' | name_site=='Q'
 d1=5; n1=6;
end

if contains('TLVPQ3',name_site)
 altd=[0 ones(1,n1)*d1 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_altit=first+cumsum(altd);
 analysis_maxwidth=[altd(2:end) altd(end)];
else
 analysis_range=(0:1)*1500;
end
lpg_s='s';

if name_site=='Q'
 f_ch_Pt=2e6;
 analysis_Tsys=100;
 analysis_txpower=8;
 f_calTemp=[];
elseif contains('3WD',name_site)
 global any_start
 analysis_Tsys=120; %manufacturer
 analysis_txpower=8;
 f_calTemp=[];
end
if exist([path_expr 'ana_def.m'])==2
  run([path_expr 'ana_def'])
end
