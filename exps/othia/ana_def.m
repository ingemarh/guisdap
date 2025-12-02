% Analysis defaults
%a_satch.sigma=4;
a_satch.cut=1;
analysis_gating=1;
analysis_ppshortlags=1;
a_phasepush=-20;
if strfind(data_path,'@32p')
 analysis_txpower=8;
 analysis_intfix(5:6)=47:48;
 analysis_plasmaline=1;
 plasma_range=col(ones(2*1125,1)*(0:3)*2262+(1:2*1125)'*ones(1,4)+12);
elseif name_site=='L'
 a_satch.clutter=[0 40];
 altd=[0 5 5 10:5:150];
 analysis_altit=110+cumsum(altd);
 analysis_fullwidth=2;
 fit_altitude([2 3 5],1)=[120;130;120];
elseif name_site=='V'
 altd=[0 5 5 10:5:150];
 analysis_altit=100+cumsum(altd);
 analysis_fullwidth=2;
 a_satch.clutter=[0 12];
elseif name_site=='T'
 altd=[0 5 5 10:5:150];
 analysis_altit=90+cumsum(altd);
 analysis_fullwidth=2;
 a_satch.clutter=[0 33];
end
