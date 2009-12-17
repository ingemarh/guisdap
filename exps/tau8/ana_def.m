% Analysis defaults
if name_site=='V'
  a_satch.do=1;
  a_satch.skip=40;
  a_satch.cut=1;
  a_satch.repair=Inf;
  plasma_range=108678+(1:128*4);
  analysis_ppshortlags=1;
elseif name_site=='K'
  a_Offsetppd=-2043;
  analysis_txlimit=-2000;
  f_ch_Pt=1.5e6;
  analysis_Tsys=200;
  analysis_range=0:1500:3000;
  f_calTemp=[];
  f_ch_az=346.30;
  f_ch_el=52.45;
  f_ch_range=339.0;
end
