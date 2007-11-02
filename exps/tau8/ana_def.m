% Analysis defaults
if name_site=='V'
  a_satch.do=1;
  a_satch.skip=40;
  a_satch.cut=1;
  a_satch.repair=Inf;
elseif name_site=='K'
  a_Offsetppd=-2050;
  analysis_txlimit=-2000;
  f_ch_Pt=1.5e6;
  analysis_Tsys=300;
  f_calTemp=[];
end
