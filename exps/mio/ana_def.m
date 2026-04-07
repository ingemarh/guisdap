% Analysis defaults
a_satch.clutter=[30];
a_satch.sigma=4;
%a_satch.plot=8;
a_satch.repair=[Inf];
a_satch.cut=1;
analysis_ppshortlags=1;
d=find(analysis_altit>455); analysis_altit(d(1))=600;
if strfind(data_path,'@32p')
 analysis_txpower=8;
 analysis_intfix(5:6)=47:48;
 if name_site=='P'
  display_spectra=1;
  a_satch.do=0;                
  analysis_range=[100:4.5:300];
  analysis_overlap=1;
  analysis_maxwidth=10;
 else
  analysis_plasmaline=1;
  plasma_range=24+col(ones(4*90,1)*(0:3)*126744+(1:4*90)'*ones(1,4)+79*90+78*1440);
 end
else
 analysis_lpf.par=load([path_expr 'ipy_lc.par4']);
 analysis_lpf.lib='clutter';
 analysis_lpf.raw=64*61;
 analysis_lpf.data=30;
 analysis_lpf.do=0;
 analysis_lpf(2).par=load([path_expr 'ipy_lac.par4']);
 analysis_lpf(2).lib='alt_decoder';
 analysis_lpf(2).raw=64*61;
 analysis_lpf(2).data=190;
 if contains(data_path,'fixed42p')
  analysis_lpf(3)=analysis_lpf(1);
  analysis_lpf(3).raw=64*61*2+160*1600;
  analysis_lpf(3).data=30+10567;
  analysis_lpf(4)=analysis_lpf(2);
  analysis_lpf(4).raw=64*61*2+160*1600;
  analysis_lpf(4).data=190+10567;
 end
end
