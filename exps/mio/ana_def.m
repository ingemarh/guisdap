% Analysis defaults
a_satch.clutter=64;
%a_satch.plot=8;
a_satch.repair=[Inf];
a_satch.cut=1;
analysis_ppshortlags=1;
analysis_altit(find(analysis_altit>850))=1000;
if strfind(data_path,'@32p')
 analysis_txpower=8;
 analysis_intfix(5:6)=47:48;
 if name_site=='P'
  display_spectra=1;
  a_satch.do=0;                
  analysis_range=[100:3.75:300];
  analysis_overlap=1;
  analysis_maxwidth=10;
 else
  analysis_plasmaline=1;
  plasma_range=24+col(ones(4*75,1)*(0:3)*425895+(1:4*75)'*ones(1,4)+175*75+174*2304);
 end
else
 analysis_lpf.par=load([path_expr 'mio_lc.par']);
 analysis_lpf.lib='clutter';
 analysis_lpf.raw=128*186;
 analysis_lpf.data=36;
 analysis_lpf.do=0;
 analysis_lpf(2).par=load([path_expr 'mio_lac.par']);
 analysis_lpf(2).lib='alt_decoder';
 analysis_lpf(2).raw=[];
 analysis_lpf(2).data=[];
 if contains(data_path,'fixed42p')
  analysis_lpf(3)=analysis_lpf(1);
  analysis_lpf(3).raw=128*186*2+528*640;
  analysis_lpf(3).data=87891+36;
  analysis_lpf(4)=analysis_lpf(2);
  analysis_lpf(4).raw=[];
  analysis_lpf(4).data=[];
 end
end
