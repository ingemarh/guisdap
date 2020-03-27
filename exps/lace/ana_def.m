if 0
if name_site=='T'
 analysis_lpf.par=load([path_expr 'lace_ua.par']);
 analysis_lpf.data=48;
 analysis_lpf.raw=133120;
 analysis_lpf.do=0;
end
analysis_lpf.lib='plwin';
analysis_lpf.nsamp=analysis_lpf.par(7);
analysis_lpf.nrep=analysis_lpf.par(6);
analysis_lpf.nwin=analysis_lpf.par(5);
analysis_lpf.wlen=analysis_lpf.par(8);
analysis_lpf.p=[0 analysis_lpf.nrep-1]; %start/stop profiles to analyse
analysis_lpf.par(18:20)=0; %no d_gates
end
