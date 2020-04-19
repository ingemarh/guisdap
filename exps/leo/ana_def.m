analysis_lpf.do=1;
analysis_lpf.lib='resampler';
analysis_lpf.par=[15 136 1190 7 2 128]; % decimation,txsamps,sigsamps,calsamps,calfreq,ncodes
analysis_lpf.nrep=128*5;
analysis_lpf.raw=[];
analysis_lpf.data=0;
analysis_lpf.nsamp=20000;
analysis_lpf.p=[0 128*5-1]; %start/stop profiles to analyse
analysis_lpf(2).par=load([path_expr 'leo_u.par']);
analysis_lpf(2).lib='plwin';
analysis_lpf(2).raw=136*128;
analysis_lpf(2).p=[0 128*5-1]; %start/stop profiles to analyse
analysis_lpf(2).data=14;

analysis_altit=80+(0:3:700^.7).^1.4;
a_satch.cut=1;
