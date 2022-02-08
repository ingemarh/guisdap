analysis_lpf.par=[1 1 640 0 0 0 0 32];
analysis_lpf(2).par=[1 1 100 0 0 0 0 32];
for i=1:2
 analysis_lpf(i).lib='clutter';
 analysis_lpf(i).nsamp=analysis_lpf(i).par(3);
 analysis_lpf(i).nrep=analysis_lpf(i).par(2);
 analysis_lpf(i).nwin=analysis_lpf(i).par(1);
end
analysis_lpf(1).data=0;
analysis_lpf(1).raw=0;
analysis_lpf(2).data=19984;
analysis_lpf(2).raw=640;
analysis_lpf(1).do=1;

analyis_control(4)=1;
analysis_code=26;
