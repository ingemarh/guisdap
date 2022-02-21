analysis_lpf.par=[1 1 630 0 0 0 0 48];
analysis_lpf(1).lib='clutter';
analysis_lpf(1).data=0;
analysis_lpf(1).raw=10;
analysis_lpf(2).par=[1 1 100 0 0 0 0 48];
analysis_lpf(2).data=29112;
analysis_lpf(2).raw=640;
analysis_lpf(2).lib='clutter';
analysis_lpf(1).do=1;

len_prof=analysis_lpf(2).raw+analysis_lpf(2).par(3);

analyis_control(4)=1;
analysis_code=26;

altd=[0 20:2:1000];
analysis_altit=100+cumsum(altd);
analysis_maxwidth=2*diff(analysis_altit);

a_satch.sigmab=100;
a_satch.sigma=1;
a_satch.skip=1;
a_satch.cut=1;
