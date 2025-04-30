analysis_lpf.par=[1 1 1000 0 0 0 0 48];
analysis_lpf(1).data=0;
analysis_lpf(1).raw=0;
analysis_lpf(1).skip=18;
analysis_lpf(2).par=[1 1 1280-1018 0 0 0 0 48];
analysis_lpf(2).data=1000*48-47*48/2;
analysis_lpf(2).raw=1000;
analysis_lpf(2).skip=1018;
[analysis_lpf.lib]=deal('clutter');
[analysis_lpf.do]=deal(1);
[analysis_lpf.loop]=deal(60); % ~1s dumps

analyis_control(4)=1;
analysis_code=26;

altd=[0 20:10:1500];
analysis_altit=100+cumsum(altd);
analysis_maxwidth=2*diff(analysis_altit);

a_satch.sigmab=100;
a_satch.sigma=3;
%a_satch.skip=0;
a_satch.cut=1;
a_satch.prep=14000;

