analysis_lpf.par=[6600 1 20]; %decimation
analysis_lpf.raw=0;
analysis_lpf.skip=340;
analysis_lpf.lib='resampler';
analysis_lpf(2).par=[fix(6600/20);1;Barker(13)];
analysis_lpf(2).raw=[];
analysis_lpf(2).lib='fir';
analysis_lpf(3).par=[1 1 6600/20-13+1 0 0 0 0 1];
analysis_lpf(3).data=0;
analysis_lpf(3).raw=[];
analysis_lpf(3).lib='clutter';
[analysis_lpf.loop]=deal(512); %~1s dumps
analysis_lpf(4).par=[318 1 150 32];
analysis_lpf(4).data=318;
analysis_lpf(4).raw=[];
analysis_lpf(4).lib='pulse2pulse';
[analysis_lpf.do]=deal(1);

analyis_control(4)=1;
analysis_code=56;

%altd=[0 10:5:1500];
%analysis_altit=100+cumsum(altd);
%analysis_maxwidth=2*diff(analysis_altit);
%analysis_maxwidth=Inf;

a_satch.sigmab=100;
a_satch.sigma=2;
a_satch.clutter=7;
a_satch.cut=0;
a_satch.prep=2000;
%a_satch.do=0;

if contains('3WD',data_path(end))
 [analysis_lpf.do]=deal(0); % integrated data
end
