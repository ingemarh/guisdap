analysis_lpf.skip=516;
analysis_lpf.par=[4188-analysis_lpf.skip 32 6]; %decimation
analysis_lpf.raw=0;
analysis_lpf.lib='resampler';
analysis_lpf.data=[];
analysis_lpf(2).par=load([path_expr 'sy16x3a_3a.par']);
analysis_lpf(2).raw=[];
analysis_lpf(2).lib='plwin';
analysis_lpf(2).data=0;
analysis_lpf(3).par=[32 32 analysis_lpf(1).par(1)/analysis_lpf(1).par(3) 0 0 0 0 1];
analysis_lpf(3).raw=[];
analysis_lpf(3).lib='clutter';
analysis_lpf(3).data=[];
analysis_lpf(4).par=load([path_expr 'sy16x3a_3b.par']);
analysis_lpf(4).raw=[];
analysis_lpf(4).lib='plwin';
analysis_lpf(4).data=[];
[analysis_lpf.loop]=deal(25); %~1s dumps
[analysis_lpf.do]=deal(1);

analysis_code=48+[0:31];
%analyis_control(4)=1;
analysis_bfrac=5;

%altd=[0 10:5:1500];
%analysis_altit=100+cumsum(altd);
%analysis_maxwidth=2*diff(analysis_altit);
%analysis_maxwidth=Inf;

a_satch.sigmab=Inf;
a_satch.sigma=100;
a_satch.clutter=70;
%a_satch.cut=0;
a_satch.prep=1250;
%a_satch.do=0;
fit_altitude([2 3 5],1)=[120;180;100];

if contains('3WD',data_path(end))
 [analysis_lpf.do]=deal(0); % integrated data
end
