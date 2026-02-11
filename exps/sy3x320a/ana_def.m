analysis_lpf(1).par=[4 4 4000 0 0 0 0 128];
analysis_lpf(1).data=0;
analysis_lpf(1).raw=0;
analysis_lpf(1).skip=35;
analysis_lpf(1).lib='clutter';
analysis_lpf(2).par=[3 4 4000 128 383 1 4 1 1 1 1 1 -1 1 -1 1 1 -1 -1];
analysis_lpf(2).data=4000*128-127*128/2;
analysis_lpf(2).raw=[];
analysis_lpf(2).skip=[];
analysis_lpf(2).lib='alt_decoder';
analysis_lpf(3).par=[1 4 1000 0 0 0 0 128];
analysis_lpf(3).data=2445952;
analysis_lpf(3).raw=4000*4;
analysis_lpf(3).skip=5000;
analysis_lpf(3).lib='clutter';

analysis_lpf(4).par=[1 4 3965 10 0 0 0 128];
analysis_lpf(4).data=analysis_lpf(3).data+1000*128-127*128/2;
analysis_lpf(4).raw=5000*4;
analysis_lpf(4).skip=35;
analysis_lpf(4).lib='clutter';
[analysis_lpf.do]=deal(1);
[analysis_lpf.loop]=deal(5); % ~0.8s dumps

analyis_control(4)=1;
analysis_code=[85:88;71 71 71 71];
[analysis_lpf.coderow]=deal(1);
analysis_lpf(4).coderow=2;

if name_site=='3'
 altd=[0 10:5:150];
 analysis_altit=100+cumsum(altd);

 if expver==2
  fit_altitude(6,1:2)=[340 Inf]; % Fit for H+
 end
 %analysis_screen=complex([360 10],[45 5])
 analysis_fullwidth=2;
else
 analysis_lpf(1).skip=0;
 analysis_Offsetppd=800+20;
end
analysis_gating=1;

a_satch.sigma=3;
%a_satch.plot=8;
%a_satch.skip=0;
a_satch.cut=1;
a_satch.prep=40000;

if contains('3WD',data_path(end))
 [analysis_lpf.do]=deal(0); % integrated data
end
