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
analysis_code=73;
if datenum(analysis_start)<datenum([2023 2 26])
 analysis_code=26;
end

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

a_satch.sigmab=10;
a_satch.sigma=3;
%a_satch.skip=0;
a_satch.cut=1;
a_satch.prep=14000;

if contains('3WD',data_path(end))
 [analysis_lpf.do]=deal(0); % integrated data
end
