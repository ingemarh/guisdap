analysis_code=51+(0:63);
analysis_lpf.data=0;
analysis_lpf.raw=0;
if name_site=='3'
 analysis_lpf.skip=21;
 if false
  analysis_lpf.par=load('sy32ac_3a.par');
  analysis_lpf.lib='plwin';
 else
  analysis_lpf.par=load('sy32ac_3c.par');
  analysis_lpf.par(3)=1000;
  analysis_lpf.lib='clutter';
  analysis_lpf(2).par=load('sy32ac_3ac.par');
  analysis_lpf(2).lib='alt_decoder';
  analysis_lpf(2).data=1268;
  analysis_lpf(2).raw=[];
  %analysis_lpf(2).skip=21;
  analysis_lpf(3).par=load('sy32ac_3c.par');
  analysis_lpf(3).par(3)=268;
  analysis_lpf(3).lib='clutter';
  analysis_lpf(3).data=1000;
  analysis_lpf(3).raw=1000*64;
  analysis_lpf(3).skip=1021;
 end
 a_satch.clutter=10;
 %a_satch.clutfac=1000000;
 a_satch.sigma=2;
 a_satch.cut=1;
 %a_satch.plot=8;
 analysis_ppshortlags=1;
else
 analysis_lpf.par=load('sy32ac_ra.par');
 analysis_lpf.lib='plwin';
 analysis_lpf.skip=0;
 for i=2:3
  analysis_lpf(i).par=load('sy32ac_rc.par');
  analysis_lpf(i).lib='clutter';
  analysis_lpf(i).data=287*3+286*64+(634+633)*(i-2);
  analysis_lpf(i).raw=634*64*(i-1);
  analysis_lpf(i).skip=634*(i-2);
 end
 analysis_Offsetppd=800-15;
end
analysis_lpf(1).do=1;

if contains('3',data_path(end))
 [analysis_lpf.do]=deal(0); % integrated data
 fit_altitude(6,1:2)=[350 Inf]; % Fit for H+
 %fit_altitude(7,1:2)=[130 250]; % Fit for NO+
 %fit_altitude(2,1)=[250]; % Fit Ti above F
end
