analysis_lpf.par=load('sy16ac_ha.par');
analysis_lpf.lib='plwin';
analysis_lpf.data=0;
analysis_lpf.raw=0;

analysis_lpf.do=1;

analyis_control(4)=2;
analysis_code=32:63;

altd=[0 20:2:1000];
analysis_altit=100+cumsum(altd);
analysis_maxwidth=2*diff(analysis_altit);

a_satch.sigmab=5;
%a_satch.sigma=10;
a_satch.clutter=8;
a_satch.cut=1;
a_satch.plot=8;
