analysis_lpf.par=load('sy16ac_ha.par');
analysis_lpf.lib='plwin';
analysis_lpf.data=0;
analysis_lpf.raw=0;

len_prof=analysis_lpf.par(7)+analysis_lpf.par(11);

analysis_lpf.do=1;

analysis_code=32:63;

a_satch.sigmab=5;
%a_satch.sigma=10;
a_satch.clutter=8;
a_satch.cut=1;
%a_satch.plot=8;
