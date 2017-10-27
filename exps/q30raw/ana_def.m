f_ch_Pt=2e6;
analysis_Tsys=150;
%global iono_model
%iono_model='gup150';
analysis_txpower=8;
f_calTemp=[];
analysis_lagprofiling.par=load([path_expr 'q16x20.par']);
analysis_lagprofiling.lib='plwin';
analysis_lagprofiling.do=1;
analysis_lagprofiling.raw=0;
analysis_lagprofiling.nsamp=analysis_lagprofiling.par(7);
analysis_lagprofiling.nrep=analysis_lagprofiling.par(6);
analysis_lagprofiling.p=[0 analysis_lagprofiling.nrep-1]; %start/stop profiles to analyse
r0_setup=2667e-6;
