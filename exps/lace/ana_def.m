if name_site=='T'
 analysis_lagprofiling.par=load([path_expr 'lace_ua.par']);
 analysis_lagprofiling.data=48;
 analysis_lagprofiling.raw=133120;
 analysis_lagprofiling.do=0;
end
analysis_lagprofiling.lib='plwin';
analysis_lagprofiling.nsamp=analysis_lagprofiling.par(7);
analysis_lagprofiling.nrep=analysis_lagprofiling.par(6);
analysis_lagprofiling.nwin=analysis_lagprofiling.par(5);
analysis_lagprofiling.wlen=analysis_lagprofiling.par(8);
analysis_lagprofiling.p=[0 analysis_lagprofiling.nrep-1]; %start/stop profiles to analyse
analysis_lagprofiling.par(18:20)=0; %no d_gates
