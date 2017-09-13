if exist('analysis_lagprofiling')
 analysis_lagprofiling.par=load([path_expr 'lace_ua.par']);
 analysis_lagprofiling.lib='plwin';
 analysis_lagprofiling.data=48;
 analysis_lagprofiling.raw=133120+(1:1778*512*2);
 analysis_lagprofiling.do=1;
end
