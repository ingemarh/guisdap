% Analysis defaults
d=find(analysis_altit<1300);
analysis_altit=analysis_altit(d);
analysis_maxwidth=analysis_maxwidth(d);

a_satch.clutter=[10 30]; a_satch.skip=1;
a_satch.sigma=4;
%a_satch.plot=8;
%a_satch.repair=-Inf;
