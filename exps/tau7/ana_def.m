% Analysis defaults
a_satch.clutter=[120];
%a_satch.sigma=4;
a_satch.repair=[Inf];
a_satch.cut=1;
d=find(analysis_altit>1200); analysis_altit(d(1))=1400;
