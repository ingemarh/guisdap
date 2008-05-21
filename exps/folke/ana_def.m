% Analysis defaults
a_satch.cut=1;
a_satch.repair=Inf;
analysis_maxwidth=analysis_maxwidth*2.;
a_satch.sigmab=2;
if ~exist('analysis_code','var')
 if siteid==6
  a_satch.clutter=23;
  analysis_code=3;
  analysis_sweep='azel';
 else
  a_satch.clutter=[12 23];
  analysis_code=1:2;
 end
end
