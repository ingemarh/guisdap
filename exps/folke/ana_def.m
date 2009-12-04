% Analysis defaults
a_satch.cut=1;
a_satch.repair=Inf;
analysis_maxwidth=analysis_maxwidth*2.;
a_satch.sigmab=2;
a_satch.clutter=[12 23 23];
analysis_ppshortlags=1;
if ~exist('analysis_code','var')
 if siteid==6
  analysis_code=3;
  analysis_sweep='azel';
  plasma_range=58990+(1:3*90);
  analysis_txpower=[72 5000]; %1:4 ratio 
 else
  analysis_code=1:2;
  analysis_txpower=[71 1250]; %4:1 ratio
 end
end
