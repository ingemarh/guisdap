% Analysis defaults
if ~exist('analysis_code','var')
 if siteid==6
  a_satch.clutter=[32 32 16];
  analysis_code=1:2;
  analysis_intallow=[Inf;Inf];
 else
  a_satch.clutter=[13 26];
  analysis_code=3:4;
  d=find(analysis_altit<1300);
  analysis_altit=analysis_altit(d);
  analysis_maxwidth=analysis_maxwidth(d);
 end
end
