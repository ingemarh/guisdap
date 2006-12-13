% Analysis defaults
a_satch.cut=1;
if ~exist('analysis_code','var')
 a_satch.clutter=[6 20];
 if siteid==6
  analysis_code=3:4;
  analysis_intallow=[Inf;Inf];
 else
  analysis_code=1:2;
 end
end
