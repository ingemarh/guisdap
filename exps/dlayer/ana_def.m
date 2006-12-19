% Analysis defaults
if name_site=='L' | name_site=='V'
  fit_altitude(2:5,1:2)=[0 0;0 0;0 0;100 Inf];
  analysis_range=[60:100 102:3:140];
end
