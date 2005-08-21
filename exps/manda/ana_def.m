% Analysis defaults
if name_site=='V'
  fit_altitude=[0 Inf;120 Inf;350 1500;0 0;100 Inf];
  first=50; last=600; d1=.45; d2=1; n1=100;
  h1=230;
elseif name_site=='T'
  fit_altitude=[0 Inf;100 Inf;130 1500;0 0;90 Inf];
  first=50; last=600; d1=.45; d2=1; n1=100;
  h1=230;
elseif name_site=='L'
  fit_altitude=[0 Inf;100 Inf;130 1500;0 0;90 Inf];
  first=50; last=600; d1=.75; d2=1; n1=60;
  h1=210;
  a_satch.clutter=88;
end
if name_site=='T' | name_site=='L' | name_site=='V'
 a_satch.skip=1;
 altd=[0 ones(1,n1)*d1 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_range=first+cumsum(altd);
 d=find(analysis_range>h1 & analysis_range <370);
 analysis_range(d(1))=mean([h1 370]);
 analysis_range(d(2:end))=[];
 analysis_maxwidth=ones(size(analysis_range));
end
