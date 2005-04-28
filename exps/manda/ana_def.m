% Analysis defaults
if name_site=='V'
  fit_altitude=[0 Inf;120 Inf;350 1500;0 0;100 Inf];
% analysis_range=[50:100 102:3:140 300:100:500];
  first=50; last=600; d1=3; d2=2; n1=15;
  a_satch.lpg_skip=[3:97 3603:3665];
  a_satch.skip=1;
elseif name_site=='T'
  fit_altitude=[0 Inf;100 Inf;130 1500;0 0;90 Inf];
% analysis_range=[50:100 102:3:140 300:100:500];
  first=50; last=600; d1=3; d2=1; n1=15;
  a_satch.lpg_skip=[3:97 3603:3665];
  a_satch.skip=1;
end
if name_site=='T' | name_site=='L' | name_site=='V'
 altd=[0 ones(1,n1)*d1 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_range=first+cumsum(altd);
 d=find(analysis_range>230 & analysis_range <370);
 analysis_range(d(1))=300;
 analysis_range(d(2:end))=[];
 analysis_maxwidth=ones(size(analysis_range));
end
