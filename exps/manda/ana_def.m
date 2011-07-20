% Analysis defaults
if name_site=='V'
  fit_altitude([2 3 5],1)=[120;350;100];
  first=50; last=600; d1=.45; d2=1; n1=100;
  h1=230; h2=370;
  a_satch.clutter=90;
  if fix(expver)==4
    a_satch.clutter=250;
    h1=200; d1=.36; first=20; n1=225;
  end
elseif name_site=='T'
  fit_altitude(2:5,1)=[100;130;107;90];
  first=50; last=600; d1=.45; d2=1; n1=100;
  h1=230; h2=370;
  if fix(expver)==4
    a_satch.clutter=250;
    h1=200; d1=.36; first=20; n1=225;
  elseif expver>2
   h1=310; h2=500;
  end
elseif name_site=='L'
  fit_altitude([2 3 5],1)=[100;130;90];
  first=50; last=600; d1=.75; d2=1; n1=60;
  h1=210; h2=370;
  if fix(expver)==4
    h1=[145 315]; h2=[240 500]; d1=.6; first=25; n1=115;
  elseif expver>1
    h1=[140 310]; h2=[270 460];
  else
    h1=210; h2=370;
  end
  a_satch.clutter=88;
  a_satch.clutfac=10;
  a_satch.sigma=3;
end
if name_site=='T' | name_site=='L' | name_site=='V'
 a_satch.repair=63; % do not repair
 a_satch.skip=1;
 altd=[0 ones(1,n1)*d1 d1:d2:(sqrt((last-first)*2*d2))];
 analysis_range=first+cumsum(altd);
 for i=1:length(h1)
  d=find(analysis_range>h1(i) & analysis_range<h2(i));
  analysis_range(d(1))=mean([h1(i) h2(i)]);
  analysis_range(d(2:end))=[];
 end
 if strfind(data_path,'mandas')
  analysis_range(find(analysis_range>300))=[];
 end
 analysis_maxwidth=ones(size(analysis_range));
end
