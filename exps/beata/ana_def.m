% Analysis defaults
%a_satch.sigma=4;
%a_satch.plot=8;
if name_site=='L'
  a_satch.clutter=[20];
  analysis_maxwidth=1.5*analysis_maxwidth;
elseif name_site=='T' | name_site=='V'
  a_satch.sigma=3;
  a_satch.cut=1;
  analysis_ppshortlags=1;
end
