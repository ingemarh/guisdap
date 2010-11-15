% Analysis defaults
%a_satch.sigma=4;
a_satch.cut=1;
analysis_ppshortlags=1;
if name_site=='L'
 d=find(analysis_altit>1200); analysis_altit(d(1))=1400;
 a_satch.clutter=[120];
 a_satch.repair=[Inf];
 analysis_maxwidth=3*analysis_maxwidth;
 analysis_gating=2;
else
 analysis_maxwidth=3*analysis_maxwidth;
 analysis_gating=2;
end
