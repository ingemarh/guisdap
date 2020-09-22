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
elseif name_site=='V'
 analysis_maxwidth=3*analysis_maxwidth;
 analysis_gating=2;
 if expver>1
  plasma_range=col(ones(5*200,1)*(0:1)*25668+(1:5*200)'*ones(1,2)+24668);
 end
end
