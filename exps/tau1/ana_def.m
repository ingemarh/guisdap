% Analysis defaults
analysis_ppshortlags=1;
if name_site=='V'
  a_satch.skip=40;
  analysis_code=[1 2];
% vhf_half_tx=1; 
elseif name_site=='T'
  a_satch.do=1;
end
