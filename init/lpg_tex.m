% lpg_tex: A script to output lpg_ parameters in TeX form
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
%  
% suitable to be printed with tex code in file lpg.tex
% 
% See also: init_GUP
fil=[path_expr name_expr name_site apustr 'lpg_i.tex'];
disp(fil)
if exist(fil)==2, delete(fil), end
fid=fopen(fil,'w');

form1=['\\+%3.0f&%2.0f&  '];
form2=['& %3.0f&%6.0f&%6.0f&%3.0f&%3.0f&%3.0f&%3.0f&%4.0f&%4.0f\\cr\n'];
for ind=1:length(lpg_ND)
  form=[form1,setstr(lpg_bcs(ind)),form2];
  fprintf(fid,form,ind,lpg_code(ind),lpg_lag(ind)*p_dtau,lpg_h(ind)*p_dtau,...
         lpg_w(ind)*p_dtau,lpg_dt(ind)*p_dtau,lpg_ND(ind),lpg_T(ind),...
         lpg_nt(ind),lpg_ra(ind),lpg_ri(ind));
end
fclose('all');
clear fil ind fid
