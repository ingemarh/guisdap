% save_GUPvar: Script to save GUP variables to a file
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% See also: design path_expr

GUP_iniver=GUP_ver;

if max(lp_nfir)>1
  lp_firsto=sparse(diff([zeros(size(lp_vc));lp_fir]));
else 
  lp_firsto=lp_fir;
end

if ~exist('apustr'), apustr=''; end
GUPvarfile=[path_expr name_expr name_site apustr 'GUPvar'];
str=['save ' GUPvarfile];
if local.matlabversion>=14
 str=[str ' -v6'];
end

str=[str ' GUP_iniver ch_fradar ch_gain p_dtau p_rep p_ND p_XMITloc p_RECloc'];
str=[str ' vc_ch vc_env vc_envo vc_p vc_adcint vc_sampling lp_dec lp_firsto lp_nfir'];
str=[str ' lp_T lp_bcs lp_code lp_dt lp_h lp_nt lp_ra lp_ri lp_t1 lp_t2 lp_vc'];

eval(str);
disp([GUPvarfile ' saved']);
