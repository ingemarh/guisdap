% save_toinitfile.m: saves ambiguity functions and other variables to a file
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% These variables are needi in the  data analysis
%
% See also: load_initfile save_GUPvar load_GUPvar path_expr

GUP_iniver=GUP_ver;
nameexpr=[name_expr name_site];

% We produce groupwise variables to save disk space
[a,ind]=windex(diff_val(vc_group),vc_group);
vcg_Aenv=vc_Aenv(:,ind);
vcg_Ap=vc_Ap(:,ind);
vcg_Apenv=vc_Apenv(:,ind);
vcg_penv=vc_penv(:,ind);
vcg_penvabs=vc_penvabs(:,ind);

if ~exist('apustr'), apustr=''; end
initfile=[path_expr name_expr name_site apustr 'init'];
saveflag='';
if local.matlabversion>=7
 saveflag='-v6';
end

global vc_routine
lpg_wr=sparse(lpg_wr);

save(initfile,'GUP_iniver','ch_fradar','ch_gain','lp_vc','lpg_ND','lpg_T',...
 'lpg_bcs','lpg_code','lpg_lpstart','lpg_lpend','lpg_lpdata','lpg_dt',...
 'lpg_h','lpg_lag','lpg_nt','lpg_ra','lpg_ri','lpg_w','lpg_wom','lpg_bac',...
 'lpg_cal','nameexpr','p_XMITloc','p_RECloc','p_D0','p_N0','p_R0','p_T0',...
 'p_dtau','p_m0','p_om','p_om0','vcg_penv','vcg_penvabs','vc_penvo',...
 'vc_ch','vcg_Aenv','vcg_Ap','vcg_Apenv','vc_group','lpg_wr','vc_routine',...
 saveflag);
disp([initfile ' saved']);

clear GUP_iniver nameexpr a ind vcg_Aenv vcg_Ap vcg_Apenv vcg_penv vcg_penvabs initfile
