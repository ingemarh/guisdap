function lpg_cut(lpgs)
% lpg_cut: To remove ranges of lpgs from analysis
% GUISDAP v8.6   08-12-30 Copyright EISCAT
%
% See also: GUIZARD
%
global lpg_ND lpg_T lpg_bac lpg_bcs lpg_cal lpg_code lpg_dt lpg_h lpg_lag ...
       lpg_lpend lpg_lpstart lpg_nt lpg_ra lpg_ri lpg_w lpg_wom lpg_wr
lpg_ra(lpgs)=[];
lpg_nt(lpgs)=[];
lpg_ri(lpgs)=[];
lpg_bcs(lpgs)=[];
lpg_bac(lpgs)=[];
lpg_h(lpgs)=[];
lpg_ND(lpgs)=[];
lpg_T(lpgs)=[];
lpg_cal(lpgs)=[];
lpg_code(lpgs)=[];
lpg_dt(lpgs)=[];
lpg_lag(lpgs)=[];
lpg_lpstart(lpgs)=[];
lpg_lpend(lpgs)=[];
lpg_w(lpgs)=[];
lpg_wom(lpgs,:)=[];
lpg_wr(:,lpgs)=[];
