function lpg_rep(n,nsh)
% lpg_rep: To duplicate lpg defs for multiple blocks
% GUISDAP v8.5   07-11-30 Copyright EISCAT
%
% See also: GUISPERT
%
global lpg_ND lpg_T lpg_bac lpg_bcs lpg_cal lpg_code lpg_dt lpg_h lpg_lag ...
       lpg_lpdata lpg_lpend lpg_lpstart lpg_nt lpg_ra lpg_ri lpg_w ...
       lpg_wom lpg_womscaled lpg_wr lpg_Ap
global vc_ch vc_group vc_routine vc_Ap lp_vc
global ch_Pt ch_gain ch_fradar
global d_data

if nargin<2, nsh=[]; end
if isempty(nsh), nsh=length(d_data)/n; end
nlpg=length(lpg_ND);
nlp=length(lp_vc);
nvc=length(vc_ch);
ncode=max(lpg_code);
nch=max(vc_ch);

n1=(0:n-1);
s=reshape((lpg_bac>0).'*(n1*nlpg),1,[]);
lpg_bac=repmat(lpg_bac,1,n)+s;
s=reshape((lpg_cal>0).'*(n1*nlpg),1,[]);
lpg_cal=repmat(lpg_cal,1,n)+s;
s=reshape(ones(nlpg,1)*n1,1,[]);
lpg_code=repmat(lpg_code,1,n)+s*ncode;
lpg_ra=repmat(lpg_ra,1,n)+s*nsh;
s=s*nlp;
lpg_lpend=repmat(lpg_lpend,1,n)+s;
lpg_lpstart=repmat(lpg_lpstart,1,n)+s;
lpg_ND=repmat(lpg_ND,1,n);
lpg_T=repmat(lpg_T,1,n);
lpg_bcs=repmat(lpg_bcs,1,n);
lpg_dt=repmat(lpg_dt,1,n);
lpg_h=repmat(lpg_h,1,n);
lpg_lag=repmat(lpg_lag,1,n);
lpg_nt=repmat(lpg_nt,1,n);
lpg_ri=repmat(lpg_ri,1,n);
lpg_w=repmat(lpg_w,1,n);
lpg_wr=repmat(lpg_wr,1,n);
lpg_wom=repmat(lpg_wom,n,1);
lpg_womscaled=repmat(lpg_womscaled,n,1);
lpg_Ap=repmat(lpg_Ap,n,1);

s=reshape(ones(nlp,1)*n1,1,[]);
lp_vc=repmat(lp_vc,1,n)+s*nvc;
lpg_lpdata=repmat(lpg_lpdata,1,n)+s*nlp;
s=reshape(ones(nvc,1)*n1,1,[]);
vc_group=repmat(vc_group,1,n)+s*nvc;
vc_ch=repmat(vc_ch,1,n)+s*nch;
vc_routine=repmat(vc_routine,1,n);
vc_Ap=repmat(vc_Ap,1,n);

ch_Pt=repmat(ch_Pt,1,n);
ch_fradar=repmat(ch_fradar,1,n);
ch_gain=repmat(ch_gain,1,n);
