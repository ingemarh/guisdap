% form_adpar.m: Utility to calculate ad_parameters
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% This internal routine calculates certain parameters for each memory location in advance
% so that they can be obtained rapidly later
% ad_range : Range to the center of ambiguity function
% ad_w     : Width of the ambiguity functions
% ad_code  : Code number
% ad_lpg   : Lag profile group number
% ad_coeff : The radar factor (to be calculated in radar_eq)
%
function form_adpar

global lpg_ra lpg_nt lpg_ri lpg_bcs lpg_h lpg_dt lpg_w lpg_code
global ad_range ad_w ad_code ad_lpg ad_coeff ADDR_SHIFT

len=max(lpg_ra+(lpg_nt-1).*lpg_ri)+1;
ad_range=zeros(1,len); ad_w=zeros(1,len);
ad_code=zeros(1,len);  ad_lpg=zeros(1,len);
ad_coeff=zeros(1,len);  % used in radar_eq
for sig=find(lpg_bcs=='s')
  addr=lpg_addr(sig)+ADDR_SHIFT; % To change from radar to Matlab addressing
  len=length(addr);
  ad_range(addr)=lpg_h(sig)+(0:lpg_nt(sig)-1)'*lpg_dt(sig);
  ad_w(addr)=lpg_w(sig)*ones(len,1);
  ad_code(addr)=lpg_code(sig)*ones(len,1);
  ad_lpg(addr)=sig*ones(len,1);
end
