% form_adpar.m: Utility to calculate ad_parameters
% GUISDAP v.8.2 03-09-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% This internal routine calculates certain parameters for each memory location in advance
% so that they can be obtained rapidly later
% ad_range : Range to the center of ambiguity function
% ad_w     : Width of the ambiguity functions
% ad_code  : Code number
% ad_lpg   : Lag profile group number
% ad_lag   : Lag value
% ad_coeff : The radar factor (to be calculated in radar_eq)
%
function form_adpar

global lpg_ra lpg_nt lpg_ri lpg_bcs lpg_h lpg_dt lpg_w lpg_code lpg_lag lpg_bac
global ad_range ad_w ad_code ad_lpg ad_coeff ad_lag ad_bac ad_bcs ADDR_SHIFT
global a_code lpg_bac lpg_cal lpg_s

zl=zeros(1,max(lpg_ra+(lpg_nt-1).*lpg_ri)+1);
ad_range=zl; ad_w=zl; ad_code=zl; ad_lpg=zl;
ad_coeff=zl; ad_lag=zl; ad_bac=zl;
for lpgs=1:length(lpg_ra)
  addr=lpg_addr(lpgs)+ADDR_SHIFT; % To change from radar to Matlab addressing
  la=length(addr);
  if ismember(lpg_bcs(lpgs),lpg_s)
    ad_range(addr)=lpg_h(lpgs)+(0:lpg_nt(lpgs)-1)'*lpg_dt(lpgs);
    ad_w(addr)=repmat(lpg_w(lpgs),1,la);
  end
  ad_code(addr)=repmat(lpg_code(lpgs),1,la);
  ad_lpg(addr)=repmat(lpgs,1,la);
  ad_lag(addr)=repmat(lpg_lag(lpgs),1,la);
  ad_bac(addr)=repmat(lpg_bac(lpgs),1,la);
  ad_bcs(addr)=repmat(lpg_bcs(lpgs),1,la);
end

if ~isempty(a_code)
%may need to change code for calibration purposes
 lpgs=find(ismember(lpg_code,a_code) & isfinite(lpg_cal));
 lpgc=unique([lpg_cal(lpgs);lpg_bac(lpgs)]);
 lpgc=lpgc(find(lpgc));
 lpgneed=lpgc(find(~ismember(lpg_code(lpgc),a_code)));
 ad_code(lpg_addr(lpgneed')+ADDR_SHIFT)=a_code(1);
 lpg_code(lpgneed)=a_code(1);
end
