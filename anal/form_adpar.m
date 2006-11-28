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

global lpg_ra lpg_nt lpg_ri lpg_bcs lpg_h lpg_dt lpg_w lpg_code lpg_lag
global ad_range ad_w ad_code ad_lpg ad_coeff ad_lag ADDR_SHIFT
global a_code lpg_bac lpg_cal

len=max(lpg_ra+(lpg_nt-1).*lpg_ri)+1;
ad_range=zeros(1,len); ad_w=zeros(1,len);
ad_code=zeros(1,len); ad_lpg=zeros(1,len);
ad_coeff=zeros(1,len); ad_lag=zeros(1,len);
for lpgs=1:length(lpg_ra)
  addr=lpg_addr(lpgs)+ADDR_SHIFT; % To change from radar to Matlab addressing
  len=length(addr);
  ol=ones(len,1);
  if lpg_bcs(lpgs)=='s'
    ad_range(addr)=lpg_h(lpgs)+(0:lpg_nt(lpgs)-1)'*lpg_dt(lpgs);
    ad_w(addr)=lpg_w(lpgs)*ol;
  end
  ad_code(addr)=lpg_code(lpgs)*ol;
  ad_lpg(addr)=lpgs*ol;
  ad_lag(addr)=lpg_lag(lpgs)*ol;
end

if ~isempty(a_code)
%may need to change code for calibration purposes
 lpgs=find(ismember(lpg_code,a_code));
 lpgc=unique([lpg_cal(lpgs);lpg_bac(lpgs)]);
 lpgc=lpgc(find(lpgc));
 lpgneed=lpgc(find(~ismember(lpg_code(lpgc),a_code)));
 ad_code(lpg_addr(lpgneed')+ADDR_SHIFT)=a_code(1);
 lpg_code(lpgneed)=a_code(1);
end
