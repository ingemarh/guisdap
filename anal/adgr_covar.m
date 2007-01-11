% adgr_covar: the error covariance matrix for two result memory address sets
% GUISDAP v.8.5 07-01-14 Copyright EISCAT, Huuskonen&Lehtinen
%
% A function to calculate the error covariance matrix for two sets of result
% memory addresses. If the two sets are identical, only diagonal and upper 
% (or lower) triangle is calculated. The covariance calculations are performed
% by the routine addr_covar
% Input parameters:
% T_noise: background temperature in K
% param: plasma parameters in scaled units
% addr1: first set of result memory addresses
% addr2: second set of result memory addresses (optional)
% vc_signal: signal strength for all virtual channels and for all lags (global)
% Output parameters
% covarRe: covariance between the real parts of the two sets
% covarIm: covariance between the imaginary parts of the two sets
% See also: addr_covar, adgr_var, calc_vcsignal, real_to_scaled
%function [covarRe,covarIm]=adgr_covar(T_noise,param,addr1,addr2)
function [covarRe,covarIm]=adgr_covar(T_noise,param,addr1,addr2)

global vc_signal lp_vc lp_dt lp_ra lp_ri lp_nt lp_t1 lp_t2 lp_dec lp_nfir lp_fir
calc_vcsignal(addr1(1),T_noise,param)
l1=length(addr1);
if nargin<4
 % Execution time saved if both sets are equal as Cov(A,B)=Cov(B,A)
 % Then only diagonal and upper (or lower) triangle calculated.
  addr2=addr1; identical=1; l2=l1;
else
  identical=0; first=1; l2=length(addr2);
end
covarRe=zeros(l1,l2);
covarIm=zeros(l1,l2);
for i=1:l1
  if identical, first=i; end
  for j=first:l2
    [covarRe(i,j),covarIm(i,j)]=addr_covar(addr1(i),addr2(j),vc_signal,lp_vc,...
                     lp_dt,lp_ra,lp_ri,lp_nt,lp_t1,lp_t2,lp_dec,lp_nfir,lp_fir);
    if identical
      covarRe(j,i)=covarRe(i,j);
      covarIm(j,i)=covarIm(i,j);
    end
  end
end
