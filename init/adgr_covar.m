% adgr_covar: the error covariance matrix for two result memory address sets
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% A function to calculate the error covariance matrix for two sets of result
% memory addresses. If the two sets are identical, only diagonal and upper 
% (or lower) triangle is calculated. The covariance calculations are performed
% by the routine addr_covar
% Input parameters:
% addr1: first set of result memory addresses
% addr2: second set of result memory addresses
% T_noise: background temperature in K
% param: plasma parameters in scaled units
% vc_signal: signal strength for all virtual channels and for all lags (global)
% Output parameters
% covarRe: covariance between the real parts of the two sets
% covarIm: covariance between the imaginary parts of the two sets
% See also: addr_covar, adgr_var, calc_vcsignal, real_to_scaled
%function [covarRe,covarIm]=adgr_covar(addr1,addr2,T_noise,param)
function [covarRe,covarIm]=adgr_covar(addr1,addr2,T_noise,param)

global vc_signal lp_vc lp_dt lp_ra lp_ri lp_nt lp_t1 lp_t2 lp_dec lp_nfir lp_fir
calc_vcsignal(addr1(1),T_noise,param)

covarRe=zeros(length(addr1),length(addr2));
covarIm=zeros(length(addr1),length(addr2));

% Execution time saved if both sets are equal as Cov(A,B)=Cov(B,A)
% Then only diagonal and upper (or lower) triangle calculated.
if length(addr1)==length(addr2),
  if all(addr1==addr2), identical=1; else, identical=0; end
end

for i=1:length(addr1)
  if identical, first=i; else, first=1; end
%		fprintf(' %.0f',i);
  for j=first:length(addr2) 
      [covarRe(i,j),covarIm(i,j)]=...
        addr_covar(addr1(i),addr2(j),vc_signal,lp_vc,lp_dt,lp_ra,...
         lp_ri,lp_nt,lp_t1,lp_t2,lp_dec,lp_nfir,lp_fir);
      if identical,
        covarRe(j,i)=covarRe(i,j);
        covarIm(j,i)=covarIm(i,j);
      end
  end
end
% fprintf(' \n')
