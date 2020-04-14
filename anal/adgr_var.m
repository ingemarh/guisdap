% adgr_var(addr,T_noise,param): variance for a set of result memory addresses.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The covariance calculations are performed by the routine addr_covar
% Input parameters:
% addr: set of result memory addresses
% T_noise: background temperature in K
% param: plasma parameters in scaled units
% vc_signal: signal strength for all virtual channels and for all lags (global)
% Output parameters
% varRe: covariance between the real parts of the two sets
% varIm: covariance between the imaginary parts of the two sets
% See also: addr_covar, adgr_covar, calc_vcsignal, real_to_scaled
%function [varRe,varIm]=adgr_var(addr,T_noise,param)
function [varRe,varIm]=adgr_var(addr,T_noise,param)

global vc_signal lp_vc lp_dt lp_ra lp_ri lp_nt lp_t1 lp_t2 lp_dec lp_nfir lp_fir
calc_vcsignal(addr(1),T_noise,param)
[varRe,varIm]=addr_covar(addr,-1,vc_signal,lp_vc,lp_dt,lp_ra,...
         lp_ri,lp_nt,lp_t1,lp_t2,lp_dec,lp_nfir,lp_fir);
