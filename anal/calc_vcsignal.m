% calc_vcsignal.m: A function to calculate the signal strength
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% A function to calculate the signal strength for all virtual channels and for all
% lag values up to the length of p*env
% Input parameters:
% addr: A reference address to give the radar factor
% T_noise: Background temperature in K
% param: plasma parameters in scaled units
% Output parameter: (global)
% vc_signal: signal strength for all virtual channels and lag values
% See also: ACF, real_to_scaled, adgr_covar, adgr_var, addr_covar
%function calc_vcsignal(addr,T_noise,param)
function calc_vcsignal(addr,T_noise,param)

global vc_signal vc_group vc_Apenv vc_Ap ad_coeff ADDR_SHIFT

% Faster calculation possible if results calculated for virtual channel groups
% One group contains all virtual channels with identical transmission and filters
[a,ind]=windex(diff_val(vc_group),vc_group);
vcg_Apenv=vc_Apenv(:,ind);
vcg_Ap=vc_Ap(:,ind);
[M,N]=size(vcg_Apenv);
len_vcsig=M;

% Calculate the plasma autocorrelation function for all lags up to length of p*env
ac=col(real(ACF(param,[0:M-1])));
% Multiply by effective pulse length (vcg_Apenv) and radar factor
K=ad_coeff(addr+ADDR_SHIFT)*(vcg_Apenv.*ac(:,ones(1,N)));

% Background power for all lags
B=zeros(size(vcg_Apenv));
[M2,N2]=size(vcg_Ap);B(1:M2,1:N2)=vcg_Ap;
apu=T_noise./B(1,:);
B=B.*apu(ones(M,1),:);

vcg_signal=K+B; % Result is sum of plasma and background contributions
vc_signal=vcg_signal(:,vc_group); % Store to all virtual channels for fast reference
