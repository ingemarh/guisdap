% scale_lpgwom.m: spectral ambiguity functions scaled by lpg_ND
% GUISDAP v.8.4 04-12-22 Copyright EISCAT, Huuskonen&Lehtinen
%
% Divides the spectral ambiguity functions by the correlator algorithm factors lpg_ND
% The balances the similar operation done to data by function scale_data
%
% Also calculates normalised lpg_Ap
%
% See also: scale_data
%
% function scale_lpgwom
function scale_lpgwom

global lpg_wom lpg_womscaled lpg_bcs lpg_ND
global lpg_Ap lp_vc vc_Ap lpg_lag lpg_bac

lpg_womscaled=zeros(size(lpg_wom));
for lpg=find(lpg_bcs=='s')
  lpg_womscaled(lpg,:)=lpg_wom(lpg,:)/lpg_ND(lpg);
end

lpg_Ap=zeros(length(lpg_lag),1);
for lpg=find(lpg_bac)
  vc=lp_vc(lpg_lp(lpg));
  lpg_Ap(lpg)=mean(Ap(vc,lpg_lag(lpg))./(Ap(vc,0)),2);
end
