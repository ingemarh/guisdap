% scale_lpgwom.m: spectral ambiguity functions scaled by lpg_ND
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Divides the spectral ambiguity functions by the correlator algorithm factors lpg_ND
% The balances the similar operation done to data by function scale_data
%
% See also: scale_data
%
% function scale_lpgwom
function scale_lpgwom

global lpg_womscaled lpg_wom lpg_bcs lpg_ND

lpg_womscaled=zeros(size(lpg_wom));
for lpg=find(lpg_bcs=='s')
  lpg_womscaled(lpg,:)=lpg_wom(lpg,:)/lpg_ND(lpg);
end  
