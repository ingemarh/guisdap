% lpgwomcalc.m: calculates the spectral ambiguity functions for all signal lpg's
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% See also: lpgwom
lpg_wom=zeros(length(lpg_bcs),length(p_om));
fprintf('\n*\n* Calculating spectral ambiguity functions for signal lpg:s\n*\n*');

for lpg=find(lpg_bcs=='s');
  fprintf(' %.0f',lpg),
  lpg_wom(lpg,:)=lpgwom(lpg);
end  
fprintf('\n*\n* spectral ambiguity functions calculated\n*\n')
clear lpg
