% find_om_grid.m: finds a suitably dense omega axis
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The frequency axis (p_om) for the spectral ambiguity function is chosen so 
% that the spectral resolution is sufficent for all cases. Therefore the
% resolution is much too good when the spectrum is wide. This function checks
% the oversampling prior to data analysis and returns spectral ambiguity
% function and frequency axis  which are sufficient but not too good.
%
% See also: half_prof
%
% function [womega,om]=find_om_grid(p,f_womega,kd2,p_om,pldfvv)
function [womega,new_om]=find_om_grid(p,f_womega,kd2,p_om,pldfvv,nostep)

global p_m0

[nin0,tit0,mim0,psi,vi]=transf(p,p_m0);
s=spec(nin0,tit0,mim0,psi,vi,kd2,p_om,pldfvv);
[M,N]=size(f_womega);
N=floor(N/2)+1;

lenp=length(p_om);
start=1;
istep=1;
ii=[];
if ~nostep
  for i=2:(lenp-1)/2
    tii=(lenp-1)/i;
    if tii==fix(tii), ii=[ii i]; end
  end
end
for i=[1 ii]
  ind=[fliplr(N-i:-i:start) N:i:lenp-start+1];
  om=p_om(ind); len=length(om);
  dom=0.5*[om(2)-om(1);om(3:len)-om(1:len-2);om(len)-om(len-1)];
  theo=f_womega(:,ind)*(s(ind).*dom);
  if i==1
    theoref=theo;
  else
    err=max(abs(theoref-theo))/max(abs(theo));
%   fprintf(' step %.0f, error %.4f\n',i, err);
    if err>1.5e-3, break, end
  end
  istep=i; new_om=om; new_ind=ind; new_dom=dom';
end
%if istep>1, fprintf(' Using a coarser frequency axis, one of every %d point used\n',istep), end
womega=f_womega(:,new_ind).*new_dom(ones(M,1),:);
