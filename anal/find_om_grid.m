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
% function [womega,om]=find_om_grid(p,p_coeffg,f_womega,kd2,p_om,pldfvv);
function [womega,om]=find_om_grid(p,p_coeffg,f_womega,kd2,p_om,pldfvv);

global p_m0

[nin0,tit0,mim0,psi,vi]=transf(p,p_m0);
s=spec(nin0,tit0,mim0,psi,vi,kd2,p_om,pldfvv);
[M,N]=size(f_womega);
N=floor(N/2)+1;

len=length(p_om);
dom=0.5*[p_om(2)-p_om(1); p_om(3:len)-p_om(1:len-2); p_om(len)-p_om(len-1)];
theoref=f_womega*(s.*dom);

start=1;
%for i=[2:4:20],
%  om=p_om(i:1:length(p_om)-i+1);
%  len=length(om);
%  dom=0.5*[om(2)-om(1); om(3:len)-om(1:len-2); om(len)-om(len-1)];
%  theo=f_womega(:,i:1:length(p_om)-i+1)*(s(i:1:length(p_om)-i+1).*dom);
%  err=max(abs(theoref-theo))/max(abs(theo)); 
%%  disp([i, err]);
%  if err>1e-3, break; end;
%  start=i;
%end;


istep=1;
for i=[2:6 8:2:20 40],
  ind=N:i:length(p_om)-start+1;
  ind=[fliplr(N-i:-i:start), ind];
  om=p_om(ind);
  len=length(om);
  dom=0.5*[om(2)-om(1); om(3:len)-om(1:len-2); om(len)-om(len-1)];
  theo=f_womega(:,ind)*(s(ind).*dom);
  err=max(abs(theoref-theo))/max(abs(theo)); 
%  fprintf(' step %.0f, error %.4f\n',i, err);
  if err>1.5e-3, break; end;
  istep=i;
end;
%if istep>1, fprintf(' Using a coarser frequency axis, one of every %.0g point used\n',istep), end
om=p_om(ind);
len=length(om);
dom=0.5*[om(2)-om(1); om(3:len)-om(1:len-2); om(len)-om(len-1)]';
womega=f_womega(:,ind).*dom(ones(M,1),:);
