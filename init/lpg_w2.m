% lpg_w2: Two-dimensional range-lag and range-frequency ambiguity functions
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% function to calculate the two-dimensional range-lag 
% and range-frequency ambiguity functions
% for a lag profile group
%
% Output:
% ww_om : two-dimensional range-omega ambiguity function
% wS    : range axis (common to ww_om and ww_lag)
% om    : frequency axis
% ww_lag: two-dimensional range-lag ambiguity function
% wL    : lag axis
%
% See also: w2om
%
%  function [ww_om,wS,om,ww_lag,wL]=lpg_w2(lpg)
  function [ww_om,wS,om,ww_lag,wL]=lpg_w2(lpg)

global lp_t1 lp_t2 lp_dt lp_vc lp_nfir lp_fir

wL=[];
for lp=lpg_lp(lpg)
  fprintf(' %.0f', lp)
  [wwom,r,om,ww,L]=w2om(lp_vc(lp),lp_t1(lp),lp_t2(lp));
  if isempty(wL),
    wL=L;
    wom=om;
    ww_lag=zeros(size(ww));
    ww_om=zeros(size(wwom));
  elseif max(wL)<max(L) | min(wL)>min(L)
    rsh=max(L)-max(wL); lsh=min(wL)-min(L);
    if rsh>0
      ww_lag=[ww_lag zeros(size(ww_lag,1),rsh)];
      wL=[wL L(end-rsh+1:end)];
    end
    if lsh>0
      ww_lag=[zeros(size(ww_lag,1),lsh) ww_lag];
      wL=[L(1:lsh) wL];
    end
  elseif any(wom~=om)
    fprintf(' Sorry, cannot handle changing frequency supports yet\n')
    return
  end
  if length(r)>0,
    maxr=r(end)+(lp_nfir(lp)-1)*lp_dt(lp);
    if rows(ww_lag)<maxr,
      ww_lag(rows(ww_lag)+1:maxr,:)=0;
      ww_om(rows(ww_om)+1:maxr,:)=0;
    end;
    ii=[];
    for j=L, ii=[ii find(j==wL)]; end
    for ind=1:lp_nfir(lp)
      R=r+(ind-1)*lp_dt(lp);
      ww_lag(round(R),ii)=ww_lag(round(R),ii)+lp_fir(ind,lp)*ww;
      ww_om(round(R),:)=ww_om(round(R),:)+lp_fir(ind,lp)*wwom;
    end
  else
    fprintf('\nFor lag profile %.0f the range ambiguity function is empty\n',lp)
  end
end
fprintf('\n')
wS=1:rows(ww_lag);
