% wrlpg.m: calculates the range ambiguity function for a lag profile group
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% See also: lpgwrcalc wr
%
%  function wsum=wrlpg(lpg)
  function wsum=wrlpg(lpg)

global lp_t1 lp_t2 lp_dt lp_vc lp_nfir lp_fir
global name_site
global p_R0 p_dtau ipp vc_routine
dummyrange=0;
if name_site=='R',
% dummyrange to avoid negative range, should maybe be > offsetppd
  dummyrange=ceil(p_R0/p_dtau);
end
wsum=0; p2p=0; clash=0;
lps=lpg_lp(lpg);
if strmatch('COR_pulsetopulse',vc_routine(lp_vc(lps)))
  if isempty(ipp)
    error('Pulse-to-pulse, but no ipp defined');
  end
  clash=ipp;
  disp('Pulse-to-pulse: Ignoring distant targets')
end
for lp=lps
  [w,r]=wr(lp_vc(lp),lp_t1(lp),lp_t2(lp),clash);
  d=find(r+dummyrange<=0);
  if length(d)>0
    warning('Got negative ranges, ignoring')
    w(d)=[]; r(d)=[];
  end
  if length(r)>0
    maxr=r(end)+(lp_nfir(lp)-1)*lp_dt(lp)+dummyrange;
    %if abs(maxr-round(maxr))<eps(2), maxr=round(maxr); end
    if length(wsum)<maxr, wsum(ceil(maxr),1)=0; end;
    for ind=1:lp_nfir(lp)
      R=round(r+(ind-1)*lp_dt(lp)+dummyrange);
      wsum(R)=wsum(R)+double(lp_fir(ind,lp))*w;
    end
  else
    fprintf('For lag profile %.0f the range ambiguity function is empty\n',lp)
  end
end
