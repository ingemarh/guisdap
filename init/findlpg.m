% findlpg.m: Produce the lag profile groups.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% We assume that two lag profiles belong to the same lag profile group
% if the start addresses lp_ra agree. The program checks that various other
% parameters are equal in the lag profiles. A hidden assumption is that 
% addresses attached to a lag profile group do not belong to any other group.
%
% See also: init_GUP
%
fprintf(['\n\nProducing the lag profile groups: ...\n\n'])
lpg_ra=diff_val(lp_ra); % find all different values
len=length(lpg_ra);
lpg_lag=zeros(1,len);lpg_dt=zeros(1,len);lpg_ND=zeros(1,len);lpg_T=zeros(1,len);
lpg_ri=zeros(1,len); lpg_nt=zeros(1,len);lpg_h=zeros(1,len);lpg_w=zeros(1,len);
lpg_bcs=zeros(1,len);lpg_code=zeros(1,len);
lpg_lpdata=zeros(1,len); lpg_lpind=0;
lpg_lpstart=zeros(1,len); lpg_lpend=zeros(1,len);
ad_lpg=[];
for ind=1:length(lpg_ra),
  lpg=find(lp_ra==lpg_ra(ind));
  lenlpg=length(lpg);
  lpg_lpdata(lpg_lpind+(1:lenlpg))=lpg;
  lpg_lpstart(ind)=lpg_lpind+1; lpg_lpend(ind)=lpg_lpind+lenlpg;
  lpg_lpind=lpg_lpind+lenlpg;
  lpg_lag(ind)=cheq(lp_t2(lpg)-lp_t1(lpg));
  lpg_dt(ind)=cheq(lp_dt(lpg).*lp_dec(lpg));
  lpg_ND(ind)=sum(sum(abs(lp_fir(:,lpg))));
  lpg_T(ind)=cheq(lp_T(lpg));
  lpg_ri(ind)=cheq(lp_ri(lpg));
  lpg_nt(ind)=cheq(lp_nt(lpg));
  lpg_h(ind)=mean(lp_h(lpg));
% note that this range value will be updated for signal lag profiles
% after the range ambiguity functions are calculated
  lpg_bcs(ind)=cheq(lp_bcs(lpg));
  lpg_code(ind)=cheq(lp_code(lpg));

  fprintf(['lpg=%3.0f code=%1.0f type=',setstr(lpg_bcs(ind))],ind,lpg_code(ind));
  fprintf(' lag=%3.0f dt=%3.0f', p_dtau*lpg_lag(ind), p_dtau*lpg_dt(ind));
  fprintf(' ND=%2.0f h=%5.0f',  lpg_ND(ind), p_dtau*lpg_h(ind));
  fprintf(' T=%3.0f nt=%3.0f',  lpg_T(ind),   lpg_nt(ind));
  fprintf(' ra=%3.0f ri=%2.0f',lpg_ra(ind), lpg_ri(ind));
  fprintf('\n');

  addr=lpg_addr(ind);sto=addr+1;
  if max(sto)>length(ad_lpg); ad_lpg(max(sto))=0; end
  index=find(ad_lpg(sto)~=0);
  if length(index)>0,
     fprintf('\n\n Conflict in the lag profile definition\n')
     fprintf(' Lag profile group %.0f\n tries to define addresses\n',ind)
     fprintf(' %5.0f',addr(index))
     fprintf('\n which already belong to lag profile groups\n')
     fprintf(' %5.0f',ad_lpg(sto(index)))
     fprintf('\n')
     error(' ')
  else
     ad_lpg(sto)=ind*ones(size(addr));
  end
end;
clear lpg ind index len lenlpg lpg_lpind addr sto ad_lpg
