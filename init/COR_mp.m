% COR_mp: Defines lag profiles for decoded multipulse experiments
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
%
% User specified input parameters:
% ra      : First result memory address to use
% ri      : result memory address increment
% vc      : virtual channel number
% type    : lag profile type, 's','b','c' and 'o' allowed 
% gating  : number of successive crossed products added to the same memory location
% N_gates : number of gates calculated
% lags    : Lag  values in us
% N_skipped : number of samples skipped at the beginning of the sample vector (often =0)
% code     : user specified code number
% Output is stored in the lp_xxx parameters (global)
%
% See also: CORR_mp, COR_check, COR_caltemp, COR_status, pulse_times, sample_times
%
%function COR_mp(ra,ri,vc,type,gating,N_gates,lags,N_skipped,code)
 function COR_mp(ra,ri,vc,type,gating,N_gates,lags,N_skipped,code)

global p_dtau vc_adcint vc_mf
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev

if (type~='b' & type~='c' & type~='s' & type~='o'),
  error(' Unknown data type in COR_mp')
end  

ra_prev=ra;

adcint=vc_adcint(vc); lags=lags/p_dtau; 
COR_check(lags,adcint)

N_lags=length(lags);
index=(lp_ind+1):(lp_ind+N_lags);
lp_T(index)=COR_caltemp(type)*ones(1,N_lags);

% find first when pulses are transmitted and samples are taken
pulsetimes=pulse_times(vc)';
fs_ti=sample_times(vc,type2ind(type));fs_ti=fs_ti(1);
if vc_mf(vc)>0, % Compensate for the delay caused by matched filter
  t1=t1+(vc_mf(vc)-1)*adcint;
end

apu=pulsetimes(1)-pulsetimes;
codelags=[0];codeskips=[0];len=length(pulsetimes);
for i=1:len-1;
  codelags=[ codelags,pulsetimes(i+1:len)-pulsetimes(i)];
  codeskips=[codeskips,apu(i)*ones(1,len-i)];
end
[codelags,ind]=sort(codelags);codeskips=codeskips(ind);
[codelags',codeskips'];
t1=[];
for lag=lags,
  ind=find(codelags==lag);
  if length(ind)==1,
    tt=fs_ti+N_skipped*adcint-codeskips(ind);
  else % for missing lags in the code
    tt=fs_ti+N_skipped*adcint;
  end
  t1=[t1,tt];    
end

lp_t1(index)=t1;
lp_h(index)=t1-pulsetimes(1);
  
% finally parameters common to all types
lp_t2(index)=t1+lags;
lp_dt(index)=adcint*ones(1,N_lags);
lp_nfir(index)=gating*ones(1,N_lags);
lp_fir(1:gating,index)=ones(gating,N_lags);
lp_dec(index)=gating*ones(1,N_lags);
lp_nt(index)=N_gates*ones(1,N_lags);
lp_vc(index)=vc*ones(1,N_lags);
lp_ra(index)=ra+(0:N_lags-1);
lp_ri(index)=ri*ones(1,N_lags);
lp_bcs(index)=type*ones(1,N_lags);
lp_code(index)=code*ones(1,N_lags);
lp_ind=lp_ind+N_lags;

% first result memory location not used
ra_next=max(lp_ra(index)+(lp_nt(index)-1).*lp_ri(index))+1;

COR_status('   COR_mp',vc,type,index)
