% COR_alter: defines lag profiles for decoded alternating code experiments. 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% COR_alter defines lag profiles for decoded alternating code experiments. It specifies
% the gates where all lags, up to the longest one, are available. It is further restricted
% to the case, where the lag values are exact multiples of the bit separation.
%
% User specified input parameters:
% ra      : Result memory addresses for the first gate, one address for each lag.
% ri      : result memory address increment, assumed equal for all lags
% vc      : virtual channel number
% type    : lag profile type, which MUST be signal ('s') 
% gating  : number of successive crossed products added to the same memory location
%           This is ofter used with oversampling
% N_gates : number of gates to be spedified
% Nbits   : Number of bits in the alternating code
% bitsep  : distance of leading edges of code bauds (optional).
%           The value equals the bit length often, but not e.g. for interlaced alternating codes 
% lags    : Lag  values in us
% N_skipped : number of samples skipped at the beginning of the sample vector (often =0)
% code     : user specified code number
% Output is stored in the lp_xxx parameters (global)
%
% See also: CORR_alter, COR_check, COR_caltemp, COR_status, pulse_times, sample_times
%
%function COR_alter(ra,ri,vc,type,gating,N_gates,Nbits,bitsep,lags,N_skipped,code)
 function COR_alter(ra,ri,vc,type,gating,N_gates,Nbits,bitsep,lags,N_skipped,code)

global vc_adcint vc_sampling vc_samplingend vc_ba bm_samples bm_next vc_mf
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev

if (type~='s'),  error(' Unknown data type in COR_alter'), end  

ra_prev=ra;

adcint=vc_adcint(vc); lags=lags/p_dtau; bitsep=bitsep/p_dtau;
COR_check(lags,adcint,bitsep)
if rem(bitsep,adcint)
  error(' Bit separation must be an exact multiple of adc interval')
end

N_lags=length(lags);
N_lp=N_lags*gating;
index=(lp_ind+1):(lp_ind+N_lp);
lp_T(index)=COR_caltemp(type)*ones(1,N_lp);

% find first when pulses are transmitted and samples are taken
pulsetimes=pulse_times(vc);
am=env(vc,pulsetimes(1)+[0:(Nbits-1)]*bitsep+1)'; % 1 added to get +-1 as the result

fs_time=sample_times(vc,type2ind(type));
t1=(fs_time(1)+N_skipped*adcint)+kron(ones(1,N_lags),(0:gating-1)*adcint);
lp_t1(index)=t1;
lp_h(index)=t1-pulsetimes(1);

lp_t2(index)=t1+kron(lags,ones(1,gating));
lp_dt(index)=adcint*ones(1,N_lp);

SB=bitsep/adcint; % Samples per bit in the alternating code
apu(index)=kron(Nbits-round(lags/bitsep),ones(1,gating));
lp_nfir(index)=(apu(index)-1)*SB+1;
for ind=index
  Ntaps=apu(ind);
  signs=am(1:Ntaps).*am(Nbits-Ntaps+1:Nbits);
  if SB==1,
    lp_fir(1:lp_nfir(ind),ind)=signs;
  else
    lp_fir(1:lp_nfir(ind),ind)=[kron(signs(1:Ntaps-1),[1;zeros(SB-1,1)]);signs(Ntaps)];
  end   
end
lp_dec(index)=gating*ones(1,N_lp);

lp_T(index)=zeros(1,N_lp);
lp_nt(index)=N_gates*ones(1,N_lp);
lp_vc(index)=vc*ones(1,N_lp);
lp_ra(index)=kron(ra,ones(1,gating));
lp_ri(index)=ri*ones(1,N_lp);
lp_bcs(index)=type*ones(1,N_lp);
lp_code(index)=code*ones(1,N_lp);
lp_ind=lp_ind+N_lp;

% first result memory location not used
ra_next=max(lp_ra(index)+(lp_nt(index)-1).*lp_ri(index))+1;

COR_status('COR_alter',vc,type,index)
