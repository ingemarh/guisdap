% COR_frac: defines lag profiles for decoded alternating code experiments
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% COR_frac defines lag profiles for fractionally sampled alternating code experiments.
% The lag profile tails (where all the lags up to the longest are not obtained) are included
%
% User specified input parameters:
% ra      : Result memory addresses for the first gate, one address for each lag.
% ri      : result memory address increment, assumed equal for all lags
% vc      : virtual channel number
% type    : lag profile type, which MUST be signal ('s') 
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
%function COR_alter(ra,ri,vc,type,N_gates,Nbits,bitsep,lags,N_skipped,code)
 function COR_alter(ra,ri,vc,type,N_gates,Nbits,bitsep,lags,N_skipped,code)

global vc_adcint vc_sampling vc_samplingend vc_ba bm_samples bm_next vc_mf
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev

if (type~='s'),  error(' Unknown data type in COR_frac'), end  

ra_prev=ra;

adcint=vc_adcint(vc); lags=lags/p_dtau; bitsep=bitsep/p_dtau;
full_lags=[];
lag_value=[];
index=1;
for ind=1:length(lags);
  if rem(lags(ind),bitsep)==0, % Full lag
    lag_value(index)=lags(ind);
    full_lags(index)=lags(ind);
    index=index+1;
  else % Fractional lag
    if lags(ind)<bitsep % lag shorter than lag one
      lag_value(index)=lags(ind);
      full_lags(index)=bitsep;
      index=index+1;
    elseif lags(ind)>(Nbits-1)*bitsep % longer than longest full lag
      lag_value(index)=lags(ind);
      full_lags(index)=(Nbits-1)*bitsep;
      index=index+1;
    else % normal fractional lags
      lag_value(index+[0,1])=[lags(ind),lags(ind)];
      full_lags(index+[0,1])=floor(lags(ind)/bitsep)*bitsep+[0,bitsep];
      index=index+2;
    end
  end
end
lags=lag_value;   
ra=ra(1)+(0:(length(lags)-1));
ri=length(lags);

COR_check(full_lags,adcint,bitsep)
if rem(bitsep,adcint)
  error(' Bit separation must be an exact multiple of adc interval')
end

N_lags=length(lags);
N_lp=N_lags;
index=(lp_ind+1):(lp_ind+N_lp);
lp_T(index)=COR_caltemp(type)*ones(1,N_lp);

% find first when pulses are transmitted and samples are taken
pulsetimes=pulse_times(vc);
am=env(vc,pulsetimes(1)+[0:(Nbits-1)]*bitsep+1)'; % 1 added to get +-1 as the result

fs_time=sample_times(vc,type2ind(type));
t1=(fs_time(1)+N_skipped*adcint)*ones(1,N_lp);
lp_t1(index)=t1;
lp_h(index)=t1-pulsetimes(1);

lp_t2(index)=t1+lags;
lp_dt(index)=adcint*ones(1,N_lp);

SB=bitsep/adcint; % Samples per bit in the alternating code
apu(index)=Nbits-round(full_lags/bitsep);
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
lp_dec(index)=ones(1,N_lp);

lp_T(index)=zeros(1,N_lp);
lp_nt(index)=N_gates*ones(1,N_lp);
lp_vc(index)=vc*ones(1,N_lp);
lp_ra(index)=ra;
lp_ri(index)=ri*ones(1,N_lp);
lp_bcs(index)=type*ones(1,N_lp);
lp_code(index)=code*ones(1,N_lp);
lp_ind=lp_ind+N_lp;

% first result memory location not used
ra_next=max(lp_ra(index)+(lp_nt(index)-1).*lp_ri(index))+1;

COR_status('COR_frac ',vc,type,index)

% Remove the following line if the lag profile tails are also wanted
return
% Now defining lag profile tails.
ind=index(lp_nfir(index)>SB);
while ~isempty(ind)
  % Cut SB coefficients from each filter which is longer than SB
  N_lp=length(ind);
  index=(lp_ind+1):(lp_ind+N_lp);
  lp_T(index)=lp_T(ind);
  lp_t1(index)=lp_t1(ind);
  lp_h(index)=lp_h(ind);
  lp_t2(index)=lp_t2(ind);
  lp_dt(index)=lp_dt(ind);
  lp_nfir(index)=lp_nfir(ind)-SB;
  lp_fir(1:lp_nfir(index),index)=lp_fir((SB+1):lp_nfir(ind),ind);
  lp_dec(index)=lp_dec(ind);
  lp_nt(index)=SB*ones(1,N_lp);
  lp_vc(index)=lp_vc(ind);
  lp_ra(index)=(lp_ra(ind)-lp_ra(ind(1)))+ra_next;
  lp_ri(index)=N_lp*ones(1,N_lp);
  lp_bcs(index)=lp_bcs(ind);
  lp_code(index)=lp_code(ind);
  lp_ind=lp_ind+N_lp;

  % first result memory location not used
  ra_next=max(lp_ra(index)+(lp_nt(index)-1).*lp_ri(index))+1;

  COR_status('COR_frac ',vc,type,index)
  
  ind=index(lp_nfir(index)>SB);

end
