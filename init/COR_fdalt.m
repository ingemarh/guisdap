% COR_fdalt: algorithm for decoded alternating code experiments (lag profile format)
% GUISDAP v.8.4 05-04-27 Copyright EISCAT
%
% COR_fdalt defines lag profiles of frequency domain decoding.
% The lag profile tails are included
% The results are arranged in the lag profile format
%
% User specified input parameters:
% ra      : Result memory addresses for the first point
% vc      : virtual channel number
% type    : lag profile type, which MUST be signal ('s') 
% N_gates : number of fully decoded gates
% NlowG   : Size of the lower tail
% NupG    : Size of the upper tail
% Nbits   : Number of bits in the each code
% Sample_skip : Number of points skipped in the sample vector
% lags    : Lag  values in us
% code    : user specified code number
% Output is stored in the lp_xxx parameters (global)
%
% See also: COR_arclp, COR_fraclp, COR_check, COR_status, pulse_times, sample_times
%
%function COR_fdalt(ra,vc,type,N_gates,NlowG,NupG,Nbits,Sample_skip,lags,code)
function COR_fdalt(ra,vc,type,N_gates,NlowG,NupG,Nbits,Sample_skip,lags,code)

global vc_adcint vc_sampling vc_samplingend vc_ba bm_samples bm_next vc_mf
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev

if (type~='s'),  error(' Unknown data type in COR_fdalt'), end  

% Store the first result memory address (calling program might need it)
ra_prev=ra;
ra0=ra;

% Store the ADC interval into a local variable
adcint=vc_adcint(vc);

% Transform the user specified lag values to p_dtau units
% and check that they are exact multiples of it
lags=lags/p_dtau;
COR_check(lags,adcint)
bitsep=adcint;

% This is the heart of the routine. Here we define how each lag profile is filtered. 

% find first when pulses are transmitted
pulsetimes=pulse_times(vc);
% the signs of the alternating code bits are obtained here
am=env(vc,pulsetimes(1)+[0:(Nbits-1)]*bitsep+1)'; % 1 added to get +-1 as the result
% Sampling start time
fs_time=sample_times(vc,type2ind(type))+Sample_skip*adcint;
pulse_len=length(am);

index=lp_ind+1; % This is the lag profile number to be used next
% Now we are ready to handle one lag profile at a time
for ind=1:length(lags)
  lag=round(lags(ind)/adcint);
  if lag==0
    NG=N_gates+1;
  else
    NG=N_gates;
  end
  lpp=[-fliplr(1:NlowG-lag) 0 1:NupG-lag];
  ra=ra0+max([lag+NlowG-Nbits 0]); % Set the counter
  ra0=ra0+NlowG+NG+NupG;
  for i=lpp
    %Find the filtering coefficients
    if i<=0 % lower tail + full
      sgns=am(1-i:end);
      lp_t1(index)=fs_time(1);  % Sample time of the first factor
    else % upper tail
      sgns=am(1:end-i);
      lp_t1(index)=fs_time(1)+(NG+i-1)*adcint; % Sample time of the first factor
    end
    lp_t2(index)=lp_t1(index)+lags(ind); % Sample time of the second factor
    if lag==0
      signs=sgns(2:end).*sgns(1:end-1);
    else
      signs=sgns(lag+1:end).*sgns(1:end-lag);
    end
    lp_nfir(index)=length(signs);
    lp_fir(1:lp_nfir(index),index)=signs;
    lp_dec(index)=1;
    lp_h(index)=lp_t1(index)-pulsetimes(1); % Estimate of the first range (not very good)
    lp_dt(index)=adcint;     % Range separation of samples 

    lp_ra(index)=ra;         % Start address of the lag profile
    lp_ri(index)=1;          % Successive locations always used
    if i==0
      lp_nt(index)=NG; % Number of points in the profile
    else
      lp_nt(index)=1; % Number of points in the profile
    end
    lp_T(index)=0;
    lp_vc(index)=vc;
    lp_bcs(index)=type;
    lp_code(index)=code;
    ra=ra+lp_nt(index);        % Advance the counter
    index=index+1;
  end
end

% first result memory location not used
ra_next=ra;
index=lp_ind+1:index-1;
lp_ind=index(length(index));

COR_status('COR_fdalt',vc,type,index)
