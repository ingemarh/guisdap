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
% bitsep  : Bit separation in us
% Sample_skip : Number of points skipped in the sample vector
% lags    : Lag  values in us
% code    : user specified code number
% Output is stored in the lp_xxx parameters (global)
%
% See also: COR_arclp, COR_fraclp, COR_check, COR_status, pulse_times, sample_times
%
%function COR_fdalt(ra,vc,type,N_gates,NlowG,NupG,Nbits,bitsep,Sample_skip,lags,code)
function COR_fdalt(ra,vc,type,N_gates,NlowG,NupG,Nbits,bitsep,Sample_skip,lags,code)

global vc_adcint vc_sampling vc_samplingend vc_ba bm_samples bm_next vc_mf
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev

if (type~='s'),  error(' Unknown data type in COR_fdalt'), end  

% Store the first result memory address (calling program might need it)
ra_prev=ra;
ra0=ra;

% Store the ADC interval into a local variable
adcint=vc_adcint(vc);
bitsep=gupround(bitsep/p_dtau);
frac=bitsep/adcint;

% Transform the user specified lag values to p_dtau units
% and check that they are exact multiples of it
lags=gupround(lags/p_dtau);
COR_check(lags,adcint)

% This is the heart of the routine. Here we define how each lag profile is filtered. 

% find first when pulses are transmitted
pulsetimes=pulse_times(vc);
% the signs of the alternating code bits are obtained here
am=env(vc,floor(pulsetimes(1)+bitsep/2)+[0:Nbits-1]*bitsep); % 1 added to get +-1 as the result
am=repmat(am,frac,1); am=am(:);
% Sampling start time
fs_time=sample_times(vc,type2ind(type))+Sample_skip*adcint;

%Construct decoding matrices
d=zeros(frac);
hdm=am*am';
ldm=zeros((Nbits-1)*frac);
for i=0:Nbits-1
  ii=i*frac+(1:frac);
  hdm(ii,ii)=d;
  if i<Nbits-1, ldm(ii,ii)=hdm(ii+frac,ii); end
end

if lags(1)==0
  ii=[1:frac 2:length(lags)];
  lres=1;
else
  ii=1:length(lags);
  lres=0;
end
index=lp_ind+1; % This is the lag profile number to be used next
% Now we are ready to handle one lag profile at a time
for ind=ii
  lag=round(lags(ind)/adcint);
  if lres==1
    NG=N_gates+1;
    sgns=diag(ldm,lag);
    if lag==frac-1, lres=0; end
  else
    NG=N_gates;
    sgns=diag(hdm,lag);
  end
  ra=ra0;                   % Set the counter
  ra0=ra0+NlowG+NG+NupG;

  for i=[-fliplr(1:NlowG)*frac 0 (1:NupG)*frac]
    %Find the filtering coefficients
    nt=1;
    if i<=0
      signs=sgns(1-i:end);
      t1=fs_time(1);
      if i==0, nt=NG; end
    else
      signs=sgns(1:end-i);
      %t1=fs_time(1)+(NG+i-frac)*bitsep;
      t1=fs_time(1)+NG*bitsep+(i-frac)*adcint;
    end
    nfir=length(signs);
    if nfir>0
      lp_t1(index)=t1;      % Sample time of the first factor
      lp_t2(index)=lp_t1(index)+lags(ind); % Sample time of the second factor
      lp_nfir(index)=nfir;
      lp_fir(1:nfir,index)=signs;
      lp_dec(index)=frac;   % Separation between gates
      lp_h(index)=lp_t1(index)-pulsetimes(1); % Estimate of the first range (not very good)
      lp_dt(index)=adcint;  % Range separation of samples 

      lp_ra(index)=ra;      % Start address of the lag profile
      lp_ri(index)=1;       % Successive locations always used
      lp_nt(index)=nt;      % Number of points in the profile
      lp_T(index)=0;
      lp_vc(index)=vc;
      lp_bcs(index)=type;
      lp_code(index)=code;
      index=index+1;
    end
    ra=ra+nt;               % Advance the counter
  end
end

% first result memory location not used
ra_next=ra;
index=lp_ind+1:index-1;
lp_ind=index(length(index));

COR_status('COR_fdalt',vc,type,index)
