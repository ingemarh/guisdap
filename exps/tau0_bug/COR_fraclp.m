% COR_fraclp: algorithm for decoded alternating code experiments (lag profile format)
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% COR_fraclp defines lag profiles for fractionally sampled alternating code experiments.
% The lag profile tails (where all the lags up to the longest are not obtained) are included
% The results are arranged in the lag profile format
%
% User specified input parameters:
% ra      : Result memory addresses for the first point
% vc      : virtual channel number
% type    : lag profile type, which MUST be signal ('s') 
% N_gates : number of gates to be spedified
% Nbits   : Number of bits in the alternating code
% bitsep  : distance of leading edges of code bauds
%           The value equals the bit length often, but not e.g. for interlaced alternating codes 
% lags    : Lag  values in us
% code    : user specified code number
% Output is stored in the lp_xxx parameters (global)
%
% See also: CORR_alter, COR_check, COR_caltemp, COR_status, pulse_times, sample_times
%
%function COR_fraclp(ra,vc,type,N_gates,Nbits,bitsep,lags,code)
 function COR_fraclp(ra,vc,type,N_gates,Nbits,bitsep,lags,code)

global vc_adcint vc_sampling vc_samplingend vc_ba bm_samples bm_next vc_mf
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev

if (type~='s'),  error(' Unknown data type in COR_fraclp'), end  

% Store the first result memory address (calling program might need it)
ra_prev=ra;

% Store the ADC interval into a local variable
adcint=vc_adcint(vc);

% Transform the user specified lag values to p_dtau units
% and check that they are exact multiples of it
lags=lags/p_dtau;
COR_check(lags,adcint)

% Transform the bit separation to p_dtau units as well
bitsep=bitsep/p_dtau;
COR_check(bitsep,adcint)

% This is the heart of the routine. Here we define how each lag profile is filtered. 
% DEFINITION: Full lag:       lag value which is an integer multiple of the baud length
%             Fractional lag: any other lag value

lag_values=[]; % This is the lag value of the lag profile
full_lags=[];  % This is the full lag value where the filter coefficients are obtained
index=1;
for ind=1:length(lags);
  if lags(ind)==0, % Zero lag will be treated as fractional lag here!
    % The zero lag profile is filtered by the lag one coefficients.
    % This produces a power profile estimate with a good range resolution
    lag_values(index)=lags(ind);
    full_lags(index)=bitsep;
    index=index+1;
  elseif rem(lags(ind),bitsep)==0, % Full lag (processed as usual)
    lag_values(index)=lags(ind);
    full_lags(index)=lags(ind);
    index=index+1;
  else % Fractional lag
    if lags(ind)<bitsep % lag shorter than lag one
      % Only one lag profile defined, filtered by the lag one coefficients
      lag_values(index)=lags(ind);
      full_lags(index)=bitsep;
      index=index+1;
    elseif lags(ind)>(Nbits-1)*bitsep % longer than longest full lag
      % Only one lag profile defined, filtered by the cofficients of the last full lag
      lag_values(index)=lags(ind);
      full_lags(index)=(Nbits-1)*bitsep;
      index=index+1;
    else % normal fractional lags
      % Two lag profiles defined
      % The coefficients are taken from both adjacent lag profiles
      lag_values(index+[0,1])=[lags(ind),lags(ind)];
      full_lags(index+[0,1])=floor(lags(ind)/bitsep)*bitsep+[0,bitsep];
      index=index+2;
    end
  end
end

% find first when pulses are transmitted
pulsetimes=pulse_times(vc);
% the signs of the alternating code bits are obtained here
am=env(vc,pulsetimes(1)+[0:(Nbits-1)]*bitsep+1)'; % 1 added to get +-1 as the result

% Sampling start time
fs_time=sample_times(vc,type2ind(type));

index=lp_ind+1; % This is the lag profile number to be used next
% Now we are ready to handle one lag profile at a time
for ind=1:length(lag_values)

  % Find the filtering coefficient
  SB=bitsep/adcint; % Samples per bit in the alternating code
  Ntaps=Nbits-round(full_lags(ind)/bitsep); % Number of taps in the filter
  signs=am(1:Ntaps).*am(Nbits-Ntaps+1:Nbits);
  if SB>1 & Ntaps>1, % If oversampled, add a correct number of zeros between each 
    signs=[kron(signs(1:Ntaps-1),[1;zeros(SB-1,1)]);signs(Ntaps)];
    Ntaps=(Ntaps-1)*SB+1; % Number of taps in the filter
  end   

  for filterlen=1:SB:Ntaps
    % Go through all filter lengths so that the lag profile tails are also formed
    if filterlen<Ntaps % These are the lag profile tails
      Ngates=SB;
    elseif filterlen==Ntaps % This is for the totally decoded region
      Ngates=N_gates+(full_lags(ind)-lag_values(ind))/adcint;
    end

   for bug1=[0 1], for bug2=[0 1]
    lp_nfir(index)=filterlen;
    lp_fir(1:filterlen,index)=signs(Ntaps-filterlen+1:Ntaps)/4;
    lp_dec(index)=1;

    lp_t1(index)=fs_time(1)+bug1*adcint;
    lp_h(index)=fs_time(1)-pulsetimes(1);    % Estimate of the first range (not very good)
    lp_t2(index)=fs_time(1)+lag_values(ind)+bug2*adcint; % Sample time of the second factor
    lp_dt(index)=adcint;             % Range separation of samples 

    lp_ra(index)=ra;     % Start address of the lag profile
    lp_ri(index)=1;      % Successive locations always used
    lp_nt(index)=Ngates; % Number of points in the profile


    lp_T(index)=0;
    lp_vc(index)=vc;
    lp_bcs(index)=type;
    lp_code(index)=code;
    index=index+1;
   end, end
    ra=ra+Ngates;        % Advance the counter
  end
end

% first result memory location not used
ra_next=ra;
index=lp_ind+1:index-1;
lp_ind=index(length(index));

COR_status('COR_fraclp',vc,type,index)

