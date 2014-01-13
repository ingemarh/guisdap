% COR_pulsetopulse: algorithm for decoded alternating code experiments (lag profile format)
% GUISDAP v.1.65 01-10-27 Copyright EISCAT
%
% COR_arclp defines lag profiles firred alternating code experiments.
% The lag profile tails (where all the lags up to the longest are not obtained) are included
% The results are arranged in the lag profile format
%
% User specified input parameters:
% ra      : Result memory addresses for the first point
% vc      : virtual channel number
% type    : lag profile type, which MUST be signal ('s') 
% N_gates : number of gates to be spedified
% Nbits   : Number of bits in the each code
% Ntaps   : Number of taps in the decoding
% lags    : Lag  values in us
% code    : user specified code number
% Output is stored in the lp_xxx parameters (global)
%
% See also: CORR_alter, COR_check, COR_caltemp, COR_status, pulse_times, sample_times
%
%function COR_pulsetopulse(ra,vc,type,N_gates,Nbits,Ntaps,lags,code)
function COR_pulsetopulse(ra,vc,type,N_gates,Nbits,Ntaps,Norm,Npuls,Ipp,lags,code)

global vc_adcint vc_sampling vc_samplingend vc_ba bm_samples bm_next vc_mf
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev
global ipp

if (type~='s'),  error(' Unknown data type in COR_pulsetopulse'), end  

% Store the first result memory address (calling program might need it)
ra_prev=ra;

% Store the ADC interval into a local variable
adcint=vc_adcint(vc);

% Transform the user specified lag values to p_dtau units
% and check that they are exact multiples of it
lags=gupround(lags/p_dtau);
COR_check(lags,adcint)

bitsep=adcint;
ipp=gupround(Ipp/p_dtau);

% This is the heart of the routine. Here we define how each lag profile is filtered. 

% find first when pulses are transmitted
pulsetimes=pulse_times(vc);
% the signs of the alternating code bits are obtained here
am=env(vc,pulsetimes(1)+[0:((Npuls-1)*ipp/bitsep+(Nbits-1))]*bitsep+1)'; % 1 added to get +-1 as the result
am_split=reshape(am(1:(end-Nbits)),ipp/bitsep,Npuls-1);
lam=size(am_split,1);
am_split=[am_split(1:Nbits,:) am(end-Nbits+1:end)];
for i=1:Npuls
 am_split(:,i)=am_split(:,i)./Norm;
end
Ncohadd=Ntaps/Nbits; % (Extra) coherent integration!
zz=[ones(Ncohadd) zeros(Ncohadd)];
% Sampling start time
fs_time=sample_times(vc,type2ind(type));
sum(sum(am_split))-1

index=lp_ind+1; % This is the lag profile number to be used next
% Now we are ready to handle one lag profile at a time
for ind=1:length(lags)
  lag=round(lags(ind)/adcint/(ipp/bitsep));
  loop2=-(Ncohadd-1):(Ncohadd-1);
 for jj=loop2
  if jj<0
    i1=1-jj; i2=Npuls-lag;
  else
    i1=1; i2=Npuls-lag-jj;
  end
  lagg=lag+jj;
  ii=[];
  for i=find(diag(zz,abs(jj)))'
    ii=[ii i+(i1:Ncohadd:i2)-1];
  end
  % Find the filtering coefficient
  for i=ii
% end,for i=1:(Npuls-lag)
    d=find(vc_sampling(:,3)>ipp*(i-1)); fstime1=vc_sampling(d(1),3);
    d=find(vc_sampling(:,3)>ipp*(i-1+lagg)); fstime2=vc_sampling(d(1),3);
    signs=am_split(:,i)*am_split(:,i+lagg)';
    loop=(-(Nbits-1)):(Nbits-1);
%   if lags(ind)==0, loop=loop(find(loop~=0)); end
    for j=loop
      signs_sub=diag(signs,j);
      if sum(abs(signs_sub))>0
        if j<0
          lp_t1(index)=fstime1-j*adcint;% Sample time of the first factor
          lp_t2(index)=fstime2;		% Sample time of the second factor
        else
          lp_t1(index)=fstime1;		% Sample time of the first factor
          lp_t2(index)=fstime2+j*adcint;% Sample time of the second factor
        end
        Newtap=size(signs_sub,1);
        lp_nfir(index)=Newtap;
        lp_fir(1:Newtap,index)=signs_sub;
        lp_dec(index)=1;

        lp_h(index)=rem(lp_t1(index),ipp)-pulsetimes(1);% Estimate of the first range (not very good)
        lp_dt(index)=adcint;	% Range separation of samples 

        lp_ra(index)=ra;	% Start address of the lag profile
        lp_ri(index)=1;		% Successive locations always used
        lp_nt(index)=N_gates;	% Number of points in the profile

        lp_T(index)=0;
        lp_vc(index)=vc;
        lp_bcs(index)=type;
        lp_code(index)=code;
        index=index+1;
      end
    end
  end
 end
  ra=ra+N_gates;	% Advance the counter
end

% first result memory location not used
ra_next=ra;
index=lp_ind+1:index-1;
lp_ind=index(length(index));

COR_status('COR_pulsetopulse',vc,type,index)

