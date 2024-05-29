% COR_trilp: defines lag profiles for the pre-GEN-system long pulse algorithm.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The range ambiguity function for the longer lags
% is much narrower than for the shorter lags.
% input parameters
% ra      : First result memory address to use
% ri      : result memory address increment
% vc      : virtual channel number
% type    : background ('b'), calibration ('c') or signal ('s')
% gating : number of crossed products added together for the lag zero
% overlap: number of products skipped between gates (positive skipping)
%         : if negative, then same crossed products are used for successive gates
% N_gates : number of gates calculated
% lags    : Lags for which output needed in us, e.g. 0:10:250;
% code    : user specified code number
% Sample_skip : Number of points skipped in the sample vector

%
% See also: CORR_trilp, COR_check, COR_caltemp, COR_status, pulse_times, sample_times
%function COR_trilp(ra,ri,vc,type,gating,overlap,N_gates,lags,code,Sample_skip)
 function COR_trilp(ra,ri,vc,type,gating,overlap,N_gates,lags,code,Sample_skip)

global vc_adcint vc_sampling vc_samplingend vc_ba bm_samples bm_next
global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir  lp_dec lp_T lp_dt p_dtau
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind lp_indold ra_next ra_prev

if (type~='b' & type~='c' & type~='s' & type~='o')
  error(' Unknown data type in COR_trilp')
end
if nargin<10, Sample_skip=0; end

ra_prev=ra;

adcint=vc_adcint(vc); lags=gupround(lags/p_dtau);
COR_check(lags,adcint)

N_maxlag=gating-1;
if any(lags>N_maxlag*adcint);
  disp(lags(lags>N_maxlag*adcint));
  error('Lags longer than possible with this algorithm ')
end

N_lags=length(lags);
index=(lp_ind+1):(lp_ind+N_lags);

lp_T(index)=COR_caltemp(type)*ones(1,N_lags);

% find first when pulses are transmitted and samples are taken
pulsetimes=pulse_times(vc);
t1=sample_times(vc,type2ind(type));t1=t1(1)+Sample_skip*adcint;
lp_t1(index)=t1*ones(1,N_lags);
lp_h(index)=lp_t1(index)-pulsetimes(1);

% finally parameters common to background, calibration and signal
lp_t2(index)=lp_t1(index)+lags;
lp_dt(index)=adcint*ones(1,N_lags);
lp_nfir(index)=round(gating-lags/adcint);
for i=index
  lp_fir(1:lp_nfir(i),i)=ones(lp_nfir(i),1);
end
lp_dec(index)=(gating+overlap)*ones(1,N_lags);
lp_nt(index)=N_gates*ones(1,N_lags);
lp_vc(index)=vc*ones(1,N_lags);
lp_ra(index)=ra+(0:N_lags-1);
lp_ri(index)=ri*ones(1,N_lags);
lp_bcs(index)=type*ones(1,N_lags);
lp_code(index)=code*ones(1,N_lags);
lp_ind=lp_ind+N_lags;

% first result memory location not used
ra_next=max(lp_ra(index)+(lp_nt(index)-1).*lp_ri(index))+1;

COR_status('COR_trilp',vc,type,index)
