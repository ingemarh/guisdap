% REC_sampling: Sets sampling start time for a virtual channel
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% input parameters
% samplingtime : the active virtual channel starts sampling then
% adcint       : ADC interval for the active virtual channel
%                effective if not specified before
%
% See also: design, REC_impresp, COR_status
%
% function REC_sampling(samplingtime,adcint)
  function REC_sampling(samplingtime,adcint) 

global vc_number vc_adcint p_dtau  type_next vc_sampling

vc=vc_number;
if length(vc_adcint)<vc, vc_adcint(vc)=0; end
if nargin==1, adcint=0; end

adcint=adcint/p_dtau;
% set adcint if not set for the virtual channel before
if vc_adcint(vc)==0 & adcint>0;
  vc_adcint(vc)=adcint;
elseif vc_adcint(vc)==0 & adcint==0;
  fprintf(' Adcinterval not yet set for virtual channel %.0f\n', vc)
  fprintf(' You must set it in the first REC_samping call after Xmission\n')
  error('')
elseif vc_adcint(vc)~=adcint & adcint>0,
  fprintf(' Conflicting adcintervals given for virtual channel %.0f\n', vc)
  fprintf(' Previous value of %.1f retained\n', vc_adcint(vc)*p_dtau)
end
   
lptype=type_next;
samplingtime=samplingtime/p_dtau;
vc_sampling=[vc_sampling; vc, type2ind(lptype),samplingtime 0 0 0];
