% REC_impresp.m: % defines various kinds of impulse responses
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The possible filter types are:
% type     data
% ====     ====
% 'box'    gives the length of box in microseconds
% 'taps'   gives the filter coefficients
% 'but'    gives the bandwidth of Butterworth filter
% 'lin'    gives the bandwidth of Linear Phase filter
% Output parameter: vc_p (global)
%
% See also: design, lin_filt, but_filt
%
% function REC_impresp(data,type)
  function REC_impresp(data,type) 

global vc_p vc_number p_dtau

if nargin==1, type='box'; end

if isstr(type)==0,
  len=data/p_dtau;  
  impresp=(1/len)*kron(type,ones(len,1));
elseif strcmp(type,'taps')
  impresp=data(:);
elseif strcmp(type,'but'),  % Butterworth filter
  impresp=but_filt(data);  
elseif strcmp(type,'lin'),  % Linear phase filter
  impresp=lin_filt(data);
else                           % Assume Box-car filter
  impresp=ones(data/p_dtau,1);
end

vc_p(1:length(impresp)+1,vc_number)=[impresp/sum(impresp);0]; % Scale to unit area
