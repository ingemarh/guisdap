% COR_check.m: checks that the lag values are exact multiples of p_dtau and ADC interval
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Execution is stopped if a mismatch is found
% Input parameters:
% p_dtau: basic time unit (global)
% lags:   Lag values in p_dtau units
% adcint: sampling interval
% bitsep: bit separation in alternating code experiments (optional)
% function COR_check(lags,adcint,bitsep);
function COR_check(lags,adcint,bitsep)

global p_dtau

err=0;
% Check if lag values are exact multiples of p_dtau
ind=find(abs(lags-round(lags))>1000*eps);
if length(ind)>0,
   fprintf('Error: All lag values are not exact multiples of p_dtau of %5.2f us\n',p_dtau)
   for lag=lags;
   fprintf('Lag %4.0f divided by p_dtau is %5.1f\n',lag*p_dtau,lag);end
   err=1;
end

% Check if lag values are exact multiples of adcint
ind=find(abs(lags-round(lags/adcint)*adcint)>1000*eps); 
if length(ind)>0,
   fprintf('Error: All lag values are not exact multiples of adcint of %5.1f us\n',adcint)
   for lag=lags;fprintf('Lag %4.0f divided by adcint is %5.1f\n',lag,lag/adcint);end
   err=1;
end

if nargin==3,
  % Check if lag values are exact multiples of bitsep
  ind=find(abs(lags-round(lags/bitsep)*bitsep)>1000*eps); 
  if length(ind)>0,
    fprintf('Error: All lag values are not exact multiples of bitsep of %5.2f us\n',bitsep)
    for lag=lags;fprintf('Lag %4.0f divided by bitsep is %5.1f\n',lag,lag/bitsep);end
    err=1;
  end
end

if err,
  fprintf(' Check parameters, stopping\n')
  error(' Error found by COR_check'),
end
