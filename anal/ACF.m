% plasma_acf=ACF(param,lags): Calculation of theoretical ACF without any corrections
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% this function returns the theoretical autocorrelation function without
% any pulse form or receiver effects. It is implemented by calling dirthe,
% and defining f_womega so that multiplying by it makes a Fourier transform
% Input parameters
% param: plasma parameters in scaled units
% lags: lag values (in p_dtau units)
% pldfvv: Plasma dispersion function in interpolation table (global)
% Output parameters:
% plasma_acf: plasma ACF for the specified parameters and lags
% See also: dirthe, real_to_scaled
%function plasma_acf=ACF(param,lags)
function plasma_acf=ACF(param,lags)

global p_om p_om0 p_dtau p_D0 k_radar pldfvv p_m0

lags=col(lags);
% Create complex exponential to make the Fourier transform
paino=exp( lags*p_om'*((p_dtau*1e-6)*p_om0(1)*sqrt(-1)) ) ;
len=length(p_om);
dom=[0; 0.5*(p_om(3:len)-p_om(1:len-2)); 0]';
[M,N]=size(paino);
% Take the frequency bin widths into account
f_womega=paino.*(dom(ones(M,1),:));

p_coeffg=ones(size(lags));
ch=1;  kd2=k_radar(ch)^2*p_D0^2;

plasma_acf=dirthe(param,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0);
plasma_acf=plasma_acf(1:length(lags));
