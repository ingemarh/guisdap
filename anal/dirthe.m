% dirthe.m: DIRect THEory for Inocherent Scatter radar measurements
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% 
% Available also as a fast mex-version
% Input parameters:
% param: plasma parameters in scaled units
% p_coeffg: radar factor for each measurement 
% f_womega: spectral ambiguity function for each measurement
% kd2: (k D)^2, k is radar k-vector and D is Debye length for scale parameters
% p_om: frequency axis for f_womega
% pldfvv: plasma dispersion function interpolation table
% Output parameter:
% theo: theoretical values for the measurements
% See also: spec, transf
function theo=dirthe(param,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0)

t_start(4)

param=real(param); % hyi hyi

%nonphys=physlim(param,p_m0);

[nin0,tit0,mim0,psi,vi]=transf(param,p_m0);
s=spec(nin0,tit0,mim0,psi,vi,kd2,p_om,pldfvv);

theo=[p_coeffg.*(f_womega*s);col(param)];
t_stop(4)
