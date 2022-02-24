% get_impresp_dec.m: Calculates filter responces from fir sequence at specific rate
% GUISDAP v.9.2 22-02-27 Copyright EISCAT
% See also: get_impresp
% function [impresp]=get_impresp(ddf,p_dtau,adc_rate)
function [impresp]=get_impresp(ddf,p_dtau,adc_rate)
if nargin<3
 adc_rate=1; %syisr
end
if ischar(ddf)
 ddf=load(ddf);
end
t_ddf=(0:length(ddf)-1)/adc_rate;
tcenter=t_ddf(end)/2;
a=p_dtau/2;
t2=tcenter-a;
t3=tcenter+a;
n=floor(t2/p_dtau);
t1=t2-n*p_dtau;
t4=t3+n*p_dtau;
t_ip=(0:round((t4-t1)/p_dtau))*p_dtau+t1;
impresp=interp1(t_ddf,ddf,t_ip,'pchip');
impresp=impresp/sum(impresp);
