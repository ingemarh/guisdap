function [err,g]=gaincurve(x,t,d,xf)
% Gain curve for receiver recovery: g=a*exp(-t/b)+c
x=exp(x);
g=x(1)*exp(-t/x(2))+x(3);
err=norm([g-d;(x(3)-xf(3))']);
