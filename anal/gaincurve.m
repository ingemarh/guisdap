function [err,g]=gaincurve(x,t,d)
% Gain curve for receiver recovery: g=a*exp(-t/b)+c
x=[atan(x(1))/15.7 exp(x(2:3))];
g=x(3)*(x(1)*exp(-t/x(2))+1);
if nargin>2
 err=norm([g-d;[exp(2*t(1)-x(2));exp(x(2)-t(end)/2)]]);
else
 err=0;
end
