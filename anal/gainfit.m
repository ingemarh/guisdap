function g=gaincurve(data,t0)
% Gain curve for receiver recovery: g=a*exp(-t/b)+c
ld=length(data); d=1;
t=(1:ld)';
dd=data; tt=t+t0; md=median(dd);
x0=[md/10 t0 md]; x=log(x0);
opts=optimset(optimset('fminsearch'),'maxfun',1e4,'maxiter',1e4);
while length(tt)~=length(d);
 x=fminsearch('gaincurve',x,opts,tt,dd,x0);
 [err,g]=gaincurve(x,tt,dd,x0);
%[P,S,mu]=polyfit([tt-ld;tt],[dd;flipud(dd)],8); g=polyval(P,tt-ld,[],mu);
 d=find(abs(g-dd)<3*std(g-dd));
 tt=tt(d); dd=dd(d);
end
[err,g]=gaincurve(x,t+t0,data,x0);
%g=polyval(P,t+t0-ld,[],mu);
%figure(9),plot(t,data,t,g),drawnow
