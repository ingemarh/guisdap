function comp=comp_model(h,Ne210,hd)
% function comp=comp_model(h,Ne210,hd)
% or Ne210=[tsec lat long];
if nargin<3, hd=17; end
if nargin<2, Ne210=[]; end
if isempty(Ne210), Ne210=1e11; end
if length(Ne210)==1
 h50=346-29*log10(Ne210/1e6);
else
 tsec=Ne210(1); lat=Ne210(2); lon=Ne210(3);
 if tsec<=0, tsec=90.5*86400; end
 if lon==0, lon=19.2; end
 if lat==0, lat=69.2; end
 tloc=rem(tsec,86400)/3600+lon/15;
 day=tsec/86400;
 ang=-(90.-lat)*cos(tloc/12*pi)-23.5*(cos((day+10)/183*pi));
 h50=210-13*erf((ang+7)/4);
end
h=(h-h50)/hd;
comp=.5+sign(h).*(1.-exp(-abs(h)))/2.;
