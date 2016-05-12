function [el,az,gain]=vhf_elaz(el_mec,ph,gain)
phtab=[0 1.22 2.44 3.67 4.89 6.12 7.35 8.58 9.82 11.06 ...
       12.31 13.56 14.82 16.09 17.37 18.65 19.95 21.25];
d=find(ph<=length(phtab) & ~rem(ph,1));
ph(d)=phtab(ph(d)+1);
c=180/pi;
a0=359.45;
elmr=(90-el_mec)/c; phr=-ph/c;
ar=atan2(tan(phr),sin(elmr));
elr=asin(cos(phr).*sin(elmr)./cos(ar));
d=find(phr~=0);
elr(d)=asin(sin(phr(d))./sin(ar(d)));
el=90-elr*c;
az=ar*c+a0;
if nargin>2
 gain=gain.*cos(phr);
%gain=gain.*cos(phr).*(1-.5*abs(tan(phr)));
end
