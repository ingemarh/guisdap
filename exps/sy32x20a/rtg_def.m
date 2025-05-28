%expinfo is 'syisr3 sy32x20'
loopc=fix(d_parbl(7)/14e-3);
if site==10
%pp/timing
 psig=0;
 psamp=1000;
 plen=640e-6;
 pdt=10.0e-6;
 prange0=d_parbl(60)*1e-6;
%noise
 back=1000;
 cal=NaN;
 bsamp=268;
 csamp=0;
%sigspec
 sig=1268;
 sigtyp={'alt'};
 sigsamp=psamp;
 siglen=plen;
 nbits=32;
 maxlag=63;
 sigdt=pdt;
 srange0=prange0;
end
