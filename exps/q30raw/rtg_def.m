loopc=512;
%pp/timing
 psig=[262+261*16 0];
 psamp=[262 262];
 plen=[320 20]*1e-6;
 pdt=[1 1]*20.0e-6;
 prange0=800.0e-6+[0 -14*20e-6];
 pbac=[1 0];
%background
 back=262+261*16+200;
 bsamp=62;
 cal=0;
 csamp=0;
%sigspec
 sig=262;
 sig0=0;
 sigtyp={'fdalt'};
 sgates=261;
 siglen=320e-6;
 nbits=16;
 maxlag=16;
 sigdt=20.0e-6;
 srange0=(800-siglen+sigdt)*1e-6;
