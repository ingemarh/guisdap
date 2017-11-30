nfft=0; nint=1; ngates=5; fradar=500e6;
freq=[-4.25 -6.75 4.25 6.75]*1e6; dt=0.4e-6; invert=1;
maxe=2; updown=0:1; nup_d=2; skip_if=1;
vs=d_parbl(57); nfreq=length(freq);
nlag42=125; nlag32=250;
addjump=20+ngates*nlag32+20+ngates*nlag42;
ant=minput('Which antenna','42m',1);
if strcmp(ant,'42m')
 nlag=nlag42; ele=81.6;
 ran=ones(ngates,1)*[41 222]+61*(0:ngates-1)'*ones(1,2);
 startad=(0:nfreq-1)*addjump+20+nlag32*ngates+20;
else
 nlag=nlag32; ele=30;
 ran=ones(ngates,1)*[41 404]+123*(0:ngates-1)'*ones(1,2);
 startad=(0:nfreq-1)*addjump+20;
end
if vs>=3.0
  uparfreq=NaN;
end
if isempty(gate), gate=3; end
band=minput('Which side','both',1);
if strcmp(band,'down')
 freq=freq(1:2); updown=0; startad=startad(1:2);
elseif strcmp(band,'up')
 freq=freq(3:4); updown=0; startad=startad(3:4);
end
