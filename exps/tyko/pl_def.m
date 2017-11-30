nfft=0; nint=1; ngates=5; fradar=500e6;
freq=[-4.2 -6.6 4.2 6.6]*1e6; dt=0.4e-6; invert=1;
maxe=2; updown=0:1; nup_d=2; skip_if=1;
vs=d_parbl(57); nfreq=length(freq);
nlag=125;
addjump=20+ngates*nlag;
ele=81.6;
ran=ones(ngates,1)*[28 206]+61*(0:ngates-1)'*ones(1,2);
startad=(0:nfreq-1)*addjump+20;
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
