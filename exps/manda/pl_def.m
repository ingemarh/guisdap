nfft=0; nint=1; ngates=2; nlag=10; fradar=500e6;
freq=[-6.4 -4.0 4.0 6.4]*1e6; dt=0.4e-6; invert=1;
ran=round(ones(ngates,1)*(70.05+[-1 1]*65.3/2)+(0:ngates-1)'*ones(1,2)*57);
%ran=ran+187.5;
maxe=1; ele=82.1; updown=0:1; nup_d=2; skip_if=1;
uparfreq=NaN;
vs=d_parbl(57);
d_date=datenum(row(d_parbl(1:6)));
if length(d_data)==length(freq)*ngates*nlag
  startad=(0:3)*ngates*nlag+1;
else
  startad=(0:3)*117460+52500+1;
end
if isempty(gate), gate=2; end
