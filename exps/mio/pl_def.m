nfft=0; nint=1; fradar=500e6; nlag=75; ngates=4;
maxe=2; ele=81.6; updown=0:1; nup_d=1; skip_if=0; invert=1;
dt=(1/3)*1e-6; uparfreq=NaN;
vs=d_parbl(57);
d_date=datenum(row(d_parbl(1:6)));
ran=round(ones(ngates,1)*([24 395])+(0:ngates-1)'*ones(1,2)*142.5);
freq=[-3.55 -6.45 3.55 6.45]; nup_d=2;
if length(d_data)==2*ngates*nlag
  startad=(0:3)*ngates*nlag+1;
else
  startad=(0:3)*425895+175*nlag+174*2304+25;
end
if isempty(gate), gate=2; end
%freq=freq(2); updown=0; startad=startad(2); %uncomment/modify for one plch
