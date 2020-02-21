nfft=0; nint=1; ngates=3; nlag=50; fradar=500e6;
freq=[-4.0 4.0]*1e6; dt=0.6e-6; invert=-1;
ran=41+(0:2)'*65*ones(1,2)+ones(3,1)*[0 140];
maxe=2; ele=81.6; updown=0:1; nup_d=1; skip_if=0;
uparfreq=[3.65 11.65];
vs=d_parbl(57);
d_date=datenum(row(d_parbl(1:6)));
if d_date>datenum(2007,11,11)
  ran=[41 252;117 329;194 405];
end
if d_date>datenum(2011,11,11)
 invert=1;
end
if vs>=3.0
  nlag=75; ngates=4;
  dt=0.4e-6; uparfreq=NaN;
  ran=[41 248;113 320;185 392;257 464];
  if vs>=4.1
   freq=[-4.2 -6.6 4.2 6.6]*1e6; nup_d=2;
   ran=[26 240;105 318;183 397;262 476];
   if d_date>datenum(2017,1,1) & d_date<datenum(2020,2,15) % from when?
    freq=fliplr(freq); clear uparfreq
   end
  elseif vs>=4.0
   ran=[26 251;116 341;206 431;296 521];
  end
end
if length(d_data)==2*ngates*nlag
  startad=(0:1)*ngates*nlag+1;
elseif length(d_data)==4*ngates*nlag
  startad=(0:3)*ngates*nlag+1;
elseif vs>=4.1
  startad=(0:3)*86627+67*nlag+66*1152+21;
elseif vs>=4.0
  startad=(0:1)*87377+67*nlag+66*1152+21;
elseif vs>=3.0
  startad=(0:1)*66545+51*nlag+50*1152+21;
elseif vs>=2.0
  startad=(0:1)*29814+34*nlag+33*768+21;
else
  startad=(0:1)*19898+22*nlag+21*768+21;
end
if isempty(gate), gate=3; end
%freq=freq(2); updown=0; startad=startad(2); %uncomment/modify for one plch
