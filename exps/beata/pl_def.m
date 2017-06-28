%ran=[47 143;123 219;200 326];
ran=[49 233;180 323;227 413]; %180 should be 137 
ran=[49 233;137 323;227 413];
nfft=0; nint=1; ngates=3;
maxe=2; nup_d=1; skip_if=0;
vs=d_parbl(57); d_date=datenum(row(d_parbl(1:6)));
if d_parbl(41)==3, %VHF
 if vs<2
  ran=2+ran;
  freq=[4.5 -4.5]*1e6; dt=0.8e-6; invert=-1; fradar=224e6;
  ele=90; updown=0:1; nlag=25;
  startad=(0:1)*27907+32815+1;
 elseif vs<2.1
  ran=[47 221;128 302;209 383;290 464]; ngates=4;
  freq=[3.6 -3.6]*1e6; dt=0.4e-6; invert=-1; fradar=224e6;
  ele=90; updown=0:1; nlag=50;
  startad=(0:1)*82232+83840+1;
  startad=startad(2); updown=0; freq=-3.6e6;
 else
  ran=[47 221;128 302;209 383;290 464]; ngates=4;
  freq=[-3.6 -6]*1e6; dt=0.4e-6; invert=-1; fradar=224e6;
  nup_d=2; ele=90; updown=0; nlag=50;
  startad=(0:1)*82232+83840+1;
 end
elseif d_parbl(41)==8, %ESR-p
 %ran=[42 379;162 499];
 ran=[42 379;200 499]; %Aviod fvalley
 freq=[-4.0 4.0]*1e6; dt=0.4e-6; invert=-1; fradar=500e6;
 ele=81.6; updown=0:1; nlag=125; ngates=2;
 startad=(0:1)*39259+43509+1;
 if d_date>datenum(2011,11,11)
  invert=1;
 end
 if d_date>datenum(2012,05,02)
  freq=[-5.5 5.5]*1e6;
 end
else
 freq=-5.0e6; dt=0.4e-6; invert=1; fradar=930e6;
 ele=77.5; updown=0; nlag=50;
 startad=83111+1;
 if vs==2.0
  uparfreq=NaN; ngates=4; nup_d=3; freq=[-3.6 -6 -8.4]*1e6;
  ran=[47 221;128 302;209 383;290 464];
  startad=(0:2)*82232+108611+1;
 end
end
if length(d_data)==nup_d*length(updown)*ngates*nlag
 startad=(0:nup_d*length(updown)-1)*ngates*nlag+1;
end
if isempty(gate), gate=2; end
%freq=freq(1); startad=startad(1); updown=0;
