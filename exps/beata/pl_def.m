%ran=[47 143;123 219;200 326];
ran=[49 233;137 323;227 413];
nfft=0; nint=1; ngates=3;
maxe=2; nup_d=1; skip_if=0;
re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_beata\d_\d+@[TVL]p');
if [strfind(expt,'v') strfind(expt,'V')]
 ran=2+ran;
 freq=[4.5 -4.5]*1e6; dt=0.8e-6; invert=-1; fradar=224e6;
 ele=90; updown=0:1; nlag=25;
 if [strfind(expt,'cut') re]
  startad=(0:1)*ngates*nlag+1;
 else
  startad=(0:1)*27907+32815+1;
 end
 %freq=freq(1); startad=startad(1); updown=0;
elseif [strfind(expt,'l') strfind(expt,'L')]
 ran=[42 266;155 379];
 freq=[-4.0 4.0]*1e6; dt=0.4e-6; invert=-1; fradar=500e6;
 ele=81.6; updown=0; nlag=125; ngates=2;
 if [strfind(expt,'cut') re]
  startad=(0:1)*ngates*nlag+1;
 else
  startad=(0:1)*39259+43509+1;
 end
 %freq=freq(1); startad=startad(1); updown=0;
else
 freq=[-5.0]*1e6; dt=0.4e-6; invert=1; fradar=930e6;
 ele=77.5; updown=0; nlag=50;
 if [strfind(expt,'cut') re]
  startad=0+1;
 else
  startad=83111+1;
 end
end
if isempty(gate), gate=2; end
