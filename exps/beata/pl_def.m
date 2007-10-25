ran=[47 143;123 219;200 326];
nfft=0; nint=1; ngates=3; nlag=25;
maxe=2; nup_d=1; skip_if=0;
if strcmp(expt,'v')
 freq=[5.5 -5.5]*1e6; dt=0.8e-6; invert=-1; fradar=224e6;
 ele=90; updown=0:1;
 if strfind(expt,'cut')
  startad=(0:1)*3*nlag+1;
 else
  startad=(0:1)*27907+32815+1;
 end
 %freq=freq(1); startad=startad(1); updown=0;
else
 freq=[-4.8]*1e6; dt=0.4e-6; invert=1; fradar=930e6;
 ele=77.5; updown=0;
 if strfind(expt,'cut')
  startad=0+1;
 else
  startad=83111+1;
 end
end
if isempty(gate), gate=3; end
