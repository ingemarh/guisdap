if strfind(expt,'steffe2')
 nfft=0; nint=1; ngates=3; nlag=175;
 freq=[-3.8 -5.4 3.8 5.4]*1e6; dt=0.6e-6; invert=-1;
 ran=[47 314;189 455;330 597]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=2; skip_if=0;
 if [strfind(expt,'cut') regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_steffe2_\d+@Lp')]
  startad=(0:3)*3*nlag+1;
 else
  startad=(0:3)*19619+10*nlag+9*1536+1;
 end
 if isempty(gate), gate=2; end
 %freq=freq([1 3]); startad=startad([1 3]); nup_d=1;
elseif strfind(expt,'plwin')
 nfft=0; nint=1; ngates=3; nlag=240;
 freq=[-4.8 4.8]*1e6; dt=0.4e-6; invert=-1;
 ran=[69 300;170 400;270 501]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=1;
 startad=[1 48165]+19*240+18*2048; skip_if=1;
 if isempty(gate), gate=2; end
 %freq=freq(1); updown=0; startad=startad(1);
elseif strfind(expt,'steffe')
 nfft=128; nint=2; ngates=1; nlag=0;
 freq=[-4 -5.3 4 5.3]*1e6; dt=0.6e-6; invert=-1;
 ran=[182 423.9]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=2;
 startad=1; gate=1; skip_if=0;
end
