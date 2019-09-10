nlag=75; nint=1; ngates=5; nfft=0;
dt=0.6e-6;
maxe=2; ele=0; updown=0; nup_d=1; skip_if=1;
ran=ones(5,1)*[44.5 385.2]+(0:4)'*ones(1,2)*138.2;
if d_parbl(41)==3
 invert=-1; freq=-[5.2 3.6]*1e6; fradar=224e6;
 re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_bella\d_\d+@Vp');
 if [strfind(expt,'cut') re]
  startad=1;
 else
  startad=14098+(0:1)*14473+1;
 end
else
 invert=1; freq=-[3.6 5.2 6.8 8.4]*1e6; fradar=930e6;
 re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_bella\d_\d+@Tp');
 if [strfind(expt,'cut') re]
  startad=1;
 else
  startad=40114+(0:3)*8087+1;
 end
end
if isempty(gate), gate=2; end
