ran=[40 273;130 363;220 453];
nfft=0; nint=1; ngates=3; nlag=90;
maxe=2; nup_d=1; skip_if=0;
freq=[3.7]*1e6; dt=1/1.5e6; invert=1; fradar=500e6;
ele=81.6; updown=0;
re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_folke\d_\d+@[L]p');
if [strfind(expt,'cut') re]
 startad=0+1;
else
 startad=58990+1;
end
if isempty(gate), gate=3; end
