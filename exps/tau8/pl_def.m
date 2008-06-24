nfft=128; nint=2; ngates=4; nlag=0;
freq=4.1*1e6; dt=0.6e-6; invert=-1;
ran=[53.3 272.4;193.4 412.5;333.4 552.5;473.3 692.4]; fradar=224e6;
maxe=2; ele=0; updown=0; nup_d=1; skip_if=1;
re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_tau8\d_\d+@Vp');
if [strfind(expt,'cut') re]
 startad=1;
else
 startad=108679;
end
if isempty(gate), gate=2; end
