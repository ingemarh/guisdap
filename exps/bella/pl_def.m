nlag=75; nint=1; ngates=5; nfft=0; invert=1;
freq=-[3.6 5.2 6.8 8.4]*1e6; dt=0.6e-6;
ran=ones(5,1)*[44.5 385.2]+(0:4)'*ones(1,2)*138.2; fradar=930e6;
maxe=2; ele=0; updown=0; nup_d=1; skip_if=1;
re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_bella\d_\d+@Tp');
if [strfind(expt,'cut') re]
 startad=1;
else
 startad=40114+(0:3)*8087+1;
end
if isempty(gate), gate=2; end
