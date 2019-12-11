if name_site=='V' && expver>1
 nlag=200; nint=1; ngates=5; nfft=0;
 dt=0.6e-6;
 maxe=2; ele=90; updown=0; skip_if=1;
 ran=ones(5,1)*[44.5 470.7]+(0:4)'*ones(1,2)*138.2;
 if d_parbl(41)==3
  invert=-1; freq=-[5.2 3.6]*1e6; nup_d=2; fradar=224e6;
  re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_tau7\d_\d+@Vp');
  if [strfind(expt,'cut') re]
   startad=(0:1)*5*200+1;
  else
   startad=24668+(0:1)*25668+1;
  end
 end
end
if isempty(gate), gate=2; end
