vs=d_parbl(57); %Experiment version

if d_parbl(41)==3 && vs>1 %VHF
 nlag=200; nint=1; ngates=5; nfft=0;
 dt=0.6e-6;
 maxe=2; ele=90; updown=0; skip_if=1;
 ran=ones(5,1)*[44.5 470.7]+(0:4)'*ones(1,2)*138.2;
 invert=-1; freq=-[5.2 3.6]*1e6; nup_d=2; fradar=223.6e6;
 re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_tau7\d_\d+@Vp');
 if [strfind(expt,'cut') re]
  startad=(0:1)*5*200+1;
 else
  startad=24688+(0:1)*25688+1;
 end
 if isempty(gate), gate=2; end
elseif d_parbl(41)==8 %ESR plasma
 nfft=256; nint=1; ngates=1; nlag=0;
 freq=[-5.7 5.7]*1e6; dt=0.4e-6; invert=-1;
 ran=[98.5 401.6]; fradar=499.7e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=1;
 startad=20+(0:1)*276+1; gate=1; skip_if=0;
 band=minput('Which side','both',1);
 if strcmp(band,'down')
  freq=freq(1); updown=0; startad=startad(1);
 elseif strcmp(band,'up')
  freq=freq(2); updown=0; startad=startad(2);
 end
end
if isempty(gate), gate=1; end