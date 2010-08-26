nfft=0; nint=1; ngates=3; nlag=50; fradar=500e6;
freq=[-4.0 4.0]*1e6; dt=0.6e-6; invert=-1;
ran=41+(0:2)'*65*ones(1,2)+ones(3,1)*[0 140];
maxe=2; ele=81.6; updown=0:1; nup_d=1; skip_if=0;
uparfreq=[3.65 11.65];
re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_ipy\d_\d+@Lp');
vs=2;
if [strfind(expt,'cut') re]
  if re
    dat=sscanf(expt,'%d-%d-%d_ipy%d_%d@Lp');
    if datenum(dat(1:3)')>datenum(2007,11,11)
      ran=[41 252;117 329;194 405];
    end
    vs=dat(4);
    if vs>2
      nlag=75; ngates=4;
    end
  end
  startad=(0:1)*ngates*nlag+1;
elseif strfind(expt,'ipy3')
  vs=3;
  startad=(0:1)*66545+51*nlag+50*1152+21;
elseif strfind(expt,'ipy2')
  startad=(0:1)*29814+34*nlag+33*768+21;
else
  vs=1;
  startad=(0:1)*19898+22*nlag+21*768+21;
end
if vs==3
  nlag=75; ngates=4; dt=0.4e-6; uparfreq=NaN;
  ran=[41 248;113 320;185 392;257 464];
end
if isempty(gate), gate=3; end
%freq=freq(2); updown=0; startad=startad(2); %uncomment/modify for one plch
