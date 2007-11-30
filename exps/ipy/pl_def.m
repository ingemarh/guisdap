nfft=0; nint=1; ngates=3; nlag=50; fradar=500e6;
freq=[-4.0 4.0]*1e6; dt=0.6e-6; invert=-1;
ran=41+(0:2)'*65*ones(1,2)+ones(3,1)*[0 140];
maxe=2; ele=81.6; updown=0:1; nup_d=1; skip_if=0;
re=regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_ipy\d_\d+@32p');
if [strfind(expt,'cut') re]
  startad=(0:1)*3*nlag+1;
  if re
    dat=sscanf(expt,'%d-%d-%d_ipy%d_%d@32p');
    if datenum(dat(1:3)')>datenum(2007,11,11)
      ran=[41 252;117 329;194 405];
    end
  end
elseif strfind(expt,'ipy2')
  startad=(0:1)*29814+34*nlag+33*768+21;
else
  startad=(0:1)*19898+22*nlag+21*768+21;
end
if isempty(gate), gate=3; end
%freq=freq(2); updown=0; startad=startad(2); %uncomment/modify for one plch
