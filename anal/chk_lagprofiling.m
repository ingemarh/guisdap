function chk_lagprofiling()
global a_lpf
i=0;
if ~isfield(a_lpf,'nrep'), a_lpf(1).nrep=[]; end
if ~isfield(a_lpf,'p'), a_lpf(1).p=[]; end
for lpf=a_lpf
 i=i+1;
 switch lpf.lib
 case 'plwin'
  lpf.nrep=lpf.par(6);
  nsamp=lpf.par(7)+lpf.par(11);
  if isempty(lpf.p)
   lpf.p=[0 lpf.nrep-1];
  end
  nwin=lpf.par(5);
  np=diff(lpf.p)+1;
  if rem(lpf.p(1),nwin)==0 && rem(lpf.p(2)+1,nwin)==0
   lpf.par(6)=np;
   win1=0;
  elseif fix(lpf.p(1)/nwin)==fix(lpf.p(2)/nwin)
   win1=rem(lpf.p(1),nwin);
   nwin=diff(lpf.p)+1;
   lpf.par(5)=nwin;
   lpf.par(6)=nwin;
  else
   error('Lagprofiling cannot go freely over loop borders (for now)')
  end
  plen=lpf.par(1);
  wlen=lpf.par(8);
  lpf.par=lpf.par([1:plen plen+win1*wlen+(1:nwin*wlen)]);
 case 'alt_decoder'
  lpf.nrep=lpf.par(2);
  nsamp=lpf.par(3);
  if isempty(lpf.p)
   lpf.p=[0 lpf.nrep-1];
  end
  nwin=lpf.par(7);
  np=diff(lpf.p)+1;
  if rem(lpf.p(1),nwin)==0 && rem(lpf.p(2)+1,nwin)==0
   lpf.par(2)=np;
   win1=0;
  elseif fix(lpf.p(1)/nwin)==fix(lpf.p(2)/nwin)
   win1=rem(lpf.p(1),nwin);
   nwin=diff(lpf.p)+1;
   lpf.par(2)=nwin;
   lpf.par(7)=nwin;
  else
   error('Lagprofiling cannot go freely over loop borders (for now)')
  end
  plen=7;
  wlen=lpf.par(1);
  lpf.par=lpf.par([1:plen plen+win1*wlen+(1:nwin*wlen)]);
 case 'clutter'
  lpf.nrep=lpf.par(2);
  nsamp=lpf.par(3);
  if isempty(lpf.p)
   lpf.p=[0 lpf.nrep-1];
  end
 case 'resampler'
  nsamp=[];
 end
 lpf.raw=lpf.raw+(lpf.p(1)*nsamp+1:(lpf.p(2)+1)*nsamp);
 a_lpf(i)=lpf;
end
return
