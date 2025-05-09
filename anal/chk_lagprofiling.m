function chk_lagprofiling(loop)
global a_lpf
i=0;
if nargin==0

if ~isfield(a_lpf,'nrep'), [a_lpf.nrep]=deal([]); end
if ~isfield(a_lpf,'p'), [a_lpf.p]=deal([]); end
if ~isfield(a_lpf,'skip'), [a_lpf.skip]=deal(0); end
if ~isfield(a_lpf,'data'), [a_lpf.data]=deal(0); end
if ~isfield(a_lpf,'raw'), [a_lpf.raw]=deal(0); end
if ~isfield(a_lpf,'loop'), [a_lpf.loop]=deal(1); end
for lpf=a_lpf
 i=i+1;
 if isempty(lpf.skip), lpf.skip=0; end
 if isempty(lpf.data), lpf.data=0; end
 %if isempty(lpf.raw), lpf.raw=0; end
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
  %if lpf.loop>1, lpf.par(2)=lpf.par(2)*lpf.loop; end
  lpf.nrep=lpf.par(2);
  nsamp=lpf.par(3);
  if isempty(lpf.p)
   lpf.p=[0 lpf.nrep-1];
  end
 case 'resampler'
  nsamp=[];
 end
 if ~isempty(lpf.raw)
  lpf.raw=lpf.raw+(lpf.p(1)*nsamp+1:(lpf.p(2)+1)*nsamp);
 end
 a_lpf(i)=lpf;
end

else

for lpf=a_lpf
 if any(lpf.p~=[0 lpf.nrep-1])
  error('guisdap:lagprof','No looping for reduced set')
 end
 i=i+1;
 switch lpf.lib
 case 'plwin'
  lpf.nrep=lpf.nrep*loop;
  lpf.par(6)=lpf.nrep;
  nsamp=lpf.par(7)+lpf.par(11);
  lpf.p=[0 lpf.nrep-1];
 case 'alt_decoder'
  lpf.nrep=lpf.nrep*loop;
  lpf.par(2)=lpf.nrep;
  nsamp=lpf.par(3);
  lpf.p=[0 lpf.nrep-1];
 case 'clutter'
  lpf.nrep=lpf.nrep*loop;
  lpf.par(2)=lpf.nrep;
  nsamp=lpf.par(3);
  lpf.p=[0 lpf.nrep-1];
 case 'resampler'
  nsamp=[];
 end
 lpf.raw=(lpf.raw(1)-1)*loop+(1:lpf.nrep*nsamp);
 a_lpf(i)=lpf;
end

end
return
