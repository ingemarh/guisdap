function chk_lagprofiling()
global a_lagprofiling
i=0;
for lpf=a_lagprofiling'
 i=i+1;
 if strcmp(lpf.lib,'plwin')
  if lpf.p(1)~=0 || lpf.p(2)~=lpf.nrep-1
   if fix(lpf.p(1)/lpf.nwin)~=fix(lpf.p(2)/lpf.nwin)
    error('Lagprofiling has to be within one loop (for now)')
   end
   lpf.nwin=diff(lpf.p)+1;
   lpf.par(5)=lpf.nwin;
   lpf.par(6)=lpf.nwin;
   lpf.par=lpf.par([1:24 24+(lpf.p(1)*lpf.wlen+1:(lpf.p(2)+1)*lpf.wlen)]);
  end
 end
 lpf.raw=lpf.raw+(lpf.p(1)*lpf.nsamp+1:(lpf.p(2)+1)*lpf.nsamp);
 a_lagprofiling(i)=lpf;
end
return
