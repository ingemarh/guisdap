% guispert.m: special experiment specific hacks
% See also: GUISPERT GUIZARD
%
if name_site=='V'
 dr=length(d_data)/2;
 ns=836; nl=30;
 lid=ns*nl-(nl-1)*nl/2;
 if d_date<datenum(2021,4,30,9,26,30) %bug in clutter.c
  d=[12+(1:lid) 2*12+lid+(1:lid) 142769+(1:lid)];
  d_data([d dr+d])=conj(d_data([d dr+d]));
 end
 for r=0:-1
  mp=d_data(r*dr+12+(1:lid));
  ma=d_data(r*dr+2*12+lid+(1:lid));
  mb=d_data(r*dr+2*142769+(1:lid));
  b=zeros(nl,1);
  for l=0:nl-1
   s=l*ns-(l-1)*l/2+(1:ns-l);
   b(l)=median_c(mb(s));
   mp(s)=mp(s)-b(l);
   ma(s)=ma(s)-b(l);
  end
  [x,e]=shinv(mp,ma,ns,[1 1 1]);
  ns=ns+nl;
  for l=0:nl-1
   s=l*ns-(l-1)*l/2+(1:ns-l);
   mp(s)=mp(s)+b(l);
  end
  lid=ns*nl-(nl-1)*nl/2;
  d_data(r*dr+12+(1:lid))=mp;
  voff(:,r)=e;
 end
end

if name_site=='L' && d_date<datenum(2025,10,08,12,00,00) % Problems with channel 3 at ESR ion line
    fa=(median(d_data((-25:-1)+887))+median(d_data((-25:-1)+63479)))/2/median(d_data((1:875)+301631));
    d_data(301620:364481)=fa*d_data(301620:364481);
end
