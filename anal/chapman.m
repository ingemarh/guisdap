function dens=chapman(h,chap)
dens=0;
for i=1:size(chap,2)
  hh=(chap(2,i)-h)/chap(3,i);
  dens=dens+chap(1,i)*exp(1+hh-exp(hh));
end
