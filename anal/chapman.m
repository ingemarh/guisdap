function dens=chapman(h,ne,h0,hd)
dens=0;
for i=1:length(ne)
 hh=(h0(i)-h)/hd(i);
 dens=dens+ne(i)*exp(1+hh-exp(hh));
end
