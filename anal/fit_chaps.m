function err=fit_chaps(x,ne,h,forc,tol,fac,f)
errextra=sinh(2*(x-forc)./tol);
x=exp(x);
ne_ch=chapman(h,x([1 4]),x([2 5]),x([3 6]));
err=(ne-ne_ch)./fac;
err=asinh(err/2);
%errextra=zeros(size(errextra));
err=[err;errextra];
if f==1, err=norm(err); end
