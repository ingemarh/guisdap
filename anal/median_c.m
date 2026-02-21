% median_c.m:  median of complex array
% GUISDAP v.9.2 20-12-07 Copyright EISCAT
% See also: subr_backgr
%function m=median_c(x,d)
function m=median_c(x,d)
if nargin<2, d=[]; end
if isempty(d)
  m=median(real(x))+i*median(imag(x));
else
  xr=sort(real(x));
  xi=sort(imag(x));
  k=ceil(length(xr)/d);
  m=median(xr(1:k))+i*median(xi(1:k));
end
