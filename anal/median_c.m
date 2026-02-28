% median_c.m:  median of complex array
% GUISDAP v.9.2 20-12-07 Copyright EISCAT
% See also: subr_backgr
%function m=median_c(x,d)
function m=median_c(x,d)
if nargin<2, d=[]; end
if isempty(d)
  m=complex(median(real(x)),median(imag(x)));
else
  [~,l]=sort(abs(x));
  k=l(1:ceil(length(l)/d));
  m=complex(median(real(x(k))),median(imag(x(k))));
end
