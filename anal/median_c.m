% median_c.m:  median of complex array
% GUISDAP v.9.2 20-12-07 Copyright EISCAT
% See also: subr_backgr
%function m=median_c(x)
function m=median_c(x)
m=median(real(x))+i*median(imag(x));
