% gupround.m:  rounds off close to integer values
%
% See also: eps
function ar=gupround(a)
ar=round(a);
if max(abs(a-ar))>eps('single'), ar=a; end
