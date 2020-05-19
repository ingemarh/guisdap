% function [secs,years,leaps]=tosecs(x)
%
% convert time in form
%   [Year Month Day Hour Min Sec]
%   [yymm ddHH MMSS]  (this is the EISCAT style)
%   [yyyymmdd HHMMSS]
% to seconds from the beginning of years and total no leap seconds
%
% See also: toYMDHMS timeconv
function [secs,years,leaps]=tosecs(x)

[m,n]=size(x);

if n~=2 && n~=3 && n~=6,
  if m==2 || m==3 || m==6,
     x=x'; n=m;
  else
    warning('GUISDAP:tosecs',sprintf('Time vectors of 2,3,6 allowed: %d',m))
    secs=NaN; years=NaN; leaps=NaN; return
  end
end
if n==2
  x=sscanf(sprintf('%08d%06d',x),'%04d%02d%02d%02d%02d%02d',[6 Inf])';
elseif n==3
  x=sscanf(sprintf('%04d%04d%04d',x),'%04d%02d%02d%02d%02d%02d',[6 Inf])';
end
if x(1,1)<100, x(:,1)=1900+x(:,1); end
[tt,leaps]=timeconv(x,'utc2gup');
secs=tt(:,2); years=tt(:,1);
