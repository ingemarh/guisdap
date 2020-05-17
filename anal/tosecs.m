% tosecs.m: converts times to seconds
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function to convert time in form
%   [Year Month Day Hour Min Sec]
%   [yymm ddHH MMSS]  (this is the EISCAT style)
%   [yyyymmdd HHMMSS]
% to seconds from the beginning of year
%
% See also: toYMDHMS timeconv
% function [secs,years]=tosecs(x)
function [secs,years]=tosecs(x)

[m,n]=size(x);

if n~=2 && n~=3 && n~=6,
  if m==2 || m==3 || m==6,
     x=x'; n=m;
  else
    fprintf('Illegal argument:')
    disp(x)
    secs=NaN; years=NaN; return
  end
end
if n==2
  x=compose('%08d%06d',x);
  f='yyyymmddHHMMSS';
  [tt,l]=timeconv(datenum(x,f),'mat2gup');
elseif n==3
  x=compose('%04d%04d%04d',x);
  f='yymmddHHMMSS';
  [tt,l]=timeconv(datenum(x,f),'mat2gup');
elseif n==6
  if x(1,1)<100, x(:,1)=1900+x(:,1); end
  [tt,l]=timeconv(x,'utc2gup');
end
secs=tt(:,2); years=tt(:,1);
