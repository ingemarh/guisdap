% tosecs.m: converts times to seconds
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function to convert time in form
% [Year Month Day Hour Min Sec]
% or in form [YYMM DDHH MMSS]   (this is the EISCAT style)
% to seconds from the beginning of year
%
% See also: toYMDHMS
% function [secs,years]=tosecs(x)
  function [secs,years]=tosecs(x)

[m,n]=size(x);
if n~=2 & n~=3 & n~=6,
  if m==2 | m==3 | m==6,
     x=x'; [m,n]=size(x);
  else
    fprintf('Illegal argument:')
    disp(x)
    secs=NaN;
    years=NaN;
  end
end

if n==2,
  y=x;
  x(:,[3,6])=rem(y,100);
  x(:,[2,5])=rem(floor(y/100),100); 
  x(:,[1,4])=floor(y/10000);   
elseif n==3,
  x(:,6)=rem(x(:,3),100); x(:,5)=floor(x(:,3)/100);
  x(:,4)=rem(x(:,2),100); x(:,3)=floor(x(:,2)/100);
  x(:,2)=rem(x(:,1),100); x(:,1)=floor(x(:,1)/100)+1900;
elseif n==6,
  if x(1,1)<100, x(:,1)=1900+x(:,1); end
end 

years=x(:,1);
for year=diff_val(x(:,1))'
  daym=[0 31 28 31 30 31 30 31 31 30 31 30 31];
  if rem(year,4)==0, daym(3)=29; end  % works up to 2100
  days=cumsum(daym(1:12))';
   
  ind=find(years==year);
  hours(ind,1)=(days(x(ind,2))+(x(ind,3)-1))*24+x(ind,4);
  secs(ind,1)=(hours(ind)*60+x(ind,5))*60+x(ind,6);
end
