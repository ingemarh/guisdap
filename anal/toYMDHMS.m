% toYMDHMS.m: converts time in seconds to YMDHMS
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% AH 94-4-15 Added the Uppsala formatting commands
% toYMDHMS.m
% function to convert time in second from the beginning of year
% to form [Year Month Day Hour Min Sec]
% If mask is given, those elements will be returned in a character
% string, with separator 'sep' if specified
%
% See also: tosecs
% function YMD=toYMDHMS(years,secs,mask,sep)
  function YMD=toYMDHMS(years,secs,mask,sep)

secs=col(secs);
if length(years)==1 years=years*ones(size(secs));
else               years=col(years); end

  days=cumsum([0 31 28 31 30 31 30 31 31 30 31 30 31])';
  days=days(:,ones(1,length(years)));
  ind=find(rem(years,4)==0);
  if length(ind)>0,days(3:13,ind)=days(3:13,ind)+1;end

  YMD(:,6)=rem(secs,60); apu=floor(secs/60);
  YMD(:,5)=rem(apu,60); apu=floor(apu/60);
  YMD(:,4)=rem(apu,24); apu=floor(apu/24)+1; 
  index=sum((apu(:,ones(1,13))'-days)>0);
  YMD(:,2)=index';
  YMD(:,3)=apu-days(YMD(:,2),1);
  YMD(:,1)=years;

if nargin==2 return, end

if nargin==3 sep=':';end

YMD(:,1)=YMD(:,1)-100*floor(YMD(:,1)/100);
fmt='%0.2d';
for i=1:length(mask)-1 fmt=[fmt sep '%0.2d']; end
for i=1:length(secs)
 YMD2(i,:)=sprintf(fmt,YMD(i,mask));
end
YMD=YMD2;

