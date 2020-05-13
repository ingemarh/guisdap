% toYMDHMS.m: converts time in seconds to YMDHMS
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% AH 94-4-15 Added the Uppsala formatting commands
% toYMDHMS.m
% function to convert time in second from the beginning of year
% to form [Year Month Day Hour Min Sec]
% If mask is given, those elements will be returned in a character
% string, with separator 'sep' if specified
% Leap second information
%
% See also: tosecs utc2tai
% function YMD=toYMDHMS(years,secs,mask,sep)
function [YMD,leap]=toYMDHMS(years,secs,mask,sep)

secs=col(secs);
if length(years)==1
 years=years*ones(size(secs));
else
 years=col(years);
end
[YMD,leap]=utc2tai([secs years],'gup2utc');

if nargin==2 return, end
if nargin==3 sep=':';end
YMD(:,1)=YMD(:,1)-100*floor(YMD(:,1)/100);
fmt='%0.2d';
for i=1:length(mask)-1 fmt=[fmt sep '%0.2d']; end
for i=1:length(secs)
 YMD2(i,:)=sprintf(fmt,YMD(i,mask));
end
YMD=YMD2;

