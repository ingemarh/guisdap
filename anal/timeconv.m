function [date,leaps] = timeconv(date0,direction)
% [date,leaps] = timeconv(date0,direction)
%
% date: date output in new format
% leaps: leap seconds between the output and input time formats
%
% Convert between different time formats utc|tai|mat|unx
% 
% date0:input time to be converted, column vecctor
%	tai float secs since 1970-1-1
%       utc datevec [y,m,d,h,m,s], s>=60 for leaps
%       mat matlab date (or string format)
%       unx float secs since 1970-1-1
%       gup [y,secs]
% direction: specifies the time formats of the input and output. 
%            Possible options: '(tai|utc|mat|unx|gup)2(tai|utc|mat|unx|gup)'
%                               Default 'utc2tai'
% Currently step times are through Jan 1 2017, but need to be added in the
% function list as they are instuted. The first addition of leapseconds (10 s) was at 
% 'Jan 1 1972'. Following dates correspond to additions of new individual
% leapseconds.
if nargin<2
    direction='utc2tai';
end
f='utc mat unx tai gup';
dir=split(direction,'2');
unx0=datenum(1970,1,1);
leapin=[];
if ~contains(f,dir{1}) &&  length(dir)>1 && ~contains(f,dir{2})
  error(['Error: ' direction ' is not a possible convert direction. Misspelled?'])
end

%% ADD NEW LEAP DATES HERE:
stepdates = [...
    'Jan 1 1972'
    'Jul 1 1972'
    'Jan 1 1973'
    'Jan 1 1974'
    'Jan 1 1975'
    'Jan 1 1976'
    'Jan 1 1977'
    'Jan 1 1978'
    'Jan 1 1979'
    'Jan 1 1980'
    'Jul 1 1981'
    'Jul 1 1982'
    'Jul 1 1983'
    'Jul 1 1985'
    'Jan 1 1988'
    'Jan 1 1990'
    'Jan 1 1991'
    'Jul 1 1992'
    'Jul 1 1993'
    'Jul 1 1994'
    'Jan 1 1996'
    'Jul 1 1997'
    'Jan 1 1999'
    'Jan 1 2006'
    'Jan 1 2009'
    'Jul 1 2012'
    'Jul 1 2015'
    'Jan 1 2017'];
%% Convert Steps to datenums and make step offsets
step=[0;10;ones(length(stepdates)-1,1);0]/86400;
steptime=cumsum(step);
stepdate = [-Inf;datenum(stepdates);Inf]; %step date conversion
%% Arg Checking
if strcmp('tai',dir{1})
  date0=date0/86400+unx0;
elseif strcmp('unx',dir{1})
  date0=date0/86400+unx0;
  if size(date,2)>1
    date0=date0+date0(:,2)/86400;  % leaps in 2nd col
    dir{1}='tai';
  end
elseif strcmp('gup',dir{1})
  date0=date0(:,2)/86400+datenum(date0(:,1),1,1);
else
  if strcmp('utc',dir{1}) && isnumeric(date0) && size(date0,2)>5 % extract secs>60
    leapin=find(date0(:,6)>=60);
  end
  date0=datenum(date0); %will error if not a proper format
end
%% Conversion to tai
if ~strcmp(dir{1},'tai')
  l=discretize(date0,stepdate);
  if strcmp(dir{1},'utc') && ~isempty(leapin)
    l(leapin)=l(leapin)-1;
  end
  date0=date0+steptime(l);
end
l=discretize(date0,stepdate+steptime);
leaps=steptime(l)*86400;
%% Conversion from tai
if strcmp(dir{2},'tai')
  date=(date0-unx0)*86400;
elseif strcmp(dir{2},'utc')
  date=date0-steptime(l);
  l60=discretize(date0,sort([stepdate;stepdate+step]))/2,
  i=find(l60(2:2:end))*2-1;
  date(i)=date(i)-1;
  date=datevec(date);
  li=length(i);
  date(i,4:6)=[repmat([23 59 60],li,1)+[zeros(li,2) date(i,6)]];
else
  date=date0-steptime(l);
  if strcmp('unx',dir{2})
    date=(date-unx0)*86400;
  elseif strcmp('gup',dir{2})
    dated=datevec(date);
    jansteps=strfind(stepdates(:,2)','a')'+1; %guptime can handle leaps over new year
    l60=discretize(date,sort([stepdate(jansteps);stepdate(jansteps)+step(jansteps)]))/2;
    i=find(l60(2:2:end))*2-1;
    dated(i,1)=dated(i,1)-1;
    date=[dated(:,1) (date-datenum(dated(:,1),1,1))*86400];
  end
end
