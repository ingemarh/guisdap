function [date,leaps] = utc2tai(date0,direction)
% [date,leaps] = utc2tai(date0,direction)
%
% date: date output in new format
% leaps: leap seconds between the output and input time formats
%
% Convert between different time formats utc|tai|mat|unx
% 
% date0:     input time to be converted
% direction: specifies the time formats of the input and output. 
%            Possible options:
%            1: '(utc|mat|unx|gup)2tai' 
%            2: 'tai2(utc|mat|unx|gup)'
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
stepdates = datenum(stepdates)'; %step date conversion
step=[10;ones(length(stepdates)-1,1)]'/86400;
steptime=cumsum(step);
ns=length(stepdates);
%% Arg Checking
if contains('unxtai',dir{1})
  date0=date0/86400+unx0;
elseif contains('gup',dir{1})
  date0=date0(:,1)/86400+datenum(date0(:,2),1,1);
else
  date0=datenum(date0); %will error if not a proper format
end
%% Array Sizing
date0=date0(:);
nt=size(date0,1);
date=date0;
leaps=zeros(nt,1);
d=find(date0>stepdates(1));
ld=length(d);
date0=repmat(date0(d),1,ns);
stepdates=repmat(stepdates,ld,1);
%% Conversion
leapsec_inday=steptime(sum((date0-stepdates)>=0,2));
leaps(d)=leapsec_inday*86400;
if strcmp(dir{2},'tai')
  date(d)=date0(:,1)+leapsec_inday;
end
if strcmp(dir{1},'tai')
  date(d)=date0(:,1)-leapsec_inday;
end
if contains('unxtai',dir{2})
  date=(date-unx0)*86400;
elseif contains('gup',dir{2})
  dated=datevec(date);
  date=[dated(:,1) (date-datenum(dated,1,1))*86400];
elseif contains('utc',dir{2})
  if strcmp(dir{1},'tai')
    step0=repmat(step,ld,1);
    [i,j]=find(date0>=stepdates & date0<(stepdates+step0));
    date(d(i))=date(d(i))-1;
    date=datevec(date);
    li=length(i);
    date(d(i),4:6)=[repmat([23 59 60],li,1)+[zeros(li,2) [date(d(i),6)+step0(j)]]];
  else
    date=datevec(date);
  end
end
