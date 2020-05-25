function [date,leaps] = timeconv(date0,direction)
% [date,leaps] = timeconv(date0,direction,inleap)
% Convert between different time formats utc|tai|mat|unx|gup
%
% date: output time in new format
% leaps: tot no leap seconds at dates
% 
% date0:     input time to be converted, column vecctor
% direction: specifies the time formats of the input and output. 
%            Possible options: '(tai|utc|mat|unx|gup|kst)2(tai|utc|mat|unx|gup)'
%            Default 'utc2tai'
%
% time formats:	tai float secs since 1970-01-01
%               utc datevec [y,m,d,h,m,s], s>=60 for leaps
%               mat matlab date 
%               unx float secs since 1970-01-01 without leaps
%               gup [y,secs] secs>365*86400 for leaps
%               kst eros3 parameter block yymm ddHH MMSS format
%
%               Optional extra column for tai|utc|unx|gup with nanosecs
%               Optional extra column for mat with leapsecs used
%
% Currently step times are through Jan 1 2017, but need to be added in the
% function list as they are instuted. The first addition of leapseconds (10 s) was at 
% 'Jan 1 1972'. Following dates correspond to additions of new individual
% leapseconds.
if nargin<2 | isempty(direction)
  direction='utc2tai';
end
if nargin<3
  inleap=[];
end
f='utc mat unx tai gup kst';
dir=split(direction,'2');
unx0=datenum(1970,1,1);
leapin=[];
if ~contains(f,dir{1}) &&  length(dir)>1 && ~contains(f,dir{2})
  error(['Error: ' direction ' is not a possible convert direction.'])
end

%% ADD NEW LEAP DATES HERE:
stepdate = [...
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
step=[0;10;ones(length(stepdate)-1,1);Inf]/86400;
steptime=cumsum(step);
jansteps=strfind(stepdate(:,2)','a')'; %guptime can handle leaps over new year
stepdate=[-Inf;datenum(stepdate);Inf]; %step date conversion
ncol=size(date0,2);
ns_in=0;
%% Arg Checking
if contains('tai unx',dir{1})
  if ncol>1
    ns=date0(:,2); ns_in=1;
  else
    ns=rem(date0,1)*1e9;
  end
  date0=fix(date0(:,1))/86400+unx0;
elseif strcmp('gup',dir{1})
  if ncol>2
    ns=date0(:,3); date0(:,2)=date0(:,2)+ns*1e-9; ns_in=1;
  else
    ns=rem(date0(:,2),1)*1e9;
  end
  leapin=find(date0(:,2)>=365*86400); % tentative leaps
  date0=fix(date0(:,2))/86400+datenum(date0(:,1),1,1);
else
  if strcmp('kst',dir{1})
    date0=sscanf(sprintf('%04d%04d%04d',date0),'%02d%02d%02d%02d%02d%02d',[6 Inf])';
    date0(:,1)=date0(:,1)+1900; dir{1}='utc'; ncol=6;
  elseif strcmp('utc',dir{1}) && ncol>5 % extract secs>=60
    if ncol>6
      ns=date0(:,7); date0(:,7:end)=[]; ns_in=1;
    else
      ns=rem(date0(:,6),1)*1e9; date0(:,6)=fix(date0(:,6));
    end
    leapin=find(date0(:,6)>=60); % tentative leaps
  elseif ncol>1
    inleap=date0(:,2); date0(:,2:end)=[];
  end
  date0=datenum(date0); %will error if not a proper format
  if ~exist('ns') & ~ns_in
    ns=rem(date0*86400,1)*1e9; date0=fix(date0*86400)/86400;
  end
end
%% Conversion to tai
if ~strcmp(dir{1},'tai')
  l=discretize(date0,stepdate);
  if ~isempty(leapin)
    stepdates=sort([stepdate;stepdate(2:end-1)+step(2:end-1)]);
    l60=discretize(date0(leapin),stepdates);
    d=find(rem(l60,2)==0);
    l(leapin(d))=l(leapin(d))-1; % valid leaps
  elseif ~isempty(inleap)
    d=find(steptime(l)*86400~=inleap);
    date0(d)=date0(d)-step(l(d));
  end
  date0=date0+steptime(l);
end
l=discretize(date0,stepdate+steptime);
leaps=steptime(l)*86400;
%% Conversion from tai
if strcmp(dir{2},'tai')
  date=round((date0-unx0)*86400);
  if ns_in
    date=[date ns];
  else
    date=date+ns*1e-9;
  end
elseif strcmp(dir{2},'utc')
  date=date0-steptime(l);
  stepdates=sort([stepdate+steptime;stepdate(2:end-1)+steptime(1:end-2)]);
  l60=discretize(date0,stepdates);
  d=find(rem(l60,2)==0);
  date(d)=date(d)-1;
  date=round(datevec(date));
  ld=length(d);
  date(d,4:6)=[repmat([23 59 60],ld,1)+[zeros(ld,2) date(d,6)]];
  if ns_in
    date=[date ns];
  else
    date(:,6)=date(:,6)+ns*1e-9;
  end
else
  date=date0-steptime(l);
  if strcmp('unx',dir{2})
    date=round((date-unx0)*86400);
    if ns_in
      date=[date ns];
    else
      date=date+ns*1e-9;
    end
  elseif strcmp('gup',dir{2})
    stepdates=sort([stepdate+steptime;stepdate(2:end-1)+steptime(1:end-2)]);
    l60=discretize(date0,stepdates);
    i=find(rem(l60,2)==0 & ismember(l60/2,jansteps));
    dated=datevec(date);
    dated(i,1)=dated(i,1)-1;
    date=[dated(:,1) round((date-datenum(dated(:,1),1,1))*86400)];
    if ns_in
      date=[date ns];
    else
      date(:,2)=date(:,2)+ns*1e-9;
    end
  elseif strcmp('mat',dir{2})
    if ns_in
      date=[date ns];
    else
      date=date+ns/86400e9;
    end
  end
end
