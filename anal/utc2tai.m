
function [date,leaps] = utc2tai(date0,direction)
% [date,leaps] = utc2tai(date0,direction)
%
% date: date output in new format
% leaps: leap seconds between the output and input time formats
%
% Convert between different time formats (utc --> tai), or vice versa (tai --> utc)
% 
% date0:     input time to be converted
% direction: specifies the time formats of the input and output. 
%            Possible options:
%            1: 'utc2tai'
%            2: 'tai2utc'
%  
% Currently step times are through Jan 1 2017, but need to be added in the
% function list as they are instuted. The first addition of leapseconds (10 s) was at 
% 'Jan 1 1972'. Following dates correspond to additions of new individual
% leapseconds.
%   
if nargin < 2
    error('Error: direction was not specified, define format of time input and output.')
end
if ~contains('utc2tai tai2utc',direction)
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
steptime = ((0:length(stepdates)-1)'+10)./86400; %corresponding step time (sec)
%% Arg Checking
if ~isnumeric(date0) %make sure date0 are datenums, if not try converting
    date0 = datenum(date0); %will error if not a proper format
end
%%
if date0<stepdates(1)
    leaps = 0;
    date = date0;
    return
end
%% Array Sizing
sz = size(date0);
date0 = date0(:);
date0 = repmat(date0,[1 size(stepdates,2)]);
stepdates = repmat(stepdates,[size(date0,1) 1]);
%% Conversion
leapsec_inday = steptime(sum((date0 - stepdates) >= 0,2)); 
leaps = leapsec_inday*86400;
if strcmp(direction,'utc2tai')
    date = date0(:,1) + leapsec_inday;
elseif strcmp(direction,'tai2utc')
    date = date0(:,1) - leapsec_inday;
end
%% Reshape Output Array
date = reshape(date,sz);
end