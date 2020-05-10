
function [dateout,leaps] = utc2taigps_test(datein,direction)
    % [date,leaps] = utc2taigps(date0,direction)
    %
    % date: date output in new format
    % leaps: leap seconds between the output and input time formats
    %
    % Convert between different time formats (utc --> tai or gps), or vice
    % versa (tai or gps --> utc)
    % 
    % date0:     input time to be converted
    % direction: specifies the time formats of the input and output. 
    %            Possible options:
    %            1: 'utc2tai'
    %            2: 'utc2gps'
    %            3: 'tai2utc'
    %            4: 'gps2utc'
if nargin < 2
    error('Error: direction was not specified, define format of time input and output.')
end
if ~isnumeric(datein) %make sure date0 are datenums, if not try converting
    datein = datenum(datein); %will error if not a proper format
end
if ~contains('utc2tai utc2gps gps2utc tai2utc',direction)
    error(['Error: ' direction ' is not a possible convert direction. Misspelled?'])
end

if contains('utc2tai utc2gps',direction)
    [date_gps, leaps_gps] = utc2gps(datein,'utc2gps');
    if strcmp(direction,'utc2tai')
        dateout = (date_gps*86400 + 19)/86400;    % tai is always 19 seconds ahead of gps
        leaps = leaps_gps + 19;
    else
        dateout = date_gps;
        leaps = leaps_gps;
    end
else
    if strcmp(direction,'tai2utc')
        date_gps = (datein*86400 - 19)/86400;     % gps is always 19 seconds behind of tai
    else
        date_gps = datein;
    end
    [dateout,leaps_gps] = utc2gps(date_gps,'gps2utc');
    if strcmp(direction,'tai2utc')
        leaps = -(leaps_gps + 19);
    else
        leaps = - leaps_gps;
    end
end

end


function [date1,leapsec_insec] = utc2gps(date0,convertdirection)
%UTC2GPS Convert UTC(GMT) time tags to GPS time accounting for leap seconds
%   or vice versa
%   UTC2GPS(date) corrects an array of UTC dates(in any matlab format) for
%   leap seconds and returns an array of GPS datenums where:
%   GPS = UTC + steptime (or vice versa)
%   Currently step times are through Jan 1 2017, but need to be added below
%   as they are instuted. All input dates must be later than the start of
%   GPS time on Jan 6 1980 00:00:00
%
%   convertdirection is either 'utc2gps' or 'gps2utc'
%% ADD NEW LEAP DATES HERE:
stepdates = [...
    'Jan 6 1980'
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
steptime = (0:length(stepdates)-1)'./86400; %corresponding step time (sec)
%% Arg Checking
if ~isempty(find(date0 < stepdates(1)))%date0 must all be after GPS start date
    error('Input dates must be after 00:00:00 on Jan 6th 1980') 
end
%% Array Sizing
sz = size(date0);
date0 = date0(:);
date0 = repmat(date0,[1 size(stepdates,2)]);
stepdates = repmat(stepdates,[size(date0,1) 1]);
%% Conversion
leapsec_inday = steptime(sum((date0 - stepdates) >= 0,2));
leapsec_insec = leapsec_inday*86400;
if strcmp(convertdirection,'utc2gps')
    date1 = date0(:,1) + leapsec_inday;
elseif strcmp(convertdirection,'gps2utc')
    date1 = date0(:,1) - leapsec_inday;
end
%% Reshape Output Array
date1 = reshape(date1,sz);
end



