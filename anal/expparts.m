function [msg,pulse,scan,comment,type,antenna]=expparts(s)

%[MSG,PULSE,SCAN,COMMENT,TYPE,ANTENNA] = EXPPARTS(NAME)
%
% Parse an EISCAT experiment name string, as specified 
% in the document
%
% TvE: Experiment Name Conventions Proposal to SAC 16/06/01
%
% If there are no errors, MSG is returned empty, else it
% contains an error message. The MSG should always be checked
% by the caller, because if there is an error, all the other
% returned values are undefined (ill-defined).
%
% Those parts that are not defined in NAME are returned empty.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

msg=[]; pulse=[]; scan=[]; comment=[]; type=[]; antenna=[];

% remove possible leading and trailing blanks
s=deblank(fliplr(s));
s=deblank(fliplr(s));

% (1) handle some special cases

if isempty(s)
  msg='empty';
  return
elseif length(s) > 32
  msg='too long';
  return
elseif s(1)=='_' | s(1)=='@'
  msg='PULSE missing';
  return
end

% (2) locate all special characters in the string

K0=find(s=='@');

% (4) Get the ANTENNA, and at the same time,
% check that only one '@' exists in the string.

if length(K0)>1
  msg='misplaced @';
  return
elseif isempty(K0)
  msg='ANTENNA missing';
  return
else
  antnames={'uhf','kir','sod','vhf','32m','42m','esr','32p'};
  [msg,antenna]=check_type(s(K0+1:end),antnames);
  if ~isempty(msg)
    return
  end
  s=s(1:K0-1);
end

while s(end)=='_' & ~isempty(s)
  s=s(1:end-1);
end
if isempty(s)
  msg='PULSE missing';
  return
end

K=find(s=='_');

% if there are no underscores in s , we are done
if isempty(K)
  pulse=s;
  return
end

% (5) We check for TYPE -- this would be the string after the last '_'

ant_str=s(K(end)+1:end);
types={'CP','FI','FR','GE','NO','NI','SW','UK','EI','3P','SP','AA'};
[type_msg,type] = check_type(ant_str,types);
% the last part was not TYPE, but it still could be either
% COMMENT or SCAN, so this it not an error (yet).
if isempty(type_msg) 
  % we found TYPE at the end of s, chop it from s
  s=s(1:K(end)-1);
  K=find(s=='_');
  if isempty(K)
    pulse=s;
    return
  end
end

% (6) Check for ONE Comment (Starts vid a Capital or number)

C=s(K+1);
K1=max(find((C>64 & C<91) | (C>47 & C<58)));
if ~isempty(K1)
  comment=s(K(K1)+1:end);
  s=s(1:K(K1)-1);
  K=find(s=='_');
  if isempty(K)
    pulse=s;
    return
  end
end

% (7) Now we have PULSE_SCAN left (or PUL_SE_SCAN)

scan=s(K(end)+1:end);
pulse=s(1:K(end)-1);

%end expparts


function [msg,type] = check_type(s,types)
%----------------------------------
msg = ''; 
type = '';
if length(s)~=length(char(types(1)))
  msg=['Illegal (' s ')'];
else
  K=strmatch(s,types);
  if isempty(K)
    msg=['Illegal (' s ')'];
  else
    type=s;
  end
end
return
%end get_type
