% DS_choose: Utility routine used by the design package
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% Depending on the value of the first input argument:
% if empty     : return the second
% if nonempty  : return the first
% Thus the second input argument is used, if the first is empty
function argv=DS_choose(arg,value)

if isempty(arg)
  argv=value;
else
  argv=arg;
end
