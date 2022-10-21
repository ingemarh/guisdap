function [status,result]=gupsystem(cmd)
%Wrapper for avoiding mathworks libraries
if isunix
  b='LD_LIBRARY_PATH="" ' ;
else
  b=''
end
[status,result]=system([b cmd]);
