function [list,msg,dpath]= getfilelist(dirpath,recurse) %{

%[LIST,MSG]= GETFILELIST(DIRPATH,RECURSE)

% 21-Jul-1998 Jm
%====================================================================

list = []; msg = ''; dirlist=[]; newer=[];
if nargin < 2
 recurse=[];
end

if strfind(dirpath,'?')
 dpath=dirpath;
 dirlist=[];
 if ~isempty(recurse) & isnumeric(recurse)
  newer=sprintf('-newer %s%08d.mat ',dpath,recurse);
  recurse=[];
 end
else
 if isempty(dirpath)
  msg = 'Empty directory path';
  return
 end

 if exist(dirpath,'dir') == 7
  dpath = dirpath;
  template = [dpath,'*.mat'];
 else
  msg = [dirpath ' is not a directory'];
  return
 end

 dirlist = dir(template);
end
if isempty(dirlist)
 dpath=fullfile(dpath,recurse,filesep);
 if isunix
  cmd=sprintf('find %s -name "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].mat" %s-print',dpath(1:end-1),newer);
  [k,dirlist]=unix(cmd);
% if k, pause(.6), [k,dirlist]=unix(cmd); end
 else
  k=0;
  dirlist=ls([dpath '[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].mat']);
 end
 d=find(dirlist<32); dirlist(d)=[];
 if k
  msg = ['Error listing mat files in ' dirpath ' ' cmd];
 elseif isempty(dirlist) & isempty(newer)
  msg = ['No mat files in ' dirpath];
 else
  try, list=reshape(dirlist,length(dpath)+12,[])';
  list=sort(str2num(list(:,length(dpath)+(1:8))))';
  catch, dirlist, msg(lasterr); list=[]; end
 end
 return
end

dirlen = length(dirlist);
list = zeros(1,dirlen);
k = 0;
for i = 1:dirlen
 name = dirlist(i).name;
 a = sscanf(name,'%d');
 if ~isempty(a)
  k = k+1;
  list(k) = a;
 end
end

if k < dirlen
 if k == 0
  msg = [dirpath ' - No valid mat files'];
  return;
 else
  list = list(1:k);
 end
end
list = sort(list);
%}getfilelist
