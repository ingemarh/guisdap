function [list,msg,dpath]= getfilelist(dirpath,recurse) %{

%[LIST,MSG]= GETFILELIST(DIRPATH,RECURSE)

% 21-Jul-1998 Jm
%====================================================================

global path_tmp

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
 files=[row(col('[0-9]')*ones(1,8)) '.mat'];
 if isunix
  d=[tempname '.txt'];
  cmd=sprintf('find %s -name "%s" %s-print >%s',dpath(1:end-1),files,newer,d);
  if unix(cmd)
   msg=['Error listing mat files in ' dirpath ' ' cmd];
  else
   list=textread(d,['%*' num2str(length(dpath)) 's%d.mat'])';
   if ~isempty(list), list=sort(list); end
  end
  delete(d)
 else
  dirlist=ls([dpath files]);
  d=find(dirlist<32); dirlist(d)=[];
  if isempty(dirlist) & isempty(newer)
   msg=['No mat files in ' dirpath];
  else
   try, list=reshape(dirlist,length(dpath)+12,[])';
   list=sort(str2num(list(:,length(dpath)+(1:8))))';
   catch, dirlist, msg=lasterr; list=[]; end
  end
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
