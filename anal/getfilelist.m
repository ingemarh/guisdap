function [list,msg]=getfilelist(dirpath,newer)

% [list,msg]= getfilelist(dirpath,newer)

global path_tmp a_realtime

list=[]; msg=''; dirlist=[];
if nargin<2
 newer=[];
end

if isempty(dirpath)
  msg='Empty directory path';
elseif isunix & a_realtime | strfind(dirpath,'?')
  if ~isempty(newer)
    newer=sprintf('-newer %s%08d.mat ',dirpath,newer);
  end
  template=[row(col('\[0-9]')*ones(1,8)) '.mat'];
  d=[tempname '.txt'];
  cmd=sprintf('find %s -name %s %s-print >%s 2>/dev/null',dirpath(1:end-1),template,newer,d);
  if unix(cmd) & unix(cmd)
    msg=['Error listing mat files in ' dirpath ' ' cmd];
  elseif exist(d)
    try, list=textread(d,['%*' num2str(length(dirpath)) 's%d.mat']);
    catch, end
  end
  delete(d)
else
  dirpath=dirpath(1:end-1);
  i=strfind(dirpath,'*');
  if i
    dirs=dir(dirpath);
    dp=fileparts(dirpath);
  else
    dirs.name=dirpath;
    dp=[];
  end
  for j=1:length(dirs)
    dirlist=dir(fullfile(dp,dirs(j).name,'*.mat'));
    dirlen=length(dirlist);
    l=zeros(dirlen,1);
    for i=dirlen:-1:1
      l(i)=sscanf(dirlist(i).name,'%8d');
    end
    list=[list;l];
  end
  if isempty(list)
    msg=[dirpath ' - No valid mat files'];
  end
  if ~isempty(newer)
    list=list(find(list>newer));
  end
end
if ~isempty(list)
  list=sort(list)';
end
