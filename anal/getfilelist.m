function [list,msg]=getfilelist(dirpath,newer)

% [list,msg]= getfilelist(dirpath,newer)

global path_tmp

list=[]; msg=''; dirlist=[];
if nargin<2
 newer=[];
end

if isempty(dirpath)
  msg='Empty directory path';
elseif isunix
  if ~isempty(newer)
    newer=sprintf('-newer %s%08d.mat ',dirpath,newer);
  end
  template=[dirpath row(col('[0-9]')*ones(1,8)) '.mat'];
  d=[tempname '.txt'];
  cmd=sprintf('find %s %s-print >%s',template,newer,d);
  if unix(cmd)
    msg=['Error listing mat files in ' dirpath ' ' cmd];
  else
    list=textread(d,['%*' num2str(length(dirpath)) 's%d.mat']);
  end
  delete(d)
% list=ls([dpath template]);
% list(find(dirlist<32))=[];
% if isempty(list)
%   msg=['No mat files in ' dirpath];
% else
%   list=reshape(list,length(dpath)+12,[])';
%   list=str2num(list(:,length(dpath)+(1:8)));
% end
elseif ~exist(dirpath,'dir')
  msg=[dirpath ' is not a directory'];
else
  dirlist=dir([dirpath '*.mat']);
  dirlen=length(dirlist);
  list=zeros(dirlen,1);
  for i=dirlen:-1:1
    list(i)=sscanf(dirlist(i).name,'%8d');
  end
  if isempty(list)
    msg=[dirpath ' - No valid mat files'];
  end
  if ~isempty(newer)
    list=list(find(list>newer));
  end
end
list=sort(list)';
