function [list,msg]=qetfilelist(dirpath,newer)

% [list,msg]= qetfilelist(dirpath,newer)

list=[]; msg=''; dirlist=[];
if nargin<2
 newer=[];
end

if isempty(dirpath)
  msg='Empty directory path';
else
  dirpath=dirpath(1:end-1);
  if strfind(dirpath,'*')
    dp=fileparts(dirpath);
    dirs=dir(dirpath);
  else
    dp='';
    dirs.name=dirpath;
  end
  for j=1:length(dirs)
    dirlist=dir(fullfile(dp,dirs(j).name,'*.bin'));
    dirlen=length(dirlist);
    l=repmat(struct('dir',fullfile(dp,dirs(j).name),'file',0,'ch',0,'time',0),[dirlen 1]);
    for i=1:dirlen
      l(i).file=dirlist(i).name;
      a=sscanf(dirlist(i).name,'ch%1d_%4d%2d%2d_%2d%2d%2d');
      l(i).ch=a(1);
      l(i).time=86400*datenum(row(a(2:7)));
    end
    list=[list;l];
  end
  if ~isempty(newer)
    d=find(cell2mat({list.time})>newer.time);
    list=list(d);
  end
end
if ~isempty(list)
  [dum,d]=sort(cell2mat({list.time})); list=list(d);
  global maxlend
  if ~isempty(maxlend) & length(d)>maxlend, list=list(1:maxlend); end
elseif isempty(newer)
  msg=[dirpath ' - No valid data files'];
end
