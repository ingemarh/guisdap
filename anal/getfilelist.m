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
  template=[row(col('\[0-9]')*ones(1,8)) '.mat\*'];
  d=[tempname '.txt'];
  cmd=sprintf('find %s -name %s %s-print >%s 2>/dev/null',dirpath(1:end-1),template,newer,d),
  if unix(cmd) & unix(cmd)
    msg=['Error listing mat files in ' dirpath ' ' cmd];
  elseif exist(d)
    try
      [dirs,file,ext]=textread(d,['%' num2str(length(dirpath)-1) 's/%d%s']);
      dirlen=length(file);
      list=repmat(struct('dir','','file',0,'ext',''),[dirlen 1]);
      for i=1:dirlen
        list(i).dir=char(dirs(i));
        list(i).file=file(i);
        list(i).ext=char(ext(i));
      end
      %ist=cell2struct({char(dir) list char(ext)},{'dir' 'file' 'ext'},2)
    catch, disp(lasterr)
    end
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
    if ~dirlen
     dirlist=dir(fullfile(dp,dirs(j).name,'*.mat.bz2'));
     dirlen=length(dirlist);
    end
    l=repmat(struct('dir',fullfile(dp,dirs(j).name),'file',0,'ext',''),[dirlen 1]);
    for i=dirlen:-1:1
      l(i).file=sscanf(dirlist(i).name,'%8d');
      l(i).ext=sscanf(dirlist(i).name,'%*8s%s');
    end
    list=[list;l];
  end
  if isempty(list)
    msg=[dirpath ' - No valid mat files'];
  end
  if ~isempty(newer)
    d=find(cell2mat({list.file})>newer.file);
    list=list(d);
  end
end
if ~isempty(list)
  [dum,d]=sort(cell2mat({list.file})); list=list(d);
end
