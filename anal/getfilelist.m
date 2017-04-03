function [list,msg]=getfilelist(dirpath,newer)

% [list,msg]= getfilelist(dirpath,newer)

global a_realtime local

list=[]; msg=''; dirlist=[];
if nargin<2
 newer=[];
end

if isempty(dirpath)
  msg='Empty directory path';
elseif isunix & a_realtime | strfind(dirpath,'?')
  i=' ';
  if ~isempty(newer)
    i=sprintf(' -newer %s/%08d%s ',newer.dir,newer.file,newer.ext);
  end
  template=[row(col('\[0-9]')*ones(1,8)) '.mat\*'];
  d=[local.tfile '.txt'];
  dirpath(strfind(dirpath,'\'))=[]; % remove escapes
  cmd=sprintf('find %s -name %s%s-print >%s 2>/dev/null',dirpath(1:end-1),template,i,d);
  if unix(cmd)
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
  if exist(d), delete(d), end
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
    dirlist=dir(fullfile(dp,dirs(j).name,'*.mat'));
    dirlen=length(dirlist);
    if ~dirlen
     dirlist=dir(fullfile(dp,dirs(j).name,'*.mat.bz2'));
     dirlen=length(dirlist);
    end
    l=repmat(struct('dir',fullfile(dp,dirs(j).name),'file',0,'ext',''),[dirlen 1]);
    for i=dirlen:-1:1
      l(i).file=sscanf(dirlist(i).name,'%08d%*s');
      l(i).ext=sscanf(dirlist(i).name,'%*8s%s');
      if ~isnumeric(l(i).file) | ~strcmp(l(i).ext(1),'.'), l(i)=[]; end
    end
    list=[list;l];
  end
  if isempty(list) % Look for hdf5 files
    if isempty(dirs)
      dp=[];
      dirs=struct('name',fileparts(dirpath));
    end
    list=[];
    for j=dirs'
      dirlist=dir(fullfile(dp,j.name,'*.hdf5'));
      for f=dirlist'
        h5file=fullfile(j.name,f.name);
        h5d=h5info(h5file);
        h5d=struct2cell(h5d.Groups(1).Groups);
        dirlen=length(h5d); h5f=char(h5d(1,:));
        l=repmat(struct('fname',h5file,'file',0),[dirlen 1]);
        for i=dirlen:-1:1
          l(i).file=sscanf(h5f(i,:),'/Data/%08d');
          if ~isnumeric(l(i).file), l(i)=[]; end
        end
        list=[list;l];
      end
    end
  end
  if ~isempty(newer)
    d=find(cell2mat({list.file})>newer.file);
    list=list(d);
  end
end
if ~isempty(list)
  [dum,d]=sort(cell2mat({list.file})); list=list(d);
  global maxlend
  if ~isempty(maxlend) & length(d)>maxlend, list=list(1:maxlend); end
elseif isempty(newer)
  msg=[dirpath ' - No valid mat/hdf5 files'];
end
