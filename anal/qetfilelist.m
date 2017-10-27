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
    qraw=0;
    dirlist=dir(fullfile(dp,dirs(j).name,'ch3*.bin'));
    if isempty(dirlist)
      dirlist=dir(fullfile(dp,dirs(j).name,'ch2*.rst'));
    end
    if isempty(dirlist)
      dirlist=dir(fullfile(dp,dirs(j).name,'*-000_000000.*'));
      qraw=1;
    end
    dirlen=length(dirlist);
    l=repmat(struct('dir',fullfile(dp,dirs(j).name),'file',0,'time',0),[dirlen 1]);
    for i=1:dirlen
      l(i).file=dirlist(i).name;
      if qraw
        d=datenum(dirlist(i).date);
      else
        a=sscanf(dirlist(i).name,'ch%1d_%4d%2d%2d_%2d%2d%2d');
        d=datenum(row(a(2:7)));
      end
      l(i).time=86400*(d-8/24);
    end
    if qraw && mean(diff(cell2mat({l.time})))<6
      %d_date=datevec(l(1).time/86400);
      %d_date=minput(sprintf('Enter date for %s: ',dirs(j).name),d_date(1:3));
      [a,b]=fileparts(dirs(j).name); d_date=sscanf(b,'%4d_%2d_%2d_%2d-%2d*s')';
      for i=1:dirlen
        d=gettimestamp(fullfile(l(i).dir,l(i).file),d_date),
        l(i).time=86400*(d-8/24);
      end
    end
    list=[list;l];
  end
  if ~isempty(newer)
    d=find(cell2mat({list.time})>newer.time,d_date);
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

function d=gettimestamp(file,d_date)
fid=fopen(file,'r');
fseek(fid,32768*511*4+7*4,'bof');
t=fliplr([sum([fread(fid,4,'ubit4');fread(fid,1,'ubit3')].*[0.001;.01;.1;1;10]) ...
  sum([fread(fid, 1, 'ubit4') fread(fid, 1, 'ubit3')].*[1 10]) ...
  sum([fread(fid, 1, 'ubit4') fread(fid, 1, 'ubit2')].*[1 10])]);
if t(1)>23 || t(2)>59 || t(3)>59.9999
  error(sprintf('Cannot get the time from %s, %d %d %g',file,t))
end
d=datenum([d_date(1:3) t]);
