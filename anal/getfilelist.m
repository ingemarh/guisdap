function [list,msg]=getfilelist(dirpath,newer)

% [list,msg]= getfilelist(dirpath,newer)

global a_realtime a_year a_lpf

list=[]; msg=''; dirlist=[];
if nargin<2
 newer=[];
end

if isempty(dirpath)
  msg='Empty directory path';
elseif isunix & a_realtime | strfind(dirpath,'?')
  i=' ';
  if ~isempty(newer)
    i=sprintf(' -newer %s ',newer.fname);
  end
  template=[row(col('\[0-9]')*ones(1,8)) '.mat\*'];
  dirpath(strfind(dirpath,'\'))=[]; % remove escapes
  cmd=sprintf('find %s -name %s%s -print 2>/dev/null',dirpath(1:end-1),template,i);
  [status,d]=gupsystem(cmd);
  if status
    msg=['Error listing mat files in ' dirpath ' ' cmd];
  elseif length(d)
    try
      d=textscan(d,'%s');
      dirlen=length(d{1});
      list=repmat(struct('fname','','file',0),[dirlen 1]);
      [list.fname]=d{1}{:};
      for i=1:dirlen
        [~,file]=fileparts(list(i).fname);
        list(i).file=sscanf(file,'%f');
      end
    catch, disp(lasterr)
    end
  end
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
    l=repmat(struct('fname','','file',0),[dirlen 1]);
    for i=dirlen:-1:1
      l(i).file=cell2mat(textscan(dirlist(i).name,'%f'));
      l(i).fname=fullfile(dp,dirs(j).name,dirlist(i).name);
      if length(l(i).file)~=1, l(i)=[]; end
    end
    list=[list;l];
  end
  if isempty(list) % Look for hdf5 files
    if isempty(dirs)
      dp=[];
      dirs=struct('name',fileparts(dirpath));
    end
    list=[]; fno=0;
    for j=dirs'
      dirlist=dir(fullfile(dp,j.name,'*.hdf5'));
      if isempty(dirlist) & ~isempty(a_lpf)
        syisr=1;
        if a_lpf(1).do
          dirlist=dir(fullfile(dp,j.name,'*.h5'));
          [~,d]=sortrows(cell2table({dirlist.name}'));
          dirlist=dirlist(d);
        else
          dirlist=dir(fullfile(dp,j.name,'*F1T.h5'));
          dirlen=length(dirlist);
          l=repmat(struct('fname','','tai',0),[dirlen 1]);
          fname=cell2mat({dirlist.name}');
          tai=timeconv(str2num(fname(:,1:16))*1e-6-8*3600,'unx2tai'); % bei->tai
          for i=1:dirlen
            l(i).fname=fullfile(dp,j.name,dirlist(i).name);
            l(i).tai=tai(i);
          end
          list=[list;l];
        end
      else
        syisr=0;
      end
      for f=dirlist'
        h5file=fullfile(j.name,f.name);
        if syisr
          if a_lpf(1).do
            tx.time=h5read(h5file,'/Tx/TransTime');
            tx.code=h5read(h5file,'/Tx/PulseCode');
            s=size(tx.time);
            if isempty(a_year), %hui hui
              tx.stamp=datevec(dir(h5file).date);
            else
              tx.stamp=a_year;
            end
            td=datenum(tx.stamp(1),1,1)-datenum(1970,1,1);
            tx.time=reshape(timeconv(td*86400+tx.time(:)-8*3600,'unx2tai'),s); %bei->tai
            fno=fno+1;
            list.fname(fno)={h5file};
            if fno==1, list.tai=[]; list.code=[]; list.profs=[]; end
            list.tai=[list.tai tx.time];
            list.code=[list.code tx.code];
            list.profs=[list.profs;s(2)];
	  %else
          %  syhead_info = h5read(h5file, '/head');
          %  sytime = double(syhead_info.dt) * 1e-6;      % us, added by wyh
          %  sytai_time = timeconv(sytime, 'unx2tai');    % ut is bei, added by wyh
          %  fno = fno + 1;
          %  list(fno, 1).fname = h5file;                  % eiscat is array of struct, added by wyh
          %  if fno == 1, list.tai = []; end
          %  list(fno, 1).tai = sytai_time;
          end
        else
          t=h5read(h5file,'/Data/Time');
	  fmt='yyyy-MM-dd''T''HH:mm:ss.SS''Z''';
	  tai=num2cell(timeconv(mean(datenum(datetime(t','InputFormat',fmt)),2),'mat2tai'));
          s=length(tai); idx=num2cell(1:s)';
          l=repmat(struct('fname',h5file,'tai',0,'idx',0),[s 1]);
          [l.tai]=tai{:};
          [l.idx]=idx{:};
          list=[list;l];
        end
      end
    end
  end
  if ~isempty(newer)
    d=find(cell2mat({list.file})>newer.file);
    list=list(d);
  end
end
if ~isempty(list)
  if isfield(list,'file')
    [dum,d]=sort(cell2mat({list.file})); list=list(d);
    global maxlend
    if ~isempty(maxlend) & length(d)>maxlend, list=list(1:maxlend); end
  elseif isfield(list,'tai')
    [dum,d]=sort(cell2mat({list.tai})); list=list(d);
    global maxlend
    if ~isempty(maxlend) & length(d)>maxlend, list=list(1:maxlend); end
  end
elseif isempty(newer)
  msg=[dirpath ' - No valid mat/hdf5 files'];
end
