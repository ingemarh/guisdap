function [list,msg]=getfilelist(dirpath,newer)

% [list,msg]= getfilelist(dirpath,newer)

global a_realtime a_lpf name_site d_filename local

list=[]; msg=''; dirlist=[]; syisr=0;
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
        site=name_site; if name_site=='3', site='S'; end
        if a_lpf(1).do
	  listfile=fullfile(dp,j.name);
          dirlist=dir(fullfile(listfile,['*' site '0.h5']));
          %%dirlist=dir(fullfile(dp,j.name,['*.h5']));
	  if isempty(dirlist)
            dirlist=dir(fullfile(listfile,[site '*_*_*_*_*_*_*.dat*-*-*']));
	    if isempty(dirlist)
              dirlist=dir(fullfile(listfile,['*_*_*_*_*_*.dat*-*-*'])); %syisr1
	    end
	    if ~isempty(dirlist)
	      if exist(fullfile(listfile,['filelist' site '.mat']))
		folde=listfile;
	        listfile=fullfile(listfile,['filelist' site '.mat']);
	      else
	        listfile=fullfile(local.gup,'mydata','filelist',[strrep(listfile,filesep,'__') site '.mat']);
	      end
	      if exist(listfile)
		disp(sprintf('Using existing listfile: %s',listfile))
	      else
                listfile=syisr_bin('filelist',fullfile(dp,j.name),site),
	      end
	      load(listfile), dirlist=[]; if exist('folde','var'), folder=folde; end
	      if size(flist,2)>7
		st=struct('fno',0,'tai',0,'code',0,'azel',0,'hdx',0,'nd',0,'t408',0);
	      else
		st=struct('fno',0,'tai',0,'code',0,'azel',0,'hdx',0,'nd',0);
	      end
	      list=repmat(st,[size(flist,1) 1]);
              flist(:,2)=timeconv(flist(:,2),'unx2tai');
	      ll=num2cell(flist(:,[1:3 6:end]));
              [list.fno]=ll{:,1};
              [list.tai]=ll{:,2};
              [list.code]=ll{:,3};
              azel=num2cell(complex(flist(:,4),flist(:,5)));
              [list.azel]=azel{:};
              [list.hdx]=ll{:,4};
              [list.nd]=ll{:,5};
	      if size(flist,2)>7
                [list.t408]=ll{:,6};
	      end
	      d_filename=cellstr([ones(size(fname,1),1)*[folder filesep] fname]);
	    end
            syisr=2;
          end
          %[~,d]=sortrows(cell2table({dirlist.name}'));
          %dirlist=dirlist(d);
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
      end
      for f=dirlist'
	fno=fno+1;
        h5file=fullfile(j.name,f.name);
        if syisr
          if a_lpf(1).do
	    if syisr==1
              head=h5read(h5file,'/head');
              good=find(head.nd>=head.nw);
              tai=num2cell(timeconv(double(head.dt(good))*1e-6,'unx2tai')); %beijing unix time!
              code=num2cell(head.bc(good)+head.lc(good));
              azel=num2cell(complex(head.az(good),head.el(good)));
              hdx=num2cell(good);
              s=length(tai);
              l=repmat(struct('fname',h5file,'tai',0,'code',0,'azel',0,'hdx',0),[s 1]);
              [l.tai]=tai{:};
              [l.code]=code{:};
              [l.azel]=azel{:};
              [l.hdx]=hdx{:};
              list=[list;l];
	    else
	      d_filename{fno}=h5file;
	      l=syisr_bin('flist',h5file,0,fno);
              list=[list;l];
	    end
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
          t=h5read(h5file,'/Data/EndTime');
          it=h5read(h5file,'/Data/IntegrationTime');
	  fmt='yyyy-MM-dd''T''HH:mm:ss.SS''Z''';
	  tai=num2cell(timeconv(datenum(datetime(t,'InputFormat',fmt)),'mat2tai')-it/2);
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
    [~,d]=sort(cell2mat({list.file})); list=list(d);
    global maxlend
    if ~isempty(maxlend) & length(d)>maxlend, list=list(1:maxlend); end
  elseif isfield(list,'tai')
    if syisr<2
      [~,d]=sort(cell2mat({list.tai})); list=list(d);
    end
    global maxlend
    if ~isempty(maxlend) & length(d)>maxlend, list=list(1:maxlend); end
  end
elseif isempty(newer)
  msg=[dirpath ' - No valid mat/hdf5 files'];
end
