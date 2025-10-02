function [intper,t1,t2,siteid,expver]=auto_parse(warn)
global path_exps name_expr name_site data_path b owner local
if nargin<1, warn=1; end
sites='KSTVLLLLQ3WD';
antennas={'kir' 'sod' 'uhf' 'vhf' '32m' '42m' '32p' 'esr' 'quj' 'san' 'wen' 'dan'};
t1=[]; t2=[]; intper=[]; siteid=[]; expver=1;
if warn==2
 o=uigetdir(data_path);
 if o
  data_path=o;
  set(b(3),'string',o)
 else
  return
 end
elseif warn==0
 name_expr='Dsp expr'; intper=60;
 t1=clock; t1=[t1(1) 1 1 0 0 0]; t2=[t1(1) 12 31 24 0 0];
 siteid=findstr(sites,local.site);
 if isempty(siteid)
  siteid=1;
 else
  siteid=siteid(1);
  d=dir(data_path), td=zeros(size(d));
  if isempty(d)
   return
  else
   for n=1:length(d)
    if isempty(expparts(d(n).name))
     td(n)=datenum(d(n).date);
    end
   end
   if any(td)
    [n,n]=max(td); data_path=fullfile(data_path,d(n).name);
    td=datevec(td(n)); t1(2)=td(2); t2(2)=td(2);
   else
    return
   end
  end
 end
end
if ~warn
 wstate=warning('query','GUISDAP:parse');
 warning('off','GUISDAP:parse');
end
if ~isempty(b) && ishandle(b(1))
 set_b=1;
else
 set_b=0;
end

[dum,expid,ext]=fileparts(data_path);
if isempty(expid), [dum,expid,ext]=fileparts(dum); end
expid=[expid ext];
[msg,pulse,scan,comment,owner,antenna]=expparts(expid);
if ~isempty(msg) & strfind(msg,'Illegal')
 %try for integrated files
 s=strsplit(expid,'_');
 if length(s)==3
  try
   t1=datenum(s{1}); t2=t1+2;
   if set_b
    set(b(4),'string',datestr(t1,'yyyy mm dd  HH MM SS'))
    set(b(5),'string',datestr(t2,'yyyy mm dd  HH MM SS'))
   end
   t1=datevec(t1); t2=datevec(t2);
   de=dir(path_exps);
   try
    pulse=validatestring(s{2}(1:end-1),{de.name});
    comment=s{2}(end);
    antenna=antennas{strfind(sites,s{3}(end))};
    scan=s{3}(1); intper=str2num(scan);
   catch,end
  catch,end
 end
end
if ~isempty(msg) & strfind(msg,'missing')
%try for hdf5 files, use the first found
 files=dir(fullfile(data_path,'EISCAT*.hdf5'));
 nf=length(files);
 if ~nf
  %try for syisr hdf5 files, use the first found
  files=dir(fullfile(data_path,'*0.h5'));
  nf=length(files);
  if ~nf
   %try for syisr bin files, use the first found
   files=dir(fullfile(data_path,'*_*_*_*_*_*.dat*-*-*'));
   nf=length(files);
   if ~nf
    warning('GUISDAP:parse',['Unable to parse directory name: ' msg])
   else
    s=syisr_bin('guess',fullfile(data_path,files(1).name))
    t1=timeconv(fix(s.unx),'unx2mat'); t2=t1+2;
    antenna=antennas{9+strfind('SWD',s.site)};
    if contains(s.mode,'Scan'), scan='cp3'; end
    de=dir(path_exps);
    try
     pulse=validatestring(s.exp,{de.name});
    catch,end
    if set_b
     set(b(4),'string',datestr(t1,'yyyy mm dd  HH MM SS'))
     set(b(5),'string',datestr(t2,'yyyy mm dd  HH MM SS'))
    end
    t1=datevec(t1); t2=datevec(t2);
   end
  else
   [s,n]=textscan(files(1).name,'%016.0f%c%01d%c0.h5');
   if n==23
    t1=timeconv(fix(s{1}*1e-6/3600)*3600,'unx2mat'); t2=t1+2;
    antenna=antennas{9+strfind('SWD',s{4})};
    h=h5read(fullfile(data_path,files(1).name),'/head',1,1);
    if h.pw==h.bw
     syexpr=sprintf('sy%d',h.bw);
    else
     syexpr=sprintf('sy%dx%d',h.pw/h.bw,h.bw);
    end
    de=dir(path_exps);
    try
     pulse=validatestring(syexpr,{de.name});
    catch,end
    if set_b
     set(b(4),'string',datestr(t1,'yyyy mm dd  HH MM SS'))
     set(b(5),'string',datestr(t2,'yyyy mm dd  HH MM SS'))
    end
    t1=datevec(t1); t2=datevec(t2);
   end
  end
 else
  s=strsplit(files(1).name,'_');
  expid=[strjoin(s(2:end-4),'_') '_' char(s(end-3))];
  [msg,pulse,scan,comment,owner,antenna]=expparts(expid);
  t1=datenum(sprintf('%s',strjoin(s(end-(2:-1:1)))),'yyyymmdd HHMMSS');
  t2=fix(t1)+1;
  if set_b
   set(b(4),'string',datestr(t1,'yyyy mm dd  HH MM SS'))
   set(b(5),'string',datestr(t2,'yyyy mm dd  HH MM SS'))
  end
  t1=datevec(t1); t2=datevec(t2);
 end
end
 nameexpr=pulse(find(pulse~='_' & pulse~='-'));
 if ~isempty(nameexpr)
  de=dir(path_exps);
  try
   nameexpr=validatestring(nameexpr,{de.name});
  catch,end
 end
 if isempty(nameexpr)
  warning('GUISDAP:parse','Unable to set the Dsp experiment, please specify')
 else
  name_expr=nameexpr;
  if set_b, set(b(1),'string',name_expr), end
  if ~isempty(comment), expver=str2num(strtok(comment,'.')); end
  if isempty(comment) | isempty(expver) 
   warning('GUISDAP:parse','Unable to set the experiment version, please specify')
   expver=1;
  else
   if set_b, set(b(11),'value',expver+1), end
  end
 end
 if isempty(antenna)
  antenna='   '; dum=[];
 else
  dum=strmatch(antenna(1:3),antennas);
 end
 if isempty(dum)
  warning('GUISDAP:parse','Unable to set the site, please specify')
 else
  name_site=sites(dum);
  siteid=strfind(sites,name_site);
  if set_b, set(b(2),'value',siteid(1)), end
 end
 if isempty(scan)
 elseif strmatch(scan,{'cp2' 'cp3' 'cp4' 'lowelnorth2' 'fix2' 'ip2' 'ip3'},'exact')
  intper=0;
 elseif [strfind(scan,'42') strmatch(scan,{'cp6' 'cp1' 'cp7' 'fixed' 'bore' 'lowel' 'zenith'},'exact')]
  intper=60;
 elseif strmatch(scan,{'lowelnorth1' 'lowelsouth1' 'lowelsouth2' 'lowelsouth3'},'exact')
  intper=120;
 end
 if isempty(intper)
  if strcmp(antenna(2:3),'2m')
   intper=0;
  elseif strcmp(antenna(1:3),'vhf')
   intper=60;
  end
 end
 if isempty(intper)
  warning('GUISDAP:parse','Unable to set the integration time, please specify')
 elseif set_b
  set(b(8),'string',num2str(intper))
 end
%end
d=dir(fullfile(data_path,'*_*'));
if ~isempty(d)
 dtmax=0;
 for i=1:length(d)
  dt=sscanf(d(i).name,'%04d%02d%02d_%02d');
  if length(dt)==4
   dt=datenum([dt' 0 0]);
   if dt>dtmax, dtmax=dt; end
  end
 end
 if dtmax
  dt=datevec(dtmax);
  t1=[dt(1:3) 0 0 0];
  %t2=datevec(datenum(t1+[0 1 0 0 0 0])-1)+[0 0 0 24 0 0];
  t2=t1; t2(4)=24;
 end
 if set_b & ~isempty(t1)
  set(b(4),'string',sprintf('%04d %02d %02d  %02d %02d %02d',t1))
  set(b(5),'string',sprintf('%04d %02d %02d  %02d %02d %02d',t2))
 end
end 
if ~warn
 warning(wstate)
end
