function [intper,t1,t2,siteid,expver]=auto_parse(warn)
global path_exps name_expr name_site data_path b owner local
if nargin<1, warn=1; end
sites='KSTVLLLL';
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
  d=dir(data_path); td=zeros(size(d));
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
if ~isempty(b) & ishandle(b(1))
 set_b=1;
else
 set_b=0;
end

[dum,expid,ext]=fileparts(data_path);
if isempty(expid), [dum,expid,ext]=fileparts(dum); end
expid=[expid ext];
[msg,pulse,scan,comment,owner,antenna]=expparts(expid);
if ~isempty(msg)
 if warn
  disp(['Unable to parse directory name: ' msg])
 end
else
 nameexpr=pulse(find(pulse~='_' & pulse~='-'));
 while ~exist(fullfile(path_exps,nameexpr))
  nameexpr=nameexpr(1:end-1);
 end
 if isempty(nameexpr)
  if warn, disp('Unable to set the Dsp experiment, please specify'), end
 else
  name_expr=nameexpr;
  if set_b, set(b(1),'string',name_expr), end
  expver=str2num(strtok(comment,'.'));
  if isempty(expver) 
   if warn, disp('Unable to set the experiment version, please specify'), end
   expver=1;
  else
   if set_b, set(b(11),'value',expver+1), end
  end
 end
 if isempty(antenna)
  antenna='   '; dum=[];
 else
  dum=strmatch(antenna(1:3),{'kir' 'sod' 'uhf' 'vhf' '32m' '42m' '32p' 'esr'});
 end
 if isempty(dum)
  if warn, disp('Unable to set the site, please specify'), end
 else
  name_site=sites(dum);
  siteid=strfind(sites(1:5),name_site);
  if set_b, set(b(2),'value',siteid), end
 end
 if isempty(scan)
 elseif strmatch(scan,{'cp2' 'cp3' 'cp4' 'lowelnorth2' 'fix2'},'exact')
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
  disp('Unable to set the integration time, please specify')
 elseif set_b
  set(b(8),'string',num2str(intper))
 end
end
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
  t1=[dt(1:2) 1 0 0 0];
  t2=datevec(datenum(t1+[0 1 0 0 0 0])-1)+[0 0 0 24 0 0];
 end
 if set_b
  set(b(4),'string',sprintf('%04d %02d %02d  %02d %02d %02d',t1))
  set(b(5),'string',sprintf('%04d %02d %02d  %02d %02d %02d',t2))
 end
end 
