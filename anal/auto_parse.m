function [intper,t1,t2]=auto_parse(warn)
global path_exps name_expr name_site data_path b owner
if nargin<1, warn=1; end
if ~isempty(b) & ishandle(b(1))
 set_b=1;
else
 set_b=0;
end
intper=-1; t1=0; t2=0; sites='KSTVLLL';
[dum,expid]=fileparts(data_path);
if isempty(expid), [dum,expid]=fileparts(dum); end
expid=data_path(length(dum)+2:end);
[msg,pulse,scan,comment,owner,antenna]=expparts(expid);
if ~isempty(msg)
 if warn, disp(['Unable to parse directory name: ' msg]), end
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
 end
 dum=strmatch(antenna(1:3),{'kir' 'sod' 'uhf' 'vhf' '32m' '42m' 'esr'});
 if isempty(dum)
  disp('Unable to set the site, please specify')
 else
  name_site=sites(dum);
  if set_b, set(b(2),'value',strfind(sites(1:5),name_site)), end
 end
 if isempty(scan)
  intper=-1;
 elseif strcmp(scan,'cp2') | strcmp(scan,'cp3')
  intper=0;
 elseif strcmp(scan,'cp6') | strcmp(scan,'cp1') | strcmp(scan,'cp7') ...
        strcmp(scan,'fixed')
  intper=60;
 elseif strcmp(scan,'lowelnorth1')
  intper=120;
 elseif strcmp(antenna(2:3),'2m')
  intper=0;
 elseif strcmp(antenna(1:3),'vhf')
  intper=60;
 else
  intper=-1;
 end
 if intper<0
  if warn, disp('Unable to set the integration time, please specify'), end
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
