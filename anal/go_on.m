function go_on(bg)
global path_GUP path_exps path_tmp name_expr name_site data_path result_path b local
go_die=0;
if nargin<1, bg=NaN; end

if ~ishandle(bg)
 if isstr(bg) & exist(bg)
  i=which(bg); if ~isempty(i), bg=i; end
  [i,i,i]=fileparts(bg);
  if strcmp(i,'.m')
   run(bg);
  else
   load(bg,'-mat')
  end
 else
  bg=NaN; load([path_tmp '.gup'],'-mat')
 end
 if nargin>0, go_die=1; end
else
 while strcmp(get(bg,'visible'),'on')
  pause(1)
  if ~ishandle(bg), return, end
 end
 [siteid,t1,t2,rt,intper,figs,extra,data_path,result_path]=save_setup;
end
if ~isnan(bg)
 save([path_tmp '.gup'],'name_expr','siteid','data_path','result_path','t1','t2','rt','intper','path_exps','figs','extra','-mat')
end
sites='KSTVLLL'; name_site=sites(siteid);
if length(strfind(result_path,'AUTO'))>1
 error('"AUTO" result path do not work, please specify')
elseif intper==-1 | strcmp(name_expr,'AUTO') | siteid==6
 [dum,expid]=fileparts(data_path);
 if isempty(expid), [dum,expid]=fileparts(dum); end
 expid=data_path(length(dum)+2:end);
 global owner
 [msg,pulse,scan,comment,owner,antenna]=expparts(expid);
 if ~isempty(msg)
  error(['Unable to parse directory name: ' msg])
 end
 if strcmp(name_expr,'AUTO')
  name_expr=pulse(find(pulse~='_' & pulse~='-'));
  while ~exist(fullfile(path_exps,name_expr))
   name_expr=name_expr(1:end-1);
  end
  if isempty(name_expr)
   error('Unable to set the Dsp experiment, please specify')
  else
   fprintf('Dsp expriment set to %s\n',name_expr)
  end
  dum=strmatch(antenna(1:3),{'kir' 'sod' 'uhf' 'vhf' '32m' '42m' 'esr'});
  if isempty(dum)
   error('Unable to set the site, please specify')
  else
   name_site=sites(dum);
   fprintf('Site set to %s\n',name_site)
  end
 end
 if intper==-1
  if isempty(scan)
   intper=60; msg='not known';
  elseif strcmp(scan,'cp2') | strcmp(scan,'cp3')
   intper=0; msg='scanning';
  elseif strcmp(scan,'cp6') | strcmp(scan,'cp1') | strcmp(scan,'cp7') ...
         strcmp(scan,'fixed')
   intper=60; msg='stationary';
  elseif strcmp(scan,'lowelnorth1')
   intper=120; msg='stationary';
  elseif strcmp(antenna(2:3),'2m')
   intper=0; msg='scanning';
  elseif strcmp(antenna(1:3),'vhf')
   intper=60; msg='stationary';
  else
   intper=60; msg='not known';
  end
  fprintf('Antenna mode %s %s: Setting integration time to %d\n',scan,msg,intper)
 end
 if datenum(clock)<datenum(t2) & rt==0 & length(local.site)==1
  rt=1;
  if ishandle(bg), set(b(7),'value',1), end
  fprintf('Stop time is in future: Setting RT mode\n')
  if figs(5)==1, figs(5)=2; end
 elseif (datenum(clock)>datenum(t2) | length(local.site)~=1) & rt==1
  rt=0;
  if ishandle(bg), set(b(7),'value',0), end
  fprintf('Stop time has passed: Unsetting RT mode\n')
 end
 if ishandle(bg)
  set(b(1),'string',name_expr)
  set(b(2),'value',strfind(sites(1:5),name_site))
  set(b(7),'value',rt)
  set(b(8),'string',num2str(intper))
  set(b(9),'string',num2str(figs))
  set(b([1:6 8:10]),'enable','inactive')
  set(bg,'visible','on')
  disp('***Hit GO to continue if these values are OK***')
  while strcmp(get(bg,'visible'),'on')
   pause(1)
   if ~ishandle(bg), return, end
  end
 else
  disp('***Hit ctrl-c if these values are wrong, analysis soon continues***')
  pause(5*rt+5)
 end
end
analysis_start=t1; analysis_end=t2; analysis_integr=intper; %for guisdap int
analysis_txlimit=100e3; analysis_realtime=rt;

if isunix
 recurse=sprintf('%04d????_??',t1(1));
else
 recurse=sprintf('%04d****_**',t1(1));
end
i1=fix(t1/10); i2=fix(t2/10);
if t1(2)==t2(2)
 recurse(5:6)=sprintf('%02d',t1(2));
 if t1(3)==t2(3)
  recurse(7:8)=sprintf('%02d',t1(3));
  if t1(4)==t2(4)
   recurse(10:11)=sprintf('%02d',t1(4));
  elseif i1(4)==i2(4)
   recurse(10)=sprintf('%d',i1(4));
  end
 elseif i1(3)==i2(3)
  recurse(7)=sprintf('%d',i1(3));
 end
elseif i1(2)==i2(2)
 recurse(5)=sprintf('%d',i1(2));
end

read_anapar
display_figures=figs;
analysis_control=[1 .01 100 1];
for i=1:size(extra,1),eval(extra(i,:));end
if rt & isunix
 [k,now]=unix('date -u "+%Y %m %d %H %M %S"');
 now=datenum(analysis_start)-datenum(str2num(now));
 if now>0
  now=now*86400+600; %wait extra 10 mins 
  fprintf('Waiting %.1f minutes for exp start...\n',now/60), pause(now)
 end
end
an_start
if go_die, quit, end
