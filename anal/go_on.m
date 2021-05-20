function go_on(bg)
global path_GUP path_exps path_tmp name_expr name_site data_path result_path b local a_gfd
go_die=0;
if nargin<1, bg=NaN; end

if ~ishandle(bg)
 if isstr(bg) & exist(bg)
  i=which(bg); if ~isempty(i), bg=i; end
  [i,i,i]=fileparts(bg);
  expver=1; %Backward compability
  if strcmp(i,'.m')
   run(bg);
  else
   if ~exist(bg), bg=[path_tmp '.gup']; end
   load(bg,'-mat')
  end
 end
 if ~isempty(a_gfd)
  name_expr=a_gfd.name_expr; expver=a_gfd.expver; siteid=a_gfd.siteid;
  data_path=a_gfd.data_path; result_path=a_gfd.result_path; intper=a_gfd.intper;
  t1=a_gfd.t1; t2=a_gfd.t2; rt=a_gfd.rt; figs=a_gfd.figs; extra=a_gfd.extra;
  path_exps=a_gfd.path_exps;
 else
  if ~exist('extra','var'), extra=' '; end
  a_gfd.name_expr=name_expr; a_gfd.expver=expver; a_gfd.siteid=siteid;
  a_gfd.data_path=data_path; a_gfd.result_path=result_path; a_gfd.intper=intper;
  a_gfd.t1=t1; a_gfd.t2=t2; a_gfd.rt=rt; a_gfd.figs=figs; a_gfd.extra=extra;
  a_gfd.path_exps=path_exps;
 end
 if nargin>0, go_die=1; end
else
 while strcmp(get(bg,'string'),'GO')
  pause(1)
  if ~ishandle(bg), return, end
 end
 [siteid,t1,t2,rt,intper,figs,extra,data_path,result_path,expver]=save_setup;
end
if ishandle(bg) | ~isnan(bg)
 save_noglobal([path_tmp '.gup'],name_expr,expver,siteid,data_path,result_path,t1,t2,rt,intper,path_exps,figs,extra)
end
sites='KSTVLLPQ'; name_site=sites(siteid);
if length(strfind(result_path,'AUTO'))>1
 error('"AUTO" result path do not work, please specify')
elseif datenum(clock)<datenum(t2) & rt==0 & length(local.site)==1
 rt=1;
 fprintf('Stop time is in future: Setting RT mode\n')
 if ishandle(bg), set(b(7),'value',1), end
 if figs(5)==1
  figs(5)=2;
  if ishandle(bg), set(b(9),'string',num2str(figs)), end
 end
elseif (datenum(clock)>datenum(t2) | length(local.site)~=1) & rt==1
 rt=0;
 if ishandle(bg), set(b(7),'value',0), end
 fprintf('Stop time has passed: Unsetting RT mode\n')
end
if ishandle(bg)
 set(b([1:6 8:10]),'enable','inactive')
end

analysis_start=t1; analysis_end=t2; analysis_integr=intper;
analysis_realtime=rt; analysis_rcprog=expver;

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
if rt & isunix
 [k,now]=unix('date -u "+%Y %m %d %H %M %S"');
 now=datenum(analysis_start)-datenum(str2num(now));
 if now>0
  now=now*86400+600; %wait extra 10 mins 
  fprintf('Waiting %.1f minutes for exp start...\n',now/60), pause(now)
 end
end
if go_die
 try
  for i=1:size(extra,1)
   ex=extra(i,:);
   while ~isempty(ex)
    [ext,ex]=strtok(ex,'#'); eval(ext)
   end
  end
  clear ext ex
  an_start
 catch
  disp(lasterr)
 end
 quit
else
 for i=1:size(extra,1),eval(extra(i,:));end
 an_start
end
