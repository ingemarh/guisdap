function go_on(bg)
global GUP_ver path_GUP path_exps path_tmp name_expr name_site data_path result_path b
go_die=0;

if nargin<1 | ~ishandle(bg)
 load([path_tmp '.gup'],'-mat')
 if nargin>0, go_die=1; end
else
 while strcmp(get(bg,'visible'),'on')
  pause(1)
  if ~ishandle(bg), return, end
 end
 siteid=get(b(2),'value');
 t1=str2num(get(b(4),'string'));
 t2=str2num(get(b(5),'string'));
 rt=get(b(7),'value');
 intper=str2num(get(b(8),'string'));
 figs=str2num(get(b(9),'string'));
 extra=get(b(10),'string');
 save([path_tmp '.gup'],'name_expr','siteid','data_path','result_path','t1','t2','rt','intper','path_exps','figs','extra','-mat')
end
if length(findstr(result_path,'AUTO'))>1
 error('"AUTO" result path do not work, please specify')
end
analysis_start=t1; analysis_end=t2; analysis_integr=intper; %for guisdap int
analysis_txlimit=100e3; analysis_realtime=rt;

sites='KSTVL'; name_site=sites(siteid);
recurse=sprintf('%04d????_??',t1(1));
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
