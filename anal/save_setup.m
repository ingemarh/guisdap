function [siteid,t1,t2,rt,intper,figs,extra,data_path,result_path,expver]=save_setup(file)
global path_tmp b

if isempty(b) | ~ishandle(b(1))
 load([path_tmp '.gup'],'-mat')
else
 global path_exps name_expr
 siteid=get(b(2),'value');
 data_path=get(b(3),'string');
 t1=str2num(get(b(4),'string'));
 t2=str2num(get(b(5),'string'));
 result_path=get(b(6),'string');
 rt=get(b(7),'value');
 intper=str2num(get(b(8),'string'));
 figs=str2num(get(b(9),'string'));
 extra=get(b(10),'string');
 expver=get(b(11),'value');
 if size(extra,2)>10
  while all(extra(:,end)==32), extra(:,end)=[]; end
 elseif isempty(extra)
  extra=' ';
 end
end

if nargin>0
 [i,i,i]=fileparts(file);
 if ~exist('extra','var'), extra=' '; end
 if strcmp(i,'.m')
  save_m(file,name_expr,expver,siteid,data_path,result_path,t1,t2,rt,intper,path_exps,figs,extra)
 else
  save_noglobal(file,name_expr,expver,siteid,data_path,result_path,t1,t2,rt,intper,path_exps,figs,extra)
 end
end
