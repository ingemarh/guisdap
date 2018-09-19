function [siteid,t1,t2,rt,intper,figs,extra,data_path,result_path,expver]=save_setup(file)
global b a_gfd

if isempty(b) | ~ishandle(b(1))
 name_expr=a_gfd.name_expr; expver=a_gfd.expver; siteid=a_gfd.siteid;
 data_path=a_gfd.data_path; result_path=a_gfd.result_path; intper=a_gfd.intper;
 t1=a_gfd.t1; t2=a_gfd.t2; rt=a_gfd.rt; figs=a_gfd.figs; extra=a_gfd.extra;
 path_exps=a_gfd.path_exps;
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
 expver=get(b(11),'value')-1;
 while size(extra,2)>1 & all(extra(:,end)==32), extra(:,end)=[]; end
 for i=size(extra,1):-1:1
  if all(extra(i,:)==32), extra(i,:)=[]; end
 end
 if isempty(extra)
  extra=' ';
 end
 a_gfd.name_expr=name_expr; a_gfd.expver=expver; a_gfd.siteid=siteid;
 a_gfd.data_path=data_path; a_gfd.result_path=result_path; a_gfd.intper=intper;
 a_gfd.t1=t1; a_gfd.t2=t2; a_gfd.rt=rt; a_gfd.figs=figs; a_gfd.extra=extra;
 a_gfd.path_exps=path_exps;
end

if nargin>0
 [i,i,i]=fileparts(file);
 if strcmp(i,'.m')
  save_m(file,name_expr,expver,siteid,data_path,result_path,t1,t2,rt,intper,path_exps,figs,extra)
 else
  save_noglobal(file,name_expr,expver,siteid,data_path,result_path,t1,t2,rt,intper,path_exps,figs,extra)
 end
end
