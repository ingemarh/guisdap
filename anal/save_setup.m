function [siteid,t1,t2,rt,intper,figs,extra,data_path,result_path]=save_setup(file)
global path_exps path_tmp name_expr b

siteid=get(b(2),'value');
data_path=get(b(3),'string');
t1=str2num(get(b(4),'string'));
t2=str2num(get(b(5),'string'));
result_path=get(b(6),'string');
rt=get(b(7),'value');
intper=str2num(get(b(8),'string'));
figs=str2num(get(b(9),'string'));
extra=get(b(10),'string');
if size(extra,2)>10, while all(extra(:,end)==32), extra(:,end)=[];
end, end

if nargin>0
 [i,i,i]=fileparts(file);
 if strcmp(i,'.m')
  save_m(file,name_expr,siteid,data_path,result_path,t1,t2,rt,intper,path_exps,figs,extra)
 else
  save(file,'name_expr','siteid','data_path','result_path','t1','t2','rt','intper','path_exps','figs','extra','-mat')
 end
end
