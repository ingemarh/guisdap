startup
using_x=prod(get(0,'ScreenSize'))-1;
%default values
name_expr='Dsp expr';
t1=[2002 12 01 00 00 00];
t2=[2002 12 01 23 59 59];
rt=0; siteid=1; intper=0;
figs=zeros(1,5);
extra='display_results=0;';
if exist([path_tmp '.gup'])
 load([path_tmp '.gup'],'-mat')
end
beg=sprintf('%04d %02d %02d  %02d %02d %02d',t1);
to=sprintf('%04d %02d %02d  %02d %02d %02d',t2);
sites='KSTVL';
if using_x & isempty(get(0,'UserData'))
global b
figure(5)
clf
set(5,'Name','GUISDAP for dummies','userdata',5,'Position',[40 170 310 400],'NumberTitle','off','defaulttextunit','pixel')
load(fullfile(path_GUP,'matfiles','logo')), plot(y,x,'.k'), axis image
set(get(gca,'child'),'markersize',10), set(gca,'visible','off')
pause(1), clf
x=100; yh=25; y=(9:-1:1)*yh+130; ty=y+yh/2; x1=60; x2=200;
bg=uicontrol('Style','pushbutton','string','GO','position',[0 0 40 30],'callback','set(bg,''visible'',''off'');try,go_on(b),catch,disp(lasterr),end','fontsize',14);
uicontrol('Style','pushbutton','string','Quit','position',[50 0 40 20],'callback','quit');
text(0,ty(1),'Dsp expr')
set(gca,'position',[0 0 1 1],'visible','off')
b(1)=uicontrol('Style','pushbutton','string',name_expr,'position',[x y(1) x1 yh],'value',0,'callback','o=uigetdir(path_exps);if o,[path_exps,name_expr]=fileparts(o);set(b(1),''string'',name_expr),end');
text(0,ty(2),'Site')
b(2)=uicontrol('Style','popupmenu','string',sites','position',[x y(2) x1 yh],'value',siteid);
text(0,ty(3),'Data path')
b(3)=uicontrol('Style','pushbutton','string',data_path,'position',[x y(3) x2 yh],'value',0,'callback','o=uigetdir(data_path);if o,data_path=o;set(b(3),''string'',o),end');
text(0,ty(4),'Start time')
b(4)=uicontrol('Style','edit','string',beg,'position',[x y(4) x2 yh]);
text(0,ty(5),'Stop time')
b(5)=uicontrol('Style','edit','string',to,'position',[x y(5) x2 yh]);
text(0,ty(6),'Result path')
b(6)=uicontrol('Style','pushbutton','string',result_path,'position',[x y(6) x2 yh],'value',0,'callback',...
 '[f,p]=uiputfile(''*'',''Save data in'',result_path);if p,result_path=fullfile(p,f,filesep);if ~strcmp(f,''AUTO'') & ~exist(result_path),mkdir(p,f);end,else,result_path=path_tmp;end,set(b(6),''string'',result_path)');
text(0,ty(7),'Real time')
b(7)=uicontrol('Style','togglebutton','string','RT','position',[x y(7) x1 yh],'value',rt);
text(0,ty(8),'Integration time')
b(8)=uicontrol('Style','edit','string',num2str(intper),'position',[x y(8) x1 yh],'tooltipstring','0=antenna move');
text(0,ty(9),'Disp figures')
b(9)=uicontrol('Style','edit','string',num2str(figs),'position',[x y(9) x2 yh],'tooltipstring','datadump powerprofile fits parameters vizu');
text(0,100,'Special')
b(10)=uicontrol('Style','edit','string',extra,'position',[x 50 x2 100],'max',100,'HorizontalAlignment','left','tooltipstring','Matlab commands!');
else
 path_exps=minput('Path exps',path_exps,'s');
 name_expr=minput('Dsp exp',name_expr,'s');
 siteid=findstr(minput('Site',sites(siteid),'s'),sites);
 data_path=minput('Data path',data_path,'s');
 t1=minput('Start time',t1);
 t2=minput('End time',t2);
 result_path=minput('Result path',result_path,'s');
 rt=minput('Realtime (0/1)',rt);
 intper=minput('Integration time',intper);
 figs=minput('Display figures',figs);
 extra=row([extra ones(size(extra(:,end)))*'#']');
 extra=minput('Extra commands (''#''=line break)',extra(1:end-1),'s');
 ex=extra; [extra,ex]=strtok(ex,'#');
 while ~isempty(ex)
  [ext,ex]=strtok(ex,'#');
  extra=char(extra,ext);
 end
 save([path_tmp '.gup'],'name_expr','siteid','data_path','result_path','t1','t2','rt','intper','path_exps','figs','extra','-mat')
 go_on
end
