startup
using_x=prod(get(0,'ScreenSize'))-1;
sites='KSTVL'; gfdfile=[path_tmp '.gup'];
%default values
if exist(gfdfile)
 load(gfdfile,'-mat')
else
 rt=0;
 [intper,t1,t2,siteid]=auto_parse(0);
 if siteid<3
  figs=[0 0 1 0 1];
  extra='%a_Offsetppd=8;';
 else
  figs=[0 1 0 0 1];
  extra='%Magic_const=1.1;';
 end
 result_path=fullfile(result_path,'AUTO');
end
beg=sprintf('%04d %02d %02d  %02d %02d %02d',t1);
to=sprintf('%04d %02d %02d  %02d %02d %02d',t2);
if using_x & isempty(get(0,'UserData'))
global b
figure(5)
clf
set(5,'Name','GUISDAP for dummies','userdata',5,'Position',[40 170 350 390],'NumberTitle','off','defaulttextunit','pixel','toolbar','auto')
load(fullfile(path_GUP,'matfiles','logo')), plot(y,x,'.k'), axis image
set(get(gca,'child'),'markersize',10), set(gca,'visible','off')
pause(1), clf
x=100; yh=25; y=(9:-1:1)*yh+130; ty=y+yh/2; x1=60; x2=240;
bg=uicontrol('Style','pushbutton','string','GO','position',[0 0 40 30],'callback','set([bg br],''visible'',''off'')','fontsize',14);
uicontrol('Style','pushbutton','string','Quit','position',[50 0 40 20],'callback','quit');
uicontrol('Style','pushbutton','string','Save','position',[50 20 40 20],'callback','o=uiputfile;if o,save_setup(o);end','tooltipstring','Save setup in file');
br=uicontrol('Style','pushbutton','string','Reset','position',[50 40 40 20],'callback','clf,if exist(gfdfile),delete(gfdfile),end,pause(1),analyse','tooltipstring','Reset to default');
set(gca,'position',[0 0 1 1],'visible','off')
text(0,ty(4),'Dsp expr')
b(1)=uicontrol('Style','pushbutton','string',name_expr,'position',[x y(4) x1 yh],'value',0,'callback','o=uigetdir(path_exps);if o,[path_exps,name_expr]=fileparts(o);set(b(1),''string'',name_expr),end');
text(0,ty(5),'Site')
b(2)=uicontrol('Style','popupmenu','string',sites','position',[x y(5) x1 yh],'value',siteid);
text(0,ty(1),'Data path')
b(3)=uicontrol('Style','pushbutton','string',data_path,'position',[x y(1) x2 yh],'value',0,'callback','auto_parse(2);');
text(0,ty(2),'Start time')
b(4)=uicontrol('Style','edit','string',beg,'position',[x y(2) x2 yh]);
text(0,ty(3),'Stop time')
b(5)=uicontrol('Style','edit','string',to,'position',[x y(3) x2 yh]);
text(0,ty(6),'Result path')
b(6)=uicontrol('Style','pushbutton','string',result_path,'position',[x y(6) x2 yh],'value',0,'callback',...
 '[f,p]=uiputfile(''*'',''Save data in'',result_path);if p,result_path=fullfile(p,f,filesep);if ~strcmp(f,''AUTO'') & ~exist(result_path),mkdir(p,f);end,else,result_path=path_tmp;end,set(b(6),''string'',result_path)');
text(0,ty(7),'Real time')
b(7)=uicontrol('Style','togglebutton','string','RT','position',[x y(7) x1 yh],'value',rt);
text(0,ty(8),'Integration time')
b(8)=uicontrol('Style','edit','string',num2str(intper),'position',[x y(8) x1 yh],'tooltipstring','0=antenna move;-1=automatic');
text(0,ty(9),'Disp figures')
b(9)=uicontrol('Style','edit','string',num2str(figs),'position',[x y(9) x2 yh],'tooltipstring','datadump powerprofile fits parameters vizu');
text(0,100,'Special')
b(10)=uicontrol('Style','edit','string',extra,'position',[x 30 x2 120],'max',100,'HorizontalAlignment','left','tooltipstring','Matlab commands!');
 go_on(bg)
else
 dpath=minput('Data path',data_path,'s');
 if ~strcmp(dpath,data_path)
  [intp,tt1,tt2,sitid]=auto_parse;
  if tt1, t1=tt1; t2=tt2; end
  if ~isempty(intp), intper=intp; end
  if ~isempty(sitid), siteid=sitid; end
 end
 t1=minput('Start time',t1);
 t2=minput('End time',t2);
 path_exps=minput('Path exps',path_exps,'s');
 name_expr=minput('Dsp exp',name_expr,'s');
 siteid=strfind(sites,minput('Site',sites(siteid),'s'));
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
 save(gfdfile,'name_expr','siteid','data_path','result_path','t1','t2','rt','intper','path_exps','figs','extra','-mat')
 go_on
end
