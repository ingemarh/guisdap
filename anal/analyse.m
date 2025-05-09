function analyse(dpath,it1,it2,iname_expr,iexpver,iintper,ifigs,iextra)
% function analyse(data_path,t1,t2,name_expr,expver,intper,figs,extra)
if nargin>0
 gupclean=1;
end
start_GUP
%default values
rt=0;
[intper,t1,t2,siteid,expver]=auto_parse(0);
if siteid<3
 figs=[0 0 1 0 1];
 extra='%a_Offsetppd=8;';
else
 figs=[0 1 0 0 1];
 extra='%Magic_const=1.1;';
end
result_path=fullfile(result_path,'AUTO');
global gfdfile
gfdfile=[path_tmp '.gup'];
if exist(gfdfile)
 load(gfdfile,'-mat')
end
sites='KSTVLXPLQ3WD';
tfor='%04d %02d %02d  %02d %02d %02g';
beg=sprintf(tfor,t1);
to=sprintf(tfor,t2);
if nargin<1 & local.x & isempty(get(0,'UserData'))
 global b
 figure(5)
 clf
 set(5,'Name','GUISDAP for dummies','userdata',5,'Position',[40 170 350 390],'NumberTitle','off','defaulttextunit','pixel','toolbar','auto')
%load(fullfile(path_GUP,'matfiles','logo')), plot(y,x,'.k'), axis image
%set(get(gca,'child'),'markersize',10), set(gca,'visible','off'), pause(1)
 guisdaplogo(1), text(-.4,-.6,'Press mouse to continue'), waitforbuttonpress
 clf
 x=100; yh=25; y=(9:-1:1)*yh+130; ty=y+yh/2; x1=60; x2=240; x3=40;
 bg=uicontrol('Style','pushbutton','string','GO','position',[0 0 40 30],'callback','if strcmp(get(gcbo,''string''),''GO''),set(gcbo,''string'',''pause'',''fontsize'',10),else,waitforbuttonpress,end','fontsize',14);
 uicontrol('Style','pushbutton','string','?','position',[0 40 20 20],'callback',['web file:///' path_GUP '/doc/howto.html'],'tooltipstring','Get some help');
 uicontrol('Style','pushbutton','string','Quit','position',[50 0 40 20],'callback','gupquit');
 uicontrol('Style','pushbutton','string','Save','position',[50 20 40 20],'callback','o=uiputfile;if o,save_setup(o);end','tooltipstring','Save setup in file');
 br=uicontrol('Style','pushbutton','string','Reset','position',[50 40 40 20],'callback','global gfdfile,if exist(gfdfile,''file''),delete(gfdfile),end,analyse','tooltipstring','Reset to default');
 set(gca,'position',[0 0 1 1],'visible','off')
 text(0,ty(4),'Dsp expr')
 b(1)=uicontrol('Style','pushbutton','string',name_expr,'position',[x y(4) x1 yh],'value',0,'callback','o=uigetdir(path_exps);if o,[path_exps,name_expr]=fileparts(o);set(gcbo,''string'',name_expr),end');
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
 '[f,p]=uiputfile(''*'',''Save data in'',result_path);if p,result_path=fullfile(p,f,filesep);if ~strcmp(f,''AUTO'') & ~exist(result_path),mkdir(p,f);end,else,result_path=path_tmp;end,set(gcbo,''string'',result_path)');
 text(0,ty(7),'Real time')
 b(7)=uicontrol('Style','togglebutton','string','RT','position',[x y(7) x1 yh],'value',rt);
 text(0,ty(8),'Integration time')
 b(8)=uicontrol('Style','edit','string',num2str(intper),'position',[x y(8) x1 yh],'tooltipstring','0=antenna move');
 text(0,ty(9),'Disp figures')
 b(9)=uicontrol('Style','edit','string',num2str(figs),'position',[x y(9) x2 yh],'tooltipstring','datadump powerprofile fits parameters vizu');
 text(0,100,'Special')
 b(10)=uicontrol('Style','edit','string',extra,'position',[x 30 x2 120],'max',100,'HorizontalAlignment','left','tooltipstring','Matlab commands!');
 text(x+x1+x3,ty(4),'Vs')
 b(11)=uicontrol('Style','popupmenu','string',num2str((0:9)'),'position',[x+2*x1 y(4) x3 yh],'value',expver+1);
 go_on(bg)
else
 global a_gfd
 if nargin>0
  if nargin<8, iextra=[]; end
  if nargin<7, ifigs=[]; end
  if nargin<6, iintper=[]; end
  if nargin<5, iexpver=[]; end
  if nargin<4, iname_expr=[]; end
  if nargin<3, it2=[]; end
  if nargin<2, it1=[]; end
  if nargin<1, dpath=[]; end
  if isempty(dpath), dpath=data_path; end
 else
  dpath=minput('Data path',data_path,'s');
 end
 if ~strcmp(dpath,data_path)
  data_path=dpath;
  [intp,tt1,tt2,sitid,expver]=auto_parse;
  if ~isempty(tt1), t1=tt1; t2=tt2; end
  if ~isempty(intp), intper=intp; end
  if ~isempty(sitid), siteid=sitid; end
 end
 if nargin>0
  if ~isempty(iextra), extra=iextra; end
  if ~isempty(ifigs), figs=ifigs; end
  if ~isempty(iintper), intper=iintper; end
  if ~isempty(iexpver), expver=iexpver; end
  if ~isempty(iname_expr), name_expr=iname_expr; end
  if ~isempty(it2), t2=it2; end
  if ~isempty(it1), t1=it1; end
 else
  t1=minput('Start time',t1);
  t2=minput('End time',t2);
  path_exps=minput('Path exps',path_exps,'s');
  name_expr=minput('Dsp exp',name_expr,'s');
  expver=minput('Exp version',expver);
  siteid=strfind(sites,minput('Site',sites(siteid),'s'));
  result_path=minput('Result path',result_path,'s');
  rt=minput('Realtime (0/1)',rt);
  intper=minput('Integration time',intper);
  figs=minput('Display figures',figs);
  extra=row([extra ones(size(extra(:,end)))*'#']');
  extra=minput('Extra commands (''#''=line break)',extra(1:end-1),'s');
 end
 ex=extra; [extra,ex]=strtok(ex,'#');
 while ~isempty(ex)
  [ext,ex]=strtok(ex,'#');
  extra=char(extra,ext);
 end
 if nargin>0
  fprintf('Data path: %s\n',data_path);
  fprintf(['Start time: ' tfor '\n  End time: ' tfor '\n'],t1,t2);
  fprintf('Dsp exp: %s vs %g\n',name_expr,expver);
  fprintf('Result path: %s\n',result_path);
  fprintf('Integration time: %g\nDisplay figures: %g %g %g %g %g\n',intper,figs);
  fprintf('Extra commands; %s\n',extra);
  pause(2)
 end
 save_noglobal(gfdfile,name_expr,expver,siteid,data_path,result_path,t1,t2,rt,intper,path_exps,figs,extra)
 a_gfd.name_expr=name_expr; a_gfd.expver=expver; a_gfd.siteid=siteid;
 a_gfd.data_path=data_path; a_gfd.result_path=result_path; a_gfd.intper=intper;
 a_gfd.t1=t1; a_gfd.t2=t2; a_gfd.rt=rt; a_gfd.figs=figs; a_gfd.extra=extra;
 a_gfd.path_exps=path_exps;
 go_on
end
