function [varargout]=vizu(action,a2,a3)
% Plot GUISDAP results
%--------------------------------------------------------------------------
%     EISCAT additions Copyright 1999-2003  EISCAT Scientific Association. 
%     Permission to use, copy, and distribute, for non-commercial purposes, 
%     is hereby granted without fee, provided that this license information 
%     and copyright notice appear in all copies.
%--------------------------------------------------------------------------
% To plot with default dir names:
% >> vizu
% To plot without interaction:
% >> vizu dir exp_type antenna
% To update the plot with new files:
% >> vizu update
% To send the figure to the default printer:
% >> vizu print [printer]
% To save the current figure in .eps and .png formats:
% >> vizu save [extra tail]
% To get more selection possibilities
% >> vizu verbose
% To get even more selection possibilities
% >> vizu VERBOSE
% To run realtime inside guisdap
% >> vizu rtgup
% To reset and start over:
% >> vizu new [action]

global Time par2D par1D rpar2D name_expr name_ant axs axc hds
global height n_tot add_plot manylim
global DATA_PATH LOCATION START_TIME END_TIME MESSAGE1
global r_RECloc path_tmp path_GUP result_path webfile local owner
if nargin<2, a2=[]; end
if nargin<3, a3=[]; end
if nargin==0
  action=[];
elseif strcmp(action,'new')
  close(findobj('type','figure','userdata',6))
  axs=[]; axc=[];
  Time=[]; DATA_PATH=[]; START_TIME=[]; MESSAGE1=[];
  action=a2; a2=a3;
end
REALT=0; manylim=1;
Loc=local.site;
if strcmp(action,'rtgup')
  global a_year a_start a_end a_realtime a_autodir
  if isempty(START_TIME)
    if isstruct(a_autodir)
      START_TIME=[a_autodir.date 0 0 0];
      END_TIME=START_TIME+[0 0 0 24 0 0];
    else
      START_TIME=toYMDHMS(a_year,a_start);
      END_TIME=toYMDHMS(a_year,a_end);
    end
    close(findobj('type','figure','userdata',6))
    axs=[]; axc=[]; action=[];
  else
    action='update';
  end
  DATA_PATH=result_path; MESSAGE1='RT';
  if ~isempty(owner), MESSAGE1=owner; end
  REALT=1; manylim=Inf;
end

PLOT_STATUS=0;		% Status limit (0-3)
if isempty(DATA_PATH)
  if strcmp(lower(action),'verbose')
    DATA_PATH=minput('Data path',result_path,1);
  elseif ~REALT
    if isdir(action) | strfind(action,'/')
      DATA_PATH=action; action=[];
      if isstr(a2), MESSAGE1=a2; end
      if isstr(a3), name_ant=a3; end
    else
      disp(['Available data directories in ' result_path]);
      dirs=dir(result_path);
      for i=1:length(dirs), if dirs(i).isdir & dirs(i).name(1)~='.'
        disp(dirs(i).name);
      end, end
      DATA_PATH=sprintf('%s%s',result_path,minput('Data directory',' ',1));
    end
  end
end

if isempty(action) | strcmp(lower(action),'verbose')
 if isempty(Time)
  [Time,par2D,par1D,rpar2D]=load_param(DATA_PATH,PLOT_STATUS);
 end
 if ~isempty(axs), delete(axs), axs=[]; end
 if ~isempty(axc), delete(axc), axc=[]; end
elseif strcmp(action,'update')
 [Time,par2D,par1D,rpar2D]=load_param(DATA_PATH,PLOT_STATUS,1);
 set(0,'currentfig',findobj('type','figure','userdata',6))
elseif strcmp(action,'print') | strcmp(action,'save')
 if isempty(axs)
  disp('Nothing to print!')
 elseif isunix
  [i,j]=unix('which addlogo.sh');
  if ~i, set(hds(2:3),'visible','off'), end
  dirs=path_tmp(1:end-1);
  if strcmp(action,'print')
    fig=sprintf('vizu%06d',round(rand*1e6)); ext='ps';
  else
    fig=sprintf('%d-%02d-%02d_%s',START_TIME(1:3),name_expr);
    ext='eps';
    if isdir(DATA_PATH)
      dirs=DATA_PATH;
      if exist(fullfile(DATA_PATH,'.gup'),'file')
        load('-mat',fullfile(DATA_PATH,'.gup'),'intper')
        if intper>0
          fig=sprintf('%s_%d',fig,intper);
        end
      end
    end
    if ~isempty(a2), fig=sprintf('%s_%s',fig,a2); end
    fig=sprintf('%s@%s',fig,name_ant);
  end
  file=fullfile(dirs,fig);
  print(gcf,['-d' ext '2c'],[file '.' ext]);
  if ~i, unix(sprintf('addlogo.sh %s %s %s >/dev/null 2>&1',dirs,fig,ext)); end
  if strcmp(action,'print') | strcmp(a3,'print')
    if isempty(a2), dev=local.printer; else, dev=a2; end
    unix(['lp -c -d' dev ' ' file '.' ext ' >/dev/null 2>&1']);
    if strcmp(action,'print'), delete([file '.' ext]), end
  end
  if strcmp(action,'save')
    gd=fullfile(matlabroot,'sys','ghostscript',filesep);
%   unix(sprintf('%s -I%sps_files -I%sfonts -dNOPAUSE -q -sDEVICE=pdfwrite -sPAPERSIZE=a4 -sOutputFile=%s.pdf %s.%s </dev/null >/dev/null',...
    unix(sprintf('%s -I%sps_files -I%sfonts -dNOPAUSE -q -sDEVICE=png256 -g580x820 -sOutputFile=%s.png %s.%s </dev/null >/dev/null',...
      fullfile(gd,'bin',lower(computer),'gs'),gd,gd,file,file,ext));
    fprintf('Created %s.%s and .png\n',file,ext)
  end
  if ~i, set(hds(2:3),'visible','on'), end
 elseif strcmp(action,'print')
  print(gcf,'-dwinc');
 else
  fig=sprintf('%d-%02d-%02d_%s@%s',START_TIME(1:3),name_expr,name_ant);
  ext='eps';
  file=fullfile(DATA_PATH,fig);
  print(gcf,['-d' ext '2c'],[file '.' ext]);
  fprintf('Created %s.%s\n',file,ext)
 end
 return
end
if isempty(Time)
  disp('Vizu: No new data!'), return
end

%%%%%%%%%%%%%%%% Parameters %%%%%%%%
WHICH_PARAM='Ne Te Ti Vi AE';  % Select parameters to plot (AE=Az,El,Power)
%%%%%%%%%% Y-Axis scales %%%%%%%%%%
Y_PARAM=2;	% Y-Axis parameter
%%%%%%%%%%%%%%%% Plot options %%%%%
TITLE={'Range (km)','Altitude (km)','Electron Density (m^{-3})',...
	'Electron Temperature (K)','Ion Temperature (K)',...
	'Ion Drift Velocity (ms^{-1})','Collision frequency (Hz)',...
	'Ion Composition (O^+/N_e)','Residual'};
TITLE1={'Azimuth(\circ)','Elevation(\circ)','Power (10kW)',...
	'System Temperature (K)'};
SCALE =[50 900		% Range km
	80 600		% Altitude km
	10.^[10 12]	% Ne m-3
	0 4000		% Te K
	0 3000		% Ti K
	-200 200	% Vi m/s
	10.^[0 5]	% collision freq Hz
	0 1		% Comp
	.1 10]; 	% Res
PLF_SCALE	=[0 10];	% Langmuir freq MHz
RAWNE_SCALE	=10.^[9 12];	% Raw Ne
LAT_SCALE	=[65 78];	% Geogr Lat deg
MESSAGE2	='Not for publication - see Rules-of-the-road'; % top
LOCATION	=local.host;	% Place where plotting performed
FIGURE_TITLE	='EISCAT RADAR';% Title for the whole figure
if ~isempty(Loc)
 LOCATION	=['EISCAT-' Loc];
end
stretchSecs	=65;	% Number of seconds to stretch data points
FS	=10;		% Font size
TL	=[0.01 0.025];	% Tick size
height(2)=.03;		% Panel separation

%% Individual experiment setup %%%%
if REALT
 SCALE(6,:)=[-400 400];
 SCALE(2,:)=[80 800];
else
 if any(par1D(:,2)<75), SCALE(6,:)=[-500 500]; end
 if all(par1D(:,2)<35), SCALE(6,:)=[-1000 1000]; end
 if all(par1D(:,2)>75), SCALE(2,:)=[80 800]; end
 if strcmp(name_expr,'CP4B')
  SCALE(2,:)=[250 1100];
  if strcmp(minput('Which beam','N',1),'N')
   GATES=[21:40];
  else
   GATES=[1:20];
  end
 end
end
if strcmp(name_expr,'arcd')
 SCALE(2,:)=[59 139]; WHICH_PARAM='Nr AE';
 if any(par1D(:,2)<90), SCALE(2,1)=50; end
elseif strcmp(name_expr,'dlayer')  | strfind(name_expr,'cp6')
 SCALE(2,:)=[59 121]; WHICH_PARAM='Ne AE';
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if ~exist('GATES')
 GATES=1:size(par2D,1);
end
if length(GATES)==1
 WHICH_PARAM='Ne TT Vi LL';
end
%%%%%%%%%% Time scales %%%%%%%%%%%%%
if strcmp(lower(action),'verbose')
% name_expr=minput('Experiment name',name_expr,1);
 if isempty(START_TIME)
  START_TIME=floor(datevec(Time(1,1)));
  END_TIME=ceil(datevec(Time(2,end)));
 end
 START_TIME=minput('Start time',START_TIME);
 END_TIME=minput('  End time',END_TIME);
 if ~isempty(a2)
  SCALE(2,:)=a2;
 elseif length(GATES)>1
  SCALE(2,:)=minput('Altitude scale',10*[floor(min(par2D(GATES(1),:,2))/10) ceil(max(par2D(GATES(end),:,2))/10)]);
 end
 if isempty(a3)
  disp('Parameters: Ne Te Ti Vi AE TT LL Rs O+ Co Nr Lf L1 Ls Pf P1')
  WHICH_PARAM=minput('Choose',WHICH_PARAM,1);
 else
  WHICH_PARAM=a3;
 end
elseif ~REALT
 if strcmp(DATA_PATH(end),filesep)
  DATA_PATH=DATA_PATH(1:end-1);
 end
 [d,dpath]=fileparts(DATA_PATH);
 START_TIME=datevec(median(Time(:))); START_TIME(4:6)=[0 0 0];
%START_TIME=[sscanf(dpath(1:10),'%4d-%2d-%2d')' 0 0 0];
 END_TIME=START_TIME+[0 0 0 24 0 0];
end
if isempty(MESSAGE1)
 MESSAGE1=minput('Type of experiment','CP',1);
end
nameant=lower(name_ant(1:3));
if strcmp(nameant,'32m') | strcmp(nameant,'42m') | strcmp(nameant,'esr')
 FIGURE_TITLE='EISCAT SVALBARD RADAR';
 stretchSecs=65;
 fradar=500e6;
elseif strcmp(nameant,'uhf') | strcmp(nameant,'kir') | strcmp(nameant,'sod')
 FIGURE_TITLE='EISCAT UHF RADAR';
 fradar=930e6;
elseif strcmp(nameant,'vhf')
 FIGURE_TITLE='EISCAT VHF RADAR';
 fradar=224e6;
end
if strcmp(action,'VERBOSE')
 Y_PARAM=minput('Y parameter (Ran-1 Alt-2 Lat-3)',Y_PARAM);
 SCALE=minput('Scales (Ran Alt Ne Te Ti Vi Coll Comp Res)',SCALE')';
 PLF_SCALE=minput('Scale (plf)',PLF_SCALE);
 RAWNE_SCALE=minput('Scale (rawNe)',RAWNE_SCALE);
 LAT_SCALE=minput('Scale (lat)',LAT_SCALE);
 manylim=Inf;
 stretchSecs=minput('Strech secs',stretchSecs);
end
option=zeros(20,1);
if findstr(WHICH_PARAM,'Ne'), option(3)=1; end
if findstr(WHICH_PARAM,'Te'), option(4)=1; end
if findstr(WHICH_PARAM,'Ti'), option(5)=1; end
if findstr(WHICH_PARAM,'Vi'), option(6)=1; end
if findstr(WHICH_PARAM,'AE'), option(11)=1; end
if findstr(WHICH_PARAM,'TT') & length(GATES)==1, option(12)=1; end
if findstr(WHICH_PARAM,'LL') & length(GATES)==1, option(13)=1; end
if findstr(WHICH_PARAM,'O+'), option(8)=1; end
if findstr(WHICH_PARAM,'Rs'), option(9)=1; end
if findstr(WHICH_PARAM,'Lf'), option(14)=option(14)+1; end
if findstr(WHICH_PARAM,'L1'), option(14)=option(14)+2; end
if findstr(WHICH_PARAM,'Ls'), option(14)=option(14)+4; end
if findstr(WHICH_PARAM,'Pf'), option(16)=option(16)+1; end
if findstr(WHICH_PARAM,'P1'), option(16)=option(16)+2; end
if findstr(WHICH_PARAM,'Co'), option(7)=1; end
if findstr(WHICH_PARAM,'Nr') & ~isempty(rpar2D), option(15)=1; end
n_tot=sum(rem(option,2)+fix(option/2)-fix(option/4));
if n_tot>6, FS=8; height(2)=.02; TL=TL/2; end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% to label plot to match window selected

month_ix={'January','February','March','April','May','June',...
	   'July','August','September','October','November','December'};
endtime=datevec(datenum(END_TIME)-1/86400);
if START_TIME(2)==endtime(2)
 if START_TIME(3)==endtime(3)
  day=sprintf('%d',START_TIME(3));
 else
  day=sprintf('%d-%d',START_TIME(3),endtime(3));
 end
 t2=sprintf('%s %s',day,char(month_ix(START_TIME(2))));
else
 t2=sprintf('%d %s - %d %s',START_TIME(3),char(month_ix(START_TIME(2))),endtime(3),char(month_ix(endtime(2))));
end
t2=sprintf('%s, %s, %s %d',name_ant,name_expr,t2,START_TIME(1));
if ~isempty(MESSAGE1)
  t2=[MESSAGE1 ', ' t2];
end 
t1=['Produced@' LOCATION ', ' date];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawnow
if isempty(findobj('type','figure','userdata',6))
 figure
 if ~prod(get(0,'ScreenSize'))-1 & local.matlabversion==13
  close(gcf),figure; % Matlab R13 bug
 end
 set(gcf,'Position',[400 30 587 807],'DefaultAxesFontSize',FS,...
   'DefaultAxesTickDir','out','DefaultTextFontSize',FS,'UserData',6,...
   'DefaultAxesXMinorTick','off','defaultaxesbox','on',...
   'DefaultAxesTickLength',TL,...
   'renderer','painters','PaperPosition',[0.4 0.7 20.65 28.4],...
   'DefaultAxesFontName','Helvetica','DefaultTextFontName','Helvetica',...
   'DefaultAxeslayer','top','DefaultsurfaceEdgeColor','none',...
   'DefaultTextHorizontalAlignment','center',...
   'Name',['GUISDAP results from ',DATA_PATH])
% set(gcf,'PaperType','A4','PaperUnits','centimeters','NumberTitle','off',...
%  'defaultAxesColorOrder',[1 0 0;0 1 0;0 0 1;0 0 0;1 0 1;0 1 1;1 1 0])
 uimenu('label','Update','callback','vizu(''update'')');
 uimenu('label','Edit','callback','vizu(''verbose'')');
 uimenu('label','Save','callback','if isunix,vizu(''save''),else,printdlg,end');
 uimenu('label','Print','callback','if isunix,vizu(''print''),else,printdlg,end');

 ti=axes('Position',[0.0 0.87 0.95 0.08]);
 set(ti,'Visible','off');
 text('Position',[0.55,0.80],'VerticalAlignment','top', ...
      'String',FIGURE_TITLE,'FontSize',16,'FontWeight','bold')
 hds=text('Position',[0.55,0.4],'VerticalAlignment','top',...
      'String',t2,'FontSize',12,'Interpreter','none');
 text('Position',[0.125,0.12],'HorizontalAlignment','left',...
      'Color',[.5 .5 .5],'VerticalAlignment','top','String',t1)
 text('Position',[0.86,0.12],'HorizontalAlignment','right',...
      'Color',[.5 .5 .5],'VerticalAlignment','top','String',MESSAGE2)
 axs=[]; axc=[];
 %logo...
 hds(2)=text('Position',[0.55,1.3],'VerticalAlignment','top','FontSize',24,...
      'FontWeight','bold','String','EISCAT Scientific Association');
 load(fullfile(path_GUP,'matfiles','logo'))
 axes('Position',[.07 .9 .1 .075]); plot(y,x,'.k')
 hds(3)=get(gca,'child'); set(hds(3),'markersize',1)
 set(gca,'xlim',[0 202],'ylim',[0 202],'visible','off')
else
 set(hds(1),'string',t2)
end
add_plot=0;
height(1)=(0.80-(n_tot-1)*height(2))/n_tot;
colormap(myb(78,1));
s=size(par2D,2);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine the Y-Axis parameter
if Y_PARAM<3
 YTitle=TITLE(Y_PARAM);
 y_param=par2D(:,:,Y_PARAM);
 Yscale=SCALE(Y_PARAM,:);
elseif Y_PARAM==3
 YTitle='Latitude (\circN)';
 y_param=par2D(:,:,1);
 ll=par1D(:,[2 1]);
 for i=1:s,for j=1:size(y_param,1)
  d=loc2gg(r_RECloc,[ll(i,:) y_param(j,i)]); y_param(j,i)=d(1);
 end,end
 Yscale=[];
end
if stretchSecs & s>1
 dt=(Time(1,2:end)-Time(2,1:end-1));
 if stretchSecs==1
  d=find(dt>0 & dt<=median(dt)+1/86399);
 else
  d=find(dt>0 & dt<=stretchSecs/86399);
 end
 if ~isempty(d)
  Time(2,d)=Time(2,d)+dt(d)/2;
  Time(1,d+1)=Time(2,d);
 end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot the parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for i=[3 7 9]
 if option(i)
 surf_plot(s,y_param(GATES,:),par2D(GATES,:,i),SCALE(i,:),Yscale,YTitle,TITLE(i),'log')
 end
end
if option(12)
 d=many(par2D(GATES,:,4:5),SCALE(4,:));
 line_plot(s,reshape(par2D(GATES,:,4:5),[],2),d,'Temperatures (K)',{'Electron' 'Ion'},[])
end
for i=[4 5 8]
 if option(i)
  surf_plot(s,y_param(GATES,:),par2D(GATES,:,i),SCALE(i,:),Yscale,YTitle,TITLE(i),[])
 end
end
if option(6)
 if length(GATES)==1
  d=many(par2D(GATES,:,6),SCALE(6,:));
  line_plot(s,par2D(GATES,:,6)',d,TITLE(6),{'positive away'},[])
 else
  surf_plot(s,y_param(GATES,:),par2D(GATES,:,6),SCALE(6,:),Yscale,YTitle,char(char(TITLE(6)),'         (away)'),[])
 end
end
if option(14)
 lf=8.98e-6*sqrt(par2D(GATES,:,3)).*sqrt(1+3*7.52e5*(fradar/3e8)^2*par2D(GATES,:,4)./par2D(GATES,:,3));
 if rem(option(14),2)
  surf_plot(s,y_param(GATES,:),lf,PLF_SCALE,Yscale,YTitle,'Plasma line frequency (MHz)',[])
 end
 if option(14)>1
  lf=find_plf_peak(s,y_param(GATES,:),Yscale,lf,16,PLF_SCALE,fix(option(14)/2)-1);
  if fix(option(14)/2)-1
   surf_plot(s,lf{2}'*ones(1,s),lf{3},[0 max(max(lf{3}))],PLF_SCALE,'Plasma line spectrum (MHz)','Power (any unit)',[])
  else
   d=many(lf,PLF_SCALE);
   line_plot(s,lf,d,'Cutoff plasma line frequency (MHz)',[],[])
  end
 end
end
if option(16)
 pf=8.98e-6*sqrt(par2D(GATES,:,3));
 if rem(option(16),2)
  surf_plot(s,y_param(GATES,:),pf,PLF_SCALE,Yscale,YTitle,'Plasma frequency (MHz)',[])
 end
 if option(16)>1
  pf=find_plf_peak(s,y_param(GATES,:),Yscale,pf,5,PLF_SCALE,0);
  d=many(pf,PLF_SCALE);
  line_plot(s,pf,d,'Cutoff plasma frequency (MHz)',[],[])
 end
end
if option(15)
 surf_plot(s,rpar2D(:,:,Y_PARAM),rpar2D(:,:,3),RAWNE_SCALE,Yscale,YTitle,'Raw electron density (m^{-3})','log')
end
if option(11)
 ae=par1D(:,[3 1 2:2:end]);
 d=find(ae(:,3)<90.1 & ae(:,3)>89.9); ae(d,2)=NaN;
 d=many(ae,[0 360]);
 line_plot(s,ae,d,'Radar parameters',TITLE1([3 1 2 4]),[])
end
if option(13)
 ll=[par1D(:,[3 2 1]) par2D(GATES,:,1)'];
 for i=1:s, ll(i,2:4)=loc2gg(r_RECloc,ll(i,2:4)); end
 if size(par1D,2)>3, ll=[ll par1D(:,4:end)]; end
 d=many(ll,[0 300])+[-10 10];
 line_plot(s,ll,d,'Radar parameters',[TITLE1(3) {'Latitude (\circN)','Longitude (\circE)'} TITLE(2) TITLE1(4) {'Offset (\mus)'}],[])
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xlabel('UNIVERSAL TIME')
drawnow
if REALT & ~isempty(Loc) & a_realtime & isunix
 if prod(get(0,'ScreenSize'))-1;
  flag='-dpng'; flag2='-r72';
 else
  flag='-dpng256'; flag2=[];
 end
 pngfile=sprintf('%sGup_%s.png',path_tmp,name_ant(1:3));
 print(gcf,flag,flag2,pngfile)
 webfile(1)=cellstr(pngfile);
end
if nargout
 i=0; j=1; argout=str2mat('yy','lf','pf','ll','xx'); xx=[];
 while i<nargout
  i=i+1;
  if j<5, j=j+1; end
  while ~exist(argout(j,:),'var') & j<5
   j=j+1;
  end
  varargout(i)={eval(argout(j,:))};
 end
end
return

%%%%% surf_plot function %%%%%%%%%%
function surf_plot(s,yparam,zparam,zscale,yscale,YTitle,Barlabel,lg)

zscale=many(zparam,zscale);
if size(zparam,1)==1
 line_plot(s,zparam',zscale,Barlabel,[],lg);
 return
end
global Time axs axc add_plot
add_plot=add_plot+1;
if length(axs)<add_plot
 setup_axes(yscale,YTitle)
 set(gca,'UserData',zscale)
else
 set(gcf,'currentaxes',axs(add_plot))
 zscale=get(gca,'UserData');
end
  
if strcmp(lg,'log')
 zparam(find(zparam<=0))=eps;
 zparam=log10(zparam); zscale=log10(zscale);
end
dy=diff(yparam)/2; yparam=[yparam-[dy(1,:);dy];yparam(end,:)+dy(end,:)];
zparam=[zparam;zparam(end,:)];
o2=ones(1,2);
for i=1:s
 surface(Time(:,i),yparam(:,i)*o2,zparam(:,i)*o2)
end

if length(axc)<add_plot
 if ~isempty(zscale)
  set(gca,'CLim',zscale+5*eps*abs(zscale).*[-1 1]);
 end
 axc=[axc my_colorbar(Barlabel,lg)];
end
return

%%%%% line_plot function %%%%%%%%%%
function line_plot(s,yparam,yscale,YTitle,Clabel,lg)
global Time axs add_plot
add_plot=add_plot+1;
if length(axs)<add_plot
 setup_axes(yscale,YTitle)
 set(gca,'UserData',yscale)
 if strcmp(lg,'log')
  set(gca,'Yscale','log')
 end
 jj=0;
 for j=1:size(yparam,2)
  if ~isempty(Clabel) & any(isfinite(yparam(:,j)))
   color=get(gca,'ColorOrder'); jj=jj+1;
   text('String',char(Clabel(j)),'Units','Normalized',...
        'Position',[1+.025*jj 0.5],'Rotation',-90,'Color',color(j,:))
  end
 end
else
 set(gcf,'currentaxes',axs(add_plot))
 yscale=get(gca,'UserData');
end

o2=ones(2,1);
if s>1
 d1=1;
 d=[find(Time(1,2:end)-Time(2,1:end-1)>0) s];
 nc=size(yparam,2);
 for i=1:length(d)
  line(col(Time(:,d1:d(i))),reshape(o2*row(yparam(d1:d(i),:)),2*(d(i)-d1+1),nc))
  d1=d(i)+1;
 end
else
 line(Time,o2*yparam)
end
return

function setup_axes(yscale,YTitle)
global axs height n_tot Time START_TIME END_TIME add_plot xticks
axs=[axs axes('Position',[.12 .06+(n_tot-add_plot)*sum(height) .7 height(1)])];
set(gca,'xgrid','on','ygrid','on')
xlim=[datenum(START_TIME) datenum(END_TIME)];
set(gca,'XLim',xlim)
tickform='HH:MM'; td=diff(xlim);
if td>7, tickform='dd/mm';
elseif td<.003, tickform='HH:MM:SS'; end
if length(axs)==1
 datetick(gca,'x',tickform,'keeplimits')
 xticks=get(gca,'xtick');
 if ~(prod(get(0,'ScreenSize'))-1) & (isempty(xticks) | all(xticks<xlim(1) | xticks>xlim(2)) | (length(xticks)<3 & td>.003))
  fs=get(gca,'fontsize');
  set(gca,'fontsize',fs/12.429) % Matlab bug...
  datetick(gca,'x',tickform,'keeplimits')
  xticks=get(gca,'xtick');
  set(gca,'fontsize',fs)
 end
end
set(gca,'xtick',xticks)
datetick(gca,'x',tickform,'keeplimits','keepticks')

if ~isempty(yscale)
 set(gca,'YLim',yscale+5*eps*abs(yscale).*[-1 1])
end
ylabel(YTitle)
return

function f=myb(nl,cut)
% create palette with nl levels
if nargin<2, cut=0; end
if nargin==0, nl=[]; end
if isempty(nl)
 nl=size(get(gcf,'colormap'),1);
end
f=[0 0 0 0 0 1 2 2 2 2 2 2 2
   0 0 0 1 2 2 2 1 0 0 0 1 2
   0 1 2 1 0 0 0 0 0 1 2 2 2]'/2;
nc=size(f,1);
n=nc-cut;
b=round([0:n-1]/(n-1)*(nl-1))+1;
%f=sin(interp1(b,f(1:n,:),1:nl)*pi/2);
f=tanh(interp1(b,f(1:n,:),1:nl))/tanh(1);
return

function l=many(x,l,p)
global manylim
if nargin<3, p=sqrt(min(size(x))); end
lx=prod(size(x));
x=sort(x(find(isfinite(x))));
llx=length(x);
p=p/100; if length(p)<2, p=ones(1,2)*p; end
p=round([1 llx]+[1 -1].*p*manylim*lx);
if llx>p(1) & p(1)>0 & x(p(1))<l(1)
 l(1)=x(p(1));
end
if llx>p(2) & p(2)>0 & x(p(2))>l(2)
 l(2)=x(p(2));
end
