function vizu(action,a2,a3)
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
% >> vizu dir exp_type
% To update the plot with new files:
% >> vizu update
% To send the figure to the default printer:
% >> vizu print
% To save the current figure in .eps and .png formats:
% >> vizu save
% To get more selection possibilities
% >> vizu verbose
% To run realtime inside guisdap
% >> vizu rtgup
% To reset and start over:
% >> vizu new [action]

global Time RAN ALT Ne Te Ti Vi Status Az El Pt Tsys name_expr name_ant axs hds
global DATA_PATH PLOT_STATUS LOCATION START_TIME END_TIME MESSAGE1
global r_RECloc path_tmp path_GUP result_path
if nargin<2, a2=[]; end
if nargin<3, a3=[]; end
if nargin==0
  action=[];
elseif strcmp(action,'new')
  close(findobj('type','figure','userdata',6))
  Time=[]; DATA_PATH=[]; START_TIME=[]; MESSAGE1=[];
  action=a2; a2=a3;
end
REALT=0;
Loc=getenv('EISCATSITE');
if strcmp(action,'rtgup')
  global a_year a_start a_end a_realtime
  if isempty(START_TIME)
    START_TIME=toYMDHMS(a_year,a_start);
    END_TIME=toYMDHMS(a_year,a_end);
    close(findobj('type','figure','userdata',6))
    action=[];
  else
    action='update';
  end
  DATA_PATH=result_path; MESSAGE1='RT';
  REALT=1;
end

% Save previous values (if any) and (re)load variables from config file

if ~isempty(PLOT_STATUS)
  OLD_STATUS=PLOT_STATUS;
else
  OLD_STATUS=-99;
end
CLPos=1.15;
if isempty(DATA_PATH)
  if strcmp(action,'verbose')
    DATA_PATH=minput('Data path',result_path,1);
  elseif ~REALT
    if isdir(action)
      DATA_PATH=action; action=[];
      if isstr(a2), MESSAGE1=a2; end
    else
      disp(['Available data directories in ' result_path]);
      if isunix
        unix(['ls -1p ' result_path ' | grep "/"']);
      else
        disp(ls(result_path));
      end
      DATA_PATH=sprintf('%s%s',result_path,minput('Data directory',' ',1));
    end
  end
end

if isempty(action) | strcmp(action,'verbose')
  if isempty(Time) | OLD_STATUS~=PLOT_STATUS
    [Time,RAN,ALT,Ne,Te,Ti,Vi,Status Az El Pt,Tsys]=load_param(DATA_PATH);
  end
  axs=[];
elseif strcmp(action,'update')
  [Time,RAN,ALT,Ne,Te,Ti,Vi,Status Az El Pt,Tsys]=load_param(DATA_PATH,1);
  if isempty(Time)
    disp('Vizu: No new data!'), return
  end
  set(0,'currentfig',findobj('type','figure','userdata',6))
  OLD_STATUS=99;
elseif strcmp(action,'print') | strcmp(action,'save')
 if isunix
  set(hds(2:3),'visible','off')
  if strcmp(action,'print')
    fig=sprintf('vizu%06d',round(rand*1e6)); dir=path_tmp(1:end-1); ext='ps';
  else
    fig=sprintf('%d-%02d-%02d_%s@%s',START_TIME(1:3),name_expr,name_ant);
    dir=DATA_PATH; ext='eps';
  end
  file=fullfile(dir,fig);
  print(gcf,['-d' ext '2c'],[file '.' ext]);
  unix(sprintf('addlogo.sh %s %s %s >/dev/null 2>&1',dir,fig,ext));
  if strcmp(action,'print')
    unix(['lp -dcolor ' file '.' ext ' >/dev/null 2>&1']);
    delete([file '.' ext])
  else
    gd=fullfile(matlabroot,'sys','ghostscript',filesep);
%   unix(sprintf('%s -I%sps_files -I%sfonts -dNOPAUSE -q -sDEVICE=pdfwrite -sPAPERSIZE=a4 -sOutputFile=%s.pdf %s.%s </dev/null >/dev/null',...
    unix(sprintf('%s -I%sps_files -I%sfonts -dNOPAUSE -q -sDEVICE=png256 -g580x820 -sOutputFile=%s.png %s.%s </dev/null >/dev/null',...
      fullfile(gd,'bin',getenv('ARCH'),'gs'),gd,gd,file,file,ext));
    fprintf('Created %s.%s and .png\n',file,ext)
  end
  set(hds(2:3),'visible','on')
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

%%%%%%%%%%%%%%%% Parameters %%%%%%%%
WHICH_PARAM='Ne Te Ti Vi AE';  % Select parameters to plot (AE=Az,El,Power)
%%%%%%%%%% Y-Axis scales %%%%%%%%%%
Y_PARAM		='Altitude';	% Y-Axis parameter
ALT_SCALE	=[80 600];	% Scale for Altitude (km)
RAN_SCALE	=[150 900];	% Scale for Range (km)
LAT_SCALE	=[65 78];	% Scale for Geog. Latitude (deg)
%%%%%%%%%%%%%%%% Plot options %%%%%
PLOT_STATUS	=0;		% Status limit (0-3)
TIME_TICKS	=12; 		% Maximum number of time ticks to print
MESSAGE2	='Not for publication - see Rules-of-the-road'; % top
LOCATION	=getenv('HOSTNAME');	% Place where plotting performed
NE_SCALE 	=10.^[10.0 12.0];	% Scale for Ne (m-3)
TE_SCALE	=[0 4000];	% Scale for Te (K)
TI_SCALE	=[0 3000];	% Scale for Ti (K)
VI_SCALE	=[-200 200];	% Scale for Vi (m/s)
FIGURE_TITLE	='EISCAT RADAR';% Title for the whole figure
if ~isempty(Loc)
  LOCATION	=['EISCAT-' Loc];
end
stretchSecs	=31;	% Number of seconds to stretch data points

%% Individual experiment setup %%%%
if REALT
 VI_SCALE=[-400 400];
 ALT_SCALE=[80 800];
else
 if any(El<75), VI_SCALE=[-500 500]; end
 if all(El<35), VI_SCALE=[-1000 1000]; end
 if all(El>75), ALT_SCALE=[80 800]; end
 if strcmp(name_expr,'CP4B')
  WHICH_BEAM=minput('Which beam','N',1);  
  ALT_SCALE=[250 1100];
  if strcmp(WHICH_BEAM,'N')
   GATES=[21:40];
  else
   GATES=[1:20];
  end
 elseif strcmp(name_expr,'gup0')
  ALT_SCALE=[150 800];
 end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if ~exist('GATES')
  GATES=1:size(ALT,1);
end
if length(GATES)==1
  WHICH_PARAM='Ne TT Vi LL';
end

%%%%%%%%%% Time scales %%%%%%%%%%%%%
if strcmp(action,'verbose')
% name_expr=minput('Experiment name',name_expr,1);
  if isempty(name_ant)
    name_ant=minput('Which antenna','32m',1);
  end
  if isempty(START_TIME)
    START_TIME=floor(datevec(Time(1,1)));
    END_TIME=ceil(datevec(Time(2,end)));
  end
  START_TIME=minput('Start time',START_TIME);
  END_TIME=minput('  End time',END_TIME);
  if length(GATES)>1
   ALT_SCALE=[floor(ALT(GATES(1))/10) ceil(ALT(GATES(end))/10)]*10;
   ALT_SCALE=minput('Altitude scale',ALT_SCALE);
  end
  WHICH_PARAM=minput('Which parameters',WHICH_PARAM,1);
elseif ~REALT
  if strcmp(DATA_PATH(end),filesep)
    DATA_PATH=DATA_PATH(1:end-1);
  end
  [d,dpath]=fileparts(DATA_PATH);
  START_TIME=[str2num(dpath(1:4)) str2num(dpath(6:7)) str2num(dpath(9:10)) 0 0 0];
  END_TIME=START_TIME+[0 0 0 24 0 0];
  name_ant=dpath(end-2:end);
end
if isempty(MESSAGE1)
  MESSAGE1=minput('Type of experiment','CP',1);
end
if strcmp(name_ant,'32m') | strcmp(name_ant,'42m')
  FIGURE_TITLE	='EISCAT SVALBARD RADAR';
  stretchSecs	=65;
elseif strcmp(name_ant,'UHF') | strcmp(name_ant,'Kir') | strcmp(name_ant,'Sod')
  FIGURE_TITLE	='EISCAT UHF RADAR';
elseif strcmp(name_ant,'VHF')
  FIGURE_TITLE	='EISCAT VHF RADAR';
end
option=zeros(7,1);
% options 'Ne Te Ti Vi AE TT LL'
if findstr(WHICH_PARAM,'Ne'), option(1)=1; end
if findstr(WHICH_PARAM,'Te'), option(2)=1; end
if findstr(WHICH_PARAM,'Ti'), option(3)=1; end
if findstr(WHICH_PARAM,'Vi'), option(4)=1; end
if findstr(WHICH_PARAM,'AE'), option(5)=1; end
if findstr(WHICH_PARAM,'TT') & length(GATES)==1, option(6)=1; end
if findstr(WHICH_PARAM,'LL') & length(GATES)==1, option(7)=1; end
n_tot=nnz(option);

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
if isempty(findobj('type','figure','userdata',6))
  figure
  if ~prod(get(0,'ScreenSize'))-1 & strcmp(version('-release'),'13')
    close(gcf),figure; % Matlab R13 bug
  end
  set(gcf,'Position',[400 30 587 807],'DefaultAxesFontSize',10,...
   'DefaultAxesTickDir','out','DefaultTextFontSize',10,'UserData',6,...
   'DefaultAxesXMinorTick','off','defaultaxesbox','on',...
   'renderer','painters','PaperPosition',[0.4 0.7 20.65 28.4],...
   'DefaultAxesFontName','Helvetica','DefaultTextFontName','Helvetica',...
   'DefaultAxeslayer','top','DefaultsurfaceEdgeColor','none',...
   'Name',['GUISDAP results from ',DATA_PATH])
% set(gcf,'PaperType','A4','PaperUnits','centimeters','NumberTitle','off',...
%  'defaultAxesColorOrder',[1 0 0;0 1 0;0 0 1;0 0 0;1 0 1;0 1 1;1 1 0])
  uimenu('label','Update','callback','vizu(''update'')');
  uimenu('label','Edit','callback','vizu(''verbose'')');
  uimenu('label','Save','callback','if isunix,vizu(''save''),else,printdlg,end');
  uimenu('label','Print','callback','if isunix,vizu(''print''),else,printdlg,end');

  ti=subplot(n_tot+1,1,1);
  set(ti,'Position',[0.0 0.87 0.95 0.08]);
  set(ti,'Visible','off');
  text('Position',[0.55,0.80],'HorizontalAlignment','center',...
      'VerticalAlignment','top', ...
      'String',FIGURE_TITLE,'FontSize',16,'FontWeight','bold')
  hds=text('Position',[0.55,0.4],'HorizontalAlignment','center',...
      'VerticalAlignment','top','String',t2,'FontSize',12,'Interpreter','none');
  text('Position',[0.125,0.12],'HorizontalAlignment','left',...
      'Color',[.5 .5 .5],'VerticalAlignment','top','String',t1)
  text('Position',[0.86,0.12],'HorizontalAlignment','right',...
      'Color',[.5 .5 .5],'VerticalAlignment','top','String',MESSAGE2)
  axs=[];
  %logo...
  hds(2)=text('Position',[0.55,1.3],'HorizontalAlignment','center',...
      'VerticalAlignment','top','FontSize',24,'FontWeight','bold',...
      'String','EISCAT Scientific Association');
  load(fullfile(path_GUP,'matfiles','logo'))
  axes('Position',[.07 .9 .1 .075]); plot(y,x,'.k')
  hds(3)=get(gca,'child'); set(hds(3),'markersize',1)
  set(gca,'xlim',[0 202],'ylim',[0 202],'visible','off')
else
  set(hds(1),'string',t2)
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine the Y-Axis parameter

if upper(Y_PARAM(1))=='R'
  Y='RAN';
  YTitle='Range (km)';
elseif upper(Y_PARAM(1))=='M'
  Y='GML';
  YTitle='Geomag. lat.';
  g=[73.60086 73.52204 73.44751 73.36974 73.29306 ...
     73.21563 73.13813 73.06375 72.98671 72.91084 ...
     72.84470 72.70182 72.47776 72.25554 72.03100 ...
     71.81182 71.59326 71.37448 71.15875 70.94426 ...
     70.72778 70.42110 70.02116 69.66894];
  GML=repmat(g,1,910);
else
  Y='ALT';
  YTitle='Altitude (km)';
end

y_param=eval(Y);
if exist([Y '_SCALE'])
  Yscale=eval([Y '_SCALE']);
else
  Yscale=[];
end;
add_plot=0;
height=(0.80-(n_tot-1)*0.03)/n_tot;
colormap(myb(78,1));
s=size(Status);
if OLD_STATUS~=PLOT_STATUS
  i=find(Status>PLOT_STATUS);
  Ne(i)=NaN; Te(i)=NaN; Ti(i)=NaN; Vi(i)=NaN;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot the parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if stretchSecs & s(2)>1
 dt=(Time(1,2:end)-Time(2,1:end-1));
 d=find(dt>0 & dt<=stretchSecs/86399);
 Time(2,d)=Time(2,d)+dt(d)/2;
 Time(1,d+1)=Time(2,d);
end
if option(1)
 if length(GATES)==1
  [add_plot]=line_plot(add_plot,action,n_tot,height,s,Ne(GATES,:)',[],'Electron Density (m^{-3})',[],CLPos,[]);
 else
  [add_plot]=surf_plot(add_plot,action,n_tot,height,s,y_param(GATES,:),Ne(GATES,:),NE_SCALE,Yscale,YTitle,'Electron Density (m^{-3})',CLPos,'log');
 end
end
if option(6)
  [add_plot]=line_plot(add_plot,action,n_tot,height,s,[Te(GATES,:);Ti(GATES,:)]',[],'Temperatures (K)',{'Electron' 'Ion'},CLPos,[]);
end
if option(2)
 if length(GATES)==1
  [add_plot]=line_plot(add_plot,action,n_tot,height,s,Te(GATES,:)',[],'Electron Temperature (K)',[],CLPos,[]);
 else
  [add_plot]=surf_plot(add_plot,action,n_tot,height,s,y_param(GATES,:),Te(GATES,:),TE_SCALE,Yscale,YTitle,'Electron Temperature (K)',CLPos,[]);
 end
end
if option(3)
 if length(GATES)==1
  [add_plot]=line_plot(add_plot,action,n_tot,height,s,Ti(GATES,:)',[],'Ion Temperature (K)',[],CLPos,[]);
 else
  [add_plot]=surf_plot(add_plot,action,n_tot,height,s,y_param(GATES,:),Ti(GATES,:),TI_SCALE,Yscale,YTitle,'Ion Temperature (K)',CLPos,[]);
 end
end
if option(4)
 if length(GATES)==1
  if VI_SCALE(1)<VI_SCALE(2)
    [add_plot]=line_plot(add_plot,action,n_tot,height,s,Vi(GATES,:)',[],'Ion Drift Velocity (ms^{-1})',{'positive away'},CLPos,[]);
  else
    [add_plot]=line_plot(add_plot,action,n_tot,height,s,-Vi(GATES,:)',[],'Ion Drift Velocity (ms^{-1})',{'positive towards'},CLPos,[]);
  end
 else
  if VI_SCALE(1)<VI_SCALE(2)
    [add_plot]=surf_plot(add_plot,action,n_tot,height,s,y_param(GATES,:),Vi(GATES,:),VI_SCALE,Yscale,YTitle,'Ion Velocity (ms^{-1})',CLPos,[]);
    text('String','(away)','Units','Normalized','Position',[CLPos-.025 0.5],...
        'HorizontalAlignment','Center','Rotation',-90)
  else
    [add_plot]=surf_plot(add_plot,action,n_tot,height,s,y_param(GATES,:),-Vi(GATES,:),-VI_SCALE,Yscale,YTitle,'Ion Velocity (ms^{-1})',CLPos,[]);
    text('String','(towards)','Units','Normalized','Position',[CLPos-.025 0.5],...
        'HorizontalAlignment','Center','Rotation',-90)
  end
 end
end
if option(5)
 [add_plot]=line_plot(add_plot,action,n_tot,height,s,[Pt Az El Tsys],[0 360],'Radar parameters',{'Power(10kW)' 'Azimuth(\circ)' 'Elevation(\circ)' 'System Temperature (K)'},CLPos,[]);
%[add_plot]=line_plot(add_plot,action,n_tot,height,s,[Pt/100 [rem(Az+180,360)-180 El]*pi/180],[],'Power and pointing',{'Power(MW)' 'Azimuth(rad)','Elevation(rad)'},CLPos,[]);
end
if option(7)
 ll=[El Az RAN(GATES,:)'];
 for i=1:size(ll,1), ll(i,:)=loc2gg(r_RECloc,ll(i,:)); end
 [add_plot]=line_plot(add_plot,action,n_tot,height,s,[Pt ll Tsys],[],'Radar parameters',{'Power(10kW)' 'Latitude(\circN)','Longitude(\circE)' 'Altitude(km)' 'System Temperature (K)'},CLPos,[]);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~strcmp(action,'update')
  set(axs,'xgrid','on','ygrid','on')
  set(axs,'XLim',[datenum(START_TIME) datenum(END_TIME)])
  tickform='HH:MM'; td=datenum(END_TIME)-datenum(START_TIME);
  if td>7, tickform='dd/mm';
  elseif td<.003, tickform='HH:MM:SS'; end
  for i=axs
    datetick(i,'x',tickform,'keeplimits')
  end
  xlabel('UNIVERSAL TIME')
end

drawnow
if REALT & ~isempty(Loc) & a_realtime & isunix
  if prod(get(0,'ScreenSize'))-1;
    flag='-dpng'; flag2='-r72';
  else
    flag='-dpng256'; flag2=[];
  end
  pngfile=sprintf('%sGup_%s.png',path_tmp,name_ant);
  print(gcf,flag,flag2,pngfile)
  [i,d]=unix('ps | grep curl | grep -v grep');
  if i
    unix(['curl -s -F file=@' pngfile ' "http://www.eiscat.com/raw/rtg/upload.cgi" >/dev/null &']);
  end
end
return

%%%%% surf_plot function %%%%%%%%%%
function [add_plot]=surf_plot(add_plot,action,n_tot,height,s,yparam,zparam,zscale,yscale,YTitle,Barlabel,CLPos,lg)

global axs Time
add_plot=add_plot+1;
if ~strcmp(action,'update')
  axe=subplot(n_tot+1,1,add_plot+1);
  set(axe,'Position',[0.12 0.06+(n_tot-add_plot)*(height+0.03) 0.8 height])
  axs=[axs axe];
else
  set(gcf,'currentaxes',axs(add_plot))
end
  
if strcmp(lg,'log')
  zparam(find(zparam<=0))=eps;
  zparam=log10(zparam); zscale=log10(zscale);
end
dy=diff(yparam)/2; yparam=[yparam-[dy(1,:);dy];yparam(end,:)+dy(end,:)];
zparam=[zparam;zparam(end,:)];
o2=ones(1,2);
for i=1:s(2)
  surface(Time(:,i),yparam(:,i)*o2,zparam(:,i)*o2)
end

if ~strcmp(action,'update')
  if ~isempty(zscale)
    set(gca,'CLim',zscale);
  end
  if ~isempty(yscale)
    set(gca,'YLim',yscale);
  end
  ylabel(YTitle)
  my_colorbar(lg)
  text('String',Barlabel,'Units','Normalized','Position',[CLPos 0.5],...
      'HorizontalAlignment','Center','Rotation',-90)
end
return

%%%%% line_plot function %%%%%%%%%%
function [add_plot]=line_plot(add_plot,action,n_tot,height,s,yparam,yscale,YTitle,Clabel,CLPos,lg)
global axs Time
add_plot=add_plot+1;
if ~strcmp(action,'update')
  axe=subplot(n_tot+1,1,add_plot+1);
  set(axe,'Position',[0.12 0.06+(n_tot-add_plot)*(height+0.03) 0.6964 height])
  axs=[axs axe];
else
  set(gcf,'currentaxes',axs(add_plot))
end

o2=ones(2,1);
if s(2)>1
 d1=1;
 d=[find(Time(1,2:end)-Time(2,1:end-1)>0) s(2)];
 nc=size(yparam,2);
 for i=1:length(d)
  line(col(Time(:,d1:d(i))),reshape(o2*row(yparam(d1:d(i),:)),2*(d(i)-d1+1),nc))
  d1=d(i)+1;
 end
else
 for i=1:s(2)
  line(Time(:,i),o2*yparam(i,:))
 end
end

if ~strcmp(action,'update')
  if strcmp(lg,'log')
    set(gca,'Yscale','log')
  end
  if ~isempty(yscale)
    set(gca,'YLim',yscale)
  end
  for j=1:size(yparam,2)
    ylabel(YTitle)
    if ~isempty(Clabel)
      color=get(gca,'ColorOrder');
      text('String',char(Clabel(j)),'Units','Normalized',...
          'Position',[1+.025*j 0.5],'HorizontalAlignment','Center',...
          'Rotation',-90,'Color',color(j,:))
    end
  end
end
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
