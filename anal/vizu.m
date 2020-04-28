function [varargout]=vizu(varargin)
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

global Time par2D par1D rpar2D name_expr name_ant name_strategy axs axc maxdy rres
persistent hds OLD_WHICH
global height n_tot add_plot manylim plf_polen max_ppw vizufig
global DATA_PATH LOCATION START_TIME END_TIME MESSAGE1 Y_TYPE
global r_RECloc path_tmp path_GUP result_path webfile local owner allnames
nvargin=length(varargin);
naction=1;
if strcmp(action(naction,nvargin,varargin),'myb')
  varargout(1)={myb(gcf,action(naction+1,nvargin,varargin),action(naction+2,nvargin,varargin))};
  return
end
vizufig=findobj('type','figure','userdata',6);
if strcmp(action(naction,nvargin,varargin),'new')
  naction=naction+1;
  if ~isempty(vizufig)
    close(vizufig), vizufig=[];
  end
end
if isempty(vizufig)
  axs=[]; axc=[];
  Time=[]; DATA_PATH=[]; START_TIME=[]; MESSAGE1=[]; OLD_WHICH=[]; allnames=[];
end
REALT=0; manylim=1;
Loc=local.site;
if strcmp(action(naction,nvargin,varargin),'rtgup')
  global a_year a_start a_end a_realtime a_autodir
  if isempty(START_TIME)
    START_TIME=toYMDHMS(a_year,a_start);
    END_TIME=toYMDHMS(a_year,a_end);
    if isstruct(a_autodir) && any([START_TIME(1:3)~=END_TIME(1:3) ~a_realtime naction>1])
      START_TIME=[a_autodir.date 0 0 0];
      END_TIME=START_TIME+[0 0 0 24 0 0];
    end
    if ~isempty(vizufig)
      close(vizufig), vizufig=[]; axs=[]; axc=[];
    end
    naction=nvargin+1; OLD_WHICH=[];
  else
    varargin{naction}='update';
  end
  DATA_PATH=result_path; MESSAGE1='RT';
  if ~isempty(owner), MESSAGE1=owner; end
  REALT=1; manylim=Inf;
end

PLOT_STATUS=[0 Inf];	% Status (0-3) and Residual limit
if isempty(DATA_PATH)
  act=action(naction,nvargin,varargin);
  if strcmp(lower(act),'verbose')
    DATA_PATH=minput('Data path',result_path,1);
    if strcmp(act,'VERBOSE')
      PLOT_STATUS=minput('Status/Residual limits',PLOT_STATUS);
      max_ppw=minput('Maximum pp resolution (km)',Inf);
    end
  elseif ~REALT
    if isdir(act) | strfind(act,filesep)
      DATA_PATH=act; naction=naction+1;
      MESSAGE1=action(naction,nvargin,varargin); naction=naction+1;
    else
      disp(['Available data directories in ' result_path]);
      dirs=dir(result_path);
      for i=1:length(dirs), if dirs(i).isdir && dirs(i).name(1)~='.'
        disp(dirs(i).name);
      end, end
      DATA_PATH=sprintf('%s%s',result_path,minput('Data directory',' ',1));
    end
  end
end

act=action(naction,nvargin,varargin);
a2=action(naction+1,nvargin,varargin);
a3=action(naction+2,nvargin,varargin);
if isempty(act) || strcmp(lower(act),'verbose')
 if isempty(Time)
  [Time,par2D,par1D,rpar2D]=load_param(DATA_PATH,PLOT_STATUS);
 end
 if ~isempty(axs), delete(axs), axs=[]; end
 if ~isempty(axc), delete(axc), axc=[]; end
 maxdy	=Inf;		% Max diff in y stretch data points
elseif strcmp(act,'update')
 [Time,par2D,par1D,rpar2D]=load_param(DATA_PATH,PLOT_STATUS,1);
 set(0,'currentfig',vizufig)
elseif strcmpi(act,'print') || strcmpi(act,'save')
 fig=sprintf('%d-%02d-%02d_%s',START_TIME(1:3),name_expr);
 if ~isempty(name_strategy), fig=sprintf('%s_%s',fig,name_strategy); end
 if ~isdir(DATA_PATH), dirs=path_tmp(1:end-1); else, dirs=DATA_PATH; end
 if ~isempty(a2), fig=sprintf('%s_%s',fig,a2); end
 fig=sprintf('%s@%s',fig,name_ant);
 file=fullfile(dirs,fig);
 if isempty(axs)
  disp('Nothing to print!')
 elseif isunix
  if strcmpi(act,'print')
    [dirs,fig]=fileparts(local.tfile); ext='ps';
  else
    ext='eps';
  end
  rotate=0; pngor='580x820'; if length(axs)==1, rotate=1; end
  if [strfind(act,'R') strfind(act,'V')], rotate=1-rotate; end
  if rotate
   ppos=get(vizufig,'PaperPosition'); s=3.4;
   set(vizufig,'PaperOrient','landscape','PaperPosition',ppos([2 1 4 3])+[0 s 0 -s])
   pngor='820x580';
  end
  %[i,j]=unix('which addlogo.sh');
  %i=i || local.matlabversion>8.3;
  %if ~i, set(hds(2:3),'visible','off'), end
  file=fullfile(dirs,fig);
  print(vizufig,['-d' ext 'c'],[file '.' ext]);
  if rotate
   set(vizufig,'PaperOrient','portrait','PaperPosition',ppos)
  end
  %if ~i
  % set(hds(2:3),'visible','on')
  % unix(sprintf('addlogo.sh %s %s %s >/dev/null 2>&1',dirs,fig,ext));
  %end
  if strcmpi(act,'print') || strcmpi(a3,'print')
    if isempty(a2), dev=local.printer; else, dev=a2; end
    unix(['lp -c -d' dev ' ' file '.' ext ' >/dev/null 2>&1']);
    if strcmpi(act,'print'), delete([file '.' ext]), end
  end
  if strcmpi(act,'save')
    gd=fullfile(matlabroot,'sys','ghostscript',filesep);
    gsbin=fullfile(gd,'bin',lower(computer),'gs');
    gsinc=sprintf('-I%sps_files -I%sfonts',gd,gd);
    if ~exist(gsbin,'file'), gsbin='gs'; gsinc=[]; end
    unix(sprintf('%s %s -dNOPAUSE -dFitPage -q -sDEVICE=pdfwrite -sOutputFile=%s.pdf %s.%s </dev/null >/dev/null',gsbin,gsinc,file,file,ext));
    unix(sprintf('%s %s -dNOPAUSE -dFitPage -q -sDEVICE=png256 -sOutputFile=%s.png %s.%s </dev/null >/dev/null',gsbin,gsinc,file,file,ext));
    delete([file '.' ext])
    fprintf('Created %s.pdf and .png\n',file)
    insert_exif(vizufig,file,{'pdf' 'png'})
  end
 elseif strcmpi(act,'print')
  print(vizufig,'-dwinc');
 else
  print(vizufig,'-dpdf',[file '.pdf']);
  print(vizufig,'-dpng256',[file '.png']);
  fprintf('Created %s.pdf and .png\n',file)
  insert_exif(vizufig,file,{'pdf' 'png'})
 end
 if strcmp(act,'save') && nargout==1
  varargout(1)={file};
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
minus='ᐨ';plus='ᐩ';three='³';one='¹';two='´';degree='°';esub='ₑ';mu='µ';
minus='¯';plus='^+';esub='_e'; %keep it to 8bit chars
%minus=char(5160);plus=char(5161);three=char(179);one=char(185);two=char(178);degree=char(176);esub=char(8337);mu=char(181);
%minus=char(175);
%minus='^-'; plus=^+;three='^3';one='^1';two='^2';degree='\circ';esub='_e';mu='\mu';
TITLE={'Range (km)','Altitude (km)',['Electron Density (m' minus three ')'],...
	'Electron Temperature (K)','Ion Temperature (K)',...
	['Ion Drift Velocity (ms' minus one ')'],'Collision frequency (Hz)',...
	['Ion Composition (O' plus '/N' esub ')'],'Residual'};
TITLE1={['Azimuth(' degree ')'],['Elevation(' degree ')'],'Power (10kW)',...
	'System Temperature (K)' 'Phasepushing (Hz)'};
SCALE =[50 900		% Range km
	80 600		% Altitude km
	10.^[10 12]	% Ne m-3
	0 4000		% Te K
	0 3000		% Ti K
	-200 200	% Vi m/s
	10.^[0 5]	% collision freq Hz
	0 1		% Comp
	.1 10]; 	% Res
Y_TYPE1	='linear';	% Y scale type
PLF_SCALE	=[0 10];	% Langmuir freq MHz
TEC_SCALE	=[0 10];	% TEC scale
RAWNE_SCALE	=10.^[9 12];	% Raw Ne
LAT_SCALE	=[65 78];	% Geogr Lat deg
AE_SCALE	=[-30 360];	% Radar parms
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
elseif strfind('dlayer cp6b',name_expr)
 SCALE(2:3,:)=[59 121;10.^[9 12]]; WHICH_PARAM='Ne AE'; 
elseif strcmp(name_expr,'manda')
 SCALE(2:3,:)=[50 500;10.^[9 12]]; WHICH_PARAM='Ne AE'; Y_TYPE1='log';
 if isempty(rres), maxdy=60; end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if ~isempty(a2)
 SCALE(2,:)=a2;
end
if ~exist('GATES')
 GATES=1:size(par2D,1);
end
if length(GATES)==1
 WHICH_PARAM='Ne TT Vi LL';
end
if ~isempty(a3)
 WHICH_PARAM=a3;
elseif ~isempty(OLD_WHICH)
 WHICH_PARAM=OLD_WHICH;
end
%%%%%%%%%% Time scales %%%%%%%%%%%%%
if strcmp(lower(act),'verbose')
 if isempty(START_TIME)
  START_TIME=floor(datevec(Time(1,1)));
  END_TIME=ceil(datevec(Time(2,end)));
  Y_TYPE=Y_TYPE1;
 end
 START_TIME=minput('Start time',START_TIME);
 END_TIME=minput('  End time',END_TIME);
 if isempty(a2) && length(GATES)>1
  SCALE(2,:)=minput('Altitude scale',10*[floor(min(par2D(GATES(1),:,2))/10) ceil(max(par2D(GATES(end),:,2))/10)]);
 end
 if isempty(a3)
  disp('Parameters: Ne Te Ti Vi AE TT LL Rs O+ Co Nr Lf L1 Ls Pf P1 TC')
  WHICH_PARAM=minput('Choose',WHICH_PARAM,1);
 end
elseif ~REALT
 if strcmp(DATA_PATH(end),filesep)
  DATA_PATH=DATA_PATH(1:end-1);
 end
 [d,dpath]=fileparts(DATA_PATH);
 START_TIME=datevec(median(Time(:))); START_TIME(4:6)=[0 0 0];
 END_TIME=START_TIME+[0 0 0 24 0 0];
end
if strcmp(lower(act),'update') && ~isempty(OLD_WHICH)
 WHICH_PARAM=OLD_WHICH;
end
OLD_WHICH=WHICH_PARAM;
nameant=lower(name_ant(1:3));
if strcmp(nameant,'32m') || strcmp(nameant,'42m') || strcmp(nameant,'esr')
 FIGURE_TITLE='EISCAT SVALBARD RADAR';
 stretchSecs=65;
 fradar=500e6;
elseif strcmp(nameant,'uhf')
 FIGURE_TITLE='EISCAT UHF RADAR';
 fradar=930e6;
elseif strcmp(nameant,'vhf')
 FIGURE_TITLE='EISCAT VHF RADAR';
 fradar=224e6;
elseif strcmp(nameant,'kir') || strcmp(nameant,'sod')
 if START_TIME(1)>2012
  FIGURE_TITLE='EISCAT VHF RADAR';
  fradar=224e6;
 else
  FIGURE_TITLE='EISCAT UHF RADAR';
  fradar=930e6;
 end
elseif strcmp(nameant,'quj')
 FIGURE_TITLE='QUJING RADAR';
 fradar=500e6;
 stretchSecs=200;
end
if ~strcmp(nameant,'quj') && isempty(MESSAGE1)
 MESSAGE1=minput('Type of experiment','CP',1);
end
S=size(par2D,2);
s=1:S;
if strcmp(act,'VERBOSE')
 GATES=minput('Gates',GATES);
 if length(GATES)>1
  Y_PARAM=minput('Y parameter (Ran-1 Alt-2 Lat-3)',Y_PARAM);
  if Y_PARAM==3
   LAT_SCALE=minput('Scale (lat)',LAT_SCALE);
  end
  Y_TYPE=minput('Y scale type',Y_TYPE,1);
  screen=[-Inf Inf -Inf Inf];
  screen=minput('AzEl screen values',screen);
  s=find(par1D(s,1)>screen(1) & par1D(s,1)<screen(2) & par1D(s,2)>screen(3) & par1D(s,2)<screen(4))';
 end
 SCALE=minput('Scales (Ran Alt Ne Te Ti Vi Coll Comp Res)',SCALE')';
 if [findstr(WHICH_PARAM,'P') findstr(WHICH_PARAM,'L')]
  PLF_SCALE=minput('Scale (plf)',PLF_SCALE);
 end
 if findstr(WHICH_PARAM,'Nr')
  RAWNE_SCALE=minput('Scale (rawNe)',RAWNE_SCALE);
 end
 if findstr(WHICH_PARAM,'AE')
  AE_SCALE=minput('Scale (AzEl)',AE_SCALE);
 end
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
if findstr(WHICH_PARAM,'TC'), option(16)=option(16)+4; end
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
if isempty(vizufig)
 vizufig=gupfigure;
 if ~local.x && round(10*local.matlabversion)==65
  close(vizufig),vizufig=gupfigure; % Matlab R13 bug
 end
 sdir=DATA_PATH; if strcmp(sdir,'.'), sdir=pwd; end
 set(vizufig,'Position',[400 30 587 807],'DefaultAxesFontSize',FS,...
   'DefaultAxesTickDir','out','DefaultTextFontSize',FS,'UserData',6,...
   'DefaultAxesXMinorTick','on','defaultaxesbox','on',...
   'DefaultAxesTickLength',TL,...
   'renderer','painters','PaperPosition',[0.4 0.7 20.65 28.4],...
   'DefaultAxeslayer','top','DefaultsurfaceEdgeColor','none',...
   'DefaultTextHorizontalAlignment','center',...
   'Name',['GUISDAP results from ',sdir])
% set(gcf,'PaperType','A4','PaperUnits','centimeters','NumberTitle','off',...
%  'defaultAxesColorOrder',[1 0 0;0 1 0;0 0 1;0 0 0;1 0 1;0 1 1;1 1 0])
 uimenu('label','Update','callback','vizu(''update'')');
 if isunix
  uimenu('label','Save','callback','vizu(''save'')');
  uimenu('label','Print','callback','vizu(''print'')');
 end

 ti=axes(vizufig,'Position',[0.0 0.87 0.95 0.08],'Visible','off');
 text(ti,'Position',[0.55,0.80],'VerticalAlignment','top', ...
      'String',FIGURE_TITLE,'FontSize',16,'FontWeight','bold','UserData','Radar')
 hds=text(ti,'Position',[0.55,0.4],'VerticalAlignment','top',...
      'String',t2,'FontSize',12,'Interpreter','none','UserData','Experiment');
 text(ti,'Position',[0.125,0.12],'HorizontalAlignment','left',...
      'Color',[.5 .5 .5],'VerticalAlignment','top','String',t1,'UserData','Computer')
 text(ti,'Position',[0.86,0.12],'HorizontalAlignment','right',...
      'Color',[.5 .5 .5],'VerticalAlignment','top','String',MESSAGE2)
 axs=[]; axc=[];
 %logo...
 if ~strcmp(nameant,'quj')
  text(ti,'Position',[0.55,1.3],'VerticalAlignment','top','FontSize',24,...
      'FontWeight','bold','String','EISCAT Scientific Association','UserData','Copyright');
 %load(fullfile(path_GUP,'matfiles','logo'))
  eiscatlogo(axes(vizufig,'Position',[.07 .89 .1 .075]),.8)
 %axes('Position',[.07 .9 .1 .075]); plot(y,x,'.k')
 %hds(3)=get(gca,'children'); set(hds(3),'markersize',1)
 %set(gca,'xlim',[0 202],'ylim',[0 202],'visible','off')
 end
else
 set(hds,'string',t2)
 ti=get(hds,'Parent');
end
text(ti,0,0,WHICH_PARAM,'Visible','off','UserData','Results')
add_plot=0;
height(1)=(0.80-(n_tot-1)*height(2))/n_tot;
colormap(vizufig,myb(vizufig,78,1));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine the Y-Axis parameter
if Y_PARAM<3
 YTitle=TITLE(Y_PARAM);
 y_param=par2D(:,:,Y_PARAM);
 Yscale=SCALE(Y_PARAM,:);
 if isinf(maxdy) && ~isempty(rres), maxdy=rres; end
elseif Y_PARAM==3
 YTitle=['Latitude (' degree 'N)'];
 y_param=par2D(:,:,1);
 ll=par1D(:,[2 1]);
 for i=1:size(par2D,2),for j=1:size(y_param,1)
  d=loc2gg(r_RECloc,[ll(i,:) y_param(j,i)]); y_param(j,i)=d(1);
 end,end
 Yscale=LAT_SCALE;
end
if stretchSecs && length(s)>1
 dt=Time(1,s(2:end))-Time(2,s(1:end-1));
 if stretchSecs==1
  d=find(dt>0 & dt<=median(dt)+1/86399);
 else
  d=find(dt>0 & dt<=stretchSecs/86399);
 end
 if ~isempty(d)
  Time(2,s(d))=Time(2,s(d))+dt(d)/2;
  Time(1,s(d+1))=Time(2,s(d));
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
  lf=find_plf_peak(S,y_param(GATES,:),Yscale,lf,16,PLF_SCALE,fix(option(14)/2)-1);
  if fix(option(14)/2)-1
   surf_plot(s,lf{2}'*ones(1,S),lf{3},[0 max(max(lf{3}))],PLF_SCALE,'Plasma line spectrum (MHz)','Power (any unit)',[])
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
  polen=plf_polen; if isempty(polen), polen=7; end
  [pf,d,TC]=find_plf_peak(S,y_param(GATES,:),Yscale,pf,polen,PLF_SCALE,0);
  if rem(option(16),4)>1
   d=many(pf,PLF_SCALE);
   line_plot(s,pf,d,'Cutoff plasma frequency (MHz)',[],[])
  end
  if option(16)>4
   d=many(TC,TEC_SCALE);
   line_plot(s,TC,d,['Total electron content (m' minus two ')'],[],[])
  end
 end
end
if option(15)
 surf_plot(s,rpar2D(:,:,Y_PARAM),rpar2D(:,:,3),RAWNE_SCALE,Yscale,YTitle,['Raw electron density (m' minus three ')'],'log')
end
if option(11)
 if size(par1D,2)>4
  ae=par1D(:,[3 1 2 4 5]);
 else
  ae=par1D(:,[3 1 2:2:end]);
 end
 d=find(ae(:,3)<90.1 & ae(:,3)>89.9); ae(d,2)=NaN;
 d=many(ae,AE_SCALE);
 line_plot(s,ae,d,'Radar parameters',TITLE1([3 1 2 4 5]),[])
end
if option(13)
 ll=[par1D(:,[3 2 1]) par2D(GATES,:,1)'];
 for i=1:size(par2D,2), ll(i,2:4)=loc2gg(r_RECloc,ll(i,2:4)); end
 if size(par1D,2)>3, ll=[ll par1D(:,4:end)]; end
 d=many(ll,[0 300])+[-10 10];
 line_plot(s,ll,d,'Radar parameters',[TITLE1(3) {['Latitude (' degree 'N)'],['Longitude (' degree 'E)']} TITLE(2) TITLE1(4) {['Offset (' mu 's)']}],[])
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xlabel('UNIVERSAL TIME')
drawnow
if REALT && ~isempty(Loc) && a_realtime && isunix
 if local.x
  flag='-dpng'; flag2='-r72';
 else
  flag='-dpng256'; flag2=[];
 end
 pngfile=sprintf('%sGup_%s.png',path_tmp,name_ant(1:3));
 print(vizufig,flag,flag2,pngfile)
 webfile(1)=cellstr(pngfile);
end
if nargout
 i=0; j=1; argout=str2mat('yy','lf','pf','ll','xx'); xx=[];
 while i<nargout
  i=i+1;
  if j<5, j=j+1; end
  while ~exist(argout(j,:),'var') && j<5
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
global Time axs axc add_plot maxdy Y_TYPE
add_plot=add_plot+1;
if length(axs)<add_plot
 ax=setup_axes(yscale,YTitle);
 set(ax,'UserData',zscale)
else
 ax=axs(add_plot);
 %set(gcf,'currentaxes',ax)
 zscale=get(ax,'UserData');
end
  
if strcmp(lg,'log')
 zparam(find(zparam<=0))=eps;
 zparam=log10(zparam); zscale=log10(zscale);
end
o2=ones(1,2);
for i=s
 d=max([2;find(isfinite(yparam(:,i)))]);
 if d
  yp=yparam(1:d,i);
  zp=zparam(1:d,i);
  dy=diff(yp); d=find(dy>maxdy); dd=find(dy<=maxdy);
  for j=flipud(d)'
   [dum,jj]=min(abs(dd-j)); jj=find(abs(dd-j)==dum);
   yp(j+1:end+2)=[yp(j:j+1)+dy(dd(jj)).*[1;-1];yp(j+1:end)];
   zp(j+1:end+2)=[NaN;NaN;zp(j+1:end)];
  end
  dy=diff(yp)/2;
  yp=[yp-[dy(1);dy];yp(end)+dy(end)];
  d=find(isnan(zp)); jp=0;
  for j=[d' length(zp)+1]
   jj=(jp+1):(j-1); jp=j; ljj=length(jj);
   if ljj
    surface(ax,ones(ljj+1,1)*Time(:,i)',yp([jj jp])*o2,zp([jj jp-1])*o2)
   end
  end
 end
end
if strcmp(Y_TYPE,'log')
 ny=length(get(ax,'ytick'));
 set(ax,'yscale',Y_TYPE)
 if length(get(ax,'ytick'))<ny
  yl=log10(get(ax,'ylim'));
  yl=logspace(yl(1),yl(2),round(ny*1.3));
  yll=10.^floor(log10(yl));
  yl=round(yl./yll).*yll;
  set(ax,'ytick',unique(yl))
 end
end

if length(axc)<add_plot
 if ~isempty(zscale)
  set(ax,'CLim',zscale+5*eps*abs(zscale).*[-1 1]);
 end
 axc=[axc my_colorbar(Barlabel,lg)];
end
return

%%%%% line_plot function %%%%%%%%%%
function line_plot(s,yparam,yscale,YTitle,Clabel,lg)
global Time axs add_plot
add_plot=add_plot+1;
if length(axs)<add_plot
 ax=setup_axes(yscale,YTitle);
 set(ax,'UserData',yscale)
 if strcmp(lg,'log')
  set(ax,'Yscale','log')
 end
 jj=0;
 for j=1:size(yparam,2)
  if ~isempty(Clabel) && any(isfinite(yparam(:,j)))
   color=get(ax,'ColorOrder'); jj=jj+1;
   text(ax,'String',char(Clabel(j)),'Units','Normalized',...
        'Position',[1+.025*jj 0.5],'Rotation',-90,'Color',color(j,:))
  end
 end
else
 ax=axs(add_plot);
 %set(gcf,'currentaxes',axs(add_plot))
 yscale=get(ax,'UserData');
end

o2=ones(2,1);
if length(s)>1
 d1=1;
 d=[find(Time(1,s(2:end))-Time(2,s(1:end-1))>0) length(s)];
 nc=size(yparam,2);
 for i=1:length(d)
  line(ax,col(Time(:,s(d1:d(i)))),reshape(o2*row(yparam(s(d1:d(i)),:)),2*(d(i)-d1+1),nc))
  d1=d(i)+1;
 end
else
 line(ax,Time(:,s),o2*yparam(s,:))
end
return

function ax=setup_axes(yscale,YTitle)
global axs height n_tot START_TIME END_TIME add_plot vizufig
ax=axes(vizufig,'Position',[.12 .06+(n_tot-add_plot)*sum(height) .7 height(1)]);
axs=[axs ax];
set(ax,'xgrid','on','ygrid','on')
mydatetick(ax,[datenum(START_TIME) datenum(END_TIME)],length(axs)==1)
if ~isempty(yscale)
 set(ax,'YLim',yscale+5*eps*abs(yscale).*[-1 1])
end
ylabel(ax,YTitle)
return

function f=myb(vizufig,nl,cut)
% create palette with nl levels
if nargin<2, cut=[]; end
if nargin==0, nl=[]; end
if isempty(cut), cut=0; end
if isempty(nl)
 nl=size(get(vizufig,'colormap'),1);
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
if llx>p(1) && p(1)>0 && x(p(1))<l(1)
 l(1)=x(p(1));
end
if llx>p(2) && p(2)>0 && x(p(2))>l(2)
 l(2)=x(p(2));
end

function comm=action(naction,narg,vizuarg)
comm=[];
if naction<=narg
 comm=vizuarg{naction};
end
