% GUISDAP v1.80
%
% The startup file for GUISDAP. 
% Defines the GUP version number and certain global variables
% namely:   GUP_ver path_GUP path_exps path_tmp name_expr name_site data_path result_path 
% If you want this to executed every time you invoke matlab,
% add reference to start_GUP to your personal startup file

clear all, clear global, clear functions

global GUP_ver path_GUP path_exps path_tmp name_expr name_site data_path result_path local

path_GUP=which('start_GUP','-all');
if iscell(path_GUP), path_GUP=char(path_GUP(end)); end
path_GUP=fileparts(fileparts(path_GUP));
path_tmp=tempdir;
path_exps=fullfile(path_GUP,'exps',filesep);
result_path=fullfile(filesep,'analysis','results',filesep);
data_path=fullfile(filesep,'data',filesep);
d=1;
if 0 & isunix
 [d,GUP_ver]=system(sprintf('echo -n `git -C %s describe --tags --long`-`git -C %s log --oneline | wc -l` 2>/dev/null',path_GUP,path_GUP));
end
if d
 GUP_ver=char(textread(fullfile(path_GUP,'.version'),'%s'));
end

fprintf('GUISDAP vs. %s by EISCAT, Lehtinen&Huuskonen\n',GUP_ver)

format compact
format short g
set(0,'DefaultAxesxMinortick','on')
set(0,'DefaultAxesyMinortick','on')
set(0,'DefaultFigureNumberTitle','off')
set(0,'DefaultFigureSelectionHighlight','off')
set(0,'DefaultFigureMenuBar','none')
set(0,'DefaultFigureToolBar','figure')
set(0,'DefaultUicontrolFontSize',10)
set(0,'DefaultAxesFontSize',12)
set(0,'DefaultTextFontSize',12)
set(0,'DefaultAxesFontName','Helvetica')
set(0,'DefaultTextFontName','Helvetica')
set(0,'DefaultAxesColorOrder',[1 0 0;0 1 0;0 0 1;0 0 0;1 0 1;0 1 1;1 1 0])
set(0,'DefaultFigurePaperType','A4')
set(0,'DefaultFigurePaperUnits','centimeters')
set(0,'DefaultFigureRenderer','painters')
%set(0,'DefaultAxesColor','none')
warning off backtrace

%localities
local.printer='color';
local.site=getenv('EISCATSITE');
local.host=getenv('HOSTNAME'); if isempty(local.host), local.host=getenv('COMPUTERNAME'); end
local.user=getenv('USER'); if isempty(local.user), local.user=getenv('USERNAME'); end
local.browser='firefox';
local.x=prod(get(0,'ScreenSize'))-1;
local.x=~usejava('jvm') || feature('ShowFigureWindows');
matver=ver('matlab');
if isempty(matver) || isempty(matver.Name)
 local.matlabversion=4;
else
 matver=matver.Version;
 d=strfind(matver,'.'); matver(d(2:end))=[];
 local.matlabversion=str2num(matver);
end
d=computer;
if ~usejava('jvm') && local.matlabversion>7.4
  set(0,'DefaultAxesButtonDownFcn','zoom')
end
if local.matlabversion>8.4
 local.host=char(getHostName(java.net.InetAddress.getLocalHost));
 set(groot,'defaultAxesTitleFontSizeMultiplier',1)
 set(groot,'defaultAxesLabelFontSizeMultiplier',1)
 set(groot,'defaultAxesTitleFontWeight','normal')
end
if local.matlabversion>7.6 && local.matlabversion<7.9 && ~isempty(strfind(d,'64')) && ~strcmp(d,'SOL64')
 disp('Matlab-EISCAT fault detected'), quit
end
clear matver d
if ~isfield(local,'tfile'), local.tfile=tempname; end
switch local.site
 case 'K'
  data_path='/data1/';
 case 'S'
  data_path='/data1/';
 case 'T'
  data_path='/data/';
 case 'L'
  data_path='/data/';
 otherwise
  local.site=[]; local.browser=[];
end
if ~isempty(getenv('LPDEST'))
 local.printer=getenv('LPDEST');
end
if ~isempty(getenv('BROWSER'))
 local.browser=getenv('BROWSER');
end
if ~exist(data_path,'dir'), data_path=path_tmp; end
if ~exist(result_path,'dir'), result_path=path_tmp; end
if ~setuplibs
 fprintf('***** using no compiled library enhancements *****\n')
elseif libisloaded('onera_desp_lib') 
 addpath(fullfile(path_GUP,'models','irbem-code','matlab'))
 onera_desp_lib_get_mlt(now,[60 20 0]);
end

warning('off','MATLAB:connector:connector:ConnectorNotRunning')
feature('DefaultCharacterSet','UTF-8');
