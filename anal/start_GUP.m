% GUISDAP v1.80
%
% The startup file for GUISDAP. 
% Defines the GUP version number and certain global variables
% namely:   GUP_ver path_GUP path_exps path_tmp name_expr name_site data_path result_path 
% If you want this to executed every time you invoke matlab,
% add reference to start_GUP to your personal startup file

clear all, clear global

global GUP_ver path_GUP path_exps path_tmp name_expr name_site data_path result_path local
GUP_ver=8.4;
fprintf('GUISDAP vs. %g by EISCAT, Lehtinen&Huuskonen\n',GUP_ver)
if exist('mrqmndiag')~=3
 fprintf('***** using no mex routine enhancements *****\n')
end

path_GUP=which('start_GUP','-all');
if iscell(path_GUP), path_GUP=char(path_GUP(end)); end
path_GUP=fileparts(fileparts(path_GUP));
path_tmp=tempdir;
path_exps=fullfile(path_GUP,'exps',filesep);
result_path=fullfile(filesep,'analysis','results',filesep);
data_path=fullfile(filesep,'data',filesep);

format compact
format short g
set(0,'defaultaxesxminortick','on')
set(0,'defaultaxesyminortick','on')
set(0,'defaultfigurenumbertitle','off')
set(0,'defaultfigureSelectionHighlight','off')
set(0,'defaultfiguremenubar','none')
set(0,'defaultfiguretoolbar','figure')
set(0,'defaultuicontrolfontsize',10)
set(0,'defaultaxesfontsize',12)
set(0,'defaulttextfontsize',12)
set(0,'defaultAxesColorOrder',[1 0 0;0 1 0;0 0 1;0 0 0;1 0 1;0 1 1;1 1 0])
set(0,'defaultFigurePaperType','A4')
set(0,'defaultFigurePaperUnits','centimeters')
set(0,'defaultaxescolor','none')

%localities
local.printer='color';
local.site=getenv('EISCATSITE');
local.host=getenv('HOSTNAME');
local.browser='netscape';
local.matlabversion=str2num(version('-release'));
local.tfile=tempname;
switch local.site
 case 'K'
  data_path='/data1/';
  local.browser='mozilla';
 case 'S'
  data_path='/data1/';
  local.printer='phaser';
 case 'T'
  local.printer='hp4550';
 case 'L'
  data_path='/data2/';
 case 'HQ'
  local.browser='mozilla';
  local.printer='hpclj';
 otherwise
  local.site=[]; local.browser=[];
  if ~isempty(getenv('LPDEST'))
   local.printer=getenv('LPDEST');
  end
  if ~isempty(getenv('BROWSER'))
   local.browser=getenv('BROWSER');
  end
end
if ~exist(data_path,'dir'), data_path=path_tmp; end
if ~exist(result_path,'dir'), result_path=path_tmp; end
