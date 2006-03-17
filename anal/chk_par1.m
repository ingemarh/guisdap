% chk_par1: Interface betwen the user supplied and internal control parameters 
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
% 
% chk_par1 is the main interface between the user supplied control parameters and
% the internal GUISDAP control parameters. There is mostly a one-to-one correspondence
% between the parameters e.g.
% User parameter   GUISDAP parameter
% analysis_altit    a_altit
% analysis_integr   a_integr
% display_figures   di_figures
% integ_deffile     a_integdeffile
% The user parameters are local to the workspace whereas the GUISDAP parameters are global
% If any of the user parameters is not specified, or empty, the corresponding GUISDAP
% parameter will get the values specified in the routine.
%
% See also: an_start globals chk_par2

% make sure that the various path names have directory separators at the end
data_path=fullfile(data_path,filesep);
result_path=fullfile(result_path,filesep);
path_GUP=fullfile(path_GUP,filesep);
path_exps=fullfile(path_exps,filesep);
path_tmp=fullfile(path_tmp,filesep);
if ~isempty(d_saveintdir)
 d_saveintdir=fullfile(d_saveintdir,filesep);
 if ~exist(d_saveintdir,'dir')
  [i,j]=fileparts(d_saveintdir(1:end-1));
  if isempty(i), mkdir(j), else, mkdir(i,j), end
 end
end

% The first if-block tries to locate the data source and produces variables 
% necessary for integration:

a_simul=[];
if exist('analysis_simul')
 % The second branch is for simulated data to be calculated from the theory
 % The presence of the variable a_simul is the flag, which is used later
 % a_simul(1): integration time
 % a_simul(2): controls the start time in steps of 7200 s, i.e. 2 hours.
 % a_simul(3): The transmitter power
 % a_simul(4): The background temperature
 % a_simul(5:7): Antenna range, azimuth and elevation
 % a_simul(8): The calibration temperature
 a_simul=[0 1 1.2e6 100 300 180 90 210];
 a_simul(1:length(analysis_simul))=analysis_simul;
 a_year=2222;
 a_start=7200*a_simul(2);
 a_end=7200*a_simul(2)+a_simul(1);
 analysis_control(4)=2;
else
 if exist('analysis_integr'),
  a_integr=analysis_integr;
 else
  a_skip=zeros(size(a_integr));
 end
 if exist('analysis_realtime'),
  a_realtime=analysis_realtime;
 else
  a_realtime=0;
 end
 if exist('analysis_txlimit'),
  a_txlim=analysis_txlimit;
 else
  a_txlim=0;
 end
 if exist('analysis_skip'),
  a_skip=analysis_skip;
 else
  a_skip=zeros(size(a_integr));
 end
 if ~exist('recurse','var'), recurse=[]; end 

 if exist('PI_init')==3
  a_rawdata=1; % This flags that NW's package is to be used
  NW2
 else
  % The normal case where it is expeced the data are matlab files
  a_rawdata=0;
  if ~a_realtime
    recurse(strfind(recurse,'?'))='*';
  end
  i=strfind(recurse,'**');
  for i=fliplr(i)
    recurse(i+1)=[];
  end
  if isempty(dir([data_path '*.mat*']))
    data_path=fullfile(data_path,recurse,filesep);
  end
  [d_filelist,msg]=getfilelist(data_path);
  if ~isempty(msg)
   error(msg)
  end
  a_start=tosecs(analysis_start); % the programs uses internally only seconds,
  a_end  =tosecs(analysis_end);   % counted from the beginning of year
  a_year=analysis_start(1);
  a_ind=0;
 end
end

a_control=[1 0.01 100 1];
% a_control(1)  No fit is tried, if the error of Ne is larger than (1) at the start
% a_control(2)  Fitting is stopped when step for all parameters is less than (2)
% a_control(3)  Maximum number of iterations
% a_control(4)  Variance calculation
%               = 1 when variance estimated from data
%               = 2 when variance estimated using ambiguity functions
if exist('analysis_control')
 ind=find(analysis_control>0);
 a_control(ind)=analysis_control(ind);
end

a_Magic_const=1;
if exist('Magic_const')
 a_Magic_const=Magic_const;
end
a_NCAR=2;
if a_realtime & isunix & ~isempty(local.site)
 a_NCAR=3;
end
if exist('NCAR','var')
 NCAR_output
 a_NCAR=NCAR;
end
if exist('analysis_code','var')
 a_code=analysis_code;
end

di_figures=[0 0 0 0 0]; di_results=0;
if name_site=='S' | name_site=='K', di_results=1; end
if exist('display_figures'), 
 di_figures(1:length(display_figures))=display_figures;
end
if exist('display_results')
 di_results=display_results; 
end

a_save=1;
if exist('analysis_save','var')
 a_save=analysis_save;
 if ~a_save
  a_NCAR=0; di_figures(5)=0; result_path=path_tmp;
 end
end
a_savespec=0;
if exist('analysis_savespec','var')
 a_savespec=analysis_savespec;
end

a_do=1;
if exist('analysis_do','var')
 a_do=analysis_do;
end
a_intfixed=1;
if exist('analysis_intfixed','var')
 a_intfixed=analysis_intfixed;
end
if exist('analysis_intallow','var')
 a_intallow=analysis_intallow;
end
if exist('analysis_txpower','var')
 a_txpower=analysis_txpower;
elseif name_site~='V' & a_year>2004
 a_txpower=[65 1000];
 fprintf('Using new measured tx power, override with analysis_txpower=8;\n')
else
 a_txpower=[8 1];
end
if length(a_txpower)==1
 if a_txpower(1)==65, a_txpower(2)=1e3; else, a_txpower(2)=1; end
end
sysTemp=[];
if exist('analysis_Tsys','var')
 sysTemp=analysis_Tsys;
end
a_rcprog=1;
if exist('analysis_rcprog','var')
 a_rcprog=analysis_rcprog;
end
