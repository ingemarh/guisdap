function [Time,Ran,Alt,Ne,Te,Ti,Vi,Comp,Res,Status,Az,El,Pt,Tsys]=...
        load_param(data_path,update)
% @(#)load_param.m	1.2 98/08/04
% Function to read the plasma parameters from
% GUISDAP result files
%
% 8/98 Modified to return NCAR convention velocity signs

global lastfile name_expr r_RECloc name_ant
if nargin<2, lastfile=0; end
data_path=fullfile(data_path,filesep);

% generate a filelist for the input directory in memory to avoid cases
% where the filelist.dat is not up to date.  Needs getfilelist.m and
% isdirectory.m in the path; if these are missing, use the following
% two lines instead:
% load([data_path, 'filelist.dat']);
% n = size(filelist);

filelist=getfilelist(data_path);
if nargin>1, filelist=filelist(filelist>lastfile); end

n=length(filelist);

n_tot=n;

%fprintf('\nSource directory: %s\n',data_path);
if n==0
  n_alt=0;
else
  lastfile=filelist(n);
  file=sprintf('%08d',filelist(1));

  load([data_path file])
  n_alt=size(r_param,1);
end
Ne	=ones(n_alt,n_tot)*NaN;
Ti	=Ne;
Te	=Ne;
Status	=Ne;
Vi	=Ne;
Time   	=zeros(2,n_tot);
Alt	=Ne;
Ran	=Ne;
Comp	=Ne;
Res	=Ne;
Az	=zeros(n_tot,1);
El	=Az;
Pt	=Az;
if exist('r_Tsys')
  Tsys	=Az;
else
  Tsys	=[];
end
for i=1:n_tot
  file=sprintf('%08d',filelist(i));
  
% fprintf('loading data from file %s.mat (%d/%d)\n',file,i,n(1))
  clear('r_*','name_*')
  load([data_path file])
  
  nalt=size(r_param,1);
  if nalt>n_alt
    nant=ones(nalt-n_alt,n_tot)*NaN;
    Ne		=[Ne;nant];
    Ti		=[Ti;nant];
    Te		=[Te;nant];
    Vi		=[Vi;nant];
    Alt		=[Alt;nant];
    Ran		=[Ran;nant];
    Res		=[Res;nant];
    Comp	=[Comp;nant];
    Status	=[Status;nant];
    n_alt=nalt;
  end
  n=1:nalt;
  Ne(n,i)   	=r_param(:,1);
  Ti(n,i)	=r_param(:,2);
  Te(n,i)	=r_param(:,2).*r_param(:,3);
  
  % if the data file contains NCAR velocities, use them, otherwise
  % invert the sign of the velocity since the GUISDAp convention is
  % opposite to that specified by NCAR
  
  if exist('r_NCARvel_PosAway')
    Vi(n,i)	=r_NCARvel_PosAway;
  else
    Vi(n,i)	=-r_param(:,5);
  end

  Alt(n,i) 	=r_h(:,1);
  Ran(n,i) 	=r_range(:,1);
  Res(n,i) 	=r_res(:,1);
  Comp(n,i) 	=r_dp(:,1);
  Time(1,i)	=datenum(r_time(1,:));
  Time(2,i)	=datenum(r_time(2,:));
  Status(n,i) 	=r_status(:,1);
  El(i)		=r_el;
  Az(i)		=r_az;
  Pt(i) 	=r_Pt/10000.0; %10 kW
  if ~isempty(Tsys), Tsys(i)=median(r_Tsys); end
end
% Cast azimuth and elevation into range 0-360, 0-90 degrees
d=find(El>90.1); El(d)=180-El(d); Az(d)=Az(d)+180;
Az=mod((Az+360),360);
d=find(El<90.1 & El>89.9); Az(d)=NaN;
%disp('Done.')
