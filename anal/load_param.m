function [Time,par2D,par1D,rpar2D]=load_param(data_path,status,update)
% Function to read the plasma parameters from GUISDAP result files
%
% [Time,par2D,par1D,rpar2D]=load_param(data_path,update)
% par2D [Ran,Alt,Ne,Te,Ti,Vi,Coll,Comp,Res]
% par1D [Az,El,Pt,Tsys,Oppd]
% rpar2D[Ran,Alt,RawNe]

global lastfile name_expr r_RECloc name_ant
if nargin<3, lastfile=0; end
if nargin<2, status=[]; end
if isempty(status), status=0; end

if ~isdir(data_path)
  [Time,par2D,par1D,rpar2D]=load_param_madrigal(data_path);
  return
end
data_path=fullfile(data_path,filesep);
list=getfilelist(data_path);
if nargin>2, list=list(cell2mat({list.file})>lastfile); end
n=length(list);
ppres=.25; % pp resolution (km)

%fprintf('\nSource directory: %s\n',data_path);
if n==0
  Time=[]; par2D=[]; par1D=[]; rpar2D=[]; return
end
r_Tsys=[]; r_pp=[];
n_tot=n;
lastfile=list(n).file;

load(canon(fullfile(list(1).dir,sprintf('%08d%s',list(1).file,list(1).ext)),0))
n_alt=size(r_param,1);

npar2D=9; nrpar2D=3;
par2D	=ones(n_alt,n_tot,npar2D)*NaN;
Time   	=zeros(2,n_tot);
if isempty('r_Tsys') | ~exist('r_Offsetppd')
  r_Offsetppd=[];
end
par1D=zeros(n_tot,length([r_az r_el r_Pt/10000 median(r_Tsys) r_Offsetppd]));
if nargout>3 & strfind('TVL',name_site) & ~isempty(r_pp)
  n_ralt=length(unique(round(r_pprange/ppres)));
  rpar2D=ones(n_ralt,n_tot,3)*NaN;
  re=6370;
else
  n_ralt=NaN;
  rpar2D=[];
end
if isempty(name_ant)
  antennas=['uhf kir sod vhf esr'];
  i=(strfind('TKSVL',name_site)-1)*4+1;
  name_ant=antennas(i:i+2);
end
for i=1:n_tot
% clear('r_*','name_*')
  load(canon(fullfile(list(i).dir,sprintf('%08d%s',list(i).file,list(i).ext)),0))
  
  nalt=size(r_param,1);
  if nalt>n_alt
    par2D=[par2D;ones(nalt-n_alt,n_tot,npar2D)*NaN];
    n_alt=nalt;
  end
  n=1:nalt;
  d=find(r_status>status); [d1,d2]=find(r_error(d,1:5)>0);
  r_param(d(d1),d2)=NaN;
% r_param=r_apriori;
  par2D(n,i,[1 2 8 9])=[r_range(:,1) r_h(:,1) r_dp(:,1) r_res(:,1)];
  par2D(n,i,[3 5 7])=r_param(:,[1 2 4]);
  par2D(n,i,[4 6])=[r_param(:,2).*r_param(:,3) -r_param(:,5)];
  Time(:,i)	=datenum(r_time);
  par1D(i,:)	=[r_az r_el r_Pt/10000 median(r_Tsys) r_Offsetppd]; %10 kW
  if isfinite(n_ralt)
    [ppr,k,l]=unique(round(r_pprange/ppres)); pp=ppr;
    nalt=length(ppr);
    for n=1:nalt
      d=find(l==n);
      ppr(n)=mean(r_pprange(d));
      pp(n)=mean(r_pp(d));
    end
    if nalt>n_ralt
      rpar2D=[rpar2D;ones(nalt-n_ralt,n_tot,nrpar2D)*NaN];
      n_ralt=nalt;
    end
    n=1:nalt;
    rpar2D(n,i,1)=ppr;
    x=ppr/re;
    rpar2D(n,i,2)=re*sqrt(1+x.*(x+2*sin(r_el/57.2957795)))-re;
    rpar2D(n,i,3)=pp;
  end
end
% Cast azimuth and elevation into range 0-360, 0-90 degrees
d=find(par1D(:,2)>90); par1D(d,2)=180-par1D(d,2); par1D(d,1)=par1D(d,1)+180;
par1D(:,1)=mod(par1D(:,1)+360,360);
%disp('Done.')
