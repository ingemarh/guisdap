function [Time,par2D,par1D,rpar2D]=load_param(data_path,status,update)
% Function to read the plasma parameters from GUISDAP result files
%
% [Time,par2D,par1D,rpar2D]=load_param(data_path,update)
% par2D [Ran,Alt,Ne,Te,Ti,Vi,Coll,Comp,Res]
% par1D [Az,El,Pt,Tsys]
% rpar2D[Ran,Alt,RawNe]

global lastfile name_expr r_RECloc name_ant
if nargin<3, lastfile=0; end
if nargin<2, status=[]; end
if isempty(status), status=0; end

data_path=fullfile(data_path,filesep);
filelist=getfilelist(data_path);
if nargin>2, filelist=filelist(filelist>lastfile); end
n=length(filelist);
ppres=.25; % pp resolution (km)

%fprintf('\nSource directory: %s\n',data_path);
if n==0
  Time=[]; par2D=[]; par1D=[]; rpar2D=[]; return
end
n_tot=n;
lastfile=filelist(n);
file=sprintf('%08d',filelist(1));
load(canon([data_path file],0))
n_alt=size(r_param,1);

npar2D=9; nrpar2D=3;
par2D	=ones(n_alt,n_tot,npar2D)*NaN;
Time   	=zeros(2,n_tot);
if exist('r_Tsys')
  par1D	=zeros(n_tot,4);
else
  par1D	=zeros(n_tot,3);
  r_Tsys=[];
end
if nargout>3 & strfind('TVL',name_site) & ~isempty(r_pp)
  n_ralt=length(unique(round(r_pprange/ppres)));
  rpar2D=ones(n_ralt,n_tot,3)*NaN;
  re=6370;
else
  n_ralt=NaN;
  rpar2D=[];
end
for i=1:n_tot
  file=sprintf('%08d',filelist(i));
  
% clear('r_*','name_*')
  load(canon([data_path file],0))
  
  nalt=size(r_param,1);
  if nalt>n_alt
    par2D=[par2D;ones(nalt-n_alt,n_tot,npar2D)*NaN];
    n_alt=nalt;
  end
  n=1:nalt;
  d=find(r_status>status); [d1,d2]=find(r_error(d,1:5)>0);
  r_param(d(d1),d2)=NaN;
  par2D(n,i,[1 2 8 9])=[r_range(:,1) r_h(:,1) r_dp(:,1) r_res(:,1)];
  par2D(n,i,[3 5 7])=r_param(:,[1 2 4]);
  par2D(n,i,[4 6])=[r_param(:,2).*r_param(:,3) -r_param(:,5)];
  Time(:,i)	=datenum(r_time);
  par1D(i,:)	=[r_az r_el r_Pt/10000 median(r_Tsys)]; %10 kW
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
